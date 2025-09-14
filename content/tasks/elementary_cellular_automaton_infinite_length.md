+++
title = "Elementary cellular automaton/Infinite length"
description = ""
date = 2019-09-29T23:43:07Z
aliases = []
[extra]
id = 17421
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "cpp",
  "d",
  "elixir",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "ruby",
  "sidef",
  "tcl",
  "zkl",
]
+++

The purpose of this task is to create a version of an [[Elementary cellular automaton]] whose number of cells is only limited by the memory size of the computer.

To be precise, consider the state of the automaton to be made of an infinite number of cells, but with a bounded [[wp:support (mathematics)|support]].  In other words, to describe the state of the automaton, you need a finite number of adjacent cells, along with their individual state, and you then consider that the individual state of each of all other cells is the negation of the closest individual cell among the previously defined finite number of cells.

Examples:


```txt

1        ->   ..., 0, 0,      1,      0, 0, ...
0, 1     ->   ..., 1, 1,   0, 1,      0, 0, ...
1, 0, 1  ->   ..., 0, 0,   1, 0, 1,   0, 0, ...

```


More complex methods can be imagined, provided it is possible to somehow encode the infinite sections.  But for this task we will stick to this simple version.


## C++


```cpp

#include <iostream>
#include <iomanip>
#include <string>

class oo {
public:
    void evolve( int l, int rule ) {
        std::string    cells = "O";
        std::cout << " Rule #" << rule << ":\n";
        for( int x = 0; x < l; x++ ) {
            addNoCells( cells );
            std::cout << std::setw( 40 + ( static_cast<int>( cells.length() ) >> 1 ) ) << cells << "\n";
            step( cells, rule );
        }
    }
private:
    void step( std::string& cells, int rule ) {
        int bin;
        std::string newCells;
        for( size_t i = 0; i < cells.length() - 2; i++ ) {
            bin = 0;
            for( size_t n = i, b = 2; n < i + 3; n++, b >>= 1 ) {
                bin += ( ( cells[n] == 'O' ? 1 : 0 ) << b );
            }
            newCells.append( 1, rule & ( 1 << bin ) ? 'O' : '.' );
        }
        cells = newCells;
    }
    void addNoCells( std::string& s ) {
        char l = s.at( 0 ) == 'O' ? '.' : 'O',
             r = s.at( s.length() - 1 ) == 'O' ? '.' : 'O';
        s = l + s + r;
        s = l + s + r;
    }
};
int main( int argc, char* argv[] ) {
    oo o;
    o.evolve( 35, 90 );
    std::cout << "\n";
    return 0;
}

```

```txt

Rule #90:                                                    Rule #30:                   
                        ..O..                                                        ..O..
                       ..O.O..                                                      ..OOO..
                      ..O...O..                                                    ..OO..O..
                     ..O.O.O.O..                                                  ..OO.OOOO..
                    ..O.......O..                                                ..OO..O...O..
                   ..O.O.....O.O..                                              ..OO.OOOO.OOO..
                  ..O...O...O...O..                                            ..OO..O....O..O..
                 ..O.O.O.O.O.O.O.O..                                          ..OO.OOOO..OOOOOO..
                ..O...............O..                                        ..OO..O...OOO.....O..
               ..O.O.............O.O..                                      ..OO.OOOO.OO..O...OOO..
              ..O...O...........O...O..                                    ..OO..O....O.OOOO.OO..O..
             ..O.O.O.O.........O.O.O.O..                                  ..OO.OOOO..OO.O....O.OOOO..
            ..O.......O.......O.......O..                                ..OO..O...OOO..OO..OO.O...O..
           ..O.O.....O.O.....O.O.....O.O..                              ..OO.OOOO.OO..OOO.OOO..OO.OOO..
          ..O...O...O...O...O...O...O...O..                            ..OO..O....O.OOO...O..OOO..O..O..
         ..O.O.O.O.O.O.O.O.O.O.O.O.O.O.O.O..                          ..OO.OOOO..OO.O..O.OOOOO..OOOOOOO..
        ..O...............................O..                        ..OO..O...OOO..OOOO.O....OOO......O..
       ..O.O.............................O.O..                      ..OO.OOOO.OO..OOO....OO..OO..O....OOO..
      ..O...O...........................O...O..                    ..OO..O....O.OOO..O..OO.OOO.OOOO..OO..O..
     ..O.O.O.O.........................O.O.O.O..                  ..OO.OOOO..OO.O..OOOOOO..O...O...OOO.OOOO..
    ..O.......O.......................O.......O..                ..OO..O...OOO..OOOO.....OOOO.OOO.OO...O...O..
   ..O.O.....O.O.....................O.O.....O.O..              ..OO.OOOO.OO..OOO...O...OO....O...O.O.OOO.OOO..
  ..O...O...O...O...................O...O...O...O..            ..OO..O....O.OOO..O.OOO.OO.O..OOO.OO.O.O...O..O..
 ..O.O.O.O.O.O.O.O.................O.O.O.O.O.O.O.O..          ..OO.OOOO..OO.O..OOO.O...O..OOOO...O..O.OO.OOOOOO..
..O...............O...............O...............O..        ..OO..O...OOO..OOOO...OO.OOOOO...O.OOOOO.O..O.....O..

```



## D

```d
import std.stdio, std.array, std.range, std.typecons, std.string, std.conv,
       std.algorithm;
alias R = replicate;

void main() {
    enum nLines = 25;
    enum notCell = (in char c) pure => (c == '1') ? "0" : "1";

    foreach (immutable rule; [90, 30]) {
        writeln("\nRule: ", rule);
        immutable ruleBits = "%08b".format(rule).retro.text;
        const neighs2next = 8.iota
                            .map!(n => tuple("%03b".format(n), [ruleBits[n]]))
                            .assocArray;

        string C = "1";
        foreach (immutable i; 0 .. nLines) {
            writefln("%2d: %s%s", i, " ".R(nLines - i), C.tr("01", ".#"));
            C = notCell(C[0]).R(2) ~ C ~ notCell(C[$ - 1]).R(2);
            C = iota(1, C.length - 1)
                .map!(i => neighs2next[C[i - 1 .. i + 2]])
                .join;
        }
    }
}
```

The output is the same as the Python entry.


## Elixir

```elixir

defmodule Elementary_cellular_automaton do
  def infinite(cell, rule, times) do
    each(cell, rule_pattern(rule), times)
  end
  
  defp each(_, _, 0), do: :ok
  defp each(cells, rules, times) do
    IO.write String.duplicate(" ", times)
    IO.puts String.replace(cells, "0", ".") |> String.replace("1", "#")
    c = not_cell(String.first(cells)) <> cells <> not_cell(String.last(cells))
    next_cells = Enum.map_join(0..String.length(cells)+1, fn i ->
      Map.get(rules, String.slice(c, i, 3))
    end)
    each(next_cells, rules, times-1)
  end
  
  defp not_cell("0"), do: "11"
  defp not_cell("1"), do: "00"
  
  defp rule_pattern(rule) do
    list = Integer.to_string(rule, 2) |> String.pad_leading(8, "0")
           |> String.codepoints |> Enum.reverse
    Enum.map(0..7, fn i -> Integer.to_string(i, 2) |> String.pad_leading(3, "0") end)
    |> Enum.zip(list) |> Map.new
  end
end

Enum.each([18, 30], fn rule ->
  IO.puts "\nRule : #{rule}"
  Elementary_cellular_automaton.infinite("1", rule, 25)
end)
```


```txt

Rule : 18
                         #
                        #.#
                       #...#
                      #.#.#.#
                     #.......#
                    #.#.....#.#
                   #...#...#...#
                  #.#.#.#.#.#.#.#
                 #...............#
                #.#.............#.#
               #...#...........#...#
              #.#.#.#.........#.#.#.#
             #.......#.......#.......#
            #.#.....#.#.....#.#.....#.#
           #...#...#...#...#...#...#...#
          #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
         #...............................#
        #.#.............................#.#
       #...#...........................#...#
      #.#.#.#.........................#.#.#.#
     #.......#.......................#.......#
    #.#.....#.#.....................#.#.....#.#
   #...#...#...#...................#...#...#...#
  #.#.#.#.#.#.#.#.................#.#.#.#.#.#.#.#
 #...............#...............#...............#

Rule : 30
                         #
                        ###
                       ##..#
                      ##.####
                     ##..#...#
                    ##.####.###
                   ##..#....#..#
                  ##.####..######
                 ##..#...###.....#
                ##.####.##..#...###
               ##..#....#.####.##..#
              ##.####..##.#....#.####
             ##..#...###..##..##.#...#
            ##.####.##..###.###..##.###
           ##..#....#.###...#..###..#..#
          ##.####..##.#..#.#####..#######
         ##..#...###..####.#....###......#
        ##.####.##..###....##..##..#....###
       ##..#....#.###..#..##.###.####..##..#
      ##.####..##.#..######..#...#...###.####
     ##..#...###..####.....####.###.##...#...#
    ##.####.##..###...#...##....#...#.#.###.###
   ##..#....#.###..#.###.##.#..###.##.#.#...#..#
  ##.####..##.#..###.#...#..####...#..#.##.######
 ##..#...###..####...##.#####...#.#####.#..#.....#

```



## Go

```go
package main

import (
    "fmt"
    "strings"
)

func btoi(b bool) int {
    if b {
        return 1
    }
    return 0
}

func evolve(l, rule int) {
    fmt.Printf(" Rule #%d:\n", rule)
    cells := "O"
    for x := 0; x < l; x++ {
        cells = addNoCells(cells)
        width := 40 + (len(cells) >> 1)
        fmt.Printf("%*s\n", width, cells)
        cells = step(cells, rule)
    }
}

func step(cells string, rule int) string {
    newCells := new(strings.Builder)
    for i := 0; i < len(cells)-2; i++ {
        bin := 0
        b := uint(2)
        for n := i; n < i+3; n++ {
            bin += btoi(cells[n] == 'O') << b
            b >>= 1
        }
        a := '.'
        if rule&(1<<uint(bin)) != 0 {
            a = 'O'
        }
        newCells.WriteRune(a)
    }
    return newCells.String()
}

func addNoCells(cells string) string {
    l, r := "O", "O"
    if cells[0] == 'O' {
        l = "."
    }
    if cells[len(cells)-1] == 'O' {
        r = "."
    }
    cells = l + cells + r
    cells = l + cells + r
    return cells
}

func main() {
    for _, r := range []int{90, 30} {
        evolve(25, r)
        fmt.Println()
    }
}
```


```txt

 Rule #90:
                                     ..O..
                                    ..O.O..
                                   ..O...O..
                                  ..O.O.O.O..
                                 ..O.......O..
                                ..O.O.....O.O..
                               ..O...O...O...O..
                              ..O.O.O.O.O.O.O.O..
                             ..O...............O..
                            ..O.O.............O.O..
                           ..O...O...........O...O..
                          ..O.O.O.O.........O.O.O.O..
                         ..O.......O.......O.......O..
                        ..O.O.....O.O.....O.O.....O.O..
                       ..O...O...O...O...O...O...O...O..
                      ..O.O.O.O.O.O.O.O.O.O.O.O.O.O.O.O..
                     ..O...............................O..
                    ..O.O.............................O.O..
                   ..O...O...........................O...O..
                  ..O.O.O.O.........................O.O.O.O..
                 ..O.......O.......................O.......O..
                ..O.O.....O.O.....................O.O.....O.O..
               ..O...O...O...O...................O...O...O...O..
              ..O.O.O.O.O.O.O.O.................O.O.O.O.O.O.O.O..
             ..O...............O...............O...............O..

 Rule #30:
                                     ..O..
                                    ..OOO..
                                   ..OO..O..
                                  ..OO.OOOO..
                                 ..OO..O...O..
                                ..OO.OOOO.OOO..
                               ..OO..O....O..O..
                              ..OO.OOOO..OOOOOO..
                             ..OO..O...OOO.....O..
                            ..OO.OOOO.OO..O...OOO..
                           ..OO..O....O.OOOO.OO..O..
                          ..OO.OOOO..OO.O....O.OOOO..
                         ..OO..O...OOO..OO..OO.O...O..
                        ..OO.OOOO.OO..OOO.OOO..OO.OOO..
                       ..OO..O....O.OOO...O..OOO..O..O..
                      ..OO.OOOO..OO.O..O.OOOOO..OOOOOOO..
                     ..OO..O...OOO..OOOO.O....OOO......O..
                    ..OO.OOOO.OO..OOO....OO..OO..O....OOO..
                   ..OO..O....O.OOO..O..OO.OOO.OOOO..OO..O..
                  ..OO.OOOO..OO.O..OOOOOO..O...O...OOO.OOOO..
                 ..OO..O...OOO..OOOO.....OOOO.OOO.OO...O...O..
                ..OO.OOOO.OO..OOO...O...OO....O...O.O.OOO.OOO..
               ..OO..O....O.OOO..O.OOO.OO.O..OOO.OO.O.O...O..O..
              ..OO.OOOO..OO.O..OOO.O...O..OOOO...O..O.OO.OOOOOO..
             ..OO..O...OOO..OOOO...OO.OOOOO...O.OOOOO.O..O.....O..

```



## Haskell

Infinite lists are natural in Haskell, however the task forces us to deal with lists that are infinite in both directions. These structures could be efficiently implemented as a ''zipper lists''. Moreover, zipper lists are instances of magic <code>Comonad</code> class, which gives beautifull implementation of cellular automata.

This solution is kinda involved, but it is guaranteed to be total and correct by type checker.

First we provide the datatype, the viewer and constructor:


```Haskell
{-# LANGUAGE DeriveFunctor #-}

import Control.Comonad
import Data.InfList (InfList (..), (+++))
import qualified Data.InfList as Inf

data Cells a = Cells (InfList a) a (InfList a) deriving Functor

view n (Cells l x r) = reverse (Inf.take n l) ++ [x] ++ (Inf.take n r)

fromList []     = fromList [0]
fromList (x:xs) = let zeros = Inf.repeat 0
                  in Cells zeros x (xs +++ zeros)
```


In order to run the CA on the domain we make it an instance of <code>Comonad</code> class. Running the CA turns to be just an iterative comonadic ''extension'' of the rule:


```Haskell
instance Comonad Cells where
  extract (Cells _ x _) = x
  duplicate x = Cells (rewind left x) x (rewind right x)
    where
      rewind dir = Inf.iterate dir . dir
      right (Cells l x (r ::: rs)) = Cells (x ::: l) r rs
      left  (Cells (l ::: ls) x r) = Cells ls l (x ::: r)

runCA rule = iterate (=>> step)
  where step (Cells (l ::: _) x (r ::: _)) = rule l x r
```


Following is the rule definition and I/O routine:


```Haskell
rule n l x r = n `div` (2^(4*l + 2*x + r)) `mod` 2

displayCA n w rule init = mapM_ putStrLn $ take n result
  where result = fmap display . view w <$> runCA rule init
        display 0 = ' '
        display _ = '*'
```


```txt
λ> displayCA 30 20 (rule 90) (fromList [1])
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
*       *                       *       *
 *     * *                     * *     * 
  *   *   *                   *   *   *  
 * * * * * *                 * * * * * * 
            *               *            
           * *             * *           
          *   *           *   *          
         * * * *         * * * *         
*       *       *       *       *       *
 *     * *     * *     * *     * *     * 
```


See also [[Elementary cellular automaton#Haskell]]


## J


Implementation note: edges are complement of the first and last represented cell, which we define as 1 for the case of an empty numeric list. (So we can represent an infinite space of 0s but not an infinite space of 1s.)

We actually only extend our edges by 9 positions (which is more than sufficient), and then trim everything up to the first change from each edge (so the result from a rule which results in all 1s will be silently converted to an empty all 0s result).

Note however that this means that positions in the result are not anchored to positions in the argument. They might correspond or they might be "off by one" position.


Implementation:


```J
ext9=: (9#1-{.!.1),],9#1-{:!.1
trim=: |.@(}.~ ] i. 1-{.)^:2
next=: trim@(((8$2) #: [) {~ 2 #. 1 - [: |: |.~"1 0&_1 0 1@]) ext9
```


In other words, a wrapped version of the [[Elementary_cellular_automaton#J|original implementation]].

example use:


```J
   ' *'{~90 next^:(i.9) 1
*                
* *              
*   *            
* * * *          
*       *        
* *     * *      
*   *   *   *    
* * * * * * * *  
*               *
```


Looks like a [[Sierpinski_triangle|Sierpinski triangle]]


## Julia

```julia
function ecainfinite(cells, rule, n)
    notcell(cell) = (cell == '1') ? '0' : '1'
    rulebits = reverse(string(rule, base = 2, pad = 8))
    neighbors2next = Dict(string(n - 1, base=2, pad=3) => rulebits[n] for n in 1:8)
    ret = String[]
    for i in 1:n
        push!(ret, cells)
        cells = notcell(cells[1])^2 * cells * notcell(cells[end])^2 # Extend/pad ends
        cells = join([neighbors2next[cells[i:i+2]] for i in 1:length(cells)-2], "")
    end
    ret
end

function testinfcells(lines::Integer)
    for rule in [90, 30]
        println("\nRule: $rule ($(string(rule, base = 2, pad = 8)))")
        s = ecainfinite("1", rule, lines)
        for i in 1:lines
            println("$i: ", " "^(lines - i), replace(replace(s[i], "0" => "."), "1" => "#"))
        end
    end
end

testinfcells(25)

```
```txt

Rule: 90 (01011010)
1:                         #
2:                        #.#
3:                       #...#
4:                      #.#.#.#
5:                     #.......#
6:                    #.#.....#.#
7:                   #...#...#...#
8:                  #.#.#.#.#.#.#.#
9:                 #...............#
10:                #.#.............#.#
11:               #...#...........#...#
12:              #.#.#.#.........#.#.#.#
13:             #.......#.......#.......#
14:            #.#.....#.#.....#.#.....#.#
15:           #...#...#...#...#...#...#...#
16:          #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
17:         #...............................#
18:        #.#.............................#.#
19:       #...#...........................#...#
20:      #.#.#.#.........................#.#.#.#
21:     #.......#.......................#.......#
22:    #.#.....#.#.....................#.#.....#.#
23:   #...#...#...#...................#...#...#...#
24:  #.#.#.#.#.#.#.#.................#.#.#.#.#.#.#.#
25: #...............#...............#...............#

Rule: 30 (00011110)
1:                         #
2:                        ###
3:                       ##..#
4:                      ##.####
5:                     ##..#...#
6:                    ##.####.###
7:                   ##..#....#..#
8:                  ##.####..######
9:                 ##..#...###.....#
10:                ##.####.##..#...###
11:               ##..#....#.####.##..#
12:              ##.####..##.#....#.####
13:             ##..#...###..##..##.#...#
14:            ##.####.##..###.###..##.###
15:           ##..#....#.###...#..###..#..#
16:          ##.####..##.#..#.#####..#######
17:         ##..#...###..####.#....###......#
18:        ##.####.##..###....##..##..#....###
19:       ##..#....#.###..#..##.###.####..##..#
20:      ##.####..##.#..######..#...#...###.####
21:     ##..#...###..####.....####.###.##...#...#
22:    ##.####.##..###...#...##....#...#.#.###.###
23:   ##..#....#.###..#.###.##.#..###.##.#.#...#..#
24:  ##.####..##.#..###.#...#..####...#..#.##.######
25: ##..#...###..####...##.#####...#.#####.#..#.....#

```



## Kotlin

```scala
// version 1.1.51

fun evolve(l: Int, rule: Int) {
    println(" Rule #$rule:")
    var cells = StringBuilder("*")
    for (x in 0 until l) {
        addNoCells(cells)
        val width = 40 + (cells.length shr 1)
        println(cells.padStart(width))
        cells = step(cells, rule)
    }
}

fun step(cells: StringBuilder, rule: Int): StringBuilder {
    val newCells = StringBuilder()
    for (i in 0 until cells.length - 2) {
        var bin = 0
        var b = 2
        for (n in i until i + 3) {
            bin += (if (cells[n] == '*') 1 else 0) shl b
            b = b shr 1
        }
        val a = if ((rule and (1 shl bin)) != 0) '*' else '.'
        newCells.append(a)
    }
    return newCells
}

fun addNoCells(s: StringBuilder) {
    val l = if (s[0] == '*') '.' else '*'
    val r = if (s[s.length - 1] == '*') '.' else '*'
    repeat(2) {
       s.insert(0, l)
       s.append(r)
    }
}

fun main(args: Array<String>) {
    evolve(35, 90)
    println()
}
```


```txt

 Rule #90:
                                     ..*..
                                    ..*.*..
                                   ..*...*..
                                  ..*.*.*.*..
                                 ..*.......*..
                                ..*.*.....*.*..
                               ..*...*...*...*..
                              ..*.*.*.*.*.*.*.*..
                             ..*...............*..
                            ..*.*.............*.*..
                           ..*...*...........*...*..
                          ..*.*.*.*.........*.*.*.*..
                         ..*.......*.......*.......*..
                        ..*.*.....*.*.....*.*.....*.*..
                       ..*...*...*...*...*...*...*...*..
                      ..*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*..
                     ..*...............................*..
                    ..*.*.............................*.*..
                   ..*...*...........................*...*..
                  ..*.*.*.*.........................*.*.*.*..
                 ..*.......*.......................*.......*..
                ..*.*.....*.*.....................*.*.....*.*..
               ..*...*...*...*...................*...*...*...*..
              ..*.*.*.*.*.*.*.*.................*.*.*.*.*.*.*.*..
             ..*...............*...............*...............*..
            ..*.*.............*.*.............*.*.............*.*..
           ..*...*...........*...*...........*...*...........*...*..
          ..*.*.*.*.........*.*.*.*.........*.*.*.*.........*.*.*.*..
         ..*.......*.......*.......*.......*.......*.......*.......*..
        ..*.*.....*.*.....*.*.....*.*.....*.*.....*.*.....*.*.....*.*..
       ..*...*...*...*...*...*...*...*...*...*...*...*...*...*...*...*..
      ..*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*..
     ..*...............................................................*..
    ..*.*.............................................................*.*..
   ..*...*...........................................................*...*..

```



## Perl

The edges of a pattern is implicitly repeating.  The code will try to lineup output by padding up to 40 spaces to the left, but since the cells keep expanding, that has to end somewhere.

```perl
sub evolve {
	my ($rule, $pattern) = @_;
	my $offset = 0;

	while (1) {
		my ($l, $r, $st);
		$pattern =~ s/^((.)\g2*)/$2$2/ and $l = $2, $offset -= length($2);
		$pattern =~ s/(.)\g1*$/$1$1/   and $r = $1;

		$st = $pattern;

		$pattern =~ tr/01/.#/;
		printf "%5d| %s%s\n", $offset, ' ' x (40 + $offset), $pattern;

		$pattern = join '', map(1 & ($rule>>oct "0b$_"),
				$l x 3,
				map(substr($st, $_, 3), 0 .. length($st)-3),
				$r x 3);
	}
}

evolve(90, "010");
```

```txt

   -1|                                        ..#..
   -2|                                       ..#.#..
   -3|                                      ..#...#..
   -4|                                     ..#.#.#.#..
   -5|                                    ..#.......#..
   -6|                                   ..#.#.....#.#..
   -7|                                  ..#...#...#...#..
   -8|                                 ..#.#.#.#.#.#.#.#..
   -9|                                ..#...............#..
  -10|                               ..#.#.............#.#..
  -11|                              ..#...#...........#...#..
  -12|                             ..#.#.#.#.........#.#.#.#..
  -13|                            ..#.......#.......#.......#..
---(infinite more lines snipped)---

```



## Perl 6

This version, while it is ''capable'' of working with infinite length cellular automata,  makes the assumption that any cells which have not been explicitly examined are in a 'null' state, neither '0' or '1'. Further it makes the assumption that a null cell, on being examined, initially contains nothing (░). Otherwise it would take infinite time to calculate every row and would be exceptionally boring to watch.

Based heavily on the code from the [[One-dimensional_cellular_automata#Perl_6|One-dimensional cellular automata]] task. Example uses rule 90 (Sierpinski triangle). 


```perl6
class Automaton {
    has $.rule;
    has @.cells;
    has @.code = $!rule.fmt('%08b').flip.comb».Int;

    method gist { @!cells.map({+$_ ?? '▲' !! '░'}).join }

    method succ {
        self.new: :$!rule, :@!code, :cells(
            ' ',
            |@!code[
                    4 «*« @!cells.rotate(-1)
                »+« 2 «*« @!cells
                »+«       @!cells.rotate(1)
            ],
            ' '
        )
    }
}

my Automaton $a .= new: :rule(90), :cells(flat '010'.comb);

# display the first 20 rows
say $a++ for ^20;

# then calculate the other infinite number of rows, (may take a while)
$a++ for ^Inf;
```

```txt
░▲░
░▲░▲░
░▲░░░▲░
░▲░▲░▲░▲░
░▲░░░░░░░▲░
░▲░▲░░░░░▲░▲░
░▲░░░▲░░░▲░░░▲░
░▲░▲░▲░▲░▲░▲░▲░▲░
░▲░░░░░░░░░░░░░░░▲░
░▲░▲░░░░░░░░░░░░░▲░▲░
░▲░░░▲░░░░░░░░░░░▲░░░▲░
░▲░▲░▲░▲░░░░░░░░░▲░▲░▲░▲░
░▲░░░░░░░▲░░░░░░░▲░░░░░░░▲░
░▲░▲░░░░░▲░▲░░░░░▲░▲░░░░░▲░▲░
░▲░░░▲░░░▲░░░▲░░░▲░░░▲░░░▲░░░▲░
░▲░▲░▲░▲░▲░▲░▲░▲░▲░▲░▲░▲░▲░▲░▲░▲░
░▲░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░▲░
░▲░▲░░░░░░░░░░░░░░░░░░░░░░░░░░░░░▲░▲░
░▲░░░▲░░░░░░░░░░░░░░░░░░░░░░░░░░░▲░░░▲░
░▲░▲░▲░▲░░░░░░░░░░░░░░░░░░░░░░░░░▲░▲░▲░▲░
^C

```



## Phix

Uses 0-expansion either side

```Phix
string s = ".#.",
       t=s, r = "........"
integer rule = 18, k, l = length(s), w = 0
for i=1 to 8 do
    r[i] = iff(mod(rule,2)?'#':'.')
    rule = floor(rule/2)
end for
for i=0 to 25 do
    ?repeat(' ',floor((55-length(s))/2))&s
    for j=1 to l do
        k = (s[iff(j=1?l:j-1)]='#')*4
          + (s[          j   ]='#')*2
          + (s[iff(j=l?1:j+1)]='#')+1
        t[j] = r[k]
    end for
    if t[1]='#' then t = '.'&t end if
    if t[$]='#' then t = t&'.' end if
    l = length(t)
    s = t
end for
```

```txt

"                          .#."
"                         .#.#."
"                        .#...#."
"                       .#.#.#.#."
"                      .#.......#."
"                     .#.#.....#.#."
"                    .#...#...#...#."
"                   .#.#.#.#.#.#.#.#."
"                  .#...............#."
"                 .#.#.............#.#."
"                .#...#...........#...#."
"               .#.#.#.#.........#.#.#.#."
"              .#.......#.......#.......#."
"             .#.#.....#.#.....#.#.....#.#."
"            .#...#...#...#...#...#...#...#."
"           .#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#."
"          .#...............................#."
"         .#.#.............................#.#."
"        .#...#...........................#...#."
"       .#.#.#.#.........................#.#.#.#."
"      .#.......#.......................#.......#."
"     .#.#.....#.#.....................#.#.....#.#."
"    .#...#...#...#...................#...#...#...#."
"   .#.#.#.#.#.#.#.#.................#.#.#.#.#.#.#.#."
"  .#...............#...............#...............#."
" .#.#.............#.#.............#.#.............#.#."

```



## Python

Infinite generator but only print 25 lines of each rule.


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
    lines = 25
    for rule in (90, 30):
        print('\nRule: %i' % rule)
        for i, c in zip(range(lines), eca_infinite('1', rule)):
            print('%2i: %s%s' % (i, ' '*(lines - i), c.replace('0', '.').replace('1', '#')))
```


```txt
Rule: 90
 0:                          #
 1:                         #.#
 2:                        #...#
 3:                       #.#.#.#
 4:                      #.......#
 5:                     #.#.....#.#
 6:                    #...#...#...#
 7:                   #.#.#.#.#.#.#.#
 8:                  #...............#
 9:                 #.#.............#.#
10:                #...#...........#...#
11:               #.#.#.#.........#.#.#.#
12:              #.......#.......#.......#
13:             #.#.....#.#.....#.#.....#.#
14:            #...#...#...#...#...#...#...#
15:           #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
16:          #...............................#
17:         #.#.............................#.#
18:        #...#...........................#...#
19:       #.#.#.#.........................#.#.#.#
20:      #.......#.......................#.......#
21:     #.#.....#.#.....................#.#.....#.#
22:    #...#...#...#...................#...#...#...#
23:   #.#.#.#.#.#.#.#.................#.#.#.#.#.#.#.#
24:  #...............#...............#...............#

Rule: 30
 0:                          #
 1:                         ###
 2:                        ##..#
 3:                       ##.####
 4:                      ##..#...#
 5:                     ##.####.###
 6:                    ##..#....#..#
 7:                   ##.####..######
 8:                  ##..#...###.....#
 9:                 ##.####.##..#...###
10:                ##..#....#.####.##..#
11:               ##.####..##.#....#.####
12:              ##..#...###..##..##.#...#
13:             ##.####.##..###.###..##.###
14:            ##..#....#.###...#..###..#..#
15:           ##.####..##.#..#.#####..#######
16:          ##..#...###..####.#....###......#
17:         ##.####.##..###....##..##..#....###
18:        ##..#....#.###..#..##.###.####..##..#
19:       ##.####..##.#..######..#...#...###.####
20:      ##..#...###..####.....####.###.##...#...#
21:     ##.####.##..###...#...##....#...#.#.###.###
22:    ##..#....#.###..#.###.##.#..###.##.#.#...#..#
23:   ##.####..##.#..###.#...#..####...#..#.##.######
24:  ##..#...###..####...##.#####...#.#####.#..#.....#
```



## Racket


Uses solution to [[Elementary cellular automaton]] saved in file "Elementary_cellular_automata.rkt"


```racket
#lang racket
; below is the code from the parent task
(require "Elementary_cellular_automata.rkt")
(require racket/fixnum)

(define (wrap-rule-infinite v-in vl-1 offset)
  (define l-bit-set? (bitwise-bit-set? (fxvector-ref v-in 0) usable-bits/fixnum-1))
  (define r-bit-set? (bitwise-bit-set? (fxvector-ref v-in vl-1) 0))
  ;; if we need to extend left offset is reduced by 1
  (define l-pad-words (if l-bit-set? 1 0))
  (define r-pad-words (if r-bit-set? 1 0))
  (define new-words (fx+ l-pad-words r-pad-words))
  (cond
    [(fx= 0 new-words) (values v-in vl-1 offset)] ; nothing changes
    [else
     (define offset- (if l-bit-set? (fx- offset 1) offset))
     (define l-sequence (if l-bit-set? (in-value 0) (in-sequences)))
     (define vl-1+ (fx+ vl-1 (fx+ l-pad-words r-pad-words)))  
     (define v-out
       (for/fxvector
           #:length (fx+ vl-1+ 1) #:fill 0 ; right padding
           ([f (in-sequences l-sequence (in-fxvector v-in))])
        f))
     (values v-out vl-1+ offset-)]))

(module+ main
  (define ng/90/infinite (CA-next-generation 90 #:wrap-rule wrap-rule-infinite))
  (for/fold ([v (fxvector #b10000000000000000000)]
             [o 1]) ; start by pushing output right by one
            ([step (in-range 40)])
    (show-automaton v #:step step #:push-right o)
    (newline)
    (ng/90/infinite v o)))
```


```txt
[         0] ..............................000000000010000000000000000000
[         1] ..............................000000000101000000000000000000
[         2] ..............................000000001000100000000000000000
[         3] ..............................000000010101010000000000000000
[         4] ..............................000000100000001000000000000000
[         5] ..............................000001010000010100000000000000
[         6] ..............................000010001000100010000000000000
[         7] ..............................000101010101010101000000000000
[         8] ..............................001000000000000000100000000000
[         9] ..............................010100000000000001010000000000
[        10] ..............................100010000000000010001000000000
[        11] 000000000000000000000000000001010101000000000101010100000000
[        12] 000000000000000000000000000010000000100000001000000010000000
[        13] 000000000000000000000000000101000001010000010100000101000000
[        14] 000000000000000000000000001000100010001000100010001000100000
[        15] 000000000000000000000000010101010101010101010101010101010000
[        16] 000000000000000000000000100000000000000000000000000000001000
[        17] 000000000000000000000001010000000000000000000000000000010100
[        18] 000000000000000000000010001000000000000000000000000000100010
[        19] 000000000000000000000101010100000000000000000000000001010101
[        20] 000000000000000000001000000010000000000000000000000010000000100000000000000000000000000000
[        21] 000000000000000000010100000101000000000000000000000101000001010000000000000000000000000000
[        22] 000000000000000000100010001000100000000000000000001000100010001000000000000000000000000000
[        23] 000000000000000001010101010101010000000000000000010101010101010100000000000000000000000000
[        24] 000000000000000010000000000000001000000000000000100000000000000010000000000000000000000000
[        25] 000000000000000101000000000000010100000000000001010000000000000101000000000000000000000000
[        26] 000000000000001000100000000000100010000000000010001000000000001000100000000000000000000000
[        27] 000000000000010101010000000001010101000000000101010100000000010101010000000000000000000000
[        28] 000000000000100000001000000010000000100000001000000010000000100000001000000000000000000000
[        29] 000000000001010000010100000101000001010000010100000101000001010000010100000000000000000000
[        30] 000000000010001000100010001000100010001000100010001000100010001000100010000000000000000000
[        31] 000000000101010101010101010101010101010101010101010101010101010101010101000000000000000000
[        32] 000000001000000000000000000000000000000000000000000000000000000000000000100000000000000000
[        33] 000000010100000000000000000000000000000000000000000000000000000000000001010000000000000000
[        34] 000000100010000000000000000000000000000000000000000000000000000000000010001000000000000000
[        35] 000001010101000000000000000000000000000000000000000000000000000000000101010100000000000000
[        36] 000010000000100000000000000000000000000000000000000000000000000000001000000010000000000000
[        37] 000101000001010000000000000000000000000000000000000000000000000000010100000101000000000000
[        38] 001000100010001000000000000000000000000000000000000000000000000000100010001000100000000000
[        39] 010101010101010100000000000000000000000000000000000000000000000001010101010101010000000000
#fx(536879104 0 33554944)
0
```



## Ruby

```ruby
def notcell(c)
  c.tr('01','10')
end

def eca_infinite(cells, rule)
  neighbours2next = Hash[8.times.map{|i|["%03b"%i, "01"[rule[i]]]}]
  c = cells
  Enumerator.new do |y|
    loop do
      y << c
      c = notcell(c[0])*2 + c + notcell(c[-1])*2        # Extend and pad the ends
      c = (1..c.size-2).map{|i| neighbours2next[c[i-1..i+1]]}.join
    end
  end
end

if __FILE__ == $0
  lines = 25
  for rule in [90, 30]
    puts "\nRule: %i" % rule
    for i, c in (0...lines).zip(eca_infinite('1', rule))
      puts '%2i: %s%s' % [i, ' '*(lines - i), c.tr('01', '.#')]
    end
  end
end
```

The output is the same as the Python entry.


## Sidef

```ruby
func evolve(rule, bin) {
    var offset = 0
    var (l='', r='')
    Inf.times {
        bin.sub!(/^((.)\g2*)/, {|_s1, s2| l = s2; offset -= s2.len; s2*2 })
        bin.sub!(/(.)\g1*$/, {|s1| r = s1; s1*2 })
        printf("%5d| %s%s\n", offset, ' ' * (40 + offset), bin.tr('01','.#'))
        bin = [l*3, 0.to(bin.len-3).map{|i| bin.substr(i, 3) }..., r*3 ].map { |t|
                1 & (rule >> t.bin)
        }.join
    }
}

evolve(90, "010")
```

```txt

   -1|                                        ..#..
   -2|                                       ..#.#..
   -3|                                      ..#...#..
   -4|                                     ..#.#.#.#..
   -5|                                    ..#.......#..
   -6|                                   ..#.#.....#.#..
   -7|                                  ..#...#...#...#..
   -8|                                 ..#.#.#.#.#.#.#.#..
   -9|                                ..#...............#..
  -10|                               ..#.#.............#.#..
  -11|                              ..#...#...........#...#..
  -12|                             ..#.#.#.#.........#.#.#.#..
  -13|                            ..#.......#.......#.......#..
  -14|                           ..#.#.....#.#.....#.#.....#.#..
  -15|                          ..#...#...#...#...#...#...#...#..
  -16|                         ..#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#..
  -17|                        ..#...............................#..
  -18|                       ..#.#.............................#.#..
  -19|                      ..#...#...........................#...#..
  -20|                     ..#.#.#.#.........................#.#.#.#..
   ⋮

```



## Tcl

```tcl
package require Tcl 8.6

oo::class create InfiniteElementaryAutomaton {
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
    method evolve {left state right} {
	set state [list $left {*}$state $right]
	set len [llength $state]
	for {set i -1;set j 0;set k 1} {$j < $len} {incr i;incr j;incr k} {
	    set a [expr {$i<0 ? $left : [lindex $state $i]}]
	    set b [lindex $state $j]
	    set c [expr {$k==$len ? $right : [lindex $state $k]}]
	    lappend result [dict get $rules $a$b$c]
	}
	return $result
    }

    method evolveEnd {endState} {
	return [dict get $rules $endState$endState$endState]
    }

    # Simple driver method; omit the initial state to get a centred dot
    method run {steps {initialState "010"}} {
	set cap [string repeat "\u2026" $steps]
	set s [split [string map ". 0 # 1" $initialState] ""]
	set left [lindex $s 0]
	set right [lindex $s end]
	set s [lrange $s 1 end-1]
	for {set i 0} {$i < $steps} {incr i} {
	    puts $cap[string map "0 . 1 #" $left[join $s ""]$right]$cap
	    set s [my evolve $left $s $right]
	    set left [my evolveEnd $left]
	    set right [my evolveEnd $right]
	    set cap [string range $cap 1 end]
	}
	puts $cap[string map "0 . 1 #" $left[join $s ""]$right]$cap
    }
}

foreach num {90 30} {
    puts "Rule ${num}:"
    set rule [InfiniteElementaryAutomaton new $num]
    $rule run 25
    $rule destroy
}
```

```txt

Rule 90:
………………………………………………………………….#.…………………………………………………………………
……………………………………………………………….#.#.………………………………………………………………
…………………………………………………………….#...#.……………………………………………………………
………………………………………………………….#.#.#.#.…………………………………………………………
……………………………………………………….#.......#.………………………………………………………
…………………………………………………….#.#.....#.#.……………………………………………………
………………………………………………….#...#...#...#.…………………………………………………
……………………………………………….#.#.#.#.#.#.#.#.………………………………………………
…………………………………………….#...............#.……………………………………………
………………………………………….#.#.............#.#.…………………………………………
……………………………………….#...#...........#...#.………………………………………
…………………………………….#.#.#.#.........#.#.#.#.……………………………………
………………………………….#.......#.......#.......#.…………………………………
……………………………….#.#.....#.#.....#.#.....#.#.………………………………
…………………………….#...#...#...#...#...#...#...#.……………………………
………………………….#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.…………………………
……………………….#...............................#.………………………
…………………….#.#.............................#.#.……………………
………………….#...#...........................#...#.…………………
……………….#.#.#.#.........................#.#.#.#.………………
…………….#.......#.......................#.......#.……………
………….#.#.....#.#.....................#.#.....#.#.…………
……….#...#...#...#...................#...#...#...#.………
…….#.#.#.#.#.#.#.#.................#.#.#.#.#.#.#.#.……
….#...............#...............#...............#.…
.#.#.............#.#.............#.#.............#.#.
Rule 30:
………………………………………………………………….#.…………………………………………………………………
……………………………………………………………….###.………………………………………………………………
…………………………………………………………….##..#.……………………………………………………………
………………………………………………………….##.####.…………………………………………………………
……………………………………………………….##..#...#.………………………………………………………
…………………………………………………….##.####.###.……………………………………………………
………………………………………………….##..#....#..#.…………………………………………………
……………………………………………….##.####..######.………………………………………………
…………………………………………….##..#...###.....#.……………………………………………
………………………………………….##.####.##..#...###.…………………………………………
……………………………………….##..#....#.####.##..#.………………………………………
…………………………………….##.####..##.#....#.####.……………………………………
………………………………….##..#...###..##..##.#...#.…………………………………
……………………………….##.####.##..###.###..##.###.………………………………
…………………………….##..#....#.###...#..###..#..#.……………………………
………………………….##.####..##.#..#.#####..#######.…………………………
……………………….##..#...###..####.#....###......#.………………………
…………………….##.####.##..###....##..##..#....###.……………………
………………….##..#....#.###..#..##.###.####..##..#.…………………
……………….##.####..##.#..######..#...#...###.####.………………
…………….##..#...###..####.....####.###.##...#...#.……………
………….##.####.##..###...#...##....#...#.#.###.###.…………
……….##..#....#.###..#.###.##.#..###.##.#.#...#..#.………
…….##.####..##.#..###.#...#..####...#..#.##.######.……
….##..#...###..####...##.#####...#.#####.#..#.....#.…
.##.####.##..###...#.##..#....#.##.#.....#####...###.

```



## zkl

```zkl
nLines,flipCell := 25, fcn(c){ (c=="1") and "0" or "1" };
foreach rule in (T(90,30)){
   println("\nRule: ", rule);
   ruleBits:="%08.2B".fmt(rule);  // eg 90-->"01011010"
   neighs2next:=(8).pump(Dictionary(),
      'wrap(n){ T("%03.2B".fmt(n), ruleBits.reverse()[n]) });
   C:="1";  // C is "1"s and "0"s, I'll auto cast to Int as needed
   foreach i in (nLines){
      println("%2d: %s%s".fmt(i," "*(nLines - i), C.translate("01",".#")));
      C=String(flipCell(C[0])*2, C, flipCell(C[-1])*2);
      C=[1..C.len()-2].pump(String,'wrap(n){ neighs2next[C[n-1,3]] });
   }
}
```

```txt

Rule: 90
 0:                          #
 1:                         #.#
 2:                        #...#
 3:                       #.#.#.#
 4:                      #.......#
 5:                     #.#.....#.#
 6:                    #...#...#...#
 7:                   #.#.#.#.#.#.#.#
 8:                  #...............#
 9:                 #.#.............#.#
10:                #...#...........#...#
11:               #.#.#.#.........#.#.#.#
12:              #.......#.......#.......#
13:             #.#.....#.#.....#.#.....#.#
14:            #...#...#...#...#...#...#...#
15:           #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
16:          #...............................#
17:         #.#.............................#.#
18:        #...#...........................#...#
19:       #.#.#.#.........................#.#.#.#
20:      #.......#.......................#.......#
21:     #.#.....#.#.....................#.#.....#.#
22:    #...#...#...#...................#...#...#...#
23:   #.#.#.#.#.#.#.#.................#.#.#.#.#.#.#.#
24:  #...............#...............#...............#

Rule: 30
 0:                          #
 1:                         ###
 2:                        ##..#
 3:                       ##.####
 4:                      ##..#...#
 5:                     ##.####.###
 6:                    ##..#....#..#
 7:                   ##.####..######
 8:                  ##..#...###.....#
 9:                 ##.####.##..#...###
10:                ##..#....#.####.##..#
11:               ##.####..##.#....#.####
12:              ##..#...###..##..##.#...#
13:             ##.####.##..###.###..##.###
14:            ##..#....#.###...#..###..#..#
15:           ##.####..##.#..#.#####..#######
16:          ##..#...###..####.#....###......#
17:         ##.####.##..###....##..##..#....###
18:        ##..#....#.###..#..##.###.####..##..#
19:       ##.####..##.#..######..#...#...###.####
20:      ##..#...###..####.....####.###.##...#...#
21:     ##.####.##..###...#...##....#...#.#.###.###
22:    ##..#....#.###..#.###.##.#..###.##.#.#...#..#
23:   ##.####..##.#..###.#...#..####...#..#.##.######
24:  ##..#...###..####...##.#####...#.#####.#..#.....#

```


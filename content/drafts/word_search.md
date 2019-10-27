+++
title = "Word search"
description = ""
date = 2019-02-27T01:04:29Z
aliases = []
[extra]
id = 20673
[taxonomies]
categories = []
tags = []
+++

{{task}}

A [[wp:Word_search|word search]] puzzle typically consists of a grid of letters in which words are hidden.

There are many varieties of word search puzzles. For the task at hand we will use a rectangular grid in which the words may be placed horizontally, vertically, or diagonally. The words may also be spelled backwards.

The words may overlap but are not allowed to zigzag, or wrap around.



;Task 
Create a 10 by 10 word search and fill it using words from the [http://www.puzzlers.org/pub/wordlists/unixdict.txt unixdict]. Use only words that are longer than 2, and contain no non-alphabetic characters.

The cells not used by the hidden words should contain the message: ''Rosetta Code'', read from left to right, top to bottom. These letters should be somewhat evenly distributed over the grid, not clumped together. The message should be in upper case, the hidden words in lower case. All cells should either contain letters from the hidden words or from the message. 

Pack a minimum of 25 words into the grid.

Print the resulting grid and the solutions.



;Example


```txt
     0  1  2  3  4  5  6  7  8  9

0    n  a  y  r  y  R  e  l  m  f 
1    y  O  r  e  t  s  g  n  a  g 
2    t  n  e  d  i  S  k  y  h  E 
3    n  o  t  n  c  p  c  w  t  T 
4    a  l  s  u  u  n  T  m  a  x 
5    r  o  k  p  a  r  i  s  h  h 
6    a  A  c  f  p  a  e  a  c  C 
7    u  b  u  t  t  t  O  l  u  n 
8    g  y  h  w  a  D  h  p  m  u 
9    m  i  r  p  E  h  o  g  a  n 

parish     (3,5)(8,5)   gangster   (9,1)(2,1)
paucity    (4,6)(4,0)   guaranty   (0,8)(0,1)
prim       (3,9)(0,9)   huckster   (2,8)(2,1)
plasm      (7,8)(7,4)   fancy      (3,6)(7,2)
hogan      (5,9)(9,9)   nolo       (1,2)(1,5)
under      (3,4)(3,0)   chatham    (8,6)(8,0)
ate        (4,8)(6,6)   nun        (9,7)(9,9)
butt       (1,7)(4,7)   hawk       (9,5)(6,2)
why        (3,8)(1,8)   ryan       (3,0)(0,0)
fay        (9,0)(7,2)   much       (8,8)(8,5)
tar        (5,7)(5,5)   elm        (6,0)(8,0)
max        (7,4)(9,4)   pup        (5,3)(3,5)
mph        (8,8)(6,8)
```






## C++


```cpp

#include <iomanip>
#include <ctime>
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <fstream>

const int WID = 10, HEI = 10, MIN_WORD_LEN = 3, MIN_WORD_CNT = 25;

class Cell {
public:
    Cell() : val( 0 ), cntOverlap( 0 ) {}
    char val; int cntOverlap;
};
class Word {
public:
    Word( std::string s, int cs, int rs, int ce, int re, int dc, int dr ) : 
      word( s ), cols( cs ), rows( rs ), cole( ce ), rowe( re ), dx( dc ), dy( dr ) {}
    bool operator ==( const std::string& s ) { return 0 == word.compare( s ); }
    std::string word;
    int cols, rows, cole, rowe, dx, dy;
};
class words {
public:
    void create( std::string& file ) {
        std::ifstream f( file.c_str(), std::ios_base::in );
        std::string word;
        while( f >> word ) {
            if( word.length() < MIN_WORD_LEN || word.length() > WID || word.length() > HEI ) continue;
            if( word.find_first_not_of( "abcdefghijklmnopqrstuvwxyz" ) != word.npos ) continue;
            dictionary.push_back( word );
        }
        f.close();
        std::random_shuffle( dictionary.begin(), dictionary.end() );
        buildPuzzle();
    }

    void printOut() {
        std::cout << "\t";
        for( int x = 0; x < WID; x++ ) std::cout << x << "  ";
        std::cout << "\n\n";
        for( int y = 0; y < HEI; y++ ) {
            std::cout << y << "\t";
            for( int x = 0; x < WID; x++ )
                std::cout << puzzle[x][y].val << "  ";
            std::cout << "\n";
        }
        size_t wid1 = 0, wid2 = 0;
        for( size_t x = 0; x < used.size(); x++ ) {
            if( x & 1 ) {
                if( used[x].word.length() > wid1 ) wid1 = used[x].word.length();
            } else {
                if( used[x].word.length() > wid2 ) wid2 = used[x].word.length();
            }
        }
        std::cout << "\n";
        std::vector<Word>::iterator w = used.begin();
        while( w != used.end() ) {
            std::cout << std::right << std::setw( wid1 ) << ( *w ).word << " (" << ( *w ).cols << ", " << ( *w ).rows << ") (" 
                      << ( *w ).cole << ", " << ( *w ).rowe << ")\t";
            w++;
            if( w == used.end() ) break;
            std::cout << std::setw( wid2 ) << ( *w ).word << " (" << ( *w ).cols << ", " << ( *w ).rows << ") (" 
                      << ( *w ).cole << ", " << ( *w ).rowe << ")\n";
            w++;
        }
        std::cout << "\n\n";
    }
private:
    void addMsg() {
        std::string msg = "ROSETTACODE";
        int stp = 9, p = rand() % stp;
        for( size_t x = 0; x < msg.length(); x++ ) {
            puzzle[p % WID][p / HEI].val = msg.at( x );
            p += rand() % stp + 4;
        }
    }
    int getEmptySpaces() {
        int es = 0;
        for( int y = 0; y < HEI; y++ ) {
            for( int x = 0; x < WID; x++ ) {
                if( !puzzle[x][y].val ) es++;
            }
        }
        return es;
    }
    bool check( std::string word, int c, int r, int dc, int dr ) {
        for( size_t a = 0; a < word.length(); a++ ) {
            if( c < 0 || r < 0 || c >= WID || r >= HEI ) return false;
            if( puzzle[c][r].val && puzzle[c][r].val != word.at( a ) ) return false;
            c += dc; r += dr;
        }
        return true;
    }
    bool setWord( std::string word, int c, int r, int dc, int dr ) {
        if( !check( word, c, r, dc, dr ) ) return false;
        int sx = c, sy = r;
        for( size_t a = 0; a < word.length(); a++ ) {
            if( !puzzle[c][r].val ) puzzle[c][r].val = word.at( a );
            else puzzle[c][r].cntOverlap++;
            c += dc; r += dr;
        }
        used.push_back( Word( word, sx, sy, c - dc, r - dr, dc, dr ) );
        return true;
    }
    bool add2Puzzle( std::string word ) {
        int x = rand() % WID, y = rand() % HEI,
            z = rand() % 8;
        for( int d = z; d < z + 8; d++ ) {
            switch( d % 8 ) {
                case 0: if( setWord( word, x, y,  1,  0 ) ) return true; break;
                case 1: if( setWord( word, x, y, -1, -1 ) ) return true; break;
                case 2: if( setWord( word, x, y,  0,  1 ) ) return true; break;
                case 3: if( setWord( word, x, y,  1, -1 ) ) return true; break;
                case 4: if( setWord( word, x, y, -1,  0 ) ) return true; break;
                case 5: if( setWord( word, x, y, -1,  1 ) ) return true; break;
                case 6: if( setWord( word, x, y,  0, -1 ) ) return true; break;
                case 7: if( setWord( word, x, y,  1,  1 ) ) return true; break;
            }
        }
        return false;
    }
    void clearWord() {
        if( used.size() ) {
            Word lastW = used.back();
            used.pop_back();

            for( size_t a = 0; a < lastW.word.length(); a++ ) {
                if( puzzle[lastW.cols][lastW.rows].cntOverlap == 0 ) {
                    puzzle[lastW.cols][lastW.rows].val = 0;
                }
                if( puzzle[lastW.cols][lastW.rows].cntOverlap > 0 ) {
                    puzzle[lastW.cols][lastW.rows].cntOverlap--;
                }
                lastW.cols += lastW.dx; lastW.rows += lastW.dy;
            }
        }
    }
    void buildPuzzle() {
        addMsg();
        int es = 0, cnt = 0;
        size_t idx = 0;
        do {
            for( std::vector<std::string>::iterator w = dictionary.begin(); w != dictionary.end(); w++ ) {
                if( std::find( used.begin(), used.end(), *w ) != used.end() ) continue;
            
                if( add2Puzzle( *w ) ) {
                    es = getEmptySpaces();
                    if( !es && used.size() >= MIN_WORD_CNT ) 
                        return;
                }
            }
            clearWord();
            std::random_shuffle( dictionary.begin(), dictionary.end() );

        } while( ++cnt < 100 );
    }
    std::vector<Word> used;
    std::vector<std::string> dictionary;
    Cell puzzle[WID][HEI];
};
int main( int argc, char* argv[] ) {
    unsigned s = unsigned( time( 0 ) );
    srand( s );
    words w; w.create( std::string( "unixdict.txt" ) );
    w.printOut();
    return 0;
}

```

{{out}}

```txt


        0  1  2  3  4  5  6  7  8  9

0       d  b  R  f  t  a  u  n  p  w
1       O  i  l  o  b  h  a  m  a  o
2       S  r  e  e  r  p  E  t  r  h
3       e  c  o  r  a  T  l  i  e  T
4       f  a  m  e  w  A  e  n  t  s
5       l  n  C  h  u  y  p  g  o  l
6       o  n  p  t  O  n  s  e  o  D
7       w  e  u  b  e  f  i  a  c  b
8       E  i  n  e  m  a  d  a  m  e
9       e  s  s  k  a  p  l  a  n  e

thereof (3, 6) (3, 0)      seen (2, 9) (5, 6)
pareto (8, 0) (8, 5)       wolf (0, 7) (0, 4)
  crib (1, 3) (1, 0)      tinge (7, 2) (7, 6)
sienna (1, 9) (1, 4)        war (4, 4) (4, 2)
dispel (6, 8) (6, 3)     kaplan (3, 9) (8, 9)
   tau (4, 0) (6, 0)        lob (2, 1) (4, 1)
   how (9, 2) (9, 0)       same (6, 6) (9, 9)
   men (4, 8) (2, 8)        feb (5, 7) (3, 7)
   ham (5, 1) (7, 1)        moe (2, 4) (2, 2)
   pan (5, 2) (7, 0)        yuh (5, 5) (3, 5)
   pun (2, 6) (2, 8)       load (9, 5) (6, 8)
   can (1, 3) (1, 5)     madame (4, 8) (9, 8)
   gob (7, 5) (9, 7)        rib (1, 2) (1, 0)
   nee (5, 6) (3, 8)        set (9, 4) (7, 2)
   alp (7, 9) (5, 9)      wolfe (0, 7) (0, 3)
   the (3, 6) (3, 4)        low (0, 5) (0, 7)
   tea (3, 6) (5, 8)        era (8, 3) (8, 1)
   nne (1, 5) (1, 7)       amen (5, 8) (2, 8)
  coot (8, 7) (8, 4)       anne (1, 4) (1, 7)
  reid (3, 3) (0, 0)        sse (2, 9) (0, 9)

```



## C sharp

{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Wordseach
{
    static class Program
    {
        readonly static int[,] dirs = {{1, 0}, {0, 1}, {1, 1}, {1, -1}, {-1, 0},
            {0, -1}, {-1, -1}, {-1, 1}};

        class Grid
        {
            public char[,] Cells = new char[nRows, nCols];
            public List<string> Solutions = new List<string>();
            public int NumAttempts;
        }

        readonly static int nRows = 10;
        readonly static int nCols = 10;
        readonly static int gridSize = nRows * nCols;
        readonly static int minWords = 25;

        readonly static Random rand = new Random();

        static void Main(string[] args)
        {
            PrintResult(CreateWordSearch(ReadWords("unixdict.txt")));
        }

        private static List<string> ReadWords(string filename)
        {
            int maxLen = Math.Max(nRows, nCols);

            return System.IO.File.ReadAllLines(filename)
                .Select(s => s.Trim().ToLower())
                .Where(s => Regex.IsMatch(s, "^[a-z]{3," + maxLen + "}$"))
                .ToList();
        }

        private static Grid CreateWordSearch(List<string> words)
        {
            int numAttempts = 0;

            while (++numAttempts < 100)
            {
                words.Shuffle();

                var grid = new Grid();
                int messageLen = PlaceMessage(grid, "Rosetta Code");
                int target = gridSize - messageLen;

                int cellsFilled = 0;
                foreach (var word in words)
                {
                    cellsFilled += TryPlaceWord(grid, word);
                    if (cellsFilled == target)
                    {
                        if (grid.Solutions.Count >= minWords)
                        {
                            grid.NumAttempts = numAttempts;
                            return grid;
                        }
                        else break; // grid is full but we didn't pack enough words, start over
                    }
                }
            }
            return null;
        }

        private static int TryPlaceWord(Grid grid, string word)
        {
            int randDir = rand.Next(dirs.GetLength(0));
            int randPos = rand.Next(gridSize);

            for (int dir = 0; dir < dirs.GetLength(0); dir++)
            {
                dir = (dir + randDir) % dirs.GetLength(0);

                for (int pos = 0; pos < gridSize; pos++)
                {
                    pos = (pos + randPos) % gridSize;

                    int lettersPlaced = TryLocation(grid, word, dir, pos);
                    if (lettersPlaced > 0)
                        return lettersPlaced;
                }
            }
            return 0;
        }

        private static int TryLocation(Grid grid, string word, int dir, int pos)
        {
            int r = pos / nCols;
            int c = pos % nCols;
            int len = word.Length;

            //  check bounds
            if ((dirs[dir, 0] == 1 && (len + c) > nCols)
                    || (dirs[dir, 0] == -1 && (len - 1) > c)
                    || (dirs[dir, 1] == 1 && (len + r) > nRows)
                    || (dirs[dir, 1] == -1 && (len - 1) > r))
                return 0;

            int rr, cc, i, overlaps = 0;

            // check cells
            for (i = 0, rr = r, cc = c; i < len; i++)
            {
                if (grid.Cells[rr, cc] != 0 && grid.Cells[rr, cc] != word[i])
                {
                    return 0;
                }

                cc += dirs[dir, 0];
                rr += dirs[dir, 1];
            }

            // place
            for (i = 0, rr = r, cc = c; i < len; i++)
            {
                if (grid.Cells[rr, cc] == word[i])
                    overlaps++;
                else
                    grid.Cells[rr, cc] = word[i];

                if (i < len - 1)
                {
                    cc += dirs[dir, 0];
                    rr += dirs[dir, 1];
                }
            }

            int lettersPlaced = len - overlaps;
            if (lettersPlaced > 0)
            {
                grid.Solutions.Add($"{word,-10} ({c},{r})({cc},{rr})");
            }

            return lettersPlaced;
        }

        private static int PlaceMessage(Grid grid, string msg)
        {
            msg = Regex.Replace(msg.ToUpper(), "[^A-Z]", "");

            int messageLen = msg.Length;
            if (messageLen > 0 && messageLen < gridSize)
            {
                int gapSize = gridSize / messageLen;

                for (int i = 0; i < messageLen; i++)
                {
                    int pos = i * gapSize + rand.Next(gapSize);
                    grid.Cells[pos / nCols, pos % nCols] = msg[i];
                }
                return messageLen;
            }
            return 0;
        }

        public static void Shuffle<T>(this IList<T> list)
        {
            int n = list.Count;
            while (n > 1)
            {
                n--;
                int k = rand.Next(n + 1);
                T value = list[k];
                list[k] = list[n];
                list[n] = value;
            }
        }

        private static void PrintResult(Grid grid)
        {
            if (grid == null || grid.NumAttempts == 0)
            {
                Console.WriteLine("No grid to display");
                return;
            }
            int size = grid.Solutions.Count;

            Console.WriteLine("Attempts: " + grid.NumAttempts);
            Console.WriteLine("Number of words: " + size);

            Console.WriteLine("\n     0  1  2  3  4  5  6  7  8  9");
            for (int r = 0; r < nRows; r++)
            {
                Console.Write("\n{0}   ", r);
                for (int c = 0; c < nCols; c++)
                    Console.Write(" {0} ", grid.Cells[r, c]);
            }

            Console.WriteLine("\n");

            for (int i = 0; i < size - 1; i += 2)
            {
                Console.WriteLine("{0}   {1}", grid.Solutions[i],
                        grid.Solutions[i + 1]);
            }
            if (size % 2 == 1)
                Console.WriteLine(grid.Solutions[size - 1]);

            Console.ReadLine();
        }        
    }
}
```



```txt

Attempts: 1
Number of words: 28

     0  1  2  3  4  5  6  7  8  9

0    i  m  n  e  p  o  R  p  i  d
1    s  u  r  O  e  l  d  n  i  b
2    n  e  S  b  a  n  d  y  a  E
3    a  s  d  i  t  h  y  t  T  b
4    m  u  a  r  n  s  a  u  d  r
5    s  m  h  T  o  s  o  i  h  o
6    d  A  c  r  t  C  p  c  r  t
7    a  y  p  e  p  a  n  O  c  h
8    e  o  D  o  r  u  x  g  s  a
9    l  b  E  w  l  a  k  n  o  h

rapid      (4,8)(8,4)   bindle     (9,1)(4,1)
bandy      (3,2)(7,2)   leadsman   (0,9)(0,2)
accost     (9,8)(4,3)   museum     (1,5)(1,0)
taste      (7,3)(3,7)   broth      (9,3)(9,7)
rosy       (3,6)(6,3)   honk       (9,9)(6,9)
chad       (2,6)(2,3)   lunch      (4,9)(8,5)
open       (5,0)(2,0)   gsa        (7,8)(9,8)
dip        (9,0)(7,0)   ansi       (0,3)(0,0)
pol        (2,7)(0,9)   boy        (1,9)(1,7)
woe        (3,9)(3,7)   tax        (4,6)(6,8)
rib        (3,4)(3,2)   not        (4,4)(4,6)
hair       (5,3)(8,6)   bat        (9,1)(7,3)
nyu        (5,2)(7,4)   ape        (5,7)(3,7)
era        (3,7)(5,9)   ere        (1,2)(3,0)

```



## D

{{trans|Java}}

```D
import std.random : Random, uniform, randomShuffle;
import std.stdio;

immutable int[][] dirs = [
    [1,  0], [ 0,  1], [ 1, 1],
    [1, -1],           [-1, 0],
    [0, -1], [-1, -1], [-1, 1]
];

enum nRows = 10;
enum nCols = 10;
enum gridSize = nRows * nCols;
enum minWords = 25;

auto rnd = Random();

class Grid {
    int numAttempts;
    char[nRows][nCols] cells;
    string[] solutions;

    this() {
        for(int row=0; row<nRows; ++row) {
            cells[row] = 0;
        }
    }
}

void main() {
    printResult(createWordSearch(readWords("unixdict.txt")));
}

string[] readWords(string filename) {
    import std.algorithm : all, max;
    import std.ascii : isAlpha;
    import std.string : chomp, toLower;

    auto maxlen = max(nRows, nCols);

    string[] words;
    auto source = File(filename);
    foreach(line; source.byLine) {
        chomp(line);
        if (line.length >= 3 && line.length <= maxlen) {
            if (all!isAlpha(line)) {
                words ~= line.toLower.idup;
            }
        }
    }

    return words;
}

Grid createWordSearch(string[] words) {
    Grid grid;
    int numAttempts;

    outer:
    while(++numAttempts < 100) {
        randomShuffle(words);

        grid = new Grid();
        int messageLen = placeMessage(grid, "Rosetta Code");
        int target = gridSize - messageLen;

        int cellsFilled;
        foreach (string word; words) {
            cellsFilled += tryPlaceWord(grid, word);
            if (cellsFilled == target) {
                if (grid.solutions.length >= minWords) {
                    grid.numAttempts = numAttempts;
                    break outer;
                } else break; // grid is full but we didn't pack enough words, start over
            }
        }
    }
    return grid;
}

int placeMessage(Grid grid, string msg) {
    import std.algorithm : filter;
    import std.ascii : isUpper;
    import std.conv : to;
    import std.string : toUpper;

    msg = to!string(msg.toUpper.filter!isUpper);

    if (msg.length > 0 && msg.length < gridSize) {
        int gapSize = gridSize / msg.length;

        for (int i=0; i<msg.length; i++) {
            int pos = i * gapSize + uniform(0, gapSize, rnd);
            grid.cells[pos / nCols][pos % nCols] = msg[i];
        }
        return msg.length;
    }
    return 0;
}

int tryPlaceWord(Grid grid, string word) {
    int randDir = uniform(0, dirs.length, rnd);
    int randPos = uniform(0, gridSize, rnd);

    for (int dir=0; dir<dirs.length; dir++) {
        dir = (dir + randDir) % dirs.length;

        for (int pos=0; pos<gridSize; pos++) {
            pos = (pos + randPos) % gridSize;

            int lettersPlaced = tryLocation(grid, word, dir, pos);
            if (lettersPlaced > 0) {
                return lettersPlaced;
            }
        }
    }
    return 0;
}

int tryLocation(Grid grid, string word, int dir, int pos) {
    import std.format;

    int r = pos / nCols;
    int c = pos % nCols;
    int len = word.length;

    //  check bounds
    if ((dirs[dir][0] == 1 && (len + c) > nCols)
            || (dirs[dir][0] == -1 && (len - 1) > c)
            || (dirs[dir][1] == 1 && (len + r) > nRows)
            || (dirs[dir][1] == -1 && (len - 1) > r)) {
        return 0;
    }

    int i, rr, cc, overlaps = 0;

    // check cells
    for (i=0, rr=r, cc=c; i<len; i++) {
        if (grid.cells[rr][cc] != 0 && grid.cells[rr][cc] != word[i]) {
            return 0;
        }
        cc += dirs[dir][0];
        rr += dirs[dir][1];
    }

    // place
    for (i=0, rr=r, cc=c; i<len; i++) {
        if (grid.cells[rr][cc] == word[i]) {
            overlaps++;
        } else {
            grid.cells[rr][cc] = word[i];
        }

        if (i < len - 1) {
            cc += dirs[dir][0];
            rr += dirs[dir][1];
        }
    }

    int lettersPlaced = len - overlaps;
    if (lettersPlaced > 0) {
        grid.solutions ~= format("%-10s (%d,%d)(%d,%d)", word, c, r, cc, rr);
    }

    return lettersPlaced;
}

void printResult(Grid grid) {
    if (grid is null || grid.numAttempts == 0) {
        writeln("No grid to display");
        return;
    }
    int size = grid.solutions.length;

    writeln("Attempts: ", grid.numAttempts);
    writeln("Number of words: ", size);

    writeln("\n     0  1  2  3  4  5  6  7  8  9");
    for (int r=0; r<nRows; r++) {
        writef("\n%d   ", r);
        for (int c=0; c<nCols; c++) {
            writef(" %c ", grid.cells[r][c]);
        }
    }

    writeln;
    writeln;

    for (int i=0; i<size-1; i+=2) {
        writef("%s   %s\n", grid.solutions[i], grid.solutions[i + 1]);
    }
    if (size % 2 == 1) {
        writeln(grid.solutions[size - 1]);
    }
}
```


{{out}}

```txt
Attempts: 1
Number of words: 30

     0  1  2  3  4  5  6  7  8  9

0    e  w  R  m  p  u  n  i  s  h
1    y  e  a  g  h  O  a  s  r  r
2    S  c  g  y  i  p  r  p  i  a
3    h  v  E  c  s  g  e  T  c  b
4    t  o  f  c  m  w  T  s  o  n
5    o  n  o  t  e  h  a  i  o  A
6    t  n  g  e  u  l  s  g  n  j
7    o  C  y  l  l  e  u  i  O  w
8    e  o  k  f  a  l  l  d  D  o
9    n  m  i  l  l  i  e  E  e  o

etc        (3,6)(3,4)   boise      (9,3)(5,7)
joseph     (9,6)(4,1)   von        (1,3)(1,5)
elude      (4,5)(8,9)   toe        (0,6)(0,8)
swag       (4,3)(7,6)   hulk       (5,5)(2,8)
psi        (7,2)(7,0)   millie     (1,9)(6,9)
mcgee      (4,4)(0,0)   mach       (3,0)(0,3)
yip        (3,2)(5,2)   fall       (3,8)(6,8)
punish     (4,0)(9,0)   fogy       (2,4)(2,7)
rico       (8,1)(8,4)   woo        (9,7)(9,9)
gmt        (5,3)(3,5)   tot        (0,6)(0,4)
lin        (6,8)(8,6)   bar        (9,3)(9,1)
era        (6,3)(6,1)   son        (7,4)(9,4)
way        (1,0)(3,2)   con        (3,4)(1,6)
yon        (2,7)(0,9)   ell        (6,9)(4,7)
gig        (5,3)(3,1)   yea        (0,1)(2,1)
```



## Go

{{trans|Java}}


The link to "unixdict" appears to be broken so I've used instead the dictionary at "/usr/share/dict/words" which came pre-installed with my Ubuntu 16.04 distribution. I've no idea whether these dictionaries are the same or not.

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "math/rand"
    "os"
    "regexp"
    "strings"
    "time"
)

var dirs = [][]int{{1, 0}, {0, 1}, {1, 1}, {1, -1}, {-1, 0}, {0, -1}, {-1, -1}, {-1, 1}}

const (
    nRows    = 10
    nCols    = nRows
    gridSize = nRows * nCols
    minWords = 25
)

var (
    re1 = regexp.MustCompile(fmt.Sprintf("^[a-z]{3,%d}$", nRows))
    re2 = regexp.MustCompile("[^A-Z]")
)

type grid struct {
    numAttempts int
    cells       [nRows][nCols]byte
    solutions   []string
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func readWords(fileName string) []string {
    file, err := os.Open(fileName)
    check(err)
    defer file.Close()
    var words []string
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        word := strings.ToLower(strings.TrimSpace(scanner.Text()))
        if re1.MatchString(word) {
            words = append(words, word)
        }
    }
    check(scanner.Err())
    return words
}

func createWordSearch(words []string) *grid {
    var gr *grid
outer:
    for i := 1; i < 100; i++ {
        gr = new(grid)
        messageLen := gr.placeMessage("Rosetta Code")
        target := gridSize - messageLen
        cellsFilled := 0
        rand.Shuffle(len(words), func(i, j int) {
            words[i], words[j] = words[j], words[i]
        })
        for _, word := range words {
            cellsFilled += gr.tryPlaceWord(word)
            if cellsFilled == target {
                if len(gr.solutions) >= minWords {
                    gr.numAttempts = i
                    break outer
                } else { // grid is full but we didn't pack enough words, start over
                    break
                }
            }
        }
    }
    return gr
}

func (gr *grid) placeMessage(msg string) int {
    msg = strings.ToUpper(msg)
    msg = re2.ReplaceAllLiteralString(msg, "")
    messageLen := len(msg)
    if messageLen > 0 && messageLen < gridSize {
        gapSize := gridSize / messageLen
        for i := 0; i < messageLen; i++ {
            pos := i*gapSize + rand.Intn(gapSize)
            gr.cells[pos/nCols][pos%nCols] = msg[i]
        }
        return messageLen
    }
    return 0
}

func (gr *grid) tryPlaceWord(word string) int {
    randDir := rand.Intn(len(dirs))
    randPos := rand.Intn(gridSize)
    for dir := 0; dir < len(dirs); dir++ {
        dir = (dir + randDir) % len(dirs)
        for pos := 0; pos < gridSize; pos++ {
            pos = (pos + randPos) % gridSize
            lettersPlaced := gr.tryLocation(word, dir, pos)
            if lettersPlaced > 0 {
                return lettersPlaced
            }
        }
    }
    return 0
}

func (gr *grid) tryLocation(word string, dir, pos int) int {
    r := pos / nCols
    c := pos % nCols
    le := len(word)

    // check bounds
    if (dirs[dir][0] == 1 && (le+c) > nCols) ||
        (dirs[dir][0] == -1 && (le-1) > c) ||
        (dirs[dir][1] == 1 && (le+r) > nRows) ||
        (dirs[dir][1] == -1 && (le-1) > r) {
        return 0
    }
    overlaps := 0

    // check cells
    rr := r
    cc := c
    for i := 0; i < le; i++ {
        if gr.cells[rr][cc] != 0 && gr.cells[rr][cc] != word[i] {
            return 0
        }
        cc += dirs[dir][0]
        rr += dirs[dir][1]
    }

    // place
    rr = r
    cc = c
    for i := 0; i < le; i++ {
        if gr.cells[rr][cc] == word[i] {
            overlaps++
        } else {
            gr.cells[rr][cc] = word[i]
        }
        if i < le-1 {
            cc += dirs[dir][0]
            rr += dirs[dir][1]
        }
    }

    lettersPlaced := le - overlaps
    if lettersPlaced > 0 {
        sol := fmt.Sprintf("%-10s (%d,%d)(%d,%d)", word, c, r, cc, rr)
        gr.solutions = append(gr.solutions, sol)
    }
    return lettersPlaced
}

func printResult(gr *grid) {
    if gr.numAttempts == 0 {
        fmt.Println("No grid to display")
        return
    }
    size := len(gr.solutions)
    fmt.Println("Attempts:", gr.numAttempts)
    fmt.Println("Number of words:", size)
    fmt.Println("\n     0  1  2  3  4  5  6  7  8  9")
    for r := 0; r < nRows; r++ {
        fmt.Printf("\n%d   ", r)
        for c := 0; c < nCols; c++ {
            fmt.Printf(" %c ", gr.cells[r][c])
        }
    }
    fmt.Println("\n")
    for i := 0; i < size-1; i += 2 {
        fmt.Printf("%s   %s\n", gr.solutions[i], gr.solutions[i+1])
    }
    if size%2 == 1 {
        fmt.Println(gr.solutions[size-1])
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    unixDictPath := "/usr/share/dict/words"
    printResult(createWordSearch(readWords(unixDictPath)))
}
```


{{out}}
Sample run:

```txt

Attempts: 2
Number of words: 28

     0  1  2  3  4  5  6  7  8  9

0    d  R  g  n  i  p  l  e  h  w 
1    o  O  o  e  g  n  i  h  n  u 
2    r  e  b  c  p  S  o  e  r  c 
3    s  l  E  g  o  j  l  e  h  s 
4    T  z  e  n  i  g  m  a  h  T 
5    s  z  e  i  o  a  n  o  o  p 
6    n  u  A  d  e  c  C  a  e  u 
7    a  p  a  r  e  l  O  n  c  t 
8    o  D  c  i  m  a  t  h  e  s 
9    r  a  E  b  e  r  e  s  i  r 

cognacs    (3,2)(9,8)   unhinge    (9,1)(3,1)
creamer    (2,8)(8,2)   whelping   (9,0)(2,0)
puzzle     (1,7)(1,2)   math       (4,8)(7,8)
birding    (3,9)(3,3)   roans      (0,9)(0,5)
riser      (9,9)(5,9)   pent       (9,5)(6,8)
chance     (9,2)(4,7)   poona      (9,5)(5,5)
enigma     (2,4)(7,4)   noes       (3,0)(0,3)
ogle       (4,5)(7,2)   puts       (9,5)(9,8)
rod        (0,2)(0,0)   sere       (7,9)(4,9)
ohs        (7,5)(9,3)   jog        (5,3)(3,3)
lei        (5,7)(3,5)   bog        (2,2)(2,0)
hes        (7,8)(9,8)   noe        (5,1)(7,3)
peg        (4,2)(2,0)   ado        (2,7)(4,5)
one        (4,3)(2,5)   acre       (1,9)(4,6)

```



## J


Implementation:


```J
require'web/gethttp'

unixdict=:verb define
  if. _1 -: fread 'unixdict.txt' do.
    (gethttp 'http://www.puzzlers.org/pub/wordlists/unixdict.txt') fwrite 'unixdict.txt'
  end.
  fread 'unixdict.txt'
)

words=:verb define
  (#~ 1 - 0&e.@e.&'abcdefghijklmnopqrstuvwxyz'@>) (#~ [: (2&< * 10&>:) #@>) <;._2 unixdict''
)

dirs=: 10#.0 0-.~>,{,~<i:1
lims=: _10+,"2 +/&>/"1 (0~:i:4)#>,{,~<<"1]1 10 1 +i."0]10*i:_1
dnms=: ;:'nw north ne west east sw south se'

genpuz=:verb define
  words=. words''
  fill=. 'ROSETTACODE'
  grid=. ,10 10$' '
  inds=. ,i.10 10
  patience=. -:#words
  key=. i.0 0
  inuse=. i.0 2
  while. (26>#key)+.0<cap=. (+/' '=grid)-#fill do.
    word=. >({~ ?@#) words
    dir=. ?@#dirs
    offs=. (inds#~(#word)<:inds{dir{lims)+/(i.#word)*/dir{dirs
    cool=. ' '=offs{grid
    sel=. */"1 cool+.(offs{grid)="1 word
    offs=. (sel*cap>:+/"1 cool)#offs
    if. (#offs) do.
      off=. ({~ ?@#) offs
      loc=. ({.off),dir
      if. -. loc e. inuse do.
        inuse=. inuse,loc
        grid=. word off} grid
        patience=. patience+1
        key=. /:~ key,' ',(10{.word),(3":1+10 10#:{.off),' ',dir{::dnms
      end.
    else. NB. grr...
      if. 0 > patience=. patience-1 do.
        inuse=.i.0 2
        key=.i.0 0
        grid=. ,10 10$' '
        patience=. -:#words
      end.
    end.
  end.
  puz=. (_23{.":i.10),' ',1j1#"1(":i.10 1),.' ',.10 10$fill (I.grid=' ')} grid
  puz,' ',1 1}._1 _1}.":((</.~ <.) i.@# * 3%#)key
)
```


Notes:

While the result is square, we flatten our intermediate results to simplify the code.

<code>dirs</code> are index offsets within the flattened grid for each of the eight cardinal directions.

<code>lims</code> is, for each cardinal direction, and for each grid position, how long of a word can fit.

<code>dnms</code> are names for each of the cardinal directions.

<code>words</code> are the viable words from unixdict, and <code>fill</code> is what we're going to leave in the puzzle for spaces not occupied by any of those words (and this could be made into a parameter).

<code>grid</code> is our working copy of the text of the word search puzzle.

<code>inds</code> are the indices into grid - we will use these as the starting positions when we place the words.

<code>patience</code> is a guard variable, to avoid problems with infinite loops if we arbitrarily place words in a non-viable fashion.

<code>key</code> lists the words we are placing, and where we placed them.

<code>inuse</code> marks location+directions which already have a word (to prevent short words such as ''sal'' from being placed as prefixes of longer words such as ''sale''). 

Once we have these, we go into a loop where:

<code>word</code> is picked arbitrarily from the viable words from unixdict.

<code>dir</code> is picked arbitrarily from one of our eight cardinal directions.

<code>offs</code> are places where we might place the word (initially limited only by geometry, but we then constrain this based on what's already been placed).

<code>cool</code> marks where our word can be placed in unoccupied spaces (and also will be used later to count how many new spaces will be occupied by the word we pick.

<code>sel</code> marks where our word can be placed such that it does not conflict with existing words.

If this leaves us with any way to place the word, we pick one of them as <code>off</code> and combine the starting location with dir in <code>loc</code> to see if a word has already been placed there and if we're good, we place the word and update our key. (It's extremely rare that loc matches an inuse location, so just ignoring that word works just fine).

Otherwise, we check if we're getting impatient (in which case we scrap the entire thing and start over).

Once we're done, we reshape our grid so it's square and attach the key. Here, <code>puz</code> is the grid formatted for display (with a space between each column, and a numeric key for each row and column).

Example run:


```J
   genpuz''
    0 1 2 3 4 5 6 7 8 9                                                
                                                                       
0   y R p y r f O a p S                                                
1   l o l s i f c c e a                                                
2   l n v z i e n r n l                                                
3   o p z s t e E i n l                                                
4   h l s a v e r d a o                                                
5   e a t a g r e e d y                                                
6   m e m a g T f T A C                                                
7   a y e r s p f z a p                                                
8   O e c n a w o l l a                                                
9   e s o p o r p c D E                                                
                                                                       
 acetate     1  8 sw   │ gam         7  5 west │ pol         1  3 sw   
 acrid       1  8 south│ holly       5  1 north│ propose    10  7 west 
 agreed      6  4 east │ massif      7  1 ne   │ rsvp        1  5 sw   
 allowance   9 10 west │ neva        3  7 sw   │ sao         8  5 south
 alloy       2 10 south│ offer       9  7 north│ save        5  3 east 
 arm         9  5 nw   │ only        4  1 ne   │ sop        10  2 east 
 ayers       8  1 east │ pap        10  4 ne   │ tee         4  5 se   
 cop        10  8 nw   │ paz         8 10 west │ wan         9  6 west 
 fizzle      1  6 sw   │ penna       1  9 south│                       

```



## Java

{{works with|Java|7}}

```java
import java.io.*;
import static java.lang.String.format;
import java.util.*;

public class WordSearch {
    static class Grid {
        int numAttempts;
        char[][] cells = new char[nRows][nCols];
        List<String> solutions = new ArrayList<>();
    }

    final static int[][] dirs = {{1, 0}, {0, 1}, {1, 1}, {1, -1}, {-1, 0},
    {0, -1}, {-1, -1}, {-1, 1}};

    final static int nRows = 10;
    final static int nCols = 10;
    final static int gridSize = nRows * nCols;
    final static int minWords = 25;

    final static Random rand = new Random();

    public static void main(String[] args) {
        printResult(createWordSearch(readWords("unixdict.txt")));
    }

    static List<String> readWords(String filename) {
        int maxLen = Math.max(nRows, nCols);

        List<String> words = new ArrayList<>();
        try (Scanner sc = new Scanner(new FileReader(filename))) {
            while (sc.hasNext()) {
                String s = sc.next().trim().toLowerCase();
                if (s.matches("^[a-z]{3," + maxLen + "}$"))
                    words.add(s);
            }
        } catch (FileNotFoundException e) {
            System.out.println(e);
        }
        return words;
    }

    static Grid createWordSearch(List<String> words) {
        Grid grid = null;
        int numAttempts = 0;

        outer:
        while (++numAttempts < 100) {
            Collections.shuffle(words);

            grid = new Grid();
            int messageLen = placeMessage(grid, "Rosetta Code");
            int target = gridSize - messageLen;

            int cellsFilled = 0;
            for (String word : words) {
                cellsFilled += tryPlaceWord(grid, word);
                if (cellsFilled == target) {
                    if (grid.solutions.size() >= minWords) {
                        grid.numAttempts = numAttempts;
                        break outer;
                    } else break; // grid is full but we didn't pack enough words, start over
                }
            }
        }

        return grid;
    }

    static int placeMessage(Grid grid, String msg) {
        msg = msg.toUpperCase().replaceAll("[^A-Z]", "");

        int messageLen = msg.length();
        if (messageLen > 0 && messageLen < gridSize) {
            int gapSize = gridSize / messageLen;

            for (int i = 0; i < messageLen; i++) {
                int pos = i * gapSize + rand.nextInt(gapSize);
                grid.cells[pos / nCols][pos % nCols] = msg.charAt(i);
            }
            return messageLen;
        }
        return 0;
    }

    static int tryPlaceWord(Grid grid, String word) {
        int randDir = rand.nextInt(dirs.length);
        int randPos = rand.nextInt(gridSize);

        for (int dir = 0; dir < dirs.length; dir++) {
            dir = (dir + randDir) % dirs.length;

            for (int pos = 0; pos < gridSize; pos++) {
                pos = (pos + randPos) % gridSize;

                int lettersPlaced = tryLocation(grid, word, dir, pos);
                if (lettersPlaced > 0)
                    return lettersPlaced;
            }
        }
        return 0;
    }

    static int tryLocation(Grid grid, String word, int dir, int pos) {

        int r = pos / nCols;
        int c = pos % nCols;
        int len = word.length();

        //  check bounds
        if ((dirs[dir][0] == 1 && (len + c) > nCols)
                || (dirs[dir][0] == -1 && (len - 1) > c)
                || (dirs[dir][1] == 1 && (len + r) > nRows)
                || (dirs[dir][1] == -1 && (len - 1) > r))
            return 0;

        int rr, cc, i, overlaps = 0;

        // check cells
        for (i = 0, rr = r, cc = c; i < len; i++) {
            if (grid.cells[rr][cc] != 0 && grid.cells[rr][cc] != word.charAt(i))
                return 0;
            cc += dirs[dir][0];
            rr += dirs[dir][1];
        }

        // place
        for (i = 0, rr = r, cc = c; i < len; i++) {
            if (grid.cells[rr][cc] == word.charAt(i))
                overlaps++;
            else
                grid.cells[rr][cc] = word.charAt(i);

            if (i < len - 1) {
                cc += dirs[dir][0];
                rr += dirs[dir][1];
            }
        }

        int lettersPlaced = len - overlaps;
        if (lettersPlaced > 0) {
            grid.solutions.add(format("%-10s (%d,%d)(%d,%d)", word, c, r, cc, rr));
        }

        return lettersPlaced;
    }

    static void printResult(Grid grid) {
        if (grid == null || grid.numAttempts == 0) {
            System.out.println("No grid to display");
            return;
        }
        int size = grid.solutions.size();

        System.out.println("Attempts: " + grid.numAttempts);
        System.out.println("Number of words: " + size);

        System.out.println("\n     0  1  2  3  4  5  6  7  8  9");
        for (int r = 0; r < nRows; r++) {
            System.out.printf("%n%d   ", r);
            for (int c = 0; c < nCols; c++)
                System.out.printf(" %c ", grid.cells[r][c]);
        }

        System.out.println("\n");

        for (int i = 0; i < size - 1; i += 2) {
            System.out.printf("%s   %s%n", grid.solutions.get(i),
                    grid.solutions.get(i + 1));
        }
        if (size % 2 == 1)
            System.out.println(grid.solutions.get(size - 1));
    }
}
```



```txt
Attempts: 2
Number of words: 27

     0  1  2  3  4  5  6  7  8  9

0    R  p  d  i  o  r  o  t  r  a 
1    O  a  o  e  s  b  l  o  c  S 
2    m  s  t  l  f  e  t  l  a  y 
3    E  t  e  i  y  o  t  s  T  i 
4    e  y  l  b  t  g  r  s  p  l 
5    r  l  T  i  A  h  o  e  e  l 
6    o  e  l  h  t  j  c  n  s  C 
7    z  l  o  u  a  a  O  t  a  t 
8    u  k  r  g  c  n  D  z  i  l 
9    o  t  r  a  v  e  l  E  v  w 

rototill   (8,0)(1,7)   polygonal  (1,0)(9,8)
fill       (4,2)(1,5)   goer       (3,8)(0,5)
travel     (1,9)(6,9)   deforest   (2,0)(9,7)
toroid     (7,0)(2,0)   truth      (1,9)(5,5)
estes      (8,5)(4,1)   ipecac     (9,3)(4,8)
ouzo       (0,9)(0,6)   pasty      (1,0)(1,4)
dote       (2,0)(2,3)   lay        (7,2)(9,2)
witch      (9,9)(5,5)   han        (3,6)(5,8)
bloc       (5,1)(8,1)   ill        (9,3)(9,5)
slot       (7,3)(7,0)   art        (9,0)(7,0)
ore        (0,6)(0,4)   bye        (3,4)(5,2)
elk        (1,6)(1,8)   jan        (5,6)(5,8)
liz        (9,8)(7,8)   dam        (2,0)(0,2)
via        (8,9)(8,7)
```




## Julia

Modified from the Go version. The task listed word list is offline, so the Debian distribution file "words.txt" was used instead.

```julia
using Random

const stepdirections = [[1, 0], [0, 1], [1, 1], [1, -1], [-1, 0], [0, -1], [-1, -1], [-1, 1]]
const nrows    = 10
const ncols    = nrows
const gridsize = nrows * ncols
const minwords = 25
const minwordsize = 3

mutable struct LetterGrid
    nattempts::Int
    nrows::Int
    ncols::Int
    cells::Matrix{Char}
    solutions::Vector{String}
    LetterGrid() = new(0, nrows, ncols, fill(' ', nrows, ncols), Vector{String}())
end

function wordmatrix(filename, usepropernames = true)
    words = [lowercase(line) for line in readlines(filename)
        if match(r"^[a-zA-Z]+$", line) != nothing && (usepropernames ||
            match(r"^[a-z]", line) != nothing) && length(line) >= minwordsize && length(line) <= ncols]
    n = 1000
    for i in 1:n
        grid = LetterGrid()
        messagelen = placemessage(grid, "Rosetta Code")
        target = grid.nrows * grid.ncols - messagelen
        cellsfilled = 0
        shuffle!(words)
        for word in words
            cellsfilled += tryplaceword(grid, word)
            if cellsfilled == target
                if length(grid.solutions) >= minwords
                    grid.nattempts = i
                    return grid
                else
                    break
                end
            end
        end
    end
    throw("Failed to place words after $n attempts")
end

function placemessage(grid, msg)
    msg = uppercase(msg)
    msg = replace(msg, r"[^A-Z]" => "")
    messagelen = length(msg)
    if messagelen > 0 && messagelen < gridsize
        p = Int.(floor.(LinRange(messagelen, gridsize, messagelen) .+
                     (rand(messagelen) .- 0.5) * messagelen / 3)) .- div(messagelen, 3)
        foreach(i -> grid.cells[div(p[i], nrows) + 1, p[i] % nrows + 1] = msg[i], 1:length(p))
        return messagelen
    end
    return 0
end

function tryplaceword(grid, word)
    for dir in shuffle(stepdirections)
        for pos in shuffle(1:length(grid.cells))
            lettersplaced = trylocation(grid, word, dir, pos)
            if lettersplaced > 0
                return lettersplaced
            end
        end
    end
    return 0
end

function trylocation(grid, word, dir, pos)
    r, c = divrem(pos, nrows) .+ [1, 1]
    positions = [[r, c] .+ (dir .* i) for i in 1:length(word)]
    if !all(x -> 0 < x[1] <= nrows && 0 < x[2] <= ncols, positions)
        return 0
    end
    for (i, p) in enumerate(positions)
        letter = grid.cells[p[1],p[2]]
        if letter != ' ' && letter != word[i]
            return 0
        end
    end
    lettersplaced = 0
    for (i, p) in enumerate(positions)
        if grid.cells[p[1], p[2]] == ' '
            lettersplaced += 1
            grid.cells[p[1],p[2]] = word[i]
        end
    end
    if lettersplaced > 0
        push!(grid.solutions, lpad(word, 10) * " $(positions[1]) to $(positions[end])")
    end
    return lettersplaced
end

function printresult(grid)
    if grid.nattempts == 0
        println("No grid to display: no solution found.")
        return
    end
    size = length(grid.solutions)
    println("Attempts: ", grid.nattempts)
    println("Number of words: ", size)
    println("\n     0  1  2  3  4  5  6  7  8  9")
    for r in 1:nrows
        print("\n", rpad(r, 4))
        for c in 1:ncols
            print(" $(grid.cells[r, c]) ")
        end
    end
    println()
    for i in 1:2:size
        println("$(grid.solutions[i])   $(i < size ? grid.solutions[i+1] : "")")
    end
end

printresult(wordmatrix("words.txt", false))

```
{{output}}
```txt

 Attempts: 1
 Number of words: 25
 
      0  1  2  3  4  5  6  7  8  9
 
 1    s  l  i  a  r  t  R  r  r  r
 2    n  o  i  t  c  u  a  O  e  e
 3    h  s  u  t  S  f  a  o  l  d
 4    e  r  y  u  k  s  E  a  w  n
 5    l  d  T  c  w  d  r  y  o  a
 6    l  T  i  h  b  g  w  b  h  l
 7    A  s  a  d  e  p  o  m  w  i
 8    C  t  w  s  r  r  n  o  O  h
 9    s  e  s  a  e  l  p  D  t  p
 10   t  e  i  d  t  E  b  o  d  e
     moped [7, 8] to [7, 4]    philander [9, 10] to [1, 10]
  largesse [3, 9] to [10, 2]         yuks [4, 3] to [4, 6]
   auction [2, 7] to [2, 1]       howler [6, 9] to [1, 9]
     beret [6, 5] to [10, 5]        whats [5, 5] to [9, 1]
    trails [1, 6] to [1, 1]         bode [10, 7] to [10, 10]
      tush [3, 4] to [3, 1]       please [9, 7] to [9, 2]
      loaf [3, 9] to [3, 6]        bored [6, 8] to [10, 4]
      hell [3, 1] to [6, 1]         sick [7, 2] to [4, 5]
       now [8, 7] to [6, 7]          dry [5, 6] to [5, 8]
      swat [7, 2] to [10, 5]         diet [10, 4] to [10, 1]
       too [9, 9] to [7, 7]          owl [8, 8] to [6, 10]
       did [7, 4] to [5, 2]          rut [4, 2] to [2, 4]
       far [3, 6] to [1, 8]

```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

import java.util.Random
import java.io.File

val dirs = listOf(
    intArrayOf( 1, 0), intArrayOf(0,  1), intArrayOf( 1,  1), intArrayOf( 1, -1),
    intArrayOf(-1, 0), intArrayOf(0, -1), intArrayOf(-1, -1), intArrayOf(-1,  1)
)

val nRows = 10
val nCols = 10
val gridSize = nRows * nCols
val minWords = 25
val rand = Random()

class Grid {
    var numAttempts = 0
    val cells = List(nRows) { CharArray(nCols) }
    val solutions = mutableListOf<String>()
}

fun readWords(fileName: String): List<String> {
    val maxLen = maxOf(nRows, nCols)
    val rx = Regex("^[a-z]{3,$maxLen}$")
    val f = File(fileName)
    return f.readLines().map { it.trim().toLowerCase() }
                        .filter { it.matches(rx) }
}

fun createWordSearch(words: List<String>): Grid {
    var numAttempts = 0
    lateinit var grid: Grid
    outer@ while (++numAttempts < 100) {
        grid = Grid()
        val messageLen = placeMessage(grid, "Rosetta Code")
        val target = gridSize - messageLen
        var cellsFilled = 0
        for (word in words.shuffled()) {
            cellsFilled += tryPlaceWord(grid, word)
            if (cellsFilled == target) {
                if (grid.solutions.size >= minWords) {
                    grid.numAttempts = numAttempts
                    break@outer
                }
                else { // grid is full but we didn't pack enough words, start over
                    break
                }
            }
        }
    }
    return grid
}

fun placeMessage(grid: Grid, msg: String): Int {
    val rx = Regex("[^A-Z]")
    val msg2 = msg.toUpperCase().replace(rx, "")
    val messageLen = msg2.length
    if (messageLen in (1 until gridSize)) {
        val gapSize = gridSize / messageLen
        for (i in 0 until messageLen) {
            val pos = i * gapSize + rand.nextInt(gapSize)
            grid.cells[pos / nCols][pos % nCols] = msg2[i]
        }
        return messageLen
    }
    return 0
}

fun tryPlaceWord(grid: Grid, word: String): Int {
    val randDir = rand.nextInt(dirs.size)
    val randPos = rand.nextInt(gridSize)
    for (d in 0 until dirs.size) {
        val dir = (d + randDir) % dirs.size
        for (p in 0 until gridSize) {
            val pos = (p + randPos) % gridSize
            val lettersPlaced = tryLocation(grid, word, dir, pos)
            if (lettersPlaced > 0) return lettersPlaced
        }
    }
    return 0
}

fun tryLocation(grid: Grid, word: String, dir: Int, pos: Int): Int {
    val r = pos / nCols
    val c = pos % nCols
    val len = word.length

    // check bounds
    if ((dirs[dir][0] == 1 && (len + c) > nCols)
        || (dirs[dir][0] == -1 && (len - 1) > c)
        || (dirs[dir][1] ==  1 && (len + r) > nRows)
        || (dirs[dir][1] == -1 && (len - 1) > r)) return 0
    var overlaps = 0

    // check cells
    var rr = r
    var cc = c
    for (i in 0 until len) {
        if (grid.cells[rr][cc] != '\u0000' && grid.cells[rr][cc] != word[i]) return 0
        cc += dirs[dir][0]
        rr += dirs[dir][1]
    }

    // place
    rr = r
    cc = c
    for (i in 0 until len) {
        if (grid.cells[rr][cc] == word[i])
            overlaps++
        else
            grid.cells[rr][cc] = word[i]

        if (i < len - 1) {
            cc += dirs[dir][0]
            rr += dirs[dir][1]
        }
    }

    val lettersPlaced = len - overlaps
    if (lettersPlaced > 0) {
        grid.solutions.add(String.format("%-10s (%d,%d)(%d,%d)", word, c, r, cc, rr))
    }
    return lettersPlaced
}

fun printResult(grid: Grid) {
    if (grid.numAttempts == 0) {
        println("No grid to display")
        return
    }
    val size = grid.solutions.size
    println("Attempts: ${grid.numAttempts}")
    println("Number of words: $size")
    println("\n     0  1  2  3  4  5  6  7  8  9")
    for (r in 0 until nRows) {
         print("\n$r   ")
         for (c in 0 until nCols) print(" ${grid.cells[r][c]} ")
    }

    println("\n")

    for (i in 0 until size - 1 step 2) {
        println("${grid.solutions[i]}   ${grid.solutions[i + 1]}")
    }
    if (size % 2 == 1) println(grid.solutions[size - 1])
}

fun main(args: Array<String>) {
    printResult(createWordSearch(readWords("unixdict.txt")))
}
```


Sample output:

```txt

Attempts: 1
Number of words: 27

     0  1  2  3  4  5  6  7  8  9

0    R  t  a  r  a  c  h  n  e  t 
1    O  c  y  r  t  s  e  c  n  a 
2    t  S  w  b  a  e  m  e  y  c 
3    e  r  e  u  b  E  o  m  o  e 
4    r  T  a  n  a  t  r  o  j  n 
5    r  g  k  T  A  o  r  t  u  t 
6    a  i  g  i  w  e  h  C  l  r 
7    p  k  c  e  d  f  a  O  i  o 
8    i  c  D  o  i  l  g  m  o  i 
9    n  g  r  i  m  e  e  d  E  d 

hemorrhage (6,0)(6,9)   ancestry   (9,1)(2,1)
terrapin   (0,2)(0,9)   julio      (8,4)(8,8)
centroid   (9,2)(9,9)   weak       (2,2)(2,5)
egg        (3,7)(1,5)   toefl      (5,4)(5,8)
grime      (1,9)(5,9)   bun        (3,2)(3,4)
tome       (7,5)(7,2)   arachne    (2,0)(8,0)
deck       (4,7)(1,7)   rico       (0,5)(3,8)
been       (4,3)(7,0)   tara       (1,0)(4,0)
ana        (2,4)(4,4)   oil        (3,8)(5,8)
wormy      (4,6)(8,2)   tab        (4,1)(4,3)
icc        (3,6)(1,8)   coo        (9,2)(7,4)
reub       (1,3)(4,3)   deem       (7,9)(4,9)
rime       (9,6)(6,9)   cat        (9,2)(9,0)
act        (2,0)(0,2)

```



## Phix


```Phix
--
-- demo\rosetta\wordsearch.exw
-- 
### =====================

--
string message = "ROSETTACODE"
sequence words, solution="", placed

constant grid = split("""
X  0  1  2  3  4  5  6  7  8  9  X
0                                X
1                                X
2                                X
3                                X
4                                X
5                                X
6                                X
7                                X
8                                X
9                                X
X  X  X  X  X  X  X  X  X  X  X  X""",'\n')

constant DX = {-1, 0,+1,+1,+1, 0,-1,-1},
         DY = {-3,-3,-3, 0,+3,+3,+3, 0}

procedure wordsearch(sequence grid, integer rqd, integer left, sequence done)
    sequence rw = shuffle(tagset(length(words))),
             rd = shuffle(tagset(8)),
             rs = shuffle(tagset(100))
    for i=1 to length(rs) do
        integer sx = floor((rs[i]-1)/10)+2,
                sy = remainder(rs[i]-1,10)*3+4
        for w=1 to length(rw) do
            string word = words[rw[w]]
            if not find(word,done[1]) then
                for d=1 to length(rd) do
                    integer {dx,dy} = {DX[rd[d]],DY[rd[d]]},
                            {nx,ny} = {sx,sy},
                            chcount = length(word)
                    sequence newgrid = grid
                    for c=1 to length(word) do
                        integer ch = grid[nx][ny]
                        if ch!=' ' then
                            if ch!=word[c] then
                                chcount = -1
                                exit
                            end if
                            chcount -= 1
                        end if
                        newgrid[nx][ny] = word[c]
                        nx += dx
                        ny += dy
                    end for
                    if chcount!=-1 then
                        sequence posinfo = {sx-2,(sy-4)/3,nx-dx-2,(ny-dy-4)/3},
                                 newdone = {append(done[1],word),append(done[2],posinfo)}
                        if rqd<=1 and left-chcount=length(message) then
                            {solution, placed} = {newgrid, newdone}
                            return
                        elsif left-chcount>length(message) then
                            wordsearch(newgrid,rqd-1,left-chcount,newdone)
                            if length(solution) then return end if
                        end if
                    end if
                end for
            end if
        end for
    end for
end procedure

function valid_word(string word)
    if length(word)<3 then return false end if
    for i=1 to length(word) do
        integer ch = word[i]
        if ch<'a'
        or ch>'z' then
            return false
        end if
    end for
    return true
end function

integer fn = open("..\\unixdict.txt","r")
words = get_text(fn,GT_LF_STRIPPED)
close(fn)
for i=length(words) to 1 by -1 do
    if not valid_word(words[i]) then
        words[i] = words[$]
        words = words[1..$-1]
    end if
end for
printf(1,"%d words loaded\n",length(words))

wordsearch(grid,25,100,{{},{}})
for x=2 to 11 do
    for y=4 to 31 by 3 do
        if solution[x][y]=' ' then
            solution[x][y] = message[1]
            message = message[2..$]
        end if
    end for
end for
if length(message) then ?9/0 end if         
puts(1,substitute(join(solution,'\n'),"X"," "))
printf(1,"\n%d words\n",length(placed[1]))
for i=1 to length(placed[1]) do
    printf(1,"%10s %10s  ",{placed[1][i],sprint(placed[2][i])})
    if mod(i,3)=0 then puts(1,"\n") end if
end for
```

{{out}}

```txt

24822 words loaded
   0  1  2  3  4  5  6  7  8  9
0  R  y  g  e  r  m  a  n  y  O
1  d  r  a  g  a  v  e  S  o  E
2  c  a  t  n  w  e  w  T  l  T
3  r  t  s  e  p  h  a  o  k  A
4  a  u  e  v  p  e  d  t  k  C
5  g  l  c  a  o  l  k  O  a  e
6  n  a  t  r  h  l  c  h  o  u
7  a  s  m  p  a  c  a  d  i  a
8  v  n  r  a  d  s  j  o  i  l
9  D  y  i  p  i  E  s  a  s  h

42 words
  salutary  {7,1,0,1}       idaho  {9,4,5,4}     jackdaw  {8,6,2,6}
      darn  {8,4,8,1}      avenge  {5,3,0,3}         van  {8,0,6,0}
       war  {2,4,0,4}        crag  {2,0,5,0}        drag  {1,0,1,3}
       gam  {5,0,7,2}        stag  {3,2,0,2}       crass  {5,2,9,6}
       apr  {8,3,6,3}       staph  {7,1,3,5}     germany  {0,2,0,8}
      laos  {6,5,9,8}        chou  {6,6,6,9}        hell  {3,5,6,5}
       wee  {2,4,4,2}      acadia  {7,4,7,9}        yolk  {0,8,3,8}
       pap  {7,3,9,3}         pry  {7,3,9,1}         usn  {4,1,2,3}
     agave  {1,2,1,6}         nat  {6,0,6,2}         pee  {3,4,1,6}
      sash  {9,6,9,9}         eel  {3,3,5,1}         hid  {9,9,7,7}
       yip  {9,1,9,3}         wok  {2,6,4,8}         raw  {0,4,2,4}
      rave  {6,3,3,3}         oak  {6,8,4,8}         oil  {8,7,8,9}
       lao  {6,5,8,7}        pest  {3,4,3,1}         doe  {7,7,5,9}
       pet  {4,4,2,2}         arc  {4,0,2,0}         tau  {4,7,6,9}

```



## Racket


{{trans|Java}}
(or at least it started out that way... so more "inspired by")


```racket
#lang racket
;; ---------------------------------------------------------------------------------------------------
(module+ main
  (display-puzzle (create-word-search))
  (newline)
  (parameterize ((current-min-words 50))
    (display-puzzle (create-word-search #:n-rows 20 #:n-cols 20))))

;; ---------------------------------------------------------------------------------------------------
(define current-min-words (make-parameter 25))

;; ---------------------------------------------------------------------------------------------------
(define (all-words pzl)
  (filter-map (good-word? pzl) (file->lines "data/unixdict.txt")))

(define (good-word? pzl)
  (let ((m (puzzle-max-word-size pzl)))
    (λ (w) (and (<= 3 (string-length w) m) (regexp-match #px"^[A-Za-z]*$" w) (string-downcase w)))))

(struct puzzle (n-rows n-cols cells solutions) #:transparent)

(define puzzle-max-word-size (match-lambda [(puzzle n-rows n-cols _ _) (max n-rows n-cols)]))

(define dirs '((-1 -1 ↖) (-1 0 ↑) (-1 1 ↗) (0 -1 ←) (0 1 →) (1 -1 ↙) (1 0 ↓) (1 1 ↘)))

;; ---------------------------------------------------------------------------------------------------
(define (display-puzzle pzl) (displayln (puzzle->string pzl)))

(define (puzzle->string pzl)
  (match-let*
      (((and pzl (puzzle n-rows n-cols cells (and solutions (app length size)))) pzl)
       (column-numbers (cons "" (range n-cols)))
       (render-row (λ (r) (cons r (map (λ (c) (hash-ref cells (cons r c) #\_)) (range n-cols)))))
       (the-grid (add-between (map (curry map (curry ~a #:width 3))
                                   (cons column-numbers (map render-row (range n-rows)))) "\n"))
       (solutions§ (solutions->string (sort solutions string<? #:key car))))
    (string-join (flatten (list the-grid "\n\n" solutions§)) "")))

(define (solutions->string solutions)
  (let* ((l1 (compose string-length car))
         (format-solution-to-max-word-size (format-solution (l1 (argmax l1 solutions)))))
    (let recur ((solutions solutions) (need-newline? #f) (acc null))
      (if (null? solutions)
          (reverse (if need-newline? (cons "\n" acc) acc))
          (let* ((spacer (if need-newline? "\n" "   "))
                 (solution (format "~a~a" (format-solution-to-max-word-size (car solutions)) spacer)))
            (recur (cdr solutions) (not need-newline?) (cons solution acc)))))))

(define (format-solution max-word-size)
  (match-lambda [(list word row col dir)
                 (string-append (~a word #:width (+ max-word-size 1))
                                (~a (format "(~a,~a ~a)" row col dir) #:width 9))]))

;; ---------------------------------------------------------------------------------------------------
(define (create-word-search #:msg (msg "Rosetta Code") #:n-rows (n-rows 10) #:n-cols (n-cols 10))
  (let* ((pzl (puzzle n-rows n-cols (hash) null))
         (MSG (sanitise-message msg))
         (n-holes (- (* n-rows n-cols) (string-length MSG))))
    (place-message (place-words pzl (shuffle (all-words pzl)) (current-min-words) n-holes) MSG)))

(define (sanitise-message msg) (regexp-replace* #rx"[^A-Z]" (string-upcase msg) ""))

(define (place-words pzl words needed-words holes)
  (let inner ((pzl pzl) (words words) (needed-words needed-words) (holes holes))
    (cond [(and (not (positive? needed-words)) (zero? holes)) pzl]
          [(null? words)
           (eprintf "no solution... retrying (~a words remaining)~%" needed-words)
           (inner pzl (shuffle words) needed-words)]
          [else
           (let/ec no-fit
             (let*-values
                 (([word words...] (values (car words) (cdr words)))
                  ([solution cells′ holes′]
                   (fit-word word pzl holes (λ () (no-fit (inner pzl words... needed-words holes)))))
                  ([solutions′] (cons solution (puzzle-solutions pzl)))
                  ([pzl′] (struct-copy puzzle pzl (solutions solutions′) (cells cells′))))
               (inner pzl′ words... (sub1 needed-words) holes′)))])))

(define (fit-word word pzl holes fail)
  (match-let* (((puzzle n-rows n-cols cells _) pzl)
               (rows (shuffle (range n-rows)))
               (cols (shuffle (range n-cols)))
               (fits? (let ((l (string-length word))) (λ (maxz z0 dz) (< -1 (+ z0 (* dz l)) maxz)))))
    (let/ec return
      (for* ((dr-dc-↗ (shuffle dirs))
             (r0 rows) (dr (in-value (car dr-dc-↗))) #:when (fits? n-rows r0 dr)
             (c0 cols) (dc (in-value (cadr dr-dc-↗))) #:when (fits? n-cols c0 dc)
             (↗ (in-value (caddr dr-dc-↗))))
        (let/ec retry/ec (attempt-word-fit pzl word r0 c0 dr dc ↗ holes return retry/ec)))
      (fail))))

(define (attempt-word-fit pzl word r0 c0 dr dc ↗ holes return retry)
  (let-values (([cells′ available-cells′]
                (for/fold ((cells′ (puzzle-cells pzl)) (holes′ holes))
                          ((w word) (i (in-naturals)))
                  (define k (cons (+ r0 (* dr i)) (+ c0 (* dc i))))
                  (cond [(not (hash-has-key? cells′ k))
                         (if (zero? holes′) (retry) (values (hash-set cells′ k w) (sub1 holes′)))]
                        [(char=? (hash-ref cells′ k) w) (values cells′ holes′)]
                        [else (retry)]))))
    (return (list word r0 c0 ↗) cells′ available-cells′)))

;; ---------------------------------------------------------------------------------------------------
(define (place-message pzl MSG)
  (match-define (puzzle n-rows n-cols cells _) pzl)
  (struct-copy puzzle pzl
               (cells
                (let loop ((r 0) (c 0) (cells cells) (msg (string->list MSG)))
                  (cond [(or (null? msg) (= r n-rows)) cells]
                        [(= c n-cols) (loop (add1 r) 0 cells msg)]
                        [(hash-has-key? cells (cons r c)) (loop r (add1 c) cells msg)]
                        [else (loop r (add1 c) (hash-set cells (cons r c) (car msg)) (cdr msg))])))))

```


{{out}}


```txt
   0  1  2  3  4  5  6  7  8  9  
0  R  s  o  y  b  e  a  n  O  p  
1  r  d  h  t  a  b  e  S  e  r  
2  o  e  n  a  o  h  k  n  l  u  
3  t  t  y  o  r  a  i  e  i  s  
4  a  e  r  u  r  n  d  g  a  s  
5  r  s  E  m  s  e  n  T  l  e  
6  i  t  a  u  c  i  h  T  f  l  
7  a  A  l  e  l  l  o  y  l  l  
8  n  a  r  s  e  r  a  l  a  C  
9  O  p  D  l  u  m  e  n  c  E  

ail       (4,8 ↑)     air       (7,0 ↑)  
are       (8,6 ←)     aye       (2,3 ↙)  
bath      (1,5 ←)     boor      (1,5 ↙)  
calf      (9,8 ↑)     detest    (1,1 ↓)  
est       (4,1 ↓)     flail     (6,8 ↑)  
heron     (6,6 ↖)     karma     (2,6 ↙)  
lares     (8,7 ←)     loy       (7,5 →)  
lumen     (9,3 →)     nehru     (0,7 ↙)  
peninsula (0,9 ↙)     precede   (9,1 ↗)  
rotarian  (1,0 ↓)     roy       (3,4 ←)  
russell   (1,9 ↓)     sling     (8,3 ↗)  
soybean   (0,1 →)     tab       (1,3 →)  
tar       (3,0 ↓)     


   0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 
0  e  o  w  e  d  i  r  o  u  l  f  a  r  t  e  t  n  i  u  q  
1  x  R  a  i  n  o  n  i  o  k  O  y  t  n  e  l  p  o  o  p  
2  i  a  l  b  a  o  h  u  n  o  i  t  a  t  r  e  s  s  i  d  
3  s  r  s  u  v  u  n  g  o  d  d  e  s  s  o  b  a  S  y  r  
4  t  m  e  d  r  s  h  e  g  n  a  m  i  n  o  l  y  e  r  a  
5  c  o  x  t  b  o  n  E  t  c  a  t  r  r  t  o  c  a  u  n  
6  n  n  y  u  a  y  o  e  i  i  n  e  p  a  y  w  n  v  b  k  
7  i  k  r  r  u  p  r  f  d  a  s  h  d  m  l  b  e  e  n  c  
8  t  y  d  e  e  n  i  b  r  d  i  i  e  n  n  a  g  s  a  a  
9  s  T  a  t  v  t  d  g  t  c  a  n  v  r  a  c  r  d  d  t  
10 i  a  a  m  a  l  n  T  h  t  e  s  f  f  o  k  u  r  A  a  
11 d  b  n  e  e  w  a  n  o  i  t  a  s  r  e  v  n  o  c  l  
12 a  l  b  g  r  h  s  h  n  e  s  t  h  e  r  d  d  p  a  i  
13 r  a  t  t  u  a  c  o  g  e  l  g  g  o  b  e  C  p  l  n  
14 t  r  e  r  n  m  d  s  l  t  n  d  d  u  r  s  a  e  c  a  
15 n  r  l  a  a  a  t  n  h  i  r  o  d  b  O  t  n  d  u  v  
16 o  e  c  u  m  a  t  g  a  a  d  g  l  e  x  i  c  a  l  i  
17 c  f  r  m  n  l  i  t  b  g  e  y  r  u  b  n  e  s  u  d  
18 o  e  i  a  s  l  a  b  y  t  i  l  a  u  q  y  a  w  s  j  
19 h  r  c  a  s  s  i  l  e  m  D  l  k  c  o  b  b  u  l  E  

abate          (12,0 ↗)    alarm          (8,15 ↖) 
alba           (2,1 →)     alma           (18,6 ↖) 
amino          (4,10 →)    andean         (9,14 ↖) 
andiron        (11,6 ↑)    ann            (8,15 ←) 
armonk         (2,1 ↓)     balsa          (18,7 ←) 
beatific       (12,2 ↗)    blowback       (3,15 ↓) 
bock           (19,15 ←)   boggle         (13,14 ←)
bred           (15,13 ↗)   bud            (2,3 ↓)  
budget         (13,14 ↙)   calculus       (11,18 ↓)
catalina       (7,19 ↓)    circlet        (19,2 ↑) 
clot           (5,16 ↖)    contradistinct (17,0 ↑) 
conversation   (11,18 ←)   danbury        (9,18 ↑) 
destiny        (12,15 ↓)   dissertation   (2,19 ←) 
dodo           (16,10 ↗)   drab           (14,11 ↙)
drank          (2,19 ↓)    dusenbury      (17,19 ←)
eavesdropped   (4,17 ↓)    enemy          (10,10 ↗)
esther         (12,9 →)    exist          (0,0 ↓)  
goddess        (3,7 →)     grant          (9,7 ↗)  
halve          (12,7 ↖)    hero           (7,11 ↘) 
hoard          (4,6 ↙)     hoc            (19,0 ↑) 
hurty          (2,6 ↙)     ivan           (16,19 ↑)
juan           (18,19 ↖)   koinonia       (1,9 ←)  
lexical        (16,12 →)   ligand         (19,11 ↖)
lone           (16,12 ↖)   lounsbury      (0,9 ↙)  
lubbock        (19,18 ←)   mange          (4,11 ←) 
manure         (16,4 ↑)    melissa        (19,9 ←) 
natty          (14,4 ↘)    nib            (8,5 →)  
nyu            (5,6 ↙)     offset         (10,14 ←)
orphic         (4,14 ↙)    owe            (0,1 →)  
pay            (6,12 →)    plenty         (1,16 ←) 
poop           (1,19 ←)    purr           (7,5 ←)  
quality        (18,14 ←)   quintet        (0,19 ←) 
rca            (9,16 ←)    read           (12,14 ↘)
referral       (19,1 ↑)    sadden         (10,11 ↖)
salt           (2,17 ↙)    sang           (9,0 ↘)  
schema         (14,7 ↖)    sexy           (3,2 ↓)  
slight         (19,4 ↗)    solid          (12,6 ↘) 
stan           (14,7 ↙)    tern           (5,8 ↙)  
tetrafluoride  (0,15 ←)    thong          (9,8 ↓)  
trauma         (13,3 ↓)    urgency        (10,16 ↑)
visit          (9,12 ↖)    von            (3,4 ↗)  
way            (18,17 ←)   wham           (11,5 ↓) 
```



## zkl

Repeat words allowed. Rather brute force as I didn't realize that the message has to fit exactly.

```zkl
fcn buildVectors(R,C){  //-->up to 8 vectors of wild card strings
   var [const] dirs=T(T(1,0), T(0,1), T(1,1), T(1,-1), T(-1,0),T(0,-1), T(-1,-1), T(-1,1));
   vs,v:=List(),List();
   foreach dr,dc in (dirs){ v.clear(); r,c:=R,C;
      while( (0<=r<10) and (0<=c<10) ){ v.append(grid[r][c]); r+=dr; c+=dc; }
      vs.append(T(v.concat(), // eg "???e??????" would match "cohen" or "mineral"
      	dr,dc));
   }
   vs.filter(fcn(v){ v[0].len()>2 }).shuffle()
}
fcn findFit(vs,words){ //-->(n, word) ie (nth vector,word), empty vs not seen
   do(1000){ foreach n,v in (vs.enumerate()){ do(10){  // lots of ties
      word:=words[(0).random(nwds)];
      if(word.matches(v[0][0,word.len()])) return(word,n); // "??" !match "abc"
   }}}
   False
}
fcn pasteWord(r,c, dr,dc, word)  // jam word into grid along vector
   { foreach char in (word){ grid[r][c]=char; r+=dr; c+=dc; } }
fcn printGrid{
   println("\n   0 1 2 3 4 5 6 7 8 9");
   foreach n,line in (grid.enumerate()){ println(n,"  ",line.concat(" ")) }
}
fcn stuff(msg){ MSG:=msg.toUpper() : Utils.Helpers.cycle(_);
   foreach r,c in (10,10){ if(grid[r][c]=="?") grid[r][c]=MSG.next() }
   MSG._n==msg.len()   // use all of, not more, not less, of msg?
}
```


```zkl
msg:="RosettaCode";

validWord:=RegExp("[A-Za-z]+\n$").matches;
File("unixdict.txt").read(*)  // dictionary file to blob, copied from web
   // blob to list of valid words
   .filter('wrap(w){ (3<w.len()<=10) and validWord(w) })  // "word\n"
   .howza(11).pump(List,"toLower")  // convert blob to list of words, removing \n
   : words:=(_);

reg fitted; do{
   var nwds=words.len(), grid=(10).pump(List(),(10).pump(List(),"?".copy).copy);
   fitted=List(); do(100){
      r,c:=(0).random(10), (0).random(10);
      if(grid[r][c]=="?"){
	 vs,wn:=buildVectors(r,c), findFit(vs,words);
	 if(wn){
	     w,n:=wn; pasteWord(r,c,vs[n][1,*].xplode(),w); 
	     fitted.append(T(r,c,w));
	  }
      }}
   print(".");
}while(fitted.len()<25 or not stuff(msg));

printGrid();
println(fitted.len()," words fitted");
fitted.pump(Console.println, T(Void.Read,3,False), 
   fcn{ vm.arglist.pump(String,
      fcn([(r,c,w)]){ "%-19s".fmt("[%d,%d]: %s  ".fmt(r,c,w)) }) }
);
fitted.apply(fcn(w){ w[2].len() }).sum(0).println();
```

{{out}}

```txt

..................................
   0 1 2 3 4 5 6 7 8 9
0  s t b n i b s d R O
1  k y s u p i d a g w
2  i S a a r n E f a a
3  s T d w o n k l b m
4  u T s e t b c o h u
5  m e d e y A e p c p
6  y r e l x e b g a C
7  h o a g d i l l o n
8  t c f p O g u n r D
9  k b o l s h o i b E
26 words fitted
[6,5]: eyed        [7,4]: dillon      [9,1]: bolshoi     [6,1]: rap         
[9,8]: broach      [4,6]: claw        [0,2]: burn        [3,3]: way         
[8,5]: gun         [2,7]: fad         [6,7]: gpo         [6,6]: beck        
[8,0]: thymus      [4,5]: boast       [1,6]: dip         [2,5]: nib         
[3,8]: bag         [4,2]: sex         [8,1]: core        [0,3]: nibs        
[7,3]: gee         [5,2]: deaf        [4,4]: twa         [5,9]: puma        
[0,0]: ski         [6,3]: lack        
102

```



## Python

{{trans|Java}}
{{works with|Python|3.x}}

```python

import re
from random import shuffle, randint

dirs = [[1, 0], [0, 1], [1, 1], [1, -1], [-1, 0], [0, -1], [-1, -1], [-1, 1]]
n_rows = 10
n_cols = 10
grid_size = n_rows * n_cols
min_words = 25


class Grid:
    def __init__(self):
        self.num_attempts = 0
        self.cells = [['' for _ in range(n_cols)] for _ in range(n_rows)]
        self.solutions = []


def read_words(filename):
    max_len = max(n_rows, n_cols)

    words = []
    with open(filename, "r") as file:
        for line in file:
            s = line.strip().lower()
            if re.match(r'^[a-z]{3,' + re.escape(str(max_len)) + r'}$', s) is not None:
                words.append(s)

    return words


def place_message(grid, msg):
    msg = re.sub(r'[^A-Z]', "", msg.upper())

    message_len = len(msg)
    if 0 < message_len < grid_size:
        gap_size = grid_size // message_len

        for i in range(0, message_len):
            pos = i * gap_size + randint(0, gap_size)
            grid.cells[pos // n_cols][pos % n_cols] = msg[i]

        return message_len

    return 0


def try_location(grid, word, direction, pos):
    r = pos // n_cols
    c = pos % n_cols
    length = len(word)

    # check bounds
    if (dirs[direction][0] == 1 and (length + c) > n_cols) or \
       (dirs[direction][0] == -1 and (length - 1) > c) or \
       (dirs[direction][1] == 1 and (length + r) > n_rows) or \
       (dirs[direction][1] == -1 and (length - 1) > r):
        return 0

    rr = r
    cc = c
    i = 0
    overlaps = 0

    # check cells
    while i < length:
        if grid.cells[rr][cc] != '' and grid.cells[rr][cc] != word[i]:
            return 0
        cc += dirs[direction][0]
        rr += dirs[direction][1]
        i += 1

    rr = r
    cc = c
    i = 0
    # place
    while i < length:
        if grid.cells[rr][cc] == word[i]:
            overlaps += 1
        else:
            grid.cells[rr][cc] = word[i]

        if i < length - 1:
            cc += dirs[direction][0]
            rr += dirs[direction][1]

        i += 1

    letters_placed = length - overlaps
    if letters_placed > 0:
        grid.solutions.append("{0:<10} ({1},{2})({3},{4})".format(word, c, r, cc, rr))

    return letters_placed


def try_place_word(grid, word):
    rand_dir = randint(0, len(dirs))
    rand_pos = randint(0, grid_size)

    for direction in range(0, len(dirs)):
        direction = (direction + rand_dir) % len(dirs)

        for pos in range(0, grid_size):
            pos = (pos + rand_pos) % grid_size

            letters_placed = try_location(grid, word, direction, pos)
            if letters_placed > 0:
                return letters_placed

    return 0


def create_word_search(words):
    grid = None
    num_attempts = 0

    while num_attempts < 100:
        num_attempts += 1
        shuffle(words)

        grid = Grid()
        message_len = place_message(grid, "Rosetta Code")
        target = grid_size - message_len

        cells_filled = 0
        for word in words:
            cells_filled += try_place_word(grid, word)
            if cells_filled == target:
                if len(grid.solutions) >= min_words:
                    grid.num_attempts = num_attempts
                    return grid
                else:
                    break # grid is full but we didn't pack enough words, start over

    return grid


def print_result(grid):
    if grid is None or grid.num_attempts == 0:
        print("No grid to display")
        return

    size = len(grid.solutions)

    print("Attempts: {0}".format(grid.num_attempts))
    print("Number of words: {0}".format(size))

    print("\n     0  1  2  3  4  5  6  7  8  9\n")
    for r in range(0, n_rows):
        print("{0}   ".format(r), end='')
        for c in range(0, n_cols):
            print(" %c " % grid.cells[r][c], end='')
        print()
    print()

    for i in range(0, size - 1, 2):
        print("{0}   {1}".format(grid.solutions[i], grid.solutions[i+1]))

    if size % 2 == 1:
        print(grid.solutions[size - 1])


if __name__ == "__main__":
    print_result(create_word_search(read_words("unixdict.txt")))

```


{{out}}

```txt

Attempts: 1
Number of words: 25

     0  1  2  3  4  5  6  7  8  9

0    f  b  R  u  e  r  u  l  t  h 
1    o  n  o  t  v  O  e  r  o  p 
2    a  S  a  b  a  x  o  b  E  m 
3    l  e  d  s  h  w  T  p  e  u 
4    w  p  v  a  n  s  u  c  k  i 
5    o  T  u  r  A  t  u  t  s  r 
6    n  s  o  p  u  m  y  d  i  t 
7    t  h  C  j  a  c  o  b  i  O 
8    t  i  r  e  h  n  i  m  D  p 
9    y  n  o  l  o  c  E  s  a  c 

exhaust    (6,1)(0,7)   hornwort   (1,7)(8,0)
btu        (3,2)(3,0)   jacobi     (3,7)(8,7)
foal       (0,0)(0,3)   triumph    (9,6)(9,0)
inherit    (6,8)(0,8)   mecum      (9,2)(5,6)
colony     (5,9)(0,9)   curve      (5,7)(1,3)
wont       (0,4)(0,7)   lure       (7,0)(4,0)
hob        (9,0)(7,2)   tidy       (9,6)(6,6)
suck       (5,4)(8,4)   san        (3,3)(1,1)
sac        (7,9)(9,9)   put        (7,3)(5,5)
led        (0,3)(2,3)   stu        (8,5)(6,5)
have       (4,3)(4,0)   min        (7,8)(5,8)
bob        (1,0)(3,2)   pup        (3,6)(1,4)
dip        (7,6)(9,8)

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    ReadOnly Dirs As Integer(,) = {
        {1, 0}, {0, 1}, {1, 1},
        {1, -1}, {-1, 0},
        {0, -1}, {-1, -1}, {-1, 1}
    }

    Const RowCount = 10
    Const ColCount = 10
    Const GridSize = RowCount * ColCount
    Const MinWords = 25

    Class Grid
        Public cells(RowCount - 1, ColCount - 1) As Char
        Public solutions As New List(Of String)
        Public numAttempts As Integer

        Sub New()
            For i = 0 To RowCount - 1
                For j = 0 To ColCount - 1
                    cells(i, j) = ControlChars.NullChar
                Next
            Next
        End Sub
    End Class

    Dim Rand As New Random()

    Sub Main()
        PrintResult(CreateWordSearch(ReadWords("unixdict.txt")))
    End Sub

    Function ReadWords(filename As String) As List(Of String)
        Dim maxlen = Math.Max(RowCount, ColCount)
        Dim words As New List(Of String)

        Dim objReader As New IO.StreamReader(filename)
        Dim line As String
        Do While objReader.Peek() <> -1
            line = objReader.ReadLine()
            If line.Length > 3 And line.Length < maxlen Then
                If line.All(Function(c) Char.IsLetter(c)) Then
                    words.Add(line)
                End If
            End If
        Loop

        Return words
    End Function

    Function CreateWordSearch(words As List(Of String)) As Grid
        For numAttempts = 1 To 1000
            Shuffle(words)

            Dim grid As New Grid()
            Dim messageLen = PlaceMessage(grid, "Rosetta Code")
            Dim target = GridSize - messageLen

            Dim cellsFilled = 0
            For Each word In words
                cellsFilled = cellsFilled + TryPlaceWord(grid, word)
                If cellsFilled = target Then
                    If grid.solutions.Count >= MinWords Then
                        grid.numAttempts = numAttempts
                        Return grid
                    Else
                        'grid is full but we didn't pack enough words, start over
                        Exit For
                    End If
                End If
            Next
        Next

        Return Nothing
    End Function

    Function PlaceMessage(grid As Grid, msg As String) As Integer
        msg = msg.ToUpper()
        msg = msg.Replace(" ", "")

        If msg.Length > 0 And msg.Length < GridSize Then
            Dim gapSize As Integer = GridSize / msg.Length

            Dim pos = 0
            Dim lastPos = -1
            For i = 0 To msg.Length - 1
                If i = 0 Then
                    pos = pos + Rand.Next(gapSize - 1)
                Else
                    pos = pos + Rand.Next(2, gapSize - 1)
                End If
                Dim r As Integer = Math.Floor(pos / ColCount)
                Dim c = pos Mod ColCount

                grid.cells(r, c) = msg(i)

                lastPos = pos
            Next
            Return msg.Length
        End If

        Return 0
    End Function

    Function TryPlaceWord(grid As Grid, word As String) As Integer
        Dim randDir = Rand.Next(Dirs.GetLength(0))
        Dim randPos = Rand.Next(GridSize)

        For d = 0 To Dirs.GetLength(0) - 1
            Dim dd = (d + randDir) Mod Dirs.GetLength(0)

            For p = 0 To GridSize - 1
                Dim pp = (p + randPos) Mod GridSize

                Dim lettersPLaced = TryLocation(grid, word, dd, pp)
                If lettersPLaced > 0 Then
                    Return lettersPLaced
                End If
            Next
        Next

        Return 0
    End Function

    Function TryLocation(grid As Grid, word As String, dir As Integer, pos As Integer) As Integer
        Dim r As Integer = pos / ColCount
        Dim c = pos Mod ColCount
        Dim len = word.Length

        'check bounds
        If (Dirs(dir, 0) = 1 And len + c >= ColCount) Or (Dirs(dir, 0) = -1 And len - 1 > c) Or (Dirs(dir, 1) = 1 And len + r >= RowCount) Or (Dirs(dir, 1) = -1 And len - 1 > r) Then
            Return 0
        End If
        If r = RowCount OrElse c = ColCount Then
            Return 0
        End If

        Dim rr = r
        Dim cc = c

        'check cells
        For i = 0 To len - 1
            If grid.cells(rr, cc) <> ControlChars.NullChar AndAlso grid.cells(rr, cc) <> word(i) Then
                Return 0
            End If

            cc = cc + Dirs(dir, 0)
            rr = rr + Dirs(dir, 1)
        Next

        'place
        Dim overlaps = 0
        rr = r
        cc = c
        For i = 0 To len - 1
            If grid.cells(rr, cc) = word(i) Then
                overlaps = overlaps + 1
            Else
                grid.cells(rr, cc) = word(i)
            End If

            If i < len - 1 Then
                cc = cc + Dirs(dir, 0)
                rr = rr + Dirs(dir, 1)
            End If
        Next

        Dim lettersPlaced = len - overlaps
        If lettersPlaced > 0 Then
            grid.solutions.Add(String.Format("{0,-10} ({1},{2})({3},{4})", word, c, r, cc, rr))
        End If

        Return lettersPlaced
    End Function

    Sub PrintResult(grid As Grid)
        If IsNothing(grid) OrElse grid.numAttempts = 0 Then
            Console.WriteLine("No grid to display")
            Return
        End If

        Console.WriteLine("Attempts: {0}", grid.numAttempts)
        Console.WriteLine("Number of words: {0}", GridSize)
        Console.WriteLine()

        Console.WriteLine("     0  1  2  3  4  5  6  7  8  9")
        For r = 0 To RowCount - 1
            Console.WriteLine()
            Console.Write("{0}   ", r)
            For c = 0 To ColCount - 1
                Console.Write(" {0} ", grid.cells(r, c))
            Next
        Next

        Console.WriteLine()
        Console.WriteLine()

        For i = 0 To grid.solutions.Count - 1
            If i Mod 2 = 0 Then
                Console.Write("{0}", grid.solutions(i))
            Else
                Console.WriteLine("   {0}", grid.solutions(i))
            End If
        Next

        Console.WriteLine()
    End Sub

    'taken from https://stackoverflow.com/a/20449161
    Sub Shuffle(Of T)(list As IList(Of T))
        Dim r As Random = New Random()
        For i = 0 To list.Count - 1
            Dim index As Integer = r.Next(i, list.Count)
            If i <> index Then
                ' swap list(i) and list(index)
                Dim temp As T = list(i)
                list(i) = list(index)
                list(index) = temp
            End If
        Next
    End Sub

End Module
```

{{out}}

```txt
Attempts: 148
Number of words: 100

     0  1  2  3  4  5  6  7  8  9

0    c  d  p  R  e  c  h  a  r  e
1    O  i  u  b  a  k  e  S  l  v
2    k  n  l  E  m  c  a  c  a  i
3    T  e  s  i  T  x  A  s  n  t
4    t  C  e  s  a  l  O  a  g  a
5    a  j  D  l  l  e  E  h  l  g
6    l  u  f  e  m  a  h  s  e  r
7    l  t  c  a  r  f  e  r  y  u
8    f  e  r  r  e  i  r  a  m  p
9    f  a  m  i  l  i  s  m  i  s

refract    (7,7)(1,7)   shameful   (7,6)(0,6)
ferreira   (0,8)(7,8)   familism   (0,9)(7,9)
langley    (8,1)(8,7)   sake       (7,3)(4,0)
pulse      (2,0)(2,4)   purgative  (9,8)(9,0)
cacm       (7,2)(4,2)   enid       (1,3)(1,0)
char       (5,0)(8,0)   flax       (2,6)(5,3)
tall       (0,4)(0,7)   isle       (3,3)(3,6)
jute       (1,5)(1,8)   myel       (8,8)(8,5)
bake       (3,1)(6,1)   cell       (2,7)(5,4)
marsh      (7,9)(7,5)   keel       (0,2)(3,5)
spur       (9,9)(9,6)   leaf       (5,4)(5,7)
cilia      (0,0)(4,4)   sims       (9,9)(6,9)
marsha     (7,9)(7,4)
```


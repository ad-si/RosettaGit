+++
title = "Magic squares of singly even order"
description = ""
date = 2019-10-06T10:13:31Z
aliases = []
[extra]
id = 20583
[taxonomies]
categories = ["task"]
languages = [
  "befunge",
  "c",
  "c_plus_plus",
  "d",
  "elixir",
  "free_basic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "lua",
  "perl",
  "perl_6",
  "phix",
  "python",
  "ruby",
  "rust",
  "zkl"
]
tags = []
+++

## Description

A [[wp:Magic_square|magic square]] is an '''NxN''' square matrix whose numbers consist of consecutive numbers arranged so that the sum of each row and column, ''and'' both diagonals are equal to the same sum (which is called the ''magic number'' or ''magic constant'').

A  magic square of singly even order has a size that is a multiple of 4, plus 2 (e.g. 6, 10, 14). This means that the subsquares have an odd size, which plays a role in the construction.



## Task
Create a magic square of 6 x 6.




## Related tasks
* [[Magic squares of odd order]]
* [[Magic squares of doubly even order]]



## See also
* [http://www.1728.org/magicsq3.htm Singly Even Magic Squares (1728.org)]







## Befunge

The size, ''N'', is specified by the first value on the stack. In the example below it is set to 6, but adequate space has been left in the code to replace that with a larger value if desired.


```befunge
6>>>>>
:00p:2/vv1:%g01p04:%g00::p03*2%g01/g00::-1_@
\00g/10g/3*4vv>0g\-1-30g+1+10g%10g*\30g+1+10g%1+ +
:%4+*2/g01g0<vv4*`\g02\!`\0:-!-g02/2g03g04-3*2\-\3
*:p02/4-2:p01<>0g00g20g-`+!!*+10g:**+.:00g%!9+,:^:
```


{{out}}

```txt
26      19      24      8       1       33
21      23      25      3       32      7
22      27      20      4       9       29
17      10      15      35      28      6
12      14      16      30      5       34
13      18      11      31      36      2
```



## C

Takes number of rows from command line, prints out usage on incorrect invocation.

```C

   #include<stdlib.h>
   #include<ctype.h>
   #include<stdio.h>

   int** oddMagicSquare(int n) {
        if (n < 3 || n % 2 == 0)
            return NULL;

        int value = 0;
        int squareSize = n * n;
        int c = n / 2, r = 0,i;

        int** result = (int**)malloc(n*sizeof(int*));

		for(i=0;i<n;i++)
			result[i] = (int*)malloc(n*sizeof(int));

        while (++value <= squareSize) {
            result[r][c] = value;
            if (r == 0) {
                if (c == n - 1) {
                    r++;
                } else {
                    r = n - 1;
                    c++;
                }
            } else if (c == n - 1) {
                r--;
                c = 0;
            } else if (result[r - 1][c + 1] == 0) {
                r--;
                c++;
            } else {
                r++;
            }
        }
        return result;
    }

    int** singlyEvenMagicSquare(int n) {
        if (n < 6 || (n - 2) % 4 != 0)
            return NULL;

        int size = n * n;
        int halfN = n / 2;
        int subGridSize = size / 4, i;

        int** subGrid = oddMagicSquare(halfN);
        int gridFactors[] = {0, 2, 3, 1};
        int** result = (int**)malloc(n*sizeof(int*));

		for(i=0;i<n;i++)
			result[i] = (int*)malloc(n*sizeof(int));

        for (int r = 0; r < n; r++) {
            for (int c = 0; c < n; c++) {
                int grid = (r / halfN) * 2 + (c / halfN);
                result[r][c] = subGrid[r % halfN][c % halfN];
                result[r][c] += gridFactors[grid] * subGridSize;
            }
        }

        int nColsLeft = halfN / 2;
        int nColsRight = nColsLeft - 1;

        for (int r = 0; r < halfN; r++)
            for (int c = 0; c < n; c++) {
                if (c < nColsLeft || c >= n - nColsRight
                        || (c == nColsLeft && r == nColsLeft)) {

                    if (c == 0 && r == nColsLeft)
                        continue;

                    int tmp = result[r][c];
                    result[r][c] = result[r + halfN][c];
                    result[r + halfN][c] = tmp;
                }
            }

        return result;
    }

	int numDigits(int n){
		int count = 1;

		while(n>=10){
			n /= 10;
			count++;
		}

		return count;
	}

	void printMagicSquare(int** square,int rows){
		int i,j;

		for(i=0;i<rows;i++){
			for(j=0;j<rows;j++){
				printf("%*s%d",rows - numDigits(square[i][j]),"",square[i][j]);
			}
			printf("\n");
		}
		printf("\nMagic constant: %d ", (rows * rows + 1) * rows / 2);
	}

	int main(int argC,char* argV[])
	{
		int n;

		if(argC!=2||isdigit(argV[1][0])==0)
			printf("Usage : %s <integer specifying rows in magic square>",argV[0]);
		else{
			n = atoi(argV[1]);
			printMagicSquare(singlyEvenMagicSquare(n),n);
		}
		return 0;
	}

```

Invocation and Output:

```txt

C:\rosettaCode>singlyEvenMagicSquare 6
    35     1     6    26    19    24
     3    32     7    21    23    25
    31     9     2    22    27    20
     8    28    33    17    10    15
    30     5    34    12    14    16
     4    36    29    13    18    11

Magic constant: 111

```



## C++


```cpp

#include <iostream>
#include <sstream>
#include <iomanip>
using namespace std;

class magicSqr
{
public:
    magicSqr() { sqr = 0; }
    ~magicSqr() { if( sqr ) delete [] sqr; }

    void create( int d ) {
        if( sqr ) delete [] sqr;
        if( d & 1 ) d++;
        while( d % 4 == 0 ) { d += 2; }
        sz = d;
        sqr = new int[sz * sz];
        memset( sqr, 0, sz * sz * sizeof( int ) );
        fillSqr();
    }
    void display() {
        cout << "Singly Even Magic Square: " << sz << " x " << sz << "\n";
        cout << "It's Magic Sum is: " << magicNumber() << "\n\n";
        ostringstream cvr; cvr << sz * sz;
        int l = cvr.str().size();

        for( int y = 0; y < sz; y++ ) {
            int yy = y * sz;
            for( int x = 0; x < sz; x++ ) {
                cout << setw( l + 2 ) << sqr[yy + x];
            }
            cout << "\n";
        }
        cout << "\n\n";
    }
private:
    void siamese( int from, int to ) {
        int oneSide = to - from, curCol = oneSide / 2, curRow = 0, count = oneSide * oneSide, s = 1;

        while( count > 0 ) {
            bool done = false;
            while ( false == done ) {
                if( curCol >= oneSide ) curCol = 0;
                if( curRow < 0 ) curRow = oneSide - 1;
                done = true;
                if( sqr[curCol + sz * curRow] != 0 ) {
                    curCol -= 1; curRow += 2;
                    if( curCol < 0 ) curCol = oneSide - 1;
                    if( curRow >= oneSide ) curRow -= oneSide;

                    done = false;
                }
            }
            sqr[curCol + sz * curRow] = s;
            s++; count--; curCol++; curRow--;
        }
    }
    void fillSqr() {
        int n = sz / 2, ns = n * sz, size = sz * sz, add1 = size / 2, add3 = size / 4, add2 = 3 * add3;

        siamese( 0, n );

        for( int r = 0; r < n; r++ ) {
            int row = r * sz;
            for( int c = n; c < sz; c++ ) {
                int m = sqr[c - n + row];

                sqr[c + row] = m + add1;
                sqr[c + row + ns] = m + add3;
                sqr[c - n + row + ns] = m + add2;
            }
        }

        int lc = ( sz - 2 ) / 4, co = sz - ( lc - 1 );
        for( int r = 0; r < n; r++ ) {
            int row = r * sz;
            for( int c = co; c < sz; c++ ) {
                sqr[c + row] -= add3;
                sqr[c + row + ns] += add3;
            }
        }
        for( int r = 0; r < n; r++ ) {
            int row = r * sz;
            for( int c = 0; c < lc; c++ ) {
                int cc = c;
                if( r == lc ) cc++;
                sqr[cc + row] += add2;
                sqr[cc + row + ns] -= add2;
            }
        }
    }
    int magicNumber() { return sz * ( ( sz * sz ) + 1 ) / 2; }

    void inc( int& a ) { if( ++a == sz ) a = 0; }

    void dec( int& a ) { if( --a < 0 ) a = sz - 1; }

    bool checkPos( int x, int y ) { return( isInside( x ) && isInside( y ) && !sqr[sz * y + x] ); }

    bool isInside( int s ) { return ( s < sz && s > -1 ); }

    int* sqr;
    int sz;
};
int main( int argc, char* argv[] ) {
    magicSqr s; s.create( 6 );
    s.display();
    return 0;
}

```

{{out}}

```txt

Singly Even Magic Square: 6 x 6
It's Magic Sum is: 111

  35   1   6  26  19  24
   3  32   7  21  23  25
  31   9   2  22  27  20
   8  28  33  17  10  15
  30   5  34  12  14  16
   4  36  29  13  18  11

```



## D

{{trans|Java}}

```d

import std.exception;
import std.stdio;

void main() {
    int n = 6;
    foreach (row; magicSquareSinglyEven(n)) {
        foreach (x; row) {
            writef("%2s ", x);
        }
        writeln();
    }
    writeln("\nMagic constant: ", (n * n + 1) * n / 2);
}

int[][] magicSquareOdd(const int n) {
    enforce(n >= 3 && n % 2 != 0, "Base must be odd and >2");

    int value = 0;
    int gridSize = n * n;
    int c = n / 2;
    int r = 0;

    int[][] result = new int[][](n, n);

    while(++value <= gridSize) {
        result[r][c] = value;
        if (r == 0) {
            if (c == n - 1) {
                r++;
            } else {
                r = n - 1;
                c++;
            }
        } else if (c == n - 1) {
            r--;
            c = 0;
        } else if (result[r - 1][c + 1] == 0) {
            r--;
            c++;
        } else {
            r++;
        }
    }

    return result;
}

int[][] magicSquareSinglyEven(const int n) {
    enforce(n >= 6 && (n - 2) % 4 == 0, "Base must be a positive multiple of four plus 2");

    int size = n * n;
    int halfN = n / 2;
    int subSquareSize = size / 4;

    int[][] subSquare = magicSquareOdd(halfN);
    int[] quadrantFactors = [0, 2, 3, 1];
    int[][] result = new int[][](n, n);

    for (int r = 0; r < n; r++) {
        for (int c = 0; c < n; c++) {
            int quadrant = (r / halfN) * 2 + (c / halfN);
            result[r][c] = subSquare[r % halfN][c % halfN];
            result[r][c] += quadrantFactors[quadrant] * subSquareSize;
        }
    }

    int nColsLeft = halfN / 2;
    int nColsRight = nColsLeft - 1;

    for (int r = 0; r < halfN; r++) {
        for (int c = 0; c < n; c++) {
            if (c < nColsLeft || c >= n - nColsRight
                || (c == nColsLeft && r == nColsLeft)) {
                if (c == 0 && r == nColsLeft) {
                    continue;
                }

                int tmp = result[r][c];
                result[r][c] = result[r + halfN][c];
                result[r + halfN][c] = tmp;
            }
        }
    }

    return result;
}

```



## Elixir

[[wp:Conway's LUX method for magic squares]]:

```elixir
defmodule Magic_square do
  @lux  %{ L: [4, 1, 2, 3], U: [1, 4, 2, 3], X: [1, 4, 3, 2] }

  def singly_even(n) when rem(n-2,4)!=0, do: raise ArgumentError, "must be even, but not divisible by 4."
  def singly_even(2), do: raise ArgumentError, "2x2 magic square not possible."
  def singly_even(n) do
    n2 = div(n, 2)
    oms = odd_magic_square(n2)
    mat = make_lux_matrix(n2)
    square = synthesis(n2, oms, mat)
    IO.puts to_string(n, square)
    square
  end

  defp odd_magic_square(m) do       # zero beginning, it is 4 multiples.
    for i <- 0..m-1, j <- 0..m-1, into: %{},
        do: {{i,j}, (m*(rem(i+j+1+div(m,2),m)) + rem(i+2*j-5+2*m, m)) * 4}
  end

  defp make_lux_matrix(m) do
    center = div(m, 2)
    lux = List.duplicate(:L, center+1) ++ [:U] ++ List.duplicate(:X, m-center-2)
    (for {x,i} <- Enum.with_index(lux), j <- 0..m-1, into: %{}, do: {{i,j}, x})
    |> Map.put({center,   center}, :U)
    |> Map.put({center+1, center}, :L)
  end

  defp synthesis(m, oms, mat) do
    range = 0..m-1
    Enum.reduce(range, [], fn i,acc ->
      {row0, row1} = Enum.reduce(range, {[],[]}, fn j,{r0,r1} ->
                       x = oms[{i,j}]
                       [lux0, lux1, lux2, lux3] = @lux[mat[{i,j}]]
                       {[x+lux0, x+lux1 | r0], [x+lux2, x+lux3 | r1]}
                     end)
      [row0, row1 | acc]
    end)
  end

  defp to_string(n, square) do
    format = String.duplicate("~#{length(to_char_list(n*n))}w ", n) <> "\n"
    Enum.map_join(square, fn row ->
      :io_lib.format(format, row)
    end)
  end
end

Magic_square.singly_even(6)
```


{{out}}

```txt

 5  8 36 33 13 16
 6  7 34 35 14 15
28 25 17 20 12  9
26 27 18 19 10 11
24 21  4  1 32 29
22 23  2  3 30 31

```



## FreeBASIC


```freebasic
' version 18-03-2016
' compile with: fbc -s console
' singly even magic square 6, 10, 14, 18...

Sub Err_msg(msg As String)
    Print msg
    Beep : Sleep 5000, 1 : Exit Sub
End Sub

Sub se_magicsq(n As UInteger, filename As String = "")

    ' filename <> "" then save square in a file
    ' filename can contain directory name
    ' if filename exist it will be overwriten, no error checking

    If n < 6 Then
        Err_msg( "Error: n is to small")
        Exit Sub
    End If

    If ((n -2) Mod 4) <> 0 Then
        Err_msg "Error: not possible to make singly" + _
                 " even magic square size " + Str(n)
        Exit Sub
    End If

    Dim As UInteger sq(1 To n, 1 To n)
    Dim As UInteger magic_sum = n * (n ^ 2 +1) \ 2
    Dim As UInteger sq_d_2 = n \ 2, q2 = sq_d_2 ^ 2
    Dim As UInteger l = (n -2) \ 4
    Dim As UInteger x = sq_d_2 \ 2 + 1, y = 1, nr = 1
    Dim As String frmt = String(Len(Str(n * n)) +1, "#")

    ' fill pattern A C
    '              D B
    ' main loop for creating magic square in section A
    ' the value for B,C and D is derived from A
    ' uses the FreeBASIC odd order magic square routine
    Do
        If sq(x, y) = 0 Then
            sq(x         , y         ) = nr          ' A
            sq(x + sq_d_2, y + sq_d_2) = nr + q2     ' B
            sq(x + sq_d_2, y         ) = nr + q2 * 2 ' C
            sq(x         , y + sq_d_2) = nr + q2 * 3 ' D
            If nr Mod sq_d_2 = 0 Then
                y += 1
            Else
                x += 1 : y -= 1
            End If
            nr += 1
        End If
        If x > sq_d_2 Then
            x = 1
            Do While sq(x,y) <> 0
                x += 1
            Loop
        End If
        If y < 1 Then
            y = sq_d_2
            Do While sq(x,y) <> 0
                y -= 1
            Loop
        End If
    Loop Until nr > q2


    ' swap left side
    For y = 1 To sq_d_2
        For x = 1 To l
            Swap sq(x, y), sq(x,y + sq_d_2)
        Next
    Next
    ' make indent
    y = (sq_d_2 \ 2) +1
    Swap sq(1, y), sq(1, y + sq_d_2) ' was swapped, restore to orignal value
    Swap sq(l +1, y), sq(l +1, y + sq_d_2)

    ' swap right side
    For y = 1 To sq_d_2
        For x = n - l +2 To n
            Swap sq(x, y), sq(x,y + sq_d_2)
        Next
    Next

    ' check columms and rows
    For y = 1 To n
        nr = 0 : l  = 0
        For x = 1 To n
            nr += sq(x,y)
            l  += sq(y,x)
        Next
        If nr <> magic_sum Or l <> magic_sum Then
            Err_msg "Error: value <> magic_sum"
            Exit Sub
        End If
    Next

    ' check diagonals
    nr = 0 : l = 0
    For x = 1 To n
        nr += sq(x, x)
        l  += sq(n - x +1, n - x +1)
    Next
    If nr <> magic_sum Or l <> magic_sum Then
        Err_msg "Error: value <> magic_sum"
        Exit Sub
    End If

    ' printing square's on screen bigger when
    ' n > 19 results in a wrapping of the line
    Print "Single even magic square size: "; n; "*"; n
    Print "The magic sum = "; magic_sum
    Print
    For y = 1 To n
        For x = 1 To n
            Print Using frmt; sq(x, y);
        Next
        Print
    Next

    ' output magic square to a file with the name provided
    If filename <> "" Then
        nr = FreeFile
        Open filename For Output As #nr
        Print #nr, "Single even magic square size: "; n; "*"; n
        Print #nr, "The magic sum = "; magic_sum
        Print #nr,
        For y = 1 To n
            For x = 1 To n
                Print #nr, Using frmt; sq(x,y);
            Next
            Print #nr,
        Next
        Close #nr
    End If

End Sub

' ------=< MAIN >=------

se_magicsq(6, "magicse6.txt") : Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Single even magic square size: 6*6
The magic sum = 111

 35  1  6 26 19 24
  3 32  7 21 23 25
 31  9  2 22 27 20
  8 28 33 17 10 15
 30  5 34 12 14 16
  4 36 29 13 18 11
```



## Go

{{trans|Java}}

```go
package main

import (
    "fmt"
    "log"
)

func magicSquareOdd(n int) ([][]int, error) {
    if n < 3 || n%2 == 0 {
        return nil, fmt.Errorf("base must be odd and > 2")
    }
    value := 1
    gridSize := n * n
    c, r := n/2, 0
    result := make([][]int, n)

    for i := 0; i < n; i++ {
        result[i] = make([]int, n)
    }

    for value <= gridSize {
        result[r][c] = value
        if r == 0 {
            if c == n-1 {
                r++
            } else {
                r = n - 1
                c++
            }
        } else if c == n-1 {
            r--
            c = 0
        } else if result[r-1][c+1] == 0 {
            r--
            c++
        } else {
            r++
        }
        value++
    }
    return result, nil
}

func magicSquareSinglyEven(n int) ([][]int, error) {
    if n < 6 || (n-2)%4 != 0 {
        return nil, fmt.Errorf("base must be a positive multiple of 4 plus 2")
    }
    size := n * n
    halfN := n / 2
    subSquareSize := size / 4
    subSquare, err := magicSquareOdd(halfN)
    if err != nil {
        return nil, err
    }
    quadrantFactors := [4]int{0, 2, 3, 1}
    result := make([][]int, n)

    for i := 0; i < n; i++ {
        result[i] = make([]int, n)
    }

    for r := 0; r < n; r++ {
        for c := 0; c < n; c++ {
            quadrant := r/halfN*2 + c/halfN
            result[r][c] = subSquare[r%halfN][c%halfN]
            result[r][c] += quadrantFactors[quadrant] * subSquareSize
        }
    }

    nColsLeft := halfN / 2
    nColsRight := nColsLeft - 1

    for r := 0; r < halfN; r++ {
        for c := 0; c < n; c++ {
            if c < nColsLeft || c >= n-nColsRight ||
                (c == nColsLeft && r == nColsLeft) {
                if c == 0 && r == nColsLeft {
                    continue
                }
                tmp := result[r][c]
                result[r][c] = result[r+halfN][c]
                result[r+halfN][c] = tmp
            }
        }
    }
    return result, nil
}

func main() {
    const n = 6
    msse, err := magicSquareSinglyEven(n)
    if err != nil {
        log.Fatal(err)
    }
    for _, row := range msse {
        for _, x := range row {
            fmt.Printf("%2d ", x)
        }
        fmt.Println()
    }
    fmt.Printf("\nMagic constant: %d\n", (n*n+1)*n/2)
}
```


{{out}}

```txt

35  1  6 26 19 24
 3 32  7 21 23 25
31  9  2 22 27 20
 8 28 33 17 10 15
30  5 34 12 14 16
 4 36 29 13 18 11

Magic constant: 111

```



## Haskell

Using [[wp:Conway's LUX method for magic squares|Conway's LUX method for magic squares]]

```haskell
import qualified Data.Map.Strict as M
import Data.List (transpose, intercalate)
import Data.Maybe (fromJust, isJust)
import Control.Monad (forM_)
import Data.Monoid ((<>))

magic :: Int -> [[Int]]
magic n = mapAsTable ((4 * n) + 2) (hiResMap n)

-- Order of square -> sequence numbers keyed by cartesian coordinates
hiResMap :: Int -> M.Map (Int, Int) Int
hiResMap n =
  let mapLux = luxMap n
      mapSiam = siamMap n
  in M.fromList $
     foldMap
       (\(xy, n) ->
           luxNums xy (fromJust (M.lookup xy mapLux)) ((4 * (n - 1)) + 1))
       (M.toList mapSiam)

-- LUX table coordinate -> L|U|X -> initial number -> 4 numbered coordinates
luxNums :: (Int, Int) -> Char -> Int -> [((Int, Int), Int)]
luxNums xy lux n =
  zipWith (\x d -> (x, n + d)) (hiRes xy) $
  case lux of
    'L' -> [3, 0, 1, 2]
    'U' -> [0, 3, 1, 2]
    'X' -> [0, 3, 2, 1]
    _ -> [0, 0, 0, 0]

-- Size of square -> integers keyed by coordinates -> rows of integers
mapAsTable :: Int -> M.Map (Int, Int) Int -> [[Int]]
mapAsTable nCols xyMap =
  let axis = [0 .. nCols - 1]
  in fmap (fromJust . flip M.lookup xyMap) <$>
     (axis >>= \y -> [axis >>= \x -> [(x, y)]])

-- Dimension of LUX table -> LUX symbols keyed by coordinates
luxMap :: Int -> M.Map (Int, Int) Char
luxMap n =
  (M.fromList . concat) $
  zipWith
    (\y xs -> (zipWith (\x c -> ((x, y), c)) [0 ..] xs))
    [0 ..]
    (luxPattern n)

-- LUX dimension -> square of L|U|X cells with two mixed rows
luxPattern :: Int -> [String]
luxPattern n =
  let d = (2 * n) + 1
      [ls, us] = replicate n <$> "LU"
      [lRow, xRow] = replicate d <$> "LX"
  in replicate n lRow <> [ls <> ('U' : ls)] <> [us <> ('L' : us)] <>
     replicate (n - 1) xRow

-- Highest zero-based index of grid -> Siamese indices keyed by coordinates
siamMap :: Int -> M.Map (Int, Int) Int
siamMap n =
  let uBound = (2 * n)
      sPath uBound sMap (x, y) n =
        let newMap = M.insert (x, y) n sMap
        in if y == uBound && x == quot uBound 2
             then newMap
             else sPath uBound newMap (nextSiam uBound sMap (x, y)) (n + 1)
  in sPath uBound (M.fromList []) (n, 0) 1

-- Highest index of square -> Siam xys so far -> xy -> next xy coordinate
nextSiam :: Int -> M.Map (Int, Int) Int -> (Int, Int) -> (Int, Int)
nextSiam uBound sMap (x, y) =
  let alt (a, b)
        | a > uBound && b < 0 = (uBound, 1) -- Top right corner ?
        | a > uBound = (0, b) -- beyond right edge ?
        | b < 0 = (a, uBound) -- above top edge ?
        | isJust (M.lookup (a, b) sMap) = (a - 1, b + 2) -- already filled ?
        | otherwise = (a, b) -- Up one, right one.
  in alt (x + 1, y - 1)

-- LUX cell coordinate -> four coordinates at higher resolution
hiRes :: (Int, Int) -> [(Int, Int)]
hiRes (x, y) =
  let [col, row] = (* 2) <$> [x, y]
      [col1, row1] = succ <$> [col, row]
  in [(col, row), (col1, row), (col, row1), (col1, row1)]

-- TESTS ----------------------------------------------------------------------
checked :: [[Int]] -> (Int, Bool)
checked square = (h, all (h ==) t)
  where
    diagonals = fmap (flip (zipWith (!!)) [0 ..]) . ((:) <*> (return . reverse))
    h:t = sum <$> square <> transpose square <> diagonals square

table :: String -> [[String]] -> [String]
table delim rows =
  let justifyRight c n s = drop (length s) (replicate n c <> s)
  in intercalate delim <$>
     transpose
       ((fmap =<< justifyRight ' ' . maximum . fmap length) <$> transpose rows)

main :: IO ()
main =
  forM_ [1, 2, 3] $
  \n -> do
    let test = magic n
    putStrLn $ unlines (table " " (fmap show <$> test))
    print $ checked test
    putStrLn ""
```

{{Out}}

```txt
32 29  4  1 24 21
30 31  2  3 22 23
12  9 17 20 28 25
10 11 18 19 26 27
13 16 36 33  5  8
14 15 34 35  6  7

(111,True)

68 65 96 93  4   1 32 29 60 57
66 67 94 95  2   3 30 31 58 59
92 89 20 17 28  25 56 53 64 61
90 91 18 19 26  27 54 55 62 63
16 13 24 21 49  52 80 77 88 85
14 15 22 23 50  51 78 79 86 87
37 40 45 48 76  73 81 84  9 12
38 39 46 47 74  75 82 83 10 11
41 44 69 72 97 100  5  8 33 36
43 42 71 70 99  98  7  6 35 34

(505,True)

120 117 156 153 192 189   4   1  40  37  76  73 112 109
118 119 154 155 190 191   2   3  38  39  74  75 110 111
152 149 188 185  28  25  36  33  72  69 108 105 116 113
150 151 186 187  26  27  34  35  70  71 106 107 114 115
184 181  24  21  32  29  68  65 104 101 140 137 148 145
182 183  22  23  30  31  66  67 102 103 138 139 146 147
 20  17  56  53  64  61  97 100 136 133 144 141 180 177
 18  19  54  55  62  63  98  99 134 135 142 143 178 179
 49  52  57  60  93  96 132 129 165 168 173 176  13  16
 50  51  58  59  94  95 130 131 166 167 174 175  14  15
 81  84  89  92 125 128 161 164 169 172   9  12  45  48
 83  82  91  90 127 126 163 162 171 170  11  10  47  46
 85  88 121 124 157 160 193 196   5   8  41  44  77  80
 87  86 123 122 159 158 195 194   7   6  43  42  79  78

(1379,True)
```



## J

Using the Strachey method:


```J

odd =: i:@<.@-: |."0 1&|:^:2 >:@i.@,~
t =: ((*: * i.@4:) +"0 2 odd)@-:
l =: (f=:$~ # , #)@((<. , >.)@%&4 # (1: , 0:))
sh =: <:@-: * (bn=:-: # 2:) #: (2: ^ <.@%&4)
lm =: sh |."0 1  l
rm =: f@bn #: <:@(2: ^ <:@<.@%&4)
a =: ((-.@lm * {.@t) + lm * {:@t)
b =: ((-.@rm * 1&{@t) + rm * 2&{@t)
c =: ((rm * 1&{@t) + -.@rm * 2&{@t)
d =: ((lm * {.@t) + -.@lm * {:@t)
m =: (a ,"1 c) , d ,"1 b

```


{{Out}}

```txt

m 6
33  7  2 24 25 20
 1 32  9 19 23 27
35  3  4 26 21 22
 6 34 29 15 16 11
28  5 36 10 14 18
 8 30 31 17 12 13

m 18
258 268 278 288  46  56  66  76   5 177 187 197 207 208 218 147 157  86
277 287 297 298  65  75   4  14  24 196 206 216 217 227 237  85  95 105
296 306 307 317   3  13  23  33  43 215 225 226 236 165 175 104 114 124
315 316 245 255  22  32  42  52  62 234 235 164 174 184 194 123 133 143
  1 254 264 274 284  51  61  71  81 163 173 183 193 203 213 142 152 162
263 273 283 293  60  70  80   9  10 182 192 202 212 222 232 161  90  91
282 292 302 312  79   8  18  19  29 201 211 221 231 241 170  99 100 110
301 311 321 250  17  27  28  38  48 220 230 240 169 179 189 109 119 129
320 249 259 269  36  37  47  57  67 239 168 178 188 198 199 128 138 148
 15  25  35  45 289 299 309 319 248  96 106 116 126 127 137 228 238 167
 34  44  54  55 308 318 247 257 267 115 125 135 136 146 156 166 176 186
 53  63  64  74 246 256 266 276 286 134 144 145 155  84  94 185 195 205
 72  73   2  12 265 275 285 295 305 153 154  83  93 103 113 204 214 224
244  11  21  31  41 294 304 314 324  82  92 102 112 122 132 223 233 243
 20  30  40  50 303 313 323 252 253 101 111 121 131 141 151 242 171 172
 39  49  59  69 322 251 261 262 272 120 130 140 150 160  89 180 181 191
 58  68  78   7 260 270 271 281 291 139 149 159  88  98 108 190 200 210
 77   6  16  26 279 280 290 300 310 158  87  97 107 117 118 209 219 229

```



## Java


```java
public class MagicSquareSinglyEven {

    public static void main(String[] args) {
        int n = 6;
        for (int[] row : magicSquareSinglyEven(n)) {
            for (int x : row)
                System.out.printf("%2s ", x);
            System.out.println();
        }
        System.out.printf("\nMagic constant: %d ", (n * n + 1) * n / 2);
    }

    public static int[][] magicSquareOdd(final int n) {
        if (n < 3 || n % 2 == 0)
            throw new IllegalArgumentException("base must be odd and > 2");

        int value = 0;
        int gridSize = n * n;
        int c = n / 2, r = 0;

        int[][] result = new int[n][n];

        while (++value <= gridSize) {
            result[r][c] = value;
            if (r == 0) {
                if (c == n - 1) {
                    r++;
                } else {
                    r = n - 1;
                    c++;
                }
            } else if (c == n - 1) {
                r--;
                c = 0;
            } else if (result[r - 1][c + 1] == 0) {
                r--;
                c++;
            } else {
                r++;
            }
        }
        return result;
    }

    static int[][] magicSquareSinglyEven(final int n) {
        if (n < 6 || (n - 2) % 4 != 0)
            throw new IllegalArgumentException("base must be a positive "
                    + "multiple of 4 plus 2");

        int size = n * n;
        int halfN = n / 2;
        int subSquareSize = size / 4;

        int[][] subSquare = magicSquareOdd(halfN);
        int[] quadrantFactors = {0, 2, 3, 1};
        int[][] result = new int[n][n];

        for (int r = 0; r < n; r++) {
            for (int c = 0; c < n; c++) {
                int quadrant = (r / halfN) * 2 + (c / halfN);
                result[r][c] = subSquare[r % halfN][c % halfN];
                result[r][c] += quadrantFactors[quadrant] * subSquareSize;
            }
        }

        int nColsLeft = halfN / 2;
        int nColsRight = nColsLeft - 1;

        for (int r = 0; r < halfN; r++)
            for (int c = 0; c < n; c++) {
                if (c < nColsLeft || c >= n - nColsRight
                        || (c == nColsLeft && r == nColsLeft)) {

                    if (c == 0 && r == nColsLeft)
                        continue;

                    int tmp = result[r][c];
                    result[r][c] = result[r + halfN][c];
                    result[r + halfN][c] = tmp;
                }
            }

        return result;
    }
}
```



```txt
35  1  6 26 19 24
 3 32  7 21 23 25
31  9  2 22 27 20
 8 28 33 17 10 15
30  5 34 12 14 16
 4 36 29 13 18 11

Magic constant: 111
```



## Julia

{{trans|Lua}}

```julia
function oddmagicsquare(order)
    if iseven(order)
        order += 1
    end
    q = zeros(Int, (order, order))
    p = 1
    i = div(order, 2) + 1
    j = 1
    while p <= order * order
        q[i, j] = p
        ti = (i + 1 > order) ? 1 : i + 1
        tj = (j - 1 < 1) ? order : j - 1
        if q[ti, tj] != 0
            ti = i
            tj = j + 1
        end
        i = ti
        j = tj
        p = p + 1
    end
    q, order
end

function singlyevenmagicsquare(order)
    if isodd(order)
        order += 1
    end
    if order % 4 == 0
        order += 2
    end
    q = zeros(Int, (order, order))
    z = div(order, 2)
    b = z * z
    c = 2 * b
    d = 3 * b
    sq, ord = oddmagicsquare(z)

    for j in 1:z, i in 1:z
        a = sq[i, j]
        q[i, j] = a
        q[i + z, j + z] = a + b
        q[i + z, j] = a + c
        q[i, j + z] = a + d
    end
    lc = div(z, 2)
    rc = lc - 1
    for j in 1:z, i in 1:order
        if i <= lc || i > order - rc || (i == lc && j == lc)
            if i != 0 || j != lc + 1
                t = q[i, j]
                q[i, j] = q[i, j + z]
                q[i, j + z] = t
            end
        end
    end
    q, order
end

function check(q)
    side = size(q)[1]
    sums = Vector{Int}()
    for n in 1:side
        push!(sums, sum(q[n, :]))
        push!(sums, sum(q[:, n]))
    end
    println(all(x->x==sums[1], sums) ?
        "Checks ok: all sides add to $(sums[1])." : "Bad sum.")
end

function display(q)
    r, c = size(q)
    for i in 1:r, j in 1:c
        nstr = lpad(string(q[i, j]), 4)
        print(j % c > 0 ? nstr : "$nstr\n")
    end
end

for o in (6, 10)
    println("\nWith order $o:")
    msq = singlyevenmagicsquare(o)[1]
    display(msq)
    check(msq)
end

```
 {{output}}
```txt

 With order 6:
  35  30  31   8   3   4
   1   5   9  28  32  36
   6   7   2  33  34  29
  26  21  22  17  12  13
  19  23  27  10  14  18
  24  25  20  15  16  11
 Checks ok: all sides add to 111.

 With order 10:
  92  98  79  85  86  17  23   4  10  11
  99  80  81  87  93  24   5   6  12  18
   1   7  13  19  25  76  82  88  94 100
   8  14  20  21   2  83  89  95  96  77
  15  16  22   3   9  90  91  97  78  84
  67  73  54  60  61  42  48  29  35  36
  74  55  56  62  68  49  30  31  37  43
  51  57  63  69  75  26  32  38  44  50
  58  64  70  71  52  33  39  45  46  27
  40  41  47  28  34  65  66  72  53  59
 Checks ok: all sides add to 505.

```



## Kotlin

{{trans|Java}}

```scala
// version 1.0.6

fun magicSquareOdd(n: Int): Array<IntArray> {
    if (n < 3 || n % 2 == 0)
         throw IllegalArgumentException("Base must be odd and > 2")

    var value = 0
    val gridSize = n * n
    var c = n / 2
    var r = 0
    val result = Array(n) { IntArray(n) }
    while (++value <= gridSize) {
        result[r][c] = value
        if (r == 0) {
            if (c == n - 1) r++
            else {
                r = n - 1
                c++
            }
        }
        else if (c == n - 1) {
            r--
            c = 0
        }
        else if (result[r - 1][c + 1] == 0) {
            r--
            c++
        }
        else r++
    }
    return result
}

fun magicSquareSinglyEven(n: Int): Array<IntArray> {
    if (n < 6 || (n - 2) % 4 != 0)
        throw IllegalArgumentException("Base must be a positive multiple of 4 plus 2")

    val size = n * n
    val halfN = n / 2
    val subSquareSize = size / 4
    val subSquare = magicSquareOdd(halfN)
    val quadrantFactors = intArrayOf(0, 2, 3, 1)
    val result = Array(n) { IntArray(n) }
    for (r in 0 until n)
        for (c in 0 until n) {
            val quadrant = r / halfN * 2  + c / halfN
            result[r][c] = subSquare[r % halfN][c % halfN]
            result[r][c] += quadrantFactors[quadrant] * subSquareSize
        }
    val nColsLeft = halfN / 2
    val nColsRight = nColsLeft - 1
    for (r in 0 until halfN)
        for (c in 0 until n)
            if (c < nColsLeft || c >= n - nColsRight || (c == nColsLeft && r == nColsLeft)) {
                if (c == 0 && r == nColsLeft) continue
                val tmp = result[r][c]
                result[r][c] = result[r + halfN][c]
                result[r + halfN][c] = tmp
            }
    return result
}

fun main(args: Array<String>) {
    val n = 6
    for (ia in magicSquareSinglyEven(n)) {
        for (i in ia) print("%2d  ".format(i))
        println()
    }
    println("\nMagic constant ${(n * n + 1) * n / 2}")
}
```


{{out}}

```txt

35   1   6  26  19  24
 3  32   7  21  23  25
31   9   2  22  27  20
 8  28  33  17  10  15
30   5  34  12  14  16
 4  36  29  13  18  11

Magic constant 111

```



## Lua

For all three kinds of Magic Squares(Odd, singly and doubly even)<br />
See [[Magic_squares/Lua]].


## Perl


See [[Magic_squares/Perl|Magic squares/Perl]] for a general magic square generator.

```perl

```



## Perl 6

See [[Magic_squares/Perl_6|Magic squares/Perl 6]] for a general magic square generator.
{{out}}
With a parameter of 6:

```txt
35  1  6 26 19 24
 3 32  7 21 23 25
31  9  2 22 27 20
 8 28 33 17 10 15
30  5 34 12 14 16
 4 36 29 13 18 11

The magic number is 111
```


With a parameter of 10:

```txt
 92  99   1   8  15  67  74  51  58  40
 98  80   7  14  16  73  55  57  64  41
  4  81  88  20  22  54  56  63  70  47
 85  87  19  21   3  60  62  69  71  28
 86  93  25   2   9  61  68  75  52  34
 17  24  76  83  90  42  49  26  33  65
 23   5  82  89  91  48  30  32  39  66
 79   6  13  95  97  29  31  38  45  72
 10  12  94  96  78  35  37  44  46  53
 11  18 100  77  84  36  43  50  27  59

The magic number is 505
```



## Phix

{{trans|FreeBASIC}}

```Phix
procedure Abort(string msg)
    puts(1,msg&"\nPress any key...")
    {} = wait_key()
    abort(0)
end procedure

function swap(sequence s, integer x1, y1, x2, y2)
    {s[x1,y1],s[x2,y2]} = {s[x2,y2],s[x1,y1]}
    return s
end function

function se_magicsq(integer n)

    if n<6 or mod(n-2,4)!=0 then
        Abort(sprintf("illegal size (%d)",{n}))
    end if

    sequence sq = repeat(repeat(0,n),n)
    integer magic_sum = n*(n*n+1)/2,
            sq_d_2 = n/2,
            q2 = power(sq_d_2,2),
            l = (n-2)/4,
            x1 = floor(sq_d_2/2)+1, x2,
            y1 = 1, y2,
            r = 1

    -- fill pattern a c
    --              d b
    -- main loop for creating magic square in section a
    -- the value for b,c and d is derived from a
    while true do
        if sq[x1,y1]=0 then
            x2 = x1+sq_d_2
            y2 = y1+sq_d_2
            sq[x1,y1] = r       -- a
            sq[x2,y2] = r+q2    -- b
            sq[x2,y1] = r+q2*2  -- c
            sq[x1,y2] = r+q2*3  -- d
            if mod(r,sq_d_2)=0 then
                y1 += 1
            else
                x1 += 1
                y1 -= 1
            end if
            r += 1
        end if
        if x1>sq_d_2 then
            x1 = 1
            while sq[x1,y1] <> 0 do
                x1 += 1
            end while
        end if
        if y1<1 then
            y1 = sq_d_2
            while sq[x1,y1] <> 0 do
                y1 -= 1
            end while
        end if
        if r>q2 then exit end if
    end while

    -- swap left side
    for y1=1 to sq_d_2 do
        y2 = y1+sq_d_2
        for x1=1 to l do
            sq = swap(sq, x1,y1, x1,y2)
        end for
    end for

    -- make indent
    y1 = floor(sq_d_2/2) +1
    y2 = y1+sq_d_2
    x1 = 1
    sq = swap(sq, x1,y1, x1,y2)
    x1 = l+1
    sq = swap(sq, x1,y1, x1,y2)

    -- swap right side
    for y1=1 to sq_d_2 do
        y2 = y1+sq_d_2
        for x1=n-l+2 to n do
            sq = swap(sq, x1,y1, x1,y2)
        end for
    end for

    -- check columms and rows
    for y1=1 to n do
        r = 0
        l = 0
        for x1=1 to n do
            r += sq[x1,y1]
            l += sq[y1,x1]
        end for
        if r<>magic_sum
        or l<>magic_sum then
            Abort("error: value <> magic_sum")
        end if
    end for

    -- check diagonals
    r = 0
    l = 0
    for x1=1 to n do
        r += sq[x1,x1]
        x2 = n-x1+1
        l += sq[x2,x2]
    end for
    if r<>magic_sum
    or l<>magic_sum then
        Abort("error: value <> magic_sum")
    end if

    return sq
end function

pp(se_magicsq(6),{pp_Nest,1,pp_IntFmt,"%3d",pp_StrFmt,1,pp_Pause,0})
```

{{out}}

```txt

{{35, 3,31, 8,30, 4},
 { 1,32, 9,28, 5,36},
 { 6, 7, 2,33,34,29},
 {26,21,22,17,12,13},
 {19,23,27,10,14,18},
 {24,25,20,15,16,11}}

```



## Python

{{trans|Lua}}

```python

import math
from sys import stdout

LOG_10 = 2.302585092994


# build odd magic square
def build_oms(s):
    if s % 2 == 0:
        s += 1
    q = [[0 for j in range(s)] for i in range(s)]
    p = 1
    i = s // 2
    j = 0
    while p <= (s * s):
        q[i][j] = p
        ti = i + 1
        if ti >= s: ti = 0
        tj = j - 1
        if tj < 0: tj = s - 1
        if q[ti][tj] != 0:
            ti = i
            tj = j + 1
        i = ti
        j = tj
        p = p + 1

    return q, s


# build singly even magic square
def build_sems(s):
    if s % 2 == 1:
        s += 1
    while s % 4 == 0:
        s += 2

    q = [[0 for j in range(s)] for i in range(s)]
    z = s // 2
    b = z * z
    c = 2 * b
    d = 3 * b
    o = build_oms(z)

    for j in range(0, z):
        for i in range(0, z):
            a = o[0][i][j]
            q[i][j] = a
            q[i + z][j + z] = a + b
            q[i + z][j] = a + c
            q[i][j + z] = a + d

    lc = z // 2
    rc = lc
    for j in range(0, z):
        for i in range(0, s):
            if i < lc or i > s - rc or (i == lc and j == lc):
                if not (i == 0 and j == lc):
                    t = q[i][j]
                    q[i][j] = q[i][j + z]
                    q[i][j + z] = t

    return q, s


def format_sqr(s, l):
    for i in range(0, l - len(s)):
        s = "0" + s
    return s + " "


def display(q):
    s = q[1]
    print(" - {0} x {1}\n".format(s, s))
    k = 1 + math.floor(math.log(s * s) / LOG_10)
    for j in range(0, s):
        for i in range(0, s):
            stdout.write(format_sqr("{0}".format(q[0][i][j]), k))
        print()
    print("Magic sum: {0}\n".format(s * ((s * s) + 1) // 2))


stdout.write("Singly Even Magic Square")
display(build_sems(6))

```

{{out}}

```txt
Singly Even Magic Square - 6 x 6

35 01 06 26 19 24
03 32 07 21 23 25
31 09 02 22 27 20
08 28 33 17 10 15
30 05 34 12 14 16
04 36 29 13 18 11
Magic sum: 111
```



## Ruby



```ruby
def odd_magic_square(n)
  n.times.map{|i| n.times.map{|j| n*((i+j+1+n/2)%n) + ((i+2*j-5)%n) + 1} }
end

def single_even_magic_square(n)
  raise ArgumentError, "must be even, but not divisible by 4." unless (n-2) % 4 == 0
  raise ArgumentError, "2x2 magic square not possible." if n == 2

  order = (n-2)/4
  odd_square = odd_magic_square(n/2)
  to_add = (0..3).map{|f| f*n*n/4}
  quarts = to_add.map{|f| odd_square.dup.map{|row|row.map{|el| el+f}} }

  sq = []
  quarts[0].zip(quarts[2]){|d1,d2| sq << [d1,d2].flatten}
  quarts[3].zip(quarts[1]){|d1,d2| sq << [d1,d2].flatten}

  sq = sq.transpose
  order.times{|i| sq[i].rotate!(n/2)}
  swap(sq[0][order], sq[0][-order-1])
  swap(sq[order][order], sq[order][-order-1])
  (order-1).times{|i| sq[-(i+1)].rotate!(n/2)}
  randomize(sq)
end

def swap(a,b)
  a,b = b,a
end

def randomize(square)
  square.shuffle.transpose.shuffle
end

def to_string(square)
  n = square.size
  fmt = "%#{(n*n).to_s.size + 1}d" * n
  square.inject(""){|str,row| str << fmt % row << "\n"}
end

puts to_string(single_even_magic_square(6))
```

{{out}}

```txt

 23  7  5 21 30 25
 18 29 36 13  4 11
 14 34 32 12  3 16
 19  6  1 26 35 24
 27  2  9 22 31 20
 10 33 28 17  8 15

```



### LUX method

[[wp:Conway's LUX method for magic squares]]

```ruby
class Magic_square
  attr_reader :square
  LUX = { L: [[4, 1], [2, 3]], U: [[1, 4], [2, 3]], X: [[1, 4], [3, 2]] }

  def initialize(n)
    raise ArgumentError, "must be even, but not divisible by 4." unless (n-2) % 4 == 0
    raise ArgumentError, "2x2 magic square not possible." if n == 2
    @n = n
    oms = odd_magic_square(n/2)
    mat = make_lux_matrix(n/2)
    @square = synthesize(oms, mat)
    puts to_s
  end

  def odd_magic_square(n)       # zero beginning, it is 4 multiples.
    n.times.map{|i| n.times.map{|j| (n*((i+j+1+n/2)%n) + ((i+2*j-5)%n)) * 4} }
  end

  def make_lux_matrix(n)
    center = n / 2
    lux = [*[:L]*(center+1), :U, *[:X]*(n-center-2)]
    matrix = lux.map{|x| Array.new(n, x)}
    matrix[center][center] = :U
    matrix[center+1][center] = :L
    matrix
  end

  def synthesize(oms, mat)
    range = 0...@n/2
    range.inject([]) do |matrix,i|
      row = [[], []]
      range.each do |j|
        x = oms[i][j]
        LUX[mat[i][j]].each_with_index{|lux,k| row[k] << lux.map{|y| x+y}}
      end
      matrix << row[0].flatten << row[1].flatten
    end
  end

  def to_s
    format = "%#{(@n*@n).to_s.size}d " * @n + "\n"
    @square.map{|row| format % row}.join
  end
end

sq = Magic_square.new(6).square
```


{{out}}

```txt

32 29  4  1 24 21
30 31  2  3 22 23
12  9 17 20 28 25
10 11 18 19 26 27
13 16 36 33  5  8
14 15 34 35  6  7

```



## Rust


```rust
use std::env;

fn main() {
    let n: usize =
        match env::args().nth(1).and_then(|arg| arg.parse().ok()).ok_or(
            "Please specify the size of the magic square, as a positive multiple of 4 plus 2.",
        ) {
            Ok(arg) if arg % 2 == 1 || arg >= 6 && (arg - 2) % 4 == 0 => arg,
            Err(e) => panic!(e),
            _ => panic!("Argument must be a positive multiple of 4 plus 2."),
        };

    let (ms, mc) = magic_square_singly_even(n);
    println!("n: {}", n);
    println!("Magic constant: {}\n", mc);
    let width = (n * n).to_string().len() + 1;
    for row in ms {
        for elem in row {
            print!("{e:>w$}", e = elem, w = width);
        }
        println!();
    }
}

fn magic_square_singly_even(n: usize) -> (Vec<Vec<usize>>, usize) {
    let size = n * n;
    let half = n / 2;
    let sub_square_size = size / 4;
    let sub_square = magic_square_odd(half);
    let quadrant_factors = [0, 2, 3, 1];
    let cols_left = half / 2;
    let cols_right = cols_left - 1;

    let ms = (0..n)
        .map(|r| {
            (0..n)
                .map(|c| {
                    let localr = if (c < cols_left
                        || c >= n - cols_right
                        || c == cols_left && r % half == cols_left)
                        && !(c == 0 && r % half == cols_left)
                    {
                        if r >= half {
                            r - half
                        } else {
                            r + half
                        }
                    } else {
                        r
                    };
                    let quadrant = localr / half * 2 + c / half;
                    let v = sub_square[localr % half][c % half];
                    v + quadrant_factors[quadrant] * sub_square_size
                })
                .collect()
        })
        .collect::<Vec<Vec<_>>>();
    (ms, (n * n + 1) * n / 2)
}

fn magic_square_odd(n: usize) -> Vec<Vec<usize>> {
    (0..n)
        .map(|r| {
            (0..n)
                .map(|c| {
                    n * (((c + 1) + (r + 1) - 1 + (n >> 1)) % n)
                        + (((c + 1) + (2 * (r + 1)) - 2) % n)
                        + 1
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<Vec<_>>>()
}
```

{{out}}

```txt

n: 6
Magic constant: 111

 35  3  4 26 21 22
  1 32  9 19 23 27
 33  7  2 24 25 20
  8 30 31 17 12 13
 28  5 36 10 14 18
  6 34 29 15 16 11


n: 10
Magic constant: 505

  92  98   4  10  11  67  73  54  60  36
  99  80   6  12  18  74  55  56  62  43
   1  82  88  19  25  51  57  63  69  50
  83  89  20  21   2  58  64  70  71  27
  90  91  22   3   9  65  66  72  53  34
  17  23  79  85  86  42  48  29  35  61
  24   5  81  87  93  49  30  31  37  68
  76   7  13  94 100  26  32  38  44  75
   8  14  95  96  77  33  39  45  46  52
  15  16  97  78  84  40  41  47  28  59


n: 18
Magic constant: 2925

 290 300 310 320   6  16  26  36  37 209 219 229 239 168 178 107 117 118
 301 311 321 250  17  27  28  38  48 220 230 240 169 179 189 109 119 129
 312 322 251 261  19  29  39  49  59 231 241 170 180 181 191 120 130 140
 323 252 253 263  30  40  50  60  70 242 171 172 182 192 202 131 141 151
   1 254 264 274 284  51  61  71  81 163 173 183 193 203 213 142 152 162
 255 265 275 285  52  62  72  73   2 174 184 194 204 214 224 153 154  83
 266 276 286 296  63  64  74   3  13 185 195 205 215 225 226 155  84  94
 277 287 297 298  65  75   4  14  24 196 206 216 217 227 237  85  95 105
 288 289 299 309  76   5  15  25  35 207 208 218 228 238 167  96 106 116
  47  57  67  77 249 259 269 279 280 128 138 148 158  87  97 188 198 199
  58  68  78   7 260 270 271 281 291 139 149 159  88  98 108 190 200 210
  69  79   8  18 262 272 282 292 302 150 160  89  99 100 110 201 211 221
  80   9  10  20 273 283 293 303 313 161  90  91 101 111 121 212 222 232
 244  11  21  31  41 294 304 314 324  82  92 102 112 122 132 223 233 243
  12  22  32  42 295 305 315 316 245  93 103 113 123 133 143 234 235 164
  23  33  43  53 306 307 317 246 256 104 114 124 134 144 145 236 165 175
  34  44  54  55 308 318 247 257 267 115 125 135 136 146 156 166 176 186
  45  46  56  66 319 248 258 268 278 126 127 137 147 157  86 177 187 197

```



## zkl

{{trans|Java}}

```zkl
class MagicSquareSinglyEven{
   fcn init(n){ var result=magicSquareSinglyEven(n) }
   fcn toString{
      sink,n:=Sink(String),result.len();  // num collumns
      fmt:="%2s ";
      foreach row in (result)
         { sink.write(row.apply('wrap(n){ fmt.fmt(n) }).concat(),"\n") }
      sink.write("\nMagic constant: %d".fmt((n*n + 1)*n/2));
      sink.close();
   }
   fcn magicSquareOdd(n){
      if (n<3 or n%2==0) throw(Exception.ValueError("base must be odd and > 2"));
      value,gridSize,c,r:=0, n*n, n/2, 0;
      result:=n.pump(List(),n.pump(List(),0).copy);  // array[n,n] of zero

      while((value+=1)<=gridSize){
	 result[r][c]=value;
	 if(r==0){
	    if(c==n-1) r+=1;
            else{ r=n-1; c+=1; }
	 }
	 else if(c==n-1){ r-=1; c=0; }
	 else if(result[r-1][c+1]==0){ r-=1; c+=1; }
	 else r+=1;
      }
      result;
   }
   fcn magicSquareSinglyEven(n){
      if (n<6 or (n-2)%4!=0)
	 throw(Exception.ValueError("base must be a positive multiple of 4 +2"));
      size,halfN,subSquareSize:=n*n,  n/2, size/4;

      subSquare:=magicSquareOdd(halfN);
      quadrantFactors:=T(0, 2, 3, 1);
      result:=n.pump(List(),n.pump(List(),0).copy);  // array[n,n] of zero

      foreach r,c in (n,n){
         quadrant:=(r/halfN)*2 + (c/halfN);
	 result[r][c]=subSquare[r%halfN][c%halfN];
	 result[r][c]+=quadrantFactors[quadrant]*subSquareSize;
      }
      nColsLeft,nColsRight:=halfN/2, nColsLeft-1;
      foreach r,c in (halfN,n){
         if ( c<nColsLeft or c>=(n-nColsRight) or
              (c==nColsLeft and r==nColsLeft) ){
	    if(c==0 and r==nColsLeft) continue;
	    tmp:=result[r][c];
	    result[r][c]=result[r+halfN][c];
	    result[r+halfN][c]=tmp;
	 }
      }
      result
   }
}
```


```zkl
MagicSquareSinglyEven(6).println();
```

{{out}}

```txt

35  1  6 26 19 24
 3 32  7 21 23 25
31  9  2 22 27 20
 8 28 33 17 10 15
30  5 34 12 14 16
 4 36 29 13 18 11

Magic constant: 111

```

+++
title = "Spiral matrix"
description = ""
date = 2019-07-08T18:10:11Z
aliases = []
[extra]
id = 2967
[taxonomies]
categories = []
tags = []
+++

{{task|Matrices}}

;Task:
Produce a spiral array.


A   ''spiral array''   is a square arrangement of the first   <big> N<sup>2</sup></big>   natural numbers,   where the

numbers increase sequentially as you go around the edges of the array spiraling inwards.


For example, given   '''5''',   produce this array:

```txt

 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8

```



;Related tasks:
*   [[Zig-zag matrix]]
*   [[Identity_matrix]]
*   [[Ulam_spiral_(for_primes)]]





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.
{{trans|BBC BASIC}}

```360asm
SPIRALM  CSECT
         USING  SPIRALM,R13
SAVEAREA B      STM-SAVEAREA(R15)
         DC     17F'0'
         DC     CL8'SPIRALM'
STM      STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15
*        ----   CODE
         LA     R0,0
         LA     R1,1
         LH     R12,N           n
         LR     R4,R1           Row=1
         LR     R5,R1           Col=1
         LR     R6,R1           BotRow=1
         LR     R7,R1           BotCol=1
         LR     R8,R12          TopRow=n
         LR     R9,R12          TopCol=n
         LR     R10,R0          Dir=0
         LR     R15,R12         n
         MR     R14,R12         R15=n*n
         LA     R11,1           k=1
LOOP     CR     R11,R15
         BH     ENDLOOP
         LR     R1,R4
         BCTR   R1,0
         MH     R1,N
         AR     R1,R5
         LR     R2,R11          k
         BCTR   R2,0
         BCTR   R1,0
         SLA    R1,1
         STH    R2,MATRIX(R1)   Matrix(Row,Col)=k-1
         CH     R10,=H'0'
         BE     DIR0
         CH     R10,=H'1'
         BE     DIR1
         CH     R10,=H'2'
         BE     DIR2
         CH     R10,=H'3'
         BE     DIR3
         B      DIRX
DIR0     CR     R5,R9           if Col<TopCol
         BNL    DIR0S
         LA     R5,1(R5)        Col=Col+1
         B      DIRX
DIR0S    LA     R10,1           Dir=1
         LA     R4,1(R4)        Row=Row+1
         LA     R6,1(R6)        BotRow=BotRow+1
         B      DIRX
DIR1     CR     R4,R8           if Row<TopRow
         BNL    DIR1S
         LA     R4,1(R4)        Row=Row+1
         B      DIRX
DIR1S    LA     R10,2           Dir=2
         BCTR   R5,0            Col=Col-1
         BCTR   R9,0            TopCol=TopCol-1
         B      DIRX
DIR2     CR     R5,R7           if Col>BotCol
         BNH    DIR2S
         BCTR   R5,0            Col=Col-1
         B      DIRX
DIR2S    LA     R10,3           Dir=3
         BCTR   R4,0            Row=Row-1
         BCTR   R8,0            TopRow=TopRow-1
         B      DIRX
DIR3     CR     R4,R6           if Row>BotRow
         BNH    DIR3S
         BCTR   R4,0            Row=Row-1
         B      DIRX
DIR3S    LA     R10,0           Dir=0
         LA     R5,1(R5)        Col=Col+1
         LA     R7,1(R7)        BotCol=BotCol+1
DIRX     EQU    *
         LA     R11,1(R11)      k=k+1
         B      LOOP
ENDLOOP  EQU    *
         LA     R4,1            i
LOOPI    CR     R4,R12
         BH     ENDLOOPI
         XR     R10,R10
         LA     R5,1            j
LOOPJ    CR     R5,R12
         BH     ENDLOOPJ
         LR     R1,R4
         BCTR   R1,0
         MH     R1,N
         AR     R1,R5
         BCTR   R1,0
         SLA    R1,1
         LH     R2,MATRIX(R1)   Matrix(i,j)
         LA     R3,BUF
         AR     R3,R10
         CVD    R2,P8
         MVC    0(4,R3),=X'40202120'
         ED     0(4,R3),P8+6
         LA     R10,4(R10)
         LA     R5,1(R5)
         B      LOOPJ
ENDLOOPJ EQU    *
         WTO    MF=(E,WTOMSG)
         LA     R4,1(R4)
         B      LOOPI
ENDLOOPI EQU    *
*        ----   END CODE
         L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
*        ----   DATA
N        DC     H'5'            max=20 (20*4=80)
         LTORG
P8       DS     PL8
WTOMSG   DS     0F
         DC     H'80',XL2'0000'
BUF      DC     CL80' '
MATRIX   DS     H               Matrix(n,n)
         YREGS
         END    SPIRALM
```

{{out}}

```txt
   0   1   2   3   4
  15  16  17  18   5
  14  23  24  19   6
  13  22  21  20   7
  12  11  10   9   8
```



## Ada


```ada
-- Spiral Square
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Spiral_Square is
   type Array_Type is array(Positive range <>, Positive range <>) of Natural;

   function Spiral (N : Positive) return Array_Type is
      Result  : Array_Type(1..N, 1..N);
      Row     : Natural := 1;
      Col     : Natural := 1;
      Max_Row : Natural := N;
      Max_Col : Natural := N;
      Min_Row : Natural := 1;
      Min_Col : Natural := 1;
   begin
      for I in 0..N**2 - 1 loop
         Result(Row, Col) := I;
         if Row = Min_Row then
            Col := Col + 1;
            if Col > Max_Col then
               Col := Max_Col;
               Row := Row + 1;
            end if;
         elsif Col = Max_Col then
            Row := Row + 1;
            if Row > Max_Row then
               Row := Max_Row;
               Col := Col - 1;
            end if;
         elsif Row = Max_Row then
            Col := Col - 1;
            if Col < Min_Col then
               Col := Min_Col;
               Row := Row - 1;
            end if;
         elsif Col = Min_Col then
            Row := Row - 1;
            if Row = Min_Row then  -- Reduce spiral
               Min_Row := Min_Row + 1;
               Max_Row := Max_Row - 1;
               Row := Min_Row;
               Min_Col := Min_Col + 1;
               Max_Col := Max_Col - 1;
               Col := Min_Col;
            end if;
         end if;
      end loop;
      return Result;
   end Spiral;

   procedure Print(Item : Array_Type) is
      Num_Digits : constant Float := Log(X => Float(Item'Length(1)**2), Base => 10.0);
      Spacing    : constant Positive := Integer(Num_Digits) + 2;
   begin
      for I in Item'range(1) loop
         for J in Item'range(2) loop
            Put(Item => Item(I,J), Width => Spacing);
         end loop;
         New_Line;
      end loop;
   end Print;

begin
   Print(Spiral(5));
end Spiral_Square;
```

The following is a variant using a different algorithm (which can also be used recursively):

```ada
function Spiral (N : Positive) return Array_Type is
   Result : Array_Type (1..N, 1..N);
   Left   : Positive := 1;
   Right  : Positive := N;
   Top    : Positive := 1;
   Bottom : Positive := N;
   Index  : Natural  := 0;
begin
   while Left < Right loop
      for I in Left..Right - 1 loop
         Result (Top, I) := Index;
         Index := Index + 1;
      end loop;
      for J in Top..Bottom - 1 loop
         Result (J, Right) := Index;
         Index := Index + 1;
      end loop;
      for I in reverse Left + 1..Right loop
         Result (Bottom, I) := Index;
         Index := Index + 1;
      end loop;
      for J in reverse Top + 1..Bottom loop
         Result (J, Left) := Index;
         Index := Index + 1;
      end loop;
      Left   := Left   + 1;
      Right  := Right  - 1;
      Top    := Top    + 1;
      Bottom := Bottom - 1;
   end loop;
   Result (Top, Left) := Index;
   return Result;
end Spiral;
```



## ALGOL 68

{{trans|Python}}
{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
INT empty=0;

PROC spiral = (INT n)[,]INT: (
    INT dx:=1, dy:=0;            # Starting increments #
    INT x:=0, y:=0;              # Starting location #
    [0:n-1,0:n-1]INT my array;
    FOR y FROM LWB my array TO UPB my array DO
        FOR x FROM LWB my array TO UPB my array DO
            my array[x,y]:=empty
        OD
    OD;
    FOR i TO n**2 DO
        my array[x,y] := i;
        INT nx:=x+dx, ny:=y+dy;
        IF ( 0<=nx AND nx<n AND 0<=ny AND ny<n | my array[nx,ny] = empty | FALSE ) THEN
            x:=nx; y:=ny
        ELSE
            INT swap:=dx; dx:=-dy; dy:=swap;
            x+:=dx; y+:=dy
        FI
    OD;
    my array
);

PROC print spiral = ([,]INT my array)VOID:(
    FOR y FROM LWB my array TO UPB my array DO
        FOR x FROM LWB my array TO UPB my array DO
            print(whole(my array[x,y],-3))
        OD;
        print(new line)
    OD
);

print spiral(spiral(5))
```

{{out}}

```txt

  1  2  3  4  5
 16 17 18 19  6
 15 24 25 20  7
 14 23 22 21  8
 13 12 11 10  9

```




## AppleScript


{{Trans|JavaScript}} (ES6)

```AppleScript
-- spiral :: Int -> [[Int]]
on spiral(n)
    script go
        on |λ|(rows, cols, start)
            if 0 < rows then
                {enumFromTo(start, start + pred(cols))} & ¬
                    map(my |reverse|, ¬
                        (transpose(|λ|(cols, pred(rows), start + cols))))
            else
                {{}}
            end if
        end |λ|
    end script

    go's |λ|(n, n, 0)
end spiral

-- TEST ------------------------------------------------------------------
on run

    wikiTable(spiral(5), ¬
        false, ¬
        "text-align:center;width:12em;height:12em;table-layout:fixed;")
end run

-- WIKI TABLE FORMAT ---------------------------------------------------------

-- wikiTable :: [Text] -> Bool -> Text -> Text
on wikiTable(lstRows, blnHdr, strStyle)
    script fWikiRows
        on |λ|(lstRow, iRow)
            set strDelim to if_(blnHdr and (iRow = 0), "!", "|")
            set strDbl to strDelim & strDelim
            linefeed & "|-" & linefeed & strDelim & space & ¬
                intercalateS(space & strDbl & space, lstRow)
        end |λ|
    end script

    linefeed & "{| class=\"wikitable\" " & ¬
        if_(strStyle ≠ "", "style=\"" & strStyle & "\"", "") & ¬
        intercalateS("", ¬
            map(fWikiRows, lstRows)) & linefeed & "|}" & linefeed
end wikiTable

-- GENERIC ------------------------------------------------------------------

-- comparing :: (a -> b) -> (a -> a -> Ordering)
on comparing(f)
    script
        on |λ|(a, b)
            tell mReturn(f)
                set fa to |λ|(a)
                set fb to |λ|(b)
                if fa < fb then
                    -1
                else if fa > fb then
                    1
                else
                    0
                end if
            end tell
        end |λ|
    end script
end comparing

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    if 0 < lng and class of xs is string then
        set acc to ""
    else
        set acc to {}
    end if
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & |λ|(item i of xs, i, xs)
        end repeat
    end tell
    return acc
end concatMap

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromTo

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- if_ :: Bool -> a -> a -> a
on if_(bool, x, y)
    if bool then
        x
    else
        y
    end if
end if_

-- intercalateS :: String -> [String] -> String
on intercalateS(sep, xs)
    set {dlm, my text item delimiters} to {my text item delimiters, sep}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end intercalateS

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

-- max :: Ord a => a -> a -> a
on max(x, y)
    if x > y then
        x
    else
        y
    end if
end max

-- maximumBy :: (a -> a -> Ordering) -> [a] -> a
on maximumBy(f, xs)
    set cmp to mReturn(f)
    script max
        on |λ|(a, b)
            if a is missing value or cmp's |λ|(a, b) < 0 then
                b
            else
                a
            end if
        end |λ|
    end script

    foldl(max, missing value, xs)
end maximumBy

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- pred :: Enum a => a -> a
on pred(x)
    (-1) + x
end pred

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary
-- assembly of a target length
-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate


-- reverse :: [a] -> [a]
on |reverse|(xs)
    if class of xs is text then
        (reverse of characters of xs) as text
    else
        reverse of xs
    end if
end |reverse|

-- If some of the rows are shorter than the following rows,
-- their elements are skipped:
-- transpose({{10,11},{20},{},{30,31,32}}) -> {{10, 20, 30}, {11, 31}, {32}}
-- transpose :: [[a]] -> [[a]]
on transpose(xxs)
    set intMax to |length|(maximumBy(comparing(my |length|), xxs))
    set gaps to replicate(intMax, {})
    script padded
        on |λ|(xs)
            set lng to |length|(xs)
            if lng < intMax then
                xs & items (lng + 1) thru -1 of gaps
            else
                xs
            end if
        end |λ|
    end script
    set rows to map(padded, xxs)

    script cols
        on |λ|(_, iCol)
            script cell
                on |λ|(row)
                    item iCol of row
                end |λ|
            end script
            concatMap(cell, rows)
        end |λ|
    end script
    map(cols, item 1 of rows)
end transpose

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- unwords :: [String] -> String
on unwords(xs)
    intercalateS(space, xs)
end unwords
```

{{Out}}
{| class="wikitable" style="text-align:center;width:12em;height:12em;table-layout:fixed;"
|-
| 0 || 1 || 2 || 3 || 4
|-
| 15 || 16 || 17 || 18 || 5
|-
| 14 || 23 || 24 || 19 || 6
|-
| 13 || 22 || 21 || 20 || 7
|-
| 12 || 11 || 10 || 9 || 8
|}


## AutoHotkey

{{trans|Python|}}
ahk forum: [http://www.autohotkey.com/forum/post-276718.html#276718 discussion]

```AutoHotkey
n := 5, dx := x := y := v := 1, dy := 0

Loop % n*n {
   a_%x%_%y% := v++
   nx := x+dx, ny := y+dy
   If (1 > nx || nx > n || 1 > ny || ny > n || a_%nx%_%ny%)
      t := dx, dx := -dy, dy := t
   x := x+dx, y := y+dy
}

Loop %n% {                      ; generate printout
   y := A_Index                 ; for each row
   Loop %n%                     ; and for each column
      s .= a_%A_Index%_%y% "`t" ; attach stored index
   s .= "`n"                    ; row is complete
}
MsgBox %s%                      ; show output
/*
---------------------------
1   2   3   4   5
16  17  18  19  6
15  24  25  20  7
14  23  22  21  8
13  12  11  10  9
---------------------------
*/
```



## AWK


```AWK

# syntax: GAWK -f SPIRAL_MATRIX.AWK [-v offset={0|1}] [size]
# converted from BBC BASIC
BEGIN {
# offset: "0" prints 0 to size^2-1 while "1" prints 1 to size^2
    offset = (offset == "") ? 0 : offset
    size = (ARGV[1] == "") ? 5 : ARGV[1]
    if (offset !~ /^[01]$/) { exit(1) }
    if (size !~ /^[0-9]+$/) { exit(1) }
    bot_col = bot_row = 0
    top_col = top_row = size - 1
    direction = col = row = 0
    for (i=0; i<=size*size-1; i++) { # build
      arr[col,row] = i + offset
      if (direction == 0) {
        if (col < top_col) { col++ }
        else { direction = 1 ; row++ ; bot_row++ }
      }
      else if (direction == 1) {
        if (row < top_row) { row++ }
        else { direction = 2 ; col-- ; top_col-- }
      }
      else if (direction == 2) {
        if (col > bot_col) { col-- }
        else { direction = 3 ; row-- ; top_row-- }
      }
      else if (direction == 3) {
        if (row > bot_row) { row-- }
        else { direction = 0 ; col++ ; bot_col++ }
      }
    }
    width = length(size ^ 2 - 1 + offset) + 1 # column width
    for (i=0; i<size; i++) { # print
      for (j=0; j<size; j++) {
        printf("%*d",width,arr[j,i])
      }
      printf("\n")
    }
    exit(0)
}

```

{{out}}

```txt

  0  1  2  3  4
 15 16 17 18  5
 14 23 24 19  6
 13 22 21 20  7
 12 11 10  9  8

```



## BBC BASIC


```bbcbasic
      N%=5
      @%=LENSTR$(N%*N%-1)+1
      BotCol%=0 : TopCol%=N%-1
      BotRow%=0 : TopRow%=N%-1
      DIM Matrix%(TopCol%,TopRow%)

      Dir%=0 : Col%=0 : Row%=0
      FOR I%=0 TO N%*N%-1
        Matrix%(Col%,Row%)=I%
        PRINT TAB(Col%*@%,Row%) I%
        CASE Dir% OF
          WHEN 0: IF Col% < TopCol% THEN Col%+=1 ELSE Dir%=1 : Row%+=1 : BotRow%+=1
          WHEN 1: IF Row% < TopRow% THEN Row%+=1 ELSE Dir%=2 : Col%-=1 : TopCol%-=1
          WHEN 2: IF Col% > BotCol% THEN Col%-=1 ELSE Dir%=3 : Row%-=1 : TopRow%-=1
          WHEN 3: IF Row% > BotRow% THEN Row%-=1 ELSE Dir%=0 : Col%+=1 : BotCol%+=1
        ENDCASE
      NEXT
      END
```



## C

Note: program produces a matrix starting from 1 instead of 0, because task says "natural numbers".

```c
#include <stdio.h>
#include <stdlib.h>

#define valid(i, j) 0 <= i && i < m && 0 <= j && j < n && !s[i][j]
int main(int c, char **v)
{
	int i, j, m = 0, n = 0;

	/* default size: 5 */
	if (c >= 2) m = atoi(v[1]);
	if (c >= 3) n = atoi(v[2]);
	if (m <= 0) m = 5;
	if (n <= 0) n = m;

	int **s = calloc(1, sizeof(int *) * m + sizeof(int) * m * n);
	s[0] = (int*)(s + m);
	for (i = 1; i < m; i++) s[i] = s[i - 1] + n;

	int dx = 1, dy = 0, val = 0, t;
	for (i = j = 0; valid(i, j); i += dy, j += dx ) {
		for (; valid(i, j); j += dx, i += dy)
			s[i][j] = ++val;

		j -= dx; i -= dy;
		t = dy; dy = dx; dx = -t;
	}

	for (t = 2; val /= 10; t++);

	for(i = 0; i < m; i++)
		for(j = 0; j < n || !putchar('\n'); j++)
			printf("%*d", t, s[i][j]);

	return 0;
}
```


Recursive method, width and height given on command line:

```c
#include <stdio.h>
#include <stdlib.h>

int spiral(int w, int h, int x, int y)
{
	return y ? w + spiral(h - 1, w, y - 1, w - x - 1) : x;
}

int main(int argc, char **argv)
{
	int w = atoi(argv[1]), h = atoi(argv[2]), i, j;
	for (i = 0; i < h; i++) {
		for (j = 0; j < w; j++)
			printf("%4d", spiral(w, h, j, i));
		putchar('\n');
	}
	return 0;
}
```



## C++


```cpp
#include <vector>
#include <memory>	// for auto_ptr
#include <cmath>	// for the ceil and log10 and floor functions
#include <iostream>
#include <iomanip>	// for the setw function

using namespace std;

typedef vector< int > IntRow;
typedef vector< IntRow > IntTable;

auto_ptr< IntTable > getSpiralArray( int dimension )
{
	auto_ptr< IntTable > spiralArrayPtr( new IntTable(
		dimension, IntRow( dimension ) ) );

	int numConcentricSquares = static_cast< int >( ceil(
		static_cast< double >( dimension ) / 2.0 ) );

	int j;
	int sideLen = dimension;
	int currNum = 0;

	for ( int i = 0; i < numConcentricSquares; i++ )
	{
		// do top side
		for ( j = 0; j < sideLen; j++ )
			( *spiralArrayPtr )[ i ][ i + j ] = currNum++;

		// do right side
		for ( j = 1; j < sideLen; j++ )
			( *spiralArrayPtr )[ i + j ][ dimension - 1 - i ] = currNum++;

		// do bottom side
		for ( j = sideLen - 2; j > -1; j-- )
			( *spiralArrayPtr )[ dimension - 1 - i ][ i + j ] = currNum++;

		// do left side
		for ( j = sideLen - 2; j > 0; j-- )
			( *spiralArrayPtr )[ i + j ][ i ] = currNum++;

		sideLen -= 2;
	}

	return spiralArrayPtr;
}

void printSpiralArray( const auto_ptr< IntTable >& spiralArrayPtr )
{
	size_t dimension = spiralArrayPtr->size();

	int fieldWidth = static_cast< int >( floor( log10(
		static_cast< double >( dimension * dimension - 1 ) ) ) ) + 2;

	size_t col;
	for ( size_t row = 0; row < dimension; row++ )
	{
		for ( col = 0; col < dimension; col++ )
			cout << setw( fieldWidth ) << ( *spiralArrayPtr )[ row ][ col ];
		cout << endl;
	}
}

int main()
{
	printSpiralArray( getSpiralArray( 5 ) );
}
```


C++ solution done properly:

```cpp
#include <vector>
#include <iostream>
using namespace std;
int main() {
	const int n = 5;
	const int dx[] = {0, 1, 0, -1}, dy[] = {1, 0, -1, 0};
	int x = 0, y = -1, c = 0;
	vector<vector<int>> m(n, vector<int>(n));
	for (int i = 0, im = 0; i < n + n - 1; ++i, im = i % 4)
		for (int j = 0, jlen = (n + n - i) / 2; j < jlen; ++j)
			m[x += dx[im]][y += dy[im]] = ++c;
	for (auto & r : m) {
		for (auto & v : r)
			cout << v << ' ';
		cout << endl;
	}
}
```


=={{header|C sharp|C#}}==
Solution based on the [[#J|J]] hints:

```csharp
public int[,] Spiral(int n) {
    int[,] result = new int[n, n];

    int pos = 0;
    int count = n;
    int value = -n;
    int sum = -1;

    do {
        value = -1 * value / n;
        for (int i = 0; i < count; i++) {
            sum += value;
            result[sum / n, sum % n] = pos++;
        }
        value *= n;
        count--;
        for (int i = 0; i < count; i++) {
            sum += value;
            result[sum / n, sum % n] = pos++;
        }
    } while (count > 0);

    return result;
}


// Method to print arrays, pads numbers so they line up in columns
public void PrintArray(int[,] array) {
    int n = (array.GetLength(0) * array.GetLength(1) - 1).ToString().Length + 1;

    for (int i = 0; i < array.GetLength(0); i++) {
        for (int j = 0; j < array.GetLength(1); j++) {
            Console.Write(array[i, j].ToString().PadLeft(n, ' '));
        }
        Console.WriteLine();
    }
}
```


Translated proper C++ solution:

```csharp


//generate spiral matrix for given N
int[,] CreateMatrix(int n){
    int[] dx = {0, 1, 0, -1}, dy = {1, 0, -1, 0};
    int x = 0, y = -1, c = 0;
    int[,] m = new int[n,n];
    for (int i = 0, im = 0; i < n + n - 1; ++i, im = i % 4)
        for (int j = 0, jlen = (n + n - i) / 2; j < jlen; ++j)
            m[x += dx[im],y += dy[im]] = ++c;
    return n;
}

//print aligned matrix
void Print(int[,] matrix) {
    var len = (int)Math.Ceiling(Math.Log10(m.GetLength(0) * m.GetLength(1)))+1;
    for(var y = 0; y<m.GetLength(1); y++){
        for(var x = 0; x<m.GetLength(0); x++){
            Console.Write(m[y, x].ToString().PadRight(len, ' '));
        }
        Console.WriteLine();
    }
}

```



### =Spiral Matrix without using an Array=



```csharp

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace spiralmat
{
    class spiral
    {
        public static int lev;
        int lev_lim, count, bk, cd, low, l, m;
        spiral()
        {
            lev = lev_lim = count = bk = cd = low = l = m = 0;
        }

        void level(int n1, int r, int c)
        {
            lev_lim = n1 % 2 == 0 ? n1 / 2 : (n1 + 1) / 2;
            if ((r <= lev_lim) && (c <= lev_lim))
                lev = Math.Min(r, c);
            else
            {
                bk = r > c ? (n1 + 1) - r : (n1 + 1) - c;
                low = Math.Min(r, c);
                if (low <= lev_lim)
                    cd = low;
                lev = cd < bk ? cd : bk;
            }
        }

        int func(int n2, int xo, int lo)
        {
            l = xo;
            m = lo;
            count = 0;
            level(n2, l, m);

            for (int ak = 1; ak < lev; ak++)
                count += 4 * (n2 - 1 - 2 * (ak - 1));
            return count;
        }

        public static void Main(string[] args)
        {
            spiral ob = new spiral();
            Console.WriteLine("Enter Order..");
            int n = int.Parse(Console.ReadLine());
            Console.WriteLine("The Matrix of {0} x {1} Order is=>\n", n, n);
            for (int i = 1; i <= n; i++)
            {
                for (int j = 1; j <= n; j++)
                    Console.Write("{0,3:D} ",
		                  ob.func(n, i, j)
				  + Convert.ToInt32(
				    ((j >= i) && (i == lev))
				      ? ((j - i) + 1)
				      : ((j == ((n + 1) - lev) && (i > lev) && (i <= j)))
				        ? (n - 2 * (lev - 1) + (i - 1) - (n - j))
					: ((i == ((n + 1) - lev) && (j < i)))
					  ? ((n - 2 * (lev - 1)) + ((n - 2 * (lev - 1)) - 1) + (i - j))
					  : ((j == lev) && (i > lev) && (i < ((n + 1) - lev)))
					    ? ((n - 2 * (lev - 1)) + ((n - 2 * (lev - 1)) - 1) + ((n - 2 * (lev - 1)) - 1) + (((n + 1) - lev) - i))
					    : 0));
                Console.WriteLine();
            }
            Console.ReadKey();
        }
    }
}

```

{{Out}}

```sh
INPUT:-

Enter order..
5

OUTPUT:-

The Matrix of 5 x 5 Order is=>

 1   2   3   4   5
16  17  18  19   6
15  24  25  20   7
14  23  22  21   8
13  12  11  10   9

INPUT:-

Enter order..
6

OUTPUT:-

The Matrix of 6 x 6 Order is=>

 1   2   3   4   5   6
20  21  22  23  24   7
19  32  33  34  25   8
18  31  36  35  26   9
17  30  29  28  27  10
16  15  14  13  12  11

```



## Clojure

Based on the [[#J|J]] hints (almost as incomprehensible, maybe)

```clojure
(defn spiral [n]
  (let [cyc (cycle [1 n -1 (- n)])]
    (->> (range (dec n) 0 -1)
         (mapcat #(repeat 2 %))
         (cons n)
         (mapcat #(repeat %2 %) cyc)
         (reductions +)
         (map vector (range 0 (* n n)))
         (sort-by second)
         (map first)))

(let [n 5]
  (clojure.pprint/cl-format
    true
    (str " ~{~<~%~," (* n 3) ":;~2d ~>~}~%")
    (spiral n)))
```

Recursive generation:
{{trans|Common Lisp}}

```clojure

(defn spiral-matrix [m n & [start]]
  (let [row (list (map #(+ start %) (range m)))]
    (if (= 1 n) row
      (concat row (map reverse
                       (apply map list
                              (spiral-matrix (dec n) m (+ start m))))))))

(defn spiral [n m] (spiral-matrix n m 1))

```



## CoffeeScript


```coffeescript

# Let's say you want to arrange the first N-squared natural numbers
# in a spiral, where you fill in the numbers clockwise, starting from
# the upper left corner.  This code computes the values for each x/y
# coordinate of the square.  (Of course, you could precompute the values
# iteratively, but what fun is that?)

spiral_value = (x, y, n) ->
  prior_legs =
    N: 0
    E: 1
    S: 2
    W: 3

  edge_run = (edge_offset) ->
    N: -> edge_offset.W - edge_offset.N
    E: -> edge_offset.N - edge_offset.E
    S: -> edge_offset.E - edge_offset.S
    W: -> edge_offset.S - edge_offset.W

  edge_offset =
    N: y
    E: n - 1 - x
    S: n - 1 - y
    W: x

  min_edge_offset = n
  for dir of edge_offset
    if edge_offset[dir] < min_edge_offset
      min_edge_offset = edge_offset[dir]
      border = dir

  inner_square_edge = n - 2 * min_edge_offset
  corner_offset = n * n - inner_square_edge * inner_square_edge
  corner_offset += prior_legs[border] * (inner_square_edge - 1)
  corner_offset + edge_run(edge_offset)[border]()

spiral_matrix = (n) ->
  # return a nested array expression
  for y in [0...n]
    for x in [0...n]
      spiral_value x, y, n

do ->
  for n in [6, 7]
    console.log "\n----Spiral n=#{n}"
    console.log spiral_matrix n

```

{{out}}
<lang>
> coffee spiral.coffee

----Spiral n=6
[ [ 0, 1, 2, 3, 4, 5 ],
  [ 19, 20, 21, 22, 23, 6 ],
  [ 18, 31, 32, 33, 24, 7 ],
  [ 17, 30, 35, 34, 25, 8 ],
  [ 16, 29, 28, 27, 26, 9 ],
  [ 15, 14, 13, 12, 11, 10 ] ]

----Spiral n=7
[ [ 0, 1, 2, 3, 4, 5, 6 ],
  [ 23, 24, 25, 26, 27, 28, 7 ],
  [ 22, 39, 40, 41, 42, 29, 8 ],
  [ 21, 38, 47, 48, 43, 30, 9 ],
  [ 20, 37, 46, 45, 44, 31, 10 ],
  [ 19, 36, 35, 34, 33, 32, 11 ],
  [ 18, 17, 16, 15, 14, 13, 12 ] ]

```




## Common Lisp

{{trans|Python}}

```lisp
(defun spiral (rows columns)
  (do ((N (* rows columns))
       (spiral (make-array (list rows columns) :initial-element nil))
       (dx 1) (dy 0) (x 0) (y 0)
       (i 0 (1+ i)))
      ((= i N) spiral)
    (setf (aref spiral y x) i)
    (let ((nx (+ x dx)) (ny (+ y dy)))
      (cond
       ((and (< -1 nx columns)
             (< -1 ny rows)
             (null (aref spiral ny nx)))
        (setf x nx
              y ny))
       (t (psetf dx (- dy)
                 dy dx)
          (setf x (+ x dx)
                y (+ y dy)))))))
```


```txt
> (pprint (spiral 6 6))

#2A((0 1 2 3 4 5)
    (19 20 21 22 23 6)
    (18 31 32 33 24 7)
    (17 30 35 34 25 8)
    (16 29 28 27 26 9)
    (15 14 13 12 11 10))

> (pprint (spiral 5 3))

#2A((0 1 2)
    (11 12 3)
    (10 13 4)
    (9 14 5)
    (8 7 6))
```

Recursive generation:

```lisp
(defun spiral (m n &optional (start 1))
  (let ((row (list (loop for x from 0 to (1- m) collect (+ x start)))))
    (if (= 1 n) row
      ;; first row, plus (n-1) x m spiral rotated 90 degrees
      (append row (map 'list #'reverse
		       (apply #'mapcar #'list
			      (spiral (1- n) m (+ start m))))))))

;; test
(loop for row in (spiral 4 3) do
      (format t "~{~4d~^~}~%" row))
```



## D


```d
void main() {
    import std.stdio;
    enum n = 5;
    int[n][n] M;
    int pos, side = n;

    foreach (immutable i; 0 .. n / 2 + n % 2) {
        foreach (immutable j; 0 .. side)
            M[i][i + j] = pos++;
        foreach (immutable j; 1 .. side)
            M[i + j][n - 1 - i] = pos++;
        foreach_reverse (immutable j; 0 .. side - 1)
            M[n - 1 - i][i + j] = pos++;
        foreach_reverse (immutable j; 1 .. side - 1)
            M[i + j][i] = pos++;
        side -= 2;
    }

    writefln("%(%(%2d %)\n%)", M);
}
```

{{out}}

```txt
 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8
```

Using a generator for any rectangular array:

```d
import std.stdio;

/// 2D spiral generator
const struct Spiral {
    int w, h;

    int opApply(int delegate(ref int, ref int, ref int) dg) {
        int idx, x, y, xy, dx = 1, dy;
        int[] subLen = [w, h-1];

        void turn() {
            auto t = -dy;
            dy = dx;
            dx = t;
            xy = 1 - xy;
        }

        void forward(int d = 1) {
            x += d * dx;
            y += d * dy;
            idx += d;
        }

        Bye:
        while (true) {
            if (subLen[xy] == 0)
                break;
            foreach (_; 0 .. subLen[xy]--)
                if (dg(idx, x, y))
                    break Bye;
                else
                    forward();
            forward(-1);
            turn();
            forward();
        }

        return 0;
    }
}

int[][] spiralMatrix(int w, int h) {
    auto m = new typeof(return)(h, w);
    foreach (value, x, y; Spiral(w, h))
        m[y][x] = value;
    return m;
}

void main() {
    foreach (r; spiralMatrix(9, 4))
        writefln("%(%2d %)", r);
}
```

{{out}}

```txt
 0  1  2  3  4  5  6  7  8
21 22 23 24 25 26 27 28  9
20 35 34 33 32 31 30 29 10
19 18 17 16 15 14 13 12 11
```


## DCL


```DCL
$ p1 = f$integer( p1 )
$ max = p1 * p1
$
$ i = 0
$ r = 1
$ rd = 0
$ c = 1
$ cd = 1
$ loop:
$  a'r'_'c' = i
$  nr = r + rd
$  nc = c + cd
$  if nr .eq. 0 .or. nc .eq. 0 .or. nr .gt. p1 .or. nc .gt. p1 .or. f$type( a'nr'_'nc' ) .nes. ""
$  then
$   gosub change_directions
$  endif
$  r = r + rd
$  c = c + cd
$  i = i + 1
$  if i .lt. max then $ goto loop
$ length = f$length( f$string( max - 1 ))
$ r = 1
$ loop2:
$  c = 1
$  output = ""
$  loop3:
$   output = output + f$fao( "!#UL ", length, a'r'_'c' )
$   c = c + 1
$   if c .le. p1 then $ goto loop3
$  write sys$output output
$  r = r + 1
$  if r .le. p1 then $ goto loop2
$ exit
$
$ change_directions:
$ if rd .eq. 0 .and cd .eq. 1
$ then
$  rd = 1
$  cd = 0
$ else
$  if rd .eq. 1 .and. cd .eq. 0
$  then
$   rd = 0
$   cd = -1
$  else
$   if rd .eq. 0 .and. cd .eq. -1
$   then
$    rd = -1
$    cd = 0
$   else
$    rd = 0
$    cd = 1
$   endif
$  endif
$ endif
$ return
```

{{out}}

```txt
$ @spiral_matrix 3
0 1 2
7 8 3
6 5 4
$ @spiral_matrix 5
 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8

...
```



## E

First, some quick data types to unclutter the actual algorithm.

{{E 2D utilities}}

```e
def spiral(size) {
  def array := makeFlex2DArray(size, size)
  var i := -1                   # Counter of numbers to fill
  var p := makeVector2(0, 0)    # "Position"
  var dp := makeVector2(1, 0)   # "Velocity"

  # If the cell we were to fill next (even after turning) is full, we're done.
  while (array[p.y(), p.x()] == null) {

    array[p.y(), p.x()] := (i += 1) # Fill cell
    def next := p + dp              # Look forward

    # If the cell we were to fill next is already full, then turn clockwise.
    # Gimmick: If we hit the edges of the array, by the modulo we wrap around
    # and see the already-filled cell on the opposite edge.
    if (array[next.y() %% size, next.x() %% size] != null) {
      dp := dp.clockwise()
    }

    # Move forward
    p += dp
  }

  return array
}
```

Example:

```e
? print(spiral(5))
 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule RC do
  def spiral_matrix(n) do
    wide = length(to_char_list(n*n-1))
    fmt = String.duplicate("~#{wide}w ", n) <> "~n"
    runs = Enum.flat_map(n..1, &[&1,&1]) |> tl
    delta = Stream.cycle([{0,1},{1,0},{0,-1},{-1,0}])
    running(Enum.zip(runs,delta),0,-1,[])
    |> Enum.with_index |> Enum.sort |>  Enum.chunk(n)
    |> Enum.each(fn row -> :io.format fmt, (for {_,i} <- row, do: i) end)
  end

  defp running([{run,{dx,dy}}|rest], x, y, track) do
    new_track = Enum.reduce(1..run, track, fn i,acc -> [{x+i*dx, y+i*dy} | acc] end)
    running(rest, x+run*dx, y+run*dy, new_track)
  end
  defp running([],_,_,track), do: track |> Enum.reverse
end

RC.spiral_matrix(5)
```


'''The other way'''

```elixir
defmodule RC do
  def spiral_matrix(n) do
    wide = String.length(to_string(n*n-1))
    fmt = String.duplicate("~#{wide}w ", n) <> "~n"
    right(n,n-1,0,[]) |> Enum.reverse |> Enum.with_index |> Enum.sort |> Enum.chunk(n) |>
      Enum.each(fn row ->
        :io.format fmt, (for {_,i} <- row, do: i)
      end)
  end

  def right(n, side, i, coordinates) do
    down(n, side, i, Enum.reduce(0..side, coordinates, fn j,acc -> [{i, i+j} | acc] end))
  end

  def down(_, 0, _, coordinates), do: coordinates
  def down(n, side, i, coordinates) do
    left(n, side-1, i, Enum.reduce(1..side, coordinates, fn j,acc -> [{i+j, n-1-i} | acc] end))
  end

  def left(n, side, i, coordinates) do
    up(n, side, i, Enum.reduce(side..0, coordinates, fn j,acc -> [{n-1-i, i+j} | acc] end))
  end

  def up(_, 0, _, coordinates), do: coordinates
  def up(n, side, i, coordinates) do
    right(n, side-1, i+1, Enum.reduce(side..1, coordinates, fn j,acc -> [{i+j, i} | acc] end))
  end
end

RC.spiral_matrix(5)
```


'''Another way'''

```elixir
defmodule RC do
  def spiral_matrix(n) do
    fmt = String.duplicate("~#{length(to_char_list(n*n-1))}w ", n) <> "~n"
    Enum.flat_map(n..1, &[&1, &1])
    |> tl
    |> Enum.reduce({{0,-1},{0,1},[]}, fn run,{{x,y},{dx,dy},acc} ->
         side = for i <- 1..run, do: {x+i*dx, y+i*dy}
         {{x+run*dx, y+run*dy}, {dy, -dx}, acc++side}
       end)
    |> elem(2)
    |> Enum.with_index
    |> Enum.sort
    |> Enum.map(fn {_,i} -> i end)
    |> Enum.chunk(n)
    |> Enum.each(fn row -> :io.format fmt, row end)
  end
end

RC.spiral_matrix(5)
```


{{out}}

```txt

 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8

```



## Euphoria


```Euphoria
function spiral(integer dimension)
    integer side, curr, curr2
    sequence s
    s = repeat(repeat(0,dimension),dimension)
    side = dimension
    curr = 0
    for i = 0 to floor(dimension/2) do
        for j = 1 to side-1 do
            s[i+1][i+j] = curr -- top
            curr2 = curr + side-1
            s[i+j][i+side] = curr2 -- right
            curr2 += side-1
            s[i+side][i+side-j+1] = curr2 -- bottom
            curr2 += side-1
            s[i+side-j+1][i+1] = curr2 -- left
            curr += 1
        end for
        curr = curr2 + 1
        side -= 2
    end for

    if remainder(dimension,2) then
        s[floor(dimension/2)+1][floor(dimension/2)+1] = curr
    end if

    return s
end function

? spiral(5)
```


{{out}}
 {
   {0,1,2,3,4},
   {15,16,17,18,5},
   {14,23,24,19,6},
   {13,22,21,20,7},
   {12,11,10,9,8}
 }


## Factor

This is an implementation of Joey Tuttle's method for computing a spiral directly as a list and then reshaping it into a matrix, as described in the [http://rosettacode.org/wiki/Spiral_matrix#J J entry]. To summarize, we construct a list with <code>n*n</code> elements by following some simple rules, then take its cumulative sum, and finally its inverse permutation (or grade in J parlance). This gives us a list which can be reshaped to the final matrix.

```factor
USING: arrays grouping io kernel math math.combinatorics
math.ranges math.statistics prettyprint sequences
sequences.repeating ;
IN: rosetta-code.spiral-matrix

: counts ( n -- seq ) 1 [a,b] 2 repeat rest ;

: vals ( n -- seq )
    [ 1 swap 2dup [ neg ] bi@ 4array ] [ 2 * 1 - cycle ] bi ;

: evJKT2 ( n -- seq )
    [ counts ] [ vals ] bi [ <array> ] 2map concat ;

: spiral ( n -- matrix )
    [ evJKT2 cum-sum inverse-permutation ] [ group ] bi ;

: spiral-demo ( -- ) 5 9 [ spiral simple-table. nl ] bi@ ;

MAIN: spiral-demo
```

{{out}}

```txt

0  1  2  3  4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10 9  8

0  1  2  3  4  5  6  7  8
31 32 33 34 35 36 37 38 9
30 55 56 57 58 59 60 39 10
29 54 71 72 73 74 61 40 11
28 53 70 79 80 75 62 41 12
27 52 69 78 77 76 63 42 13
26 51 68 67 66 65 64 43 14
25 50 49 48 47 46 45 44 15
24 23 22 21 20 19 18 17 16

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
PROGRAM SPIRAL

  IMPLICIT NONE

  INTEGER, PARAMETER :: size = 5
  INTEGER :: i, x = 0, y = 1, count = size, n = 0
  INTEGER :: array(size,size)

  DO i = 1, count
    x = x + 1
      array(x,y) = n
    n = n + 1
  END DO

  DO
    count = count  - 1
      DO i = 1, count
        y = y + 1
        array(x,y) = n
        n = n + 1
      END DO
      DO i = 1, count
        x = x - 1
        array(x,y) = n
        n = n + 1
      END DO
      IF (n > size*size-1) EXIT
      count = count - 1
      DO i = 1, count
        y = y - 1
        array(x,y) = n
        n = n + 1
      END DO
      DO i = 1, count
        x = x + 1
        array(x,y) = n
        n = n + 1
      END DO
      IF (n > size*size-1) EXIT
  END DO

  DO y = 1, size
    DO x = 1, size
      WRITE (*, "(I4)", ADVANCE="NO") array (x, y)
    END DO
    WRITE (*,*)
  END DO

END PROGRAM SPIRAL
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Enum Direction
  across
  down
  back
  up
End Enum

Dim As Integer n

Do
  Input "Enter size of matrix "; n
Loop Until n > 0

Dim spiral(1 To n, 1 To n) As Integer '' all zero by default

' enter the numbers 0 to (n^2 - 1) spirally in the matrix

Dim As Integer row = 1, col = 1, lowRow = 1, highRow = n, lowCol = 1, highCol = n
Dim d As Direction = across

For i As Integer = 0 To (n * n - 1)
  spiral(row, col) = i
  Select Case d
    Case across
      col += 1
      If col > highCol Then
        col = highCol
        row += 1
        d = down
      End if
    Case down
      row += 1
      If row > highRow Then
        row = highRow
        col -= 1
        d = back
      End if
    Case back
      col -= 1
      If col < lowCol Then
        col = lowCol
        row -= 1
        d = up
        lowRow += 1
      End If
    Case up
      row -= 1
      If row < lowRow Then
        row = lowRow
        col += 1
        d = across
        highRow -= 1
        lowCol += 1
        highCol -= 1
      End If
  End Select
Next

' print spiral matrix if n < 20
Print
If n < 20 Then
  For i As Integer = 1 To n
    For j As Integer = 1 To n
      Print Using "####"; spiral(i, j);
    Next j
    Print
  Next i
Else
  Print "Matrix is too big to display on 80 column console"
End If

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Enter size of matrix ? 5

   0   1   2   3   4
  15  16  17  18   5
  14  23  24  19   6
  13  22  21  20   7
  12  11  10   9   8

```


=={{header|F_Sharp|F#}}==
No fancy schmancy elegance here, just putting the numbers in the right place (though I commend the elegance)...

```fsharp
let Spiral n =
    let sq = Array2D.create n n 0                                   // Set up an output array
    let nCur = ref -1                                               // Current value being inserted
    let NextN() = nCur := (!nCur+1) ; !nCur                         // Inc current value and return new value
    let Frame inset =                                               // Create the "frame" at an offset from the outside
        let rangeF = [inset..(n - inset - 2)]                       // Range we use going forward
        let rangeR = [(n - inset - 1)..(-1)..(inset + 1)]           // Range we use going backward
        rangeF |> Seq.iter (fun i -> sq.[inset,i] <- NextN())       // Top of frame
        rangeF |> Seq.iter (fun i -> sq.[i,n-inset-1] <- NextN())   // Right side of frame
        rangeR |> Seq.iter (fun i -> sq.[n-inset-1,i] <- NextN())   // Bottom of frame
        rangeR |> Seq.iter (fun i -> sq.[i,inset] <- NextN())       // Left side of frame
    [0..(n/2 - 1)] |> Seq.iter (fun i -> Frame i)                   // Fill in all frames
    if n &&& 1 = 1 then sq.[n/2,n/2] <- n*n - 1                     // If n is odd, fill in the last single value
    sq                                                              // Return our output array
```



## GAP


```gap># Spiral matrix with numbers 1 .. n<sup>2</sup
, more natural in GAP
SpiralMatrix := function(n)
  local i, j, k, di, dj, p, vi, vj, imin, imax, jmin, jmax;
  a := NullMat(n, n);
  vi := [ 1, 0, -1, 0 ];
  vj := [ 0, 1, 0, -1 ];
  imin := 0;
  imax := n;
  jmin := 1;
  jmax := n + 1;
  p := 1;
  di := vi[p];
  dj := vj[p];
  i := 1;
  j := 1;
  for k in [1 .. n*n] do
    a[j][i] := k;
    i := i + di;
    j := j + dj;
    if i < imin or i > imax or j < jmin or j > jmax then
      i := i - di;
      j := j - dj;
      p := RemInt(p, 4) + 1;
      di := vi[p];
      dj := vj[p];
      i := i + di;
      j := j + dj;
      if p = 1 then
        imax := imax - 1;
      elif p = 2 then
        jmax := jmax - 1;
      elif p = 3 then
        imin := imin + 1;
      else
        jmin := jmin + 1;
      fi;
    fi;
  od;
  return a;
end;

PrintArray(SpiralMatrix(5));
# [ [   1,   2,   3,   4,   5 ],
#   [  16,  17,  18,  19,   6 ],
#   [  15,  24,  25,  20,   7 ],
#   [  14,  23,  22,  21,   8 ],
#   [  13,  12,  11,  10,   9 ] ]
```



## Go


```go
package main

import (
    "fmt"
    "strconv"
)

var n = 5

func main() {
    if n < 1 {
        return
    }
    top, left, bottom, right := 0, 0, n-1, n-1
    sz := n * n
    a := make([]int, sz)
    i := 0
    for left < right {
        // work right, along top
        for c := left; c <= right; c++ {
            a[top*n+c] = i
            i++
        }
        top++
        // work down right side
        for r := top; r <= bottom; r++ {
            a[r*n+right] = i
            i++
        }
        right--
        if top == bottom {
            break
        }
        // work left, along bottom
        for c := right; c >= left; c-- {
            a[bottom*n+c] = i
            i++
        }
        bottom--
        // work up left side
        for r := bottom; r >= top; r-- {
            a[r*n+left] = i
            i++
        }
        left++
    }
    // center (last) element
    a[top*n+left] = i

    // print
    w := len(strconv.Itoa(n*n - 1))
    for i, e := range a {
        fmt.Printf("%*d ", w, e)
        if i%n == n-1 {
            fmt.Println("")
        }
    }
}
```



## Groovy

Naive "path-walking" solution:

```groovy
enum Direction {
    East([0,1]), South([1,0]), West([0,-1]), North([-1,0]);
    private static _n
    private final stepDelta
    private bound

    private Direction(delta) {
        stepDelta = delta
    }

    public static setN(int n) {
        Direction._n = n
        North.bound = 0
        South.bound = n-1
        West.bound = 0
        East.bound = n-1
    }

    public List move(i, j) {
        def dir = this
        def newIJDir = [[i,j],stepDelta].transpose().collect { it.sum() } + dir
        if (((North.bound)..(South.bound)).contains(newIJDir[0])
            && ((West.bound)..(East.bound)).contains(newIJDir[1])) {
            newIJDir
        } else {
            (++dir).move(i, j)
        }
    }

    public Object next() {
        switch (this) {
            case North: West.bound++; return East;
            case East: North.bound++; return South;
            case South: East.bound--; return West;
            case West: South.bound--; return North;
        }
    }
}

def spiralMatrix = { n ->
    if (n < 1) return []
    def M = (0..<n).collect { [0]*n }
    def i = 0
    def j = 0
    Direction.n = n
    def dir = Direction.East
    (0..<(n**2)).each { k ->
        M[i][j] = k
        (i,j,dir) = (k < (n**2 - 1)) \
            ? dir.move(i,j) \
            : [i,j,dir]
    }
    M
}
```

Test:

```groovy
(1..10).each { n ->
    spiralMatrix(n).each { row ->
        row.each { printf "%5d", it }
        println()
    }
    println ()
}
```

{{out}}
<pre style="height:30ex;overflow:scroll;">    0

    0    1
    3    2

    0    1    2
    7    8    3
    6    5    4

    0    1    2    3
   11   12   13    4
   10   15   14    5
    9    8    7    6

    0    1    2    3    4
   15   16   17   18    5
   14   23   24   19    6
   13   22   21   20    7
   12   11   10    9    8

    0    1    2    3    4    5
   19   20   21   22   23    6
   18   31   32   33   24    7
   17   30   35   34   25    8
   16   29   28   27   26    9
   15   14   13   12   11   10

    0    1    2    3    4    5    6
   23   24   25   26   27   28    7
   22   39   40   41   42   29    8
   21   38   47   48   43   30    9
   20   37   46   45   44   31   10
   19   36   35   34   33   32   11
   18   17   16   15   14   13   12

    0    1    2    3    4    5    6    7
   27   28   29   30   31   32   33    8
   26   47   48   49   50   51   34    9
   25   46   59   60   61   52   35   10
   24   45   58   63   62   53   36   11
   23   44   57   56   55   54   37   12
   22   43   42   41   40   39   38   13
   21   20   19   18   17   16   15   14

    0    1    2    3    4    5    6    7    8
   31   32   33   34   35   36   37   38    9
   30   55   56   57   58   59   60   39   10
   29   54   71   72   73   74   61   40   11
   28   53   70   79   80   75   62   41   12
   27   52   69   78   77   76   63   42   13
   26   51   68   67   66   65   64   43   14
   25   50   49   48   47   46   45   44   15
   24   23   22   21   20   19   18   17   16

    0    1    2    3    4    5    6    7    8    9
   35   36   37   38   39   40   41   42   43   10
   34   63   64   65   66   67   68   69   44   11
   33   62   83   84   85   86   87   70   45   12
   32   61   82   95   96   97   88   71   46   13
   31   60   81   94   99   98   89   72   47   14
   30   59   80   93   92   91   90   73   48   15
   29   58   79   78   77   76   75   74   49   16
   28   57   56   55   54   53   52   51   50   17
   27   26   25   24   23   22   21   20   19   18
```



## Haskell

Solution based on the [[#J|J]] hints:

```haskell
import Data.List
import Control.Monad
grade xs = map snd. sort $ zip xs [0..]
values n = cycle [1,n,-1,-n]
counts n = (n:).concatMap (ap (:) return)  $ [n-1,n-2..1]
reshape n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))
spiral n = reshape n . grade. scanl1 (+). concat $ zipWith replicate (counts n) (values n)
displayRow = putStrLn . intercalate " " . map show
main = mapM displayRow $ spiral 5
```


An alternative, point-free solution based on the same J source.


```haskell
import Data.List
import Control.Applicative
counts = tail . reverse . concat . map (replicate 2) . enumFromTo 1
values = cycle . ((++) <$> map id <*> map negate) . (1 :) . (: [])
grade = map snd . sort . flip zip [0..]
copies = grade . scanl1 (+) . concat . map (uncurry replicate) . (zip <$> counts <*> values)
parts = (<*>) take $ (.) <$> (map . take) <*> (iterate . drop) <*> copies
disp = (>> return ()) . mapM (putStrLn . intercalate " " . map show) . parts
main = disp 5
```


Another alternative:

```haskell
import Data.List (transpose)
import Text.Printf (printf)

-- spiral is the first row plus a smaller spiral rotated 90 deg
spiral 0 _ _ = [[]]
spiral h w s = [s .. s+w-1] : rot90 (spiral w (h-1) (s+w))
	where rot90 = (map reverse).transpose

-- this is sort of hideous, someone may want to fix it
main = mapM_ (\row->mapM_ ((printf "%4d").toInteger) row >> putStrLn "") (spiral 10 9 1)
```



Or less ambitiously,
{{Trans|AppleScript}}

```Haskell
import Data.List (intercalate, transpose)
import Control.Monad (join)

spiral :: Int -> [[Int]]
spiral n = go n n 0
  where
    go rows cols x
      | 0 < rows =
        [x .. pred cols + x] :
        fmap reverse (transpose $ go cols (pred rows) (x + cols))
      | otherwise = [[]]


-- TABLE FORMATTING ----------------------------------------

wikiTable :: Show a => [[a]] -> String
wikiTable =
  join .
  ("{| class=\"wikitable\" style=\"text-align: right;" :) .
  ("width:12em;height:12em;table-layout:fixed;\"\n|-\n" :) .
  return .
  (++ "\n|}") .
  intercalate "\n|-\n" .
  fmap (('|' :) . (' ' :) . intercalate " || " . fmap show)
```

{{Out}}
{| class="wikitable" style="text-align: right;width:12em;height:12em;table-layout:fixed;"|-
| 0 || 1 || 2 || 3 || 4
|-
| 15 || 16 || 17 || 18 || 5
|-
| 14 || 23 || 24 || 19 || 6
|-
| 13 || 22 || 21 || 20 || 7
|-
| 12 || 11 || 10 || 9 || 8
|}

=={{header|Icon}} and {{header|Unicon}}==
At first I looked at keeping the filling of the matrix on track using /M[r,c] which fails when out of bounds or if the cell is null, but then I noticed the progression of the row and column increments from corner to corner reminded me of sines and cosines.  I'm not sure if the use of a trigonometric function counts as elegance, perversity, or both.  The generator could be easily modified to start at an arbitrary corner.  Or count down to produce and evolute.

```Icon
procedure main(A)        # spiral matrix
N := 0 < integer(\A[1]|5)           # N=1... (dfeault 5)
WriteMatrix(SpiralMatrix(N))
end

procedure WriteMatrix(M)             #: write the matrix
every x := M[r := 1 to *M, c := 1 to *M[r]] do
   writes(right(\x|"-", 3), if c = *M[r] then "\n" else "")
return
end

procedure SpiralMatrix(N)            #: create spiral matrix
every (!(M := list(N))):= list(N)    # build empty matrix NxN
                                     # setup before starting first turn
corner := 0                          # . corner we're at
i := -1                              # . cell contents
r:= 1 ; c :=0                        # . row & col
cincr := integer(sin(0))             # . column incr

until i > N^2 do {
   rincr := cincr                              # row incr follows col
   cincr := integer(sin(&pi/2*(corner+:=1)))   # col incr at each corner
   if (run := N-corner/2)  = 0 then break      # shorten run to 0 at U/R & L/L
   every run to 1 by -1 do
      M[r +:= rincr,c +:= cincr] := i +:= 1    # move, count, and fill
   }
return M
end
```


{{out}}

```txt
  0  1  2  3  4
 15 16 17 18  5
 14 23 24 19  6
 13 22 21 20  7
 12 11 10  9  8
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "SpiralMa.bas"
110 TEXT 80
120 INPUT PROMPT "Enter size of matrix (max. 10): ":N
130 NUMERIC A(1 TO N,1 TO N)
140 CALL INIT(A)
150 CALL WRITE(A)
160 DEF INIT(REF T)
170   LET BCOL,BROW,COL,ROW=1:LET TCOL,TROW=N:LET DIR=0
180   FOR I=0 TO N^2-1
190     LET T(COL,ROW)=I
200     SELECT CASE DIR
210     CASE 0
220       IF ROW<TROW THEN
230         LET ROW=ROW+1
240       ELSE
250         LET DIR=1:LET COL=COL+1:LET BCOL=BCOL+1
260       END IF
270     CASE 1
280       IF COL<TCOL THEN
290         LET COL=COL+1
300       ELSE
310         LET DIR=2:LET ROW=ROW-1:LET TROW=TROW-1
320       END IF
330     CASE 2
340       IF ROW>BROW THEN
350         LET ROW=ROW-1
360       ELSE
370         LET DIR=3:LET COL=COL-1:LET TCOL=TCOL-1
380       END IF
390     CASE 3
400       IF COL>BCOL THEN
410         LET COL=COL-1
420       ELSE
430         LET DIR=0:LET ROW=ROW+1:LET BROW=BROW+1
440       END IF
450     END SELECT
460   NEXT
470 END DEF
480 DEF WRITE(REF T)
490   FOR I=LBOUND(T,1) TO UBOUND(T,1)
500     FOR J=LBOUND(T,2) TO UBOUND(T,2)
510       PRINT USING " ##":T(I,J);
520     NEXT
530     PRINT
540   NEXT
550 END DEF
```



## J

This function is the result of
some [http://www.jsoftware.com/papers/play132.htm beautiful insights]:

```j
spiral =: ,~ $ [: /: }.@(2 # >:@i.@-) +/\@# <:@+: $ (, -)@(1&,)

   spiral 5
 0  1  2  3 4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10  9 8
```

Would you like [[Talk:Spiral#J|some hints]] that will allow you to reimplement it in another language?

These inward spiralling arrays are known as "involutes"; we can also generate outward-spiraling "evolutes", and we can start or end the spiral at any corner, and go in either direction (clockwise or counterclockwise).  See the first link (to JSoftware.com).


## Java

{{trans|C++}}
{{works with|Java|1.5+}}

```java5
public class Blah {

  public static void main(String[] args) {
    print2dArray(getSpiralArray(5));
  }

  public static int[][] getSpiralArray(int dimension) {
    int[][] spiralArray = new int[dimension][dimension];

    int numConcentricSquares = (int) Math.ceil((dimension) / 2.0);

    int j;
    int sideLen = dimension;
    int currNum = 0;

    for (int i = 0; i < numConcentricSquares; i++) {
      // do top side
      for (j = 0; j < sideLen; j++) {
        spiralArray[i][i + j] = currNum++;
      }

      // do right side
      for (j = 1; j < sideLen; j++) {
        spiralArray[i + j][dimension - 1 - i] = currNum++;
      }

      // do bottom side
      for (j = sideLen - 2; j > -1; j--) {
        spiralArray[dimension - 1 - i][i + j] = currNum++;
      }

      // do left side
      for (j = sideLen - 2; j > 0; j--) {
        spiralArray[i + j][i] = currNum++;
      }

      sideLen -= 2;
    }

    return spiralArray;
  }

  public static void print2dArray(int[][] array) {
    for (int[] row : array) {
      for (int elem : row) {
        System.out.printf("%3d", elem);
      }
      System.out.println();
    }
  }
}
```

{{out}}

```txt
  0  1  2  3  4
 15 16 17 18  5
 14 23 24 19  6
 13 22 21 20  7
 12 11 10  9  8
```



## JavaScript



### Imperative



```javascript
spiralArray = function (edge) {
    var arr = Array(edge),
        x = 0, y = edge,
        total = edge * edge--,
        dx = 1, dy = 0,
        i = 0, j = 0;
    while (y) arr[--y] = [];
    while (i < total) {
        arr[y][x] = i++;
        x += dx; y += dy;
        if (++j == edge) {
            if (dy < 0) {x++; y++; edge -= 2}
            j = dx; dx = -dy; dy = j; j = 0;
       }
    }
    return arr;
}

// T E S T:
arr = spiralArray(edge = 5);
for (y= 0; y < edge; y++) console.log(arr[y].join(" "));

```

{{out}}

```txt

0 1 2 3 4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10 9 8
```



### Functional



### =ES5=


Translating one of the Haskell versions:


```JavaScript
(function (n) {

  // Spiral: the first row plus a smaller spiral rotated 90 degrees clockwise
  function spiral(lngRows, lngCols, nStart) {
    return lngRows ? [range(nStart, (nStart + lngCols) - 1)].concat(
      transpose(
        spiral(lngCols, lngRows - 1, nStart + lngCols)
      ).map(reverse)
    ) : [
      []
    ];
  }

  // rows and columns transposed (for 90 degree rotation)
  function transpose(lst) {
    return lst.length > 1 ? lst[0].map(function (_, col) {
      return lst.map(function (row) {
        return row[col];
      });
    }) : lst;
  }

  // elements in reverse order (for 90 degree rotation)
  function reverse(lst) {
    return lst.length > 1 ? lst.reduceRight(function (acc, x) {
      return acc.concat(x);
    }, []) : lst;
  }

  // [m..n]
  function range(m, n) {
    return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
      return m + i;
    });
  }

  // TESTING

  var lstSpiral = spiral(n, n, 0);


  // OUTPUT FORMATTING - JSON and wikiTable
  function wikiTable(lstRows, blnHeaderRow, strStyle) {
    return '{| class="wikitable" ' + (
      strStyle ? 'style="' + strStyle + '"' : ''
    ) + lstRows.map(function (lstRow, iRow) {
      var strDelim = ((blnHeaderRow && !iRow) ? '!' : '|');

      return '\n|-\n' + strDelim + ' ' + lstRow.map(function (v) {
        return typeof v === 'undefined' ? ' ' : v;
      }).join(' ' + strDelim + strDelim + ' ');
    }).join('') + '\n|}';
  }

  return [
    wikiTable(

      lstSpiral,

      false,
      'text-align:center;width:12em;height:12em;table-layout:fixed;'
    ),

    JSON.stringify(lstSpiral)
  ].join('\n\n');

})(5);
```


Output:

{| class="wikitable" style="text-align:center;width:12em;height:12em;table-layout:fixed;"
|-
| 0 || 1 || 2 || 3 || 4
|-
| 15 || 16 || 17 || 18 || 5
|-
| 14 || 23 || 24 || 19 || 6
|-
| 13 || 22 || 21 || 20 || 7
|-
| 12 || 11 || 10 || 9 || 8
|}


```JavaScript
[[0,1,2,3,4],[15,16,17,18,5],[14,23,24,19,6],[13,22,21,20,7],[12,11,10,9,8]]
```




### =ES6=

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // main :: () -> String
    const main = () =>
        unlines(
            map(unwords, spiral(5))
        );

    // spiral :: Int -> [[Int]]
    const spiral = n => {
        const go = (rows, cols, start) =>
            0 < rows ? [
                enumFromTo(start, start + pred(cols)),
                ...map(
                    reverse,
                    transpose(
                        go(
                            cols,
                            pred(rows),
                            start + cols
                        )
                    )
                )
            ] : [
                []
            ];
        return go(n, n, 0);
    };

    // GENERIC FUNCTIONS ----------------------------------

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs.map(f))
        })() : [];

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        m <= n ? iterateUntil(
            x => n <= x,
            x => 1 + x,
            m
        ) : [];

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    //  Ordering: (LT|EQ|GT):
    //  GT: 1 (or other positive n)
    //    EQ: 0
    //  LT: -1 (or other negative n)

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        0 < xs.length ? (
            xs.slice(1)
            .reduce((a, x) => 0 < f(x, a) ? x : a, xs[0])
        ) : undefined;


    // pred :: Enum a => a -> a
    const pred = x => x - 1;

    // reverse :: [a] -> [a]
    const reverse = xs =>
        'string' !== typeof xs ? (
            xs.slice(0).reverse()
        ) : xs.split('').reverse().join('');

    // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);

    // transpose :: [[a]] -> [[a]]
    const transpose = tbl => {
        const
            gaps = replicate(
                length(maximumBy(comparing(length), tbl)), []
            ),
            rows = map(xs => xs.concat(gaps.slice(xs.length)), tbl);
        return map(
            (_, col) => concatMap(row => [row[col]], rows),
            rows[0]
        );
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
0 1 2 3 4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10 9 8
```



## jq

The strategy employed here is to start at [0,0] and move to the right ([0,1] == same row, next column)
until we reach a boundary or a populated cell; then turn right, and proceed as before.

Initially fill the matrix with "false" so we can easily distinguish between unvisited cells (false) and non-existent cells (null).

'''Infrastructure''':

```jq
# Create an m x n matrix
 def matrix(m; n; init):
   if m == 0 then []
   elif m == 1 then [range(0;n)] | map(init)
   elif m > 0 then
     matrix(1;n;init) as $row
     | [range(0;m)] | map( $row )
   else error("matrix\(m);_;_) invalid")
   end ;

# Print a matrix neatly, each cell occupying n spaces
def neatly(n):
  def right: tostring | ( " " * (n-length) + .);
  . as $in
  | length as $length
  | reduce range (0;$length) as $i
      (""; . + reduce range(0;$length) as $j
      (""; "\(.)\($in[$i][$j] | right )" ) + "\n" ) ;

def right:
  if   . == [1,  0] then [ 0, -1]
  elif . == [0, -1] then [-1,  0]
  elif . == [-1, 0] then [ 0,  1]
  elif . == [0,  1] then [ 1,  0]
  else error("invalid direction: \(.)")
  end;
```

'''Create a spiral n by n matrix'''

```jq
def spiral(n):
  # we just placed m at i,j, and we are moving in the direction d
  def _next(i; j; m; d):
    if m == (n*n) - 1 then .
    elif .[i+d[0]][j+d[1]] == false
      then .[i+d[0]][j+d[1]]   = m+1 | _next(i+d[0]; j+d[1]; m+1; d)
    else (d|right) as $d
       |   .[i+$d[0]][j+$d[1]] = m+1 | _next(i+$d[0]; j+$d[1]; m+1; $d)
    end;

  matrix(n;n;false) | .[0][0] = 0 | _next(0;0;0; [0,1]) ;

# Example
spiral(5) | neatly(3)
```

{{Out}}
 $ jq -n -r -f spiral.jq
   0  1  2  3  4
  15 16 17 18  5
  14 23 24 19  6
  13 22 21 20  7
  12 11 10  9  8


## Julia

Define an iterator that marches through matrix indices in the spiral pattern, which makes it easy to generate spiral matrices and related objects.  Note that Julia uses column major ordering of matrices and that Julia allows multi-dimensional arrays to be addressed by scalar index as well as by subscripts.

'''Spiral Matrix Iterator'''

```Julia

immutable Spiral
    m::Int
    n::Int
    cmax::Int
    dir::Array{Array{Int,1},1}
    bdelta::Array{Array{Int,1},1}
end

function Spiral(m::Int, n::Int)
    cmax = m*n
    dir = Array{Int,1}[[0,1], [1,0], [0,-1], [-1,0]]
    bdelta = Array{Int,1}[[0,0,0,1], [-1,0,0,0],
                          [0,-1,0,0], [0,0,1,0]]
    Spiral(m, n, cmax, dir, bdelta)
end

function spiral(m::Int, n::Int)
    0<m&&0<n || error("The matrix dimensions must be positive.")
    Spiral(m, n)
end
spiral(n::Int) = spiral(n, n)

type SpState
    cnt::Int
    dirdex::Int
    cell::Array{Int,1}
    bounds::Array{Int,1}
end

Base.length(sp::Spiral) = sp.cmax
Base.start(sp::Spiral) = SpState(1, 1, [1,1], [sp.n,sp.m,1,1])
Base.done(sp::Spiral, sps::SpState) = sps.cnt > sp.cmax

function Base.next(sp::Spiral, sps::SpState)
    s = sub2ind((sp.m, sp.n), sps.cell[1], sps.cell[2])
    if sps.cell[rem1(sps.dirdex+1, 2)] == sps.bounds[sps.dirdex]
        sps.bounds += sp.bdelta[sps.dirdex]
        sps.dirdex = rem1(sps.dirdex+1, 4)
    end
    sps.cell += sp.dir[sps.dirdex]
    sps.cnt += 1
    return (s, sps)
end

```


'''Helper Functions'''

```Julia

using Formatting

function width{T<:Integer}(n::T)
    w = ndigits(n)
    n < 0 || return w
    return w + 1
end

function pretty{T<:Integer}(a::Array{T,2}, indent::Int=4)
    lo, hi = extrema(a)
    w = max(width(lo), width(hi))
    id = " "^indent
    fe = FormatExpr(@sprintf(" {:%dd}", w))
    s = id
    nrow = size(a)[1]
    for i in 1:nrow
        for j in a[i,:]
            s *= format(fe, j)
        end
        i != nrow || continue
        s *= "\n"*id
    end
    return s
end

```


'''Main'''

```Julia

n = 5
println("The n = ", n, " spiral matrix:")
a = zeros(Int, (n, n))
for (i, s) in enumerate(spiral(n))
    a[s] = i-1
end
println(pretty(a))

m = 3
println()
println("Generalize to a non-square matrix (", m, "x", n, "):")
a = zeros(Int, (m, n))
for (i, s) in enumerate(spiral(m, n))
    a[s] = i-1
end
println(pretty(a))

p = primes(10^3)
n = 7
println()
println("An n = ", n, " prime spiral matrix:")
a = zeros(Int, (n, n))
for (i, s) in enumerate(spiral(n))
    a[s] = p[i]
end
println(pretty(a))

```


{{out}}

```txt

The n = 5 spiral matrix:
      0  1  2  3  4
     15 16 17 18  5
     14 23 24 19  6
     13 22 21 20  7
     12 11 10  9  8

Generalize to a non-square matrix (3x5):
      0  1  2  3  4
     11 12 13 14  5
     10  9  8  7  6

An n = 7 prime spiral matrix:
       2   3   5   7  11  13  17
      89  97 101 103 107 109  19
      83 173 179 181 191 113  23
      79 167 223 227 193 127  29
      73 163 211 199 197 131  31
      71 157 151 149 139 137  37
      67  61  59  53  47  43  41

```



## Kotlin

{{trans|C#}}

```scala
// version 1.1.3

typealias Vector = IntArray
typealias Matrix = Array<Vector>

fun spiralMatrix(n: Int): Matrix {
    val result = Matrix(n) { Vector(n) }
    var pos = 0
    var count = n
    var value = -n
    var sum = -1
    do {
        value = -value / n
        for (i in 0 until count) {
            sum += value
            result[sum / n][sum % n] = pos++
        }
        value *= n
        count--
        for (i in 0 until count) {
            sum += value
            result[sum / n][sum % n] = pos++
        }
    }
    while (count > 0)
    return result
}

fun printMatrix(m: Matrix) {
    for (i in 0 until m.size) {
        for (j in 0 until m.size) print("%2d ".format(m[i][j]))
        println()
    }
    println()
}

fun main(args: Array<String>) {
    printMatrix(spiralMatrix(5))
    printMatrix(spiralMatrix(10))
}
```


{{out}}

```txt

 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8

 0  1  2  3  4  5  6  7  8  9
35 36 37 38 39 40 41 42 43 10
34 63 64 65 66 67 68 69 44 11
33 62 83 84 85 86 87 70 45 12
32 61 82 95 96 97 88 71 46 13
31 60 81 94 99 98 89 72 47 14
30 59 80 93 92 91 90 73 48 15
29 58 79 78 77 76 75 74 49 16
28 57 56 55 54 53 52 51 50 17
27 26 25 24 23 22 21 20 19 18

```



## Liberty BASIC

Extended to include automatic scaling of the display scale and font. See [http://www.diga.me.uk/spiralM5.gif spiralM5]

```lb
nomainwin

UpperLeftX   = 50
UpperLeftY   = 50
WindowWidth  =900
WindowHeight =930

statictext #w.st, "", 10, 850, 870, 40

open "Spiral matrix" for graphics_nsb_nf as #w

#w "trapclose [quit]"
#w "backcolor darkblue; color cyan; fill darkblue"

for N =2 to 50

    #w.st "!font courier_new "; int(  60 /N); " bold"
    #w    "down; font arial ";  int( 240 /N); " bold"

    g$ ="ruld"                                  '   direction sequence
    if N/2 =int( N/2) then pg =2 else pg =0     '   pointer to current direction
                                                '   last move is left or right depending on N even/odd
    d$ =""

    for i =1 to N -1                            '   calculate direction to move
        d$ =nChar$( i, mid$( g$, pg +1, 1)) +d$
        pg =( pg +1) mod 4
        d$ =nChar$( i, mid$( g$, pg +1, 1)) +d$
        pg =( pg +1) mod 4
    next i

    d$ =nChar$( N -1, "r") +d$                  '   first row

    #w.st "   N ="; N; "  "; d$

    xp =60 +250 /N
    yp =80 +250 /N

    stp =int( 750 /N)

    for i =0 to N^2 -1
        dir$ =mid$( d$, i, 1)
        select case dir$
            case "r"
                xp =xp +stp
            case "d"
                yp =yp +stp
            case "l"
                xp =xp -stp
            case "u"
                yp =yp -stp
        end select

        #w "place "; xp; " "; yp
        #w "\"; i
    next i

    timer 3000, [on]
    wait
  [on]
    timer 0
    #w "cls"
    scan
next N

wait

function nChar$( n, i$)
    for i =1 to n
        nChar$ =nChar$ +i$
    next i
end function

[quit]
close #w
end
```



## Lua


```lua
av, sn = math.abs, function(s) return s~=0 and s/av(s) or 0 end
function sindex(y, x) -- returns the value at (x, y) in a spiral that starts at 1 and goes outwards
  if y == -x and y >= x then return (2*y+1)^2 end
  local l = math.max(av(y), av(x))
  return (2*l-1)^2+4*l+2*l*sn(x+y)+sn(y^2-x^2)*(l-(av(y)==l and sn(y)*x or sn(x)*y)) -- OH GOD WHAT
end

function spiralt(side)
  local ret, start, stop = {}, math.floor((-side+1)/2), math.floor((side-1)/2)
  for i = 1, side do
    ret[i] = {}
    for j = 1, side do
      ret[i][j] = side^2 - sindex(stop - i + 1,start + j - 1) --moves the coordinates so (0,0) is at the center of the spiral
    end
  end
  return ret
end

for i,v in ipairs(spiralt(8)) do for j, u in ipairs(v) do io.write(u .. "   ") end print() end
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
We split the task up in 2 functions, one that adds a 'ring' around a present matrix. And a function that adds rings to a 'core':

```Mathematica
AddSquareRing[x_List/;Equal@@Dimensions[x] && Length[Dimensions[x]]==2]:=Module[{new=x,size,smallest},
 size=Length[x];
 smallest=x[[1,1]];
 Do[
  new[[i]]=Prepend[new[[i]],smallest-i];
  new[[i]]=Append[new[[i]],smallest-3 size+i-3]
 ,{i,size}];
 PrependTo[new,Range[smallest-3size-3-size-1,smallest-3size-3]];
 AppendTo[new,Range[smallest-size-1,smallest-size-size-2,-1]];
 new
]
MakeSquareSpiral[size_Integer/;size>0]:=Module[{largest,start,times},
 start=size^2+If[Mod[size,2]==0,{{-4,-3},{-1,-2}},{{-1}}];
 times=If[Mod[size,2]==0,size/2-1,(size-1)/2];
 Nest[AddSquareRing,start,times]
]
```

Examples:

```Mathematica
MakeSquareSpiral[2] // MatrixForm
MakeSquareSpiral[7] // MatrixForm
```

gives back:
<math>
\left(
\begin{array}{cc}
 0 & 1 \\
 3 & 2
\end{array}
\right)
</math>

<math>
\left(
\begin{array}{ccccccc}
 0 & 1 & 2 & 3 & 4 & 5 & 6 \\
 23 & 24 & 25 & 26 & 27 & 28 & 7 \\
 22 & 39 & 40 & 41 & 42 & 29 & 8 \\
 21 & 38 & 47 & 48 & 43 & 30 & 9 \\
 20 & 37 & 46 & 45 & 44 & 31 & 10 \\
 19 & 36 & 35 & 34 & 33 & 32 & 11 \\
 18 & 17 & 16 & 15 & 14 & 13 & 12
\end{array}
\right)
</math>


## MATLAB

There already exists a command to generate a spiral matrix in MATLAB. But, it creates a matrix that spirals outward, not inward like the task specification requires. It turns out that these matrices can be transformed into each other using some pretty simple transformations.

We start with a simple linear transformation:
<math>(-spiral(n))+n^2</math>
Then depending on if n is odd or even we use either an up/down or left/right mirror transformation.

```MATLAB
function matrix = reverseSpiral(n)

    matrix = (-spiral(n))+n^2;

    if mod(n,2)==0
        matrix = flipud(matrix);
    else
        matrix = fliplr(matrix);
    end

end %reverseSpiral
```

Sample Usage:

```MATLAB>>
 reverseSpiral(5)

ans =

     0     1     2     3     4
    15    16    17    18     5
    14    23    24    19     6
    13    22    21    20     7
    12    11    10     9     8
```



## Maxima


```maxima
spiral(n) := block([a, i, j, k, p, di, dj, vi, vj, imin, imax, jmin, jmax],
a: zeromatrix(n, n),
vi: [1, 0, -1, 0],
vj: [0, 1, 0, -1],
imin: 0,
imax: n,
jmin: 1,
jmax: n + 1,
p: 1,
di: vi[p],
dj: vj[p],
i: 1,
j: 1,
for k from 1 thru n*n do (
   a[j, i]: k,
   i: i + di,
   j: j + dj,
   if i < imin or i > imax or j < jmin or j > jmax then (
      i: i - di,
      j: j - dj,
      p: mod(p, 4) + 1,
      di: vi[p],
      dj: vj[p],
      i: i + di,
      j: j + dj,
      if p = 1 then imax: imax - 1
      elseif p = 2 then jmax: jmax - 1
      elseif p = 3 then imin: imin + 1
      else jmin: jmin + 1
   )
),
a
)$

spiral(5);
/* matrix([ 1,  2,  3,  4,  5],
          [16, 17, 18, 19,  6],
          [15, 24, 25, 20,  7],
          [14, 23, 22, 21,  8],
          [13, 12, 11, 10,  9]) */
```



## NetRexx

{{Trans|ooRexx}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

parse arg size .

if \size.datatype('W') then do
  printArray(generateArray(3))
  say
  printArray(generateArray(4))
  say
  printArray(generateArray(5))
  say
  end
else do
  printArray(generateArray(size))
  say
  end

return

-- -----------------------------------------------------------------------------
method generateArray(dimension = int) private static returns int[,]

  -- the output array
  array = int[dimension, dimension]

  -- get the number of squares, including the center one if
  -- the dimension is odd

  squares = dimension % 2 + dimension // 2

  -- length of a side for the current square
  sidelength = dimension
  current = 0

  loop i_ = 0 to squares - 1

    -- do each side of the current square
    -- top side
    loop j_ = 0 to sidelength - 1
      array[i_, i_ + j_] = current
      current = current + 1
      end j_

    -- down the right side
    loop j_ = 1 to sidelength - 1
      array[i_ + j_, dimension - 1 - i_] = current
      current = current + 1
      end j_

    -- across the bottom
    loop j_ = sidelength - 2 to 0 by -1
      array[dimension - 1 - i_, i_ + j_] = current
      current = current + 1
      end j_

    -- and up the left side
    loop j_ = sidelength - 2 to 1 by -1
      array[i_ + j_, i_] = current
      current = current + 1
      end j_

    -- reduce the length of the side by two rows
    sidelength = sidelength - 2
    end i_

  return array

-- -----------------------------------------------------------------------------
method printArray(array = int[,]) private static

  dimension = array[1].length
  rl = formatSize(array)
  loop i_ = 0 to dimension - 1
    line = Rexx("|")
    loop j_ = 0 to dimension - 1
      line = line Rexx(array[i_, j_]).right(rl)
      end j_
    line = line "|"
    say line
    end i_

  return

-- -----------------------------------------------------------------------------
method formatSize(array = int[,]) private static returns Rexx

  dim = array[1].length
  maxNum = Rexx(dim * dim - 1).length()

  return maxNum


```


{{out}}

```txt

|  0  1  2 |
|  7  8  3 |
|  6  5  4 |

|  0  1  2  3 |
| 11 12 13  4 |
| 10 15 14  5 |
|  9  8  7  6 |

|  0  1  2  3  4 |
| 15 16 17 18  5 |
| 14 23 24 19  6 |
| 13 22 21 20  7 |
| 12 11 10  9  8 |

```



## Nim


```nim
import strutils

type Pos = tuple[x, y: int]

proc newSeqWith[T](len: int, init: T): seq[T] =
  result = newSeq[T] len
  for i in 0 .. <len:
    result[i] = init

proc `^`*(base: int, exp: int): int =
  var (base, exp) = (base, exp)
  result = 1

  while exp != 0:
    if (exp and 1) != 0:
      result *= base
    exp = exp shr 1
    base *= base

proc `$`(m: seq[seq[int]]): string =
  result = ""
  for r in m:
    for c in r:
      result.add align($c, 2) & " "
    result.add "\n"

proc spiral(n): auto =
  result = newSeqWith(n, newSeqWith[int](n, -1))
  var dx = 1
  var dy, x, y = 0
  for i in 0 .. <(n^2):
    result[y][x] = i
    let (nx, ny) = (x+dx, y+dy)
    if nx in 0 .. <n and ny in 0 .. <n and result[ny][nx] == -1:
      x = nx
      y = ny
    else:
      swap dx, dy
      dx = -dx
      x = x + dx
      y = y + dy

echo spiral(5)
```

{{out}}

```txt
 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8
```



## OCaml


```ocaml
let next_dir = function
  |  1,  0 ->  0, -1
  |  0,  1 ->  1,  0
  | -1,  0 ->  0,  1
  |  0, -1 -> -1,  0
  | _ -> assert false

let next_pos ~pos:(x,y) ~dir:(nx,ny) = (x+nx, y+ny)

let next_cell ar ~pos:(x,y) ~dir:(nx,ny) =
  try ar.(x+nx).(y+ny)
  with _ -> -2

let for_loop n init fn =
  let rec aux i v =
    if i < n then aux (i+1) (fn i v)
  in
  aux 0 init

let spiral ~n =
  let ar = Array.make_matrix n n (-1) in
  let pos = 0, 0 in
  let dir = 0, 1 in
  let set (x, y) i = ar.(x).(y) <- i in
  let step (pos, dir) =
    match next_cell ar pos dir with
    | -1 -> (next_pos pos dir, dir)
    | _ -> let dir = next_dir dir in (next_pos pos dir, dir)
  in
  for_loop (n*n) (pos, dir)
           (fun i (pos, dir) -> set pos i; step (pos, dir));
  (ar)

let print =
  Array.iter (fun line ->
    Array.iter (Printf.printf " %2d") line;
    print_newline())

let () = print(spiral 5)
```


Another implementation:

```ocaml
let spiral n =
   let ar = Array.make_matrix n n (-1) in
   let out i = i < 0 || i >= n in
   let too_far (x,y) = out x || out y || ar.(x).(y) >= 0 in
   let step x y (dx,dy) = (x+dx,y+dy) in
   let turn (i,j) = (j,-i) in
   let rec iter (x,y) d i =
      ar.(x).(y) <- i;
      if i < n*n-1 then
         let d' = if too_far (step x y d) then turn d else d in
         iter (step x y d') d' (i+1) in
   (iter (0,0) (0,1) 0; ar)

let show =
   Array.iter (fun v -> Array.iter (Printf.printf " %2d") v; print_newline())

let _ = show (spiral 5)
```



## Octave

{{trans|Stata}}

```octave
function a = spiral(n)
  a = ones(n*n, 1);
  u = -(i = n) * (v = ones(n, 1));
  for k = n-1:-1:1
    j = 1:k;
    a(j+i) = u(j) = -u(j);
    a(j+(i+k)) = v(j) = -v(j);
    i += 2*k;
  endfor
  a(cumsum(a)) = 1:n*n;
  a = reshape(a, n, n)'-1;
endfunction

>> spiral(5)
ans =

    0    1    2    3    4
   15   16   17   18    5
   14   23   24   19    6
   13   22   21   20    7
   12   11   10    9    8
```



## ooRexx


```ooRexx

call printArray generateArray(3)
say
call printArray generateArray(4)
say
call printArray generateArray(5)

::routine generateArray
  use arg dimension
  -- the output array
  array = .array~new(dimension, dimension)

  -- get the number of squares, including the center one if
  -- the dimension is odd
  squares = dimension % 2 + dimension // 2
  -- length of a side for the current square
  sidelength = dimension
  current = 0
  loop i = 1 to squares
      -- do each side of the current square
      -- top side
      loop j = 0 to sidelength - 1
          array[i, i + j] = current
          current += 1
      end
      -- down the right side
      loop j = 1 to sidelength - 1
          array[i + j, dimension - i + 1] = current
          current += 1
      end
      -- across the bottom
      loop j = sidelength - 2 to 0 by -1
          array[dimension - i + 1, i + j] = current
          current += 1
      end
      -- and up the left side
      loop j = sidelength - 2 to 1 by -1
          array[i + j, i] = current
          current += 1
      end
      -- reduce the length of the side by two rows
      sidelength -= 2
  end
  return array

::routine printArray
  use arg array
  dimension = array~dimension(1)
  loop i = 1 to dimension
      line = "|"
      loop j = 1 to dimension
          line = line array[i, j]~right(2)
      end
      line = line "|"
      say line
   end

```

{{out}}

```txt

|  0  1  2 |
|  7  8  3 |
|  6  5  4 |

|  0  1  2  3 |
| 11 12 13  4 |
| 10 15 14  5 |
|  9  8  7  6 |

|  0  1  2  3  4 |
| 15 16 17 18  5 |
| 14 23 24 19  6 |
| 13 22 21 20  7 |
| 12 11 10  9  8 |

```




## Oz

Simple, recursive solution:

```oz
declare
  fun {Spiral N}
     %% create nested array
     Arr = {Array.new 1 N unit}
     for Y in 1..N do Arr.Y := {Array.new 1 N 0} end
     %% fill it recursively with increasing numbers
     C = {Counter 0}
  in
     {Fill Arr 1 N C}
     Arr
  end

  proc {Fill Arr S E C}
     %% go right
     for X in S..E do
        Arr.S.X := {C}
     end
     %% go down
     for Y in S+1..E do
        Arr.Y.E := {C}
     end
     %% go left
     for X in E-1..S;~1 do
        Arr.E.X := {C}
     end
     %% go up
     for Y in E-1..S+1;~1 do
        Arr.Y.S := {C}
     end
     %% fill the inner rectangle
     if E - S > 1 then {Fill Arr S+1 E-1 C} end
  end

  fun {Counter N}
     C = {NewCell N}
  in
     fun {$}
        C := @C + 1
     end
  end
in
  {Inspect {Spiral 5}}
```



## PARI/GP



```parigp
spiral(dim) = {
  my (M = matrix(dim, dim), p = s = 1, q = i = 0);
  for (n=1, dim,
    for (b=1, dim-n+1, M[p,q+=s] = i; i++);
    for (b=1, dim-n, M[p+=s,q] = i; i++);
    s = -s;
  );
  M
}
```


Output:
```txt
spiral(7)

[ 0  1  2  3  4  5  6]

[23 24 25 26 27 28  7]

[22 39 40 41 42 29  8]

[21 38 47 48 43 30  9]

[20 37 46 45 44 31 10]

[19 36 35 34 33 32 11]

[18 17 16 15 14 13 12]
```



## Pascal


```pascal
program Spiralmat;
type
  tDir = (left,down,right,up);
  tdxy = record
           dx,dy: longint;
         end;
  tdeltaDir = array[tDir] of tdxy;
const
  Nextdir : array[tDir] of tDir = (down,right,up,left);
  cDir : tDeltaDir = ((dx:1;dy:0),(dx:0;dy:1),(dx:-1;dy:0),(dx:0;dy:-1));
  cMaxN = 32;
type
  tSpiral =  array[0..cMaxN,0..cMaxN] of LongInt;

function FillSpiral(n:longint):tSpiral;
var
  b,i,k, dn,x,y : longInt;
  dir : tDir;
  tmpSp : tSpiral;
BEGIN
  b := 0;
  x := 0;
  y := 0;
  //only for the first line
  k := -1;
  dn := n-1;
  tmpSp[x,y] := b;
  dir :=  left;
  repeat
    i := 0;
    while i < dn do
    begin
      inc(b);
      tmpSp[x,y] := b;
      inc(x,cDir[dir].dx);
      inc(y,cDir[dir].dy);
      inc(i);
    end;
    Dir:= NextDir[dir];
    inc(k);
    IF k > 1 then
    begin
      k := 0;
      //shorten the line every second direction change
      dn := dn-1;
      if dn <= 0 then
        BREAK;
    end;
  until false;
  //the last
  tmpSp[x,y] := b+1;
  FillSpiral := tmpSp;
end;

var
  a : tSpiral;
  x,y,n : LongInt;
BEGIN
  For n := 1 to 5{cMaxN} do
  begin
    A:=FillSpiral(n);
    For y := 0 to n-1 do
    begin
      For x := 0 to n-1 do
        write(A[x,y]:4);
      writeln;
    end;
    writeln;
  end;
END.

```

{{out}}

```txt
   1

   1   2
   4   3
....
   1   2   3   4   5
  16  17  18  19   6
  15  24  25  20   7
  14  23  22  21   8
  13  12  11  10   9

```



## Perl


```perl
sub spiral
 {my ($n, $x, $y, $dx, $dy, @a) = (shift, 0, 0, 1, 0);
  foreach (0 .. $n**2 - 1)
     {$a[$y][$x] = $_;
      my ($nx, $ny) = ($x + $dx, $y + $dy);
      ($dx, $dy) =
          $dx ==  1 && ($nx == $n || defined $a[$ny][$nx])
        ? ( 0,  1)
        : $dy ==  1 && ($ny == $n || defined $a[$ny][$nx])
        ? (-1,  0)
        : $dx == -1 && ($nx  <  0 || defined $a[$ny][$nx])
        ? ( 0, -1)
        : $dy == -1 && ($ny  <  0 || defined $a[$ny][$nx])
        ? ( 1,  0)
        : ($dx, $dy);
      ($x, $y) = ($x + $dx, $y + $dy);}
  return @a;}

foreach (spiral 5)
   {printf "%3d", $_ foreach @$_;
    print "\n";}
```



## Perl 6

===Object-oriented Solution===
Suppose we set up a Turtle class like this:

```perl6
class Turtle {
    my @dv =  [0,-1], [1,-1], [1,0], [1,1], [0,1], [-1,1], [-1,0], [-1,-1];
    my $points = 8; # 'compass' points of neighbors on grid: north=0, northeast=1, east=2, etc.

    has @.loc = 0,0;
    has $.dir = 0;
    has %.world;
    has $.maxegg;
    has $.range-x;
    has $.range-y;

    method turn-left ($angle = 90) { $!dir -= $angle / 45; $!dir %= $points; }
    method turn-right($angle = 90) { $!dir += $angle / 45; $!dir %= $points; }

    method lay-egg($egg) {
    %!world{~@!loc} = $egg;
    $!maxegg max= $egg;
    $!range-x minmax= @!loc[0];
    $!range-y minmax= @!loc[1];
    }

    method look($ahead = 1) {
    my $there = @!loc »+« @dv[$!dir] »*» $ahead;
    %!world{~$there};
    }

    method forward($ahead = 1) {
    my $there = @!loc »+« @dv[$!dir] »*» $ahead;
    @!loc = @($there);
    }

    method showmap() {
    my $form = "%{$!maxegg.chars}s";
    my $endx = $!range-x.max;
        for $!range-y.list X $!range-x.list -> ($y, $x) {
        print (%!world{"$x $y"} // '').fmt($form);
        print $x == $endx ?? "\n" !! ' ';
    }
    }
}

# Now we can build the spiral in the normal way from outside-in like this:

sub MAIN(Int $size = 5) {
my $t = Turtle.new(dir => 2);
my $counter = 0;
$t.forward(-1);
for 0..^ $size -> $ {
    $t.forward;
    $t.lay-egg($counter++);
}
for $size-1 ... 1 -> $run {
    $t.turn-right;
    $t.forward, $t.lay-egg($counter++) for 0..^$run;
    $t.turn-right;
    $t.forward, $t.lay-egg($counter++) for 0..^$run;
}
$t.showmap;
}
```


Or we can build the spiral from inside-out like this:


```perl6
sub MAIN(Int $size = 5) {
my $t = Turtle.new(dir => ($size %% 2 ?? 4 !! 0));
my $counter = $size * $size;
while $counter {
    $t.lay-egg(--$counter);
    $t.turn-left;
    $t.turn-right if $t.look;
    $t.forward;
}
$t.showmap;
}
```

Note that with these "turtle graphics" we don't actually have to care about the coordinate system, since the <code>showmap</code> method can show whatever rectangle was modified by the turtle.  So unlike the standard inside-out algorithm, we don't have to find the center of the matrix first.


### Procedural Solution


```perl6
sub spiral_matrix ( $n ) {
    my @sm;
    my $len = $n;
    my $pos = 0;

    for ^($n/2).ceiling -> $i {
        my $j = $i +  1;
        my $e = $n - $j;

        @sm[$i     ][$i + $_] = $pos++ for         ^(  $len); # Top
        @sm[$j + $_][$e     ] = $pos++ for         ^(--$len); # Right
        @sm[$e     ][$i + $_] = $pos++ for reverse ^(  $len); # Bottom
        @sm[$j + $_][$i     ] = $pos++ for reverse ^(--$len); # Left
    }

    return @sm;
}

say .fmt('%3d') for spiral_matrix(5);
```

{{out}}

```txt
 0   1   2   3   4
15  16  17  18   5
14  23  24  19   6
13  22  21  20   7
12  11  10   9   8
```



## Phix

{{Trans|Python}}
Simple is better.

```Phix
integer n = 5
string fmt = sprintf("%%%dd",length(sprintf("%d",n*n)))
integer x = 1, y = 0, c = 0, dx = 0, dy = 1, len = n
sequence m = repeat(repeat("??",n),n)
for i=1 to 2*n do                               -- 2n runs..
    for j=1 to len do                           -- of a length...
        x += dx
        y += dy
        m[x][y] = sprintf(fmt,c)
        c += 1
    end for
    len -= and_bits(i,1)                        -- ..-1 every other
    {dx,dy} = {dy,-dx}                          -- in new direction
end for

for i=1 to n do
    m[i] = join(m[i])
end for
puts(1,join(m,"\n"))
```

{{out}}

```txt

 0  1  2  3  4  5
19 20 21 22 23  6
18 31 32 33 24  7
17 30 35 34 25  8
16 29 28 27 26  9
15 14 13 12 11 10

```



## PicoLisp

This example uses 'grid' from "lib/simul.l", which maintains a two-dimensional structure and is normally used for simulations and board games.

```PicoLisp
(load "@lib/simul.l")

(de spiral (N)
   (prog1 (grid N N)
      (let (Dir '(north east south west .)  This 'a1)
         (for Val (* N N)
            (=: val Val)
            (setq This
               (or
                  (with ((car Dir) This)
                     (unless (: val) This) )
                  (with ((car (setq Dir (cdr Dir))) This)
                     (unless (: val) This) ) ) ) ) ) ) )

(mapc
   '((L)
      (for This L (prin (align 3 (: val))))
      (prinl) )
   (spiral 5) )
```

{{out}}

```txt
  1  2  3  4  5
 16 17 18 19  6
 15 24 25 20  7
 14 23 22 21  8
 13 12 11 10  9
```



## PL/I


```pli
/* Generates a square matrix containing the integers from 0 to N**2-1, */
/* where N is the length of one side of the square.                    */
/* Written 22 February 2010.                                           */
   declare n fixed binary;

put skip list ('Please type the size of the square:');
get list (n);

begin;
   declare A(n,n) fixed binary;
   declare (i, j, iinc, jinc, q) fixed binary;

   A = -1;

   i, j = 1; iinc = 0; jinc = 1;
   do q = 0 to n**2-1;
      if a(i,j) < 0 then
         a(i,j) = q;
      else
         do;
             /* back up */
             j = j -jinc; i = i - iinc;
             /* change direction */
             if iinc = 0 & jinc = 1 then do; iinc = 1; jinc = 0; end;
             else if iinc =  1 & jinc =  0 then do; iinc =  0; jinc = -1; end;
             else if iinc =  0 & jinc = -1 then do; iinc = -1; jinc =  0; end;
             else if iinc = -1 & jinc =  0 then do; iinc =  0; jinc =  1; end;
            /* Take one step in the new direction */
             i = i + iinc; j = j + jinc;
             a(i,j) = q;
         end;
      if i+iinc > n | i+iinc < 1 then
         do;
            iinc = 0; jinc = 1;
            if j+1 > n then jinc = -1; else if j-1 < 1 then jinc = 1;
            if a(i+iinc,j+jinc) >= 0 then jinc = -jinc;
            /* j = j + jinc; /* to move on from the present (filled) position */
         end;
      else i = i + iinc;
      if j+jinc > n | j+jinc < 1 then
         do;
            jinc = 0; iinc = 1;
            if i+1 > n then iinc = -1; else if i-1 < 1 then iinc = 1;
            if a(i+iinc,j+jinc) >= 0 then iinc = -iinc;
            i = i + iinc; /* to move on from the present (filled) position */
         end;
      else j = j + jinc;
   end;

   /* Display the square. */
   do i = 1 to n;
      put skip edit (A(i,*)) (F(4));
   end;

end;
```



## PowerShell


```powershell
function Spiral-Matrix ( [int]$N )
    {
    #  Initialize variables
    $X = 0
    $Y = -1
    $i = 0
    $Sign = 1

    #  Intialize array
    $A = New-Object 'int[,]' $N, $N

    #  Set top row
    1..$N | ForEach { $Y += $Sign; $A[$X,$Y] = ++$i }

    #  For each remaining half spiral...
    ForEach ( $M in ($N-1)..1 )
        {
        #  Set the vertical quarter spiral
        1..$M | ForEach { $X += $Sign; $A[$X,$Y] = ++$i }

        #  Curve the spiral
        $Sign = -$Sign

        #  Set the horizontal quarter spiral
        1..$M | ForEach { $Y += $Sign; $A[$X,$Y] = ++$i }
        }

    #  Convert the array to text output
    $Spiral = ForEach ( $X in 1..$N ) { ( 1..$N | ForEach { $A[($X-1),($_-1)] } ) -join "`t" }

    return $Spiral
    }

Spiral-Matrix 5
""
Spiral-Matrix 7
```

{{out}}

```txt
1	2	3	4	5
16	17	18	19	6
15	24	25	20	7
14	23	22	21	8
13	12	11	10	9

1	2	3	4	5	6	7
24	25	26	27	28	29	8
23	40	41	42	43	30	9
22	39	48	49	44	31	10
21	38	47	46	45	32	11
20	37	36	35	34	33	12
19	18	17	16	15	14	13
```



## Prolog


```Prolog

%  Prolog implementation: SWI-Prolog 7.2.3

replace([_|T], 0, E, [E|T]) :- !.
replace([H|T], N, E, Xs)    :-
  succ(N1, N), replace(T, N1, E, Xs1), Xs = [H|Xs1].

% True if Xs is the Original grid with the element at (X, Y) replaces by E.
replace_in([H|T], (0, Y), E, Xs) :- replace(H, Y, E, NH), Xs = [NH|T], !.
replace_in([H|T], (X, Y), E, Xs) :-
  succ(X1, X), replace_in(T, (X1, Y), E, Xs1), Xs = [H|Xs1].

% True, if E is the value at (X, Y) in Xs
get_in(Xs, (X, Y), E) :- nth0(X, Xs, L), nth0(Y, L, E).

create(N, Mx) :-             % NxN grid full of nils
  numlist(1, N, Ns),
  findall(X, (member(_, Ns), X = nil), Ls),
  findall(X, (member(_, Ns), X = Ls), Mx).

% Depending of the direction, returns two possible coordinates and directions
% (C,D) that will be used in case of a turn, and (A,B) otherwise.
ops(right, (X,Y), (A,B), (C,D), D1, D2) :-
  A is X, B is Y+1, D1 = right, C is X+1, D is Y, D2 = down.

ops(left, (X,Y), (A,B), (C,D), D1, D2) :-
  A is X, B is Y-1, D1 = left, C is X-1, D is Y, D2 = up.

ops(up, (X,Y), (A,B), (C,D), D1, D2) :-
  A is X-1, B is Y, D1 = up, C is X, D is Y+1, D2 = right.

ops(down, (X,Y), (A,B), (C,D), D1, D2) :-
  A is X+1, B is Y, D1 = down, C is X, D is Y-1, D2 = left.

% True if NCoor is the right coor in spiral shape. Returns a new direction also.
next(Dir, Mx, Coor, NCoor, NDir) :-
  ops(Dir, Coor, C1, C2, D1, D2),
  (get_in(Mx, C1, nil) -> NCoor = C1, NDir = D1
                        ; NCoor = C2, NDir = D2).

% Returns an spiral with [H|Vs] elements called R, only work if the length of
% [H|Vs], is the square of the size of the grid.
spiralH(Dir, Mx, Coor, [H|Vs], R)  :-
 replace_in(Mx, Coor, H, NMx),
 (Vs = [] -> R = NMx
           ; next(Dir, Mx, Coor, NCoor, NDir),
             spiralH(NDir, NMx, NCoor, Vs, R)).

% True if Mx is the grid in spiral shape of the numbers from 0 to N*N-1.
spiral(N, Mx) :-
  Sq is N*N-1, numlist(0, Sq, Ns),
  create(N, EMx), spiralH(right, EMx, (0,0), Ns, Mx).

```

{{out}}

```txt

?- spiral(6,Mx), forall(member(X,Mx), writeln(X)).
[0,1,2,3,4,5]
[19,20,21,22,23,6]
[18,31,32,33,24,7]
[17,30,35,34,25,8]
[16,29,28,27,26,9]
[15,14,13,12,11,10]

```



## PureBasic

{{trans|Fortran}}

```PureBasic
Procedure spiralMatrix(size = 1)
  Protected i, x = -1, y, count = size, n
  Dim a(size - 1,size - 1)

  For i = 1 To count
    x + 1
    a(x,y) = n
    n + 1
  Next

  Repeat
    count - 1
    For i = 1 To count
      y + 1
      a(x,y) = n
      n + 1
    Next
    For i = 1 To count
      x - 1
      a(x,y) = n
      n + 1
    Next

    count - 1
    For i = 1 To count
      y - 1
      a(x,y) = n
      n + 1
    Next
    For i = 1 To count
      x + 1
      a(x,y) = n
      n + 1
    Next
  Until count < 1

  PrintN("Spiral: " + Str(Size) + #CRLF$)
  Protected colWidth = Len(Str(size * size - 1)) + 1
  For y = 0 To size - 1
    For x = 0 To size - 1
      Print("" + LSet(Str(a(x, y)), colWidth, " ") + "")
    Next
    PrintN("")
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  spiralMatrix(2)
  PrintN("")
  spiralMatrix(5)


  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
Spiral: 2

0 1
3 2


Spiral: 5

0  1  2  3  4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10 9  8
```



## Python


```python
def spiral(n):
    dx,dy = 1,0            # Starting increments
    x,y = 0,0              # Starting location
    myarray = [[None]* n for j in range(n)]
    for i in xrange(n**2):
        myarray[x][y] = i
        nx,ny = x+dx, y+dy
        if 0<=nx<n and 0<=ny<n and myarray[nx][ny] == None:
            x,y = nx,ny
        else:
            dx,dy = -dy,dx
            x,y = x+dx, y+dy
    return myarray

def printspiral(myarray):
    n = range(len(myarray))
    for y in n:
        for x in n:
            print "%2i" % myarray[x][y],
        print

printspiral(spiral(5))
```

{{out}}

```txt

 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8

```



### Recursive Solution


```python
def spiral(n):
    def spiral_part(x, y, n):
        if x == -1 and y == 0:
            return -1
        if y == (x+1) and x < (n // 2):
            return spiral_part(x-1, y-1, n-1) + 4*(n-y)
        if x < (n-y) and y <= x:
            return spiral_part(y-1, y, n) + (x-y) + 1
        if x >= (n-y) and y <= x:
            return spiral_part(x, y-1, n) + 1
        if x >= (n-y) and y > x:
            return spiral_part(x+1, y, n) + 1
        if x < (n-y) and y > x:
            return spiral_part(x, y-1, n) - 1

    array = [[0] * n for j in xrange(n)]
    for x in xrange(n):
        for y in xrange(n):
            array[x][y] = spiral_part(y, x, n)
    return array

for row in spiral(5):
    print " ".join("%2s" % x for x in row)
```

Adding a cache for the ''spiral_part'' function it could be quite efficient.


Recursion by rotating the solution for rest of the square except the first row,

```python
def rot_right(a):
    return zip(*a[::-1])

def sp(m, n, start = 0):
    """ Generate number range spiral of dimensions m x n
    """
    if n == 0:
        yield ()
    else:
        yield tuple(range(start, m + start))
        for row in rot_right(list(sp(n - 1, m, m + start))):
            yield row

def spiral(m):
    return sp(m, m)

for row in spiral(5):
    print(''.join('%3i' % i for i in row))
```



Another way, based on preparing lists ahead

```python
def spiral(n):
    dat = [[None] * n for i in range(n)]
    le = [[i + 1, i + 1] for i in reversed(range(n))]
    le = sum(le, [])[1:]  # for n = 5 le will be [5, 4, 4, 3, 3, 2, 2, 1, 1]
    dxdy = [[1, 0], [0, 1], [-1, 0], [0, -1]] * ((len(le) + 4) / 4)  # long enough
    x, y, val = -1, 0, -1
    for steps, (dx, dy) in zip(le, dxdy):
        x, y, val = x + dx, y + dy, val + 1
        for j in range(steps):
            dat[y][x] = val
            if j != steps-1:
                x, y, val = x + dx, y + dy, val + 1
    return dat

for row in spiral(5): # calc spiral and print it
    print ' '.join('%3s' % x for x in row)
```



### Functional Solutions

{{works with|Python|2.6, 3.0}}

```python
import itertools

concat = itertools.chain.from_iterable
def partial_sums(items):
    s = 0
    for x in items:
        s += x
        yield s

grade = lambda xs: sorted(range(len(xs)), key=xs.__getitem__)
values = lambda n: itertools.cycle([1,n,-1,-n])
counts = lambda n: concat([i,i-1] for i in range(n,0,-1))
reshape = lambda n, xs: zip(*([iter(xs)] * n))

spiral = lambda n: reshape(n, grade(list(partial_sums(concat(
                       [v]*c for c,v in zip(counts(n), values(n)))))))

for row in spiral(5):
    print(' '.join('%3s' % x for x in row))
```



Or, as an alternative to generative mutation:
{{Works with|Python|3.7}}
{{Trans|Haskell}}

```python
'''Spiral Matrix'''


# spiral :: Int -> [[Int]]
def spiral(n):
    '''The rows of a spiral matrix of order N.
    '''
    def go(rows, cols, x):
        return [list(range(x, x + cols))] + [
            list(reversed(x)) for x
            in zip(*go(cols, rows - 1, x + cols))
        ] if 0 < rows else [[]]
    return go(n, n, 0)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Spiral matrix of order 5, in wiki table markup'''

    print(wikiTable(
        spiral(5)
    ))


# FORMATTING ----------------------------------------------

# wikiTable :: [[a]] -> String
def wikiTable(rows):
    '''Wiki markup for a no-frills tabulation of rows.'''
    return '{| class="wikitable" style="' + (
        'width:12em;height:12em;table-layout:fixed;"|-\n'
    ) + '\n|-\n'.join([
        '| ' + ' || '.join([str(cell) for cell in row])
        for row in rows
    ]) + '\n|}'


# MAIN ---
if __name__ == '__main__':
    main()
```

{| class="wikitable" style="width:12em;height:12em;table-layout:fixed;"|-
| 0 || 1 || 2 || 3 || 4
|-
| 15 || 16 || 17 || 18 || 5
|-
| 14 || 23 || 24 || 19 || 6
|-
| 13 || 22 || 21 || 20 || 7
|-
| 12 || 11 || 10 || 9 || 8
|}


###  Simple solution


```python
def spiral_matrix(n):
    m = [[0] * n for i in range(n)]
    dx, dy = [0, 1, 0, -1], [1, 0, -1, 0]
    x, y, c = 0, -1, 1
    for i in range(n + n - 1):
        for j in range((n + n - i) // 2):
            x += dx[i % 4]
            y += dy[i % 4]
            m[x][y] = c
            c += 1
    return m
for i in spiral_matrix(5): print(*i)
```

{{out}}
<lang>1 2 3 4 5
16 17 18 19 6
15 24 25 20 7
14 23 22 21 8
13 12 11 10 9
```



## R


```R
spiral_matrix <- function(n) {
    stopifnot(is.numeric(n))
    stopifnot(n > 0)
    steps <- c(1, n, -1, -n)
    reps <- n - seq_len(n * 2 - 1L) %/% 2
    indicies <- rep(rep_len(steps, length(reps)), reps)
    indicies <- cumsum(indicies)
    values <- integer(length(indicies))
    values[indicies] <- seq_along(indicies)
    matrix(values, n, n, byrow = TRUE)
}
```

{{out}}

```R>
 spiral_matrix(5)
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    2    3    4    5
[2,]   16   17   18   19    6
[3,]   15   24   25   20    7
[4,]   14   23   22   21    8
[5,]   13   12   11   10    9

> t(spiral_matrix(5))
     [,1] [,2] [,3] [,4] [,5]
[1,]    1   16   15   14   13
[2,]    2   17   24   23   12
[3,]    3   18   25   22   11
[4,]    4   19   20   21   10
[5,]    5    6    7    8    9
```



### Recursive Solution


```R
spiral_matrix <- function(n) {
    spiralv <- function(v) {
        n <- sqrt(length(v))
        if (n != floor(n))
            stop("length of v should be a square of an integer")
        if (n == 0)
            stop("v should be of positive length")
        if (n == 1)
            m <- matrix(v, 1, 1)
        else
            m <- rbind(v[1:n], cbind(spiralv(v[(2 * n):(n^2)])[(n - 1):1, (n - 1):1], v[(n + 1):(2 * n - 1)]))
        m
    }
    spiralv(1:(n^2))
}
```



## Racket


```racket

#lang racket
(require math)

(define (spiral rows columns)
  (define (index x y) (+ (* x columns) y))
  (do ((N (* rows columns))
       (spiral (make-vector (* rows columns) #f))
       (dx 1) (dy 0) (x 0) (y 0)
       (i 0 (+ i 1)))
      ((= i N) spiral)
    (vector-set! spiral (index y x) i)
    (let ((nx (+ x dx)) (ny (+ y dy)))
      (cond
       ((and (< -1 nx columns)
             (< -1 ny rows)
             (not (vector-ref spiral (index ny nx))))
        (set! x nx)
        (set! y ny))
       (else
        (set!-values (dx dy) (values (- dy) dx))
        (set! x (+ x dx))
        (set! y (+ y dy)))))))

(vector->matrix 4 4 (spiral 4 4))

```

{{out}}

```racket

(mutable-array #[#[0 1 2 3] #[11 12 13 4] #[10 15 14 5] #[9 8 7 6]])

```



## REXX

Original logic borrowed (mostly) from the [[#Fortran|Fortran]] example.

### static column width


```rexx
/*REXX program displays a spiral in a  square array  (of any size)  starting at  START. */
parse arg size start .                           /*obtain optional arguments from the CL*/
if size =='' | size ==","  then size =5          /*Not specified?  Then use the default.*/
if start=='' | start==","  then start=0          /*Not specified?  Then use the default.*/
tot=size**2;           L=length(tot + start)     /*total number of elements in spiral.  */
k=size                                           /*K:   is the counter for the spiral.  */
row=1;       col=0                               /*start spiral at    row 1,  column 0. */
                                                 /* [↓]  construct the numbered spiral. */
     do n=0  for k;    col=col + 1;   @.col.row=n + start;   end;       if k==0  then exit
                                                 /* [↑]  build the first row of spiral. */
     do until  n>=tot                                                   /*spiral matrix.*/
        do one=1  to -1  by -2  until n>=tot;   k=k-1                   /*perform twice.*/
          do n=n  for k;   row=row + one;    @.col.row=n + start;   end /*for the row···*/
          do n=n  for k;   col=col - one;    @.col.row=n + start;   end /* "   "  col···*/
        end   /*one*/                                                   /* ↑↓ direction.*/
     end      /*until n≥tot*/                    /* [↑]   done with the matrix spiral.  */
                                                 /* [↓]   display spiral to the screen. */
  do      r=1  for size;    _=   right(@.1.r, L) /*construct display   row   by    row. */
       do c=2  for size -1; _=_  right(@.c.r, L) /*construct a line  for the display.   */
       end   /*col*/                             /* [↑]  line has an extra leading blank*/
  say _                                          /*display a line (row) of the spiral.  */
  end      /*row*/                               /*stick a fork in it,  we're all done. */
```

{{out|output|text=  using the default array size of:   '''5'''}}

```txt

 0  1  2  3  4
15 16 17 18  5
14 23 24 19  6
13 22 21 20  7
12 11 10  9  8

```

{{out|output|text=  using an array size   and   start value of:   '''10'''   '''-70000'''}}

```txt

-70000 -69999 -69998 -69997 -69996 -69995 -69994 -69993 -69992 -69991
-69965 -69964 -69963 -69962 -69961 -69960 -69959 -69958 -69957 -69990
-69966 -69937 -69936 -69935 -69934 -69933 -69932 -69931 -69956 -69989
-69967 -69938 -69917 -69916 -69915 -69914 -69913 -69930 -69955 -69988
-69968 -69939 -69918 -69905 -69904 -69903 -69912 -69929 -69954 -69987
-69969 -69940 -69919 -69906 -69901 -69902 -69911 -69928 -69953 -69986
-69970 -69941 -69920 -69907 -69908 -69909 -69910 -69927 -69952 -69985
-69971 -69942 -69921 -69922 -69923 -69924 -69925 -69926 -69951 -69984
-69972 -69943 -69944 -69945 -69946 -69947 -69948 -69949 -69950 -69983
-69973 -69974 -69975 -69976 -69977 -69978 -69979 -69980 -69981 -69982

```

{{out|output|text=  (shown at <sup>3</sup>/<sub>4</sub> size)   using an array size of:    '''36'''
<b>
<pre style="font-size:75%">
   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32   33   34   35
 139  140  141  142  143  144  145  146  147  148  149  150  151  152  153  154  155  156  157  158  159  160  161  162  163  164  165  166  167  168  169  170  171  172  173   36
 138  271  272  273  274  275  276  277  278  279  280  281  282  283  284  285  286  287  288  289  290  291  292  293  294  295  296  297  298  299  300  301  302  303  174   37
 137  270  395  396  397  398  399  400  401  402  403  404  405  406  407  408  409  410  411  412  413  414  415  416  417  418  419  420  421  422  423  424  425  304  175   38
 136  269  394  511  512  513  514  515  516  517  518  519  520  521  522  523  524  525  526  527  528  529  530  531  532  533  534  535  536  537  538  539  426  305  176   39
 135  268  393  510  619  620  621  622  623  624  625  626  627  628  629  630  631  632  633  634  635  636  637  638  639  640  641  642  643  644  645  540  427  306  177   40
 134  267  392  509  618  719  720  721  722  723  724  725  726  727  728  729  730  731  732  733  734  735  736  737  738  739  740  741  742  743  646  541  428  307  178   41
 133  266  391  508  617  718  811  812  813  814  815  816  817  818  819  820  821  822  823  824  825  826  827  828  829  830  831  832  833  744  647  542  429  308  179   42
 132  265  390  507  616  717  810  895  896  897  898  899  900  901  902  903  904  905  906  907  908  909  910  911  912  913  914  915  834  745  648  543  430  309  180   43
 131  264  389  506  615  716  809  894  971  972  973  974  975  976  977  978  979  980  981  982  983  984  985  986  987  988  989  916  835  746  649  544  431  310  181   44
 130  263  388  505  614  715  808  893  970 1039 1040 1041 1042 1043 1044 1045 1046 1047 1048 1049 1050 1051 1052 1053 1054 1055  990  917  836  747  650  545  432  311  182   45
 129  262  387  504  613  714  807  892  969 1038 1099 1100 1101 1102 1103 1104 1105 1106 1107 1108 1109 1110 1111 1112 1113 1056  991  918  837  748  651  546  433  312  183   46
 128  261  386  503  612  713  806  891  968 1037 1098 1151 1152 1153 1154 1155 1156 1157 1158 1159 1160 1161 1162 1163 1114 1057  992  919  838  749  652  547  434  313  184   47
 127  260  385  502  611  712  805  890  967 1036 1097 1150 1195 1196 1197 1198 1199 1200 1201 1202 1203 1204 1205 1164 1115 1058  993  920  839  750  653  548  435  314  185   48
 126  259  384  501  610  711  804  889  966 1035 1096 1149 1194 1231 1232 1233 1234 1235 1236 1237 1238 1239 1206 1165 1116 1059  994  921  840  751  654  549  436  315  186   49
 125  258  383  500  609  710  803  888  965 1034 1095 1148 1193 1230 1259 1260 1261 1262 1263 1264 1265 1240 1207 1166 1117 1060  995  922  841  752  655  550  437  316  187   50
 124  257  382  499  608  709  802  887  964 1033 1094 1147 1192 1229 1258 1279 1280 1281 1282 1283 1266 1241 1208 1167 1118 1061  996  923  842  753  656  551  438  317  188   51
 123  256  381  498  607  708  801  886  963 1032 1093 1146 1191 1228 1257 1278 1291 1292 1293 1284 1267 1242 1209 1168 1119 1062  997  924  843  754  657  552  439  318  189   52
 122  255  380  497  606  707  800  885  962 1031 1092 1145 1190 1227 1256 1277 1290 1295 1294 1285 1268 1243 1210 1169 1120 1063  998  925  844  755  658  553  440  319  190   53
 121  254  379  496  605  706  799  884  961 1030 1091 1144 1189 1226 1255 1276 1289 1288 1287 1286 1269 1244 1211 1170 1121 1064  999  926  845  756  659  554  441  320  191   54
 120  253  378  495  604  705  798  883  960 1029 1090 1143 1188 1225 1254 1275 1274 1273 1272 1271 1270 1245 1212 1171 1122 1065 1000  927  846  757  660  555  442  321  192   55
 119  252  377  494  603  704  797  882  959 1028 1089 1142 1187 1224 1253 1252 1251 1250 1249 1248 1247 1246 1213 1172 1123 1066 1001  928  847  758  661  556  443  322  193   56
 118  251  376  493  602  703  796  881  958 1027 1088 1141 1186 1223 1222 1221 1220 1219 1218 1217 1216 1215 1214 1173 1124 1067 1002  929  848  759  662  557  444  323  194   57
 117  250  375  492  601  702  795  880  957 1026 1087 1140 1185 1184 1183 1182 1181 1180 1179 1178 1177 1176 1175 1174 1125 1068 1003  930  849  760  663  558  445  324  195   58
 116  249  374  491  600  701  794  879  956 1025 1086 1139 1138 1137 1136 1135 1134 1133 1132 1131 1130 1129 1128 1127 1126 1069 1004  931  850  761  664  559  446  325  196   59
 115  248  373  490  599  700  793  878  955 1024 1085 1084 1083 1082 1081 1080 1079 1078 1077 1076 1075 1074 1073 1072 1071 1070 1005  932  851  762  665  560  447  326  197   60
 114  247  372  489  598  699  792  877  954 1023 1022 1021 1020 1019 1018 1017 1016 1015 1014 1013 1012 1011 1010 1009 1008 1007 1006  933  852  763  666  561  448  327  198   61
 113  246  371  488  597  698  791  876  953  952  951  950  949  948  947  946  945  944  943  942  941  940  939  938  937  936  935  934  853  764  667  562  449  328  199   62
 112  245  370  487  596  697  790  875  874  873  872  871  870  869  868  867  866  865  864  863  862  861  860  859  858  857  856  855  854  765  668  563  450  329  200   63
 111  244  369  486  595  696  789  788  787  786  785  784  783  782  781  780  779  778  777  776  775  774  773  772  771  770  769  768  767  766  669  564  451  330  201   64
 110  243  368  485  594  695  694  693  692  691  690  689  688  687  686  685  684  683  682  681  680  679  678  677  676  675  674  673  672  671  670  565  452  331  202   65
 109  242  367  484  593  592  591  590  589  588  587  586  585  584  583  582  581  580  579  578  577  576  575  574  573  572  571  570  569  568  567  566  453  332  203   66
 108  241  366  483  482  481  480  479  478  477  476  475  474  473  472  471  470  469  468  467  466  465  464  463  462  461  460  459  458  457  456  455  454  333  204   67
 107  240  365  364  363  362  361  360  359  358  357  356  355  354  353  352  351  350  349  348  347  346  345  344  343  342  341  340  339  338  337  336  335  334  205   68
 106  239  238  237  236  235  234  233  232  231  230  229  228  227  226  225  224  223  222  221  220  219  218  217  216  215  214  213  212  211  210  209  208  207  206   69
 105  104  103  102  101  100   99   98   97   96   95   94   93   92   91   90   89   88   87   86   85   84   83   82   81   80   79   78   77   76   75   74   73   72   71   70

```

</b>


### minimum column width

This REXX version automatically adjusts the width of the spiral matrix columns to minimize the area of the matrix display (so more elements may be shown on a display screen).

```rexx
/*REXX program displays a spiral in a  square array  (of any size)  starting at  START. */
parse arg size start .                           /*obtain optional arguments from the CL*/
if size =='' | size ==","  then size =5          /*Not specified?  Then use the default.*/
if start=='' | start==","  then start=0          /*Not specified?  Then use the default.*/
tot=size**2;           L=length(tot + start)     /*total number of elements in spiral.  */
k=size                                           /*K:   is the counter for the spiral.  */
row=1;       col=0                               /*start spiral at    row 1,  column 0. */
                                                 /* [↓]  construct the numbered spiral. */
     do n=0  for k;   col=col + 1;    @.col.row=n + start;   end;   if k==0  then exit
                                                 /* [↑]  build the first row of spiral. */
     do until  n>=tot                                                   /*spiral matrix.*/
        do one=1  to -1  by -2  until n>=tot;     k=k - 1               /*perform twice.*/
          do n=n  for k;   row=row + one;   @.col.row=n + start;   end  /*for the row···*/
          do n=n  for k;   col=col - one;   @.col.row=n + start;   end  /* "   "  col···*/
        end   /*one*/                                                   /* ↑↓ direction.*/
     end      /*until n≥tot*/                    /* [↑]   done with the matrix spiral.  */
!.=0                                             /* [↓]   display spiral to the screen. */
     do two=0  for 2                             /*1st time?  Find max column and width.*/
       do   r=1  for size;  _=                   /*construct display   row  by   row.   */
         do c=1  for size;  x=@.c.r              /*construct a line  column by column.  */
         if two  then _=_ right(x, !.c)          /*construct a line  for the display.   */
                 else !.c=max(!.c, length(x))    /*find the maximum width of the column.*/
         end   /*c*/                             /* [↓]  line has an extra leading blank*/
       if two  then say substr(_, 2)             /*this SUBSTR ignores the first blank. */
       end     /*r*/
     end       /*two*/                           /*stick a fork in it,  we're all done. */
```

{{out|output|text=  (shown at <sup>3</sup>/<sub>4</sub> size)   using an array size of:    '''36'''
<b>
<pre style="font-size:75%">
  0   1   2   3   4   5   6   7   8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26  27  28  29  30  31  32  33  34 35
139 140 141 142 143 144 145 146 147  148  149  150  151  152  153  154  155  156  157  158  159  160  161  162  163  164  165 166 167 168 169 170 171 172 173 36
138 271 272 273 274 275 276 277 278  279  280  281  282  283  284  285  286  287  288  289  290  291  292  293  294  295  296 297 298 299 300 301 302 303 174 37
137 270 395 396 397 398 399 400 401  402  403  404  405  406  407  408  409  410  411  412  413  414  415  416  417  418  419 420 421 422 423 424 425 304 175 38
136 269 394 511 512 513 514 515 516  517  518  519  520  521  522  523  524  525  526  527  528  529  530  531  532  533  534 535 536 537 538 539 426 305 176 39
135 268 393 510 619 620 621 622 623  624  625  626  627  628  629  630  631  632  633  634  635  636  637  638  639  640  641 642 643 644 645 540 427 306 177 40
134 267 392 509 618 719 720 721 722  723  724  725  726  727  728  729  730  731  732  733  734  735  736  737  738  739  740 741 742 743 646 541 428 307 178 41
133 266 391 508 617 718 811 812 813  814  815  816  817  818  819  820  821  822  823  824  825  826  827  828  829  830  831 832 833 744 647 542 429 308 179 42
132 265 390 507 616 717 810 895 896  897  898  899  900  901  902  903  904  905  906  907  908  909  910  911  912  913  914 915 834 745 648 543 430 309 180 43
131 264 389 506 615 716 809 894 971  972  973  974  975  976  977  978  979  980  981  982  983  984  985  986  987  988  989 916 835 746 649 544 431 310 181 44
130 263 388 505 614 715 808 893 970 1039 1040 1041 1042 1043 1044 1045 1046 1047 1048 1049 1050 1051 1052 1053 1054 1055  990 917 836 747 650 545 432 311 182 45
129 262 387 504 613 714 807 892 969 1038 1099 1100 1101 1102 1103 1104 1105 1106 1107 1108 1109 1110 1111 1112 1113 1056  991 918 837 748 651 546 433 312 183 46
128 261 386 503 612 713 806 891 968 1037 1098 1151 1152 1153 1154 1155 1156 1157 1158 1159 1160 1161 1162 1163 1114 1057  992 919 838 749 652 547 434 313 184 47
127 260 385 502 611 712 805 890 967 1036 1097 1150 1195 1196 1197 1198 1199 1200 1201 1202 1203 1204 1205 1164 1115 1058  993 920 839 750 653 548 435 314 185 48
126 259 384 501 610 711 804 889 966 1035 1096 1149 1194 1231 1232 1233 1234 1235 1236 1237 1238 1239 1206 1165 1116 1059  994 921 840 751 654 549 436 315 186 49
125 258 383 500 609 710 803 888 965 1034 1095 1148 1193 1230 1259 1260 1261 1262 1263 1264 1265 1240 1207 1166 1117 1060  995 922 841 752 655 550 437 316 187 50
124 257 382 499 608 709 802 887 964 1033 1094 1147 1192 1229 1258 1279 1280 1281 1282 1283 1266 1241 1208 1167 1118 1061  996 923 842 753 656 551 438 317 188 51
123 256 381 498 607 708 801 886 963 1032 1093 1146 1191 1228 1257 1278 1291 1292 1293 1284 1267 1242 1209 1168 1119 1062  997 924 843 754 657 552 439 318 189 52
122 255 380 497 606 707 800 885 962 1031 1092 1145 1190 1227 1256 1277 1290 1295 1294 1285 1268 1243 1210 1169 1120 1063  998 925 844 755 658 553 440 319 190 53
121 254 379 496 605 706 799 884 961 1030 1091 1144 1189 1226 1255 1276 1289 1288 1287 1286 1269 1244 1211 1170 1121 1064  999 926 845 756 659 554 441 320 191 54
120 253 378 495 604 705 798 883 960 1029 1090 1143 1188 1225 1254 1275 1274 1273 1272 1271 1270 1245 1212 1171 1122 1065 1000 927 846 757 660 555 442 321 192 55
119 252 377 494 603 704 797 882 959 1028 1089 1142 1187 1224 1253 1252 1251 1250 1249 1248 1247 1246 1213 1172 1123 1066 1001 928 847 758 661 556 443 322 193 56
118 251 376 493 602 703 796 881 958 1027 1088 1141 1186 1223 1222 1221 1220 1219 1218 1217 1216 1215 1214 1173 1124 1067 1002 929 848 759 662 557 444 323 194 57
117 250 375 492 601 702 795 880 957 1026 1087 1140 1185 1184 1183 1182 1181 1180 1179 1178 1177 1176 1175 1174 1125 1068 1003 930 849 760 663 558 445 324 195 58
116 249 374 491 600 701 794 879 956 1025 1086 1139 1138 1137 1136 1135 1134 1133 1132 1131 1130 1129 1128 1127 1126 1069 1004 931 850 761 664 559 446 325 196 59
115 248 373 490 599 700 793 878 955 1024 1085 1084 1083 1082 1081 1080 1079 1078 1077 1076 1075 1074 1073 1072 1071 1070 1005 932 851 762 665 560 447 326 197 60
114 247 372 489 598 699 792 877 954 1023 1022 1021 1020 1019 1018 1017 1016 1015 1014 1013 1012 1011 1010 1009 1008 1007 1006 933 852 763 666 561 448 327 198 61
113 246 371 488 597 698 791 876 953  952  951  950  949  948  947  946  945  944  943  942  941  940  939  938  937  936  935 934 853 764 667 562 449 328 199 62
112 245 370 487 596 697 790 875 874  873  872  871  870  869  868  867  866  865  864  863  862  861  860  859  858  857  856 855 854 765 668 563 450 329 200 63
111 244 369 486 595 696 789 788 787  786  785  784  783  782  781  780  779  778  777  776  775  774  773  772  771  770  769 768 767 766 669 564 451 330 201 64
110 243 368 485 594 695 694 693 692  691  690  689  688  687  686  685  684  683  682  681  680  679  678  677  676  675  674 673 672 671 670 565 452 331 202 65
109 242 367 484 593 592 591 590 589  588  587  586  585  584  583  582  581  580  579  578  577  576  575  574  573  572  571 570 569 568 567 566 453 332 203 66
108 241 366 483 482 481 480 479 478  477  476  475  474  473  472  471  470  469  468  467  466  465  464  463  462  461  460 459 458 457 456 455 454 333 204 67
107 240 365 364 363 362 361 360 359  358  357  356  355  354  353  352  351  350  349  348  347  346  345  344  343  342  341 340 339 338 337 336 335 334 205 68
106 239 238 237 236 235 234 233 232  231  230  229  228  227  226  225  224  223  222  221  220  219  218  217  216  215  214 213 212 211 210 209 208 207 206 69
105 104 103 102 101 100  99  98  97   96   95   94   93   92   91   90   89   88   87   86   85   84   83   82   81   80   79  78  77  76  75  74  73  72  71 70

```

</b>


## Ring


```ring

# Project : Spiral matrix

load "guilib.ring"
load "stdlib.ring"
new qapp
        {
        win1 = new qwidget() {
                   setwindowtitle("Spiral matrix")
                   setgeometry(100,100,600,400)
                   n = 5
                   result = newlist(n,n)
                   spiral = newlist(n,n)
                   k = 1
                   top = 1
                   bottom = n
                   left = 1
                   right = n
                   while (k <= n*n)
                           for  i= left to right
                                result[top][i] = k
                                k = k + 1
                           next
                           top = top + 1
                           for i = top to bottom
                                result[i][right] = k
                                k = k + 1
                           next
                           right = right - 1
                           for i = right to left step -1
                                result[bottom][i] = k
                                k = k + 1
                           next
                           bottom = bottom - 1
                           for i = bottom to top step -1
                                result[i][left] = k
                                k = k + 1
                           next
                           left = left + 1
                   end
                   for m = 1 to n
                        for p = 1 to n
                             spiral[p][m] = new qpushbutton(win1) {
                                                  x = 150+m*40
                                                  y = 30 + p*40
                                                  setgeometry(x,y,40,40)
                                                  settext(string(result[m][p]))
                                                  }
                        next
                   next
                   show()
                   }
                   exec()
                   }

```

Output:

[http://kepkezelo.com/images/qyooy2wqd8s502ul977v.jpg Spiral matrix]


## Ruby

{{trans|Python}}

```ruby
def spiral(n)
  spiral = Array.new(n) {Array.new(n, nil)}     # n x n array of nils
  runs = n.downto(0).each_cons(2).to_a.flatten  # n==5; [5,4,4,3,3,2,2,1,1,0]
  delta = [[1,0], [0,1], [-1,0], [0,-1]].cycle
  x, y, value = -1, 0, -1
  for run in runs
    dx, dy = delta.next
    run.times { spiral[y+=dy][x+=dx] = (value+=1) }
  end
  spiral
end

def print_matrix(m)
  width = m.flatten.map{|x| x.to_s.size}.max
  m.each {|row| puts row.map {|x| "%#{width}s " % x}.join}
end

print_matrix spiral(5)
```

{{out}}

```txt

 0  1  2  3 4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10  9 8

```


The other way
{{trans|D}}

```ruby
n = 5
m = Array.new(n){Array.new(n)}
pos, side = -1, n
for i in 0 .. (n-1)/2
  (0...side).each{|j| m[i][i+j]     = (pos+=1) }
  (1...side).each{|j| m[i+j][n-1-i] = (pos+=1) }
  side -= 2
  side.downto(0) {|j| m[n-1-i][i+j] = (pos+=1) }
  side.downto(1) {|j| m[i+j][i]     = (pos+=1) }
end

fmt = "%#{(n*n-1).to_s.size}d " * n
puts m.map{|row| fmt % row}
```


Output as above.


It processes the Array which is for work without creating it.

```ruby
def spiral_matrix(n)
  x, y, dx, dy = -1, 0, 0, -1
  fmt = "%#{(n*n-1).to_s.size}d " * n
  n.downto(1).flat_map{|x| [x, x-1]}.flat_map{|run|
    dx, dy = -dy, dx                    # turn 90
    run.times.map { [y+=dy, x+=dx] }
  }.each_with_index.sort.map(&:last).each_slice(n){|row| puts fmt % row}
end

spiral_matrix(5)
```



## Scala


```scala
class Folder(){
  var dir = (1,0)
  var pos = (-1,0)
  def apply(l:List[Int], a:Array[Array[Int]]) = {
    var (x,y) = pos  //start position
    var (dx,dy) = dir //direction
    l.foreach {e => x = x + dx; y = y + dy; a(y)(x) = e }  //copy l elements to array using current direction
    pos = (x,y)
    dir = (-dy, dx) //turn
  }
}
def spiral(n:Int) = {
  def dup(n:Int) = (1 to n).flatMap(i=>List(i,i)).toList
  val folds = n :: dup(n-1).reverse  //define fold part lengths

  var array = new Array[Array[Int]](n,n)
  val fold = new Folder()

  var seq = (0 until n*n).toList  //sequence to fold
  folds.foreach {len => fold(seq.take(len),array); seq = seq.drop(len)}
  array
}
```

Explanation: if you see the sequence of numbers to spiral around as a tape to fold around, you can see this pattern on the lenght of tape segment to fold in each step:
:<math>N,\ N-1,\ N-1,\ \ldots,\ 1,\ 1</math>
Using this the solution becomes very simple,
# make the list of lengths to fold
# create the sequence to fold
# for each segment call a fold function that keeps track of where it is and knows how to turn around.
It's simple to make this generic, changing start position, initial direction, etc.
The code could be more compact, but I'm leaving it like this for clarity.


## Scilab

{{trans|Octave}}
<lang>function a = spiral(n)
  a = ones(n*n, 1)
  v = ones(n, 1)
  u = -n*v;
  i = n
  for k = n-1:-1:1
    j = 1:k
    u(j) = -u(j)
    a(j+i) = u(j)
    v(j) = -v(j)
    a(j+(i+k)) = v(j)
    i = i+2*k
  end
  a(cumsum(a)) = (1:n*n)'
  a = matrix(a, n, n)'-1
endfunction

-->spiral(5)
 ans  =

    0.     1.     2.     3.     4.
    15.    16.    17.    18.    5.
    14.    23.    24.    19.    6.
    13.    22.    21.    20.    7.
    12.    11.    10.    9.     8.
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: matrix is array array integer;

const func matrix: spiral (in integer: n) is func
  result
    var matrix: myArray is matrix.value;
  local
    var integer: i is 0;
    var integer: dx is 1;
    var integer: dy is 0;
    var integer: x is 1;
    var integer: y is 1;
    var integer: nx is 0;
    var integer: ny is 0;
    var integer: swap is 0;
  begin
    myArray := n times n times 0;
    for i range 1 to n**2 do
      myArray[x][y] := i;
      nx := x + dx;
      ny := y + dy;
      if nx >= 1 and nx <= n and ny >= 1 and ny <= n and myArray[nx][ny] = 0 then
        x := nx;
        y := ny;
      else
        swap := dx;
        dx := -dy;
        dy := swap;
        x +:= dx;
        y +:= dy;
      end if;
    end for;
  end func;

const proc: writeMatrix (in matrix: myArray) is func
  local
    var integer: x is 0;
    var integer: y is 0;
  begin
    for key y range myArray do
      for key x range myArray[y] do
        write(myArray[x][y] lpad 4);
      end for;
      writeln;
    end for;
  end func;

const proc: main is func
  begin
    writeMatrix(spiral(5));
  end func;
```

{{out}}

```txt

   1   2   3   4   5
  16  17  18  19   6
  15  24  25  20   7
  14  23  22  21   8
  13  12  11  10   9

```



## Sidef

{{trans|Perl}}

```ruby
func spiral(n) {
    var (x, y, dx, dy, a) = (0, 0, 1, 0, [])
    { |i|
        a[y][x] = i
        var (nx, ny) = (x+dx, y+dy)
        (  if (dx ==  1 && (nx == n || a[ny][nx]!=nil)) { [ 0,  1] }
        elsif (dy ==  1 && (ny == n || a[ny][nx]!=nil)) { [-1,  0] }
        elsif (dx == -1 && (nx  < 0 || a[ny][nx]!=nil)) { [ 0, -1] }
        elsif (dy == -1 && (ny  < 0 || a[ny][nx]!=nil)) { [ 1,  0] }
        else                                            { [dx, dy] }
        ) » (\dx, \dy)
        x = x+dx
        y = y+dy
    } << (1 .. n**2)
    return a
}
 
spiral(5).each { |row|
    row.map {"%3d" % _}.join(' ').say
}
```

{{out}}

```txt

  1   2   3   4   5
 16  17  18  19   6
 15  24  25  20   7
 14  23  22  21   8
 13  12  11  10   9

```



## Stata


```stata
function spiral_mat(n) {
	a = J(n*n, 1, 1)
	u = J(n, 1, -n)
	v = J(n, 1, 1)
	for (k=(i=n)-1; k>=1; i=i+2*k--) {
		j = 1..k
		a[j:+i] = u[j] = -u[j]
		a[j:+(i+k)] = v[j] = -v[j]
	}
	return(rowshape(invorder(runningsum(a)),n):-1)
}

spiral_mat(5)
        1    2    3    4    5
    +--------------------------+
  1 |   0    1    2    3    4  |
  2 |  15   16   17   18    5  |
  3 |  14   23   24   19    6  |
  4 |  13   22   21   20    7  |
  5 |  12   11   10    9    8  |
    +--------------------------+
```



## Tcl

Using <code>print_matrix</code> from [[Matrix Transpose#Tcl]]

```tcl
package require Tcl 8.5
namespace path {::tcl::mathop}
proc spiral size {
    set m [lrepeat $size [lrepeat $size .]]
    set x 0; set dx 0
    set y -1; set dy 1
    set i -1
    while {$i < $size ** 2 - 1} {
        if {$dy == 0} {
            incr x $dx
            if {0 <= $x && $x < $size && [lindex $m $x $y] eq "."} {
                lset m $x $y [incr i]
            } else {
                # back up and change direction
                incr x [- $dx]
                set dy [- $dx]
                set dx 0
            }
        } else {
            incr y $dy
            if {0 <= $y && $y < $size && [lindex $m $x $y] eq "."} {
                lset m $x $y [incr i]
            } else {
                # back up and  change direction
                incr y [- $dy]
                set dx $dy
                set dy 0
            }
        }
    }
    return $m
}

print_matrix [spiral 5]
```


```txt
 0  1  2  3 4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10  9 8
```


=={{header|TI-83 BASIC}}==
{{trans|BBC Basic}}

```ti83b
5->N
DelVar [F]
{N,N}→dim([F])
1→A: N→B
1→C: N→D
0→E: E→G
1→I: 1→J
For(K,1,N*N)
K-1→[F](I,J)
If E=0: Then
If J<D: Then
J+1→J
Else: 1→G
I+1→I: A+1→A
End
End
If E=1: Then
If I<B: Then
I+1→I
Else: 2→G
J-1→J: D-1→D
End
End
If E=2: Then
If J>C: Then
J-1→J
Else: 3→G
I-1→I: B-1→B
End
End
If E=3: Then
If I>A: Then
I-1→I
Else: 0→G
J+1→J: C+1→C
End
End
G→E
End
[F]
```

{{out}}

```txt
[[0  1  2  3  4]
 [15 16 17 18 5]
 [14 23 24 19 6]
 [13 22 21 20 7]
 [12 11 10 9  8]]
```



## TSE SAL


```TSE SAL


// library: math: create: array: spiral: inwards <description></description> <version control></version control> <version>1.0.0.0.15</version> (filenamemacro=creamasi.s) [<Program>] [<Research>] [kn, ri, mo, 31-12-2012 01:15:43]
PROC PROCMathCreateArraySpiralInwards( INTEGER nI )
 // e.g. PROC Main()
 // e.g. STRING s1[255] = "5"
 // e.g. IF ( NOT ( Ask( "math: create: array: spiral: inwards: nI = ", s1, _EDIT_HISTORY_ ) ) AND ( Length( s1 ) > 0 ) ) RETURN() ENDIF
 // e.g.  PROCMathCreateArraySpiralInwards( Val( s1 ) )
 // e.g. END
 // e.g.
 // e.g. <F12> Main()
 //
 INTEGER columnEndI = 0
 //
 INTEGER columnBeginI = nI - 1
 //
 INTEGER rowEndI = 0
 //
 INTEGER rowBeginI = nI - 1
 //
 INTEGER columnI = 0
 //
 INTEGER rowI = 0
 //
 INTEGER minI = 0
 INTEGER maxI = nI * nI - 1
 INTEGER I = 0
 //
 INTEGER columnWidthI = Length( Str( nI * nI - 1 ) ) + 1
 //
 INTEGER directionRightI = 0
 INTEGER directionLeftI = 1
 INTEGER directionDownI = 2
 INTEGER directionUpI = 3
 //
 INTEGER directionI = directionRightI
 //
 FOR I = minI TO maxI
  //
  SetGlobalInt( Format( "MatrixS", columnI, ",", rowI ), I )
  // SetGlobalInt( Format( "MatrixS", columnI, ",", rowI ), I )
  //
  PutStrXY( ( Query( ScreenCols ) / 8 ) + columnI * columnWidthI, ( Query( ScreenRows ) / 8 ) + rowI, Str( I ), Color( BRIGHT RED ON WHITE ) )
  // PutStrXY( ( Query( ScreenCols ) / 8 ) + columnI * columnWidthI, ( Query( ScreenRows ) / 8 ) + rowI, Str( I + 1 ), Color( BRIGHT RED ON WHITE ) )
  //
  CASE directionI
   //
   WHEN directionRightI
    //
    IF ( columnI < columnBeginI )
     //
     columnI = columnI + 1
     //
    ELSE
     //
     directionI = directionDownI
     //
     rowI = rowI + 1
     //
     rowEndI = rowEndI + 1
     //
    ENDIF
    //
   WHEN directionDownI
    //
    IF ( rowI < rowBeginI )
     //
     rowI = rowI + 1
     //
    ELSE
     //
     directionI = directionLeftI
     //
     columnI = columnI - 1
     //
     columnBeginI = columnBeginI - 1
     //
    ENDIF
    //
   WHEN directionLeftI
    //
    IF ( columnI > columnEndI )
     //
     columnI = columnI - 1
     //
    ELSE
     //
     directionI = directionUpI
     //
     rowI = rowI - 1
     //
     rowBeginI = rowBeginI - 1
     //
    ENDIF
    //
   WHEN directionUpI
    //
    IF ( rowI > rowEndI )
     //
     rowI = rowI - 1
     //
    ELSE
     //
     directionI = directionRightI
     //
     columnI = columnI + 1
     //
     columnEndI = columnEndI + 1
     //
    ENDIF
    //
   OTHERWISE
    //
    Warn( Format( "PROCMathCreateArraySpiralInwards(", " ", "case", " ", ":", " ", Str( directionI ), ": not known" ) )
    //
    RETURN()
    //
  ENDCASE
  //
 ENDFOR
 //
END

PROC Main()
STRING s1[255] = "5"
IF ( NOT ( Ask( "math: create: array: spiral: inwards: nI = ", s1, _EDIT_HISTORY_ ) ) AND ( Length( s1 ) > 0 ) ) RETURN() ENDIF
 PROCMathCreateArraySpiralInwards( Val( s1 ) )
END


```



## uBasic/4tH

{{trans|C}}
This recursive version is quite compact.
<lang>Input "Width:  ";w
Input "Height: ";h
Print

For i = 0 To h-1
  For j = 0 To w-1
    Print Using "__#"; FUNC(_Spiral(w,h,j,i));
  Next
  Print
Next
End


_Spiral Param(4)
If d@ Then
  Return (a@ + FUNC(_Spiral(b@-1, a@, d@ - 1, a@ - c@ - 1)))
Else
  Return (c@)
EndIf
```


## Ursala

Helpful hints from the [[#J|J]] example are gratefully acknowledged. The spiral function works for any n, and results are shown for n equal to 5, 6, and 7. The results are represented as lists of lists rather than arrays.

```Ursala
#import std
#import nat
#import int

spiral =

^H/block nleq-<lS&r+ -+
   num@NiC+ sum:-0*yK33x+ (|\LL negation**)+ rlc ~&lh==1,
   ~&rNNXNXSPlrDlSPK32^lrtxiiNCCSLhiC5D/~& iota*+ iota+-

#cast %nLLL

examples = spiral* <5,6,7>
```

{{out}}

```txt

<
   <
      <0,1,2,3,4>,
      <15,16,17,18,5>,
      <14,23,24,19,6>,
      <13,22,21,20,7>,
      <12,11,10,9,8>>,
   <
      <0,1,2,3,4,5>,
      <19,20,21,22,23,6>,
      <18,31,32,33,24,7>,
      <17,30,35,34,25,8>,
      <16,29,28,27,26,9>,
      <15,14,13,12,11,10>>,
   <
      <0,1,2,3,4,5,6>,
      <23,24,25,26,27,28,7>,
      <22,39,40,41,42,29,8>,
      <21,38,47,48,43,30,9>,
      <20,37,46,45,44,31,10>,
      <19,36,35,34,33,32,11>,
      <18,17,16,15,14,13,12>>>
```



## VBScript

{{trans|BBC BASIC}}

```vb

Function build_spiral(n)
	botcol = 0 : topcol = n - 1
	botrow = 0 : toprow = n - 1
	'declare a two dimensional array
	Dim matrix()
	ReDim matrix(topcol,toprow)
	dir = 0 : col = 0 : row = 0
	'populate the array
	For i = 0 To n*n-1
		matrix(col,row) = i
		Select Case dir
			Case 0
				If col < topcol Then
					col = col + 1
				Else
					dir = 1 : row = row + 1 : botrow = botrow + 1
				End If
			Case 1
				If row < toprow Then
					row = row + 1
				Else
					dir = 2 : col = col - 1 : topcol = topcol - 1
				End If
			Case 2
				If col > botcol Then
					col = col - 1
				Else
					dir = 3 : row = row - 1 : toprow = toprow - 1
				End If
			Case 3
				If row > botrow Then
					row = row - 1
				Else
					dir = 0 : col = col + 1 : botcol = botcol + 1
				End If
		End Select
	Next
	'print the array
	For y = 0 To n-1
		For x = 0 To n-1
			WScript.StdOut.Write matrix(x,y) & vbTab
		Next
		WScript.StdOut.WriteLine
	Next
End Function

build_spiral(CInt(WScript.Arguments(0)))

```


{{Out}}

```txt

F:\>cscript /nologo build_spiral.vbs 5
0       1       2       3       4
15      16      17      18      5
14      23      24      19      6
13      22      21      20      7
12      11      10      9       8

F:\>cscript /nologo build_spiral.vbs 7
0       1       2       3       4       5       6
23      24      25      26      27      28      7
22      39      40      41      42      29      8
21      38      47      48      43      30      9
20      37      46      45      44      31      10
19      36      35      34      33      32      11
18      17      16      15      14      13      12

```



## Visual Basic

=
## VB6
=
{{trans|Java}}
This requires VB6.

```vb
Option Explicit

Sub Main()
    print2dArray getSpiralArray(5)
End Sub

Function getSpiralArray(dimension As Integer) As Integer()
    ReDim spiralArray(dimension - 1, dimension - 1) As Integer

    Dim numConcentricSquares As Integer
    numConcentricSquares = dimension \ 2
    If (dimension Mod 2) Then numConcentricSquares = numConcentricSquares + 1


    Dim j As Integer, sideLen As Integer, currNum As Integer
    sideLen = dimension

    Dim i As Integer
    For i = 0 To numConcentricSquares - 1
        ' do top side
        For j = 0 To sideLen - 1
            spiralArray(i, i + j) = currNum
            currNum = currNum + 1
        Next

        ' do right side
        For j = 1 To sideLen - 1
            spiralArray(i + j, dimension - 1 - i) = currNum
            currNum = currNum + 1
        Next

        ' do bottom side
        For j = sideLen - 2 To 0 Step -1
            spiralArray(dimension - 1 - i, i + j) = currNum
            currNum = currNum + 1
        Next

        ' do left side
        For j = sideLen - 2 To 1 Step -1
            spiralArray(i + j, i) = currNum
            currNum = currNum + 1
        Next

        sideLen = sideLen - 2
    Next

    getSpiralArray = spiralArray()
End Function

Sub print2dArray(arr() As Integer)
    Dim row As Integer, col As Integer
    For row = 0 To UBound(arr, 1)
        For col = 0 To UBound(arr, 2) - 1
            Debug.Print arr(row, col),
        Next
        Debug.Print arr(row, UBound(arr, 2))
    Next
End Sub
```


=
## VBA
=

### =Solution 1=

{{trans|Java}}
{{works with|VBA/Excel}}

```vb
Sub spiral()
    Dim n As Integer, a As Integer, b As Integer
    Dim numCsquares As Integer, sideLen As Integer, currNum As Integer
    Dim j As Integer, i As Integer
    Dim j1 As Integer, j2 As Integer, j3 As Integer

    n = 5

    Dim spiralArr(9, 9) As Integer
    numCsquares = CInt(Application.WorksheetFunction.Ceiling(n / 2, 1))
    sideLen = n
    currNum = 0
    For i = 0 To numCsquares - 1
        'do top side
        For j = 0 To sideLen - 1
            currNum = currNum + 1
            spiralArr(i, i + j) = currNum
        Next j

        'do right side
        For j1 = 1 To sideLen - 1
            currNum = currNum + 1
            spiralArr(i + j1, n - 1 - i) = currNum
        Next j1

        'do bottom side
        j2 = sideLen - 2
        Do While j2 > -1
            currNum = currNum + 1
            spiralArr(n - 1 - i, i + j2) = currNum
            j2 = j2 - 1
        Loop

        'do left side
        j3 = sideLen - 2
        Do While j3 > 0
            currNum = currNum + 1
            spiralArr(i + j3, i) = currNum
            j3 = j3 - 1
        Loop

        sideLen = sideLen - 2
    Next i

    For a = 0 To n - 1
        For b = 0 To n - 1
        Cells(a + 1, b + 1).Select
            ActiveCell.Value = spiralArr(a, b)
        Next b
    Next a
End Sub
```



### =Solution 2=


```vb
Sub spiral(n As Integer)
  Const FREE = -9        'negative number indicates unoccupied cell
  Dim A() As Integer
  Dim rowdelta(3) As Integer
  Dim coldelta(3) As Integer

  'initialize A to a matrix with an extra "border" of occupied cells
  'this avoids having to test if we've reached the edge of the matrix

  ReDim A(0 To n + 1, 0 To n + 1)

  'Since A is initialized with zeros, setting A(1 to n,1 to n) to "FREE"
  'leaves a "border" around it occupied with zeroes

  For i = 1 To n: For j = 1 To n: A(i, j) = FREE: Next: Next

  'set amount to move in directions "right", "down", "left", "up"

  rowdelta(0) = 0: coldelta(0) = 1
  rowdelta(1) = 1: coldelta(1) = 0
  rowdelta(2) = 0: coldelta(2) = -1
  rowdelta(3) = -1: coldelta(3) = 0

  curnum = 0

  'set current cell position
  col = 1
  row = 1

  'set current direction
  theDir = 0  'theDir = 1 will fill the matrix counterclockwise

  'ok will be true as long as there is a free cell left
  ok = True

  Do While ok

     'occupy current FREE cell and increase curnum
      A(row, col) = curnum
      curnum = curnum + 1

      'check if next cell in current direction is free
      'if not, try another direction in clockwise fashion
      'if all directions lead to occupied cells then we are finished!

      ok = False
      For i = 0 To 3
        newdir = (theDir + i) Mod 4
        If A(row + rowdelta(newdir), col + coldelta(newdir)) = FREE Then
          'yes, move to it and change direction if necessary
          theDir = newdir
          row = row + rowdelta(theDir)
          col = col + coldelta(theDir)
          ok = True
          Exit For
        End If
      Next i
  Loop

  'print result
  For i = 1 To n
    For j = 1 To n
      Debug.Print A(i, j),
    Next
    Debug.Print
  Next

End Sub
```

{{out}}

```txt

spiral 5
 0             1             2             3             4
 15            16            17            18            5
 14            23            24            19            6
 13            22            21            20            7
 12            11            10            9             8

spiral 6
 0             1             2             3             4             5
 19            20            21            22            23            6
 18            31            32            33            24            7
 17            30            35            34            25            8
 16            29            28            27            26            9
 15            14            13            12            11            10

```


=
## Visual Basic .NET
=
'''Platform:''' [[.NET]]

From VB6. This requires Visual Basic .Net.

```vbnet
Module modSpiralArray
    Sub Main()
        print2dArray(getSpiralArray(5))
    End Sub

    Function getSpiralArray(dimension As Integer) As Object
        Dim spiralArray(,) As Integer
        Dim numConcentricSquares As Integer

        ReDim spiralArray(dimension - 1, dimension - 1)
        numConcentricSquares = dimension \ 2
        If (dimension Mod 2) Then numConcentricSquares = numConcentricSquares + 1

        Dim j As Integer, sideLen As Integer, currNum As Integer
        sideLen = dimension

        Dim i As Integer
        For i = 0 To numConcentricSquares - 1
            ' do top side
            For j = 0 To sideLen - 1
                spiralArray(i, i + j) = currNum
                currNum = currNum + 1
            Next
            ' do right side
            For j = 1 To sideLen - 1
                spiralArray(i + j, dimension - 1 - i) = currNum
                currNum = currNum + 1
            Next
            ' do bottom side
            For j = sideLen - 2 To 0 Step -1
                spiralArray(dimension - 1 - i, i + j) = currNum
                currNum = currNum + 1
            Next
            ' do left side
            For j = sideLen - 2 To 1 Step -1
                spiralArray(i + j, i) = currNum
                currNum = currNum + 1
            Next
            sideLen = sideLen - 2
        Next
        getSpiralArray = spiralArray
    End Function

    Sub print2dArray(arr)
        Dim row As Integer, col As Integer, s As String
        For row = 0 To UBound(arr, 1)
            s = ""
            For col = 0 To UBound(arr, 2)
                s = s & " " & Right("  " & arr(row, col), 3)
            Next
            Debug.Print(s)
        Next
    End Sub

End Module

```



## XPL0


```XPL0
def N=5;
int A(N,N);
int I, J, X, Y, Steps, Dir;
include c:\cxpl\codes;
[Clear;
I:= 0;  X:= -1;  Y:= 0;  Steps:= N;  Dir:= 0;
repeat  for J:= 1 to Steps do
                [case Dir&3 of
                  0: X:= X+1;
                  1: Y:= Y+1;
                  2: X:= X-1;
                  3: Y:= Y-1
                other [];
                A(X,Y):= I;
                Cursor(X*3,Y);  IntOut(0,I);
                I:= I+1;
                ];
        Dir:= Dir+1;
        if Dir&1 then Steps:= Steps-1;
until   Steps = 0;
Cursor(0,N);
]
```


{{out}}

```txt

0  1  2  3  4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10 9  8

```



## zkl

{{trans|C}} which turns out to be the same as Ruby

```zkl
fcn spiralMatrix(n){
   sm:=(0).pump(n,List,(0).pump(n,List,False).copy); //L(L(False,False..), L(F,F,..) ...)
   drc:=Walker.cycle(T(0,1,0), T(1,0,1), T(0,-1,0), T(-1,0,1)); // deltas
   len:=n; r:=0; c:=-1; z:=-1; while(len>0){ //or do(2*n-1){
      dr,dc,dl:=drc.next();
      do(len-=dl){ sm[r+=dr][c+=dc]=(z+=1); }
   }
   sm
}
```


```zkl
foreach n in (T(5,-1,0,1,2)){
   spiralMatrix(n).pump(Console.println,fcn(r){ r.apply("%4d".fmt).concat() });
   println("---");
}
```

{{out}}

```txt

   0   1   2   3   4
  15  16  17  18   5
  14  23  24  19   6
  13  22  21  20   7
  12  11  10   9   8
---
---
---
   0
---
   0   1
   3   2
---

```


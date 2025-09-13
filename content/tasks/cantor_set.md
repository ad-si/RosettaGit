+++
title = "Cantor set"
description = ""
date = 2019-08-24T23:05:23Z
aliases = []
[extra]
id = 21783
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Draw a Cantor set.
See details: [https://en.wikipedia.org/wiki/Cantor_set Cantor set]



## 11l

{{trans|Python}}

```11l
V WIDTH = 81
V HEIGHT = 5

F cantor(start, len, index)
   V seg = len I/ 3
   I seg == 0
      R
   L(it) 0 .< :HEIGHT - index
      V i = index + it
      L(jt) 0 .< seg
         V j = start + seg + jt
         V pos = i * :WIDTH + j
         :lines[pos] = ‘ ’
   cantor(start, seg, index + 1)
   cantor(start + seg * 2, seg, index + 1)

V lines = [‘*’] * (WIDTH * HEIGHT)
cantor(0, WIDTH, 1)

L(i) 0 .< HEIGHT
   V beg = WIDTH * i
   print((lines[beg .< beg + WIDTH]).join(‘’))
```

{{out}}

```txt

*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *

```



## ALGOL 68


```algol68
BEGIN
    # draw a Cantor Set using ASCII                                            #
    INT    lines     = 5; # number of lines for the set                        #
    # we must choose the line width so that the width of each segment is       #
    # divisible by 3 ( except for the final line where the segment width will  #
    # be 1 )                                                                   #
    INT    set width = 3 ^ ( lines - 1 );
    [ set width ]CHAR set;
    # start with a complete line #
    FOR i TO set width DO set[ i ] := "#" OD;
    print( ( set, newline ) );
    # repeatedly modify the line, replacing the middle third of each segment   #
    # with blanks                                                              #
    INT   segment width := set width OVER 3;
    WHILE segment width > 0 DO
        INT   set pos := 1;
        WHILE set pos < ( set width - segment width ) DO
            set pos   +:= segment width;
            FOR char pos FROM set pos TO ( set pos + segment width ) - 1 DO
                set[ char pos ] := " "
            OD;
            set pos +:= segment width
        OD;
        print( ( set, newline ) );
        segment width OVERAB 3
    OD
END
```

{{out}}

```txt

#################################################################################
###########################                           ###########################
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #

```



## ALGOL W

Based on the Algol 68 sample.

```algolw
begin
    % draw a Cantor Set using ASCII                                            %
    integer LINES;        % number of lines for the set                        %
    integer setWidth;     % width of each line of the set                      %
    % we must choose the line width so that the width of each segment is       %
    % divisible by 3 ( except for the final line where the segment width will  %
    % be 1 )                                                                   %
    LINES    := 5;
    setWidth := round( 3 ** ( LINES - 1 ) );
    begin % start new block so the array can have computed bounds              %
        logical array set ( 1 :: setWidth );
        integer segmentWidth;
        % start with a complete line %
        for i := 1 until setWidth do set( i ) := true;
        segmentWidth := setWidth;
        for l := 1 until LINES do begin
            % print the latest line, all lines start with a "#"                %
            write( "#" );
            for i := 2 until setWidth do writeon( if set( i ) then "#" else " " );
            % modify the line, replacing the middle third of each segment      %
            % with blanks, unless this was the last line                       %
            if l < LINES then begin
                integer   setPos;
                segmentWidth := segmentWidth div 3;
                setPos := 1;
                while setPos < ( setWidth - segmentWidth ) do begin
                    setPos := setPos + segmentWidth;
                    for charPos := setPos until ( setPos + segmentWidth ) - 1 do set( charPos ) := false;
                    setPos := setPos + segmentWidth
                end while_setPos_in_range ;
            end if_l_lt_LINES
        end for_l
    end
end.
```

{{out}}

```txt

#################################################################################
###########################                           ###########################
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #

```


## AppleScript


```applescript
-- cantor :: [String] -> [String]
on cantor(xs)
    script go
        on |λ|(s)
            set m to (length of s) div 3
            set blocks to text 1 thru m of s

            if "█" = text 1 of s then
                {blocks, replicate(m, space), blocks}
            else
                {s}
            end if
        end |λ|
    end script
    concatMap(go, xs)
end cantor


-- showCantor :: Int -> String
on showCantor(n)
    unlines(map(my concat, ¬
        take(n, iterate(cantor, ¬
            {replicate(3 ^ (n - 1), "█")}))))
end showCantor


-- TEST ---------------------------------------------------
on run
    showCantor(5)
end run


-- GENERIC ------------------------------------------------

-- concat :: [[a]] -> [a]
-- concat :: [String] -> String
on concat(xs)
    set lng to length of xs
    if 0 < lng and string is class of (item 1 of xs) then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to lng
        set acc to acc & item i of xs
    end repeat
    acc
end concat

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    set acc to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & |λ|(item i of xs, i, xs)
        end repeat
    end tell
    return acc
end concatMap

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

-- iterate :: (a -> a) -> a -> Gen [a]
on iterate(f, x)
    script
        property v : missing value
        property g : mReturn(f)'s |λ|
        on |λ|()
            if missing value is v then
                set v to x
            else
                set v to g(v)
            end if
            return v
        end |λ|
    end script
end iterate


-- replicate :: Int -> String -> String
on replicate(n, s)
    set out to ""
    if n < 1 then return out
    set dbl to s

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate


-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to xs's |λ|()
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines
```

{{Out}}

```txt
█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █
```



## AWK


```AWK

# syntax: GAWK -f CANTOR_SET.AWK
# converted from C
BEGIN {
    WIDTH = 81
    HEIGHT = 5
    for (i=0; i<HEIGHT; ++i) {
      for (j=0; j<WIDTH; ++j) {
        lines[i][j] = "*"
      }
    }
    cantor(0,WIDTH,1)
    for (i=0; i<HEIGHT; ++i) {
      for (j=0; j<WIDTH; ++j) {
        printf("%s",lines[i][j])
      }
      printf("\n")
    }
    exit(0)
}
function cantor(start,leng,indx,  i,j,seg) {
    seg = int(leng/3)
    if (seg == 0) { return }
    for (i=indx; i<HEIGHT; ++i) {
      for (j=start+seg; j<start+seg*2; ++j) {
        lines[i][j] = " "
      }
    }
    cantor(start,seg,indx+1)
    cantor(start+seg*2,seg,indx+1)
}

```

{{out}}

```txt

*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *

```



## BASIC256

{{trans|FreeBASIC}}

```BASIC256

global ancho, alto, intervalo
ancho = 81 : alto = 5
dim intervalo(alto, ancho)

subroutine Cantor()
	for i = 0 to alto - 1
		for j = 0 to ancho - 1
			intervalo[i, j] = "■"
		next j
	next i
end subroutine

subroutine ConjCantor(inicio, longitud, indice)
	segmento = longitud / 3
	if segmento = 0 then return
	for i = indice to alto - 1
		for j = inicio + segmento to inicio + segmento * 2 - 1
			intervalo[i, j] = " "
		next j
	next i
	call ConjCantor(inicio, segmento, indice + 1)
	call ConjCantor(inicio + segmento * 2, segmento, indice + 1)
end subroutine

call Cantor()
call ConjCantor(0, ancho, 1)
for i = 0 to alto - 1
	for j = 0 to ancho - 1
		print intervalo[i, j];
	next j
	print
next i
End

```

{{out}}

```txt

Igual que la entrada de FreeBASIC.

```



## C

Translated from Kotlin.

```c
#include <stdio.h>

#define WIDTH 81
#define HEIGHT 5

char lines[HEIGHT][WIDTH];

void init() {
    int i, j;
    for (i = 0; i < HEIGHT; ++i) {
        for (j = 0; j < WIDTH; ++j) lines[i][j] = '*';
    }
}

void cantor(int start, int len, int index) {
    int i, j, seg = len / 3;
    if (seg == 0) return;
    for (i = index; i < HEIGHT; ++i) {
        for (j = start + seg; j < start + seg * 2; ++j) lines[i][j] = ' ';
    }
    cantor(start, seg, index + 1);
    cantor(start + seg * 2, seg, index + 1);
}

void print() {
    int i, j;
    for (i = 0; i < HEIGHT; ++i) {
        for (j = 0; j < WIDTH; ++j) printf("%c", lines[i][j]);
        printf("\n");
    }
}

int main() {
    init();
    cantor(0, WIDTH, 1);
    print();
    return 0;
}
```


{{output}}

```txt

*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *

```



## C++

Translated from D.

```cpp
#include <iostream>

const int WIDTH = 81;
const int HEIGHT = 5;

char lines[WIDTH*HEIGHT];

void cantor(int start, int len, int index) {
	int seg = len / 3;
	if (seg == 0) return;
	for (int i = index; i < HEIGHT; i++) {
		for (int j = start + seg; j < start + seg * 2; j++) {
			int pos = i * WIDTH + j;
			lines[pos] = ' ';
		}
	}
	cantor(start,           seg, index + 1);
	cantor(start + 2 * seg, seg, index + 1);
}

int main() {
	// init
	for (int i = 0; i < WIDTH*HEIGHT; i++) {
		lines[i] = '*';
	}

	// calculate
	cantor(0, WIDTH, 1);

	// print
	for (int i = 0; i < HEIGHT*WIDTH; i += WIDTH) {
		printf("%.*s\n", WIDTH, lines + i);
	}

	return 0;
}
```

{{out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```


## C#

Translated from Java.

```c#
using System;

namespace CantorSet {
    class Program {
        const int WIDTH = 81;
        const int HEIGHT = 5;
        private static char[,] lines = new char[HEIGHT, WIDTH];

        static Program() {
            for (int i = 0; i < HEIGHT; i++) {
                for (int j = 0; j < WIDTH; j++) {
                    lines[i, j] = '*';
                }
            }
        }

        private static void Cantor(int start, int len, int index) {
            int seg = len / 3;
            if (seg == 0) return;
            for (int i = index; i < HEIGHT; i++) {
                for (int j = start + seg; j < start + seg * 2; j++) {
                    lines[i, j] = ' ';
                }
            }
            Cantor(start, seg, index + 1);
            Cantor(start + seg * 2, seg, index + 1);
        }

        static void Main(string[] args) {
            Cantor(0, WIDTH, 1);
            for (int i = 0; i < HEIGHT; i++) {
                for (int j = 0; j < WIDTH; j++) {
                    Console.Write(lines[i,j]);
                }
                Console.WriteLine();
            }
        }
    }
}
```

{{out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```



## D

{{trans|C}}

```d
import std.stdio;

enum WIDTH = 81;
enum HEIGHT = 5;

char[WIDTH*HEIGHT] lines;

void cantor(int start, int len, int index) {
    int seg = len / 3;
    if (seg == 0) return;
    for (int i=index; i<HEIGHT; i++) {
        for (int j=start+seg; j<start+seg*2; j++) {
            int pos = i*WIDTH + j;
            lines[pos] = ' ';
        }
    }
    cantor(start, seg, index+1);
    cantor(start+seg*2, seg, index+1);
}

void main() {
    // init
    lines[] = '*';

    // calculate
    cantor(0, WIDTH, 1);

    // print
    for (int i=0; i<HEIGHT; i++) {
        int beg = WIDTH * i;
        writeln(lines[beg..beg+WIDTH]);
    }
}
```

{{out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```



## Factor


```factor
USING: grouping.extras io kernel math sequences
sequences.repeating ;
IN: rosetta-code.cantor-set

CONSTANT: width 81
CONSTANT: depth 5

: cantor ( n -- seq )
    dup 0 = [ drop { 0 1 } ]
    [ 1 - cantor [ 3 / ] map dup [ 2/3 + ] map append ] if ;

! Produces a sequence of lengths from a Cantor set, depending on
! width. Even indices are solid; odd indices are blank.
! e.g. 2 cantor gaps -> { 9 9 9 27 9 9 9 }
!
: gaps ( seq -- seq )
    [ width * ] map [ - abs ] 2clump-map ;

: print-cantor ( n -- )
    cantor gaps [ even? "#" " " ? swap repeat ] map-index
    concat print ;

depth <iota> [ print-cantor ] each
```

{{out}}

```txt

#################################################################################
###########################                           ###########################
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #

```



## Forth

Where is says <code>[email protected]</code> it should say <code>c&#64;</code>, but I'm not keen on writing it as <code>c&amp;#64;</code> in the actual code.

```Forth
warnings off

4  \ iterations
: **        1 swap  0 ?DO over * LOOP  nip ;
3 swap **  constant width  \ Make smallest step 1

create string  here width char # fill  width allot
: print     string width type cr ;

\  Overwrite string with new holes of size 'length'.
\  Pointer into string at TOS.
create length  width ,
: reduce    length dup @ 3 / swap ! ;
: done?     dup string - width >= ;
: hole?     dup c@ bl = ;
: skip      length @ + ;
: whipe     dup length @ bl fill  skip ;
: step      hole? IF skip skip skip ELSE skip whipe skip THEN ;
: split     reduce string BEGIN step done? UNTIL drop ;

\  Main
: done?     length @ 1 <= ;
: step      split print ;
: go        print BEGIN step done? UNTIL ;

go bye
```

Output:

```txt
#################################################################################
###########################                           ###########################
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #
```




## FreeBASIC


```freebasic

Const ancho = 81
Const alto = 5
Dim Shared intervalo(alto, ancho) As String
Dim As Integer i, j

Sub Cantor()
    Dim As Integer i, j
    For i = 0 To alto - 1
        For j = 0 To ancho - 1
            intervalo(i, j) = Chr(254)
        Next j
    Next i
End Sub

Sub ConjCantor(inicio As Integer, longitud As Integer, indice As Integer)
    Dim As Integer i, j
    Dim segmento As Integer = longitud / 3
    If segmento = 0 Then Return
    For i = indice To alto - 1
        For j = inicio + segmento To inicio + segmento * 2 - 1
            intervalo(i, j) = Chr(32)
        Next j
    Next i
    ConjCantor(inicio, segmento, indice + 1)
    ConjCantor(inicio + segmento * 2, segmento, indice + 1)
End Sub

Cantor()
ConjCantor(0, ancho, 1)
For i = 0 To alto - 1
    For j = 0 To ancho - 1
        Print intervalo(i, j);
    Next j
    Print
Next i
End

```

{{out}}

```txt

█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █

```



## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

const (
    width = 81
    height = 5
)

var lines [height][width]byte

func init() {
    for i := 0; i < height; i++ {
        for j := 0; j < width; j++ {
            lines[i][j] = '*'
        }
    }
}

func cantor(start, len, index int) {
    seg := len / 3
    if seg == 0 {
        return
    }
    for i := index; i < height; i++ {
        for j := start + seg; j < start + 2 * seg; j++ {
            lines[i][j] = ' '
        }
    }
    cantor(start, seg, index + 1)
    cantor(start + seg * 2, seg, index + 1)
}

func main() {
    cantor(0, width, 1)
    for _, line := range lines {
        fmt.Println(string(line[:]))
    }
}
```


{{out}}

```txt

*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *

```



## Haskell


### Interval bars

{{Trans|Python}} (Functional version)

```haskell
import Data.Bool (bool)

cantor :: [(Bool, Int)] -> [(Bool, Int)]
cantor = (go =<<)
  where
    go (bln, n) =
      let m = quot n 3
      in bool [(bln, n)] [(True, m), (False, m), (True, m)] (bln && 1 < n)

cantorLines :: Int -> String
cantorLines n =
  unlines $ showCantor <$> take n (iterate cantor [(True, 3 ^ (n - 1))])

showCantor :: [(Bool, Int)] -> String
showCantor = (uncurry (flip replicate . bool ' ' '*') =<<)

main :: IO ()
main = putStrLn $ cantorLines 5
```

{{Out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```


Or, using strings for the model as well as the display:


```haskell
import Data.Bool (bool)

cantor :: [String] -> [String]
cantor = (go =<<)
  where
    go x =
      let m = quot (length x) 3
          block = take m x
      in bool [x] [block, replicate m ' ', block] ('█' == head x)

cantorLines :: Int -> String
cantorLines n =
  unlines $ concat <$> take n (iterate cantor [replicate (3 ^ (n - 1)) '█'])

main :: IO ()
main = putStrLn $ cantorLines 5
```

{{Out}}

```txt
█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █
```



### Dual representation

Intervals as fraction pairs, and intervals as graphic bars:

```haskell
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.List (intercalate, mapAccumL, maximumBy)
import Data.Bool (bool)

cantor :: (Rational, Rational) -> [[(Rational, Rational)]]
cantor =
  let go (x, y) =
        let r = (y - x) / 3
        in [(x, x + r), (y - r, y)]
  in iterate (>>= go) . return

main :: IO ()
main = do
  let xs = take 4 $ cantor (0, 1)
  (putStrLn . unlines) $ intervalRatios <$> xs
  putStrLn $ intervalBars xs

-- DISPLAY FUNCTIONS ---------------------------------------------------
intervalBars :: [[(Rational, Rational)]] -> String
intervalBars xs =
  let go w xs =
        concat . snd $
        mapAccumL
          (\a (rx, ry) ->
              let (wx, wy) = (w * rx, w * ry)
              in ( wy -- Accumulator – end of previous interval.
                 , replicate (floor (wx - a)) ' ' -- Preceding gap, and
                    ++
                   replicate (floor (wy - wx)) '█' -- interval bar.
                  ))
          0
          xs
      d = maximum $ (denominator . fst) <$> last xs
  in unlines $ go (d % 1) <$> xs

intervalRatios :: [(Rational, Rational)] -> String
intervalRatios xs =
  let go (rx, ry) = intercalate ", " $ showRatio <$> [rx, ry]
  in '(' : intercalate ") (" (go <$> xs) ++ ")"

showRatio :: Rational -> String
showRatio r =
  let d = denominator r
  in show (numerator r) ++ bool [] ('/' : show d) (1 /= d)
```

{{Out}}

```txt
(0, 1)
(0, 1/3) (2/3, 1)
(0, 1/9) (2/9, 1/3) (2/3, 7/9) (8/9, 1)
(0, 1/27) (2/27, 1/9) (2/9, 7/27) (8/27, 1/3) (2/3, 19/27) (20/27, 7/9) (8/9, 25/27) (26/27, 1)

███████████████████████████
█████████         █████████
███   ███         ███   ███
█ █   █ █         █ █   █ █
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Cantor.bas"
110 GRAPHICS HIRES 2
120 SET PALETTE BLACK,WHITE
130 CALL CANTOR(28,500,1216,32)
140 DEF CANTOR(X,Y,L,HEIGHT)
150   IF L>3 THEN
160     PLOT X,Y;X+L,Y,X,Y+4;X+L,Y+4
170     CALL CANTOR(X,Y-HEIGHT,L/3,HEIGHT)
180     CALL CANTOR(X+2*L/3,Y-HEIGHT,L/3,HEIGHT)
190   END IF
200 END DEF
```



## J

The argument to the cantor_dust monad is an integer that describes the depth of the dust.  Shown here are results for cantor_dust 2  and for  cantor_dust 3 .  It works by checking for 1 digits in the base 3 representation of the coordinates.  These background coordinates are plotted with # character using ASCII art.  1j1 #"1 expands the lines to improve aspect ratio on character cell (console) display.  }:"1 curtails the extra space character line by line.  < draws a pretty box.

```J

odometer =: [: (4 $. $.) $&1

cantor_dust =: monad define
 shape =. ,~ 3 ^ y
 a =. shape $ ' '
 i =. odometer shape
 < (}:"1) 1j1 #"1 '#' (([: <"1 [: ;/"1 (#~ 1 e."1 [: (,/"2) 3 3&#:)) i)}a
)

```



```txt

   cantor_dust 2
┌─────────────────┐
│  #   # # #   #  │
│# # # # # # # # #│
│  #   # # #   #  │
│# # # # # # # # #│
│# # # # # # # # #│
│# # # # # # # # #│
│  #   # # #   #  │
│# # # # # # # # #│
│  #   # # #   #  │
└─────────────────┘

   cantor_dust 3
┌─────────────────────────────────────────────────────┐
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
│# # # # # # # # # # # # # # # # # # # # # # # # # # #│
│  #   # # #   #     #   # # #   #     #   # # #   #  │
└─────────────────────────────────────────────────────┘


```

With an `x' argument cantor_dust generalizes to higher dimensions.  Try  3 cantor_dust 2

```J

cantor_dust =: 2&$: :(dyad define)
 shape =. x # 3 ^ y
 a =. shape $ ' '
 i =. odometer shape
 < (}:"1) 1j1 #"1 '#' (([: <"1 [: ;/"1 (#~ 1 e."1 [: (,/"2) 3 3&#:)) i)} a
)

```



## Java

{{trans|Kotlin}}

```java
public class App {
    private static final int WIDTH = 81;
    private static final int HEIGHT = 5;

    private static char[][] lines;
    static {
        lines = new char[HEIGHT][WIDTH];
        for (int i = 0; i < HEIGHT; i++) {
            for (int j = 0; j < WIDTH; j++) {
                lines[i][j] = '*';
            }
        }
    }

    private static void cantor(int start, int len, int index) {
        int seg = len / 3;
        if (seg == 0) return;
        for (int i = index; i < HEIGHT; i++) {
            for (int j = start + seg; j < start + seg * 2; j++) {
                lines[i][j] = ' ';
            }
        }
        cantor(start, seg, index + 1);
        cantor(start + seg * 2, seg, index + 1);
    }

    public static void main(String[] args) {
        cantor(0, WIDTH, 1);
        for (int i = 0; i < HEIGHT; i++) {
            for (int j = 0; j < WIDTH; j++) {
                System.out.print(lines[i][j]);
            }
            System.out.println();
        }
    }
}

```

{{out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```



## JavaScript

{{Trans|Python}} (Functional version)
{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    const main = () => {

        // cantor :: [(Bool, Int)] -> [(Bool, Int)]
        const cantor = xs => {
            const go = ([bln, n]) =>
                bln && 1 < n ? (() => {
                    const lng = Math.floor(n / 3);
                    return [
                        [true, lng],
                        [false, lng],
                        [true, lng]
                    ]
                })() : [
                    [bln, n]
                ];
            return concatMap(go, xs);
        };

        // cantorLines :: Int -> String
        const cantorLines = n =>
            unlines(
                map(showCantor,
                    take(n,
                        iterate(
                            cantor,
                            [
                                [true, Math.pow(3, n - 1)]
                            ]
                        )
                    )
                )
            );

        // showCantor :: [(Bool, Int)] -> String
        const showCantor = xs =>
            concat(map(
                ([bln, n]) => replicate(n, bln ? '*' : ' '), xs
            ));

        console.log(
            cantorLines(5)
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // iterate :: (a -> a) -> a -> Gen [a]
    function* iterate(f, x) {
        let v = x;
        while (true) {
            yield(v);
            v = f(v);
        }
    }

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // replicate :: Int -> String -> String
    const replicate = (n, s) => s.repeat(n);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```


Or, using strings for the model as well as the display:
{{Trans|Haskell}}

```javascript
(() => {
    'use strict';

    const main = () => showCantor(5);

    // showCantor :: Int -> String
    const showCantor = n =>
        unlines(map(concat,
            take(n,
                iterate(
                    cantor,
                    [replicate(Math.pow(3, n - 1), '█')]
                )
            )
        ));

    // cantor :: [String] -> [String]
    const cantor = xs => {
        const go = s => {
            const
                m = Math.floor(s.length / 3),
                blocks = take(m, s);
            return '█' === s[0] ? (
                [blocks, replicate(m, ' '), blocks]
            ) : [s];
        };
        return concatMap(go, xs);
    };

    // GENERIC FUNCTIONS ----------------------------

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // iterate :: (a -> a) -> a -> Gen [a]
    function* iterate(f, x) {
        let v = x;
        while (true) {
            yield(v);
            v = f(v);
        }
    }

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // replicate :: Int -> String -> String
    const replicate = (n, s) => s.repeat(n);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █
```



### Dual representation

{{Trans|Haskell}}
{{Trans|Python}}


Cantor ternary intervals rendered both as lists of  ratio pairs, and as graphic bars.
In the case of languages like Javascript which lack a built-in Ratio type, or standard Fraction/Ratio library, rendering stages of the set elaboration as lists of fraction pairs may take more work than rendering them as graphic lines.


```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {

        const xs = take(4, iterate(cantor, [Tuple(0, 1)]));

        console.log(
            unlines(map(intervalRatios, xs)) + '\n',
        );
        console.log(
            intervalBars(xs)
        );
    };

    // cantor :: [(Rational, Rational)] -> [(Rational, Rational)]
    const cantor = xs => {
        const go = tpl => {
            const [r1, r2] = map(rational, Array.from(tpl));
            const third = ratioDiv(ratioMinus(r2, r1), 3);
            return [
                Tuple(r1, ratioPlus(r1, third)),
                Tuple(ratioMinus(r2, third), r2)
            ];
        };
        return concatMap(go, xs);
    };

    // intervalRatios :: [(Rational, Rational)] -> String
    const intervalRatios = xs => {
        const go = tpl =>
            map(compose(showRatio, rational),
                Array.from(tpl)
            ).join(', ');
        return '(' + map(go, xs).join(') (') + ')';
    };

    // intervalBars :: [[(Rational, Rational)]] -> String
    const intervalBars = xs => {
        const go = w => xs =>
            concat(snd(mapAccumL(
                (a, tpl) => {
                    const [wx, wy] = map(
                        r => ratioMult(w, rational(r)),
                        Array.from(tpl)
                    );
                    return Tuple(
                        wy,
                        replicateString(floor(ratioMinus(wx, a)), ' ') +
                        replicateString(floor(ratioMinus(wy, wx)), '█')
                    );
                },
                0, xs
            )));
        const d = maximum(map(x => fst(x).d, last(xs)));
        return unlines(map(go(ratio(d, 1)), xs));
    };


    // GENERIC FUNCTIONS ----------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // abs :: Num -> Num
    const abs = Math.abs;

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // floor :: Num -> Int
    const floor = x => {
        const
            nr = (
                'Ratio' !== x.type ? (
                    properFraction
                ) : properFracRatio
            )(x),
            n = nr[0];
        return 0 > nr[1] ? n - 1 : n;
    };

    // foldl1 :: (a -> a -> a) -> [a] -> a
    const foldl1 = (f, xs) =>
        1 < xs.length ? xs.slice(1)
        .reduce(f, xs[0]) : xs[0];

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // gcd :: Int -> Int -> Int
    const gcd = (x, y) => {
        const
            _gcd = (a, b) => (0 === b ? a : _gcd(b, a % b)),
            abs = Math.abs;
        return _gcd(abs(x), abs(y));
    };

    // iterate :: (a -> a) -> a -> Gen [a]
    function* iterate(f, x) {
        let v = x;
        while (true) {
            yield(v);
            v = f(v);
        }
    }

    // last :: [a] -> a
    const last = xs =>
        0 < xs.length ? xs.slice(-1)[0] : undefined;

    // lcm :: Int -> Int -> Int
    const lcm = (x, y) =>
        (x === 0 || y === 0) ? 0 : Math.abs(Math.floor(x / gcd(x, y)) * y);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // 'The mapAccumL function behaves like a combination of map and foldl;
    // it applies a function to each element of a list, passing an accumulating
    // parameter from left to right, and returning a final value of this
    // accumulator together with the new list.' (See Hoogle)

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    const mapAccumL = (f, acc, xs) =>
        xs.reduce((a, x, i) => {
            const pair = f(a[0], x, i);
            return Tuple(pair[0], a[1].concat(pair[1]));
        }, Tuple(acc, []));

    // maximum :: Ord a => [a] -> a
    const maximum = xs =>
        0 < xs.length ? (
            foldl1((a, x) => x > a ? x : a, xs)
        ) : undefined;

    // properFracRatio :: Ratio -> (Int, Ratio)
    const properFracRatio = nd => {
        const [q, r] = Array.from(quotRem(nd.n, nd.d));
        return Tuple(q, ratio(r, nd.d));
    };

    // properFraction :: Real -> (Int, Real)
    const properFraction = n => {
        const i = Math.floor(n) + (n < 0 ? 1 : 0);
        return Tuple(i, n - i);
    };

    // quot :: Int -> Int -> Int
    const quot = (n, m) => Math.floor(n / m);

    // quotRem :: Int -> Int -> (Int, Int)
    const quotRem = (m, n) =>
        Tuple(Math.floor(m / n), m % n);

    // ratio :: Int -> Int -> Ratio Int
    const ratio = (x, y) => {
        const go = (x, y) =>
            0 !== y ? (() => {
                const d = gcd(x, y);
                return {
                    type: 'Ratio',
                    'n': quot(x, d), // numerator
                    'd': quot(y, d) // denominator
                };
            })() : undefined;
        return go(x * signum(y), abs(y));
    };

    // ratioDiv :: Rational -> Rational -> Rational
    const ratioDiv = (n1, n2) => {
        const [r1, r2] = map(rational, [n1, n2]);
        return ratio(r1.n * r2.d, r1.d * r2.n);
    };

    // ratioMinus :: Rational -> Rational -> Rational
    const ratioMinus = (n1, n2) => {
        const [r1, r2] = map(rational, [n1, n2]);
        const d = lcm(r1.d, r2.d);
        return ratio(
            (r1.n * (d / r1.d)) - (r2.n * (d / r2.d)),
            d
        );
    };

    // ratioMult :: Rational -> Rational -> Rational
    const ratioMult = (n1, n2) => {
        const [r1, r2] = map(rational, [n1, n2]);
        return ratio(r1.n * r2.n, r1.d * r2.d);
    };

    // ratioPlus :: Rational -> Rational -> Rational
    const ratioPlus = (n1, n2) => {
        const [r1, r2] = map(rational, [n1, n2]);
        const d = lcm(r1.d, r2.d);
        return ratio(
            (r1.n * (d / r1.d)) + (r2.n * (d / r2.d)),
            d
        );
    };

    // rational :: Num a => a -> Rational
    const rational = x =>
        isNaN(x) ? x : ratio(x, 1);

    // replicateString :: Int -> String -> String
    const replicateString = (n, s) => s.repeat(n);

    // showRatio :: Ratio -> String
    const showRatio = r =>
        'Ratio' !== r.type ? (
            r.toString()
        ) : r.n.toString() + (
            1 !== r.d ? (
                '/' + r.d.toString()
            ) : ''
        );

    // signum :: Num -> Num
    const signum = n => 0 > n ? -1 : (0 < n ? 1 : 0);

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
(0, 1)
(0, 1/3) (2/3, 1)
(0, 1/9) (2/9, 1/3) (2/3, 7/9) (8/9, 1)
(0, 1/27) (2/27, 1/9) (2/9, 7/27) (8/27, 1/3) (2/3, 19/27) (20/27, 7/9) (8/9, 25/27) (26/27, 1)

███████████████████████████
█████████         █████████
███   ███         ███   ███
█ █   █ █         █ █   █ █
```



## Julia

{{trans|AWK}}

```julia
const width = 81
const height = 5

function cantor!(lines, start, len, idx)
    seg = div(len, 3)
    if seg > 0
        for i in idx+1:height, j in start + seg + 1: start + seg * 2
            lines[i, j] = ' '
        end
        cantor!(lines, start, seg, idx + 1)
        cantor!(lines, start + 2 * seg, seg, idx + 1)
    end
end

lines = fill(UInt8('#'), height, width)
cantor!(lines, 0, width, 1)

for i in 1:height, j in 1:width
    print(Char(lines[i, j]), j == width ? "\n" : "")
end

```
{{out}}

```txt

#################################################################################
###########################                           ###########################
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #

```



## Kotlin

Simple terminal drawing.

```scala
// Version 1.2.31

const val WIDTH = 81
const val HEIGHT = 5

val lines = List(HEIGHT) { CharArray(WIDTH) { '*' } }

fun cantor(start: Int, len: Int, index: Int) {
    val seg = len / 3
    if (seg == 0) return
    for (i in index until HEIGHT) {
        for (j in start + seg until start + seg * 2) lines[i][j] = ' '
    }
    cantor(start, seg, index + 1)
    cantor(start + seg * 2, seg, index + 1)
}

fun main(args: Array<String>) {
    cantor(0, WIDTH, 1)
    lines.forEach { println(it) }
}
```


{{output}}

```txt

*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *

```



## Lua

{{trans|python}}

```lua
local WIDTH = 81
local HEIGHT = 5
local lines = {}

function cantor(start, length, index)
    -- must be local, or only one side will get calculated
    local seg = math.floor(length / 3)
    if 0 == seg then
        return nil
    end

    -- remove elements that are not in the set
    for it=0, HEIGHT - index do
        i = index + it
        for jt=0, seg - 1 do
            j = start + seg + jt
            pos = WIDTH * i + j
            lines[pos] = ' '
        end
    end

    -- left side
    cantor(start,           seg, index + 1)
    -- right side
    cantor(start + seg * 2, seg, index + 1)
    return nil
end

-- initialize the lines
for i=0, WIDTH * HEIGHT do
    lines[i] = '*'
end

-- calculate
cantor(0, WIDTH, 1)

-- print the result sets
for i=0, HEIGHT-1 do
    beg = WIDTH * i
    for j=beg, beg+WIDTH-1 do
        if j <= WIDTH * HEIGHT then
            io.write(lines[j])
        end
    end
    print()
end
```

{{out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```


=={{header|Modula-2}}==
{{trans|Kotlin}}

```modula2
MODULE Cantor;
FROM Terminal IMPORT Write,WriteLn,ReadChar;

CONST
    WIDTH = 81;
    HEIGHT = 5;
VAR
    lines : ARRAY[0..HEIGHT] OF ARRAY[0..WIDTH] OF CHAR;

PROCEDURE Init;
VAR i,j : CARDINAL;
BEGIN
    FOR i:=0 TO HEIGHT DO
        FOR j:=0 TO WIDTH DO
            lines[i,j] := '*'
        END
    END
END Init;

PROCEDURE Cantor(start,len,index : CARDINAL);
VAR i,j,seg : CARDINAL;
BEGIN
    seg := len DIV 3;
    IF seg=0 THEN RETURN END;
    FOR i:=index TO HEIGHT-1 DO
        j := start+seg;
        FOR j:=start+seg TO start+seg*2-1 DO
            lines[i,j] := ' '
        END
    END;
    Cantor(start, seg, index+1);
    Cantor(start+seg*2, seg, index+1)
END Cantor;

PROCEDURE Print;
VAR i,j : CARDINAL;
BEGIN
    FOR i:=0 TO HEIGHT-1 DO
        FOR j:=0 TO WIDTH-1 DO
            Write(lines[i,j])
        END;
        WriteLn
    END
END Print;

BEGIN
    Init;
    Cantor(0,WIDTH,1);
    Print;

    ReadChar;
END Cantor.
```



## Perl

{{trans|Perl 6}}

```Perl
use strict;
use feature 'say';

sub cantor {
    our($height) = @_;
    my $width = 3 ** ($height - 1);

    our @lines = ('#' x $width) x $height;

    sub trim_middle_third {
        my($len, $start, $index) = @_;
        my $seg = int $len / 3
            or return;

        for my $i ( $index .. $height - 1 ) {
          for my $j ( 0 .. $seg - 1 ) {
            substr $lines[$i], $start + $seg + $j, 1, ' ';
          }
        }

        trim_middle_third( $seg, $start + $_, $index + 1 ) for 0, $seg * 2;
    }

    trim_middle_third( $width, 0, 1 );
    @lines;
}

say for cantor(5);
```


{{Out}}

```txt
#################################################################################
###########################                           ###########################
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #
```



## Perl 6

{{trans|Kotlin}}

```perl6
sub cantor ( Int $height ) {
    my $width = 3 ** ($height - 1);

    my @lines = ( "\c[FULL BLOCK]" x $width ) xx $height;

    my sub _trim_middle_third ( $len, $start, $index ) {
        my $seg = $len div 3
            or return;

        for ( $index ..^ $height ) X ( 0 ..^ $seg ) -> ( $i, $j ) {
            @lines[$i].substr-rw( $start + $seg + $j, 1 ) = ' ';
        }

        _trim_middle_third( $seg, $start + $_, $index + 1 ) for 0, $seg * 2;
    }

    _trim_middle_third( $width, 0, 1 );
    return @lines;
}

.say for cantor(5);
```


{{Out}}

```txt
█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █
```



## Phix

Based on Algol 68, but even simpler, shorter, and sweeter!

```Phix
integer n = 5,
        w = power(3,n-1),
        len = w
string line = repeat('#',w)&"\n"

while 1 do
    puts(1,line)
    if len=1 then exit end if
    len /= 3
    integer pos = 1
    while pos<(w-len) do
        pos += len
        line[pos..pos+len-1] = ' '
        pos += len
    end while
end while
```

{{out}}

```txt

#################################################################################
###########################                           ###########################
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #

```



## Python


### Imperative


```python
WIDTH = 81
HEIGHT = 5

lines=[]
def cantor(start, len, index):
    seg = len / 3
    if seg == 0:
        return None
    for it in xrange(HEIGHT-index):
        i = index + it
        for jt in xrange(seg):
            j = start + seg + jt
            pos = i * WIDTH + j
            lines[pos] = ' '
    cantor(start,           seg, index + 1)
    cantor(start + seg * 2, seg, index + 1)
    return None

lines = ['*'] * (WIDTH*HEIGHT)
cantor(0, WIDTH, 1)

for i in xrange(HEIGHT):
    beg = WIDTH * i
    print ''.join(lines[beg : beg+WIDTH])
```

{{out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```


### Functional

Separating ''(Bool, Int)'' model from ''String'' display:
{{Works with|Python|3.7}}

```python
'''Cantor set – separating model from display'''

from functools import (reduce)
import itertools


# cantor :: [(Bool, Int)] -> [(Bool, Int)]
def cantor(xs):
    '''A Cantor segmentation step.'''
    def go(tpl):
        (bln, n) = tpl
        m = n // 3
        return [
            (True, m), (False, m), (True, m)
        ] if bln and (1 < n) else [tpl]
    return concatMap(go)(xs)


# cantorLines :: Int -> String
def cantorLines(n):
    '''A text block display of n
       Cantor-segmented lines.
    '''
    m = n - 1
    repeat = itertools.repeat
    return '\n'.join(
        [showCantor(x) for x in (
            reduce(
                lambda a, f: a + [f(a[-1])],
                repeat(cantor, m),
                [[(True, 3 ** m)]]
            )
        )]
    )


# showCantor :: [(Bool, Int)] -> String
def showCantor(xs):
    '''A text block display of a list of
       Cantor line segments.
    '''
    return ''.join(
        concatMap(lambda tpl: tpl[1] * ('█' if tpl[0] else ' '))(
            xs
        )
    )


# main :: IO ()
def main():
    '''Testing to depth 5'''

    print(
        cantorLines(5)
    )


# GENERIC -------------------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    chain = itertools.chain
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt

█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █
```


Or, using strings for both model and display:
{{Trans|JavaScript}}
{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Cantor set – strings as both model and display.'''

from itertools import (chain, islice)


# cantorLines :: Int -> String
def cantorLines(n):
    '''N levels of cantor segmentation,
       obtained and displayed in the
       form of lines of block characters.
    '''
    return '\n'.join(
        [''.join(x) for x in islice(
            iterate(cantor)(
                [3 ** (n - 1) * '█']
            ), n
        )]
    )


# cantor :: [String] -> [String]
def cantor(xs):
    '''A cantor line derived from its predecessor.'''
    def go(s):
        m = len(s) // 3
        blocks = s[0:m]
        return [
            blocks, m * ' ', blocks
        ] if '█' == s[0] else [s]
    return concatMap(go)(xs)


# MAIN ----------------------------------------------------
# main :: IO ()
def main():
    '''Testing cantor line generation to level 5'''

    print(
        cantorLines(5)
    )

# GENERIC -------------------------------------------------


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated
       applications of f to x.
    '''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █
```


===Dual representations – fractional and graphic===
{{Works with|Python|3.7}}

```python
'''A Cantor set generator, and two different
   representations of its output.
'''

from itertools import (islice, chain)
from functools import (reduce)
from fractions import Fraction


# cantor :: Generator [[(Fraction, Fraction)]]
def cantor():
    '''A non-finite stream of successive Cantor
       partitions of the line, in the form of
       lists of fraction pairs.
    '''
    def go(xy):
        (x, y) = xy
        third = Fraction(y - x, 3)
        return [(x, x + third), (y - third, y)]

    xs = [(0, 1)]
    while True:
        yield xs
        xs = concatMap(go)(xs)


# fractionLists :: [(Fraction, Fraction)] -> String
def fractionLists(xs):
    '''A fraction pair representation of a
       Cantor-partitioned line.
    '''
    def go(xy):
        return ', '.join(map(showRatio, xy))
    return ' '.join('(' + go(x) + ')' for x in xs)


# intervalBars :: [(Fraction, Fraction)] -> String
def intervalBars(w):
    '''A block diagram representation of a
       Cantor-partitioned line.
    '''
    def go(xs):
        def show(a, tpl):
            [x, y] = [int(w * r) for r in tpl]
            return (
                y,  # Accumulator - end of previous interval
                (' ' * (x - a)) + ('█' * (y - x))  # A gap + an interval bar
            )
        return mapAccumL(show)(0)(xs)
    return lambda xs: ''.join(go(xs)[1])


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Testing the generation of successive
       Cantor subdivisions of the line, and
       displaying them both as lines of fraction
       pairs and as graphic interval bars.
    '''
    xs = list(islice(cantor(), 4))
    w = max(xy[1].denominator for xy in xs[-1])
    print(
        '\n'.join(map(fractionLists, xs)),
        '\n'
    )
    print(
        '\n'.join(map(intervalBars(w), xs))
    )


# GENERIC -----------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
def mapAccumL(f):
    '''A tuple of an accumulation and a list derived by a
       combined map and fold,
       with accumulation from left to right.
    '''
    def go(a, x):
        tpl = f(a[0], x)
        return (tpl[0], a[1] + [tpl[1]])
    return lambda acc: lambda xs: (
        reduce(go, xs, (acc, []))
    )


# showRatio :: Ratio -> String
def showRatio(r):
    '''String representation of the ratio r.'''
    d = r.denominator
    return str(r.numerator) + (
        '/' + str(d) if 1 != d else ''
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
(0, 1)
(0, 1/3) (2/3, 1)
(0, 1/9) (2/9, 1/3) (2/3, 7/9) (8/9, 1)
(0, 1/27) (2/27, 1/9) (2/9, 7/27) (8/27, 1/3) (2/3, 19/27) (20/27, 7/9) (8/9, 25/27) (26/27, 1)

███████████████████████████
█████████         █████████
███   ███         ███   ███
█ █   █ █         █ █   █ █
```



## Racket

{{trans|Kotlin}}


```racket
#lang racket/base
;; {trans|Kotlin}}

(define current-width (make-parameter 81))

(define current-height (make-parameter 5))

(define (Cantor_set (w (current-width)) (h (current-height)))
  (define lines (build-list h (λ (_) (make-bytes w (char->integer #\#)))))
  (define (cantor start len index)
    (let* ((seg (quotient len 3))
           (seg-start (+ start seg))
           (seg-end (+ seg-start seg)))
      (unless (zero? seg)
        (for* ((i (in-range index h))
               (j (in-range seg-start seg-end)))
          (bytes-set! (list-ref lines i) j (char->integer #\space)))
        (cantor start seg (add1 index))
        (cantor seg-end seg (add1 index)))))
  (cantor 0 w 1)
  lines)

(module+ main
  (for-each displayln (Cantor_set)))

```

{{out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *

```



## REXX


```rexx
/*REXX program displays an ASCII diagram of a Canter Set as a set of (character) lines. */
w= linesize()                                    /*obtain the width of the display term.*/
if w==0  then w=81                               /*Can't obtain width?  Use the default.*/
                   do lines=0;        _=3**lines /*calculate powers of three  (# lines).*/
                   if _>w  then leave            /*Too large?  We passed the max value. */
                   #=_                           /*this value of a width─of─line is OK. */
                   end   /*lines*/               /* [↑]  calculate a useable line width.*/
w= #                                             /*use the (last) useable line width.   */
$= copies('■', #)                                /*populate the display line with blocks*/
                   do j=0  until #==0            /*show Cantor set as a line of chars.  */
                   if j>0  then do k=#+1  by  #+#  to w        /*skip 1st line blanking.*/
                                $=overlay(left('', #), $, k)   /*blank parts of a line. */
                                end   /*j*/
                   say $                         /*display a line of the Cantor Set.    */
                   #= # % 3                      /*the part (thirds) to be blanked out. */
                   end   /*j*/                   /*stick a fork in it,  we're all done. */
```

This REXX program makes use of   '''linesize'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

Some REXXes don't have this BIF, so the   '''linesize.rex'''   REXX program is included here   ──►   [[LINESIZE.REX]].

{{out|output|text=  when using the default size of the terminal width of 100:}}

(Shown at half size.)
<pre style="font-size:50%">
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■                           ■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■         ■■■■■■■■■                           ■■■■■■■■■         ■■■■■■■■■
■■■   ■■■         ■■■   ■■■                           ■■■   ■■■         ■■■   ■■■
■ ■   ■ ■         ■ ■   ■ ■                           ■ ■   ■ ■         ■ ■   ■ ■

```

{{out|output|text=  when using the default size of the terminal width of 250:}}

(Shown at half size.)
<pre style="font-size:50%">
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                                 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■                           ■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                                 ■■■■■■■■■■■■■■■■■■■■■■■■■■■                           ■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■         ■■■■■■■■■                           ■■■■■■■■■         ■■■■■■■■■                                                                                 ■■■■■■■■■         ■■■■■■■■■                           ■■■■■■■■■         ■■■■■■■■■
■■■   ■■■         ■■■   ■■■                           ■■■   ■■■         ■■■   ■■■                                                                                 ■■■   ■■■         ■■■   ■■■                           ■■■   ■■■         ■■■   ■■■
■ ■   ■ ■         ■ ■   ■ ■                           ■ ■   ■ ■         ■ ■   ■ ■                                                                                 ■ ■   ■ ■         ■ ■   ■ ■                           ■ ■   ■ ■         ■ ■   ■ ■

```



## Ring


```ring

# Project : Cantor set

load "guilib.ring"
paint = null

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("")
                  setgeometry(100,100,800,600)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,800,600)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(150,500,100,30)
                          settext("draw")
                          setclickevent("draw()")
                  }
                  show()
        }
        exec()
        }

func draw
        p1 = new qpicture()
               color = new qcolor() {
               setrgb(0,0,255,255)
        }
        pen = new qpen() {
                 setcolor(color)
                 setwidth(10)
        }
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

        cantor(10,20,600)

        endpaint()
        }
        label1 { setpicture(p1) show() }
        return

func cantor(x,y,lens)
        if lens >= 10
           paint.drawline(x,y,x+lens,y)
           y = y + 20
           cantor(x,y,floor(lens/3))
           cantor(x+floor(lens*2/3),y,floor(lens/3))
        ok

```

Output image:

[https://www.dropbox.com/s/ap7c3301i0syh4e/CantorSet.jpg?dl=0 Cantor set]

## Ruby

This works by numbering the segments (starting with 0) in base 3. Print whitespace if this number contains the digit 1; a black square otherwise.

```ruby
lines = 5

(0..lines).each do |exp|
  seg_size = 3**(lines-exp-1)
  chars = (3**exp).times.map{ |n| n.digits(3).any?(1) ? " " : "█"}
  puts chars.map{ |c| c * seg_size }.join
end

```

{{out}}

```txt
█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █


```



## Scala

===Imperative Programming (Q&D)===

```Scala
object CantorSetQD extends App {
  val (width, height) = (81, 5)

  val lines = Seq.fill[Array[Char]](height)(Array.fill[Char](width)('*'))

  def cantor(start: Int, len: Int, index: Int) {
    val seg = len / 3

    println(start, len, index)

    if (seg != 0) {
      for (i <- index until height;
           j <- (start + seg) until (start + seg * 2)) lines(i)(j) = ' '

      cantor(start, seg, index + 1)
      cantor(start + seg * 2, seg, index + 1)
    }
  }

  cantor(0, width, 1)
  lines.foreach(l => println(l.mkString))
}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/QrqaHeu/0 (JavaScript)]
or by [https://scastie.scala-lang.org/4JTi1zXzRq6H4yUlAYNTSw Scastie (JVM)].

===Functional Programming (Recommended)===

```Scala
object CantorSetFP extends App {
  val (width, height) = (81, 5)

  def lines = (1 to height).map(_ => (0 until width).toSet)

  def cantorSet(pre: Seq[Set[Int]], start: Int, len: Int, index: Int): Seq[Set[Int]] = {
    val seg = len / 3

    def cantorSet1(pre: Seq[Set[Int]], start: Int, index: Int): Seq[Set[Int]] = {
      def elementsStuffing(pre: Set[Int], start: Int): Set[Int] =
        pre -- ((start + seg) until (start + seg * 2))

      for (n <- 0 until height)
        yield if (index to height contains n) elementsStuffing(pre(n), start)
        else pre(n)
    }

    if (seg == 0) pre
    else {
      def version0 = cantorSet1(pre, start, index)
      def version1 = cantorSet(cantorSet1(pre, start, index), start, seg, index + 1)

      cantorSet(version1, start + seg * 2, seg, index + 1)
    }
  }

  def output: Seq[Set[Int]] = cantorSet(lines, 0, width, 1)

  println(
    output.map(l => (0 to width).map(pos => if (l contains pos) '*' else ' ').mkString)
      .mkString("\n"))
}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/szUjPWO/7 (JavaScript)]
or by [https://scastie.scala-lang.org/ZaNUtEOcStuBfJImOnEz5Q Scastie (JVM)].


## Sidef

{{trans|Perl 6}}

```ruby
func cantor (height) {
    var width = 3**(height - 1)
    var lines = height.of { "\N{FULL BLOCK}" * width }

    func trim_middle_third (len, start, index) {
        var seg = (len // 3) || return()

        for i, j in ((index ..^ height) ~X (0 ..^ seg)) {
            lines[i].replace!(Regex("^.{#{start + seg + j}}\\K."), ' ')
        }

        [0, 2*seg].each { |k|
            trim_middle_third(seg, start + k, index + 1)
        }
    }

    trim_middle_third(width, 0, 1)
    return lines
}

cantor(5).each { .say }
```

{{out}}

```txt

█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █

```



## Smalltalk

{{works with|GNU Smalltalk}}
Smalltalk represents Intervals' start and stop values as Fraction, so precision is kept for quilte a while.

```Smalltalk
Object subclass: CantorSet [

    | intervals |

    CantorSet class >> new
        [^self basicNew
            initialize;
            yourself]

    initialize
        [intervals := Array with: (CantorInterval
            from: 0
            to: 1)]

    split
        [intervals := intervals gather: [:each | each split]]

    displayOn: aStream atScale: aNumber
        [| current |
        current := 0.
        intervals do:
            [:each |
            (each start - current) * aNumber timesRepeat: [aStream space].
            each length * aNumber timesRepeat: [aStream nextPut: $#].
            current := each stop].
        aStream nl]
]

Interval subclass: CantorInterval [

    split
        [| oneThird left right |
        oneThird := self length / 3.
        left := self class
            from: start
            to: start + oneThird.
        right := self class
            from: stop - oneThird
            to: stop.
        ^Array
            with: left
            with: right]

    start  [^start]
    stop   [^stop]
    length [^stop - start]

    printOn: aStream
        [aStream << ('%1[%2,%3]' % {self class name. start. stop})]
]

Object subclass: TestCantor [

    TestCantor class >> iterations: anInteger
        [| cantorset scale count |
        scale := 3 raisedTo: anInteger. "Make smallest interval 1"
        count := 0.
        cantorset := CantorSet new.

        [cantorset
            displayOn: Transcript
            atScale: scale.
        count < anInteger] whileTrue:
            [cantorset split.
            count := count + 1]]
]

TestCantor iterations: 4.
```

Output:

```txt
#################################################################################
###########################                           ###########################
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Const WIDTH = 81
    Const HEIGHT = 5
    Dim lines(HEIGHT, WIDTH) As Char

    Sub Init()
        For i = 0 To HEIGHT - 1
            For j = 0 To WIDTH - 1
                lines(i, j) = "*"
            Next
        Next
    End Sub

    Sub Cantor(start As Integer, len As Integer, index As Integer)
        Dim seg As Integer = len / 3
        If seg = 0 Then
            Return
        End If
        For i = index To HEIGHT - 1
            For j = start + seg To start + seg * 2 - 1
                lines(i, j) = " "
            Next
        Next
        Cantor(start, seg, index + 1)
        Cantor(start + seg * 2, seg, index + 1)
    End Sub

    Sub Main()
        Init()
        Cantor(0, WIDTH, 1)
        For i = 0 To HEIGHT - 1
            For j = 0 To WIDTH - 1
                Console.Write(lines(i, j))
            Next
            Console.WriteLine()
        Next
    End Sub

End Module
```

{{out}}

```txt
*********************************************************************************
***************************                           ***************************
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
```



## zkl


```zkl
const WIDTH=81, HEIGHT=5;
var lines=HEIGHT.pump(List,List.createLong(WIDTH,"\U2588;").copy);  // full block

fcn cantor(start,len,index){
   (seg:=len/3) or return();
   foreach i,j in ([index..HEIGHT-1], [start + seg .. start + seg*2 - 1]){
      lines[i][j]=" ";
   }
   cantor(start, seg, index + 1);
   cantor(start + seg*2, seg, index + 1);
}(0,WIDTH,1);

lines.pump(Console.println,"concat");
```

{{out}}

```txt

█████████████████████████████████████████████████████████████████████████████████
███████████████████████████                           ███████████████████████████
█████████         █████████                           █████████         █████████
███   ███         ███   ███                           ███   ███         ███   ███
█ █   █ █         █ █   █ █                           █ █   █ █         █ █   █ █

```


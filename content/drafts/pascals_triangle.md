+++
title = "Pascal's triangle"
description = ""
date = 2019-10-22T13:08:49Z
aliases = []
[extra]
id = 2892
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}

[[wp:Pascal's triangle|Pascal's triangle]] is an arithmetic and geometric figure often associated with the name of [[wp:Blaise Pascal|Blaise Pascal]], but also studied centuries earlier in India, Persia, China and elsewhere.

Its first few rows look like this:
     1
    1 1
   1 2 1
  1 3 3 1
where each element of each row is either 1 or the sum of the two elements right above it.

For example, the next row of the triangle would be:
:::   '''1'''   (since the first element of each row doesn't have two elements above it)
:::   '''4'''   (1 + 3)
:::   '''6'''   (3 + 3)
:::   '''4'''   (3 + 1)
:::   '''1'''   (since the last element of each row doesn't have two elements above it)

So the triangle now looks like this:
     1
    1 1
   1 2 1
  1 3 3 1
 1 4 6 4 1

Each row   <tt> n </tt>   (starting with row   0   at the top) shows the coefficients of the binomial expansion of   <big><big> (x + y)<sup>n</sup>. </big></big>


;Task:
Write a function that prints out the first   <tt> n </tt>   rows of the triangle   (with   <tt> f(1) </tt>   yielding the row consisting of only the element '''1''').

This can be done either by summing elements from the previous rows or using a binary coefficient or combination function.

Behavior for   <big><tt> n ≤ 0 </tt></big>   does not need to be uniform, but should be noted.


;See also:
* [[Evaluate binomial coefficients]]





## 360 Assembly

{{trans|PL/I}}

```360asm
*        Pascal's triangle         25/10/2015
PASCAL   CSECT
         USING  PASCAL,R15         set base register
         LA     R7,1               n=1
LOOPN    C      R7,=A(M)           do n=1 to m
         BH     ELOOPN             if n>m then goto
         MVC    U,=F'1'            u(1)=1
         LA     R8,PG              pgi=@pg
         LA     R6,1               i=1
LOOPI    CR     R6,R7              do i=1 to n
         BH     ELOOPI             if i>n then goto
         LR     R1,R6              i
         SLA    R1,2               i*4
         L      R3,T-4(R1)         t(i)
         L      R4,T(R1)           t(i+1)
         AR     R3,R4              t(i)+t(i+1)
         ST     R3,U(R1)           u(i+1)=t(i)+t(i+1)
         LR     R1,R6              i
         SLA    R1,2               i*4
         L      R2,U-4(R1)         u(i)
         XDECO  R2,XD              edit u(i)
         MVC    0(4,R8),XD+8       output u(i):4
         LA     R8,4(R8)           pgi=pgi+4
         LA     R6,1(R6)           i=i+1
         B      LOOPI              end i
ELOOPI   MVC    T((M+1)*(L'T)),U   t=u
         XPRNT  PG,80              print
         LA     R7,1(R7)           n=n+1
         B      LOOPN              end n
ELOOPN   XR     R15,R15            set return code
         BR     R14                return to caller
M        EQU    11                 <== input
T        DC     (M+1)F'0'          t(m+1) init 0
U        DC     (M+1)F'0'          u(m+1) init 0
PG       DC     CL80' '            pg     init ' '
XD       DS     CL12               temp
         YREGS
         END    PASCAL
```

{{out}}

```txt

   1
   1   1
   1   2   1
   1   3   3   1
   1   4   6   4   1
   1   5  10  10   5   1
   1   6  15  20  15   6   1
   1   7  21  35  35  21   7   1
   1   8  28  56  70  56  28   8   1
   1   9  36  84 126 126  84  36   9   1
   1  10  45 120 210 252 210 120  45  10   1

```



## 8th

One way, using array operations:

```forth

\ print the array
: .arr \ a -- a
  ( . space ) a:each ;

: pasc \ a --
  \ print the row
  .arr cr
  dup
  \ create two rows from the first, one with a leading the other with a trailing 0
  [0] 0 a:insert swap 0 a:push
  \ add the arrays together to make the new one
  ' n:+ a:op ;

\ print the first 16 rows:
[1] ' pasc 16 times

```


Another way, using the relation between element 'n' and element 'n-1' in a row:

```forth

: ratio \ m n -- num denom
  tuck n:- n:1+ swap ;

\ one item in the row: n m
: pascitem \ n m -- n
	r@ swap
	ratio
	n:*/ n:round int
	dup . space ;

\ One row of Pascal's triangle
: pascline \ n --
	>r 1 int dup . space
	' pascitem
	1 r@ loop rdrop drop cr ;

\ Calculate the first 'n' rows of Pascal's triangle:
: pasc \ n
	' pascline 0 rot loop cr ;

15 pasc

```



## Ada


The specification of auxiliary package "Pascal". "First_Row" outputs a row with a single "1", "Next_Row" computes the next row from a given row, and "Length" gives the number of entries in a row. The package is also used for the Catalan numbers solution [[http://rosettacode.org/wiki/Catalan_numbers/Pascal%27s_triangle]]


```ada
package Pascal is

   type Row is array (Natural range <>) of Natural;

   function Length(R: Row) return Positive;

   function First_Row(Max_Length: Positive) return Row;

   function Next_Row(R: Row) return Row;

end Pascal;
```


The implementation of that auxiliary package "Pascal":


```Ada
package body Pascal is

   function First_Row(Max_Length: Positive) return Row is
      R: Row(0 .. Max_Length) := (0 | 1 => 1, others => 0);
   begin
      return R;
   end First_Row;

   function Next_Row(R: Row) return Row is
      S: Row(R'Range);
   begin
      S(0) := Length(R)+1;
      S(Length(S)) := 1;
      for J in reverse 2 .. Length(R) loop
         S(J) := R(J)+R(J-1);
      end loop;
      S(1) := 1;
      return S;
   end Next_Row;

   function Length(R: Row) return Positive is
   begin
      return R(0);
   end Length;

end Pascal;
```


The main program, using "Pascal". It prints the desired number of rows. The number is read from the command line.


```Ada
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_Line, Pascal; use Pascal;

procedure Triangle is

   Number_Of_Rows: Positive := Integer'Value(Ada.Command_Line.Argument(1));
   Row: Pascal.Row := First_Row(Number_Of_Rows);

begin
   loop
      -- print one row
      for J in 1 .. Length(Row) loop
	 Ada.Integer_Text_IO.Put(Row(J), 5);
      end loop;
      Ada.Text_IO.New_Line;
      exit when Length(Row) >= Number_Of_Rows;
      Row := Next_Row(Row);
   end loop;
end Triangle;
```


{{out}}


```txt
>./triangle 12
    1
    1    1
    1    2    1
    1    3    3    1
    1    4    6    4    1
    1    5   10   10    5    1
    1    6   15   20   15    6    1
    1    7   21   35   35   21    7    1
    1    8   28   56   70   56   28    8    1
    1    9   36   84  126  126   84   36    9    1
    1   10   45  120  210  252  210  120   45   10    1
    1   11   55  165  330  462  462  330  165   55   11    1
```



## ALGOL 68


```algol68
PRIO MINLWB = 8, MAXUPB = 8;
OP MINLWB = ([]INT a,b)INT: (LWB a<LWB b|LWB a|LWB b),
   MAXUPB = ([]INT a,b)INT: (UPB a>UPB b|UPB a|UPB b);

OP + = ([]INT a,b)[]INT:(
  [a MINLWB b:a MAXUPB b]INT out; FOR i FROM LWB out TO UPB out DO out[i]:= 0 OD;
  out[LWB a:UPB a] := a; FOR i FROM LWB b TO UPB b DO out[i]+:= b[i] OD;
  out
);

INT width = 4, stop = 9;
FORMAT centre = $n((stop-UPB row+1)*width OVER 2)(q)$;

FLEX[1]INT row := 1; # example of rowing #
FOR i WHILE
  printf((centre, $g(-width)$, row, $l$));
# WHILE # i < stop DO
  row := row[AT 1] + row[AT 2]
OD
```

{{Out}}

```txt

                     1
                   1   1
                 1   2   1
               1   3   3   1
             1   4   6   4   1
           1   5  10  10   5   1
         1   6  15  20  15   6   1
       1   7  21  35  35  21   7   1
     1   8  28  56  70  56  28   8   1

```



## ALGOL W


```algolw
begin
    % prints the first n lines of Pascal's triangle lines %
    % if n is <= 0, no output is produced                 %
    procedure printPascalTriangle( integer value n ) ;
        if n > 0 then begin
            integer array pascalLine ( 1 :: n );
            pascalLine( 1 ) := 1;
            for line := 1 until n do begin
                for i := line - 1 step - 1 until 2 do pascalLine( i ) := pascalLine( i - 1 ) + pascalLine( i );
                pascalLine( line ) := 1;
                write( s_w := 0, " " );
                for i := line until n do writeon( s_w := 0, "   " );
                for i := 1 until line do writeon( i_w := 6, s_w := 0, pascalLine( i ) )
            end for_line ;
        end printPascalTriangle ;

    printPascalTriangle( 8 )

end.
```

{{out}}

```txt

                              1
                           1     1
                        1     2     1
                     1     3     3     1
                  1     4     6     4     1
               1     5    10    10     5     1
            1     6    15    20    15     6     1
         1     7    21    35    35    21     7     1

```



## APL

Pascal' s triangle of order ⍵


```apl

{A←0,⍳⍵ ⋄ ⍉A∘.!A}

```


example


```apl

{A←0,⍳⍵ ⋄ ⍉A∘.!A} 3

```



```txt

1 0 0 0
1 1 0 0
1 2 1 0
1 3 3 1

```




## AppleScript


Drawing n rows from a generator:


```AppleScript

-- pascal :: Generator [[Int]]
on pascal()
    script nextRow
        on |λ|(row)
            zipWith(my plus, {0} & row, row & {0})
        end |λ|
    end script
    iterate(nextRow, {1})
end pascal


on run
    showPascal(take(7, pascal()))
end run


-- showPascal :: [[Int]] -> String
on showPascal(xs)
    set w to length of intercalate("   ", item -1 of xs)
    script align
        on |λ|(x)
            |center|(w, space, intercalate("   ", x))
        end |λ|
    end script
    unlines(map(align, xs))
end showPascal


-- GENERIC ABSTRACTIONS -------------------------------------------------------

-- center :: Int -> Char -> String -> String
on |center|(n, cFiller, strText)
    set lngFill to n - (length of strText)
    if lngFill > 0 then
        set strPad to replicate(lngFill div 2, cFiller) as text
        set strCenter to strPad & strText & strPad
        if lngFill mod 2 > 0 then
            cFiller & strCenter
        else
            strCenter
        end if
    else
        strText
    end if
end |center|

-- intercalate :: String -> [String] -> String
on intercalate(sep, xs)
    set {dlm, my text item delimiters} to {my text item delimiters, sep}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end intercalate

-- iterate :: (a -> a) -> a -> Generator [a]
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

-- length :: [a] -> Int
on |length|(xs)
    set c to class of xs
    if list is c or string is c then
        length of xs
    else
        2 ^ 30 -- (simple proxy for non-finite)
    end if
end |length|

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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

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

-- plus :: Num -> Num -> Num
on plus(a, b)
    a + b
end plus

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
            set end of ys to xs's |λ|()
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

-- unwords :: [String] -> String
on unwords(xs)
    set {dlm, my text item delimiters} to {my text item delimiters, space}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end unwords

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(|length|(xs), |length|(ys))
    if 1 > lng then return {}
    set xs_ to take(lng, xs) -- Allow for non-finite
    set ys_ to take(lng, ys) -- generators like cycle etc
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs_, item i of ys_)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```txt
              1
            1   1
          1   2   1
        1   3   3   1
      1   4   6   4   1
   1   5   10   10   5   1
1   6   15   20   15   6   1
```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/viewtopic.php?p=276617#276617 discussion]

```AutoHotkey
n := 8, p0 := "1"        ; 1+n rows of Pascal's triangle
Loop %n% {
   p := "p" A_Index, %p% := v := 1, q := "p" A_Index-1
   Loop Parse, %q%, %A_Space%
      If (A_Index > 1)
         %p% .= " " v+A_LoopField, v := A_LoopField
   %p% .= " 1"
}
                         ; Triangular Formatted output
VarSetCapacity(tabs,n,Asc("`t"))
t .= tabs "`t1"
Loop %n% {
   t .= "`n" SubStr(tabs,A_Index)
   Loop Parse, p%A_Index%, %A_Space%
      t .= A_LoopField "`t`t"
}
Gui Add, Text,, %t%      ; Show result in a GUI
Gui Show
Return

GuiClose:
  ExitApp
```


Alternate {{works with|AutoHotkey L}}

```AutoHotkey
Msgbox % format(pascalstriangle())
Return

format(o) ; converts object to string
{
	For k, v in o
		s .= IsObject(v) ? format(v) "`n" : v " "
	Return s
}
pascalstriangle(n=7) ; n rows of Pascal's triangle
{
	p := Object(), z:=Object()
	Loop, % n
		Loop, % row := A_Index
			col := A_Index
			, p[row, col] := row = 1 and col = 1
				? 1
				: (p[row-1, col-1] = "" ; math operations on blanks return blanks; I want to assume zero
					? 0
					: p[row-1, col-1])
				+ (p[row-1, col] = ""
					? 0
					: p[row-1, col])
	Return p
}
```

n <= 0 returns empty


## AWK


```awk
$ awk 'BEGIN{for(i=0;i<6;i++){c=1;r=c;for(j=0;j<i;j++){c*=(i-j)/(j+1);r=r" "c};print r}}'
```

{{Out}}
```txt

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1

```



## BASIC


### Summing from Previous Rows

{{works with|FreeBASIC}}
This implementation uses an array to store one row of the triangle.
DIM initializes the array values to zero. For first row, "1" is then stored in the array.
To calculate values for next row, the value in cell (i-1) is added to each cell (i).
This summing is done from right to left so that it can be done on-place, without using a tmp buffer.
Because of symmetry, the values can be displayed from left to right.

Space for max 5 digit numbers is reserved when formatting the display.
The maximum size of triangle is 100 rows, but in practice it is limited by screen space.
If the user enters value less than 1, the first row is still always displayed.


```freebasic
DIM i             AS Integer
DIM row           AS Integer
DIM nrows         AS Integer
DIM values(100)   AS Integer

INPUT "Number of rows: "; nrows
values(1) = 1
PRINT TAB((nrows)*3);"  1"
FOR row = 2 TO nrows
    PRINT TAB((nrows-row)*3+1);
    FOR i = row TO 1 STEP -1
        values(i) = values(i) + values(i-1)
        PRINT USING "##### "; values(i);
    NEXT i
    PRINT
NEXT row
```



## Batch File

Based from the Fortran Code.

```dos
@echo off
setlocal enabledelayedexpansion

::The Main Thing...
cls
echo.
set row=15
call :pascal
echo.
pause
exit /b 0
::/The Main Thing.

::The Functions...
:pascal
	set /a prev=%row%-1
	for /l %%I in (0,1,%prev%) do (
		set c=1&set r=
		for /l %%K in (0,1,%row%) do (
			if not !c!==0 (
				call :numstr !c!
				set r=!r!!space!!c!
			)
			set /a c=!c!*^(%%I-%%K^)/^(%%K+1^)
		)
		echo !r!
	)
goto :EOF

:numstr
	::This function returns the number of whitespaces to be applied on each numbers.
	set cnt=0&set proc=%1&set space=
	:loop
	set currchar=!proc:~%cnt%,1!
	if not "!currchar!"=="" set /a cnt+=1&goto loop
	set /a numspaces=5-!cnt!
	for /l %%A in (1,1,%numspaces%) do set "space=!space! "
goto :EOF
::/The Functions.
```

{{Out}}

```txt
    1
    1    1
    1    2    1
    1    3    3    1
    1    4    6    4    1
    1    5   10   10    5    1
    1    6   15   20   15    6    1
    1    7   21   35   35   21    7    1
    1    8   28   56   70   56   28    8    1
    1    9   36   84  126  126   84   36    9    1
    1   10   45  120  210  252  210  120   45   10    1
    1   11   55  165  330  462  462  330  165   55   11    1
    1   12   66  220  495  792  924  792  495  220   66   12    1
    1   13   78  286  715 1287 1716 1716 1287  715  286   78   13    1
    1   14   91  364 1001 2002 3003 3432 3003 2002 1001  364   91   14    1

Press any key to continue . . .
```



## BBC BASIC


```bbcbasic
      nrows% = 10

      colwidth% = 4
      @% = colwidth% : REM Set column width
      FOR row% = 1 TO nrows%
        PRINT SPC(colwidth%*(nrows% - row%)/2);
        acc% = 1
        FOR element% = 1 TO row%
          PRINT acc%;
          acc% = acc% * (row% - element%) / element% + 0.5
        NEXT
        PRINT
      NEXT row%
```

{{Out}}

```txt
                     1
                   1   1
                 1   2   1
               1   3   3   1
             1   4   6   4   1
           1   5  10  10   5   1
         1   6  15  20  15   6   1
       1   7  21  35  35  21   7   1
     1   8  28  56  70  56  28   8   1
   1   9  36  84 126 126  84  36   9   1
```



## Befunge


```Befunge
0" :swor fo rebmuN">:#,_&> 55+, v
v01*p00-1:g00.:<1p011p00:\-1_v#:<
>g:1+10p/48*,:#^_$ 55+,1+\: ^>$$@
```

{{Out}}

```txt
Number of rows: 10

1
1  1
1  2  1
1  3  3  1
1  4  6  4  1
1  5  10  10  5  1
1  6  15  20  15  6  1
1  7  21  35  35  21  7  1
1  8  28  56  70  56  28  8  1
1  9  36  84  126  126  84  36  9  1
```



## Bracmat


```bracmat
( out$"Number of rows? "
& get':?R
& -1:?I
&   whl
  ' ( 1+!I:<!R:?I
    & 1:?C
    & -1:?K
    & !R+-1*!I:?tabs
    & whl'(!tabs+-1:>0:?tabs&put$\t)
    &   whl
      ' ( 1+!K:~>!I:?K
        & put$(!C \t\t)
        & !C*(!I+-1*!K)*(!K+1)^-1:?C
        )
    & put$\n
    )
&
)
```

{{Out}}

```txt
Number of rows?
7
                                                1
                                        1               1
                                1               2               1
                        1               3               3               1
                1               4               6               4               1
        1               5               10              10              5               1
1               6               15              20              15              6               1
```



## Burlesque



```burlesque

blsq ) {1}{1 1}{^^2CO{p^?+}m[1+]1[+}15E!#s<-spbx#S
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1
1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1
1 9 36 84 126 126 84 36 9 1
1 10 45 120 210 252 210 120 45 10 1
1 11 55 165 330 462 462 330 165 55 11 1
1 12 66 220 495 792 924 792 495 220 66 12 1
1 13 78 286 715 1287 1716 1716 1287 715 286 78 13 1
1 14 91 364 1001 2002 3003 3432 3003 2002 1001 364 91 14 1
1 15 105 455 1365 3003 5005 6435 6435 5005 3003 1365 455 105 15 1
1 16 120 560 1820 4368 8008 11440 12870 11440 8008 4368 1820 560 120 16 1

```





## C


{{trans|Fortran}}


```c
#include <stdio.h>

void pascaltriangle(unsigned int n)
{
  unsigned int c, i, j, k;

  for(i=0; i < n; i++) {
    c = 1;
    for(j=1; j <= 2*(n-1-i); j++) printf(" ");
    for(k=0; k <= i; k++) {
      printf("%3d ", c);
      c = c * (i-k)/(k+1);
    }
    printf("\n");
  }
}

int main()
{
  pascaltriangle(8);
  return 0;
}
```



### Recursive


```c
#include <stdio.h>

#define D 32
int pascals(int *x, int *y, int d)
{
	int i;
	for (i = 1; i < d; i++)
		printf("%d%c", y[i] = x[i - 1] + x[i],
			i < d - 1 ? ' ' : '\n');

	return D > d ? pascals(y, x, d + 1) : 0;
}

int main()
{
	int x[D] = {0, 1, 0}, y[D] = {0};
	return pascals(x, y, 0);
}
```



### Adding previous row values



```c
void triangleC(int nRows) {
    if (nRows <= 0) return;
    int *prevRow = NULL;
    for (int r = 1; r <= nRows; r++) {
        int *currRow = malloc(r * sizeof(int));
        for (int i = 0; i < r; i++) {
            int val = i==0 || i==r-1 ? 1 : prevRow[i-1] + prevRow[i];
            currRow[i] = val;
            printf(" %4d", val);
        }
        printf("\n");
        free(prevRow);
        prevRow = currRow;
    }
    free(prevRow);
}
```



## C++


```cpp
#include <iostream>
#include <algorithm>
#include<cstdio>
using namespace std;
void Pascal_Triangle(int size) {

	int a[100][100];
	int i, j;

	//first row and first coloumn has the same value=1
	for (i = 1; i <= size; i++) {
		a[i][1] = a[1][i] = 1;
	}

	//Generate the full Triangle
	for (i = 2; i <= size; i++) {
		for (j = 2; j <= size - i; j++) {
			if (a[i - 1][j] == 0 || a[i][j - 1] == 0) {
				break;
			}
			a[i][j] = a[i - 1][j] + a[i][j - 1];
		}
	}

	/*
	  1 1 1 1
	  1 2 3
	  1 3
	  1

	first print as above format-->

	for (i = 1; i < size; i++) {
		for (j = 1; j < size; j++) {
			if (a[i][j] == 0) {
					break;
			}
				printf("%8d",a[i][j]);
		}
			cout<<"\n\n";
	}*/

	// standard Pascal Triangle Format

	int row,space;
	for (i = 1; i < size; i++) {
		space=row=i;
		j=1;

		while(space<=size+(size-i)+1){
			 cout<<" ";
			 space++;
		 }

		while(j<=i){
			if (a[row][j] == 0){
				   break;
			   }

			if(j==1){
				printf("%d",a[row--][j++]);
			}
			else
				printf("%6d",a[row--][j++]);
		}
			cout<<"\n\n";
	}

}

int main()
{
	//freopen("out.txt","w",stdout);

	int size;
	cin>>size;
	Pascal_Triangle(size);
}

}
```

===C++11 (with dynamic and semi-static vectors)===
Constructs the whole triangle in memory before printing it. Uses vector of vectors as a 2D array with variable column size. Theoretically, semi-static version should work a little faster.

```cpp
// Compile with -std=c++11
#include<iostream>
#include<vector>
using namespace std;
void print_vector(vector<int> dummy){
	for (vector<int>::iterator i = dummy.begin(); i != dummy.end(); ++i)
		cout<<*i<<" ";
	cout<<endl;
}
void print_vector_of_vectors(vector<vector<int>> dummy){
	for (vector<vector<int>>::iterator i = dummy.begin(); i != dummy.end(); ++i)
		print_vector(*i);
	cout<<endl;
}
vector<vector<int>> dynamic_triangle(int dummy){
	vector<vector<int>> result;
	if (dummy > 0){ // if the argument is 0 or negative exit immediately
		vector<int> row;
		// The first row
		row.push_back(1);
		result.push_back(row);
		// The second row
		if (dummy > 1){
			row.clear();
			row.push_back(1); row.push_back(1);
			result.push_back(row);
		}
		// The other rows
		if (dummy > 2){
			for (int i = 2; i < dummy; i++){
				row.clear();
				row.push_back(1);
				for (int j = 1; j < i; j++)
					row.push_back(result.back().at(j - 1) + result.back().at(j));
				row.push_back(1);
				result.push_back(row);
			}
		}
	}
	return result;
}
vector<vector<int>> static_triangle(int dummy){
	vector<vector<int>> result;
	if (dummy > 0){ // if the argument is 0 or negative exit immediately
		vector<int> row;
		result.resize(dummy); // This should work faster than consecutive push_back()s
		// The first row
		row.resize(1);
		row.at(0) = 1;
		result.at(0) = row;
		// The second row
		if (result.size() > 1){
			row.resize(2);
			row.at(0) = 1; row.at(1) = 1;
			result.at(1) = row;
		}
		// The other rows
		if (result.size() > 2){
			for (int i = 2; i < result.size(); i++){
				row.resize(i + 1); // This should work faster than consecutive push_back()s
				row.front() = 1;
				for (int j = 1; j < row.size() - 1; j++)
					row.at(j) = result.at(i - 1).at(j - 1) + result.at(i - 1).at(j);
				row.back() = 1;
				result.at(i) = row;
			}
		}
	}
	return result;
}
int main(){
	vector<vector<int>> triangle;
	int n;
	cout<<endl<<"The Pascal's Triangle"<<endl<<"Enter the number of rows: ";
	cin>>n;
	// Call the dynamic function
	triangle = dynamic_triangle(n);
	cout<<endl<<"Calculated using dynamic vectors:"<<endl<<endl;
	print_vector_of_vectors(triangle);
	// Call the static function
	triangle = static_triangle(n);
	cout<<endl<<"Calculated using static vectors:"<<endl<<endl;
	print_vector_of_vectors(triangle);
	return 0;
}


```

===C++11 (with a class) ===
A full fledged example with a class definition and methods to retrieve data, worthy of the title object-oriented.

```cpp
// Compile with -std=c++11
#include<iostream>
#include<vector>
using namespace std;
class pascal_triangle{
	vector<vector<int>> data; // This is the actual data
	void print_row(vector<int> dummy){
		for (vector<int>::iterator i = dummy.begin(); i != dummy.end(); ++i)
			cout<<*i<<" ";
		cout<<endl;
	}
public:
	pascal_triangle(int dummy){ // Everything is done on the construction phase
		if (dummy > 0){ // if the argument is 0 or negative exit immediately
			vector<int> row;
			data.resize(dummy); // Theoretically this should work faster than consecutive push_back()s
			// The first row
			row.resize(1);
			row.at(0) = 1;
			data.at(0) = row;
			// The second row
			if (data.size() > 1){
				row.resize(2);
				row.at(0) = 1; row.at(1) = 1;
				data.at(1) = row;
			}
			// The other rows
			if (data.size() > 2){
				for (int i = 2; i < data.size(); i++){
					row.resize(i + 1); // Theoretically this should work faster than consecutive push_back()s
					row.front() = 1;
					for (int j = 1; j < row.size() - 1; j++)
						row.at(j) = data.at(i - 1).at(j - 1) + data.at(i - 1).at(j);
					row.back() = 1;
					data.at(i) = row;
				}
			}
		}
	}
	~pascal_triangle(){
		for (vector<vector<int>>::iterator i = data.begin(); i != data.end(); ++i)
			i->clear(); // I'm not sure about the necessity of this loop!
		data.clear();
	}
	void print_row(int dummy){
		if (dummy < data.size())
			for (vector<int>::iterator i = data.at(dummy).begin(); i != data.at(dummy).end(); ++i)
				cout<<*i<<" ";
		cout<<endl;
	}
	void print(){
		for (int i = 0; i < data.size(); i++)
			print_row(i);
	}
	int get_coeff(int dummy1, int dummy2){
		int result = 0;
		if ((dummy1 < data.size()) && (dummy2 < data.at(dummy1).size()))
				result = data.at(dummy1).at(dummy2);
		return result;
	}
	vector<int> get_row(int dummy){
		vector<int> result;
		if (dummy < data.size())
			result = data.at(dummy);
		return result;
	}
};
int main(){
	int n;
	cout<<endl<<"The Pascal's Triangle with a class!"<<endl<<endl<<"Enter the number of rows: ";
	cin>>n;
	pascal_triangle myptri(n);
	cout<<endl<<"The whole triangle:"<<endl;
	myptri.print();
	cout<<endl<<"Just one row:"<<endl;
	myptri.print_row(n/2);
	cout<<endl<<"Just one coefficient:"<<endl;
	cout<<myptri.get_coeff(n/2, n/4)<<endl<<endl;
	return 0;
}


```

=={{header|C sharp|C#}}==
{{trans|Fortran}}
Produces no output when n is less than or equal to zero.


```csharp
using System;

namespace RosettaCode {

    class PascalsTriangle {

        public static void CreateTriangle(int n) {
            if (n > 0) {
                for (int i = 0; i < n; i++) {
                    int c = 1;
                    Console.Write(" ".PadLeft(2 * (n - 1 - i)));
                    for (int k = 0; k <= i; k++) {
                        Console.Write("{0}", c.ToString().PadLeft(3));
                        c = c * (i - k) / (k + 1);
                    }
                    Console.WriteLine();
                }
            }
        }

        public static void Main() {
            CreateTriangle(8);
        }
    }
}
```


===Arbitrarily large numbers (BigInteger), arbitrary row selection===

```csharp
using System;
using System.Linq;
using System.Numerics;
using System.Collections.Generic;

namespace RosettaCode
{
	public static class PascalsTriangle
	{
		public static IEnumerable<BigInteger[]> GetTriangle(int quantityOfRows)
		{
			IEnumerable<BigInteger> range = Enumerable.Range(0, quantityOfRows).Select(num => new BigInteger(num));
			return range.Select(num => GetRow(num).ToArray());
		}

		public static IEnumerable<BigInteger> GetRow(BigInteger rowNumber)
		{
			BigInteger denominator = 1;
			BigInteger numerator = rowNumber;

			BigInteger currentValue = 1;
			for (BigInteger counter = 0; counter <= rowNumber; counter++)
			{
				yield return currentValue;
				currentValue = BigInteger.Multiply(currentValue, numerator--);
				currentValue = BigInteger.Divide(currentValue, denominator++);
			}
			yield break;
		}

		public static string FormatTriangleString(IEnumerable<BigInteger[]> triangle)
		{
			int maxDigitWidth = triangle.Last().Max().ToString().Length;
			IEnumerable<string> rows = triangle.Select(arr =>
					string.Join(" ", arr.Select(array => CenterString(array.ToString(), maxDigitWidth)) )
			);
			int maxRowWidth = rows.Last().Length;
			return string.Join(Environment.NewLine, rows.Select(row => CenterString(row, maxRowWidth)));
		}

		private static string CenterString(string text, int width)
		{
			int spaces = width - text.Length;
			int padLeft = (spaces / 2) + text.Length;
			return text.PadLeft(padLeft).PadRight(width);
		}
	}
}
```


Example:

```csharp
static void Main()
{
	IEnumerable<BigInteger[]> triangle = PascalsTriangle.GetTriangle(20);
	string output = PascalsTriangle.FormatTriangleString(triangle)
	Console.WriteLine(output);
}
```


{{out}}

```txt

                                                           1
                                                        1     1
                                                     1     2     1
                                                  1     3     3     1
                                               1     4     6     4     1
                                            1     5    10    10     5     1
                                         1     6    15    20    15     6     1
                                      1     7    21    35    35    21     7     1
                                   1     8    28    56    70    56    28     8     1
                                1     9    36    84    126   126   84    36     9     1
                             1    10    45    120   210   252   210   120   45    10     1
                          1    11    55    165   330   462   462   330   165   55    11     1
                       1    12    66    220   495   792   924   792   495   220   66    12     1
                    1    13    78    286   715  1287  1716  1716  1287   715   286   78    13     1
                 1    14    91    364  1001  2002  3003  3432  3003  2002  1001   364   91    14     1
              1    15    105   455  1365  3003  5005  6435  6435  5005  3003  1365   455   105   15     1
           1    16    120   560  1820  4368  8008  11440 12870 11440 8008  4368  1820   560   120   16     1
        1    17    136   680  2380  6188  12376 19448 24310 24310 19448 12376 6188  2380   680   136   17     1
     1    18    153   816  3060  8568  18564 31824 43758 48620 43758 31824 18564 8568  3060   816   153   18     1
  1    19    171   969  3876  11628 27132 50388 75582 92378 92378 75582 50388 27132 11628 3876   969   171   19     1

```



## Clojure


For n < 1, prints nothing, always returns nil.  Copied from the Common Lisp implementation below, but with local functions and explicit tail-call-optimized recursion (recur).

```lisp
(defn pascal [n]
  (let [newrow (fn newrow [lst ret]
                   (if lst
                       (recur (rest lst)
                              (conj ret (+ (first lst) (or (second lst) 0))))
                       ret))
        genrow (fn genrow [n lst]
                   (when (< 0 n)
                     (do (println lst)
                         (recur (dec n) (conj (newrow lst []) 1)))))]
    (genrow n [1])))
(pascal 4)
```

And here's another version, using the ''partition'' function to produce the sequence of pairs in a row, which are summed and placed between two ones to produce the next row:

```lisp

(defn nextrow [row]
  (vec (concat [1] (map #(apply + %) (partition 2 1 row)) [1] )))

(defn pascal [n]
  (assert (and (integer? n) (pos? n)))
  (let [triangle (take n (iterate nextrow [1]))]
    (doseq [row triangle]
      (println row))))

```

The ''assert'' form causes the ''pascal'' function to throw an exception unless the argument is (integral and) positive.

Here's a third version using the ''iterate'' function

```lisp

(def pascal
  (iterate
    (fn [prev-row]
      (->>
        (concat [[(first prev-row)]] (partition 2 1 prev-row) [[(last prev-row)]])
        (map (partial apply +) ,,,)))
     [1]))

```


Another short version which returns an infinite pascal triangle as a list, using the iterate function.


```lisp

(def pascal
  (iterate #(concat [1]
                    (map + % (rest %))
                    [1])
           [1]))

```


One can then get the first n rows using the take function


```lisp

(take 10 pascal) ; returns a list of the first 10 pascal rows

```


Also, one can retrieve the nth row using the nth function


```lisp

(nth pascal 10) ;returns the nth row

```



## CoffeeScript

This version assumes n is an integer and n >= 1.  It efficiently computes binomial coefficients.

```coffeescript

pascal = (n) ->
  width = 6
  for r in [1..n]
    s = ws (width/2) * (n-r) # center row
    output = (n) -> s += pad width, n
    cell = 1
    output cell
    # Compute binomial coefficients as you go
    # across the row.
    for c in [1...r]
      cell *= (r-c) / c
      output cell
    console.log s

ws = (n) ->
  s = ''
  s += ' ' for i in [0...n]
  s

pad = (cnt, n) ->
  s = n.toString()
  # There is probably a better way to do this.
  cnt -= s.length
  right = Math.floor(cnt / 2)
  left = cnt - right
  ws(left) + s + ws(right)

pascal(7)


```


{{Out}}

```txt

> coffee pascal.coffee
                     1
                  1     1
               1     2     1
            1     3     3     1
         1     4     6     4     1
      1     5    10    10     5     1
   1     6    15    20    15     6     1

```



## Commodore BASIC


```BASIC
10 INPUT "HOW MANY";N
20 IF N<1 THEN END
30 DIM C(N)
40 DIM D(N)
50 LET C(1)=1
60 LET D(1)=1
70 FOR J=1 TO N
80 FOR I=1 TO N-J+1
90 PRINT "  ";
100 NEXT I
110 FOR I=1 TO J
120 PRINT C(I)" ";
130 NEXT I
140 PRINT
150 IF J=N THEN END
160 C(J+1)=1
170 D(J+1)=1
180 FOR I=1 TO J-1
190 D(I+1)=C(I)+C(I+1)
200 NEXT I
210 FOR I=1 TO J
220 C(I)=D(I)
230 NEXT I
240 NEXT J
```


Output:
<lang>RUN
HOW MANY? 8
                   1
                 1   1
               1   2   1
             1   3   3   1
           1   4   6   4   1
         1   5   10   10   5   1
       1   6   15   20   15   6   1
     1   7   21   35   35   21   7   1
   1   8   28   56   70   56   28   8    1
READY.

```



## Common Lisp

To evaluate, call (pascal n). For n < 1, it simply returns nil.


```lisp
(defun pascal (n)
   (genrow n '(1)))

(defun genrow (n l)
   (when (< 0 n)
       (print l)
       (genrow (1- n) (cons 1 (newrow l)))))

(defun newrow (l)
   (if (> 2 (length l))
      '(1)
      (cons (+ (car l) (cadr l)) (newrow (cdr l)))))
```


An iterative solution with ''loop'', using ''nconc'' instead of ''collect'' to keep track of the last ''cons''. Otherwise, it would be necessary to traverse the list to do a ''(rplacd (last a) (list 1))''.


```lisp
(defun pascal-next-row (a)
    (loop :for q :in a
          :and p = 0 :then q
          :as s = (list (+ p q))
          :nconc s :into a
          :finally (rplacd s (list 1))
                   (return a)))

(defun pascal (n)
    (loop :for a = (list 1) :then (pascal-next-row a)
          :repeat n
          :collect a))
```


Another iterative solution, this time using pretty-printing to automatically print the triangle in the shape of a triangle in the terminal. The pascal-print function determines the length of the final row and uses it to decide how wide the triangle should be.


```lisp

(defun next-pascal (l)
  `(1 ,@(loop for i from 0 to (- (length l) 2)
	      collect (+ (nth i l) (nth (1+ i) l)))
    1))

(defun pascal-print (r)
    (let* ((pasc (loop with p = (list (list 1))
		       repeat r do (nconc p (list (apply #'next-pascal (last p))))
		       finally (return p)))
	   (len (length (format nil "~A" (car (last pasc))))))
      (format t (format nil "~~{~~~D:@<~~{~~A ~~}~~>~~%~~}" len) pasc)))

```


For example:


```lisp
(pascal-print 4)
```

<lang>
    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1

```


```lisp
(pascal-print 8)
```

<lang>
           1
          1 1
         1 2 1
        1 3 3 1
       1 4 6 4 1
     1 5 10 10 5 1
   1 6 15 20 15 6 1
  1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1

```



## Component Pascal

{{Works with|BlackBox Component Builder}}

```oberon2

MODULE PascalTriangle;
IMPORT StdLog, DevCommanders, TextMappers;

TYPE
	Expansion* = POINTER TO ARRAY OF LONGINT;

PROCEDURE Show*(e: Expansion);
VAR
	i: INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(e)) & (e[i] # 0) DO
		StdLog.Int(e[i]);
		INC(i)
	END;
	StdLog.Ln
END Show;

PROCEDURE GenFor*(p: LONGINT): Expansion;
VAR
	expA,expB: Expansion;
	i,j: LONGINT;

	PROCEDURE Swap(VAR x,y: Expansion);
	VAR
		swap: Expansion;
	BEGIN
		swap := x; x := y; y := swap
	END Swap;

BEGIN
	ASSERT(p >= 0);
	NEW(expA,p + 2);NEW(expB,p + 2);
	FOR i := 0 TO p DO
		IF i = 0 THEN expA[0] := 1
		ELSE
			FOR j := 0 TO i DO
				IF j = 0 THEN
					expB[j] := expA[j]
				ELSE
					expB[j] := expA[j - 1] + expA[j]
				END
			END;
			Swap(expA,expB)
		END;
	END;
	expB := NIL; (* for the GC *)
	RETURN expA
END GenFor;


PROCEDURE Do*;
VAR
	s: TextMappers.Scanner;
	exp: Expansion;
BEGIN
	s.ConnectTo(DevCommanders.par.text);
	s.SetPos(DevCommanders.par.beg);
	s.Scan;
	WHILE (~s.rider.eot) DO
		IF (s.type = TextMappers.char) & (s.char = '~') THEN
			RETURN
		ELSIF (s.type = TextMappers.int) THEN
			exp := GenFor(s.int);
			Show(exp)
		END;
		s.Scan
	END
END Do;

END PascalTriangle.

```


```txt
Execute: ^Q PascalTriangle.Do 0 1 2 3 4 5 6 7 8 9 10 11 12~
```

{{out}}

```txt

 1
 1 1
 1 2 1
 1 3 3 1
 1 4 6 4 1
 1 5 10 10 5 1
 1 6 15 20 15 6 1
 1 7 21 35 35 21 7 1
 1 8 28 56 70 56 28 8 1
 1 9 36 84 126 126 84 36 9 1
 1 10 45 120 210 252 210 120 45 10 1
 1 11 55 165 330 462 462 330 165 55 11 1
 1 12 66 220 495 792 924 792 495 220 66 12 1

```


## D


### Less functional Version


```d
int[][] pascalsTriangle(in int rows) pure nothrow {
    auto tri = new int[][rows];
    foreach (r; 0 .. rows) {
        int v = 1;
        foreach (c; 0 .. r+1) {
            tri[r] ~= v;
            v = (v * (r - c)) / (c + 1);
        }
    }
    return tri;
}

void main() {
    immutable t = pascalsTriangle(10);
    assert(t == [[1],
                [1, 1],
               [1, 2, 1],
             [1, 3, 3, 1],
           [1, 4, 6, 4, 1],
         [1, 5, 10, 10, 5, 1],
       [1, 6, 15, 20, 15, 6, 1],
     [1, 7, 21, 35, 35, 21, 7, 1],
    [1, 8, 28, 56, 70, 56, 28, 8, 1],
  [1, 9, 36, 84, 126, 126, 84, 36, 9, 1]]);
}
```


### More functional Version


```d
import std.stdio, std.algorithm, std.range;

auto pascal() pure nothrow {
    return [1].recurrence!q{ zip(a[n - 1] ~ 0, 0 ~ a[n - 1])
                             .map!q{ a[0] + a[1] }
                             .array };
}

void main() {
    pascal.take(5).writeln;
}
```

{{out}}

```txt
[[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1]]
```



### Alternative Version

There is similarity between Pascal's triangle and [[Sierpinski triangle]].
Their difference are the initial line and the operation that act on the line element to produce next line.
The following is a generic pascal's triangle implementation for positive number of lines output (n).

```d
import std.stdio, std.string, std.array, std.format;

string Pascal(alias dg, T, T initValue)(int n) {
    string output;

    void append(in T[] l) {
        output ~= " ".replicate((n - l.length + 1) * 2);
        foreach (e; l)
            output ~= format("%4s", format("%4s", e));
        output ~= "\n";
    }

    if (n > 0) {
        T[][] lines = [[initValue]];
        append(lines[0]);
        foreach (i; 1 .. n) {
            lines ~= lines[i - 1] ~ initValue; // length + 1
            foreach (int j; 1 .. lines[i-1].length)
                lines[i][j] = dg(lines[i-1][j], lines[i-1][j-1]);
            append(lines[i]);
        }
    }
    return output;
}

string delegate(int n) genericPascal(alias dg, T, T initValue)() {
    mixin Pascal!(dg, T, initValue);
    return &Pascal;
}

void main() {
    auto pascal = genericPascal!((int a, int b) => a + b, int, 1)();
    static char xor(char a, char b) { return a == b ? '_' : '*'; }
    auto sierpinski = genericPascal!(xor, char, '*')();

    foreach (i; [1, 5, 9])
        writef(pascal(i));
    // an order 4 sierpinski triangle is a 2^4 lines generic
    // Pascal triangle with xor operation
    foreach (i; [16])
        writef(sierpinski(i));
}
```

{{out}}

```txt
     1
             1
           1   1
         1   2   1
       1   3   3   1
     1   4   6   4   1
                     1
                   1   1
                 1   2   1
               1   3   3   1
             1   4   6   4   1
           1   5  10  10   5   1
         1   6  15  20  15   6   1
       1   7  21  35  35  21   7   1
     1   8  28  56  70  56  28   8   1
                                   *
                                 *   *
                               *   _   *
                             *   *   *   *
                           *   _   _   _   *
                         *   *   _   _   *   *
                       *   _   *   _   *   _   *
                     *   *   *   *   *   *   *   *
                   *   _   _   _   _   _   _   _   *
                 *   *   _   _   _   _   _   _   *   *
               *   _   *   _   _   _   _   _   *   _   *
             *   *   *   *   _   _   _   _   *   *   *   *
           *   _   _   _   *   _   _   _   *   _   _   _   *
         *   *   _   _   *   *   _   _   *   *   _   _   *   *
       *   _   *   _   *   _   *   _   *   _   *   _   *   _   *
     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
```



## Dart


```dart

import 'dart:io';

pascal(n) {
  if(n<=0) print("Not defined");

  else if(n==1) print(1);

  else {
    List<List<int>> matrix = new List<List<int>>();
    matrix.add(new List<int>());
    matrix.add(new List<int>());
    matrix[0].add(1);
    matrix[1].add(1);
    matrix[1].add(1);
    for (var i = 2; i < n; i++) {
      List<int> list = new List<int>();
      list.add(1);
      for (var j = 1; j<i; j++) {
        list.add(matrix[i-1][j-1]+matrix[i-1][j]);
      }
      list.add(1);
      matrix.add(list);
    }
    for(var i=0; i<n; i++) {
      for(var j=0; j<=i; j++) {
        stdout.write(matrix[i][j]);
        stdout.write(' ');
      }
      stdout.write('\n');
    }
  }
}

void main() {
  pascal(0);
  pascal(1);
  pascal(3);
  pascal(6);
}




```



## Delphi


```delphi
program PascalsTriangle;

procedure Pascal(r:Integer);
var
  i, c, k:Integer;
begin
  for i := 0 to r - 1 do
  begin
    c := 1;
    for k := 0 to i do
    begin
      Write(c:3);
      c := c * (i - k) div (k + 1);
    end;
    Writeln;
  end;
end;

begin
  Pascal(9);
end.
```



## DWScript

Doesn't print anything for negative or null values.

```delphi
procedure Pascal(r : Integer);
var
   i, c, k : Integer;
begin
   for i:=0 to r-1 do begin
      c:=1;
      for k:=0 to i do begin
         Print(Format('%4d', [c]));
         c:=(c*(i-k)) div (k+1);
      end;
      PrintLn('');
   end;
end;

Pascal(9);
```

{{Out}}

```txt
   1
   1   1
   1   2   1
   1   3   3   1
   1   4   6   4   1
   1   5  10  10   5   1
   1   6  15  20  15   6   1
   1   7  21  35  35  21   7   1
   1   8  28  56  70  56  28   8   1
```



## E


So as not to bother with text layout, this implementation generates a HTML fragment. It uses a single mutable array, appending one 1 and adding to each value the preceding value.


```e
def pascalsTriangle(n, out) {
    def row := [].diverge(int)
    out.print("<table style='text-align: center; border: 0; border-collapse: collapse;'>")
    for y in 1..n {
        out.print("<tr>")
        row.push(1)
        def skip := n - y
        if (skip > 0) {
            out.print(`<td colspan="$skip"></td>`)
        }
        for x => v in row {
            out.print(`<td>$v</td><td></td>`)
        }
        for i in (1..!y).descending() {
            row[i] += row[i - 1]
        }
        out.println("</tr>")
    }
    out.print("</table>")
}
```



```e>def out := <file:triangle.html
.textWriter()
try {
    pascalsTriangle(15, out)
} finally {
    out.close()
}
makeCommand("yourFavoriteWebBrowser")("triangle.html")
```



## Eiffel



```eiffel

note
	description    : "Prints pascal's triangle"
	output         : "[
    			   Per requirements of the RosettaCode example, execution will print the first n rows of pascal's triangle
    			  ]"
	date           : "19 December 2013"
	authors        : "Sandro Meier", "Roman Brunner"
	revision       : "1.0"
	libraries      : "Relies on HASH_TABLE from EIFFEL_BASE library"
	implementation : "[
			   Recursive implementation to calculate the n'th row.
			 ]"
	warning        : "[
				Will not work for large n's (INTEGER_32)
		         ]"

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization

	make
		local
			n:INTEGER
		do
			create {HASH_TABLE[ARRAY[INTEGER],INTEGER]}pascal_lines.make (n) --create the hash_table object
			io.new_line
			n:=25
			draw(n)
		end
feature
	line(n:INTEGER):ARRAY[INTEGER]
		--Calculates the n'th line
	local
		upper_line:ARRAY[INTEGER]
		i:INTEGER
	do
		if	n=1 then	--trivial case first line
			create Result.make_filled (0, 1, n+2)
			Result.put (0, 1)
			Result.put (1, 2)
			Result.put (0, 3)
		elseif pascal_lines.has (n) then	--checks if the result was already calculated
			Result := pascal_lines.at (n)
		else	--calculates the n'th line recursively
			create Result.make_filled(0,1,n+2) --for caluclation purposes add a 0 at the beginning of each line
			Result.put (0, 1)
			upper_line:=line(n-1)
			from
				i:=1
			until
				i>upper_line.count-1
			loop
				Result.put(upper_line[i]+upper_line[i+1],i+1)
				i:=i+1
			end
			Result.put (0, n+2)	--for caluclation purposes add a 0 at the end of each line
			pascal_lines.put (Result, n)
		end
	end

	draw(n:INTEGER)
		--draw n lines of pascal's triangle
	local
		space_string:STRING
		width, i:INTEGER

	do
		space_string:=" "		--question of design: add space_string at the beginning of each line
		width:=line(n).count
		space_string.multiply (width)
		from
			i:=1
		until
			i>n
		loop
			space_string.remove_tail (1)
			io.put_string (space_string)
			across line(i) as c
			loop
				if
					c.item/=0
				then
					io.put_string (c.item.out+" ")
				end
			end
			io.new_line
			i:=i+1
		end
	end

feature --Access
	pascal_lines:HASH_TABLE[ARRAY[INTEGER],INTEGER]
		--Contains all already calculated lines
end

```



## Elixir


```elixir
defmodule Pascal do
  def triangle(n), do: triangle(n,[1])

  def triangle(0,list), do: list
  def triangle(n,list) do
    IO.inspect list
    new_list = Enum.zip([0]++list, list++[0]) |> Enum.map(fn {a,b} -> a+b end)
    triangle(n-1,new_list)
  end
end

Pascal.triangle(8)
```


{{out}}

```txt

[1]
[1, 1]
[1, 2, 1]
[1, 3, 3, 1]
[1, 4, 6, 4, 1]
[1, 5, 10, 10, 5, 1]
[1, 6, 15, 20, 15, 6, 1]
[1, 7, 21, 35, 35, 21, 7, 1]

```



## Erlang



```erlang

-import(lists).
-export([pascal/1]).

pascal(1)-> [[1]];
pascal(N) ->
    L = pascal(N-1),
    [H|_] = L,
    [lists:zipwith(fun(X,Y)->X+Y end,[0]++H,H++[0])|L].

```


{{Out}}

```txt

  Eshell V5.5.5  (abort with ^G)
  1> pascal:pascal(5).
  [[1,4,6,4,1],[1,3,3,1],[1,2,1],[1,1],[1]]

```



## ERRE


```ERRE

PROGRAM PASCAL_TRIANGLE

PROCEDURE PASCAL(R%)
  LOCAL I%,C%,K%
    FOR I%=0 TO R%-1 DO
      C%=1
      FOR K%=0 TO I% DO
        WRITE("###";C%;)
        C%=(C%*(I%-K%)) DIV (K%+1)
      END FOR
      PRINT
   END FOR
END PROCEDURE

BEGIN
  PASCAL(9)
END PROGRAM

```

Output:

```txt

  1
  1  1
  1  2  1
  1  3  3  1
  1  4  6  4  1
  1  5 10 10  5  1
  1  6 15 20 15  6  1
  1  7 21 35 35 21  7  1
  1  8 28 56 70 56 28  8  1

```



## Euphoria


### Summing from Previous Rows


```Euphoria
sequence row
row = {}
for m = 1 to 10 do
    row = row & 1
    for n = length(row)-1 to 2 by -1 do
        row[n] += row[n-1]
    end for
    print(1,row)
    puts(1,'\n')
end for
```


{{Out}}

```txt

 {1}
 {1,1}
 {1,2,1}
 {1,3,3,1}
 {1,4,6,4,1}
 {1,5,10,10,5,1}
 {1,6,15,20,15,6,1}
 {1,7,21,35,35,21,7,1}
 {1,8,28,56,70,56,28,8,1}
 {1,9,36,84,126,126,84,36,9,1}

```


=={{header|F Sharp|F#}}==

```fsharp
let rec nextrow l =
    match l with
    | []      -> []
    | h :: [] -> [1]
    | h :: t  -> h + t.Head :: nextrow t

let pascalTri n = List.scan(fun l i -> 1 :: nextrow l) [1] [1 .. n]

for row in pascalTri(10) do
    for i in row do
        printf "%s" (i.ToString() + ", ")
    printfn "%s" "\n"

```



## Factor


This implementation works by summing the previous line content. Result for n < 1 is the same as for n == 1.


```factor
USING: grouping kernel math sequences ;

: (pascal) ( seq -- newseq )
    dup last 0 prefix 0 suffix 2 <clumps> [ sum ] map suffix ;

: pascal ( n -- seq )
    1 - { { 1 } } swap [ (pascal) ] times ;
```


It works as:


```factor
5 pascal .
{ { 1 } { 1 1 } { 1 2 1 } { 1 3 3 1 } { 1 4 6 4 1 } }
```



## Fantom



```fantom

class Main
{
  Int[] next_row (Int[] row)
  {
    new_row := [1]
    (row.size-1).times |i|
    {
      new_row.add (row[i] + row[i+1])
    }
    new_row.add (1)

    return new_row
  }

  Void print_pascal (Int n)  // no output for n <= 0
  {
    current_row := [1]
    n.times
    {
      echo (current_row.join(" "))
      current_row = next_row (current_row)
    }
  }

  Void main ()
  {
    print_pascal (10)
  }
}

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Pascal%27s_triangle this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: init ( n -- )
  here swap cells erase  1 here ! ;
: .line ( n -- )
  cr here swap 0 do dup @ . cell+ loop drop ;
: next ( n -- )
  here swap 1- cells here + do
    i @ i cell+ +!
  -1 cells +loop ;
: pascal ( n -- )
      dup init   1  .line
  1 ?do i next i 1+ .line loop ;
```

This is a bit more efficient.
{{trans|C}}

```forth
: PascTriangle
  cr dup 0
  ?do
     1 over 1- i - 2* spaces i 1+ 0 ?do dup 4 .r j i - * i 1+ / loop cr drop
  loop drop
;

13 PascTriangle
```



## Fortran

{{works with|Fortran|90 and later}}
Prints nothing for n<=0. Output formatting breaks down for n>20

```fortran
PROGRAM Pascals_Triangle

  CALL Print_Triangle(8)

END PROGRAM Pascals_Triangle

SUBROUTINE Print_Triangle(n)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER :: c, i, j, k, spaces

  DO i = 0, n-1
     c = 1
     spaces = 3 * (n - 1 - i)
     DO j = 1, spaces
        WRITE(*,"(A)", ADVANCE="NO") " "
     END DO
     DO k = 0, i
        WRITE(*,"(I6)", ADVANCE="NO") c
        c = c * (i - k) / (k + 1)
     END DO
     WRITE(*,*)
  END DO

END SUBROUTINE Print_Triangle
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub pascalTriangle(n As UInteger)
  If n = 0 Then Return
  Dim prevRow(1 To n) As UInteger
  Dim currRow(1 To n) As UInteger
  Dim start(1 To n) As UInteger  ''stores starting column for each row
  start(n) = 1
  For i As Integer = n - 1 To 1 Step -1
    start(i) = start(i + 1) + 3
  Next
  prevRow(1) = 1
  Print Tab(start(1));
  Print 1U
  For i As UInteger = 2 To n
    For j As UInteger = 1 To i
      If j = 1 Then
        Print Tab(start(i)); "1";
        currRow(1) = 1
      ElseIf j = i Then
        Print "     1"
        currRow(i) = 1
      Else
        currRow(j) = prevRow(j - 1) + prevRow(j)
        Print Using "######"; currRow(j); "    ";
      End If
    Next j
    For j As UInteger = 1 To i
      prevRow(j) = currRow(j)
    Next j
  Next i
End Sub

pascalTriangle(14)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

                                       1
                                    1     1
                                 1     2     1
                              1     3     3     1
                           1     4     6     4     1
                        1     5    10    10     5     1
                     1     6    15    20    15     6     1
                  1     7    21    35    35    21     7     1
               1     8    28    56    70    56    28     8     1
            1     9    36    84   126   126    84    36     9     1
         1    10    45   120   210   252   210   120    45    10     1
      1    11    55   165   330   462   462   330   165    55    11     1
   1    12    66   220   495   792   924   792   495   220    66    12     1
1    13    78   286   715  1287  1716  1716  1287   715   286    78    13     1

```



## FunL


###  Summing from Previous Rows

{{trans|Scala}}

```funl
import lists.zip

def
  pascal( 1 ) = [1]
  pascal( n ) = [1] + map( (a, b) -> a + b, zip(pascal(n-1), pascal(n-1).tail()) ) + [1]
```



###  Combinations

{{trans|Haskell}}

```funl
import integers.choose

def pascal( n ) = [choose( n - 1, k ) | k <- 0..n-1]
```


=== Pascal's Triangle ===

```funl
def triangle( height ) =
  width = max( map(a -> a.toString().length(), pascal(height)) )

  if 2|width
    width++

  for n <- 1..height
    print( ' '*((width + 1)\2)*(height - n) )
    println( map(a -> format('%' + width + 'd ', a), pascal(n)).mkString() )

triangle( 10 )
```


{{out}}

```txt

                    1
                  1   1
                1   2   1
              1   3   3   1
            1   4   6   4   1
          1   5  10  10   5   1
        1   6  15  20  15   6   1
      1   7  21  35  35  21   7   1
    1   8  28  56  70  56  28   8   1
  1   9  36  84 126 126  84  36   9   1

```



## GAP


```gap
Pascal := function(n)
	local i, v;
	v := [1];
	for i in [1 .. n] do
		Display(v);
		v := Concatenation([0], v) + Concatenation(v, [0]);
	od;
end;

Pascal(9);
# [ 1 ]
# [ 1, 1 ]
# [ 1, 2, 1 ]
# [ 1, 3, 3, 1 ]
# [ 1, 4, 6, 4, 1 ]
# [ 1, 5, 10, 10, 5, 1 ]
# [ 1, 6, 15, 20, 15, 6, 1 ]
# [ 1, 7, 21, 35, 35, 21, 7, 1 ]
# [ 1, 8, 28, 56, 70, 56, 28, 8, 1 ]
```



## Go

No output for n < 1.  Otherwise, output formatted left justified.

```go

package main

import "fmt"

func printTriangle(n int) {
    // degenerate cases
    if n <= 0 {
        return
    }
    fmt.Println(1)
    if n == 1 {
        return
    }
    // iterate over rows, zero based
    a := make([]int, (n+1)/2)
    a[0] = 1
    for row, middle := 1, 0; row < n; row++ {
        // generate new row
        even := row&1 == 0
        if even {
            a[middle+1] = a[middle] * 2
        }
        for i := middle; i > 0; i-- {
            a[i] += a[i-1]
        }
        // print row
        for i := 0; i <= middle; i++ {
            fmt.Print(a[i], " ")
        }
        if even {
            middle++
        }
        for i := middle; i >= 0; i-- {
            fmt.Print(a[i], " ")
        }
        fmt.Println("")
    }
}

func main() {
    printTriangle(4)
}

```

Output:

```txt

1
1 1
1 2 1
1 3 3 1

```



## Groovy


###  Recursive

In the spirit of the Haskell "think in whole lists" solution here is a list-driven, minimalist solution:

```groovy
def pascal
pascal = { n -> (n <= 1) ? [1] : [[0] + pascal(n - 1), pascal(n - 1) + [0]].transpose().collect { it.sum() } }
```

However, this solution is horribly inefficient (O(''n''**2)). It slowly grinds to a halt on a reasonably powerful PC after about line 25 of the triangle.

Test program:

```groovy
def count = 15
(1..count).each { n ->
    printf ("%2d:", n); (0..(count-n)).each { print "    " }; pascal(n).each{ printf("%6d  ", it) }; println ""
}
```


{{out}}

```txt
 1:                                                                 1
 2:                                                             1       1
 3:                                                         1       2       1
 4:                                                     1       3       3       1
 5:                                                 1       4       6       4       1
 6:                                             1       5      10      10       5       1
 7:                                         1       6      15      20      15       6       1
 8:                                     1       7      21      35      35      21       7       1
 9:                                 1       8      28      56      70      56      28       8       1
10:                             1       9      36      84     126     126      84      36       9       1
11:                         1      10      45     120     210     252     210     120      45      10       1
12:                     1      11      55     165     330     462     462     330     165      55      11       1
13:                 1      12      66     220     495     792     924     792     495     220      66      12       1
14:             1      13      78     286     715    1287    1716    1716    1287     715     286      78      13       1
15:         1      14      91     364    1001    2002    3003    3432    3003    2002    1001     364      91      14       1
```


=={{header|GW-BASIC}}==

```qbasic
10 INPUT "Number of rows? ",R
20 FOR I=0 TO R-1
30 C=1
40 FOR K=0 TO I
50 PRINT USING "####";C;
60 C=C*(I-K)/(K+1)
70 NEXT
80 PRINT
90 NEXT
```


Output:

```txt

Number of rows? 7
   1
   1   1
   1   2   1
   1   3   3   1
   1   4   6   4   1
   1   5  10  10   5   1
   1   6  15  20  15   6   1

```



## Haskell

An approach using the "think in whole lists" principle: Each row in
the triangle can be calculated from the previous row by adding a
shifted version of itself to it, keeping the ones at the ends. The
Prelude function ''zipWith'' can be used to add two lists, but it
won't keep the old values when one list is shorter. So we need a
similar function


```haskell
zapWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zapWith f xs [] = xs
zapWith f [] ys = ys
zapWith f (x:xs) (y:ys) = f x y : zapWith f xs ys
```


Now we can shift a list and add it to itself, extending it by keeping
the ends:


```haskell
extendWith f [] = []
extendWith f xs@(x:ys) = x : zapWith f xs ys
```


And for the whole (infinite) triangle, we just iterate this operation,
starting with the first row:


```haskell
pascal = iterate (extendWith (+)) [1]
```


For the first ''n'' rows, we just take the first ''n'' elements from this
list, as in


```haskell
*Main> take 6 pascal
[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
```


A shorter approach, plagiarized from [http://www.haskell.org/haskellwiki/Blow_your_mind]

```haskell
-- generate next row from current row
nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

-- returns the first n rows
pascal = iterate nextRow [1]
```


Alternatively, using list comprehensions:


```haskell

pascal :: [[Integer]]
pascal =
  (1 : [ 0 | _ <- head pascal])
  : [zipWith (+) (0:row) row | row <- pascal]

```



```haskell

*Pascal> take 5 <$> (take 5 $ triangle)
[[1,0,0,0,0],[1,1,0,0,0],[1,2,1,0,0],[1,3,3,1,0],[1,4,6,4,1]]

```


With binomial coefficients:

```haskell
fac = product . enumFromTo 1

binCoef n k = (fac n) `div` ((fac k) * (fac $ n - k))

pascal n = map (binCoef $ n - 1) [0..n-1]
```


Example:

```haskell
*Main> putStr $ unlines $ map unwords $ map (map show) $ pascal 10
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1
1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1
1 9 36 84 126 126 84 36 9 1

```



## HicEst


```HicEst
   CALL Pascal(30)

SUBROUTINE Pascal(rows)
   CHARACTER fmt*6
   WRITE(Text=fmt, Format='"i", i5.5') 1+rows/4

   DO row = 0, rows-1
     n = 1
     DO k = 0, row
       col = rows*(rows-row+2*k)/4
       WRITE(Row=row+1, Column=col, F=fmt) n
       n = n * (row - k) / (k + 1)
     ENDDO
   ENDDO
END
```

=={{header|Icon}} and {{header|Unicon}}==
The code below is slightly modified from the library version of pascal which prints 0's to the full width of the carpet.
It also presents the data as an isoceles triangle.

```Icon
link math

procedure main(A)
every n := !A do  {    # for each command line argument
   n := integer(\n) | &null
   pascal(n)
   }
end

procedure pascal(n)		#: Pascal triangle
   /n := 16
   write("width=", n, " height=", n)	# carpet header
   fw := *(2 ^ n)+1
   every i := 0 to n - 1 do {
      writes(repl(" ",fw*(n-i)/2))
      every j := 0 to n - 1 do
         writes(center(binocoef(i, j),fw) | break)
      write()
      }
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/math.icn math provides binocoef]
[http://www.cs.arizona.edu/icon/library/src/procs/pascal.icn math provides the original version of pascal]

Sample output:
```txt
->pascal 1 4 8
width=1 height=1
 1
width=4 height=4
       1
     1  1
    1  2  1
  1  3  3  1
width=8 height=8
                 1
               1   1
             1   2   1
           1   3   3   1
         1   4   6   4   1
       1   5   10  10  5   1
     1   6   15  20  15  6   1
   1   7   21  35  35  21  7   1
->
```



## IDL


```IDL
Pro Pascal, n
;n is the number of lines of the triangle to be displayed
 r=[1]
 print, r
  for i=0, (n-2) do begin
    pascalrow,r
  endfor
End

Pro PascalRow, r
  for i=0,(n_elements(r)-2) do begin
    r[i]=r[i]+r[i+1]
  endfor
r= [1, r]
print, r

End
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "PascalTr.bas"
110 TEXT 80
120 LET ROW=12
130 FOR I=0 TO ROW
140   LET C=1
150   PRINT TAB(37-I*3);
160   FOR K=0 TO I
170     PRINT USING " #### ":C;
180     LET C=C*(I-K)/(K+1)
190   NEXT
200   PRINT
210 NEXT
```


{{out}}

```txt
                                         1
                                      1     1
                                   1     2     1
                                1     3     3     1
                             1     4     6     4     1
                          1     5    10    10     5     1
                       1     6    15    20    15     6     1
                    1     7    21    35    35    21     7     1
                 1     8    28    56    70    56    28     8     1
              1     9    36    84   126   126    84    36     9     1
           1    10    45   120   210   252   210   120    45    10     1
        1    11    55   165   330   462   462   330   165    55    11     1
     1    12    66   220   495   792   924   792   495   220    66    12     1
```



## J


```j
   !~/~ i.5
1 0 0 0 0
1 1 0 0 0
1 2 1 0 0
1 3 3 1 0
1 4 6 4 1
```



```j
   ([: ":@-.&0"1 !~/~)@i. 5
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
```



```j
   (-@|. |."_1 [: ":@-.&0"1 !~/~)@i. 5
     1
    1 1
   1 2 1
  1 3 3 1
 1 4 6 4 1
```


See the [[Talk:Pascal's_triangle#J_Explanation|talk page]] for explanation of earlier version

See also [[Pascal_matrix_generation#J|Pascal matrix generation]] and [[Sierpinski_triangle#J|Sierpinski triangle]].


## Java


### Summing from Previous Rows

{{works with|Java|1.5+}}

```java
import java.util.ArrayList;
...//class definition, etc.
public static void genPyrN(int rows){
	if(rows < 0) return;
	//save the last row here
	ArrayList<Integer> last = new ArrayList<Integer>();
	last.add(1);
	System.out.println(last);
	for(int i= 1;i <= rows;++i){
		//work on the next row
		ArrayList<Integer> thisRow= new ArrayList<Integer>();
		thisRow.add(last.get(0)); //beginning
		for(int j= 1;j < i;++j){//loop the number of elements in this row
			//sum from the last row
			thisRow.add(last.get(j - 1) + last.get(j));
		}
		thisRow.add(last.get(0)); //end
		last= thisRow;//save this row
		System.out.println(thisRow);
	}
}
```



### Combinations

This method is limited to 21 rows because of the limits of <tt>long</tt>. Calling <tt>pas</tt> with an argument of 22 or above will cause intermediate math to wrap around and give false answers.

```java
public class Pas{
	public static void main(String[] args){
		//usage
		pas(20);
	}

	public static void pas(int rows){
		for(int i = 0; i < rows; i++){
			for(int j = 0; j <= i; j++){
				System.out.print(ncr(i, j) + " ");
			}
			System.out.println();
		}
	}

	public static long ncr(int n, int r){
		return fact(n) / (fact(r) * fact(n - r));
	}

	public static long fact(int n){
		long ans = 1;
		for(int i = 2; i <= n; i++){
			ans *= i;
		}
		return ans;
	}
}
```



### Using arithmetic calculation of each row element

This method is limited to 30 rows because of the limits of integer calculations (probably when calculating the multiplication). If m is declared as long then 62 rows can be printed.

```java

public class Pascal {
	private static void printPascalLine (int n) {
		if (n < 1)
			return;
		int m = 1;
		System.out.print("1 ");
		for (int j=1; j<n; j++) {
			m = m * (n-j)/j;
			System.out.print(m);
			System.out.print(" ");
		}
		System.out.println();
	}

	public static void printPascal (int nRows) {
		for(int i=1; i<=nRows; i++)
			printPascalLine(i);
	}
}

```



## JavaScript


### ES5


### =Imperative=

{{works with|SpiderMonkey}}
{{works with|V8}}

```javascript
// Pascal's triangle object
function pascalTriangle (rows) {

	// Number of rows the triangle contains
	this.rows = rows;

	// The 2D array holding the rows of the triangle
	this.triangle = new Array();
	for (var r = 0; r < rows; r++) {
		this.triangle[r] = new Array();
		for (var i = 0; i <= r; i++) {
			if (i == 0 || i == r)
				this.triangle[r][i] = 1;
			else
				this.triangle[r][i] = this.triangle[r-1][i-1]+this.triangle[r-1][i];
		}
	}

	// Method to print the triangle
	this.print = function(base) {
		if (!base)
			base = 10;

		// Private method to calculate digits in number
		var digits = function(n,b) {
			var d = 0;
			while (n >= 1) {
				d++;
				n /= b;
			}
			return d;
		}

		// Calculate max spaces needed
		var spacing = digits(this.triangle[this.rows-1][Math.round(this.rows/2)],base);

		// Private method to add spacing between numbers
		var insertSpaces = function(s) {
			var buf = "";
			while (s > 0) {
				s--;
				buf += " ";
			}
			return buf;
		}

		// Print the triangle line by line
		for (var r = 0; r < this.triangle.length; r++) {
			var l = "";
			for (var s = 0; s < Math.round(this.rows-1-r); s++) {
				l += insertSpaces(spacing);
			}
			for (var i = 0; i < this.triangle[r].length; i++) {
				if (i != 0)
					l += insertSpaces(spacing-Math.ceil(digits(this.triangle[r][i],base)/2));
				l += this.triangle[r][i].toString(base);
				if (i < this.triangle[r].length-1)
					l += insertSpaces(spacing-Math.floor(digits(this.triangle[r][i],base)/2));
			}
			print(l);
		}
	}

}

// Display 4 row triangle in base 10
var tri = new pascalTriangle(4);
tri.print();
// Display 8 row triangle in base 16
tri = new pascalTriangle(8);
tri.print(16);
```

Output:

```txt
$ d8 pascal.js
   1
  1 1
 1 2 1
1 3 3 1
              1
            1   1
          1   2   1
        1   3   3   1
      1   4   6   4   1
    1   5   a   a   5   1
  1   6   f   14  f   6   1
1   7   15  23  23  15  7   1
```



### =Functional=

{{Trans|Haskell}}

```JavaScript
(function (n) {
    'use strict';

    // PASCAL TRIANGLE --------------------------------------------------------

    // pascal :: Int -> [[Int]]
    function pascal(n) {
        return foldl(function (a) {
            var xs = a.slice(-1)[0]; // Previous row
            return append(a, [zipWith(
                function (a, b) {
                    return a + b;
                },
                append([0], xs),
                append(xs, [0])
            )]);
        }, [
            [1] // Initial seed row
        ], enumFromTo(1, n - 1));
    };


    // GENERIC FUNCTIONS ------------------------------------------------------

    // (++) :: [a] -> [a] -> [a]
    function append(xs, ys) {
        return xs.concat(ys);
    };

    // enumFromTo :: Int -> Int -> [Int]
    function enumFromTo(m, n) {
        return Array.from({
            length: Math.floor(n - m) + 1
        }, function (_, i) {
            return m + i;
        });
    };

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    function foldl(f, a, xs) {
        return xs.reduce(f, a);
    };

    // foldr (a -> b -> b) -> b -> [a] -> b
    function foldr(f, a, xs) {
        return xs.reduceRight(f, a);
    };

    // map :: (a -> b) -> [a] -> [b]
    function map(f, xs) {
        return xs.map(f);
    };

    // min :: Ord a => a -> a -> a
    function min(a, b) {
        return b < a ? b : a;
    };

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    function zipWith(f, xs, ys) {
        return Array.from({
            length: min(xs.length, ys.length)
        }, function (_, i) {
            return f(xs[i], ys[i]);
        });
    };

    // TEST and FORMAT --------------------------------------------------------
    var lstTriangle = pascal(n);

    // [[a]] -> bool -> s -> s
    function wikiTable(lstRows, blnHeaderRow, strStyle) {
        return '{| class="wikitable" ' + (strStyle ? 'style="' + strStyle +
                '"' : '') + lstRows.map(function (lstRow, iRow) {
                var strDelim = blnHeaderRow && !iRow ? '!' : '|';
                return '\n|-\n' + strDelim + ' ' + lstRow.map(function (v) {
                        return typeof v === 'undefined' ? ' ' : v;
                    })
                    .join(' ' + strDelim + strDelim + ' ');
            })
            .join('') + '\n|}';
    }

    var lstLastLine = lstTriangle.slice(-1)[0],
        lngBase = lstLastLine.length * 2 - 1,
        nWidth = lstLastLine.reduce(function (a, x) {
            var d = x.toString()
                .length;
            return d > a ? d : a;
        }, 1) * lngBase;

    return [wikiTable(lstTriangle.map(function (lst) {
            return lst.join(';;')
                .split(';');
        })
        .map(function (line, i) {
            var lstPad = Array((lngBase - line.length) / 2);
            return lstPad.concat(line)
                .concat(lstPad);
        }), false, 'text-align:center;width:' + nWidth + 'em;height:' + nWidth +
        'em;table-layout:fixed;'), JSON.stringify(lstTriangle)].join('\n\n');
})(7);
```

{{Out}}
{| class="wikitable" style="text-align:center;width:26em;height:26em;table-layout:fixed;"
|-
|  ||  ||  ||  ||  ||  || 1 ||  ||  ||  ||  ||  ||
|-
|  ||  ||  ||  ||  || 1 ||  || 1 ||  ||  ||  ||  ||
|-
|  ||  ||  ||  || 1 ||  || 2 ||  || 1 ||  ||  ||  ||
|-
|  ||  ||  || 1 ||  || 3 ||  || 3 ||  || 1 ||  ||  ||
|-
|  ||  || 1 ||  || 4 ||  || 6 ||  || 4 ||  || 1 ||  ||
|-
|  || 1 ||  || 5 ||  || 10 ||  || 10 ||  || 5 ||  || 1 ||
|-
| 1 ||  || 6 ||  || 15 ||  || 20 ||  || 15 ||  || 6 ||  || 1
|}


```JavaScript
[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1]]
```



### ES6


```JavaScript
(() => {
    'use strict';

    const main = () =>
        showPascal(take(7, pascal()));


    // pascal :: Generator [[Int]]
    const pascal = () =>
        iterate(
            xs => zipWith(
                plus,
                append([0], xs), append(xs, [0])
            ),
            [1]
        );

    // showPascal :: [[Int]] -> String
    const showPascal = xs => {
        const
            w = length(intercalate('   ', last(xs))),
            align = xs => center(w, ' ', intercalate('   ', xs));
        return unlines(map(align, xs));
    };


    // GENERIC FUNCTIONS ----------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // append (++) :: [a] -> [a] -> [a]
    // append (++) :: String -> String -> String
    const append = (xs, ys) => xs.concat(ys);

    // Size of space -> filler Char -> String -> Centered String

    // center :: Int -> Char -> String -> String
    const center = (n, c, s) => {
        const
            qr = quotRem(n - s.length, 2),
            q = qr[0];
        return replicateString(q, c) +
            s + replicateString(q + qr[1], c);
    };

    // intercalate :: String -> [String] -> String
    const intercalate = (s, xs) =>
        xs.join(s);

    // iterate :: (a -> a) -> a -> Generator [a]
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

    // Returns Infinity over objects without finite length
    // this enables zip and zipWith to choose the shorter
    // argument when one non-finite like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs => xs.length || Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // plus :: Num -> Num -> Num
    const plus = (a, b) => a + b;

    // quotRem :: Int -> Int -> (Int, Int)
    const quotRem = (m, n) =>
        Tuple(Math.floor(m / n), m % n);

    // replicateString :: Int -> String -> String
    const replicateString = (n, s) => s.repeat(n);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        xs.constructor.constructor.name !== 'GeneratorFunction' ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // Use of `take` and `length` here allows zipping with non-finite lists
    // i.e. generators like cycle, repeat, iterate.

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i], bs[i], i));
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
             1
           1   1
         1   2   1
       1   3   3   1
     1   4   6   4   1
  1   5   10   10   5   1
1   6   15   20   15   6   1
```



## jq

{{works with|jq|1.4}}
pascal(n) as defined here produces a stream of n arrays,
each corresponding to a row of the Pascal triangle.
The implementation avoids any arithmetic except addition.

```jq
# pascal(n) for n>=0; pascal(0) emits an empty stream.
def pascal(n):
  def _pascal:  # input: the previous row
    . as $in
    | .,
      if length >= n then empty
      else
        reduce range(0;length-1) as $i
          ([1]; . + [ $in[$i] + $in[$i + 1] ]) + [1] | _pascal
      end;
  if n <= 0 then empty else [1] | _pascal end ;
```

'''Example''':
 pascal(5)
{{ Out }}

```sh
$ jq -c -n -f pascal_triangle.jq
[1]
[1,1]
[1,2,1]
[1,3,3,1]
[1,4,6,4,1]
```


'''Using recurse/1'''

Here is an equivalent implementation that uses the built-in filter, recurse/1, instead of the inner function.

```jq
def pascal(n):
  if n <= 0 then empty
  else [1]
  | recurse( if length >= n then empty
             else . as $in
             | reduce range(0;length-1) as $i
                 ([1]; . + [ $in[$i] + $in[$i + 1] ]) + [1]
             end)
  end;
```


## Julia


 function pascal(n)
   if n<=0
    print("n has to have a positive value")
   end
   x=0
   while x<=n
    for a=0:x
     print(binomial(x,a))
    end
    println("")
    x+=1
   end
 end

```txt

pascal(4)
1
11
121
1331
14641

```


Another solution using matrix exponentiation.


```Julia

iround(x) = round(Int64, x)

triangle(n) = iround.(expm(diagm(1:n, -1)))

function pascal(n)
  println.(join.([filter(!iszero, triangle(n)[i,:]) for i in 1:(n+1)], " "))
  return
end

```


{{Out}}


```txt

pascal(5)
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1

```



## K


```K

pascal:{(x-1){+':x,0}\1}
pascal 6
(1
 1 1
 1 2 1
 1 3 3 1
 1 4 6 4 1
 1 5 10 10 5 1)
```



## Kotlin


```kotlin
fun pas(rows: Int) {
    for (i in 0..rows - 1) {
        for (j in 0..i)
            print(ncr(i, j).toString() + " ")
        println()
    }
}

fun ncr(n: Int, r: Int) = fact(n) / (fact(r) * fact(n - r))

fun fact(n: Int) : Long {
    var ans = 1.toLong()
    for (i in 2..n)
        ans *= i
    return ans
}

fun main(args: Array<String>) = pas(args[0].toInt())
```



## Liberty BASIC


```lb
input "How much rows would you like? "; n
dim a$(n)

for i=  0 to n
       c = 1
       o$ =""
       for k =0 to i
             o$ =o$ ; c; " "
             c =c *(i-k)/(k+1)
       next k
       a$(i)=o$
next i

maxLen = len(a$(n))
for i=  0 to n
    print space$((maxLen-len(a$(i)))/2);a$(i)
next i

end
```



## Locomotive Basic



```locobasic
10 CLS
20 INPUT "Number of rows? ", rows:GOSUB 40
30 END
40 FOR i=0 TO rows-1
50 c=1
60 FOR k=0 TO i
70 PRINT USING "####";c;
80 c=c*(i-k)/(k+1)
90 NEXT
100 PRINT
110 NEXT
120 RETURN
```


Output:


```txt

Number of rows? 7
   1
   1   1
   1   2   1
   1   3   3   1
   1   4   6   4   1
   1   5  10  10   5   1
   1   6  15  20  15   6   1

```



## Logo


```logo
to pascal :n
  if :n = 1 [output [1]]
  localmake "a pascal :n-1
  output (sentence first :a (map "sum butfirst :a butlast :a) last :a)
end

for [i 1 10] [print pascal :i]
```


## Lua


```lua

function nextrow(t)
  local ret = {}
  t[0], t[#t+1] = 0, 0
  for i = 1, #t do ret[i] = t[i-1] + t[i] end
  return ret
end

function triangle(n)
  t = {1}
  for i = 1, n do
    print(unpack(t))
    t = nextrow(t)
  end
end

```



## Maple


```maple
f:=n->seq(print(seq(binomial(i,k),k=0..i)),i=0..n-1);

f(3);
```

    1
   1 1
  1 2 1


## Mathematica


```Mathematica
Column[StringReplace[ToString /@ Replace[MatrixExp[SparseArray[
{Band[{2,1}] -> Range[n-1]},{n,n}]],{x__,0..}->{x},2] ,{"{"|"}"|","->" "}], Center]
```

[[File:MmaPascal.png]]

=={{header|MATLAB}} / {{header|Octave}}==

A matrix containing the pascal triangle can be obtained this way:

```MATLAB
pascal(n);
```



```txt
>> pascal(6)
ans =

     1     1     1     1     1     1
     1     2     3     4     5     6
     1     3     6    10    15    21
     1     4    10    20    35    56
     1     5    15    35    70   126
     1     6    21    56   126   252


```


The binomial coefficients can be extracted from the Pascal triangle in this way:

```MATLAB
  binomCoeff = diag(rot90(pascal(n)))',
```



```txt
>> for k=1:6,diag(rot90(pascal(k)))', end
ans =  1
ans =

   1   1

ans =

   1   2   1

ans =

   1   3   3   1

ans =

   1   4   6   4   1

ans =

    1    5   10   10    5    1


```

Another way to get a formated pascals triangle is to use the convolution method:

```txt
>>
x = [1  1] ;
y = 1;
for k=8:-1:1
    fprintf(['%', num2str(k), 'c'], zeros(1,3)),
    fprintf('%6d', y), fprintf('\n')
   y = conv(y,x);

end

```

The result is:

```txt
>>

                          1
                       1     1
                    1     2     1
                 1     3     3     1
              1     4     6     4     1
           1     5    10    10     5     1
        1     6    15    20    15     6     1
     1     7    21    35    35    21     7     1

```



## Maxima


```maxima
sjoin(v, j) := apply(sconcat, rest(join(makelist(j, length(v)), v)))$

display_pascal_triangle(n) := for i from 0 thru 6 do disp(sjoin(makelist(binomial(i, j), j, 0, i), " "));

display_pascal_triangle(6);
/* "1"
   "1 1"
   "1 2 1"
   "1 3 3 1"
   "1 4 6 4 1"
   "1 5 10 10 5 1"
   "1 6 15 20 15 6 1" */
```




## Metafont


(The formatting starts to be less clear when numbers start to have more than two digits)


```metafont
vardef bincoeff(expr n, k) =
save ?;
? := (1 for i=(max(k,n-k)+1) upto n: * i endfor )
     / (1 for i=2 upto min(k, n-k): * i endfor); ?
enddef;

def pascaltr expr c =
  string s_;
  for i := 0 upto (c-1):
    s_ := "" for k=0 upto (c-i): & "  " endfor;
    s_ := s_ for k=0 upto i: & decimal(bincoeff(i,k))
             & "  " if bincoeff(i,k)<9: & " " fi endfor;
    message s_;
  endfor
enddef;

pascaltr(4);
end
```



## Microsoft Small Basic

{{trans|GW-BASIC}}

```microsoftsmallbasic

TextWindow.Write("Number of rows? ")
r = TextWindow.ReadNumber()
For i = 0 To r - 1
  c = 1
  For k = 0 To i
    TextWindow.CursorLeft = (k + 1) * 4 - Text.GetLength(c)
    TextWindow.Write(c)
    c = c * (i - k) / (k + 1)
  EndFor
  TextWindow.WriteLine("")
EndFor

```


Output:

```txt

Number of rows? 7
   1
   1   1
   1   2   1
   1   3   3   1
   1   4   6   4   1
   1   5  10  10   5   1
   1   6  15  20  15   6   1

```


=={{header|Modula-2}}==

```modula2
MODULE Pascal;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE PrintLine(n : INTEGER);
VAR
    buf : ARRAY[0..63] OF CHAR;
    m,j : INTEGER;
BEGIN
    IF n<1 THEN RETURN END;
    m := 1;
    WriteString("1 ");
    FOR j:=1 TO n-1 DO
        m := m * (n - j) DIV j;
        FormatString("%i ", buf, m);
        WriteString(buf)
    END;
    WriteLn
END PrintLine;

PROCEDURE Print(n : INTEGER);
VAR i : INTEGER;
BEGIN
    FOR i:=1 TO n DO
        PrintLine(i)
    END
END Print;

BEGIN
    Print(10);

    ReadChar
END Pascal.
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 1000 -- allow very large numbers
parse arg rows .
if rows = '' then rows = 11 -- default to 11 rows
printPascalTriangle(rows)
return

-- -----------------------------------------------------------------------------
method printPascalTriangle(rows = 11) public static
  lines = ''
  mx = (factorial(rows - 1) / factorial(rows % 2) / factorial(rows - 1 - rows % 2)).length() -- width of widest number

  loop row = 1 to rows
    n1 = 1.center(mx)
    line = n1
    loop col = 2 to row
      n2 = col - 1
      n1 = n1 * (row - n2) / n2
      line = line n1.center(mx)
      end col
    lines[row] = line.strip()
    end row

  -- display triangle
  ml = lines[rows].length() -- length of longest line
  loop row = 1 to rows
    say lines[row].centre(ml)
    end row

  return

-- -----------------------------------------------------------------------------
method factorial(n) public static
  fac = 1
  loop n_ = 2 to n
    fac = fac * n_
    end n_
  return fac /*calc. factorial*/

```

{{out}}

```txt

                    1
                  1   1
                1   2   1
              1   3   3   1
            1   4   6   4   1
          1   5  10  10   5   1
        1   6  15  20  15   6   1
      1   7  21  35  35  21   7   1
    1   8  28  56  70  56  28   8   1
  1   9  36  84  126 126 84  36   9   1
1  10  45  120 210 252 210 120 45  10   1

```



## Nial

Like J

(pascal.nial)

```nial
factorial is recur [ 0 =, 1 first, pass, product, -1 +]
combination is fork [ > [first, second], 0 first,
   / [factorial second, * [factorial - [second, first], factorial first] ]
]
pascal is transpose each combination cart [pass, pass] tell
```

Using it

```nial
|loaddefs 'pascal.nial'
|pascal 5
```



## Nim


```nim
import sequtils

proc pascal(n: int) =
  var row = @[1]
  for r in 1..n:
    echo row
    row = zip(row & @[0], @[0] & row).mapIt(int, it[0] + it[1])

pascal(10)
```



## OCaml



```ocaml
(* generate next row from current row *)
let next_row row =
  List.map2 (+) ([0] @ row) (row @ [0])

(* returns the first n rows *)
let pascal n =
  let rec loop i row =
    if i = n then []
    else row :: loop (i+1) (next_row row)
  in loop 0 [1]
```



## Octave


```octave
function pascaltriangle(h)
  for i = 0:h-1
    for k = 0:h-i
      printf("  ");
    endfor
    for j = 0:i
      printf("%3d ", bincoeff(i, j));
    endfor
    printf("\n");
  endfor
endfunction

pascaltriangle(4);
```



## Oforth


No result if n <= 0


```Oforth
: pascal(n)  [ 1 ] #[ dup println dup 0 + 0 rot + zipWith(#+) ] times(n) drop ;
```


{{out}}

```txt

10 pascal
[1]
[1, 1]
[1, 2, 1]
[1, 3, 3, 1]
[1, 4, 6, 4, 1]
[1, 5, 10, 10, 5, 1]
[1, 6, 15, 20, 15, 6, 1]
[1, 7, 21, 35, 35, 21, 7, 1]
[1, 8, 28, 56, 70, 56, 28, 8, 1]
[1, 9, 36, 84, 126, 126, 84, 36, 9, 1]

```



## Oz


```oz
declare
  fun {NextLine Xs}
     {List.zip 0|Xs {Append Xs [0]}
      fun {$ Left Right}
         Left + Right
      end}
  end

  fun {Triangle N}
     {List.take {Iterate [1] NextLine} N}
  end

  fun lazy {Iterate I F}
     I|{Iterate {F I} F}
  end

  %% Only works nicely for N =< 5.
  proc {PrintTriangle T}
     N = {Length T}
  in
     for
        Line in T
        Indent in N-1..0;~1
     do
        for _ in 1..Indent do {System.printInfo " "} end
        for L in Line do {System.printInfo L#" "} end
        {System.printInfo "\n"}
     end
  end
in
  {PrintTriangle {Triangle 5}}
```


For n = 0, prints nothing. For negative n, throws an exception.

## PARI/GP


```parigp
pascals_triangle(N)= {
my(row=[],prevrow=[]);
for(x=1,N,
    if(x>5,break(1));
         row=eval(Vec(Str(11^(x-1))));
         print(row));
prevrow=row;
for(y=6,N,
   for(p=2,#prevrow,
         row[p]=prevrow[p-1]+prevrow[p]);
         row=concat(row,1);
         prevrow=row;
         print(row);
     );
}
```



## Pascal


```pascal
Program PascalsTriangle(output);

procedure Pascal(r : Integer);
  var
    i, c, k : Integer;
  begin
    for i := 0 to r-1 do
    begin
      c := 1;
      for k := 0 to i do
      begin
        write(c:3);
        c := (c * (i-k)) div (k+1);
      end;
      writeln;
   end;
end;

begin
  Pascal(9)
end.
```

Output:

```txt
% ./PascalsTriangle
  1
  1  1
  1  2  1
  1  3  3  1
  1  4  6  4  1
  1  5 10 10  5  1
  1  6 15 20 15  6  1
  1  7 21 35 35 21  7  1
  1  8 28 56 70 56 28  8  1

```



## Perl

These functions perform as requested in the task: they print out the first ''n'' lines.  If ''n'' <= 0, they print nothing.  The output is simple (no fancy formatting).

```perl
sub pascal {
  my $rows = shift;
  my @next = (1);
  for my $n (1 .. $rows) {
    print "@next\n";
    @next = (1, (map $next[$_]+$next[$_+1], 0 .. $n-2), 1);
  }
}
```


If you want more than 68 rows, then use either "use bigint" or "use Math::GMP qw/:constant/" inside the function to enable bigints.  We can also use a binomial function which will expand to bigints if many rows are requested:
{{libheader|ntheory}}

```perl
use ntheory qw/binomial/;
sub pascal {
  my $rows = shift;
  for my $n (0 .. $rows-1) {
    print join(" ", map { binomial($n,$_) } 0 .. $n), "\n";
  }
}
```


Here is a non-obvious version using bignum, which is limited to the first 23 rows because of the algorithm used:

```perl
use bignum;
sub pascal_line { $_[0] ? unpack "A(A6)*", 1000001**$_[0] : 1 }
sub pascal { print "@{[map -+-$_, pascal_line $_]}\n" for 0..$_[0]-1 }
```


This triangle is build using the 'sock' or 'hockey stick' pattern property. Here I use the word tartaglia and not pascal because in my country it's called after the  Niccolò Fontana, known also as Tartaglia. A full graphical implementation of 16 properties that can be found in the triangle can be found at mine [https://github.com/LorenzoTa/Tartaglia-s-triangle Tartaglia's triangle]


```perl

#!/usr/bin/perl
use strict;
use warnings;

{
  my @tartaglia ;
  sub tartaglia {
      my ($x,$y) = @_;
      if ($x == 0 or $y == 0)  { $tartaglia[$x][$y]=1 ; return 1};
      my $ret ;
      foreach my $yps (0..$y){
        $ret += ( $tartaglia[$x-1][$yps] || tartaglia($x-1,$yps) );
      }
      $tartaglia[$x][$y] = $ret;
      return $ret;
  }
}
sub tartaglia_row {
    my $y = shift;
    my $x = 0;
    my @row;
    $row[0] = &tartaglia($x,$y+1);
    foreach my $pos (0..$y-1) {push @row, tartaglia(++$x,--$y)}
    return @row;
}


for (0..5) {print join ' ', tartaglia_row($_),"\n"}
print "\n\n";


print tartaglia(3,3),"\n";
my @third = tartaglia_row(5);
print "@third\n";

```


which output

```txt

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1


20
1 5 10 10 5 1

```



## Perl 6

{{works with|rakudo|2015-10-03}}

###  using a lazy sequence generator


The following routine returns a lazy list of lines using the sequence operator (<tt>...</tt>).  With a lazy result you need not tell the routine how many you want; you can just use a slice subscript to get the first N lines:

```perl6
sub pascal {
    [1], { [0, |$_ Z+ |$_, 0] } ... *
}

.say for pascal[^10];
```


One problem with the routine above is that it might recalculate the sequence each time you call it.  Slightly more idiomatic would be to define the sequence as a lazy constant.  Here we use the <tt>@</tt> sigil to indicate that the sequence should cache its values for reuse, and use an explicit parameter <tt>$prev</tt> for variety:


```perl6
constant @pascal = [1], -> $prev { [0, |$prev Z+ |$prev, 0] } ... *;

.say for @pascal[^10];
```


Since we use ordinary subscripting, non-positive inputs throw an index-out-of-bounds error.


###  recursive


{{trans|Haskell}}


```perl6
multi sub pascal (1) { $[1] }
multi sub pascal (Int $n where 2..*) {
    my @rows = pascal $n - 1;
    |@rows, [0, |@rows[*-1] Z+ |@rows[*-1], 0 ];
}

.say for pascal 10;
```


Non-positive inputs throw a multiple-dispatch error.


###  iterative


{{trans|Perl}}

```perl6
sub pascal ($n where $n >= 1) {
   say my @last = 1;
   for 1 .. $n - 1 -> $row {
       @last = 1, |map({ @last[$_] + @last[$_ + 1] }, 0 .. $row - 2), 1;
       say @last;
   }
}

pascal 10;
```


Non-positive inputs throw a type check error.

{{Output}}

```txt
[1]
[1 1]
[1 2 1]
[1 3 3 1]
[1 4 6 4 1]
[1 5 10 10 5 1]
[1 6 15 20 15 6 1]
[1 7 21 35 35 21 7 1]
[1 8 28 56 70 56 28 8 1]
[1 9 36 84 126 126 84 36 9 1]
```



## Phix


```Phix
sequence row = {}
for m = 1 to 13 do
    row = row & 1
    for n=length(row)-1 to 2 by -1 do
        row[n] += row[n-1]
    end for
    printf(1,repeat(' ',(13-m)*2))
    for i=1 to length(row) do
        printf(1," %3d",row[i])
    end for
    puts(1,'\n')
end for
```

{{out}}
<pre style="font-size: 8px">
                           1
                         1   1
                       1   2   1
                     1   3   3   1
                   1   4   6   4   1
                 1   5  10  10   5   1
               1   6  15  20  15   6   1
             1   7  21  35  35  21   7   1
           1   8  28  56  70  56  28   8   1
         1   9  36  84 126 126  84  36   9   1
       1  10  45 120 210 252 210 120  45  10   1
     1  11  55 165 330 462 462 330 165  55  11   1
   1  12  66 220 495 792 924 792 495 220  66  12   1

```


"Reflected" Pascal's triangle, it uses symmetry property to "mirror" second part. It determines  even and odd strings. automatically.

## PHP


```php

<?php
 //Author Ivan Gavryshin @dcc0
function tre($n) {
  $ck=1;
  $kn=$n+1;

 if($kn%2==0) {
 $kn=$kn/2;
 $i=0;
  }
 else
  {

  $kn+=1;
  $kn=$kn/2;
  $i= 1;
}

 for ($k = 1; $k <= $kn-1; $k++) {
   $ck = $ck/$k*($n-$k+1);
   $arr[] = $ck;
   echo  "+" . $ck ;

  }


if ($kn>1) {
  echo $arr[i];
  $arr=array_reverse($arr);
 for ($i; $i<= $kn-1; $i++) {
 echo  "+" . $arr[$i]  ;
     }

   }

 }
 //set amount of strings here
 while ($n<=20) {
 ++$n;
 echo tre($n);
 echo "<br/>";
}


?>

```



## PHP


```php
function pascalsTriangle($num){
	$c = 1;
	$triangle = Array();
	for($i=0;$i<=$num;$i++){
		$triangle[$i] = Array();
		if(!isset($triangle[$i-1])){
			$triangle[$i][] = $c;
		}else{
			for($j=0;$j<count($triangle[$i-1])+1;$j++){
				$triangle[$i][] = (isset($triangle[$i-1][$j-1]) && isset($triangle[$i-1][$j])) ? $triangle[$i-1][$j-1] + $triangle[$i-1][$j] : $c;
			}
		}
	}
	return $triangle;
}

$tria = pascalsTriangle(8);
foreach($tria as $val){
	foreach($val as $value){
		echo $value . ' ';
	}
	echo '
';
}
```

                                        1
                                      1   1
                                    1   2   1
                                  1   3   3   1
                                1   4   6   4   1
                              1   5  10  10   5   1
                            1   6  15  20  15   6   1
                          1   7  21  35  35  21   7   1
                        1   8  28  56  70  56  28   8   1


## PL/I


```PL/I

declare (t, u)(40) fixed binary;
declare (i, n) fixed binary;

t,u = 0;
get (n);
if n <= 0 then return;

do n = 1 to n;
   u(1) = 1;
   do i = 1 to n;
      u(i+1) = t(i) + t(i+1);
   end;
   put skip edit ((u(i) do i = 1 to n)) (col(40-2*n), (n+1) f(4));
   t = u;
end;

```


<lang>
                                        1
                                      1   1
                                    1   2   1
                                  1   3   3   1
                                1   4   6   4   1
                              1   5  10  10   5   1
                            1   6  15  20  15   6   1
                          1   7  21  35  35  21   7   1
                        1   8  28  56  70  56  28   8   1
                      1   9  36  84 126 126  84  36   9   1
                    1  10  45 120 210 252 210 120  45  10   1

```



## PicoLisp

{{trans|C}}

```PicoLisp
(de pascalTriangle (N)
   (for I N
      (space (* 2 (- N I)))
      (let C 1
         (for K I
            (prin (align 3 C) " ")
            (setq C (*/ C (- I K) K)) ) )
      (prinl) ) )
```



## Potion


```potion
printpascal = (n) :
   if (n < 1) :
      1 print
      (1)
   . else :
      prev = printpascal(n - 1)
      prev append(0)
      curr = (1)
      n times (i):
         curr append(prev(i) + prev(i + 1))
      .
      "\n" print
      curr join(", ") print
      curr
   .
.

printpascal(read number integer)
```



## PowerShell


```powershell

$Infinity = 1
$NewNumbers = $null
$Numbers = $null
$Result = $null
$Number = $null
$Power = $args[0]

Write-Host $Power

For(
   $i=0;
   $i -lt $Infinity;
   $i++
   )
   {
    $Numbers = New-Object Object[] 1
    $Numbers[0] = $Power
   For(
      $k=0;
      $k -lt $NewNumbers.Length;
      $k++
      )
      {
       $Numbers = $Numbers + $NewNumbers[$k]
      }
   If(
     $i -eq 0
     )
     {
      $Numbers = $Numbers + $Power
     }
    $NewNumbers = New-Object Object[] 0
   Try
   {
   For(
      $j=0;
      $j -lt $Numbers.Length;
      $j++
      )
      {
       $Result = $Numbers[$j] + $Numbers[$j+1]
       $NewNumbers = $NewNumbers + $Result
      }
   }
   Catch [System.Management.Automation.RuntimeException]
   {
    Write-Warning "Value was too large for a Decimal. Script aborted."
    Break;
   }
   Foreach(
          $Number in $Numbers
          )
          {
          If(
            $Number.ToString() -eq "+unendlich"
            )
            {
             Write-Warning "Value was too large for a Decimal. Script aborted."
             Exit
            }
          }
    Write-Host $Numbers
    $Infinity++
   }

```


Save the above code to a .ps1 script file and start it by calling its name and providing N.


```txt

PS C:\> & '.\Pascals Triangle.ps1' 1

----

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1
1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1
1 9 36 84 126 126 84 36 9 1
1 10 45 120 210 252 210 120 45 10 1
1 11 55 165 330 462 462 330 165 55 11 1
1 12 66 220 495 792 924 792 495 220 66 12 1
1 13 78 286 715 1287 1716 1716 1287 715 286 78 13 1
1 14 91 364 1001 2002 3003 3432 3003 2002 1001 364 91 14 1
1 15 105 455 1365 3003 5005 6435 6435 5005 3003 1365 455 105 15 1

```



## Prolog

Difference-lists are used to make quick append.

```Prolog
pascal(N) :-
	pascal(1, N, [1], [[1]|X]-X, L),
	maplist(my_format, L).

pascal(Max, Max, L, LC, LF) :-
	!,
	make_new_line(L, NL),
	append_dl(LC, [NL|X]-X, LF-[]).

pascal(N, Max, L, NC, LF) :-
	build_new_line(L, NL),
	append_dl(NC, [NL|X]-X, NC1),
	N1 is N+1,
	pascal(N1, Max, NL, NC1, LF).

build_new_line(L, R) :-
	build(L, 0, X-X, R).

build([], V, RC, RF) :-
	append_dl(RC, [V|Y]-Y, RF-[]).

build([H|T], V, RC, R) :-
	V1 is V+H,
	append_dl(RC, [V1|Y]-Y, RC1),
	build(T, H, RC1, R).

append_dl(X1-X2, X2-X3, X1-X3).

% to have a correct output !
my_format([H|T]) :-
	write(H),
	maplist(my_writef, T),
	nl.

my_writef(X) :-
	writef(' %5r', [X]).

```


Output :

```Prolog
 ?- pascal(15).
1
1     1
1     2     1
1     3     3     1
1     4     6     4     1
1     5    10    10     5     1
1     6    15    20    15     6     1
1     7    21    35    35    21     7     1
1     8    28    56    70    56    28     8     1
1     9    36    84   126   126    84    36     9     1
1    10    45   120   210   252   210   120    45    10     1
1    11    55   165   330   462   462   330   165    55    11     1
1    12    66   220   495   792   924   792   495   220    66    12     1
1    13    78   286   715  1287  1716  1716  1287   715   286    78    13     1
1    14    91   364  1001  2002  3003  3432  3003  2002  1001   364    91    14     1
1    15   105   455  1365  3003  5005  6435  6435  5005  3003  1365   455   105    15     1
true.

```


### An alternative

The above use of difference lists is a really innovative example of late binding.  Here's an alternative source which, while possibly not as efficient (or as short) as the previous example, may be a little easier to read and understand.

```prolog
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Produce a pascal's triangle of depth N
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%  Prolog is declarative.  The predicate pascal/3 below says that to produce
%  a row of depth N, we can do so by first producing the row at depth(N-1),
%  and then adding the paired values in that row.  The triangle is produced
%  by prepending the row at N-1 to the preceding rows as recursion unwinds.
%  The triangle produced by pascal/3 is upside down and lacks the last row,
%  so pascal/2 prepends the last row to the triangle and reverses it.
%  Finally, pascal/1 produces the triangle, iterates each row and prints it.
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pascal_row([V], [V]).                           % No more value pairs to add
pascal_row([V0, V1|T], [V|Rest]) :-             % Add values from preceding row
    V is V0 + V1, !, pascal_row([V1|T], Rest).  % Drops initial value (1).

pascal(1, [1], []).    % at depth 1, this row is [1] and no preceding rows.
pascal(N, [1|ThisRow], [Last|Preceding]) :- % Produce a row of depth N
    succ(N0, N),                            % N is the successor to N0
    pascal(N0, Last, Preceding),            % Get the previous row
    !, pascal_row(Last, ThisRow).           % Calculate this row from the previous

pascal(N, Triangle) :-
   pascal(N, Last, Rows),             % Retrieve row at depth N and preceding rows
   !, reverse([Last|Rows], Triangle). % Add last row to triangle and reverse order

pascal(N) :-
  pascal(N, Triangle), member(Row, Triangle), % Iterate and write each row
  write(Row), nl, fail.
pascal(_).
```

*Output*:

```prolog
?- pascal(5).
[1]
[1,1]
[1,2,1]
[1,3,3,1]
[1,4,6,4,1]
```



## PureBasic



```PureBasic
Procedure pascaltriangle( n.i)

  For i=  0 To  n
       c = 1
       For k=0 To i
             Print(Str( c)+" ")
         c = c * (i-k)/(k+1);
        Next ;k
    PrintN(" "); nächste zeile
  Next ;i

EndProcedure

OpenConsole()
Parameter.i = Val(ProgramParameter(0))
pascaltriangle(Parameter);
Input()
```



## Python


### Procedural


```python
def pascal(n):
   """Prints out n rows of Pascal's triangle.
   It returns False for failure and True for success."""
   row = [1]
   k = [0]
   for x in range(max(n,0)):
      print row
      row=[l+r for l,r in zip(row+k,k+row)]
   return n>=1
```


or by creating a scan function:

```Python
def scan(op, seq, it):
  a = []
  result = it
  a.append(it)
  for x in seq:
    result = op(result, x)
    a.append(result)
  return a

def pascal(n):
    def nextrow(row, x):
        return [l+r for l,r in zip(row+[0,],[0,]+row)]

    return scan(nextrow, range(n-1), [1,])

for row in pascal(4):
    print(row)
```



### Functional


The itertools module yields a simple functional definition of '''scanl''' in terms of '''accumulate''', and '''zipWith''' can be defined in terms either of '''itertools.starmap''', or the base '''map'''.

With a scanl and a zipWith to hand, we can derive both finite and non-finite lists of pascal rows from a simple '''nextPascal''' step function:

{{Works with|Python|3.7}}

```python
'''Pascal's triangle'''

from itertools import (accumulate, chain, islice)
from operator import (add)


# nextPascal :: [Int] -> [Int]
def nextPascal(xs):
    '''A row of Pascal's triangle
       derived from a preceding row.'''
    return zipWith(add)([0] + xs)(xs + [0])


# pascalTriangle :: Generator [[Int]]
def pascalTriangle():
    '''A non-finite stream of
       Pascal's triangle rows.'''
    return iterate(nextPascal)([1])


# finitePascalRows :: Int -> [[Int]]
def finitePascalRows(n):
    '''The first n rows of Pascal's triangle.'''
    def go(a, _):
        return nextPascal(a)
    return scanl(go)([1])(
        range(1, n)
    )


# TESTS ---------------------------------------------------
# main :: IO ()
def main():
    '''Test of two different approaches:
        - taking from a non-finite stream of rows,
        - or constructing a finite list of rows.'''
    print(unlines(map(
        showPascal,
        [
            take(7)(
                pascalTriangle()        # Non finite,
            ),
            finitePascalRows(7)         # finite.
        ]
    )))


# showPascal :: [[Int]] -> String
def showPascal(xs):
    '''Stringification of a list of
       Pascal triangle rows.'''
    ys = list(xs)

    def align(w):
        return lambda ns: center(w)(
            ' '
        )('   '.join(map(str, ns)))
    w = len('   '.join((map(str, ys[-1]))))
    return '\n'.join(map(align(w), ys))


# GENERIC -------------------------------------------------


# center :: Int -> Char -> String -> String
def center(n):
    '''String s padded with c to approximate centre,
       fitting in but not truncated to width n.'''
    def go(c, s):
        qr = divmod(n - len(s), 2)
        q = qr[0]
        return (q * c) + s + ((q + qr[1]) * c)
    return lambda c: lambda s: go(c, s)


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# scanl :: (b -> a -> b) -> b -> [a] -> [b]
def scanl(f):
    '''scanl is like reduce, but returns a succession of
       intermediate values, building from the left.'''
    return lambda a: lambda xs: (
        accumulate(chain([a], xs), f)
    )


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
def zipWith(f):
    '''A list constructed by zipping with a
       custom function, rather than with the
       default tuple constructor.'''
    return lambda xs: lambda ys: (
        list(map(f, xs, ys))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
             1
           1   1
         1   2   1
       1   3   3   1
     1   4   6   4   1
  1   5   10   10   5   1
1   6   15   20   15   6   1
             1
           1   1
         1   2   1
       1   3   3   1
     1   4   6   4   1
  1   5   10   10   5   1
1   6   15   20   15   6   1
```



## q


```q

pascal:{(x-1){0+':x,0}\1}
pascal 5
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1

```



## Qi

{{trans|Haskell}}

```Qi

(define iterate
  _ _ 0 -> []
  F V N -> [V|(iterate F (F V) (1- N))])

(define next-row
  R -> (MAPCAR + [0|R] (append R [0])))

(define pascal
  N -> (iterate next-row [1] N))

```



## R

{{trans|Octave}}

```R
pascalTriangle <- function(h) {
  for(i in 0:(h-1)) {
    s <- ""
    for(k in 0:(h-i)) s <- paste(s, "  ", sep="")
    for(j in 0:i) {
      s <- paste(s, sprintf("%3d ", choose(i, j)), sep="")
    }
    print(s)
  }
}
```


Here's an R version:


```R
pascalTriangle <- function(h) {
  lapply(0:h, function(i) choose(i, 0:i))
}
```



## Racket


Iterative version by summing rows up to <math>n</math>.


```Racket
#lang racket

(define (pascal n)
  (define (next-row current-row)
    (map + (cons 0 current-row)
           (append current-row '(0))))
  (reverse
   (for/fold ([triangle '((1))])
             ([row (in-range 1 n)])
     (cons (next-row (first triangle)) triangle))))



```



## RapidQ


### Summing from Previous Rows

{{trans|BASIC}}

The main difference to BASIC implementation is the output formatting.
RapidQ does not support PRINT USING. Instead, function FORMAT$() is used.
TAB() is not supported, so SPACE$() was used instead.

Another difference is that in RapidQ, DIM does not clear array values to zero.
But if dimensioning is done with DEF..., you can give the initial values in curly braces.
If less values are given than there are elements in the array, the remaining positions are initialized to zero.
RapidQ does not require simple variables to be declared before use.


```rapidq
DEFINT values(100) = {0,1}

INPUT "Number of rows: "; nrows
PRINT SPACE$((nrows)*3);"  1"
FOR row = 2 TO nrows
    PRINT SPACE$((nrows-row)*3+1);
    FOR i = row TO 1 STEP -1
        values(i) = values(i) + values(i-1)
        PRINT FORMAT$("%5d ", values(i));
    NEXT i
    PRINT
NEXT row
```



### Using binary coefficients

{{trans|BASIC}}

```rapidq
INPUT "Number of rows: "; nrows
FOR row = 0 TO nrows-1
    c = 1
    PRINT SPACE$((nrows-row)*3);
    FOR i = 0 TO row
        PRINT FORMAT$("%5d ", c);
        c = c * (row - i) / (i+1)
    NEXT i
    PRINT
NEXT row
```



## Red


```Red
Red[]
pascal-triangle: function [
    n [ integer! ] "number of rows"
 ][
    row: make vector! [ 1 ]
    loop n [
        print row
        left: copy row
        right: copy row
        insert left 0
        append right 0
        row: left + right
    ]
]
```

Output:

```txt

pascal-triangle 7
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1

```



## Retro


```Retro
2 elements i j
: pascalTriangle
  cr dup
  [ dup !j 1 swap 1+ [ !i dup putn space @j @i - * @i 1+ / ] iter cr drop ] iter drop
;
13 pascalTriangle
```




## REXX

There is no practical limit for this REXX version, triangles up to 46 rows have been
generated (without wrapping) in a screen window with a width of 620 characters.

If the number (of rows) specified is negative, the output is written to a (disk) file
instead.   Triangles with over a 1,000 rows have easily been created.
The output file created (that is written to disk) is named     '''PASCALS.n'''
    where   '''n'''   is the absolute value of the number entered.


Note:    Pascal's triangle is also known as:
:::*   Khayyam's triangle
:::*   Khayyam─Pascal's triangle
:::*   Tartaglia's triangle
:::*   Yang Hui's triangle

```rexx
/*REXX program displays (or writes to a file)   Pascal's triangle  (centered/formatted).*/
numeric digits 3000                              /*be able to handle gihugeic triangles.*/
parse arg nn .                                   /*obtain the optional argument from CL.*/
if nn=='' | nn==","  then nn=10                  /*Not specified?  Then use the default.*/
N=abs(nn)                                        /*N  is the number of rows in triangle.*/
w=length( !(N-1) / !(N%2) / !(N-1-N%2) )         /*W:  the width of the biggest integer.*/
@.=1;    $.=@.;   unity=right(1, w)              /*defaults rows & lines; aligned unity.*/
                                                 /* [↓]  build rows of Pascals' triangle*/
  do   r=1  for N;           rm=r-1              /*Note:  the first column is always  1.*/
    do c=2  to rm;           cm=c-1              /*build the rest of the columns in row.*/
    @.r.c= @.rm.cm + @.rm.c                      /*assign value to a specific row & col.*/
    $.r  = $.r     right(@.r.c, w)               /*and construct a line for output (row)*/
    end   /*c*/                                  /* [↑]    C  is the column being built.*/
  if r\==1  then $.r=$.r  unity                  /*for  rows≥2,  append a trailing  "1".*/
  end     /*r*/                                  /* [↑]    R  is the  row   being built.*/
                                                 /* [↑]  WIDTH: for nicely looking line.*/
width=length($.N)                                /*width of the last (output) line (row)*/
                                                 /*if NN<0, output is written to a file.*/
      do r=1  for N;     $$=center($.r, width)   /*center this particular Pascals' row. */
      if nn>0  then say                       $$ /*SAY    if   NN    is positive,  else */
               else call lineout 'PASCALS.'n, $$ /*write this Pascal's row ───►  a file.*/
      end   /*r*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!: procedure; !=1;  do j=2  to arg(1); !=!*j; end /*j*/;  return !   /*compute factorial*/
```

'''output'''   when using the input of:   <tt> 11 </tt>

```txt

                    1
                  1   1
                1   2   1
              1   3   3   1
            1   4   6   4   1
          1   5  10  10   5   1
        1   6  15  20  15   6   1
      1   7  21  35  35  21   7   1
    1   8  28  56  70  56  28   8   1
  1   9  36  84 126 126  84  36   9   1
1  10  45 120 210 252 210 120  45  10   1

```

'''output'''   when using the input of:   <tt> 22 </tt>

(Output shown at   <big>'''<sup>4</sup>/<sub>5</sub>'''</big>   size.)
<pre style="font-size:80%">
                                                                         1
                                                                      1      1
                                                                  1      2      1
                                                               1      3      3      1
                                                           1      4      6      4      1
                                                        1      5     10     10      5      1
                                                    1      6     15     20     15      6      1
                                                 1      7     21     35     35     21      7      1
                                             1      8     28     56     70     56     28      8      1
                                          1      9     36     84    126    126     84     36      9      1
                                      1     10     45    120    210    252    210    120     45     10      1
                                   1     11     55    165    330    462    462    330    165     55     11      1
                               1     12     66    220    495    792    924    792    495    220     66     12      1
                            1     13     78    286    715   1287   1716   1716   1287    715    286     78     13      1
                        1     14     91    364   1001   2002   3003   3432   3003   2002   1001    364     91     14      1
                     1     15    105    455   1365   3003   5005   6435   6435   5005   3003   1365    455    105     15      1
                 1     16    120    560   1820   4368   8008  11440  12870  11440   8008   4368   1820    560    120     16      1
              1     17    136    680   2380   6188  12376  19448  24310  24310  19448  12376   6188   2380    680    136     17      1
          1     18    153    816   3060   8568  18564  31824  43758  48620  43758  31824  18564   8568   3060    816    153     18      1
       1     19    171    969   3876  11628  27132  50388  75582  92378  92378  75582  50388  27132  11628   3876    969    171     19      1
   1     20    190   1140   4845  15504  38760  77520 125970 167960 184756 167960 125970  77520  38760  15504   4845   1140    190     20      1
1     21    210   1330   5985  20349  54264 116280 203490 293930 352716 352716 293930 203490 116280  54264  20349   5985   1330    210     21      1

```



## Ring


```ring

row = 5
for i = 0 to row - 1
    col = 1
    see left("     ",row-i)
    for k = 0 to i
        see "" + col + " "
        col = col*(i-k)/(k+1)
    next
    see nl
next

```

Output:

```txt

     1
    1 1
   1 2 1
  1 3 3 1
 1 4 6 4 1

```



## Ruby


```ruby
def pascal(n)
  raise ArgumentError, "must be positive." if n < 1
  yield ar = [1]
  (n-1).times do
    ar.unshift(0).push(0) # tack a zero on both ends
    yield ar = ar.each_cons(2).map(&:sum)
  end
end

pascal(8){|row| puts row.join(" ").center(20)}
```

{{out}}

```txt

         1
        1 1
       1 2 1
      1 3 3 1
     1 4 6 4 1
   1 5 10 10 5 1
  1 6 15 20 15 6 1
1 7 21 35 35 21 7 1

```


Or for more or less a translation of the two line Haskell version (with inject being abused a bit I know):


```ruby
def next_row(row) ([0] + row).zip(row + [0]).collect {|l,r| l + r } end

def pascal(n) n.times.inject([1]) {|x,_| next_row x } end

8.times{|i| p pascal(i)}
```

{{out}}

```txt

[1]
[1, 1]
[1, 2, 1]
[1, 3, 3, 1]
[1, 4, 6, 4, 1]
[1, 5, 10, 10, 5, 1]
[1, 6, 15, 20, 15, 6, 1]
[1, 7, 21, 35, 35, 21, 7, 1]

```



## Run BASIC


```runbasic
input "number of rows? ";r
for i = 0 to r - 1
  c = 1
  print left$("                          ",(r*2)-(i*2));
  for k = 0 to i
    print using("####",c);
    c = c*(i-k)/(k+1)
  next
  print
next
```
Output:

```txt
Number of rows? ?5
             1
           1   1
         1   2   1
       1   3   3   1
     1   4   6   4   1
```



## Rust

{{trans|C}}

```rust

fn pascal_triangle(n: u64)
{

  for i in 0..n {
    let mut c = 1;
    for _j in 1..2*(n-1-i)+1 {
      print!(" ");
    }
    for k in 0..i+1 {
      print!("{:2} ", c);
      c = c * (i-k)/(k+1);
    }
    println!();
  }
}

```



## Scala


### Functional solutions


### =Summing: Recursive row definition=


```scala

  def tri(row: Int): List[Int] =
    row match {
      case 1 => List(1)
      case n: Int => 1 +: ((tri(n - 1) zip tri(n - 1).tail) map { case (a, b) => a + b }) :+ 1
    }
```

Function to pretty print n rows:

```scala
def prettyTri(n:Int) = (1 to n) foreach {i => print(" "*(n-i)); tri(i) map (c => print(c + " ")); println}

prettyTri(5)
```

{{Out}}

```txt
    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1
```

====Summing: Scala Stream (Recursive & Memoization)====

```Scala
object Blaise extends App {
  def pascalTriangle(): Stream[Vector[Int]] =
    Vector(1) #:: Stream.iterate(Vector(1, 1))(1 +: _.sliding(2).map(_.sum).toVector :+ 1)

  val output = pascalTriangle().take(15).map(_.mkString(" "))
  val longest = output.last.length

  println("Pascal's Triangle")
  output.foreach(line => println(s"${" " * ((longest - line.length) / 2)}$line"))
}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/8VqiX0P/1 ScalaFiddle (JavaScript)] or by [https://scastie.scala-lang.org/c3dDWMCcT3eoydy6QJcWCw Scastie (JVM)].


## Scheme

{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
(define (next-row row)
  (map + (cons 0 row) (append row '(0))))

(define (triangle row rows)
  (if (= rows 0)
      '()
      (cons row (triangle (next-row row) (- rows 1)))))

(triangle (list 1) 5)

```

Output:
<lang>((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: numRows is 0;
    var array integer: values is [] (0, 1);
    var integer: row is 0;
    var integer: index is 0;
  begin
    write("Number of rows: ");
    readln(numRows);
    writeln("1" lpad succ(numRows) * 3);
    for row range 2 to numRows do
      write("" lpad (numRows - row) * 3);
      values &:= [] 0;
      for index range succ(row) downto 2 do
        values[index] +:= values[pred(index)];
        write(" " <& values[index] lpad 5);
      end for;
      writeln;
    end for;
  end func;
```



## Sidef


```ruby
func pascal(rows) {
    var row = [1]
    { | n|
        say row.join(' ')
        row = [1, {|i| row[i] + row[i+1] }.map(0 .. n-2)..., 1]
    } << 1..rows
}
 
pascal(10)
```



## Stata

First, a few ways to compute a "Pascal matrix". With the first, the upper triangle is made of missing values (zeros with the other two).


```stata
function pascal1(n) {
	return(comb(J(1,n,0::n-1),J(n,1,0..n-1)))
}

function pascal2(n) {
	a = I(n)
	a[.,1] = J(n,1,1)
	for (i=3; i<=n; i++) {
		a[i,2..i-1] = a[i-1,2..i-1]+a[i-1,1..i-2]
	}
	return(a)
}

function pascal3(n) {
	a = J(n,n,0)
	for (i=1; i<n; i++) {
		a[i+1,i] = i
	}
	s = p = I(n)
	k = 1
	for (i=0; i<n; i++) {
		p = p*a/k++
		s = s+p
	}
	return(s)
}
```


Now print the Pascal triangle.


```stata
function print_pascal_triangle(n) {
	a = pascal1(n)
	for (i=1; i<=n; i++) {
		for (j=1; j<=i; j++) {
			printf("%10.0f",a[i,j])
		}
		printf("\n")
	}
}

print_pascal_triangle(5)
         1
         1         1
         1         2         1
         1         3         3         1
         1         4         6         4         1
```



## Swift


```swift
func pascal(n:Int)->[Int]{
    if n==1{
        let a=[1]
        print(a)
        return a
    }
    else{
        var a=pascal(n:n-1)
        var temp=a
        for i in 0..<a.count{
            if i+1==a.count{
                temp.append(1)
                break
            }
            temp[i+1] = a[i]+a[i+1]
        }
        a=temp
        print(a)
        return a
    }
}
let waste = pascal(n:10)

```



## Tcl


### Summing from Previous Rows


```tcl
proc pascal_iterative n {
    if {$n < 1} {error "undefined behaviour for n < 1"}
    set row [list 1]
    lappend rows $row
    set i 1
    while {[incr i] <= $n} {
        set prev $row
        set row [list 1]
        for {set j 1} {$j < [llength $prev]} {incr j} {
            lappend row [expr {[lindex $prev [expr {$j - 1}]] + [lindex $prev $j]}]
        }
        lappend row 1
        lappend rows $row
    }
    return $rows
}

puts [join [pascal_iterative 6] \n]
```


```txt
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
```


### Using binary coefficients

{{trans|BASIC}}

```tcl
proc pascal_coefficients n {
    if {$n < 1} {error "undefined behaviour for n < 1"}
    for {set i 0} {$i < $n} {incr i} {
        set c 1
        set row [list $c]
        for {set j 0} {$j < $i} {incr j} {
            set c [expr {$c * ($i - $j) / ($j + 1)}]
            lappend row $c
        }
        lappend rows $row
    }
    return $rows
}

puts [join [pascal_coefficients 6] \n]
```


### Combinations

{{trans|Java}}
Thanks to Tcl 8.5's arbitrary precision integer arithmetic, this solution is not limited to a couple of dozen rows.  Uses a caching factorial calculator to improve performance.

```tcl
package require Tcl 8.5

proc pascal_combinations n {
    if {$n < 1} {error "undefined behaviour for n < 1"}
    for {set i 0} {$i < $n} {incr i} {
        set row [list]
        for {set j 0} {$j <= $i} {incr j} {
            lappend row [C $i $j]
        }
        lappend rows $row
    }
    return $rows
}

proc C {n k} {
    expr {[ifact $n] / ([ifact $k] * [ifact [expr {$n - $k}]])}
}

set fact_cache {1 1}
proc ifact n {
    global fact_cache
    if {$n < [llength $fact_cache]} {
        return [lindex $fact_cache $n]
    }
    set i [expr {[llength $fact_cache] - 1}]
    set sum [lindex $fact_cache $i]
    while {$i < $n} {
        incr i
        set sum [expr {$sum * $i}]
        lappend fact_cache $sum
    }
    return $sum
}

puts [join [pascal_combinations 6] \n]
```



### Comparing Performance


```tcl
set n 100
puts "calculate $n rows:"
foreach proc {pascal_iterative pascal_coefficients pascal_combinations} {
    puts "$proc: [time [list $proc $n] 100]"
}
```

{{Out}}

```txt
calculate 100 rows:
pascal_iterative: 2800.14 microseconds per iteration
pascal_coefficients: 8760.98 microseconds per iteration
pascal_combinations: 38176.66 microseconds per iteration
```


=={{header|TI-83 BASIC}}==

### Using Addition of Previous Rows


```ti83b
PROGRAM:PASCALTR
:Lbl IN
:ClrHome
:Disp "NUMBER OF ROWS"
:Input N
:If N < 1:Goto IN
:{N,N}→dim([A])
:"CHEATING TO MAKE IT FASTER"
:For(I,1,N)
:1→[A](1,1)
:End
:For(I,2,N)
:For(J,2,I)
:[A](I-1,J-1)+[A](I-1,J)→[A](I,J)
:End
:End
:[A]
```


### Using nCr Function


```ti83b
PROGRAM:PASCALTR
:Lbl IN
:ClrHome
:Disp "NUMBER OF ROWS"
:Input N
:If N < 1:Goto IN
:{N,N}→dim([A])
:For(I,2,N)
:For(J,2,I)
:(I-1) nCr (J-1)→[A](I,J)
:End
:End
:[A]
```


## Turing



```turing

procedure pascal (n : int)
    for i : 0 .. n
        var c : int
        c := 1
        for k : 0 .. i
            put intstr(c) + " " ..
            c := c * (i - k) div (k + 1)
        end for
        put ""
    end for
end pascal

pascal(5)
```



## uBasic/4tH

<lang>Input "Number Of Rows: "; N
@(1) = 1
Print Tab((N+1)*3);"1"

For R = 2 To N
    Print Tab((N-R)*3+1);
    For I = R To 1 Step -1
        @(I) = @(I) + @(I-1)
        Print Using "______";@(i);
    Next
Next

Print
End
```

Output:

```txt
Number Of Rows: 10
                                 1
                              1     1
                           1     2     1
                        1     3     3     1
                     1     4     6     4     1
                  1     5    10    10     5     1
               1     6    15    20    15     6     1
            1     7    21    35    35    21     7     1
         1     8    28    56    70    56    28     8     1
      1     9    36    84   126   126    84    36     9     1

0 OK, 0:380

```


## UNIX Shell

{{works with|Bourne Again SHell}}
Any n <= 1 will print the "1" row.

```bash
#! /bin/bash
pascal() {
    local -i n=${1:-1}
    if (( n <= 1 )); then
        echo 1
    else
        local output=$( $FUNCNAME $((n - 1)) )
        set -- $( tail -n 1 <<<"$output" )   # previous row
        echo "$output"
        printf "1 "
        while [[ -n $1 ]]; do
            printf "%d " $(( $1 + ${2:-0} ))
            shift
        done
        echo
    fi
}
pascal "$1"
```



## Ursala

Zero maps to the empty list. Negatives are inexpressible.
This solution uses a library function for binomial coefficients.

```Ursala
#import std
#import nat

pascal = choose**ziDS+ iota*t+ iota+ successor
```

This solution uses direct summation. The algorithm is to
insert zero at the head of a list (initially the unit list <1>), zip it with its reversal,
map the sum over the list of pairs, iterate n times, and return the trace.

```Ursala
#import std
#import nat

pascal "n" = (next"n" sum*NiCixp) <1>
```

test program:

```Ursala
#cast %nLL

example = pascal 10
```

{{Out}}

```txt
<
   <1>,
   <1,1>,
   <1,2,1>,
   <1,3,3,1>,
   <1,4,6,4,1>,
   <1,5,10,10,5,1>,
   <1,6,15,20,15,6,1>,
   <1,7,21,35,35,21,7,1>,
   <1,8,28,56,70,56,28,8,1>,
   <1,9,36,84,126,126,84,36,9,1>>
```



## VBA


```vb
Option Base 1
Private Sub pascal_triangle(n As Integer)
    Dim odd() As String
    Dim eve() As String
    ReDim odd(1)
    ReDim eve(2)
    odd(1) = "  1"
    For i = 1 To n
        If i Mod 2 = 1 Then
            Debug.Print String$(2 * n - 2 * i, " ") & Join(odd, " ")
            eve(1) = "  1"
            ReDim Preserve eve(i + 1)
            For j = 2 To i
                eve(j) = Format(CStr(Val(odd(j - 1)) + Val(odd(j))), "@@@")
            Next j
            eve(i + 1) = "  1"
        Else
            Debug.Print String$(2 * n - 2 * i, " ") & Join(eve, " ")
            odd(1) = "  1"
            ReDim Preserve odd(i + 1)
            For j = 2 To i
                odd(j) = Format(CStr(Val(eve(j - 1)) + Val(eve(j))), "@@@")
            Next j
            odd(i + 1) = "  1"
        End If
    Next i
End Sub
Public Sub main()
    pascal_triangle 13
End Sub
```
{{out}}

```txt
                          1
                        1   1
                      1   2   1
                    1   3   3   1
                  1   4   6   4   1
                1   5  10  10   5   1
              1   6  15  20  15   6   1
            1   7  21  35  35  21   7   1
          1   8  28  56  70  56  28   8   1
        1   9  36  84 126 126  84  36   9   1
      1  10  45 120 210 252 210 120  45  10   1
    1  11  55 165 330 462 462 330 165  55  11   1
  1  12  66 220 495 792 924 792 495 220  66  12   1
```


## VBScript

Derived from the BASIC version.

```vb
Pascal_Triangle(WScript.Arguments(0))
Function Pascal_Triangle(n)
	Dim values(100)
	values(1) = 1
	WScript.StdOut.Write values(1)
	WScript.StdOut.WriteLine
	For row = 2 To n
		For i = row To 1 Step -1
			values(i) = values(i) + values(i-1)
			WScript.StdOut.Write values(i) & " "
		Next
		WScript.StdOut.WriteLine
	Next
End Function
```

{{out}}
Invoke from a command line.

```txt

F:\VBScript>cscript /nologo rosettacode-pascals_triangle.vbs 6
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1

```



## Vedit macro language


### Summing from Previous Rows

{{trans|BASIC}}

Vedit macro language does not have actual arrays (edit buffers are normally used for storing larger amounts of data).
However, a numeric register can be used as index to access another numeric register.
For example, if #99 contains value 2, then #@99 accesses contents of numeric register #2.


```vedit
#100 = Get_Num("Number of rows: ", STATLINE)
#0=0; #1=1
Ins_Char(' ', COUNT, #100*3-2) Num_Ins(1)
for (#99 = 2; #99 <= #100; #99++) {
    Ins_Char(' ', COUNT, (#100-#99)*3)
    #@99 = 0
    for (#98 = #99; #98 > 0; #98--) {
	#97 = #98-1
	#@98 += #@97
	Num_Ins(#@98, COUNT, 6)
    }
    Ins_Newline
}
```



### Using binary coefficients

{{trans|BASIC}}

```vedit
#1 = Get_Num("Number of rows: ", STATLINE)
for (#2 = 0; #2 < #1; #2++) {
    #3 = 1
    Ins_Char(' ', COUNT, (#1-#2-1)*3)
    for (#4 = 0; #4 <= #2; #4++) {
	Num_Ins(#3, COUNT, 6)
	#3 = #3 * (#2-#4) / (#4+1)
    }
    Ins_Newline
}
```




## Visual Basic

{{works with|Visual Basic|VB6 Standard}}

```vb
Sub pascaltriangle()
    'Pascal's triangle
    Const m = 11
    Dim t(40) As Integer, u(40) As Integer
    Dim i As Integer, n As Integer, s As String, ss As String
    ss = ""
    For n = 1 To m
        u(1) = 1
        s = ""
        For i = 1 To n
            u(i + 1) = t(i) + t(i + 1)
            s = s & u(i) & " "
            t(i) = u(i)
        Next i
        ss = ss & s & vbCrLf
    Next n
    MsgBox ss, , "Pascal's triangle"
End Sub 'pascaltriangle
```

{{out}}

```txt

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1
1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1
1 9 36 84 126 126 84 36 9 1
1 10 45 120 210 252 210 120 45 10 1

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Numerics

Module Module1
    Iterator Function GetRow(rowNumber As BigInteger) As IEnumerable(Of BigInteger)
        Dim denominator As BigInteger = 1
        Dim numerator = rowNumber

        Dim currentValue As BigInteger = 1
        For counter = 0 To rowNumber
            Yield currentValue
            currentValue = currentValue * numerator
            numerator = numerator - 1
            currentValue = currentValue / denominator
            denominator = denominator + 1
        Next
    End Function

    Function GetTriangle(quantityOfRows As Integer) As IEnumerable(Of BigInteger())
        Dim range = Enumerable.Range(0, quantityOfRows).Select(Function(num) New BigInteger(num))
        Return range.Select(Function(num) GetRow(num).ToArray())
    End Function

    Function CenterString(text As String, width As Integer)
        Dim spaces = width - text.Length
        Dim padLeft = (spaces / 2) + text.Length
        Return text.PadLeft(padLeft).PadRight(width)
    End Function

    Function FormatTriangleString(triangle As IEnumerable(Of BigInteger())) As String
        Dim maxDigitWidth = triangle.Last().Max().ToString().Length
        Dim rows = triangle.Select(Function(arr) String.Join(" ", arr.Select(Function(array) CenterString(array.ToString(), maxDigitWidth))))
        Dim maxRowWidth = rows.Last().Length
        Return String.Join(Environment.NewLine, rows.Select(Function(row) CenterString(row, maxRowWidth)))
    End Function

    Sub Main()
        Dim triangle = GetTriangle(20)
        Dim output = FormatTriangleString(triangle)
        Console.WriteLine(output)
    End Sub

End Module
```

{{out}}

```txt
                                                           1
                                                        1     1
                                                     1     2     1
                                                  1     3     3     1
                                               1     4     6     4     1
                                            1     5     10    10    5     1
                                         1     6     15    20    15    6     1
                                      1     7     21    35    35    21    7     1
                                   1     8     28    56    70    56    28    8     1
                                1     9     36    84   126   126    84    36    9     1
                             1     10    45   120   210   252   210   120    45    10    1
                          1     11    55   165   330   462   462   330   165    55    11    1
                       1     12    66   220   495   792   924   792   495   220    66    12    1
                    1     13    78   286   715  1287  1716  1716  1287   715   286    78    13    1
                 1     14    91   364  1001  2002  3003  3432  3003  2002  1001   364    91    14    1
              1     15   105   455  1365  3003  5005  6435  6435  5005  3003  1365   455   105    15    1
           1     16   120   560  1820  4368  8008  11440 12870 11440 8008  4368  1820   560   120    16    1
        1     17   136   680  2380  6188  12376 19448 24310 24310 19448 12376 6188  2380   680   136    17    1
     1     18   153   816  3060  8568  18564 31824 43758 48620 43758 31824 18564 8568  3060   816   153    18    1
  1     19   171   969  3876  11628 27132 50388 75582 92378 92378 75582 50388 27132 11628 3876   969   171    19    1
```



## XBasic

{{trans|GW-BASIC}}
{{works with|Windows XBasic}}

```xbasic

PROGRAM "pascal"
VERSION "0.0001"

DECLARE FUNCTION Entry()

FUNCTION Entry()
  r@@ = UBYTE(INLINE$("Number of rows? "))
  FOR i@@ = 0 TO r@@ - 1
    c%% = 1
    FOR k@@ = 0 TO i@@
      PRINT FORMAT$("####", c%%);
      c%% = c%% * (i@@ - k@@) / (k@@ + 1)
    NEXT k@@
    PRINT
  NEXT i@@
END FUNCTION
END PROGRAM

```

{{out}}

```txt

Number of rows? 7
   1
   1   1
   1   2   1
   1   3   3   1
   1   4   6   4   1
   1   5  10  10   5   1
   1   6  15  20  15   6   1

```



## XPL0


```XPL0
include c:\cxpl\codes;

proc Pascal(N);         \Display the first N rows of Pascal's triangle
int  N;                 \if N<=0 then nothing is displayed
int  Row, I, Old(40), New(40);
[for Row:= 0 to N-1 do
        [New(0):= 1;
        for I:= 1 to Row do New(I):= Old(I-1) + Old(I);
        for I:= 1 to (N-Row-1)*2 do ChOut(0, ^ );
        for I:= 0 to Row do
                [if New(I)<100 then ChOut(0, ^ );
                IntOut(0, New(I));
                if New(I)<10 then ChOut(0, ^ );
                ChOut(0, ^ );
                ];
        New(Row+1):= 0;
        I:= Old;  Old:= New;  New:= I;
        CrLf(0);
        ];
];

Pascal(13)
```


{{Out}}

```txt

                         1
                       1   1
                     1   2   1
                   1   3   3   1
                 1   4   6   4   1
               1   5   10  10  5   1
             1   6   15  20  15  6   1
           1   7   21  35  35  21  7   1
         1   8   28  56  70  56  28  8   1
       1   9   36  84 126 126  84  36  9   1
     1   10  45 120 210 252 210 120  45  10  1
   1   11  55 165 330 462 462 330 165  55  11  1
 1   12  66 220 495 792 924 792 495 220  66  12  1

```



## zkl

{{trans|C}}

```zkl
fcn pascalTriangle(n){ // n<=0-->""
   foreach i in (n){
      c := 1;
      print(" "*(2*(n-1-i)));
      foreach k in (i+1){
         print("%3d ".fmt(c));
         c = c * (i-k)/(k+1);
      }
      println();
   }
}

pascalTriangle(8);
```

{{out}}

```txt

                1
              1   1
            1   2   1
          1   3   3   1
        1   4   6   4   1
      1   5  10  10   5   1
    1   6  15  20  15   6   1
  1   7  21  35  35  21   7   1

```



## ZX Spectrum Basic


In edit mode insert:

```BASIC
 10 INPUT "How many rows? ";n
 15 IF n<1 THEN GO TO 210
 20 DIM c(n)
 25 DIM d(n)
 30 LET c(1)=1
 35 LET d(1)=1
 40 FOR r=1 TO n
 50 FOR i=1 TO (n-r)
 60 PRINT " ";
 70 NEXT i
 80 FOR i=1 TO r
 90 PRINT c(i);" ";
100 NEXT i
110 PRINT
120 IF r>=n THEN GO TO 140
130 LET d(r+1)=1
140 FOR i=2 TO r
150 LET d(i)=c(i-1)+c(i)
160 NEXT i
165 IF r>=n THEN GO TO 200
170 FOR i=1 TO r+1
180 LET c(i)=d(i)
190 NEXT i
200 NEXT r
```


Then in command mode (basically don't put a number in front):

```BASIC>RUN</lang

{{out}}

```txt

        1
       1 1
      1 2 1
     1 3 3 1
    1 4 6 4 1
   1 5 10 10 5 1
  1 6 15 20 15 6 1
 1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1

```


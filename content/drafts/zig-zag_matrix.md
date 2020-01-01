+++
title = "Zig-zag matrix"
description = ""
date = 2019-07-21T10:31:53Z
aliases = []
[extra]
id = 2961
[taxonomies]
categories = []
tags = []
+++

{{task|Matrices}}

;Task:
Produce a zig-zag array.


A   ''zig-zag''   array is a square arrangement of the first   <big>N<sup>2</sup></big>   integers,   where the

numbers increase sequentially as you zig-zag along the array's   [https://en.wiktionary.org/wiki/antidiagonal anti-diagonals].

For a graphical representation, see   [[wp:Image:JPEG_ZigZag.svg|JPG zigzag]]   (JPG uses such arrays to encode images).


For example, given   '''5''',   produce this array:

```txt

 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

```



;Related tasks:
*   [[Spiral matrix]]
*   [[Identity_matrix]]
*   [[Ulam_spiral_(for_primes)]]


;See also:
*   Wiktionary entry:   [https://en.wiktionary.org/wiki/antidiagonal anti-diagonals]





## 360 Assembly


```360asm
*        Zig-zag matrix            15/08/2015
ZIGZAGMA CSECT
         USING  ZIGZAGMA,R12       set base register
         LR     R12,R15            establish addressability
         LA     R9,N               n : matrix size
         LA     R6,1               i=1
         LA     R7,1               j=1
         LR     R11,R9             n
         MR     R10,R9             *n
         BCTR   R11,0              R11=n**2-1
         SR     R8,R8              k=0
LOOPK    CR     R8,R11             do k=0 to n**2-1
         BH     ELOOPK             k>limit
         LR     R1,R6              i
         BCTR   R1,0               -1
         MR     R0,R9              *n
         LR     R2,R7              j
         BCTR   R2,0               -1
         AR     R1,R2              (i-1)*n+(j-1)
         SLA    R1,1               index=((i-1)*n+j-1)*2
         STH    R8,T(R1)           t(i,j)=k
         LR     R2,R6              i
         AR     R2,R7              i+j
         LA     R1,2               2
         SRDA   R2,32              shift right r1 to r2
         DR     R2,R1              (i+j)/2
         LTR    R2,R2              if mod(i+j,2)=0
         BNZ    ELSEMOD
         CR     R7,R9              if j<n
         BNL    ELSE1
         LA     R7,1(R7)           j=j+1
         B      EIF1
ELSE1    LA     R6,2(R6)           i=i+2
EIF1     CH     R6,=H'1'           if i>1
         BNH    NOT1
         BCTR   R6,0               i=i-1
NOT1     B      NOT2
ELSEMOD  CR     R6,R9              if i<n
         BNL    ELSE2
         LA     R6,1(R6)           i=i+1
         B      EIF2
ELSE2    LA     R7,2(R7)           j=j+2
EIF2     CH     R7,=H'1'           if j>1
         BNH    NOT2
         BCTR   R7,0               j=j-1
NOT2     LA     R8,1(R8)           k=k+1
         B      LOOPK
ELOOPK   LA     R6,1               end k; i=1
LOOPI    CR     R6,R9              do i=1 to n
         BH     ELOOPI             i>n
         LA     R10,0              ibuf=0  buffer index
         MVC    BUFFER,=CL80' '
         LA     R7,1               j=1
LOOPJ    CR     R7,R9              do j=1 to n
         BH     ELOOPJ             j>n
         LR     R1,R6              i
         BCTR   R1,0               -1
         MR     R0,R9              *n
         LR     R2,R7              j
         BCTR   R2,0               -1
         AR     R1,R2              (i-1)*n+(j-1)
         SLA    R1,1               index=((i-1)*n+j-1)*2
         LH     R2,T(R1)           t(i,j)
         LA     R3,BUFFER
         AR     R3,R10
         XDECO  R2,XDEC            edit t(i,j) length=12
         MVC    0(4,R3),XDEC+8     move in buffer length=4
         LA     R10,4(R10)         ibuf=ibuf+1
         LA     R7,1(R7)           j=j+1
         B      LOOPJ
ELOOPJ   XPRNT  BUFFER,80          end j
         LA     R6,1(R6)           i=i+1
         B      LOOPI
ELOOPI   XR     R15,R15            end i; return_code=0
         BR     R14                return to caller
N        EQU    5                  matrix size
BUFFER   DS     CL80
XDEC     DS     CL12
T        DS     (N*N)H             t(n,n) matrix
         YREGS
         END    ZIGZAGMA
```

{{out}}

```txt

   0   1   5   6  14
   2   4   7  13  15
   3   8  12  16  21
   9  11  17  20  22
  10  18  19  23  24

```



## ActionScript


```as

package
{
   public class ZigZagMatrix extends Array
   {

      private var height:uint;
      private var width:uint;
      public var mtx:Array = [];

      public function ZigZagMatrix(size:uint)
      {
         this.height = size;
         this.width = size;

         this.mtx = [];
         for (var i:uint = 0; i < size; i++) {
            this.mtx[i] = [];
         }
         i = 1;
         var j:uint = 1;
         for (var e:uint = 0; e < size*size; e++) {
            this.mtx[i-1][j-1] = e;
            if ((i + j) % 2 == 0) {
               // Even stripes
               if (j < size) j ++;
               else       i += 2;
               if (i > 1) i --;
            } else {
               // Odd stripes
               if (i < size) i ++;
               else       j += 2;
               if (j > 1) j --;
            }
         }
      }
   }
}

```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Zig_Zag is

   type Matrix is array (Positive range <>, Positive range <>) of Natural;
   function Zig_Zag (Size : Positive) return Matrix is
      Data : Matrix (1..Size, 1..Size);
      I, J : Integer := 1;
   begin
      Data (1, 1) := 0;
      for Element in 1..Size**2 - 1 loop
         if (I + J) mod 2 = 0 then
            -- Even stripes
            if J < Size then
               J := J + 1;
            else
               I := I + 2;
            end if;
            if I > 1 then
               I := I - 1;
            end if;
         else
            -- Odd stripes
            if I < Size then
               I := I + 1;
            else
               J := J + 2;
            end if;
            if J > 1 then
               J := J - 1;
            end if;
         end if;
         Data (I, J) := Element;
      end loop;
      return Data;
   end Zig_Zag;

   procedure Put (Data : Matrix) is
   begin
      for I in Data'Range (1) loop
         for J in Data'Range (2) loop
            Put (Integer'Image (Data (I, J)));
         end loop;
         New_Line;
      end loop;
   end Put;

begin
   Put (Zig_Zag (5));
end Test_Zig_Zag;
```

The function Zig_Zag generates a square matrix filled as requested by the task.

{{out}}

```txt

 0 1 5 6 14
 2 4 7 13 15
 3 8 12 16 21
 9 11 17 20 22
 10 18 19 23 24

```



## Agena

Tested with Agena 2.9.5 Win32

```agena
# zig-zag matrix

makeZigZag := proc( n :: number ) :: table is

    local move := proc( x :: number, y :: number, upRight :: boolean ) is
        if   y = n then
            upRight := not upRight;
            x := x + 1
        elif x = 1 then
            upRight := not upRight;
            y := y + 1
        else
            x := x - 1;
            y := y + 1
        fi;
        return x, y, upRight
    end ;

    # create empty table
    local result := [];
    for i to n do
        result[ i ] := [];
        for j to n do result[ i, j ] := 0 od
    od;

    # fill the table
    local x, y, upRight := 1, 1, true;
    for i to n * n do
        result[ x, y ] := i - 1;
        if upRight then
            x, y, upRight := move( x, y, upRight )
        else
            y, x, upRight := move( y, x, upRight )
        fi
    od;

    return result
end;

scope
    local m := makeZigZag( 5 );
    for i to size m do
        for j to size m do
            printf( " %3d", m[ i, j ] )
        od;
        print()
    od
epocs
```

{{out}}

```txt

   0   1   5   6  14
   2   4   7  13  15
   3   8  12  16  21
   9  11  17  20  22
  10  18  19  23  24

```



## ALGOL 68

{{trans|D}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
PROC zig zag = (INT n)[,]INT: (
    PROC move = (REF INT i, j)VOID: (
        IF j < n THEN
            i := ( i <= 1 | 1 | i-1 );
            j +:= 1
        ELSE
            i +:= 1
        FI
    );

    [n, n]INT a;
    INT x:=LWB a, y:=LWB a;

    FOR v FROM 0 TO n**2-1 DO
        a[y, x] := v;
        IF ODD (x + y) THEN
            move(x, y)
        ELSE
            move(y, x)
        FI
    OD;
    a
);

INT dim = 5;
#IF formatted transput possible THEN
  FORMAT d = $z-d$;
  FORMAT row = $"("n(dim-1)(f(d)",")f(d)")"$;
  FORMAT block = $"("n(dim-1)(f(row)","lx)f(row)")"l$;

  printf((block, zig zag(dim)))
ELSE#
  [,]INT result = zig zag(dim);
  FOR i TO dim DO
    print((result[i,], new line))
  OD
#FI#
```

{{out}}
{|border="1" style="border-collapse: collapse; border: 5px double grey;"
|align=center width=50%| With formatted transput possible, e.g. [[ALGOL 68G]]
|align=center| '''not''' formatted transput possible, e.g. [[ELLA ALGOL 68]]
|-
|align=center|
```txt

((  0,  1,  5,  6, 14),
 (  2,  4,  7, 13, 15),
 (  3,  8, 12, 16, 21),
 (  9, 11, 17, 20, 22),
 ( 10, 18, 19, 23, 24))

```

||
```txt

         +0          +1          +5          +6         +14
         +2          +4          +7         +13         +15
         +3          +8         +12         +16         +21
         +9         +11         +17         +20         +22
        +10         +18         +19         +23         +24

```

|}


## ALGOL W

Based on the Agena sample.

```algolw
begin % zig-zag matrix %
    % z is returned holding a zig-zag matrix of order n, z must be at least n x n %
    procedure makeZigZag ( integer value n
                         ; integer array z( *, * )
                         ) ;
    begin
        procedure move ;
        begin
            if   y = n then begin
                upRight := not upRight;
                x := x + 1
                end
            else if x = 1 then begin
                upRight := not upRight;
                y := y + 1
                end
            else begin
                x := x - 1;
                y := y + 1
            end
        end move ;
        procedure swapXY ;
        begin
            integer swap;
            swap := x;
            x    := y;
            y    := swap;
        end swapXY ;
        integer x, y;
        logical upRight;
        % initialise the n x n matrix in z %
        for i := 1 until n do for j := 1 until n do z( i, j ) := 0;
        % fill in the zig-zag matrix %
        x := y := 1;
        upRight := true;
        for i := 1 until n * n do begin
            z( x, y ) := i - 1;
            if upRight then move
            else begin
                swapXY;
                move;
                swapXY
            end;
        end;
    end makeZigZap ;

    begin
        integer array zigZag( 1 :: 10, 1 :: 10 );
        for n := 5 do begin
            makeZigZag( n, zigZag );
            for i := 1 until n do begin
                 write( i_w := 4, s_w := 1, zigZag( i, 1 ) );
                for j := 2 until n do writeon( i_w := 4, s_w := 1, zigZag( i, j ) );
            end
        end
    end

end.
```

{{out}}

```txt

   0    1    5    6   14
   2    4    7   13   15
   3    8   12   16   21
   9   11   17   20   22
  10   18   19   23   24

```



## APL

{{works with|Dyalog APL}}

{{trans|J}}

```apl
      zz   ←  {⍵⍴⎕IO-⍨⍋⊃,/{(2|⍴⍵):⌽⍵⋄⍵}¨(⊂w)/¨⍨w{↓⍵∘.=⍨∪⍵}+/[1]⍵⊤w←⎕IO-⍨⍳×/⍵}   ⍝  General zigzag (any rectangle)
      zzSq ←  {zz,⍨⍵}                                                           ⍝  Square zigzag
      zzSq 5
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
```



## AppleScript



### Iterative


Here's a vector & matrix boundary detection approach to the Zig-zap matrix:

```AppleScript
set n to 5 -- Size of zig-zag matrix (n^2 cells).

-- Create an empty matrix.
set m to {}
repeat with i from 1 to n
	set R to {}
	repeat with j from 1 to n
		set end of R to 0
	end repeat
	set end of m to R
end repeat

-- Populate the matrix in a zig-zag manner.
set {x, y, v, d} to {1, 1, 0, 1}
repeat while v < (n ^ 2)
	if 1 ≤ x and x ≤ n and 1 ≤ y and y ≤ n then
		set {m's item y's item x, x, y, v} to {v, x + d, y - d, v + 1}
	else if x > n then
		set {x, y, d} to {n, y + 2, -d}
	else if y > n then
		set {x, y, d} to {x + 2, n, -d}
	else if x < 1 then
		set {x, y, d} to {1, y, -d}
	else if y < 1 then
		set {x, y, d} to {x, 1, -d}
	end if
end repeat
--> R = {{0, 1, 5, 6, 14}, {2, 4, 7, 13, 15}, {3, 8, 12, 16, 21}, {9, 11, 17, 20, 22}, {10, 18, 19, 23, 24}}

-- Reformat the matrix into a table for viewing.
repeat with i in m
	repeat with j in i
		set j's contents to  (characters -(length of (n ^ 2 as string)) thru -1 of ("          " & j)) as string
	end repeat
	set end of i to return
end repeat
return return & m as string
```

But this can be improved upon by building the matrix by populating empty AppleScript lists (it's about 50% faster when n=50):
```AppleScript
set n to 5

set m to {}
repeat with i from 1 to n
	set end of m to {} -- Built a foundation for the matrix out of n empty lists.
end repeat

set {v, d, i} to {0, -1, 1}
repeat while v < n ^ 2
	if length of m's item i < n then
		set {end of m's item i, i, v} to {f(v, n), i + d, v + 1}
		if i < 1 then
			set {i, d} to {1, -d}
		else if i > n then
			set {i, d} to {n, -d}
		else if i > 1 and (count of m's item (i - 1)) = 1 then
			set d to -d
		end if
	else
		set {i, d} to {i + 1, 1}
	end if
end repeat

-- Handler/function to format the cells on the fly.
on f(v, n)
	return (characters -(length of (n ^ 2 as string)) thru -1 of ("          " & v)) as string
end f

-- Reformat the matrix into a table for viewing.
set text item delimiters to ""
repeat with i in m
	set i's contents to (i as string) & return
end repeat
return return & m as string

```

{{out}} for both scripts is:
```txt
"
   0   1   5   6  14
   2   4   7  13  15
   3   8  12  16  21
   9  11  17  20  22
  10  18  19  23  24
"
```




### Recursive

By functional composition:

```AppleScript
-- zigzagMatrix
on zigzagMatrix(n)

    -- diagonals :: n -> [[n]]
    script diagonals
        on |λ|(n)
            script mf
                on diags(xs, iCol, iRow)
                    if (iCol < length of xs) then
                        if iRow < n then
                            set iNext to iCol + 1
                        else
                            set iNext to iCol - 1
                        end if

                        set {headList, tail} to splitAt(iCol, xs)
                        {headList} & diags(tail, iNext, iRow + 1)
                    else
                        {xs}
                    end if
                end diags
            end script

            diags(enumFromTo(0, n * n - 1), 1, 1) of mf
        end |λ|
    end script

    -- oddReversed :: [a] -> Int -> [a]
    script oddReversed
        on |λ|(lst, i)
            if i mod 2 = 0 then
                lst
            else
                reverse of lst
            end if
        end |λ|
    end script

    rowsFromDiagonals(n, map(oddReversed, |λ|(n) of diagonals))

end zigzagMatrix

-- Rows of given length from list of diagonals
-- rowsFromDiagonals :: Int -> [[a]] -> [[a]]
on rowsFromDiagonals(n, lst)
    if length of lst > 0 then

        -- lengthOverOne :: [a] -> Bool
        script lengthOverOne
            on |λ|(lst)
                length of lst > 1
            end |λ|
        end script

        set {edge, residue} to splitAt(n, lst)

        {map(my head, edge)} & ¬
            rowsFromDiagonals(n, ¬
                map(my tail, ¬
                    filter(lengthOverOne, edge)) & residue)
    else
        {}
    end if
end rowsFromDiagonals


-- TEST -----------------------------------------------------------------------
on run

    zigzagMatrix(5)

end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- head :: [a] -> a
on head(xs)
    if length of xs > 0 then
        item 1 of xs
    else
        missing value
    end if
end head

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
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- splitAt:: n -> list -> {n items from start of list, rest of list}
-- splitAt :: Int -> [a] -> ([a], [a])
on splitAt(n, xs)
    if n > 0 and n < length of xs then
        {items 1 thru n of xs, items (n + 1) thru -1 of xs}
    else
        if n < 1 then
            {{}, xs}
        else
            {xs, {}}
        end if
    end if
end splitAt

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail
```

{{Out}}

```txt
{{0, 1, 5, 6, 14},
{2, 4, 7, 13, 15},
{3, 8, 12, 16, 21},
{9, 11, 17, 20, 22},
{10, 18, 19, 23, 24}}
```



## Applesoft BASIC


```ApplesoftBasic
100 S = 5
110 S2 = S ^ 2 : REM SQUARED
120 H = S2 / 2 : REM HALFWAY
130 S2 = S2 - 1
140 DX = 1 : REM INITIAL
150 DY = 0 : REM DIRECTION
160 N = S - 1
170 DIM A%(N, N)

200 FOR I = 0 TO H
210     A%(X, Y) = I
220     A%(N - X, N - Y) = S2 - I
230     X = X + DX
240     Y = Y + DY
250     IF Y = 0 THEN DY = DY + 1 : IF DY THEN DX = -DX
260     IF X = 0 THEN DX = DX + 1 : IF DX THEN DY = -DY
270 NEXT I

300 FOR Y = 0 TO N
310     FOR X = 0 TO N
320         IF X THEN PRINT TAB(X * (LEN(STR$(S2)) + 1) + 1);
330         PRINT A%(X, Y);
340     NEXT X
350     PRINT
360 NEXT Y
```



## ATS

<lang>
(* ****** ****** *)
//
#include
"share/atspre_define.hats" // defines some names
#include
"share/atspre_staload.hats" // for targeting C
#include
"share/HATS/atspre_staload_libats_ML.hats" // for ...
//
(* ****** ****** *)
//
extern
fun
Zig_zag_matrix(n: int): void
//
(* ****** ****** *)

fun max(a: int, b: int): int =
  if a > b then a else b

fun movex(n: int, x: int, y: int): int =
  if y < n-1 then max(0, x-1) else x+1

fun movey(n: int, x: int, y: int): int =
  if y < n-1 then y+1 else y

fun zigzag(n: int, i: int, row: int, x: int, y: int): void =
  if i = n*n then ()
  else
    let
      val () = (if x = row then begin print i; print ','; end else ())
      //val () = (begin print x; print ' '; print y; print ' '; print i; print ' '; end)
      val nextX: int = if ((x+y) % 2) = 0 then movex(n, x, y) else movey(n, y, x)
      val nextY: int = if ((x+y) % 2) = 0 then movey(n, x, y) else movex(n, y, x)
    in
      zigzag(n, i+1, row, nextX, nextY)
    end

implement
Zig_zag_matrix(n) =
  let
    fun loop(row: int): void =
      if row = n then () else
        let
          val () = zigzag(n, 0, row, 0, 0)
          val () = println!(" ")
        in
          loop(row + 1)
        end
  in
    loop(0)
  end

(* ****** ****** *)

implement
main0() = () where
{
  val () = Zig_zag_matrix(5)
} (* end of [main0] *)

(* ****** ****** *)

```



## AutoHotkey

{{trans|lisp}}
contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276307.html#276307 forum].

```AutoHotkey
n = 5                           ; size
v := x := y := 1                ; initial values
Loop % n*n {                    ; for every array element
   a_%x%_%y% := v++             ; assign the next index
   If ((x+y)&1)                 ; odd diagonal
      If (x < n)                ; while inside the square
         y -= y<2 ? 0 : 1, x++  ; move right-up
      Else y++                  ; on the edge increment y, but not x: to even diagonal
   Else                         ; even diagonal
      If (y < n)                ; while inside the square
         x -= x<2 ? 0 : 1, y++  ; move left-down
      Else x++                  ; on the edge increment x, but not y: to odd diagonal
}

Loop %n% {                      ; generate printout
   x := A_Index                 ; for each row
   Loop %n%                     ; and for each column
      t .= a_%x%_%A_Index% "`t" ; attach stored index
   t .= "`n"                    ; row is complete
}
MsgBox %t%                      ; show output
```



## AutoIt


```autoit

#include <Array.au3>
$Array = ZigZag(5)
_ArrayDisplay($Array)

Func ZigZag($int)
	Local $av_array[$int][$int]
	Local $x = 1, $y = 1
	For $I = 0 To $int ^ 2 -1
		$av_array[$x-1][$y-1] = $I
		If Mod(($x + $y), 2) = 0 Then ;Even
			if ($y < $int) Then
				$y += 1
			Else
				$x += 2
			EndIf
			if ($x > 1) Then $x -= 1
		Else ; ODD
			if ($x < $int) Then
				$x += 1
			Else
				$y += 2
			EndIf
			If $y > 1 Then $y -= 1
		EndIf
	Next
	Return $av_array
EndFunc   ;==>ZigZag

```



## AWK


```AWK

# syntax: GAWK -f ZIG-ZAG_MATRIX.AWK [-v offset={0|1}] [size]
BEGIN {
# offset: "0" prints 0 to size^2-1 while "1" prints 1 to size^2
    offset = (offset == "") ? 0 : offset
    size = (ARGV[1] == "") ? 5 : ARGV[1]
    if (offset !~ /^[01]$/) { exit(1) }
    if (size !~ /^[0-9]+$/) { exit(1) }
    width = length(size ^ 2 - 1 + offset) + 1
    i = j = 1
    for (n=0; n<=size^2-1; n++) { # build array
      arr[i-1,j-1] = n + offset
      if ((i+j) % 2 == 0) {
        if (j < size) { j++ } else { i+=2 }
        if (i > 1) { i-- }
      }
      else {
        if (i < size) { i++ } else { j+=2 }
        if (j > 1) { j-- }
      }
    }
    for (row=0; row<size; row++) { # show array
      for (col=0; col<size; col++) {
        printf("%*d",width,arr[row,col])
      }
      printf("\n")
    }
    exit(0)
}

```

{{out}}

```txt

  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```



## BBC BASIC


```bbcbasic
      Size% = 5
      DIM array%(Size%-1,Size%-1)

      i% = 1
      j% = 1
      FOR e% = 0 TO Size%^2-1
        array%(i%-1,j%-1) = e%
        IF ((i% + j%) AND 1) = 0 THEN
          IF j% < Size% j% += 1 ELSE i% += 2
          IF i% > 1 i% -= 1
        ELSE
          IF i% < Size% i% += 1 ELSE j% += 2
          IF j% > 1 j% -= 1
        ENDIF
      NEXT

      @% = &904
      FOR row% = 0 TO Size%-1
        FOR col% = 0 TO Size%-1
          PRINT array%(row%,col%);
        NEXT
        PRINT
      NEXT row%
```

{{out}}

```txt

   0   1   5   6  14
   2   4   7  13  15
   3   8  12  16  21
   9  11  17  20  22
  10  18  19  23  24

```



## Befunge

The size, ''N'', is specified by the first value on the stack - 5 in the example below. The upper limit is constrained only by the range of the playfield cells used for variables, since we're using an algorithm that calculates the values on the fly rather than building them up in memory. On an 8 bit interpreter this means an upper limit of at least 127, but with an extended cell range the size of ''N'' can be almost unlimited.


```befunge>>> 5 >>00p0010p:1:>20p030pv
0g-:0`*:*-:00g:*1-55+/>\55+/:v  v:,*84<
v:++!\**2p01:+1g01:g02$$_>>#^4#00#+p#1:#+1#g0#0g#3<^/+ 55\_$:>55+/\|
>55+,20g!00g10g`>#^_$$$@^!`g03g00!g04++**2p03:+1g03!\*+1*2g01:g04.$<
```


{{out}}

```txt
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int main(int c, char **v)
{
	int i, j, m, n, *s;

	/* default size: 5 */
	if (c < 2 || ((m = atoi(v[1]))) <= 0) m = 5;

	/* alloc array*/
	s = malloc(sizeof(int) * m * m);

	for (i = n = 0; i < m * 2; i++)
		for (j = (i < m) ? 0 : i-m+1; j <= i && j < m; j++)
			s[(i&1)? j*(m-1)+i : (i-j)*m+j ] = n++;

	for (i = 0; i < m * m; putchar((++i % m) ? ' ':'\n'))
		printf("%3d", s[i]);

	/* free(s) */
	return 0;
}
```

{{out}}

```txt
% ./a.out 7
  0  1  5  6 14 15 27
  2  4  7 13 16 26 28
  3  8 12 17 25 29 38
  9 11 18 24 30 37 39
 10 19 23 31 36 40 45
 20 22 32 35 41 44 46
 21 33 34 42 43 47 48
```



## C++


```cpp
#include <vector>
#include <memory>	// for auto_ptr
#include <cmath>	// for the log10 and floor functions
#include <iostream>
#include <iomanip>	// for the setw function

using namespace std;

typedef vector< int > IntRow;
typedef vector< IntRow > IntTable;

auto_ptr< IntTable > getZigZagArray( int dimension )
{
	auto_ptr< IntTable > zigZagArrayPtr( new IntTable(
		dimension, IntRow( dimension ) ) );

	// fill along diagonal stripes (oriented as "/")
	int lastValue = dimension * dimension - 1;
	int currNum = 0;
	int currDiag = 0;
	int loopFrom;
	int loopTo;
	int i;
	int row;
	int col;
	do
	{
		if ( currDiag < dimension ) // if doing the upper-left triangular half
		{
			loopFrom = 0;
			loopTo = currDiag;
		}
		else // doing the bottom-right triangular half
		{
			loopFrom = currDiag - dimension + 1;
			loopTo = dimension - 1;
		}

		for ( i = loopFrom; i <= loopTo; i++ )
		{
			if ( currDiag % 2 == 0 ) // want to fill upwards
			{
				row = loopTo - i + loopFrom;
				col = i;
			}
			else // want to fill downwards
			{
				row = i;
				col = loopTo - i + loopFrom;
			}

			( *zigZagArrayPtr )[ row ][ col ] = currNum++;
		}

		currDiag++;
	}
	while ( currDiag <= lastValue );

	return zigZagArrayPtr;
}

void printZigZagArray( const auto_ptr< IntTable >& zigZagArrayPtr )
{
	size_t dimension = zigZagArrayPtr->size();

	int fieldWidth = static_cast< int >( floor( log10(
		static_cast< double >( dimension * dimension - 1 ) ) ) ) + 2;

	size_t col;
	for ( size_t row = 0; row < dimension; row++ )
	{
		for ( col = 0; col < dimension; col++ )
			cout << setw( fieldWidth ) << ( *zigZagArrayPtr )[ row ][ col ];
		cout << endl;
	}
}

int main()
{
	printZigZagArray( getZigZagArray( 5 ) );
}
```

{{out}}

```txt

  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```



## C sharp



```csharp
public static int[,] ZigZag(int n)
{
    int[,] result = new int[n, n];
    int i = 0, j = 0;
    int d = -1; // -1 for top-right move, +1 for bottom-left move
    int start = 0, end = n * n - 1;
    do
    {
        result[i, j] = start++;
        result[n - i - 1, n - j - 1] = end--;

        i += d; j -= d;
        if (i < 0)
        {
            i++; d = -d; // top reached, reverse
        }
        else if (j < 0)
        {
            j++; d = -d; // left reached, reverse
        }
    } while (start < end);
    if (start == end)
        result[i, j] = start;
    return result;
}
```



## Ceylon


```ceylon
class ZigZag(Integer size) {

	value data = Array {
		for (i in 0:size)
		Array.ofSize(size, 0)
	};

	variable value i = 1;
	variable value j = 1;

	for (element in 0 : size^2) {
		data[j - 1]?.set(i - 1, element);
		if ((i + j).even) {
			if (j < size) {
				j++;
			}
			else {
				i += 2;
			}
			if (i > 1) {
				i--;
			}
		}
		else {
			if (i < size) {
				i++;
			}
			else {
				j += 2;
			}
			if (j > 1) {
				j--;
			}
		}
	}

	shared void display() {
		for (row in data) {
			for (element in row) {
				process.write(element.string.pad(3));
			}
			print(""); //newline
		}
	}
}

shared void run() {
	value zz = ZigZag(5);
	zz.display();
}
```



## Clojure

Purely functional approach.

```Clojure
(defn partitions [sizes coll]
  (lazy-seq
   (when-let [n (first sizes)]
     (when-let [s (seq coll)]
       (cons (take n coll)
	     (partitions (next sizes) (drop n coll)))))))

(defn take-from [n colls]
  (lazy-seq
   (when-let [s (seq colls)]
     (let [[first-n rest-n] (split-at n s)]
       (cons (map first first-n)
	     (take-from n (concat (filter seq (map rest first-n)) rest-n)))))))

(defn zig-zag [n]
  (->> (partitions (concat (range 1 (inc n)) (range (dec n) 0 -1)) (range (* n n)))
       (map #(%1 %2) (cycle [reverse identity]) ,)
       (take-from n ,)))

user> (zig-zag 5)
(( 0  1  5  6 14)
 ( 2  4  7 13 15)
 ( 3  8 12 16 21)
 ( 9 11 17 20 22)
 (10 18 19 23 24))

user> (zig-zag 6)
(( 0  1  5  6 14 15)
 ( 2  4  7 13 16 25)
 ( 3  8 12 17 24 26)
 ( 9 11 18 23 27 32)
 (10 19 22 28 31 33)
 (20 21 29 30 34 35))
```



## CoffeeScript


```coffeescript

# Calculate a zig-zag pattern of numbers like so:
#   0 1 5
#   2 4 6
#   3 7 8
#
# There are many interesting ways to solve this; we
# try for an algebraic approach, calculating triangle
# areas, so that me minimize space requirements.

zig_zag_value = (x, y, n) ->

  upper_triangle_zig_zag = (x, y) ->
    # calculate the area of the triangle from the prior
    # diagonals
    diag = x + y
    triangle_area = diag * (diag+1) / 2
    # then add the offset along the diagonal
    if diag % 2 == 0
      triangle_area + y
    else
      triangle_area + x

  if x + y < n
    upper_triangle_zig_zag x, y
  else
    # For the bottom right part of the matrix, we essentially
    # use reflection to count backward.
    bottom_right_cell = n * n - 1
    n -= 1
    v = upper_triangle_zig_zag(n-x, n-y)
    bottom_right_cell - v

zig_zag_matrix = (n) ->
  row = (i) -> (zig_zag_value i, j, n for j in [0...n])
  (row i for i in [0...n])

do ->
  for n in [4..6]
    console.log "---- n=#{n}"
    console.log zig_zag_matrix(n)
    console.log "\n"

```


{{out}}

```txt

> coffee zigzag.coffee
---- n=4
[ [ 0, 1, 5, 6 ],
  [ 2, 4, 7, 12 ],
  [ 3, 8, 11, 13 ],
  [ 9, 10, 14, 15 ] ]


---- n=5
[ [ 0, 1, 5, 6, 14 ],
  [ 2, 4, 7, 13, 15 ],
  [ 3, 8, 12, 16, 21 ],
  [ 9, 11, 17, 20, 22 ],
  [ 10, 18, 19, 23, 24 ] ]


---- n=6
[ [ 0, 1, 5, 6, 14, 15 ],
  [ 2, 4, 7, 13, 16, 25 ],
  [ 3, 8, 12, 17, 24, 26 ],
  [ 9, 11, 18, 23, 27, 32 ],
  [ 10, 19, 22, 28, 31, 33 ],
  [ 20, 21, 29, 30, 34, 35 ] ]

```




## Common Lisp

==={{trans|Java}} (but with zero-based indexes and combining the even and odd cases)===

```lisp
(defun zigzag (n)
  (flet ((move (i j)
           (if (< j (1- n))
               (values (max 0 (1- i)) (1+ j))
               (values (1+ i) j))))
    (loop with a = (make-array (list n n) :element-type 'integer)
          with x = 0
          with y = 0
          for v from 0 below (* n n)
          do (setf (aref a x y) v)
             (if (evenp (+ x y))
                 (setf (values x y) (move x y))
                 (setf (values y x) (move y x)))
          finally (return a))))
```



### An alternative approach


```lisp

; ZigZag
;
; Nigel Galloway.
; June 4th., 2012
;
(defun ZigZag (COLS)
  (let ((cs 2) (st '(1 2)) (dx '(-1 1)))
    (defun new_cx (i)
      (setq st (append st (list (setq cs (+ cs (* 2 i))) (setq cs (+ 1 cs))))
            dx (append dx '(-1 1))))
    (do ((i 2 (+ 2 i))) ((>= i COLS)) (new_cx i))
    (do ((i (- COLS 1 (mod COLS 2)) (+ -2 i))) ((<= i 0)) (new_cx i))
    (do ((i 0 (+ 1 i))) ((>= i COLS))
      (format t "~%")
      (do ((j i (+ 1 j))) ((>= j (+ COLS i)))
        (format t "~3d" (nth j st))
        (setf (nth j st) (+ (nth j st) (nth j dx)))))))

```

(ZigZag 5) Produces:

```txt

  1  2  6  7 15
  3  5  8 14 16
  4  9 13 17 22
 10 12 18 21 23
 11 19 20 24 25

```

(ZigZag 8) Produces:

```txt

  1  2  6  7 15 16 28 29
  3  5  8 14 17 27 30 43
  4  9 13 18 26 31 42 44
 10 12 19 25 32 41 45 54
 11 20 24 33 40 46 53 55
 21 23 34 39 47 52 56 61
 22 35 38 48 51 57 60 62
 36 37 49 50 58 59 63 64

```

(ZigZag 9) Produces:

```txt

  1  2  6  7 15 16 28 29 45
  3  5  8 14 17 27 30 44 46
  4  9 13 18 26 31 43 47 60
 10 12 19 25 32 42 48 59 61
 11 20 24 33 41 49 58 62 71
 21 23 34 40 50 57 63 70 72
 22 35 39 51 56 64 69 73 78
 36 38 52 55 65 68 74 77 79
 37 53 54 66 67 75 76 80 81

```



## Crystal

{{trans|Ruby}}

```ruby
def zigzag(n)
  (seq=(0...n).to_a).product(seq)
    .sort_by {|x,y| [x+y, (x+y).even? ? y : -y]}
    .map_with_index{|v, i| {v, i}}.sort.map(&.last).each_slice(n).to_a
end

def print_matrix(m)
  format = "%#{m.flatten.max.to_s.size}d " * m[0].size
  m.each {|row| puts format % row}
end

print_matrix zigzag(5)
```

{{out}}

```txt

 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

```



## D

{{trans|Common Lisp}}

```d
int[][] zigZag(in int n) pure nothrow @safe {
    static void move(in int n, ref int i, ref int j)
    pure nothrow @safe @nogc {
        if (j < n - 1) {
            if (i > 0) i--;
            j++;
        } else
            i++;
    }

    auto a = new int[][](n, n);
    int x, y;
    foreach (v; 0 .. n ^^ 2) {
        a[y][x] = v;
        (x + y) % 2 ? move(n, x, y) : move(n, y, x);
    }
    return a;
}

void main() {
    import std.stdio;

    writefln("%(%(%2d %)\n%)", 5.zigZag);
}
```

{{out}}

```txt
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
```



### Alternative Version

{{trans|Scala}}
Same output.

```d
import std.stdio, std.algorithm, std.range, std.array;

int[][] zigZag(in int n) pure nothrow {
    static struct P2 { int x, y; }
    const L = iota(n ^^ 2).map!(i => P2(i % n, i / n)).array
              .sort!q{ (a.x + a.y == b.x + b.y) ?
                       ((a.x + a.y) % 2 ? a.y < b.y : a.x < b.x) :
                       (a.x + a.y) < (b.x + b.y) }.release;

    auto result = new typeof(return)(n, n);
    foreach (immutable i, immutable p; L)
        result[p.y][p.x] = i;
    return result;
}

void main() {
    writefln("%(%(%2d %)\n%)", 5.zigZag);
}
```



## E

First, some tools originally written for [[Spiral]] (only the array is used):

{{E 2D utilities}}

Then the code. {{trans|D}}

```e
def zigZag(n) {
  def move(&i, &j) {
      if (j < (n - 1)) {
          i := 0.max(i - 1)
          j += 1
      } else {
          i += 1
      }
  }

  def array := makeFlex2DArray(n, n)
  var x := 0
  var y := 0

  for i in 1..n**2 {
      array[y, x] := i
      if ((x + y) % 2 == 0) {
          move(&x, &y)
      } else {
          move(&y, &x)
      }
  }
  return array
}
```



## Elena

{{trans|C#}}
ELENA 4.x:

```elena
import extensions;

extension op : IntNumber
{
    zigzagMatrix()
    {
        auto result := new IntMatrix(self, self);

        int i := 0;
        int j := 0;
        int d := -1;
        int start := 0;
        int end := self*self - 1;

        while (start < end)
        {
            result.setAt(i, j, start); start += 1;
            result.setAt(self - i - 1, self - j - 1, end); end -= 1;

            i := i + d;
            j := j - d;
            if (i < 0)
            {
                i:=i+1; d := d.Negative
            }
            else if (j < 0)
            {
                j := j + 1; d := d.Negative
            }
        };

        if (start == end)
        {
            result.setAt(i, j, start)
        };

        ^ result
    }
}

public program()
{
    console.printLine(5.zigzagMatrix()).readChar()
}
```



## Elixir


```elixir
defmodule RC do
  require Integer
  def zigzag(n) do
    fmt = "~#{to_char_list(n*n-1) |> length}w "
    (for x <- 1..n, y <- 1..n, do: {x,y})
      |> Enum.sort_by(fn{x,y}->{x+y, if(Integer.is_even(x+y), do: y, else: x)} end)
      |> Enum.with_index |> Enum.sort
      |> Enum.each(fn {{_x,y},i} ->
           :io.format fmt, [i]
           if y==n, do: IO.puts ""
         end)
  end
end

RC.zigzag(5)
```


{{out}}

```txt

 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

```



## Erlang


```Erlang

-module( zigzag ).

-export( [matrix/1, task/0] ).

matrix( N ) ->
	{{_X_Y, N}, Proplist} = lists:foldl( fun matrix_as_proplist/2, {{{0, 0}, N}, []}, lists:seq(0, (N * N) - 1) ),
	[columns( X, Proplist ) || X <- lists:seq(0, N - 1)].

task() -> matrix( 5 ).



columns( Column, Proplist ) -> lists:sort( [Value || {{_X, Y}, Value} <- Proplist, Y =:= Column] ).

matrix_as_proplist( N, {{X_Y, Max}, Acc} ) ->
	Next = next_indexes( X_Y, Max ),
	{{Next, Max}, [{X_Y, N} | Acc]}.

next_indexes( {X, Y}, Max ) when Y + 1 =:= Max, (X + Y) rem 2 =:= 0  -> {X + 1, Y - 1};
next_indexes( {X, Y}, Max ) when Y + 1 =:= Max, (X + Y) rem 2 =:= 1  -> {X + 1, Y};
next_indexes( {X, Y}, Max ) when X + 1 =:= Max, (X + Y) rem 2 =:= 0  -> {X, Y + 1};
next_indexes( {X, Y}, Max ) when X + 1 =:= Max, (X + Y) rem 2 =:= 1  -> {X - 1, Y + 1};
next_indexes( {X, 0}, _Max ) when X rem 2 =:= 0 -> {X + 1, 0};
next_indexes( {X, 0}, _Max ) when X rem 2 =:= 1 -> {X - 1, 1};
next_indexes( {0, Y}, _Max ) when Y rem 2 =:= 0 -> {1, Y - 1};
next_indexes( {0, Y}, _Max ) when Y rem 2 =:= 1 -> {0, Y + 1};
next_indexes( {X, Y}, _Max ) when (X + Y) rem 2 =:= 0 -> {X + 1, Y - 1};
next_indexes( {X, Y}, _Max ) when (X + Y) rem 2 =:= 1 -> {X - 1, Y + 1}.

```

{{out}}

```txt

71> zigzag:task().
[[0,1,5,6,14],
 [2,4,7,13,15],
 [3,8,12,16,21],
 [9,11,17,20,22],
 [10,18,19,23,24]]

```




## ERRE


```ERRE
PROGRAM ZIG_ZAG

!$DYNAMIC
     DIM ARRAY%[0,0]

BEGIN
     SIZE%=5
     !$DIM ARRAY%[SIZE%-1,SIZE%-1]

     I%=1
     J%=1
     FOR E%=0 TO SIZE%^2-1 DO
          ARRAY%[I%-1,J%-1]=E%
          IF ((I%+J%) AND 1)=0 THEN
              IF J%<SIZE% THEN J%+=1 ELSE I%+=2 END IF
              IF I%>1 THEN I%-=1 END IF
           ELSE
              IF I%<SIZE% THEN I%+=1 ELSE J%+=2 END IF
              IF J%>1 THEN J%-=1 END IF
          END IF
     END FOR

     FOR ROW%=0 TO SIZE%-1 DO
         FOR COL%=0 TO SIZE%-1 DO
            WRITE("###";ARRAY%[ROW%,COL%];)
         END FOR
         PRINT
     END FOR
END PROGRAM
```

{{out}}

```txt
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
```



## Euphoria

{{trans|C#}}

```Euphoria
function zigzag(integer size)
    sequence s
    integer i, j, d, max
    s = repeat(repeat(0,size),size)
    i = 1  j = 1  d = -1
    max = size*size
    for n = 1 to floor(max/2)+1 do
        s[i][j] = n
        s[size-i+1][size-j+1] = max-n+1
        i += d  j-= d
        if i < 1 then
            i += 1  d = -d
        elsif j < 1 then
            j += 1  d = -d
        end if
    end for
    return s
end function

? zigzag(5)
```

{{out}}
 {
   {1,2,6,7,15},
   {3,5,8,14,16},
   {4,9,13,17,22},
   {10,12,18,21,23},
   {11,19,20,24,25}
 }

=={{header|F_Sharp|F#}}==

```fsharp

//Produce a zig zag matrix - Nigel Galloway: April 7th., 2015
let zz l a =
  let N = Array2D.create l a 0
  let rec gng (n, i, g, e) =
    N.[n,i] <- g
    match e with
    | _ when i=a-1 && n=l-1 -> N
    | 1 when n = l-1        -> gng (n, i+1, g+1, 2)
    | 2 when i = a-1        -> gng (n+1, i, g+1, 1)
    | 1 when i = 0          -> gng (n+1, 0, g+1, 2)
    | 2 when n = 0          -> gng (0, i+1, g+1, 1)
    | 1                     -> gng (n+1, i-1, g+1, 1)
    | _                     -> gng (n-1, i+1, g+1, 2)
  gng (0, 0, 0, 2)

```

{{out}}

```fsharp>zz 5 5</lang


```txt

[[0; 1; 5; 6; 14]
 [2; 4; 7; 13; 15]
 [3; 8; 12; 16; 21]
 [9; 11; 17; 20; 22]
 [10; 18; 19; 23; 24]]

```


```fsharp>zz 8 8</lang


```txt

[[0; 1; 5; 6; 14; 15; 27; 28]
 [2; 4; 7; 13; 16; 26; 29; 42]
 [3; 8; 12; 17; 25; 30; 41; 43]
 [9; 11; 18; 24; 31; 40; 44; 53]
 [10; 19; 23; 32; 39; 45; 52; 54]
 [20; 22; 33; 38; 46; 51; 55; 60]
 [21; 34; 37; 47; 50; 56; 59; 61]
 [35; 36; 48; 49; 57; 58; 62; 63]]

```

Let's try something a little less square man

```fsharp>zz 5 8</lang


```txt

[[0; 1; 5; 6; 14; 15; 24; 25]
 [2; 4; 7; 13; 16; 23; 26; 33]
 [3; 8; 12; 17; 22; 27; 32; 34]
 [9; 11; 18; 21; 28; 31; 35; 38]
 [10; 19; 20; 29; 30; 36; 37; 39]]

```



## Factor


```factor
USING: columns fry kernel make math math.ranges prettyprint
sequences sequences.cords sequences.extras ;
IN: rosetta-code.zig-zag-matrix

: [1,b,1] ( n -- seq )
    [1,b] dup but-last-slice <reversed> cord-append ;

: <reversed-evens> ( seq -- seq' )
    [ even? [ <reversed> ] when ] map-index ;

: diagonals ( n -- seq )
    [ sq <iota> ] [ [1,b,1] ] bi
    [ [ cut [ , ] dip ] each ] { } make nip <reversed-evens> ;

: zig-zag-matrix ( n -- seq )
    [ diagonals ] [ dup ] bi '[
        [
            dup 0 <column> _ head ,
            [ _ < [ rest-slice ] when ] map-index harvest
        ] until-empty
    ] { } make ;

: zig-zag-demo ( -- ) 5 zig-zag-matrix simple-table. ;

MAIN: zig-zag-demo
```

{{out}}

```txt

0  1  5  6  14
2  4  7  13 15
3  8  12 16 21
9  11 17 20 22
10 18 19 23 24

```



## Fan


```Fan
using gfx  // for Point; convenient x/y wrapper

**
** A couple methods for generating a 'zigzag' array like
**
**   0  1  5  6
**   2  4  7 12
**   3  8 11 13
**   9 10 14 15
**
class ZigZag
{
  ** return an n x n array of uninitialized Int
  static Int[][] makeSquareArray(Int n)
  {
    Int[][] grid := Int[][,] {it.size=n}
    n.times |i| { grid[i] = Int[,] {it.size=n} }
    return grid
  }


  Int[][] zig(Int n)
  {
    grid := makeSquareArray(n)

    move := |Int i, Int j->Point|
    { return j < n - 1 ? Point(i <= 0 ? 0 : i-1, j+1) : Point(i+1, j) }
    pt := Point(0,0)
    (n*n).times |i| {
      grid[pt.y][pt.x] = i
      if ((pt.x+pt.y)%2 != 0) pt = move(pt.x,pt.y)
      else {tmp:= move(pt.y,pt.x); pt = Point(tmp.y, tmp.x) }
    }
    return grid
  }

  public static Int[][] zag(Int size)
  {
    data := makeSquareArray(size)

    Int i := 1
    Int j := 1
    for (element:=0; element < size * size; element++)
    {
      data[i - 1][j - 1] = element
      if((i + j) % 2 == 0) {
        // Even stripes
        if (j < size) {
          j++
        } else {
          i += 2
        }
        if (i > 1) {
          i--
        }
      } else {
        // Odd stripes
        if (i < size) {
          i++;
        } else {
          j += 2
        }
        if (j > 1) {
          j--
        }
      }
    }
    return data;
  }

  Void print(Int[][] data)
  {
    data.each |row|
    {
      buf := StrBuf()
      row.each |num|
      {
        buf.add(num.toStr.justr(3))
      }
      echo(buf)
    }
  }

  Void main()
  {
    echo("zig method:")
    print(zig(8))
    echo("\nzag method:")
    print(zag(8))
  }
}
```



## Forth


```forth
0 value diag

: south  diag abs + cell+ ;

' cell+ value zig
' south value zag

: init ( n -- )
  1- cells negate to diag
  ['] cell+ to zig
  ['] south to zag ;

: swap-diag   zig zag to zig to zag ;

: put ( n addr -- n+1 addr )
  2dup !  swap 1+ swap ;

: turn ( addr -- addr+E/S )
  zig execute  swap-diag
  diag negate to diag ;

: zigzag ( matrix n -- )
  { n } n init
  0 swap
  n 1 ?do
    put turn
    i 0 do put diag + loop
  loop
  swap-diag
  n 1 ?do
    put turn
    n i 1+ ?do put diag + loop
  loop
  ! ;

: .matrix ( n matrix -- )
  over 0 do
    cr
    over 0 do
      dup @ 3 .r cell+
    loop
  loop 2drop ;

: test ( n -- )  here over zigzag here .matrix ;
5 test
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24 ok
```




## Fortran

{{works with|Fortran|90 and later}}

```fortran
PROGRAM ZIGZAG

  IMPLICIT NONE
    INTEGER, PARAMETER :: size = 5
    INTEGER :: zzarray(size,size), x(size*size), y(size*size), i, j

    ! index arrays
    x = (/ ((j, i = 1, size), j = 1, size) /)
    y = (/ ((i, i = 1, size), j = 1, size) /)

    ! Sort indices
    DO i = 2, size*size
       j = i - 1
       DO WHILE (j>=1 .AND. (x(j)+y(j)) > (x(i)+y(i)))
          j = j - 1
       END DO
       x(j+1:i) = cshift(x(j+1:i),-1)
       y(j+1:i) = cshift(y(j+1:i),-1)
    END DO

    ! Create zig zag array
    DO i = 1, size*size
       IF (MOD(x(i)+y(i), 2) == 0) THEN
          zzarray(x(i),y(i)) = i - 1
       ELSE
          zzarray(y(i),x(i)) = i - 1
       END IF
    END DO

    ! Print zig zag array
    DO j = 1, size
       DO i = 1, size
          WRITE(*, "(I5)", ADVANCE="NO") zzarray(i,j)
       END DO
       WRITE(*,*)
    END DO

 END PROGRAM ZIGZAG
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim As Integer n

Do
  Input "Enter size of matrix "; n
Loop Until n > 0

Dim zigzag(1 To n, 1 To n) As Integer '' all zero by default

' enter the numbers 0 to (n^2 - 1) in the matrix's anti-diagonals
zigzag(1, 1) = 0
If n > 1 Then
  Dim As Integer row = 0, col = 3
  Dim As Boolean down = true, increment = true
  Dim As Integer i = 0, j = 2, k
  Do
    If down Then
      For k = 1 To j
        i += 1
        row += 1
        col -= 1
        zigzag(row, col) = i
      Next
      down = false
    Else
      For k = 1 To j
        i += 1
        row -= 1
        col += 1
        zigzag(row, col) = i
      Next
      down = true
    End If
    If increment Then
      j += 1
      If j > n Then
        j = n - 1
        increment = false
      End If
    Else
      j -= 1
      If j = 0 Then Exit Do
    End If
    If down AndAlso increment Then
      col += 2
      row -= 1
    ElseIf Not Down AndAlso increment Then
      row += 2
      col -= 1
    ElseIf down AndAlso Not increment Then
      col += 1
    Else '' Not down AndAlso NotIncrement
      row += 1
    End If
  Loop
End If

' print zigzag matrix if n < 20
Print
If n < 20 Then
  For i As Integer = 1 To n
    For j As Integer = 1 To n
      Print Using "####"; zigzag(i, j);
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

Enter size of matrix ? 8

   0   1   5   6  14  15  27  28
   2   4   7  13  16  26  29  42
   3   8  12  17  25  30  41  43
   9  11  18  24  31  40  44  53
  10  19  23  32  39  45  52  54
  20  22  33  38  46  51  55  60
  21  34  37  47  50  56  59  61
  35  36  48  49  57  58  62  63

```



## GAP


```gap
ZigZag := function(n)
  local a, i, j, k;
  a := NullMat(n, n);
  i := 1;
  j := 1;
  for k in [0 .. n*n - 1] do
    a[i][j] := k;
    if (i + j) mod 2 = 0 then
      if j < n then
        j := j + 1;
      else
        i := i + 2;
      fi;
      if i > 1 then
        i := i - 1;
      fi;
    else
      if i < n then
        i := i + 1;
      else
        j := j + 2;
      fi;
      if j > 1 then
        j := j - 1;
      fi;
    fi;
  od;
  return a;
end;

PrintArray(ZigZag(5));
# [ [   0,   1,   5,   6,  14 ],
#   [   2,   4,   7,  13,  15 ],
#   [   3,   8,  12,  16,  21 ],
#   [   9,  11,  17,  20,  22 ],
#   [  10,  18,  19,  23,  24 ] ]
```


## Go

{{trans|Groovy}}  Edge direct algorithm

```go
package main

import (
    "fmt"
    "strconv"
)

func zz(n int) []int {
    r := make([]int, n*n)
    i := 0
    n2 := n * 2
    for d := 1; d <= n2; d++ {
        x := d - n
        if x < 0 {
            x = 0
        }
        y := d - 1
        if y > n-1 {
            y = n - 1
        }
        j := n2 - d
        if j > d {
            j = d
        }
        for k := 0; k < j; k++ {
            if d&1 == 0 {
                r[(x+k)*n+y-k] = i
            } else {
                r[(y-k)*n+x+k] = i
            }
            i++
        }
    }

    return r
}

func main() {
    const n = 5
    w := len(strconv.Itoa(n*n - 1))
    for i, e := range zz(n) {
        fmt.Printf("%*d ", w, e)
        if i%n == n-1 {
            fmt.Println("")
        }
    }
}
```

{{out}}

```txt

 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

```



## Groovy



###  Edge

An odd technique that traverses the grid edges directly
and calculates the transform onto the grid.


```groovy
def zz = { n ->
  grid = new int[n][n]
  i = 0
  for (d in 1..n*2) {
    (x, y) = [Math.max(0, d - n), Math.min(n - 1, d - 1)]
     Math.min(d, n*2 - d).times {
       grid[d%2?y-it:x+it][d%2?x+it:y-it] = i++;
      }
  }
  grid
}
```


{{out}}

```txt

 > zz(5).each { it.each { print("${it}".padLeft(3)) }; println() }
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```



###  Cursor


Ported from the Java example


```groovy
def zz = { n->
  move = { i, j -> j < n - 1 ? [i <= 0 ? 0 : i-1, j+1] : [i+1, j] }
  grid = new int[n][n]
  (x, y) = [0, 0]
  (n**2).times {
    grid[y][x] = it
    if ((x+y)%2) (x,y) = move(x,y)
    else (y,x) = move(y,x)
  }
  grid
}
```


{{out}}

```txt

 > zz(5).each { it.each { print("${it}".padLeft(3)) }; println() }
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```



###  Sorting

Ported from the Python example with some input from J


```groovy
def zz = { n ->
  (0..<n*n).collect { [x:it%n,y:(int)(it/n)] }.sort { c->
    [c.x+c.y, (((c.x+c.y)%2) ? c.y : -c.y)]
  }.with { l -> l.inject(new int[n][n]) { a, c -> a[c.y][c.x] = l.indexOf(c); a } }
}
```


{{out}}

```txt

 > zz(5).each { it.each { print("${it}".padLeft(3)) }; println() }
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```



## Haskell


Computing the array:


```haskell
import Data.Array (array, bounds, range, (!))
import Data.Monoid (mappend)
import Data.List (sortBy)
import Text.Printf (printf)

compZig (x, y) (x', y') =
  compare (x + y) (x' + y') `mappend`
  if even (x + y)
    then compare x x'
    else compare y y'

zigZag upper = array b $ zip (sortBy compZig (range b)) [0 ..]
  where
    b = ((0, 0), upper)
```


<tt>compZig</tt> compares coordinates using the order of a zigzag walk:
primarily, the antidiagonals; secondarily, alternating directions along them.

In <tt>zigZag</tt>, <tt>array</tt> takes the bounds and a list of indexes paired with values.
We take the list of all indexes, <tt>range b</tt>, and sort it in the zigzag order, then zip that with the integers starting from 0.
(This algorithm was inspired by the explanation of the J example.)

Displaying the array (not part of the task):


```haskell
-- format a 2d array of integers neatly
show2d a =
  unlines
    [ unwords
       [ printf "%3d" (a ! (x, y) :: Integer)
       | x <- axis fst ]
    | y <- axis snd ]
  where
    (l, h) = bounds a
    axis f = [f l .. f h]

main = mapM_ (putStr . ('\n' :) . show2d . zigZag) [(3, 3), (4, 4), (10, 2)]
```



Or, building a list of lists with mapAccumL:

```haskell
import Data.Text (justifyRight, pack, unpack)
import Data.List (mapAccumL)
import Data.Bool (bool)

zigZag :: Int -> [[Int]]
zigZag = horizontals <*> diagonals
  where
    diagonals n =
      let slope = [1 .. n - 1]
      in snd $
         mapAccumL
           (\xs h ->
               let (grp, rst) = splitAt h xs
               in (rst, bool id reverse (0 /= mod h 2) grp))
           [0 .. (n * n) - 1]
           (slope ++ [n] ++ reverse slope)
    horizontals n xss
      | null xss = []
      | otherwise =
        let (edge, rst) = splitAt n xss
        in (head <$> edge) :
           horizontals n (dropWhile null (tail <$> edge) ++ rst)

main :: IO ()
main =
  putStrLn $
  unlines $
  concatMap unpack . fmap (justifyRight 3 ' ' . pack . show) <$> zigZag 5
```

{{Out}}

```txt
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24
```


=={{header|Icon}} and {{header|Unicon}}==
This solution works for both Icon and Unicon.


```icon
procedure main(args)
   n := integer(!args) | 5
   every !(A := list(n)) := list(n)
   A := zigzag(A)
   show(A)
end

procedure show(A)
    every writes(right(!A,5) | "\n")
end

procedure zigzag(A)
    x := [0,0]
    every i := 0 to (*A^2 -1) do {
        x := nextIndices(*A, x)
        A[x[1]][x[2]] := i
        }
    return A
end

procedure nextIndices(n, x)
    return if (x[1]+x[2])%2 = 0
           then if x[2] = n then [x[1]+1, x[2]] else [max(1, x[1]-1), x[2]+1]
           else if x[1] = n then [x[1], x[2]+1] else [x[1]+1, max(1, x[2]-1)]
end
```


{{out}}

```txt
->zz
    0    1    5    6   14
    2    4    7   13   15
    3    8   12   16   21
    9   11   17   20   22
   10   18   19   23   24
->

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "ZigZag.bas"
110 LET SIZE=5
120 NUMERIC A(1 TO SIZE,1 TO SIZE)
130 LET I,J=1
140 FOR E=0 TO SIZE^2-1
150   LET A(I,J)=E
160   IF ((I+J) BAND 1)=0 THEN
170     IF J<SIZE THEN
180       LET J=J+1
190     ELSE
200       LET I=I+2
210     END IF
220     IF I>1 THEN LET I=I-1
230   ELSE
240     IF I<SIZE THEN
250       LET I=I+1
260     ELSE
270       LET J=J+2
280     END IF
290     IF J>1 THEN LET J=J-1
300   END IF
310 NEXT
320 FOR ROW=1 TO SIZE
330   FOR COL=1 TO SIZE
340     PRINT USING " ##":A(ROW,COL);
350   NEXT
360   PRINT
370 NEXT
```



## J


A succinct way:

```j
   ($ [: /:@; <@|.`</.@i.)@,~ 5
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
```


This version is longer, but more "mathematical" and less "procedural":

```j
   ($ [: /:@; [: <@(A.~_2|#)/. i.)@,~ 5
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
```


Leveraging a [[Talk:Zig Zag#reading_the_J_examples|useful relationship among the indices]]:

```j
   ($ ([: /:@;@(+/"1 <@|.`</. ]) (#: i.@(*/))))@,~ 5
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
```


By the way, all the edge cases are handled transparently,
without any special checks.
Furthermore, by simply ''removing'' the trailing <tt>@,~</tt>
from the solutions, they automatically generalize
to rectangular (non-square) matrices:

```j
   ($ [: /:@; [: <@|.`</. i.) 5 3
0  1  5
2  4  6
3  7 11
8 10 12
9 13 14
```



## Java

{{trans|Ada}}

```java
public static int[][] Zig_Zag(final int size)
{
 int[][] data = new int[size][size];
 int i = 1;
 int j = 1;
 for (int element = 0; element < size * size; element++)
 {
  data[i - 1][j - 1] = element;
  if ((i + j) % 2 == 0)
  {
   // Even stripes
   if (j < size)
    j++;
   else
    i+= 2;
   if (i > 1)
    i--;
  }
  else
  {
   // Odd stripes
   if (i < size)
    i++;
   else
    j+= 2;
   if (j > 1)
    j--;
  }
 }
 return data;
}
```



## JavaScript



### Imperative


{{works with|SpiderMonkey}} for the <code>print()</code> function.

{{trans|Java}}

Subclasses the Matrix class defined at [[Matrix Transpose#JavaScript]]

```javascript
function ZigZagMatrix(n) {
    this.height = n;
    this.width = n;

    this.mtx = [];
    for (var i = 0; i < n; i++)
        this.mtx[i] = [];

    var i=1, j=1;
    for (var e = 0; e < n*n; e++) {
        this.mtx[i-1][j-1] = e;
        if ((i + j) % 2 == 0) {
            // Even stripes
            if (j < n) j ++;
            else       i += 2;
            if (i > 1) i --;
        } else {
            // Odd stripes
            if (i < n) i ++;
            else       j += 2;
            if (j > 1) j --;
        }
    }
}
ZigZagMatrix.prototype = Matrix.prototype;

var z = new ZigZagMatrix(5);
print(z);
print();

z = new ZigZagMatrix(4);
print(z);
```

{{out}}

```txt
0,1,5,6,14
2,4,7,13,15
3,8,12,16,21
9,11,17,20,22
10,18,19,23,24

0,1,5,6
2,4,7,12
3,8,11,13
9,10,14,15
```



### Functional



### =ES5=



```JavaScript
(function (n) {

    // Read range of values into a series of 'diagonal rows'
    // for a square of given dimension,
    // starting at diagonal row i.
    //  [
    //   [0],
    //   [1, 2],
    //   [3, 4, 5],
    //   [6, 7, 8, 9],
    //   [10, 11, 12, 13, 14],
    //   [15, 16, 17, 18],
    //   [19, 20, 21],
    //   [22, 23],
    //   [24]
    //  ]

    // diagonals :: n -> [[n]]
      function diagonals(n) {
          function diags(xs, iCol, iRow) {
              if (iCol < xs.length) {
                  var xxs = splitAt(iCol, xs);

                  return [xxs[0]].concat(diags(
                      xxs[1],
                      (iCol + (iRow < n ? 1 : -1)),
                      iRow + 1
                  ));
              } else return [xs];
          }

          return diags(range(0, n * n - 1), 1, 1);
      }



    // Recursively read off n heads from the diagonals (as rows)
    // n -> [[n]] -> [[n]]
    function nHeads(n, lst) {
        var zipEdge = lst.slice(0, n);

        return lst.length ? [zipEdge.map(function (x) {
            return x[0];
        })].concat(nHeads(n, [].concat.apply([], zipEdge.map(function (
                x) {
                return x.length > 1 ? [x.slice(1)] : [];
            }))
            .concat(lst.slice(n)))) : [];
    }

    // range(intFrom, intTo, optional intStep)
    // Int -> Int -> Maybe Int -> [Int]
    function range(m, n, delta) {
        var d = delta || 1,
            blnUp = n > m,
            lng = Math.floor((blnUp ? n - m : m - n) / d) + 1,
            a = Array(lng),
            i = lng;

        if (blnUp)
            while (i--) a[i] = (d * i) + m;
        else
            while (i--) a[i] = m - (d * i);
        return a;
    }

    // splitAt :: Int -> [a] -> ([a],[a])
    function splitAt(n, xs) {
        return [xs.slice(0, n), xs.slice(n)];
    }

    // Recursively take n heads from the alternately reversed diagonals

    //  [                                            [
    //   [0],           ->    [0, 1, 5, 6, 14] and:
    //   [1, 2],                                       [2],
    //   [5, 4, 3],                                    [4, 3],
    //   [6, 7, 8, 9],                                 [7, 8, 9],
    //   [14, 13, 12, 11, 10],                         [13, 12, 11, 10],
    //   [15, 16, 17, 18],                             [15, 16, 17, 18],
    //   [21, 20, 19],                                 [21, 20, 19],
    //   [22, 23],                                     [22, 23],
    //   [24]                                          [24]
    // ]                                             ]
    //
    //    In the next recursion with the remnant on the right, the next
    //    5 heads will be [2, 4, 7, 13, 15] - the second row of our zig zag matrix.
    //    (and so forth)


    return nHeads(n, diagonals(n)
        .map(function (x, i) {
            i % 2 || x.reverse();
            return x;
        }));

})(5);
```


{{Out}}


```JavaScript
[[0, 1, 5, 6, 14],
 [2, 4, 7, 13, 15],
 [3, 8, 12, 16, 21],
 [9, 11, 17, 20, 22],
 [10, 18, 19, 23, 24]]
```



### =ES6=



```JavaScript
(n => {

    // diagonals :: n -> [[n]]
    function diagonals(n) {
        let diags = (xs, iCol, iRow) => {
            if (iCol < xs.length) {
                let xxs = splitAt(iCol, xs);

                return [xxs[0]].concat(diags(
                    xxs[1],
                    iCol + (iRow < n ? 1 : -1),
                    iRow + 1
                ));
            } else return [xs];
        }

        return diags(range(0, n * n - 1), 1, 1);
    }


    // Recursively read off n heads of diagonal lists
    // rowsFromDiagonals :: n -> [[n]] -> [[n]]
    function rowsFromDiagonals(n, lst) {
        if (lst.length) {
            let [edge, rest] = splitAt(n, lst);

            return [edge.map(x => x[0])]
                .concat(rowsFromDiagonals(n,
                    edge.filter(x => x.length > 1)
                    .map(x => x.slice(1))
                    .concat(rest)
                ));
        } else return [];
    }

    // GENERIC FUNCTIONS

    // splitAt :: Int -> [a] -> ([a],[a])
    function splitAt(n, xs) {
        return [xs.slice(0, n), xs.slice(n)];
    }

    // range :: From -> To -> Maybe Step -> [Int]
    // range :: Int -> Int -> Maybe Int -> [Int]
    function range(m, n, step) {
        let d = (step || 1) * (n >= m ? 1 : -1);

        return Array.from({
            length: Math.floor((n - m) / d) + 1
        }, (_, i) => m + (i * d));
    }

    // ZIG-ZAG MATRIX

    return rowsFromDiagonals(n,
        diagonals(n)
        .map((x, i) => (i % 2 || x.reverse()) && x)
    );

})(5);
```


{{Out}}

```JavaScript
[[0, 1, 5, 6, 14],
 [2, 4, 7, 13, 15],
 [3, 8, 12, 16, 21],
 [9, 11, 17, 20, 22],
 [10, 18, 19, 23, 24]]
```



## Joy


```Joy

(*
    From the library.
*)
DEFINE reverse == [] swap shunt;
       shunt   == [swons] step.

(*
    Split according to the parameter given.
*)
DEFINE take-drop  == [dup] swap dup [[] cons [take swap] concat concat] dip []
                     cons concat [drop] concat.

(*
    Take the first of a list of lists.
*)
DEFINE take-first == [] cons 3 [dup] times [dup] swap concat [take [first] map
                     swap dup] concat swap concat [drop swap] concat swap
                     concat [take [rest] step []] concat swap concat [[cons]
                     times swap concat 1 drop] concat.

DEFINE zigzag ==

(*
    Use take-drop to generate a list of lists.
*)
4 [dup] times 1 swap from-to-list swap pred 1 swap from-to-list reverse concat
swap dup * pred 0 swap from-to-list swap [take-drop i] step [pop list] [cons]
while

(*
    The odd numbers must be modified with reverse.
*)
[dup size 2 div popd [1 =] [pop reverse] [pop] ifte] map

(*
    Take the first of the first of n lists.
*)
swap dup take-first [i] cons times pop

(*
    Merge the n separate lists.
*)
[] [pop list] [cons] while

(*
    And print them.
*)
swap dup * pred 'd 1 1 format size succ [] cons 'd swons [1 format putchars]
concat [step '\n putch] cons step.

11 zigzag.
```



## jq

Infrastructure:

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
      (""; "\(.) \($in[$i][$j] | right )" ) + "\n" ) ;

```

Create a zigzag matrix by zigzagging:

```jq
def zigzag(n):

  # unless m == n*n, place m at (i,j), pointing
  # in the direction d, where d = [drow, dcolumn]:
  def _next(i; j; m; d):
    if m == (n*n) then . else .[i][j] = m end
    | if m == (n*n) - 1 then .
      elif i == n-1 then if j+1 < n then .[i][j+1] = m+1 | _next(i-1; j+2; m+2; [-1, 1]) else . end
      elif i ==   0 then if j+1 < n then .[i][j+1] = m+1 | _next(i+1; j  ; m+2; [ 1,-1])
                         else            .[i+1][j] = m+1 | _next(i+2; j-1; m+2; [ 1,-1]) end
      elif j == n-1 then if i+1 < n then .[i+1][j] = m+1 | _next(i+2; j-1; m+2; [ 1,-1]) else . end
      elif j ==   0 then if i+1 < n then .[i+1][j] = m+1 | _next(i;   j+1; m+2; [-1, 1])
                         else            .[i][j+1] = m+1 | _next(i-1; j+1; m+2; [-1, 1]) end
      else _next(i+ d[0]; j+ d[1]; m+1;  d)
      end ;
  matrix(n;n;-1) | _next(0;0; 0; [0,1]) ;

# Example
zigzag(5) | neatly(4)
```

{{out}}

```txt

$ jq -n -r -f zigzag.jq
    0    1    5    6   14
    2    4    7   13   15
    3    8   12   16   21
    9   11   17   20   22
   10   18   19   23   24

```


###  another solution


```jq
#!/usr/bin/env jq -Mnrc -f
#
# solve zigzag matrix by constructing list of 2n+1 column "runs"
# and then shifting them into final form.
#
# e.g. for n=3 initial runs are [[0],[1,2],[3,4,5],[6,7],[8]]
# runs below are shown as columns:
#
#   initial column runs    0  1  3  6  8
#                             2  4  7
#                                5
#
#   reverse cols 0,2,4     0  1  5  6  8
#                             2  4  7
#                                3
#
#   shift cols 3,4 down    0  1  5
#                             2  4  6
#                                3  7  8
#
#   shift rows left        0  1  5
#   to get final zigzag    2  4  6
#                          3  7  8

    def N:  $n ;                                # size of matrix
    def NR: 2*N - 1;                            # number of runs
    def abs: if .<0 then -. else . end ;        # absolute value
    def runlen: N-(N-.|abs) ;                   # length of run
    def makeruns: [
        foreach range(1;NR+1) as $r (           # for each run
          {c:0}                                 # state counter
        ; .l = ($r|runlen)                      # length of this run
        | .r = [range(.c;.c+.l)]                # values in this run
        | .c += .l                              # increment counter
        ; .r                                    # produce run
        ) ] ;                                   # collect into array
    def even: .%2==0 ;                          # is input even?
    def reverseruns:                            # reverse alternate runs
      .[keys|map(select(even))[]] |= reverse ;
    def zeros: [range(.|N-length)|0] ;          # array of padding zeros
    def shiftdown:
      def pad($r):                              # pad run with zeros
        if $r < N                               # determine where zeros go
        then . = . + zeros                      # at back for left runs
        else . = zeros + .                      # at front for right runs
        end ;
      reduce keys[] as $r (.;.[$r] |= pad($r)); # shift rows down with pad
    def shiftleft: [
        range(N) as $r
      | [   range($r;$r+N) as $c
          | .[$c][$r]
        ]
      ] ;
    def width: [.[][]]|max|tostring|1+length;   # width of largest value
    def justify($w): (($w-length)*" ") + . ;    # leading spaces
    def format:
        width as $w                             # compute width
      | map(map(tostring | justify($w)))[]      # justify values
      | join(" ")
    ;
      makeruns                                  # create column runs
    | reverseruns                               # reverse alternate runs
    | shiftdown                                 # shift right runs down
    | shiftleft                                 # shift rows left
    | format                                    # format final result


```

{{out}}

```txt

$ ./zigzag.jq --argjson n 8
  0   1   5   6  14  15  27  28
  2   4   7  13  16  26  29  42
  3   8  12  17  25  30  41  43
  9  11  18  24  31  40  44  53
 10  19  23  32  39  45  52  54
 20  22  33  38  46  51  55  60
 21  34  37  47  50  56  59  61
 35  36  48  49  57  58  62  63

```



## Julia


###  simple solution


```Julia
function zigzag_matrix(n::Int)
    matrix = zeros(Int, n, n)
    x, y = 1, 1
    for i = 0:(n*n-1)
        matrix[y,x] = i
        if (x + y) % 2 == 0
            # Even stripes
            if x < n
                x += 1
                y -= (y > 1)
            else
                y += 1
            end
        else
            # Odd stripes
            if y < n
                x -= (x > 1)
                y += 1
            else
                x += 1
            end
        end
    end
    return matrix
end
```


{{out}}

```txt
julia> zigzag_matrix(5)
5×5 Array{Int64,2}:
  0   1   5   6  14
  2   4   7  13  15
  3   8  12  16  21
  9  11  17  20  22
 10  18  19  23  24

```



###  a more generic solution

Create an iterator that steps through a matrix's indices in the zig-zag pattern and use this to create zig-zag matrices and related objects.

'''Zig-Zag Iterator'''

```Julia

immutable ZigZag
    m::Int
    n::Int
    diag::Array{Int,1}
    cmax::Int
    numd::Int
    lohi::(Int,Int)
end

function zigzag(m::Int, n::Int)
    0<m && 0<n || error("The matrix dimensions must be positive.")
    ZigZag(m, n, [-1,1], m*n, m+n-1, extrema([m,n]))
end
zigzag(n::Int) = zigzag(n, n)

type ZZState
    cnt::Int
    cell::Array{Int,1}
    dir::Int
    dnum::Int
    dlen::Int
    dcnt::Int
end

Base.length(zz::ZigZag) = zz.cmax
Base.start(zz::ZigZag) = ZZState(1, [1,1], 1, 1, 1, 1)
Base.done(zz::ZigZag, zzs::ZZState) = zzs.cnt > zz.cmax

function Base.next(zz::ZigZag, zzs::ZZState)
    s = sub2ind((zz.m, zz.n), zzs.cell[1], zzs.cell[2])
    if zzs.dcnt == zzs.dlen
        if isodd(zzs.dnum)
            if zzs.cell[2] < zz.n
                zzs.cell[2] += 1
            else
                zzs.cell[1] += 1
            end
        else
            if zzs.cell[1] < zz.m
                zzs.cell[1] += 1
            else
                zzs.cell[2] += 1
            end
        end
        zzs.dcnt = 1
        zzs.dnum += 1
        zzs.dir = -zzs.dir
        if zzs.dnum <= zz.lohi[1]
            zzs.dlen += 1
        elseif zz.lohi[2] < zzs.dnum
            zzs.dlen -= 1
        end
    else
        zzs.cell += zzs.dir*zz.diag
        zzs.dcnt += 1
    end
    zzs.cnt += 1
    return (s, zzs)
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
println("The n = ", n, " zig-zag matrix:")
a = zeros(Int, (n, n))
for (i, s) in enumerate(zigzag(n))
    a[s] = i-1
end
println(pretty(a))

m = 3
println()
println("Generalize to a non-square matrix (", m, "x", n, "):")
a = zeros(Int, (m, n))
for (i, s) in enumerate(zigzag(m, n))
    a[s] = i-1
end
println(pretty(a))

p = primes(10^3)
n = 7
println()
println("An n = ", n, " prime spiral matrix:")
a = zeros(Int, (n, n))
for (i, s) in enumerate(zigzag(n))
    a[s] = p[i]
end
println(pretty(a))

```


{{out}}

```txt

The n = 5 zig-zag matrix:
      0  1  5  6 14
      2  4  7 13 15
      3  8 12 16 21
      9 11 17 20 22
     10 18 19 23 24

Generalize to a non-square matrix (3x5):
      0  1  5  6 11
      2  4  7 10 12
      3  8  9 13 14

An n = 7 prime spiral matrix:
       2   3  13  17  47  53 107
       5  11  19  43  59 103 109
       7  23  41  61 101 113 167
      29  37  67  97 127 163 173
      31  71  89 131 157 179 199
      73  83 137 151 181 197 211
      79 139 149 191 193 223 227

```



## Kotlin


```scala
// version 1.1.3

typealias Vector = IntArray
typealias Matrix = Array<Vector>

fun zigzagMatrix(n: Int): Matrix {
    val result = Matrix(n) { Vector(n) }
    var down = false
    var count = 0
    for (col in 0 until n) {
        if (down)
            for (row in 0..col) result[row][col - row] = count++
        else
            for (row in col downTo 0) result[row][col - row] = count++
        down = !down
    }
    for (row in 1 until n) {
        if (down)
           for (col in n - 1 downTo row) result[row + n - 1 - col][col] = count++
        else
           for (col in row until n) result[row + n - 1 - col][col] = count++
        down = !down
    }
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
    printMatrix(zigzagMatrix(5))
    printMatrix(zigzagMatrix(10))
}
```


{{out}}

```txt

 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

 0  1  5  6 14 15 27 28 44 45
 2  4  7 13 16 26 29 43 46 63
 3  8 12 17 25 30 42 47 62 64
 9 11 18 24 31 41 48 61 65 78
10 19 23 32 40 49 60 66 77 79
20 22 33 39 50 59 67 76 80 89
21 34 38 51 58 68 75 81 88 90
35 37 52 57 69 74 82 87 91 96
36 53 56 70 73 83 86 92 95 97
54 55 71 72 84 85 93 94 98 99

```



## Lasso



```lasso

var(
     'square'  = array
    ,'size'    = integer( 5 )// for a 5 X 5 square
    ,'row'     = array
    ,'x'       = integer( 1 )
    ,'y'       = integer( 1 )
    ,'counter' = integer( 1 )
);

// create place-holder matrix
loop( $size );
   $row = array;

   loop( $size );
      $row->insert( 0 );

    /loop;

   $square->insert( $row );

/loop;

while( $counter < $size * $size );
   // check downward diagonal
   if(
         $x > 1
         &&
         $y < $square->size
         &&
         $square->get( $y + 1 )->get( $x - 1 ) == 0
      );

         $x -= 1;
         $y += 1;

   // check upward diagonal
   else(
         $x < $square->size
         &&
         $y > 1
         &&
         $square->get( $y - 1 )->get( $x + 1 ) == 0
      );

         $x += 1;
         $y -= 1;

   // check right
   else(
         (
            $y == 1
            ||
            $y == $square->size
         )
         &&
         $x < $square->size
         &&
         $square->get( $y )->get( $x + 1 ) == 0
      );

      $x += 1;

   // down
   else;
      $y += 1;

   /if;

   $square->get( $y )->get( $x ) = loop_count;

   $counter += 1;

/while;

$square;

```



## Lua


```Lua

local zigzag = {}

function zigzag.new(n)
    local a = {}
    local i -- cols
    local j -- rows

    a.n = n
    a.val = {}

    for j = 1, n do
        a.val[j] = {}
        for i = 1, n do
            a.val[j][i] = 0
        end
    end

    i = 1
    j = 1

    local di
    local dj
    local k = 0

    while k < n * n do
        a.val[j][i] = k
        k = k + 1
        if i == n then
            j = j + 1
            a.val[j][i] = k
            k = k + 1
            di = -1
            dj = 1
        end
        if j == 1 then
            i = i + 1
            a.val[j][i] = k
            k = k + 1
            di = -1
            dj = 1
        end
        if j == n then
            i = i + 1
            a.val[j][i] = k
            k = k + 1
            di = 1
            dj = -1
        end
        if i == 1 then
            j = j + 1
            a.val[j][i] = k
            k = k + 1
            di = 1
            dj = -1
        end
        i = i + di
        j = j + dj
    end

    setmetatable(a, {__index = zigzag, __tostring = zigzag.__tostring})
    return a
end

function zigzag:__tostring()
    local s = {}
    for j = 1, self.n do
        local row = {}
        for i = 1, self.n do
            row[i] = string.format('%d', self.val[j][i])
        end
        s[j] = table.concat(row, ' ')
    end
    return table.concat(s, '\n')
end

print(zigzag.new(5))

```



## M4


```M4
divert(-1)

define(`set2d',`define(`$1[$2][$3]',`$4')')
define(`get2d',`defn(`$1[$2][$3]')')
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')
define(`show2d',
   `for(`x',0,decr($2),
      `for(`y',0,decr($3),`format(`%2d',get2d($1,x,y)) ')
')')

dnl  <name>,<size>
define(`zigzag',
   `define(`j',1)`'define(`k',1)`'for(`e',0,eval($2*$2-1),
      `set2d($1,decr(j),decr(k),e)`'ifelse(eval((j+k)%2),0,
         `ifelse(eval(k<$2),1,
            `define(`k',incr(k))',
            `define(`j',eval(j+2))')`'ifelse(eval(j>1),1,
            `define(`j',decr(j))')',
         `ifelse(eval(j<$2),1,
            `define(`j',incr(j))',
            `define(`k',eval(k+2))')`'ifelse(eval(k>1),1,
            `define(`k',decr(k))')')')')

divert

zigzag(`a',5)
show2d(`a',5,5)
```


{{out}}

```txt

 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Rule-based implementation, the upper-left half is correctly calculated
using a direct formula.
The lower-right half is then 'mirrored' from the upper-left half.

```Mathematica
ZigZag[size_Integer/;size>0]:=Module[{empty=ConstantArray[0,{size,size}]},
 empty=ReplacePart[empty,{i_,j_}:>1/2 (i+j)^2-(i+j)/2-i (1-Mod[i+j,2])-j Mod[i+j,2]];
 ReplacePart[empty,{i_,j_}/;i+j>size+1:> size^2-tmp[[size-i+1,size-j+1]]-1]
]
```

Ported from the java-example:

```Mathematica
ZigZag2[size_] := Module[{data, i, j, elem},
 data = ConstantArray[0, {size, size}];
 i = j = 1;
 For[elem = 0, elem < size^2, elem++,
  data[[i, j]] = elem;
  If[Mod[i + j, 2] == 0,
   If[j < size, j++, i += 2];
   If[i > 1, i--]
   ,
   If[i < size, i++, j += 2];
   If[j > 1, j--];
   ];
  ];
 data
 ]
```

Examples:

```Mathematica
ZigZag[5] // MatrixForm
ZigZag2[6] // MatrixForm
```

gives back:

<math>
\left(
\begin{array}{ccccc}
 0 & 1 & 5 & 6 & 14 \\
 2 & 4 & 7 & 13 & 15 \\
 3 & 8 & 12 & 16 & 21 \\
 9 & 11 & 17 & 20 & 22 \\
 10 & 18 & 19 & 23 & 24
\end{array}
\right)
</math>

<math>
\left(
\begin{array}{cccccc}
 0 & 1 & 5 & 6 & 14 & 15 \\
 2 & 4 & 7 & 13 & 16 & 25 \\
 3 & 8 & 12 & 17 & 24 & 26 \\
 9 & 11 & 18 & 23 & 27 & 32 \\
 10 & 19 & 22 & 28 & 31 & 33 \\
 20 & 21 & 29 & 30 & 34 & 35
\end{array}
\right)
</math>


## MATLAB

This isn't the best way to solve this task
and the algorithm is completely unintuitive
without some major exploration of the code.
But! It is pretty fast for n < 10000.


```MATLAB
function matrix = zigZag(n)

    %This is very unintiutive. This algorithm parameterizes the
    %zig-zagging movement along the matrix indicies. The easiest way to see
    %what this algorithm does is to go through line-by-line and write out
    %what the algorithm does on a peace of paper.

    matrix = zeros(n);
    counter = 1;
    flipCol = true;
    flipRow = false;

    %This for loop does the top-diagonal of the matrix
    for i = (2:n)
        row = (1:i);
        column = (1:i);

        %Causes the zig-zagging. Without these conditionals,
        %you would end up with a diagonal matrix.
        %To see what happens, comment these conditionals out.
        if flipCol
            column = fliplr(column);
            flipRow = true;
            flipCol = false;
        elseif flipRow
            row = fliplr(row);
            flipRow = false;
            flipCol = true;
        end

        %Selects a diagonal of the zig-zag matrix and places the
        %correct integer value in each index along that diagonal
        for j = (1:numel(row))
            matrix(row(j),column(j)) = counter;
            counter = counter + 1;
        end
    end

    %This for loop does the bottom-diagonal of the matrix
    for i = (2:n)
        row = (i:n);
        column = (i:n);

        %Causes the zig-zagging. Without these conditionals,
        %you would end up with a diagonal matrix.
        %To see what happens comment these conditionals out.
        if flipCol
            column = fliplr(column);
            flipRow = true;
            flipCol = false;
        elseif flipRow
            row = fliplr(row);
            flipRow = false;
            flipCol = true;
        end

        %Selects a diagonal of the zig-zag matrix and places the
        %correct integer value in each index along that diagonal
        for j = (1:numel(row))
            matrix(row(j),column(j)) = counter;
            counter = counter + 1;
        end
    end


end
```


{{out}}

```txt
>> zigZag(5)

ans =

     0     1     5     6    14
     2     4     7    13    15
     3     8    12    16    21
     9    11    17    20    22
    10    18    19    23    24
```




## Maxima


```maxima
zigzag(n) := block([a, i, j],
a: zeromatrix(n, n),
i: 1,
j: 1,
for k from 0 thru n*n - 1 do (
   a[i, j]: k,
   if evenp(i + j) then (
      if j < n then j: j + 1 else i: i + 2,
      if i > 1 then i: i - 1
   ) else (
      if i < n then i: i + 1 else j: j + 2,
      if j > 1 then j: j - 1
   )
),
a)$

zigzag(5);
/* matrix([ 0,  1,  5,  6, 14],
          [ 2,  4,  7, 13, 15],
          [ 3,  8, 12, 16, 21],
          [ 9, 11, 17, 20, 22],
          [10, 18, 19, 23, 24]) */
```


=={{header|Modula-3}}==

```modula3
MODULE ZigZag EXPORTS Main;

IMPORT IO, Fmt;

TYPE Matrix = REF ARRAY OF ARRAY OF CARDINAL;

PROCEDURE Create(size: CARDINAL): Matrix =
  PROCEDURE move(VAR i, j: INTEGER) =
    BEGIN
      IF j < (size - 1) THEN
        IF (i - 1) < 0 THEN
          i := 0;
        ELSE
          i := i - 1;
        END;
        INC(j);
      ELSE
        INC(i);
      END;
    END move;

  VAR data := NEW(Matrix, size, size);
      x, y: INTEGER := 0;
  BEGIN
    FOR v := 0 TO size * size - 1 DO
      data[y, x] := v;
      IF (x + y) MOD 2 = 0 THEN
        move(y, x);
      ELSE
        move(x, y);
      END;
    END;
    RETURN data;
  END Create;

PROCEDURE Print(data: Matrix) =
  BEGIN
    FOR i := FIRST(data^) TO LAST(data^) DO
      FOR j := FIRST(data[0]) TO LAST(data[0]) DO
        IO.Put(Fmt.F("%3s", Fmt.Int(data[i, j])));
      END;
      IO.Put("\n");
    END;
  END Print;

BEGIN
  Print(Create(5));
END ZigZag.
```

{{out}}

```txt

  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```



## NetRexx

{{trans|REXX}}

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

zigzag(5)

return

method zigzag(msize) public static

  row = 1
  col = 1

  ziggy = Rexx(0)
  loop j_ = 0 for msize * msize
    ziggy[row, col] = j_
    if (row + col) // 2 == 0 then do
      if col < msize then -
        col = col + 1
      else row = row + 2
      if row \== 1 then -
        row = row - 1
      end
    else do
      if row < msize then -
        row = row + 1
      else col = col + 2
      if col \== 1 then -
        col = col - 1
      end
    end j_

  L = (msize * msize - 1).length             /*for a constant element width.  */
  loop row = 1 for msize                     /*show all the matrix's rows.    */
    rowOut = ''
    loop col = 1 for msize
      rowOut = rowOut ziggy[row, col].right(L)
      end col
    say rowOut
    end row

  return

```



## Nim

{{trans|Python}}

```nim
import algorithm, strutils

type Pos = tuple[x, y: int]

template newSeqWith(len: int, init: expr): expr =
  var result {.gensym.} = newSeq[type(init)](len)
  for i in 0 .. <len:
    result[i] = init
  result

proc `$`(m: seq[seq[int]]): string =
  result = ""
  for r in m:
    for c in r:
      result.add align($c, 2) & " "
    result.add "\n"

proc zigzagMatrix(n): auto =
  result = newSeqWith(n, newSeq[int](n))

  var indices = newSeq[Pos]()

  for x in 0 .. <n:
    for y in 0 .. <n:
      indices.add((x,y))

  sort(indices, proc(a, b: Pos): int =
    result = a.x + a.y - b.x - b.y
    if result == 0: result =
      (if (a.x + a.y) mod 2 == 0: a.y else: -a.y) -
      (if (b.x + b.y) mod 2 == 0: b.y else: -b.y))

  for i, p in indices:
    result[p.x][p.y] = i

echo zigzagMatrix(6)
```

{{out}}

```txt
 0  1  5  6 14 15
 2  4  7 13 16 25
 3  8 12 17 24 26
 9 11 18 23 27 32
10 19 22 28 31 33
20 21 29 30 34 35
```



## Objeck

{{trans|Java}}

```ocaml

function : native : ZigZag(size : Int) ~ Int[,] {
  data := Int->New[size, size];
  i := 1;
  j := 1;

  max := size * size;
  for(element := 0; element < max ; element += 1;) {
    data[i - 1, j - 1] := element;

    if((i + j) % 2 = 0) {
      # even stripes
      if(j < size){
        j += 1;
      }
      else{
        i+= 2;
      };

      if(i > 1) {
        i -= 1;
      };
    }
    else{
      # ddd stripes
      if(i < size){
        i += 1;
      }
      else{
        j+= 2;
      };

      if(j > 1){
        j -= 1;
      };
    };
  };

  return data;
}

```




## OCaml


{{trans|Common Lisp}}


```ocaml
let zigzag n =
  (* move takes references and modifies them directly *)
  let move i j =
    if !j < n - 1 then begin
      i := max 0 (!i - 1);
      incr j
    end else
      incr i
  in
  let a = Array.make_matrix n n 0
  and x = ref 0 and y = ref 0 in
  for v = 0 to n * n - 1 do
    a.(!x).(!y) <- v;
    if (!x + !y) mod 2 = 0 then
      move x y
    else
      move y x
  done;
  a
```



## Octave

{{trans|Stata}}


```octave
function a = zigzag1(n)
  j = 1:n;
  u = repmat([-1; 1], n, 1);
  v = j.*(2*j-3);
  v = reshape([v; v+1], 2*n, 1);
  a = zeros(n, n);
  for i = 1:n
    a(:, i) = v(i+j);
    v += u;
  endfor
endfunction

function a = zigzag2(n)
  a = zigzag1(n);
  v = (1:n-1)'.^2;
  for i = 2:n
    a(n+2-i:n, i) -= v(1:i-1);
  endfor
endfunction

>> zigzag2(5)
ans =

    0    1    5    6   14
    2    4    7   13   15
    3    8   12   16   21
    9   11   17   20   22
   10   18   19   23   24
```


Alternate solution, filling pairs of diagonals.


```octave
function a = zigzag3(n)
  a = zeros(n, n);
  for k=1:n
    d = (2*(j = mod(k, 2))-1)*(n-1);
    m = (n-1)*(k-1);
    a(k+(1-j)*m:d:k+j*m) = k*(k-1)/2:k*(k+1)/2-1;
    a(n*(n+1-k)+(1-j)*m:d:n*(n+1-k)+j*m) = n*n-k*(k+1)/2:n*n-k*(k-1)/2-1;
  endfor
endfunction

>> zigzag3(5)
ans =

    0    1    5    6   14
    2    4    7   13   15
    3    8   12   16   21
    9   11   17   20   22
   10   18   19   23   24
```



## ooRexx

{{trans|Java}}

```ooRexx

call printArray zigzag(3)
say
call printArray zigzag(4)
say
call printArray zigzag(5)

::routine zigzag
  use strict arg size

  data = .array~new(size, size)
  row = 1
  col = 1

  loop element = 0 to (size * size) - 1
      data[row, col] = element
      -- even stripes
      if (row + col) // 2 = 0 then do
          if col < size then col += 1
          else row += 2
          if row > 1 then row -= 1
      end
      -- odd rows
      else do
          if row < size then row += 1
          else col += 2
          if col > 1 then col -= 1
      end
  end

  return data

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

|  0  1  5 |
|  2  4  6 |
|  3  7  8 |

|  0  1  5  6 |
|  2  4  7 12 |
|  3  8 11 13 |
|  9 10 14 15 |

|  0  1  5  6 14 |
|  2  4  7 13 15 |
|  3  8 12 16 21 |
|  9 11 17 20 22 |
| 10 18 19 23 24 |

```



## Oz

Implemented as a state machine:

```oz
declare
  %%            state          move   success     failure
  States = unit(right:        [ 1# 0  downLeft    downInstead]
                downInstead:  [ 0# 1  downLeft    terminate]
                downLeft:     [~1# 1  downLeft    down]
                down:         [ 0# 1  topRight    rightInstead]
                rightInstead: [ 1# 0  topRight    terminate]
                topRight:     [ 1#~1  topRight    right])

  fun {CreateZigZag N}
     ZZ = {Create2DTuple N N}

     %% recursively walk through 2D tuple and set values
     proc {Walk Pos=X#Y Count State}
        [Dir Success Failure] = States.State
        NextPos = {Record.zip Pos Dir Number.'+'}
        Valid = {Record.all NextPos fun {$ C} C > 0 andthen C =< N end}
        NewPos = if Valid then NextPos else Pos end
        NewCount = if Valid then Count + 1 else Count end
        NewState = if Valid then Success else Failure end
     in
        ZZ.Y.X = Count
        if NewState \= terminate then
           {Walk NewPos NewCount NewState}
        end
     end
  in
     {Walk 1#1 0 right}
     ZZ
  end

  fun {Create2DTuple W H}
     T = {MakeTuple unit H}
  in
     {Record.forAll T fun {$} {MakeTuple unit W} end}
     T
  end
in
  {Inspect {CreateZigZag 5}}
```



## PARI/GP

{{trans|C.23}}

```parigp
zz(n)={
	my(M=matrix(n,n),i,j,d=-1,start,end=n^2-1);
	while(ct--,
		M[i+1,j+1]=start;
		M[n-i,n-j]=end;
		start++;
		end--;
		i+=d;
		j-=d;
		if(i<0,
			i++;
			d=-d
		,
			if(j<0,
				j++;
				d=-d
			)
		);
		if(start>end,return(M))
	)
};
```



## Pascal


```Pascal
Program zigzag( input, output );

const
  size = 5;
var
  zzarray: array [1..size, 1..size] of integer;
  element, i, j: integer;
  direction: integer;
  width, n: integer;

begin
  i := 1;
  j := 1;
  direction := 1;
  for element := 0 to (size*size) - 1 do
  begin
    zzarray[i,j] := element;
    i := i + direction;
    j := j - direction;
    if (i = 0) then
      begin
        direction := -direction;
        i := 1;
        if (j > size) then
        begin
          j := size;
          i := 2;
        end;
      end
    else if (i > size) then
      begin
        direction := -direction;
        i := size;
        j := j + 2;
      end
    else if (j = 0) then
      begin
        direction := -direction;
        j := 1;
        if (i > size) then
        begin
          j := 2;
          i := size;
        end;
      end
    else if (j > size) then
      begin
        direction := -direction;
        j := size;
        i := i + 2;
      end;
  end;

  width := 2;
  n     := size;
  while (n > 0) do
  begin
    width := width + 1;
    n     := n div 10;
  end;
  for j := 1 to size do
  begin
    for i := 1 to size do
      write(zzarray[i,j]:width);
    writeln;
  end;
end.
```


{{out}}

```txt

  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```


{{out}} with size set to 6

```txt

  0  1  5  6 14 15
  2  4  7 13 16 25
  3  8 12 17 24 26
  9 11 18 23 27 32
 10 19 22 28 31 33
 20 21 29 30 34 35

```






{{trans|Seed7}}



```Pascal

Program zigzag;
{$APPTYPE CONSOLE}

const
  size = 5;

  var
  s: array [1..size, 1..size] of integer;
  i, j, d, max, n: integer;

begin
    i := 1;
    j := 1;
    d := -1;
    max := 0;
    n := 0;
    max := size * size;

  for n := 1 to (max div 2)+1 do begin
      s[i,j] := n;
      s[size - i + 1,size - j + 1] := max - n + 1;
      i:=i+d;
      j:=j-d;
      if i < 1 then begin
        inc(i);
        d := -d;
        end else if j < 1 then begin
        inc(j);
        d := -d;
      end;
    end;

  for j := 1 to size do
  begin
    for i := 1 to size do
      write(s[i,j]:4);
    writeln;
  end;

end.

```


{{out}} Size 5

```txt

   1   3   4  10  11
   2   5   9  12  19
   6   8  13  18  20
   7  14  17  21  24
  15  16  22  23  25

```


{{out}} Size 8

```txt

   1   3   4  10  11  21  22  36
   2   5   9  12  20  23  35  37
   6   8  13  19  24  34  38  49
   7  14  18  25  33  39  48  50
  15  17  26  32  40  47  51  58
  16  27  31  41  46  52  57  59
  28  30  42  45  53  56  60  63
  29  43  44  54  55  61  62  64

```



## Perl


```perl
use 5.010;

sub zig_zag {
    my $n          = shift;
    my $max_number = $n**2;
    my @matrix;
    my $number = 0;
    for my $j ( 0 .. --$n ) {
        for my $i (
            $j % 2
            ? 0 .. $j
            : reverse 0 .. $j
          )
        {
            $matrix[$i][ $j - $i ] = $number++;
            #next if $j == $n;
            $matrix[ $n - $i ][ $n - ( $j - $i ) ] = $max_number - $number;
        }
    }
    return @matrix;
}

my @zig_zag_matrix = zig_zag(5);
say join "\t", @{$_} foreach @zig_zag_matrix;

```



{{trans|Haskell}}

```perl
sub zig_zag {
    my ($w, $h, @r, $n) = @_;

    $r[ $_->[1] ][ $_->[0] ] = $n++	for
    	sort {	$a->[0] + $a->[1] <=> $b->[0] + $b->[1]	  or
		($a->[0] + $a->[1]) % 2
			? $a->[1] <=> $b->[1]
			: $a->[0] <=> $b->[0]
	}
	map  {	my $e = $_;
		map{ [$e, $_] } 0 .. $w-1
	} 0 .. $h - 1;
    @r
}

print map{ "@$_\n" } zig_zag(3, 5);
```



## Perl 6

Using the same Turtle class as in the [[Spiral_matrix#Perl_6|Spiral matrix]] task:


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

sub MAIN(Int $size = 5) {
    my $t = Turtle.new(dir => 1);
    my $counter = 0;
    for 1 ..^ $size -> $run {
	for ^$run {
	    $t.lay-egg($counter++);
	    $t.forward;
	}
	my $yaw = $run %% 2 ?? -1 !! 1;
	$t.turn-right($yaw * 135); $t.forward; $t.turn-right($yaw * 45);
    }
    for $size ... 1 -> $run {
	for ^$run -> $ {
	    $t.lay-egg($counter++);
	    $t.forward;
	}
	$t.turn-left(180); $t.forward;
	my $yaw = $run %% 2 ?? 1 !! -1;
	$t.turn-right($yaw * 45); $t.forward; $t.turn-left($yaw * 45);
    }
    $t.showmap;
}
```



## Phix

{{Trans|C#}}

```Phix
integer n = 9
integer zstart = 0, zend = n*n-1
--integer zstart = 1, zend = n*n
string fmt = sprintf("%%%dd",length(sprintf("%d",zend)))
sequence m = repeat(repeat("??",n),n)
integer x = 1, y = 1, d = -1
while 1 do
    m[x][y] = sprintf(fmt,zstart)
    if zstart=zend then exit end if
    zstart += 1
    m[n-x+1][n-y+1] = sprintf(fmt,zend)
    zend -= 1
    x += d
    y -= d
    if x<1 then
        x += 1
        d = -d
    elsif y<1 then
        y += 1
        d = -d
    end if
end while

for i=1 to n do
    m[i] = join(m[i])
end for
puts(1,join(m,"\n"))
```

Alternative:

```Phix
integer n = 5
string fmt = sprintf("%%%dd",length(sprintf("%d",n*n-1)))
sequence m = repeat(repeat("??",n),n)
integer x = 1, y = 1
for d=0 to n*n-1 do
    m[y][x] = sprintf(fmt,d)
    if mod(x+y,2) then
        {x,y} = iff(y<n?{x-(x>1),y+1}:{x+1,y})
    else
        {x,y} = iff(x<n?{x+1,y-(y>1)}:{x,y+1})
    end if
end for

for i=1 to n do
    m[i] = join(m[i])
end for
puts(1,join(m,"\n"))
```



## PHP


```php
function ZigZagMatrix($num) {
    $matrix = array();
    for ($i = 0; $i < $num; $i++){
		$matrix[$i] = array();
	}

    $i=1;
	$j=1;
    for ($e = 0; $e < $num*$num; $e++) {
        $matrix[$i-1][$j-1] = $e;
        if (($i + $j) % 2 == 0) {
            if ($j < $num){
				$j++;
			}else{
				$i += 2;
			}
            if ($i > 1){
				$i --;
			}
        } else {
            if ($i < $num){
				$i++;
			}else{
				$j += 2;
			}
            if ($j > 1){
				$j --;
			}
        }
    }
	return $matrix;
}
```



## PicoLisp

This example uses 'grid' from "lib/simul.l", which maintains
a two-dimensional structure and is normally used
for simulations and board games.

```PicoLisp
(load "@lib/simul.l")

(de zigzag (N)
   (prog1 (grid N N)
      (let (D '(north west  south east  .)  E '(north east .)  This 'a1)
         (for Val (* N N)
            (=: val Val)
            (setq This
               (or
                  ((cadr D) ((car D) This))
                  (prog
                     (setq D (cddr D))
                     ((pop 'E) This) )
                  ((pop 'E) This) ) ) ) ) ) )

(mapc
   '((L)
      (for This L (prin (align 3 (: val))))
      (prinl) )
   (zigzag 5) )
```

{{out}}

```txt
  1  2  6  7 15
  3  5  8 14 16
  4  9 13 17 22
 10 12 18 21 23
 11 19 20 24 25
```



## PL/I


```pli
/* Fill a square matrix with the values 0 to N**2-1,     */
/* in a zig-zag fashion.                                 */
/* N is the length of one side of the square.            */
/* Written 22 February 2010.                             */

   declare n fixed binary;

   put skip list ('Please type the size of the matrix:');
   get list (n);

begin;
   declare A(n,n) fixed binary;
   declare (i, j, inc, q) fixed binary;

   on subrg snap begin;
      declare i fixed binary;
      do  i = 1 to n;
         put skip edit (a(i,*)) (f(4));
      end;
      stop;
   end;

   A = -1;
   inc = -1;
   i, j = 1;

loop:
   do q = 0 to n**2-1;
      a(i,j) = q;
      if q = n**2-1 then leave;
      if i = 1 & j = n then
         if iand(j,1) = 1 then /* odd-sided matrix */
            do; i = i + 1; inc = -inc; iterate loop; end;
         else  /* an even-sided matrix */
            do; i = i + inc; j = j - inc; iterate loop; end;
      if inc = -1 then if i+inc < 1 then
         do; inc = -inc; j = j + 1; a(i,j) = q; iterate loop; end;
      if inc = 1 then if i+inc > n then
         do; inc = -inc; j = j + 1; a(i,j) = q; iterate loop; end;
      if inc = 1 then if j-inc < 1 then
         do; inc = -inc; i = i + 1; a(i,j) = q; iterate loop; end;
      if inc = -1 then if j - inc > n then
         do; inc = -inc; i = i + 1; a(i,j) = q; iterate loop; end;
      i = i + inc; j = j - inc;
   end;

   /* Display the square. */
   do  i = 1 to n;
      put skip edit (a(i,*)) (f(4));
   end;
end;
```

{{out}}

```txt
   0   1   5   6  14
   2   4   7  13  15
   3   8  12  16  21
   9  11  17  20  22
  10  18  19  23  24
```



## PlainTeX

The code works with any etex engine.

```tex
\long\def\antefi#1#2\fi{#2\fi#1}
\def\fornum#1=#2to#3(#4){%
	\edef#1{\number\numexpr#2}\edef\fornumtemp{\noexpand\fornumi\expandafter\noexpand\csname fornum\string#1\endcsname
		{\number\numexpr#3}{\ifnum\numexpr#4<0 <\else>\fi}{\number\numexpr#4}\noexpand#1}\fornumtemp
}
\long\def\fornumi#1#2#3#4#5#6{\def#1{\unless\ifnum#5#3#2\relax\antefi{#6\edef#5{\number\numexpr#5+(#4)\relax}#1}\fi}#1}
\def\elem(#1,#2){\numexpr(#1+#2)*(#1+#2-1)/2-(\ifodd\numexpr#1+#2\relax#1\else#2\fi)\relax}
\def\zzmat#1{%
	\noindent% quit vertical mode
	\fornum\yy=1to#1(+1){%
		\fornum\xx=1to#1(+1){%
			\ifnum\numexpr\xx+\yy\relax<\numexpr#1+2\relax
				\hbox to 2em{\hfil\number\elem(\xx,\yy)}%
			\else
				\hbox to 2em{\hfil\number\numexpr#1*#1-1-\elem(#1+1-\xx,#1+1-\yy)\relax}%
			\fi
		}%
		\par\noindent% next line + quit vertical mode
	}\par
}
\zzmat{5}
\bye
```


pdf output:

```txt
 0   1   5   6  14
 2   4   7  13  15
 3   8  12  16  21
 9  11  17  20  22
10  18  19  23  24
```



## PostScript


This implementation is far from being elegant or smart,
but it builds the ''zigzag'' how a human being could do,
and also draws lines to show the path.


```postscript
%!PS
%%BoundingBox: 0 0 300 200
/size 9 def % defines row * column (9*9 -> 81 numbers,
            % from 0 to 80)
/itoa { 2 string cvs } bind def
% visual bounding box...
% 0 0 moveto 300 0 lineto 300 200 lineto 0 200 lineto
% closepath stroke
20 150 translate
% it can be easily enhanced to support more columns and
% rows. This limit is put here just to avoid more than 2
% digits, mainly because of formatting
size size mul 99 le {
   /Helvetica findfont 14 scalefont setfont
   /ulimit size size mul def
   /sizem1 size 1 sub def
   % prepare the number list
   0 ulimit 1 sub { dup 1 add } repeat
   ulimit array astore
   /di -1 def /dj 1 def
   /ri 1 def /rj 0 def /pus true def
   0 0 moveto
   /i 0 def /j 0 def
   {  % can be rewritten a lot better :)
      0.8 setgray i 30 mul j 15 mul neg lineto stroke
      0 setgray i 30 mul j 15 mul neg moveto itoa show
      i 30 mul j 15 mul neg moveto
      pus {
         i ri add size ge {
             /ri 0 def /rj 1 def
         } if
         j rj add size ge {
             /ri 1 def /rj 0 def
         } if
         /pus false def
         /i i ri add def
         /j j rj add def
         /ri rj /rj ri def def
      } {
          i di add dup    0 le
                  exch sizem1 ge or
          j dj add dup    0 le
                  exch sizem1 ge or
             or {
                /pus true def
                /i i di add def /j j dj add def
                /di di neg def /dj dj neg def
          } {
                /i i di add def /j j dj add def
          } ifelse
      } ifelse
   } forall
   stroke showpage
} if
%%EOF
```



## PowerShell


```PowerShell
function zigzag( [int] $n ) {
    $zigzag=New-Object 'Object[,]' $n,$n
    $nodd = $n -band 1
    $nm1 = $n - 1
    $i=0;
    $j=0;
    foreach( $k in 0..( $n * $n - 1 ) ) {
        $zigzag[$i,$j] = $k
        $iodd = $i -band 1
        $jodd = $j -band 1
        if( ( $j -eq $nm1 ) -and ( $iodd -ne $nodd ) ) {
            $i++
        } elseif( ( $i -eq $nm1 ) -and ( $jodd -eq $nodd ) ) {
            $j++
        } elseif( ( $i -eq 0 ) -and ( -not $jodd ) ) {
            $j++
        } elseif( ( $j -eq 0 ) -and $iodd ) {
            $i++
        } elseif( $iodd -eq $jodd ) {
            $i--
            $j++
        } else {
            $i++
            $j--
        }
    }
    ,$zigzag
}

function displayZigZag( [int] $n ) {
    $a = zigzag $n
    0..$n | ForEach-Object {
        $b=$_
        $pad=($n*$n-1).ToString().Length
        "$(0..$n | ForEach-Object {
            "{0,$pad}" -f $a[$b,$_]
        } )"
    }
}
```



### An Alternate Display

Display the zig-zag matrix using the <code>Format-Wide</code> cmdlet:

```PowerShell

zigzag 5 | Format-Wide {"{0,2}" -f $_} -Column 5 -Force

```

{{Out}}

```txt

 0                          1                          5                          6                         14
 2                          4                          7                         13                         15
 3                          8                         12                         16                         21
 9                         11                         17                         20                         22
10                         18                         19                         23                         24

```



## Prolog

{{Works with|SWi-Prolog}}

```Prolog
zig_zag(N) :-
	zig_zag(N, N).

% compute zig_zag for a matrix of Lig lines of Col columns
zig_zag(Lig, Col) :-
	length(M, Lig),
	maplist(init(Col), M),
	fill(M, 0, 0, 0, Lig, Col, up),
	% display the matrix
	maplist(print_line, M).


fill(M, Cur, L, C, NL, NC, _) :-
	L is NL - 1,
	C is NC - 1,
	nth0(L, M, Line),
	nth0(C, Line, Cur).

fill(M, Cur, L, C, NL, NC, Sens) :-
	nth0(L, M, Line),
	nth0(C, Line, Cur),
	Cur1 is Cur + 1,
	compute_next(NL, NC, L, C, Sens, L1, C1, Sens1),
	fill(M, Cur1, L1, C1, NL, NC, Sens1).


init(N, L) :-
	length(L, N).

% compute_next
% arg1 : Number of lnes of the matrix
% arg2 : number of columns of the matrix
% arg3 : current line
% arg4 : current column
% arg5 : current direction of movement
% arg6 : nect line
% arg7 : next column
% arg8 : next direction of movement
compute_next(_NL, NC, 0, Col, up, 0, Col1, down) :-
	Col < NC - 1,
	Col1 is Col+1.

compute_next(_NL, NC, 0, Col, up, 1, Col, down) :-
	Col is NC - 1.

compute_next(NL, _NC, Lig, 0, down, Lig1, 0, up) :-
	Lig < NL - 1,
	Lig1 is Lig+1.

compute_next(NL, _NC, Lig, 0, down, Lig, 1, up) :-
	Lig is NL - 1.

compute_next(NL, _NC, Lig, Col, down, Lig1, Col1, down) :-
	Lig < NL - 1,
	Lig1 is Lig + 1,
	Col1 is Col-1.

compute_next(NL, _NC, Lig, Col, down, Lig, Col1, up) :-
	Lig is NL - 1,
	Col1 is Col+1.

compute_next(_NL, NC, Lig, Col, up, Lig1, Col1, up) :-
	Col < NC - 1,
	Lig1 is Lig - 1,
	Col1 is Col+1.

compute_next(_NL, NC, Lig, Col, up, Lig1, Col, down) :-
	Col is NC - 1,
	Lig1 is Lig + 1.


print_line(L) :-
	maplist(print_val, L),
	nl.

print_val(V) :-
	writef('%3r ', [V]).

```

{{out}}

```txt
?- zig_zag(5).
  0   1   5   6  14
  2   4   7  13  15
  3   8  12  16  21
  9  11  17  20  22
 10  18  19  23  24
true .

?- zig_zag(5, 7).
  0   1   5   6  14  15  24
  2   4   7  13  16  23  25
  3   8  12  17  22  26  31
  9  11  18  21  27  30  32
 10  19  20  28  29  33  34
true .

?- zig_zag(7,5).
  0   1   5   6  14
  2   4   7  13  15
  3   8  12  16  24
  9  11  17  23  25
 10  18  22  26  31
 19  21  27  30  32
 20  28  29  33  34
true .

```



## PureBasic

{{trans|AutoHotkey}}


```purebasic
Procedure zigZag(size)
  Protected i, v, x, y

  Dim a(size - 1, size - 1)

  x = 1
  y = 1
  For i = 1 To  size * size  ;loop once for each element
    a(x - 1, y - 1) = v      ;assign the next index

    If (x + y) & 1 = 0       ;even diagonal (zero based count)
      If x < size            ;while inside the square
        If y > 1             ;move right-up
          y - 1
        EndIf
        x + 1
      Else
        y + 1                ;on the edge increment y, but not x until diagonal is odd
      EndIf
    Else                     ;odd diagonal (zero based count)
      If y < size            ;while inside the square
        If x > 1             ;move left-down
          x - 1
        EndIf
        y + 1
      Else
        x + 1                ;on the edge increment x, but not y until diagonal is even
      EndIf
    EndIf
    v + 1
  Next


  ;generate and show printout
  PrintN("Zig-zag matrix of size " + Str(size) + #CRLF$)
  maxDigitCount = Len(Str(size * size)) + 1
  For y = 0 To size - 1
    For x = 0 To size - 1
      Print(RSet(Str(a(x, y)), maxDigitCount, " "))
    Next
    PrintN("")
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  zigZag(5)
  zigZag(6)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
Zig-zag matrix of size 5

  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

Zig-zag matrix of size 6

  0  1  5  6 14 15
  2  4  7 13 16 25
  3  8 12 17 24 26
  9 11 18 23 27 32
 10 19 22 28 31 33
 20 21 29 30 34 35

```



## Python


### Python: By sorting indices

There is a full explanation of the algorithm used
by [http://paddy3118.blogspot.com/2008/08/zig-zag.html paddy3118].
{{Works with|Python|3}}

```python
def zigzag(n):
    '''zigzag rows'''
    def compare(xy):
        x, y = xy
        return (x + y, -y if (x + y) % 2 else y)
    xs = range(n)
    return {index: n for n, index in enumerate(sorted(
        ((x, y) for x in xs for y in xs),
        key=compare
    ))}


def printzz(myarray):
    '''show zigzag rows as lines'''
    n = int(len(myarray) ** 0.5 + 0.5)
    xs = range(n)
    print('\n'.join(
        [''.join("%3i" % myarray[(x, y)] for x in xs) for y in xs]
    ))


printzz(zigzag(6))
```

{{out}}

```txt
  0  2  3  9 10 20
  1  4  8 11 19 21
  5  7 12 18 22 29
  6 13 17 23 28 30
 14 16 24 27 31 34
 15 25 26 32 33 35
```


===Alternative version, {{trans|Common Lisp}}===

```python

# pylint: disable=invalid-name
# pylint: disable=unused-argument
"ZigZag iterator."
import sys

if sys.version_info[0] >= 3:
    xrange = range

def move(x, y, columns, rows):
    "Tells us what to do next with x and y."
    if y < (rows - 1):
        return max(0, x-1), y+1
    return x+1, y

def zigzag(rows, columns):
    "ZigZag iterator, yields indices."
    x, y = 0, 0
    size = rows * columns
    for _ in xrange(size):
        yield y, x
        if (x + y) & 1:
            x, y = move(x, y, columns, rows)
        else:
            y, x = move(y, x, rows, columns)

# test code
i, rows, cols = 0, 5, 5
mat = [[0 for x in range(cols)] for y in range(rows)]
for (y, x) in zigzag(rows, cols):
    mat[y][x], i = i, i + 1

from pprint import pprint
pprint(mat)

```

{{out}}


```python
[[0, 1, 5, 6, 14],
 [2, 4, 7, 13, 15],
 [3, 8, 12, 16, 21],
 [9, 11, 17, 20, 22],
 [10, 18, 19, 23, 24]]
```


===Alternative version, inspired by the Common Lisp Alternative Approach===

```python

COLS = 9
def CX(x, ran):
  while True:
    x += 2 * next(ran)
    yield x
    x += 1
    yield x
ran = []
d = -1
for V in CX(1,iter(list(range(0,COLS,2)) + list(range(COLS-1-COLS%2,0,-2)))):
  ran.append(iter(range(V, V+COLS*d, d)))
  d *= -1
for x in range(0,COLS):
  for y in range(x, x+COLS):
    print(repr(next(ran[y])).rjust(3), end = ' ')
  print()

```

{{out}} COLS = 5 Produces:

```txt

  1   2   6   7  15
  3   5   8  14  16
  4   9  13  17  22
 10  12  18  21  23
 11  19  20  24  25

```

{{out}} COLS = 8 Produces:

```txt

  1   2   6   7  15  16  28  29
  3   5   8  14  17  27  30  43
  4   9  13  18  26  31  42  44
 10  12  19  25  32  41  45  54
 11  20  24  33  40  46  53  55
 21  23  34  39  47  52  56  61
 22  35  38  48  51  57  60  62
 36  37  49  50  58  59  63  64

```

{{out}} COLS = 9 Produces:

```txt

  1   2   6   7  15  16  28  29  45
  3   5   8  14  17  27  30  44  46
  4   9  13  18  26  31  43  47  60
 10  12  19  25  32  42  48  59  61
 11  20  24  33  41  49  58  62  71
 21  23  34  40  50  57  63  70  72
 22  35  39  51  56  64  69  73  78
 36  38  52  55  65  68  74  77  79
 37  53  54  66  67  75  76  80  81

```


###  Another alternative version


```python

from __future__ import print_function

import math


def zigzag( dimension):
    ''' generate the zigzag indexes for a square array
        Exploiting the fact that an array is symmetrical around its
        centre
    '''
    NUMBER_INDEXES = dimension ** 2
    HALFWAY = NUMBER_INDEXES // 2
    KERNEL_ODD = dimension & 1

    xy = [0 for _ in range(NUMBER_INDEXES)]
    # start at 0,0
    ix = 0
    iy = 0
    # 'fake' that we are going up and right
    direction = 1
    # the first index is always 0, so start with the second
    # until halfway
    for i in range(1, HALFWAY + KERNEL_ODD):
        if direction > 0:
            # going up and right
            if iy == 0:
                # are at top
                ix += 1
                direction = -1
            else:
                ix += 1
                iy -= 1
        else:
            # going down and left
            if ix == 0:
                # are at left
                iy += 1
                direction = 1
            else:
                ix -= 1
                iy += 1
        # update the index position
        xy[iy * dimension + ix] = i

    # have first half, but they are scattered over the list
    # so find the zeros to replace
    for i in range(1, NUMBER_INDEXES):
        if xy[i] == 0 :
            xy[i] = NUMBER_INDEXES - 1 - xy[NUMBER_INDEXES - 1 - i]

    return xy


def main(dim):
    zz = zigzag(dim)
    print( 'zigzag of {}:'.format(dim))
    width = int(math.ceil(math.log10(dim**2)))
    for j in range(dim):
        for i in range(dim):
            print('{:{width}}'.format(zz[j * dim + i], width=width), end=' ')
        print()


if __name__ == '__main__':
    main(5)

```


```txt

zigzag of 5:
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

```



## Rascal

{{incorrect|Rascal|Output is striped rather than zig-zag
i.e. your numbers always increase  going diagonally down and to the left when it should alternativly increase/decrease.}}
This is a translation of the [[Zig-zag_matrix#Python|Python]] example.
As explained on the [[Talk:Zig-zag_matrix#anti-diagonals|Talk]] page,
the key way to understand a zig-zag matrix is to write down
an example with coordinates:

```rascal
0 (0,0), 1 (0,1), 3 (0,2)
2 (1,0), 4 (1,1), 6 (1,2)
5 (2,0), 7 (2,1), 8 (2,2)
```

If you order these coordinates on the number, you create the order:

```rascal
 0 (0,0), 1 (0,1), 2 (1,0), 3 (0,2), 4 (1,1), 5 (2,0), 6 (1,2), 7 (2,1), 8 (2,2)
```

One can observe that this increases with the sum of the coordinates,
and secondly with the the first number of the coordinates.
The Rascal example uses this phenomenon:

```rascal
import util::Math;
import List;
import Set;
import IO;

alias cd = tuple[int,int];

public rel[cd, int] zz(int n){
	 indexorder = sort([<x,y>| x <- [0..n], y <- [0..n]],
	 bool (cd a, cd b){
	 	if (a[0]+a[1] > b[0]+b[1])
	 		return false;
	 	elseif(a[0] < b[0])
	 		return false;
	 	else
	 		return true;
	 		;
	 	});
	 return {<indexorder[z] , z> | z <- index(indexorder)};
}

public void printzz(rel[cd, int] myarray){
    n = floor(sqrt(size(myarray)));
    for (x <- [0..n-1]){
        for (y <- [0..n-1]){
                print("<myarray[<y,x>]>\t");}
        println();}
}
```

{{out}}

```rascal>rascal
printzz(zz(4))
{0}	{1}	{3}	{6}	{10}
{2}	{4}	{7}	{11}	{15}
{5}	{8}	{12}	{16}	{19}
{9}	{13}	{17}	{20}	{22}
{14}	{18}	{21}	{23}	{24}
ok
```



## Qi


This is a purely functional, very inefficient, and straight forward solution.
The code can probably be simplified somewhat.


```qi

(define odd? A -> (= 1 (MOD A 2)))
(define even? A -> (= 0 (MOD A 2)))

(define zigzag-val
  0 0 N -> 0

  X 0 N -> (1+ (zigzag-val (1- X) 0 N)) where (odd? X)
  X 0 N -> (1+ (zigzag-val (1- X) 1 N))

  0 Y N -> (1+ (zigzag-val 1 (1- Y) N)) where (odd? Y)
  0 Y N -> (1+ (zigzag-val 0 (1- Y) N))

  X Y N -> (1+ (zigzag-val (MAX 0 (1- X)) (MIN (1- N) (1+ Y)) N)) where (even? (+ X Y))
  X Y N -> (1+ (zigzag-val (MIN (1- N) (1+ X)) (MAX 0 (1- Y)) N)))

(define range
  E E -> []
  S E -> [S|(range (1+ S) E)])

(define zigzag
  N -> (map (/. Y
                (map (/. X
                         (zigzag-val X Y N))
                     (range 0 N)))
            (range 0 N)))

```



## R

{{trans|Java}}

```R
zigzag <- function(size)
{
   digits <- seq_len(size^2) - 1
   mat <- matrix(0, nrow = size, ncol=size)
   i <- 1
   j <- 1
   for(element in digits)
   {
      mat[i,j] <- element
      if((i + j) %% 2 == 0)
      {
         # Even stripes
         if(j < size) j <- j + 1 else i <- i + 2
         if(i > 1) i <- i - 1
      } else
      {
         # Odd stripes
         if(i < size) i <- i + 1 else j <- j + 2
         if(j > 1) j <- j - 1
      }
   }
   mat
}

zigzag(5)
```



## Racket


```racket

#lang racket

(define/match (compare i j)
  [((list x y) (list a b)) (or (< x a) (and (= x a) (< y b)))])

(define/match (key i)
  [((list x y)) (list (+ x y) (if (even? (+ x y)) (- y) y))])

(define (zigzag-ht n)
  (define indexorder
    (sort (for*/list ([x n] [y n]) (list x y))
          compare #:key key))
  (for/hash ([(n i) (in-indexed indexorder)]) (values n i)))

(define (zigzag n)
  (define ht (zigzag-ht n))
  (for/list ([x n])
    (for/list ([y n])
      (hash-ref ht (list x y)))))

(zigzag 4)

```

{{out}}

```racket

'((0 2 3 9)
  (1 4 8 10)
  (5 7 11 14)
  (6 12 13 15))

```



## REXX

This REXX version allows the optional specification of the   '''start'''   and   '''increment'''   values.

```rexx
/*REXX program  produces and displays a    zig─zag  matrix   (a square array).          */
parse arg n start inc .                          /*obtain optional arguments from the CL*/
if     n=='' |     n==","  then     n=5          /*Not specified?  Then use the default.*/
if start=='' | start==","  then start=0          /* "      "         "   "   "     "    */
if   inc=='' |   inc==","  then   inc=1          /* "      "         "   "   "     "    */
row=1;     col=1                                 /*start with the  1st row,  1st column.*/
size=n**2                                        /*the size of array.                   */
           do j=start  by inc  for size;    @.row.col=j
           if (row+col)//2==0  then do;  if col<n    then col=col+1;  else row=row+2
                                         if row\==1  then row=row-1
                                    end
                               else do;  if row<n    then row=row+1;  else col=col+2
                                         if col\==1  then col=col-1
                                    end
           end   /*j*/                           /* [↑]     //    is REXX  ÷  remainder.*/

w=max(length(start), length(start + size*inc) )  /*maximum width of any matrix element. */

  do      r=1  for n  ;   _=  right(@.r.1, w)    /*show all the rows of the matrix.     */
       do c=2  for n-1;   _=_ right(@.r.c, w)    /*build a line for the output for a row*/
       end   /*c*/                               /* [↑]  matrix elements are aligned.   */
  say _                                          /*show the matrix row just constructed.*/
  end        /*r*/                               /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input of:   <tt> 5 </tt>

```txt

 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

```



## Ring


```ring

# Project  Zig-zag matrix

load "guilib.ring"
load "stdlib.ring"
new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("Zig-zag matrix")
                  setgeometry(100,100,600,400)
                  n = 5
                  a = newlist(n,n)
                  zigzag = newlist(n,n)
                  for j = 1 to n
                       for i = 1 to n
                            a[j][i] = 0
                       next
                  next
                  i = 1
                  j = 1
                  k = 1
                  while k < n * n
                          a[j][i] = k
                          k = k + 1
                          if i = n
                             j = j + 1
                             a[j][i] = k
                             k = k + 1
                             di = -1
                             dj = 1
                          ok
                          if j = 1
                             i = i + 1
                             a[j][i] = k
                             k = k + 1
                             di = -1
                             dj = 1
                          ok
                          if j = n
                             i = i + 1
                             a[j][i] = k
                             k = k + 1
                             di = 1
                             dj = -1
                          ok
                          if i = 1
                             j = j + 1
                             a[j][i] = k
                             k = k + 1
                             di = 1
                             dj = -1
                          ok
                          i = i + di
                          j = j + dj
                  end
                  for p = 1 to n
                       for m = 1 to n
                            zigzag[p][m] = new qpushbutton(win1) {
                                                  x = 150+m*40
                                                  y = 30 + p*40
                                                  setgeometry(x,y,40,40)
                                                  settext(string(a[p][m]))
                                                  }
                       next
                  next
        show()
        }
        exec()
        }

```

Output:

[http://kepkezelo.com/images/kk86ng7p4gcl7z3p7vo1.jpg Zig-Zag matrix]


## Ruby

{{trans|Python}}

```ruby
def zigzag(n)
  (seq=*0...n).product(seq)
    .sort_by {|x,y| [x+y, (x+y).even? ? y : -y]}
    .each_with_index.sort.map(&:last).each_slice(n).to_a
end

def print_matrix(m)
  format = "%#{m.flatten.max.to_s.size}d " * m[0].size
  puts m.map {|row| format % row}
end

print_matrix zigzag(5)
```

{{out}}

```txt

 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24

```



## Scala

Uses the array indices sort solution used by others here.


```scala
  def zigzag(n: Int): Array[Array[Int]] = {
    val l = for (i <- 0 until n*n) yield (i%n, i/n)
    val lSorted = l.sortWith {
      case ((x,y), (u,v)) =>
        if (x+y == u+v)
          if ((x+y) % 2 == 0) x<u else y<v
        else x+y < u+v
    }
    val res = Array.ofDim[Int](n, n)
    lSorted.zipWithIndex foreach {
      case ((x,y), i) => res(y)(x) = i
    }
    res
  }

  zigzag(5).foreach{
    ar => ar.foreach(x => print("%3d".format(x)))
    println
  }
```

Output:

```scala
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```



## Scilab

{{trans|Octave}}

<lang>function a = zigzag3(n)
  a = zeros(n, n)
  for k=1:n
    j = modulo(k, 2)
    d = (2*j-1)*(n-1)
    m = (n-1)*(k-1)
    a(k+(1-j)*m:d:k+j*m) = k*(k-1)/2:k*(k+1)/2-1
    a(n*(n+1-k)+(1-j)*m:d:n*(n+1-k)+j*m) = n*n-k*(k+1)/2:n*n-k*(k-1)/2-1
  end
endfunction

-->zigzag3(5)
 ans  =

    0.     1.     5.     6.     14.
    2.     4.     7.     13.    15.
    3.     8.     12.    16.    21.
    9.     11.    17.    20.    22.
    10.    18.    19.    23.    24.
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: matrix is array array integer;

const func matrix: zigzag (in integer: size) is func
  result
    var matrix: s is matrix.value;
  local
    var integer: i is 1;
    var integer: j is 1;
    var integer: d is -1;
    var integer: max is 0;
    var integer: n is 0;
  begin
    s := size times size times 0;
    max := size ** 2;
    for n range 1 to max div 2 + 1 do
      s[i][j] := n;
      s[size - i + 1][size - j + 1] := max - n + 1;
      i +:= d;
      j -:= d;
      if i < 1 then
        incr(i);
        d := -d;
      elsif j < 1 then
        incr(j);
        d := -d;
      end if;
    end for;
  end func;

const proc: main is func
  local
    var matrix: s is matrix.value;
    var integer: i is 0;
    var integer: num is 0;
  begin
    s := zigzag(7);
    for i range 1 to length(s) do
      for num range s[i] do
        write(num lpad 4);
      end for;
      writeln;
    end for;
  end func;
```


{{out}}

```txt

   1   2   6   7  15  16  28
   3   5   8  14  17  27  29
   4   9  13  18  26  30  39
  10  12  19  25  31  38  40
  11  20  24  32  37  41  46
  21  23  33  36  42  45  47
  22  34  35  43  44  48  49

```



## Sidef

{{trans|Perl}}

```ruby
func zig_zag(w, h) {

    var r = []
    var n = 0

    h.of { |e|
        w.of { |f|
            [e, f]
        }
    }.reduce('+').sort { |a, b|
           (a[0]+a[1] <=> b[0]+b[1]) ||
           (a[0]+a[1] -> is_even ? a[0]<=>b[0]
                                 : a[1]<=>b[1])
    }.each { |a|
       r[a[1]][a[0]] = n++
    }

    return r
}

zig_zag(5, 5).each { say .join('', {|i| "%4i" % i}) }
```

{{out}}

```txt

   0   1   5   6  14
   2   4   7  13  15
   3   8  12  16  21
   9  11  17  20  22
  10  18  19  23  24

```



## Stata

The requested zig-zag matrix can be constructed as a correction of another zig-zag matrix, which is a square "view" of the infinite zig-zag matrix. Here is the latter:


```stata
function zigzag1(n) {
	j = 0::n-1
	u = J(1, n, (-1, 1))
	v = (j:*(2:*j:+3))
	v = rowshape((v,v:+1), 1)
	a = J(n, n, .)
	for (i=1; i<=n; i++) {
		a[i, .] = v[j:+i]
		v = v+u
	}
	return(a)
}

zigzag1(5)
        1    2    3    4    5
    +--------------------------+
  1 |   0    1    5    6   14  |
  2 |   2    4    7   13   16  |
  3 |   3    8   12   17   25  |
  4 |   9   11   18   24   31  |
  5 |  10   19   23   32   40  |
    +--------------------------+
```


Now the corrected matrix, which solves the task:


```stata
function zigzag2(n) {
	a = zigzag1(n)
	v = (1..n-1):^2
	for (i=1; i<n; i++) {
		a[n-i+1, i+1..n] = a[n-i+1, i+1..n] - v[1..n-i]
	}
	return(a)
}

zigzag2(5)
        1    2    3    4    5
    +--------------------------+
  1 |   0    1    5    6   14  |
  2 |   2    4    7   13   15  |
  3 |   3    8   12   16   21  |
  4 |   9   11   17   20   22  |
  5 |  10   18   19   23   24  |
    +--------------------------+
```


The correction is given by the difference:


```stata
zigzag1(5)-zigzag2(5)
[symmetric]
        1    2    3    4    5
    +--------------------------+
  1 |   0                      |
  2 |   0    0                 |
  3 |   0    0    0            |
  4 |   0    0    1    4       |
  5 |   0    1    4    9   16  |
    +--------------------------+
```



## Tcl

Using <code>print_matrix</code> from [[Matrix Transpose#Tcl|Matrix Transpose]]…

```tcl
proc zigzag {size} {
    set m [lrepeat $size [lrepeat $size .]]
    set x 0; set dx -1
    set y 0; set dy 1

    for {set i 0} {$i < $size ** 2} {incr i} {
        if {$x >= $size} {
            incr x -1
            incr y 2
            negate dx dy
        } elseif {$y >= $size} {
            incr x 2
            incr y -1
            negate dx dy
        } elseif {$x < 0 && $y >= 0} {
            incr x
            negate dx dy
        } elseif {$x >= 0 && $y < 0} {
            incr y
            negate dx dy
        }
        lset m $x $y $i
        incr x $dx
        incr y $dy
    }
    return $m
}

proc negate {args} {
    foreach varname $args {
        upvar 1 $varname var
        set var [expr {-1 * $var}]
    }
}

print_matrix [zigzag 5]
```

{{out}}

```txt
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
```



## uBasic/4tH

{{trans|BBC BASIC}}
<lang>S = 5

i = 1
j = 1

For e = 0 To (S*S)-1
  @((i-1) * S + (j-1)) = e

  If (i + j) % 2 = 0 Then

    If j < S Then
      j = j + 1
    Else
      i = i + 2
    EndIf

    If i > 1 Then
      i = i - 1
    EndIf
  Else

    If i < S
      i = i + 1
    Else
      j = j + 2
    EndIf

    If j > 1
      j = j - 1
    EndIf
  EndIf
Next

For r = 0 To S-1
  For c = 0 To S-1
    Print Using "___#";@(r * S + c);
  Next
  Print
Next
```

{{out}}

```txt
   0   1   5   6  14
   2   4   7  13  15
   3   8  12  16  21
   9  11  17  20  22
  10  18  19  23  24

0 OK, 0:428
```



## Ursala

adapted from the J solution

```Ursala
#import std
#import nat

zigzag = ~&mlPK2xnSS+ num+ ==+sum~~|=xK9xSL@iiK0+ iota
```

test program (three examples):

```Ursala
#cast %nLLL

tests = zigzag* <4,5,6>
```

{{out}}

```txt
<
   <
      <0,1,5,6>,
      <2,4,7,12>,
      <3,8,11,13>,
      <9,10,14,15>>,
   <
      <0,1,5,6,14>,
      <2,4,7,13,15>,
      <3,8,12,16,21>,
      <9,11,17,20,22>,
      <10,18,19,23,24>>,
   <
      <0,1,5,6,14,15>,
      <2,4,7,13,16,25>,
      <3,8,12,17,24,26>,
      <9,11,18,23,27,32>,
      <10,19,22,28,31,33>,
      <20,21,29,30,34,35>>>
```



## VBA


```VBA

Public Sub zigzag(n)
Dim a() As Integer
'populate a (1,1) to a(n,n) in zigzag pattern

'check if n too small
If n < 1 Then
  Debug.Print "zigzag: enter a number greater than 1"
  Exit Sub
End If

'initialize
ReDim a(1 To n, 1 To n)
i = 1       'i is the row
j = 1       'j is the column
P = 0       'P is the next number
a(i, j) = P 'fill in initial value

'now zigzag through the matrix and fill it in
Do While (i <= n) And (j <= n)
  'move one position to the right or down the rightmost column, if possible
  If j < n Then
    j = j + 1
  ElseIf i < n Then
    i = i + 1
  Else
    Exit Do
  End If
  'fill in
  P = P + 1: a(i, j) = P
  'move down to the left
  While (j > 1) And (i < n)
    i = i + 1: j = j - 1
    P = P + 1: a(i, j) = P
  Wend
  'move one position down or to the right in the bottom row, if possible
  If i < n Then
    i = i + 1
  ElseIf j < n Then
    j = j + 1
  Else
    Exit Do
  End If
  P = P + 1: a(i, j) = P
  'move back up to the right
  While (i > 1) And (j < n)
    i = i - 1: j = j + 1
    P = P + 1: a(i, j) = P
  Wend
Loop

'print result
Debug.Print "Result for n="; n; ":"
For i = 1 To n
  For j = 1 To n
    Debug.Print a(i, j),
  Next
  Debug.Print
Next
End Sub

```


{{out}}

```txt

zigzag 5
Result for n= 5 :
 0             1             5             6             14
 2             4             7             13            15
 3             8             12            16            21
 9             11            17            20            22
 10            18            19            23            24

zigzag 6
Result for n= 6 :
 0             1             5             6             14            15
 2             4             7             13            16            25
 3             8             12            17            24            26
 9             11            18            23            27            32
 10            19            22            28            31            33
 20            21            29            30            34            35

```



## VBScript

{{trans|BBC BASIC}}

```vb
ZigZag(Cint(WScript.Arguments(0)))

Function ZigZag(n)
	Dim arrZ()
	ReDim arrZ(n-1,n-1)
	i = 1
	j = 1
	For e = 0 To (n^2) - 1
		arrZ(i-1,j-1) = e
		If ((i + j ) And 1) = 0 Then
			If j < n Then
				j = j + 1
			Else
				i = i + 2
			End If
			If i > 1 Then
				i = i - 1
			End If
		Else
			If i < n Then
				i = i + 1
			Else
				j = j + 2
			End If
			If j > 1 Then
				j = j - 1
			End If
		End If
	Next
	For k = 0 To n-1
		For l = 0 To n-1
			WScript.StdOut.Write Right("  " & arrZ(k,l),3)
		Next
		WScript.StdOut.WriteLine
	Next
End Function
```


{{out}}

```txt

C:\>cscript /nologo ZigZag.vbs 5
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

C:\>cscript /nologo ZigZag.vbs 7
  0  1  5  6 14 15 27
  2  4  7 13 16 26 28
  3  8 12 17 25 29 38
  9 11 18 24 30 37 39
 10 19 23 31 36 40 45
 20 22 32 35 41 44 46
 21 33 34 42 43 47 48

```



## XPL0


```XPL0
include c:\cxpl\codes;
def N=6;
int A(N,N), X, Y, I, D;
[I:=0; X:=0; Y:=0; D:=1;
repeat  A(X,Y):=I;
        case of
         X+D>=N: [D:=-D; Y:=Y+1];
         Y-D>=N: [D:=-D; X:=X+1];
         X+D<0:  [D:=-D; Y:=Y+1];
         Y-D<0:  [D:=-D; X:=X+1]
        other    [X:=X+D; Y:=Y-D];
        I:=I+1;
until   I>=N*N;
for Y:=0 to N-1 do
    [for X:=0 to N-1 do
        [I:=A(X,Y);
        ChOut(0,^ );
        if I<10 then ChOut(0,^ );
        IntOut(0, I);
        ];
    CrLf(0);
    ];
]
```


{{out}}

```txt

  0  1  5  6 14 15
  2  4  7 13 16 25
  3  8 12 17 24 26
  9 11 18 23 27 32
 10 19 22 28 31 33
 20 21 29 30 34 35

```



## Yabasic


```Yabasic
Size = 5
DIM array(Size-1, Size-1)

i = 1
j = 1
FOR e = 0 TO Size^2-1
  array(i-1, j-1) = e
  IF and((i + j), 1) = 0 THEN
    IF j < Size then j = j + 1 ELSE i = i + 2 end if
    IF i > 1 i = i - 1
  ELSE
    IF i < Size then i = i + 1 ELSE j = j + 2 end if
    IF j > 1 j = j - 1
  ENDIF
NEXT e

FOR row = 0 TO Size-1
  FOR col = 0 TO Size-1
    PRINT array(row,col) USING "##";
  NEXT col
  PRINT
NEXT row
```



## zkl

{{trans|Groovy}}

```zkl
fcn zz(n){
   grid := (0).pump(n,List, (0).pump(n,List).copy).copy();
   ri := Ref(0);
   foreach d in ([1..n*2]){
      x:=(0).max(d - n); y:=(n - 1).min(d - 1);
      (0).pump(d.min(n*2 - d),Void,'wrap(it){
         grid[if(d%2)y-it else x+it][if(d%2)x+it else y-it] = ri.inc();
      });
   }
   grid.pump(String,'wrap(r){("%3s"*n+"\n").fmt(r.xplode())});
}
```

{{trans|C}}
Using list comprehension (and side effects) for the double loop,
the resulting list is just thrown away,
which is easier than creating an enumerated list and sorting.

```zkl
fcn ceg(m){
   s  := (0).pump(m*m,List).copy(); // copy to make writable
   rn := Ref(0);
   [[(i,j); [0..m*2-1]; '{[(0).max(i-m+1) .. i.min(m-1)]};
         '{ s[ if(i.isOdd) j*(m-1)+i else (i-j)*m+j ] = rn.inc(); }]];
   s.pump(String,T(Void.Read,m-1), ("%3s"*m+"\n").fmt);
}
```

To be pedantic, the same as above,
but using the output of the list comprehension:

```zkl
fcn ceg2(m){
   rn := Ref(0);
   [[(i,j); [0..m*2-1]; '{[(0).max(i-m+1) .. i.min(m-1)]};
         '{ T( if(i.isOdd) j*(m-1)+i else (i-j)*m+j;, rn.inc() ) }]]
   .sort(fcn([(a,_)], [(b,_)]){ a<b }).apply("get",1)
   .pump(String,T(Void.Read,m-1), ("%3s"*m+"\n").fmt);
}
```

{{out}} The results are the same:

```txt

zz(5).println();
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24

```


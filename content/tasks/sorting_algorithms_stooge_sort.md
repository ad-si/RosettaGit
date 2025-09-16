+++
title = "Sorting algorithms/Stooge sort"
description = ""
date = 2019-03-22T11:29:52Z
aliases = []
[extra]
id = 7801
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "basic",
  "bbc_basic",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "eiffel",
  "elena",
  "elixir",
  "euphoria",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "maxscript",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powerbasic",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "smalltalk",
  "swift",
  "tcl",
  "ubasic_4th",
  "xpl0",
  "yorick",
  "zkl",
]
+++

## Task

Show the   [[wp:Stooge sort|Stooge Sort]]   for an array of integers.


The Stooge Sort algorithm is as follows:
 <b>algorithm</b> stoogesort(<b>array</b> L, i = 0, j = <b>length</b>(L)-1)
      <b>if</b> L[j] < L[i] <b>then</b>
          L[i] <b>↔</b> L[j]
      <b>if</b> j - i > 1 <b>then</b>
          t <b>:=</b> (j - i + 1)/3
          stoogesort(L, i  , j-t)
          stoogesort(L, i+t, j  )
          stoogesort(L, i  , j-t)
      <b>return</b> L





## Ada


Using slices and 'First / 'Last removes the need for I / J parameters.


```Ada
with Ada.Text_IO;
procedure Stooge is
   type Integer_Array is array (Positive range <>) of Integer;
   procedure Swap (Left, Right : in out Integer) is
      Temp : Integer := Left;
   begin
      Left  := Right;
      Right := Temp;
   end Swap;
   procedure Stooge_Sort (List : in out Integer_Array) is
      T : Natural := List'Length / 3;
   begin
      if List (List'Last) < List (List'First) then
         Swap (List (List'Last), List (List'First));
      end if;
      if List'Length > 2 then
         Stooge_Sort (List (List'First     .. List'Last - T));
         Stooge_Sort (List (List'First + T .. List'Last));
         Stooge_Sort (List (List'First     .. List'Last - T));
      end if;
   end Stooge_Sort;
   Test_Array : Integer_Array := (1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7);
begin
   Stooge_Sort (Test_Array);
   for I in Test_Array'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_Array (I)));
      if I /= Test_Array'Last then
         Ada.Text_IO.Put (", ");
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end Stooge;
```


```txt
-6, -5, -3, -2,  1,  3,  3,  4,  5,  5,  7,  7,  7,  9,  10
```



## ALGOL 68

```algol68
# swaps the values of the two REF INTs #
PRIO =:= = 1;
OP   =:= = ( REF INT a, b )VOID: ( INT t := a; a := b; b := t );

# returns the array of INTs sorted via the stooge sort algorithm #
PROC stooge sort = ( []INT array )[]INT:
     BEGIN
         PROC stooge sort segment = ( REF[]INT l, INT i, j )VOID:
             BEGIN
                 IF l[j] < l[i] THEN l[ i ] =:= l[ j ] FI;
                 IF j - i > 1
                 THEN
                     INT t := (j - i + 1) OVER 3;
                     stooge sort segment( l, i,     j - t );
                     stooge sort segment( l, i + t, j     );
                     stooge sort segment( l, i,     j - t )
                 FI
             END # stooge sort segment # ;

         [ LWB array : UPB array ]INT result := array;
         stooge sort segment( result, LWB result, UPB result );
         result
     END # stooge sort # ;

# test the stooge sort #
[]INT data = ( 67, -201, 0, 9, 9, 231, 4 );
print( ( "before: ", data, newline, "after:  ", stooge sort( data ), newline ) )
```

```txt

before:         +67       -201         +0         +9         +9       +231         +4
after:         -201         +0         +4         +9         +9        +67       +231

```



## AutoHotkey


```AutoHotkey
StoogeSort(L, i:=1, j:=""){
	if !j
		j := L.MaxIndex()
	if (L[j] < L[i]){
		temp := L[i]
		L[i] := L[j]
		L[j] := temp
	}
	if (j - i > 1){
		t := floor((j - i + 1)/3)
		StoogeSort(L, i, j-t)
		StoogeSort(L, i+t, j)
		StoogeSort(L, i, j-t)
	}
	return L
}
```

Examples:
```AutoHotkey
MsgBox % map(StoogeSort([123,51,6,73,3,-12,0]))
return

map(obj){
	for k, v in obj
		res .= v ","
	return trim(res, ",")
}
```

Outputs:
```txt
-12,0,3,6,51,73,123
```



## BASIC

This ''might'' work with older versions of QB, but that is untested.
It ''definitely'' does '''not''' work with QBasic.


```qbasic
DECLARE SUB stoogesort (L() AS LONG, i AS LONG, j AS LONG)

RANDOMIZE TIMER

CONST arraysize = 10

DIM x(arraysize) AS LONG
DIM i AS LONG

PRINT "Before: ";
FOR i = 0 TO arraysize
    x(i) = INT(RND * 100)
    PRINT x(i); " ";
NEXT
PRINT

stoogesort x(), 0, arraysize

PRINT "After: ";
FOR i = 0 TO arraysize
    PRINT x(i); " ";
NEXT
PRINT

SUB stoogesort (L() AS LONG, i AS LONG, j AS LONG)
    IF L(j) < L(i) THEN SWAP L(i), L(j)
    IF (j - i) > 1 THEN
        DIM t AS LONG
        t = (j - i + 1) / 3
        stoogesort L(), i, j - t
        stoogesort L(), i + t, j
        stoogesort L(), i, j - t
    END IF
END SUB
```


 Before:  15   42   21   50   33   65   40   43   20   25   19
 After:  15   19   20   21   33   25   40   42   43   50   65
 Before:  99   21   84   55   32   26   86   27   8   45   11
 After:  8   11   21   26   27   32   45   55   84   86   99
 Before:  11   50   11   37   97   94   92   70   92   57   88
 After:  11   11   37   50   57   70   88   92   92   94   97


## BBC BASIC


```bbcbasic
      DIM test%(9)
      test%() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCstoogesort(test%(), 0, DIM(test%(),1))
      FOR i% = 0 TO 9
        PRINT test%(i%) ;
      NEXT
      PRINT
      END

      DEF PROCstoogesort(l%(), i%, j%)
      LOCAL t%
      IF l%(j%) < l%(i%) SWAP l%(i%), l%(j%)
      IF j% - i% > 1 THEN
        t% = (j% - i% + 1)/3
        PROCstoogesort(l%(), i%, j%-t%)
        PROCstoogesort(l%(), i%+t%, j%)
        PROCstoogesort(l%(), i%, j%-t%)
      ENDIF
      ENDPROC
```

```txt

       -31         0         1         2         2         4        65        83        99       782

```



## C


```c
#include <stdio.h>

#define SWAP(r,s)  do{ t=r; r=s; s=t; } while(0)

void StoogeSort(int a[], int i, int j)
{
   int t;

   if (a[j] < a[i]) SWAP(a[i], a[j]);
   if (j - i > 1)
   {
       t = (j - i + 1) / 3;
       StoogeSort(a, i, j - t);
       StoogeSort(a, i + t, j);
       StoogeSort(a, i, j - t);
   }
}

int main(int argc, char *argv[])
{
   int nums[] = {1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7};
   int i, n;

   n = sizeof(nums)/sizeof(int);
   StoogeSort(nums, 0, n-1);

   for(i = 0; i <= n-1; i++)
      printf("%5d", nums[i]);

   return 0;
}
```



## C++


```Cpp

#include <iostream>
#include <time.h>

//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
class stooge
{
public:
    void sort( int* arr, int start, int end )
    {
        if( arr[start] > arr[end - 1] ) swap( arr[start], arr[end - 1] );
	int n = end - start; if( n > 2 )
	{
	    n /= 3; sort( arr, start, end - n );
	    sort( arr, start + n, end ); sort( arr, start, end - n );
        }
    }
};
//------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( static_cast<unsigned int>( time( NULL ) ) ); stooge s; int a[80], m = 80;
    cout << "before:\n";
    for( int x = 0; x < m; x++ ) { a[x] = rand() % 40 - 20;  cout << a[x] << " "; }
    s.sort( a, 0, m ); cout << "\n\nafter:\n";
    for( int x = 0; x < m; x++ ) cout << a[x] << " "; cout << "\n\n";
    return system( "pause" );
}

```

```txt

before:
5 -15 11 18 -14 -20 6 -4 -1 -8 12 -18 -12 -4 -10 -8 13 4 0 16 7 -13 -13 -1 11 -9
 13 -14 9 -19 -1 14 6 -4 7 -8 -15 -11 -9 3 10 3 -2 -5 12 -8 -2 10 -10 9 14 9 -12
 19 -16 -6 -13 -18 -3 -13 -12 8 -8 -10 -16 5 8 -10 -10 6 -14 -20 -16 7 15 11 -19
 -18 10 -15

after:
-20 -20 -19 -19 -18 -18 -18 -16 -16 -16 -15 -15 -15 -14 -14 -14 -13 -13 -13 -13
-12 -12 -12 -11 -10 -10 -10 -10 -10 -9 -9 -8 -8 -8 -8 -8 -6 -5 -4 -4 -4 -3 -2 -2
 -1 -1 -1 0 3 3 4 5 5 6 6 6 7 7 7 8 8 9 9 9 10 10 10 11 11 11 12 12 13 13 14 14
15 16 18 19

```


## C#
<lang C sharp|C#>    public static void Sort<T>(List<T> list) where T : IComparable {
        if (list.Count > 1) {
            StoogeSort(list, 0, list.Count - 1);
        }
    }
    private static void StoogeSort<T>(List<T> L, int i, int j) where T : IComparable {
        if (L[j].CompareTo(L[i])<0) {
            T tmp = L[i];
            L[i] = L[j];
            L[j] = tmp;
        }
        if (j - i > 1) {
            int t = (j - i + 1) / 3;
            StoogeSort(L, i, j - t);
            StoogeSort(L, i + t, j);
            StoogeSort(L, i, j - t);
        }
    }
```



## Clojure


```clojure
(defn swap [v x y]
   (assoc! v y (v x) x (v y)))

(defn stooge-sort
  ([v] (persistent! (stooge-sort (transient v) 0 (dec (count v)))))
  ([v i j]
    (if (> (v i) (v j)) (swap v i j) v)
    (if (> (- j i) 1)
      (let [t (int (Math/floor (/ (inc (- j i)) 3)))]
        (stooge-sort v i (- j t))
        (stooge-sort v (+ i t) j)
        (stooge-sort v i (- j t))))
    v))
```



## COBOL

```cobol
       >>
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. stooge-sort-test.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  Arr-Len                             CONSTANT 7.

01  arr-area                            VALUE "00004001000020000005000230000000000".
    03  arr-elt                         PIC 9(5) OCCURS Arr-Len TIMES
                                        INDEXED BY arr-idx.

PROCEDURE DIVISION.
    DISPLAY "Unsorted: " NO ADVANCING
    PERFORM VARYING arr-idx FROM 1 BY 1 UNTIL Arr-Len < arr-idx
        DISPLAY arr-elt (arr-idx) " " NO ADVANCING
    END-PERFORM
    DISPLAY SPACE

    CALL "stooge-sort" USING arr-area, OMITTED, OMITTED

    DISPLAY "Sorted:   " NO ADVANCING
    PERFORM VARYING arr-idx FROM 1 BY 1 UNTIL Arr-Len < arr-idx
        DISPLAY arr-elt (arr-idx) " " NO ADVANCING
    END-PERFORM
    DISPLAY SPACE
    .
END PROGRAM stooge-sort-test.


IDENTIFICATION DIVISION.
PROGRAM-ID. stooge-sort RECURSIVE.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  Arr-Len                             CONSTANT 7.

01  i                                   PIC 99 COMP.
01  j                                   PIC 99 COMP.

01  temp                                PIC 9(5).

01  t                                   PIC 99 COMP.

LINKAGE SECTION.
01  arr-area.
    03  arr-elt                         PIC 9(5) OCCURS Arr-Len TIMES.

01  i-val                               PIC 99 COMP.
01  j-val                               PIC 99 COMP.

PROCEDURE DIVISION USING arr-area, OPTIONAL i-val, OPTIONAL j-val.
    IF i-val IS OMITTED
        MOVE 1 TO i
    ELSE
        MOVE i-val TO i
    END-IF
    IF j-val IS OMITTED
        MOVE Arr-Len TO j
    ELSE
        MOVE j-val TO j
    END-IF

    IF arr-elt (j) < arr-elt (i)
        MOVE arr-elt (i) TO temp
        MOVE arr-elt (j) TO arr-elt (i)
        MOVE temp TO arr-elt (j)
    END-IF

    IF j - i + 1 >= 3
        COMPUTE t = (j - i + 1) / 3
        SUBTRACT t FROM j
        CALL "stooge-sort" USING arr-area, CONTENT i, j
        ADD t TO i, j
        CALL "stooge-sort" USING arr-area, CONTENT i, j
        SUBTRACT t FROM i, j
        CALL "stooge-sort" USING arr-area, CONTENT i, j
    END-IF
    .
END PROGRAM stooge-sort.
```


```txt

Unsorted: 00004 00100 00200 00005 00023 00000 00000
Sorted:   00000 00000 00004 00005 00023 00100 00200

```



## Common Lisp


```lisp
(defun stooge-sort (vector &optional (i 0) (j (1- (length vector))))
  (when (> (aref vector i) (aref vector j))
    (rotatef (aref vector i) (aref vector j)))
  (when (> (- j i) 1)
    (let ((third (floor (1+ (- j i)) 3)))
      (stooge-sort vector i (- j third))
      (stooge-sort vector (+ i third) j)
      (stooge-sort vector i (- j third))))
  vector)
```



## D


```d
import std.stdio, std.algorithm, std.array;

void stoogeSort(T)(T[] seq) pure nothrow {
    if (seq.back < seq.front)
        swap(seq.front, seq.back);

    if (seq.length > 2) {
        immutable m = seq.length / 3;
        seq[0 .. $ - m].stoogeSort();
        seq[m .. $].stoogeSort();
        seq[0 .. $ - m].stoogeSort();
    }
}

void main() {
    auto data = [1, 4, 5, 3, -6, 3, 7, 10, -2, -5];
    data.stoogeSort();
    writeln(data);
}
```

 [-6, -5, -2, 1, 3, 3, 4, 5, 7, 10]


## Eiffel


```Eiffel

class
	STOOGE_SORT
feature
	stoogesort (ar: ARRAY[INTEGER]; i,j: INTEGER)
                -- Sorted array in ascending order.
	require
		ar_not_empty: ar.count >= 0
		i_in_range: i>=1
		j_in_range: j <= ar.count
		boundary_set: i<=j
	local
		t: REAL_64
		third: INTEGER
		swap: INTEGER
	do
		if ar[j]< ar[i] then
			swap:= ar[i]
			ar[i]:=ar[j]
			ar[j]:= swap
		end
		if j-i >1 then
			t:= (j-i+1)/3
			third:= t.floor
			stoogesort(ar, i, j-third)
			stoogesort(ar, i+third, j)
			stoogesort(ar, i, j-third)
		end
	end
end

```

Test:

```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			test := <<2, 5, 66, -2, 0, 7>>
			io.put_string ("%Nunsorted:%N")
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
			create stoogesort
			stoogesort.stoogesort (test, 1, test.count)
			io.put_string ("%Nsorted:%N")
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
		end

	test: ARRAY [INTEGER]

	stoogesort: STOOGE_SORT

end

```

```txt

unsorted:
2 5 66 -2 0 7
sorted:
-2 0 2 5 7 66

```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'routines;

extension op
{
    stoogeSort()
        = self.stoogeSort(0, self.Length - 1);

    stoogeSort(IntNumber i, IntNumber j)
    {
        if(self[j]<self[i])
        {
            self.exchange(i,j)
        };
        if (j - i > 1)
        {
            int t := (j - i + 1) / 3;
            self.stoogeSort(i,j-t);
            self.stoogeSort(i+t,j);
            self.stoogeSort(i,j-t)
        }
    }
}

public program()
{
    var list := new Range(0, 15).selectBy:(n => randomGenerator.eval(20)).toArray();

    console.printLine("before:", list.asEnumerable());
    console.printLine("after:", list.stoogeSort().asEnumerable())
}
```

```txt

before:0,1,18,17,4,13,18,8,2,10,15,17,11,1,17
after:0,1,1,2,4,8,10,11,13,15,17,17,17,18,18

```



## Elixir


```elixir
defmodule Sort do
  def stooge_sort(list) do
    stooge_sort(List.to_tuple(list), 0, length(list)-1) |> Tuple.to_list
  end

  defp stooge_sort(tuple, i, j) do
    if (vj = elem(tuple, j)) < (vi = elem(tuple, i)) do
      tuple = put_elem(tuple,i,vj) |> put_elem(j,vi)
    end
    if j - i > 1 do
      t = div(j - i + 1, 3)
      tuple
      |> stooge_sort(i, j-t)
      |> stooge_sort(i+t, j)
      |> stooge_sort(i, j-t)
    else
      tuple
    end
  end
end

(for _ <- 1..20, do: :rand.uniform(20)) |> IO.inspect
|> Sort.stooge_sort |> IO.inspect
```


```txt

[18, 8, 19, 19, 17, 17, 1, 5, 17, 9, 13, 6, 7, 19, 1, 6, 11, 20, 17, 12]
[1, 1, 5, 6, 6, 7, 8, 9, 11, 12, 13, 17, 17, 17, 17, 18, 19, 19, 19, 20]

```



## Euphoria


```euphoria
function stooge(sequence s, integer i, integer j)
    object temp
    integer t
    if compare(s[j], s[i]) < 0 then
        temp = s[i]
        s[i] = s[j]
        s[j] = temp
    end if
    if j - i > 1 then
        t = floor((j - i + 1)/3)
        s = stooge(s, i  , j-t)
        s = stooge(s, i+t, j  )
        s = stooge(s, i  , j-t)
    end if
    return s
end function

function stoogesort(sequence s)
    return stooge(s,1,length(s))
end function

constant s = rand(repeat(1000,10))

? s
? stoogesort(s)
```


```txt
{875,616,725,922,463,740,949,476,697,455}
{455,463,476,616,697,725,740,875,922,949}

```



## Factor


```factor
USING: kernel locals math prettyprint sequences ;
IN: rosetta-code.stooge-sort

<PRIVATE

:: (stooge-sort) ( seq i j -- )
    j i [ seq nth ] bi@ < [
        j i seq exchange
    ] when
    j i - 1 > [
        j i - 1 + 3 /i :> t
        seq i j t - (stooge-sort)
        seq i t + j (stooge-sort)
        seq i j t - (stooge-sort)
    ] when ;

PRIVATE>

: stooge-sort ( seq -- sortedseq )
    [ clone dup ] [ drop 0 ] [ length 1 - ] tri (stooge-sort) ;

: stooge-sort-demo ( -- )
    { 1 4 5 3 -6 3 7 10 -2 -5 } stooge-sort . ;

MAIN: stooge-sort-demo
```

```txt

{ -6 -5 -2 1 3 3 4 5 7 10 }

```



## Fortran

```fortran
program Stooge
  implicit none

  integer :: i
  integer :: array(50) = (/ (i, i = 50, 1, -1) /) ! Reverse sorted array

  call Stoogesort(array)
  write(*,"(10i5)") array

contains

recursive subroutine Stoogesort(a)
  integer, intent(in out) :: a(:)
  integer :: j, t, temp

   j = size(a)
   if(a(j) < a(1)) then
     temp = a(j)
     a(j) = a(1)
     a(1) = temp
   end if

  if(j > 2) then
    t = j / 3
    call Stoogesort(a(1:j-t))
    call Stoogesort(a(1+t:j))
    call Stoogesort(a(1:j-t))
  end if

end subroutine
end program
```


## FreeBASIC


```freebasic
' version 23-10-2016
' compile with: fbc -s console

Sub stoogesort(s() As Long, l As Long, r As Long)

    If s(r) < s(l) Then
        Swap s(r), s(l)
    End If

    If r - l > 1 Then
        Var t = (r - l +1) \ 3
        stoogesort(s(), l    , r - t)
        stoogesort(s(), l + t, r    )
        stoogesort(s(), l    , r - t)
    End If
End Sub

' ------=< MAIN >=------

Dim As Long i, array(-7 To 7)
Dim As Long a = LBound(array), b = UBound(array)

Randomize Timer
For i = a To b : array(i) = i  : Next
For i = a To b ' little shuffle
    Swap array(i), array(Int(Rnd * (b - a +1)) + a)
Next

Print "unsorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print

stoogesort(array(), LBound(array), UBound(array))

Print "  sorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Unsorted
   0   3  -6   2   1  -4   7   5   6  -3   4  -7  -1  -5  -2

After heapsort
  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## Go


```go
package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}

func main() {
    fmt.Println("before:", a)
    stoogesort(a)
    fmt.Println("after: ", a)
    fmt.Println("nyuk nyuk nyuk")
}

func stoogesort(a []int) {
    last := len(a) - 1
    if a[last] < a[0] {
        a[0], a[last] = a[last], a[0]
    }
    if last > 1 {
        t := len(a) / 3
        stoogesort(a[:len(a)-t])
        stoogesort(a[t:])
        stoogesort(a[:len(a)-t])
    }
}
```



## Haskell



```haskell
import Data.List
import Control.Arrow
import Control.Monad

insertAt e k = uncurry(++).second ((e:).drop 1). splitAt k

swapElems :: [a] -> Int -> Int -> [a]
swapElems xs i j = insertAt (xs!!j) i $ insertAt (xs!!i) j xs

stoogeSort [] = []
stoogeSort [x] = [x]
stoogeSort xs = doss 0 (length xs - 1) xs
doss :: (Ord a) => Int -> Int -> [a] -> [a]
doss i j xs
      | j-i>1 = doss i (j-t) $ doss (i+t) j $ doss i (j-t) xs'
      | otherwise = xs'
    where t = (j-i+1)`div`3
	  xs'
	    | xs!!j < xs!!i = swapElems xs i j
	    | otherwise = xs
```

Example:

```haskell
*Main> stoogeSort [1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7]
[-6,-5,-3,-2,1,3,3,4,5,5,7,7,7,9,10]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()              #: demonstrate various ways to sort a list and string
   demosort(stoogesort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure stoogesort(X,op,i,j)           #: return sorted list ascending(or descending)
local t

   if /i := 0 then {
      j := *X
      op := sortop(op,X)                 # select how and what we sort
      }

   if op(X[j],X[i]) then
      X[i] :=: X[j]
   if j - i > 1 then {
      t := (j - i + 1) / 3
      X := stoogesort(X,op,i,j-t)
      X := stoogesort(X,op,i+t,j)
      X := stoogesort(X,op,i,j-t)
      }
   return X                              # X must be returned and assigned to sort a string
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]].
The full demosort exercises the named sort of a list with
op = "numeric", "string", ">>" (lexically gt, descending),
">" (numerically gt, descending), a custom comparator, and also a string.
{{out}} Abbreviated sample

```txt
Sorting Demo using procedure stoogesort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "StoogSrt.bas"
110 RANDOMIZE
120 NUMERIC ARRAY(5 TO 16)
130 CALL INIT(ARRAY)
140 CALL WRITE(ARRAY)
150 CALL STOOGESORT(ARRAY,LBOUND(ARRAY),UBOUND(ARRAY))
160 CALL WRITE(ARRAY)
170 DEF INIT(REF A)
180   FOR I=LBOUND(A) TO UBOUND(A)
190     LET A(I)=RND(99)+1
200   NEXT
210 END DEF
220 DEF WRITE(REF A)
230   FOR I=LBOUND(A) TO UBOUND(A)
240     PRINT A(I);
250   NEXT
260   PRINT
270 END DEF
280 DEF STOOGESORT(REF A,I,J)
290   NUMERIC T
300   IF A(J)<A(I) THEN LET T=A(J):LET A(J)=A(I):LET A(I)=T
310   IF J-I>1 THEN
320     LET T=IP((J-I+1)/3)
330     CALL STOOGESORT(A,I,J-T)
340     CALL STOOGESORT(A,I+T,J)
350     CALL STOOGESORT(A,I,J-T)
360   END IF
370 END DEF
```



## J


```j
swapElems=: |.@:{`[`]}

stoogeSort=: 3 : 0
  (0,<:#y) stoogeSort y
:
  if. >/x{y do. y=.x swapElems y end.
  if. 1<-~/x do.
    t=. <.3%~1+-~/x
    (x-0,t) stoogeSort (x+t,0) stoogeSort (x-0,t) stoogeSort y
  else. y end.
)
```

Example:

```j
   (,: stoogeSort) ?~13
3 10 8 4 7 12 1 2 11 6  5  9  0
0  1 2 3 4  5 6 7  8 9 10 11 12

```



## Java


```java
import java.util.Arrays;

public class Stooge {
    public static void main(String[] args) {
        int[] nums = {1, 4, 5, 3, -6, 3, 7, 10, -2, -5};
        stoogeSort(nums);
        System.out.println(Arrays.toString(nums));
    }

    public static void stoogeSort(int[] L) {
        stoogeSort(L, 0, L.length - 1);
    }

    public static void stoogeSort(int[] L, int i, int j) {
        if (L[j] < L[i]) {
            int tmp = L[i];
            L[i] = L[j];
            L[j] = tmp;
        }
        if (j - i > 1) {
            int t = (j - i + 1) / 3;
            stoogeSort(L, i, j - t);
            stoogeSort(L, i + t, j);
            stoogeSort(L, i, j - t);
        }
    }
}
```

```txt
[-6, -5, -2, 1, 3, 3, 4, 5, 7, 10]
```



## JavaScript


```javascript
function stoogeSort (array, i, j) {
    if (j === undefined) {
        j = array.length - 1;
    }

    if (i === undefined) {
        i = 0;
    }

    if (array[j] < array[i]) {
        var aux = array[i];
        array[i] = array[j];
        array[j] = aux;
    }

    if (j - i > 1) {
        var t = Math.floor((j - i + 1) / 3);
        stoogeSort(array, i, j-t);
        stoogeSort(array, i+t, j);
        stoogeSort(array, i, j-t);
    }
};
```

Example:

```javascript
arr = [9,1,3,10,13,4,2];
stoogeSort(arr);
console.log(arr);
```

```txt
[1, 2, 3, 4, 9, 10, 13]
```


## jq

```jq
def stoogesort:
  def swap(i;j): .[i] as $t | .[i] = .[j] | .[j] = $t;

  # for efficiency, define an auxiliary function
  # that takes as input [L, i, j]
  def ss: .[1] as $i | .[2] as $j
     | .[0]
     | if .[$j] < .[$i] then swap($i;$j) else . end
     | if $j - $i > 1 then
          (($j - $i + 1) / 3 | floor) as $t
          | [., $i, $j-$t] | ss
	  | [., $i+$t, $j] | ss
	  | [., $i, $j-$t] | ss
       else .
       end;

  [., 0, length-1] | ss;
```

'''Example'''

```jq
([],
 [1],
 [1,2],
 [1,3,2,4],
 [1,4,5,3,-6,3,7,10,-2,-5]
) | stoogesort
```

```sh
$ jq -c -n -f stooge_sort.jq
[]
[1]
[1,2]
[1,2,3,4]
[-6,-5,-2,1,3,3,4,5,7,10]
```



## Julia

```julia
function stoogesort!(a::Array, i::Int=1, j::Int=length(a))
    if a[j] < a[i]
        a[[i, j]] = a[[j, i]];
    end

    if (j - i) > 1
        t = round(Int, (j - i + 1) / 3)
        a = stoogesort!(a, i,     j - t)
        a = stoogesort!(a, i + t, j)
        a = stoogesort!(a, i,     j - t)
    end

    return a
end

x = randn(10)
@show x stoogesort!(x)
```


```txt
x = [0.222881, -1.06902, -1.07703, 0.466872, 1.52261, -0.25279, -1.72147, -0.217577, -0.556917, 2.13601]
stoogesort!(x) = [-1.72147, -1.07703, -1.06902, -0.556917, -0.25279, -0.217577, 0.222881, 0.466872, 1.52261, 2.13601]
```



## Kotlin


```scala
// version 1.1.0

fun stoogeSort(a: IntArray, i: Int, j: Int) {
    if (a[j] < a[i]) {
        val temp = a[j]
        a[j] = a[i]
        a[i] = temp
    }
    if (j - i > 1) {
        val t = (j - i + 1) / 3
        stoogeSort(a, i, j - t)
        stoogeSort(a, i + t, j)
        stoogeSort(a, i, j - t)
    }
}

fun main(args: Array<String>) {
    val a = intArrayOf(100, 2, 56, 200, -52, 3, 99, 33, 177, -199)
    println("Original : ${a.asList()}")
    stoogeSort(a, 0, a.size - 1)
    println("Sorted   : ${a.asList()}")
}
```


```txt

Original : [100, 2, 56, 200, -52, 3, 99, 33, 177, -199]
Sorted   : [-199, -52, 2, 3, 33, 56, 99, 100, 177, 200]

```



## Lua


An example using a [[Y combinator]] for anonymous recursion
and made generic with an optional predicate parameter.


```lua

local Y = function (f)
  return (function(x) return x(x) end)(function(x) return f(function(...) return x(x)(...) end) end)
end

function stoogesort(L, pred)

  pred = pred or function(a,b) return a < b end

  Y(function(recurse)
    return function(i,j)
      if pred(L[j], L[i]) then
        L[j],L[i] = L[i],L[j]
      end
      if j - i > 1 then
        local t = math.floor((j - i + 1)/3)
        recurse(i,j-t)
        recurse(i+t,j)
        recurse(i,j-t)
      end
    end
  end)(1,#L)

  return L
end

print(unpack(stoogesort{9,7,8,5,6,3,4,2,1,0}))


```



## Mathematica


```Mathematica
stoogeSort[lst_, I_, J_] := Module[{i = I, j = J, list = lst},

 If[list[[j]] < list[[i]], list[[{i,j}]] = list[[{j,i}]];]

 If[(j-i) > 1, t = Round[(j-i+1)/3];
  list=stoogeSort[list,i,j-t];
  list=stoogeSort[list,i+t,j];
  list=stoogeSort[list,i,j-t];];

 list
]
```



```txt
stoogeSort[{3,2,9,6,8},1,5]
{2,3,6,8,9}
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
%Required inputs:
%i = 1
%j = length(list)
%
function list = stoogeSort(list,i,j)

    if list(j) < list(i)
        list([i j]) = list([j i]);
    end

    if (j - i) > 1
        t = round((j-i+1)/3);
        list = stoogeSort(list,i,j-t);
        list = stoogeSort(list,i+t,j);
        list = stoogeSort(list,i,j-t);
    end

end
```

```MATLAB
>>
 stoogeSort([1 -6 4 -9],1,4)

ans =

    -9    -6     1     4
```



## MAXScript


```MAXScript
fn stoogeSort arr i: j: =
(
	if i == unsupplied do i = 1
	if j == unsupplied do j = arr.count

	if arr[j] < arr[i] do
	(
		swap arr[j] arr[i]
	)
	if j - i > 1 do
	(
		local  t = (j - i+1)/3
		arr = stoogeSort arr i:i j:(j-t)
		arr = stoogeSort arr i:(i+t) j:j
		arr = stoogeSort arr i:i j:(j-t)
	)
	return arr
)
```

```MAXScript
a = for i in 1 to 15 collect random 1 20
#(10, 2, 1, 19, 18, 20, 18, 5, 13, 2, 13, 9, 7, 10, 6)
stoogeSort a
#(1, 2, 2, 5, 6, 7, 9, 10, 10, 13, 13, 18, 18, 19, 20)

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

iList = [int 1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7]
sList = Arrays.copyOf(iList, iList.length)

sList = stoogeSort(sList)

lists = [iList, sList]
loop ln = 0 to lists.length - 1
  cl = lists[ln]
  rpt = Rexx('')
  loop ct = 0 to cl.length - 1
    rpt = rpt cl[ct]
    end ct
    say '['rpt.strip().changestr(' ', ',')']'
  end ln

return

method stoogeSort(L_ = int[], i_ = 0, j_ = L_.length - 1) public static returns int[]

  if L_[j_] < L_[i_] then do
    Lt     = L_[i_]
    L_[i_] = L_[j_]
    L_[j_] = Lt
    end
  if j_ - i_ > 1 then do
    t_ = (j_ - i_ + 1) % 3
    L_ = stoogeSort(L_, i_, j_ - t_)
    L_ = stoogeSort(L_, i_ + t_, j_)
    L_ = stoogeSort(L_, i_, j_ - t_)
    end

  return L_

```

```txt

[1,4,5,3,-6,3,7,10,-2,-5,7,5,9,-3,7]
[-6,-5,-3,-2,1,3,3,4,5,5,7,7,7,9,10]

```



## Nim


```nim
proc stoogeSort[T](a: var openarray[T], i, j: int) =
  if a[j] < a[i]: swap a[i], a[j]
  if j - i > 1:
    let t = (j - i + 1) div 3
    stoogeSort(a, i, j - t)
    stoogeSort(a, i + t, j)
    stoogeSort(a, i, j - t)

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
stoogeSort a, 0, a.high
echo a
```

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## Objeck


```objeck

bundle Default {
  class Stooge {
    function : Main(args : String[]) ~ Nil {
      nums := [1, 4, 5, 3, -6, 3, 7, 10, -2, -5];
      StoogeSort(nums);
      each(i : nums) {
        IO.Console->Print(nums[i])->Print(",");
      };
      IO.Console->PrintLine();
    }

    function : native : StoogeSort(l : Int[]) ~ Nil {
      StoogeSort(l, 0, l->Size() - 1);
    }

    function : native : StoogeSort(l : Int[], i : Int, j : Int) ~ Nil {
       if(l[j] < l[i]) {
        tmp := l[i];
        l[i] := l[j];
        l[j] := tmp;
      };

      if(j - i > 1) {
        t := (j - i + 1) / 3;
        StoogeSort(l, i, j - t);
        StoogeSort(l, i + t, j);
        StoogeSort(l, i, j - t);
      };
    }
  }
}

```



## OCaml



```ocaml
let swap ar i j =
  let tmp = ar.(i) in
  ar.(i) <- ar.(j);
  ar.(j) <- tmp

let stoogesort ar =
  let rec aux i j =
    if ar.(j) < ar.(i) then
      swap ar i j
    else if j - i > 1 then begin
      let t = (j - i + 1) / 3 in
      aux (i) (j-t);
      aux (i+t) (j);
      aux (i) (j-t);
    end
  in
  aux 0 (Array.length ar - 1)
```


testing:

```ocaml
let () =
  let ar = [| 3; 1; 7; 2; 6; 5; 4; 9; 8 |] in
  stoogesort ar;
  Array.iter (Printf.printf " %d") ar;
  print_newline()
```



## ooRexx

```ooRexx
/* Rexx */

call demo
return
exit

-- -----------------------------------------------------------------------------
--  Stooge sort implementation
-- -----------------------------------------------------------------------------
::routine stoogeSort
  use arg rL_, i_ = 0, j_ = .nil
  if j_ = .nil then j_ = rL_~items() - 1

  if rL_~get(j_) < rL_~get(i_) then do
    Lt = rL_~get(i_)
    rL_~set(i_, rL_~get(j_))
    rL_~set(j_, Lt)
    end
  if j_ - i_ > 1 then do
    t_ = (j_ - i_ + 1) % 3
    rL_ = stoogeSort(rL_, i_, j_ - t_)
    rL_ = stoogeSort(rL_, i_ + t_, j_)
    rL_ = stoogeSort(rL_, i_, j_ - t_)
    end

  return rL_

-- -----------------------------------------------------------------------------
-- Demonstrate the implementation
-- -----------------------------------------------------------------------------
::routine demo

  iList = .nlist~of(1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7)
  sList = iList~copy()

  placesList = .nlist~of( -
      "UK  London",     "US  New York",   "US  Boston",     "US  Washington" -
    , "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"     -
  )

  sList = stoogeSort(sList)
  sortedList = stoogeSort(placesList~copy())

  iLists = .list~of(iList, sList)
  loop ln = 0 to iLists~items() - 1
    icl = iLists[ln]
    rpt = ''
    loop ct = 0 to icl~items() - 1
      rpt = rpt icl[ct]
      end ct
      say '['rpt~strip()~changestr(' ', ',')']'
    end ln

  say
  sLists = .list~of(placesList, sortedList)
  loop ln = 0 to sLists~items() - 1
    scl = sLists[ln]
    loop ct = 0 to scl~items() - 1
      say right(ct + 1, 3)':' scl[ct]
      end ct
      say
    end ln

  return

-- -----------------------------------------------------------------------------
-- Helper class.  Map get and set methods for easier conversion from java.util.List
-- -----------------------------------------------------------------------------
::class NList mixinclass List public

-- Map get() to at()
::method get
  use arg ix
  return self~at(ix)

-- Map set() to put()
::method set
  use arg ix, item
  self~put(item, ix)
  return

```

```txt

[1,4,5,3,-6,3,7,10,-2,-5,7,5,9,-3,7]
[-6,-5,-3,-2,1,3,3,4,5,5,7,7,7,9,10]

  1: UK  London
  2: US  New York
  3: US  Boston
  4: US  Washington
  5: UK  Washington
  6: US  Birmingham
  7: UK  Birmingham
  8: UK  Boston

  1: UK  Birmingham
  2: UK  Boston
  3: UK  London
  4: UK  Washington
  5: US  Birmingham
  6: US  Boston
  7: US  New York
  8: US  Washington

```



## Oz


```oz
declare
  proc {StoogeSort Arr}
     proc {Swap I J}
        Tmp = Arr.I
     in
        Arr.I := Arr.J
        Arr.J := Tmp
     end

     proc {Sort I J}
        Size = J-I+1
     in
        if Arr.J < Arr.I then
           {Swap I J}
        end
        if Size >= 3 then
           Third = Size div 3
        in
           {Sort I J-Third}
           {Sort I+Third J}
           {Sort I J-Third}
        end
     end
  in
     {Sort {Array.low Arr} {Array.high Arr}}
  end

  Arr = {Tuple.toArray unit(1 4 5 3 ~6 3 7 10 ~2 ~5 7 5 9 ~3 7)}
in
  {System.printInfo "\nUnsorted: "}
  for I in {Array.low Arr}..{Array.high Arr} do
     {System.printInfo Arr.I#", "}
  end

  {StoogeSort Arr}

  {System.printInfo "\nSorted  : "}
  for I in {Array.low Arr}..{Array.high Arr} do
     {System.printInfo Arr.I#", "}
  end
```


```txt
Unsorted: 1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7,
Sorted  : -6, -5, -3, -2, 1, 3, 3, 4, 5, 5, 7, 7, 7, 9, 10,
```



## PARI/GP


```parigp
stoogeSort(v)={
  local(v=v); \\ Give children access to v
  ss(1,#v);   \\ Sort
  v
}

ss(i,j)={
  my(t);
  if(v[j]<v[i],
    t=v[i];
    v[i]=v[j];
    v[j]=t
  );
  if(j-i > 1,
    t=(1+j-i)\3;
    ss(i,j-t);
    ss(i+t,j);
    ss(i,j-t)
  )
};
```



## Pascal


```pascal
program StoogeSortDemo;

type
  TIntArray = array of integer;

procedure stoogeSort(var m: TIntArray; i, j: integer);
  var
    t, temp: integer;
  begin
    if m[j] < m[i] then
    begin
      temp := m[j];
      m[j] := m[i];
      m[i] := temp;
    end;
    if j - i > 1 then
    begin
      t := (j - i + 1) div 3;
      stoogesort(m, i, j-t);
      stoogesort(m, i+t, j);
      stoogesort(m, i, j-t);
    end;
  end;

var
  data: TIntArray;
  i: integer;

begin
  setlength(data, 8);
  Randomize;
  writeln('The data before sorting:');
  for i := low(data) to high(data) do
  begin
    data[i] := Random(high(data));
    write(data[i]:4);
  end;
  writeln;
  stoogeSort(data, low(data), high(data));
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
```

```txt
./StoogeSort
The data before sorting:
   6   1   2   1   5   2   1   5
The data after sorting:
   1   1   1   2   2   5   5   6

```



## Perl


```perl
sub stooge {
        use integer;
        my ($x, $i, $j) = @_;

        $i //= 0;
        $j //= $#$x;

        if ( $x->[$j] < $x->[$i] ) {
                @$x[$i, $j] = @$x[$j, $i];
        }
        if ( $j - $i > 1 ) {
                my $t = ($j - $i + 1) / 3;
                stooge( $x, $i,      $j - $t );
                stooge( $x, $i + $t, $j      );
                stooge( $x, $i,      $j - $t );
        }
}


my @a = map (int rand(100), 1 .. 10);
print "Before @a\n";
stooge(\@a);
print "After  @a\n";

```



## Perl 6


```perl6
sub stoogesort( @L, $i = 0, $j = @L.end ) {
    @L[$j,$i] = @L[$i,$j] if @L[$i] > @L[$j];

    my $interval = $j - $i;

    if $interval > 1 {
         my $t = ( $interval + 1 ) div 3;
         stoogesort( @L, $i   , $j-$t );
         stoogesort( @L, $i+$t, $j    );
         stoogesort( @L, $i   , $j-$t );
    }
    return @L;
}

my @L = 1, 4, 5, 3, -6, 3, 7, 10, -2, -5;

stoogesort(@L).Str.say;

```



## Phix

Copy of [[Sorting_algorithms/Stooge_sort#Euphoria|Euphoria]]

```Phix
function stoogesort(sequence s, integer i=1, integer j=length(s))
integer t
    if s[j]<s[i] then
        {s[i],s[j]} = {s[j],s[i]}
    end if
    if j-i>1 then
        t = floor((j-i+1)/3)
        s = stoogesort(s,i,  j-t)
        s = stoogesort(s,i+t,j  )
        s = stoogesort(s,i,  j-t)
    end if
    return s
end function
```



## PHP


```php

function stoogeSort(&$arr, $i, $j)
{
	if($arr[$j] < $arr[$i])
	{
		list($arr[$j],$arr[$i]) = array($arr[$i], $arr[$j]);
	}
	if(($j - $i) > 1)
	{
		$t = ($j - $i + 1) / 3;
		stoogesort($arr, $i, $j - $t);
		stoogesort($arr, $i + $t, $j);
		stoogesort($arr, $i, $j - $t);
	}
}

```



## PicoLisp


```PicoLisp
(de stoogeSort (L N)
   (default N (length L))
   (let P (nth L N)
      (when (> (car L) (car P))
         (xchg L P) ) )
   (when (> N 2)
      (let D (/ N 3)
         (stoogeSort L (- N D))
         (stoogeSort (nth L (inc D)) (- N D))
         (stoogeSort L (- N D)) ) )
   L )
```

Test:

```txt
: (apply < (stoogeSort (make (do 100 (link (rand))))))
-> T
```



## PL/I


```pli
stoogesort: procedure (L) recursive; /* 16 August 2010 */
   declare L(*) fixed binary;
   declare (i, j, t, temp) fixed binary;

   j = hbound(L,1);
   do i = lbound(L, 1) to j;
     if L(j) < L(i) then
         do; temp = L(i); L(i) = L(j); L(j) = temp; end;
     if j - i > 1 then
         do;
            t = (j - i + 1)/3;
            call stoogesort(L, i  , j-t);
            call stoogesort(L, i+t, j  );
            call stoogesort(L, i  , j-t);
         end;
   end;
end stoogesort;
```



## PowerBASIC


[[PowerBASIC for DOS]] can use the BASIC code above,
by removing <code>CONST</code> and changing
all instances of <code>arraysize</code> to
<code>%arraysize</code> (note the percent sign).

This version is closely based on the BASIC code above.


```powerbasic
%arraysize = 10

SUB stoogesort (L() AS LONG, i AS LONG, j AS LONG)
    IF L(j) < L(i) THEN SWAP L(i), L(j)
    IF (j - i) > 1 THEN
        DIM t AS LONG
        t = (j - i + 1) / 3
        stoogesort L(), i, j - t
        stoogesort L(), i + t, j
        stoogesort L(), i, j - t
    END IF
END SUB

FUNCTION PBMAIN () AS LONG
    RANDOMIZE TIMER

    DIM x(%arraysize) AS LONG
    DIM i AS LONG, s AS STRING

    s = "Before: "
    FOR i = 0 TO %arraysize
        x(i) = INT(RND * 100)
        s = s & STR$(x(i)) & " "
    NEXT

    stoogesort x(), 0, %arraysize

    #IF %DEF(%PB_CC32)
        PRINT s
        s = ""
    #ELSE
        s = s & $CRLF
    #ENDIF

    s = s & "After: "
    FOR i = 0 TO %arraysize
        s = s & STR$(x(i)) & " "
    NEXT

    ? s
END FUNCTION
```


 Before:  88  32  82  88  0  82  65  87  40  1  69
 After:  0  1  32  40  65  69  82  82  87  88  88
 Before:  60  64  95  11  52  26  7  4  51  67  47
 After:  4  7  11  26  47  51  52  60  64  67  95
 Before:  47  88  67  76  60  66  69  86  92  41  6
 After:  6  41  47  60  66  67  69  76  88  86  92


## PowerShell


```PowerShell
Function StoogeSort( [Int32[]] $L )
{
	$i = 0
	$j = $L.length-1
	if( $L[$j] -lt $L[$i] )
	{
		$L[$i] = $L[$i] -bxor $L[$j]
		$L[$j] = $L[$i] -bxor $L[$j]
		$L[$i] = $L[$i] -bxor $L[$j]
	}
	if( $j -gt 1 )
	{
		$t = [int] ( ( $j + 1 ) / 3 )
		$k = $j - $t + 1
		[Array]::Copy( [Int32[]] ( StoogeSort( $L[0..( $j - $t ) ] ) ), $L, $k )
		[Array]::ConstrainedCopy( [Int32[]] ( StoogeSort( $L[$t..$j ] ) ), 0, $L, $t, $k )
		[Array]::Copy( [Int32[]] ( StoogeSort( $L[0..( $j - $t ) ] ) ), $L, $k )
	}
	$L
}

StoogeSort 9, 7, 5, 3, 1, 2, 4, 6, 8
```



## PureBasic


```PureBasic
Procedure Stooge_Sort(Array L.i(1), i=0 , j=0)
  If j=0
    j=ArraySize(L())
  EndIf
  If L(i)>L(j)
    Swap L(i), L(j)
  EndIf
  If j-i>1
    Protected t=(j-i+1)/3
    Stooge_Sort(L(), i,   j-t)
    Stooge_Sort(L(), i+t, j )
    Stooge_Sort(L(), i,   j-t)
  EndIf
EndProcedure
```

Implementation may be as
```PureBasic
Define AmountOfPosts=(?Stop_Data-?Start_data)/SizeOf(Integer)
Dim    Xyz.i(AmountOfPosts)
CopyMemory(?Start_data, @Xyz(), ?Stop_Data-?Start_data)

Stooge_Sort(Xyz())

For i=0 To ArraySize(Xyz())
  Debug Xyz(i)
Next i

DataSection
  Start_data:
  Data.i  1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7
  Stop_Data:
EndDataSection
```



## Python


```python
>>>
 data = [1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7]
>>> def stoogesort(L, i=0, j=None):
	if j is None:
		j = len(L) - 1
	if L[j] < L[i]:
		L[i], L[j] = L[j], L[i]
	if j - i > 1:
		t = (j - i + 1) // 3
		stoogesort(L, i  , j-t)
		stoogesort(L, i+t, j  )
		stoogesort(L, i  , j-t)
	return L

>>> stoogesort(data)
[-6, -5, -3, -2, 1, 3, 3, 4, 5, 5, 7, 7, 7, 9, 10]
```


This alternate solution uses a wrapper function
to compute the initial value of ''j''
rather than detecting the sentinel value ''None''.

```python
>>>
 def stoogesort(L, i, j):
	if L[j] < L[i]:
		L[i], L[j] = L[j], L[i]
	if j - i > 1:
		t = (j - i + 1) // 3
		stoogesort(L, i  , j-t)
		stoogesort(L, i+t, j  )
		stoogesort(L, i  , j-t)
	return L

>>> def stooge(L): return stoogesort(L, 0, len(L) - 1)

>>> data = [1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7]
>>> stooge(data)
[-6, -5, -3, -2, 1, 3, 3, 4, 5, 5, 7, 7, 7, 9, 10]
```



## R


```R
stoogesort = function(vect) {
	i = 1
	j = length(vect)
	if(vect[j] < vect[i])  vect[c(j, i)] = vect[c(i, j)]
	if(j - i > 1) {
		t = (j - i + 1) %/% 3
		vect[i:(j - t)] = stoogesort(vect[i:(j - t)])
		vect[(i + t):j] = stoogesort(vect[(i + t):j])
		vect[i:(j - t)] = stoogesort(vect[i:(j - t)])
	}
	vect
}

v = sample(21, 20)
k = stoogesort(v)
v
k
```

```txt

 [1] 13  5 20 16 11 19 17  7  9 14 21 18  2 10  1  6  8  4 15 12
 [1]  1  2  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21

```



## Racket


```racket

#lang racket
(define (stooge-sort xs [i 0] [j (- (vector-length xs) 1)])
  (define (x i) (vector-ref xs i))
  (define (x! i v) (vector-set! xs i v))
  (define (swap! i j) (define t (x i)) (x! i (x j)) (x! j t))
  (when (> (x i) (x j)) (swap! i j))
  (when (> (- j i) 1)
    (define t (quotient (+ j (- i) 1) 3))
    (stooge-sort xs i (- j t))
    (stooge-sort xs (+ i t) j)
    (stooge-sort xs i (- j t)))
  xs)

```



## REXX

This REXX example starts an array at element zero   (but any integer could be used);   a zero-

based index was used because the algorithm shown in the Rosetta Code task used zero.

```REXX
/*REXX program sorts  an  integer array   @.   [the first element starts at index zero].*/
parse arg N .                                    /*obtain an optional argument from C.L.*/
if N=='' | N==","  then N=19                     /*Not specified?  Then use the default.*/
call gen@                                        /*generate a type of scattered array.  */
call show    'before sort'                       /*show the   before   array elements.  */
say copies('▒', wN+wV+ 50)                       /*show a separator line (between shows)*/
call stoogeSort  0, N                            /*invoke the  Stooge Sort.             */
call show    ' after sort'                       /*show the    after   array elements.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen@: wV= 0;   do k=0  to N;      @.k= k*2 + k*-1**k;     if @.k//7==0  then @.k= -100 - k
               wV= max(wV, length(@.k) );  end;        wN=length(N);                return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: do j=0 to N; say right('element',22) right(j,wN) arg(1)":" right(@.j,wV); end;return
/*──────────────────────────────────────────────────────────────────────────────────────*/
stoogeSort: procedure expose @.;  parse arg i,j                  /*sort from  I ───► J. */
            if @.j<@.i  then parse value @.i @.j  with  @.j @.i  /*swap  @.i  with  @.j */
            if j-i>1    then do;   t=(j-i+1) % 3                 /*%:  integer division.*/
                                   call stoogeSort  i  ,  j-t    /*invoke recursively.  */
                                   call stoogeSort  i+t,  j      /*   "        "        */
                                   call stoogeSort  i  ,  j-t    /*   "        "        */
                             end
            return
```

{{out|output|text=  when using the default (internal generated) inputs:

```txt

               element  0 before sort: -100
               element  1 before sort:    1
               element  2 before sort:    6
               element  3 before sort:    3
               element  4 before sort:   12
               element  5 before sort:    5
               element  6 before sort:   18
               element  7 before sort: -107
               element  8 before sort:   24
               element  9 before sort:    9
               element 10 before sort:   30
               element 11 before sort:   11
               element 12 before sort:   36
               element 13 before sort:   13
               element 14 before sort: -114
               element 15 before sort:   15
               element 16 before sort:   48
               element 17 before sort:   17
               element 18 before sort:   54
               element 19 before sort:   19
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
               element  0  after sort: -114
               element  1  after sort: -107
               element  2  after sort: -100
               element  3  after sort:    1
               element  4  after sort:    3
               element  5  after sort:    5
               element  6  after sort:    6
               element  7  after sort:    9
               element  8  after sort:   11
               element  9  after sort:   12
               element 10  after sort:   13
               element 11  after sort:   15
               element 12  after sort:   17
               element 13  after sort:   18
               element 14  after sort:   19
               element 15  after sort:   24
               element 16  after sort:   30
               element 17  after sort:   36
               element 18  after sort:   48
               element 19  after sort:   54

```



## Ring


```ring

test = [4, 65, 2, -31, 0, 99, 2, 83, 782, 1]
stoogeSort(test, 1, len(test))
for i = 1 to 10
    see "" + test[i] + " "
next
see nl

func stoogeSort list, i, j
     if list[j] < list[i]
        temp = list[i]
        list[i] = list[j]
        list[j] = temp ok
     if j - i > 1
        t = (j - i + 1)/3
        stoogeSort(list, i, j-t)
        stoogeSort(list, i+t, j)
        stoogeSort(list, i, j-t) ok
     return list

```

Output:

```txt

-31 0 1 2 2 4 65 83 99 782

```



## Ruby


```ruby
class Array
  def stoogesort
    self.dup.stoogesort!
  end

  def stoogesort!(i = 0, j = self.length-1)
    if self[j] < self[i]
      self[i], self[j] = self[j], self[i]
    end
    if j - i > 1
      t = (j - i + 1)/3
      stoogesort!(i, j-t)
      stoogesort!(i+t, j)
      stoogesort!(i, j-t)
    end
    self
  end
end

p [1,4,5,3,-6,3,7,10,-2,-5].stoogesort
```


```txt
[-6, -5, -2, 1, 3, 3, 4, 5, 7, 10]
```



## Rust


```rust
fn stoogesort<E>
(a: &mut [E])
    where E: PartialOrd
{
    let len = a.len();

    if a.first().unwrap() > a.last().unwrap() {
        a.swap(0, len - 1);
    }
    if len - 1 > 1 {
        let t = len / 3;
        stoogesort(&mut a[..len - 1]);
        stoogesort(&mut a[t..]);
        stoogesort(&mut a[..len - 1]);
    }
}

fn main() {
    let mut numbers = vec![1_i32, 9, 4, 7, 6, 5, 3, 2, 8];
    println!("Before: {:?}", &numbers);
    stoogesort(&mut numbers);
    println!("After: {:?}", &numbers);
}
```



## Scala


```Scala
object StoogeSort extends App {
  def stoogeSort(a: Array[Int], i: Int, j: Int) {
    if (a(j) < a(i)) {
      val temp = a(j)
      a(j) = a(i)
      a(i) = temp
    }
    if (j - i > 1) {
      val t = (j - i + 1) / 3
      stoogeSort(a, i, j - t)
      stoogeSort(a, i + t, j)
      stoogeSort(a, i, j - t)
    }
  }

  val a = Array(100, 2, 56, 200, -52, 3, 99, 33, 177, -199)
  println(s"Original : ${a.mkString(", ")}")
  stoogeSort(a, 0, a.length - 1)
  println(s"Sorted   : ${a.mkString(", ")}")
}
```


See it running in your browser by [https://scastie.scala-lang.org/QTCrb5SNTVqDNC6oRQRmZw Scastie (JVM)].

## Sidef


```ruby
func stooge(x, i, j) {
    if (x[j] < x[i]) {
        x.swap(i, j)
    }

    if (j-i > 1) {
        var t = ((j - i + 1) / 3)
        stooge(x, i,     j - t)
        stooge(x, i + t, j    )
        stooge(x, i,     j - t)
    }
}

var a = 10.of { 100.irand }

say "Before #{a}"
stooge(a, 0, a.end)
say "After  #{a}"
```



## Smalltalk

```smalltalk
OrderedCollection extend [
    stoogeSortFrom: i to: j [
	(self at: j) < (self at: i)
	  ifTrue: [ self swapElement: i with: j ].
	j - i > 1
          ifTrue: [
	      |t| t := (j - i + 1)//3.
	      self stoogeSortFrom: i to: (j-t).
	      self stoogeSortFrom: (i+t) to: j.
	      self stoogeSortFrom: i to: (j-t)
          ]
    ]
    stoogeSort [ self stoogeSortFrom: 1 to: (self size) ]
    swapElement: i with: j [
	|t| t := self at: i.
        self at: i put: (self at: j).
	self at: j put: t
    ]
].

|test|
test := #( 1 4 5 3 -6 3 7 10 -2 -5) asOrderedCollection.
test stoogeSort.
test printNl.
```



## Swift


```Swift
func stoogeSort(inout arr:[Int], _ i:Int = 0, var _ j:Int = -1) {
    if j == -1 {
        j = arr.count - 1
    }

    if arr[i] > arr[j] {
        swap(&arr[i], &arr[j])
    }

    if j - i > 1 {
        let t = (j - i + 1) / 3
        stoogeSort(&arr, i, j - t)
        stoogeSort(&arr, i + t, j)
        stoogeSort(&arr, i, j - t)
    }
}

var a = [-4, 2, 5, 2, 3, -2, 1, 100, 20]

stoogeSort(&a)

println(a)
```

```txt

[-4, -2, 1, 2, 2, 3, 5, 20, 100]

```



## Tcl

```tcl
package require Tcl 8.5

proc stoogesort {L {i 0} {j -42}} {
   if {$j == -42} {# Magic marker
      set j [expr {[llength $L]-1}]
   }
   set Li [lindex $L $i]
   set Lj [lindex $L $j]
   if {$Lj < $Li} {
      lset L $i $Lj
      lset L $j $Li
   }
   if {$j-$i > 1} {
      set t [expr {($j-$i+1)/3}]
      set L [stoogesort $L $i [expr {$j-$t}]]
      set L [stoogesort $L [expr {$i+$t}] $j]
      set L [stoogesort $L $i [expr {$j-$t}]]
   }
   return $L
}

stoogesort {1 4 5 3 -6 3 7 10 -2 -5}
```

```txt
-6 -5 -2 1 3 3 4 5 7 10
```



## uBasic/4tH

<lang>PRINT "Stooge sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Stoogesort (n)
  PROC _ShowArray (n)
PRINT

END


_InnerStooge PARAM(2)                  ' Stoogesort
  LOCAL(1)

  IF @(b@) < @(a@) Then Proc _Swap (a@, b@)
  IF b@ - a@ > 1 THEN
    c@ = (b@ - a@ + 1)/3
    PROC _InnerStooge (a@, b@-c@)
    PROC _InnerStooge (a@+c@, b@)
    PROC _InnerStooge (a@, b@-c@)
  ENDIF
RETURN


_Stoogesort PARAM(1)
  PROC _InnerStooge (0, a@ -  1)
RETURN


_Swap PARAM(2)                         ' Swap two array elements
  PUSH @(a@)
  @(a@) = @(b@)
  @(b@) = POP()
RETURN


_InitArray                             ' Init example array
  PUSH 4, 65, 2, -31, 0, 99, 2, 83, 782, 1

  FOR i = 0 TO 9
    @(i) = POP()
  NEXT

RETURN (i)


_ShowArray PARAM (1)                   ' Show array subroutine
  FOR i = 0 TO a@-1
    PRINT @(i),
  NEXT

  PRINT
RETURN
```



## XPL0


```XPL0
code ChOut=8, IntOut=11;        \intrinsic routines

proc StoogeSort(L, I, J);       \Sort array L
int L, I, J;
int  T;
[if L(J) < L(I) then
    [T:= L(I);  L(I):= L(J);  L(J):= T]; \swap
if J-I > 1 then
    [T:= (J-I+1)/3;
    StoogeSort(L, I, J-T);
    StoogeSort(L, I+T, J);
    StoogeSort(L, I, J-T);
    ];
];

int A, I;
[A:= [3, 1, 4, 1, -5, 9, 2, 6, 5, 4];
StoogeSort(A, 0, 10-1);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
]
```


```txt

-5 1 1 2 3 4 4 5 6 9

```



## Yorick

Based on pseudocode, except using Yorick's 1-based arrays.
Sorts in place.

```yorick
func stoogesort(&L, i, j) {
  if(is_void(i)) i = 1;
  if(is_void(j)) j = numberof(L);
  if(L(j) < L(i))
    L([i,j]) = L([j,i]);
  if(j - i > 1) {
    t = (j - i + 1)/3;
    stoogesort, L, i, j-t;
    stoogesort, L, i+t, j;
    stoogesort, L, i, j-t;
  }
}
```


Example interactive use:

```txt
> foo = [1,4,5,3,-6,3,7,10,-2,-5]
> stoogesort, foo
> foo
[-6,-5,-2,1,3,3,4,5,7,10]
```



## zkl


```zkl
fcn stoogeSort(list,i=0,j=Void){ if(Void==j) j=list.len() - 1; // default parameters set before call
   if(list[j]<list[i]) list.swap(i,j);
   if(j - i >1){
      t:=(j - i + 1)/3;
      stoogeSort(list,i  , j-t);
      stoogeSort(list,i+t, j  );
      stoogeSort(list,i  , j-t);
   }
   list
}
```


```zkl
stoogeSort(List(67,-201,0,9,9,231,4)).println();
```

```txt

L(-201,0,4,9,9,67,231)

```


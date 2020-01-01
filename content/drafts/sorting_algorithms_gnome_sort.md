+++
title = "Sorting algorithms/Gnome sort"
description = ""
date = 2019-10-18T20:11:48Z
aliases = []
[extra]
id = 4088
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}
{{Wikipedia|Gnome sort}}


Gnome sort is a sorting algorithm which is similar to [[Insertion sort]], except that moving an element to its proper place is accomplished by a series of swaps, as in [[Bubble Sort]].

The pseudocode for the algorithm is:
 '''function''' ''gnomeSort''(a[0..size-1])
     i := 1
     j := 2
     '''while''' i < size '''do'''
         '''if''' a[i-1] <= a[i] '''then'''
             ''// for descending sort, use >= for comparison''
             i := j
             j := j + 1
         '''else'''
             '''swap''' a[i-1] '''and''' a[i]
             i := i - 1
             '''if''' i = 0 '''then'''
                 i := j
                 j := j + 1
             '''endif'''
         '''endif'''
     '''done'''


;Task:
Implement the Gnome sort in your language to sort an array (or list) of numbers.





## ActionScript


```ActionScript
function gnomeSort(array:Array)
{
	var pos:uint = 0;
	while(pos < array.length)
	{
		if(pos == 0 || array[pos] >= array[pos-1])
			pos++;
		else
		{
			var tmp = array[pos];
			array[pos] = array[pos-1];
			array[pos-1] = tmp;
			pos--;
		}
	}
	return array;
}
```



## Ada

This example is a generic procedure for constrained array types.

```Ada
generic
   type Element_Type is private;
   type Index is (<>);
   type Collection is array(Index) of Element_Type;
   with function "<=" (Left, Right : Element_Type) return Boolean is <>;
procedure Gnome_Sort(Item : in out Collection);
```



```Ada
procedure Gnome_Sort(Item : in out Collection) is
   procedure Swap(Left, Right : in out Element_Type) is
      Temp : Element_Type := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap;

   I : Integer := Index'Pos(Index'Succ(Index'First));
   J : Integer := I + 1;
begin
   while I <= Index'Pos(Index'Last) loop
      if Item(Index'Val(I - 1)) <= Item(Index'Val(I)) then
         I := J;
         J := J + 1;
      else
         Swap(Item(Index'Val(I - 1)), Item(Index'Val(I)));
         I := I - 1;
         if I = Index'Pos(Index'First) then
            I := J;
            J := J + 1;
         end if;
      end if;
   end loop;
end Gnome_Sort;
```

Usage example:

```Ada
with Gnome_Sort;
with Ada.Text_Io; use Ada.Text_Io;

procedure Gnome_Sort_Test is
   type Index is range 0..9;
   type Buf is array(Index) of Integer;
   procedure Sort is new Gnome_Sort(Integer, Index, Buf);
   A : Buf := (900, 700, 800, 600, 400, 500, 200, 100, 300, 0);
begin
   for I in A'range loop
      Put(Integer'Image(A(I)));
   end loop;
   New_Line;
   Sort(A);
   for I in A'range loop
      Put(Integer'Image(A(I)));
   end loop;
   New_Line;
end Gnome_Sort_Test;
```



## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386}}

```algol68
MODE SORTSTRUCT = CHAR;

PROC inplace gnome sort = (REF[]SORTSTRUCT list)REF[]SORTSTRUCT:
BEGIN
  INT i:=LWB list + 1, j:=LWB list + 2;
  WHILE i <= UPB list DO
    IF list[i-1] <= list[i] THEN
      i := j; j+:=1
    ELSE
      SORTSTRUCT swap = list[i-1]; list[i-1]:= list[i]; list[i]:= swap;
      i-:=1;
      IF i=LWB list THEN i:=j; j+:=1 FI
    FI
  OD;
  list
END;

PROC gnome sort = ([]SORTSTRUCT seq)[]SORTSTRUCT:
  in place gnome sort(LOC[LWB seq: UPB seq]SORTSTRUCT:=seq);

[]SORTSTRUCT char array data = "big fjords vex quick waltz nymph";
print((gnome sort(char array data), new line))
```

Output:

```txt

     abcdefghiijklmnopqrstuvwxyz

```


## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276379.html#276379 forum]

```AutoHotkey
MsgBox % GnomeSort("")
MsgBox % GnomeSort("xxx")
MsgBox % GnomeSort("3,2,1")
MsgBox % GnomeSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")

GnomeSort(var) {                         ; SORT COMMA SEPARATED LIST
   StringSplit a, var, `,                ; make array, size = a0
   i := 2, j := 3
   While i <= a0 {                       ; stop when sorted
      u := i-1
      If (a%u% < a%i%)                   ; search for pairs to swap
         i := j, j := j+1
      Else {                             ; swap
         t := a%u%, a%u% := a%i%, a%i% := t
         If (--i = 1)                    ; restart search
            i := j, j++
      }
   }
   Loop % a0                             ; construct string from sorted array
      sorted .= "," . a%A_Index%
   Return SubStr(sorted,2)               ; drop leading comma
}
```



## AWK


AWK arrays can be passed as parameters, but not returned, so they are usually global.

This version includes the mark/resume optimization. It remembers where it was before backing up so that once an item is backed up to its proper place the process resumes from where it was before backing up.


```awk
#!/usr/bin/awk -f

BEGIN {
    d[1] = 3.0
    d[2] = 4.0
    d[3] = 1.0
    d[4] = -8.4
    d[5] = 7.2
    d[6] = 4.0
    d[7] = 1.0
    d[8] = 1.2
    showD("Before: ")
    gnomeSortD()
    showD("Sorted: ")
    exit
}

function gnomeSortD(    i) {
    for (i = 2; i <= length(d); i++) {
        if (d[i] < d[i-1]) gnomeSortBackD(i)
    }
}

function gnomeSortBackD(i,     t) {
    for (; i > 1 && d[i] < d[i-1]; i--) {
        t = d[i]
        d[i] = d[i-1]
        d[i-1] = t
    }
}

function showD(p,   i) {
    printf p
    for (i = 1; i <= length(d); i++) {
        printf d[i] " "
    }
    print ""
}

```


Example output:
 Before: 3 4 1 -8.4 7.2 4 1 1.2
 Sorted: -8.4 1 1 1.2 3 4 4 7.2


## BASIC

{{works with|QuickBasic|4.5}}

{{trans|C}}

```qbasic
dim a(0 to n-1) as integer
'...more code...
sort:
i = 1
j = 2

while(i < ubound(a) - lbound(a))
  if a(i-1) <= a(i) then
    i = j
    j = j + 1
  else
    swap a(i-1), a(i)
    i = i - 1
    if i = 0 then
       i = j
       j = j + 1
    end if
  end if
wend
```



## Batch File

{{works with|Windows NT}}


```dos
@ECHO OFF
SETLOCAL EnableExtensions EnableDelayedExpansion
:: GnomeSort.cmd in WinNT Batch using pseudo array.
:: Set the number of random elements to sort.
SET numElements=100
:: Decrement numElements for use in zero-based loops as in (0, 1, %numElements% - 1).
SET /A tmpElements=%numElements% - 1

:: Create array of random numbers and output to file.
ECHO GnomeSort Random Input 0 to %tmpElements%:>%~n0.txt
FOR /L %%X IN (0, 1, %tmpElements%) DO (
	SET array[%%X]=!RANDOM!
	ECHO !array[%%X]!>>%~n0.txt
)

:GnomeSort
:: Initialize the pointers i-1, i, and j.
SET gs1=0
SET gs2=1
SET gs3=2
:GS_Loop
:: Implementing a WHILE loop in WinNT batch using GOTO. It only executes
:: if the condition is true and continues until the condition is false.
:: First, display [i-1][j - 2] to the Title Bar.
SET /A gsTmp=%gs3% - 2
TITLE GnomeSort:[%gs1%][%gsTmp%] of %tmpElements%
:: ...then start Main Loop. It's a direct implementation of the
:: pseudo code supplied by Rosetta Code. I had to add an additional
:: pointer to represent i-1, because of limitations in WinNT Batch.
IF %gs2% LSS %numElements% (
	REM if i-1 <= i advance pointers to next unchecked element, then loop.
	IF !array[%gs1%]! LEQ !array[%gs2%]! (
		SET /A gs1=%gs3% - 1
		SET /A gs2=%gs3%
		SET /A gs3=%gs3% + 1
	) ELSE (
	REM ... else swap i-1 and i, decrement pointers to check previous element, then loop.
		SET gsTmp=!array[%gs1%]!
		SET array[%gs1%]=!array[%gs2%]!
		SET array[%gs2%]=!gsTmp!
		SET /A gs1-=1
		SET /A gs2-=1
		REM if first element has been reached, set pointers to next unchecked element.
		IF !gs2! EQU 0 (
			SET /A gs1=%gs3% - 1
			SET /A gs2=%gs3%
			SET /A gs3=%gs3% + 1
		)
	)
	GOTO :GS_Loop
)
TITLE GnomeSort:[%gs1%][%gsTmp%] - Done!

:: write sorted elements out to file
ECHO.>>%~n0.txt
ECHO GnomeSort Sorted Output 0 to %tmpElements%:>>%~n0.txt
FOR /L %%X IN (0, 1, %tmpElements%) DO ECHO !array[%%X]!>>%~n0.txt

ENDLOCAL
EXIT /B 0
```



## BBC BASIC


```BBCBASIC
DEF PROC_GnomeSort1(Size%)
I%=2
J%=2
REPEAT
  IF data%(J%-1) <=data%(J%) THEN
    I%+=1
    J%=I%
  ELSE
    SWAP data%(J%-1),data%(J%)
    J%-=1
    IF J%=1 THEN
       I%+=1
       J%=I%
    ENDIF
  ENDIF
UNTIL I%>Size%
ENDPROC
```



## C


This algorithm sorts in place modifying the passed ''array'' (of <code>n</code> integer numbers).

```c
void gnome_sort(int *a, int n)
{
  int i=1, j=2, t;
# define swap(i, j) { t = a[i]; a[i] = a[j]; a[j] = t; }
  while(i < n) {
    if (a[i - 1] > a[i]) {
      swap(i - 1, i);
      if (--i) continue;
    }
    i = j++;
  }
# undef swap
}
```


## C#


```c#

        public static void gnomeSort(int[] anArray)
        {
            int first = 1;
            int second = 2;

            while (first < anArray.Length)
            {
                if (anArray[first - 1] <= anArray[first])
                {
                    first = second;
                    second++;
                }
                else
                {
                    int tmp = anArray[first - 1];
                    anArray[first - 1] = anArray[first];
                    anArray[first] = tmp;
                    first -= 1;
                    if (first == 0)
                    {
                        first = 1;
                        second = 2;
                    }
                }

            }
        }

```



## C++

Uses C++11. Compile with
 g++ -std=c++11 gnome.cpp

```cpp
#include <algorithm>
#include <iterator>
#include <iostream>

template<typename RandomAccessIterator>
void gnome_sort(RandomAccessIterator begin, RandomAccessIterator end) {
  auto i = begin + 1;
  auto j = begin + 2;

  while (i < end) {
    if (!(*i < *(i - 1))) {
      i = j;
      ++j;
    } else {
      std::iter_swap(i - 1, i);
      --i;
      if (i == begin) {
        i = j;
        ++j;
      }
    }
  }
}

int main() {
  int a[] = {100, 2, 56, 200, -52, 3, 99, 33, 177, -199};
  gnome_sort(std::begin(a), std::end(a));
  copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
  std::cout << "\n";
}
```

Output:

```txt

-199 -52 2 3 33 56 99 100 177 200

```



## Clojure

{{trans|Haskell}}

```clojure
(defn gnomesort
  ([c] (gnomesort c <))
  ([c pred]
     (loop [x [] [y1 & ys :as y] (seq c)]
       (cond (empty? y) x
             (empty? x) (recur (list y1) ys)
             true (let [zx (last x)]
                    (if (pred y1 zx)
                      (recur (butlast x) (concat (list y1 zx) ys))
                      (recur (concat x (list y1)) ys)))))))

(println (gnomesort [3 1 4 1 5 9 2 6 5]))
```



## COBOL

Procedure division stuff only.

```COBOL
       C-SORT SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           SET WB-IX-1 TO 2.
           MOVE 1 TO WC-NEXT-POSN.

           PERFORM E-GNOME UNTIL WC-NEXT-POSN > WC-SIZE.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       E-GNOME SECTION.
       E-000.
           IF WB-ENTRY(WB-IX-1 - 1) NOT > WB-ENTRY(WB-IX-1)
              ADD 1       TO WC-NEXT-POSN
              SET WB-IX-1 TO WC-NEXT-POSN
           ELSE
              MOVE WB-ENTRY(WB-IX-1 - 1) TO WC-TEMP
              MOVE WB-ENTRY(WB-IX-1)     TO WB-ENTRY(WB-IX-1 - 1)
              MOVE WC-TEMP               TO WB-ENTRY(WB-IX-1)
              SET WB-IX-1                DOWN BY 1
              IF WB-IX-1 = 1
                 ADD 1       TO WC-NEXT-POSN
                 SET WB-IX-1 TO WC-NEXT-POSN.

       E-999.
           EXIT.
```



## Common Lisp


```lisp
(defun gnome-sort (array predicate &aux (length (length array)))
  (do ((position (min 1 length)))
      ((eql length position) array)
    (cond
     ((eql 0 position)
      (incf position))
     ((funcall predicate
               (aref array position)
               (aref array (1- position)))
      (rotatef (aref array position)
               (aref array (1- position)))
      (decf position))
     (t (incf position)))))
```


## D


```d
import std.stdio, std.algorithm;

void gnomeSort(T)(T arr) {
    int i = 1, j = 2;
    while (i < arr.length) {
        if (arr[i-1] <= arr[i]) {
            i = j;
            j++;
        } else {
            swap(arr[i-1], arr[i]);
            i--;
            if (i == 0) {
                i = j;
                j++;
            }
        }
    }
}

void main() {
    auto a = [3,4,2,5,1,6];
    gnomeSort(a);
    writeln(a);
}
```



## Delphi

Dynamic array is a 0-based array of variable length

Static array is an arbitrary-based array of fixed length

```Delphi
program TestGnomeSort;

{$APPTYPE CONSOLE}

{.$DEFINE DYNARRAY}  // remove '.' to compile with dynamic array

type
  TItem = Integer;   // declare ordinal type for array item
{$IFDEF DYNARRAY}
  TArray = array of TItem;          // dynamic array
{$ELSE}
  TArray = array[0..15] of TItem;   // static array
{$ENDIF}

procedure GnomeSort(var A: TArray);
var
  Item: TItem;
  I, J: Integer;

begin
  I:= Low(A) + 1;
  J:= Low(A) + 2;
  while I <= High(A) do begin
    if A[I - 1] <= A[I] then begin
      I:= J;
      J:= J + 1;
    end
    else begin
      Item:= A[I - 1];
      A[I - 1]:= A[I];
      A[I]:= Item;
      I:= I - 1;
      if I = Low(A) then begin
        I:= J;
        J:= J + 1;
      end;
    end;
  end;
end;

var
  A: TArray;
  I: Integer;

begin
{$IFDEF DYNARRAY}
  SetLength(A, 16);
{$ENDIF}
  for I:= Low(A) to High(A) do
    A[I]:= Random(100);
  for I:= Low(A) to High(A) do
    Write(A[I]:3);
  Writeln;
  GnomeSort(A);
  for I:= Low(A) to High(A) do
    Write(A[I]:3);
  Writeln;
  Readln;
end.
```

Output:

```txt

  0  3 86 20 27 67 31 16 37 42  8 47  7 84  5 29
  0  3  5  7  8 16 20 27 29 31 37 42 47 67 84 86

```



## DWScript


```delphi
procedure GnomeSort(a : array of Integer);
var
   i, j : Integer;
begin
   i := 1;
   j := 2;
   while i < a.Length do begin
      if a[i-1] <= a[i] then begin
         i := j;
         j := j + 1;
      end else begin
         a.Swap(i-1, i);
         i := i - 1;
         if i = 0 then begin
            i := j;
            j := j + 1;
         end;
      end;
   end;
end;

var i : Integer;
var a := new Integer[16];

Print('{');
for i := 0 to a.High do begin
   a[i] := i xor 5;
   Print(Format('%3d ', [a[i]]));
end;
PrintLn('}');

GnomeSort(a);

Print('{');
for i := 0 to a.High do
   Print(Format('%3d ', [a[i]]));
PrintLn('}');

```

Ouput :

```txt

{  5   4   7   6   1   0   3   2  13  12  15  14   9   8  11  10 }
{  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15 }

```



## E



```e
def gnomeSort(array) {
    var size := array.size()
    var i := 1
    var j := 2
    while (i < size) {
        if (array[i-1] <= array[i]) {
            i := j
            j += 1
        } else {
            def t := array[i-1]
            array[i-1] := array[i]
            array[i] := t
            i -= 1
            if (i <=> 0) {
                i := j
                j += 1
            }
        }
    }
}
```



```e
? def a := [7,9,4,2,1,3,6,5,0,8].diverge()
# value: [7, 9, 4, 2, 1, 3, 6, 5, 0, 8].diverge()

? gnomeSort(a)
? a
# value: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].diverge()
```



## Eiffel



```Eiffel

class
	GNOME_SORT [G -> COMPARABLE]

feature

	sort (ar: ARRAY [G]): ARRAY [G]
			-- Sorted array in ascending order.
		require
			array_not_void: ar /= Void
		local
			i, j: INTEGER
			ith: G
		do
			create Result.make_empty
			Result.deep_copy (ar)
			from
				i := 2
				j := 3
			until
				i > Result.count
			loop
				if Result [i - 1] <= Result [i] then
					i := j
					j := j + 1
				else
					ith := Result [i - 1]
					Result [i - 1] := Result [i]
					Result [i] := ith
					i := i - 1
					if i = 1 then
						i := j
						j := j + 1
					end
				end
			end
		ensure
			Same_length: ar.count = Result.count
			Result_is_sorted: is_sorted (Result)
		end

feature {NONE}

	is_sorted (ar: ARRAY [G]): BOOLEAN
			--- Is 'ar' sorted in ascending order?
		require
			ar_not_empty: ar.is_empty = False
		local
			i: INTEGER
		do
			Result := True
			from
				i := ar.lower
			until
				i = ar.upper
			loop
				if ar [i] > ar [i + 1] then
					Result := False
				end
				i := i + 1
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
			test := <<7, 99, -7, 1, 0, 25, -10>>
			io.put_string ("unsorted:%N")
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
			io.new_line
			io.put_string ("sorted:%N")
			create gnome
			test := gnome.sort (test)
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
		end

	test: ARRAY [INTEGER]

	gnome: GNOME_SORT [INTEGER]

end


```

{{out}}

```txt

Unsorted:
7    99    -7    1    0    25    -10
Sorted:
-7    -10    0    1    7    25    99

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;

extension op
{
    gnomeSort()
    {
        var list := self.clone();
        int i := 1;
        int j := 2;

        while (i < list.Length)
        {
            if (list[i-1]<=list[i])
            {
                i := j;
                j += 1
            }
            else
            {
                list.exchange(i-1,i);
                i -= 1;
                if (i==0)
                {
                    i := 1;
                    j := 2
                }
            }
        };

        ^ list
    }
}

public program()
{
    var list := new int[]::(3, 9, 4, 6, 8, 1, 7, 2, 5);

    console.printLine("before:", list.asEnumerable());
    console.printLine("after :", list.gnomeSort().asEnumerable())
}
```

{{out}}

```txt

before:3,9,4,6,8,1,7,2,5
after :1,2,3,4,5,6,7,8,9

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Sort do
  def gnome_sort([]), do: []
  def gnome_sort([h|t]), do: gnome_sort([h], t)

  defp gnome_sort(list, []), do: list
  defp gnome_sort([prev|p], [next|n]) when next > prev, do: gnome_sort(p, [next,prev|n])
  defp gnome_sort(p, [next|n]), do: gnome_sort([next|p], n)
end

IO.inspect Sort.gnome_sort([8,3,9,1,3,2,6])
```


{{out}}

```txt

[1, 2, 3, 3, 6, 8, 9]

```



## Erlang



```Erlang
-module(gnome_sort).
-export([gnome/1]).

gnome(L, []) -> L;
gnome([Prev|P], [Next|N]) when Next > Prev ->
	gnome(P, [Next|[Prev|N]]);
gnome(P, [Next|N]) ->
	gnome([Next|P], N).
gnome([H|T]) -> gnome([H], T).
```


```Erlang
Eshell V5.7.3  (abort with ^G)
1> c(gnome_sort).
{ok,gnome_sort}
2> gnome_sort:gnome([8,3,9,1,3,2,6]).
[1,2,3,3,6,8,9]
3>
```



## Euphoria


```euphoria
function gnomeSort(sequence s)
    integer i,j
    object temp
    i = 1
    j = 2
    while i < length(s) do
        if compare(s[i], s[i+1]) <= 0 then
            i = j
            j += 1
        else
            temp = s[i]
            s[i] = s[i+1]
            s[i+1] = temp
            i -= 1
            if i = 0 then
                i = j
                j += 1
            end if
        end if
    end while
    return s
end function
```


=={{header|F Sharp|F#}}==

```fsharp
let inline gnomeSort (a: _ []) =
  let rec loop i j =
    if i < a.Length then
      if a.[i-1] <= a.[i] then loop j (j+1) else
        let t = a.[i-1]
        a.[i-1] <- a.[i]
        a.[i] <- t
        if i=1 then loop j (j+1) else loop (i-1) j
  loop 1 2
```



## Factor

 USING: kernel math sequences ;
 IN: rosetta-code.gnome-sort

 : inc-pos ( pos seq -- pos' seq )
     [ 1 + ] dip ; inline

 : dec-pos ( pos seq -- pos' seq )
     [ 1 - ] dip ; inline

 : take-two ( pos seq -- elt-at-pos-1 elt-at-pos )
     [ dec-pos nth ] [ nth ] 2bi ; inline

 : need-swap? ( pos seq -- pos seq ? )
     over 1 < [ f ] [ 2dup take-two > ] if ;

 : swap-back ( pos seq -- pos seq' )
     [ take-two ] 2keep
     [ dec-pos set-nth ] 2keep
     [ set-nth ] 2keep ;

 : gnome-sort ( seq -- sorted-seq )
     1 swap [ 2dup length < ] [
         2dup [ need-swap? ] [ swap-back dec-pos ] while
         2drop inc-pos
     ] while nip ;
Example:
 IN: scratchpad '''USE: rosetta-code.gnome-sort'''
 Loading resource:extra/rosetta-code/gnome-sort/gnome-sort.factor
 IN: scratchpad '''V{ 10 9 5 7 4 3 6 8 1 2 } gnome-sort .'''
 V{ 1 2 3 4 5 6 7 8 9 10 }


## Fantom



```fantom

class Main
{
  Int[] gnomesort (Int[] list)
  {
    i := 1
    j := 2
    while (i < list.size)
    {
      if (list[i-1] <= list[i])
      {
        i = j
        j += 1
      }
      else
      {
        list.swap(i-1, i)
        i -= 1
        if (i == 0)
        {
          i = j
          j += 1
        }
      }
    }

    return list
  }

  Void main ()
  {
    list := [4,1,5,8,2,1,5,7]
    echo ("" + list + " sorted is " + gnomesort (list))
  }
}

```


Output:

```txt

[4, 1, 5, 8, 2, 1, 5, 7] sorted is [1, 1, 2, 4, 5, 5, 7, 8]

```



## Forth


```forth
defer precedes
defer exchange

: gnomesort                   ( a n)
  swap >r 1                   ( n c)
  begin                       ( n c)
    over over >               ( n c f)
  while                       ( n c)
    dup if                    ( n c)
      dup dup 1- over over r@ precedes
      if r@ exchange 1- else drop drop 1+ then
    else 1+ then              ( n c)
  repeat drop drop r> drop    ( --)
;

create example
  8 93 69 52 50 79 33 52 19 77 , , , , , , , , , ,

:noname >r cells r@ + @ swap cells r> + @ swap < ; is precedes
:noname >r cells r@ + swap cells r> + over @ over @ swap rot ! swap ! ; is exchange

: .array 10 0 do example i cells + ? loop cr ;

.array example 10 gnomesort .array
```

A slightly optimized version is presented below, where Gnome sort keeps track of its previous position. This reduces the number of iterations significantly.

```forth
: gnomesort+                           ( a n)
  swap >r 2 tuck 1-                    ( c2 n c1)
  begin                                ( c2 n c1)
    over over >                        ( c2 n c1 f)
  while                                ( c2 n c1)
    dup if                             ( c2 n c1)
      dup dup 1- over over r@ precedes
      if r@ exchange 1- else drop drop drop >r dup 1+ swap r> swap then
    else drop >r dup 1+ swap r> swap then
  repeat drop drop drop r> drop
;                                      ( --)
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program example

  implicit none

  integer :: array(8) = (/ 2, 8, 6, 1, 3, 5, 4, 7 /)

  call Gnomesort(array)
  write(*,*) array

contains

subroutine Gnomesort(a)

  integer, intent(in out) :: a(:)
  integer :: i, j, temp

  i = 2
  j = 3
  do while (i <= size(a))
    if (a(i-1) <= a(i)) then
      i = j
      j = j + 1
    else
      temp = a(i-1)
      a(i-1) = a(i)
      a(i) = temp
      i = i -  1
      if (i == 1) then
        i = j
        j = j + 1
      end if
    end if
  end do

end subroutine Gnomesort

end program example
```


## FreeBASIC

Used the task pseudo code as a base

```freebasic
' version 21-10-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

Sub gnomesort(gnome() As Long)
    ' sort from lower bound to the highter bound
    ' array's can have subscript range from -2147483648 to +2147483647
    Dim As Long lb = LBound(gnome)
    Dim As Long ub = UBound(gnome)
    Dim As Long i = lb +1, j = lb +2

    While i < (ub +1)
        ' replace "<=" with ">=" for downwards sort
        If gnome(i -1) <= gnome(i) Then
            i = j
            j += 1
        Else
            Swap gnome(i -1), gnome(i)
            i -= 1
            If i = lb Then
                i = j
                j += 1
            End If
        End If
    Wend

End Sub

' ------=< MAIN >=------

Dim As Long i, array(-7 To 7)

Dim As Long a = LBound(array), b = UBound(array)

Randomize Timer
For i = a To b : array(i) = i  : Next
For i = a To b ' little shuffle
    Swap array(i), array(Int(Rnd * (b - a +1)) + a)
Next

Print "unsort ";
For i = a To b : Print Using "####"; array(i); : Next : Print
gnomesort(array())  ' sort the array
Print "  sort ";
For i = a To b : Print Using "####"; array(i); : Next : Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
unsort    4  -5   5   1  -3  -1  -2  -6   0   7  -4   6   2  -7   3
  sort   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=d91a871bd9f43cd9644c89baa3ee861a Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short
Dim siCounti As Short = 1
Dim siCountj As Short = 2
Dim siToSort As Short[] = [249, 28, 111, 36, 171, 98, 29, 448, 44, 147, 154, 46, 102, 183, 24]

Print "To sort: - ";
GoSub Display

While siCounti < siToSort.Count
  If siToSort[siCounti - 1] <= siToSort[siCounti] Then
    siCounti = siCountj
    Inc siCountj
  Else
    Swap siToSort[siCounti - 1], siToSort[siCounti]
    Dec siCounti
    If siCounti = 0 Then
      siCounti = siCountj
      Inc siCountj
    Endif
  Endif
Wend

Print "Sorted: -  ";
GoSub Display

Return
'--------------------------------------------
Display:

For siCount = 0 To siToSort.Max
  Print Format(Str(siToSort[siCount]), "####");
  If siCount <> siToSort.max Then Print ",";
Next

Print
Return

End
```

Output:

```txt

To sort: -  249,  28, 111,  36, 171,  98,  29, 448,  44, 147, 154,  46, 102, 183,  24
Sorted: -    24,  28,  29,  36,  44,  46,  98, 102, 111, 147, 154, 171, 183, 249, 448

```



## Go


```go
package main

import "fmt"

func main() {
    a := []int{170, 45, 75, -90, -802, 24, 2, 66}
    fmt.Println("before:", a)
    gnomeSort(a)
    fmt.Println("after: ", a)
}

func gnomeSort(a []int) {
    for i, j := 1, 2; i < len(a); {
        if a[i-1] > a[i] {
            a[i-1], a[i] = a[i], a[i-1]
            i--
            if i > 0 {
                continue
            }
        }
        i = j
        j++
    }
}
```


More generic version that sorts anything that implements <code>sort.Interface</code>:

```go
package main

import (
  "sort"
  "fmt"
)

func main() {
    a := []int{170, 45, 75, -90, -802, 24, 2, 66}
    fmt.Println("before:", a)
    gnomeSort(sort.IntSlice(a))
    fmt.Println("after: ", a)
}

func gnomeSort(a sort.Interface) {
    for i, j := 1, 2; i < a.Len(); {
        if a.Less(i, i-1) {
            a.Swap(i-1, i)
            i--
            if i > 0 {
                continue
            }
        }
        i = j
        j++
    }
}
```



## Groovy

Solution:

```groovy
def makeSwap = { a, i, j = i+1 -> print "."; a[[j,i]] = a[[i,j]] }

def checkSwap = { list, i, j = i+1 -> [(list[i] > list[j])].find{ it }.each { makeSwap(list, i, j) } }

def gnomeSort = { input ->
    def swap = checkSwap.curry(input)
    def index = 1
    while (index < input.size()) {
        index += (swap(index-1) && index > 1) ? -1 : 1
    }
    input
}
```


Test:

```groovy
println (gnomeSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (gnomeSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
```


Output:

```txt
..............................................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
.........................................................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
```



## Haskell


```haskell
gnomeSort [] = []
gnomeSort (x:xs) = gs [x] xs
    where
	gs vv@(v:vs) (w:ws)
		| v<=w = gs (w:vv) ws
		| otherwise = gs vs (w:v:ws)
	gs [] (y:ys) = gs [y] ys
	gs xs [] = reverse xs
-- keeping the first argument of gs in reverse order avoids the deterioration into cubic behaviour
```



## Io

Naive version:

```io
List do(
    gnomeSortInPlace := method(
        idx := 1
        while(idx <= size,
            if(idx == 0 or at(idx) > at(idx - 1)) then(
                idx = idx + 1
            ) else(
                swapIndices(idx, idx - 1)
                idx = idx - 1
            )
        )
    self)
)

lst := list(5, -1, -4, 2, 9)
lst gnomeSortInPlace println # ==> list(-4, -1, 2, 5, 9)
```


Optimized version, storing the position before traversing back towards the begining of the list ([[wp:Gnome_sort#Optimization|Wikipedia]])

```io
List do(
    gnomeSortInPlace := method(
        idx1 := 1
        idx2 := 2

        while(idx1 <= size,
            if(idx1 == 0 or at(idx1) > at(idx1 - 1)) then(
                idx1 = idx2
                idx2 = idx2 + 1
            ) else(
                swapIndices(idx1, idx1 - 1)
                idx1 = idx1 - 1
            )
        )
    self)
)

lst := list(5, -1, -4, 2, 9)
lst gnomeSortInPlace println # ==> list(-4, -1, 2, 5, 9)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   demosort(gnomesort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure gnomesort(X,op)            #: return sorted list
local i,j

   op := sortop(op,X)                # select how and what we sort

   j := (i := 2) + 1                 # translation of pseudo code
   while i <= *X do {
      if op(X[i],X[i-1]) then {
         X[i] :=: X[i -:= 1]
         if i > 1 then next
      }
      j := (i := j) + 1
   }
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]]. The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.

Abbreviated sample output:
```txt
Sorting Demo using procedure gnomesort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>
100 PROGRAM "GnomeSrt.bas"
110 RANDOMIZE
120 NUMERIC ARRAY(-5 TO 12)
130 CALL INIT(ARRAY)
140 CALL WRITE(ARRAY)
150 CALL GNOMESORT(ARRAY)
160 CALL WRITE(ARRAY)
170 DEF INIT(REF A)
180   FOR I=LBOUND(A) TO UBOUND(A)
190     LET A(I)=RND(98)+1
200   NEXT
210 END DEF
220 DEF WRITE(REF A)
230   FOR I=LBOUND(A) TO UBOUND(A)
240     PRINT A(I);
250   NEXT
260   PRINT
270 END DEF
280 DEF GNOMESORT(REF A)
290   LET I=LBOUND(A)+1:LET J=I+1
300   DO WHILE I<=UBOUND(A)
310     IF A(I-1)<=A(I) THEN
320       LET I=J:LET J=J+1
330     ELSE
340       LET T=A(I-1):LET A(I-1)=A(I):LET A(I)=T
350       LET I=I-1
360       IF I=LBOUND(A) THEN LET I=J:LET J=J+1
370     END IF
380   LOOP
390 END DEF
```



## J

{{eff note|J|/:~}}

```J
position=: 0 {.@I.@, [
swap=: ] A.~ *@position * #@[ !@- <:@position
gnome=: swap~ 2 >/\ ]
gnomes=: gnome^:_
```


Note 1: this implementation of swap is actually rather silly, and will not work on long lists.  A better implementation would be:
```J
swap=: position (] {~ _1 0 + [)`(0 _1 + [)`]}^:(*@[) ]
```


Note 2: this implementation only works for domains where > is defined (in J, "greater than" only works on numbers).  To work around this issue, you could define:
```J
gt=:  -.@(-: /:~)@,&<
gnome=: swap~ 2 gt/\ ]
```


Note 3: this implementation does not return intermediate results.  If you want them, you could use
```J
gnomeps=: gnome^:a:
```


Note 4: gnomeps just shows intermediate results and does not show the gnome's position.  You can extract the gnome's position using an expression such as
```J
2 ~:/\ gnomeps
```
.


## Java

{{trans|C}}

```java
public static void gnomeSort(int[] a)
{
  int i=1;
  int j=2;

  while(i < a.length) {
    if ( a[i-1] <= a[i] ) {
      i = j; j++;
    } else {
      int tmp = a[i-1];
      a[i-1] = a[i];
      a[i--] = tmp;
      i = (i==0) ? j++ : i;
    }
  }
}
```



## JavaScript


```javascript
function gnomeSort(a) {
    function moveBack(i) {
        for( ; i > 0 && a[i-1] > a[i]; i--) {
            var t = a[i];
            a[i] = a[i-1];
            a[i-1] = t;
        }
    }
    for (var i = 1; i < a.length; i++) {
        if (a[i-1] > a[i]) moveBack(i);
    }
    return a;
}
```



## jq

{{works with | jq|1.4}}
This implementation adheres to the specification.
The array to be sorted, however, can be any JSON array.

```jq
# As soon as "condition" is true, then emit . and stop:
def do_until(condition; next):
  def u: if condition then . else (next|u) end;
  u;

# input: an array
def gnomeSort:
  def swap(i;j): .[i] as $x | .[i]=.[j] | .[j]=$x;

  length as $length
  # state: [i, j, ary]
  | [1, 2, .]
  | do_until( .[0] >= $length;
              .[0] as $i | .[1] as $j
              | .[2]
              # for descending sort, use >= for comparison
              | if .[$i-1] <= .[$i] then [$j, $j + 1, .]
                else swap( $i-1; $i)
                | ($i - 1) as $i
                | if $i == 0 then [$j, $j + 1, .]
                  else [$i, $j, .]
                  end
                end )
  | .[2];
```

'''Example''':

```jq
[(2|sqrt), [1], null, 1, 0.5, 2, 1, -3, {"a": "A"}] | gnomeSort
```

{{Out}}

```sh
$ jq -M -n -f Gnome_sort.jq
[
  null,
  -3,
  0.5,
  1,
  1,
  1.4142135623730951,
  2,
  [
    1
  ],
  {
    "a": "A"
  }
]
```



## Julia

{{works with|Julia|0.6}}


```julia
function gnomesort!(arr::Vector)
    i, j = 1, 2
    while i < length(arr)
        if arr[i] ≤ arr[i+1]
            i = j
            j += 1
        else
            arr[i], arr[i+1] = arr[i+1], arr[i]
            i -= 1
            if i == 0
                i = j
                j += 1
            end
        end
    end
    return arr
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", bogosort!(v))
```


{{out}}

```txt
# unordered: [9, -8, 0, 3, 2, 10, 5, 4, 5, 5]
 -> ordered: [-8, 0, 2, 3, 4, 5, 5, 5, 9, 10]
```



## Kotlin


```scala
// version 1.1.0

fun <T: Comparable<T>> gnomeSort(a: Array<T>, ascending: Boolean = true) {
    var i = 1
    var j = 2
    while (i < a.size)
        if (ascending && (a[i - 1] <= a[i]) ||
           !ascending && (a[i - 1] >= a[i]))
            i = j++
        else {
            val temp = a[i - 1]
            a[i - 1] = a[i]
            a[i--] = temp
            if (i == 0) i = j++
        }
}

fun main(args: Array<String>) {
    val array = arrayOf(100, 2, 56, 200, -52, 3, 99, 33, 177, -199)
    println("Original      : ${array.asList()}")
    gnomeSort(array)
    println("Sorted (asc)  : ${array.asList()}")
    gnomeSort(array, false)
    println("Sorted (desc) : ${array.asList()}")
}
```


{{out}}

```txt

Original      : [100, 2, 56, 200, -52, 3, 99, 33, 177, -199]
Sorted (asc)  : [-199, -52, 2, 3, 33, 56, 99, 100, 177, 200]
Sorted (desc) : [200, 177, 100, 99, 56, 33, 3, 2, -52, -199]

```



## Lua

Keep in mind that Lua arrays initial index is 1.

```lua
function gnomeSort(a)
    local i, j = 2, 3

    while i <= #a do
        if a[i-1] <= a[i] then
            i = j
            j = j + 1
        else
            a[i-1], a[i] = a[i], a[i-1] -- swap
            i = i - 1
            if i == 1 then -- 1 instead of 0
                i = j
                j = j + 1
            end
        end
    end
end
```

Example:

```lua
list = { 5, 6, 1, 2, 9, 14, 2, 15, 6, 7, 8, 97 }
gnomeSort(list)
for i, j in pairs(list) do
    print(j)
end
```



## Maple


```Maple
arr := Array([17,3,72,0,36,2,3,8,40,0]):
len := numelems(arr):
i := 2:
while (i <= len) do
	if (i=1 or arr[i] >= arr[i-1]) then
		i++:
	else
		temp:= arr[i]:
		arr[i] := arr[i-1]:
		arr[i-1] := temp:
		i--:
	end if:
end do:
arr;
```

{{Out|Output}}

```txt
[0,0,2,3,3,8,17,36,40,72]
```



## Mathematica


```Mathematica
gnomeSort[list_]:=Module[{i=2,j=3},
While[ i<=Length[[list]],
If[ list[[i-1]]<=list[[i]],
 i=j; j++;,
 list[[i-1;;i]]=list[[i;;i-1]];i--;];
If[i==1,i=j;j++;]
]]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function list = gnomeSort(list)

    i = 2;
    j = 3;

    while i <= numel(list)

        if list(i-1) <= list(i)
            i = j;
            j = j+1;
        else
            list([i-1 i]) = list([i i-1]); %Swap
            i = i-1;
            if i == 1
                i = j;
                j = j+1;
            end
        end %if

    end %while
end %gnomeSort
```


Sample Usage:

```MATLAB>>
 gnomeSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```



## MAXScript


```MAXScript
fn gnomeSort arr =
(
	local i = 2
	local j = 3
	while i <= arr.count do
	(
		if arr[i-1] <= arr[i] then
		(
			i = j
			j += 1
		)
		else
		(
			swap arr[i-1] arr[i]
			i -= 1
			if i == 1 then
			(
				i = j
				j += 1
			)
		)
	)
	return arr
)
```

Output:

```MAXScript

a = for i in 1 to 10 collect random 1 20
#(20, 10, 16, 2, 19, 12, 11, 3, 5, 16)
gnomesort a
#(2, 3, 5, 10, 11, 12, 16, 16, 19, 20)

```



## Metafont


```metafont
def gnomesort(suffix v)(expr n) =
begingroup save i, j, t;
  i := 1; j := 2;
  forever: exitif not (i < n);
    if v[i-1] <= v[i]:
      i := j; j := j + 1;
    else:
      t := v[i-1];
      v[i-1] := v[i];
      v[i] := t;
      i := i - 1;
      i := if i=0: j; j := j + 1 else: i fi;
    fi
  endfor
endgroup enddef;
```



```metafont
numeric a[];
for i = 0 upto 9: a[i] := uniformdeviate(40); message decimal a[i]; endfor
message char10;

gnomesort(a, 10);
for i = 0 upto 9: message decimal a[i]; endfor
end
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

import java.util.List

i1 = ArrayList(Arrays.asList([Integer(3), Integer(3), Integer(1), Integer(2), Integer(4), Integer(3), Integer(5), Integer(6)]))
say i1.toString
say gnomeSort(i1).toString

return

method isTrue public constant binary returns boolean
  return 1 == 1

method isFalse public constant binary returns boolean
  return \isTrue

method gnomeSort(a = String[], sAsc = isTrue) public constant binary returns String[]

  rl = String[a.length]
  al = List gnomeSort(Arrays.asList(a), sAsc)
  al.toArray(rl)

  return rl

method gnomeSort(a = List, sAsc = isTrue) public constant binary returns ArrayList

  sDsc = \sAsc -- Ascending/descending switches
  ra = ArrayList(a)
  i_ = 1
  j_ = 2
  loop label i_ while i_ < ra.size
    ctx = (Comparable ra.get(i_ - 1)).compareTo(Comparable ra.get(i_))
    if (sAsc & ctx <= 0) | (sDsc & ctx >= 0) then do
      i_ = j_
      j_ = j_ + 1
      end
    else do
      swap = ra.get(i_)
      ra.set(i_, ra.get(i_ - 1))
      ra.set(i_ - 1, swap)
      i_ = i_ - 1
      if i_ = 0 then do
        i_ = j_
        j_ = j_ + 1
        end
      end
    end i_

  return ra

```

;Output

```txt

[3, 3, 1, 2, 4, 3, 5, 6]
[1, 2, 3, 3, 3, 4, 5, 6]

```



## Nim


```nim
proc gnomeSort[T](a: var openarray[T]) =
  var
    n = a.len
    i = 1
    j = 2
  while i < n:
    if a[i-1] > a[i]:
      swap a[i-1], a[i]
      dec i
      if i > 0: continue
    i = j
    inc j

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
gnomeSort a
echo a
```

Output:

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## Objeck

{{trans|C}}

```objeck

function : GnomeSort(a : Int[]) {
  i := 1;
  j := 2;
 
  while(i < a->Size()) {
    if (a[i-1] <= a[i]) {
      i := j;
      j += 1;
    }
    else {
      tmp := a[i-1];
      a[i-1] := a[i];
      a[i] := tmp;
      i -= 1;
      if(i = 0) {
        i := j;
        j += 1;
      };
    };
  };
}

```



## OCaml

from the toplevel:

```ocaml
# let gnome_sort a =
    let i = ref 1
    and j = ref 2 in
    while !i < Array.length a do
      if a.(!i-1) <= a.(!i) then
      begin
        i := !j;
        j := !j + 1;
      end else begin
        swap a (!i-1) (!i);
        i := !i - 1;
        if !i = 0 then begin
          i := !j;
          j := !j + 1;
        end;
      end;
    done;
  ;;
val gnome_sort : 'a array -> unit = <fun>

# let a = [| 7; 9; 4; 2; 1; 3; 6; 5; 0; 8; |] ;;
val a : int array = [|7; 9; 4; 2; 1; 3; 6; 5; 0; 8|]

# gnome_sort a ;;
- : unit = ()

# a ;;
- : int array = [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|]
```



## Octave


```octave
function s = gnomesort(v)
  i = 2; j = 3;
  while ( i <= length(v) )
    if ( v(i-1) <= v(i) )
      i = j;
      j++;
    else
      t = v(i-1);
      v(i-1) = v(i);
      v(i) = t;
      i--;
      if ( i == 1 )
	i = j;
	j++;
      endif
    endif
  endwhile
  s = v;
endfunction
```



```octave
v = [7, 9, 4, 2, 1, 3, 6, 5, 0, 8];
disp(gnomesort(v));
```



## ooRexx


### version 1

{{Trans|NetRexx}}

```ooRexx
/* Rexx */

call demo
return
exit

-- -----------------------------------------------------------------------------
--  Gnome sort implementation
-- -----------------------------------------------------------------------------
::routine gnomeSort
  use arg ra, sAsc = ''
  if sAsc = '' then sAsc = isTrue()

  sDsc = \sAsc -- Ascending/descending switches
  i_ = 1
  j_ = 2
  loop label i_ while i_ < ra~items()
    ctx = (ra~get(i_ - 1))~compareTo(ra~get(i_))
    if (sAsc & ctx <= 0) | (sDsc & ctx >= 0) then do
      i_ = j_
      j_ = j_ + 1
      end
    else do
      swap = ra~get(i_)
      ra~set(i_, ra~get(i_ - 1))
      ra~set(i_ - 1, swap)
      i_ = i_ - 1
      if i_ = 0 then do
        i_ = j_
        j_ = j_ + 1
        end
      end
    end i_

  return ra

-- -----------------------------------------------------------------------------
-- Demonstrate the implementation
-- -----------------------------------------------------------------------------
::routine demo
  placesList = .nlist~of( -
      "UK  London",     "US  New York",   "US  Boston",     "US  Washington" -
    , "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"     -
  )

  lists = .nlist~of( -
      placesList -
    , gnomeSort(placesList~copy()) -
  )

  loop ln = 0 to lists~items() - 1
    cl = lists[ln]
    loop ct = 0 to cl~items() - 1
      say right(ct + 1, 3)':' cl[ct]
      end ct
    say
    end ln

  i_.0 = 3
  i_.1 = .nlist~of(3, 3, 1, 2, 4, 3, 5, 6)
  i_.2 = gnomeSort(i_.1~copy(), isTrue())
  i_.3 = gnomeSort(i_.1~copy(), isFalse())
  loop s_ = 1 to i_.0
    ss = ''
    loop x_ = 0 to i_.s_~items() - 1
      ss ||= right(i_.s_~get(x_), 3)' '
      end x_
    say strip(ss, 'T')
    end s_

  return

-- -----------------------------------------------------------------------------
::routine isTrue
  return 1 == 1

-- -----------------------------------------------------------------------------
::routine isFalse
  return \isTrue()

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

'''Output:

```txt

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

  3   3   1   2   4   3   5   6
  1   2   3   3   3   4   5   6
  6   5   4   3   3   3   2   1

```



### version 2

{{trans|REXX}}

```oorexx
/* REXX ---------------------------------------------------------------
* 28.06.2014 Walter Pachl
* 30.06.2014 corrected in sync with REXX version 2
*--------------------------------------------------------------------*/
  Call gen                         /* generate the array elements.   */
  Call show 'before sort'          /* show  "before"  array elements.*/
  Call gnomeSort x                 /* invoke the infamous gnome sort.*/
  Call show ' after sort'          /* show  "after"   array elements.*/
  Exit                             /* done.                          */

gnomesort: Procedure
  Use Arg x
  n=x~items
  k=2
  Do j=3 While k<=n
    km=k-1
    If x[km]<=x[k] Then
      k=j
    Else Do
      t=x[km]; x[km]=x[k]; x[k]=t  /* swap two entries in the array. */
      k=k-1
      If k==1 Then
        k=j
      Else
        j=j-1
      End
    End
  Return

gen:
  x=.array~of('---the seven virtues---','
### =================
',,
              'Faith','Hope','Charity  [Love]','Fortitude','  Justice',,
              'Prudence','Temperance')
  Return
show:
  Say arg(1)
  Do i=1 To x~items
    Say 'element' right(i,2) x[i]
    End
  Return
```

'''output'''
Similar to REXX version 2


## Oz

{{trans|Haskell}}

```oz
declare
  fun {GnomeSort Xs}
     case Xs of nil then nil
     [] X|Xr then {Loop [X] Xr}
     end
  end

  fun {Loop Vs Ws}
     case [Vs Ws]
     of [V|_  W|Wr] andthen V =< W then {Loop W|Vs Wr}
     [] [V|Vr W|Wr] then {Loop Vr W|V|Wr}
     [] [nil  W|Wr] then {Loop [W] Wr}
     [] [Vs   nil ] then {Reverse Vs}
     end
  end
in
  {Show {GnomeSort [3 1 4 1 5 9 2 6 5]}}
```



## PARI/GP


```parigp
gnomeSort(v)={
  my(i=2,j=3,n=#v,t);
  while(i<=n,
    if(v[i-1]>v[i],
      t=v[i];
      v[i]=v[i-1];
      v[i--]=t;
      if(i==1, i=j; j++);
    ,
      i=j;
      j++
    )
  );
  v
};
```



## Pascal


```pascal
procedure gnomesort(var arr : Array of Integer; size : Integer);
var
   i, j, t	: Integer;
begin
   i := 1;
   j := 2;
   while i < size do begin
      if arr[i-1] <= arr[i] then
      begin
	 i := j;
	 j := j + 1
      end
      else begin
	 t := arr[i-1];
	 arr[i-1] := arr[i];
	 arr[i] := t;
	 i := i - 1;
	 if i = 0 then begin
	    i := j;
	    j := j + 1
	 end
      end
   end;
end;
```



## Perl


```perl
use strict;

sub gnome_sort
{
    my @a = @_;

    my $size = scalar(@a);
    my $i = 1;
    my $j = 2;
    while($i < $size) {
	if ( $a[$i-1] <= $a[$i] ) {
	    $i = $j;
	    $j++;
	} else {
	    @a[$i, $i-1] = @a[$i-1, $i];
	    $i--;
	    if ($i == 0) {
		$i = $j;
		$j++;
	    }
	}
    }
    return @a;
}
```



```perl
my @arr = ( 10, 9, 8, 5, 2, 1, 1, 0, 50, -2 );
print "$_\n" foreach gnome_sort( @arr );
```



## Perl 6

{{Works with|rakudo|2016.03}}


```perl6
sub gnome_sort (@a) {
    my ($i, $j) = 1, 2;
    while $i < @a {
        if @a[$i - 1] <= @a[$i] {
            ($i, $j) = $j, $j + 1;
        }
        else {
            (@a[$i - 1], @a[$i]) = @a[$i], @a[$i - 1];
            $i--;
            ($i, $j) = $j, $j + 1 if $i == 0;
        }
    }
}
```


my @n = (1..10).roll(20);
say @n.&gnome_sort ~~ @n.sort ?? 'ok' !! 'not ok';


## Phix

Copy of [[Sorting_algorithms/Gnome_sort#Euphoria|Euphoria]]

```Phix
function gnomeSort(sequence s)
integer i = 1, j = 2
    while i<length(s) do
        if s[i]<=s[i+1] then
            i = j
            j += 1
        else
            {s[i],s[i+1]} = {s[i+1],s[i]}
            i -= 1
            if i = 0 then
                i = j
                j += 1
            end if
        end if
    end while
    return s
end function

?gnomeSort(shuffle(tagset(10)))
```



## PHP


```php
function gnomeSort($arr){
	$i = 1;
	$j = 2;
	while($i < count($arr)){
		if ($arr[$i-1] <= $arr[$i]){
			$i = $j;
			$j++;
		}else{
			list($arr[$i],$arr[$i-1]) = array($arr[$i-1],$arr[$i]);
			$i--;
			if($i == 0){
				$i = $j;
				$j++;
			}
		}
	}
	return $arr;
}
$arr = array(3,1,6,2,9,4,7,8,5);
echo implode(',',gnomeSort($arr));
```


```txt
1,2,3,4,5,6,7,8,9
```



## PicoLisp


```PicoLisp
(de gnomeSort (Lst)
   (let J (cdr Lst)
      (for (I Lst (cdr I))
         (if (>= (cadr I) (car I))
            (setq I J  J (cdr J))
            (xchg I (cdr I))
            (if (== I Lst)
               (setq I J  J (cdr J))
               (setq I (prior I Lst)) ) ) ) )
   Lst )
```




## PL/I


```PL/I
SORT: PROCEDURE OPTIONS (MAIN);
   DECLARE A(0:9) FIXED STATIC INITIAL (5, 2, 7, 1, 9, 8, 6, 3, 4, 0);

   CALL GNOME_SORT (A);
   put skip edit (A) (f(7));

GNOME_SORT: PROCEDURE (A) OPTIONS (REORDER); /* 9 September 2015 */
   declare A(*) fixed;
   declare t fixed;
   declare (i, j) fixed;

   i = 1; j = 2;
   do while (i <= hbound(A));
      if a(i-1) <= a(i) then
         do;
            i = j; j = j + 1;
         end;
      else
         do;
            t = a(i-1); a(i-1) = a(i); a(i) = t;
            i = i - 1;
            if i = 0 then do; i = j; j = j + 1; end;
         end;
   end;

END GNOME_SORT;

END SORT;
```

Results:

```txt

      0      1      2      3      4      5      6      7      8      9

```



## PowerBASIC


The [[#BASIC|BASIC]] example will work as-is if the array is declared in the same function as the sort. This example doesn't require that, but forces you to know your data type beforehand.


```powerbasic
SUB gnomeSort (a() AS LONG)
    DIM i AS LONG, j AS LONG
    i = 1
    j = 2
    WHILE (i < UBOUND(a) + 1)
        IF (a(i - 1) <= a(i)) THEN
            i = j
            INCR j
        ELSE
            SWAP a(i - 1), a(i)
            DECR i
            IF 0 = i THEN
                i = j
                INCR j
            END IF
        END IF
    WEND
END SUB

FUNCTION PBMAIN () AS LONG
    DIM n(9) AS LONG, x AS LONG
    RANDOMIZE TIMER
    OPEN "output.txt" FOR OUTPUT AS 1
    FOR x = 0 TO 9
        n(x) = INT(RND * 9999)
        PRINT #1, n(x); ",";
    NEXT
    PRINT #1,
    gnomeSort n()
    FOR x = 0 TO 9
        PRINT #1, n(x); ",";
    NEXT
    CLOSE 1
END FUNCTION
```


Sample output:
 7426 , 7887 , 8297 , 2671 , 7586 , 7160 , 1195 , 665 , 9352 , 6199 ,
 665 , 1195 , 2671 , 6199 , 7160 , 7426 , 7586 , 7887 , 8297 , 9352 ,


## PowerShell


```PowerShell

function gnomesort($a) {
    $size, $i, $j = $a.Count, 1, 2
    while($i -lt $size) {
        if ($a[$i-1] -le $a[$i]) {
            $i = $j
            $j++
        }
        else {
            $a[$i-1], $a[$i] = $a[$i], $a[$i-1]
            $i--
            if($i -eq 0) {
                $i = $j
                $j++
            }
        }
    }
    $a
}
$array = @(60, 21, 19, 36, 63, 8, 100, 80, 3, 87, 11)
"$(gnomesort $array)"

```

<b>Output:</b>

```txt

3 8 11 19 21 36 60 63 80 87 100

```



## PureBasic


```PureBasic
Procedure GnomeSort(Array a(1))
  Protected Size = ArraySize(a()) + 1
  Protected i = 1, j = 2

  While i < Size
    If a(i - 1) <= a(i)
      ;for descending SORT, use >= for comparison
      i = j
      j + 1
    Else
      Swap a(i - 1), a(i)
      i - 1
      If i = 0
        i = j
        j + 1
      EndIf
    EndIf
  Wend
EndProcedure
```



## Python


```python>>>
 def gnomesort(a):
	i,j,size = 1,2,len(a)
	while i < size:
		if a[i-1] <= a[i]:
			i,j = j, j+1
		else:
			a[i-1],a[i] = a[i],a[i-1]
			i -= 1
			if i == 0:
				i,j = j, j+1
	return a

>>> gnomesort([3,4,2,5,1,6])
[1, 2, 3, 4, 5, 6]
>>>
```



## R


```r
gnomesort <- function(x)
{
   i <- 1
   j <- 1
   while(i < length(x))
   {
      if(x[i] <= x[i+1])
      {
         i <- j
         j <- j+1
      } else
      {
         temp <- x[i]
         x[i] <- x[i+1]
         x[i+1]  <- temp
         i <- i - 1
         if(i == 0)
         {
            i <- j
            j <- j+1
         }
      }
   }
   x
}
gnomesort(c(4, 65, 2, -31, 0, 99, 83, 782, 1)) # -31   0   1   2   4  65  83  99 782
```



## Racket


```racket

#lang racket

;; Translation of the pseudo code
(define (gnome-sort1 a <=?)
  (define size (vector-length a))
  (define (swap i j)
    (define t (vector-ref a i))
    (vector-set! a i (vector-ref a j))
    (vector-set! a j t))
  (let loop ([i 1] [j 2])
    (when (< i size)
      (if (<=? (vector-ref a (sub1 i)) (vector-ref a i))
        (loop j (add1 j))
        (begin (swap (sub1 i) i)
               (let ([i (sub1 i)])
                 (if (zero? i)
                   (loop j (add1 j))
                   (loop i j)))))))
  a)
(gnome-sort1 (vector 3 2 1 4 5 6) <=)

;; a functional version, roughly like the Scheme entry
(define (gnome-sort2 l <=?)
  (match l
    [(list) l]
    [(list x xs ...)
     (let loop ([x `((,x) . ,xs)])
       (match x
         [`(,ps) ps]
         [`((,p . ,ps) ,n . ,ns)
          (loop (cond [(<=? n p)  `((,n ,p . ,ps) . ,ns)]
                      [(null? ps) `((,n) ,p . ,ns)]
                      [else       `(,ps ,n ,p . ,ns)]))]))]))
(gnome-sort2 '(3 2 1 4 5 6) <=)

```



## Rascal


```rascal
import List;

public list[int] gnomeSort(a){
	i = 1;
	j = 2;
	while(i < size(a)){
		if(a[i-1] <= a[i]){
			i = j;
			j += 1;}
		else{
			temp = a[i-1];
			a[i-1] = a[i];
			a[i] = temp;
			i = i - 1;
			if(i == 0){
				i = j;
				j += 1;}}}
	return a;
}
```


An example output:


```rascal
gnomeSort([4, 65, 2, -31, 0, 99, 83, 782, 1])
list[int]: [-31,0,1,2,4,65,83,99,782]
```



## REXX


### version 1

This REXX version uses an unity-based array   (instead of a zero-based array).

```rexx
/*REXX program sorts an array using the gnome sort algorithm (elements contain blanks). */
call  gen                                        /*generate the  @   stemmed array.     */
call  show         'before sort'                 /*display the   before  array elements.*/
say copies('▒', 60)                              /*show a separator line between sorts. */
call  gnomeSort     #                            /*invoke the well─known  gnome  sort.  */
call  show         ' after sort'                 /*display the    after  array elements.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen: @.=;  @.1= '---the seven virtues---';    @.4= "Hope"           ;    @.7= 'Justice'
           @.2= '
### =================
';    @.5= "Charity  [Love]";    @.8= 'Prudence'
           @.3= 'Faith'                  ;    @.6= "Fortitude"      ;    @.9= 'Temperance'
             do #=1  while @.#\==''; end;   #= #-1;   w= length(#);  return /*get #items*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
gnomeSort: procedure expose @.;  parse arg n;    k= 2             /*N: is number items. */
                  do j=3  while k<=n;            p= k - 1         /*P: is previous item.*/
                  if @.p<<=@.k  then do;  k= j;  iterate;   end   /*order is OK so far. */
                  _= @.p;       @.p= @.k;        @.k= _           /*swap two @ entries. */
                  k= k-1;       if k==1  then k=j;  else j= j-1   /*test for 1st index. */
                  end    /*j*/
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  do j=1  for #;  say '       element'  right(j, w)  arg(1)":"  @.j;   end;    return
```

{{out|output|:}}

```txt

       element 1 before sort: ---the seven virtues---
       element 2 before sort:
### =================

       element 3 before sort: Faith
       element 4 before sort: Hope
       element 5 before sort: Charity  [Love]
       element 6 before sort: Fortitude
       element 7 before sort: Justice
       element 8 before sort: Prudence
       element 9 before sort: Temperance
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
       element 1  after sort: ---the seven virtues---
       element 2  after sort:
### =================

       element 3  after sort: Charity  [Love]
       element 4  after sort: Faith
       element 5  after sort: Fortitude
       element 6  after sort: Hope
       element 7  after sort: Justice
       element 8  after sort: Prudence
       element 9  after sort: Temperance

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 28.06.2014 Walter Pachl  cf ooRexx version 2
* only style changes (reformatting) and adapt for ooRexx compatibility
* NOTE that leading blanks are ignored in the comparison ('  Justice')
* unless strict comparison is used  (i.e., 'If x.km<<=x.k Then')
* 30.06.2014 WP added the missing else clause
*--------------------------------------------------------------------*/
                                   /* generate the array elements.   */
  Call gen '---the seven virtues---','
### =================
',,
           'Faith','Hope','Charity  [Love]','Fortitude','  Justice',,
           'Prudence','Temperance'
  Call show 'before sort'          /* show  "before"  array elements.*/
  Call gnomeSort                   /* invoke the infamous gnome sort.*/
  Call show ' after sort'          /* show  "after"   array elements.*/
  Exit                             /* done.                          */

gnomesort: Procedure Expose x.
  k=2
  Do j=3 While k<=x.0
    km=k-1
    If x.km<=x.k Then
      k=j
    Else Do
      t=x.km; x.km=x.k; x.k=t      /* swap two entries in the array. */
      k=k-1
      If k==1 Then
        k=j
      Else
        j=j-1
      End
    End
  Return

gen: Procedure Expose x.
  Do i=1 To arg()
    x.i=arg(i)
    End
  x.0=arg()
  Return

show:  Procedure Expose x.
  Say arg(1)
  Do i=1 To x.0
    Say 'element' right(i,2) x.i
    End
  Return

```

'''output'''

```txt
before sort
element  1 ---the seven virtues---
element  2
### =================

element  3 Faith
element  4 Hope
element  5 Charity  [Love]
element  6 Fortitude
element  7   Justice
element  8 Prudence
element  9 Temperance
 after sort
element  1 ---the seven virtues---
element  2
### =================

element  3 Charity  [Love]
element  4 Faith
element  5 Fortitude
element  6 Hope
element  7   Justice
element  8 Prudence
element  9 Temperance
```



## Ring


```ring

aList = [ 5, 6, 1, 2, 9, 14, 15, 7, 8, 97]
gnomeSort(aList)
for i=1 to len(aList)
    see "" + aList[i] + " "
next

func gnomeSort a
     i = 2
     j = 3
     while i < len(a)
           if a[i-1] <= a[i]
              i = j
              j = j + 1
           else
              temp = a[i-1]
              a[i-1] = a[i]
              a[i] = temp
              i = i - 1
              if i = 1
                 i = j
                 j = j + 1 ok ok end

```



## Ruby


```ruby
class Array
  def gnomesort!
    i, j = 1, 2
    while i < length
      if self[i-1] <= self[i]
        i, j = j, j+1
      else
        self[i-1], self[i] = self[i], self[i-1]
        i -= 1
        if i == 0
          i, j = j, j+1
        end
      end
    end
    self
  end
end
ary = [7,6,5,9,8,4,3,1,2,0]
ary.gnomesort!
# => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Scala


```Scala
object GnomeSort {
  def gnomeSort(a: Array[Int]): Unit = {
    var (i, j) = (1, 2)
    while ( i < a.length)
      if (a(i - 1) <= a(i)) { i = j; j += 1 }
    else {
      val tmp = a(i - 1)
      a(i - 1) = a(i)
      a({i -= 1; i + 1}) = tmp
      i = if (i == 0) {j += 1; j - 1} else i
    }
  }
}
```


## Scheme


```scheme
; supply comparison function, which returns true if first and second
; arguments are in order or equal.
(define (gnome-sort-compar in-order input-list)
  (let gnome ((p (list (car input-list)))
              (n (cdr input-list)))
    (if (null? n) ; no more flowerpots?
        p ; we're done
        (let ((prev-pot (car p))
              (next-pot (car n)))
          (if (in-order next-pot prev-pot)
              ; if the pots are in order, step forwards.
              ; otherwise, exchange the two pots, and step backwards.
              (gnome (cons next-pot p) ; Prev list grows
                     (cdr n)) ; Next list shorter by one
              (if (null? (cdr p)) ; are we at the beginning?
                  (gnome                    ; if so, we can't step back
                   (list next-pot)          ; simply exchange the pots without
                   (cons prev-pot (cdr n))) ; changing lengths of lists
                  (gnome
                   (cdr p) ; Prev list shorter by one
                   (cons next-pot (cons prev-pot (cdr n))))))))))
```

 #;1> (gnome-sort-compar <= '(98 36 2 78 5 81 32 90 73 21 94 28 53 25 10 99))
 (2 5 10 21 25 28 32 36 53 73 78 81 90 94 98 99)


## Sidef


```ruby
class Array {
    method gnomesort {
        var (i=1, j=2);
        var len = self.len;
        while (i < len) {
            if (self[i-1] <= self[i]) {
                (i, j) = (j, j+1);
            }
            else {
                self[i-1, i] = self[i, i-1];
                if (--i == 0) {
                    (i, j) = (j, j+1);
                }
            }
        }
        return self;
    }
}

var ary = [7,6,5,9,8,4,3,1,2,0];
say ary.gnomesort;
```

{{out}}

```txt
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Smalltalk



```smalltalk
Smalltalk at: #gnomesort put: nil.

"Utility"
OrderedCollection extend [
  swap: a with: b [ |t|
      t := self at: a.
      self at: a put: b.
      self at: b put: t
  ]
].

"Gnome sort as block closure"
gnomesort := [ :c |
  |i j|
  i := 2. j := 3.
  [ i <= (c size) ]
    whileTrue: [
       (c at: (i-1)) <= (c at: i)
          ifTrue: [
             i := j copy.
             j := j + 1.
          ]
          ifFalse: [
             c swap: (i-1) with: i.
             i := i - 1.
             i == 1
               ifTrue: [
                  i := j copy.
                  j := j + 1
               ]
          ]
    ]
].
```


'''Testing'''


```smalltalk
|r o|
r := Random new.
o := OrderedCollection new.

1 to: 10 do: [ :i | o add: (r between: 0 and: 50) ].

gnomesort value: o.

1 to: 10 do: [ :i | (o at: i) displayNl ].
```



## Tcl

{{tcllib|struct::list}}

```tcl
package require Tcl 8.5
package require struct::list

proc gnomesort {a} {
    set i 1
    set j 2
    set size [llength $a]
    while {$i < $size} {
        if {[lindex $a [expr {$i - 1}]] <= [lindex $a $i]} {
            set i $j
            incr j
        } else {
            struct::list swap a [expr {$i - 1}] $i
            incr i -1
            if {$i == 0} {
                set i $j
                incr j
            }
        }
    }
    return $a
}

puts [gnomesort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
```


=={{header|TI-83 BASIC}}==
Store input into L<sub>1</sub>, run prgmSORTGNOM, output will be in L<sub>2</sub>.
 :1→P
 :L<sub>1</sub>→L<sub>2</sub>
 :While P<dim(L<sub>2</sub>)
 :If PP=1
 :Then
 :P+1→P
 :Else
 :If L<sub>2</sub>(P)≥L<sub>2</sub>(P-1)
 :Then
 :P+1→P
 :Else
 :L<sub>2</sub>(P)→Q
 :L<sub>2</sub>(P-1)→L<sub>2</sub>(P)
 :Q→L<sub>2</sub>(P-1)
 :P-1→P
 :End
 :End
 :End
 :If L<sub>2</sub>(dim(L<sub>2</sub>))<L<sub>2</sub>(dim(L<sub>2</sub>)-1)
 :Then
 :L<sub>2</sub>(dim(L<sub>2</sub>))→Q
 :L<sub>2</sub>(dim(L<sub>2</sub>)-1)→L<sub>2</sub>(dim(L<sub>2</sub>))
 :Q→L<sub>2</sub>(dim(L<sub>2</sub>)-1)
 :End
 :DelVar P
 :DelVar Q
 :Return


## uBasic/4tH

<lang>PRINT "Gnome sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Gnomesort (n)
  PROC _ShowArray (n)
PRINT

END


_Gnomesort PARAM (1)                   ' Gnome sort
  LOCAL (2)
  b@=1
  c@=2

  DO WHILE b@ < a@
    IF @(b@-1) > @(b@) THEN
      PROC _Swap (b@, b@-1)
      b@ = b@ - 1
      IF b@ THEN
        CONTINUE
      ENDIF
    ENDIF
    b@ = c@
    c@ = c@ + 1
  LOOP
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


## Ursala

The function is parameterized by a relational predicate and
operates on a list. The algorithm is to scan forward until
finding an item out of order, bubble it backwards to its
proper position and resume scanning forward from where it was.

```Ursala
gnome_sort "p" =

@NiX ^=lx -+                                                                     # iteration
   ~&r?\~& @lNXrX ->llx2rhPlrPCTxPrtPX~&lltPlhPrCXPrX ~&ll&& @llPrXh not "p",    # backward bubble
   ->~&rhPlCrtPX ~&r&& ~&lZ!| @bh "p"+-                                          # forward scan
```

Most of the code is wasted on dull but unavoidable stack manipulations.
Here is a test program using the lexical partial order relation.

```Ursala
#import std
#cast %s

t = gnome_sort(lleq) 'dfijhkwlawfkjnksdjnoqwjefgflkdsgioi'
```

output:

```txt

'adddeffffgghiiijjjjkkkkllnnooqsswww'

```


## VBA

{{trans|Phix}}
```vb
Private Function gnomeSort(s As Variant) As Variant
    Dim i As Integer: i = 1
    Dim j As Integer: j = 2
    Dim tmp As Integer
    Do While i < UBound(s)
        If s(i) <= s(i + 1) Then
            i = j
            j = j + 1
        Else
            tmp = s(i)
            s(i) = s(i + 1)
            s(i + 1) = tmp
            i = i - 1
            If i = 0 Then
                i = j
                j = j + 1
            End If
        End If
    Loop
    gnomeSort = s
End Function

Public Sub main()
    Debug.Print Join(gnomeSort([{5, 3, 1, 7, 4, 1, 1, 20}]), ", ")
End Sub
```
{{out}}

```txt
1, 1, 1, 3, 4, 5, 7, 20
```


## VBScript


### =Implementation=


```vb
function gnomeSort( a )
	dim i
	dim j
	i = 1
	j = 2
	do while i < ubound( a ) + 1
		if a(i-1) <= a(i) then
			i = j
			j = j + 1
		else
			swap a(i-1), a(i)
			i = i - 1
			if i = 0 then
				i = j
				j = j + 1
			end if
		end if
	loop
	gnomeSort = a
end function

sub swap( byref x, byref y )
	dim temp
	temp = x
	x = y
	y = temp
end sub
```


### =Invocation=


```vb
dim a
dim b

a = array( "zanzibar", "aardvark","ampicillin","zulu","gogodala", "wolverhampton")
b = gnomeSort( a )
wscript.echo join(a, ", ")

a = array( 234,567,345,568,2345,89,547,23,649,5769,456,456,567)
b = gnomeSort( a )
wscript.echo join(a, ", ")

a = array( true, false, true, true, false, false, true, false)
b = gnomeSort( a )
wscript.echo join(a, ", ")

a = array( 1,2,2,4,67789,-3,-45.99)
b = gnomeSort( a )
wscript.echo join(a, ", ")

a = array( now(), now()-1,now()+2)
b = gnomeSort( a )
wscript.echo join(a, ", ")
```



### =Output=


```VBScript
aardvark, ampicillin, gogodala, wolverhampton, zanzibar, zulu
23, 89, 234, 345, 456, 456, 547, 567, 567, 568, 649, 2345, 5769
True, True, True, True, False, False, False, False
-45.99, -3, 1, 2, 2, 4, 67789
2/02/2010 10:19:51 AM, 3/02/2010 10:19:51 AM, 5/02/2010 10:19:51 AM
```

Note: All data in VBScript is of type Variant. Thus the code can sort many different types of data without code modification.


## XPL0


```XPL0
code ChOut=8, IntOut=11;

proc GnomeSort(A(0..Size-1));   \Sort array A
int  A, Size;
int  I, J, T;
[I:= 1;
J:= 2;
while I < Size do
    if A(I-1) <= A(I) then
        [\ for descending sort, use >= for comparison
        I:= J;
        J:= J+1;
        ]
    else
        [T:= A(I-1);  A(I-1):= A(I);  A(I):= T; \swap
        I:= I-1;
        if I = 0 then
            [I:= J;
            J:= J+1;
            ];
        ];
];

int A, I;
[A:= [3, 1, 4, 1, -5, 9, 2, 6, 5, 4];
GnomeSort(A, 10);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
]
```


{{out}}

```txt

-5 1 1 2 3 4 4 5 6 9

```



## zkl


```zkl
fcn gnomeSort(a){
   i,j,size := 1,2,a.len();
   while(i < size){
      if(a[i-1] > a[i]){ // for descending sort, use < for comparison
	 a.swap(i-1,i);
	 i = i - 1;
	 if(i) continue;
      }
      i = j;
      j = j + 1;
   }//while
   a
}
```


```zkl
gnomeSort("This is a test".split("")).println();
```

{{out}}

```txt
L(" "," "," ","T","a","e","h","i","i","s","s","s","t","t")
```


{{omit from|GUISS}}

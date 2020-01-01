+++
title = "Sorting algorithms/Comb sort"
description = ""
date = 2019-10-18T20:10:38Z
aliases = []
[extra]
id = 5382
[taxonomies]
categories = []
tags = []
+++

{{clarified-review}}
{{task|Sorting Algorithms}}
{{Sorting Algorithm}}

;Task:
Implement a   ''comb sort''.


The '''Comb Sort''' is a variant of the [[Bubble Sort]].

Like the [[Shell sort]], the Comb Sort increases the gap used in comparisons and exchanges.

Dividing the gap by   <big><math>(1-e^{-\varphi})^{-1} \approx 1.247330950103979</math> </big>   works best, but   <big> 1.3</big>   may be more practical.


Some implementations use the insertion sort once the gap is less than a certain amount.


;Also see:
*   the Wikipedia article:   [[wp:Comb sort|Comb sort]].


Variants:
* Combsort11 makes sure the gap ends in (11, 8, 6, 4, 3, 2, 1), which is significantly faster than the other two possible endings.
* Combsort with different endings changes to a more efficient sort when the data is almost sorted (when the gap is small).   Comb sort with a low gap isn't much better than the Bubble Sort.



Pseudocode:
 '''function''' combsort('''array''' input)
     gap := input'''.size''' ''//initialize gap size''
     '''loop until''' gap = 1 '''and''' swaps = 0
         ''//update the gap value for a next comb. Below is an example''
         gap := int(gap / 1.25)
         '''if''' gap < 1
           ''//minimum gap is 1''
           gap := 1
         '''end if'''
         i := 0
         swaps := 0 ''//see [[Bubble Sort]] for an explanation''
         ''//a single "comb" over the input list''
         '''loop until''' i + gap >= input'''.size''' ''//see [[Shell sort]] for similar idea''
             '''if''' input[i] > input[i+gap]
                 '''swap'''(input[i], input[i+gap])
                 swaps := 1 ''// Flag a swap has occurred, so the''
                            ''// list is not guaranteed sorted''
             '''end if'''
             i := i + 1
         '''end loop'''
     '''end loop'''
 '''end function'''





## 360 Assembly

Translation from prototype.

The program uses ASM structured macros and two ASSIST macros to keep the code as short as possible.

```360asm
*        Comb sort                 23/06/2016
COMBSORT CSECT
         USING  COMBSORT,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         L      R2,N               n
         BCTR   R2,0               n-1
         ST     R2,GAP             gap=n-1
         DO     UNTIL=(CLC,GAP,EQ,=F'1',AND,CLI,SWAPS,EQ,X'00') repeat
         L      R4,GAP               gap                             |
         MH     R4,=H'100'           gap*100                         |
         SRDA   R4,32                .                               |
         D      R4,=F'125'           /125                            |
         ST     R5,GAP               gap=int(gap/1.25)               |
         IF     CLC,GAP,LT,=F'1'     if gap<1 then  -----------+     |
         MVC    GAP,=F'1'              gap=1                   |     |
         ENDIF  ,                    end if  <-----------------+     |
         MVI    SWAPS,X'00'          swaps=false                     |
         LA     RI,1                 i=1                             |
         DO     UNTIL=(C,R3,GT,N)    do i=1 by 1 until i+gap>n  ---+ |
         LR     R7,RI                  i                           | |
         SLA    R7,2                   .                           | |
         LA     R7,A-4(R7)             r7=@a(i)                    | |
         LR     R8,RI                  i                           | |
         A      R8,GAP                 i+gap                       | |
         SLA    R8,2                   .                           | |
         LA     R8,A-4(R8)             r8=@a(i+gap)                | |
         L      R2,0(R7)               temp=a(i)                   | |
         IF     C,R2,GT,0(R8)          if a(i)>a(i+gap) then  ---+ | |
         MVC    0(4,R7),0(R8)            a(i)=a(i+gap)           | | |
         ST     R2,0(R8)                 a(i+gap)=temp           | | |
         MVI    SWAPS,X'01'              swaps=true              | | |
         ENDIF  ,                      end if  <-----------------+ | |
         LA     RI,1(RI)               i=i+1                       | |
         LR     R3,RI                  i                           | |
         A      R3,GAP                 i+gap                       | |
         ENDDO  ,                    end do  <---------------------+ |
         ENDDO  ,                  until gap=1 and not swaps  <------+
         LA     R3,PG              pgi=0
         LA     RI,1               i=1
         DO     WHILE=(C,RI,LE,N)  do i=1 to n  -------+
         LR     R1,RI                i                 |
         SLA    R1,2                 .                 |
         L      R2,A-4(R1)           a(i)              |
         XDECO  R2,XDEC              edit a(i)         |
         MVC    0(4,R3),XDEC+8       output a(i)       |
         LA     R3,4(R3)             pgi=pgi+4         |
         LA     RI,1(RI)             i=i+1             |
         ENDDO  ,                  end do  <-----------+
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
A     DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
      DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
N        DC     A((N-A)/L'A)       number of items of a
GAP      DS     F                  gap
SWAPS    DS     X                  flag for swaps
PG       DS     CL80               output buffer
XDEC     DS     CL12               temp for edit
         YREGS
RI       EQU    6                  i
         END    COMBSORT
```

{{out}}

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



## ActionScript


```ActionScript
function combSort(input:Array)
{
	var gap:uint = input.length;
	var swapped:Boolean = false;
	while(gap > 1 || swapped)
	{
		gap /= 1.25;
		swapped = false;
		for(var i:uint = 0; i + gap < input.length; i++)
		{
			if(input[i] > input[i+gap])
			{
				var tmp = input[i];
				input[i] = input[i+gap];
				input[i+gap]=tmp;
				swapped = true;
			}
		}
	}
	return input;
}
```



## Ada



```Ada
with Ada.Text_IO;
procedure Comb_Sort is
   generic
      type Element_Type is private;
      type Index_Type is range <>;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function ">" (Left, Right : Element_Type) return Boolean is <>;
      with function "+" (Left : Index_Type; Right : Natural) return Index_Type is <>;
      with function "-" (Left : Index_Type; Right : Natural) return Index_Type is <>;
   procedure Comb_Sort (Data: in out Array_Type);

   procedure Comb_Sort (Data: in out Array_Type) is
      procedure Swap (Left, Right : in Index_Type) is
         Temp : Element_Type := Data(Left);
      begin
         Data(Left)  := Data(Right);
         Data(Right) := Temp;
      end Swap;
      Gap : Natural := Data'Length;
      Swap_Occured : Boolean;
   begin
      loop
         Gap := Natural (Float(Gap) / 1.25 - 0.5);
         if Gap < 1 then
            Gap := 1;
         end if;
         Swap_Occured := False;
         for I in Data'First .. Data'Last - Gap loop
            if Data (I) > Data (I+Gap) then
               Swap (I, I+Gap);
               Swap_Occured := True;
            end if;
         end loop;
         exit when Gap = 1 and not Swap_Occured;
      end loop;
   end Comb_Sort;

   type Integer_Array is array (Positive range <>) of Integer;
   procedure Int_Comb_Sort is new Comb_Sort (Integer, Positive, Integer_Array);
   Test_Array : Integer_Array := (1, 3, 256, 0, 3, 4, -1);
begin
   Int_Comb_Sort (Test_Array);
   for I in Test_Array'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_Array (I)));
   end loop;
   Ada.Text_IO.New_Line;
end Comb_Sort;
```


Output:

```txt
-1 0 1 3 3 4 256
```



## AutoHotkey


```autohotkey
List1 = 23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78
List2 = 88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70

List2Array(List1, "MyArray")
CombSort("MyArray")
MsgBox, % List1 "`n" Array2List("MyArray")

List2Array(List2, "MyArray")
CombSort("MyArray")
MsgBox, % List2 "`n" Array2List("MyArray")



;---------------------------------------------------------------------------
CombSort(Array) { ; CombSort of Array %Array%, length = %Array%0
;---------------------------------------------------------------------------
    Gap := %Array%0
    While Gap > 1 Or Swaps {
        If (Gap > 1)
            Gap := 4 * Gap // 5
        i := Swaps := False
        While (j := ++i + Gap) <= %Array%0 {
            If (%Array%%i% > %Array%%j%) {
                Swaps := True
                %Array%%i% := (%Array%%j% "", %Array%%j% := %Array%%i%)
            }
        }
    }
}



;---------------------------------------------------------------------------
List2Array(List, Array) { ; creates an array from a comma separated list
;---------------------------------------------------------------------------
    global
    StringSplit, %Array%, List, `,
}



;---------------------------------------------------------------------------
Array2List(Array) { ; returns a comma separated list from an array
;---------------------------------------------------------------------------
    Loop, % %Array%0
        List .= (A_Index = 1 ? "" : ",") %Array%%A_Index%
    Return, List
}
```

Message (1) box shows:

```txt
23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78
12,14,23,24,24,31,35,38,46,51,57,57,58,76,78,89,92,95,97,99
```

Message (2) box shows:

```txt
88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70
0,4,5,8,14,18,20,31,33,44,62,70,73,75,76,78,81,82,84,88
```



## AWK


```awk
function combsort( a, len,    gap, igap, swap, swaps, i )
{
	gap = len
	swaps = 1

	while( gap > 1 || swaps )
	{
		gap /= 1.2473;
		if ( gap < 1 ) gap = 1
		i = swaps = 0
		while( i + gap < len )
		{
			igap = i + int(gap)
			if ( a[i] > a[igap] )
			{
				swap = a[i]
				a[i] = a[igap]
				a[igap] = swap
				swaps = 1
			}
			i++;
		}
	}
}

BEGIN {
	a[0] = 5
	a[1] = 2
	a[2] = 7
	a[3] = -11
	a[4] = 6
	a[5] = 1

	combsort( a, length(a) )

	for( i=0; i<length(a); i++ )
		print a[i]
}
```



## BBC BASIC


```BBC BASIC
DEF PROC_CombSort11(Size%)

gap%=Size%
REPEAT
  IF gap% > 1 THEN
     gap%=gap% / 1.3
     IF gap%=9 OR gap%=10 gap%=11
  ENDIF
  I% = 1
  Finished%=TRUE
  REPEAT
    IF data%(I%) > data%(I%+gap%) THEN
       SWAP data%(I%),data%(I%+gap%)
       Finished% = FALSE
    ENDIF
    I%+=1
  UNTIL I%+gap% > Size%
UNTIL gap%=1 AND Finished%

ENDPROC
```



## C

Implementation of Combsort11. Its efficiency can be improved by just switching to Insertion sort when the gap size becomes less than 10.

```c
void Combsort11(double a[], int nElements)
{
  int i, j, gap, swapped = 1;
  double temp;

  gap = nElements;
  while (gap > 1 || swapped == 1)
  {
    gap = gap * 10 / 13;
    if (gap == 9 || gap == 10) gap = 11;
    if (gap < 1) gap = 1;
    swapped = 0;
    for (i = 0, j = gap; j < nElements; i++, j++)
    {
      if (a[i] > a[j])
      {
        temp = a[i];
        a[i] = a[j];
        a[j] = temp;
        swapped = 1;
      }
    }
  }
}
```



## C++

This is copied from [[wp:Comb sort|the Wikipedia article]].

```cpp
template<class ForwardIterator>

void combsort ( ForwardIterator first, ForwardIterator last )
{
    static const double shrink_factor = 1.247330950103979;
    typedef typename std::iterator_traits<ForwardIterator>::difference_type difference_type;
    difference_type gap = std::distance(first, last);
    bool swaps = true;

    while ( (gap > 1) || (swaps == true) ){
        if (gap > 1)
            gap = static_cast<difference_type>(gap/shrink_factor);

        swaps = false;
        ForwardIterator itLeft(first);
        ForwardIterator itRight(first); std::advance(itRight, gap);

        for ( ; itRight!=last; ++itLeft, ++itRight ){
            if ( (*itRight) < (*itLeft) ){
                std::iter_swap(itLeft, itRight);
                swaps = true;
            }
        }
    }
}
```


=={{header|C sharp|C#}}==

```csharp
using System;

namespace CombSort
{
    class Program
    {
        static void Main(string[] args)
        {
            int[] unsorted = new int[] { 3, 5, 1, 9, 7, 6, 8, 2, 4 };
            Console.WriteLine(string.Join(",", combSort(unsorted)));
        }
        public static int[] combSort(int[] input)
        {
            double gap = input.Length;
            bool swaps = true;
            while (gap > 1 || swaps)
            {
                gap /= 1.247330950103979;
                if (gap < 1) { gap = 1; }
                int i = 0;
                swaps = false;
                while (i + gap < input.Length)
                {
                    int igap = i + (int)gap;
                    if (input[i] > input[igap])
                    {
                        int swap = input[i];
                        input[i] = input[igap];
                        input[igap] = swap;
                        swaps = true;
                    }
                    i++;
                }
            }
            return input;
        }
    }
}
```



## COBOL

This excerpt contains just enough of the procedure division to show the workings. See the example for the bubble sort for a more complete program.

```COBOL
       C-PROCESS SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           MOVE WC-SIZE TO WC-GAP.

           PERFORM E-COMB UNTIL WC-GAP = 1 AND FINISHED.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       E-COMB SECTION.
       E-000.
           IF WC-GAP > 1
              DIVIDE WC-GAP BY 1.3 GIVING WC-GAP
              IF WC-GAP = 9 OR 10
                 MOVE 11 TO WC-GAP.

           MOVE 1   TO WC-SUB-1.
           MOVE "Y" TO WF-FINISHED.

           PERFORM F-SCAN UNTIL WC-SUB-1 + WC-GAP > WC-SIZE.

       E-999.
           EXIT.

       F-SCAN SECTION.
       F-000.
           ADD WC-SUB-1 WC-GAP GIVING WC-SUB-2.
           IF WB-ENTRY(WC-SUB-1) > WB-ENTRY(WC-SUB-2)
              MOVE WB-ENTRY(WC-SUB-1) TO WC-TEMP
              MOVE WB-ENTRY(WC-SUB-2) TO WB-ENTRY(WC-SUB-1)
              MOVE WC-TEMP            TO WB-ENTRY(WC-SUB-2)
              MOVE "N"                TO WF-FINISHED.

           ADD 1 TO WC-SUB-1.

       F-999.
           EXIT.
```



## Common Lisp


```lisp
(defparameter *shrink* 1.3)

(defun comb-sort (input)
  (loop with input-size = (length input)
        with gap = input-size
        with swapped
        do (when (> gap 1)
             (setf gap (floor gap *shrink*)))
           (setf swapped nil)
           (loop for lo from 0
                 for hi from gap below input-size
                 when (> (aref input lo) (aref input hi))
                   do (rotatef (aref input lo) (aref input hi))
                      (setf swapped t))
        while (or (> gap 1) swapped)
        finally (return input)))
```



## D

{{trans|Python}}

```d
import std.stdio, std.algorithm;

void combSort(T)(T[] input) pure nothrow @safe @nogc {
    int gap = input.length;
    bool swaps = true;

    while (gap > 1 || swaps) {
        gap = max(1, cast(int)(gap / 1.2473));
        swaps = false;
        foreach (immutable i; 0 .. input.length - gap)
            if (input[i] > input[i + gap]) {
                input[i].swap(input[i + gap]);
                swaps = true;
            }
    }
}

void main() {
    auto data = [28, 44, 46, 24, 19, 2, 17, 11, 25, 4];
    data.combSort;
    data.writeln;
}
```

{{out}}

```txt
[2, 4, 11, 17, 19, 24, 25, 28, 44, 46]
```



## Eiffel


```Eiffel


class
	COMB_SORT [G -> COMPARABLE]

feature

	combsort (ar: ARRAY [G]): ARRAY [G]
			-- Sorted array in ascending order.
		require
			array_not_void: ar /= Void
		local
			gap, i: INTEGER
			swap: G
			swapped: BOOLEAN
			shrink: REAL_64
		do
			create Result.make_empty
			Result.deep_copy (ar)
			gap := Result.count
			from
			until
				gap = 1 and swapped = False
			loop
				from
					i := Result.lower
					swapped := False
				until
					i + gap > Result.count
				loop
					if Result [i] > Result [i + gap] then
						swap := Result [i]
						Result [i] := Result [i + gap]
						Result [i + gap] := swap
						swapped := True
					end
					i := i + 1
				end
				shrink := gap / 1.3
				gap := shrink.floor
				if gap < 1 then
					gap := 1
				end
			end
		ensure
			Result_is_set: Result /= Void
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
			test := <<1, 5, 99, 2, 95, 7, -7>>
			io.put_string ("unsorted" + "%N")
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
			io.put_string ("%N" + "sorted:" + "%N")
			create combsort
			test := combsort.combsort (test)
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
		end

	combsort: COMB_SORT [INTEGER]

	test: ARRAY [INTEGER]

end

```

{{out}}

```txt

unsorted:
1 5 99 2 95 7 -7
sorted:
-7 1 2 5 7 95 99

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'math;
import system'routines;

extension op
{
    combSort()
    {
        var list := self.clone();

        real gap := list.Length;
        bool swaps := true;
        while (gap > 1 || swaps)
        {
            gap /= 1.247330950103979r;
            if (gap<1) { gap := 1 };

            int i := 0;
            swaps := false;
            while (i + gap.RoundedInt < list.Length)
            {
                int igap := i + gap.RoundedInt;
                if (list[i] > list[igap])
                {
                    list.exchange(i,igap);
                    swaps := true
                };
                i += 1
            }
        };

        ^ list
    }
}

public program()
{
    var list := new int[]::(3, 5, 1, 9, 7, 6, 8, 2, 4 );

    console.printLine("before:", list.asEnumerable());
    console.printLine("after :", list.combSort().asEnumerable())
}
```

{{out}}

```txt

before:3,5,1,9,7,6,8,2,4
after :1,2,3,4,5,6,7,8,9

```



## Elixir


```elixir
defmodule Sort do
  def comb_sort([]), do: []
  def comb_sort(input) do
    comb_sort(List.to_tuple(input), length(input), 0) |> Tuple.to_list
  end

  defp comb_sort(output, 1, 0), do: output
  defp comb_sort(input, gap, _) do
    gap = max(trunc(gap / 1.25), 1)
    {output,swaps} = Enum.reduce(0..tuple_size(input)-gap-1, {input,0}, fn i,{acc,swap} ->
      if (x = elem(acc,i)) > (y = elem(acc,i+gap)) do
        {acc |> put_elem(i,y) |> put_elem(i+gap,x), 1}
      else
        {acc,swap}
      end
    end)
    comb_sort(output, gap, swaps)
  end
end

(for _ <- 1..20, do: :rand.uniform(20)) |> IO.inspect |> Sort.comb_sort |> IO.inspect
```


{{out}}

```txt

[10, 7, 8, 13, 4, 11, 13, 12, 18, 11, 5, 7, 3, 4, 15, 1, 17, 16, 7, 14]
[1, 3, 4, 4, 5, 7, 7, 7, 8, 10, 11, 11, 12, 13, 13, 14, 15, 16, 17, 18]

```



## Forth

This is an implementation of Comb sort with a different ending. Here [[Gnome sort]] is used, since it is rather small. The dataset is rather large, because otherwise the Comb sort routine would never kick in, passing control to Gnome sort almost right away. Note Comb sort can be kept much simpler this way, because Combsort11 optimizations and swapped flags can be discarded.

```forth
defer precedes
defer exchange

: gnomesort                            ( a n)
  swap >r 1                            ( n c)
  begin                                ( n c)
    over over >                        ( n c f)
  while                                ( n c)
    dup if                             ( n c)
      dup dup 1- over over r@ precedes
      if r@ exchange 1- else drop drop 1+ then
    else 1+ then                       ( n c)
  repeat drop drop r> drop             ( --)
;

: combsort                             ( a n --)
  dup begin                            ( a n g)
    10 13 */ tuck >r >r 0              ( a g 0)
    begin                              ( a g 0)
      over r@ <                        ( a g 0 f)
    while                              ( a g 0)
      rot >r over over r@ precedes     ( g 0 f)
      if over over r@ exchange then    ( g 0)
      r> rot 1+ rot 1+                 ( a g 0)
    repeat drop drop r> r>             ( a n g)
    dup 9 <                            ( a n g f)
  until drop gnomesort                 ( --)
;

create example
  8 93 69 52 50 79 33 52 19 77 , , , , , , , , , ,
 72 85 11 61 64 80 64 76 47 65 , , , , , , , , , ,
  13 47 23 40 87 45 2 48 22 69 , , , , , , , , , ,
  1 53 33 60 57 14 76 32 59 12 , , , , , , , , , ,
 74 38 39 22 87 28 37 93 71 88 , , , , , , , , , ,
 56 35 48 99 21 35 26 28 58 85 , , , , , , , , , ,
 27 16 54 88 82 18 45 64 45 87 , , , , , , , , , ,
   98 97 60 77 43 1 64 0 32 89 , , , , , , , , , ,
  77 90 68 83 9 76 10 10 95 12 , , , , , , , , , ,
   99 23 74 58 54 25 50 9 94 1 , , , , , , , , , ,

:noname >r cells r@ + @ swap cells r> + @ swap < ; is precedes
:noname >r cells r@ + swap cells r> + over @ over @ swap rot ! swap ! ; is exchange

: .array 100 0 do example i cells + ? loop cr ;

.array example 100 combsort .array
```



### Less Clever Version

This version is an academic demonstration that aligns with the algorithm. As is, it is limited to use one static array and sorts in ascending order only.

```forth
\ combsort for the Forth Newbie (GForth)

HEX
\ gratuitous variables ( Add clarity but NOT re-entrant)
VARIABLE GAP
VARIABLE SORTED      \ flag

DECIMAL
100 CONSTANT SIZE

\ allocate a small array of cells
CREATE Q   SIZE CELLS ALLOT

\ operator to index into the array
: ]Q  ( n -- adr) CELLS Q + ;

\ fill array and see array
: INITDATA ( -- )  SIZE 0 DO   SIZE I -  I ]Q !  LOOP ;

: SEEDATA  ( -- )  CR  SIZE 0 DO   I ]Q @ U.   LOOP ;

\ compute a new gap using scaled division
\ factored out for this example. Could be a macro or inline code.
: /1.3  ( n -- n' )    10 13 */ ;

\ factored out for this example. Could be a macro or inline code.
: XCHG  ( adr1 adr2 n1 n2-- ) SWAP ROT !  SWAP ! ;

: COMBSORT ( n -- )
    DUP >R                     \ copy n to return stack
    1+ GAP !                   \ set GAP to n+1
    BEGIN
      GAP @  /1.3  GAP !       \ re-compute the gap
      SORTED ON
      R@ GAP @ -  0            \ n-gap is loop limit
      DO
         I GAP @ + ]Q   I ]Q   \ compute array addresses
         OVER @ OVER @         \ fetch the data in each cell
         2DUP <                \ compare a copy of the data
         IF
            XCHG               \ Exchange the data in the cells
            SORTED OFF         \ flag we are not sorted
         ELSE
            2DROP 2DROP        \ remove address and data
         THEN
      LOOP
      SORTED @  GAP @ 0=  AND  \ test for complete
   UNTIL
   R> DROP ;                   \ remove 'n' from return stack
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Combsort_Demo
  implicit none

  integer, parameter :: num = 20
  real :: array(num)

  call random_seed
  call random_number(array)
  write(*,*) "Unsorted array:-"
  write(*,*) array
  write(*,*)
  call combsort(array)
  write(*,*) "Sorted array:-"
  write(*,*) array

contains

  subroutine combsort(a)

    real, intent(in out) :: a(:)
    real :: temp
    integer :: i, gap
    logical :: swapped = .true.

    gap = size(a)
    do while (gap > 1 .or. swapped)
      gap = gap / 1.3
      if (gap < 1) gap = 1
      swapped = .false.
      do i = 1, size(a)-gap
        if (a(i) > a(i+gap)) then
          temp = a(i)
          a(i) = a(i+gap)
          a(i+gap) = temp;
          swapped = .true.
        end if
      end do
    end do

  end subroutine combsort

end program Combsort_Demo
```



## FreeBASIC


```Freebasic
' version 21-10-2016
' compile with: fbc -s console
' for boundary checks on array's compile with: fbc -s console -exx

Sub compsort(bs() As Long)
    ' sort from lower bound to the highter bound
    ' array's can have subscript range from -2147483648 to +2147483647
    Dim As Long lb = LBound(bs)
    Dim As Long ub = UBound(bs)
    Dim As Long gap = ub - lb
    Dim As Long done, i

    Do
        gap = Int (gap / 1.3)
        If gap < 1 Then gap = 1
        done = 0
        For i = lb To ub - gap
            If bs(i) > bs(i + gap) Then
                Swap bs(i), bs(i + gap)
                done = 1
            End If
        Next
    Loop Until ((gap = 1) And (done = 0))

End Sub

Sub comp11sort(bs() As Long)
    ' sort from lower bound to the higher bound
    ' array's can have subscript range from -2147483648 to +2147483647
    Dim As Long lb = LBound(bs)
    Dim As Long ub = UBound(bs)
    Dim As Long gap = ub - lb
    Dim As Long done, i

    Do
        gap = Int(gap / 1.24733)
        If gap = 9 Or gap = 10 Then
            gap = 11
        ElseIf gap < 1 Then
            gap = 1
        End If
        done = 0
        For i = lb To ub - gap
            If bs(i) > bs(i + gap) Then
                Swap bs(i), bs(i + gap)
                done = 1
            End If
        Next
    Loop Until ((gap = 1) And (done = 0))

End Sub

' ------=< MAIN >=------

Dim As Long i, array(-7 To 7)

Dim As Long a = LBound(array), b = UBound(array)

Randomize Timer
For i = a To b : array(i) = i  : Next
For i = a To b ' little shuffle
    Swap array(i), array(Int(Rnd * (b - a +1)) + a)
Next

Print "normal comb sort"
Print "unsorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print
compsort(array())  ' sort the array
Print "  sorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print

Print
Print "comb11 sort"
For i = a To b ' little shuffle
    Swap array(i), array(Int(Rnd * (b - a +1)) + a)
Next
Print "unsorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print
comp11sort(array())  ' sort the array
Print "  sorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
normal comb sort
unsorted   -6   5  -1  -3  -7   6   1   7  -4   3   4  -2  -5   0   2
  sorted   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7

comb11 sort
unsorted    4  -7  -1   1   2   3  -3   7   0  -2   6  -5   5  -6  -4
  sorted   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=ade780ac2893fcfc95bf0d3feff6a3a8 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siToSort As Short[] = [249, 28, 111, 36, 171, 98, 29, 448, 44, 147, 154, 46, 102, 183, 24,
                          120, 19, 123, 2, 17, 226, 11, 211, 25, 191, 205, 77]
Dim siStart As Short
Dim siGap As Short = siToSort.Max
Dim bSorting, bGap1 As Boolean

Print "To sort: -"
ShowWorking(siToSort)
Print

Repeat
  bSorting = False
  siStart = 0
  If siGap = 1 Then bGap1 = True

  Repeat
    If siToSort[siStart] > siToSort[siStart + siGap] Then
      Swap siToSort[siStart], siToSort[siStart + siGap]
      bSorting = True
    End If
    Inc siStart
  Until siStart + siGap > siToSort.Max

  If bSorting Then ShowWorking(siToSort)
  siGap /= 1.3
  If siGap < 1 Then siGap = 1

Until bSorting = False And bGap1 = True

End
'-----------------------------------------
Public Sub ShowWorking(siToSort As Short[])
Dim siCount As Short

For siCount = 0 To siToSort.Max
  Print Str(siToSort[siCount]);
  If siCount <> siToSort.Max Then Print ",";
Next

Print

End
```

Output:

```txt

To sort: -
249,28,111,36,171,98,29,448,44,147,154,46,102,183,24,120,19,123,2,17,226,11,211,25,191,205,77

77,28,111,36,171,98,29,448,44,147,154,46,102,183,24,120,19,123,2,17,226,11,211,25,191,205,249
77,11,111,25,171,98,29,448,44,147,154,46,102,183,24,120,19,123,2,17,226,28,211,36,191,205,249
77,11,111,2,17,98,28,211,36,147,154,46,102,183,24,120,19,123,25,171,226,29,448,44,191,205,249
46,11,111,2,17,19,28,25,36,147,29,77,44,183,24,120,98,123,211,171,226,154,448,102,191,205,249
36,11,29,2,17,19,24,25,46,123,111,77,44,154,28,102,98,147,211,171,226,183,448,120,191,205,249
24,11,29,2,17,19,36,25,28,102,98,77,44,154,46,123,111,120,191,171,226,183,448,147,211,205,249
17,11,29,2,24,19,36,25,28,102,46,77,44,120,98,123,111,154,191,147,211,183,249,171,226,205,448
2,11,19,17,24,28,36,25,29,44,46,77,102,111,98,123,120,154,183,147,171,191,205,211,226,249,448
2,11,19,17,24,25,29,28,36,44,46,77,98,111,102,123,120,147,171,154,183,191,205,211,226,249,448
2,11,17,19,24,25,28,29,36,44,46,77,98,102,111,120,123,147,154,171,183,191,205,211,226,249,448

```


## Go


```go
package main

import "fmt"

func main() {
    a := []int{170, 45, 75, -90, -802, 24, 2, 66}
    fmt.Println("before:", a)
    combSort(a)
    fmt.Println("after: ", a)
}

func combSort(a []int) {
    if len(a) < 2 {
        return
    }
    for gap := len(a); ; {
        if gap > 1 {
            gap = gap * 4 / 5
        }
        swapped := false
        for i := 0; ; {
            if a[i] > a[i+gap] {
                a[i], a[i+gap] = a[i+gap], a[i]
                swapped = true
            }
            i++
            if i+gap >= len(a) {
                break
            }
        }
        if gap == 1 && !swapped {
            break
        }
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
    combSort(sort.IntSlice(a))
    fmt.Println("after: ", a)
}

func combSort(a sort.Interface) {
    if a.Len() < 2 {
        return
    }
    for gap := a.Len(); ; {
        if gap > 1 {
            gap = gap * 4 / 5
        }
        swapped := false
        for i := 0; ; {
            if a.Less(i+gap, i) {
                a.Swap(i, i+gap)
                swapped = true
            }
            i++
            if i+gap >= a.Len() {
                break
            }
        }
        if gap == 1 && !swapped {
            break
        }
    }
}
```



## Groovy

Combsort solution:

```groovy
def makeSwap = { a, i, j -> print "."; a[i] ^= a[j]; a[j] ^= a[i]; a[i] ^= a[j] }

def checkSwap = { a, i, j -> [(a[i] > a[j])].find { it }.each { makeSwap(a, i, j) } }

def combSort = { input ->
    def swap = checkSwap.curry(input)
    def size = input.size()
    def gap = size
    def swapped = true
    while (gap != 1 || swapped) {
        gap = (gap / 1.247330950103979) as int
        gap = (gap < 1) ? 1 : gap
        swapped = (0..<(size-gap)).any { swap(it, it + gap) }
    }
    input
}
```


Combsort11 solution:

```groovy
def combSort11 = { input ->
    def swap = checkSwap.curry(input)
    def size = input.size()
    def gap = size
    def swapped = true
    while (gap != 1 || swapped) {
        gap = (gap / 1.247330950103979) as int
        gap = ((gap < 1) ? 1 : ([10,9].contains(gap) ? 11 : gap))
        swapped = (0..<(size-gap)).any { swap(it, it + gap) }
    }
    input
}
```


Test:

```groovy
println   (combSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (combSort11([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println ()
println   (combSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
println (combSort11([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
```


Output:

```txt
..................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
..........................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]

...............................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
...............................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]

```



## Haskell


```haskell
import Data.List
import Control.Arrow
import Control.Monad

flgInsert x xs = ((x:xs==) &&& id)$ insert x xs

gapSwapping k = (and *** concat. transpose). unzip
  . map (foldr (\x (b,xs) -> first (b &&)$ flgInsert x xs) (True,[]))
  . transpose. takeWhile (not.null). unfoldr (Just. splitAt k)

combSort xs = (snd. fst) $ until (\((b,_),g)-> b && g==1)
    (\((_,xs),g) ->(gapSwapping g xs, fg g)) ((False,xs), fg $ length xs)
  where fg = max 1. truncate. (/1.25). fromIntegral
```

Example:

```haskell
*Main> combSort [23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78]
[12,14,23,24,24,31,35,38,46,51,57,57,58,76,78,89,92,95,97,99]
```



## Io


```io
List do(
    combSortInPlace := method(
        gap := size
        swap := true

        while(gap > 1 or swap,
            swap = false
            gap = (gap / 1.25) floor

            for(i, 0, size - gap,
                if(at(i) > at(i + gap),
                    swap = true
                    swapIndices(i, i + gap)
                )
            )
        )
    self)
)

lst := list(23, 76, 99, 58, 97, 57, 35, 89, 51, 38, 95, 92, 24, 46, 31, 24, 14, 12, 57, 78)
lst combSortInPlace println # ==> list(12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   demosort(combsort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure combsort(X,op)                  #: return sorted X
local gap,swapped,i

   op := sortop(op,X)                     # select how and what we sort

   swappped := gap := *X                  # initialize gap size and say swapped
   until /swapped & gap = 1 do {
      gap := integer(gap / 1.25)          # update the gap value for a next comb
      gap <:= 1                           # minimum gap of 1
      swapped := &null

       i := 0
       until (i +:= 1) + gap > *X do      # a single "comb" over the input list
         if op(X[i+gap],X[i]) then
            X[i+1] :=: X[swapped := i]    # swap and flag as unsorted
      }
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]]. The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.

Abbreviated sample output:
```txt
Sorting Demo using procedure combsort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "CombSrt.bas"
110 RANDOMIZE
120 NUMERIC ARRAY(11 TO 30)
130 CALL INIT(ARRAY)
140 CALL WRITE(ARRAY)
150 CALL COMBSORT(ARRAY)
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
280 DEF COMBSORT(REF A)
290   LET N,GAP=UBOUND(A):LET SW=1
300   DO WHILE GAP>1 OR SW
310     LET GAP=MAX(INT(GAP/1.3),1):LET SW=0
320     FOR I=LBOUND(A) TO N-GAP
330       IF A(I)>A(I+GAP) THEN
340         LET T=A(I):LET A(I)=A(I+GAP):LET A(I+GAP)=T
350         LET SW=1
360       END IF
370     NEXT
380   LOOP
390 END DEF
```



## J

{{eff note|J|/:~}}
Large gap sizes allow some parallelism in comparisons and swaps.  (If the gap size is G, then G pairs can be compared and swapped in parallel.)  Beyond that, however, the data flow complexity of this algorithm requires a fair bit of micro-management.


```J
combSort=:3 :0
  gap=. #y
  whilst.1 < gap+swaps do.
    swaps=. 0
    i=. i.2,gap=. 1 >. <.gap%1.25
    while.{:$i=.i #"1~ ({: i) < #y do.
      swaps=.swaps+#{:k=.i #"1~b=. >/ i{y
      i=. i+gap
      y=.((|.k){y) k} y
    end.
  end.
  y
)
```


Example use:
    combSort 23 76 99 58 97 57 35 89 51 38 95 92 24 46 31 24 14 12 57 78
 12 14 23 24 24 31 35 38 46 51 57 57 58 76 78 89 92 95 97 99
    combSort 88 18 31 44 4 0 8 81 14 78 20 76 84 33 73 75 82 5 62 70
 0 4 5 8 14 18 20 31 33 44 62 70 73 75 76 78 81 82 84 88


## Java

This is copied from [[wp:Comb sort|the Wikipedia article]].

```java>public static <E extends Comparable<? super E>
 void sort(E[] input) {
    int gap = input.length;
    boolean swapped = true;
    while (gap > 1 || swapped) {
        if (gap > 1) {
            gap = (int) (gap / 1.3);
        }
        swapped = false;
        for (int i = 0; i + gap < input.length; i++) {
            if (input[i].compareTo(input[i + gap]) > 0) {
                E t = input[i];
                input[i] = input[i + gap];
                input[i + gap] = t;
                swapped = true;
            }
        }
    }
}
```



## JavaScript


```javascript

  // Node 5.4.1 tested implementation (ES6)
  function is_array_sorted(arr) {
      var sorted = true;
      for (var i = 0; i < arr.length - 1; i++) {
          if (arr[i] > arr[i + 1]) {
              sorted = false;
              break;
          }
      }
      return sorted;
  }

  // Array to sort
  var arr = [4, 9, 0, 3, 1, 5];

  var iteration_count = 0;
  var gap = arr.length - 2;
  var decrease_factor = 1.25;

  // Until array is not sorted, repeat iterations
  while (!is_array_sorted(arr)) {
      // If not first gap
      if (iteration_count > 0)
      // Calculate gap
          gap = (gap == 1) ? gap : Math.floor(gap / decrease_factor);

      // Set front and back elements and increment to a gap
      var front = 0;
      var back = gap;
      while (back <= arr.length - 1) {
          // If elements are not ordered swap them
          if (arr[front] > arr[back]) {
              var temp = arr[front];
              arr[front] = arr[back];
              arr[back] = temp;
          }

          // Increment and re-run swapping
          front += 1;
          back += 1;
      }
      iteration_count += 1;
  }

  // Print the sorted array
  console.log(arr);
}
```



{{out}}

```txt

 [0, 1, 3, 4, 5, 9]

```



## jq

{{works with|jq|1.4}}
An implementation of the pseudo-code in the task description:

```jq
# Input should be the array to be sorted.
def combsort:

  # As soon as "condition" is true, emit . and stop:
  def do_until(condition; next):
    def u: if condition then . else (next|u) end;
  u;

  def swap(i;j):
    if i==j then . else .[i] as $tmp | .[i] = .[j] | .[j] = $tmp end;

   . as $in
  | length as $length
    # state: [gap, swaps, array] where:
    #   gap is the gap size;
    #   swaps is a boolean flag indicating a swap has occurred,
    #         implying that the array might not be sorted;
    #   array is the current state of the array being sorted
  | [ $length, false, $in ]
  | do_until( .[0] == 1 and .[1] == false;
      # update the gap value for the next "comb":
      ([1, ((.[0] / 1.25) | floor)] | max) as $gap   # minimum gap is 1

      # state: [i, swaps, array]
      | [0, false, .[2]]
      # a single "comb" over the input list:
      | do_until( (.[0] + $gap) >= $length;
          .[0] as $i
          | if .[2][$i] > .[2][$i+$gap] then
              [$i+1, true, (.[2]|swap($i; $i+$gap))]
            else .[0] += 1
            end)
      | .[0] = $gap )
  | .[2] ;
```



## Julia


```julia
# v0.6

function combsort!(x::Array)::Array
    gap, swaps = length(x), true
    while gap > 1 || swaps
        gap = floor(Int, gap / 1.25)
        i, swaps = 0, false
        while i + gap < length(x)
            if x[i+1] > x[i+1+gap]
                x[i+1], x[i+1+gap] = x[i+1+gap], x[i+1]
                swaps = true
            end
            i += 1
        end
    end
    return x
end

x = randn(100)
@show x combsort!(x)
@assert issorted(x)
```


{{out}}

```txt
x = [1.41167, 1.19626, 0.821703, 0.336024, -0.708447, 0.694578, 1.49075, -1.07124, -1.59686, -0.720135]
combsort!(x) = [-1.59686, -1.07124, -0.720135, -0.708447, 0.336024, 0.694578, 0.821703, 1.19626, 1.41167, 1.49075]
```



## Kotlin


```scala
// version 1.1.2

fun <T : Comparable<T>> combSort(input: Array<T>) {
    var gap = input.size
    if (gap <= 1) return  // already sorted
    var swaps = false
    while (gap > 1 || swaps) {
        gap = (gap / 1.247331).toInt()
        if (gap < 1) gap = 1
        var i = 0
        swaps = false
        while (i + gap < input.size) {
            if (input[i] > input[i + gap]) {
                val tmp = input[i]
                input[i] = input[i + gap]
                input[i + gap] = tmp
                swaps = true
            }
            i++
        }
    }
}

fun main(args: Array<String>) {
    val ia = arrayOf(28, 44, 46, 24, 19, 2, 17, 11, 25, 4)
    println("Unsorted : ${ia.contentToString()}")
    combSort(ia)
    println("Sorted   : ${ia.contentToString()}")
    println()
    val ca = arrayOf('X', 'B', 'E', 'A', 'Z', 'M', 'S', 'L', 'Y', 'C')
    println("Unsorted : ${ca.contentToString()}")
    combSort(ca)
    println("Sorted   : ${ca.contentToString()}")
}
```


{{out}}

```txt

Unsorted : [28, 44, 46, 24, 19, 2, 17, 11, 25, 4]
Sorted   : [2, 4, 11, 17, 19, 24, 25, 28, 44, 46]

Unsorted : [X, B, E, A, Z, M, S, L, Y, C]
Sorted   : [A, B, C, E, L, M, S, X, Y, Z]

```



## Liberty BASIC


```lb

'randomize 0.5
itemCount = 20
    dim item(itemCount)
    for i = 1 to itemCount
        item(i) = int(rnd(1) * 100)
    next i
    print "Before Sort"
    for i = 1 to itemCount
        print item(i)
    next i
    print: print
't0=time$("ms")

    gap=itemCount
    while gap>1 or swaps <> 0
        gap=int(gap/1.25)
        'if gap = 10 or gap = 9 then gap = 11    'uncomment to get Combsort11
        if gap <1 then gap = 1
        i = 1
        swaps = 0
        for i = 1 to itemCount-gap
            if item(i) > item(i + gap) then
                temp = item(i)
                item(i) = item(i + gap)
                item(i + gap) = temp
                swaps = 1
            end if
        next
    wend

    print "After Sort"
't1=time$("ms")
'print t1-t0

    for i = 1 to itemCount
        print item(i)
    next i
end

```



## Lua


```lua
function combsort(t)
  local gapd, gap, swaps = 1.2473, #t, 0
  while gap + swaps > 1 do
    local k = 0
    swaps = 0
    if gap > 1 then gap = math.floor(gap / gapd) end
    for k = 1, #t - gap do
      if t[k] > t[k + gap] then
        t[k], t[k + gap], swaps = t[k + gap], t[k], swaps + 1
      end
    end
  end
  return t
end

print(unpack(combsort{3,5,1,2,7,4,8,3,6,4,1}))
```



## Maple


```Maple
swap := proc(arr, a, b)
	local temp;
	temp := arr[a]:
	arr[a] := arr[b]:
	arr[b] := temp:
end proc:
newGap := proc(gap)
	local new;
	new := trunc(gap*10/13);
	if (new < 1) then return 1; end if;
	return new;
end proc;
combsort := proc(arr, len)
	local gap, swapped,i, temp;
	swapped := true:
	gap := len:
	while ((not gap = 1) or swapped) do
		gap := newGap(gap):
		swapped := false:
		for i from 1 to len-gap by 1 do
			if (arr[i] > arr[i+gap]) then
				temp := arr[i]:
				arr[i] := arr[i+gap]:
				arr[i+gap] := temp:
				swapped:= true:
			end if:
		end do:
	end do:
end proc:
arr := Array([17,3,72,0,36,2,3,8,40,0]);
combsort(arr, numelems(arr));
arr;
```

{{Out|Output}}

```txt
[0,0,2,3,3,8,17,36,40,72]
```



## Mathematica


```Mathematica
combSort[list_] := Module[{ gap = 0, listSize = 0, swaps = True},
        gap = listSize = Length[list];
        While[ !((gap <= 1) && (swaps == False)),

            gap = Floor@Divide[gap, 1.25];
            If[ gap < 1, gap = 1]; i = 1; swaps = False;

            While[ ! ((i + gap - 1) >= listSize),
                If[ list[[i]] > list[[i + gap]], swaps = True;
                list[[i ;; i + gap]] = list[[i + gap ;; i ;; -1]];
                ];
            i++;
            ]
        ]
]
```



```txt
combSort@{2, 1, 3, 7, 6}
->{1, 2, 3, 6, 7}

```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function list = combSort(list)

    listSize = numel(list);
    gap = int32(listSize); %Coerce gap to an int so we can use the idivide function
    swaps = true; %Swap flag

    while not((gap <= 1) && (swaps == false))

        gap = idivide(gap,1.25,'floor'); %Int divide, floor the resulting operation

        if gap < 1
            gap = 1;
        end

        i = 1; %i equals 1 because all arrays are 1 based in MATLAB
        swaps = false;

        %i + gap must be subtracted by 1 because the pseudo-code was writen
        %for 0 based arrays
        while not((i + gap - 1) >= listSize)

            if (list(i) > list(i+gap))
                list([i i+gap]) = list([i+gap i]); %swap
                swaps = true;
            end
        i = i + 1;

        end %while
    end %while
end %combSort
```


Sample Output:

```MATLAB>>
 combSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```



## MAXScript


```MAXScript
fn combSort arr =
(
	local gap = arr.count
	local swaps = 1
	while not (gap == 1 and swaps == 0) do
	(
		gap = (gap / 1.25) as integer
		if gap < 1 do
		(
			gap = 1
		)
		local i = 1
		swaps = 0
		while not (i + gap > arr.count) do
		(
			if arr[i] > arr[i+gap] do
			(
				swap arr[i] arr[i+gap]
				swaps = 1
			)
			i += 1

		)


	)
	return arr
)
```

Output:

```MAXScript

a = for i in 1 to 10 collect random 1 10
#(2, 6, 5, 9, 10, 7, 2, 6, 1, 4)
combsort a
#(1, 2, 2, 4, 5, 6, 6, 7, 9, 10)

```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

placesList = [String -
    "UK  London",     "US  New York"   -
  , "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham" -
  , "UK  Birmingham", "UK  Boston"     -
]
sortedList = combSort(String[] Arrays.copyOf(placesList, placesList.length))

lists = [placesList, sortedList]
loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method combSort(input = String[]) public constant binary returns String[]

  swaps = isTrue
  gap = input.length
  loop label comb until gap = 1 & \swaps
    gap = int gap / 1.25
    if gap < 1 then
      gap = 1
    i_ = 0
    swaps = isFalse
    loop label swaps until i_ + gap >= input.length
      if input[i_].compareTo(input[i_ + gap]) > 0 then do
        swap            = input[i_]
        input[i_]       = input[i_ + gap]
        input[i_ + gap] = swap
        swaps           = isTrue
        end
        i_ = i_ + 1
      end swaps
    end comb

  return input

method isTrue public constant binary returns boolean
  return 1 == 1

method isFalse public constant binary returns boolean
  return \isTrue

```

;Output

```txt

UK  London
US  New York
US  Boston
US  Washington
UK  Washington
US  Birmingham
UK  Birmingham
UK  Boston

UK  Birmingham
UK  Boston
UK  London
UK  Washington
US  Birmingham
US  Boston
US  New York
US  Washington

```



## Nim


```nim
proc combSort[T](a: var openarray[T]) =
  var gap = a.len
  var swapped = true
  while gap > 1 or swapped:
    gap = gap * 10 div 13
    if gap == 9 or gap == 10: gap = 11
    if gap < 1: gap = 1
    swapped = false
    var i = 0
    for j in gap .. <a.len:
      if a[i] > a[j]:
        swap a[i], a[j]
        swapped = true
      inc i

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
combSort a
echo a
```

Output:

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## Objeck


```objeck

bundle Default {
  class Stooge {
    function : Main(args : String[]) ~ Nil {
      nums := [3, 5, 1, 9, 7, 6, 8, 2, 4];
      CombSort(nums);
      each(i : nums) {
        IO.Console->Print(nums[i])->Print(",");
      };
      IO.Console->PrintLine();
    }

    function : CombSort(input : Int[]) ~ Nil {
      gap : Float := input->Size();
      swaps := true;
      while(gap > 1 | swaps) {
        gap /= 1.247330950103979;
        if(gap < 1) { gap := 1; };
        i : Int := 0;
        swaps := false;
        while(i + gap < input->Size()) {
          igap : Int := i + gap->As(Int);
          if (input[i] > input[igap]) {
            swap : Int := input[i];
            input[i] := input[igap];
            input[igap] := swap;
            swaps := true;
          };
          i += 1;
        };
      };
    }
  }
}

```



## OCaml



```ocaml
let comb_sort ~input =
  let input_length = Array.length input in
  let gap = ref(input_length) in
  let swapped = ref true in
  while (!gap > 1 || !swapped) do
    if (!gap > 1) then
      gap := int_of_float (float !gap /. 1.3);

    swapped := false;
    for i = 0 to input_length - !gap do
      if input.(i) > input.(i + !gap) then begin
        let tmp = input.(i) in
        input.(i) <- input.(i + !gap);
        input.(i + !gap) <- tmp;
        swapped := true;
      end
    done
  done
;;
```



## Oz


```oz
declare
  proc {CombSort Arr}
     Low = {Array.low Arr}
     High = {Array.high Arr}
     Size = High - Low + 1
     Gap = {NewCell Size}
     Swapped = {NewCell true}
     proc {Swap I J}
        Arr.J := (Arr.I := Arr.J)
     end
  in
     for while:@Gap>1 orelse @Swapped do
        if @Gap > 1 then
           Gap := {Float.toInt {Floor {Int.toFloat @Gap} / 1.3}}
        end
        Swapped := false
        for I in Low..High-@Gap do
           if Arr.I > Arr.(I+@Gap) then
              {Swap I I+@Gap}
              Swapped := true
           end
        end
     end
  end
  Arr = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {CombSort Arr}
  {Show {Array.toRecord unit Arr}}
```



## PARI/GP


```parigp
combSort(v)={
  my(phi=(1+sqrt(5))/2,magic=1/(1-exp(-phi)),g=#v,swaps);
  while(g>1 | swaps,
    if(g>1, g\=magic);
    swaps=0;
    for(i=1,#v-g,
      if(v[i]>v[i+g],
        my(t=v[i]);
        v[i]=v[i+g];
        v[i+g]=t;
        swaps++
      )
    )
  );
  v
};
```



## Pascal


```pascal
program CombSortDemo;


// NOTE: The array is 1-based
//       If you want to use this code on a 0-based array, see below
type
  TIntArray = array[1..40] of integer;

var
  data: TIntArray;
  i: integer;

procedure combSort(var a: TIntArray);
  var
    i, gap, temp: integer;
    swapped: boolean;
  begin
    gap := length(a);
    swapped := true;
    while (gap > 1) or swapped do
    begin
      gap := trunc(gap / 1.3);
      if (gap < 1) then
        gap := 1;
      swapped := false;
      for i := 1 to length(a) - gap do
        if a[i] > a[i+gap] then
        begin
	  temp := a[i];
          a[i] := a[i+gap];
          a[i+gap] := temp;
          swapped := true;
        end;
    end;
  end;

begin
  Randomize;
  writeln('The data before sorting:');
  for i := low(data) to high(data) do
  begin
    data[i] := Random(high(data));
    write(data[i]:4);
  end;
  writeln;
  combSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
```

Output:

```txt

The data before sorting:
  10  26  32  10   9  32  38  37  12   9  16   7  25   1  37   7  24  22   7  36   2   5  10   5  33  35  32  18   5  28   7   5  36  12  16  36  24   3  29  15
The data after sorting:
   1   2   3   5   5   5   5   7   7   7   7   9   9  10  10  10  12  12  15  16  16  18  22  24  24  25  26  28  29  32  32  32  33  35  36  36  36  37  37  38

```



```pascal
program CombSortDemo;


// NOTE: The array is 0-based
//       If you want to use this code on a 1-based array, see above
type
  TIntArray = array[0..39] of integer;

var
  data: TIntArray;
  i: integer;

procedure combSort(var a: TIntArray);
  var
    i, gap, temp: integer;
    swapped: boolean;
  begin
    gap := length(a);
    swapped := true;
    while (gap > 1) or swapped do
    begin
      gap := trunc(gap / 1.3);
      if (gap < 1) then
        gap := 1;
      swapped := false;
      for i := 0 to length(a) - gap - 1 do
        if a[i] > a[i+gap] then
        begin
	  temp := a[i];
          a[i] := a[i+gap];
          a[i+gap] := temp;
          swapped := true;
        end;
    end;
  end;

begin
  Randomize;
  writeln('The data before sorting:');
  for i := low(data) to high(data) do
  begin
    data[i] := Random(high(data));
    write(data[i]:4);
  end;
  writeln;
  combSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
```



## Perl


```perl
sub combSort {
    my @arr = @_;
    my $gap = @arr;
    my $swaps = 1;
    while ($gap > 1 || $swaps) {
        $gap /= 1.25 if $gap > 1;
        $swaps = 0;
        foreach my $i (0 .. $#arr - $gap) {
            if ($arr[$i] > $arr[$i+$gap]) {
                @arr[$i, $i+$gap] = @arr[$i+$gap, $i];
                $swaps = 1;
            }
        }
    }
    return @arr;
}
```



## Perl 6

{{trans|Perl}}

```perl6
sub comb_sort ( @a is copy ) {
    my $gap = +@a;
    my $swaps = 1;
    while $gap > 1 or $swaps {
        $gap = ( ($gap * 4) div 5 ) || 1 if $gap > 1;

        $swaps = 0;
        for ^(+@a - $gap) -> $i {
            my $j = $i + $gap;
            if @a[$i] > @a[$j] {
                @a[$i, $j] .= reverse;
                $swaps = 1;
            }
        }
    }
    return @a;
}

my @weights = (^50).map: { 100 + ( 1000.rand.Int / 10 ) };
say @weights.sort.Str eq @weights.&comb_sort.Str ?? 'ok' !! 'not ok';

```



## Phix


```Phix
function comb_sort(sequence s)
integer gap = length(s)-1
    while 1 do
        gap = max(floor(gap/1.3),1)
        integer swapped = 0
        for i=1 to length(s)-gap do
            object si = s[i]
            if si>s[i+gap] then
                s[i] = s[i+gap]
                s[i+gap] = si
                swapped = 1
            end if
        end for
        if gap=1 and swapped=0 then exit end if
    end while
    return s
end function
```



## PHP


```php
function combSort($arr){
	$gap = count($arr);
        $swap = true;
	while ($gap > 1 || $swap){
		if($gap > 1) $gap /= 1.25;

		$swap = false;
		$i = 0;
		while($i+$gap < count($arr)){
			if($arr[$i] > $arr[$i+$gap]){
				list($arr[$i], $arr[$i+$gap]) = array($arr[$i+$gap],$arr[$i]);
				$swap = true;
			}
			$i++;
		}
	}
	return $arr;
}
```



## PicoLisp


```PicoLisp
(de combSort (Lst)
   (let (Gap (length Lst)  Swaps NIL)
      (while (or (> Gap 1) Swaps)
         (setq Gap (max 1 (/ (* Gap 4) 5)))
         (off Swaps)
         (use Lst
            (for (G (cdr (nth Lst Gap))  G  (cdr G))
               (when (> (car Lst) (car G))
                  (xchg Lst G)
                  (on Swaps) )
               (pop 'Lst) ) ) ) )
   Lst )
```

Output:

```txt
: (combSort (88 18 31 44 4 0 8 81 14 78 20 76 84 33 73 75 82 5 62 70))
-> (0 4 5 8 14 18 20 31 33 44 62 70 73 75 76 78 81 82 84 88)
```



## PL/I


```PL/I

/* From the pseudocode. */
comb_sort: procedure (A);
   declare A(*) fixed;
   declare t fixed;
   declare (i, gap) fixed binary (31);
   declare swaps bit (1) aligned;

   gap = hbound(A,1) - lbound(A,1);  /* initialize the gap size. */
   do until (gap <= 1 & swaps);
      /* update the gap value for a next comb. */
      put skip data (gap);
      gap = gap / 1.25e0;
      put skip data (gap);
      swaps = '1'b;
      /* a single "comb" over the array. */
      do i = lbound(A,1) by 1 until (i + gap >= hbound(A,1));
         if A(i) > A(i+gap) then
            do;
               t = A(i); A(i) = A(i+gap); A(i+gap) = t;
               swaps = '0'b; /* Flag a swap has occurred, so */
                             /* the list is not guaranteed sorted. */
            end;
       end;
   end;
end comb_sort;

```



## PowerShell

Massaging gap to always hit 11.  Based on PowerShell from [[Cocktail Sort]]

```PowerShell
function CombSort ($a) {
    $l = $a.Length
	$gap = 11
	while( $gap -lt $l )
	{
		$gap = [Math]::Floor( $gap*1.3 )
	}
	if( $l -gt 1 )
	{
		$hasChanged = $true
		:outer while ($hasChanged -or ( $gap -gt 1 ) ) {
			$count = 0
			$hasChanged = $false
			if( $gap -gt 1 ) {
				$gap = [Math]::Floor( $gap/1.3 )
			} else {
				$l--
			}
			for ($i = 0; $i -lt ( $l - $gap ); $i++) {
				if ($a[$i] -gt $a[$i+$gap]) {
					$a[$i], $a[$i+$gap] = $a[$i+$gap], $a[$i]
					$hasChanged = $true
					$count++
				}
			}
		}
	}
	$a
}

$l = 100; CombSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( -( $l - 1 ), $l - 1 ) } )
```



## PureBasic

Implementation of CombSort11.

```PureBasic
;sorts an array of integers
Procedure combSort11(Array a(1))
  Protected i, gap, swaps = 1
  Protected nElements = ArraySize(a())

  gap = nElements
  While (gap > 1) Or (swapped = 1)
    gap * 10 / 13
    If gap = 9 Or gap = 10: gap = 11:  EndIf
    If gap < 1: gap = 1: EndIf

    i = 0
    swaps = 0
    While (i + gap) <= nElements
      If a(i) > a(i + gap)
        Swap a(i), a(i + gap)
        swaps = 1
      EndIf
      i + 1
    Wend
  Wend
EndProcedure
```

Implementation of CombSort.

```PureBasic
;sorts an array of integers
Procedure combSort(Array a(1))
  Protected i, gap, swaps = 1
  Protected nElements = ArraySize(a())

  gap = nElements
  While (gap > 1) Or (swaps = 1)
    gap = Int(gap / 1.25)

    i = 0
    swaps = 0
    While (i + gap) <= nElements
      If a(i) > a(i + gap)
        Swap a(i), a(i + gap)
        swaps = 1
      EndIf
      i + 1
    Wend
  Wend
EndProcedure
```



## Python


```python>>>
 def combsort(input):
    gap = len(input)
    swaps = True
    while gap > 1 or swaps:
        gap = max(1, int(gap / 1.25))  # minimum gap is 1
        swaps = False
        for i in range(len(input) - gap):
            j = i+gap
            if input[i] > input[j]:
                input[i], input[j] = input[j], input[i]
                swaps = True


>>> y = [88, 18, 31, 44, 4, 0, 8, 81, 14, 78, 20, 76, 84, 33, 73, 75, 82, 5, 62, 70]
>>> combsort(y)
>>> assert y == sorted(y)
>>> y
[0, 4, 5, 8, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
>>>
```



## R


```R

comb.sort<-function(a){
  gap<-length(a)
  swaps<-1
  while(gap>1 & swaps==1){
    gap=floor(gap/1.3)
    if(gap<1){
      gap=1
      }
    swaps=0
    i=1
    while(i+gap<=length(a)){
        if(a[i]>a[i+gap]){
        a[c(i,i+gap)] <- a[c(i+gap,i)]
        swaps=1
        }
        i<-i+1
      }
  }
  return(a)
}


```




## Racket


```racket

#lang racket
(require (only-in srfi/43 vector-swap!))

(define (comb-sort xs)
  (define (ref i) (vector-ref xs i))
  (define (swap i j) (vector-swap! xs i j))
  (define (new gap) (max 1 (exact-floor (/ gap 1.25))))
  (define size (vector-length xs))
  (let loop ([gap size] [swaps 0])
    (unless (and (= gap 1) (= swaps 0))
      (loop (new gap)
            (for/fold ([swaps 0]) ([i (in-range 0 (- size gap))])
              (cond
                [(> (ref i) (ref (+ i gap)))
                 (swap i (+ i gap))
                 (+ swaps 1)]
                [swaps])))))
  xs)

```



## REXX


```rexx
/*REXX program  sorts  and displays  a  stemmed array  using the  comb sort  algorithm. */
call gen;                        w=length(#)     /*generate the  @  array elements.     */
call show       'before sort'                    /*display the  before  array elements. */
     say  copies('▒', 60)                        /*display a separator line  (a fence). */
call combSort  #                                 /*invoke the comb sort (with # entries)*/
call show       ' after sort'                    /*display the   after  array elements. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
combSort: procedure expose @.;   parse arg N     /*N:  is the number of  @  elements.   */
          g=N - 1                                /*G:  is the gap between the sort COMBs*/
                  do  until g<=1 & done;  done=1 /*assume sort is done  (so far).       */
                  g=g * 0.8  % 1                 /*equivalent to:   g=trunc( g / 1.25)  */
                  if g==0  then g=1              /*handle case of the gap is too small. */
                      do j=1  until $ >= N;    $=j + g       /*$:  temp index variable. */
                      if @.j > @.$  then do;   _=@.j;    @.j=@.$;   @.$=_;   done=0;   end
                      end   /*j*/
                  end       /*until*/            /* [↑]  swap two elements in the array.*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen:      @.   =                            ;       @.12 = "dodecagon         12"
          @.1  = '----polygon---  sides'    ;       @.13 = "tridecagon        13"
          @.2  = '
### =========== ====
'   ;       @.14 = "tetradecagon      14"
          @.3  = 'triangle           3'     ;       @.15 = "pentadecagon      15"
          @.4  = 'quadrilateral      4'     ;       @.16 = "hexadecagon       16"
          @.5  = 'pentagon           5'     ;       @.17 = "heptadecagon      17"
          @.6  = 'hexagon            6'     ;       @.18 = "octadecagon       18"
          @.7  = 'heptagon           7'     ;       @.19 = "enneadecagon      19"
          @.8  = 'octagon            8'     ;       @.20 = "icosagon          20"
          @.9  = 'nonagon            9'     ;       @.21 = "hectogon         100"
          @.10 = 'decagon           10'     ;       @.22 = "chiliagon       1000"
          @.11 = 'hendecagon        11'     ;       @.23 = "myriagon       10000"
                   do #=1  while  @.#\=='';  end;   #=#-1  /*find how many elements in @*/
          return                                 /* [↑]  adjust # because of the DO loop*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:     do k=1  for #; say right('element',15) right(k,w)  arg(1)":"  @.k;  end;  return
```

Data trivia:   A   ''hendecagon''   (also known as an   ''undecagon''
  or   ''unidecagon'')   is from the Greek word
''hendeka''   [eleven]   and   ''gon─''   [corner].


{{out|output|:}}
<pre style="height:80ex">
        element  1 before sort: ----polygon---  sides
        element  2 before sort:
### =========== ====

        element  3 before sort: triangle           3
        element  4 before sort: quadrilateral      4
        element  5 before sort: pentagon           5
        element  6 before sort: hexagon            6
        element  7 before sort: heptagon           7
        element  8 before sort: octagon            8
        element  9 before sort: nonagon            9
        element 10 before sort: decagon           10
        element 11 before sort: hendecagon        11
        element 12 before sort: dodecagon         12
        element 13 before sort: tridecagon        13
        element 14 before sort: tetradecagon      14
        element 15 before sort: pentadecagon      15
        element 16 before sort: hexadecagon       16
        element 17 before sort: heptadecagon      17
        element 18 before sort: octadecagon       18
        element 19 before sort: enneadecagon      19
        element 20 before sort: icosagon          20
        element 21 before sort: hectogon         100
        element 22 before sort: chiliagon       1000
        element 23 before sort: myriagon       10000
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
        element  1  after sort: ----polygon---  sides
        element  2  after sort:
### =========== ====

        element  3  after sort: chiliagon       1000
        element  4  after sort: decagon           10
        element  5  after sort: dodecagon         12
        element  6  after sort: enneadecagon      19
        element  7  after sort: hectogon         100
        element  8  after sort: hendecagon        11
        element  9  after sort: heptadecagon      17
        element 10  after sort: heptagon           7
        element 11  after sort: hexadecagon       16
        element 12  after sort: hexagon            6
        element 13  after sort: icosagon          20
        element 14  after sort: myriagon       10000
        element 15  after sort: nonagon            9
        element 16  after sort: octadecagon       18
        element 17  after sort: octagon            8
        element 18  after sort: pentadecagon      15
        element 19  after sort: pentagon           5
        element 20  after sort: quadrilateral      4
        element 21  after sort: tetradecagon      14
        element 22  after sort: triangle           3
        element 23  after sort: tridecagon        13

```



## Ring


```ring

aList = [3,5,1,2,7,4,8,3,6,4,1]
see combsort(aList)

func combsort t
     gapd = 1.2473
     gap =  len(t)
     swaps = 0
     while gap + swaps > 1
           k = 0
           swaps = 0
           if gap > 1 gap = floor(gap / gapd) ok
           for k = 1 to len(t) - gap
               if t[k] > t[k + gap]
                  temp = t[k]
                  t[k] = t[k + gap]
                  t[k + gap] = temp
                  swaps = swaps + 1 ok
           next
     end
        return t

```



## Ruby


```ruby
class Array
  def combsort!
    gap = size
    swaps = true
    while gap > 1 or swaps
      gap = [1, (gap / 1.25).to_i].max
      swaps = false
      0.upto(size - gap - 1) do |i|
        if self[i] > self[i+gap]
          self[i], self[i+gap] = self[i+gap], self[i]
          swaps = true
        end
      end
    end
    self
  end
end

p [23, 76, 99, 58, 97, 57, 35, 89, 51, 38, 95, 92, 24, 46, 31, 24, 14, 12, 57, 78].combsort!
```

results in

```txt
[12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
```



## Scala

===Imperative version (Ugly, side effects)===

```Scala
object CombSort extends App {
  val ia = Array(28, 44, 46, 24, 19, 2, 17, 11, 25, 4)
  val ca = Array('X', 'B', 'E', 'A', 'Z', 'M', 'S', 'L', 'Y', 'C')

  def sorted[E](input: Array[E])(implicit ord: Ordering[E]): Array[E] = {
    import ord._
    var gap = input.length
    var swapped = true
    while (gap > 1 || swapped) {
      if (gap > 1) gap = (gap / 1.3).toInt
      swapped = false
      for (i <- 0 until input.length - gap)
        if (input(i) >= input(i + gap)) {
          val t = input(i)
          input(i) = input(i + gap)
          input(i + gap) = t
          swapped = true
        }
    }
    input
  }

  println(s"Unsorted : ${ia.mkString("[", ", ", "]")}")
  println(s"Sorted   : ${sorted(ia).mkString("[", ", ", "]")}\n")

  println(s"Unsorted : ${ca.mkString("[", ", ", "]")}")
  println(s"Sorted   : ${sorted(ca).mkString("[", ", ", "]")}")

}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/7ykMPZx/0 ScalaFiddle (JavaScript)] or by [https://scastie.scala-lang.org/Gp1ZcxnPQAKvToWFZLU7OA Scastie (JVM)].

## Sather


```sather
class SORT{T < $IS_LT{T}} is

  private swap(inout a, inout b:T) is
    temp ::= a;
    a := b;
    b := temp;
  end;

-- ---------------------------------------------------------------------------------
  comb_sort(inout a:ARRAY{T}) is
    gap ::= a.size;
    swapped ::= true;
    loop until!(gap <= 1 and ~swapped);
      if gap > 1 then
        gap := (gap.flt / 1.25).int;
      end;
      i ::= 0;
      swapped := false;
      loop until! ( (i + gap) >= a.size );
        if (a[i] > a[i+gap]) then
	  swap(inout a[i], inout a[i+gap]);
	  swapped := true;
	end;
        i := i + 1;
      end;
    end;
  end;
end;

class MAIN is
  main is
    a:ARRAY{INT} := |88, 18, 31, 44, 4, 0, 8, 81, 14, 78, 20, 76, 84, 33, 73, 75, 82, 5, 62, 70|;
    b ::= a.copy;
    SORT{INT}::comb_sort(inout b);
    #OUT + b + "\n";
  end;
end;
```



## Sidef


```ruby
func comb_sort(arr) {
    var gap = arr.len;
    var swaps = true;
    while (gap > 1 || swaps) {
        gap.div!(1.25).int! if (gap > 1);
        swaps = false;
        for i in ^(arr.len - gap) {
            if (arr[i] > arr[i+gap]) {
                arr[i, i+gap] = arr[i+gap, i];
                swaps = true;
            }
        }
    }
    return arr;
}
```



## Swift

{{trans|C}}

```Swift
func combSort(inout list:[Int]) {
    var swapped = true
    var gap = list.count

    while gap > 1 || swapped {
        gap = gap * 10 / 13

        if gap == 9 || gap == 10 {
            gap = 11
        } else if gap < 1 {
            gap = 1
        }

        swapped = false

        for var i = 0, j = gap; j < list.count; i++, j++ {
            if list[i] > list[j] {
                (list[i], list[j]) = (list[j], list[i])
                swapped = true
            }
        }
    }
}
```



## Tcl


```tcl
proc combsort {input} {
    set gap [llength $input]
    while 1 {
	set gap [expr {int(floor($gap / 1.3))}]
	set swaps 0
	for {set i 0} {$i+$gap < [llength $input]} {incr i} {
	    set j [expr {$i+$gap}]
	    if {[lindex $input $i] > [lindex $input $j]} {
		set tmp [lindex $input $i]
		lset input $i [lindex $input $j]
		lset input $j $tmp
		incr swaps
	    }
	}
	if {$gap <= 1 && !$swaps} break
    }
    return $input
}

set data {23 76 99 58 97 57 35 89 51 38 95 92 24 46 31 24 14 12 57 78}
puts [combsort $data]
```

Produces this output:

```txt
12 14 23 24 24 31 35 38 46 51 57 57 58 76 78 89 92 95 97 99
```


=={{header|TI-83 BASIC}}==
Requires [[Insertion sort#TI-83 BASIC|prgmSORTINS]]. Gap division of 1.3. Switches to [[Insertion sort]] when gap is less than 5.
 :L<sub>1</sub>→L<sub>2</sub>
 :dim(L<sub>2</sub>)→A
 :While A>5 and B=0
 :int(A/1.3)→A
 :1→C
 :0→B
 :While (C+A)≥dim(L<sub>2</sub>)
 :If L<sub>2</sub>(C)>L<sub>2</sub>(C+A)
 :Then
 :L<sub>2</sub>(C)→D
 :L<sub>2</sub>(C+A)→L<sub>2</sub>(C)
 :D→L<sub>2</sub>(C+A)
 :1→B
 :End
 :C+1→C
 :End
 :DelVar A
 :DelVar B
 :DelVar C
 :DelVar D
 :L<sub>1</sub>→L<sub>3</sub>
 :L<sub>2</sub>→L<sub>1</sub>
 :[[Insertion sort#TI-83 BASIC|prgmSORTINS]]
 :L<sub>3</sub>→L<sub>1</sub>
 :DelVar L<sub>3</sub>
 :Return

{{omit from|GUISS}}


## uBasic/4tH

<lang>PRINT "Comb sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Combsort (n)
  PROC _ShowArray (n)
PRINT

END


_Combsort PARAM (1)                    ' Combsort subroutine
  LOCAL(4)
  b@ = a@
  c@ = 1

  DO WHILE (b@ > 1) + c@

    b@ = (b@ * 10) / 13

    IF (b@ = 9) + (b@ = 10) THEN b@ = 11
    IF b@ < 1 THEN b@ = 1

    c@ = 0
    d@ = 0
    e@ = b@

    DO WHILE e@ < a@
      IF @(d@) > @(e@) THEN PROC _Swap (d@, e@) : c@ = 1
      d@ = d@ + 1
      e@ = e@ + 1
    LOOP
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



## VBA

{[trans|Phix}}
```vb
Function comb_sort(ByVal s As Variant) As Variant
    Dim gap As Integer: gap = UBound(s)
    Dim swapped As Integer
    Do While True
        gap = WorksheetFunction.Max(WorksheetFunction.Floor_Precise(gap / 1.3), 1)
        swapped = False
        For i = 0 To UBound(s) - gap
            si = Val(s(i))
            If si > Val(s(i + gap)) Then
                s(i) = s(i + gap)
                s(i + gap) = CStr(si)
                swapped = True
            End If
        Next i
        If gap = 1 And Not swapped Then Exit Do
    Loop
    comb_sort = s
End Function

Public Sub main()
    Dim s(9) As Variant
    For i = 0 To 9
        s(i) = CStr(Int(1000 * Rnd))
    Next i
    Debug.Print Join(s, ", ")
    Debug.Print Join(comb_sort(s), ", ")
End Sub
```
{{out}}

```txt
45, 414, 862, 790, 373, 961, 871, 56, 949, 364
45, 56, 364, 373, 414, 790, 862, 871, 949, 961
```


## zkl

{{trans|D}}

```zkl
fcn combSort(list){
   len,gap,swaps:=list.len(),len,True;
   while(gap>1 or swaps){
      gap,swaps=(1).max(gap.toFloat()/1.2473), False;
      foreach i in (len - gap){
         if(list[i]>list[i + gap]){
	    list.swap(i,i + gap);
	    swaps=True;
	 }
      }
   }
   list
}
```


```zkl
combSort(List(28, 44, 46, 24, 19, 2, 17, 11, 25, 4)).println();
combSort("This is a test".toData()).text.println();
```

{{out}}

```txt

L(2,4,11,17,19,24,25,28,44,46)
   Taehiissstt

```


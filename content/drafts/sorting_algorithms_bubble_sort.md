+++
title = "Sorting algorithms/Bubble sort"
description = ""
date = 2019-10-20T08:02:09Z
aliases = []
[extra]
id = 1628
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}

;Task:
Sort an array of elements using the bubble sort algorithm.   The elements must have a total order and the index of the array can be of any discrete type.   For languages where this is not possible, sort an array of integers.

The bubble sort is generally considered to be the simplest sorting algorithm.

Because of its simplicity and ease of visualization, it is often taught in introductory computer science courses.

Because of its abysmal O(n<sup>2</sup>) performance, it is not used often for large (or even medium-sized) datasets.

The bubble sort works by passing sequentially over a list, comparing each value to the one immediately after it.   If the first value is greater than the second, their positions are switched.   Over a number of passes, at most equal to the number of elements in the list, all of the values drift into their correct positions (large values "bubble" rapidly toward the end, pushing others down around them).
Because each pass finds the maximum item and puts it at the end, the portion of the list to be sorted can be reduced at each pass.
A boolean variable is used to track whether any changes have been made in the current pass; when a pass completes without changing anything, the algorithm exits.

This can be expressed in pseudo-code as follows (assuming 1-based indexing):
 '''repeat'''
     '''if''' itemCount <= 1
         '''return'''
     hasChanged := false
     '''decrement''' itemCount
     '''repeat with''' index '''from''' 1 '''to''' itemCount
         '''if''' (item '''at''' index) > (item '''at''' (index + 1))
             swap (item '''at''' index) with (item '''at''' (index + 1))
             hasChanged := true
 '''until''' hasChanged = '''false'''


;References:
* The article on [[wp:Bubble_sort|Wikipedia]].
* Dance [http://www.youtube.com/watch?v=lyZQPjUT5B4&feature=youtu.be interpretation].





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.

```360 Assembly
*        Bubble Sort               01/11/2014 & 23/06/2016
BUBBLE   CSECT
         USING  BUBBLE,R13,R12     establish base registers
SAVEAREA B      STM-SAVEAREA(R15)  skip savearea
         DC     17F'0'             my savearea
STM      STM    R14,R12,12(R13)    save calling context
         ST     R13,4(R15)         link mySA->prevSA
         ST     R15,8(R13)         link prevSA->mySA
         LR     R13,R15            set mySA & set 4K addessability
         LA     R12,2048(R13)      .
         LA     R12,2048(R12)      set 8K addessability
         L      RN,N               n
         BCTR   RN,0               n-1
         DO UNTIL=(LTR,RM,Z,RM)    repeat  ------------------------+
         LA     RM,0                 more=false                    |
         LA     R1,A                 @a(i)                         |
         LA     R2,4(R1)             @a(i+1)                       |
         LA     RI,1                 i=1                           |
         DO WHILE=(CR,RI,LE,RN)      for i=1 to n-1  ------------+ |
         L      R3,0(R1)               a(i)                      | |
         IF     C,R3,GT,0(R2)          if a(i)>a(i+1) then  ---+ | |
         L      R9,0(R1)                 r9=a(i)               | | |
         L      R3,0(R2)                 r3=a(i+1)             | | |
         ST     R3,0(R1)                 a(i)=r3               | | |
         ST     R9,0(R2)                 a(i+1)=r9             | | |
         LA     RM,1                     more=true             | | |
         ENDIF  ,                      end if  <---------------+ | |
         LA     RI,1(RI)               i=i+1                     | |
         LA     R1,4(R1)               next a(i)                 | |
         LA     R2,4(R2)               next a(i+1)               | |
         ENDDO  ,                    end for  <------------------+ |
         ENDDO  ,                  until not more  <---------------+
         LA     R3,PG              pgi=0
         LA     RI,1               i=1
         DO WHILE=(C,RI,LE,N)      do i=1 to n  -------+
         LR     R1,RI                i                 |
         SLA    R1,2                 .                 |
         L      R2,A-4(R1)           a(i)              |
         XDECO  R2,XDEC              edit a(i)         |
         MVC    0(4,R3),XDEC+8       output a(i)       |
         LA     R3,4(R3)             pgi=pgi+4         |
         LA     RI,1(RI)             i=i+1             |
         ENDDO  ,                  end do  <-----------+
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       restore caller savearea
         LM     R14,R12,12(R13)    restore context
         XR     R15,R15            set return code to 0
         BR     R14                return to caller
A     DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
      DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
N        DC     A((N-A)/L'A)       number of items of a *
PG       DC     CL80' '
XDEC     DS     CL12
         LTORG
         YREGS
RI       EQU    6                  i
RN       EQU    7                  n-1
RM       EQU    8                  more
         END    BUBBLE
```

{{out}}

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



## ACL2


```Lisp
(defun bubble (xs)
   (if (endp (rest xs))
       (mv nil xs)
       (let ((x1 (first xs))
             (x2 (second xs)))
         (if (> x1 x2)
             (mv-let (_ ys)
                     (bubble (cons x1 (rest (rest xs))))
                (declare (ignore _))
                (mv t (cons x2 ys)))
             (mv-let (has-changed ys)
                     (bubble (rest xs))
                (mv has-changed (cons x1 ys)))))))

(defun bsort-r (xs limit)
   (declare (xargs :measure (nfix limit)))
   (if (zp limit)
       xs
       (mv-let (has-changed ys)
               (bubble xs)
          (if has-changed
              (bsort-r ys (1- limit))
              ys))))

(defun bsort (xs)
   (bsort-r xs (len xs)))
```



## ActionScript


```actionscript
public function bubbleSort(toSort:Array):Array
{
	var changed:Boolean = false;

	while (!changed)
	{
		changed = true;

		for (var i:int = 0; i < toSort.length - 1; i++)
		{
			if (toSort[i] > toSort[i + 1])
			{
				var tmp:int = toSort[i];
				toSort[i] = toSort[i + 1];
				toSort[i + 1] = tmp;

				changed = false;
			}
		}
	}

	return toSort;
}
```



## Ada

{{works with|GCC|4.1.2}}


```ada
generic
 type Element is private;
 with function "=" (E1, E2 : Element) return Boolean is <>;
 with function "<" (E1, E2 : Element) return Boolean is <>;
 type Index is (<>);
 type Arr is array (Index range <>) of Element;
procedure Bubble_Sort (A : in out Arr);

procedure Bubble_Sort (A : in out Arr) is
 Finished : Boolean;
 Temp     : Element;
begin
 loop
  Finished := True;
  for J in A'First .. Index'Pred (A'Last) loop
   if A (Index'Succ (J)) < A (J) then
    Finished := False;
    Temp := A (Index'Succ (J));
    A (Index'Succ (J)) := A (J);
    A (J) := Temp;
   end if;
  end loop;
  exit when Finished;
 end loop;
end Bubble_Sort;

--  Example of usage:
with Ada.Text_IO; use Ada.Text_IO;
with Bubble_Sort;
procedure Main is
 type Arr is array (Positive range <>) of Integer;
 procedure Sort is new
  Bubble_Sort
   (Element => Integer,
    Index   => Positive,
    Arr     => Arr);
 A : Arr := (1, 3, 256, 0, 3, 4, -1);
begin
 Sort (A);
 for J in A'Range loop
  Put (Integer'Image (A (J)));
 end loop;
 New_Line;
end Main;
```



## ALGOL 68


```algol68
MODE DATA = INT;
PROC swap = (REF[]DATA slice)VOID:
(
  DATA tmp = slice[1];
  slice[1] := slice[2];
  slice[2] := tmp
);

PROC sort = (REF[]DATA array)VOID:
(
  BOOL sorted;
  INT shrinkage := 0;
  FOR size FROM UPB array - 1 BY -1 WHILE
    sorted := TRUE;
    shrinkage +:= 1;
    FOR i FROM LWB array TO size DO
      IF array[i+1] < array[i] THEN
        swap(array[i:i+1]);
        sorted := FALSE
      FI
    OD;
    NOT sorted
  DO SKIP OD
);

main:(
  [10]INT random := (1,6,3,5,2,9,8,4,7,0);

  printf(($"Before: "10(g(3))l$,random));
  sort(random);
  printf(($"After: "10(g(3))l$,random))
)
```

{{out}}

```txt

 Before:  +1 +6 +3 +5 +2 +9 +8 +4 +7 +0
 After:  +0 +1 +2 +3 +4 +5 +6 +7 +8 +9

```



## ALGOL W


```algolw
begin
    % As algol W does not allow overloading, we have to have type-specific   %
    % sorting procedures - this bubble sorts an integer array                %
    % as there is no way for the procedure to determine the array bounds, we %
    % pass the lower and upper bounds in lb and ub                           %
    procedure bubbleSortIntegers( integer array item( * )
                                ; integer value lb
                                ; integer value ub
                                ) ;
    begin
        integer lower, upper;

        lower := lb;
        upper := ub;

        while
            begin
                logical swapped;
                upper   := upper - 1;
                swapped := false;
                for i := lower until upper
                do begin
                    if item( i ) > item( i + 1 )
                    then begin
                        integer val;
                        val           := item( i );
                        item( i )     := item( i + 1 );
                        item( i + 1 ) := val;
                        swapped       := true;
                    end if_must_swap ;
                end for_i ;
                swapped
            end
        do  begin end;
    end bubbleSortIntegers ;

    begin % test the bubble sort                                             %
        integer array data( 1 :: 10 );

        procedure writeData ;
        begin
            write( data( 1 ) );
            for i := 2 until 10 do writeon( data( i ) );
        end writeData ;

        % initialise data to unsorted values                                 %
        integer       dPos;
        dPos  := 1;
        for i := 16, 2, -6, 9, 90, 14, 0, 23, 8, 9
        do begin
            data( dPos ) := i;
            dPos         := dPos + 1;
        end for_i ;

        i_w := 3; s_w := 1; % set output format %
        writeData;
        bubbleSortIntegers( data, 1, 10 );
        writeData;
    end test
end.
```

{{out}}

```txt

 16   2  -6   9  90  14   0  23   8   9
 -6   0   2   8   9   9  14  16  23  90

```



## Arendelle


A function that returns a sorted version of it's x input


```txt
&lt; x &gt; ( i , 0 )

( sjt , 1; 0; 0 ) // swapped:0 / j:1 / temp:2

[ @sjt = 1 ,

	( sjt , 0 )
	( sjt[ 1 ] , +1 )

	( i , 0 )

	[ @i &lt; @x? - @sjt[ 1 ],

		{ @x[ @i ] &lt; @x[ @i + 1 ],

			( sjt[ 2 ] , @x[ @i ] )
			( x[ @i ] , @x[ @i + 1 ] )
			( x[ @i + 1 ] , @sjt[ 2 ] )
			( sjt , 1 )
		}

		( i , +1 )
	]
]

( return , @x )
```



## Arturo



```arturo
bubbleSort [items]{
    loop $(range $(size items)-1 0) [n]{
        swapped false
        loop $(range 0 n-1) [i]{
            if items.[i]>items.[i+1] {
                tmp items.[i+1]
                items.[i+1] items.[i]
                items.[i] tmp
                swapped true
            }
        }
        if $(not swapped) { return items }
    }
    return items
}

print $(bubbleSort #(3 1 2 8 5 7 9 4 6))
```


{{out}}


```txt
#(1 2 3 4 5 6 7 8 9)
```



## AutoHotkey


```AutoHotkey
var =
(
dog
cat
pile
abc
)
MsgBox % bubblesort(var)

bubblesort(var) ; each line of var is an element of the array
{
  StringSplit, array, var, `n
  hasChanged = 1
  size := array0
  While hasChanged
  {
    hasChanged = 0
    Loop, % (size - 1)
    {
      i := array%A_Index%
      aj := A_Index + 1
      j := array%aj%
      If (j < i)
      {
        temp := array%A_Index%
        array%A_Index% := array%aj%
        array%aj% := temp
        hasChanged = 1
      }
    }
  }
  Loop, % size
    sorted .= array%A_Index% . "`n"
  Return sorted
}
```



## AWK

Sort the standard input and print it to standard output.

```awk
{ # read every line into an array
  line[NR] = $0
}
END { # sort it with bubble sort
  do {
    haschanged = 0
    for(i=1; i < NR; i++) {
      if ( line[i] > line[i+1] ) {
	t = line[i]
	line[i] = line[i+1]
	line[i+1] = t
	haschanged = 1
      }
    }
  } while ( haschanged == 1 )
  # print it
  for(i=1; i <= NR; i++) {
    print line[i]
  }
}
```


GNU awk contains built in functions for sorting, but POSIX
Awk doesn't. Here is a generic bubble sort() implementation that you
can copy/paste to your
Awk programs. Adapted from the above example. Note that it
is not possible to return arrays from Awk functions so the
array is "edited in place". The extra parameters passed in
function's argument list is a well known trick to define local
variables.


```awk

# Test this example file from command line with:
#
#    awk -f file.awk /dev/null
#
# Code by Jari Aalto <jari.aalto A T cante net>
# Licensed and released under GPL-2+, see http://spdx.org/licenses

function alen(array,   dummy, len) {
    for (dummy in array)
        len++;

    return len;
}

function sort(array,   haschanged, len, tmp, i)
{
    len = alen(array)
    haschanged = 1

    while ( haschanged == 1 )
    {
        haschanged = 0

        for (i = 1; i <= len - 1; i++)
        {
            if (array[i] > array[i+1])
            {
                tmp = array[i]
                array[i] = array[i + 1]
                array[i + 1] = tmp
                haschanged = 1
            }
        }
    }
}

# An Example. Sorts array to order: b, c, z
{
    array[1] = "c"
    array[2] = "z"
    array[3] = "b"
    sort(array)
    print array[1] " " array[2] " " array[3]
    exit
}

```



## bash


I hope to see vastly improved versions of bubble_sort.


```bash

$ function bubble_sort() {
    local a=("$@")
    local n
    local i
    local j
    local t
    ft=(false true)
    n=${#a[@]} # array length
    i=n
    while ${ft[$(( 0 < i ))]}
    do
        j=0
        while ${ft[$(( j+1 < i ))]}
        do
            if ${ft[$(( a[j+1] < a[j] ))]}
            then
    	        t=${a[j+1]}
    	        a[j+1]=${a[j]}
    	        a[j]=$t
    	    fi
            t=$(( ++j ))
        done
        t=$(( --i ))
    done
    echo ${a[@]}
}

> > > > > > > > > > > > > > > > > > > > > > > > > $ # this line output from bash
$ bubble_sort 3 2 8
2 3 8
$ # create an array variable
$ a=(2 45 83 89 1 82 69 88 112 99 0 82 58 65 782 74 -31 104 4 2)
$ bubble_sort ${a[@]}
-31 0 1 2 2 4 45 58 65 69 74 82 82 83 88 89 99 104 112 782
$ b=($( bubble_sort ${a[@]} ) )
$ echo ${#b[@]}
20
$ echo ${b[@]}
-31 0 1 2 2 4 45 58 65 69 74 82 82 83 88 89 99 104 112 782
$

```



## BASIC

{{works with|QuickBasic|4.5}}

{{trans|Java}}
Assume numbers are in a DIM of size "size" called "nums".

```qbasic

DO
  changed = 0
  FOR I = 1 to size -1
    IF nums(I) > nums(I + 1) THEN
      tmp = nums(I)
      nums(I) = nums(I + 1)
      nums(I + 1) = tmp
      changed = 1
    END IF
  NEXT
LOOP WHILE(NOT changed)
```


=
## Applesoft BASIC
=

```basic
0 GOSUB 7 : IC = I%(0)
1 FOR HC = -1 TO 0
2     LET IC = IC - 1
3     FOR I = 1 TO IC
4         IF I%(I) > I%(I + 1) THEN H = I%(I) : I%(I) = I%(I + 1) : I%(I + 1) = H : HC = -2 * (IC > 1)
5 NEXT I, HC
6 GOSUB 9 : END
7 DIM I%(18000) : I%(0) = 50
8 FOR I = 1 TO I%(0) : I%(I) = INT (RND(1) * 65535) - 32767 : NEXT
9 FOR I = 1 TO I%(0) : PRINT I%(I)" "; : NEXT I : PRINT : RETURN
```


=
## Sinclair ZX81 BASIC
=
Works with the 1k RAM model. For simplicity, and to make it easy to animate the sort as it is going on, this implementation sorts a string of eight-bit unsigned integers which can be treated as character codes; it could easily be amended to sort an array of numbers or an array of strings, but the array would need to be dimensioned at the start.

```basic
 10 LET S$="FIRE BURN AND CAULDRON BUBBLE"
 20 PRINT S$
 30 LET L=LEN S$-1
 40 LET C=0
 50 FOR I=1 TO L
 60 IF S$(I)<=S$(I+1) THEN GOTO 120
 70 LET T$=S$(I)
 80 LET S$(I)=S$(I+1)
 90 LET S$(I+1)=T$
100 PRINT AT 0,I-1;S$(I TO I+1)
110 LET C=1
120 NEXT I
130 LET L=L-1
140 IF C THEN GOTO 40
```

{{out}}

```txt
    AABBBBCDDEEFILLNNNORRRUUU
```


=
## BASIC256
=
{{works with|BASIC256 }}

```basic256

Dim a(11): ordered=false
print "Original set"
For n = 0 to 9
a[n]=int(rand*20+1)
print a[n]+", ";
next n
#algorithm
while ordered=false
   ordered=true
   For n = 0 to 9
      if a[n]> a[n+1] then
          x=a[n]
          a[n]=a[n+1]
          a[n+1]=x
          ordered=false
       end if
    next n
end while

print
print "Ordered set"
For n = 1 to 10
print a[n]+", ";
next n

```

{{out}}(example)

```txt

Original set
2, 10, 17, 13, 20, 14, 3, 17, 16, 16,
Ordered set
2, 3, 10, 13, 14, 16, 16, 17, 17, 20,

```


=
## BBC BASIC
=
The Bubble sort is very inefficient for 99% of cases. This routine uses a couple of 'tricks' to try and mitigate the inefficiency to a limited extent.  Note that the array index is assumed to start at zero.

```bbcbasic
      DIM test(9)
      test() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCbubblesort(test(), 10)
      FOR i% = 0 TO 9
        PRINT test(i%) ;
      NEXT
      PRINT
      END

      DEF PROCbubblesort(a(), n%)
      LOCAL i%, l%
      REPEAT
        l% = 0
        FOR i% = 1 TO n%-1
          IF a(i%-1) > a(i%) THEN
            SWAP a(i%-1),a(i%)
            l% = i%
          ENDIF
        NEXT
        n% = l%
      UNTIL l% = 0
      ENDPROC
```

{{out}}

```txt

       -31         0         1         2         2         4        65        83        99       782

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "BubblSrt.bas"
110 RANDOMIZE
120 NUMERIC ARRAY(-5 TO 9)
130 CALL INIT(ARRAY)
140 CALL WRITE(ARRAY)
150 CALL BUBBLESORT(ARRAY)
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
280 DEF BUBBLESORT(REF A)
290   DO
300     LET CH=0
310     FOR I=LBOUND(A) TO UBOUND(A)-1
320       IF A(I)>A(I+1) THEN LET T=A(I):LET A(I)=A(I+1):LET A(I+1)=T:LET CH=1
330     NEXT
340   LOOP WHILE CH
350 END DEF
```



## C


```c
#include <stdio.h>

void bubble_sort (int *a, int n) {
    int i, t, j = n, s = 1;
    while (s) {
        s = 0;
        for (i = 1; i < j; i++) {
            if (a[i] < a[i - 1]) {
                t = a[i];
                a[i] = a[i - 1];
                a[i - 1] = t;
                s = 1;
            }
        }
        j--;
    }
}

int main () {
    int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
    int n = sizeof a / sizeof a[0];
    int i;
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    bubble_sort(a, n);
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    return 0;
}

```

{{out}}

```txt

4 65 2 -31 0 99 2 83 782 1
-31 0 1 2 2 4 65 83 99 782

```



## C++

Uses C++11. Compile with
 g++ -std=c++11 bubble.cpp

```cpp
#include <algorithm>
#include <iostream>
#include <iterator>

template <typename RandomAccessIterator>
void bubble_sort(RandomAccessIterator begin, RandomAccessIterator end) {
  bool swapped = true;
  while (begin != end-- && swapped) {
    swapped = false;
    for (auto i = begin; i != end; ++i) {
      if (*(i + 1) < *i) {
        std::iter_swap(i, i + 1);
        swapped = true;
      }
    }
  }
}

int main() {
  int a[] = {100, 2, 56, 200, -52, 3, 99, 33, 177, -199};
  bubble_sort(std::begin(a), std::end(a));
  copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
  std::cout << "\n";
}
```

{{out}}

```txt

-199 -52 2 3 33 56 99 100 177 200

```


## C#
{{works with|C sharp|C#|3.0+}}

```c#
using System;
using System.Collections.Generic;

namespace RosettaCode.BubbleSort
{
    public static class BubbleSortMethods
    {
        //The "this" keyword before the method parameter identifies this as a C# extension
        //method, which can be called using instance method syntax on any generic list,
        //without having to modify the generic List<T> code provided by the .NET framework.
        public static void BubbleSort<T>(this List<T> list) where T : IComparable
        {
            bool madeChanges;
            int itemCount = list.Count;
            do
            {
                madeChanges = false;
                itemCount--;
                for (int i = 0; i < itemCount; i++)
                {
                    if (list[i].CompareTo(list[i + 1]) > 0)
                    {
                        T temp = list[i + 1];
                        list[i + 1] = list[i];
                        list[i] = temp;
                        madeChanges = true;
                    }
                }
            } while (madeChanges);
        }
    }

    //A short test program to demonstrate the BubbleSort. The compiler will change the
    //call to testList.BubbleSort() into one to BubbleSortMethods.BubbleSort<T>(testList).
    class Program
    {
        static void Main()
        {
            List<int> testList = new List<int> { 3, 7, 3, 2, 1, -4, 10, 12, 4 };
            testList.BubbleSort();
            foreach (var t in testList) Console.Write(t + " ");
        }
    }
}
```



## Clojure

Bubble sorts a Java ArrayList in place. Uses 'doseq' iteration construct with a short-circuit when a pass didn't produce any change, and within the pass, an atomic 'changed' variable that gets reset whenever a change occurs.


```clojure
(ns bubblesort
  (:import java.util.ArrayList))

(defn bubble-sort
  "Sort in-place.
  arr must implement the Java List interface and should support
  random access, e.g. an ArrayList."
  ([arr] (bubble-sort compare arr))
  ([cmp arr]
     (letfn [(swap! [i j]
                    (let [t (.get arr i)]
                      (doto arr
                        (.set i (.get arr j))
                        (.set j t))))
             (sorter [stop-i]
                     (let [changed (atom false)]
                       (doseq [i (range stop-i)]
                         (if (pos? (cmp (.get arr i) (.get arr (inc i))))
                           (do
                             (swap! i (inc i))
                             (reset! changed true))))
                       @changed))]
       (doseq [stop-i (range (dec (.size arr)) -1 -1)
               :while (sorter stop-i)])
       arr)))

(println (bubble-sort (ArrayList. [10 9 8 7 6 5 4 3 2 1])))
```


Purely functional version working on Clojure sequences:

```clojure
(defn- bubble-step
  "was-changed: whether any elements prior to the current first element
  were swapped;
  returns a two-element vector [partially-sorted-sequence is-sorted]"
 [less? xs was-changed]
  (if (< (count xs) 2)
    [xs (not was-changed)]
    (let [[x1 x2 & xr] xs
	  first-is-smaller   (less? x1 x2)
	  is-changed         (or was-changed (not first-is-smaller))
	  [smaller larger]   (if first-is-smaller [x1 x2] [x2 x1])
	  [result is-sorted] (bubble-step
			      less? (cons larger xr) is-changed)]
      [(cons smaller result) is-sorted])))

(defn bubble-sort
  "Takes an optional less-than predicate and a sequence.
  Returns the sorted sequence.
  Very inefficient (O(n²))"
  ([xs] (bubble-sort <= xs))
  ([less? xs]
     (let [[result is-sorted] (bubble-step less? xs false)]
       (if is-sorted
	 result
	 (recur less? result)))))

(println (bubble-sort [10 9 8 7 6 5 4 3 2 1]))
```



## CMake

Only for lists of integers.


```cmake
# bubble_sort(var [value1 value2...]) sorts a list of integers.
function(bubble_sort var)
  math(EXPR last "${ARGC} - 1")  # Prepare to sort ARGV[1]..ARGV[last].
  set(again YES)
  while(again)
    set(again NO)
    math(EXPR last "${last} - 1")               # Decrement last index.
    foreach(index RANGE 1 ${last})              # Loop for each index.
      math(EXPR index_plus_1 "${index} + 1")
      set(a "${ARGV${index}}")                  # a = ARGV[index]
      set(b "${ARGV${index_plus_1}}")           # b = ARGV[index + 1]
      if(a GREATER "${b}")                      # If a > b...
        set(ARGV${index} "${b}")                # ...then swap a, b
        set(ARGV${index_plus_1} "${a}")         #    inside ARGV.
        set(again YES)
      endif()
    endforeach(index)
  endwhile()

  set(answer)
  math(EXPR last "${ARGC} - 1")
  foreach(index RANGE 1 "${last}")
    list(APPEND answer "${ARGV${index}}")
  endforeach(index)
  set("${var}" "${answer}" PARENT_SCOPE)
endfunction(bubble_sort)
```



```cmake
bubble_sort(result 33 11 44 22 66 55)
message(STATUS "${result}")
```



```txt
-- 11;22;33;44;55;66
```



## COBOL

This is a complete program that demonstrates the bubble sort algorithm in COBOL.
<br/>This version is for COBOL-74 which does not have in-line performs, nor END-IF and related constructs.

```cobol

       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      BUBBLESORT.
       AUTHOR.                          DAVE STRATFORD.
       DATE-WRITTEN.                    MARCH 2010.
       INSTALLATION.                    HEXAGON SYSTEMS LIMITED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 ICL VME.
       OBJECT-COMPUTER.                 ICL VME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FA-INPUT-FILE  ASSIGN FL01.
           SELECT FB-OUTPUT-FILE ASSIGN FL02.

       DATA DIVISION.
       FILE SECTION.

       FD  FA-INPUT-FILE.
       01  FA-INPUT-REC.
         03  FA-DATA                    PIC S9(6).

       FD  FB-OUTPUT-FILE.
       01  FB-OUTPUT-REC                PIC S9(6).

       WORKING-STORAGE SECTION.
       01  WA-IDENTITY.
         03  WA-PROGNAME                PIC X(10) VALUE "BUBBLESORT".
         03  WA-VERSION                 PIC X(6) VALUE "000001".

       01  WB-TABLE.
         03  WB-ENTRY                   PIC 9(8) COMP SYNC OCCURS 100000
                                                  INDEXED BY WB-IX-1.

       01  WC-VARS.
         03  WC-SIZE                    PIC S9(8) COMP SYNC.
         03  WC-TEMP                    PIC S9(8) COMP SYNC.
         03  WC-END                     PIC S9(8) COMP SYNC.
         03  WC-LAST-CHANGE             PIC S9(8) COMP SYNC.

       01  WF-CONDITION-FLAGS.
         03  WF-EOF-FLAG                PIC X.
           88  END-OF-FILE              VALUE "Y".
         03  WF-EMPTY-FILE-FLAG         PIC X.
           88  EMPTY-FILE               VALUE "Y".

       PROCEDURE DIVISION.
       A-MAIN SECTION.
       A-000.
           PERFORM B-INITIALISE.
           IF NOT EMPTY-FILE
              PERFORM C-SORT.
           PERFORM D-FINISH.

       A-999.
           STOP RUN.

       B-INITIALISE SECTION.
       B-000.
           DISPLAY "*** " WA-PROGNAME " VERSION "
                          WA-VERSION " STARTING ***".

           MOVE ALL "N" TO WF-CONDITION-FLAGS.
           OPEN INPUT FA-INPUT-FILE.
           SET WB-IX-1 TO 0.

           READ FA-INPUT-FILE AT END MOVE "Y" TO WF-EOF-FLAG
                                                 WF-EMPTY-FILE-FLAG.

           PERFORM BA-READ-INPUT UNTIL END-OF-FILE.

           CLOSE FA-INPUT-FILE.

           SET WC-SIZE TO WB-IX-1.

       B-999.
           EXIT.

       BA-READ-INPUT SECTION.
       BA-000.
           SET WB-IX-1 UP BY 1.
           MOVE FA-DATA TO WB-ENTRY(WB-IX-1).

           READ FA-INPUT-FILE AT END MOVE "Y" TO WF-EOF-FLAG.

       BA-999.
           EXIT.

       C-SORT SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           MOVE WC-SIZE TO WC-END.
           PERFORM E-BUBBLE UNTIL WC-END = 1.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       D-FINISH SECTION.
       D-000.
           OPEN OUTPUT FB-OUTPUT-FILE.
           SET WB-IX-1 TO 1.

           PERFORM DA-WRITE-OUTPUT UNTIL WB-IX-1 > WC-SIZE.

           CLOSE FB-OUTPUT-FILE.

           DISPLAY "*** " WA-PROGNAME " FINISHED ***".

       D-999.
           EXIT.

       DA-WRITE-OUTPUT SECTION.
       DA-000.
           WRITE FB-OUTPUT-REC FROM WB-ENTRY(WB-IX-1).
           SET WB-IX-1 UP BY 1.

       DA-999.
           EXIT.

       E-BUBBLE SECTION.
       E-000.
           MOVE 1 TO WC-LAST-CHANGE.

           PERFORM F-PASS VARYING WB-IX-1 FROM 1 BY 1
                          UNTIL WB-IX-1 = WC-END.

           MOVE WC-LAST-CHANGE TO WC-END.

       E-999.
           EXIT.

       F-PASS SECTION.
       F-000.
           IF WB-ENTRY(WB-IX-1) > WB-ENTRY(WB-IX-1 + 1)
              SET  WC-LAST-CHANGE        TO WB-IX-1
              MOVE WB-ENTRY(WB-IX-1)     TO WC-TEMP
              MOVE WB-ENTRY(WB-IX-1 + 1) TO WB-ENTRY(WB-IX-1)
              MOVE WC-TEMP               TO WB-ENTRY(WB-IX-1 + 1).

       F-999.
           EXIT.

```


A more modern version of COBOL.

```cobol

       identification division.
       program-id. BUBBLSRT.
       data division.
       working-storage section.
       01 changed-flag      pic x.
          88 hasChanged         value 'Y'.
          88 hasNOTChanged      value 'N'.
       01 itemCount         pic 99.
       01 tempItem          pic 99.
       01 itemArray.
          03 itemArrayCount pic 99.
          03 item           pic 99 occurs 99 times
                                   indexed by itemIndex.
      *
       procedure division.
       main.
      * place the values to sort into itemArray
           move 10 to itemArrayCount
           move 28 to item (1)
           move 44 to item (2)
           move 46 to item (3)
           move 24 to item (4)
           move 19 to item (5)
           move  2 to item (6)
           move 17 to item (7)
           move 11 to item (8)
           move 24 to item (9)
           move  4 to item (10)
      * store the starting count in itemCount and perform the sort
           move itemArrayCount to itemCount
           perform bubble-sort
      * output the results
           perform varying itemIndex from 1 by 1
              until itemIndex > itemArrayCount
              display item (itemIndex) ';' with no advancing
           end-perform
      * thats it!
           stop run.
      *
       bubble-sort.
           perform with test after until hasNOTchanged
              set hasNOTChanged to true
              subtract 1 from itemCount
              perform varying itemIndex from 1 by 1
                 until itemIndex > itemCount
                 if item (itemIndex) > item (itemIndex + 1)
                    move item (itemIndex) to tempItem
                    move item (itemIndex + 1) to item (itemIndex)
                    move tempItem to item (itemIndex + 1)
                    set hasChanged to true
                 end-if
              end-perform
           end-perform
           .

```


{{out}}

```txt
 Output: 02;04;11;17;19;24;24;28;44;46;
```



## Common Lisp

Bubble sort an sequence in-place, using the < operator for comparison if no comaprison function is provided

```lisp
(defun bubble-sort (sequence &optional (compare #'<))
  "sort a sequence (array or list) with an optional comparison function (cl:< is the default)"
  (loop with sorted = nil until sorted do
        (setf sorted t)
        (loop for a below (1- (length sequence)) do
              (unless (funcall compare (elt sequence a)
                                       (elt sequence (1+ a)))
                (rotatef (elt sequence a)
                         (elt sequence (1+ a)))
                (setf sorted nil)))))
```



```lisp
(bubble-sort (list 5 4 3 2 1))
```


<code>elt</code> has linear access time for lists, making the prior implementation of bubble-sort very expensive (although very clear, and straightforward to code.  Here is an implementation that works efficiently for both vectors and lists.  For lists it also has the nice property that the input list and the sorted list begin with the same <code>cons</code> cell.


```lisp
(defun bubble-sort-vector (vector predicate &aux (len (1- (length vector))))
  (do ((swapped t)) ((not swapped) vector)
    (setf swapped nil)
    (do ((i (min 0 len) (1+ i))) ((eql i len))
      (when (funcall predicate (aref vector (1+ i)) (aref vector i))
        (rotatef (aref vector i) (aref vector (1+ i)))
        (setf swapped t)))))

(defun bubble-sort-list (list predicate)
  (do ((swapped t)) ((not swapped) list)
    (setf swapped nil)
    (do ((list list (rest list))) ((endp (rest list)))
      (when (funcall predicate (second list) (first list))
        (rotatef (first list) (second list))
        (setf swapped t)))))

(defun bubble-sort (sequence predicate)
  (etypecase sequence
    (list (bubble-sort-list sequence predicate))
    (vector (bubble-sort-vector sequence predicate))))
```



## D


```d
import std.stdio, std.algorithm : swap;

T[] bubbleSort(T)(T[] data) pure nothrow
{
    foreach_reverse (n; 0 .. data.length)
    {
        bool swapped;
        foreach (i; 0 .. n)
            if (data[i] > data[i + 1]) {
                swap(data[i], data[i + 1]);
                swapped = true;
            }
        if (!swapped)
            break;
    }
    return data;
}


void main()
{
    auto array = [28, 44, 46, 24, 19, 2, 17, 11, 25, 4];
    writeln(array.bubbleSort());
}
```

{{out}}

```txt
[2, 4, 11, 17, 19, 24, 25, 28, 44, 46]
```



## Dart


```txt

List<num> bubbleSort(List<num> list) {
  var retList = new List<num>.from(list);
  var tmp;
  var swapped = false;
  do {
    swapped = false;
    for(var i = 1; i < retList.length; i++) {
      if(retList[i - 1] > retList[i]) {
        tmp = retList[i - 1];
        retList[i - 1] = retList[i];
        retList[i] = tmp;
        swapped = true;
      }
    }
  } while(swapped);

  return retList;
}

```



## Delphi

Dynamic array is a 0-based array of variable length

Static array is an arbitrary-based array of fixed length

```Delphi
program TestBubbleSort;

{$APPTYPE CONSOLE}

{.$DEFINE DYNARRAY}  // remove '.' to compile with dynamic array

type
  TItem = Integer;   // declare ordinal type for array item
{$IFDEF DYNARRAY}
  TArray = array of TItem;          // dynamic array
{$ELSE}
  TArray = array[0..15] of TItem;   // static array
{$ENDIF}

procedure BubbleSort(var A: TArray);
var
  Item: TItem;
  K, L, J: Integer;

begin
  L:= Low(A) + 1;
  repeat
    K:= High(A);
    for J:= High(A) downto L do begin
      if A[J - 1] > A[J] then begin
        Item:= A[J - 1];
        A[J - 1]:= A[J];
        A[J]:= Item;
        K:= J;
      end;
    end;
    L:= K + 1;
  until L > High(A);
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
  BubbleSort(A);
  for I:= Low(A) to High(A) do
    Write(A[I]:3);
  Writeln;
  Readln;
end.
```

{{out}}

```txt

  0  3 86 20 27 67 31 16 37 42  8 47  7 84  5 29
  0  3  5  7  8 16 20 27 29 31 37 42 47 67 84 86

```



## Dyalect



```dyalect
func bubbleSort(list) {
    var done = false
    while !done {
        done = true
        for i in 1..(list.len()-1) {
            if list[i - 1] > list[i] {
                var x = list[i]
                list[i] = list[i - 1]
                list[i - 1] = x
                done = false
            }
        }
    }
}

var xs = [3,1,5,4,2,6]
bubbleSort(xs)
print(xs)
```


{{out}}


```txt
[1, 2, 3, 4, 5, 6]
```



## E


```e
def bubbleSort(target) {
  __loop(fn {
    var changed := false
    for i in 0..(target.size() - 2) {
      def [a, b] := target(i, i + 2)
      if (a > b) {
        target(i, i + 2) := [b, a]
        changed := true
      }
    }
    changed
  })
}
```


(Uses the primitive __loop directly because it happens to map to the termination test for this algorithm well.)


## EchoLisp


```scheme

;; sorts a vector of objects in place
;; proc is an user defined comparison procedure

(define (bubble-sort V proc)
(define length (vector-length V))
    (for* ((i (in-range 0 (1- length))) (j (in-range (1+ i) length)))
    (unless (proc (vector-ref V i) (vector-ref V j)) (vector-swap! V i j)))
    V)


(define V #( albert antoinette elvis zen simon))
(define (sort/length a b) ;; sort by string length
    (< (string-length a) (string-length b)))

(bubble-sort V sort/length)
    → #(zen simon elvis albert antoinette)

```



## EDSAC order code

This demo of a bubble sort on the EDSAC shows how awkward it was to deal with arrays
in the absence of an index register (one was added in 1953).
To refer to an array element at a given index, the programmer had to manufacture an
EDSAC order referring to the correct address, then plant that order in the code.

To clarify the EDSAC program, an equivalent Pascal program is added as a comment.

```edsac

  [Bubble sort demo for Rosetta Code website]
  [EDSAC program. Initial Orders 2]

  [Sorts a list of double-word integers.
   List must be loaded at an even address.
   First item gives number of items to follow.
   Address of list is placed in location 49.
   List can then be referred to with code letter L.]
          T49K
          P300F [<---------- address of list here]

  [Subroutine R2, reads positive integers during input of orders.
   Items separated by F; list ends with #TZ.]
  GKT20FVDL8FA40DUDTFI40FA40FS39FG@S2FG23FA5@T5@E4@E13Z

  [Tell R2 where to store integers it reads from tape.]
        T    #L  ['T m D' in documentation, but this also works]

  [Lists of integers, comment out all except one]
  [10 integers from digits of pi]
  10F314159F265358F979323F846264F338327F950288F419716F939937F510582F097494#TZ

  [32 integers from digits of e ]
  [32F
   27182818F28459045F23536028F74713526F62497757F24709369F99595749F66967627F
   72407663F03535475F94571382F17852516F64274274F66391932F00305992F18174135F
   96629043F57290033F42952605F95630738F13232862F79434907F63233829F88075319F
   52510190F11573834F18793070F21540891F49934884F16750924F47614606F68082264#TZ]

  [Library subroutine P7, prints positive integer at 0D.
   35 locations; load at aneven address.]
        T  56 K
  GKA3FT26@H28#@NDYFLDT4DS27@TFH8@S8@T1FV4DAFG31@SFLDUFOFFFSFL4F
  T4DA1FA27@G11@XFT28#ZPFT27ZP1024FP610D@524D!FO30@SFL8FE22@

  [The EDSAC code below implements the following Pascal program,
   where the integers to be sorted are in a 1-based array x.
   Since the assembler used (EdsacPC by Martin Campbell-Kelly)
   doesn't allow square brackets inside comments, they are
   replaced here by curly brackets.]
  [
    swapped := true;
    j := n; // number of items
    while (swapped and (j >= 2)) do begin
      swapped := false;
      for i := 1 to j - 1 do begin
        // Using temp in the comparison makes the EDSAC code a bit simpler
        temp := x{i};
        if (x{i + 1} < temp) then begin
          x{i} := x{i + 1};
          x{i + 1} := temp;
          swapped := true;
        end;
      end;
      dec(j);
    end;
  ]
    [Main routine]
        T 100 K
        G     K
    [0] P F P F [double-word temporary store]
    [2] P     F [flag for swapped, >= 0 if true, < 0 if false]
    [3] P     F ['A' order for x{j}; implicitly defines j]
    [4] P   2 F [to change list index by 1, i.e.change address by 2]
    [5] A    #L ['A' order for number of items]
    [6] A   2#L ['A' order for x{1}]
    [7] A   4#L ['A' order for x{2}]
    [8] I2046 F [add to convert 'A' order to 'T' and dec address by 2]
    [9] K4096 F [(1) minimum 17-bit value (2) teleprinter null]
   [10] P     D [constant 1, used in printing]
   [11] #     F [figure shift]
   [12] &     F [line feed]
   [13] @     F [carriage return]

       [Enter here with acc = 0]
   [14] T   2 @ [swapped := true]
        A     L [get count, n in Pascal program above]
        L   1 F [times 4 by shifting]
        A   5 @ [make 'A' order for x{n}; initializes j := n]

       [Start 'while' loop of Pascal program.
        Here acc = 'A' order for x{j}]
   [18] U   3 @ [update j]
        S   7 @ [subtract 'A' order for x{2}]
        G  56 @ [if j < 2 then done]
        T     F [acc := 0]
        A   2 @ [test for swapped, acc >= 0 if so]
        G  56 @ [if not swapped then done]
        A   9 @ [change acc from >= 0 to < 0]
        T   2 @ [swapped := false until swap occurs]
        A   6 @ ['A' order for x{1}; initializes i := 1]

       [Start 'for' loop of Pascal program.
        Here acc = 'A' order for x{i}]
   [27] U  36 @ [store order]
        S   3 @ [subtract 'A' order for x{j}]
        E  52 @ [out of 'for' loop if i >= j]
        T     F [clear acc]
        A  36 @ [load 'A' order for x{i}]
        A   4 @ [inc address by 2]
        U  38 @ [plant 'A' order for x{i + 1}]
        A   8 @ ['A' to 'T', and dec address by 2]
        T  42 @ [plant 'T' order for x{i}]
   [36] A    #L [load x{i}; this order implicitly defines i]
        T    #@ [temp := x{i}]
   [38] A    #L [load x{i + 1}]
        S    #@ [acc := x{i + 1} - temp]
        E  49 @ [don't swap if x{i + 1} >= temp]

       [Here to swap x{i} and x{i + 1}]
        A    #@ [restore acc := x{i + 1} after test]
   [42] T    #L [x{i} := x{i + 1}]
        A  42 @ [load 'T' order for x{i}]
        A   4 @ [inc address by 2]
        T  47 @ [plant 'T' order for x{i + 1}]
        A    #@ [load temp]
   [47] T    #L [to x{i + 1}]
        T   2 @ [swapped := 0 (true)]

   [49] T     F [clear acc]
        A  38 @ [load 'A' order for x{i + 1}]
        G  27 @ [loop (unconditional) to inc i]

   [52] T     F
        A   3 @ [load 'A' order for x{j}]
        S   4 @ [dec address by 2]
        G  18 @ [loop (unconditional) to dec j]

       [Print the sorted list of integers]
   [56] O  11 @ [figure shift]
        T     F [clear acc]
        A   5 @ [load 'A' order for head of list]
        T  65 @ [plant in code below]
        S     L [load negative number of items]
   [61] T     @ [use first word of temp store for count]
        A  65 @ [load 'A' order for item]
        A   4 @ [inc address by 2]
        T  65 @ [store back]
   [65] A    #L [load next item in list]
        T     D [to 0D for printing]
   [67] A  67 @ [for subroutine return]
        G  56 F [print integer, clears acc]
        O  13 @ [print CR]
        O  12 @ [print LF]
        A     @ [negative count]
        A  10 @ [add 1]
        G  61 @ [loop back till count = 0]
   [74] O   9 @ [null to flush teleprinter buffer]
        Z     F [stop]
        E  14 Z [define entry point]
        P     F [acc = 0 on entry]

```

{{out}}

```txt

     97494
    265358
    314159
    338327
    419716
    510582
    846264
    939937
    950288
    979323

```



## Eiffel

{{works with|EiffelStudio|6.6 (with provisional loop syntax)}}

This solution is presented in two classes. The first is a simple application that creates a set, an instance of <code lang="eiffel">MY_SORTED_SET</code>, and adds elements to the set in unsorted order. It iterates across the set printing the elements, then it sorts the set, and reprints the elements.


```eiffel
class
    APPLICATION
create
    make

feature
    make
            -- Create and print sorted set
        do
            create my_set.make
            my_set.put_front (2)
            my_set.put_front (6)
            my_set.put_front (1)
            my_set.put_front (5)
            my_set.put_front (3)
            my_set.put_front (9)
            my_set.put_front (8)
            my_set.put_front (4)
            my_set.put_front (10)
            my_set.put_front (7)
            print ("Before: ")
            across my_set as ic loop print (ic.item.out + " ")  end
            print ("%NAfter : ")
            my_set.sort
            across my_set as ic loop print (ic.item.out + " ")  end
        end

    my_set: MY_SORTED_SET [INTEGER]
            -- Set to be sorted
end
```


The second class is <code lang="eiffel">MY_SORTED_SET</code>.


```eiffel
class
    MY_SORTED_SET [G -> COMPARABLE]
inherit
    TWO_WAY_SORTED_SET [G]
        redefine
            sort
        end
create
    make

feature
    sort
            -- Sort with bubble sort
        local
            l_unchanged: BOOLEAN
            l_item_count: INTEGER
            l_temp: G
        do
            from
                l_item_count := count
            until
                l_unchanged
            loop
                l_unchanged := True
                l_item_count := l_item_count - 1
                across 1 |..| l_item_count as ic loop
                    if Current [ic.item] > Current [ic.item + 1] then
                        l_temp := Current [ic.item]
                        Current [ic.item] := Current [ic.item + 1]
                        Current [ic.item + 1] := l_temp
                        l_unchanged := False
                    end
                end
            end
        end
end
```


This class inherits from the Eiffel library class <code lang="eiffel">TWO_WAY_SORTED_SET</code>, which implements sets whose elements are comparable. Therefore, the set can be ordered and in fact is kept so under normal circumstances.

<code lang="eiffel">MY_SORTED_SET</code> redefines only the routine <code lang="eiffel">sort</code> which contains the implementation of the sort algorithm. The implementation in the redefined version of <code lang="eiffel">sort</code> in <code lang="eiffel">MY_SORTED_SET</code> uses a bubble sort.

{{out}}

```txt

Before: 7 10 4 8 9 3 5 1 6 2
After : 1 2 3 4 5 6 7 8 9 10

```


<code lang="eiffel">TWO_WAY_SORTED_SET</code> is implemented internally as a list.
For this example, we use the feature <code lang="eiffel">put_front</code> which explicitly adds each new element to the beginning of the list, allowing us to show that the elements are unordered until we sort them.
It also causes, in the "Before" output, the elements to be printed in the reverse of the order in which they were added.
Under normal circumstances, we would use the feature <code lang="eiffel">extend</code> (rather than <code lang="eiffel">put_front</code>) to add elements to the list.
This would assure that the order was maintained even as elements were added.


## Elena

{{trans|C#}}
ELENA 4.1 :

```elena
import system'routines;
import extensions;

extension op
{
    bubbleSort()
    {
        var list := self.clone();

        bool madeChanges := true;
        int itemCount := list.Length;
        while (madeChanges)
        {
            madeChanges := false;
            itemCount -= 1;
            for(int i := 0, i < itemCount, i += 1)
            {
                if (list[i] > list[i + 1])
                {
                    list.exchange(i,i+1);
                    madeChanges := true
                }
            }
        };

        ^ list
    }
}

public program()
{
    var list := new int[]::(3, 7, 3, 2, 1, -4, 10, 12, 4);
    console.printLine(list.bubbleSort().asEnumerable())
}
```

{{out}}

```txt

-4,1,2,3,3,4,7,10,12

```



## Elixir


```elixir
defmodule Sort do
  def bsort(list) when is_list(list) do
    t = bsort_iter(list)

    if t == list, do: t, else: bsort(t)
  end

  def bsort_iter([x, y | t]) when x > y, do: [y | bsort_iter([x | t])]
  def bsort_iter([x, y | t]), do: [x | bsort_iter([y | t])]
  def bsort_iter(list), do: list
end
```



## Erlang

sort/3 copied from Stackoverflow.

```Erlang

-module( bubble_sort ).

-export( [list/1, task/0] ).

list( To_be_sorted ) -> sort( To_be_sorted, [], true ).

task() ->
	List = "asdqwe123",
	Sorted = list( List ),
	io:fwrite( "List ~p is sorted ~p~n", [List, Sorted] ).


sort( [], Acc, true ) -> lists:reverse( Acc );
sort( [], Acc, false ) -> sort( lists:reverse(Acc), [], true );
sort( [X, Y | T], Acc, _Done ) when X > Y -> sort( [X | T], [Y | Acc], false );
sort( [X | T], Acc, Done ) -> sort( T, [X | Acc], Done ).

```

{{out}}

```txt

7> bubble_sort:task().
List "asdqwe123" is sorted "123adeqsw"

```



## ERRE


```ERRE

PROGRAM BUBBLE_SORT

DIM FLIPS%,N,J

DIM A%[100]

BEGIN

! init random number generator
   RANDOMIZE(TIMER)
! fills array A% with random data
   FOR N=1 TO UBOUND(A%,1) DO
     A%[N]=RND(1)*256
   END FOR
! sort array
   FLIPS%=TRUE
   WHILE FLIPS% DO
     FLIPS%=FALSE
     FOR N=1 TO UBOUND(A%,1)-1 DO
        IF A%[N]>A%[N+1] THEN
            SWAP(A%[N],A%[N+1])
            FLIPS%=TRUE
        END IF
     END FOR
   END WHILE
! print sorted array
   FOR N=1 TO UBOUND(A%,1) DO
     PRINT(A%[N];)
   END FOR
   PRINT
END PROGRAM

```



## Euphoria


```euphoria
function bubble_sort(sequence s)
    object tmp
    integer changed
    for j = length(s) to 1 by -1 do
        changed = 0
        for i = 1 to j-1 do
            if compare(s[i], s[i+1]) > 0 then
                tmp = s[i]
                s[i] = s[i+1]
                s[i+1] = tmp
                changed = 1
            end if
        end for
        if not changed then
            exit
        end if
    end for
    return s
end function

include misc.e
constant s = {4, 15, "delta", 2, -31, 0, "alfa", 19, "gamma", 2, 13, "beta", 782, 1}

puts(1,"Before: ")
pretty_print(1,s,{2})
puts(1,"\nAfter: ")
pretty_print(1,bubble_sort(s),{2})
```


{{out}}

```txt
Before: {
  4,
  15,
  "delta",
  2,
  -31,
  0,
  "alfa",
  19,
  "gamma",
  2,
  13,
  "beta",
  782,
  1
}
After: {
  -31,
  0,
  1,
  2,
  2,
  4,
  13,
  15,
  19,
  782,
  "alfa",
  "beta",
  "delta",
  "gamma"
}
```



## Ezhil


```Ezhil


## இந்த நிரல் ஒரு பட்டியலில் உள்ள எண்களை Bubble Sort என்ற முறைப்படி ஏறுவரிசையிலும் பின்னர் அதையே இறங்குவரிசையிலும் அடுக்கித் தரும்

## மாதிரிக்கு நாம் ஏழு எண்களை எடுத்துக்கொள்வோம்

எண்கள் = [5, 1, 10, 8, 1, 21, 4, 2]
எண்கள்பிரதி = எண்கள்

பதிப்பி "ஆரம்பப் பட்டியல்:"
பதிப்பி எண்கள்

நீளம் = len(எண்கள்)
குறைநீளம் = நீளம் - 1

@(குறைநீளம் != -1) வரை
  மாற்றம் = -1
  @(எண் = 0, எண் < குறைநீளம், எண் = எண் + 1) ஆக
    முதலெண் = எடு(எண்கள், எண்)
    இரண்டாமெண் = எடு(எண்கள், எண் + 1)
    @(முதலெண் > இரண்டாமெண்) ஆனால்

      ## பெரிய எண்களை ஒவ்வொன்றாகப் பின்னே நகர்த்துகிறோம்

      வெளியேஎடு(எண்கள், எண்)
      நுழைக்க(எண்கள், எண், இரண்டாமெண்)
      வெளியேஎடு(எண்கள், எண் + 1)
      நுழைக்க(எண்கள், எண் + 1, முதலெண்)
      மாற்றம் = எண்
    முடி
  முடி
  குறைநீளம் = மாற்றம்
முடி

பதிப்பி "ஏறு வரிசையில் அமைக்கப்பட்ட பட்டியல்:"
பதிப்பி எண்கள்

## இதனை இறங்குவரிசைக்கு மாற்றுவதற்கு எளிய வழி

தலைகீழ்(எண்கள்)

## இப்போது, நாம் ஏற்கெனவே எடுத்துவைத்த எண்களின் பிரதியை Bubble Sort முறைப்படி இறங்குவரிசைக்கு மாற்றுவோம்

நீளம் = len(எண்கள்பிரதி)
குறைநீளம் = நீளம் - 1

@(குறைநீளம் != -1) வரை
  மாற்றம் = -1
  @(எண் = 0, எண் < குறைநீளம், எண் = எண் + 1) ஆக
    முதலெண் = எடு(எண்கள்பிரதி, எண்)
    இரண்டாமெண் = எடு(எண்கள்பிரதி, எண் + 1)
    @(முதலெண் < இரண்டாமெண்) ஆனால்

      ## சிறிய எண்களை ஒவ்வொன்றாகப் பின்னே நகர்த்துகிறோம்

      வெளியேஎடு(எண்கள்பிரதி, எண்)
      நுழைக்க(எண்கள்பிரதி, எண், இரண்டாமெண்)
      வெளியேஎடு(எண்கள்பிரதி, எண் + 1)
      நுழைக்க(எண்கள்பிரதி, எண் + 1, முதலெண்)
      மாற்றம் = எண்
    முடி
  முடி
  குறைநீளம் = மாற்றம்
முடி

பதிப்பி "இறங்கு வரிசையில் அமைக்கப்பட்ட பட்டியல்:"
பதிப்பி எண்கள்பிரதி


```


=={{header|F Sharp|F#}}==

```fsharp
let BubbleSort (lst : list<int>) =
    let rec sort accum rev lst =
        match lst, rev with
        | [], true -> accum |> List.rev
        | [], false -> accum |> List.rev |> sort [] true
        | x::y::tail, _ when x > y -> sort (y::accum) false (x::tail)
        | head::tail, _ -> sort (head::accum) rev tail
    sort [] true lst

```



## Factor


```factor
USING: fry kernel locals math math.order sequences
sequences.private ;
IN: rosetta.bubble

<PRIVATE

:: ?exchange ( i seq quot -- ? )
    i i 1 + [ seq nth-unsafe ] bi@ quot call +gt+ = :> doit?
    doit? [ i i 1 + seq exchange ] when
    doit? ; inline

: 1pass ( seq quot -- ? )
    [ [ length 1 - iota ] keep ] dip
    '[ _ _ ?exchange ] [ or ] map-reduce ; inline

PRIVATE>

: sort! ( seq quot -- )
    over empty?
    [ 2drop ] [ '[ _ _ 1pass ] loop ] if ; inline

: natural-sort! ( seq -- )
    [ <=> ] sort! ;
```


It is possible to pass your own comparison operator to <code>sort!</code>, so you can f.e. sort your sequence backwards with passing <code>[ >=< ]</code> into it.


```factor
10 [ 10000 random ] replicate
[ "Before:  " write . ]
[ "Natural: " write [ natural-sort! ] keep . ]
[ "Reverse: " write [ [ >=< ] sort! ] keep . ] tri
```


 Before:  { 3707 5045 4661 1489 3140 7195 8844 6506 6322 3199 }
 Natural: { 1489 3140 3199 3707 4661 5045 6322 6506 7195 8844 }
 Reverse: { 8844 7195 6506 6322 5045 4661 3707 3199 3140 1489 }


## Fish

This is not a complete implementation of bubblesort: it doesn't keep a boolean flag whether to stop, so it goes on printing each stage of the sorting process ad infinitum.


```fish
v Sorts the (pre-loaded) stack
  with bubblesort.
v                     <
\l0=?;l&
>&:1=?v1-&2[$:{:{](?${
          >~{ao       ^
      >~}l &{   v
o","{n:&-1^?=0:&<
```



## Forth

Sorts the 'cnt' cells stored at 'addr' using the test stored in the deferred word 'bubble-test'. Uses forth local variables for clarity.


```forth
defer bubble-test
' > is bubble-test

: bubble { addr cnt -- }
  cnt 1 do
    addr cnt i - cells bounds do
      i 2@ bubble-test if i 2@ swap i 2! then
    cell +loop
  loop ;
```


This is the same algorithm done without the local variables:


```forth
: bubble ( addr cnt -- )
  dup 1 do
    2dup i - cells bounds do
      i 2@ bubble-test if i 2@ swap i 2! then
    cell +loop
  loop ;
```


Version with ''O(n)'' best case:

```forth
: bubble ( addr len -- )
  begin
    1- 2dup  true -rot  ( sorted addr len-1 )
    cells bounds ?do
      i 2@ bubble-test if
        i 2@ swap i 2!
        drop false   ( mark unsorted )
      then
    cell +loop  ( sorted )
  until 2drop ;
```


Test any version with this:
 create test
 8 , 1 , 4 , 2 , 10 , 3 , 7 , 9 , 6 , 5 ,
 here test - cell / constant tcnt

 test tcnt cells dump
 ' > is bubble-test
 test tcnt bubble
 test tcnt cells dump
 ' < is bubble-test
 test tcnt bubble
 test tcnt cells dump


## Fortran


```fortran
SUBROUTINE Bubble_Sort(a)
  REAL, INTENT(in out), DIMENSION(:) :: a
  REAL :: temp
  INTEGER :: i, j
  LOGICAL :: swapped

  DO j = SIZE(a)-1, 1, -1
    swapped = .FALSE.
    DO i = 1, j
      IF (a(i) > a(i+1)) THEN
        temp = a(i)
        a(i) = a(i+1)
        a(i+1) = temp
        swapped = .TRUE.
      END IF
    END DO
    IF (.NOT. swapped) EXIT
  END DO
END SUBROUTINE Bubble_Sort
```


## FreeBASIC

Per task pseudo code:

```FreeBASIC
' version 21-10-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

Sub bubblesort(bs() As Long)
    ' sort from lower bound to the highter bound
    ' array's can have subscript range from -2147483648 to +2147483647
    Dim As Long lb = LBound(bs)
    Dim As Long ub = UBound(bs)
    Dim As Long done, i

    Do
        done = 0
        For i = lb To ub -1
            ' replace "<" with ">" for downwards sort
            If bs(i) > bs(i +1) Then
                Swap bs(i), bs(i +1)
                done = 1
            End If
        Next
    Loop Until done = 0

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
bubblesort(array())  ' sort the array
Print "  sort ";
For i = a To b : Print Using "####"; array(i); : Next : Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
unsort   -7   3  -4  -6   4  -1  -2   2   7   0   5   1  -3  -5   6
  sort   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=ba84832d633cb92bbe6c2f54704819c3 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim byToSort As Byte[] = [249, 28, 111, 36, 171, 98, 29, 448, 44, 147, 154, 46, 102, 183, 24,
                          120, 19, 123, 2, 17, 226, 11, 211, 25, 191, 205, 77]
Dim byCount As Byte
Dim bSorting As Boolean

Print "To sort: -"
ShowWorking(byToSort)
Print
Repeat
  bSorting = False
  For byCount = 0 To byToSort.Max - 1
    If byToSort[byCount] > byToSort[byCount + 1] Then
      Swap byToSort[byCount], byToSort[byCount + 1]
      bSorting = True
    Endif
  Next
  If bSorting Then ShowWorking(byToSort)
Until bSorting = False
End
'-----------------------------------------
Public Sub ShowWorking(byToSort As Byte[])
Dim byCount As Byte

For byCount = 0 To byToSort.Max
  Print Str(byToSort[byCount]);
  If byCount <> byToSort.Max Then Print ",";
Next

Print

End
```

Output:

```txt

To sort: -
249,28,111,36,171,98,29,192,44,147,154,46,102,183,24,120,19,123,2,17,226,11,211,25,191,205,77

28,111,36,171,98,29,192,44,147,154,46,102,183,24,120,19,123,2,17,226,11,211,25,191,205,77,249
28,36,111,98,29,171,44,147,154,46,102,183,24,120,19,123,2,17,192,11,211,25,191,205,77,226,249
28,36,98,29,111,44,147,154,46,102,171,24,120,19,123,2,17,183,11,192,25,191,205,77,211,226,249
28,36,29,98,44,111,147,46,102,154,24,120,19,123,2,17,171,11,183,25,191,192,77,205,211,226,249
28,29,36,44,98,111,46,102,147,24,120,19,123,2,17,154,11,171,25,183,191,77,192,205,211,226,249
28,29,36,44,98,46,102,111,24,120,19,123,2,17,147,11,154,25,171,183,77,191,192,205,211,226,249
28,29,36,44,46,98,102,24,111,19,120,2,17,123,11,147,25,154,171,77,183,191,192,205,211,226,249
28,29,36,44,46,98,24,102,19,111,2,17,120,11,123,25,147,154,77,171,183,191,192,205,211,226,249
28,29,36,44,46,24,98,19,102,2,17,111,11,120,25,123,147,77,154,171,183,191,192,205,211,226,249
28,29,36,44,24,46,19,98,2,17,102,11,111,25,120,123,77,147,154,171,183,191,192,205,211,226,249
28,29,36,24,44,19,46,2,17,98,11,102,25,111,120,77,123,147,154,171,183,191,192,205,211,226,249
28,29,24,36,19,44,2,17,46,11,98,25,102,111,77,120,123,147,154,171,183,191,192,205,211,226,249
28,24,29,19,36,2,17,44,11,46,25,98,102,77,111,120,123,147,154,171,183,191,192,205,211,226,249
24,28,19,29,2,17,36,11,44,25,46,98,77,102,111,120,123,147,154,171,183,191,192,205,211,226,249
24,19,28,2,17,29,11,36,25,44,46,77,98,102,111,120,123,147,154,171,183,191,192,205,211,226,249
19,24,2,17,28,11,29,25,36,44,46,77,98,102,111,120,123,147,154,171,183,191,192,205,211,226,249
19,2,17,24,11,28,25,29,36,44,46,77,98,102,111,120,123,147,154,171,183,191,192,205,211,226,249
2,17,19,11,24,25,28,29,36,44,46,77,98,102,111,120,123,147,154,171,183,191,192,205,211,226,249
2,17,11,19,24,25,28,29,36,44,46,77,98,102,111,120,123,147,154,171,183,191,192,205,211,226,249
2,11,17,19,24,25,28,29,36,44,46,77,98,102,111,120,123,147,154,171,183,191,192,205,211,226,249

```


=={{header|g-fu}}==
<lang g-fu>
(fun bubbles (vs)
  (let done? F n (len vs))

  (while (not done?)
    (set done? T n (- n 1))

    (for (n i)
      (let x (# vs i) j (+ i 1) y (# vs j))
      (if (> x y) (set done? F (# vs i) y (# vs j) x))))

  vs)

(bubbles '(2 1 3))
---
(1 2 3)

```




## Go

Per task pseudocode:

```go
package main

import "fmt"

func main() {
    list := []int{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    fmt.Println("unsorted:", list)

    bubblesort(list)
    fmt.Println("sorted!  ", list)
}

func bubblesort(a []int) {
    for itemCount := len(a) - 1; ; itemCount-- {
        hasChanged := false
        for index := 0; index < itemCount; index++ {
            if a[index] > a[index+1] {
                a[index], a[index+1] = a[index+1], a[index]
                hasChanged = true
            }
        }
        if hasChanged == false {
            break
        }
    }
}
```


More generic version that can sort anything that implements <code>sort.Interface</code>:

```go
package main

import (
  "sort"
  "fmt"
)

func main() {
    list := []int{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    fmt.Println("unsorted:", list)

    bubblesort(sort.IntSlice(list))
    fmt.Println("sorted!  ", list)
}

func bubblesort(a sort.Interface) {
    for itemCount := a.Len() - 1; ; itemCount-- {
        hasChanged := false
        for index := 0; index < itemCount; index++ {
            if a.Less(index+1, index) {
                a.Swap(index, index+1)
                hasChanged = true
            }
        }
        if !hasChanged {
            break
        }
    }
}
```



## Groovy

Solution:

```groovy
def makeSwap = { a, i, j = i+1 -> print "."; a[[i,j]] = a[[j,i]] }

def checkSwap = { a, i, j = i+1 -> [(a[i] > a[j])].find { it }.each { makeSwap(a, i, j) } }

def bubbleSort = { list ->
    boolean swapped = true
    while (swapped) { swapped = (1..<list.size()).any { checkSwap(list, it-1) } }
    list
}
```


Test Program:

```groovy
println bubbleSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4])
println bubbleSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1])
```


{{out}}

```txt
..............................................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
.........................................................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
```



## Haskell

This version checks for changes in a separate step for simplicity, because Haskell has no variables to track them with.

```haskell>bsort :: Ord a =
 [a] -> [a]
bsort s = case _bsort s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where _bsort (x:x2:xs) | x > x2    = x2:(_bsort (x:xs))
                         | otherwise = x:(_bsort (x2:xs))
        _bsort s = s
```


This version uses the polymorphic <tt>Maybe</tt> type to designate unchanged lists. (The type signature of <tt>_bsort</tt> is now <tt>Ord a => [a] -> Maybe [a]</tt>.) It is slightly faster than the previous one.


```haskell
import Data.Maybe (fromMaybe)
import Control.Monad

bsort :: Ord a => [a] -> [a]
bsort s = maybe s bsort $ _bsort s
  where _bsort (x:x2:xs) = if x > x2
            then Just $ x2 : fromMaybe (x:xs) (_bsort $ x:xs)
            else liftM (x:) $ _bsort (x2:xs)
        _bsort _         = Nothing
```


This version is based on the above, but avoids sorting the whole list each time. To implement this without a counter and retain using pattern matching, inner sorting is reversed, and then the result is reversed back. Sorting is based on a predicate, e.g., (<) or (>).


```haskell
import Data.Maybe (fromMaybe)
import Control.Monad

bubbleSortBy ::  (a -> a -> Bool) -> [a] -> [a]
bubbleSortBy f as = case innerSort $ reverse as of
                         Nothing -> as
                         Just v  -> let (x:xs) = reverse v
                                   in x : bubbleSortBy f xs
    where innerSort (a:b:cs) = if b `f` a
                                  then liftM (a:) $ innerSort (b:cs)
                                  else Just $ b : fromMaybe (a:cs)
                                                (innerSort $ a:cs)
          innerSort _        = Nothing

bsort :: Ord a => [a] -> [a]
bsort =  bubbleSortBy (<)
```



## HicEst


```fortran
SUBROUTINE Bubble_Sort(a)
  REAL :: a(1)

  DO j = LEN(a)-1, 1, -1
    swapped = 0
    DO i = 1, j
      IF (a(i) > a(i+1)) THEN
        temp = a(i)
        a(i) = a(i+1)
        a(i+1) = temp
        swapped = 1
      ENDIF
    ENDDO
    IF (swapped == 0) RETURN
  ENDDO
END
```


=={{header|Icon}} and {{header|Unicon}}==
Icon/Unicon implementation of a bubble sort

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   demosort(bubblesort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure bubblesort(X,op)           #: return sorted list
local i,swapped

   op := sortop(op,X)                # select how and what we sort

   swapped := 1
   while \swapped := &null do         # the sort
      every  i := 2 to *X do
         if op(X[i],X[i-1]) then
            X[i-1] :=: X[swapped := i]
   return X
end
```


{{out}}

```txt
Sorting Demo using procedure bubblesort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
    with op = "numeric":     [ 1 2 3 3 5 6 9 14 ]   (0 ms)
    with op = "string":      [ 1 14 2 3 3 5 6 9 ]   (0 ms)
    with op = ">>":          [ 9 6 5 3 3 2 14 1 ]   (0 ms)
    with op = ">":           [ 14 9 6 5 3 3 2 1 ]   (0 ms)
    with op = procedure cmp: [ 1 2 3 3 5 6 9 14 ]   (1 ms)
    with op = "cmp":         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```


The following code supports this and other sorting demonstrations.
*  Sorting illustrates a difference in the way Icon and Unicon handles data types.  Built-in operators for comparing data types make a syntactic distinction between numeric and string types, and sorting structured and user-defined types require custom code.  An added complication arises because mixed types are allowed. Two approaches are possible here: (1) that taken by the built-in sort which sorts first by type and then value  The sort order of types is: &null, integer, real, string, cset, procedure, list, set, table, and record; and (2) Coercion of types which is used here (and implemented in 'sortop') to decide on using string or numeric sorting.  These sorts will not handle more exotic type mixes.
*  The 'sortop' procedure allows various methods of comparison be selected including customized ones.  The example could be made more general to deal with coercion of types like cset to string (admittedly an uninteresting example as csets are already sorted). Custom comparators are shown by and example procedure 'cmp'.
*  'demosort' can apply different sorting procedures and operators to lists and strings to show how this works. The routines 'displaysort' and 'writex' are helpers.


```icon
invocable all                # for op

procedure sortop(op,X)                  #: select how to sort

    op := case op of {
             "string":  "<<"
             "numeric": "<"
             &null:     if type(!X) == "string" then "<<" else "<"
             default:   op
          }
return proc(op, 2) | runerr(123, image(op))
end

procedure cmp(a,b)                    #: example custom comparison procedure
    return a < b                      #  Imagine a complex comparison test here!
end

procedure demosort(sortproc,L,s)      # demonstrate sort on L and s

    write("Sorting Demo using ",image(sortproc))
    writes("  on list : ")
    writex(L)
    displaysort(sortproc,L)           # default string sort
    displaysort(sortproc,L,"numeric") # explicit numeric sort
    displaysort(sortproc,L,"string")  # explicit string sort
    displaysort(sortproc,L,">>")      # descending string sort
    displaysort(sortproc,L,">")       # descending numeric sort
    displaysort(sortproc,L,cmp)       # ascending custom comparison
    displaysort(sortproc,L,"cmp")     # ascending custom comparison

    writes("  on string : ")
    writex(s)
    displaysort(sortproc,s)           # sort characters in a string
    write()
    return
end

procedure displaysort(sortproc,X,op)  #: helper to show sort behavior
local t,SX
    writes("    with op = ",left(image(op)||":",15))
    X := copy(X)
    t := &time
    SX := sortproc(X,op)
    writex(SX,"   (",&time - t," ms)")
    return
end

procedure writex(X,suf[])             #: helper for displaysort
    if type(X) == "string" then
        writes(image(X))
    else {
        writes("[")
        every writes(" ",image(!X))
        writes(" ]")
        }
    every writes(!suf)
    write()
return
end
```



## J

{{eff note|J|/:~ list}}

```j
bubbleSort=:  (([ (<. , >.) {.@]) , }.@])/^:_
```


Test program:


```j
   ?. 10 $ 10
4 6 8 6 5 8 6 6 6 9
   bubbleSort ?. 10 $ 10
4 5 6 6 6 6 6 8 8 9
```


For the most part, bubble sort works against J's strengths.  However, once a single pass has been implemented as a list operation, <code>^:_</code> tells J to repeat this until the result stops changing.


## Java

Bubble sorting (ascending) an array of any <tt>Comparable</tt> type:

```java>public static <E extends Comparable<? super E>
 void bubbleSort(E[] comparable) {
    boolean changed = false;
    do {
        changed = false;
        for (int a = 0; a < comparable.length - 1; a++) {
            if (comparable[a].compareTo(comparable[a + 1]) > 0) {
                E tmp = comparable[a];
                comparable[a] = comparable[a + 1];
                comparable[a + 1] = tmp;
                changed = true;
            }
        }
    } while (changed);
}
```


For descending, simply switch the direction of comparison:

```java
if (comparable[a].compareTo(comparable[b]) < 0){
   //same swap code as before
}
```



## JavaScript


```javascript
Array.prototype.bubblesort = function() {
    var done = false;
    while (!done) {
        done = true;
        for (var i = 1; i<this.length; i++) {
            if (this[i-1] > this[i]) {
                done = false;
                [this[i-1], this[i]] = [this[i], this[i-1]]
            }
        }
    }
    return this;
}
```


{{works with|SEE|3.0}}
{{works with|OSSP js|1.6.20070208}}

```javascript
Array.prototype.bubblesort = function() {
  var done = false;
  while (! done) {
    done = true;
    for (var i = 1; i < this.length; i++) {
      if (this[i - 1] > this[i]) {
        done = false;
        var tmp = this[i - 1];
        this[i - 1] = this[i];
        this[i] = tmp;
      }
    }
  }
  return this;
}
```


Example:

```javascript
var my_arr = ["G", "F", "C", "A", "B", "E", "D"];
my_arr.bubblesort();
print(my_arr);
```


{{out}}

```txt

 A,B,C,D,E,F,G

```



## jq


```jq
def bubble_sort:
  def swap(i;j): .[i] as $x | .[i]=.[j] | .[j]=$x;

  # input/output: [changed, list]
  reduce range(0; length) as $i
    ( [false, .];
      if $i > 0 and (.[0]|not) then .
      else reduce range(0; (.[1]|length) - $i - 1) as $j
        (.[0] = false;
        .[1] as $list
        | if $list[$j] > $list[$j + 1] then [true, ($list|swap($j; $j+1))]
          else .
          end )
      end  ) | .[1] ;
```

'''Example''':

```jq
(
 [3,2,1],
 [1,2,3],
 ["G", "F", "C", "A", "B", "E", "D"]
)  | bubble_sort
```

{{Out}}

```sh
$ jq -c -n -f Bubble_sort.jq
[1,2,3]
[1,2,3]
["A","B","C","D","E","F","G"]
```



## Julia

{{works with|Julia|0.6}}


```julia
function bubblesort!(arr::AbstractVector)
    for _ in 2:length(arr), j in 1:length(arr)-1
        if arr[j] > arr[j+1]
            arr[j], arr[j+1] = arr[j+1], arr[j]
        end
    end
    return arr
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", bubblesort!(v))
```


{{out}}

```txt
# unordered: [7, 4, -1, -8, 8, -1, 5, 6, -3, -5]
 -> ordered: [-8, -5, -3, -1, -1, 4, 5, 6, 7, 8]
```



## Kotlin

{{trans|Java}}


```scala
import java.util.Comparator

fun <T> bubbleSort(a: Array<T>, c: Comparator<T>) {
    var changed: Boolean
    do {
        changed = false
        for (i in 0..a.size - 2) {
            if (c.compare(a[i], a[i + 1]) > 0) {
                val tmp = a[i]
                a[i] = a[i + 1]
                a[i + 1] = tmp
                changed = true
            }
        }
    } while (changed)
}
```



## Io


```Io

List do(
  bubblesort := method(
    t := true
    while( t,
      t := false
      for( j, 0, self size - 2,
        if( self at( j ) start > self at( j+1 ) start,
          self swapIndices( j,j+1 )
          t := true
        )
      )
    )
    return( self )
  )
)

```



## Liberty BASIC


```lb

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
    counter = itemCount
    do
        hasChanged = 0
        for i = 1 to counter - 1
            if item(i) > item(i + 1) then
                temp = item(i)
                item(i) = item(i + 1)
                item(i + 1) = temp
                hasChanged = 1
            end if
        next i
        counter =counter -1
    loop while hasChanged = 1
    print "After Sort"
    for i = 1 to itemCount
        print item(i)
    next i
end

```



## Lisaac


```Lisaac
Section Header

+ name := BUBBLE_SORT;

- external := `#include <time.h>`;

Section Public

- main <- (
  + a : ARRAY(INTEGER);

  a := ARRAY(INTEGER).create 0 to 100;
  `srand(time(NULL))`;
  0.to 100 do { i : INTEGER;
    a.put `rand()`:INTEGER to i;
  };

  bubble a;

  a.foreach { item : INTEGER;
    item.print;
    '\n'.print;
  };
);

- bubble a : ARRAY(INTEGER) <- (
  + lower, size, t : INTEGER;
  + sorted : BOOLEAN;
  lower := a.lower;
  size := a.upper - lower + 1;
  {
    sorted := TRUE;
    size := size - 1;
    0.to (size - 1) do { i : INTEGER;
      (a.item(lower + i + 1) < a.item(lower + i)).if {
        t := a.item(lower + i + 1);
        a.put (a.item(lower + i)) to (lower + i + 1);
        a.put t to (lower + i);
        sorted := FALSE;
      };
    };
  }.do_while {!sorted};
);
```



## Lua



```Lua

function bubbleSort(A)
  local itemCount=#A
  local hasChanged
  repeat
    hasChanged = false
    itemCount=itemCount - 1
    for i = 1, itemCount do
      if A[i] > A[i + 1] then
        A[i], A[i + 1] = A[i + 1], A[i]
        hasChanged = true
      end
    end
  until hasChanged == false
end

```


Example:

```lua

list = { 5, 6, 1, 2, 9, 14, 2, 15, 6, 7, 8, 97 }
bubbleSort(list)
for i, j in pairs(list) do
    print(j)
end

```



## Lucid

[http://i.csc.uvic.ca/home/hei/lup/06.html]

```lucid
bsort(a) = if iseod(first a) then a else
              follow(bsort(allbutlast(b)),last(b)) fi
  where
   b = bubble(a);
   bubble(a) = smaller(max, next a)
       where
        max = first a fby larger(max, next a);
        larger(x,y) = if iseod(y) then y elseif x
       end;
   follow(x,y) = if xdone then y upon xdone else x fi
                   where
                      xdone = iseod x fby xdone or iseod x;
                   end;
   last(x) = (x asa iseod next x) fby eod;
   allbutlast(x) = if not iseod(next x) then x else eod fi;
  end
```




## M2000 Interpreter

'''A=(1,2,3,4)''' is a pointer to an auto array. We can read one item '''Array(A,0)=1''', or we can add 1 to all items '''A++''', or add 5 to all items '''A+=5'''. We can link to standard interface, '''Link A to A()''' so now '''A(1)++''' increment 2nd item by one. '''Print A''' print all items, one per column

'''A=Stack:=1,2,3,4''' is a pointer to a stack object. We can read any item using '''StackItem()''', from 1 (we can omit number 1 for first item, the top). Stack items can be move very fast, it is a linked list. To apply stack statements we have to make A as current stack (preserving current stack) using Stack A { }, so we can drop 2 items (1 and 2) using '''Stack A {Drop 2}'''. '''Print A''' print all items, one per column



```M2000 Interpreter

Module Bubble {
      function bubblesort {
                  dim a()
                  \\ []  is a stack object,  interpreter pass current stack pointer, and set a new stack for current stack
                  \\ array( stackobject ) get a stack object and return an array
                  a()=array([])
                  itemcount=len(a())
                   repeat {
                        haschange=false
                        if itemcount>1 then {
                              for index=0 to itemcount-2 {
                                  if a(index)>a(index+1) then swap a(index), a(index+1) : haschange=true
                              }
                        }
                       itemcount--
                   } until not haschange
                   =a()
      }
      \\ function can take parameters
      Print bubblesort(5,3,2,7,6,1)
      A=(2, 10, 17, 13, 20, 14, 3, 17, 16, 16)

      \\ !A copy values from array A to function stack
      B=bubblesort(!A)
      Print Len(A)=10
      Print B
      \\ Print array  in descending order
      k=each(b, -1, 1)
      While k {
            Print Array(k),
      }
      \\ sort two arrays in one
      Print BubbleSort(!A, !B)
      \\ We can use a stack object,  and values pop from object
      Z=Stack:=2, 10, 17, 13, 20, 14, 3, 17, 16, 16
      Print Len(Z)=10
      Def GetStack(x)=Stack(x)
      Z1=GetStack(BubbleSort(!Z))
      Print Type$(Z1)="mStiva"
      Print Z1
      Print Len(Z1)
      Print Len(Z)=0  ' now Z is empty
}
Bubble

```



## M4


```M4
divert(-1)

define(`randSeed',141592653)
define(`setRand',
   `define(`randSeed',ifelse(eval($1<10000),1,`eval(20000-$1)',`$1'))')
define(`rand_t',`eval(randSeed^(randSeed>>13))')
define(`random',
   `define(`randSeed',eval((rand_t^(rand_t<<18))&0x7fffffff))randSeed')

define(`set',`define(`$1[$2]',`$3')')
define(`get',`defn(`$1[$2]')')
define(`new',`set($1,size,0)')
dnl  for the heap calculations, it's easier if origin is 0, so set value first
define(`append',
   `set($1,size,incr(get($1,size)))`'set($1,get($1,size),$2)')

dnl  swap(<name>,<j>,<name>[<j>],<k>)  using arg stack for the temporary
define(`swap',`set($1,$2,get($1,$4))`'set($1,$4,$3)')

define(`deck',
   `new($1)for(`x',1,$2,
         `append(`$1',eval(random%100))')')
define(`show',
   `for(`x',1,get($1,size),`get($1,x) ')')
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')

define(`bubbleonce',
   `for(`x',1,$2,
      `ifelse(eval(get($1,x)>get($1,incr(x))),1,
         `swap($1,x,get($1,x),incr(x))`'1')')0')
define(`bubbleupto',
   `ifelse(bubbleonce($1,$2),0,
      `',
      `bubbleupto($1,decr($2))')')
define(`bubblesort',
   `bubbleupto($1,decr(get($1,size)))')

divert
deck(`a',10)
show(`a')
bubblesort(`a')
show(`a')
```


{{out}}

```txt
17 63 80 55 90 88 25 9 71 38

9 17 25 38 55 63 71 80 88 90
```



## Maple

<lang>arr := Array([17,3,72,0,36,2,3,8,40,0]):
len := numelems(arr):
while(true) do
	change := false:
	len--;
	for i from 1 to len do
		if (arr[i] > arr[i+1]) then
			temp := arr[i]:
			arr[i] := arr[i+1]:
			arr[i+1] := temp:
			change := true:
		end if:
	end do:
	if (not change) then break end if:
end do:
arr;
```

{{Out|Output}}

```txt
[0,0,2,3,3,8,17,36,40,72]
```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
bubbleSort[{w___, x_, y_, z___}] /; x > y := bubbleSort[{w, y, x, z}]
bubbleSort[sortedList_] := sortedList
```

Example:

```txt
bubbleSort[{10, 3, 7, 1, 4, 3, 8, 13, 9}]
{1, 3, 3, 4, 7, 8, 9, 10, 13}
```



## MATLAB



```MATLAB
function list = bubbleSort(list)

    hasChanged = true;
    itemCount = numel(list);

    while(hasChanged)

        hasChanged = false;
        itemCount = itemCount - 1;

        for index = (1:itemCount)

            if(list(index) > list(index+1))
                list([index index+1]) = list([index+1 index]); %swap
                hasChanged = true;
            end %if

        end %for
    end %while
end %bubbleSort
```


{{out}}

```txt
bubbleSort([5 3 8 4 9 7 6 2 1])

ans =

     1     2     3     4     5     6     7     8     9
```



## MAXScript


```maxscript
fn bubbleSort arr =
(
    while true do
    (
        changed = false
        for i in 1 to (arr.count - 1) do
        (
            if arr[i] > arr[i+1] then
            (
                swap arr[i] arr[i+1]
                changed = true
            )
        )
        if not changed then exit
    )
    arr
)
```

 -- Usage

```maxscript
myArr = #(9, 8, 7, 6, 5, 4, 3, 2, 1)
myArr = bubbleSort myArr
```



## MMIX


```mmix
Ja        IS $127

          LOC Data_Segment
DataSeg   GREG @
Array     IS @-Data_Segment
          OCTA   999,200,125,1,1020,40,4,5,60,100
ArrayLen  IS (@-Array-Data_Segment)/8

NL        IS  @-Data_Segment
	  BYTE #a,0
	  LOC  @+(8-@)&7

Buffer    IS @-Data_Segment


            LOC #1000
            GREG @
sorted      IS  $5
i           IS  $6
size        IS  $1
a           IS  $0
t           IS  $20
t1          IS  $21
t2          IS  $22
% Input: $0 ptr to array, $1 its length (in octabyte)
% Trashed: $5, $6, $1, $20, $21, $22
BubbleSort  SETL  sorted,1          % sorted = true
            SUB   size,size,1       % size--
            SETL  i,0               % i = 0
3H          CMP   t,i,size          % i < size ?
            BNN   t,1F              % if false, end for loop
            8ADDU $12,i,a           % compute addresses of the
            ADDU  t,i,1             % octas a[i] and a[i+1]
            8ADDU $11,t,a
            LDO   t1,$12,0          % get their values
            LDO   t2,$11,0
            CMP   t,t1,t2           % compare
            BN    t,2F              % if t1<t2, next
            STO   t1,$11,0          % else swap them
            STO   t2,$12,0
            SETL  sorted,0          % sorted = false
2H          INCL  i,1               % i++
            JMP   3B                % next (for loop)
1H          PBZ   sorted,BubbleSort % while sorted is false, loop
            GO    Ja,Ja,0

% Help function (Print an octabyte)
% Input:    $0 (the octabyte)
BufSize     IS    64
            GREG  @
PrintInt8   ADDU  t,DataSeg,Buffer  % get buffer address
            ADDU  t,t,BufSize       % end of buffer
            SETL  t1,0              % final 0 for Fputs
            STB   t1,t,0
1H          SUB   t,t,1             % t--
            DIV   $0,$0,10          % ($0,rR) = divmod($0,10)
            GET   t1,rR             % get reminder
            INCL  t1,'0'            % turn it into ascii digit
            STB   t1,t,0            % store it
            PBNZ  $0,1B             % if $0 /= 0, loop
            OR    $255,t,0          % $255 = t
            TRAP  0,Fputs,StdOut
            GO    Ja,Ja,0           % print and return


Main        ADDU  $0,DataSeg,Array  % $0 = Array address
            SETL  $1,ArrayLen       % $1 = Array Len
            GO    Ja,BubbleSort     % BubbleSort it
            SETL  $4,ArrayLen       % $4 = ArrayLen
	    ADDU  $3,DataSeg,Array  % $3 = Array address
2H          BZ    $4,1F             % if $4 == 0, break
            LDO   $0,$3,0           % $0 = * ($3 + 0)
            GO    Ja,PrintInt8      % print the octa
            ADDU  $255,DataSeg,NL   % add a trailing newline
	    TRAP  0,Fputs,StdOut
            ADDU  $3,$3,8           % next octa
            SUB   $4,$4,1           % $4--
	    JMP   2B                % loop
1H          XOR   $255,$255,$255
            TRAP  0,Halt,0          % exit(0)
```


=={{header|Modula-2}}==

```modula2
PROCEDURE BubbleSort(VAR a: ARRAY OF INTEGER);
  VAR
    changed:        BOOLEAN;
    temp, count, i: INTEGER;
BEGIN
  count := HIGH(a);
  REPEAT
    changed := FALSE;
    DEC(count);
    FOR i := 0 TO count DO
      IF a[i] > a[i+1] THEN
        temp := a[i];
        a[i] := a[i+1];
        a[i+1] := temp;
        changed := TRUE
      END
    END
  UNTIL NOT changed
END BubbleSort;
```


=={{header|Modula-3}}==


```modula3
MODULE Bubble;

PROCEDURE Sort(VAR a: ARRAY OF INTEGER) =
  VAR sorted: BOOLEAN;
      temp, len: INTEGER := LAST(a);
  BEGIN
    WHILE NOT sorted DO
      sorted := TRUE;
      DEC(len);
      FOR i := FIRST(a) TO len DO
        IF a[i+1] < a[i] THEN
          temp := a[i];
          a[i] := a[i + 1];
          a[i + 1] := temp;
          sorted := FALSE;
        END;
      END;
    END;
  END Sort;
END Bubble.
```



## N/t/roff


This program may output to paper (Postscript/PDF or actual printout) or a line-printer/terminal depending on the device specification.

This implementation is not reverse-compatible classical TROFF from Bell Labs, as TROFF then was extremely limited in what it could do.  It will work with GNU Troff, though.  The classical version of TROFF could only do recursive macro calls, which is integral to the functioning of <code>.AREADLN</code>, but not <code>.while</code> looping constructs, which is integral to the functioning of <code>.ASORT</code>; it could also only support numerical registers with name consisting of two characters maximum, so a register named <code>A9</code> would be okay (2 characters), but not <code>A123</code> (4 characters).

Block comments start with <code>.ig</code> and end with <code>..</code>.  Single-line comments begin with <code>\"</code>

{{works with|GROFF (GNU Troff)|1.22.2}}


```N/t/roff

.ig
Bubble sort algorithm in Troff

### ========================


:For: Rosetta Code
:Author: Stephanie Björk (Katt)
:Date: December 1, 2017
..
.ig
Array implementation: \(*A
---------------------------
This is an array implementation that takes advantage of Troff's numerical
registers.  Registers ``Ax``, where ``x`` is a base-10 Hindu-Arabic numeral and
0 < ``x`` < \(if, are used by array \(*A.  The array counter which holds the
number of items in the array is stored in register ``Ac``.  This array
implementation is one-indexed (array elements count from 1), though it could be
hardcoded again to become zero-indexed depending on what the programmer favours.
..
.nr Ac 0 1 \" Array counter
.
.de APUSH
.nr A\\n+(Ac \\$1
.. \" de APUSH
.
.de AREADLN
.APUSH \\$1
.if \\n(.$>1 \{ \
.	shift
.	AREADLN \\$*
\} \" if \\n(.$>1
.. \" de AREADLN
.
.de ASWAP
.nr tmp \\n[A\\$1]
.nr A\\$1 \\n[A\\$2]
.nr A\\$2 \\n[tmp]
.rm tmp
.. \" de ASWAP
.
.de ASORT
.nr swapped 1
.nr Ad \\n(Ac+1
.while \\n[swapped] \{ \
.	nr swapped 0
.	nr Ai 1
.	nr Ad -1
.	while \\n(Ai<\\n(Ad \{ \
.		nr Aj \\n(Ai+1
.		if \\n[A\\n(Ai]>\\n[A\\n(Aj] \{ \
.			ASWAP \\n(Ai \\n(Aj
.			nr swapped 1
\} \" if \\n[A\\n(Ai]>\\n[A\\n(Aj]
.		nr Ai +1
\} \" while \\n(Ai<\\n(Ac
\} \" while \\n[swapped]
.. \" de ASORT
.
.ig
Begin Program
-------------
The program's procedural body.  Here, we push all our potentially-unsorted
integer tokens sequentially, call a subroutine to sort them, and print all the
sorted items.
..
.AREADLN 12 87 23 77 0 66 45 92 3 0 2 1 9 9 5 4 4 4 \" Our input items to sort.
.ASORT \" Sort all items in the array.
.
.\" Output sorted items
.nr Ai 0 1
.while \n(Ai<\n(Ac \n[A\n+[Ai]]

```



### Output

<code>
0 0 1 2 3 4 4 4 5 9 9 12 23 45 66 77 87 92
</code>


## Nemerle


### Functional


```Nemerle
using System;
using System.Console;

module Bubblesort
{
    Bubblesort[T] (x : list[T]) : list[T]
      where T : IComparable
    {
        def isSorted(y)
        {
            |[_] => true
            |y1::y2::ys => (y1.CompareTo(y2) < 0) && isSorted(y2::ys)
        }

        def sort(y)
        {
            |[y]        => [y]
            |y1::y2::ys => if (y1.CompareTo(y2) < 0) y1::sort(y2::ys)
                           else y2::sort(y1::ys)
        }

        def loop(y)
        {
            if (isSorted(y)) y else {def z = sort(y); loop(z)}
        }

        match(x)
        {
            |[]  => []
            |_   => loop(x)
        }
    }

    Main() : void
    {
        def empty = [];
        def single = [2];
        def several = [2, 6, 1, 7, 3, 9, 4];
        WriteLine(Bubblesort(empty));
        WriteLine(Bubblesort(single));
        WriteLine(Bubblesort(several));
    }
}
```


### Imperative

{{trans|C#}}
We use an array for this version so that we can update in place. We could use a C# style list (as in the C# example), but that seemed too easy to confuse with a Nemerle style list.

```Nemerle
using System;
using System.Console;

module Bubblesort
{
    public static Bubblesort[T](this x : array[T]) : void
      where T : IComparable
    {
        mutable changed = false;
        def ln = x.Length;

        do
        {
            changed = false;
            foreach (i in [0 .. (ln - 2)])
            {
                when (x[i].CompareTo(x[i + 1]) > 0)
                {
                    x[i] <-> x[i + 1];
                    changed = true;
                }
            }
        } while (changed);
    }

    Main() : void
    {
        def several = array[2, 6, 1, 7, 3, 9, 4];
        several.Bubblesort();
        foreach (i in several)
            Write($"$i  ");
    }
}
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
sortedList = bubbleSort(String[] Arrays.copyOf(placesList, placesList.length))

lists = [placesList, sortedList]
loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method bubbleSort(list = String[]) public constant binary returns String[]

listLen = list.length
loop i_ = 0 to listLen - 1
  loop j_ = i_ + 1 to listLen - 1
    if list[i_].compareTo(list[j_]) > 0 then do
      tmpstor  = list[j_]
      list[j_] = list[i_]
      list[i_] = tmpstor
      end
    end j_
  end i_

return list

```

{{out}}
<pre style="height: 20ex; overflow: scroll;">
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



### Translation of Pseudocode

This version is a direct implementation of this task's pseudocode.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

placesList = [String -
    "UK  London",     "US  New York"   -
  , "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham" -
  , "UK  Birmingham", "UK  Boston"     -
]
sortedList = bubbleSort(String[] Arrays.copyOf(placesList, placesList.length))

lists = [placesList, sortedList]
loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method bubbleSort(item = String[]) public constant binary returns String[]

hasChanged = boolean
itemCount = item.length
loop label h_ until \hasChanged
  hasChanged  = isFalse
  itemCount = itemCount - 1
  loop index = 0 to itemCount - 1
    if item[index].compareTo(item[index + 1]) > 0 then do
      swap            = item[index]
      item[index]     = item[index + 1]
      item[index + 1] = swap
      hasChanged      = isTrue
      end
    end index
  end h_

return item

method isTrue public constant binary returns boolean
  return 1 == 1

method isFalse public constant binary returns boolean
  return \isTrue

```



## Nim


```nim
proc bubbleSort[T](a: var openarray[T]) =
  var t = true
  for n in countdown(a.len-2, 0):
    if not t: break
    t = false
    for j in 0..n:
      if a[j] <= a[j+1]: continue
      swap a[j], a[j+1]
      t = true

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
bubbleSort a
echo a
```

{{out}}

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## Objeck

{{trans|C}}

```objeck

function : Swap(p : Int[]) ~ Nil {
  t := p[0];
  p[0] := p[1];
  p[1] := t;
}

function : Sort(a : Int[]) ~ Nil {
  do {
    sorted := true;
    size -= 1;
    for (i:=0; i<a->Size(); i+=1;) {
      if (a[i+1] < a[i]) {
        swap(a+i);
        sorted := 0;
      };
    };
  }
  while (sorted = false);
}

```


=={{header|Objective-C}}==

```objc
- (NSArray *) bubbleSort:(NSMutableArray *)unsorted {
    BOOL done = false;

    while (!done) {
        done = true;
        for (int i = 1; i < unsorted.count; i++) {
            if ( [[unsorted objectAtIndex:i-1] integerValue] > [[unsorted objectAtIndex:i] integerValue] ) {
                [unsorted exchangeObjectAtIndex:i withObjectAtIndex:i-1];
                done = false;
            }
        }
    }

    return unsorted;
}

```


## OCaml

Like the Haskell versions above:

This version checks for changes in a separate step for simplicity.

```ocaml
let rec bsort s =
  let rec _bsort = function
    | x :: x2 :: xs when x > x2 ->
        x2 :: _bsort (x :: xs)
    | x :: x2 :: xs ->
        x :: _bsort (x2 :: xs)
    | s -> s
  in
  let t = _bsort s in
    if t = s then t
    else bsort t
```


This version uses the polymorphic <tt>option</tt> type to designate unchanged lists. (The type signature of <tt>_bsort</tt> is now <tt>'a list -> 'a list option</tt>.) It is slightly faster than the previous one.

```ocaml
let rec bsort s =
  let rec _bsort = function
    | x :: x2 :: xs when x > x2 -> begin
        match _bsort (x :: xs) with
          | None -> Some (x2 :: x :: xs)
          | Some xs2 -> Some (x2 :: xs2)
      end
    | x :: x2 :: xs -> begin
        match _bsort (x2 :: xs) with
          | None -> None
          | Some xs2 -> Some (x :: xs2)
      end
    | _ -> None
  in
    match _bsort s with
      | None -> s
      | Some s2 -> bsort s2
```



## Octave


```octave
function s = bubblesort(v)
  itemCount = length(v);
  do
    hasChanged = false;
    itemCount--;
    for i = 1:itemCount
      if ( v(i) > v(i+1) )
	v([i,i+1]) = v([i+1,i]);  % swap
	hasChanged = true;
      endif
    endfor
  until(hasChanged == false)
  s = v;
endfunction
```



```octave
v = [9,8,7,3,1,100];
disp(bubblesort(v));
```



## ooRexx

===Reimplementation of [[#NetRexx|NetRexx]]===
{{trans|NetRexx}}
This version exploits the &quot;Collection Classes&quot; and some other features of the language that are only available in Open Object Rexx.

```ooRexx
/* Rexx */
Do
  placesList = sampleData()
  call show placesList
  say
  sortedList = bubbleSort(placesList)
  call show sortedList
  say

  return
End
Exit

-- -----------------------------------------------------------------------------
bubbleSort:
procedure
Do
  il = arg(1)
  sl = il~copy

  listLen = sl~size
  loop i_ = 1 to listLen
    loop j_ = i_ + 1 to listLen
      cmpi = sl[i_]
      cmpj = sl[j_]
      if cmpi > cmpj then do
        sl[i_] = cmpj
        sl[j_] = cmpi
        end
      end j_
    end i_
  return sl
End
Exit

-- -----------------------------------------------------------------------------
show:
procedure
Do
  al = arg(1)

  loop e_ over al
    say e_
    end e_

  return
End
Exit

-- -----------------------------------------------------------------------------
sampleData:
procedure
Do
  placesList = .array~of( ,
    "UK  London",     "US  New York",   "US  Boston",     "US  Washington", ,
    "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"      ,
    )

  return placesList
End
Exit


```

{{out}}
<pre style="height: 20ex; overflow: scroll;">
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


### Translation of Pseudocode

This version is a direct implementation of this task's pseudocode.

```ooRexx
/* Rexx */
Do
  placesList = sampleData()
  call show placesList
  say
  sortedList = bubbleSort(placesList)
  call show sortedList
  say

  return
End
Exit

-- -----------------------------------------------------------------------------
bubbleSort:
procedure
Do
  il = arg(1)
  sl = il~copy
  itemCount = sl~size

  loop label c_ until \hasChanged
    hasChanged = isFalse()
    itemCount = itemCount - 1
    loop i_ = 1 to itemCount
      if sl[i_] > sl[i_ + 1] then do
        t_         = sl[i_]
        sl[i_]     = sl[i_ + 1]
        sl[i_ + 1] = t_
        hasChanged = isTrue()
        end
      end i_
    end c_

  return sl
End
Exit

-- -----------------------------------------------------------------------------
show:
procedure
Do
  al = arg(1)

  loop e_ over al
    say e_
    end e_

  return
End
Exit

-- -----------------------------------------------------------------------------
sampleData:
procedure
Do
  placesList = .array~of( ,
    "UK  London",     "US  New York",   "US  Boston",     "US  Washington", ,
    "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"      ,
    )

  return placesList
End
Exit

-- -----------------------------------------------------------------------------
isTrue: procedure
  return (1 == 1)

-- -----------------------------------------------------------------------------
isFalse: procedure
  return \isTrue()

```


===Classic [[REXX|Rexx]] Implementation===
A more &quot;traditional&quot; implementation of version 1 using only Rexx primitive constructs.  This version has been tested with the ''Open Object Rexx'' and ''Regina'' Rexx interpreters and could equally have been exhibited as a [[#REXX|Rexx]] solution.

```ooRexx
/* Rexx */
Do
  placesList. = ''
  sortedList. = ''
  call sampleData
  call bubbleSort

  do i_ = 1 to placesList.0
    say placesList.i_
    end i_
  say

  do i_ = 1 to sortedList.0
    say sortedList.i_
    end i_
  say

  return
End
Exit

/* -------------------------------------------------------------------------- */
bubbleSort:
procedure expose sortedList. placesList.
Do
  /* Copy list */
  do !_ = 0 to placesList.0
    sortedList.!_ = placesList.!_
    end !_

  listLen = sortedList.0
  do i_ = 1 to listLen
    do j_ = i_ + 1 to listLen
      if sortedList.i_ > sortedList.j_ then do
        !_            = sortedList.j_
        sortedList.j_ = sortedList.i_
        sortedList.i_ = !_
        end
      end j_
    end i_
  return
End
Exit

/* -------------------------------------------------------------------------- */
sampleData:
procedure expose placesList.
Do
  ! = 0
  ! = ! + 1; placesList.0 = !; placesList.! = "UK  London"
  ! = ! + 1; placesList.0 = !; placesList.! = "US  New York"
  ! = ! + 1; placesList.0 = !; placesList.! = "US  Boston"
  ! = ! + 1; placesList.0 = !; placesList.! = "US  Washington"
  ! = ! + 1; placesList.0 = !; placesList.! = "UK  Washington"
  ! = ! + 1; placesList.0 = !; placesList.! = "US  Birmingham"
  ! = ! + 1; placesList.0 = !; placesList.! = "UK  Birmingham"
  ! = ! + 1; placesList.0 = !; placesList.! = "UK  Boston"

  return
End
Exit

```



## Oz

In-place sorting of mutable arrays:

```oz
declare
  proc {BubbleSort Arr}
     proc {Swap I J}
        Arr.J := (Arr.I := Arr.J) %% assignment returns the old value
     end
     IsSorted = {NewCell false}
     MaxItem = {NewCell {Array.high Arr}-1}
  in
     for until:@IsSorted do
        IsSorted := true
        for I in {Array.low Arr}..@MaxItem do
           if Arr.I > Arr.(I+1) then
              IsSorted := false
              {Swap I I+1}
           end
        end
        MaxItem := @MaxItem - 1
     end
  end
  Arr = {Tuple.toArray unit(10 9 8 7 6 5 4 3 2 1)}
in
  {BubbleSort Arr}
  {Inspect Arr}
```


Purely-functional sorting of immutable lists:

```oz
declare
  local
     fun {Loop Xs Changed ?IsSorted}
        case Xs
        of X1|X2|Xr andthen X1 > X2 then
           X2|{Loop X1|Xr true IsSorted}
        [] X|Xr then
           X|{Loop Xr Changed IsSorted}
        [] nil then
           IsSorted = {Not Changed}
           nil
        end
     end
  in
     fun {BubbleSort Xs}
        IsSorted
        Result = {Loop Xs false ?IsSorted}
     in
        if IsSorted then Result
        else {BubbleSort Result}
        end
     end
  end
in
  {Show {BubbleSort [3 1 4 1 5 9 2 6 5]}}
```



## PARI/GP


```parigp
bubbleSort(v)={
  for(i=1,#v-1,
    for(j=i+1,#v,
      if(v[j]<v[i],
        my(t=v[j]);
        v[j]=v[i];
        v[i]=t
      )
    )
  );
  v
};
```



## Pascal


```pascal
procedure bubble_sort(var list: array of real);
var
  i, j, n: integer;
  t: real;
begin
  n := length(list);
  for i := n downto 2 do
    for j := 0 to i - 1 do
      if list[j] > list[j + 1] then
      begin
        t := list[j];
        list[j] := list[j + 1];
        list[j + 1] := t;
      end;
end;
```


Usage:
```pascal
var
  list: array[0 .. 9] of real;
// ...
bubble_sort(list);
```



## Perl


```perl
# Sorts an array in place
sub bubble_sort {
    for my $i (0 .. $#_){
        for my $j ($i + 1 .. $#_){
            $_[$j] < $_[$i] and @_[$i, $j] = @_[$j, $i];
        }
    }
}
```


Usage:


```perl
my @a = (39, 25, 30, 28, 36, 72, 98, 25, 43, 38);
bubble_sort(@a);
```



## Perl 6

{{works with|Rakudo|#24 "Seoul"}}


```perl6
sub bubble_sort (@a) {
    for ^@a -> $i {
        for $i ^..^ @a -> $j {
            @a[$j] < @a[$i] and @a[$i, $j] = @a[$j, $i];
        }
    }
}
```



## Phix

Copy of [[Sorting_algorithms/Bubble_sort#Euphoria|Euphoria]]

```Phix
function bubble_sort(sequence s)
object tmp
integer changed
    for j=length(s) to 1 by -1 do
        changed = 0
        for i=1 to j-1 do
            if s[i]>s[i+1] then
                {s[i],s[i+1],changed} = {s[i+1],s[i],1}
            end if
        end for
        if changed=0 then exit end if
    end for
    return s
end function

constant s = {4, 15, "delta", 2, -31, 0, "alfa", 19, "gamma", 2, 13, "beta", 782, 1}

puts(1,"Before: ")
?s
puts(1,"After: ")
?bubble_sort(s)
```

{{out}}

```txt

Before: {4,15,"delta",2,-31,0,"alpha",19,"gamma",2,13,"beta",782,1}
After: {-31,0,1,2,2,4,13,15,19,782,"alpha","beta","delta","gamma"}

```



## PHP



```php
function bubbleSort(array &$array) {
  $c = count($array) - 1;
  do {
    $swapped = false;
    for ($i = 0; $i < $c; ++$i) {
      if ($array[$i] > $array[$i + 1]) {
        list($array[$i + 1], $array[$i]) =
                array($array[$i], $array[$i + 1]);
        $swapped = true;
      }
    }
  } while ($swapped);
  return $array;
}
```



## PL/I


```pli
/* A primitive bubble sort */
bubble_sort: procedure (A);
   declare A(*) fixed binary;
   declare temp fixed binary;
   declare i fixed binary, no_more_swaps bit (1) aligned;

   do until (no_more_swaps);
      no_more_swaps = true;
      do i = lbound(A,1) to hbound(A,1)-1;
         if A(i) > A(i+1) then
            do; temp = A(i); A(i) = A(i+1); A(i+1) = temp;
                no_more_swaps = false;
            end;
      end;
   end;
end bubble_sort;
```



## PicoLisp


```PicoLisp
(de bubbleSort (Lst)
   (use Chg
      (loop
         (off Chg)
         (for (L Lst (cdr L) (cdr L))
            (when (> (car L) (cadr L))
               (xchg L (cdr L))
               (on Chg) ) )
         (NIL Chg Lst) ) ) )
```



## Pop11


```pop11
define bubble_sort(v);
lvars n=length(v), done=false, i;
while not(done) do
   true -> done;
   n - 1 -> n;
   for i from 1 to n do
      if v(i) > v(i+1) then
         false -> done;
         ;;; Swap using multiple assignment
         (v(i+1), v(i)) -> (v(i), v(i+1));
      endif;
   endfor;
endwhile;
enddefine;

;;; Test it
vars ar = { 10 8 6 4 2 1 3 5 7 9};
bubble_sort(ar);
ar =>
```



## PostScript


```PostScript

/bubblesort{
/x exch def
/temp x x length 1 sub get def
/i x length 1 sub def
/j i 1 sub def

x length 1 sub{
i 1 sub{
x j 1 sub get x j get lt
{
/temp x j 1 sub get def
x j 1 sub x j get put
x j temp put
}if
/j j 1 sub def
}repeat
/i i 1 sub def
/j i 1 sub def
}repeat
x pstack
}def

```



## PowerShell


```powershell
function bubblesort ($a) {
    $l = $a.Length
    $hasChanged = $true
    while ($hasChanged) {
        $hasChanged = $false
        $l--
        for ($i = 0; $i -lt $l; $i++) {
            if ($a[$i] -gt $a[$i+1]) {
                $a[$i], $a[$i+1] = $a[$i+1], $a[$i]
                $hasChanged = $true
            }
        }
    }
}
```



## Prolog

It's surprisingly easy in Prolog while coding this sort, to accidentally create a sort that is similar, but not identical
to the bubble sort algorithm. Some of these are easier and shorter to code and work as well if not better.  Having said that,
it's difficult to think of a reason to code the bubble sort these days except as an example of inefficiency.

```prolog
%___________________________________________________________________________
% Bubble sort

bubble(0, Res, Res, sorted).
bubble(Len, [A,B|T], Res, unsorted) :- A > B, !, bubble(Len,[B,A|T], Res, _).
bubble(Len, [A|T], [A|Ts], Ch) :- L is Len-1, bubble(L, T, Ts, Ch).

bubblesort(In, Out) :- length(In, Len), bubblesort(Len, In, Out).
bubblesort(0, In, In).
bubblesort(Len, In, Out) :-
	bubble(Len, In, Bubbled, SortFlag),  % bubble the list
	(SortFlag=sorted -> Out=Bubbled;     % list is already sorted
	 SegLen is Len - 1,		     % one fewer to process
	 writef('bubbled=%w\n', [Bubbled]),  % show progress
	 bubblesort(SegLen, Bubbled, Out)).

test :-  In = [8,9,1,3,4,2,6,5,4],
	 writef('  input=%w\n', [In]),
	 bubblesort(In, R),
	 writef('-> %w\n', [R]).
```

for example:

```txt
?- test.
  input=[8,9,1,3,4,2,6,5,4]
bubbled=[8,1,3,4,2,6,5,4,9]
bubbled=[1,3,4,2,6,5,4,8,9]
bubbled=[1,3,2,4,5,4,6,8,9]
bubbled=[1,2,3,4,4,5,6,8,9]
-> [1,2,3,4,4,5,6,8,9]
true.
```


### Alternative version

Should be ISO (but tested only with GNU Prolog).
Note: doesn't constuct list for each swap, only for each pass.

```prolog
:- initialization(main).


bubble_sort(Xs,Res) :-
    write(Xs), nl
  , bubble_pass(Xs,Ys,Changed)
  , ( Changed == true -> bubble_sort(Ys,Res) ; Res = Xs )
  .

bubble_pass(Xs,Res,Changed) :-
    Xs = [X|Ys], Ys = [Y|Zs]
  , ( X > Y -> H = Y, T = [X|Zs], Changed = true
             ; H = X, T = Ys
    )
  , Res = [H|R], !, bubble_pass(T,R,Changed)
  ; Res = Xs
  .


test([8,9,1,3,4,2,6,5,4]).

main :- test(T), bubble_sort(T,_), halt.
```

{{Out}}

```txt
[8,9,1,3,4,2,6,5,4]
[8,1,3,4,2,6,5,4,9]
[1,3,4,2,6,5,4,8,9]
[1,3,2,4,5,4,6,8,9]
[1,2,3,4,4,5,6,8,9]
```



## PureBasic


```PureBasic
Procedure bubbleSort(Array a(1))
  Protected i, itemCount, hasChanged

  itemCount = ArraySize(a())
  Repeat
    hasChanged = #False
    itemCount - 1
    For i = 0 To itemCount
      If a(i) > a(i + 1)
        Swap a(i), a(i + 1)
        hasChanged = #True
      EndIf
    Next
  Until hasChanged = #False
EndProcedure
```



## Python


```python
def bubble_sort(seq):
    """Inefficiently sort the mutable sequence (list) in place.
       seq MUST BE A MUTABLE SEQUENCE.

       As with list.sort() and random.shuffle this does NOT return
    """
    changed = True
    while changed:
        changed = False
        for i in xrange(len(seq) - 1):
            if seq[i] > seq[i+1]:
                seq[i], seq[i+1] = seq[i+1], seq[i]
                changed = True
    return seq

if __name__ == "__main__":
   """Sample usage and simple test suite"""

   from random import shuffle

   testset = range(100)
   testcase = testset[:] # make a copy
   shuffle(testcase)
   assert testcase != testset  # we've shuffled it
   bubble_sort(testcase)
   assert testcase == testset  # we've unshuffled it back into a copy
```



## Qi


```Qi
(define bubble-shot
  [A]     -> [A]
  [A B|R] -> [B|(bubble-shot [A|R])] where (> A B)
  [A  |R] -> [A|(bubble-shot R)])

(define bubble-sort
  X -> (fix bubble-shot X))

(bubble-sort [6 8 5 9 3 2 2 1 4 7])

```



## R


```R
bubblesort <- function(v) {
  itemCount <- length(v)
  repeat {
    hasChanged <- FALSE
    itemCount <- itemCount - 1
    for(i in 1:itemCount) {
      if ( v[i] > v[i+1] ) {
        t <- v[i]
        v[i] <- v[i+1]
        v[i+1] <- t
        hasChanged <- TRUE
      }
    }
    if ( !hasChanged ) break;
  }
  v
}

v <- c(9,8,7,3,1,100)
print(bubblesort(v))
```



## Ra


```Ra

class BubbleSort
	**Sort a list with the Bubble Sort algorithm**

	on start

		args := program arguments
		.sort(args)
		print args

	define sort(list) is shared
		**Sort the list**

		test
			list := [4, 2, 7, 3]
			.sort(list)
			assert list = [2, 3, 4, 7]

		body
			last := list.count - 1

			post while changed

				changed := false

				for i in last

					if list[i] > list[i + 1]
						temp := list[i]
						list[i] := list[i + 1]
						list[i + 1] := temp
						changed := true

```



## Racket


This bubble sort sorts the elelement in the vector v with regard to <?.


```racket

#lang racket

(define (bubble-sort <? v)
  (define len (vector-length v))
  (define ref vector-ref)
  (let loop ([max len]
             [again? #f])
    (for ([i (in-range 0 (- max 1))]
          [j (in-range 1 max)])
      (define vi (ref v i))
      (when (<? (ref v j) vi)
        (vector-set! v i (ref v j))
        (vector-set! v j vi)
        (set! again? #t)))
    (when again? (loop (- max 1) #f)))
  v)

```


Example: Sorting a vector of length 10 with random entries.

```racket

(bubble-sort < (for/vector ([_ 10]) (random 20)))

```



## REALbasic


Sorts an array of Integers


```vb

  Dim sortable() As Integer = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  sortable.Shuffle() ' sortable is now randomized
  Dim swapped As Boolean
  Do
    Dim index, bound As Integer
    bound = sortable.Ubound

    While index < bound
      If sortable(index) > sortable(index + 1) Then
        Dim s As Integer = sortable(index)
        sortable.Remove(index)
        sortable.Insert(index + 1, s)
        swapped = True
      End If
      index = index + 1
    Wend

  Loop Until Not swapped
'sortable is now sorted

```



## REXX

===version 0, alpha-numeric vertical list===
This REXX version sorts (using a bubble sort) and displays an array   (of alpha-numeric items)   in a vertical list.

```rexx
/*REXX program sorts an array  (of any kind of items)  using the  bubble─sort algorithm.*/
call gen                                         /*generate the array elements  (items).*/
call show   'before sort'                        /*show the  before  array elements.    */
     say copies('▒', 70)                         /*show a separator line (before/after).*/
call bSort         #                             /*invoke the bubble sort  with # items.*/
call show   ' after sort'                        /*show the  after   array elements.    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bSort: procedure expose @.;  parse arg n         /*N: is the number of @ array elements.*/
         do m=n-1  by -1  until ok;         ok=1 /*keep sorting the  @ array until done.*/
           do j=1  for m;  k=j+1;  if @.j<=@.k  then iterate       /*elements in order? */
           _=@.j;  @.j=@.k;  @.k=_;         ok=0 /*swap two elements;  flag as not done.*/
           end   /*j*/
         end     /*m*/;        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen: @.=;         @.1 = '---letters of the Hebrew alphabet---' ;   @.13= "kaph    [kaf]"
                  @.2 = '
### ==============================
' ;   @.14= "lamed"
                  @.3 = 'aleph   [alef]'                       ;   @.15= "mem"
                  @.4 = 'beth    [bet]'                        ;   @.16= "nun"
                  @.5 = 'gimel'                                ;   @.17= "samekh"
                  @.6 = 'daleth  [dalet]'                      ;   @.18= "ayin"
                  @.7 = 'he'                                   ;   @.19= "pe"
                  @.8 = 'waw     [vav]'                        ;   @.20= "sadhe   [tsadi]"
                  @.9 = 'zayin'                                ;   @.21= "qoph    [qof]"
                  @.10= 'heth    [het]'                        ;   @.22= "resh"
                  @.11= 'teth    [tet]'                        ;   @.23= "shin"
                  @.12= 'yod'                                  ;   @.24= "taw     [tav]"
        do #=1  until @.#=='';  end;      #=#-1  /*determine #elements in list; adjust #*/
     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:   do j=1  for #; say '     element' right(j,length(#)) arg(1)":"  @.j; end;   return
```

{{out|output|text=  when using the internal array list:}}

(Shown at   '''<sup>5</sup>/<sub>6</sub>'''   size.)
<pre style="font-size:84%">
     element  1 before sort: ---letters of the Hebrew alphabet---
     element  2 before sort:
### ==============================

     element  3 before sort: aleph   [alef]
     element  4 before sort: beth    [bet]
     element  5 before sort: gimel
     element  6 before sort: daleth  [dalet]
     element  7 before sort: he
     element  8 before sort: waw     [vav]
     element  9 before sort: zayin
     element 10 before sort: heth    [het]
     element 11 before sort: teth    [tet]
     element 12 before sort: yod
     element 13 before sort: kaph    [kaf]
     element 14 before sort: lamed
     element 15 before sort: mem
     element 16 before sort: nun
     element 17 before sort: samekh
     element 18 before sort: ayin
     element 19 before sort: pe
     element 20 before sort: sadhe   [tsadi]
     element 21 before sort: qoph    [qof]
     element 22 before sort: resh
     element 23 before sort: shin
     element 24 before sort: taw     [tav]
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
     element  1  after sort: ---letters of the Hebrew alphabet---
     element  2  after sort:
### ==============================

     element  3  after sort: aleph   [alef]
     element  4  after sort: ayin
     element  5  after sort: beth    [bet]
     element  6  after sort: daleth  [dalet]
     element  7  after sort: gimel
     element  8  after sort: he
     element  9  after sort: heth    [het]
     element 10  after sort: kaph    [kaf]
     element 11  after sort: lamed
     element 12  after sort: mem
     element 13  after sort: nun
     element 14  after sort: pe
     element 15  after sort: qoph    [qof]
     element 16  after sort: resh
     element 17  after sort: sadhe   [tsadi]
     element 18  after sort: samekh
     element 19  after sort: shin
     element 20  after sort: taw     [tav]
     element 21  after sort: teth    [tet]
     element 22  after sort: waw     [vav]
     element 23  after sort: yod
     element 24  after sort: zayin

```


===version 1, random integers, horizontal list===
This REXX version sorts (using a bubble sort) and displays a random array of numbers   (amount is specifiable from the command line)   in a horizontal list.

Programming note:   a check was made to not exceed REXX's upper range limit of the   '''random'''   BIF.

```rexx
/*REXX program sorts an array (of any kind of numbers)  using the bubble─sort algorithm.*/
parse arg N .;   if N=='' | N==","  then N=30    /*obtain optional size of array from CL*/
call gen  N                                      /*generate the array elements (items). */
call show        'before sort:'                  /*show the   before   array elements.  */
call bSort  N                                    /*invoke the bubble sort  with N items.*/
call show        ' after sort:'                  /*show the   after    array elements.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bSort: procedure expose @.;  parse arg n         /*N: is the number of @ array elements.*/
       do m=n-1  by -1  until ok;     ok=1       /*keep sorting the  @ array until done.*/
           do j=1  for m;   k=j+1;  if @.j>@.k  then parse value @.j @.k 0 with @.k @.j ok
           end   /*j*/                           /* [↑]  swap 2 elements, flag as ¬done.*/
       end       /*m*/;      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen:   h=min(N+N,1e5);  w=length(h);      do j=1  for N;  @.j=random(h);  end;      return
show:  parse arg $;  do k=1  for N;  $=$  right(@.k, w);         end;     say $;    return
```

{{out|output|text=  when using a internally generated random array of thirty integers   (which are right-justified for alignment in the display):}}

```txt

before sort: 20 57 49 20 31 51 37  1  8  0 38 42 33 41  5 23 34 60 10 15 60 54 36 13 25 24 59  3 35 10
 after sort:  0  1  3  5  8 10 10 13 15 20 20 23 24 25 31 33 34 35 36 37 38 41 42 49 51 54 57 59 60 60

```


===version 2, random integers, horizontal list===
{{trans|PL/I}}

```rexx
Call random ,,1000
Do i=1 To 10
  a.i=random(20)
  End
a.0=i-1
Call show 'vorher '
Call bubble_sort
Call show 'nachher'
Exit
bubble_sort: Procedure Expose a.
  Do Until no_more_swaps
    no_more_swaps=1
    Do i=1 To a.0-1
      i1=i+1
      if a.i > a.i1 Then Do
        temp=a.i; a.i=a.i1; a.i1=temp
        no_more_swaps=0
      End
   End
  End
Return
show:
  l=''; Do i=1 To a.0; l=l a.i; End; Say arg(1)':'l
  Return
```

{{out}}

```txt
vorher : 9 17 16 19 5 7 3 20 16 0
nachher: 0 3 5 7 9 16 16 17 19 20
```


===version 3, random integers, horizontal list, with interim plots===
This REXX program is a modified version of the first REXX program, with produces a snapshot of the plot in progress.

The random number generation uses the numbers from     '''1''' ───► '''N'''     (in sequential
order),   and then those numbers

are randomized.   This is done to make the displaying of the plot symmetric   (a straight upward diagonal slope).

Note that the command to clear the terminal screen is hard-coded as:   '''CLS'''

Also note that only four snapshots of the sort-in-progress is shown here,   the REXX program will show a snapshot of ''every''

sorting pass;   the       ''at   (about)   nnn% sorted''       was added after-the-fact.

```rexx
/*REXX program sorts an array (of any kind of numbers)  using the bubble─sort algorithm.*/
parse arg N seed .                               /*obtain optional size of array from CL*/
if N=='' | N==","       then N=30                /*Not specified?  Then use the default.*/
if datatype(seed, 'W')  then call random ,,seed  /*An integer?  Use the seed for RANDOM.*/
call gen    N                                    /*generate the array elements (items). */
call show        'before sort:'                  /*show the   before   array elements.  */
              $$= $                              /*keep "before" copy for after the sort*/
call bSort  N                                    /*invoke the bubble sort  with N items.*/
          say $$
call show        ' after sort:'                  /*show the   after    array elements.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bSort: procedure expose @.;  parse arg #         /*N: is the number of @ array elements.*/
       call disp                                 /*show a snapshot of the unsorted array*/
       do m=#-1  by -1  until ok;    ok=1        /*keep sorting the  @ array until done.*/
           do j=1  for m;   k=j+1
           if @.j>@.k  then do;     parse value    @.j  @.k  0      with      @.k  @.j  ok
                            end
           end   /*j*/                           /* [↑]  swap 2 elements, flag as ¬done.*/
       call disp                                 /*show snapshot of partially sorted @. */
       end       /*m*/;      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen:   do j=1  for N;  @.j= j;  end
       do k=1  for N;  g= random(1,N);  parse value @.k @.g  with  @.g @.k;  end;   return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  parse arg $;  do k=1  for N;  $=$  right(@.k, length(N));  end;     say $;   return
/*──────────────────────────────────────────────────────────────────────────────────────*/
disp:  'CLS';    $.=                             /*"CLS" is the command to clear screen.*/
                     do e=1  for #;         $.e= '│'overlay("☼", $.e, @.e);     end  /*e*/
                     do s=#  for #  by -1;  say $.s;                            end  /*s*/
       say "└"copies('─', #)                     /*display the horizontal axis at bottom*/
       return
```

{{out|output|text=  when using the default input:}}

```txt

│                 ☼
│                    ☼
│        ☼
│         ☼
│ ☼
│                ☼
│                      ☼
│               ☼
│☼
│     ☼
│            ☼
│                       ☼                                 at 0% sorted
│                  ☼
│       ☼
│    ☼
│                     ☼
│                   ☼
│      ☼
│             ☼
│           ☼
│   ☼
│              ☼
│  ☼
│          ☼
└────────────────────────

```


```txt

│                             ☼
│                            ☼
│                           ☼
│                          ☼
│                         ☼
│                        ☼
│                       ☼
│                      ☼
│                     ☼
│     ☼
│                    ☼
│                   ☼
│            ☼
│                  ☼                                     at about 25% sorted
│                 ☼
│                ☼
│  ☼
│         ☼
│               ☼
│              ☼
│   ☼
│             ☼
│           ☼
│          ☼
│        ☼
│       ☼
│      ☼
│    ☼
│☼
│ ☼
└──────────────────────────────

```


```txt

│                             ☼
│                            ☼
│                           ☼
│                          ☼
│                         ☼
│                        ☼
│                       ☼
│                      ☼
│                     ☼
│                    ☼
│                   ☼
│                  ☼
│                 ☼
│                ☼
│               ☼                                        at about 50% sorted
│              ☼
│     ☼
│             ☼
│            ☼
│           ☼
│          ☼
│         ☼
│        ☼
│  ☼
│       ☼
│      ☼
│    ☼
│   ☼
│ ☼
│☼
└──────────────────────────────

```


```txt

│                             ☼
│                            ☼
│                           ☼
│                          ☼
│                         ☼
│                        ☼
│                       ☼
│                      ☼
│                     ☼
│                    ☼
│                   ☼
│                  ☼
│                 ☼                                     at 100% sorted
│                ☼
│               ☼
│              ☼
│             ☼
│            ☼
│           ☼
│          ☼
│         ☼
│        ☼
│       ☼
│      ☼
│     ☼
│    ☼
│   ☼
│  ☼
│ ☼
│☼
└──────────────────────────────


before sort: 11  3 15  4 12 14  7 20 22  5  8 19 24 13  6  1 16 23 17  2 10  9 21 18
 after sort:  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24

```



## Ring


```ring
bubbleList = [4,2,1,3]
flag = 0
bubbleSort(bubbleList)
see bubbleList

func bubbleSort A
     n = len(A)
     while flag = 0
           flag = 1
           for i = 1 to n-1
               if A[i] > A[i+1]
                  temp = A[i]
                  A[i] = A[i+1]
                  A[i+1] = temp
                  flag = 0
                ok
            next
      end

```



## Ruby

{{eff note|Ruby|Array.sort!}}This example adds the bubblesort! method to the Array object. Below are two different methods that show four different iterating constructs in ruby.


```ruby
class Array
  def bubblesort1!
    length.times do |j|
      for i in 1...(length - j)
        if self[i] < self[i - 1]
          self[i], self[i - 1] = self[i - 1], self[i]
        end
      end
    end
    self
  end
   def bubblesort2!
    each_index do |index|
      (length - 1).downto( index ) do |i|
        self[i-1], self[i] = self[i], self[i-1] if self[i-1] < self[i]
      end
    end
    self
  end
end
ary = [3, 78, 4, 23, 6, 8, 6]
ary.bubblesort1!
p ary
# => [3, 4, 6, 6, 8, 23, 78]
```



## Run BASIC


```runbasic
siz = 100
dim data$(siz)
unSorted = 1

WHILE unSorted
  unSorted = 0
  FOR i = 1 TO siz -1
    IF data$(i) > data$(i + 1) THEN
      tmp       = data$(i)
      data$(i)  = data$(i + 1)
      data$(i + 1) = tmp
      unSorted  = 1
    END IF
  NEXT
WEND
```



## Rust


```rust
fn bubble_sort<T: Ord>(values: &mut[T]) {
    let mut n = values.len();
    let mut swapped = true;

    while swapped {
        swapped = false;

        for i in 1..n {
            if values[i - 1] > values[i] {
                values.swap(i - 1, i);
                swapped = true;
            }
        }

        n = n - 1;
    }
}

fn main() {
    // Sort numbers.
    let mut numbers = [8, 7, 1, 2, 9, 3, 4, 5, 0, 6];
    println!("Before: {:?}", numbers);

    bubble_sort(&mut numbers);
    println!("After: {:?}", numbers);

    // Sort strings.
    let mut strings = ["empty", "beach", "art", "car", "deal"];
    println!("Before: {:?}", strings);

    bubble_sort(&mut strings);
    println!("After: {:?}", strings);
}
```



## Sather


```sather
class SORT{T < $IS_LT{T}} is
  private swap(inout a, inout b:T) is
    temp ::= a;
    a := b;
    b := temp;
  end;
  bubble_sort(inout a:ARRAY{T}) is
    i:INT;
    if a.size < 2 then return; end;
    loop
      sorted ::= true;
      loop i := 0.upto!(a.size - 2);
        if a[i+1] < a[i] then
          swap(inout a[i+1], inout a[i]);
          sorted := false;
        end;
      end;
      until!(sorted);
    end;
  end;
end;
```



```sather
class MAIN is
  main is
    a:ARRAY{INT} := |10, 9, 8, 7, 6, -10, 5, 4|;
    SORT{INT}::bubble_sort(inout a);
    #OUT + a + "\n";
  end;
end;
```


This should be able to sort (in ascending order) any object for which <code>is_lt</code> (less than) is defined.


## Scala

{{libheader|Scala}}
This slightly more complex version of Bubble Sort avoids errors with indices.


```scala
def bubbleSort[T](arr: Array[T])(implicit o: Ordering[T]) {
  import o._
  val consecutiveIndices = (arr.indices, arr.indices drop 1).zipped
  var hasChanged = true
  do {
    hasChanged = false
    consecutiveIndices foreach { (i1, i2) =>
      if (arr(i1) > arr(i2)) {
        hasChanged = true
        val tmp = arr(i1)
        arr(i1) = arr(i2)
        arr(i2) = tmp
      }
    }
  } while(hasChanged)
}
```



```scala
import scala.annotation.tailrec

def bubbleSort(xt: List[Int]) = {
  @tailrec
  def bubble(xs: List[Int], rest: List[Int], sorted: List[Int]): List[Int] = xs match {
    case x :: Nil =>
      if (rest.isEmpty) x :: sorted
      else bubble(rest, Nil, x :: sorted)
    case a :: b :: xs =>
      if (a > b) bubble(a :: xs, b :: rest, sorted)
      else       bubble(b :: xs, a :: rest, sorted)
  }
  bubble(xt, Nil, Nil)
}
```



## Scheme


```scheme
(define (bubble-sort x gt?)
  (letrec
    ((fix (lambda (f i)
       (if (equal? i (f i))
           i
           (fix f (f i)))))

     (sort-step (lambda (l)
        (if (or (null? l) (null? (cdr l)))
            l
            (if (gt? (car l) (cadr l))
                (cons (cadr l) (sort-step (cons (car l) (cddr l))))
                (cons (car  l) (sort-step (cdr l))))))))

  (fix sort-step x)))
```


This solution recursively finds the fixed point of sort-step. A comparison function must be passed to bubblesort. Example usages:

```scheme
(bubble-sort (list 1 3 5 9 8 6 4 2) >)
(bubble-sort (string->list "Monkey") char<?)
```


Here is the same function, using a different syntax:


```scheme
(define (bsort l gt?)
  (define (dosort l)
    (cond ((null? (cdr l))
           l)
          ((gt? (car l) (cadr l))
           (cons (cadr l) (dosort (cons (car l) (cddr l)))))
          (else
           (cons (car l) (dosort (cdr l))))))
  (let ((try (dosort l)))
    (if (equal? l try)
        l
        (bsort try gt?))))

```

For example, you could do

```scheme
(bsort > '(2 4 6 2))
(1 2 3)
```



## Scilab

<lang>function b=BubbleSort(a)
  n=length(a)
  swapped=%T
  while swapped
    swapped=%F
    for i=1:1:n-1
      if a(i)>a(i+1) then
        temp=a(i)
        a(i)=a(i+1)
        a(i+1)=temp
        swapped=%T
      end
    end
  end
  b=a
endfunction BubbleSort
```

{{out}}
<pre style="height:20ex">-->y=[5 4 3 2 1]
 y  =
     5.    4.    3.    2.    1.
-->x=BubbleSort(a)
 x  =
     1.    2.    3.    4.    5.
```



## Scratch

This solution is hosted at the [https://scratch.mit.edu/projects/65560042/ Scratch site], because it is difficult to document visual programming solutions directly here at Rosetta Code.  There you can see the solution results as well as examine the code.  This solution is intended to illustrate the Bubble sort algorithm rather than to maximize performance.  Scratch provides visual queues to indicate list access, and these are used to help show what is happening.


## Seed7


```seed7
const proc: bubbleSort (inout array elemType: arr) is func
  local
    var boolean: swapped is FALSE;
    var integer: i is 0;
    var elemType: help is elemType.value;
  begin
    repeat
      swapped := FALSE;
      for i range 1 to length(arr) - 1 do
        if arr[i] > arr[i + 1] then
          help := arr[i];
          arr[i] := arr[i + 1];
          arr[i + 1] := help;
          swapped := TRUE;
        end if;
      end for;
    until not swapped;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/sorting.htm#bubbleSort]

## Shen

Bubble sort a vector in-place, using the < operator for comparison.

```shen
(tc +)

(define swap
  { (vector number) --> number --> number --> (vector number) }
  A I1 I2 -> (let Z (<-vector A I1)
               (do (vector-> A I1 (<-vector A I2))
                   (vector-> A I2 Z))))

(define one-pass
  { (vector number) --> number --> boolean --> number --> boolean }
  A N Swapped N -> (do (if (> (<-vector A (- N 1)) (<-vector A N))
                           (swap A (- N 1) N))
                       Swapped)
  A N Swapped I -> (if (> (<-vector A (- I 1)) (<-vector A I))
                       (do (swap A (- I 1) I)
                           (one-pass A N true (+ I 1)))
                       (one-pass A N Swapped (+ I 1))))

(define bubble-h
  { boolean --> (vector number) --> number --> (vector number) }
  true A N -> (bubble-h (one-pass A N false 2) A N)
  false A N -> A)

(define bubble-sort
  { (vector number) --> (vector number) }
  A -> (let N (limit A)
         (bubble-h (one-pass A N false 2) A N)))
```



```shen
(datatype some-globals

  __________
  (value *arr*) : (vector number);)

(set *arr* (vector 5))
(vector-> (value *arr*) 1 5)
(vector-> (value *arr*) 2 1)
(vector-> (value *arr*) 3 4)
(vector-> (value *arr*) 4 2)
(vector-> (value *arr*) 5 8)
(bubble-sort (value *arr*))
```


Here is a more idiomatic implementation:
{{trans|Qi}}


```shen
(tc +)

(define bubble-shot
  { (vector number) --> (vector number) }
  (@v A <>) -> (@v A <>)
  (@v A B R) -> (@v B (bubble-shot (@v A R))) where (> A B)
  (@v A R) -> (@v A (bubble-shot R)))

(define bubble-sort
  { (vector number) --> (vector number) }
  X -> (fix (function bubble-shot) X))
```



```shen
(bubble-sort (@v 5 1 4 2 3 <>))
```



## Sidef


```ruby
func bubble_sort(arr) {
    loop {
        var swapped = false
        { |i|
            if (arr[i] > arr[i+1]) {
                arr[i, i+1] = arr[i+1, i]
                swapped = true
            }
        } << ^arr.end
        swapped || break
    }
    return arr
}
```



## Simula


```simula
BEGIN

    PROCEDURE BUBBLESORT(A); NAME A; INTEGER ARRAY A;
    BEGIN
       INTEGER LOW, HIGH, I;
       BOOLEAN SWAPPED;

       PROCEDURE SWAP(I, J); INTEGER I, J;
       BEGIN
           INTEGER TEMP;
           TEMP := A(I); A(I) := A(J); A(J) := TEMP;
       END**OF**SWAP;

       LOW := LOWERBOUND(A, 1);
       HIGH := UPPERBOUND(A, 1);
       SWAPPED := TRUE;
       WHILE SWAPPED DO
       BEGIN
         SWAPPED := FALSE;
         FOR I := LOW + 1 STEP 1 UNTIL HIGH DO
         BEGIN
           COMMENT IF THIS PAIR IS OUT OF ORDER ;
           IF A(I - 1) > A(I) THEN
           BEGIN
             COMMENT SWAP THEM AND REMEMBER SOMETHING CHANGED ;
             SWAP(I - 1, I);
             SWAPPED := TRUE;
           END;
         END;
       END;
    END**OF**BUBBLESORT;

    INTEGER ARRAY A(1:10);
    INTEGER I, N;
    I := 1;
    FOR N := 6, 8, 5, 9, 3, 2, 2, 1, 4, 7 DO
    BEGIN
        A(I) := N; I := I + 1;
    END;
    BUBBLESORT(A);
    FOR I:= 1 STEP 1 UNTIL 10 DO
        OUTINT(A(I), 5);
    OUTIMAGE;

END;
```

{{out}}

```txt

    1    2    2    3    4    5    6    7    8    9

```



## Smalltalk

A straight translation from the pseudocode above. Swap is done with a [[wp:Smalltalk#Code_blocks|block closure]].


```smalltalk
|item swap itemCount hasChanged|
item := #(1 4 5 6 10 8 7 61 0 -3) copy.
swap :=
	[:indexOne :indexTwo|
	|temp|
	temp := item at: indexOne.
	item at: indexOne put: (item at: indexTwo).
	item at: indexTwo put: temp].

itemCount := item size.
[hasChanged := false.
itemCount := itemCount - 1.
1 to: itemCount do:
	[:index |
	(item at: index) > (item at: index + 1) ifTrue:
		[swap value: index value: index + 1.
		hasChanged := true]].
hasChanged] whileTrue.
```



## SNOBOL4



```SNOBOL4
*       # Sort array in place, return array
        define('bubble(a,alen)i,j,ub,tmp') :(bubble_end)
bubble  i = 1; ub = alen
outer   gt(ub,1) :f(bdone)
        j = 1
inner   le(a<j>, a<j + 1>) :s(incrj)
        tmp = a<j>
        a<j> = a<j + 1>
        a<j + 1> = tmp
incrj   j = lt(j + 1,ub) j + 1 :s(inner)
        ub = ub - 1 :(outer)
bdone   bubble = a :(return)
bubble_end

*       # Fill array with test data
        str = '33 99 15 54 1 20 88 47 68 72'
        output = str; arr = array(10)
floop   i = i + 1; str span('0123456789') . arr<i> = :s(floop)

*       # Test and display
        bubble(arr,10); str = ''
sloop   j = j + 1; str = str arr<j> ' ' :s(sloop)
        output = trim(str)
end
```


{{out}}

```txt
33 99 15 54 1 20 88 47 68 72
1 15 20 33 47 54 68 72 88 99
```



## SPARK

{{works with|SPARK GPL|2010}}

The first version is based on the Ada version, with Integer for both the array index and the array element.

Static analysis of this code shows that it is guaranteed free of any run-time error when called from any other SPARK code.

```Ada
package Bubble
is

   type Arr is array(Integer range <>) of Integer;

   procedure Sort (A : in out Arr);
   --# derives A from *;

end Bubble;


package body Bubble
is
   procedure Sort (A : in out Arr)
   is
      Finished : Boolean;
      Temp     : Integer;
   begin
      if A'Last /= A'First then
         loop
            Finished := True;
            for J in Integer range A'First .. A'Last - 1 loop
               if A (J + 1) < A (J) then
                  Finished := False;
                  Temp := A (J + 1);
                  A (J + 1) := A (J);
                  A (J) := Temp;
               end if;
            end loop;
            --# assert A'Last /= A'First;
            exit when Finished;
         end loop;
      end if;
   end Sort;

end Bubble;

```

The next version has a postcondition to guarantee that the returned array is sorted correctly.  This requires the two proof rules that follow the source.  The Ada code is identical with the first version.

```Ada
package Bubble
is

   type Arr is array(Integer range <>) of Integer;

   --  Sorted is a proof function with the definition:
   --    Sorted(A, From_I, To_I)
   --      <->
   --    (for all I in Integer range From_I .. To_I - 1 =>
   --               (A(I) <= A(I + 1))) .
   --
   --# function Sorted (A            : Arr;
   --#                  From_I, To_I : Integer) return Boolean;

   procedure Sort (A : in out Arr);
   --# derives A from *;
   --# post Sorted(A, A'First, A'Last);

end Bubble;


package body Bubble
is
   procedure Sort (A : in out Arr)
   is
      Finished : Boolean;
      Temp     : Integer;
   begin
      if A'Last > A'First then
         loop
            Finished := True;
            for J in Integer range A'First .. A'Last - 1
            --# assert Finished -> Sorted(A, A'First, J);
            loop
               if A (J + 1) < A (J) then
                  Finished := False;
                  Temp := A (J + 1);
                  A (J + 1) := A (J);
                  A (J) := Temp;
               end if;
            end loop;
            --# assert A'Last /= A'First
            --#   and  (Finished -> Sorted(A, A'First, A'Last));
            exit when Finished;
         end loop;
      end if;
   end Sort;

end Bubble;

```

The proof rules are stated here without justification (but they are fairly obvious). A formal proof of these rules from the definition of Sorted has been completed.

```txt

bubble_sort_rule(1): sorted(A, I, J)
                       may_be_deduced_from
                     [ J <= I ] .

bubble_sort_rule(2): Fin -> sorted(A, I, J + 1)
                       may_be_deduced_from
                     [ Fin -> sorted(A, I, J),
                       element(A, [J]) <= element(A, [J + 1]) ] .

```

Both of the two versions above use an inner loop that scans over all the array on every pass of the outer loop.  This makes the proof in the second version very simple.

The final version scans over a reducing portion of the array in the inner loop, consequently the proof becomes more complex. The package specification for this version is the same as the second version above. The package body defines two more proof functions.

```Ada
package body Bubble
is
   procedure Sort (A : in out Arr)
   is
      Finished : Boolean;

      --  In_Position is a proof function with the definition:
      --    In_Position(A, A_Start, A_I, A_End)
      --      <->
      --    ((for all K in Integer range A_Start .. A_I - 1 =>
      --                (A(K) <= A(A_I)))
      --     and
      --     Sorted(A, A_I, A_End) .
      --
      --# function In_Position (A                  : Arr;
      --#                       A_Start, A_I, A_End : Integer) return Boolean;

      --  Swapped is a proof function with the definition:
      --    Swapped(A_In, A_Out, I1, I2)
      --      <->
      --    (A_Out = A_In[I1 => A_In(I2); I2 => A_In(I1)]).
      --
      --# function Swapped (A_In, A_Out : Arr;
      --#                   I1, I2      : Integer) return Boolean;

      procedure Swap (A  : in out Arr;
                      I1 : in     Integer;
                      I2 : in     Integer)
      --# derives A from *, I1, I2;
      --# pre  I1 in A'First .. A'Last
      --#  and I2 in A'First .. A'Last;
      --# post Swapped(A~, A, I1, I2);
      is
         Temp : Integer;
      begin
         Temp  := A(I2);
         A(I2) := A(I1);
         A(I1) := Temp;
      end Swap;
      pragma Inline (Swap);

   begin
      if A'Last > A'First then
         for I in reverse Integer range A'First + 1 .. A'Last loop
            Finished := True;
            for J in Integer range A'First .. I - 1 loop
               if A (J + 1) < A (J) then
                  Finished := False;
                  Swap (A, J, J + 1);
               end if;
               --# assert I% = I  --  I is unchanged by execution of the loop
               --#   and  (for all K in Integer range A'First .. J =>
               --#                    (A(K) <= A(J + 1)))
               --#   and  (I < A'Last -> In_Position(A, A'First, I + 1, A'Last))
               --#   and  (Finished -> Sorted(A, A'First, J + 1));
            end loop;
            exit when Finished;
            --# assert In_Position(A, A'First, I, A'Last);
         end loop;
      end if;
   end Sort;

end Bubble;

```

Completion of the proof of this version requires more rules than the previous version and they are rather more complex. Creation of these rules is quite straightforward - I tend to write whatever rules the Simplifier needs first and then validate them afterwards. A formal proof of these rules from the definition of Sorted, In_Position and Swapped has been completed.

```txt
bubble_sort_rule(1):  sorted(A, I, J)
                        may_be_deduced_from
                      [ J <= I ] .

bubble_sort_rule(2):  sorted(A, I - 1, J)
                        may_be_deduced_from
                      [ sorted(A, I, J),
                        element(A, [I - 1]) <= element(A, [I]) ] .

bubble_sort_rule(3):  Fin -> sorted(A, I, J + 1)
                        may_be_deduced_from
                      [ Fin -> sorted(A, I, J),
                        element(A, [J]) <= element(A, [J + 1]) ] .

bubble_sort_rule(4):  sorted(A, Fst, Lst)
                        may_be_deduced_from
                      [ sorted(A, Fst, I),
                        I < Lst -> in_position(A, Fst, I + 1, Lst),
                        I <= Lst ] .

bubble_sort_rule(5):  in_position(A, Fst, I, Lst)
                        may_be_deduced_from
                      [ I < Lst -> in_position(A, Fst, I + 1, Lst),
                        for_all(K : integer, Fst <= K and K <= I - 1
                                  -> element(A, [K]) <= element(A, [I])),
                        I >= Fst,
                        I <= Lst ] .

bubble_sort_rule(6):  I < Lst -> in_position(A2, Fst, I + 1, Lst)
                        may_be_deduced_from
                      [ I < Lst -> in_position(A1, Fst, I + 1, Lst),
                        swapped(A1, A2, J + 1, J + 2),
                        J + 2 < I + 1,
                        J >= Fst ] .

bubble_sort_rule(7):  I - 1 < Lst -> in_position(A2, Fst, I, Lst)
                        may_be_deduced_from
                      [ in_position(A1, Fst, I, Lst),
                        swapped(A1, A2, J, J + 1),
                        J + 1 < I,
                        J >= Fst ] .

bubble_sort_rule(8):  for_all(K : integer, I <= K and K <= I
                                 -> element(A, [K]) <= element(A, [I + 1]))
                        may_be_deduced_from
                      [ element(A, [I]) <= element(A, [I + 1]) ] .

bubble_sort_rule(9):  for_all(K : integer, I <= K and K <= I
                                 -> element(A2, [K]) <= element(A2, [I + 1]))
                        may_be_deduced_from
                      [ element(A1, [I]) > element(A1, [I + 1]),
                        swapped(A1, A2, I, I + 1) ] .

bubble_sort_rule(10): for_all(K2 : integer, Fst <= K2 and K2 <= J + 1
                                 -> element(A, [K2]) <= element(A, [J + 2]))
                        may_be_deduced_from
                      [ for_all(K1 : integer, Fst <= K1 and K1 <= J
                                   -> element(A, [K1]) <= element(A, [J + 1])),
                        element(A, [J + 1]) <= element(A, [J + 2]) ] .

bubble_sort_rule(11): for_all(K2 : integer, Fst <= K2 and K2 <= J + 1
                                 -> element(A2, [K2]) <= element(A2, [J + 2]))
                        may_be_deduced_from
                      [ for_all(K1 : integer, Fst <= K1 and K1 <= J
                                   -> element(A1, [K1]) <= element(A1, [J + 1])),
                        element(A1, [J + 1]) > element(A1, [J + 2]),
                        swapped(A1, A2, J + 1, J + 2) ] .

```


{{works with|SPARK GPL|2014}}

File '''bubble.ads''':

```ada
package Bubble with SPARK_Mode is

   type Arr is array (Integer range <>) of Integer;

   function Sorted (A : Arr) return Boolean is
     (for all I in A'First .. A'Last - 1 => A(I) <= A(I + 1))
       with
         Ghost,
         Pre => A'Last > Integer'First;

   function Bubbled (A : Arr) return Boolean is
     (for all I in A'First .. A'Last - 1 => A(I) <= A(A'Last))
       with
         Ghost,
         Pre => A'Last > Integer'First;

   procedure Sort (A : in out Arr)
     with
       Pre => A'Last > Integer'First and A'Last < Integer'Last,
     Post => Sorted (A);

end Bubble;

```


File '''bubble.adb''':

```ada
package body Bubble with SPARK_Mode is

   procedure Sort (A : in out Arr)
   is
      Prev : Arr (A'Range) with Ghost;
      Done : Boolean;
   begin
      for I in reverse A'First .. A'Last - 1 loop
         Prev := A;
         Done := True;
         for J in A'First .. I loop
            if A(J) > A(J + 1) then
               declare
                  TMP : Integer := A(J);
               begin
                  A(J) := A(J + 1);
                  A(J + 1) := TMP;
                  Done := False;
               end;
            end if;
            pragma Loop_Invariant (if Done then Sorted (A(A'First .. J + 1)));
            pragma Loop_Invariant (Bubbled (A(A'First .. J + 1)));
            pragma Loop_Invariant (A(J + 2 .. A'Last) = Prev(J + 2 .. A'Last));
            pragma Loop_Invariant (for some K in A'First .. J + 1 =>
                                     A(J + 1) = Prev(K));
         end loop;
         exit when Done;
         pragma Loop_Invariant (if Done then Sorted (A));
         pragma Loop_Invariant (Bubbled (A(A'First .. I + 1)));
         pragma Loop_Invariant (Sorted (A(I + 1 .. A'Last)));
      end loop;
   end Sort;

end Bubble;

```


File '''main.adb''':

```ada
with Ada.Integer_Text_IO;
with Bubble;

procedure Main is
   A : Bubble.Arr := (5,4,6,3,7,2,8,1,9);
begin
   Bubble.Sort (A);
   for I in A'Range loop
      Ada.Integer_Text_IO.Put (A(I));
   end loop;
end Main;

```


File '''bubble.gpr''':

```ada
project Bubble is

   for Main use ("main.adb");

end Bubble;

```


To verify the program, execute the command: '''gnatprove -P bubble.gpr -j0 --level=2'''

File '''gnatprove/gnatprove.out''':

```txt
Summary of SPARK analysis

### ===================


--------------------------------------------------------------------------------------------------------------------
SPARK Analysis results        Total       Flow   Interval   CodePeer                  Provers   Justified   Unproved
--------------------------------------------------------------------------------------------------------------------
Data Dependencies                 .          .          .          .                        .           .          .
Flow Dependencies                 .          .          .          .                        .           .          .
Initialization                    6          6          .          .                        .           .          .
Non-Aliasing                      .          .          .          .                        .           .          .
Run-time Checks                  36          .          .          .                36 (CVC4)           .          .
Assertions                       14          .          .          .    14 (CVC4 64%, Z3 36%)           .          .
Functional Contracts              7          .          .          .     7 (CVC4 89%, Z3 11%)           .          .
LSP Verification                  .          .          .          .                        .           .          .
--------------------------------------------------------------------------------------------------------------------
Total                            63    6 (10%)          .          .                 57 (90%)           .          .


Analyzed 2 units
in unit bubble, 4 subprograms and packages out of 4 analyzed
  Bubble at bubble.ads:1 flow analyzed (0 errors, 0 checks and 0 warnings) and proved (0 checks)
  Bubble.Bubbled at bubble.ads:11 flow analyzed (0 errors, 0 checks and 0 warnings) and proved (3 checks)
  Bubble.Sort at bubble.ads:17 flow analyzed (0 errors, 0 checks and 0 warnings) and proved (50 checks)
  Bubble.Sorted at bubble.ads:5 flow analyzed (0 errors, 0 checks and 0 warnings) and proved (4 checks)
in unit main, 0 subprograms and packages out of 1 analyzed
  Main at main.adb:4 skipped

```



## Standard ML

Assumes a list of integers.

```txt

fun bubble_select [] = []
  | bubble_select [a] = [a]
  | bubble_select (a::b::xs) =
    if b < a then b::(bubble_select(a::xs)) else a::(bubble_select(b::xs))

fun bubblesort [] = []
  | bubblesort (x::xs) =bubble_select (x::(bubblesort xs))

```



## Stata


```stata
mata
function bubble_sort(a) {
	n = length(a)
	for (j = n; j >= 2; j--) {
		q = 1
		for (i = 2; i <= j; i++) {
			if (a[i-1] > a[i]) {
				q = 0
				s = a[i-1]
				a[i-1] = a[i]
				a[i] = s
			}
		}
		if (q) return
	}
}
end
```



## Swift


```Swift>func bubbleSort<T:Comparable
(inout list:[T]) {
    var done = false
    while !done {
        done = true
        for i in 1..<list.count {
            if list[i - 1] > list[i] {
                (list[i], list[i - 1]) = (list[i - 1], list[i])
                done = false
            }
        }
    }
}
```


=={{header|TI-83 BASIC}}==
Input your data into L<sub>1</sub> and run this program to organize it.
 :L<sub>1</sub>→L<sub>2</sub>
 :1+dim(L<sub>2</sub>)→N
 :For(D,1,dim(L<sub>2</sub>))
 :N-1→N
 :0→I
 :For(C,1,dim(L<sub>2</sub>)-2)
 :For(A,dim(L<sub>2</sub>)-N+1,dim(L<sub>2</sub>)-1)
 :If L<sub>2</sub>(A)&gt;L<sub>2</sub>(A+1)
 :Then
 :1→I
 :L<sub>2</sub>(A)→B
 :L<sub>2</sub>(A+1)→L<sub>2</sub>(A)
 :B→L<sub>2</sub>(A+1)
 :End
 :End
 :End
 :If I=0
 :Goto C
 :End
 :Lbl C
 :If L<sub>2</sub>(1)&gt;L<sub>2</sub>(2)
 :Then
 :L<sub>2</sub>(1)→B
 :L<sub>2</sub>(2)→L<sub>2</sub>(1)
 :B→L<sub>2</sub>(2)
 :End
 :DelVar A
 :DelVar B
 :DelVar C
 :DelVar D
 :DelVar N
 :DelVar I
 :Return

[[wp:Odd-even sort|Odd-Even Bubble Sort]] (same IO):
 :"ODD-EVEN"
 :L<sub>1</sub>→L<sub>2</sub>(
 :1+dim(L<sub>2</sub>)→N
 :For(D,1,dim(L<sub>2</sub>))
 :N-1→N
 :0→O
 :For(C,1,dim(L<sub>2</sub>)-2)
 :For(A,dim(L<sub>2</sub>)-N+2,dim(L<sub>2</sub>)-1,2)
 :If L<sub>2</sub>(A)>L<sub>2</sub>(A+1)
 :Then
 :1→O
 :L<sub>2</sub>(A)→B
 :L<sub>2</sub>(A+1)→L<sub>2</sub>(A)
 :B→L<sub>2</sub>(A+1)
 :End
 :End
 :For(A,dim(L<sub>2</sub>)-N+1,dim(L<sub>2</sub>)-1,2)
 :If L<sub>2</sub>(A)>L<sub>2</sub>(A+1)
 :Then
 :1→O
 :L<sub>2</sub>(A)→B
 :L<sub>2</sub>(A+1)→L<sub>2</sub>(A)
 :B→L<sub>2</sub>(A+1)
 :End
 :End
 :End
 :If O=0
 :Goto C
 :End
 :Lbl C
 :If L<sub>2</sub>(1)>L<sub>2</sub>(2)
 :Then
 :L<sub>2</sub>(1)→B
 :L<sub>2</sub>(2)→L<sub>2</sub>(1)
 :B→L<sub>2</sub>(2)
 :End
 :DelVar A
 :DelVar B
 :DelVar C
 :DelVar D
 :DelVar N
 :DelVar O
 :Return

Implementation of the pseudo code given at the top of the page. Place data to be sorted in L<sub>1</sub>
 :dim(L<sub>1</sub>)→D
 :Repeat C=0
   :0→C
   :D–1→D
   :For(I,1,D)
     :If L<sub>1</sub>(I)>L<sub>1</sub>(I+1):Then
       :L<sub>1</sub>(I)→C
       :L<sub>1</sub>(I+1)→L<sub>1</sub>(I)
       :C→L<sub>1</sub>(I+1)
       :1→C
     :End
   :End
 :End
 :L<sub>1</sub>


## Tailspin


```tailspin

templates bubblesort
  templates bubble
    @: 1;
    1..$-1 -> #
    $@ !
    <?($@bubblesort($+1) <..~$@bubblesort($)>)>
      @: $;
      def temp: $@bubblesort($@);
      @bubblesort($@): $@bubblesort($@+1);
      @bubblesort($@+1): $temp;
  end bubble

  @: $;
  $::length -> #
  $@ !

  <2..>
    $ -> bubble -> #
end bubblesort

[4,5,3,8,1,2,6,7,9,8,5] -> bubblesort -> !OUT::write

```



## Tcl

{{tcllib|struct::list}}

```tcl
package require Tcl 8.5
package require struct::list

proc bubblesort {A} {
    set len [llength $A]
    set swapped true
    while {$swapped} {
        set swapped false
        for {set i 0} {$i < $len - 1} {incr i} {
            set j [expr {$i + 1}]
            if {[lindex $A $i] > [lindex $A $j]} {
                struct::list swap A $i $j
                set swapped true
            }
        }
        incr len -1
    }
    return $A
}

puts [bubblesort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
```


Idiomatic code uses the builtin <code>lsort</code> instead, which is a stable O(''n'' log ''n'') sort.


## Toka

Toka does not have a bubble sort predefined, but it is easy to code a simple one:


```toka
#! A simple Bubble Sort function
value| array count changed |
[ ( address count -- )
  to count to array
  count 0
  [ count 0
    [ i array array.get i 1 + array array.get 2dup >
      [ i array array.put  i 1 + array array.put ]
      [ 2drop ] ifTrueFalse
    ] countedLoop
    count 1 - to count
  ] countedLoop
] is bsort

#! Code to display an array
[ ( array count -- )
  0 swap [ dup i swap array.get . ] countedLoop drop cr
] is .array

#! Create a 10-cell array
10 cells is-array foo

#! Fill it with random values
  20  1 foo array.put
  50  2 foo array.put
 650  3 foo array.put
 120  4 foo array.put
 110  5 foo array.put
 101  6 foo array.put
1321  7 foo array.put
1310  8 foo array.put
 987  9 foo array.put
 10 10 foo array.put

#! Display the array, sort it, and display it again
foo 10 .array
foo 10 bsort
foo 10 .array
```



## TorqueScript



```TorqueScript
//Note that we're assuming that the list of numbers is separated by tabs.
function bubbleSort(%list)
{
	%ct = getFieldCount(%list);
	for(%i = 0; %i < %ct; %i++)
	{
		for(%k = 0; %k < (%ct - %i - 1); %k++)
		{
			if(getField(%list, %k) > getField(%list, %k+1))
			{
				%tmp = getField(%list, %k);
				%list = setField(%list, %k, getField(%list, %k+1));
				%list = setField(%list, %k+1, %tmp);
			}
		}
	}
	return %list;
}
```



## uBasic/4tH

<lang>PRINT "Bubble sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Bubblesort (n)
  PROC _ShowArray (n)
PRINT

END

_Bubblesort PARAM(1)                   ' Bubble sort
  LOCAL (2)

  DO
    b@ = 0
    FOR c@ = 1 TO a@-1
      IF @(c@-1) > @(c@) THEN PROC _Swap (c@, c@-1) : b@ = c@
    NEXT
    a@ = b@
    UNTIL b@ = 0
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


## Unicon

See [[#Icon|Icon]].


## UnixPipes


```bash
rm -f _sortpass

reset() {
   test -f _tosort || mv _sortpass _tosort
}

bpass() {
  (read a; read b
  test -n "$b" -a "$a" && (
      test $a -gt $b && (reset; echo $b;  (echo $a ; cat) | bpass ) || (echo $a;  (echo $b ; cat) | bpass )
  ) || echo $a)
}

bubblesort() {
  cat > _tosort
  while test -f _tosort
  do
      cat _tosort | (rm _tosort;cat) |bpass > _sortpass
  done
  cat _sortpass
}

cat to.sort | bubblesort
```



## Ursala

The bubblesort function is parameterized by a relational predicate.

```Ursala
#import nat

bubblesort "p" = @iNX ^=T ^llPrEZryPrzPlrPCXlQ/~& @l ~&aitB^?\~&a "p"?ahthPX/~&ahPfatPRC ~&ath2fahttPCPRC

#cast %nL

example = bubblesort(nleq) <362,212,449,270,677,247,567,532,140,315>
```

{{out}}

```txt
<140,212,247,270,315,362,449,532,567,677>
```



## VBA

{{trans|Phix}}
```vb
Private Function bubble_sort(s As Variant) As Variant
    Dim tmp As Variant
    Dim changed As Boolean
    For j = UBound(s) To 1 Step -1
        changed = False
        For i = 1 To j - 1
            If s(i) > s(i + 1) Then
                tmp = s(i)
                s(i) = s(i + 1)
                s(i + 1) = tmp
                changed = True
            End If
        Next i
        If Not changed Then
            Exit For
        End If
    Next j
    bubble_sort = s
End Function

Public Sub main()
    s = [{4, 15, "delta", 2, -31, 0, "alfa", 19, "gamma", 2, 13, "beta", 782, 1}]
    Debug.Print "Before: "
    Debug.Print Join(s, ", ")
    Debug.Print "After: "
    Debug.Print Join(bubble_sort(s), ", ")
End Sub
```
{{out}}

```txt
Before:
4, 15, delta, 2, -31, 0, alfa, 19, gamma, 2, 13, beta, 782, 1
After:
-31, 0, 1, 2, 2, 4, 13, 15, 19, 782, alfa, beta, delta, gamma
```


## VBScript

Doing the decr and incr thing is superfluous, really. I just had stumbled over the byref thing for <code>swap</code> and wanted to see where else it would work.

For those unfamiliar with Perth, WA Australia, the five strings being sorted are names of highways.


### ==Implementation==


```vb

sub decr( byref n )
	n = n - 1
end sub

sub incr( byref n )
	n = n + 1
end sub

sub swap( byref a, byref b)
	dim tmp
	tmp = a
	a = b
	b = tmp
end sub

function bubbleSort( a )
	dim changed
	dim itemCount
	itemCount = ubound(a)
	do
		changed = false
		decr itemCount
		for i = 0 to itemCount
			if a(i) > a(i+1) then
				swap a(i), a(i+1)
				changed = true
			end if
		next
	loop until not changed
	bubbleSort = a
end function

```



### ==Invocation==


```vb

dim a
a = array( "great eastern", "roe", "stirling", "albany", "leach")
wscript.echo join(a,", ")
bubbleSort a
wscript.echo join(a,", ")

```


{{out}}

```txt

great eastern, roe, stirling, albany, leach
albany, great eastern, leach, roe, stirling

```



## Visual Basic .NET

'''Platform:''' [[.NET]]

{{works with|Visual Basic .NET|9.0+}}

```vbnet
Do Until NoMoreSwaps = True
     NoMoreSwaps = True
     For Counter = 1 To (NumberOfItems - 1)
         If List(Counter) > List(Counter + 1) Then
             NoMoreSwaps = False
             Temp = List(Counter)
             List(Counter) = List(Counter + 1)
             List(Counter + 1) = Temp
         End If
     Next
     NumberOfItems = NumberOfItems - 1
Loop
```



## X86 Assembly

Translation of XPL0. Assemble with tasm, tlink /t

```asm
        .model  tiny
        .code
        .486
        org     100h
start:  mov     si, offset array
        mov     ax, 40          ;length of array (not including $)
        call    bsort
        mov     dx, si          ;point to array
        mov     ah, 09h         ;display it as a string
        int     21h
        ret
array   db      "Pack my box with five dozen liquor jugs.$"

;Bubble sort: si = array addrsss, ax = number of bytes
bsort:  pusha
        xchg    cx, ax          ;get size of array N
        dec     cx              ;for J:= N-1 downto 0
bs10:   xor     bx, bx          ;for I:= 0 to J-1
bs20:   mov     ax, [bx+si]
        cmp     al, ah          ;if A(I) > A(I+1) then
        jbe     bs30
         xchg   al, ah          ; swap bytes
         mov    [bx+si], ax
bs30:   inc     bx              ;next I
        cmp     bx, cx
        jb      bs20
        loop    bs10
        popa
        ret
        end     start
```


{{out}}

```txt

       .Pabcdeefghiiijklmnoooqrstuuvwxyz

```



## Xojo


```xojo
Dim temp, count As Integer
Dim isDirty As Boolean
count = Ubound(list) // count the array size

// loop through until we don't move any numbers... this means we are sorted
Do
  isDirty = False // we haven't touched anything yet
  For i As Integer = 1 To count - 1 // loop through all the numbers
    If list(i) > list(i + 1) Then // if the right number is smaller then the left.. swap
      temp = list(i + 1)
      list(i + 1) = list(i)
      list(i) = temp
      isDirty = True // we touched the data so mark it as dirty
    End
  Next
Loop Until isDirty = False // if we made it without touching the data then we are done
```




## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings

proc    BSort(A, N);            \Bubble sort array in ascending order
char    A;                      \address of array
int     N;                      \number of items in array (size)
int     I, J, T;
[for J:= N-1 downto 0 do
    for I:= 0 to J-1 do
        if A(I) > A(I+1) then
            [T:= A(I);  A(I):= A(I+1);  A(I+1):= T];    \swap items
];      \BSort

func    StrLen(Str);            \Return number of characters in an ASCIIZ string
char    Str;
int     I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;

char    Str;
[Str:= "Pack my box with five dozen liquor jugs.";
BSort(Str, StrLen(Str));
Text(0, Str);  CrLf(0);
]
```


{{out}}

```txt

"       .Pabcdeefghiiijklmnoooqrstuuvwxyz"

```



## Yabasic


```Yabasic
// Animated sort.
// Original idea by William Tang, obtained from MicroHobby 25 Years (https://microhobby.speccy.cz/zxsf/MH-25Years.pdf)

clear screen

n=15 : m=18 : y=9 : t$=chr$(17)+chr$(205)+chr$(205)
dim p(n), p$(n)

for x=1 TO n
    p(x)=ran(15)+1
    p$(x)=str$(p(x),"##.######")
    print at(0,x) p$(x)
next x

for j=1 to n-1
    for i=j+1 to n
        l=n+j-i+1
        if p(j) > p(l) then
            print color("yellow","red") at(0,j) p$(j)
            if l<>m then
                for x=m to l step sig(l-m): print at(18,x) t$ : print at (18,x+sig(m-l)) "   " : pause .02 : next x
            end if
            for x=17 TO y step -1 : print at(x,l) t$+" " : pause .02 : next x
            for x=0 TO 10 : print at(x,l) " "+p$(l)+t$ : pause .02 : next x
            for x=l TO j STEP -1 : print at(11,x) p$(l)+t$ : print at(11,x+1) "            " : pause .02 : next x
            print at(0,j) "            "
            for x=j+1 TO l-1 : print color("yellow","red") at(0,x) p$(j) : pause .02 : print at(0,x) p$(x) : pause .02 : next x
            print at(0,l) p$(j)
            for x=10 TO 0 step -1 : print at(x,j) p$(l)+t$+" " : pause .02 : next x
            for x=y TO 17 : print at(x,j) " "+t$ : pause .02 : next x
            m=j
            t=p(l) : tem$=p$(l)
            p(l)=p(j) : p$(l)=p$(j)
            p(j)=t : p$(j)=tem$
        end if
        pause .02
    next i
next j

for x=m TO 18 : print at(18,x-1) "   " : print at(18,x) t$ : pause .02 : next x

```



## Yorick


```yorick
func bubblesort(&items) {
  itemCount = numberof(items);
  do {
    hasChanged = 0;
    itemCount--;
    for(index = 1; index <= itemCount; index++) {
      if(items(index) > items(index+1)) {
        items([index,index+1]) = items([index+1,index]);
        hasChanged = 1;
      }
    }
  } while(hasChanged);
}
```



## zkl


```zkl
fcn bubbleSort(list){
   itemCount := list.len();
   do{
      hasChanged := False;
      foreach index in (itemCount -= 1){
	 if (list[index] > list[index + 1]){
	    list.swap(index,index + 1);
	    hasChanged = True;
	 }
      }
   }while(hasChanged);
   list
}
```

Or, punting early termination:

```zkl
fcn bubbleSort(list){
   foreach n,index in ([list.len()-1..0,-1],n){
      if (list[index] > list[index + 1]) list.swap(index,index + 1);
   }
   list
}
```


```zkl
bubbleSort("This is a test".split("")).println();
```

{{out}}

```txt
L(" "," "," ","T","a","e","h","i","i","s","s","s","t","t")
```



## ZX Spectrum Basic


```zxbasic
5000 CLS
5002 LET a$="": FOR f=1 TO 64: LET a$=a$+CHR$ (32+INT (RND*96)): NEXT f
5004 PRINT a$; AT 10,0;"ZigZag BubbleSORT"
5010 LET la=LEN a$
5011 LET i=1: LET u=0
5020 LET d=0: LET p=(u=0)-(u=1)
5021 LET l=(i AND u=0)+(la-i+u AND u=1)
5030 IF u=0 THEN  IF a$(l+1)>=a$(l) THEN  GO TO 5050
5031 IF u=1 THEN  IF a$(l-1)<=a$(l) THEN  GO TO 5050
5040 LET d=1
5042 LET t$=a$(l+p)
5043 LET a$(l+p)=a$(l)
5044 LET a$(l)=t$
5050 LET l=l+p
5051 PRINT AT 10,21;a$(l);AT 12,0;a$
5055 IF l<=la-i AND l>=i THEN  GO TO 5023
5061 LET i=i+NOT u
5063 LET u=NOT u
5064 IF d AND i<la THEN  GO TO 5020
5072 PRINT AT 12,0;a$
9000 STOP
```


The traditional solution:


```zxbasic
  10 LET siz=32
  20 DIM d$(siz)
  30 REM Populate d$
  40 FOR n=1 TO siz: LET d$(n)=CHR$ (48+INT (RND*75)): NEXT n
  50 PRINT d$
  60 LET unSorted=0
  70 FOR i=1 TO siz-1
  80 IF d$(i)>d$(i+1) THEN LET t$=d$(i): LET d$(i)=d$(i+1): LET d$(i+1)=t$: LET unSorted=1
  90 NEXT i
 100 IF unSorted THEN LET siz=siz-1: GO TO 60
 110 PRINT d$
```


{{omit from|GUISS}}

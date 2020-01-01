+++
title = "Sorting algorithms/Selection sort"
description = ""
date = 2019-10-20T04:11:13Z
aliases = []
[extra]
id = 3138
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}{{Sorting Algorithm}}

;Task:
Sort an [[array]] (or list) of elements using the Selection sort algorithm.


It works as follows:

First find the smallest element in the array and exchange it with the element in the first position, then find the second smallest element and exchange it with the element in the second position, and continue in this way until the entire array is sorted.


Its asymptotic complexity is   <big> [[O]](n<sup>2</sup>) </big>   making it inefficient on large arrays.

Its primary purpose is for when writing data is very expensive (slow) when compared to reading, eg. writing to flash memory or EEPROM.

No other sorting algorithm has less data movement.


;Reference:
* Wikipedia:   [[wp:Selection_sort|Selection sort]]





## 360 Assembly

{{trans|PL/I}}
The program uses ASM structured macros and two ASSIST macros to keep the code as short as possible.

```360asm
*        Selection sort            26/06/2016
SELECSRT CSECT
         USING  SELECSRT,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     RJ,1               j=1
         DO WHILE=(C,RJ,LE,N)      do j=1 to n
         LR     RK,RJ                k=j
         LR     R1,RJ                j
         SLA    R1,2                 .
         LA     R3,A-4(R1)           @a(j)
         L      RT,0(R3)             temp=a(j)
         LA     RI,1(RJ)             i=j+1
         DO WHILE=(C,RI,LE,N)        do i=j+1 to n
         LR     R1,RI                  i
         SLA    R1,2                   .
         L      R2,A-4(R1)             a(i)
         IF CR,RT,GT,R2 THEN           if temp>a(i) then
         LR     RT,R2                    temp=a(i)
         LR     RK,RI                    k=i
         ENDIF  ,                      end if
         LA     RI,1(RI)               i=i+1
         ENDDO  ,                    end do
		 L      R0,0(R3)             a(j)
         LR     R1,RK                k
         SLA    R1,2                 .
         ST     R0,A-4(R1)           a(k)=a(j)
         ST     RT,0(R3)             a(j)=temp;
         LA     RJ,1(RJ)             j=j+1
         ENDDO  ,                  end do
         LA     R3,PG              pgi=0
         LA     RI,1               i=1
         DO     WHILE=(C,RI,LE,N)  do i=1 to n
         LR     R1,RI                i
         SLA    R1,2                 .
         L      R2,A-4(R1)           a(i)
         XDECO  R2,XDEC              edit a(i)
         MVC    0(4,R3),XDEC+8       output a(i)
         LA     R3,4(R3)             pgi=pgi+4
         LA     RI,1(RI)             i=i+1
         ENDDO  ,                  end do
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
A     DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
      DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
N        DC     A((N-A)/L'A)       number of items of a
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp for xdeco
         YREGS
RI       EQU    6                  i
RJ       EQU    7                  j
RK       EQU    8                  k
RT       EQU    9                  temp
         END    SELECSRT
```

{{out}}

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



## ActionScript


```ActionScript
function selectionSort(input: Array):Array {
	//find the i'th element
	for (var i:uint = 0; i < input.length; i++) {
		//set minIndex to an arbitrary value
		var minIndex:uint=i;
		//find the smallest number
		for (var j:uint = i; j < input.length; j++) {
			if (input[j]<input[minIndex]) {
				minIndex=j;
			}
		}
		//swap the smallest number into place
		var tmp:Number=input[i];
		input[i]=input[minIndex];
		input[minIndex]=tmp;
	}
	return input;
}
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Selection_Sort is

   type Integer_Array is array (Positive range <>) of Integer;
   procedure Sort (A : in out Integer_Array) is
      Min  : Positive;
      Temp : Integer;
   begin
      for I in A'First..A'Last - 1 loop
         Min := I;
         for J in I + 1..A'Last loop
            if A (Min) > A (J) then
               Min := J;
            end if;
         end loop;
         if Min /= I then
            Temp    := A (I);
            A (I)   := A (Min);
            A (Min) := Temp;
         end if;
      end loop;
   end Sort;

   A : Integer_Array := (4, 9, 3, -2, 0, 7, -5, 1, 6, 8);
begin
   Sort (A);
   for I in A'Range loop
      Put (Integer'Image (A (I)) & " ");
   end loop;
end Test_Selection_Sort;
```

{{out}}

```txt

-5 -2  0  1  3  4  6  7  8  9

```



## ALGOL 68

{{trans|Ada}}

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
MODE DATA = REF CHAR;

PROC in place selection sort = (REF[]DATA a)VOID:
BEGIN
   INT min;
   DATA temp;
   FOR i FROM LWB a TO UPB a DO
      min := i;
      FOR j FROM i + 1 TO UPB a DO
         IF a [min] > a [j] THEN
            min := j
         FI
      OD;
      IF min /= i THEN
         temp    := a [i];
         a [i]   := a [min];
         a [min] := temp
      FI
   OD
END # in place selection sort #;

[32]CHAR data := "big fjords vex quick waltz nymph";
[UPB data]DATA ref data;  FOR i TO UPB data DO ref data[i] := data[i] OD;
in place selection sort(ref data);
FOR i TO UPB ref data DO print(ref data[i]) OD; print(new line);
print((data))
```

{{out}}

```txt

     abcdefghiijklmnopqrstuvwxyz
big fjords vex quick waltz nymph

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program selectionSort.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessSortOk:       .asciz "Table sorted.\n"
szMessSortNok:      .asciz "Table not sorted !!!!!.\n"
sMessResult:        .ascii "Value  : "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:  .asciz "\n"

.align 4
iGraine:  .int 123456
.equ NBELEMENTS,      10
#TableNumber:      .int   1,3,6,2,5,9,10,8,4,7
TableNumber:     .int   10,9,8,7,6,5,4,3,2,1
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                              @ entry of program

1:
    ldr r0,iAdrTableNumber                         @ address number table
    mov r1,#0
    mov r2,#NBELEMENTS                             @ number of élements
    bl selectionSort
    ldr r0,iAdrTableNumber                         @ address number table
    bl displayTable

    ldr r0,iAdrTableNumber                         @ address number table
    mov r1,#NBELEMENTS                             @ number of élements
    bl isSorted                                    @ control sort
    cmp r0,#1                                      @ sorted ?
    beq 2f
    ldr r0,iAdrszMessSortNok                       @ no !! error sort
    bl affichageMess
    b 100f
2:                                                 @ yes
    ldr r0,iAdrszMessSortOk
    bl affichageMess
100:                                               @ standard end of the program
    mov r0, #0                                     @ return code
    mov r7, #EXIT                                  @ request to exit program
    svc #0                                         @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrTableNumber:          .int TableNumber
iAdrszMessSortOk:         .int szMessSortOk
iAdrszMessSortNok:        .int szMessSortNok
/******************************************************************/
/*     control sorted table                                   */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the number of elements  > 0  */
/* r0 return 0  if not sorted   1  if sorted */
isSorted:
    push {r2-r4,lr}                                    @ save registers
    mov r2,#0
    ldr r4,[r0,r2,lsl #2]
1:
    add r2,#1
    cmp r2,r1
    movge r0,#1
    bge 100f
    ldr r3,[r0,r2, lsl #2]
    cmp r3,r4
    movlt r0,#0
    blt 100f
    mov r4,r3
    b 1b
100:
    pop {r2-r4,lr}
    bx lr                                              @ return
/******************************************************************/
/*         selection sort                                              */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the first element    */
/* r2 contains the number of element */
selectionSort:
    push {r1-r7,lr}                                        @ save registers
    mov r3,r1                                              @ start index i
    sub r7,r2,#1                                           @ compute n - 1
1:                                                         @ start loop
    mov r4,r3
    add r5,r3,#1                                           @ init index 2
2:
    ldr r1,[r0,r4,lsl #2]                                  @ load value A[mini]
    ldr r6,[r0,r5,lsl #2]                                  @ load value A[j]
    cmp r6,r1                                              @ compare value
    movlt r4,r5                                            @ j -> mini
    add r5,#1                                              @ increment index j
    cmp r5,r2                                              @ end ?
    blt 2b                                                 @ no -> loop
    cmp r4,r3                                              @ mini <> j ?
    beq 3f                                                 @ no
    ldr r1,[r0,r4,lsl #2]                                  @ yes swap A[i] A[mini]
    ldr r6,[r0,r3,lsl #2]
    str r1,[r0,r3,lsl #2]
    str r6,[r0,r4,lsl #2]
3:
    add r3,#1                                              @ increment i
    cmp r3,r7                                              @ end ?
    blt 1b                                                 @ no -> loop

100:
    pop {r1-r7,lr}
    bx lr                                                  @ return

/******************************************************************/
/*      Display table elements                                */
/******************************************************************/
/* r0 contains the address of table */
displayTable:
    push {r0-r3,lr}                                    @ save registers
    mov r2,r0                                          @ table address
    mov r3,#0
1:                                                     @ loop display table
    ldr r0,[r2,r3,lsl #2]
    ldr r1,iAdrsMessValeur                             @ display value
    bl conversion10                                    @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                                   @ display message
    add r3,#1
    cmp r3,#NBELEMENTS - 1
    ble 1b
    ldr r0,iAdrszCarriageReturn
    bl affichageMess
100:
    pop {r0-r3,lr}
    bx lr
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:	                                            @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b	                                    @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                      @ and move spaces in end on area
    mov r0,r4                                         @ result length
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                    @ restaur registres
    bx lr                                             @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD



```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/topic44657-105.html discussion]

```AutoHotkey
MsgBox % SelecSort("")
MsgBox % SelecSort("xxx")
MsgBox % SelecSort("3,2,1")
MsgBox % SelecSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")

SelecSort(var) {                         ; SORT COMMA SEPARATED LIST
   StringSplit a, var, `,                ; make array, size = a0

   Loop % a0-1 {
      i := A_Index, mn := a%i%, j := m := i
      Loop % a0-i {                      ; find minimum
          j++
          If (a%j% < mn)
             mn := a%j%, m := j
      }
      t := a%i%, a%i% := a%m%, a%m% := t ; swap first with minimum
   }
   Loop % a0                             ; construct string from sorted array
      sorted .= "," . a%A_Index%
   Return SubStr(sorted,2)               ; drop leading comma
}
```



## AWK


```awk
function getminindex(gl, gi, gs)
{
  min = gl[gi]
  gm = gi
  for(gj=gi; gj <= gs; gj++) {
    if ( gl[gj] < min ) {
      min = gl[gj]
      gm = gj
    }
  }
  return gm
}

{
  line[NR] = $0
}
END { # sort it with selection sort
  for(i=1; i <= NR; i++) {
    mi = getminindex(line, i, NR)
    t = line[i]
    line[i] = line[mi];
    line[mi] = t
  }
  #print it
  for(i=1; i <= NR; i++) {
    print line[i]
  }
}
```



## BBC BASIC


```BBCBASIC
DEF PROC_SelectionSort(Size%)
FOR I% = 1 TO Size%-1
   lowest% = I%
   FOR J% = (I% + 1) TO Size%
      IF data%(J%) < data%(lowest%) lowest% = J%
   NEXT J%
   IF I%<>lowest% SWAP data%(I%),data%(lowest%)
NEXT I%
ENDPROC
```



## C



```c
#include <stdio.h>

void selection_sort (int *a, int n) {
    int i, j, m, t;
    for (i = 0; i < n; i++) {
        for (j = i, m = i; j < n; j++) {
            if (a[j] < a[m]) {
                m = j;
            }
        }
        t = a[i];
        a[i] = a[m];
        a[m] = t;
    }
}

int main () {
    int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
    int n = sizeof a / sizeof a[0];
    int i;
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    selection_sort(a, n);
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
 g++ -std=c++11 selection.cpp

```cpp
#include <algorithm>
#include <iterator>
#include <iostream>

template<typename ForwardIterator> void selection_sort(ForwardIterator begin,
                                                       ForwardIterator end) {
  for(auto i = begin; i != end; ++i) {
    std::iter_swap(i, std::min_element(i, end));
  }
}

int main() {
  int a[] = {100, 2, 56, 200, -52, 3, 99, 33, 177, -199};
  selection_sort(std::begin(a), std::end(a));
  copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
  std::cout << "\n";
}
```

{{out}}

```txt

-199 -52 2 3 33 56 99 100 177 200

```


=={{header|C sharp|C#}}==
This is a generic implementation that works with any type that implements the IComparable interface


```csharp>class SelectionSort<T
 where T : IComparable {
    public T[] Sort(T[] list) {
        int k;
        T temp;

        for (int i = 0; i < list.Length; i++) {
            k = i;
            for (int j=i + 1; j < list.Length; j++) {
                if (list[j].CompareTo(list[k]) < 0) {
                    k = j;
                }
            }
            temp = list[i];
            list[i] = list[k];
            list[k] = temp;
        }

        return list;
    }
}
```


Example of usage:

```csharp
String[] str = { "this", "is", "a", "test", "of", "generic", "selection", "sort" };

SelectionSort<String> mySort = new SelectionSort<string>();

String[] result = mySort.Sort(str);

for (int i = 0; i < result.Length; i++) {
    Console.WriteLine(result[i]);
}
```


{{out}}

```txt
a
generic
is
of
selection
sort
test
this
```



## Clojure

This is an implementation that mutates a Java arraylist in place.


```lisp
(import 'java.util.ArrayList)

(defn arr-swap! [#^ArrayList arr i j]
  (let [t (.get arr i)]
    (doto arr
      (.set i (.get arr j))
      (.set j t))))

(defn sel-sort!
  ([arr] (sel-sort! compare arr))
  ([cmp #^ArrayList arr]
     (let [n (.size arr)]
       (letfn [(move-min!
		[start-i]
		(loop [i start-i]
		  (when (< i n)
		    (when (< (cmp (.get arr i) (.get arr start-i)) 0)
		      (arr-swap! arr start-i i))
		    (recur (inc i)))))]
	 (doseq [start-i (range (dec n))]
	   (move-min! start-i))
	 arr))))
```



## COBOL


```COBOL
           PERFORM E-SELECTION VARYING WB-IX-1 FROM 1 BY 1
                               UNTIL WB-IX-1 = WC-SIZE.

...

       E-SELECTION SECTION.
       E-000.
           SET WC-LOWEST   TO WB-IX-1.
           ADD 1 WC-LOWEST GIVING WC-START

           PERFORM F-PASS VARYING WB-IX-2 FROM WC-START BY 1
                          UNTIL WB-IX-2 > WC-SIZE.

           IF WB-IX-1 NOT = WC-LOWEST
              MOVE WB-ENTRY(WC-LOWEST) TO WC-TEMP
              MOVE WB-ENTRY(WB-IX-1)   TO WB-ENTRY(WC-LOWEST)
              MOVE WC-TEMP             TO WB-ENTRY(WB-IX-1).

       E-999.
           EXIT.

       F-PASS SECTION.
       F-000.
           IF WB-ENTRY(WB-IX-2) < WB-ENTRY(WC-LOWEST)
              SET WC-LOWEST TO WB-IX-2.

       F-999.
           EXIT.
```



## Common Lisp



```lisp
(defun selection-sort-vector (array predicate)
  (do ((length (length array))
       (i 0 (1+ i)))
      ((eql i length) array)
    (do ((mindex i)
         (min (aref array i))
         (j i (1+ j)))
        ((eql j length)
         (rotatef (aref array i) (aref array mindex)))
      (when (funcall predicate (aref array j) min)
        (setf min (aref array j)
              mindex j)))))

(defun selection-sort-list (list predicate)
  (flet ((min-first (list)
           (do ((before-min nil)
                (min (first list))
                (prev list (rest prev))
                (curr (rest list) (rest curr)))
               ((endp curr)
                (if (null before-min) list
                  (let ((min (cdr before-min)))
                    (rplacd before-min (cdr min))
                    (rplacd min list)
                    min)))
             (when (funcall predicate (first curr) min)
               (setf before-min prev
                     min (first curr))))))
    (let ((result (min-first list)))
      (do ((head result (rest head)))
          ((endp (rest head)) result)
        (rplacd head (min-first (rest head)))))))

(defun selection-sort (sequence predicate)
  (etypecase sequence
    (list (selection-sort-list sequence predicate))
    (vector (selection-sort-vector sequence predicate))))
```


Example use:


```txt
> (selection-sort (list 8 7 4 3 2 0 9 1 5 6) '<)
(0 1 2 3 4 5 6 7 8 9)

> (selection-sort (vector 8 7 4 3 2 0 9 1 5 6) '>)
#(9 8 7 6 5 4 3 2 1 0)
```




## Crystal

This sorts the array in-place.

```crystal
def selectionSort(array : Array)
    (0...array.size-1).each do |i|
        nextMinIndex = i
        (i+1...array.size).each do |j|
            if array[j] < array[nextMinIndex]
                nextMinIndex = j
            end
        end
        if i != nextMinIndex
            array.swap(i, nextMinIndex)
        end
    end
end
```



## D

The actual function is very short.

```d
import std.stdio, std.algorithm, std.array, std.traits;

enum AreSortableArrayItems(T) = isMutable!T &&
                                __traits(compiles, T.init < T.init) &&
                                !isNarrowString!(T[]);

void selectionSort(T)(T[] data) if (AreSortableArrayItems!T) {
    foreach (immutable i, ref d; data)
        data.drop(i).minPos[0].swap(d);
} unittest {
    int[] a0;
    a0.selectionSort;

    auto a1 = [1];
    a1.selectionSort;
    assert(a1 == [1]);

    auto a2 = ["a", "b"];
    a2.selectionSort;
    assert(a2 == ["a", "b"]);

    auto a3 = ["b", "a"];
    a3.selectionSort;
    assert(a3 == ["a", "b"]);

    auto a4 = ['a', 'b'];
    static assert(!__traits(compiles, a4.selectionSort));

    dchar[] a5 = ['b', 'a'];
    a5.selectionSort;
    assert(a5 == "ab"d);

    import std.typecons;
    alias Nullable!int N;
    auto a6 = [N(2), N(1)];
    a6.selectionSort; // Not nothrow.
    assert(a6 == [N(1), N(2)]);

    auto a7 = [1.0+0i, 2.0+0i]; // To be deprecated.
    static assert(!__traits(compiles, a7.selectionSort));

    import std.complex;
    auto a8 = [complex(1), complex(2)];
    static assert(!__traits(compiles, a8.selectionSort));

    static struct F {
        int x;
        int opCmp(F f) { // Not pure.
            return x < f.x ? -1 : (x > f.x ? 1 : 0);
        }
    }
    auto a9 = [F(2), F(1)];
    a9.selectionSort;
    assert(a9 == [F(1), F(2)]);
}

void main() {
    auto a = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2];
    a.selectionSort;
    a.writeln;
}
```

{{out}}

```txt
[1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 5, 6, 7, 8, 9, 9, 9]
```



## Delphi



### Array sort

Dynamic array is a 0-based array of variable length

Static array is an arbitrary-based array of fixed length

```Delphi
program TestSelectionSort;

{$APPTYPE CONSOLE}

{.$DEFINE DYNARRAY}  // remove '.' to compile with dynamic array

type
  TItem = Integer;   // declare ordinal type for array item
{$IFDEF DYNARRAY}
  TArray = array of TItem;          // dynamic array
{$ELSE}
  TArray = array[0..15] of TItem;   // static array
{$ENDIF}

procedure SelectionSort(var A: TArray);
var
  Item: TItem;
  I, J, M: Integer;

begin
  for I:= Low(A) to High(A) - 1 do begin
    M:= I;
    for J:= I + 1 to High(A) do
      if A[J] < A[M] then M:= J;
    Item:= A[M];
    A[M]:= A[I];
    A[I]:= Item;
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
  SelectionSort(A);
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



### String sort

// string is 1-based variable-length array of Char

```Delphi
procedure SelectionSort(var S: string);
var
  Lowest: Char;
  I, J, M, L: Integer;

begin
  L:= Length(S);
  for I:= 1 to L - 1 do begin
    M:= I;
    for J:= I + 1 to L do
      if S[J] < S[M] then M:= J;
    Lowest:= S[M];
    S[M]:= S[I];
    S[I]:= Lowest;
  end;
end;
```


```txt

// in : S = 'the quick brown fox jumps over the lazy dog'
// out: S = '        abcdeeefghhijklmnoooopqrrsttuuvwxyz'

```



## E



```e
def selectionSort := {
  def cswap(c, a, b) {
    def t := c[a]
    c[a]  := c[b]
    c[b]  := t
    println(c)
  }

  def indexOfMin(array, first, last) {
    var min := array[first]
    var mini := first
    for i in (first+1)..last {
      if (array[i] < min) {
        min := array[i]
        mini := i
      }
    }
    return mini
  }

  /** Selection sort (in-place). */
  def selectionSort(array) {
    def last := (array.size()-1)
    for i in 0..(last - 1) {
      cswap(array, i, indexOfMin(array, i + 1, last))
    }
  }
}
```


## EasyLang


<lang>subr sort
  for i = 0 to len data[] - 2
    min_pos = i
    for j = i + 1 to len data[] - 1
      if data[j] < data[min_pos]
        min_pos = j
      .
    .
    swap data[i] data[min_pos]
  .
.
data[] = [ 29 4 72 44 55 26 27 77 92 5 ]
call sort
print data[]
```



## EchoLisp


### List sort


```scheme

;; recursive version (adapted from Racket)
(lib 'list) ;; list-delete
(define (sel-sort xs  (x0))
	(cond
		[(null? xs) null]
		[else (set! x0 (apply min xs))
			  (cons x0 (sel-sort (list-delete xs x0)))]))

(sel-sort (shuffle (iota 13)))
    → (0 1 2 3 4 5 6 7 8 9 10 11 12)

;; straightforward and more efficient implementation using list-swap!
(define (sel-sort list)
		(maplist (lambda( L)
			(first (list-swap! L (first L) (apply min L )))) list))

(sel-sort (shuffle (iota 13)))
    → (0 1 2 3 4 5 6 7 8 9 10 11 12)


```


### Array sort


```scheme

;; sort an array in place
(define (sel-sort a  (amin) (imin))
	(define ilast (1- (vector-length a)))
	(for [(i ilast)]
		(set! amin [a (setv! imin i)]) ;; imin := i , amin := a[imin]
		(for [(j (in-range (1+ i) (1+ ilast)))]
			(when (< [a j] amin) (set! amin [a (setv! imin j)])))
		(vector-swap! a i imin))
		a )

(define a #(9 8 2 6 3 5 4))
(sel-sort a)
    → #( 2 3 4 5 6 8 9)

```




## Eiffel



```Eiffel

class
	SELECTION_SORT [G -> COMPARABLE]

feature {NONE}

	index_of_min (ar: ARRAY [G]; lower: INTEGER): INTEGER
			--Index of smallest element in 'ar' in the range of lower and the max index.
		require
			lower_positiv: lower >= 1
			lower_in_range: lower <= ar.count
			ar_not_void: ar /= Void
		local
			i: INTEGER
			min: G
		do
			from
				i := lower
				min := ar.item (i)
				Result := i
			until
				i + 1 > ar.count
			loop
				if ar.item (i + 1) < min then
					min := ar.item (i + 1)
					Result := i + 1
				end
				i := i + 1
			end
		ensure
			result_is_set: Result /= Void
		end

	sort (ar: ARRAY [G]): ARRAY [G]
			-- sort array ar with selectionsort
		require
			ar_not_void: ar /= Void
		local
			min_index: INTEGER
			ith: G
		do
			create Result.make_empty
			Result.deep_copy (ar)
			across
				Result as ic
			loop
				min_index := index_of_min (Result, ic.cursor_index)
				ith := Result [ic.cursor_index]
				Result [ic.cursor_index] := Result [min_index]
				Result [min_index] := ith
			end
		ensure
			Result_is_set: Result /= Void
			Result_sorted: is_sorted (Result) = True
		end

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

feature

	selectionsort (ar: ARRAY [G]): ARRAY [G]
		do
			Result := sort (ar)
		end

end

```

Test:

```eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			test := <<1, 27, 32, 99, 1, -7, 3, 5, 7>>
			io.put_string ("Unsorted: ")
			across
				test as ic
			loop
				io.put_string (ic.item.out + " ")
			end
			create selectionsort
			io.put_string ("%NSorted: ")
			test := selectionsort.selectionsort (test)
			across
				test as ar
			loop
				io.put_string (ar.item.out + " ")
			end
		end

	test: ARRAY [INTEGER]

	selectionsort: SELECTION_SORT [INTEGER]

end

```

{{out}}

```txt

Unsorted: 1 27 32 99 1 -7 3 5 7
Sorted: -7 1 1 3 5 7 27 32 99

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;

extension op
{
    selectionSort()
    {
        var copy := self.clone();

        for(int i := 0, i < copy.Length, i += 1)
        {
            int k := i;
            for(int j := i + 1, j < copy.Length, j += 1)
            {
                if (copy[j] < copy[k])
                {
                    k := j
                }
            };
            copy.exchange(i,k)
        };

        ^ copy
    }
}

public program()
{
    var list := new string[]::("this", "is", "a", "test", "of", "generic", "selection", "sort");

    console.printLine("before:",list.asEnumerable());
    console.printLine("after:",list.selectionSort().asEnumerable())
}
```

{{out}}

```txt

before:this,is,a,test,of,generic,selection,sort
after:a,generic,is,of,selection,sort,test,this

```



## Elixir


```elixir
defmodule Sort do
  def selection_sort(list) when is_list(list), do: selection_sort(list, [])

  defp selection_sort([], sorted), do: sorted
  defp selection_sort(list, sorted) do
    max = Enum.max(list)
    selection_sort(List.delete(list, max), [max | sorted])
  end
end
```


Example:

```txt

iex(10)> Sort.selection_sort([5,3,9,4,1,6,8,2,7])
[1, 2, 3, 4, 5, 6, 7, 8, 9]

```


## Erlang


```Erlang

-module(solution).
-import(lists,[delete/2,max/1]).
-compile(export_all).
selection_sort([],Sort)-> Sort;
selection_sort(Ar,Sort)->
	M=max(Ar),
	Ad=delete(M,Ar),
	selection_sort(Ad,[M|Sort]).
print_array([])->ok;
print_array([H|T])->
	io:format("~p~n",[H]),
	print_array(T).

main()->
	Ans=selection_sort([1,5,7,8,4,10],[]),
	print_array(Ans).

```




## Euphoria


```euphoria
function selection_sort(sequence s)
    object tmp
    integer m
    for i = 1 to length(s) do
        m = i
        for j = i+1 to length(s) do
            if compare(s[j],s[m]) < 0 then
                m = j
            end if
        end for
        tmp = s[i]
        s[i] = s[m]
        s[m] = tmp
    end for
    return s
end function

include misc.e
constant s = {4, 15, "delta", 2, -31, 0, "alfa", 19, "gamma", 2, 13, "beta", 782, 1}

puts(1,"Before: ")
pretty_print(1,s,{2})
puts(1,"\nAfter: ")
pretty_print(1,selection_sort(s),{2})
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


=={{header|F Sharp|F#}}==

```fsharp

let rec ssort = function
    [] -> []
    | x::xs ->
        let min, rest =
            List.fold (fun (min,acc) x ->
                             if h<min then (h, min::acc)
                             else (min, h::acc))
              (x, []) xs
        in min::ssort rest

```



## Factor


```factor
USING: kernel math sequences sequences.extras ;

: select ( m n seq -- )
    [ dup ] 2dip [ <slice> [ ] infimum-by* drop over + ]
    [ exchange ] bi ;

: selection-sort! ( seq -- seq' )
    [ ] [ length dup ] [ ] tri [ select ] 2curry each-integer ;
```

Example use

```factor
IN: scratchpad { 5 -6 3 9 -2 4 -1 -6 5 -5 } selection-sort!

--- Data stack:
{ -6 -6 -5 -2 -1 3 4 5 5 9 }
```



## Forth


```forth
defer less?   ' < is less?

: least ( start end -- least )
  over cell+ do
    i @ over @ less? if drop i then
  cell +loop ;
: selection ( array len -- )
  cells over + tuck ( end start end )
  cell- swap do   ( end )
    i over least ( end least )
    i @ over @ i ! swap !
  cell +loop drop ;

create array 8 , 1 , 4 , 2 , 10 , 3 , 7 , 9 , 6 , 5 ,

array 10 selection
array 10 cells dump
```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
PROGRAM SELECTION

  IMPLICIT NONE

  INTEGER :: intArray(10) = (/ 4, 9, 3, -2, 0, 7, -5, 1, 6, 8 /)

  WRITE(*,"(A,10I5)") "Unsorted array:", intArray
  CALL Selection_sort(intArray)
  WRITE(*,"(A,10I5)") "Sorted array  :", intArray

CONTAINS

  SUBROUTINE Selection_sort(a)
    INTEGER, INTENT(IN OUT) :: a(:)
    INTEGER :: i, minIndex, temp

    DO i = 1, SIZE(a)-1
       minIndex = MINLOC(a(i:), 1) + i - 1
       IF (a(i) > a(minIndex)) THEN
          temp = a(i)
          a(i) = a(minIndex)
          a(minIndex) = temp
       END IF
    END DO
  END SUBROUTINE Selection_sort

END PROGRAM SELECTION
```

{{out}}

```txt

Unsorted array:    4    9    3   -2    0    7   -5    1    6    8
Sorted array  :   -5   -2    0    1    3    4    6    7    8    9

```


## FreeBASIC


```freebasic
' version 03-12-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

Sub selectionsort(arr() As Long)

    ' sort from lower bound to the highter bound
    ' array's can have subscript range from -2147483648 to +2147483647

    Dim As Long i, j, x
    Dim As Long lb = LBound(arr)
    Dim As Long ub = UBound(arr)

    For i = lb To ub -1
        x = i
        For j = i +1 To ub
            If arr(j) < arr(x) Then x = j
        Next
        If x <> i Then
            Swap arr(i), arr(x)
        End If
    Next

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
selectionsort(array())  ' sort the array
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
unsort    1  -7  -5  -4   6   5  -3   4   2   0   3  -6  -2   7  -1
  sort   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## Gambas


```gambas

siLow As Short = -99  'Set the lowest value number to create
siHigh As Short = 99  'Set the highest value number to create
siQty As Short = 20   'Set the quantity of numbers to create

Public Sub Main()
Dim siToSort As Short[] = CreateNumbersToSort()
Dim siPos, siLow, siChar, siCount As Short

PrintOut("To sort: ", siToSort)

For siCount = 0 To siToSort.Max
  siChar = siCount
  For siPos = siCount + 1 To siToSort.Max
    If siToSort[siChar] > siToSort[siPos] Then siChar = siPos
  Next
  siLow = siToSort[siChar]
  siToSort.Delete(siChar, 1)
  siToSort.Add(siLow, siCount)
Next

PrintOut(" Sorted: ", siToSort)

End
'---------------------------------------------------------
Public Sub PrintOut(sText As String, siToSort As String[])
Dim siCount As Short

Print sText;

For siCount = 0 To siToSort.Max
  Print siToSort[siCount];
  If siCount <> siToSort.max Then Print ", ";
Next

Print

End
'---------------------------------------------------------
Public Sub CreateNumbersToSort() As Short[]
Dim siCount As Short
Dim siList As New Short[]

For siCount = 0 To siQty
  siList.Add(Rand(siLow, siHigh))
Next

Return siList

End
```


Output:

```txt

To sort: -11, -64, -20, -84, 94, -60, -82, -82, 37, -30, -75, 73, 19, -97, 81, -26, 55, 8, -15, -31, 36
 Sorted: -97, -84, -82, -82, -75, -64, -60, -31, -30, -26, -20, -15, -11, 8, 19, 36, 37, 55, 73, 81, 94

```


## GAP


```gap
SelectionSort := function(v)
   local i, j, k, n, m;
   n := Size(v);
   for i in [1 .. n] do
      k := i;
      m := v[i];
      for j in [i + 1 .. n] do
         if v[j] < m then
            k := j;
            m := v[j];
         fi;
      od;
      v[k] := v[i];
      v[i] := m;
   od;
end;

v := List([1 .. 100], n -> Random([1 .. 100]));
SelectionSort(v);
v;
```



## Go


```go
package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}

func main() {
    fmt.Println("before:", a)
    selectionSort(a)
    fmt.Println("after: ", a)
}

func selectionSort(a []int) {
    last := len(a) - 1
    for i := 0; i < last; i++ {
        aMin := a[i]
        iMin := i
        for j := i + 1; j < len(a); j++ {
            if a[j] < aMin {
                aMin = a[j]
                iMin = j
            }
        }
        a[i], a[iMin] = aMin, a[i]
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

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}

func main() {
    fmt.Println("before:", a)
    selectionSort(sort.IntSlice(a))
    fmt.Println("after: ", a)
}

func selectionSort(a sort.Interface) {
    last := a.Len() - 1
    for i := 0; i < last; i++ {
        iMin := i
        for j := i + 1; j < a.Len(); j++ {
            if a.Less(j, iMin) {
                iMin = j
            }
        }
        a.Swap(i, iMin)
    }
}
```



## Haskell


```haskell
import Data.List (delete)

selSort :: (Ord a) => [a] -> [a]
selSort [] = []
selSort xs = selSort (delete x xs) ++ [x]
  where x = maximum xs
```



## Haxe


```haxe
static function selectionSort(arr:Array<Int>) {
	var len = arr.length;
	for (index in 0...len)
	{
		var minIndex = index;
		for (remainingIndex in (index+1)...len)
		{
			if (arr[minIndex] > arr[remainingIndex]) {
				minIndex = remainingIndex;
			}
		}
		if (index != minIndex) {
			var temp = arr[index];
			arr[index] = arr[minIndex];
			arr[minIndex] = temp;
		}
	}
}
```



## Io


```io
List do (
    selectionSortInPlace := method(
        size repeat(idx,
            swapIndices(idx, indexOf(slice(idx, size) min))
        )
    )
)

l := list(-1, 4, 2, -9)
l selectionSortInPlace println # ==> list(-9, -1, 2, 4)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   demosort(selectionsort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end


procedure selectionsort(X,op)                           #: return sorted list ascending(or descending)
local i,m

   op := sortop(op,X)                                   # select how and what we sort
   every i := 1 to *X-1 do {
      m := i
      every j := i + 1 to *X do
         if op(X[j],X[m]) then m := j                   # find X that belongs @i low (or high)
      X[m ~= i] :=: X[m]
      }
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]]. The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.

{{out}} Abbreviated sample

```txt
Sorting Demo using procedure selectionsort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "SelecSrt.bas"
110 RANDOMIZE
120 NUMERIC ARRAY(-5 TO 14)
130 CALL INIT(ARRAY)
140 CALL WRITE(ARRAY)
150 CALL SELECTIONSORT(ARRAY)
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
280 DEF SELECTIONSORT(REF A)
290   FOR I=LBOUND(A) TO UBOUND(A)-1
300     LET MN=A(I):LET INDEX=I
310     FOR J=I+1 TO UBOUND(A)
320       IF MN>A(J) THEN LET MN=A(J):LET INDEX=J
330     NEXT
340     LET A(INDEX)=A(I):LET A(I)=MN
350   NEXT
360 END DEF
```



## J

{{eff note|J|/:~}}
Create the following script and load it to a J session.

```j
selectionSort=: verb define
  data=. y
  for_xyz. y do.
    temp=. xyz_index }. data
    nvidx=. xyz_index + temp i. <./ temp
    data=. ((xyz_index, nvidx) { data) (nvidx, xyz_index) } data
  end.
  data
)
```


In an email discussion, Roger_Hui presented the following tacit code:

```j
ix=: C.~ <@~.@(0, (i. <./))
ss1=: ({. , $:@}.)@ix^:(*@#)
```


To validate:

```j
   [data=. 6 15 19 12 14 19 0 17 0 14
6 15 19 12 14 19 0 17 0 14
   selectionSort data
0 0 6 12 14 14 15 17 19 19
   ss1 data
0 0 6 12 14 14 15 17 19 19
```



## Java

This algorithm sorts in place. The call <tt>sort(array)</tt> will rearrange the array and not create a new one.

```java
public static void sort(int[] nums){
	for(int currentPlace = 0;currentPlace<nums.length-1;currentPlace++){
		int smallest = Integer.MAX_VALUE;
		int smallestAt = currentPlace+1;
		for(int check = currentPlace; check<nums.length;check++){
			if(nums[check]<smallest){
				smallestAt = check;
				smallest = nums[check];
			}
		}
		int temp = nums[currentPlace];
		nums[currentPlace] = nums[smallestAt];
		nums[smallestAt] = temp;
	}
}
```



## JavaScript

This algorithm sorts array of numbers.

```javascript
function selectionSort(nums) {
  var len = nums.length;
  for(var i = 0; i < len; i++) {
    var minAt = i;
    for(var j = i + 1; j < len; j++) {
      if(nums[j] < nums[minAt])
        minAt = j;
    }

    if(minAt != i) {
      var temp = nums[i];
      nums[i] = nums[minAt];
      nums[minAt] = temp;
    }
  }
  return nums;
}
```



## jq

The following implementation does not impose any restrictions on the types of entities that may appear in the array to be sorted.  That is, the array may include any collection of JSON entities.

The definition also illustrates the use of an inner function (swap), and the use of jq's reduction operator, <tt>reduce</tt>.
```jq
# Sort any array
def selection_sort:
  def swap(i;j): if i == j then . else .[i] as $tmp | .[i] = .[j] | .[j] = $tmp end;
  length as $length
  | reduce range(0; $length) as $currentPlace
      # state: $array
      ( .;
        . as $array
        | (reduce range( $currentPlace; $length) as $check
            # state: [ smallestAt, smallest] except initially [null]
            ( [$currentPlace+1] ;
               if length == 1 or $array[$check] < .[1]
               then [$check, $array[$check] ]
               else .
               end
             )) as $ans
          | swap( $currentPlace; $ans[0] )
          ) ;
```
Example:
```jq

[1, 3.3, null, 2, null, [1,{"a":1 }] ] | selection_sort

```

{{Out}}

```txt

[
  null,
  null,
  1,
  2,
  3.3,
  [
    1,
    {
      "a": 1
    }
  ]
]

```


## Julia

{{works with|Julia|0.6}}


```julia
function selectionsort!(arr::Vector{<:Real})
    len = length(arr)
    if len < 2 return arr end
    for i in 1:len-1
        lmin, j = findmin(arr[i+1:end])
        if lmin < arr[i]
            arr[i+j] = arr[i]
            arr[i] = lmin
        end
    end
    return arr
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", selectionsort!(v))
```


{{out}}

```txt
# unordered: [2, -10, 0, -10, -9, -3, -3, 7, 8, -3]
 -> ordered: [-10, -10, -9, -3, -3, -3, 0, 2, 7, 8]
```



## Kotlin

{{trans|C#}}

```scala>fun <T : Comparable<T>> Array<T
.selection_sort() {
    for (i in 0..size - 2) {
        var k = i
        for (j in i + 1..size - 1)
            if (this[j] < this[k])
                k = j

        if (k != i) {
            val tmp = this[i]
            this[i] = this[k]
            this[k] = tmp
        }
    }
}

fun main(args: Array<String>) {
    val i = arrayOf(4, 9, 3, -2, 0, 7, -5, 1, 6, 8)
    i.selection_sort()
    println(i.joinToString())

    val s = Array(i.size, { -i[it].toShort() })
    s.selection_sort()
    println(s.joinToString())

    val c = arrayOf('z', 'h', 'd', 'c', 'a')
    c.selection_sort()
    println(c.joinToString())
}
```

{{out}}

```txt
-5, -2, 0, 1, 3, 4, 6, 7, 8, 9
-9, -8, -7, -6, -4, -3, -1, 0, 2, 5
a, c, d, h, z
```



## Liberty BASIC


```lb
    itemCount = 20
    dim A(itemCount)
    for i = 1 to itemCount
        A(i) = int(rnd(1) * 100)
    next i

    print "Before Sort"
    gosub [printArray]

'--- Selection sort algorithm
    for i = 1 to itemCount-1
        jMin = i
        for j = i+1 to itemCount
            if A(j) < A(jMin) then jMin = j
        next
        tmp = A(i)
        A(i) = A(jMin)
        A(jMin) = tmp
    next
'--- end of (Selection sort algorithm)

    print "After Sort"
    gosub [printArray]
end

[printArray]
    for i = 1 to itemCount
        print using("###", A(i));
    next i
    print
return

```



## Lua


```lua
function SelectionSort( f )
    for k = 1, #f-1 do
        local idx = k
        for i = k+1, #f do
            if f[i] < f[idx] then
                idx = i
            end
        end
        f[k], f[idx] = f[idx], f[k]
    end
end


f = { 15, -3, 0, -1, 5, 4, 5, 20, -8 }

SelectionSort( f )

for i in next, f do
    print( f[i] )
end
```



## Maple


```Maple
arr:= Array([17,3,72,0,36,2,3,8,40,0]):
len := numelems(arr):
for i to len-1 do
	j_min := i:
	for j from i+1 to len do
		if arr[j] < arr[j_min] then
			j_min := j:
		end if:
	end do:
	if (not j_min = i) then
		temp := arr[i]:
		arr[i] := arr[j_min]:
		arr[j_min] := temp:
	end if:
end do:
arr;
```

{{Out|Output}}

```txt
[0,0,2,3,3,8,17,36,40,72]
```



## Mathematica

Procedural solution with custom min function:


```Mathematica
SelectSort[x_List] := Module[{n = 1, temp, xi = x, j},
  While[n <= Length@x,
   temp = xi[[n]];
   For[j = n, j <= Length@x, j++,
    If[xi[[j]] < temp, temp = xi[[j]]];
    ];
   xi[[n ;;]] = {temp}~Join~
     Delete[xi[[n ;;]], First@Position[xi[[n ;;]], temp] ];
   n++;
   ];
  xi
  ]
```


Recursive solution using a pre-existing Min[] function:


```Mathematica
SelectSort2[x_List]:= Flatten[{Min@x, If[Length@x > 1, SelectSort2@Drop[x, First@Position[x, Min@x]], {}] }];
```


Validate by testing the ordering of a random number of randomly-sized random lists:


```Mathematica
{And @@ Table[l = RandomInteger[150, RandomInteger[1000]];
   Through[And[Length@# == Length@SelectSort@# &, OrderedQ@SelectSort@# &]@l],
   {RandomInteger[150]}],
 Block[{$RecursionLimit = Infinity},
  And @@ Table[l = RandomInteger[150, RandomInteger[1000]];
    Through[And[Length@# == Length@SelectSort2@# &, OrderedQ@SelectSort2@# &]@l],
    {RandomInteger[150]}]
  ]}
```


Validation Result:

```txt
{True, True}
```


=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
function list = selectionSort(list)

    listSize = numel(list);

    for i = (1:listSize-1)

        minElem = list(i);
        minIndex = i;

        %This for loop can be vectorized, but there will be no significant
        %increase in sorting efficiency.
        for j = (i:listSize)
            if list(j) <= minElem
                minElem = list(j);
                minIndex = j;
            end
        end

        if i ~= minIndex
            list([minIndex i]) = list([i minIndex]); %Swap
        end

    end %for
end %selectionSort
```


Sample Usage:

```MATLAB>>
 selectionSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```



## Maxima


```maxima
selection_sort(v) := block([k, m, n],
n: length(v),
for i: 1 thru n do (
   k: i,
   m: v[i],
   for j: i + 1 thru n do
      if v[j] < m then (k: j, m: v[j]),
   v[k]: v[i],
   v[i]: m
))$

v: makelist(random(199) - 99, i, 1, 10);    /* [52, -85, 41, -70, -59, 88, 19, 80, 90, 44] */
selection_sort(v)$
v;                                          /* [-85, -70, -59, 19, 41, 44, 52, 80, 88, 90] */
```



## MAXScript


```maxscript
fn selectionSort arr =
(
    local min = undefined
    for i in 1 to arr.count do
    (
        min = i
        for j in i+1 to arr.count do
        (
            if arr[j] < arr[min] then
            (
                min = j
            )
        )
        swap arr[i] arr[min]
    )
    arr
)

data = selectionSort #(4, 9, 3, -2, 0, 7, -5, 1, 6, 8)
print data
```



## N/t/roff


```N/t/roff
.de end
..
.de array
.	nr \\$1.c 0 1
.	de \\$1.push end
.		nr \\$1..\\\\n+[\\$1.c] \\\\$1
.	end
.	de \\$1.pushln end
.		if \\\\n(.$>0 .\\$1.push \\\\$1
.		if \\\\n(.$>1 \{ \
.			shift
.			\\$1.pushln \\\\$@
.		\}
.	end
.	de \\$1.dump end
.		nr i 0 1
.		while \\\\n+i<=\\\\n[\\$1.c] .tm \\\\n[\\$1..\\\\ni]
.		rr i
.	end
.	de \\$1.swap end
.		if (\\\\$1<=\\\\n[\\$1.c])&(\\\\$2<=\\\\n[\\$1.c]) \{ \
.			nr b \\\\n[\\$1..\\\\$1]
.			nr \\$1..\\\\$1 \\\\n[\\$1..\\\\$2]
.			nr \\$1..\\\\$2 \\\\nb
.			rr b
.		\}
.	end
..
.array myArray
.myArray.pushln 14 62 483 21 12 11 0 589 212 10 5 4 95 4 2 2 12 0 0
.de sort
.	nr i 0 1
.	while \\n+i<=\\n[\\$1.c] \{ \
.		nr j \\ni 1
.		nr st \\nj
.		while \\n+j<=\\n[\\$1.c] \{ \
.			if \\n[\\$1..\\nj]<\\n[\\$1..\\n(st] .nr st \\nj
.		\}
.		if !\\n(st=\\ni .\\$1.swap \\ni \\n(st
.	\}
..
.sort myArray
.myArray.dump
```



### Output

<lang>0
0
0
2
2
4
4
5
10
11
12
12
14
21
62
95
212
483
589
```



## Nemerle

{{trans|C#}}

```Nemerle
using System;
using System.Console;

module Selection
{
    public static Sort[T](this a : array[T]) : void
      where T : IComparable
    {
        mutable k = 0;
        def lastindex = a.Length - 1;

        foreach (i in [0 .. lastindex])
        {
            k = i;
            foreach (j in [i .. lastindex])
                when (a[j].CompareTo(a[k]) < 0) k = j;
            a[i] <-> a[k];
        }
    }

    Main() : void
    {
        def arr = array[6, 2, 8, 3, 9, 4, 7, 3, 9, 1];
        arr.Sort();
        foreach (i in arr) Write($"$i  ");
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

import java.util.List

placesList = [String -
    "UK  London",     "US  New York",   "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"     -
]

lists = [ -
    placesList -
  , selectionSort(String[] Arrays.copyOf(placesList, placesList.length)) -
]

loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method selectionSort(a = String[]) public constant binary returns String[]

  rl = String[a.length]
  al = List selectionSort(Arrays.asList(a))
  al.toArray(rl)

  return rl

method selectionSort(a = List) public constant binary returns ArrayList

  ra = ArrayList(a)
  n  = ra.size

  iPos = int
  iMin = int

  loop iPos = 0 to n - 1
    iMin = iPos
    loop i_ = iPos + 1 to n - 1
      if (Comparable ra.get(i_)).compareTo(Comparable ra.get(iMin)) < 0 then do
        iMin = i_
        end
      end i_
    if iMin \= iPos then do
      swap = ra.get(iPos)
      ra.set(iPos, ra.get(iMin))
      ra.set(iMin, swap)
      end
    end iPos

  return ra

```

{{out}}

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
proc selectionSort[T](a: var openarray[T]) =
  let n = a.len
  for i in 0 .. <n:
    var m = i
    for j in i .. <n:
      if a[j] < a[m]:
        m = j
    swap a[i], a[m]

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
selectionSort a
echo a
```

{{out}}

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## OCaml


```ocaml
let rec selection_sort = function
    [] -> []
  | first::lst ->
      let rec select_r small output = function
          [] -> small :: selection_sort output
        | x::xs when x < small -> select_r x (small::output) xs
        | x::xs                -> select_r small (x::output) xs
      in
      select_r first [] lst
```



## Oforth



```Oforth
: selectSort(l)
| b j i k s |
   l size ->s
   l asListBuffer ->b

   s loop: i [
      i dup ->k b at
      i 1 + s for: j [ b at(j) 2dup <= ifTrue: [ drop ] else: [ nip j ->k ] ]
      k i b at b put i swap b put
      ]
   b dup freeze ;
```



## ooRexx



```oorexx
/*REXX ****************************************************************
* program sorts an  array  using  the  selection-sort  method.
* derived from REXX solution
* Note that ooRexx can process Elements of the stem argument (Use Arg)
* 06.10.2010 Walter Pachl
**********************************************************************/
call generate                       /*generate the array elements.   */
call show 'before sort'             /*show the before array elements,*/
call selectionSort x.               /*invoke the selection sort.     */
call show 'after sort'              /*show the  after array elements.*/
exit                                /*stick a fork in it, we're done.*/

selectionSort: Procedure
  Use Arg s.                        /* gain access to the argument   */
  do j=1 To s.0-1
    t=s.j;
    p=j;
    do k=j+1 to s.0
      if s.k<t then do;
        t=s.k;
        p=k;
        end
      end
    if p=j then
      iterate
    t=s.j;
    s.j=s.p;
    s.p=t
    end
  return

show:
  Parse Arg heading
  Say heading
  Do i=1 To x.0
    Say i'  'x.i
    End
  say copies('-',79)
  Return
return

generate:
  x.1='---The seven hills of Rome:---'
  x.2='
### ========================
'
  x.3='Caelian'
  x.4='Palatine'
  x.5='Capitoline'
  x.6='Virminal'
  x.7='Esquiline'
  x.8='Quirinal'
  x.9='Aventine'
  x.0=9
  return
```



## Oz

Although lists are much more used in Oz than arrays, this algorithm seems more natural for arrays.

```oz
declare
  proc {SelectionSort Arr}
     proc {Swap K L}
        Arr.K := (Arr.L := Arr.K)
     end
     Low = {Array.low Arr}
     High = {Array.high Arr}
  in
     %% for every index I of the array
     for I in Low..High do
	%% find the index of the minimum element
	%% with an index >= I
	Min = {NewCell Arr.I}
        MinIndex = {NewCell I}
     in
        for J in I..High do
  	 if Arr.J < @Min then
	    Min := Arr.J
	    MinIndex := J
  	 end
	end
	%% and put that minimum element to the left
	{Swap @MinIndex I}
     end
  end

  A = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {SelectionSort A}
  {Show {Array.toRecord unit A}}
```



## PARI/GP


```parigp
selectionSort(v)={
  for(i=1,#v-1,
    my(mn=i,t);
    for(j=i+1,#v,
      if(v[j]<v[mn],mn=j)
    );
    t=v[mn];
    v[mn]=v[i];
    v[i]=t
  );
  v
};
```



## Pascal

See [[Sorting_algorithms/Selection_sort#Delphi | Delphi]]


## Perl

{{trans|Tcl}}

```perl
sub selection_sort
  {my @a = @_;
   foreach my $i (0 .. $#a - 1)
      {my $min = $i + 1;
       $a[$_] < $a[$min] and $min = $_ foreach $min .. $#a;
       $a[$i] > $a[$min] and @a[$i, $min] = @a[$min, $i];}
   return @a;}
```



## Perl 6

Solution 1:

```perl6
sub selection_sort ( @a is copy ) {
    for 0 ..^ @a.end -> $i {
        my $min = [ $i+1 .. @a.end ].min: { @a[$_] };
        @a[$i, $min] = @a[$min, $i] if @a[$i] > @a[$min];
    }
    return @a;
}

my @data = 22, 7, 2, -5, 8, 4;
say 'input  = ' ~ @data;
say 'output = ' ~ @data.&selection_sort;

```


{{out}}

```txt
input  = 22 7 2 -5 8 4
output = -5 2 4 7 8 22

```


Solution 2:

```perl6
sub selectionSort(@tmp) {
    for ^@tmp -> $i {
        my $min = $i; @tmp[$i, $_] = @tmp[$_, $i] if @tmp[$min] > @tmp[$_] for $i^..^@tmp;
    }
    return @tmp;
}

```


{{out}}

```txt
input  = 22 7 2 -5 8 4
output = -5 2 4 7 8 22

```



## Phix

Copy of [[Sorting_algorithms/Selection_sort#Euphoria|Euphoria]]

```Phix
function selection_sort(sequence s)
integer m
    for i=1 to length(s) do
        m = i
        for j=i+1 to length(s) do
            if s[j]<s[m] then
                m = j
            end if
        end for
        {s[i],s[m]} = {s[m],s[i]}
    end for
    return s
end function
```



## PHP

Iterative:

```php
function selection_sort(&$arr) {
    $n = count($arr);
    for($i = 0; $i < count($arr); $i++) {
        $min = $i;
        for($j = $i + 1; $j < $n; $j++){
            if($arr[$j] < $arr[$min]){
                $min = $j;
            }
        }
        list($arr[$i],$arr[$min]) = array($arr[$min],$arr[$i]);
    }
}
```

Recursive:

```php
function selectionsort($arr,$result=array()){
    if(count($arr) == 0){
        return $result;
    }
    $nresult = $result;
    $nresult[] = min($arr);
    unset($arr[array_search(min($arr),$arr)]);
    return selectionsort($arr,$nresult);
}
```



## PicoLisp


```PicoLisp
(de selectionSort (Lst)
   (map
      '((L) (and (cdr L) (xchg L (member (apply min @) L))))
      Lst )
   Lst )
```



## PL/I


```PL/I

Selection: procedure options (main);         /* 2 November 2013 */

   declare a(10) fixed binary initial (
      5, 7, 3, 98, 4, -3, 25, 20, 60, 17);

   put edit (trim(a)) (a, x(1));

   call Selection_Sort (a);

   put skip edit (trim(a)) (a, x(1));

Selection_sort: procedure (a);
   declare a(*) fixed binary;
   declare t fixed binary;
   declare n fixed binary;
   declare (i, j, k) fixed binary;

   n = hbound(a,1);
   do j = 1 to n;
      k = j; t = a(j);
      do i = j+1 to n;
         if t > a(i) then do; t = a(i); k = i; end;
      end;
      a(k) = a(j); a(j) = t;
   end;
end Selection_Sort;

end Selection;

```

Results:

```txt

5 7 3 98 4 -3 25 20 60 17
-3 3 4 5 7 17 20 25 60 98

```



## PowerShell


```PowerShell
Function SelectionSort( [Array] $data )
{
	$datal=$data.length-1
	0..( $datal - 1 ) | ForEach-Object {
		$min = $data[ $_ ]
		$mini = $_
		( $_ + 1 )..$datal | ForEach-Object {
			if( $data[ $_ ] -lt $min ) {
				$min = $data[ $_ ]
				$mini = $_
			}
		}
		$temp = $data[ $_ ]
		$data[ $_ ] = $min
		$data[ $mini ] = $temp
	}
	$data
}

$l = 100; SelectionSort( ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } ) )
```



## Prolog

Works with '''SWI-Prolog 6.3.11''' (needs nth0/4).

```Prolog

selection_sort([], []).
selection_sort([H | L], [H1 | L2]) :-
	exchange(H, L, H1, L1),
	selection_sort(L1, L2).


exchange(H, [], H, []).

exchange(H, L, H1, L1) :-
	min_list(L, H2),
	(   H < H2
	->  H1 = H, L1 = L
	;   H1 = H2,
	    % does the exchange of the number H
	    % and the min of the list
	    nth0(Ind, L, H1, L2),
	    nth0(Ind, L1, H, L2)).

```


## PureBasic


```PureBasic
Procedure selectionSort(Array a(1))
  Protected i, j, lastIndex, minIndex

  lastIndex = ArraySize(a())
  For i = 0 To lastIndex - 1
    minIndex = i
    For j = i + 1 To lastIndex
      If a(minIndex) > a(j)
        minIndex = j
      EndIf
    Next
    Swap a(minIndex), a(i)
  Next
EndProcedure
```



## Python



```python
def selection_sort(lst):
    for i, e in enumerate(lst):
        mn = min(range(i,len(lst)), key=lst.__getitem__)
        lst[i], lst[mn] = lst[mn], e
    return lst
```



## R

For loop:

```r
selectionsort.loop <- function(x)
{
   lenx <- length(x)
   for(i in seq_along(x))
   {
      mini <- (i - 1) + which.min(x[i:lenx])
      start_ <- seq_len(i-1)
      x <- c(x[start_], x[mini], x[-c(start_, mini)])
   }
   x
}
```

Recursive:

(A prettier solution, but, you may need to increase the value of options("expressions") to test it.  Also, you may get a stack overflow if the length of the input vector is more than a few thousand.)

```r
selectionsort.rec <- function(x)
{
   if(length(x) > 1)
   {
      mini <- which.min(x)
      c(x[mini], selectionsort(x[-mini]))
   } else x
}
```



## Qi

{{trans|sml}}

```qi
(define select-r
  Small []     Output -> [Small | (selection-sort Output)]
  Small [X|Xs] Output -> (select-r X Xs [Small|Output]) where (< X Small)
  Small [X|Xs] Output -> (select-r Small Xs [X|Output]))

(define selection-sort
  []          -> []
  [First|Lst] -> (select-r First Lst []))

(selection-sort [8 7 4 3 2 0 9 1 5 6])

```



## Racket


```racket

#lang racket
(define (selection-sort xs)
  (cond [(empty? xs) '()]
        [else (define x0 (apply min xs))
              (cons x0 (selection-sort (remove x0 xs)))]))

```



## Ra


```Ra

class SelectionSort
	**Sort a list with the Selection Sort algorithm**

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
			count := list.count
			last := count - 1

			for i in last

				minCandidate := i
				j := i + 1

				while j < count
					if list[j] < list[minCandidate], minCandidate := j
					j :+ 1

				temp := list[i]
				list[i] := list[minCandidate]
				list[minCandidate] := temp

```



## REXX


```rexx
/*REXX program  sorts  a  stemmed array  using the   selection─sort   algorithm.        */
@.=;                     @.1 = '---The seven hills of Rome:---'
                         @.2 = '
### ========================
';        @.6 = 'Virminal'
                         @.3 = 'Caelian'                       ;        @.7 = 'Esquiline'
                         @.4 = 'Palatine'                      ;        @.8 = 'Quirinal'
                         @.5 = 'Capitoline'                    ;        @.9 = 'Aventine'
           do #=1  until @.#=='';    end         /*find the number of items in the array*/
# = # - 1                                        /*adjust  #  (because of  DO  index).  */
call show   'before sort'                        /*show the   before   array elements.  */
     say  copies('▒', 65)                        /*show a nice separator line  (fence). */
call selectionSort   #                           /*invoke selection sort (and # items). */
call show   ' after sort'                        /*show the    after   array elements.  */
exit                                             /*stick a fork in it,  we're a;; done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
selectionSort: procedure expose @.;  parse arg n
                     do j=1  for n-1;         _= @.j;         p= j
                         do k=j+1  to n;      if @.k>=_  then iterate
                         _= @.k;      p= k       /*this item is out─of─order, swap later*/
                         end   /*k*/
                     if p==j  then iterate       /*if the same, the order of items is OK*/
                     _= @.j;  @.j= @.p;  @.p= _  /*swap 2 items that're out─of─sequence.*/
                     end   /*j*/
               return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  do i=1  for #;  say '       element' right(i,length(#)) arg(1)":" @.i; end;  return
```

{{out|output|:}}

```txt

       element 1 before sort: ---The seven hills of Rome:---
       element 2 before sort:
### ========================

       element 3 before sort: Caelian
       element 4 before sort: Palatine
       element 5 before sort: Capitoline
       element 6 before sort: Virminal
       element 7 before sort: Esquiline
       element 8 before sort: Quirinal
       element 9 before sort: Aventine
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
       element 1  after sort: ---The seven hills of Rome:---
       element 2  after sort:
### ========================

       element 3  after sort: Aventine
       element 4  after sort: Caelian
       element 5  after sort: Capitoline
       element 6  after sort: Esquiline
       element 7  after sort: Palatine
       element 8  after sort: Quirinal
       element 9  after sort: Virminal

```



## Ring


```ring

aList = [7,6,5,9,8,4,3,1,2,0]
see sortList(aList)

func sortList list
     count = len(list) + 1
     last = count - 1

     for i = 1 to last
          minCandidate = i
          j = i + 1
          while j < count
	        if list[j] < list[minCandidate] minCandidate = j ok
	        j = j + 1
          end
          temp = list[i]
          list[i] = list[minCandidate]
          list[minCandidate] = temp
      next
      return list

```



## Ruby

{{trans|Tcl}}

```ruby
class Array
  def selectionsort!
    for i in 0..length-2
      min_idx = i
      for j in (i+1)...length
        min_idx = j  if self[j] < self[min_idx]
      end
      self[i], self[min_idx] = self[min_idx], self[i]
    end
    self
  end
end
ary = [7,6,5,9,8,4,3,1,2,0]
p ary.selectionsort!
# => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Run BASIC


```runbasic
siz = 10
dim srdData(siz)
for i = 1 to siz
srtData(i) = rnd(0) * 100
next i

FOR i = 1 TO siz-1
   lo = i
   FOR j = (i + 1) TO siz
      IF srtData(j) < srtData(lo) lo = j
   NEXT j
   if i <> lo then
   temp        = srtData(i)
   srtData(i)  = srtData(lo)
   srtData(lo) = temp
   end if
NEXT i

for i = 1 to siz
print i;chr$(9);srtData(i)
next i
```


```txt
1	20.5576419
2	32.4299311
3	48.345375
4	54.135847
5	63.1427764
6	67.8079128
7	85.2134895
8	91.3576602
9	95.4280853
10	98.8323211
```



## Rust


```rust

fn selection_sort(array: &mut [i32]) {

    let mut min;

    for i in 0..array.len() {

        min = i;

        for j in (i+1)..array.len() {

            if array[j] < array[min] {
                min = j;
            }
        }

        let tmp = array[i];
        array[i] = array[min];
        array[min] = tmp;
    }
}

fn main() {

    let mut array = [ 9, 4, 8, 3, -5, 2, 1, 6 ];
    println!("The initial array is {:?}", array);

    selection_sort(&mut array);
    println!(" The sorted array is {:?}", array);
}

```



## Scala


```scala
def swap(a: Array[Int], i1: Int, i2: Int) = { val tmp = a(i1); a(i1) = a(i2); a(i2) = tmp }

def selectionSort(a: Array[Int]) =
  for (i <- 0 until a.size - 1)
    swap(a, i, (i + 1 until a.size).foldLeft(i)((currMin, index) =>
      if (a(index) < a(currMin)) index else currMin))
```


This version avoids the extra definition by using a function literal:


```scala
def selectionSort(a: Array[Int]) =  for (i <- 0 until a.size - 1) (
  { (i1: Int, i2: Int) => val tmp = a(i1); a(i1) = a(i2); a(i2) = tmp }
  ) (i, (i + 1 until a.size).foldLeft(i)((currMin, index) => if (a(index) < a(currMin)) index else currMin) )
```


Functional way:

```scala
def selectionSort[T <% Ordered[T]](list: List[T]): List[T] = {
  def remove(e: T, list: List[T]): List[T] =
    list match {
      case Nil => Nil
      case x :: xs if x == e => xs
      case x :: xs => x :: remove(e, xs)
    }

  list match {
    case Nil => Nil
    case _ =>
      val min = list.min
      min :: selectionSort(remove(min, list))
  }
}

```



## Seed7


```seed7
const proc: selectionSort (inout array elemType: arr) is func
  local
    var integer: i is 0;
    var integer: j is 0;
    var integer: min is 0;
    var elemType: help is elemType.value;
  begin
    for i range 1 to length(arr) - 1 do
      min := i;
      for j range i + 1 to length(arr) do
        if arr[j] < arr[min] then
          min := j;
        end if;
      end for;
      help := arr[min];
      arr[min] := arr[i];
      arr[i] := help;
    end for;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/sorting.htm#selectionSort]


## Sidef

{{trans|Ruby}}

```ruby
class Array {
    method selectionsort {
        for i in ^(self.end) {
            var min_idx = i
            for j in (i+1 .. self.end) {
                if (self[j] < self[min_idx]) {
                    min_idx = j
                }
            }
            self.swap(i, min_idx)
        }
        return self
    }
}

var nums = [7,6,5,9,8,4,3,1,2,0];
say nums.selectionsort;

var strs = ["John", "Kate", "Zerg", "Alice", "Joe", "Jane"];
say strs.selectionsort;
```



## Standard ML


```sml
fun selection_sort [] = []
  | selection_sort (first::lst) =
    let
      val (small, output) = foldl
        (fn (x, (small, output)) =>
            if x < small then
              (x, small::output)
            else
              (small, x::output)
        ) (first, []) lst
    in
      small :: selection_sort output
    end
```


## Stata


```stata
mata
function selection_sort(real vector a) {
	real scalar i, j, k, n
	n = length(a)
	for (i = 1; i < n; i++) {
		k = i
		for (j = i+1; j <= n; j++) {
			if (a[j] < a[k]) k = j
		}
		if (k != i) a[(i, k)] = a[(k, i)]
	}
}
end
```


## Swift


```Swift
func selectionSort(inout arr:[Int]) {
    var min:Int

    for n in 0..<arr.count {
        min = n

        for x in n+1..<arr.count {
            if (arr[x] < arr[min]) {
                min = x
            }
        }

        if min != n {
            let temp = arr[min]
            arr[min] = arr[n]
            arr[n] = temp
        }
    }
}
```



## Tcl

{{tcllib|struct::list}}

```tcl
package require Tcl 8.5
package require struct::list

proc selectionsort {A} {
    set len [llength $A]
    for {set i 0} {$i < $len - 1} {incr i} {
        set min_idx [expr {$i + 1}]
        for {set j $min_idx} {$j < $len} {incr j} {
            if {[lindex $A $j] < [lindex $A $min_idx]} {
                set min_idx $j
            }
        }
        if {[lindex $A $i] > [lindex $A $min_idx]} {
            struct::list swap A $i $min_idx
        }
    }
    return $A
}

puts [selectionsort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
```


=={{header|TI-83 BASIC}}==
Store input into L<sub>1</sub> and prgmSORTSLCT will store the sorted output into L<sub>2</sub>.
 :L<sub>1</sub>→L<sub>2</sub>
 :dim(L<sub>2</sub>)→I
 :For(A,1,I)
 :A→C
 :0→X
 :For(B,A,I)
 :If L<sub>2</sub>(B)<L<sub>2</sub>(C)
 :Then
 :B→C
 :1→X
 :End
 :End
 :If X=1
 :Then
 :L<sub>2</sub>(C)→B
 :L<sub>2</sub>(A)→L<sub>2</sub>(C)
 :B→L<sub>2</sub>(A)
 :End
 :End
 :DelVar A
 :DelVar B
 :DelVar C
 :DelVar I
 :DelVar X
 :Return


## uBasic/4tH

<lang>PRINT "Selection sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Selectionsort (n)
  PROC _ShowArray (n)
PRINT

END


_Selectionsort PARAM (1)               ' Selection sort
  LOCAL (3)

  FOR b@ = 0 TO a@-1
    c@ = b@

    FOR d@ = b@ TO a@-1
      IF @(d@) < @(c@) THEN c@ = d@
    NEXT

    IF b@ # c@ THEN PROC _Swap (b@, c@)
  NEXT
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

The selection_sort function is parameterized by a relational predicate p.
There are no arrays in Ursala so it uses a list, and the selected item
is deleted from the list and inserted into another on each iteration
rather than swapped with a preceding item of the same list.

```Ursala
#import std

selection_sort "p" = @iNX ~&l->rx ^(gldif ==,~&r)^/~&l ^|C/"p"$- ~&
```

This is already a bad way to code a sorting algorithm in this
language, but with only a bit more work, we can get a bigger and
slower version that more closely simulates the operations of
repeatedly reordering an array.

```Ursala
selection_sort "p" = ~&itB^?a\~&a ^|JahPfatPRC/~& ~=-~BrhPltPClhPrtPCTlrTQrS^D/"p"$- ~&
```

Here is a test program sorting by the partial order relation on natural
numbers.

```Ursala
#import nat
#cast %nL

example = selection_sort(nleq) <294,263,240,473,596,392,621,348,220,815>
```

{{out}}

```txt
<220,240,263,294,348,392,473,596,621,815>
```



## VBA

I shameless stole the swap function from the bubblesort VBscript implementation.


```VBA

sub swap( byref a, byref b)
	dim tmp
	tmp = a
	a = b
	b = tmp
end sub

function selectionSort (a)
	for i = 0 to ubound(a)
	k = i
		for j=i+1 to ubound(a)
			if a(j) < a(i) then
				swap a(i), a(j)
			end if
		next
	next
	selectionSort = a
end function

```


## VBScript


```vb
Function Selection_Sort(s)
	arr = Split(s,",")
	For i = 0 To UBound(arr)
		For j = i To UBound(arr)
			temp = arr(i)
			If arr(j) < arr(i) Then
				arr(i) = arr(j)
				arr(j) = temp
			End If
		Next
	Next
	Selection_Sort = (Join(arr,","))
End Function

WScript.StdOut.Write "Pre-Sort" & vbTab & "Sorted"
WScript.StdOut.WriteLine
WScript.StdOut.Write "3,2,5,4,1" & vbTab & Selection_Sort("3,2,5,4,1")
WScript.StdOut.WriteLine
WScript.StdOut.Write "c,e,b,a,d" & vbTab & Selection_Sort("c,e,b,a,d")
```

{{out}}

```txt

Pre-Sort	Sorted
3,2,5,4,1	1,2,3,4,5
c,e,b,a,d	a,b,c,d,e

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings

proc    SelSort(A, N);          \Selection sort
char    A;                      \address of array
int     N;                      \number of elements in array (size)
int     I, J, S, JS, T;
[for I:= 0 to N-2 do
        [S:= (~0)>>1;
        for J:= I to N-1 do     \find smallest element
                if A(J) < S then [S:= A(J);  JS:= J];
        T:= A(I);  A(I):= A(JS);  A(JS):= T;
        ];
];

func    StrLen(Str);            \Return number of characters in an ASCIIZ string
char    Str;
int     I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;

char    Str;
[Str:= "Pack my box with five dozen liquor jugs.";
SelSort(Str, StrLen(Str));
Text(0, Str);  CrLf(0);
]
```


{{out}}

```txt

       .Pabcdeefghiiijklmnoooqrstuuvwxyz

```



## zkl


```zkl
fcn selectionSort(list){  // sort a list of ints
   copy,r:=list.copy(),List();
   while(copy){
      min,idx:=(0).min(copy), copy.find(min);
      r.append(min);
      copy.del(idx);
   }
   r
}
```


```zkl
selectionSort(List(28, 44, 46, 24, 19, -5, 2, 17, 11, 25, 4)).println();
```

{{out}}

```txt

L(-5,2,4,11,17,19,24,25,28,44,46)

```



{{omit from|GUISS}}

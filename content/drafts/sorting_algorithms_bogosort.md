+++
title = "Sorting algorithms/Bogosort"
description = ""
date = 2019-10-20T02:52:13Z
aliases = []
[extra]
id = 2850
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}

;Task:
[[wp:Bogosort|Bogosort]] a list of numbers.


Bogosort simply shuffles a collection randomly until it is sorted.

"Bogosort" is a perversely inefficient algorithm only used as an in-joke.

Its average run-time is   O(n!)   because the chance that any given shuffle of a set will end up in sorted order is about one in   ''n''   factorial,   and the worst case is infinite since there's no guarantee that a random shuffling will ever produce a sorted sequence.

Its best case is   O(n)   since a single pass through the elements may suffice to order them.


Pseudocode:
 '''while not''' InOrder(list) '''do'''
    Shuffle(list)
 '''done'''



The [[Knuth shuffle]] may be used to implement the shuffle part of this algorithm.





## ActionScript


```actionscript
public function bogoSort(arr:Array):Array
{
    while (!sorted(arr))
    {
        shuffle(arr);
    }

    return arr;
}

public function shuffle(arr:Array):void
{
    for (var i:int = 0; i < arr.length; i++)
    {
        var rand:int = Math.floor(Math.random() * arr.length);
        var tmp:* = arr[i];
        arr[i] = arr[rand];
        arr[rand] = tmp;
    }
}

public function sorted(arr:Array):Boolean
{
    var last:int = arr[0];

    for (var i:int = 1; i < arr.length; i++)
    {
        if (arr[i] < last)
        {
            return false;
        }

        last = arr[i];
    }

    return true;
}
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Bogosort is
   generic
      type Ordered is private;
      type List is array (Positive range <>) of Ordered;
      with function "<" (L, R : Ordered) return Boolean is <>;
   procedure Bogosort (Data : in out List);

   procedure Bogosort (Data : in out List) is
      function Sorted return Boolean is
      begin
         for I in Data'First..Data'Last - 1 loop
            if not (Data (I) < Data (I + 1)) then
               return False;
            end if;
         end loop;
         return True;
      end Sorted;
      subtype Index is Integer range Data'Range;
      package Dices is new Ada.Numerics.Discrete_Random (Index);
      use Dices;
      Dice : Generator;
      procedure Shuffle is
         J    : Index;
         Temp : Ordered;
      begin
         for I in Data'Range loop
            J := Random (Dice);
            Temp := Data (I);
            Data (I) := Data (J);
            Data (J) := Temp;
         end loop;
      end Shuffle;
   begin
      while not Sorted loop
         Shuffle;
      end loop;
   end Bogosort;

   type List is array (Positive range <>) of Integer;
   procedure Integer_Bogosort is new Bogosort (Integer, List);
   Sequence : List := (7,6,3,9);
begin
   Integer_Bogosort (Sequence);
   for I in Sequence'Range loop
      Put (Integer'Image (Sequence (I)));
   end loop;
end Test_Bogosort;
```

The solution is generic.
The procedure Bogosort can be instantiated
with any copyable comparable type.
In the example given it is the standard Integer type.
{{out}}

```txt

 3 6 7 9

```


## ALGOL 68

{{trans|python}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}


```algol68
MODE TYPE = INT;

PROC random shuffle = (REF[]TYPE l)VOID: (
    INT range = UPB l - LWB l + 1;
    FOR index FROM LWB l TO UPB l DO
        TYPE tmp := l[index];
        INT other := ENTIER (LWB l + random * range);
        l[index] := l[other];
        l[other] := tmp
    OD
);

PROC in order = (REF[]TYPE l)BOOL: (
    IF LWB l >= UPB l THEN
        TRUE
    ELSE
        TYPE last := l[LWB l];
        FOR index FROM LWB l + 1 TO UPB l DO
            IF l[index] < last THEN
                GO TO return false
            FI;
            last := l[index]
        OD;
        TRUE EXIT
        return false: FALSE
    FI
);

PROC bogo sort = (REF[]TYPE l)REF[]TYPE: (
    WHILE NOT in order(l) DO
        random shuffle(l)
    OD;
    l
);

[6]TYPE sample := (61, 52, 63, 94, 46, 18);
print((bogo sort(sample), new line))
```

{{out}}
        +18        +46        +52        +61        +63        +94


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program bogosort.s   */

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
sMessResult:      .ascii "Value  : "
sMessValeur:       .fill 11, 1, ' '            @ size => 11
szCarriageReturn: .asciz "\n"

.align 4
iGraine:  .int 123456
.equ NBELEMENTS,      6
TableNumber:	     .int   1,2,3,4,5,6,7,8,9,10

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                           @ entry of program

1:
    ldr r0,iAdrTableNumber                      @ address number table
    mov r1,#NBELEMENTS                          @ number of élements
    bl knuthShuffle

    @ table  display elements
    ldr r2,iAdrTableNumber
    mov r3,#0
2:                                              @ loop display table
    ldr r0,[r2,r3,lsl #2]
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message
    add r3,#1
    cmp r3,#NBELEMENTS - 1
    ble 2b
    ldr r0,iAdrszCarriageReturn
    bl affichageMess

    ldr r0,iAdrTableNumber                      @ address number table
    mov r1,#NBELEMENTS                          @ number of élements
    bl isSorted                                 @ control sort
    cmp r0,#1                                   @ sorted ?
    bne 1b                                      @ no -> loop


100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc #0                                      @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:    .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrTableNumber:          .int TableNumber
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
/*     knuthShuffle Shuffle                                  */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the number of elements */
knuthShuffle:
    push {r2-r5,lr}                                    @ save registers
    mov r5,r0                                          @ save table address
    mov r2,#0                                          @ start index
1:
    mov r0,r2                                          @ generate aleas
    bl genereraleas
    ldr r3,[r5,r2,lsl #2]                              @ swap number on the table
    ldr r4,[r5,r0,lsl #2]
    str r4,[r5,r2,lsl #2]
    str r3,[r5,r0,lsl #2]
    add r2,#1                                          @ next number
    cmp r2,r1                                          @ end ?
    blt 1b                                             @ no -> loop

100:
    pop {r2-r5,lr}
    bx lr                                              @ return

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
/***************************************************/
/*   Generation random number                  */
/***************************************************/
/* r0 contains limit  */
genereraleas:
    push {r1-r4,lr}                                    @ save registers
    ldr r4,iAdriGraine
    ldr r2,[r4]
    ldr r3,iNbDep1
    mul r2,r3,r2
    ldr r3,iNbDep1
    add r2,r2,r3
    str r2,[r4]                                        @ maj de la graine pour l appel suivant
    cmp r0,#0
    beq 100f
    mov r1,r0                                          @ divisor
    mov r0,r2                                          @ dividende
    bl division
    mov r0,r3                                          @ résult = remainder

100:                                                   @ end function
    pop {r1-r4,lr}                                     @ restaur registers
    bx lr                                              @ return
/*****************************************************/
iAdriGraine: .int iGraine
iNbDep1: .int 0x343FD
iNbDep2: .int 0x269EC3
/***************************************************/
/* integer division unsigned                       */
/***************************************************/
division:
    /* r0 contains dividend */
    /* r1 contains divisor */
    /* r2 returns quotient */
    /* r3 returns remainder */
    push {r4, lr}
    mov r2, #0                                         @ init quotient
    mov r3, #0                                         @ init remainder
    mov r4, #32                                        @ init counter bits
    b 2f
1:                                                     @ loop
    movs r0, r0, LSL #1                                @ r0 <- r0 << 1 updating cpsr (sets C if 31st bit of r0 was 1)
    adc r3, r3, r3                                     @ r3 <- r3 + r3 + C. This is equivalent to r3 ? (r3 << 1) + C
    cmp r3, r1                                         @ compute r3 - r1 and update cpsr
    subhs r3, r3, r1                                   @ if r3 >= r1 (C=1) then r3 <- r3 - r1
    adc r2, r2, r2                                     @ r2 <- r2 + r2 + C. This is equivalent to r2 <- (r2 << 1) + C
2:
    subs r4, r4, #1                                    @ r4 <- r4 - 1
    bpl 1b                                             @ if r4 >= 0 (N=0) then loop
    pop {r4, lr}
    bx lr


```





## AutoHotkey


```AutoHotkey
MsgBox % Bogosort("987654")
MsgBox % Bogosort("319208")
MsgBox % Bogosort("fedcba")
MsgBox % Bogosort("gikhjl")

Bogosort(sequence) {
  While !Sorted(sequence)
    sequence := Shuffle(sequence)
  Return sequence
}

Sorted(sequence) {
  Loop, Parse, sequence
  {
    current := A_LoopField
    rest := SubStr(sequence, A_Index)
    Loop, Parse, rest
    {
      If (current > A_LoopField)
        Return false
    }
  }
  Return true
}

Shuffle(sequence) {
  Max := StrLen(sequence) + 1
  Loop % StrLen(sequence) {
    Random, Num, 1, % Max - A_Index
    Found .= SubStr(sequence, Num, 1)
    sequence := SubStr(sequence, 1, Num-1) . SubStr(sequence, Num+1)
  }
  Return Found
}
```



## AWK

Sort standard input and output to the standard output

```awk
function randint(n)
{
  return int(n * rand())
}

function sorted(sa, sn)
{
  for(si=1; si < sn; si++) {
    if ( sa[si] > sa[si+1] ) return 0;
  }
  return 1
}

{
  line[NR] = $0
}
END { # sort it with bogo sort
  while ( sorted(line, NR) == 0 ) {
    for(i=1; i <= NR; i++) {
      r = randint(NR) + 1
      t = line[i]
      line[i] = line[r]
      line[r] = t
    }
  }
  #print it
  for(i=1; i <= NR; i++) {
    print line[i]
  }
}
```



## BBC BASIC


```bbcbasic
      DIM test(9)
      test() = 4, 65, 2, 31, 0, 99, 2, 83, 782, 1

      shuffles% = 0
      WHILE NOT FNsorted(test())
        shuffles% += 1
        PROCshuffle(test())
      ENDWHILE
      PRINT ;shuffles% " shuffles required to sort "; DIM(test(),1)+1 " items."
      END

      DEF PROCshuffle(d())
      LOCAL I%
      FOR I% = DIM(d(),1)+1 TO 2 STEP -1
        SWAP d(I%-1), d(RND(I%)-1)
      NEXT
      ENDPROC

      DEF FNsorted(d())
      LOCAL I%
      FOR I% = 1 TO DIM(d(),1)
        IF d(I%) < d(I%-1) THEN = FALSE
      NEXT
      = TRUE
```

{{out}}

```txt

383150 shuffles required to sort 10 items.

```



## Brat


```brat
bogosort = { list |
	sorted = list.sort #Kinda cheating here
	while { list != sorted } { list.shuffle! }
	list
}

p bogosort [15 6 2 9 1 3 41 19]
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool is_sorted(int *a, int n)
{
  while ( --n >= 1 ) {
    if ( a[n] < a[n-1] ) return false;
  }
  return true;
}

void shuffle(int *a, int n)
{
  int i, t, r;
  for(i=0; i < n; i++) {
    t = a[i];
    r = rand() % n;
    a[i] = a[r];
    a[r] = t;
  }
}

void bogosort(int *a, int n)
{
  while ( !is_sorted(a, n) ) shuffle(a, n);
}

int main()
{
  int numbers[] = { 1, 10, 9,  7, 3, 0 };
  int i;

  bogosort(numbers, 6);
  for (i=0; i < 6; i++) printf("%d ", numbers[i]);
  printf("\n");
}
```



## C++

Uses C++11. Compile with
 g++ -std=c++11 bogo.cpp

```cpp
#include <algorithm>
#include <iostream>
#include <iterator>
#include <random>

template <typename RandomAccessIterator, typename Predicate>
void bogo_sort(RandomAccessIterator begin, RandomAccessIterator end,
               Predicate p) {
  std::random_device rd;
  std::mt19937 generator(rd());
  while (!std::is_sorted(begin, end, p)) {
    std::shuffle(begin, end, generator);
  }
}

template <typename RandomAccessIterator>
void bogo_sort(RandomAccessIterator begin, RandomAccessIterator end) {
  bogo_sort(
      begin, end,
      std::less<
          typename std::iterator_traits<RandomAccessIterator>::value_type>());
}

int main() {
  int a[] = {100, 2, 56, 200, -52, 3, 99, 33, 177, -199};
  bogo_sort(std::begin(a), std::end(a));
  copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
  std::cout << "\n";
}
```

{{out}}

```txt

-199 -52 2 3 33 56 99 100 177 200

```


=={{header|C sharp|C#}}==
{{works with|C sharp|C#|3.0+}}

```csharp
using System;
using System.Collections.Generic;

namespace RosettaCode.BogoSort
{
    public static class BogoSorter
    {
        public static void Sort<T>(List<T> list) where T:IComparable
        {
            while (!list.isSorted())
            {
                list.Shuffle();
            }
        }

        private static bool isSorted<T>(this IList<T> list) where T:IComparable
        {
            if(list.Count<=1)
                return true;
            for (int i = 1 ; i < list.Count; i++)
                if(list[i].CompareTo(list[i-1])<0) return false;
            return true;
        }

        private static void Shuffle<T>(this IList<T> list)
        {
            Random rand = new Random();
            for (int i = 0; i < list.Count; i++)
            {
                int swapIndex = rand.Next(list.Count);
                T temp = list[swapIndex];
                list[swapIndex] = list[i];
                list[i] = temp;
            }
        }
    }

    class TestProgram
    {
        static void Main()
        {
            List<int> testList = new List<int> { 3, 4, 1, 8, 7, 4, -2 };
            BogoSorter.Sort(testList);
            foreach (int i in testList) Console.Write(i + " ");
        }

    }
}
```



## Clojure



```clojure
(defn in-order? [order xs]
  (or (empty? xs)
      (apply order xs)))

(defn bogosort [order xs]
  (if (in-order? order xs) xs
    (recur order (shuffle xs))))

(println (bogosort < [7 5 12 1 4 2 23 18]))
```



## COBOL

This program generates an array of ten pseudo-random numbers in the range 0 to 999 and then sorts them into ascending order. Eventually.

```cobol
identification division.
program-id. bogo-sort-program.
data division.
working-storage section.
01  array-to-sort.
    05 item-table.
        10 item          pic 999
            occurs 10 times.
01  randomization.
    05 random-seed       pic 9(8).
    05 random-index      pic 9.
01  flags-counters-etc.
    05 array-index       pic 99.
    05 adjusted-index    pic 99.
    05 temporary-storage pic 999.
    05 shuffles          pic 9(8)
        value zero.
    05 sorted            pic 9.
01  numbers-without-leading-zeros.
    05 item-no-zeros     pic z(4).
    05 shuffles-no-zeros pic z(8).
procedure division.
control-paragraph.
    accept random-seed from time.
    move function random(random-seed) to item(1).
    perform random-item-paragraph varying array-index from 2 by 1
    until array-index is greater than 10.
    display 'BEFORE SORT:' with no advancing.
    perform show-array-paragraph varying array-index from 1 by 1
    until array-index is greater than 10.
    display ''.
    perform shuffle-paragraph through is-it-sorted-paragraph
    until sorted is equal to 1.
    display 'AFTER SORT: ' with no advancing.
    perform show-array-paragraph varying array-index from 1 by 1
    until array-index is greater than 10.
    display ''.
    move shuffles to shuffles-no-zeros.
    display shuffles-no-zeros ' SHUFFLES PERFORMED.'
    stop run.
random-item-paragraph.
    move function random to item(array-index).
show-array-paragraph.
    move item(array-index) to item-no-zeros.
    display item-no-zeros with no advancing.
shuffle-paragraph.
    perform shuffle-items-paragraph,
    varying array-index from 1 by 1
    until array-index is greater than 10.
    add 1 to shuffles.
is-it-sorted-paragraph.
    move 1 to sorted.
    perform item-in-order-paragraph varying array-index from 1 by 1,
    until sorted is equal to zero
    or array-index is equal to 10.
shuffle-items-paragraph.
    move function random to random-index.
    add 1 to random-index giving adjusted-index.
    move item(array-index) to temporary-storage.
    move item(adjusted-index) to item(array-index).
    move temporary-storage to item(adjusted-index).
item-in-order-paragraph.
    add 1 to array-index giving adjusted-index.
    if item(array-index) is greater than item(adjusted-index)
    then move zero to sorted.
```

{{out}}

```txt
BEFORE SORT: 141 503 930 105  78 518 180 907 791 361
AFTER SORT:   78 105 141 180 361 503 518 791 907 930
  237262 SHUFFLES PERFORMED.
```



## Common Lisp


Sortedp checks that each element of a list is related by predicate to the next element of the list.  I.e., <code>(sortedp (x<sub>1</sub> x<sub>2</sub> … x<sub>n</sub>) pred)</code> is true when each of <code>(pred x<sub>1</sub> x<sub>2</sub>)</code>, …, <code>(pred x<sub>n-1</sub> x<sub>n</sub>)</code> is true.

<code>nshuffle</code> is the same code as in [[Knuth shuffle#Common Lisp|Knuth shuffle]].


```lisp
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i ))))
  sequence)

(defun sortedp (list predicate)
  (every predicate list (rest list)))

(defun bogosort (list predicate)
  (do ((list list (nshuffle list)))
      ((sortedp list predicate) list)))
```



## Crystal


```crystal
def knuthShuffle(items : Array)
    i = items.size-1
    while i > 1
        j = Random.rand(0..i)
        items.swap(i, j)

        i -= 1
    end
end

def sorted?(items : Array)
    prev = items[0]
    items.each do |item|
        if item < prev
            return false
        end
        prev = item
    end
    return true
end

def bogoSort(items : Array)
    while !sorted?(items)
        knuthShuffle(items)
    end
end
```



## D


```d
import std.stdio, std.algorithm, std.random;

void bogoSort(T)(T[] data) {
    while (!isSorted(data))
        randomShuffle(data);
}

void main() {
    auto array = [2, 7, 41, 11, 3, 1, 6, 5, 8];
    bogoSort(array);
    writeln(array);
}
```

{{out}}

```txt
[1, 2, 3, 5, 6, 7, 8, 11, 41]
```



## E


Using the shuffle from [[Knuth shuffle#E]].


```e
def isSorted(list) {
    if (list.size() == 0) { return true }
    var a := list[0]
    for i in 1..!(list.size()) {
        var b := list[i]
        if (a > b) { return false }
        a := b
    }
    return true
}

def bogosort(list, random) {
    while (!isSorted(list)) {
        shuffle(list, random)
    }
}
```



## Eiffel


```Eiffel

class
	BOGO_SORT

feature

	bogo_sort (ar: ARRAY [INTEGER]): ARRAY [INTEGER]
			-- Sorted array in ascending order.
		do
			from
			until
				is_sorted (ar) = True
			loop
				Result := shuffel (ar)
			end
		end

feature {NONE}

	is_sorted (ar: ARRAY [INTEGER]): BOOLEAN
			-- Is 'ar' sorted in ascending order?
		require
			not_void: ar /= Void
		local
			i: INTEGER
		do
			Result := True
			from
				i := 1 + 1
			invariant
				i >= 1 + 1 and i <= ar.count + 1
			until
				i > ar.count
			loop
				Result := Result and ar [i - 1] <= ar [i]
				i := i + 1
			variant
				ar.count + 1 - i
			end
		end

	shuffle (ar: ARRAY [INTEGER]): ARRAY [INTEGER]
			-- Array containing the same elements as 'ar' in a shuffled order.
		require
			more_than_one_element: ar.count > 1
		local
			count, j, ith: INTEGER
			random: V_RANDOM
		do
			create random
			create Result.make_empty
			Result.deep_copy (ar)
			count := ar.count
			across
				1 |..| count as c
			loop
				j := random.bounded_item (c.item, count)
				ith := Result [c.item]
				Result [c.item] := Result [j]
				Result [j] := ith
				random.forth
			end
		ensure
			same_elements: across ar as a all Result.has (a.item) end
		end

end
</lang >
TEST:

```Eiffel

class
	APPLICATION

create
	make

feature {NONE}

	make
		do
			test := <<3, 2, 5, 7, 1>>
			io.put_string ("Unsorted: ")
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
			create sorter
			test := sorter.bogo_sort (test)
			io.put_string ("%NSorted: ")
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
		end

	test: ARRAY [INTEGER]

	sorter: BOGO_SORT

end

```

{{out}}

```txt

Unsorted: 3 2 5 7 1
Sorted: 1 2 3 5 7

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;

extension op
{
    bogoSorter()
    {
        var list := self;

        until (list.isAscendant())
        {
            list := list.randomize(list.Length)
        };

        ^ list
    }
}

public program()
{
    var list := new int[]::(3, 4, 1, 8, 7, -2, 0);

    console.printLine("before:", list.asEnumerable());
    console.printLine("after :", list.bogoSorter().asEnumerable())
}
```

{{out}}

```txt

before:3,4,1,8,7,-2,0
after :-2,0,1,3,4,7,8

```



## Elixir


```elixir
defmodule Sort do
  def bogo_sort(list) do
    if sorted?(list) do
      list
    else
      bogo_sort(Enum.shuffle(list))
    end
  end

  defp sorted?(list) when length(list)<=1, do: true
  defp sorted?([x, y | _]) when x>y, do: false
  defp sorted?([_, y | rest]), do: sorted?([y | rest])
end
```


Example:

```txt

iex(114)> Sort.bogo_sort([5,3,9,4,1,6,8,2,7])
[1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Euphoria


```euphoria
function shuffle(sequence s)
    object temp
    integer j
    for i = length(s) to 1 by -1 do
        j = rand(i)
        if i != j then
            temp = s[i]
            s[i] = s[j]
            s[j] = temp
        end if
    end for
    return s
end function

function inOrder(sequence s)
    for i = 1 to length(s)-1 do
        if compare(s[i],s[i+1]) > 0 then
            return 0
        end if
    end for
    return 1
end function

function bogosort(sequence s)
    while not inOrder(s) do
        ? s
        s = shuffle(s)
    end while
    return s
end function

? bogosort(shuffle({1,2,3,4,5,6}))
```


{{out}}

```txt
{1,2,5,4,6,3}
{5,1,3,6,2,4}
{4,6,1,2,5,3}
.............
{1,2,6,5,4,3}
{5,3,1,2,6,4}
{1,2,3,4,5,6}

```



## Factor


```factor
USING: grouping kernel math random sequences ;

: sorted? ( seq -- ? ) 2 <clumps> [ first2 <= ] all? ;
: bogosort ( seq -- newseq ) [ dup sorted? ] [ randomize ] until ;
```



## Fantom



```fantom

class Main
{
  Bool in_order (Int[] items)
  {
    (0..<(items.size-1)).toList.all |Int i -> Bool|
    {
      items[i] <= items[i+1]
    }
  }

  Int[] bogosort (Int[] items)
  {
    while (!in_order(items))
    {
      items.shuffle
    }
    return items
  }

  Void main ()
  {
    // example
    echo ("Sorting [3,4,2,1] gives " + bogosort ([3,4,2,1]))
  }
}

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
MODULE BOGO
IMPLICIT NONE
CONTAINS
  FUNCTION Sorted(a)
    LOGICAL :: Sorted
    INTEGER, INTENT(IN) :: a(:)
    INTEGER :: i

    Sorted = .TRUE.
    DO i = 1, SIZE(a)-1
      IF(a(i) > a(i+1)) THEN
        Sorted = .FALSE.
        EXIT
      END IF
    END DO
  END FUNCTION Sorted

  SUBROUTINE SHUFFLE(a)
    INTEGER, INTENT(IN OUT) :: a(:)
    INTEGER :: i, rand, temp
    REAL :: x

    DO i = SIZE(a), 1, -1
       CALL RANDOM_NUMBER(x)
       rand = INT(x * i) + 1
       temp = a(rand)
       a(rand) = a(i)
       a(i) = temp
    END DO
  END SUBROUTINE
END MODULE

PROGRAM BOGOSORT

  USE BOGO
  IMPLICIT NONE
  INTEGER :: iter = 0
  INTEGER :: array(8) = (/2, 7, 5, 3, 4, 8, 6, 1/)
  LOGICAL :: s

  DO
    s = Sorted(array)
    IF (s) EXIT
    CALL SHUFFLE(array)
    iter = iter + 1
  END DO
  WRITE (*,*) "Array required", iter, " shuffles to sort"

END PROGRAM BOGOSORT
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=b2b766f379d809cbf054c2d32d76c453 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sSorted As String = "123456789"                       'The desired outcome
Dim sTest, sChr As String                                 'Various strings
Dim iCounter As Integer                                   'Loop counter

Do
  Inc iCounter                                            'Increase counter value
  Repeat                                                  'Repeat
    sChr = Chr(Rand(49, 57))                              'Get a random number and convert it to a character e.g. 49="1"
    If Not InStr(sTest, sChr) Then sTest &= sChr          'If the random character is not in sTest then add it
  Until Len(sTest) = 9                                    'Loop until sTest has 9 characters
  Print sTest                                             'Print the string to test
  If sTest = sSorted Then Break                           'If sTest = sSorted then get out of the loop
  sTest = ""                                              'Empty sTest and try again
Loop

Print "Solved in " & Str(iCounter) & " loops"             'Print the result

End
```

Output: (This example was completed in under 2 seconds)

```txt

.........
129536487
345218769
482713659
286745931
123456789
Solved in 155283 loops

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "sort"
    "time"
)

func main() {
    list := []int{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    rand.Seed(time.Now().UnixNano())
    fmt.Println("unsorted:", list)
    temp := make([]int, len(list))
    copy(temp, list)
    for !sort.IntsAreSorted(temp) {
        for i, v := range rand.Perm(len(list)) {
            temp[i] = list[v]
        }
    }
    fmt.Println("sorted!  ", temp)
}
```

{{out}} (sometimes takes a few seconds)

```txt

unsorted: [31 41 59 26 53 58 97 93 23 84]
sorted!   [23 26 31 41 53 58 59 84 93 97]

```



## Groovy

Solution (also implicitly tracks the number of shuffles required):

```groovy
def bogosort = { list ->
    def n = list.size()
    while (n > 1 && (1..<n).any{ list[it-1] > list[it] }) {
        print '.'*n
        Collections.shuffle(list)
    }
    list
}
```


Test Program:

```groovy
println (bogosort([3,1,2]))
```


{{out}} trial 1:

```txt
..............................[1, 2, 3]
```


{{out}} trial 2:

```txt
...................................................[1, 2, 3]
```



## Haskell


```haskell
import System.Random
import Data.Array.IO
import Control.Monad

isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy _ [] = True
isSortedBy f xs = all (uncurry f) . (zip <*> tail) $ xs


-- from http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

bogosortBy :: (a -> a -> Bool) -> [a] -> IO [a]
bogosortBy f xs | isSortedBy f xs = return xs
                | otherwise       = shuffle xs >>= bogosortBy f

bogosort :: Ord a => [a] -> IO [a]
bogosort = bogosortBy (<)
```

Example:

```txt

*Main> bogosort [7,5,12,1,4,2,23,18]
[1,2,4,5,7,12,18,23]

```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure shuffle(l)
   repeat {
       !l :=: ?l
       suspend l
   }
end

procedure sorted(l)
   local i
   if (i := 2 to *l & l[i] >= l[i-1]) then return &fail else return 1
end

procedure main()
   local l
   l := [6,3,4,5,1]
   |( shuffle(l) & sorted(l)) \1 & every writes(" ",!l)
end
```



## Inform 6


```Inform 6
[ shuffle a n i j tmp;
  for(i = n - 1: i > 0: i--)
  {
    j = random(i + 1) - 1;

    tmp = a->j;
    a->j = a->i;
    a->i = tmp;
  }
];

[ is_sorted a n i;
  for(i = 0: i < n - 1: i++)
  {
    if(a->i > a->(i + 1)) rfalse;
  }

  rtrue;
];

[ bogosort a n;
  while(~~is_sorted(a, n))
  {
    shuffle(a, n);
  }
];
```



## Io


```io
List do(
    isSorted := method(
        slice(1) foreach(i, x,
            if (x < at(i), return false)
        )
        return true;
    )

    bogoSortInPlace := method(
        while(isSorted not,
            shuffleInPlace()
        )
    )
)

lst := list(2, 1, 4, 3)
lst bogoSortInPlace println # ==> list(1, 2, 3, 4), hopefully :)
```


## J

{{eff note|J|/:~}}

```j
bogo=: monad define
  whilst.  +./ 2 >/\ Ry  do. Ry=. (A.~ ?@!@#) y  end. Ry
)
```



## Java

Without Collections, Lists or Iterators. With a counter.

```java


public class BogoSort
{
	public static void main(String[] args)
	{
		//Enter array to be sorted here
		int[] arr={4,5,6,0,7,8,9,1,2,3};

		BogoSort now=new BogoSort();
		System.out.print("Unsorted: ");
		now.display1D(arr);

		now.bogo(arr);

		System.out.print("Sorted: ");
		now.display1D(arr);
	}
	void bogo(int[] arr)
	{
		//Keep a track of the number of shuffles
		int shuffle=1;
		for(;!isSorted(arr);shuffle++)
			shuffle(arr);
		//Boast
		System.out.println("This took "+shuffle+" shuffles.");
	}
	void shuffle(int[] arr)
	{
		//Standard Fisher-Yates shuffle algorithm
		int i=arr.length-1;
		while(i>0)
			swap(arr,i--,(int)(Math.random()*i));
	}
	void swap(int[] arr,int i,int j)
	{
		int temp=arr[i];
		arr[i]=arr[j];
		arr[j]=temp;
	}
	boolean isSorted(int[] arr)
	{

		for(int i=1;i<arr.length;i++)
			if(arr[i]<arr[i-1])
				return false;
		return true;
	}
	void display1D(int[] arr)
	{
		for(int i=0;i<arr.length;i++)
			System.out.print(arr[i]+" ");
		System.out.println();
	}

}

```


{{out}}

```txt
Unsorted: 4 5 6 0 7 8 9 1 2 3
This took 23104714 shuffles.
Sorted: 0 1 2 3 4 5 6 7 8 9
```



{{works with|Java|1.5+}}
This implementation works for all comparable types (types with <tt>compareTo</tt> defined).

```java5
import java.util.Collections;
import java.util.List;
import java.util.Iterator;

public class Bogosort {
    private static <T extends Comparable<? super T>> boolean isSorted(List<T> list) {
        if (list.isEmpty())
            return true;
        Iterator<T> it = list.iterator();
        T last = it.next();
        while (it.hasNext()) {
            T current = it.next();
            if (last.compareTo(current) > 0)
                return false;
            last = current;
        }
        return true;
    }

    public static <T extends Comparable<? super T>> void bogoSort(List<T> list) {
        while (!isSorted(list))
            Collections.shuffle(list);
    }
}
```



## JavaScript


```javascript
shuffle = function(v) {
    for(var j, x, i = v.length; i; j = Math.floor(Math.random() * i), x = v[--i], v[i] = v[j], v[j] = x);
    return v;
};

isSorted = function(v){
    for(var i=1; i<v.length; i++) {
        if (v[i-1] > v[i]) { return false; }
    }
    return true;
}

bogosort = function(v){
    var sorted = false;
    while(sorted == false){
        v = shuffle(v);
        sorted = isSorted(v);
    }
    return v;
}
```



## Julia

{{works with|Julia|0.6}}


```julia
function bogosort!(arr::AbstractVector)
    while !issorted(arr)
        shuffle!(arr)
    end
    return arr
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", bogosort!(v))
```


{{out}}

```txt
# unordered: [-7, 0, -6, -1, -6, -1, -3, -1, 4, 8]
 -> ordered: [-7, -6, -6, -3, -1, -1, -1, 0, 4, 8]
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.2

const val RAND_MAX = 32768 // big enough for this

val rand = java.util.Random()

fun isSorted(a: IntArray): Boolean {
    val n = a.size
    if (n < 2) return true
    for (i in 1 until n) {
        if (a[i] < a[i - 1]) return false
    }
    return true
}

fun shuffle(a: IntArray) {
    val n = a.size
    if (n < 2) return
    for (i in 0 until n) {
        val t = a[i]
        val r = rand.nextInt(RAND_MAX) % n
        a[i] = a[r]
        a[r] = t
    }
}

fun bogosort(a: IntArray) {
   while (!isSorted(a)) shuffle(a)
}

fun main(args: Array<String>) {
    val a = intArrayOf(1, 10, 9,  7, 3, 0)
    println("Before sorting : ${a.contentToString()}")
    bogosort(a)
    println("After sorting  : ${a.contentToString()}")
}
```


{{out}}

```txt

Before sorting : [1, 10, 9, 7, 3, 0]
After sorting  : [0, 1, 3, 7, 9, 10]

```



## Lua


```lua
function bogosort (list)
    if type (list) ~= 'table' then return list end

    -- Fisher-Yates Knuth shuffle
    local function shuffle ()
        local rand = math.random(1,#list)
        for i=1,#list do
            list[i],list[rand] = list[rand],list[i]
            rand = math.random(1,#list)
        end
    end

    -- Returns true only if list is now sorted
    local function in_order ()
        local last = list[1]
        for i,v in next,list do
            if v < last then return false end
            last = v
        end
        return true
    end

    while not in_order() do shuffle() end

    return list
end
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
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')
define(`set',`define(`$1[$2]',`$3')')
define(`new',`set($1,size,0)')
define(`get',`defn($1[$2])')
define(`append',
   `set($1,size,incr(get($1,size)))`'set($1,get($1,size),$2)')
define(`deck',
   `new($1)for(`x',1,$2,
         `append(`$1',random)')')
define(`show',
   `for(`x',1,get($1,size),`get($1,x)`'ifelse(x,get($1,size),`',`, ')')')
define(`swap',`set($1,$2,get($1,$4))`'set($1,$4,$3)')
define(`shuffle',
   `for(`x',1,get($1,size),
      `swap($1,x,get($1,x),eval(1+random%get($1,size)))')')
define(`inordern',
   `ifelse(eval($2>=get($1,size)),1,
      1,
      `ifelse(eval(get($1,$2)>get($1,incr($2))),1,
         0,
         `inordern(`$1',incr($2))')')')
define(`inorder',`inordern($1,1)')
define(`bogosort',
   `ifelse(inorder(`$1'),0,`nope shuffle(`$1')`'bogosort(`$1')')')
divert

deck(`b',6)
show(`b')
bogosort(`b')
show(`b')
```



## Maple


```Maple
arr := Array([2,3,1]):
len := numelems(arr):
#Translation of C, random swapping
shuffle_arr := proc(arr, len)
	local i, r, temp:
	for i from 1 to len do
		temp := arr[i]:
		r := rand(1..len)():
		arr[i] := arr[r]:
		arr[r] := temp:
	end do:
end proc:
while(not ListTools:-Sorted(convert(arr, list))) do
	shuffle_arr(arr, len):
end do:
arr;
```

{{Out|Output}}

```txt
[1 2 3]
```



## Mathematica


```Mathematica
Bogosort[x_List] := Block[{t=x},While[!OrderedQ[t],t=RandomSample[x]]; t]

Bogosort[{1, 2, 6, 4, 0, -1, Pi, 3, 5}]
=> {-1, 0, 1, 2, 3, Pi, 4, 5, 6}
```


=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
function list = bogoSort(list)
    while( ~issorted(list) ) %Check to see if it is sorted
        list = list( randperm(numel(list)) ); %Randomly sort the list
    end
end
```


{{out}}

```MATLAB
bogoSort([5 3 8 4 9 7 6 2 1])

ans =

     1     2     3     4     5     6     7     8     9

```



## MAXScript


```maxscript
fn notSorted arr =
(
    if arr.count > 0 then
    (
        local current = arr[1]
        for i in 2 to arr.count do
        (
            if current > arr[i] then
            (
                return true
            )
            current = arr[i]
        )
    )
    false
)

fn randSort x y =
(
    random -1 1
)

fn shuffle arr =
(
    qsort arr randSort
    arr
)

fn bogosort arr =
(
    while notSorted arr do
    (
        arr = shuffle arr
    )
    arr
)
```


=={{header|Modula-3}}==


```modula3
MODULE Bogo EXPORTS Main;

IMPORT IO, Fmt, Random;

VAR a := ARRAY [1..5] OF INTEGER {1, 2, 3, 4, 5};
    count := 0;

PROCEDURE Shuffle(VAR a: ARRAY OF INTEGER) =
  VAR temp: INTEGER;
  BEGIN
    WITH rand = NEW(Random.Default).init() DO
      FOR i := FIRST(a) TO LAST(a) - 1 DO
        WITH j = rand.integer(i, LAST(a)) DO
          temp := a[i];
          a[i] := a[j];
          a[j] := temp;
        END;
      END;
    END;
  END Shuffle;

PROCEDURE Sorted(VAR a: ARRAY OF INTEGER): BOOLEAN =
  BEGIN
    IF NUMBER(a) <= 1 THEN
      RETURN TRUE;
    END;
    FOR i := FIRST(a) + 1 TO LAST(a) DO
      IF (a[i] < a[i - 1]) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END Sorted;

BEGIN
  Shuffle(a);
  WHILE NOT Sorted(a) DO
    Shuffle(a);
    INC(count);
  END;
  FOR i := FIRST(a) TO LAST(a) DO
    IO.PutInt(a[i]);
    IO.Put(" ");
  END;
  IO.Put("\nRequired " & Fmt.Int(count) & " shuffles\n");
END Bogo.
```



## Nemerle


```Nemerle
using System;
using System.Console;
using Nemerle.Imperative;

module Bogosort
{
    public static Bogosort[T] (this x : array[T]) : void
      where T : IComparable
    {
        def rnd = Random();
        def shuffle(a)
        {
            foreach (i in [0 .. (a.Length - 2)])
            a[i] <-> a[(rnd.Next(i, a.Length))];
        }

        def isSorted(b)
        {
            when (b.Length <= 1) return true;
            foreach (i in [1 .. (b.Length - 1)])
                when (b[i].CompareTo(b[i - 1]) < 0) return false;
            true;
        }

        def loop()
        {
            unless (isSorted(x)) {shuffle(x); loop();};
        }

        loop()
    }

    Main() : void
    {
        def sortme = array[1, 5, 3, 6, 7, 3, 8, -2];
        sortme.Bogosort();
        foreach (i in sortme) Write($"$i  ");
    }
}
```



## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

import java.util.List

method isSorted(list = List) private static returns boolean

  if list.isEmpty then
    return isTrue

  it = list.iterator
  last = Comparable it.next
  loop label i_ while it.hasNext
    current = Comparable it.next
    if last.compareTo(current) > 0 then
      return isFalse
    last = current
    end i_

  return isTrue

method bogoSort(list = List) private static
  loop label s_ while \isSorted(list)
    Collections.shuffle(list)
    end s_

  return

method main(args = String[]) public constant
  samples = [int 31, 41, 59, 26, 53, 58, 97, 93, 23, 84]
  alst = ArrayList(samples.length)
  loop iv = 0 to samples.length - 1
    alst.add(Integer(samples[iv]))
    end iv

  say 'unsorted:' alst.toString
  bogoSort(alst)
  say 'sorted:  ' alst.toString

  return

method isTrue public static returns boolean
  return 1 == 1

method isFalse public static returns boolean
  return \isTrue

```

{{out}}

```txt

unsorted: [31, 41, 59, 26, 53, 58, 97, 93, 23, 84]
sorted:   [23, 26, 31, 41, 53, 58, 59, 84, 93, 97]

```



## Nim


```nim
import math
randomize()

proc shuffle[T](x: var openarray[T]) =
  for i in countdown(x.high, 0):
    let j = random(i + 1)
    swap(x[i], x[j])

proc isSorted[T](s: openarray[T]): bool =
  var last = low(T)
  for c in s:
    if c < last:
      return false
    last = c
  return true

proc bogoSort[T](a: var openarray[T]) =
  while not isSorted a: shuffle a

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
bogoSort a
echo a
```

{{out}}

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```


=={{header|Oberon-2}}==
{{Works with|Oxford Oberon-2 Compiler}}

```oberon2
MODULE Bogo;

   IMPORT Out, Random;

   VAR a: ARRAY 10 OF INTEGER;

   PROCEDURE Init;
      VAR i: INTEGER;
   BEGIN
      FOR i := 0 TO LEN(a) - 1 DO
         a[i] := i + 1;
      END;
   END Init;

   PROCEDURE Sorted(VAR a: ARRAY OF INTEGER): BOOLEAN;
      VAR i: INTEGER;
   BEGIN
      IF LEN(a) <= 1 THEN
         RETURN TRUE;
      END;
      FOR i := 1 TO LEN(a) - 1 DO
         IF (a[i] < a[i - 1]) THEN
            RETURN FALSE;
         END;
      END;
      RETURN TRUE;
   END Sorted;

   PROCEDURE Shuffle*(VAR a: ARRAY OF INTEGER);
      VAR n, t, r: INTEGER;
   BEGIN
      FOR n := 0 TO LEN(a) - 1 DO
         r := Random.Roll(n);
         t := a[n];
         a[n] := a[r];
         a[r] := t;
      END;
   END Shuffle;

BEGIN
   Init;
   Shuffle(a);
   WHILE ~Sorted(a) DO
      Shuffle(a);
   END;
   FOR i := 0 TO LEN(a) - 1 DO
      Out.Int(a[i], 0);
      Out.String(" ");
   END;
   Out.Ln;
END Bogo.
```


Init initializes the array as 1 thru 10, then it is shuffled, and then the while loop continually shuffles until Sorted returns true.


## OCaml


```ocaml
let rec is_sorted comp = function
 | e1 :: e2 :: r -> comp e1 e2 <= 0 && is_sorted comp (e2 :: r)
 | _             -> true

(* Fisher-Yates shuffle on lists; uses temp array *)
let shuffle l =
  let ar = Array.of_list l in
    for n = Array.length ar - 1 downto 1 do
      let k = Random.int (n+1) in
      let temp = ar.(k) in (* swap ar.(k) and ar.(n) *)
        ar.(k) <- ar.(n);
        ar.(n) <- temp
    done;
    Array.to_list ar

let rec bogosort li =
  if is_sorted compare li then
    li
  else
    bogosort (shuffle li)
```

Example:

```txt

# bogosort [7;5;12;1;4;2;23;18] ;;
- : int list = [1; 2; 4; 5; 7; 12; 18; 23]

```



## Oz

We use an array because that made most sense for the Knuth Shuffle task. Usually you would use lists for stuff like this in Oz.


```oz
declare
  proc {BogoSort Arr}
     for while:{Not {InOrder Arr}} do
        {Shuffle Arr}
     end
  end

  fun {InOrder Arr}
     for I in {Array.low Arr}+1..{Array.high Arr}
	return:Return default:true
     do
        if Arr.(I-1) > Arr.I then {Return false} end
     end
  end

  proc {Shuffle Arr}
     Low = {Array.low Arr}
     High = {Array.high Arr}
  in
     for I in High..Low;~1 do
	J = Low + {OS.rand} mod (I - Low + 1)
        OldI = Arr.I
     in
	Arr.I := Arr.J
        Arr.J := OldI
     end
  end

  X = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {BogoSort X}
  {Show {Array.toRecord unit X}}
```



## PARI/GP

This implementation sorts 9 distinct elements in only 600 milliseconds.

```parigp
bogosort(v)={
  while(1,
    my(u=vecextract(v,numtoperm(#v,random((#v)!))));
    for(i=2,#v,if(u[i]<u[i-1], next(2)));
    return(u)
  );
};
```



## Pascal



```Pascal
program bogosort;

const
  max = 5;
type
  list = array [1..max] of integer;

{ Print a list }
procedure printa(a: list);
var
  i: integer;
begin
  for i := 1 to max do
    write(a[i], ' ');
  writeln
end;

{ Knuth shuffle }
procedure shuffle(var a: list);
var
  i,k,tmp: integer;
begin
  for i := max downto 2 do begin
     k := random(i) + 1;
     if (a[i] <> a[k]) then begin
       tmp := a[i]; a[i] := a[k]; a[k] := tmp
     end
  end
end;

{ Check for sorted list }
function sorted(a: list): boolean;
var
  i: integer;
begin
  sorted := True;
  for i := 2 to max do
    if (a[i - 1] > a[i]) then begin
      sorted := False; exit
    end
end;

{ Bogosort }
procedure bogo(var a: list);
var
  i: integer;
begin
  i := 1; randomize;
  write(i,': '); printa(a);
  while not sorted(a) do begin
    shuffle(a);
    i := i + 1; write(i,': '); printa(a)
  end
end;

{ Test and display }
var
  a: list;
  i: integer;

begin
  for i := 1 to max do
    a[i] := (max + 1) - i;
  bogo(a);
end.
```


{{out}}

```txt
1: 5 4 3 2 1
2: 3 5 4 1 2
. . . . . .
22: 3 2 1 5 4
23: 1 2 3 4 5
```



## Perl


```perl
use List::Util qw(shuffle);

sub bogosort
 {my @l = @_;
  @l = shuffle(@l) until in_order(@l);
  return @l;}

sub in_order
 {my $last = shift;
  foreach (@_)
     {$_ >= $last or return 0;
      $last = $_;}
  return 1;}
```



## Perl 6


```perl6
sub bogosort (@list is copy) {
    @list .= pick(*) until [<=] @list;
    return @list;
}

my @nums = (^5).map: { rand };
say @nums.sort.Str eq @nums.&bogosort.Str ?? 'ok' !! 'not ok';

```



## Phix


```Phix
function inOrder(sequence s)
    return s==sort(s)   -- <snigger>
end function

function bogosort(sequence s)
    while not inOrder(s) do
        ? s
        s = shuffle(s)
    end while
    return s
end function

? bogosort(shuffle({1,2,3,4,5,6}))
```

{{out}}

```txt

...
{4,3,1,5,2,6}
{1,3,4,6,5,2}
{2,3,4,1,5,6}
{1,2,3,4,5,6}

```



## PHP


```php
function bogosort($l) {
    while (!in_order($l))
        shuffle($l);
    return $l;
}

function in_order($l) {
    for ($i = 1; $i < count($l); $i++)
        if ($l[$i] < $l[$i-1])
            return FALSE;
    return TRUE;
}
```



## PicoLisp


```PicoLisp
(de bogosort (Lst)
   (loop
      (map
         '((L) (rot L (rand 1 (length L))))
         Lst )
      (T (apply <= Lst) Lst) ) )
```

{{out}}

```txt
: (bogosort (make (do 9 (link (rand 1 999)))))
-> (1 167 183 282 524 556 638 891 902)

: (bogosort (make (do 9 (link (rand 1 999)))))
-> (20 51 117 229 671 848 883 948 978)

: (bogosort (make (do 9 (link (rand 1 999)))))
-> (1 21 72 263 391 476 794 840 878)
```



## PL/I

{{trans|REXX}}

```pli
*process source xref;
 bogosort: Proc Options(main);
 Dcl SYSPRINT Print;
 Dcl (HBOUND,RANDOM,TIME) Builtin;
 Dcl tim Pic'(9)9';
 Dcl timms Pic'(3)9' def tim pos(7);
 tim=time();
 x=random(timms);
 Dcl a(5)       Dec Fixed(5,1) Init(-21,333,0,444.4,1);
 Dcl (x,y,temp) Dec Fixed(5,1);
 Dcl (n,bogo,j,u,v) Bin Fixed(31);
 n=hbound(a);
 Call tell('un-bogoed');
 loop:
 Do bogo=1 By 1;
   Do j=1 To n-1;
     jp=j+1;
     x=a(j);
     y=a(jp);
     if y>=x Then
       Iterate;
     u=rand(1,n);
     Do Until v^=u
       v=rand(1,n);
       End;
     Temp=a(u);
     a(u)=a(v);
     a(v)=temp;
     Iterate loop;
     End;
   Leave;
   End;

 Put Edit('number of bogo sorts performed =',bogo)(Skip,a,f(4));
 call tell('   bogoed');
 Return;

 tell: Proc(txt);
 Dcl txt Char(*);
 Dcl t Bin Fixed(31);
 Put Edit(txt)(skip,a);
 Do t=1 to n;
   Put Edit(a(t))(Skip,f(6,1));
   End;
 End;

 rand: Proc(lo,hi) Returns(Bin Fixed(31));
 Dcl (lo,hi,res) Bin Fixed(31);
 Dcl r Bin Float(31);
 r=random();
 res=r*(hi-lo+1)+lo;
 Return(res);
 End;
 End;
```

{{out}}

```txt
un-bogoed
 -21.0
 333.0
   0.0
 444.4
   1.0
number of bogo sorts performed =   8
   bogoed
 -21.0
   0.0
   1.0
 333.0
 444.4
```



## PowerShell

Shuffle taken from [[Knuth Shuffle]]

```PowerShell
function shuffle ($a) {
    $c = $a.Clone()  # make copy to avoid clobbering $a
    1..($c.Length - 1) | ForEach-Object {
        $i = Get-Random -Minimum $_ -Maximum $c.Length
        $c[$_-1],$c[$i] = $c[$i],$c[$_-1]
        $c[$_-1]  # return newly-shuffled value
    }
    $c[-1]  # last value
}

function isSorted( [Array] $data )
{
	$sorted = $true
	for( $i = 1; ( $i -lt $data.length ) -and $sorted; $i++ )
	{
		$sorted = $data[ $i - 1 ] -le $data[ $i ]
	}
	$sorted
}

function BogoSort ( [Array] $indata ) {
	$data = $indata.Clone()
	while( -not ( isSorted $data ) ) {
		$data = shuffle $indata
	}
	$data
}

$l = 7; BogoSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
```



## PureBasic


```PureBasic
Procedure KnuthShuffle (Array a(1))
  Protected i, Size = ArraySize(a())
  For i = 0 To Size
    Swap a(i), a(Random(Size))
  Next
EndProcedure

Procedure isSorted(Array a(1))
  Protected i, Size = ArraySize(a())
  For i = 1 To Size
    If a(i) < a(i - 1)
      ProcedureReturn #False
    EndIf
  Next
  ProcedureReturn #True
EndProcedure

Procedure BogoSort(Array a(1))
  Protected Size = ArraySize(a()) + 1, iter

  While Not isSorted(a())
    iter + 1
    KnuthShuffle(a())
  Wend
  MessageRequester("Results","Array of " + Str(Size) + " integers required " + Str(iter) + " shuffles To SORT.")
EndProcedure

Dim b(10)
For i = 0 To 10
  b(i) = Random(100)
Next

BogoSort(b())
```

{{out}}

```txt
Array of 10 integers required 2766901 shuffles To SORT.
```



## Python


```python
import random

def bogosort(l):
    while not in_order(l):
        random.shuffle(l)
    return l

def in_order(l):
    if not l:
        return True
    last = l[0]
    for x in l[1:]:
        if x < last:
            return False
        last = x
    return True
```


Alternative definition for ''in_order'' (Python 2.5)

```python
def in_order(l):
    return all( l[i] <= l[i+1] for i in xrange(0,len(l)-1))
```


An alternative implementation for Python 2.5 or later:

```python
import random
def bogosort(lst):
   random.shuffle(lst)  # must shuffle it first or it's a bug if lst was pre-sorted! :)
   while lst != sorted(lst):
       random.shuffle(lst)
   return lst
```


Another alternative implementation, using iterators for maximum efficiency:


```python
import operator
import random
from itertools import dropwhile, imap, islice, izip, repeat, starmap

def shuffled(x):
    x = x[:]
    random.shuffle(x)
    return x

bogosort = lambda l: next(dropwhile(
    lambda l: not all(starmap(operator.le, izip(l, islice(l, 1, None)))),
    imap(shuffled, repeat(l))))
```



## Qi


```Qi

(define remove-element
  0   [_ | R] -> R
  Pos [A | R] -> [A | (remove-element (1- Pos) R)])

(define get-element
  Pos R -> (nth (1+ Pos) R))

(define shuffle-0
  Pos R -> [(get-element Pos R) | (shuffle (remove-element Pos R))])

(define shuffle
  [] -> []
  R  -> (shuffle-0 (RANDOM (length R)) R))

(define in-order?
  []        -> true
  [A]       -> true
  [A B | R] -> (in-order? [B | R]) where (<= A B)
  _         -> false)

(define bogosort
  Suggestion -> Suggestion where (in-order? Suggestion)
  Suggestion -> (bogosort (shuffle Suggestion)))

```



## R


```R
bogosort <- function(x) {
   while(is.unsorted(x)) x <- sample(x)
   x
}

n <- c(1, 10, 9, 7, 3, 0)
bogosort(n)
```



## Racket


Only the first line is needed to implement the bogo sort, the rest
is unit tests and an example.


```racket

#lang racket
(define (bogo-sort l) (if (apply <= l) l (bogo-sort (shuffle l))))

(require rackunit)
(check-equal? (bogo-sort '(6 5 4 3 2 1)) '(1 2 3 4 5 6))
(check-equal? (bogo-sort (shuffle '(1 1 1 2 2 2))) '(1 1 1 2 2 2))

(let ((unsorted (for/list ((i 10)) (random 1000))))
  (displayln unsorted)
  (displayln (bogo-sort unsorted)))

```


{{out}} (chances are you won't get quite this!):

```txt

(703 931 12 713 894 232 778 86 700 26)
(12 26 86 232 700 703 713 778 894 931)

```



## REXX


### true bogo sort


```rexx
/*REXX program performs a type of  bogo sort  on  numbers in an array.  */
parse arg list                         /*obtain optional list from C.L. */
if list=''  then list=-21 333 0 444.4  /*Not defined?  Then use default.*/
#=words(list)                          /*the number of numbers in list. */
   do i=1  for words(list);  @.i=word(list,i);  end   /*create an array.*/
call tell 'before bogo sort'

  do bogo=1

    do j=1  for #-1;   jp=j+1          /* [↓]  compare a # with the next*/
    if @.jp>=@.j  then iterate         /*so far, so good;  keep looking.*/
                                       /*get 2 unique random #s for swap*/
       do  until a\==b;  a=random(1, #);     b=random(1, #);    end

    parse value @.a @.b  with  @.b @.a /*swap 2 random numbers in array.*/
    iterate bogo                       /*go and try another bogo sort.  */
    end     /*j*/

  leave                                /*we're finished with bogo sort. */
  end       /*bogo*/                   /* [↓]  show the # of bogo sorts.*/

say 'number of bogo sorts performed =' bogo
call tell ' after bogo sort'
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────TELL subroutine─────────────────────*/
tell:  say;  say center(arg(1), 50, '─')
                 do t=1  for #
                 say arg(1)  'element'right(t, length(#))'='right(@.t, 18)
                 end   /*t*/
say
return
```

{{out}} using the default input:

```txt

─────────────────before bogo sort─────────────────
before bogo sort element   1=               -21
before bogo sort element   2=               333
before bogo sort element   3=                 0
before bogo sort element   4=             444.4

number of bogo sorts performed = 6

───────────────── after bogo sort─────────────────
 after bogo sort element   1=               -21
 after bogo sort element   2=                 0
 after bogo sort element   3=               333
 after bogo sort element   4=             444.4

```



### modified bogo sort


When a number is found out of order, two random numbers between the first number's position and

the position of the last number checked are swapped (in other words, swap two numbers within what

has already been sorted and including the number out-of-order.   The search then starts over.

This is repeated as often as it takes to finally get the array in order.

```rexx
/*REXX program performs a type of  bogo sort  on numbers in an array.   */
@.1 =   0  ;     @.11=    -64  ;     @.21=     4096  ;    @.31=    6291456
@.2 =   0  ;     @.12=     64  ;     @.22=    40960  ;    @.32=    5242880
@.3 =   1  ;     @.13=    256  ;     @.23=    16384  ;    @.33=  -15728640
@.4 =   2  ;     @.14=      0  ;     @.24=  -114688  ;    @.34=  -27262976
@.5 =   0  ;     @.15=   -768  ;     @.25=  -131072  ;    @.35=   29360128
@.6 =  -4  ;     @.16=   -512  ;     @.26=   262144  ;    @.36=  104857600
@.7 =   0  ;     @.17=   2048  ;     @.27=   589824  ;    @.37=  -16777216
@.8 =  16  ;     @.18=   3072  ;     @.28=  -393216  ;    @.38= -335544320
@.9 =  16  ;     @.19=  -4096  ;     @.29= -2097152  ;    @.39= -184549376
@.10= -32  ;     @.20= -12288  ;     @.30=  -262144  ;    @.40=  905969664
                          /* [↑]   @.1  is really the 0th Berstel number*/
#=40                      /*we have a list of two score Berstel numbers.*/
call tell 'before bogo sort'

  do bogo=1

    do j=1  for #;   ?=@.j             /*?  is the next number in array.*/

      do k=j+1  to #
      if @.k>=?  then iterate          /*is this # in order?  Get next. */
                                       /*get 2 unique random #s for swap*/
         do  until a\==b;  a=random(j, k);     b=random(j, k);    end

      parse value @.a @.b  with  @.b @.a    /*swap 2 random #s in array.*/
      iterate bogo                     /*go and try another bogo sort.  */
      end   /*k*/
    end     /*j*/

  leave                                /*we're finished with bogo sort. */
  end       /*bogo*/                   /* [↓]  show the # of bogo sorts.*/

say 'number of bogo sorts performed =' bogo
call tell ' after bogo sort'
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────TELL subroutine─────────────────────*/
tell:  say;  say center(arg(1), 50, '─')
                 do t=1  for #
                 say arg(1)  'element'right(t, length(#))'='right(@.t, 18)
                 end   /*t*/
say
return
```

{{out}}
<pre style="height:30ex">
─────────────────before bogo sort─────────────────
before bogo sort element 1=                 0
before bogo sort element 2=                 0
before bogo sort element 3=                 1
before bogo sort element 4=                 2
before bogo sort element 5=                 0
before bogo sort element 6=                -4
before bogo sort element 7=                 0
before bogo sort element 8=                16
before bogo sort element 9=                16
before bogo sort element10=               -32
before bogo sort element11=               -64
before bogo sort element12=                64
before bogo sort element13=               256
before bogo sort element14=                 0
before bogo sort element15=              -768
before bogo sort element16=              -512
before bogo sort element17=              2048
before bogo sort element18=              3072
before bogo sort element19=             -4096
before bogo sort element20=            -12288
before bogo sort element21=              4096
before bogo sort element22=             40960
before bogo sort element23=             16384
before bogo sort element24=           -114688
before bogo sort element25=           -131072
before bogo sort element26=            262144
before bogo sort element27=            589824
before bogo sort element28=           -393216
before bogo sort element29=          -2097152
before bogo sort element30=           -262144
before bogo sort element31=           6291456
before bogo sort element32=           5242880
before bogo sort element33=         -15728640
before bogo sort element34=         -27262976
before bogo sort element35=          29360128
before bogo sort element36=         104857600
before bogo sort element37=         -16777216
before bogo sort element38=        -335544320
before bogo sort element39=        -184549376
before bogo sort element40=         905969664

number of bogo sorts performed = 1891

───────────────── after bogo sort─────────────────
 after bogo sort element 1=        -335544320
 after bogo sort element 2=        -184549376
 after bogo sort element 3=         -27262976
 after bogo sort element 4=         -16777216
 after bogo sort element 5=         -15728640
 after bogo sort element 6=          -2097152
 after bogo sort element 7=           -393216
 after bogo sort element 8=           -262144
 after bogo sort element 9=           -131072
 after bogo sort element10=           -114688
 after bogo sort element11=            -12288
 after bogo sort element12=             -4096
 after bogo sort element13=              -768
 after bogo sort element14=              -512
 after bogo sort element15=               -64
 after bogo sort element16=               -32
 after bogo sort element17=                -4
 after bogo sort element18=                 0
 after bogo sort element19=                 0
 after bogo sort element20=                 0
 after bogo sort element21=                 0
 after bogo sort element22=                 0
 after bogo sort element23=                 1
 after bogo sort element24=                 2
 after bogo sort element25=                16
 after bogo sort element26=                16
 after bogo sort element27=                64
 after bogo sort element28=               256
 after bogo sort element29=              2048
 after bogo sort element30=              3072
 after bogo sort element31=              4096
 after bogo sort element32=             16384
 after bogo sort element33=             40960
 after bogo sort element34=            262144
 after bogo sort element35=            589824
 after bogo sort element36=           5242880
 after bogo sort element37=           6291456
 after bogo sort element38=          29360128
 after bogo sort element39=         104857600
 after bogo sort element40=         905969664

```

More tests showed that:

```txt

number of bogo sorts performed = 2583
number of bogo sorts performed = 2376
number of bogo sorts performed = 1791
number of bogo sorts performed = 2537
number of bogo sorts performed = 1856
number of bogo sorts performed = 2339
number of bogo sorts performed = 2511
number of bogo sorts performed = 2652
number of bogo sorts performed = 1697
number of bogo sorts performed = 1782
number of bogo sorts performed = 2074
number of bogo sorts performed = 4017
number of bogo sorts performed = 2469
number of bogo sorts performed = 3707
number of bogo sorts performed = 1729
number of bogo sorts performed = 1705
number of bogo sorts performed = 4071

```



## Ring


```ring

# Project : Sorting algorithms/Bogosort

test = [4, 65, 2, 31, 0, 99, 2, 83, 782, 1]
shuffles = 0
while ! sorted(test)
        shuffles = shuffles + 1
        shuffle(test)
end
see "" + shuffles + " shuffles required to sort " + len(test)  + " items:" + nl
showarray(test)

func shuffle(d)
        for i = len(d) to 2 step -1
             item = random(i) + 1
             if item <= len(d)
                temp = d[i-1]
                d[i-1] = d[item]
                d[item] = temp
             else
                i = i -1
             ok
next

func sorted(d)
        for j = 2 to len(d)
             if d[j] < d[j-1]
                return false
             ok
        next
        return true

func showarray(vect)
        see "["
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + ", "
        next
        svect = left(svect, len(svect) - 2)
        see svect
        see "]" + nl

```

Output:

```txt

508888 shuffles required to sort 10 items:
[0, 1, 2, 2, 4, 31, 65, 83, 99, 782]

```



## Ruby


```ruby
def shuffle(l)
    l.sort_by { rand }
end

def bogosort(l)
    l = shuffle(l) until in_order(l)
    l
end

def in_order(l)
    (0..l.length-2).all? {|i| l[i] <= l[i+1] }
end
```


An alternative implementation:


```ruby
def shuffle(l)
    l.sort_by { rand }
end

def bogosort(l)
   l = shuffle(l) until l == l.sort
   l
end
```


{{works with|Ruby|1.8.7+}}


```ruby
def in_order(l)
    (0..l.length-2).all? {|i| l[i] <= l[i+1] }
end

def bogosort(l)
   l.shuffle! until in_order(l)
   l
end
```



## Rust

Works with Rust 1.11+, requires rand module
{{libheader|rand}}

```rust
extern crate rand;
use rand::Rng;

fn bogosort_by<T,F>(order: F, coll: &mut [T])
    where F: Fn(&T, &T) -> bool
{
    let mut rng = rand::thread_rng();
    while !is_sorted_by(&order, coll) {
        rng.shuffle(coll);
    }
}

#[inline]
fn is_sorted_by<T,F>(order: F, coll: &[T]) -> bool
    where F: Fn(&T,&T) -> bool,
{
    coll[..].iter().zip(&coll[1..]).all(|(x,y)| order(x,y))
}


fn main() {
    let mut testlist = [1,55,88,24,990876,312,67,0,854,13,4,7];
    bogosort_by(|x,y| x < y, &mut testlist);
    println!("{:?}", testlist);
    bogosort_by(|x,y| x > y, &mut testlist);
    println!("{:?}", testlist);
}

```



## Scala

{{works with|Scala|2.8}}

```scala
def isSorted(l: List[Int]) = l.iterator sliding 2 forall (s => s.head <= s.last)
def bogosort(l: List[Int]): List[Int] = if (isSorted(l)) l else bogosort(scala.util.Random.shuffle(l))
```



## Sidef


```ruby
func in_order(a) {
    return true if (a.len <= 1);
    var first = a[0];
    a.ft(1).all { |elem| first <= elem  ? do { first = elem; true } : false }
}

func bogosort(a) {
    a.shuffle! while !in_order(a);
    return a;
}

var arr = 5.of{ 100.rand.int };
say "Before: #{arr}";
say "After:  #{bogosort(arr)}";
```

{{out}}

```txt

Before: 57 45 83 85 33
After:  33 45 57 83 85

```



## Smalltalk

{{works with|GNU Smalltalk}}

This implementation uses closures rather than extending collections to provide a bogosort method.

```smalltalk
Smalltalk at: #isItSorted put: [ :c |
  |isit|
  isit := false.
  (2 to: (c size)) detect: [ :i |
    ( (c at: ( i - 1 )) > (c at: i) )
  ] ifNone: [ isit := true ].
  isit
].
Smalltalk at: #bogosort put: [ :c |
  [ isItSorted value: c ] whileFalse: [
     1 to: (c size) do: [ :i |
        |r t|
        r := (Random between: 1 and: (c size)).
        t := (c at: i).
        c at: i put: (c at: r).
        c at: r put: t
     ]
  ]
].

|tobesorted|
tobesorted := { 2 . 7 . 5 . 3 . 4 . 8 . 6 . 1 }.
bogosort value: tobesorted.
tobesorted displayNl.
```



## SNOBOL4



```SNOBOL4
* Library for random()
-include 'Random.sno'

*       # String -> array
        define('s2a(str,n)i') :(s2a_end)
s2a     s2a = array(n); str = str ' '
sa1     str break(' ') . s2a<i = i + 1> span(' ') = :s(sa1)f(return)
s2a_end

*       # Array -> string
        define('a2s(a)i') :(a2s_end)
a2s     a2s = a2s a<i = i + 1> ' ' :s(a2s)f(return)
a2s_end

*       # Knuth shuffle in-place
        define('shuffle(a)alen,n,k,tmp') :(shuffle_end)
shuffle n = alen = prototype(a);
sh1     k = convert(random() * alen,'integer') + 1
        eq(a<n>,a<k>) :s(sh2)
        tmp = a<n>; a<n> = a<k>; a<k> = tmp
sh2     n = gt(n,1) n - 1 :s(sh1)
        shuffle = a :(return)
shuffle_end

*       # sorted( ) predicate -> Succeed/Fail
        define('sorted(a)alen,i') :(sorted_end)
sorted  alen = prototype(a); i = 1
std1    i = lt(i,alen) i + 1 :f(return)
        gt(a<i - 1>,a<i>) :s(freturn)f(std1)
sorted_end

*       # Bogosort
        define('bogo(a)') :(bogo_end)
bogo    output = (i = i + 1) ': ' a2s(a)
        bogo = sorted(a) a :s(return)
        shuffle(a) :(bogo)
bogo_end

*       # Test and display
        bogo(s2a('5 4 3 2 1',5))
end
```


{{out}}

```txt
1: 5 4 3 2 1
2: 2 1 4 3 5
. . . . . .
117: 3 2 1 5 4
118: 1 2 3 4 5
```



## Swift


```swift
import Darwin

func shuffle<T>(inout array: [T]) {
  for i in 1..<array.count {
    let j = Int(arc4random_uniform(UInt32(i)))
    (array[i], array[j]) = (array[j], array[i])
  }
}

func issorted<T:Comparable>(ary: [T]) -> Bool {
  for i in 0..<(ary.count-1) {
    if ary[i] > ary[i+1] {
      return false
    }
  }
  return true
}

func bogosort<T:Comparable>(inout ary: [T]) {
  while !issorted(ary) {
    shuffle(&ary)
  }
}
```



## Tcl


```tcl
package require Tcl 8.5

proc shuffleInPlace {listName} {
    upvar 1 $listName list
    set len [set len2 [llength $list]]
    for {set i 0} {$i < $len-1} {incr i; incr len2 -1} {
        # Pick cell to swap with
        set n [expr {int($i + $len2 * rand())}]
        # Perform swap
        set temp [lindex $list $i]
        lset list $i [lindex $list $n]
        lset list $n $temp
    }
}
proc inOrder {list} {
    set prev [lindex $list 0]
    foreach item [lrange $list 1 end] {
        if {$prev > $item} {
            return false
        }
        set prev $item
    }
    return true
}
proc bogosort {list} {
    while { ! [inOrder $list]} {
        shuffleInPlace list
    }
    return $list
}
```


=={{header|TI-83 BASIC}}==
Same IO as BozoSort (below).
 :"BOGO"
 :L<sub>1</sub>→L<sub>2</sub>
 :Lbl A
 :dim(L<sub>2</sub>)→A
 :For(B,1,dim(L<sub>2</sub>)-1)
 :randInt(1,A)→C
 :L<sub>2</sub>(C)→D
 :L<sub>2</sub>(A)→L<sub>2</sub>(C)
 :D→L<sub>2</sub>(A)
 :A-1→A
 :End
 :For(D,1,dim(L<sub>2</sub>)-1)
 :If L<sub>2</sub>(D)>L<sub>2</sub>(D+1)
 :Goto A
 :End
 :DelVar A
 :DelVar B
 :DelVar C
 :DelVar D
 :Return

This isn't a bogosort, but a bozosort. Store input into L<sub>1</sub>, run prgmSORTBOZO, outputs to L<sub>2</sub>
 :L<sub>1</sub>→L<sub>2</sub>
 :Lbl T
 :0→B
 :For(A,1,dim(L<sub>2</sub>)-1)
 :If L<sub>2</sub>(A)>L<sub>2</sub>(A+1)
 :1→B
 :End
 :If B=0
 :Goto E
 :randInt(1,dim(L<sub>2</sub>))→C
 :randInt(1,dim(L<sub>2</sub>))→D
 :L<sub>2</sub>(C)→E
 :L<sub>2</sub>(C+1)→L<sub>2</sub>(C)
 :E→L<sub>2</sub>(C+1)
 :Goto T
 :Lbl E
 :DelVar A
 :DelVar B
 :DelVar C
 :DelVar D
 :DelVar E
 :Stop


## Ursala



```Ursala
#import std
#import nat

shuffle = @iNX ~&l->r ^jrX/~&l ~&lK8PrC

bogosort = (not ordered nleq)-> shuffle

#cast %nL

example = bogosort <8,50,0,12,47,51>
```

{{out}}

```txt
<0,8,12,47,50,51>
```



## VBA

{{trans|Phix}}
```vb
Private Function Knuth(a As Variant) As Variant
    Dim t As Variant, i As Integer
    If Not IsMissing(a) Then
        For i = UBound(a) To LBound(a) + 1 Step -1
            j = Int((UBound(a) - LBound(a) + 1) * Rnd + LBound(a))
            t = a(i)
            a(i) = a(j)
            a(j) = t
        Next i
    End If
    Knuth = a
End Function

Private Function inOrder(s As Variant)
    i = 2
    Do While i <= UBound(s)
         If s(i) < s(i - 1) Then
            inOrder = False
            Exit Function
        End If
        i = i + 1
    Loop
    inOrder = True
End Function

Private Function bogosort(ByVal s As Variant) As Variant
    Do While Not inOrder(s)
        Debug.Print Join(s, ", ")
        s = Knuth(s)
    Loop
    bogosort = s
End Function

Public Sub main()
    Debug.Print Join(bogosort(Knuth([{1,2,3,4,5,6}])), ", ")
End Sub
```


```txt
...
1, 3, 2, 5, 6, 4
6, 2, 1, 3, 4, 5
2, 6, 5, 4, 1, 3
2, 6, 3, 4, 1, 5
1, 2, 3, 4, 5, 6
```


## VBScript


### ==Implementation==


```vb
sub swap( byref a, byref b )
	dim tmp
	tmp = a
	a = b
	b = tmp
end sub

'knuth shuffle (I think)
function shuffle( a )
	dim i
	dim r
	randomize timer
	for i = lbound( a ) to ubound( a )
		r = int( rnd * ( ubound( a ) + 1 )  )
		if r <> i then
			swap a(i), a(r)
		end if
	next
	shuffle = a
end function

function inOrder( a )
	dim res
	dim i
	for i = 0 to ubound( a ) - 1
		res = ( a(i) <= a(i+1) )
		if res = false then exit for
	next
	inOrder = res
end function
```



### ==Invocation==


```vb
dim a
a = array(11, 1, 2, 3, 4, 4, 6, 7, 8)

dim t
t = timer
while not inorder( a )
	shuffle a
wend
wscript.echo timer-t, "seconds"
wscript.echo join( a, ", " )
```


=====A few outputs (timed)=====

```txt

10.34766 seconds
1, 2, 3, 4, 4, 6, 7, 8, 11

0.5039063 seconds
1, 2, 3, 4, 4, 6, 7, 8, 11

1.980469 seconds
1, 2, 3, 4, 4, 6, 7, 8, 11

```



## XPL0


```XPL0
code Ran=1, ChOut=8, IntOut=11;

proc BogoSort(A, L);    \Sort array A of length L
int  A, L;
int  I, J, T;
[loop   [I:= 0;
        loop    [if A(I) > A(I+1) then quit;
                I:= I+1;
                if I >= L-1 then return;
                ];
        I:= Ran(L);  J:= Ran(L);
        T:= A(I);  A(I):= A(J);  A(J):= T;
        ];
];

int A, I;
[A:= [3, 1, 4, 1, -5, 9, 2, 6, 5, 4];
BogoSort(A, 10);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
]
```


{{out}}

```txt

-5 1 1 2 3 4 4 5 6 9

```



## zkl


```zkl
fcn increasing(list){
   list.len()<2 or
   list.reduce(fcn(a,b){ if(b<a) return(Void.Stop,False); b }).toBool()
}

ns:=L(5,23,1,6,123,7,23);
while(not increasing(ns)){ ns=ns.shuffle() }
ns.println();
```

{{out}}

```txt
L(1,5,6,7,23,23,123)
```


{{omit from|GUISS}}

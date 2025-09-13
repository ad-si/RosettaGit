+++
title = "Sattolo cycle"
description = ""
date = 2019-07-02T14:56:06Z
aliases = []
[extra]
id = 21083
[taxonomies]
categories = ["task"]
tags = []
+++

The   [[wp:Sattolo's algorithm|Sattolo cycle]]   is an algorithm for randomly shuffling an array in such a way that each element ends up in a new position.

Implement the Sattolo cycle for an integer array (or, if possible, an array of any type).

Given an array '''''items''''' with indices ranging from ''0'' to '''''last''''', the algorithm can be defined as follows (pseudo-code):

 '''for''' ''i'' '''from''' ''last'' '''downto''' 1 '''do''':
     '''let''' ''j'' = random integer in range ''0'' <math>\leq</math> ''j'' < ''i''
     '''swap''' ''items''[''i''] '''with''' ''items''[''j'']

Notes:
* It modifies the input array in-place. If that is unreasonable in your programming language, you may amend the algorithm to return the shuffled items as a new array instead.
* The algorithm can also be amended to iterate from left to right, if that is more convenient.
* The only difference between this and the Knuth shuffle, is that <math>j</math> is chosen from the range ''0'' <math>\leq</math> ''j'' < ''i'', rather than ''0'' <math>\leq</math> ''j'' <math>\leq</math> ''i''. This is what ensures that every element ends up in a new position, as long as there are at least two elements.

{| class="wikitable"
|-
! Input array
! Possible output arrays
|-
| <tt>[]</tt>
| <tt>[]</tt>
|-
| <tt>[10]</tt>
| <tt>[10]</tt>
|-
| <tt>[10, 20]</tt>
| <tt>[20, 10]</tt>
|-
| <tt>[10, 20, 30]</tt>
| <tt>[20, 30, 10]</tt>
<tt>[30, 10, 20]</tt>
|-
| <tt style="white-space:nowrap">[11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]</tt>
| ''39,916,800 possibilities. You'll know you have a correct one if it has the same elements as the input array, but none in their original place.''
|}

* [[Knuth shuffle]]

<hr>


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program sattolo.s   */

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
.equ NBELEMENTS,      9
TableNumber:	     .int   4,6,7,10,11,15,22,30,35

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
    ldr r0,iAdrTableNumber                      @ address number table
    mov r1,#NBELEMENTS                          @ number of élements
    bl satShuffle
    ldr r2,iAdrTableNumber
    mov r3,#0
1:                                              @ loop display table
	ldr r0,[r2,r3,lsl #2]
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message
    add r3,#1
    cmp r3,#NBELEMENTS - 1
    ble 1b

    ldr r0,iAdrszCarriageReturn
    bl affichageMess
    /*    2e shuffle             */
    ldr r0,iAdrTableNumber                     @ address number table
    mov r1,#NBELEMENTS                         @ number of élements
    bl satShuffle
    ldr r2,iAdrTableNumber
    mov r3,#0
2:                                             @ loop display table
    ldr r0,[r2,r3,lsl #2]
    ldr r1,iAdrsMessValeur                     @ display value
    bl conversion10                            @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                           @ display message
    add r3,#1
    cmp r3,#NBELEMENTS - 1
    ble 2b

100:                                           @ standard end of the program
    mov r0, #0                                 @ return code
    mov r7, #EXIT                              @ request to exit program
    svc #0                                     @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrTableNumber:          .int TableNumber

/******************************************************************/
/*     Sattolo Shuffle                                  */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the number of elements */
satShuffle:
    push {r2-r6,lr}                                    @ save registers
    mov r5,r0                                          @ save table address
    mov r2,#1                                          @ start index
    mov r4,r1                                          @ last index + 1
1:
    sub r1,r2,#1                                       @ index - 1
    mov r0,r1                                          @ generate aleas
    bl genereraleas
    ldr r3,[r5,r1,lsl #2]                              @ swap number on the table
    ldr r6,[r5,r0,lsl #2]
    str r6,[r5,r1,lsl #2]
    str r3,[r5,r0,lsl #2]
    add r2,#1                                           @ next number
    cmp r2,r4                                           @ end ?
    ble 1b                                              @ no -> loop

100:
    pop {r2-r6,lr}
    bx lr                                               @ return

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
    bl divisionpar10U                               @unsigned  r0 <- dividende. quotient ->r0 reste -> r1
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
    mov r0,r4                                       @ result length
    mov r1,#' '                                     @ space
3:
    strb r1,[r3,r4]                                 @ store space in area
    add r4,#1                                       @ next position
    cmp r4,#LGZONECAL
    ble 3b                                          @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                  @ restaur registres
    bx lr                                           @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                       @ save value
    //mov r3,#0xCCCD                                @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                               @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                             @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                            @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                              @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                            @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                            @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                           @ leave function
iMagicNumber:  	.int 0xCCCCCCCD
/***************************************************/
/*   Generation random number                  */
/***************************************************/
/* r0 contains limit  */
genereraleas:
    push {r1-r4,lr}                                  @ save registers
    ldr r4,iAdriGraine
    ldr r2,[r4]
    ldr r3,iNbDep1
    mul r2,r3,r2
    ldr r3,iNbDep1
    add r2,r2,r3
    str r2,[r4]                                      @ maj de la graine pour l appel suivant
    cmp r0,#0
    beq 100f
    mov r1,r0                                        @ divisor
    mov r0,r2                                        @ dividende
    bl division
    mov r0,r3                                        @ résult = remainder

100:                                                 @ end function
    pop {r1-r4,lr}                                   @ restaur registers
    bx lr                                            @ return
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



## C

This is generic to the extreme, although the function is technically being fed strings, it can handle any type, as shown in the outputs below :

### Interactive and without hardcoded inputs


```C

#include<stdlib.h>
#include<stdio.h>
#include<time.h>

void sattoloCycle(void** arr,int count){
	int i,j;
	void* temp;

	if(count<2)
		return;
	for(i=count-1;i>=1;i--){
		j = rand()%i;
		temp = arr[j];
		arr[j] = arr[i];
		arr[i] = temp;
	}
}

int main(int argC,char* argV[])
{
	int i;

	if(argC==1)
		printf("Usage : %s <array elements separated by a space each>",argV[0]);
	else{
                srand((unsigned)time(NULL));
		sattoloCycle((void*)(argV + 1),argC-1);

		for(i=1;i<argC;i++)
			printf("%s ",argV[i]);
	}
	return 0;
}

```

Output:

```txt

C:\rosettaCode>sattoloCycle.exe ""

C:\rosettaCode>sattoloCycle.exe 10
10
C:\rosettaCode>sattoloCycle.exe 10 20
20 10
C:\rosettaCode>sattoloCycle.exe 10 20 30
30 10 20
C:\rosettaCode>sattoloCycle.exe 11 12 13 14 15 16 17 18 19 20 21 22
16 17 11 12 13 20 22 14 15 21 18 19
C:\rosettaCode>sattoloCycle.exe s a t t o l o C y c l e
l o s a t c e t o l C y
C:\rosettaCode>sattoloCycle.exe 1 2.3 4.2 1 3 e r q t 2 1 oo 2.1 eds
1 2.1 2.3 q r eds 1 e 3 t 1 2 oo 4.2
C:\rosettaCode>sattoloCycle.exe totally mixed up random string ( 1 2.3 2 ) which will get even more { a 2 q.1 } mixed up.
mixed q.1 a 1 up ) 2 even { will ( } 2 more totally random get which string up. 2.3 mixed

```



### Non Interactive and with hardcoded inputs

Same code but with hardcoded integer arrays as in the task to show that the function can handle any type.

```C

#include<stdlib.h>
#include<stdio.h>
#include<time.h>

void sattoloCycle(void** arr,int count){
	int i,j;
	void* temp;

	if(count<2)
		return;
	for(i=count-1;i>=1;i--){
		j = rand()%i;
		temp = arr[j];
		arr[j] = arr[i];
		arr[i] = temp;
	}
}

int main()
{
	int i;

	int a[] = {};
	int b[] = {10};
	int c[] = {10, 20};
	int d[] = {10, 20, 30};
	int e[] = {11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22};

	srand((unsigned)time(NULL));
	sattoloCycle((void*)a,0);

	printf("\nShuffled a = ");
	for(i=0;i<0;i++)
		printf("%d ",a[i]);

	sattoloCycle((void*)b,1);

	printf("\nShuffled b = ");
	for(i=0;i<1;i++)
		printf("%d ",b[i]);

	sattoloCycle((void*)c,2);

	printf("\nShuffled c = ");
	for(i=0;i<2;i++)
		printf("%d ",c[i]);

	sattoloCycle((void*)d,3);

	printf("\nShuffled d = ");
	for(i=0;i<3;i++)
		printf("%d ",d[i]);

	sattoloCycle((void*)e,12);

	printf("\nShuffled e = ");
	for(i=0;i<12;i++)
		printf("%d ",e[i]);

	return 0;
}

```

Output:

```txt

Shuffled a =
Shuffled b = 10
Shuffled c = 20 10
Shuffled d = 20 30 10
Shuffled e = 13 18 14 20 17 15 21 19 16 12 22 11

```



## C++


```cpp

#include <ctime>
#include <string>
#include <iostream>
#include <algorithm>

class cycle{
public:
    template <class T>
    void cy( T* a, int len ) {
        int i, j;
        show( "original: ", a, len );
        std::srand( unsigned( time( 0 ) ) );

        for( int i = len - 1; i > 0; i-- ) {
            do {
                j = std::rand() % i;
            } while( j >= i );
            std::swap( a[i], a[j] );
        }

        show( "  cycled: ", a, len ); std::cout << "\n";
    }
private:
    template <class T>
    void show( std::string s, T* a, int len ) {
        std::cout << s;
        for( int i = 0; i < len; i++ ) {
            std::cout << a[i] << " ";
        }
        std::cout << "\n";
    }
};
int main( int argc, char* argv[] ) {
    std::string d0[] = { "" },
                d1[] = { "10" },
                d2[] = { "10", "20" };
    int         d3[] = { 10, 20, 30 },
                d4[] = { 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 };
    cycle c;
    c.cy( d0, sizeof( d0 ) / sizeof( d0[0] ) );
    c.cy( d1, sizeof( d1 ) / sizeof( d1[0] ) );
    c.cy( d2, sizeof( d2 ) / sizeof( d2[0] ) );
    c.cy( d3, sizeof( d3 ) / sizeof( d3[0] ) );
    c.cy( d4, sizeof( d4 ) / sizeof( d4[0] ) );

    return 0;
}

```

```txt

original:
  cycled:

original: 10
  cycled: 10

original: 10 20
  cycled: 20 10

original: 10 20 30
  cycled: 30 10 20

original: 11 12 13 14 15 16 17 18 19 20 21 22
  cycled: 13 17 14 22 11 18 20 12 21 19 15 16

```


## C#

```c#
private static readonly Random Rand = new Random();

void sattoloCycle<T>(IList<T> items) {
    for (var i = items.Count; i-- > 1;) {
        int j = Rand.Next(i);
        var tmp = items[i];
        items[i] = items[j];
        items[j] = tmp;
    }
}
```



## D


```D
import std.stdio;

void main() {
    auto items = [0,1,2,3,4,5];
    sattoloCycle(items);
    items.writeln;
}

/// The Sattolo cycle is an algorithm for randomly shuffling an array in such a way that each element ends up in a new position.
void sattoloCycle(R)(R items) {
    import std.algorithm : swapAt;
    import std.random : uniform;

    for (int i=items.length; i-- > 1;) {
        int j = uniform(0, i);
        items.swapAt(i, j);
    }
}

unittest {
    import std.range : lockstep;
    auto o = ['a', 'b', 'c', 'd', 'e'];

    auto s = o.dup;
    sattoloCycle(s);
    foreach (a, b; lockstep(o, s)) {
        assert(a != b, "An element stayed in place unexpectedly.");
    }
}
```


Several runs shown

```txt
[2, 4, 1, 5, 3, 0]
[3, 0, 4, 5, 1, 2]
[3, 5, 4, 1, 0, 2]
[5, 4, 3, 0, 2, 1]

```



## Factor


```factor
USING: arrays io kernel literals math math.ranges prettyprint
random sequences ;
IN: rosetta-code.sattolo-cycle

: (sattolo) ( seq -- seq' )
    dup dup length 1 - 1 [a,b]
    [ dup iota random rot exchange ] with each ;

: sattolo ( seq -- seq/seq' )
    dup length 1 > [ (sattolo) ] when ;

{
    { }
    { 10 }
    { 10 20 }
    { 10 20 30 }
    $[ 11 22 [a,b] >array ]
}
[
    [ "original: " write .         ]
    [ "cycled:   " write sattolo . ] bi nl
] each
```

```txt

original: { }
cycled:   { }

original: { 10 }
cycled:   { 10 }

original: { 10 20 }
cycled:   { 20 10 }

original: { 10 20 30 }
cycled:   { 30 10 20 }

original: { 11 12 13 14 15 16 17 18 19 20 21 22 }
cycled:   { 16 19 20 13 17 18 22 14 21 15 11 12 }

```



## Free Pascal


```pascal
program sattolocycle;
{$ifdef fpc}{$mode delphi}{$endif}
uses math;
var
  a:Array of cardinal;
  i,j:integer;
  t:cardinal;
begin
  randomize;
  a:=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19];
  i := length(a);
  while i > 0 do
  begin
    dec(i);
    j :=randomrange(Low(a),i);
    t:=a[i];a[i]:=a[j];a[j]:=t;
    write(a[i]:4);
  end;
end.
```


```txt

Output in Free Pascal:
  2  14  12  13   0   1  15   9   7   6   3  18  10   4  16   5  19   8  11  17
Note output in Delphi differs because of different PRNG algorithms

```



## FreeBASIC


```freebasic
' version 22-10-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

' sort from lower bound to the highter bound
' array's can have subscript range from -2147483648 to +2147483647

Sub sattolo_cycle(a() As Long)

    Dim As Long lb = LBound(a)
    Dim As ULong n = UBound(a) - lb +1
    Dim As ULong i, j

    Randomize Timer

    For i = n -1 To 1 Step -1
        j =Fix(Rnd * (i))       ' 0 <= j < i
        Swap a(lb + i), a(lb + j)
    Next

End Sub

' ------=< MAIN >=------

Dim As Long i, array(1 To 52)

For i = 1 To 52 : array(i) = i : Next

Print "Starting array from 1 to 52"
For i = 1 To 52
    Print Using " ###";array(i);
Next : Print : Print

sattolo_cycle(array())

Print "After Sattolo_Cycle"
For i = 1 To 52
    Print Using " ###";array(i);
Next : Print : Print


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Starting array from 1 to 52
   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52

After Sattolo_Cycle
  40  48   7  25  32  17  44   4   8  13  18  47   5  29  10  20  49  39  11  51   3  21  46   2  38  16  28  37  12  50   1   9  52  19  22  30  36  27  45  15  24  23  33  41  14  31  43  26  35  34  42   6
```



## Go


```go

package main

import (
	"math/rand"
	"fmt"
)

func main() {
	list := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	for i := 1; i <= 10; i++ {
		sattoloCycle(list)
		fmt.Println(list)
	}
}

func sattoloCycle(list []int) {
	for x := len(list) -1; x > 0; x-- {
		j := rand.Intn(x)
		list[x], list[j] = list[j], list[x]
	}
}

```

```txt

[4 5 1 7 3 9 10 2 8 6]
[7 9 5 1 2 3 4 8 6 10]
[2 3 9 4 6 8 7 1 10 5]
[6 2 10 1 8 4 5 9 7 3]
[8 3 7 2 10 1 6 4 9 5]
[7 5 1 4 9 2 3 10 6 8]
[6 8 3 10 2 4 7 1 5 9]
[1 6 8 7 9 5 4 2 3 10]
[9 5 10 6 2 8 1 7 4 3]
[7 3 1 10 4 2 8 6 5 9]

```



## Haskell


```haskell
import Control.Monad ((>=>), (>>=), forM_)
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import System.Random.MWC

type MutVec m a = M.MVector (PrimState m) a

-- Perform an in-place shuffle of the vector, making it a single random cyclic
-- permutation of its initial value.  The vector is also returned for
-- convenience.
cyclicPermM :: PrimMonad m => Gen (PrimState m) -> MutVec m a -> m (MutVec m a)
cyclicPermM rand vec = forM_ [1..M.length vec-1] upd >> return vec
  where upd i = uniformR (0, i-1) rand >>= M.swap vec i

-- Return a vector that is a single random cyclic permutation of the argument.
cyclicPerm :: PrimMonad m => Gen (PrimState m) -> V.Vector a -> m (V.Vector a)
cyclicPerm rand = V.thaw >=> cyclicPermM rand >=> V.unsafeFreeze

--------------------------------------------------------------------------------

test :: Show a => [a] -> IO ()
test xs = do
  let orig = V.fromList xs
  cyc <- withSystemRandom . asGenIO $ \rand -> cyclicPerm rand orig
  putStrLn $ "original: " ++ show orig
  putStrLn $ "  cycled: " ++ show cyc

main :: IO ()
main = do
  test ([] :: [()])
  test [10 :: Int]
  test [10, 20 :: Int]
  test [10, 20, 30 :: Int]
  test [11..22 :: Int]
  -- Also works for other types.
  test "abcdef"
```


```txt

$ ./sattolo
original: []
  cycled: []
original: [10]
  cycled: [10]
original: [10,20]
  cycled: [20,10]
original: [10,20,30]
  cycled: [20,30,10]
original: [11,12,13,14,15,16,17,18,19,20,21,22]
  cycled: [13,14,16,11,17,20,18,21,22,15,19,12]
original: "abcdef"
  cycled: "cfeabd"

```



## J


The key "feature" of this algorithm is that it cannot generate some legal random permutations. For example, given a two element list, it will always reverse that list.

Implementation:


```J
sattolo=:3 :0
  for_i.}:i.-#y do.
    j=.?i
    y=. (<i,j) C. y
  end.
  y
)

```


Example use:


```J
   sattolo ''

   sattolo ,10
10
   sattolo 10 20
20 10
   sattolo 10 20 30
30 10 20
   sattolo 11+i.12
19 18 15 21 12 17 22 16 20 13 11 14
```



## Java


```Java
private static final Random rng = new Random();

void sattoloCycle(Object[] items) {
    for (int i = items.length-1; i > 0; i--) {
        int j = rng.nextInt(i);
        Object tmp = items[i];
        items[i] = items[j];
        items[j] = tmp;
    }
}
```



## JavaScript


```JavaScript
function sattoloCycle(items) {
    for (var i = items.length-1; i > 0; i--) {
        var j = Math.floor(Math.random() * i);
        var tmp = items[i];
        items[i] = items[j];
        items[j] = tmp;
    }
}
```



## Jsish


```javascript
/* Sattolo cycle array shuffle, in Jsish */
function sattoloCycle(items:array):void {
    for (var i = items.length-1; i > 0; i--) {
        var j = Math.floor(Math.random() * i);
        var tmp = items[i];
        items[i] = items[j];
        items[j] = tmp;
    }
}

if (Interp.conf('unitTest')) {
    Math.srand(0);
    for (var a of [[], [10], [10,20], [10,20,30], [11,12,13,14,15,16,17,18,19,20,21,22]]) {
;       a;
        sattoloCycle(a);
;       a;
    }
}

/*
=!EXPECTSTART!=
a ==> []
a ==> []
a ==> [ 10 ]
a ==> [ 10 ]
a ==> [ 10, 20 ]
a ==> [ 20, 10 ]
a ==> [ 10, 20, 30 ]
a ==> [ 30, 10, 20 ]
a ==> [ 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ]
a ==> [ 22, 11, 17, 15, 12, 14, 19, 13, 21, 18, 16, 20 ]
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u sattoloCycle.jsi
[PASS] sattoloCycle.jsi
```



## Julia

```julia
function sattolocycle!(arr::Array, last::Int=length(arr))
    for i in last:-1:2
        j = rand(1:i-1)
        arr[i], arr[j] = arr[j], arr[i]
    end
    return arr
end

@show sattolocycle!([])
@show sattolocycle!([10])
@show sattolocycle!([10, 20, 30])
@show sattolocycle!([11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22])
```


```txt
sattolocycle!([]) = Any[]
sattolocycle!([10]) = [10]
sattolocycle!([10, 20, 30]) = [30, 10, 20]
sattolocycle!([11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]) = [19, 20, 15, 11, 17, 18, 21, 22, 13, 16, 12, 14]
```



## Kotlin


```scala
// version 1.0.6

fun <T> sattolo(items: Array<T>) {
    for (i in items.size - 1 downTo 1) {
        val j = (Math.random() * i).toInt()
        val t = items[i]
        items[i] = items[j]
        items[j] = t
    }
}

fun main(args: Array<String>) {
    val items = arrayOf(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    println(items.joinToString())
    sattolo(items)
    println(items.joinToString())
}
```

Sample output:
```txt

11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22
22, 11, 19, 12, 21, 14, 18, 20, 17, 16, 13, 15


```



## Lua


```Lua
function sattolo (items)
    local j
    for i = #items, 2, -1 do
        j = math.random(i - 1)
        items[i], items[j] = items[j], items[i]
    end
end

math.randomseed(os.time())
local testCases = {
    {},
    {10},
    {10, 20},
    {10, 20, 30},
    {11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22}
}
for _, array in pairs(testCases) do
    sattolo(array)
    print("[" .. table.concat(array, ", ") .. "]")
end
```

```txt
[]
[10]
[20, 10]
[30, 10, 20]
[15, 17, 22, 18, 16, 19, 21, 11, 12, 13, 20, 14]
```


=={{header|Modula-2}}==

```modula2
MODULE SattoloCycle;
FROM FormatString IMPORT FormatString;
FROM RandomNumbers IMPORT Randomize,Random;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE SwapInt(VAR a,b : INTEGER);
VAR t : INTEGER;
BEGIN
    t := a;
    a := b;
    b := t;
END SwapInt;

TYPE
    ARR = ARRAY[0..5] OF INTEGER;
VAR
    buf : ARRAY[0..63] OF CHAR;
    items : ARR;
    i,j : INTEGER;
BEGIN
    Randomize(0);
    items := ARR{0,1,2,3,4,5};

    FOR i:=0 TO HIGH(items) DO
        j := Random(0,i);
        SwapInt(items[i], items[j]);
    END;

    FOR i:=0 TO HIGH(items) DO
        FormatString(" %i", buf, items[i]);
        WriteString(buf)
    END;

    ReadChar
END SattoloCycle.
```



## Nim

```nim
import random

proc sattoloCycle[T](a: var openArray[T]) =
  var j = 0
  if a.len < 2:
    return
  for i in countdown(a.high, 1):
    j = rand(int.high) mod i
    swap a[j], a[i]

var a: seq[int] = @[]
var b: seq[int] = @[10]
var c: seq[int] = @[10, 20]
var d: seq[int] = @[10, 20, 30]
var e: seq[int] = @[11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]

randomize()

a.sattoloCycle()
echo "Shuffled a = ", $a

b.sattoloCycle()
echo "\nShuffled b = ", $b

c.sattoloCycle()
echo "\nShuffled c = ", $c

d.sattoloCycle()
echo "\nShuffled d = ", $d

e.sattoloCycle()
echo "\nShuffled e = ", $e
```


```txt
Shuffled a = @[]

Shuffled b = @[10]

Shuffled c = @[20, 10]

Shuffled d = @[20, 30, 10]

Shuffled e = @[20, 21, 14, 17, 13, 18, 12, 22, 11, 15, 16, 19]
```



## Objeck

```objeck
class Sattolo {
  function : Main(args : String[]) ~ Nil {
    array := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    SattoloCycle(array);
    array->ToString()->PrintLine();
  }

  function : SattoloCycle(items : Int[]) ~ Nil {
    each(i : items) {
      j := (Float->Random() * 100.0)->As(Int) % items->Size();
      tmp := items[i];
      items[i] := items[j];
      items[j] := tmp;
    };
  }
}

```


Output:

```txt

[9,8,4,5,10,1,2,6,3,7]

```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


@interface NSMutableArray (SattoloCycle)
- (void)sattoloCycle;
@end
@implementation NSMutableArray (SattoloCycle)
- (void)sattoloCycle {
  for (NSUInteger i = self.count-1; i > 0; i--) {
    NSUInteger j = arc4random_uniform(i);
    [self exchangeObjectAtIndex:i withObjectAtIndex:j];
  }
}
@end
```



## OCaml


```ocaml
let sattolo_cycle arr =
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int i in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done
```



## Perl


```perl
@a = 0..30;

printf "%2d ", $_ for @a; print "\n";
sattolo_cycle(\@a);
printf "%2d ", $_ for @a; print "\n";

sub sattolo_cycle {
    my($array) = @_;
    for $i (reverse 0 .. -1+@$array) {
        my $j = int rand $i;
        @$array[$j, $i] = @$array[$i, $j];
    }
}
```

```txt
 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
18  5  9 25  3 23 12  2 26 21 16  4 11 15 20  1 27 10 29  7  6 28 24  8 13 17 19  0 14 30 22
```


## Perl 6


This modifies the array passed as argument, in-place.


```perl6
sub sattolo-cycle (@array) {
    for reverse 1 .. @array.end -> $i {
        my $j = (^$i).pick;
        @array[$j, $i] = @array[$i, $j];
    }
}

my @a = flat 'A' .. 'Z', 'a' .. 'z';

say @a;
sattolo-cycle(@a);
say @a;
```


```txt
[A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j k l m n o p q r s t u v w x y z]
[r G w g W Z D X M f Q A c i H Y J F s z m v x P b U j n q I N e O L o C d u a K S V l y R T B k t h p E]
```



## Phix


```Phix
sequence cards = tagset(52)
puts(1,"Before: ")      ?cards
for i=52 to 2 by -1 do
    integer r = rand(i-1)
    {cards[r],cards[i]} = {cards[i],cards[r]}
end for
puts(1,"After:  ")      ?cards
for i=1 to 52 do
    if cards[i]=i then ?9/0 end if
end for
if sort(cards)!=tagset(52) then ?9/0 end if
puts(1,"Sorted: ")      ?sort(cards)
```

<pre style="font-size: 12px">
Before: {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52}
After:  {51,47,8,9,20,5,43,21,12,2,7,19,4,32,10,23,30,29,31,38,13,44,41,26,42,15,34,46,27,33,40,18,24,17,28,48,3,45,11,22,39,1,35,49,36,14,6,25,50,16,52,37}
Sorted: {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52}

```



## PHP


```PHP
function sattoloCycle($items) {
   for ($i = 0; $i < count($items); $i++) {
        $j = floor((mt_rand() / mt_getrandmax()) * $i);
        $tmp = $items[$i];
        $items[$i] = $items[$j];
        $items[$j] = $tmp;
    }
    return $items;
}

```



## PicoLisp


```PicoLisp
(seed (in "/dev/urandom" (rd 8)))

(de sattolo (Lst)
   (for (N (length Lst) (>= N 2) (dec N))
      (let I (rand 1 (dec N))
         (xchg (nth Lst N) (nth Lst I)) ) ) )

(let L (range 1 15)
   (println 'before L)
   (sattolo L)
   (println 'after L) )
```

```txt

before (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
after (4 1 12 6 2 13 9 11 8 5 3 14 7 15 10)

```



## Python




```python

>>> from random import randrange
>>> def sattoloCycle(items):
	for i in range(len(items) - 1, 0, -1):
		j = randrange(i)  # 0 <= j <= i-1
		items[j], items[i] = items[i], items[j]


>>> # Tests
>>> for _ in range(10):
	lst = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	sattoloCycle(lst)
	print(lst)


[5, 8, 1, 2, 6, 4, 3, 9, 10, 7]
[5, 9, 8, 10, 4, 3, 6, 2, 1, 7]
[10, 5, 8, 3, 9, 1, 4, 2, 6, 7]
[10, 5, 2, 6, 9, 7, 8, 3, 1, 4]
[7, 4, 8, 5, 10, 3, 2, 9, 1, 6]
[2, 3, 10, 9, 4, 5, 8, 1, 7, 6]
[5, 7, 4, 6, 2, 9, 3, 10, 8, 1]
[3, 10, 7, 2, 9, 5, 8, 4, 1, 6]
[2, 6, 5, 3, 9, 8, 10, 7, 1, 4]
[3, 6, 2, 5, 10, 4, 1, 9, 7, 8]
>>>
```



## Racket



```racket
#lang racket

;; although the shuffle is in-place, returning the shuffled vector makes
;; testing a little easier
(define (sattolo-shuffle v)
  (for ((i (in-range (sub1 (vector-length v)) 0 -1)))
    (define j (random i))
    (define tmp (vector-ref v i))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j tmp))
  v)

(define (derangement-of? A B #:strict? (strict? #t))
  (match* (A B)
    [('() '()) #t]
    [((list a) (list a)) #:when strict? #t]
    [((list a _ ...) (list a _ ...)) #f]
    [((list _ as ...) (list _ bs ...))
     (derangement-of? as bs #:strict? #t)]
    [((vector as ...) (vector bs ...))
     (derangement-of? as bs #:strict? strict?)]))

(module+ test
  (require rackunit)

  (check-equal? (sattolo-shuffle (vector)) #())
  (check-equal? (sattolo-shuffle (vector 10)) #(10))
  (check-equal? (sattolo-shuffle (vector 'inky)) #(inky))

  (define v′ (sattolo-shuffle (vector 11 12 13 14 15 16 17 18 19 20 21)))

  v′

  (check-true (derangement-of? #(11 12 13 14 15 16 17 18 19 20 21) v′)))
```


```txt
'#(21 19 12 11 18 17 14 16 15 13 20)
```



## REXX


###  version 1

This REXX example uses a zero-based array;   (to match the pseudo-code).

The array elements values can be of any type (even mixed):   integer, floating point, characters, ···

The values of the array elements are specified via the command line (C.L.).

```rexx
/*REXX program  implements and displays a  Sattolo shuffle  for an array  (of any type).*/
parse arg a;    say 'original:'      space(a)    /*obtain args from the CL; display 'em.*/
   do x=0 for words(a);  @.x=word(a, x+1);  end  /*assign all elements to the @. array. */
                                                 /* [↑]  build an array of given items. */
       do #=x-1  by -1  to 1;  j=random(0, #-1)  /*get a random integer between 0 & I-1.*/
       parse value @.#  @.j    with    @.j  @.#  /*swap two array elements, J is random.*/
       end   /*j*/                               /* [↑]  shuffle @ via Sattolo algorithm*/
$=                                               /* [↓]  build a list of shuffled items.*/
       do k=0  for x;   $=$  @.k;    end  /*k*/  /*append the next element in the array.*/
say  ' Sattolo:'        strip($)                 /*stick a fork in it,  we're all done. */
```

```txt

original:
 Sattolo:

```

```txt

original: 10
 Sattolo: 10

```

```txt

original: 10 20
 Sattolo: 20 10

```

```txt

original: 10 20 30
 Sattolo: 20 30 10

```

{{out|output|text=  when using the input of:   <tt> 11 12 13 14 15 16 17 18 19 20 21 22 </tt>

```txt

original: 11 12 13 14 15 16 17 18 19 20 21 22
 Sattolo: 15 14 17 19 18 12 22 13 20 21 11 16

```

'''output'''   when using the input of:   <tt> -1 0 00 oNe 2.7 /\ [] +6e1 <nowiki> ~~~ </nowiki> </tt>}}

```txt

original: -1 0 00 one 2.7 /\ [] +6e1 ~~~
 Sattolo: /\ 00 +6e1 0 ~~~ oNe -1 2.7 []

```



###  version 2


```rexx
n=25
Do i=0 To n
  a.i=i
  b.i=i
  End
Call show ' pre'
Do i=n to 1 By -1
  j=random(0,i-1)
  Parse Value a.i a.j With a.j a.i
  End
Call show 'post'
Do i=0 To n
  If a.i=b.i Then
    Say i a.i '=' b.i
  End
Exit
Show:
ol=arg(1)
Do i=0 To n
  ol=ol right(a.i,2)
  End
Say ol
Return
```

```txt
 pre  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
post  3  4  8 18 14 21 20 13 10  1 25  7  2 24 12 23  5 11  6 22 16 19  9  0 15 17
```



## Ring


```ring

# Project : Sattolo cycle

a = "123456789abcdefghijklmnopqrstuvwxyz"
n = len(a)
sit = list(n)

for i = 1 to n
    sit[i] = substr(a, i, 1)
next
showsit()
for i = n to 1 step -1
    j = floor(i * random(9)/10) + 1
    h = sit[i]
    sit[i] = sit[j]
    sit[j] = h
next
showsit()

func showsit
     for i = 1 to n
         see sit[i] + " "
     next
     see nl

```

Output:

```txt

1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z
i v 3 c 7 x 6 5 4 n a b r t e f g 2 8 u m o p w q l j h 9 s d y k z 1

```



## Ruby



```ruby

> class Array
>   def sattolo_cycle!
>     (length - 1).downto(1) do |i|
*       j = rand(i)
>       self[i], self[j] = self[j], self[i]
>     end
>     self
>   end
> end
=> :sattolo_cycle!

> # Tests
> 10.times do
*   p [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].sattolo_cycle!
> end
[10, 6, 9, 7, 8, 1, 3, 2, 5, 4]
[3, 7, 5, 10, 4, 8, 1, 2, 6, 9]
[10, 3, 4, 8, 9, 7, 1, 5, 6, 2]
[8, 7, 4, 2, 6, 9, 1, 5, 10, 3]
[2, 7, 5, 10, 8, 3, 6, 9, 4, 1]
[2, 10, 8, 6, 1, 3, 5, 9, 7, 4]
[8, 5, 6, 1, 4, 9, 2, 10, 7, 3]
[5, 4, 10, 7, 2, 1, 8, 9, 3, 6]
[9, 8, 4, 2, 6, 1, 5, 10, 3, 7]
[9, 4, 2, 7, 6, 1, 10, 3, 8, 5]
=> 10
```



## Run BASIC


```Runbasic
a$	= "123456789abcdefghijklmnopqrstuvwxyz"
n	= len(a$)
dim sit$(n)      '  hold area to string
global n

for i = 1 to n			' put string in array
    sit$(i) = mid$(a$,i,1)
next i

call shoSit			' show before change

for i		= n to 1 step -1
    j		= int(i * rnd(1)) + 1
    h$		= sit$(i)
    sit$(i)	= sit$(j)
    sit$(j)	= h$
next i

call shoSit			' show after change
end

sub shoSit
    for i = 1 to n
       print sit$(i);" ";
    next i
    print
end sub

```

```txt
Output:
1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z
d c 5 e v 3 n 7 8 h r p 2 y j l s x q 6 f 9 o a u i w 4 1 m g z t k b
```



## Scala


```Scala
def shuffle[T](a: Array[T]): Array[T] = {
  scala.util.Random.shuffle(a)
  a
}
```


## SequenceL


```sequenceL

import <Utilities/Random.sl>;
import <Utilities/Sequence.sl>;

sattolo(x(1), seed) := shuffle(x, seedRandom(seed), size(x));

shuffle(x(1), RG, n) :=
	let
		next := getRandom(RG);
	in
		x when n <= 1 else
		shuffle(swap(x, n, next.Value mod (n - 1) + 1), next.Generator, n - 1);

swap(list(1), i(0), j(0)) := swapHelper(list, i, j, list[i], list[j]);
swapHelper(list(1), i(0), j(0), vali(0), valj(0)) := setElementAt(setElementAt(list, i, valj), j, vali);


```



## Sidef

Modifies the array in-place:

```ruby
func sattolo_cycle(arr) {
    for i in (arr.len ^.. 1) {
        arr.swap(i, i.irand)
    }
}
```



## Smalltalk

```Smalltalk
SequenceableCollection extend [

    sattoloCycle
        [1 to: self size-1 do:
            [:a || b |
            b := Random between: a+1 and: self size.
            self swap: a with: b]]
]
```

Modifies the collection in-place. Collections that don't support that,
like strings, will throw an exception.

Use example:

```Smalltalk>st
 #() copy sattoloCycle
()
st> #(10) copy sattoloCycle
(10 )
st> #(10 20) copy sattoloCycle
(20 10 )
st> #(10 20 30) copy sattoloCycle
(30 10 20 )
st> #(10 20 30) copy sattoloCycle
(20 30 10 )
st> #(11 12 13 14 15 16 17 18 19 20 21 22) copy sattoloCycle
(22 13 17 18 14 12 15 21 16 11 20 19 )
st> 'Sattolo cycle' asArray sattoloCycle asString
'yocS talcelto'
```



## Swift



```swift
extension Array {
  public mutating func satalloShuffle() {
    for i in stride(from: index(before: endIndex), through: 1, by: -1) {
      swapAt(i, .random(in: 0..<i))
    }
  }

  public func satalloShuffled() -> [Element] {
    var arr = Array(self)

    arr.satalloShuffle()

    return arr
  }
}

let testCases = [
  [],
  [10, 20],
  [10, 20, 30],
  [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]
]

for testCase in testCases {
  let shuffled = testCase.satalloShuffled()

  guard zip(testCase, shuffled).allSatisfy(!=) else {
    fatalError("satallo shuffle failed")
  }

  print("\(testCase) shuffled = \(shuffled)")
}
```


```txt
[] shuffled = []
[10, 20] shuffled = [20, 10]
[10, 20, 30] shuffled = [20, 30, 10]
[11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22] shuffled = [20, 22, 17, 12, 19, 14, 15, 13, 21, 16, 11, 18]
```



## TypeScript


```TypeScript>function sattoloCycle<T
(items: Array<T>): void {
    for (let i = items.length; i--> 1;) {
        const j = Math.floor(Math.random() * i);
        const tmp = items[i];
        items[i] = items[j];
        items[j] = tmp;
    }
}
```



## VBA


```vb
Private Sub Sattolo(Optional ByRef a As Variant)
    Dim t As Variant, i As Integer
    If Not IsMissing(a) Then
        For i = UBound(a) To lbound(a)+1 Step -1
            j = Int((UBound(a) - 1 - LBound(a) + 1) * Rnd + LBound(a))
            t = a(i)
            a(i) = a(j)
            a(j) = t
        Next i
    End If
End Sub
Public Sub program()
    Dim b As Variant, c As Variant, d As Variant, e As Variant
    Randomize
    'imagine an empty array on this line
    b = [{10}]
    c = [{10, 20}]
    d = [{10, 20, 30}]
    e = [{11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22}]
    f = [{"This ", "is ", "a ", "test"}]
    Debug.Print "Before:"
    Sattolo 'feeding an empty array ;)
    Debug.Print "After: "
    Debug.Print "Before:";
    For Each i In b: Debug.Print i;: Next i: Debug.Print
    Sattolo b
    Debug.Print "After: ";
    For Each i In b: Debug.Print i;: Next i: Debug.Print
    Debug.Print "Before:";
    For Each i In c: Debug.Print i;: Next i: Debug.Print
    Sattolo c
    Debug.Print "After: ";
    For Each i In c: Debug.Print i;: Next i: Debug.Print
    Debug.Print "Before:";
    For Each i In d: Debug.Print i;: Next i: Debug.Print
    Sattolo d
    Debug.Print "After: ";
    For Each i In d: Debug.Print i;: Next i: Debug.Print
    Debug.Print "Before:";
    For Each i In e: Debug.Print i;: Next i: Debug.Print
    Sattolo e
    Debug.Print "After: ";
    For Each i In e: Debug.Print i;: Next i: Debug.Print
    Debug.Print "Before:";
    For Each i In f: Debug.Print i;: Next i: Debug.Print
    Sattolo f
    Debug.Print "After: ";
    For Each i In f: Debug.Print i;: Next i: Debug.Print
End Sub

```
```txt
Before:
After:
Before: 10
After:  10
Before: 10  20
After:  20  10
Before: 10  20  30
After:  20  10  30
Before: 11  12  13  14  15  16  17  18  19  20  21  22
After:  16  18  19  17  12  20  22  14  11  13  15  21
Before:This is a test
After: testa is This

```



## Yabasic


```Yabasic
sub sattolo$(l$)
    local i, j, items$(1), n, t$

    n = token(l$, items$(), ",")

    for i = n to 2 step -1
        j = int(ran(i - 1)) + 1
        t$ = items$(i)
        items$(i) = items$(j)
        items$(j) = t$
    next

    t$ = ""
    for i = 1 to n
    	t$ = t$ + items$(i) + ","
    next
    return left$(t$, len(t$) - 1)
end sub

data "", "10", "10,20", "10,20,30", "11,12,13,14,15,16,17,18,19,20,21,22"

for n = 1 to 5
    read item$ : print "[", sattolo$(item$), "]"
next
```



## zkl


```zkl
fcn sattoloCycle(list){	// in place
   foreach i in ([list.len()-1 .. 1,-1]){
      list.swap(i,(0).random(i));  # 0 <= j < i
   }
   list
}
```


```zkl
sattoloCycle([0..9].walk().copy()).println();
sattoloCycle("this is a test".split()).println();
```

```txt

L(6,3,8,2,5,7,1,0,9,4)
L("test","this","is","a")

```


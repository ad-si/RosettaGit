+++
title = "RPG Attributes Generator"
description = ""
date = 2019-10-17T23:15:41Z
aliases = []
[extra]
id = 21909
[taxonomies]
categories = ["task"]
tags = []
+++

You're running a tabletop RPG, and your players are creating characters.

Each character has six core attributes: strength, dexterity, constitution, intelligence, wisdom, and charisma.

One way of generating values for these attributes is to roll four, 6-sided dice (d6) and sum the three highest rolls, discarding the lowest roll.

Some players like to assign values to their attributes in the order they're rolled.

To ensure generated characters don't put players at a disadvantage, the following requirements must be satisfied:

* The total of all character attributes must be at least 75.
* At least two of the attributes must be at least 15.
<p></p>
However, this can require a lot of manual dice rolling. A programatic solution would be much faster.

'''Task'''

Write a program that:
# Generates 4 random, whole values between 1 and 6.
# Saves the sum of the 3 largest values.
# Generates a total of 6 values this way.
# Displays the total, and all 6 values once finished.
<p></p>
* The order in which each value was generated must be preserved.
* The total of all 6 values must be at least 75.
* At least 2 of the values must be 15 or more.
<p></p>

## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program rpg.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

.equ NBTIRAGES,     4
.equ NBTIRAGESOK,   3
.equ NBVALUES,      6
.equ TOTALMIN,      75
.equ MAXVALUE,      15
.equ NBMAXVALUE,    2
/*******************************************/
/* Fichier des macros                       */
/********************************************/
.include "../../ficmacros.s"

/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:        .ascii "Value  = "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:   .asciz "\n"
sMessResultT:       .ascii "Total  = "
sMessValeurT:       .fill 11, 1, ' '            @ size => 11
                    .asciz "\n"
sMessResultQ:       .ascii "Values above 15  = "
sMessValeurQ:       .fill 11, 1, ' '            @ size => 11
                    .asciz "\n"

.align 4
iGraine:  .int 123456

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
tiTirages:               .skip  4 * NBTIRAGES
tiValues:                .skip  4 * NBVALUES
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program

1:                                                @ begin loop 1
    mov r2,#0                                     @ counter value >15
    mov r4,#0                                     @ loop indice
    mov r5,#0                                     @ total
    ldr r3,iAdrtiValues                           @ table values address
2:
    bl genValue                                   @ call generate value
    str r0,[r3,r4,lsl #2]                         @ store in table
    add r5,r0                                     @ compute total
    cmp r0,#MAXVALUE                                    @ count value > 15
    addge r2,#1
    add r4,#1                                     @ increment indice
    cmp r4,#NBVALUES                              @ end ?
    blt 2b
    cmp r5,#TOTALMIN                              @ compare 75
    blt 1b                                        @ < loop
    cmp r2,#NBMAXVALUE                                     @ compare value > 15
    blt 1b                                        @ < loop
    ldr r0,iAdrtiValues                           @ display values
    bl displayTable
    mov r0,r5                                     @ total
    ldr r1,iAdrsMessValeurT                       @ display value
    bl conversion10                               @ call conversion decimal
    ldr r0,iAdrsMessResultT
    bl affichageMess                              @ display message

     mov r0,r2                                    @ counter value > 15
    ldr r1,iAdrsMessValeurQ                       @ display value
    bl conversion10                               @ call conversion decimal
    ldr r0,iAdrsMessResultQ
    bl affichageMess                              @ display message

100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrsMessValeurT:         .int sMessValeurT
iAdrsMessResultT:         .int sMessResultT
iAdrsMessValeurQ:         .int sMessValeurQ
iAdrsMessResultQ:         .int sMessResultQ
iAdrtiValues:             .int tiValues
/******************************************************************/
/*     generate value                                  */
/******************************************************************/
/* r0 returns the value             */
genValue:
    push {r1-r4,lr}                           @ save registers
    mov r4,#0                                 @ indice loop
    ldr r1,iAdrtiTirages                      @ table tirage address
1:
    mov r0,#6
    bl genereraleas                           @ result 0 to 5
    add r0,#1                                 @ for 1 to 6
    str r0,[r1,r4,lsl #2]                     @ store tirage
    add r4,#1                                 @ increment indice
    cmp r4,#NBTIRAGES                         @ end ?
    blt 1b                                    @ no -> loop
    ldr r0,iAdrtiTirages                      @ table tirage address
    mov r1,#0                                 @ first item
    mov r2,#NBTIRAGES                         @ number of tirages
    bl shellSort                              @ sort table decreasing
    mov r4,#0                                 @ raz indice loop
    mov r0,#0                                 @ total
    ldr r1,iAdrtiTirages                      @ table tirage address
2:
    ldr r2,[r1,r4,lsl #2]                     @ read tirage
    add r0,r2                                 @ compute sum
    add r4,#1                                 @ inrement indice
    cmp r4,#NBTIRAGESOK                       @ end ?
    blt 2b
100:
    pop {r1-r4,lr}
    bx lr                                     @ return
iAdrtiTirages:           .int tiTirages
/***************************************************/
/*   shell Sort  decreasing                                  */
/***************************************************/
/* r0 contains the address of table */
/* r1 contains the first element but not use !!   */
/*   this routine use first element at index zero !!!  */
/* r2 contains the number of element */
shellSort:
    push {r0-r7,lr}              @save registers

    sub r2,#1                    @ index last item
    mov r1,r2                    @ init gap = last item
1:                               @ start loop 1
    lsrs r1,#1                   @ gap = gap / 2
    beq 100f                     @ if gap = 0 -> end
    mov r3,r1                    @ init loop indice 1
2:                               @ start loop 2
    ldr r4,[r0,r3,lsl #2]        @ load first value
    mov r5,r3                    @ init loop indice 2
3:                               @ start loop 3
    cmp r5,r1                    @ indice < gap
    blt 4f                       @ yes -> end loop 2
    sub r6,r5,r1                 @ index = indice - gap
    ldr r7,[r0,r6,lsl #2]        @ load second value
    cmp r4,r7                    @ compare values
    strgt r7,[r0,r5,lsl #2]      @ store if >
    subgt r5,r1                  @ indice = indice - gap
    bgt 3b                       @ and loop
4:                               @ end loop 3
    str r4,[r0,r5,lsl #2]        @ store value 1 at indice 2
    add r3,#1                    @ increment indice 1
    cmp r3,r2                    @ end ?
    ble 2b                       @ no -> loop 2
    b 1b                         @ yes loop for new gap

100:                             @ end function
    pop {r0-r7,lr}               @ restaur registers
    bx lr                        @ return


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
    cmp r3,#NBVALUES - 1
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

1:                                                  @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b                                          @ and loop
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
    str r2,[r4]                                        @ maj de la graine pour appel suivant
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



## C

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int compareInts(const void *i1, const void *i2) {
    int a = *((int *)i1);
    int b = *((int *)i2);
    return a - b;
}

int main() {
    int i, j, nsum, vsum, vcount, values[6], numbers[4];
    srand(time(NULL));
    for (;;) {
        vsum = 0;
        for (i = 0; i < 6; ++i) {
            for (j = 0; j < 4; ++j) {
                numbers[j] = 1 + rand() % 6;
            }
            qsort(numbers, 4, sizeof(int), compareInts);
            nsum = 0;
            for (j = 1; j < 4; ++j) {
                nsum += numbers[j];
            }
            values[i] = nsum;
            vsum += values[i];
        }
        if (vsum < 75) continue;
        vcount = 0;
        for (j = 0; j < 6; ++j) {
            if (values[j] >= 15) vcount++;
        }
        if (vcount < 2) continue;
        printf("The 6 random numbers generated are:\n");
        printf("[");
        for (j = 0; j < 6; ++j) printf("%d ", values[j]);
        printf("\b]\n");
        printf("\nTheir sum is %d and %d of them are >= 15\n", vsum, vcount);
        break;
    }
    return 0;
}
```


Sample run:

```txt

The 6 random numbers generated are:
[9 15 15 17 13 8]

Their sum is 77 and 3 of them are >= 15

```



## C++

GCC 4.9.2, unoptimised.

```cpp
#include <algorithm>
#include <ctime>
#include <iostream>
#include <cstdlib>
#include <string>

using namespace std;

int main()
{
    srand(time(0));

    unsigned int attributes_total = 0;
    unsigned int count = 0;
    int attributes[6] = {};
    int rolls[4] = {};

    while(attributes_total < 75 || count < 2)
    {
        attributes_total = 0;
        count = 0;

        for(int attrib = 0; attrib < 6; attrib++)
        {
            for(int roll = 0; roll < 4; roll++)
            {
                rolls[roll] = 1 + (rand() % 6);
            }

            sort(rolls, rolls + 4);
            int roll_total = rolls[1] + rolls[2] + rolls[3];

            attributes[attrib] = roll_total;
            attributes_total += roll_total;

            if(roll_total >= 15) count++;
        }
    }

    cout << "Attributes generated : [";
    cout << attributes[0] << ", ";
    cout << attributes[1] << ", ";
    cout << attributes[2] << ", ";
    cout << attributes[3] << ", ";
    cout << attributes[4] << ", ";
    cout << attributes[5];

    cout << "]\nTotal: " << attributes_total;
    cout << ", Values above 15 : " << count;

    return 0;
}
```

Sample run:

```txt

Attributes generated : [13, 13, 17, 14, 10, 16]
Total: 83, Values above 15 : 2

```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Linq;

static class Module1
{
    static Random r = new Random();

    static List<int> getThree(int n)
    {
        List<int> g3 = new List<int>();
        for (int i = 0; i < 4; i++) g3.Add(r.Next(n) + 1);
        g3.Sort(); g3.RemoveAt(0); return g3;
    }

    static List<int> getSix()
    {
        List<int> g6 = new List<int>();
        for (int i = 0; i < 6; i++) g6.Add(getThree(6).Sum());
        return g6;
    }

    static void Main(string[] args)
    {
        bool good = false; do {
            List<int> gs = getSix(); int gss = gs.Sum(); int hvc = gs.FindAll(x => x > 14).Count;
            Console.Write("attribs: {0}, sum={1}, ({2} sum, high vals={3})",
                          string.Join(", ", gs), gss, gss >= 75 ? "good" : "low", hvc);
            Console.WriteLine(" - {0}", (good = gs.Sum() >= 75 && hvc > 1) ? "success" : "failure");
        } while (!good);
    }
}
```

sample outputs:

```txt
attribs: 10, 11, 11, 11, 11, 14, sum=68, (low sum, high vals=0) - failure
attribs: 16, 13, 12, 10, 15, 16, sum=82, (good sum, high vals=3) - success
```


```txt
attribs: 16, 8, 9, 15, 16, 12, sum=76, (good sum, high vals=3) - success
```


=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>RPGGEN
    set attr = $lb("")    ; empty list to start
    write "Rules:",!,"1.) Total of 6 attributes must be at least 75.",!,"2.) At least two scores must be 15 or more.",!

    ; loop until valid result
    do {
        ; loop through 6 attributes
        for i = 1:1:6 {
            set (low, dice, keep) = ""

            ; roll 4 dice each time
            for j = 1:1:4 {
                set roll = $r(6) + 1
                set dice = dice + roll

                if (roll < low) || (low = "") {
                    set low = roll
                }
            }    ; 4 dice rolls per attribute

            set keep = (dice - low)
            set $list(attr,i) = keep
        }    ; 6 attributes

        ; loop the ending list
        set (tot,bigs) = 0
        for i = 1:1:$listlength(attr) {
            set cur = $listget(attr,i)
            set tot = tot + cur

            if (cur >= 15) {
                set bigs = bigs + 1
            }
        }

        ; analyze results
        write !,"Scores: "_$lts(attr)
        set result = $select((tot < 75) && (bigs < 2):0,tot < 75:1,bigs < 2:2,1:3)
        write !,?5,$case(result,
                    0:"Total "_tot_" too small and not enough attributes ("_bigs_") >= 15.",
                    1:"Total "_tot_" too small.",
                    2:"Need 2 or more attributes >= 15, only have "_bigs_".",
                    3:"Total "_tot_" and "_bigs_ " attributes >=15 are sufficient.")
    } while (result '= 3)

    quit
```


```txt

SAMPLES>do ^RPGGEN
Rules:
1.) Total of 6 attributes must be at least 75.
2.) At least two scores must be 15 or more.

Scores: 13,14,12,14,16,14
     Need 2 or more attributes >= 15, only have 1.
Scores: 9,16,13,14,12,16
     Total 80 and 2 attributes >=15 are sufficient.

```



## Crystal


```Ruby
def roll_stat
  dices = Array(Int32).new(4) { rand(1..6) }
  dices.sum - dices.min
end

def roll_character
  loop do
    stats = Array(Int32).new(6) { roll_stat }
    return stats if stats.sum >= 75 && stats.count(&.>=(15)) >= 2
  end
end

10.times do
  stats = roll_character
  puts "stats: #{stats}, sum is #{stats.sum}"
end
```


sample output:

```txt
stats: [14, 11, 18, 14, 12, 16], sum is 85
stats: [10, 12, 13, 16, 17, 16], sum is 84
stats: [12, 17, 13, 11, 17, 13], sum is 83
stats: [16, 12, 11, 9, 16, 12], sum is 76
stats: [14, 17, 12, 15, 16, 14], sum is 88
stats: [9, 17, 17, 7, 9, 16], sum is 75
stats: [17, 14, 17, 12, 12, 13], sum is 85
stats: [16, 8, 14, 12, 11, 16], sum is 77
stats: [17, 13, 11, 10, 14, 16], sum is 81
stats: [11, 16, 11, 13, 15, 16], sum is 82
```



## Dyalect

```dyalect
func getThree(n) {
    var g3 = []
    for i in 0..33 {
        g3.add(rnd(max: n) + 1)
    }
    g3.sort()
    g3.removeAt(0)
    g3
}

func getSix() {
    var g6 = []
    for i in 0..5 {
        g6.add(getThree(6).sum())
    }
    g6
}

func Array.sum() {
    var acc = 0
    for x in this {
        acc += x
    }
    acc
}

func Array.findAll(pred) {
    for x in this when pred(x) {
        yield x
    }
}

var good = false

while !good {
    var gs = getSix()
    var gss = gs.sum()
    var hvc = gs.findAll(x => x > 14).len()
    print("attribs: \(String.join(gs, separator: ", ")), sum=\(gss), (\(if gss >= 75 { "good" } else { "low" }) sum, high vals=\(hvc))", terminator: "")
    good = gs.sum() >= 75 && hvc > 1
    print(" - " + (if good { "success" } else { "failure" }))
}
```


```txt
attribs: [103, 133, 110, 130, 120, 101], sum=697, (good sum, high vals=6) - success
```




## EasyLang


<lang>len v[] 6
repeat
  vsum = 0
  vmin = 0
  for i range 6
    val = 0
    min = 6
    for j range 4
      h = random 6 + 1
      val += h
      if h < min
        min = h
      .
    .
    val -= min
    v[i] = val
    if val >= 15
      vmin += 1
    .
    vsum += val
  .
  until vsum >= 75 and vmin >= 2
.
print "Attribues: " & " " & v[]
print "Total: " & " " & vsum
```



```txt

Attributes:  [ 18 13 17 15 9 11 ]
Total:  83

```



## Factor

```factor
USING: combinators.short-circuit dice formatting io kernel math
math.statistics qw sequences ;
IN: rosetta-code.rpg-attributes-generator

CONSTANT: stat-names qw{ Str Dex Con Int Wis Cha }

: attribute ( -- n )
    4 [ ROLL: 1d6 ] replicate 3 <iota> kth-largests sum ;

: stats ( -- seq ) 6 [ attribute ] replicate ;

: valid-stats? ( seq -- ? )
    { [ [ 15 >= ] count 2 >= ] [ sum 75 >= ] } 1&& ;

: generate-valid-stats ( -- seq )
    f [ dup valid-stats? ] [ drop stats ] do until ;

: stats-info ( seq -- )
    [ sum ] [ [ 15 >= ] count ] bi
    "Total: %d\n# of attributes >= 15: %d\n" printf ;

: main ( -- )
    generate-valid-stats dup stat-names swap
    [ "%s: %d\n" printf ] 2each nl stats-info ;

MAIN: main
```

```txt

Str: 9
Dex: 13
Con: 14
Int: 17
Wis: 17
Cha: 11

Total: 81
# of attributes >= 15: 2

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
    s := rand.NewSource(time.Now().UnixNano())
    r := rand.New(s)
    for {
        var values [6]int
        vsum := 0
        for i := range values {
            var numbers [4]int
            for j := range numbers {
                numbers[j] = 1 + r.Intn(6)
            }
            sort.Ints(numbers[:])
            nsum := 0
            for _, n := range numbers[1:] {
                nsum += n
            }
            values[i] = nsum
            vsum += values[i]
        }
        if vsum < 75 {
            continue
        }
        vcount := 0
        for _, v := range values {
            if v >= 15 {
                vcount++
            }
        }
        if vcount < 2 {
            continue
        }
        fmt.Println("The 6 random numbers generated are:")
        fmt.Println(values)
        fmt.Println("\nTheir sum is", vsum, "and", vcount, "of them are >= 15")
        break
    }
}
```


Sample run:

```txt

The 6 random numbers generated are:
[16 15 7 14 9 15]

Their sum is 76 and 3 of them are >= 15

```



## Haskell


```haskell
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.List (sort)

character :: IO [Int]
character =
  discardUntil
    (((&&) . (75 <) . sum) <*> ((2 <=) . length . filter (15 <=)))
    (replicateM 6 $
     (sum . tail . sort) <$> replicateM 4 (randomRIO (1, 6 :: Int)))

discardUntil :: ([Int] -> Bool) -> IO [Int] -> IO [Int]
discardUntil p throw =
  let go =
        throw >>=
        \x ->
           if p x
             then return x
             else go
  in go

-- TEST -----------------------------------------------------------
main :: IO ()
main = replicateM 10 character >>= mapM_ (print . (sum >>= (,)))
```

```txt
Sample computation:

(86,[15,13,17,17,13,11])
(80,[16,11,15,13,11,14])
(77,[15,8,15,11,15,13])
(76,[12,11,17,11,7,18])
(76,[11,16,8,15,15,11])
(87,[14,15,12,16,15,15])
(84,[11,11,16,15,15,16])
(77,[14,13,15,8,11,16])
(80,[12,15,11,17,15,10])
(89,[15,12,16,17,12,17])
```



## J

'twould be more efficient to work with index origin 0, then increment the roll once at output.

```J

roll=: [: >: 4 6 ?@:$ 6:
massage=: +/ - <./
generate_attributes=: massage@:roll
accept=: (75 <: +/) *. (2 <: [: +/ 15&<:)
Until=: conjunction def 'u^:(0-:v)^:_'

NB. show displays discarded attribute sets
NB. and since roll ignores arguments, echo would suffice in place of show
show=: [ echo

NB. use: generate_character 'name'
generate_character=: (; (+/ ; ])@:([: generate_attributes@:show Until accept 0:))&>@:boxopen

```




```txt

   generate_character 'MrKent'
0
15 12 16 9 12 8
13 17 11 12 14 9
16 7 14 12 13 14
13 8 6 12 7 9
12 12 17 14 13 13
┌──────┬──┬─────────────────┐
│MrKent│79│13 16 16 10 10 14│
└──────┴──┴─────────────────┘
   generate_character ;: 'Ti StMary Judas'
0
12 11 12 15 13 12
13 13 14 16 8 11
0
16 14 9 11 12 12
10 14 9 11 7 9
0
┌──────┬──┬─────────────────┐
│Ti    │81│17 9 11 16 13 15 │
├──────┼──┼─────────────────┤
│StMary│79│15 18 16 13 11 6 │
├──────┼──┼─────────────────┤
│Judas │83│16 11 12 15 17 12│
└──────┴──┴─────────────────┘


```



## Java


```Java

    static boolean goodRoll = false;

    public static int genAttribute(){
        // Create a new Random object to populate our array with. We use nextInt(6)+1 because 6 is exclusive and 0 is inclusive
        Random dice = new Random();
        int[] sumArray = {dice.nextInt(6)+1, dice.nextInt(6)+1, dice.nextInt(6)+1, dice.nextInt(6)+1};

        // Sort the array ascending, last 3 dice will always be the highest, return sum.
        java.util.Arrays.sort(sumArray);
        return (sumArray[1] + sumArray[2] + sumArray[3]);
    }

    public static boolean checkFinalArray(int[] checkArray){
        int fifteenCount = 0;

        // First check for how many 15+'s
        for (int z : checkArray){
            if (z >= 15){
                fifteenCount++;
            }
        }

        return (fifteenCount >= 2 && Arrays.stream(checkArray).sum() >= 75);
    }

    public static void main(String[] args) {
        // Here we use a while loop to make sure that while the conditions aren't met, we reroll.
        while (!goodRoll){
            int[] finalArray;
            finalArray = new int[6];

            // Generate 6 attributes using above method genAttributes()
            for (int i = 0; i<6;++i){
                finalArray[i] = genAttribute();
            }
            // Pass finalArray to be checked
            if (checkFinalArray(finalArray)){
                System.out.println("sum: " + Arrays.stream(finalArray).sum());
                // Enhanced for to print each die
                for (int x : finalArray){
                    System.out.println(x);
                }
                goodRoll = true; // Exit the loop if conditions are met.
            }
        }
    }

```

```txt

sum: 79
10
16
14
16
8
15

```



## Javascript


### Imperative


```javascript
function roll() {
  const stats = {
    total: 0,
    rolls: []
  }
  let count = 0;

  for(let i=0;i<=5;i++) {
    let d6s = [];

    for(let j=0;j<=3;j++) {
      d6s.push(Math.ceil(Math.random() * 6))
    }

    d6s.sort().splice(0, 1);
    rollTotal = d6s.reduce((a, b) => a+b, 0);

    stats.rolls.push(rollTotal);
    stats.total += rollTotal;
  }

  return stats;
}

let rolledCharacter = roll();

while(rolledCharacter.total < 75 || rolledCharacter.rolls.filter(a => a >= 15).length < 2){
  rolledCharacter = roll();
}

console.log(`The 6 random numbers generated are:
${rolledCharacter.rolls.join(', ')}

Their sum is ${rolledCharacter.total} and ${rolledCharacter.rolls.filter(a => a >= 15).length} of them are >= 15`);
```


Sample run:

```txt

The 6 random numbers generated are:
11, 17, 12, 12, 9, 16

Their sum is 77 and 2 of them are >= 15

```



### Functional

{{Trans|Python}} (Functional composition version)

```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () =>
        // 10 random heroes drawn from
        // a non-finite series.
        unlines(map(
            xs => show(sum(xs)) +
            ' -> [' + show(xs) + ']',

            take(10, heroes(
                seventyFivePlusWithTwo15s
            ))
        ));

    // seventyFivePlusWithTwo15s :: [Int] -> Bool
    const seventyFivePlusWithTwo15s = xs =>
        // Total score over 75,
        // with two or more qualities scoring 15.
        75 < sum(xs) && 1 < length(filter(
            x => 15 === x, xs
        ));

    // heroes :: Gen IO [(Int, Int, Int, Int, Int, Int)]
    function* heroes(p) {
        // Non-finite list of heroes matching
        // the requirements of predicate p.
        while (true) {
            yield hero(p)
        }
    }

    // hero :: (Int -> Bool) -> IO (Int, Int, Int, Int, Int, Int)
    const hero = p =>
        // A random character matching the
        // requirements of predicate p.
        until(p, character, []);

    // character :: () -> IO [Int]
    const character = () =>
        // A random character with six
        // integral attributes.
        map(() => sum(tail(sort(map(
                randomRInt(1, 6),
                enumFromTo(1, 4)
            )))),
            enumFromTo(1, 6)
        );


    // GENERIC FUNCTIONS ----------------------------------

    // enumFromTo :: (Int, Int) -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // e.g. map(randomRInt(1, 10), enumFromTo(1, 20))

    // randomRInt :: Int -> Int -> IO () -> Int
    const randomRInt = (low, high) => () =>
        low + Math.floor(
            (Math.random() * ((high - low) + 1))
        );

    // show :: a -> String
    const show = x => x.toString()

    // sort :: Ord a => [a] -> [a]
    const sort = xs => xs.slice()
        .sort((a, b) => a < b ? -1 : (a > b ? 1 : 0));

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

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

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ---
    return main();
})();
```

A sample of 10 character attribute sets:

```txt
79 -> [12,15,12,15,13,12]
92 -> [17,16,15,15,17,12]
82 -> [12,14,13,13,15,15]
84 -> [15,16,18,10,15,10]
83 -> [15,12,17,14,10,15]
83 -> [14,15,10,15,14,15]
77 -> [12,13,15,11,15,11]
81 -> [15,8,16,15,15,12]
79 -> [15,15,11,17,12,9]
76 -> [14,12,9,15,15,11]
```



## Julia


```julia
roll_skip_lowest(dice, sides) = (r = rand(collect(1:sides), dice); sum(r) - minimum(r))

function rollRPGtoon()
    attributes = zeros(Int, 6)
    attsum = 0
    gte15 = 0

    while attsum < 75 || gte15 < 2
        for i in 1:6
            attributes[i] = roll_skip_lowest(4, 6)
        end
        attsum = sum(attributes)
        gte15 = mapreduce(x -> x >= 15, +, attributes)
    end

    println("New RPG character roll: $attributes. Sum is $attsum, and $gte15 are >= 15.")
end

rollRPGtoon()
rollRPGtoon()
rollRPGtoon()

```
```txt

New RPG character roll: [15, 16, 15, 11, 9, 15]. Sum is 81, and 4 are >= 15.
New RPG character roll: [12, 14, 15, 12, 10, 16]. Sum is 79, and 2 are >= 15.
New RPG character roll: [10, 12, 13, 13, 15, 17]. Sum is 80, and 2 are >= 15.

```



## Kotlin


```scala
// Version 1.2.51

import java.util.Random

fun main(args: Array<String>) {
    val r = Random()
    while (true) {
        val values = IntArray(6)
        for (i in 0..5) {
            val numbers = IntArray(4) { 1 + r.nextInt(6) }
            numbers.sort()
            values[i] = numbers.drop(1).sum()
        }
        val vsum = values.sum()
        val vcount = values.count { it >= 15 }
        if (vsum < 75 || vcount < 2) continue
        println("The 6 random numbers generated are:")
        println(values.asList())
        println("\nTheir sum is $vsum and $vcount of them are >= 15")
        break
    }
}
```


Sample run:

```txt

The 6 random numbers generated are:
[13, 14, 13, 15, 17, 8]

Their sum is 80 and 2 of them are >= 15

```



## MiniScript


```MiniScript
roll = function()
    results = []
    for i in range(0,3)
        results.push ceil(rnd * 6)
    end for
    results.sort
    results.remove 0
    return results.sum
end function

while true
    attributes = []
    gt15 = 0    // (how many attributes > 15)
    for i in range(0,5)
        attributes.push roll
        if attributes[i] > 15 then gt15 = gt15 + 1
    end for

    print "Attribute values: " + attributes.join(", ")
    print "Attributes total:  " + attributes.sum

    if attributes.sum >= 75 and gt15 >= 2 then break
    print "Attributes failed, rerolling"
    print
end while
print "Success!"

```

```txt
Attribute values: 11, 13, 8, 10, 8, 10
Attributes total:  60
Attributes failed, rerolling

Attribute values: 11, 13, 14, 13, 15, 14
Attributes total:  80
Attributes failed, rerolling

Attribute values: 13, 11, 12, 9, 13, 12
Attributes total:  70
Attributes failed, rerolling

Attribute values: 18, 17, 12, 10, 17, 12
Attributes total:  86
Success!
```


=={{header|Pascal|FreePascal}}==


```Pascal

program attributes;

var
   total, roll,score, count: integer;
   atribs : array [1..6] of integer;

begin
    randomize; {Initalise the random number genertor}
    repeat
        count:=0;
        total:=0;
        for score :=1 to 6 do begin
           {roll:=random(18)+1;   produce a number up to 18, pretty much the same results}
           for diceroll:=1 to 4 do dice[diceroll]:=random(6)+1; {roll 4 six sided die}

           {find lowest rolled dice. If we roll two or more equal low rolls then we
	    eliminate the first of them, change '<' to '<=' to eliminate last low die}
           lowroll:=7;
	   lowdie:=0;
	   for diceroll:=1 to 4 do if (dice[diceroll] < lowroll) then begin
	       lowroll := dice[diceroll];
	       lowdie := diceroll;
	   end;
           {add up higest three dice}
	   roll:=0;
	   for diceroll:=1 to 4 do if (diceroll <> lowdie) then roll := roll + dice[diceroll];
           atribs[score]:=roll;
           total := total + roll;
           if (roll>15) then count:=count+1;
        end;
   until ((total>74) and (count>1)); {this evens out different rolling methods }
   { Prettily print the attributes out }
   writeln('Attributes :');
   for count:=1 to 6 do
      writeln(count,'.......',atribs[count]:2);
   writeln('       ---');
   writeln('Total  ',total:3);
   writeln('       ---');
end.

```

```txt

Attributes :
1....... 5
2.......13
3....... 8
4.......17
5.......15
6.......18
       ---
Total   76
       ---
Attributes :
1.......17
2.......13
3.......17
4.......12
5.......12
6.......16
       ---
Total   87
       ---
Attributes :
1.......16
2....... 9
3.......10
4.......17
5.......15
6....... 9
       ---
Total   76

```



## Perl


```perl
use strict;
use List::Util 'sum';

my ($min_sum, $hero_attr_min, $hero_count_min) = <75 15 3>;
my @attr_names = <Str Int Wis Dex Con Cha>;

sub heroic { scalar grep { $_ >= $hero_attr_min } @_ }

sub roll_skip_lowest {
    my($dice, $sides) = @_;
    sum( (sort map { 1 + int rand($sides) } 1..$dice)[1..$dice-1] );
}

my @attr;
do {
    @attr = map { roll_skip_lowest(6,4) } @attr_names;
} until sum(@attr) >= $min_sum and heroic(@attr) >= $hero_count_min;

printf "%s = %2d\n", $attr_names[$_], $attr[$_] for 0..$#attr;
printf "Sum = %d, with %d attributes >= $hero_attr_min\n", sum(@attr), heroic(@attr);
```

```txt
Str = 13
Int = 15
Wis =  9
Dex = 19
Con = 17
Cha = 10
Sum = 83, with 3 attributes >= 15
```



## Perl 6

```perl6
my ( $min_sum, $hero_attr_min, $hero_count_min ) = 75, 15, 2;
my @attr-names = <Str Int Wis Dex Con Cha>;

sub heroic { + @^a.grep: * >= $hero_attr_min }

my @attr;
repeat until @attr.sum     >= $min_sum
         and heroic(@attr) >= $hero_count_min {

    @attr = @attr-names.map: { (1..6).roll(4).sort(+*).skip(1).sum };
}

say @attr-names Z=> @attr;
say "Sum: {@attr.sum}, with {heroic(@attr)} attributes >= $hero_attr_min";
```

```txt

(Str => 15 Int => 16 Wis => 13 Dex => 11 Con => 15 Cha => 6)
Sum: 76, with 3 attributes >= 15

```



## Phix


```Phix
sequence numbers = repeat(0,6)
integer t,n
while true do
    for i=1 to length(numbers) do
        sequence ni = sq_rand(repeat(6,4))
        numbers[i] = sum(ni)-min(ni)
    end for
    t = sum(numbers)
    n = sum(sq_ge(numbers,15))
    if t>=75 and n>=2 then exit end if
    ?"re-rolling..."  -- (occasionally >20)
end while
printf(1,"The 6 attributes generated are:\n")
printf(1,"strength %d, dexterity %d, constitution %d, "&
         "intelligence %d, wisdom %d, and charisma %d.\n",
         numbers)
printf(1,"\nTheir sum is %d and %d of them are >=15\n",{t,n})
```

```txt

"re-rolling..."
"re-rolling..."
The 6 attributes generated are:
strength 14, dexterity 15, constitution 17, intelligence 9, wisdom 13, and charisma 18.

Their sum is 86 and 3 of them are >=15

```



## PHP


```php
<?php

$attributesTotal = 0;
$count = 0;

while($attributesTotal < 75 || $count < 2) {
    $attributes = [];

    foreach(range(0, 5) as $attribute) {
        $rolls = [];

        foreach(range(0, 3) as $roll) {
            $rolls[] = rand(1, 6);
        }

        sort($rolls);
        array_shift($rolls);

        $total = array_sum($rolls);

        if($total >= 15) {
            $count += 1;
        }

        $attributes[] = $total;
    }

    $attributesTotal = array_sum($attributes);
}

print_r($attributes);
```


## PureBasic


```purebasic
#heroicAttributeMinimum = 15
#heroicAttributeCountMinimum = 2
#attributeSumMinimum = 75
#attributeCount = 6

Procedure roll_attribute()
  Protected i, sum
  Dim rolls(3)

  For i = 0 To 3
    rolls(i) = Random(6, 1)
  Next i

  ;sum the highest three rolls
  SortArray(rolls(), #PB_Sort_Descending)
  For i = 0 To 2
    sum + rolls(i)
  Next
  ProcedureReturn sum
EndProcedure

Procedure displayAttributes(List attributes(), sum, heroicCount)
  Protected output$

  output$ = "Attributes generated: ["
  ForEach attributes()
    output$ + attributes()
    If ListIndex(attributes()) <> #attributeCount - 1: output$ + ", ": EndIf
  Next
  output$ + "]"
  PrintN(output$)
  PrintN("Total: " + sum + ", Values " + #heroicAttributeMinimum + " or above: " + heroicCount)
EndProcedure

Procedure Gen_attributes()
  Protected i, attributesSum, heroicAttributesCount

  NewList attributes()
  Repeat
    ClearList(attributes())
    attributesSum = 0: heroicAttributesCount = 0
    For i = 1 To #attributeCount
      AddElement(attributes())
      attributes() = roll_attribute()
      attributesSum + attributes()
      heroicAttributesCount + Bool(attributes() >= #heroicAttributeMinimum)
    Next
  Until attributesSum >= #attributeSumMinimum And heroicAttributesCount >= #heroicAttributeCountMinimum

  displayAttributes(attributes(), attributesSum, heroicAttributesCount)
EndProcedure

If OpenConsole("RPG Attributes Generator")
  Gen_attributes()
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Attributes generated: [13, 17, 17, 11, 9, 17]
Total: 84, Values 15 or above: 3
```



## Python


### Python: Simple


```python
import random
random.seed()
attributes_total = 0
count = 0

while attributes_total < 75 or count < 2:
    attributes = []

    for attribute in range(0, 6):
        rolls = []

        for roll in range(0, 4):
            result = random.randint(1, 6)
            rolls.append(result)

        sorted_rolls = sorted(rolls)
        largest_3 = sorted_rolls[1:]
        rolls_total = sum(largest_3)

        if rolls_total >= 15:
            count += 1

        attributes.append(rolls_total)

    attributes_total = sum(attributes)

print(attributes_total, attributes)
```


Sample run:

```txt
(74, [16, 10, 12, 9, 16, 11])
```



### Python: Nested Comprehensions #1


```python
import random
random.seed()
total = 0
count = 0

while total < 75 or count < 2:
    attributes = [(sum(sorted([random.randint(1, 6) for roll in range(0, 4)])[1:])) for attribute in range(0, 6)]

    for attribute in attributes:
        if attribute >= 15:
            count += 1

    total = sum(attributes)

print(total, attributes)
```


Sample run:

```txt
(77, [17, 8, 15, 13, 12, 12])
```



### Python: Nested Comprehensions #2

With comprehensions for checking candidate values in the while expression.

```python
import random

def compute():
    values = []
    while (sum(values) < 75                            # Total must be >= 75
           or sum(1 for v in values if v >= 15) < 2):  # Two must be >= 15
        values = [sum(sorted(random.randint(1, 6) for _ in range(4))[1:]) for _ in range(6)]
    return sum(values), values

for i in range(3):
    print(*compute())

```


```txt
81 [12, 17, 9, 9, 17, 17]
75 [16, 7, 13, 12, 15, 12]
81 [15, 11, 15, 16, 10, 14]
```



### Python: Functional composition

Composing a hero-generator from reusable functions:
```python
'''RPG Attributes Generator'''

from itertools import islice
from operator import eq
import random


# heroes :: Gen IO [(Int, Int, Int, Int, Int, Int)]
def heroes(p):
    '''Non-finite list of heroes matching
       the requirements of predicate p.'''
    while True:
        yield hero(p)


# hero :: ([Int] -> Bool) -> IO (Int, Int, Int, Int, Int, Int)
def hero(p):
    '''A random character matching the
       requirements of predicate p.'''
    return tuple(
        until(p)(character)([])
    )


# character :: () -> IO [Int]
def character(_):
    '''A random character with six
       integral attributes.'''
    return [
        sum(sorted(map(
            randomRInt(1)(6),
            enumFromTo(1)(4)
        ))[1:])
        for _ in enumFromTo(1)(6)
    ]


# TEST -------------------------------------------------
# main :: IO ()
def main():
    '''Test :: Sample of 10'''

    # seventyFivePlusWithTwo15s :: [Int] -> Bool
    def seventyFivePlusIncTwo15s(xs):
        '''Sums to 75 or more,
           and includes at least two 15s.'''
        return 75 <= sum(xs) and (
            1 < len(list(filter(curry(eq)(15), xs)))
        )

    print('A sample of 10:\n')
    print(unlines(
        str(sum(x)) + ' -> ' + str(x) for x
        in take(10)(heroes(
            seventyFivePlusIncTwo15s
        ))
    ))


# GENERIC -------------------------------------------------

# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    return lambda a: lambda b: f(a, b)


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# randomRInt :: Int -> Int -> IO () -> Int
def randomRInt(m):
    '''Returns a generator function
       which can be applied to any argument,
       always returning some integer in
       the range m to n.'''
    return lambda n: lambda _: random.randint(m, n)


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


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


if __name__ == '__main__':
    main()
```


```txt
A sample of 10:

76 -> (15, 14, 12, 9, 15, 11)
85 -> (12, 11, 16, 15, 15, 16)
80 -> (15, 11, 15, 9, 13, 17)
81 -> (15, 14, 12, 13, 15, 12)
82 -> (10, 12, 13, 15, 15, 17)
77 -> (9, 15, 11, 15, 15, 12)
83 -> (15, 13, 13, 15, 15, 12)
84 -> (10, 16, 15, 14, 14, 15)
79 -> (17, 15, 10, 11, 15, 11)
75 -> (15, 13, 7, 11, 14, 15)
```



## Racket


```racket
#lang racket

(define (d6 . _)
  (+ (random 6) 1))

(define (best-3-of-4d6 . _)
  (apply + (rest (sort (build-list 4 d6) <))))

(define (generate-character)
  (let* ((rolls (build-list 6 best-3-of-4d6))
         (total (apply + rolls)))
    (if (or (< total 75) (< (length (filter (curryr >= 15) rolls)) 2))
        (generate-character)
        (values rolls total))))

(module+ main
  (define-values (rolled-stats total) (generate-character))
  (printf "Rolls:\t~a~%Total:\t~a" rolled-stats total))
```


```txt
Rolls:	(11 16 10 13 12 15)
Total:	77
```



## REXX


### version 1


```rexx
/* REXX
Generates 4 random, whole values between 1 and 6.
Saves the sum of the 3 largest values.
Generates a total of 6 values this way.
Displays the total, and all 6 values once finished.
*/
Do try=1 By 1
  ge15=0
  sum=0
  ol=''
  Do i=1 To 6
    rl=''
    Do j=1 To 4
      rl=rl (random(5)+1)
      End
    rl=wordsort(rl)
    rsum.i=maxsum()
    If rsum.i>=15 Then ge15=ge15+1
    sum=sum+rsum.i
    ol=ol right(rsum.i,2)
    End
  Say ol '->' ge15 sum
  If ge15>=2 & sum>=75 Then Leave
  End
Say try 'iterations'
Say ol '=>' sum
Exit

maxsum: procedure Expose rl
/**********************************************************************
* Comute the sum of the 3 largest values
**********************************************************************/
  m=0
  Do i=2 To 4
    m=m+word(rl,i)
    End
  Return m

wordsort: Procedure
/**********************************************************************
* Sort the list of words supplied as argument. Return the sorted list
**********************************************************************/
  Parse Arg wl
  wa.=''
  wa.0=0
  Do While wl<>''
    Parse Var wl w wl
    Do i=1 To wa.0
      If wa.i>w Then Leave
      End
    If i<=wa.0 Then Do
      Do j=wa.0 To i By -1
        ii=j+1
        wa.ii=wa.j
        End
      End
    wa.i=w
    wa.0=wa.0+1
    End
  swl=''
  Do i=1 To wa.0
    swl=swl wa.i
    End
  Return strip(swl)
```

```txt
I:\>rexx cast
 13 13  8 15 14 11 -> 1 74
 10  9 13  7 15  9 -> 1 63
 15 15 14 13 17 14 -> 3 88
3 iterations
 15 15 14 13 17 14 => 88
```



### version 2

This REXX version doesn't need a sort to compute the sum of the largest three (of four) values.

```rexx
/*REXX program generates values for six core attributes for a  RPG  (Role Playing Game).*/
   do  until  m>=2 & $$>=75;   $$= 0;     list=  /*do rolls until requirements are met. */
   m= 0                                          /*the number of values ≥ 15   (so far).*/
        do 6;                  $= 0              /*6 values (meet criteria); attrib. sum*/
             do d=1  for 4;    @.d= random(1, 6) /*roll four random dice (six sided die)*/
             $= $ + @.d                          /*also obtain their sum  (of die pips).*/
             end   /*d*/                         /* [↓]  use of MIN  BIF avoids sorting.*/
        $= $  -  min(@.1, @.2, @.3, @.4)         /*obtain the sum of the highest 3 rolls*/
        list= list  $;         $$= $$ + $        /*append $──►list; add $ to overall $$.*/
        $$= $$ + $                               /*add the  $  sum  to the overall sum. */
        m= m + ($>=15)                           /*get # of rolls that meet the minimum.*/
        end       /*do 6*/                       /* [↑]  gen six core attribute values. */
   end            /*until*/                      /*stick a fork in it,  we're all done. */
say 'The total for '     list      "  is ──► "       $$', '     m     " entries are ≥ 15."
```

```txt

The total for   14 12 15 16 14 15   is ──►  86,  3  entries are ≥ 15.

```



### version 3

A variation of version 2

```rexx
/*REXX program generates values for six core attributes for an RPG (Role Playing Game).*/
Do n=1 By 1 until m>=2 & tot>=75;
  slist=''
  tot=0
  m=0
  Do 6
    sum=0
    Do d=1 To 4;
      cast.d=random(1,6)
      sum=sum+cast.d
      End
    min=min(cast.1,cast.2,cast.3,cast.4)
    sum=sum-min
    slist=slist sum
    tot=tot+sum
    m=m+(sum>=15)
    end
  Say 'the total for' space(slist) 'is -->' tot', 'm' entries are >= 15.'
  end
Say 'Solution found with' n 'iterations'
```

```txt
I:\>rexx rpg
the total for 12 14 14 13 12 9 is --> 74, 0 entries are >= 15.
the total for 15 11 13 14 10 10 is --> 73, 1 entries are >= 15.
the total for 18 12 12 11 16 10 is --> 79, 2 entries are >= 15.
Solution found with 3 iterations
```



## Ring


```ring

# Project  : RPG Attributes Generator

load "stdlib.ring"
attributestotal = 0
count = 0
while attributestotal < 75 or count < 2
        attributes = []
        for attribute = 0 to 6
             rolls = []
             largest3 = []
             for roll = 0 to 4
                  result = random(5)+1
                  add(rolls,result)
             next
             sortedrolls = sort(rolls)
             sortedrolls = reverse(sortedrolls)
             for n = 1 to 3
                  add(largest3,sortedrolls[n])
             next
             rollstotal = sum(largest3)
             if rollstotal >= 15
                count = count + 1
             ok
             add(attributes,rollstotal)
        next
        attributestotal = sum(attributes)
end
showline()

func sum(aList)
       num = 0
       for n = 1 to len(aList)
            num = num + aList[n]
       next
       return num

func showline()
        line = "(" + attributestotal + ", ["
        for n = 1 to len(attributes)
             line = line + attributes[n] + ", "
        next
        line = left(line,len(line)-2)
        line = line + "])"
        see line + nl

```

Output:

```txt

(95, [14, 11, 14, 13, 16, 11, 16])

```



## Ruby


```ruby
res = []
until res.sum >= 75 && res.count{|n| n >= 15} >= 2 do
  res = Array.new(6) do
    a = Array.new(4){rand(1..6)}
    a.sum - a.min
  end
end

p res
puts "sum: #{res.sum}"

```

```txt
[12, 14, 17, 12, 16, 9]
sum: 80

```



## Rust

Repeats until the attributes generated meet specifications.
```rust

use rand::distributions::Uniform;
use rand::prelude::{thread_rng, ThreadRng};
use rand::Rng;

fn main() {
    for _ in 0..=10 {
        attributes_engine();
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Dice {
    amount: i32,
    range: Uniform<i32>,
    rng: ThreadRng,
}

impl Dice {
    //  Modeled after d20 polyhederal dice use and notation.
    //  roll_pool() - returns Vec<i32> with length of vector determined by dice amount.
    //  attribute_out() - returns i32, by sorting a dice pool of 4d6, dropping the lowest integer, and summing all elements.
    pub fn new(amount: i32, size: i32) -> Self {
        Self {
            amount,
            range: Uniform::new(1, size + 1),
            rng: thread_rng(),
        }
    }

    fn roll_pool(mut self) -> Vec<i32> {
        (0..self.amount)
            .map(|_| self.rng.sample(self.range))
            .collect()
    }

    fn attribute_out(&self) -> i32 {
        // Sort dice pool lowest to high and drain all results to exclude the lowest before summing.
        let mut attribute_array: Vec<i32> = self.roll_pool();
        attribute_array.sort();
        attribute_array.drain(1..=3).sum()
    }
}

fn attributes_finalizer() -> (Vec<i32>, i32, bool) {
    let die: Dice = Dice::new(4, 6);
    let mut attributes: Vec<i32> = Vec::new();

    for _ in 0..6 {
        attributes.push(die.attribute_out())
    }

    let attributes_total: i32 = attributes.iter().sum();

    let numerical_condition: bool = attributes
        .iter()
        .filter(|attribute| **attribute >= 15)
        .count()
        >= 2;

    (attributes, attributes_total, numerical_condition)
}

fn attributes_engine() {
    loop {
        let (attributes, attributes_total, numerical_condition) = attributes_finalizer();
        if (attributes_total >= 75) && (numerical_condition) {
            println!(
                "{:?} | sum: {:?}",
                attributes, attributes_total
            );
            break;
        } else {
            continue;
        }
    }
}

```

Sample output, running the generator ten times:

```txt

[15, 12, 15, 11, 10, 18] | sum: 81
[12, 14, 16, 14, 8, 17]  | sum: 81
[15, 15, 11, 10, 12, 17] | sum: 80
[7, 14, 12, 17, 15, 12]  | sum: 77
[13, 15, 16, 7, 11, 15]  | sum: 77
[11, 13, 12, 15, 15, 12] | sum: 78
[15, 16, 13, 14, 11, 8]  | sum: 77
[14, 12, 17, 16, 16, 14] | sum: 89
[11, 16, 12, 9, 16, 17]  | sum: 81
[10, 18, 9, 13, 12, 16]  | sum: 78
[15, 15, 14, 17, 12, 10] | sum: 83

```



## Scala


```scala

import scala.util.Random
Random.setSeed(1)

def rollDice():Int = {
  val v4 = Stream.continually(Random.nextInt(6)+1).take(4)
  v4.sum - v4.min
}

def getAttributes():Seq[Int] = Stream.continually(rollDice()).take(6)

def getCharacter():Seq[Int] = {
  val attrs = getAttributes()
  println("generated => " + attrs.mkString("[",",", "]"))
  (attrs.sum, attrs.filter(_>15).size) match {
    case (a, b)  if (a < 75 || b < 2) => getCharacter
    case _ => attrs
  }
}

println("picked => " + getCharacter.mkString("[", ",", "]"))

```



```txt

generated => [13,13,12,11,10,14]
generated => [17,12,15,11,9,6]
generated => [16,9,9,11,13,14]
generated => [11,12,10,18,7,17]
picked => [11,12,10,18,7,17]

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: count is 0;
    var integer: total is 0;
    var integer: attribIdx is 0;
    var integer: diceroll is 0;
    var integer: sumOfRolls is 0;
    var array integer: attribute is 6 times 0;
    var array integer: dice is 4 times 0;
  begin
    repeat
      count := 0;
      total := 0;
      for key attribIdx range attribute do
        for key diceroll range dice do
          dice[diceroll] := rand(1, 6);
        end for;
        dice := sort(dice);
        sumOfRolls := 0;
        for diceroll range 2 to maxIdx(dice) do  # Discard the lowest roll
          sumOfRolls +:= dice[diceroll];
        end for;
        attribute[attribIdx] := sumOfRolls;
        total +:= sumOfRolls;
        if sumOfRolls >= 15 then
          incr(count);
        end if;
      end for;
    until total >= 75 and count >= 2;
    writeln("Attributes:");
    for key attribIdx range attribute do
       writeln(attribIdx <& " ..... " <& attribute[attribIdx] lpad 2);
    end for;
    writeln("       ----");
    writeln("Total  " <& total lpad 3);
  end func;
```


```txt

Attributes:
1 ..... 11
2 ..... 10
3 .....  7
4 ..... 18
5 ..... 16
6 ..... 14
       ----
Total   76

```



## Visual Basic .NET

repeats until a successful outcome occurs

```vbnet
Module Module1

    Dim r As New Random

    Function getThree(n As Integer) As List(Of Integer)
        getThree = New List(Of Integer)
        For i As Integer = 1 To 4 : getThree.Add(r.Next(n) + 1) : Next
        getThree.Sort() : getThree.RemoveAt(0)
    End Function

    Function getSix() As List(Of Integer)
        getSix = New List(Of Integer)
        For i As Integer = 1 To 6 : getSix.Add(getThree(6).Sum) : Next
    End Function

    Sub Main(args As String())
        Dim good As Boolean = False : Do
            Dim gs As List(Of Integer) = getSix(), gss As Integer = gs.Sum,
                hvc As Integer = gs.FindAll(Function(x) x > 14).Count
            Console.Write("attribs: {0}, sum={1}, ({2} sum, high vals={3})",
                          String.Join(", ", gs), gss, If(gss >= 75, "good", "low"), hvc)
            good = gs.Sum >= 75 AndAlso hvc > 1
            Console.WriteLine(" - {0}", If(good, "success", "failure"))
        Loop Until good
    End Sub
End Module
```

sample outputs:

```txt
attribs: 8, 15, 10, 13, 12, 8, sum=66, (low sum, high vals=1) - failure
attribs: 9, 11, 7, 10, 17, 12, sum=66, (low sum, high vals=1) - failure
attribs: 18, 14, 12, 11, 16, 9, sum=80, (good sum, high vals=2) - success
```


```txt
attribs: 10, 12, 9, 13, 17, 6, sum=67, (low sum, high vals=1) - failure
attribs: 14, 11, 17, 12, 8, 11, sum=73, (low sum, high vals=1) - failure
attribs: 13, 9, 12, 14, 10, 13, sum=71, (low sum, high vals=0) - failure
attribs: 13, 9, 14, 14, 14, 12, sum=76, (good sum, high vals=0) - failure
attribs: 11, 12, 7, 8, 10, 11, sum=59, (low sum, high vals=0) - failure
attribs: 15, 4, 9, 18, 9, 12, sum=67, (low sum, high vals=2) - failure
attribs: 17, 16, 14, 8, 8, 9, sum=72, (low sum, high vals=2) - failure
attribs: 18, 16, 13, 9, 9, 10, sum=75, (good sum, high vals=2) - success
```


## zkl


```zkl
reg attrs=List(), S,N;
do{
   attrs.clear();
   do(6){
      abcd:=(4).pump(List,(0).random.fp(1,7));   // list of 4 [1..6] randoms
      attrs.append(abcd.sum(0) - (0).min(abcd)); // sum and substract min
   }
}while((S=attrs.sum(0))<75 or (N=attrs.filter('>=(15)).len())<2);
println("Random numbers: %s\nSums to %d, with %d >= 15"
        .fmt(attrs.concat(","),S,N));
```

```txt

Random numbers: 15,15,7,17,10,13
Sums to 77 with 3 >= 15

```


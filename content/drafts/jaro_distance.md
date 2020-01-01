+++
title = "Jaro distance"
description = ""
date = 2019-09-18T01:32:57Z
aliases = []
[extra]
id = 20019
[taxonomies]
categories = []
tags = []
+++

{{task}}

The Jaro distance is a measure of similarity between two strings.

The higher the Jaro distance for two strings is, the more similar the strings are.

The score is normalized such that   '''0'''   equates to no similarity and   '''1'''   is an exact match.


;;Definition

The Jaro distance   <math>d_j</math>   of two given strings   <math>s_1</math>   and   <math>s_2</math>   is

: <math>d_j = \left\{

\begin{array}{l l}
  0 & \text{if }m = 0\\
  \frac{1}{3}\left(\frac{m}{|s_1|} + \frac{m}{|s_2|} + \frac{m-t}{m}\right) & \text{otherwise} \end{array} \right.</math>

Where:

* <math>m</math>   is the number of ''matching characters'';
* <math>t</math>   is half the number of ''transpositions''.


Two characters from   <math>s_1</math>   and   <math>s_2</math>   respectively, are considered ''matching'' only if they are the same and not farther than   <math>\left\lfloor\frac{\max(|s_1|,|s_2|)}{2}\right\rfloor-1</math>.

Each character of   <math>s_1</math>   is compared with all its matching
characters in   <math>s_2</math>.

The number of matching (but different sequence order) characters
divided by 2 defines the number of ''transpositions''.


;;Example

Given the strings   <math>s_1</math>   ''DWAYNE''   and   <math>s_2</math>   ''DUANE''   we find:

* <math>m = 4</math>
* <math>|s_1| = 6</math>
* <math>|s_2| = 5</math>
* <math>t = 0</math>


We find a Jaro score of:

: <math>d_j = \frac{1}{3}\left(\frac{4}{6} + \frac{4}{5} + \frac{4-0}{4}\right) = 0.822</math>


;Task

Implement the Jaro-distance algorithm and show the distances for each of the following pairs:

* ("MARTHA", "MARHTA")
* ("DIXON", "DICKSONX")
* ("JELLYFISH", "SMELLYFISH")


; See also
* [[wp:Jaro-Winkler_distance|Jaro–Winkler distance]] on Wikipedia.




## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program jarodist.s   */


/* Constantes    */
.equ BUFFERSIZE,   100
.equ STDIN,  0                                              @ Linux input console
.equ STDOUT, 1                                              @ Linux output console
.equ EXIT,   1                                              @ Linux syscall
.equ READ,   3                                              @ Linux syscall
.equ WRITE,  4                                              @ Linux syscall

.equ SIZESTRING,   256
/* Initialized data */
.data
szCarriageReturn:  .asciz "\n"
szMessResult:       .ascii " Jaro distance * 1000 = "   @ message result
sMessValeur:        .fill 12, 1, ' '
                       .asciz "\n"
szMessDeb:           .asciz "For : "
szMessSep:           .asciz " and "
szString1:           .asciz  "DWAYNE"
szString1A:           .asciz  "DUANE"
szString2:           .asciz  "MARTHA"
szString2A:           .asciz  "MARHTA"
szString3:           .asciz  "DIXON"
szString3A:           .asciz  "DICKSONX"
szString4:           .asciz   "JELLYFISH"
szString4A:           .asciz  "SMELLYFISH"
/* UnInitialized data */
.bss
iTabString1:        .skip 4 * SIZESTRING
iTabString2:        .skip 4 * SIZESTRING
sBuffer:    .skip    BUFFERSIZE

/*  code section */
.text
.global main
main:                                               @ entry of program

    ldr r0,iAdrszString1                            @ address string 1
    ldr r1,iAdrszString1A                           @ address string 2
    bl preparation                                  @ compute jaro distance

    ldr r0,iAdrszString2                            @ address string 1
    ldr r1,iAdrszString2A                           @ address string 2
    bl preparation                                  @ compute jaro distance

    ldr r0,iAdrszString3                            @ address string 1
    ldr r1,iAdrszString3A                           @ address string 2
    bl preparation                                  @ compute jaro distance

    ldr r0,iAdrszString4                            @ address string 1
    ldr r1,iAdrszString4A                           @ address string 2
    bl preparation                                  @ compute jaro distance
100:                                                @ standard end of the program
    mov r0, #0                                      @ return code
    pop {fp,lr}                                     @restaur 2 registers
    mov r7, #EXIT                                   @ request to exit program
    swi 0                                           @ perform the system call

iAdrsMessValeur:        .int sMessValeur
iAdrsBuffer:             .int sBuffer
iAdrszMessResult:       .int szMessResult
iAdrszCarriageReturn:  .int szCarriageReturn
iAdrszMessDeb:           .int szMessDeb
iAdrszMessSep:           .int szMessSep
iAdrszString1:           .int szString1
iAdrszString1A:           .int szString1A
iAdrszString2:           .int szString2
iAdrszString2A:           .int szString2A
iAdrszString3:           .int szString3
iAdrszString3A:           .int szString3A
iAdrszString4:           .int szString4
iAdrszString4A:           .int szString4A
/******************************************************************/
/*     preparation compute                                       */
/******************************************************************/
/* r0 contains the address of the first string */
/* r1 contains the address of the second string */
preparation:
    push {r2,lr}                                   @ save  registers
    mov r2,r0                                      @ save first address
    ldr r0,iAdrszMessDeb                           @ display the two strings
    bl affichageMess
    mov r0,r2
    bl affichageMess
    ldr r0,iAdrszMessSep
    bl affichageMess
    mov r0,r1
    bl affichageMess
    mov r0,r2                                       @ address string 1
                                                    @ and r1 contains address string 2
    bl jaroDistance                                 @ compute jaro distance

    @ conversion register to string
    ldr r1,iAdrsMessValeur
    bl conversion10                                  @ conversion register to string
    ldr r0,iAdrszMessResult
    bl affichageMess                                 @ display message
100:
    pop {r2,lr}                                      @ restaur registers
    bx lr                                            @ return
/******************************************************************/
/*     Compute Jaro distance                                      */
/******************************************************************/
/* r0 contains the address of the first string */
/* r1 contains the address of the second string */
/* r0 returns jaro distance * 1000 because not use float compute !!!  */
jaroDistance:
    push {r1-r11,lr}                                @ save  registers
    mov r6,r0                                       @ save address string 1
    bl strLength                                    @ size string
    cmp r0,#0                                       @ empty string ?
    beq 100f
    mov r4,r0
    mov r0,r1
    bl strLength                                    @ size string 2
    cmp r0,#0                                       @ empty string ?
    beq 100f
    mov r5,r0
    mov r2,#0                                       @ initialisation tables 1 and 2
    mov r3,#0
    ldr r7,iadriTabString1
    ldr r8,iadriTabString2
1:                                                  @loop start
    str r3,[r7,r2,lsl #2]
    str r3,[r8,r2,lsl #2]
    add r2,#1
    cmp r2,#SIZESTRING
    blt 1b

    cmp r4,r5                                      @ compute match distance
    lsrle   r3,r5,#1                               @ length max / 2
    lsrgt   r3,r4,#1
    sub r3,#1                                       @ - 1
    sub r4,#1                                       @ last index string 1
    sub r5,#1                                       @ last index string 2
    mov r11,#0                                      @ match counter
    mov r2,#0                                       @ index loop

2:                                                  @ loop match
    subs r7,r2,r3
    movlt r7,#0                                     @ compute start
    add r8,r2,r3
    add r8,#1
    cmp r8,r5
    movgt r8,r5                                      @ compute end
3:
    ldr r10,iadriTabString2                          @ load element table 2 at location r7
    ldr r9,[r10,r7,lsl #2]
    cmp r9,#0                                        @ if not zero continue
    bne 4f
    ldrb r9,[r6,r2]                                  @ compare characters of two strings
    ldrb r10,[r1,r7]
    cmp r9,r10
    bne 4f                                           @ not equal

    ldr r10,iadriTabString2                           @ match
    mov r9,#1                                         @ store 1 in two tables
    str r9,[r10,r7,lsl #2]
    ldr r10,iadriTabString1
    str r9,[r10,r2,lsl #2]
    add r11,#1                                        @ increment counter match
    b 5f                                             @ end loop 2
4:
    add r7,#1                                         @ following character string 2
    cmp r7,r8                                         @ end ?
    ble 3b
5:
    add r2,#1                                         @ following character string 1
    cmp r2,r4                                         @ end string ?
    ble 2b

    cmp r11,#0                                        @ return if 0 match
    moveq r0,#0
    beq 100f

    /* compute transposition */
    mov r2,#0                                         @ loop indice
    mov r3,#0                                         @ indice string 2
    mov r7,#0                                         @ counter transposition
6:
    ldr r10,iadriTabString1                           @ character match ?
    ldr r9,[r10,r2,lsl #2]
    cmp r9,#0
    beq 8f                                             @ no
    ldr r10,iadriTabString2
7:
    ldr r9,[r10,r3,lsl #2]                             @ yes, search match in table 2
    cmp r9,#0
    addeq r3,#1
    beq 7b
    ldrb r9,[r6,r2]                                    @ compare characters
    ldrb r10,[r1,r3]
    cmp r9,r10
    addne r7,#1                                        @ not equals add 1 to counter

    add r3,#1                                          @ following characters string 2
8:
    add r2,#1                                          @ following characters string 1
    cmp r2,r4                                          @ end string ?
    ble 6b                                             @ no loop
    lsr r7,#1                                          @ counter / 2
    /* Final */
    mov r6,#1000                                       @ factor 1000 for not use float compute !!!
    mul r9,r6,r11                                      @ compute match * 1000
    mul r7,r6,r7                                       @ compute transposition * 1000
    mov r0,r9                                          @ match
    add r1,r4,#1                                       @ size string 1
    bl division
    mov r8,r2
    mov r0,r9                                          @ match
    add r1,r5,#1                                       @ size string 2
    bl division
    add r8,r2
    sub r0,r9,r7                                       @ compute match - transposition
    mov r1,r11                                         @ match
    bl division
    add r8,r2
    mov r0,r8                                          @ division total / 3
    mov r1,#3
    bl division
    mov r0,r2                                          @ return value
100:
    pop {r1-r11,lr}                                    @ restaur registers
    bx lr                                              @ return
iadriTabString1:      .int iTabString1
iadriTabString2:      .int iTabString2
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                           @ save  registres
    mov r2,#0                                       @ counter length
1:                                                  @ loop length calculation
    ldrb r1,[r0,r2]                                 @ read octet start position + index
    cmp r1,#0                                       @ if 0 its over
    addne r2,r2,#1                                  @ else add 1 in the length
    bne 1b                                          @ and loop
                                                    @ so here r2 contains the length of the message
    mov r1,r0                                       @ address message in r1
    mov r0,#STDOUT                                  @ code to write to the standard output Linux
    mov r7, #WRITE                                  @ code call system "write"
    svc #0                                          @ call systeme
    pop {r0,r1,r2,r7,lr}                            @ restaur des  2 registres */
    bx lr                                           @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                      @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:	                                                 @ start loop
    bl divisionpar10U                                    @unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                           @ digit
    strb r1,[r3,r2]                                      @ store digit on area
    cmp r0,#0                                            @ stop if quotient = 0
    subne r2,#1                                          @ else previous position
    bne 1b	                                         @ and loop
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
    mov r0,r4                                          @ result length
    mov r1,#' '                                        @ space
3:
    strb r1,[r3,r4]                                    @ store space in area
    add r4,#1                                          @ next position
    cmp r4,#LGZONECAL
    ble 3b                                             @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                     @ restaur registres
    bx lr                                              @return

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
/*   calcul size string                            */
/***************************************************/
/* r0 string address                 */
/* r0 returns size string            */
strLength:
    push {r1,r2,lr}
    mov r1,#0                                         @ init counter
1:
   ldrb r2,[r0,r1]                                    @ load byte of string index r1
   cmp r2,#0                                          @ end string ?
   addne r1,#1                                        @ no -> +1 counter
   bne 1b                                             @ and loop

100:
    mov r0,r1
    pop {r1,r2,lr}
    bx lr
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


## AWK


```AWK

# syntax: GAWK -f JARO_DISTANCE.AWK
BEGIN {
    main("DWAYNE","DUANE")
    main("MARTHA","MARHTA")
    main("DIXON","DICKSONX")
    main("JELLYFISH","SMELLYFISH")
    exit(0)
}
function main(str1,str2) {
    printf("%9.7f '%s' '%s'\n",jaro(str1,str2),str1,str2)
}
function jaro(str1,str2,  begin,end,i,j,k,leng1,leng2,match_distance,matches,str1_arr,str2_arr,transpositions) {
    leng1 = length(str1)
    leng2 = length(str2)
    if (leng1 == 0 && leng2 == 0) { # both strings are empty
      return(1)
    }
    if (leng1 == 0 || leng2 == 0) { # only one string is empty
      return(0)
    }
    match_distance = int(max(leng1,leng2)/2-1)
    for (i=1; i<=leng1; i++) { # find matches
      begin = int(max(0,i-match_distance))
      end = int(min(i+match_distance+1,leng2))
      for (j=begin; j<=end; j++) {
        if (str2_arr[j]) { continue }
        if (substr(str1,i,1) != substr(str2,j,1)) { continue }
        str1_arr[i] = 1
        str2_arr[j] = 1
        matches++
        break
      }
    }
    if (matches == 0) {
      return(0)
    }
    k = 0
    for (i=1; i<=leng1; i++) { # count transpositions
      if (!str1_arr[i]) { continue }
      while (!str2_arr[k]) {
        k++
      }
      if (substr(str1,i,1) != substr(str2,k,1)) {
        transpositions++
      }
      k++
    }
    transpositions /= 2
    return((matches/leng1)+(matches/leng2)+((matches-transpositions)/matches))/3
}
function max(x,y) { return((x > y) ? x : y) }
function min(x,y) { return((x < y) ? x : y) }

```

{{out}}

```txt

0.8222222 'DWAYNE' 'DUANE'
0.9444444 'MARTHA' 'MARHTA'
0.7666667 'DIXON' 'DICKSONX'
0.8962963 'JELLYFISH' 'SMELLYFISH'

```


## C


```cpp
#include <iostream>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#define TRUE    1
#define FALSE   0

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

double jaro(const char *str1, const char *str2) {
    // length of the strings
    int str1_len = strlen(str1);
    int str2_len = strlen(str2);

    // if both strings are empty return 1
    // if only one of the strings is empty return 0
    if (str1_len == 0) return str2_len == 0 ? 1.0 : 0.0;

    // max distance between two chars to be considered matching
    // floor() is ommitted due to integer division rules
    int match_distance = (int) max(str1_len, str2_len)/2 - 1;

    // arrays of bools that signify if that char in the matching string has a match
    int *str1_matches = calloc(str1_len, sizeof(int));
    int *str2_matches = calloc(str2_len, sizeof(int));

    // number of matches and transpositions
    double matches = 0.0;
    double transpositions = 0.0;

    // find the matches
    for (int i = 0; i < str1_len; i++) {
        // start and end take into account the match distance
        int start = max(0, i - match_distance);
        int end = min(i + match_distance + 1, str2_len);

        for (int k = start; k < end; k++) {
            // if str2 already has a match continue
            if (str2_matches[k]) continue;
            // if str1 and str2 are not
            if (str1[i] != str2[k]) continue;
            // otherwise assume there is a match
            str1_matches[i] = TRUE;
            str2_matches[k] = TRUE;
            matches++;
            break;
        }
    }

    // if there are no matches return 0
    if (matches == 0) {
        free(str1_matches);
        free(str2_matches);
        return 0.0;
    }

    // count transpositions
    int k = 0;
    for (int i = 0; i < str1_len; i++) {
        // if there are no matches in str1 continue
        if (!str1_matches[i]) continue;
        // while there is no match in str2 increment k
        while (!str2_matches[k]) k++;
        // increment transpositions
        if (str1[i] != str2[k]) transpositions++;
        k++;
    }

    // divide the number of transpositions by two as per the algorithm specs
    // this division is valid because the counted transpositions include both
    // instances of the transposed characters.
    transpositions /= 2.0;

    // free the allocated memory
    free(str1_matches);
    free(str2_matches);

    // return the Jaro distance
    return ((matches / str1_len) +
        (matches / str2_len) +
        ((matches - transpositions) / matches)) / 3.0;
}

int main() {
    printf("%f\n", jaro("MARTHA",    "MARHTA"));
    printf("%f\n", jaro("DIXON",     "DICKSONX"));
    printf("%f\n", jaro("JELLYFISH", "SMELLYFISH"));
}
```

{{out}}

```txt

0.944444
0.766667
0.896296

```



## C++

{{trans|C}}

```cpp
#include <algorithm>
#include <iostream>
#include <string>

double jaro(const std::string s1, const std::string s2) {
    const uint l1 = s1.length(), l2 = s2.length();
    if (l1 == 0)
        return l2 == 0 ? 1.0 : 0.0;
    const uint match_distance = std::max(l1, l2) / 2 - 1;
    bool s1_matches[l1];
    bool s2_matches[l2];
    std::fill(s1_matches, s1_matches + l1, false);
    std::fill(s2_matches, s2_matches + l2, false);
    uint matches = 0;
    for (uint i = 0; i < l1; i++)
    {
        const int end = std::min(i + match_distance + 1, l2);
        for (int k = std::max(0u, i - match_distance); k < end; k++)
            if (!s2_matches[k] && s1[i] == s2[k])
            {
                s1_matches[i] = true;
                s2_matches[k] = true;
                matches++;
                break;
            }
    }
    if (matches == 0)
        return 0.0;
    double t = 0.0;
    uint k = 0;
    for (uint i = 0; i < l1; i++)
        if (s1_matches[i])
        {
            while (!s2_matches[k]) k++;
            if (s1[i] != s2[k]) t += 0.5;
            k++;
        }

    const double m = matches;
    return (m / l1 + m / l2 + (m - t) / m) / 3.0;
}

int main() {
    using namespace std;
    cout << jaro("MARTHA", "MARHTA") << endl;
    cout << jaro("DIXON", "DICKSONX") << endl;
    cout << jaro("JELLYFISH", "SMELLYFISH") << endl;
    return 0;
}
```



## Clojure


```clojure

(ns test-project-intellij.core
  (:gen-class))

(defn find-matches [s t]
  " find match locations in the two strings "
  " s_matches is set to true wherever there is a match in t and t_matches is set conversely "
  (let [s_len (count s)
        t_len (count t)
        match_distance (int (- (/ (max s_len t_len) 2) 1))
        matches 0
        transpositions 0
        fn-start (fn [i] (max 0 (- i match_distance)))              ; function to compute starting position
        fn-end (fn [i] (min (+ i match_distance 1) (- t_len 1))) ]  ; function to compute end position
    (loop [i 0
           start (fn-start i)
           end (fn-end i)
           k start
           s_matches (vec (repeat (count s) false))
           t_matches (vec (repeat (count t) false))
           matches 0]

      (if (< i s_len)

        (if (<= k end)

          (if (get t_matches k)
            ; continue with next k
            (recur i start end (inc k) s_matches t_matches matches)

            (if (= (get s i) (get t k))
              ; match a position, so update matches, s_matches, t_matches to reflect match
              (recur (inc i) (fn-start (inc i)) (fn-end (inc i)) (fn-start (inc i)) (assoc s_matches i true) (assoc t_matches k true) (inc matches))
              ; no match so try next k
              (recur i start end (inc k) s_matches t_matches matches)))

          ; End of k iterations, so increment i and set k to start based upon i
          (recur (inc i) (fn-start (inc i)) (fn-end (inc i)) (fn-start (inc i)) s_matches t_matches matches))

        ; End of i iterations
        [matches s_matches t_matches]))))

(defn count-transpositions [s t s_matches t_matches]
  " Utility function to count the number of transpositions "
  (let [s_len (count s)]
    (loop [i 0
           k 0
           transpositions 0]

      (if (< i s_len)
        ; still elements in s (since i < s_len)
        (if (not (get s_matches i nil))
          ; skip to next i since there are no matches in s
          (recur (inc i) k transpositions)
          ; checking for match in t
          (if (not (get t_matches k nil))
            ; keeping looping around as long as there are no matches in t
            (recur i (inc k) transpositions)
            (if (not= (get s i) (get t k))
              ; increment transposition count (if strings don't equal at match location)
              (recur (inc i) (inc k) (inc transpositions))
              ; was a match, so advance i and k without increasing transpositions count
              (recur (inc i) (inc k) transpositions))))
        ; Return count
        transpositions))))

(defn jaro [s t]
  " Main Jaro Distance routine"
  (if (= s t)
    1
    (let [[matches s_matches t_matches]  (find-matches s t)]
      (if (= 0 matches)
        0
        (let [s_len (count s)
              t_len (count t)
              transpositions (count-transpositions s t s_matches t_matches)]
          (float (/ (+ (/ matches s_len) (/ matches t_len) (/ (- matches (/ transpositions 2)) matches)) 3)))))))


(println (jaro "MARTHA" "MARHTA"))
(println (jaro "DIXON" "DICKSONX"))
(println (jaro "JELLYFISH" "SMELLYFISH"))

```

{{out}}

```txt

0.9444444
0.76666665
0.8962963

```



## COBOL

{{trans|Java}}

```cobol

       identification division.
       program-id. JaroDistance.

       environment division.
       configuration section.
       repository.
           function length intrinsic
           function trim intrinsic
           function max intrinsic
           function min intrinsic
           .

       data division.
       working-storage section.
       77  s                      pic x(255).
       77  t                      pic x(255).
       77  s-length               pic 9(3).
       77  t-length               pic 9(3).
       77  i                      pic 9(3).
       77  j                      pic 9(3).
       77  k                      pic 9(3).
       77  start-pos              pic 9(3).
       77  end-pos                pic 9(3).
       77  match-distance         pic 9(3).
       77  matches                pic 9(3).
       77  transpositions         pic 9(3).
       77  distance               pic 9v9(8).

       01  jaro-table.
           05 filler              occurs 255.
              10 filler           pic 9(1).
                 88 s-matches     value 1 when set to false is 0.
              10 filler           pic 9(1).
                 88 t-matches     value 1 when set to false is 0.

       procedure division.
       main.
           move "MARTHA" to s
           move "MARHTA" to t
           perform jaro-calc-and-show
           move "DIXON" to s
           move "DICKSONX" to t
           perform jaro-calc-and-show
           move "JELLYFISH" to s
           move "SMELLYFISH" to t
           perform jaro-calc-and-show
           stop run
           .
       jaro-calc-and-show.
           perform jaro-distance
           display trim(s) " -> " trim(t) ", distance=" distance
           .
       jaro-distance.
           move length(trim(s)) to s-length
           move length(trim(t)) to t-length
           if s-length = zeros and t-length = zeros
              move 1 to distance
              exit paragraph
           end-if

           compute match-distance = max(s-length, t-length) / 2 - 1
           move low-values to jaro-table
           move zeros to matches
           move zeros to transpositions
           perform varying i from 1 by 1 until i > s-length
              move max(1, i - match-distance) to start-pos
              move min(i + match-distance, t-length) to end-pos
              perform varying j from start-pos by 1 until j > end-pos
                 if t-matches(j) or s(i:1) <> t(j:1)
                    exit perform cycle
                 end-if,
                 set s-matches(i), t-matches(j) to true
                 add 1 to matches
                 exit perform
              end-perform
           end-perform
           if matches = zeros
              move matches to distance
              exit paragraph
           end-if

           move 1 to k
           perform varying i from 1 by 1 until i > s-length
              if not s-matches(i)
                 exit perform cycle
              end-if
              perform until t-matches(k)
                 add 1 to k
              end-perform
              if s(i:1) <> t(k:1)
                 add 1 to transpositions
              end-if
              add 1 to k
           end-perform

           compute distance = ((matches / s-length) + (matches / t-length) +
                               ((matches - transpositions / 2) / matches)) / 3
           .

```

{{out}}

```txt

MARTHA -> MARHTA, distance=0.94444444
DIXON -> DICKSONX, distance=0.76666666
JELLYFISH -> SMELLYFISH, distance=0.89629629

```



## CoffeeScript

{{trans|C++}}

```coffeescript
jaro = (s1, s2) ->
    l1 = s1.length
    l2 = s2.length
    if l1 == 0 then return if l2 == 0 then 1.0 else 0.0
    match_distance = Math.max(l1, l2) / 2 - 1
    s1_matches = []
    s2_matches = []
    m = 0
    for i in [0...l1]
        end = Math.min(i + match_distance + 1, l2)
        for k in [Math.max(0, i - match_distance)...end]
            if !s2_matches[k] and s1[i] == s2[k]
                s1_matches[i] = true
                s2_matches[k] = true
                m++
                break
    if m == 0
        0.0
    else
        t = 0.0
        k = 0
        for i in [0...l1]
            if s1_matches[i]
                until s2_matches[k] then k++
                if s1[i] != s2[k++] then t += 0.5
        (m / l1 + m / l2 + (m - t) / m) / 3.0

console.log jaro "MARTHA", "MARHTA"
console.log jaro "DIXON", "DICKSONX"
console.log jaro "JELLYFISH", "SMELLYFISH"
```

{{Out}}

```txt
0.9444444444444445
0.7666666666666666
0.8962962962962964
```



## Crystal

{{trans|Ruby}}

```ruby
def jaro(s, t)
    return 1.0 if s == t

    s_len = s.size
    t_len = t.size
    match_distance = ({s_len, t_len}.max / 2) - 1

    s_matches = Array.new(s_len, false)
    t_matches = Array.new(t_len, false)
    matches = 0.0

    s_len.times do |i|
        j_start = {0, i - match_distance}.max
        j_end = {i + match_distance, t_len - 1}.min

        (j_start..j_end).each do |j|
            t_matches[j] && next                # -> next if t_matches[j]
            s[i] == t[j] || next                # -> next unless s[i] == t[j]
            s_matches[i] = true
            t_matches[j] = true
            matches += 1.0
            break
        end
    end

    return 0.0 if matches == 0.0

    k = 0
    transpositions = 0.0
    s_len.times do |i|
        s_matches[i] || next                    # -> next unless s_matches[i]
        while ! t_matches[k]; k += 1 end        # -> k += 1 until t_matches[k]
        s[i] == t[k] || (transpositions += 1.0) # -> (transpositions += 1.0) unless s[i] == t[k]
        k += 1
    end

    ((matches / s_len) + (matches / t_len) +
    ((matches - transpositions / 2.0) / matches)) / 3.0
end

%w( MARTHA    MARHTA
    DIXON     DICKSONX
    JELLYFISH SMELLYFISH
  ).each_slice(2) { |(s ,t)| puts "jaro(#{s}, #{t}) = #{"%.10f" % jaro(s, t)}" }

```

{{out}}

```txt

jaro(MARTHA, MARHTA) = 0.9444444444
jaro(DIXON, DICKSONX) = 0.7666666667
jaro(JELLYFISH, SMELLYFISH) = 0.8962962963

```



## D

{{trans|Kotlin}}

```D
auto jaro(in string s1, in string s2) {
    int s1_len = cast(int) s1.length;
    int s2_len = cast(int) s2.length;
    if (s1_len == 0 && s2_len == 0) return 1;

    import std.algorithm.comparison: min, max;
    auto match_distance = max(s1_len, s2_len) / 2 - 1;
    auto s1_matches = new bool[s1_len];
    auto s2_matches = new bool[s2_len];
    int matches = 0;
    for (auto i = 0; i < s1_len; i++) {
        auto start = max(0, i - match_distance);
        auto end = min(i + match_distance + 1, s2_len);
        for (auto j = start; j < end; j++)
            if (!s2_matches[j] && s1[i] == s2[j]) {
                s1_matches[i] = true;
                s2_matches[j] = true;
                matches++;
                break;
            }
    }
    if (matches == 0) return 0;

    auto t = 0.0;
    auto k = 0;
    for (auto i = 0; i < s1_len; i++)
        if (s1_matches[i]) {
            while (!s2_matches[k]) k++;
            if (s1[i] != s2[k++]) t += 0.5;
        }
    double m = matches;
    return (m / s1_len + m / s2_len + (m - t) / m) / 3.0;
}

void main() {
    import std.stdio: writeln;
    writeln(jaro(   "MARTHA",      "MARHTA"));
    writeln(jaro(    "DIXON",    "DICKSONX"));
    writeln(jaro("JELLYFISH",  "SMELLYFISH"));
}
```


```txt
0.944444
0.766667
0.896296
```



## Elixir

{{trans|Ruby}}
{{works with|Elixir|1.3}}

```elixir
defmodule Jaro do
  def distance(s, t) when is_binary(s) and is_binary(t), do:
    distance(to_charlist(s), to_charlist(t))
  def distance(x, x), do: 1.0
  def distance(s, t) do
    s_len = length(s)
    t_len = length(t)
    {s_matches, t_matches, matches} = matching(s, t, s_len, t_len)
    if matches == 0 do
      0.0
    else
      {k, transpositions} = transposition(s, t, s_matches, t_matches)
      ((matches / s_len) +
       (matches / t_len) +
       ((matches - transpositions/2) / matches)) / 3
    end
  end

  defp matching(s, t, s_len, t_len) do
    match_distance = div(max(s_len, t_len), 2) - 1
    ac0 = {List.duplicate(false, s_len), List.duplicate(false, t_len), 0}
    Enum.reduce(0..s_len-1, ac0, fn i,acc ->
      j_start = max(0, i-match_distance)
      j_end = min(i+match_distance, t_len-1)
      Enum.reduce_while(j_start..j_end, acc, fn j,{sm,tm,m} ->
        if Enum.at(tm, j) or Enum.at(s, i) != Enum.at(t, j) do
          {:cont, {sm, tm, m}}
        else
          {:halt, { List.replace_at(sm, i, true),
                    List.replace_at(tm, j, true),
                    m + 1 }}
        end
      end)
    end)
  end

  defp transposition(s, t, s_matches, t_matches) do
    Enum.reduce(0..length(s)-1, {0,0}, fn i,{k,transpositions} ->
      if Enum.at(s_matches, i) do
        k = k + (Enum.drop(t_matches, k)
                 |> Enum.take_while(fn matche -> not matche end)
                 |> length)
        if Enum.at(s, i) == Enum.at(t, k), do: {k+1, transpositions},
                                         else: {k+1, transpositions+1}
      else
        {k, transpositions}
      end
    end)
  end
end

~w( MARTHA    MARHTA
    DIXON     DICKSONX
    JELLYFISH SMELLYFISH )c
|> Enum.chunk(2)
|> Enum.each(fn [s,t] ->
     :io.format "jaro(~s, ~s) = ~.10f~n", [inspect(s), inspect(t), Jaro.distance(s, t)]
   end)
```


{{out}}

```txt

jaro('MARTHA', 'MARHTA') = 0.9444444444
jaro('DIXON', 'DICKSONX') = 0.7666666667
jaro('JELLYFISH', 'SMELLYFISH') = 0.8962962963

```


Elixir has a built-in function (<code>String.jaro_distance</code>).


## Factor

{{works with|Factor|0.99 development release 2019-03-17+}}

```factor
USING: formatting fry generalizations kernel locals make math
math.order sequences sequences.extras ;
IN: rosetta-code.jaro-distance

: match? ( s1 s2 n -- ? )
    [ pick nth swap indices nip ] [ 2nip ]
    [ drop [ length ] bi@ max 2/ 1 - ] 3tri
    '[ _ - abs _ <= ] any? ;

: matches ( s1 s2 -- seq )
    over length <iota> [
        [ [ nip swap nth ] [ match? ] 3bi [ , ] [ drop ] if ]
        2with each
    ] "" make ;

: transpositions ( s1 s2 -- n )
    2dup swap [ matches ] 2bi@ [ = not ] 2count 2/ ;

:: jaro ( s1 s2 -- x )
    s1 s2 matches length :> m
    s1 length            :> |s1|
    s2 length            :> |s2|
    s1 s2 transpositions :> t
    m zero? [ 0 ] [ m |s1| / m |s2| / m t - m / + + 1/3 * ] if ;

: jaro-demo ( -- )
    "DWAYNE" "DUANE"
    "MARTHA" "MARHTA"
    "DIXON" "DICKSONX"
    "JELLYFISH" "SMELLYFISH" [
        2dup jaro dup >float "%u %u jaro -> %u (~%.5f)\n" printf
    ] 2 4 mnapply ;

MAIN: jaro-demo
```

{{out}}

```txt

"DWAYNE" "DUANE" jaro -> 37/45 (~0.82222)
"MARTHA" "MARHTA" jaro -> 17/18 (~0.94444)
"DIXON" "DICKSONX" jaro -> 23/30 (~0.76667)
"JELLYFISH" "SMELLYFISH" jaro -> 121/135 (~0.89630)

```



## FreeBASIC


```freebasic
' version 09-10-2016
' compile with: fbc -s console

#Macro max(x, y)
  IIf((x) > (y), (x), (y))
#EndMacro

#Macro min(x, y)
  IIf((x) < (y), (x), (y))
#EndMacro

Function jaro(word1 As String, word2 As String) As Double

  If Len(word1) > Len(word2) Then Swap word1, word2

  Dim As Long i, j, j1, m, t
  Dim As Long s1 = Len(word1)
  Dim As Long s2 = Len(word2)
  Dim As Long max_dist = s2 \ 2 -1  ' integer division

  For i = 0 To s1 -1
    If word1[i] = word2[j] Then
      m = m +1
      word2[j] = 32
    Else
      For j1 = max(0, i - max_dist) To min(s2 -1, i + max_dist)
        If word1[i] = word2[j1] Then
          t = t +1
          m = m +1
          word2[j1] = 32
         If j1 > j Then j = j1
        End If
      Next
    End If
    j = j + 1
  Next

  If m = 0 Then Return 0

  t = t \ 2
  Return (m / s1 + m / s2 + ((m - t) / m)) / 3

End Function

' ------=< MAIN >=------

Print
Print " jaro (MARTHA,    MARHTA)     ="; jaro("MARTHA", "MARHTA")
Print " jaro (DIXON,     DICKSONX)   ="; jaro("DIXON", "DICKSONX")
Print " jaro (JELLYFISH, SMELLYFISH) ="; jaro("JELLYFISH", "SMELLYFISH")


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
 jaro (MARTHA,    MARHTA)     = 0.9444444444444444
 jaro (DIXON,     DICKSONX)   = 0.7666666666666667
 jaro (JELLYFISH, SMELLYFISH) = 0.8962962962962963
```



## Go


```go
package main

import "fmt"

func jaro(str1, str2 string) float64 {
    if len(str1) == 0 && len(str2) == 0 {
        return 1
    }
    if len(str1) == 0 || len(str2) == 0 {
        return 0
    }
    match_distance := len(str1)
    if len(str2) > match_distance {
        match_distance = len(str2)
    }
    match_distance = match_distance/2 - 1
    str1_matches := make([]bool, len(str1))
    str2_matches := make([]bool, len(str2))
    matches := 0.
    transpositions := 0.
    for i := range str1 {
        start := i - match_distance
        if start < 0 {
            start = 0
        }
        end := i + match_distance + 1
        if end > len(str2) {
            end = len(str2)
        }
        for k := start; k < end; k++ {
            if str2_matches[k] {
                continue
            }
            if str1[i] != str2[k] {
                continue
            }
            str1_matches[i] = true
            str2_matches[k] = true
            matches++
            break
        }
    }
    if matches == 0 {
        return 0
    }
    k := 0
    for i := range str1 {
        if !str1_matches[i] {
            continue
        }
        for !str2_matches[k] {
            k++
        }
        if str1[i] != str2[k] {
            transpositions++
        }
        k++
    }
    transpositions /= 2
    return (matches/float64(len(str1)) +
        matches/float64(len(str2)) +
        (matches-transpositions)/matches) / 3
}

func main() {
    fmt.Printf("%f\n", jaro("MARTHA", "MARHTA"))
    fmt.Printf("%f\n", jaro("DIXON", "DICKSONX"))
    fmt.Printf("%f\n", jaro("JELLYFISH", "SMELLYFISH"))
}
```

{{out}}

```txt

0.944444
0.766667
0.896296

```



## Haskell


```Haskell
import Data.List (sortBy, elemIndex, intercalate)
import Data.Ord (comparing)
import Text.Printf (printf)
import Data.Maybe (mapMaybe)

jaro :: Ord a => [a] -> [a] -> Float
jaro x y =
  let f = (fromIntegral . length)
      [m, t] = [f, fromIntegral . transpositions] <*> [matches x y]
      [s1, s2] = [f] <*> [x, y]
  in case m of
       0 -> 0
       _ -> (1 / 3) * ((m / s1) + (m / s2) + ((m - t) / m))

matches :: Eq a => [a] -> [a] -> [(Int, a)]
matches s1 s2 =
  let [(l1, xs), (l2, ys)] =
        sortBy (comparing fst) ((length >>= (,)) <$> [s1, s2])
      r = quot l2 2 - 1
  in mapMaybe
       (\(c, n)
         -- Initial chars out of range ?
          ->
           let offset = max 0 (n - (r + 1))
               -- Any offset for this char within range.
           in elemIndex c (drop offset (take (n + r) ys)) >>=
              (\i -> Just (offset + i, c)))
       (zip xs [1 ..])

transpositions :: Ord a => [(Int, a)] -> Int
transpositions = length . filter (uncurry (>)) . (zip <*> tail)

-- TEST ----------------------------------------------------------------------
main :: IO ()
main =
  mapM_ putStrLn $
  (\(s1, s2) -> intercalate " -> " [s1, s2, printf "%.3f\n" $ jaro s1 s2]) <$>
  [ ("DWAYNE", "DUANE")
  , ("MARTHA", "MARHTA")
  , ("DIXON", "DICKSONX")
  , ("JELLYFISH", "SMELLYFISH")
  ]
```

{{Out}}

```txt
DWAYNE -> DUANE -> 0.822

MARTHA -> MARHTA -> 0.944

DIXON -> DICKSONX -> 0.767

JELLYFISH -> SMELLYFISH -> 0.896
```



## Haxe

{{trans|Kotlin}}
{{works with|Neko|2.1.0}}

```Haxe
class Jaro {
    private static function jaro(s1: String, s2: String): Float {
        var s1_len = s1.length;
        var s2_len = s2.length;
        if (s1_len == 0 && s2_len == 0) return 1;

        var match_distance = Std.int(Math.max(s1_len, s2_len)) / 2 - 1;
        var matches = { s1: [for(n in 0...s1_len) false], s2: [for(n in 0...s2_len) false] };
        var m = 0;
        for (i in 0...s1_len) {
            var start = Std.int(Math.max(0, i - match_distance));
            var end = Std.int(Math.min(i + match_distance + 1, s2_len));
            for (j in start...end)
                if (!matches.s2[j] && s1.charAt(i) == s2.charAt(j)) {
	                matches.s1[i] = true;
	                matches.s2[j] = true;
	                m++;
	                break;
                }
        }
        if (m == 0) return 0;

        var k = 0;
        var t = 0.;
        for (i in 0...s1_len)
            if (matches.s1[i]) {
            	while (!matches.s2[k]) k++;
            	if (s1.charAt(i) != s2.charAt(k++)) t += 0.5;
            }

        return (m / s1_len + m / s2_len + (m - t) / m) / 3.0;
    }

    public static function main() {
        Sys.println(jaro(   "MARTHA",      "MARHTA"));
        Sys.println(jaro(    "DIXON",    "DICKSONX"));
        Sys.println(jaro("JELLYFISH",  "SMELLYFISH"));
    }
}
```

{{Out}}

```txt
0.944444444444445
0.766666666666667
0.896296296296296
```



## J


Implementation:


```J
jaro=: dyad define
  d=. ((x >.&# y)%2)-1
  e=. (x =/y) * d >: |x -/&(i.@#) y
  xm=. (+./"1 e)#x
  ym=. (+./"2 e)#y
  m=. xm <.&# ym
  t=. (+/xm ~:&(m&{.) ym)%2
  s1=. #x
  s2=. #y
  ((m%s1)+(m%s2)+(m-t)%m)%3
)
```


Task examples:


```J
   'MARTHA' jaro 'MARHTA'
0.944444
   'DIXON' jaro 'DICKSONX'
0.766667
   'JELLYFISH' jaro 'SMELLYFISH'
0.896296
```



## Java


```java
public class JaroDistance {
    public static double jaro(String s, String t) {
        int s_len = s.length();
        int t_len = t.length();

        if (s_len == 0 && t_len == 0) return 1;

        int match_distance = Integer.max(s_len, t_len) / 2 - 1;

        boolean[] s_matches = new boolean[s_len];
        boolean[] t_matches = new boolean[t_len];

        int matches = 0;
        int transpositions = 0;

        for (int i = 0; i < s_len; i++) {
            int start = Integer.max(0, i-match_distance);
            int end = Integer.min(i+match_distance+1, t_len);

            for (int j = start; j < end; j++) {
                if (t_matches[j]) continue;
                if (s.charAt(i) != t.charAt(j)) continue;
                s_matches[i] = true;
                t_matches[j] = true;
                matches++;
                break;
            }
        }

        if (matches == 0) return 0;

        int k = 0;
        for (int i = 0; i < s_len; i++) {
            if (!s_matches[i]) continue;
            while (!t_matches[k]) k++;
            if (s.charAt(i) != t.charAt(k)) transpositions++;
            k++;
        }

        return (((double)matches / s_len) +
                ((double)matches / t_len) +
                (((double)matches - transpositions/2.0) / matches)) / 3.0;
    }

    public static void main(String[] args) {
        System.out.println(jaro(   "MARTHA",      "MARHTA"));
        System.out.println(jaro(    "DIXON",    "DICKSONX"));
        System.out.println(jaro("JELLYFISH",  "SMELLYFISH"));
    }
}
```

{{out}}

```txt

0.9444444444444445
0.7666666666666666
0.8962962962962964

```


## jq


    def jaro(s1; s2):

      def when(p; q): if p then q else . end;

      (s1|length) as $len1
      | (s2|length) as $len2
      | (( [$len1, $len2] | max ) / 2 - 1) as $match_standard
      | {m:0, p:0}
      | reduce range(0; $len1) as $l1
        (.; s1[$l1:$l1+1] as $t1
            | reduce range(0; $len2) as $l2
              (.; s2[$l2:$l2+1] as $t2
                  | when( $t1 == $t2;
                          when( ($l2-$l1) <= $match_standard and ($l1-$l2) <= $match_standard;
    		                .m+=1)
                          | when($l2 == $l1; .p += 1) ) ) )
      | ((.m-.p)/2) as $t
      | ( (.m/$len1) + (.m/$len2) + ((.m-$t)/.m) ) / 3
    ;

    jaro("MARTHA";"MARHTA")
    , jaro("DIXON"; "DICKSONX")
    , jaro("JELLYFISH";"SMELLYFISH")

Output:

```txt

    0.9444444444444444
    0.6833333333333332
    0.8870370370370371

```



## Julia

{{works with|Julia|0.6}}

```julia
function jarodistance(s1::AbstractString, s2::AbstractString)
    m = t = p = l1 = l2 = 0
    matchstd = max(length(s1), length(s2)) / 2 - 1
    for i in s1[1:end]
        l1 += 1
        l2 = 0
        for j in s2[1:end]
            l2 += 1
            if i == j
                if abs(l2 - l1) ≤ matchstd m += 1 end
                if l2 == l1 p += 1 end
            end
        end
    end
    t = (m - p) / 2
    d = 1 / 3 * (m / length(s1) + m / length(s2) + (m - t) / m)
    return d
end

const testcouples = (("MARTHA", "MARHTA"), ("DIXON", "DICKSONX"), ("JELLYFISH", "SMELLYFISH"))
for (s1, s2) in testcouples
    println("jarodistance(\"$s1\", \"$s2\") = ", @sprintf "%2.2f" jarodistance(s1, s2))
end
```


{{out}}

```txt
jarodistance("MARTHA", "MARHTA") = 0.94
jarodistance("DIXON", "DICKSONX") = 0.68
jarodistance("JELLYFISH", "SMELLYFISH") = 0.89
```



## Kotlin

{{trans|Java}}

```scala
object Jaro {
    fun distance(s1: String, s2: String): Double {
        val s1_len = s1.length
        val s2_len = s2.length
        if (s1_len == 0 && s2_len == 0) return 1.0
        val match_distance = Math.max(s1_len, s2_len) / 2 - 1
        val s1_matches = BooleanArray(s1_len)
        val s2_matches = BooleanArray(s2_len)
        var matches = 0
        for (i in 0..s1_len - 1) {
            val start = Math.max(0, i - match_distance)
            val end = Math.min(i + match_distance + 1, s2_len)
            (start..end - 1).find { j -> !s2_matches[j] && s1[i] == s2[j] } ?. let {
                s1_matches[i] = true
                s2_matches[it] = true
                matches++
            }
        }
        if (matches == 0) return 0.0
        var t = 0.0
        var k = 0
        (0..s1_len - 1).filter { s1_matches[it] }.forEach { i ->
            while (!s2_matches[k]) k++
            if (s1[i] != s2[k]) t += 0.5
            k++
        }

        val m = matches.toDouble()
        return (m / s1_len + m / s2_len + (m - t) / m) / 3.0
    }
}

fun main(args: Array<String>) {
    println(Jaro.distance("MARTHA", "MARHTA"))
    println(Jaro.distance("DIXON", "DICKSONX"))
    println(Jaro.distance("JELLYFISH", "SMELLYFISH"))
}
```



## Objeck

{{trans|Java}}

```objeck
class JaroDistance  {
  function : Main(args : String[]) ~ Nil {
    Jaro("MARTHA", "MARHTA")->PrintLine();
    Jaro("DIXON", "DICKSONX")->PrintLine();
    Jaro("JELLYFISH", "SMELLYFISH")->PrintLine();
  }

  function : Jaro(s : String, t : String) ~ Float {
    s_len := s->Size();
    t_len := t->Size();

    if (s_len = 0 & t_len = 0) { return 1; };

    match_distance := Int->Max(s_len, t_len) / 2 - 1;

    s_matches := Bool->New[s_len];
    t_matches := Bool->New[t_len];

    matches := 0;
    transpositions := 0;

    for (i := 0; i < s_len; i++;) {
      start := Int->Max(0, i-match_distance);
      end := Int->Min(i+match_distance+1, t_len);

      for (j := start; j < end; j++;) {
        if (t_matches[j]) { continue; };
        if (s->Get( i) <> t->Get( j)) { continue; };
        s_matches[i] := true;
        t_matches[j] := true;
        matches++;
        break;
      };
    };

    if (matches = 0) { return 0; };

    k := 0;
    for (i := 0; i < s_len; i++;) {
      if (<>s_matches[i]) { continue; };
      while (<>t_matches[k]) { k++; };
      if (s->Get( i) <> t->Get( k)) { transpositions++; };
      k++;
    };

    return ((matches->As(Float) / s_len) +
        (matches->As(Float) / t_len) +
        ((matches->As(Float) - transpositions/2.0) / matches)) / 3.0;
  }
}
```


{{output}}

```txt

0.944444
0.766667
0.896296

```



## PARI/GP

This version was translated from Java and Perl.

{{Works with|PARI/GP|2.7.4 and above}}


```parigp

\\Jaro distance between 2 strings s1 and s2.
\\ 4/12/16 aev
jaroDist(s1,s2)={
my(vt1=Vecsmall(s1),vt2=Vecsmall(s2),n1=#s1,n2=#s2,d,
   md=max(n1,n2)\2-1,cs,ce,mc=0,tr=0,k=1,ds,
   s1m=vector(n1,z,0),s2m=vector(n2,z,0));
if(!n1||!n2, return(0));
for(i=1,n1,
  cs=max(1,i-md);
  ce=min(i+md+1,n2);
  for(j=cs,ce,
    if(s2m[j],next);
    if(vt1[i]!=vt2[j], next);
    mc++; s1m[i]=1; s2m[j]=1; break;
  );\\fend j
);\\fend i
if(!mc, return(0));
for(i=1,n1,
  if(!s1m[i], next);
  while(!s2m[k], k++);
  if(vt1[i]!=vt2[k], tr++);
  k++
);\\fend i
d=(mc/n1+mc/n2+(mc-tr/2)/mc)/3.0;
ds=Strprintf("%.5f",d);
print(" *** Jaro distance is: ",ds," for strings: ",s1,", ",s2);
return(d);
}

{ \\ Testing:
jaroDist("MARTHA","MARHTA");
jaroDist("DIXON","DICKSONX");
jaroDist("JELLYFISH","SMELLYFISH");
jaroDist("DWAYNE","DUANE");
}

```


{{Output}}


```txt

 *** Jaro distance is: 0.94444 for strings: MARTHA, MARHTA
 *** Jaro distance is: 0.76667 for strings: DIXON, DICKSONX
 *** Jaro distance is: 0.89630 for strings: JELLYFISH, SMELLYFISH
 *** Jaro distance is: 0.82222 for strings: DWAYNE, DUANE

```



## Pascal


```pascal

//converted from C source by /u/bleuge
function ssJaroWinkler(s1,s2:string):double;
var
  l1,l2,match_distance,matches,i,k,trans:integer;
  bs1,bs2:array[1..255] of boolean; //used to avoid getmem, max string length is 255
begin
  l1:=length(s1);
  l2:=length(s2);
  fillchar(bs1,sizeof(bs1),0); //set booleans to false
  fillchar(bs2,sizeof(bs2),0);
  if l1=0 then
    if l2=0 then exit(1)
    else exit(0);
  match_distance:=(max(l1,l2) div 2)-1;
  matches:=0;
  trans:=0;
  for i := 1 to l1 do
  begin
    for k := max(1,i-match_distance) to min(i+match_distance,l2) do
    begin
      if bs2[k] then continue;
      if s1[i]<>s2[k] then continue;
      bs1[i]:=true;
      bs2[k]:=true;
      inc(matches);
      break;
    end;
  end;
  if matches=0 then exit(0);
  k:=1;
  for i := 1 to l1 do
  begin
    if (bs1[i]=false) then continue;
    while (bs2[k]=false) do inc(k);
    if s1[i]<>s2[k] then inc(trans);
    inc(k);
  end;
  trans:=trans div 2;
  result:=((matches/l1)+(matches/l2)+((matches-trans)/matches))/3;
end;
//test
 writeln(formatfloat('0.######',ssJaroWinkler('DWAYNE','DUANE')));
 writeln(formatfloat('0.######',ssJaroWinkler('MARTHA','MARHTA')));
 writeln(formatfloat('0.######',ssJaroWinkler('DIXON','DICKSONX')));
 writeln(formatfloat('0.######',ssJaroWinkler('JELLYFISH','SMELLYFISH')));

```

{{out}}

```txt

0,822222
0,944444
0,766667
0,896296

```



## Perl


```perl
use List::Util qw(min max);

sub jaro {
    my ($s, $t) = @_;

    my $s_len = length($s);
    my $t_len = length($t);

    return 1 if $s_len == 0 and $t_len == 0;

    my $match_distance = int(max($s_len, $t_len) / 2) - 1;

    my @s_matches;
    my @t_matches;

    my @s = split(//, $s);
    my @t = split(//, $t);

    my $matches = 0;
    foreach my $i (0 .. $#s) {

        my $start = max(0, $i - $match_distance);
        my $end = min($i + $match_distance + 1, $t_len);

        foreach my $j ($start .. $end - 1) {
            $t_matches[$j] and next;
            $s[$i] eq $t[$j] or next;
            $s_matches[$i] = 1;
            $t_matches[$j] = 1;
            $matches++;
            last;
        }
    }

    return 0 if $matches == 0;

    my $k              = 0;
    my $transpositions = 0;

    foreach my $i (0 .. $#s) {
        $s_matches[$i] or next;
        until ($t_matches[$k]) { ++$k }
        $s[$i] eq $t[$k] or ++$transpositions;
        ++$k;
    }

    (($matches / $s_len) + ($matches / $t_len) +
        (($matches - $transpositions / 2) / $matches)) / 3;
}

printf("%f\n", jaro("MARTHA",    "MARHTA"));
printf("%f\n", jaro("DIXON",     "DICKSONX"));
printf("%f\n", jaro("JELLYFISH", "SMELLYFISH"));
```

{{out}}

```txt

0.944444
0.766667
0.896296

```



## Perl 6

{{trans|Perl}}

```perl6
sub jaro ($s, $t) {

    return 1 if $s eq $t;

    my $s_len = + my @s = $s.comb;
    my $t_len = + my @t = $t.comb;

    my $match_distance = ($s_len max $t_len) div 2 - 1;

    my @s_matches;
    my @t_matches;
    my $matches = 0;

    for ^@s -> $i {

        my $start = 0 max $i - $match_distance;
        my $end = $i + $match_distance min $t_len;

        for $start .. $end -> $j {
            @t_matches[$j] and next;
            @s[$i] eq @t[$j] or next;
            @s_matches[$i] = 1;
            @t_matches[$j] = 1;
            $matches++;
            last;
        }
    }

    return 0 if $matches == 0;

    my $k              = 0;
    my $transpositions = 0;

    for ^@s -> $i {
        @s_matches[$i] or next;
        until @t_matches[$k] { ++$k }
        @s[$i] eq @t[$k] or ++$transpositions;
        ++$k;
    }

    ($matches / $s_len + $matches / $t_len +
        (($matches - $transpositions / 2) / $matches)) / 3;
}

printf("%f\n", jaro("MARTHA",    "MARHTA"));
printf("%f\n", jaro("DIXON",     "DICKSONX"));
printf("%f\n", jaro("JELLYFISH", "SMELLYFISH"));
```

{{out}}

```txt

0.944444
0.766667
0.896296

```



## Phix


```Phix
function jaro(string str1, str2)
    str1 = trim(upper(str1))
    str2 = trim(upper(str2))
    integer len1 = length(str1),
            len2 = length(str2),
            match_distance = floor(max(len1,len2)/2)-1,
            match_count = 0,
            half_transposed = 0

    if len1==0 then return len2==0 end if

    -- count the number of matches
    sequence m1 = repeat(false,len1),
             m2 = repeat(false,len2)
    for i=1 to len1 do
        for k=max(1,i-match_distance)
           to min(len2,i+match_distance) do
            if not m2[k] then
                if str1[i]=str2[k] then
                    m1[i] = true
                    m2[k] = true
                    match_count += 1
                    exit
                end if
            end if
        end for
    end for

    if match_count==0 then return 0 end if

    -- count the number of half-transpositions
    integer k = 1
    for i=1 to len1 do
        if m1[i] then
            while not m2[k] do k += 1 end while
            half_transposed += (str1[i]!=str2[k])
            k += 1
        end if
    end for
    integer transpositions = floor(half_transposed/2),
            not_transposed = match_count - transpositions
    --
    -- return the average of:
    --   percentage/fraction of the first string matched,
    --   percentage/fraction of the second string matched, and
    --   percentage/fraction of matches that were not transposed.
    --
    return (match_count/len1 +
            match_count/len2 +
            not_transposed/match_count)/3
end function

constant testcouples = {{"CRATE","TRACE"},
                        {"JONES","JOHNSON"},
                        {"ABCVWXYZ","CABVWXYZ"},
                        {"DWAYNE","DUANE"},
                        {"MARTHA", "MARHTA"},
                        {"DIXON", "DICKSONX"},
                        {"JELLYFISH", "SMELLYFISH"}}

for i=1 to length(testcouples) do
    string {s1, s2} = testcouples[i]
    printf(1,"%f <== jaro(\"%s\", \"%s\")\n",{jaro(s1,s2),s1,s2})
end for
```

{{out}}

```txt

0.733333 <== jaro("CRATE", "TRACE")
0.790476 <== jaro("JONES", "JOHNSON")
0.958333 <== jaro("ABCVWXYZ", "CABVWXYZ")
0.822222 <== jaro("DWAYNE", "DUANE")
0.944444 <== jaro("MARTHA", "MARHTA")
0.766667 <== jaro("DIXON", "DICKSONX")
0.896296 <== jaro("JELLYFISH", "SMELLYFISH")

```



## Python



### Procedural


{{Works with|Python|3}}

```python
'''Jaro distance'''

from __future__ import division


def jaro(s, t):
    '''Jaro distance between two strings.'''
    s_len = len(s)
    t_len = len(t)

    if s_len == 0 and t_len == 0:
        return 1

    match_distance = (max(s_len, t_len) // 2) - 1

    s_matches = [False] * s_len
    t_matches = [False] * t_len

    matches = 0
    transpositions = 0

    for i in range(s_len):
        start = max(0, i - match_distance)
        end = min(i + match_distance + 1, t_len)

        for j in range(start, end):
            if t_matches[j]:
                continue
            if s[i] != t[j]:
                continue
            s_matches[i] = True
            t_matches[j] = True
            matches += 1
            break

    if matches == 0:
        return 0

    k = 0
    for i in range(s_len):
        if not s_matches[i]:
            continue
        while not t_matches[k]:
            k += 1
        if s[i] != t[k]:
            transpositions += 1
        k += 1

    return ((matches / s_len) +
            (matches / t_len) +
            ((matches - transpositions / 2) / matches)) / 3


def main():
    '''Tests'''

    for s, t in [('MARTHA', 'MARHTA'),
                 ('DIXON', 'DICKSONX'),
                 ('JELLYFISH', 'SMELLYFISH')]:
        print("jaro(%r, %r) = %.10f" % (s, t, jaro(s, t)))


if __name__ == '__main__':
    main()
```

{{out}}

```txt
jaro('MARTHA', 'MARHTA') = 0.9444444444
jaro('DIXON', 'DICKSONX') = 0.7666666667
jaro('JELLYFISH', 'SMELLYFISH') = 0.8962962963
```



### Composition of pure functions


{{Trans|Haskell}}
{{Works with|Python|3}}

```python
'''Jaro distance between two strings'''

from functools import reduce
import itertools


# jaro :: String -> String -> Float
def jaro(x):
    '''The Jaro distance between two strings.'''
    def go(s1, s2):
        m, t = fanArrow(len)(transpositionSum)(
            matches(s1, s2)
        )
        return 0 if 0 == m else (
            (1 / 3) * ((m / len(s1)) + (m / len(s2)) + ((m - t) / m))
        )
    return lambda y: go(x, y)


# main :: IO ()
def main():
    '''Tests'''

    print(
        tabulated('Jaro distances:\n')(str)(showPrecision(3))(
            uncurry(jaro)
        )([
            ("DWAYNE", "DUANE"),
            ("MARTHA", "MARHTA"),
            ("DIXON", "DICKSONX"),
            ("JELLYFISH", "SMELLYFISH")
        ])
    )


# JARO HELPER FUNCTIONS -----------------------------------


# transpositionSum :: [(Int, Char)] -> Int
def transpositionSum(xs):
    '''A count of the transpositions in xs.'''
    def f(a, xy):
        x, y = xy
        return 1 + a if fst(x) > fst(y) else a
    return reduce(f, zip(xs, xs[1:]), 0)


# matches :: String -> String -> [(Int, Char)]
def matches(s1, s2):
    '''A list of (Index, Char) correspondences
       between the two strings s1 and s2.'''

    [(_, xs), (l2, ys)] = sorted(map(
        fanArrow(len)(list), [s1, s2]
    ))
    r = l2 // 2 - 1

    # match :: (Int, (Char, Int)) -> (Int, Char)
    def match(a, nc):
        n, c = nc
        offset = max(0, n - (1 + r))

        def indexChar(x):
            return a + [(offset + x, c)]

        return maybe(a)(indexChar)(
            elemIndex(c)(
                drop(offset)(take(n + r)(ys))
            )
        )
    return reduce(match, enumerate(xs), [])


# GENERIC FUNCTIONS ---------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Function composition.'''
    return lambda f: lambda x: g(f(x))


# drop :: Int -> [a] -> [a]
# drop :: Int -> String -> String
def drop(n):
    '''The sublist of xs beginning at (zero-based) index n.'''
    def go(xs):
        if isinstance(xs, list):
            return xs[n:]
        else:
            take(n)(xs)
            return xs
    return lambda xs: go(xs)


# elemIndex :: Eq a => a -> [a] -> Maybe Int
def elemIndex(x):
    '''Just the index of the first element in xs
       which is equal to x,
       or Nothing if there is no such element.'''
    def go(xs):
        try:
            return Just(xs.index(x))
        except ValueError:
            return Nothing()
    return lambda xs: go(xs)


# fanArrow (&&&) :: (a -> b) -> (a -> c) -> (a -> (b, c))
def fanArrow(f):
    '''A tuple of the outputs of two separate functions
       applied to the same value.'''
    return lambda g: lambda x: (f(x), g(x))


# fst :: (a, b) -> a
def fst(tpl):
    '''First component of a tuple.'''
    return tpl[0]


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where the Maybe value is Just(x).'''
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# showPrecision Int -> Float -> String
def showPrecision(n):
    '''A string showing a floating point number
       at a given degree of precision.'''
    return lambda x: str(round(x, n))


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
                f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join(
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        )
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    islice = itertools.islice
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple derived from a curried function.'''
    return lambda xy: f(xy[0])(
        xy[1]
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Jaro distances:

        ('DWAYNE', 'DUANE') -> 0.822
       ('MARTHA', 'MARHTA') -> 0.944
      ('DIXON', 'DICKSONX') -> 0.767
('JELLYFISH', 'SMELLYFISH') -> 0.896
```



## Racket


{{trans|C}}
(kinda)

Returns an exact value for the Jaro distance.


```racket
#lang racket/base
;; {{trans|C}}
(require data/bit-vector)

(define (jaro-distance str1 str2)
  (define str1-len (string-length str1))
  (define str2-len (string-length str2))
  (cond
    [(and (zero? str1-len) (zero? str2-len)) 0]
    [(or  (zero? str1-len) (zero? str2-len)) 1]
    [else
     ;; vectors of bools that signify if that char in the matching string has a match
     (define str1-matches (make-bit-vector str1-len))
     (define str2-matches (make-bit-vector str2-len))
     (define matches
       ;; max distance between two chars to be considered matching
       (let ((match-distance (sub1 (quotient (max str1-len str2-len) 2))))
         (for/fold ((matches 0))
                   ((i (in-range 0 str1-len))
                    (c1 (in-string str1)))
           (define start (max 0 (- i match-distance)))
           (define end (min (+ i match-distance 1) str2-len))
           (for/fold ((matches matches))
                     ((k (in-range start end))
                      (c2 (in-string str2 start))
                      #:unless (bit-vector-ref str2-matches k) ; if str2 already has a match continue
                      #:when (char=? c1 c2) ; if str1 and str2 are not
                      #:final #t)
             ;; otherwise assume there is a match
             (bit-vector-set! str1-matches i #t)
             (bit-vector-set! str2-matches k #t)
             (add1 matches)))))
     (cond
       [(zero? matches) 0]
       [else
        (define-values (transpositions*2 k+)
          (for/fold ((transpositions 0) (k 0))
                    ((i (in-range 0 str1-len))
                     (c1 (in-string str1))
                     (b1 (in-bit-vector str1-matches))
                     ;; if there are no matches in str1 continue
                     #:when b1)
            (define k+ (for/first ((k+ (in-range k str2-len))
                                   (b2 (in-bit-vector str2-matches k))
                                   #:when b2)
                         k+))
            (values
             (+ transpositions (if (char=? c1 (string-ref str2 k+)) 0 1)) ; increment transpositions
             (add1 k+)))) ;; while there is no match in str2 increment k

        ;; divide the number of transpositions by two as per the algorithm specs
        ;; this division is valid because the counted transpositions include both
        ;; instances of the transposed characters.
        (define transpositions (quotient transpositions*2 2))

        ;; return the Jaro distance
        (/ (+ (/ matches str1-len)
              (/ matches str2-len)
              (/ (- matches transpositions) matches))
           3)])]))

(module+ test
  (jaro-distance "MARTHA"    "MARHTA"); 0.944444
  (exact->inexact (jaro-distance "MARTHA"    "MARHTA")); 0.944444
  (jaro-distance "DIXON"     "DICKSONX"); 0.766667
  (exact->inexact (jaro-distance "DIXON"     "DICKSONX")); 0.766667
  (jaro-distance "JELLYFISH" "SMELLYFISH"); 0.896296
  (exact->inexact (jaro-distance "JELLYFISH" "SMELLYFISH"))); 0.896296
```


{{out}}
The <code>exact->inexact</code> calls in the tests give an inexact, floating point version of the rational values.

```txt
17/18
0.9444444444444444
23/30
0.7666666666666667
121/135
0.8962962962962963
```



## REXX


```rexx
/*REXX program computes the  Jaro distance  between two strings  (or a list of strings).*/
@.=                                              /*define a default for the  @.  array. */
parse arg @.1                                    /*obtain an optional character string. */
if @.1=''  then do;  @.1= 'MARTHA     MARHTA'    /*nothing specified?  Use the defaults.*/
                     @.2= 'DIXON      DICKSONX'
                     @.3= 'JELLYFISH  SMELLYFISH'
                     @.4= 'DWAYNE     DUANE'
                end                              /* [↑]  embedded blanks are list as is.*/
      do j=1  while @.j\==''                     /*process all the strings in the list. */
      d= jaroDist(@.j)
      say 'Jaro distance is  '          format(d, , 5)        " for strings:  "        @.j
      end   /*j*/                                /* └──── digits past the decimal point.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
jaroDist: procedure; arg s.1 s.2 .;       L1= length(s.1);      L2= length(s.2);       m=0
          if L1==0 | L2==0  then return 0        /*check if any string is a null string.*/
          f= max(L1, L2) % 2   -   1             /*calculate furthest distanced allowed.*/
          r.=0                                   /* [↓]  see if the char is near enough.*/
              do k=1  for L1;   p= pos( substr(s.1, k, 1), s.2,  max(1, k-f) );    r.k= p
              if p\==0 & abs(p-k)<=f  then m=m+1 /*if near enough, count it as a match. */
                                      else r.k=0 /*       ··· otherwise, don't count it.*/
              end   /*k*/
          t=0
              do o=1  for L1;         om= o - 1
              if pos( substr(s.1, o, 1), s.2)==0  |  r.o==0  |  r.om==0  then iterate
              if r.o<r.om  then t= t + 1
              end   /*o*/                        /* [↑]  count number of transpositions.*/

          if m==0  then return 0
                        return (m/L1  +  m/L2  +  (m-t)/m)   /   3
```

{{out|output|text=  when using the default inputs:}}

```txt

Jaro distance is   0.94444  for strings:   MARTHA     MARHTA
Jaro distance is   0.76667  for strings:   DIXON      DICKSONX
Jaro distance is   0.89630  for strings:   JELLYFISH  SMELLYFISH
Jaro distance is   0.82222  for strings:   DWAYNE     DUANE

```



## Ring


```ring

# Project : Jaro distance

decimals(12)

see " jaro (MARTHA, MARHTA)  = " +  jaro("MARTHA", "MARHTA") + nl
see " jaro (DIXON, DICKSONX) = " + jaro("DIXON", "DICKSONX") + nl
see " jaro (JELLYFISH, SMELLYFISH) = " + jaro("JELLYFISH", "SMELLYFISH") + nl

func jaro(word1, word2)
        if len(word1) > len(word2)
            swap(word1, word2)
        ok
        j = 1
        t = 0
        m = 0
        s1 = len(word1)
        s2 = len(word2)
        maxdist = (s2 / 2) -1
        for i = 1 to s1
             if word1[i] = word2[j] and j < max(len(word2), len(word2)) + 1
                m = m +1
                word2[j] = char(32)
             else
                for j1 = max(1, i - maxdist) to min(s2 -1, i + maxdist)
                     if word1[i] = word2[j1]
                        t = t +1
                        m = m +1
                        word2[j1] = char(32)
                        if j1 > j and j1 < max(len(word2), len(word2)) + 1
                           j = j1
                        ok
                    ok
                next
             ok
             if j < max(len(word2), len(word2))
                j = j + 1
             ok
        next
        if m = 0
           return 0
        ok
        t = floor(t / 2)
        return (m / s1 + m / s2 + ((m - t) / m)) / 3

func swap(a, b)
        temp = a
        a = b
        b = temp
        return [a, b]

```

Output:

```txt

jaro (MARTHA, MARHTA)  = 0.944444444444
jaro (DIXON, DICKSONX) = 0.766666666667
jaro (JELLYFISH, SMELLYFISH) = 0.896296296296

```



## Ruby


```ruby
def jaro(s, t)
    return 1.0 if s == t

    s_len = s.size
    t_len = t.size
    match_distance = ([s_len, t_len].max / 2) - 1

    s_matches = []
    t_matches = []
    matches = 0.0

    s_len.times do |i|
        j_start = [0, i-match_distance].max
        j_end = [i+match_distance, t_len-1].min

        (j_start..j_end).each do |j|
            t_matches[j] && next
            s[i] == t[j] || next
            s_matches[i] = true
            t_matches[j] = true
            matches += 1.0
            break
        end
    end

    return 0.0 if matches == 0.0

    k = 0
    transpositions = 0.0
    s_len.times do |i|
        s_matches[i] || next
        k += 1 until t_matches[k]
        s[i] == t[k] || (transpositions += 1.0)
        k += 1
    end

    ((matches / s_len) +
     (matches / t_len) +
     ((matches - transpositions/2.0) / matches)) / 3.0
end

%w(
    MARTHA    MARHTA
    DIXON     DICKSONX
    JELLYFISH SMELLYFISH
).each_slice(2) do |s,t|
    puts "jaro(#{s.inspect}, #{t.inspect}) = #{'%.10f' % jaro(s, t)}"
end
```

{{out}}

```txt

jaro("MARTHA", "MARHTA") = 0.9444444444
jaro("DIXON", "DICKSONX") = 0.7666666667
jaro("JELLYFISH", "SMELLYFISH") = 0.8962962963

```



## Rust

{{trans|C++}}

```rust
use std::cmp;

pub fn jaro(s1: &str, s2: &str) -> f64 {
    let s1_len = s1.len();
    let s2_len = s2.len();
    if s1_len == 0 && s2_len == 0 { return 1.0; }
    let match_distance = cmp::max(s1_len, s2_len) / 2 - 1;
    let mut s1_matches = vec![false; s1_len];
    let mut s2_matches = vec![false; s2_len];
    let mut m: isize = 0;
    for i in 0..s1_len {
        let start = cmp::max(0, i as isize - match_distance as isize) as usize;
        let end = cmp::min(i + match_distance + 1, s2_len);
        for j in start..end {
            if !s2_matches[j] && s1.as_bytes()[i] == s2.as_bytes()[j] {
                s1_matches[i] = true;
                s2_matches[j] = true;
                m += 1;
                break;
            }
        }
    }
    if m == 0 { return 0.0; }
    let mut t = 0.0;
    let mut k = 0;
    for i in 0..s1_len {
        if s1_matches[i] {
            while !s2_matches[k] { k += 1; }
            if s1.as_bytes()[i] != s2.as_bytes()[k] { t += 0.5; }
            k += 1;
        }
    }

    let m = m as f64;
    (m / s1_len as f64 + m / s2_len as f64 + (m  - t) / m) / 3.0
}

fn main() {
    let pairs = [("MARTHA", "MARHTA"), ("DIXON", "DICKSONX"), ("JELLYFISH", "SMELLYFISH")];
    for p in pairs.iter() { println!("{}/{} = {}", p.0, p.1, jaro(p.0, p.1)); }
}
```

{{Out}}

```txt
MARTHA/MARHTA = 0.9444444444444445
DIXON/DICKSONX = 0.7666666666666666
JELLYFISH/SMELLYFISH = 0.8962962962962964
```



## Scala

{{trans|Java}}

```scala
object Jaro extends App {

    def distance(s1: String, s2: String): Double = {
        val s1_len = s1.length
        val s2_len = s2.length
        if (s1_len == 0 && s2_len == 0) return 1.0
        val match_distance = Math.max(s1_len, s2_len) / 2 - 1
        val s1_matches = Array.ofDim[Boolean](s1_len)
        val s2_matches = Array.ofDim[Boolean](s2_len)
        var matches = 0
        for (i <- 0 until s1_len) {
            val start = Math.max(0, i - match_distance)
            val end = Math.min(i + match_distance + 1, s2_len)
            start until end find { j => !s2_matches(j) && s1(i) == s2(j) } match {
                case Some(j) =>
                    s1_matches(i) = true
                    s2_matches(j) = true
                    matches += 1
                case None =>
            }
        }
        if (matches == 0) return 0.0
        var t = 0.0
        var k = 0
        0 until s1_len filter s1_matches foreach { i =>
            while (!s2_matches(k)) k += 1
            if (s1(i) != s2(k)) t += 0.5
            k += 1
        }

        val m = matches.toDouble
        (m / s1_len + m / s2_len + (m - t) / m) / 3.0
    }

    val strings = List(("MARTHA", "MARHTA"), ("DIXON", "DICKSONX"), ("JELLYFISH", "SMELLYFISH"))
    strings.foreach { s => println(distance(s._1, s._2)) }
}
```



## Sidef


```ruby
func jaro(s, t) {

    return 1 if (s == t)

    var s_len = s.len
    var t_len = t.len

    var match_distance = ((s_len `max` t_len) // 2 - 1)

    var s_matches = []
    var t_matches = []

    var matches = 0
    var transpositions = 0

    for i (^s_len) {
        var start = (0 `max` i-match_distance)
        var end = (i+match_distance `min` t_len-1)

        for k (start..end) {
            t_matches[k] && next
            s[i] == t[k] || next
            s_matches[i] = true
            t_matches[k] = true
            matches++
            break
        }
    }

    return 0 if (matches == 0)

    var k = 0
    for i (^s_len) {
        s_matches[i] || next
        while (!t_matches[k]) { ++k }
        s[i] == t[k] || ++transpositions
        ++k
    }

    ((matches / s_len) +
      (matches / t_len) +
        ((matches - transpositions/2) / matches)) / 3
}

for pair in [
    [%c"MARTHA",    %c"MARHTA"],
    [%c"DIXON",     %c"DICKSONX"],
    [%c"JELLYFISH", %c"SMELLYFISH"],
] {
    say "jaro(#{pair.map{.join.dump}.join(', ')}) = #{'%.10f' % jaro(pair...)}"
}
```

{{out}}

```txt

jaro("MARTHA", "MARHTA") = 0.9444444444
jaro("DIXON", "DICKSONX") = 0.7666666667
jaro("JELLYFISH", "SMELLYFISH") = 0.8962962963

```



## Stata

Here we use the [https://ideas.repec.org/c/boc/bocode/s457850a.html jarowinkler] package from SSC. To install the package, type


```stata>ssc install jarowinkler</lang


Now the program for the task:


```stata
clear
input str20 a str20 b
DWAYNE DUANE
MARTHA MARHTA
DIXON DICKSONX
JELLYFISH SMELLYFISH
end

jarowinkler a b, gen(jw) jaroonly(jaro)
format %8.3f jaro
format %-20s a b
list a b jaro
```


'''Output'''


```txt
     +--------------------------------+
     | a           b             jaro |
     |--------------------------------|
  1. | DWAYNE      DUANE        0.822 |
  2. | MARTHA      MARHTA       0.944 |
  3. | DIXON       DICKSONX     0.767 |
  4. | JELLYFISH   SMELLYFISH   0.896 |
     +--------------------------------+
```


## Swift


```Swift
 func jaroWinklerMatch(_ s: String, _ t: String) -> Double {
    let s_len: Int = s.count
    let t_len: Int = t.count

    if s_len == 0 && t_len == 0 {
        return 1.0
    }

    if s_len == 0 || t_len == 0 {
        return 0.0
    }

    var match_distance: Int = 0

    if s_len == 1 && t_len == 1 {
        match_distance = 1
    } else {
        match_distance = ([s_len, t_len].max()!/2) - 1
    }


    var s_matches = [Bool]()
    var t_matches = [Bool]()

    for _ in 1...s_len {
        s_matches.append(false)
    }

    for _ in 1...t_len {
        t_matches.append(false)
    }

    var matches: Double = 0.0
    var transpositions: Double = 0.0

    for i in 0...s_len-1 {

        let start = [0, (i-match_distance)].max()!
        let end = [(i + match_distance), t_len-1].min()!

        if start > end {
            break
        }

        for j in start...end {

            if t_matches[j] {
                continue
            }

            if s[String.Index.init(encodedOffset: i)] != t[String.Index.init(encodedOffset: j)] {
                continue
            }
            // We must have a match
            s_matches[i] = true
            t_matches[j] = true
            matches += 1
            break
        }
    }

    if matches == 0 {
        return 0.0
    }

    var k = 0
    for i in 0...s_len-1 {
        if !s_matches[i] {
            continue
        }
        while !t_matches[k] {
            k += 1
        }
        if s[String.Index.init(encodedOffset: i)] != t[String.Index.init(encodedOffset: k)] {

            transpositions += 1
        }

        k += 1
    }

    let top = (matches / Double(s_len)) + (matches / Double(t_len)) + (matches - (transpositions / 2)) / matches
    return top/3
}

print("DWAYNE/DUANE:", jaroWinklerMatch("DWAYNE", "DUANE"))
print("MARTHA/MARHTA:", jaroWinklerMatch("MARTHA", "MARHTA"))
print("DIXON/DICKSONX:", jaroWinklerMatch("DIXON", "DICKSONX"))
print("JELLYFISH/SMELLYFISH:", jaroWinklerMatch("JELLYFISH", "SMELLYFISH"))

```


{{out}}

```txt

DWAYNE/DUANE: 0.822222222222222
MARTHA/MARHTA: 0.944444444444445
DIXON/DICKSONX: 0.766666666666667
JELLYFISH/SMELLYFISH: 0.896296296296296

```



## Tcl


```Tcl
proc jaro {s1 s2} {
    set l1 [string length $s1]
    set l2 [string length $s2]
    set dmax [expr {max($l1, $l2)/2 - 1}]   ;# window size to scan for matches
    set m1 {}                               ;# match indices
    set m2 {}
    for {set i 0} {$i < $l1} {incr i} {
        set jmin [expr {$i - $dmax}]        ;# don't worry about going out-of-bounds
        set jmax [expr {$i + $dmax}]        ;# because [string index] will return {} safely
        for {set j $jmin} {$j <= $jmax} {incr j} {
            if {$j in $m2} continue   ;# don't double-count matches
            if {[string index $s1 $i] eq [string index $s2 $j]} {
                lappend m1 $i
                lappend m2 $j
                break
            }
        }
    }
    set T 0                 ;# number of transpositions
    set oj -1
    foreach j $m2 {
        if {$j < $oj} {incr T}
        set oj $j
    }
    set T [expr {$T / 2.0}]
    set M [expr {1.0 * [llength $m1]}]  ;# number of matches
    expr { ( ($M / $l1) + ($M / $l2) + (($M - $T) / $M) ) / 3.0 }
}


foreach {s t} {
    DWAYNE DUANE
    MARTHA MARHTA
    DIXON  DICKSONX
    JELLYFISH SMELLYFISH
} {
    puts "[jaro $s $t]:\t$s / $t"
}
```


{{out}}

```txt
0.8222222222222223:     DWAYNE / DUANE
0.9722222222222222:     MARTHA / MARHTA
0.7666666666666666:     DIXON / DICKSONX
0.8962962962962964:     JELLYFISH / SMELLYFISH
```




## VBA


```vb

Option Explicit

Function JaroWinkler(text1 As String, text2 As String, Optional p As Double = 0.1) As Double
Dim dummyChar, match1, match2 As String
Dim i, f, t, j, m, l, s1, s2, limit As Integer

i = 1
Do
    dummyChar = Chr(i)
    i = i + 1
Loop Until InStr(1, text1 & text2, dummyChar, vbTextCompare) = 0

s1 = Len(text1)
s2 = Len(text2)
limit = WorksheetFunction.Max(0, Int(WorksheetFunction.Max(s1, s2) / 2) - 1)
match1 = String(s1, dummyChar)
match2 = String(s2, dummyChar)

For l = 1 To WorksheetFunction.Min(4, s1, s2)
    If Mid(text1, l, 1) <> Mid(text2, l, 1) Then Exit For
Next l
l = l - 1

For i = 1 To s1
    f = WorksheetFunction.Min(WorksheetFunction.Max(i - limit, 1), s2)
    t = WorksheetFunction.Min(WorksheetFunction.Max(i + limit, 1), s2)
    j = InStr(1, Mid(text2, f, t - f + 1), Mid(text1, i, 1), vbTextCompare)
    If j > 0 Then
        m = m + 1
        text2 = Mid(text2, 1, f + j - 2) & dummyChar & Mid(text2, f + j)
        match1 = Mid(match1, 1, i - 1) & Mid(text1, i, 1) & Mid(match1, i + 1)
        match2 = Mid(match2, 1, f + j - 2) & Mid(text1, i, 1) & Mid(match2, f + j)
    End If
Next i
match1 = Replace(match1, dummyChar, "", 1, -1, vbTextCompare)
match2 = Replace(match2, dummyChar, "", 1, -1, vbTextCompare)
t = 0
For i = 1 To m
    If Mid(match1, i, 1) <> Mid(match2, i, 1) Then t = t + 1
Next i

JaroWinkler = (m / s1 + m / s2 + (m - t / 2) / m) / 3
JaroWinkler = JaroWinkler + (1 - JaroWinkler) * l * WorksheetFunction.Min(0.25, p)
End Function

```



## zkl


```zkl
   //-->String of matched characters, ordered
fcn _jaro(str1,str2, matchDistance){
   cs:=Sink(String);
   foreach i,c in ([0..].zip(str1)){
      str2.find(c,(0).max(i - matchDistance),i + matchDistance) :
      if(Void!=_) cs.write(c);
   }
   cs.close()
}

fcn jaro(str1,str2){
   s1Len,s2Len,matchDistance := str1.len(), str2.len(), s1Len.max(s2Len)/2 - 1;
   cs12,cs21 := _jaro(str1,str2, matchDistance), _jaro(str2,str1, matchDistance);

   matches:=cs12.len().toFloat();
   if(not matches) return(0.0);
   transpositions:=cs12.walker().zipWith('!=,cs21).filter().sum(0)/2;

   ( matches/s1Len + matches/s2Len +
      ((matches - transpositions)/matches) ) / 3.0
}
```


```zkl
foreach s,t in (T(
     T("MARTHA","MARHTA"), T("DIXON","DICKSONX"), T("JELLYFISH","SMELLYFISH"))){
   println(0'|jaro("%s","%s") = %.10f|.fmt(s,t,jaro(s,t)));
}
```

{{out}}

```txt

jaro("MARTHA","MARHTA") = 0.9444444444
jaro("DIXON","DICKSONX") = 0.7666666667
jaro("JELLYFISH","SMELLYFISH") = 0.8962962963

```



## ZX Spectrum Basic

{{trans|FreeBASIC}}

```zxbasic
10 LET a$="MARTHA": LET b$="MARHTA": PRINT a$;", ";b$;": ";: GO SUB 1000: PRINT jaro
20 LET a$="DIXON": LET b$="DICKSONX": PRINT a$;", ";b$;": ";: GO SUB 1000: PRINT jaro
30 LET a$="JELLYFISH": LET b$="SMELLYFISH": PRINT a$;", ";b$;": ";: GO SUB 1000: PRINT jaro
900 STOP
1000 REM Jaro subroutine
1010 LET s1=LEN a$: LET s2=LEN b$: LET j=1: LET m=0: LET t=0
1030 IF s1>s2 THEN LET z$=a$: LET a$=b$: LET b$=z$: LET z=s1: LET s1=s2: LET s2=z
1035 LET maxdist=INT (s2/2)
1040 FOR i=1 TO s1
1050 IF a$(i)=b$(j) THEN LET m=m+1: LET b$(j)=" ": GO TO 2000
1080 FOR k=FN x(1,i-maxdist) TO FN n(s2,i+maxdist)
1090 IF a$(i)=b$(k) THEN LET t=t+1: LET m=m+1: LET b$(k)=" ": IF k>j THEN LET j=k
1100 NEXT k
2000 IF j<s2 THEN LET j=j+1:
2010 NEXT i
2020 IF m=0 THEN LET jaro=0: RETURN
2030 LET t=INT (t/2)
2040 LET jaro=(m/s1+m/s2+((m-t)/m))/3
2050 RETURN
5000 REM Functions
5010 DEF FN x(a,b)=(a AND a>b)+(b AND a<b)+(a AND a=b): REM max function
5020 DEF FN n(a,b)=(a AND a<b)+(b AND a>b)+(a AND a=b): REM min function
```

{{out}}

```txt
MARTHA, MARHTA: 0.94444444
DIXON, DICKSONX: 0.76666667
JELLYFISH, SMELLYFISH: 0.8962963
```


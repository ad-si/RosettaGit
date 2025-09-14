+++
title = "Almost prime"
description = ""
date = 2019-10-16T07:31:22Z
aliases = []
[extra]
id = 17227
[taxonomies]
categories = ["Prime Numbers", "task"]
tags = []
languages = [
  "11l",
  "ada",
  "algol_68",
  "arm_assembly",
  "autohotkey",
  "awk",
  "befunge",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "echolisp",
  "elixir",
  "erlang",
  "erre",
  "factor",
  "freebasic",
  "frink",
  "futhark",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "maple",
  "nim",
  "objeck",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "potion",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sequencel",
  "sidef",
  "tcl",
  "ubasic_4th",
  "vba",
  "vbscript",
  "visual_basic_.net",
  "xbasic",
  "yabasic",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

A   [[wp:Almost prime|k-Almost-prime]]   is a natural number   <math>n</math>   that is the product of   <math>k</math>   (possibly identical) primes.


;Example:
1-almost-primes,       where   <math>k=1</math>,   are the prime numbers themselves.

2-almost-primes,   where   <math>k=2</math>,   are the   [[Semiprime|semiprimes]].


;Task:
Write a function/method/subroutine/... that generates k-almost primes and use it to create a table here of the first ten members of k-Almost primes for   <math>1 <= K <= 5</math>.


;Related tasks:
*   [[Semiprime]]
*   [[:Category:Prime Numbers]]





## 11l

{{trans|Kotlin}}

```11l
F k_prime(k, =n)
   V f = 0
   V p = 2
   L f < k & p * p <= n
      L n % p == 0
         n /= p
         f++
      p++
   R f + (I n > 1 {1} E 0) == k

F primes(k, n)
   V i = 2
   Array[Int] list
   L list.len < n
      I k_prime(k, i)
         list.append(i)
      i++
   R list

L(k) 1..5
   print(‘k = ’k‘: ’, end' ‘’)
   print(primes(k, 10))
```

{{out}}

```txt
k = 1: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
k = 2: [4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
k = 3: [8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
k = 4: [16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
k = 5: [32, 48, 72, 80, 108, 112, 120, 162, 168, 176]
```



## Ada


This imports the package '''Prime_Numbers''' from [[Prime decomposition#Ada]].


```ada
with Prime_Numbers, Ada.Text_IO;

procedure Test_Kth_Prime is

   package Integer_Numbers is new
     Prime_Numbers (Natural, 0, 1, 2);
   use Integer_Numbers;

   Out_Length: constant Positive := 10; -- 10 k-th almost primes
   N: Positive; -- the "current number" to be checked

begin
   for K in 1 .. 5 loop
      Ada.Text_IO.Put("K =" & Integer'Image(K) &":  ");
      N := 2;
      for I in 1 .. Out_Length loop
	 while Decompose(N)'Length /= K loop
	    N := N + 1;
	 end loop; -- now N is Kth almost prime;
	 Ada.Text_IO.Put(Integer'Image(Integer(N)));
	 N := N + 1;
      end loop;
      Ada.Text_IO.New_Line;
   end loop;
end Test_Kth_Prime;
```


{{output}}
 K = 1:   2 3 5 7 11 13 17 19 23 29
 K = 2:   4 6 9 10 14 15 21 22 25 26
 K = 3:   8 12 18 20 27 28 30 42 44 45
 K = 4:   16 24 36 40 54 56 60 81 84 88
 K = 5:   32 48 72 80 108 112 120 162 168 176


## ALGOL 68

Worth noticing is the n(...)(...) picture in the printf and the WHILE ... DO SKIP OD idiom which is quite common in ALgol 68.

```algol68
BEGIN
   INT examples=10, classes=5;
   MODE SEMIPRIME = STRUCT ([examples]INT data, INT count);
   [classes]SEMIPRIME semi primes;
   PROC num facs = (INT n) INT :
COMMENT
   Return number of not necessarily distinct prime factors of n.
   Not very efficient for large n ...
COMMENT
   BEGIN
      INT tf := 2, residue := n, count := 1;
      WHILE tf < residue DO
	 INT remainder = residue MOD tf;
	 ( remainder = 0 | count +:= 1; residue %:= tf | tf +:= 1 )
      OD;
      count
   END;
   PROC update table = (REF []SEMIPRIME table, INT i) BOOL :
COMMENT
   Add i to the appropriate row of the table, if any, unless that row
   is already full. Return a BOOL which is TRUE when all of the table
   is full.
COMMENT
   BEGIN
      INT k := num facs(i);
      IF k <= classes
      THEN
	 INT c = 1 + count OF table[k];
	 ( c <= examples | (data OF table[k])[c] := i; count OF table[k] := c )
      FI;
      INT sum := 0;
      FOR i TO classes DO sum +:= count OF table[i] OD;
      sum < classes * examples
   END;
   FOR i TO classes DO count OF semi primes[i] := 0 OD;
   FOR i FROM 2 WHILE update table (semi primes, i) DO SKIP OD;
   FOR i TO classes
   DO
      printf (($"k = ", d, ":", n(examples)(xg(0))l$, i, data OF semi primes[i]))
   OD
END
```

{{out}}

```txt
k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program kprime.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

.equ MAXI,  10
.equ MAXIK,  5
/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessDeb:           .ascii "k="
sMessValeurDeb:     .fill 11, 1, ' '            @ size => 11

sMessResult:        .ascii " "
sMessValeur:        .fill 11, 1, ' '            @ size => 11

szCarriageReturn:   .asciz "\n"


/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program
    mov r3,#1                                     @ k
1:                                                @ start loop k
    mov r0,r3
    ldr r1,iAdrsMessValeurDeb
    bl conversion10                               @ call conversion decimal
    ldr r0,iAdrsMessValeurDeb
    mov r1,#':'
    strb r1,[r0,#2]                               @ write : after k value
    mov r1,#0
    strb r1,[r0,#3]                               @ final zéro
    ldr r0,iAdrsMessDeb
    bl affichageMess                              @ display message
    mov r4,#2                                     @ n
    mov r5,#0                                     @ result counter
2:                                                @ start loop n
    mov r0,r4
    mov r1,r3
    bl kprime                                     @ is kprine ?
    cmp r0,#0
    beq 3f                                        @ no
    mov r0,r4
    ldr r1,iAdrsMessValeur
    bl conversion10                               @ call conversion decimal
    ldr r0,iAdrsMessValeur
    mov r1,#0
    strb r1,[r0,#4]                               @ final zéro
    ldr r0,iAdrsMessResult
    bl affichageMess                              @ display message
    add r5,#1                                     @ increment counter
3:
    add r4,#1                                     @ increment n
    cmp r5,#MAXI                                  @ maxi ?
    blt 2b                                        @ no -> loop
    ldr r0,iAdrszCarriageReturn
    bl affichageMess                              @ display carriage return
    add r3,#1                                     @ increment k
    cmp r3,#MAXIK                                 @ maxi ?
    ble 1b                                        @ no -> loop

100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrsMessValeurDeb:       .int sMessValeurDeb
iAdrsMessDeb:             .int sMessDeb
/******************************************************************/
/*     compute kprime (n,k)                                       */
/******************************************************************/
/* r0 contains n */
/* r1 contains k */
kprime:
    push {r1-r7,lr}                                   @ save  registers
    mov r5,r0                                         @ save n
    mov r7,r1                                         @ save k
    mov r4,#0                                         @ counter product
    mov r1,#2                                         @ divisor
1:                                                    @ start loop
    cmp r4,r7                                         @ counter >= k
    bge 4f                                            @ yes -> end
    mul r6,r1,r1                                      @ compute product
    cmp r6,r5                                         @ > n
    bgt 4f                                            @ yes -> end
2:                                                    @ start loop division
    mov r0,r5                                         @ dividende
    bl division                                       @ by r1
    cmp r3,#0                                         @ remainder = 0 ?
    bne 3f                                            @ no
    mov r5,r2                                         @ yes -> n = n / r1
    add r4,#1                                         @ increment counter
    b 2b                                              @ and loop
3:
    add r1,#1                                         @ increment divisor
    b 1b                                              @ and loop
4:                                                    @ end compute
    cmp r5,#1                                         @ n > 1
    addgt r4,#1                                       @ yes increment counter
    cmp r4,r7                                         @ counter = k ?
    movne r0,#0                                       @ no -> no kprime
    moveq r0,#1                                       @ yes -> kprime
100:
    pop {r1-r7,lr}                                    @ restaur registers
    bx lr                                             @return
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
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD
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

<p>Output:</p>

```txt

k=1 : 2    3    5    7    11   13   17   19   23   29
k=2 : 4    6    9    10   14   15   21   22   25   26
k=3 : 8    12   18   20   27   28   30   42   44   45
k=4 : 16   24   36   40   54   56   60   81   84   88
k=5 : 32   48   72   80   108  112  120  162  168  176

```


## AutoHotkey

Translation of the C Version

```AutoHotkey
kprime(n,k) {
	p:=2, f:=0
	while( (f<k) && (p*p<=n) ) {
		while ( 0==mod(n,p) ) {
			n/=p
			f++
		}
		p++
	}
	return f + (n>1) == k
}

k:=1, results:=""
while( k<=5 ) {
	i:=2, c:=0, results:=results "k =" k ":"
	while( c<10 ) {
		if (kprime(i,k)) {
			results:=results " " i
			c++
		}
		i++
	}
	results:=results "`n"
	k++
}

MsgBox % results
```


'''Output (Msgbox):'''

```txt
k =1: 2 3 5 7 11 13 17 19 23 29
k =2: 4 6 9 10 14 15 21 22 25 26
k =3: 8 12 18 20 27 28 30 42 44 45
k =4: 16 24 36 40 54 56 60 81 84 88
k =5: 32 48 72 80 108 112 120 162 168 176
```


## AWK


```AWK

# syntax: GAWK -f ALMOST_PRIME.AWK
BEGIN {
    for (k=1; k<=5; k++) {
      printf("%d:",k)
      c = 0
      i = 1
      while (c < 10) {
        if (kprime(++i,k)) {
          printf(" %d",i)
          c++
        }
      }
      printf("\n")
    }
    exit(0)
}
function kprime(n,k,  f,p) {
    for (p=2; f<k && p*p<=n; p++) {
      while (n % p == 0) {
        n /= p
        f++
      }
    }
    return(f + (n > 1) == k)
}

```

<p>Output:</p>

```txt

1: 2 3 5 7 11 13 17 19 23 29
2: 4 6 9 10 14 15 21 22 25 26
3: 8 12 18 20 27 28 30 42 44 45
4: 16 24 36 40 54 56 60 81 84 88
5: 32 48 72 80 108 112 120 162 168 176

```


## Befunge

{{trans|C}}
The extra spaces are to ensure it's readable on buggy interpreters that don't include a space after numeric output.


```befunge>1
::48*"= k",,,,02p.":",01v
|^ v0!`\*:g40:<p402p300:+1<
K| >2g03g`*#v_ 1`03g+02g->|
F@>/03g1+03p>vpv+1\.:,*48 <
P#|!\g40%g40:<4>:9`>#v_\1^|
|^>#!1#`+#50#:^#+1,+5>#5$<|
```


{{out}}

```txt
k = 1 : 2  3  5  7  11  13  17  19  23  29
k = 2 : 4  6  9  10  14  15  21  22  25  26
k = 3 : 8  12  18  20  27  28  30  42  44  45
k = 4 : 16  24  36  40  54  56  60  81  84  88
k = 5 : 32  48  72  80  108  112  120  162  168  176
```



## C


```c
#include <stdio.h>

int kprime(int n, int k)
{
	int p, f = 0;
	for (p = 2; f < k && p*p <= n; p++)
		while (0 == n % p)
			n /= p, f++;

	return f + (n > 1) == k;
}

int main(void)
{
	int i, c, k;

	for (k = 1; k <= 5; k++) {
		printf("k = %d:", k);

		for (i = 2, c = 0; c < 10; i++)
			if (kprime(i, k)) {
				printf(" %d", i);
				c++;
			}

		putchar('\n');
	}

	return 0;
}
```

{{out}}

```txt

k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176

```



## C++

{{trans|Kotlin}}

```cpp
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <list>

bool k_prime(unsigned n, unsigned k) {
    unsigned f = 0;
    for (unsigned p = 2; f < k && p * p <= n; p++)
        while (0 == n % p) { n /= p; f++; }
    return f + (n > 1 ? 1 : 0) == k;
}

std::list<unsigned> primes(unsigned k, unsigned n)  {
    std::list<unsigned> list;
    for (unsigned i = 2;list.size() < n;i++)
        if (k_prime(i, k)) list.push_back(i);
    return list;
}

int main(const int argc, const char* argv[]) {
    using namespace std;
    for (unsigned k = 1; k <= 5; k++) {
        ostringstream os("");
        const list<unsigned> l = primes(k, 10);
        for (list<unsigned>::const_iterator i = l.begin(); i != l.end(); i++)
            os << setw(4) << *i;
        cout << "k = " << k << ':' << os.str() << endl;
    }

	return EXIT_SUCCESS;
}
```

{{out}}

```txt
k = 1:   2   3   5   7  11  13  17  19  23  29
k = 2:   4   6   9  10  14  15  21  22  25  26
k = 3:   8  12  18  20  27  28  30  42  44  45
k = 4:  16  24  36  40  54  56  60  81  84  88
k = 5:  32  48  72  80 108 112 120 162 168 176
```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace AlmostPrime
{
    class Program
    {
        static void Main(string[] args)
        {
            foreach (int k in Enumerable.Range(1, 5))
            {
                KPrime kprime = new KPrime() { K = k };
                Console.WriteLine("k = {0}: {1}",
                    k, string.Join<int>(" ", kprime.GetFirstN(10)));
            }
        }
    }

    class KPrime
    {
        public int K { get; set; }

        public bool IsKPrime(int number)
        {
            int primes = 0;
            for (int p = 2; p * p <= number && primes < K; ++p)
            {
                while (number % p == 0 && primes < K)
                {
                    number /= p;
                    ++primes;
                }
            }
            if (number > 1)
            {
                ++primes;
            }
            return primes == K;
        }

        public List<int> GetFirstN(int n)
        {
            List<int> result = new List<int>();
            for (int number = 2; result.Count < n; ++number)
            {
                if (IsKPrime(number))
                {
                    result.Add(number);
                }
            }
            return result;
        }
    }
}
```

{{out}}

```txt

k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176

```



## Clojure



```clojure


(ns clojure.examples.almostprime
	(:gen-class))

(defn divisors [n]
    " Finds divisors by looping through integers 2, 3,...i.. up to sqrt (n) [note: rather than compute sqrt(), test with i*i <=n] "
    (let [div (some #(if (= 0 (mod n %)) % nil) (take-while #(<= (* % %) n) (iterate inc 2)))]
        (if div                                                         ; div = nil (if no divisor found else its the divisor)
            (into [] (concat (divisors div) (divisors (/ n div))))      ; Concat the two divisors of the two divisors
            [n])))                                                      ; Number is prime so only itself as a divisor

(defn divisors-k [k n]
    " Finds n numbers with k divisors.  Does this by looping through integers 2, 3, ... filtering (passing) ones with k divisors and
      taking the first n "
    (->> (iterate inc 2)            ; infinite sequence of numbers starting at 2
         (map divisors)             ; compute divisor of each element of sequence
         (filter #(= (count %) k))  ; filter to take only elements with k divisors
         (take n)                   ; take n elements from filtered sequence
         (map #(apply * %))))       ; compute number by taking product of divisors

(println (for [k (range 1 6)]
          (println "k:" k (divisors-k k 10))))

}
```

{{out}}

```txt

(k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176)
nil

```



## Common Lisp


```lisp
(defun start ()
  (loop for k from 1 to 5
    do (format t "k = ~a: ~a~%" k (collect-k-almost-prime k))))

(defun collect-k-almost-prime (k &optional (d 2) (lst nil))
  (cond ((= (length lst) 10) (reverse lst))
        ((= (?-primality d) k) (collect-k-almost-prime k (+ d 1) (cons d lst)))
        (t (collect-k-almost-prime k (+ d 1) lst))))

(defun ?-primality (n &optional (d 2) (c 0))
  (cond ((> d (isqrt n)) (+ c 1))
        ((zerop (rem n d)) (?-primality (/ n d) d (+ c 1)))
        (t (?-primality n (+ d 1) c))))
```

{{out}}

```txt

k = 1: (2 3 5 7 11 13 17 19 23 29)
k = 2: (4 6 9 10 14 15 21 22 25 26)
k = 3: (8 12 18 20 27 28 30 42 44 45)
k = 4: (16 24 36 40 54 56 60 81 84 88)
k = 5: (32 48 72 80 108 112 120 162 168 176)
NIL
```



## D

This contains a copy of the function <code>decompose</code> from the Prime decomposition task.
{{trans|Ada}}

```d
import std.stdio, std.algorithm, std.traits;

Unqual!T[] decompose(T)(in T number) pure nothrow
in {
    assert(number > 1);
} body {
    typeof(return) result;
    Unqual!T n = number;

    for (Unqual!T i = 2; n % i == 0; n /= i)
        result ~= i;
    for (Unqual!T i = 3; n >= i * i; i += 2)
        for (; n % i == 0; n /= i)
            result ~= i;

    if (n != 1)
        result ~= n;
    return result;
}

void main() {
    enum outLength = 10; // 10 k-th almost primes.

    foreach (immutable k; 1 .. 6) {
        writef("K = %d: ", k);
        auto n = 2; // The "current number" to be checked.
        foreach (immutable i; 1 .. outLength + 1) {
            while (n.decompose.length != k)
                n++;
            // Now n is K-th almost prime.
            write(n, " ");
            n++;
        }
        writeln;
    }
}
```


{{out}}

```txt
K = 1: 2 3 5 7 11 13 17 19 23 29
K = 2: 4 6 9 10 14 15 21 22 25 26
K = 3: 8 12 18 20 27 28 30 42 44 45
K = 4: 16 24 36 40 54 56 60 81 84 88
K = 5: 32 48 72 80 108 112 120 162 168 176
```



## Delphi

{{trans|C}}


```Delphi

program AlmostPrime;

{$APPTYPE CONSOLE}

function IsKPrime(const n, k: Integer): Boolean;
var
  p, f, v: Integer;
begin
  f := 0;
  p := 2;
  v := n;
  while (f < k) and (p*p <= n) do begin
    while (v mod p) = 0 do begin
      v := v div p;
      Inc(f);
    end;
    Inc(p);
  end;
  if v > 1 then Inc(f);
  Result := f = k;
end;

var
  i, c, k: Integer;

begin
  for k := 1 to 5 do begin
    Write('k = ', k, ':');
    c := 0;
    i := 2;
    while c < 10 do begin
      if IsKPrime(i, k) then begin
        Write(' ', i);
        Inc(c);
      end;
      Inc(i);
    end;
    WriteLn;
  end;
end.

```

{{out}}

```txt
K = 1: 2 3 5 7 11 13 17 19 23 29
K = 2: 4 6 9 10 14 15 21 22 25 26
K = 3: 8 12 18 20 27 28 30 42 44 45
K = 4: 16 24 36 40 54 56 60 81 84 88
K = 5: 32 48 72 80 108 112 120 162 168 176

```



## EchoLisp

Small numbers : filter the sequence [ 2 .. n]

```scheme

(define (almost-prime? p k)
	(= k (length (prime-factors p))))

(define (almost-primes k nmax)
	(take (filter (rcurry almost-prime? k) [2 ..]) nmax))

(define (task (kmax 6) (nmax 10))
	(for ((k [1 .. kmax]))
		(write 'k= k '|)
		(for-each write (almost-primes k nmax))
		(writeln)))

```

{{out}}

```scheme

(task)

k= 1 | 2 3 5 7 11 13 17 19 23 29
k= 2 | 4 6 9 10 14 15 21 22 25 26
k= 3 | 8 12 18 20 27 28 30 42 44 45
k= 4 | 16 24 36 40 54 56 60 81 84 88
k= 5 | 32 48 72 80 108 112 120 162 168 176

```

Large numbers : generate - combinations with repetitions - k-almost-primes up to pmax.

```scheme

(lib 'match)
(define-syntax-rule (: v i) (vector-ref v i))
(reader-infix ':) ;; abbrev (vector-ref v i) === [v : i]


(lib 'bigint)
(define cprimes (list->vector (primes 10000)))

;; generates next k-almost-prime < pmax
;; c = vector of k primes indices c[i] <= c[j]
;; p = vector of intermediate products prime[c[0]]*prime[c[1]]*..
;; p[k-1] is the generated k-almost-prime
;; increment one c[i] at each step

(define (almost-next pmax k c p)
    (define almost-prime #f)
    (define cp 0)

    (for ((i (in-range (1- k) -1 -1))) ;; look backwards for c[i] to increment
        (vector-set! c i (1+ [c : i])) ;; increment c[i]
        (set! cp [cprimes : [c : i]])
        (vector-set! p i (if (> i 0) (* [ p : (1- i)] cp) cp)) ;; update partial product

        (when (< [p : i) pmax)
	    (set! almost-prime
            (and  ;; set followers to c[i] value
	       (for ((j (in-range (1+ i) k)))
	       (vector-set! c j [c : i])
	       (vector-set! p j (*  [ p : (1- j)] cp))
	       #:break (>= [p : j] pmax) => #f )
	       [p  : (1- k)]
	  ) ;; // and
	  ) ;; set!
	  ) ;; when
    #:break almost-prime
    ) ;; // for i
    almost-prime )

;; not sorted list of k-almost-primes < pmax
(define (almost-primes k nmax)
    (define base (expt 2 k)) ;; first one is 2^k
    (define pmax (* base nmax))
    (define c (make-vector k #0))
    (define p (build-vector k (lambda(i) (expt #2 (1+ i)))))

    (cons base
	(for/list
	((almost-prime (in-producer almost-next pmax k c p )))
	 almost-prime)))


```


{{out}}

```scheme

;; we want  500-almost-primes from the 10000-th.
(take (drop (list-sort < (almost-primes 500 10000)) 10000 ) 10)

(7241149198492252834202927258094752774597239286103014697435725917649659974371690699721153852986
440733637405206125678822081264723636566725108094369093648384
etc ...

;; The first one is 2^497 * 3 * 17 * 347 , same result as Haskell.


```





## Elixir

{{trans|Erlang}}

```elixir
defmodule Factors do
  def factors(n), do: factors(n,2,[])

  defp factors(1,_,acc), do: acc
  defp factors(n,k,acc) when rem(n,k)==0, do: factors(div(n,k),k,[k|acc])
  defp factors(n,k,acc)                 , do: factors(n,k+1,acc)

  def kfactors(n,k), do: kfactors(n,k,1,1,[])

  defp kfactors(_tn,tk,_n,k,_acc) when k == tk+1, do: IO.puts "done! "
  defp kfactors(tn,tk,_n,k,acc) when length(acc) == tn do
    IO.puts "K: #{k} #{inspect acc}"
    kfactors(tn,tk,2,k+1,[])
  end
  defp kfactors(tn,tk,n,k,acc) do
    case length(factors(n)) do
      ^k -> kfactors(tn,tk,n+1,k,acc++[n])
      _  -> kfactors(tn,tk,n+1,k,acc)
    end
  end
end

Factors.kfactors(10,5)
```


{{out}}

```txt

K: 1 [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
K: 2 [4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
K: 3 [8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
K: 4 [16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
K: 5 [32, 48, 72, 80, 108, 112, 120, 162, 168, 176]
done!
```



## Erlang

Using the factors function from  [[Prime_decomposition#Erlang]].


```erlang

-module(factors).
-export([factors/1,kfactors/0,kfactors/2]).

factors(N) ->
     factors(N,2,[]).

factors(1,_,Acc) -> Acc;
factors(N,K,Acc) when N rem K == 0 ->
    factors(N div K,K, [K|Acc]);
factors(N,K,Acc) ->
    factors(N,K+1,Acc).

kfactors() -> kfactors(10,5,1,1,[]).
kfactors(N,K) -> kfactors(N,K,1,1,[]).
kfactors(_Tn,Tk,_N,K,_Acc) when K == Tk+1 ->  io:fwrite("Done! ");
kfactors(Tn,Tk,N,K,Acc) when length(Acc) == Tn  ->
    io:format("K: ~w ~w ~n", [K, Acc]),
    kfactors(Tn,Tk,2,K+1,[]);

kfactors(Tn,Tk,N,K,Acc) ->
    case length(factors(N)) of K ->
     kfactors(Tn,Tk, N+1,K, Acc ++ [ N ] );
      _ ->
      kfactors(Tn,Tk, N+1,K, Acc) end.

```


{{out}}

```txt

9> factors:kfactors(10,5).
K: 1 [2,3,5,7,11,13,17,19,23,29]
K: 2 [4,6,9,10,14,15,21,22,25,26]
K: 3 [8,12,18,20,27,28,30,42,44,45]
K: 4 [16,24,36,40,54,56,60,81,84,88]
K: 5 [32,48,72,80,108,112,120,162,168,176]
Done! ok
10> factors:kfactors(15,10).
K: 1 [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
K: 2 [4,6,9,10,14,15,21,22,25,26,33,34,35,38,39]
K: 3 [8,12,18,20,27,28,30,42,44,45,50,52,63,66,68]
K: 4 [16,24,36,40,54,56,60,81,84,88,90,100,104,126,132]
K: 5 [32,48,72,80,108,112,120,162,168,176,180,200,208,243,252]
K: 6 [64,96,144,160,216,224,240,324,336,352,360,400,416,486,504]
K: 7 [128,192,288,320,432,448,480,648,672,704,720,800,832,972,1008]
K: 8 [256,384,576,640,864,896,960,1296,1344,1408,1440,1600,1664,1944,2016]
K: 9 [512,768,1152,1280,1728,1792,1920,2592,2688,2816,2880,3200,3328,3888,4032]
K: 10 [1024,1536,2304,2560,3456,3584,3840,5184,5376,5632,5760,6400,6656,7776,8064]
Done! ok

```



## ERRE


```ERRE

PROGRAM ALMOST_PRIME

!
! for rosettacode.org
!

!$INTEGER

PROCEDURE KPRIME(N,K->KP)
  LOCAL P,F
  FOR P=2 TO 999 DO
      EXIT IF NOT((F<K) AND (P*P<=N))
      WHILE (N MOD P)=0 DO
         N/=P
         F+=1
      END WHILE
  END FOR
  KP=(F-(N>1)=K)
END PROCEDURE

BEGIN
  PRINT(CHR$(12);)  !CLS
  FOR K=1 TO 5 DO
     PRINT("k =";K;":";)
     C=0
     FOR I=2 TO 999 DO
        EXIT IF NOT(C<10)
        KPRIME(I,K->KP)
        IF KP THEN
            PRINT(I;)
            C+=1
        END IF
     END FOR
     PRINT
  END FOR
END PROGRAM

```

{{out}}

```txt
K = 1: 2  3  5  7  11  13  17  19  23  29
K = 2: 4  6  9  10  14  15  21  22  25  26
K = 3: 8  12  18  20  27  28  30  42  44  45
K = 4: 16  24  36  40  54  56  60  81  84  88
K = 5: 32  48  72  80  108  112  120  162  168  176
```



## Factor


```factor
USING: formatting fry kernel lists lists.lazy locals
math.combinatorics math.primes.factors math.ranges sequences ;
IN: rosetta-code.almost-prime

: k-almost-prime? ( n k -- ? )
    '[ factors _ <combinations> [ product ] map ]
    [ [ = ] curry ] bi any? ;

:: first10 ( k -- seq )
    10 0 lfrom [ k k-almost-prime? ] lfilter ltake list>array ;

5 [1,b] [ dup first10 "K = %d: %[%3d, %]\n" printf ] each
```

{{out}}

```txt

K = 1: {   2,   3,   5,   7,  11,  13,  17,  19,  23,  29 }
K = 2: {   4,   6,   9,  10,  14,  15,  21,  22,  25,  26 }
K = 3: {   8,  12,  18,  20,  27,  28,  30,  42,  44,  45 }
K = 4: {  16,  24,  36,  40,  54,  56,  60,  81,  84,  88 }
K = 5: {  32,  48,  72,  80, 108, 112, 120, 162, 168, 176 }

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function kPrime(n As Integer, k As Integer) As Boolean
   Dim f As Integer = 0
   For i As Integer = 2 To n
     While n Mod i = 0
       If f = k Then Return false
       f += 1
       n \= i
     Wend
   Next
   Return f = k
End Function

Dim As Integer i, c, k
For k = 1 To 5
  Print "k = "; k; " : ";
  i = 2
  c = 0
  While c < 10
    If kPrime(i, k) Then
      Print Using "### "; i;
      c += 1
    End If
    i += 1
  Wend
  Print
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

k =  1 :   2   3   5   7  11  13  17  19  23  29
k =  2 :   4   6   9  10  14  15  21  22  25  26
k =  3 :   8  12  18  20  27  28  30  42  44  45
k =  4 :  16  24  36  40  54  56  60  81  84  88
k =  5 :  32  48  72  80 108 112 120 162 168 176

```



## Frink


```frink
for k = 1 to 5
{
   n=2
   count = 0
   print["k=$k:"]
   do
   {
      if length[factorFlat[n]] == k
      {
         print[" $n"]
         count = count + 1
      }
      n = n + 1
   } while count < 10

   println[]
}
```


Output:

```txt

k=1: 2 3 5 7 11 13 17 19 23 29
k=2: 4 6 9 10 14 15 21 22 25 26
k=3: 8 12 18 20 27 28 30 42 44 45
k=4: 16 24 36 40 54 56 60 81 84 88
k=5: 32 48 72 80 108 112 120 162 168 176

```



## Futhark



```Futhark

let kprime(n: i32, k: i32): bool =
  let (p,f) = (2, 0)
  let (n,_,f) = loop (n, p, f) while f < k && p*p <= n do
    let (n,f) = loop (n, f) while 0 == n % p do
      (n/p, f+1)
    in (n, p+1, f)
  in f + (if n > 1 then 1 else 0) == k

let main(m: i32): [][]i32 =
  let f k =
    let ps = replicate 10 0
    let (_,_,ps) = loop (i,c,ps) = (2,0,ps) while c < 10 do
      if kprime(i,k) then
        unsafe let ps[c] = i
               in (i+1, c+1, ps)
      else (i+1, c, ps)
    in ps
  in map f (1...m)

```


=={{header|F_Sharp|F#}}==

```fsharp
let rec genFactor (f, n) =
    if f > n then None
    elif n % f = 0 then Some (f, (f, n/f))
    else genFactor (f+1, n)


let factorsOf (num) =
    Seq.unfold (fun (f, n) -> genFactor (f, n)) (2, num)

let kFactors k = Seq.unfold (fun n ->
    let rec loop m =
        if Seq.length (factorsOf m) = k then m
        else loop (m+1)
    let next = loop n
    Some(next, next+1)) 2

[1 .. 5]
|> List.iter (fun k ->
        printfn "%A" (Seq.take 10 (kFactors k) |> Seq.toList))
```

{{out}}

```txt
[2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
[4; 6; 9; 10; 14; 15; 21; 22; 25; 26]
[8; 12; 18; 20; 27; 28; 30; 42; 44; 45]
[16; 24; 36; 40; 54; 56; 60; 81; 84; 88]
[32; 48; 72; 80; 108; 112; 120; 162; 168; 176]
```



=={{Header|Go}}==

```go
package main

import "fmt"

func kPrime(n, k int) bool {
    nf := 0
    for i := 2; i <= n; i++ {
        for n%i == 0 {
            if nf == k {
                return false
            }
            nf++
            n /= i
        }
    }
    return nf == k
}

func gen(k, n int) []int {
    r := make([]int, n)
    n = 2
    for i := range r {
        for !kPrime(n, k) {
            n++
        }
        r[i] = n
        n++
    }
    return r
}

func main() {
    for k := 1; k <= 5; k++ {
        fmt.Println(k, gen(k, 10))
    }
}
```

{{out}}

```txt

1 [2 3 5 7 11 13 17 19 23 29]
2 [4 6 9 10 14 15 21 22 25 26]
3 [8 12 18 20 27 28 30 42 44 45]
4 [16 24 36 40 54 56 60 81 84 88]
5 [32 48 72 80 108 112 120 162 168 176]

```



## Groovy


```Groovy

 public class almostprime
{
public static boolean kprime(int n,int k)
  {
    int i,div=0;
     for(i=2;(i*i <= n) && (div<k);i++)
      {
        while(n%i==0)
          {
            n = n/i;
            div++;
          }
      }
   return div + ((n > 1)?1:0) == k;
  }
  public static void main(String[] args)
    {
      int i,l,k;
       for(k=1;k<=5;k++)
        {
          println("k = " + k + ":");
           l = 0;
            for(i=2;l<10;i++)
              {
                if(kprime(i,k))
                {
                  print(i + " ");
                  l++;
                }
              }
          println();
        }
     }
}​

```

{{out}}

```txt

k = 1:
2 3 5 7 11 13 17 19 23 29
k = 2:
4 6 9 10 14 15 21 22 25 26
k = 3:
8 12 18 20 27 28 30 42 44 45
k = 4:
16 24 36 40 54 56 60 81 84 88
k = 5:
32 48 72 80 108 112 120 162 168 176

```


=={{header|GW-BASIC}}==
{{trans|FreeBASIC}}
{{works with|PC-BASIC|any}}

```qbasic

10  'Almost prime
20  FOR K% = 1 TO 5
30   PRINT "k = "; K%; ": ";
40   LET I% = 2
50   LET C% = 0
60   WHILE C% < 10
70    LET AN% = I%: LET AK% = K%: GOSUB 1000
80    IF ISKPRIME <> 0 THEN PRINT USING "### "; I%;: LET C% = C% + 1
90    LET I% = I% + 1
100  WEND
110  PRINT
120 NEXT K%
130 END

995  ' Check if n (AN%) is a k (AK%) prime
1000 LET F% = 0
1010 FOR II% = 2 TO AN%
1020  WHILE AN% MOD II% = 0
1030   IF F% = AK% THEN LET ISKPRIME = 0: RETURN
1040   LET F% = F% + 1
1050   LET AN% = AN% \ II%
1060  WEND
1070 NEXT II%
1080 LET ISKPRIME = (F% = AK%)
1090 RETURN

```

{{out}}

```txt

k =  1 :   2   3   5   7  11  13  17  19  23  29
k =  2 :   4   6   9  10  14  15  21  22  25  26
k =  3 :   8  12  18  20  27  28  30  42  44  45
k =  4 :  16  24  36  40  54  56  60  81  84  88
k =  5 :  32  48  72  80 108 112 120 162 168 176

```



## Haskell


```Haskell>isPrime :: Integral a =
 a -> Bool
isPrime n = not $ any ((0 ==) . (mod n)) [2..(truncate $ sqrt $ fromIntegral n)]

primes :: [Integer]
primes = filter isPrime [2..]

isKPrime :: (Num a, Eq a) => a -> Integer -> Bool
isKPrime 1 n = isPrime n
isKPrime k n = any (isKPrime (k - 1)) sprimes
  where
    sprimes = map fst $ filter ((0 ==) . snd) $ map (divMod n) $ takeWhile (< n) primes

kPrimes :: (Num a, Eq a) => a -> [Integer]
kPrimes k = filter (isKPrime k) [2..]

main :: IO ()
main = flip mapM_ [1..5] $ \k ->
  putStrLn $ "k = " ++ show k ++ ": " ++ (unwords $ map show (take 10 $ kPrimes k))
```

{{out}}

```txt
k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176
```


Larger ''k''s require more complicated methods:

```haskell
primes = 2:3:[n | n <- [5,7..], foldr (\p r-> p*p > n || rem n p > 0 && r)
	True (drop 1 primes)]

merge aa@(a:as) bb@(b:bs)
	| a < b = a:merge as bb
	| otherwise = b:merge aa bs

-- n-th item is all k-primes not divisible by any of the first n primes
notdivs k = f primes $ kprimes (k-1) where
	f (p:ps) s = map (p*) s : f ps (filter ((/=0).(`mod`p)) s)

kprimes k
	| k == 1 = primes
	| otherwise = f (head ndk) (tail ndk) (tail $ map (^k) primes) where
		ndk = notdivs k
		-- tt is the thresholds for merging in next sequence
		-- it is equal to "map head seqs", but don't do that
		f aa@(a:as) seqs tt@(t:ts)
			| a < t = a : f as seqs tt
			| otherwise = f (merge aa $ head seqs) (tail seqs) ts

main = do
	-- next line is for task requirement:
	mapM_ (\x->print (x, take 10 $ kprimes x)) [1 .. 5]

	putStrLn "\n10000th to 10100th 500-amost primes:"
	mapM_ print $ take 100 $ drop 10000 $ kprimes 500
```

{{out}}

```txt

(1,[2,3,5,7,11,13,17,19,23,29])
(2,[4,6,9,10,14,15,21,22,25,26])
(3,[8,12,18,20,27,28,30,42,44,45])
(4,[16,24,36,40,54,56,60,81,84,88])
(5,[32,48,72,80,108,112,120,162,168,176])

10000th to 10100th 500-amost primes:
7241149198492252834202927258094752774597239286103014697435725917649659974371690699721153852986440733637405206125678822081264723636566725108094369093648384
        <...snipped 99 more equally unreadable numbers...>

```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages.

```unicon
link "factors"

procedure main()
    every writes(k := 1 to 5,": ") do
        every writes(right(genKap(k),5)\10|"\n")
end

procedure genKap(k)
    suspend (k = *factors(n := seq(q)), n)
end
```


Output:

```txt

->ap
1:     2    3    5    7   11   13   17   19   23   29
2:     4    6    9   10   14   15   21   22   25   26
3:     8   12   18   20   27   28   30   42   44   45
4:    16   24   36   40   54   56   60   81   84   88
5:    32   48   72   80  108  112  120  162  168  176
->

```



## J


```J
   (10 {. [:~.[:/:~[:,*/~)^:(i.5)~p:i.10
 2  3  5  7  11  13  17  19  23  29
 4  6  9 10  14  15  21  22  25  26
 8 12 18 20  27  28  30  42  44  45
16 24 36 40  54  56  60  81  84  88
32 48 72 80 108 112 120 162 168 176
```

Explanation:
#Generate 10 primes.
#Multiply each of them by the first ten primes
#Sort and find unique values, take the first ten of those
#Multiply each of them by the first ten primes
#Sort and find unique values, take the first ten of those
:...
The results of the odd steps in this procedure are the desired result.


## Java


```java
public class AlmostPrime {
    public static void main(String[] args) {
        for (int k = 1; k <= 5; k++) {
            System.out.print("k = " + k + ":");

            for (int i = 2, c = 0; c < 10; i++) {
                if (kprime(i, k)) {
                    System.out.print(" " + i);
                    c++;
                }
            }

            System.out.println("");
        }
    }

    public static boolean kprime(int n, int k) {
        int f = 0;
        for (int p = 2; f < k && p * p <= n; p++) {
            while (n % p == 0) {
                n /= p;
                f++;
            }
        }
        return f + ((n > 1) ? 1 : 0) == k;
    }
}
```

{{out}}

```txt

k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176

```



## Javascript


```javascript
function almostPrime (n, k) {
    var divisor = 2, count = 0
    while(count < k + 1 && n != 1) {
        if (n % divisor == 0) {
            n = n / divisor
            count = count + 1
        } else {
            divisor++
        }
    }
    return count == k
}

for (var k = 1; k <= 5; k++) {
    document.write("
k=", k, ": ")
    var count = 0, n = 0
    while (count <= 10) {
        n++
        if (almostPrime(n, k)) {
            document.write(n, " ")
            count++
        }
    }
}
```

{{out}}

```txt

k=1: 2 3 5 7 11 13 17 19 23 29 31
k=2: 4 6 9 10 14 15 21 22 25 26 33
k=3: 8 12 18 20 27 28 30 42 44 45 50
k=4: 16 24 36 40 54 56 60 81 84 88 90
k=5: 32 48 72 80 108 112 120 162 168 176 180
```



## jq

{{Works with| jq|1.4}}
'''Infrastructure:'''

```jq
# Recent versions of jq (version > 1.4) have the following definition of "until":
def until(cond; next):
  def _until:
    if cond then . else (next|_until) end;
  _until;

# relatively_prime(previous) tests whether the input integer is prime
# relative to the primes in the array "previous":
def relatively_prime(previous):
  . as $in
  | (previous|length) as $plen
  # state: [found, ix]
  |  [false, 0]
  | until( .[0] or .[1] >= $plen;
           [ ($in % previous[.[1]]) == 0, .[1] + 1] )
  | .[0] | not ;

# Emit a stream in increasing order of all primes (from 2 onwards)
# that are less than or equal to mx:
def primes(mx):

  # The helper function, next, has arity 0 for tail recursion optimization;
  # it expects its input to be the array of previously found primes:
  def next:
     . as $previous
     | ($previous | .[length-1]) as $last
     | if ($last >= mx) then empty
       else ((2 + $last)
       | until( relatively_prime($previous) ; . + 2)) as $nextp
       | if $nextp <= mx
         then $nextp, (( $previous + [$nextp] ) | next)
	 else empty
         end
       end;
  if mx <= 1 then empty
  elif mx == 2 then 2
  else (2, 3, ( [2,3] | next))
  end
;

# Return an array of the distinct prime factors of . in increasing order
def prime_factors:

  # Return an array of prime factors of . given that "primes"
  # is an array of relevant primes:
  def pf(primes):
    if . <= 1 then []
    else . as $in
    | if ($in | relatively_prime(primes)) then [$in]
      else reduce primes[] as $p
             ([];
              if ($in % $p) != 0 then .
 	      else . + [$p] +  (($in / $p) | pf(primes))
	      end)
      end
      | unique
    end;

  if . <= 1 then []
  else . as $in
  | pf( [ primes( (1+$in) | sqrt | floor)  ] )
  end;

# Return an array of prime factors of . repeated according to their multiplicities:
def prime_factors_with_multiplicities:
  # Emit p according to the multiplicity of p
  # in the input integer assuming p > 1
  def multiplicity(p):
    if   .  < p     then empty
    elif . == p     then p
    elif (. % p) == 0 then
       ((./p) | recurse( if (. % p) == 0 then (. / p) else empty end) | p)
    else empty
    end;

  if . <= 1 then []
  else . as $in
  | prime_factors as $primes
  | if ($in|relatively_prime($primes)) then [$in]
    else reduce $primes[]  as $p
           ([];
            if ($in % $p) == 0 then . + [$in|multiplicity($p)] else . end )
    end
  end;
```

'''isalmostprime'''

```jq
def isalmostprime(k): (prime_factors_with_multiplicities | length) == k;

# Emit a stream of the first N almost-k primes
def almostprimes(N; k):
  if N <= 0 then empty
  else
    # state [remaining, candidate, answer]
    [N, 1, null]
    | recurse( if .[0] <= 0 then empty
	       elif (.[1] | isalmostprime(k)) then [.[0]-1, .[1]+1, .[1]]
	       else [.[0], .[1]+1, null]
               end)
    | .[2] | select(. != null)
  end;
```

 '''The task:'''

```jq
range(1;6) as $k | "k=\($k): \([almostprimes(10;$k)])"
```

{{out}}

```sh
$ jq -c -r -n -f Almost_prime.jq
k=1: [2,3,5,7,11,13,17,19,23,29]
k=2: [4,6,9,10,14,15,21,22,25,26]
k=3: [8,12,18,20,27,28,30,42,44,45]
k=4: [16,24,36,40,54,56,60,81,84,88]
k=5: [32,48,72,80,108,112,120,162,168,176]
```



## Julia

{{works with|Julia|1.1}}

```julia
using Primes

isalmostprime(n::Integer, k::Integer) = sum(values(factor(n))) == k

function almostprimes(N::Integer, k::Integer) # return first N almost-k primes
    P = Vector{typeof(k)}(undef,N)
    i = 0; n = 2
    while i < N
        if isalmostprime(n, k) P[i += 1] = n end
        n += 1
    end
    return P
end

for k in 1:5
    println("$k-Almost-primes: ", join(almostprimes(10, k), ", "), "...")
end
```


{{out}}

```txt
1-Almost-primes: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29...
2-Almost-primes: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26...
3-Almost-primes: 8, 12, 18, 20, 27, 28, 30, 42, 44, 45...
4-Almost-primes: 16, 24, 36, 40, 54, 56, 60, 81, 84, 88...
5-Almost-primes: 32, 48, 72, 80, 108, 112, 120, 162, 168, 176...
```



## Lua


```Lua
-- Returns boolean indicating whether n is k-almost prime
function almostPrime (n, k)
    local divisor, count = 2, 0
    while count < k + 1 and n ~= 1 do
        if n % divisor == 0 then
            n = n / divisor
            count = count + 1
        else
            divisor = divisor + 1
        end
    end
    return count == k
end

-- Generates table containing first ten k-almost primes for given k
function kList (k)
    local n, kTab = 2^k, {}
    while #kTab < 10 do
        if almostPrime(n, k) then
            table.insert(kTab, n)
        end
        n = n + 1
    end
    return kTab
end

-- Main procedure, displays results from five calls to kList()
for k = 1, 5 do
    io.write("k=" .. k .. ": ")
    for _, v in pairs(kList(k)) do
        io.write(v .. ", ")
    end
    print("...")
end
```

{{out}}

```txt
k=1: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, ...
k=2: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26, ...
k=3: 8, 12, 18, 20, 27, 28, 30, 42, 44, 45, ...
k=4: 16, 24, 36, 40, 54, 56, 60, 81, 84, 88, ...
k=5: 32, 48, 72, 80, 108, 112, 120, 162, 168, 176, ...
```



## Kotlin

{{trans|Java}}

```scala
fun Int.k_prime(x: Int): Boolean {
    var n = x
    var f = 0
    var p = 2
    while (f < this && p * p <= n) {
        while (0 == n % p) { n /= p; f++ }
        p++
    }
    return f + (if (n > 1) 1 else 0) == this
}

fun Int.primes(n : Int) : List<Int> {
    var i = 2
    var list = mutableListOf<Int>()
    while (list.size < n) {
        if (k_prime(i)) list.add(i)
        i++
    }
    return list
}

fun main(args: Array<String>) {
    for (k in 1..5)
        println("k = $k: " + k.primes(10))
}
```

{{out}}

```txt
k = 1: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
k = 2: [4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
k = 3: [8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
k = 4: [16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
k = 5: [32, 48, 72, 80, 108, 112, 120, 162, 168, 176]
```



## Liberty BASIC

{{trans|FreeBASIC}}
{{works with|Just BASIC}}

```lb

for k = 1 to 5
    print "k = "; k; ": ";
    i = 2
    c = 0
    while c < 10
        if kPrime(i, k) then
            print using("###", i); " ";
            c = c + 1
        end if
        i = i + 1
    wend
    print
next k
end

function kPrime(n, k)
    f = 0
    for i = 2 to n
    while n mod i = 0
        if f = k then kPrime = 0: exit function
        f = f + 1
        n = int(n / i)
    wend
    next i
    kPrime = abs(f = k)
end function

```

{{out}}

```txt

k = 1:   2   3   5   7  11  13  17  19  23  29
k = 2:   4   6   9  10  14  15  21  22  25  26
k = 3:   8  12  18  20  27  28  30  42  44  45
k = 4:  16  24  36  40  54  56  60  81  84  88
k = 5:  32  48  72  80 108 112 120 162 168 176

```



## Maple


```Maple
AlmostPrimes:=proc(k, numvalues::posint:=10)
    local aprimes, i, intfactors;
    aprimes := Array([]);
    i := 0;

    do
        i := i + 1;
        intfactors := ifactors(i)[2];
        intfactors := [seq(seq(intfactors[i][1], j=1..intfactors[i][2]),i = 1..numelems(intfactors))];
        if numelems(intfactors) = k then
            ArrayTools:-Append(aprimes,i);
        end if;
    until numelems(aprimes) = 10:
    aprimes;
end proc:
<seq( AlmostPrimes(i), i = 1..5 )>;
```

{{out}}

```txt

[[2, 3, 5, 7, 11, 13, 17, 19, 23, 29],
 [4, 6, 9, 10, 14, 15, 21, 22, 25, 26],
 [8, 12, 18, 20, 27, 28, 30, 42, 44, 45],
 [16, 24, 36, 40, 54, 56, 60, 81, 84, 88],
 [32, 48, 72, 80, 108, 112, 120, 162, 168, 176]]
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
kprimes[k_,n_] :=
  (* generates a list of the n smallest k-almost-primes *)
  Module[{firstnprimes, runningkprimes = {}},
  firstnprimes = Prime[Range[n]];
  runningkprimes = firstnprimes;
  Do[
   runningkprimes =
     Outer[Times, firstnprimes , runningkprimes ] // Flatten // Union  // Take[#, n] & ;
   (* only keep lowest n numbers in our running list *)
   , {i, 1, k - 1}];
  runningkprimes
  ]
(* now to create table with n=10 and k ranging from 1 to 5 *)
Table[Flatten[{"k = " <> ToString[i] <> ": ", kprimes[i, 10]}], {i,1,5}] // TableForm
```

{{out}}

```txt
k = 1: 	2	3	5	7	11	13	17	19	23	29
k = 2: 	4	6	9	10	14	15	21	22	25	26
k = 3: 	8	12	18	20	27	28	30	42	44	45
k = 4: 	16	24	36	40	54	56	60	81	84	88
k = 5: 	32	48	72	80	108	112	120	162	168	176
```


=={{header|Modula-2}}==

```modula2
MODULE AlmostPrime;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE KPrime(n,k : INTEGER) : BOOLEAN;
VAR p,f : INTEGER;
BEGIN
    f := 0;
    p := 2;
    WHILE (f<k) AND (p*p<=n) DO
        WHILE n MOD p = 0 DO
            n := n DIV p;
            INC(f)
        END;
        INC(p)
    END;
    IF n>1 THEN
        RETURN f+1 = k
    END;
    RETURN f = k
END KPrime;

VAR
    buf : ARRAY[0..63] OF CHAR;
    i,c,k : INTEGER;
BEGIN
    FOR k:=1 TO 5 DO
        FormatString("k = %i:", buf, k);
        WriteString(buf);

        i:=2;
        c:=0;
        WHILE c<10 DO
            IF KPrime(i,k) THEN
                FormatString(" %i", buf, i);
                WriteString(buf);
                INC(c)
            END;
            INC(i)
        END;

        WriteLn;
    END;

    ReadChar;
END AlmostPrime.
```



## Nim


```Nim
proc prime(k: int, listLen: int): seq[int] =
 result = @[]
 var
  test: int = 2
  curseur: int = 0
 while curseur < listLen:
  var
   i: int = 2
   compte = 0
   n = test
  while i <= n:
   if (n mod i)==0:
    n = n div i
    compte += 1
   else:
    i += 1
  if compte == k:
   result.add(test)
   curseur += 1
  test += 1

for k in 1..5:
 echo "k = ",k," : ",prime(k,10)
```

{{out}}

```txt
k = 1 : @[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
k = 2 : @[4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
k = 3 : @[8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
k = 4 : @[16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
k = 5 : @[32, 48, 72, 80, 108, 112, 120, 162, 168, 176]
```



## Objeck

{{trans|C}}

```objeck
class Kth_Prime {
  function : native : kPrime(n : Int, k : Int) ~ Bool {
    f := 0;
    for (p := 2; f < k & p*p <= n; p+=1;) {
      while (0 = n % p) {
        n /= p; f+=1;
      };
    };

    return f + ((n > 1) ? 1 : 0) = k;
  }

  function : Main(args : String[]) ~ Nil {
    for (k := 1; k <= 5; k+=1;) {
      "k = {$k}:"->Print();

      c := 0;
      for (i := 2; c < 10; i+=1;) {
        if (kPrime(i, k)) {
          " {$i}"->Print();
          c+=1;
        };
      };
      '\n'->Print();
    };
  }
}
```


{{out}}

```txt

k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176
```



## Oforth



```Oforth
: kprime?( n k -- b )
| i |
   0 2 n for: i [
      while( n i /mod swap 0 = ) [ ->n 1+ ] drop
      ]
   k ==
;

: table( k -- [] )
| l |
   Array new dup ->l
   2 while (l size 10 <>) [ dup k kprime? if dup l add then 1+ ]
   drop
;
```


{{out}}

```txt

>#[ table .cr ] 5 each
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
[4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
[8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
[16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
[32, 48, 72, 80, 108, 112, 120, 162, 168, 176]

```



## PARI/GP


```parigp
almost(k)=my(n); for(i=1,10,while(bigomega(n++)!=k,); print1(n", "));
for(k=1,5,almost(k);print)
```

{{out}}

```txt
2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
4, 6, 9, 10, 14, 15, 21, 22, 25, 26,
8, 12, 18, 20, 27, 28, 30, 42, 44, 45,
16, 24, 36, 40, 54, 56, 60, 81, 84, 88,
32, 48, 72, 80, 108, 112, 120, 162, 168, 176,
```


## Pascal

{{libheader|primTrial}}
{{works with|Free Pascal}}

```Pascal
program AlmostPrime;
{$IFDEF FPC}
  {$Mode Delphi}
{$ENDIF}
uses
  primtrial;
var
  i,K,cnt : longWord;
BEGIN
  K := 1;
  repeat
    cnt := 0;
    i := 2;
    write('K=',K:2,':');
    repeat
      if isAlmostPrime(i,K) then
      Begin
        write(i:6,' ');
        inc(cnt);
      end;
      inc(i);
    until cnt = 9;
    writeln;
    inc(k);
  until k > 10;
END.
```

;output:

```txt
K= 1 :    2     3     5     7    11    13    17    19    23    29
K= 2 :    4     6     9    10    14    15    21    22    25    26
K= 3 :    8    12    18    20    27    28    30    42    44    45
K= 4 :   16    24    36    40    54    56    60    81    84    88
K= 5 :   32    48    72    80   108   112   120   162   168   176
K= 6 :   64    96   144   160   216   224   240   324   336   352
K= 7 :  128   192   288   320   432   448   480   648   672   704
K= 8 :  256   384   576   640   864   896   960  1296  1344  1408
K= 9 :  512   768  1152  1280  1728  1792  1920  2592  2688  2816
K=10 : 1024  1536  2304  2560  3456  3584  3840  5184  5376  5632
```



## Perl

Using a CPAN module, which is simple and fast:
{{libheader|ntheory}}

```perl
use ntheory qw/factor/;
sub almost {
  my($k,$n) = @_;
  my $i = 1;
  map { $i++ while scalar factor($i) != $k; $i++ } 1..$n;
}
say "$_ : ", join(" ", almost($_,10)) for 1..5;
```

{{out}}

```txt

1 : 2 3 5 7 11 13 17 19 23 29
2 : 4 6 9 10 14 15 21 22 25 26
3 : 8 12 18 20 27 28 30 42 44 45
4 : 16 24 36 40 54 56 60 81 84 88
5 : 32 48 72 80 108 112 120 162 168 176

```

or writing everything by hand:

```perl
use strict;
use warnings;

sub k_almost_prime;

for my $k ( 1 .. 5 ) {
	my $almost = 0;
	print join(", ", map {
		1 until k_almost_prime ++$almost, $k;
		"$almost";
	} 1 .. 10), "\n";
}

sub nth_prime;

sub k_almost_prime {
	my ($n, $k) = @_;
	return if $n <= 1 or $k < 1;
	my $which_prime = 0;
	for my $count ( 1 .. $k ) {
		while( $n % nth_prime $which_prime ) {
			++$which_prime;
		}
		$n /= nth_prime $which_prime;
		return if $n == 1 and $count != $k;
	}
	($n == 1) ? 1 : ();
}

BEGIN {
	# This is loosely based on one of the python solutions
	# to the RC Sieve of Eratosthenes task.
	my @primes = (2, 3, 5, 7);
	my $p_iter = 1;
	my $p = $primes[$p_iter];
	my $q = $p*$p;
	my %sieve;
	my $candidate = $primes[-1] + 2;
	sub nth_prime {
		my $n = shift;
		return if $n < 0;
		OUTER: while( $#primes < $n ) {
			while( my $s = delete $sieve{$candidate} ) {
				my $next = $s + $candidate;
				$next += $s while exists $sieve{$next};
				$sieve{$next} = $s;
				$candidate += 2;
			}
			while( $candidate < $q ) {
				push @primes, $candidate;
				$candidate += 2;
				next OUTER if exists $sieve{$candidate};
			}
			my $twop = 2 * $p;
			my $next = $q + $twop;
			$next += $twop while exists $sieve{$next};
			$sieve{$next} = $twop;
			$p = $primes[++$p_iter];
			$q = $p * $p;
			$candidate += 2;
		}
		return $primes[$n];
	}
}
```

{{out}}

```txt
2, 3, 5, 7, 11, 13, 17, 19, 23, 29
4, 6, 9, 10, 14, 15, 21, 22, 25, 26
8, 12, 18, 20, 27, 28, 30, 42, 44, 45
16, 24, 36, 40, 54, 56, 60, 81, 84, 88
32, 48, 72, 80, 108, 112, 120, 162, 168, 176

```



## Perl 6

{{trans|C}}
{{works with|Rakudo|2015.12}}

```perl6
sub is-k-almost-prime($n is copy, $k) returns Bool {
    loop (my ($p, $f) = 2, 0; $f < $k && $p*$p <= $n; $p++) {
        $n /= $p, $f++ while $n %% $p;
    }
    $f + ($n > 1) == $k;
}

for 1 .. 5 -> $k {
    say ~.[^10]
        given grep { is-k-almost-prime($_, $k) }, 2 .. *
}
```

{{out}}

```txt
2 3 5 7 11 13 17 19 23 29
4 6 9 10 14 15 21 22 25 26
8 12 18 20 27 28 30 42 44 45
16 24 36 40 54 56 60 81 84 88
32 48 72 80 108 112 120 162 168 176
```

Here is a solution with identical output based on the <tt>factors</tt> routine from [[Count_in_factors#Perl_6]] (to be included manually until we decide where in the distribution to put it).


```perl6
constant @primes = 2, |(3, 5, 7 ... *).grep: *.is-prime;

multi sub factors(1) { 1 }
multi sub factors(Int $remainder is copy) {
    gather for @primes -> $factor {
        # if remainder < factor², we're done
        if $factor * $factor > $remainder {
            take $remainder if $remainder > 1;
            last;
        }
        # How many times can we divide by this prime?
        while $remainder %% $factor {
            take $factor;
            last if ($remainder div= $factor) === 1;
        }
    }
}

constant @factory = lazy 0..* Z=> flat (0, 0, map { +factors($_) }, 2..*);

sub almost($n) { map *.key, grep *.value == $n, @factory }

put almost($_)[^10] for 1..5;
```



## Phix


```Phix

-- Naieve stuff, mostly, but coded with enthuiasm!
-- Following the idea behind (but not the code from!) the J submission:
--  Generate 10 primes (kept in p10)                            -- (print K=1)
--  Multiply each of them by the first ten primes
--  Sort and find unique values, take the first ten of those    -- (print K=2)
--  Multiply each of them by the first ten primes
--  Sort and find unique values, take the first ten of those    -- (print K=3)
--  ...
-- However I just keep a "top 10", using a bubble insertion, and stop
--  multiplying as soon as everything else for p10[i] will be too big.

-- (as calculated earlier from this routine,
--  or that "return 1" in pi() works just fine.)
--constant f17={2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59}
constant f17={2,3,5,7,11,13,17}

function pi(integer n)
-- approximates the number of primes less than or equal to n
--  if n<=10 then return 4 end if
--  -- best estimate
--  return floor(n/(log(n)-1))
--  if n<=20 then return 1 end if -- (or use a table:)
    if n<17 then
        for i=1 to length(f17) do
            if n<=f17[i] then return i end if
        end for
    end if
--  -- upper bound for n>=17 (Rosser and Schoenfeld 1962):
--  return floor(1.25506*n/log(n))
    -- lower bound for n>=17 (Rosser and Schoenfeld 1962):
    return floor(n/log(n))
end function

function primes(integer n)
-- return the first n prime numbers (tested 0 to 20,000, which took ~86s)
sequence prime
integer count = 0
integer lowN, highN, midN

    -- First, iteratively estimate the sieve size required
    lowN = 2*n
    highN = n*n+1
    while lowN<highN do
        midN = floor((lowN+highN)/2)
        if pi(midN)>n then
            highN = midN
        else
            lowN = midN+1
        end if
    end while
    -- Then apply standard sieve and store primes as we find
    -- them towards the (no longer used) start of the sieve.
    prime = repeat(1,highN)
    for i=2 to highN do
        if prime[i] then
            count += 1
            prime[count] = i
            if count>=n then exit end if
            for k=i+i to highN by i do
                prime[k] = 0
            end for
        end if
    end for
    return prime[1..n]
end function

procedure display(integer k, sequence kprimes)
    printf(1,"%d: ",k)
    for i=1 to length(kprimes) do
        printf(1,"%5d",kprimes[i])
    end for
    puts(1,"\n")
end procedure

function bubble(sequence next, integer v)
-- insert v into next (discarding next[$]), keeping next in ascending order
-- (relies on next[1] /always/ being smaller that anything that we insert.)
    for i=length(next)-1 to 1 by -1 do
        if v>next[i] then
            next[i+1] = v
            exit
        end if
        next[i+1] = next[i]
    end for
    return next
end function

procedure almost_prime()
sequence p10 = primes(10)
sequence apk = p10  -- (almostprime[k])
sequence next = repeat(0,length(p10))
integer high, test
    for k=1 to 5 do
        display(k,apk)
        if k=5 then exit end if
        next = apk
        for i=1 to length(p10) do
--          next[i] = apk[i]*p10[1]
            next[i] = apk[i]*2
        end for
        high = next[$]
        for i=2 to length(p10) do
            for j=1 to length(next) do
                test = apk[j]*p10[i]
                if not find(test,next) then
                    if test>high then exit end if
                    next = bubble(next,test)
                    high = next[$]
                end if
            end for
        end for
        apk = next
    end for
    if getc(0) then end if
end procedure

    almost_prime()


```

{{out}}

```txt

1:     2    3    5    7   11   13   17   19   23   29
2:     4    6    9   10   14   15   21   22   25   26
3:     8   12   18   20   27   28   30   42   44   45
4:    16   24   36   40   54   56   60   81   84   88
5:    32   48   72   80  108  112  120  162  168  176

```

and a translation of the C version, with improved variable names and some extra notes

```Phix


function kprime(integer n, integer k)
--
-- returns true if n has exactly k factors
--
-- p is a "pseudo prime" in that 2,3,4,5,6,7,8,9,10,11 will behave
--  exactly like 2,3,5,7,11, ie the remainder(n,4)=0 (etc) will never
--  succeed because remainder(n,2) would have succeeded twice first.
--  Hence for larger n consider replacing p+=1 with p=next_prime(),
--  then again, on "" this performs an obscene number of divisions..
--
integer p = 2,
        factors = 0

    while factors<k and p*p<=n do
        while remainder(n,p)=0 do
            n = n/p
            factors += 1
        end while
        p += 1
    end while
    factors += (n>1)
    return factors==k
end function

procedure almost_primeC()
integer nextkprime, count

    for k=1 to 5 do
        printf(1,"k = %d: ", k);
        nextkprime = 2
        count = 0
        while count<10 do
            if kprime(nextkprime, k) then
                printf(1," %4d", nextkprime)
                count += 1
            end if
            nextkprime += 1
        end while
        puts(1,"\n")
    end for
    if getc(0) then end if
end procedure

    almost_primeC()

```


{{out}}

```txt

k = 1:     2    3    5    7   11   13   17   19   23   29
k = 2:     4    6    9   10   14   15   21   22   25   26
k = 3:     8   12   18   20   27   28   30   42   44   45
k = 4:    16   24   36   40   54   56   60   81   84   88
k = 5:    32   48   72   80  108  112  120  162  168  176

```



## PicoLisp


```PicoLisp
(de factor (N)
   (make
      (let
         (D 2
            L (1 2 2 . (4 2 4 2 4 6 2 6 .))
            M (sqrt N) )
         (while (>= M D)
            (if (=0 (% N D))
               (setq M
                  (sqrt (setq N (/ N (link D)))) )
               (inc 'D (pop 'L)) ) )
         (link N) ) ) )

(de almost (N)
   (let (X 2  Y 0)
      (make
         (loop
            (when (and (nth (factor X) N) (not (cdr @)))
               (link X)
               (inc 'Y) )
            (T (= 10 Y) 'done)
            (inc 'X) ) ) ) )

(for I 5
   (println I '-> (almost I) ) )

(bye)
```



## Potion


```potion
# Converted from C
kprime = (n, k):
  p = 2, f = 0
  while (f < k && p*p <= n):
    while (0 == n % p):
      n /= p
      f++.
    p++.
  n = if (n > 1): 1.
      else: 0.
  f + n == k.

1 to 5 (k):
  "k = " print, k print, ":" print
  i = 2, c = 0
  while (c < 10):
    if (kprime(i, k)): " " print, i print, c++.
    i++
  .
  "" say.
```


C and Potion take 0.006s, Perl5 0.028s


## Prolog


```prolog
% almostPrime(K, +Take, List) succeeds if List can be unified with the
% first Take K-almost-primes.
% Notice that K need not be specified.
% To avoid having to cache or recompute the first Take primes, we define
% almostPrime/3 in terms of almostPrime/4 as follows:
%
almostPrime(K, Take, List) :-
  % Compute the list of the first Take primes:
  nPrimes(Take, Primes),
  almostPrime(K, Take, Primes, List).

almostPrime(1, Take, Primes, Primes).

almostPrime(K, Take, Primes, List) :-
  generate(2, K),  % generate K >= 2
  K1 is K - 1,
  almostPrime(K1, Take, Primes, L),
  multiplylist( Primes, L, Long),
  sort(Long, Sorted), % uniquifies
  take(Take, Sorted, List).

```
That's it. The rest is machinery. For portability, a compatibility section is included below.

```Prolog
nPrimes( M, Primes) :- nPrimes( [2], M, Primes).

nPrimes( Accumulator, I, Primes) :-
	next_prime(Accumulator, Prime),
	append(Accumulator, [Prime], Next),
	length(Next, N),
	( N = I -> Primes = Next; nPrimes( Next, I, Primes)).

% next_prime(+Primes, NextPrime) succeeds if NextPrime is the next
% prime after a list, Primes, of consecutive primes starting at 2.
next_prime([2], 3).
next_prime([2|Primes], P) :-
	last(Primes, PP),
	P2 is PP + 2,
	generate(P2, N),
	1 is N mod 2,		        % odd
	Max is floor(sqrt(N+1)),	% round-off paranoia
	forall( (member(Prime, [2|Primes]),
		 (Prime =< Max -> true
		 ; (!, fail))), N mod Prime > 0 ),
	!,
        P = N.

% multiply( +A, +List, Answer )
multiply( A, [], [] ).
multiply( A, [X|Xs], [AX|As] ) :-
  AX is A * X,
  multiply(A, Xs, As).

% multiplylist( L1, L2, List ) succeeds if List is the concatenation of X * L2
% for successive elements X of L1.
multiplylist( [], B, [] ).
multiplylist( [A|As], B, List ) :-
   multiply(A, B, L1),
   multiplylist(As, B, L2),
   append(L1, L2, List).

take(N, List, Head) :-
  length(Head, N),
  append(Head,X,List).

```


```Prolog
%%%%% compatibility section %%%%%

:- if(current_prolog_flag(dialect, yap)).
generate(Min, I) :- between(Min, inf, I).

append([],L,L).
append([X|Xs], L, [X|Ls]) :- append(Xs,L,Ls).

:- endif.

:- if(current_prolog_flag(dialect, swi)).
generate(Min, I) :- between(Min, inf, I).
:- endif.

:- if(current_prolog_flag(dialect, yap)).
append([],L,L).
append([X|Xs], L, [X|Ls]) :- append(Xs,L,Ls).

last([X], X).
last([_|Xs],X) :- last(Xs,X).

:- endif.

:- if(current_prolog_flag(dialect, gprolog)).
generate(Min, I) :-
  current_prolog_flag(max_integer, Max),
  between(Min, Max, I).
:- endif.

```

Example using SWI-Prolog:
```txt

?- between(1,5,I),
   (almostPrime(I, 10, L) -> writeln(L)), fail.

[2,3,5,7,11,13,17,19,23,29]
[4,6,9,10,14,15,21,22,25,26]
[8,12,18,20,27,28,30,42,44,45]
[16,24,36,40,54,56,60,81,84,88]
[32,48,72,80,108,112,120,162,168,176]

?- time( (almostPrime(5, 10, L), writeln(L))).
[32,48,72,80,108,112,120,162,168,176]
% 1,906 inferences, 0.001 CPU in 0.001 seconds (84% CPU, 2388471 Lips)

```



## PureBasic

{{trans|C}}

```PureBasic
EnableExplicit

Procedure.b kprime(n.i, k.i)
  Define p.i = 2,
         f.i = 0

  While f < k And p*p <= n
    While n % p = 0
      n / p
      f + 1
    Wend
    p + 1
  Wend

  ProcedureReturn Bool(f + Bool(n > 1) = k)

EndProcedure

;___main____
If Not OpenConsole("Almost prime")
  End -1
EndIf

Define i.i,
       c.i,
       k.i

For k = 1 To 5
  Print("k = " + Str(k) + ":")

  i = 2
  c = 0
  While c < 10
    If kprime(i, k)
      Print(RSet(Str(i),4))
      c + 1
    EndIf
    i + 1
  Wend
  PrintN("")
Next

Input()
```

{{out}}

```txt
k = 1:   2   3   5   7  11  13  17  19  23  29
k = 2:   4   6   9  10  14  15  21  22  25  26
k = 3:   8  12  18  20  27  28  30  42  44  45
k = 4:  16  24  36  40  54  56  60  81  84  88
k = 5:  32  48  72  80 108 112 120 162 168 176
```



## Python

This imports [[Prime decomposition#Python]]

```python
from prime_decomposition import decompose
from itertools import islice, count
try:
    from functools import reduce
except:
    pass


def almostprime(n, k=2):
    d = decompose(n)
    try:
        terms = [next(d) for i in range(k)]
        return reduce(int.__mul__, terms, 1) == n
    except:
        return False

if __name__ == '__main__':
    for k in range(1,6):
        print('%i: %r' % (k, list(islice((n for n in count() if almostprime(n, k)), 10))))
```

{{out}}

```txt
1: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
2: [4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
3: [8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
4: [16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
5: [32, 48, 72, 80, 108, 112, 120, 162, 168, 176]
```


----


### An updated version with no import dependencies.




```python

# k-Almost-primes
# Python 3.6.3
# no imports

def prime_factors(m=2):

    for i in range(2, m):
        r, q = divmod(m, i)
        if not q:
            return [i] + prime_factors(r)
    return [m]

def k_almost_primes(n, k=2):
    multiples = set()
    lists = list()
    for x in range(k+1):
        lists.append([])

    for i in range(2, n+1):
        if i not in multiples:
            if len(lists[1]) < 10:
                lists[1].append(i)
            multiples.update(range(i*i, n+1, i))
    print("k=1: {}".format(lists[1]))

    for j in range(2, k+1):
        for m in multiples:
            l = prime_factors(m)
            ll = len(l)
            if ll == j and len(lists[j]) < 10:
                lists[j].append(m)

        print("k={}: {}".format(j, lists[j]))

k_almost_primes(200, 5)
# try:
#k_almost_primes(6000, 10)

```

{{out}}

```txt

>>> %Run k_almost_primes.py
k=1: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
k=2: [4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
k=3: [8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
k=4: [16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
k=5: [32, 48, 72, 80, 108, 112, 120, 162, 168, 176]

```



## R

This uses the function from [[Prime decomposition#R]]

```rsplus
#
### =========================================================

# Find k-Almost-primes
# R implementation
#
### =========================================================

#---------------------------------------------------------------
# Function for prime factorization from Rosetta Code
#---------------------------------------------------------------

findfactors <- function(n) {
  d <- c()
  div <- 2; nxt <- 3; rest <- n
  while( rest != 1 ) {
    while( rest%%div == 0 ) {
      d <- c(d, div)
      rest <- floor(rest / div)
    }
    div <- nxt
    nxt <- nxt + 2
  }
  d
}

#---------------------------------------------------------------
# Find k-Almost-primes
#---------------------------------------------------------------

almost_primes <- function(n = 10, k = 5) {

  # Set up matrix for storing of the results

  res <- matrix(NA, nrow = k, ncol = n)
  rownames(res) <- paste("k = ", 1:k, sep = "")
  colnames(res) <- rep("", n)

  # Loop over k

  for (i in 1:k) {

    tmp <- 1

    while (any(is.na(res[i, ]))) { # Keep looping if there are still missing entries in the result-matrix
      if (length(findfactors(tmp)) == i) { # Check number of factors
        res[i, which.max(is.na(res[i, ]))] <- tmp
      }
      tmp <- tmp + 1
    }
  }
  print(res)
}
```


{{out}}

```txt

k = 1  2  3  5  7  11  13  17  19  23  29
k = 2  4  6  9 10  14  15  21  22  25  26
k = 3  8 12 18 20  27  28  30  42  44  45
k = 4 16 24 36 40  54  56  60  81  84  88
k = 5 32 48 72 80 108 112 120 162 168 176

```



## Racket



```racket
#lang racket
(require (only-in math/number-theory factorize))

(define ((k-almost-prime? k) n)
  (= k (for/sum ((f (factorize n))) (cadr f))))

(define KAP-table-values
  (for/list ((k (in-range 1 (add1 5))))
    (define kap? (k-almost-prime? k))
    (for/list ((j (in-range 10)) (i (sequence-filter kap? (in-naturals 1))))
      i)))

(define (format-table t)
  (define longest-number-length
    (add1 (order-of-magnitude (argmax order-of-magnitude (cons (length t) (apply append t))))))
  (define (fmt-val v) (~a v #:width longest-number-length #:align 'right))
  (string-join
   (for/list ((r t) (k (in-naturals 1)))
     (string-append
      (format "║ k = ~a║ " (fmt-val k))
      (string-join (for/list ((c r)) (fmt-val c)) "| ")
      "║"))
   "\n"))

(displayln (format-table KAP-table-values))
```


{{out}}

```txt
║ k =   1║   2|   3|   5|   7|  11|  13|  17|  19|  23|  29║
║ k =   2║   4|   6|   9|  10|  14|  15|  21|  22|  25|  26║
║ k =   3║   8|  12|  18|  20|  27|  28|  30|  42|  44|  45║
║ k =   4║  16|  24|  36|  40|  54|  56|  60|  81|  84|  88║
║ k =   5║  32|  48|  72|  80| 108| 112| 120| 162| 168| 176║
```



## REXX


### naive version

The method used is to count the number of factors in the number to determine the K-primality.

The first three   '''k-almost'''   primes for each   '''K'''   group are computed directly   (rather than found).

```rexx
/*REXX program  computes and displays  the  first  N  K─almost  primes  from   1 ──► K. */
parse arg N K .                                  /*get optional arguments from the C.L. */
if N=='' | N==","  then N=10                     /*N  not specified?   Then use default.*/
if K=='' | K==","  then K= 5                     /*K   "      "          "   "     "    */
                                                 /*W: is the width of K, used for output*/
    do m=1  for  K;     $=2**m;  fir=$           /*generate & assign 1st K─almost prime.*/
    #=1;                if #==N  then leave      /*#: K─almost primes; Enough are found?*/
    #=2;                $=$  3*(2**(m-1))        /*generate & append 2nd K─almost prime.*/
    if #==N  then leave                          /*#: K─almost primes; Enough are found?*/
    if m==1  then _=fir + fir                    /* [↓]  gen & append 3rd K─almost prime*/
             else do;  _=9 * (2**(m-2));    #=3;    $=$  _;    end
        do j=_ + m - 1   until #==N              /*process an  K─almost prime  N  times.*/
        if factr()\==m  then iterate             /*not the correct  K─almost  prime?    */
        #=# + 1;         $=$ j                   /*bump K─almost counter; append it to $*/
        end   /*j*/                              /* [↑]   generate  N  K─almost  primes.*/
    say right(m, length(K))"─almost ("N') primes:'     $
    end       /*m*/                              /* [↑]  display a line for each K─prime*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
factr: z=j;                    do f=0  while z// 2==0;  z=z% 2;  end  /*divisible by  2.*/
                               do f=f  while z// 3==0;  z=z% 3;  end  /*divisible  "  3.*/
                               do f=f  while z// 5==0;  z=z% 5;  end  /*divisible  "  5.*/
                               do f=f  while z// 7==0;  z=z% 7;  end  /*divisible  "  7.*/
                               do f=f  while z//11==0;  z=z%11;  end  /*divisible  " 11.*/
                               do f=f  while z//13==0;  z=z%13;  end  /*divisible  " 13.*/
         do p=17  by 6  while  p<=z              /*insure  P  isn't divisible by three. */
         parse var  p   ''  -1  _                /*obtain the right─most decimal digit. */
                                                 /* [↓]  fast check for divisible by 5. */
         if _\==5  then do; do f=f+1  while z//p==0; z=z%p; end;  f=f-1; end  /*÷ by P? */
         if _ ==3  then iterate                  /*fast check for  X  divisible by five.*/
         x=p+2;             do f=f+1  while z//x==0; z=z%x; end;  f=f-1       /*÷ by X? */
         end   /*i*/                             /* [↑]  find all the factors in  Z.    */

       if f==0  then return 1                    /*if  prime (f==0),  then return unity.*/
                     return f                    /*return to invoker the number of divs.*/
```

{{out|output|text=  when using the default input:}}

```txt

1─almost (10) primes: 2 3 5 7 11 13 17 19 23 29
2─almost (10) primes: 4 6 9 10 14 15 21 22 25 26
3─almost (10) primes: 8 12 18 20 27 28 30 42 44 45
4─almost (10) primes: 16 24 36 40 54 56 60 81 84 88
5─almost (10) primes: 32 48 72 80 108 112 120 162 168 176

```

{{out|output|text=  when using the input of:   <tt>   20   12 </tt>}}

```txt

 1─almost (20) primes: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
 2─almost (20) primes: 4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57
 3─almost (20) primes: 8 12 18 20 27 28 30 42 44 45 50 52 63 66 68 70 75 76 78 92
 4─almost (20) primes: 16 24 36 40 54 56 60 81 84 88 90 100 104 126 132 135 136 140 150 152
 5─almost (20) primes: 32 48 72 80 108 112 120 162 168 176 180 200 208 243 252 264 270 272 280 300
 6─almost (20) primes: 64 96 144 160 216 224 240 324 336 352 360 400 416 486 504 528 540 544 560 600
 7─almost (20) primes: 128 192 288 320 432 448 480 648 672 704 720 800 832 972 1008 1056 1080 1088 1120 1200
 8─almost (20) primes: 256 384 576 640 864 896 960 1296 1344 1408 1440 1600 1664 1944 2016 2112 2160 2176 2240 2400
 9─almost (20) primes: 512 768 1152 1280 1728 1792 1920 2592 2688 2816 2880 3200 3328 3888 4032 4224 4320 4352 4480 4800
10─almost (20) primes: 1024 1536 2304 2560 3456 3584 3840 5184 5376 5632 5760 6400 6656 7776 8064 8448 8640 8704 8960 9600
11─almost (20) primes: 2048 3072 4608 5120 6912 7168 7680 10368 10752 11264 11520 12800 13312 15552 16128 16896 17280 17408 17920 19200
12─almost (20) primes: 4096 6144 9216 10240 13824 14336 15360 20736 21504 22528 23040 25600 26624 31104 32256 33792 34560 34816 35840 38400

```



### optimized version

This optimized REXX version can be   ''over a hundred times''   faster than the naive version.

Some of the optimizations are:
:::*   calculating the first   <big> 2<sup>(K-1)</sup> </big>   K─almost primes for each   '''K'''   group
:::*   generating the primes (up to the limit) instead of dividing by (most) divisors.
:::*   extending the   ''up-front''   prime divisors in the '''factr''' function.


The 1<sup>st</sup> optimization (bullet) allows the direct computation   (instead of searching)   of all K─almost primes up to the first   ''odd''   prime in the list.

Once the required primes are generated, the finding of the K─almost primes is almost instantaneous.

```rexx
/*REXX program  computes and displays  the first    N    K─almost primes from  1 ──► K. */
parse arg N K .                                  /*obtain optional arguments from the CL*/
if N=='' | N==','  then N=10                     /*N  not specified?   Then use default.*/
if K=='' | K==','  then K= 5                     /*K   "      "          "   "     "    */
nn=N;  N=abs(N);   w=length(K)                   /*N positive? Then show K─almost primes*/
limit= (2**K) * N / 2                            /*this is the limit for most K-primes. */
if N==1  then limit=limit * 2                    /*  "   "  "    "    "  a    N    of 1.*/
if K==1  then limit=limit * 4                    /*  "   "  "    "    "  a K─prime  " 2.*/
if K==2  then limit=limit * 2                    /*  "   "  "    "    "  "    "     " 4.*/
if K==3  then limit=limit * 3 % 2                /*  "   "  "    "    "  "    "     " 8.*/
call genPrimes  limit + 1                        /*generate primes up to the  LIMIT + 1.*/
say 'The highest prime computed: '        @.#        " (under the limit of " limit').'
say                                              /* [↓]  define where 1st K─prime is odd*/
d.=0;  d.2=  2;  d.3 =  4;  d.4 =  7;  d.5 = 13;  d.6 = 22;  d.7 =  38;   d.8=63
       d.9=102;  d.10=168;  d.11=268;  d.12=426;  d.13=673;  d.14=1064
d!=0
    do m=1  for  K;    d!=max(d!,d.m)            /*generate & assign 1st K─almost prime.*/
    mr=right(m,w);     mm=m-1

    $=;           do #=1  to min(N, d!)          /*assign some doubled K─almost primes. */
                  $=$  d.mm.# * 2
                  end   /*#*/
    #=#-1
    if m==1  then from=2
             else from=1 + word($, words($) )

        do j=from   until  #==N                  /*process an  K─almost prime  N  times.*/
        if factr()\==m  then iterate             /*not the correct  K─almost  prime?    */
        #=#+1;   $=$ j                           /*bump K─almost counter; append it to $*/
        end   /*j*/                              /* [↑]   generate  N  K─almost  primes.*/

    if nn>0  then say mr"─almost ("N') primes:'     $
             else say '    the last'  mr  "K─almost prime: "   word($, words($))
                                               /* [↓]  assign K─almost primes.*/
          do q=1  for #;     d.m.q=word($,q)             ;   end  /*q*/
          do q=1  for #;  if d.m.q\==d.mm.q*2  then leave;   end  /*q*/
                                               /* [↑]  count doubly-duplicates*/
/*──── say copies('─',40)  'for '   m", "   q-1   'numbers were doubly─duplicated.' ────*/
/*──── say                                                                          ────*/
    end       /*m*/                              /* [↑]  display a line for each K─prime*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
factr: if #.j\==.  then return #.j
       z=j;                                do f=0 while z// 2==0; z=z% 2; end   /*÷ by 2*/
                                           do f=f while z// 3==0; z=z% 3; end   /*÷ "  3*/
                                           do f=f while z// 5==0; z=z% 5; end   /*÷ "  5*/
                                           do f=f while z// 7==0; z=z% 7; end   /*÷ "  7*/
                                           do f=f while z//11==0; z=z%11; end   /*÷ " 11*/
                                           do f=f while z//13==0; z=z%13; end   /*÷ " 13*/
                                           do f=f while z//17==0; z=z%17; end   /*÷ " 17*/
                                           do f=f while z//19==0; z=z%19; end   /*÷ " 19*/

         do i=9    while  @.i<=z;       d=@.i    /*divide by some higher primes.        */
           do f=f  while z//d==0;   z=z%d;  end  /*is  Z  divisible by the  prime  D ?  */
         end   /*i*/                             /* [↑]  find all factors in  Z.        */

       if f==0  then f=1;   #.j=f;   return f    /*Is prime (f≡0)?   Then return unity. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
genPrimes: arg x;             @.=;      @.1=2;     @.2=3;    #.=.;     #=2;     s.#=@.#**2
             do j=@.# +2  by 2  to x             /*only find odd primes from here on.   */
                do p=2  while s.p<=j             /*divide by some known low odd primes. */
                if j//@.p==0  then iterate j     /*Is  J  divisible by X?  Then ¬ prime.*/
                end   /*p*/                      /* [↓]  a prime  (J)  has been found.  */
             #=#+1;    @.#=j;   #.j=1;   s.#=j*j /*bump prime count, and also assign ···*/
             end      /*j*/                      /* ··· the # of factors, prime, prime².*/
           return                                /* [↑]  not an optimal prime generator.*/
```

{{out|output|text=  when using the input of:   <tt>   20   16 </tt>}}

```txt

The highest prime computed:  655357  (under the limit of  655360).

 1─almost (20) primes:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
 2─almost (20) primes:  4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57
 3─almost (20) primes:  8 12 18 20 27 28 30 42 44 45 50 52 63 66 68 70 75 76 78 92
 4─almost (20) primes:  16 24 36 40 54 56 60 81 84 88 90 100 104 126 132 135 136 140 150 152
 5─almost (20) primes:  32 48 72 80 108 112 120 162 168 176 180 200 208 243 252 264 270 272 280 300
 6─almost (20) primes:  64 96 144 160 216 224 240 324 336 352 360 400 416 486 504 528 540 544 560 600
 7─almost (20) primes:  128 192 288 320 432 448 480 648 672 704 720 800 832 972 1008 1056 1080 1088 1120 1200
 8─almost (20) primes:  256 384 576 640 864 896 960 1296 1344 1408 1440 1600 1664 1944 2016 2112 2160 2176 2240 2400
 9─almost (20) primes:  512 768 1152 1280 1728 1792 1920 2592 2688 2816 2880 3200 3328 3888 4032 4224 4320 4352 4480 4800
10─almost (20) primes:  1024 1536 2304 2560 3456 3584 3840 5184 5376 5632 5760 6400 6656 7776 8064 8448 8640 8704 8960 9600
11─almost (20) primes:  2048 3072 4608 5120 6912 7168 7680 10368 10752 11264 11520 12800 13312 15552 16128 16896 17280 17408 17920 19200
12─almost (20) primes:  4096 6144 9216 10240 13824 14336 15360 20736 21504 22528 23040 25600 26624 31104 32256 33792 34560 34816 35840 38400
13─almost (20) primes:  8192 12288 18432 20480 27648 28672 30720 41472 43008 45056 46080 51200 53248 62208 64512 67584 69120 69632 71680 76800
14─almost (20) primes:  16384 24576 36864 40960 55296 57344 61440 82944 86016 90112 92160 102400 106496 124416 129024 135168 138240 139264 143360 153600
15─almost (20) primes:  32768 49152 73728 81920 110592 114688 122880 165888 172032 180224 184320 204800 212992 248832 258048 270336 276480 278528 286720 307200
16─almost (20) primes:  65536 98304 147456 163840 221184 229376 245760 331776 344064 360448 368640 409600 425984 497664 516096 540672 552960 557056 573440 614400

```



## Ring


```ring

for ap = 1 to 5
    see "k = " + ap + ":"
    aList = []
    for n = 1 to 200
        num = 0
        for nr = 1 to n
            if n%nr=0 and isPrime(nr)=1
               num = num + 1
               pr = nr
               while true
                     pr = pr * nr
                     if n%pr = 0
                        num = num + 1
                     else exit ok
               end ok
        next
        if (ap = 1 and isPrime(n) = 1) or (ap > 1 and num = ap)
           add(aList, n)
           if len(aList)=10 exit ok ok
     next
     for m = 1 to len(aList)
           see " " + aList[m]
     next
     see nl
next

func isPrime num
     if (num <= 1) return 0 ok
     if (num % 2 = 0 and num != 2) return 0 ok
     for i = 3 to floor(num / 2) -1 step 2
         if (num % i = 0) return 0 ok
     next
     return 1

```

Output:

```txt

k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176

```



## Ruby


```ruby
require 'prime'

def almost_primes(k=2)
  return to_enum(:almost_primes, k) unless block_given?
  1.step {|n| yield n if n.prime_division.sum( &:last ) == k }
end

(1..5).each{|k| puts almost_primes(k).take(10).join(", ")}
```

{{out}}

```txt

2, 3, 5, 7, 11, 13, 17, 19, 23, 29
4, 6, 9, 10, 14, 15, 21, 22, 25, 26
8, 12, 18, 20, 27, 28, 30, 42, 44, 45
16, 24, 36, 40, 54, 56, 60, 81, 84, 88
32, 48, 72, 80, 108, 112, 120, 162, 168, 176

```


{{trans|J}}

```ruby
require 'prime'

p ar = pr = Prime.take(10)
4.times{p ar = ar.product(pr).map{|(a,b)| a*b}.uniq.sort.take(10)}
```

{{out}}

```txt

[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
[4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
[8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
[16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
[32, 48, 72, 80, 108, 112, 120, 162, 168, 176]

```



## Rust


```rust
fn is_kprime(n: u32, k: u32) -> bool {
    let mut primes = 0;
    let mut f = 2;
    let mut rem = n;
    while primes < k && rem > 1{
        while (rem % f) == 0 && rem > 1{
            rem /= f;
            primes += 1;
        }
        f += 1;
    }
    rem == 1 && primes == k
}

struct KPrimeGen {
    k: u32,
    n: u32,
}

impl Iterator for KPrimeGen {
    type Item = u32;
    fn next(&mut self) -> Option<u32> {
        self.n += 1;
        while !is_kprime(self.n, self.k) {
            self.n += 1;
        }
        Some(self.n)
    }
}

fn kprime_generator(k: u32) -> KPrimeGen {
    KPrimeGen {k: k, n: 1}
}

fn main() {
    for k in 1..6 {
        println!("{}: {:?}", k, kprime_generator(k).take(10).collect::<Vec<_>>());
    }
}
```

{{out}}

```txt

1: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
2: [4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
3: [8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
4: [16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
5: [32, 48, 72, 80, 108, 112, 120, 162, 168, 176]

```



## Scala


```Scala
def isKPrime(n: Int, k: Int, d: Int = 2): Boolean = (n, k, d) match {
    case (n, k, _) if n == 1 => k == 0
    case (n, _, d) if n % d == 0 => isKPrime(n / d, k - 1, d)
    case (_, _, _) => isKPrime(n, k, d + 1)
}

def kPrimeStream(k: Int): Stream[Int] = {
    def loop(n: Int): Stream[Int] =
        if (isKPrime(n, k)) n #:: loop(n+ 1)
        else loop(n + 1)
    loop(2)
}

for (k <- 1 to 5) {
    println( s"$k: [${ kPrimeStream(k).take(10) mkString " " }]" )
}
```


{{out}}

```txt

1: [2 3 5 7 11 13 17 19 23 29]
2: [4 6 9 10 14 15 21 22 25 26]
3: [8 12 18 20 27 28 30 42 44 45]
4: [16 24 36 40 54 56 60 81 84 88]
5: [32 48 72 80 108 112 120 162 168 176]

```



## SequenceL


```sequencel>import <Utilities/Conversion.sl
;
import <Utilities/Sequence.sl>;

main(args(2)) :=
	let
		result := firstNKPrimes(1 ... 5, 10);

		output[i] := "k = " ++ intToString(i) ++ ": " ++ delimit(intToString(result[i]), ' ');
	in
		delimit(output, '\n');

firstNKPrimes(k, N) := firstNKPrimesHelper(k, N, 2, []);

firstNKPrimesHelper(k, N, current, result(1)) :=
	let
		newResult := result when not isKPrime(k, current) else result ++ [current];
	in
		result when size(result) = N
	else
		firstNKPrimesHelper(k, N, current + 1, newResult);

isKPrime(k, n) := size(primeFactorization(n)) = k;
```


Using Prime Decomposition Solution [http://rosettacode.org/wiki/Prime_decomposition#SequenceL]

{{out}}

```txt

main.exe
"k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176"

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: kprime (in var integer: number, in integer: k) is func
  result
    var boolean: kprime is FALSE;
  local
    var integer: p is 2;
    var integer: f is 0;
  begin
    while f < k and p * p <= number do
      while number rem p = 0 do
        number := number div p;
        incr(f);
      end while;
      incr(p);
    end while;
    kprime := f + ord(number > 1) = k;
  end func;

const proc: main is func
  local
    var integer: k is 0;
    var integer: number is 0;
    var integer: count is 0;
  begin
    for k range 1 to 5 do
      write("k = " <& k <& ":");
      count := 0;
      for number range 2 to integer.last until count >= 10 do
        if kprime(number, k) then
          write(" " <& number);
          incr(count);
        end if;
      end for;
      writeln;
    end for;
  end func;
```


{{out}}

```txt

k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176

```



## Sidef

{{trans|Perl 6}}

```ruby
func is_k_almost_prime(n, k) {
    for (var (p, f) = (2, 0); (f < k) && (p*p <= n); ++p) {
        (n /= p; ++f) while (p `divides` n)
    }
    n > 1 ? (f.inc == k) : (f == k)
}

{ |k|
    var x = 10
    say gather {
        { |i|
            if (is_k_almost_prime(i, k)) {
                take(i)
                --x == 0 && break
            }
        } << 1..Inf
    }
} << 1..5
```

{{out}}

```txt

[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
[4, 6, 9, 10, 14, 15, 21, 22, 25, 26]
[8, 12, 18, 20, 27, 28, 30, 42, 44, 45]
[16, 24, 36, 40, 54, 56, 60, 81, 84, 88]
[32, 48, 72, 80, 108, 112, 120, 162, 168, 176]

```



## Tcl

{{works with|Tcl|8.6}}
{{tcllib|math::numtheory}}

```tcl
package require Tcl 8.6
package require math::numtheory

proc firstNprimes n {
    for {set result {};set i 2} {[llength $result] < $n} {incr i} {
	if {[::math::numtheory::isprime $i]} {
	    lappend result $i
	}
    }
    return $result
}

proc firstN_KalmostPrimes {n k} {
    set p [firstNprimes $n]
    set i [lrepeat $k 0]
    set c {}

    while true {
	dict set c [::tcl::mathop::* {*}[lmap j $i {lindex $p $j}]] ""
	for {set x 0} {$x < $k} {incr x} {
	    lset i $x [set xx [expr {([lindex $i $x] + 1) % $n}]]
	    if {$xx} break
	}
	if {$x == $k} break
    }
    return [lrange [lsort -integer [dict keys $c]] 0 [expr {$n - 1}]]
}

for {set K 1} {$K <= 5} {incr K} {
    puts "$K => [firstN_KalmostPrimes 10 $K]"
}
```

{{out}}

```txt

1 => 2 3 5 7 11 13 17 19 23 29
2 => 4 6 9 10 14 15 21 22 25 26
3 => 8 12 18 20 27 28 30 42 44 45
4 => 16 24 36 40 54 56 60 81 84 88
5 => 32 48 72 80 108 112 120 162 168 176

```



## uBasic/4tH

{{trans|C}}
<lang>Local(3)

For c@ = 1 To 5
  Print "k = ";c@;": ";

  b@=0

  For a@ = 2 Step 1 While b@ < 10
    If FUNC(_kprime (a@,c@)) Then
       b@ = b@ + 1
       Print " ";a@;
    EndIf
  Next

  Print
Next

End

_kprime Param(2)
  Local(2)

  d@ = 0
  For c@ = 2 Step 1 While (d@ < b@) * ((c@ * c@) < (a@ + 1))
    Do While (a@ % c@) = 0
      a@ = a@ / c@
      d@ = d@ + 1
    Loop
  Next
Return (b@ = (d@ + (a@ > 1)))
```

{{out}}

```txt
k = 1:  2 3 5 7 11 13 17 19 23 29
k = 2:  4 6 9 10 14 15 21 22 25 26
k = 3:  8 12 18 20 27 28 30 42 44 45
k = 4:  16 24 36 40 54 56 60 81 84 88
k = 5:  32 48 72 80 108 112 120 162 168 176

0 OK, 0:200
```


## VBA

{{trans|Phix}}
```vb
Private Function kprime(ByVal n As Integer, k As Integer) As Boolean
    Dim p As Integer, factors As Integer
    p = 2
    factors = 0
    Do While factors < k And p * p <= n
        Do While n Mod p = 0
            n = n / p
            factors = factors + 1
        Loop
        p = p + 1
    Loop
    factors = factors - (n > 1) 'true=-1
    kprime = factors = k
End Function

Private Sub almost_primeC()
    Dim nextkprime As Integer, count As Integer
    Dim k As Integer
    For k = 1 To 5
        Debug.Print "k ="; k; ":";
        nextkprime = 2
        count = 0
        Do While count < 10
            If kprime(nextkprime, k) Then
                Debug.Print " "; Format(CStr(nextkprime), "@@@@@");
                count = count + 1
            End If
            nextkprime = nextkprime + 1
        Loop
        Debug.Print
    Next k
End Sub
```
{{out}}

```txt
k = 1 :     2     3     5     7    11    13    17    19    23    29
k = 2 :     4     6     9    10    14    15    21    22    25    26
k = 3 :     8    12    18    20    27    28    30    42    44    45
k = 4 :    16    24    36    40    54    56    60    81    84    88
k = 5 :    32    48    72    80   108   112   120   162   168   176
```


## VBScript

Repurposed the VBScript code for the Prime Decomposition task.

```vb

For k = 1 To 5
	count = 0
	increment = 1
	WScript.StdOut.Write "K" & k & ": "
	Do Until count = 10
		If PrimeFactors(increment) = k Then
			WScript.StdOut.Write increment & " "
			count = count + 1
		End If
		increment = increment + 1
	Loop
	WScript.StdOut.WriteLine
Next

Function PrimeFactors(n)
	PrimeFactors = 0
	arrP = Split(ListPrimes(n)," ")
	divnum = n
	Do Until divnum = 1
		For i = 0 To UBound(arrP)-1
			If divnum = 1 Then
				Exit For
			ElseIf divnum Mod arrP(i) = 0 Then
				divnum = divnum/arrP(i)
				PrimeFactors = PrimeFactors + 1
			End If
		Next
	Loop
End Function

Function IsPrime(n)
	If n = 2 Then
		IsPrime = True
	ElseIf n <= 1 Or n Mod 2 = 0 Then
		IsPrime = False
	Else
		IsPrime = True
		For i = 3 To Int(Sqr(n)) Step 2
			If n Mod i = 0 Then
				IsPrime = False
				Exit For
			End If
		Next
	End If
End Function

Function ListPrimes(n)
	ListPrimes = ""
	For i = 1 To n
		If IsPrime(i) Then
			ListPrimes = ListPrimes & i & " "
		End If
	Next
End Function

```


{{Out}}

```txt

K1: 2 3 5 7 11 13 17 19 23 29
K2: 4 6 9 10 14 15 21 22 25 26
K3: 8 12 18 20 27 28 30 42 44 45
K4: 16 24 36 40 54 56 60 81 84 88
K5: 32 48 72 80 108 112 120 162 168 176

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Class KPrime
        Public K As Integer

        Public Function IsKPrime(number As Integer) As Boolean
            Dim primes = 0
            Dim p = 2
            While p * p <= number AndAlso primes < K
                While number Mod p = 0 AndAlso primes < K
                    number = number / p
                    primes = primes + 1
                End While
                p = p + 1
            End While
            If number > 1 Then
                primes = primes + 1
            End If
            Return primes = K
        End Function

        Public Function GetFirstN(n As Integer) As List(Of Integer)
            Dim result As New List(Of Integer)
            Dim number = 2
            While result.Count < n
                If IsKPrime(number) Then
                    result.Add(number)
                End If
                number = number + 1
            End While
            Return result
        End Function
    End Class

    Sub Main()
        For Each k In Enumerable.Range(1, 5)
            Dim kprime = New KPrime With {
                .K = k
            }
            Console.WriteLine("k = {0}: {1}", k, String.Join(" ", kprime.GetFirstN(10)))
        Next
    End Sub

End Module
```

{{out}}

```txt
k = 1: 2 3 5 7 11 13 17 19 23 29
k = 2: 4 6 9 10 14 15 21 22 25 26
k = 3: 8 12 18 20 27 28 30 42 44 45
k = 4: 16 24 36 40 54 56 60 81 84 88
k = 5: 32 48 72 80 108 112 120 162 168 176
```



## XBasic

{{trans|FreeBASIC}}
{{works with|Windows XBasic}}

```xbasic

PROGRAM "almostprime"
VERSION "0.0001"

DECLARE FUNCTION Entry()
INTERNAL FUNCTION KPrime(n%%, k%%)

FUNCTION Entry()
  FOR k@@ = 1 TO 5
    PRINT "k ="; k@@; ": ";
    i%% = 2
    c%% = 0
    DO WHILE c%% < 10
      IFT KPrime(i%%, k@@) THEN
        PRINT FORMAT$("### ", i%%);
        INC c%%
      END IF
      INC i%%
    LOOP
    PRINT
  NEXT k@@
END FUNCTION

FUNCTION KPrime(n%%, k%%)
  f%% = 0
  FOR i%% = 2 TO n%%
    DO WHILE n%% MOD i%% = 0
      IF f%% = k%% THEN RETURN $$FALSE
      INC f%%
      n%% = n%% \ i%%
    LOOP
  NEXT i%%
  RETURN f%% = k%%
END FUNCTION

END PROGRAM

```

{{out}}

```txt

k = 1:   2   3   5   7  11  13  17  19  23  29
k = 2:   4   6   9  10  14  15  21  22  25  26
k = 3:   8  12  18  20  27  28  30  42  44  45
k = 4:  16  24  36  40  54  56  60  81  84  88
k = 5:  32  48  72  80 108 112 120 162 168 176

```


## Yabasic

{{trans|Lua}}

```Yabasic
// Returns boolean indicating whether n is k-almost prime
sub almostPrime(n, k)
    local divisor, count

    divisor = 2

    while(count < (k + 1) and n <> 1)
        if not mod(n, divisor) then
            n = n / divisor
            count = count + 1
        else
            divisor = divisor + 1
        end if
    wend
    return count = k
end sub

// Generates table containing first ten k-almost primes for given k
sub kList(k, kTab())
    local n, i

    n = 2^k : i = 1
    while(i < 11)
        if almostPrime(n, k) then
            kTab(i) = n
            i = i + 1
        end if
        n = n + 1
    wend
end sub

// Main procedure, displays results from five calls to kList()
dim kTab(10)
for k = 1 to 5
    print "k = ", k, " : ";
    kList(k, kTab())
    for n = 1 to 10
        print kTab(n), ", ";
    next
    print "..."
next
```



## zkl

{{trans|Ruby}}{{trans|J}}
Using the prime generator from task [[Extensible prime generator#zkl]].

Can't say I entirely understand this algorithm. Uses list comprehension to calculate the outer/tensor product (p10 ⊗ ar).

```zkl
primes:=Utils.Generator(Import("sieve").postponed_sieve);
(p10:=ar:=primes.walk(10)).println();
do(4){
   (ar=([[(x,y);ar;p10;'*]] : Utils.Helpers.listUnique(_).sort()[0,10])).println();
}
```

{{out}}

```txt

L(2,3,5,7,11,13,17,19,23,29)
L(4,6,9,10,14,15,21,22,25,26)
L(8,12,18,20,27,28,30,42,44,45)
L(16,24,36,40,54,56,60,81,84,88)
L(32,48,72,80,108,112,120,162,168,176)

```



## ZX Spectrum Basic

{{trans|AWK}}

```zxbasic
10 FOR k=1 TO 5
20 PRINT k;":";
30 LET c=0: LET i=1
40 IF c=10 THEN GO TO 100
50 LET i=i+1
60 GO SUB 1000
70 IF r THEN PRINT " ";i;: LET c=c+1
90 GO TO 40
100 PRINT
110 NEXT k
120 STOP
1000 REM kprime
1010 LET p=2: LET n=i: LET f=0
1020 IF f=k OR (p*p)>n THEN GO TO 1100
1030 IF n/p=INT (n/p) THEN LET n=n/p: LET f=f+1: GO TO 1030
1040 LET p=p+1: GO TO 1020
1100 LET r=(f+(n>1)=k)
1110 RETURN
```


{{out}}

```txt
1: 2 3 5 7 11 13 17 19 23 29
2: 4 6 9 10 14 15 21 22 25 26
3: 8 12 18 20 27 28 30 42 44 45
4: 16 24 36 40 54 56 60 81 84 88
5: 32 48 72 80 108 112 120 162 168 176
```


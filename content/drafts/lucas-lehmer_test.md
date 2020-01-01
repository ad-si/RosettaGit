+++
title = "Lucas-Lehmer test"
description = ""
date = 2019-10-20T11:47:38Z
aliases = []
[extra]
id = 2593
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}
[[Category:Arbitrary precision]]
[[Category:Arithmetic operations]]

Lucas-Lehmer Test: for <math>p</math> an odd prime, the Mersenne number <math>2^p-1</math> is prime if and only if <math>2^p-1</math> divides <math>S(p-1)</math> where <math>S(n+1)=(S(n))^2-2</math>, and <math>S(1)=4</math>.


;Task:
Calculate all Mersenne primes up to the implementation's
maximum precision, or the 47<sup>th</sup> Mersenne prime   (whichever comes first).





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.

```360asm
*        Lucas-Lehmer test
LUCASLEH CSECT
         USING  LUCASLEH,R12
SAVEARA  B      STM-SAVEARA(R15)
         DC     17F'0'
         DC     CL8'LUCASLEH'
STM      STM    R14,R12,12(R13) save calling context
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R12,R15         set addessability
*        ----   CODE
         LA     R2,2            R2=2
         LA     R11,0           R11:N'
         BCTR   R11,0           N':=X'FFFFFFFF'
         LA     R10,1           R10:N N=1
         LA     R4,1            R4:IEXP
         LA     R6,1            step
         LH     R7,IEXPMAX      R7:IEXPMAX limit
LOOPE    BXH    R4,R6,ENDLOOPE	do iexp=2 to iexpmax
         SR     R3,R3           R3:S S=0
         CR     R4,R2           if iexp=2 then S=0
         BE     OKS
         LA     R3,4            else S=4
OKS      EQU    *
         SLDA   R10,1           n=(n+1)*2-1
         LA     R5,0            I
         LA     R8,1            step
         LR     R9,R4           IEXP
         SR     R9,R2           IEXP-2 limit
LOOPI    BXH    R5,R8,ENDLOOPI	do i=1 to iexp-2
*        ----   compute s=(s*s-2) MOD n
         SR     R14,R14         R14=0
         LR     R15,R3          R15=S
         MR     R14,R3          R{14-15}=S*S
         SLR    R15,R2          R15=R15-2=S*S-2
         BNM    *+6             skip next if no borrow
         BCTR   R14,0           perform borrow
         DR     R14,R10         R10=N
         LR     R3,R14          R14=MOD
         B      LOOPI
ENDLOOPI EQU    *
         LTR    R3,R3
         BNZ    NOPRT           if s<>0 then no print
         CVD    R4,P            store to packed P
         UNPK   Z,P             Z=P
         MVC    C,Z             C=Z
         OI     C+L'C-1,X'F0'   zap sign
         MVC    WTOBUF(4),C+12
         MVI    WTOBUF,C'M'
         WTO    MF=(E,WTOMSG)
NOPRT    EQU    *
         B      LOOPE
ENDLOOPE EQU    *
*        ----   END CODE
RETURN   EQU    *
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
*        ----   DATA
IEXPMAX  DC     H'31'
I        DS     H
IEXP     DS     H
S        DS     F
N        DS     F
P        DS     PL8             packed
Z        DS     ZL16            zoned
C        DS     CL16            character
WTOMSG   DS     0F
         DC     H'80',XL2'0000'
WTOBUF   DC     80C' '
         LTORG
         YREGS
         END    LUCASLEH
```

{{out}}

```txt
M002
M003
M005
M007
M013
M017
M019
M031
```



## Ada


```ada
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

procedure Lucas_Lehmer_Test is
   type Ull is mod 2**64;
   function Mersenne(Item : Integer) return Boolean is
      S : Ull := 4;
      MP : Ull := 2**Item - 1;
   begin
      if Item = 2 then
         return True;
      else
         for I in 3..Item loop
            S := (S * S - 2) mod MP;
         end loop;
         return S = 0;
      end if;
   end Mersenne;
   Upper_Bound : constant Integer := 64;
begin
   Put_Line(" Mersenne primes:");
   for P in 2..Upper_Bound loop
      if Mersenne(P) then
         Put(" M");
         Put(Item => P, Width => 1);
      end if;
   end loop;
end Lucas_Lehmer_Test;
```

{{Out}}
 Mersenne primes:
 M2 M3 M5 M7 M13 M17 M19 M31


## Agena

Because of the very large numbers computed,
the mapm binding is used to calculate with arbitrary precision.

```agena
readlib 'mapm';

mapm.xdigits(100);

mersenne := proc(p::number) is
   local s, m;
   s := 4;
   m := mapm.xnumber(2^p) - 1;
   if p = 2 then
      return true
   else
      for i from 3 to p do
         s := (mapm.xnumber(s)^2 - 2) % m
      od;
      return mapm.xtoNumber(s) = 0
   fi
end;

for i from 3 to 64 do
   if mersenne(i) then
      write('M' & i & ' ')
   fi
od;
```

produces:

```agena>M3 M5 M7 M13 M17 M19 M31</lang



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''}}

```algol68
PRAGMAT stack=1M precision=20000 PRAGMAT

PROC is prime = ( INT p )BOOL:
  IF p = 2 THEN TRUE
  ELIF p <= 1 OR p MOD 2 = 0 THEN FALSE
  ELSE
    BOOL prime := TRUE;
    FOR i FROM 3 BY 2 TO ENTIER sqrt(p)
      WHILE prime := p MOD i /= 0 DO SKIP OD;
    prime
  FI;

PROC is mersenne prime = ( INT p )BOOL:
  IF p = 2 THEN TRUE
  ELSE
    LONG LONG INT m p :=  LONG LONG 2 ** p - 1, s := 4;
    FROM 3 TO p DO
      s := (s ** 2 - 2) MOD m p
    OD;
    s = 0
  FI;

test:(
  INT upb prime = ( long long bits width - 1 ) OVER 2; # no unsigned #
  INT upb count = 45; # find 45 mprimes if INT has enough bits #

  printf(($" Finding Mersenne primes in M[2.."g(0)"]: "l$,upb prime));

  INT count:=0;
  FOR p FROM 2 TO upb prime WHILE
    IF is prime(p) THEN
      IF is mersenne prime(p) THEN
        printf (($" M"g(0)$,p));
        count +:= 1
      FI
    FI;
    count <= upb count
  DO SKIP OD
)
```

{{Out}}

```txt

Finding Mersenne primes in M[2..33252]:
M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213 M19937 M21701 M23209

```

See also: http://www.xs4all.nl/~jmvdveer/mersenne.a68.html


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program lucaslehmer.s   */
/* use library gmp     */
/* link with gcc option -lgmp */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ NBRECH,          30

/* Initialized data */
.data
szMessResult:       .ascii "Prime : M"
sMessValeur:        .fill 11, 1, ' '            @ size => 11
                    .asciz "\n"

szCarriageReturn:   .asciz "\n"
szformat:           .asciz "nombre= %Zd\n"

/* UnInitialized data */
.bss
.align 4
spT:                .skip 100
mpT:                .skip 100
Deux:               .skip 100
snT:                .skip 100
/*  code section */
.text
.global main
main:
    ldr r0,iAdrDeux                       @ create big number = 2
    mov r1,#2
    bl __gmpz_init_set_ui
    ldr r0,iAdrspT                        @ init big number
    bl __gmpz_init
    ldr r0,iAdrmpT                        @ init big number
    bl __gmpz_init
    mov r5,#3                             @ start number
    mov r6,#0                             @ result counter
1:
    ldr r0,iAdrspT                        @ conversion integer in big number gmp
    mov r1,r5
    bl __gmpz_set_ui
    ldr r0,iAdrspT                        @ control if exposant is prime !
    ldr r0,iAdrspT
    mov r1,#25
    bl __gmpz_probab_prime_p
    cmp r0,#0
    beq 5f

2:
    //ldr r1,iAdrspT                      @ example number display
    //ldr r0,iAdrszformat
    //bl __gmp_printf
/******** Compute (2 pow p) - 1   ******/
    ldr r0,iAdrmpT                        @ compute 2 pow p
    ldr r1,iAdrDeux
    mov r2,r5
    bl __gmpz_pow_ui
    ldr r0,iAdrmpT
    ldr r1,iAdrmpT
    mov r2,#1
    bl __gmpz_sub_ui                      @ then (2 pow p) - 1

    ldr r0,iAdrsnT
    mov r1,#4
    bl __gmpz_init_set_ui                 @ init big number with 4

/**********  Test lucas_lehner  *******/
    mov r4,#2                             @ loop counter
3:                                        @ begin loop
    ldr r0,iAdrsnT
    ldr r1,iAdrsnT
    mov r2,#2
    bl __gmpz_pow_ui                      @ compute square big number

    ldr r0,iAdrsnT
    ldr r1,iAdrsnT
    mov r2,#2
    bl __gmpz_sub_ui                      @ = (sn *sn) - 2

    ldr r0,iAdrsnT                        @ compute remainder -> sn
    ldr r1,iAdrsnT                        @ sn
    ldr r2,iAdrmpT                        @ p
    bl __gmpz_tdiv_r

    //ldr r1,iAdrsnT                      @ display number for control
    //ldr r0,iAdrszformat
    //bl __gmp_printf

    add r4,#1                             @ increment counter
    cmp r4,r5                             @ end ?
    blt 3b                                @ no -> loop
                                          @ compare result with zero
    ldr r0,iAdrsnT
    mov r1,#0
    bl __gmpz_cmp_d
    cmp r0,#0
    bne 5f
/********* is prime display result      *********/
    mov r0,r5
    ldr r1,iAdrsMessValeur                @ display value
    bl conversion10                       @ call conversion decimal
    ldr r0,iAdrszMessResult               @ display message
    bl affichageMess
    add r6,#1                             @ increment counter result
    cmp r6,#NBRECH
    bge 10f
5:
    add r5,#2                             @ increment number by two
    b 1b                                  @ and loop

10:
    ldr r0,iAdrDeux                       @ clear memory big number
    bl __gmpz_clear
    ldr r0,iAdrsnT
    bl __gmpz_clear
    ldr r0,iAdrmpT
    bl __gmpz_clear
    ldr r0,iAdrspT
   bl __gmpz_clear
100:                                      @ standard end of the program
    mov r0, #0                            @ return code
    mov r7, #EXIT                         @ request to exit program
    svc 0                                 @ perform system call
iAdrszMessResult:         .int szMessResult
iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrszformat:             .int szformat
iAdrspT:                  .int spT
iAdrmpT:                  .int mpT
iAdrDeux:                 .int Deux
iAdrsnT:                  .int snT
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return
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


```

{{Out}}

```txt

Prime : M3
Prime : M5
Prime : M7
Prime : M13
Prime : M17
Prime : M19
Prime : M31
Prime : M61
Prime : M89
Prime : M107
Prime : M127
Prime : M521
Prime : M607
Prime : M1279
Prime : M2203
Prime : M2281
Prime : M3217
Prime : M4253
Prime : M4423
Exception en point flottant

```


## AWK


```AWK

# syntax: GAWK -f LUCAS-LEHMER_TEST.AWK
# converted from Pascal
BEGIN {
    printf("Mersenne primes:")
    n = 1
    for (exponent=2; exponent<=32; exponent++) {
      s = (exponent == 2) ? 0 : 4
      n = (n+1)*2-1
      for (i=1; i<=exponent-2; i++) {
        s = (s*s-2)%n
      }
      if (s == 0) {
        printf(" M%s",exponent)
      }
    }
    printf("\n")
    exit(0)
}

```

{{Out}}

```txt

Mersenne primes: M2 M3 M5 M7 M13 M17 M19

```


## BBC BASIC

{{works with|BBC BASIC for Windows}}
Using its native arithmetic BBC BASIC can only test up to M23.

```bbcbasic
      *FLOAT 64
      PRINT "Mersenne Primes:"
      FOR p% = 2 TO 23
        IF FNlucas_lehmer(p%) PRINT "M" ; p%
      NEXT
      END

      DEF FNlucas_lehmer(p%)
      LOCAL i%, mp, sn
      IF p% = 2 THEN = TRUE
      IF (p% AND 1) = 0 THEN = FALSE
      mp = 2^p% - 1
      sn = 4
      FOR i% = 3 TO p%
        sn = sn^2 - 2
        sn -= (mp * INT(sn / mp))
      NEXT
      = (sn = 0)
```

{{Out}}

```txt

Mersenne Primes:
M2
M3
M5
M7
M13
M17
M19

```



## Bracmat

Only <code>exponent</code>s that are prime are tried. The primality test of these numbers uses a side effect of Bracmat's attempt at
computing a root of a small enough number. ('small enough' meaning that the number must fit in a computer word, normally 32 or 64 bits.)
To do that, Bracmat first creates a list of factors of the number and then takes the root of each factor. For example, to compute <code>54^2/3</code>,
Bracmat first creates the expression <code>(2*3^3)^2/3</code> and then <code>2^2/3*3^(3*2/3)</code>, which becomes <code>2^2/3*9</code>.
If a number cannot be factorized, (either because it is prime or because it is to great to fit in a computer word) the root expression doesn't change much.
For example, the expression <code>13^(13^-1)</code> becomes <code>13^1/13</code>, and this matches the pattern <code>13^%</code>.

```bracmat
  ( clk$:?t0:?now
  & ( time
    =   ( print
        =
          .   put
            $ ( str
              $ ( div$(!arg,1)
                  ","
                  (   div$(mod$(!arg*100,100),1):?arg
                    & !arg:<10
                    & 0
                  |
                  )
                  !arg
                  " "
                )
              )
        )
      & -1*!now+(clk$:?now):?SEC
      & print$!SEC
      & print$(!now+-1*!t0)
      & put$"s: "
    )
  & 3:?exponent
  &   whl
    ' ( !exponent:~>12000
      & (   !exponent^(!exponent^-1):!exponent^%
          & 4:?s
          & 2^!exponent+-1:?n
          & 0:?i
          &   whl
            ' ( 1+!i:?i
              & !exponent+-2:~<!i
              & mod$(!s^2+-2.!n):?s
              )
          & (   !s:0
              & !time
              & out$(str$(M !exponent " is PRIME!"))
            |
            )
        |
        )
      & 1+!exponent:?exponent
      )
  & done
  );
```

{{Out}} (after 4.5 hours):

```txt
0,00 0,00 s: M3 is PRIME!
0,00 0,00 s: M5 is PRIME!
0,00 0,00 s: M7 is PRIME!
0,00 0,00 s: M13 is PRIME!
0,00 0,00 s: M17 is PRIME!
0,00 0,01 s: M19 is PRIME!
0,00 0,01 s: M31 is PRIME!
0,00 0,01 s: M61 is PRIME!
0,01 0,02 s: M89 is PRIME!
0,01 0,03 s: M107 is PRIME!
0,00 0,04 s: M127 is PRIME!
0,50 0,54 s: M521 is PRIME!
0,29 0,84 s: M607 is PRIME!
6,81 7,65 s: M1279 is PRIME!
38,35 46,01 s: M2203 is PRIME!
6,32 52,33 s: M2281 is PRIME!
116,01 168,34 s: M3217 is PRIME!
293,09 461,44 s: M4253 is PRIME!
64,61 526,05 s: M4423 is PRIME!
8863,90 9389,95 s: M9689 is PRIME!
1101,12 10491,08 s: M9941 is PRIME!
5618,45 16109,53 s: M11213 is PRIME!
```



## C



### GMP

This uses some pre-tests to show how we can skip some numbers with relatively inexpensive methods.  This also does a simple optimization of the modulus.  It takes about 30 seconds to get to M11213.  This is substantially faster than many of the other solutions, though certainly not comparable to dedicated programs such as Prime95.

Takes an optional argument to test up to the given value.

{{libheader|GMP}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <gmp.h>

int lucas_lehmer(unsigned long p)
{
  mpz_t V, mp, t;
  unsigned long k, tlim;
  int res;

  if (p == 2) return 1;
  if (!(p&1)) return 0;

  mpz_init_set_ui(t, p);
  if (!mpz_probab_prime_p(t, 25)) /* if p is composite, 2^p-1 is not prime */
    { mpz_clear(t); return 0; }

  if (p < 23)                     /* trust the PRP test for these values */
    { mpz_clear(t); return (p != 11); }

  mpz_init(mp);
  mpz_setbit(mp, p);
  mpz_sub_ui(mp, mp, 1);

  /* If p=3 mod 4 and p,2p+1 both prime, then 2p+1 | 2^p-1.  Cheap test. */
  if (p > 3 && p % 4 == 3) {
    mpz_mul_ui(t, t, 2);
    mpz_add_ui(t, t, 1);
    if (mpz_probab_prime_p(t,25) && mpz_divisible_p(mp, t))
      { mpz_clear(mp); mpz_clear(t); return 0; }
  }

  /* Do a little trial division first.  Saves quite a bit of time. */
  tlim = p/2;
  if (tlim > (ULONG_MAX/(2*p)))
    tlim = ULONG_MAX/(2*p);
  for (k = 1; k < tlim; k++) {
    unsigned long q = 2*p*k+1;
    /* factor must be 1 or 7 mod 8 and a prime */
    if ( (q%8==1 || q%8==7) &&
         q % 3 && q % 5 && q % 7 &&
         mpz_divisible_ui_p(mp, q) )
      { mpz_clear(mp); mpz_clear(t); return 0; }
  }

  mpz_init_set_ui(V, 4);
  for (k = 3; k <= p; k++) {
    mpz_mul(V, V, V);
    mpz_sub_ui(V, V, 2);
    /* mpz_mod(V, V, mp) but more efficiently done given mod 2^p-1 */
    if (mpz_sgn(V) < 0) mpz_add(V, V, mp);
    /* while (n > mp) { n = (n >> p) + (n & mp) } if (n==mp) n=0 */
    /* but in this case we can have at most one loop plus a carry */
    mpz_tdiv_r_2exp(t, V, p);
    mpz_tdiv_q_2exp(V, V, p);
    mpz_add(V, V, t);
    while (mpz_cmp(V, mp) >= 0) mpz_sub(V, V, mp);
  }
  res = !mpz_sgn(V);
  mpz_clear(t); mpz_clear(mp); mpz_clear(V);
  return res;
}

int main(int argc, char* argv[]) {
  unsigned long i, n = 43112609;
  if (argc >= 2) n = strtoul(argv[1], 0, 10);
  for (i = 1; i <= n; i++) {
    if (lucas_lehmer(i)) {
      printf("M%lu ", i);
      fflush(stdout);
    }
  }
  printf("\n");
  return 0;
}
```


{{out}}
(partial output after 50 minutes)

```txt
M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213 M11213 M19937 M21701 M23209 M44497
```



### Small inputs with native types

{{works with|gcc|4.1.2 20070925 (Red Hat 4.1.2-27)}}
{{works with|C99}}
Compiler options: gcc -std=c99 -lm Lucas-Lehmer_test.c -o Lucas-Lehmer_test


```c
#include <math.h>
#include <stdio.h>
#include <limits.h>
#pragma precision=log10l(ULLONG_MAX)/2

typedef enum { FALSE=0, TRUE=1 } BOOL;

BOOL is_prime( int p ){
  if( p == 2 ) return TRUE;
  else if( p <= 1 || p % 2 == 0 ) return FALSE;
  else {
    BOOL prime = TRUE;
    const int to = sqrt(p);
    int i;
    for(i = 3; i <= to; i+=2)
      if (!(prime = p % i))break;
    return prime;
  }
}

BOOL is_mersenne_prime( int p ){
  if( p == 2 ) return TRUE;
  else {
    const long long unsigned m_p = ( 1LLU << p ) - 1;
    long long unsigned s = 4;
    int i;
    for (i = 3; i <= p; i++){
      s = (s * s - 2) % m_p;
    }
    return s == 0;
  }
}

int main(int argc, char **argv){

  const int upb = log2l(ULLONG_MAX)/2;
  int p;
  printf(" Mersenne primes:\n");
  for( p = 2; p <= upb; p += 1 ){
    if( is_prime(p) && is_mersenne_prime(p) ){
      printf (" M%u",p);
    }
  }
  printf("\n");
}
```


{{Out}}

```txt

 Mersenne primes:
 M2 M3 M5 M7 M13 M17 M19 M31

```



## C++

Straightforward method.
{{libheader|GMP}}


```cpp
#include <iostream>
#include <gmpxx.h>

static bool is_mersenne_prime(mpz_class p)
{
        if( 2 == p )
                return true;
        else
        {
                mpz_class s(4);
                mpz_class div( (mpz_class(1) << p.get_ui()) - 1 );
                for( mpz_class i(3);  i <= p;  ++i )
                {
                        s =  (s * s - mpz_class(2)) % div ;
                }

                return ( s == mpz_class(0) );
        }
}

int main()
{
        mpz_class maxcount(45);
        mpz_class found(0);
        mpz_class check(0);
        for( mpz_nextprime(check.get_mpz_t(), check.get_mpz_t());
             found < maxcount;
             mpz_nextprime(check.get_mpz_t(), check.get_mpz_t()))
        {
                //std::cout << "P" << check << " " << std::flush;
                if( is_mersenne_prime(check) )
                {
                        ++found;
                        std::cout << "M" << check << " " << std::flush;
                }
        }
}
```


{{out}} (Incomplete; It takes a long time.)

```txt

 M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213 M19937 M21701 M23209 M44497

```


## C#
{{works with|Visual Studio|2010}}
{{works with|.NET Framework|4.0}}

```c#
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Threading.Tasks;

namespace LucasLehmerTestForRosettaCode
{
    public class LucasLehmerTest
    {
        static BigInteger ZERO = new BigInteger(0);
        static BigInteger ONE = new BigInteger(1);
        static BigInteger TWO = new BigInteger(2);
        static BigInteger FOUR = new BigInteger(4);

        private static bool isMersennePrime(int p)
        {
            if (p % 2 == 0) return (p == 2);
            else {
                for (int i = 3; i <= (int)Math.Sqrt(p); i += 2)
                    if (p % i == 0) return false; //not prime
                BigInteger m_p = BigInteger.Pow(TWO, p) - ONE;
                BigInteger s = FOUR;
                for (int i = 3; i <= p; i++)
                    s = (s * s - TWO) % m_p;
                return s == ZERO;
            }
        }

        public static int[] GetMersennePrimeNumbers(int upTo)
        {
            List<int> response = new List<int>();
            Parallel.For(2, upTo + 1, i => {
                if (isMersennePrime(i)) response.Add(i);
            });
            response.Sort();
            return response.ToArray();
        }

        static void Main(string[] args)
        {
            int[] mersennePrimes = LucasLehmerTest.GetMersennePrimeNumbers(11213);
            foreach (int mp in mersennePrimes)
                Console.Write("M" + mp+" ");
            Console.ReadLine();
        }
    }
}
```


{{out}} (Run only to 11213)

```txt

 M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213

```



## Clojure


```lisp
(defn prime? [i]
  (cond (< i 4)           (>= i 2)
        (zero? (rem i 2)) false
  :else (not-any? #(zero? (rem i %)) (range 3 (inc (Math/sqrt i))))))))

(defn mersenne? [p] (or (= p 2)
  (let [mp   (dec (bit-shift-left 1 p))]
    (loop [n 3 s 4]
      (if (> n p)
        (zero? s)
        (recur (inc n) (rem (- (* s s) 2) mp)))))))

(filter mersenne? (filter prime? (iterate inc 1)))
```

{{Out}}

```txt

 Infinite list of Mersenne primes:
 (2 3 5 7 13 17 19 31 61 89 107 127 521 607 1279 2203 2281 3217 4253...

```



## Common Lisp

{{trans|Clojure}}

```lisp

(defun or-f (&optional a b) (or a b));necessary for reduce, as 'or' is implemented as a macro

(defun prime-p (n)
  (cond ((< n  4) (>= n 2))
        ((zerop (rem n 2)) nil)
        (t (not (reduce #'or-f (mapcar (lambda (x) (zerop (rem n x))) (loop for i from 3 to (sqrt n) collect i)))))))

(defun mersenne-p (p)
  (or (= p 2)
      (let ((mp (- 1 (expt 2 p))))
        (do ((n 3) (s 4))
          ((> n p) (zerop s))
          (incf n)
          (setf s (rem (- (* s s) 2) mp))))))

(princ (remove-if-not #'mersenne-p (remove-if-not #'prime-p (loop for i to 5000 collect i))))

```

{{out}}

```txt

(2 3 5 7 13 17 19 31 61 89 107 127 521 607 1279 2203 2281 3217 4253 4423)

```




## D

{{trans|Python}}

```d
import std.stdio, std.math, std.bigint;

bool isPrime(in uint p) pure nothrow @safe @nogc {
    if (p < 2 || p % 2 == 0)
        return p == 2;
    foreach (immutable i; 3 .. cast(uint)real(p).sqrt + 1)
        if (p % i == 0)
            return false;
    return true;
}

bool isMersennePrime(in uint p) pure nothrow /*@safe*/ {
    if (!p.isPrime)
        return false;
    if (p == 2)
        return true;
    immutable mp = (1.BigInt << p) - 1;
    auto s = 4.BigInt;
    foreach (immutable _; 3 .. p + 1)
        s = (s ^^ 2 - 2) % mp;
    return s == 0;
}

void main() {
    foreach (immutable p; 2 .. 2_300)
        if (p.isMersennePrime) {
            write('M', p, ' ');
            stdout.flush;
        }
}
```

{{out}}

```txt
M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281
```

With p up to 10_000 it prints:

```txt
M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9941
```



## DWScript

Using Integer type, which is 64bit, limits the search to M31.

```delphi
function IsMersennePrime(p : Integer) : Boolean;
var
   i, s, m_p : Integer;
begin
   if p=2 then
      Result:=True
   else begin
      m_p := (1 shl p)-1;
      s := 4;
      for i:=3 to p do
         s:=(s*s-2) mod m_p;
      Result:=(s=0);
   end;
end;

const upperBound = Round(Log2(High(Integer))/2);

PrintLn('Finding Mersenne primes in M[2..' + IntToStr(upperBound) + ']: ');
Print('M2');

var p : Integer;
for p:=3 to upperBound step 2 do begin
   if IsMersennePrime(p) then
      Print(' M'+IntToStr(p));
end;
PrintLn('');
```

{{Out}}

```txt
 M2 M3 M5 M7 M13 M17 M19 M31
```



## EchoLisp


```scheme

(require 'bigint)
(define (mersenne-prime? odd-prime: p)
	(define mp (1- (expt 2 p)))
	(define s #4)
	(for [(i (in-range 3 (1+ p)))]
		(set! s (% (- (* s s) 2) mp)))
	(when (zero? s) (printf "M%d" p)))

;; run it in the background
(require 'tasks)
(define LP (primes 10000)) ; list of candidate primes

(define (mp-task LP)
	(mersenne-prime? (first LP))
	(rest LP)) ;; return next state

(task-run (make-task mp-task LP))

→  M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule LucasLehmer do
  use Bitwise
  def test do
    for p <- 2..1300, p==2 or s(bsl(1,p)-1, p-1)==0, do: IO.write "M#{p} "
  end

  defp s(mp, 1), do: rem(4, mp)
  defp s(mp, n) do
    x = s(mp, n-1)
    rem(x*x-2, mp)
  end
end

LucasLehmer.test
```


{{out}}

```txt

M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279

```



## Erlang


```erlang
-module(mp).
-export([main/0]).

main() -> [ io:format("M~p ", [P]) || P <- lists:seq(2,700), (P == 2) orelse (s((1 bsl P) - 1, P-1) == 0) ].

s(MP,1) -> 4 rem MP;
s(MP,N) -> X=s(MP,N-1), (X*X - 2) rem MP.
```


In 3 seconds will print

```txt

M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607

```

Testing larger numbers (i.e. 5000) is possible but will take few minutes.


## ERRE

With native arithmetic up to 23: for bigger numbers you must use MULPREC program.

```ERRE
PROGRAM LL_TEST

!$DOUBLE

PROCEDURE LUCAS_LEHMER(P%->RES)
     LOCAL I%,MP,SN
     IF P%=2 THEN RES%=TRUE EXIT PROCEDURE END IF
     IF (P% AND 1)=0 THEN RES%=FALSE EXIT PROCEDURE END IF
     MP=2^P%-1
     SN=4
     FOR I%=3 TO P% DO
        SN=SN^2-2
        SN-=(MP*INT(SN/MP))
     END FOR
     RES%=(SN=0)
END PROCEDURE

BEGIN
     PRINT("Mersenne Primes:")
     FOR P%=2 TO 23 DO
        LUCAS_LEHMER(P%->RES%)
        IF RES% THEN PRINT("M";P%) END IF
     END FOR
END PROGRAM

```

{{out}}

```txt
Mersenne Primes:
M 2
M 3
M 5
M 7
M 13
M 17
M 19

```


=={{header|F Sharp|F#}}==
Simple arbitrary-precision version:

```fsharp
let rec s mp n =
  if n = 1 then 4I % mp else ((s mp (n - 1)) ** 2 - 2I) % mp

[ for p in 2..47 do
    if p = 2 || s ((1I <<< p) - 1I) (p - 1) = 0I then
      yield p ]
```


Tail-recursive version:

```fsharp
let IsMersennePrime exponent =
    if exponent <= 1 then failwith "Exponent must be >= 2"
    let prime = 2I ** exponent - 1I;

    let rec LucasLehmer i acc =
        match i with
        | x when x = exponent - 2 -> acc
        | x -> LucasLehmer (x + 1) ((acc*acc - 2I) % prime)

    LucasLehmer 0 4I = 0I

```


Version using library folding function (way shorter and faster than the above):

```fsharp
let IsMersennePrime exponent =
    if exponent <= 1 then failwith "Exponent must be >= 2"
    let prime = 2I ** exponent - 1I;

    let LucasLehmer =
        [| 1 .. exponent-2 |] |> Array.fold (fun acc _ -> (acc*acc - 2I) % prime) 4I

    LucasLehmer = 0I

```



## Factor


```factor
USING: io math.primes.lucas-lehmer math.ranges prettyprint
sequences ;

47 [1,b] [ lucas-lehmer ] filter
"Mersenne primes:" print
[ "M" write pprint bl ] each nl
```

{{out}}

```txt

Mersenne primes:
M2 M3 M5 M7 M13 M17 M19 M31

```



## Forth

<lang>: lucas-lehmer
  1+ 2 do
    4 i 2 <> * abs swap 1+ dup + 1- swap
    i 1- 1 ?do dup * 2 - over mod loop 0= if ." M" i . then
  loop cr
;

1 15 lucas-lehmer
```



## Frink

Frink's <CODE>isPrime</CODE> function automatically detects numbers of the form 2<sup>n</sup>-1 and performs a Lucas-Lehmer test on them, including testing if n is prime, which is sufficient to prove primality for this form.


```frink

for n = primes[]
   if isPrime[2^n-1]
      println[n]

```



## Fortran

{{works with|Fortran|90 and later}}
Only Mersenne number with prime exponent can be themselves prime but for the small numbers used in this example it was not worth the effort to include this check. As the size of the exponent increases this becomes more important.

```fortran
PROGRAM LUCAS_LEHMER
  IMPLICIT NONE

  INTEGER, PARAMETER :: i64 = SELECTED_INT_KIND(18)
  INTEGER(i64) :: s, n
  INTEGER :: i, exponent

  DO exponent = 2, 31
     IF (exponent == 2) THEN
        s = 0
     ELSE
        s = 4
     END IF
     n = 2_i64**exponent - 1
     DO i = 1, exponent-2
        s = MOD(s*s - 2, n)
     END DO
     IF (s==0) WRITE(*,"(A,I0,A)") "M", exponent, " is PRIME"
  END DO

END PROGRAM LUCAS_LEHMER
```


## FreeBASIC


### Native types for Mersenne primes <= M63


```freebasic
' version 18-09-2015
' compile with: fbc -s console

#Ifndef TRUE        ' define true and false for older freebasic versions
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function mul_mod(a As ULongInt, b As ULongInt, modulus As ULongInt) As ULongInt
    ' returns a * b mod modulus

    Dim As ULongInt x , y = a ' a mod modulus, but a is already smaller then modulus

    While b > 0
        If (b And 1) = 1 Then
            x = (x + y) Mod modulus
        End If
        y = (y Shl 1) Mod modulus
        b = b Shr 1
    Wend
    Return x

End Function

Function LLT(p As UInteger) As Integer

    Dim As ULongInt s = 4, m = 1
    m = m Shl p : m = m - 1       ' m = 2 ^ p - 1

    For i As Integer = 2 To p - 1
        s = mul_mod(s, s, m) - 2
    Next

    If s = 0 Then Return TRUE Else Return FALSE

End Function

' ------=< MAIN >=------

Dim As UInteger p

Print
' M2 can not be tested, we start with 3
for p = 3 To 63
    If LLT(p) = TRUE Then Print " M";Str(p);
Next

Print
' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
 M3 M5 M7 M13 M17 M19 M31 M61
```

==={{libheader|GMP}}===
Uses the trick from the '''C''' entry to avoid the slow Mod

```freebasic
' version 18-09-2015
' compile with: fbc -s console

#Include Once "gmp.bi"

#Macro init_big_int (a)
    Dim As Mpz_ptr a = Allocate( Len(__mpz_struct))
    Mpz_init(a)
#EndMacro

' ------=< MAIN >=------

Const As UInteger max = 12000  ' 230 sec., 10000 about 125 sec.

Dim As UInteger p, x
Dim As Byte sieve(max)

Dim As String buffer = Space(Len(Str(max))+1)

init_big_int(m)
init_big_int(s)
init_big_int(r)

' sieve to find the primes
' remove even numbers except 2
For p = 4 To Sqr(max) Step 2
    sieve(p) = 1
Next

For p = 3 To Sqr(max) Step 2
    For x = p * p To max Step p * 2
        sieve(x) = 1
    Next
Next

' exception: the test will not work for p = 2

For p = 3 To max Step 2            ' odd numbers only

    If sieve(p) = 1 Then Continue For

    Mpz_set_ui(s, 4)                 ' s(0) = 4
    Mpz_set_ui(m, 1)                 ' set m to 1
    Mpz_mul_2exp(m, m, p)            ' m = m shl p =  2 ^ p
    Mpz_sub_ui(m, m, 1)              ' m = m - 1 =  2 ^ p - 1

    For x = 2 To p - 1
        Mpz_mul(s, s, s)               ' s = s * s
        Mpz_sub_ui(s, s, 2)            ' s = s - 2
        ' Mpz_fdiv_r(s, s, m)          ' s = s mod m
        If Mpz_sgn(s) < 0 Then
            Mpz_add(s, s ,m)
        Else
            Mpz_tdiv_r_2exp(r, s, p)
            Mpz_tdiv_q_2exp(s, s, p)
            Mpz_add(s, s, r)
        End If
        If (Mpz_cmp(s, m) >= 0) Then Mpz_sub(s, s, m)
    Next

    'If Mpz_cmp_ui(s, 0) = 0 Then
    '   LSet buffer = Str(p)
    '   Print "M"; buffer; " is prime"
    'End If
    If Mpz_cmp_ui(s, 0) = 0 Then
        Print "M";Str(p),
    End If
Next
Print

Mpz_clear (m)  ' cleanup
DeAllocate(m)
Mpz_clear (s)
DeAllocate(s)
Mpz_clear (r)
DeAllocate(r)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
M3            M5            M7            M13           M17
M19           M31           M61           M89           M107
M127          M521          M607          M1279         M2203
M2281         M3217         M4253         M4423         M9689
M9941         M11213
```



## FunL


```funl
def mersenne( p ) =
  if p == 2 then return true

  var s = 4
  var M = 2^p - 1

  repeat p - 2
    s = (s*s - 2) mod M

  s == 0

import integers.primes

for p <- primes().filter( mersenne ).take( 20 )
  println( 'M' + p )
```


{{out}}


```txt

M2
M3
M5
M7
M13
M17
M19
M31
M61
M89
M107
M127
M521
M607
M1279
M2203
M2281
M3217
M4253
M4423

```



## GAP


```gap
LucasLehmer := function(n)
    local i, m, s;
    if n = 2 then
        return true;
    elif not IsPrime(n) then
        return false;
    else
        m := 2^n - 1;
        s := 4;
        for i in [3 .. n] do
            s := RemInt(s*s, m) - 2;
        od;
        return s = 0;
    fi;
end;

Filtered([1 .. 2000], LucasLehmer);
[2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279]
```



## Go

Processing the first list indicates that the test works.  Processing the second shows it working on some larger numbers.

```go
package main

import (
    "fmt"
    "math/big"
)

var primes = []uint{3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
    53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127}

var mersennes = []uint{521, 607, 1279, 2203, 2281, 3217, 4253, 4423, 9689,
    9941, 11213, 19937, 21701, 23209, 44497, 86243, 110503, 132049, 216091,
    756839, 859433, 1257787, 1398269, 2976221, 3021377, 6972593, 13466917,
    20996011, 24036583}

func main() {
    llTest(primes)
    fmt.Println()
    llTest(mersennes)
}

func llTest(ps []uint) {
    var s, m big.Int
    one := big.NewInt(1)
    two := big.NewInt(2)
    for _, p := range ps {
        m.Sub(m.Lsh(one, p), one)
        s.SetInt64(4)
        for i := uint(2); i < p; i++ {
            s.Mod(s.Sub(s.Mul(&s, &s), two), &m)
        }
        if s.BitLen() == 0 {
            fmt.Printf("M%d ", p)
        }
    }
}
```

{{out}}

```txt

M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127
M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213 M19937...

```



## Haskell

{{works with|GHC|GHCi|6.8.2}}
{{works with|GHC|6.8.2}}


```haskell
module Main
  where

main = printMersennes $ take 45 $ filter lucasLehmer $ sieve [2..]

s mp 1 = 4 `mod` mp
s mp n = ((s mp $ n-1)^2-2) `mod` mp

lucasLehmer 2 = True
lucasLehmer p = s (2^p-1) (p-1) == 0

printMersennes = mapM_ (\x -> putStrLn $ "M" ++ show x)
```

It is pointed out on the [[Sieve of Eratosthenes]] page that the following "sieve" is inefficient. Nonetheless it takes very little time compared to the Lucas-Lehmer test itself.

```haskell
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
```

It takes about 30 minutes to get up to:

```txt

M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213

```



## HicEst


```HicEst
s = 0
DO exponent = 2, 31
  IF(exponent > 2) s = 4
  n = 2^exponent - 1
  DO i = 1, exponent-2
    s = MOD(s*s - 2, n)
  ENDDO
  IF(s == 0) WRITE(Messagebox) 'M', exponent, ' is prime;', n
ENDDO

END
```



## J


See [[j:Essays/Primality%20Tests#Lucas-Lehmer|Primality Tests essay on the J wiki]].


## Java

We use arbitrary-precision integers in order to be able to test any arbitrary prime.


```java
import java.math.BigInteger;
public class Mersenne
{

    public static boolean isPrime(int p) {
        if (p == 2)
            return true;
        else if (p <= 1 || p % 2 == 0)
            return false;
        else {
            int to = (int)Math.sqrt(p);
            for (int i = 3; i <= to; i += 2)
                if (p % i == 0)
                    return false;
            return true;
        }
    }

    public static boolean isMersennePrime(int p) {
        if (p == 2)
            return true;
        else {
            BigInteger m_p = BigInteger.ONE.shiftLeft(p).subtract(BigInteger.ONE);
            BigInteger s = BigInteger.valueOf(4);
            for (int i = 3; i <= p; i++)
                s = s.multiply(s).subtract(BigInteger.valueOf(2)).mod(m_p);
            return s.equals(BigInteger.ZERO);
        }
    }

    // an arbitrary upper bound can be given as an argument
    public static void main(String[] args) {
        int upb;
        if (args.length == 0)
            upb = 500;
        else
            upb = Integer.parseInt(args[0]);

        System.out.print(" Finding Mersenne primes in M[2.." + upb + "]:\nM2 ");
        for (int p = 3; p <= upb; p += 2)
            if (isPrime(p) && isMersennePrime(p))
                System.out.print(" M" + p);
        System.out.println();
    }
}
```

{{Out}} (after about eight hours):

```txt

 Finding Mersenne primes in M[2..2147483647]:
 M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213

```



## Javascript

In JavaScript we using BigInt ( numbers with 'n' suffix ) - so we can use really big numbers


```Javascript

////////// In JavaScript we don't have sqrt for BigInt - so here is implementation
    function newtonIteration(n, x0) {
        const x1 = ((n / x0) + x0) >> 1n;
        if (x0 === x1 || x0 === (x1 - 1n)) {
            return x0;
        }
        return newtonIteration(n, x1);
    }

    function sqrt(value) {
        if (value < 0n) {
            throw 'square root of negative numbers is not supported'
        }

        if (value < 2n) {
            return value;
        }
        return newtonIteration(value, 1n);
    }
////////// End of sqrt implementation

    function isPrime(p) {
        if (p == 2n) {
            return true;
        } else if (p <= 1n || p % 2n === 0n) {
            return false;
        } else {
            var to = sqrt(p);
            for (var i = 3n; i <= to; i += 2n)
            if (p % i == 0n) {
                return false;
            }
            return true;
        }
    }

    function isMersennePrime(p) {
        if (p == 2n) {
            return true;
        } else {
            var m_p = (1n << p) - 1n;
            var s = 4n;
            for (var i = 3n; i <= p; i++) {
                s = (s * s - 2n) % m_p;
            }
            return s === 0n;
        }
    }

    var  upb = 5000;
    var tm = Date.now();
    console.log(`Finding Mersenne primes in M[2..${upb}]:`);
    console.log('M2');
    for (var p = 3n; p <= upb; p += 2n){
        if (isPrime(p) && isMersennePrime(p)) {
            console.log("M" + p);
        }
    }
    console.log(`... Took: ${Date.now()-tm} ms`);

```

{{Out}}

```txt

Finding Mersenne primes in M[2..5000]:
M2
M3
M5
M7
M13
M17
M19
M31
M61
M89
M107
M127
M521
M607
M1279
M2203
M2281
M3217
M4253
M4423
... Took: 107748 ms

```



## Julia


```Julia

using Primes

function getmersenneprimes(n)
    t1 = time()
    count = 0
    i = 2
    while(n > count)
        if(isprime(i) && ismersenneprime(2^BigInt(i) - 1))
            println("M$i, cumulative time elapsed: $(time() - t1) seconds")
            count += 1
        end
        i += 1
    end
end

getmersenneprimes(50)

```

{{output}}
```txt

M2, cumulative time elapsed: 0.019999980926513672 seconds
M3, cumulative time elapsed: 0.02200007438659668 seconds
M5, cumulative time elapsed: 0.02200007438659668 seconds
M7, cumulative time elapsed: 0.02200007438659668 seconds
M13, cumulative time elapsed: 0.02200007438659668 seconds
M17, cumulative time elapsed: 0.02200007438659668 seconds
M19, cumulative time elapsed: 0.02200007438659668 seconds
M31, cumulative time elapsed: 0.02200007438659668 seconds
M61, cumulative time elapsed: 0.023000001907348633 seconds
M89, cumulative time elapsed: 0.024000167846679688 seconds
M107, cumulative time elapsed: 0.02500009536743164 seconds
M127, cumulative time elapsed: 0.026000022888183594 seconds
M521, cumulative time elapsed: 0.12400007247924805 seconds
M607, cumulative time elapsed: 0.14300012588500977 seconds
M1279, cumulative time elapsed: 0.6940000057220459 seconds
M2203, cumulative time elapsed: 2.5870001316070557 seconds
M2281, cumulative time elapsed: 2.88700008392334 seconds
M3217, cumulative time elapsed: 8.276000022888184 seconds
M4253, cumulative time elapsed: 20.874000072479248 seconds
M4423, cumulative time elapsed: 23.56000018119812 seconds
M9689, cumulative time elapsed: 338.970999956131 seconds
M9941, cumulative time elapsed: 373.2020001411438 seconds
M11213, cumulative time elapsed: 557.3210000991821 seconds
M19937, cumulative time elapsed: 3963.986000061035 seconds
M21701, cumulative time elapsed: 5330.933000087738 seconds
M23209, cumulative time elapsed: 6783.236999988556 seconds
M44497, cumulative time elapsed: 57961.360000133514 seconds

```



## Kotlin

In view of the Java result, I've set the program to stop at M4423 so it will run in a reasonable time (about 85 seconds) on a typical laptop:

```scala
// version 1.0.6

import java.math.BigInteger

const val MAX = 19

val bigTwo  = BigInteger.valueOf(2L)
val bigFour = bigTwo * bigTwo

fun isPrime(n: Int): Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d : Int = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun main(args: Array<String>) {
    var count = 0
    var p = 3   // first odd prime
    var s: BigInteger
    var m: BigInteger
    while (true) {
        m = bigTwo.shiftLeft(p - 1) - BigInteger.ONE
        s = bigFour
        for (i in 1 .. p - 2) s = (s * s - bigTwo) % m
        if (s == BigInteger.ZERO) {
            count +=1
            print("M$p ")
            if (count == MAX) {
                println()
                break
            }
        }
        // obtain next odd prime
        while(true) {
            p += 2
            if (isPrime(p)) break
        }
    }
}
```


{{out}}

```txt

M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423

```



## Mathematica

This version is very speedy and is bounded.

```Mathematica
Select[Table[M = 2^p - 1;
  For[i = 1; s = 4, i <= p - 2, i++, s = Mod[s^2 - 2, M]];
  If[s == 0, "M" <> ToString@p, p], {p,
   Prime /@ Range[300]}], StringQ]

=> {M3, M5, M7, M13, M17, M19, M31, M61, M89, M107, M127, M521, M607, M1279}
```


This version is unbounded (and timed):

```Mathematica
t = SessionTime[];
For[p = 2, True, p = NextPrime[p], M = 2^p - 1;
 For[i = 1; s = 4, i <= p - 2, i++, s = Mod[s^2 - 2, M]];
 If[s == 0, Print["M" <> ToString@p]]]
(SessionTime[] - t) {Seconds, Minutes/60, Hours/3600, Days/86400}
```


I'll see what this gets.


## MATLAB


MATLAB suffers from a lack of an arbitrary precision math (bignums) library.
It also doesn't have great support for 64-bit integer arithmetic...or at least MATLAB 2007 doesn't. So, the best precision we have is doubles; therefore, this script can only find up to M19 and no greater.

```MATLAB
function [mNumber,mersennesPrime] = mersennePrimes()

    function isPrime = lucasLehmerTest(thePrime)

        llResidue = 4;
        mersennesPrime = (2^thePrime)-1;

        for i = ( 1:thePrime-2 )
            llResidue = mod( ((llResidue^2) - 2),mersennesPrime );
        end

        isPrime = (llResidue == 0);

    end

    %Because IEEE764 Double is the highest precision number we can
    %represent in MATLAB, the highest Mersenne Number we can test is 2^52.
    %In addition, because we have this cap, we can only test up to the
    %number 30 for Mersenne Primeness. When we input 31 into the
    %Lucas-Lehmer test, during the computation of the residue, the
    %algorithm multiplies two numbers together the result of which is
    %greater than 2^53. Because we require every digit to be significant,
    %this leads to an error. The Lucas-Lehmer test should say that M31 is a
    %Mersenne Prime, but because of the rounding error in calculating the
    %residues caused by floating-point arithmetic, it does not. So M30 is
    %the largest number we test.

    mNumber = (3:30);

    [isPrime] = arrayfun(@lucasLehmerTest,mNumber);

    mNumber = [2 mNumber(isPrime)];
    mersennesPrime = (2.^mNumber) - 1;

end
```


{{Out}}

```MATLAB
[mNumber,mersennesPrime] = mersennePrimes

mNumber =

     2     3     5     7    13    17    19


mersennesPrime =

           3           7          31         127        8191      131071      524287
```



## Maxima


```maxima
lucas_lehmer(p) := block([s, n, i],
   if not primep(p) then false elseif p = 2 then true else
   (s: 4,
   n: 2^p - 1,
   for i: 2 thru p - 1 do s: mod(s*s - 2, n),
   is(s = 0))
)$

sublist(makelist(i, i, 1, 200), lucas_lehmer);
/* [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127] */
```


=={{header|Modula-3}}==
Modula-3 uses L as the literal for <tt>LONGINT</tt>.

```modula3
MODULE LucasLehmer EXPORTS Main;

IMPORT IO, Fmt, Long;

PROCEDURE Mersenne(p: CARDINAL): BOOLEAN =
  VAR
    s := 4L;
    m := Long.Shift(1L, p) - 1L; (* 2^p - 1 *)
  BEGIN
    IF p = 2 THEN
      RETURN TRUE;
    ELSE
      FOR i := 3 TO p DO
        s := (s * s - 2L) MOD m;
      END;
      RETURN s = 0L;
    END;
  END Mersenne;

BEGIN
  FOR i := 2 TO 63 DO
    IF Mersenne(i) THEN
      IO.Put("M" & Fmt.Int(i) & " ");
    END;
  END;
  IO.Put("\n");
END LucasLehmer.
```

{{Out}}

```txt
M2 M3 M5 M7 M13 M17 M19 M31
```



## Nim


```nim
import math

proc isPrime(a: int): bool =
  if a == 2: return true
  if a < 2 or a mod 2 == 0: return false
  for i in countup(3, int sqrt(float a), 2):
    if a mod i == 0:
      return false
  return true

proc isMersennePrime(p: int): bool =
  if p == 2: return true
  let mp = (1'i64 shl p) - 1
  var s = 4'i64
  for i in 3 .. p:
    s = (s * s - 2) mod mp
  result = s == 0

let upb = int((log2 float int64.high) / 2)
echo " Mersenne primes:"
for p in 2 .. upb:
  if isPrime(p) and isMersennePrime(p):
    stdout.write " M",p
echo ""
```

{{Out}}

```txt
 Mersenne primes:
 M2 M3 M5 M7 M13 M17 M19 M31
```



## Oz

Oz's multiple precision number system use GMP core.

```oz
%% compile : ozc -x <file.oz>
functor
import
  Application
  System
define

  fun {Arg Idx Default}
    Cmd = {Application.getArgs plain}
    Len = {Length Cmd}
  in
    if Len < Idx then
      Default
    else
      {StringToInt {Nth Cmd Idx}}
    end
  end

  fun {LLtest N}
    Mp = {Pow 2 N} - 1
    fun {S K} X T
    in
      if K == 1 then 4
      else
        T = {S K-1}
        X = T * T - 2
        X mod Mp
      end
    end
  in
    if N == 2 then
      true
    else
      {S N-1} == 0
    end
  end

  proc {FindLL X}
    fun {Sieve Ls}
      case Ls of nil then nil
      [] X|Xs then
        fun {DIV M} M mod X \= 0  end
      in
        X|{Sieve {Filter Xs DIV}}
      end
    end
  in
    if {IsList X} then
      case X of nil then skip
      [] M|Ms then
        {System.printInfo "M"#M#" "}
        {FindLL Ms}
      end
    else
      {FindLL {Filter {Sieve 2|{List.number 3 X 2}} LLtest}}
    end
  end

  Num = {Arg 1 607}

  {FindLL Num}

  {Application.exit 0}
end
```



## PARI/GP


```parigp
LL(p)={
  my(m=Mod(4,1<<p-1));
  for(i=3,p,m=m^2-2);
  m==0
};

search()={
  print("2^2-1");
  forprime(p=3,43112609,
    if(LL(p), print("2^"p"-1"))
  )
};
```



## Pascal

int64 is good enough up to M31:

```pascal
Program LucasLehmer(output);
var
  s, n: int64;
  i, exponent: integer;
begin
  n := 1;
  for exponent := 2 to 31 do
  begin
    if exponent = 2 then
      s := 0
    else
      s := 4;
    n := (n + 1)*2 - 1;  // This saves from needing the math unit for exponentiation
    for i := 1 to exponent-2 do
      s := (s*s - 2) mod n;
    if s = 0 then
      writeln('M', exponent, ' is PRIME!');
  end;
end.
```

{{Out}}

```txt
:> ./LucasLehmer
M2 is PRIME!
M3 is PRIME!
M5 is PRIME!
M7 is PRIME!
M13 is PRIME!
M17 is PRIME!
M19 is PRIME!
M31 is PRIME!

```



## Perl

Using [https://metacpan.org/pod/Math::GMP Math::GMP]:

```perl
use Math::GMP qw/:constant/;

sub is_prime { Math::GMP->new(shift)->probab_prime(12); }

sub is_mersenne_prime {
  my $p = shift;
  return 1 if $p == 2;
  my $mp = 2 ** $p - 1;
  my $s = 4;
  $s = ($s * $s - 2) % $mp  for 3..$p;
  $s == 0;
}

foreach my $p (2 .. 43_112_609) {
  print "M$p\n" if is_prime($p) && is_mersenne_prime($p);
}
```


The ntheory module offers a couple options.  This is direct:
{{libheader|ntheory}}

```perl
use ntheory qw/:all/;
$|=1; # flush output on every print
my $n = 0;
for (1..47) {
  1 while !is_mersenne_prime(++$n);
  print "M$n ";
}
print "\n";
```

However it uses knowledge from the thousands of CPU years spent by GIMPS to accelerate results for known values, so doesn't actually run the L-L test until after the 44th value, although code is included for C, Perl, and C+GMP.  If we substitute <tt>Math::Prime::Util::GMP::is_mersenne_prime</tt> we can force the test to run.

A less opaque method uses the modular Lucas sequence, though it has no pretesting other than primality and calculates both <math>U_k</math> and <math>V_k</math> so won't be as fast:

```perl
use ntheory qw/:all/;
use bigint try=>"GMP,Pari";
forprimes {
  my $p = $_;
  my $mp1 = 2**$p;
  print "M$p\n" if $p == 2 || 0 == (lucas_sequence($mp1-1, 4, 1, $mp1))[0];
} 43_112_609;
```


We can also use the core module <code>Math::BigInt</code>:
{{trans|Python}}

```perl
sub is_prime {
    my $p = shift;
    if ($p == 2) {
        return 1;
    } elsif ($p <= 1 || $p % 2 == 0) {
        return 0;
    } else {
        my $limit = sqrt($p);
        for (my $i = 3; $i <= $limit; $i += 2) {
            return 0 if $p % $i == 0;
        }
        return 1;
    }
}

sub is_mersenne_prime {
    use bigint;
    my $p = shift;
    if ($p == 2) {
        return 1;
    } else {
        my $m_p = 2 ** $p - 1;
        my $s = 4;

        foreach my $i (3 .. $p) {
            $s = ($s ** 2 - 2) % $m_p;
        }
        return $s == 0;
    }
}

my $precision = 20000;   # maximum requested number of decimal places of 2 ** MP-1 #
my $long_bits_width = $precision / log(2) * log(10);
my $upb_prime = int(($long_bits_width - 1)/2);    # no unsigned #
my $upb_count = 45;      # find 45 mprimes if int was given enough bits #

print " Finding Mersenne primes in M[2..$upb_prime]:\n";

my $count = 0;
foreach my $p (2 .. $upb_prime) {
    if (is_prime($p) && is_mersenne_prime($p)) {
        print "M$p\n";
        $count++;
    }
    last if $count >= $upb_count;
}
```



## Perl 6


```perl6
multi is_mersenne_prime(2) { True }
multi is_mersenne_prime(Int $p) {
    my $m_p = 2 ** $p - 1;
    my $s = 4;
    $s = $s.expmod(2, $m_p) - 2 for 3 .. $p;
    !$s
}

.say for (2,3,5,7 … *).hyper(:8degree).grep( *.is-prime ).map: { next unless .&is_mersenne_prime; "M$_" };
```

{{out|On my system}}
Letting it run for about a minute...

```txt
M2
M3
M5
M7
M13
M17
M19
M31
M61
M89
M107
M127
M521
M607
M1279
M2203
M2281
M3217
M4253
M4423
M9689
M9941
M11213
^C

real	0m55.527s
user	6m47.106s
sys	0m0.404s
```



## Phix

{{libheader|mpfr}}
Native types work up to M31, after which inaccuracies mean that we need to wheel out gmp. Uses the mod replacement trick from C/FreeBASIC(gmp)

```Phix
bool full = true -- (see extended output below)
constant limit = iff(full?20:23)

include mpfr.e

function mersenne(integer p)
    if p = 2 then return true end if
    if not is_prime(p) then return false end if
    mpz s := mpz_init(4),
        m := mpz_init(),
        r = mpz_init()
    mpz_ui_pow_ui(m, 2, p)
    mpz_sub_si(m,m,1)
    for i=3 to p do
        mpz_mul(s,s,s)
        mpz_sub_si(s,s,2)
--      mpz_mod(s,s,m)
        if mpz_sign(s) < 0 then
            mpz_add(s, s ,m)
        else
            mpz_tdiv_r_2exp(r, s, p)
            mpz_tdiv_q_2exp(s, s, p)
            mpz_add(s, s, r)
        end if
        if (mpz_cmp(s, m) >= 0) then mpz_sub(s, s, m) end if
    end for
    bool res = mpz_cmp_si(s,0)=0
    {s,m,r} = mpz_free({s,m,r})
    return res
end function

atom t0 = time(), t1 = t0
integer i=2, j = 1, count = 0
constant mersennes = {1279, 2203, 2281, 3217, 4253, 4423, 9689, 9941, 11213, 19937, 21701,
                      23209, 44497, 86243, 110503, 132049, 216091, 756839, 859433, 1257787,
                      1398269, 2976221, 3021377, 6972593, 13466917, 20996011, 24036583,
                      25964951, 30402457, 32582657, 37156667, 42643801, 43112609}

while count<limit do
    if mersenne(i) then
        count += 1
        string e = iff(time()-t1<0.1?"",", "&elapsed(time()-t1))
        printf(1,"M%d (%d%s)\n",{i,count,e})
        t1 = time()
    end if
    if full or i<1000 then
        i += 1
    else
        i = mersennes[j]
        j += 1
    end if
end while
printf(1,"completed in %s\n",{elapsed(time()-t0)})
```

{{out}}

```txt

M2 (1)
M3 (2)
M5 (3)
M7 (4)
M13 (5)
M17 (6)
M19 (7)
M31 (8)
M61 (9)
M89 (10)
M107 (11)
M127 (12)
M521 (13, 0.1s)
M607 (14)
M1279 (15, 0.7s)
M2203 (16, 2.0s)
M2281 (17, 0.3s)
M3217 (18, 4.0s)
M4253 (19, 8.0s)
M4423 (20, 1.7s)
completed in 16.9s

```

Using the idea from Go of using a mersennes table above 1000 to speed things up, ie by setting full to false we get:

```txt

(ditto)
M1279 (15, 0.3s)
M2203 (16)
M2281 (17)
M3217 (18)
M4253 (19)
M4423 (20)
M9689 (21, 0.5s)
M9941 (22, 0.5s)
M11213 (23, 0.6s)
completed in 2.5s

```

Three more entries in one sixth of the time. Increasing the limit to 31 (with full still false) we can also get

```txt

(ditto)
M19937 (24, 2.1s)
M21701 (25, 2.5s)
M23209 (26, 3.0s)
M44497 (27, 15.3s)
M86243 (28, 1 minute and 12s)
M110503 (29, 1 minute and 53s)
M132049 (30, 2 minutes and 46s)
M216091 (31, 7 minutes and 45s)
completed in 14 minutes and 01s

```

but beyond that I gave up.


## PicoLisp


```PicoLisp
(de prime? (N)
   (or
      (= N 2)
      (and
         (> N 1)
         (bit? 1 N)
         (let S (sqrt N)
            (for (D 3  T  (+ D 2))
               (T (> D S) T)
               (T (=0 (% N D)) NIL) ) ) ) ) )

(de mersenne? (P)
   (or
      (= P 2)
      (let (MP (dec (>> (- P) 1))  S 4)
         (do (- P 2)
            (setq S (% (- (* S S) 2) MP)) )
         (=0 S) ) ) )
```

{{Out}}

```txt
: (for N 10000
   (and (prime? N) (mersenne? N) (println N)) )
2
3
5
7
13
17
19
31
61
89
107
127
521
607
1279
2203
2281
3217
4253
4423
9689
9941
```



## Pop11


Checking large numbers takes a lot of time so we limit p to
be smaller than 1000.


```pop11
define Lucas_Lehmer_Test(p);
   lvars mp = 2**p - 1, sn = 4, i;
   for i from 2 to p - 1 do
       (sn*sn - 2) rem mp -> sn;
   endfor;
   sn = 0;
enddefine;

lvars p = 3;
printf('M2', '%p\n');
while p < 1000 do
   if Lucas_Lehmer_Test(p) then
       printf('M', '%p');
       printf(p, '%p\n');
   endif;
   p + 2 -> p;
endwhile;
```


{{Out}} (obtained in few seconds)

```pop11
M2
M3
M5
M7
M13
M17
M19
M31
M61
M89
M107
M127
M521
M607
```



## PowerShell

This is just a translation of VBScript using [bigint], it could be optimized.
Flirt with the girl in the cubicle next door while it runs:

```PowerShell

function Get-MersennePrime ([bigint]$Maximum = 4800)
{
    [bigint]$n = [bigint]::One

    for ($exp = 2; $exp -lt $Maximum; $exp++)
    {
        if ($exp -eq 2)
        {
            $s = 0
        }
        else
        {
            $s = 4
        }

        $n = ($n + 1) * 2 - 1

        for ($i = 1; $i -le $exp - 2; $i++)
        {
            $s = ($s * $s - 2) % $n
        }

        if ($s -eq 0)
        {
            $exp
        }
    }
}

```


```PowerShell

Get-MersennePrime | Format-Wide {"{0,4}" -f $_} -Column 4 -Force

```

{{Out}}

```txt

   2                                 3                                 5                                7
  13                                17                                19                               31
  61                                89                               107                              127
 521                               607                              1279                             2203
2281                              3217                              4253                             4423

```



## Prolog


```prolog

show(Count) :-
    findall(N, limit(Count, (between(2, infinite, N), mersenne_prime(N))), S),
    forall(member(P, S), (write(P), write(" "))), nl.

lucas_lehmer_seq(M, L) :-
    lazy_list(ll_iter, 4-M, L).

ll_iter(S-M, T-M, T) :-
    T is ((S*S) - 2) mod M.

drop(N, Lz1, Lz2) :-
    append(Pfx, Lz2, Lz1), length(Pfx, N), !.

mersenne_prime(2).
mersenne_prime(P) :-
    P > 2,
    prime(P),
    M is (1 << P) - 1,
    lucas_lehmer_seq(M, Residues),
    Skip is P - 3, drop(Skip, Residues, [R|_]),
    R =:= 0.

% check if a number is prime
%
wheel235(L) :-
   W = [4, 2, 4, 2, 4, 6, 2, 6 | W],
   L = [1, 2, 2 | W].

prime(N) :- N < 2, !, false.
prime(N) :-
   wheel235(W),
   prime(N, 2, W).

prime(N, D, _) :- D*D > N, !.
prime(N, D, _) :- N mod D =:= 0, !, false.
prime(N, D, [A|As]) :- D2 is D + A, prime(N, D2, As).

```

{{Out}}

```txt

?- show(20).
2 3 5 7 13 17 19 31 61 89 107 127 521 607 1279 2203 2281 3217 4253 4423
true.

```



## PureBasic

PureBasic has no large integer support. Calculations are limited to the range of a signed quad integer type.

```PureBasic
Procedure Lucas_Lehmer_Test(p)
  Protected mp.q = (1 << p) - 1, sn.q = 4, i
  For i = 3 To p
    sn = (sn * sn - 2) % mp
  Next
  If sn = 0
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

#upperBound = SizeOf(Quad) * 8 - 1 ;equivalent to significant bits in a signed quad integer
If OpenConsole()
  Define p = 3
  PrintN("M2")
  While p <= #upperBound
    If Lucas_Lehmer_Test(p)
      PrintN("M" + Str(p))
    EndIf
    p + 2
  Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{Out}}

```txt
M2
M3
M5
M7
M13
M17
M19
M31
```



## Python


```python

from sys import stdout
from math import sqrt, log

def is_prime ( p ):
  if p == 2: return True # Lucas-Lehmer test only works on odd primes
  elif p <= 1 or p % 2 == 0: return False
  else:
    for i in range(3, int(sqrt(p))+1, 2 ):
      if p % i == 0: return False
    return True

def is_mersenne_prime ( p ):
  if p == 2:
    return True
  else:
    m_p = ( 1 << p ) - 1
    s = 4
    for i in range(3, p+1):
      s = (s ** 2 - 2) % m_p
    return s == 0

precision = 20000   # maximum requested number of decimal places of 2 ** MP-1 #
long_bits_width = precision * log(10, 2)
upb_prime = int( long_bits_width - 1 ) / 2    # no unsigned #
upb_count = 45      # find 45 mprimes if int was given enough bits #

print (" Finding Mersenne primes in M[2..%d]:"%upb_prime)

count=0
for p in range(2, int(upb_prime+1)):
  if is_prime(p) and is_mersenne_prime(p):
    print("M%d"%p),
    stdout.flush()
    count += 1
  if count >= upb_count: break
print

```


{{Out}}

```txt
 Finding Mersenne primes in M[2..33218]:
 M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213 M19937 M21701 M23209
```



###  Faster loop without division


```python

def isqrt(n):
    if n < 0:
        raise ValueError
    elif n < 2:
        return n
    else:
        a = 1 << ((1 + n.bit_length()) >> 1)
        while True:
            b = (a + n // a) >> 1
            if b >= a:
                return a
            a = b

def isprime(n):
    if n < 5:
        return n == 2 or n == 3
    elif n%2 == 0:
        return False
    else:
        r = isqrt(n)
        k = 3
        while k <= r:
            if n%k == 0:
                return False
            k += 2
        return True

def lucas_lehmer_fast(n):
    if n == 2:
        return True
    elif not isprime(n):
        return False
    else:
        m = 2**n - 1
        s = 4
        for i in range(2, n):
            sqr = s*s
            s = (sqr & m) + (sqr >> n)
            if s >= m:
                s -= m
            s -= 2
        return s == 0

# test taken from the previous rosetta implementation

from math import log
from sys import stdout

precision = 20000   # maximum requested number of decimal places of 2 ** MP-1 #
long_bits_width = precision * log(10, 2)
upb_prime = int( long_bits_width - 1 ) / 2    # no unsigned #
# upb_count = 45      # find 45 mprimes if int was given enough bits #
upb_count = 15      # find 45 mprimes if int was given enough bits #

print (" Finding Mersenne primes in M[2..%d]:"%upb_prime)

count=0
# for p in range(2, upb_prime+1):
for p in range(2, int(upb_prime+1)):
  if lucas_lehmer_fast(p):
    print("M%d"%p),
    stdout.flush()
    count += 1
  if count >= upb_count: break
print

```


The main loop may be run much faster using [https://pypi.python.org/pypi/gmpy2 gmpy2] :


```python
import gmpy2 as mp

def lucas_lehmer(n):
    if n == 2:
        return True
    if not mp.is_prime(n):
        return False
    two = mp.mpz(2)
    m = two**n - 1
    s = two*two
    for i in range(2, n):
        sqr = s*s
        s = (sqr & m) + (sqr >> n)
        if s >= m:
            s -= m
        s -= two
    return mp.is_zero(s)
```


With this, one can test all primes below 10^5 in around 24 hours on a Core i5 processor, with only one running thread.

The primes found are

2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279, 2203, 2281, 3217, 4253, 4423, 9689, 9941, 11213, 19937, 21701, 23209, 44497, 86243

Of course, they agree with [http://oeis.org/A000043 OEIS A000043].


## R


```r

# vectorized approach based on scalar code from primeSieve and mersenne in CRAN package `numbers`
require(gmp)
n <- 4423  # note that the sieve below assumes n > 9

# sieve the set of primes up to n
p <- seq(1, n, by = 2)
q <- length(p)
p[1] <- 2
for (k in seq(3, sqrt(n), by = 2))
  if (p[(k + 1)/2] != 0)
    p[seq((k * k + 1)/2, q, by = k)] <- 0
p <- p[p > 0]
cat(p[1]," special case M2 == 3\n")
p <- p[-1]

z2 <- gmp::as.bigz(2)
z4 <- z2 * z2
zp <- gmp::as.bigz(p)
zmp <- z2^zp - 1
S <- rep(z4, length(p))

for (i in 1:(p[length(p)] - 2)){
  S <- gmp::mod.bigz(S * S - z2, zmp)
  if( i+2 == p[1] ){
    if( S[1] == 0 ){
      cat( p[1], "\n")
      flush.console()
    }
    p <-  p[-1]
    zmp <- zmp[-1]
    S <-  S[-1]
  }
}

```



## Racket


```racket

#lang racket
(require math)

(define (mersenne-prime? p)
  (divides? (- (expt 2 p) 1) (S (- p 1))))

(define (S n)
  (if (= n 1) 4 (- (sqr (S (- n 1))) 2)))

(define (loop p)
  (when (mersenne-prime? p)
    (displayln p))
  (loop (next-prime p)))

(loop 3)

```



## REXX

REXX won't have a problem with the large number of digits involved, but since it's an interpreted language,

such massive number crunching isn't conducive in searching for large primes.

```rexx
/*REXX pgm uses the Lucas─Lehmer primality test for prime powers of 2  (Mersenne primes)*/
@.=0; @.2=1; @.3=1; @.5=1; @.7=1; @.11=1; @.13=1 /*a partial list of some low primes.   */
!.=@.;  !.0=1; !.2=1; !.4=1; !.5=1; !.6=1; !.8=1 /*#'s with these last digs aren't prime*/
parse arg limit .                                /*obtain optional arguments from the CL*/
if limit==''  then limit= 200                    /*Not specified?  Then use the default.*/
say center('Mersenne prime index list',70-3,"═") /*show a fancy─dancy header (or title).*/
say  right('M'2, 25)      " [1 decimal digit]"   /*left─justify them to align&look nice.*/
                                                 /* [►] note that J==1 is a special case*/
         do j=1  by 2  to limit                  /*there're only so many hours in a day.*/
         power= j + (j==1)                       /*POWER ≡ J    except   for when  J=1. */
         if \isPrime(power)  then iterate        /*if POWER isn't prime, then ignore it.*/
         $= LL2(power)                           /*perform the Lucas─Lehmer 2 (LL2) test*/
         if $==''            then iterate        /*Did it flunk LL2?   Then skip this #.*/
         say  right($, 25)   MPsize              /*left─justify them to align&look nice.*/
         end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure expose !. @.                  /*allow 2 stemmed arrays to be accessed*/
         parse arg  x    ''  -1  z               /*obtain variable   X   and last digit.*/
         if @.x      then return 1               /*is  X  already found to be a prime?  */
         if !.z      then return 0               /*is last decimal digit even or a five?*/
         if x//3==0  then return 0               /*divisible by three?  Then not a prime*/
         if x//7==0  then return 0               /*divisible by seven?    "   "  "   "  */
                do j=11  by 6   until j*j > x    /*ensures that J isn't divisible by 3. */
                if x //  j   ==0  then return 0  /*Is X divisible by  J   ?             */
                if x // (j+2)==0  then return 0  /* " "     "      "  J+2 ?         ___ */
                end   /*j*/                      /* [↑]  perform  DO  loop through √ x  */
         @.x=1;                         return 1 /*indicate number  X  is a prime.      */
/*──────────────────────────────────────────────────────────────────────────────────────*/
LL2: procedure expose MPsize;    parse arg ?     /*Lucas─Lehmer test on    2**?  -  1   */
     if ?==2  then s=0                           /*handle special case for an even prime*/
              else s=4                           /* [↓]  same as NUMERIC FORM SCIENTIFIC*/
     numeric form;               q= 2**?         /*ensure correct form for REXX numbers.*/
           /*╔═══════════════════════════════════════════════════════════════════════════╗
           ╔═╝ Compute a power of 2 using only 9 decimal digits.  One million digits     ║
           ║ could be used, but that really slows up computations.  So, we start with the║
           ║ default of 9 digits, and then find the ten's exponent in the product (2**?),║
           ║ double it,  and then add 6.    {2  is all that's needed,  but  6  is a lot  ║
           ║ safer.}   The doubling is for the squaring of   S    (below, for  s*s).   ╔═╝
           ╚═══════════════════════════════════════════════════════════════════════════╝*/
     if pos('E', q)\==0  then do                 /*is number in exponential notation ?  */
                                  parse var q 'E' tenPow            /*get the exponent. */
                                  numeric digits  tenPow * 2 + 6    /*expand precision. */
                              end                                   /*REXX used dec FP. */
                         else numeric digits    digits() * 2 + 6    /*use 9*2 + 6 digits*/
     q=2**? - 1                                  /*compute a power of two,  minus one.  */
        r= q // 8                                /*obtain   Q   modulus  eight.         */
     if r==1 | r==7  then nop                    /*before crunching, do a simple test.  */
                     else return ''              /*modulus   Q   isn't one  or  seven.  */
                 do ?-2;       s= (s*s -2) // q  /*lather,  rinse,  repeat   ···        */
                 end                             /* [↑]   compute and test for a  MP.   */
     if s\==0  then return ''                    /*Not a Mersenne prime?  Return a null.*/
     sz= length(q)                               /*obtain number of decimal digs in MP. */
     MPsize=' ['sz      "decimal digit"s(sz)']'  /*define a literal to display after MP.*/
                    return 'M'?                  /*return "modified" # (Mersenne index).*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:   if arg(1)==1  then return arg(3);  return word(arg(2) 's', 1)   /*simple pluralizer*/
```

{{out|output|text=  when the following is used for input:     <tt> 10000 </tt>}}

```txt

═════════════════════Mersenne prime index list═════════════════════
                       M2  [1 decimal digit]
                       M3  [1 decimal digit]
                       M5  [2 decimal digits]
                       M7  [3 decimal digits]
                      M13  [4 decimal digits]
                      M17  [6 decimal digits]
                      M19  [6 decimal digits]
                      M31  [10 decimal digits]
                      M61  [19 decimal digits]
                      M89  [27 decimal digits]
                     M107  [33 decimal digits]
                     M127  [39 decimal digits]
                     M521  [157 decimal digits]
                     M607  [183 decimal digits]
                    M1279  [386 decimal digits]
                    M2203  [664 decimal digits]
                    M2281  [687 decimal digits]
                    M3217  [969 decimal digits]
                    M4253  [1281 decimal digits]
                    M4423  [1332 decimal digits]
                    M9689  [2917 decimal digits]
                    M9941  [2993 decimal digits]

```



## Ring


```ring

see "Mersenne Primes :" + nl
for p = 2 to 18
    if lucasLehmer(p) see "M"  + p + nl ok
next

func lucasLehmer p
     i = 0 mp = 0 sn = 0
     if p = 2 return true ok
     if (p and 1) = 0 return false ok
     mp = pow(2,p) - 1
     sn = 4
     for i = 3 to p
         sn = pow(sn,2) - 2
         sn -= (mp * floor(sn / mp))
     next
     return (sn=0)

```



## RPL


```RPL

%%HP: T(3)A(R)F(.);                                                          ; ASCII transfer header
\<< DUP LN DUP \pi * 4 SWAP / 1 + UNROT / * IP 2 { 2 } ROT 2 SWAP            ; input n; n := Int(n/ln(n)*(1 + 4/(pi*ln(n)))), p:=2; (n ~ number of primes less then n, pi used here only as a convenience),  2 is assumed to be the 1st elemente in the list
  START SWAP NEXTPRIME DUP UNROT DUP 2 SWAP ^ 1 - 4 PICK3 2 - 1 SWAP         ; for i := 2 to n,  p := nextprime;  s := 4; m := 2^p - 1;
    START SQ 2 - OVER MOD                                                    ;   for j := 1 to p - 2;  s := s^2 mod m;
    NEXT NIP NOT { + } { DROP } IFTE                                         ;   next j;  if s = 0 then add p to the list else discard p;
  NEXT NIP                                                                   ; next i;
\>>

```

{{out}}

```txt
Outputs for arguments 130, 607 and 2281, respectively:

{ 2 3 5 7 13 17 19 31 61 89 107 127 }
{ 2 3 5 7 13 17 19 31 61 89 107 127 521 607 }
{ 2 3 5 7 13 17 19 31 61 89 107 127 521 607 1279 2203 2281 }

These take respectively 1m 22s on the real HP 50g, 4m 29s and 10h 29m 23s on the emulator (Debug4 running on PC under WinXP, Intel(R) Core(TM) Duo CPU T2350 @ 1.86GHz).

```



## Ruby


```ruby
def is_prime ( p )
  return true  if p == 2
  return false if p <= 1 || p.even?
  (3 .. Math.sqrt(p)).step(2) do |i|
    return false  if p % i == 0
  end
  true
end

def is_mersenne_prime ( p )
  return true  if p == 2
  m_p = ( 1 << p ) - 1
  s = 4
  (p-2).times { s = (s ** 2 - 2) % m_p }
  s == 0
end

precision = 20000   # maximum requested number of decimal places of 2 ** MP-1 #
long_bits_width = precision / Math.log(2) * Math.log(10)
upb_prime = (long_bits_width - 1).to_i / 2    # no unsigned #
upb_count = 45      # find 45 mprimes if int was given enough bits #

puts " Finding Mersenne primes in M[2..%d]:" % upb_prime

count = 0
for p in 2..upb_prime
  if is_prime(p) && is_mersenne_prime(p)
    print "M%d " % p
    count += 1
  end
  break  if count >= upb_count
end
puts
```


{{out}}

```txt
 Finding Mersenne primes in M[2..33218]:
 M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217 M4253 M4423 M9689 M9941 M11213 M19937 M21701 M23209
```



## Rust


```rust


extern crate rug;
extern crate primal;

use rug::Integer;
use rug::ops::Pow;
use std::thread::spawn;

fn is_mersenne (p : usize) {
    let p = p as u32;
    let mut m = Integer::from(1);
    m = m << p;
    m = Integer::from(&m - 1);
    let mut flag1 = false;
    for k in 1..10_000 {
        let mut flag2 = false;
        let mut div : u32 = 2*k*p + 1;
        if &div >= &m {break; }
        for j in [3,5,7,11,13,17,19,23,29,31,37].iter() {
            if div % j == 0 {
                flag2 = true;
                break;
            }
        }
        if flag2 == true {continue;}
        if div % 8 != 1 && div % 8 != 7 { continue; }
        if m.is_divisible_u(div) {
            flag1 = true;
            break;
        }
    }
    if flag1 == true {return ()}
    let mut s = Integer::from(4);
    let two = Integer::from(2);
    for _i in 2..p {
		let mut sqr = s.pow(2);
		s = Integer::from(&Integer::from(&sqr & &m) + &Integer::from(&sqr >> p));
		if &s >= &m {s = s - &m}
		s = Integer::from(&s - &two);
    }
	if s == 0 {println!("Mersenne : {}",p);}
}

fn main () {
    println!("Mersenne : 2");
    let limit = 11_214;
    let mut thread_handles = vec![];
    for p in primal::Primes::all().take_while(|p| *p < limit) {
        thread_handles.push(spawn(move || is_mersenne(p)));
    }
    for handle in thread_handles {
        handle.join().unwrap();
    }
}

```

with Intel(R) Core(TM) i7-5500U CPU @ 2.40GHz :
Less than 8,6 seconds to get the Mersenne primes up to 11213
{{out}}

```txt

Mersenne : 2
Mersenne : 5
Mersenne : 3
Mersenne : 7
Mersenne : 13
Mersenne : 17
Mersenne : 19
Mersenne : 31
Mersenne : 61
Mersenne : 89
Mersenne : 127
Mersenne : 107
Mersenne : 521
Mersenne : 607
Mersenne : 1279
Mersenne : 2281
Mersenne : 2203
Mersenne : 3217
Mersenne : 4423
Mersenne : 4253
Mersenne : 9689
Mersenne : 9941
Mersenne : 11213

real	0m8.581s
user	0m33.894s
sys	0m0.107s

```



## Scala

{{libheader|Scala}}
In accordance with definition of Mersenne primes it could only be a Mersenne number with prime exponent.

```Scala
object LLT extends App {
  import Stream._

  def primeSieve(s: Stream[Int]): Stream[Int] =
    s.head #:: primeSieve(s.tail filter { _ % s.head != 0 })
  val primes = primeSieve(from(2))

  def mersenne(p: Int): BigInt = (BigInt(2) pow p) - 1

  def s(mp: BigInt, p: Int): BigInt = { if (p == 1) 4 else ((s(mp, p - 1) pow 2) - 2) % mp }

  val upbPrime = 9941
  println(s"Finding Mersenne primes in M[2..$upbPrime]")
  ((primes takeWhile (_ <= upbPrime)).par map { p => (p, mersenne(p)) }
    map { p => if (p._1 == 2) (p, 0) else (p, s(p._2, p._1 - 1)) } filter { _._2 == 0 })
    .foreach { p =>
      println(s"prime M${(p._1)._1}: " +
        { if ((p._1)._1 < 200) (p._1)._2 else s"(${(p._1)._2.toString.size} digits)" })
    }
  println("That's All Folks!")
}
```

{{out}} After approx 20 minutes (2.10 GHz dual core)
<pre style="height: 30ex; overflow: scroll">Finding Mersenne primes in M[2..9999]
prime M2: 3
prime M3: 7
prime M5: 31
prime M7: 127
prime M13: 8191
prime M17: 131071
prime M19: 524287
prime M31: 2147483647
prime M61: 2305843009213693951
prime M89: 618970019642690137449562111
prime M107: 162259276829213363391578010288127
prime M127: 170141183460469231731687303715884105727
prime M521: (157 digits)
prime M607: (183 digits)
prime M1279: (386 digits)
prime M2203: (664 digits)
prime M2281: (687 digits)
prime M3217: (969 digits)
prime M4253: (1281 digits)
prime M4423: (1332 digits)
prime M9689: (2917 digits)
prime M9941: (2993 digits)
That's All Folks!
```



## Scheme


```scheme
;;;The heart of the algorithm
(define (S n)
  (let ((m (- (expt 2 n) 1)))
    (let loop ((c (- n 2)) (a 4))
      (if (zero? c)
          a
          (loop (- c 1) (remainder (- (* a a) 2) m))))))

(define (mersenne-prime? n)
  (if (= n 2)
    #t
    (zero? (S n))))

;;;Trivial unoptimized implementation for the base primes
(define (next-prime x)
  (if (prime? (+ x 1))
      (+ x 1)
      (next-prime (+ x 1))))

(define (prime? x)
  (let loop ((c 2))
    (cond ((>= c x) #t)
          ((zero? (remainder x c)) #f)
          (else (loop (+ c 1))))))

;;Main loop
(let loop ((i 45) (p 2))
  (if (not (zero? i))
      (if (mersenne-prime? p)
          (begin
            (display "M") (display p) (display " ")
            (loop (- i 1) (next-prime p)))
          (loop i (next-prime p)))))
```


 M2 M3 M5 M7 M13...


## Scilab

<lang>  iexpmax=30
  n=1
  for iexp=2:iexpmax
      if iexp==2 then s=0; else s=4; end
      n=(n+1)*2-1
      for i=1:iexp-2
          s=modulo((s*s-2),n)
      end
      if s==0 then printf("M%d ",iexp); end
  end
```

{{out}}

```txt
M2 M3 M5 M7 M13 M17 M19
```



## Seed7

To get maximum speed the program should be [http://seed7.sourceforge.net/scrshots/comp.htm compiled] with -O2.


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func boolean: isPrime (in integer: number) is func
  result
    var boolean: prime is FALSE;
  local
    var integer: upTo is 0;
    var integer: testNum is 3;
  begin
    if number = 2 then
      prime := TRUE;
    elsif number rem 2 = 0 or number <= 1 then
      prime := FALSE;
    else
      upTo := sqrt(number);
      while number rem testNum <> 0 and testNum <= upTo do
        testNum +:= 2;
      end while;
      prime := testNum > upTo;
    end if;
  end func;

const func boolean: lucasLehmerTest (in integer: p) is func
  result
    var boolean: prime is TRUE;
  local
    var bigInteger: m_p is 0_;
    var bigInteger: s is 4_;
    var integer: i is 0;
  begin
    if p <> 2 then
      m_p := 2_ ** p - 1_;
      for i range 2 to pred(p) do
        s := (s ** 2 - 2_) rem m_p;
      end for;
      prime := s = 0_;
    end if;
  end func;

const proc: main is func
  local
    var integer: p is 2;
  begin
    writeln(" Mersenne primes:");
    while p <= 3217 do
      if isPrime(p) and lucasLehmerTest(p) then
        write(" M" <& p);
        flush(OUT);
      end if;
      incr(p);
    end while;
    writeln;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#lucasLehmerTest lucasLehmerTest],
[http://seed7.sourceforge.net/algorith/math.htm#isPrime isPrime]

{{Out}}

```txt

 Mersenne primes:
 M2 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203 M2281 M3217

```



## Sidef

{{trans|Perl 6}}

```ruby
func is_mersenne_prime(p) {
    return true if (p == 2)
    var s = 4
    var M = (2**p - 1)
    { s = powmod(s, 2, M)-2 } * (p-2)
    s == 0
}

Inf.times {|n|
    if (n.is_prime && is_mersenne_prime(n)) {
        say "M#{n}"
    }
}
```

{{out}}

```txt

M2
M3
M5
M7
M13
M17
M19
M31
M61
M89
M107
M127
M521
M607
M1279
M2203
M2281
^C

```



## Swift


{{libheader|AttaSwift BigInt}}

Uses a sieve of Eratosthenes.


```swift
func lucasLehmer(_ p: Int) -> Bool {
  let m = BigInt(2).power(p) - 1
  var s = BigInt(4)

  for _ in 0..<p-2 {
    s = ((s * s) - 2) % m
  }

  return s == 0
}

for prime in Eratosthenes(upTo: 70) where lucasLehmer(prime) {
  let m = Int(pow(2, Double(prime))) - 1

  print("2^\(prime) - 1 = \(m) is prime")
}
```


{{out}}

```txt
2^3 - 1 = 7 is prime
2^5 - 1 = 31 is prime
2^7 - 1 = 127 is prime
2^13 - 1 = 8191 is prime
2^17 - 1 = 131071 is prime
2^19 - 1 = 524287 is prime
2^31 - 1 = 2147483647 is prime
2^61 - 1 = 2305843009213693951 is prime
```



## Tcl

{{trans|Pop11}}

```Tcl
proc main argv {
    set n 0
    set t [clock seconds]
    show_mersenne 2 [incr n] t

    for {set p 3} {$p <= [lindex $argv 0]} {incr p 2} {
        if {![prime $p]} continue
        if {[LucasLehmer $p]} {
            show_mersenne $p [incr n] t
        }
    }
}
proc show_mersenne {p n timevar} {
    upvar 1 $timevar time
    set now [clock seconds]
    puts [format "%2d: %5ds  M%s" $n [expr {$now - $time}] $p]
    set time $now
}
proc prime i {
   if {$i in {2 3}} {return 1}
   prime0 $i [expr {int(sqrt($i))}]
}
proc prime0 {i div} {
    expr {!($i % $div)? 0: $div <= 2? 1: [prime0 $i [incr div -1]]}
}
proc LucasLehmer p {
    set mp [expr {2**$p-1}]
    set s  4
    for {set i 2} {$i < $p} {incr i} {
        set s [expr {($s**2 - 2) % $mp}]
    }
    expr {$s == 0}
}

main 33218
```

{{Out}}
The program was still running, but as the next Mersenne prime is 19937
there will be a long wait until the program finds it.

```txt
 1:     0s  M2
 2:     0s  M3
 3:     0s  M5
 4:     0s  M7
 5:     0s  M13
 6:     0s  M17
 7:     0s  M19
 8:     0s  M31
 9:     0s  M61
10:     0s  M89
11:     0s  M107
12:     0s  M127
13:     1s  M521
14:     0s  M607
15:     4s  M1279
16:    21s  M2203
17:     4s  M2281
18:    69s  M3217
19:   180s  M4253
20:    39s  M4423
21:  5543s  M9689
22:   655s  M9941
23:  3546s  M11213
```


=={{header|TI-83 BASIC}}==

```ti83b
19→M
1→N
For(E,2,M)
If E=2
Then:0→S
Else:4→S
End
(N+1)*2-1→N
For(I,1,E-2)
Reminder(S*S-2,N)→S
End
If S=0
Then:Disp E
End
End
```

{{out}}

```txt
2
3
5
7
13
17
19
```



## uBasic/4tH

{{Trans|VBScript}}
<lang>m = 15
n = 1
For j = 2 To m
    If j = 2 Then
        s = 0
    Else
        s = 4
    EndIf
    n = (n + 1) * 2 - 1
    For i = 1 To j - 2
        s = (s * s - 2) % n
    Next i
    If s = 0 Then Print "M";j
Next
```


## VBScript


```vb
iexpmax = 15
n=1
out=""
For iexp = 2 To iexpmax
	If iexp = 2 Then
		s = 0
	Else
		s = 4
	End If
	n = (n + 1) * 2 - 1
	For i = 1 To iexp - 2
		s = (s * s - 2) Mod n
	Next
	If s = 0 Then
		out=out & "M" & iexp & " "
	End If
Next
Wscript.echo out
```

{{Out}}

```txt
M2 M3 M5 M7 M13
```



## Visual Basic .NET

{{works with|Visual Basic .NET|2011}}

```vbnet
Public Class LucasLehmer
    Private Sub btnGo_Click(sender As Object, e As EventArgs) Handles btnGo.Click
        Const iexpmax = 31
        Dim s, n As Long
        Dim i, iexp As Integer
        n = 1
        txtOut.Text = ""
        For iexp = 2 To iexpmax
            If iexp = 2 Then
                s = 0
            Else
                s = 4
            End If
            n = (n + 1) * 2 - 1
            For i = 1 To iexp - 2
                s = (s * s - 2) Mod n
            Next i
            If s = 0 Then
                txtOut.Text = txtOut.Text & "M" & iexp & " "
            End If
        Next iexp
    End Sub
End Class
```

{{Out}}

```txt

M2 M3 M5 M7 M13 M17 M19 M31
```



## zkl

Using [[Extensible prime generator#zkl]] and the GMP library.

```zkl
var [const] BN=Import.lib("zklBigNum");	// lib GMP
primes:=Utils.Generator(Import("sieve").postponed_sieve);
fcn isMersennePrime(p){
   if(p==2) return(True);
   mp:=BN(1).shiftLeft(p) - 1; // 2^p - 1, a BIG number, like 1000s of digits
   s:=BN(4); do(p-2){ s.mul(s).sub(2).mod(mp) } // the % REALLY cuts down on mem usage
   return(s==0);
}
```

Calculating S(n) is done in place (overwriting the value in the BigInt with the result); this really cuts down on memory usage.
<lang>mersennePrimes:=primes.tweak(fcn(p){ isMersennePrime(p) and p or Void.Skip });
println("Mersenne primes:");
foreach mp in (mersennePrimes) { print(" M",mp); }
```

This will "continuously" spew out Mersenne Primes.

Tweaking a Walker (aka iterator, Generators are a class of Walker) basically puts a filter on the underlying iterator, in this case, ignoring prime numbers that are not Mersenne primes and passing those that are.
{{out}}

```txt

Mersenne primes:
 M2 M3 M5 M7 M13 M17 M19 M31 M61 M89 M107 M127 M521 M607 M1279 M2203
 M2281 M3217 M4253 M4423 M9689 M9941 M11213 M19937 M21701 M23209 M44497 ^C

```

Additionally, this problem is readily threaded and has a linear speedup. Since there are lots of calculations between results, the [bigger] results are basically time sorted. However, N times faster doesn't mean much given the huge calculations used by the LL test (math with thousands of digits ain't quick).

Using five threads:

```zkl
ps,mpOut := Thread.Pipe(),Thread.Pipe(); // how the threads will communicate
fcn(ps){   // a thread to generate primes, sleeps most of the time
   Utils.Generator(Import("sieve").postponed_sieve).pump(ps)
}.launch(ps);

do(4){ // four threads to perform the Lucas-Lehmer test
   fcn(ps,out){ ps.pump(out,isMersennePrime,Void.Filter) }.launch(ps,mpOut)
}
println("Mersenne primes:");
foreach mp in (mpOut) { print(" M",mp); }
```



{{omit from|GUISS}}

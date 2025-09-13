+++
title = "Pythagorean triples"
description = ""
date = 2019-10-20T20:46:23Z
aliases = []
[extra]
id = 9980
[taxonomies]
categories = ["task"]
tags = []
+++

A [[wp:Pythagorean_triple|Pythagorean triple]] is defined as three positive integers <math>(a, b, c)</math> where <math>a < b < c</math>, and <math>a^2+b^2=c^2.</math>

They are called primitive triples if <math>a, b, c</math> are co-prime, that is, if their pairwise greatest common divisors <math>{\rm gcd}(a, b) = {\rm gcd}(a, c) = {\rm gcd}(b, c) = 1</math>.

Because of their relationship through the Pythagorean theorem, a, b, and c are co-prime if a and b are co-prime (<math>{\rm gcd}(a, b) = 1</math>).

Each triple forms the length of the sides of a right triangle, whose perimeter is <math>P=a+b+c</math>.


## Task

The task is to determine how many Pythagorean triples there are with a perimeter no larger than 100 and the number of these that are primitive.


;Extra credit:
Deal with large values.   Can your program handle a maximum perimeter of 1,000,000?   What about 10,000,000?   100,000,000?

Note: the extra credit is not for you to demonstrate how fast your language is compared to others;   you need a proper algorithm to solve them in a timely manner.


## Related tasks

*   [[Euler's sum of powers conjecture]]
*   [[List comprehensions]]
*   [[Pythagorean quadruples]]





## 360 Assembly

```360asm
*        Pythagorean triples -     12/06/2018
PYTHTRI  CSECT
         USING  PYTHTRI,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         MVC    PMAX,=F'1'         pmax=1
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=F'6')    do i=1 to 6
         L      R5,PMAX              pmax
         MH     R5,=H'10'            *10
         ST     R5,PMAX              pmax=pmax*10
         MVC    PRIM,=F'0'           prim=0
         MVC    COUNT,=F'0'          count=0
         L      R1,PMAX              pmax
         BAL    R14,ISQRT            isqrt(pmax)
         SRA    R0,1                 /2
         ST     R0,NMAX              nmax=isqrt(pmax)/2
         LA     R7,1                 n=1
       DO WHILE=(C,R7,LE,NMAX)       do n=1 to nmax
         LA     R9,1(R7)               m=n+1
         LR     R5,R9                  m
         AR     R5,R7                  +n
         MR     R4,R9                  *m
         SLA    R5,1                   *2
         LR     R8,R5                  p=2*m*(m+n)
       DO WHILE=(C,R8,LE,PMAX)         do while p<=pmax
         LR     R1,R9                    m
         LR     R2,R7                    n
         BAL    R14,GCD                  gcd(m,n)
       IF C,R0,EQ,=F'1' THEN             if gcd(m,n)=1 then
         L      R2,PRIM                    prim
         LA     R2,1(R2)                   +1
         ST     R2,PRIM                    prim=prim+1
         L      R4,PMAX                    pmax
         SRDA   R4,32                      ~
         DR     R4,R8                      /p
         A      R5,COUNT                   +count
         ST     R5,COUNT                   count=count+pmax/p
       ENDIF    ,                        endif
         LA     R9,2(R9)                 m=m+2
         LR     R5,R9                    m
         AR     R5,R7                    +n
         MR     R4,R9                    *m
         SLA    R5,1                     *2
         LR     R8,R5                    p=2*m*(m+n)
       ENDDO    ,                      enddo n
         LA     R7,1(R7)               n++
       ENDDO    ,                    enddo n
         L      R1,PMAX              pmax
         XDECO  R1,XDEC              edit pmax
         MVC    PG+15(9),XDEC+3      output pmax
         L      R1,COUNT             count
         XDECO  R1,XDEC              edit count
         MVC    PG+33(9),XDEC+3      output count
         L      R1,PRIM              prim
         XDECO  R1,XDEC              edit prim
         MVC    PG+55(9),XDEC+3      output prim
         XPRNT  PG,L'PG              print
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
NMAX     DS     F                  nmax
PMAX     DS     F                  pmax
COUNT    DS     F                  count
PRIM     DS     F                  prim
PG     DC CL80'Max Perimeter: ........., Total: ........., Primitive:'
XDEC     DS     CL12
GCD      EQU    *  --------------- function gcd(a,b)
         STM    R2,R7,GCDSA        save context
         LR     R3,R1              c=a
         LR     R4,R2              d=b
GCDLOOP  LR     R6,R3              c
         SRDA   R6,32              ~
         DR     R6,R4              /d
         LTR    R6,R6              if c mod d=0
         BZ     GCDELOOP           then leave loop
         LR     R5,R6              e=c mod d
         LR     R3,R4              c=d
         LR     R4,R5              d=e
         B      GCDLOOP            loop
GCDELOOP LR     R0,R4              return(d)
         LM     R2,R7,GCDSA        restore context
         BR     R14                return
GCDSA    DS     6A                 context store
ISQRT    EQU    *  --------------- function isqrt(n)
         STM    R3,R10,ISQRTSA     save context
         LR     R6,R1              n=r1
         LR     R10,R6             sqrtn=n
         SRA    R10,1              sqrtn=n/2
       IF LTR,R10,Z,R10 THEN       if sqrtn=0 then
         LA     R10,1                sqrtn=1
       ELSE     ,                  else
         LA     R9,0                 snm2=0
         LA     R8,0                 snm1=0
         LA     R7,0                 sn=0
         LA     R3,0                 okexit=0
       DO UNTIL=(C,R3,EQ,=A(1))      do until okexit=1
         AR     R10,R7                 sqrtn=sqrtn+sn
         LR     R9,R8                  snm2=snm1
         LR     R8,R7                  snm1=sn
         LR     R4,R6                  n
         SRDA   R4,32                  ~
         DR     R4,R10                 /sqrtn
         SR     R5,R10                 -sqrtn
         SRA    R5,1                   /2
         LR     R7,R5                  sn=(n/sqrtn-sqrtn)/2
       IF C,R7,EQ,=F'0',OR,CR,R7,EQ,R9 THEN  if sn=0 or sn=snm2 then
         LA     R3,1                     okexit=1
       ENDIF    ,                      endif
       ENDDO    ,                    enddo until
       ENDIF    ,                  endif
         LR     R5,R10             sqrtn
         MR     R4,R10             *sqrtn
       IF CR,R5,GT,R6 THEN         if sqrtn*sqrtn>n then
         BCTR   R10,0                sqrtn=sqrtn-1
       ENDIF    ,                  endif
         LR     R0,R10             return(sqrtn)
         LM     R3,R10,ISQRTSA     restore context
         BR     R14                return
ISQRTSA  DS     8A                 context store
         YREGS
         END    PYTHTRI
```

```txt

Max Perimeter:        10, Total:         0, Primitive:         0
Max Perimeter:       100, Total:        17, Primitive:         7
Max Perimeter:      1000, Total:       325, Primitive:        70
Max Perimeter:     10000, Total:      4858, Primitive:       703
Max Perimeter:    100000, Total:     64741, Primitive:      7026
Max Perimeter:   1000000, Total:    808950, Primitive:     70229

```



## Ada


Translation of efficient method from C, see [[wp:Pythagorean_triple#Parent.2Fchild_relationships|the WP article]]. Compiles on gnat/gcc.


```Ada
with Ada.Text_IO;

procedure Pythagorean_Triples is

   type Large_Natural is range 0 .. 2**63-1;
     -- this is the maximum for gnat

   procedure New_Triangle(A, B, C: Large_Natural;
                          Max_Perimeter: Large_Natural;
                          Total_Cnt, Primitive_Cnt: in out Large_Natural) is
      Perimeter: constant Large_Natural := A + B + C;
   begin
      if Perimeter <= Max_Perimeter then
         Primitive_Cnt := Primitive_Cnt + 1;
         Total_Cnt     := Total_Cnt + Max_Perimeter / Perimeter;
         New_Triangle(A-2*B+2*C,     2*A-B+2*C,    2*A-2*B+3*C,   Max_Perimeter, Total_Cnt, Primitive_Cnt);
         New_Triangle(A+2*B+2*C,     2*A+B+2*C,    2*A+2*B+3*C,   Max_Perimeter, Total_Cnt, Primitive_Cnt);
         New_Triangle(2*B+2*C-A,     B+2*C-2*A,    2*B+3*C-2*A,   Max_Perimeter, Total_Cnt, Primitive_Cnt);
      end if;
   end New_Triangle;

   T_Cnt, P_Cnt: Large_Natural;

begin
   for I in 1 .. 9 loop
      T_Cnt := 0;
      P_Cnt := 0;
      New_Triangle(3,4,5, 10**I, Total_Cnt => T_Cnt, Primitive_Cnt => P_Cnt);
      Ada.Text_IO.Put_Line("Up to 10 **" & Integer'Image(I) & " :" &
                             Large_Natural'Image(T_Cnt) & " Triples," &
                             Large_Natural'Image(P_Cnt) & " Primitives");
   end loop;
end Pythagorean_Triples;
```


Output:


```txt
Up to 10 ** 1 : 0 Triples, 0 Primitives
Up to 10 ** 2 : 17 Triples, 7 Primitives
Up to 10 ** 3 : 325 Triples, 70 Primitives
Up to 10 ** 4 : 4858 Triples, 703 Primitives
Up to 10 ** 5 : 64741 Triples, 7026 Primitives
Up to 10 ** 6 : 808950 Triples, 70229 Primitives
Up to 10 ** 7 : 9706567 Triples, 702309 Primitives
Up to 10 ** 8 : 113236940 Triples, 7023027 Primitives
Up to 10 ** 9 : 1294080089 Triples, 70230484 Primitives
```



## ANSI Standard BASIC


Translation of BBC BASIC Program


```ANSI Standard BASIC
100 DECLARE EXTERNAL SUB tri
110 !
120 PUBLIC NUMERIC U0(3,3), U1(3,3), U2(3,3), all, prim
130 DIM seed(3)
140 MAT READ U0, U1, U2
150 DATA 1, -2, 2, 2, -1, 2, 2, -2, 3
160 DATA 1, 2, 2, 2, 1, 2, 2, 2, 3
170 DATA -1, 2, 2, -2, 1, 2, -2, 2, 3
180 !
190 MAT READ seed
200 DATA 3, 4, 5
210 FOR power  = 1 TO 7
220    LET all  = 0
230    LET prim  = 0
240    CALL tri(seed, 10^power , all , prim)
250    PRINT "Up to 10^";power,
260    PRINT USING "######### triples ######### primitives":all,prim
270 NEXT power
280 END
290 !
300 EXTERNAL SUB tri(i(), mp, all, prim)
310 DECLARE EXTERNAL FUNCTION SUM
320 DECLARE NUMERIC t(3)
330 !
340 IF SUM(i) > mp THEN EXIT SUB
350 LET prim = prim + 1
360 LET all  = all + INT(mp  / SUM(i))
370 !
380 MAT t = U0 * i
390 CALL tri(t, mp , all , prim)
400 MAT t = U1 * i
410 CALL tri(t, mp , all , prim)
420 MAT t = U2 * i
430 CALL tri(t, mp , all , prim)
440 END SUB
450 !
460 EXTERNAL FUNCTION SUM(a())
470 LET temp = 0
480 FOR i=LBOUND(a) TO UBOUND(a)
490    LET temp = temp + a(i)
500 NEXT i
510 LET SUM = temp
520 END FUNCTION
```



## AutoHotkey


```autohotkey
#NoEnv
SetBatchLines, -1
#SingleInstance, Force

; Greatest common divisor, from http://rosettacode.org/wiki/Greatest_common_divisor#AutoHotkey
gcd(a,b) {
	Return b=0 ? Abs(a) : Gcd(b,mod(a,b))
}

count_triples(max) {
	primitives := 0, triples := 0, m := 2
	while m <= (max / 2)**0.5
	{
		n := mod(m, 2) + 1
		,p := 2*m*(m + n)
		, delta := 4*m
		while n < m and p <= max
			gcd(m, n) = 1
				? (primitives++
				, triples += max // p)
				: ""
			, n += 2
			, p += delta
		m++
	}
	Return primitives " primitives out of " triples " triples"
}

Loop, 8
	Msgbox % 10**A_Index ": " count_triples(10**A_Index)
```


```txt
10: 0 primitives out of 0 triples
100: 7 primitives out of 17 triples
1000: 70 primitives out of 325 triples
10000: 703 primitives out of 4858 triples
100000: 7026 primitives out of 64741 triples
1000000: 70229 primitives out of 808950 triples
10000000: 702309 primitives out of 9706567 triples
100000000: 7023027 primitives out of 113236940 triples
```


## AWK


```AWK

# syntax: GAWK -f PYTHAGOREAN_TRIPLES.AWK
# converted from Go
BEGIN {
    printf("%5s %11s %11s %11s %s\n","limit","limit","triples","primitives","seconds")
    for (max_peri=10; max_peri<=1E9; max_peri*=10) {
      t = systime()
      prim = 0
      total = 0
      new_tri(3,4,5)
      printf("10^%-2d %11d %11d %11d %d\n",++n,max_peri,total,prim,systime()-t)
    }
    exit(0)
}
function new_tri(s0,s1,s2,  p) {
    p = s0 + s1 + s2
    if (p <= max_peri) {
      prim++
      total += int(max_peri / p)
      new_tri(+1*s0-2*s1+2*s2,+2*s0-1*s1+2*s2,+2*s0-2*s1+3*s2)
      new_tri(+1*s0+2*s1+2*s2,+2*s0+1*s1+2*s2,+2*s0+2*s1+3*s2)
      new_tri(-1*s0+2*s1+2*s2,-2*s0+1*s1+2*s2,-2*s0+2*s1+3*s2)
    }
}

```

```txt

limit       limit     triples  primitives seconds
10^1           10           0           0 0
10^2          100          17           7 0
10^3         1000         325          70 0
10^4        10000        4858         703 0
10^5       100000       64741        7026 0
10^6      1000000      808950       70229 0
10^7     10000000     9706567      702309 2
10^8    100000000   113236940     7023027 12
10^9   1000000000  1294080089    70230484 116

```



## BBC BASIC

The built-in array arithmetic is very well suited to this task!

```bbcbasic
      DIM U0%(2,2), U1%(2,2), U2%(2,2), seed%(2)
      U0%() =  1, -2, 2,  2, -1, 2,  2, -2, 3
      U1%() =  1,  2, 2,  2,  1, 2,  2,  2, 3
      U2%() = -1,  2, 2, -2,  1, 2, -2,  2, 3

      seed%() = 3, 4, 5
      FOR power% = 1 TO 7
        all% = 0 : prim% = 0
        PROCtri(seed%(), 10^power%, all%, prim%)
        PRINT "Up to 10^"; power%, ": " all% " triples" prim% " primitives"
      NEXT
      END

      DEF PROCtri(i%(), mp%, RETURN all%, RETURN prim%)
      LOCAL t%() : DIM t%(2)

      IF SUM(i%()) > mp% ENDPROC
      prim% += 1
      all% += mp% DIV SUM(i%())

      t%() = U0%() . i%()
      PROCtri(t%(), mp%, all%, prim%)
      t%() = U1%() . i%()
      PROCtri(t%(), mp%, all%, prim%)
      t%() = U2%() . i%()
      PROCtri(t%(), mp%, all%, prim%)
      ENDPROC
```

'''Output:'''

```txt

Up to 10^1:          0 triples         0 primitives
Up to 10^2:         17 triples         7 primitives
Up to 10^3:        325 triples        70 primitives
Up to 10^4:       4858 triples       703 primitives
Up to 10^5:      64741 triples      7026 primitives
Up to 10^6:     808950 triples     70229 primitives
Up to 10^7:    9706567 triples    702309 primitives
Up to 10^8:  113236940 triples   7023027 primitives

```



## Bracmat

```bracmat
(pythagoreanTriples=
  total prim max-peri U
.       (.(1,-2,2) (2,-1,2) (2,-2,3))
        (.(1,2,2) (2,1,2) (2,2,3))
        (.(-1,2,2) (-2,1,2) (-2,2,3))
    : ?U
  & ( new-tri
    =     i t p Urows Urow Ucols
        , a b c loop A B C
      .     !arg:(,?a,?b,?c)
          & !a+!b+!c:~>!max-peri:?p
          & 1+!prim:?prim
          & div$(!max-peri.!p)+!total:?total
          & !U:?Urows
          & ( loop
            =   !Urows:(.?Urow) ?Urows
              & !Urow:?Ucols
              & :?t
              &   whl
                ' ( !Ucols:(?A,?B,?C) ?Ucols
                  & (!t,!a*!A+!b*!B+!c*!C):?t
                  )
              & new-tri$!t
              & !loop
            )
          & !loop
        |
    )
  & ( Main
    =   seed
      .   (,3,4,5):?seed
        & 10:?max-peri
        &   whl
          ' ( 0:?total:?prim
            & new-tri$!seed
            &   out
              $ ( str
                $ ( "Up to "
                    !max-peri
                    ": "
                    !total
                    " triples, "
                    !prim
                    " primitives."
                  )
                )
            & !max-peri*10:~>10000000:?max-peri
            )
    )
  & Main$
);

pythagoreanTriples$;

```


Output (under Linux):

```txt
Up to 10: 0 triples, 0 primitives.
Up to 100: 17 triples, 7 primitives.
Up to 1000: 325 triples, 70 primitives.
Up to 10000: 4858 triples, 703 primitives.
Up to 100000: 64741 triples, 7026 primitives.
Up to 1000000: 808950 triples, 70229 primitives.
Up to 10000000: 9706567 triples, 702309 primitives.
Up to 100000000: 113236940 triples, 7023027 primitives.
```


Under Windows XP Command prompt the last result is unattainable due to stack overflow.
With very few changes we can get rid of the stack exhausting recursion. Instead of calling <code>new-tri</code> recursively, be push the triples to test onto a stack and return to the <code>Main</code> function. In the innermost loop we pop a triple from the stack and call <code>new-tri</code>. The memory overhead is only a few megabytes for a max perimeter of 100,000,000. On my Windows XP box the whole computation takes at least 15 minutes! Given enough time (and memory), the program can compute results for larger perimeters.


```bracmat
(pythagoreanTriples=
  total prim max-peri U stack
.       (.(1,-2,2) (2,-1,2) (2,-2,3))
        (.(1,2,2) (2,1,2) (2,2,3))
        (.(-1,2,2) (-2,1,2) (-2,2,3))
    : ?U
  & ( new-tri
    =     i t p Urows Urow Ucols Ucol
        , a b c loop A B C
      .     !arg:(,?a,?b,?c)
          & !a+!b+!c:~>!max-peri:?p
          & 1+!prim:?prim
          & div$(!max-peri.!p)+!total:?total
          & !U:?Urows
          & ( loop
            =   !Urows:(.?Urow) ?Urows
              & !Urow:?Ucols
              & :?t
              &   whl
                ' ( !Ucols:(?A,?B,?C) ?Ucols
                  & (!t,!a*!A+!b*!B+!c*!C):?t
                  )
              & !t !stack:?stack
              & !loop
            )
          & !loop
        |
    )
  & ( Main
    =   seed
      .   10:?max-peri
        &   whl
          ' ( 0:?total:?prim
            & (,3,4,5):?stack
            &   whl
              ' (!stack:%?seed ?stack&new-tri$!seed)
            &   out
              $ ( str
                $ ( "Up to "
                    !max-peri
                    ": "
                    !total
                    " triples, "
                    !prim
                    " primitives."
                  )
                )
            & !max-peri*10:~>100000000:?max-peri
            )
    )
  & Main$
);

pythagoreanTriples$;
```



## C


Sample implemention; naive method, patentedly won't scale to larger numbers, despite the attempt to optimize it.  Calculating up to 10000 is already a test of patience.

```c
#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long xint;
typedef unsigned long ulong;

inline ulong gcd(ulong m, ulong n)
{
    ulong t;
    while (n) { t = n; n = m % n; m = t; }
    return m;
}

int main()
{
    ulong a, b, c, pytha = 0, prim = 0, max_p = 100;
    xint aa, bb, cc;

    for (a = 1; a <= max_p / 3; a++) {
        aa = (xint)a * a;
        printf("a = %lu\r", a); /* show that we are working */
        fflush(stdout);

        /*  max_p/2: valid limit, because one side of triangle
         *  must be less than the sum of the other two
         */
        for (b = a + 1; b < max_p/2; b++) {
            bb = (xint)b * b;
            for (c = b + 1; c < max_p/2; c++) {
                cc = (xint)c * c;
                if (aa + bb < cc) break;
                if (a + b + c > max_p) break;

                if (aa + bb == cc) {
                    pytha++;
                    if (gcd(a, b) == 1) prim++;
                }
            }
        }
    }

    printf("Up to %lu, there are %lu triples, of which %lu are primitive\n",
        max_p, pytha, prim);

    return 0;
}
```
output:<lang>Up to 100, there are 17 triples, of which 7 are primitive
```

Efficient method, generating primitive triples only as described in [[wp:Pythagorean_triple#Parent.2Fchild_relationships|the same WP article]]:
```c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/* should be 64-bit integers if going over 1 billion */
typedef unsigned long xint;
#define FMT "%lu"

xint total, prim, max_peri;
xint U[][9] =  {{ 1, -2, 2,  2, -1, 2,  2, -2, 3},
        { 1,  2, 2,  2,  1, 2,  2,  2, 3},
        {-1,  2, 2, -2,  1, 2, -2,  2, 3}};

void new_tri(xint in[])
{
    int i;
    xint t[3], p = in[0] + in[1] + in[2];

    if (p > max_peri) return;

    prim ++;

    /* for every primitive triangle, its multiples would be right-angled too;
     * count them up to the max perimeter */
    total += max_peri / p;

    /* recursively produce next tier by multiplying the matrices */
    for (i = 0; i < 3; i++) {
        t[0] = U[i][0] * in[0] + U[i][1] * in[1] + U[i][2] * in[2];
        t[1] = U[i][3] * in[0] + U[i][4] * in[1] + U[i][5] * in[2];
        t[2] = U[i][6] * in[0] + U[i][7] * in[1] + U[i][8] * in[2];
        new_tri(t);
    }
}

int main()
{
    xint seed[3] = {3, 4, 5};

    for (max_peri = 10; max_peri <= 100000000; max_peri *= 10) {
        total = prim = 0;
        new_tri(seed);

        printf( "Up to "FMT": "FMT" triples, "FMT" primitives.\n",
            max_peri, total, prim);
    }
    return 0;
}
```
Output<lang>Up to 10: 0 triples, 0 primitives.
Up to 100: 17 triples, 7 primitives.
Up to 1000: 325 triples, 70 primitives.
Up to 10000: 4858 triples, 703 primitives.
Up to 100000: 64741 triples, 7026 primitives.
Up to 1000000: 808950 triples, 70229 primitives.
Up to 10000000: 9706567 triples, 702309 primitives.
Up to 100000000: 113236940 triples, 7023027 primitives.
```


Same as above, but with loop unwound and third recursion eliminated:

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/* should be 64-bit integers if going over 1 billion */
typedef unsigned long xint;
#define FMT "%lu"

xint total, prim, max_peri;

void new_tri(xint in[])
{
    int i;
    xint t[3], p;
    xint x = in[0], y = in[1], z = in[2];

recur:  p = x + y + z;
    if (p > max_peri) return;

    prim ++;
    total += max_peri / p;

    t[0] = x - 2 * y + 2 * z;
    t[1] = 2 * x - y + 2 * z;
    t[2] = t[1] - y + z;
    new_tri(t);

    t[0] += 4 * y;
    t[1] += 2 * y;
    t[2] += 4 * y;
    new_tri(t);

    z = t[2] - 4 * x;
    y = t[1] - 4 * x;
    x = t[0] - 2 * x;
    goto recur;
}

int main()
{
    xint seed[3] = {3, 4, 5};

    for (max_peri = 10; max_peri <= 100000000; max_peri *= 10) {
        total = prim = 0;
        new_tri(seed);

        printf( "Up to "FMT": "FMT" triples, "FMT" primitives.\n",
            max_peri, total, prim);
    }
    return 0;
}
```


## C#

Based on Ada example, which is a translation of efficient method from C, see [[wp:Pythagorean_triple#Parent.2Fchild_relationships|the WP article]].


```C sharp
using System;

namespace RosettaCode.CSharp
{
    class Program
    {
        static void Count_New_Triangle(ulong A, ulong B, ulong C, ulong Max_Perimeter, ref ulong Total_Cnt, ref ulong Primitive_Cnt)
        {
            ulong Perimeter = A + B + C;

            if (Perimeter <= Max_Perimeter)
            {
                Primitive_Cnt = Primitive_Cnt + 1;
                Total_Cnt = Total_Cnt + Max_Perimeter / Perimeter;
                Count_New_Triangle(A + 2 * C - 2 * B, 2 * A + 2 * C - B, 2 * A + 3 * C - 2 * B, Max_Perimeter, ref Total_Cnt, ref Primitive_Cnt);
                Count_New_Triangle(A + 2 * B + 2 * C, 2 * A + B + 2 * C, 2 * A + 2 * B + 3 * C, Max_Perimeter, ref Total_Cnt, ref Primitive_Cnt);
                Count_New_Triangle(2 * B + 2 * C - A, B + 2 * C - 2 * A, 2 * B + 3 * C - 2 * A, Max_Perimeter, ref Total_Cnt, ref Primitive_Cnt);
            }
        }

        static void Count_Pythagorean_Triples()
        {
            ulong T_Cnt, P_Cnt;

            for (int I = 1; I <= 8; I++)
            {
                T_Cnt = 0;
                P_Cnt = 0;
                ulong ExponentNumberValue = (ulong)Math.Pow(10, I);
                Count_New_Triangle(3, 4, 5, ExponentNumberValue, ref T_Cnt, ref P_Cnt);
                Console.WriteLine("Perimeter up to 10E" + I + " : " + T_Cnt + " Triples, " + P_Cnt + " Primitives");
            }
        }

        static void Main(string[] args)
        {
            Count_Pythagorean_Triples();
        }
    }
}
```


Output:


```txt
Perimeter up to 10E1 : 0 Triples, 0 Primitives
Perimeter up to 10E2 : 17 Triples, 7 Primitives
Perimeter up to 10E3 : 325 Triples, 70 Primitives
Perimeter up to 10E4 : 4858 Triples, 703 Primitives
Perimeter up to 10E5 : 64741 Triples, 7026 Primitives
Perimeter up to 10E6 : 808950 Triples, 70229 Primitives
Perimeter up to 10E7 : 9706567 Triples, 702309 Primitives
Perimeter up to 10E8 : 113236940 Triples, 7023027 Primitives
```



## Clojure

This version is based on Euclid's formula:
for each pair ''(m,n)'' such that ''m>n>0'', ''m'' and ''n'' coprime and of opposite polarity (even/odd),
there is a primitive Pythagorean triple. It can be proven that the converse is true as well.

```clojure
(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn pyth [peri]
  (for [m (range 2 (Math/sqrt (/ peri 2)))
        n (range (inc (mod m 2)) m 2) ; n<m, opposite polarity
        :let [p (* 2 m (+ m n))]      ; = a+b+c for this (m,n)
        :while (<= p peri)
        :when (= 1 (gcd m n))
        :let [m2 (* m m), n2 (* n n),
              [a b] (sort [(- m2 n2) (* 2 m n)]), c (+ m2 n2)]
        k (range 1 (inc (quot peri p)))]
    [(= k 1) (* k a) (* k b) (* k c)]))

(defn rcount [ts] ; (->> peri pyth rcount) produces [total, primitive] counts
  (reduce (fn [[total prims] t] [(inc total), (if (first t) (inc prims) prims)])
    [0 0]
    ts))
```

To handle really large perimeters, we can dispense with actually generating the triples and just calculate the counts:

```clojure
(defn pyth-count [peri]
  (reduce (fn [[total prims] k] [(+ total k), (inc prims)]) [0 0]
    (for [m (range 2 (Math/sqrt (/ peri 2)))
          n (range (inc (mod m 2)) m 2) ; n<m, opposite polarity
          :let [p (* 2 m (+ m n))]      ; = a+b+c for this (m,n)
          :while (<= p peri)
          :when (= 1 (gcd m n))]
      (quot peri p))))
```



## CoffeeScript

This algorithm scales linearly with the max perimeter.  It uses two loops that are capped by the square root of the half-perimeter to examine/count provisional values of m and n, where m and n generate a, b, c, and p using simple number theory.


```coffeescript

gcd = (x, y) ->
  return x if y == 0
  gcd(y, x % y)

# m,n generate primitive Pythag triples
#
# preconditions:
#   m, n are integers of different parity
#   m > n
#   gcd(m,n) == 1 (coprime)
#
# m, n generate: [m*m - n*n, 2*m*n, m*m + n*n]
# perimeter is 2*m*m + 2*m*n = 2 * m * (m+n)
count_triples = (max_perim) ->
  num_primitives = 0
  num_triples = 0
  m = 2
  upper_limit = Math.sqrt max_perim / 2
  while m <= upper_limit
    n = m % 2 + 1
    p = 2*m*m + 2*m*n
    delta = 4*m
    while n < m and p <= max_perim
      if gcd(m, n) == 1
        num_primitives += 1
        num_triples += Math.floor max_perim / p
      n += 2
      p += delta
    m += 1
  console.log num_primitives, num_triples

max_perim = Math.pow 10, 9 # takes under a minute
count_triples(max_perim)

```

output

```txt

time coffee pythag_triples.coffee
70230484 1294080089
real    0m45.989s

```



## Common Lisp


```lisp
(defun mmul (a b)
  (loop for x in a collect
    (loop for y in x
          for z in b sum (* y z))))

(defun count-tri (lim &aux (prim 0) (cnt 0))
  (labels ((count1 (tr &aux (peri (reduce #'+ tr)))
             (when (<= peri lim)
               (incf prim)
               (incf cnt (truncate lim peri))
               (count1 (mmul '(( 1 -2 2) ( 2 -1 2) ( 2 -2 3)) tr))
               (count1 (mmul '(( 1  2 2) ( 2  1 2) ( 2  2 3)) tr))
               (count1 (mmul '((-1  2 2) (-2  1 2) (-2  2 3)) tr)))))
    (count1 '(3 4 5))
    (format t "~a: ~a prim, ~a all~%" lim prim cnt)))

(loop for p from 2 do (count-tri (expt 10 p)))
```
output<lang>100: 7 prim, 17 all
1000: 70 prim, 325 all
10000: 703 prim, 4858 all
100000: 7026 prim, 64741 all
1000000: 70229 prim, 808950 all
10000000: 702309 prim, 9706567 all
...
```



## Crystal

```ruby
class PythagoranTriplesCounter
  def initialize(limit = 0)
    @limit = limit
    @total = 0
    @primitives = 0
    generate_triples(3, 4, 5)
  end

  def total; @total end
  def primitives; @primitives end

  private def generate_triples(a, b, c)
    perim = a + b + c
    return if perim > @limit

    @primitives += 1
    @total += @limit / perim

    generate_triples( a-2*b+2*c, 2*a-b+2*c, 2*a-2*b+3*c )
    generate_triples( a+2*b+2*c, 2*a+b+2*c, 2*a+2*b+3*c )
    generate_triples(-a+2*b+2*c,-2*a+b+2*c,-2*a+2*b+3*c )
  end
end

perim = 10
while perim <= 100_000_000
  c = PythagoranTriplesCounter.new perim
  p [perim, c.total, c.primitives]
  perim *= 10
end
```


output

```txt
[10, 0, 0]
[100, 17, 7]
[1000, 325, 70]
[10000, 4858, 703]
[100000, 64741, 7026]
[1000000, 808950, 70229]
[10000000, 9706567, 702309]
[100000000, 113236940, 7023027]
```



## D


### Lazy Functional Version

With hints from the Haskell solution.

```d
void main() @safe {
    import std.stdio, std.range, std.algorithm, std.typecons, std.numeric;

    enum triples = (in uint n) pure nothrow @safe /*@nogc*/ =>
        iota(1, n + 1)
        .map!(z => iota(1, z + 1)
                   .map!(x => iota(x, z + 1).map!(y => tuple(x, y, z))))
        .joiner.joiner
        .filter!(t => t[0] ^^ 2 + t[1] ^^ 2 == t[2] ^^ 2 && t[].only.sum <= n)
        .map!(t => tuple(t[0 .. 2].gcd == 1, t[]));

    auto xs = triples(100);
    writeln("Up to 100 there are ", xs.count, " triples, ",
            xs.filter!q{ a[0] }.count, " are primitive.");
}
```

```txt
Up to 100 there are 17 triples, 7 are primitive.
```



### Shorter Version


```d
ulong[2] tri(ulong lim, ulong a=3, ulong b=4, ulong c=5)
pure nothrow @safe @nogc {
    immutable l = a + b + c;
    if (l > lim)
        return [0, 0];
    typeof(return) r = [1, lim / l];
    r[] += tri(lim,  a - 2*b + 2*c,  2*a - b + 2*c,  2*a - 2*b + 3*c)[];
    r[] += tri(lim,  a + 2*b + 2*c,  2*a + b + 2*c,  2*a + 2*b + 3*c)[];
    r[] += tri(lim, -a + 2*b + 2*c, -2*a + b + 2*c, -2*a + 2*b + 3*c)[];
    return r;
}

void main() /*@safe*/ {
    import std.stdio;
    foreach (immutable p; 1 .. 9)
        writeln(10 ^^ p, ' ', tri(10 ^^ p));
}
```

```txt
10 [0, 0]
100 [7, 17]
1000 [70, 325]
10000 [703, 4858]
100000 [7026, 64741]
1000000 [70229, 808950]
10000000 [702309, 9706567]
100000000 [7023027, 113236940]
```

Run-time (32 bit system): about 0.80 seconds with ldc2.


### Short SIMD Version

With LDC compiler this is a little faster than the precedent version (remove @nogc to compile it with the current version of LDC compiler).

```d
import std.stdio, core.simd;

ulong2 tri(in ulong lim, in ulong a=3, in ulong b=4, in ulong c=5)
pure nothrow @safe @nogc {
    immutable l = a + b + c;
    if (l > lim)
        return [0, 0];
    typeof(return) r = [1, lim / l];
    r += tri(lim,  a - 2*b + 2*c,  2*a - b + 2*c,  2*a - 2*b + 3*c);
    r += tri(lim,  a + 2*b + 2*c,  2*a + b + 2*c,  2*a + 2*b + 3*c);
    r += tri(lim, -a + 2*b + 2*c, -2*a + b + 2*c, -2*a + 2*b + 3*c);
    return r;
}

void main() /*@safe*/ {
    foreach (immutable p; 1 .. 9)
        writeln(10 ^^ p, ' ', tri(10 ^^ p).array);
}
```

The output is the same. Run-time (32 bit system): about 0.67 seconds with ldc2.


### Faster Version

```d
import std.stdio;

alias Xuint = uint; // ulong if going over 1 billion.

__gshared Xuint nTriples, nPrimitives, limit;

void countTriples(Xuint x, Xuint y, Xuint z) nothrow @nogc {
    while (true) {
        immutable p = x + y + z;
        if (p > limit)
            return;

        nPrimitives++;
        nTriples += limit / p;

        auto t0 = x - 2 * y + 2 * z;
        auto t1 = 2 * x - y + 2 * z;
        auto t2 = t1 - y + z;
        countTriples(t0, t1, t2);

        t0 += 4 * y;
        t1 += 2 * y;
        t2 += 4 * y;
        countTriples(t0, t1, t2);

        z = t2 - 4 * x;
        y = t1 - 4 * x;
        x = t0 - 2 * x;
    }
}

void main() {
    foreach (immutable p; 1 .. 9) {
        limit = Xuint(10) ^^ p;
        nTriples = nPrimitives = 0;
        countTriples(3, 4, 5);
        writefln("Up to %11d: %11d triples, %9d primitives.",
                 limit, nTriples, nPrimitives);
    }
}
```

```txt
Up to          10:           0 triples,         0 primitives.
Up to         100:          17 triples,         7 primitives.
Up to        1000:         325 triples,        70 primitives.
Up to       10000:        4858 triples,       703 primitives.
Up to      100000:       64741 triples,      7026 primitives.
Up to     1000000:      808950 triples,     70229 primitives.
Up to    10000000:     9706567 triples,    702309 primitives.
Up to   100000000:   113236940 triples,   7023027 primitives.
```

Run-time: about 0.27 seconds with ldc2.

Using the power p up to 11, using ulong for xuint, and compiling with the dmd <code>-L/STACK:10000000</code> switch to increase the stack size to about 10MB:

```txt
Up to          10:           0 triples,         0 primitives.
Up to         100:          17 triples,         7 primitives.
Up to        1000:         325 triples,        70 primitives.
Up to       10000:        4858 triples,       703 primitives.
Up to      100000:       64741 triples,      7026 primitives.
Up to     1000000:      808950 triples,     70229 primitives.
Up to    10000000:     9706567 triples,    702309 primitives.
Up to   100000000:   113236940 triples,   7023027 primitives.
Up to  1000000000:  1294080089 triples,  70230484 primitives.
Up to 10000000000: 14557915466 triples, 702304875 primitives.
```

Total run-time up to 10_000_000_000: about 63 seconds.

Waiting less than half an hour:

```txt
Up to           10:            0 triples,         0 primitives.
Up to          100:           17 triples,         7 primitives.
Up to         1000:          325 triples,        70 primitives.
Up to        10000:         4858 triples,       703 primitives.
Up to       100000:        64741 triples,      7026 primitives.
Up to      1000000:       808950 triples,     70229 primitives.
Up to     10000000:      9706567 triples,    702309 primitives.
Up to    100000000:    113236940 triples,   7023027 primitives.
Up to   1000000000:   1294080089 triples,  70230484 primitives.
Up to  10000000000:  14557915466 triples, 702304875 primitives.
Up to 100000000000: 161750315680 triples, 7023049293 primitives.
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		local
			perimeter: INTEGER
		do
			perimeter := 100
			from
			until
				perimeter > 1000000
			loop
				total := 0
				primitive_triples := 0
				count_pythagorean_triples (3, 4, 5, perimeter)
				io.put_string ("There are " + total.out + " triples, below " + perimeter.out + ". Of which " + primitive_triples.out + " are primitives.%N")
				perimeter := perimeter * 10
			end
		end

	count_pythagorean_triples (a, b, c, perimeter: INTEGER)
			-- Total count of pythagorean triples and total count of primitve triples below perimeter.
		local
			p: INTEGER
		do
			p := a + b + c
			if p <= perimeter then
				primitive_triples := primitive_triples + 1
				total := total + perimeter // p
				count_pythagorean_triples (a + 2 * (- b + c), 2 * (a + c) - b, 2 * (a - b + c) + c, perimeter)
				count_pythagorean_triples (a + 2 * (b + c), 2 * (a + c) + b, 2 * (a + b + c) + c, perimeter)
				count_pythagorean_triples (- a + 2 * (b + c), 2 * (- a + c) + b, 2 * (- a + b + c) + c, perimeter)
			end
		end

feature {NONE}

	primitive_triples: INTEGER

	total: INTEGER

end

```

```txt

There are 17 triples, below 100. Of which 7 are primitives.
There are 325 triples, below 1000. Of which 70 are primitives.
There are 4858 triples, below 10000. Of which 703 are primitives.
There are 64741 triples, below 100000. Of which 7026 are primitives.
There are 808950 triples, below 1000000. Of which 70229 are primitives.

```



## Elixir

```elixir
defmodule RC do
  def count_triples(limit), do: count_triples(limit,3,4,5)

  defp count_triples(limit, a, b, c) when limit<(a+b+c), do: {0,0}
  defp count_triples(limit, a, b, c) do
    {p1, t1} = count_triples(limit, a-2*b+2*c, 2*a-b+2*c, 2*a-2*b+3*c)
    {p2, t2} = count_triples(limit, a+2*b+2*c, 2*a+b+2*c, 2*a+2*b+3*c)
    {p3, t3} = count_triples(limit,-a+2*b+2*c,-2*a+b+2*c,-2*a+2*b+3*c)
    {1+p1+p2+p3, div(limit, a+b+c)+t1+t2+t3}
  end
end

list = for n <- 1..8, do: Enum.reduce(1..n, 1, fn(_,acc)->10*acc end)
Enum.each(list, fn n -> IO.inspect {n, RC.count_triples(n)} end)
```


```txt

{10, {0, 0}}
{100, {7, 17}}
{1000, {70, 325}}
{10000, {703, 4858}}
{100000, {7026, 64741}}
{1000000, {70229, 808950}}
{10000000, {702309, 9706567}}
{100000000, {7023027, 113236940}}

```



## Erlang



```Erlang
%%
%% Pythagorian triples in Erlang, J.W. Luiten
%%
-module(triples).
-export([main/1]).

%% Transformations t1, t2 and t3 to generate new triples
t1(A, B, C) ->
  {A-2*B+2*C, 2*A-B+2*C, 2*A-2*B+3*C}.
t2(A, B, C) ->
  {A+2*B+2*C, 2*A+B+2*C, 2*A+2*B+3*C}.
t3(A, B, C) ->
  {2*B+2*C-A, B+2*C-2*A, 2*B+3*C-2*A}.

%% Generation of triples
count_triples(A, B, C, Tot_acc, Cnt_acc, Max_perimeter) when (A+B+C) =< Max_perimeter ->
  Tot1 = Tot_acc + Max_perimeter div (A+B+C),
  {A1, B1, C1} = t1(A, B, C),
  {Tot2, Cnt2} = count_triples(A1, B1, C1, Tot1, Cnt_acc+1, Max_perimeter),

  {A2, B2, C2} = t2(A, B, C),
  {Tot3, Cnt3} = count_triples(A2, B2, C2, Tot2, Cnt2, Max_perimeter),

  {A3, B3, C3} = t3(A, B, C),
  {Tot4, Cnt4} = count_triples(A3, B3, C3, Tot3, Cnt3, Max_perimeter),
  {Tot4, Cnt4};
count_triples(_A, _B, _C, Tot_acc, Cnt_acc, _Max_perimeter) ->
  {Tot_acc, Cnt_acc}.

count_triples(A, B, C, Pow) ->
  Max = trunc(math:pow(10, Pow)),
  {Tot, Prim} = count_triples(A, B, C, 0, 0, Max),
  {Pow, Tot, Prim}.

count_triples(Pow) ->
  count_triples(3, 4, 5, Pow).

%% Display a single result.
display_result({Pow, Tot, Prim}) ->
  io:format("Up to 10 ** ~w : ~w triples, ~w primitives~n", [Pow, Tot, Prim]).

main(Max) ->
  L = lists:seq(1, Max),
  Answer = lists:map(fun(X) -> count_triples(X) end, L),
  lists:foreach(fun(Result) -> display_result(Result) end, Answer).
```


Output:


```txt
Up to 10 ** 1 : 0 triples, 0 primitives
Up to 10 ** 2 : 17 triples, 7 primitives
Up to 10 ** 3 : 325 triples, 70 primitives
Up to 10 ** 4 : 4858 triples, 703 primitives
Up to 10 ** 5 : 64741 triples, 7026 primitives
Up to 10 ** 6 : 808950 triples, 70229 primitives
Up to 10 ** 7 : 9706567 triples, 702309 primitives
Up to 10 ** 8 : 113236940 triples, 7023027 primitives
Up to 10 ** 9 : 1294080089 triples, 70230484 primitives
Up to 10 ** 10 : 14557915466 triples, 702304875 primitives
Up to 10 ** 11 : 161750315680 triples, 7023049293 primitives

```



## ERRE


```ERRE
PROGRAM PIT

BEGIN

  PRINT(CHR$(12);) !CLS
  PRINT(TIME$)

  FOR POWER=1 TO 7 DO
    PLIMIT=10#^POWER
    UPPERBOUND=INT(1+PLIMIT^0.5)
    PRIMITIVES=0
    TRIPLES=0
    EXTRAS=0          ! will count the in-range multiples of any primitive

    FOR M=2 TO UPPERBOUND DO
        FOR N=1+(M MOD 2=1) TO M-1 STEP 2 DO
            TERM1=2*M*N
            TERM2=M*M-N*N
            TERM3=M*M+N*N
            PERIMETER=TERM1+TERM2+TERM3

            IF PERIMETER<=PLIMIT THEN TRIPLES=TRIPLES+1

            A=TERM1
            B=TERM2

            REPEAT
                R=A-B*INT(A/B)
                A=B
                B=R
            UNTIL R<=0

            ! we've found a primitive triple if a = 1, since hcf =1.
            ! and it is inside perimeter range. Save it in an array
            IF (A=1) AND (PERIMETER<=PLIMIT) THEN
                PRIMITIVES=PRIMITIVES+1

                !-----------------------------------------------
                !swap so in increasing order of side length
                !-----------------------------------------------
                IF TERM1>TERM2 THEN SWAP(TERM1,TERM2)
                !-----------------------------------------------
                !we have the primitive & removed any multiples.
                !Now calculate ALL the multiples in range.
                !-----------------------------------------------
                NEX=INT(PLIMIT/PERIMETER)
                EXTRAS=EXTRAS+NEX
            END IF

            !scan
        END FOR
    END FOR

    PRINT("Primit. with perimeter <=";10#^power;"is";primitives;"&";extras;"non-prim.triples.")
    PRINT(TIME$)
  END FOR

  PRINT PRINT("** End **")
END PROGRAM
```

```txt
16:08:39
Primit. with perimeter <= 10 is 0 & 0 non-prim.triples.
16:08:39
Primit. with perimeter <= 100 is 7 & 17 non-prim.triples.
16:08:39
Primit. with perimeter <= 1000 is 70 & 325 non-prim.triples.
16:08:39
Primit. with perimeter <= 10000 is 703 & 4858 non-prim.triples.
16:08:39
Primit. with perimeter <= 100000 is 7026 & 64741 non-prim.triples.
16:08:41
Primit. with perimeter <= 1000000 is 70229 & 808950 non-prim.triples.
16:09:07
Primit. with perimeter <= 10000000 is 702309 & 9706567 non-prim.triples.
16:13:10

** End **
```



## Euphoria

```euphoria
function tri(atom lim, sequence in)
    sequence r
    atom p
    p = in[1] + in[2] + in[3]
    if p > lim then
        return {0, 0}
    end if
    r = {1, floor(lim / p)}
    r += tri(lim, { in[1]-2*in[2]+2*in[3],  2*in[1]-in[2]+2*in[3],  2*in[1]-2*in[2]+3*in[3]})
    r += tri(lim, { in[1]+2*in[2]+2*in[3],  2*in[1]+in[2]+2*in[3],  2*in[1]+2*in[2]+3*in[3]})
    r += tri(lim, {-in[1]+2*in[2]+2*in[3], -2*in[1]+in[2]+2*in[3], -2*in[1]+2*in[2]+3*in[3]})
    return r
end function

atom max_peri
max_peri = 10
while max_peri <= 100000000 do
    printf(1,"%d: ", max_peri)
    ? tri(max_peri, {3, 4, 5})
    max_peri *= 10
end while
```


Output:

```txt
10: {0,0}
100: {7,17}
1000: {70,325}
10000: {703,4858}
100000: {7026,64741}
1000000: {70229,808950}
10000000: {702309,9706567}
100000000: {7023027,113236940}

```



=={{header|F_Sharp|F#}}==
```fsharp
let isqrt n =
    let rec iter t =
        let d = n - t*t
        if (0 <= d) && (d < t+t+1) // t*t <= n < (t+1)*(t+1)
        then t else iter ((t+(n/t))/2)
    iter 1

let rec gcd a b =
    let t = a % b
    if t = 0 then b else gcd b t

let coprime a b = gcd a b = 1

let num_to ms =
    let mutable ctr = 0
    let mutable prim_ctr = 0
    let max_m = isqrt (ms/2)
    for m = 2 to max_m do
        for j = 0 to (m/2) - 1 do
            let n = m-(2*j+1)
            if coprime m n then
                let s = 2*m*(m+n)
                if s <= ms then
                    ctr <- ctr + (ms/s)
                    prim_ctr <- prim_ctr + 1
    (ctr, prim_ctr)

let show i =
    let s, p = num_to i in
    printfn "For perimeters up to %d there are %d total and %d primitive" i s p;;

List.iter show [ 100; 1000; 10000; 100000; 1000000; 10000000; 100000000 ]
```

```txt
For perimeters up to 100 there are 17 total and 7 primitive
For perimeters up to 1000 there are 325 total and 70 primitive
For perimeters up to 10000 there are 4858 total and 703 primitive
For perimeters up to 100000 there are 64741 total and 7026 primitive
For perimeters up to 1000000 there are 808950 total and 70229 primitive
For perimeters up to 10000000 there are 9706567 total and 702309 primitive
For perimeters up to 100000000 there are 113236940 total and 7023027 primitive
```



## Factor

Pretty slow (100 times slower than C)...


```factor
USING: accessors arrays formatting kernel literals math
math.functions math.matrices math.ranges sequences ;
IN: rosettacode.pyth

CONSTANT: T1 {
  {  1  2  2 }
  { -2 -1 -2 }
  {  2  2  3 }
}
CONSTANT: T2 {
  {  1  2  2 }
  {  2  1  2 }
  {  2  2  3 }
}
CONSTANT: T3 {
  { -1 -2 -2 }
  {  2  1  2 }
  {  2  2  3 }
}

CONSTANT: base { 3 4 5 }

TUPLE: triplets-count primitives total ;
: <0-triplets-count> ( -- a ) 0 0 \ triplets-count boa ;
: next-triplet ( triplet T -- triplet' ) [ 1array ] [ m. ] bi* first ;
: candidates-triplets ( seed -- candidates )
  ${ T1 T2 T3 } [ next-triplet ] with map ;
: add-triplets ( current-triples limit triplet -- stop )
  sum 2dup > [
   /i [ + ] curry change-total
   [ 1 + ] change-primitives drop t
  ] [ 3drop f ] if ;
: all-triplets ( current-triples limit seed -- triplets )
  3dup add-triplets [
    candidates-triplets [ all-triplets ] with swapd reduce
  ] [ 2drop ] if ;
: count-triplets ( limit -- count )
  <0-triplets-count> swap base all-triplets ;
: pprint-triplet-count ( limit count -- )
  [ total>> ] [ primitives>> ] bi
  "Up to %d: %d triples, %d primitives.\n" printf ;
: pyth ( -- )
  8 [1,b] [ 10^ dup count-triplets pprint-triplet-count ] each ;
```



```txt
Up to 10: 0 triples, 0 primitives.
Up to 100: 17 triples, 7 primitives.
Up to 1000: 325 triples, 70 primitives.
Up to 10000: 4858 triples, 703 primitives.
Up to 100000: 64741 triples, 7026 primitives.
Up to 1000000: 808950 triples, 70229 primitives.
Up to 10000000: 9706567 triples, 702309 primitives.
Up to 100000000: 113236940 triples, 7023027 primitives.
Running time: 57.968821207 seconds
```





## Forth




```Forth



\ Two methods to create Pythagorean Triples
\ this code has been tested using Win32Forth and gforth

: pythag_fibo ( f1 f0 -- )
     \ Create Pythagorean Triples from 4 element Fibonacci series
     \ this is called with the first two members of a 4 element Fibonacci series
     \ Price and Burkhart have two good articles about this method
     \ "Pythagorean Tree: A New Species" and
     \ "Heron's Formula, Descartes Circles, and Pythagorean Triangles"
     \ Horadam found out how to compute Pythagorean Triples from Fibonacci series

     \ compute the two other members of the Fibonacci series and put them in
     \ local variables.  I was unable to do this with out using locals
     2DUP + 2DUP + 2OVER 2DUP + 2DUP +
     LOCALS| f3 f2 f1 f0 |

     wk_level @  9 .r f0 8 .r  f1 8 .r  f2 8 .r  f3 8 .r

     \ this block calculates the sides of the Pythagorean Triangle using single precision
     \ f0 f3 * 14 .r                 \ side a  (always odd)
     \ 2 f1 * f2 * 10 .r             \ side b  (a multiple of 4)
     \ f0 f2 * f1 f3 * + 10 .r       \ side c, the hyponenuse, (always odd)

     \ this block calculates double precision values
     f0 f3 um* 15 d.r                    \ side a  (always odd)
     2 f1 * f2 um* 15 d.r                \ side b  (a multiple of 4)
     f0 f2 um* f1 f3 um* d+ 17 d.r  cr   \ side c, the hypotenuse, (always odd)

     MAX_LEVEL @ wk_LEVEL @ U> IF   \ TRUE if MAX_LEVEL > WK_LEVEL
     wk_level @ 1+ wk_level !

     \ this creates a teranary tree of Pythagorean triples
     \ use a two of the members of the Fibonacci series as seeds for the
     \ next level
     \ It's the same tree created by Barning or Hall using matrix multiplication
     f3 f1 recurse
     f3 f2 recurse
     f0 f2 recurse

     wk_level @ 1- wk_level !

     else
     then

     drop drop drop drop ;

\ implements the Fibonacci series -- Pythagorean triple
\ the stack contents sets how many iteration levels there will be
: pf_test
     \ the stack contents set up the maximum level
     max_level !
     0 wk_level !
     cr

     \ call the function with the first two elements of the base Fibonacci series
     1 1 pythag_fibo  ;

: gcd ( a b -- gcd )
  begin ?dup while tuck mod repeat ;

\ this is the classical algorithm, known to Euclid, it is explained in many
\ books on Number Theory
\ this generates all primitive Pythagorean triples

\ i -- inner loop index or current loop index
\ j -- outer loop index
\ stack contents is the upper limit for j
\ i and j can not both be odd
\ the gcd( i, j ) must be 1
\ j is greater than i
\ the stack contains the upper limit of the j variable
: pythag_ancn  ( limit -- )
     cr
     1 + 2 do
        i 1 and if 2 else 1 then
        \ this sets the start value of the inner loop so that
        \ if the outer loop index is odd only even inner loop indices happen
        \ if the outer loop index is even only odd inner loop indices happen
        i swap do
             i j gcd 1 - 0> if else  \ do this if gcd( i, j ) is 1
             j 5 .r i 5 .r

             \ j j * i i * - 12 .r   \ a side of Pythagorean triangle (always odd)
             \ i j * 2 * 9 .r        \ b side of Pythagorean triangle (multiple of 4)
             \ i i * j j * + 9 .r    \ hypotenuse of Pythagorean triangle (always odd)

             \ this block calculates double precision Pythagorean triple values
             j j um* i i um* d- 15 d.r    \ a side of Pythagorean triangle (always odd)
             i j um* d2* 15 d.r           \ b side of Pythagorean triangle (multiple of 4)
             i i um* j j um* d+ 17 d.r    \ hypotenuse of Pythagorean triangle (always odd)

             cr then 2 +loop       \ keep i being all odd or all even
     loop ;



Current directory: C:\Forth ok
FLOAD 'C:\Forth\ancien_fibo_pythag.F'  ok
  ok

  ok
  ok
3 pf_test
        0       1       1       2       3              3              4                5
        1       3       1       4       5             15              8               17
        2       5       1       6       7             35             12               37
        3       7       1       8       9             63             16               65
        3       7       6      13      19            133            156              205
        3       5       6      11      17             85            132              157
        2       5       4       9      13             65             72               97
        3      13       4      17      21            273            136              305
        3      13       9      22      31            403            396              565
        3       5       9      14      23            115            252              277
        2       3       4       7      11             33             56               65
        3      11       4      15      19            209            120              241
        3      11       7      18      25            275            252              373
        3       3       7      10      17             51            140              149
        1       3       2       5       7             21             20               29
        2       7       2       9      11             77             36               85
        3      11       2      13      15            165             52              173
        3      11       9      20      29            319            360              481
        3       7       9      16      25            175            288              337
        2       7       5      12      17            119            120              169
        3      17       5      22      27            459            220              509
        3      17      12      29      41            697            696              985
        3       7      12      19      31            217            456              505
        2       3       5       8      13             39             80               89
        3      13       5      18      23            299            180              349
        3      13       8      21      29            377            336              505
        3       3       8      11      19             57            176              185
        1       1       2       3       5              5             12               13
        2       5       2       7       9             45             28               53
        3       9       2      11      13            117             44              125
        3       9       7      16      23            207            224              305
        3       5       7      12      19             95            168              193
        2       5       3       8      11             55             48               73
        3      11       3      14      17            187             84              205
        3      11       8      19      27            297            304              425
        3       5       8      13      21            105            208              233
        2       1       3       4       7              7             24               25
        3       7       3      10      13             91             60              109
        3       7       4      11      15            105             88              137
        3       1       4       5       9              9             40               41
 ok
  ok
10 pythag_ancn
    2    1              3              4                5
    3    2              5             12               13
    4    1             15              8               17
    4    3              7             24               25
    5    2             21             20               29
    5    4              9             40               41
    6    1             35             12               37
    6    5             11             60               61
    7    2             45             28               53
    7    4             33             56               65
    7    6             13             84               85
    8    1             63             16               65
    8    3             55             48               73
    8    5             39             80               89
    8    7             15            112              113
    9    2             77             36               85
    9    4             65             72               97
    9    8             17            144              145
   10    1             99             20              101
   10    3             91             60              109
   10    7             51            140              149
   10    9             19            180              181
 ok


```



## Fortran

```fortran
module triples
  implicit none

  integer :: max_peri, prim, total
  integer :: u(9,3) = reshape((/ 1, -2, 2,  2, -1, 2,  2, -2, 3, &
                                 1,  2, 2,  2,  1, 2,  2,  2, 3, &
                                -1,  2, 2, -2,  1, 2, -2,  2, 3 /), &
                                (/ 9, 3 /))

contains

recursive subroutine new_tri(in)
  integer, intent(in) :: in(:)
  integer :: i
  integer :: t(3), p

  p = sum(in)
  if (p > max_peri) return

  prim = prim + 1
  total = total + max_peri / p
  do i = 1, 3
    t(1) = sum(u(1:3, i) * in)
    t(2) = sum(u(4:6, i) * in)
    t(3) = sum(u(7:9, i) * in)
    call new_tri(t);
  end do
end subroutine new_tri
end module triples

program Pythagorean
  use triples
  implicit none

  integer :: seed(3) = (/ 3, 4, 5 /)

  max_peri = 10
  do
    total = 0
    prim = 0
    call new_tri(seed)
    write(*, "(a, i10, 2(i10, a))") "Up to", max_peri, total, " triples",  prim, " primitives"
    if(max_peri == 100000000) exit
    max_peri = max_peri * 10
  end do
end program Pythagorean
```

Output:
```txt
Up to         10          0 triples         0 primitives
Up to        100         17 triples         7 primitives
Up to       1000        325 triples        70 primitives
Up to      10000       4858 triples       703 primitives
Up to     100000      64741 triples      7026 primitives
Up to    1000000     808950 triples     70229 primitives
Up to   10000000    9706567 triples    702309 primitives
Up to  100000000  113236940 triples   7023027 primitives
```



## FreeBASIC


The upper limit is set to 12(10^12), this will take about 3-4 hours.
If you can't wait that long better lower it to 11(10^11).


### Version 1

Normal version

```freebasic
' version 30-05-2016
' compile with: fbc -s console

' primitive pythagoras triples
' a = m^2 - n^2, b = 2mn, c = m^2 + n^2
' m, n are positive integers and m > n
' m - n = odd and GCD(m, n) = 1
' p = a + b + c

' max m for give perimeter
' p = m^2 - n^2 + 2mn + m^2 + n^2
' p = 2mn + m^2 + m^2 + n^2 - n^2 = 2mn + 2m^2
' m >> n and n = 1 ==> p = 2m + 2m^2 = 2m(1 + m)
' m >> 1 ==> p = 2m(m) = 2m^2
' max m for given perimeter = sqr(p / 2)

Function gcd(x As UInteger, y As UInteger) As UInteger

    Dim As UInteger t

    While y
        t = y
        y = x Mod y
        x = t
    Wend
    Return x

End Function


Sub pyth_trip(limit As ULongInt, ByRef trip As ULongInt, ByRef prim As ULongInt)

    Dim As ULongInt perimeter, lby2 = limit Shr 1
    Dim As UInteger m, n
    Dim As ULongInt a, b, c

    For m = 2 To Sqr(limit / 2)
        For n = 1 + (m And 1) To (m - 1) Step 2
            ' common divisor, try next n
            If (gcd(m, n) > 1) Then Continue For
            a = CULngInt(m) * m - n * n
            b = CULngInt(m) * n * 2
            c = CULngInt(m) * m + n * n
            perimeter = a + b + c
            ' perimeter > limit, since n goes up try next m
            If perimeter >= limit Then Continue For, For
            prim += 1
            If perimeter < lby2 Then
                trip += limit \ perimeter
            Else
                trip += 1
            End If
        Next n
    Next m

End Sub


' ------=< MAIN >=------

Dim As String str1, buffer = Space(14)
Dim As ULongInt limit, trip, prim
Dim As Double t, t1 = Timer

Print "below           triples   primitive            time"
Print

For x As UInteger = 1 To 12
    t = Timer
    limit = 10 ^ x : trip = 0 : prim = 0
    pyth_trip(limit, trip, prim)
    LSet buffer, Str(prim) : str1 = buffer
    Print Using "10^##  ################   "; x; trip;
    If x > 7 Then
        Print str1;
        Print Using "  ######.## sec."; Timer - t
    Else
        Print str1
    End If
Next x

Print : Print
Print Using "Total time needed #######.## sec."; Timer - t1


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
below           triples   primitive            time

10^ 1                 0   0
10^ 2                17   7
10^ 3               325   70
10^ 4              4858   703
10^ 5             64741   7026
10^ 6            808950   70229
10^ 7           9706567   702309
10^ 8         113236940   7023027              0.94 sec.
10^ 9        1294080089   70230484            10.13 sec.
10^10       14557915466   702304875          109.75 sec.
10^11      161750315680   7023049293        1204.56 sec.
10^12     1779214833461   70230492763      13031.84 sec.

Total time needed   14357.31 sec.
```


### Version 2

Attempt to make a faster version (about 20% faster)


```freebasic
' version 30-05-2016
' compile with: fbc -s console

' max m for give perimeter
' p = m^2 - n^2 + 2mn + m^2 + n^2
' p = 2mn + m^2 + m^2 + n^2 - n^2 = 2mn + 2m^2
' m >> n and n = 1 ==> p = 2m + 2m^2 = 2m(1 + m)
' m >> 1 ==> p = 2m(m) = 2m^2
' max m for given perimeter = sqr(p / 2)

Function gcd(x As UInteger, y As UInteger) As UInteger

    Dim As UInteger t

    While y
        t = y
        y = x Mod y
        x = t
    Wend
    Return x

End Function

Sub pyth_trip_fast(limit As ULongInt, ByRef trip As ULongInt, ByRef prim As ULongInt)


    Dim As ULongInt perimeter, lby2 = limit Shr 1
    Dim As UInteger mx2 = 4

    For m As UInteger = 2 To Sqr(limit / 2)
        perimeter = (CULngInt(m) * m * 2) - IIf(m And 1, 0, m * 2)
        mx2 = mx2 + 4
        For n As UInteger = 1 + (m And 1) To (m - 1) Step 2
            perimeter += mx2
            ' common divisor, try next n
            If (gcd(m, n) > 1) Then Continue For
            ' perimeter > limit, since n goes up try next m
            If perimeter >= limit Then Continue For, For
            prim += 1
            If perimeter < lby2 Then
                trip += limit \ perimeter
            Else
                trip += 1
            End If
        Next n
    Next m

End Sub


' ------=< MAIN >=------

Dim As String str1, buffer = Space(14)
Dim As ULongInt limit, trip, prim
Dim As Double t, t1 = Timer

Print "below           triples   primitive            time"
Print

For x As UInteger = 1 To 12
    t = Timer
    limit = 10 ^ x : trip = 0 : prim = 0
    pyth_trip_fast(limit, trip, prim)
    LSet buffer, Str(prim) : str1 = buffer
    Print Using "10^##  ################   "; x; trip;
    If x > 7 Then
        Print str1;
        Print Using "  ######.## sec."; Timer - t
    Else
        Print str1
    End If
Next x

Print : Print
Print Using "Total time needed #######.## sec."; Timer - t1


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
below           triples   primitive            time

10^ 1                 0   0
10^ 2                17   7
10^ 3               325   70
10^ 4              4858   703
10^ 5             64741   7026
10^ 6            808950   70229
10^ 7           9706567   702309
10^ 8         113236940   7023027              0.66 sec.
10^ 9        1294080089   70230484             7.48 sec.
10^10       14557915466   702304875           83.92 sec.
10^11      161750315680   7023049293         945.95 sec.
10^12     1779214833461   70230492763      10467.94 sec.

Total time needed   11506.01 sec.
```


The time needed is about 11 times the time needed for the previous limit.
To calculate 10^12 with the fast version that would take about 32 hours.
The variable that holds the number of triples will eventual overflow
at 10^18 - 10^19. To reach that stage you need the program to run
for a few years.


## Go


```go
package main

import "fmt"

var total, prim, maxPeri int64

func newTri(s0, s1, s2 int64) {
    if p := s0 + s1 + s2; p <= maxPeri {
        prim++
        total += maxPeri / p
        newTri(+1*s0-2*s1+2*s2, +2*s0-1*s1+2*s2, +2*s0-2*s1+3*s2)
        newTri(+1*s0+2*s1+2*s2, +2*s0+1*s1+2*s2, +2*s0+2*s1+3*s2)
        newTri(-1*s0+2*s1+2*s2, -2*s0+1*s1+2*s2, -2*s0+2*s1+3*s2)
    }
}

func main() {
    for maxPeri = 100; maxPeri <= 1e11; maxPeri *= 10 {
        prim = 0
        total = 0
        newTri(3, 4, 5)
        fmt.Printf("Up to %d:  %d triples, %d primitives\n",
            maxPeri, total, prim)
    }
}
```

Output:

```txt

Up to 100:  17 triples, 7 primitives
Up to 1000:  325 triples, 70 primitives
Up to 10000:  4858 triples, 703 primitives
Up to 100000:  64741 triples, 7026 primitives
Up to 1000000:  808950 triples, 70229 primitives
Up to 10000000:  9706567 triples, 702309 primitives
Up to 100000000:  113236940 triples, 7023027 primitives
Up to 1000000000:  1294080089 triples, 70230484 primitives
Up to 10000000000:  14557915466 triples, 702304875 primitives
Up to 100000000000:  161750315680 triples, 7023049293 primitives

```



## Groovy


### Parent/Child Algorithm

Solution:

```groovy
class Triple {
    BigInteger a, b, c
    def getPerimeter() { this.with { a + b + c } }
    boolean isValid() { this.with { a*a + b*b == c*c } }
}

def initCounts (def n = 10) {
    (n..1).collect { 10g**it }.inject ([:]) { Map map, BigInteger perimeterLimit ->
        map << [(perimeterLimit): [primative: 0g, total: 0g]]
    }
}

def findPythagTriples, findChildTriples

findPythagTriples = {Triple t = new Triple(a:3, b:4, c:5), Map counts = initCounts() ->
    def p = t.perimeter
    def currentCounts = counts.findAll { pLimit, tripleCounts -> p <= pLimit }
    if (! currentCounts || ! t.valid) { return }
    currentCounts.each { pLimit, tripleCounts ->
        tripleCounts.with { primative ++; total += pLimit.intdiv(p) }
    }
    findChildTriples(t, currentCounts)
    counts
}

findChildTriples = { Triple t, Map counts ->
    t.with {
        [
            [ a - 2*b + 2*c,  2*a - b + 2*c,  2*a - 2*b + 3*c],
            [ a + 2*b + 2*c,  2*a + b + 2*c,  2*a + 2*b + 3*c],
            [-a + 2*b + 2*c, -2*a + b + 2*c, -2*a + 2*b + 3*c]
        ]*.sort().each { aa, bb, cc ->
            findPythagTriples(new Triple(a:aa, b:bb, c:cc), counts)
        }
    }
}
```


Test:

```groovy
printf ('    LIMIT       PRIMATIVE          ALL\n')
findPythagTriples().sort().each { perimeterLimit, result ->
    def exponent = perimeterLimit.toString().size() - 1
    printf ('a+b+c <= 10E%2d  %9d %12d\n', exponent, result.primative, result.total)
}
```


Output:

```txt
    LIMIT       PRIMATIVE          ALL
a+b+c <= 10E 1          0            0
a+b+c <= 10E 2          7           17
a+b+c <= 10E 3         70          325
a+b+c <= 10E 4        703         4858
a+b+c <= 10E 5       7026        64741
a+b+c <= 10E 6      70229       808950
a+b+c <= 10E 7     702309      9706567
a+b+c <= 10E 8    7023027    113236940
a+b+c <= 10E 9   70230484   1294080089
a+b+c <= 10E10  702304875  14557915466
```



## Haskell



```haskell
pytr :: Int -> [(Bool, Int, Int, Int)]
pytr n =
  filter
    (\(_, a, b, c) -> a + b + c <= n)
    [ (prim a b c, a, b, c)
    | a <- xs
    , b <- drop a xs
    , c <- drop b xs
    , a ^ 2 + b ^ 2 == c ^ 2 ]
  where
    xs = [1 .. n]
    prim a b _ = gcd a b == 1

main :: IO ()
main =
  putStrLn $
  "Up to 100 there are " ++
  show (length xs) ++
  " triples, of which " ++
  show (length $ filter (\(x, _, _, _) -> x) xs) ++ " are primitive."
  where
    xs = pytr 100
```


```txt
Up to 100 there are 17 triples, of which 7 are primitive.
```


Or equivalently (desugaring the list comprehension down to nested concatMaps, and pruning back the search space a little):

```haskell
pythagoreanTriplesBelow :: Int -> [[Int]]
pythagoreanTriplesBelow n =
  let m = quot n 2
  in concatMap
       (\x ->
           concatMap
             (\y ->
                 concatMap
                   (\z ->
                       if x + y + z <= n && x ^ 2 + y ^ 2 == z ^ 2
                         then [[x, y, z]]
                         else [])
                   [y + 1 .. m])
             [x + 1 .. m])
       [1 .. m]

-- TEST -------------------------------------------------------------------------
main :: IO ()
main =
  mapM_
    (print . length)
    ([id, filter (\[x, y, _] -> gcd x y == 1)] <*> [pythagoreanTriplesBelow 100])
```

```txt
17
7
```


Recursive primitive generation:

```haskell
triangles :: Int -> [[Int]]
triangles max_peri
  | max_peri < 12 = []
  | otherwise = concat tiers
  where
    tiers = takeWhile (not . null) $ iterate tier [[3, 4, 5]]
    tier = concatMap (filter ((<= max_peri) . sum) . tmul)
    tmul t =
      map
        (map (sum . zipWith (*) t))
        [ [[1, -2, 2], [2, -1, 2], [2, -2, 3]]
        , [[1, 2, 2], [2, 1, 2], [2, 2, 3]]
        , [[-1, 2, 2], [-2, 1, 2], [-2, 2, 3]]
        ]

triangleCount max_p = (length t, sum $ map ((max_p `div`) . sum) t)
  where
    t = triangles max_p

main :: IO ()
main =
  mapM_
    ((putStrLn . (\n -> show n ++ " " ++ show (triangleCount n))) . (10 ^))
    [1 .. 7]
```

```txt
10 (0,0)
100 (7,17)
1000 (70,325)
10000 (703,4858)
100000 (7026,64741)
1000000 (70229,808950)
10000000 (702309,9706567)
```


=={{header|Icon}} and {{header|Unicon}}==
This uses the elegant formula (#IV) from [[wp:Formulas_for_generating_Pythagorean_triples|Formulas for generating Pythagorean triples]]


```Icon

link numbers
link printf

procedure main(A)  # P-triples

   plimit := (0 < integer(\A[1])) | 100 # get perimiter limit

   nonprimitiveS := set()  # record unique non-primitives triples
   primitiveS := set()     # record unique primitive triples

   u :=  0
   while (g := (u +:= 1)^2) + 3 * u + 2 < plimit / 2 do {
      every v := seq(1) do {
         a := g + (i := 2*u*v)
         b := (h := 2*v^2) + i
         c := g + h + i
         if (p := a + b + c) > plimit then break

         insert( (gcd(u,v)=1 & u%2=1, primitiveS) | nonprimitiveS, memo(a,b,c))
         every k := seq(2) do {      # k is for larger non-primitives
            if k*p > plimit then break
            insert(nonprimitiveS,memo(a*k,b*k,c*k) )
            }
         }
      }

printf("Under perimiter=%d: Pythagorean Triples=%d including primitives=%d\n",
       plimit,*nonprimitiveS+*primitiveS,*primitiveS)

every put(gcol := []  , &collections)
printf("Time=%d, Collections: total=%d string=%d block=%d",&time,gcol[1],gcol[3],gcol[4])
end


procedure memo(x[]) #: return a csv string of arguments in sorted order
every (s := "") ||:= !sort(x) do s ||:= ","
return s[1:-1]
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn numbers.icn provides gcd]
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf]

The output from some sample runs with BLKSIZE=500000000 and STRSIZE=50000000 is below.  It starts getting very slow after 10M at about 9 minutes (times are shown in ms.  I suspect there may be faster gcd algorithms that would speed this up.

Output:
```txt

Under perimiter=10: Pythagorean Triples=0 including primitives=0
Time=3, Collections: total=0 string=0 block=0

Under perimiter=100: Pythagorean Triples=17 including primitives=7
Time=3, Collections: total=0 string=0 block=0

Under perimiter=1000: Pythagorean Triples=325 including primitives=70
Time=6, Collections: total=0 string=0 block=0

Under perimiter=10000: Pythagorean Triples=4858 including primitives=703
Time=57, Collections: total=0 string=0 block=0

Under perimiter=100000: Pythagorean Triples=64741 including primitives=7026
Time=738, Collections: total=0 string=0 block=0

Under perimiter=1000000: Pythagorean Triples=808950 including primitives=70229
Time=12454, Collections: total=0 string=0 block=0

Under perimiter=10000000: Pythagorean Triples=9706567 including primitives=702309
Time=560625, Collections: total=16 string=8 block=8
```




## J


Brute force approach:


```j
pytr=: 3 :0
  r=. i. 0 3
  for_a. 1 + i. <.(y-1)%3 do.
    b=. 1 + a + i. <.(y%2)-3*a%2
    c=. a +&.*: b
    keep=. (c = <.c) *. y >: a+b+c
    if. 1 e. keep do.
      r=. r, a,.b ,.&(keep&#) c
    end.
  end.
  (,.~ prim"1)r
)

prim=: 1 = 2 +./@{. |:
```


Example use:

First column indicates whether the triple is primitive, and the remaining three columns are a, b and c.


```j
   pytr 100
1  3  4  5
1  5 12 13
0  6  8 10
1  7 24 25
1  8 15 17
0  9 12 15
1  9 40 41
0 10 24 26
0 12 16 20
1 12 35 37
0 15 20 25
0 15 36 39
0 16 30 34
0 18 24 30
1 20 21 29
0 21 28 35
0 24 32 40
   (# , [: {. +/) pytr 10
0 0
   (# , [: {. +/) pytr 100
17 7
   (# , [: {. +/) pytr 1000
325 70
   (# , [: {. +/) pytr 10000
4858 703
```


pytr 10000 takes 4 seconds on this laptop, and time to complete grows with square of perimeter, so pytr 1e6 should take something like 11 hours using this algorithm on this machine.

A slightly smarter approach:


```j
trips=:3 :0
  'm n'=. |:(#~ 1 = 2 | +/"1)(#~ >/"1) ,/ ,"0/~ }. i. <. %: y
  prim=. (#~ 1 = 2 +./@{. |:) (#~ y >: +/"1)m (-&*: ,. +:@* ,. +&*:) n
  /:~ ; <@(,.~ # {. 1:)@(*/~ 1 + y i.@<.@% +/)"1 prim
)
```


usage for trips is the same as for pytr.  Thus:


```j
   (# , 1 {. +/) trips 10
0 0
   (# , 1 {. +/) trips 100
17 7
   (# , 1 {. +/) trips 1000
325 70
   (# , 1 {. +/) trips 10000
4858 703
   (# , 1 {. +/) trips 100000
64741 7026
   (# , 1 {. +/) trips 1000000
808950 70229
   (# , 1 {. +/) trips 10000000
9706567 702309
```


The last line took about 16 seconds.

That said, we do not actually have to generate all the triples, we just need to count them.  Thus:


```j
trc=:3 :0
  'm n'=. |:(#~ 1 = 2 | +/"1)(#~ >/"1) ,/ ,"0/~ }. i. <. %: y
  <.y%+/"1 (#~ 1 = 2 +./@{. |:) (#~ y >: +/"1)m (-&*: ,. +:@* ,. +&*:) n
)
```


The result is a list of positive integers, one number for each primitive triple which fits within the limit, giving the number of triples which are multiples of that primitive triple whose perimeter is no greater than the limiting perimeter.

<lang>   (#,+/)trc 1e8
7023027 113236940
```


But note that J's memory footprint reached 6.7GB during the computation, so to compute larger values the computation would have to be broken up into reasonable sized blocks.


## Java


### Brute force

[[Category:Arbitrary precision]]Theoretically, this can go "forever", but it takes a while, so only the minimum is shown. Luckily, <code>BigInteger</code> has a GCD method built in.

```java

import java.math.BigInteger;
import static java.math.BigInteger.ONE;

public class PythTrip{

    public static void main(String[] args){
        long tripCount = 0, primCount = 0;

        //change this to whatever perimeter limit you want;the RAM's the limit
        BigInteger periLimit = BigInteger.valueOf(100),
                peri2 = periLimit.divide(BigInteger.valueOf(2)),
                peri3 = periLimit.divide(BigInteger.valueOf(3));

        for(BigInteger a = ONE; a.compareTo(peri3) < 0; a = a.add(ONE)){
            BigInteger aa = a.multiply(a);

            for(BigInteger b = a.add(ONE);
                    b.compareTo(peri2) < 0; b = b.add(ONE)){
                BigInteger bb = b.multiply(b);
                BigInteger ab = a.add(b);
                BigInteger aabb = aa.add(bb);

                for(BigInteger c = b.add(ONE);
                        c.compareTo(peri2) < 0; c = c.add(ONE)){

                    int compare = aabb.compareTo(c.multiply(c));
                    //if a+b+c > periLimit
                    if(ab.add(c).compareTo(periLimit) > 0){
                        break;
                    }
                    //if a^2 + b^2 != c^2
                    if(compare < 0){
                        break;
                    }else if (compare == 0){
                        tripCount++;
                        System.out.print(a + ", " + b + ", " + c);

                        //does binary GCD under the hood
                        if(a.gcd(b).equals(ONE)){
                            System.out.print(" primitive");
                            primCount++;
                        }
                        System.out.println();
                    }
                }
            }
        }
        System.out.println("Up to a perimeter of " + periLimit + ", there are "
                + tripCount + " triples, of which " + primCount + " are primitive.");
    }
}
```

Output:

```txt
3, 4, 5 primitive
5, 12, 13 primitive
6, 8, 10
7, 24, 25 primitive
8, 15, 17 primitive
9, 12, 15
9, 40, 41 primitive
10, 24, 26
12, 16, 20
12, 35, 37 primitive
15, 20, 25
15, 36, 39
16, 30, 34
18, 24, 30
20, 21, 29 primitive
21, 28, 35
24, 32, 40
Up to a perimeter of 100, there are 17 triples, of which 7 are primitive.
```


### Brute force primitives with scaling

[[Pythagorean triples/Java/Brute force primitives]]

### Parent/child

{{trans|Perl 6}} (with limited modification for saving a few BigInteger operations)
This can also go "forever" theoretically. Letting it go to another order of magnitude overflowed the stack on the computer this was tested on. This version also does not show the triples as it goes, it only counts them.

```java5
import java.math.BigInteger;

public class Triples{
    public static BigInteger LIMIT;
    public static final BigInteger TWO = BigInteger.valueOf(2);
    public static final BigInteger THREE = BigInteger.valueOf(3);
    public static final BigInteger FOUR = BigInteger.valueOf(4);
    public static final BigInteger FIVE = BigInteger.valueOf(5);
    public static long primCount = 0;
    public static long tripCount = 0;

    //I don't know Japanese :p
    public static void parChild(BigInteger a, BigInteger b, BigInteger c){
        BigInteger perim = a.add(b).add(c);
        if(perim.compareTo(LIMIT) > 0) return;
        primCount++; tripCount += LIMIT.divide(perim).longValue();
        BigInteger a2 = TWO.multiply(a), b2 = TWO.multiply(b), c2 = TWO.multiply(c),
                   c3 = THREE.multiply(c);
        parChild(a.subtract(b2).add(c2),
                 a2.subtract(b).add(c2),
                 a2.subtract(b2).add(c3));
        parChild(a.add(b2).add(c2),
                 a2.add(b).add(c2),
                 a2.add(b2).add(c3));
        parChild(a.negate().add(b2).add(c2),
                 a2.negate().add(b).add(c2),
                 a2.negate().add(b2).add(c3));
    }

    public static void main(String[] args){
        for(long i = 100; i <= 10000000; i*=10){
            LIMIT = BigInteger.valueOf(i);
            primCount = tripCount = 0;
            parChild(THREE, FOUR, FIVE);
            System.out.println(LIMIT + ": " + tripCount + " triples, " + primCount + " primitive.");
        }
    }
}
```

Output:

```txt
100: 17 triples, 7 primitive.
1000: 325 triples, 70 primitive.
10000: 4858 triples, 703 primitive.
100000: 64741 triples, 7026 primitive.
1000000: 808950 triples, 70229 primitive.
10000000: 9706567 triples, 702309 primitive.
```



## JavaScript


### ES6

Exhaustive search of a full cartesian product. Not scalable.

```JavaScript
(() => {
    'use strict';

    // Arguments: predicate, maximum perimeter
    // pythTripleCount :: ((Int, Int, Int) -> Bool) -> Int -> Int
    const pythTripleCount = (p, maxPerim) => {
        const xs = enumFromTo(1, Math.floor(maxPerim / 2));

        return  concatMap(x =>
                concatMap(y =>
                concatMap(z =>
                    ((x + y + z <= maxPerim) &&
                        (x * x + y * y === z * z) &&
                        p(x, y, z)) ? [
                        [x, y, z]
                    ] : [], // (Empty lists disappear under concatenation)
                xs.slice(y)), xs.slice(x)), xs
            )
            .length;
    };

    // GENERIC FUNCTIONS --------------------------------------

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.length > 0 ? [].concat.apply([], xs.map(f)) : [];

    // enumFromTo :: Enum a => a -> a -> [a]
    const enumFromTo = (m, n) =>
        (typeof m !== 'number' ? (
            enumFromToChar
        ) : enumFromToInt)
        .apply(null, [m, n]);

    // enumFromToInt :: Int -> Int -> [Int]
    const enumFromToInt = (m, n) =>
        n >= m ? Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i) : [];

    // gcd :: Int -> Int -> Int
    const gcd = (x, y) => {
        const _gcd = (a, b) => (b === 0 ? a : _gcd(b, a % b));
        return _gcd(Math.abs(x), Math.abs(y));
    };

    // MAIN ---------------------------------------------------
    return [10, 100, 1000]
        .map(n => ({
            maxPerimeter: n,
            triples: pythTripleCount(x => true, n),
            primitives: pythTripleCount((x, y, _) => gcd(x, y) === 1, n)
        }));

})();
```

```JavaScript
[{"maxPerimeter":10, "triples":0, "primitives":0},
 {"maxPerimeter":100, "triples":17, "primitives":7},
 {"maxPerimeter":1000, "triples":325, "primitives":70}]
```



## jq

The jq program presented here is based on Euclid's formula, and uses the same algorithm and notation as in the AutoHotKey section.

The implementation illustrates how an inner function with arity 0 can
attain a high level of efficiency with both jq 1.4 and later. A simpler implementation is possible with versions of jq greater than 1.4.

```jq
def gcd(a; b):
  def _gcd:
    if .[1] == 0 then .[0]
    else [.[1], .[0] % .[1]] | _gcd
    end;
  [a,b] | _gcd ;

# Return: [total, primitives] for pythagorean triangles having
# perimeter no larger than peri.
# The following uses Euclid's formula with the convention: m > n.
def count(peri):

  # The inner function can be used to count for a given value of m:
  def _count:
    # state [n,m,p, [total, primitives]]
    .[0] as $n | .[1] as $m | .[2] as $p
    | if $n < $m and $p <= peri then
        if (gcd($m;$n) == 1)
        then .[3] | [ (.[0] + ((peri/$p)|floor) ),  (.[1] + 1)]
        else .[3]
        end
        | [$n+2, $m, $p+4*$m, .] | _count
      else .
      end;

  # m^2 < m*(m+1) <= m*(m+n) = perimeter/2
  reduce range(2;  (peri/2) | sqrt + 1) as $m
    ( [1, 2, 12, [0,0]];
      (($m % 2) + 1) as $n
      | (2 * $m * ($m + $n) ) as $p   # a+b+c for this (m,n)
      | [$n, $m, $p, .[3]] | _count
    ) | .[3] ;

# '''Example''':
def pow(i): . as $in | reduce range(0; i) as $j (1; . * $in);

range(1; 9) | . as $i | 10|pow($i) as $i | "\($i):  \(count($i) )"

```

```sh
$ jq -M -c -r -n -f Pythagorean_triples.jq
10:  [0,0]
100:  [17,7]
1000:  [325,70]
10000:  [4858,703]
100000:  [64741,7026]
1000000:  [808950,70229]
10000000:  [9706567,702309]
100000000:  [113236940,7023027]

```



## Julia

This solution uses the the Euclidian concept of m and n as generators of Pythagorean triplets.  When m and n are coprime and have opposite parity, the generated triplets are primitive.  It works reasonably well up to a limit of 10^10.

```Julia

function primitiven{T<:Integer}(m::T)
    1 < m || return T[]
    m != 2 || return T[1]
    !isprime(m) || return T[2:2:m-1]
    rp = trues(m-1)
    if isodd(m)
        rp[1:2:m-1] = false
    end
    for p in keys(factor(m))
        rp[p:p:m-1] = false
    end
    T[1:m-1][rp]
end

function pythagoreantripcount{T<:Integer}(plim::T)
    primcnt = 0
    fullcnt = 0
    11 < plim || return (primcnt, fullcnt)
    for m in 2:plim
        p = 2m^2
        p+2m <= plim || break
        for n in primitiven(m)
            q = p + 2m*n
            q <= plim || break
            primcnt += 1
            fullcnt += div(plim, q)
        end
    end
    return (primcnt, fullcnt)
end

println("Counting Pythagorian Triplets within perimeter limits:")
println("    Limit          All   Primitive")
for om in 1:10
    (pcnt, fcnt) = pythagoreantripcount(10^om)
    println(@sprintf "    10^%02d  %11d   %9d" om fcnt pcnt)
end

```


```txt

Counting Pythagorian Triplets within perimeter limits:
    Limit          All   Primitive
    10^01            0           0
    10^02           17           7
    10^03          325          70
    10^04         4858         703
    10^05        64741        7026
    10^06       808950       70229
    10^07      9706567      702309
    10^08    113236940     7023027
    10^09   1294080089    70230484
    10^10  14557915466   702304875

```



## Kotlin

Due to deep recursion, I needed to increase the stack size to 4MB to get up to a maximum perimeter of 10 billion. Expect a run time of around 30 seconds on a typical laptop.

```scala
// version 1.1.2

var total = 0L
var prim = 0L
var maxPeri = 0L

fun newTri(s0: Long, s1: Long, s2: Long) {
    val p = s0 + s1 + s2
    if (p <= maxPeri) {
        prim++
        total += maxPeri / p
        newTri( s0 - 2 * s1 + 2 * s2,  2 * s0 - s1 + 2 * s2,  2 * s0 - 2 * s1 + 3 * s2)
        newTri( s0 + 2 * s1 + 2 * s2,  2 * s0 + s1 + 2 * s2,  2 * s0 + 2 * s1 + 3 * s2)
        newTri(-s0 + 2 * s1 + 2 * s2, -2 * s0 + s1 + 2 * s2, -2 * s0 + 2 * s1 + 3 * s2)
    }
}

fun main(args: Array<String>) {
    maxPeri = 100
    while (maxPeri <= 10_000_000_000L) {
        prim = 0
        total = 0
        newTri(3, 4, 5)
        println("Up to $maxPeri: $total triples, $prim primatives")
        maxPeri *= 10
    }
}
```


```txt

Up to 100: 17 triples, 7 primatives
Up to 1000: 325 triples, 70 primatives
Up to 10000: 4858 triples, 703 primatives
Up to 100000: 64741 triples, 7026 primatives
Up to 1000000: 808950 triples, 70229 primatives
Up to 10000000: 9706567 triples, 702309 primatives
Up to 100000000: 113236940 triples, 7023027 primatives
Up to 1000000000: 1294080089 triples, 70230484 primatives
Up to 10000000000: 14557915466 triples, 702304875 primatives

```



## Lasso


```lasso
// Brute Force: Too slow for large numbers
define num_pythagorean_triples(max_perimeter::integer) => {
   local(max_b) = (#max_perimeter / 3)*2

   return (
      with a in 1 to (#max_b - 1)
      sum integer(
         with b in (#a + 1) to #max_b
         let c = math_sqrt(#a*#a + #b*#b)
         where #c == integer(#c)
         where #c > #b
         where (#a+#b+#c) <= #max_perimeter
         sum 1
      )
   )
}
stdout(`Number of Pythagorean Triples in a Perimeter of 100: `)
stdoutnl(num_pythagorean_triples(100))

```

Output:

```txt
Number of Pythagorean Triples in a Perimeter of 100: 17

```



## Liberty BASIC


```lb

print time$()

for power =1 to 6
    perimeterLimit =10^power
    upperBound     =int( 1 +perimeterLimit^0.5)
    primitives     =0
    triples        =0
    extras         =0   '   will count the in-range multiples of any primitive

    for m =2 to upperBound
        for n =1 +( m mod 2 =1) to m -1 step 2
            term1      =2 *m *n
            term2      =m *m -n *n
            term3      =m *m +n *n
            perimeter  =term1 +term2 +term3

            if perimeter <=perimeterLimit then triples =triples +1

            a     =term1
            b     =term2

            do
                r = a mod b
                a =b
                b =r
            loop until r <=0

            if ( a =1) and ( perimeter <=perimeterLimit) then                       'we've found a primitive triple if a =1, since hcf =1. And it is inside perimeter range. Save it in an array
                primitives =primitives +1
                if term1 >term2 then temp =term1: term1 =term2: term2 =temp         'swap so in increasing order of side length
                nEx =int( perimeterLimit /perimeter)                     'We have the primitive & removed any multiples. Now calculate ALL the multiples in range.
                extras =extras +nEx
            end if

            scan
        next n
    next m

    print "  Number of primitives having perimeter below "; 10^power, " was "; primitives, " & "; extras, " non-primitive triples."
    print time$()
next power

print "End"
end

```


```txt

17:59:34
Number of primitives having perimeter below 10 was 0 & 0 non-primitive triples.
17:59:34
Number of primitives having perimeter below 100 was 7 & 17 non-primitive triples.
17:59:34
Number of primitives having perimeter below 1000 was 70 & 325 non-primitive triples.
17:59:34
Number of primitives having perimeter below 10000 was 703 & 4858 non-primitive triples.
17:59:35
Number of primitives having perimeter below 100000 was 7026 & 64741 non-primitive triples.
17:59:42
Number of primitives having perimeter below 1000000 was 70229 & 808950 non-primitive triples.
18:01:30
End

```



## Mathematica

Short code but not a very scalable approach...

```Mathematica
pythag[n_] := Block[{soln = Solve[{a^2 + b^2 == c^2, a + b + c <= n, 0 < a < b < c}, {a, b, c}, Integers]},
        {Length[soln], Count[GCD[a, b] == GCD[b, c] == GCD[c, a] == 1 /. soln, True]}
      ]
```


```txt
pythag[10]
{0,0}

pythag[100]
{17, 7}

pythag[1000]
{325, 70}
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
N=  100;
  a = 1:N;
  b = a(ones(N,1),:).^2;
  b = b+b';
  b = sqrt(b);  [y,x]=find(b==fix(b)); % test
  % here some alternative tests
  % b = b.^(1/k); [y,x]=find(b==fix(b)); % test 2
  % [y,x]=find(b==(fix(b.^(1/k)).^k));  % test 3
  % b=b.^(1/k); [y,x]=find(abs(b - round(b)) <= 4*eps*b);

  z  = sqrt(x.^2+y.^2);
  ix = (z+x+y<100) & (x < y) & (y < z);
  p  = find(gcd(x(ix),y(ix))==1);   % find primitive triples

  printf('There are %i Pythagorean Triples and %i primitive triples with a perimeter smaller than %i.\n',...
         sum(ix), length(p), N);
```


Output:

```txt
 There are 17 Pythagorean Triples and 7 primitive triples with a perimeter smaller than 100.
```



## Mercury


From [[List comprehensions]]:


```mercury

:- module comprehension.
:- interface.
:- import_module io.
:- import_module int.

:- type triple ---> triple(int, int, int).

:- pred pythTrip(int::in,triple::out) is nondet.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module solutions.

pythTrip(Limit,triple(X,Y,Z)) :-
    nondet_int_in_range(1,Limit,X),
    nondet_int_in_range(X,Limit,Y),
    nondet_int_in_range(Y,Limit,Z),
    pow(Z,2) = pow(X,2) + pow(Y,2).

main(!IO) :-
    solutions((pred(Triple::out) is nondet :- pythTrip(20,Triple)),Result),
    write(Result,!IO).

```


=={{header|Modula-3}}==
Note that this code only works on 64bit machines (where <tt>INTEGER</tt> is 64 bits). Modula-3 provides a <tt>LONGINT</tt> type, which is 64 bits on 32 bit systems, but there is a bug in the implementation apparently.

```modula3
MODULE PyTriple64 EXPORTS Main;

IMPORT IO, Fmt;

VAR tcnt, pcnt, max, i: INTEGER;

PROCEDURE NewTriangle(a, b, c: INTEGER; VAR tcount, pcount: INTEGER) =
  VAR perim := a + b + c;
  BEGIN
    IF perim <= max THEN
      pcount := pcount + 1;
      tcount := tcount + max DIV perim;
      NewTriangle(a-2*b+2*c, 2*a-b+2*c, 2*a-2*b+3*c, tcount, pcount);
      NewTriangle(a+2*b+2*c, 2*a+b+2*c, 2*a+2*b+3*c, tcount, pcount);
      NewTriangle(2*b+2*c-a, b+2*c-2*a, 2*b+3*c-2*a, tcount, pcount);
    END;
  END NewTriangle;

BEGIN
  i := 100;

  REPEAT
    max := i;
    tcnt := 0;
    pcnt := 0;
    NewTriangle(3, 4, 5, tcnt, pcnt);
    IO.Put(Fmt.Int(i) & ": " & Fmt.Int(tcnt) & " Triples, " &
      Fmt.Int(pcnt) & " Primitives\n");
    i := i * 10;
  UNTIL i = 10000000;
END PyTriple64.
```


Output:

```txt

100: 17 Triples, 7 Primitives
1000: 325 Triples, 70 Primitives
10000: 4858 Triples, 703 Primitives
100000: 64741 Triples, 7026 Primitives
1000000: 808950 Triples, 70229 Primitives

```



## Nim

```nim
const u = [[ 1, -2,  2,  2, -1,  2,  2, -2,  3],
           [ 1,  2,  2,  2,  1,  2,  2,  2,  3],
           [-1,  2,  2, -2,  1,  2, -2,  2,  3]]

var
  total, prim = 0
  maxPeri = 10

proc newTri(ins: array[0..2, int]) =
  var p = ins[0] + ins[1] + ins[2]
  if p > maxPeri: return
  inc(prim)
  total += maxPeri div p

  for i in 0..2:
    newTri([u[i][0] * ins[0] + u[i][1] * ins[1] + u[i][2] * ins[2],
            u[i][3] * ins[0] + u[i][4] * ins[1] + u[i][5] * ins[2],
            u[i][6] * ins[0] + u[i][7] * ins[1] + u[i][8] * ins[2]])

while maxPeri <= 100_000_000:
  total = 0
  prim = 0
  newTri([3, 4, 5])
  echo "Up to ", maxPeri, ": ", total, " triples, ", prim, " primitives"
  maxPeri *= 10
```

Output:

```txt
Up to 10: 0 triples, 0 primitives
Up to 100: 17 triples, 7 primitives
Up to 1000: 325 triples, 70 primitives
Up to 10000: 4858 triples, 703 primitives
Up to 100000: 64741 triples, 7026 primitives
Up to 1000000: 808950 triples, 70229 primitives
Up to 10000000: 9706567 triples, 702309 primitives
Up to 100000000: 113236940 triples, 7023027 primitives
```



## OCaml


```OCaml
let isqrt n =
   let rec iter t =
      let d = n - t*t in
      if (0 <= d) && (d < t+t+1) (* t*t <= n < (t+1)*(t+1) *)
      then t else iter ((t+(n/t))/2)
   in iter 1

let rec gcd a b =
   let t = a mod b in
   if t = 0 then b else gcd b t

let coprime a b = gcd a b = 1

let num_to ms =
   let ctr = ref 0 in
   let prim_ctr = ref 0 in
   let max_m = isqrt (ms/2) in
   for m = 2 to max_m do
      for j = 0 to (m/2) - 1 do
         let n = m-(2*j+1) in
         if coprime m n then
            let s = 2*m*(m+n) in
            if s <= ms then
               (ctr := !ctr + (ms/s); incr prim_ctr)
      done
   done;
  (!ctr, !prim_ctr)

let show i =
  let s, p = num_to i in
  Printf.printf "For perimeters up to %d there are %d total and %d primitive\n%!" i s p;;

List.iter show [ 100; 1000; 10000; 100000; 1000000; 10000000; 100000000 ]
```

Output:

```txt
For perimeters up to 100 there are 17 total and 7 primitive
For perimeters up to 1000 there are 325 total and 70 primitive
For perimeters up to 10000 there are 4858 total and 703 primitive
For perimeters up to 100000 there are 64741 total and 7026 primitive
For perimeters up to 1000000 there are 808950 total and 70229 primitive
For perimeters up to 10000000 there are 9706567 total and 702309 primitive
For perimeters up to 100000000 there are 113236940 total and 7023027 primitive
```



## Ol


```scheme

; triples generator based on Euclid's formula, creates lazy list
(define (euclid-formula max)
   (let loop ((a 3) (b 4) (c 5) (tail #null))
      (if (<= (+ a b c) max)
         (cons (tuple a b c) (lambda ()
            (let ((d (- b)) (z (- a)))
            (loop (+ a d d c c) (+ a a d c c) (+ a a d d c c c) (lambda ()
               (loop (+ a b b c c) (+ a a b c c) (+ a a b b c c c) (lambda ()
                  (loop (+ z b b c c) (+ z z b c c) (+ z z b b c c c) tail))))))))
         tail)))

; let's do calculations
(define (calculate max)
   (let loop ((p 0) (t 0) (ll (euclid-formula max)))
      (cond
         ((null? ll)
            (cons p t))
         ((function? ll)
            (loop p t (ll)))
         (else
            (let ((triple (car ll)))
               (loop (+ p 1) (+ t (div max (apply + triple)))
               (cdr ll)))))))

; print values for 10..100000
(for-each (lambda (max)
      (print max ": " (calculate max)))
   (map (lambda (n) (expt 10 n)) (iota 6 1)))

```


```txt

10: (0 . 0)
100: (7 . 17)
1000: (70 . 325)
10000: (703 . 4858)
100000: (7026 . 64741)
1000000: (70229 . 808950)

```



## PARI/GP

This version is reasonably efficient and can handle inputs like a million quickly.

```parigp
do(lim)={
  my(prim,total,P);
  lim\=1;
  for(m=2,sqrtint(lim\2),
    forstep(n=1+m%2,min(sqrtint(lim-m^2),m-1),2,
      P=2*m*(m+n);
      if(gcd(m,n)==1 && P<=lim,
        prim++;
        total+=lim\P
      )
    )
  );
  [prim,total]
};
do(100)
```



## Pascal


```pascal
Program PythagoreanTriples (output);

var
  total, prim, maxPeri: int64;

procedure newTri(s0, s1, s2: int64);
  var
    p: int64;
  begin
    p := s0 + s1 + s2;
    if p <= maxPeri then
    begin
      inc(prim);
      total := total + maxPeri div p;
      newTri( s0 + 2*(-s1+s2),  2*( s0+s2) - s1,  2*( s0-s1+s2) + s2);
      newTri( s0 + 2*( s1+s2),  2*( s0+s2) + s1,  2*( s0+s1+s2) + s2);
      newTri(-s0 + 2*( s1+s2),  2*(-s0+s2) + s1,  2*(-s0+s1+s2) + s2);
    end;
  end;

begin
  maxPeri := 100;
  while maxPeri <= 1e10 do
  begin
    prim := 0;
    total := 0;
    newTri(3, 4, 5);
    writeln('Up to ', maxPeri, ': ', total, ' triples, ', prim, ' primitives.');
    maxPeri := maxPeri * 10;
  end;
end.
```

Output (on Core2Duo 2GHz laptop):

```txt
time ./PythagoreanTriples
Up to 100: 17 triples, 7 primitives.
Up to 1000: 325 triples, 70 primitives.
Up to 10000: 4858 triples, 703 primitives.
Up to 100000: 64741 triples, 7026 primitives.
Up to 1000000: 808950 triples, 70229 primitives.
Up to 10000000: 9706567 triples, 702309 primitives.
Up to 100000000: 113236940 triples, 7023027 primitives.
Up to 1000000000: 1294080089 triples, 70230484 primitives.
Up to 10000000000: 14557915466 triples, 702304875 primitives.
109.694u 0.094s 1:50.43 99.4%   0+0k 0+0io 0pf+0w
```



## Perl


```perl
sub gcd {
    my ($n, $m) = @_;
    while($n){
        my $t = $n;
        $n = $m % $n;
        $m = $t;
    }
    return $m;
}

sub tripel {
    my $pmax  = shift;
    my $prim  = 0;
    my $count = 0;
    my $nmax = sqrt($pmax)/2;
    for( my $n=1; $n<=$nmax; $n++ ) {
        for( my $m=$n+1; (my $p = 2*$m*($m+$n)) <= $pmax; $m+=2 ) {
            next unless 1==gcd($m,$n);
            $prim++;
            $count += int $pmax/$p;
        }
    }
    printf "Max. perimeter: %d, Total: %d, Primitive: %d\n", $pmax, $count, $prim;
}

tripel 10**$_ for 1..8;

```

```txt
Max. perimeter: 10, Total: 0, Primitive: 0
Max. perimeter: 100, Total: 17, Primitive: 7
Max. perimeter: 1000, Total: 325, Primitive: 70
Max. perimeter: 10000, Total: 4858, Primitive: 703
Max. perimeter: 100000, Total: 64741, Primitive: 7026
Max. perimeter: 1000000, Total: 808950, Primitive: 70229
Max. perimeter: 10000000, Total: 9706567, Primitive: 702309
Max. perimeter: 100000000, Total: 113236940, Primitive: 7023027

```




## Perl 6

Here is a straight-forward, naive brute force implementation:

```perl6
constant limit = 100;

for [X] [^limit] xx 3 -> (\a, \b, \c) {
    say [a, b, c] if a < b < c and a**2 + b**2 == c**2
}
```

<pre style="height:25ex">3 4 5
5 12 13
6 8 10
7 24 25
8 15 17
9 12 15
9 40 41
10 24 26
11 60 61
12 16 20
12 35 37
13 84 85
14 48 50
15 20 25
15 36 39
16 30 34
16 63 65
18 80 82
20 21 29
20 48 52
21 28 35
21 72 75
24 32 40
24 45 51
24 70 74
25 60 65
27 36 45
28 45 53
30 40 50
30 72 78
32 60 68
33 44 55
33 56 65
35 84 91
36 48 60
36 77 85
39 52 65
39 80 89
40 42 58
40 75 85
42 56 70
45 60 75
48 55 73
48 64 80
51 68 85
54 72 90
57 76 95
60 63 87
65 72 97
```

Here is a slightly less naive brute force implementation, but still not practical for large perimeter limits.

```perl6
my $limit = 10000;
my atomicint $i = 0;
my @triples[$limit/2];
(3 .. $limit/2).race.map: -> $c {
   for 1 .. $c -> $a {
       my $b = ($c * $c - $a * $a).sqrt;
       last if $c + $a + $b > $limit;
       last if $a > $b;
       @triples[$i⚛++] = ([gcd] $c, $a, $b) > 1 ?? 0 !! 1 if $b == $b.Int;
   }
}

say my $result = "There are {+@triples.grep:{$_ !eqv Any}} Pythagorean Triples with a perimeter <= $limit,"
 ~"\nof which {[+] @triples.grep: so *} are primitive.";
```

```txt
There are 4858 Pythagorean Triples with a perimeter <= 10000,
of which 703 are primitive.
```

Here's a much faster version.  Hint, "oyako" is Japanese for "parent/child". <tt>:-)</tt>

```perl6
sub triples($limit) {
    my $primitive = 0;
    my $civilized = 0;

    sub oyako($a, $b, $c) {
        my $perim = $a + $b + $c;
        return if $perim > $limit;
        ++$primitive; $civilized += $limit div $perim;
        oyako( $a - 2*$b + 2*$c,  2*$a - $b + 2*$c,  2*$a - 2*$b + 3*$c);
        oyako( $a + 2*$b + 2*$c,  2*$a + $b + 2*$c,  2*$a + 2*$b + 3*$c);
        oyako(-$a + 2*$b + 2*$c, -2*$a + $b + 2*$c, -2*$a + 2*$b + 3*$c);
    }

    oyako(3,4,5);
    "$limit => ($primitive $civilized)";
}

for 10,100,1000 ... * -> $limit {
    say triples $limit;
}
```

Output:

```txt
10 => (0 0)
100 => (7 17)
1000 => (70 325)
10000 => (703 4858)
100000 => (7026 64741)
1000000 => (70229 808950)
10000000 => (702309 9706567)
100000000 => (7023027 113236940)
1000000000 => (70230484 1294080089)
^C
```

The geometric sequence of limits will continue on forever, so eventually when you get tired of waiting (about a billion on my computer), you can just stop it.  Another efficiency trick of note: we avoid passing the limit in as a parameter to the inner helper routine, but instead supply the limit via the lexical scope.  Likewise, the accumulators are referenced lexically, so only the triples themselves need to be passed downward, and nothing needs to be returned.

Here is an alternate version that avoids naming any scalars that can be handled by vector processing instead. Using vectorized ops allows a bit more potential for parallelization in theory, but techniques like the use of complex numbers to add two numbers in parallel, and the use of <tt>gather</tt>/<tt>take</tt> generate so much overhead that this version runs 70-fold slower than the previous one.

```perl6
constant @coeff = [[+1, -2, +2], [+2, -1, +2], [+2, -2, +3]],
                  [[+1, +2, +2], [+2, +1, +2], [+2, +2, +3]],
                  [[-1, +2, +2], [-2, +1, +2], [-2, +2, +3]];

sub triples($limit) {

    sub oyako(@trippy) {
        my $perim = [+] @trippy;
        return if $perim > $limit;
        take (1 + ($limit div $perim)i);
        for @coeff -> @nine {
            oyako (map -> @three { [+] @three »*« @trippy }, @nine);
        }
        return;
    }

    my $complex = 0i + [+] gather oyako([3,4,5]);
    "$limit => ({$complex.re, $complex.im})";
}

for 10, 100, 1000, 10000 -> $limit {
    say triples $limit;
}
```

```txt
10 => (0 0)
100 => (7 17)
1000 => (70 325)
10000 => (703 4858)
```



## Phix

```Phix
atom total, prim, maxPeri = 10

procedure tri(atom s0, s1, s2)
atom p = s0 + s1 + s2
    if p<=maxPeri then
        prim += 1
        total += floor(maxPeri/p)
        tri( s0+2*(-s1+s2), 2*( s0+s2)-s1, 2*( s0-s1+s2)+s2);
        tri( s0+2*( s1+s2), 2*( s0+s2)+s1, 2*( s0+s1+s2)+s2);
        tri(-s0+2*( s1+s2), 2*(-s0+s2)+s1, 2*(-s0+s1+s2)+s2);
    end if
end procedure

while maxPeri<=1e8 do
    prim := 0;
    total := 0;
    tri(3, 4, 5);
    printf(1,"Up to %d: %d triples, %d primitives.\n", {maxPeri,total,prim})
    maxPeri *= 10;
end while
```

```txt

Up to 10: 0 triples, 0 primitives.
Up to 100: 17 triples, 7 primitives.
Up to 1000: 325 triples, 70 primitives.
Up to 10000: 4858 triples, 703 primitives.
Up to 100000: 64741 triples, 7026 primitives.
Up to 1000000: 808950 triples, 70229 primitives.
Up to 10000000: 9706567 triples, 702309 primitives.
Up to 100000000: 113236940 triples, 7023027 primitives.

```



## PicoLisp

```PicoLisp
(for (Max 10  (>= 100000000 Max)  (* Max 10))
   (let (Total 0  Prim 0  In (3 4 5))
      (recur (In)
         (let P (apply + In)
            (when (>= Max P)
               (inc 'Prim)
               (inc 'Total (/ Max P))
               (for Row
                  (quote
                     (( 1 -2 2) ( 2 -1 2) ( 2 -2 3))
                     (( 1  2 2) ( 2  1 2) ( 2  2 3))
                     ((-1  2 2) (-2  1 2) (-2  2 3)) )
                  (recurse
                     (mapcar '((U) (sum * U In)) Row) ) ) ) ) )
      (prinl "Up to " Max ": " Total " triples, " Prim " primitives.") ) )
```

Output:

```txt
Up to 10: 0 triples, 0 primitives.
Up to 100: 17 triples, 7 primitives.
Up to 1000: 325 triples, 70 primitives.
Up to 10000: 4858 triples, 703 primitives.
Up to 100000: 64741 triples, 7026 primitives.
Up to 1000000: 808950 triples, 70229 primitives.
Up to 10000000: 9706567 triples, 702309 primitives.
Up to 100000000: 113236940 triples, 7023027 primitives.
```



## PL/I

Version 1

```PL/I
*process source attributes xref or(!);
 /*********************************************************************
 * REXX pgm counts number of  Pythagorean triples
 * that exist given a max perimeter of  N,
 * and also counts how many of them are primatives.
 * 05.05.2013 Walter Pachl  translated from REXX version 2
 *********************************************************************/
 pyt: Proc Options(main);
 Dcl sysprint Print;
 Dcl (addr,mod,right) Builtin;
 Dcl memn Bin Fixed(31) Init(0);
 Dcl mabca(300) Char(12);
 Dcl 1 mabc,
      2 ma Dec fixed(7),
      2 mb Dec fixed(7),
      2 mc Dec fixed(7);
 Dcl mabce Char(12) Based(addr(mabc));
 Dcl 1 abc,
      2 a Dec fixed(7),
      2 b Dec fixed(7),
      2 c Dec fixed(7);
 Dcl abce Char(12) Based(addr(abc));
 Dcl (prims,trips,m,n,aa,aabb,cc,aeven,ab) Dec Fixed(7);
 mabca='';
 trips=0;
 prims=0;
 n=100;
 la:
 Do a=3 To n/3;
    aa=a*a;                       /* limit side to 1/3 of perimeter.*/
    aeven=mod(a,2)=0;
 lb:Do b=a+1 By 1+aeven;          /* triangle can't be isosceles.   */
      ab=a+b;                     /* compute partial perimeter.     */
      If ab>=n Then
        Iterate la;               /* a+b>perimeter?  Try different A*/
      aabb=aa+b*b;                /* compute sum of  a² + b² (cheat)*/
      Do c=b+1 By 1;
        cc=c*c;                   /* 3rd side:   also compute  c²   */
        If aeven Then
          If mod(c,2)=0 Then
            Iterate;
        If ab+c>n Then
          Iterate la;              /* a+b+c > perimeter?  Try diff A.*/
        If cc>aabb Then
          Iterate lb;              /* c² >  a²+b² ?  Try different B.*/
        If cc^=aabb Then
          Iterate;                 /* c² ¬= a²+b² ?  Try different C.*/
        If mema(abce) Then
          Iterate;
        trips=trips+1;             /* eureka.                        */
        prims=prims+1;             /* count this primitive triple.   */
        Put Edit(a,b,c,'   ',right(a**2+b**2,5),right(c**2,5),a+b+c)
                (Skip,f(4),2(f(5)),a,2(f(6)),f(9));
        Do m=2 By 1;
          ma=a*m;
          mb=b*m;
          mc=c*m;                  /* gen non-primitives.            */
          If ma+mb+mc>n Then
            Leave;
                                   /* is this multiple a triple ?    */
          trips=trips+1;           /* yuppers, then we found another.*/
          If mod(m,2)=1 Then       /* store as even multiple.        */
            call mems(mabce);
          Put Edit(ma,mb,mc,' * ',
                          right(ma**2+mb**2,5),right(mc**2,5),ma+mb+mc)
                (Skip,f(4),2(f(5)),a,2(f(6)),f(9));
          End;                     /* m                              */
        End;                       /* c                              */
      End;                         /* b                              */
    End;                           /* a                              */
  Put Edit('max perimeter = ',n,   /* show a single line of output.  */
           'Pythagorean triples =',trips,
           'primitives =',prims)
          (Skip,a,f(5),2(x(9),a,f(4)));

 mems: Proc(e);
 Dcl e Char(12);
 memn+=1;
 mabca(memn)=e;
 End;

 mema: Proc(e) Returns(bit(1));
 Dcl e Char(12);
 Do memi=1 To memn;
   If mabca(memi)=e Then Return('1'b);
   End;
 Return('0'b);
 End;

 End;
```

Version 2

```PL/I

pythagorean: procedure options (main, reorder); /* 23 January 2014 */
   declare (a, b, c) fixed (3);
   declare (asquared, bsquared) fixed;
   declare (triples, primitives) fixed binary(31) initial (0);

   do a = 1 to 100;
      asquared = a*a;
      do b = a+1 to 100;
         bsquared = b*b;
         do c = b+1 to 100;
            if a+b+c <= 100 then
             if asquared + bsquared = c*c then
               do;
                  triples = triples + 1;
                  if GCD(a,b) = 1 then primitives = primitives + 1;
               end;
         end;
      end;
   end;
   put skip data (triples, primitives);


GCD: procedure (a, b) returns (fixed binary (31)) recursive;
   declare (a, b) fixed binary (31);

   if b = 0 then return (a);

   return (GCD (b, mod(a, b)) );

end GCD;

end pythagorean;
```

Output:

```txt

TRIPLES=            17  PRIMITIVES=             7;

```

Output for P=1000:

```txt

TRIPLES=           325  PRIMITIVES=            70;

```



## PowerShell


```PowerShell

function triples($p) {
    if($p -gt 4) {
        # ai + bi + ci = pi <= p
        # ai < bi < ci --> 3ai < pi <= p and ai + 2bi < pi <= p
        $pa = [Math]::Floor($p/3)
        1..$pa | foreach {
            $ai = $_
            $pb = [Math]::Floor(($p-$ai)/2)
            ($ai+1)..$pb | foreach {
                $bi = $_
                $pc = $p-$ai-$bi
                ($bi+1)..$pc | where {
                    $ci = $_
                    $pi = $ai + $bi + $ci
                    $ci*$ci -eq $ai*$ai + $bi*$bi
                 } |
                foreach {
                    [pscustomobject]@{
                        a = "$ai"
                        b = "$bi"
                        c = "$ci"
                        p = "$pi"
                    }
                }
            }
        }
    }
    else {
        Write-Error "$p is not greater than 4"
    }
}
function gcd ($a, $b)  {
    function pgcd ($n, $m)  {
        if($n -le $m) {
            if($n -eq 0) {$m}
            else{pgcd $n ($m%$n)}
        }
        else {pgcd $m $n}
    }
    $n = [Math]::Abs($a)
    $m = [Math]::Abs($b)
    (pgcd $n $m)
}
$triples = (triples 100)

$coprime = $triples |
where {((gcd $_.a $_.b) -eq 1) -and ((gcd $_.a $_.c) -eq 1) -and  ((gcd $_.b $_.c) -eq 1)}

"There are $(($triples).Count) Pythagorean triples with perimeter no larger than 100
 and $(($coprime).Count) of them are coprime."

```

<b>Output:</b>

```txt

There are 17 Pythagorean triples with perimeter no larger than 100 and 7 of them are coprime.

```



## Prolog


```prolog

show :-
    Data = [100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000],
    forall(
        member(Max, Data),
        (count_triples(Max, Total, Prim),
         format("upto ~D, there are ~D Pythagorean triples (~D primitive.)~n", [Max, Total, Prim]))).

div(A, B, C) :- C is A div B.

count_triples(Max, Total, Prims) :-
    findall(S, (triple(Max, A, B, C), S is A + B + C), Ps),
    length(Ps, Prims),
    maplist(div(Max), Ps, Counts), sumlist(Counts, Total).

% - between_by/4

between_by(A, B, N, K) :-
    C is (B - A) div N,
    between(0, C, J),
    K is N*J + A.

% - Pythagorean triple generator

triple(P, A, B, C) :-
    Max is floor(sqrt(P/2)) - 1,
    between(0, Max, M),
    Start is (M /\ 1) + 1, succ(Pm, M),
    between_by(Start, Pm, 2, N),
    gcd(M, N) =:= 1,
    X is M*M - N*N,
    Y is 2*M*N,
    C is M*M + N*N,
    order2(X, Y, A, B),
    (A + B + C) =< P.

order2(A, B, A, B) :- A < B, !.
order2(A, B, B, A).

```


```txt

?- show.
upto 100, there are 17 Pythagorean triples (7 primitive.)
upto 1,000, there are 325 Pythagorean triples (70 primitive.)
upto 10,000, there are 4,857 Pythagorean triples (702 primitive.)
upto 100,000, there are 64,741 Pythagorean triples (7,026 primitive.)
upto 1,000,000, there are 808,950 Pythagorean triples (70,229 primitive.)
upto 10,000,000, there are 9,706,567 Pythagorean triples (702,309 primitive.)
upto 100,000,000, there are 113,236,940 Pythagorean triples (7,023,027 primitive.)
true.

```



## PureBasic


<math>(a=m^2-n^2) \and (b=2mn) \and (c=m^2+n^2) \and (p=a+b+c) \rightarrow</math><br /><br />
<math>p=2m^2+2mn \le 100,000,000 \rightarrow</math><br /><br />
<math>m^2+mn \le 50,000,000 \rightarrow</math><br /><br />

<math>m^2+mn -50,000,000 \le 0 \rightarrow</math><br /><br />
<math>n \le 10,000</math><br /><br />


```purebasic


Procedure.i ConsoleWrite(t.s)  ; compile using /CONSOLE option
        OpenConsole()
        PrintN (t.s)
        CloseConsole()
        ProcedureReturn 1
EndProcedure

Procedure.i StdOut(t.s)  ; compile using /CONSOLE option
        OpenConsole()
        Print(t.s)
        CloseConsole()
        ProcedureReturn 1
EndProcedure

Procedure.i gcDiv(n,m) ; greatest common divisor
if n=0:ProcedureReturn m:endif
while m <> 0
  if n > m
    n - m
  else
    m - n
  endif
wend
ProcedureReturn n
EndProcedure

st=ElapsedMilliseconds()

nmax      =10000
power     =8

dim primitiveA(power)
dim alltripleA(power)
dim pmaxA(power)

x=1
for i=1 to power
  x*10:pmaxA(i)=x/2
next

for n=1 to nmax
  for m=(n+1) to (nmax+1) step 2 ; assure m-n is odd
    d=gcDiv(n,m)
    p=m*m+m*n
    for i=1 to power
      if p<=pmaxA(i)
        if d =1
        primitiveA(i)+1        ; right here we have the primitive perimeter "seed" 'p'
        k=1:q=p*k              ; set k to one to include p : use q as the 'p*k'
          while q<=pmaxA(i)
          alltripleA(i)+1      ; accumulate multiples of this perimeter while q <= pmaxA(i)
          k+1:q=p*k
          wend
        endif
      endif
    next
  next
next

for i=1 to power
  t.s="Up to "+str(pmaxA(i)*2)+": "
  t.s+str(alltripleA(i))+" triples, "
  t.s+str(primitiveA(i))+" primitives."
  ConsoleWrite(t.s)
next
ConsoleWrite("")
et=ElapsedMilliseconds()-st:ConsoleWrite("Elapsed time = "+str(et)+" milliseconds")

```



; Output

```txt

>cmd /c "C:\_sys\temp\PythagoreanTriples.exe"

Up to 10: 0 triples, 0 primitives.
Up to 100: 17 triples, 7 primitives.
Up to 1000: 325 triples, 70 primitives.
Up to 10000: 4858 triples, 703 primitives.
Up to 100000: 64741 triples, 7026 primitives.
Up to 1000000: 808950 triples, 70229 primitives.
Up to 10000000: 9706567 triples, 702309 primitives.
Up to 100000000: 113236940 triples, 7023027 primitives.

Elapsed time = 5163 milliseconds

>Exit code: 0


```



## Python

Two methods, the second of which is much faster

```python
from fractions import gcd


def pt1(maxperimeter=100):
    '''
# Naive method
    '''
    trips = []
    for a in range(1, maxperimeter):
        aa = a*a
        for b in range(a, maxperimeter-a+1):
            bb = b*b
            for c in range(b, maxperimeter-b-a+1):
                cc = c*c
                if a+b+c > maxperimeter or cc > aa + bb: break
                if aa + bb == cc:
                    trips.append((a,b,c, gcd(a, b) == 1))
    return trips

def pytrip(trip=(3,4,5),perim=100, prim=1):
    a0, b0, c0 = a, b, c = sorted(trip)
    t, firstprim = set(), prim>0
    while a + b + c <= perim:
        t.add((a, b, c, firstprim>0))
        a, b, c, firstprim = a+a0, b+b0, c+c0, False
    #
    t2 = set()
    for a, b, c, firstprim in t:
        a2, a5, b2, b5, c2, c3, c7 = a*2, a*5, b*2, b*5, c*2, c*3, c*7
        if  a5 - b5 + c7 <= perim:
            t2 |= pytrip(( a - b2 + c2,  a2 - b + c2,  a2 - b2 + c3), perim, firstprim)
        if  a5 + b5 + c7 <= perim:
            t2 |= pytrip(( a + b2 + c2,  a2 + b + c2,  a2 + b2 + c3), perim, firstprim)
        if -a5 + b5 + c7 <= perim:
            t2 |= pytrip((-a + b2 + c2, -a2 + b + c2, -a2 + b2 + c3), perim, firstprim)
    return t | t2

def pt2(maxperimeter=100):
    '''
# Parent/child relationship method:
# http://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples#XI.
    '''
    trips = pytrip((3,4,5), maxperimeter, 1)
    return trips

def printit(maxperimeter=100, pt=pt1):
    trips = pt(maxperimeter)
    print("  Up to a perimeter of %i there are %i triples, of which %i are primitive"
          % (maxperimeter,
             len(trips),
             len([prim for a,b,c,prim in trips if prim])))

for algo, mn, mx in ((pt1, 250, 2500), (pt2, 500, 20000)):
    print(algo.__doc__)
    for maxperimeter in range(mn, mx+1, mn):
        printit(maxperimeter, algo)

```


;Output:

```txt

# Naive method

  Up to a perimeter of 250 there are 56 triples, of which 18 are primitive
  Up to a perimeter of 500 there are 137 triples, of which 35 are primitive
  Up to a perimeter of 750 there are 227 triples, of which 52 are primitive
  Up to a perimeter of 1000 there are 325 triples, of which 70 are primitive
  Up to a perimeter of 1250 there are 425 triples, of which 88 are primitive
  Up to a perimeter of 1500 there are 527 triples, of which 104 are primitive
  Up to a perimeter of 1750 there are 637 triples, of which 123 are primitive
  Up to a perimeter of 2000 there are 744 triples, of which 140 are primitive
  Up to a perimeter of 2250 there are 858 triples, of which 156 are primitive
  Up to a perimeter of 2500 there are 969 triples, of which 175 are primitive

# Parent/child relationship method:
# http://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples#XI.

  Up to a perimeter of 500 there are 137 triples, of which 35 are primitive
  Up to a perimeter of 1000 there are 325 triples, of which 70 are primitive
  Up to a perimeter of 1500 there are 527 triples, of which 104 are primitive
  Up to a perimeter of 2000 there are 744 triples, of which 140 are primitive
  Up to a perimeter of 2500 there are 969 triples, of which 175 are primitive
  Up to a perimeter of 3000 there are 1204 triples, of which 211 are primitive
  Up to a perimeter of 3500 there are 1443 triples, of which 245 are primitive
  Up to a perimeter of 4000 there are 1687 triples, of which 280 are primitive
  Up to a perimeter of 4500 there are 1931 triples, of which 314 are primitive
  Up to a perimeter of 5000 there are 2184 triples, of which 349 are primitive
  Up to a perimeter of 5500 there are 2442 triples, of which 385 are primitive
  Up to a perimeter of 6000 there are 2701 triples, of which 422 are primitive
  Up to a perimeter of 6500 there are 2963 triples, of which 457 are primitive
  Up to a perimeter of 7000 there are 3224 triples, of which 492 are primitive
  Up to a perimeter of 7500 there are 3491 triples, of which 527 are primitive
  Up to a perimeter of 8000 there are 3763 triples, of which 560 are primitive
  Up to a perimeter of 8500 there are 4029 triples, of which 597 are primitive
  Up to a perimeter of 9000 there are 4304 triples, of which 631 are primitive
  Up to a perimeter of 9500 there are 4577 triples, of which 667 are primitive
  Up to a perimeter of 10000 there are 4858 triples, of which 703 are primitive
  Up to a perimeter of 10500 there are 5138 triples, of which 736 are primitive
  Up to a perimeter of 11000 there are 5414 triples, of which 770 are primitive
  Up to a perimeter of 11500 there are 5699 triples, of which 804 are primitive
  Up to a perimeter of 12000 there are 5980 triples, of which 839 are primitive
  Up to a perimeter of 12500 there are 6263 triples, of which 877 are primitive
  Up to a perimeter of 13000 there are 6559 triples, of which 913 are primitive
  Up to a perimeter of 13500 there are 6843 triples, of which 949 are primitive
  Up to a perimeter of 14000 there are 7129 triples, of which 983 are primitive
  Up to a perimeter of 14500 there are 7420 triples, of which 1019 are primitive
  Up to a perimeter of 15000 there are 7714 triples, of which 1055 are primitive
  Up to a perimeter of 15500 there are 8004 triples, of which 1089 are primitive
  Up to a perimeter of 16000 there are 8304 triples, of which 1127 are primitive
  Up to a perimeter of 16500 there are 8595 triples, of which 1159 are primitive
  Up to a perimeter of 17000 there are 8884 triples, of which 1192 are primitive
  Up to a perimeter of 17500 there are 9189 triples, of which 1228 are primitive
  Up to a perimeter of 18000 there are 9484 triples, of which 1264 are primitive
  Up to a perimeter of 18500 there are 9791 triples, of which 1301 are primitive
  Up to a perimeter of 19000 there are 10088 triples, of which 1336 are primitive
  Up to a perimeter of 19500 there are 10388 triples, of which 1373 are primitive
  Up to a perimeter of 20000 there are 10689 triples, of which 1408 are primitive
```

Barebone minimum for this task:
```Python
from sys import setrecursionlimit
setrecursionlimit(2000) # 2000 ought to be big enough for everybody

def triples(lim, a = 3, b = 4, c = 5):
    l = a + b + c
    if l > lim: return (0, 0)
    return reduce(lambda x, y: (x[0] + y[0], x[1] + y[1]), [
        (1, lim / l),
        triples(lim,  a - 2*b + 2*c,  2*a - b + 2*c,  2*a - 2*b + 3*c),
        triples(lim,  a + 2*b + 2*c,  2*a + b + 2*c,  2*a + 2*b + 3*c),
        triples(lim, -a + 2*b + 2*c, -2*a + b + 2*c, -2*a + 2*b + 3*c) ])

for peri in [10 ** e for e in range(1, 8)]:
    print peri, triples(peri)
```
Output:<lang>10 (0, 0)
100 (7, 17)
1000 (70, 325)
10000 (703, 4858)
100000 (7026, 64741)
1000000 (70229, 808950)
10000000 (702309, 9706567)
```



## Racket


```racket
#lang racket

#| Euclid's enumeration formula and counting is fast enough for extra credit.

 For maximum perimeter P₀, the primitive triples are enumerated by n,m with:

   1 ≤ n < m
   perimeter P(n, m) ≤ P₀ where P(n, m) = (m² - n²) + 2mn + (m² + n²) = 2m(m+n)
   m and n of different parity and coprime.

 Since n < m, a simple close non-tight bound on n is P(n, n) < P₀.
 For each of these the exact set of m's can be enumerated.

 Each primitive triple with perimeter p represents one triple for each kp ≤ P₀,
  of which there are floor(P₀/p) k's. |#

(define (P n m) (* 2 m (+ m n)))
(define (number-of-triples P₀)
  (for/fold ([primitive 0] [all 0])
    ([n (in-naturals 1)]
     #:break (>= (P n n) P₀))
    (for*/fold ([primitive′ primitive] [all′ all])
      ([m (in-naturals (+ n 1))]
       #:break (> (P n m) P₀)
       #:when (and (odd? (- m n)) (coprime? m n)))
      (values (+ primitive′ 1)
              (+ all′ (quotient P₀ (P n m)))))))


(define (print-results P₀)
  (define-values (primitive all) (number-of-triples P₀))
  (printf "~a ~a:\n  ~a, ~a.\n"
          "Number of Pythagorean triples and primitive triples with perimeter ≤"
          P₀
          all primitive))
(print-results 100)
(time (print-results (* 100 1000 1000)))

#|
   Number of Pythagorean triples and primitive triples with perimeter ≤ 100:
     17, 7.
   Number of Pythagorean triples and primitive triples with perimeter ≤ 100000000:
     113236940, 7023027.
   cpu time: 11976 real time: 12215 gc time: 2381
|#
```



## REXX


### using  GCD  for determinacy


```rexx
/*REXX program counts the number of  Pythagorean triples  that exist given a maximum    */
/*──────────────────── perimeter of  N, and also counts how many of them are primitives.*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 100                   /*Not specified?  Then use the default.*/
              do j=1  for N;   @.j= j*j;   end   /*pre-compute some squares.            */
N66= N * 2%3                                     /*calculate  2/3  of  N     (for a+b). */
T= 0;   P= 0                                     /*set the number of Triples, Primitives*/
              do a=3  to N%3                     /*limit  side  to 1/3 of the perimeter.*/
                 do b= a+1                       /*the triangle can't be  isosceles.    */
                 ab= a + b                       /*compute a partial perimeter (2 sides)*/
                 if ab>=N66       then iterate a /*is a+b≥66% perimeter? Try different A*/
                 aabb= @.a + @.b                 /*compute the sum of  a²+b²  (shortcut)*/
                    do c=b+1                     /*compute the value of the third side. */
                    if ab+c > N   then iterate a /*is a+b+c>perimeter ? Try different A.*/
                    if @.c >aabb  then iterate b /*is     c²  > a²+b² ? Try      "    B.*/
                    if @.c\==aabb then iterate   /*is     c² ¬= a²+b² ? Try      "    C.*/
                    T= T + 1                     /*eureka. We found a Pythagorean triple*/
                    P= P + (gcd(a, b)==1)        /*is this  triple  a primitive triple? */
                    end   /*c*/
                 end      /*b*/
              end         /*a*/
_= left('', 7)                                   /*for padding the output with 7 blanks.*/
say 'max perimeter ='    N   _    "Pythagorean triples ="    T    _    'primitives ='    P
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gcd: procedure; parse arg x,y;  do until y==0; parse value x//y y with y x; end;  return x
```

```txt

max perimeter = 100         Pythagorean triples = 17         primitives = 7

```

```txt

max perimeter = 1000         Pythagorean triples = 325         primitives = 70

```



### using single evenness for determinacy

This REXX version takes advantage that primitive Pythagorean triples must have one and only one   ''even''   number.

This REXX version is about   '''10%'''   faster than the 1<sup>st</sup> REXX version.

Non-primitive Pythagorean triples are generated after a primitive triple is found.

```rexx
/*REXX program counts the number of  Pythagorean triples  that exist given a maximum    */
/*──────────────────── perimeter of  N, and also counts how many of them are primitives.*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 100                   /*Not specified?  Then use the default.*/
@.= 0;        do j=1  for N;   @.j= j*j;   end   /*pre-compute some squares.            */
N66= N * 2%3                                     /*calculate  2/3  of  N     (for a+b). */
P= 0; T= 0;   do a=3  to N%3                     /*limit  side  to 1/3 of the perimeter.*/
              aEven= a//2==0                     /*set variable to  1   if  A  is even. */
                do b=a+1  by 1+aEven;  ab= a + b /*the triangle can't be isosceles.     */
                if ab>=N66       then iterate a  /*is a+b≥66% perimeter? Try different A*/
                aabb= @.a + @.b                  /*compute the sum of  a²+b²  (shortcut)*/
                  do c=b + 1                     /*compute the value of the third side. */
                  if aEven       then if c//2==0  then iterate  /*both A&C even? Skip it*/
                  if ab+c>n      then iterate a  /*a+b+c > perimeter? Try different  A. */
                  if @.c > aabb  then iterate b  /*is  c² >  a²+b² ?   "      "      B. */
                  if @.c\==aabb  then iterate    /*is  c² ¬= a²+b² ?   "      "      C. */
                  if @.a.b.c     then iterate    /*Is this a duplicate?  Then try again.*/
                  T= T + 1                       /*Eureka! We found a Pythagorean triple*/
                  P= P + 1                       /*count this also as a primitive triple*/
                    do m=2  while a*m+b*m+c*m<=N /*generate non-primitives Pythagoreans.*/
                    T= T + 1                     /*Eureka! We found a Pythagorean triple*/
                    am= a*m;   bm= b*m;  cm= c*m /*create some short-cut variable names.*/
                    @.am.bm.cm= 1                /*mark Pythagorean triangle as a triple*/
                    end   /*m*/
                  end     /*c*/
                end       /*b*/
              end         /*a*/                  /*stick a fork in it,  we're all done. */
_= left('', 7)                                   /*for padding the output with 7 blanks.*/
say 'max perimeter ='    N   _    "Pythagorean triples ="    T    _    'primitives ='    P
```

```txt

max perimeter = 10000         Pythagorean triples = 4858         primitives = 703

```



## Ring


```ring

size = 100
sum = 0
prime = 0
for i = 1 to size
   for j = i + 1 to size
       for k = 1 to size
           if pow(i,2) + pow(j,2) = pow(k,2) and (i+j+k) < 101
              if gcd(i,j) = 1 prime += 1 ok
              sum += 1
              see "" + i + " " + j + " " + k + nl ok
       next
   next
next
see "Total : " + sum + nl
see "Primitives : " + prime + nl

func gcd gcd, b
     while b
           c   = gcd
           gcd = b
           b   = c % b
     end
     return gcd

```

Output:

```txt

3 4 5
5 12 13
6 8 10
7 24 25
8 15 17
9 12 15
9 40 41
10 24 26
12 16 20
12 35 37
15 20 25
15 36 39
16 30 34
18 24 30
20 21 29
21 28 35
24 32 40
Total : 17
Primitives : 7

```



## Ruby

```ruby
class PythagoranTriplesCounter
  def initialize(limit)
    @limit = limit
    @total = 0
    @primitives = 0
    generate_triples(3, 4, 5)
  end
  attr_reader :total, :primitives

  private
  def generate_triples(a, b, c)
    perim = a + b + c
    return if perim > @limit

    @primitives += 1
    @total += @limit / perim

    generate_triples( a-2*b+2*c, 2*a-b+2*c, 2*a-2*b+3*c)
    generate_triples( a+2*b+2*c, 2*a+b+2*c, 2*a+2*b+3*c)
    generate_triples(-a+2*b+2*c,-2*a+b+2*c,-2*a+2*b+3*c)
  end
end

perim = 10
while perim <= 100_000_000
  c = PythagoranTriplesCounter.new perim
  p [perim, c.total, c.primitives]
  perim *= 10
end
```


output

```txt
[10, 0, 0]
[100, 17, 7]
[1000, 325, 70]
[10000, 4858, 703]
[100000, 64741, 7026]
[1000000, 808950, 70229]
[10000000, 9706567, 702309]
[100000000, 113236940, 7023027]
```



## Rust


```rust
use std::thread;

fn f1 (a : u64, b : u64, c : u64, d : u64) -> u64 {
    let mut primitive_count = 0;
    for triangle in [[a - 2*b + 2*c, 2*a - b + 2*c, 2*a - 2*b + 3*c],
                     [a + 2*b + 2*c, 2*a + b + 2*c, 2*a + 2*b + 3*c],
                     [2*b + 2*c - a, b + 2*c - 2*a, 2*b + 3*c - 2*a]] .iter() {
        let l  = triangle[0] + triangle[1] + triangle[2];
        if l > d { continue; }
        primitive_count +=  1 + f1(triangle[0], triangle[1], triangle[2], d);
    }
    primitive_count
}

fn f2 (a : u64, b : u64, c : u64, d : u64) -> u64 {
    let mut triplet_count = 0;
    for triangle in [[a - 2*b + 2*c, 2*a - b + 2*c, 2*a - 2*b + 3*c],
                     [a + 2*b + 2*c, 2*a + b + 2*c, 2*a + 2*b + 3*c],
                     [2*b + 2*c - a, b + 2*c - 2*a, 2*b + 3*c - 2*a]] .iter() {
        let l  = triangle[0] + triangle[1] + triangle[2];
        if l > d { continue; }
        triplet_count +=  (d/l) + f2(triangle[0], triangle[1], triangle[2], d);
    }
    triplet_count
}

fn main () {
    let new_th_1 = thread::Builder::new().stack_size(32 * 1024 * 1024).spawn (move || {
        let mut i = 100;
        while i <= 100_000_000_000 {
            println!(" Primitive triples below {} : {}", i, f1(3, 4, 5, i) + 1);
            i *= 10;
        }
    }).unwrap();

    let new_th_2 =thread::Builder::new().stack_size(32 * 1024 * 1024).spawn (move || {
        let mut i = 100;
        while i <= 100_000_000_000 {
            println!(" Triples below {} : {}", i, f2(3, 4, 5, i) + i/12);
            i *= 10;
        }
    }).unwrap();

    new_th_1.join().unwrap();
    new_th_2.join().unwrap();
}
```

```txt
 Primitive triples below 100 : 7
 Triples below 100 : 17
 Primitive triples below 1000 : 70
 Triples below 1000 : 325
 Primitive triples below 10000 : 703
 Triples below 10000 : 4858
 Primitive triples below 100000 : 7026
 Triples below 100000 : 64741
 Primitive triples below 1000000 : 70229
 Triples below 1000000 : 808950
 Primitive triples below 10000000 : 702309
 Triples below 10000000 : 9706567
 Primitive triples below 100000000 : 7023027
 Triples below 100000000 : 113236940
 Primitive triples below 1000000000 : 70230484
 Triples below 1000000000 : 1294080089
 Primitive triples below 10000000000 : 702304875
 Triples below 10000000000 : 14557915466
 Primitive triples below 100000000000 : 7023049293
 Triples below 100000000000 : 161750315680

real	2m22.676s
user	3m39.239s
sys	0m0.024s
```


## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/CAz60TW/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/soOLJ673Q82l78OCgIx4oQ Scastie (remote JVM)].

```Scala
object PythagoreanTriples extends App {

  println("               Limit Primatives          All")

  for {e <- 2 to 7
       limit = math.pow(10, e).longValue()
  } {
    var primCount, tripCount = 0

    def parChild(a: BigInt, b: BigInt, c: BigInt): Unit = {
      val perim = a + b + c
      val (a2, b2, c2, c3) = (2 * a, 2 * b, 2 * c, 3 * c)
      if (limit >= perim) {
        primCount += 1
        tripCount += (limit / perim).toInt
        parChild(a - b2 + c2, a2 - b + c2, a2 - b2 + c3)
        parChild(a + b2 + c2, a2 + b + c2, a2 + b2 + c3)
        parChild(-a + b2 + c2, -a2 + b + c2, -a2 + b2 + c3)
      }
    }

    parChild(BigInt(3), BigInt(4), BigInt(5))
    println(f"a + b + c <= ${limit.toFloat}%3.1e  $primCount%9d $tripCount%12d")
  }
}
```



## Scheme

```Scheme
(use srfi-42)

(define (py perim)
  (define prim 0)
  (values
    (sum-ec
      (: c perim) (: b c) (: a b)
      (if (and (<= (+ a b c) perim)
               (= (square c) (+ (square b) (square a)))))
      (begin (when (= 1 (gcd a b)) (inc! prim)))
      1)
    prim))
```

<b>Testing:</b>

```txt

gosh> (py 100)
17
7

```




## Scratch

Scratch is a visual programming language. Click the link, then "see inside" to see the code.

https://scratch.mit.edu/projects/79066598/

Output: 17 Pythagorean triples with a perimeter less than 100, 7 of which are primitive.


## Seed7

The example below uses [http://seed7.sourceforge.net/libraries/bigint.htm bigInteger] numbers:


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

var bigInteger: total is 0_;
var bigInteger: prim is 0_;
var bigInteger: max_peri is 10_;

const proc: new_tri (in bigInteger: a, in bigInteger: b, in bigInteger: c) is func
  local
    var bigInteger: p is 0_;
  begin
    p := a + b + c;
    if p <= max_peri then
      incr(prim);
      total +:= max_peri div p;
      new_tri( a - 2_*b + 2_*c,  2_*a - b + 2_*c,  2_*a - 2_*b + 3_*c);
      new_tri( a + 2_*b + 2_*c,  2_*a + b + 2_*c,  2_*a + 2_*b + 3_*c);
      new_tri(-a + 2_*b + 2_*c, -2_*a + b + 2_*c, -2_*a + 2_*b + 3_*c);
    end if;
  end func;

const proc: main is func
  begin
    while max_peri <= 100000000_ do
      total := 0_;
      prim := 0_;
      new_tri(3_, 4_, 5_);
      writeln("Up to " <& max_peri <& ": " <& total <& " triples, " <& prim <& " primitives.");
      max_peri *:= 10_;
    end while;
  end func;
```


Output:

```txt

Up to 10: 0 triples, 0 primitives.
Up to 100: 17 triples, 7 primitives.
Up to 1000: 325 triples, 70 primitives.
Up to 10000: 4858 triples, 703 primitives.
Up to 100000: 64741 triples, 7026 primitives.
Up to 1000000: 808950 triples, 70229 primitives.
Up to 10000000: 9706567 triples, 702309 primitives.
Up to 100000000: 113236940 triples, 7023027 primitives.

```



## Sidef

```ruby
func triples(limit) {
    var primitive = 0
    var civilized = 0

    func oyako(a, b, c) {
        (var perim = a+b+c) > limit || (
            primitive++
            civilized += int(limit / perim)
            oyako( a - 2*b + 2*c,  2*a - b + 2*c,  2*a - 2*b + 3*c)
            oyako( a + 2*b + 2*c,  2*a + b + 2*c,  2*a + 2*b + 3*c)
            oyako(-a + 2*b + 2*c, -2*a + b + 2*c, -2*a + 2*b + 3*c)
        )
    }

    oyako(3,4,5)
    "#{limit} => (#{primitive} #{civilized})"
}
 
for n (1..Inf) {
    say triples(10**n)
}
```


```txt

10 => (0 0)
100 => (7 17)
1000 => (70 325)
10000 => (703 4858)
100000 => (7026 64741)
1000000 => (70229 808950)
^C

```



## Swift

```Swift
var total = 0
var prim = 0
var maxPeri = 100

func newTri(s0:Int, _ s1:Int, _ s2: Int) -> () {

    let p = s0 + s1 + s2
    if p <= maxPeri {
        prim += 1
        total += maxPeri / p
        newTri( s0 + 2*(-s1+s2), 2*( s0+s2) - s1, 2*( s0-s1+s2) + s2)
        newTri( s0 + 2*( s1+s2), 2*( s0+s2) + s1, 2*( s0+s1+s2) + s2)
        newTri(-s0 + 2*( s1+s2), 2*(-s0+s2) + s1, 2*(-s0+s1+s2) + s2)
    }
}

while maxPeri <= 100_000_000 {
    prim = 0
    total = 0
    newTri(3, 4, 5)
    print("Up to \(maxPeri) : \(total) triples \( prim) primitives.")
    maxPeri *= 10
}
```


```txt

Up to 100 : 17 triples 7 primitives.
Up to 1000 : 325 triples 70 primitives.
Up to 10000 : 4858 triples 703 primitives.
Up to 100000 : 64741 triples 7026 primitives.
Up to 1000000 : 808950 triples 70229 primitives.
Up to 10000000 : 9706567 triples 702309 primitives.
Up to 100000000 : 113236940 triples 7023027 primitives.

```



## Tcl

Using the efficient method based off the Wikipedia article:
<!--There's no technical reason to limit the code to just these values, but generation does get progressively slower with larger maximum perimiters. 10M is about as much as I have patience for; I'm generally impatient! -->

```tcl
proc countPythagoreanTriples {limit} {
    lappend q 3 4 5
    set idx [set count [set prim 0]]
    while {$idx < [llength $q]} {
    set a [lindex $q $idx]
    set b [lindex $q [incr idx]]
    set c [lindex $q [incr idx]]
    incr idx
    if {$a + $b + $c <= $limit} {
        incr prim
        for {set i 1} {$i*$a+$i*$b+$i*$c <= $limit} {incr i} {
        incr count
        }
        lappend q \
        [expr {$a + 2*($c-$b)}] [expr {2*($a+$c) - $b}] [expr {2*($a-$b) + 3*$c}] \
        [expr {$a + 2*($b+$c)}] [expr {2*($a+$c) + $b}] [expr {2*($a+$b) + 3*$c}] \
        [expr {2*($b+$c) - $a}] [expr {2*($c-$a) + $b}] [expr {2*($b-$a) + 3*$c}]
    }
    }
    return [list $count $prim]
}
for {set i 10} {$i <= 10000000} {set i [expr {$i*10}]} {
    lassign [countPythagoreanTriples $i] count primitive
    puts "perimeter limit $i => $count triples, $primitive primitive"
}
```

Output:

```txt

perimeter limit 10 => 0 triples, 0 primitive
perimeter limit 100 => 17 triples, 7 primitive
perimeter limit 1000 => 325 triples, 70 primitive
perimeter limit 10000 => 4858 triples, 703 primitive
perimeter limit 100000 => 64741 triples, 7026 primitive
perimeter limit 1000000 => 808950 triples, 70229 primitive
perimeter limit 10000000 => 9706567 triples, 702309 primitive

```



## VBA

```vb
Dim total As Variant, prim As Variant, maxPeri As Variant
Private Sub newTri(s0 As Variant, s1 As Variant, s2 As Variant)
    Dim p As Variant
    p = CDec(s0) + CDec(s1) + CDec(s2)
    If p <= maxPeri Then
        prim = prim + 1
        total = total + maxPeri \ p
        newTri s0 + 2 * (-s1 + s2), 2 * (s0 + s2) - s1, 2 * (s0 - s1 + s2) + s2
        newTri s0 + 2 * (s1 + s2), 2 * (s0 + s2) + s1, 2 * (s0 + s1 + s2) + s2
        newTri -s0 + 2 * (s1 + s2), 2 * (-s0 + s2) + s1, 2 * (-s0 + s1 + s2) + s2
      End If
End Sub
Public Sub Program_PythagoreanTriples()
    maxPeri = CDec(100)
    Do While maxPeri <= 10000000#
        prim = CDec(0)
        total = CDec(0)
        newTri 3, 4, 5
        Debug.Print "Up to "; maxPeri; ": "; total; " triples, "; prim; " primitives."
        maxPeri = maxPeri * 10
    Loop
End Sub
```
```txt
Up to  100 :  17  triples,  7  primitives.
Up to  1000 :  325  triples,  70  primitives.
Up to  10000 :  4858  triples,  703  primitives.
Up to  100000 :  64741  triples,  7026  primitives.
Up to  1000000 :  808950  triples,  70229  primitives.
Up to  10000000 :  9706567  triples,  702309  primitives.

```


## VBScript

```vb

For i=1 To 8
	WScript.StdOut.WriteLine triples(10^i)
Next

Function triples(pmax)
	prim=0 : count=0 : nmax=Sqr(pmax)/2 : n=1
	Do While n <= nmax
		m=n+1 : p=2*m*(m+n)
		Do While p <= pmax
			If gcd(m,n)=1 Then
				prim=prim+1
				count=count+Int(pmax/p)
			End If
			m=m+2
			p=2*m*(m+n)
		Loop
		n=n+1
	Loop
	triples = "Max Perimeter: " & pmax &_
				", Total: " & count &_
				", Primitive: " & prim
End Function

Function gcd(a,b)
	c = a : d = b
	Do
		If c Mod d > 0 Then
			e = c Mod d
			c = d
			d = e
		Else
			gcd = d
			Exit Do
		End If
	Loop
End Function

```



## Visual Basic

```vb
Option Explicit

Dim total As Long, prim As Long, maxPeri As Long

Public Sub NewTri(ByVal s0 As Long, ByVal s1 As Long, ByVal s2 As Long)
Dim p As Long, x1 As Long, x2 As Long
    p = s0 + s1 + s2
    If p <= maxPeri Then
        prim = prim + 1
        total = total + maxPeri \ p
        x1 = s0 + s2
        x2 = s1 + s2
        NewTri s0 + 2 * (-s1 + s2), 2 * x1 - s1, 2 * (x1 - s1) + s2
        NewTri s0 + 2 * x2, 2 * x1 + s1, 2 * (x1 + s1) + s2
        NewTri -s0 + 2 * x2, 2 * (-s0 + s2) + s1, 2 * (-s0 + x2) + s2
    End If
End Sub

Public Sub Main()
    maxPeri = 100
    Do While maxPeri <= 10& ^ 8
        prim = 0
        total = 0
        NewTri 3, 4, 5
        Debug.Print "Up to "; maxPeri; ": "; total; " triples, "; prim; " primitives."
        maxPeri = maxPeri * 10
    Loop
End Sub
```

```txt
Up to  100 :  17  triples,  7  primitives.
Up to  1000 :  325  triples,  70  primitives.
Up to  10000 :  4858  triples,  703  primitives.
Up to  100000 :  64741  triples,  7026  primitives.
Up to  1000000 :  808950  triples,  70229  primitives.
Up to  10000000 :  9706567  triples,  702309  primitives.
Up to  100000000 :  113236940  triples,  7023027  primitives.
```



## zkl

```zkl
fcn tri(lim,a=3,b=4,c=5){
    p:=a + b + c;
    if(p>lim) return(0,0);
    T(1,lim/p).zipWith('+,
       tri(lim,  a - 2*b + 2*c,  2*a - b + 2*c,  2*a - 2*b + 3*c),
       tri(lim,  a + 2*b + 2*c,  2*a + b + 2*c,  2*a + 2*b + 3*c),
       tri(lim, -a + 2*b + 2*c, -2*a + b + 2*c, -2*a + 2*b + 3*c)
    );
}
```


```zkl
n:=10; do(10){ println("%,d: %s".fmt(n,tri(n).reverse())); n*=10; }
```

```txt
10: L(0,0)
100: L(17,7)
1,000: L(325,70)
10,000: L(4858,703)
100,000: L(64741,7026)
1,000,000: L(808950,70229)
10,000,000: L(9706567,702309)
VM#1 caught this unhandled exception:
   AssertionError : That is one big stack, infinite recursion?
Stack trace for VM#1 ():
   jj.tri addr:112  args(4) reg(1) R
   <repeats 3578 times>
   jj.__constructor addr:23  args(0) reg(2) R
   ...

```

Max stack size is arbitrary but not adjustable.

## ZX Spectrum Basic

ZX Spectrum: 8 bit microprocessor 3.5 Mhz doing all the work.
In an effort to get some decent speed the program is made to be as fast as it can.

It takes about 90 seconds for limit = 10 000 and 17 minutes for limit=100 000 and 3.5 hours for limit = 1000 000.

To set the limits.
Set in line nr: 1 L to the starting limit.
Set in line nr: 11 IF L<=(last limit to calculate)

Ex. start as limit 100 and end on limit 1000.
Set in line nr: 1 LET L=100.
Set in line nr: 11 IF L<=1000 THEN GO TO 2


```zxbasic
   1 LET Y=0: LET X=0: LET Z=0: LET V=0: LET U=0: LET L=10: LET T=0: LET P=0: LET N=4: LET M=0: PRINT "limit   trip.   prim."
   2 FOR U=2 TO INT (SQR (L/2)): LET Y=U-INT (U/2)*2: LET N=N+4: LET M=U*U*2: IF Y=0 THEN LET M=M-U-U
   3 FOR V=1+Y TO U-1 STEP 2: LET M=M+N: LET X=U: LET Y=V
   4 LET Z=Y: LET Y=X-INT (X/Y)*Y: LET X=Z: IF Y<>0 THEN GO TO 4
   5 IF X>1 THEN GO TO 8
   6 IF M>L THEN GO TO 9
   7 LET P=P+1: LET T=T+INT (L/M)
   8 NEXT V
   9 NEXT U
  10 PRINT L;TAB 8;T;TAB 16;P
  11 LET N=4: LET T=0: LET P=0: LET L=L*10: IF L<=100000 THEN GO TO 2
```

```txt
limit   trip.   prim.
10      0       0
100     17      7
1000    325     70
10000   4858    703
100000  64741   7026
1000000 808950  70229
```



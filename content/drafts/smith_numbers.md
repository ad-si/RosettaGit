+++
title = "Smith numbers"
description = ""
date = 2019-09-13T15:31:18Z
aliases = []
[extra]
id = 20737
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[wp:Smith numbers|Smith numbers]] are numbers such that the [[Sum_digits_of_an_integer|sum of the decimal digits of the integers]] that make up that number is the same as the sum of the decimal digits of its prime factors excluding 1.

By definition, all primes are ''excluded'' as they (naturally) satisfy this condition!

Smith numbers are also known as   ''joke''   numbers.


;Example
Using the number '''166'''

Find the prime factors of '''166''' which are: '''2''' x '''83'''

Then, take those two prime factors and sum all their decimal digits: '''2 + 8 + 3''' which is '''13'''

Then, take the decimal digits of '''166''' and add their decimal digits: '''1 + 6 + 6''' which is '''13'''

Therefore, the number '''166''' is a Smith number.


;Task
Write a program to find all Smith numbers ''below'' 10000.


;See also
* from Wikipedia:   [[https://en.wikipedia.org/wiki/Smith_number Smith number]].
* from MathWorld:   [[http://mathworld.wolfram.com/SmithNumber.html Smith number]].
* from OEIS A6753:   [[https://oeis.org/A006753 OEIS sequence A6753]].
* from OEIS A104170:   [[https://oeis.org/A104170 Number of Smith numbers below 10^n]].
* from The Prime pages:   [[http://primes.utm.edu/glossary/xpage/SmithNumber.html Smith numbers]].





## 360 Assembly

{{trans|Rexx}}

```360asm
*        Smith numbers -           02/05/2017
SMITHNUM CSECT
         USING  SMITHNUM,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R10,PG             pgi=0
         LA     R6,4               i=4
       DO WHILE=(C,R6,LE,N)        do i=4 to n
         LR     R1,R6                i
         BAL    R14,SUMD             call sumd(i)
         ST     R0,SS                ss=sumd(i)
         LR     R1,R6                i
         BAL    R14,SUMFACTR         call sumfactr(i)
       IF C,R0,EQ,SS THEN            if sumd(i)=sumfactr(i) then
         L      R2,NN                  nn
         LA     R2,1(R2)               nn+1
         ST     R2,NN                  nn=nn+1
         XDECO  R6,XDEC                i
         MVC    0(5,R10),XDEC+7        output i
         LA     R10,5(R10)             pgi+=5
         L      R4,IPG                 ipg
         LA     R4,1(R4)               ipg+1
         ST     R4,IPG                 ipg=ipg+1
       IF C,R4,EQ,=F'16' THEN          if ipg=16 then
         XPRNT  PG,80                    print buffer
         MVC    PG,=CL80' '              clear buffer
         LA     R10,PG                   pgi=0
         MVC    IPG,=F'0'                ipg=0
       ENDIF    ,                      endif
       ENDIF    ,                    endif
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R4,IPG             ipg
       IF LTR,R4,NZ,R4 THEN        if ipg<>0 then
         XPRNT  PG,80                print buffer
       ENDIF    ,                  endif
         L      R1,NN              nn
         XDECO  R1,XDEC            edit nn
         MVC    PGT(4),XDEC+8      output nn
         L      R1,N               n
         XDECO  R1,XDEC            edit n
         MVC    PGT+28(5),XDEC+7   output n
         XPRNT  PGT,80             print
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
*------- ----   ----------------------------------------
SUMD     EQU    *                  sumd(x)
         SR     R0,R0              s=0
       DO WHILE=(LTR,R1,NZ,R1)     do while x<>0
         LR     R2,R1                x
         SRDA   R2,32                ~
         D      R2,=F'10'            x/10
         LR     R1,R3                x=x/10
         AR     R0,R2                s=s+x//10
       ENDDO    ,                  enddo while
         BR     R14                return s
*------- ----   ----------------------------------------
SUMFACTR EQU    *                  sumfactr(z)
         ST     R14,SAVER14        store r14
         ST     R1,ZZ              z
         SR     R8,R8              m=0
         SR     R9,R9              f=0
         L      R4,ZZ              z
         SRDA   R4,32              ~
         D      R4,=F'2'           z/2
       DO WHILE=(LTR,R4,Z,R4)      do while z//2=0
         LA     R8,2(R8)             m=m+2
         LA     R9,1(R9)             f=f+1
         L      R5,ZZ                z
         SRA    R5,1                 z/2
         ST     R5,ZZ                z=z/2
         LA     R4,0                 z
         D      R4,=F'2'             z/2
       ENDDO    ,                  enddo while
         L      R4,ZZ              z
         SRDA   R4,32              ~
         D      R4,=F'3'           z/3
       DO WHILE=(LTR,R4,Z,R4)      do while z//3=0
         LA     R8,3(R8)             m=m+3
         LA     R9,1(R9)             f=f+1
         L      R4,ZZ                z
         SRDA   R4,32                ~
         D      R4,=F'3'             z/3
         ST     R5,ZZ                z=z/3
         LA     R4,0                 z
         D      R4,=F'3'             z/3
       ENDDO    ,                  enddo while
         LA     R7,5               do j=5 by 2 while j<=z and j*j<=n
WHILEJ   C      R7,ZZ                if j>z
         BH     EWHILEJ              then leave while
         LR     R5,R7                j
         MR     R4,R7                *j
         C      R5,N                 if j*j>n
         BH     EWHILEJ              then leave while
         LR     R4,R7                j
         SRDA   R4,32                ~
         D      R4,=F'3'             j/3
         LTR    R4,R4                if j//3=0
         BZ     ITERJ                then goto iterj
         L      R4,ZZ                z
         SRDA   R4,32                ~
         DR     R4,R7                z/j
       DO WHILE=(LTR,R4,Z,R4)        do while z//j=0
         LA     R9,1(R9)               f=f+1
         LR     R1,R7                  j
         BAL    R14,SUMD               call sumd(j)
         AR     R8,R0                  m=m+sumd(j)
         L      R4,ZZ                  z
         SRDA   R4,32                  ~
         DR     R4,R7                  z/j
         ST     R5,ZZ                  z=z/j
         LA     R4,0                   ~
         DR     R4,R7                  z/j
       ENDDO    ,                    enddo while
ITERJ    LA     R7,2(R7)             j+=2
         B      WHILEJ             enddo
EWHILEJ  L      R4,ZZ              z
       IF C,R4,NE,=F'1' THEN       if z<>1 then
         LA     R9,1(R9)             f=f+1
         L      R1,ZZ                z
         BAL    R14,SUMD             call sumd(z)
         AR     R8,R0                m=m+sumd(z)
       ENDIF    ,                  endif
       IF C,R9,LT,=F'2' THEN       if f<2 then
         SR     R8,R8                mm=0
       ENDIF    ,                  endif
         LR     R0,R8              return m
         L      R14,SAVER14        restore r14
         BR     R14                return
SAVER14  DS     A                  save r14
*        ----   ----------------------------------------
N        DC     F'10000'           n
NN       DC     F'0'               nn
IPG      DC     F'0'               ipg
SS       DS     F                  ss
ZZ       DS     F                  z
PG       DC     CL80' '            buffer
PGT      DC     CL80'xxxx smith numbers found <= xxxxx'
XDEC     DS     CL12               temp
         YREGS
         END    SMITHNUM
```

{{out}}

```txt

    4   22   27   58   85   94  121  166  202  265  274  319  346  355  378  382
  391  438  454  483  517  526  535  562  576  588  627  634  636  645  648  654
  663  666  690  706  728  729  762  778  825  852  861  895  913  915  922  958
  985 1086 1111 1165 1219 1255 1282 1284 1376 1449 1507 1581 1626 1633 1642 1678
 1736 1755 1776 1795 1822 1842 1858 1872 1881 1894 1903 1908 1921 1935 1952 1962
 1966 2038 2067 2079 2155 2173 2182 2218 2227 2265 2286 2326 2362 2366 2373 2409
 2434 2461 2475 2484 2515 2556 2576 2578 2583 2605 2614 2679 2688 2722 2745 2751
 2785 2839 2888 2902 2911 2934 2944 2958 2964 2965 2970 2974 3046 3091 3138 3168
 3174 3226 3246 3258 3294 3345 3366 3390 3442 3505 3564 3595 3615 3622 3649 3663
 3690 3694 3802 3852 3864 3865 3930 3946 3973 4054 4126 4162 4173 4185 4189 4191
 4198 4209 4279 4306 4369 4414 4428 4464 4472 4557 4592 4594 4702 4743 4765 4788
 4794 4832 4855 4880 4918 4954 4959 4960 4974 4981 5062 5071 5088 5098 5172 5242
 5248 5253 5269 5298 5305 5386 5388 5397 5422 5458 5485 5526 5539 5602 5638 5642
 5674 5772 5818 5854 5874 5915 5926 5935 5936 5946 5998 6036 6054 6084 6096 6115
 6171 6178 6187 6188 6252 6259 6295 6315 6344 6385 6439 6457 6502 6531 6567 6583
 6585 6603 6684 6693 6702 6718 6760 6816 6835 6855 6880 6934 6981 7026 7051 7062
 7068 7078 7089 7119 7136 7186 7195 7227 7249 7287 7339 7402 7438 7447 7465 7503
 7627 7674 7683 7695 7712 7726 7762 7764 7782 7784 7809 7824 7834 7915 7952 7978
 8005 8014 8023 8073 8077 8095 8149 8154 8158 8185 8196 8253 8257 8277 8307 8347
 8372 8412 8421 8466 8518 8545 8568 8628 8653 8680 8736 8754 8766 8790 8792 8851
 8864 8874 8883 8901 8914 9015 9031 9036 9094 9166 9184 9193 9229 9274 9276 9285
 9294 9296 9301 9330 9346 9355 9382 9386 9387 9396 9414 9427 9483 9522 9535 9571
 9598 9633 9634 9639 9648 9657 9684 9708 9717 9735 9742 9760 9778 9840 9843 9849
 9861 9880 9895 9924 9942 9968 9975 9985
 376 smith numbers found <= 10000

```



## Ada

{{works with|Ada|2012}}

```ada

with Ada.Text_IO;

procedure smith is
  type Vector is array (natural range <>) of Positive;
  empty_vector : constant Vector(1..0):= (others=>1);

  function digits_sum (n : Positive) return Positive is
  (if n < 10 then n else n mod 10 + digits_sum (n / 10));

  function prime_factors (n : Positive; d : Positive := 2) return Vector is
  (if n = 1 then empty_vector elsif n mod d = 0 then prime_factors (n / d, d) & d
   else prime_factors (n, d + (if d=2 then 1 else 2)));

  function vector_digits_sum (v : Vector) return Natural is
  (if v'Length = 0 then 0 else digits_sum (v(v'First)) + vector_digits_sum (v(v'First+1..v'Last)));

begin
  for n in 1..10000 loop
    declare
      primes : Vector := prime_factors (n);
    begin
      if  primes'Length > 1 and then vector_digits_sum (primes) = digits_sum (n) then
        Ada.Text_IO.put (n'img);
      end if;
    end;
  end loop;
end smith;

```



## ALGOL 68


```algol68
# sieve of Eratosthene: sets s[i] to TRUE if i is prime, FALSE otherwise #
PROC sieve = ( REF[]BOOL s )VOID:
     BEGIN
        # start with everything flagged as prime                             #
        FOR i TO UPB s DO s[ i ] := TRUE OD;
        # sieve out the non-primes                                           #
        s[ 1 ] := FALSE;
        FOR i FROM 2 TO ENTIER sqrt( UPB s ) DO
            IF s[ i ] THEN FOR p FROM i * i BY i TO UPB s DO s[ p ] := FALSE OD FI
        OD
     END # sieve # ;

# construct a sieve of primes up to the maximum number required for the task #
INT max number = 10 000;
[ 1 : max number ]BOOL is prime;
sieve( is prime );

# returns the sum of the digits of n                                         #
OP DIGITSUM = ( INT n )INT:
   BEGIN
       INT sum  := 0;
       INT rest := ABS n;
       WHILE rest > 0 DO
           sum +:= rest MOD 10;
           rest OVERAB 10
       OD;
       sum
   END # DIGITSUM # ;

# returns TRUE if n is a Smith number, FALSE otherwise                       #
# n must be between 1 and max number                                         #
PROC is smith = ( INT n )BOOL:
     IF is prime[ ABS n ] THEN
         # primes are not Smith numbers                                      #
         FALSE
     ELSE
         # find the factors of n and sum the digits of the factors           #
         INT rest             := ABS n;
         INT factor digit sum := 0;
         INT factor           := 2;
         WHILE factor < max number AND rest > 1 DO
             IF NOT is prime[ factor ] THEN
                 # factor isn't a prime                                      #
                 factor +:= 1
             ELSE
                 # factor is a prime                                         #
                 IF rest MOD factor /= 0 THEN
                     # factor isn't a factor of n                            #
                     factor +:= 1
                 ELSE
                     # factor is a factor of n                               #
                     rest OVERAB factor;
                     factor digit sum +:= DIGITSUM factor
                 FI
             FI
         OD;
         ( factor digit sum = DIGITSUM n )
     FI # is smith # ;

# print all the Smith numbers below the maximum required                     #
INT smith count := 0;
FOR n TO max number - 1 DO
    IF is smith( n ) THEN
        # have a smith number #
        print( ( whole( n, -7 ) ) );
        smith count +:= 1;
        IF smith count MOD 10 = 0 THEN
            print( ( newline ) )
        FI
    FI
OD;
print( ( newline, "THere are ", whole( smith count, -7 ), " Smith numbers below ", whole( max number, -7 ), newline ) )

```

{{out}}

```txt

      4     22     27     58     85     94    121    166    202    265
    274    319    346    355    378    382    391    438    454    483
    ...
   9717   9735   9742   9760   9778   9840   9843   9849   9861   9880
   9895   9924   9942   9968   9975   9985
THere are     376 Smith numbers below   10000

```


## AWK


```AWK

# syntax: GAWK -f SMITH_NUMBERS.AWK
# converted from C
BEGIN {
    limit = 10000
    printf("Smith Numbers < %d:\n",limit)
    for (a=4; a<limit; a++) {
      num_factors = num_prime_factors(a)
      if (num_factors < 2) {
        continue
      }
      prime_factors(a)
      if (sum_digits(a) == sum_factors(num_factors)) {
        printf("%4d ",a)
        if (++cr % 16 == 0) {
          printf("\n")
        }
      }
      delete arr
    }
    printf("\n")
    exit(0)
}
function num_prime_factors(x,  p,pf) {
    p = 2
    pf = 0
    if (x == 1) {
      return(1)
    }
    while (1) {
      if (!(x % p)) {
        pf++
        x = int(x/p)
        if (x == 1) {
          return(pf)
        }
      }
      else {
        p++
      }
    }
}
function prime_factors(x,  p,pf) {
    p = 2
    pf = 0
    if (x == 1) {
      arr[pf] = 1
    }
    else {
      while (1) {
        if (!(x % p)) {
          arr[pf++] = p
          x = int(x/p)
          if (x == 1) {
            return
          }
        }
        else {
          p++
        }
      }
    }
}
function sum_digits(x,  sum,y) {
    while (x) {
      y = x % 10
      sum += y
      x = int(x/10)
    }
    return(sum)
}
function sum_factors(x,  a,sum) {
    sum = 0
    for (a=0; a<x; a++) {
      sum += sum_digits(arr[a])
    }
    return(sum)
}

```

{{out}}

```txt

Smith Numbers < 10000:
   4   22   27   58   85   94  121  166  202  265  274  319  346  355  378  382
 391  438  454  483  517  526  535  562  576  588  627  634  636  645  648  654
 663  666  690  706  728  729  762  778  825  852  861  895  913  915  922  958
 985 1086 1111 1165 1219 1255 1282 1284 1376 1449 1507 1581 1626 1633 1642 1678
1736 1755 1776 1795 1822 1842 1858 1872 1881 1894 1903 1908 1921 1935 1952 1962
1966 2038 2067 2079 2155 2173 2182 2218 2227 2265 2286 2326 2362 2366 2373 2409
2434 2461 2475 2484 2515 2556 2576 2578 2583 2605 2614 2679 2688 2722 2745 2751
2785 2839 2888 2902 2911 2934 2944 2958 2964 2965 2970 2974 3046 3091 3138 3168
3174 3226 3246 3258 3294 3345 3366 3390 3442 3505 3564 3595 3615 3622 3649 3663
3690 3694 3802 3852 3864 3865 3930 3946 3973 4054 4126 4162 4173 4185 4189 4191
4198 4209 4279 4306 4369 4414 4428 4464 4472 4557 4592 4594 4702 4743 4765 4788
4794 4832 4855 4880 4918 4954 4959 4960 4974 4981 5062 5071 5088 5098 5172 5242
5248 5253 5269 5298 5305 5386 5388 5397 5422 5458 5485 5526 5539 5602 5638 5642
5674 5772 5818 5854 5874 5915 5926 5935 5936 5946 5998 6036 6054 6084 6096 6115
6171 6178 6187 6188 6252 6259 6295 6315 6344 6385 6439 6457 6502 6531 6567 6583
6585 6603 6684 6693 6702 6718 6760 6816 6835 6855 6880 6934 6981 7026 7051 7062
7068 7078 7089 7119 7136 7186 7195 7227 7249 7287 7339 7402 7438 7447 7465 7503
7627 7674 7683 7695 7712 7726 7762 7764 7782 7784 7809 7824 7834 7915 7952 7978
8005 8014 8023 8073 8077 8095 8149 8154 8158 8185 8196 8253 8257 8277 8307 8347
8372 8412 8421 8466 8518 8545 8568 8628 8653 8680 8736 8754 8766 8790 8792 8851
8864 8874 8883 8901 8914 9015 9031 9036 9094 9166 9184 9193 9229 9274 9276 9285
9294 9296 9301 9330 9346 9355 9382 9386 9387 9396 9414 9427 9483 9522 9535 9571
9598 9633 9634 9639 9648 9657 9684 9708 9717 9735 9742 9760 9778 9840 9843 9849
9861 9880 9895 9924 9942 9968 9975 9985

```



## C

{{trans|C++}}

```c

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

int numPrimeFactors(unsigned x) {
    unsigned p = 2;
    int pf = 0;
    if (x == 1)
        return 1;
    else {
        while (true) {
            if (!(x % p)) {
                pf++;
                x /= p;
                if (x == 1)
                    return pf;
            }
            else
                ++p;
        }
    }
}

void primeFactors(unsigned x, unsigned* arr) {
    unsigned p = 2;
    int pf = 0;
    if (x == 1)
        arr[pf] = 1;
    else {
        while (true) {
            if (!(x % p)) {
                arr[pf++] = p;
                x /= p;
                if (x == 1)
                    return;
            }
            else
                p++;
        }
    }
}

unsigned sumDigits(unsigned x) {
    unsigned sum = 0, y;
    while (x) {
        y = x % 10;
        sum += y;
        x /= 10;
    }
    return sum;
}

unsigned sumFactors(unsigned* arr, int size) {
    unsigned sum = 0;
    for (int a = 0; a < size; a++)
        sum += sumDigits(arr[a]);
    return sum;
}

void listAllSmithNumbers(unsigned x) {
    unsigned *arr;
    for (unsigned a = 4; a < x; a++) {
        int numfactors = numPrimeFactors(a);
        arr = (unsigned*)malloc(numfactors * sizeof(unsigned));
        if (numfactors < 2)
            continue;
        primeFactors(a, arr);
        if (sumDigits(a) == sumFactors(arr,numfactors))
            printf("%4u ",a);
        free(arr);
    }
}

int main(int argc, char* argv[]) {
    printf("All the Smith Numbers < 10000 are:\n");
    listAllSmithNumbers(10000);
    return 0;
}

```

{{out}}

```txt

All the Smith Numbers < 10000 are:
   4   22   27   58   85   94  121  166  202  265  274  319  346  355  378  382
 391  438  454  483  517  526  535  562  576  588  627  634  636  645  648  654
 663  666  690  706  728  729  762  778  825  852  861  895  913  915  922  958
 985 1086 1111 1165 1219 1255 1282 1284 1376 1449 1507 1581 1626 1633 1642 1678
1736 1755 1776 1795 1822 1842 1858 1872 1881 1894 1903 1908 1921 1935 1952 1962
1966 2038 2067 2079 2155 2173 2182 2218 2227 2265 2286 2326 2362 2366 2373 2409
2434 2461 2475 2484 2515 2556 2576 2578 2583 2605 2614 2679 2688 2722 2745 2751
2785 2839 2888 2902 2911 2934 2944 2958 2964 2965 2970 2974 3046 3091 3138 3168
3174 3226 3246 3258 3294 3345 3366 3390 3442 3505 3564 3595 3615 3622 3649 3663
3690 3694 3802 3852 3864 3865 3930 3946 3973 4054 4126 4162 4173 4185 4189 4191
4198 4209 4279 4306 4369 4414 4428 4464 4472 4557 4592 4594 4702 4743 4765 4788
4794 4832 4855 4880 4918 4954 4959 4960 4974 4981 5062 5071 5088 5098 5172 5242
5248 5253 5269 5298 5305 5386 5388 5397 5422 5458 5485 5526 5539 5602 5638 5642
5674 5772 5818 5854 5874 5915 5926 5935 5936 5946 5998 6036 6054 6084 6096 6115
6171 6178 6187 6188 6252 6259 6295 6315 6344 6385 6439 6457 6502 6531 6567 6583
6585 6603 6684 6693 6702 6718 6760 6816 6835 6855 6880 6934 6981 7026 7051 7062
7068 7078 7089 7119 7136 7186 7195 7227 7249 7287 7339 7402 7438 7447 7465 7503
7627 7674 7683 7695 7712 7726 7762 7764 7782 7784 7809 7824 7834 7915 7952 7978
8005 8014 8023 8073 8077 8095 8149 8154 8158 8185 8196 8253 8257 8277 8307 8347
8372 8412 8421 8466 8518 8545 8568 8628 8653 8680 8736 8754 8766 8790 8792 8851
8864 8874 8883 8901 8914 9015 9031 9036 9094 9166 9184 9193 9229 9274 9276 9285
9294 9296 9301 9330 9346 9355 9382 9386 9387 9396 9414 9427 9483 9522 9535 9571
9598 9633 9634 9639 9648 9657 9684 9708 9717 9735 9742 9760 9778 9840 9843 9849
9861 9880 9895 9924 9942 9968 9975 9985

```



## C++


```cpp

#include <iostream>
#include <vector>
#include <iomanip>

void primeFactors( unsigned n, std::vector<unsigned>& r ) {
    int f = 2; if( n == 1 ) r.push_back( 1 );
    else {
        while( true ) {
            if( !( n % f ) ) {
                r.push_back( f );
                n /= f; if( n == 1 ) return;
            }
            else f++;
        }
    }
}
unsigned sumDigits( unsigned n ) {
    unsigned sum = 0, m;
    while( n ) {
        m = n % 10; sum += m;
        n -= m; n /= 10;
    }
    return sum;
}
unsigned sumDigits( std::vector<unsigned>& v ) {
    unsigned sum = 0;
    for( std::vector<unsigned>::iterator i = v.begin(); i != v.end(); i++ ) {
        sum += sumDigits( *i );
    }
    return sum;
}
void listAllSmithNumbers( unsigned n ) {
    std::vector<unsigned> pf;
    for( unsigned i = 4; i < n; i++ ) {
        primeFactors( i, pf ); if( pf.size() < 2 ) continue;
        if( sumDigits( i ) == sumDigits( pf ) )
            std::cout << std::setw( 4 ) << i << " ";
        pf.clear();
    }
    std::cout << "\n\n";
}
int main( int argc, char* argv[] ) {
    listAllSmithNumbers( 10000 );
    return 0;
}

```

{{out}}

```txt

   4   22   27   58   85   94  121  166  202  265  274  319  346  355  378  382
 391  438  454  483  517  526  535  562  576  627  634  636  645  663  666  690
...
9301 9330 9346 9355 9382 9386 9387 9396 9427 9483 9535 9571 9598 9633 9634 9639
9648 9657 9684 9708 9717 9735 9742 9760 9778 9843 9849 9861 9880 9895 9975 9985

```


## C#
{{trans|java}}

```c#
using System;
using System.Collections.Generic;

namespace SmithNumbers {
    class Program {
        static int SumDigits(int n) {
            int sum = 0;
            while (n > 0) {
                n = Math.DivRem(n, 10, out int rem);
                sum += rem;
            }
            return sum;
        }

        static List<int> PrimeFactors(int n) {
            List<int> result = new List<int>();

            for (int i = 2; n % i == 0; n /= i) {
                result.Add(i);
            }

            for (int i = 3; i * i < n; i += 2) {
                while (n % i == 0) {
                    result.Add(i);
                    n /= i;
                }
            }

            if (n != 1) {
                result.Add(n);
            }

            return result;
        }

        static void Main(string[] args) {
            const int SIZE = 8;
            int count = 0;
            for (int n = 1; n < 10_000; n++) {
                var factors = PrimeFactors(n);
                if (factors.Count > 1) {
                    int sum = SumDigits(n);
                    foreach (var f in factors) {
                        sum -= SumDigits(f);
                    }
                    if (sum == 0) {
                        Console.Write("{0,5}", n);
                        if (count == SIZE - 1) {
                            Console.WriteLine();
                        }
                        count = (count + 1) % SIZE;
                    }
                }
            }
        }
    }
}
```

{{out}}

```txt
    4   22   27   58   85   94  166  202
  265  274  319  346  355  378  382  391
  438  454  483  517  526  535  562  627
  634  636  645  648  654  663  666  690
  706  728  729  762  778  825  852  861
  895  913  915  922  958  985 1086 1111
 1165 1219 1255 1282 1284 1376 1449 1507
 1581 1626 1633 1642 1678 1736 1755 1776
 1795 1822 1842 1858 1872 1881 1894 1903
 1908 1921 1935 1952 1962 1966 2038 2067
 2079 2155 2166 2173 2182 2218 2227 2265
 2286 2326 2362 2373 2409 2434 2461 2475
 2484 2515 2556 2576 2578 2583 2605 2614
 2679 2688 2722 2745 2751 2785 2839 2902
 2911 2934 2944 2958 2964 2965 2970 2974
 3046 3091 3138 3168 3226 3246 3258 3294
 3345 3366 3390 3442 3505 3564 3595 3615
 3622 3649 3663 3690 3694 3802 3852 3864
 3865 3930 3946 3973 4054 4126 4162 4173
 4185 4189 4191 4198 4209 4279 4306 4369
 4414 4428 4464 4472 4557 4592 4594 4702
 4743 4765 4788 4794 4832 4855 4880 4918
 4954 4959 4960 4974 4981 5062 5071 5088
 5098 5172 5242 5248 5253 5269 5298 5305
 5386 5388 5397 5422 5458 5485 5526 5539
 5602 5638 5642 5674 5772 5818 5854 5874
 5926 5935 5936 5946 5998 6036 6054 6096
 6115 6171 6178 6187 6188 6252 6259 6295
 6315 6344 6385 6439 6457 6502 6531 6567
 6583 6585 6603 6684 6693 6702 6718 6816
 6835 6855 6880 6934 6981 7026 7051 7062
 7068 7078 7089 7119 7136 7186 7195 7227
 7249 7287 7339 7402 7438 7447 7465 7503
 7627 7674 7683 7695 7712 7726 7762 7764
 7782 7784 7809 7824 7834 7915 7935 7938
 7952 7978 8005 8014 8023 8073 8077 8095
 8149 8154 8158 8185 8196 8253 8257 8277
 8307 8347 8372 8412 8421 8466 8518 8545
 8568 8628 8653 8680 8736 8754 8766 8790
 8792 8851 8864 8874 8883 8901 8914 9015
 9031 9036 9094 9166 9184 9193 9229 9274
 9276 9285 9294 9296 9301 9330 9346 9355
 9382 9387 9396 9414 9427 9483 9535 9537
 9571 9598 9633 9634 9639 9648 9657 9684
 9708 9717 9735 9742 9760 9778 9840 9843
 9849 9861 9880 9895 9924 9942 9968 9975
 9985
```



## Clojure


```clojure
(defn divisible? [a b]
  (zero? (mod a b)))

(defn prime? [n]
  (and (> n 1) (not-any? (partial divisible? n) (range 2 n))))

(defn prime-factors
  ([n] (prime-factors n 2 '()))
  ([n candidate acc]
    (cond
      (<= n 1) (reverse acc)
      (zero? (rem n candidate)) (recur
                                  (/ n candidate)
                                  candidate
                                  (cons candidate acc))
      :else (recur n (inc candidate) acc))))

(defn sum-digits [n]
  (reduce + (map #(- (int %) (int \0)) (str n))))

(defn smith-number? [n]
  (and (not (prime? n))
       (= (sum-digits n)
          (sum-digits (clojure.string/join "" (prime-factors n))))))

(filter smith-number? (range 1 10000))
```


{{out}}

```txt

(4 22 27 58 85 94 121 166 202 265 274 319 346 355 378 382 391
 ...
 9760 9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975 9985)

```



## D

{{trans|Java}} mostly

```D
import std.stdio;

void main() {
    int cnt;
    for (int n=1; n<10_000; n++) {
        auto factors = primeFactors(n);
        if (factors.length > 1) {
            int sum = sumDigits(n);
            foreach (f; factors) {
                sum -= sumDigits(f);
            }
            if (sum==0) {
                writef("%4s  ", n);
                cnt++;
            }
            if (cnt==10) {
                cnt = 0;
                writeln();
            }
        }
    }
}

auto primeFactors(int n) {
    import std.array : appender;
    auto result = appender!(int[]);

    for (int i=2; n%i==0; n/=i) {
        result.put(i);
    }

    for (int i=3; i*i<=n; i+=2) {
        while (n%i==0) {
            result.put(i);
            n/=i;
        }
    }

    if (n!=1) {
        result.put(n);
    }

    return result.data;
}

int sumDigits(int n) {
    int sum;
    while (n > 0) {
        sum += (n%10);
        n /= 10;
    }
    return sum;
}
```


{{out}}

```txt
   4    22    27    58    85    94   121   166   202   265
 274   319   346   355   378   382   391   438   454   483
 517   526   535   562   576   588   627   634   636   645
 648   654   663   666   690   706   728   729   762   778
 825   852   861   895   913   915   922   958   985  1086
1111  1165  1219  1255  1282  1284  1376  1449  1507  1581
1626  1633  1642  1678  1736  1755  1776  1795  1822  1842
1858  1872  1881  1894  1903  1908  1921  1935  1952  1962
1966  2038  2067  2079  2155  2173  2182  2218  2227  2265
2286  2326  2362  2366  2373  2409  2434  2461  2475  2484
2515  2556  2576  2578  2583  2605  2614  2679  2688  2722
2745  2751  2785  2839  2888  2902  2911  2934  2944  2958
2964  2965  2970  2974  3046  3091  3138  3168  3174  3226
3246  3258  3294  3345  3366  3390  3442  3505  3564  3595
3615  3622  3649  3663  3690  3694  3802  3852  3864  3865
3930  3946  3973  4054  4126  4162  4173  4185  4189  4191
4198  4209  4279  4306  4369  4414  4428  4464  4472  4557
4592  4594  4702  4743  4765  4788  4794  4832  4855  4880
4918  4954  4959  4960  4974  4981  5062  5071  5088  5098
5172  5242  5248  5253  5269  5298  5305  5386  5388  5397
5422  5458  5485  5526  5539  5602  5638  5642  5674  5772
5818  5854  5874  5915  5926  5935  5936  5946  5998  6036
6054  6084  6096  6115  6171  6178  6187  6188  6252  6259
6295  6315  6344  6385  6439  6457  6502  6531  6567  6583
6585  6603  6684  6693  6702  6718  6760  6816  6835  6855
6880  6934  6981  7026  7051  7062  7068  7078  7089  7119
7136  7186  7195  7227  7249  7287  7339  7402  7438  7447
7465  7503  7627  7674  7683  7695  7712  7726  7762  7764
7782  7784  7809  7824  7834  7915  7952  7978  8005  8014
8023  8073  8077  8095  8149  8154  8158  8185  8196  8253
8257  8277  8307  8347  8372  8412  8421  8466  8518  8545
8568  8628  8653  8680  8736  8754  8766  8790  8792  8851
8864  8874  8883  8901  8914  9015  9031  9036  9094  9166
9184  9193  9229  9274  9276  9285  9294  9296  9301  9330
9346  9355  9382  9386  9387  9396  9414  9427  9483  9522
9535  9571  9598  9633  9634  9639  9648  9657  9684  9708
9717  9735  9742  9760  9778  9840  9843  9849  9861  9880
9895  9924  9942  9968  9975  9985
```



## Elixir


```elixir
defmodule Smith do
  def number?(n) do
    d = decomposition(n)
    length(d)>1 and sum_digits(n) == Enum.map(d, &sum_digits/1) |> Enum.sum
  end

  defp sum_digits(n) do
    Integer.digits(n) |> Enum.sum
  end

  defp decomposition(n, k\\2, acc\\[])
  defp decomposition(n, k, acc) when n < k*k, do: [n | acc]
  defp decomposition(n, k, acc) when rem(n, k) == 0, do: decomposition(div(n, k), k, [k | acc])
  defp decomposition(n, k, acc), do: decomposition(n, k+1, acc)
end

m = 10000
smith = Enum.filter(1..m, &Smith.number?/1)
IO.puts "#{length(smith)} smith numbers below #{m}:"
IO.puts "First 10: #{Enum.take(smith,10) |> Enum.join(", ")}"
IO.puts "Last  10: #{Enum.take(smith,-10) |> Enum.join(", ")}"
```


{{out}}

```txt

376 smith numbers below 10000:
First 10: 4, 22, 27, 58, 85, 94, 121, 166, 202, 265
Last  10: 9843, 9849, 9861, 9880, 9895, 9924, 9942, 9968, 9975, 9985

```



## Factor

<lang>USING: formatting grouping io kernel math.primes.factors
math.ranges math.text.utils sequences sequences.deep ;

: (smith?) ( n factors -- ? )
    [ 1 digit-groups sum ]
    [ [ 1 digit-groups ] map flatten sum = ] bi* ; inline

: smith? ( n -- ? )
    dup factors dup length 1 = [ 2drop f ] [ (smith?) ] if ;

10,000 [1,b] [ smith? ] filter 10 group
[ [ "%4d " printf ] each nl ] each
```

{{out}}

```txt

   4   22   27   58   85   94  121  166  202  265
 274  319  346  355  378  382  391  438  454  483
 517  526  535  562  576  588  627  634  636  645
 648  654  663  666  690  706  728  729  762  778
 825  852  861  895  913  915  922  958  985 1086
1111 1165 1219 1255 1282 1284 1376 1449 1507 1581
1626 1633 1642 1678 1736 1755 1776 1795 1822 1842
1858 1872 1881 1894 1903 1908 1921 1935 1952 1962
1966 2038 2067 2079 2155 2173 2182 2218 2227 2265
2286 2326 2362 2366 2373 2409 2434 2461 2475 2484
2515 2556 2576 2578 2583 2605 2614 2679 2688 2722
2745 2751 2785 2839 2888 2902 2911 2934 2944 2958
2964 2965 2970 2974 3046 3091 3138 3168 3174 3226
3246 3258 3294 3345 3366 3390 3442 3505 3564 3595
3615 3622 3649 3663 3690 3694 3802 3852 3864 3865
3930 3946 3973 4054 4126 4162 4173 4185 4189 4191
4198 4209 4279 4306 4369 4414 4428 4464 4472 4557
4592 4594 4702 4743 4765 4788 4794 4832 4855 4880
4918 4954 4959 4960 4974 4981 5062 5071 5088 5098
5172 5242 5248 5253 5269 5298 5305 5386 5388 5397
5422 5458 5485 5526 5539 5602 5638 5642 5674 5772
5818 5854 5874 5915 5926 5935 5936 5946 5998 6036
6054 6084 6096 6115 6171 6178 6187 6188 6252 6259
6295 6315 6344 6385 6439 6457 6502 6531 6567 6583
6585 6603 6684 6693 6702 6718 6760 6816 6835 6855
6880 6934 6981 7026 7051 7062 7068 7078 7089 7119
7136 7186 7195 7227 7249 7287 7339 7402 7438 7447
7465 7503 7627 7674 7683 7695 7712 7726 7762 7764
7782 7784 7809 7824 7834 7915 7952 7978 8005 8014
8023 8073 8077 8095 8149 8154 8158 8185 8196 8253
8257 8277 8307 8347 8372 8412 8421 8466 8518 8545
8568 8628 8653 8680 8736 8754 8766 8790 8792 8851
8864 8874 8883 8901 8914 9015 9031 9036 9094 9166
9184 9193 9229 9274 9276 9285 9294 9296 9301 9330
9346 9355 9382 9386 9387 9396 9414 9427 9483 9522
9535 9571 9598 9633 9634 9639 9648 9657 9684 9708
9717 9735 9742 9760 9778 9840 9843 9849 9861 9880
9895 9924 9942 9968 9975 9985

```



## Fortran

This is F90 style, to take advantage of module PRIMESTUFF from [[Extensible_prime_generator]] to get at a supply of prime numbers and related routines, and contains a slightly trimmed module FACTORISE from the [[Fractran|FRACTRAN]] project that factorises a number but which doesn't need the slight extras for the FRACTRAN process. Re-using code is good, but one must watch out for forgotten details that may not fit into the new context: the FRACTRAN project wanted the ''number'' of the prime, not the prime number (itself) in its lists of factors, whereas this project wanted the actual prime number in its list of factors. So, it would be PRIME(F.PNUM(i)), because "PNUM" means "the prime's number"... However, acquiring the i'th prime via PRIME(i) is not a matter of array access, it involves a function with some fancy arithmetic. Since the factorisation requires consecutive prime numbers, using NEXTPRIME(F) is a better choice, and the run is much faster since many numbers are being factorised: the FRACTRAN project factorised only a few. So, a change from "PNUM" to "PVAL" with the prime's value stored instead of its index, even though this means that PNUM(0) which holds the number of prime factors becomes PVAL(0): discordance in the mnemonics. Then, having started along these lines, a rewrite was provoked, prompted by the recollection that function ISPRIME does ''not'' engage in the standard slog through possible prime factors (except for two), since for odd numbers it refers to its big bit array. Accessing this array takes time as it is in a disc file, but the operating system buffers popular records in memory (a record is 4096 bytes for 32736 bits as each starts with a four-byte count, thus the first record spans 3 to 65473), so timing runs is a frustrating business. There seemed no gross change in speed, so that's good enough for a demonstration. The code involves a GO TO statement because there is no <code>repeat ... until ''test''</code> construction provided in Fortran and a <code>DO WHILE ... END DO</code> would involve a wasted first test. Because I really hate array bound errors there is a check against LASTP even though the array will never overflow for INTEGER*4, but (potentially) someday the code might be inflated to INTEGER*8 or some other larger capacity and the necessary adjustments be overlooked. One could have <code>IF (LASTP.LE.9 .AND. HUGE(N).GT.2147483648) STOP "Oi! INTEGER*4 usage!"</code> to check this (and a good compiler would convert it to no code if all was well) but that's tiresome too and only checks for some problems. Accordingly, the code for adding a factor to the list is too messy to replicate, and making it into a service subroutine is tiresome: thus does structure falter when spaghetti is not forgotten.

Similarly, initial attempts foundered before I realised that the sum of the digits of the prime factors did ''not'' mean that of the unique prime factors once only but included each appearance of a prime factor, so it was DIGITSUM(F.PVAL(i),BASE)*F.PPOW(i) for success.  And, since one is deemed to have no prime factors, one does not appear even though it is not skipped as being a prime number.

The factorisation is represented in a data aggregate, which is returned by function FACTOR. This is a facility introduced with F90, and before that one would have to use a collection of ordinary arrays to identify the list of primes and powers of a factorisation because functions could only return simple variables. Also, earlier compilers did not allow the use of the function's name as a variable within the function, or might allow this but produce incorrect results. However, modern facilities are not always entirely beneficial. Here, the function returns a full set of data for type FACTORED, even though often only the first few elements of the arrays will be needed and the rest could be ignored. It is possible to declare the arrays of type FACTORED to be "allocatable" with their size being determined at run time for each invocation of function FACTOR, at the cost of a lot of additional syntax and statements, plus the common annoyance of not knowing "how big" until ''after'' the list has been produced. Alas, such arrangements incur a performance penalty with ''every'' reference to the allocatable entities. See for example [[Sequence_of_primorial_primes#Run-time_allocation]]

For layout purposes, the numbers found were stashed in a line buffer rather than attempt to mess with the latter-day facilities of "non-advancing" output. This should be paramaterised for documentation purposes with say <code>MBUF = 20</code> rather than just using the magic constant of 20, however getting that into the FORMAT statement would require <code>FORMAT(<MBUF>I6)</code> and this <n> facility may not be recognised. Alternatively, one could put <code>FORMAT(666I6)</code> and hope that MBUF would never exceed 666.
```Fortran
      MODULE FACTORISE	!Produce a little list...
       USE PRIMEBAG		!This is a common need.
       INTEGER LASTP		!Some size allowances.
       PARAMETER (LASTP = 9)	!2*3*5*7*11*13*17*19*23*29 = 6,469,693,230, > 2,147,483,647.
       TYPE FACTORED		!Represent a number fully factored.
        INTEGER PVAL(0:LASTP)	!As a list of prime number indices with PVAL(0) the count.
        INTEGER PPOW(LASTP)	!And the powers. for the fingered primes.
       END TYPE FACTORED	!Rather than as a simple number multiplied out.

       CONTAINS		!Now for the details.
        SUBROUTINE SHOWFACTORS(N)	!First, to show an internal data structure.
         TYPE(FACTORED) N	!It is supplied as a list of prime factors.
         INTEGER I		!A stepper.
          DO I = 1,N.PVAL(0)	!Step along the list.
            IF (I.GT.1) WRITE (MSG,"('x',$)")	!Append a glyph for "multiply".
            WRITE (MSG,"(I0,$)") N.PVAL(I)	!The prime number's value.
            IF (N.PPOW(I).GT.1) WRITE (MSG,"('^',I0,$)") N.PPOW(I)	!With an interesting power?
          END DO		!On to the next element in the list.
          WRITE (MSG,1) N.PVAL(0)	!End the line
    1     FORMAT (": Factor count ",I0)	!With a count of prime factors.
        END SUBROUTINE SHOWFACTORS	!Hopefully, this will not be needed often.

        TYPE(FACTORED) FUNCTION FACTOR(IT)	!Into a list of primes and their powers.
Careful! 1 is not a factor of N, but if N is prime, N is. N = product of its prime factors.
         INTEGER IT,N	!The number and a similar style copy to damage.
         INTEGER F,FP	!A factor and a power.
          IF (IT.LE.0) STOP "Factor only positive numbers!"	!Or else...
          FACTOR.PVAL(0) = 0	!No prime factors have been found. One need not apply.
          F = 0			!NEXTPRIME(F) will return 2, the first factor to try.
          N = IT		!A copy I can damage.
Collapse N into its prime factors.
   10     DO WHILE(N.GT.1)	!Carthaga delenda est?
            IF (ISPRIME(N)) THEN!If the remnant is a prime number,
              F = N			!Then it is the last factor.
              FP = 1			!Its power is one.
              N = 1			!And the reduction is finished.
             ELSE		!Otherwise, continue trying larger factors.
              FP = 0			!It has no power yet.
   11         F = NEXTPRIME(F)		!Go for the next possible factor.
              DO WHILE(MOD(N,F).EQ.0)	!Well?
                FP = FP + 1			!Count a factor..
                N = N/F				!Reduce the number.
              END DO			!Until F's multiplicity is exhausted.
              IF (FP.LE.0) GO TO 11	!No presence? Try the next factor: N has some...
            END IF		!One way or another, F is a prime factor and FP its power.
            IF (FACTOR.PVAL(0).GE.LASTP) THEN	!Have I room in the list?
              WRITE (MSG,1) IT,LASTP		!Alas.
    1         FORMAT ("Factoring ",I0," but with provision for only ",	!This shouldn't happen,
     1         I0," distinct prime factors!")	!If LASTP is correct for the current INTEGER size.
              CALL SHOWFACTORS(FACTOR)		!Show what has been found so far.
              STOP "Not enough storage!"	!Quite.
            END IF			!But normally,
            FACTOR.PVAL(0) = FACTOR.PVAL(0) + 1	!Admit another factor.
            FACTOR.PVAL(FACTOR.PVAL(0)) = F	!The prime number found to be a factor.
            FACTOR.PPOW(FACTOR.PVAL(0)) = FP	!Place its power.
          END DO		!Now seee what has survived.
        END FUNCTION FACTOR	!Thus, a list of primes and their powers.
      END MODULE FACTORISE	!Careful! PVAL(0) is the number of prime factors.

      MODULE SMITHSTUFF	!Now for the strange stuff.
       CONTAINS		!The two special workers.
        INTEGER FUNCTION DIGITSUM(N,BASE)	!Sums the digits of N.
         INTEGER N,IT	!The number, and a copy I can damage.
         INTEGER BASE	!The base for arithmetic,
         IF (N.LT.0) STOP "DigitSum: negative numbers need not apply!"
          DIGITSUM = 0	!Here we go.
          IT = N	!This value will be damaged.
          DO WHILE(IT.GT.0)	!Something remains?
            DIGITSUM = MOD(IT,BASE) + DIGITSUM	!Yes. Grap the low-order digit.
            IT = IT/BASE			!And descend a power.
          END DO		!Perhaps something still remains.
        END FUNCTION DIGITSUM	!Numerology.

        LOGICAL FUNCTION SMITHNUM(N,BASE)	!Worse numerology.
         USE FACTORISE		!To find the prime factord of N.
         INTEGER N		!The number of interest.
         INTEGER BASE		!The base of the numerology.
         TYPE(FACTORED) F	!A list.
         INTEGER I,FD		!Assistants.
          F = FACTOR(N)		!Hopefully, LASTP is large enough for N.
c          write (6,"(a,I0,1x)",advance="no") "N=",N
c          call ShowFactors(F)
          FD = 0		!Attempts via the SUM facility involved too many requirements.
          DO I = 1,F.PVAL(0)	!For each of the prime factors found...
            FD = DIGITSUM(F.PVAL(I),BASE)*F.PPOW(I) + FD	!Not forgetting the multiplicity.
          END DO		!On to the next prime factor in the list.
          SMITHNUM = FD.EQ.DIGITSUM(N,BASE)	!This is the rule.
        END FUNCTION SMITHNUM	!So, is N a joker?
      END MODULE SMITHSTUFF	!Simple enough.

      USE PRIMEBAG	!Gain access to GRASPPRIMEBAG.
      USE SMITHSTUFF	!The special stuff.
      INTEGER LAST		!Might as well document this.
      PARAMETER (LAST = 9999)	!The specification is BELOW 10000...
      INTEGER I,N,BASE		!Workers.
      INTEGER NB,BAG(20)	!Prepare a line's worth of results.
      MSG = 6	!Standard output.

      WRITE (MSG,1) LAST	!Hello.
    1 FORMAT ('To find the "Smith" numbers up to ',I0)
      IF (.NOT.GRASPPRIMEBAG(66)) STOP "Gan't grab my file!"	!Attempt in hope.

   10 DO BASE = 2,12	!Flexible numerology.
        WRITE (MSG,11) BASE	!Here we go again.
   11   FORMAT (/,"Working in base ",I0)
        N = 0			!None found.
        NB = 0			!So, none are bagged.
        DO I = 1,LAST		!Step through the span.
          IF (ISPRIME(I)) CYCLE		!Prime numbers are boring Smith numbers. Skip them.
          IF (SMITHNUM(I,BASE)) THEN	!So?
            N = N + 1				!Count one in.
            IF (NB.GE.20) THEN			!A full line's worth with another to come?
              WRITE (MSG,12) BAG			!Yep. Roll the line to make space.
   12         FORMAT (20I6)				!This will do for a nice table.
              NB = 0					!The line is now ready.
            END IF				!So much for a line buffer.
            NB = NB + 1				!Count another entry.
            BAG(NB) = I				!Place it.
          END IF			!So much for a Smith style number.
        END DO			!On to the next candidate number.
        WRITE (MSG,12) BAG(1:NB)!Wave the tail end.
        WRITE (MSG,13) N	!Save the human some counting.
   13   FORMAT (I9," found.")	!Just in case.
      END DO		!On to the next base.
      END	!That was strange.
```


Output: selecting the base ten result:

```txt

Working in base 10
     4    22    27    58    85    94   121   166   202   265   274   319   346   355   378   382   391   438   454   483
   517   526   535   562   576   588   627   634   636   645   648   654   663   666   690   706   728   729   762   778
...etc
  9346  9355  9382  9386  9387  9396  9414  9427  9483  9522  9535  9571  9598  9633  9634  9639  9648  9657  9684  9708
  9717  9735  9742  9760  9778  9840  9843  9849  9861  9880  9895  9924  9942  9968  9975  9985
      376 found.

```

For the various bases, the counts were
 Base:     2   3   4   5   6   7   8   9  10  11  12
 Count:  615 459 417 327 716 245 432 250 376 742 448

Reverting to counting each prime of a factorisation once only did not simply reject all those Smith numbers that had repeated prime factors, it added new entries, for example 9940: the "smith" numbers?

```txt

Working in base 10
    22    58    84    85    94   136   160   166   202   234   250   265   274   308   319   336   346   355   361   364
   382   391   424   438   454   456   476   483   516   517   526   535   562   627   634   644   645   650   654   660
   663   690   702   706   732   735   762   778   855   860   861   895   913   915   922   948   958   985  1086  1111
  1116  1148  1165  1219  1255  1282  1312  1344  1404  1484  1507  1550  1576  1581  1600  1612  1626  1633  1642  1650
  1665  1678  1708  1752  1795  1812  1822  1824  1842  1858  1876  1894  1903  1921  1924  1966  2008  2038  2064  2067
  2106  2155  2166  2173  2182  2218  2227  2232  2236  2265  2275  2325  2326  2352  2356  2362  2373  2401  2409  2434
  2461  2500  2515  2541  2565  2578  2605  2614  2616  2625  2640  2679  2722  2751  2760  2785  2826  2839  2872  2902
  2911  2924  2958  2960  2965  2974  3036  3042  3046  3048  3091  3138  3164  3172  3226  3246  3268  3285  3339  3344
  3345  3381  3390  3393  3442  3474  3476  3484  3505  3552  3556  3592  3595  3615  3618  3622  3625  3630  3649  3694
  3712  3736  3792  3802  3836  3850  3865  3892  3912  3920  3930  3933  3946  3973  4024  4054  4116  4126  4148  4160
  4162  4173  4188  4189  4191  4198  4209  4212  4228  4235  4268  4275  4279  4306  4344  4369  4396  4414  4456  4460
  4473  4564  4590  4594  4636  4656  4676  4702  4744  4765  4770  4776  4794  4820  4824  4844  4855  4905  4918  4920
  4954  4974  4980  4981  5022  5052  5062  5068  5071  5094  5098  5145  5150  5168  5176  5242  5253  5268  5269  5298
  5305  5332  5344  5348  5386  5397  5412  5422  5425  5458  5464  5484  5485  5525  5539  5548  5602  5612  5638  5642
  5652  5674  5715  5742  5752  5818  5840  5854  5874  5926  5935  5946  5998  6016  6027  6054  6060  6066  6115  6175
  6178  6184  6187  6244  6259  6260  6295  6315  6356  6364  6385  6390  6439  6457  6472  6475  6500  6502  6504  6512
  6524  6531  6564  6567  6583  6585  6596  6600  6603  6604  6616  6620  6633  6692  6693  6702  6714  6718  6741  6835
  6855  6900  6904  6934  6950  6960  6980  6981  7008  7026  7028  7038  7048  7051  7052  7062  7076  7078  7089  7150
  7186  7195  7196  7212  7228  7236  7249  7268  7287  7335  7339  7362  7364  7402  7428  7438  7447  7465  7503  7506
  7525  7624  7627  7650  7674  7683  7726  7756  7762  7782  7809  7834  7850  7915  7924  7978  8005  8014  8023  8076
  8077  8084  8091  8095  8145  8149  8158  8164  8185  8214  8224  8244  8257  8277  8284  8292  8308  8325  8334  8347
  8415  8420  8421  8466  8508  8518  8545  8600  8653  8673  8720  8724  8754  8780  8790  8816  8851  8914  8924  8932
  8955  8982  9015  9028  9031  9052  9094  9096  9116  9166  9180  9193  9229  9274  9285  9294  9301  9306  9330  9333
  9346  9350  9355  9382  9412  9425  9427  9436  9483  9528  9535  9540  9571  9598  9630  9634  9650  9652  9711  9716
  9717  9735  9742  9772  9778  9843  9861  9895  9916  9940  9942  9985
      492 found.

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub getPrimeFactors(factors() As UInteger, n As UInteger)
  If n < 2 Then Return
  Dim factor As UInteger = 2
  Do
    If n Mod factor = 0 Then
      Redim Preserve factors(0 To UBound(factors) + 1)
      factors(UBound(factors)) = factor
      n \= factor
      If n = 1 Then Return
    Else
      ' non-prime factors will always give a remainder > 0 as their own factors have already been removed
      ' so it's not worth checking that the next potential factor is prime
      factor += 1
    End If
  Loop
End Sub

Function sumDigits(n As UInteger) As UInteger
  If n < 10 Then Return n
  Dim sum As UInteger = 0
  While n > 0
    sum += n Mod 10
    n \= 10
  Wend
  Return sum
End Function

Function isSmith(n As UInteger) As Boolean
  If n < 2 Then Return False
  Dim factors() As UInteger
  getPrimeFactors factors(), n
  If UBound(factors) = 0 Then Return False  '' n must be prime if there's only one factor
  Dim primeSum As UInteger = 0
  For i As UInteger = 0 To UBound(factors)
    primeSum += sumDigits(factors(i))
  Next
  Return sumDigits(n) = primeSum
End Function

Print "The Smith numbers below 10000 are : "
Print
Dim count As UInteger = 0
For i As UInteger = 2 To 9999
  If isSmith(i) Then
    Print Using "#####"; i;
    count += 1
  End If
Next
Print : Print
Print count; " numbers found"
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

The Smith numbers below 10000 are :

    4   22   27   58   85   94  121  166  202  265  274  319  346  355  378  382
  391  438  454  483  517  526  535  562  576  588  627  634  636  645  648  654
  663  666  690  706  728  729  762  778  825  852  861  895  913  915  922  958
  985 1086 1111 1165 1219 1255 1282 1284 1376 1449 1507 1581 1626 1633 1642 1678
 1736 1755 1776 1795 1822 1842 1858 1872 1881 1894 1903 1908 1921 1935 1952 1962
 1966 2038 2067 2079 2155 2173 2182 2218 2227 2265 2286 2326 2362 2366 2373 2409
 2434 2461 2475 2484 2515 2556 2576 2578 2583 2605 2614 2679 2688 2722 2745 2751
 2785 2839 2888 2902 2911 2934 2944 2958 2964 2965 2970 2974 3046 3091 3138 3168
 3174 3226 3246 3258 3294 3345 3366 3390 3442 3505 3564 3595 3615 3622 3649 3663
 3690 3694 3802 3852 3864 3865 3930 3946 3973 4054 4126 4162 4173 4185 4189 4191
 4198 4209 4279 4306 4369 4414 4428 4464 4472 4557 4592 4594 4702 4743 4765 4788
 4794 4832 4855 4880 4918 4954 4959 4960 4974 4981 5062 5071 5088 5098 5172 5242
 5248 5253 5269 5298 5305 5386 5388 5397 5422 5458 5485 5526 5539 5602 5638 5642
 5674 5772 5818 5854 5874 5915 5926 5935 5936 5946 5998 6036 6054 6084 6096 6115
 6171 6178 6187 6188 6252 6259 6295 6315 6344 6385 6439 6457 6502 6531 6567 6583
 6585 6603 6684 6693 6702 6718 6760 6816 6835 6855 6880 6934 6981 7026 7051 7062
 7068 7078 7089 7119 7136 7186 7195 7227 7249 7287 7339 7402 7438 7447 7465 7503
 7627 7674 7683 7695 7712 7726 7762 7764 7782 7784 7809 7824 7834 7915 7952 7978
 8005 8014 8023 8073 8077 8095 8149 8154 8158 8185 8196 8253 8257 8277 8307 8347
 8372 8412 8421 8466 8518 8545 8568 8628 8653 8680 8736 8754 8766 8790 8792 8851
 8864 8874 8883 8901 8914 9015 9031 9036 9094 9166 9184 9193 9229 9274 9276 9285
 9294 9296 9301 9330 9346 9355 9382 9386 9387 9396 9414 9427 9483 9522 9535 9571
 9598 9633 9634 9639 9648 9657 9684 9708 9717 9735 9742 9760 9778 9840 9843 9849
 9861 9880 9895 9924 9942 9968 9975 9985

376 numbers found

```



## Go

{{trans|C}}

```Go

package main

import "fmt"

func numPrimeFactors(x uint) int {
	var p uint = 2
	var pf int
	if x == 1 {
		return 1
	}
	for {
		if (x % p) == 0 {
			pf++
			x /= p
			if x == 1 {
				return pf
			}
		} else {
			p++
		}
	}
}

func primeFactors(x uint, arr []uint) {
	var p uint = 2
	var pf int
	if x == 1 {
		arr[pf] = 1
		return
	}
	for {
		if (x % p) == 0 {
			arr[pf] = p
			pf++
			x /= p
			if x == 1 {
				return
			}
		} else {
			p++
		}
	}
}

func sumDigits(x uint) uint {
	var sum uint
	for x != 0 {
		sum += x % 10
		x /= 10
	}
	return sum
}

func sumFactors(arr []uint, size int) uint {
	var sum uint
	for a := 0; a < size; a++ {
		sum += sumDigits(arr[a])
	}
	return sum
}

func listAllSmithNumbers(maxSmith uint) {
	var arr []uint
	var a uint
	for a = 4; a < maxSmith; a++ {
		numfactors := numPrimeFactors(a)
		arr = make([]uint, numfactors)
		if numfactors < 2 {
			continue
		}
		primeFactors(a, arr)
		if sumDigits(a) == sumFactors(arr, numfactors) {
			fmt.Printf("%4d ", a)
		}
	}
}

func main() {
	const maxSmith = 10000
	fmt.Printf("All the Smith Numbers less than %d are:\n", maxSmith)
	listAllSmithNumbers(maxSmith)
	fmt.Println()
}

```

{{out}}
```txt

All the Smith Numbers less than 10000 are:
   4   22   27   58   85   94  121  166  202  265  274  319  346  355  378  382  391  438  454  483  517  526  535  562  576  588  627  634  636  645  648  654  663  666  690  706  728  729  762  778  825  852  861  895  913  915  922  958  985 1086 1111 1165 1219 1255 1282 1284 1376 1449 1507 1581 1626 1633 1642 1678 1736 1755 1776 1795 1822 1842 1858 1872 1881 1894 1903 1908 1921 1935 1952 1962 1966 2038 2067 2079 2155 2173 2182 2218 2227 2265 2286 2326 2362 2366 2373 2409 2434 2461 2475 2484 2515 2556 2576 2578 2583 2605 2614 2679 2688 2722 2745 2751 2785 2839 2888 2902 2911 2934 2944 2958 2964 2965 2970 2974 3046 3091 3138 3168 3174 3226 3246 3258 3294 3345 3366 3390 3442 3505 3564 3595 3615 3622 3649 3663 3690 3694 3802 3852 3864 3865 3930 3946 3973 4054 4126 4162 4173 4185 4189 4191 4198 4209 4279 4306 4369 4414 4428 4464 4472 4557 4592 4594 4702 4743 4765 4788 4794 4832 4855 4880 4918 4954 4959 4960 4974 4981 5062 5071 5088 5098 5172 5242 5248 5253 5269 5298 5305 5386 5388 5397 5422 5458 5485 5526 5539 5602 5638 56425674 5772 5818 5854 5874 5915 5926 5935 5936 5946 5998 6036 6054 6084 6096 6115 6171 6178 6187 6188 6252 6259 6295 6315 6344 6385 6439 6457 6502 6531 6567 6583 6585 6603 6684 6693 6702 6718 6760 6816 6835 6855 6880 6934 6981 7026 7051 7062 7068 7078 7089 7119 7136 7186 7195 7227 7249 7287 7339 7402 7438 7447 7465 7503 7627 7674 7683 7695 7712 7726 7762 7764 7782 7784 7809 7824 7834 7915 7952 7978 8005 8014 8023 8073 8077 8095 8149 8154 8158 8185 8196 8253 8257 8277 8307 8347 8372 8412 8421 8466 8518 8545 8568 8628 8653 8680 8736 8754 8766 8790 8792 8851 8864 8874 8883 8901 8914 9015 9031 9036 9094 9166 9184 9193 9229 9274 9276 9285 9294 9296 9301 9330 9346 9355 9382 9386 9387 9396 9414 9427 9483 9522 9535 9571 9598 9633 9634 9639 9648 9657 9684 9708 9717 9735 9742 9760 9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975 9985

```



## Haskell



```haskell
import Data.List (unfoldr)
import Data.Tuple (swap)
import Data.Bool (bool)

isSmith :: Int -> Bool
isSmith n = pfs /= [n] && sumDigits n == foldr ((+) . sumDigits) 0 pfs
  where
    sumDigits = sum . baseDigits 10
    pfs = primeFactors n

primeFactors :: Int -> [Int]
primeFactors n =
  let fs =
        take 1 $ filter ((0 ==) . rem n) [2 .. (floor . sqrt . fromIntegral) n]
  in bool (fs ++ primeFactors (div n (head fs))) [n] (null fs)

baseDigits :: Int -> Int -> [Int]
baseDigits base = unfoldr remQuot
  where
    remQuot 0 = Nothing
    remQuot x = Just (swap (quotRem x base))

lowSmiths :: [Int]
lowSmiths = filter isSmith [2 .. 9999]

lowSmithCount :: Int
lowSmithCount = length lowSmiths

main :: IO ()
main =
  mapM_
    putStrLn
    [ "Count of Smith Numbers below 10k:"
    , show lowSmithCount
    , "\nFirst 15 Smith Numbers:"
    , unwords (show <$> take 15 lowSmiths)
    , "\nLast 12 Smith Numbers below 10k:"
    , unwords (show <$> drop (lowSmithCount - 12) lowSmiths)
    ]
```

{{Out}}

```txt
Count of Smith Numbers below 10k:
376

First 15 Smith Numbers:
4 22 27 58 85 94 121 166 202 265 274 319 346 355 378

Last 12 Smith Numbers below 10k:
9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975 9985
```



## J

Implementation:

```J
digits=: 10&#.inv
sumdig=: +/@,@digits
notprime=: -.@(1&p:)
smith=: #~  notprime * (=&sumdig q:)every
```


Task example:

```J
   #smith }.i.10000
376
   q:376
2 2 2 47
   47 8$smith }.i.10000
   4   22   27   58   85   94  121  166
 202  265  274  319  346  355  378  382
 391  438  454  483  517  526  535  562
 576  588  627  634  636  645  648  654
 663  666  690  706  728  729  762  778
 825  852  861  895  913  915  922  958
 985 1086 1111 1165 1219 1255 1282 1284
1376 1449 1507 1581 1626 1633 1642 1678
1736 1755 1776 1795 1822 1842 1858 1872
1881 1894 1903 1908 1921 1935 1952 1962
1966 2038 2067 2079 2155 2173 2182 2218
2227 2265 2286 2326 2362 2366 2373 2409
2434 2461 2475 2484 2515 2556 2576 2578
2583 2605 2614 2679 2688 2722 2745 2751
2785 2839 2888 2902 2911 2934 2944 2958
2964 2965 2970 2974 3046 3091 3138 3168
3174 3226 3246 3258 3294 3345 3366 3390
3442 3505 3564 3595 3615 3622 3649 3663
3690 3694 3802 3852 3864 3865 3930 3946
3973 4054 4126 4162 4173 4185 4189 4191
4198 4209 4279 4306 4369 4414 4428 4464
4472 4557 4592 4594 4702 4743 4765 4788
4794 4832 4855 4880 4918 4954 4959 4960
4974 4981 5062 5071 5088 5098 5172 5242
5248 5253 5269 5298 5305 5386 5388 5397
5422 5458 5485 5526 5539 5602 5638 5642
5674 5772 5818 5854 5874 5915 5926 5935
5936 5946 5998 6036 6054 6084 6096 6115
6171 6178 6187 6188 6252 6259 6295 6315
6344 6385 6439 6457 6502 6531 6567 6583
6585 6603 6684 6693 6702 6718 6760 6816
6835 6855 6880 6934 6981 7026 7051 7062
7068 7078 7089 7119 7136 7186 7195 7227
7249 7287 7339 7402 7438 7447 7465 7503
7627 7674 7683 7695 7712 7726 7762 7764
7782 7784 7809 7824 7834 7915 7952 7978
8005 8014 8023 8073 8077 8095 8149 8154
8158 8185 8196 8253 8257 8277 8307 8347
8372 8412 8421 8466 8518 8545 8568 8628
8653 8680 8736 8754 8766 8790 8792 8851
8864 8874 8883 8901 8914 9015 9031 9036
9094 9166 9184 9193 9229 9274 9276 9285
9294 9296 9301 9330 9346 9355 9382 9386
9387 9396 9414 9427 9483 9522 9535 9571
9598 9633 9634 9639 9648 9657 9684 9708
9717 9735 9742 9760 9778 9840 9843 9849
9861 9880 9895 9924 9942 9968 9975 9985
```


(first we count how many smith numbers are in our result, then we look at the prime factors of that count - turns out that 8 columns of 47 numbers each is perfect for this task.)


## Java

{{works with|Java|7}}

```java
import java.util.*;

public class SmithNumbers {

    public static void main(String[] args) {
        for (int n = 1; n < 10_000; n++) {
            List<Integer> factors = primeFactors(n);
            if (factors.size() > 1) {
                int sum = sumDigits(n);
                for (int f : factors)
                    sum -= sumDigits(f);
                if (sum == 0)
                    System.out.println(n);
            }
        }
    }

    static List<Integer> primeFactors(int n) {
        List<Integer> result = new ArrayList<>();

        for (int i = 2; n % i == 0; n /= i)
            result.add(i);

        for (int i = 3; i * i <= n; i += 2) {
            while (n % i == 0) {
                result.add(i);
                n /= i;
            }
        }

        if (n != 1)
            result.add(n);

        return result;
    }

    static int sumDigits(int n) {
        int sum = 0;
        while (n > 0) {
            sum += (n % 10);
            n /= 10;
        }
        return sum;
    }
}
```


```txt
4
22
27
58
85
94
121
...
9924
9942
9968
9975
9985
```



## JavaScript


### ES6

{{Trans|Haskell}}
{{Trans|Python}}

```JavaScript
(() => {
    'use strict';

    // isSmith :: Int -> Bool
    const isSmith = n => {
        const pfs = primeFactors(n);
        return (1 < pfs.length || n !== pfs[0]) && (
            sumDigits(n) === pfs.reduce(
                (a, x) => a + sumDigits(x),
                0
            )
        );
    };

    // TEST -----------------------------------------------

    // main :: IO ()
    const main = () => {

        // lowSmiths :: [Int]
        const lowSmiths = enumFromTo(2)(9999)
            .filter(isSmith);

        // lowSmithCount :: Int
        const lowSmithCount = lowSmiths.length;
        return [
            "Count of Smith Numbers below 10k:",
            show(lowSmithCount),
            "\nFirst 15 Smith Numbers:",
            unwords(take(15)(lowSmiths)),
            "\nLast 12 Smith Numbers below 10000:",
            unwords(drop(lowSmithCount - 12)(lowSmiths))
        ].join('\n');
    };

    // SMITH ----------------------------------------------

    // primeFactors :: Int -> [Int]
    const primeFactors = x => {
        const go = n => {
            const fs = take(1)(
                dropWhile(x => 0 != n % x)(
                    enumFromTo(2)(
                        floor(sqrt(n))
                    )
                )
            );
            return 0 === fs.length ? [n] : fs.concat(
                go(floor(n / fs[0]))
            );
        };
        return go(x);
    };

    // sumDigits :: Int -> Int
    const sumDigits = n =>
        unfoldl(
            x => 0 === x ? (
                Nothing()
            ) : Just(quotRem(x)(10))
        )(n).reduce((a, x) => a + x, 0);


    // GENERIC --------------------------------------------

    // Nothing :: Maybe a
    const Nothing = () => ({
        type: 'Maybe',
        Nothing: true,
    });

    // Just :: a -> Maybe a
    const Just = x => ({
        type: 'Maybe',
        Nothing: false,
        Just: x
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a => b => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // drop :: Int -> [a] -> [a]
    // drop :: Int -> String -> String
    const drop = n => xs =>
        xs.slice(n)


    // dropWhile :: (a -> Bool) -> [a] -> [a]
    // dropWhile :: (Char -> Bool) -> String -> String
    const dropWhile = p => xs => {
        const lng = xs.length;
        return 0 < lng ? xs.slice(
            until(i => i === lng || !p(xs[i]))(
                i => 1 + i
            )(0)
        ) : [];
    };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = m => n =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // floor :: Num -> Int
    const floor = Math.floor;


    // quotRem :: Int -> Int -> (Int, Int)
    const quotRem = m => n =>
        Tuple(Math.floor(m / n))(
            m % n
        );

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    // sqrt :: Num -> Num
    const sqrt = n =>
        (0 <= n) ? Math.sqrt(n) : undefined;

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = n => xs =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));


    // unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
    const unfoldl = f => v => {
        let
            xr = [v, v],
            xs = [];
        while (true) {
            const mb = f(xr[0]);
            if (mb.Nothing) {
                return xs
            } else {
                xr = mb.Just;
                xs = [xr[1]].concat(xs);
            }
        }
    };

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = p => f => x => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    return main();
})();
```

{{Out}}

```txt
Count of Smith Numbers below 10k:
376

First 15 Smith Numbers:
4 22 27 58 85 94 121 166 202 265 274 319 346 355 378

Last 12 Smith Numbers below 10000:
9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975 9985
```



## Julia


```julia
# v0.6

function sumdigits(n::Integer)
    sum = 0
    while n > 0
        sum += n % 10
        n = div(n, 10)
    end
    return sum
end

using Primes
issmith(n::Integer) = !isprime(n) && sumdigits(n) == sum(sumdigits(f) for f in factor(Vector, n))

smithnumbers = collect(n for n in 2:10000 if issmith(n))
println("Smith numbers up to 10000:\n$smithnumbers")
```


{{out}}

```txt
Smith numbers up to 10000:
[4, 22, 27, 58, 85, 94, 121, 166, 202, 265, 274, 319, 346, 355, 378, 382, 391, 438, 454, 483, 517, 526, 535,
562, 576, 588, 627, 634, 636, 645, 648, 654, 663, 666, 690, 706, 728, 729, 762, 778, 825, 852, 861, 895, 913,
915, 922, 958, 985, 1086, 1111, 1165, 1219, 1255, 1282, 1284, 1376, 1449, 1507, 1581, 1626, 1633, 1642, 1678,
1736, 1755, 1776, 1795, 1822, 1842, 1858, 1872, 1881, 1894, 1903, 1908, 1921, 1935, 1952, 1962, 1966, 2038,
2067, 2079, 2155, 2173, 2182, 2218, 2227, 2265, 2286, 2326, 2362, 2366, 2373, 2409, 2434, 2461, 2475, 2484,
2515, 2556, 2576, 2578, 2583, 2605, 2614, 2679, 2688, 2722, 2745, 2751, 2785, 2839, 2888, 2902, 2911, 2934,
2944, 2958, 2964, 2965, 2970, 2974, 3046, 3091, 3138, 3168, 3174, 3226, 3246, 3258, 3294, 3345, 3366, 3390,
3442, 3505, 3564, 3595, 3615, 3622, 3649, 3663, 3690, 3694, 3802, 3852, 3864, 3865, 3930, 3946, 3973, 4054,
4126, 4162, 4173, 4185, 4189, 4191, 4198, 4209, 4279, 4306, 4369, 4414, 4428, 4464, 4472, 4557, 4592, 4594,
4702, 4743, 4765, 4788, 4794, 4832, 4855, 4880, 4918, 4954, 4959, 4960, 4974, 4981, 5062, 5071, 5088, 5098,
5172, 5242, 5248, 5253, 5269, 5298, 5305, 5386, 5388, 5397, 5422, 5458, 5485, 5526, 5539, 5602, 5638, 5642,
5674, 5772, 5818, 5854, 5874, 5915, 5926, 5935, 5936, 5946, 5998, 6036, 6054, 6084, 6096, 6115, 6171, 6178,
6187, 6188, 6252, 6259, 6295, 6315, 6344, 6385, 6439, 6457, 6502, 6531, 6567, 6583, 6585, 6603, 6684, 6693,
6702, 6718, 6760, 6816, 6835, 6855, 6880, 6934, 6981, 7026, 7051, 7062, 7068, 7078, 7089, 7119, 7136, 7186,
7195, 7227, 7249, 7287, 7339, 7402, 7438, 7447, 7465, 7503, 7627, 7674, 7683, 7695, 7712, 7726, 7762, 7764,
7782, 7784, 7809, 7824, 7834, 7915, 7952, 7978, 8005, 8014, 8023, 8073, 8077, 8095, 8149, 8154, 8158, 8185,
8196, 8253, 8257, 8277, 8307, 8347, 8372, 8412, 8421, 8466, 8518, 8545, 8568, 8628, 8653, 8680, 8736, 8754,
8766, 8790, 8792, 8851, 8864, 8874, 8883, 8901, 8914, 9015, 9031, 9036, 9094, 9166, 9184, 9193, 9229, 9274,
9276, 9285, 9294, 9296, 9301, 9330, 9346, 9355, 9382, 9386, 9387, 9396, 9414, 9427, 9483, 9522, 9535, 9571,
9598, 9633, 9634, 9639, 9648, 9657, 9684, 9708, 9717, 9735, 9742, 9760, 9778, 9840, 9843, 9849, 9861, 9880,
9895, 9924, 9942, 9968, 9975, 9985]
```



## Kotlin

{{trans|FreeBASIC}}

```scala
// version 1.0.6

fun getPrimeFactors(n: Int): MutableList<Int> {
    val factors = mutableListOf<Int>()
    if (n < 2) return factors
    var factor = 2
    var nn = n
    while (true) {
        if (nn % factor == 0) {
            factors.add(factor)
            nn /= factor
            if (nn == 1) return factors
        }
        else if (factor >= 3) factor += 2
        else factor = 3
    }
}

fun sumDigits(n: Int): Int = when {
        n < 10 -> n
        else   -> {
            var sum = 0
            var nn = n
            while (nn > 0) {
                sum += (nn % 10)
                nn /= 10
            }
            sum
        }
    }

fun isSmith(n: Int): Boolean {
    if (n < 2) return false
    val factors = getPrimeFactors(n)
    if (factors.size == 1) return false
    val primeSum = factors.sumBy { sumDigits(it) }
    return sumDigits(n) == primeSum
}

fun main(args: Array<String>) {
    println("The Smith numbers below 10000 are:\n")
    var count = 0
    for (i in 2 until 10000) {
        if (isSmith(i)) {
            print("%5d".format(i))
            count++
        }
    }
    println("\n\n$count numbers found")
}
```


{{out}}

```txt

    4   22   27   58   85   94  121  166  202  265  274  319  346  355  378  382
  391  438  454  483  517  526  535  562  576  588  627  634  636  645  648  654
  663  666  690  706  728  729  762  778  825  852  861  895  913  915  922  958
  985 1086 1111 1165 1219 1255 1282 1284 1376 1449 1507 1581 1626 1633 1642 1678
 1736 1755 1776 1795 1822 1842 1858 1872 1881 1894 1903 1908 1921 1935 1952 1962
 1966 2038 2067 2079 2155 2173 2182 2218 2227 2265 2286 2326 2362 2366 2373 2409
 2434 2461 2475 2484 2515 2556 2576 2578 2583 2605 2614 2679 2688 2722 2745 2751
 2785 2839 2888 2902 2911 2934 2944 2958 2964 2965 2970 2974 3046 3091 3138 3168
 3174 3226 3246 3258 3294 3345 3366 3390 3442 3505 3564 3595 3615 3622 3649 3663
 3690 3694 3802 3852 3864 3865 3930 3946 3973 4054 4126 4162 4173 4185 4189 4191
 4198 4209 4279 4306 4369 4414 4428 4464 4472 4557 4592 4594 4702 4743 4765 4788
 4794 4832 4855 4880 4918 4954 4959 4960 4974 4981 5062 5071 5088 5098 5172 5242
 5248 5253 5269 5298 5305 5386 5388 5397 5422 5458 5485 5526 5539 5602 5638 5642
 5674 5772 5818 5854 5874 5915 5926 5935 5936 5946 5998 6036 6054 6084 6096 6115
 6171 6178 6187 6188 6252 6259 6295 6315 6344 6385 6439 6457 6502 6531 6567 6583
 6585 6603 6684 6693 6702 6718 6760 6816 6835 6855 6880 6934 6981 7026 7051 7062
 7068 7078 7089 7119 7136 7186 7195 7227 7249 7287 7339 7402 7438 7447 7465 7503
 7627 7674 7683 7695 7712 7726 7762 7764 7782 7784 7809 7824 7834 7915 7952 7978
 8005 8014 8023 8073 8077 8095 8149 8154 8158 8185 8196 8253 8257 8277 8307 8347
 8372 8412 8421 8466 8518 8545 8568 8628 8653 8680 8736 8754 8766 8790 8792 8851
 8864 8874 8883 8901 8914 9015 9031 9036 9094 9166 9184 9193 9229 9274 9276 9285
 9294 9296 9301 9330 9346 9355 9382 9386 9387 9396 9414 9427 9483 9522 9535 9571
 9598 9633 9634 9639 9648 9657 9684 9708 9717 9735 9742 9760 9778 9840 9843 9849
 9861 9880 9895 9924 9942 9968 9975 9985

376 numbers found

```



## Lua

Slightly long-winded prime factor function but it's a bit faster than the 'easy' way.

```Lua
-- Returns a boolean indicating whether n is prime
function isPrime (n)
    if n < 2 then return false end
    if n < 4 then return true end
    if n % 2 == 0 then return false end
    for d = 3, math.sqrt(n), 2 do
        if n % d == 0 then return false end
    end
    return true
end

-- Returns a table of the prime factors of n
function primeFactors (n)
    local pfacs, divisor = {}, 1
    if n < 1 then return pfacs end
    while not isPrime(n) do
        while not isPrime(divisor) do divisor = divisor + 1 end
        while n % divisor == 0 do
            n = n / divisor
            table.insert(pfacs, divisor)
        end
        divisor = divisor + 1
        if n == 1 then return pfacs end
    end
    table.insert(pfacs, n)
    return pfacs
end

-- Returns the sum of the digits of n
function sumDigits (n)
    local sum, nStr = 0, tostring(n)
    for digit = 1, nStr:len() do
        sum = sum + tonumber(nStr:sub(digit, digit))
    end
    return sum
end

-- Returns a boolean indicating whether n is a Smith number
function isSmith (n)
    if isPrime(n) then return false end
    local sumFacs = 0
    for _, v in ipairs(primeFactors(n)) do
        sumFacs = sumFacs + sumDigits(v)
    end
    return sumFacs == sumDigits(n)
end

-- Main procedure
for n = 1, 10000 do
    if isSmith(n) then io.write(n .. "\t") end
end
```

Seems silly to paste in all 376 numbers but rest assured the output agrees with https://oeis.org/A006753


## M2000 Interpreter

We make a 80X40 console, and prints 376 smith numbers, using 5 character column width, $(,5) leave first argument and pass second as column width. Using $(4,5) we can print proportional in columns (by default is 0, prints any font as monospaced font). In console we can mix any kind of text, bold, italics, colored and graphics too. Console is bitmap type, Text prints with transparent background, so to print over text, we have to clear first. This happen automatic with scrolling for last line (can be scroll reverse too). There are some variants for print statement and here we use Print Over to clear the line before, and we can make some temporary changes too.

We handle refresh from module (set fast! is for maximum speed), using refresh statement. We use Euler's Sieve, it is 10 times faster than Eratosthenes Sieve.

variable i used for For { } and change inside block, but structure For use own counter,so we get the right i (the next value), when block start again.

Not all factors calculated for a number, if sum of digits are greater than sum of digits of that number.

At the end we get a list (an inventory object with keys only). Print statement prints all keys (normally data, but if key isn't paired with data,then key is read only data)


```M2000 Interpreter

Module Checkit {
      Set Fast !
      Form 80, 40
      Refresh
      Function Smith(max=10000) {
            Function SumDigit(a$) {
                  def long sum
                  For i=1 to len(a$) {sum+=val(mid$(a$,i, 1)) }
                  =sum
            }
                  x=max
                  \\ Euler's Sieve
                        Dim r(x+1)=1
                        k=2
                        k2=k**2
                        While k2<x {
                              For m=k2 to x step k {r(m)=0}
                              Repeat {
                              k++ :  k2=k**2
                              } Until r(k)=1 or k2>x
                        }
            r(0)=0
            smith=0
            smith2=0
            lastI=0
            inventory smithnumbers
            Top=max div 100
            c=4
            For i=4 to max {
                if c> top then  print over $(0,6), ceil(i/max*100);"%" : Refresh : c=1
                c++
                  if r(i)=0 then {
                        smith=sumdigit(str$(i)) : lastI=i
                        smith2=0
                        do {
                              ii=int(sqrt(i))+1
                              do {  ii-- :   while r(ii)<>1 {ii--} } until i mod ii=0
                               if ii<2 then smith2+=sumdigit(str$(i)):exit
                               smith3=sumdigit(str$(ii))
                              do {
                                   smith2+=smith3
                                    i=i div ii : if ii<2  or i<2 then exit
                              } until  i mod ii<>0  or smith2>smith
                        } until i<2 or smith2>smith
                       If  smith=smith2 then Append smithnumbers, lastI
                  }
            }
            =smithnumbers
      }
      const MaxNumbers=10000
      numbers= Smith(MaxNumbers)
      Print
      Print $(,5), numbers
      Print
      Print format$(" {0} smith numbers found <= {1}", Len(numbers), MaxNumbers)
}
Checkit

```



## Maple


```Maple
isSmith := proc(n::posint)
local factors, sumofDigits, sumofFactorDigits, x;
if isprime(n) then
  return false;
else
  sumofDigits := add(x, x = convert(n, base, 10));
  sumofFactorDigits := add(map(x -> op(convert(x, base, 10)), [op(NumberTheory:-PrimeFactors(n))]));
  return evalb(sumofDigits = sumofFactorDigits);
end if;
end proc:

findSmith := proc(n::posint)
return select(isSmith, [seq(1 .. n - 1)]);
end proc:

findSmith(10000);
```

{{out}}

```txt
[4, 22, 27, 58, 85, 94, 121, 166, 202, 265, 274, 319, 346, 355, 378, 382, 391, 438, 454, 483, 517, 526, 535, 562, 576, 588, 627, 634, 636, 645, 648, 654, 663, 666, 690, 706, 728, 729, 762, 778, 825, 852, 861, 895, 913, 915, 922, 958, 985, 1086, 1111, 1165, 1219, 1255, 1282, 1284, 1376, 1449, 1507, 1581, 1626, 1633, 1642, 1678, 1736, 1755, 1776, 1795, 1822, 1842, 1858, 1872, 1881, 1894, 1903, 1908, 1921, 1935, 1952, 1962, 1966, 2038, 2067, 2079, 2155, 2173, 2182, 2218, 2227, 2265, 2286, 2326, 2362, 2366, 2373, 2409, 2434, 2461, 2475, 2484, 2515, 2556, 2576, 2578, 2583, 2605, 2614, 2679, 2688, 2722, 2745, 2751, 2785, 2839, 2888, 2902, 2911, 2934, 2944, 2958, 2964, 2965, 2970, 2974, 3046, 3091, 3138, 3168, 3174, 3226, 3246, 3258, 3294, 3345, 3366, 3390, 3442, 3505, 3564, 3595, 3615, 3622, 3649, 3663, 3690, 3694, 3802, 3852, 3864, 3865, 3930, 3946, 3973, 4054, 4126, 4162, 4173, 4185, 4189, 4191, 4198, 4209, 4279, 4306, 4369, 4414, 4428, 4464, 4472, 4557, 4592, 4594, 4702, 4743, 4765, 4788, 4794, 4832, 4855, 4880, 4918, 4954, 4959, 4960, 4974, 4981, 5062, 5071, 5088, 5098, 5172, 5242, 5248, 5253, 5269, 5298, 5305, 5386, 5388, 5397, 5422, 5458, 5485, 5526, 5539, 5602, 5638, 5642, 5674, 5772, 5818, 5854, 5874, 5915, 5926, 5935, 5936, 5946, 5998, 6036, 6054, 6084, 6096, 6115, 6171, 6178, 6187, 6188, 6252, 6259, 6295, 6315, 6344, 6385, 6439, 6457, 6502, 6531, 6567, 6583, 6585, 6603, 6684, 6693, 6702, 6718, 6760, 6816, 6835, 6855, 6880, 6934, 6981, 7026, 7051, 7062, 7068, 7078, 7089, 7119, 7136, 7186, 7195, 7227, 7249, 7287, 7339, 7402, 7438, 7447, 7465, 7503, 7627, 7674, 7683, 7695, 7712, 7726, 7762, 7764, 7782, 7784, 7809, 7824, 7834, 7915, 7952, 7978, 8005, 8014, 8023, 8073, 8077, 8095, 8149, 8154, 8158, 8185, 8196, 8253, 8257, 8277, 8307, 8347, 8372, 8412, 8421, 8466, 8518, 8545, 8568, 8628, 8653, 8680, 8736, 8754, 8766, 8790, 8792, 8851, 8864, 8874, 8883, 8901, 8914, 9015, 9031, 9036, 9094, 9166, 9184, 9193, 9229, 9274, 9276, 9285, 9294, 9296, 9301, 9330, 9346, 9355, 9382, 9386, 9387, 9396, 9414, 9427, 9483, 9522, 9535, 9571, 9598, 9633, 9634, 9639, 9648, 9657, 9684, 9708, 9717, 9735, 9742, 9760, 9778, 9840, 9843, 9849, 9861, 9880, 9895, 9924, 9942, 9968, 9975, 9985]
```



## Mathematica


```Mathematica
smithQ[n_] := Not[PrimeQ[n]] &&
            Total[IntegerDigits[n]] == Total[IntegerDigits /@ Flatten[ConstantArray @@@ FactorInteger[n]],2];

Select[Range[2, 10000], smithQ]
```


{{out}}

```txt
{4, 22, 27, 58, 85, 94, 121, 166, 202, 265, 274, 319, 346, 355, 378, 382, 391, 438, 454, 483, 517, 526, 535, 562, 576, 588, 627, 634, 636, 645, 648, 654, 663, 666, 690, 706, 728, 729, 762, 778, 825, 852, 861, 895, 913, 915, 922, 958, 985, 1086, 1111, 1165, 1219, 1255, 1282, 1284, 1376, 1449, 1507, 1581, 1626, 1633, 1642, 1678, 1736, 1755, 1776, 1795, 1822, 1842, 1858, 1872, 1881, 1894, 1903, 1908, 1921, 1935, 1952, 1962, 1966, 2038, 2067, 2079, 2155, 2173, 2182, 2218, 2227, 2265, 2286, 2326, 2362, 2366, 2373, 2409, 2434, 2461, 2475, 2484, 2515, 2556, 2576, 2578, 2583, 2605, 2614, 2679, 2688, 2722, 2745, 2751, 2785, 2839, 2888, 2902, 2911, 2934, 2944, 2958, 2964, 2965, 2970, 2974, 3046, 3091, 3138, 3168, 3174, 3226, 3246, 3258, 3294, 3345, 3366, 3390, 3442, 3505, 3564, 3595, 3615, 3622, 3649, 3663, 3690, 3694, 3802, 3852, 3864, 3865, 3930, 3946, 3973, 4054, 4126, 4162, 4173, 4185, 4189, 4191, 4198, 4209, 4279, 4306, 4369, 4414, 4428, 4464, 4472, 4557, 4592, 4594, 4702, 4743, 4765, 4788, 4794, 4832, 4855, 4880, 4918, 4954, 4959, 4960, 4974, 4981, 5062, 5071, 5088, 5098, 5172, 5242, 5248, 5253, 5269, 5298, 5305, 5386, 5388, 5397, 5422, 5458, 5485, 5526, 5539, 5602, 5638, 5642, 5674, 5772, 5818, 5854, 5874, 5915, 5926, 5935, 5936, 5946, 5998, 6036, 6054, 6084, 6096, 6115, 6171, 6178, 6187, 6188, 6252, 6259, 6295, 6315, 6344, 6385, 6439, 6457, 6502, 6531, 6567, 6583, 6585, 6603, 6684, 6693, 6702, 6718, 6760, 6816, 6835, 6855, 6880, 6934, 6981, 7026, 7051, 7062, 7068, 7078, 7089, 7119, 7136, 7186, 7195, 7227, 7249, 7287, 7339, 7402, 7438, 7447, 7465, 7503, 7627, 7674, 7683, 7695, 7712, 7726, 7762, 7764, 7782, 7784, 7809, 7824, 7834, 7915, 7952, 7978, 8005, 8014, 8023, 8073, 8077, 8095, 8149, 8154, 8158, 8185, 8196, 8253, 8257, 8277, 8307, 8347, 8372, 8412, 8421, 8466, 8518, 8545, 8568, 8628, 8653, 8680, 8736, 8754, 8766, 8790, 8792, 8851, 8864, 8874, 8883, 8901, 8914, 9015, 9031, 9036, 9094, 9166, 9184, 9193, 9229, 9274, 9276, 9285, 9294, 9296, 9301, 9330, 9346, 9355, 9382, 9386, 9387, 9396, 9414, 9427, 9483, 9522, 9535, 9571, 9598, 9633, 9634, 9639, 9648, 9657, 9684, 9708, 9717, 9735, 9742, 9760, 9778, 9840, 9843, 9849, 9861, 9880, 9895, 9924, 9942, 9968, 9975, 9985}
```


=={{header|Modula-2}}==

```modula2
MODULE SmithNumbers;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE SumDigits(n : INTEGER) : INTEGER;
VAR sum : INTEGER;
BEGIN
    sum := 0;
    WHILE n > 0 DO
        sum := sum + (n MOD 10);
        n := n DIV 10;
    END;
    RETURN sum;
END SumDigits;

VAR
    n,i,j,fc,sum,rc : INTEGER;
    buf : ARRAY[0..63] OF CHAR;
BEGIN
    rc := 0;
    FOR i:=1 TO 10000 DO
        n := i;
        fc := 0;
        sum := SumDigits(n);

        j := 2;
        WHILE n MOD j = 0 DO
            INC(fc);
            sum := sum - SumDigits(j);
            n := n DIV j;
        END;

        j := 3;
        WHILE j*j<=n DO
            WHILE n MOD j = 0 DO
                INC(fc);
                sum := sum - SumDigits(j);
                n := n DIV j;
            END;
            INC(j,2);
        END;

        IF n#1 THEN
            INC(fc);
            sum := sum - SumDigits(n);
        END;

        IF (fc>1) AND (sum=0) THEN
            FormatString("%4i  ", buf, i);
            WriteString(buf);
            INC(rc);
            IF rc=10 THEN
                rc := 0;
                WriteLn;
            END;
        END;
    END;

    ReadChar;
END SmithNumbers.
```



## Objeck


```objeck
use Collection;

class Test {
  function : Main(args : String[]) ~ Nil {
    for(n := 1; n < 10000; n+=1;) {
      factors := PrimeFactors(n);
      if(factors->Size() > 1) {
        sum := SumDigits(n);
        each(i : factors) {
          sum -= SumDigits(factors->Get(i));
        };

        if(sum = 0) {
          n->PrintLine();
        };
      };
    };
  }

  function : PrimeFactors(n : Int) ~ IntVector {
    result := IntVector->New();

    for(i := 2; n % i = 0; n /= i;) {
      result->AddBack(i);
    };

    for(i := 3; i * i <= n; i += 2;) {
      while(n % i = 0) {
        result->AddBack(i);
        n /= i;
      };
    };

    if(n <> 1) {
      result->AddBack(n);
    };

    return result;
  }

  function : SumDigits(n : Int) ~ Int {
    sum := 0;
    while(n > 0) {
      sum += (n % 10);
      n /= 10;
    };

    return sum;
  }
}
```



```txt

4
22
27
58
85
94
121
166
202
...
9975
9985

```



## PARI/GP


```parigp
isSmith(n)=my(f=factor(n)); if(#f~==1 && f[1,2]==1, return(0)); sum(i=1, #f~, sumdigits(f[i, 1])*f[i, 2]) == sumdigits(n);
select(isSmith, [1..9999])
```

{{out}}

```txt
%1 = [4, 22, 27, 58, 85, 94, 121, 166, 202, 265, 274, 319, 346, 355, 378, 382, 391, 438, 454, 483, 517, 526, 535, 562, 576, 588, 627, 634, 636, 645, 648, 654, 663, 666, 690, 706, 728, 729, 762, 778, 825, 852, 861, 895, 913, 915, 922, 958, 985, 1086, 1111, 1165, 1219, 1255, 1282, 1284, 1376, 1449, 1507, 1581, 1626, 1633, 1642, 1678, 1736, 1755, 1776, 1795, 1822, 1842, 1858, 1872, 1881, 1894, 1903, 1908, 1921, 1935, 1952, 1962, 1966, 2038, 2067, 2079, 2155, 2173, 2182, 2218, 2227, 2265, 2286, 2326, 2362, 2366, 2373, 2409, 2434, 2461, 2475, 2484, 2515, 2556, 2576, 2578, 2583, 2605, 2614, 2679, 2688, 2722, 2745, 2751, 2785, 2839, 2888, 2902, 2911, 2934, 2944, 2958, 2964, 2965, 2970, 2974, 3046, 3091, 3138, 3168, 3174, 3226, 3246, 3258, 3294, 3345, 3366, 3390, 3442, 3505, 3564, 3595, 3615, 3622, 3649, 3663, 3690, 3694, 3802, 3852, 3864, 3865, 3930, 3946, 3973, 4054, 4126, 4162, 4173, 4185, 4189, 4191, 4198, 4209, 4279, 4306, 4369, 4414, 4428, 4464, 4472, 4557, 4592, 4594, 4702, 4743, 4765, 4788, 4794, 4832, 4855, 4880, 4918, 4954, 4959, 4960, 4974, 4981, 5062, 5071, 5088, 5098, 5172, 5242, 5248, 5253, 5269, 5298, 5305, 5386, 5388, 5397, 5422, 5458, 5485, 5526, 5539, 5602, 5638, 5642, 5674, 5772, 5818, 5854, 5874, 5915, 5926, 5935, 5936, 5946, 5998, 6036, 6054, 6084, 6096, 6115, 6171, 6178, 6187, 6188, 6252, 6259, 6295, 6315, 6344, 6385, 6439, 6457, 6502, 6531, 6567, 6583, 6585, 6603, 6684, 6693, 6702, 6718, 6760, 6816, 6835, 6855, 6880, 6934, 6981, 7026, 7051, 7062, 7068, 7078, 7089, 7119, 7136, 7186, 7195, 7227, 7249, 7287, 7339, 7402, 7438, 7447, 7465, 7503, 7627, 7674, 7683, 7695, 7712, 7726, 7762, 7764, 7782, 7784, 7809, 7824, 7834, 7915, 7952, 7978, 8005, 8014, 8023, 8073, 8077, 8095, 8149, 8154, 8158, 8185, 8196, 8253, 8257, 8277, 8307, 8347, 8372, 8412, 8421, 8466, 8518, 8545, 8568, 8628, 8653, 8680, 8736, 8754, 8766, 8790, 8792, 8851, 8864, 8874, 8883, 8901, 8914, 9015, 9031, 9036, 9094, 9166, 9184, 9193, 9229, 9274, 9276, 9285, 9294, 9296, 9301, 9330, 9346, 9355, 9382, 9386, 9387, 9396, 9414, 9427, 9483, 9522, 9535, 9571, 9598, 9633, 9634, 9639, 9648, 9657, 9684, 9708, 9717, 9735, 9742, 9760, 9778, 9840, 9843, 9849, 9861, 9880, 9895, 9924, 9942, 9968, 9975, 9985]
```


{{works with|PARI/GP|2.6.0+}}
2.6.0 introduced the <code>forcomposite</code> iterator, removing the need to check each term for primality.

```parigp
forcomposite(n=4,9999, f=factor(n); if(#f~==1 && f[1,2]==1, next); if(sum(i=1, #f~, sumdigits(f[i, 1])*f[i, 2]) == sumdigits(n), print1(n" ")))
```


{{works with|PARI/GP|2.10.0+}}
2.10.0 gave us <code>forfactored</code> which speeds the process up by sieving for factors.

```parigp
forfactored(n=4,9999, f=n[2]; if(#f~==1 && f[1,2]==1, next); if(sum(i=1, #f~, sumdigits(f[i, 1])*f[i, 2]) == sumdigits(n[1]), print1(n[1]" ")))
```



## Pascal

{{works with|Free Pascal}}
Using a segmented sieve of erathostenes and mark every number with the index of its prime factor <= sqrt(number).
I use a presieved segment to reduce the time for small primes.
I thought, it would be a small speed improvement ;-)

the function IncDgtSum delivers the next sum of digits very fast (2.6 s for 1 to  1e9 )

```pascal
program SmithNum;
{$IFDEF FPC}
  {$MODE objFPC} //result and  useful for x64
  {$CODEALIGN PROC=64}
{$ENDIF}
uses
  sysutils;
type
  tdigit  = byte;
  tSum    = LongInt;
const
  base = 10;
  //maxDigitCnt *(base-1) <= High(tSum)
  //maxDigitCnt <= High(tSum) DIV (base-1);
  maxDigitCnt = 16;

  StartPrimNo = 6;
  csegsieveSIze = 2*3*5*7*11*13;//prime 0..5
type
  tDgtSum = record
              dgtNum : LongInt;
              dgtSum : tSum;
              dgts   : array[0..maxDigitCnt-1] of tdigit;
            end;
  tNumFactype = word;
  tnumFactor = record
                 numfacCnt: tNumFactype;
                 numfacts : array[1..15] of tNumFactype;
               end;
  tpnumFactor= ^tnumFactor;

  tsieveprim = record
                 spPrim   : Word;
                 spDgtsum : Word;
                 spOffset : LongWord;
               end;
  tpsieveprim = ^tsieveprim;

  tsievePrimarr  = array[0..6542-1] of tsieveprim;
  tsegmSieve     = array[1..csegsieveSIze] of tnumFactor;

var
  Primarr:tsievePrimarr;
  copySieve,
  actSieve : tsegmSieve;
  PrimDgtSum :tDgtSum;
  PrimCnt : NativeInt;

function IncDgtSum(var ds:tDgtSum):boolean;
//add 1 to dgts and corrects sum of Digits
//return if overflow happens
var
  i : NativeInt;
Begin
  i := High(ds.dgts);
  inc(ds.dgtNum);
  repeat
    IF ds.dgts[i] < Base-1 then
    //add one and done
    Begin
      inc(ds.dgts[i]);
      inc(ds.dgtSum);
      BREAK;
    end
    else
    Begin
      ds.dgts[i] := 0;
      dec(ds.dgtSum,Base-1);
    end;
    dec(i);
 until i < Low(ds.dgts);
 result := i < Low(ds.dgts)
end;

procedure OutDgtSum(const ds:tDgtSum);
var
  i : NativeInt;
Begin
  i := Low(ds.dgts);
  repeat
    write(ds.dgts[i]:3);
    inc(i);
  until i > High(ds.dgts);
  writeln(' sum of digits :  ',ds.dgtSum:3);
end;

procedure OutSieve(var s:tsegmSieve);
var
  i,j : NativeInt;
Begin
  For i := Low(s) to High(s) do
    with s[i] do
    Begin
      write(i:6,numfacCnt:4);
      For j := 1 to numfacCnt do
        write(numFacts[j]:5);
      writeln;
    end;
end;

procedure SieveForPrimes;
// sieve for all primes < High(Word)
var
  sieve : array of byte;
  pS : pByte;
  p,i   : NativeInt;
Begin
  setlength(sieve,High(Word));
  Fillchar(sieve[Low(sieve)],length(sieve),#0);
  pS:= @sieve[0]; //zero based
  dec(pS);// make it one based
  //sieve
  p := 2;
  repeat
    i := p*p;
    IF i> High(Word) then
      BREAK;
    repeat pS[i] := 1; inc(i,p); until i > High(Word);
    repeat inc(p) until pS[p] = 0;
  until false;
  //now fill array of primes
  fillchar(PrimDgtSum,SizeOf(PrimDgtSum),#0);
  IncDgtSum(PrimDgtSum);//1
  i := 0;
  For p := 2 to High(Word) do
  Begin
    IncDgtSum(PrimDgtSum);
    if pS[p] = 0 then
    Begin
      with PrimArr[i] do
      Begin
        spOffset := 2*p;//start at 2*prime
        spPrim   := p;
        spDgtsum := PrimDgtSum.dgtSum;
      end;
      inc(i);
    end;
  end;
  PrimCnt := i-1;
end;

procedure MarkWithPrime(SpIdx:NativeInt;var sf:tsegmSieve);
var
  i : NativeInt;
  pSf :^tnumFactor;
  MarkPrime : NativeInt;
Begin
  with Primarr[SpIdx] do
  Begin
    MarkPrime := spPrim;
    i :=  spOffSet;
    IF i <= csegsieveSize then
    Begin
      pSf := @sf[i];
      repeat
        pSf^.numFacts[pSf^.numfacCnt+1] := SpIdx;
        inc(pSf^.numfacCnt);
        inc(pSf,MarkPrime);
        inc(i,MarkPrime);
      until i > csegsieveSize;
    end;
    spOffset := i-csegsieveSize;
  end;
end;

procedure InitcopySieve(var cs:tsegmSieve);
var
  pr: NativeInt;
Begin
  fillchar(cs[Low(cs)],sizeOf(cs),#0);
  For Pr := 0 to 5 do
  Begin
    with Primarr[pr] do
     spOffset := spPrim;//mark the prime too
    MarkWithPrime(pr,cs);
  end;
end;

procedure MarkNextSieve(var s:tsegmSieve);
var
  idx: NativeInt;
Begin
  s:= copySieve;
  For idx := StartPrimNo to PrimCnt do
    MarkWithPrime(idx,s);
end;

function DgtSumInt(n: NativeUInt):NativeUInt;
var
  r : NativeUInt;
Begin
  result := 0;
  repeat
    r := n div base;
    inc(result,n-base*r);
    n := r
  until r = 0;
end;

{function DgtSumOfFac(pN: tpnumFactor;dgtNo:tDgtSum):boolean;}
function TestSmithNum(pN: tpnumFactor;dgtNo:tDgtSum):boolean;
var
  i,k,r,dgtSumI,dgtSumTarget : NativeUInt;
  pSp:tpsieveprim;
  pNumFact : ^tNumFactype;
Begin
  i := dgtNo.dgtNum;
  dgtSumTarget :=dgtNo.dgtSum;

  dgtSumI := 0;
  with pN^ do
  Begin
    k := numfacCnt;
    pNumFact := @numfacts[k];
  end;

  For k := k-1 downto 0 do
  Begin
    pSp := @PrimArr[pNumFact^];
    r := i DIV pSp^.spPrim;
    repeat
      i := r;
      r := r DIV pSp^.spPrim;
      inc(dgtSumI,pSp^.spDgtsum);
    until (i - r* pSp^.spPrim) <> 0;
    IF dgtSumI > dgtSumTarget then
    Begin
      result := false;
      EXIT;
    end;
    dec(pNumFact);
  end;
  If i <> 1 then
    inc(dgtSumI,DgtSumInt(i));
  result := dgtSumI = dgtSumTarget
end;

function CheckSmithNo(var s:tsegmSieve;var dgtNo:tDgtSum;Lmt:NativeInt=csegsieveSIze):NativeUInt;
var
  pNumFac : tpNumFactor;
  i : NativeInt;
Begin
  result := 0;
  i := low(s);
  pNumFac := @s[i];
  For i := i to lmt do
  Begin
    incDgtSum(dgtNo);
    IF pNumFac^.numfacCnt<> 0 then
      IF TestSmithNum(pNumFac,dgtNo) then
      Begin
        inc(result);
        //Mark as smith number
        inc(pNumFac^.numfacCnt,1 shl 15);
      end;
    inc(pNumFac);
  end;
end;

const
  limit = 100*1000*1000;
var
  actualNo :tDgtSum;
  i,s : NativeInt;
Begin
  SieveForPrimes;
  InitcopySieve(copySieve);
  i := 1;
  s:= -6;//- 2,3,5,7,11,13

  fillchar(actualNo,SizeOf(actualNo),#0);
  while i < Limit-csegsieveSize do
  Begin
    MarkNextSieve(actSieve);
    inc(s,CheckSmithNo(actSieve,actualNo));
    inc(i, csegsieveSize);
  end;
  //check the rest
  MarkNextSieve(actSieve);
  inc(s,CheckSmithNo(actSieve,actualNo,Limit-i+1));
  write(s:8,' smith-numbers up to ',actualNo.dgtnum:10);
end.

```
{{out}}

```txt
64-Bit FPC 3.1.1 -O3 -Xs  i4330 3.5 Ghz
       6 smith-numbers up to        100
      49 smith-numbers up to       1000
     376 smith-numbers up to      10000
    3294 smith-numbers up to     100000
   29928 smith-numbers up to    1000000 real   0m00.064s
  278411 smith-numbers up to   10000000 real   0m00.661s
 2632758 smith-numbers up to  100000000 real   0m06.981s
25154060 smith-numbers up to 1000000000 real   1m14.077s

  Number of Smith numbers below 10^n.     1
  1:1, 2:6, 3:49, 4:376, 5:3294, 6:29928, 7:278411, 8:2632758,
  9:25154060, 10:241882509, 11:2335807857, 12:22635291815,13:219935518608

```



## Perl

{{libheader|ntheory}}

```perl
use ntheory qw/:all/;
my @smith;
forcomposites {
  push @smith, $_  if sumdigits($_) == sumdigits(join("",factor($_)));
} 10000-1;
say scalar(@smith), " Smith numbers below 10000.";
say "@smith";
```

{{out}}

```txt
376 Smith numbers below 10000.
4 22 27 58 85 94 121 166 202 ... 9924 9942 9968 9975 9985
```


{{works with|ntheory|0.71+}}
Version 0.71 of the <code>ntheory</code> module added <code>forfactored</code>, similar to Pari/GP's 2.10.0 addition.  For large inputs this can halve the time taken compared to <code>forcomposites</code>.

```perl
use ntheory ":all";
my $t=0;
forfactored { $t++ if @_ > 1 && sumdigits($_) == sumdigits(join "",@_); } 10**8;
say $t;
```



## Perl 6


```perl6
constant @primes = 2, |(3, 5, 7 ... *).grep: *.is-prime;

multi factors ( 1 ) { 1 }
multi factors ( Int $remainder is copy ) {
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
# Code above here is verbatim from RC:Count_in_factors#Perl6

sub is_smith_number ( Int $n ) {
  (!$n.is-prime) and ( [+] $n.comb ) == ( [+] factors($n).join.comb );
}

my @s = grep &is_smith_number, 2 ..^ 10_000;
say "{@s.elems} Smith numbers below 10_000";
say 'First 10: ', @s[  ^10      ];
say 'Last  10: ', @s[ *-10 .. * ];
```

{{out}}

```txt
376 Smith numbers below 10_000
First 10: (4 22 27 58 85 94 121 166 202 265)
Last  10: (9843 9849 9861 9880 9895 9924 9942 9968 9975 9985)
```



## Phix

Note that the builtin prime_factors(4) yields {2}, rather than {2,2}, hence the inner loop (admittedly repeat..until style would be better, if only Phix had that).

```Phix
function sum_digits(integer n, integer base=10)
integer res = 0
    while n do
        res += remainder(n,base)
        n = floor(n/base)
    end while
    return res
end function

function smith(integer n)
    sequence p = prime_factors(n)
    integer sp = 0, w = n
    for i=1 to length(p) do
        integer pi = p[i],
                spi = sum_digits(pi)
        while mod(w,pi)=0 do
            sp += spi
            w = floor(w/pi)
        end while
    end for
    return sum_digits(n)=sp
end function

sequence s = {}
for i=1 to 10000 do
    if smith(i) then s &= i end if
end for
?length(s)
s[8..-8] = {"..."}
?s
```


```txt

376
{4,22,27,58,85,94,121,"...",9880,9895,9924,9942,9968,9975,9985}

```



## PicoLisp


```PicoLisp
(de factor (N)
   (make
      (let (D 2  L (1 2 2 . (4 2 4 2 4 6 2 6 .))  M (sqrt N))
         (while (>= M D)
            (if (=0 (% N D))
               (setq M (sqrt (setq N (/ N (link D)))))
               (inc 'D (pop 'L)) ) )
         (link N) ) ) )
(de sumdigits (N)
   (sum format (chop N)) )
(de smith (X)
   (make
      (for N X
         (let R (factor N)
            (and
               (cdr R)
               (= (sum sumdigits R) (sumdigits N))
               (link N) ) ) ) ) )
(let L (smith 10000)
   (println 'first-10 (head 10 L))
   (println 'last-10 (tail 10 L))
   (println 'all (length L)) )
```

{{out}}

```txt
first-10 (4 22 27 58 85 94 121 166 202 265)
last-10 (9843 9849 9861 9880 9895 9924 9942 9968 9975 9985)
all 376
```



## PureBasic


```PureBasic
DisableDebugger
#ECHO=#True ; #True: Print all results
Global NewList f.i()

Procedure.i ePotenz(Wert.i)
  Define.i var=Wert, i
  While var
    i+1
    var/10
  Wend
  ProcedureReturn i
EndProcedure

Procedure.i n_Element(Wert.i,Stelle.i=1)
  If Stelle>0
    ProcedureReturn (Wert%Int(Pow(10,Stelle))-Wert%Int(Pow(10,Stelle-1)))/Int(Pow(10,Stelle-1))
  Else
    ProcedureReturn 0
  EndIf
EndProcedure

Procedure.i qSumma(Wert.i)
  Define.i sum, pos
  For pos=1 To ePotenz(Wert)
    sum+ n_Element(Wert,pos)
  Next pos
  ProcedureReturn sum
EndProcedure

Procedure.b IsPrime(n.i)
  Define.i i=5
  If n<2 : ProcedureReturn #False : EndIf
  If n%2=0 : ProcedureReturn Bool(n=2) : EndIf
  If n%3=0 : ProcedureReturn Bool(n=3) : EndIf
  While i*i<=n
    If n%i=0 : ProcedureReturn #False : EndIf
    i+2
    If n%i=0 : ProcedureReturn #False : EndIf
    i+4
  Wend
  ProcedureReturn #True
EndProcedure

Procedure PFZ(n.i,pf.i=2)
  If n>1 And n<>pf
    If n%pf=0
      AddElement(f()) : f()=pf
      PFZ(n/pf,pf)
    Else
      While Not IsPrime(pf+1) : pf+1 : Wend
      PFZ(n,pf+1)
    EndIf
  ElseIf n=pf
    AddElement(f()) : f()=pf
  EndIf
EndProcedure

OpenConsole("Smith numbers")
;upto=100 : sn=0 : Gosub Smith_loop
;upto=1000 : sn=0 : Gosub Smith_loop
upto=10000 : sn=0 : Gosub Smith_loop
Input()
End

Smith_loop:
  For i=2 To upto
    ClearList(f()) : qs=0
    PFZ(i)
    CompilerIf #ECHO : Print(Str(i)+~": \t") : CompilerEndIf
    ForEach f()
      CompilerIf #ECHO : Print(Str(F())+~"\t") : CompilerEndIf
      qs+qSumma(f())
    Next
    If ListSize(f())>1 And qSumma(i)=qs
      CompilerIf #ECHO : Print("SMITH-NUMBER") : CompilerEndIf
      sn+1
    EndIf
    CompilerIf #ECHO : PrintN("") : CompilerEndIf
  Next
  Print(~"\n"+Str(sn)+" Smith number up to "+Str(upto))
Return
```

{{out}}

```txt
.
.
.
9975:   3       5       5       7       19      SMITH-NUMBER
9976:   2       2       2       29      43
9977:   11      907
9978:   2       3       1663
9979:   17      587
9980:   2       2       5       499
9981:   3       3       1109
9982:   2       7       23      31
9983:   67      149
9984:   2       2       2       2       2       2       2       2       3       13
9985:   5       1997    SMITH-NUMBER
9986:   2       4993
9987:   3       3329
9988:   2       2       11      227
9989:   7       1427
9990:   2       3       3       3       5       37
9991:   97      103
9992:   2       2       2       1249
9993:   3       3331
9994:   2       19      263
9995:   5       1999
9996:   2       2       3       7       7       17
9997:   13      769
9998:   2       4999
9999:   3       3       11      101
10000:  2       2       2       2       5       5       5       5

376 Smith number up To 10000
```



## Python


### Procedural


```python
from sys import stdout


def factors(n):
    rt = []
    f = 2
    if n == 1:
        rt.append(1);
    else:
        while 1:
            if 0 == ( n % f ):
                rt.append(f);
                n //= f
                if n == 1:
                    return rt
            else:
                f += 1
    return rt


def sum_digits(n):
    sum = 0
    while n > 0:
        m = n % 10
        sum += m
        n -= m
        n //= 10

    return sum


def add_all_digits(lst):
    sum = 0
    for i in range (len(lst)):
        sum += sum_digits(lst[i])

    return sum


def list_smith_numbers(cnt):
    for i in range(4, cnt):
        fac = factors(i)
        if len(fac) > 1:
            if sum_digits(i) == add_all_digits(fac):
                stdout.write("{0} ".format(i) )

# entry point
list_smith_numbers(10_000)
```

{{out}}
```txt

4 22 27 58 85 94 121 166 202 265 274 319 346 355 378 382 391 438 454 483 517 526 535 562 576 588 627 634 636 645 648 654 663 666
...
9535 9571 9598 9633 9634 9639 9648 9657 9684 9708 9717 9735 9742 9760 9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975 9985
```




### Functional

{{Works with|Python|3.7}}

```python
'''Smith numbers'''

from itertools import dropwhile
from functools import reduce
from math import floor, sqrt


# isSmith :: Int -> Bool
def isSmith(n):
    '''True if n is a Smith number.'''
    pfs = primeFactors(n)
    return (1 < len(pfs) or n != pfs[0]) and (
        sumDigits(n) == reduce(
            lambda a, x: a + sumDigits(x),
            pfs, 0
        )
    )


# primeFactors :: Int -> [Int]
def primeFactors(x):
    '''List of prime factors of x'''
    def go(n):
        fs = list(dropwhile(
            mod(n),
            range(2, 1 + floor(sqrt(n)))
        ))[0:1]

        return fs + go(floor(n / fs[0])) if fs else [n]
    return go(x)


# sumDigits :: Int -> Int
def sumDigits(n):
    '''The sum of the decimal digits of n'''
    def f(x):
        return Just(divmod(x, 10)) if x else Nothing()
    return sum(unfoldl(f)(n))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Count and samples of Smith numbers below 10k'''

    lowSmiths = [x for x in range(2, 10000) if isSmith(x)]
    lowSmithCount = len(lowSmiths)

    print('\n'.join([
        'Count of Smith Numbers below 10k:',
        str(lowSmithCount),
        '\nFirst 15 Smith Numbers:',
        ' '.join(str(x) for x in lowSmiths[0:15]),
        '\nLast 12 Smith Numbers below 10000:',
        ' '.join(str(x) for x in lowSmiths[lowSmithCount - 12:])
    ]))


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.
       Wrapper containing the result of a computation.
    '''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.
       Empty wrapper returned where a computation is not possible.
    '''
    return {'type': 'Maybe', 'Nothing': True}


# mod :: Int -> Int -> Int
def mod(n):
    '''n modulo d'''
    return lambda d: n % d


# unfoldl(lambda x: Just(((x - 1), x)) if 0 != x else Nothing())(10)
# -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
def unfoldl(f):
    '''Dual to reduce or foldl.
       Where these reduce a list to a summary value, unfoldl
       builds a list from a seed value.
       Where f returns Just(a, b), a is appended to the list,
       and the residual b is used as the argument for the next
       application of f.
       When f returns Nothing, the completed list is returned.
    '''
    def go(v):
        x, r = v, v
        xs = []
        while True:
            mb = f(x)
            if mb.get('Nothing'):
                return xs
            else:
                x, r = mb.get('Just')
                xs.insert(0, r)
        return xs
    return lambda x: go(x)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Count of Smith Numbers below 10k:
376

First 15 Smith Numbers:
4 22 27 58 85 94 121 166 202 265 274 319 346 355 378

Last 12 Smith Numbers below 10000:
9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975 9985
```



## Racket


```racket
#lang racket
(require math/number-theory)

(define (sum-of-digits n)
  (let inr ((n n) (s 0))
    (if (zero? n) s (let-values (([q r] (quotient/remainder n 10))) (inr q (+ s r))))))

(define (smith-number? n)
  (and (not (prime? n))
       (= (sum-of-digits n)
          (for/sum ((pe (in-list (factorize n))))
            (* (cadr pe) (sum-of-digits (car pe)))))))

(module+ test
  (require rackunit)
  (check-equal? (sum-of-digits 0) 0)
  (check-equal? (sum-of-digits 33) 6)
  (check-equal? (sum-of-digits 30) 3)

  (check-true (smith-number? 166)))

(module+ main
  (let loop ((ns (filter smith-number? (range 1 (add1 10000)))))
    (unless (null? ns)
      (let-values (([l r] (split-at ns (min (length ns) 15))))
        (displayln l)
        (loop r)))))
```


{{out}}

```txt
(4 22 27 58 85 94 121 166 202 265 274 319 346 355 378)
(382 391 438 454 483 517 526 535 562 576 588 627 634 636 645)
(648 654 663 666 690 706 728 729 762 778 825 852 861 895 913)
```

&hellip;

```txt
(9396 9414 9427 9483 9522 9535 9571 9598 9633 9634 9639 9648 9657 9684 9708)
(9717 9735 9742 9760 9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975)
(9985)
```



## REXX


### unoptimized


```rexx
/*REXX program  finds  (and maybe displays)  Smith  (or joke)  numbers up to a given  N.*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=10000                  /*Not specified?  Then use the default.*/
tell= (N>0);            N=abs(N) - 1             /*use the  │N│  for computing  (below).*/
w=length(N)                                      /*W:  used for aligning Smith numbers. */
#=0                                              /*#:  Smith numbers found  (so far).   */
@=;  do j=4  to  N;                              /*process almost all numbers up to  N. */
     if sumD(j) \== sumfactr(j)  then iterate    /*Not a Smith number?   Then ignore it.*/
     #=#+1                                       /*bump the Smith number counter.       */
     if \tell  then iterate                      /*Not showing the numbers? Keep looking*/
     @=@ right(j, w);         if length(@)>199  then do;    say substr(@, 2);    @=;   end
     end   /*j*/                                 /* [↑]  if N>0,  then display Smith #s.*/

if @\==''  then say substr(@, 2)                 /*if any residual Smith #s, display 'em*/
say                                              /* [↓]  display the number of Smith #s.*/
say #    ' Smith numbers found  ≤ '   N"."       /*display number of Smith numbers found*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sumD:     parse arg x 1 s 2;   do d=2  for length(x)-1; s=s+substr(x,d,1); end;   return s
/*──────────────────────────────────────────────────────────────────────────────────────*/
sumFactr: procedure;  parse arg z;       $=0;    f=0             /*obtain the Z number. */
             do  while z//2==0;  $=$+2;  f=f+1;  z=z% 2;  end    /*maybe add factor of 2*/
             do  while z//3==0;  $=$+3;  f=f+1;  z=z% 3;  end    /*  "    "     "    " 3*/
                                                                 /*                  ___*/
             do j=5  by 2  while j<=z  &  j*j<=n                 /*minimum of Z or  √ N */
             if j//3==0  then iterate                            /*skip factors that ÷ 3*/
                do while z//j==0; f=f+1; $=$+sumD(j); z=z%j; end /*maybe reduce  Z by J */
             end   /*j*/                                         /* [↓]  Z:  what's left*/
          if z\==1  then do;      f=f+1; $=$+sumD(z);        end /*Residual?  Then add Z*/
          if f<2    then return 0                                /*Prime?   Not a Smith#*/
                         return $                                /*else return sum digs.*/
```

{{out|output|text=  when using the default input:}}

(Shown at   <big>'''<sup>2</sup>/<sub>3</sub>'''</big>   size.)
<pre style="font-size:67%">
   4   22   27   58   85   94  121  166  202  265  274  319  346  355  378  382  391  438  454  483  517  526  535  562  576  588  627  634  636  645  648  654  663  666  690  706  728  729  762  778
 825  852  861  895  913  915  922  958  985 1086 1111 1165 1219 1255 1282 1284 1376 1449 1507 1581 1626 1633 1642 1678 1736 1755 1776 1795 1822 1842 1858 1872 1881 1894 1903 1908 1921 1935 1952 1962
1966 2038 2067 2079 2155 2173 2182 2218 2227 2265 2286 2326 2362 2366 2373 2409 2434 2461 2475 2484 2515 2556 2576 2578 2583 2605 2614 2679 2688 2722 2745 2751 2785 2839 2888 2902 2911 2934 2944 2958
2964 2965 2970 2974 3046 3091 3138 3168 3174 3226 3246 3258 3294 3345 3366 3390 3442 3505 3564 3595 3615 3622 3649 3663 3690 3694 3802 3852 3864 3865 3930 3946 3973 4054 4126 4162 4173 4185 4189 4191
4198 4209 4279 4306 4369 4414 4428 4464 4472 4557 4592 4594 4702 4743 4765 4788 4794 4832 4855 4880 4918 4954 4959 4960 4974 4981 5062 5071 5088 5098 5172 5242 5248 5253 5269 5298 5305 5386 5388 5397
5422 5458 5485 5526 5539 5602 5638 5642 5674 5772 5818 5854 5874 5915 5926 5935 5936 5946 5998 6036 6054 6084 6096 6115 6171 6178 6187 6188 6252 6259 6295 6315 6344 6385 6439 6457 6502 6531 6567 6583
6585 6603 6684 6693 6702 6718 6760 6816 6835 6855 6880 6934 6981 7026 7051 7062 7068 7078 7089 7119 7136 7186 7195 7227 7249 7287 7339 7402 7438 7447 7465 7503 7627 7674 7683 7695 7712 7726 7762 7764
7782 7784 7809 7824 7834 7915 7952 7978 8005 8014 8023 8073 8077 8095 8149 8154 8158 8185 8196 8253 8257 8277 8307 8347 8372 8412 8421 8466 8518 8545 8568 8628 8653 8680 8736 8754 8766 8790 8792 8851
8864 8874 8883 8901 8914 9015 9031 9036 9094 9166 9184 9193 9229 9274 9276 9285 9294 9296 9301 9330 9346 9355 9382 9386 9387 9396 9414 9427 9483 9522 9535 9571 9598 9633 9634 9639 9648 9657 9684 9708
9717 9735 9742 9760 9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975 9985

376  Smith numbers found  ≤  9999.

```



### optimized

This REXX version uses a faster version of the   '''sumFactr'''   function;   it's over   '''20'''   times faster than the

unoptimized version using a (negative) one million for   '''N'''.

```rexx
/*REXX program  finds  (and maybe displays)  Smith  (or joke)  numbers up to a given  N.*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=10000                  /*Not specified?  Then use the default.*/
tell= (N>0);            N=abs(N) - 1             /*use the  │N│  for computing  (below).*/
#=0                                              /*the number of Smith numbers (so far).*/
w=length(N)                                      /*W:  used for aligning Smith numbers. */
@=;    do j=4  for  max(0, N-3)                  /*process almost all numbers up to  N. */
       if sumD(j) \== sumFactr(j)  then iterate  /*Not a Smith number?   Then ignore it.*/
       #=#+1                                     /*bump the Smith number counter.       */
       if \tell  then iterate                    /*Not showing the numbers? Keep looking*/
       @=@ right(j, w);        if length(@)>199 then do;   say substr(@, 2);    @=;    end
       end   /*j*/                               /* [↑]  if N>0,  then display Smith #s.*/

if @\==''  then say substr(@, 2)                 /*if any residual Smith #s, display 'em*/
say                                              /* [↓]  display the number of Smith #s.*/
say #   ' Smith numbers found  ≤ '  max(0,N)"."  /*display number of Smith numbers found*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sumD:     parse arg x 1 s 2;   do d=2  for length(x)-1; s=s+substr(x,d,1); end;   return s
/*──────────────────────────────────────────────────────────────────────────────────────*/
sumFactr: procedure;  parse arg z;      $=0;   f=0           /*obtain  Z  number (arg1).*/
             do  while z// 2==0; $=$+ 2; f=f+1; z=z% 2;  end /*maybe add factor of   2. */
             do  while z// 3==0; $=$+ 3; f=f+1; z=z% 3;  end /*  "    "     "    "   3. */
             do  while z// 5==0; $=$+ 5; f=f+1; z=z% 5;  end /*  "    "     "    "   5. */
             do  while z// 7==0; $=$+ 7; f=f+1; z=z% 7;  end /*  "    "     "    "   7. */
          t=z;  r=0;  q=1;       do while q<=t; q=q*4;   end /*R:  will be the iSqrt(Z).*/
             do while q>1;  q=q%4;  _=t-r-q;  r=r%2;  if _>=0  then do;  t=_;  r=r+q;  end
             end   /*while q>1*/                             /* [↑] compute int. SQRT(Z)*/

             do j=11  by 6  to r  while j<=z                 /*skip factors that are ÷ 3*/
             parse var  j  ''  -1  _;     if _\==5 then,     /*is last dec. digit ¬a 5 ?*/
               do  while z//j==0; f=f+1; $=$+sumD(j); z=z%j; end   /*maybe reduce Z by J*/
             if _==3  then iterate;      y=j+2
               do  while z//y==0; f=f+1; $=$+sumD(y); z=z%y; end   /*maybe reduce Z by Y*/
             end   /*j*/                                     /* [↓]  Z  is what's left. */
          if z\==1  then do;      f=f+1; $=$+sumD(z);  end   /*if a residual, then add Z*/
          if f<2    then return 0                            /*Is prime? It's not Smith#*/
                         return $                            /*else, return sum of digs.*/
```

{{out|output|text=  when using the input of (negative) one million:     <tt> -1000000 </tt>}}

```txt

29928  Smith numbers found  ≤  999999.

```



## Ring


{{incorrect|Ring|

 This program does not find   (nor show)   all the Smith numbers <big> &lt; </big> 10,000.

}}


```ring

# Project : Smith numbers

see "All the Smith Numbers < 1000 are:" + nl

for prime = 1 to 1000
    decmp = []
    sum1 = sumDigits(prime)
    decomp(prime)
    sum2 = 0
    if len(decmp)>1
       for n=1 to len(decmp)
           cstr = string(decmp[n])
           for m= 1 to len(cstr)
               sum2 = sum2 + number(cstr[m])
           next
       next
    ok
    if sum1 = sum2
       see "" + prime + " "
    ok
next

func decomp nr
     for i = 1 to nr
         if isPrime(i) and nr % i = 0
            add(decmp, i)
            pr = i
            while true
                  pr = pr * i
                  if nr%pr = 0
                     add(decmp, i)
                  else
                     exit
                  ok
            end
         ok
     next

func isPrime num
     if (num <= 1) return 0 ok
        if (num % 2 = 0 and num != 2) return 0 ok
        for i = 3 to floor(num / 2) -1 step 2
            if (num % i = 0) return 0 ok
        next
        return 1

func sumDigits n
     sum = 0
     while n > 0.5
           m = floor(n / 10)
           digit = n - m * 10
           sum = sum + digit
           n = m
     end
     return sum

```

Output:

```txt

All the Smith Numbers < 1000 are:
4 22 27 58 85 94 121 166 202 265 274 319 346 355 378 382 391 438 454 483 517 526 535 562 576 588 627 634 636 645 648 654 663 666 690 706 728 729 762 778 825 852 861 895 913 915 922 958 985

```



## Ruby


```ruby
require "prime"

class Integer

  def smith?
    return false if prime?
    digits.sum == prime_division.map{|pr,n| pr.digits.sum * n}.sum
  end

end

n   = 10_000
res = 1.upto(n).select(&:smith?)

puts "#{res.size} smith numbers below #{n}:
#{res.first(5).join(", ")},... #{res.last(5).join(", ")}"
```

{{out}}

```txt

376 smith numbers below 10000:
4, 22, 27, 58, 85,... 9924, 9942, 9968, 9975, 9985

```



## Rust


```Rust
fn main () {
    //We just need the primes below 100
    let primes = vec![2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97];
    let mut solution = Vec::new();
    let mut number;
    for i in 4..10000 {
        //Factorize each number below 10.000
        let mut prime_factors = Vec::new();
        number = i;
        for j in &primes {
            while number % j == 0 {
                number = number / j;
                prime_factors.push(j);
            }
            if number == 1 { break; }
        }
        //Number is 1 (not a prime factor) if the factorization is complete or a prime bigger than 100
        if number != 1 { prime_factors.push(&number); }
        //Avoid the prime numbers
        if prime_factors.len() < 2 { continue; }
        //Check the smith number definition
        if prime_factors.iter().fold(0, |n,x| n + x.to_string().chars().map(|d| d.to_digit(10).unwrap()).fold(0, |n,x| n + x))
            == i.to_string().chars().map(|d| d.to_digit(10).unwrap()).fold(0, |n,x| n + x) {
            solution.push(i);
        }
    }
    println!("Smith numbers below 10000 ({}) : {:?}",solution.len(), solution);
}
```


{{out}}

```txt
Smith numbers below 10000 (376) : [4, 22, 27, 58, 85, 94, 121, 166, 202, 265, 274, 319, 346, 355, 378, 382, 391, 438, 454, 483, 517, 526, 535, 562, 576, 588, 627, 634, 636, 645, 648, 654, 663, 666, 690, 706, 728, 729, 762, 778, 825, 852, 861, 895, 913, 915, 922, 958, 985, 1086, 1111, 1165, 1219, 1255, 1282, 1284, 1376, 1449, 1507, 1581, 1626, 1633, 1642, 1678, 1736, 1755, 1776, 1795, 1822, 1842, 1858, 1872, 1881, 1894, 1903, 1908, 1921, 1935, 1952, 1962, 1966, 2038, 2067, 2079, 2155, 2173, 2182, 2218, 2227, 2265, 2286, 2326, 2362, 2366, 2373, 2409, 2434, 2461, 2475, 2484, 2515, 2556, 2576, 2578, 2583, 2605, 2614, 2679, 2688, 2722, 2745, 2751, 2785, 2839, 2888, 2902, 2911, 2934, 2944, 2958, 2964, 2965, 2970, 2974, 3046, 3091, 3138, 3168, 3174, 3226, 3246, 3258, 3294, 3345, 3366, 3390, 3442, 3505, 3564, 3595, 3615, 3622, 3649, 3663, 3690, 3694, 3802, 3852, 3864, 3865, 3930, 3946, 3973, 4054, 4126, 4162, 4173, 4185, 4189, 4191, 4198, 4209, 4279, 4306, 4369, 4414, 4428, 4464, 4472, 4557, 4592, 4594, 4702, 4743, 4765, 4788, 4794, 4832, 4855, 4880, 4918, 4954, 4959, 4960, 4974, 4981, 5062, 5071, 5088, 5098, 5172, 5242, 5248, 5253, 5269, 5298, 5305, 5386, 5388, 5397, 5422, 5458, 5485, 5526, 5539, 5602, 5638, 5642, 5674, 5772, 5818, 5854, 5874, 5915, 5926, 5935, 5936, 5946, 5998, 6036, 6054, 6084, 6096, 6115, 6171, 6178, 6187, 6188, 6252, 6259, 6295, 6315, 6344, 6385, 6439, 6457, 6502, 6531, 6567, 6583, 6585, 6603, 6684, 6693, 6702, 6718, 6760, 6816, 6835, 6855, 6880, 6934, 6981, 7026, 7051, 7062, 7068, 7078, 7089, 7119, 7136, 7186, 7195, 7227, 7249, 7287, 7339, 7402, 7438, 7447, 7465, 7503, 7627, 7674, 7683, 7695, 7712, 7726, 7762, 7764, 7782, 7784, 7809, 7824, 7834, 7915, 7952, 7978, 8005, 8014, 8023, 8073, 8077, 8095, 8149, 8154, 8158, 8185, 8196, 8253, 8257, 8277, 8307, 8347, 8372, 8412, 8421, 8466, 8518, 8545, 8568, 8628, 8653, 8680, 8736, 8754, 8766, 8790, 8792, 8851, 8864, 8874, 8883, 8901, 8914, 9015, 9031, 9036, 9094, 9166, 9184, 9193, 9229, 9274, 9276, 9285, 9294, 9296, 9301, 9330, 9346, 9355, 9382, 9386, 9387, 9396, 9414, 9427, 9483, 9522, 9535, 9571, 9598, 9633, 9634, 9639, 9648, 9657, 9684, 9708, 9717, 9735, 9742, 9760, 9778, 9840, 9843, 9849, 9861, 9880, 9895, 9924, 9942, 9968, 9975, 9985]

real	0m0.014s
user	0m0.014s
sys	0m0.000s
```


## Scala


```Scala
object SmithNumbers extends App {

  def sumDigits(_n: Int): Int = {
    var n = _n
    var sum = 0
    while (n > 0) {
      sum += (n % 10)
      n /= 10
    }
    sum
  }

  def primeFactors(_n: Int): List[Int] = {
    var n = _n
    val result = new collection.mutable.ListBuffer[Int]
    val i = 2
    while (n % i == 0) {
      result += i
      n /= i
    }
    var j = 3
    while (j * j <= n) {
      while (n % j == 0) {
        result += i
        n /= j
      }
      j += 2
    }
    if (n != 1) result += n
    result.toList
  }

  for (n <- 1 until 10000) {
    val factors = primeFactors(n)
    if (factors.size > 1) {
      var sum = sumDigits(n)
      for (f <- factors) sum -= sumDigits(f)
      if (sum == 0) println(n)
    }
  }

}
```



## Sidef

{{trans|Perl 6}}

```ruby
var primes = Enumerator({ |callback|
    static primes = Hash()
    var p = 2
    loop {
        callback(p)
        p = (primes{p} := p.next_prime)
    }
})

func factors(remainder) {

    remainder == 1 && return([remainder])

    gather {
        primes.each { |factor|
            if (factor*factor > remainder) {
                take(remainder) if (remainder > 1)
                break
            }

            while (factor.divides(remainder)) {
                take(factor)
                break if ((remainder /= factor) == 1)
            }
        }
    }
}

func is_smith_number(n) {
    !n.is_prime && (n.digits.sum == factors(n).join.to_i.digits.sum)
}

var s = range(2, 10_000).grep { is_smith_number(_) }
say "#{s.len} Smith numbers below 10_000"
say "First 10: #{s.first(10)}"
say "Last  10: #{s.last(10)}"
```

{{out}}

```txt

376 Smith numbers below 10_000
First 10: [4, 22, 27, 58, 85, 94, 121, 166, 202, 265]
Last  10: [9843, 9849, 9861, 9880, 9895, 9924, 9942, 9968, 9975, 9985]

```



## Stata


```stata
function factor(_n) {
	n = _n
	a = J(14, 2, .)
	i = 0
	if (mod(n, 2)==0) {
		j = 0
		while (mod(n, 2)==0) {
			j++
			n = n/2
		}
		i++
		a[i,1] = 2
		a[i,2] = j
	}
	for (k=3; k*k<=n; k=k+2) {
		if (mod(n, k)==0) {
			j = 0
			while (mod(n, k)==0) {
				j++
				n = n/k
			}
			i++
			a[i,1] = k
			a[i,2] = j
		}
	}
	if (n>1) {
		i++
		a[i,1] = n
		a[i,2] = 1
	}
	return(a[1::i,.])
}

function sumdigits(_n) {
	n = _n
	for (s=0; n>0; n=floor(n/10)) s = s+mod(n,10)
	return(s)
}

function smith(n) {
	a = J(n, 1, .)
	i = 0
	for (j=2; j<=n; j++) {
		f = factor(j)
		m = rows(f)
		if (m>1 | f[1,2]>1) {
			s = 0
			for (k=1; k<=m; k++) s = s+sumdigits(f[k,1])*f[k,2]
			if (s==sumdigits(j)) a[++i] = j
		}
	}
	return(a[1::i])
}

a = smith(10000)
n = rows(a)
n
  376

a[1::10]'

         1     2     3     4     5     6     7     8     9    10
    +-------------------------------------------------------------+
  1 |    4    22    27    58    85    94   121   166   202   265  |
    +-------------------------------------------------------------+

a[n-9::n]'

          1      2      3      4      5      6      7      8      9     10
    +-----------------------------------------------------------------------+
  1 |  9843   9849   9861   9880   9895   9924   9942   9968   9975   9985  |
    +-----------------------------------------------------------------------+
```



## Tcl


```Tcl
proc factors {x} {
    # list the prime factors of x in ascending order
    set result [list]
    while {$x % 2 == 0} {
        lappend result 2
        set x [expr {$x / 2}]
    }
    for {set i 3} {$i*$i <= $x} {incr i 2} {
        while {$x % $i == 0} {
            lappend result $i
            set x [expr {$x / $i}]
        }
    }
    if {$x != 1} {lappend result $x}
    return $result
}

proc digitsum {n} {
    ::tcl::mathop::+ {*}[split $n ""]
}

proc smith? {n} {
    set fs [factors $n]
    if {[llength $fs] == 1} {
        return false    ;# $n is prime
    }
    expr {[digitsum $n] == [digitsum [join $fs ""]]}
}
proc range {n} {
    for {set i 1} {$i < $n} {incr i} {lappend result $i}
    return $result
}

set smiths [lmap i [range 10000] {
    if {![smith? $i]} continue
    set i
}]

puts [lrange $smiths 0 12]...
puts ...[lrange $smiths end-12 end]
puts "([llength $smiths] total)"

```


{{out}}

```txt
4 22 27 58 85 94 121 166 202 265 274 319 346...
...9760 9778 9840 9843 9849 9861 9880 9895 9924 9942 9968 9975 9985
(376 total)
```



## zkl

Uses the code (primeFactors) from [[Prime decomposition#zkl]].

```zkl
fcn smithNumbers(N=0d10_000){ // -->(Smith numbers to N)
   [2..N].filter(fcn(n){
      (pfs:=primeFactors(n)).len()>1 and
      n.split().sum(0)==primeFactors(n).apply("split").flatten().sum(0)
   })
}
```


```zkl
sns:=smithNumbers();
sns.toString(*).println(" ",sns.len()," numbers");
```

{{out}}

```txt

L(4,22,27,58,85,94,121,166,202,265,274,319,346,355,378,382,391, ...
3091,3138,3168,3174,3226,3246,3258,3294,3345,3366,3390,3442,3505, ...
9942,9968,9975,9985) 376 numbers

```


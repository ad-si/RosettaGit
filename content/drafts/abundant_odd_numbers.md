+++
title = "Abundant odd numbers"
description = ""
date = 2019-10-18T14:56:13Z
aliases = []
[extra]
id = 22324
[taxonomies]
categories = []
tags = []
+++

{{task}}

An [[wp:Abundant_number|Abundant number]] is a number '''n''' for which the   ''sum of divisors''   '''σ(n) > 2n''',

or,   equivalently,   the   ''sum of proper divisors''   (or aliquot sum)       '''s(n) > n'''.


;E.G.:
'''12'''   is abundant, it has the proper divisors     '''1,2,3,4 <small>&</small> 6'''     which sum to   '''16'''   ( > '''12''' or '''n''');

       or alternately,   has the sigma sum of   '''1,2,3,4,6 <small>&</small> 12'''   which sum to   '''28'''   ( > '''24''' or '''2n''').


Abundant numbers are common, though '''even''' abundant numbers seem to be much more common than '''odd''' abundant numbers.

To make things more interesting, this task is specifically about finding   ''odd abundant numbers''.


;Task
*Find and display here: at least the first 25 abundant odd numbers and either their proper divisor sum or sigma sum.
*Find and display here: the one thousandth abundant odd number and either its proper divisor sum or sigma sum.
*Find and display here: the first abundant odd number greater than one billion (10<sup>9</sup>) and either its proper divisor sum or sigma sum.


;References:
:*   the OEIS entry:   [http://oeis.org/A005231 odd abundant numbers (odd numbers n whose sum of divisors exceeds 2n)].
:*   American Journal of Mathematics, Vol. 35, No. 4 (Oct., 1913), pp. 413-422 - Finiteness of the Odd Perfect and Primitive Abundant Numbers with n Distinct Prime Factors (LE Dickson)





## 360 Assembly


```360asm
*        Abundant odd numbers      18/09/2019
ABUNODDS CSECT
         USING  ABUNODDS,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R8,0               n=0
         LA     R6,3               i=3
       DO WHILE=(C,R8,LT,NN1)      do i=3 by 2 until n>=nn1
         BAL    R14,SIGMA            s=sigma(i)
       IF    CR,R9,GT,R6 THEN        if s>i then
         LA     R8,1(R8)               n++
         BAL    R14,PRINT              print results
       ENDIF    ,                    endif
         LA     R6,2(R6)             i+=2
       ENDDO    ,                  enddo i
         LA     R8,0               n=0
         LA     R6,3               i=3
         XR     R1,R1              f=false
       DO WHILE=(C,R1,EQ,=F'0')    do i=3 by 2 while not f
         BAL    R14,SIGMA            s=sigma(i)
       IF    CR,R9,GT,R6 THEN        if s>i then
         LA     R8,1(R8)               n++
       IF      C,R8,GE,NN2 THEN        if n>=nn2 then
         BAL    R14,PRINT                print results
         LA     R1,1                     f=true
       ENDIF    ,                      endif
       ENDIF    ,                    endif
         LA     R6,2(R6)             i+=2
       ENDDO    ,                  enddo i
         LA     R8,0               n=0
         L      R6,NN3             i=mm3
         LA     R6,1(R6)           +1
         XR     R1,R1              f=false
       DO WHILE=(C,R1,EQ,=F'0')    do i=nn3+1 by 2 while not f
         BAL    R14,SIGMA            s=sigma(i)
       IF    CR,R9,GT,R6 THEN        if s>i then
         BAL    R14,PRINT              print results
         LA     R1,1                   f=true
       ENDIF    ,                    endif
         LA     R6,2(R6)             i+=2
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling save
SIGMA    CNOP   0,4                ---- subroutine sigma
         LA     R9,1               s=1
         LA     R7,3               j=3
         LR     R5,R7              j
         MR     R4,R7              j*j
       DO WHILE=(CR,R5,LT,R6)      do j=3 by 2 while j*j<i
         LR     R4,R6                i
         SRDA   R4,32                ~
         DR     R4,R7                i/j
       IF   LTR,R4,Z,R4 THEN         if mod(i,j)=0 then
         AR     R9,R7                  s+j
         LR     R4,R6                  i
         SRDA   R4,32                  ~
         DR     R4,R7                  i/j
         AR     R9,R5                  s=s+j+i/j
       ENDIF    ,                    endif
         LA     R7,2(R7)             j+=2
         LR     R5,R7                j
         MR     R4,R7                j*j
       ENDDO    ,                  enddo j
       IF    CR,R5,EQ,R6 THEN      if j*j=i then
         AR     R9,R7              s=s+j
       ENDIF    ,                  endif
         BR     R14                ---- end of subroutine sigma
PRINT    CNOP   0,4                ---- subroutine print
         XDECO  R8,XDEC            edit n
         MVC    BUF(4),XDEC+8      output n
         XDECO  R6,BUF+14          edit & output i
         XDECO  R9,BUF+33          edit & output s
         XPRNT  BUF,L'BUF          print buffer
         BR     R14                ---- end of subroutine print
NN1      DC     F'25'              nn1=25
NN2      DC     F'1000'            nn2=1000
NN3      DC     F'1000000000'      nn3=1000000000
BUF      DC     CL80'.... - number=............ sigma=............'
XDEC     DS     CL12               temp for edit
         REGEQU                    equate registers
         END    ABUNODDS
```

{{out}}

```txt

   1 - number=         945 sigma=         975
   2 - number=        1575 sigma=        1649
   3 - number=        2205 sigma=        2241
   4 - number=        2835 sigma=        2973
   5 - number=        3465 sigma=        4023
   6 - number=        4095 sigma=        4641
   7 - number=        4725 sigma=        5195
   8 - number=        5355 sigma=        5877
   9 - number=        5775 sigma=        6129
  10 - number=        5985 sigma=        6495
  11 - number=        6435 sigma=        6669
  12 - number=        6615 sigma=        7065
  13 - number=        6825 sigma=        7063
  14 - number=        7245 sigma=        7731
  15 - number=        7425 sigma=        7455
  16 - number=        7875 sigma=        8349
  17 - number=        8085 sigma=        8331
  18 - number=        8415 sigma=        8433
  19 - number=        8505 sigma=        8967
  20 - number=        8925 sigma=        8931
  21 - number=        9135 sigma=        9585
  22 - number=        9555 sigma=        9597
  23 - number=        9765 sigma=       10203
  24 - number=       10395 sigma=       12645
  25 - number=       11025 sigma=       11946
1000 - number=      492975 sigma=      519361
   0 - number=  1000000575 sigma=  1083561009

```




## Ada


This solution uses the package ''Generic_Divisors'' from the Proper Divisors task
[[http://rosettacode.org/wiki/Proper_divisors#Ada]].


```Ada
with Ada.Text_IO, Generic_Divisors;

procedure Odd_Abundant is
   function Same(P: Positive) return Positive is (P);

   package Divisor_Sum is new Generic_Divisors
     (Result_Type => Natural, None => 0, One => Same, Add =>  "+");

   function Abundant(N: Positive) return Boolean is
      (Divisor_Sum.Process(N) > N);

   package NIO is new Ada.Text_IO.Integer_IO(Natural);

   Current: Positive := 1;

   procedure Print_Abundant_Line
     (Idx: Positive; N: Positive; With_Idx: Boolean:= True) is
   begin
      if With_Idx then
	 NIO.Put(Idx, 6);  Ada.Text_IO.Put(" |");
      else
	 Ada.Text_IO.Put("   *** |");
      end if;
      NIO.Put(N, 12); Ada.Text_IO.Put(" | ");
      NIO.Put(Divisor_Sum.Process(N), 12); Ada.Text_IO.New_Line;
   end Print_Abundant_Line;

begin
   -- the first 25 abundant odd numbers
   Ada.Text_IO.Put_Line(" index |      number | proper divisor sum ");
   Ada.Text_IO.Put_Line("-------+-------------+--------------------");
   for I in 1 .. 25 loop
      while not Abundant(Current) loop
	 Current := Current + 2;
      end loop;
      Print_Abundant_Line(I, Current);
      Current := Current + 2;
   end loop;

   -- the one thousandth abundant odd number
   Ada.Text_IO.Put_Line("-------+-------------+--------------------");
   for I in 26 .. 1_000 loop
      Current := Current + 2;
      while not Abundant(Current) loop
	 Current := Current + 2;
      end loop;
   end loop;
   Print_Abundant_Line(1000, Current);

   -- the first abundant odd number greater than 10**9
   Ada.Text_IO.Put_Line("-------+-------------+--------------------");
   Current := 10**9+1;
   while not Abundant(Current) loop
      Current := Current + 2;
   end loop;
   Print_Abundant_Line(1, Current, False);
end Odd_Abundant;
```


{{out}}

```txt
 Index |      Number | proper divisor sum
-------+-------------+--------------------
     1 |         945 |          975
     2 |        1575 |         1649
     3 |        2205 |         2241
     4 |        2835 |         2973
     5 |        3465 |         4023
     6 |        4095 |         4641
     7 |        4725 |         5195
     8 |        5355 |         5877
     9 |        5775 |         6129
    10 |        5985 |         6495
    11 |        6435 |         6669
    12 |        6615 |         7065
    13 |        6825 |         7063
    14 |        7245 |         7731
    15 |        7425 |         7455
    16 |        7875 |         8349
    17 |        8085 |         8331
    18 |        8415 |         8433
    19 |        8505 |         8967
    20 |        8925 |         8931
    21 |        9135 |         9585
    22 |        9555 |         9597
    23 |        9765 |        10203
    24 |       10395 |        12645
    25 |       11025 |        11946
-------+-------------+--------------------
  1000 |      492975 |       519361
-------+-------------+--------------------
   *** |  1000000575 |   1083561009
```



## ALGOL 68


```algol68
BEGIN
    # find some abundant odd numbers - numbers where the sum of the proper    #
    #                                  divisors is bigger than the number     #
    #                                  itself                                 #

    # returns the sum of the proper divisors of n                             #
    PROC divisor sum = ( INT n )INT:
    BEGIN
        INT sum := 1;
        FOR d FROM 2 TO ENTIER sqrt( n ) DO
            IF n MOD d = 0 THEN
                sum +:= d;
                IF INT other d := n OVER d;
                   other d /= d
                THEN
                    sum +:= other d
                FI
            FI
        OD;
        sum
    END # divisor sum # ;
    # find numbers required by the task                                       #
    BEGIN
        # first 25 odd abundant numbers                                       #
        INT odd number := 1;
        INT a count    := 0;
        INT d sum      := 0;
        print( ( "The first 25 abundant odd numbers:", newline ) );
        WHILE a count < 25 DO
            IF ( d sum := divisor sum( odd number ) ) > odd number THEN
                a count +:= 1;
                print( ( whole( odd number, -6 )
                       , " proper divisor sum: "
                       , whole( d sum, 0 )
                       , newline
                       )
                     )
            FI;
            odd number +:= 2
        OD;
        # 1000th odd abundant number                                          #
        WHILE a count < 1 000 DO
            IF ( d sum := divisor sum( odd number ) ) > odd number THEN
                a count := a count + 1
            FI;
            odd number +:= 2
        OD;
        print( ( "1000th abundant odd number:"
               , newline
               , "    "
               , whole( odd number - 2, 0 )
               , " proper divisor sum: "
               , whole( d sum, 0 )
               , newline
               )
             );
        # first odd abundant number > one billion                             #
        odd number := 1 000 000 001;
        BOOL found := FALSE;
        WHILE NOT found DO
            IF ( d sum := divisor sum( odd number ) ) > odd number THEN
                found  := TRUE;
                print( ( "First abundant odd number > 1 000 000 000:"
                       , newline
                       , "    "
                       , whole( odd number, 0 )
                       , " proper divisor sum: "
                       , whole( d sum, 0 )
                       , newline
                       )
                     )
            FI;
            odd number +:= 2
        OD
    END
END
```

{{out}}

```txt

The first 25 abundant odd numbers:
   945 proper divisor sum: 975
  1575 proper divisor sum: 1649
  2205 proper divisor sum: 2241
  2835 proper divisor sum: 2973
  3465 proper divisor sum: 4023
  4095 proper divisor sum: 4641
  4725 proper divisor sum: 5195
  5355 proper divisor sum: 5877
  5775 proper divisor sum: 6129
  5985 proper divisor sum: 6495
  6435 proper divisor sum: 6669
  6615 proper divisor sum: 7065
  6825 proper divisor sum: 7063
  7245 proper divisor sum: 7731
  7425 proper divisor sum: 7455
  7875 proper divisor sum: 8349
  8085 proper divisor sum: 8331
  8415 proper divisor sum: 8433
  8505 proper divisor sum: 8967
  8925 proper divisor sum: 8931
  9135 proper divisor sum: 9585
  9555 proper divisor sum: 9597
  9765 proper divisor sum: 10203
 10395 proper divisor sum: 12645
 11025 proper divisor sum: 11946
1000th abundant odd number:
    492975 proper divisor sum: 519361
First abundant odd number > 1 000 000 000:
    1000000575 proper divisor sum: 1083561009

```



## BASIC256

{{trans|Visual Basic .NET}}

```BASIC256

numimpar = 1
contar = 0
sumaDiv = 0

function SumaDivisores(n)
	# Devuelve la suma de los divisores propios de n
	suma = 1
	i = int(sqr(n))

	for d = 2 to i
		if n % d = 0 then
			suma += d
			otroD = n \ d
			if otroD <> d Then suma += otroD
		end if
	Next d
	Return suma
End Function

# Encontrar los números requeridos por la tarea:

# primeros 25 números abundantes impares
Print "Los primeros 25 números impares abundantes:"
While contar < 25
	sumaDiv = SumaDivisores(numimpar)
	If sumaDiv > numimpar Then
		contar += 1
		Print numimpar & " suma divisoria adecuada: " & sumaDiv
	End If
	numimpar += 2
End While

# 1000er número impar abundante
While contar < 1000
	sumaDiv = SumaDivisores(numimpar)
	print sumaDiv & "  " & contar
	If sumaDiv > numimpar Then contar += 1
	numimpar += 2
End While
Print Chr(10) & "1000º número impar abundante:"
Print "    " & (numimpar - 2) & " suma divisoria adecuada: " & sumaDiv

# primer número impar abundante > mil millones (millardo)
numimpar = 1000000001
encontrado = False
While Not encontrado
	sumaDiv = SumaDivisores(numimpar)
	If sumaDiv > numimpar Then
		encontrado = True
		Print Chr(10) & "Primer número impar abundante > 1 000 000 000:"
		Print "    " & numimpar & " suma divisoria adecuada: " & sumaDiv
	End If
	numimpar += 2
End While
End

```




## C


```c
#include <stdio.h>
#include <math.h>

// The following function is for odd numbers ONLY
// Please use "for (unsigned i = 2, j; i*i <= n; i ++)" for even and odd numbers
unsigned sum_proper_divisors(const unsigned n) {
  unsigned sum = 1;
  for (unsigned i = 3, j; i < sqrt(n)+1; i += 2) if (n % i == 0) sum += i + (i == (j = n / i) ? 0 : j);
  return sum;
}

int main(int argc, char const *argv[]) {
  unsigned n, c;
  for (n = 1, c = 0; c < 25; n += 2) if (n < sum_proper_divisors(n)) printf("%u: %u\n", ++c, n);

  for ( ; c < 1000; n += 2) if (n < sum_proper_divisors(n)) c ++;
  printf("\nThe one thousandth abundant odd number is: %u\n", n);

  for (n = 1000000001 ;; n += 2) if (n < sum_proper_divisors(n)) break;
  printf("The first abundant odd number above one billion is: %u\n", n);

  return 0;
}
```

{{out}}

```txt
1: 945
2: 1575
3: 2205
4: 2835
5: 3465
6: 4095
7: 4725
8: 5355
9: 5775
10: 5985
11: 6435
12: 6615
13: 6825
14: 7245
15: 7425
16: 7875
17: 8085
18: 8415
19: 8505
20: 8925
21: 9135
22: 9555
23: 9765
24: 10395
25: 11025

The one thousandth abundant odd number is: 492977
The first abundant odd number above one billion is: 1000000575
```




## C++

{{trans|Go}}

```cpp
#include <algorithm>
#include <iostream>
#include <numeric>
#include <sstream>
#include <vector>

std::vector<int> divisors(int n) {
    std::vector<int> divs{ 1 };
    std::vector<int> divs2;

    for (int i = 2; i*i <= n; i++) {
        if (n%i == 0) {
            int j = n / i;
            divs.push_back(i);
            if (i != j) {
                divs2.push_back(j);
            }
        }
    }
    std::copy(divs2.crbegin(), divs2.crend(), std::back_inserter(divs));

    return divs;
}

int sum(const std::vector<int>& divs) {
    return std::accumulate(divs.cbegin(), divs.cend(), 0);
}

std::string sumStr(const std::vector<int>& divs) {
    auto it = divs.cbegin();
    auto end = divs.cend();
    std::stringstream ss;

    if (it != end) {
        ss << *it;
        it = std::next(it);
    }
    while (it != end) {
        ss << " + " << *it;
        it = std::next(it);
    }

    return ss.str();
}

int abundantOdd(int searchFrom, int countFrom, int countTo, bool printOne) {
    int count = countFrom;
    int n = searchFrom;
    for (; count < countTo; n += 2) {
        auto divs = divisors(n);
        int tot = sum(divs);
        if (tot > n) {
            count++;
            if (printOne && count < countTo) {
                continue;
            }
            auto s = sumStr(divs);
            if (printOne) {
                printf("%d < %s = %d\n", n, s.c_str(), tot);
            } else {
                printf("%2d. %5d < %s = %d\n", count, n, s.c_str(), tot);
            }
        }
    }
    return n;
}

int main() {
    using namespace std;

    const int max = 25;
    cout << "The first " << max << " abundant odd numbers are:\n";
    int n = abundantOdd(1, 0, 25, false);

    cout << "\nThe one thousandth abundant odd number is:\n";
    abundantOdd(n, 25, 1000, true);

    cout << "\nThe first abundant odd number above one billion is:\n";
    abundantOdd(1e9 + 1, 0, 1, true);

    return 0;
}
```

{{out}}

```txt
The first 25 abundant odd numbers are:
 1.   945 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975
 2.  1575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649
 3.  2205 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241
 4.  2835 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973
 5.  3465 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023
 6.  4095 < 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641
 7.  4725 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195
 8.  5355 < 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877
 9.  5775 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129
10.  5985 < 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495
11.  6435 < 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669
12.  6615 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065
13.  6825 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063
14.  7245 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731
15.  7425 < 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455
16.  7875 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349
17.  8085 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331
18.  8415 < 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433
19.  8505 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967
20.  8925 < 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931
21.  9135 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585
22.  9555 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597
23.  9765 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203
24. 10395 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645
25. 11025 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946

The one thousandth abundant odd number is:
492975 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325 = 519361

The first abundant odd number above one billion is:
1000000575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009
```



## C sharp


```csharp
using static System.Console;
using System.Collections.Generic;
using System.Linq;

public static class AbundantOddNumbers
{
    public static void Main() {
        WriteLine("First 25 abundant odd numbers:");
        foreach (var x in AbundantNumbers().Take(25)) WriteLine(x.Format());
        WriteLine();
        WriteLine($"The 1000th abundant odd number: {AbundantNumbers().ElementAt(999).Format()}");
        WriteLine();
        WriteLine($"First abundant odd number > 1b: {AbundantNumbers(1_000_000_001).First().Format()}");
    }

    static IEnumerable<(int n, int sum)> AbundantNumbers(int start = 3) =>
        start.UpBy(2).Select(n => (n, sum: n.DivisorSum())).Where(x => x.sum > x.n);

    static int DivisorSum(this int n) => 3.UpBy(2).TakeWhile(i => i * i <= n).Where(i => n % i == 0)
        .Select(i => (a:i, b:n/i)).Sum(p => p.a == p.b ? p.a : p.a + p.b) + 1;

    static IEnumerable<int> UpBy(this int n, int step) {
        for (int i = n; ; i+=step) yield return i;
    }

    static string Format(this (int n, int sum) pair) => $"{pair.n:N0} with sum {pair.sum:N0}";
}
```

{{out}}

```txt

First 25 abundant odd numbers:
945 with sum 975
1,575 with sum 1,649
2,205 with sum 2,241
2,835 with sum 2,973
3,465 with sum 4,023
4,095 with sum 4,641
4,725 with sum 5,195
5,355 with sum 5,877
5,775 with sum 6,129
5,985 with sum 6,495
6,435 with sum 6,669
6,615 with sum 7,065
6,825 with sum 7,063
7,245 with sum 7,731
7,425 with sum 7,455
7,875 with sum 8,349
8,085 with sum 8,331
8,415 with sum 8,433
8,505 with sum 8,967
8,925 with sum 8,931
9,135 with sum 9,585
9,555 with sum 9,597
9,765 with sum 10,203
10,395 with sum 12,645
11,025 with sum 11,946

The 1000th abundant odd number: 492,975 with sum 519,361

First abundant odd number > 1b: 1,000,000,575 with sum 1,083,561,009
```



## D

{{trans|C++}}

```d
import std.stdio;

int[] divisors(int n) {
    import std.range;

    int[] divs = [1];
    int[] divs2;

    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            int j = n / i;
            divs ~= i;
            if (i != j) {
                divs2 ~= j;
            }
        }
    }
    divs ~= retro(divs2).array;

    return divs;
}

int abundantOdd(int searchFrom, int countFrom, int countTo, bool printOne) {
    import std.algorithm.iteration;
    import std.array;
    import std.conv;

    int count = countFrom;
    int n = searchFrom;
    for (; count < countTo; n += 2) {
        auto divs = divisors(n);
        int tot = sum(divs);
        if (tot > n) {
            count++;
            if (printOne && count < countTo) {
                continue;
            }
            auto s = divs.map!(to!string).join(" + ");
            if (printOne) {
                writefln("%d < %s = %d", n, s, tot);
            } else {
                writefln("%2d. %5d < %s = %d", count, n, s, tot);
            }
        }
    }
    return n;
}

void main() {
    const int max = 25;
    writefln("The first %d abundant odd numbers are:", max);
    int n = abundantOdd(1, 0, 25, false);

    writeln("\nThe one thousandth abundant odd number is:");
    abundantOdd(n, 25, 1000, true);

    writeln("\nThe first abundant odd number above one billion is:");
    abundantOdd(cast(int)(1e9 + 1), 0, 1, true);
}
```

{{out}}

```txt
The first 25 abundant odd numbers are:
 1.   945 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975
 2.  1575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649
 3.  2205 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241
 4.  2835 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973
 5.  3465 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023
 6.  4095 < 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641
 7.  4725 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195
 8.  5355 < 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877
 9.  5775 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129
10.  5985 < 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495
11.  6435 < 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669
12.  6615 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065
13.  6825 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063
14.  7245 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731
15.  7425 < 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455
16.  7875 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349
17.  8085 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331
18.  8415 < 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433
19.  8505 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967
20.  8925 < 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931
21.  9135 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585
22.  9555 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597
23.  9765 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203
24. 10395 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645
25. 11025 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946

The one thousandth abundant odd number is:
492975 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325 = 519361

The first abundant odd number above one billion is:
1000000575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009
```



## Factor


```factor
USING: arrays formatting io kernel lists lists.lazy math
math.primes.factors sequences tools.memory.private ;
IN: rosetta-code.abundant-odd-numbers

: σ ( n -- sum ) divisors sum ;
: abundant? ( n -- ? ) [ σ ] [ 2 * ] bi > ;
: abundant-odds-from ( n -- list )
    dup even? [ 1 + ] when
    [ 2 + ] lfrom-by [ abundant? ] lfilter ;

: first25 ( -- seq ) 25 1 abundant-odds-from ltake list>array ;
: 1,000th ( -- n ) 1 abundant-odds-from 999 [ cdr ] times car ;
: first>10^9 ( -- n ) 1,000,000,001 abundant-odds-from car ;

GENERIC: show ( obj -- )
M: integer show dup σ [ commas ] bi@ "%-6s σ = %s\n" printf ;
M: array show [ show ] each ;

: abundant-odd-numbers-demo ( -- )
    first25 "First 25 abundant odd numbers:"
    1,000th "1,000th abundant odd number:"
    first>10^9 "First abundant odd number > one billion:"
    [ print show nl ] 2tri@ ;

MAIN: abundant-odd-numbers-demo
```

{{out}}

```txt

First 25 abundant odd numbers:
945    σ = 1,920
1,575  σ = 3,224
2,205  σ = 4,446
2,835  σ = 5,808
3,465  σ = 7,488
4,095  σ = 8,736
4,725  σ = 9,920
5,355  σ = 11,232
5,775  σ = 11,904
5,985  σ = 12,480
6,435  σ = 13,104
6,615  σ = 13,680
6,825  σ = 13,888
7,245  σ = 14,976
7,425  σ = 14,880
7,875  σ = 16,224
8,085  σ = 16,416
8,415  σ = 16,848
8,505  σ = 17,472
8,925  σ = 17,856
9,135  σ = 18,720
9,555  σ = 19,152
9,765  σ = 19,968
10,395 σ = 23,040
11,025 σ = 22,971

1,000th abundant odd number:
492,975 σ = 1,012,336

First abundant odd number > one billion:
1,000,000,575 σ = 2,083,561,584

```



## FreeBASIC

{{trans|Visual Basic .NET}}

```freebasic

Declare Function SumaDivisores(n As Integer) As Integer

Dim numimpar As Integer = 1
Dim contar As Integer = 0
Dim sumaDiv As Integer = 0

Function SumaDivisores(n As Integer) As Integer
    ' Devuelve la suma de los divisores propios de n
    Dim suma As Integer = 1
    Dim As Integer d, otroD

    For d = 2 To Cint(Sqr(n))
        If n Mod d = 0 Then
            suma += d
            otroD = n \ d
            If otroD <> d Then suma += otroD
        End If
    Next d
    Return suma
End Function

' Encontrar los números requeridos por la tarea:

' primeros 25 números abundantes impares
Print "Los primeros 25 números impares abundantes:"
Do While contar < 25
    sumaDiv = SumaDivisores(numimpar)
    If sumaDiv > numimpar Then
        contar += 1
        Print using "######"; numimpar;
        Print " suma divisoria adecuada: " & sumaDiv
    End If
    numimpar += 2
Loop

' 1000er número impar abundante
Do While contar < 1000
    sumaDiv = SumaDivisores(numimpar)
    If sumaDiv > numimpar Then contar += 1
    numimpar += 2
Loop
Print Chr(10) & "1000º número impar abundante:"
Print "    " & (numimpar - 2) & " suma divisoria adecuada: " & sumaDiv

' primer número impar abundante > mil millones (millardo)
numimpar = 1000000001
Dim encontrado As Boolean = False
Do While Not encontrado
    sumaDiv = SumaDivisores(numimpar)
    If sumaDiv > numimpar Then
        encontrado = True
        Print Chr(10) & "Primer número impar abundante > 1 000 000 000:"
        Print "    " & numimpar & " suma divisoria adecuada: " & sumaDiv
    End If
    numimpar += 2
Loop
End

```

{{out}}

```txt

Los primeros 25 números impares abundantes:
   945 suma divisoria adecuada: 975
  1575 suma divisoria adecuada: 1649
  2205 suma divisoria adecuada: 2241
  2835 suma divisoria adecuada: 2973
  3465 suma divisoria adecuada: 4023
  4095 suma divisoria adecuada: 4641
  4725 suma divisoria adecuada: 5195
  5355 suma divisoria adecuada: 5877
  5775 suma divisoria adecuada: 6129
  5985 suma divisoria adecuada: 6495
  6435 suma divisoria adecuada: 6669
  6615 suma divisoria adecuada: 7065
  6825 suma divisoria adecuada: 7063
  7245 suma divisoria adecuada: 7731
  7425 suma divisoria adecuada: 7455
  7875 suma divisoria adecuada: 8349
  8085 suma divisoria adecuada: 8331
  8415 suma divisoria adecuada: 8433
  8505 suma divisoria adecuada: 8967
  8925 suma divisoria adecuada: 8931
  9135 suma divisoria adecuada: 9585
  9555 suma divisoria adecuada: 9597
  9765 suma divisoria adecuada: 10203
 10395 suma divisoria adecuada: 12645
 11025 suma divisoria adecuada: 11946

1000º número impar abundante:
    492975 suma divisoria adecuada: 519361

Primer número impar abundante > 1 000 000 000:
    1000000575 suma divisoria adecuada: 1083561009

```



## Go


```go
package main

import (
    "fmt"
    "strconv"
)

func divisors(n int) []int {
    divs := []int{1}
    divs2 := []int{}
    for i := 2; i*i <= n; i++ {
        if n%i == 0 {
            j := n / i
            divs = append(divs, i)
            if i != j {
                divs2 = append(divs2, j)
            }
        }
    }
    for i := len(divs2) - 1; i >= 0; i-- {
        divs = append(divs, divs2[i])
    }
    return divs
}

func sum(divs []int) int {
    tot := 0
    for _, div := range divs {
        tot += div
    }
    return tot
}

func sumStr(divs []int) string {
    s := ""
    for _, div := range divs {
        s += strconv.Itoa(div) + " + "
    }
    return s[0 : len(s)-3]
}

func abundantOdd(searchFrom, countFrom, countTo int, printOne bool) int {
    count := countFrom
    n := searchFrom
    for ; count < countTo; n += 2 {
        divs := divisors(n)
        if tot := sum(divs); tot > n {
            count++
            if printOne && count < countTo {
                continue
            }
            s := sumStr(divs)
            if !printOne {
                fmt.Printf("%2d. %5d < %s = %d\n", count, n, s, tot)
            } else {
                fmt.Printf("%d < %s = %d\n", n, s, tot)
            }
        }
    }
    return n
}

func main() {
    const max = 25
    fmt.Println("The first", max, "abundant odd numbers are:")
    n := abundantOdd(1, 0, 25, false)

    fmt.Println("\nThe one thousandth abundant odd number is:")
    abundantOdd(n, 25, 1000, true)

    fmt.Println("\nThe first abundant odd number above one billion is:")
    abundantOdd(1e9+1, 0, 1, true)
}
```


{{out}}

```txt

The first 25 abundant odd numbers are:
 1.   945 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975
 2.  1575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649
 3.  2205 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241
 4.  2835 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973
 5.  3465 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023
 6.  4095 < 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641
 7.  4725 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195
 8.  5355 < 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877
 9.  5775 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129
10.  5985 < 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495
11.  6435 < 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669
12.  6615 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065
13.  6825 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063
14.  7245 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731
15.  7425 < 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455
16.  7875 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349
17.  8085 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331
18.  8415 < 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433
19.  8505 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967
20.  8925 < 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931
21.  9135 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585
22.  9555 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597
23.  9765 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203
24. 10395 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645
25. 11025 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946

The one thousandth abundant odd number is:
492975 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325 = 519361

The first abundant odd number above one billion is:
1000000575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009

```



## Haskell


```Haskell
import Data.List (nub)

divisorSum :: Integral a => a -> a
divisorSum n =
  sum
    . map (\i -> sum $ nub [i, n `quot` i])
    . filter ((== 0) . (n `rem`))
    $ takeWhile ((<= n) . (^ 2)) [1 ..]

oddAbundants :: Integral a => a -> [(a, a)]
oddAbundants n =
  [ (i, divisorSum i) | i <- [n ..], odd i, divisorSum i > i * 2 ]

printAbundant :: (Int, Int) -> IO ()
printAbundant (n, s) =
  putStrLn
    $  show n
    ++ " with "
    ++ show s
    ++ " as the sum of all proper divisors."

main :: IO ()
main = do
  putStrLn "The first 25 odd abundant numbers are:"
  mapM_ printAbundant . take 25 $ oddAbundants 1
  putStrLn "The 1000th odd abundant number is:"
  printAbundant $ oddAbundants 1 !! 1000
  putStrLn "The first odd abundant number above 1000000000 is:"
  printAbundant . head . oddAbundants $ 10 ^ 9
```


{{out}}

```txt
The first 25 odd abundant numbers are:
945 with 1920 as the sum of all proper divisors.
1575 with 3224 as the sum of all proper divisors.
2205 with 4446 as the sum of all proper divisors.
2835 with 5808 as the sum of all proper divisors.
3465 with 7488 as the sum of all proper divisors.
4095 with 8736 as the sum of all proper divisors.
4725 with 9920 as the sum of all proper divisors.
5355 with 11232 as the sum of all proper divisors.
5775 with 11904 as the sum of all proper divisors.
5985 with 12480 as the sum of all proper divisors.
6435 with 13104 as the sum of all proper divisors.
6615 with 13680 as the sum of all proper divisors.
6825 with 13888 as the sum of all proper divisors.
7245 with 14976 as the sum of all proper divisors.
7425 with 14880 as the sum of all proper divisors.
7875 with 16224 as the sum of all proper divisors.
8085 with 16416 as the sum of all proper divisors.
8415 with 16848 as the sum of all proper divisors.
8505 with 17472 as the sum of all proper divisors.
8925 with 17856 as the sum of all proper divisors.
9135 with 18720 as the sum of all proper divisors.
9555 with 19152 as the sum of all proper divisors.
9765 with 19968 as the sum of all proper divisors.
10395 with 23040 as the sum of all proper divisors.
11025 with 22971 as the sum of all proper divisors.
The 1000th odd abundant number is:
493185 with 1017792 as the sum of all proper divisors.
The first odd abundant number above 1000000000 is:
1000000575 with 2083561584 as the sum of all proper divisors.
```


Or alternatively (and already a little faster):

```haskell
import Data.Bool (bool)

abundantTuple :: Int -> [(Int, Int)]
abundantTuple n =
  let x = divisorSum n
  in bool [] [(n, x)] (n < x)

divisorSum :: Int -> Int
divisorSum n =
  sum lows +
  sum (drop (bool 0 1 (iRoot * iRoot == n)) (reverse (quot n <$> tail lows)))
  where
    iRoot = floor (sqrt $ fromIntegral n)
    lows = filter ((== 0) . rem n) [1 .. iRoot]

main :: IO ()
main = do
  putStrLn "First 25 abundant odd numbers with their divisor sums:"
  mapM_ print $ take 25 ([1,3 ..] >>= abundantTuple)
  --
  putStrLn "\n1000th odd abundant number with its divisor sum:"
  print $ ([1,3 ..] >>= abundantTuple) !! 999
  --
  putStrLn "\nFirst odd abundant number over 10^9, with its divisor sum:"
  let billion = 10 ^ 9 :: Int
  print $ head ([1 + billion,3 + billion ..] >>= abundantTuple)
```

{{Out}}

```txt
First 25 abundant odd numbers with their divisor sums:
(945,975)
(1575,1649)
(2205,2241)
(2835,2973)
(3465,4023)
(4095,4641)
(4725,5195)
(5355,5877)
(5775,6129)
(5985,6495)
(6435,6669)
(6615,7065)
(6825,7063)
(7245,7731)
(7425,7455)
(7875,8349)
(8085,8331)
(8415,8433)
(8505,8967)
(8925,8931)
(9135,9585)
(9555,9597)
(9765,10203)
(10395,12645)
(11025,11946)

1000th odd abundant number with its divisor sum:
(492975,519361)

First odd abundant number over 10^9, with its divisor sum:
(1000000575,1083561009)
```



## J


```txt

   NB. https://www.math.upenn.edu/~deturck/m170/wk3/lecture/sumdiv.html
   s=: ([: */ [: ((<:@:(^ >:)/) % <:@:{.) __&q:)&>

   assert 6045 -: s 1800

   aliquot_sum=: -~ s

   abundant=: < aliquot_sum

   Filter=: (#~`)(`:6)

   A=: abundant Filter 1 2 p. i. 260000  NB. a batch of abundant odd numbers

   # A   NB. more than 1000, it's enough.
1054

   NB. the first odd abundant numbers
   (,: aliquot_sum) 26 {. A
945 1575 2205 2835 3465 4095 4725 5355 5775 5985 6435 6615 6825 7245 7425 7875 8085 8415 8505 8925 9135 9555  9765 10395 11025 11655
975 1649 2241 2973 4023 4641 5195 5877 6129 6495 6669 7065 7063 7731 7455 8349 8331 8433 8967 8931 9585 9597 10203 12645 11946 12057

   NB. the one thousandth abundant odd number
   (,: aliquot_sum) 999 { A
492975
519361


   k=: adverb def '1000 * m'
   1x k k k
1000000000

   abundant Filter (1x k k k) + 1 2x p. i. 10x k
1000000575 1000001475 1000001625 1000001835 1000002465 1000003095 1000003725 1000004355 1000004775 1000004985 1000005435 1000005615 1000005825 1000006245 1000006425 1000006875 1000007505 1000008765 1000009395 1000010025 1000010655 1000011285 1000011705 100...

   (,: aliquot_sum) {. abundant Filter (1x k k k) + 1 2x p. i. 10x k
1000000575
1083561009

```



## JavaScript


### ES6

Composing reusable functions and generators:
{{Trans|Python}}

```javascript
(() => {
    'use strict';
    const main = () => {

        // abundantTuple :: Int -> [(Int, Int)]
        const abundantTuple = n => {
            // Either a list containing the tuple of N
            // and its divisor sum (if n is abundant),
            // or otherwise an empty list.
            const x = divisorSum(n);
            return n < x ? ([
                Tuple(n)(x)
            ]) : [];
        };

        // divisorSum :: Int -> Int
        const divisorSum = n => {
            // Sum of the divisors of n.
            const
                floatRoot = Math.sqrt(n),
                intRoot = Math.floor(floatRoot),
                lows = filter(x => 0 === n % x)(
                    enumFromTo(1)(intRoot)
                );
            return sum(lows.concat(map(quot(n))(
                intRoot === floatRoot ? (
                    lows.slice(1, -1)
                ) : lows.slice(1)
            )));
        };

        // TEST ---------------------------------------
        console.log(
            'First 25 abundant odd numbers, with their divisor sums:'
        )
        console.log(unlines(map(showTuple)(
            take(25)(
                concatMapGen(abundantTuple)(
                    enumFromThen(1)(3)
                )
            )
        )));
        console.log(
            '\n\n1000th abundant odd number, with its divisor sum:'
        )
        console.log(showTuple(
            take(1)(drop(999)(
                concatMapGen(abundantTuple)(
                    enumFromThen(1)(3)
                )
            ))[0]
        ))
        console.log(
            '\n\nFirst abundant odd number above 10^9, with divisor sum:'
        )
        const billion = Math.pow(10, 9);
        console.log(showTuple(
            take(1)(
                concatMapGen(abundantTuple)(
                    enumFromThen(1 + billion)(3 + billion)
                )
            )[0]
        ))
    };


    // GENERAL REUSABLE FUNCTIONS -------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a => b => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // concatMapGen :: (a -> [b]) -> Gen [a] -> Gen [b]
    const concatMapGen = f =>
        function*(xs) {
            let
                x = xs.next(),
                v = undefined;
            while (!x.done) {
                v = f(x.value);
                if (0 < v.length) {
                    yield v[0];
                }
                x = xs.next();
            }
        };

    // drop :: Int -> [a] -> [a]
    // drop :: Int -> Generator [a] -> Generator [a]
    // drop :: Int -> String -> String
    const drop = n => xs =>
        Infinity > length(xs) ? (
            xs.slice(n)
        ) : (take(n)(xs), xs);

    // dropAround :: (a -> Bool) -> [a] -> [a]
    // dropAround :: (Char -> Bool) -> String -> String
    const dropAround = p => xs => dropWhile(p)(
        dropWhileEnd(p)(xs)
    );

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

    // dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    // dropWhileEnd :: (Char -> Bool) -> String -> String
    const dropWhileEnd = p => xs => {
        let i = xs.length;
        while (i-- && p(xs[i])) {}
        return xs.slice(0, i + 1);
    };

    // enumFromThen :: Int -> Int -> Gen [Int]
    const enumFromThen = x =>
        // A non-finite stream of integers,
        // starting with x and y, and continuing
        // with the same interval.
        function*(y) {
            const d = y - x;
            let v = y + d;
            yield x;
            yield y;
            while (true) {
                yield v;
                v = d + v;
            }
        };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = m => n =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = f => xs => xs.filter(f);

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // quot :: Int -> Int -> Int
    const quot = n => m => Math.floor(n / m);

    // show :: a -> String
    const show = JSON.stringify;

    // showTuple :: Tuple -> String
    const showTuple = tpl =>
        '(' + enumFromTo(0)(tpl.length - 1)
        .map(x => unQuoted(show(tpl[x])))
        .join(',') + ')';

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

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = p => f => x => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // unQuoted :: String -> String
    const unQuoted = s =>
        dropAround(x => 34 === x.codePointAt(0))(
            s
        );

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
First 25 abundant odd numbers, with their divisor sums:
(945,975)
(1575,1649)
(2205,2241)
(2835,2973)
(3465,4023)
(4095,4641)
(4725,5195)
(5355,5877)
(5775,6129)
(5985,6495)
(6435,6669)
(6615,7065)
(6825,7063)
(7245,7731)
(7425,7455)
(7875,8349)
(8085,8331)
(8415,8433)
(8505,8967)
(8925,8931)
(9135,9585)
(9555,9597)
(9765,10203)
(10395,12645)
(11025,11946)

1000th abundant odd number, with its divisor sum:
(492975,519361)

First abundant odd number above 10^9, with divisor sum:
(1000000575,1083561009)
```



## Julia


```julia
using Primes

function propfact(n)
    f = [one(n)]
    for (p, x) in factor(n)
        f = reduce(vcat, [f*p^i for i in 1:x], init=f)
    end
    pop!(f)
    f
end

isabundant(n) = sum(propfact(n)) > n
prettyprintfactors(n) = (a = propfact(n); println("$n has proper divisors $a, these sum to $(sum(a))."))

function oddabundantsfrom(startingint, needed, nprint=0)
    n = isodd(startingint) ? startingint : startingint + 1
    count = 0
    while count < needed
        if isabundant(n)
            if nprint == 0
                prettyprintfactors(n)
            elseif nprint == count + 1
                prettyprintfactors(n)
                break
            end
            count += 1
        end
        n += 2
    end
end

println("First 25 abundant odd numbers:")
oddabundantsfrom(2, 25)

println("The thousandth abundant odd number:")
oddabundantsfrom(2, 1001, 1000)

println("The first abundant odd number greater than one billion:")
oddabundantsfrom(1000000000, 1)

```
{{out}}

```txt

First 25 abundant odd numbers:
945 has proper divisors [1, 3, 9, 27, 5, 15, 45, 135, 7, 21, 63, 189, 35, 105, 315], these sum to 975.
1575 has proper divisors [1, 3, 9, 5, 15, 45, 25, 75, 225, 7, 21, 63, 35, 105, 315, 175, 525], these sum to 1649.
2205 has proper divisors [1, 3, 9, 5, 15, 45, 7, 21, 63, 35, 105, 315, 49, 147, 441, 245, 735], these sum to 2241.
2835 has proper divisors [1, 3, 9, 27, 81, 5, 15, 45, 135, 405, 7, 21, 63, 189, 567, 35, 105, 315, 945], these sum to 2973.
3465 has proper divisors [1, 3, 9, 5, 15, 45, 7, 21, 63, 35, 105, 315, 11, 33, 99, 55, 165, 495, 77, 231, 693, 385, 1155], these sum to 4023.
4095 has proper divisors [1, 3, 9, 5, 15, 45, 7, 21, 63, 35, 105, 315, 13, 39, 117, 65, 195, 585, 91, 273, 819, 455, 1365], these sum to 4641.
4725 has proper divisors [1, 3, 9, 27, 5, 15, 45, 135, 25, 75, 225, 675, 7, 21, 63, 189, 35, 105, 315, 945, 175, 525, 1575], these sum to 5195.
5355 has proper divisors [1, 3, 9, 5, 15, 45, 7, 21, 63, 35, 105, 315, 17, 51, 153, 85, 255, 765, 119, 357, 1071, 595, 1785], these sum to 5877.
5775 has proper divisors [1, 3, 5, 15, 25, 75, 7, 21, 35, 105, 175, 525, 11, 33, 55, 165, 275, 825, 77, 231, 385, 1155, 1925], these sum to 6129.
5985 has proper divisors [1, 3, 9, 5, 15, 45, 7, 21, 63, 35, 105, 315, 19, 57, 171, 95, 285, 855, 133, 399, 1197, 665, 1995], these sum to 6495.
6435 has proper divisors [1, 3, 9, 5, 15, 45, 11, 33, 99, 55, 165, 495, 13, 39, 117, 65, 195, 585, 143, 429, 1287, 715, 2145], these sum to 6669.
6615 has proper divisors [1, 3, 9, 27, 5, 15, 45, 135, 7, 21, 63, 189, 35, 105, 315, 945, 49, 147, 441, 1323, 245, 735, 2205], these sum to 7065.
6825 has proper divisors [1, 3, 5, 15, 25, 75, 7, 21, 35, 105, 175, 525, 13, 39, 65, 195, 325, 975, 91, 273, 455, 1365, 2275], these sum to 7063.
7245 has proper divisors [1, 3, 9, 5, 15, 45, 7, 21, 63, 35, 105, 315, 23, 69, 207, 115, 345, 1035, 161, 483, 1449, 805, 2415], these sum to 7731.
7425 has proper divisors [1, 3, 9, 27, 5, 15, 45, 135, 25, 75, 225, 675, 11, 33, 99, 297, 55, 165, 495, 1485, 275, 825, 2475], these sum to 7455.
7875 has proper divisors [1, 3, 9, 5, 15, 45, 25, 75, 225, 125, 375, 1125, 7, 21, 63, 35, 105, 315, 175, 525, 1575, 875, 2625], these sum to 8349.
8085 has proper divisors [1, 3, 5, 15, 7, 21, 35, 105, 49, 147, 245, 735, 11, 33, 55, 165, 77, 231, 385, 1155, 539, 1617, 2695], these sum to 8331.
8415 has proper divisors [1, 3, 9, 5, 15, 45, 11, 33, 99, 55, 165, 495, 17, 51, 153, 85, 255, 765, 187, 561, 1683, 935, 2805], these sum to 8433.
8505 has proper divisors [1, 3, 9, 27, 81, 243, 5, 15, 45, 135, 405, 1215, 7, 21, 63, 189, 567, 1701, 35, 105, 315, 945, 2835], these sum to 8967.
8925 has proper divisors [1, 3, 5, 15, 25, 75, 7, 21, 35, 105, 175, 525, 17, 51, 85, 255, 425, 1275, 119, 357, 595, 1785, 2975], these sum to 8931.
9135 has proper divisors [1, 3, 9, 5, 15, 45, 7, 21, 63, 35, 105, 315, 29, 87, 261, 145, 435, 1305, 203, 609, 1827, 1015, 3045], these sum to 9585.
9555 has proper divisors [1, 3, 5, 15, 7, 21, 35, 105, 49, 147, 245, 735, 13, 39, 65, 195, 91, 273, 455, 1365, 637, 1911, 3185], these sum to 9597.
9765 has proper divisors [1, 3, 9, 5, 15, 45, 7, 21, 63, 35, 105, 315, 31, 93, 279, 155, 465, 1395, 217, 651, 1953, 1085, 3255], these sum to 10203.
10395 has proper divisors [1, 3, 9, 27, 5, 15, 45, 135, 7, 21, 63, 189, 35, 105, 315, 945, 11, 33, 99, 297, 55, 165, 495, 1485, 77, 231, 693, 2079, 385, 1155, 3465], these sum to 12645.
11025 has proper divisors [1, 3, 9, 5, 15, 45, 25, 75, 225, 7, 21, 63, 35, 105, 315, 175, 525, 1575, 49, 147, 441, 245, 735, 2205, 1225, 3675], these sum to 11946.
The thousandth abundant odd number:
492975 has proper divisors [1, 3, 9, 5, 15, 45, 25, 75, 225, 7, 21, 63, 35, 105, 315, 175, 525, 1575, 313, 939, 2817, 1565, 4695, 14085, 7825, 23475, 70425, 2191, 6573, 19719, 10955, 32865, 98595, 54775, 164325], these sum to 519361.
The first abundant odd number greater than one billion:
1000000575 has proper divisors [1, 3, 9, 5, 15, 45, 25, 75, 225, 7, 21, 63, 35, 105, 315, 175, 525, 1575, 49, 147, 441, 245, 735, 2205, 1225, 3675, 11025, 90703, 272109, 816327, 453515, 1360545, 4081635, 2267575, 6802725, 20408175, 634921, 1904763, 5714289, 3174605, 9523815, 28571445, 15873025, 47619075, 142857225, 4444447, 13333341, 40000023, 22222235, 66666705, 200000115, 111111175, 333333525], these sum to 1083561009.

```



## Kotlin

{{trans|D}}

```scala
fun divisors(n: Int): List<Int> {
    val divs = mutableListOf(1)
    val divs2 = mutableListOf<Int>()

    var i = 2
    while (i * i <= n) {
        if (n % i == 0) {
            val j = n / i
            divs.add(i)
            if (i != j) {
                divs2.add(j)
            }
        }
        i++
    }

    divs.addAll(divs2.reversed())

    return divs
}

fun abundantOdd(searchFrom: Int, countFrom: Int, countTo: Int, printOne: Boolean): Int {
    var count = countFrom
    var n = searchFrom

    while (count < countTo) {
        val divs = divisors(n)
        val tot = divs.sum()
        if (tot > n) {
            count++
            if (!printOne || count >= countTo) {
                val s = divs.joinToString(" + ")
                if (printOne) {
                    println("$n < $s = $tot")
                } else {
                    println("%2d. %5d < %s = %d".format(count, n, s, tot))
                }
            }
        }

        n += 2
    }

    return n
}


fun main() {
    val max = 25
    println("The first $max abundant odd numbers are:")
    val n = abundantOdd(1, 0, 25, false)

    println("\nThe one thousandth abundant odd number is:")
    abundantOdd(n, 25, 1000, true)

    println("\nThe first abundant odd number above one billion is:")
    abundantOdd((1e9 + 1).toInt(), 0, 1, true)
}
```

{{out}}

```txt
The first 25 abundant odd numbers are:
 1.   945 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975
 2.  1575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649
 3.  2205 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241
 4.  2835 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973
 5.  3465 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023
 6.  4095 < 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641
 7.  4725 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195
 8.  5355 < 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877
 9.  5775 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129
10.  5985 < 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495
11.  6435 < 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669
12.  6615 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065
13.  6825 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063
14.  7245 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731
15.  7425 < 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455
16.  7875 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349
17.  8085 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331
18.  8415 < 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433
19.  8505 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967
20.  8925 < 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931
21.  9135 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585
22.  9555 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597
23.  9765 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203
24. 10395 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645
25. 11025 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946

The one thousandth abundant odd number is:
492975 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325 = 519361

The first abundant odd number above one billion is:
1000000575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009
```



## Lua


```lua
-- Return the sum of the proper divisors of x
function sumDivs (x)
  local sum, sqr = 1, math.sqrt(x)
  for d = 2, sqr do
    if x % d == 0 then
      sum = sum + d
      if d ~= sqr then sum = sum + (x/d) end
    end
  end
  return sum
end

-- Return a table of odd abundant numbers
function oddAbundants (mode, limit)
  local n, count, divlist, divsum = 1, 0, {}
  repeat
    n = n + 2
    divsum = sumDivs(n)
    if divsum > n then
      table.insert(divlist, {n, divsum})
      count = count + 1
      if mode == "Above" and n > limit then return divlist[#divlist] end
    end
  until count == limit
  if mode == "First" then return divlist end
  if mode == "Nth" then return divlist[#divlist] end
end

-- Write a result to stdout
function showResult (msg, t)
  print(msg .. ": the proper divisors of " .. t[1] .. " sum to " .. t[2])
end

-- Main procedure
for k, v in pairs(oddAbundants("First", 25)) do  showResult(k, v) end
showResult("1000", oddAbundants("Nth", 1000))
showResult("Above 1e6", oddAbundants("Above", 1e6))
```

{{out}}

```txt
1: the proper divisors of 945 sum to 975
2: the proper divisors of 1575 sum to 1649
3: the proper divisors of 2205 sum to 2241
4: the proper divisors of 2835 sum to 2973
5: the proper divisors of 3465 sum to 4023
6: the proper divisors of 4095 sum to 4641
7: the proper divisors of 4725 sum to 5195
8: the proper divisors of 5355 sum to 5877
9: the proper divisors of 5775 sum to 6129
10: the proper divisors of 5985 sum to 6495
11: the proper divisors of 6435 sum to 6669
12: the proper divisors of 6615 sum to 7065
13: the proper divisors of 6825 sum to 7063
14: the proper divisors of 7245 sum to 7731
15: the proper divisors of 7425 sum to 7455
16: the proper divisors of 7875 sum to 8349
17: the proper divisors of 8085 sum to 8331
18: the proper divisors of 8415 sum to 8433
19: the proper divisors of 8505 sum to 8967
20: the proper divisors of 8925 sum to 8931
21: the proper divisors of 9135 sum to 9585
22: the proper divisors of 9555 sum to 9597
23: the proper divisors of 9765 sum to 10203
24: the proper divisors of 10395 sum to 12645
25: the proper divisors of 11025 sum to 11946
1000: the proper divisors of 492975 sum to 519361
Above 1e6: the proper divisors of 1000125 sum to 1076547
```



## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use strict;
use warnings;
use feature 'say';
use ntheory qw/divisor_sum divisors/;

sub odd_abundants {
    my($start,$count) = @_;
    my $n = int(( $start + 2 ) / 3);
    $n   += 1 if 0 == $n % 2;
    $n   *= 3;
    my @out;
    while (@out < $count) {
        $n += 6;
        next unless (my $ds = divisor_sum($n)) > 2*$n;
        my @d = divisors($n);
        push @out, sprintf "%6d: divisor sum: %s = %d", $n, join(' + ', @d[0..@d-2]), $ds-$n;
    }
    @out;
}

say 'First 25 abundant odd numbers:';
say for odd_abundants(1, 25);
say "\nOne thousandth abundant odd number:\n", (odd_abundants(1, 1000))[999];
say "\nFirst abundant odd number above one billion:\n", odd_abundants(999_999_999, 1);
```

{{out}}
<pre style="height:20ex">First 25 abundant odd numbers:
   945: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975
  1575: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649
  2205: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241
  2835: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973
  3465: divisor sum: 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023
  4095: divisor sum: 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641
  4725: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195
  5355: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877
  5775: divisor sum: 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129
  5985: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495
  6435: divisor sum: 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669
  6615: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065
  6825: divisor sum: 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063
  7245: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731
  7425: divisor sum: 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455
  7875: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349
  8085: divisor sum: 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331
  8415: divisor sum: 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433
  8505: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967
  8925: divisor sum: 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931
  9135: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585
  9555: divisor sum: 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597
  9765: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203
 10395: divisor sum: 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645
 11025: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946

One thousandth abundant odd number:
492975: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325 = 519361

First abundant odd number above one billion:
1000000575: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009
```



## Perl 6

{{works with|Rakudo|2019.03}}


```perl6
sub odd-abundant (\x) {
    my @l = x.is-prime ?? 1 !! flat
    1, (3 .. x.sqrt.floor).map: -> \d {
         next unless d +& 1;
         my \y = x div d;
         next if y * d !== x;
         d !== y ?? (d, y) !! d
    };
    @l.sum > x ?? @l.sort !! Empty;
}

sub odd-abundants (Int :$start-at is copy) {
    $start-at = ( $start-at + 2 ) div 3;
    $start-at += $start-at %% 2;
    $start-at *= 3;
    ($start-at, *+6 ... *).hyper.map: {
        next unless my $oa = .&odd-abundant;
        sprintf "%6d: divisor sum: {$oa.join: ' + '} = {$oa.sum}", $_
    }
}

put 'First 25 abundant odd numbers:';
.put for odd-abundants( :start-at(1) )[^25];

put "\nOne thousandth abundant odd number:\n" ~ odd-abundants( :start-at(1) )[999] ~

"\n\nFirst abundant odd number above one billion:\n" ~ odd-abundants( :start-at(1_000_000_000) ).head;
```

{{out}}

```txt
First 25 abundant odd numbers:
   945: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975
  1575: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649
  2205: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241
  2835: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973
  3465: divisor sum: 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023
  4095: divisor sum: 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641
  4725: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195
  5355: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877
  5775: divisor sum: 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129
  5985: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495
  6435: divisor sum: 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669
  6615: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065
  6825: divisor sum: 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063
  7245: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731
  7425: divisor sum: 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455
  7875: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349
  8085: divisor sum: 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331
  8415: divisor sum: 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433
  8505: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967
  8925: divisor sum: 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931
  9135: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585
  9555: divisor sum: 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597
  9765: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203
 10395: divisor sum: 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645
 11025: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946

One thousandth abundant odd number:
492975: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325 = 519361

First abundant odd number above one billion:
1000000575: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009
```



## Phix


```Phix
function abundantOdd(integer n, done, lim, bool printAll)
    while done<lim do
        atom tot = sum(factors(n,-1))
        if tot>n then
            done += 1
            if printAll or done=lim then
                string ln = iff(printAll?sprintf("%2d. ",done):"")
                printf(1,"%s%,6d (proper sum:%,d)\n",{ln,n,tot})
            end if
        end if
        n += 2
    end while
    printf(1,"\n")
    return n
end function
printf(1,"The first 25 abundant odd numbers are:\n")
integer n = abundantOdd(1, 0, 25, true)
printf(1,"The one thousandth abundant odd number is:")
{} = abundantOdd(n, 25, 1000, false)
printf(1,"The first abundant odd number above one billion is:")
{} = abundantOdd(1e9+1, 0, 1, false)
```

{{out}}

```txt

The first 25 abundant odd numbers are:
 1.    945 (proper sum:975)
 2.  1,575 (proper sum:1,649)
 3.  2,205 (proper sum:2,241)
 4.  2,835 (proper sum:2,973)
 5.  3,465 (proper sum:4,023)
 6.  4,095 (proper sum:4,641)
 7.  4,725 (proper sum:5,195)
 8.  5,355 (proper sum:5,877)
 9.  5,775 (proper sum:6,129)
10.  5,985 (proper sum:6,495)
11.  6,435 (proper sum:6,669)
12.  6,615 (proper sum:7,065)
13.  6,825 (proper sum:7,063)
14.  7,245 (proper sum:7,731)
15.  7,425 (proper sum:7,455)
16.  7,875 (proper sum:8,349)
17.  8,085 (proper sum:8,331)
18.  8,415 (proper sum:8,433)
19.  8,505 (proper sum:8,967)
20.  8,925 (proper sum:8,931)
21.  9,135 (proper sum:9,585)
22.  9,555 (proper sum:9,597)
23.  9,765 (proper sum:10,203)
24. 10,395 (proper sum:12,645)
25. 11,025 (proper sum:11,946)

The one thousandth abundant odd number is:492,975 (proper sum:519,361)

The first abundant odd number above one billion is:1,000,000,575 (proper sum:1,083,561,009)

```



## PicoLisp


```PicoLisp
(de accud (Var Key)
   (if (assoc Key (val Var))
      (con @ (inc (cdr @)))
      (push Var (cons Key 1)) )
   Key )
(de **sum (L)
   (let S 1
      (for I (cdr L)
         (inc 'S (** (car L) I)) )
      S ) )
(de factor-sum (N)
   (if (=1 N)
      0
      (let
         (R NIL
            D 2
            L (1 2 2 . (4 2 4 2 4 6 2 6 .))
            M (sqrt N)
            N1 N
            S 1 )
         (while (>= M D)
            (if (=0 (% N1 D))
               (setq M
                  (sqrt (setq N1 (/ N1 (accud 'R D)))) )
               (inc 'D (pop 'L)) ) )
         (accud 'R N1)
         (for I R
            (setq S (* S (**sum I))) )
         (- S N) ) ) )
(de factor-list NIL
   (let (N 1  C 0)
      (make
         (loop
            (when (> (setq @@ (factor-sum N)) N)
               (link (cons N @@))
               (inc 'C) )
            (inc 'N 2)
            (T (= C 1000)) ) ) ) )
(let L (factor-list)
   (for N 25
      (println N (++ L)) )
   (println 1000 (last L))
   (println
      '****
      1000000575
      (factor-sum 1000000575) ) )
```

{{out}}

```txt

1 (945 . 975)
2 (1575 . 1649)
3 (2205 . 2241)
4 (2835 . 2973)
5 (3465 . 4023)
6 (4095 . 4641)
7 (4725 . 5195)
8 (5355 . 5877)
9 (5775 . 6129)
10 (5985 . 6495)
11 (6435 . 6669)
12 (6615 . 7065)
13 (6825 . 7063)
14 (7245 . 7731)
15 (7425 . 7455)
16 (7875 . 8349)
17 (8085 . 8331)
18 (8415 . 8433)
19 (8505 . 8967)
20 (8925 . 8931)
21 (9135 . 9585)
22 (9555 . 9597)
23 (9765 . 10203)
24 (10395 . 12645)
25 (11025 . 11946)
1000 (492975 . 519361)
**** 1000000575 1083561009

```



## PureBasic

{{trans|C}}

```PureBasic
NewList l_sum.i()


Procedure.i sum_proper_divisors(n.i)
  Define.i sum, i=3, j
  Shared l_sum()
  AddElement(l_sum())
  l_sum()=1
  While i<Sqr(n)+1
    If n%i=0
      sum+i
      AddElement(l_sum())
      l_sum()=i
      j=n/i
      If i<>j
        sum+j
        AddElement(l_sum())
        l_sum()=j
      EndIf
    EndIf
    i+2
  Wend
  ProcedureReturn sum+1
EndProcedure


If OpenConsole("Abundant_odd_numbers")
  Define.i n, c, s

  n=1
  c=0
  While c<25
    ClearList(l_sum())
    s=sum_proper_divisors(n)
    If n<s
      SortList(l_sum(),#PB_Sort_Ascending)
      c+1
      Print(RSet(Str(c),3)+": "+RSet(Str(n),6)+" -> "+RSet(Str(s),6))
      ForEach l_sum()
        If ListIndex(l_sum())=0
          Print(" = ")
        Else
          Print("+")
        EndIf
        Print(Str(l_sum()))
      Next
      PrintN("")
    EndIf
    n+2
  Wend

  n-2
  While c<1000
    s=sum_proper_divisors(n+2)
    c+Bool(n<s)
    n+2
  Wend
  PrintN(~"\nThe one thousandth abundant odd number is: "+Str(n)+
         ~"\n\tand the proper divisor sum is: "+Str(s))

  n=1000000001-2
  Repeat
    n+2
    s=sum_proper_divisors(n)
  Until n<s
  PrintN("The first abundant odd number above one billion is: "+Str(n)+
         ~"\n\tand the proper divisor sum is: "+Str(s))

  Input()
EndIf
```

{{out}}

```txt
  1:    945 -&gt;    975 = 1+3+5+7+9+15+21+27+35+45+63+105+135+189+315
  2:   1575 -&gt;   1649 = 1+3+5+7+9+15+21+25+35+45+63+75+105+175+225+315+525
  3:   2205 -&gt;   2241 = 1+3+5+7+9+15+21+35+45+49+63+105+147+245+315+441+735
  4:   2835 -&gt;   2973 = 1+3+5+7+9+15+21+27+35+45+63+81+105+135+189+315+405+567+945
  5:   3465 -&gt;   4023 = 1+3+5+7+9+11+15+21+33+35+45+55+63+77+99+105+165+231+315+385+495+693+1155
  6:   4095 -&gt;   4641 = 1+3+5+7+9+13+15+21+35+39+45+63+65+91+105+117+195+273+315+455+585+819+1365
  7:   4725 -&gt;   5195 = 1+3+5+7+9+15+21+25+27+35+45+63+75+105+135+175+189+225+315+525+675+945+1575
  8:   5355 -&gt;   5877 = 1+3+5+7+9+15+17+21+35+45+51+63+85+105+119+153+255+315+357+595+765+1071+1785
  9:   5775 -&gt;   6129 = 1+3+5+7+11+15+21+25+33+35+55+75+77+105+165+175+231+275+385+525+825+1155+1925
 10:   5985 -&gt;   6495 = 1+3+5+7+9+15+19+21+35+45+57+63+95+105+133+171+285+315+399+665+855+1197+1995
 11:   6435 -&gt;   6669 = 1+3+5+9+11+13+15+33+39+45+55+65+99+117+143+165+195+429+495+585+715+1287+2145
 12:   6615 -&gt;   7065 = 1+3+5+7+9+15+21+27+35+45+49+63+105+135+147+189+245+315+441+735+945+1323+2205
 13:   6825 -&gt;   7063 = 1+3+5+7+13+15+21+25+35+39+65+75+91+105+175+195+273+325+455+525+975+1365+2275
 14:   7245 -&gt;   7731 = 1+3+5+7+9+15+21+23+35+45+63+69+105+115+161+207+315+345+483+805+1035+1449+2415
 15:   7425 -&gt;   7455 = 1+3+5+9+11+15+25+27+33+45+55+75+99+135+165+225+275+297+495+675+825+1485+2475
 16:   7875 -&gt;   8349 = 1+3+5+7+9+15+21+25+35+45+63+75+105+125+175+225+315+375+525+875+1125+1575+2625
 17:   8085 -&gt;   8331 = 1+3+5+7+11+15+21+33+35+49+55+77+105+147+165+231+245+385+539+735+1155+1617+2695
 18:   8415 -&gt;   8433 = 1+3+5+9+11+15+17+33+45+51+55+85+99+153+165+187+255+495+561+765+935+1683+2805
 19:   8505 -&gt;   8967 = 1+3+5+7+9+15+21+27+35+45+63+81+105+135+189+243+315+405+567+945+1215+1701+2835
 20:   8925 -&gt;   8931 = 1+3+5+7+15+17+21+25+35+51+75+85+105+119+175+255+357+425+525+595+1275+1785+2975
 21:   9135 -&gt;   9585 = 1+3+5+7+9+15+21+29+35+45+63+87+105+145+203+261+315+435+609+1015+1305+1827+3045
 22:   9555 -&gt;   9597 = 1+3+5+7+13+15+21+35+39+49+65+91+105+147+195+245+273+455+637+735+1365+1911+3185
 23:   9765 -&gt;  10203 = 1+3+5+7+9+15+21+31+35+45+63+93+105+155+217+279+315+465+651+1085+1395+1953+3255
 24:  10395 -&gt;  12645 = 1+3+5+7+9+11+15+21+27+33+35+45+55+63+77+99+105+135+165+189+231+297+315+385+495+693+945+1155+1485+2079+3465
 25:  11025 -&gt;  11946 = 1+3+5+7+9+15+21+25+35+45+49+63+75+105+147+175+225+245+315+441+525+735+1225+1575+2205+3675

The one thousandth abundant odd number is: 492975
	and the proper divisor sum is: 519361
The first abundant odd number above one billion is: 1000000575
	and the proper divisor sum is: 1083561009

```



## Python


### Procedural

{{trans|Visual Basic .NET}}

```Python
#!/usr/bin/python
# Abundant odd numbers - Python

oddNumber  = 1
aCount  = 0
dSum  = 0

from math import sqrt

def divisorSum(n):
    sum = 1
    i = int(sqrt(n)+1)

    for d in range (2, i):
        if n % d == 0:
            sum += d
            otherD = n // d
            if otherD != d:
                sum += otherD
    return sum

print ("The first 25 abundant odd numbers:")
while aCount  < 25:
    dSum  = divisorSum(oddNumber )
    if dSum  > oddNumber :
        aCount  += 1
        print("{0:5} proper divisor sum: {1}". format(oddNumber ,dSum ))
    oddNumber  += 2

while aCount  < 1000:
    dSum  = divisorSum(oddNumber )
    if dSum  > oddNumber :
        aCount  += 1
    oddNumber  += 2
print ("\n1000th abundant odd number:")
print ("    ",(oddNumber - 2)," proper divisor sum: ",dSum)

oddNumber  = 1000000001
found  = False
while not found :
    dSum  = divisorSum(oddNumber )
    if dSum  > oddNumber :
        found  = True
        print ("\nFirst abundant odd number > 1 000 000 000:")
        print ("    ",oddNumber," proper divisor sum: ",dSum)
    oddNumber  += 2
```

{{out}}

```txt

The first 25 abundant odd numbers:
  945 proper divisor sum: 975
 1575 proper divisor sum: 1649
 2205 proper divisor sum: 2241
 2835 proper divisor sum: 2973
 3465 proper divisor sum: 4023
 4095 proper divisor sum: 4513
 4725 proper divisor sum: 5195
 5355 proper divisor sum: 5877
 5775 proper divisor sum: 5977
 5985 proper divisor sum: 6495
 6435 proper divisor sum: 6669
 6615 proper divisor sum: 7065
 6825 proper divisor sum: 7063
 7245 proper divisor sum: 7731
 7425 proper divisor sum: 7455
 7875 proper divisor sum: 8349
 8085 proper divisor sum: 8331
 8415 proper divisor sum: 8433
 8505 proper divisor sum: 8967
 8925 proper divisor sum: 8931
 9135 proper divisor sum: 9585
 9555 proper divisor sum: 9597
 9765 proper divisor sum: 10203
10395 proper divisor sum: 12645
11025 proper divisor sum: 11946

1000th abundant odd number:
     492975  proper divisor sum:  519361

First abundant odd number > 1 000 000 000:
     1000000575  proper divisor sum:  1083561009

```



### Functional


```python
'''Odd abundant numbers'''

from math import sqrt
from itertools import chain, count, islice


# abundantTuple :: Int -> [(Int, Int)]
def abundantTuple(n):
    '''A list containing the tuple of N and its divisor
       sum, if n is abundant, or an empty list.
    '''
    x = divisorSum(n)
    return [(n, x)] if n < x else []


#  divisorSum :: Int -> Int
def divisorSum(n):
    '''Sum of the divisors of n.'''
    floatRoot = sqrt(n)
    intRoot = int(floatRoot)
    blnSquare = intRoot == floatRoot
    lows = [x for x in range(1, 1 + intRoot) if 0 == n % x]
    return sum(lows + [
        n // x for x in (
            lows[1:-1] if blnSquare else lows[1:]
        )
    ])


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Subsets of abundant odd numbers.'''

    # First 25.
    print('First 25 abundant odd numbers with their divisor sums:')
    for x in take(25)(
            concatMap(abundantTuple)(
                enumFromThen(1)(3)
            )
    ):
        print(x)

    # The 1000th.
    print('\n1000th odd abundant number with its divisor sum:')
    print(
        take(1000)(
            concatMap(abundantTuple)(
                enumFromThen(1)(3)
            )
        )[-1]
    )

    # First over 10^9.
    print('\nFirst odd abundant number over 10^9, with its divisor sum:')
    billion = (10 ** 9)
    print(
        take(1)(
            concatMap(abundantTuple)(
                enumFromThen(1 + billion)(3 + billion)
            )
        )[0]
    )


# GENERAL FUNCTIONS ---------------------------------------

# enumFromThen :: Int -> Int -> [Int]
def enumFromThen(m):
    '''A non-finite stream of integers
       starting at m, and continuing
       at the interval between m and n.
    '''
    return lambda n: count(m, n - m)


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function f
       has been mapped.
       The list monad can be derived by using an (a -> [b])
       function which wraps its output in a list (using an
       empty list to represent computational failure).
    '''
    return lambda xs: (
        chain.from_iterable(map(f, xs))
    )


# take :: Int -> [a] -> [a]
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.
    '''
    return lambda xs: (
        list(islice(xs, n))
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
First 25 abundant odd numbers with their divisor sums:
(945, 975)
(1575, 1649)
(2205, 2241)
(2835, 2973)
(3465, 4023)
(4095, 4641)
(4725, 5195)
(5355, 5877)
(5775, 6129)
(5985, 6495)
(6435, 6669)
(6615, 7065)
(6825, 7063)
(7245, 7731)
(7425, 7455)
(7875, 8349)
(8085, 8331)
(8415, 8433)
(8505, 8967)
(8925, 8931)
(9135, 9585)
(9555, 9597)
(9765, 10203)
(10395, 12645)
(11025, 11946)

1000th odd abundant number with its divisor sum:
(492975, 519361)

First odd abundant number over 10^9, with its divisor sum:
(1000000575, 1083561009)
```



## Racket



```racket
#lang racket

(require math/number-theory
         racket/generator)

(define (make-generator start)
  (in-generator
   (for ([n (in-naturals start)] #:when (odd? n))
     (define divisor-sum (- (apply + (divisors n)) n))
     (when (> divisor-sum n) (yield (list n divisor-sum))))))

(for/list ([i (in-range 25)] [x (make-generator 0)]) x) ; Task 1
(for/last ([i (in-range 1000)] [x (make-generator 0)]) x) ; Task 2
(for/first ([x (make-generator (add1 (inexact->exact 1e9)))]) x) ; Task 3
```


{{out}}

```txt

'((945 975)
  (1575 1649)
  (2205 2241)
  (2835 2973)
  (3465 4023)
  (4095 4641)
  (4725 5195)
  (5355 5877)
  (5775 6129)
  (5985 6495)
  (6435 6669)
  (6615 7065)
  (6825 7063)
  (7245 7731)
  (7425 7455)
  (7875 8349)
  (8085 8331)
  (8415 8433)
  (8505 8967)
  (8925 8931)
  (9135 9585)
  (9555 9597)
  (9765 10203)
  (10395 12645)
  (11025 11946))
'(492975 519361)
'(1000000575 1083561009)

```



## REXX

A wee bit of coding was added to add commas to numbers (because of the larger numbers) as well as alignment of the output.

The   '''sigO'''   is a specialized version of   '''sigma'''   optimized just for odd numbers.

```rexx
/*REXX pgm displays abundant odd numbers:  1st 25,  one─thousandth,  first > 1 billion. */
parse arg Nlow Nuno Novr .                       /*obtain optional arguments from the CL*/
if Nlow=='' | Nlow==","  then Nlow=          25  /*Not specified?  Then use the default.*/
if Nuno=='' | Nuno==","  then Nuno=        1000  /* "      "         "   "   "     "    */
if Novr=='' | Novr==","  then Novr=  1000000000  /* "      "         "   "   "     "    */
numeric digits max(9, length(Novr) )             /*ensure enough decimal digits for  // */
@= 'odd abundant number'                         /*variable for annotating the output.  */
# = 0                                            /*count of odd abundant numbers so far.*/
      do j=3  by 2  until #>=Nlow;   $= sigO(j)  /*get the  sigma  for an odd integer.  */
      if $<=j  then iterate                      /*sigma  ≤  J ?    Then ignore it.     */
      #= # + 1                                   /*bump the counter for abundant odd #'s*/
      say rt(th(#))   @    'is:'rt(commas(j), 8)    rt("sigma=")    rt(commas($), 9)
      end  /*j*/
say
# = 0                                            /*count of odd abundant numbers so far.*/
      do j=3  by 2;                  $= sigO(j)  /*get the  sigma  for an odd integer.  */
      if $<=j    then iterate                    /*sigma  ≤  J ?    Then ignore it.     */
      #= # + 1                                   /*bump the counter for abundant odd #'s*/
      if #<Nuno  then iterate                    /*Odd abundant# count<Nuno?  Then skip.*/
      say rt(th(#))   @    'is:'rt(commas(j), 8)    rt("sigma=")    rt(commas($), 9)
      leave                                      /*we're finished displaying NUNOth num.*/
      end  /*j*/
say
      do j=1+Novr%2*2  by 2;         $= sigO(j)  /*get sigma for an odd integer > Novr. */
      if $<=j    then iterate                    /*sigma  ≤  J ?    Then ignore it.     */
      say rt(th(1))   @  'over'  commas(Novr)  "is: "   commas(j)  rt('sigma=')  commas($)
      leave                                      /*we're finished displaying NOVRth num.*/
      end  /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas:parse arg _;  do c_=length(_)-3  to 1  by -3; _=insert(',', _, c_);  end;  return _
rt:    procedure;  parse arg #,len;     if len==''  then len= 20;     return right(#, len)
th:    parse arg th; return th||word('th st nd rd',1+(th//10)*(th//100%10\==1)*(th//10<4))
/*──────────────────────────────────────────────────────────────────────────────────────*/
sigO:  parse arg x;            s= 1              /*sigma for odd integers.           ___*/
                do k=3  by 2  while k*k<x        /*divide by all odd integers up to √ x */
                if x//k==0  then  s= s + k + x%k /*add the two divisors to (sigma) sum. */
                end   /*k*/                      /*                                  ___*/
       if k*k==x  then  return s + k             /*Was  X  a square?    If so, add  √ x */
                        return s                 /*return (sigma) sum of the divisors.  */
```

{{out|output|text=  when using the default input:}}

```txt

                 1st odd abundant number is:     945               sigma=       975
                 2nd odd abundant number is:   1,575               sigma=     1,649
                 3rd odd abundant number is:   2,205               sigma=     2,241
                 4th odd abundant number is:   2,835               sigma=     2,973
                 5th odd abundant number is:   3,465               sigma=     4,023
                 6th odd abundant number is:   4,095               sigma=     4,641
                 7th odd abundant number is:   4,725               sigma=     5,195
                 8th odd abundant number is:   5,355               sigma=     5,877
                 9th odd abundant number is:   5,775               sigma=     6,129
                10th odd abundant number is:   5,985               sigma=     6,495
                11th odd abundant number is:   6,435               sigma=     6,669
                12th odd abundant number is:   6,615               sigma=     7,065
                13th odd abundant number is:   6,825               sigma=     7,063
                14th odd abundant number is:   7,245               sigma=     7,731
                15th odd abundant number is:   7,425               sigma=     7,455
                16th odd abundant number is:   7,875               sigma=     8,349
                17th odd abundant number is:   8,085               sigma=     8,331
                18th odd abundant number is:   8,415               sigma=     8,433
                19th odd abundant number is:   8,505               sigma=     8,967
                20th odd abundant number is:   8,925               sigma=     8,931
                21st odd abundant number is:   9,135               sigma=     9,585
                22nd odd abundant number is:   9,555               sigma=     9,597
                23rd odd abundant number is:   9,765               sigma=    10,203
                24th odd abundant number is:  10,395               sigma=    12,645
                25th odd abundant number is:  11,025               sigma=    11,946

              1000th odd abundant number is: 492,975               sigma=   519,361

                 1st odd abundant number over 1,000,000,000 is:  1,000,000,575               sigma= 1,083,561,009

```



## Ring


```ring

#Project: Anbundant odd numbers

max = 100000000
limit = 25
nr = 0
m = 1
check = 0
index = 0
see "working..." + nl
see "wait for done..." + nl
while true
      check = 0
      if m%2 = 1
         nice(m)
      ok
      if check = 1
         nr = nr + 1
      ok
      if nr = max
         exit
      ok
      m = m + 1
end
see "done..." + nl

func nice(n)
     check = 0
     nArray = []
     for i = 1 to n - 1
         if n % i = 0
            add(nArray,i)
         ok
     next
     sum = 0
     for p = 1 to len(nArray)
         sum = sum + nArray[p]
     next
     if sum > n
        check = 1
        index = index + 1
        if index < limit + 1
           showArray(n,nArray,sum,index)
        ok
        if index = 100
           see "One thousandth abundant odd number:" + nl
           showArray2(n,nArray,sum,index)
        ok
        if index = 100000000
           see "First abundant odd number above one billion:" + nl
           showArray2(n,nArray,sum,index)
        ok
     ok

func showArray(n,nArray,sum,index)
        see "" + index + ". " + string(n) + ": divisor sum: "
        for m = 1 to len(nArray)
            if m < len(nArray)
               see string(nArray[m]) + " + "
            else
               see string(nArray[m]) + " = " + string(sum) + nl + nl
            ok
        next

func showArray2(n,nArray,sum,index)
        see "" + index + ". " + string(n) + ": divisor sum: " +
        see string(nArray[m]) + " = " + string(sum) + nl + nl

```



```txt

working...
wait for done...
1. 945: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975

2. 1575: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649

3. 2205: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241

4. 2835: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973

5. 3465: divisor sum: 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023

6. 4095: divisor sum: 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641

7. 4725: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195

8. 5355: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877

9. 5775: divisor sum: 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129

10. 5985: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495

11. 6435: divisor sum: 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669

12. 6615: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065

13. 6825: divisor sum: 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063

14. 7245: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731

15. 7425: divisor sum: 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455

16. 7875: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349

17. 8085: divisor sum: 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331

18. 8415: divisor sum: 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433

19. 8505: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967

20. 8925: divisor sum: 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931

21. 9135: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585

22. 9555: divisor sum: 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597

23. 9765: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203

24. 10395: divisor sum: 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645

25. 11025: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946

One thousandth abundant odd number:
1000. 492975: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325 = 519361

First abundant odd number above one billion:
100000000. 1000000575: divisor sum: 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009
done...

```



## Ruby

proper_divisors method taken from http://rosettacode.org/wiki/Proper_divisors#Ruby

```ruby
require "prime"

class Integer
  def proper_divisors
    return [] if self == 1
    primes = prime_division.flat_map{|prime, freq| [prime] * freq}
    (1...primes.size).each_with_object([1]) do |n, res|
      primes.combination(n).map{|combi| res << combi.inject(:*)}
    end.flatten.uniq
  end
end

def generator_odd_abundants(from=1)
  from += 1 if from.even?
  Enumerator.new do |y|
    from.step(nil, 2) do |n|
      sum = n.proper_divisors.sum
      y << [n, sum] if sum > n
    end
  end
end

generator_odd_abundants.take(25).each{|n, sum| puts "#{n} with sum #{sum}" }
puts "\n%d with sum %#d" % generator_odd_abundants.take(1000).last
puts "\n%d with sum %#d" % generator_odd_abundants(1_000_000_000).next

```



## Rust

{{trans|Go}}

```rust
fn divisors(n: u64) -> Vec<u64> {
    let mut divs = vec![1];
    let mut divs2 = Vec::new();

    for i in (2..).take_while(|x| x * x <= n).filter(|x| n % x == 0) {
        divs.push(i);
        let j = n / i;
        if i != j {
            divs2.push(j);
        }
    }
    divs.extend(divs2.iter().rev());

    divs
}

fn sum_string(v: Vec<u64>) -> String {
    v[1..]
        .iter()
        .fold(format!("{}", v[0]), |s, i| format!("{} + {}", s, i))
}

fn abundant_odd(search_from: u64, count_from: u64, count_to: u64, print_one: bool) -> u64 {
    let mut count = count_from;
    for n in (search_from..).step_by(2) {
        let divs = divisors(n);
        let total: u64 = divs.iter().sum();
        if total > n {
            count += 1;
            let s = sum_string(divs);
            if !print_one {
                println!("{}. {} < {} = {}", count, n, s, total);
            } else if count == count_to {
                println!("{} < {} = {}", n, s, total);
            }
        }
        if count == count_to {
            break;
        }
    }
    count_to
}

fn main() {
    let max = 25;
    println!("The first {} abundant odd numbers are:", max);
    let n = abundant_odd(1, 0, max, false);

    println!("The one thousandth abundant odd number is:");
    abundant_odd(n, 25, 1000, true);

    println!("The first abundant odd number above one billion is:");
    abundant_odd(1e9 as u64 + 1, 0, 1, true);
}
```

{{out}}

```txt
The first 25 abundant odd numbers are:
1. 945 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975
2. 1575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649
3. 2205 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241
4. 2835 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973
5. 3465 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023
6. 4095 < 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641
7. 4725 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195
8. 5355 < 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877
9. 5775 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129
10. 5985 < 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495
11. 6435 < 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669
12. 6615 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065
13. 6825 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063
14. 7245 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731
15. 7425 < 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455
16. 7875 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349
17. 8085 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331
18. 8415 < 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433
19. 8505 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967
20. 8925 < 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931
21. 9135 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585
22. 9555 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597
23. 9765 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203
24. 10395 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645
25. 11025 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946
The one thousandth abundant odd number is:
479115 < 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 27 + 35 + 39 + 45 + 63 + 65 + 81 + 91 + 105 + 117 + 135 + 169 + 189 + 195 + 273 + 315 + 351 + 405 + 455 + 507 + 567 + 585 + 819 + 845 + 945 + 1053 + 1183 + 1365 + 1521 + 1755 + 2457 + 2535 + 2835 + 3549 + 4095 + 4563 + 5265 + 5915 + 7371 + 7605 + 10647 + 12285 + 13689 + 17745 + 22815 + 31941 + 36855 + 53235 + 68445 + 95823 + 159705 = 583749
The first abundant odd number above one billion is:
1000000575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009

```



## Scala

{{trans|D}}

```scala
import scala.collection.mutable.ListBuffer

object Abundant {
  def divisors(n: Int): ListBuffer[Int] = {
    val divs = new ListBuffer[Int]
    divs.append(1)

    val divs2 = new ListBuffer[Int]
    var i = 2

    while (i * i <= n) {
      if (n % i == 0) {
        val j = n / i
        divs.append(i)
        if (i != j) {
          divs2.append(j)
        }
      }
      i += 1
    }

    divs.appendAll(divs2.reverse)
    divs
  }

  def abundantOdd(searchFrom: Int, countFrom: Int, countTo: Int, printOne: Boolean): Int = {
    var count = countFrom
    var n = searchFrom
    while (count < countTo) {
      val divs = divisors(n)
      val tot = divs.sum
      if (tot > n) {
        count += 1
        if (!printOne || !(count < countTo)) {
          val s = divs.map(a => a.toString).mkString(" + ")
          if (printOne) {
            printf("%d < %s = %d\n", n, s, tot)
          } else {
            printf("%2d. %5d < %s = %d\n", count, n, s, tot)
          }
        }
      }
      n += 2
    }

    n
  }

  def main(args: Array[String]): Unit = {
    val max = 25
    printf("The first %d abundant odd numbers are:\n", max)
    val n = abundantOdd(1, 0, max, printOne = false)

    printf("\nThe one thousandth abundant odd number is:\n")
    abundantOdd(n, 25, 1000, printOne = true)

    printf("\nThe first abundant odd number above one billion is:\n")
    abundantOdd((1e9 + 1).intValue(), 0, 1, printOne = true)
  }
}
```

{{out}}

```txt
The first 25 abundant odd numbers are:
 1.   945 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315 = 975
 2.  1575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525 = 1649
 3.  2205 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735 = 2241
 4.  2835 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945 = 2973
 5.  3465 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155 = 4023
 6.  4095 < 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365 = 4641
 7.  4725 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575 = 5195
 8.  5355 < 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785 = 5877
 9.  5775 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925 = 6129
10.  5985 < 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995 = 6495
11.  6435 < 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145 = 6669
12.  6615 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205 = 7065
13.  6825 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275 = 7063
14.  7245 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415 = 7731
15.  7425 < 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475 = 7455
16.  7875 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625 = 8349
17.  8085 < 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695 = 8331
18.  8415 < 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805 = 8433
19.  8505 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835 = 8967
20.  8925 < 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975 = 8931
21.  9135 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045 = 9585
22.  9555 < 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185 = 9597
23.  9765 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255 = 10203
24. 10395 < 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465 = 12645
25. 11025 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 = 11946

The one thousandth abundant odd number is:
492975 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325 = 519361

The first abundant odd number above one billion is:
1000000575 < 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525 = 1083561009
```



## Sidef


```ruby
func is_abundant(n) {
    n.sigma > 2*n
}

func odd_abundants (from = 1) {
     from =  (from + 2)//3
     from += (from%2 - 1)
     3*from .. Inf `by` 6 -> lazy.grep(is_abundant)
}

say         " Index |      Number | proper divisor sum"
const sep = "-------+-------------+-------------------\n"
const fstr = "%6s | %11s | %11s\n"

print sep

odd_abundants().first(25).each_kv {|k,n|
    printf(fstr, k+1, n, n.sigma-n)
}

with (odd_abundants().nth(1000)) {|n|
    printf(sep + fstr, 1000, n, n.sigma-n)
}

with(odd_abundants(1e9).first) {|n|
    printf(sep + fstr, '***', n, n.sigma-n)
}
```

{{out}}

```txt

 Index |      Number | proper divisor sum
-------+-------------+-------------------
     1 |         945 |         975
     2 |        1575 |        1649
     3 |        2205 |        2241
     4 |        2835 |        2973
     5 |        3465 |        4023
     6 |        4095 |        4641
     7 |        4725 |        5195
     8 |        5355 |        5877
     9 |        5775 |        6129
    10 |        5985 |        6495
    11 |        6435 |        6669
    12 |        6615 |        7065
    13 |        6825 |        7063
    14 |        7245 |        7731
    15 |        7425 |        7455
    16 |        7875 |        8349
    17 |        8085 |        8331
    18 |        8415 |        8433
    19 |        8505 |        8967
    20 |        8925 |        8931
    21 |        9135 |        9585
    22 |        9555 |        9597
    23 |        9765 |       10203
    24 |       10395 |       12645
    25 |       11025 |       11946
-------+-------------+-------------------
  1000 |      492975 |      519361
-------+-------------+-------------------
   *** |  1000000575 |  1083561009

```



## Visual Basic .NET

{{Trans|ALGOL 68}}

```vbnet
Module AbundantOddNumbers
    ' find some abundant odd numbers - numbers where the sum of the proper
    '                                  divisors is bigger than the number
    '                                  itself

    ' returns the sum of the proper divisors of n
    Private Function divisorSum(n As Integer) As Integer
        Dim sum As Integer = 1
        For d As Integer = 2 To Math.Round(Math.Sqrt(n))
            If n Mod d = 0 Then
                sum += d
                Dim otherD As Integer = n \ d
                IF otherD <> d Then
                    sum += otherD
                End If
            End If
        Next d
        Return sum
    End Function

    ' find numbers required by the task
    Public Sub Main(args() As String)
        ' first 25 odd abundant numbers
        Dim oddNumber As Integer = 1
        Dim aCount As Integer = 0
        Dim dSum As Integer = 0
        Console.Out.WriteLine("The first 25 abundant odd numbers:")
        Do While aCount < 25
            dSum = divisorSum(oddNumber)
            If dSum > oddNumber Then
                aCount += 1
                Console.Out.WriteLine(oddNumber.ToString.PadLeft(6) & " proper divisor sum: " & dSum)
            End If
            oddNumber += 2
        Loop
        ' 1000th odd abundant number
        Do While aCount < 1000
            dSum = divisorSum(oddNumber)
            If dSum > oddNumber Then
                aCount += 1
            End If
            oddNumber += 2
        Loop
        Console.Out.WriteLine("1000th abundant odd number:")
        Console.Out.WriteLine("    " & (oddNumber - 2) & " proper divisor sum: " & dSum)
        ' first odd abundant number > one billion
        oddNumber = 1000000001
        Dim found As Boolean = False
        Do While Not found
            dSum = divisorSum(oddNumber)
            If dSum > oddNumber Then
                found = True
                Console.Out.WriteLine("First abundant odd number > 1 000 000 000:")
                Console.Out.WriteLine("    " & oddNumber & " proper divisor sum: " & dSum)
            End If
            oddNumber += 2
        Loop
    End Sub
End Module
```

{{out}}

```txt

The first 25 abundant odd numbers:
   945 proper divisor sum: 975
  1575 proper divisor sum: 1649
  2205 proper divisor sum: 2241
  2835 proper divisor sum: 2973
  3465 proper divisor sum: 4023
  4095 proper divisor sum: 4641
  4725 proper divisor sum: 5195
  5355 proper divisor sum: 5877
  5775 proper divisor sum: 6129
  5985 proper divisor sum: 6495
  6435 proper divisor sum: 6669
  6615 proper divisor sum: 7065
  6825 proper divisor sum: 7063
  7245 proper divisor sum: 7731
  7425 proper divisor sum: 7455
  7875 proper divisor sum: 8349
  8085 proper divisor sum: 8331
  8415 proper divisor sum: 8433
  8505 proper divisor sum: 8967
  8925 proper divisor sum: 8931
  9135 proper divisor sum: 9585
  9555 proper divisor sum: 9597
  9765 proper divisor sum: 10203
 10395 proper divisor sum: 12645
 11025 proper divisor sum: 11946
1000th abundant odd number:
    492975 proper divisor sum: 519361
First abundant odd number > 1 000 000 000:
    1000000575 proper divisor sum: 1083561009

```



## zkl


```zkl
fcn oddAbundants(startAt=3){  //--> iterator
   Walker.zero().tweak(fcn(rn){
      n:=rn.value;
      while(True){
	 sum:=0;
	 foreach d in ([3.. n.toFloat().sqrt().toInt(), 2]){
	    if( (y:=n/d) *d != n) continue;
	    sum += ((y==d) and y or y+d)
	 }
	 if(sum>n){ rn.set(n+2); return(n) }
	 n+=2;
      }
   }.fp(Ref(startAt.isOdd and startAt or startAt+1)))
}
```


```zkl
fcn oddDivisors(n){  // -->sorted List
   [3.. n.toFloat().sqrt().toInt(), 2].pump(List(1),'wrap(d){
      if( (y:=n/d) *d != n) return(Void.Skip);
      if (y==d) y else T(y,d)
    }).flatten().sort()
}
fcn printOAs(oas){  // List | int
   foreach n in (vm.arglist.flatten()){
      ds:=oddDivisors(n);
      println("%6,d: %6,d = %s".fmt(n, ds.sum(0), ds.sort().concat(" + ")))
   }
}
```


```zkl
oaw:=oddAbundants();

println("First 25 abundant odd numbers:");
oaw.walk(25) : printOAs(_);

println("\nThe one thousandth abundant odd number is:");
oaw.drop(1_000 - 25).value : printOAs(_);

println("\nThe first abundant odd number above one billion is:");
printOAs(oddAbundants(1_000_000_000).next());
```

{{out}}
<pre style="height:45ex; font-size:83%">
   945:    975 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 105 + 135 + 189 + 315
 1,575:  1,649 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 315 + 525
 2,205:  2,241 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 35 + 45 + 49 + 63 + 105 + 147 + 245 + 315 + 441 + 735
 2,835:  2,973 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 315 + 405 + 567 + 945
 3,465:  4,023 = 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 165 + 231 + 315 + 385 + 495 + 693 + 1155
 4,095:  4,641 = 1 + 3 + 5 + 7 + 9 + 13 + 15 + 21 + 35 + 39 + 45 + 63 + 65 + 91 + 105 + 117 + 195 + 273 + 315 + 455 + 585 + 819 + 1365
 4,725:  5,195 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 27 + 35 + 45 + 63 + 75 + 105 + 135 + 175 + 189 + 225 + 315 + 525 + 675 + 945 + 1575
 5,355:  5,877 = 1 + 3 + 5 + 7 + 9 + 15 + 17 + 21 + 35 + 45 + 51 + 63 + 85 + 105 + 119 + 153 + 255 + 315 + 357 + 595 + 765 + 1071 + 1785
 5,775:  6,129 = 1 + 3 + 5 + 7 + 11 + 15 + 21 + 25 + 33 + 35 + 55 + 75 + 77 + 105 + 165 + 175 + 231 + 275 + 385 + 525 + 825 + 1155 + 1925
 5,985:  6,495 = 1 + 3 + 5 + 7 + 9 + 15 + 19 + 21 + 35 + 45 + 57 + 63 + 95 + 105 + 133 + 171 + 285 + 315 + 399 + 665 + 855 + 1197 + 1995
 6,435:  6,669 = 1 + 3 + 5 + 9 + 11 + 13 + 15 + 33 + 39 + 45 + 55 + 65 + 99 + 117 + 143 + 165 + 195 + 429 + 495 + 585 + 715 + 1287 + 2145
 6,615:  7,065 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 49 + 63 + 105 + 135 + 147 + 189 + 245 + 315 + 441 + 735 + 945 + 1323 + 2205
 6,825:  7,063 = 1 + 3 + 5 + 7 + 13 + 15 + 21 + 25 + 35 + 39 + 65 + 75 + 91 + 105 + 175 + 195 + 273 + 325 + 455 + 525 + 975 + 1365 + 2275
 7,245:  7,731 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 23 + 35 + 45 + 63 + 69 + 105 + 115 + 161 + 207 + 315 + 345 + 483 + 805 + 1035 + 1449 + 2415
 7,425:  7,455 = 1 + 3 + 5 + 9 + 11 + 15 + 25 + 27 + 33 + 45 + 55 + 75 + 99 + 135 + 165 + 225 + 275 + 297 + 495 + 675 + 825 + 1485 + 2475
 7,875:  8,349 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 125 + 175 + 225 + 315 + 375 + 525 + 875 + 1125 + 1575 + 2625
 8,085:  8,331 = 1 + 3 + 5 + 7 + 11 + 15 + 21 + 33 + 35 + 49 + 55 + 77 + 105 + 147 + 165 + 231 + 245 + 385 + 539 + 735 + 1155 + 1617 + 2695
 8,415:  8,433 = 1 + 3 + 5 + 9 + 11 + 15 + 17 + 33 + 45 + 51 + 55 + 85 + 99 + 153 + 165 + 187 + 255 + 495 + 561 + 765 + 935 + 1683 + 2805
 8,505:  8,967 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 27 + 35 + 45 + 63 + 81 + 105 + 135 + 189 + 243 + 315 + 405 + 567 + 945 + 1215 + 1701 + 2835
 8,925:  8,931 = 1 + 3 + 5 + 7 + 15 + 17 + 21 + 25 + 35 + 51 + 75 + 85 + 105 + 119 + 175 + 255 + 357 + 425 + 525 + 595 + 1275 + 1785 + 2975
 9,135:  9,585 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 29 + 35 + 45 + 63 + 87 + 105 + 145 + 203 + 261 + 315 + 435 + 609 + 1015 + 1305 + 1827 + 3045
 9,555:  9,597 = 1 + 3 + 5 + 7 + 13 + 15 + 21 + 35 + 39 + 49 + 65 + 91 + 105 + 147 + 195 + 245 + 273 + 455 + 637 + 735 + 1365 + 1911 + 3185
 9,765: 10,203 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 31 + 35 + 45 + 63 + 93 + 105 + 155 + 217 + 279 + 315 + 465 + 651 + 1085 + 1395 + 1953 + 3255
10,395: 12,645 = 1 + 3 + 5 + 7 + 9 + 11 + 15 + 21 + 27 + 33 + 35 + 45 + 55 + 63 + 77 + 99 + 105 + 135 + 165 + 189 + 231 + 297 + 315 + 385 + 495 + 693 + 945 + 1155 + 1485 + 2079 + 3465
11,025: 11,946 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675

The one thousandth abundant odd number is:
492,975: 519,361 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 63 + 75 + 105 + 175 + 225 + 313 + 315 + 525 + 939 + 1565 + 1575 + 2191 + 2817 + 4695 + 6573 + 7825 + 10955 + 14085 + 19719 + 23475 + 32865 + 54775 + 70425 + 98595 + 164325

The first abundant odd number above one billion is:
1,000,000,575: 1,083,561,009 = 1 + 3 + 5 + 7 + 9 + 15 + 21 + 25 + 35 + 45 + 49 + 63 + 75 + 105 + 147 + 175 + 225 + 245 + 315 + 441 + 525 + 735 + 1225 + 1575 + 2205 + 3675 + 11025 + 90703 + 272109 + 453515 + 634921 + 816327 + 1360545 + 1904763 + 2267575 + 3174605 + 4081635 + 4444447 + 5714289 + 6802725 + 9523815 + 13333341 + 15873025 + 20408175 + 22222235 + 28571445 + 40000023 + 47619075 + 66666705 + 111111175 + 142857225 + 200000115 + 333333525

```


+++
title = "Euler's sum of powers conjecture"
description = ""
date = 2019-06-23T01:07:08Z
aliases = []
[extra]
id = 19005
[taxonomies]
categories = ["task"]
tags = []
+++

There is a conjecture in mathematics that held for over two hundred years before it was disproved by the finding of a counterexample in 1966 by [http://www.ams.org/journals/mcom/1967-21-097/S0025-5718-1967-0220669-3/S0025-5718-1967-0220669-3.pdf Lander and Parkin].


;Euler's (disproved) sum of powers   [[wp:Euler's sum of powers conjecture|conjecture]]:
   <big>At least  k  positive  k<sup>th</sup>  powers are required to sum to a  k<sup>th</sup>  power,
   except for the trivial case of one  k<sup>th</sup> power:  y<sup>k</sup> = y<sup>k</sup> </big>

Lander and Parkin are known to have used a brute-force search on a   [[wp:CDC_6600|CDC 6600]]   computer restricting numbers to those less than 250.


## Task

Write a program to search for an integer solution for:
<big><big>
: <code> x<sub>0</sub><sup>5</sup> + x<sub>1</sub><sup>5</sup> + x<sub>2</sub><sup>5</sup> + x<sub>3</sub><sup>5</sup> == y<sup>5</sup> </code>
</big></big>
Where all   <big><big> <code> x<sub>i</sub></code></big></big>'s   and   <big><big><code> y </code></big></big>   are distinct integers between   '''0'''   and   '''250'''   (exclusive).

Show an answer here.


## Related tasks

*   [[Pythagorean quadruples]].
*   [[Pythagorean triples]].





## 11l

```11l
F eulers_sum_of_powers()
   V max_n = 250
   V pow_5 = (0 .< max_n).map(n -> Int64(n ^ 5))
   V pow5_to_n = Dict(0 .< max_n, n -> (Int64(n ^ 5), n))

   L(x0) 1 .< max_n
      L(x1) 1 .< x0
         L(x2) 1 .< x1
            L(x3) 1 .< x2
               V pow_5_sum = pow_5[x0] + pow_5[x1] + pow_5[x2] + pow_5[x3]
               I pow_5_sum C pow5_to_n
                  V y = pow5_to_n[pow_5_sum]
                  R (x0, x1, x2, x3, y)

V r = eulers_sum_of_powers()
print(‘#.^5 + #.^5 + #.^5 + #.^5 = #.^5’.format(r[0], r[1], r[2], r[3], r[4]))
```


```txt
133^5 + 110^5 + 84^5 + 27^5 = 144^5
```



## 360 Assembly

This program could have been run in 1964. Here, for maximum compatibility, we use only the basic 360 instruction set. Macro instruction XPRNT can be replaced by a WTO.

```360asm

EULERCO  CSECT
         USING  EULERCO,R13
         B      80(R15)
         DC     17F'0'
         DC     CL8'EULERCO'
         STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15
         ZAP    X1,=P'1'
LOOPX1   ZAP    PT,MAXN            do x1=1 to maxn-4
         SP     PT,=P'4'
         CP     X1,PT
         BH     ELOOPX1
         ZAP    PT,X1
         AP     PT,=P'1'
         ZAP    X2,PT
LOOPX2   ZAP    PT,MAXN            do x2=x1+1 to maxn-3
         SP     PT,=P'3'
         CP     X2,PT
         BH     ELOOPX2
         ZAP    PT,X2
         AP     PT,=P'1'
         ZAP    X3,PT
LOOPX3   ZAP    PT,MAXN            do x3=x2+1 to maxn-2
         SP     PT,=P'2'
         CP     X3,PT
         BH     ELOOPX3
         ZAP    PT,X3
         AP     PT,=P'1'
         ZAP    X4,PT
LOOPX4   ZAP    PT,MAXN            do x4=x3+1 to maxn-1
         SP     PT,=P'1'
         CP     X4,PT
         BH     ELOOPX4
         ZAP    PT,X4
         AP     PT,=P'1'
         ZAP    X5,PT              x5=x4+1
         ZAP    SUMX,=P'0'         sumx=0
         ZAP    PT,X1              x1
         BAL    R14,POWER5
         AP     SUMX,PT
         ZAP    PT,X2              x2
         BAL    R14,POWER5
         AP     SUMX,PT
         ZAP    PT,X3              x3
         BAL    R14,POWER5
         AP     SUMX,PT
         ZAP    PT,X4              x4
         BAL    R14,POWER5
         AP     SUMX,PT            sumx=x1**5+x2**5+x3**5+x4**5
         ZAP    PT,X5              x5
         BAL    R14,POWER5
         ZAP    VALX,PT            valx=x5**5
LOOPX5   CP     X5,MAXN            while x5<=maxn & valx<=sumx
         BH     ELOOPX5
         CP     VALX,SUMX
         BH     ELOOPX5
         CP     VALX,SUMX          if valx=sumx
         BNE    NOTEQUAL
         MVI    BUF,C' '
         MVC    BUF+1(79),BUF      clear buffer
         MVC    WC,MASK
         ED     WC,X1              x1
         MVC    BUF+0(8),WC+8
         MVC    WC,MASK
         ED     WC,X2              x2
         MVC    BUF+8(8),WC+8
         MVC    WC,MASK
         ED     WC,X3              x3
         MVC    BUF+16(8),WC+8
         MVC    WC,MASK
         ED     WC,X4              x4
         MVC    BUF+24(8),WC+8
         MVC    WC,MASK
         ED     WC,X5              x5
         MVC    BUF+32(8),WC+8
         XPRNT  BUF,80             output x1,x2,x3,x4,x5
         B      ELOOPX1
NOTEQUAL ZAP    PT,X5
         AP     PT,=P'1'
         ZAP    X5,PT              x5=x5+1
         ZAP    PT,X5
         BAL    R14,POWER5
         ZAP    VALX,PT            valx=x5**5
         B      LOOPX5
ELOOPX5  AP     X4,=P'1'
         B      LOOPX4
ELOOPX4  AP     X3,=P'1'
         B      LOOPX3
ELOOPX3  AP     X2,=P'1'
         B      LOOPX2
ELOOPX2  AP     X1,=P'1'
         B      LOOPX1
ELOOPX1  L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
POWER5   ZAP    PQ,PT              ^1
         MP     PQ,PT              ^2
         MP     PQ,PT              ^3
         MP     PQ,PT              ^4
         MP     PQ,PT              ^5
         ZAP    PT,PQ
         BR     R14
MAXN     DC     PL8'250'
X1       DS     PL8
X2       DS     PL8
X3       DS     PL8
X4       DS     PL8
X5       DS     PL8
SUMX     DS     PL8
VALX     DS     PL8
PT       DS     PL8
PQ       DS     PL8
WC       DS     CL17
MASK     DC     X'40',13X'20',X'212060'  CL17
BUF      DS     CL80
         YREGS
         END

```

```txt

      27      84     110     133     144

```



## Ada



```Ada
with Ada.Text_IO;

procedure Sum_Of_Powers is

   type Base is range 0 .. 250; -- A, B, C, D and Y are in that range
   type Num is range 0 .. 4*(250**5); -- (A**5 + ... + D**5) is in that range
   subtype Fit is Num range 0 .. 250**5; -- Y**5 is in that range

   Modulus: constant Num := 254;
   type Modular is mod Modulus;

   type Result_Type is array(1..5) of Base; -- this will hold A,B,C,D and Y

   type Y_Type is array(Modular) of Base;
   type Y_Sum_Type is array(Modular) of Fit;

   Y_Sum: Y_Sum_Type := (others => 0);
   Y: Y_Type := (others => 0);
      -- for I in 0 .. 250, we set Y_Sum(I**5 mod Modulus) := I**5
      --                       and Y(I**5 mod Modulus) := I
      -- Modulus has been chosen to avoid collisions on (I**5 mod Modulus)
      -- later we will compute Sum_ABCD := A**5 + B**5 + C**5 + D**5
      -- and check if Y_Sum(Sum_ABCD mod modulus) = Sum_ABCD

   function Compute_Coefficients return Result_Type is

      Sum_A: Fit;
      Sum_AB, Sum_ABC, Sum_ABCD: Num;
      Short: Modular;

   begin
      for A in Base(0) .. 246 loop
         Sum_A := Num(A) ** 5;
         for B in A .. 247 loop
            Sum_AB := Sum_A + (Num(B) ** 5);
            for C in Base'Max(B,1) .. 248 loop -- if A=B=0 then skip C=0
               Sum_ABC := Sum_AB + (Num(C) ** 5);
               for D in C .. 249 loop
                  Sum_ABCD := Sum_ABC + (Num(D) ** 5);
                  Short    := Modular(Sum_ABCD mod Modulus);
                  if Y_Sum(Short) = Sum_ABCD then
                     return A & B & C & D & Y(Short);
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      return 0 & 0 & 0 & 0 & 0;
   end Compute_Coefficients;

   Tmp: Fit;
   ABCD_Y: Result_Type;

begin -- main program

   -- initialize Y_Sum and Y
   for I in Base(0) .. 250 loop
      Tmp := Num(I)**5;
      if Y_Sum(Modular(Tmp mod Modulus)) /= 0 then
         raise Program_Error with "Collision: Change Modulus and recompile!";
      else
         Y_Sum(Modular(Tmp mod Modulus)) := Tmp;
         Y(Modular(Tmp mod Modulus)) := I;
      end if;
   end loop;

   -- search for a solution (A, B, C, D, Y)
   ABCD_Y := Compute_Coefficients;

   -- output result
   for Number of ABCD_Y loop
      Ada.Text_IO.Put(Base'Image(Number));
   end loop;
   Ada.Text_IO.New_Line;

end Sum_Of_Powers;
```

```txt
 27 84 110 133 144

```



## ALGOL 68

```algol68
# max number will be the highest integer we will consider                    #
INT max number = 250;

# Construct a table of the fifth powers of 1 : max number                    #
[ max number ]LONG INT fifth;
FOR i TO max number DO
    LONG INT i2 =  i * i;
    fifth[ i ] := i2 * i2 * i
OD;

# find the first a, b, c, d, e such that a^5 + b^5 + c^5 + d^5 = e^5         #
# as the fifth powers are in order, we can use a binary search to determine  #
# whether the value is in the table                                          #
BOOL found := FALSE;
FOR a TO max number WHILE NOT found DO
    FOR b FROM a TO max number WHILE NOT found DO
        FOR c FROM b TO max number WHILE NOT found DO
            FOR d FROM c TO max number WHILE NOT found DO
                LONG INT sum   = fifth[a] + fifth[b] + fifth[c] + fifth[d];
                INT      low  := d;
                INT      high := max number;
                WHILE low < high
                  AND NOT found
                DO
                    INT e := ( low + high ) OVER 2;
                    IF fifth[ e ] = sum
                    THEN
                        # the value at e is a fifth power                    #
                        found := TRUE;
                        print( ( ( whole( a, 0 ) + "^5 + " + whole( b, 0 ) + "^5 + "
                                 + whole( c, 0 ) + "^5 + " + whole( d, 0 ) + "^5 = "
                                 + whole( e, 0 ) + "^5"
                                 )
                               , newline
                               )
                             )
                    ELIF sum < fifth[ e ]
                    THEN high := e - 1
                    ELSE low  := e + 1
                    FI
                OD
            OD
        OD
    OD
OD
```

Output:

```txt

27^5 + 84^5 + 110^5 + 133^5 = 144^5

```



## ALGOL W

As suggested by the REXX solution, we find a solution to a^5 + b^5 + c^5 = e^5 - d^5 which results in a significant reduction in run time.


Algol W integers are 32-bit only, so we simulate the necessary 12 digit arithmetic with pairs of integers.

```algolw
begin
    % find a, b, c, d, e such that a^5 + b^5 + c^5 + d^5 = e^5                              %
    %                        where 1 <= a <= b <= c <= d <= e <= 250                        %
    % we solve this using the equivalent equation a^5 + b^5 + c^5 = e^5 - d^5               %
    % 250^5 is 976 562 500 000 - too large for a 32 bit number so we will use pairs of      %
    % integers and constrain their values to be in 0..1 000 000                             %
    % Note only positive numbers are needed                                                 %
    integer MAX_NUMBER, MAX_V;
    MAX_NUMBER := 250;
    MAX_V      := 1000000;
    begin
        % quick sorts the fifth power differences table                                     %
        procedure quickSort5 ( integer value lb, ub ) ;
            if ub > lb then begin
                % more than one element, so must sort                                       %
                integer left, right, pivot, pivotLo, pivotHi;
                left    := lb;
                right   := ub;
                % choosing the middle element of the array as the pivot %
                pivot   := left + ( ( ( right + 1 ) - left ) div 2 );
                pivotLo := loD( pivot );
                pivotHi := hiD( pivot );
                while begin
                    while left  <= ub
                      and begin integer cmp;
                                cmp := hiD( left ) - pivotHi;
                                if cmp = 0 then cmp := loD( left ) - pivotLo;
                                cmp < 0
                          end
                    do left := left + 1;
                    while right >= lb
                      and begin integer cmp;
                                cmp := hiD( right ) - pivotHi;
                                if cmp = 0 then cmp := loD( right ) - pivotLo;
                                cmp > 0
                          end
                    do right := right - 1;
                    left <= right
                end do begin
                    integer swapLo, swapHi, swapD, swapE;
                    swapLo       := loD( left  );
                    swapHi       := hiD( left  );
                    swapD        := Dd(  left  );
                    swapE        := De(  left  );
                    loD( left  ) := loD( right );
                    hiD( left  ) := hiD( right );
                    Dd(  left  ) := Dd(  right );
                    De(  left  ) := De(  right );
                    loD( right ) := swapLo;
                    hiD( right ) := swapHi;
                    Dd(  right ) := swapD;
                    De(  right ) := swapE;
                    left         := left  + 1;
                    right        := right - 1
                end while_left_le_right ;
                quickSort5( lb,   right );
                quickSort5( left, ub    )
            end quickSort5 ;
        % table of fifth powers                                                             %
        integer array lo5, hi5         ( 1 :: MAX_NUMBER );
        % table if differences between fifth powers                                         %
        integer array loD, hiD, De, Dd ( 1 :: MAX_NUMBER * MAX_NUMBER );
        integer dUsed, dPos;
        % compute fifth powers                                                              %
        for i := 1 until MAX_NUMBER do begin
            lo5( i ) := i * i; hi5( i ) := 0;
            for p := 3 until 5 do begin
                integer carry;
                lo5( i ) := lo5( i ) * i;
                carry    := lo5( i ) div MAX_V;
                lo5( i ) := lo5( i ) rem MAX_V;
                hi5( i ) := hi5( i ) * i;
                hi5( i ) := hi5( i ) + carry
            end for_p
        end for_i ;
        % compute the differences between fifth powers e^5 - d^5, 1 <= d < e <= MAX_NUMBER  %
        dUsed := 0;
        for e := 2 until MAX_NUMBER do begin
            for d := 1 until e - 1  do begin
                dUsed := dUsed + 1;
                De(  dUsed ) := e;
                Dd(  dUsed ) := d;
                loD( dUsed ) := lo5( e ) - lo5( d );
                hiD( dUsed ) := hi5( e ) - hi5( d );
                if loD( dUsed ) < 0 then begin
                    loD( dUsed ) := loD( dUsed ) + MAX_V;
                    hiD( dUsed ) := hiD( dUsed ) - 1
                end if_need_to_borrow
            end for_d
        end for_e;
        % sort the fifth power differences                                                  %
        quickSort5( 1, dUsed );
        % attempt to find a^5 + b^5 + c^5 = e^5 - d^5                                       %
        for a := 1 until MAX_NUMBER do begin
            integer loA, hiA;
            loA := lo5( a ); hiA := hi5( a );
            for b := a until MAX_NUMBER do begin
                integer loB, hiB;
                loB := lo5( b ); hiB := hi5( b );
                for c := b until MAX_NUMBER do begin
                    integer low, high, loSum, hiSum;
                    loSum :=                       loA + loB + lo5( c );
                    hiSum := ( loSum div MAX_V ) + hiA + hiB + hi5( c );
                    loSum :=   loSum rem MAX_V;
                    % look for hiSum,loSum in hiD,loD                                       %
                    low   := 1;
                    high  := dUsed;
                    while low < high do begin
                        integer mid, cmp;
                        mid := ( low + high ) div 2;
                        cmp := hiD( mid ) - hiSum;
                        if cmp = 0 then cmp := loD( mid ) - loSum;
                        if cmp = 0 then begin
                            % the value at mid is the difference of two fifth powers        %
                            write( i_w := 1, s_w := 0
                                 , a, "^5 + ", b, "^5 + ", c, "^5 + "
                                 , Dd( mid ), "^5 = ", De( mid ), "^5"
                                 );
                            go to found
                            end
                        else if cmp > 0 then high := mid - 1
                        else                 low  := mid + 1
                    end while_low_lt_high
                end for_c
            end for_b
        end for_a ;
found :
    end
end.
```

```txt

27^5 + 84^5 + 110^5 + 133^5 = 144^5

```



## AWK


```AWK

# syntax: GAWK -f EULERS_SUM_OF_POWERS_CONJECTURE.AWK
BEGIN {
    start_int = systime()
    main()
    printf("%d seconds\n",systime()-start_int)
    exit(0)
}
function main(  sum,s1,x0,x1,x2,x3) {
    for (x0=1; x0<=250; x0++) {
      for (x1=1; x1<=x0; x1++) {
        for (x2=1; x2<=x1; x2++) {
          for (x3=1; x3<=x2; x3++) {
            sum = (x0^5) + (x1^5) + (x2^5) + (x3^5)
            s1 = int(sum ^ 0.2)
            if (sum == s1^5) {
              printf("%d^5 + %d^5 + %d^5 + %d^5 = %d^5\n",x0,x1,x2,x3,s1)
              return
            }
          }
        }
      }
    }
}

```

```txt

133^5 + 110^5 + 84^5 + 27^5 = 144^5
15 seconds

```



## C

The trick to speed up was the observation that for any x we have x^5=x modulo 2, 3, and 5, according to the Fermat's little theorem. Thus, based on the Chinese Remainder Theorem we have x^5==x modulo 30 for any x. Therefore, when we have computed the left sum s=a^5+b^5+c^5+d^5, then we know that the right side e^5 must be such that s==e modulo 30. Thus, we do not have to consider all values of e, but only values in the form e=e0+30k, for some starting value e0, and any k. Also, we follow the constraints 1<=a<b<c<d<e<N in the main loop.

```c
// Alexander Maximov, July 2nd, 2015
#include <stdio.h>
#include <time.h>
typedef long long mylong;

void compute(int N, char find_only_one_solution)
{	const int M = 30;   /* x^5 == x modulo M=2*3*5 */
	int a, b, c, d, e;
	mylong s, t, max, *p5 = (mylong*)malloc(sizeof(mylong)*(N+M));

	for(s=0; s < N; ++s)
		p5[s] = s * s, p5[s] *= p5[s] * s;
	for(max = p5[N - 1]; s < (N + M); p5[s++] = max + 1);

	for(a = 1; a < N; ++a)
	for(b = a + 1; b < N; ++b)
	for(c = b + 1; c < N; ++c)
	for(d = c + 1, e = d + ((t = p5[a] + p5[b] + p5[c]) % M); ((s = t + p5[d]) <= max); ++d, ++e)
	{	for(e -= M; p5[e + M] <= s; e += M); /* jump over M=30 values for e>d */
		if(p5[e] == s)
		{	printf("%d %d %d %d %d\r\n", a, b, c, d, e);
			if(find_only_one_solution) goto onexit;
		}
	}
onexit:
	free(p5);
}

int main(void)
{
	int tm = clock();
	compute(250, 0);
	printf("time=%d milliseconds\r\n", (int)((clock() - tm) * 1000 / CLOCKS_PER_SEC));
	return 0;
}
```

The fair way to measure the speed of the code above is to measure it's run time to find all possible solutions to the problem, given N (and not just a single solution, since then the time may depend on the way and the order we organize for-loops).

```txt

27 84 110 133 144
time=235 milliseconds

```


Another test with N=1000 produces the following results:

```txt

27 84 110 133 144
54 168 220 266 288
81 252 330 399 432
108 336 440 532 576
135 420 550 665 720
162 504 660 798 864
time=65743 milliseconds

```


PS: The solution for C++ provided below is actually quite good in its design idea behind. However, with all proposed tricks to speed up, the measurements for C++ solution for N=1000 showed the execution time 81447ms (+23%) on the same environment as above for C solution (same machine, same compiler, 64-bit platform). The reason that C++ solution is a bit slower is, perhaps, the fact that the inner loops over rs have complexity ~N/2 steps in average, while with the modulo 30 trick that complexity can be reduced down to ~N/60 steps, although one "expensive" extra %-operation is still needed.


## C++

The simplest brute-force find is already reasonably quick:

```cpp
#include <iostream>
#include <cmath>
#include <set>
using namespace std;

bool find()
{
	const auto MAX = 250;
	vector<double> pow5(MAX);
	for (auto i = 1; i < MAX; i++)
		pow5[i] = (double)i * i * i * i * i;
	for (auto x0 = 1; x0 < MAX; x0++) {
		for (auto x1 = 1; x1 < x0; x1++) {
			for (auto x2 = 1; x2 < x1; x2++) {
				for (auto x3 = 1; x3 < x2; x3++) {
					auto sum = pow5[x0] + pow5[x1] + pow5[x2] + pow5[x3];
					if (binary_search(pow5.begin(), pow5.end(), sum))
					{
						cout << x0 << " " << x1 << " " << x2 << " " << x3 << " " << pow(sum, 1.0 / 5.0) << endl;
						return true;
					}
				}
			}
		}
	}
	// not found
	return false;
}

int main(void)
{
	int tm = clock();
	if (!find())
		cout << "Nothing found!\n";
	printf("time=%d milliseconds\r\n", (int)((clock() - tm) * 1000 / CLOCKS_PER_SEC));
	return 0;
}
```

```txt

133 110 84 27 144
time=234 milliseconds

```

We can accelerate this further by creating a parallel std::set<double> p5s containing the elements of the std::vector pow5, and using it to replace the call to std::binary_search:

```cpp
	set<double> pow5s;
	for (auto i = 1; i < MAX; i++)
	{
		pow5[i] = (double)i * i * i * i * i;
		pow5s.insert(pow5[i]);
	}
	//...
        if (pow5s.find(sum) != pow5s.end())
```

This reduces the timing to 125 ms on the same hardware.

A different, more effective optimization is to note that each successive sum is close to the previous one, and use a bidirectional linear search with memory.  We also note that inside the innermost loop, we only need to search upward, so we hoist the downward linear search to the loop over x2.

```cpp
bool find()
{
	const auto MAX = 250;
	vector<double> pow5(MAX);
	for (auto i = 1; i < MAX; i++)
		pow5[i] = (double)i * i * i * i * i;
	auto rs = 5;
	for (auto x0 = 1; x0 < MAX; x0++) {
		for (auto x1 = 1; x1 < x0; x1++) {
			for (auto x2 = 1; x2 < x1; x2++) {
				auto s2 = pow5[x0] + pow5[x1] + pow5[x2];
				while (rs > 0 && pow5[rs] > s2) --rs;
				for (auto x3 = 1; x3 < x2; x3++) {
					auto sum = s2 + pow5[x3];
					while (rs < MAX - 1 && pow5[rs] < sum) ++rs;
					if (pow5[rs] == sum)
					{
						cout << x0 << " " << x1 << " " << x2 << " " << x3 << " " << pow(sum, 1.0 / 5.0) << endl;
						return true;
					}
				}
			}
		}
	}
	// not found
	return false;
}
```

This reduces the timing to around 25 ms.  We could equally well replace rs with an iterator inside pow5; the timing is unaffected.

For comparison with the C code, we also check the timing of an exhaustive search up to MAX=1000.  (Don't try this in Python.)  This takes 87.2 seconds on the same hardware, comparable to the results found by the C code authors, and supports their conclusion that the mod-30 trick used in the C solution leads to better scalability than the iterator optimizations.

Fortunately, we can incorporate the same trick into the above code, by inserting a forward jump to a feasible solution mod 30 into the loop over x3:

```cpp
				for (auto x3 = 1; x3 < x2; x3++)
				{
					// go straight to the first appropriate x3, mod 30
					if (int err30 = (x0 + x1 + x2 + x3 - rs) % 30)
						x3 += 30 - err30;
					if (x3 >= x2)
						break;
					auto sum = s2 + pow5[x3];
```

With this refinement, the exhaustive search up to MAX=1000 takes 16.9 seconds.

Thanks, C guys!


### Second version


We can create a more efficient method by using the idea (taken from the EchoLisp listing below) of precomputing difference between pairs of fifth powers.  If we combine this with the above idea of using linear search with memory, this still requires asymptotically O(N^4) time (because of the linear search within diffs), but is at least twice as fast as the solution above using the mod-30 trick.  Exhaustive search up to MAX=1000 took 6.2 seconds for me (64-bit on 3.4GHz i7).  It is not clear how it can be combined with the mod-30 trick.

The asymptotic behavior can be improved to O(N^3 ln N) by replacing the linear search with an increasing-increment "hunt" (and the outer linear search, which is also O(N^4), with a call to std::upper_bound).  With this replacement, the first solution was found in 0.05 seconds; exhaustive search up to MAX=1000 took 2.80 seconds; and the second nontrivial solution (discarding multiples of the first solution), at y==2615, was found in 94.6 seconds. Note: there is no solution 2615, because 645^5 + 1523^5 + 1722^5 +2506^5 = 122 280 854 808 884 376, but 2615^5=122 280 854 808 884 375. This is an error due to limitation in mantissa of double type (52 bits). 128 bit type is required for the next solution 85359.



```cpp
template<class C_, class LT_> C_ Unique(const C_& src, const LT_& less)
{
	C_ retval(src);
	std::sort(retval.begin(), retval.end(), less);
	retval.erase(unique(retval.begin(), retval.end()), retval.end());
	return retval;
}

template<class I_, class P_> I_ HuntFwd(const I_& hint, const I_& end, const P_& less)	// if less(x) is false, then less(x+1) must also be false
{
	I_ retval(hint);
	int step = 1;
	// expanding phase
	while (end - retval > step)
	{
		I_ test = retval + step;
		if (!less(test))
			break;
		retval = test;
		step <<= 1;
	}
	// contracting phase
	while (step > 1)
	{
		step >>= 1;
		if (end - retval <= step)
			continue;
		I_ test = retval + step;
		if (less(test))
			retval = test;
	}
	if (retval != end && less(retval))
		++retval;
	return retval;
}

bool DPFind(int how_many)
{
	const int MAX = 1000;
	vector<double> pow5(MAX);
	for (int i = 1; i < MAX; i++)
		pow5[i] = (double)i * i * i * i * i;
	vector<pair<double, int>> diffs;
	for (int i = 2; i < MAX; ++i)
	{
		for (int j = 1; j < i; ++j)
			diffs.emplace_back(pow5[i] - pow5[j], j);
	}
	auto firstLess = [](const pair<double, int>& lhs, const pair<double, int>& rhs) { return lhs.first < rhs.first; };
	diffs = Unique(diffs, firstLess);

	for (int x4 = 4; x4 < MAX - 1; ++x4)
	{
		for (int x3 = 3; x3 < x4; ++x3)
		{
			// if (133 * x3 == 110 * x4) continue;	// skip duplicates of first solution
			const auto s2 = pow5[x4] + pow5[x3];
			auto pd = upper_bound(diffs.begin() + 1, diffs.end(), make_pair(s2, 0), firstLess) - 1;
			for (int x2 = 2; x2 < x3; ++x2)
			{
				const auto sum = s2 + pow5[x2];
				pd = HuntFwd(pd, diffs.end(), [&](decltype(pd) it){ return it->first < sum; });
				if (pd != diffs.end() && pd->first == sum && pd->second < x3)	// find each solution only once
				{
					const double y = pow(pd->first + pow5[pd->second], 0.2);
					cout << x4 << " " << x3 << " " << x2 << " " << pd->second << " -> " << static_cast<int>(y + 0.5) << "\n";
					if (--how_many <= 0)
						return true;
				}
			}
		}
	}
	return false;
}
```


Thanks, EchoLisp guys!

## C#
```c#
using System;

namespace EulerSumOfPowers {
    class Program {
        const int MAX_NUMBER = 250;

        static void Main(string[] args) {
            bool found = false;
            long[] fifth = new long[MAX_NUMBER];

            for (int i = 1; i <= MAX_NUMBER; i++) {
                long i2 = i * i;
                fifth[i - 1] = i2 * i2 * i;
            }

            for (int a = 0; a < MAX_NUMBER && !found; a++) {
                for (int b = a; b < MAX_NUMBER && !found; b++) {
                    for (int c = b; c < MAX_NUMBER && !found; c++) {
                        for (int d = c; d < MAX_NUMBER && !found; d++) {
                            long sum = fifth[a] + fifth[b] + fifth[c] + fifth[d];
                            int e = Array.BinarySearch(fifth, sum);
                            found = e >= 0;
                            if (found) {
                                Console.WriteLine("{0}^5 + {1}^5 + {2}^5 + {3}^5 = {4}^5", a + 1, b + 1, c + 1, d + 1, e + 1);
                            }
                        }
                    }
                }
            }
        }
    }
}
```

===Paired Powers, Mod 30, etc...===
```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Euler_cs
{
    class Program
    {
        struct Pair
        {
            public int a, b;
            public Pair(int x, int y)
            {
                a = x; b = y;
            }
        }

        static int min = 1, max = 250;
        static ulong[] p5;
        static SortedDictionary<ulong, Pair>[] sum2 =
                   new SortedDictionary<ulong, Pair>[30];

        static string Fmt(Pair p)
        {
            return string.Format("{0}^5 + {1}^5", p.a, p.b);
        }

        public static void InitM()
        {
            for (int i = 0; i <= 29; i++)
                sum2[i] = new SortedDictionary<ulong, Pair>();
            p5 = new ulong[max + 1];
            p5[min] = Convert.ToUInt64(min) * Convert.ToUInt64(min);
            p5[min] *= p5[min] * Convert.ToUInt64(min);
            for (int i = min; i <= max - 1; i++)
            {
                for (int j = i + 1; j <= max; j++)
                {
                    p5[j] = Convert.ToUInt64(j) * Convert.ToUInt64(j);
                    p5[j] *= p5[j] * Convert.ToUInt64(j);
                    if (j == max) continue;
                    ulong x = p5[i] + p5[j];
                    sum2[x % 30].Add(x, new Pair(i, j));
                }
            }
        }

        static List<string> CalcM(int m)
        {
            List<string> res = new List<string>();
            for (int i = max; i >= min; i--)
            {
                ulong p = p5[i]; int pm = i % 30, mp = (pm - m + 30) % 30;
                foreach (var s in sum2[m].Keys)
                {
                    if (p <= s) break;
                    ulong t = p - s;
                    if (sum2[mp].Keys.Contains(t) && sum2[mp][t].a > sum2[m][s].b)
                        res.Add(string.Format("  {1} + {2} = {0}^5",
                            i, Fmt(sum2[m][s]), Fmt(sum2[mp][t])));
                }
            }
            return res;
        }

        static int Snip(string s)
        {
            int p = s.IndexOf("=") + 1;
            return Convert.ToInt32(s.Substring(p, s.IndexOf("^", p) - p));
        }

        static int CompareRes(string x, string y)
        {
            int res = Snip(x).CompareTo(Snip(y));
            if (res == 0) res = x.CompareTo(y);
            return res;
        }

        static int Validify(int def, string s)
        {
            int res = def, t = 0; int.TryParse(s, out t);
            if (t >= 1 && t < Math.Pow((double)(ulong.MaxValue >> 1), 0.2))
                res = t;
            return res;
        }

        static void Switch(ref int a, ref int b)
        {
            int t = a; a = b; b = t;
        }

        static void Main(string[] args)
        {
            if (args.Count() > 1)
            {
                min = Validify(min, args[0]);
                max = Validify(max, args[1]);
                if (max < min) Switch(ref max, ref min);
            }
            else if (args.Count() == 1)
                max = Validify(max, args[0]);
            Console.WriteLine("Mod 30 shortcut with threading, checking from {0} to {1}...", min, max);
            List<string> res = new List<string>();
            DateTime st = DateTime.Now;
            List<Task<List<string>>> taskList = new List<Task<List<string>>>();
            InitM();
            for (int j = 0; j <= 29; j++)
            {
                var jj = j;
                taskList.Add(Task.Run(() => CalcM(jj)));
            }
            Task.WhenAll(taskList);
            foreach (var item in taskList.Select(t => t.Result))
                res.AddRange(item);
            res.Sort(CompareRes);
            foreach (var item in res)
                Console.WriteLine(item);
            Console.WriteLine("  Computation time to check entire space was {0} seconds",
                (DateTime.Now - st).TotalSeconds);
            if (System.Diagnostics.Debugger.IsAttached)
                Console.ReadKey();
        }
    }
}
```

{{out}}(no command line arguments)
```txt
Mod 30 shortcut with threading, checking from 1 to 250...
27^5 + 84^5 + 110^5 + 133^5 = 144^5
Computation time to check entire space was 0.0838058 seconds
```

(command line argument = "1000")
```txt
Mod 30 shortcut with threading, checking from 1 to 1000...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5
  54^5 + 168^5 + 220^5 + 266^5 = 288^5
  81^5 + 252^5 + 330^5 + 399^5 = 432^5
  108^5 + 336^5 + 440^5 + 532^5 = 576^5
  135^5 + 420^5 + 550^5 + 665^5 = 720^5
  162^5 + 504^5 + 660^5 + 798^5 = 864^5
  Computation time to check entire space was 5.4109744 seconds
```



## Clojure


```Clojure

(ns test-p.core
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.data.int-map :as i]))

(defn solve-power-sum [max-value max-sols]
  " Finds solutions by using method approach of EchoLisp
    Large difference is we store a dictionary of all combinations
    of y^5 - x^5 with the x, y value so we can simply lookup rather than have to search "
  (let [pow5 (mapv #(math/expt % 5) (range 0 (* 4 max-value)))                  ; Pow5 = Generate Lookup table for x^5
        y5-x3 (into (i/int-map) (for [x (range 1 max-value)                     ; For x0^5 + x1^5 + x2^5 + x3^5  = y^5
                                      y (range (+ 1 x) (* 4 max-value))]        ; compute y5-x3 = set of all possible differnences
                                  [(- (get pow5 y) (get pow5 x)) [x y]])) ; note: (get pow5 y) is closure for: pow5[y]
        solutions-found (atom 0)]

    (for [x0 (range 1 max-value)                                    ; Search over x0, x1, x2 for sums equal y5-x3
          x1 (range 1 x0)
          x2 (range 1 x1)
          :when (< @solutions-found max-sols)
          :let [sum (apply + (map pow5 [x0 x1 x2]))]         ; compute sum of items to the 5th power
          :when (contains? y5-x3 sum)]                       ; check if sum is in set of differences
      (do
        (swap! solutions-found inc)                          ; increment counter for solutions found
        (concat [x0 x1 x2] (get y5-x3 sum))))))              ; create result (since in set of differences)

; Output results with numbers in ascending order placing results into a set (i.e. #{}) so duplicates are discarded
                                                            ; CPU i7 920 Quad Core @2.67 GHz clock Windows 10
(println (into #{} (map sort (solve-power-sum 250 1))))   ; MAX = 250, find only 1 value: Duration was 0.26 seconds
(println (into #{} (map sort (solve-power-sum 1000 1000))));MAX = 1000, high max-value so all solutions found: Time = 4.8 seconds

```

Output

```txt

1st Solution with MAX = 250 (Solution Time: 260 ms CPU i7 920 Quad Core)
#{(27 84 110 133 144))

All Solutions with MAX = 1000 (Solution Time: 4.8 seconds CPU i7 920 Quad Core)
#{(27 84 110 133 144)
(162 504 660 798 864)
(135 420 550 665 720)
(108 336 440 532 576)
(189 588 770 931 1008)
(54 168 220 266 288)
(81 252 330 399 432)}

```



## COBOL


```cobol

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EULER.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       1   TABLE-LENGTH CONSTANT 250.
       1   SEARCHING-FLAG     PIC 9.
        88  FINISHED-SEARCHING VALUE IS 1
                               WHEN SET TO FALSE IS 0.
       1  CALC.
        3  A               PIC 999 USAGE COMPUTATIONAL-5.
        3  B               PIC 999 USAGE COMPUTATIONAL-5.
        3  C               PIC 999 USAGE COMPUTATIONAL-5.
        3  D               PIC 999 USAGE COMPUTATIONAL-5.
        3  ABCD            PIC 9(18) USAGE COMPUTATIONAL-5.
        3  FIFTH-ROOT-OFFS PIC 999 USAGE COMPUTATIONAL-5.
        3  POWER-COUNTER   PIC 999 USAGE COMPUTATIONAL-5.
        88 POWER-MAX       VALUE TABLE-LENGTH.

       1   PRETTY.
        3  A               PIC ZZ9.
        3  FILLER          VALUE "^5 + ".
        3  B               PIC ZZ9.
        3  FILLER          VALUE "^5 + ".
        3  C               PIC ZZ9.
        3  FILLER          VALUE "^5 + ".
        3  D               PIC ZZ9.
        3  FILLER          VALUE "^5 = ".
        3  FIFTH-ROOT-OFFS PIC ZZ9.
        3  FILLER          VALUE "^5.".

       1   FIFTH-POWER-TABLE   OCCURS TABLE-LENGTH TIMES
                               ASCENDING KEY IS FIFTH-POWER
                               INDEXED BY POWER-INDEX.
        3  FIFTH-POWER PIC 9(18) USAGE COMPUTATIONAL-5.


       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           SET FINISHED-SEARCHING TO FALSE.
           PERFORM POWERS-OF-FIVE-TABLE-INIT.
           PERFORM VARYING
               A IN CALC
               FROM 1 BY 1 UNTIL A IN CALC = TABLE-LENGTH

               AFTER B IN CALC
               FROM 1 BY 1 UNTIL B IN CALC = A IN CALC

               AFTER C IN CALC
               FROM 1 BY 1 UNTIL C IN CALC = B IN CALC

               AFTER D IN CALC
               FROM 1 BY 1 UNTIL D IN CALC = C IN CALC

               IF FINISHED-SEARCHING
                   STOP RUN
               END-IF

               PERFORM POWER-COMPUTATIONS

           END-PERFORM.

       POWER-COMPUTATIONS.

           MOVE ZERO TO ABCD IN CALC.

           ADD FIFTH-POWER(A IN CALC)
               FIFTH-POWER(B IN CALC)
               FIFTH-POWER(C IN CALC)
               FIFTH-POWER(D IN CALC)
                   TO ABCD IN CALC.

           SET POWER-INDEX TO 1.

           SEARCH ALL FIFTH-POWER-TABLE
               WHEN FIFTH-POWER(POWER-INDEX) = ABCD IN CALC
                      MOVE POWER-INDEX TO FIFTH-ROOT-OFFS IN CALC
                      MOVE CORRESPONDING CALC TO PRETTY
                      DISPLAY PRETTY END-DISPLAY
                      SET FINISHED-SEARCHING TO TRUE
           END-SEARCH

           EXIT PARAGRAPH.

       POWERS-OF-FIVE-TABLE-INIT.
           PERFORM VARYING POWER-COUNTER FROM 1 BY 1 UNTIL POWER-MAX
               COMPUTE FIFTH-POWER(POWER-COUNTER) =
                   POWER-COUNTER *
                   POWER-COUNTER *
                   POWER-COUNTER *
                   POWER-COUNTER *
                   POWER-COUNTER
               END-COMPUTE
           END-PERFORM.
           EXIT PARAGRAPH.

       END PROGRAM EULER.

```

Output

```txt

133^5 + 110^5 +  84^5 +  27^5 = 144^5.

```



## Common Lisp


```lisp

(ql:quickload :alexandria)
(let ((fifth-powers (mapcar #'(lambda (x) (expt x 5))
                            (alexandria:iota 250))))
  (loop named outer for x0 from 1 to (length fifth-powers) do
    (loop for x1 from 1 below x0 do
      (loop for x2 from 1 below x1 do
        (loop for x3 from 1 below x2 do
          (let ((x-sum (+ (nth x0 fifth-powers)
                          (nth x1 fifth-powers)
                          (nth x2 fifth-powers)
                          (nth x3 fifth-powers))))
            (if (member x-sum fifth-powers)
                  (return-from outer (list x0 x1 x2 x3 (round (expt x-sum 0.2)))))))))))

```

```txt
(133 110 84 27 144)
```



## D


### First version

```d
import std.stdio, std.range, std.algorithm, std.typecons;

auto eulersSumOfPowers() {
    enum maxN = 250;
    auto pow5 = iota(size_t(maxN)).map!(i => ulong(i) ^^ 5).array.assumeSorted;

    foreach (immutable x0; 1 .. maxN)
        foreach (immutable x1; 1 .. x0)
            foreach (immutable x2; 1 .. x1)
                foreach (immutable x3; 1 .. x2) {
                    immutable powSum = pow5[x0] + pow5[x1] + pow5[x2] + pow5[x3];
                    if (pow5.contains(powSum))
                        return tuple(x0, x1, x2, x3, pow5.countUntil(powSum));
                }
    assert(false);
}

void main() {
    writefln("%d^5 + %d^5 + %d^5 + %d^5 == %d^5", eulersSumOfPowers[]);
}
```

```txt
133^5 + 110^5 + 84^5 + 27^5 == 144^5
```

Run-time about 0.64 seconds.
A Range-based Haskell-like solution is missing because of Issue 14833.


### Second version

```d
void main() {
    import std.stdio, std.range, std.algorithm, std.typecons;

    enum uint MAX = 250;
    uint[ulong] p5;
    Tuple!(uint, uint)[ulong] sum2;

    foreach (immutable i; 1 .. MAX) {
        p5[ulong(i) ^^ 5] = i;
        foreach (immutable j; i .. MAX)
            sum2[ulong(i) ^^ 5 + ulong(j) ^^ 5] = tuple(i, j);
    }

    const sk = sum2.keys.sort().release;
    foreach (p; p5.keys.sort())
        foreach (immutable s; sk) {
            if (p <= s)
                break;
            if (p - s in sum2) {
                writeln(p5[p], " ", tuple(sum2[s][], sum2[p - s][]));
                return; // Finds first only.
            }
        }
}
```

```txt
144 Tuple!(uint, uint, uint, uint)(27, 84, 110, 133)
```

Run-time about 0.10 seconds.


### Third version

This solution is a brutal translation of the iterator-based C++ version, and it should be improved to use more idiomatic D Ranges.

```d
import core.stdc.stdio, std.typecons, std.math, std.algorithm, std.range;

alias Pair = Tuple!(double, int);
alias PairPtr = Pair*;

// If less(x) is false, then less(x + 1) must also be false.
PairPtr huntForward(Pred)(PairPtr hint, const PairPtr end, const Pred less) pure nothrow @nogc {
    PairPtr result = hint;
    int step = 1;

    // Expanding phase.
    while (end - result > step) {
        PairPtr test = result + step;
        if (!less(test))
            break;
        result = test;
        step <<= 1;
    }

    // Contracting phase.
    while (step > 1) {
        step >>= 1;
        if (end - result <= step)
            continue;
        PairPtr test = result + step;
        if (less(test))
            result = test;
    }
    if (result != end && less(result))
        ++result;
    return result;
}


bool dPFind(int how_many) nothrow {
    enum MAX = 1_000;

    double[MAX] pow5;
    foreach (immutable i; 1 .. MAX)
        pow5[i] = double(i) ^^ 5;

    Pair[] diffs0; // Will contain (MAX-1) * (MAX-2) / 2 pairs.
    foreach (immutable i; 2 .. MAX)
        foreach (immutable j; 1 .. i)
            diffs0 ~= Pair(pow5[i] - pow5[j], j);

    // Remove pairs with duplicate first items.
    diffs0.length -= diffs0.sort!q{ a[0] < b[0] }.uniq.copy(diffs0).length;
    auto diffs = diffs0.assumeSorted!q{ a[0] < b[0] };

    foreach (immutable x4; 4 .. MAX - 1) {
        foreach (immutable x3; 3 .. x4) {
            immutable s2 = pow5[x4] + pow5[x3];
            auto pd0 = diffs[1 .. $].upperBound(Pair(s2, 0));
            PairPtr pd = &pd0[0] - 1;
            foreach (immutable x2; 2 .. x3) {
                immutable sum = s2 + pow5[x2];
                const PairPtr endPtr = &diffs[$ - 1] + 1;
                // This lambda heap-allocates.
                pd = huntForward(pd, endPtr, (in PairPtr p) pure => (*p)[0] < sum);
                if (pd != endPtr && (*pd)[0] == sum && (*pd)[1] < x3) { // Find each solution only once.
                    immutable y = ((*pd)[0] + pow5[(*pd)[1]]) ^^ 0.2;
                    printf("%d %d %d %d : %d\n", x4, x3, x2, (*pd)[1], cast(int)(y + 0.5));
                    if (--how_many <= 0)
                        return true;
                }
            }
        }
    }

    return false;
}


void main() nothrow {
    if (!dPFind(100))
        printf("Search finished.\n");
}
```

```txt
133 110 27 84 : 144
133 110 84 27 : 144
266 220 54 168 : 288
266 220 168 54 : 288
399 330 81 252 : 432
399 330 252 81 : 432
532 440 108 336 : 576
532 440 336 108 : 576
665 550 135 420 : 720
665 550 420 135 : 720
798 660 162 504 : 864
798 660 504 162 : 864
Search finished.
```

Run-time about 7.1 seconds.


## EchoLisp

To speed up things, we search for x0, x1, x2 such as x0^5 + x1^5 + x2^5 = a difference of 5-th powers.

```lisp

(define dim 250)

;; speed up n^5
(define (p5 n) (* n n n n n))
(remember 'p5) ;; memoize

;; build vector of all  y^5 - x^5 diffs - length 30877
(define all-y^5-x^5
	(for*/vector
		[(x (in-range 1 dim))  (y (in-range (1+ x) dim))]
		(- (p5 y) (p5 x))))

;; sort to use vector-search
(begin (vector-sort! <  all-y^5-x^5) 'sorted)

 ;; find couple (x y) from y^5 - x^5
(define (x-y y^5-x^5)
	(for*/fold (x-y null)
	[(x (in-range 1 dim)) (y (in-range (1+ x ) dim))]
		(when
			(= (- (p5 y) (p5 x)) y^5-x^5)
			(set! x-y (list x y))
			(break #t)))) ; stop on first

;; search
(for*/fold  (sol null)
	[(x0 (in-range 1 dim)) (x1 (in-range (1+ x0) dim)) (x2 (in-range (1+ x1) dim))]
	(set! sol (+ (p5 x0) (p5 x1) (p5 x2)))
 	(when
 		(vector-search sol all-y^5-x^5)  ;; x0^5 + x1^5 + x2^5 = y^5 - x3^5 ???
 		(set! sol (append (list x0 x1 x2) (x-y  sol))) ;; found
 		(break #t))) ;; stop on first

  →   (27 84 110 133 144) ;; time 2.8 sec


```



## Elixir

```elixir
defmodule Euler do
  def sum_of_power(max \\ 250) do
    {p5, sum2} = setup(max)
    sk = Enum.sort(Map.keys(sum2))
    Enum.reduce(Enum.sort(Map.keys(p5)), Map.new, fn p,map ->
      sum(sk, p5, sum2, p, map)
    end)
  end

  defp setup(max) do
    Enum.reduce(1..max, {%{}, %{}}, fn i,{p5,sum2} ->
      i5 = i*i*i*i*i
      add = for j <- i..max, into: sum2, do: {i5 + j*j*j*j*j, [i,j]}
      {Map.put(p5, i5, i), add}
    end)
  end

  defp sum([], _, _, _, map), do: map
  defp sum([s|_], _, _, p, map) when p<=s, do: map
  defp sum([s|t], p5, sum2, p, map) do
    if sum2[p - s],
      do:   sum(t, p5, sum2, p, Map.put(map, Enum.sort(sum2[s] ++ sum2[p-s]), p5[p])),
      else: sum(t, p5, sum2, p, map)
  end
end

Enum.each(Euler.sum_of_power, fn {k,v} ->
  IO.puts Enum.map_join(k, " + ", fn i -> "#{i}**5" end) <> " = #{v}**5"
end)
```


```txt

27**5 + 84**5 + 110**5 + 133**5 = 144**5

```



## ERRE


```ERRE
PROGRAM EULERO

CONST MAX=250

!$DOUBLE

FUNCTION POW5(X)
    POW5=X*X*X*X*X
END FUNCTION

!$INCLUDE="PC.LIB"

BEGIN
   CLS
   FOR X0=1 TO MAX DO
     FOR X1=1 TO X0 DO
        FOR X2=1 TO X1 DO
           FOR X3=1 TO X2 DO
              LOCATE(3,1) PRINT(X0;X1;X2;X3)
              SUM=POW5(X0)+POW5(X1)+POW5(X2)+POW5(X3)
              S1=INT(SUM^0.2#+0.5#)
              IF SUM=POW5(S1) THEN PRINT(X0,X1,X2,X3,S1) END IF
           END FOR
        END FOR
     END FOR
   END FOR
END PROGRAM
```

```txt

133 110 84 27 144

```


=={{header|F_Sharp|F#}}==

```fsharp

//Find 4 integers whose 5th powers sum to the fifth power of an integer (Quickly!) - Nigel Galloway: April 23rd., 2015
let G =
  let GN = Array.init<float> 250 (fun n -> (float n)**5.0)
  let rec gng (n, i, g, e) =
    match (n, i, g, e) with
    | (250,_,_,_) -> "No Solution Found"
    | (_,250,_,_) -> gng (n+1, n+1, n+1, n+1)
    | (_,_,250,_) -> gng (n, i+1, i+1, i+1)
    | (_,_,_,250) -> gng (n, i, g+1, g+1)
    | _ -> let l = GN.[n] + GN.[i] + GN.[g] + GN.[e]
           match l with
           | _ when l > GN.[249]           -> gng(n,i,g+1,g+1)
           | _ when l = round(l**0.2)**5.0 -> sprintf "%d**5 + %d**5 + %d**5 + %d**5 = %d**5" n i g e (int (l**0.2))
           | _                             -> gng(n,i,g,e+1)
  gng (1, 1, 1, 1)

```

```txt

"27**5 + 84**5 + 110**5 + 133**5 = 144**5"

```



## Factor

This solution uses Factor's <code>backtrack</code> vocabulary (based on continuations) to simplify the reduction of the search space. Each time <code>xn</code> is called, a new summand is introduced which can only take on a value as high as the previous summand - 1. This also creates a checkpoint for the backtracker. <code>fail</code> causes the backtracking to occur.

```factor
USING: arrays backtrack kernel literals math.functions
math.ranges prettyprint sequences ;

CONSTANT: pow5 $[ 0 250 [a,b) [ 5 ^ ] map ]

: xn ( n1 -- n2 n2 ) [1,b) amb-lazy dup ;

250 xn xn xn xn drop 4array dup pow5 nths sum dup pow5
member? [ pow5 index suffix . ] [ 2drop fail ] if
```

```txt

{ 133 110 84 27 144 }

```



## Forth

```forth

: sq  dup * ;
: 5^  dup sq sq * ;

create pow5 250 cells allot
:noname
   250 0 DO  i 5^  pow5 i cells + !  LOOP ; execute

: @5^  cells pow5 + @ ;

: solution? ( n -- n )
   pow5 250 cells bounds DO
      dup i @ = IF  drop i pow5 - cell / unloop EXIT  THEN
   cell +LOOP drop 0 ;

\ GFORTH only provides 2 index variables: i, j
\ so the code creates locals for two outer loop vars, k & l

: euler  ( -- )
   250 4 DO i { l }
      l 3 DO i { k }
         k 2 DO
            i 1 DO
               i @5^ j @5^ + k @5^ + l @5^ + solution?
               dup IF
                  l . k . j . i . . cr
                  unloop unloop unloop unloop EXIT
               ELSE
                  drop
               THEN
            LOOP
         LOOP
      LOOP
   LOOP ;

euler
bye

```

```txt

$ gforth-fast ./euler.fs
133 110 84 27 144

```


## Fortran


### FORTRAN IV

To solve this problem, we must handle integers up 250**5 ~= 9.8*10**11 . So we need integers with at less 41 bits.
In 1966 all Fortrans were not equal. On IBM360, INTEGER was a 32-bit integer; on CDC6600, INTEGER was a 60-bit integer.
And Leon J. Lander and Thomas R. Parkin used the CDC6600.

```fortran
C EULER SUM OF POWERS CONJECTURE - FORTRAN IV
C FIND I1,I2,I3,I4,I5 : I1**5+I2**5+I3**5+I4**5=I5**5
      INTEGER I,P5(250),SUMX
      MAXN=250
      DO 1 I=1,MAXN
   1  P5(I)=I**5
      DO 6 I1=1,MAXN
      DO 6 I2=1,MAXN
      DO 6 I3=1,MAXN
      DO 6 I4=1,MAXN
      SUMX=P5(I1)+P5(I2)+P5(I3)+P5(I4)
      I5=1
   2  IF(I5-MAXN) 3,3,6
   3  IF(P5(I5)-SUMX) 5,4,6
   4  WRITE(*,300) I1,I2,I3,I4,I5
      STOP
   5  I5=I5+1
      GOTO 2
   6  CONTINUE
 300  FORMAT(5(1X,I3))
      END
```

```txt

  27  84 110 133 144

```



### Fortran 95

```fortran
program sum_of_powers
  implicit none

  integer, parameter :: maxn = 249
  integer, parameter :: dprec = selected_real_kind(15)
  integer :: i, x0, x1, x2, x3, y
  real(dprec) :: n(maxn), sumx

  n = (/ (real(i, dprec)**5, i = 1, maxn) /)

outer: do x0 = 1, maxn
         do x1 = 1, maxn
           do x2 = 1, maxn
             do x3 = 1, maxn
               sumx = n(x0)+ n(x1)+ n(x2)+ n(x3)
               y = 1
               do while(y <= maxn .and. n(y) <= sumx)
                 if(n(y) == sumx) then
                   write(*,*) x0, x1, x2, x3, y
                   exit outer
                 end if
                 y = y + 1
               end do
             end do
           end do
         end do
       end do outer

end program
```

```txt
          27          84         110         133         144
```



## FreeBASIC


```freebasic
' version 14-09-2015
' compile with: fbc -s console

' some constants calculated when the program is compiled

Const As UInteger max = 250
Const As ULongInt pow5_max = CULngInt(max) * max * max * max * max
' limit x1, x2, x3
Const As UInteger limit_x1 = (pow5_max / 4) ^ 0.2
Const As UInteger limit_x2 = (pow5_max / 3) ^ 0.2
Const As UInteger limit_x3 = (pow5_max / 2) ^ 0.2

' ------=< MAIN >=------

Dim As ULongInt pow5(max), ans1, ans2, ans3
Dim As UInteger x1, x2, x3, x4, x5 , m1, m2

Cls : Print

For x1 = 1 To max
    pow5(x1) = CULngInt(x1) * x1 * x1 * x1 * x1
Next x1

For x1 = 1 To limit_x1
    For x2 = x1 +1 To limit_x2
        m1 = x1 + x2
        ans1 = pow5(x1) + pow5(x2)
        If ans1 > pow5_max Then Exit For
        For x3 = x2 +1 To limit_x3
            ans2 = ans1 + pow5(x3)
            If ans2 > pow5_max Then Exit For
            m2 = (m1 + x3) Mod 30
            If m2 = 0 Then m2 = 30
            For x4 = x3 +1 To max -1
                ans3 = ans2 + pow5(x4)
                If ans3 > pow5_max Then Exit For
                For x5 = x4 + m2 To max Step 30
                    If ans3 < pow5(x5) Then Exit For
                    If ans3 = pow5(x5) Then
                        Print x1; "^5 + "; x2; "^5 + "; x3; "^5 + "; _
                              x4; "^5 = "; x5; "^5"
                        Exit For, For
                    EndIf
                Next x5
            Next x4
        Next x3
    Next x2
Next x1

Print
Print "done"

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
27^5 + 84^5 + 110^5 + 133^5 = 144^5
```



## Go

```go
package main

import (
	"fmt"
	"log"
)

func main() {
	fmt.Println(eulerSum())
}

func eulerSum() (x0, x1, x2, x3, y int) {
	var pow5 [250]int
	for i := range pow5 {
		pow5[i] = i * i * i * i * i
	}
	for x0 = 4; x0 < len(pow5); x0++ {
		for x1 = 3; x1 < x0; x1++ {
			for x2 = 2; x2 < x1; x2++ {
				for x3 = 1; x3 < x2; x3++ {
					sum := pow5[x0] +
						pow5[x1] +
						pow5[x2] +
						pow5[x3]
					for y = x0 + 1; y < len(pow5); y++ {
						if sum == pow5[y] {
							return
						}
					}
				}
			}
		}
	}
	log.Fatal("no solution")
	return
}
```

```txt

133 110 84 27 144

```



## Haskell


```haskell
import Data.List
import Data.List.Ordered

main :: IO ()
main = print $ head [(x0,x1,x2,x3,x4) |
                                        -- choose x0, x1, x2, x3
                                        -- so that 250 < x3 < x2 < x1 < x0
                                        x3 <- [1..250-1],
                                        x2 <- [1..x3-1],
                                        x1 <- [1..x2-1],
                                        x0 <- [1..x1-1],

                                        let p5Sum = x0^5 + x1^5 + x2^5 + x3^5,

                                        -- lazy evaluation of powers of 5
                                        let p5List = [i^5|i <- [1..]],

                                        -- is sum a power of 5 ?
                                        member p5Sum p5List,

                                        -- which power of 5 is sum ?
                                        let Just x4 = elemIndex p5Sum p5List ]
```

```txt

(27,84,110,133,144)

```


Or, using dictionaries of powers and sums, and thus rather faster:
```haskell
import qualified Data.Map.Strict as M
import Data.List (find, intercalate)
import Data.Maybe (maybe)

iFrom, iTo :: Int
iFrom = 1

iTo = 249

xs :: [Int]
xs = [iFrom .. iTo]

powerMap :: M.Map Int Int
powerMap = M.fromList (zip ((^ 5) <$> xs) xs)

sumMap :: M.Map Int (Int, Int)
sumMap =
  M.fromList
    [ ((x ^ 5) + (y ^ 5), (x, y))
    | x <- xs
    , y <- tail xs
    , x > y ]

mbExample :: Maybe (Int, Int)
mbExample =
  find
    (\(p, s) -> M.member (p - s) sumMap)
    (M.keys powerMap >>=
     (\p -> takeWhile (< p) (M.keys sumMap) >>= (\s -> [(p, s)])))

showExample :: (Int, Int) -> String
showExample (p, s) =
  let (a, b) = sumMap M.! (p - s)
      (c, d) = sumMap M.! s
  in "a counter-example in range " ++
     rangeString xs ++
     ":\n\n" ++
     intercalate "^5 + " (show <$> [a, b, c, d]) ++
     "^5 = " ++ show (powerMap M.! p) ++ "^5"

rangeString :: [Int] -> String
rangeString [] = "[]"
rangeString (x:xs) = '[' : show x ++ " .. " ++ show (last xs) ++ "]"

main :: IO ()
main =
  putStrLn $
  "Euler's sum of powers conjecture – " ++
  maybe
    ("no counter-example found in the range " ++ rangeString xs)
    showExample
    mbExample
```

```txt
Euler's sum of powers conjecture – a counter-example in range [1 .. 249]:

133^5 + 110^5 + 84^5 + 27^5 = 144^5
```



## J



```J
   require 'stats'
   (#~ (= <.)@((+/"1)&.:(^&5)))1+4 comb 248
27 84 110 133
```


Explanation:


```J>1+4 comb 248</lang
 finds all the possibilities for our four arguments.

Then,
```J
(#~ (= <.)@((+/"1)&.:(^&5)))
```
 discards the cases we are not interested in. (It only keeps the case(s) where the fifth root of the sum of the fifth powers is an integer.)

Only one possibility remains.

Here's a significantly faster approach (about 100 times faster), based on the echolisp implementation:


```J
find5=:3 :0
  y=. 250
  n=. i.y
  p=. n^5
  a=. (#~ 0&<),-/~p
  s=. /:~a
  l=. (i.*:y)(#~ 0&<),-/~p
  c=. 3 comb <.5%:(y^5)%4
  t=. +/"1 c{p
  x=. (t e. s)#t
  |.,&<&~./|:(y,y)#:l#~a e. x
)
```


Use:


```J
   find5''
┌─────────────┬───┐
│27 84 110 133│144│
└─────────────┴───┘
```


Note that this particular implementation is a bit hackish, since it relies on the solution being unique for the range of numbers being considered. If there were more solutions it would take a little extra code (though not much time) to untangle them.


## Java

Tested with Java 6.

```java
public class eulerSopConjecture
{

    static final int    MAX_NUMBER = 250;

    public static void main( String[] args )
    {
        boolean found = false;
        long[]  fifth = new long[ MAX_NUMBER ];

        for( int i = 1; i <= MAX_NUMBER; i ++ )
        {
            long i2 =  i * i;
            fifth[ i - 1 ] = i2 * i2 * i;
        } // for i

        for( int a = 0; a < MAX_NUMBER && ! found ; a ++ )
        {
            for( int b = a; b < MAX_NUMBER && ! found ; b ++ )
            {
                for( int c = b; c < MAX_NUMBER && ! found ; c ++ )
                {
                    for( int d = c; d < MAX_NUMBER && ! found ; d ++ )
                    {
                        long sum  = fifth[a] + fifth[b] + fifth[c] + fifth[d];
                        int  e = java.util.Arrays.binarySearch( fifth, sum );
                        found  = ( e >= 0 );
                        if( found )
                        {
                            // the value at e is a fifth power
                            System.out.print( (a+1) + "^5 + "
                                            + (b+1) + "^5 + "
                                            + (c+1) + "^5 + "
                                            + (d+1) + "^5 = "
                                            + (e+1) + "^5"
                                            );
                        } // if found;;
                    } // for d
                } // for c
            } // for b
        } // for a
    } // main

} // eulerSopConjecture
```

Output:

```txt

27^5 + 84^5 + 110^5 + 133^5 = 144^5

```



## JavaScript


### ES5


```javascript
var eulers_sum_of_powers = function (iMaxN) {

    var aPow5 = [];
    var oPow5ToN = {};

    for (var iP = 0; iP <= iMaxN; iP++) {
        var iPow5 = Math.pow(iP, 5);
        aPow5.push(iPow5);
        oPow5ToN[iPow5] = iP;
    }

    for (var i0 = 1; i0 <= iMaxN; i0++) {
        for (var i1 = 1; i1 <= i0; i1++) {
            for (var i2 = 1; i2 <= i1; i2++) {
                for (var i3 = 1; i3 <= i2; i3++) {
                    var iPow5Sum = aPow5[i0] + aPow5[i1] + aPow5[i2] + aPow5[i3];
                    if (typeof oPow5ToN[iPow5Sum] != 'undefined') {
                        return {
                            i0: i0,
                            i1: i1,
                            i2: i2,
                            i3: i3,
                            iSum: oPow5ToN[iPow5Sum]
                        };
                    }
                }
            }
        }
    }

};

var oResult = eulers_sum_of_powers(250);

console.log(oResult.i0 + '^5 + ' + oResult.i1 + '^5 + ' + oResult.i2 +
    '^5 + ' + oResult.i3 + '^5 = ' + oResult.iSum + '^5');
```


 133^5 + 110^5 + 84^5 + 27^5 = 144^5
'''This'''{{trans|D}} that verify:  a^5 + b^5 + c^5 + d^5 = x^5

```javascript
var N=1000, first=false
var ns={}, npv=[]
for (var n=0; n<=N; n++) {
	var np=Math.pow(n,5); ns[np]=n; npv.push(np)
}
loop:
for (var a=1;   a<=N; a+=1)
for (var b=a+1; b<=N; b+=1)
for (var c=b+1; c<=N; c+=1)
for (var d=c+1; d<=N; d+=1) {
	var x = ns[ npv[a]+npv[b]+npv[c]+npv[d] ]
	if (!x) continue
	print( [a, b, c, d, x] )
	if (first) break loop
}
function print(c) {
	var e='<sup>5</sup>', ep=e+' + '
	document.write(c[0], ep, c[1], ep, c[2], ep, c[3], e, ' = ', c[4], e, '
')
}
```

'''Or this'''{{trans|C}} that verify:  a^5 + b^5 + c^5 + d^5 = x^5

```javascript
var N=1000, first=false
var npv=[], M=30 // x^5 == x modulo M (=2*3*5)
for (var n=0; n<=N; n+=1) npv[n]=Math.pow(n, 5)
var mx=1+npv[N]; while(n<=N+M) npv[n++]=mx

loop:
for (var a=1;   a<=N; a+=1)
for (var b=a+1; b<=N; b+=1)
for (var c=b+1; c<=N; c+=1)
for (var t=npv[a]+npv[b]+npv[c], d=c+1, x=t%M+d; (n=t+npv[d])<mx; d+=1, x+=1) {
	while (npv[x]<=n) x+=M; x-=M // jump over M=30 values for x>d
	if (npv[x] != n) continue
	print( [a, b, c, d, x] )
	if (first) break loop;
}
function print(c) {
	var e='<sup>5</sup>', ep=e+' + '
	document.write(c[0], ep, c[1], ep, c[2], ep, c[3], e, ' = ', c[4], e, '
')
}
```

'''Or this'''{{trans|EchoLisp}} that verify:  a^5 + b^5 + c^5 = x^5 - d^5

```javascript
var N=1000, first=false
var dxs={}, pow=Math.pow
for (var d=1; d<=N; d+=1)
	for (var dp=pow(d,5), x=d+1; x<=N; x+=1)
		dxs[pow(x,5)-dp]=[d,x]
loop:
for (var a=1; a<N; a+=1)
for (var ap=pow(a,5), b=a+1; b<N; b+=1)
for (var abp=ap+pow(b,5), c=b+1; c<N; c+=1) {
	var dx = dxs[ abp+pow(c,5) ]
	if (!dx || c >= dx[0]) continue
	print( [a, b, c].concat( dx ) )
	if (first) break loop
}
function print(c) {
	var e='<sup>5</sup>', ep=e+' + '
	document.write(c[0], ep, c[1], ep, c[2], ep, c[3], e, ' = ', c[4], e, '
')
}
```

'''Or this'''{{trans|Python}}  that verify:  a^5 + b^5 = x^5 - (c^5 + d^5)

```javascript
var N=1000, first=false
var is={}, ipv=[], ijs={}, ijpv=[], pow=Math.pow
for (var i=1; i<=N; i+=1) {
	var ip=pow(i,5); is[ip]=i; ipv.push(ip)
	for (var j=i+1; j<=N; j+=1) {
		var ijp=ip+pow(j,5); ijs[ijp]=[i,j]; ijpv.push(ijp)
	}
}
ijpv.sort( function (a,b) {return a - b } )
loop:
for (var i=0, ei=ipv.length; i<ei; i+=1)
for (var xp=ipv[i], j=0, je=ijpv.length; j<je; j+=1) {
	var cdp = ijpv[j]
	if (cdp >= xp) break
	var cd = ijs[xp-cdp]
	if (!cd) continue
	var ab = ijs[cdp]
	if (ab[1] >= cd[0]) continue
	print( [].concat(ab, cd, is[xp]) )
	if (first) break loop
}
function print(c) {
	var e='<sup>5</sup>', ep=e+' + '
	document.write(c[0], ep, c[1], ep, c[2], ep, c[3], e, ' = ', c[4], e, '
')
}
```

  27<sup>5</sup> + 84<sup>5</sup> + 110<sup>5</sup> + 133 = 144<sup>5</sup>
  54<sup>5</sup> + 168<sup>5</sup> + 220<sup>5</sup> + 266 = 288<sup>5</sup>
  81<sup>5</sup> + 252<sup>5</sup> + 330<sup>5</sup> + 399 = 432<sup>5</sup>
  108<sup>5</sup> + 336<sup>5</sup> + 440<sup>5</sup> + 532 = 576<sup>5</sup>
  135<sup>5</sup> + 420<sup>5</sup> + 550<sup>5</sup> + 665 = 720<sup>5</sup>
  162<sup>5</sup> + 504<sup>5</sup> + 660<sup>5</sup> + 798 = 864<sup>5</sup>


### ES6


### =Procedural=


```JavaScript
(() => {
    'use strict';

    const eulersSumOfPowers = intMax => {
        const
            pow = Math.pow,
            xs = range(0, intMax)
            .map(x => pow(x, 5)),
            dct = xs.reduce((a, x, i) =>
                (a[x] = i,
                    a
                ), {});

        for (let a = 1; a <= intMax; a++) {
            for (let b = 2; b <= a; b++) {
                for (let c = 3; c <= b; c++) {
                    for (let d = 4; d <= c; d++) {
                        const sumOfPower = dct[xs[a] + xs[b] + xs[c] + xs[d]];
                        if (sumOfPower !== undefined) {
                            return [a, b, c, d, sumOfPower];
                        }
                    }
                }
            }
        }
        return undefined;
    };

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // TEST
    const soln = eulersSumOfPowers(250);
    return soln ? soln.slice(0, 4)
        .map(x => `${x}^5`)
        .join(' + ') + ` = ${soln[4]}^5` : 'No solution found.'

})();
```

```txt
133^5 + 110^5 + 84^5 + 27^5 = 144^5
```



### =Functional=

Using dictionaries of powers and sums, and a little faster than the procedural version above:
```javascript
(() => {
    'use strict';

    const main = () => {

        const
            iFrom = 1,
            iTo = 249,
            xs = enumFromTo(1, 249),
            p5 = x => Math.pow(x, 5);

        const
            // powerMap :: Dict Int Int
            powerMap = mapFromList(
                zip(map(p5, xs), xs)
            ),
            // sumMap :: Dict Int (Int, Int)
            sumMap = mapFromList(
                bind(
                    xs,
                    x => bind(
                        tail(xs),
                        y => Tuple(
                            p5(x) + p5(y),
                            Tuple(x, y)
                        )
                    )
                )
            );

        // mbExample :: Maybe (Int, Int)
        const mbExample = find(
            tpl => member(fst(tpl) - snd(tpl), sumMap),
            bind(
                map(x => parseInt(x, 10),
                    keys(powerMap)
                ),
                p => bind(
                    takeWhile(
                        x => x < p,
                        map(x => parseInt(x, 10),
                            keys(sumMap)
                        )
                    ),
                    s => [Tuple(p, s)]
                )
            )
        );

        // showExample :: (Int, Int) -> String
        const showExample = tpl => {
            const [p, s] = Array.from(tpl);
            const [a, b] = Array.from(sumMap[p - s]);
            const [c, d] = Array.from(sumMap[s]);
            return 'Counter-example found:\n' + intercalate(
                '^5 + ',
                map(str, [a, b, c, d])
            ) + '^5 = ' + str(powerMap[p]) + '^5';
        };

        return maybe(
            'No counter-example found',
            showExample,
            mbExample
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Just :: a -> Maybe a
    const Just = x => ({
        type: 'Maybe',
        Nothing: false,
        Just: x
    });

    // Nothing :: Maybe a
    const Nothing = () => ({
        type: 'Maybe',
        Nothing: true,
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // bind (>>=) :: [a] -> (a -> [b]) -> [b]
    const bind = (xs, mf) => [].concat.apply([], xs.map(mf));

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];


    // enumFromTo :: (Int, Int) -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // find :: (a -> Bool) -> [a] -> Maybe a
    const find = (p, xs) => {
        for (let i = 0, lng = xs.length; i < lng; i++) {
            if (p(xs[i])) return Just(xs[i]);
        }
        return Nothing();
    };

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // intercalate :: [a] -> [[a]] -> [a]
    // intercalate :: String -> [String] -> String
    const intercalate = (sep, xs) =>
        0 < xs.length && 'string' === typeof sep &&
        'string' === typeof xs[0] ? (
            xs.join(sep)
        ) : concat(intersperse(sep, xs));

    // intersperse(0, [1,2,3]) -> [1, 0, 2, 0, 3]

    // intersperse :: a -> [a] -> [a]
    // intersperse :: Char -> String -> String
    const intersperse = (sep, xs) => {
        const bln = 'string' === typeof xs;
        return xs.length > 1 ? (
            (bln ? concat : x => x)(
                (bln ? (
                    xs.split('')
                ) : xs)
                .slice(1)
                .reduce((a, x) => a.concat([sep, x]), [xs[0]])
            )) : xs;
    };

    // keys :: Dict -> [String]
    const keys = Object.keys;

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

    // mapFromList :: [(k, v)] -> Dict
    const mapFromList = kvs =>
        kvs.reduce(
            (a, kv) => {
                const k = kv[0];
                return Object.assign(a, {
                    [
                        (('string' === typeof k) && k) || JSON.stringify(k)
                    ]: kv[1]
                });
            }, {}
        );

    // Default value (v) if m.Nothing, or f(m.Just)

    // maybe :: b -> (a -> b) -> Maybe a -> b
    const maybe = (v, f, m) =>
        m.Nothing ? v : f(m.Just);

    // member :: Key -> Dict -> Bool
    const member = (k, dct) => k in dct;

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // str :: a -> String
    const str = x => x.toString();

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

    // takeWhile :: (a -> Bool) -> [a] -> [a]
    // takeWhile :: (Char -> Bool) -> String -> String
    const takeWhile = (p, xs) =>
        xs.constructor.constructor.name !==
        'GeneratorFunction' ? (() => {
            const lng = xs.length;
            return 0 < lng ? xs.slice(
                0,
                until(
                    i => lng === i || !p(xs[i]),
                    i => 1 + i,
                    0
                )
            ) : [];
        })() : takeWhileGen(p, xs);


    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // Use of `take` and `length` here allows for zipping with non-finite
    // lists - i.e. generators like cycle, repeat, iterate.

    // zip :: [a] -> [b] -> [(a, b)]
    const zip = (xs, ys) => {
        const lng = Math.min(length(xs), length(ys));
        return Infinity !== lng ? (() => {
            const bs = take(lng, ys);
            return take(lng, xs).map((x, i) => Tuple(x, bs[i]));
        })() : zipGen(xs, ys);
    };

    // MAIN ---
    return main();
})();
```

```txt
Counter-example found:
133^5 + 110^5 + 84^5 + 27^5 = 144^5
```



## jq

This version finds all non-decreasing solutions within the specified bounds,
using a brute-force but not entirely blind approach.

```jq
# Search for y in 1 .. maxn (inclusive) for a solution to SIGMA (xi ^ 5) = y^5
# and for each solution with x0<=x1<=...<x3, print [x0, x1, x3, x3, y]
#
def sum_of_powers_conjecture(maxn):
  def p5: . as $in | (.*.) | ((.*.) * $in);
  def fifth: log / 5 | exp;

  # return the fifth root if . is a power of 5
  def integral_fifth_root: fifth | if . == floor then . else false end;

  (maxn | p5) as $uber
  | range(1; maxn) as $x0
  | ($x0 | p5) as $s0
  | if $s0 < $uber then range($x0; ($uber - $s0 | fifth) + 1) as $x1
    | ($s0 + ($x1 | p5)) as $s1
    | if $s1 < $uber then range($x1; ($uber - $s1 | fifth) + 1) as $x2
      | ($s1 + ($x2 | p5)) as $s2
        | if $s2 < $uber then range($x2; ($uber - $s2 | fifth) + 1) as $x3
          | ($s2 + ($x3 | p5)) as $sumx
	  | ($sumx | integral_fifth_root)
	  | if . then [$x0,$x1,$x2,$x3,.] else empty end
	  else empty
	  end
      else empty
      end
    else empty
    end ;
```

'''The task:'''

```jq
sum_of_powers_conjecture(249)
```

```sh
$ jq -c -n -f Euler_sum_of_powers_conjecture_fifth_root.jq
[27,84,110,133,144]
```



## Julia


```Julia

const lim = 250
const pwr = 5
const p = [i^pwr for i in 1:lim]

x = zeros(Int, pwr-1)
y = 0

for a in combinations(1:lim, pwr-1)
    b = searchsorted(p, sum(p[a]))
    0 < length(b) || continue
    x = a
    y = b[1]
    break
end

if y == 0
    println("No solution found for power = ", pwr, " and limit = ", lim, ".")
else
    s = [@sprintf("%d^%d", i, pwr) for i in x]
    s = join(s, " + ")
    println("A solution is ", s, " = ", @sprintf("%d^%d", y, pwr), ".")
end

```


```txt

A solution is 27^5 + 84^5 + 110^5 + 133^5 = 144^5.

```



## Kotlin


```scala
fun main(args: Array<String>) {
    val p5 = LongArray(250){ it.toLong() * it * it * it * it }
    var sum: Long
    var y: Int
    var found = false
    loop@ for (x0 in 0 .. 249)
        for (x1 in 0 .. x0 - 1)
            for (x2 in 0 .. x1 - 1)
                for (x3 in 0 .. x2 - 1) {
                    sum = p5[x0] + p5[x1] + p5[x2] + p5[x3]
                    y = p5.binarySearch(sum)
                    if (y >= 0) {
                        println("$x0^5 + $x1^5 + $x2^5 + $x3^5 = $y^5")
                        found = true
                        break@loop
                    }
                }
    if (!found) println("No solution was found")
}
```


```txt

133^5 + 110^5 + 84^5 + 27^5 = 144^5

```



## Lua

Brute force but still takes under two seconds with LuaJIT.

```Lua
-- Fast table search (only works if table values are in order)
function binarySearch (t, n)
    local start, stop, mid = 1, #t
    while start < stop do
        mid = math.floor((start + stop) / 2)
        if n == t[mid] then
            return mid
        elseif n < t[mid] then
            stop = mid - 1
        else
            start = mid + 1
        end
    end
    return nil
end

-- Test Euler's sum of powers conjecture
function euler (limit)
    local pow5, sum = {}
    for i = 1, limit do pow5[i] = i^5 end
    for x0 = 1, limit do
        for x1 = 1, x0 do
            for x2 = 1, x1 do
                for x3 = 1, x2 do
                    sum = pow5[x0] + pow5[x1] + pow5[x2] + pow5[x3]
                    if binarySearch(pow5, sum) then
                        print(x0 .. "^5 + " .. x1 .. "^5 + " .. x2 .. "^5 + " .. x3 .. "^5 = " .. sum^(1/5) .. "^5")
                        return true
                    end
                end
            end
        end
    end
    return false
end

-- Main procedure
if euler(249) then
    print("Time taken: " .. os.clock() .. " seconds")
else
    print("Looks like he was right after all...")
end
```

```txt
133^5 + 110^5 + 84^5 + 27^5 = 144^5
Time taken: 1.247 seconds
```



## Mathematica


```Mathematica
Sort[FindInstance[
   x0^5 + x1^5 + x2^5 + x3^5 == y^5 && x0 > 0 && x1 > 0 && x2 > 0 &&
    x3 > 0, {x0, x1, x2, x3, y}, Integers][[1, All, -1]]]
```

```txt

{27,84,110,133,144}
```



## Microsoft Small Basic


```smallbasic
' Euler sum of powers conjecture - 03/07/2015
  'find: x1^5+x2^5+x3^5+x4^5=x5^5
  '-> x1=27 x2=84 x3=110 x4=133 x5=144
  maxn=250
  For i=1 to maxn
	p5[i]=Math.Power(i,5)
  EndFor
  For x1=1 to maxn-4
    For x2=x1+1 to maxn-3
      'TextWindow.WriteLine("x1="+x1+", x2="+x2)
      For x3=x2+1 to maxn-2
        'TextWindow.WriteLine("x1="+x1+", x2="+x2+", x3="+x3)
        For x4=x3+1 to maxn-1
          'TextWindow.WriteLine("x1="+x1+", x2="+x2+", x3="+x3+", x4="+x4)
          x5=x4+1
          valx=p5[x5]
          sumx=p5[x1]+p5[x2]+p5[x3]+p5[x4]
          While x5<=maxn and valx<=sumx
            If valx=sumx Then
              TextWindow.WriteLine("Found!")
              TextWindow.WriteLine("-> "+x1+"^5+"+x2+"^5+"+x3+"^5+"+x4+"^5="+x5+"^5")
              TextWindow.WriteLine("x5^5="+sumx)
              Goto EndPgrm
            EndIf
            x5=x5+1
            valx=p5[x5]
          EndWhile 'x5
        EndFor 'x4
      EndFor 'x3
    EndFor 'x2
  EndFor 'x1
 EndPgrm:
```

```txt
Found!
-> 27^5+84^5+110^5+133^5=144^5
x5^5=61917364224
```


=={{header|Modula-2}}==
```modula2
MODULE EulerConjecture;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE Pow5(a : LONGINT) : LONGINT;
BEGIN
    RETURN a * a * a * a * a
END Pow5;

VAR
    buf : ARRAY[0..63] OF CHAR;
    a,b,c,d,e,sum,curr : LONGINT;
BEGIN
    FOR a:=0 TO 250 DO
        FOR b:=a TO 250 DO
            IF b=a THEN CONTINUE END;
            FOR c:=b TO 250 DO
                IF (c=a) OR (c=b) THEN CONTINUE END;
                FOR d:=c TO 250 DO
                    IF (d=a) OR (d=b) OR (d=c) THEN CONTINUE END;
                    sum := Pow5(a) + Pow5(b) + Pow5(c) + Pow5(d);
                    FOR e:=d TO 250 DO
                        IF (e=a) OR (e=b) OR (e=c) OR (e=d) THEN CONTINUE END;
                        curr := Pow5(e);
                        IF (sum#0) AND (sum=curr) THEN
                            FormatString("%l^5 + %l^5 + %l^5 + %l^5 = %l^5\n", buf, a, b, c, d, e);
                            WriteString(buf)
                        ELSIF curr > sum THEN
                            BREAK
                        END
                    END;
                END;
            END;
        END;
    END;

    WriteString("Done");
    WriteLn;
    ReadChar
END EulerConjecture.
```



## Nim

```Nim

# Brute force approach

import times

# assumes an array of non-decreasing positive integers
proc binarySearch(a : openArray[int], target : int) : int =
  var left, right, mid : int
  left = 0
  right = len(a) - 1
  while true :
    if left > right : return 0  # no match found
    mid = (left + right) div 2
    if a[mid] < target :
      left = mid + 1
    elif a[mid] > target :
      right = mid - 1
    else :
      return mid  # match found

var
  p5 : array[250, int]
  sum = 0
  y, t1 : int

let t0 = cpuTime()

for i in 1 .. 249 :
  p5[i] = i * i * i * i * i

for x0 in 1 .. 249 :
  for x1 in 1 .. x0 - 1 :
    for x2 in 1 .. x1 - 1 :
      for x3 in 1 .. x2 - 1 :
        sum = p5[x0] + p5[x1] + p5[x2] + p5[x3]
        y = binarySearch(p5, sum)
        if y > 0 :
          t1 = int((cputime() - t0) * 1000.0)
          echo "Time : ", t1, " milliseconds"
          echo  $x0 & "^5 + " & $x1 & "^5 + " & $x2 & "^5 + " & $x3 & "^5 = " & $y & "^5"
          quit()

if y == 0 :
  echo "No solution was found"

```


```txt

Time : 156 milliseconds
133^5 + 110^5 + 84^5 + 27^5 = 144^5

```



## Oforth



```Oforth
: eulerSum
| i j k l ip jp kp |
   250 loop: i [
      i 5 pow ->ip
      i 1 + 250 for: j [
         j 5 pow ip + ->jp
         j 1 + 250 for: k [
            k 5 pow jp + ->kp
            k 1 + 250 for: l [
               kp l 5 pow + 0.2 powf dup asInteger == ifTrue: [ [ i, j, k, l ] println ]
              ]
            ]
         ]
      ] ;
```


```txt

>eulerSum
[27, 84, 110, 133]

```



## PARI/GP

Naive script:

```parigp
forvec(v=vector(4,i,[0,250]), if(ispower(v[1]^5+v[2]^5+v[3]^5+v[4]^5,5,&n), print(n" "v)), 2)
```

```txt
144 [27, 84, 110, 133]
```


Naive + caching (<code>setbinop</code>):

```parigp
{
v2=setbinop((x,y)->[min(x,y),max(x,y),x^5+y^5],[0..250]); \\ sums of two fifth powers
for(i=2,#v2,
  for(j=1,i-1,
    if(v2[i][2]<v2[j][2] && ispower(v2[i][3]+v2[j][3],5,&n) && #(v=Set([v2[i][1],v2[i][2],v2[j][1],v2[j][2]]))==4,
      print(n" "v)
    )
  )
)
}
```

```txt
144 [27, 84, 110, 133]
```



## Pascal

slightly improved.Reducing calculation time by temporary sum and early break.

```pascal
program Pot5Test;
{$IFDEF FPC} {$MODE DELPHI}{$ELSE]{$APPTYPE CONSOLE}{$ENDIF}
type
  tTest = double;//UInt64;{ On linux 32Bit double is faster than  Uint64 }
var
  Pot5 : array[0..255] of tTest;
  res,tmpSum : tTest;
  x0,x1,x2,x3, y : NativeUint;//= Uint32 or 64 depending on OS xx-Bit
  i : byte;
BEGIN
  For i := 1 to 255 do
    Pot5[i] := (i*i*i*i)*Uint64(i);

  For x0 := 1 to 250-3 do
    For x1 := x0+1 to 250-2 do
      For x2 := x1+1 to 250-1 do
      Begin
        //set y here only, because pot5 is strong monoton growing,
        //therefor the sum is strong monoton growing too.
        y := x2+2;// aka x3+1
        tmpSum := Pot5[x0]+Pot5[x1]+Pot5[x2];
        For x3 := x2+1 to 250 do
        Begin
          res := tmpSum+Pot5[x3];
          while (y< 250) AND (res > Pot5[y]) do
            inc(y);
          IF y > 250 then BREAK;
          if res = Pot5[y] then
            writeln(x0,'^5+',x1,'^5+',x2,'^5+',x3,'^5 = ',y,'^5');
        end;
      end;
END.

```

;output:

```txt

27^5+84^5+110^5+133^5 = 144^5
real  0m1.091s {Uint64; Linux 32}real  0m0.761s {double; Linux 32}real  0m0.511s{Uint64; Linux 64}

```



## Perl

Brute Force:

```perl>use constant MAX =
 250;
my @p5 = (0,map { $_**5 } 1 .. MAX-1);
my $s = 0;
my %p5 = map { $_ => $s++ } @p5;
for my $x0 (1..MAX-1) {
  for my $x1 (1..$x0-1) {
    for my $x2 (1..$x1-1) {
      for my $x3 (1..$x2-1) {
        my $sum = $p5[$x0] + $p5[$x1] + $p5[$x2] + $p5[$x3];
        die "$x3 $x2 $x1 $x0 $p5{$sum}\n" if exists $p5{$sum};
      }
    }
  }
}
```

```txt
27 84 110 133 144
```


Adding some optimizations makes it 5x faster with similar output, but obfuscates things.
```perl>use constant MAX =
 250;
my @p5 = (0,map { $_**5 } 1 .. MAX-1);
my $rs = 5;
for my $x0 (1..MAX-1) {
  for my $x1 (1..$x0-1) {
    for my $x2 (1..$x1-1) {
      my $s2 = $p5[$x0] + $p5[$x1] + $p5[$x2];
      $rs-- while $rs > 0 && $p5[$rs] > $s2;
      for (my $x3 = 1;  $x3 < $x2;  $x3++) {
        my $e30 = ($x0 + $x1 + $x2 + $x3 - $rs) % 30;
        $x3 += (30-$e30) if $e30;
        last if $x3 >= $x2;
        my $sum = $s2 + $p5[$x3];
        $rs++ while $rs < MAX-1 && $p5[$rs] < $sum;
        die "$x3 $x2 $x1 $x0 $rs\n" if $p5[$rs] == $sum;
      }
    }
  }
}
```



## Perl 6

```perl6
constant MAX = 250;

my %po5{Int};
my %sum2{Int};

(1..MAX).map: -> $i {
    %po5{$i**5} = $i;
    for 1..MAX -> $j {
        %sum2{$i**5 + $j**5} = ($i, $j);
    }
}

my @sk = %sum2.keys.sort;
%po5.keys.sort.race.map: -> $p {
    for @sk -> $s {
        next if $p <= $s;
        if %sum2{$p - $s} {
            say ((sort |%sum2{$s}[],|%sum2{$p-$s}[]) X~ '⁵').join(' + ') ~ " =  %po5{$p}" ~ "⁵";
            exit;
        }
    }
}
```

```txt
27⁵ + 84⁵ + 110⁵ + 133⁵ =  144⁵
```



## Phix

Around four seconds, not spectacularly fast. My naive brute force was over a minute. This is not where Phix shines.

Quitting when the first is found drops the main loop to 0.7s, so 1.1s in all, vs 4.3s for the full search.

Without the return 0, you just get six permutes (of ordered pairs) for 144.

```Phix
constant MAX = 250

constant p5 = new_dict(),
         sum2 = new_dict()

atom t0 = time()
for i=1 to MAX do
    atom i5 = power(i,5)
    setd(i5,i,p5)
    for j=1 to i-1 do
        atom j5 = power(j,5)
        setd(j5+i5,{j,i},sum2)
    end for
end for

?time()-t0

function forsum2(object s, object data, object p)
    if p<=s then return 0 end if
    integer k = getd_index(p-s,sum2)
    if k!=NULL then
        ?getd(p,p5)&data&getd_by_index(k,sum2)
        return 0 -- (show one solution per p)
    end if
    return 1
end function

function forp5(object key, object /*data*/, object /*user_data*/)
    traverse_dict(routine_id("forsum2"),key,sum2)
    return 1
end function

traverse_dict(routine_id("forp5"),0,p5)

?time()-t0
```

```txt

0.421
{144,27,84,110,133}
4.312

```



## PHP

```php
<?php

function eulers_sum_of_powers () {
	$max_n = 250;
	$pow_5 = array();
	$pow_5_to_n = array();
	for ($p = 1; $p <= $max_n; $p ++) {
		$pow5 = pow($p, 5);
		$pow_5 [$p] = $pow5;
		$pow_5_to_n[$pow5] = $p;
	}
	foreach ($pow_5 as $n_0 => $p_0) {
		foreach ($pow_5 as $n_1 => $p_1) {
			if ($n_0 < $n_1) continue;
			foreach ($pow_5 as $n_2 => $p_2) {
				if ($n_1 < $n_2) continue;
				foreach ($pow_5 as $n_3 => $p_3) {
					if ($n_2 < $n_3) continue;
					$pow_5_sum = $p_0 + $p_1 + $p_2 + $p_3;
					if (isset($pow_5_to_n[$pow_5_sum])) {
						return array($n_0, $n_1, $n_2, $n_3, $pow_5_to_n[$pow_5_sum]);
					}
				}
			}
		}
	}
}

list($n_0, $n_1, $n_2, $n_3, $y) = eulers_sum_of_powers();

echo "$n_0^5 + $n_1^5 + $n_2^5 + $n_3^5 = $y^5";

?>
```


```txt
133^5 + 110^5 + 84^5 + 27^5 = 144^5
```



## PicoLisp


```PicoLisp
(off P)
(off S)

(for I 250
   (idx
      'P
      (list (setq @@ (** I 5)) I)
      T )
   (for (J I (>= 250 J) (inc J))
      (idx
         'S
         (list (+ @@ (** J 5)) (list I J))
         T ) ) )
(println
   (catch 'found
      (for A (idx 'P)
         (for B (idx 'S)
            (T (<= (car A) (car B)))
            (and
               (lup S (- (car A) (car B)))
               (throw 'found
                  (conc
                     (cadr (lup S (car B)))
                     (cadr (lup S (- (car A) (car B))))
                     (cdr (lup P (car A))) ) ) ) ) ) ) )
```

```txt
(27 84 110 133 144)
```



## PowerShell

'''Brute Force Search'''

This is a slow algorithm, so attempts have been made to speed it up, including pre-computing the powers, using an ArrayList for them, and using [int] to cast the 5th root rather than use truncate.

```powershell
# EULER.PS1
$max = 250

$powers =  New-Object System.Collections.ArrayList
for ($i = 0; $i -lt $max; $i++) {
  $tmp = $powers.Add([Math]::Pow($i, 5))
}

for ($x0 = 1; $x0 -lt $max; $x0++) {
  for ($x1 = 1; $x1 -lt $x0; $x1++) {
    for ($x2 = 1; $x2 -lt $x1; $x2++) {
      for ($x3 = 1; $x3 -lt $x2; $x3++) {
        $sum = $powers[$x0] + $powers[$x1] + $powers[$x2] + $powers[$x3]
        $S1 = [int][Math]::pow($sum,0.2)

        if ($sum -eq $powers[$S1]) {
          Write-host "$x0^5 + $x1^5 + $x2^5 + $x3^5 = $S1^5"
          return
        }
      }
    }
  }
}
```

```txt
PS > measure-command { .\euler.ps1 | out-default }
133^5 + 110^5 + 84^5 + 27^5 = 144^5


Days              : 0
Hours             : 0
Minutes           : 0
Seconds           : 31
Milliseconds      : 608
Ticks             : 316082251
TotalDays         : 0.000365835938657407
```



## Prolog


```prolog

makepowers :-
    retractall(pow5(_, _)),
    between(1, 249, X),
    Y is X * X * X * X * X,
    assert(pow5(X, Y)),
    fail.
makepowers.

within(A, Bx, N) :-  % like between but with an exclusive upper bound
   succ(B, Bx),
   between(A, B, N).

solution(X0, X1, X2, X3, Y) :-
    makepowers,
    within(4, 250, X0), pow5(X0, X0_5th),
    within(3, X0,  X1), pow5(X1, X1_5th),
    within(2, X1,  X2), pow5(X2, X2_5th),
    within(1, X2,  X3), pow5(X3, X3_5th),
    Y_5th is X0_5th + X1_5th + X2_5th + X3_5th,
    pow5(Y, Y_5th).

```

```txt

?- solution(X0,X1,X2,X3,Y).
X0 = 133,
X1 = 110,
X2 = 84,
X3 = 27,
Y = 144 .

```


## PureBasic


```PureBasic

EnableExplicit

; assumes an array of non-decreasing positive integers
Procedure.q BinarySearch(Array a.q(1), Target.q)
  Protected l = 0, r = ArraySize(a()), m
  Repeat
    If l > r : ProcedureReturn 0 : EndIf; no match found
    m = (l + r) / 2
    If a(m) < target
      l = m + 1
    ElseIf a(m) > target
      r = m - 1
    Else
      ProcedureReturn m ; match found
    EndIf
  ForEver
EndProcedure

Define i, x0, x1, x2, x3, y
Define.q sum
Define Dim p5.q(249)

For i = 1 To 249
  p5(i) = i * i * i * i * i
Next

If OpenConsole()
  For x0 = 1 To 249
    For x1 = 1 To x0 - 1
      For x2 = 1 To x1 - 1
        For x3 = 1 To x2 - 1
          sum = p5(x0) + p5(x1) + p5(x2) + p5(x3)
          y = BinarySearch(p5(), sum)
          If y > 0
            PrintN(Str(x0) + "^5 + " + Str(x1) + "^5 + " + Str(x2) + "^5 + " + Str(x3) + "^5 = " + Str(y) + "^5")
            Goto finish
          EndIf
        Next x3
      Next x2
    Next x1
  Next x0

  PrintN("No solution was found")
  finish:
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


```txt

133^5 + 110^5 + 84^5 + 27^5 = 144^5

```



## Python


### Procedural


```python
def eulers_sum_of_powers():
    max_n = 250
    pow_5 = [n**5 for n in range(max_n)]
    pow5_to_n = {n**5: n for n in range(max_n)}
    for x0 in range(1, max_n):
        for x1 in range(1, x0):
            for x2 in range(1, x1):
                for x3 in range(1, x2):
                    pow_5_sum = sum(pow_5[i] for i in (x0, x1, x2, x3))
                    if pow_5_sum in pow5_to_n:
                        y = pow5_to_n[pow_5_sum]
                        return (x0, x1, x2, x3, y)

print("%i**5 + %i**5 + %i**5 + %i**5 == %i**5" % eulers_sum_of_powers())
```


```txt
133**5 + 110**5 + 84**5 + 27**5 == 144**5
```


The above can be written as:
```python
from itertools import combinations

def eulers_sum_of_powers():
    max_n = 250
    pow_5 = [n**5 for n in range(max_n)]
    pow5_to_n = {n**5: n for n in range(max_n)}
    for x0, x1, x2, x3 in combinations(range(1, max_n), 4):
        pow_5_sum = sum(pow_5[i] for i in (x0, x1, x2, x3))
        if pow_5_sum in pow5_to_n:
            y = pow5_to_n[pow_5_sum]
            return (x0, x1, x2, x3, y)

print("%i**5 + %i**5 + %i**5 + %i**5 == %i**5" % eulers_sum_of_powers())
```


```txt
27**5 + 84**5 + 110**5 + 133**5 == 144**5
```


It's much faster to cache and look up sums of two fifth powers, due to the small allowed range:

```python
MAX = 250
p5, sum2 = {}, {}

for i in range(1, MAX):
	p5[i**5] = i
	for j in range(i, MAX):
		sum2[i**5 + j**5] = (i, j)

sk = sorted(sum2.keys())
for p in sorted(p5.keys()):
	for s in sk:
		if p <= s: break
		if p - s in sum2:
			print(p5[p], sum2[s] + sum2[p-s])
			exit()
```

```txt
144 (27, 84, 110, 133)
```



### Composition of pure functions

```python
'''Euler's sum of powers conjecture'''

from itertools import (chain, takewhile)


# main :: IO ()
def main():
    '''Search for counter-example'''

    xs = enumFromTo(1)(249)

    powerMap = {x**5: x for x in xs}
    sumMap = {
        x**5 + y**5: (x, y)
        for x in xs[1:]
        for y in xs if x > y
    }

    # isExample :: (Int, Int) -> Bool
    def isExample(ps):
        p, s = ps
        return p - s in sumMap

    # display :: (Int, Int) -> String
    def display(ps):
        p, s = ps
        a, b = sumMap[p - s]
        c, d = sumMap[s]
        return '^5 + '.join([str(n) for n in [a, b, c, d]]) + (
            '^5 = ' + str(powerMap[p]) + '^5'
        )

    print(__doc__ + ' – counter-example:\n')
    print(
        maybe('No counter-example found.')(display)(
            find(isExample)(
                bind(powerMap.keys())(
                    lambda p: bind(
                        takewhile(
                            lambda x: p > x,
                            sumMap.keys()
                        )
                    )(lambda s: [(p, s)])
                )
            )
        )
    )


# GENERIC -------------------------------------------------


# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# bind (>>=) :: [a] -> (a -> [b]) -> [b]
def bind(xs):
    '''List monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.
    '''
    return lambda f: chain.from_iterable(map(f, xs))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: range(m, 1 + n)


# find :: (a -> Bool) -> [a] -> Maybe a
def find(p):
    '''Just the first element in the list that matches p,
       or Nothing if no elements match.'''
    def go(xs):
        for x in xs:
            if p(x):
                return Just(x)
        return Nothing()
    return lambda xs: go(xs)


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where m is Just(x).'''
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Euler's sum of powers conjecture – counter-example:

133^5 + 110^5 + 84^5 + 27^5 = 144^5
```



## Racket

```scheme
#lang racket
(define MAX 250)
(define pow5 (make-vector MAX))
(for ([i (in-range 1 MAX)])
  (vector-set! pow5 i (expt i 5)))
(define pow5s (list->set (vector->list pow5)))
(let/ec break
  (for* ([x0 (in-range 1 MAX)]
         [x1 (in-range 1 x0)]
         [x2 (in-range 1 x1)]
         [x3 (in-range 1 x2)])
    (define sum (+ (vector-ref pow5 x0)
                   (vector-ref pow5 x1)
                   (vector-ref pow5 x2)
                   (vector-ref pow5 x3)))
    (when (set-member? pow5s sum)
      (displayln (list x0 x1 x2 x3 (inexact->exact (round (expt sum 1/5)))))
      (break))))
```

```txt

(133 110 84 27 144)

```



## REXX

Programming note:   the 3<sup>rd</sup> argument can be specified which causes an attempt to find   '''N'''   solutions.

The starting and ending (low and high) values can also be specified   (to limit or expand the search range).

If any of the arguments are omitted, they default to the Rosetta Code task's specifications.

The method used is:

::*   precompute all powers of five   (within the confines of allowed integers)
::*   precompute all (positive) differences between <u>two</u> applicable 5<sup>th</sup> powers
::*   see if any of the sums of any three   5<sup>th</sup>   powers are equal to any of those (above) differences
::*           {thanks to the real nifty idea   (↑↑↑)   from user ID   '''G. Brougnard'''}
::*   see if the sum of any four   5<sup>th</sup>   powers is equal to   ''any''   5<sup>th</sup> power
::*           (this is needed as the fourth number   <big>'''d'''</big>   isn't known yet).
::*   {all of the above utilizes REXX's   ''sparse stemmed array hashing''   which eliminates the need for sorting.}

By implementing (user ID)   G. Brougnard's   idea of   ''differences of two 5<sup>th</sup> powers'',
the time used for computation was reduced by over a factor of seventy.


In essence, the new formula being solved is:       <big><big> aⁿ   +   bⁿ   +   cⁿ     ==     xⁿ   ─   dⁿ </big></big>

which lends itself to algorithm optimization by (only) having to:
:*   [the right side of the above equation]   pre-compute all possible differences between any two applicable
         integer powers of five   (there are 30,135 unique differences)
:*   [the   left side of the above equation]   sum any applicable three integer powers of five
:*   [the   <big><big>==</big></big>   part of the above equation]   see if any of the above left─side sums match any of the   ≈30k   right─side differences

```rexx
/*REXX program finds unique positive integers for ────────── aⁿ+bⁿ+cⁿ+dⁿ==xⁿ  where n=5 */
parse arg L H N .                                /*get optional  LOW, HIGH,  #solutions.*/
if L=='' | L==","  then L=   0  + 1              /*Not specified?  Then use the default.*/
if H=='' | H==","  then H= 250  - 1              /* "      "         "   "   "     "    */
if N=='' | N==","  then N=   1                   /* "      "         "   "   "     "    */
w= length(H)                                     /*W:  used for display aligned numbers.*/
say center(' 'subword(sourceLine(1), 9, 3)" ", 70 +5*w, '─')  /*show title from 1st line*/
numeric digits 1000                              /*be able to handle the next expression*/
numeric digits max(9, length(3*H**5) )           /* "   "   "    "   3* [H to 5th power]*/
bH= H - 2;                 cH= H - 1             /*calculate the upper  DO  loop limits.*/
!.= 0                                            /* [↓]  define values of  5th  powers. */
       do pow=1  for H;    @.pow= pow**5;     _= @.pow;        !._= 1;          $._= pow
       end   /*pow*/
?.= !.
       do    j=4   for H-3                       /*use the range of:   four  to   cH.   */
          do k=j+1  to H;  _= @.k - @.j;  ?._= 1 /*compute the   xⁿ - dⁿ    differences.*/
          end   /*k*/                            /* [↑]  diff. is always positive as k>j*/
       end      /*j*/                            /*define [↑]    5th  power differences.*/
#= 0                                             /*#:  is the number of solutions found.*/   /* [↓]  for N=∞ solutions.*/
    do       a=L    to H-3;     s0=      @.a     /*traipse through possible  A  values. */   /*◄──done       246 times.*/
      do     b=a+1  to bH;      s1= s0 + @.b     /*   "       "        "     B    "     */   /*◄──done    30,381 times.*/
        do   c=b+1  to cH;      s2= s1 + @.c     /*   "       "        "     C    "     */   /*◄──done 2,511,496 times.*/
        if ?.s2  then do d=c+1  to H;  s= s2+@.d /*find the appropriate solution.       */
                      if !.s  then call results  /*Is it a solution?   Then display it. */
                      end   /*d*/                /* [↑]    !.S  is a boolean.           */
        end                 /*c*/
      end                   /*b*/
    end                     /*a*/

if #==0  then say "Didn't find a solution.";           exit 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
results: _= left('', 5);     #= # + 1            /*_:  used as a spacer; bump # counter.*/
         say _  'solution'   right(#,length(N))":"  _  'a='right(a,w)   _  "b="right(b,w),
             _  'c='right(c,w)     _  "d="right(d,w)    _   'x='right($.s,w+1)
         if #<N  then return                     /*return, keep searching for more sols.*/
         exit #                                  /*stick a fork in it,  we're all done. */
```

```txt

──────────────────────────── aⁿ+bⁿ+cⁿ+dⁿ==xⁿ  where ⁿ=5 ─────────────────────────────
      solution 1:       a= 27       b= 84       c=110       d=133       x= 144

```

```txt

─────────────────────────────── aⁿ+bⁿ+cⁿ+dⁿ==xⁿ  where ⁿ=5 ───────────────────────────────
      solution   1:       a=  27       b=  84       c= 110       d= 133       x=  144
      solution   2:       a=  54       b= 168       c= 220       d= 266       x=  288
      solution   3:       a=  81       b= 252       c= 330       d= 399       x=  432
      solution   4:       a= 108       b= 336       c= 440       d= 532       x=  576
      solution   5:       a= 135       b= 420       c= 550       d= 665       x=  720
      solution   6:       a= 162       b= 504       c= 660       d= 798       x=  864
      solution   7:       a= 189       b= 588       c= 770       d= 931       x= 1008
      solution   8:       a= 216       b= 672       c= 880       d=1064       x= 1152
      solution   9:       a= 243       b= 756       c= 990       d=1197       x= 1296
      solution  10:       a= 270       b= 840       c=1100       d=1330       x= 1440
      solution  11:       a= 297       b= 924       c=1210       d=1463       x= 1584
      solution  12:       a= 324       b=1008       c=1320       d=1596       x= 1728
      solution  13:       a= 351       b=1092       c=1430       d=1729       x= 1872
      solution  14:       a= 378       b=1176       c=1540       d=1862       x= 2016
      solution  15:       a= 405       b=1260       c=1650       d=1995       x= 2160
      solution  16:       a= 432       b=1344       c=1760       d=2128       x= 2304
      solution  17:       a= 459       b=1428       c=1870       d=2261       x= 2448
      solution  18:       a= 486       b=1512       c=1980       d=2394       x= 2592
      solution  19:       a= 513       b=1596       c=2090       d=2527       x= 2736
      solution  20:       a= 540       b=1680       c=2200       d=2660       x= 2880
      solution  21:       a= 567       b=1764       c=2310       d=2793       x= 3024
      solution  22:       a= 594       b=1848       c=2420       d=2926       x= 3168
      solution  23:       a= 621       b=1932       c=2530       d=3059       x= 3312
      solution  24:       a= 648       b=2016       c=2640       d=3192       x= 3456
      solution  25:       a= 675       b=2100       c=2750       d=3325       x= 3600
      solution  26:       a= 702       b=2184       c=2860       d=3458       x= 3744
      solution  27:       a= 729       b=2268       c=2970       d=3591       x= 3888

```



## Ring


```ring

# Project : Euler's sum of powers conjecture

max=250
for w = 1 to max
     for x = 1 to w
          for y = 1 to x
               for z = 1 to y
                    sum = pow(w,5) + pow(x,5) + pow(y,5) + pow(z,5)
                    s1  = floor(pow(sum,0.2))
                    if sum = pow(s1,5)
                       see "" + w + "^5 + " + x + "^5 + " + y + "^5 + " + z + "^5 = " + s1 + "^5"
                    ok
               next
          next
     next
next

```

Output:

```txt

133^5 + 110^5 + 84^5 + 27^5 = 144^5

```



## Ruby

Brute force:

```ruby
power5 = (1..250).each_with_object({}){|i,h| h[i**5]=i}
result = power5.keys.repeated_combination(4).select{|a| power5[a.inject(:+)]}
puts result.map{|a| a.map{|i| "#{power5[i]}**5"}.join(' + ') + " = #{power5[a.inject(:+)]}**5"}
```

```txt

27**5 + 84**5 + 110**5 + 133**5 = 144**5

```


Faster version:
```ruby
p5, sum2, max = {}, {}, 250
(1..max).each do |i|
  p5[i**5] = i
  (i..max).each{|j| sum2[i**5 + j**5] = [i,j]}
end

result = {}
sk = sum2.keys.sort
p5.keys.sort.each do |p|
  sk.each do |s|
    break if p <= s
    result[(sum2[s] + sum2[p-s]).sort] = p5[p]  if sum2[p - s]
  end
end
result.each{|k,v| puts k.map{|i| "#{i}**5"}.join(' + ') + " = #{v}**5"}
```

The output is the same above.


## Run BASIC


```runbasic

max=250
FOR w = 1 TO max
  FOR x = 1 TO w
    FOR y = 1 TO x
      FOR z = 1 TO y
      sum = w^5 + x^5 + y^5 + z^5
      s1  = INT(sum^0.2)
      IF sum=s1^5 THEN
        PRINT w;"^5 + ";x;"^5 + ";y;"^5 + ";z;"^5 = ";s1;"^5"
        end
      end if
      NEXT z
    NEXT y
  NEXT x
NEXT w
```


```txt

133^5 + 110^5 + 84^5 + 27^5 = 144^5

```



## Rust


```rust
const MAX_N : u64 = 250;

fn eulers_sum_of_powers() -> (usize, usize, usize, usize, usize) {
    let pow5: Vec<u64> = (0..MAX_N).map(|i| i.pow(5)).collect();
    let pow5_to_n = |pow| pow5.binary_search(&pow);

    for x0 in 1..MAX_N as usize {
        for x1 in 1..x0 {
            for x2 in 1..x1 {
                for x3 in 1..x2 {
                    let pow_sum = pow5[x0] + pow5[x1] + pow5[x2] + pow5[x3];
                    if let Ok(n) = pow5_to_n(pow_sum) {
                        return (x0, x1, x2, x3, n)
                    }
                }
            }
        }
    }

    panic!();
}

fn main() {
	let (x0, x1, x2, x3, y) = eulers_sum_of_powers();
	println!("{}^5 + {}^5 + {}^5 + {}^5 == {}^5", x0, x1, x2, x3, y)
}
```

```txt
133^5 + 110^5 + 84^5 + 27^5 == 144^5
```



## Scala


### Functional programming


```Scala
import scala.collection.Searching.{Found, search}

object EulerSopConjecture extends App {

  val (maxNumber, fifth) = (250, (1 to 250).map { i => math.pow(i, 5).toLong })

  def binSearch(fact: Int*) = fifth.search(fact.map(f => fifth(f)).sum)

  def sop = (0 until maxNumber)
    .flatMap(a => (a until maxNumber)
      .flatMap(b => (b until maxNumber)
        .flatMap(c => (c until maxNumber)
          .map { case x$1@d => (binSearch(a, b, c, d), x$1) }
          .withFilter { case (f, _) => f.isInstanceOf[Found] }
          .map { case (f, d) => (a + 1, b + 1, c + 1, d + 1, f.insertionPoint + 1) }))).take(1)
    .map { case (a, b, c, d, f) => s"$a⁵ + $b⁵ + $c⁵ + $d⁵ = $f⁵" }

  println(sop)

}
```

{{Out}}Vector(27⁵ + 84⁵ + 110⁵ + 133⁵ = 144⁵)


## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: binarySearch (in array integer: arr, in integer: aKey) is func
  result
    var integer: index is 0;
  local
    var integer: low is 1;
    var integer: high is 0;
    var integer: middle is 0;
  begin
    high := length(arr);
    while index = 0 and low <= high do
      middle := (low + high) div 2;
      if aKey < arr[middle] then
        high := pred(middle);
      elsif aKey > arr[middle] then
        low := succ(middle);
      else
        index := middle;
      end if;
    end while;
  end func;

const proc: main is func
  local
    var array integer: p5 is 249 times 0;
    var integer: i is 0;
    var integer: x0 is 0;
    var integer: x1 is 0;
    var integer: x2 is 0;
    var integer: x3 is 0;
    var integer: sum is 0;
    var integer: y is 0;
    var boolean: found is FALSE;
  begin
    for i range 1 to 249 do
      p5[i] := i ** 5;
    end for;
    for x0 range 1 to 249 until found do
      for x1 range 1 to pred(x0) until found do
        for x2 range 1 to pred(x1) until found do
          for x3 range 1 to pred(x2) until found do
            sum := p5[x0] + p5[x1] + p5[x2] + p5[x3];
            y := binarySearch(p5, sum);
            if y > 0 then
              writeln(x0 <& "**5 + " <& x1 <& "**5 + " <& x2 <& "**5 + " <& x3 <& "**5 = " <& y <& "**5");
              found := TRUE;
            end if;
          end for;
        end for;
      end for;
    end for;
    if not found then
      writeln("No solution was found");
    end if;
  end func;
```


```txt

133**5 + 110**5 + 84**5 + 27**5 = 144**5

```



## Sidef

```ruby
define range = (1 ..^ 250)

var p5 = Hash()
var sum2 = Hash()

for i in (range) {
    p5{i**5} = i
    for j in (range) {
        sum2{i**5 + j**5} = [i, j]
    }
}

var sk = sum2.keys.map{ Num(_) }.sort

for p in (p5.keys.map{ Num(_) }.sort) {

    var s = sk.first {|s|
        p > s && sum2.exists(p-s)
    } \\ next

    var t = (sum2{s} + sum2{p-s} -> map{|n| "#{n}⁵" }.join(' + '))
    say "#{t} = #{p5{p}}⁵"
    break
}
```

```txt

84⁵ + 27⁵ + 133⁵ + 110⁵ =  144⁵

```



## VBA

```vb
Public Declare Function GetTickCount Lib "kernel32.dll" () As Long
Public Sub begin()
    start_int = GetTickCount()
    main
    Debug.Print (GetTickCount() - start_int) / 1000 & " seconds"
End Sub
Private Function pow(x, y) As Variant
    pow = CDec(Application.WorksheetFunction.Power(x, y))
End Function
Private Sub main()
    For x0 = 1 To 250
        For x1 = 1 To x0
            For x2 = 1 To x1
                For x3 = 1 To x2
                    sum = CDec(pow(x0, 5) + pow(x1, 5) + pow(x2, 5) + pow(x3, 5))
                    s1 = Int(pow(sum, 0.2))
                    If sum = pow(s1, 5) Then
                        Debug.Print x0 & "^5 + " & x1 & "^5 + " & x2 & "^5 + " & x3 & "^5 = " & s1
                        Exit Sub
                    End If
                Next x3
            Next x2
        Next x1
    Next x0
End Sub
```
```txt
133^5 + 110^5 + 84^5 + 27^5 = 144
160,187 seconds
```


## VBScript

```vb
Max=250

For X0=1 To Max
	For X1=1 To X0
		For X2=1 To X1
			For X3=1 To X2
				Sum=fnP5(X0)+fnP5(X1)+fnP5(X2)+fnP5(X3)
				S1=Int(Sum^0.2)
				If Sum=fnP5(S1) Then
					WScript.StdOut.Write X0 & " " & X1 & " " & X2 & " " & X3 & " " & S1
					WScript.Quit
				End If
			Next
		Next
	Next
Next

Function fnP5(n)
	fnP5 = n ^ 5
End Function
```


```txt
133 110 84 27 144
```



## Visual Basic .NET


### Paired Powers Algorithm

```vbnet
Module Module1

    Structure Pair
        Dim a, b As Integer
        Sub New(x as integer, y as integer)
            a = x : b = y
        End Sub
    End Structure

    Dim max As Integer = 250
    Dim p5() As Long,
        sum2 As SortedDictionary(Of Long, Pair) = New SortedDictionary(Of Long, Pair)

    Function Fmt(p As Pair) As String
        Return String.Format("{0}^5 + {1}^5", p.a, p.b)
    End Function

    Sub Init()
        p5(0) = 0 : p5(1) = 1 : For i As Integer = 1 To max - 1
            For j As Integer = i + 1 To max
                p5(j) = CLng(j) * j : p5(j) *= p5(j) * j
                sum2.Add(p5(i) + p5(j), New Pair(i, j))
            Next
        Next
    End Sub

    Sub Calc(Optional findLowest As Boolean = True)
        For i As Integer = 1 To max : Dim p As Long = p5(i)
            For Each s In sum2.Keys
                Dim t As Long = p - s : If t <= 0 Then Exit For
                If sum2.Keys.Contains(t) AndAlso sum2.Item(t).a > sum2.Item(s).b Then
                    Console.WriteLine("  {1} + {2} = {0}^5", i, Fmt(sum2.Item(s)), Fmt(sum2.Item(t)))
                    If findLowest Then Exit Sub
                End If
            Next : Next
    End Sub

    Sub Main(args As String())
        If args.Count > 0 Then
            Dim t As Integer = 0 : Integer.TryParse(args(0), t)
            If t > 0 AndAlso t < 5405 Then max = t
        End If
        Console.WriteLine("Checking from 1 to {0}...", max)
        For i As Integer = 0 To 1
            ReDim p5(max) : sum2.Clear()
            Dim st As DateTime = DateTime.Now
            Init() : Calc(i = 0)
            Console.WriteLine("{0}  Computation time to {2} was {1} seconds{0}", vbLf,
                (DateTime.Now - st).TotalSeconds, If(i = 0, "find lowest one", "check entire space"))
        Next
        If Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module
```

{{out}}(No command line arguments)

```txt
Checking from 1 to 250...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5

  Computation time to find lowest one was 0.0807819 seconds

  27^5 + 84^5 + 110^5 + 133^5 = 144^5

  Computation time to check entire space was 0.3830103 seconds
```

Command line argument = "1000"
```txt

Checking from 1 to 1000...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5

  Computation time to find lowest one was 0.3112007 seconds

  27^5 + 84^5 + 110^5 + 133^5 = 144^5
  54^5 + 168^5 + 220^5 + 266^5 = 288^5
  81^5 + 252^5 + 330^5 + 399^5 = 432^5
  108^5 + 336^5 + 440^5 + 532^5 = 576^5
  135^5 + 420^5 + 550^5 + 665^5 = 720^5
  162^5 + 504^5 + 660^5 + 798^5 = 864^5

  Computation time to check entire space was 28.8847393 seconds
```


### Paired Powers w/ Mod 30 Shortcut and Threading

If one divides the searched array of powers ('''sum2m()''') into 30 pieces, the search time can be reduced by only searching the appropriate one (determined by the Mod 30 value of the value being sought).  Once broken down by this, it is now easier to use threading to further reduce the computation time.<br/>The following compares the plain paired powers algorithm to the plain powers plus the mod 30 shortcut algorithm, without and with threading.

```vbnet
Module Module1

    Structure Pair
        Dim a, b As Integer
        Sub New(x As Integer, y As Integer)
            a = x : b = y
        End Sub
    End Structure

    Dim min As Integer = 1, max As Integer = 250
    Dim p5() As Long,
        sum2 As SortedDictionary(Of Long, Pair) = New SortedDictionary(Of Long, Pair),
        sum2m(29) As SortedDictionary(Of Long, Pair)

    Function Fmt(p As Pair) As String
        Return String.Format("{0}^5 + {1}^5", p.a, p.b)
    End Function

    Sub Init()
        p5(0) = 0 : p5(min) = CLng(min) * min : p5(min) *= p5(min) * min
        For i As Integer = min To max - 1
            For j As Integer = i + 1 To max
                p5(j) = CLng(j) * j : p5(j) *= p5(j) * j
                If j = max Then Continue For
                sum2.Add(p5(i) + p5(j), New Pair(i, j))
            Next
        Next
    End Sub

    Sub InitM()
        For i As Integer = 0 To 29 : sum2m(i) = New SortedDictionary(Of Long, Pair) : Next
        p5(0) = 0 : p5(min) = CLng(min) * min : p5(min) *= p5(min) * min
        For i As Integer = min To max - 1
            For j As Integer = i + 1 To max
                p5(j) = CLng(j) * j : p5(j) *= p5(j) * j
                If j = max Then Continue For
                Dim x As Long = p5(i) + p5(j)
                sum2m(x Mod 30).Add(x, New Pair(i, j))
            Next
        Next
    End Sub

    Sub Calc(Optional findLowest As Boolean = True)
        For i As Integer = min To max : Dim p As Long = p5(i)
            For Each s In sum2.Keys
                Dim t As Long = p - s : If t <= 0 Then Exit For
                If sum2.Keys.Contains(t) AndAlso sum2.Item(t).a > sum2.Item(s).b Then
                    Console.WriteLine("  {1} + {2} = {0}^5", i,
                        Fmt(sum2.Item(s)), Fmt(sum2.Item(t)))
                    If findLowest Then Exit Sub
                End If
            Next : Next
    End Sub

    Function CalcM(m As Integer) As List(Of String)
        Dim res As New List(Of String)
        For i As Integer = min To max
            Dim pm As Integer = i Mod 30,
                mp As Integer = (pm - m + 30) Mod 30
            For Each s In sum2m(m).Keys
                Dim t As Long = p5(i) - s : If t <= 0 Then Exit For
                If sum2m(mp).Keys.Contains(t) AndAlso
                  sum2m(mp).Item(t).a > sum2m(m).Item(s).b Then
                    res.Add(String.Format("  {1} + {2} = {0}^5",
                        i, Fmt(sum2m(m).Item(s)), Fmt(sum2m(mp).Item(t))))
                End If
            Next : Next
        Return res
    End Function

    Function Snip(s As String) As Integer
        Dim p As Integer = s.IndexOf("=") + 1
        Return s.Substring(p, s.IndexOf("^", p) - p)
    End Function

    Function CompareRes(ByVal x As String, ByVal y As String) As Integer
        CompareRes = Snip(x).CompareTo(Snip(y))
        If CompareRes = 0 Then CompareRes = x.CompareTo(y)
    End Function

    Function Validify(def As Integer, s As String) As Integer
        Validify = def : Dim t As Integer = 0 : Integer.TryParse(s, t)
        If t >= 1 AndAlso Math.Pow(t, 5) < (Long.MaxValue >> 1) Then Validify = t
    End Function

    Sub Switch(ByRef a As Integer, ByRef b As Integer)
        Dim t As Integer = a : a = b : b = t
    End Sub

    Sub Main(args As String())
        Select Case args.Count
            Case 1 : max = Validify(max, args(0))
            Case > 1
                min = Validify(min, args(0))
                max = Validify(max, args(1))
                If max < min Then Switch(max, min)
        End Select
        Console.WriteLine("Paired powers, checking from {0} to {1}...", min, max)
        For i As Integer = 0 To 1
            ReDim p5(max) : sum2.Clear()
            Dim st As DateTime = DateTime.Now
            Init() : Calc(i = 0)
            Console.WriteLine("{0}  Computation time to {2} was {1} seconds{0}", vbLf,
                (DateTime.Now - st).TotalSeconds, If(i = 0, "find lowest one", "check entire space"))
        Next
        For i As Integer = 0 To 1
            Console.WriteLine("Paired powers with Mod 30 shortcut (entire space) {2}, checking from {0} to {1}...",
                min, max, If(i = 0, "sequential", "parallel"))
            ReDim p5(max)
            Dim res As List(Of String) = New List(Of String)
            Dim st As DateTime = DateTime.Now
            Dim taskList As New List(Of Task(Of List(Of String)))
            InitM()
            Select Case i
                Case 0
                    For j As Integer = 0 To 29
                        res.AddRange(CalcM(j))
                    Next
                Case 1
                    For j As Integer = 0 To 29 : Dim jj = j
                        taskList.Add(Task.Run(Function() CalcM(jj)))
                    Next
                    Task.WhenAll(taskList)
                    For Each item In taskList.Select(Function(t) t.Result)
                        res.AddRange(item) : Next
            End Select
            res.Sort(AddressOf CompareRes)
            For Each item In res
                Console.WriteLine(item) : Next
            Console.WriteLine("{0}  Computation time was {1} seconds{0}", vbLf, (DateTime.Now - st).TotalSeconds)
        Next
        If Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module
```

{{out}}(No command line arguments)
```txt
Paired powers, checking from 1 to 250...
27^5 + 84^5 + 110^5 + 133^5 = 144^5

Computation time to find lowest one was 0.0781252 seconds

27^5 + 84^5 + 110^5 + 133^5 = 144^5

Computation time to check entire space was 0.3280574 seconds

Paired powers with Mod 30 shortcut (entire space) sequential, checking from 1 to 250...
27^5 + 84^5 + 110^5 + 133^5 = 144^5

Computation time was 0.2655529 seconds

Paired powers with Mod 30 shortcut (entire space) parallel, checking from 1 to 250...
27^5 + 84^5 + 110^5 + 133^5 = 144^5

Computation time was 0.0624651 seconds
```

(command line argument = "1000")
```txt
Paired powers, checking from 1 to 1000...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5

  Computation time to find lowest one was 0.2499343 seconds

  27^5 + 84^5 + 110^5 + 133^5 = 144^5
  54^5 + 168^5 + 220^5 + 266^5 = 288^5
  81^5 + 252^5 + 330^5 + 399^5 = 432^5
  108^5 + 336^5 + 440^5 + 532^5 = 576^5
  135^5 + 420^5 + 550^5 + 665^5 = 720^5
  162^5 + 504^5 + 660^5 + 798^5 = 864^5

  Computation time to check entire space was 27.805961 seconds

Paired powers with Mod 30 shortcut (entire space) sequential, checking from 1 to 1000...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5
  54^5 + 168^5 + 220^5 + 266^5 = 288^5
  81^5 + 252^5 + 330^5 + 399^5 = 432^5
  108^5 + 336^5 + 440^5 + 532^5 = 576^5
  135^5 + 420^5 + 550^5 + 665^5 = 720^5
  162^5 + 504^5 + 660^5 + 798^5 = 864^5

  Computation time was 23.8068928 seconds

Paired powers with Mod 30 shortcut (entire space) parallel, checking from 1 to 1000...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5
  54^5 + 168^5 + 220^5 + 266^5 = 288^5
  81^5 + 252^5 + 330^5 + 399^5 = 432^5
  108^5 + 336^5 + 440^5 + 532^5 = 576^5
  135^5 + 420^5 + 550^5 + 665^5 = 720^5
  162^5 + 504^5 + 660^5 + 798^5 = 864^5

  Computation time was 5.4205943 seconds
```

(command line arguments = "27 864")
```txt
Paired powers, checking from 27 to 864...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5

  Computation time to find lowest one was 0.1562309 seconds

  27^5 + 84^5 + 110^5 + 133^5 = 144^5
  54^5 + 168^5 + 220^5 + 266^5 = 288^5
  81^5 + 252^5 + 330^5 + 399^5 = 432^5
  108^5 + 336^5 + 440^5 + 532^5 = 576^5
  135^5 + 420^5 + 550^5 + 665^5 = 720^5
  162^5 + 504^5 + 660^5 + 798^5 = 864^5

  Computation time to check entire space was 15.8243802 seconds

Paired powers with Mod 30 shortcut (entire space) sequential, checking from 27 to 864...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5
  54^5 + 168^5 + 220^5 + 266^5 = 288^5
  81^5 + 252^5 + 330^5 + 399^5 = 432^5
  108^5 + 336^5 + 440^5 + 532^5 = 576^5
  135^5 + 420^5 + 550^5 + 665^5 = 720^5
  162^5 + 504^5 + 660^5 + 798^5 = 864^5

  Computation time was 13.0438215 seconds

Paired powers with Mod 30 shortcut (entire space) parallel, checking from 27 to 864...
  27^5 + 84^5 + 110^5 + 133^5 = 144^5
  54^5 + 168^5 + 220^5 + 266^5 = 288^5
  81^5 + 252^5 + 330^5 + 399^5 = 432^5
  108^5 + 336^5 + 440^5 + 532^5 = 576^5
  135^5 + 420^5 + 550^5 + 665^5 = 720^5
  162^5 + 504^5 + 660^5 + 798^5 = 864^5

  Computation time was 3.0305365 seconds
```

(command line arguments = "189 1008")
```txt
Paired powers, checking from 189 to 1008...
  189^5 + 588^5 + 770^5 + 931^5 = 1008^5

  Computation time to find lowest one was 14.6840411 seconds

  189^5 + 588^5 + 770^5 + 931^5 = 1008^5

  Computation time to check entire space was 14.7777685 seconds

Paired powers with Mod 30 shortcut (entire space) sequential, checking from 189 to 1008...
  189^5 + 588^5 + 770^5 + 931^5 = 1008^5

  Computation time was 12.4814705 seconds

Paired powers with Mod 30 shortcut (entire space) parallel, checking from 189 to 1008...
  189^5 + 588^5 + 770^5 + 931^5 = 1008^5

  Computation time was 2.7180777 seconds
```



## zkl

Uses two look up tables for efficiency. Counts from 0 for ease of coding.

```zkl
pow5s:=[1..249].apply("pow",5); // (1^5, 2^5, 3^5 .. 249^5)
pow5r:=pow5s.enumerate().apply("reverse").toDictionary(); // [144^5:144, ...]
foreach x0,x1,x2,x3 in (249,x0,x1,x2){
   sum:=pow5s[x0] + pow5s[x1] + pow5s[x2] + pow5s[x3];
   if(pow5r.holds(sum))
      println("%d^5 + %d^5 + %d^5 + %d^5 = %d^5"
          .fmt(x3+1,x2+1,x1+1,x0+1,pow5r[sum]+1));
      break(4);  // the foreach is actually four loops
}
```

```txt
27^5 + 84^5 + 110^5 + 133^5 = 144^5
```

Using the Python technique of caching double sums results in a 5x speed up [to the first/only solution]; actually the speed up is close to 25x but creating the caches dominates the runtime to the first solution.
```zkl
p5,sum2:=Dictionary(),Dictionary();
foreach i in ([1..249]){
   p5[i.pow(5)]=i;
   foreach j in ([i..249]){ sum2[i.pow(5) + j.pow(5)]=T(i,j) } // 31,125 keys
}

sk:=sum2.keys.apply("toInt").copy().sort(); // RW list sorts faster than a RO one
foreach p,s in (p5.keys.apply("toInt"),sk){
   if(p<=s) break;
   if(sum2.holds(p - s)){
      println("%d^5 + %d^5 + %d^5 + %d^5 = %d^5"
          .fmt(sum2[s].xplode(),sum2[p - s].xplode(),p5[p]));
      break(2);  // or get permutations
   }
}
```

Note: dictionary keys are always strings and copying a read only list creates a read write list.
```txt
27^5 + 84^5 + 110^5 + 133^5 = 144^5
```



## ZX Spectrum Basic

{{incorrect|ZX Spectrum Basic|ZX Spectrum Basic has one numerical type, a floating point type of 5 bytes, of which the first byte holds the mantissa leaving 4 bytes for the number. 144^5 is to big to fit in 4 bytes.
It will lose precision, the program will not find the a solution.}}
Very, very, very slow. Even with an emulator at full speed.

```zxbasic
10 LET max=250
20 FOR w=1 TO max: FOR x=1 TO w: FOR y=1 TO x: FOR z=1 TO y
30 LET sum=w^5+x^5+y^5+z^5
40 LET s1=INT (sum^0.2)
50 IF sum=s1^5 THEN PRINT w;"^5+";x;"^5+";y;"^5+";z;"^5=";s1;"^5": STOP
60 NEXT z: NEXT y: NEXT x: NEXT w
```


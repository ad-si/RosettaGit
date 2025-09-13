+++
title = "Fractran"
description = ""
date = 2019-09-17T16:04:14Z
aliases = []
[extra]
id = 17107
[taxonomies]
categories = ["task"]
tags = []
+++

'''[[wp:FRACTRAN|FRACTRAN]]''' is a Turing-complete esoteric programming language invented by the mathematician [[wp:John Horton Conway|John Horton Conway]].

A FRACTRAN program is an ordered list of positive fractions <math>P = (f_1, f_2, \ldots, f_m)</math>, together with an initial positive integer input <math>n</math>.


The program is run by updating the integer <math>n</math> as follows:

* for the first fraction, <math>f_i</math>, in the list for which <math>nf_i</math> is an integer, replace <math>n</math> with <math>nf_i</math> ;
* repeat this rule until no fraction in the list produces an integer when multiplied by <math>n</math>, then halt.



Conway gave a program for primes in FRACTRAN:

: <math>17/91</math>, <math>78/85</math>, <math>19/51</math>, <math>23/38</math>, <math>29/33</math>, <math>77/29</math>, <math>95/23</math>, <math>77/19</math>, <math>1/17</math>, <math>11/13</math>, <math>13/11</math>, <math>15/14</math>, <math>15/2</math>, <math>55/1</math>

Starting with <math>n=2</math>, this FRACTRAN program will change <math>n</math> to <math>15=2\times (15/2)</math>, then <math>825=15\times (55/1)</math>, generating the following sequence of integers:

: <math>2</math>, <math>15</math>, <math>825</math>, <math>725</math>, <math>1925</math>, <math>2275</math>, <math>425</math>, <math>390</math>, <math>330</math>, <math>290</math>, <math>770</math>, <math>\ldots</math>

After 2, this sequence contains the following powers of 2:

:<math>2^2=4</math>, <math>2^3=8</math>, <math>2^5=32</math>, <math>2^7=128</math>, <math>2^{11}=2048</math>, <math>2^{13}=8192</math>, <math>2^{17}=131072</math>, <math>2^{19}=524288</math>, <math>\ldots</math>

which are the prime powers of 2.


## Task

Write a program that reads a list of fractions in a ''natural'' format from the keyboard or from a string,
to parse it into a sequence of fractions (''i.e.'' two integers),
and runs the FRACTRAN starting from a provided integer, writing the result at each step.
It is also required that the number of steps is limited (by a parameter easy to find).


;Extra credit:
Use this program to derive the first '''20''' or so prime numbers.


## See also

For more on how to program FRACTRAN as a universal programming language, see:
* J. H. Conway (1987). Fractran: A Simple Universal Programming Language for Arithmetic. In: Open Problems in Communication and Computation, pages 4–26. Springer.

* J. H. Conway (2010). "FRACTRAN: A simple universal programming language for arithmetic". In Jeffrey C. Lagarias. The Ultimate Challenge: the 3x+1 problem. American Mathematical Society. pp. 249–264. ISBN 978-0-8218-4940-8. Zbl 1216.68068.

* [http://scienceblogs.com/goodmath/2006/10/27/prime-number-pathology-fractra/Prime Number Pathology: Fractran] by  Mark C. Chu-Carroll; October 27, 2006.





## 360 Assembly


```360asm
*        FRACTRAN                  17/02/2019
FRACTRAN CSECT
         USING  FRACTRAN,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,TERMS)    do i=1 to terms
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,=A(NF))     do j=1 to nfracs
         LR     R1,R7                  j
         SLA    R1,3                   ~
         L      R2,FRACS-4(R1)         d(j)
         L      R4,NN                  nn
         SRDA   R4,32                  ~
         DR     R4,R2                  nn/d(j)
       IF       LTR,R4,Z,R4 THEN       if mod(nn,d(j))=0 then
         XDECO  R6,XDEC                  edit i
         MVC    PG(3),XDEC+9             output i
         L      R1,NN                    nn
         XDECO  R1,PG+5                  edit & output nn
         XPRNT  PG,L'PG                  print buffer
         LR     R1,R7                    j
         SLA    R1,3                     ~
         L      R3,FRACS-8(R1)           n(j)
         MR     R4,R3                    *n(j)
         ST     R5,NN                    nn=nn/d(j)*n(j)
         B      LEAVEJ                   leave j
       ENDIF    ,                      end if
         LA     R7,1(R7)               j++
       ENDDO    ,                    end do j
LEAVEJ   LA     R6,1(R6)             i++
       ENDDO    ,                  end do i
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
NF       EQU    (TERMS-FRACS)/8    number of fracs
NN       DC     F'2'               nn
FRACS  DC  F'17',F'91',F'78',F'85',F'19',F'51',F'23',F'38',F'29',F'33'
       DC  F'77',F'29',F'95',F'23',F'77',F'19',F'1',F'17',F'11',F'13'
       DC  F'13',F'11',F'15',F'14',F'15',F'2',F'55',F'1'
TERMS    DC     F'100'             terms
PG       DC     CL80'*** :'        buffer
XDEC     DS     CL12               temp
         REGEQU
         END    FRACTRAN
```

```txt

  1 :           2
  2 :          15
  3 :         825
  4 :         725
  5 :        1925
  6 :        2275
  7 :         425
  8 :         390
  9 :         330
 10 :         290
...
 99 :        2128
100 :        1288

```




## Ada



```Ada
with Ada.Text_IO;

procedure Fractan is

   type Fraction is record Nom: Natural; Denom: Positive; end record;
   type Frac_Arr is array(Positive range <>) of Fraction;

   function "/" (N: Natural; D: Positive) return Fraction is
      Frac: Fraction := (Nom => N, Denom => D);
   begin
      return Frac;
   end "/";

   procedure F(List: Frac_Arr; Start: Positive; Max_Steps: Natural) is
      N: Positive := Start;
      J: Positive;
   begin
      Ada.Text_IO.Put(" 0:" & Integer'Image(N) & "   ");
      for I in 1 .. Max_Steps loop
	 J := List'First;
	 loop
	    if N mod List(J).Denom = 0 then
	       N := (N/List(J).Denom) * List(J).Nom;
	       exit; -- found fraction
	    elsif J >= List'Last then
	       return; -- did try out all fractions
	    else
	       J := J + 1; -- try the next fraction
	    end if;
	 end loop;
	 Ada.Text_IO.Put(Integer'Image(I) & ":" & Integer'Image(N) & "   ");
      end loop;
   end F;

begin
   -- F((2/3, 7/2, 1/5, 1/7, 1/9, 1/4, 1/8), 2, 100);
   -- output would be "0: 2    1: 7    2: 1" and then terminate

   F((17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23,
      77/19,  1/17, 11/13, 13/11, 15/14,  15/2, 55/1),
     2, 15);
   -- output is "0: 2    1: 15    2: 825    3: 725   ...   14: 132    15: 116"
end Fractan;
```


```txt
 0: 2    1: 15    2: 825    3: 725    4: 1925    5: 2275    6: 425    7: 390    8: 330    9: 290    10: 770    11: 910    12: 170    13: 156    14: 132    15: 116
```



## ALGOL 68

```algol68
# as the numbers required for finding the first 20 primes are quite large, #
# we use Algol 68G's LONG LONG INT with a precision of 100 digits          #
PR precision 100 PR

# mode to hold fractions #
MODE FRACTION = STRUCT( INT numerator, INT denominator );

# define / between two INTs to yield a FRACTION #
OP / = ( INT a, b )FRACTION: ( a, b );

# mode to define a FRACTRAN progam #
MODE FRACTRAN = STRUCT( FLEX[0]FRACTION data
                      , LONG LONG INT   n
                      , BOOL            halted
                      );
# prepares a FRACTRAN program for use - sets the initial value of n and halted to FALSE #
PRIO STARTAT = 1;
OP   STARTAT = ( REF FRACTRAN f, INT start )REF FRACTRAN:
BEGIN
    halted OF f := FALSE;
         n OF f := start;
    f
END;

# sets n OF f to the next number in the sequence or sets halted OF f to TRUE if the sequence has ended #
OP NEXT = ( REF FRACTRAN f )LONG LONG INT:
    IF halted OF f
    THEN n OF f := 0
    ELSE
        BOOL          found  := FALSE;
        LONG LONG INT result := 0;
        FOR pos FROM LWB data OF f TO UPB data OF f WHILE NOT found DO
            LONG LONG INT value       = n OF f * numerator OF ( ( data OF f )[ pos ] );
            INT           denominator = denominator OF ( ( data OF f )[ pos ] );
            IF found := ( value MOD denominator = 0 ) THEN result := value OVER denominator FI
        OD;
        IF NOT found THEN halted OF f := TRUE FI;
        n OF f := result
    FI ;

# generate and print the sequence of numbers from a FRACTRAN pogram #
PROC print fractran sequence = ( REF FRACTRAN f, INT start, INT limit )VOID:
BEGIN
    VOID( f STARTAT start );
    print( ( "0: ", whole( start, 0 ) ) );
    FOR i TO limit
    WHILE VOID( NEXT f );
          NOT halted OF f
    DO
        print( ( " " + whole( i, 0 ) + ": " + whole( n OF f, 0 ) ) )
    OD;
    print( ( newline ) )
END ;

# print the first 16 elements from the primes FRACTRAN program #
FRACTRAN pf := ( ( 17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14,  15/2, 55/1 ), 0, FALSE );
print fractran sequence( pf, 2, 15 );

# find some primes using the pf FRACTRAN progam - n is prime for the members in the sequence that are 2^n #
INT primes found := 0;
VOID( pf STARTAT 2 );
INT pos := 0;
print( ( "seq position  prime sequence value", newline ) );
WHILE primes found < 20 AND NOT halted OF pf DO
    LONG LONG INT value      := NEXT pf;
    INT      power of 2 := 0;
    pos +:= 1;
    WHILE value MOD 2 = 0 AND value > 0 DO power of 2 PLUSAB 1; value OVERAB 2 OD;
    IF value = 1 THEN
        # found a prime #
        primes found +:= 1;
        print( ( whole( pos, -12 ) + " " + whole( power of 2, -6 ) + " (" + whole( n OF pf, 0 ) + ")", newline ) )
    FI
OD
```

```txt

0: 2 1: 15 2: 825 3: 725 4: 1925 5: 2275 6: 425 7: 390 8: 330 9: 290 10: 770 11: 910 12: 170 13: 156 14: 132 15: 116
seq position  prime sequence value
          19      2 (4)
          69      3 (8)
         280      5 (32)
         707      7 (128)
        2363     11 (2048)
        3876     13 (8192)
        8068     17 (131072)
       11319     19 (524288)
       19201     23 (8388608)
       36866     29 (536870912)
       45551     31 (2147483648)
       75224     37 (137438953472)
      101112     41 (2199023255552)
      117831     43 (8796093022208)
      152025     47 (140737488355328)
      215384     53 (9007199254740992)
      293375     59 (576460752303423488)
      327020     61 (2305843009213693952)
      428553     67 (147573952589676412928)
      507519     71 (2361183241434822606848)

```



## AutoHotkey


```AutoHotkey
n := 2, steplimit := 15, numerator := [], denominator := []
s := "17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1"

Loop, Parse, s, % A_Space
    if (!RegExMatch(A_LoopField, "^(\d+)/(\d+)$", m))
        MsgBox, % "Invalid input string (" A_LoopField ")."
    else
        numerator[A_Index] := m1, denominator[A_Index] := m2

SetFormat, FloatFast, 0.0
Gui, Add, ListView, R10 W100 -Hdr, |
SysGet, VSBW, 2
LV_ModifyCol(1, 95 - VSBW), LV_Add( , 0 ": " n)
Gui, Show

Loop, % steplimit {
    i := A_Index
    Loop, % numerator.MaxIndex()
        if (!Mod(nn := n * numerator[A_Index] / denominator[A_Index], 1)) {
            LV_Modify(LV_Add( , i ": " (n := nn)), "Vis")
            continue, 2
        }
    break
}
```

```txt
0: 2
1: 15
2: 825
3: 725
4: 1925
5: 2275
6: 425
7: 390
8: 330
9: 290
10: 770
11: 910
12: 170
13: 156
14: 132
15: 116
```



## bash


the "factor" command allows one to decrypt the data. For example, the program below computes the product of a and b, entered as 2<sup>a</sup> and 3<sup>b</sup>, the product being 5<sup>a×b</sup>. Two arrays are computed from the fractions, ns for the numerators and ds for the denominators. Then, every time where the multiplication by a fraction yields an integer, the output of the division is stored into a csv file in factored format.


```bash
#! /bin/bash
program="1/1 455/33 11/13 1/11 3/7 11/2 1/3"
echo $program | tr " " "\n" | cut -d"/" -f1 | tr "\n" " " > "data"
read -a ns < "data"
echo $program | tr " " "\n" | cut -d"/" -f2 | tr "\n" " " > "data"
read -a ds < "data"


t=0
n=72
echo "steps of computation" > steps.csv
while [ $t -le 6 ]; do
	if [ $(($n*${ns[$t]}%${ds[$t]})) -eq 0 ]; then
		let "n=$(($n*${ns[$t]}/${ds[$t]}))"
		let "t=0"
		factor $n >> steps.csv
	fi
	let "t=$t+1"
done

```


If at the beginning n=72=2<sup>3</sup>×3<sup>2</sup> (to compute 3×2), the steps of the computation look like this:


```txt
steps of computation
72: 2 2 2 3 3
396: 2 2 3 3 11
5460: 2 2 3 5 7 13
4620: 2 2 3 5 7 11
63700: 2 2 5 5 7 7 13
53900: 2 2 5 5 7 7 11
4900: 2 2 5 5 7 7
2100: 2 2 3 5 5 7
900: 2 2 3 3 5 5
4950: 2 3 3 5 5 11
68250: 2 3 5 5 5 7 13
57750: 2 3 5 5 5 7 11
796250: 2 5 5 5 5 7 7 13
673750: 2 5 5 5 5 7 7 11
61250: 2 5 5 5 5 7 7
26250: 2 3 5 5 5 5 7
11250: 2 3 3 5 5 5 5
61875: 3 3 5 5 5 5 11
853125: 3 5 5 5 5 5 7 13
721875: 3 5 5 5 5 5 7 11
9953125: 5 5 5 5 5 5 7 7 13
8421875: 5 5 5 5 5 5 7 7 11
765625: 5 5 5 5 5 5 7 7
328125: 3 5 5 5 5 5 5 7
140625: 3 3 5 5 5 5 5 5
46875: 3 5 5 5 5 5 5
15625: 5 5 5 5 5 5

```


This file can be opened with a spreadsheet to draw the successive states of the prime numbers (with countif) and then look how the computation is done in successive steps.



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

	::Set the inputs
set "code=17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1"
set "n=2"

	::Basic validation of code
for %%. in (!code!) do (
	echo.%%.|findstr /r /c:"^[0-9][0-9]*/[1-9][0-9]*$">nul||goto error_code
)
	::Validate the input
set /a "tst=1*!n!" 2>nul
if !tst! lss 0 goto error_input
if !tst! equ 0 (if not "!n!"=="0" (goto error_input))

	::Set the limit outputs
	set limit=20

	::Execute the code
echo.Input:
echo.	!n!
echo.Output:
for /l %%? in (1,1,!limit!) do (
	set shouldwehalt=1
	for %%A in (!code!) do (
		for /f "tokens=1,2 delims=/" %%B in ("%%A") do (
			set /a "tst=!n! %% %%C"
			if !tst! equ 0 (
				if !shouldwehalt! equ 1 (
					set shouldwehalt=0
					set /a "n=n*%%B/%%C"
					echo.	!n!
				)
			)
		)
	)
	if !shouldwehalt! equ 1 goto halt
)

:halt
echo.
pause
exit /b 0

:error_code
echo.Syntax error in code.
echo.
pause
exit /b 1

:error_input
echo.Invalid input.
echo.
pause
exit /b 1
```

```txt
Input:
        2
Output:
        15
        825
        725
        1925
        2275
        425
        390
        330
        290
        770
        910
        170
        156
        132
        116
        308
        364
        68
        4
        30

Press any key to continue . . .
```



## Befunge


This takes as input a space-separated list of fractions, a starting value, and the number of iterations to output.

Note that in some interpreters you may need to press <Return> twice after entering the fractions if the ''Starting value'' prompt doesn't at first appear.


```befunge
p0" :snoitcarF">:#,_>&00g5p~$&00g:v
v"Starting value: "_^#-*84~p6p00+1<
>:#,_&0" :snoitaretI">:#,_#@>>$&\:v
:$_\:10g5g*:10g6g%v1:\1$\$<|!:-1\.<
g0^<!:-1\p01+1g01$_10g6g/\^>\010p00
```


```txt
Fractions: 17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1
Starting value: 2
Iterations: 16
2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132 116
```



## Bracmat

This program computes the first twenty primes. It has to do almost 430000 iterations to arrive at the twentieth prime, so instead of immediately writing each number to the terminal, it adds it to a list. After the set number of iterations, the list of numbers is written to a text file numbers.lst (21858548 bytes), so you can inspect it. Because it takes some time to do all iterations, its is advisable to write the source code below in a text file 'fractran' and run it in batch mode in the background, instead of starting Bracmat in interactive mode and typing the program at the prompt. The primes, together with the largest number found, are written to a file FRACTRAN.OUT.

```bracmat
(fractran=
  np n fs A Z fi P p N L M
.   !arg:(?N,?n,?fs)                 {Number of iterations, start n, fractions}
  & :?P:?L                           {Initialise accumulators.}
  &   whl
    ' ( -1+!N:>0:?N                  {Stop when counted down to zero.}
      & !n !L:?L                     {Prepend all numbers to result list.}
      & (2\L!n:#?p&!P !p:?P|)        {If log2(n) is rational, append it to list of primes.}
      & !fs:? (/?fi&!n*!fi:~/:?n) ?  {This line does the following (See task description):
                                      "for the first fraction, fi, in the list for which
                                       nfi is an integer, replace n by nfi ;"}
      )
  & :?M
  & whl'(!L:%?n ?L&!n !M:?M)         {Invert list of numbers. (append to long list is
                                      very expensive. Better to prepend and finally invert.}
  & (!M,!P)                          {Return the two lists}
);



( clk$:?t0
& fractran$(430000, 2, 17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1)
  : (?numbers,?primes)
& lst$(numbers,"numbers.lst",NEW)
& put$("
FRACTRAN found these primes:"
  !primes
  "\nThe list of numbers is saved in numbers.txt
The biggest number in the list is"
  (   0:?max
    & !numbers:? (>%@!max:?max&~) ?
  | !max
  )
str$("\ntime: " flt$(clk$+-1*!t0,4) " sec\n")
, "FRACTRAN.OUT",NEW)
);
```

In Linux, run the program as follows (assuming bracmat and the file 'fractran' are in the CWD):

```txt
./bracmat 'get$fractran' &
```

{{out}} in FRACTRAN.OUT:

```txt
FRACTRAN found these primes:
  1
  2
  3
  5
  7
  11
  13
  17
  19
  23
  29
  31
  37
  41
  43
  47
  53
  59
  61
  67

The list of numbers is saved in numbers.txt
The biggest number in the list is
  1842775069354845065175076326808495219647033145169559640049770986129640260031692378106527030467987060546875

time: 1,8668*10E3 sec

```



## C

Using GMP.  Powers of two are in brackets.
For extra credit, pipe the output through <code>| less -S</code>.

```c
#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

typedef struct frac_s *frac;
struct frac_s {
	int n, d;
	frac next;
};

frac parse(char *s)
{
	int offset = 0;
	struct frac_s h = {0}, *p = &h;

	while (2 == sscanf(s, "%d/%d%n", &h.n, &h.d, &offset)) {
		s += offset;
		p = p->next = malloc(sizeof *p);
		*p = h;
		p->next = 0;
	}

	return h.next;
}

int run(int v, char *s)
{
	frac n, p = parse(s);
	mpz_t val;
	mpz_init_set_ui(val, v);

loop:	n = p;
	if (mpz_popcount(val) == 1)
		gmp_printf("\n[2^%d = %Zd]", mpz_scan1(val, 0), val);
	else
		gmp_printf(" %Zd", val);

	for (n = p; n; n = n->next) {
		// assuming the fractions are not reducible
		if (!mpz_divisible_ui_p(val, n->d)) continue;

		mpz_divexact_ui(val, val, n->d);
		mpz_mul_ui(val, val, n->n);
		goto loop;
	}

	gmp_printf("\nhalt: %Zd has no divisors\n", val);

	mpz_clear(val);
	while (p) {
		n = p->next;
		free(p);
		p = n;
	}

	return 0;
}

int main(void)
{
	run(2,	"17/91 78/85 19/51 23/38 29/33 77/29 95/23 "
		"77/19 1/17 11/13 13/11 15/14 15/2 55/1");

	return 0;
}
```



## C++


```cpp

#include <iostream>
#include <sstream>
#include <iterator>
#include <vector>
#include <cmath>

using namespace std;

class fractran
{
public:
    void run( std::string p, int s, int l  )
    {
        start = s; limit = l;
        istringstream iss( p ); vector<string> tmp;
        copy( istream_iterator<string>( iss ), istream_iterator<string>(), back_inserter<vector<string> >( tmp ) );

        string item; vector< pair<float, float> > v;
	pair<float, float> a;
	for( vector<string>::iterator i = tmp.begin(); i != tmp.end(); i++ )
	{
	    string::size_type pos = ( *i ).find( '/', 0 );
	    if( pos != std::string::npos )
	    {
		a = make_pair( atof( ( ( *i ).substr( 0, pos ) ).c_str() ), atof( ( ( *i ).substr( pos + 1 ) ).c_str() ) );
		v.push_back( a );
	    }
	}

	exec( &v );
    }

private:
    void exec( vector< pair<float, float> >* v )
    {
	int cnt = 0;
	while( cnt < limit )
	{
	    cout << cnt << " : " << start << "\n";
	    cnt++;
	    vector< pair<float, float> >::iterator it = v->begin();
	    bool found = false; float r;
	    while( it != v->end() )
	    {
		r  = start * ( ( *it ).first / ( *it ).second );
		if( r == floor( r ) )
		{
		    found = true;
		    break;
		}
		++it;
	    }

	    if( found ) start = ( int )r;
	    else break;
	}
    }
    int start, limit;
};
int main( int argc, char* argv[] )
{
    fractran f; f.run( "17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1", 2, 15 );
    cin.get();
    return 0;
}

```

```txt

0 : 2
1 : 15
2 : 825
3 : 725
4 : 1925
5 : 2275
6 : 425
7 : 390
8 : 330
9 : 290
10 : 770
11 : 910
12 : 170
13 : 156
14 : 132

```



## Common Lisp



```lisp
(defun fractran (n frac-list)
  (lambda ()
    (prog1
      n
      (when n
        (let ((f (find-if (lambda (frac)
                            (integerp (* n frac)))
                          frac-list)))
          (when f (setf n (* f n))))))))


;; test

(defvar *primes-ft* '(17/91 78/85 19/51 23/38 29/33 77/29 95/23
                      77/19 1/17 11/13 13/11 15/14 15/2 55/1))

(loop with fractran-instance = (fractran 2 *primes-ft*)
      repeat 20
      for next = (funcall fractran-instance)
      until (null next)
      do (print next))
```


```txt
2
15
825
725
1925
2275
425
390
330
290
770
910
170
156
132
116
308
364
68
4
```



## D


### Simple Version

```d
import std.stdio, std.algorithm, std.conv, std.array;

void fractran(in string prog, int val, in uint limit) {
    const fracts = prog.split.map!(p => p.split("/").to!(int[])).array;

    foreach (immutable n; 0 .. limit) {
        writeln(n, ": ", val);
        const found = fracts.find!(p => val % p[1] == 0);
        if (found.empty)
            break;
        val = found.front[0] * val / found.front[1];
    }
}

void main() {
    fractran("17/91 78/85 19/51 23/38 29/33 77/29 95/23
              77/19 1/17 11/13 13/11 15/14 15/2 55/1", 2, 15);
}
```

```txt
0: 2
1: 15
2: 825
3: 725
4: 1925
5: 2275
6: 425
7: 390
8: 330
9: 290
10: 770
11: 910
12: 170
13: 156
14: 132
```



### Lazy Version


```d
import std.stdio, std.algorithm, std.conv, std.array, std.range;

struct Fractran {
    int front;
    bool empty = false;
    const int[][] fracts;

    this(in string prog, in int val) {
        this.front = val;
        fracts = prog.split.map!(p => p.split("/").to!(int[])).array;
    }

    void popFront() {
        const found = fracts.find!(p => front % p[1] == 0);
        if (found.empty)
            empty = true;
        else
            front = found.front[0] * front / found.front[1];
    }
}

void main() {
    Fractran("17/91 78/85 19/51 23/38 29/33 77/29 95/23
              77/19 1/17 11/13 13/11 15/14 15/2 55/1", 2)
    .take(15).writeln;
}
```

```txt
[2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132]
```



## Elixir

```elixir
defmodule Fractran do
  use Bitwise

  defp binary_to_ratio(b) do
    [_, num, den] = Regex.run(~r/(\d+)\/(\d+)/, b)
    {String.to_integer(num), String.to_integer(den)}
  end

  def load(program) do
    String.split(program) |> Enum.map(&binary_to_ratio(&1))
  end

  defp step(_, []), do: :halt
  defp step(n, [f|fs]) do
    {p, q} = mulrat(f, {n, 1})
    case q do
        1 -> p
        _ -> step(n, fs)
    end
  end

  def exec(k, n, program) do
    exec(k-1, n, fn (_) -> true end, program, [n]) |> Enum.reverse
  end

  def exec(k, n, pred, program) do
    exec(k-1, n, pred, program, [n]) |> Enum.reverse
  end

  defp exec(0, _, _, _, steps), do: steps
  defp exec(k, n, pred, program, steps) do
    case step(n, program) do
        :halt -> steps
        m -> if pred.(m), do: exec(k-1, m, pred, program, [m|steps]),
                        else: exec(k, m, pred, program, steps)
    end
  end

  def is_pow2(n), do: band(n, n-1) == 0

  def lowbit(n), do: lowbit(n, 0)

  defp lowbit(n, k) do
    case band(n, 1) do
        0 -> lowbit(bsr(n, 1), k + 1)
        1 -> k
    end
  end

  # rational multiplication
  defp mulrat({a, b}, {c, d}) do
    {p, q} = {a*c, b*d}
    g = gcd(p, q)
    {div(p, g), div(q, g)}
  end

  defp gcd(a, 0), do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))
end

primegen = Fractran.load("17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1")
IO.puts "The first few states of the Fractran prime automaton are:\n#{inspect Fractran.exec(20, 2, primegen)}\n"
prime = Fractran.exec(26, 2, &Fractran.is_pow2/1, primegen)
        |> Enum.map(&Fractran.lowbit/1)
        |> tl
IO.puts "The first few primes are:\n#{inspect prime}"
```


```txt

The first few states of the Fractran prime automaton are:
[2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132, 116, 308, 364, 68, 4]

The first few primes are:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

```



## Erlang

The exec() function can be passed a predicate which filters steps that satisfy a condition, which for the prime automata is a check to see if the number is a power of 2.

```erlang
#! /usr/bin/escript

-mode(native).
-import(lists, [map/2, reverse/1]).

binary_to_ratio(B) ->
    {match, [_, Num, Den]} = re:run(B, "([0-9]+)/([0-9]+)"),
    {binary_to_integer(binary:part(B, Num)),
     binary_to_integer(binary:part(B, Den))}.

load(Program) ->
    map(fun binary_to_ratio/1, re:split(Program, "[ ]+")).

step(_, []) -> halt;
step(N, [F|Fs]) ->
    {P, Q} = mulrat(F, {N, 1}),
    case Q of
        1 -> P;
        _ -> step(N, Fs)
    end.

exec(K, N, Program) -> reverse(exec(K - 1, N, fun (_) -> true end, Program, [N])).
exec(K, N, Pred, Program) -> reverse(exec(K - 1, N, Pred, Program, [N])).

exec(0, _, _, _, Steps) -> Steps;
exec(K, N, Pred, Program, Steps) ->
    case step(N, Program) of
        halt -> Steps;
        M -> case Pred(M) of
                true  -> exec(K - 1, M, Pred, Program, [M|Steps]);
                false -> exec(K, M, Pred, Program, Steps)
            end
    end.


is_pow2(N) -> N band (N - 1) =:= 0.

lowbit(N) -> lowbit(N, 0).
lowbit(N, K) ->
    case N band 1 of
        0 -> lowbit(N bsr 1, K + 1);
        1 -> K
    end.

main(_) ->
    PrimeGen = load("17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1"),
    io:format("The first few states of the Fractran prime automaton are: ~p~n~n", [exec(20, 2, PrimeGen)]),
    io:format("The first few primes are: ~p~n", [tl(map(fun lowbit/1, exec(26, 2, fun is_pow2/1, PrimeGen)))]).


% rational multiplication

mulrat({A, B}, {C, D}) ->
    {P, Q} = {A*C, B*D},
    G = gcd(P, Q),
    {P div G, Q div G}.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

```

```txt

└─ $ ▶ ./fractran.erl
The first few states of the Fractran prime automaton are: [2,15,825,725,1925,
                                                           2275,425,390,330,
                                                           290,770,910,170,
                                                           156,132,116,308,
                                                           364,68,4]

The first few primes are: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,
                           67,71,73,79,83,89,97]


```



## Factor


```factor
USING: io kernel math math.functions math.parser multiline
prettyprint sequences splitting ;
IN: rosetta-code.fractran

STRING: fractran-string
17/91 78/85 19/51 23/38 29/33 77/29 95/23
77/19 1/17 11/13 13/11 15/14 15/2 55/1
;

: fractran-parse ( str -- seq )
    " \n" split [ string>number ] map ;

: fractran-step ( seq n -- seq n'/f )
    2dup [ * integer? ] curry find nip dup [ * ] [ nip ] if ;

: fractran-run-full ( seq n -- )
    [ dup ] [ dup . fractran-step ] while 2drop ;

: fractran-run-limited ( seq n steps -- )
    [ dup pprint bl fractran-step ] times 2drop nl ;

: fractran-primes ( #primes seq n -- )
    [ pick zero? ] [
        dup 2 logn dup [ floor 1e-9 ~ ] [ 1. = not ] bi and [
            dup 2 logn >integer pprint bl [ 1 - ] 2dip
        ] when fractran-step
    ] until 3drop nl ;

: main ( -- )
    fractran-string fractran-parse 2
    [ "First 20 numbers: " print 20 fractran-run-limited nl ]
    [ "First 20 primes: " print [ 20 ] 2dip fractran-primes ]
    2bi ;

MAIN: main
```

```txt

First 20 numbers:
2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132 116 308 364 68 4

First 20 primes:
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Fractran this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran


### The Plan

As ever, how long is a piece of string: what might be the upper limit for the list of fractions? Similarly, obtaining the source code to be interpreted is a nuisance since as ever, one doesn't know how long it might be and performing a scan to find out will require more messing about than the activity of interest. One could compile in the specified example, but this is not very flexible even though a re-compilation will be swift. So, a pre-emptive requirement: first specify the number of fractions to be read in. Then reading the fractions has its annoyances also (aside from copying from the example text carrying along all the .html formatting) since given 2/3 a slash is not a suitable delimiter for free-format input, indeed it signifies end-of-input. Again, one can devise an input procedure involving a suitable scan, with more code drowning the stuff specific to the task. So, replace the slashes with a space, an acceptable free-format delimiter. As a comma is also acceptable, use those between the fractions, so, <code>17 91, 78 85, 19 51, 23 38, 29 33, 77 29, 95 23, 77 19, 1 17, 11 13, 13 11, 15 14, 15 2, 55 1</code> Then, all that remains is to specify the starting number, and as well, a step limit. Naturally, the output can be in a more gracious form. It is presented as the input is read so that should something go awry there would be some indication of what was going on.


### The Code

The source style is F77 except for the use of the I0 format code, though not all F77 compilers will offer INTEGER*8. By not using the MODULE scheme, array parameters can't be declared via P(:) which implies a secret additional parameter giving the size of the array and which can be accessed via the likes of <code>UBOUND(P, DIM = 1)</code> Instead, the old-style specification involves no additional parameters and can be given as P(*) meaning "no statement" as to the upper bound, or P(M) which ''may'' be interpreted as the upper bound being the value of M in the compilers that allow this. The actual upper bound of the parameter is unknown and unchecked, so the older style of P(12345) or similar might be used. Rather to my surprise, this compiler (Compaq F90/95) complained if parameter M was declared after the arrays P(M),Q(M) as it is my habit to declare parameters in the order of their appearance.
```Fortran
C:\Nicky\RosettaCode\FRACTRAN\FRACTRAN.for(6) : Warning: This name has not been given an explicit type.   [M]
       INTEGER P(M),Q(M)!The terms of the fractions.
```

So much for multi-pass compilers!

Similarly, without the MODULE protocol, in all calling routines function FRACTRAN would be deemed floating-point so a type declaration is needed in each.
```Fortran
      INTEGER FUNCTION FRACTRAN(N,P,Q,M)	!Notion devised by J. H. Conway.
Careful: the rule is N*P/Q being integer. N*6/3 is integer always because this is N*2/1, but 3 may not divide N.
Could check GCD(P,Q), dividing out the common denominator so MOD(N,Q) works.
       INTEGER*8 N	!The work variable. Modified!
       INTEGER M	!The number of fractions supplied.
       INTEGER P(M),Q(M)!The terms of the fractions.
       INTEGER I	!A stepper.
        DO I = 1,M	!Search the supplied fractions, P(i)/Q(i).
          IF (MOD(N,Q(I)).EQ.0) THEN	!Does the denominator divide N?
            N = N/Q(I)*P(I)	!Yes, compute N*P/Q but trying to dodge overflow.
            FRACTRAN = I	!Report the hit.
           RETURN		!Done!
          END IF	!Otherwise,
        END DO		!Try the next fraction in the order supplied.
        FRACTRAN = 0	!No hit.
      END FUNCTION FRACTRAN	!That's it! Even so, "Turing complete"...

      PROGRAM POKE
      INTEGER FRACTRAN		!Not the default type of function.
      INTEGER P(66),Q(66)	!Holds the fractions as P(i)/Q(i).
      INTEGER*8 N		!The working number.
      INTEGER I,IT,L,M		!Assistants.

      WRITE (6,1)	!Announce.
    1 FORMAT ("Interpreter for J.H. Conway's FRACTRAN language.")

Chew into an example programme.
      OPEN (10,FILE = "Fractran.txt",STATUS="OLD",ACTION="READ")	!Rather than compiled-in stuff.
      READ (10,*) L	!I need to know this without having to scan the input.
      WRITE (6,2) L	!Reveal in case of trouble.
    2 FORMAT (I0," fractions, as follow:")	!Should the input evoke problems.
      READ (10,*) (P(I),Q(I),I = 1,L)	!Ask for the specified number of P,Q pairs.
      WRITE (6,3) (P(I),Q(I),I = 1,L)	!Show what turned up.
    3 FORMAT (24(I0,"/",I0:", "))	!As P(i)/Q(i) pairs. The colon means that there will be no trailing comma.
      READ (10,*) N,M			!The start value, and the step limit.
      CLOSE (10)			!Finished with input.
      WRITE (6,4) N,M			!Hopefully, all went well.
    4 FORMAT ("Start with N = ",I0,", step limit ",I0)

Commence.
      WRITE (6,10) 0,N		!Splat a heading.
   10 FORMAT (/,"  Step  #F: N",/,I6,4X,": ",I0)	!Matched FORMAT 11.
      DO I = 1,M		!Here we go!
        IT = FRACTRAN(N,P,Q,L)		!Do it!
        WRITE (6,11) I,IT,N		!Show it!
   11   FORMAT (I6,I4,": ",I0)		!N last, as it may be big.
        IF (IT.LE.0) EXIT		!No hit, so quit.
      END DO			!The next step.
      END	!Whee!

```



### The Results

Output:

```txt

Interpreter for J.H. Conway's FRACTRAN language.
14 fractions, as follow:
17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1
Start with N = 2, step limit 28

  Step  #F: N
     0    : 2
     1  13: 15
     2  14: 825
     3   5: 725
     4   6: 1925
     5  11: 2275
     6   1: 425
     7   2: 390
     8  10: 330
     9   5: 290
    10   6: 770
    11  11: 910
    12   1: 170
    13   2: 156
    14  10: 132
    15   5: 116
    16   6: 308
    17  11: 364
    18   1: 68
    19   9: 4
    20  13: 30
    21  13: 225
    22  14: 12375
    23   5: 10875
    24   6: 28875
    25   5: 25375
    26   6: 67375
    27  11: 79625
    28   1: 14875

```

Later Fortrans might offer the library function <code>POPCNT(n)</code> which returns the number of on-bits in an integer, most convenient for detecting a straight power of two in a binary computer. Adjusting the interpretation loop to be
```Fortran
      DO I = 1,M		!Here we go!
        IT = FRACTRAN(N,P,Q,L)		!Do it!
        IF (POPCNT(N).EQ.1) WRITE (6,11) I,IT,N		!Show it!
   11   FORMAT (I6,I4,": ",I0)		!N last, as it may be big.
        IF (IT.LE.0) EXIT		!No hit, so quit.
        IF (N.LE.0) THEN		!Otherwise, worry about overflow.
          WRITE (6,*) "Integer overflow!"	!Justified. The test is not certain.
          WRITE (6,11) I,IT,N			!Alas, the step failed.
          EXIT					!Give in.
        END IF				!So much for overflow.
      END DO			!The next step.

```

Leads to the following output:

```txt

Interpreter for J.H. Conway's FRACTRAN language.
14 fractions, as follow:
17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1
Start with N = 2, step limit 246810

  Step  #F: N
     0    : 2
    19   9: 4
    69   9: 8
   280   9: 32
   707   9: 128
  2363   9: 2048
 Integer overflow!
  2863   6: -3816591145242754741

```

And no, fraction #9 is not always the one with the power of two only, for instance step 130 with N = 2<sup>4</sup>x7. So, even a 64-bit integer is not enough, and with a 32-bit integer,

```txt

 Integer overflow!
   296   6: -1406264171

```

In the absence of a proper test such as the <code>IF OVERFLOW ... </code> of First Fortran (1957), the check for overflow relies on the improper appearance of negative numbers during two's-complement binary arithmetic that should only produce positive numbers. This will catch about half the events: N*M might or might not leave the sign bit on when the result overflows.


### Revised Plan

One could introduce a scheme for multi-precision (or "bignum") arithmetic, but there is no need to maintain a unified number N because there are no additions or subtractions, only multiplications and division without remainders. In other words, why not mess about with collections of prime factors? That is, represent N, P, and Q as the list of prime numbers with their powers for each case. To calculate N*P, just add the list of prime number powers for P to that for N, and similarly, for N/Q subtract. At no stage (except perhaps to decorate output or for debugging) need the list be multiplied out to give the unified number and so there is no need for multi-precision multiplies and divides. To determine if N is divisible by Q (that is, if N*fraction = N*P/Q is integral), check only that the primes listed by Q have powers no greater than those of the corresponding primes for N.

To facilitate access without searching the list of primes for N, instead its list is represented by an array of powers, NPPOW, with most entries zero. Thus, NPPOW(i) has the power for PRIME(i) in the factorisation of N, and for N = 2, NPPOW(1) = 1 with all other elements zero. But for FP (the factorisation of P) and FQ (for Q) there is a proper list provided via type FACTORED, whereby FP.PLIST(0) is the count of prime factors and <code>FP.PLIST(1:FP.PLIST(0))</code> fingers the prime numbers that are factors, with FP.PPOW(i) having their corresponding powers. Thus, FP.PLIST(2) has the index of the second prime factor of P (should it have so many), which is PRIME(FP.PLIST(2)), and its power is FP.PPOW(2). Accordingly, to determine if N (as NPPOW) is divisible by one of the fractions FQ, the appropriate elements of NPPOW (that give its powers) must be compared to the corresponding powers in FQ.PPOW, and if ALL of the powers in NPPOW fingered by FQ.PLIST are greater than or equal to those in FQ.PPOW, then a hit! For this purpose, the factorisation of a prime number ''includes'' that prime number. Note that the prime number factorisation of one has no elements in its list, and happily, the ALL operation applied for no tests yields ''true'' as desired, because one divides any number. Thus, in the FRACTRAN op-code system, a fraction P/1 is always a match, and no fractions beyond it will ever be considered.

As a part of the preparation, for each fraction the greatest common divisor is divided out to remove the possibility that converting the test ''N times fraction'' being integral via ''fraction = P/Q'' to ''Q divides N'' will behave differently. For example, N*6/3 will always be integral, but N may not be divisible by three. Reducing 6/3 to 2/1 however will work as will reducing 55/25 to 11/5. The example contains no such occasions, but the possibility nags.

For output, the value of N will not be shown multiplied out but via a schedule showing the powers of the first few prime numbers that form its factorisation. Rather than staring in puzzlement at opaque monster strings of digits, one can view each separate prime factor's power counting up and down as the calculation proceeds. A simple scan of all the factorisations soon determines the highest prime employed, and this never changes. An extension of this checks for which primes are omitted, and in this example, none are. However, a further extension modifies the output of the schedule of powers so as to blank out those that are zero. This could be achieved by replacing the likes of <code>WRITE (...) NPPOW</code> by <code>WRITE (...) I6FMT(NPPOW)</code> where function I6FMT writes out its integer with <code>I6</code> format if the value is positive, and supplies six spaces if not, were it not that few Fortran systems allow such re-entrant usage of the system for formatted output. So, prepare the output with a straightforward WRITE to a CHARACTER variable, blank out the portions where unwanted values appear, and write the result.


### Revised Code

Because this scheme requires a supply of prime numbers, it is convenient to employ the routines prepared for the [[Extensible_prime_generator|extensible prime generator]] via module PRIMEBAG. So, this means escalating to the F90 style, and given that, some compound data structures can be used (for better mnemonics) in place of collections of arrays.
```Fortran
       MODULE CONWAYSIDEA	!Notion devised by J. H. Conway.
       USE PRIMEBAG		!This is a common need.
       INTEGER LASTP,ENUFF	!Some size allowances.
       PARAMETER (LASTP = 66, ENUFF = 66)	!Should suffice for the example in mind.
       INTEGER NPPOW(1:LASTP)	!Represent N as a collection of powers of prime numbers.
       TYPE FACTORED		!But represent P and Q of freaction = P/Q
        INTEGER PNUM(0:LASTP)	!As a list of prime number indices with PNUM(0) the count.
        INTEGER PPOW(LASTP)	!And the powers. for the fingered primes.
       END TYPE FACTORED	!Rather than as a simple number multiplied out.
       TYPE(FACTORED) FP(ENUFF),FQ(ENUFF)	!Thus represent a factored fraction, P(i)/Q(i).
       INTEGER PLIVE(ENUFF),NL	!Helps subroutine SHOWN display NPPOW.
       CONTAINS		!Now for the details.
        SUBROUTINE SHOWFACTORS(N)	!First, to show an internal data structure.
         TYPE(FACTORED) N	!It is supplied as a list of prime factors.
         INTEGER I		!A stepper.
          DO I = 1,N.PNUM(0)	!Step along the list.
            IF (I.GT.1) WRITE (MSG,"('x',$)")	!Append a glyph for "multiply".
            WRITE (MSG,"(I0,$)") PRIME(N.PNUM(I))	!The prime fingered in the list.
            IF (N.PPOW(I).GT.1) WRITE (MSG,"('^',I0,$)") N.PPOW(I)	!With an interesting power?
          END DO		!On to the next element in the list.
          WRITE (MSG,1) N.PNUM(0)	!End the line
    1     FORMAT (": Factor count ",I0)	!With a count of prime factors.
        END SUBROUTINE SHOWFACTORS	!Hopefully, this will not be needed often.

        TYPE(FACTORED) FUNCTION FACTOR(IT)	!Into a list of primes and their powers.
         INTEGER IT,N	!The number and a copy to damage.
         INTEGER P,POW	!A stepper and a power.
         INTEGER F,NF	!A factor and a counter.
          IF (IT.LE.0) STOP "Factor only positive numbers!"	!Or else...
          N = IT	!A copy I can damage.
          NF = 0	!No factors found.
          P = 0		!Because no primes have been tried.
       PP:DO WHILE (N.GT.1)	!Step through the possibilities.
       	    P = P + 1		!Another prime impends.
            F = PRIME(P)	!Grab a possible factor.
            POW = 0		!It has no power yet.
         FP:DO WHILE(MOD(N,F).EQ.0)	!Well?
              POW = POW + 1			!Count a factor..
              N = N/F				!Reduce the number.
            END DO FP			!The P'th prime's power's produced.
            IF (POW.GT.0) THEN	!So, was it a factor?
              IF (NF.GE.LASTP) THEN	!Yes. Have I room in the list?
                WRITE (MSG,1) IT,LASTP	!Alas.
    1           FORMAT ("Factoring ",I0," but with provision for only ",
     1           I0," prime factors!")
                FACTOR.PNUM(0) = NF	!Place the count so far,
                CALL SHOWFACTORS(FACTOR)!So this can be invoked.
                STOP "Not enough storage!"	!Quite.
              END IF			!But normally,
              NF = NF + 1		!Admit another factor.
              FACTOR.PNUM(NF) = P	!Identify the prime. NOT the prime itself.
              FACTOR.PPOW(NF) = POW	!Place its power.
            END IF		!So much for that factor.
          END DO PP	!Try another prime, if N > 1 still.
          FACTOR.PNUM(0) = NF	!Place the count.
        END FUNCTION FACTOR	!Thus, a list of primes and their powers.

        INTEGER FUNCTION GCD(I,J)	!Greatest common divisor.
         INTEGER I,J	!Of these two integers.
         INTEGER N,M,R	!Workers.
          N = MAX(I,J)	!Since I don't want to damage I or J,
          M = MIN(I,J)	!These copies might as well be the right way around.
    1     R = MOD(N,M)		!Divide N by M to get the remainder R.
          IF (R.GT.0) THEN	!Remainder zero?
            N = M			!No. Descend a level.
            M = R			!M-multiplicity has been removed from N.
            IF (R .GT. 1) GO TO 1	!No point dividing by one.
          END IF			!If R = 0, M divides N.
          GCD = M			!There we are.
        END FUNCTION GCD	!Euclid lives on!

        INTEGER FUNCTION FRACTRAN(L)	!Applies Conway's idea to a list of fractions.
Could abandon all parameters since global variables have the details...
         INTEGER L	!The last fraction to consider.
         INTEGER I,NF	!Assistants.
          DO I = 1,L		!Step through the fractions in the order they were given.
            NF = FQ(I).PNUM(0)	!How many factors are listed in FQ(I)?
            IF (ALL(NPPOW(FQ(I).PNUM(1:NF))	!Can N (as NPPOW) be divided by Q (as FQ)?
     1       .GE.         FQ(I).PPOW(1:NF))) THEN	!By comparing the supplies of prime factors.
              FRACTRAN = I			!Yes!
              NPPOW(FQ(I).PNUM(1:NF)) = NPPOW(FQ(I).PNUM(1:NF))	!Remove prime powers from N
     1            - FQ(I).PPOW(1:NF)				!Corresponding to Q.
              NF = FP(I).PNUM(0)				!Add powers to N
              NPPOW(FP(I).PNUM(1:NF)) = NPPOW(FP(I).PNUM(1:NF))	!Corresponding to P.
     1            + FP(I).PPOW(1:NF)				!Thus, N = N/Q*P.
             RETURN			!That's all it takes! No multiplies nor divides!
            END IF		!So much for that fraction.
          END DO		!This relies on ALL(zero tests) yielding true, as when Q = 1.
          FRACTRAN = 0		!No hit.
        END FUNCTION FRACTRAN	!No massive multi-precision arithmetic!

        SUBROUTINE SHOWN(S,F)	!Service routine to show the state after a step is calculated.
Could imaging a function I6FMT(23) that returns "    23" and "      " for non-positive numbers.
Can't do it, as if this were invoked via a WRITE statement, re-entrant use of WRITE usually fails.
         INTEGER S,F	!Step number, Fraction number.
         INTEGER I	!A stepper.
         CHARACTER*(9+4+1 + NL*6) ALINE	!A scratchpad matching FORMAT 103.
          WRITE (ALINE,103) S,F,NPPOW(PLIVE(1:NL))	!Show it!
  103     FORMAT (I9,I4,":",<NL>I6)	!As a sequence of powers of primes.
          IF (F.LE.0) ALINE(10:13) = ""	!Scrub when no fraction is fingered.
          DO I = 1,NL		!Step along the live primes.
            IF (NPPOW(PLIVE(I)).GT.0) CYCLE	!Ignoring the empowered ones.
            ALINE(15 + (I - 1)*6:14 + I*6) = ""	!Blank out zero powers.
          END DO		!On to the next.
          WRITE (MSG,"(A)") ALINE	!Reveal at last.
        END SUBROUTINE SHOWN	!A struggle.
      END MODULE CONWAYSIDEA	!Simple...

      PROGRAM POKE
      USE CONWAYSIDEA	!But, where does he get his ideas from?
      INTEGER P(ENUFF),Q(ENUFF)	!Holds the fractions as P(i)/Q(i).
      INTEGER N		!The working number.
      INTEGER LF	!Last fraction given.
      INTEGER LP	!Last prime needed.
      INTEGER MS	!Maximum number of steps.
      INTEGER I,IT	!Assistants.
      LOGICAL*1 PUSED(ENUFF)	!Track the usage of prime numbers,

      MSG = 6		!Standard output.
      WRITE (6,1)	!Announce.
    1 FORMAT ("Interpreter for J. H. Conway's FRACTRAN language.")

Chew into an example programme.
   10 OPEN (10,FILE = "Fractran.txt",STATUS="OLD",ACTION="READ")	!Rather than compiled-in stuff.
      READ (10,*) LF		!I need to know this without having to scan the input.
      WRITE (MSG,11) LF		!Reveal in case of trouble.
   11 FORMAT (I0," fractions, as follow:")	!Should the input evoke problems.
      READ (10,*)    (P(I),Q(I),I = 1,LF)	!Ask for the specified number of P,Q pairs.
      WRITE (MSG,12) (P(I),Q(I),I = 1,LF)	!Show what turned up.
   12 FORMAT (24(I0,"/",I0:", "))	!As P(i)/Q(i) pairs. The colon means that there will be no trailing comma.
      READ (10,*) N,MS			!The start value, and the step limit.
      CLOSE (10)			!Finished with input.
      WRITE (MSG,13) N,MS		!Hopefully, all went well.
   13 FORMAT ("Start with N = ",I0,", step limit ",I0)
      IF (.NOT.GRASPPRIMEBAG(66)) STOP "Gan't grab my file of primes!"	!Attempt in hope.

Convert the starting number to a more convenient form, an array of powers of successive prime numbers.
   20 FP(1) = FACTOR(N)		!Borrow one of the factor list variables.
      NPPOW = 0			!Clear all prime factor counts.
      DO I = 1,FP(1).PNUM(0)	!Now find what they are.
        NPPOW(FP(1).PNUM(I)) = FP(1).PPOW(I)	!Convert from a variable-length list
      END DO			!To a fixed-length random-access array.
      PUSED = NPPOW.GT.0	!Note which primes have been used.
      LP = FP(1).PNUM(FP(1).PNUM(0))	!Recall the last prime required. More later.
Convert the supplied P(i)/Q(i) fractions to lists of prime number factors and powers in FP(i) and FQ(i).
      DO I = 1,LF	!Step through the fractions.
        IT = GCD(P(I),Q(I))	!Suspicion.
        IF (IT.GT.1) THEN	!Justified?
          WRITE (MSG,21) I,P(I),Q(I),IT	!Alas. Complain. The rule is N*(P/Q) being integer.
   21     FORMAT ("Fraction ",I3,", ",I0,"/",I0,!N*6/3 is integer always because this is N*2/1, but 3 may not divide N.
     1     " has common factor ",I0,"!")	!By removing IT,
          P(I) = P(I)/IT			!The test need merely check if N is divisible by Q.
          Q(I) = Q(I)/IT			!And, as N is factorised in NPPOW
        END IF					!And Q in FQ, subtractions of powers only is needed.
        FP(I) = FACTOR(P(I))		!Righto, form the factor list for P.
        PUSED(FP(I).PNUM(1:FP(I).PNUM(0))) = .TRUE.	!Mark which primes it fingers.
        LP = MAX(LP,FP(I).PNUM(FP(I).PNUM(0)))		!One has no prime factors: PNUM(0) = 0.
        FQ(I) = FACTOR(Q(I))		!And likewise for Q.
        PUSED(FQ(I).PNUM(1:FQ(I).PNUM(0))) = .TRUE.	!Some primes may be omitted.
        LP = MAX(LP,FQ(I).PNUM(FQ(I).PNUM(0)))		!If no prime factors, PNUM(0) fingers element zero, which is zero.
      END DO		!All this messing about saves on multiplication and division.
Check which primes are in use, preparing an index of live primes..
      NL = 0		!No live primes.
      DO I = 1,LP	!Check up to the last prime.
        IF (PUSED(I)) THEN	!This one used?
          NL = NL + 1		!Yes. Another.
          PLIVE(NL) = I		!Fingered.
        END IF		!So much for that prime.
      END DO		!On to the next.
      WRITE (MSG,22) NL,LP,PRIME(LP)	!Remark on usage.
   22 FORMAT ("Require ",I0," primes only, up to Prime(",I0,") = ",I0)	!Presume always more than one prime.
      IF (LP.GT.LASTP) STOP "But, that's too many for array NPPOW!"

Cast forth a heading.
  100 WRITE (MSG,101) (PRIME(PLIVE(I)), I = 1,NL)	!Splat a heading.
  101 FORMAT (/,14X,"N as powers of prime factors",/,	!The prime heading,
     1 5X,"Step  F#:",<LP>I6)		!With primes beneath.
      CALL SHOWN(0,0)	!Initial state of N as NPPOW. Step zero, no fraction.

Commence!
      DO I = 1,MS	!Here we go!
        IT = FRACTRAN(LF)	!Do it!
        CALL SHOWN(I,IT)	!Show it!
        IF (IT.LE.0) EXIT	!Quit it?
      END DO		!The next step.
Complete!
      END	!Whee!

```



### Revised Results

Edited in are >>> markers for the prime powers of two.

```txt

Interpreter for J. H. Conway's FRACTRAN language.
14 fractions, as follow:
17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1
Start with N = 2, step limit 100
Require 10 primes only, up to Prime(10) = 29

              N as powers of prime factors
     Step  F#:     2     3     5     7    11    13    17    19    23    29
        0    :     1
        1  13:           1     1
        2  14:           1     2           1
        3   5:                 2                                         1
        4   6:                 2     1     1
        5  11:                 2     1           1
        6   1:                 2                       1
        7   2:     1     1     1                 1
        8  10:     1     1     1           1
        9   5:     1           1                                         1
       10   6:     1           1     1     1
       11  11:     1           1     1           1
       12   1:     1           1                       1
       13   2:     2     1                       1
       14  10:     2     1                 1
       15   5:     2                                                     1
       16   6:     2                 1     1
       17  11:     2                 1           1
       18   1:     2                                   1
  >>>  19   9:     2
       20  13:     1     1     1
       21  13:           2     2
       22  14:           2     3           1
       23   5:           1     3                                         1
       24   6:           1     3     1     1
       25   5:                 3     1                                   1
       26   6:                 3     2     1
       27  11:                 3     2           1
       28   1:                 3     1                 1
       29   2:     1     1     2     1           1
       30   1:     1     1     2                       1
       31   2:     2     2     1                 1
       32  10:     2     2     1           1
       33   5:     2     1     1                                         1
       34   6:     2     1     1     1     1
       35   5:     2           1     1                                   1
       36   6:     2           1     2     1
       37  11:     2           1     2           1
       38   1:     2           1     1                 1
       39   2:     3     1           1           1
       40   1:     3     1                             1
       41   3:     3                                         1
       42   4:     2                                               1
       43   7:     2           1                             1
       44   4:     1           1                                   1
       45   7:     1           2                             1
       46   4:                 2                                   1
       47   7:                 3                             1
       48   8:                 3     1     1
       49  11:                 3     1           1
       50   1:                 3                       1
       51   2:     1     1     2                 1
       52  10:     1     1     2           1
       53   5:     1           2                                         1
       54   6:     1           2     1     1
       55  11:     1           2     1           1
       56   1:     1           2                       1
       57   2:     2     1     1                 1
       58  10:     2     1     1           1
       59   5:     2           1                                         1
       60   6:     2           1     1     1
       61  11:     2           1     1           1
       62   1:     2           1                       1
       63   2:     3     1                       1
       64  10:     3     1                 1
       65   5:     3                                                     1
       66   6:     3                 1     1
       67  11:     3                 1           1
       68   1:     3                                   1
  >>>  69   9:     3
       70  13:     2     1     1
       71  13:     1     2     2
       72  13:           3     3
       73  14:           3     4           1
       74   5:           2     4                                         1
       75   6:           2     4     1     1
       76   5:           1     4     1                                   1
       77   6:           1     4     2     1
       78   5:                 4     2                                   1
       79   6:                 4     3     1
       80  11:                 4     3           1
       81   1:                 4     2                 1
       82   2:     1     1     3     2           1
       83   1:     1     1     3     1                 1
       84   2:     2     2     2     1           1
       85   1:     2     2     2                       1
       86   2:     3     3     1                 1
       87  10:     3     3     1           1
       88   5:     3     2     1                                         1
       89   6:     3     2     1     1     1
       90   5:     3     1     1     1                                   1
       91   6:     3     1     1     2     1
       92   5:     3           1     2                                   1
       93   6:     3           1     3     1
       94  11:     3           1     3           1
       95   1:     3           1     2                 1
       96   2:     4     1           2           1
       97   1:     4     1           1                 1
       98   3:     4                 1                       1
       99   4:     3                 1                             1
      100   7:     3           1     1                       1

```

This time, restricting output to only occasions when N is a power of two requires no peculiar bit-counting function. Just change the interpretation loop to
```Fortran
      DO I = 1,MS	!Here we go!
        IT = FRACTRAN(LF)	!Do it!
        IF (ALL(NPPOW(2:LP).EQ.0)) CALL SHOWN(I,IT)	!Show it!
        IF (IT.LE.0) EXIT	!Quit it?
      END DO		!The next step.
```


Output:

```txt

Interpreter for J. H. Conway's FRACTRAN language.
14 fractions, as follow:
17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1
Start with N = 2, step limit 6666666
Require 10 primes only, up to Prime(10) = 29

              N as powers of prime factors
     Step  F#:     2     3     5     7    11    13    17    19    23    29
        0    :     1
       19   9:     2
       69   9:     3
      280   9:     5
      707   9:     7
     2363   9:    11
     3876   9:    13
     8068   9:    17
    11319   9:    19
    19201   9:    23
    36866   9:    29
    45551   9:    31
    75224   9:    37
   101112   9:    41
   117831   9:    43
   152025   9:    47
   215384   9:    53
   293375   9:    59
   327020   9:    61
   428553   9:    67
   507519   9:    71
   555694   9:    73
   700063   9:    79
   808331   9:    83
   989526   9:    89
  1273490   9:    97
  1434366   9:   101
  1530213   9:   103
  1710923   9:   107
  1818254   9:   109
  2019962   9:   113
  2833089   9:   127
  3104685   9:   131
  3546320   9:   137
  3720785   9:   139
  4549718   9:   149
  4755581   9:   151
  5329874   9:   157
  5958403   9:   163
  6400897   9:   167
```

Execution took about two seconds.


### Add and Multiply

Examples taken from Wikipaedia...

```txt

Interpreter for J. H. Conway's FRACTRAN language.
1 fraction, as follows:
3/2
Start with N = 72, step limit 66
Require 2 primes only, up to Prime(2) = 3

              N as powers of prime factors
     Step  F#:     2     3
        0    :     3     2
        1   1:     2     3
        2   1:     1     4
        3   1:           5
        4    :           5

```

The initial value is 72 = 2<sup>3</sup>x3<sup>2</sup> so that "register" two holds 3 and register three holds 2. On completion, register three holds 5, the sum of 2 and 3.

```txt

Interpreter for J. H. Conway's FRACTRAN language.
6 fractions, as follow:
455/33, 11/13, 1/11, 3/7, 11/2, 1/3
Start with N = 72, step limit 66
Require 6 primes only, up to Prime(6) = 13

              N as powers of prime factors
     Step  F#:     2     3     5     7    11    13
        0    :     3     2
        1   5:     2     2                 1
        2   1:     2     1     1     1           1
        3   2:     2     1     1     1     1
        4   1:     2           2     2           1
        5   2:     2           2     2     1
        6   3:     2           2     2
        7   4:     2     1     2     1
        8   4:     2     2     2
        9   5:     1     2     2           1
       10   1:     1     1     3     1           1
       11   2:     1     1     3     1     1
       12   1:     1           4     2           1
       13   2:     1           4     2     1
       14   3:     1           4     2
       15   4:     1     1     4     1
       16   4:     1     2     4
       17   5:           2     4           1
       18   1:           1     5     1           1
       19   2:           1     5     1     1
       20   1:                 6     2           1
       21   2:                 6     2     1
       22   3:                 6     2
       23   4:           1     6     1
       24   4:           2     6
       25   6:           1     6
       26   6:                 6
       27    :                 6

```

Here, register two holds 3 and register three holds 2. Their product appears in register five.


## FreeBASIC

Added a compiler condition to make the program work with the old GMP.bi header file

```FreeBasic
' version 06-07-2015
' compile with: fbc -s console
' uses gmp

#Include Once "gmp.bi"

' in case the two #define's are missing from 'gmp.bi' define them now
#Ifndef mpq_numref
    #Define mpq_numref(Q) (@(Q)->_mp_num)
    #Define mpq_denref(Q) (@(Q)->_mp_den)
#EndIf

Dim As String prog(0 To ...) = {"17/91", "78/85", "19/51", "23/38", "29/33",_
"77/29", "95/23", "77/19", "1/17", "11/13", "13/11", "15/14", "15/2", "55/1"}

Dim As UInteger i, j, c, max = UBound(prog)
Dim As Integer scanbit

Dim As ZString Ptr gmp_str : gmp_str = Allocate(10000)
Dim As Mpq_ptr  in_, out_
in_ = Allocate(Len(__mpq_struct)) : Mpq_init(in_)
out_ = Allocate(Len(__mpq_struct)) : Mpq_init(out_)
Dim As mpz_ptr num, den
num = Allocate(Len(__mpz_struct)) : Mpz_init(num)
den = Allocate(Len(__mpz_struct)) : Mpz_init(den)

Dim As mpq_ptr instruction(max)
For i = 0 To max
    instruction(i) = Allocate(Len(__mpq_struct))
    mpq_init(instruction(i))
    mpq_set_str(instruction(i), prog(i), 10 )
Next

mpq_set_str(in_ ,"2",10)
i = 0 : j = 0
Print "2";
Do
    mpq_mul(out_, instruction(i), in_)
    i = i + 1
    den = mpq_denref(out_)
    If mpz_cmp_ui(den, 1) = 0 Then
        Mpq_get_str(gmp_str, 10, out_)
        Print ", ";*gmp_str;
        mpq_swap(in_, out_)
        i = 0
        j = j + 1
    End If
Loop Until j > 14

' this one only display if the integer is 2^p, p being prime
mpq_set_str(in_ ,"2",10)
i = 0 : j = 0 : c = 0
Print : Print : Print
Print "count          iterations    prime  2^prime"

Do
    mpq_mul(out_, instruction(i), in_)
    i = i + 1
    j = j + 1
    den = mpq_denref(out_)
    If mpz_cmp_ui(den, 1) = 0 Then
        num = mpq_numref(out_)
        scanbit =  mpz_scan1(num, 0)
        ' if scanbit = 0 then number is odd
        If scanbit > 0 Then
            ' return from mpz_scan1(num, scanbit+1) is -1 for power of 2
            If mpz_scan1(num, scanbit +1) = -1 Then
                If c <= 20 Then Mpq_get_str(gmp_str, 10, out_) Else *gmp_str = ""
                c = c + 1
                Print Using "##### ################### ########  "; c; j; scanbit;
                Print *gmp_str
                If InKey <> "" Then Exit Do
            End If
        End If
        mpq_swap(in_, out_)
        i = 0
    End If
Loop

' Loop Until scanbit > 300
' Loop Until InKey <> ""
' Loop Until scanbit > 300 Or InKey <> ""
' stopping conditions will slow down the hole loop
' loop will check for key if it's printing a result

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132, 116

count          iterations    prime  2^prime
    1                 129        2  4
    2                 425        3  8
    3                1563        5  32
    4                3735        7  128
    5               11674       11  2048
    6               18811       13  8192
    7               38010       17  131072
    8               52854       19  524288
    9               88134       23  8388608
   10              166070       29  536870912
   11              204575       31  2147483648
   12              333931       37  137438953472
   13              446506       41  2199023255552
   14              519556       43  8796093022208
   15              667496       47  140737488355328
   16              940183       53  9007199254740992
   17             1274660       59  576460752303423488
   18             1419935       61  2305843009213693952
   19             1853979       67  147573952589676412928
   20             2191673       71  2361183241434822606848
shorten output file
   42            34533967      181
   43            40326168      191
```



## Go

Basic task:  This compiles to produce a program that reads the limit, starting number n, and list of fractions as command line arguments, with the list of fractions as a single argument.

```go
package main

import (
    "fmt"
    "log"
    "math/big"
    "os"
    "strconv"
    "strings"
)

func compile(src string) ([]big.Rat, bool) {
    s := strings.Fields(src)
    r := make([]big.Rat, len(s))
    for i, s1 := range s {
        if _, ok := r[i].SetString(s1); !ok {
            return nil, false
        }
    }
    return r, true
}

func exec(p []big.Rat, n *big.Int, limit int) {
    var q, r big.Int
rule:
    for i := 0; i < limit; i++ {
        fmt.Printf("%d ", n)
        for j := range p {
            q.QuoRem(n, p[j].Denom(), &r)
            if r.BitLen() == 0 {
                n.Mul(&q, p[j].Num())
                continue rule
            }
        }
        break
    }
    fmt.Println()
}

func usage() {
    log.Fatal("usage: ft <limit> <n> <prog>")
}

func main() {
    if len(os.Args) != 4 {
        usage()
    }
    limit, err := strconv.Atoi(os.Args[1])
    if err != nil {
        usage()
    }
    var n big.Int
    _, ok := n.SetString(os.Args[2], 10)
    if !ok {
        usage()
    }
    p, ok := compile(os.Args[3])
    if !ok {
        usage()
    }
    exec(p, &n, limit)
}
```

```txt

> ft 15 2 "17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1"
2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132

```

Extra credit:  This invokes above program with appropriate arguments,
and processes the output to obtain the 20 primes.

```go
package main

import (
    "fmt"
    "log"
    "math/big"
    "os"
    "os/exec"
)

func main() {
    c := exec.Command("ft", "1000000", "2", `17/91 78/85 19/51 23/38
        29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1`)
    c.Stderr = os.Stderr
    r, err := c.StdoutPipe()
    if err != nil {
        log.Fatal(err)
    }
    if err = c.Start(); err != nil {
        log.Fatal(err)
    }
    var n big.Int
    for primes := 0; primes < 20; {
        if _, err = fmt.Fscan(r, &n); err != nil {
            log.Fatal(err)
        }
        l := n.BitLen() - 1
        n.SetBit(&n, l, 0)
        if n.BitLen() == 0 && l > 1 {
            fmt.Printf("%d ", l)
            primes++
        }
    }
    fmt.Println()
}
```

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71

```



## Haskell


### Running the program


```haskell
import Data.List (find)
import Data.Ratio (Ratio, (%), denominator)

fractran :: (Integral a) => [Ratio a] -> a -> [a]
fractran fracts n = n :
  case find (\f -> n `mod` denominator f == 0) fracts of
    Nothing -> []
    Just f -> fractran fracts $ truncate (fromIntegral n * f)
```


Example:

```txt
λ> let prog = [17 % 91,78 % 85,19 % 51,23 % 38,29 % 33,77 % 29,95 % 23,77 % 19,1 % 17,11 % 13,13 % 11,15 % 14,15 % 2,55 % 1]

λ> take 15 $ fractran prog 2
[2,15,825,725,1925,2275,425,390,330,290,770,910,170,156,132]
```



### Reading the program

Additional import

```Haskell
import Data.List.Split (splitOn)
```


```Haskell
readProgram :: String -> [Ratio Int]
readProgram = map (toFrac . splitOn "/") . splitOn ","
  where toFrac [n,d] = read n % read d
```


Example of running the program:

```txt
λ> let prog = readProgram "17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23 , 77/19,  1/17, 11/13, 13/11, 15/14,  15/2, 55/1"

λ> take 15 $ fractran prog 2
[2,15,825,725,1925,2275,425,390,330,290,770,910,170,156,132]
```



### Generation of primes

Additional import

```Haskell
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)
```


```Haskell
primes :: [Int]
primes = mapMaybe log2 $ fractran prog 2
  where
    prog =
      [ 17 % 91
      , 78 % 85
      , 19 % 51
      , 23 % 38
      , 29 % 33
      , 77 % 29
      , 95 % 23
      , 77 % 19
      , 1 % 17
      , 11 % 13
      , 13 % 11
      , 15 % 14
      , 15 % 2
      , 55 % 1
      ]
    log2 = fmap succ . elemIndex 2 . takeWhile even . iterate (`div` 2)
```



```txt
λ> take 20 primes
[1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67]
```


=={{header|Icon}} and {{header|Unicon}}==
Works in both languages:


```unicon
record fract(n,d)

procedure main(A)
    fractran("17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1", 2)
end

procedure fractran(s, n, limit)
    execute(parse(s),n, limit)
end

procedure parse(s)
    f := []
    s ? while not pos(0) do {
            tab(upto(' ')|0) ? put(f,fract(tab(upto('/')), (move(1),tab(0))))
            move(1)
            }
    return f
end

procedure execute(f,d,limit)
     /limit := 15
     every !limit do {
         if d := (d%f[i := !*f].d == 0, (writes(" ",d)/f[i].d)*f[i].n) then {}
         else break write()
         }
     write()
end
```


```txt

->fractan
 2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132
->

```



## J



### Hybrid version

'''Solution:'''

```j
toFrac=: '/r' 0&".@charsub ]                           NB. read fractions from string
fractran15=: ({~ (= <.) i. 1:)@(toFrac@[ * ]) ^:(<15)  NB. return first 15 Fractran results
```


'''Example:'''

```j
   taskstr=: '17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1'
   taskstr fractran15 2
2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132
```



### Tacit version


'''Solution'''

This is a variation of the previous solution which it is not entirely tacit due to the use of the explicit standard library verb (function) charsub.  The adverb (functional) fractan is defined as a fixed tacit adverb (that is, a stateless point-free functional),


```j
fractan=. (((({~ (1 i.~ (= <.)))@:* ::]^:)(`]))(".@:('1234567890r ' {~ '1234567890/ '&i.)@:[`))(`:6)
```


The argument of fractan specifies a limit for the number of steps; if the limit is boxed the intermediate results are also included in the result.

'''Example'''


```j
   '17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1' (<15) fractan 2
2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132
```



'''Extra credit'''

The prime numbers are produced via the adverb primes; its argument has the same specifications as the argument for the fractan adverb (which is used in its definition),


```j
primes=. ('fractan'f.) ((1 }. 2 ^. (#~ *./@:e.&2 0"1@:q:))@:)

   '17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1' (<555555) primes 2
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
```


primes is also a stateless point-free functional,


```j
   primes
((((({~ (1 i.~ (= <.)))@:* ::]^:)(`]))(".@:('1234567890r ' {~ '1234567890/ '&i.)@:[`))(`:6))((1 }. 2 ^. (#~ *./@:e.&2 0"1@:q:))@:)
```



'''Turing completeness of J's stateless point-free dialect'''

When _ is the limit argument (i.e., when no limit is imposed) the run will halt according to the FRACTAN programming language specifications (the run might also be forced to halt if a trivial changeless single cycle, induced by a useless 1/1 fraction, is detected).  Thus, the FRACTAN associated verb (function) is,


```j
   _ fractan
".@:('1234567890r ' {~ '1234567890/ '&i.)@:[ ({~ (1 i.~ (= <.)))@:* ::]^:_ ]
```


Actually, most of the code above is there to comply with the task's requirement of a "''natural'' format."  When J's format for fractions is used the FRACTAN verb becomes,


```j
FRACTAN=. ({~ (1 i.~ (= <.)))@:* ::]^:_
```


which is an indirect concise confirmation that J's fixed tacit dialect is Turing complete.

In the following example, FRACTAN calculates the product 4 * 6, the initial value 11664 = (2^4)*(3^6) holds 4 in the register associated with 2 and holds 6 in the register associated with 3; the result 59604644775390625 = 5^24 holds the product 24 = 4 * 6 in the register associated with 5,


```j
   455r33 11r13 1r11 3r7 11r2 1r3 FRACTAN 11664
59604644775390625
```



## Java


```java
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Fractran{

   public static void main(String []args){

       new Fractran("17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1", 2);
   }
   final int limit = 15;


   Vector<Integer> num = new Vector<>();
   Vector<Integer> den = new Vector<>();
   public Fractran(String prog, Integer val){
      compile(prog);
      dump();
      exec(2);
    }


   void compile(String prog){
      Pattern regexp = Pattern.compile("\\s*(\\d*)\\s*\\/\\s*(\\d*)\\s*(.*)");
      Matcher matcher = regexp.matcher(prog);
      while(matcher.find()){
         num.add(Integer.parseInt(matcher.group(1)));
         den.add(Integer.parseInt(matcher.group(2)));
         matcher = regexp.matcher(matcher.group(3));
      }
   }

   void exec(Integer val){
       int n = 0;
       while(val != null && n<limit){
           System.out.println(n+": "+val);
           val = step(val);
           n++;
       }
   }
   Integer step(int val){
       int i=0;
       while(i<den.size() && val%den.get(i) != 0) i++;
       if(i<den.size())
           return num.get(i)*val/den.get(i);
       return null;
   }

   void dump(){
       for(int i=0; i<den.size(); i++)
           System.out.print(num.get(i)+"/"+den.get(i)+" ");
       System.out.println();
   }
}
```



## JavaScript


### Imperative


```javascript
// Parses the input string for the numerators and denominators
function compile(prog, numArr, denArr) {
    let regex = /\s*(\d*)\s*\/\s*(\d*)\s*(.*)/m;
    let result;
    while (result = regex.exec(prog)) {
        numArr.push(result[1]);
        denArr.push(result[2]);
        prog = result[3];
    }
    return [numArr, denArr];
}

// Outputs the result of the compile stage
function dump(numArr, denArr) {
    let output = "";
    for (let i in numArr) {
        output += `${numArr[i]}/${denArr[i]} `;
    }
    return `${output}
`;
}

// Step
function step(val, numArr, denArr) {
    let i = 0;
    while (i < denArr.length && val % denArr[i] != 0) i++;
    return numArr[i] * val / denArr[i];
}

// Executes Fractran
function exec(val, i, limit, numArr, denArr) {
    let output = "";
    while (val && i < limit) {
        output += `${i}: ${val}
`;
        val = step(val, numArr, denArr);
        i++;
    }
    return output;
}

// Main
// Outputs to DOM (clears and writes at the body tag)
let body = document.body;
let [num, den] = compile("17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1", [], []);
body.innerHTML = dump(num, den);
body.innerHTML += exec(2, 0, 15, num, den);
```



### Functional

Here is a functionally composed version, which also derives a few primes. I may have missed something, but this first draft suggests that we may need bigInt support (which JS lacks) to get as far as the sixth prime.

```javascript
(() => {
    'use strict';

    // fractran :: [Ratio Int] -> Int -> Gen [Int]
    const fractran = (xs, n) => {
        function* go(n) {
            const p = r => 0 === v % r.d;
            let
                v = n,
                mb = find(p, xs);
            yield v
            while (!mb.Nothing) {
                mb = bindMay(
                    find(p, xs),
                    r => (
                        v = truncate({
                            type: 'Ratio',
                            n: v * r.n,
                            d: r.d
                        }),
                        Just(v)
                    )
                );
                mb.Just && (yield v)
            }
        };
        return go(n);
    };

    // readRatios :: String -> [Ratio]
    const readRatios = s =>
        map(x => ratio(...map(read, splitOn('/', x))),
            splitOn(',', s)
        );

    // main :: IO()
    const main = () => {

        // strRatios :: String
        const strRatios = `17/91, 78/85, 19/51, 23/38, 29/33, 77/29,
                    95/23 , 77/19,  1/17, 11/13, 13/11, 15/14,  15/2, 55/1`;

        showLog(
            'First fifteen steps:',
            take(15,
                fractran(readRatios(strRatios), 2)
            )
        );

        showLog(
            'First five primes:',
            take(5,
                mapMaybeGen(
                    x => fmapMay(
                        succ,
                        elemIndex(
                            2,
                            takeWhileGen(
                                even,
                                iterate(n => div(n, 2), x)
                            )
                        )
                    ),
                    fractran(readRatios(strRatios), 2)
                )
            )
        );
    };

    // GENERIC ABSTRACTIONS ----------------------------

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

    // abs :: Num -> Num
    const abs = Math.abs;

    // bindMay (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    const bindMay = (mb, mf) =>
        mb.Nothing ? mb : mf(mb.Just);

    // div :: Int -> Int -> Int
    const div = (x, y) => Math.floor(x / y);

    // elemIndex :: Eq a => a -> [a] -> Maybe Int
    const elemIndex = (x, xs) => {
        const i = xs.indexOf(x);
        return -1 === i ? (
            Nothing()
        ) : Just(i);
    };

    // even :: Int -> Bool
    const even = n => 0 === n % 2;

    // find :: (a -> Bool) -> [a] -> Maybe a
    const find = (p, xs) => {
        for (let i = 0, lng = xs.length; i < lng; i++) {
            if (p(xs[i])) return Just(xs[i]);
        }
        return Nothing();
    };

    // fmapMay (<$>) :: (a -> b) -> Maybe a -> Maybe b
    const fmapMay = (f, mb) =>
        mb.Nothing ? (
            mb
        ) : Just(f(mb.Just));

    // foldl :: (a -> b -> a) -> a -> [b] -> a
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // gcd :: Int -> Int -> Int
    const gcd = (x, y) => {
        const
            _gcd = (a, b) => (0 === b ? a : _gcd(b, a % b)),
            abs = Math.abs;
        return _gcd(abs(x), abs(y));
    };

    // iterate :: (a -> a) -> a -> Gen [a]
    function* iterate(f, x) {
        let v = x;
        while (true) {
            yield(v);
            v = f(v);
        }
    }

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // mapMaybeGen :: (a -> Maybe b) -> Gen [a] -> [b]
    function* mapMaybeGen(mf, gen) {
        let v = take(1, gen);
        while (0 < v.length) {
            let mb = mf(v[0]);
            if (!mb.Nothing) yield mb.Just
            v = take(1, gen);
        }
    }

    // properFracRatio :: Ratio -> (Int, Ratio)
    const properFracRatio = nd => {
        const [q, r] = Array.from(quotRem(nd.n, nd.d));
        return Tuple(q, ratio(r, nd.d));
    };

    // quot :: Int -> Int -> Int
    const quot = (n, m) => Math.floor(n / m);

    // quotRem :: Int -> Int -> (Int, Int)
    const quotRem = (m, n) =>
        Tuple(Math.floor(m / n), m % n);

    // ratio :: Int -> Int -> Ratio Int
    const ratio = (n, d) =>
        0 !== d ? (() => {
            const g = gcd(n, d);
            return {
                type: 'Ratio',
                'n': quot(n, g), // numerator
                'd': quot(d, g) // denominator
            }
        })() : undefined;

    // read :: Read a => String -> a
    const read = JSON.parse;

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // splitOn :: [a] -> [a] -> [[a]]
    // splitOn :: String -> String -> [String]
    const splitOn = (pat, src) =>
        src.split(pat);

    // succ :: Int -> Int
    const succ = x =>
        1 + x;

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        xs.constructor.constructor.name !== 'GeneratorFunction' ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // takeWhileGen :: (a -> Bool) -> Gen [a] -> [a]
    const takeWhileGen = (p, xs) => {
        const ys = [];
        let
            nxt = xs.next(),
            v = nxt.value;
        while (!nxt.done && p(v)) {
            ys.push(v);
            nxt = xs.next();
            v = nxt.value
        }
        return ys;
    };

    // truncate :: Num -> Int
    const truncate = x =>
        'Ratio' === x.type ? (
            properFracRatio(x)[0]
        ) : properFraction(x)[0];

    // MAIN ---
    return main();
})();
```

```txt
"First fifteen steps:" -> [2,15,825,725,1925,2275,425,390,330,290,770,910,170,156,132]
"First five primes:" -> [1,2,3,5,7]
```



## Julia

```julia
function fractran(n::Integer, ratios::Vector{<:Rational}, steplim::Integer)
    rst = zeros(BigInt, steplim)
    for i in 1:steplim
        rst[i] = n
        if (pos = findfirst(x -> isinteger(n * x), ratios)) > 0
            n *= ratios[pos]
        else
            break
        end
    end
    return rst
end

using IterTools
macro ratio_str(s)
    a = split(s, r"[\s,/]+")
    return collect(parse(BigInt, n) // parse(BigInt, d) for (n, d) in partition(a, 2))
end

fracs = ratio"""17 / 91, 78 / 85, 19 / 51, 23 / 38, 29 / 33, 77 / 29, 95 / 23,
                77 / 19, 1 / 17, 11 / 13, 13 / 11, 15 / 14, 15 / 2, 55 / 1"""
println("The first 20 in the series are ", fractran(2, fracs, 20))

prmfound = 0
n = big(2)
while prmfound < 20
    if isinteger(log2(n))
        prmfound += 1
        println("Prime $prmfound found: $n is 2 ^ $(Int(log2(n)))")
    end
    n = fractran(n, fracs, 2)[2]
end
```


```txt
The first 20 in the series are BigInt[2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132, 116, 308, 364, 68, 4]
Prime 1 found: 2 is 2 ^ 1
Prime 2 found: 4 is 2 ^ 2
Prime 3 found: 8 is 2 ^ 3
Prime 4 found: 32 is 2 ^ 5
Prime 5 found: 128 is 2 ^ 7
Prime 6 found: 2048 is 2 ^ 11
Prime 7 found: 8192 is 2 ^ 13
Prime 8 found: 131072 is 2 ^ 17
Prime 9 found: 524288 is 2 ^ 19
Prime 10 found: 8388608 is 2 ^ 23
Prime 11 found: 536870912 is 2 ^ 29
Prime 12 found: 2147483648 is 2 ^ 31
Prime 13 found: 137438953472 is 2 ^ 37
Prime 14 found: 2199023255552 is 2 ^ 41
Prime 15 found: 8796093022208 is 2 ^ 43
Prime 16 found: 140737488355328 is 2 ^ 47
Prime 17 found: 9007199254740992 is 2 ^ 53
Prime 18 found: 576460752303423488 is 2 ^ 59
Prime 19 found: 2305843009213693952 is 2 ^ 61
Prime 20 found: 147573952589676412928 is 2 ^ 67
```



## Kotlin


```scala
// version 1.1.3

import java.math.BigInteger

class Fraction(val num: BigInteger, val denom: BigInteger) {
    operator fun times(n: BigInteger) = Fraction (n * num, denom)

    fun isIntegral() = num % denom == BigInteger.ZERO
}

fun String.toFraction(): Fraction {
    val split = this.split('/')
    return Fraction(BigInteger(split[0]), BigInteger(split[1]))
}

val BigInteger.isPowerOfTwo get() = this.and(this - BigInteger.ONE) == BigInteger.ZERO

val log2 = Math.log(2.0)

fun fractran(program: String, n: Int, limit: Int, primesOnly: Boolean): List<Int> {
    val fractions = program.split(' ').map { it.toFraction() }
    val results = mutableListOf<Int>()
    if (!primesOnly) results.add(n)
    var nn = BigInteger.valueOf(n.toLong())
    while (results.size < limit) {
        val frac = fractions.find { (it * nn).isIntegral() } ?: break
        nn = nn * frac.num / frac.denom
        if (!primesOnly) {
           results.add(nn.toInt())
        }
        else if (primesOnly && nn.isPowerOfTwo) {
           val prime = (Math.log(nn.toDouble()) / log2).toInt()
           results.add(prime)
        }
    }
    return results
}

fun main(args: Array<String>) {
    val program = "17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1"
    println("First twenty numbers:")
    println(fractran(program, 2, 20, false))
    println("\nFirst twenty primes:")
    println(fractran(program, 2, 20, true))
}
```


```txt

First twenty numbers:
[2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132, 116, 308, 364, 68, 4]

First twenty primes:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
This isn't as efficient as possible for long lists of fractions, since it doesn't stop doing n*listelements once it finds an integer. Instead, it computes "is integer?" for n*{all list elements}. For short lists that's probably not a big deal.


```Mathematica
fractionlist = {17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1};
n = 2;
steplimit = 20;
j = 0;
break = False;
While[break == False && j <= steplimit,
 newlist = n fractionlist;
 isintegerlist = IntegerQ[#] & /@ newlist;
 truepositions = Position[isintegerlist, True];
 If[Length[truepositions] == 0,
  break = True,
  Print[ToString[j] <> ": " <> ToString[n]];
  n = newlist[[truepositions[[1, 1]]]]; j++;
  ]
 ]
```

```txt
0: 2
1: 15
2: 825
3: 725
4: 1925
5: 2275
6: 425
7: 390
8: 330
9: 290
10: 770
11: 910
12: 170
13: 156
14: 132
15: 116
16: 308
17: 364
18: 68
19: 4
20: 30
```



## OCaml

This reads a Fractran program from standard input (keyboard or file) and runs it with the input given by the command line arguments, using arbitrary-precision numbers and fractions.

```ocaml
open Num

let get_input () =
   num_of_int (
     try int_of_string Sys.argv.(1)
     with _ -> 10)

let get_max_steps () =
   try int_of_string Sys.argv.(2)
   with _ -> 50

let read_program () =
   let line = read_line () in
   let words = Str.split (Str.regexp " +") line in
   List.map num_of_string words

let is_int n = n =/ (integer_num n)

let run_program num prog =

   let replace n =
      let rec step = function
      | [] -> None
      | h :: t ->
            let n' = h */ n in
            if is_int n' then Some n' else step t in
      step prog in

   let rec repeat m lim =
      Printf.printf "  %s\n" (string_of_num m);
      if lim = 0 then print_endline "Reached max step limit" else
         match replace m with
         | None -> print_endline "Finished"
         | Some x -> repeat x (lim-1)
   in

   let max_steps = get_max_steps () in
   repeat num max_steps

let () =
   let num = get_input () in
   let prog = read_program () in
   run_program num prog
```


The program


```txt
17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1
```


saved in a file can can be run with the command:


```txt
ocaml nums.cma str.cma fractran.ml 2 30 < primes.fct
```

```txt
  2
  15
  825
  725
  1925
  2275
  425
  390
  330
  290
  770
  910
  170
  156
  132
  116
  308
  364
  68
  4
  30
  225
  12375
  10875
  28875
  25375
  67375
  79625
  14875
  13650
  2550
Reached max step limit

```



## PARI/GP


In this version ideas were borrowed from C++, Java and JavaScript.

```parigp

\\ FRACTRAN
\\ 4/27/16 aev
fractran(val,ft,lim)={
my(ftn=#ft,fti,di,L=List(),j=0);
while(val&&j<lim, listput(L,val);
  for(i=1,ftn, fti=ft[i]; di=denominator(fti);
      if(val%di==0, break));\\fend i
  val= numerator(fti)*val/di; j++);\\wend j
return(Vec(L));
}

{\\ Executing:
my(v=[17/91,78/85,19/51,23/38,29/33,77/29,95/23,77/19,1/17,11/13,13/11,15/14,15/2,55/1]);
print(fractran(2,v,15));
}

```


```txt

[2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132]

```



## Perl

Instead of printing all steps, I chose to only print those steps which were a power of two.
This makes the fact that it's a prime-number-generating program much clearer.


```perl
use strict;
use warnings;
use Math::BigRat;

my ($n, @P) = map Math::BigRat->new($_), qw{
2 17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1
};

$|=1;
MAIN: for( 1 .. 5000 ) {
	print " " if $_ > 1;
	my ($pow, $rest) = (0, $n->copy);
	until( $rest->is_odd ) {
		++$pow;
		$rest->bdiv(2);
	}
	if( $rest->is_one ) {
		print "2**$pow";
	} else {
		#print $n;
	}
	for my $f_i (@P) {
		my $nf_i = $n * $f_i;
		next unless $nf_i->is_int;
		$n = $nf_i;
		next MAIN;
	}
	last;
}

print "\n";

```


If you uncomment the
```txt
#print $n
```
, it will print all the steps.


## Perl 6

A Fractran program potentially returns an infinite list, and infinite lists are a common data structure in Perl 6.  The limit is therefore enforced only by slicing the infinite list.

```perl6
sub fractran(@program) {
    2, { first Int, map (* * $_).narrow, @program } ... 0
}
say fractran(<17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11
        15/14 15/2 55/1>)[^100];
```

```txt
(2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132 116 308 364 68 4 30 225 12375 10875 28875 25375 67375 79625 14875 13650 2550 2340 1980 1740 4620 4060 10780 12740 2380 2184 408 152 92 380 230 950 575 2375 9625 11375 2125 1950 1650 1450 3850 4550 850 780 660 580 1540 1820 340 312 264 232 616 728 136 8 60 450 3375 185625 163125 433125 380625 1010625 888125 2358125 2786875 520625 477750 89250 81900 15300 14040 11880 10440 27720 24360 64680 56840 150920 178360 33320 30576 5712 2128 1288)
```

'''Extra credit:'''
We can weed out all the powers of two into another infinite constant list based on the first list.  In this case the sequence is limited only by our patience, and a ^C from the terminal.  The <tt>.msb</tt> method finds the most significant bit of an integer, which conveniently is the base-2 log of the power-of-two in question.

```perl6
sub fractran(@program) {
    2, { first Int, map (* * $_).narrow, @program } ... 0
}
for fractran <17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11
        15/14 15/2 55/1> {
        say $++, "\t", .msb, "\t", $_ if 1 +< .msb == $_;
}
```

```txt

0       1       2
1	2	4
2	3	8
3	5	32
4	7	128
5	11	2048
6	13	8192
7	17	131072
8	19	524288
9	23	8388608
^C
```



## Phix

Using the same ideas as the Fortran entry (thanks!).

For example, suppose that known_factors happens to be {2,3,5}. [the exact order may vary]

2 is held as {1,0,0}, ie 2^1 * 3^0 * 5^0, and 15 as {0,1,1}, ie 2^0 * 3^1 * 5^1.

Notice that 2, being a power of 2, has zero in all slots other than 2.

We can say that 15 is not exactly divisible by 2 because the power of 2 is too large.

Division (to whole integer) is performed simply by subtracting the corresponding powers.


```Phix
constant steps = 20,
         primes = 20

sequence known_factors = {} -- nb: no specific order

function as_primes(integer n)
-- eg as_primes(55) -> {5,11} -> indexes to known_factors
    sequence pf = prime_factors(n,duplicates:=true)
    sequence res = repeat(0,length(known_factors))
    for i=1 to length(pf) do
        integer k = find(pf[i],known_factors)
        if k=0 then
            known_factors = append(known_factors,pf[i])
            res = append(res,1)
        else
            res[k] += 1
        end if
    end for
    return res
end function

function parse(string s)
    sequence res = split(s)
    for i=1 to length(res) do
        sequence sri = scanf(res[i],"%d/%d")
        if length(sri)!=1 then ?9/0 end if  -- oops!
        integer {{n,d}} = sri
        res[i] = {as_primes(n),as_primes(d)}
    end for
    return res
end function

function combine_factors(sequence n)
-- (inverse of as_primes)
    atom res = 1
    for i=1 to length(n) do
        if n[i]!=0 then
            res *= power(known_factors[i],n[i])
        end if
    end for
    return res
end function

function step(sequence pgm, sequence n)
    for pc=1 to length(pgm) do
        sequence d = pgm[pc][2], res = n
        bool ok = true
        for f=1 to length(d) do
            if d[f]!=0 then
                if f>length(n) or d[f]>res[f] then
                    ok = false
                    exit
                end if
                res[f] -= d[f]
            end if
        end for
        if ok then
            n = pgm[pc][1]
            for i=1 to length(n) do
                res[i] += n[i]
            end for
            return res
        end if
    end for
    return 0
end function

constant src = "17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1"
sequence pgm = parse(src)
object n = as_primes(2)
sequence res = {}
for i=1 to steps do
    n = step(pgm,n)
    if n=0 then exit end if
    res = append(res,combine_factors(n))
end for
printf(1,"first %d results: %s\n",{steps,sprint(res)})

n = as_primes(2)
integer k2 = find(2,known_factors)
sequence n0 = repeat(0,length(known_factors))
res = {}
integer iteration = 1
atom t0 = time()
while length(res)<primes do
    n = step(pgm,n)
    if n=0 then exit end if
    n0[k2] = n[k2]
    if n=n0 then -- (ie all non-2 are 0)
        -- and the prime itself is ready and waiting...
        res = append(res,n[k2])
    end if
    iteration += 1
end while
printf(1,"first %d primes: %s\n",{primes,sprint(res)})
printf(1,"%d iterations in %s\n",{iteration,elapsed(time()-t0)})
```

```txt

first 20 results: {15,825,725,1925,2275,425,390,330,290,770,910,170,156,132,116,308,364,68,4,30}
first 20 primes: {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71}
507520 iterations in 0.5s

```



## Python


### Python: Generate series from a fractran program


```python
from fractions import Fraction

def fractran(n, fstring='17 / 91, 78 / 85, 19 / 51, 23 / 38, 29 / 33,'
                        '77 / 29, 95 / 23, 77 / 19, 1 / 17, 11 / 13,'
                        '13 / 11, 15 / 14, 15 / 2, 55 / 1'):
    flist = [Fraction(f) for f in fstring.replace(' ', '').split(',')]

    n = Fraction(n)
    while True:
        yield n.numerator
        for f in flist:
            if (n * f).denominator == 1:
                break
        else:
            break
        n *= f

if __name__ == '__main__':
    n, m = 2, 15
    print('First %i members of fractran(%i):\n  ' % (m, n) +
          ', '.join(str(f) for f,i in zip(fractran(n), range(m))))
```


```txt
First 15 members of fractran(2):
  2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132
```



### =Python: Generate primes=

Use fractran above as a module imported into the following program.


```python

from fractran import fractran

def fractran_primes():
    for i, fr in enumerate(fractran(2), 1):
        binstr = bin(fr)[2:]
        if binstr.count('1') == 1:
            prime = binstr.count('0')
            if prime > 1:   # Skip 2**0 and 2**1
                yield prime, i

if __name__ == '__main__':
    for (prime, i), j in zip(fractran_primes(), range(15)):
        print("Generated prime %2i from the %6i'th member of the fractran series" % (prime, i))
```


```txt
Generated prime  2 from the     20'th member of the fractran series
Generated prime  3 from the     70'th member of the fractran series
Generated prime  5 from the    281'th member of the fractran series
Generated prime  7 from the    708'th member of the fractran series
Generated prime 11 from the   2364'th member of the fractran series
Generated prime 13 from the   3877'th member of the fractran series
Generated prime 17 from the   8069'th member of the fractran series
Generated prime 19 from the  11320'th member of the fractran series
Generated prime 23 from the  19202'th member of the fractran series
Generated prime 29 from the  36867'th member of the fractran series
Generated prime 31 from the  45552'th member of the fractran series
Generated prime 37 from the  75225'th member of the fractran series
Generated prime 41 from the 101113'th member of the fractran series
Generated prime 43 from the 117832'th member of the fractran series
Generated prime 47 from the 152026'th member of the fractran series
```



## Racket

{{trans|D}} Simple version, without sequences.

```Racket
#lang racket

(define (displaysp x)
  (display x)
  (display " "))

(define (read-string-list str)
  (map string->number
       (string-split (string-replace str " " "") ",")))

(define (eval-fractran n list)
  (for/or ([e (in-list list)])
    (let ([en (* e n)])
      (and (integer? en) en))))

(define (show-fractran fr n s)
  (printf "First ~a members of fractran(~a):\n" s n)
  (displaysp n)
  (for/fold ([n n]) ([i (in-range (- s 1))])
    (let ([new-n (eval-fractran n fr)])
      (displaysp new-n)
      new-n))
  (void))

(define fractran
  (read-string-list
   (string-append "17 / 91, 78 / 85, 19 / 51, 23 / 38, 29 / 33,"
                  "77 / 29, 95 / 23, 77 / 19, 1 / 17, 11 / 13,"
                  "13 / 11, 15 / 14, 15 / 2, 55 / 1")))

(show-fractran fractran 2 15)
```

```txt
First 15 members of fractran(2):
2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132
```



## REXX

Programming note:   extra blanks can be inserted in the fractions before and/or after the solidus   ['''<big>/</big>'''].

### showing all terms


```rexx
/*REXX program runs  FRACTRAN  for a given set of  fractions  and  from a specified  N. */
numeric digits 2000                              /*be able to handle larger numbers.    */
parse arg N terms fracs                          /*obtain optional arguments from the CL*/
if N==''     |     N==","  then     N= 2         /*Not specified?  Then use the default.*/
if terms=='' | terms==","  then terms= 100       /* "      "         "   "   "      "   */
if fracs=''                then fracs= '17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23,',
                                       '77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1'
                                                 /* [↑]  The default for the fractions. */
f= space(fracs, 0)                               /*remove all blanks from the FRACS list*/
                   do #=1  while f\=='';    parse var  f   n.#   '/'   d.#   ","   f
                   end   /*#*/                   /* [↑]  parse all the fractions in list*/
#= # - 1                                         /*the number of fractions just found.  */
say #   'fractions:'   fracs                     /*display number and actual fractions. */
say 'N  is starting at '   N                     /*display the starting number  N.      */
say terms   ' terms are being shown:'            /*display a kind of header/title.      */

    do     j=1  for  terms                       /*perform the DO loop for each   term. */
        do k=1  for  #                           /*   "     "   "   "   "    "  fraction*/
        if N // d.k \== 0  then iterate          /*Not an integer?  Then ignore it.     */
        say right('term' j, 35)    "──► "    N   /*display the  Nth  term  with the  N. */
        N= N  %  d.k  *  n.k                     /*calculate next term (use %≡integer ÷)*/
        iterate j                                /*go start calculating the next term.  */
        end   /*k*/                              /* [↑]  if an integer, we found a new N*/
    end       /*j*/                              /*stick a fork in it,  we're all done. */
```

<pre style="height:63ex">
14 fractions: 17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1
N  is starting at  2
100  terms are being shown:
                             term 1 ──►  2
                             term 2 ──►  15
                             term 3 ──►  825
                             term 4 ──►  725
                             term 5 ──►  1925
                             term 6 ──►  2275
                             term 7 ──►  425
                             term 8 ──►  390
                             term 9 ──►  330
                            term 10 ──►  290
                            term 11 ──►  770
                            term 12 ──►  910
                            term 13 ──►  170
                            term 14 ──►  156
                            term 15 ──►  132
                            term 16 ──►  116
                            term 17 ──►  308
                            term 18 ──►  364
                            term 19 ──►  68
                            term 20 ──►  4
                            term 21 ──►  30
                            term 22 ──►  225
                            term 23 ──►  12375
                            term 24 ──►  10875
                            term 25 ──►  28875
                            term 26 ──►  25375
                            term 27 ──►  67375
                            term 28 ──►  79625
                            term 29 ──►  14875
                            term 30 ──►  13650
                            term 31 ──►  2550
                            term 32 ──►  2340
                            term 33 ──►  1980
                            term 34 ──►  1740
                            term 35 ──►  4620
                            term 36 ──►  4060
                            term 37 ──►  10780
                            term 38 ──►  12740
                            term 39 ──►  2380
                            term 40 ──►  2184
                            term 41 ──►  408
                            term 42 ──►  152
                            term 43 ──►  92
                            term 44 ──►  380
                            term 45 ──►  230
                            term 46 ──►  950
                            term 47 ──►  575
                            term 48 ──►  2375
                            term 49 ──►  9625
                            term 50 ──►  11375
                            term 51 ──►  2125
                            term 52 ──►  1950
                            term 53 ──►  1650
                            term 54 ──►  1450
                            term 55 ──►  3850
                            term 56 ──►  4550
                            term 57 ──►  850
                            term 58 ──►  780
                            term 59 ──►  660
                            term 60 ──►  580
                            term 61 ──►  1540
                            term 62 ──►  1820
                            term 63 ──►  340
                            term 64 ──►  312
                            term 65 ──►  264
                            term 66 ──►  232
                            term 67 ──►  616
                            term 68 ──►  728
                            term 69 ──►  136
                            term 70 ──►  8
                            term 71 ──►  60
                            term 72 ──►  450
                            term 73 ──►  3375
                            term 74 ──►  185625
                            term 75 ──►  163125
                            term 76 ──►  433125
                            term 77 ──►  380625
                            term 78 ──►  1010625
                            term 79 ──►  888125
                            term 80 ──►  2358125
                            term 81 ──►  2786875
                            term 82 ──►  520625
                            term 83 ──►  477750
                            term 84 ──►  89250
                            term 85 ──►  81900
                            term 86 ──►  15300
                            term 87 ──►  14040
                            term 88 ──►  11880
                            term 89 ──►  10440
                            term 90 ──►  27720
                            term 91 ──►  24360
                            term 92 ──►  64680
                            term 93 ──►  56840
                            term 94 ──►  150920
                            term 95 ──►  178360
                            term 96 ──►  33320
                            term 97 ──►  30576
                            term 98 ──►  5712
                            term 99 ──►  2128
                           term 100 ──►  1288

```



### showing prime numbers

Programming note:   if the number of terms specified (the 2<sup>nd</sup> argument) is negative, then only powers of two are displayed.

```rexx
/*REXX program runs  FRACTRAN  for a given set of  fractions  and  from a specified  N. */
numeric digits 999;        w= length( digits() ) /*be able to handle gihugeic numbers.  */
parse arg N terms fracs                          /*obtain optional arguments from the CL*/
if N==''     |     N==","  then     N=   2       /*Not specified?  Then use the default.*/
if terms=='' | terms==","  then terms= 100       /* "      "         "   "   "      "   */
if fracs=''                then fracs= '17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23,',
                                       '77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1'
                                                 /* [↑]  The default for the fractions. */
f=space(fracs, 0)                                /*remove all blanks from the FRACS list*/
                   do #=1  while f\=='';    parse var  f    n.#   '/'   d.#   ","   f
                   end   /*#*/                   /* [↑]  parse all the fractions in list*/
#= # - 1                                         /*adjust the number of fractions found.*/
tell= terms>0                                    /*flag:  show number  or  a power of 2.*/
!.= 0;                              _= 1         /*the default value  for powers of  2. */
if \tell  then do p=1  until length(_)>digits();        _= _ + _;              !._= 1
               if p==1  then @._= left('', w + 9)        "2**"left(p, w)    ' '
                        else @._= '(prime' right(p, w)")  2**"left(p, w)    ' '
               end   /*p*/                       /* [↑]  build   powers of 2   tables.  */
L= length(N)                                     /*length in decimal digits of integer N*/
say #  'fractions:'  fracs                       /*display number and actual fractions. */
say 'N  is starting at '  N                      /*display the starting number   N.     */
if tell  then say terms  ' terms are being shown:'                     /*display header.*/
         else say 'only powers of two are being shown:'                /*   "       "   */
q='(max digits used:'                            /*a literal used in the   SAY   below. */

  do j=1  for  abs(terms)                        /*perform DO loop once for each  term. */
      do k=1  for  #                             /*   "     "   "    "   "    " fraction*/
      if N // d.k \== 0  then iterate            /*Not an integer?  Then ignore it.     */
      if tell then say right('term' j, 35)   "──► "   N      /*display Nth term and N.*/
              else if !.N  then say right('term' j,15)  "──►"   @.N  q  right(L,w)")  "  N
      N= N  %  d.k  *  n.k                       /*calculate next term (use %≡integer ÷)*/
      L= max(L, length(N) )                      /*the maximum number of decimal digits.*/
      iterate j                                  /*go start calculating the next term.  */
      end   /*k*/                                /* [↑]  if an integer, we found a new N*/
  end       /*j*/                                /*stick a fork in it,  we're  done.    */
```

(negative fifty million)

```txt

14 fractions: 17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1
N  is starting at  2
only powers of two are being shown:
         term 1                  2**1     (max digits used:   1)   2
        term 20 ──► (prime   2)  2**2     (max digits used:   4)   4
        term 70 ──► (prime   3)  2**3     (max digits used:   5)   8
       term 281 ──► (prime   5)  2**5     (max digits used:   8)   32
       term 708 ──► (prime   7)  2**7     (max digits used:  12)   128
      term 2364 ──► (prime  11)  2**11    (max digits used:  18)   2048
      term 3877 ──► (prime  13)  2**13    (max digits used:  21)   8192
      term 8069 ──► (prime  17)  2**17    (max digits used:  27)   131072
     term 11320 ──► (prime  19)  2**19    (max digits used:  30)   524288
     term 19202 ──► (prime  23)  2**23    (max digits used:  36)   8388608
     term 36867 ──► (prime  29)  2**29    (max digits used:  46)   536870912
     term 45552 ──► (prime  31)  2**31    (max digits used:  49)   2147483648
     term 75225 ──► (prime  37)  2**37    (max digits used:  58)   137438953472
...
    (some output elided.)
...
 term 193455490 ──► (prime 523)  2**523   (max digits used: 808)   27459190640522438859927603196325572869077741200573221637577853836742172733590624208490238562645818219909185245565923432148487951998866575250296113164460228608

```

Output note:   There are intermediary numbers (that aren't powers of two) that are hundreds of digits long.




## Ruby


```ruby
ar = %w[17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1]
FractalProgram = ar.map(&:to_r)                                #=> array of rationals

Runner = Enumerator.new do |y|
  num = 2
  loop{ y << num *= FractalProgram.detect{|f| (num*f).denominator == 1} }
end

prime_generator = Enumerator.new do |y|
  Runner.each do |num|
    l = Math.log2(num)
    y << l.to_i if l.floor == l
  end
end

# demo
p Runner.take(20).map(&:numerator)
p prime_generator.take(20)
```


```txt

[15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132, 116, 308, 364, 68, 4, 30]
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

```



## Scala


```scala
class TestFractran extends FunSuite {
  val program = Fractran("17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1")
  val expect = List(2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770, 910, 170, 156, 132)

  test("find first fifteen fractran figures") {
    assert((program .execute(2) take 15 toList) === expect)
  }
}

object Fractran {
  val pattern = """\s*(\d+)\s*/\s*(\d+)\s*""".r
  def parse(m: Match) = ((m group 1).toInt, (m group 2).toInt)
  def apply(program: String) =  new Fractran(
    pattern.findAllMatchIn(program).map(parse).toList)
}

class Fractran(val numDem: List[(Int,Int)]) {
  def execute(value: Int) = unfold(value) { v =>
    numDem indexWhere(v % _._2 == 0) match {
      case i if i > -1 => Some(v, numDem(i)._1 * v / numDem(i)._2)
      case _ => None
    }
  }
}
```



## Scheme

Scheme naturally handles fractions, translating to integers as required.
The first part of the code translates from a string representation, as required, but equally the user could type the list of fractions in directly as a list.


```scheme

(import (scheme base)
        (scheme inexact)
        (scheme read)
        (scheme write))

(define *string-fractions* ; string input of fractions
  "17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19
  1/17 11/13 13/11 15/14 15/2 55/1")

(define *fractions* ; create vector of fractions from string input
  (list->vector ; convert result to a vector, for constant access times
    (read (open-input-string ; read from the string of fractions, as a list
            (string-append "(" *string-fractions* ")")))))

;; run a fractran interpreter, returning the next number for n
;; or #f if no next number available
;; assume fractions: ordered vector of positive fractions
;;                n: a positive integer
(define (fractran fractions n)
  (let ((max-n (vector-length fractions)))
    (let loop ((i 0))
      (cond ((= i max-n)
             #f)
            ((integer? (* n (vector-ref fractions i)))
             (* n (vector-ref fractions i)))
            (else
              (loop (+ 1 i)))))))

;; Task
(define (display-result max-n)
  (do ((i 0 (+ 1 i))
       (n 2 (fractran *fractions* n)))
    ((= i max-n) (newline))
    (display n) (display " ")))

(display "Task: ")
(display-result 20) ; show first 20 numbers

;; Extra Credit: derive first 20 prime numbers
(define (generate-primes target-number initial-n)
  (define (is-power-of-two? n)
    (and (> n 2)
         (integer? (log n 2))))
  (define (extract-prime n)
    (exact (log n 2)))
  ;
  (let loop ((count 0)
             (n initial-n))
    (when (< count target-number)
      (cond ((eq? n #f)
             (display "-- FAILED TO COMPUTE N --\n"))
            ((is-power-of-two? n)
             (display (extract-prime n)) (display " ")
             (loop (+ 1 count)
                   (fractran *fractions* n)))
            (else
              (loop count
                    (fractran *fractions* n))))))
  (newline))

(display "Primes:\n")
(generate-primes 20 2) ; create first 20 primes


```


```txt

Task: 2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132 116 308 364 68 4
Primes:
2 3 5 7 11 13 17 19 23 37 41 43 53 61 67 71 73 79 83 89

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "rational.s7i";

const func array integer: fractran (in integer: limit, in var integer: number, in array rational: program) is func
  result
    var array integer: output is 0 times 0;
  local
    var integer: index is 1;
    var rational: newNumber is 0/1;
  begin
    output := [] (number);
    while index <= length(program) and length(output) <= limit do
      newNumber := rat(number) * program[index];
      if newNumber = rat(trunc(newNumber)) then
        number := trunc(newNumber);
        output &:= number;
        index := 1;
      else
        incr(index);
      end if;
    end while;
  end func;

const proc: main is func
  local
    const array rational: program is []
        (17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1);
    var array integer: output is 0 times 0;
    var integer: number is 0;
  begin
    output := fractran(15, 2, program);
    for number range output do
      write(number <& " ");
    end for;
    writeln;
  end func;
```


```txt

2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132 116

```


Program to compute prime numbers with fractran (The program has no limit, use CTRL-C to terminate it):

```seed7
$ include "seed7_05.s7i";
  include "bigrat.s7i";

const proc: fractran (in var bigInteger: number, in array bigRational: program) is func
  local
    var integer: index is 1;
    var bigRational: newNumber is 0_/1_;
  begin
    while index <= length(program) do
      newNumber := rat(number) * program[index];
      if newNumber = rat(trunc(newNumber)) then
        number := trunc(newNumber);
        if 2_ ** ord(log2(number)) = number then
          writeln(log2(number));
        end if;
        index := 1;
      else
        incr(index);
      end if;
    end while;
  end func;

const proc: main is func
  local
    const array bigRational: program is []
        (17_/91_, 78_/85_, 19_/51_, 23_/38_, 29_/33_, 77_/29_, 95_/23_, 77_/19_, 1_/17_, 11_/13_, 13_/11_, 15_/14_, 15_/2_, 55_/1_);
  begin
    fractran(2_, program);
  end func;
```


```txt

2
3
5
7
11
13
17
19
23
29
31
37
41
43
47
53
59
61
67
71
73
79
83
89
97
101

```



## Sidef

```ruby
var str ="17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1"
const FractalProgram = str.split(',').map{.num}      #=> array of rationals

func runner(n, callback) {
    var num = 2
    n.times {
        callback(num *= FractalProgram.find { |f| f * num -> is_int })
    }
}

func prime_generator(n, callback) {
    var x = 0;
    runner(Inf, { |num|
        var l = num.log2
        if (l.floor == l) {
            callback(l.int)
            ++x == n && return nil
        }
    })
}

STDOUT.autoflush(true)

runner(20, {|n| print (n, ' ') })
print "\n"

prime_generator(20, {|n| print (n, ' ') })
print "\n"
```

```txt

15 825 725 1925 2275 425 390 330 290 770 910 170 156 132 116 308 364 68 4 30
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71

```



## Tcl

```tcl
package require Tcl 8.6

oo::class create Fractran {
    variable fracs nco
    constructor {fractions} {
	set fracs {}
	foreach frac $fractions {
	    if {[regexp {^(\d+)/(\d+),?$} $frac -> num denom]} {
		lappend fracs $num $denom
	    } else {
		return -code error "$frac is not a supported fraction"
	    }
	}
	if {![llength $fracs]} {
	    return -code error "need at least one fraction"
	}
    }

    method execute {n {steps 15}} {
	set co [coroutine [incr nco] my Generate $n]
	for {set i 0} {$i < $steps} {incr i} {
	    lappend result [$co]
	}
	catch {rename $co ""}
	return $result
    }

    method Step {n} {
	foreach {num den} $fracs {
	    if {$n % $den} continue
	    return [expr {$n * $num / $den}]
	}
	return -code break
    }
    method Generate {n} {
	yield [info coroutine]
	while 1 {
	    yield $n
	    set n [my Step $n]
	}
	return -code break
    }
}

set ft [Fractran new {
    17/91 78/85 19/51 23/38 29/33 77/29 95/23
    77/19 1/17 11/13 13/11 15/14 15/2 55/1
}]
puts [$ft execute 2]
```

```txt

2 15 825 725 1925 2275 425 390 330 290 770 910 170 156 132

```

You can just collect powers of 2 by monkey-patching in something like this:

```tcl
oo::objdefine $ft method pow2 {n} {
    set co [coroutine [incr nco] my Generate 2]
    set pows {}
    while {[llength $pows] < $n} {
	set item [$co]
	if {($item & ($item-1)) == 0} {
	    lappend pows $item
	}
    }
    return $pows
}
puts [$ft pow2 10]
```

Which will then produce this additional output:

```txt

2 4 8 32 128 2048 8192 131072 524288 8388608

```


=={{header|TI-83 BASIC}}==
```ti83b
100->T
2->N
{17,78,19,23,29,77,95,77, 1,11,13,15,15,55}->LA
{91,85,51,38,33,29,23,19,17,13,11,14, 2, 1}->LB
Dim(LA)->U
T->Dim(LC)
For(I,1,T)
	1->J: 1->F
	While J<=U and F=1
		If remainder(N,LB(J))=0
		Then
			Disp N
			N->LC(I)
			iPart(N/LB(J))*LA(J)->N
			0->F
		End
		J+1->J
	End
End
```

Note:
  -> stands for Store symbol
  L  stands for List  symbol in LA,LB,LC
```txt

           2
          15
         825
         725
        1925
        2275
         425
         390
         330
         290
...
        2128
        1288

```




## VBA

This implementations follows the Wikipedia description of [[wp:FRACTRAN|FRACTRAN]]. There are test, decrement and increment instructions on an array of variables which are the exponents of the prime factors of the argument, which are the only instructions used to run the program with the function run, or go through it step by step with the function steps. An auxiliary factor function is used to compile the FRACTRAN program.

```vb
Option Base 1
Public prime As Variant
Public nf As New Collection
Public df As New Collection
Const halt = 20
Private Sub init()
    prime = [{2,3,5,7,11,13,17,19,23,29,31}]
End Sub
Private Function factor(f As Long) As Variant
    Dim result(10) As Integer
    Dim i As Integer: i = 1
    Do While f > 1
        Do While f Mod prime(i) = 0
            f = f \ prime(i)
            result(i) = result(i) + 1
        Loop
        i = i + 1
    Loop
    factor = result
End Function
Private Function decrement(ByVal a As Variant, b As Variant) As Variant
    For i = LBound(a) To UBound(a)
        a(i) = a(i) - b(i)
    Next i
    decrement = a
End Function
Private Function increment(ByVal a As Variant, b As Variant) As Variant
    For i = LBound(a) To UBound(a)
        a(i) = a(i) + b(i)
    Next i
    increment = a
End Function
Private Function test(a As Variant, b As Variant)
    flag = True
    For i = LBound(a) To UBound(a)
        If a(i) < b(i) Then
            flag = False
            Exit For
        End If
    Next i
    test = flag
End Function
Private Function unfactor(x As Variant) As Long
    result = 1
    For i = LBound(x) To UBound(x)
        result = result * prime(i) ^ x(i)
    Next i
    unfactor = result
End Function
Private Sub compile(program As String)
    program = Replace(program, " ", "")
    programlist = Split(program, ",")
    For Each instruction In programlist
        parts = Split(instruction, "/")
        nf.Add factor(Val(parts(0)))
        df.Add factor(Val(parts(1)))
    Next instruction
End Sub
Private Function run(x As Long) As Variant
    n = factor(x)
    counter = 0
    Do While True
        For i = 1 To df.Count
            If test(n, df(i)) Then
                n = increment(decrement(n, df(i)), nf(i))
                Exit For
            End If
        Next i
        Debug.Print unfactor(n);
        counter = counter + 1
        If num = 31 Or counter >= halt Then Exit Do
    Loop
    Debug.Print
    run = n
End Function
Private Function steps(x As Variant) As Variant
    'expects x=factor(n)
    For i = 1 To df.Count
        If test(x, df(i)) Then
            x = increment(decrement(x, df(i)), nf(i))
            Exit For
        End If
    Next i
    steps = x
End Function
Private Function is_power_of_2(x As Variant) As Boolean
    flag = True
    For i = LBound(x) + 1 To UBound(x)
        If x(i) > 0 Then
            flag = False
            Exit For
        End If
    Next i
    is_power_of_2 = flag
End Function
Private Function filter_primes(x As Long, max As Integer) As Long
    n = factor(x)
    i = 0: iterations = 0
    Do While i < max
        If is_power_of_2(steps(n)) Then
            Debug.Print n(1);
            i = i + 1
        End If
        iterations = iterations + 1
    Loop
    Debug.Print
    filter_primes = iterations
End Function
Public Sub main()
    init
    compile ("17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14,  15/2, 55/1")
    Debug.Print "First 20 results:"
    output = run(2)
    Debug.Print "First 30 primes:"
    Debug.Print "after"; filter_primes(2, 30); "iterations."
End Sub
```
```txt
First 20 results:
 15  825  725  1925  2275  425  390  330  290  770  910  170  156  132  116  308  364  68  4  30
First 30 primes:
 2  3  5  7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71  73  79  83  89  97  101  103  107  109  113
after 2019962 iterations.
```



## zkl


```zkl
var fracs="17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17,"
   "11/13,  13/11, 15/14, 15/2, 55/1";
fcn fractranW(n,fracsAsOneBigString){ //-->Walker (iterator)
  fracs:=(fracsAsOneBigString-" ").split(",").apply(
	 fcn(frac){ frac.split("/").apply("toInt") }); //( (n,d), (n,d), ...)
   Walker(fcn(rn,fracs){
      n:=rn.value;
      foreach a,b in (fracs){
	 if(n*a%b == 0){
	    rn.set(n*a/b);
	    return(n);
	 }
      }
   }.fp(Ref(n),fracs))
}
```


```zkl
fractranW(2,fracs).walk(20).println();
```

```txt

L(2,15,825,725,1925,2275,425,390,330,290,770,910,170,156,132,116,308,364,68,4)

```

```zkl
var [const] BN=Import("zklBigNum");  // libGMP
fcn fractranPrimes{
   foreach n,fr in ([1..].zip(fractranW(BN(2),fracs))){
      if(fr.num1s==1){
	 p:=(fr.toString(2) - "1").len();  // count zeros
	 if(p>1)
	    println("Prime %3d from the nth Fractran(%8d): %d".fmt(p,n,fr));
      }
   }
}
fractranPrimes();
```

```txt

Prime   2 from the nth Fractran(      20): 4
Prime   3 from the nth Fractran(      70): 8
Prime   5 from the nth Fractran(     281): 32
...
Prime 227 from the nth Fractran(15956646): 215679573337205118357336120696157045389097155380324579848828881993728
Prime 229 from the nth Fractran(16429799): 862718293348820473429344482784628181556388621521298319395315527974912
Prime 233 from the nth Fractran(17293373): 13803492693581127574869511724554050904902217944340773110325048447598592
Prime 239 from the nth Fractran(18633402): 883423532389192164791648750371459257913741948437809479060803100646309888
...

```


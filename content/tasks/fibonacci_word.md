+++
title = "Fibonacci word"
description = ""
date = 2018-07-29T14:02:05Z
aliases = []
[extra]
id = 15207
[taxonomies]
categories = ["task"]
tags = []
+++

The   Fibonacci Word   may be created in a manner analogous to the   Fibonacci Sequence    [http://hal.archives-ouvertes.fr/docs/00/36/79/72/PDF/The_Fibonacci_word_fractal.pdf as described here]:

     Define   F_Word<sub>1</sub>   as   '''1'''
     Define   F_Word<sub>2</sub>   as   '''0'''
     Form     F_Word<sub>3</sub>   as   F_Word<sub>2</sub>     concatenated with   F_Word<sub>1</sub>    i.e.:   '''01'''
     Form     F_Word<sub>n</sub>   as   F_Word<sub>n-1</sub>   concatenated with   F_word<sub>n-2</sub>


## Task

Perform the above steps for     n = 37.

You may display the first few but not the larger values of   n.

<small>{Doing so will get the task's author into trouble with them what be (again!).} </small>

Instead, create a table for   F_Words   '''1'''   to   '''37'''   which shows:
::*   The number of characters in the word
::*   The word's [[Entropy]]


## Related tasks

*   [[Fibonacci_word/fractal|Fibonacci word/fractal]]
*   [[Entropy]]
*   [[Entropy/Narcissist]]





## Ada


```Ada
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Unbounded,
  Ada.Strings.Unbounded.Text_IO, Ada.Numerics.Long_Elementary_Functions,
  Ada.Long_Float_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Unbounded,
  Ada.Strings.Unbounded.Text_IO, Ada.Numerics.Long_Elementary_Functions,
  Ada.Long_Float_Text_IO;

procedure Fibonacci_Words is

   function Entropy (S : Unbounded_String) return Long_Float is
      CF    : array (Character) of Natural := (others => 0);
      Len   : constant Natural             := Length (S);
      H     : Long_Float                   := 0.0;
      Ratio : Long_Float;
   begin
      for I in 1 .. Len loop
         CF (Element (S, I)) := CF (Element (S, I)) + 1;
      end loop;
      for C in Character loop
         Ratio := Long_Float (CF (C)) / Long_Float (Len);
         if Ratio /= 0.0 then
            H := H - Ratio * Log (Ratio, 2.0);
         end if;
      end loop;
      return H;
   end Entropy;

   procedure Print_Line (Word : Unbounded_String; Number : Integer) is
   begin
      Put (Number, 4);
      Put (Length (Word), 10);
      Put (Entropy (Word), 2, 15, 0);
      if Length (Word) < 35 then
         Put ("  " & Word);
      end if;
      New_Line;
   end Print_Line;

   First, Second, Result : Unbounded_String;

begin
   Set_Col (4);  Put ("N");
   Set_Col (9);  Put ("Length");
   Set_Col (16); Put ("Entropy");
   Set_Col (35); Put_Line ("Word");
   First := To_Unbounded_String ("1");
   Print_Line (First, 1);
   Second := To_Unbounded_String ("0");
   Print_Line (Second, 2);
   for N in 3 .. 37 loop
      Result := Second & First;
      Print_Line (Result, N);
      First  := Second;
      Second := Result;
   end loop;
end Fibonacci_Words;

```

Output

```txt
   N    Length Entropy            Word
   1         1 0.000000000000000  1
   2         1 0.000000000000000  0
   3         2 1.000000000000000  01
   4         3 0.918295834054490  010
   5         5 0.970950594454669  01001
   6         8 0.954434002924965  01001010
   7        13 0.961236604722876  0100101001001
   8        21 0.958711882977132  010010100100101001010
   9        34 0.959686893774217  0100101001001010010100100101001001
  10        55 0.959316032054378
  11        89 0.959457915838670
  12       144 0.959403754221023
  13       233 0.959424446955987
  14       377 0.959416543740441
  15       610 0.959419562603144
  16       987 0.959418409515224
  17      1597 0.959418849957810
  18      2584 0.959418681724032
  19      4181 0.959418745983664
  20      6765 0.959418721438675
  21     10946 0.959418730814028
  22     17711 0.959418727232962
  23     28657 0.959418728600807
  24     46368 0.959418728078337
  25     75025 0.959418728277903
  26    121393 0.959418728201675
  27    196418 0.959418728230792
  28    317811 0.959418728219670
  29    514229 0.959418728223918
  30    832040 0.959418728222296
  31   1346269 0.959418728222916
  32   2178309 0.959418728222679
  33   3524578 0.959418728222769
  34   5702887 0.959418728222735
  35   9227465 0.959418728222748
  36  14930352 0.959418728222743
  37  24157817 0.959418728222745

```



## Aime


```aime
real
entropy(data b)
{
    integer count, i;
    real ones, zeros;

    ones = zeros = 0;

    i = -(count = ~b);
    while (i) {
        if (b[i] == '0') {
            zeros += 1;
        } else {
            ones += 1;
        }

        i += 1;
    }

    return -(ones /= count) * log2(ones) - (zeros /= count) * log2(zeros);
}

integer
main(void)
{
    data a, b;
    integer i;

    a = "1";
    b = "0";

    o_form("%2d %9d /w12p10d10/ ~\n", 1, ~a, 0r, a);
    o_form("%2d %9d /w12p10d10/ ~\n", 2, ~b, 0r, b);
    i = 3;
    while (i <= 37) {
        bu_copy(a, 0, b);
        o_form("%2d %9d /w12p10d10/ ~\n", i, ~a, entropy(a),
               i < 10 ? a.string : "");
        i += 1;
        b.swap(a);
    }

    return 0;
}
```

```txt
 1         1 0            1
 2         1 0            0
 3         2 1            01
 4         3  .9182958340 010
 5         5  .9709505944 01001
 6         8  .9544340029 01001010
 7        13  .9612366047 0100101001001
 8        21  .9587118829 010010100100101001010
 9        34  .9596868937 0100101001001010010100100101001001
10        55  .9593160320
11        89  .9594579158
12       144  .9594037542
13       233  .9594244469
14       377  .9594165437
15       610  .9594195626
16       987  .9594184095
17      1597  .9594188499
18      2584  .9594186817
19      4181  .9594187459
20      6765  .9594187214
21     10946  .9594187308
22     17711  .9594187272
23     28657  .9594187286
24     46368  .9594187280
25     75025  .9594187282
26    121393  .9594187282
27    196418  .9594187282
28    317811  .9594187282
29    514229  .9594187282
30    832040  .9594187282
31   1346269  .9594187282
32   2178309  .9594187282
33   3524578  .9594187282
34   5702887  .9594187282
35   9227465  .9594187282
36  14930352  .9594187282
37  24157817  .9594187282
```



## ALGOL 68

```algol68
# calculate some details of "Fibonacci Words"               #

# fibonacci word 1 = "1"                                    #
# fibonacci word 2 = "0"                                    #
#                3 = word 2 cat word 1 = "01"               #
#                n = word n-1 cat word n-2                  #

# note the words contain only the characters "0" and "1"    #
# also                                                      #
#    C(word n) = C(word n-1) + C(word n-2)                  #
#           where C(x) = the number of characters in x      #
# Similarly,                                                #
#      C0(word n) = C0(word n-1) + C0(word n-2)             #
# and  C1(word n) = C1(word n-1) + C1(word n-2)             #
#      where C0(x) = the number of "0"s in x and            #
#            C1(x) = the number of "1"s in x                #

# we therefore don't have to calculate the words themselves #


# prints the statistics for the fibonacci words from 1 to max number #
PROC print fibonacci word stats = ( INT max number )VOID:
BEGIN


    # prints some statistics for a fibonacci word:                   #
    #        the word number, its length and its entropy             #
    PROC print one words stats = ( INT word
                                 , INT zeros
                                 , INT ones
                                 )VOID:
    BEGIN

        REAL probability := 0;
        REAL entropy     := 0;
        INT  word length  = zeros + ones;

        IF zeros > 0
        THEN
            # the word contains some zeros #
            probability := zeros / word length;
            entropy    -:= probability * log( probability )
        FI;

        IF ones > 0
        THEN
            # the word contains some ones #
            probability := ones  / word length;
            entropy    -:= probability * log( probability )
        FI;

        # we want entropy in bits so convert to log base 2 #
        entropy /:= log( 2 );

        print( ( ( whole( word,         -5 )
                 + " "
                 + whole( word length, -12 )
                 + " "
                 + fixed( entropy, -8, 4 )
                 )
               , newline
               )
             )


    END; # print one words stats #


    INT zeros one     = 0; # number of zeros in word 1 #
    INT ones  one     = 1; # number of ones  in word 1 #
    INT zeros two     = 1; # number of zeros in word 2 #
    INT ones  two     = 0; # number of ones  in word 2 #

    print( ( " word       length  entropy", newline ) );

    IF max number > 0
    THEN
        # we want at least one number's statistics #
        print one words stats( 1, zeros one, ones one );

        IF max number > 1
        THEN
            # we want at least 2 number's statistics #
            print one words stats( 2, zeros two, ones two );

            IF max number > 2
            THEN
                # we want more statistics #

                INT zeros n minus 1 := zeros two;
                INT ones  n minus 1 := ones  two;
                INT zeros n minus 2 := zeros one;
                INT ones  n minus 2 := ones  one;

                FOR word FROM 3 TO max number DO

                    INT zeros n := zeros n minus 1 + zeros n minus 2;
                    INT ones  n := ones  n minus 1 + ones  n minus 2;

                    print one words stats( word, zeros n, ones n );

                    zeros n minus 2 := zeros n minus 1;
                    ones  n minus 2 := ones  n minus 1;
                    zeros n minus 1 := zeros n;
                    ones  n minus 1 := ones  n
                OD
            FI
        FI
    FI

END; # print fibonacci word stats #


main:
(
    # print the statistics for the first 37 fibonacci words #
    print fibonacci word stats( 37 )
)

```

```txt

 word       length  entropy
    1            1   0.0000
    2            1   0.0000
    3            2   1.0000
    4            3   0.9183
    5            5   0.9710
    6            8   0.9544
    7           13   0.9612
    8           21   0.9587
    9           34   0.9597
   10           55   0.9593
   11           89   0.9595
   12          144   0.9594
   13          233   0.9594
   14          377   0.9594
   15          610   0.9594
   16          987   0.9594
   17         1597   0.9594
   18         2584   0.9594
   19         4181   0.9594
   20         6765   0.9594
   21        10946   0.9594
   22        17711   0.9594
   23        28657   0.9594
   24        46368   0.9594
   25        75025   0.9594
   26       121393   0.9594
   27       196418   0.9594
   28       317811   0.9594
   29       514229   0.9594
   30       832040   0.9594
   31      1346269   0.9594
   32      2178309   0.9594
   33      3524578   0.9594
   34      5702887   0.9594
   35      9227465   0.9594
   36     14930352   0.9594
   37     24157817   0.9594

```



## APL


```apl

      F_WORD←{{⍵,,/⌽¯2↑⍵}⍣(0⌈⍺-2),¨⍵}
      ENTROPY←{-+/R×2⍟R←(+⌿⍵∘.=∪⍵)÷⍴⍵}
      FORMAT←{'N' 'LENGTH' 'ENTROPY'⍪(⍳⍵),↑{(⍴⍵),ENTROPY ⍵}¨⍵ F_WORD 1 0}

```

```txt

      FORMAT 37
 N   LENGTH       ENTROPY
 1        1  0
 2        1  0
 3        2  1
 4        3  0.9182958341
 5        5  0.9709505945
 6        8  0.9544340029
 7       13  0.9612366047
 8       21  0.958711883
 9       34  0.9596868938
10       55  0.9593160321
11       89  0.9594579158
12      144  0.9594037542
13      233  0.959424447
14      377  0.9594165437
15      610  0.9594195626
16      987  0.9594184095
17     1597  0.95941885
18     2584  0.9594186817
19     4181  0.959418746
20     6765  0.9594187214
21    10946  0.9594187308
22    17711  0.9594187272
23    28657  0.9594187286
24    46368  0.9594187281
25    75025  0.9594187283
26   121393  0.9594187282
27   196418  0.9594187282
28   317811  0.9594187282
29   514229  0.9594187282
30   832040  0.9594187282
31  1346269  0.9594187282
32  2178309  0.9594187282
33  3524578  0.9594187282
34  5702887  0.9594187282
35  9227465  0.9594187282
36 14930352  0.9594187282
37 24157817  0.9594187282

```



## AutoHotkey


```AutoHotkey
SetFormat, FloatFast, 0.15
SetBatchLines, -1
OutPut := "N`tLength`t`tEntropy`n"
        . "1`t" 1 "`t`t" Entropy(FW1 := "1") "`n"
        . "2`t" 1 "`t`t" Entropy(FW2 := "0") "`n"
Loop, 35
{
    FW3 := FW2 FW1, FW1 := FW2, FW2 := FW3
    Output .= A_Index + 2 "`t" StrLen(FW3) (A_Index > 33 ? "" : "`t") "`t" Entropy(FW3) "`n"
}
MsgBox, % Output

Entropy(n)
{
    a := [], len:= StrLen(n), m := n
    while StrLen(m)
    {
        s := SubStr(m, 1, 1)
        m := RegExReplace(m, s, "", c)
        a[s] := c
    }
    for key, val in a
    {
        m := Log(p := val / len)
        e -= p * m / Log(2)
    }
    return, e
}
```

'''Output:'''

```txt
N	Length		Entropy
1	1		0.000000000000000
2	1		0.000000000000000
3	2		1.000000000000000
4	3		0.918295834054490
5	5		0.970950594454669
6	8		0.954434002924965
7	13		0.961236604722875
8	21		0.958711882977132
9	34		0.959686893774216
10	55		0.959316032054378
11	89		0.959457915838669
12	144		0.959403754221023
13	233		0.959424446955987
14	377		0.959416543740440
15	610		0.959419562603144
16	987		0.959418409515225
17	1597		0.959418849957810
18	2584		0.959418681724033
19	4181		0.959418745983664
20	6765		0.959418721438676
21	10946		0.959418730814027
22	17711		0.959418727232962
23	28657		0.959418728600807
24	46368		0.959418728078337
25	75025		0.959418728277903
26	121393		0.959418728201676
27	196418		0.959418728230791
28	317811		0.959418728219671
29	514229		0.959418728223918
30	832040		0.959418728222296
31	1346269		0.959418728222915
32	2178309		0.959418728222679
33	3524578		0.959418728222769
34	5702887		0.959418728222735
35	9227465		0.959418728222748
36	14930352	0.959418728222743
37	24157817	0.959418728222745
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void print_headings()
{
	printf("%2s", "N");
	printf(" %10s", "Length");
	printf(" %-20s", "Entropy");
	printf(" %-40s", "Word");
	printf("\n");
}

double calculate_entropy(int ones, int zeros)
{
	double result = 0;

	int total = ones + zeros;
	result -= (double) ones / total * log2((double) ones / total);
	result -= (double) zeros / total * log2((double) zeros / total);

	if (result != result) { // NAN
		result = 0;
	}

	return result;
}

void print_entropy(char *word)
{
	int ones = 0;
	int zeros = 0;

	int i;
	for (i = 0; word[i]; i++) {
		char c = word[i];

		switch (c) {
			case '0':
				zeros++;
				break;
			case '1':
				ones++;
				break;
		}
	}

	double entropy = calculate_entropy(ones, zeros);
	printf(" %-20.18f", entropy);
}

void print_word(int n, char *word)
{
	printf("%2d", n);

	printf(" %10ld", strlen(word));

	print_entropy(word);

	if (n < 10) {
		printf(" %-40s", word);
	} else {
		printf(" %-40s", "...");
	}

	printf("\n");
}

int main(int argc, char *argv[])
{
	print_headings();

	char *last_word = malloc(2);
	strcpy(last_word, "1");

	char *current_word = malloc(2);
	strcpy(current_word, "0");

	print_word(1, last_word);
	int i;
	for (i = 2; i <= 37; i++) {
		print_word(i, current_word);

		char *next_word = malloc(strlen(current_word) + strlen(last_word) + 1);
		strcpy(next_word, current_word);
		strcat(next_word, last_word);

		free(last_word);
		last_word = current_word;
		current_word = next_word;
	}

	free(last_word);
	free(current_word);
	return 0;
}
```

```txt
 N     Length Entropy              Word
 1          1 0.000000000000000000 1
 2          1 0.000000000000000000 0
 3          2 1.000000000000000000 01
 4          3 0.918295834054489557 010
 5          5 0.970950594454668581 01001
 6          8 0.954434002924964942 01001010
 7         13 0.961236604722875865 0100101001001
 8         21 0.958711882977131724 010010100100101001010
 9         34 0.959686893774216898 0100101001001010010100100101001001
10         55 0.959316032054377654 ...
11         89 0.959457915838669573 ...
12        144 0.959403754221022975 ...
13        233 0.959424446955986721 ...
14        377 0.959416543740440608 ...
15        610 0.959419562603144094 ...
16        987 0.959418409515224280 ...
17       1597 0.959418849957809905 ...
18       2584 0.959418681724032107 ...
19       4181 0.959418745983663834 ...
20       6765 0.959418721438675459 ...
21      10946 0.959418730814027731 ...
22      17711 0.959418727232961954 ...
23      28657 0.959418728600807347 ...
24      46368 0.959418728078337057 ...
25      75025 0.959418728277902866 ...
26     121393 0.959418728201675397 ...
27     196418 0.959418728230791773 ...
28     317811 0.959418728219670225 ...
29     514229 0.959418728223918382 ...
30     832040 0.959418728222295791 ...
31    1346269 0.959418728222915518 ...
32    2178309 0.959418728222678929 ...
33    3524578 0.959418728222769079 ...
34    5702887 0.959418728222734662 ...
35    9227465 0.959418728222747874 ...
36   14930352 0.959418728222742767 ...
37   24157817 0.959418728222744654 ...

```



## C++


```cpp
#include <string>
#include <map>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <iomanip>

double log2( double number ) {
   return ( log( number ) / log( 2 ) ) ;
}

double find_entropy( std::string & fiboword ) {
   std::map<char , int> frequencies ;
   std::for_each( fiboword.begin( ) , fiboword.end( ) ,
	 [ & frequencies ]( char c ) { frequencies[ c ]++ ; } ) ;
   int numlen = fiboword.length( ) ;
   double infocontent = 0 ;
   for ( std::pair<char , int> p : frequencies ) {
      double freq = static_cast<double>( p.second ) / numlen ;
      infocontent += freq * log2( freq ) ;
   }
   infocontent *= -1 ;
   return infocontent ;
}

void printLine( std::string &fiboword , int n ) {
   std::cout << std::setw( 5 ) << std::left << n ;
   std::cout << std::setw( 12 ) << std::right << fiboword.size( ) ;
   std::cout << "  " << std::setw( 16 ) << std::setprecision( 13 )
      << std::left << find_entropy( fiboword ) ;
   std::cout << "\n" ;
}

int main( ) {
   std::cout << std::setw( 5 ) << std::left << "N" ;
   std::cout << std::setw( 12 ) << std::right << "length" ;
   std::cout << "  " << std::setw( 16 ) << std::left << "entropy" ;
   std::cout << "\n" ;
   std::string firststring ( "1" ) ;
   int n = 1 ;
   printLine( firststring , n ) ;
   std::string secondstring( "0" ) ;
   n++ ;
   printLine( secondstring , n ) ;
   while ( n < 37 ) {
      std::string resultstring = firststring + secondstring ;
      firststring.assign( secondstring ) ;
      secondstring.assign( resultstring ) ;
      n++ ;
      printLine( resultstring , n ) ;
   }
   return 0 ;
}
```

```txt
N          length  entropy
1               1  -0
2               1  -0
3               2  1
4               3  0.9182958340545
5               5  0.9709505944547
6               8  0.954434002925
7              13  0.9612366047229
8              21  0.9587118829771
9              34  0.9596868937742
10             55  0.9593160320544
11             89  0.9594579158387
12            144  0.959403754221
13            233  0.959424446956
14            377  0.9594165437404
15            610  0.9594195626031
16            987  0.9594184095152
17           1597  0.9594188499578
18           2584  0.959418681724
19           4181  0.9594187459837
20           6765  0.9594187214387
21          10946  0.959418730814
22          17711  0.959418727233
23          28657  0.9594187286008
24          46368  0.9594187280783
25          75025  0.9594187282779
26         121393  0.9594187282017
27         196418  0.9594187282308
28         317811  0.9594187282197
29         514229  0.9594187282239
30         832040  0.9594187282223
31        1346269  0.9594187282229
32        2178309  0.9594187282227
33        3524578  0.9594187282228
34        5702887  0.9594187282227
35        9227465  0.9594187282227
36       14930352  0.9594187282227
37       24157817  0.9594187282227

```



## C#


```C sharp
using SYS = System;
using SCG = System.Collections.Generic;

//
// Basically a port of the C++ solution as posted
// 2017-11-12.
//
namespace FibonacciWord
{
  class Program
  {
    static void Main( string[] args )
    {
      PrintHeading();
      string firstString = "1";
      int n = 1;
      PrintLine( n, firstString );
      string secondString = "0";
      ++n;
      PrintLine( n, secondString );
      while ( n < 37 )
      {
        string resultString = firstString + secondString;
        firstString = secondString;
        secondString = resultString;
        ++n;
        PrintLine( n, resultString );
      }
    }

    private static void PrintLine( int n, string result )
    {
      SYS.Console.Write( "{0,-5}", n );
      SYS.Console.Write( "{0,12}", result.Length );
      SYS.Console.WriteLine( "  {0,-16}", GetEntropy( result ) );
    }

    private static double GetEntropy( string result )
    {
      SCG.Dictionary<char, int> frequencies = new SCG.Dictionary<char, int>();
      foreach ( char c in result )
      {
        if ( frequencies.ContainsKey( c ) )
        {
          ++frequencies[c];
        }
        else
        {
          frequencies[c] = 1;
        }
      }

      int length = result.Length;
      double entropy = 0;
      foreach ( var keyValue in frequencies )
      {
        double freq = (double)keyValue.Value / length;
        entropy += freq * SYS.Math.Log( freq, 2 );
      }

      return -entropy;
    }

    private static void PrintHeading()
    {
      SYS.Console.Write( "{0,-5}", "N" );
      SYS.Console.Write( "{0,12}", "Length" );
      SYS.Console.WriteLine( "  {0,-16}", "Entropy" );
    }
  }
}
```

```txt
N          Length  Entropy
1               1  0
2               1  0
3               2  1
4               3  0.91829583405449
5               5  0.970950594454669
6               8  0.954434002924965
7              13  0.961236604722876
8              21  0.958711882977132
9              34  0.959686893774217
10             55  0.959316032054378
11             89  0.95945791583867
12            144  0.959403754221023
13            233  0.959424446955987
14            377  0.959416543740441
15            610  0.959419562603144
16            987  0.959418409515225
17           1597  0.95941884995781
18           2584  0.959418681724032
19           4181  0.959418745983664
20           6765  0.959418721438676
21          10946  0.959418730814028
22          17711  0.959418727232962
23          28657  0.959418728600807
24          46368  0.959418728078337
25          75025  0.959418728277903
26         121393  0.959418728201676
27         196418  0.959418728230792
28         317811  0.95941872821967
29         514229  0.959418728223918
30         832040  0.959418728222296
31        1346269  0.959418728222916
32        2178309  0.959418728222679
33        3524578  0.959418728222769
34        5702887  0.959418728222735
35        9227465  0.959418728222748
36       14930352  0.959418728222743
37       24157817  0.959418728222745
```



## Clojure


```Clojure
(defn entropy [s]
  (let [len (count s), log-2 (Math/log 2)]
    (->> (frequencies s)
         (map (fn [[_ v]]
                (let [rf (/ v len)]
                  (-> (Math/log rf) (/ log-2) (* rf) Math/abs))))
         (reduce +))))

(defn fibonacci [cat a b]
  (lazy-seq
    (cons a (fibonacci b (cat a b)))))

; you could also say (fibonacci + 0 1) or (fibonacci concat '(0) '(1))

(printf "%2s %10s %17s %s%n" "N" "Length" "Entropy" "Fibword")
(doseq [i (range 1 38)
        w (take 37 (fibonacci str "1" "0"))]
  (printf "%2d %10d %.15f %s%n" i (count w) (entropy w) (if (<= i 8) w "..."))))
```


Output

```txt
 N     Length           Entropy Fibword
 1          1 0,000000000000000 1
 2          1 0,000000000000000 0
 3          2 1,000000000000000 01
 4          3 0,918295834054490 010
 5          5 0,970950594454669 01001
 6          8 0,954434002924965 01001010
 7         13 0,961236604722876 0100101001001
 8         21 0,958711882977132 010010100100101001010
 9         34 0,959686893774217 ...
10         55 0,959316032054378 ...
11         89 0,959457915838670 ...
12        144 0,959403754221023 ...
13        233 0,959424446955987 ...
14        377 0,959416543740441 ...
15        610 0,959419562603144 ...
16        987 0,959418409515224 ...
17       1597 0,959418849957810 ...
18       2584 0,959418681724032 ...
19       4181 0,959418745983664 ...
20       6765 0,959418721438676 ...
21      10946 0,959418730814028 ...
22      17711 0,959418727232962 ...
23      28657 0,959418728600807 ...
24      46368 0,959418728078337 ...
25      75025 0,959418728277903 ...
26     121393 0,959418728201676 ...
27     196418 0,959418728230792 ...
28     317811 0,959418728219670 ...
29     514229 0,959418728223918 ...
30     832040 0,959418728222296 ...
31    1346269 0,959418728222916 ...
32    2178309 0,959418728222679 ...
33    3524578 0,959418728222769 ...
34    5702887 0,959418728222735 ...
35    9227465 0,959418728222748 ...
36   14930352 0,959418728222743 ...
37   24157817 0,959418728222745 ...
```



## Common Lisp


```lisp
(defun make-fibwords (array)
  (loop for i from 0 below 37
        for j = "0" then (concatenate 'string j k)
        and k = "1" then j
     do (setf (aref array i) k))
  array)

(defvar *fib* (make-fibwords (make-array 37)))

(defun entropy (string)
  (let ((table (make-hash-table :test 'eql))
        (entropy 0d0)
        (n (length string)))
    (mapc (lambda (c)
            (setf (gethash c table) (+ (gethash c table 0) 1)))
          (coerce string 'list))
    (maphash (lambda (k v)
               (declare (ignore k))
               (decf entropy (* (/ v n) (log (/ v n) 2))))
             table)
    entropy))

(defun string-or-dots (string)
  (if (> (length string) 40)
      "..."
      string))

(format t "~2A ~10A ~17A ~A~%" "N" "Length" "Entropy" "Fibword")
(loop for i below 37
      for n = (aref *fib* i) do
     (format t "~2D ~10D ~17,15F ~A~%"
             (1+ i) (length n) (entropy n) (string-or-dots n)))
```



## D


```d
import std.stdio, std.algorithm, std.math, std.string, std.range;

real entropy(T)(T[] s) pure nothrow
if (__traits(compiles, s.sort())) {
    immutable sLen = s.length;
    return s
           .sort()
           .group
           .map!(g => g[1] / real(sLen))
           .map!(p => -p * p.log2)
           .sum;
}

void main() {
    enum uint nMax = 37;

    "  N     Length               Entropy Fibword".writeln;
    uint n = 1;
    foreach (s; recurrence!q{a[n - 1] ~ a[n - 2]}("1", "0").take(nMax))
        writefln("%3d %10d %2.19f %s", n++, s.length,
                 s.dup.representation.entropy.abs,
                 s.length < 25 ? s : "<too long>");
}
```

```txt
  N     Length               Entropy Fibword
  1          1 0.0000000000000000000 1
  2          1 0.0000000000000000000 0
  3          2 1.0000000000000000000 01
  4          3 0.9182958340544895148 010
  5          5 0.9709505944546686389 01001
  6          8 0.9544340029249649645 01001010
  7         13 0.9612366047228758727 0100101001001
  8         21 0.9587118829771318087 010010100100101001010
  9         34 0.9596868937742169332 <too long>
 10         55 0.9593160320543776778 <too long>
 11         89 0.9594579158386694616 <too long>
 12        144 0.9594037542210229294 <too long>
 13        233 0.9594244469559867586 <too long>
 14        377 0.9594165437404407387 <too long>
 15        610 0.9594195626031441501 <too long>
 16        987 0.9594184095152243127 <too long>
 17       1597 0.9594188499578098556 <too long>
 18       2584 0.9594186817240321066 <too long>
 19       4181 0.9594187459836638143 <too long>
 20       6765 0.9594187214386754146 <too long>
 21      10946 0.9594187308140277232 <too long>
 22      17711 0.9594187272329619428 <too long>
 23      28657 0.9594187286008073761 <too long>
 24      46368 0.9594187280783369149 <too long>
 25      75025 0.9594187282779028735 <too long>
 26     121393 0.9594187282016754604 <too long>
 27     196418 0.9594187282307917413 <too long>
 28     317811 0.9594187282196703115 <too long>
 29     514229 0.9594187282239183197 <too long>
 30     832040 0.9594187282222957250 <too long>
 31    1346269 0.9594187282229155010 <too long>
 32    2178309 0.9594187282226787676 <too long>
 33    3524578 0.9594187282227691918 <too long>
 34    5702887 0.9594187282227346529 <too long>
 35    9227465 0.9594187282227478455 <too long>
 36   14930352 0.9594187282227428063 <too long>
 37   24157817 0.9594187282227447312 <too long>
```



## EchoLisp


```scheme

(lib 'struct)
(struct FW ( count0 count1 length string)) ;; a fibonacci word
(define (F-word n) ;; generator
    (define a (F-word (1- n)))
    (define b (F-word (- n 2)))
    (FW
        (+ (FW-count0 a) (FW-count0 b))
        (+ (FW-count1 a) (FW-count1 b))
        (+ (FW-length a) (FW-length b))
        (if (> n 9) "..." (string-append (FW-string a) (FW-string b)))))

(remember 'F-word (vector 0 (FW 0 1 1 "1") (FW 1 0  1 "0")))

(define (entropy fw)
    (define p (// (FW-count0 fw) (FW-length fw)))
    (cond
    ((= p 0) 0)
    ((= p 1) 0)
        (else (- 0 (* p (log2 p)) (* (- 1 p) (log2 (- 1 p)))))))


(define (task (n 38) (fw))
    (for ((i (in-range 1 n)))
        (set! fw (F-word i))
        (printf "%3d %10d %24d %a"
            i (FW-length fw) (entropy fw) (FW-string fw))))

```

```txt

      1          1                        0 1
      2          1                        0 0
      3          2                        1 01
      4          3       0.9182958340544896 010
      5          5       0.9709505944546686 01001
      6          8       0.9544340029249649 01001010
      7         13        0.961236604722876 0100101001001
      8         21       0.9587118829771318 010010100100101001010
      9         34       0.9596868937742169 0100101001001010010100100101001001
     10         55       0.9593160320543777 ...
     11         89       0.9594579158386696 ...
     12        144        0.959403754221023 ...
     13        233       0.9594244469559867 ...
     14        377       0.9594165437404408 ...
     15        610       0.9594195626031441 ...
     16        987       0.9594184095152243 ...
     17       1597       0.9594188499578099 ...
     18       2584       0.9594186817240321 ...
     19       4181       0.9594187459836638 ...
     20       6765       0.9594187214386756 ...
     21      10946       0.9594187308140278 ...
     22      17711        0.959418727232962 ...
     23      28657       0.9594187286008073 ...
     24      46368       0.9594187280783368 ...
     25      75025       0.9594187282779029 ...
     26     121393       0.9594187282016755 ...
     27     196418       0.9594187282307918 ...
     28     317811       0.9594187282196702 ...
     29     514229       0.9594187282239184 ...
     30     832040       0.9594187282222959 ...
     31    1346269       0.9594187282229156 ...
     32    2178309       0.9594187282226788 ...
     33    3524578       0.9594187282227693 ...
     34    5702887       0.9594187282227347 ...
     35    9227465       0.9594187282227479 ...
     36   14930352       0.9594187282227429 ...
     37   24157817       0.9594187282227449 ...

```




## Elixir

```elixir
defmodule RC do
  def entropy(str) do
    leng = String.length(str)
    String.to_charlist(str)
    |> Enum.reduce(Map.new, fn c,acc -> Map.update(acc, c, 1, &(&1+1)) end)
    |> Map.values
    |> Enum.reduce(0, fn count, entropy ->
         freq = count / leng
         entropy - freq * :math.log2(freq)      # log2 was added with Erlang/OTP 18
       end)
  end
end

fibonacci_word = Stream.unfold({"1","0"}, fn{a,b} -> {a, {b, b<>a}} end)

IO.puts "  N    Length       Entropy       Fibword"
fibonacci_word |> Enum.take(37) |> Enum.with_index
|> Enum.each(fn {word,i} ->
  len = String.length(word)
  str = if len < 60, do: word, else: "<too long>"
  :io.format "~3w  ~8w  ~17.15f  ~s~n", [i+1, len, RC.entropy(word), str]
end)
```


```txt

  N    Length       Entropy       Fibword
  1         1  0.000000000000000  1
  2         1  0.000000000000000  0
  3         2  1.000000000000000  01
  4         3  0.918295834054490  010
  5         5  0.970950594454669  01001
  6         8  0.954434002924965  01001010
  7        13  0.961236604722876  0100101001001
  8        21  0.958711882977132  010010100100101001010
  9        34  0.959686893774217  0100101001001010010100100101001001
 10        55  0.959316032054378  0100101001001010010100100101001001010010100100101001010
 11        89  0.959457915838670  <too long>
 12       144  0.959403754221023  <too long>
 13       233  0.959424446955987  <too long>
 14       377  0.959416543740441  <too long>
 15       610  0.959419562603144  <too long>
 16       987  0.959418409515225  <too long>
 17      1597  0.959418849957810  <too long>
 18      2584  0.959418681724032  <too long>
 19      4181  0.959418745983664  <too long>
 20      6765  0.959418721438676  <too long>
 21     10946  0.959418730814028  <too long>
 22     17711  0.959418727232962  <too long>
 23     28657  0.959418728600807  <too long>
 24     46368  0.959418728078337  <too long>
 25     75025  0.959418728277903  <too long>
 26    121393  0.959418728201676  <too long>
 27    196418  0.959418728230792  <too long>
 28    317811  0.959418728219670  <too long>
 29    514229  0.959418728223918  <too long>
 30    832040  0.959418728222296  <too long>
 31   1346269  0.959418728222916  <too long>
 32   2178309  0.959418728222679  <too long>
 33   3524578  0.959418728222769  <too long>
 34   5702887  0.959418728222735  <too long>
 35   9227465  0.959418728222748  <too long>
 36  14930352  0.959418728222743  <too long>
 37  24157817  0.959418728222745  <too long>

```


=={{header|F_Sharp|F#}}==

```fsharp
// include the code from /wiki/Entropy#F.23 for the entropy function

let fiboword  =
    Seq.unfold
        (fun (state : string * string) ->
            Some (fst state, (snd state, (snd state) + (fst state)))) ("1", "0")

printfn "%3s %10s %10s %s" "#" "Length" "Entropy" "Word (if length < 40)"
Seq.iteri (fun i (s : string) ->
    printfn "%3i %10i %10.7g %s" (i+1) s.Length (entropy s) (if s.Length < 40 then s else ""))
    (Seq.take 37 fiboword)
```

```txt
  #     Length    Entropy Word (if length < 40)
  1          1          0 1
  2          1          0 0
  3          2          1 01
  4          3  0.9182958 010
  5          5  0.9709506 01001
  6          8   0.954434 01001010
  7         13  0.9612366 0100101001001
  8         21  0.9587119 010010100100101001010
  9         34  0.9596869 0100101001001010010100100101001001
 10         55   0.959316
 11         89  0.9594579
 12        144  0.9594038
 13        233  0.9594244
 14        377  0.9594165
 15        610  0.9594196
 16        987  0.9594184
 17       1597  0.9594188
 18       2584  0.9594187
 19       4181  0.9594187
 20       6765  0.9594187
 21      10946  0.9594187
 22      17711  0.9594187
 23      28657  0.9594187
 24      46368  0.9594187
 25      75025  0.9594187
 26     121393  0.9594187
 27     196418  0.9594187
 28     317811  0.9594187
 29     514229  0.9594187
 30     832040  0.9594187
 31    1346269  0.9594187
 32    2178309  0.9594187
 33    3524578  0.9594187
 34    5702887  0.9594187
 35    9227465  0.9594187
 36   14930352  0.9594187
 37   24157817  0.9594187
```



## Factor

It is not necessary to calculate each fibonacci word, since every fibonacci word less than 37 is contained in the 37th fibonacci word. In order to obtain the nth fibonacci word ( <= 37 ), we start with the 37th fibonacci word and take the subsequence from index 0 to the nth fibonacci number, as in the standard fibonacci sequence.

```factor
USING: assocs combinators formatting kernel math math.functions
math.ranges math.statistics namespaces pair-rocket sequences ;
IN: rosetta-code.fibonacci-word

SYMBOL: 37th-fib-word

: fib ( n -- m )
    {
        1 => [ 1 ]
        2 => [ 1 ]
        [ [ 1 - fib ] [ 2 - fib ] bi + ]
    } case ;

: fib-word ( n -- seq )
    {
        1 => [ "1" ]
        2 => [ "0" ]
        [ [ 1 - fib-word ] [ 2 - fib-word ] bi append ]
    } case ;

: nth-fib-word ( n -- seq )
    dup 1 =
    [ drop "1" ] [ 37th-fib-word get swap fib head ] if ;

: entropy ( seq -- entropy )
    [ length ] [ histogram >alist [ second ] map ] bi
    [ swap / ] with map
    [ dup log 2 log / * ] map-sum
    dup 0. = [ neg ] unless ;

37 fib-word 37th-fib-word set
"N" "Length" "Entropy" "%2s  %8s  %10s\n" printf
37 [1,b] [
   dup nth-fib-word [ length ] [ entropy ] bi
   "%2d  %8d  %.8f\n" printf
] each
```

```txt

 N    Length     Entropy
 1         1  0.00000000
 2         1  0.00000000
 3         2  1.00000000
 4         3  0.91829583
 5         5  0.97095059
 6         8  0.95443400
 7        13  0.96123660
 8        21  0.95871188
 9        34  0.95968689
10        55  0.95931603
11        89  0.95945792
12       144  0.95940375
13       233  0.95942445
14       377  0.95941654
15       610  0.95941956
16       987  0.95941841
17      1597  0.95941885
18      2584  0.95941868
19      4181  0.95941875
20      6765  0.95941872
21     10946  0.95941873
22     17711  0.95941873
23     28657  0.95941873
24     46368  0.95941873
25     75025  0.95941873
26    121393  0.95941873
27    196418  0.95941873
28    317811  0.95941873
29    514229  0.95941873
30    832040  0.95941873
31   1346269  0.95941873
32   2178309  0.95941873
33   3524578  0.95941873
34   5702887  0.95941873
35   9227465  0.95941873
36  14930352  0.95941873
37  24157817  0.95941873

```



## FreeBASIC


```FreeBASIC
' version 25-06-2015
' compile with: fbc -s console

Function calc_entropy(source As String, base_ As Integer) As Double

    Dim As Integer i, sourcelen = Len(source), totalchar(255)
    Dim As Double prop, entropy

    For i = 0 To sourcelen -1
        totalchar(source[i]) += 1
    Next

    For i = 0 To 255
        If totalchar(i) = 0 Then Continue For
        prop = totalchar(i) / sourcelen
        entropy = entropy - (prop * Log (prop) / Log(base_))
    Next

    Return entropy

End Function

' ------=< MAIN >=------

Dim As String fw1 = "1" , fw2 = "0", fw3
Dim As Integer i, n

Print"   N     Length  Entropy           Word"
n = 1
Print Using " ###";n; : Print Using " ###########"; Len(fw1);
Print Using " ##.############### "; calc_entropy(fw1,2);
Print fw1
n = 2
Print Using " ###";n ;: Print Using " ###########"; Len(fw2);
Print Using " ##.############### "; calc_entropy(fw2,2);
Print  fw2


For n = 1 To 35

    fw1 = "1" : fw2 = "0" ' construct string
    For i = 1 To n
        fw3 = fw2 + fw1
        Swap fw1, fw2    ' swap pointers of fw1 and fw2
        Swap fw2, fw3    ' swap pointers of fw2 and fw3
    Next
    fw1 = "" : fw3 = ""  ' free up memory

    Print Using " ### ########### ##.############### "; n +2; Len(fw2);_
                                                  calc_entropy(fw2, 2);
    If Len(fw2) < 55 Then Print fw2 Else Print

Next

Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
   N     Length  Entropy           Word
   1           1  0.000000000000000 1
   2           1  0.000000000000000 0
   3           2  1.000000000000000 01
   4           3  0.918295834054490 010
   5           5  0.970950594454669 01001
   6           8  0.954434002924965 01001010
   7          13  0.961236604722876 0100101001001
   8          21  0.958711882977132 010010100100101001010
   9          34  0.959686893774217 0100101001001010010100100101001001
  10          55  0.959316032054378
  11          89  0.959457915838670
  12         144  0.959403754221023
  13         233  0.959424446955987
  14         377  0.959416543740441
  15         610  0.959419562603144
  16         987  0.959418409515224
  17        1597  0.959418849957810
  18        2584  0.959418681724032
  19        4181  0.959418745983664
  20        6765  0.959418721438676
  21       10946  0.959418730814028
  22       17711  0.959418727232962
  23       28657  0.959418728600807
  24       46368  0.959418728078337
  25       75025  0.959418728277903
  26      121393  0.959418728201675
  27      196418  0.959418728230792
  28      317811  0.959418728219670
  29      514229  0.959418728223918
  30      832040  0.959418728222296
  31     1346269  0.959418728222916
  32     2178309  0.959418728222679
  33     3524578  0.959418728222769
  34     5702887  0.959418728222735
  35     9227465  0.959418728222748
  36    14930352  0.959418728222743
  37    24157817  0.959418728222745
```



## Go


```go
package main

import (
	"fmt"
	"math"
)

// From http://rosettacode.org/wiki/Entropy#Go
func entropy(s string) float64 {
	m := map[rune]float64{}
	for _, r := range s {
		m[r]++
	}
	hm := 0.
	for _, c := range m {
		hm += c * math.Log2(c)
	}
	l := float64(len(s))
	return math.Log2(l) - hm/l
}

const F_Word1 = "1"
const F_Word2 = "0"

func FibonacciWord(n int) string {
	a, b := F_Word1, F_Word2
	for ; n > 1; n-- {
		a, b = b, b+a
	}
	return a
}

func FibonacciWordGen() <-chan string {
	ch := make(chan string)
	go func() {
		a, b := F_Word1, F_Word2
		for {
			ch <- a
			a, b = b, b+a
		}
	}()
	return ch
}

func main() {
	fibWords := FibonacciWordGen()
	fmt.Printf("%3s %9s  %-18s  %s\n", "N", "Length", "Entropy", "Word")
	n := 1
	for ; n < 10; n++ {
		s := <-fibWords
		// Just to show the function and generator do the same thing:
		if s2 := FibonacciWord(n); s != s2 {
			fmt.Printf("For %d, generator produced %q, function produced %q\n", n, s, s2)
		}
		fmt.Printf("%3d %9d  %.16f  %s\n", n, len(s), entropy(s), s)
	}
	for ; n <= 37; n++ {
		s := <-fibWords
		fmt.Printf("%3d %9d  %.16f\n", n, len(s), entropy(s))
	}
}
```

[http://play.golang.org/p/e1whcGGOU1 Run in the Go Playground.]

```txt
  N    Length  Entropy             Word
  1         1  0.0000000000000000  1
  2         1  0.0000000000000000  0
[...]
 37  24157817  0.9594187282227438
```



## Haskell



```Haskell
module Main where

import Control.Monad
import Data.List
import Data.Monoid
import Text.Printf

entropy :: (Ord a) => [a] -> Double
entropy = sum
        . map (\c -> (c *) . logBase 2 $ 1.0 / c)
        . (\cs -> let { sc = sum cs } in map (/ sc) cs)
        . map (fromIntegral . length)
        . group
        . sort

fibonacci :: (Monoid m) => m -> m -> [m]
fibonacci a b = unfoldr (\(a,b) -> Just (a, (b, a <> b))) (a,b)

main :: IO ()
main = do
    printf "%2s %10s %17s %s\n" "N" "length" "entropy" "word"
    zipWithM_ (\i v -> let { l = length v } in printf "%2d %10d %.15f %s\n"
                   i l (entropy v) (if l > 40 then "..." else v))
              [1..38::Int]
              (take 37 $ fibonacci "1" "0")
```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both Icon and Unicon.  The first eight Fibonacci
words are shown, while the Fibonacci word length and [[Entropy]] are shown for all 37.


```unicon
procedure main(A)
    n := integer(A[1]) | 37
    write(right("N",4)," ",right("length",15)," ",left("Entrophy",15)," ",
          " Fibword")
    every w := fword(i := 1 to n) do {
        writes(right(i,4)," ",right(*w,15)," ",left(H(w),15))
        if i <= 8 then write(": ",w) else write()
        }
end

procedure fword(n)
    static fcache
    initial fcache := table()
    /fcache[n] := case n of {
                     1: "1"
                     2: "0"
                     default: fword(n-1)||fword(n-2)
                     }
    return fcache[n]
end

procedure H(s)
    P := table(0.0)
    every P[!s] +:= 1.0/*s
    every (h := 0.0) -:= P[c := key(P)] * log(P[c],2)
    return h
end
```


Sample run:


```txt

->fw
   N          length Entrophy         Fibword
   1               1 0.0            : 1
   2               1 0.0            : 0
   3               2 1.0            : 01
   4               3 0.9182958340544: 010
   5               5 0.9709505944546: 01001
   6               8 0.9544340029249: 01001010
   7              13 0.9612366047228: 0100101001001
   8              21 0.9587118829771: 010010100100101001010
   9              34 0.9596868937742
  10              55 0.9593160320543
  11              89 0.9594579158386
  12             144 0.9594037542210
  13             233 0.9594244469559
  14             377 0.9594165437404
  15             610 0.9594195626031
  16             987 0.9594184095152
  17            1597 0.9594188499578
  18            2584 0.9594186817240
  19            4181 0.9594187459836
  20            6765 0.9594187214387
  21           10946 0.9594187308140
  22           17711 0.9594187272330
  23           28657 0.9594187286009
  24           46368 0.9594187280783
  25           75025 0.9594187282781
  26          121393 0.9594187282015
  27          196418 0.9594187282313
  28          317811 0.9594187282195
  29          514229 0.9594187282251
  30          832040 0.9594187282196
  31         1346269 0.9594187282169
  32         2178309 0.9594187282191
  33         3524578 0.9594187282130
  34         5702887 0.9594187282322
  35         9227465 0.9594187281818
  36        14930352 0.9594187282743
  37        24157817 0.9594187282928
->

```



## J

Implementation:

```J
F_Words=: (,<@;@:{~&_1 _2)@]^:(2-~[)&('1';'0')
```


Also, from the [[Entropy#J|entropy]] page we need:

```J
entropy=:  +/@:-@(* 2&^.)@(#/.~ % #)
```


Task example:


```J
   (,.~#\)(#,entropy)@> F_Words 37
 1         1        0
 2         1        0
 3         2        1
 4         3 0.918296
 5         5 0.970951
 6         8 0.954434
 7        13 0.961237
 8        21 0.958712
 9        34 0.959687
10        55 0.959316
11        89 0.959458
12       144 0.959404
13       233 0.959424
14       377 0.959417
15       610  0.95942
16       987 0.959418
17      1597 0.959419
18      2584 0.959419
19      4181 0.959419
20      6765 0.959419
21     10946 0.959419
22     17711 0.959419
23     28657 0.959419
24     46368 0.959419
25     75025 0.959419
26    121393 0.959419
27    196418 0.959419
28    317811 0.959419
29    514229 0.959419
30    832040 0.959419
31 1.34627e6 0.959419
32 2.17831e6 0.959419
33 3.52458e6 0.959419
34 5.70289e6 0.959419
35 9.22747e6 0.959419
36 1.49304e7 0.959419
37 2.41578e7 0.959419
```



## Java


```Java
import java.util.*;

public class FWord {
    private /*v*/ String fWord0 = "";
    private /*v*/ String fWord1 = "";

    private String nextFWord () {
        final String result;

        if ( "".equals ( fWord1 ) )      result = "1";
        else if ( "".equals ( fWord0 ) ) result = "0";
        else                             result = fWord1 + fWord0;

        fWord0 = fWord1;
        fWord1 = result;

        return result;
    }

    public static double entropy ( final String source ) {
        final int                        length = source.length ();
        final Map < Character, Integer > counts = new HashMap < Character, Integer > ();
        /*v*/ double                     result = 0.0;

        for ( int i = 0; i < length; i++ ) {
            final char c = source.charAt ( i );

            if ( counts.containsKey ( c ) ) counts.put ( c, counts.get ( c ) + 1 );
            else                            counts.put ( c, 1 );
        }

        for ( final int count : counts.values () ) {
            final double proportion = ( double ) count / length;

            result -= proportion * ( Math.log ( proportion ) / Math.log ( 2 ) );
        }

        return result;
    }

    public static void main ( final String [] args ) {
        final FWord fWord = new FWord ();

        for ( int i = 0; i < 37;  ) {
            final String word = fWord.nextFWord ();

            System.out.printf ( "%3d %10d %s %n", ++i, word.length (), entropy ( word ) );
        }
    }
}
```

Output:

```txt
  1          1 0.0
  2          1 0.0
  3          2 1.0
  4          3 0.9182958340544896
  5          5 0.9709505944546686
  6          8 0.9544340029249649
  7         13 0.961236604722876
  8         21 0.9587118829771318
  9         34 0.9596868937742169
 10         55 0.9593160320543777
 11         89 0.9594579158386696
 12        144 0.959403754221023
 13        233 0.9594244469559867
 14        377 0.9594165437404407
 15        610 0.9594195626031441
 16        987 0.9594184095152245
 17       1597 0.9594188499578099
 18       2584 0.9594186817240321
 19       4181 0.9594187459836638
 20       6765 0.9594187214386756
 21      10946 0.9594187308140278
 22      17711 0.959418727232962
 23      28657 0.9594187286008073
 24      46368 0.9594187280783371
 25      75025 0.9594187282779029
 26     121393 0.9594187282016755
 27     196418 0.9594187282307918
 28     317811 0.9594187282196702
 29     514229 0.9594187282239184
 30     832040 0.9594187282222959
 31    1346269 0.9594187282229156
 32    2178309 0.9594187282226789
 33    3524578 0.9594187282227691
 34    5702887 0.9594187282227347
 35    9227465 0.9594187282227479
 36   14930352 0.9594187282227429
 37   24157817 0.9594187282227448

```



## JavaScript


```JavaScript
//makes outputting a table possible in environments
//that don't support console.table()
function console_table(xs) {
    function pad(n,s) {
        var res = s;
        for (var i = s.length; i < n; i++)
            res += " ";
        return res;
    }

    if (xs.length === 0)
        console.log("No data");
    else {
        var widths = [];
        var cells = [];
        for (var i = 0; i <= xs.length; i++)
            cells.push([]);

        for (var s in xs[0]) {
            var len = s.length;
            cells[0].push(s);

            for (var i = 0; i < xs.length; i++) {
                var ss = "" + xs[i][s];
                len = Math.max(len, ss.length);
                cells[i+1].push(ss);
            }
            widths.push(len);
        }
        var s = "";
        for (var x = 0; x < cells.length; x++) {
            for (var y = 0; y < widths.length; y++)
                s += "|" + pad(widths[y], cells[x][y]);
            s += "|\n";
        }
        console.log(s);
    }
}

//returns the entropy of a string as a number
function entropy(s) {
     //create an object containing each individual char
	//and the amount of iterations per char
    function prob(s) {
        var h = Object.create(null);
        s.split('').forEach(function(c) {
           h[c] && h[c]++ || (h[c] = 1);
        });
        return h;
    }

    s = s.toString(); //just in case
    var e = 0, l = s.length, h = prob(s);

    for (var i in h ) {
        var p = h[i]/l;
        e -= p * Math.log(p) / Math.log(2);
    }
    return e;
}

//creates Fibonacci Word to n as described on Rosetta Code
//see rosettacode.org/wiki/Fibonacci_word
function fibWord(n) {
    var wOne = "1", wTwo = "0", wNth = [wOne, wTwo], w = "", o = [];

    for (var i = 0; i < n; i++) {
        if (i === 0 || i === 1) {
            w = wNth[i];
        } else {
            w = wNth[i - 1] + wNth[i - 2];
            wNth.push(w);
        }
        var l = w.length;
        var e = entropy(w);

        if (l <= 21) {
        	o.push({
            	N: i + 1,
            	Length: l,
            	Entropy: e,
            	Word: w
        	});
        } else {
        	o.push({
            	N: i + 1,
            	Length: l,
            	Entropy: e,
            	Word: "..."
        	});
        }
    }

    try {
    	console.table(o);
    } catch (err) {
    	console_table(o);
    }
}

fibWord(37);
```

Output:

```txt
|N |Length  |Entropy           |Word                 |
|1 |1       |0                 |1                    |
|2 |1       |0                 |0                    |
|3 |2       |1                 |01                   |
|4 |3       |0.9182958340544896|010                  |
|5 |5       |0.9709505944546688|01001                |
|6 |8       |0.954434002924965 |01001010             |
|7 |13      |0.961236604722876 |0100101001001        |
|8 |21      |0.9587118829771318|010010100100101001010|
|9 |34      |0.9596868937742169|...                  |
|10|55      |0.9593160320543777|...                  |
|11|89      |0.9594579158386696|...                  |
|12|144     |0.959403754221023 |...                  |
|13|233     |0.9594244469559867|...                  |
|14|377     |0.9594165437404407|...                  |
|15|610     |0.9594195626031441|...                  |
|16|987     |0.9594184095152245|...                  |
|17|1597    |0.9594188499578098|...                  |
|18|2584    |0.9594186817240322|...                  |
|19|4181    |0.9594187459836638|...                  |
|20|6765    |0.9594187214386755|...                  |
|21|10946   |0.9594187308140276|...                  |
|22|17711   |0.959418727232962 |...                  |
|23|28657   |0.9594187286008075|...                  |
|24|46368   |0.959418728078337 |...                  |
|25|75025   |0.959418728277903 |...                  |
|26|121393  |0.9594187282016755|...                  |
|27|196418  |0.9594187282307918|...                  |
|28|317811  |0.9594187282196702|...                  |
|29|514229  |0.9594187282239184|...                  |
|30|832040  |0.9594187282222958|...                  |
|31|1346269 |0.9594187282229155|...                  |
|32|2178309 |0.9594187282226788|...                  |
|33|3524578 |0.9594187282227693|...                  |
|34|5702887 |0.9594187282227347|...                  |
|35|9227465 |0.9594187282227479|...                  |
|36|14930352|0.9594187282227428|...                  |
|37|24157817|0.9594187282227447|...                  |
```




## jq

'''Entropy''':

```jq
# Input: an array of strings.
# Output: an object with the strings as keys,
# the values of which are the corresponding frequencies.
def counter:
  reduce .[] as $item ( {}; .[$item] += 1 ) ;

# entropy in bits of the input string
def entropy:
  (explode | map( [.] | implode ) | counter | [ .[] | . * (.|log) ] | add) as $sum
  | ((length|log) - ($sum / length)) / (2|log) ;
```

'''Pretty printing''':

```jq
# truncate n places after the decimal point;
# return a string since it can readily be converted back to a number
def precision(n):
  tostring as $s | $s | index(".")
  | if . then $s[0:.+n+1] else $s end ;

# Right-justify but do not truncate
def rjustify(n):
  tostring | length as $length
  | if n <= $length then . else " " * (n-$length) + . end;

# Attempt to align decimals so integer part is in a field of width n
def align(n):
  tostring | index(".") as $ix
  | if n < $ix then .
    elif $ix then (.[0:$ix]|rjustify(n)) +.[$ix:]
    else rjustify(n)
    end ;

```

'''The task''':

```jq
# Generate the first n terms of the Fibonacci word sequence
# as a stream of arrays of the form [index, word]
def fibonacci_words(n):
  # input: [f(i-2), f(i-1), countdown, counter]
  def fib:
    if .[2] == 1 then [.[3], .[0]]
    else
      (.[1] + .[0]) as $sum
      | [ .[3], .[0]], ([ .[1], $sum, (.[2] - 1), (.[3] + 1) ] | fib)
    end;
  if n <= 0 then empty
  else (["1", "0", n, 1] | fib)
  end;

def task(n):
  fibonacci_words(n)
  | .[0] as $i
  | (.[1]|length) as $len
  | (.[1]|entropy) as $e
  | "\($i|rjustify(3)) \($len|rjustify(10))  \($e|precision(6))"
;

task(37)

```

{{Out}} (head and tail)

```txt
$ jq -n -r -f fibonacci_word.rc
  1          1  0
  2          1  0
  3          2  1
  4          3  0.918295
  5          5  0.970950
  6          8  0.954434
  7         13  0.961236
  8         21  0.958711
  9         34  0.959686
 10         55  0.959316
 11         89  0.959457
 12        144  0.959403
 13        233  0.959424
 14        377  0.959416
 15        610  0.959419
 16        987  0.959418
 ...
 36   14930352  0.959418
 37   24157817  0.959418
```



## Julia

```julia
using DataStructures
entropy(s::AbstractString) = -sum(x -> x / length(s) * log2(x / length(s)), values(counter(s)))

function fibboword(n::Int64)
    # Initialize the result
    r = Array{String}(n)
    # First element
    r[1] = "0"
    # If more than 2, set the second element
    if n ≥ 2 r[2] = "1" end
    # Recursively create elements > 3
    for i in 3:n
        r[i] = r[i - 1] * r[i - 2]
    end
    return r
end

function testfibbo(n::Integer)
    fib = fibboword(n)
    for i in 1:length(fib)
        @printf("%3d%9d%12.6f\n", i, length(fib[i]), entropy(fib[i]))
    end
    return 0
end

println("  n\tlength\tentropy")
testfibbo(37)
```


```txt
  n	length	entropy
  1        1   -0.000000
  2        1   -0.000000
  3        2    1.000000
  4        3    0.918296
  5        5    0.970951
  6        8    0.954434
  7       13    0.961237
  8       21    0.958712
  9       34    0.959687
 10       55    0.959316
 11       89    0.959458
 12      144    0.959404
 13      233    0.959424
 14      377    0.959417
 15      610    0.959420
 16      987    0.959418
 17     1597    0.959419
 18     2584    0.959419
 19     4181    0.959419
 20     6765    0.959419
 21    10946    0.959419
 22    17711    0.959419
 23    28657    0.959419
 24    46368    0.959419
 25    75025    0.959419
 26   121393    0.959419
 27   196418    0.959419
 28   317811    0.959419
 29   514229    0.959419
 30   832040    0.959419
 31  1346269    0.959419
 32  2178309    0.959419
 33  3524578    0.959419
 34  5702887    0.959419
 35  9227465    0.959419
 36 14930352    0.959419
 37 24157817    0.959419
```



## Kotlin


```scala
// version 1.0.6

fun fibWord(n: Int): String {
    if (n < 1) throw IllegalArgumentException("Argument can't be less than 1")
    if (n == 1) return "1"
    val words = Array(n){ "" }
    words[0] = "1"
    words[1] = "0"
    for (i in 2 until n) words[i] = words[i - 1] + words[i - 2]
    return words[n - 1]
}

fun log2(d: Double) = Math.log(d) / Math.log(2.0)

fun shannon(s: String): Double {
    if (s.length <= 1) return 0.0
    val count0 = s.count { it == '0' }
    val count1 = s.length - count0
    val nn = s.length.toDouble()
    return -(count0 / nn * log2(count0 / nn) + count1 / nn * log2(count1 / nn))
}

fun main(args: Array<String>) {
    println("N    Length       Entropy             Word")
    println("--  --------  ------------------  ----------------------------------")
    for (i in 1..37) {
        val s = fibWord(i)
        print(String.format("%2d  %8d  %18.16f", i, s.length, shannon(s)))
        if (i < 10) println("  $s")
        else println()
    }
}
```


```txt

N    Length       Entropy             Word
--  --------  ------------------  ----------------------------------
 1         1  0.0000000000000000  1
 2         1  0.0000000000000000  0
 3         2  1.0000000000000000  01
 4         3  0.9182958340544896  010
 5         5  0.9709505944546686  01001
 6         8  0.9544340029249649  01001010
 7        13  0.9612366047228760  0100101001001
 8        21  0.9587118829771318  010010100100101001010
 9        34  0.9596868937742169  0100101001001010010100100101001001
10        55  0.9593160320543777
11        89  0.9594579158386696
12       144  0.9594037542210230
13       233  0.9594244469559867
14       377  0.9594165437404407
15       610  0.9594195626031441
16       987  0.9594184095152245
17      1597  0.9594188499578099
18      2584  0.9594186817240321
19      4181  0.9594187459836638
20      6765  0.9594187214386756
21     10946  0.9594187308140278
22     17711  0.9594187272329620
23     28657  0.9594187286008073
24     46368  0.9594187280783371
25     75025  0.9594187282779029
26    121393  0.9594187282016755
27    196418  0.9594187282307918
28    317811  0.9594187282196702
29    514229  0.9594187282239184
30    832040  0.9594187282222959
31   1346269  0.9594187282229156
32   2178309  0.9594187282226789
33   3524578  0.9594187282227691
34   5702887  0.9594187282227347
35   9227465  0.9594187282227479
36  14930352  0.9594187282227429
37  24157817  0.9594187282227448

```



## Lua


```Lua
-- Return the base two logarithm of x
function log2 (x) return math.log(x) / math.log(2) end

-- Return the Shannon entropy of X
function entropy (X)
    local N, count, sum, i = X:len(), {}, 0
    for char = 1, N do
        i = X:sub(char, char)
        if count[i] then
            count[i] = count[i] + 1
        else
            count[i] = 1
        end
    end
    for n_i, count_i in pairs(count) do
        sum = sum + count_i / N * log2(count_i / N)
    end
    return -sum
end

-- Return a table of the first n Fibonacci words
function fibWords (n)
    local fw = {1, 0}
    while #fw < n do fw[#fw + 1] = fw[#fw] .. fw[#fw - 1] end
    return fw
end

-- Main procedure
print("n\tWord length\tEntropy")
for k, v in pairs(fibWords(37)) do
    v = tostring(v)
    io.write(k .. "\t" .. #v)
    if string.len(#v) < 8 then io.write("\t") end
    print("\t" .. entropy(v))
end
```

```txt
n       Word length     Entropy
1       1               -0
2       1               -0
3       2               1
4       3               0.91829583405449
5       5               0.97095059445467
6       8               0.95443400292496
7       13              0.96123660472288
8       21              0.95871188297713
9       34              0.95968689377422
10      55              0.95931603205438
11      89              0.95945791583867
12      144             0.95940375422102
13      233             0.95942444695599
14      377             0.95941654374044
15      610             0.95941956260314
16      987             0.95941840951522
17      1597            0.95941884995781
18      2584            0.95941868172403
19      4181            0.95941874598366
20      6765            0.95941872143868
21      10946           0.95941873081403
22      17711           0.95941872723296
23      28657           0.95941872860081
24      46368           0.95941872807834
25      75025           0.9594187282779
26      121393          0.95941872820168
27      196418          0.95941872823079
28      317811          0.95941872821967
29      514229          0.95941872822392
30      832040          0.9594187282223
31      1346269         0.95941872822292
32      2178309         0.95941872822268
33      3524578         0.95941872822277
34      5702887         0.95941872822273
35      9227465         0.95941872822275
36      14930352        0.95941872822274
37      24157817        0.95941872822274
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
entropy = (p - 1) Log[2, 1 - p] - p Log[2, p];

TableForm[
 Table[{k, Fibonacci[k],
   Quiet@Check[N[entropy /. {p -> Fibonacci[k - 1]/Fibonacci[k]}, 15],
      0]}, {k, 37}],
 TableHeadings -> {None, {"N", "Length", "Entropy"}}]
```


```txt
N	Length		Entropy
1	1		0
2	1		0
3	2		1.00000000000000
4	3		0.918295834054490
5	5		0.970950594454669
6	8		0.954434002924965
7	13		0.961236604722876
8	21		0.958711882977132
9	34		0.959686893774217
10	55		0.959316032054378
11	89		0.959457915838669
12	144		0.959403754221023
13	233		0.959424446955987
14	377		0.959416543740441
15	610		0.959419562603144
16	987		0.959418409515224
17	1597		0.959418849957810
18	2584		0.959418681724032
19	4181		0.959418745983664
20	6765		0.959418721438675
21	10946		0.959418730814028
22	17711		0.959418727232962
23	28657		0.959418728600807
24	46368		0.959418728078337
25	75025		0.959418728277903
26	121393		0.959418728201675
27	196418		0.959418728230792
28	317811		0.959418728219670
29	514229		0.959418728223918
30	832040		0.959418728222296
31	1346269		0.959418728222916
32	2178309		0.959418728222679
33	3524578		0.959418728222769
34	5702887		0.959418728222735
35	9227465		0.959418728222748
36	14930352	0.959418728222743
37	24157817	0.959418728222745

```



## Objeck


```objeck
use Collection;

class FibonacciWord {
  function : native : GetEntropy(result : String) ~ Float {
    frequencies := IntMap->New();

    each(i : result) {
      c := result->Get(i);

      if(frequencies->Has(c)) {
        count := frequencies->Find(c)->As(IntHolder);
        count->Set(count->Get() + 1);
      }
      else {
        frequencies->Insert(c, IntHolder->New(1));
      };
    };

    length := result->Size();
    entropy := 0.0;

    counts := frequencies->GetValues();
    each(i : counts) {
      count := counts->Get(i)->As(IntHolder)->Get();
      freq := count->As(Float) / length;
      entropy += freq * (freq->Log() / 2.0->Log());
    };

    return -1 * entropy;
  }

  function : native : PrintLine(n : Int, result : String) ~ Nil {
    n->Print();
    '\t'->Print();

    result->Size()->Print();
    "\t\t"->Print();

    GetEntropy(result)->PrintLine();
    }

  function : Main(args : String[]) ~ Nil {
    firstString := "1";
    n := 1;
    PrintLine( n, firstString );
    secondString := "0";
    n += 1;
    PrintLine( n, secondString );

    while(n < 37) {
      resultString := "{$secondString}{$firstString}";
      firstString := secondString;
      secondString := resultString;
      n  += 1;
      PrintLine( n, resultString );
    };
  }
}
```


Output:

```txt

1       1		-0
2       1		-0
3       2		1
4       3		0.918295834
5       5		0.970950594
6       8		0.954434003
7       13		0.961236605
8       21		0.958711883
9       34		0.959686894
10      55		0.959316032
11      89		0.959457916
12      144		0.959403754
13      233		0.959424447
14      377		0.959416544
15      610		0.959419563
16      987		0.95941841
17      1597		0.95941885
18      2584		0.959418682
19      4181		0.959418746
20      6765		0.959418721
21      10946		0.959418731
22      17711		0.959418727
23      28657		0.959418729
24      46368		0.959418728
25      75025		0.959418728
26      121393		0.959418728
27      196418		0.959418728
28      317811		0.959418728
29      514229		0.959418728
30      832040		0.959418728
31      1346269		0.959418728
32      2178309		0.959418728
33      3524578		0.959418728
34      5702887		0.959418728
35      9227465		0.959418728
36      14930352	0.959418728
37      24157817	0.959418728

```



## Oforth



```Oforth
: entropy(s) -- f
| freq sz |
   s size dup ifZero: [ return ] asFloat ->sz
   ListBuffer initValue(255, 0) ->freq
   s apply( #[ dup freq at 1+ freq put ] )
   0.0 freq applyIf( #[ 0 <> ], #[ sz / dup ln * - ] ) Ln2 / ;


: FWords(n)
| ws i |
   ListBuffer new dup add("1") dup add("0") dup ->ws
   3 n for: i [ i 1- ws at  i 2 - ws at  +  ws add ]
   dup map(#[ dup size swap entropy Pair new]) apply(#println) ;
```


```txt

FWords(37)
[1, 0]
[1, 0]
[2, 1]
[3, 0.918295834054489]
[5, 0.970950594454669]
[8, 0.954434002924965]
[13, 0.961236604722876]
[21, 0.958711882977132]
[34, 0.959686893774217]
[55, 0.959316032054378]
[89, 0.95945791583867]
[144, 0.959403754221023]
[233, 0.959424446955987]
[377, 0.959416543740441]
[610, 0.959419562603144]
[987, 0.959418409515225]
[1597, 0.95941884995781]
[2584, 0.959418681724032]
[4181, 0.959418745983664]
[6765, 0.959418721438675]
[10946, 0.959418730814028]
[17711, 0.959418727232962]
[28657, 0.959418728600807]
[46368, 0.959418728078337]
[75025, 0.959418728277903]
[121393, 0.959418728201676]
[196418, 0.959418728230792]
[317811, 0.95941872821967]
[514229, 0.959418728223918]
[832040, 0.959418728222296]
[1346269, 0.959418728222916]
[2178309, 0.959418728222679]
[3524578, 0.959418728222769]
[5702887, 0.959418728222735]
[9227465, 0.959418728222748]
[14930352, 0.959418728222743]
[24157817, 0.959418728222745]

```



## ooRexx

```oorexx
/* REXX ---------------------------------------------------------------
* 09.08.2014 Walter Pachl 'copied' from REXX
* lists the # of chars in fibonacci words and the words' entropy
* as well as (part of) the Fibonacci word and the number of 0's and 1's
* Note: ooRexx allows for computing up to 47 Fibonacci words
*--------------------------------------------------------------------*/
  Numeric Digits 20                /* use more precision,  default=9.*/
  Parse Arg n fw.1 fw.2 .          /* get optional args from the C.L.*/
  If n=='' Then n=50               /* Not specified? Then use default*/
  If fw.1=='' Then fw.1=1          /* "      "        "   "     "    */
  If fw.2=='' Then fw.2=0          /* "      "        "   "     "    */
  hdr1=' N     length  Entropy                 Fibonacci word    ',
                                                '# of zeroes # of ones'
  hdr2='-- ----------  ----------------------  --------------------',
                                                  '--------- ---------'
  Say hdr1
  Say hdr2
  Do j=1 For n                     /* display  N  fibonacci words.   */
    j1=j-1
    j2=j-2
    If j>2 Then                    /* calculate FIBword if we need to*/
      fw.j=fw.j1||fw.j2
    If length(fw.j)<20 Then
      fwd=left(fw.j,20)            /* display the Fibonacci word     */
    Else
      fwd=left(fw.j,5)'...'right(fw.j,12) /* display parts thereof   */
    Say right(j,2)'  'right(length(fw.j),9)'  'entropy(fw.j)'  'fwd,
                                            right(aa.0,9) right(aa.1,9)
    End
  Say hdr2
  Say hdr1
  Exit

entropy: Procedure Expose aa.
  Parse Arg dd
  l=length(dd)
  d=digits()
  aa.0=l-length(space(translate(dd,,0),0)) /*fast way to count zeroes*/
  aa.1=l-aa.0                      /* and figure the number of ones. */
  If l==1 Then
    Return left(0,d+2)             /* handle special case of one char*/
  s=0                              /* [?] calc entropy for each char */
  do i=1 for 2
    _=i-1                          /* construct a chr from the ether.*/
    p=aa._/l                       /* 'probability of aa-_ in fw     */
    s=s-p*rxmlog(p,d,2)            /* add (negatively) the entropies.*/
    End
  If s=1 Then
    Return left(1,d+2)             /* return a left-justified  "1".  */
  Return format(s,,d)              /* normalize the number (sum or S)*/
::requires rxm.cls
```

```txt
  N     length  Entropy                 Fibonacci word     # of zeroes # of ones
-- ----------  ----------------------  -------------------- --------- ---------
 1          1  0                       1                            0         1
 2          1  0                       0                            1         0
 3          2  1                       01                           1         1
 4          3  0.91829583405448951479  010                          2         1
 5          5  0.97095059445466863901  01001                        3         2
 6          8  0.95443400292496496456  01001010                     5         3
 7         13  0.96123660472287587273  0100101001001                8         5
 8         21  0.95871188297713180865  01001...100101001010        13         8
 9         34  0.95968689377421693318  01001...100101001001        21        13
10         55  0.95931603205437767776  01001...100101001010        34        21
11         89  0.95945791583866946165  01001...100101001001        55        34
12        144  0.95940375422102292948  01001...100101001010        89        55
13        233  0.95942444695598675866  01001...100101001001       144        89
14        377  0.95941654374044073871  01001...100101001010       233       144
15        610  0.95941956260314415022  01001...100101001001       377       233
16        987  0.95941840951522431271  01001...100101001010       610       377
17       1597  0.95941884995780985566  01001...100101001001       987       610
18       2584  0.95941868172403210666  01001...100101001010      1597       987
19       4181  0.95941874598366381432  01001...100101001001      2584      1597
20       6765  0.95941872143867541462  01001...100101001010      4181      2584
21      10946  0.95941873081402772314  01001...100101001001      6765      4181
22      17711  0.95941872723296194268  01001...100101001010     10946      6765
23      28657  0.95941872860080737603  01001...100101001001     17711     10946
24      46368  0.95941872807833691493  01001...100101001010     28657     17711
25      75025  0.95941872827790287342  01001...100101001001     46368     28657
26     121393  0.95941872820167546032  01001...100101001010     75025     46368
27     196418  0.95941872823079174125  01001...100101001001    121393     75025
28     317811  0.95941872821967031157  01001...100101001010    196418    121393
29     514229  0.95941872822391831971  01001...100101001001    317811    196418
30     832040  0.95941872822229572500  01001...100101001010    514229    317811
31    1346269  0.95941872822291550102  01001...100101001001    832040    514229
32    2178309  0.95941872822267876765  01001...100101001010   1346269    832040
33    3524578  0.95941872822276919174  01001...100101001001   2178309   1346269
34    5702887  0.95941872822273465282  01001...100101001010   3524578   2178309
35    9227465  0.95941872822274784553  01001...100101001001   5702887   3524578
36   14930352  0.95941872822274280637  01001...100101001010   9227465   5702887
37   24157817  0.95941872822274473113  01001...100101001001  14930352   9227465
38   39088169  0.95941872822274399592  01001...100101001010  24157817  14930352
39   63245986  0.95941872822274427677  01001...100101001001  39088169  24157817
40  102334155  0.95941872822274416950  01001...100101001010  63245986  39088169
41  165580141  0.95941872822274421049  01001...100101001001 102334155  63245986
42  267914296  0.95941872822274419481  01001...100101001010 165580141 102334155
43  433494437  0.95941872822274420081  01001...100101001001 267914296 165580141
44  701408733  0.95941872822274419851  01001...100101001010 433494437 267914296
45  134903170  0.95941872822274419940  01001...100101001001 701408733 433494437
46  836311903  0.95941872822274419905  01001...100101001010 134903170 701408733
47  971215073  0.95941872822274419920  01001...100101001001 836311903 134903170
    22 *-*       fw.j=fw.j1||fw.j2
Error 5 running D:\fwoo.rex line 22:  System resources exhausted
```



## PARI/GP


```parigp
ent(a,b)=[a,b]=[a,b]/(a+b);(a*log(if(a,a,1))+b*log(if(b,b,1)))/log(1/2)
allocatemem(75<<20) \\ Allocate 75 MB stack space
F=vector(37);F[1]="1";F[2]="0";for(n=3,37,F[n]=Str(F[n-1],F[n-2]))
for(n=1,37,print(n" "fibonacci(n)" "ent(fibonacci(n-1),fibonacci(n-2))))
```


For those output fascists:

```txt
1 1 0.E-9
2 1 0.E-9
3 2 1.00000000
4 3 0.918295834
5 5 0.970950594
6 8 0.954434003
7 13 0.961236604
8 21 0.958711883
9 34 0.959686894
10 55 0.959316032
11 89 0.959457916
12 144 0.959403754
13 233 0.959424447
14 377 0.959416544
15 610 0.959419563
16 987 0.959418409
17 1597 0.959418850
18 2584 0.959418682
19 4181 0.959418746
20 6765 0.959418721
21 10946 0.959418731
22 17711 0.959418727
23 28657 0.959418728
24 46368 0.959418728
25 75025 0.959418728
26 121393 0.959418728
27 196418 0.959418728
28 317811 0.959418728
29 514229 0.959418728
30 832040 0.959418728
31 1346269 0.959418728
32 2178309 0.959418728
33 3524578 0.959418728
34 5702887 0.959418728
35 9227465 0.959418728
36 14930352 0.959418728
37 24157817 0.959418728
```



## Pascal


As in Algol68 statet, you needn't to create the long string.

```pascal
program FibWord;
{$IFDEF DELPHI}
   {$APPTYPE CONSOLE}
{$ENDIF}
const
  FibSMaxLen = 35;
type
  tFibString = string[2*FibSMaxLen];//Ansistring;
  tFibCnt = longWord;
  tFib = record
            ZeroCnt,
            OneCnt : tFibCnt;
//            fibS   : tFibString;//didn't work :-(
         end;
var
  FibSCheck : boolean;
  Fib0,Fib1 : tFib;
  FibS0,FibS1: tFibString;

procedure  FibInit;
Begin
  with Fib0 do
  begin
    ZeroCnt := 1;
    OneCnt  := 0;
  end;

  with Fib1 do
  begin
    ZeroCnt := 0;
    OneCnt  := 1;
  end;
  FibS0 := '1';
  FibS1 := '0';
  FibSCheck := true;
end;

Function FibLength(const F:Tfib):tFibCnt;
begin
  FibLength := F.ZeroCnt+F.OneCnt;
end;

function FibEntropy(const F:Tfib):extended;
const
  rcpLn2 = 1.0/ln(2);
var
  entrp,
  ratio: extended;
begin
  entrp := 0.0;
  ratio := F.ZeroCnt/FibLength(F);
  if Ratio <> 0.0 then
    entrp :=  -ratio*ln(ratio)*rcpLn2;
  ratio := F.OneCnt/FibLength(F);
  if Ratio <> 0.0 then
    entrp :=  entrp-ratio*ln(ratio)*rcpLn2;
  FibEntropy:=entrp
end;

procedure FibSExtend;
var
  tmpS : tFibString;
begin
  IF FibSCheck then
  begin
    tmpS  := FibS0+FibS1;
    FibS0 := FibS1;
    FibS1 := tmpS;
    FibSCheck := (length(FibS1) < FibSMaxLen);
  end;
end;

procedure FibNext;
var
  tmpFib : tFib;
Begin
  tmpFib.ZeroCnt := Fib0.ZeroCnt+Fib1.ZeroCnt;
  tmpFib.OneCnt  := Fib0.OneCnt +Fib1.OneCnt;
  Fib0 := Fib1;
  Fib1 := tmpFib;
  IF FibSCheck then
    FibSExtend;
end;

procedure FibWrite(const F:Tfib);
begin
//  With F do
//    write(ZeroCnt:10,OneCnt:10,FibLength(F):10,FibEntropy(f):17:14);
  write(FibLength(F):10,FibEntropy(F):17:14);
  IF FibSCheck then
    writeln('  ',FibS1)
  else
    writeln('  ....');
end;

var
  i : integer;
BEGIN
  FibInit;
  writeln('No.     Length   Entropy         Word');
  write(1:4);FibWrite(Fib0);
  write(2:4);FibWrite(Fib1);
  For i := 3 to 37 do
  begin
    FibNext;
    write(i:4);
    FibWrite(Fib1);
  end;
END.

```

The same output:

```txt
No.     Length   Entropy         Word
   1         1-0.00000000000000  0
   2         1 0.00000000000000  0
   3         2 1.00000000000000  10
   4         3 0.91829583405449  010
   5         5 0.97095059445467  10010
   6         8 0.95443400292496  01010010
   7        13 0.96123660472288  1001001010010
   8        21 0.95871188297713  010100101001001010010
   9        34 0.95968689377422  1001001010010010100101001001010010
  10        55 0.95931603205438  ....
  11        89 0.95945791583867  ....
shortened
  35   9227465 0.95941872822275  ....
  36  14930352 0.95941872822274  ....
  37  24157817 0.95941872822274  ....

```



## Perl


```Perl
sub fiboword;
{
    my ($a, $b, $count) = (1, 0, 0);
    sub fiboword {
        $count++;
        return $a if $count == 1;
        return $b if $count == 2;
        ($a, $b) = ($b, "$b$a");
        return $b;
    }
}
sub entropy {
    my %c;
    $c{$_}++ for split //, my $str = shift;
    my $e = 0;
    for (values %c) {
        my $p = $_ / length $str;
        $e -= $p * log $p;
    }
    return $e / log 2;
}

my $count;
while ($count++ < 37) {
    my $word = fiboword;
    printf "%5d\t%10d\t%.8e\t%s\n",
    $count,
    length($word),
    entropy($word),
    $count > 9 ? '' : $word
}
```



## Perl 6



```perl6
constant @fib-word = 1, 0, { $^b ~ $^a } ... *;

sub entropy {
    -log(2) R/
        [+] map -> \p { p * log p },
            $^string.comb.Bag.values »/» $string.chars
}
for @fib-word[^37] {
    printf "%5d\t%10d\t%.8e\t%s\n",
    (state $n)++, .chars, .&entropy, $n > 10 ?? '' !! $_;
}
```


That works, but is terribly slow due to all the string processing and bag creation, just to count 0's and 1's.  By contrast, the following prints the table up to 100 almost instantly by tracking the values to calculate entropy in parallel with the actual strings. This works in Perl 6 because lazy lists are calculated on demand, so if we don't actually ask for the larger string forms, we don't calculate them.  Which would be relatively difficult for a string containing 573147844013817084101 characters, unless you happen to have a computer with a zettabyte or so of memory sitting in your garage.


```perl6
constant @fib-word = '1', '0', { $^b ~ $^a } ... *;
constant @fib-ones = 1, 0, * + * ... *;
constant @fib-chrs = 1, 1, * + * ... *;

multi entropy(0) { 0 }
multi entropy(1) { 0 }
multi entropy($n) {
    my $chars = @fib-chrs[$n];
    my $ones  = @fib-ones[$n];
    my $zeros = $chars - $ones;
    -log(2) R/
        [+] map -> \p { p * log p },
            $ones / $chars, $zeros / $chars
}

for 0..100 -> $n {
    printf "%5d\t%21d\t%.15e\t%s\n",
	    $n, @fib-chrs[$n], entropy($n), $n > 9 ?? '' !! @fib-word[$n];
}
```

```txt
    0	                    1	0.000000000000000e+00	1
    1	                    1	0.000000000000000e+00	0
    2	                    2	1.000000000000000e+00	01
    3	                    3	9.182958340544895e-01	010
    4	                    5	9.709505944546688e-01	01001
    5	                    8	9.544340029249650e-01	01001010
    6	                   13	9.612366047228759e-01	0100101001001
    7	                   21	9.587118829771317e-01	010010100100101001010
    8	                   34	9.596868937742167e-01	0100101001001010010100100101001001
    9	                   55	9.593160320543776e-01	0100101001001010010100100101001001010010100100101001010
   10	                   89	9.594579158386695e-01
   11	                  144	9.594037542210229e-01
   12	                  233	9.594244469559866e-01
   13	                  377	9.594165437404406e-01
   14	                  610	9.594195626031441e-01
   15	                  987	9.594184095152244e-01
   16	                 1597	9.594188499578099e-01
   17	                 2584	9.594186817240321e-01
   18	                 4181	9.594187459836640e-01
   19	                 6765	9.594187214386754e-01
   20	                10946	9.594187308140276e-01
   21	                17711	9.594187272329618e-01
   22	                28657	9.594187286008074e-01
   23	                46368	9.594187280783370e-01
   24	                75025	9.594187282779029e-01
   25	               121393	9.594187282016755e-01
   26	               196418	9.594187282307919e-01
   27	               317811	9.594187282196701e-01
   28	               514229	9.594187282239183e-01
   29	               832040	9.594187282222958e-01
   30	              1346269	9.594187282229156e-01
   31	              2178309	9.594187282226789e-01
   32	              3524578	9.594187282227692e-01
   33	              5702887	9.594187282227345e-01
   34	              9227465	9.594187282227477e-01
   35	             14930352	9.594187282227427e-01
   36	             24157817	9.594187282227447e-01
   37	             39088169	9.594187282227441e-01
   38	             63245986	9.594187282227441e-01
   39	            102334155	9.594187282227441e-01
   40	            165580141	9.594187282227441e-01
   41	            267914296	9.594187282227441e-01
   42	            433494437	9.594187282227441e-01
   43	            701408733	9.594187282227441e-01
   44	           1134903170	9.594187282227441e-01
   45	           1836311903	9.594187282227441e-01
   46	           2971215073	9.594187282227441e-01
   47	           4807526976	9.594187282227441e-01
   48	           7778742049	9.594187282227441e-01
   49	          12586269025	9.594187282227441e-01
   50	          20365011074	9.594187282227441e-01
   51	          32951280099	9.594187282227441e-01
   52	          53316291173	9.594187282227441e-01
   53	          86267571272	9.594187282227441e-01
   54	         139583862445	9.594187282227441e-01
   55	         225851433717	9.594187282227441e-01
   56	         365435296162	9.594187282227441e-01
   57	         591286729879	9.594187282227441e-01
   58	         956722026041	9.594187282227441e-01
   59	        1548008755920	9.594187282227441e-01
   60	        2504730781961	9.594187282227441e-01
   61	        4052739537881	9.594187282227441e-01
   62	        6557470319842	9.594187282227441e-01
   63	       10610209857723	9.594187282227441e-01
   64	       17167680177565	9.594187282227441e-01
   65	       27777890035288	9.594187282227441e-01
   66	       44945570212853	9.594187282227441e-01
   67	       72723460248141	9.594187282227441e-01
   68	      117669030460994	9.594187282227441e-01
   69	      190392490709135	9.594187282227441e-01
   70	      308061521170129	9.594187282227441e-01
   71	      498454011879264	9.594187282227441e-01
   72	      806515533049393	9.594187282227441e-01
   73	     1304969544928657	9.594187282227441e-01
   74	     2111485077978050	9.594187282227441e-01
   75	     3416454622906707	9.594187282227441e-01
   76	     5527939700884757	9.594187282227441e-01
   77	     8944394323791464	9.594187282227441e-01
   78	    14472334024676221	9.594187282227441e-01
   79	    23416728348467685	9.594187282227441e-01
   80	    37889062373143906	9.594187282227441e-01
   81	    61305790721611591	9.594187282227441e-01
   82	    99194853094755497	9.594187282227441e-01
   83	   160500643816367088	9.594187282227441e-01
   84	   259695496911122585	9.594187282227441e-01
   85	   420196140727489673	9.594187282227441e-01
   86	   679891637638612258	9.594187282227441e-01
   87	  1100087778366101931	9.594187282227441e-01
   88	  1779979416004714189	9.594187282227441e-01
   89	  2880067194370816120	9.594187282227441e-01
   90	  4660046610375530309	9.594187282227441e-01
   91	  7540113804746346429	9.594187282227441e-01
   92	 12200160415121876738	9.594187282227441e-01
   93	 19740274219868223167	9.594187282227441e-01
   94	 31940434634990099905	9.594187282227441e-01
   95	 51680708854858323072	9.594187282227441e-01
   96	 83621143489848422977	9.594187282227441e-01
   97	135301852344706746049	9.594187282227441e-01
   98	218922995834555169026	9.594187282227441e-01
   99	354224848179261915075	9.594187282227441e-01
  100	573147844013817084101	9.594187282227441e-01
```



## Phix


```Phix
function log2(atom v)
    return log(v)/log(2)
end function

function entropy(sequence s)
sequence symbols = {},
         counts = {}
    integer N = length(s)
    for i=1 to N do
        object si = s[i]
        integer k = find(si,symbols)
        if k=0 then
            symbols  = append(symbols,si)
            counts = append(counts,1)
        else
            counts[k] += 1
        end if
    end for
    atom H = 0
    integer n = length(counts)
    for i=1 to n do
        atom ci = counts[i]/N
        H -= ci*log2(ci)
    end for
    return H
end function

sequence F_words = {"1","0"}
for i=3 to 37 do
    F_words = append(F_words,F_words[i-1]&F_words[i-2])
end for

for i=1 to length(F_words) do
    printf(1,"%2d: length %9d, entropy %f %s\n",
        {i,length(F_words[i]),entropy(F_words[i]),
         iff(i<10?F_words[i],"...")})
end for
```

```txt

 1: length         1, entropy 0.000000 1
 2: length         1, entropy 0.000000 0
 3: length         2, entropy 1.000000 01
 4: length         3, entropy 0.918296 010
 5: length         5, entropy 0.970951 01001
 6: length         8, entropy 0.954434 01001010
 7: length        13, entropy 0.961237 0100101001001
 8: length        21, entropy 0.958712 010010100100101001010
 9: length        34, entropy 0.959687 0100101001001010010100100101001001
10: length        55, entropy 0.959316 ...
<shortened>
35: length   9227465, entropy 0.959419 ...
36: length  14930352, entropy 0.959419 ...
37: length  24157817, entropy 0.959419 ...

```



## PL/I


```PL/I
fibword: procedure options (main);  /* 9 October 2013 */
   declare (fn, fnp1, fibword) bit (32000) varying;
   declare (i, ln, lnp1, lfibword) fixed binary(31);

   fn = '1'b; fnp1 = '0'b; ln, lnp1 = 1;
   put skip edit (1, length(fn), fn)     (f(2), f(10), x(1), b);
   put skip edit (2, length(fnp1), fnp1) (f(2), f(10), x(1), b);
   do i = 3 to 37;
      lfibword = lnp1 + ln;
      ln = lnp1;
      lnp1 = lfibword;
      if i <= 10 then
         do;
            fibword = fnp1 || fn;
            put skip edit (i, length(fibword), fibword) (f(2), f(10), x(1), b);
            fn = fnp1; fnp1 = fibword;
         end;
      else
         do;
            put skip edit (i, lfibword) (f(2), f(10));
         end;
   end;

end fibword;
```


```txt
 1         1 1
 2         1 0
 3         2 01
 4         3 010
 5         5 01001
 6         8 01001010
 7        13 0100101001001
 8        21 010010100100101001010
 9        34 0100101001001010010100100101001001
10        55 0100101001001010010100100101001001010010100100101001010
11        89
12       144
13       233
14       377
15       610
16       987
17      1597
18      2584
19      4181
20      6765
21     10946
22     17711
23     28657
24     46368
25     75025
26    121393
27    196418
28    317811
29    514229
30    832040
31   1346269
32   2178309
33   3524578
34   5702887
35   9227465
36  14930352
37  24157817
```



## PureBasic


```purebasic
EnableExplicit
Define fwx$, n.i
NewMap uchar.i()

Macro RowPrint(ns,ls,es,ws)
  Print(RSet(ns,4," ")+RSet(ls,12," ")+" "+es+" ") : If Len(ws)<55 : PrintN(ws) : Else : PrintN("...") : EndIf
EndMacro

Procedure.d nlog2(x.d) : ProcedureReturn Log(x)/Log(2) : EndProcedure

Procedure countchar(s$, Map uchar())
  If Len(s$)
    uchar(Left(s$,1))=CountString(s$,Left(s$,1)) : s$=RemoveString(s$,Left(s$,1))
    ProcedureReturn countchar(s$, uchar())
  EndIf
EndProcedure

Procedure.d ce(fw$)
  Define e.d
  Shared uchar()
  countchar(fw$,uchar())
  ForEach uchar() : e-uchar()/Len(fw$)*nlog2(uchar()/Len(fw$)) : Next
  ProcedureReturn e
EndProcedure

Procedure.s fw(n.i,a$="0",b$="1",m.i=2)
  Select n : Case 1 : ProcedureReturn a$ : Case 2 : ProcedureReturn b$ : EndSelect
  If m<n : ProcedureReturn fw(n,b$+a$,a$,m+1) : EndIf
  ProcedureReturn Mid(a$,3)+ReverseString(Left(a$,2))
EndProcedure

OpenConsole()
PrintN("   N      Length Entropy           Word")
For n=1 To 37 : fwx$=fw(n) : RowPrint(Str(n),Str(Len(fwx$)),StrD(ce(fwx$),15),fwx$) : Next
Input()
```


```txt
   N      Length Entropy           Word
   1           1 0.000000000000000 0
   2           1 0.000000000000000 1
   3           2 1.000000000000000 01
   4           3 0.918295834054490 010
   5           5 0.970950594454669 01001
   6           8 0.954434002924965 01001010
   7          13 0.961236604722876 0100101001001
   8          21 0.958711882977132 010010100100101001010
   9          34 0.959686893774217 0100101001001010010100100101001001
  10          55 0.959316032054378 ...
  11          89 0.959457915838670 ...
  12         144 0.959403754221023 ...
  13         233 0.959424446955987 ...
  14         377 0.959416543740441 ...
  15         610 0.959419562603144 ...
  16         987 0.959418409515225 ...
  17        1597 0.959418849957810 ...
  18        2584 0.959418681724032 ...
  19        4181 0.959418745983664 ...
  20        6765 0.959418721438676 ...
  21       10946 0.959418730814028 ...
  22       17711 0.959418727232962 ...
  23       28657 0.959418728600807 ...
  24       46368 0.959418728078337 ...
  25       75025 0.959418728277903 ...
  26      121393 0.959418728201676 ...
  27      196418 0.959418728230792 ...
  28      317811 0.959418728219670 ...
  29      514229 0.959418728223918 ...
  30      832040 0.959418728222296 ...
  31     1346269 0.959418728222916 ...
  32     2178309 0.959418728222679 ...
  33     3524578 0.959418728222769 ...
  34     5702887 0.959418728222735 ...
  35     9227465 0.959418728222748 ...
  36    14930352 0.959418728222743 ...
  37    24157817 0.959418728222745 ...
```



## Python


```python>>>
 import math
>>> from collections import Counter
>>>
>>> def entropy(s):
...     p, lns = Counter(s), float(len(s))
...     return -sum( count/lns * math.log(count/lns, 2) for count in p.values())
...
>>>
>>> def fibword(nmax=37):
...     fwords = ['1', '0']
...     print('%-3s %10s %-10s %s' % tuple('N Length Entropy Fibword'.split()))
...     def pr(n, fwords):
...         while len(fwords) < n:
...             fwords += [''.join(fwords[-2:][::-1])]
...         v = fwords[n-1]
...         print('%3i %10i %10.7g %s' % (n, len(v), entropy(v), v if len(v) < 20 else '<too long>'))
...     for n in range(1, nmax+1): pr(n, fwords)
...
>>> fibword()
N       Length Entropy    Fibword
  1          1         -0 1
  2          1         -0 0
  3          2          1 01
  4          3  0.9182958 010
  5          5  0.9709506 01001
  6          8   0.954434 01001010
  7         13  0.9612366 0100101001001
  8         21  0.9587119 <too long>
  9         34  0.9596869 <too long>
 10         55   0.959316 <too long>
 11         89  0.9594579 <too long>
 12        144  0.9594038 <too long>
 13        233  0.9594244 <too long>
 14        377  0.9594165 <too long>
 15        610  0.9594196 <too long>
 16        987  0.9594184 <too long>
 17       1597  0.9594188 <too long>
 18       2584  0.9594187 <too long>
 19       4181  0.9594187 <too long>
 20       6765  0.9594187 <too long>
 21      10946  0.9594187 <too long>
 22      17711  0.9594187 <too long>
 23      28657  0.9594187 <too long>
 24      46368  0.9594187 <too long>
 25      75025  0.9594187 <too long>
 26     121393  0.9594187 <too long>
 27     196418  0.9594187 <too long>
 28     317811  0.9594187 <too long>
 29     514229  0.9594187 <too long>
 30     832040  0.9594187 <too long>
 31    1346269  0.9594187 <too long>
 32    2178309  0.9594187 <too long>
 33    3524578  0.9594187 <too long>
 34    5702887  0.9594187 <too long>
 35    9227465  0.9594187 <too long>
 36   14930352  0.9594187 <too long>
 37   24157817  0.9594187 <too long>
>>>
```





## R

With inspiration from [http://rosettacode.org/wiki/Entropy#R here] for the entropy function:

```rsplus
entropy <- function(s)
{
  if (length(s) > 1)
    return(sapply(s, entropy))

  freq <- prop.table(table(strsplit(s, '')[1]))
  ret <- -sum(freq * log(freq, base=2))

  return(ret)
}

fibwords <- function(n)
{
  if (n == 1)
    fibwords <- "1"
  else
    fibwords <- c("1", "0")

  if (n > 2)
  {
    for (i in 3:n)
      fibwords <- c(fibwords, paste(fibwords[i-1L], fibwords[i-2L], sep=""))
  }

  str <- if (n > 7) replicate(n-7, "too long") else NULL
  fibwords.print <- c(fibwords[1:min(n, 7)], str)

  ret <- data.frame(Length=nchar(fibwords), Entropy=entropy(fibwords), Fibwords=fibwords.print)
  rownames(ret) <- NULL
  return(ret)
}
```


Output:

```txt
> fibwords(37)
     Length   Entropy      Fibwords
1         1 0.0000000             1
2         1 0.0000000             0
3         2 1.0000000            01
4         3 0.9182958           010
5         5 0.9709506         01001
6         8 0.9544340      01001010
7        13 0.9612366 0100101001001
8        21 0.9587119      too long
9        34 0.9596869      too long
10       55 0.9593160      too long
11       89 0.9594579      too long
12      144 0.9594038      too long
13      233 0.9594244      too long
14      377 0.9594165      too long
15      610 0.9594196      too long
16      987 0.9594184      too long
17     1597 0.9594188      too long
18     2584 0.9594187      too long
19     4181 0.9594187      too long
20     6765 0.9594187      too long
21    10946 0.9594187      too long
22    17711 0.9594187      too long
23    28657 0.9594187      too long
24    46368 0.9594187      too long
25    75025 0.9594187      too long
26   121393 0.9594187      too long
27   196418 0.9594187      too long
28   317811 0.9594187      too long
29   514229 0.9594187      too long
30   832040 0.9594187      too long
31  1346269 0.9594187      too long
32  2178309 0.9594187      too long
33  3524578 0.9594187      too long
34  5702887 0.9594187      too long
35  9227465 0.9594187      too long
36 14930352 0.9594187      too long
37 24157817 0.9594187      too long
```





## Racket


Uses [[Entropy]] Racket task implementation.

Not as minimal as is could be, since we might have needed scope for more
interesting hooks for e.g. the [[Fibonacci word/fractal]].

So to start, a massively generalised version:

```racket
#lang racket
(provide F-Word gen-F-Word (struct-out f-word) f-word-max-length)
(require "entropy.rkt") ; save Entropy task implementation as "entropy.rkt"

(define f-word-max-length (make-parameter 80))
(define-struct f-word (str length count-0 count-1))
(define (string->f-word str)
  (apply f-word str
         (call-with-values
          (λ ()
            (for/fold
                ((l 0) (zeros 0) (ones 0))
              ((c str))
              (match c
                (#\0 (values (add1 l) (add1 zeros) ones))
                (#\1 (values (add1 l) zeros (add1 ones))))))
          list)))
(define F-Word# (make-hash))

(define (gen-F-Word n #:key-id key-id #:word-1 word-1 #:word-2 word-2 #:merge-fn merge-fn)
  (define sub-F-Word (match-lambda (1 word-1) (2 word-2) ((? number? n) (merge-fn n))))
  (hash-ref! F-Word# (list key-id (f-word-max-length) n) (λ () (sub-F-Word n))))

(define (F-Word n)
  (define f-word-1 (string->f-word "1"))
  (define f-word-2 (string->f-word "0"))
  (define (f-word-merge>2 n)
    (define f-1 (F-Word (- n 1)))
    (define f-2 (F-Word (- n 2)))
    (define length+  (+ (f-word-length f-1) (f-word-length f-2)))
    (define count-0+ (+ (f-word-count-0 f-1) (f-word-count-0 f-2)))
    (define count-1+ (+ (f-word-count-1 f-1) (f-word-count-1 f-2)))
    (define str+
      (if (and (f-word-max-length)
               (> length+ (f-word-max-length)))
          (format "<string too long (~a)>" length+)
          (string-append (f-word-str f-1) (f-word-str f-2))))
    (f-word str+ length+ count-0+ count-1+))

  (gen-F-Word n
              #:key-id 'words
              #:word-1 f-word-1
              #:word-2 f-word-2
              #:merge-fn f-word-merge>2))

(module+ main
  (parameterize ((f-word-max-length 80))
    (for ((n (sequence-map add1 (in-range 37))))
      (define W (F-Word n))
      (define e (hash-entropy (hash 0 (f-word-count-0 W)
                                    1 (f-word-count-1 W))))
      (printf "~a ~a ~a ~a~%"
              (~a n #:width 3 #:align 'right)
              (~a (f-word-length W) #:width 9 #:align 'right)
              (real->decimal-string e 12)
              (~a (f-word-str W))))))

(module+ test
  (require rackunit)
  (check-match (F-Word 4) (f-word "010" _ _ _))
  (check-match (F-Word 5) (f-word "01001" _ _ _))
  (check-match (F-Word 8) (f-word "010010100100101001010" _ _ _)))
```

Output:

```txt
  1         1 0.000000000000 1
  2         1 0.000000000000 0
  3         2 1.000000000000 01
  4         3 0.918295834054 010
  5         5 0.970950594455 01001
  6         8 0.954434002925 01001010
  7        13 0.961236604723 0100101001001
  8        21 0.958711882977 010010100100101001010
  9        34 0.959686893774 0100101001001010010100100101001001
 10        55 0.959316032054 0100101001001010010100100101001001010010100100101001010
 11        89 0.959457915839 <string too long (89)>
 12       144 0.959403754221 <string too long (144)>
 13       233 0.959424446956 <string too long (233)>
 14       377 0.959416543740 <string too long (377)>
 15       610 0.959419562603 <string too long (610)>
 16       987 0.959418409515 <string too long (987)>
 17      1597 0.959418849958 <string too long (1597)>
 18      2584 0.959418681724 <string too long (2584)>
 19      4181 0.959418745984 <string too long (4181)>
 20      6765 0.959418721439 <string too long (6765)>
 21     10946 0.959418730814 <string too long (10946)>
 22     17711 0.959418727233 <string too long (17711)>
 23     28657 0.959418728601 <string too long (28657)>
 24     46368 0.959418728078 <string too long (46368)>
 25     75025 0.959418728278 <string too long (75025)>
 26    121393 0.959418728202 <string too long (121393)>
 27    196418 0.959418728231 <string too long (196418)>
 28    317811 0.959418728220 <string too long (317811)>
 29    514229 0.959418728224 <string too long (514229)>
 30    832040 0.959418728222 <string too long (832040)>
 31   1346269 0.959418728223 <string too long (1346269)>
 32   2178309 0.959418728223 <string too long (2178309)>
 33   3524578 0.959418728223 <string too long (3524578)>
 34   5702887 0.959418728223 <string too long (5702887)>
 35   9227465 0.959418728223 <string too long (9227465)>
 36  14930352 0.959418728223 <string too long (14930352)>
 37  24157817 0.959418728223 <string too long (24157817)>

```


And a simpler implementation:

```racket
#lang racket

(define f-word-max-length (make-parameter 80))
(define-struct f-word (str length count-0 count-1))

(define F-Word# (make-hash))
(define (F-Word n)
  (hash-ref!
   F-Word#
   (list (f-word-max-length) n)
   (λ ()
     (match n
      (1 (f-word "1" 1 0 1))
      (2 (f-word "0" 1 1 0))
      ((? number? n)
       (define f-1 (F-Word (- n 1)))
       (define f-2 (F-Word (- n 2)))
       (define length+  (+ (f-word-length f-1) (f-word-length f-2)))
       (define count-0+ (+ (f-word-count-0 f-1) (f-word-count-0 f-2)))
       (define count-1+ (+ (f-word-count-1 f-1) (f-word-count-1 f-2)))
       (define str+
         (if (and (f-word-max-length)
                  (> length+ (f-word-max-length)))
             (format "<string too long (~a)>" length+)
             (string-append (f-word-str f-1) (f-word-str f-2))))
       (f-word str+ length+ count-0+ count-1+))))))

(module+ test
  (require rackunit)
  (check-match (F-Word 4) (f-word "010" _ _ _))
  (check-match (F-Word 5) (f-word "01001" _ _ _))
  (check-match (F-Word 8) (f-word "010010100100101001010" _ _ _)))
```



## REXX

Programming note:   32-bit Regina REXX (under Windows/XP) can execute this program with   N='''42'''   without exhausting system resources,   the 64-bit version of Regina can calculate bigger Fibonacci words.

```rexx
/*REXX program displays the number of chars in a fibonacci word, and the word's entropy.*/
d=20;     de=d+6;    numeric digits de           /*use more precision (the default is 9)*/
parse arg N .                                    /*get optional argument from the C.L.  */
if N==''  | N==","  then N=42                    /*Not specified?  Then use the default.*/
say center('N', 5)   center("length", 12)   center('entropy', de)   center("Fib word", 56)
say copies('─', 5)   copies("─"     , 12)   copies('─'      , de)   copies("─"       , 56)
c=1                                              /* [↓]  display   N   fibonacci words. */
      do j=1  for N;  if j==2  then c=0          /*test for the case of  J  equals  2.  */
      if j==3 then parse value 1 0 with a b      /*  "   "   "    "   "  "    "     3.  */
      if j>2  then c=b || a;  L=length(c)        /*calculate the FIBword  if we need to.*/
      if L<56  then Fw= c
               else Fw= '{the word is too wide to display, length is: ' L"}"
      say right(j,4)  right(L,12)    '  '    entropy()    "  "    Fw
      a=b;   b=c                                 /*define the new values for  A  and  B.*/
      end   /*j*/                                /*display text msg;                    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
entropy: if L==1  then return left(0, d+2)       /*handle special case of one character.*/
         !.0=length( space( translate(c,,1), 0)) /*efficient way to count the  "zeroes".*/
         !.1=L-!.0; $=0;  do i=1  for 2;   _=i-1 /*construct character from the ether.  */
                          $=$ -!._/L*log2(!._/L) /*add  (negatively)  the entropies.    */
                          end   /*i*/
         if $=1  then return   left(1, d+2)      /*return a left─justified  "1"  (one). */
                      return format($,,d)        /*normalize the sum  (S)  number.      */
/*──────────────────────────────────────────────────────────────────────────────────────*/
log2: procedure; parse arg x 1 xx;  ig=x>1.5;  is=1-2*(ig\==1);  numeric digits 5+digits()
      e=2.71828182845904523536028747135266249775724709369995957496696762772407663035354759
      m=0;  do  while  ig & xx>1.5 | \ig&xx<.5;   _=e;         do j=-1;   iz=xx* _ ** - is
            if j>=0  then if ig & iz<1 | \ig&iz>.5  then leave;  _=_*_; izz=iz;  end /*j*/
            xx=izz;  m=m+is*2**j;  end /*while*/;     x=x* e** -m -1;   z=0;   _=-1;   p=z
                   do k=1;   _=-_*x;   z=z+_/k;   if z=p  then leave;   p=z;    end  /*k*/
            r=z+m;            if arg()==2  then return r;             return r / log2(2,.)
```

'''output'''   for the first 42 Fibonacci words:

```txt

  N      length             entropy                                   Fib word
───── ──────────── ────────────────────────── ────────────────────────────────────────────────────────
   1            1    0                         1
   2            1    0                         0
   3            2    1                         01
   4            3    0.91829583405448951479    010
   5            5    0.97095059445466863900    01001
   6            8    0.95443400292496496454    01001010
   7           13    0.96123660472287587275    0100101001001
   8           21    0.95871188297713180865    010010100100101001010
   9           34    0.95968689377421693320    0100101001001010010100100101001001
  10           55    0.95931603205437767775    0100101001001010010100100101001001010010100100101001010
  11           89    0.95945791583866946166    {the word is too wide to display, length is:  89}
  12          144    0.95940375422102292947    {the word is too wide to display, length is:  144}
  13          233    0.95942444695598675869    {the word is too wide to display, length is:  233}
  14          377    0.95941654374044073872    {the word is too wide to display, length is:  377}
  15          610    0.95941956260314415023    {the word is too wide to display, length is:  610}
  16          987    0.95941840951522431271    {the word is too wide to display, length is:  987}
  17         1597    0.95941884995780985568    {the word is too wide to display, length is:  1597}
  18         2584    0.95941868172403210665    {the word is too wide to display, length is:  2584}
  19         4181    0.95941874598366381433    {the word is too wide to display, length is:  4181}
  20         6765    0.95941872143867541464    {the word is too wide to display, length is:  6765}
  21        10946    0.95941873081402772313    {the word is too wide to display, length is:  10946}
  22        17711    0.95941872723296194271    {the word is too wide to display, length is:  17711}
  23        28657    0.95941872860080737603    {the word is too wide to display, length is:  28657}
  24        46368    0.95941872807833691494    {the word is too wide to display, length is:  46368}
  25        75025    0.95941872827790287341    {the word is too wide to display, length is:  75025}
  26       121393    0.95941872820167546034    {the word is too wide to display, length is:  121393}
  27       196418    0.95941872823079174127    {the word is too wide to display, length is:  196418}
  28       317811    0.95941872821967031158    {the word is too wide to display, length is:  317811}
  29       514229    0.95941872822391831972    {the word is too wide to display, length is:  514229}
  30       832040    0.95941872822229572499    {the word is too wide to display, length is:  832040}
  31      1346269    0.95941872822291550103    {the word is too wide to display, length is:  1346269}
  32      2178309    0.95941872822267876765    {the word is too wide to display, length is:  2178309}
  33      3524578    0.95941872822276919175    {the word is too wide to display, length is:  3524578}
  34      5702887    0.95941872822273465282    {the word is too wide to display, length is:  5702887}
  35      9227465    0.95941872822274784552    {the word is too wide to display, length is:  9227465}
  36     14930352    0.95941872822274280635    {the word is too wide to display, length is:  14930352}
  37     24157817    0.95941872822274473114    {the word is too wide to display, length is:  24157817}
  38     39088169    0.95941872822274399594    {the word is too wide to display, length is:  39088169}
  39     63245986    0.95941872822274427676    {the word is too wide to display, length is:  63245986}
  40    102334155    0.95941872822274416950    {the word is too wide to display, length is:  102334155}
  41    165580141    0.95941872822274421047    {the word is too wide to display, length is:  165580141}
  42    267914296    0.95941872822274419482    {the word is too wide to display, length is:  267914296}

```



## Ring


```ring

# Project : Fibonacci word

fw1 = "1"
fw2 = "0"

see "N   Length  Entropy                Word" + nl
n = 1
see "" + n + "      " + len(fw1) + "           " + calcentropy(fw1,2) + "      " + fw1 + nl
n = 2
see "" + n + "      " + len(fw2) + "           " + calcentropy(fw2,2) + "      " + fw2 + nl

for n = 1 to 55
      fw3 = fw2 + fw1
      temp = fw2
      fw2 = fw3
      fw1 = temp
      if len(fw3) < 55
         see "" + (n+2) + "      " + len(fw3) + "          " + calcentropy(fw3,2) + "     " + fw3 + nl
      ok
next

func calcentropy(source,b)
        decimals(11)
        entropy = 0
        countOfChar = list(255)
        charCount  =len( source)
        usedChar  =""
        for i =1 to len( source)
             ch =substr(source, i, 1)
             if not(substr( usedChar, ch))
                usedChar =usedChar +ch
             ok
             j  =substr( usedChar, ch)
            countOfChar[j] =countOfChar[j] +1
        next
        l =len(usedChar)
        for i =1 to l
             probability =countOfChar[i] /charCount
             entropy =entropy - (probability *logBase(probability, 2))
        next
        return entropy

func swap(a, b)
        temp = a
        a = b
        b = temp
        return [a, b]

func logBase (x, b)
        logBase =log( x) /log( 2)
        return logBase

```

Output:

```txt

   N       Length Entropy           Word
   1           1  0.000000000000000 1
   2           1  0.000000000000000 0
   3           2  1.000000000000000 01
   4           3  0.918295834054490 010
   5           5  0.970950594454669 01001
   6           8  0.954434002924965 01001010
   7          13  0.961236604722876 0100101001001
   8          21  0.958711882977132 010010100100101001010
   9          34  0.959686893774217 0100101001001010010100100101001001
  10          55  0.959316032054378
  11          89  0.959457915838670
  12         144  0.959403754221023
  13         233  0.959424446955987
  14         377  0.959416543740441
  15         610  0.959419562603144
  16         987  0.959418409515224
  17        1597  0.959418849957810
  18        2584  0.959418681724032
  19        4181  0.959418745983664
  20        6765  0.959418721438676
  21       10946  0.959418730814028
  22       17711  0.959418727232962
  23       28657  0.959418728600807
  24       46368  0.959418728078337
  25       75025  0.959418728277903
  26      121393  0.959418728201675
  27      196418  0.959418728230792
  28      317811  0.959418728219670
  29      514229  0.959418728223918
  30      832040  0.959418728222296
  31     1346269  0.959418728222916
  32     2178309  0.959418728222679
  33     3524578  0.959418728222769
  34     5702887  0.959418728222735
  35     9227465  0.959418728222748
  36    14930352  0.959418728222743
  37    24157817  0.959418728222745

```



## Ruby


Includes code for entropy from [[Entropy#Ruby|Entropy]] page.


```ruby
#encoding: ASCII-8BIT

def entropy(s)
  counts = Hash.new(0.0)
  s.each_char { |c| counts[c] += 1 }
  leng = s.length

  counts.values.reduce(0) do |entropy, count|
    freq = count / leng
    entropy - freq * Math.log2(freq)
  end
end

n_max = 37
words = ['1', '0']

for n in words.length ... n_max
  words << words[-1] + words[-2]
end

puts '%3s %9s %15s  %s' % %w[N Length Entropy Fibword]
words.each.with_index(1) do |word, i|
  puts '%3i %9i %15.12f  %s' % [i, word.length, entropy(word), word.length<60 ? word : '<too long>']
end
```


```txt

  N    Length         Entropy  Fibword
  1         1  0.000000000000  1
  2         1  0.000000000000  0
  3         2  1.000000000000  01
  4         3  0.918295834054  010
  5         5  0.970950594455  01001
  6         8  0.954434002925  01001010
  7        13  0.961236604723  0100101001001
  8        21  0.958711882977  010010100100101001010
  9        34  0.959686893774  0100101001001010010100100101001001
 10        55  0.959316032054  0100101001001010010100100101001001010010100100101001010
 11        89  0.959457915839  <too long>
 12       144  0.959403754221  <too long>
 13       233  0.959424446956  <too long>
 14       377  0.959416543740  <too long>
 15       610  0.959419562603  <too long>
 16       987  0.959418409515  <too long>
 17      1597  0.959418849958  <too long>
 18      2584  0.959418681724  <too long>
 19      4181  0.959418745984  <too long>
 20      6765  0.959418721439  <too long>
 21     10946  0.959418730814  <too long>
 22     17711  0.959418727233  <too long>
 23     28657  0.959418728601  <too long>
 24     46368  0.959418728078  <too long>
 25     75025  0.959418728278  <too long>
 26    121393  0.959418728202  <too long>
 27    196418  0.959418728231  <too long>
 28    317811  0.959418728220  <too long>
 29    514229  0.959418728224  <too long>
 30    832040  0.959418728222  <too long>
 31   1346269  0.959418728223  <too long>
 32   2178309  0.959418728223  <too long>
 33   3524578  0.959418728223  <too long>
 34   5702887  0.959418728223  <too long>
 35   9227465  0.959418728223  <too long>
 36  14930352  0.959418728223  <too long>
 37  24157817  0.959418728223  <too long>

```



## Rust

This is not implemented in any sort of generic way and is probably fairly inefficient.


```rust>struct Fib<T
 {
    curr: T,
    next: T,
}

impl<T> Fib<T> {
    fn new(curr: T, next: T) -> Self {
        Fib { curr: curr, next: next, }
    }
}

impl Iterator for Fib<String>  {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.curr.clone();
        self.curr = self.next.clone();
        self.next = format!("{}{}", ret, self.next);
        Some(ret)
    }
}

fn get_entropy(s: &[u8]) -> f64 {
    let mut entropy = 0.0;
    let mut histogram = [0.0; 256];

    for i in 0..s.len() {
        histogram.get_mut(s[i] as usize).map(|v| *v += 1.0);
    }

    for i in 0..256 {
        if histogram[i] > 0.0 {
            let ratio = histogram[i] / s.len() as f64;
            entropy -= ratio * ratio.log2();
        }
    }
    entropy
}

fn main() {
    let f = Fib::new("1".to_string(), "0".to_string());
        println!("{:10} {:10} {:10} {:60}", "N", "Length", "Entropy", "Word");
    for (i, s) in f.take(37).enumerate() {
        let word = if s.len() > 60 {"Too long"} else {&*s};
        println!("{:10} {:10} {:.10} {:60}", i + 1, s.len(), get_entropy(&s.bytes().collect::<Vec<_>>()), word);
    }
}
```


```txt

N          Length     Entropy    Word
         1          1 0.0000000000 1
         2          1 0.0000000000 0
         3          2 1.0000000000 10
         4          3 0.9182958341 010
         5          5 0.9709505945 10010
         6          8 0.9544340029 01010010
         7         13 0.9612366047 1001001010010
         8         21 0.9587118830 010100101001001010010
         9         34 0.9596868938 1001001010010010100101001001010010
        10         55 0.9593160321 0101001010010010100101001001010010010100101001001010010
        11         89 0.9594579158 Too long
        12        144 0.9594037542 Too long
        13        233 0.9594244470 Too long
        14        377 0.9594165437 Too long
        15        610 0.9594195626 Too long
        16        987 0.9594184095 Too long
        17       1597 0.9594188500 Too long
        18       2584 0.9594186817 Too long
        19       4181 0.9594187460 Too long
        20       6765 0.9594187214 Too long
        21      10946 0.9594187308 Too long          t
        22      17711 0.9594187272 Too long
        23      28657 0.9594187286 Too long
        24      46368 0.9594187281 Too long
        25      75025 0.9594187283 Too long
        26     121393 0.9594187282 Too long
        27     196418 0.9594187282 Too long
        28     317811 0.9594187282 Too long
        29     514229 0.9594187282 Too long
        30     832040 0.9594187282 Too long
        31    1346269 0.9594187282 Too long
        32    2178309 0.9594187282 Too long
        33    3524578 0.9594187282 Too long
        34    5702887 0.9594187282 Too long
        35    9227465 0.9594187282 Too long
        36   14930352 0.9594187282 Too long
        37   24157817 0.9594187282 Too long


```



## Scala


```scala

//word iterator
def fibIt = Iterator.iterate(("1","0")){case (f1,f2) => (f2,f1+f2)}.map(_._1)

//entropy calculator
def entropy(src: String): Double = {
  val xs = src.groupBy(identity).map(_._2.length)
  var result = 0.0
  xs.foreach{c =>
    val p = c.toDouble / src.length
    result -= p * (Math.log(p) / Math.log(2))
  }
  result
}

//printing (spaces inserted to get the tabs align properly)
val it = fibIt.zipWithIndex.map(w => (w._2, w._1.length, entropy(w._1)))
println(it.take(37).map{case (n,l,e) => s"$n).\t$l       \t$e"}.mkString("\n"))

```


```txt

0).	1       	0.0
1).	1       	0.0
2).	2       	1.0
3).	3       	0.9182958340544896
4).	5       	0.9709505944546686
5).	8       	0.9544340029249649
6).	13       	0.961236604722876
7).	21       	0.9587118829771318
8).	34       	0.9596868937742169
9).	55       	0.9593160320543777
10).	89       	0.9594579158386696
11).	144       	0.959403754221023
12).	233       	0.9594244469559867
13).	377       	0.9594165437404407
14).	610       	0.9594195626031441
15).	987       	0.9594184095152245
16).	1597       	0.9594188499578099
17).	2584       	0.9594186817240321
18).	4181       	0.9594187459836638
19).	6765       	0.9594187214386756
20).	10946       	0.9594187308140278
21).	17711       	0.959418727232962
22).	28657       	0.9594187286008073
23).	46368       	0.9594187280783371
24).	75025       	0.9594187282779029
25).	121393       	0.9594187282016755
26).	196418       	0.9594187282307918
27).	317811       	0.9594187282196702
28).	514229       	0.9594187282239184
29).	832040       	0.9594187282222959
30).	1346269       	0.9594187282229156
31).	2178309       	0.9594187282226789
32).	3524578       	0.9594187282227691
33).	5702887       	0.9594187282227347
34).	9227465       	0.9594187282227479
35).	14930352       	0.9594187282227429
36).	24157817       	0.9594187282227448

```



## Scheme



```scheme

(import (scheme base)
        (scheme inexact)
        (scheme write))

(define *words* (make-vector 38 ""))

(define (create-words)
  (vector-set! *words* 1 "1")
  (vector-set! *words* 2 "0")
  (do ((i 3 (+ 1 i)))
    ((= i (vector-length *words*)) )
    (vector-set! *words* i (string-append (vector-ref *words* (- i 1))
                                           (vector-ref *words* (- i 2))))))

;; in this context, word only contains 1 or 0
(define (entropy word)
  (let* ((N (string-length word))
         (num-ones 0)
         (num-zeros 0))
    (string-for-each (lambda (c)
                       (if (char=? c #\1)
                         (set! num-ones (+ 1 num-ones))
                         (set! num-zeros (+ 1 num-zeros))))
                     word)
    (if (or (zero? num-ones) (zero? num-zeros))
      0
      (- 0
         (* (/ num-ones N) (log (/ num-ones N) 2))
         (* (/ num-zeros N) (log (/ num-zeros N) 2))))))

;; display values
(create-words)
(do ((i 1 (+ 1 i)))
  ((= i (vector-length *words*)) )
  (display (string-append (number->string i)
                          " "
                          (number->string
                            (string-length (vector-ref *words* i)))
                          " "
                          (number->string
                            (entropy (vector-ref *words* i)))
                          "\n")))

```


```txt

1 1 0
2 1 0
3 2 1.0
4 3 0.9182958340544896
5 5 0.9709505944546686
6 8 0.9544340029249649
7 13 0.961236604722876
8 21 0.9587118829771318
9 34 0.9596868937742169
10 55 0.9593160320543777
11 89 0.9594579158386696
12 144 0.959403754221023
13 233 0.9594244469559867
14 377 0.9594165437404407
15 610 0.9594195626031441
16 987 0.9594184095152245
17 1597 0.9594188499578099
18 2584 0.9594186817240321
19 4181 0.9594187459836638
20 6765 0.9594187214386756
21 10946 0.9594187308140278
22 17711 0.959418727232962
23 28657 0.9594187286008073
24 46368 0.9594187280783371
25 75025 0.9594187282779029
26 121393 0.9594187282016755
27 196418 0.9594187282307918
28 317811 0.9594187282196702
29 514229 0.9594187282239184
30 832040 0.9594187282222959
31 1346269 0.9594187282229156
32 2178309 0.9594187282226789
33 3524578 0.9594187282227691
34 5702887 0.9594187282227347
35 9227465 0.9594187282227479
36 14930352 0.9594187282227429
37 24157817 0.9594187282227448

```



## Scilab


Two different approaches were implemented, and their execution times can be compared. Both examples use Scilab's [[Entropy#Scilab|entropy]] example. It is worth noting that the time spent executing entropy() is quite significant when using the iterative method, e.g. it usually takes 27 times longer to calculate the 37th word's entropy than it takes to generate it.


### Recursive function

<lang>exec('.\entropy.sci',0);

function word=fiboword(n)
    word_1 = '1'; word_2 = '0';
    select n
    case 1
        word = word_1
    case 2
        word = word_2;
    case 3
        word = strcat([word_2 word_1]);
    else
        word = strcat([fiboword(n-1) fiboword(n-2)])
    end
endfunction

final_length = 37;

N=[1:final_length]';
char_length = zeros(N);
entropies = zeros(N);
tic();
for i=1:final_length
    word = fiboword(i);
    char_length(i) = length(word);
    entropies(i) = entropy(word);
end
time = toc();

disp('EXECUTION TIME: '+string(time)+'s.');
disp(['N', 'LENGTH', 'ENTROPY'; string([N char_length entropies])]);
```


```txt
 EXECUTION TIME: 442.87612s.

!N   LENGTH    ENTROPY    !
!                         !
!1   1         0          !
!                         !
!2   1         0          !
!                         !
!3   2         1          !
!                         !
!4   3         0.9182958  !
!                         !
!5   5         0.9709506  !
!                         !
!6   8         0.954434   !
!                         !
!7   13        0.9612366  !
!                         !
!8   21        0.9587119  !
!                         !
!9   34        0.9596869  !
!                         !
!10  55        0.9593160  !
!                         !
!11  89        0.9594579  !
!                         !
!12  144       0.9594038  !
!                         !
!13  233       0.9594244  !
!                         !
!14  377       0.9594165  !
!                         !
!15  610       0.9594196  !
!                         !
!16  987       0.9594184  !
!                         !
!17  1597      0.9594188  !
!                         !
!18  2584      0.9594187  !
!                         !
!19  4181      0.9594187  !
!                         !
!20  6765      0.9594187  !
!                         !
!21  10946     0.9594187  !
!                         !
!22  17711     0.9594187  !
!                         !
!23  28657     0.9594187  !
!                         !
!24  46368     0.9594187  !
!                         !
!25  75025     0.9594187  !
!                         !
!26  121393    0.9594187  !
!                         !
!27  196418    0.9594187  !
!                         !
!28  317811    0.9594187  !
!                         !
!29  514229    0.9594187  !
!                         !
!30  832040    0.9594187  !
!                         !
!31  1346269   0.9594187  !
!                         !
!32  2178309   0.9594187  !
!                         !
!33  3524578   0.9594187  !
!                         !
!34  5702887   0.9594187  !
!                         !
!35  9227465   0.9594187  !
!                         !
!36  14930352  0.9594187  !
!                         !
!37  24157817  0.9594187  !
```



### Iterative method

<lang>exec('.\entropy.sci',0);

final_length = 37;

word_n = '';
word_n_1 = '';
word_n_2 = '';

N = [1:final_length]';
word_length = zeros(N);
entropies = zeros(N);

tic();
for i = 1:final_length
    if i == 1 then
        word_n = '1';
    elseif i == 2
        word_n = '0';
    elseif i == 3
        word_n = '01';
        word_n_1 = '0';
    else
        word_n_2 = word_n_1;
        word_n_1 = word_n;
        word_n = word_n_1 + word_n_2;
    end
    word_length(i) = length(word_n);
    entropies(i) = entropy(word_n);
end
time = toc();

disp('EXECUTION TIME: '+string(time)+'s.');
disp(['N', 'LENGTH', 'ENTROPY'; string([N word_length entropies])]);
```


```txt
 EXECUTION TIME: 37.962248s.

!N   LENGTH    ENTROPY    !
!                         !
!1   1         0          !
!                         !
!2   1         0          !
!                         !
!3   2         1          !
!                         !
!4   3         0.9182958  !
!                         !
!5   5         0.9709506  !
!                         !
!6   8         0.954434   !
!                         !
!7   13        0.9612366  !
!                         !
!8   21        0.9587119  !
!                         !
!9   34        0.9596869  !
!                         !
!10  55        0.9593160  !
!                         !
!11  89        0.9594579  !
!                         !
!12  144       0.9594038  !
!                         !
!13  233       0.9594244  !
!                         !
!14  377       0.9594165  !
!                         !
!15  610       0.9594196  !
!                         !
!16  987       0.9594184  !
!                         !
!17  1597      0.9594188  !
!                         !
!18  2584      0.9594187  !
!                         !
!19  4181      0.9594187  !
!                         !
!20  6765      0.9594187  !
!                         !
!21  10946     0.9594187  !
!                         !
!22  17711     0.9594187  !
!                         !
!23  28657     0.9594187  !
!                         !
!24  46368     0.9594187  !
!                         !
!25  75025     0.9594187  !
!                         !
!26  121393    0.9594187  !
!                         !
!27  196418    0.9594187  !
!                         !
!28  317811    0.9594187  !
!                         !
!29  514229    0.9594187  !
!                         !
!30  832040    0.9594187  !
!                         !
!31  1346269   0.9594187  !
!                         !
!32  2178309   0.9594187  !
!                         !
!33  3524578   0.9594187  !
!                         !
!34  5702887   0.9594187  !
!                         !
!35  9227465   0.9594187  !
!                         !
!36  14930352  0.9594187  !
!                         !
!37  24157817  0.9594187  !
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const func float: entropy (in string: stri) is func
  result
    var float: entropy is 0.0;
  local
    var hash [char] integer: count is (hash [char] integer).value;
    var char: ch is ' ';
    var float: p is 0.0;
  begin
    for ch range stri do
      if ch in count then
        incr(count[ch]);
      else
        count @:= [ch] 1;
      end if;
    end for;
    for key ch range count do
      p := flt(count[ch]) / flt(length(stri));
      entropy -:= p * log(p) / log(2.0);
    end for;
  end func ;

const func string: fibWord (in integer: number) is func
  result
    var string: fibWord is "1";
  local
    var integer: i is 0;
    var string: a is "1";
    var string: c is "";
  begin
    if number >= 2 then
      fibWord := "0";
      for i range 3 to number do
        c := a;
        a := fibWord;
        fibWord &:= c;
      end for;
    end if;
  end func;

const proc: main is func
  local
    var integer: index is 0;
    var string: fibWord is "";
  begin
    for index range 1 to 37 do
      fibWord := fibWord(index);
      writeln(index lpad 2 <& length(fibWord) lpad 10 <& " " <& entropy(fibWord) digits 15);
    end for;
  end func;
```


```txt

 1         1 0.000000000000000
 2         1 0.000000000000000
 3         2 1.000000000000000
 4         3 0.918295834054490
 5         5 0.970950594454669
 6         8 0.954434002924965
 7        13 0.961236604722876
 8        21 0.958711882977132
 9        34 0.959686893774217
10        55 0.959316032054378
11        89 0.959457915838670
12       144 0.959403754221023
13       233 0.959424446955987
14       377 0.959416543740441
15       610 0.959419562603144
16       987 0.959418409515225
17      1597 0.959418849957810
18      2584 0.959418681724032
19      4181 0.959418745983664
20      6765 0.959418721438675
21     10946 0.959418730814028
22     17711 0.959418727232962
23     28657 0.959418728600807
24     46368 0.959418728078337
25     75025 0.959418728277903
26    121393 0.959418728201676
27    196418 0.959418728230792
28    317811 0.959418728219670
29    514229 0.959418728223918
30    832040 0.959418728222296
31   1346269 0.959418728222916
32   2178309 0.959418728222679
33   3524578 0.959418728222769
34   5702887 0.959418728222735
35   9227465 0.959418728222748
36  14930352 0.959418728222743
37  24157817 0.959418728222745

```



## Sidef

```ruby
func entropy(s) {
    [0] + (s.chars.freq.values »/» s.len) -> reduce { |a,b|
        a - b*b.log2
    }
}

var n_max = 37
var words = ['1', '0']

{
    words.append(words[-1] + words[-2])
} * (n_max - words.len)

say ('%3s %10s %15s  %s' % <N Length Entropy Fibword>...)

for i in ^words {
    var word = words[i]
    say ('%3i %10i %15.12f  %s' % (i+1,
                                   word.len,
                                   entropy(word),
                                   word.len<30 ? word : '<too long>'))
}
```



## Tcl


```tcl
proc fibwords {n} {
    set fw {1 0}
    while {[llength $fw] < $n} {
	lappend fw [lindex $fw end][lindex $fw end-1]
    }
    return $fw
}

proc fibwordinfo {num word} {
    # Entropy calculator from Tcl solution of that task
    set log2 [expr log(2)]
    set len [string length $word]
    foreach char [split $word ""] {dict incr counts $char}
    set entropy 0.0
    foreach count [dict values $counts] {
	set freq [expr {$count / double($len)}]
	set entropy [expr {$entropy - $freq * log($freq)/$log2}]
    }
    # Output formatting from Clojure solution
    puts [format "%2d %10d %.15f %s" $num $len $entropy \
	    [if {$len < 35} {set word} {subst "<too long>"}]]
}

# Output formatting from Clojure solution
puts [format "%2s %10s %17s %s" N Length Entropy Fibword]
foreach word [fibwords 37] {
    fibwordinfo [incr i] $word
}
```

```txt

 N     Length           Entropy Fibword
 1          1 0.000000000000000 1
 2          1 0.000000000000000 0
 3          2 1.000000000000000 01
 4          3 0.918295834054490 010
 5          5 0.970950594454669 01001
 6          8 0.954434002924965 01001010
 7         13 0.961236604722876 0100101001001
 8         21 0.958711882977132 010010100100101001010
 9         34 0.959686893774217 0100101001001010010100100101001001
10         55 0.959316032054378 <too long>
11         89 0.959457915838670 <too long>
12        144 0.959403754221023 <too long>
13        233 0.959424446955987 <too long>
14        377 0.959416543740441 <too long>
15        610 0.959419562603144 <too long>
16        987 0.959418409515225 <too long>
17       1597 0.959418849957810 <too long>
18       2584 0.959418681724032 <too long>
19       4181 0.959418745983664 <too long>
20       6765 0.959418721438675 <too long>
21      10946 0.959418730814028 <too long>
22      17711 0.959418727232962 <too long>
23      28657 0.959418728600807 <too long>
24      46368 0.959418728078337 <too long>
25      75025 0.959418728277903 <too long>
26     121393 0.959418728201676 <too long>
27     196418 0.959418728230792 <too long>
28     317811 0.959418728219670 <too long>
29     514229 0.959418728223918 <too long>
30     832040 0.959418728222296 <too long>
31    1346269 0.959418728222916 <too long>
32    2178309 0.959418728222679 <too long>
33    3524578 0.959418728222769 <too long>
34    5702887 0.959418728222735 <too long>
35    9227465 0.959418728222748 <too long>
36   14930352 0.959418728222743 <too long>
37   24157817 0.959418728222745 <too long>

```



## zkl

```zkl
fcn entropy(bs){ //binary String-->Float
   len:=bs.len(); num1s:=(bs-"0").len();
   T(num1s,len-num1s).filter().apply('wrap(p){ p=p.toFloat()/len; -p*p.log() })
   .sum(0.0) / (2.0).log();
}

"  N     Length      Entropy Fibword".println();
ws:=L("1","0");
foreach n in ([1..37]){
   if(n>2) ws.append(ws[-1]+ws[-2]);
   w:=ws[-1];
   "%3d %10d %2.10f %s".fmt(n,w.len(),entropy(w),
      w.len()<50 and w or "<too long>").println();
}
```

```txt

  N     Length      Entropy Fibword
  1          1 0.0000000000 0
  2          1 0.0000000000 0
  3          2 1.0000000000 01
  4          3 0.9182958341 010
  5          5 0.9709505945 01001
  6          8 0.9544340029 01001010
  7         13 0.9612366047 0100101001001
  8         21 0.9587118830 010010100100101001010
  9         34 0.9596868938 0100101001001010010100100101001001
 10         55 0.9593160321 <too long>
...
 36   14930352 0.9594187282 <too long>
 37   24157817 0.9594187282 <too long>

```



## ZX Spectrum Basic

```zxbasic
10 LET x$="1": LET y$="0": LET z$=""
20 PRINT "N, Length, Entropy, Word"
30 LET n=1
40 PRINT n;" ";LEN x$;" ";
50 LET s$=x$: LET base=2: GO SUB 1000
60 PRINT entropy
70 PRINT x$
80 LET n=2
90 PRINT n;" ";LEN y$;" ";
100 LET s$=y$: GO SUB 1000
110 PRINT entropy
120 PRINT y$
130 FOR n=1 TO 18
140 LET x$="1": LET y$="0"
150 FOR i=1 TO n
160 LET z$=y$+x$
170 LET p$=x$: LET x$=y$: LET y$=p$
180 LET p$=y$: LET y$=z$: LET z$=p$
190 NEXT i
200 LET x$="": LET z$=""
210 LET s$=y$: GO SUB 1000
220 PRINT n+2;" ";LEN y$;" ";entropy
230 PRINT y$ AND (LEN y$<32)
240 NEXT n
250 STOP
1000 REM Calculate entropy
1010 LET sourcelen=LEN s$: LET entropy=0
1020 DIM t(255)
1030 FOR j=1 TO sourcelen
1040 LET digit=VAL s$(j)+1: LET t(digit)=t(digit)+1
1050 NEXT j
1060 FOR j=1 TO 255
1070 IF t(j)>0 THEN LET prop=t(j)/sourcelen: LET entropy=entropy-(prop*LN (prop)/LN (base))
1080 NEXT j
1090 RETURN
```


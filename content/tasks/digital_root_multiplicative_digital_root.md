+++
title = "Digital root/Multiplicative digital root"
description = ""
date = 2019-05-25T04:47:43Z
aliases = []
[extra]
id = 17509
[taxonomies]
categories = ["task", "Mathematics"]
tags = []
languages = [
  "11l",
  "ada",
  "algol_68",
  "algol_w",
  "awk",
  "bracmat",
  "c",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "nim",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "pl_i",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "tcl",
  "zkl",
]
+++

The [[wp:Multiplicative digital root|multiplicative digital root]] (MDR) and multiplicative persistence (MP) of a number, <math>n</math>, is calculated rather like the [[Digital root]] except digits are multiplied instead of being added:
# Set <math>m</math> to <math>n</math> and <math>i</math> to <math>0</math>.
# While <math>m</math> has more than one digit:
#* Find a replacement <math>m</math> as the multiplication of the digits of the current value of <math>m</math>.
#* Increment <math>i</math>.
# Return <math>i</math> (= MP) and <math>m</math> (= MDR)


## Task

* Tabulate the MP and MDR of the numbers 123321, 7739, 893, 899998
* Tabulate MDR versus the first five numbers having that MDR, something like:

```txt

MDR: [n0..n4]

###   =====

  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]

```

Show all output on this page.


## References

* [http://mathworld.wolfram.com/MultiplicativeDigitalRoot.html Multiplicative Digital Root] on Wolfram Mathworld.
* [http://oeis.org/A031347 Multiplicative digital root] on The On-Line Encyclopedia of Integer Sequences.
* [https://www.youtube.com/watch?v=Wim9WJeDTHQ What's special about 277777788888899?] - Numberphile video





## 11l

```11l
F mdroot(n)
   V count = 0
   V mdr = n
   L mdr > 9
      V m = mdr
      V digits_mul = 1
      L m != 0
         digits_mul *= m % 10
         m = m I/ 10
      mdr = digits_mul
      count++
   R (count, mdr)

print(‘Number: (MP, MDR)’)
print(‘
### ===  ======
’)
L(n) (123321, 7739, 893, 899998)
   print(‘#6: ’.format(n), end' ‘’)
   print(mdroot(n))

[[Int]] table
table.resize(10)
V n = 0
L min(table.map(row -> row.len)) < 5
   table[mdroot(n)[1]].append(n)
   n++

print(‘’)
print(‘MP: [n0..n4]’)
print(‘==
### ==
’)
L(val) table
   print(‘#2: ’.format(L.index), end' ‘’)
   print(val[0.<5])
```


```txt

Number: (MP, MDR)

### ===  ======

123321: (3, 8)
  7739: (3, 8)
   893: (3, 2)
899998: (2, 0)

MP: [n0..n4]
==
### ==

 0: [0, 10, 20, 25, 30]
 1: [1, 11, 111, 1111, 11111]
 2: [2, 12, 21, 26, 34]
 3: [3, 13, 31, 113, 131]
 4: [4, 14, 22, 27, 39]
 5: [5, 15, 35, 51, 53]
 6: [6, 16, 23, 28, 32]
 7: [7, 17, 71, 117, 171]
 8: [8, 18, 24, 29, 36]
 9: [9, 19, 33, 91, 119]

```



## Ada


The solution uses the Package "Generic_Root" from the additive digital roots [[http://rosettacode.org/wiki/Digital_root#Ada]].


```Ada
with Ada.Text_IO, Generic_Root;   use Generic_Root;

procedure Multiplicative_Root is

   procedure Compute is new Compute_Root("*"); -- "*" for multiplicative roots

   package TIO renames Ada.Text_IO;
   package NIO is new TIO.Integer_IO(Number);

   procedure Print_Numbers(Target_Root: Number; How_Many: Natural) is
      Current: Number := 0;
      Root, Pers: Number;
   begin
       for I in 1 .. How_Many loop
	  loop
	     Compute(Current, Root, Pers);
	     exit when Root = Target_Root;
	     Current := Current + 1;
	  end loop;
	  NIO.Put(Current, Width => 6);
	  if I < How_Many then
	     TIO.Put(",");
	  end if;
	  Current := Current + 1;
       end loop;
   end Print_Numbers;

   Inputs: Number_Array := (123321, 7739, 893, 899998);
   Root, Pers: Number;
begin
   TIO.Put_Line("  Number   MDR    MP");
   for I in Inputs'Range loop
       Compute(Inputs(I), Root, Pers);
       NIO.Put(Inputs(I), Width => 8);
       NIO.Put(Root, Width => 6);
       NIO.Put(Pers, Width => 6);
       TIO.New_Line;
   end loop;
   TIO.New_Line;

   TIO.Put_Line(" MDR    first_five_numbers_with_that_MDR");
   for I in 0 .. 9 loop
      TIO.Put("  " & Integer'Image(I) & "  ");
      Print_Numbers(Target_Root => Number(I), How_Many => 5);
      TIO.New_Line;
   end loop;
end Multiplicative_Root;
```


```txt
  Number   MDR    MP
  123321     8     3
    7739     8     3
     893     2     3
  899998     0     2

 MDR    first_five_numbers_with_that_MDR
   0       0,    10,    20,    25,    30
   1       1,    11,   111,  1111, 11111
   2       2,    12,    21,    26,    34
   3       3,    13,    31,   113,   131
   4       4,    14,    22,    27,    39
   5       5,    15,    35,    51,    53
   6       6,    16,    23,    28,    32
   7       7,    17,    71,   117,   171
   8       8,    18,    24,    29,    36
   9       9,    19,    33,    91,   119

```



## ALGOL 68

```algol68
# Multiplicative Digital Roots                                                #

# structure to hold the results of calculating the digital root & persistence #
MODE DR = STRUCT( INT root, INT persistence );

# calculate the multiplicative digital root and persistence of a number       #
PROC md root = ( INT number )DR:
BEGIN

    # calculate the product of the digits of a number                         #
    PROC digit product = ( INT number )INT:
    BEGIN

        INT    result := 1;
        INT    rest   := number;

        WHILE
            result TIMESAB ( rest MOD 10 );
            rest   OVERAB  10;
            rest > 0
        DO
           SKIP
        OD;

        result
    END; # digit product #

    INT mp  := 0;
    INT mdr := ABS number;

    WHILE mdr > 9
    DO
        mp +:= 1;
        mdr := digit product( mdr )
    OD;

    ( mdr, mp )
END; # md root #

# prints a number and its MDR and MP                                          #
PROC print md root = ( INT number )VOID:
BEGIN
    DR mdr = md root( number );
    print( ( whole( number, -6 )
           , ": MDR: ", whole( root        OF mdr,  0 )
           , ", MP: ",  whole( persistence OF mdr, -2 )
           , newline
           )
         )
END; # print md root #

# prints the first few numbers with each possible Multiplicative Digital      #
# Root. The number of values to print is specified as a parameter             #
PROC tabulate mdr = ( INT number of values )VOID:
BEGIN

    [ 0 : 9, 1 : number of values ]INT mdr values;
    [ 0 : 9                       ]INT mdr counts;
    mdr counts[ AT 1 ] := ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );

    # find the first few numbers with each possible mdr                       #

    INT values found    := 0;
    INT required values := 10 * number of values;

    FOR value FROM 0 WHILE values found < required values
    DO
        DR mdr = md root( value );
        IF mdr counts[ root OF mdr ] < number of values
        THEN
            # need more values with this multiplicative digital root          #
            values found              +:= 1;
            mdr counts[ root OF mdr ] +:= 1;
            mdr values[ root OF mdr, mdr counts[ root OF mdr ] ] := value
        FI
    OD;

    # print the values #

    print( ( "MDR: [n0..n" + whole( number of values - 1, 0 ) + "]", newline ) );
    print( ( "
###   =====
", newline ) );
    FOR mdr pos FROM 1 LWB mdr values TO 1 UPB mdr values
    DO
        STRING separator := ": [";
        print( ( whole( mdr pos, -3 ) ) );
        FOR val pos FROM 2 LWB mdr values TO 2 UPB mdr values
        DO
            print( ( separator + whole( mdr values[ mdr pos, val pos ], 0 ) ) );
            separator := ", "
        OD;
        print( ( "]", newline ) )
    OD

END; # tabulate mdr #

main:(
    print md root( 123321 );
    print md root(   7739 );
    print md root(    893 );
    print md root( 899998 );
    tabulate mdr( 5 )
)
```

```txt

123321: MDR: 8, MP:  3
  7739: MDR: 8, MP:  3
   893: MDR: 2, MP:  3
899998: MDR: 0, MP:  2
MDR: [n0..n4]

###   =====

  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]

```



## ALGOL W


```algolw
begin
    % calculate the Multiplicative Digital Root (mdr) and Multiplicative Persistence (mp) of n %
    procedure getMDR ( integer value  n
                     ; integer result mdr, mp
                     ) ;
    begin
        mp  := 0;
        mdr := abs n;
        while mdr > 9 do begin
           integer v;
           v   := mdr;
           mdr := 1;
           while begin
               mdr := mdr * ( v rem 10 );
               v   := v div 10;
               v > 0
           end do begin end;
           mp := mp + 1;
        end while_mdr_gt_9 ;
    end getMDR ;

    % task test cases %
    write( "       N MDR MP" );
    for n := 123321, 7739, 893, 899998 do begin
        integer mdr, mp;
        getMDR( n, mdr, mp );
        write( s_w := 1, i_w := 8, n, i_w := 3, mdr, i_w := 2, mp )
    end for_n ;

    begin % find the first 5 numbers with each possible MDR %
        integer requiredMdrs;
        requiredMdrs := 5;
        begin
            integer array firstFew ( 0 :: 9, 1 :: requiredMdrs );
            integer array mdrFOund ( 0 :: 9 );
            integer       totalFound, requiredTotal, n;
            for i := 0 until 9 do mdrFound( i ) := 0;
            totalFound    := 0;
            requiredTotal := 10 * requiredMdrs;
            n             := -1;
            while totalFound < requiredTotal do begin
                integer mdr, mp;
                n := n + 1;
                getMDR( n, mdr, mp );
                if mdrFound( mdr ) < requiredMdrs then begin
                    % found another number with this MDR and haven't found enough yet %
                    totalFound                       := totalFound + 1;
                    mdrFound( mdr )                  := mdrFound( mdr ) + 1;
                    firstFew( mdr, mdrFound( mdr ) ) := n
                end if_found_another_MDR
            end while_totalFound_lt_requiredTotal ;
            % print the table of MDRs andnumbers %
            write( "MDR: [n0..n4]" );
            write( "
###   =====
" );
            for v := 0 until 9 do begin
                write( i_w := 3, s_w := 0, v, ": [" );
                for foundPos := 1 until requiredMdrs do begin
                    if foundPos > 1 then writeon( s_w := 0, ", " );
                    writeon( i_w := 1, s_w := 0, firstFew( v, foundPos ) )
                end for_foundPos ;
                writeon( s_w := 0, "]" )
            end for_v
        end
    end

end.
```

```txt

       N MDR MP
  123321   8  3
    7739   8  3
     893   2  3
  899998   0  2
MDR: [n0..n4]

###   =====

  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]

```



## AWK


```AWK
# Multiplicative Digital Roots

BEGIN {

    printMdrAndMp( 123321 );
    printMdrAndMp(   7739 );
    printMdrAndMp(    893 );
    printMdrAndMp( 899998 );

    tabulateMdr( 5 );

} # BEGIN

function printMdrAndMp( n )
{
    calculateMdrAndMp( n );
    printf( "%6d: MDR: %d, MP: %2d\n", n, MDR, MP );
} # printMdrAndMp

function calculateMdrAndMp( n,                     mdrStr, digit )
{

    MP  = 0;                     # global Multiplicative Persistence
    MDR = ( n < 0 ? -n : n );    # global Multiplicative Digital Root

    while( MDR > 9 )
    {
        MP ++;
        mdrStr = "" MDR;
        MDR    = 1;
        for( digit = 1; digit <= length( mdrStr ); digit ++ )
        {
            MDR *= ( substr( mdrStr, digit, 1 ) * 1 );
        } # for digit
    } # while MDR > 9

} # calculateMdrAndMp

function tabulateMdr( n,                  rqdValues, valueCount, value, pos )
{

    # generate a table of the first n numbers with each possible MDR

    rqdValues  = n * 10;
    valueCount = 0;

    for( value = 0; valueCount < rqdValues; value ++ )
    {
        calculateMdrAndMp( value );
        if( mdrCount[ MDR ] < n )
        {
            # still need another value with this MDR
            valueCount ++;
            mdrCount[ MDR ] ++;
            mdrValues[ MDR ":" mdrCount[ MDR ] ] = value;
        } # if mdrCount[ MDR ] < n
    } # for value

    # print the table

    printf( "MDR: [n0..n%d]\n", n - 1 );
    printf( "
###   =====
\n" );

    for( pos = 0; pos < 10; pos ++ )
    {
        printf( "%3d:", pos );
        separator = " [";
        for( value = 1; value <= n; value ++ )
        {
            printf( "%s%d", separator, mdrValues[ pos ":" value ] );
            separator = ", "
        } # for value
        printf( "]\n" );
    } # for pos

} # tabulateMdr
```

```txt

123321: MDR: 8, MP:  3
  7739: MDR: 8, MP:  3
   893: MDR: 2, MP:  3
899998: MDR: 0, MP:  2
MDR: [n0..n4]

###   =====

  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]

```



## Bracmat


```bracmat
(
& ( MP/MDR
  =   prod L n
    .   ( prod
        =   d
          .   @(!arg:%@?d ?arg)&!d*prod$!arg
            | 1
        )
      & !arg:?L
      &   whl
        ' ( @(!arg:? [>1)
          & (prod$!arg:?arg) !L:?L
          )
      & !L:? [?n
      & (!n+-1.!arg)
  )
& ( test
  =   n
    .     !arg:%?n ?arg
        & out$(!n "\t:" MP/MDR$!n)
        & test$!arg
      |
  )
& test$(123321 7739 893 899998)
& 0:?i
& 1:?collecting:?done
&   whl
  ' ( !i+1:?i
    & MP/MDR$!i:(?MP.?MDR)
    & ( !done:?*(!MDR.)^((?.)+?)*?
      |   (!MDR.)^(!i.)*!collecting:?collecting
        & (   !collecting:?A*(!MDR.)^(?is+[5)*?Z
            & !A*!Z:?collecting
            & (!MDR.)^!is*!done:?done
          |
          )
      )
    & !collecting:~1
    )
&   whl
  ' ( !done:(?MDR.)^?is*?done
    & put$(!MDR ":")
    & whl'(!is:(?i.)+?is&put$(!i " "))
    & put$\n
    )
);
```

```txt
123321  : (3.8)
7739    : (3.8)
893     : (3.2)
899998  : (2.0)
0 :10  20  25  30  40
1 :1  11  111  1111  11111
2 :2  12  21  26  34
3 :3  13  31  113  131
4 :4  14  22  27  39
5 :5  15  35  51  53
6 :6  16  23  28  32
7 :7  17  71  117  171
8 :8  18  24  29  36
9 :9  19  33  91  119
```



## C


```C

#include <stdio.h>

#define twidth 5
#define mdr(rmdr, rmp, n)\
    do { *rmp = 0; _mdr(rmdr, rmp, n); } while (0)

void _mdr(int *rmdr, int *rmp, long long n)
{
    /* Adjust r if 0 case, so we don't return 1 */
    int r = n ? 1 : 0;
    while (n) {
        r *= (n % 10);
        n /= 10;
    }

    (*rmp)++;
    if (r >= 10)
        _mdr(rmdr, rmp, r);
    else
        *rmdr = r;
}

int main(void)
{
    int i, j, vmdr, vmp;
    const int values[] = { 123321, 7739, 893, 899998 };
    const int vsize    = sizeof(values) / sizeof(values[0]);

    /* Initial test values */
    printf("Number    MDR    MP\n");
    for (i = 0; i < vsize; ++i) {
        mdr(&vmdr, &vmp, values[i]);
        printf("%6d   %3d   %3d\n", values[i], vmdr, vmp);
    }

    /* Determine table values */
    int table[10][twidth] = { 0 };
    int tfill[10]         = { 0 };
    int total             = 0;
    for (i = 0; total < 10 * twidth; ++i) {
        mdr(&vmdr, &vmp, i);
        if (tfill[vmdr] < twidth) {
            table[vmdr][tfill[vmdr]++] = i;
            total++;
        }
    }

    /* Print calculated table values */
    printf("\nMDR: [n0..n4]\n");
    for (i = 0; i < 10; ++i) {
        printf("%3d: [", i);
        for (j = 0; j < twidth; ++j)
            printf("%d%s", table[i][j], j != twidth - 1 ? ", " : "");
        printf("]\n");
    }

    return 0;
}

```

```txt

Number    MDR    MP
123321     8     3
  7739     8     3
   893     2     3
899998     0     2

MDR: [n0..n4]
  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static Tuple<int, int> DigitalRoot(long num)
    {
        int mp = 0;
        while (num > 9)
        {
            num = num.ToString().ToCharArray().Select(x => x - '0').Aggregate((a, b) => a * b);
            mp++;
        }
        return new Tuple<int, int>(mp, (int)num);
    }
    static void Main(string[] args)
    {
        foreach (long num in new long[] { 123321, 7739, 893, 899998 })
        {
            var t = DigitalRoot(num);
            Console.WriteLine("{0} has multiplicative persistence {1} and multiplicative digital root {2}", num, t.Item1, t.Item2);
        }

        const int twidth = 5;
        List<long>[] table = new List<long>[10];
        for (int i = 0; i < 10; i++)
            table[i] = new List<long>();
        long number = -1;
        while (table.Any(x => x.Count < twidth))
        {
            var t = DigitalRoot(++number);
            if (table[t.Item2].Count < twidth)
                table[t.Item2].Add(number);
        }
        for (int i = 0; i < 10; i++)
            Console.WriteLine(" {0} : [{1}]", i, string.Join(", ", table[i]));
    }
}
```

```txt
123321 has multiplicative persistence 3 and multiplicative digital root 8
7739 has multiplicative persistence 3 and multiplicative digital root 8
893 has multiplicative persistence 3 and multiplicative digital root 2
899998 has multiplicative persistence 2 and multiplicative digital root 0
 0 : [0, 10, 20, 25, 30]
 1 : [1, 11, 111, 1111, 11111]
 2 : [2, 12, 21, 26, 34]
 3 : [3, 13, 31, 113, 131]
 4 : [4, 14, 22, 27, 39]
 5 : [5, 15, 35, 51, 53]
 6 : [6, 16, 23, 28, 32]
 7 : [7, 17, 71, 117, 171]
 8 : [8, 18, 24, 29, 36]
 9 : [9, 19, 33, 91, 119]
```



## C++


```cpp

#include <iomanip>
#include <map>
#include <vector>
#include <iostream>
using namespace std;

void calcMDR( int n, int c, int& a, int& b )
{
    int m = n % 10; n /= 10;
    while( n )
    {
	m *= ( n % 10 );
	n /= 10;
    }
    if( m >= 10 ) calcMDR( m, ++c, a, b );
    else { a = m; b = c; }
}

void table()
{
    map<int, vector<int> > mp;
    int n = 0, a, b;
    bool f = true;
    while( f )
    {
	f = false;
	calcMDR( n, 1, a, b );
	mp[a].push_back( n );
	n++;
	for( int x = 0; x < 10; x++ )
	    if( mp[x].size() < 5 )
	    { f = true; break; }
    }

    cout << "|  MDR  |  [n0..n4]\n+-------+------------------------------------+\n";
    for( int x = 0; x < 10; x++ )
    {
	cout << right << "| " << setw( 6 ) << x << "| ";
	for( vector<int>::iterator i = mp[x].begin(); i != mp[x].begin() + 5; i++ )
	    cout << setw( 6 ) << *i << " ";
	cout << "|\n";
    }
    cout << "+-------+------------------------------------+\n\n";
}

int main( int argc, char* argv[] )
{
    cout << "|  NUMBER  |   MDR    |    MP    |\n+----------+----------+----------+\n";
    int numbers[] = { 123321, 7739, 893, 899998 }, a, b;
    for( int x = 0; x < 4; x++ )
    {
	cout << right << "| "  << setw( 9 ) << numbers[x] << "| ";
	calcMDR( numbers[x], 1, a, b );
	cout << setw( 9 ) << a  << "| " << setw( 9 ) << b << "|\n";
    }
    cout << "+----------+----------+----------+\n\n";
    table();
    return system( "pause" );
}

```

```txt

|  NUMBER  |   MDR    |    MP    |
+----------+----------+----------+
|    123321|         8|         3|
|      7739|         8|         3|
|       893|         2|         3|
|    899998|         0|         2|
+----------+----------+----------+

|  MDR  |  [n0..n4]
+-------+------------------------------------+
|      0|      0     10     20     25     30 |
|      1|      1     11    111   1111  11111 |
|      2|      2     12     21     26     34 |
|      3|      3     13     31    113    131 |
|      4|      4     14     22     27     39 |
|      5|      5     15     35     51     53 |
|      6|      6     16     23     28     32 |
|      7|      7     17     71    117    171 |
|      8|      8     18     24     29     36 |
|      9|      9     19     33     91    119 |
+-------+------------------------------------+

```



## Common Lisp


```lisp

(defun mdr/p (n)
  "Return a list with MDR and MP of n"
  (if (< n 10)
    (list n 0)
    (mdr/p-aux n 1 1)))

(defun mdr/p-aux (n a c)
  (cond ((and (zerop n) (< a 10)) (list a c))
	((zerop n) (mdr/p-aux a 1 (+ c 1)))
	(t (mdr/p-aux (floor n 10) (* (rem n 10) a) c))))

(defun first-n-number-for-each-root (n &optional (r 0) (lst nil) (c 0))
  "Return the first m number with MDR = 0 to 9"
  (cond ((and (= (length lst) n) (= r 9)) (format t "~3@a: ~a~%" r (reverse lst)))
	((= (length lst) n) (format t "~3@a: ~a~%" r (reverse lst))
	                    (first-n-number-for-each-root n (+ r 1) nil 0))
	((= (first (mdr/p c)) r) (first-n-number-for-each-root n r (cons c lst) (+ c 1)))
	(t (first-n-number-for-each-root n r lst (+ c 1)))))

(defun start ()
  (format t "Number: MDR  MD~%")
  (loop for el in '(123321 7739 893 899998)
        do (format t "~6@a: ~{~3@a ~}~%" el (mdr/p el)))
  (format t "~%MDR: [n0..n4]~%")
  (first-n-number-for-each-root 5))
```

```txt

Number: MDR  MD
123321:   8   3
  7739:   8   3
   893:   2   3
899998:   0   2

MDR: [n0..n4]
  0: (0 10 20 25 30)
  1: (1 11 111 1111 11111)
  2: (2 12 21 26 34)
  3: (3 13 31 113 131)
  4: (4 14 22 27 39)
  5: (5 15 35 51 53)
  6: (6 16 23 28 32)
  7: (7 17 71 117 171)
  8: (8 18 24 29 36)
  9: (9 19 33 91 119)
```



## Component Pascal

```oberon2

MODULE MDR;
IMPORT StdLog, Strings, TextMappers, DevCommanders;

PROCEDURE CalcMDR(x: LONGINT; OUT mdr, mp: LONGINT);
VAR
	str: ARRAY 64 OF CHAR;
	i: INTEGER;
BEGIN
	mdr := 1; mp := 0;
	LOOP
		Strings.IntToString(x,str);
		IF LEN(str$) = 1 THEN mdr := x; EXIT END;
		i := 0;mdr := 1;
		WHILE i < LEN(str$) DO
			mdr := mdr * (ORD(str[i]) - ORD('0'));
			INC(i)
		END;
		INC(mp);
		x := mdr
	END
END CalcMDR;

PROCEDURE Do*;
VAR
	mdr,mp: LONGINT;
	s: TextMappers.Scanner;
BEGIN
	s.ConnectTo(DevCommanders.par.text);
	s.SetPos(DevCommanders.par.beg);
	REPEAT
		s.Scan;
		IF (s.type = TextMappers.int) OR (s.type = TextMappers.lint) THEN
			CalcMDR(s.int,mdr,mp);
			StdLog.Int(s.int);
			StdLog.String(" MDR: ");StdLog.Int(mdr);
			StdLog.String(" MP: ");StdLog.Int(mp);StdLog.Ln
		END
	UNTIL s.rider.eot;
END Do;

PROCEDURE Show(i: INTEGER; x: ARRAY OF LONGINT);
VAR
	k: INTEGER;
BEGIN
	StdLog.Int(i);StdLog.String(": ");
	FOR k := 0 TO LEN(x) - 1 DO
		StdLog.Int(x[k])
	END;
	StdLog.Ln
END Show;

PROCEDURE FirstFive*;
VAR
	i,j: INTEGER;
	five: ARRAY 5 OF LONGINT;
	x,mdr,mp: LONGINT;
BEGIN
	FOR i := 0 TO 9 DO
		j := 0;x := 0;
		WHILE (j < LEN(five)) DO
			CalcMDR(x,mdr,mp);
			IF mdr = i THEN five[j] := x; INC(j) END;
			INC(x)
		END;
		Show(i,five)
	END
END FirstFive;

END MDR.

```

Execute:
^Q MDR.Do 123321 7739 893 899998 ~
```txt

 123321 MDR:  8 MP:  3
 7739 MDR:  8 MP:  3
 893 MDR:  2 MP:  3
 899998 MDR:  0 MP:  2

```

Execute:
^Q MDR.FirstFive
```txt

 0:  0 10 20 25 30
 1:  1 11 111 1111 11111
 2:  2 12 21 26 34
 3:  3 13 31 113 131
 4:  4 14 22 27 39
 5:  5 15 35 51 53
 6:  6 16 23 28 32
 7:  7 17 71 117 171
 8:  8 18 24 29 36
 9:  9 19 33 91 119

```


## D

```d
import std.stdio, std.algorithm, std.typecons, std.range, std.conv;

/// Multiplicative digital root.
auto mdRoot(in int n) pure /*nothrow*/ {
    auto mdr = [n];
    while (mdr.back > 9)
        mdr ~= reduce!q{a * b}(1, mdr.back.text.map!(d => d - '0'));
        //mdr ~= mdr.back.text.map!(d => d - '0').mul;
        //mdr ~= mdr.back.reverseDigits.mul;
    return tuple(mdr.length - 1, mdr.back);
}

void main() {
    "Number: (MP, MDR)\n
### ===  ======
".writeln;
    foreach (immutable n; [123321, 7739, 893, 899998])
        writefln("%6d: (%s, %s)", n, n.mdRoot[]);

    auto table = (int[]).init.repeat.enumerate!int.take(10).assocArray;
    auto n = 0;
    while (table.byValue.map!walkLength.reduce!min < 5) {
        table[n.mdRoot[1]] ~= n;
        n++;
    }
    "\nMP: [n0..n4]\n==
### ==
".writeln;
    foreach (const mp; table.byKey.array.sort())
        writefln("%2d: %s", mp, table[mp].take(5));
}
```

```txt
Number: (MP, MDR)

### ===  ======

123321: (3, 8)
  7739: (3, 8)
   893: (3, 2)
899998: (2, 0)

MP: [n0..n4]
==
### ==

 0: [0, 10, 20, 25, 30]
 1: [1, 11, 111, 1111, 11111]
 2: [2, 12, 21, 26, 34]
 3: [3, 13, 31, 113, 131]
 4: [4, 14, 22, 27, 39]
 5: [5, 15, 35, 51, 53]
 6: [6, 16, 23, 28, 32]
 7: [7, 17, 71, 117, 171]
 8: [8, 18, 24, 29, 36]
 9: [9, 19, 33, 91, 119]
```



### Alternative Version


```d
import std.stdio, std.algorithm, std.typecons, std.range;

uint digitsProduct(uint n) pure nothrow @nogc {
    typeof(return) result = !!n;
    while (n) {
        result *= n % 10;
        n /= 10;
    }
    return result;
}

/// Multiplicative digital root.
Tuple!(size_t, uint) mdRoot(uint m) pure nothrow {
    auto mdr = m
               .recurrence!((a, n) => a[n - 1].digitsProduct)
               .until!q{ a <= 9 }(OpenRight.no).array;
    return tuple(mdr.length - 1, mdr.back);
}

void main() {
    "Number: (MP, MDR)\n
### ===  ======
".writeln;
    foreach (immutable n; [123321, 7739, 893, 899998])
        writefln("%6d: (%s, %s)", n, n.mdRoot[]);

    auto table = (int[]).init.repeat.enumerate!int.take(10).assocArray;
    auto n = 0;
    while (table.byValue.map!walkLength.reduce!min < 5) {
        table[n.mdRoot[1]] ~= n;
        n++;
    }
    "\nMP: [n0..n4]\n==
### ==
".writeln;
    foreach (const mp; table.byKey.array.sort())
        writefln("%2d: %s", mp, table[mp].take(5));
}
```



### More Efficient Version


```d
import std.stdio, std.algorithm, std.range;

/// Multiplicative digital root.
uint[2] mdRoot(in uint n) pure nothrow @nogc {
    uint mdr = n;
    uint count = 0;

    while (mdr > 9) {
        uint m = mdr;
        uint digitsMul = !!m;
        while (m) {
            digitsMul *= m % 10;
            m /= 10;
        }
        mdr = digitsMul;
        count++;
    }

    return [count, mdr];
}

void main() {
    "Number: [MP, MDR]\n
### ===  ======
".writeln;
    foreach (immutable n; [123321, 7739, 893, 899998])
        writefln("%6d: %s", n, n.mdRoot);

    auto table = (int[]).init.repeat.enumerate!int.take(10).assocArray;
    auto n = 0;
    while (table.byValue.map!walkLength.reduce!min < 5) {
        table[n.mdRoot[1]] ~= n;
        n++;
    }
    "\nMP: [n0..n4]\n==
### ==
".writeln;
    foreach (const mp; table.byKey.array.sort())
        writefln("%2d: %s", mp, table[mp].take(5));
}
```

The output is similar.


## Elixir


```elixir
defmodule Digital do
  def mdroot(n), do: mdroot(n, 0)

  defp mdroot(n, persist) when n < 10, do: {n, persist}
  defp mdroot(n, persist), do: mdroot(product(n, 1), persist+1)

  defp product(0, prod), do: prod
  defp product(n, prod), do: product(div(n, 10), prod*rem(n, 10))

  def task1(data) do
    IO.puts "Number: MDR  MP\n
### ===
  =="
    Enum.each(data, fn n ->
      {mdr, persist} = mdroot(n)
      :io.format "~6w:   ~w  ~2w~n", [n, mdr, persist]
    end)
  end

  def task2(m \\ 5) do
    IO.puts "\nMDR: [n0..n#{m-1}]\n
###   =====
"
    map = add_map(0, m, Map.new)
    Enum.each(0..9, fn i ->
      first = map[i] |> Enum.reverse |> Enum.take(m)
      IO.puts "  #{i}: #{inspect first}"
    end)
  end

  defp add_map(n, m, map) do
    {mdr, _persist} = mdroot(n)
    new_map = Map.update(map, mdr, [n], fn vals -> [n | vals] end)
    min_len = Map.values(new_map) |> Enum.map(&length(&1)) |> Enum.min
    if min_len < m, do: add_map(n+1, m, new_map),
                  else: new_map
  end
end

Digital.task1([123321, 7739, 893, 899998])
Digital.task2
```


```txt

Number: MDR  MP

### ===
  ==
123321:   8   3
  7739:   8   3
   893:   2   3
899998:   0   2

MDR: [n0..n4]

###   =====

  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]

```



## Factor


```factor
USING: arrays formatting fry io kernel lists lists.lazy math
math.text.utils prettyprint sequences ;
IN: rosetta-code.multiplicative-digital-root

: mdr ( n -- {persistence,root} )
    0 swap
    [ 1 digit-groups dup length 1 > ] [ product [ 1 + ] dip ] while
    dup empty? [ drop { 0 } ] when first 2array ;

: print-mdr ( n -- )
    dup [ 1array ] dip mdr append
    "%-12d has multiplicative persistence %d and MDR %d.\n"
    vprintf ;

: first5 ( n -- seq ) ! first 5 numbers with MDR of n
    0 lfrom swap '[ mdr second _ = ] lfilter 5 swap ltake list>array ;

: print-first5 ( i n -- )
    "%-5d" printf bl first5 [ "%-5d " printf ] each nl ;

: header ( -- )
    "MDR | First five numbers with that MDR" print
    "--------------------------------------" print ;

: first5-table ( -- )
    header 10 iota [ print-first5 ] each-index ;

: main ( -- )
    { 123321 7739 893 899998 } [ print-mdr ] each nl first5-table ;

MAIN: main
```

```txt

123321       has multiplicative persistence 3 and MDR 8.
7739         has multiplicative persistence 3 and MDR 8.
893          has multiplicative persistence 3 and MDR 2.
899998       has multiplicative persistence 2 and MDR 0.

MDR | First five numbers with that MDR
--------------------------------------
0     0     10    20    25    30
1     1     11    111   1111  11111
2     2     12    21    26    34
3     3     13    31    113   131
4     4     14    22    27    39
5     5     15    35    51    53
6     6     16    23    28    32
7     7     17    71    117   171
8     8     18    24    29    36
9     9     19    33    91    119

```



## Fortran


```Fortran

!Implemented by Anant Dixit (Oct, 2014)
program mdr
implicit none
integer :: i, mdr, mp, n, j
character(len=*), parameter :: hfmt = '(A18)', nfmt = '(I6)'
character(len=*), parameter :: cfmt = '(A3)', rfmt = '(I3)', ffmt = '(I9)'

write(*,hfmt) 'Number   MDR   MP '
write(*,*) '------------------'

i = 123321
call root_pers(i,mdr,mp)
write(*,nfmt,advance='no') i
write(*,cfmt,advance='no') '   '
write(*,rfmt,advance='no') mdr
write(*,cfmt,advance='no') '   '
write(*,rfmt) mp

i = 3939
call root_pers(i,mdr,mp)
write(*,nfmt,advance='no') i
write(*,cfmt,advance='no') '   '
write(*,rfmt,advance='no') mdr
write(*,cfmt,advance='no') '   '
write(*,rfmt) mp

i = 8822
call root_pers(i,mdr,mp)
write(*,nfmt,advance='no') i
write(*,cfmt,advance='no') '   '
write(*,rfmt,advance='no') mdr
write(*,cfmt,advance='no') '   '
write(*,rfmt) mp

i = 39398
call root_pers(i,mdr,mp)
write(*,nfmt,advance='no') i
write(*,cfmt,advance='no') '   '
write(*,rfmt,advance='no') mdr
write(*,cfmt,advance='no') '   '
write(*,rfmt) mp

write(*,*)
write(*,*)
write(*,*) 'First five numbers with MDR in first column: '
write(*,*) '---------------------------------------------'

do i = 0,9
  n = 0
  j = 0
  write(*,rfmt,advance='no') i
  do
    call root_pers(j,mdr,mp)
    if(mdr.eq.i) then
      n = n+1
      if(n.eq.5) then
        write(*,ffmt) j
        exit
      else
        write(*,ffmt,advance='no') j
      end if
    end if
    j = j+1
  end do
end do

end program

subroutine root_pers(i,mdr,mp)
implicit none
integer :: N, s, a, i, mdr, mp
n = i
a = 0
if(n.lt.10) then
  mdr = n
  mp = 0
  return
end if
do while(n.ge.10)
  a = a + 1
  s = 1
  do while(n.gt.0)
    s = s * mod(n,10)
    n = int(real(n)/10.0D0)
  end do
  n = s
end do
mdr = s
mp = a
end subroutine


```



```txt

Number   MDR   MP
 ------------------
123321     8     3
  3939     2     4
  8822     0     3
 39398     0     3


 First five numbers with MDR in first column:
 ---------------------------------------------
  0        0       10       20       25       30
  1        1       11      111     1111    11111
  2        2       12       21       26       34
  3        3       13       31      113      131
  4        4       14       22       27       39
  5        5       15       35       51       53
  6        6       16       23       28       32
  7        7       17       71      117      171
  8        8       18       24       29       36
  9        9       19       33       91      119


```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function multDigitalRoot(n As UInteger, ByRef mp As Integer, base_ As Integer = 10) As Integer
  Dim mdr As Integer
  mp = 0
  Do
    mdr = IIf(n > 0, 1, 0)
    While n > 0
      mdr *= n Mod base_
      n = n \ base_
    Wend
    mp += 1
    n = mdr
  Loop until mdr < base_
  Return mdr
End Function

Dim As Integer mdr, mp
Dim a(3) As UInteger = {123321, 7739, 893, 899998}
For i As UInteger = 0 To 3
 mp = 0
 mdr = multDigitalRoot(a(i), mp)
 Print a(i); Tab(10); "MDR ="; mdr; Tab(20); "MP ="; mp
 Print
Next
Print
Print "MDR    1     2     3     4     5"
Print "
###   ========================
"
Print
Dim num(0 To 9, 0 To 5) As UInteger '' all zero by default
Dim As UInteger n = 0, count = 0
Do
  mdr = multDigitalRoot(n, mp)
  If num(mdr, 0) < 5 Then
    num(mdr, 0) += 1
    num(mdr, num(mdr, 0)) = n
    count += 1
  End If
  n += 1
Loop Until count = 50

For i As UInteger = 0 To 9
  Print i; ":" ;
  For j As UInteger = 1 To 5
    Print Using "######"; num(i, j);
  Next j
  Print
Next i

Print
Print "Press any key to quit"
Sleep
```


```txt

123321   MDR = 8   MP = 3

7739     MDR = 8   MP = 3

893      MDR = 2   MP = 3

899998   MDR = 0   MP = 2


MDR    1     2     3     4     5

###   ========================


0:     0    10    20    25    30
1:     1    11   111  1111 11111
2:     2    12    21    26    34
3:     3    13    31   113   131
4:     4    14    22    27    39
5:     5    15    35    51    53
6:     6    16    23    28    32
7:     7    17    71   117   171
8:     8    18    24    29    36
9:     9    19    33    91   119

```



## Go


```go
package main

import "fmt"

// Only valid for n > 0 && base >= 2
func mult(n uint64, base int) (mult uint64) {
	for mult = 1; mult > 0 && n > 0; n /= uint64(base) {
		mult *= n % uint64(base)
	}
	return
}

// Only valid for n >= 0 && base >= 2
func MultDigitalRoot(n uint64, base int) (mp, mdr int) {
	var m uint64
	for m = n; m >= uint64(base); mp++ {
		m = mult(m, base)
	}
	return mp, int(m)
}

func main() {
	const base = 10
	const size = 5

	const testFmt = "%20v %3v %3v\n"
	fmt.Printf(testFmt, "Number", "MDR", "MP")
	for _, n := range [...]uint64{
		123321, 7739, 893, 899998,
		18446743999999999999,
		// From http://mathworld.wolfram.com/MultiplicativePersistence.html
		3778888999, 277777788888899,
	} {
		mp, mdr := MultDigitalRoot(n, base)
		fmt.Printf(testFmt, n, mdr, mp)
	}
	fmt.Println()

	var list [base][]uint64
	for i := range list {
		list[i] = make([]uint64, 0, size)
	}
	for cnt, n := size*base, uint64(0); cnt > 0; n++ {
		_, mdr := MultDigitalRoot(n, base)
		if len(list[mdr]) < size {
			list[mdr] = append(list[mdr], n)
			cnt--
		}
	}
	const tableFmt = "%3v: %v\n"
	fmt.Printf(tableFmt, "MDR", "First")
	for i, l := range list {
		fmt.Printf(tableFmt, i, l)
	}
}
```

```txt

              Number MDR  MP
              123321   8   3
                7739   8   3
                 893   2   3
              899998   0   2
18446743999999999999   0   2
          3778888999   0  10
     277777788888899   0  11

MDR: First
  0: [0 10 20 25 30]
  1: [1 11 111 1111 11111]
  2: [2 12 21 26 34]
  3: [3 13 31 113 131]
  4: [4 14 22 27 39]
  5: [5 15 35 51 53]
  6: [6 16 23 28 32]
  7: [7 17 71 117 171]
  8: [8 18 24 29 36]
  9: [9 19 33 91 119]

```



## Haskell

Note that in the function <code>mdrNums</code> we don't know in advance how many numbers we'll need to examine to find the first 5 associated with all the MDRs.  Using a lazy array to accumulate these numbers allows us to keep the function simple.

```haskell
import Control.Arrow
import Data.Array
import Data.LazyArray
import Data.List (unfoldr)
import Data.Tuple
import Text.Printf

-- The multiplicative persistence (MP) and multiplicative digital root (MDR) of
-- the argument.
mpmdr :: Integer -> (Int, Integer)
mpmdr = (length *** head) . span (> 9) . iterate (product . digits)

-- Pairs (mdr, ns) where mdr is a multiplicative digital root and ns are the
-- first k numbers having that root.
mdrNums :: Int -> [(Integer, [Integer])]
mdrNums k = assocs $ lArrayMap (take k) (0,9) [(snd $ mpmdr n, n) | n <- [0..]]

digits :: Integral t => t -> [t]
digits 0 = [0]
digits n = unfoldr step n
  where step 0 = Nothing
        step k = Just (swap $ quotRem k 10)

printMpMdrs :: [Integer] -> IO ()
printMpMdrs ns = do
  putStrLn "Number MP MDR"
  putStrLn "
### === ==
"
  sequence_ [printf "%6d %2d %2d\n" n p r | n <- ns, let (p,r) = mpmdr n]

printMdrNums:: Int -> IO ()
printMdrNums k = do
  putStrLn "MDR Numbers"
  putStrLn "
###  ====
"
  let showNums = unwords . map show
  sequence_ [printf "%2d  %s\n" mdr $ showNums ns | (mdr,ns) <- mdrNums k]

main :: IO ()
main = do
  printMpMdrs [123321, 7739, 893, 899998]
  putStrLn ""
  printMdrNums 5
```

Note that the values in the first column of the table are MDRs, as shown in the task's sample output, not MP as incorrectly stated in the task statement and column header.

```txt
Number MP MDR

### === ==

123321  3  8
  7739  3  8
   893  3  2
899998  2  0

MDR Numbers

###  ====

 0  0 10 20 25 30
 1  1 11 111 1111 11111
 2  2 12 21 26 34
 3  3 13 31 113 131
 4  4 14 22 27 39
 5  5 15 35 51 53
 6  6 16 23 28 32
 7  7 17 71 117 171
 8  8 18 24 29 36
 9  9 19 33 91 119
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages:

```unicon
procedure main(A)
    write(right("n",8)," ",right("MP",8),right("MDR",5))
    every r := mdr(n := 123321|7739|893|899998) do
        write(right(n,8),":",right(r[1],8),right(r[2],5))
    write()
    write(right("MDR",5),"  ","[n0..n4]")
    every m := 0 to 9 do {
        writes(right(m,5),": [")
        every writes(right((m = mdr(n := seq(m))[2],.n)\5,6))
        write("]")
        }
end

procedure mdr(m)
    i := 0
    while (.m > 10, m := multd(m), i+:=1)
    return [i,m]
end

procedure multd(m)
    c := 1
    while m > 0 do c *:= 1(m%10, m/:=10)
    return c
end
```


```txt

->drmdr
       n       MP  MDR
  123321:       3    8
    7739:       3    8
     893:       3    2
  899998:       2    0

  MDR  [n0..n4]
    0: [     0    20    30    40    45]
    1: [     1    11   111  1111 11111]
    2: [     2    12    21    26    34]
    3: [     3    13    31   113   131]
    4: [     4    14    22    27    39]
    5: [     5    15    35    51    53]
    6: [     6    16    23    28    32]
    7: [     7    17    71   117   171]
    8: [     8    18    24    29    36]
    9: [     9    19    33    91   119]
->

```



## J


First, we need something to split a number into digits:


```J
   10&#.inv 123321
1 2 3 3 2 1
```


Second, we need to find their product:


```J
   */@(10&#.inv) 123321
36
```


Then we use this inductively until it converges:


```J
   */@(10&#.inv)^:a: 123321
123321 36 18 8
```


MP is one less than the length of this list, and MDR is the last element of this list:


```J
   (<:@#,{:) */@(10&#.inv)^:a: 123321
3 8
   (<:@#,{:) */@(10&#.inv)^:a: 7739
3 8
   (<:@#,{:) */@(10&#.inv)^:a: 893
3 2
   (<:@#,{:) */@(10&#.inv)^:a: 899998
2 0
```


For the table, we don't need that whole list, we only need the final value. Then use these values to classify the original argument (taking the first five from each group):


```J
   (5&{./.~ (*/@(10&#.inv)^:_)"0) i.20000
0 10  20   25    30
1 11 111 1111 11111
2 12  21   26    34
3 13  31  113   131
4 14  22   27    39
5 15  35   51    53
6 16  23   28    32
7 17  71  117   171
8 18  24   29    36
9 19  33   91   119
```


Note that since the first 10 non-negative integers are single digit values, the first column here doubles as a label (representing the corresponding multiplicative digital root).


## Java

```java
import java.util.*;

public class MultiplicativeDigitalRoot {

    public static void main(String[] args) {

        System.out.println("NUMBER  MDR   MP");
        for (long n : new long[]{123321, 7739, 893, 899998}) {
            long[] a = multiplicativeDigitalRoot(n);
            System.out.printf("%6d %4d %4d%n", a[0], a[1], a[2]);
        }

        System.out.println();

        Map<Long, List<Long>> table = new HashMap<>();
        for (long i = 0; i < 10; i++)
            table.put(i, new ArrayList<>());

        for (long cnt = 0, n = 0; cnt < 10;) {
            long[] res = multiplicativeDigitalRoot(n++);
            List<Long> list = table.get(res[1]);
            if (list.size() < 5) {
                list.add(res[0]);
                cnt = list.size() == 5 ? cnt + 1 : cnt;
            }
        }

        System.out.println("MDR: first five numbers with same MDR");
        table.forEach((key, lst) -> {
            System.out.printf("%3d: ", key);
            lst.forEach(e -> System.out.printf("%6s ", e));
            System.out.println();
        });
    }

    public static long[] multiplicativeDigitalRoot(long n) {
        int mp = 0;
        long mdr = n;
        while (mdr > 9) {
            long m = mdr;
            long total = 1;
            while (m > 0) {
                total *= m % 10;
                m /= 10;
            }
            mdr = total;
            mp++;
        }
        return new long[]{n, mdr, mp};
    }
}
```



```txt
NUMBER  MDR   MP
123321    8    3
  7739    8    3
   893    2    3
899998    0    2

MDR: first five numbers with same MDR
  0:      0     10     20     25     30
  1:      1     11    111   1111  11111
  2:      2     12     21     26     34
  3:      3     13     31    113    131
  4:      4     14     22     27     39
  5:      5     15     35     51     53
  6:      6     16     23     28     32
  7:      7     17     71    117    171
  8:      8     18     24     29     36
  9:      9     19     33     91    119
```



## jq


```jq
def do_until(condition; next):
  def u: if condition then . else (next|u) end;
  u;

def mdroot(n):
  def multiply: reduce .[] as $i (1; .*$i);
  # state: [mdr, persist]
  [n, 0]
  | do_until( .[0] < 10;
              [(.[0] | tostring | explode | map(.-48) | multiply), .[1] + 1]
            );

# Produce a table with 10 rows (numbered from 0),
# showing the first n numbers having the row-number as the mdr
def tabulate(n):
  # state: [answer_matrix, next_i]
  def tab:
    def minlength: map(length) | min;
    .[0] as $matrix
    | .[1] as $i
    | if (.[0]|minlength) == n then .[0]
      else (mdroot($i) | .[0]) as $mdr
      | if $matrix[$mdr]|length < n then
          ($matrix[$mdr] + [$i]) as $row
          | $matrix | setpath([$mdr]; $row)
        else $matrix
        end
      | [ ., $i + 1 ]
      | tab
      end;

  [[], 0]  | tab;
```

'''Example''':
```jq

def neatly:
  . as $in
  | range(0;length)
  | "\(.): \($in[.])";

def rjust(n): tostring | (n-length)*" " + .;

# The task:
"  i   : [MDR, MP]",
((123321, 7739, 893, 899998) as $i
 | "\($i|rjust(6)): \(mdroot($i))"),
"",
"Tabulation",
"MDR: [n0..n4]",
(tabulate(5) | neatly)
```

```sh
$ jq -n -r -c -f mdr.jq

  i   : [MDR, MP]
123321: [8,3]
  7739: [8,3]
   893: [2,3]
899998: [0,2]

Tabulation
MDR: [n0..n4]
0: [0,10,20,25,30]
1: [1,11,111,1111,11111]
2: [2,12,21,26,34]
3: [3,13,31,113,131]
4: [4,14,22,27,39]
5: [5,15,35,51,53]
6: [6,16,23,28,32]
7: [7,17,71,117,171]
8: [8,18,24,29,36]
9: [9,19,33,91,119]
```



## Julia

'''Function'''

```Julia

function digitalmultroot{S<:Integer,T<:Integer}(n::S, bs::T=10)
    -1 < n && 1 < bs || throw(DomainError())
    ds = n
    pers = 0
    while bs <= ds
        ds = prod(digits(ds, bs))
        pers += 1
    end
    return (pers, ds)
end

```

'''Main'''

```Julia

const bs = 10
const excnt = 5

println("Testing Multiplicative Digital Root.\n")
for i in [123321, 7739, 893, 899998]
    (pers, ds) = digitalmultroot(i, bs)
    print(@sprintf("%8d", i))
    print(" has persistence ", pers)
    println(" and digital root ", ds)
end

dmr = zeros(Int, bs, excnt)
hasroom = trues(bs)
dex = ones(Int, bs)

i = 0
while any(hasroom)
    (pers, ds) = digitalmultroot(i, bs)
    ds += 1
    if hasroom[ds]
        dmr[ds, dex[ds]] = i
        dex[ds] += 1
        if dex[ds] > excnt
            hasroom[ds] = false
        end
    end
    i += 1
end

println("\n MDR:    First ", excnt, " numbers having this MDR")
for (i, d) in enumerate(0:(bs-1))
    print(@sprintf("%4d: ", d))
    println(join([@sprintf("%6d", dmr[i, j]) for j in 1:excnt], ","))
end

```


```txt

Testing Multiplicative Digital Root.

  123321 has persistence 3 and digital root 8
    7739 has persistence 3 and digital root 8
     893 has persistence 3 and digital root 2
  899998 has persistence 2 and digital root 0

 MDR:    First 5 numbers having this MDR
   0:      0,    10,    20,    25,    30
   1:      1,    11,   111,  1111, 11111
   2:      2,    12,    21,    26,    34
   3:      3,    13,    31,   113,   131
   4:      4,    14,    22,    27,    39
   5:      5,    15,    35,    51,    53
   6:      6,    16,    23,    28,    32
   7:      7,    17,    71,   117,   171
   8:      8,    18,    24,    29,    36
   9:      9,    19,    33,    91,   119

```



## Kotlin

```scala
// version 1.1.2

fun multDigitalRoot(n: Int): Pair<Int, Int> = when {
        n < 0   -> throw IllegalArgumentException("Negative numbers not allowed")
        else    -> {
            var mdr: Int
            var mp = 0
            var nn = n
            do {
                mdr = if (nn > 0) 1 else 0
                while (nn > 0) {
                    mdr *= nn % 10
                    nn /= 10
                }
                mp++
                nn = mdr
            }
            while (mdr >= 10)
            Pair(mdr, mp)
        }
    }

fun main(args: Array<String>) {
    val ia = intArrayOf(123321, 7739, 893, 899998)
    for (i in ia) {
        val (mdr, mp) = multDigitalRoot(i)
        println("${i.toString().padEnd(9)} MDR = $mdr  MP = $mp")
    }
    println()
    println("MDR   n0    n1    n2    n3    n4")
    println("
###   ========================
")
    val ia2 = Array(10) { IntArray(6) } // all zero by default
    var n = 0
    var count = 0
    do {
        val (mdr, _) = multDigitalRoot(n)
        if (ia2[mdr][0] < 5) {
            ia2[mdr][0]++
            ia2[mdr][ia2[mdr][0]] = n
            count++
        }
        n++
    }
    while (count < 50)

    for (i in 0..9) {
        print("$i:")
        for (j in 1..5) print("%6d".format(ia2[i][j]))
        println()
    }
}
```


```txt

123321    MDR = 8  MP = 3
7739      MDR = 8  MP = 3
893       MDR = 2  MP = 3
899998    MDR = 0  MP = 2

MDR   n0    n1    n2    n3    n4

###   ========================

0:     0    10    20    25    30
1:     1    11   111  1111 11111
2:     2    12    21    26    34
3:     3    13    31   113   131
4:     4    14    22    27    39
5:     5    15    35    51    53
6:     6    16    23    28    32
7:     7    17    71   117   171
8:     8    18    24    29    36
9:     9    19    33    91   119

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica

ClearAll[mdr, mp, nums];
mdr[n_] := NestWhile[Times @@ IntegerDigits[#] &, n, # > 9 &];
mp[n_] := Length@NestWhileList[Times @@ IntegerDigits[#] &, n, # > 9 &] - 1;
TableForm[{#, mdr[#], mp[#]} & /@ {123321, 7739, 893, 899998},
  TableHeadings -> {None, {"Number", "MDR", "MP"}}]
nums = ConstantArray[{}, 10];
For[i = 0, Min[Length /@ nums] < 5, i++,  AppendTo[nums[[mdr[i] + 1]], i]];
TableForm[Table[{i, Take[nums[[i + 1]], 5]}, {i, 0, 9}],
  TableHeadings -> {None, {"MDR", "First 5"}}, TableDepth -> 2]

```


```txt

Number   MDR   MP
-----------------
123321   8     3
7739     8     3
893      2     3
899998   0     2

MDR   First 5
-----------------------------
0   {0, 10, 20, 25, 30}
1   {1, 11, 111, 1111, 11111}
2   {2, 12, 21, 26, 34}
3   {3, 13, 31, 113, 131}
4   {4, 14, 22, 27, 39}
5   {5, 15, 35, 51, 53}
6   {6, 16, 23, 28, 32}
7   {7, 17, 71, 117, 171}
8   {8, 18, 24, 29, 36}
9   {9, 19, 33, 91, 119}

```



## Nim

```nim
import strutils, future

template newSeqWith(len: int, init: expr): expr =
  var result {.gensym.} = newSeq[type(init)](len)
  for i in 0 .. <len:
    result[i] = init
  result

proc mdroot(n): tuple[mp, mdr: int] =
  var mdr = @[n]
  while mdr[mdr.high] > 9:
    var n = 1
    for dig in $mdr[mdr.high]:
      n *= parseInt($dig)
    mdr.add n
  (mdr.high, mdr[mdr.high])

for n in [123321, 7739, 893, 899998]:
  echo align($n, 6)," ",mdroot(n)
echo ""

var table = newSeqWith(10, newSeq[int]())
for n in 0..int.high:
  if table.map((x: seq[int]) => x.len).min >= 5: break
  table[mdroot(n).mdr].add n

for mp, val in table:
  echo mp,": ",val[0..4]
```

```txt
123321 (mp: 3, mdr: 8)
  7739 (mp: 3, mdr: 8)
   893 (mp: 3, mdr: 2)
899998 (mp: 2, mdr: 0)

0: @[0, 10, 20, 25, 30]
1: @[1, 11, 111, 1111, 11111]
2: @[2, 12, 21, 26, 34]
3: @[3, 13, 31, 113, 131]
4: @[4, 14, 22, 27, 39]
5: @[5, 15, 35, 51, 53]
6: @[6, 16, 23, 28, 32]
7: @[7, 17, 71, 117, 171]
8: @[8, 18, 24, 29, 36]
9: @[9, 19, 33, 91, 119]
```



## PARI/GP


```parigp
a(n)=my(i);while(n>9,n=factorback(digits(n));i++);[i,n];
apply(a, [123321, 7739, 893, 899998])
v=vector(10,i,[]); forstep(n=0,oo,1, t=a(n)[2]+1; if(#v[t]<5,v[t]=concat(v[t],n); if(vecmin(apply(length,v))>4, return(v))))
```

```txt
%1 = [[3, 8], [3, 8], [3, 2], [2, 0]]
%2 = [[0, 10, 20, 25, 30], [1, 11, 111, 1111, 11111], [2, 12, 21, 26, 34], [3, 13, 31, 113, 131], [4, 14, 22, 27, 39], [5, 15, 35, 51, 53], [6, 16, 23, 28, 32], [7, 17, 71, 117, 171], [8, 18, 24, 29, 36], [9, 19, 33, 91, 119]]
```



## Perl

```Perl
use warnings;
use strict;

sub mdr {
  my $n = shift;
  my($count, $mdr) = (0, $n);
  while ($mdr > 9) {
    my($m, $dm) = ($mdr, 1);
    while ($m) {
      $dm *= $m % 10;
      $m = int($m/10);
    }
    $mdr = $dm;
    $count++;
  }
  ($count, $mdr);
}

print "Number: (MP, MDR)\n
### ===  ======
\n";
foreach my $n (123321, 7739, 893, 899998) {
  printf "%6d: (%d, %d)\n", $n, mdr($n);
}
print "\nMP: [n0..n4]\n==
### ==
\n";
foreach my $target (0..9) {
  my $i = 0;
  my @n = map { $i++ while (mdr($i))[1] != $target; $i++; } 1..5;
  print " $target: [", join(", ", @n), "]\n";
}
```

```txt
Number: (MP, MDR)

### ===  ======

123321: (3, 8)
  7739: (3, 8)
   893: (3, 2)
899998: (2, 0)

MP: [n0..n4]
==
### ==

 0: [0, 10, 20, 25, 30]
 1: [1, 11, 111, 1111, 11111]
 2: [2, 12, 21, 26, 34]
 3: [3, 13, 31, 113, 131]
 4: [4, 14, 22, 27, 39]
 5: [5, 15, 35, 51, 53]
 6: [6, 16, 23, 28, 32]
 7: [7, 17, 71, 117, 171]
 8: [8, 18, 24, 29, 36]
 9: [9, 19, 33, 91, 119]
```



## Perl 6


```perl6
sub multiplicative-digital-root(Int $n) {
    return .elems - 1, .[.end]
        given cache($n, {[*] .comb} ... *.chars == 1)
}

for 123321, 7739, 893, 899998 {
    say "$_: ", .&multiplicative-digital-root;
}

for ^10 -> $d {
    say "$d : ", .[^5]
        given (1..*).grep: *.&multiplicative-digital-root[1] == $d;
}
```

```txt
123321: 3 8
7739: 3 8
893: 3 2
899998: 2 0
0 : 10 20 25 30 40
1 : 1 11 111 1111 11111
2 : 2 12 21 26 34
3 : 3 13 31 113 131
4 : 4 14 22 27 39
5 : 5 15 35 51 53
6 : 6 16 23 28 32
7 : 7 17 71 117 171
8 : 8 18 24 29 36
9 : 9 19 33 91 119
```



## Phix


```Phix
function mdr_mp(integer m)
integer mp = 0
    while m>9 do
        integer newm = 1
        while m do
            newm *= remainder(m,10)
            m = floor(m/10)
        end while
        m = newm
        mp += 1
    end while
    return {m,mp}
end function

constant tests = {123321, 7739, 893, 899998}
printf(1,"Number     MDR     MP\n")
printf(1,"
### ===
     ==\n")
for i=1 to length(tests) do
    integer ti = tests[i]
    printf(1,"%6d %6d %6d\n",ti&mdr_mp(ti))
end for

integer i=0, found = 0
sequence res = repeat({},10)
while found<50 do
    integer {mdr,mp} = mdr_mp(i)
    if length(res[mdr+1])<5 then
        res[mdr+1] &= i
        found += 1
    end if
    i += 1
end while

printf(1,"\nMDR    1     2     3     4     5")
printf(1,"\n
###   ========================
\n")

for i=1 to 10 do
    printf(1,"%2d %5d %5d %5d %5d %5d\n",prepend(res[i],i-1))
end for
```

```txt

Number     MDR     MP

### ===
     ==
123321      8      3
  7739      8      3
   893      2      3
899998      0      2

MDR    1     2     3     4     5

###   ========================

 0     0    10    20    25    30
 1     1    11   111  1111 11111
 2     2    12    21    26    34
 3     3    13    31   113   131
 4     4    14    22    27    39
 5     5    15    35    51    53
 6     6    16    23    28    32
 7     7    17    71   117   171
 8     8    18    24    29    36
 9     9    19    33    91   119

```



## PL/I


### version 1

```pli
multiple: procedure options (main);  /* 29 April 2014 */

   declare n fixed binary (31);

find_mdr: procedure;
   declare (mdr, mp, p) fixed binary (31);

   mdr = n;
   do mp = 1 by 1 until (p <= 9);
      p = 1;
      do until (mdr = 0); /* Form product of the digits in mdr. */
         p = mod(mdr, 10) * p;
         mdr= mdr/10;
      end;
      mdr = p;
   end;
   put skip data (n, mdr, mp);
end find_mdr;

   do n = 123321, 7739, 893, 899998;
      call find_mdr;
   end;

end multiple;
```

```txt
N=        123321        MDR=             8      MP=             3;
N=          7739        MDR=             8      MP=             3;
N=           893        MDR=             2      MP=             3;
N=        899998        MDR=             0      MP=             2;
```



### version 2


```pli
 mdrt: Proc Options(main);
 Dcl (x,p,r) Bin Fixed(31);
 Put Edit('number   persistence   multiplicative digital root')(Skip,a);
 Put Edit('-------  -----------   ---------------------------')(Skip,a);
 Call task1(123321);
 Call task1(  7739);
 Call task1(   893);
 Call task1(899998);

 task1: Procedure(x);
 Dcl x Bin Fixed(31);
 Call mdr(x,p,r);
 Put Edit(x,p,r)(Skip,f(8),f(8),f(22));
 End;

 Dcl zn(0:9) Bin Fixed(31);
 Dcl z(0:9,5) Bin Fixed(31);
 zn=0;
 zn(0)=1;
 z(0,1)=0;
 Do x=1 To 11111;
   Call mdr(x,p,r);
   If zn(r)<5 Then Do;
     zn(r)+=1;
     z(r,zn(r))=x;
     End;
   End;
 Put Edit(' ')(Skip,a);
 Put Edit('MDR  first 5 numbers that have a matching MDR')(Skip,a);
 Put Edit('---  ----------------------------------------')(Skip,a);

 Do r=0 To 9;
   Put Edit(r,'  ')(Skip,f(3),a);
   Do i=1 To 5;
     Put Edit(z(r,i))(f(6));
     End;
   End;

 mdr: Procedure(y,p,r);
 Dcl (y,p,r) Bin Fixed(31);
 Dcl (k,yy) Bin Fixed(31);
 Dcl pic Pic'(10)9';
 Dcl d   Pic'9';
 pic=abs(y);
 Do p=1 By 1 Until(pic<10);
   Do k=1 To 10 Until(substr(pic,k,1)>'0');
     End;
   r=1;
   Do k=k To 10;
     d=substr(pic,k,1);
     r=r*d;
     End;
   pic=r;
   End;
 End;
 End;
```

```txt
number   persistence   multiplicative digital root
-------  -----------   ---------------------------
  123321       3                     8
    7739       3                     8
     893       3                     2
  899998       2                     0

MDR  first 5 numbers that have a matching MDR
---  ----------------------------------------
  0       0    10    20    25    30
  1       1    11   111  1111 11111
  2       2    12    21    26    34
  3       3    13    31   113   131
  4       4    14    22    27    39
  5       5    15    35    51    53
  6       6    16    23    28    32
  7       7    17    71   117   171
  8       8    18    24    29    36
  9       9    19    33    91   119
```



## Python

===Python: Inspired by the solution to the [[Digital root#Python|Digital root]] task===

```python
try:
    from functools import reduce
except:
    pass

def mdroot(n):
    'Multiplicative digital root'
    mdr = [n]
    while mdr[-1] > 9:
        mdr.append(reduce(int.__mul__, (int(dig) for dig in str(mdr[-1])), 1))
    return len(mdr) - 1, mdr[-1]

if __name__ == '__main__':
    print('Number: (MP, MDR)\n
### ===  ======
')
    for n in (123321, 7739, 893, 899998):
        print('%6i: %r' % (n, mdroot(n)))

    table, n = {i: [] for i in range(10)}, 0
    while min(len(row) for row in table.values()) < 5:
        mpersistence, mdr = mdroot(n)
        table[mdr].append(n)
        n += 1
    print('\nMP: [n0..n4]\n==
### ==
')
    for mp, val in sorted(table.items()):
        print('%2i: %r' % (mp, val[:5]))
```


```txt
Number: (MP, MDR)

### ===  ======

123321: (3, 8)
  7739: (3, 8)
   893: (3, 2)
899998: (2, 0)

MP: [n0..n4]
==
### ==

 0: [0, 10, 20, 25, 30]
 1: [1, 11, 111, 1111, 11111]
 2: [2, 12, 21, 26, 34]
 3: [3, 13, 31, 113, 131]
 4: [4, 14, 22, 27, 39]
 5: [5, 15, 35, 51, 53]
 6: [6, 16, 23, 28, 32]
 7: [7, 17, 71, 117, 171]
 8: [8, 18, 24, 29, 36]
 9: [9, 19, 33, 91, 119]
```


===Python: Inspired by the [[Digital_root/Multiplicative_digital_root#More_Efficient_Version|more efficient version of D]].===
Substitute the following function to run twice as fast when calculating  mdroot(n) with n in range(1000000).

```python
def mdroot(n):
    count, mdr = 0, n
    while mdr > 9:
        m, digitsMul = mdr, 1
        while m:
            m, md = divmod(m, 10)
            digitsMul *= md
        mdr = digitsMul
        count += 1
    return count, mdr
```


(Exactly the same as before).


## Racket


```racket
#lang racket
(define (digital-product n)
  (define (inr-d-p m rv)
    (cond
      [(zero? m) rv]
      [else (define-values (q r) (quotient/remainder m 10))
            (if (zero? r) 0 (inr-d-p q (* rv r)))])) ; lazy on zero
  (inr-d-p n 1))

(define (mdr/mp n)
  (define (inr-mdr/mp m i)
    (if (< m 10) (values m i) (inr-mdr/mp (digital-product m) (add1 i))))
  (inr-mdr/mp n 0))

(printf "Number\tMDR\tmp~%
### ===\t
\t==~%")
(for ((n (in-list '(123321 7739 893 899998))))
  (define-values (mdr mp) (mdr/mp n))
  (printf "~a\t~a\t~a~%" n mdr mp))

(printf "~%MDR\t[n0..n4]~%
### \t=====
~%")
(for ((MDR (in-range 10)))
  (define (has-mdr? n) (define-values (mdr mp) (mdr/mp n)) (= mdr MDR))
  (printf "~a\t~a~%" MDR (for/list ((_ 5) (n (sequence-filter has-mdr? (in-naturals)))) n)))
```

```txt
Number	MDR	mp
======	===	==
123321	8	3
7739	8	3
893	2	3
899998	0	2

MDR	[n0..n4]
===
### ==

0	(0 10 20 25 30)
1	(1 11 111 1111 11111)
2	(2 12 21 26 34)
3	(3 13 31 113 131)
4	(4 14 22 27 39)
5	(5 15 35 51 53)
6	(6 16 23 28 32)
7	(7 17 71 117 171)
8	(8 18 24 29 36)
9	(9 19 33 91 119)
```



## REXX


### idomatic version


```rexx
/*REXX program finds the  persistence and multiplicative  digital root  of some numbers.*/
numeric digits 100                               /*increase the number of decimal digits*/
parse arg x                                      /*obtain optional arguments from the CL*/
if x='' | x=","  then x=123321 7739 893 899998   /*Not specified?  Then use the default.*/
say center('number', 8)      ' persistence   multiplicative digital root'
say copies('─'     , 8)      ' ───────────   ───────────────────────────'
                                                 /* [↑]  the title  and  separator.     */
     do j=1  for words(x);        n=word(x, j)   /*process each number in the   X  list.*/
     parse value  MDR(n)   with   mp mdr         /*obtain the persistence and the  MDR. */
     say right(n,8) center(mp,13) center(mdr,30) /*display a number,  persistence,  MDR.*/
     end   /*j*/                                 /* [↑]  show MP & MDR for each number. */
say;                     target=5
say 'MDR        first '  target  " numbers that have a matching MDR"
say '═══   ═══════════════════════════════════════════════════'

     do k=0  for 10; hits=0;   _=                /*show numbers that have an MDR of  K. */
       do m=k  until hits==target                /*find target numbers with an MDR of K.*/
       if word( MDR(m), 2)\==k  then iterate     /*is this the  MDR  that's wanted?     */
       hits=hits + 1;       _=space(_ m',')      /*yes, we got a hit,  add to the list. */
       end   /*m*/                               /* [↑]  built a list of MDRs that = K. */
     say " "k':     ['strip(_, , ',')"]"         /*display the  K  (MDR)  and the list. */
     end     /*k*/                               /* [↑]  done with the   K   MDR list.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
MDR: procedure; parse arg y; y=abs(y)            /*get the number and determine the MDR.*/
                   do p=1      until  y<10;    parse var  y  r  2
                       do k=2  to length(y);   r=r * substr(y, k, 1)
                       end   /*k*/
                   y=r
                   end       /*p*/               /* [↑]  wash, rinse, and repeat ···    */
                return p r                       /*return the persistence and the  MDR. */
```

```txt

 number   persistence   multiplicative digital root
────────  ───────────   ───────────────────────────
  123321       3                     8
    7739       3                     8
     893       3                     2
  899998       2                     0

MDR        first  5  numbers that have a matching MDR
═══   ═══════════════════════════════════════════════════
 0:     [0, 10, 20, 25, 30]
 1:     [1, 11, 111, 1111, 11111]
 2:     [2, 12, 21, 26, 34]
 3:     [3, 13, 31, 113, 131]
 4:     [4, 14, 22, 27, 39]
 5:     [5, 15, 35, 51, 53]
 6:     [6, 16, 23, 28, 32]
 7:     [7, 17, 71, 117, 171]
 8:     [8, 18, 24, 29, 36]
 9:     [9, 19, 33, 91, 119]

```


===ultra-fast version===
This fast version can handle a target of five hundred numbers with ease for the 2<sup>nd</sup> part of the task's requirement.

```rexx
/*REXX program finds the  persistence and multiplicative  digital root  of some numbers.*/
numeric digits 2000                              /*increase the number of decimal digits*/
parse arg target x                               /*obtain optional arguments from the CL*/
if \datatype(target, 'W')  then target=25        /*Not specified?  Then use the default.*/
if x='' | x=","  then x=123321 7739 893 899998   /* "      "         "   "   "      "   */
say center('number',8)  ' persistence   multiplicative digital root'
say copies('─'     ,8)  ' ───────────   ───────────────────────────'
                                                 /* [↑]  the title  and  the separator. */
     do j=1  for words(x);  n=abs( word(x, j) )  /*process each number in the list.     */
     parse value  MDR(n)   with   mp mdr         /*obtain the persistence and the MDR.  */
     say right(n,8) center(mp,13) center(mdr,30) /*display the number, persistence, MDR.*/
     end   /*j*/                                 /* [↑] show MP and MDR for each number.*/
say                                              /* [↓] show a blank and the title line.*/
say 'MDR       first '  target  " numbers that have a matching MDR"
say '═══  ' copies("═",(target+(target+1)**2)%2) /*display a separator line (for title).*/

    do k=0  for 9;              hits=0           /*show numbers that have an MDR of  K. */
    _=
    if k==7  then _=@7                           /*handle the special case of  seven.   */
             else do m=k  until hits==target     /*find target numbers with an MDR of K.*/
                  parse var  m  ''  -1  ?        /*obtain the right─most digit of  M.   */
                  if k\==0  then if ?==0           then iterate
                  if k==5   then if ?//2==0        then iterate
                  if k==1   then m=copies(1, hits+1)
                            else if MDR(m, 1)\==k  then iterate
                  hits=hits+1                               /*got a hit, add to the list*/
                  _=space(_ m)                              /*elide superfluous blanks. */
                  if k==3  then do;      o=strip(m, 'T', 1) /*strip trailing ones from M*/
                                if o==3  then m=copies(1, length(m))3   /*make a new  M.*/
                                         else do;   t=pos(3, m) - 1     /*position of 3 */
                                              m=overlay(3, translate(m, 1, 3), t)
                                              end     /* [↑] shift the "3" 1 place left.*/
                                m=m - 1               /*adjust for  DO  index increment.*/
                                end                   /* [↑]  a shortcut to adj DO index*/
                  end   /*m*/                         /* [↑]  built a list of MDRs = K  */

    say " "k':    ['_"]"                         /*display the  K  (MDR)  and the list. */
    if k==3  then @7=translate(_, 7, k)          /*save for later, a special  "7"  case.*/
    end   /*k*/                                  /* [↑]  done with the  K  MDR list.    */

@.=                                              /* [↓]  handle MDR of  "9"  special.   */
_=translate(@7, 9, 7)                            /*translate string for MDR  of nine.   */
@9=translate(_, , ',')                           /*remove trailing commas from numbers. */
@3=                                              /*assign null string before building.  */

   do j=1  for words(@9)                         /*process each number for  MDR 9  case.*/
   _=space( translate( word(@9, j), , 9),  0)    /*elide all  "9"s   using   SPACE(x,0).*/
   L=length(_) + 1                               /*use a "fudged" length of the number. */
   new=                                          /*these are the new numbers  (so far). */

        do k=0 for L;       q=insert(3, _, k)    /*insert the  1st  "3"  into the number*/
          do i=k  to L;     z=insert(3, q, i)    /*   "    "   2nd  "3"    "   "     "  */
          if @.z\==''  then iterate              /*if already define, ignore the number.*/
          @.z=z;            new=z new            /*define it,  and then add to the list.*/
          end   /*i*/                            /* [↑]  end of  2nd  insertion of  "3".*/
        end     /*k*/                            /* [↑]   "  "   1st      "      "   "  */

   @3=space(@3 new)                              /*remove blanks,  then add to the list.*/
   end          /*j*/                            /* [↑]  end of insertion of the  "3"s. */

a1=@9;  a2=@3                                    /*define some strings for the merge.   */
@=                                               /* [↓]  merge two lists,  3s  and  9s. */
      do  while  a1\==''  &  a2\==''             /*process while the lists aren't empty.*/
      x=word(a1, 1);   y=word(a2, 1)             /*obtain the 1st word in A1 & A2 lists.*/
      if x=='' | y=='' then leave                /*are   X   or   Y   empty?            */
      if x<y  then do;   @=@ x;   a1=delword(a1, 1, 1);   end   /*add  X  to the @ list.*/
              else do;   @=@ y;   a2=delword(a2, 1, 1);   end   /* "   Y   "  "  "   "  */
      end   /*while*/                            /* [↑]  only process just enough nums. */

@=subword(@, 1, target)                          /*elide the last trailing comma in list*/
say " "9':     ['@"]"                            /*display the  "9" (MDR)  and the list.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
MDR: procedure; parse arg y,s;        y=abs(y)   /*get the number and determine the MDR.*/
                   do p=1      until  y<10;    parse var  y  r  2
                       do k=2  to length(y);   r=r * substr(y, k, 1)
                       end   /*k*/
                   y=r
                   end       /*p*/               /* [↑]  wash, rinse, and repeat ···    */
                if s==1  then return r           /*return multiplicative digital root.  */
                return p r                       /*return the persistence and the  MDR. */
```

```txt

 number   persistence   multiplicative digital root
────────  ───────────   ───────────────────────────
  123321       3                     8
    7739       3                     8
     893       3                     2
  899998       2                     0

MDR       first  34  numbers that have a matching MDR
═══   ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
 0:     [0 10 20 25 30 40 45 50 52 54 55 56 58 59 60 65 69 70 78 80 85 87 90 95 96 100 101 102 103 104 105 106 107 108]
 1:     [1 11 111 1111 11111 111111 1111111 11111111 111111111 1111111111 11111111111 111111111111 1111111111111 11111111111111 111111111111111 1111111111111111 11111111111111111 111111111111111111 1111111111111111111 11111111111111111111 111111111111111111111 1111111111111111111111 11111111111111111111111 111111111111111111111111 1111111111111111111111111 11111111111111111111111111 111111111111111111111111111 1111111111111111111111111111 11111111111111111111111111111 111111111111111111111111111111 1111111111111111111111111111111 11111111111111111111111111111111 111111111111111111111111111111111 1111111111111111111111111111111111]
 2:     [2 12 21 26 34 37 43 62 73 112 121 126 134 137 143 162 173 211 216 223 232 261 278 279 287 297 299 314 317 322 341 367 369 371]
 3:     [3 13 31 113 131 311 1113 1131 1311 3111 11113 11131 11311 13111 31111 111113 111131 111311 113111 131111 311111 1111113 1111131 1111311 1113111 1131111 1311111 3111111 11111113 11111131 11111311 11113111 11131111 11311111]
 4:     [4 14 22 27 39 41 72 89 93 98 114 122 127 139 141 172 189 193 198 212 217 221 249 266 271 277 294 319 333 338 346 364 379 383]
 5:     [5 15 35 51 53 57 75 115 135 151 153 157 175 315 351 355 359 395 511 513 517 531 535 539 553 557 571 575 579 593 597 715 751 755]
 6:     [6 16 23 28 32 44 47 48 61 68 74 82 84 86 116 123 128 132 144 147 148 161 168 174 182 184 186 213 218 224 227 228 231 238]
 7:     [7 17 71 117 171 711 1117 1171 1711 7111 11117 11171 11711 17111 71111 111117 111171 111711 117111 171111 711111 1111117 1111171 1111711 1117111 1171111 1711111 7111111 11111117 11111171 11111711 11117111 11171111 11711111]
 8:     [8 18 24 29 36 38 42 46 49 63 64 66 67 76 77 79 81 83 88 92 94 97 99 118 124 129 136 138 142 146 149 163 164 166]
 9:     [9 19 33 91 119 133 191 313 331 911 1119 1133 1191 1313 1331 1911 3113 3131 3311 9111 11119 11133 11191 11313 11331 11911 13113 13131 13311 19111 31113 31131 31311 33111]

```



## Ring


```ring

# Project : Digital root/Multiplicative digital root

load "stdlib.ring"
root = newlist(10, 5)
for r = 1 to 10
     for x = 1 to 5
          root[r][x] = 0
     next
next
root2 = list(10)
for y = 1 to 10
     root2[y] = 0
next
see "Number  MDR   MP" + nl
num = [123321, 7739, 893, 899998]
digroot(num)
see nl
num = 0:12000
digroot(num)
see "First five numbers with MDR in first column:" + nl
for n1 = 1 to 10
     see "" + (n1-1) + " => "
     for n2 = 1 to 5
         see "" + root[n1][n2] + "  "
     next
     see nl
next

func digroot(num)
        for n = 1 to len(num)
            sum = 0
            numold = num[n]
            while true
                    pro = 1
                    strnum = string(numold)
                    for nr = 1 to len(strnum)
                        pro = pro * number(strnum[nr])
                    next
                    sum = sum + 1
                    numold = pro
                    numn = string(num[n])
                    sp = 6 - len(string(num[n]))
                    if sp > 0
                       for p = 1 to sp + 2
                           numn = " " + numn
                       next
                    ok
                    if len(string(numold)) = 1 and len(num) < 5
                       see "" + numn + "     " + numold + "       " + sum + nl
                       exit
                    ok
                    if len(string(numold)) = 1 and len(num) > 4
                       root2[numold+1] = root2[numold+1] + 1
                       if root2[numold+1] < 6
                          root[numold+1][root2[numold+1]] = num[n]
                       ok
                       exit
                    ok
              end
        next

```

Output:

```txt

Number  MDR   MP
123321   8     3
  7739     8     3
   893      2     3
899998   0     2

First five numbers with MDR in first column:
0 => 0  10  20  25  30
1 => 1  11  111  1111  11111
2 => 2  12  21  26  34
3 => 3  13  31  113  131
4 => 4  14  22  27  39
5 => 5  15  35  51  53
6 => 6  16  23  28  32
7 => 7  17  71  117  171
8 => 8  18  24  29  36
9 => 9  19  33  91  119

```



## Ruby

```ruby
def mdroot(n)
  mdr, persist = n, 0
  until mdr < 10 do
    mdr = mdr.to_s.each_char.map(&:to_i).inject(:*)
    persist += 1
  end
  [mdr, persist]
end

puts "Number: MDR  MP", "
### ===
  =="
[123321, 7739, 893, 899998].each{|n| puts "%6d:   %d  %2d" % [n, *mdroot(n)]}

counter = Hash.new{|h,k| h[k]=[]}
0.step do |i|
  counter[mdroot(i).first] << i
  break if counter.values.all?{|v| v.size >= 5 }
end
puts "", "MDR: [n0..n4]", "
###   =====
"
10.times{|i| puts "%3d: %p" % [i, counter[i].first(5)]}
```

```txt

Number: MDR  MP

### ===
  ==
123321:   8   3
  7739:   8   3
   893:   2   3
899998:   0   2

MDR: [n0..n4]

###   =====

  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]

```



## Scala

```Scala
import Stream._

object MDR extends App {

  def mdr(x: BigInt, base: Int = 10): (BigInt, Long) = {
    def multiplyDigits(x: BigInt): BigInt = ((x.toString(base) map (_.asDigit)) :\ BigInt(1))(_*_)
    def loop(p: BigInt, c: Long): (BigInt, Long) = if (p < base) (p, c) else loop(multiplyDigits(p), c+1)
    loop(multiplyDigits(x), 1)
  }

  printf("%15s\t%10s\t%s\n","Number","MDR","MP")
  printf("%15s\t%10s\t%s\n","======","===","==")
  Seq[BigInt](123321, 7739, 893, 899998, BigInt("393900588225"), BigInt("999999999999")) foreach {x =>
    val (s, c) = mdr(x)
    printf("%15s\t%10s\t%2s\n",x,s,c)
  }
  println

  val mdrs: Stream[Int] => Stream[(Int, BigInt)] = i => i map (x => (x, mdr(x)._1))

  println("MDR: [n0..n4]")
  println("
### = =====
")
  ((for {i <- 0 to 9} yield (mdrs(from(0)) take 11112 toList) filter {_._2 == i})
    .map {_ take 5} map {xs => xs map {_._1}}).zipWithIndex
    .foreach{p => printf("%3s: [%s]\n",p._2,p._1.mkString(", "))}

}
```


```txt

         Number        MDR      MP

### ===
      ==
         123321          8       3
           7739          8       3
            893          2       3
         899998          0       2
   393900588225          0       1
   999999999999          0       3

MDR: [n0..n4]

### = =====

  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]
```



## Sidef

```ruby
func mdroot(n) {
  var (mdr, persist) = (n, 0)
  while (mdr >= 10) {
    mdr = mdr.digits.prod
    ++persist
  }
  [mdr, persist]
}

say "Number: MDR  MP\n
### ===
  =="
[123321, 7739, 893, 899998].each{|n| "%6d: %3d %3d\n" \
                           .printf(n, mdroot(n)...) }

var counter = Hash()

Inf.times { |j|
  counter{mdroot(j).first} := [] << j
  break if counter.values.all {|v| v.len >= 5 }
}
 
say "\nMDR: [n0..n4]\n
###   =====
"
10.times {|i| "%3d: %s\n".printf(i, counter{i}.first(5)) }
```


```txt

Number: MDR  MP

### ===
  ==
123321:   8   3
  7739:   8   3
   893:   2   3
899998:   0   2

MDR: [n0..n4]

###   =====

  0: [0, 10, 20, 25, 30]
  1: [1, 11, 111, 1111, 11111]
  2: [2, 12, 21, 26, 34]
  3: [3, 13, 31, 113, 131]
  4: [4, 14, 22, 27, 39]
  5: [5, 15, 35, 51, 53]
  6: [6, 16, 23, 28, 32]
  7: [7, 17, 71, 117, 171]
  8: [8, 18, 24, 29, 36]
  9: [9, 19, 33, 91, 119]

```



## Tcl


```tcl
proc mdr {n} {
    if {$n < 0 || ![string is integer $n]} {
	error "must be an integer"
    }
    for {set i 0} {$n > 9} {incr i} {
	set n [tcl::mathop::* {*}[split $n ""]]
    }
    return [list $i $n]
}
```

Demonstrating:

```tcl
puts "Number: MP MDR"
puts [regsub -all . "Number: MP MDR" -]
foreach n {123321 7739 893 899998} {
    puts [format "%6d: %2d %3d" $n {*}[mdr $n]]
}
puts ""
# The longEnough variable counts how many roots have at least 5 values accumulated for them
for {set i [set longEnough 0]} {$longEnough < 10} {incr i} {
    set root [lindex [mdr $i] 1]
    if {[llength [lappend accum($root) $i]] == 5} {incr longEnough}
}
puts "MDR: \[n\u2080\u2026n\u2084\]"
puts [regsub -all . "MDR: \[n\u2080\u2026n\u2084\]" -]
for {set i 0} {$i < 10} {incr i} {
    puts [format "%3d: (%s)" $i [join [lrange $accum($i) 0 4] ", "]]
}
```

```txt

Number: MP MDR
--------------
123321:  3   8
  7739:  3   8
   893:  3   2
899998:  2   0

MDR: [n₀…n₄]
------------
  0: (0, 10, 20, 25, 30)
  1: (1, 11, 111, 1111, 11111)
  2: (2, 12, 21, 26, 34)
  3: (3, 13, 31, 113, 131)
  4: (4, 14, 22, 27, 39)
  5: (5, 15, 35, 51, 53)
  6: (6, 16, 23, 28, 32)
  7: (7, 17, 71, 117, 171)
  8: (8, 18, 24, 29, 36)
  9: (9, 19, 33, 91, 119)

```



## zkl

```zkl
fcn mdroot(n){ // Multiplicative digital root
   mdr := List(n);
   while (mdr[-1] > 9){
      mdr.append(mdr[-1].split().reduce('*,1));
   }
   return(mdr.len() - 1, mdr[-1]);
}
```


```zkl
fcn mdroot(n){
   count:=0; mdr:=n;
   while(mdr > 9){
      m:=mdr; digitsMul:=1;
      while(m){
	 reg md;
	 m,md=m.divr(10);
	 digitsMul *= md;
      }
      mdr = digitsMul;
      count += 1;
   }
   return(count, mdr);
}
```


```zkl
println("Number:  (MP, MDR)\n
### ====  ======
");
foreach n in (T(123321, 7739, 893, 899998))
  { println("%7,d: %s".fmt(n, mdroot(n))) }

table:=D([0..9].zip(fcn{List()}).walk());  // dictionary(0:List, 1:List, ...)
n    :=0;
while(table.values.filter(fcn(r){r.len()<5})){ // until each entry has >=5 values
   mpersistence, mdr := mdroot(n);
   table[mdr].append(n);
   n += 1;
}
println("\nMP: [n0..n4]\n==
### ==
");
foreach mp in (table.keys.sort()){
   println("%2d: %s".fmt(mp, table[mp][0,5])); //print first five values
}
```

```txt

Number:  (MP, MDR)

### ====  ======

123,321: L(3,8)
  7,739: L(3,8)
    893: L(3,2)
899,998: L(2,0)

MP: [n0..n4]
==
### ==

 0: L(0,10,20,25,30)
 1: L(1,11,111,1111,11111)
 2: L(2,12,21,26,34)
 3: L(3,13,31,113,131)
 4: L(4,14,22,27,39)
 5: L(5,15,35,51,53)
 6: L(6,16,23,28,32)
 7: L(7,17,71,117,171)
 8: L(8,18,24,29,36)
 9: L(9,19,33,91,119)

```


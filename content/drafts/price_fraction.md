+++
title = "Price fraction"
description = ""
date = 2019-07-22T14:21:45Z
aliases = []
[extra]
id = 6443
[taxonomies]
categories = []
tags = []
+++

{{task|Financial operations}}

A friend of mine runs a pharmacy.   He has a specialized function in his Dispensary application which receives a decimal value of currency and replaces it to a standard value.   This value is regulated by a government department.


;Task:
Given a floating point value between   0.00   and   1.00,   rescale according to the following table:

 >=  0.00  <  0.06  :=  0.10
 >=  0.06  <  0.11  :=  0.18
 >=  0.11  <  0.16  :=  0.26
 >=  0.16  <  0.21  :=  0.32
 >=  0.21  <  0.26  :=  0.38
 >=  0.26  <  0.31  :=  0.44
 >=  0.31  <  0.36  :=  0.50
 >=  0.36  <  0.41  :=  0.54
 >=  0.41  <  0.46  :=  0.58
 >=  0.46  <  0.51  :=  0.62
 >=  0.51  <  0.56  :=  0.66
 >=  0.56  <  0.61  :=  0.70
 >=  0.61  <  0.66  :=  0.74
 >=  0.66  <  0.71  :=  0.78
 >=  0.71  <  0.76  :=  0.82
 >=  0.76  <  0.81  :=  0.86
 >=  0.81  <  0.86  :=  0.90
 >=  0.86  <  0.91  :=  0.94
 >=  0.91  <  0.96  :=  0.98
 >=  0.96  <  1.01  :=  1.00





## Ada


```Ada

type Price is delta 0.01 digits 3 range 0.0..1.0;
function Scale (Value : Price) return Price is
   X : constant array (1..19) of Price :=
          (  0.06, 0.11, 0.16, 0.21, 0.26,  0.31, 0.36, 0.41, 0.46, 0.51,
             0.56, 0.61, 0.66, 0.71, 0.76,  0.81, 0.86, 0.91, 0.96
          );
   Y : constant array (1..20) of Price :=
          (  0.10, 0.18, 0.26, 0.32, 0.38,  0.44, 0.50, 0.54, 0.58, 0.62,
             0.66, 0.70, 0.74, 0.78, 0.82,  0.86, 0.90, 0.94, 0.98, 1.0
          );
   Low    : Natural := X'First;
   High   : Natural := X'Last;
   Middle : Natural;
begin
   loop
      Middle := (Low + High) / 2;
      if Value = X (Middle) then
         return Y (Middle + 1);
      elsif Value < X (Middle) then
         if Low = Middle then
            return Y (Low);
         end if;
         High := Middle - 1;
      else
         if High = Middle then
            return Y (High + 1);
         end if;
         Low := Middle + 1;
      end if;
   end loop;
end Scale;

```

The solution uses fixed point type to prevent rounding and representation issues. With the above declarations a full coverage test:

```Ada

with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Price_Fraction is
   -- Put the declarations here
   Value : Price := Price'First;
begin
   loop
      Put_Line (Price'Image (Value) & "->" & Price'Image (Scale (Value)));
      exit when Value = Price'Last;
      Value := Price'Succ (Value);
   end loop;
end Test_Price_Fraction;

```

{{out}}
<div style="height: 200px;overflow:scroll">

```txt

 0.00-> 0.10
 0.01-> 0.10
 0.02-> 0.10
 0.03-> 0.10
 0.04-> 0.10
 0.05-> 0.10
 0.06-> 0.18
 0.07-> 0.18
 0.08-> 0.18
 0.09-> 0.18
 0.10-> 0.18
 0.11-> 0.26
 0.12-> 0.26
 0.13-> 0.26
 0.14-> 0.26
 0.15-> 0.26
 0.16-> 0.32
 0.17-> 0.32
 0.18-> 0.32
 0.19-> 0.32
 0.20-> 0.32
 0.21-> 0.38
 0.22-> 0.38
 0.23-> 0.38
 0.24-> 0.38
 0.25-> 0.38
 0.26-> 0.44
 0.27-> 0.44
 0.28-> 0.44
 0.29-> 0.44
 0.30-> 0.44
 0.31-> 0.50
 0.32-> 0.50
 0.33-> 0.50
 0.34-> 0.50
 0.35-> 0.50
 0.36-> 0.54
 0.37-> 0.54
 0.38-> 0.54
 0.39-> 0.54
 0.40-> 0.54
 0.41-> 0.58
 0.42-> 0.58
 0.43-> 0.58
 0.44-> 0.58
 0.45-> 0.58
 0.46-> 0.62
 0.47-> 0.62
 0.48-> 0.62
 0.49-> 0.62
 0.50-> 0.62
 0.51-> 0.66
 0.52-> 0.66
 0.53-> 0.66
 0.54-> 0.66
 0.55-> 0.66
 0.56-> 0.70
 0.57-> 0.70
 0.58-> 0.70
 0.59-> 0.70
 0.60-> 0.70
 0.61-> 0.74
 0.62-> 0.74
 0.63-> 0.74
 0.64-> 0.74
 0.65-> 0.74
 0.66-> 0.78
 0.67-> 0.78
 0.68-> 0.78
 0.69-> 0.78
 0.70-> 0.78
 0.71-> 0.82
 0.72-> 0.82
 0.73-> 0.82
 0.74-> 0.82
 0.75-> 0.82
 0.76-> 0.86
 0.77-> 0.86
 0.78-> 0.86
 0.79-> 0.86
 0.80-> 0.86
 0.81-> 0.90
 0.82-> 0.90
 0.83-> 0.90
 0.84-> 0.90
 0.85-> 0.90
 0.86-> 0.94
 0.87-> 0.94
 0.88-> 0.94
 0.89-> 0.94
 0.90-> 0.94
 0.91-> 0.98
 0.92-> 0.98
 0.93-> 0.98
 0.94-> 0.98
 0.95-> 0.98
 0.96-> 1.00
 0.97-> 1.00
 0.98-> 1.00
 0.99-> 1.00
 1.00-> 1.00

```

</div>


## AutoHotkey


```AutoHotkey
; Submitted by MasterFocus --- http://tiny.cc/iTunis

Loop
{
  InputBox, OutputVar, Price Fraction Example, Insert the value to be rounded.`n* [ 0 < value < 1 ]`n* Press ESC or Cancel to exit, , 200, 150
  If ErrorLevel
    Break
  MsgBox % "Input: " OutputVar "`nResult: " PriceFraction( OutputVar )
}

;-----------------------------------------

PriceFraction( p_Input )
{

  If p_Input is not float ; returns 0 if input is not a float
    Return 0

  If ( ( p_Input <= 0 ) OR ( p_Input >= 1 ) ) ; returns 0 is input is out of range
    Return 0

  ; declaring the table (arbitrary delimiters in use are '§' and '|')
  l_List := "0.06|0.10§0.11|0.18§0.16|0.26§0.21|0.32§0.26|0.38§0.31|0.44§0.36|0.50§0.41|0.54§0.46|0.58§0.51|0.62§0.56|0.66§0.61|0.70§0.66|0.74§0.71|0.78§0.76|0.82§0.81|0.86§0.86|0.90§0.91|0.94§0.96|0.98§1.01|1.00"

  Loop, Parse, l_List, § ; retrieves each field (delimited by '§')
  {
    StringSplit, l_Array, A_LoopField, | ; splits current field (using delimiter '|')
    If ( p_Input <= l_Array1 )
      Return l_Array2 ; returns the second value if input <= first value
  }

  Return 0 ; returns 0, indicating failure (shouldn't be reached though)

}
```



## ALGOL 68

{{trans|C}} - note: This specimen retains the original [[Price Fraction#C|C]] coding style.

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - specimen requires formatted transput}}

```algol68
main:
(
    # Just get a random price between 0 and 1 #
    # srand(time(NIL)); #
    REAL price := random;
    REAL tops := 0.06;
    REAL std val := 0.10;

    # Conditionals are a little odd here "(price-0.001 < tops AND
    price+0.001 > tops)" is to check if they are equal. Stupid
    C floats, right?   :) #
    WHILE ( price>tops OR (price-0.001 < tops AND price+0.001 > tops) ) AND tops<=1.01
    DO
        tops+:=0.05;

        IF std val < 0.26 THEN
                std val +:= 0.08
        ELIF std val < 0.50 THEN
                std val +:= 0.06
        ELSE
                std val +:= 0.04
        FI;

        IF std val > 0.98 THEN
                std val := 1.0
        FI
    OD;

    printf(($"Value :   "z.2dl,"Converted to standard :   "z.2dl$, price, std val))
)
```

{{out}}

```txt

Value :   0.38
Converted to standard :   0.54

```



## AWK


```AWK

BEGIN {
    O = ".06 .11 .16 .21 .26 .31 .36 .41 .46 .51 .56 .61 .66 .71 .76 .81 .86 .91 .96 1.01"
    N = ".10 .18 .26 .32 .38 .44 .50 .54 .58 .62 .66 .70 .74 .78 .82 .86 .90 .94 .98 1.00"
    fields = split(O,Oarr," ") # original values
    split(N,Narr," ") # replacement values
    for (i=-.01; i<=1.02; i+=.01) { # test
      printf("%5.2f = %4.2f\n",i,lookup(i))
    }
}
function lookup(n,  i) {
    if (n < 0 || n > 1.01) {
      return(0) # when input is out of range
    }
    for (i=1; i<=fields; i++) {
      # +10 is used because .11 returned .18 instead of .26
      # under AWK95, GAWK, and MAWK; Thompson Automation's TAWK worked correctly
      if (n+10 < Oarr[i]+10) {
        return(Narr[i])
      }
    }
}

```



## BASIC


{{works with|QBasic}}

This could also be done by building an array, but I felt that this was simpler.


```qbasic
DECLARE FUNCTION PriceFraction! (price AS SINGLE)

RANDOMIZE TIMER
DIM x AS SINGLE
x = RND
PRINT x, PriceFraction(x)

FUNCTION PriceFraction! (price AS SINGLE)
    'returns price unchanged if invalid value
    SELECT CASE price
        CASE IS < 0!
            PriceFraction! = price
        CASE IS < .06
            PriceFraction! = .1
        CASE IS < .11
            PriceFraction! = .18
        CASE IS < .16
            PriceFraction! = .26
        CASE IS < .21
            PriceFraction! = .32
        CASE IS < .26
            PriceFraction! = .38
        CASE IS < .31
            PriceFraction! = .44
        CASE IS < .36
            PriceFraction! = .5
        CASE IS < .41
            PriceFraction! = .54
        CASE IS < .46
            PriceFraction! = .58
        CASE IS < .51
            PriceFraction! = .62
        CASE IS < .56
            PriceFraction! = .66
        CASE IS < .61
            PriceFraction! = .7
        CASE IS < .66
            PriceFraction! = .74
        CASE IS < .71
            PriceFraction! = .78
        CASE IS < .76
            PriceFraction! = .82
        CASE IS < .81
            PriceFraction! = .86
        CASE IS < .86
            PriceFraction! = .9
        CASE IS < .91
            PriceFraction! = .94
        CASE IS < .96
            PriceFraction! = .98
        CASE IS < 1.01
            PriceFraction! = 1!
        CASE ELSE
            PriceFraction! = price
    END SELECT
END FUNCTION
```


{{out}} (run 5 times):
 .7388727      .82
 .8593103      .9
 .826687       .9
 .3444635      .5
 .0491907      .1


## BBC BASIC


```bbcbasic
      PRINT FNpricefraction(0.5)
      END

      DEF FNpricefraction(p)
      IF p < 0.06 THEN = 0.10
      IF p < 0.11 THEN = 0.18
      IF p < 0.16 THEN = 0.26
      IF p < 0.21 THEN = 0.32
      IF p < 0.26 THEN = 0.38
      IF p < 0.31 THEN = 0.44
      IF p < 0.36 THEN = 0.50
      IF p < 0.41 THEN = 0.54
      IF p < 0.46 THEN = 0.58
      IF p < 0.51 THEN = 0.62
      IF p < 0.56 THEN = 0.66
      IF p < 0.61 THEN = 0.70
      IF p < 0.66 THEN = 0.74
      IF p < 0.71 THEN = 0.78
      IF p < 0.76 THEN = 0.82
      IF p < 0.81 THEN = 0.86
      IF p < 0.86 THEN = 0.90
      IF p < 0.91 THEN = 0.94
      IF p < 0.96 THEN = 0.98
      = 1.00
```



## Bracmat

Bracmat has no native support for floating point variables nor for the fixed point values in the conversion table. Instead this solution just applies a string comparison.

```bracmat
( ( convert
  =
    .         ("0.06"."0.10")
              ("0.11"."0.18")
              ("0.16"."0.26")
              ("0.21"."0.32")
              ("0.26"."0.38")
              ("0.31"."0.44")
              ("0.36"."0.50")
              ("0.41"."0.54")
              ("0.46"."0.58")
              ("0.51"."0.62")
              ("0.56"."0.66")
              ("0.61"."0.70")
              ("0.66"."0.74")
              ("0.71"."0.78")
              ("0.76"."0.82")
              ("0.81"."0.86")
              ("0.86"."0.90")
              ("0.91"."0.94")
              ("0.96"."0.98")
              ("1.01"."1.00")
          : ? (>!arg.?arg) ?
        & !arg
      | "invalid input"
  )
& -1:?n
&   whl
  ' ( !n+1:?n:<103
    & ( @(!n:? [<2)&str$("0.0" !n):?a
      | @(!n:? [<3)&str$("0." !n):?a
      |   @(!n:?ones [-3 ?decimals)
        & str$(!ones "." !decimals):?a
      )
    & out$(!a "-->" convert$!a)
    )
)
```

{{out}}

```txt
0.00 --> 0.10
0.01 --> 0.10
0.02 --> 0.10
0.03 --> 0.10
0.04 --> 0.10
0.05 --> 0.10
0.06 --> 0.18
0.07 --> 0.18
0.08 --> 0.18
0.09 --> 0.18
0.10 --> 0.18
0.11 --> 0.26
0.12 --> 0.26
0.13 --> 0.26
0.14 --> 0.26
0.15 --> 0.26
0.16 --> 0.32
0.17 --> 0.32
  ...
0.85 --> 0.90
0.86 --> 0.94
0.87 --> 0.94
0.88 --> 0.94
0.89 --> 0.94
0.90 --> 0.94
0.91 --> 0.98
0.92 --> 0.98
0.93 --> 0.98
0.94 --> 0.98
0.95 --> 0.98
0.96 --> 1.00
0.97 --> 1.00
0.98 --> 1.00
0.99 --> 1.00
1.00 --> 1.00
1.01 --> invalid input
1.02 --> invalid input
```



## C


```c
#include <stdio.h>

double table[][2] = {
	{0.06, 0.10}, {0.11, 0.18}, {0.16, 0.26}, {0.21, 0.32},
	{0.26, 0.38}, {0.31, 0.44}, {0.36, 0.50}, {0.41, 0.54},
	{0.46, 0.58}, {0.51, 0.62}, {0.56, 0.66}, {0.61, 0.70},
	{0.66, 0.74}, {0.71, 0.78}, {0.76, 0.82}, {0.81, 0.86},
	{0.86, 0.90}, {0.91, 0.94}, {0.96, 0.98}, {1.01, 1.00},
	{-1, 0}, /* guarding element */
};

double price_fix(double x)
{
	int i;
	for (i = 0; table[i][0] > 0; i++)
		if (x < table[i][0]) return table[i][1];

	abort(); /* what else to do? */
}

int main()
{
	int i;
	for (i = 0; i <= 100; i++)
		printf("%.2f %.2f\n", i / 100., price_fix(i / 100.));

	return 0;
}
```


=={{header|C sharp|C#}}==

```csharp
namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            for (int x = 0; x < 10; x++)
            {
                Console.WriteLine("In: {0:0.00}, Out: {1:0.00}", ((double)x) / 10, SpecialRound(((double)x) / 10));
            }

            Console.WriteLine();

            for (int x = 0; x < 10; x++)
            {
                Console.WriteLine("In: {0:0.00}, Out: {1:0.00}", ((double)x) / 10 + 0.05, SpecialRound(((double)x) / 10 + 0.05));
            }

            Console.WriteLine();
            Console.WriteLine("In: {0:0.00}, Out: {1:0.00}", 1.01, SpecialRound(1.01));

            Console.Read();
        }

        private static double SpecialRound(double inValue)
        {
            if (inValue > 1) return 1;

            double[] Splitters = new double[] {
                   0.00 , 0.06 , 0.11 , 0.16 , 0.21 ,
                   0.26 , 0.31 , 0.36 , 0.41 , 0.46 ,
                   0.51 , 0.56 , 0.61 , 0.66 , 0.71 ,
                   0.76 , 0.81 , 0.86 , 0.91 , 0.96 };

            double[] replacements = new double[] {
                    0.10 , 0.18 , 0.26 , 0.32 , 0.38 ,
                    0.44 , 0.50 , 0.54 , 0.58 , 0.62 ,
                    0.66 , 0.70 , 0.74 , 0.78 , 0.82 ,
                    0.86 , 0.90 , 0.94 , 0.98 , 1.00 };

            for (int x = 0; x < Splitters.Length - 1; x++)
            {
                if (inValue >= Splitters[x] &&
                    inValue < Splitters[x + 1])
                {
                    return replacements[x];
                }
            }

            return inValue;
        }
    }
}
```



## C++


```cpp
#include <iostream>
#include <cmath>

int main( ) {
   double froms[ ] = { 0.00 , 0.06 , 0.11 , 0.16 , 0.21 , 0.26 ,
       0.31 , 0.36 , 0.41 , 0.46 , 0.51 , 0.56 , 0.61 , 0.66 ,
       0.71 , 0.76 , 0.81 , 0.86 , 0.91 , 0.96 } ;
   double tos[ ] = { 0.06 , 0.11 , 0.16 , 0.21 , 0.26 , 0.31 ,
      0.36 , 0.41 , 0.46 , 0.51 , 0.56 , 0.61 , 0.66 , 0.71 ,
      0.76 , 0.81 , 0.86 , 0.91 , 0.96 , 1.01 } ;
   double replacements [] = { 0.10 , 0.18 , 0.26 , 0.32 , 0.38 ,
      0.44 , 0.50 , 0.54 , 0.58 , 0.62 , 0.66 , 0.70 , 0.74 ,
      0.78 , 0.82 , 0.86 , 0.90 , 0.94 , 0.98 , 1.00 } ;
   double number = 0.1 ;
   std::cout << "Enter a fractional number between 0 and 1 ( 0 to end )!\n" ;
   std::cin >> number ;
   while ( number != 0 ) {
      if ( number < 0 || number > 1 ) {
	 std::cerr << "Error! Only positive values between 0 and 1 are allowed!\n" ;
	 return 1 ;
      }
      int n = 0 ;
      while ( ! ( number >= froms[ n ] && number < tos[ n ] ) )
	 n++ ;
      std::cout << "-->" << replacements[ n ] << '\n' ;
      std::cout << "Enter a fractional number ( 0 to end )!\n" ;
      std::cin >> number ;
   }
   return 0 ;
}

```


{{out}}

```txt

Enter a fractional number between 0 and 1 ( 0 to end )!
0.7
-->0.78
Enter a fractional number ( 0 to end )!
0.32
-->0.5
Enter a fractional number ( 0 to end )!
0.12
-->0.26
Enter a fractional number ( 0 to end )!
0

```



## Clipper


```dbase
FUNCTION PriceFraction( npQuantDispensed )
    LOCAL aPriceFraction := { {0,.06,.1},;
                            {.06,.11,.18}, ;
                            {.11,.16,.26}, ;
                            {.16,.21,.32}, ;
                            {.21,.26,.38}, ;
                            {.26,.31,.44}, ;
                            {.31,.36,.5}, ;
                            {.36,.41,.54}, ;
                            {.41,.46,.58}, ;
                            {.46,.51,.62}, ;
                            {.51,.56,.66}, ;
                            {.56,.61,.7}, ;
                            {.61,.66,.74}, ;
                            {.66,.71,.78}, ;
                            {.71,.76,.82}, ;
                            {.76,.81,.86}, ;
                            {.81,.86,.9}, ;
                            {.86,.91,.94}, ;
                            {.91,.96,.98} }
    LOCAL nResult
    LOCAL nScan
    IF npQuantDispensed = 0
            nResult = 0
    ELSEIF npQuantDispensed >= .96
            nResult = 1
    ELSE
            nScan := ASCAN( aPriceFraction, ;
                   { |aItem| npQuantDispensed >= aItem[ 1 ] .AND.;
                             npQuantDispensed <  aItem[ 2 ] } )
            nResult := aPriceFraction[ nScan ][ 3 ]
    END IF
    RETURN nResult
```


The function above crashes with an array access bound error if the value passed is negative.
Also, the spec. indicates that 0.00 should be replaced with standard value 0.10, not 0.
The following is a more concise solution:


```Clipper
Procedure Main()
   Local i
   For i := -0.02 to 1.02 STEP 0.03
      ? i, "->", PriceFraction(i), i+0.02, "->", PriceFraction(i+0.02)
   Next
Return


Static Function PriceFraction( nValue )
   Local nResult
   Local n
   // Function is only defined for values 0 to 1.00
   // Return NIL for anything else
   // Table of values {V1, V2} = {Threshhold, Standard value}
   #define TV_THRESHHOLD 1
   #define TV_STD_VALUE  2
   Local aTable := { {0,    NIL },;
                     {0.06, 0.10},;
                     {0.11, 0.18},;
                     {0.16, 0.26},;
                     {0.21, 0.32},;
                     {0.26, 0.38},;
                     {0.31, 0.44},;
                     {0.36, 0.50},;
                     {0.41, 0.54},;
                     {0.46, 0.58},;
                     {0.51, 0.62},;
                     {0.56, 0.66},;
                     {0.61, 0.70},;
                     {0.66, 0.74},;
                     {0.71, 0.78},;
                     {0.76, 0.82},;
                     {0.81, 0.86},;
                     {0.86, 0.90},;
                     {0.91, 0.94},;
                     {0.96, 0.98},;
                     {1.01, 1.00} }
   n := AScan( aTable, {|x| nValue < x[TV_THRESHHOLD] })
   If n > 0
      nResult := aTable[n][TV_STD_VALUE]
   Else
      nResult := NIL
   Endif
Return nResult
```


{{out}}
<pre style="height:30ex;overflow:scroll">        -0.02 -> NIL          0.00 ->          0.10
         0.01 ->          0.10          0.03 ->          0.10
         0.04 ->          0.10          0.06 ->          0.18
         0.07 ->          0.18          0.09 ->          0.18
         0.10 ->          0.18          0.12 ->          0.26
         0.13 ->          0.26          0.15 ->          0.26
         0.16 ->          0.32          0.18 ->          0.32
         0.19 ->          0.32          0.21 ->          0.38
         0.22 ->          0.38          0.24 ->          0.38
         0.25 ->          0.38          0.27 ->          0.44
         0.28 ->          0.44          0.30 ->          0.44
         0.31 ->          0.50          0.33 ->          0.50
         0.34 ->          0.50          0.36 ->          0.54
         0.37 ->          0.54          0.39 ->          0.54
         0.40 ->          0.54          0.42 ->          0.58
         0.43 ->          0.58          0.45 ->          0.58
         0.46 ->          0.62          0.48 ->          0.62
         0.49 ->          0.62          0.51 ->          0.66
         0.52 ->          0.66          0.54 ->          0.66
         0.55 ->          0.66          0.57 ->          0.70
         0.58 ->          0.70          0.60 ->          0.70
         0.61 ->          0.74          0.63 ->          0.74
         0.64 ->          0.74          0.66 ->          0.78
         0.67 ->          0.78          0.69 ->          0.78
         0.70 ->          0.78          0.72 ->          0.82
         0.73 ->          0.82          0.75 ->          0.82
         0.76 ->          0.86          0.78 ->          0.86
         0.79 ->          0.86          0.81 ->          0.90
         0.82 ->          0.90          0.84 ->          0.90
         0.85 ->          0.90          0.87 ->          0.94
         0.88 ->          0.94          0.90 ->          0.94
         0.91 ->          0.98          0.93 ->          0.98
         0.94 ->          0.98          0.96 ->          1.00
         0.97 ->          1.00          0.99 ->          1.00
         1.00 ->          1.00          1.02 -> NIL
```



## Clojure

{{trans|JavaScript}}

```clojure
(def values [10 18 26 32 38 44 50 54 58 62 66 70 74 78 82 86 90 94 98 100])

(defn price [v]
  (format "%.2f" (double (/ (values (int (/ (- (* v 100) 1) 5))) 100))))
```






{{out}}

```txt

user=> (price 0.50)
"0.62"
user=> (let [k (map #(double (/ % 100)) (range 101))] (sort (zipmap k (map #(price %) k))))
([0.0 "0.10"] [0.01 "0.10"] [0.02 "0.10"] [0.03 "0.10"] [0.04 "0.10"] [0.05 "0.10"]
 [0.06 "0.18"] [0.07 "0.18"] [0.08 "0.18"] [0.09 "0.18"] [0.1 "0.18"]
 [0.11 "0.26"] [0.12 "0.26"] [0.13 "0.26"] [0.14 "0.26"] [0.15 "0.26"]
 [0.16 "0.32"] [0.17 "0.32"] [0.18 "0.32"] [0.19 "0.32"] [0.2 "0.32"]
 [0.21 "0.38"] [0.22 "0.38"] [0.23 "0.38"] [0.24 "0.38"] [0.25 "0.38"]
 [0.26 "0.44"] [0.27 "0.44"] [0.28 "0.44"] [0.29 "0.44"] [0.3 "0.44"]
 [0.31 "0.50"] [0.32 "0.50"] [0.33 "0.50"] [0.34 "0.50"] [0.35 "0.50"]
 [0.36 "0.54"] [0.37 "0.54"] [0.38 "0.54"] [0.39 "0.54"] [0.4 "0.54"]
 [0.41 "0.58"] [0.42 "0.58"] [0.43 "0.58"] [0.44 "0.58"] [0.45 "0.58"]
 [0.46 "0.62"] [0.47 "0.62"] [0.48 "0.62"] [0.49 "0.62"] [0.5 "0.62"]
 [0.51 "0.66"] [0.52 "0.66"] [0.53 "0.66"] [0.54 "0.66"] [0.55 "0.66"]
 [0.56 "0.70"] [0.57 "0.70"] [0.58 "0.70"] [0.59 "0.70"] [0.6 "0.70"]
 [0.61 "0.74"] [0.62 "0.74"] [0.63 "0.74"] [0.64 "0.74"] [0.65 "0.74"]
 [0.66 "0.78"] [0.67 "0.78"] [0.68 "0.78"] [0.69 "0.78"] [0.7 "0.78"]
 [0.71 "0.82"] [0.72 "0.82"] [0.73 "0.82"] [0.74 "0.82"] [0.75 "0.82"]
 [0.76 "0.86"] [0.77 "0.86"] [0.78 "0.86"] [0.79 "0.86"] [0.8 "0.86"]
 [0.81 "0.90"] [0.82 "0.90"] [0.83 "0.90"] [0.84 "0.90"] [0.85 "0.90"]
 [0.86 "0.94"] [0.87 "0.94"] [0.88 "0.94"] [0.89 "0.94"] [0.9 "0.94"]
 [0.91 "0.98"] [0.92 "0.98"] [0.93 "0.98"] [0.94 "0.98"] [0.95 "0.98"]
 [0.96 "1.00"] [0.97 "1.00"] [0.98 "1.00"] [0.99 "1.00"] [1.0 "1.00"])
```



## Common Lisp


```lisp
(defun scale (value)
  (cond ((minusp value) (error "invalid value: ~A" value))
        ((< value 0.06) 0.10)
        ((< value 0.11) 0.18)
        ((< value 0.16) 0.26)
        ((< value 0.21) 0.32)
        ((< value 0.26) 0.38)
        ((< value 0.31) 0.44)
        ((< value 0.36) 0.50)
        ((< value 0.41) 0.54)
        ((< value 0.46) 0.58)
        ((< value 0.51) 0.62)
        ((< value 0.56) 0.66)
        ((< value 0.61) 0.70)
        ((< value 0.66) 0.74)
        ((< value 0.71) 0.78)
        ((< value 0.76) 0.82)
        ((< value 0.81) 0.86)
        ((< value 0.86) 0.90)
        ((< value 0.91) 0.94)
        ((< value 0.96) 0.98)
        ((< value 1.01) 1.00)
        (t (error "invalid value: ~A" value))))
```



## D


```d
import std.stdio, std.range;

double priceRounder(in double price) pure nothrow
in {
    assert(price >= 0 && price <= 1.0);
} body {
    static immutable cin  = [.06, .11, .16, .21, .26, .31, .36, .41,
                             .46, .51, .56, .61, .66, .71, .76, .81,
                             .86, .91, .96, 1.01],
                     cout = [.10, .18, .26, .32, .38, .44, .50, .54,
                             .58, .62, .66, .70, .74, .78, .82, .86,
                             .90, .94, .98, 1.00];
    return cout[cin.assumeSorted.lowerBound(price).length];
}

void main() {
    foreach (const price; [0.7388727, 0.8593103, 0.826687, 0.3444635])
        price.priceRounder.writeln;
}
```

{{out}}

```txt
0.82
0.9
0.9
0.5
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			--Tests the price_adjusted feature.
		local
			i: REAL
		do
			create price_fraction.initialize
			from
				i := 5
			until
				i = 100
			loop
				io.put_string ("Given: ")
				io.put_real (i / 100)
				io.put_string ("%TAdjusted:")
				io.put_real (price_fraction.adjusted_price (i / 100))
				io.new_line
				i := i + 5
			end
		end

	price_fraction: PRICE_FRACTION

end

```


```Eiffel

class
	PRICE_FRACTION

create
	initialize

feature

	initialize
			-- Initializes limit and price to the given values.
		do
			limit := <<0.06, 0.11, 0.16, 0.21, 0.26, 0.31, 0.36, 0.41, 0.46, 0.51, 0.56, 0.61, 0.66, 0.71, 0.76, 0.81, 0.86, 0.91, 0.96, 1.01>>
			price := <<0.10, 0.18, 0.26, 0.32, 0.38, 0.44, 0.50, 0.54, 0.58, 0.62, 0.66, 0.70, 0.74, 0.78, 0.81, 0.86, 0.90, 0.94, 0.98, 1.00>>
		end

	adjusted_price (n: REAL): REAL
			-- Adjusted price according to the given price values.
		local
			i: INTEGER
			found: BOOLEAN
		do
			from
				i := 1
			until
				i > limit.count or found
			loop
				if n <= limit [i] then
					Result := (price [i])
					found := True
				end
				i := i + 1
			end
		end

feature {NONE}

	limit: ARRAY [REAL]

	price: ARRAY [REAL]

end

```

{{out}}

```txt

Given: 0.05    Adjusted:0.1
Given: 0.1     Adjusted:0.18
Given: 0.15    Adjusted:0.26
Given: 0.2     Adjusted:0.32
Given: 0.25    Adjusted:0.38
Given: 0.3     Adjusted:0.44
Given: 0.35    Adjusted:0.5
Given: 0.4     Adjusted:0.54
Given: 0.45    Adjusted:0.58
Given: 0.5     Adjusted:0.62
Given: 0.55    Adjusted:0.66
Given: 0.6     Adjusted:0.7
Given: 0.65    Adjusted:0.74
Given: 0.7     Adjusted:0.78
Given: 0.75    Adjusted:0.81
Given: 0.8     Adjusted:0.86
Given: 0.85    Adjusted:0.9
Given: 0.9     Adjusted:0.94
Given: 0.95    Adjusted:0.98

```



## Elixir


```elixir
defmodule Price do
  @table [ {0.06, 0.10}, {0.11, 0.18}, {0.16, 0.26}, {0.21, 0.32}, {0.26, 0.38},
           {0.31, 0.44}, {0.36, 0.50}, {0.41, 0.54}, {0.46, 0.58}, {0.51, 0.62},
           {0.56, 0.66}, {0.61, 0.70}, {0.66, 0.74}, {0.71, 0.78}, {0.76, 0.82},
           {0.81, 0.86}, {0.86, 0.90}, {0.91, 0.94}, {0.96, 0.98}, {1.01, 1.00} ]

  def fraction(value) when value in 0..1 do
    {_, standard_value} = Enum.find(@table, fn {upper_limit, _} -> value < upper_limit end)
    standard_value
  end
end

val = for i <- 0..100, do: i/100
Enum.each(val, fn x ->
  :io.format "~5.2f ->~5.2f~n", [x, Price.fraction(x)]
end)
```


{{out}}

```txt

 0.00 -> 0.10
 0.01 -> 0.10
 0.02 -> 0.10
 0.03 -> 0.10
 0.04 -> 0.10
 0.05 -> 0.10
 0.06 -> 0.18
 0.07 -> 0.18
 0.08 -> 0.18
 0.09 -> 0.18
 0.10 -> 0.18
 0.11 -> 0.26
...
 0.95 -> 0.98
 0.96 -> 1.00
 0.97 -> 1.00
 0.98 -> 1.00
 0.99 -> 1.00
 1.00 -> 1.00

```



## Erlang


```erlang
priceFraction(N) when N < 0 orelse N > 1 ->
    erlang:error('Values must be between 0 and 1.');
priceFraction(N) when N < 0.06 -> 0.10;
priceFraction(N) when N < 0.11 -> 0.18;
priceFraction(N) when N < 0.16 -> 0.26;
priceFraction(N) when N < 0.21 -> 0.32;
priceFraction(N) when N < 0.26 -> 0.38;
priceFraction(N) when N < 0.31 -> 0.44;
priceFraction(N) when N < 0.36 -> 0.50;
priceFraction(N) when N < 0.41 -> 0.54;
priceFraction(N) when N < 0.46 -> 0.58;
priceFraction(N) when N < 0.51 -> 0.62;
priceFraction(N) when N < 0.56 -> 0.66;
priceFraction(N) when N < 0.61 -> 0.70;
priceFraction(N) when N < 0.66 -> 0.74;
priceFraction(N) when N < 0.71 -> 0.78;
priceFraction(N) when N < 0.76 -> 0.82;
priceFraction(N) when N < 0.81 -> 0.86;
priceFraction(N) when N < 0.86 -> 0.90;
priceFraction(N) when N < 0.91 -> 0.94;
priceFraction(N) when N < 0.96 -> 0.98;
priceFraction(N) -> 1.00.
```



## Euphoria

{{trans|C}}

```euphoria
constant table = {
    {0.06, 0.10}, {0.11, 0.18}, {0.16, 0.26}, {0.21, 0.32},
    {0.26, 0.38}, {0.31, 0.44}, {0.36, 0.50}, {0.41, 0.54},
    {0.46, 0.58}, {0.51, 0.62}, {0.56, 0.66}, {0.61, 0.70},
    {0.66, 0.74}, {0.71, 0.78}, {0.76, 0.82}, {0.81, 0.86},
    {0.86, 0.90}, {0.91, 0.94}, {0.96, 0.98}, {1.01, 1.00}
}

function price_fix(atom x)
    for i = 1 to length(table) do
        if x < table[i][1] then
            return table[i][2]
        end if
    end for
    return -1
end function

for i = 0 to 99 do
    printf(1, "%.2f %.2f\n", { i/100, price_fix(i/100) })
end for
```



=={{header|F_Sharp|F#}}==
Inspired by Python's bisect solution. Using decimal (System.Decimal) to avoid number representation problems with floats.

```fsharp
let cin = [ 0.06m .. 0.05m ..1.01m ]
let cout = [0.1m; 0.18m] @ [0.26m .. 0.06m .. 0.44m] @ [0.50m .. 0.04m .. 0.98m] @ [1.m]

let priceadjuster p =
    let rec bisect lo hi =
        if lo < hi then
            let mid = (lo+hi)/2.
            let left = p < cin.[int mid]
            bisect (if left then lo else mid+1.) (if left then mid else hi)
        else lo

    if p < 0.m || 1.m < p then p
    else cout.[int (bisect 0. (float cin.Length))]

[ 0.m .. 0.01m .. 1.m ]
|> Seq.ofList
|> Seq.iter (fun p -> printfn "%.2f -> %.2f" p (priceadjuster p))
```

{{out}}
The same as shown by Ada as of 2013-11-03T17:42Z (apart from whitespace formatting)


## Factor


```factor
CONSTANT: dispensary-data {
{ 0.06 0.10 }
{ 0.11 0.18 }
{ 0.16 0.26 }
{ 0.21 0.32 }
{ 0.26 0.38 }
{ 0.31 0.44 }
{ 0.36 0.50 }
{ 0.41 0.54 }
{ 0.46 0.58 }
{ 0.51 0.62 }
{ 0.56 0.66 }
{ 0.61 0.70 }
{ 0.66 0.74 }
{ 0.71 0.78 }
{ 0.76 0.82 }
{ 0.81 0.86 }
{ 0.86 0.90 }
{ 0.91 0.94 }
{ 0.96 0.98 }
{ 1.01 1.00 } }

: price-fraction ( n -- n ) dispensary-data [ first over >= ] find 2nip second ;

{ 0 0.5 0.65 0.66 1 } [ price-fraction ] map
```


{{out}}

{ 0.1 0.62 0.74 0.74 1.0 }


## Fantom



```fantom

class Defn // to hold the three numbers from a 'row' in the table
{
  Float low
  Float high
  Float value
  new make (Float low, Float high, Float value)
  {
    this.low = low
    this.high = high
    this.value = value
  }
}

class PriceConverter
{
  Defn[] defns := [,]
  new make (Str table) // process given table and store numbers from each row in a defn
  {
    table.split('\n').each |Str line|
    {
      data := line.split
      defns.add (Defn(Float.fromStr(data[1]), Float.fromStr(data[3]), Float.fromStr(data[5])))
    }
  }

  public Float convert (Float price) // convert by looking through list of defns
  {
    Float result := price
    defns.each |Defn defn|
    {
      if (price >= defn.low && price < defn.high)
        result = defn.value
    }
    return result
  }
}

class Main
{
  public static Void main ()
  {
    table := ">=  0.00  <  0.06  :=  0.10
              >=  0.06  <  0.11  :=  0.18
              >=  0.11  <  0.16  :=  0.26
              >=  0.16  <  0.21  :=  0.32
              >=  0.21  <  0.26  :=  0.38
              >=  0.26  <  0.31  :=  0.44
              >=  0.31  <  0.36  :=  0.50
              >=  0.36  <  0.41  :=  0.54
              >=  0.41  <  0.46  :=  0.58
              >=  0.46  <  0.51  :=  0.62
              >=  0.51  <  0.56  :=  0.66
              >=  0.56  <  0.61  :=  0.70
              >=  0.61  <  0.66  :=  0.74
              >=  0.66  <  0.71  :=  0.78
              >=  0.71  <  0.76  :=  0.82
              >=  0.76  <  0.81  :=  0.86
              >=  0.81  <  0.86  :=  0.90
              >=  0.86  <  0.91  :=  0.94
              >=  0.91  <  0.96  :=  0.98
              >=  0.96  <  1.01  :=  1.00"
    converter := PriceConverter (table)
    10.times  // simple test with random values
    {
      price := (0..100).random.toFloat / 100
      echo ("$price -> ${converter.convert (price)}")
    }
  }
}

```



## Forth


A floating-point version wouldn't be hard -- four words would change ( , @ @ cell+ -to- f, f@ f@ float+ ), EVALUATE would be replaced with a small word that forced a floating-point interpretation, and the return stack would not be used in ROUND -- but it would be strikingly unusual.  See this page's discussion.


```forth
: as begin parse-word dup while evaluate , repeat 2drop ;

create bounds   as  96 91 86 81 76 71 66 61 56 51 46 41 36 31 26 21 16 11  6  0
create official as 100 98 94 90 86 82 78 74 70 66 62 58 54 50 44 38 32 26 18 10

: official@ ( a-bounds -- +n )
  \ (a+n) - a + b = (a+n) + (b - a) = (b+n)
  [ official bounds - ] literal + @ ;

: round ( n-cents -- n-cents' )
  >r bounds begin dup @ r@ > while cell+ repeat
  r> drop official@ ;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program price_fraction

  implicit none
  integer, parameter :: i_max = 10
  integer :: i
  real, dimension (20), parameter :: in =                           &
    & (/0.00, 0.06, 0.11, 0.16, 0.21, 0.26, 0.31, 0.36, 0.41, 0.46, &
    &   0.51, 0.56, 0.61, 0.66, 0.71, 0.76, 0.81, 0.86, 0.91, 0.96/)
  real, dimension (20), parameter :: out =                          &
    & (/0.10, 0.18, 0.26, 0.32, 0.38, 0.44, 0.50, 0.54, 0.58, 0.62, &
    &   0.66, 0.70, 0.74, 0.78, 0.82, 0.86, 0.90, 0.94, 0.98, 1.00/)
  real :: r

  do i = 1, i_max
    call random_number (r)
    write (*, '(f8.6, 1x, f4.2)') r, out (maxloc (in, r >= in))
  end do

end program price_fraction
```

{{out}}
<lang>0.997560 1.00
0.566825 0.70
0.965915 1.00
0.747928 0.82
0.367391 0.54
0.480637 0.62
0.073754 0.18
0.005355 0.10
0.347081 0.50
0.342244 0.50
```



## FreeBASIC


```freebasic
' FB 1.050.0 Win64

Function rescale(price As Double) As Double
  If price < 0.00 OrElse price > 1.00 Then Return price
  Select Case price
    Case Is < 0.06 : Return 0.10
    Case Is < 0.11 : Return 0.18
    Case Is < 0.16 : Return 0.26
    Case Is < 0.21 : Return 0.32
    Case Is < 0.26 : Return 0.38
    Case Is < 0.31 : Return 0.44
    Case Is < 0.36 : Return 0.50
    Case Is < 0.41 : Return 0.54
    Case Is < 0.46 : Return 0.58
    Case Is < 0.51 : Return 0.62
    Case Is < 0.56 : Return 0.66
    Case Is < 0.61 : Return 0.70
    Case Is < 0.66 : Return 0.74
    Case Is < 0.71 : Return 0.78
    Case Is < 0.76 : Return 0.82
    Case Is < 0.81 : Return 0.86
    Case Is < 0.86 : Return 0.90
    Case Is < 0.91 : Return 0.94
    Case Is < 0.96 : Return 0.98
  End Select
  Return 1.00
End Function

For i As Integer = 1 To 100
  Dim d As Double = i/100.0
  Print Using "#.##"; d;
  Print " -> ";
  Print Using "#.##"; rescale(d);
  Print "  ";
  If i Mod 5 = 0 Then Print
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

0.01 -> 0.10  0.02 -> 0.10  0.03 -> 0.10  0.04 -> 0.10  0.05 -> 0.10
0.06 -> 0.18  0.07 -> 0.18  0.08 -> 0.18  0.09 -> 0.18  0.10 -> 0.18
0.11 -> 0.26  0.12 -> 0.26  0.13 -> 0.26  0.14 -> 0.26  0.15 -> 0.26
0.16 -> 0.32  0.17 -> 0.32  0.18 -> 0.32  0.19 -> 0.32  0.20 -> 0.32
0.21 -> 0.38  0.22 -> 0.38  0.23 -> 0.38  0.24 -> 0.38  0.25 -> 0.38
0.26 -> 0.44  0.27 -> 0.44  0.28 -> 0.44  0.29 -> 0.44  0.30 -> 0.44
0.31 -> 0.50  0.32 -> 0.50  0.33 -> 0.50  0.34 -> 0.50  0.35 -> 0.50
0.36 -> 0.54  0.37 -> 0.54  0.38 -> 0.54  0.39 -> 0.54  0.40 -> 0.54
0.41 -> 0.58  0.42 -> 0.58  0.43 -> 0.58  0.44 -> 0.58  0.45 -> 0.58
0.46 -> 0.62  0.47 -> 0.62  0.48 -> 0.62  0.49 -> 0.62  0.50 -> 0.62
0.51 -> 0.66  0.52 -> 0.66  0.53 -> 0.66  0.54 -> 0.66  0.55 -> 0.66
0.56 -> 0.70  0.57 -> 0.70  0.58 -> 0.70  0.59 -> 0.70  0.60 -> 0.70
0.61 -> 0.74  0.62 -> 0.74  0.63 -> 0.74  0.64 -> 0.74  0.65 -> 0.74
0.66 -> 0.78  0.67 -> 0.78  0.68 -> 0.78  0.69 -> 0.78  0.70 -> 0.78
0.71 -> 0.82  0.72 -> 0.82  0.73 -> 0.82  0.74 -> 0.82  0.75 -> 0.82
0.76 -> 0.86  0.77 -> 0.86  0.78 -> 0.86  0.79 -> 0.86  0.80 -> 0.86
0.81 -> 0.90  0.82 -> 0.90  0.83 -> 0.90  0.84 -> 0.90  0.85 -> 0.90
0.86 -> 0.94  0.87 -> 0.94  0.88 -> 0.94  0.89 -> 0.94  0.90 -> 0.94
0.91 -> 0.98  0.92 -> 0.98  0.93 -> 0.98  0.94 -> 0.98  0.95 -> 0.98
0.96 -> 1.00  0.97 -> 1.00  0.98 -> 1.00  0.99 -> 1.00  1.00 -> 1.00

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=87527eed297164593d88aa2c35898eaf Click this link to run this code]'''

```gambas
Public Sub Main()
Dim byValue As Byte[] = [10, 18, 26, 32, 38, 44, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86, 90, 94, 98, 100]
Dim byLimit As Byte[] = [6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 81, 86, 91, 96]
Dim byCount, byCheck As Byte

For byCount = 0 To 100
  For byCheck = 0 To byLimit.Max
    If byCount < byLimit[byCheck] Then Break
  Next
  Print Format(byCount / 100, "0.00") & " = " & Format(byValue[byCheck] / 100, "0.00") & gb.Tab;
  If byCount Mod 5 = 0 Then Print
Next

End
```

Output:

```txt

0.00 = 0.10
0.01 = 0.10     0.02 = 0.10     0.03 = 0.10     0.04 = 0.10     0.05 = 0.10
0.06 = 0.18     0.07 = 0.18     0.08 = 0.18     0.09 = 0.18     0.10 = 0.18
0.11 = 0.26     0.12 = 0.26     0.13 = 0.26     0.14 = 0.26     0.15 = 0.26
0.16 = 0.32     0.17 = 0.32     0.18 = 0.32     0.19 = 0.32     0.20 = 0.32
0.21 = 0.38     0.22 = 0.38     0.23 = 0.38     0.24 = 0.38     0.25 = 0.38
0.26 = 0.44     0.27 = 0.44     0.28 = 0.44     0.29 = 0.44     0.30 = 0.44
0.31 = 0.50     0.32 = 0.50     0.33 = 0.50     0.34 = 0.50     0.35 = 0.50
0.36 = 0.54     0.37 = 0.54     0.38 = 0.54     0.39 = 0.54     0.40 = 0.54
0.41 = 0.58     0.42 = 0.58     0.43 = 0.58     0.44 = 0.58     0.45 = 0.58
0.46 = 0.62     0.47 = 0.62     0.48 = 0.62     0.49 = 0.62     0.50 = 0.62
0.51 = 0.66     0.52 = 0.66     0.53 = 0.66     0.54 = 0.66     0.55 = 0.66
0.56 = 0.70     0.57 = 0.70     0.58 = 0.70     0.59 = 0.70     0.60 = 0.70
0.61 = 0.74     0.62 = 0.74     0.63 = 0.74     0.64 = 0.74     0.65 = 0.74
0.66 = 0.78     0.67 = 0.78     0.68 = 0.78     0.69 = 0.78     0.70 = 0.78
0.71 = 0.82     0.72 = 0.82     0.73 = 0.82     0.74 = 0.82     0.75 = 0.82
0.76 = 0.86     0.77 = 0.86     0.78 = 0.86     0.79 = 0.86     0.80 = 0.86
0.81 = 0.90     0.82 = 0.90     0.83 = 0.90     0.84 = 0.90     0.85 = 0.90
0.86 = 0.94     0.87 = 0.94     0.88 = 0.94     0.89 = 0.94     0.90 = 0.94
0.91 = 0.98     0.92 = 0.98     0.93 = 0.98     0.94 = 0.98     0.95 = 0.98
0.96 = 1.00     0.97 = 1.00     0.98 = 1.00     0.99 = 1.00     1.00 = 1.00

```


## Go


```go
func pf(v float64) float64 {
    switch {
    case v < .06: return .10
    case v < .11: return .18
    case v < .16: return .26
    case v < .21: return .32
    case v < .26: return .38
    case v < .31: return .44
    case v < .36: return .50
    case v < .41: return .54
    case v < .46: return .58
    case v < .51: return .62
    case v < .56: return .66
    case v < .61: return .70
    case v < .66: return .74
    case v < .71: return .78
    case v < .76: return .82
    case v < .81: return .86
    case v < .86: return .90
    case v < .91: return .94
    case v < .96: return .98
    }
    return 1
}
```



## Groovy


```groovy
def priceFraction(value) {
    assert value >= 0.0 && value <= 1.0

    def priceMappings = [(0.06): 0.10, (0.11): 0.18, (0.16): 0.26, (0.21): 0.32, (0.26): 0.38,
         (0.31): 0.44, (0.36): 0.50, (0.41): 0.54, (0.46): 0.58, (0.51): 0.62,
         (0.56): 0.66, (0.61): 0.70, (0.66): 0.74, (0.71): 0.78, (0.76): 0.82,
         (0.81): 0.86, (0.86): 0.90, (0.91): 0.94, (0.96): 0.98]

    for (price in priceMappings.keySet()) {
        if (value < price) return priceMappings[price]
    }
    1.00
}

for (def v = 0.00; v <= 1.00; v += 0.01) {
    println "$v --> ${priceFraction(v)}"
}
```

{{out}}
<div style="height: 200px;overflow:scroll">

```txt
0.00 --> 0.10
0.01 --> 0.10
0.02 --> 0.10
0.03 --> 0.10
0.04 --> 0.10
0.05 --> 0.10
0.06 --> 0.18
0.07 --> 0.18
0.08 --> 0.18
0.09 --> 0.18
0.10 --> 0.18
0.11 --> 0.26
0.12 --> 0.26
0.13 --> 0.26
0.14 --> 0.26
0.15 --> 0.26
0.16 --> 0.32
0.17 --> 0.32
0.18 --> 0.32
0.19 --> 0.32
0.20 --> 0.32
0.21 --> 0.38
0.22 --> 0.38
0.23 --> 0.38
0.24 --> 0.38
0.25 --> 0.38
0.26 --> 0.44
0.27 --> 0.44
0.28 --> 0.44
0.29 --> 0.44
0.30 --> 0.44
0.31 --> 0.50
0.32 --> 0.50
0.33 --> 0.50
0.34 --> 0.50
0.35 --> 0.50
0.36 --> 0.54
0.37 --> 0.54
0.38 --> 0.54
0.39 --> 0.54
0.40 --> 0.54
0.41 --> 0.58
0.42 --> 0.58
0.43 --> 0.58
0.44 --> 0.58
0.45 --> 0.58
0.46 --> 0.62
0.47 --> 0.62
0.48 --> 0.62
0.49 --> 0.62
0.50 --> 0.62
0.51 --> 0.66
0.52 --> 0.66
0.53 --> 0.66
0.54 --> 0.66
0.55 --> 0.66
0.56 --> 0.70
0.57 --> 0.70
0.58 --> 0.70
0.59 --> 0.70
0.60 --> 0.70
0.61 --> 0.74
0.62 --> 0.74
0.63 --> 0.74
0.64 --> 0.74
0.65 --> 0.74
0.66 --> 0.78
0.67 --> 0.78
0.68 --> 0.78
0.69 --> 0.78
0.70 --> 0.78
0.71 --> 0.82
0.72 --> 0.82
0.73 --> 0.82
0.74 --> 0.82
0.75 --> 0.82
0.76 --> 0.86
0.77 --> 0.86
0.78 --> 0.86
0.79 --> 0.86
0.80 --> 0.86
0.81 --> 0.90
0.82 --> 0.90
0.83 --> 0.90
0.84 --> 0.90
0.85 --> 0.90
0.86 --> 0.94
0.87 --> 0.94
0.88 --> 0.94
0.89 --> 0.94
0.90 --> 0.94
0.91 --> 0.98
0.92 --> 0.98
0.93 --> 0.98
0.94 --> 0.98
0.95 --> 0.98
0.96 --> 1.00
0.97 --> 1.00
0.98 --> 1.00
0.99 --> 1.00
1.00 --> 1.00
```

</div>


## Haskell


```haskell
price_fraction n
  | n < 0 || n > 1 = error "Values must be between 0 and 1."
  | n < 0.06 = 0.10
  | n < 0.11 = 0.18
  | n < 0.16 = 0.26
  | n < 0.21 = 0.32
  | n < 0.26 = 0.38
  | n < 0.31 = 0.44
  | n < 0.36 = 0.50
  | n < 0.41 = 0.54
  | n < 0.46 = 0.58
  | n < 0.51 = 0.62
  | n < 0.56 = 0.66
  | n < 0.61 = 0.70
  | n < 0.66 = 0.74
  | n < 0.71 = 0.78
  | n < 0.76 = 0.82
  | n < 0.81 = 0.86
  | n < 0.86 = 0.90
  | n < 0.91 = 0.94
  | n < 0.96 = 0.98
  | otherwise = 1.00
```

Alternative {{trans|OCaml}}:

```haskell
table = [
    (0.06, 0.10),   (0.11, 0.18),   (0.16, 0.26),   (0.21, 0.32),   (0.26, 0.38),
    (0.31, 0.44),   (0.36, 0.50),   (0.41, 0.54),   (0.46, 0.58),   (0.51, 0.62),
    (0.56, 0.66),   (0.61, 0.70),   (0.66, 0.74),   (0.71, 0.78),   (0.76, 0.82),
    (0.81, 0.86),   (0.86, 0.90),   (0.91, 0.94),   (0.96, 0.98),   (1.01, 1.00),
  ]

price_fraction n
  | n < 0 || n > 1 = error "Values must be between 0 and 1."
  | otherwise = snd $ head $ dropWhile ((<= n) . fst) table
```



## HicEst


```HicEst
DIMENSION upperbound(20), rescaleTo(20), temp(20)
upperbound = (.06,.11,.16,.21,.26,.31,.36,.41,.46,.51,.56,.61,.66,.71,.76,.81,.86,.91,.96,1.01)
rescaleTo =  (.10,.18,.26,.32,.38,.44,.50,.54,.58,.62,.66,.70,.74,.78,.82,.86,.90,.94,.98,1.00)

DO test = 1, 10
  value = RAN(0.5, 0.5)
  temp = value > upperbound
  PriceFraction = rescaleTo( INDEX(temp, 0) )
  WRITE(Format="F8.6, F6.2") value, PriceFraction
ENDDO
```


```txt
0.589230  0.70
0.017623  0.10
0.314343  0.50
0.553303  0.66
0.676283  0.78
0.016883  0.10
0.265656  0.44
0.460880  0.62
0.837450  0.90
0.228953  0.38
```


=={{header|Icon}} and {{header|Unicon}}==


```Icon

record Bounds(low,high,new)

# rescale given value according to a list of bounds
procedure rescale (i, bounds)
  every bound := !bounds do
    if bound.low <= i < bound.high
      then return bound.new
  return fail # could not find i in bounds
end

procedure main ()
  bounds := [
    Bounds(0.00, 0.06, 0.10),
    Bounds(0.06, 0.11, 0.18),
    Bounds(0.11, 0.16, 0.26),
    Bounds(0.16, 0.21, 0.32),
    Bounds(0.21, 0.26, 0.38),
    Bounds(0.26, 0.31, 0.44),
    Bounds(0.31, 0.36, 0.50),
    Bounds(0.36, 0.41, 0.54),
    Bounds(0.41, 0.46, 0.58),
    Bounds(0.46, 0.51, 0.62),
    Bounds(0.51, 0.56, 0.66),
    Bounds(0.56, 0.61, 0.70),
    Bounds(0.61, 0.66, 0.74),
    Bounds(0.66, 0.71, 0.78),
    Bounds(0.71, 0.76, 0.82),
    Bounds(0.76, 0.81, 0.86),
    Bounds(0.81, 0.86, 0.90),
    Bounds(0.86, 0.91, 0.94),
    Bounds(0.91, 0.96, 0.98),
    Bounds(0.96, 1.01, 1.00)
  ]

  # test the procedure
  every i := 0.00 to 1.00 by 0.1 do {
    write (i || " rescaled is " || rescale(i, bounds))
  }
end

```


{{out}}

```txt

0.0 rescaled is 0.1
0.1 rescaled is 0.18
0.2 rescaled is 0.32
0.3 rescaled is 0.44
0.4 rescaled is 0.54
0.5 rescaled is 0.62
0.6 rescaled is 0.7
0.7 rescaled is 0.78
0.8 rescaled is 0.86
0.9 rescaled is 0.94
1.0 rescaled is 1.0

```



## Inform 7

Inform doesn't have native floating-point support; this version uses fixed point numbers with two decimal places.


```inform7
Home is a room.

Price is a kind of value. 0.99 specifies a price.

Table of Price Standardization
upper bound	replacement
0.06		0.10
0.11		0.18
0.16		0.26
0.21		0.32
0.26		0.38
0.31		0.44
0.36		0.50
0.41		0.54
0.46		0.58
0.51		0.62
0.56		0.66
0.61		0.70
0.66		0.74
0.71		0.78
0.76		0.82
0.81		0.86
0.86		0.90
0.91		0.94
0.96		0.98
1.01		1.00

To decide which price is the standardized value of (P - price):
	repeat with N running from 1 to the number of rows in the Table of Price Standardization:
		choose row N in the Table of Price Standardization;
		if P is less than the upper bound entry, decide on the replacement entry.

When play begins:
	repeat with N running from 1 to 5:
		let P be a random price between 0.00 and 1.00;
		say "[P] -> [standardized value of P].";
	end the story.
```



## J

'''Solution:'''

```j
le  =: -0.96 0.91 0.86 0.81 0.76 0.71 0.66 0.61 0.56 0.51 0.46 0.41 0.36 0.31 0.26 0.21 0.16 0.11 0.06 0.0
out =:  1.00 0.98 0.94 0.90 0.86 0.82 0.78 0.74 0.70 0.66 0.62 0.58 0.54 0.50 0.44 0.38 0.32 0.26 0.18 0.1

priceFraction =:  out {~ le I. -
```


'''Example:'''

```j
   priceFraction 0.34 0.070145 0.06 0.05 0.50214 0.56 1 0.99 0
0.5 0.18 0.18 0.1 0.62 0.7 1 1 0.1
```


This implementation performs a binary search on the boundary values, and then uses the resulting index to select from the result values.

To prevent J's binary search from doing the wrong thing for values equal to a boundary, both the boundary values and the search value are negated.


## Java


```java
import java.util.Random;

public class Main {
	private static float priceFraction(float f) {
		if (0.00f <= f && f < 0.06f) return 0.10f;
		else if (f < 0.11f) return 0.18f;
		else if (f < 0.16f) return 0.26f;
		else if (f < 0.21f) return 0.32f;
		else if (f < 0.26f) return 0.38f;
		else if (f < 0.31f) return 0.44f;
		else if (f < 0.36f) return 0.50f;
		else if (f < 0.41f) return 0.54f;
		else if (f < 0.46f) return 0.58f;
		else if (f < 0.51f) return 0.62f;
		else if (f < 0.56f) return 0.66f;
		else if (f < 0.61f) return 0.70f;
		else if (f < 0.66f) return 0.74f;
		else if (f < 0.71f) return 0.78f;
		else if (f < 0.76f) return 0.82f;
		else if (f < 0.81f) return 0.86f;
		else if (f < 0.86f) return 0.90f;
		else if (f < 0.91f) return 0.94f;
		else if (f < 0.96f) return 0.98f;
		else if (f < 1.01f) return 1.00f;
		else throw new IllegalArgumentException();
	}

	public static void main(String[] args) {
		Random rnd = new Random();
		for (int i = 0; i < 5; i++) {
			float f = rnd.nextFloat();
			System.out.format("%8.6f -> %4.2f%n", f, priceFraction(f));
		}
	}
}
```

{{out}}

```txt
0.149969 -> 0.26
0.310605 -> 0.50
0.616683 -> 0.74
0.194047 -> 0.32
0.724852 -> 0.82
```




## JavaScript


In the task definition, the first step is 0.06, the rest are 0.05
so a re-factoring can subtract 0.01 from the value and divide by 0.05 to get the step.

Working with decimal numbers in JavaScript has issues, e.g. 0.06 - 0.01 = 0.049999999999999996 due to using IEEE 754 double precision numbers that can't accurately represent all decimals. So values are multiplied by 100 and integer arithmetic is used.

Note that multiplying a string by a number produces a number, the bitwise OR (|) truncates floating point numbers to integer, making it a concise replacement for ''Math.floor''.

Passing a value outside the range 0 <= x < 1.01 will return undefined.


```javascript
function getScaleFactor(v) {

  var values = ['0.10','0.18','0.26','0.32','0.38','0.44','0.50','0.54',
                '0.58','0.62','0.66','0.70','0.74','0.78','0.82','0.86',
                '0.90','0.94','0.98','1.00'];

  return values[(v * 100 - 1) / 5 | 0];
}
```



## jq

The solution given here is based on the JavaScript solution.

```jq
def getScaleFactor:
  ["0.10","0.18","0.26","0.32","0.38","0.44","0.50","0.54",
   "0.58","0.62","0.66","0.70","0.74","0.78","0.82","0.86",
   "0.90","0.94","0.98","1.00"] as $values
  | $values[ (. * 100 - 1) / 5 | floor ] ;
```

The full coverage test as given in the Ada example:

```jq
def test:
  (range(0;10)  | "0.0\(.) -> \( 0.01 * . | getScaleFactor)"),
  (range(10;100) | "0.\(.) -> \( 0.01 * . | getScaleFactor)");

test
```

Run the test, showing the first few lines of output:

```txt

$ jq -n -r -f Price_fraction.jq
0.00 -> 1.00
0.01 -> 0.10
0.02 -> 0.10
0.03 -> 0.10
0.04 -> 0.10
0.05 -> 0.10
0.06 -> 0.18
0.07 -> 0.18
0.08 -> 0.18
0.09 -> 0.18
0.10 -> 0.18
0.11 -> 0.26
...
```



## Julia

This solution is somewhat straightforward but does highlight a couple of Julia features.  The interval cut-offs and values are exactly represented by rational numbers.  The interval to which an input value belongs is identified by applying the <code>findfirst</code> (true value) function to an element-wise comparison (<code>.&lt;</code>) of this value to the cut-off array.

```Julia

const PFCUT = [6:5:101]//100
const PFVAL = [10:8:26, 32:6:50, 54:4:98, 100]//100

function pricefraction{T<:FloatingPoint}(a::T)
    zero(T) <= a || error("a = ", a, ", but it must be >= 0.")
    a <= one(T) || error("a = ", a, ", but it must be <= 1.")
    convert(T, PFVAL[findfirst(a .< PFCUT)])
end

test = [0.:0.05:1., 0.51, 0.56, 0.61, rand(), rand(), rand(), rand()]

println("Testing the price fraction function")
for t in test
    println(@sprintf "    %.4f -> %.4f" t pricefraction(t))
end

```


{{out}}

```txt

Testing the price fraction function
    0.0000 -> 0.1000
    0.0500 -> 0.1000
    0.1000 -> 0.1800
    0.1500 -> 0.2600
    0.2000 -> 0.3200
    0.2500 -> 0.3800
    0.3000 -> 0.4400
    0.3500 -> 0.5000
    0.4000 -> 0.5400
    0.4500 -> 0.5800
    0.5000 -> 0.6200
    0.5500 -> 0.6600
    0.6000 -> 0.7000
    0.6500 -> 0.7400
    0.7000 -> 0.7800
    0.7500 -> 0.8200
    0.8000 -> 0.8600
    0.8500 -> 0.9000
    0.9000 -> 0.9400
    0.9500 -> 0.9800
    1.0000 -> 1.0000
    0.5100 -> 0.6600
    0.5600 -> 0.7000
    0.6100 -> 0.7400
    0.5603 -> 0.7000
    0.9812 -> 1.0000
    0.5127 -> 0.6600
    0.4821 -> 0.6200

```



## K

Translation of the J solution:


```K

le:- 0.96 0.91 0.86 0.81 0.76 0.71 0.66 0.61 0.56 0.51 0.46 0.41 0.36 0.31 0.26 0.21 0.16 0.11 0.06 0.0
out: 1.00 0.98 0.94 0.90 0.86 0.82 0.78 0.74 0.70 0.66 0.62 0.58 0.54 0.50 0.44 0.38 0.32 0.26 0.18 0.1

pf:{out@_bin[le;-x]}'

```

{{out}}

```txt

   pf 0.6094701 0.5003597 0.8512954 0.08951883 0.6868076
0.7 0.62 0.9 0.18 0.78

```



## Kotlin


```scala
// version 1.0.6

fun rescale(price: Double): Double =
    when {
        price < 0.06 ->  0.10
        price < 0.11 ->  0.18
        price < 0.16 ->  0.26
        price < 0.21 ->  0.32
        price < 0.26 ->  0.38
        price < 0.31 ->  0.44
        price < 0.36 ->  0.50
        price < 0.41 ->  0.54
        price < 0.46 ->  0.58
        price < 0.51 ->  0.62
        price < 0.56 ->  0.66
        price < 0.61 ->  0.70
        price < 0.66 ->  0.74
        price < 0.71 ->  0.78
        price < 0.76 ->  0.82
        price < 0.81 ->  0.86
        price < 0.86 ->  0.90
        price < 0.91 ->  0.94
        price < 0.96 ->  0.98
        else         ->  1.00
    }

fun main(args: Array<String>) {
    var d: Double
    for (i in 1..100) {
        d = i / 100.0
        print(String.format("%4.2f -> %4.2f  ", d, rescale(d)))
        if (i % 5 == 0) println()
    }
}
```


{{out}}

```txt

0.01 -> 0.10  0.02 -> 0.10  0.03 -> 0.10  0.04 -> 0.10  0.05 -> 0.10
0.06 -> 0.18  0.07 -> 0.18  0.08 -> 0.18  0.09 -> 0.18  0.10 -> 0.18
0.11 -> 0.26  0.12 -> 0.26  0.13 -> 0.26  0.14 -> 0.26  0.15 -> 0.26
0.16 -> 0.32  0.17 -> 0.32  0.18 -> 0.32  0.19 -> 0.32  0.20 -> 0.32
0.21 -> 0.38  0.22 -> 0.38  0.23 -> 0.38  0.24 -> 0.38  0.25 -> 0.38
0.26 -> 0.44  0.27 -> 0.44  0.28 -> 0.44  0.29 -> 0.44  0.30 -> 0.44
0.31 -> 0.50  0.32 -> 0.50  0.33 -> 0.50  0.34 -> 0.50  0.35 -> 0.50
0.36 -> 0.54  0.37 -> 0.54  0.38 -> 0.54  0.39 -> 0.54  0.40 -> 0.54
0.41 -> 0.58  0.42 -> 0.58  0.43 -> 0.58  0.44 -> 0.58  0.45 -> 0.58
0.46 -> 0.62  0.47 -> 0.62  0.48 -> 0.62  0.49 -> 0.62  0.50 -> 0.62
0.51 -> 0.66  0.52 -> 0.66  0.53 -> 0.66  0.54 -> 0.66  0.55 -> 0.66
0.56 -> 0.70  0.57 -> 0.70  0.58 -> 0.70  0.59 -> 0.70  0.60 -> 0.70
0.61 -> 0.74  0.62 -> 0.74  0.63 -> 0.74  0.64 -> 0.74  0.65 -> 0.74
0.66 -> 0.78  0.67 -> 0.78  0.68 -> 0.78  0.69 -> 0.78  0.70 -> 0.78
0.71 -> 0.82  0.72 -> 0.82  0.73 -> 0.82  0.74 -> 0.82  0.75 -> 0.82
0.76 -> 0.86  0.77 -> 0.86  0.78 -> 0.86  0.79 -> 0.86  0.80 -> 0.86
0.81 -> 0.90  0.82 -> 0.90  0.83 -> 0.90  0.84 -> 0.90  0.85 -> 0.90
0.86 -> 0.94  0.87 -> 0.94  0.88 -> 0.94  0.89 -> 0.94  0.90 -> 0.94
0.91 -> 0.98  0.92 -> 0.98  0.93 -> 0.98  0.94 -> 0.98  0.95 -> 0.98
0.96 -> 1.00  0.97 -> 1.00  0.98 -> 1.00  0.99 -> 1.00  1.00 -> 1.00

```



## Liberty BASIC


```lb

dim DR(38)    'decimal range
dim PF(38)    'corresponding price fraction
range$="0.06 0.11 0.16 0.21 0.26 0.31 0.36 0.41 0.46 0.51 0.56 0.61 0.66 0.71 0.76 0.81 0.86 0.91 0.96 0.01"
frac$="0.10 0.18 0.26 0.32 0.38 0.44 0.50 0.54 0.58 0.62 0.66 0.70 0.74 0.78 0.82 0.86 0.90 0.94 0.98 1.00"
for i = 1 to 38
  DR(i)=val(word$(range$,i))
  PF(i)=val(word$(frac$,i))
next

for i = 0 to .99 step 0.03
  print i;" -> ";PriceFraction(i)
next
end

Function PriceFraction(n)
    PriceFraction=n  'return original if outside test bounds
    for i = 1 to 38
    if n<=DR(i) then
        PriceFraction=PF(i)
        exit for
    end if
    next
    end function

```



## Lua


```lua
scaleTable = {
    {0.06, 0.10}, {0.11, 0.18}, {0.16, 0.26}, {0.21, 0.32},
    {0.26, 0.38}, {0.31, 0.44}, {0.36, 0.50}, {0.41, 0.54},
    {0.46, 0.58}, {0.51, 0.62}, {0.56, 0.66}, {0.61, 0.70},
    {0.66, 0.74}, {0.71, 0.78}, {0.76, 0.82}, {0.81, 0.86},
    {0.86, 0.90}, {0.91, 0.94}, {0.96, 0.98}, {1.01, 1.00}
}

function rescale (price)
    if price < 0 or price > 1 then return "Out of range!" end
    for k, v in pairs(scaleTable) do
        if price < v[1] then return v[2] end
    end
end

math.randomseed(os.time())
for i = 1, 5 do
    rnd = math.random()
    print("Random value:", rnd)
    print("Adjusted price:", rescale(rnd))
    print()
end
```

{{out}}

```txt
Random value:   0.61946413522022
Adjusted price: 0.74

Random value:   0.81141947958698
Adjusted price: 0.9

Random value:   0.55691473099814
Adjusted price: 0.66

Random value:   0.19704311677601
Adjusted price: 0.32

Random value:   0.36528313938816
Adjusted price: 0.54
```



## Maple


```maple
priceFraction := proc(price)
	local values, standard, newPrice, i;
	values := [0, 0.06, 0.11, 0.16, 0.21, 0.26, 0.31, 0.36, 0.41, 0.46, 0.51, 0.56, 0.61,
			 0.66, 0.71, 0.76, 0.81, 0.86, 0.91, 0.96, 1.01];
	standard := [0.10, 0.18, 0.26, 0.32, 0.38, 0.44, 0.50, 0.54, 0.58, 0.62, 0.66, 0.70,
			   0.74, 0.78, 0.82, 0.86, 0.90, 0.94, 0.98, 1.00];
	for i to numelems(standard) do
		if price >= values[i] and price < values[i+1] then
			newPrice := standard[i];
		end if;
	end do;
	printf("%f --> %.2f\n", price, newPrice);
end proc:

randomize():
for i to 5 do
	priceFraction (rand(0.0..1.0)());
end do;
```

{{out}}

```txt
0.524386 --> 0.66
0.887957 --> 0.94
0.670196 --> 0.78
0.875601 --> 0.94
0.540447 --> 0.66
```



## Mathematica


```Mathematica
PriceFraction[x_]:=Piecewise[{{.1, 0 <= x < 0.06}, {.18, x < .11}, {.26,x < 0.16},
{.32, x < .21}, {.38, x < .26}, {.44, x < 0.31}, {.5, x < .36},
{.54, x < .41}, {.58, x < .46}, {.62, x < .51}, {.66, x < .56},
{.70, x < .61}, {.74, x < .66}, {.78, x < .71}, {.82, x < .76},
{.86, x < .81}, {.90, x < .86}, {.94, x < .91}, {.98, x < .96}}, 1]
```



=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
  function y = rescale(x)

     L = [0,.06:.05:1.02];
     V = [.1,.18,.26,.32,.38,.44,.50,.54,.58,.62,.66,.70,.74,.78,.82,.86,.9,.94,.98,1];

     y = x;
     for k=1:numel(x);
        y(k) = V(sum(L<=x(k)));
     end;
  end;

   t=0:0.001:1;
   plot(t,rescale(t));
```



## Mercury


```Mercury
:- module price.
:- interface.
:- import_module int.
:- type price == int.
:- func standard(price) = price.

:- implementation.
:- import_module require, list.

standard(P) = SP :-
        require(P >= 0, "P must be positive"),
        Cents = P `mod` 100,
        P + adjust(Cents) = SP.

:- func adjust(int) = int.
adjust(Cents) = adjust(Cents, rules).

:- func adjust(int, list(price_rule)) = int.
adjust(_, []) = unexpected("price", "adjust/2", "exhausted rules").
adjust(N, [rule(Low, High, To)|T]) = R :-
        ( N >= Low, N < High -> To - N = R ; adjust(N, T) = R ).

:- type price_rule ---> rule(int, int, int).
:- func rules = list(price_rule).
rules = [rule(00, 06, 10),
        rule(06, 11, 18),
        rule(11, 16, 26),
        rule(16, 21, 32),
        rule(21, 26, 38),
        rule(26, 31, 44),
        rule(31, 36, 50),
        rule(36, 41, 54),
        rule(41, 46, 58),
        rule(46, 51, 62),
        rule(51, 56, 66),
        rule(56, 61, 70),
        rule(61, 66, 74),
        rule(66, 71, 78),
        rule(71, 76, 82),
        rule(76, 81, 86),
        rule(81, 86, 90),
        rule(86, 91, 94),
        rule(91, 96, 98),
        rule(96, 101, 100)].
```


A build system might turn the text of the table into the definition of a hundred-element array of adjustments.  In that case,


```Mercury
adjust(Cents) = array.lookup(price_table, Cents).
```



## MUMPS


```MUMPS
PRICFRAC(X)
 ;Outputs a specified value dependent upon the input value
 ;The non-inclusive upper limits are encoded in the PFMAX string, and the values
 ;to convert to are encoded in the PFRES string.
 NEW PFMAX,PFRES,I,RESULT
 SET PFMAX=".06^.11^.16^.21^.26^.31^.36^.41^.46^.51^.56^.61^.66^.71^.76^.81^.86^.91^.96^1.01"
 SET PFRES=".10^.18^.26^.32^.38^.44^.50^.54^.58^.62^.66^.70^.74^.78^.82^.86^.90^.94^.98^1.00"
 Q:(X<0)!(X>1.01) ""
 FOR I=1:1:$LENGTH(PFMAX,"^") Q:($DATA(RESULT)'=0)  SET:X<$P(PFMAX,"^",I) RESULT=$P(PFRES,"^",I)
 KILL PFMAX,PFRES,I
 QUIT RESULT
```

{{out}}

```txt
USER>W $$PRICFRAC^ROSETTA(.04)
.10
USER>W $$PRICFRAC^ROSETTA(.06)
.18
USER>W $$PRICFRAC^ROSETTA(.40)
.54
USER>W $$PRICFRAC^ROSETTA(1.40)

USER>W $$PRICFRAC^ROSETTA(.81)
.90
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- -----------------------------------------------------------------------------
method runSample(arg) public static
  parse arg in_val .
  if in_val \= '' then test_vals = [in_val]
  else                 test_vals = getTestData()

  say 'Input Adjustment'
  loop tv = 0 to test_vals.length - 1
    in_val = test_vals[tv]
    adjust = priceFraction(in_val)
    say in_val.format(null, 2).right(5) adjust.format(null, 2).right(10)
    end tv

  return

-- -----------------------------------------------------------------------------
method priceFraction(in_val) public static
  out_val = -1
  limit_table = getLimitTable()
  limit_table_K = limit_table.length
  loop p1 = 0 to limit_table_K - 1
    pair = limit_table[p1]
    hi_limit = pair[0]
    adjustmt = pair[1]
    if in_val < hi_limit then do
      out_val = adjustmt
      leave p1
      end
    end p1
  if out_val = -1 then signal IllegalArgumentException('Input' in_val 'is outside of acceptable range.')

  return out_val

-- -----------------------------------------------------------------------------
method getLimitTable() public static returns Rexx[,]
  limit_table = [ -
    [0.06, 0.10], [0.11, 0.18], [0.16, 0.26], [0.21, 0.32], [0.26, 0.38], -
    [0.31, 0.44], [0.36, 0.50], [0.41, 0.54], [0.46, 0.58], [0.51, 0.62], -
    [0.56, 0.66], [0.61, 0.70], [0.66, 0.74], [0.71, 0.78], [0.76, 0.82], -
    [0.81, 0.86], [0.86, 0.90], [0.91, 0.94], [0.96, 0.98], [1.01, 1.00]  -
  ]
  return limit_table

-- -----------------------------------------------------------------------------
method getTestData() private static returns Rexx[]
  test_vals = Rexx[5]
  rng = Random(1024)
  loop tv = 0 to test_vals.length - 1
    test_vals[tv] = rng.nextFloat()
    end tv
  return test_vals

```

{{out}}

```txt

Input Adjustment
 0.64       0.74
 0.32       0.50
 0.85       0.90
 0.93       0.98
 0.62       0.74

```



## Nim


```nim
import strutils, math

const
  pricemap: array[0 .. 19, int] = [10,18,26,32,38,44,50,54,58,62,66,70,74,78,82,86,90,94,98,100]

# outputs an int (=>float*100)
proc floatToPrice100(f: float): int =
    # indx: 0.1-0.05->0, 0.06-0.10->1, 0.11-0.15->2, .....
    var valu: int = toInt(f*100)
    if valu == 0:
        result = 10
    else:
        dec(valu)
        # inc indx every 5 of valu, so value of 1..100 translates to indx of 0..19
        var indx: int = 2*int(valu/10)+int((valu%%10)/5)
        result = pricemap[indx]

# str representation of an int (that is a representation of a float price)
proc price100ToStr(p: int): string =
    if p < 10:
       result = "0.0" & $p
    if p < 100:
       result = "0." & $p
    else:
       result = "1.00"

randomize()
var i: int = 0

for x in 0 .. 10:
   i = random(101)
   echo("Price for ", i.price100ToStr(), ", is: ", float(i/100).floatToPrice100().price100ToStr())
```

{{out}}
A random output something like:

```txt
Price for 0.73, is: 0.82
Price for 0.29, is: 0.44
Price for 0.25, is: 0.38
Price for 0.52, is: 0.66
Price for 0.66, is: 0.78
Price for 0.23, is: 0.38
Price for 0.62, is: 0.74
Price for 0.26, is: 0.44
Price for 0.70, is: 0.78
Price for 0.69, is: 0.78
Price for 0.39, is: 0.54
```



## Objeck

{{trans|C#}}

```objeck
class PriceFraction {
  function : Main(args : String[]) ~ Nil {
    for(i := 0; i < 5; i++;) {
      f := Float->Random();
      r := SpecialRound(f);
      "{$f} -> {$r}"->PrintLine();
    };
  }

  function : SpecialRound(inValue : Float) ~ Float {
    if (inValue > 1) {
      return 1;
    };

    splitters := [
      0.00 , 0.06 , 0.11 , 0.16 , 0.21 ,
      0.26 , 0.31 , 0.36 , 0.41 , 0.46 ,
      0.51 , 0.56 , 0.61 , 0.66 , 0.71 ,
      0.76 , 0.81 , 0.86 , 0.91 , 0.96 ];

    replacements := [
      0.10 , 0.18 , 0.26 , 0.32 , 0.38 ,
      0.44 , 0.50 , 0.54 , 0.58 , 0.62 ,
      0.66 , 0.70 , 0.74 , 0.78 , 0.82 ,
      0.86 , 0.90 , 0.94 , 0.98 , 1.00 ];

    for(x := 0; x < splitters->Size() - 1; x+=1;) {
      if (inValue >= splitters[x] & inValue < splitters[x + 1]) {
        return replacements[x];
      };
    };

    return inValue;
  }
}
```


{{output}}

```txt

0.317901 -> 0.5
0.691109 -> 0.78
0.790891 -> 0.86
0.269922 -> 0.44
0.690891 -> 0.78

```



## OCaml



```ocaml
let price_fraction v =
  if v < 0.0 || v >= 1.01 then
    invalid_arg "price_fraction";
  let rec aux = function
  | (x,r)::tl ->
      if v < x then r
      else aux tl
  | [] -> assert false
  in
  aux [
    0.06, 0.10;   0.11, 0.18;   0.16, 0.26;   0.21, 0.32;   0.26, 0.38;
    0.31, 0.44;   0.36, 0.50;   0.41, 0.54;   0.46, 0.58;   0.51, 0.62;
    0.56, 0.66;   0.61, 0.70;   0.66, 0.74;   0.71, 0.78;   0.76, 0.82;
    0.81, 0.86;   0.86, 0.90;   0.91, 0.94;   0.96, 0.98;   1.01, 1.00;
  ]
```



```ocaml
let () =
  let ok_tests = [
    (0.3793, 0.54);
    (0.4425, 0.58);
    (0.0746, 0.18);
    (0.6918, 0.78);
    (0.2993, 0.44);
    (0.5486, 0.66);
    (0.7848, 0.86);
    (0.9383, 0.98);
    (0.2292, 0.38);
  ] in
  Printf.printf " input   res   ok\n";
  List.iter (fun (v,ok) ->
    let r = price_fraction v in
    Printf.printf " %6g  %g  %b\n" v r (r = ok);
  ) ok_tests;
;;
```



## Oforth



```oforth
[.06, .11, .16, .21, .26, .31, .36, .41, .46, .51, .56, .61, .66, .71, .76, .81, .86, .91, .96, 1.01] const: IN
[.10, .18, .26, .32, .38, .44, .50, .54, .58, .62, .66, .70, .74, .78, .82, .86, .90, .94, .98, 1.00] const: OUT

: priceFraction(f)
| i |
   IN size loop: i [ f IN at(i) < ifTrue: [ OUT at(i) return ] ]
   null ;
```


{{Out}}

```txt

>[0.7388727, 0.8593103, 0.826687, 0.3444635] map(#priceFraction) .
[0.82, 0.9, 0.9, 0.5] ok

```



## Oz

Using a for-loop with return and a default value for values >= 1.01.
For out-of-range input, a "failed value" is returned,
i.e. a value that throws an exception when it is accessed.


```oz
fun {PriceFraction X}
   OutOfRange = {Value.failed outOfRange(X)}
in
   for Limit#Result in
      [0.00#OutOfRange
       0.06#0.10 0.11#0.18 0.16#0.26 0.21#0.32 0.26#0.38 0.31#0.44 0.36#0.5
       0.41#0.54 0.46#0.58 0.51#0.62 0.56#0.66 0.61#0.70 0.66#0.74 0.71#0.78
       0.76#0.82 0.81#0.86 0.86#0.90 0.91#0.94 0.96#0.98 1.01#1.00
      ]
      return:Return
      default:OutOfRange
   do
      if X < Limit then {Return Result} end
   end
end
```



## PARI/GP


```parigp
priceLookup=[6,11,16,21,26,31,41,46,51,56,61,66,71,76,81,86,91,96,101];
priceReplace=[10,18,26,32,38,44,50,54,58,62,66,70,74,78,82,86,90,94,98,100];
pf(x)={
  x*=100;
  for(i=1,19,
    if(x<priceLookup[i], return(priceReplace[i]))
  );
  "nasal demons"
};
```



## Pascal


```pascal
Program PriceFraction(output);

const
  limit: array [1..20] of real =
           (0.06, 0.11, 0.16, 0.21, 0.26, 0.31, 0.36, 0.41, 0.46, 0.51,
            0.56, 0.61, 0.66, 0.71, 0.76, 0.81, 0.86, 0.91, 0.96, 1.01);
  price: array [1..20] of real =
           (0.10, 0.18, 0.26, 0.32, 0.38, 0.44, 0.50, 0.54, 0.58, 0.62,
            0.66, 0.70, 0.74, 0.78, 0.81, 0.86, 0.90, 0.94, 0.98, 1.00);

var
  cost: real;
  i, j: integer;

begin
  randomize;
  for i := 1 to 10 do
  begin
    cost := random;
    j := high(limit);
    while cost < limit[j] do
      dec(j);
    writeln (cost:6:4, ' -> ', price[j+1]:4:2);
  end;
end.
```

{{out}}

```txt
% ./PriceFraction
0.8145 -> 0.90
0.6347 -> 0.74
0.0464 -> 0.10
0.9603 -> 1.00
0.3629 -> 0.54
0.5074 -> 0.62
0.4516 -> 0.58
0.2340 -> 0.38
0.4142 -> 0.58
0.8327 -> 0.90
```



## Perl


```Perl
my @table = map [ /([\d\.]+)/g ], split "\n", <<'TBL';
>=  0.00  <  0.06  :=  0.10
>=  0.06  <  0.11  :=  0.18
>=  0.11  <  0.16  :=  0.26
>=  0.16  <  0.21  :=  0.32
>=  0.21  <  0.26  :=  0.38
>=  0.26  <  0.31  :=  0.44
>=  0.31  <  0.36  :=  0.50
>=  0.36  <  0.41  :=  0.54
>=  0.41  <  0.46  :=  0.58
>=  0.46  <  0.51  :=  0.62
>=  0.51  <  0.56  :=  0.66
>=  0.56  <  0.61  :=  0.70
>=  0.61  <  0.66  :=  0.74
>=  0.66  <  0.71  :=  0.78
>=  0.71  <  0.76  :=  0.82
>=  0.76  <  0.81  :=  0.86
>=  0.81  <  0.86  :=  0.90
>=  0.86  <  0.91  :=  0.94
>=  0.91  <  0.96  :=  0.98
>=  0.96  <  1.01  :=  1.00
TBL

sub convert {
        my $money = shift;
        for (@table) {
                return $_->[2] if $_->[0] <= $money and $_->[1] > $money
        }
        die "Can't find currency conversion for $money. Counterfeit?"
}

# try it out
for (1 .. 10) {
        my $m = rand(1);
        printf "%.3f -> %g\n", $m, convert($m);
}

```



## Perl 6


Simple solution, doing a linear search.

Note that in Perl 6 we don't have to worry about floating-point misrepresentations of decimals, because decimal fractions are stored as rationals.

{{works with|rakudo|2016.07}}

```perl6
sub price-fraction ($n where 0..1) {
    when $n < 0.06 { 0.10 }
    when $n < 0.11 { 0.18 }
    when $n < 0.16 { 0.26 }
    when $n < 0.21 { 0.32 }
    when $n < 0.26 { 0.38 }
    when $n < 0.31 { 0.44 }
    when $n < 0.36 { 0.50 }
    when $n < 0.41 { 0.54 }
    when $n < 0.46 { 0.58 }
    when $n < 0.51 { 0.62 }
    when $n < 0.56 { 0.66 }
    when $n < 0.61 { 0.70 }
    when $n < 0.66 { 0.74 }
    when $n < 0.71 { 0.78 }
    when $n < 0.76 { 0.82 }
    when $n < 0.81 { 0.86 }
    when $n < 0.86 { 0.90 }
    when $n < 0.91 { 0.94 }
    when $n < 0.96 { 0.98 }
    default        { 1.00 }
}

while prompt("value: ") -> $value {
    say price-fraction(+$value);
}
```


If we expect to rescale many prices, a better approach would be to build a look-up array of 101 entries.
Memory is cheap, and array indexing is blazing fast.


```perl6
my @price = map *.value, flat
    ( 0 ..^ 6  X=> 0.10),
    ( 6 ..^ 11 X=> 0.18),
    (11 ..^ 16 X=> 0.26),
    (16 ..^ 21 X=> 0.32),
    (21 ..^ 26 X=> 0.38),
    (26 ..^ 31 X=> 0.44),
    (31 ..^ 36 X=> 0.50),
    (36 ..^ 41 X=> 0.54),
    (41 ..^ 46 X=> 0.58),
    (46 ..^ 51 X=> 0.62),
    (51 ..^ 56 X=> 0.66),
    (56 ..^ 61 X=> 0.70),
    (61 ..^ 66 X=> 0.74),
    (66 ..^ 71 X=> 0.78),
    (71 ..^ 76 X=> 0.82),
    (76 ..^ 81 X=> 0.86),
    (81 ..^ 86 X=> 0.90),
    (86 ..^ 91 X=> 0.94),
    (91 ..^ 96 X=> 0.98),
    (96 ..^101 X=> 1.00),
;

while prompt("value: ") -> $value {
    say @price[$value * 100] // "Out of range";
}
```


We can also build this same look-up array by parsing the table as formatted in the task description:

{{works with|rakudo|2016.07}}

```perl6
my $table = q:to/END/;
>=  0.00  <  0.06  :=  0.10
>=  0.06  <  0.11  :=  0.18
>=  0.11  <  0.16  :=  0.26
>=  0.16  <  0.21  :=  0.32
>=  0.21  <  0.26  :=  0.38
>=  0.26  <  0.31  :=  0.44
>=  0.31  <  0.36  :=  0.50
>=  0.36  <  0.41  :=  0.54
>=  0.41  <  0.46  :=  0.58
>=  0.46  <  0.51  :=  0.62
>=  0.51  <  0.56  :=  0.66
>=  0.56  <  0.61  :=  0.70
>=  0.61  <  0.66  :=  0.74
>=  0.66  <  0.71  :=  0.78
>=  0.71  <  0.76  :=  0.82
>=  0.76  <  0.81  :=  0.86
>=  0.81  <  0.86  :=  0.90
>=  0.86  <  0.91  :=  0.94
>=  0.91  <  0.96  :=  0.98
>=  0.96  <  1.01  :=  1.00
END

my @price;

for $table.lines {
    /:s '>='  (\S+)  '<'  (\S+)  ':='  (\S+)/;
    @price[$0*100 ..^ $1*100] »=» +$2;
}

while prompt("value: ") -> $value {
    say @price[$value * 100] // "Out of range";
}
```



## Phix


```Phix
constant TBL=split("""
>=  0.00  <  0.06  :=  0.10
>=  0.06  <  0.11  :=  0.18
>=  0.11  <  0.16  :=  0.26
>=  0.16  <  0.21  :=  0.32
>=  0.21  <  0.26  :=  0.38
>=  0.26  <  0.31  :=  0.44
>=  0.31  <  0.36  :=  0.50
>=  0.36  <  0.41  :=  0.54
>=  0.41  <  0.46  :=  0.58
>=  0.46  <  0.51  :=  0.62
>=  0.51  <  0.56  :=  0.66
>=  0.56  <  0.61  :=  0.70
>=  0.61  <  0.66  :=  0.74
>=  0.66  <  0.71  :=  0.78
>=  0.71  <  0.76  :=  0.82
>=  0.76  <  0.81  :=  0.86
>=  0.81  <  0.86  :=  0.90
>=  0.86  <  0.91  :=  0.94
>=  0.91  <  0.96  :=  0.98
>=  0.96  <  1.01  :=  1.00""",'\n')

sequence limits = {0},
         prices = {-1}
atom lt,price
for i=1 to length(TBL) do
    {{?,lt,price}} = scanf(TBL[i],">=  %.2f  <  %.2f  :=  %.2f")
    limits = append(limits,lt)
    prices = append(prices,price)
end for

function price_fix(atom p)
    for i=1 to length(limits) do
        if p<limits[i] then
            return prices[i]
        end if
    end for
    return -1
end function

for i=-1 to 101 do
    printf(1, "%5.2f %5.2f\n", {i/100,price_fix(i/100)})
end for
```



## PicoLisp


```PicoLisp
(scl 2)

(de price (Pr)
   (format
      (cdr
         (rank Pr
            (quote
               (0.00 . 0.10)
               (0.06 . 0.18)
               (0.11 . 0.26)
               (0.16 . 0.32)
               (0.21 . 0.38)
               (0.26 . 0.44)
               (0.31 . 0.50)
               (0.36 . 0.54)
               (0.41 . 0.58)
               (0.46 . 0.62)
               (0.51 . 0.66)
               (0.56 . 0.70)
               (0.61 . 0.74)
               (0.66 . 0.78)
               (0.71 . 0.82)
               (0.76 . 0.86)
               (0.81 . 0.90)
               (0.86 . 0.94)
               (0.91 . 0.98)
               (0.96 . 1.00) ) ) )
      *Scl ) )

(for N (0.3793 0.4425 0.0746 0.6918 0.2993 0.5486 0.7848 0.9383 0.2292)
   (prinl (price N)) )
```

{{out}}

```txt
0.54
0.58
0.18
0.78
0.44
0.66
0.86
0.98
0.38
```



## PL/I


### version 1


```PL/I
declare t(20) fixed decimal (3,2) static initial (
   .06, .11, .16, .21, .26, .31, .36, .41, .46,  .51,
   .56, .61, .66, .71, .76, .81, .86, .91, .96, 1.01);
declare r(20) fixed decimal (3,2) static initial (
   .10, .18, .26, .32, .38, .44, .50, .54, .58, .62,
   .66, .70, .74, .78, .82, .86, .90, .94, .98, 1);
declare x float, d fixed decimal (3,2);
declare i fixed binary;

loop:
   do i = 1 to 20;
      if x < t(i) then
         do; d = r(i); leave loop; end;
   end;
```



### version 2

{{trans|REXX version2}}

```PL/I
cpt: Proc Options(main);
 Dcl x Dec Fixed(4,2);
 Do x=0 To 1 By 0.01;
   Put Edit(x,' -> ',cp(x))(Skip,f(4,2),a,f(4,2));
   End;
 cp: Proc(p) Returns(Dec Fixed(4,2));
 Dcl r(20) Dec Fixed(4,2) static init(
   .10, .18, .26, .32, .38, .44, .50, .54, .58, .62,
   .66, .70, .74, .78, .82, .86, .90, .94, .98, 1);
 Dcl p Dec Fixed(4,2);
 Dcl i Bin Fixed;
 i=trunc((100*p-1)/5)+1;
 Return(r(i));
 End;
 End;
```



## PowerShell


```PowerShell

function Convert-PriceFraction
{
    [CmdletBinding()]
    [OutputType([double])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [ValidateScript({$_ -ge 0.0 -and $_ -le 1.0})]
        [double]
        $InputObject
    )

    Process
    {
        foreach ($fraction in $InputObject)
        {
            switch ($fraction)
            {
                {$_ -lt 0.06} {0.10; break}
                {$_ -lt 0.11} {0.18; break}
                {$_ -lt 0.16} {0.26; break}
                {$_ -lt 0.21} {0.32; break}
                {$_ -lt 0.26} {0.38; break}
                {$_ -lt 0.31} {0.44; break}
                {$_ -lt 0.36} {0.50; break}
                {$_ -lt 0.41} {0.54; break}
                {$_ -lt 0.46} {0.58; break}
                {$_ -lt 0.51} {0.62; break}
                {$_ -lt 0.56} {0.66; break}
                {$_ -lt 0.61} {0.70; break}
                {$_ -lt 0.66} {0.74; break}
                {$_ -lt 0.71} {0.78; break}
                {$_ -lt 0.76} {0.82; break}
                {$_ -lt 0.81} {0.86; break}
                {$_ -lt 0.86} {0.90; break}
                {$_ -lt 0.91} {0.94; break}
                {$_ -lt 0.96} {0.98; break}
                Default       {1.00}
            }
        }
    }
}

```


```PowerShell

.7388727, .8593103, .826687, .3444635, .0491907 | Convert-PriceFraction | ForEach-Object {"{0:C}" -f $_}

```

{{Out}}

```txt

$0.82
$0.90
$0.90
$0.50
$0.10

```



## PureBasic


```PureBasic
Procedure.f PriceFraction(price.f)
  ;returns price unchanged if value is invalid
  Protected fraction
  Select price * 100
    Case 0 To 5
      fraction = 10
    Case 06 To 10
      fraction = 18
    Case 11 To 15
      fraction = 26
    Case 16 To 20
      fraction = 32
    Case 21 To 25
      fraction = 38
    Case 26 To 30
      fraction = 44
    Case 31 To 35
      fraction = 5
    Case 36 To 40
      fraction = 54
    Case 41 To 45
      fraction = 58
    Case 46 To 50
      fraction = 62
    Case 51 To 55
      fraction = 66
    Case 56 To 60
      fraction = 7
    Case 61 To 65
      fraction = 74
    Case 66 To 70
      fraction = 78
    Case 71 To 75
      fraction = 82
    Case 76 To 80
      fraction = 86
    Case 81 To 85
      fraction = 9
    Case 86 To 90
      fraction = 94
    Case 91 To 95
      fraction = 98
    Case 96 To 100
      fraction = 100
    Default
      ProcedureReturn price
  EndSelect

  ProcedureReturn fraction / 100
EndProcedure

If OpenConsole()
  Define x.f, i

  For i = 1 To 10
    x = Random(10000)/10000
    PrintN(StrF(x, 4) + " -> " + StrF(PriceFraction(x), 2))
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
0.3793 -> 0.54
0.4425 -> 0.58
0.0746 -> 0.18
0.6918 -> 0.78
0.2993 -> 0.44
0.5486 -> 0.66
0.7848 -> 0.86
0.9383 -> 0.98
0.2292 -> 0.38
0.9560 -> 1.00
```



## Python

Using the [http://docs.python.org/library/bisect.html bisect] standard module to reduce the comparisons with members of the cin array.


```python>>>
 import bisect
>>> _cin  = [.06, .11, .16, .21, .26, .31, .36, .41, .46, .51, .56, .61, .66, .71, .76, .81, .86, .91, .96, 1.01]
>>> _cout = [.10, .18, .26, .32, .38, .44, .50, .54, .58, .62, .66, .70, .74, .78, .82, .86, .90, .94, .98, 1.00]
>>> def pricerounder(pricein):
	return _cout[ bisect.bisect_right(_cin, pricein) ]
```


When dealing with money it is good to think about possible loss of precision. If we change the units to be integer cents we could use the following exact routine:

```python>>>
 import bisect
>>> _cin  = [ 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 81, 86, 91, 96, 101]
>>> _cout = [10, 18, 26, 32, 38, 44, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86, 90, 94, 98, 100]
>>> def centsrounder(centsin):
	return _cout[ bisect.bisect_right(_cin, centsin) ]
```

Other options are to use the fractions or decimals modules for calculating money to a known precision.


'''Bisection library code'''

:The <code>bisect</code> Python standard library function uses the following code that improves on a simple linear scan through a sorted list:
:
```python
def bisect_right(a, x, lo=0, hi=None):
    """Return the index where to insert item x in list a, assuming a is sorted.

    The return value i is such that all e in a[:i] have e <= x, and all e in
    a[i:] have e > x.  So if x already appears in the list, a.insert(x) will
    insert just after the rightmost x already there.

    Optional args lo (default 0) and hi (default len(a)) bound the
    slice of a to be searched.
    """

    if lo < 0:
        raise ValueError('lo must be non-negative')
    if hi is None:
        hi = len(a)
    while lo < hi:
        mid = (lo+hi)//2
        if x < a[mid]: hi = mid
        else: lo = mid+1
    return lo
```



## R


```r

price_fraction <- function(x)
{
  stopifnot(all(x >= 0 & x <= 1))
  breaks <- seq(0.06, 1.01, 0.05)
  values <- c(.1, .18, .26, .32, .38, .44, .5, .54, .58, .62, .66, .7, .74, .78, .82, .86, .9, .94, .98, 1)
  indices <- sapply(x, function(x) which(x < breaks)[1])
  values[indices]
}

#Example usage:
price_fraction(c(0, .01, 0.06, 0.25, 1))                # 0.10 0.10 0.18 0.38 1.00

```


You can extract the contents of the table as follows:


```r

dfr <- read.table(tc <- textConnection(
">=  0.00  <  0.06  :=  0.10
>=  0.06  <  0.11  :=  0.18
>=  0.11  <  0.16  :=  0.26
>=  0.16  <  0.21  :=  0.32
>=  0.21  <  0.26  :=  0.38
>=  0.26  <  0.31  :=  0.44
>=  0.31  <  0.36  :=  0.50
>=  0.36  <  0.41  :=  0.54
>=  0.41  <  0.46  :=  0.58
>=  0.46  <  0.51  :=  0.62
>=  0.51  <  0.56  :=  0.66
>=  0.56  <  0.61  :=  0.70
>=  0.61  <  0.66  :=  0.74
>=  0.66  <  0.71  :=  0.78
>=  0.71  <  0.76  :=  0.82
>=  0.76  <  0.81  :=  0.86
>=  0.81  <  0.86  :=  0.90
>=  0.86  <  0.91  :=  0.94
>=  0.91  <  0.96  :=  0.98
>=  0.96  <  1.01  :=  1.00")); close(tc)
breaks <- dfr$V4
values <- dfr$V6

```



## Racket



```Racket

#lang racket

(define table
  '([0 #f]
    [0.06 0.10] [0.11 0.18] [0.16 0.26] [0.21 0.32] [0.26 0.38] [0.31 0.44]
    [0.36 0.50] [0.41 0.54] [0.46 0.58] [0.51 0.62] [0.56 0.66] [0.61 0.70]
    [0.66 0.74] [0.71 0.78] [0.76 0.82] [0.81 0.86] [0.86 0.90] [0.91 0.94]
    [0.96 0.98] [1.01 1.00])

  ;; returns #f for negatives or values >= 1.01
(define (convert x) (for/or ([c table]) (and (< x (car c)) (cadr c))))

```



## Raven

{{trans|JavaScript}}

```Raven
define getScaleFactor use $v
   [ 0.1 0.18 0.26 0.32 0.38 0.44 0.50 0.54 0.58 0.62 0.66 0.70 0.74 0.78 0.82 0.86 0.90 0.94 0.98 1.0 ] as $vals
   $v 100 * 1 - 5 /    20 min  0 max  1 prefer dup $v "val: %g  indx: %d\n" print   $vals swap get

0 100 9 range each
    100.0 / dup getScaleFactor swap "%.2g -> %.2g\n" print
```

{{out}}

```txt
0 -> 0.1
0.09 -> 0.18
0.18 -> 0.32
0.27 -> 0.44
0.36 -> 0.54
0.45 -> 0.58
0.54 -> 0.66
0.63 -> 0.74
0.72 -> 0.82
0.81 -> 0.9
0.9 -> 0.94
0.99 -> 1

```



## REXX


### version 1


```rexx
/*REXX program to  rescale a  (decimal fraction)  price (0.99 ──► 1.00).*/
pad='     '                            /*for inserting spaces into msg. */
      do j=0  to 1  by .01;        if j==0 then j=0.00   /*special case.*/
      say pad 'original price ──►' j pad adjPrice(j) " ◄── adjusted price"
      end   /*j*/
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────ADJPRICE subroutine─────────────────*/
adjPrice:  procedure;   parse arg ?
                select
                when ?<0.06  then ?=0.10
                when ?<0.11  then ?=0.18
                when ?<0.16  then ?=0.26
                when ?<0.21  then ?=0.32
                when ?<0.26  then ?=0.38
                when ?<0.31  then ?=0.44
                when ?<0.36  then ?=0.50
                when ?<0.41  then ?=0.54
                when ?<0.46  then ?=0.58
                when ?<0.51  then ?=0.62
                when ?<0.56  then ?=0.66
                when ?<0.61  then ?=0.70
                when ?<0.66  then ?=0.74
                when ?<0.71  then ?=0.78
                when ?<0.76  then ?=0.82
                when ?<0.81  then ?=0.86
                when ?<0.86  then ?=0.90
                when ?<0.91  then ?=0.94
                when ?<0.96  then ?=0.98
                when ?<1.01  then ?=1.00
                otherwise    nop
                end   /*select*/
return ?
```

{{out}}
<pre style="height:30ex">
      original price ──► 0.00       0.10  ◄── adjusted price
      original price ──► 0.01       0.10  ◄── adjusted price
      original price ──► 0.02       0.10  ◄── adjusted price
      original price ──► 0.03       0.10  ◄── adjusted price
      original price ──► 0.04       0.10  ◄── adjusted price
      original price ──► 0.05       0.10  ◄── adjusted price
      original price ──► 0.06       0.18  ◄── adjusted price
      original price ──► 0.07       0.18  ◄── adjusted price
      original price ──► 0.08       0.18  ◄── adjusted price
      original price ──► 0.09       0.18  ◄── adjusted price
      original price ──► 0.10       0.18  ◄── adjusted price
      original price ──► 0.11       0.26  ◄── adjusted price
      original price ──► 0.12       0.26  ◄── adjusted price
      original price ──► 0.13       0.26  ◄── adjusted price
      original price ──► 0.14       0.26  ◄── adjusted price
      original price ──► 0.15       0.26  ◄── adjusted price
      original price ──► 0.16       0.32  ◄── adjusted price
      original price ──► 0.17       0.32  ◄── adjusted price
      original price ──► 0.18       0.32  ◄── adjusted price
      original price ──► 0.19       0.32  ◄── adjusted price
      original price ──► 0.20       0.32  ◄── adjusted price
      original price ──► 0.21       0.38  ◄── adjusted price
      original price ──► 0.22       0.38  ◄── adjusted price
      original price ──► 0.23       0.38  ◄── adjusted price
      original price ──► 0.24       0.38  ◄── adjusted price
      original price ──► 0.25       0.38  ◄── adjusted price
      original price ──► 0.26       0.44  ◄── adjusted price
      original price ──► 0.27       0.44  ◄── adjusted price
      original price ──► 0.28       0.44  ◄── adjusted price
      original price ──► 0.29       0.44  ◄── adjusted price
      original price ──► 0.30       0.44  ◄── adjusted price
      original price ──► 0.31       0.50  ◄── adjusted price
      original price ──► 0.32       0.50  ◄── adjusted price
      original price ──► 0.33       0.50  ◄── adjusted price
      original price ──► 0.34       0.50  ◄── adjusted price
      original price ──► 0.35       0.50  ◄── adjusted price
      original price ──► 0.36       0.54  ◄── adjusted price
      original price ──► 0.37       0.54  ◄── adjusted price
      original price ──► 0.38       0.54  ◄── adjusted price
      original price ──► 0.39       0.54  ◄── adjusted price
      original price ──► 0.40       0.54  ◄── adjusted price
      original price ──► 0.41       0.58  ◄── adjusted price
      original price ──► 0.42       0.58  ◄── adjusted price
      original price ──► 0.43       0.58  ◄── adjusted price
      original price ──► 0.44       0.58  ◄── adjusted price
      original price ──► 0.45       0.58  ◄── adjusted price
      original price ──► 0.46       0.62  ◄── adjusted price
      original price ──► 0.47       0.62  ◄── adjusted price
      original price ──► 0.48       0.62  ◄── adjusted price
      original price ──► 0.49       0.62  ◄── adjusted price
      original price ──► 0.50       0.62  ◄── adjusted price
      original price ──► 0.51       0.66  ◄── adjusted price
      original price ──► 0.52       0.66  ◄── adjusted price
      original price ──► 0.53       0.66  ◄── adjusted price
      original price ──► 0.54       0.66  ◄── adjusted price
      original price ──► 0.55       0.66  ◄── adjusted price
      original price ──► 0.56       0.70  ◄── adjusted price
      original price ──► 0.57       0.70  ◄── adjusted price
      original price ──► 0.58       0.70  ◄── adjusted price
      original price ──► 0.59       0.70  ◄── adjusted price
      original price ──► 0.60       0.70  ◄── adjusted price
      original price ──► 0.61       0.74  ◄── adjusted price
      original price ──► 0.62       0.74  ◄── adjusted price
      original price ──► 0.63       0.74  ◄── adjusted price
      original price ──► 0.64       0.74  ◄── adjusted price
      original price ──► 0.65       0.74  ◄── adjusted price
      original price ──► 0.66       0.78  ◄── adjusted price
      original price ──► 0.67       0.78  ◄── adjusted price
      original price ──► 0.68       0.78  ◄── adjusted price
      original price ──► 0.69       0.78  ◄── adjusted price
      original price ──► 0.70       0.78  ◄── adjusted price
      original price ──► 0.71       0.82  ◄── adjusted price
      original price ──► 0.72       0.82  ◄── adjusted price
      original price ──► 0.73       0.82  ◄── adjusted price
      original price ──► 0.74       0.82  ◄── adjusted price
      original price ──► 0.75       0.82  ◄── adjusted price
      original price ──► 0.76       0.86  ◄── adjusted price
      original price ──► 0.77       0.86  ◄── adjusted price
      original price ──► 0.78       0.86  ◄── adjusted price
      original price ──► 0.79       0.86  ◄── adjusted price
      original price ──► 0.80       0.86  ◄── adjusted price
      original price ──► 0.81       0.90  ◄── adjusted price
      original price ──► 0.82       0.90  ◄── adjusted price
      original price ──► 0.83       0.90  ◄── adjusted price
      original price ──► 0.84       0.90  ◄── adjusted price
      original price ──► 0.85       0.90  ◄── adjusted price
      original price ──► 0.86       0.94  ◄── adjusted price
      original price ──► 0.87       0.94  ◄── adjusted price
      original price ──► 0.88       0.94  ◄── adjusted price
      original price ──► 0.89       0.94  ◄── adjusted price
      original price ──► 0.90       0.94  ◄── adjusted price
      original price ──► 0.91       0.98  ◄── adjusted price
      original price ──► 0.92       0.98  ◄── adjusted price
      original price ──► 0.93       0.98  ◄── adjusted price
      original price ──► 0.94       0.98  ◄── adjusted price
      original price ──► 0.95       0.98  ◄── adjusted price
      original price ──► 0.96       1.00  ◄── adjusted price
      original price ──► 0.97       1.00  ◄── adjusted price
      original price ──► 0.98       1.00  ◄── adjusted price
      original price ──► 0.99       1.00  ◄── adjusted price
      original price ──► 1.00       1.00  ◄── adjusted price

```



### version 2


```rexx
/* REXX ***************************************************************
* Inspired by some other solutions tested with version 1 (above)
* 20.04.2013 Walter Pachl
* 03.11.2013 -"- move r. computation (once is enough)
**********************************************************************/
rl='0.10 0.18 0.26 0.32 0.38 0.44 0.50 0.54 0.58 0.62',
   '0.66 0.70 0.74 0.78 0.82 0.86 0.90 0.94 0.98 1.00'
Do i=1 To 20
  Parse Var rl r.i rl
  End
Do x=0 To 1 By 0.01
  old=adjprice(x)
  new=adjprice2(x)
  If old<>new Then tag='??'
  else tag=''
  Say x old new  tag
  End
Exit

adjprice2: Procedure Expose r.
  i=((100*arg(1)-1)%5+1)
  Return r.i
```



## Ring


```ring

see pricefraction(0.5)

func pricefraction n
     if n < 0.06 return 0.10 ok
     if n < 0.11 return 0.18 ok
     if n < 0.16 return 0.26 ok
     if n < 0.21 return 0.32 ok
     if n < 0.26 return 0.38 ok
     if n < 0.31 return 0.44 ok
     if n < 0.36 return 0.50 ok
     if n < 0.41 return 0.54 ok
     if n < 0.46 return 0.58 ok
     if n < 0.51 return 0.62 ok
     if n < 0.56 return 0.66 ok
     if n < 0.61 return 0.70 ok
     if n < 0.66 return 0.74 ok
     if n < 0.71 return 0.78 ok
     if n < 0.76 return 0.82 ok
     if n < 0.81 return 0.86 ok
     if n < 0.86 return 0.90 ok
     if n < 0.91 return 0.94 ok
     if n < 0.96 return 0.98 ok
     return 1


```



## Ruby

A simple function with hardcoded values.

```ruby
def rescale_price_fraction(value)
  raise ArgumentError, "value=#{value}, must have: 0 <= value < 1.01" if value < 0 || value >= 1.01
  if     value < 0.06  then  0.10
  elsif  value < 0.11  then  0.18
  elsif  value < 0.16  then  0.26
  elsif  value < 0.21  then  0.32
  elsif  value < 0.26  then  0.38
  elsif  value < 0.31  then  0.44
  elsif  value < 0.36  then  0.50
  elsif  value < 0.41  then  0.54
  elsif  value < 0.46  then  0.58
  elsif  value < 0.51  then  0.62
  elsif  value < 0.56  then  0.66
  elsif  value < 0.61  then  0.70
  elsif  value < 0.66  then  0.74
  elsif  value < 0.71  then  0.78
  elsif  value < 0.76  then  0.82
  elsif  value < 0.81  then  0.86
  elsif  value < 0.86  then  0.90
  elsif  value < 0.91  then  0.94
  elsif  value < 0.96  then  0.98
  elsif  value < 1.01  then  1.00
  end
end
```


Or, where we can cut and paste the textual table in one place

{{works with|Ruby|1.8.7+}} for the <code>String#lines</code> method.
For Ruby 1.8.6, use <code>String#each_line</code>


```ruby
class Price
  ConversionTable = <<-END_OF_TABLE
    >=  0.00  <  0.06  :=  0.10
    >=  0.06  <  0.11  :=  0.18
    >=  0.11  <  0.16  :=  0.26
    >=  0.16  <  0.21  :=  0.32
    >=  0.21  <  0.26  :=  0.38
    >=  0.26  <  0.31  :=  0.44
    >=  0.31  <  0.36  :=  0.50
    >=  0.36  <  0.41  :=  0.54
    >=  0.41  <  0.46  :=  0.58
    >=  0.46  <  0.51  :=  0.62
    >=  0.51  <  0.56  :=  0.66
    >=  0.56  <  0.61  :=  0.70
    >=  0.61  <  0.66  :=  0.74
    >=  0.66  <  0.71  :=  0.78
    >=  0.71  <  0.76  :=  0.82
    >=  0.76  <  0.81  :=  0.86
    >=  0.81  <  0.86  :=  0.90
    >=  0.86  <  0.91  :=  0.94
    >=  0.91  <  0.96  :=  0.98
    >=  0.96  <  1.01  :=  1.00
  END_OF_TABLE

  RE = %r{ ([<>=]+) \s* (\d\.\d\d) \s* ([<>=]+) \s* (\d\.\d\d) \D+ (\d\.\d\d) }x

  # extract the comparison operators and numbers from the table
  CONVERSION_TABLE = ConversionTable.lines.inject([]) do |table, line|
    m = line.match(RE)
    if not m.nil? and m.length == 6
      table << [m[1], m[2].to_f, m[3], m[4].to_f, m[5].to_f]
    end
    table
  end

  MIN_COMP, MIN = CONVERSION_TABLE[0][0..1]
  MAX_COMP, MAX = CONVERSION_TABLE[-1][2..3]

  def initialize(value)
    if (not value.send(MIN_COMP, MIN)) or (not value.send(MAX_COMP, MAX))
      raise ArgumentError, "value=#{value}, must have: #{MIN} #{MIN_COMP} value #{MAX_COMP} #{MAX}"
    end
    @standard_value = CONVERSION_TABLE.find do |comp1, lower, comp2, upper, standard|
      value.send(comp1, lower) and value.send(comp2, upper)
    end.last
  end
  attr_reader :standard_value
end
```


And a test suite

```ruby
require 'test/unit'

class PriceFractionTests < Test::Unit::TestCase
  @@ok_tests = [
    [0.3793, 0.54],
    [0.4425, 0.58],
    [0.0746, 0.18],
    [0.6918, 0.78],
    [0.2993, 0.44],
    [0.5486, 0.66],
    [0.7848, 0.86],
    [0.9383, 0.98],
    [0.2292, 0.38],
  ]
  @@bad_tests = [1.02, -3]

  def test_ok
    @@ok_tests.each do |val, exp|
      assert_equal(exp, rescale_price_fraction(val))
      assert_equal(exp, Price.new(val).standard_value)
    end
    @@bad_tests.each do |val|
      assert_raise(ArgumentError) {rescale_price_fraction(val)}
      assert_raise(ArgumentError) {Price.new(val).standard_value}
    end
  end
end
```


{{out}}

```txt
Loaded suite price_fraction
Started
.
Finished in 0.001000 seconds.

1 tests, 22 assertions, 0 failures, 0 errors, 0 skips
```



## Run BASIC


```runbasic
data .06, .1,.11,.18,.16,.26,.21,.32,.26,.38,.31,.44,.36,.50,.41,.54,.46,.58,.51,.62
data .56,.66,.61,.70,.66,.74,.71,.78,.76,.82,.81,.86,.86,.90,.91,.94,.96,.98

dim od(100)
dim nd(100)
for i = 1 to 19
read oldDec
read newDec
j = j + 1
for j = j to oldDec * 100
   nd(j) = newDec
next j
next i

[loop]
input "Gimme a number";numb
decm  = val(using("##",(numb mod 1) * 100))
print numb;" -->";nd(decm)

goto [loop]
```


```txt
Gimme a number?12.676
12.676 -->0.78
Gimme a number?4.876
4.876 -->0.94
Gimme a number?34.12
34.12 -->0.26
```



## Rust


```rust
fn fix_price(num: f64) -> f64 {
    match num {
        0.96...1.00 => 1.00,
        0.91...0.96 => 0.98,
        0.86...0.91 => 0.94,
        0.81...0.86 => 0.90,
        0.76...0.81 => 0.86,
        0.71...0.76 => 0.82,
        0.66...0.71 => 0.78,
        0.61...0.66 => 0.74,
        0.56...0.61 => 0.70,
        0.51...0.56 => 0.66,
        0.46...0.51 => 0.62,
        0.41...0.46 => 0.58,
        0.36...0.41 => 0.54,
        0.31...0.36 => 0.50,
        0.26...0.31 => 0.44,
        0.21...0.26 => 0.38,
        0.16...0.21 => 0.32,
        0.11...0.16 => 0.26,
        0.06...0.11 => 0.18,
        0.00...0.06 => 0.10,
        // panics on invalid value
        _ => unreachable!(),
    }
}

fn main() {
    let mut n: f64 = 0.04;
    while n <= 1.00 {
        println!("{:.2} => {}", n, fix_price(n));
        n += 0.04;
    }
}

// and a unit test to check that we haven't forgotten a branch, use 'cargo test' to execute test.
//
// typically this could be included in the match as those check for exhaustiveness already
// by explicitly listing all remaining ranges / values instead of a catch-all underscore (_)
// but f64::NaN, f64::INFINITY and f64::NEG_INFINITY can't be matched like this
#[test]
fn exhaustiveness_check() {
    let mut input_price = 0.;
    while input_price <= 1. {
        fix_price(input_price);
        input_price += 0.01;
    }
}
```


{{out}}


```txt
0.04 => 0.1
0.09 => 0.18
0.14 => 0.26
0.19 => 0.32
0.24 => 0.38
0.29 => 0.44
0.34 => 0.5
0.39 => 0.54
0.44 => 0.58
0.49 => 0.62
0.54 => 0.66
0.59 => 0.7
0.64 => 0.74
0.69 => 0.78
0.74 => 0.82
0.79 => 0.86
0.84 => 0.9
0.89 => 0.94
0.94 => 0.98
0.99 => 1
```


'''Output of unit test:'''

```txt

running 1 test
test exhaustiveness_check ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured
```



## Scala


```scala
def priceFraction(x:Double)=x match {
   case n if n>=0 && n<0.06 => 0.10
   case n if n<0.11 => 0.18
   case n if n<0.36 => ((((n*100).toInt-11)/5)*6+26)/100.toDouble
   case n if n<0.96 => ((((n*100).toInt-31)/5)*4+50)/100.toDouble
   case _ => 1.00
}

def testPriceFraction()=
   for(n <- 0.00 to (1.00, 0.01)) println("%.2f  %.2f".format(n, priceFraction(n)))
```

{{out}}

```txt

0,00  0,10
0,01  0,10
0,02  0,10
0,03  0,10
0,04  0,10
0,05  0,10
0,06  0,18
...
0,25  0,38
0,26  0,44
0,27  0,44
0,28  0,44
0,29  0,44
0,30  0,44
0,31  0,50
0,32  0,50
0,33  0,50
0,34  0,50
0,35  0,50
0,36  0,54
0,37  0,54
...
0,88  0,94
0,89  0,94
0,90  0,94
0,91  0,98
0,92  0,98
0,93  0,98
0,94  0,98
0,95  0,98
0,96  1,00
0,97  1,00
0,98  1,00
0,99  1,00
1,00  1,00

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func float: computePrice (in float: x) is func
  result
    var float: price is 0.0;
  begin
    if x >= 0.0 and x < 0.06 then
      price := 0.10;
    elsif x < 0.11 then
      price := 0.18;
    elsif x < 0.36 then
      price := flt(((trunc(x * 100.0) - 11) div 5) * 6 + 26) / 100.0;
    elsif x < 0.96 then
      price := flt(((trunc(x * 100.0) - 31) div 5) * 4 + 50) / 100.0;
    else
      price := 1.0;
    end if;
  end func;

const proc: main is func
  local
    var integer: i is 0;
  begin
    for i range 0 to 100 do
      writeln(flt(i) / 100.0 digits 2 <& " " <& computePrice(flt(i) / 100.0) digits 2);
    end for;
  end func;
```


The following variant of ''computePrice'' works with a table and raises RANGE_ERROR when x < 0.0 or x >= 1.01 holds:

```seed7
const array array float: table is [] (
    [] (0.06, 0.10), [] (0.11, 0.18), [] (0.16, 0.26), [] (0.21, 0.32), [] (0.26, 0.38),
    [] (0.31, 0.44), [] (0.36, 0.50), [] (0.41, 0.54), [] (0.46, 0.58), [] (0.51, 0.62),
    [] (0.56, 0.66), [] (0.61, 0.70), [] (0.66, 0.74), [] (0.71, 0.78), [] (0.76, 0.82),
    [] (0.81, 0.86), [] (0.86, 0.90), [] (0.91, 0.94), [] (0.96, 0.98), [] (1.01, 1.00));

const func float: computePrice (in float: x) is func
  result
    var float: price is 0.0;
  local
    var integer: index is 1;
  begin
    if x >= 0.0 then
      while x >= table[index][1] do
        incr(index);
      end while;
      price := table[index][2];
    else
      raise RANGE_ERROR;
    end if;
  end func;
```



## Sidef


```ruby
var table = <<'EOT'.lines.map { .words.grep{.is_numeric}.map{.to_n} }
>=  0.00  <  0.06  :=  0.10
>=  0.06  <  0.11  :=  0.18
>=  0.11  <  0.16  :=  0.26
>=  0.16  <  0.21  :=  0.32
>=  0.21  <  0.26  :=  0.38
>=  0.26  <  0.31  :=  0.44
>=  0.31  <  0.36  :=  0.50
>=  0.36  <  0.41  :=  0.54
>=  0.41  <  0.46  :=  0.58
>=  0.46  <  0.51  :=  0.62
>=  0.51  <  0.56  :=  0.66
>=  0.56  <  0.61  :=  0.70
>=  0.61  <  0.66  :=  0.74
>=  0.66  <  0.71  :=  0.78
>=  0.71  <  0.76  :=  0.82
>=  0.76  <  0.81  :=  0.86
>=  0.81  <  0.86  :=  0.90
>=  0.86  <  0.91  :=  0.94
>=  0.91  <  0.96  :=  0.98
>=  0.96  <  1.01  :=  1.00
EOT

func price(money) {
    table.each { |row|
           (row[0] <= money) ->
        && (row[1] >  money) ->
        && return row[2];
    }
    die "Out of range";
}

for n in %n(0.3793 0.4425 0.0746 0.6918 0.2993 0.5486 0.7848 0.9383 0.2292) {
    say price(n);
}
```

{{out}}

```txt

0.54
0.58
0.18
0.78
0.44
0.66
0.86
0.98
0.38

```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
"Table driven rescale"
Object subclass: PriceRescale [
  |table|
  PriceRescale class  >> new: theTable [
    ^ self basicNew initialize: theTable
  ]
  initialize: theTable [
     table := theTable asOrderedCollection.
     ^self
  ]
  rescale: aPrice [ |v1 v2|
    1 to: (table size - 1) do: [:i|
      v1 := table at: i.
      v2 := table at: (i+1).
      ((aPrice >= (v1 x)) & (aPrice < (v2 x)))
        ifTrue: [ ^ v1 y ]
    ].
    (aPrice < ((v1:=(table first)) x)) ifTrue: [ ^ v1 y ].
    (aPrice >= ((v1:=(table last)) x)) ifTrue: [ ^ v1 y ]
  ]
].

|pr|
pr := PriceRescale
         new: {  0.00@0.10 .
                 0.06@0.18 .
                 0.11@0.26 .
                 0.16@0.32 .
                 0.21@0.38 .
                 0.26@0.44 .
                 0.31@0.50 .
                 0.36@0.54 .
                 0.41@0.58 .
                 0.46@0.62 .
                 0.51@0.66 .
                 0.56@0.70 .
                 0.61@0.74 .
                 0.66@0.78 .
                 0.71@0.82 .
                 0.76@0.86 .
                 0.81@0.90 .
                 0.86@0.94 .
                 0.91@0.98 .
                 0.96@1.00 .
                 1.01@1.00
               }.

"get a price"
(pr rescale: ( (Random between: 0 and: 100)/100 )) displayNl.
```



## Tcl

Structured as two functions, one to parse the input data as described in the problem into a form which Tcl can work with easily, and the other to perform the mapping.

```tcl
# Used once to turn the table into a "nice" form
proc parseTable table {
    set map {}
    set LINE_RE {^ *>= *([0-9.]+) *< *([0-9.]+) *:= *([0-9.]+) *$}
    foreach line [split $table \n] {
	if {[string trim $line] eq ""} continue
	if {[regexp $LINE_RE $line -> min max target]} {
	    lappend map $min $max $target
	} else {
	    error "invalid table format: $line"
	}
    }
    return $map
}

# How to apply the "nice" table to a particular value
proc priceFraction {map value} {
    foreach {minimum maximum target} $map {
	if {$value >= $minimum && $value < $maximum} {return $target}
    }
    # Failed to map; return the input
    return $value
}
```

How it is used:

```tcl
# Make the mapping
set inputTable {
    >=  0.00  <  0.06  :=  0.10
    >=  0.06  <  0.11  :=  0.18
    >=  0.11  <  0.16  :=  0.26
    >=  0.16  <  0.21  :=  0.32
    >=  0.21  <  0.26  :=  0.38
    >=  0.26  <  0.31  :=  0.44
    >=  0.31  <  0.36  :=  0.50
    >=  0.36  <  0.41  :=  0.54
    >=  0.41  <  0.46  :=  0.58
    >=  0.46  <  0.51  :=  0.62
    >=  0.51  <  0.56  :=  0.66
    >=  0.56  <  0.61  :=  0.70
    >=  0.61  <  0.66  :=  0.74
    >=  0.66  <  0.71  :=  0.78
    >=  0.71  <  0.76  :=  0.82
    >=  0.76  <  0.81  :=  0.86
    >=  0.81  <  0.86  :=  0.90
    >=  0.86  <  0.91  :=  0.94
    >=  0.91  <  0.96  :=  0.98
    >=  0.96  <  1.01  :=  1.00
}
set map [parseTable $inputTable]

# Apply the mapping to some inputs (from the Oz example)
foreach example {.7388727 .8593103 .826687 .3444635 .0491907} {
    puts "$example -> [priceFraction $map $example]"
}
```

{{out}}

```txt

.7388727 -> 0.82
.8593103 -> 0.90
.826687 -> 0.90
.3444635 -> 0.50
.0491907 -> 0.10

```



## Ursala


```Ursala
#import flo

le  = <0.06,.11,.16,.21,.26,.31,.36,.41,.46,.51,.56,.61,.66,.71,.76,.81,.86,.91,.96,1.01>
out = <0.10,.18,.26,.32,.38,.44,.50,.54,.58,.62,.66,.70,.74,.78,.82,.86,.90,.94,.98,1.>

price_fraction = fleq@rlPlX*|rhr\~&p(le,out)
```

main points:
* <code>~&p(le,out)</code> zips the pair of lists <code>le</code> and <code>out</code> into a list of pairs
* A function of the form <code>f\y</code> applied to an argument <code>x</code> evaluates to <code>f(x,y)</code>
* A function of the form <code>f*|</code> applied to a pair <code>(x,y)</code> where <code>y</code> is a list, makes a list of pairs with <code>x</code> on the left of each item and an item of <code>y</code> on the right. Then it applies <code>f</code> to each pair, makes a list of the right sides of those for which <code>f</code> returned true, and makes a separate list of the right sides of those for which <code>f</code> returned false.
* The suffix <code>rhr</code> after the <code>*|</code> operator extracts the right side of the head of the right list from the result.
* The operand to the <code>*|</code> operator, <code>fleq@rlPlX</code> is the less-or-equal predicate on floating point numbers, composed with the function <code>~&rlPlX</code> which transforms a triple <code>(u,(v,w))</code> to <code>(v,u)</code>

test program:

```Ursala
#cast %eL

test = price_fraction* <0.34,0.070145,0.06,0.05,0.50214,0.56,1.,0.99,0.>

```

{{out}}

```txt
<
   5.000000e-01,
   1.800000e-01,
   1.800000e-01,
   1.000000e-01,
   6.200000e-01,
   7.000000e-01,
   1.000000e+00,
   1.000000e+00,
   1.000000e-01>
```



## VBA


```vb

Option Explicit

Sub Main()
Dim test, i As Long
    test = Array(0.34, 0.070145, 0.06, 0.05, 0.50214, 0.56, 1#, 0.99, 0#, 0.7388727)
    For i = 0 To UBound(test)
        Debug.Print test(i) & " := " & Price_Fraction(CSng(test(i)))
    Next i
End Sub

Private Function Price_Fraction(n As Single) As Single
Dim Vin, Vout, i As Long
    Vin = Array(0.06, 0.11, 0.16, 0.21, 0.26, 0.31, 0.36, 0.41, 0.46, 0.51, 0.56, 0.61, 0.66, 0.71, 0.76, 0.81, 0.86, 0.91, 0.96, 1.01)
    Vout = Array(0.1, 0.18, 0.26, 0.32, 0.38, 0.44, 0.5, 0.54, 0.58, 0.62, 0.66, 0.7, 0.74, 0.78, 0.82, 0.86, 0.9, 0.94, 0.98, 1#)
    For i = 0 To UBound(Vin)
        If n < Vin(i) Then Price_Fraction = Vout(i): Exit For
    Next i
End Function
```

{{Out}}

```txt
0.34 := 0.5
0.070145 := 0.18
0.06 := 0.18
0.05 := 0.1
0.50214 := 0.62
0.56 := 0.7
1 := 1
0.99 := 1
0 := 0.1
0.7388727 := 0.82
```



## VBScript


```vb

Function pf(p)
    If p < 0.06 Then
        pf = 0.10
    ElseIf p < 0.11 Then
    	pf = 0.18
    ElseIf p < 0.16 Then
    	pf = 0.26
    ElseIf p < 0.21 Then
    	pf = 0.32
    ElseIf p < 0.26 Then
    	pf = 0.38
    ElseIf p < 0.31 Then
    	pf = 0.44
    ElseIf p < 0.36 Then
    	pf = 0.50
    ElseIf p < 0.41 Then
    	pf = 0.54
    ElseIf p < 0.46 Then
    	pf = 0.58
    ElseIf p < 0.51 Then
    	pf = 0.62
    ElseIf p < 0.56 Then
    	pf = 0.66
    ElseIf p < 0.61 Then
    	pf = 0.70
    ElseIf p < 0.66 Then
    	pf = 0.74
    ElseIf p < 0.71 Then
    	pf = 0.78
    ElseIf p < 0.76 Then
    	pf = 0.82
    ElseIf p < 0.81 Then
    	pf = 0.86
    ElseIf p < 0.86 Then
    	pf = 0.90
    ElseIf p < 0.91 Then
    	pf = 0.94
    ElseIf p < 0.96 Then
    	pf = 0.98
    Else
    	pf = 1.00
    End If
End Function

WScript.Echo pf(0.7388727)
WScript.Echo pf(0.8593103)
WScript.Echo pf(0.826687)
WScript.Echo pf(0.3444635)

```


{{Out}}

```txt

0.82
0.9
0.9
0.5

```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

func real Price(V);     \Convert to standard value
real V;
[V:= V + 0.001;         \avoids possible rounding error i.e. 0.059999999
case of
  V < 0.06: ret 0.10;
  V < 0.11: ret 0.18;
  V < 0.16: ret 0.26;
  V < 0.21: ret 0.32;
  V < 0.26: ret 0.38;
  V < 0.31: ret 0.44;
  V < 0.36: ret 0.50;
  V < 0.41: ret 0.54;
  V < 0.46: ret 0.58;
  V < 0.51: ret 0.62;
  V < 0.56: ret 0.66;
  V < 0.61: ret 0.70;
  V < 0.66: ret 0.74;
  V < 0.71: ret 0.78;
  V < 0.76: ret 0.82;
  V < 0.81: ret 0.86;
  V < 0.86: ret 0.90;
  V < 0.91: ret 0.94;
  V < 0.96: ret 0.98
other       ret 1.00;
];

[Format(1,2);
RlOut(0, Price(0.0599));  CrLf(0);
RlOut(0, Price(0.10));  CrLf(0);
RlOut(0, Price(1.0));  CrLf(0);
]
```


{{out}}

```txt

0.18
0.18
1.00

```



## zkl


```zkl
fcn convert(price){ // float --> float
             //   <    -->, increments of 0.05 but tables are easier to update
   var vert=T( T(0.06, 0.10), T(0.11, 0.18), T(0.16, 0.26),
	       T(0.21, 0.32), T(0.26, 0.38), T(0.31, 0.44),
	       T(0.36, 0.50), T(0.41, 0.54), T(0.46, 0.58),
	       T(0.51, 0.62), T(0.56, 0.66), T(0.61, 0.70),
	       T(0.66, 0.74), T(0.71, 0.78), T(0.76, 0.82),
	       T(0.81, 0.86), T(0.86, 0.90), T(0.91, 0.94),
	       T(0.96, 0.98), T(1.01, 1.00), );
   vert.filter1('wrap([(a,_)]){ price<a })[1]
}
```


```zkl
fcn convert2(price){  // shifting the fractional part to the integer portion
   var vert=T(0.10, 0.18, 0.26, 0.32, 0.38, 0.44, 0.50, 0.54, 0.58, 0.62,
              0.66, 0.70, 0.74, 0.78, 0.82, 0.86, 0.90, 0.94, 0.98, 1.00);
    vert[(price*100-1)/005];
}
```


```zkl
T(0.7388727, 0.8593103, 0.826687, 0.3444635, 0.0491907).apply(convert) .println();
T(0.7388727, 0.8593103, 0.826687, 0.3444635, 0.0491907).apply(convert2).println();
```

{{out}}

```txt

L(0.82,0.9,0.9,0.5,0.1)
L(0.82,0.9,0.9,0.5,0.1)

```


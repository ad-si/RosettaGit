+++
title = "Magic squares of doubly even order"
description = ""
date = 2019-10-05T22:32:59Z
aliases = []
[extra]
id = 20582
[taxonomies]
categories = []
tags = []
+++

{{task}}
A [[wp:Magic_square|magic square]] is an '''NxN''' square matrix whose numbers consist of consecutive numbers arranged so that the sum of each row and column, ''and'' both diagonals are equal to the same sum (which is called the ''magic number'' or ''magic constant''). 

A magic square of doubly even order has a size that is a multiple of four (e.g. 4, 8, 12). This means that the subsquares also have an even size, which plays a role in the construction.

<!--  As more computer programming languages will be added, they will "fill up" the space to the left of this light blue grid, and the first language entry will be the (normal) full width, so the big size is essential "free space".   Gerard Schildberger.   --> 
{| class="wikitable" style="float:right;border: 2px solid black; background:lightblue; color:black; margin-left:auto;margin-right:auto;text-align:center;width:22em;height:15em;table-layout:fixed;font-size:100%"
|-
|<big>'''1'''</big>||<big>'''2'''</big>||<big>'''62'''</big>||<big>'''61'''</big>||<big>'''60'''</big>||<big>'''59'''</big>||<big>'''7'''</big>||<big>'''8'''</big>
|-
|<big>'''9'''</big>||<big>'''10'''</big>||<big>'''54'''</big>||<big>'''53'''</big>||<big>'''52'''</big>||<big>'''51'''</big>||<big>'''15'''</big>||<big>'''16'''</big>
|-
|<big>'''48'''</big>||<big>'''47'''</big>||<big>'''19'''</big>||<big>'''20'''</big>||<big>'''21'''</big>||<big>'''22'''</big>||<big>'''42'''</big>||<big>'''41'''</big>
|-
|<big>'''40'''</big>||<big>'''39'''</big>||<big>'''27'''</big>||<big>'''28'''</big>||<big>'''29'''</big>||<big>'''30'''</big>||<big>'''34'''</big>||<big>'''33'''</big>
|-
|<big>'''32'''</big>||<big>'''31'''</big>||<big>'''35'''</big>||<big>'''36'''</big>||<big>'''37'''</big>||<big>'''38'''</big>||<big>'''26'''</big>||<big>'''25'''</big>
|-
|<big>'''24'''</big>||<big>'''23'''</big>||<big>'''43'''</big>||<big>'''44'''</big>||<big>'''45'''</big>||<big>'''46'''</big>||<big>'''18'''</big>||<big>'''17'''</big>
|-
|<big>'''49'''</big>||<big>'''50'''</big>||<big>'''14'''</big>||<big>'''13'''</big>||<big>'''12'''</big>||<big>'''11'''</big>||<big>'''55'''</big>||<big>'''56'''</big>
|-
|<big>'''57'''</big>||<big>'''58'''</big>||<big>'''6'''</big>||<big>'''5'''</big>||<big>'''4'''</big>||<big>'''3'''</big>||<big>'''63'''</big>||<big>'''64'''</big>
|}


;Task
Create a magic square of 8 x 8.




; Related tasks
* [[Magic squares of odd order]]
* [[Magic squares of singly even order]]



; See also:
* [http://www.1728.org/magicsq2.htm Doubly Even Magic Squares (1728.org)]





## 360 Assembly

{{trans|Java}}

```360asm
*        Magic squares of doubly even order  01/03/2017
MAGICSDB CSECT
         USING  MAGICSDB,R13
         B      72(R15)            skip save area
         DC     17F'0'             save area
         STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15            end of prolog
         SR     R8,R8              k=0
         LA     R6,0               i=0
       DO WHILE=(C,R6,LE,=A(N-1))  do i=0 to n-1
         MVC    PG,=CL80' '          clear buffer
         LA     R9,PG                pgi=0
         LA     R7,0                 j=0
       DO WHILE=(C,R7,LE,=A(N-1))    do j=0 to n-1
         LR     R4,R7                  j
         SRDA   R4,32                  >>r5
         D      R4,MULT                /mult
         LR     R2,R5                  r2=j/mult
         LR     R4,R6                  i
         SRDA   R4,32                  >>r5
         D      R4,MULT                /mult
         SLA    R5,2                   r5=(i/mult)*4
         AR     R2,R5                  bitpos=j/mult+(i/mult)*4
         STC    R2,XSLL+3              number_of_shift=bitpos
         L      R5,=F'1'               1
         EX     0,XSLL                 r5=1<<bitpos  (SLL R5,bitpos)
         L      R4,BITS                bits
         NR     R4,R5                  bits and (1<<bitpos)
       IF LTR,R4,NZ,R4 THEN            if (bits and (1<<bitpos))<>0
         LA     R10,1(R8)                x=k+1
       ELSE     ,                      else
         L      R10,SIZE                 size
         SR     R10,R8                   x=size-k
       ENDIF    ,                      endif
         XDECO  R10,XDEC               edit x
         MVC    0(4,R9),XDEC+8         output x
         LA     R9,4(R9)               pgi+=4
         LA     R8,1(R8)               k++
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         MVC    PG,=CL80'magic constant='
         L      R1,=A((N*N+1)*N/2) magicnum=(n*n+1)*n/2
         XDECO  R1,XDEC            edit magicnum
         MVC    PG+15(4),XDEC+8    output magicnum
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)
         XR     R15,R15            rc=0
         BR     R14                exit
XSLL     SLL    R5,0               shift left logical
N        EQU    8                  <= input value n
SIZE     DC     A(N*N)             size=n*n
MULT     DC     A(N/4)             mult=n/4  (multiple of 4)
BITS     DC     XL2'0000',BL2'1001011001101001'  pattern
PG       DS     CL80               buffer
XDEC     DS     CL12               temp for xdeco
         YREGS
         END    MAGICSDB
```

{{out}}

```txt

   1   2  62  61  60  59   7   8
   9  10  54  53  52  51  15  16
  48  47  19  20  21  22  42  41
  40  39  27  28  29  30  34  33
  32  31  35  36  37  38  26  25
  24  23  43  44  45  46  18  17
  49  50  14  13  12  11  55  56
  57  58   6   5   4   3  63  64
magic constant= 260

```



## AppleScript

{{Trans|JavaScript}}

```AppleScript
-- MAGIC SQUARE OF DOUBLY EVEN ORDER -----------------------------------------

-- magicSquare :: Int -> [[Int]]
on magicSquare(n)
    if n mod 4 > 0 then
        {}
    else
        set sqr to n * n
        
        set maybePowerOfTwo to asPowerOfTwo(sqr)
        if maybePowerOfTwo is not missing value then
            
            -- For powers of 2, the (append not) 'magic' series directly
            -- yields the truth table that we need
            set truthSeries to magicSeries(maybePowerOfTwo)
        else
            -- where n is not a power of 2, we can replicate a
            -- minimum truth table, horizontally and vertically
            
            script scale
                on |λ|(x)
                    replicate(n / 4, x)
                end |λ|
            end script
            
            set truthSeries to ¬
                flatten(scale's |λ|(map(scale, splitEvery(4, magicSeries(4)))))
        end if
        
        set limit to sqr + 1
        script inOrderOrReversed
            on |λ|(x, i)
                cond(x, i, limit - i)
            end |λ|
        end script
        
        -- Taken directly from an integer series  [1..sqr] where True
        -- and from the reverse of that series where False
        splitEvery(n, map(inOrderOrReversed, truthSeries))
    end if
end magicSquare

-- magicSeries :: Int -> [Bool]
on magicSeries(n)
    script boolToggle
        on |λ|(x)
            not x
        end |λ|
    end script
    
    if n ≤ 0 then
        {true}
    else
        set xs to magicSeries(n - 1)
        xs & map(boolToggle, xs)
    end if
end magicSeries


-- TEST ----------------------------------------------------------------------
on run
    
    formattedTable(magicSquare(8))
    
end run

-- formattedTable :: [[Int]] -> String
on formattedTable(lstTable)
    set n to length of lstTable
    set w to 2.5 * n
    "magic(" & n & ")" & linefeed & wikiTable(lstTable, ¬
        false, "text-align:center;width:" & ¬
        w & "em;height:" & w & "em;table-layout:fixed;")
end formattedTable

-- wikiTable :: [Text] -> Bool -> Text -> Text
on wikiTable(lstRows, blnHdr, strStyle)
    script fWikiRows
        on |λ|(lstRow, iRow)
            set strDelim to cond(blnHdr and (iRow = 0), "!", "|")
            set strDbl to strDelim & strDelim
            linefeed & "|-" & linefeed & strDelim & space & ¬
                intercalate(space & strDbl & space, lstRow)
        end |λ|
    end script
    
    linefeed & "{| class=\"wikitable\" " & ¬
        cond(strStyle ≠ "", "style=\"" & strStyle & "\"", "") & ¬
        intercalate("", ¬
            map(fWikiRows, lstRows)) & linefeed & "|}" & linefeed
end wikiTable


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- asPowerOfTwo :: Int -> maybe Int
on asPowerOfTwo(n)
    if not isPowerOf(2, n) then
        missing value
    else
        set strCMD to ("echo 'l(" & n as string) & ")/l(2)' | bc -l"
        (do shell script strCMD) as integer
    end if
end asPowerOfTwo

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    script append
        on |λ|(a, b)
            a & b
        end |λ|
    end script
    
    foldl(append, {}, map(f, xs))
end concatMap

-- cond :: Bool -> a -> a -> a
on cond(bool, f, g)
    if bool then
        f
    else
        g
    end if
end cond

-- flatten :: Tree a -> [a]
on flatten(t)
    if class of t is list then
        concatMap(my flatten, t)
    else
        t
    end if
end flatten

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- isPowerOf :: Int -> Int -> Bool
on isPowerOf(k, n)
    set v to k
    script remLeft
        on |λ|(x)
            x mod v is not 0
        end |λ|
    end script
    
    script integerDiv
        on |λ|(x)
            x div v
        end |λ|
    end script
    
    |until|(remLeft, integerDiv, n) = 1
end isPowerOf

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map


-- Lift 2nd class handler function into 1st class script wrapper 
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary 
-- assembly of a target length

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}
    
    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- splitAt :: Int -> [a] -> ([a],[a])
on splitAt(n, xs)
    if n > 0 and n < length of xs then
        if class of xs is text then
            {items 1 thru n of xs as text, items (n + 1) thru -1 of xs as text}
        else
            {items 1 thru n of xs, items (n + 1) thru -1 of xs}
        end if
    else
        if n < 1 then
            {{}, xs}
        else
            {xs, {}}
        end if
    end if
end splitAt

-- splitEvery :: Int -> [a] -> [[a]]
on splitEvery(n, xs)
    if length of xs ≤ n then
        {xs}
    else
        set {gp, t} to splitAt(n, xs)
        {gp} & splitEvery(n, t)
    end if
end splitEvery

-- until :: (a -> Bool) -> (a -> a) -> a -> a
on |until|(p, f, x)
    set mp to mReturn(p)
    set v to x
    
    tell mReturn(f)
        repeat until mp's |λ|(v)
            set v to |λ|(v)
        end repeat
    end tell
    return v
end |until|
```

{{Out}}
magic(8)

{| class="wikitable" style="text-align:center;width:20.0em;height:20.0em;table-layout:fixed;"
|-
| 1 || 63 || 62 || 4 || 60 || 6 || 7 || 57
|-
| 56 || 10 || 11 || 53 || 13 || 51 || 50 || 16
|-
| 48 || 18 || 19 || 45 || 21 || 43 || 42 || 24
|-
| 25 || 39 || 38 || 28 || 36 || 30 || 31 || 33
|-
| 32 || 34 || 35 || 29 || 37 || 27 || 26 || 40
|-
| 41 || 23 || 22 || 44 || 20 || 46 || 47 || 17
|-
| 49 || 15 || 14 || 52 || 12 || 54 || 55 || 9
|-
| 8 || 58 || 59 || 5 || 61 || 3 || 2 || 64
|}


## AWK

{{trans|C#}}
Since standard awk does not support bitwise operators, we provide the function countup to do the magic.

```AWK
# Usage: GAWK -f MAGIC_SQUARES_OF_DOUBLY_EVEN_ORDER.AWK
BEGIN {
    n = 8
    msquare[0, 0] = 0
    if (magicsquaredoublyeven(msquare, n)) {
        for (i = 0; i < n; i++) {
            for (j = 0; j < n; j++) {
                printf("%2d ", msquare[i, j])
            }
            printf("\n")
        }
        printf("\nMagic constant: %d\n", (n * n + 1) * n / 2)
        exit 1
    } else {
        exit 0
    }
}
function magicsquaredoublyeven(msquare, n,    size, mult, r, c, i) {
    if (n < 4 || n % 4 != 0) {
        printf("Base must be a positive multiple of 4.\n")
        return 0
    }
    size = n * n
    mult = n / 4 # how many multiples of 4

    i = 0
    for (r = 0; r < n; r++) {
        for (c = 0; c < n; c++) {
            msquare[r, c] = countup(r, c, mult) ? i + 1 : size - i
            i++
        }
    }
    return 1
}
function countup(r, c, mult,    pattern, bitpos) {
    # Returns 1, if we are in a count-up zone (0 otherwise)
    pattern = "1001011001101001"
    bitpos =  int(c / mult) + int(r / mult) * 4 + 1
    return substr(pattern, bitpos, 1) + 0
}
```


{{out}}

```txt
 1  2 62 61 60 59  7  8
 9 10 54 53 52 51 15 16
48 47 19 20 21 22 42 41
40 39 27 28 29 30 34 33
32 31 35 36 37 38 26 25
24 23 43 44 45 46 18 17
49 50 14 13 12 11 55 56
57 58  6  5  4  3 63 64

Magic constant: 260
```



## Befunge

The size, ''N'', is specified by the first value on the stack. The algorithm is loosely based on the [[Magic_squares_of_doubly_even_order#Java|Java]] implementation.


```befunge>8>>>v
10p00g:*1-*\110g2*-*+1+.:00g%!9+,:#v_@
p00:<^:!!-!%3//4g00%g00\!!%3/*:g00*4:::-1<*:
```


{{out}}

```txt
1       2       62      61      60      59      7       8
9       10      54      53      52      51      15      16
48      47      19      20      21      22      42      41
40      39      27      28      29      30      34      33
32      31      35      36      37      38      26      25
24      23      43      44      45      46      18      17
49      50      14      13      12      11      55      56
57      58      6       5       4       3       63      64
```



## C

Takes number of rows from command line, prints out usage on incorrect invocation.

```C

#include<stdlib.h>
#include<ctype.h>
#include<stdio.h>

int** doublyEvenMagicSquare(int n) {
	if (n < 4 || n % 4 != 0)
		return NULL;

	int bits = 38505;
	int size = n * n;
	int mult = n / 4,i,r,c,bitPos;

	int** result = (int**)malloc(n*sizeof(int*));
	
	for(i=0;i<n;i++)
		result[i] = (int*)malloc(n*sizeof(int));

	for (r = 0, i = 0; r < n; r++) {
		for (c = 0; c < n; c++, i++) {
			bitPos = c / mult + (r / mult) * 4;
			result[r][c] = (bits & (1 << bitPos)) != 0 ? i + 1 : size - i;
		}
	}
	return result;
}

int numDigits(int n){
	int count = 1;
	
	while(n>=10){
		n /= 10;
		count++;
	}
	
	return count;
}

void printMagicSquare(int** square,int rows){
	int i,j,baseWidth = numDigits(rows*rows) + 3;
	
	printf("Doubly Magic Square of Order : %d and Magic Constant : %d\n\n",rows,(rows * rows + 1) * rows / 2);
	
	for(i=0;i<rows;i++){
		for(j=0;j<rows;j++){
			printf("%*s%d",baseWidth - numDigits(square[i][j]),"",square[i][j]);
		}
		printf("\n");
	}
}

int main(int argC,char* argV[])
{
	int n;
	
	if(argC!=2||isdigit(argV[1][0])==0)
		printf("Usage : %s <integer specifying rows in magic square>",argV[0]);
	else{
		n = atoi(argV[1]);
		printMagicSquare(doublyEvenMagicSquare(n),n);
	}
	return 0;
}

```

Invocation and Output :

```txt

C:\rosettaCode>doublyEvenMagicSquare 12
Doubly Magic Square of Order : 12 and Magic Constant : 870

     1     2     3   141   140   139   138   137   136    10    11    12
    13    14    15   129   128   127   126   125   124    22    23    24
    25    26    27   117   116   115   114   113   112    34    35    36
   108   107   106    40    41    42    43    44    45    99    98    97
    96    95    94    52    53    54    55    56    57    87    86    85
    84    83    82    64    65    66    67    68    69    75    74    73
    72    71    70    76    77    78    79    80    81    63    62    61
    60    59    58    88    89    90    91    92    93    51    50    49
    48    47    46   100   101   102   103   104   105    39    38    37
   109   110   111    33    32    31    30    29    28   118   119   120
   121   122   123    21    20    19    18    17    16   130   131   132
   133   134   135     9     8     7     6     5     4   142   143   144

```



## C++


```cpp>#include <iostream

#include <sstream>
#include <iomanip>
using namespace std;

class magicSqr
{
public: 
    magicSqr( int d ) {
        while( d % 4 > 0 ) { d++; }
        sz = d;
        sqr = new int[sz * sz];
        fillSqr();
    }
    ~magicSqr() { delete [] sqr; }

    void display() const {
        cout << "Doubly Even Magic Square: " << sz << " x " << sz << "\n";
        cout << "It's Magic Sum is: " << magicNumber() << "\n\n";
        ostringstream cvr; cvr << sz * sz;
        int l = cvr.str().size();
 
        for( int y = 0; y < sz; y++ ) {
            int yy = y * sz;
            for( int x = 0; x < sz; x++ ) {
                cout << setw( l + 2 ) << sqr[yy + x];
            }
            cout << "\n";
        }
        cout << "\n\n";
    }
private:
    void fillSqr() {
        static const bool tempAll[4][4] = {{ 1, 0, 0, 1 }, { 0, 1, 1, 0 }, { 0, 1, 1, 0 }, { 1, 0, 0, 1 } };
        int i = 0;
        for( int curRow = 0; curRow < sz; curRow++ ) {
            for( int curCol = 0; curCol < sz; curCol++ ) {
                sqr[curCol + sz * curRow] = tempAll[curRow % 4][curCol % 4] ? i + 1 : sz * sz - i;
                i++;
            }
        }
    }
    int magicNumber() const { return sz * ( ( sz * sz ) + 1 ) / 2; }
 
    int* sqr;
    int sz;
};
 
int main( int argc, char* argv[] ) {
    magicSqr s( 8 );
    s.display();
    return 0;
}
```

{{out}}

```txt

Doubly Even Magic Square: 8 x 8
It's Magic Sum is: 260

   1  63  62   4   5  59  58   8
  56  10  11  53  52  14  15  49
  48  18  19  45  44  22  23  41
  25  39  38  28  29  35  34  32
  33  31  30  36  37  27  26  40
  24  42  43  21  20  46  47  17
  16  50  51  13  12  54  55   9
  57   7   6  60  61   3   2  64

```



## C#

{{trans|Java}}

```csharp
using System;

namespace MagicSquareDoublyEven
{
    class Program
    {
        static void Main(string[] args)
        {
            int n = 8;
            var result = MagicSquareDoublyEven(n);
            for (int i = 0; i < result.GetLength(0); i++)
            {
                for (int j = 0; j < result.GetLength(1); j++)
                    Console.Write("{0,2} ", result[i, j]);
                Console.WriteLine();
            }
            Console.WriteLine("\nMagic constant: {0} ", (n * n + 1) * n / 2);
            Console.ReadLine();
        }

        private static int[,] MagicSquareDoublyEven(int n)
        {
            if (n < 4 || n % 4 != 0)
                throw new ArgumentException("base must be a positive "
                        + "multiple of 4");

            // pattern of count-up vs count-down zones
            int bits = 0b1001_0110_0110_1001;
            int size = n * n;
            int mult = n / 4;  // how many multiples of 4

            int[,] result = new int[n, n];

            for (int r = 0, i = 0; r < n; r++)
            {
                for (int c = 0; c < n; c++, i++)
                {
                    int bitPos = c / mult + (r / mult) * 4;
                    result[r, c] = (bits & (1 << bitPos)) != 0 ? i + 1 : size - i;
                }
            }
            return result;
        }
    }
}
```


```txt
 1  2 62 61 60 59  7  8
 9 10 54 53 52 51 15 16
48 47 19 20 21 22 42 41
40 39 27 28 29 30 34 33
32 31 35 36 37 38 26 25
24 23 43 44 45 46 18 17
49 50 14 13 12 11 55 56
57 58  6  5  4  3 63 64

Magic constant: 260
```



## D

{{trans|Java}}

```D
import std.stdio;

void main() {
    int n=8;
    foreach(row; magicSquareDoublyEven(n)) {
        foreach(col; row) {
            writef("%2s ", col);
        }
        writeln;
    }
    writeln("\nMagic constant: ", (n*n+1)*n/2);
}

int[][] magicSquareDoublyEven(int n) {
    import std.exception;
    enforce(n>=4 && n%4 == 0, "Base must be a positive multiple of 4");

    int bits = 0b1001_0110_0110_1001;
    int size = n * n;
    int mult = n / 4;  // how many multiples of 4

    int[][] result;
    result.length = n;
    foreach(i; 0..n) {
        result[i].length = n;
    }

    for (int r=0, i=0; r<n; r++) {
        for (int c=0; c<n; c++, i++) {
            int bitPos = c / mult + (r / mult) * 4;
            result[r][c] = (bits & (1 << bitPos)) != 0 ? i + 1 : size - i;
        }
    }

    return result;
}
```


{{out}}

```txt
 1  2 62 61 60 59  7  8
 9 10 54 53 52 51 15 16
48 47 19 20 21 22 42 41
40 39 27 28 29 30 34 33
32 31 35 36 37 38 26 25
24 23 43 44 45 46 18 17
49 50 14 13 12 11 55 56
57 58  6  5  4  3 63 64

Magic constant: 260
```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import system'routines;
import extensions;
import extensions'routines;
 
MagicSquareDoublyEven(int n)
{
    if(n < 4 || n.mod(4) != 0)
        { InvalidArgumentException.new:"base must be a positive multiple of 4".raise() };
 
    int bits := 09669h;
    int size := n * n;
    int mult := n / 4;
 
    var result := new IntMatrix(n,n);
    int i := 0;
    for (int r := 0, r < n, r += 1)
    {
        for(int c := 0, c < n, c += 1, i += 1)
        {
            int bitPos := c / mult + (r / mult) * 4;
 
            result[r][c] := ((bits && (1 $shl bitPos)) != 0).iif(i+1,size - i)
        }
    };
 
    ^ result
}
 
public program()
{
    int n := 8;
 
    console.printLine(MagicSquareDoublyEven(n));
 
    console.printLine().printLine("Magic constant: ",(n * n + 1) * n / 2)
}
```

{{out}}

```txt

1,2,62,61,60,59,7,8
9,10,54,53,52,51,15,16
48,47,19,20,21,22,42,41
40,39,27,28,29,30,34,33
32,31,35,36,37,38,26,25
24,23,43,44,45,46,18,17
49,50,14,13,12,11,55,56
57,58,6,5,4,3,63,64

Magic constant: 260

```



## Elixir


```elixir
defmodule Magic_square do
  def doubly_even(n) when rem(n,4)!=0, do: raise ArgumentError, "must be even, but not divisible by 4."
  def doubly_even(n) do
    n2 = n * n
    Enum.zip(1..n2, make_pattern(n))
    |> Enum.map(fn {i,p} -> if p, do: i, else: n2 - i + 1 end)
    |> Enum.chunk(n)
    |> to_string(n)
    |> IO.puts
  end
  
  defp make_pattern(n) do
    pattern = Enum.reduce(1..4, [true], fn _,acc ->
                acc ++ Enum.map(acc, &(!&1))
              end) |> Enum.chunk(4)
    for i <- 0..n-1, j <- 0..n-1, do: Enum.at(pattern, rem(i,4)) |> Enum.at(rem(j,4))
  end
 
  defp to_string(square, n) do
    format = String.duplicate("~#{length(to_char_list(n*n))}w ", n) <> "\n"
    Enum.map_join(square, fn row ->
      :io_lib.format(format, row)
    end)
  end
end

Magic_square.doubly_even(8)
```


{{out}}

```txt

 1 63 62  4  5 59 58  8
56 10 11 53 52 14 15 49
48 18 19 45 44 22 23 41
25 39 38 28 29 35 34 32
33 31 30 36 37 27 26 40
24 42 43 21 20 46 47 17
16 50 51 13 12 54 55  9
57  7  6 60 61  3  2 64

```



## Factor


```factor
USING: arrays combinators.short-circuit formatting fry
generalizations kernel math math.matrices prettyprint sequences
;
IN: rosetta-code.doubly-even-magic-squares

: top?    ( loc n -- ? ) [ second ] dip 1/4 * <  ;
: bottom? ( loc n -- ? ) [ second ] dip 3/4 * >= ;
: left?   ( loc n -- ? ) [ first  ] dip 1/4 * <  ;
: right?  ( loc n -- ? ) [ first  ] dip 3/4 * >= ;
: corner? ( loc n -- ? )
    {
        [ { [ top?    ] [ left?  ] } ]
        [ { [ top?    ] [ right? ] } ]
        [ { [ bottom? ] [ left?  ] } ]
        [ { [ bottom? ] [ right? ] } ]
    } [ 2&& ] map-compose 2|| ;

: center? ( loc n -- ? )
    { [ top? ] [ bottom? ] [ left? ] [ right? ] } [ not ]
    map-compose 2&& ;

: backward? ( loc n -- ? ) { [ corner? ] [ center? ] } 2|| ;
: forward   ( loc n -- m ) [ first2 ] dip * 1 + + ;
: backward  ( loc n -- m ) tuck forward [ sq ] dip - 1 + ;

: (doubly-even-magic-square) ( n -- matrix )
    [ dup 2array matrix-coordinates flip ] [ 3 dupn ] bi
    '[ dup _ backward? [ _ backward ] [ _ forward ] if ]
    matrix-map ;

ERROR: invalid-order order ;

: check-order ( n -- )
    dup { [ zero? not ] [ 4 mod zero? ] } 1&& [ drop ]
    [ invalid-order ] if ;

: doubly-even-magic-square ( n -- matrix )
    dup check-order (doubly-even-magic-square) ;

: main ( -- )
    { 4 8 12 } [
        dup doubly-even-magic-square dup
        [ "Order: %d\n" printf ]
        [ simple-table. ]
        [ first sum "Magic constant: %d\n\n" printf ] tri*
    ] each ;

MAIN: main
```

{{out}}

```txt

Order: 4
16 2  3  13
5  11 10 8
9  7  6  12
4  14 15 1
Magic constant: 34

Order: 8
64 63 3  4  5  6  58 57
56 55 11 12 13 14 50 49
17 18 46 45 44 43 23 24
25 26 38 37 36 35 31 32
33 34 30 29 28 27 39 40
41 42 22 21 20 19 47 48
16 15 51 52 53 54 10 9
8  7  59 60 61 62 2  1
Magic constant: 260

Order: 12
144 143 142 4   5   6   7   8   9   135 134 133
132 131 130 16  17  18  19  20  21  123 122 121
120 119 118 28  29  30  31  32  33  111 110 109
37  38  39  105 104 103 102 101 100 46  47  48
49  50  51  93  92  91  90  89  88  58  59  60
61  62  63  81  80  79  78  77  76  70  71  72
73  74  75  69  68  67  66  65  64  82  83  84
85  86  87  57  56  55  54  53  52  94  95  96
97  98  99  45  44  43  42  41  40  106 107 108
36  35  34  112 113 114 115 116 117 27  26  25
24  23  22  124 125 126 127 128 129 15  14  13
12  11  10  136 137 138 139 140 141 3   2   1
Magic constant: 870

```



## FreeBASIC


```freebasic
' version 18-03-2016
' compile with: fbc -s console
' doubly even magic square 4, 8, 12, 16...

Sub Err_msg(msg As String)
    Print msg
    Beep : Sleep 5000, 1 : Exit Sub
End Sub

Sub de_magicsq(n As UInteger, filename As String = "")

    ' filename <> "" then save square in a file
    ' filename can contain directory name
    ' if filename exist it will be overwriten, no error checking

    If n < 4 Then
        Err_msg( "Error: n is to small")
        Exit Sub
    End If

    If (n Mod 4) <> 0 Then
        Err_msg "Error: not possible to make doubly" + _
        " even magic square size " + Str(n)
        Exit Sub
    End If

    Dim As UInteger sq(1 To n, 1 To n)
    Dim As UInteger magic_sum = n * (n ^ 2 +1) \ 2
    Dim As UInteger q = n \ 4
    Dim As UInteger x, y, nr = 1
    Dim As String frmt = String(Len(Str(n * n)) +1, "#")

    ' set up the square
    For y = 1 To n
        For x = q +1 To n - q
            sq(x,y) = 1
        Next
    Next
    For x = 1 To n
        For y = q +1 To n - q
            sq(x, y) Xor= 1
        Next
    Next

    ' fill the square
    q = n * n +1
    For y = 1 To n
        For x = 1 To n
            If sq(x,y) = 0 Then
                sq(x,y) = q - nr
            Else
                sq(x,y) = nr
            End If
            nr += 1
        Next
    Next

    ' check columms and rows
    For y = 1 To n
        nr = 0 : q = 0
        For x = 1 To n
            nr += sq(x,y)
            q += sq(y,x)
        Next
        If nr <> magic_sum Or q <> magic_sum Then
            Err_msg "Error: value <> magic_sum"
            Exit Sub
        End If
    Next

    ' check diagonals
    nr = 0 : q = 0
    For x = 1 To n
        nr += sq(x, x)
        q += sq(n - x +1, n - x +1)
    Next
    If nr <> magic_sum Or q <> magic_sum Then
        Err_msg "Error: value <> magic_sum"
        Exit Sub
    End If

    ' printing square's on screen bigger when
    ' n > 19 results in a wrapping of the line
    Print "Single even magic square size: "; n; "*"; n
    Print "The magic sum = "; magic_sum
    Print
    For y = 1 To n
        For x = 1 To n
            Print Using frmt; sq(x, y);
        Next
        Print
    Next

    ' output magic square to a file with the name provided
    If filename <> "" Then
        nr = FreeFile
        Open filename For Output As #nr
        Print #nr, "Single even magic square size: "; n; "*"; n
        Print #nr, "The magic sum = "; magic_sum
        Print #nr,
        For y = 1 To n
            For x = 1 To n
                Print #nr, Using frmt; sq(x,y);
            Next
            Print #nr,
        Next
        Close #nr
    End If

End Sub

' ------=< MAIN >=------

de_magicsq(8, "magic8de.txt") : Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Single even magic square size: 8*8
The magic sum = 260

 64 63  3  4  5  6 58 57
 56 55 11 12 13 14 50 49
 17 18 46 45 44 43 23 24
 25 26 38 37 36 35 31 32
 33 34 30 29 28 27 39 40
 41 42 22 21 20 19 47 48
 16 15 51 52 53 54 10  9
  8  7 59 60 61 62  2  1
```



## Go


```Go
package main

import (
	"fmt"
	"log"
	"strings"
)

const dimensions int = 8

func setupMagicSquareData(d int) ([][]int, error) {
	var output [][]int
	if d < 4 || d%4 != 0 {
		return [][]int{}, fmt.Errorf("Square dimension must be a positive number which is divisible by 4")
	}
	var bits uint = 0x9669 // 0b1001011001101001
	size := d * d
	mult := d / 4
	for i, r := 0, 0; r < d; r++ {
		output = append(output, []int{})
		for c := 0; c < d; i, c = i+1, c+1 {
			bitPos := c/mult + (r/mult)*4
			if (bits & (1 << uint(bitPos))) != 0 {
				output[r] = append(output[r], i+1)
			} else {
				output[r] = append(output[r], size-i)
			}
		}
	}
	return output, nil
}

func arrayItoa(input []int) []string {
	var output []string
	for _, i := range input {
		output = append(output, fmt.Sprintf("%4d", i))
	}
	return output
}

func main() {
	data, err := setupMagicSquareData(dimensions)
	if err != nil {
		log.Fatal(err)
	}
	magicConstant := (dimensions * (dimensions*dimensions + 1)) / 2
	for _, row := range data {
		fmt.Println(strings.Join(arrayItoa(row), " "))
	}
	fmt.Printf("\nMagic Constant: %d\n", magicConstant)
}

```

{{Out}}

```txt
   1    2   62   61   60   59    7    8
   9   10   54   53   52   51   15   16
  48   47   19   20   21   22   42   41
  40   39   27   28   29   30   34   33
  32   31   35   36   37   38   26   25
  24   23   43   44   45   46   18   17
  49   50   14   13   12   11   55   56
  57   58    6    5    4    3   63   64

Magic Constant: 260

```



## Haskell


```Haskell
import Data.List (transpose, unfoldr, intercalate)
import Data.List.Split (chunksOf)
import Data.Bool (bool)
import Control.Monad (forM_)

magicSquare :: Int -> [[Int]]
magicSquare n
  | rem n 4 > 0 = []
  | otherwise =
    chunksOf n $ zipWith (flip (bool =<< (-) limit)) series [1 .. sqr]
  where
    sqr = n * n
    limit = sqr + 1
    series
      | isPowerOf 2 n = magicSeries $ floor (logBase 2 (fromIntegral sqr))
      | otherwise =
        concat . concat . concat . scale $ scale <$> chunksOf 4 (magicSeries 4)
      where
        scale = replicate $ quot n 4

magicSeries :: Int -> [Bool]
magicSeries = (iterate ((++) <*> fmap not) [True] !!)

isPowerOf :: Int -> Int -> Bool
isPowerOf k n = until ((0 /=) . flip rem k) (`quot` k) n == 1

-- TEST AND DISPLAY FUNCTIONS --------------------------------------------------
checked :: [[Int]] -> (Int, Bool)
checked square =
  let diagonals =
        fmap (flip (zipWith (!!)) [0 ..]) . ((:) <*> (return . reverse))
      h:t =
        sum <$>
        square ++ -- rows
        transpose square ++ -- cols
        diagonals square -- diagonals
  in (h, all (h ==) t)

table :: String -> [[String]] -> [String]
table delim rows =
  let justifyRight c n s = drop (length s) (replicate n c ++ s)
  in intercalate delim <$>
     transpose
       ((fmap =<< justifyRight ' ' . maximum . fmap length) <$> transpose rows)

main :: IO ()
main =
  forM_ [4, 8, 16] $
  \n -> do
    let test = magicSquare n
    putStrLn $ unlines (table " " (fmap show <$> test))
    print $ checked test
    putStrLn []
```

{{Out}}

```txt
 1 15 14  4
12  6  7  9
 8 10 11  5
13  3  2 16

(34,True)

 1 63 62  4 60  6  7 57
56 10 11 53 13 51 50 16
48 18 19 45 21 43 42 24
25 39 38 28 36 30 31 33
32 34 35 29 37 27 26 40
41 23 22 44 20 46 47 17
49 15 14 52 12 54 55  9
 8 58 59  5 61  3  2 64

(260,True)

  1 255 254   4 252   6   7 249 248  10  11 245  13 243 242  16
240  18  19 237  21 235 234  24  25 231 230  28 228  30  31 225
224  34  35 221  37 219 218  40  41 215 214  44 212  46  47 209
 49 207 206  52 204  54  55 201 200  58  59 197  61 195 194  64
192  66  67 189  69 187 186  72  73 183 182  76 180  78  79 177
 81 175 174  84 172  86  87 169 168  90  91 165  93 163 162  96
 97 159 158 100 156 102 103 153 152 106 107 149 109 147 146 112
144 114 115 141 117 139 138 120 121 135 134 124 132 126 127 129
128 130 131 125 133 123 122 136 137 119 118 140 116 142 143 113
145 111 110 148 108 150 151 105 104 154 155 101 157  99  98 160
161  95  94 164  92 166 167  89  88 170 171  85 173  83  82 176
 80 178 179  77 181  75  74 184 185  71  70 188  68 190 191  65
193  63  62 196  60 198 199  57  56 202 203  53 205  51  50 208
 48 210 211  45 213  43  42 216 217  39  38 220  36 222 223  33
 32 226 227  29 229  27  26 232 233  23  22 236  20 238 239  17
241  15  14 244  12 246 247   9   8 250 251   5 253   3   2 256

(2056,True)
```



## J


```J

mask =: ,#: 8 $ 153 102 102 153
t =: (, 65&-) each >:i.64
8 8 $ mask{each t

```

{{Out}}

```txt

┌──┬──┬──┬──┬──┬──┬──┬──┐
│64│2 │3 │61│60│6 │7 │57│
├──┼──┼──┼──┼──┼──┼──┼──┤
│9 │55│54│12│13│51│50│16│
├──┼──┼──┼──┼──┼──┼──┼──┤
│17│47│46│20│21│43│42│24│
├──┼──┼──┼──┼──┼──┼──┼──┤
│40│26│27│37│36│30│31│33│
├──┼──┼──┼──┼──┼──┼──┼──┤
│32│34│35│29│28│38│39│25│
├──┼──┼──┼──┼──┼──┼──┼──┤
│41│23│22│44│45│19│18│48│
├──┼──┼──┼──┼──┼──┼──┼──┤
│49│15│14│52│53│11│10│56│
├──┼──┼──┼──┼──┼──┼──┼──┤
│8 │58│59│5 │4 │62│63│1 │
└──┴──┴──┴──┴──┴──┴──┴──┘

```



## Java


```java
public class MagicSquareDoublyEven {

    public static void main(String[] args) {
        int n = 8;
        for (int[] row : magicSquareDoublyEven(n)) {
            for (int x : row)
                System.out.printf("%2s ", x);
            System.out.println();
        }
        System.out.printf("\nMagic constant: %d ", (n * n + 1) * n / 2);
    }

    static int[][] magicSquareDoublyEven(final int n) {
        if (n < 4 || n % 4 != 0)
            throw new IllegalArgumentException("base must be a positive "
                    + "multiple of 4");

        // pattern of count-up vs count-down zones
        int bits = 0b1001_0110_0110_1001;
        int size = n * n;
        int mult = n / 4;  // how many multiples of 4

        int[][] result = new int[n][n];

        for (int r = 0, i = 0; r < n; r++) {
            for (int c = 0; c < n; c++, i++) {
                int bitPos = c / mult + (r / mult) * 4;
                result[r][c] = (bits & (1 << bitPos)) != 0 ? i + 1 : size - i;
            }
        }
        return result;
    }
}
```



```txt
 1  2 62 61 60 59  7  8 
 9 10 54 53 52 51 15 16 
48 47 19 20 21 22 42 41 
40 39 27 28 29 30 34 33 
32 31 35 36 37 38 26 25 
24 23 43 44 45 46 18 17 
49 50 14 13 12 11 55 56 
57 58  6  5  4  3 63 64 

Magic constant: 260
```




## JavaScript



### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // doublyEvenMagicSquare :: Int -> [[Int]]
    const doublyEvenMagicSquare = n =>
        0 === n % 4 ? (() => {
            const
                sqr = n * n,
                power = Math.log2(sqr),
                scale = replicate(n / 4);
            return chunksOf(n)(
                map((x, i) => x ? 1 + i : sqr - i)(
                    isInt(power) ? truthSeries(power) : (
                        compose(
                            flatten,
                            scale,
                            map(scale),
                            chunksOf(4)
                        )(truthSeries(4))
                    )
                )
            );
        })() : undefined;

    // truthSeries :: Int -> [Bool]
    const truthSeries = n =>
        0 >= n ? (
            [true]
        ) : (() => {
            const xs = truthSeries(n - 1);
            return xs.concat(xs.map(x => !x));
        })();



    // TEST -----------------------------------------------
    const main = () =>
        // Magic squares of orders 4, 8 and 12, with
        // checks of row, column and diagonal sums.
        intercalate('\n\n')(
            map(n => {
                const
                    lines = doublyEvenMagicSquare(n),
                    sums = map(sum)(
                        lines.concat(
                            transpose(lines)
                            .concat(diagonals(lines))
                        )
                    ),
                    total = sums[0];
                return unlines([
                    "Order: " + str(n),
                    "Summing to: " + str(total),
                    "Row, column and diagonal sums checked: " +
                    str(all(eq(total))(sums)) + '\n',
                    unlines(map(compose(
                        intercalate('  '),
                        map(compose(justifyRight(3)(' '), str))
                    ))(lines))
                ]);
            })([4, 8, 12])
        );


    // GENERIC FUNCTIONS ----------------------------------

    // all :: (a -> Bool) -> [a] -> Bool
    const all = p =>
        // True if p(x) holds for every x in xs.
        xs => xs.every(p);

    // chunksOf :: Int -> [a] -> [[a]]
    const chunksOf = n => xs =>
        enumFromThenTo(0)(n)(
            xs.length - 1
        ).reduce(
            (a, i) => a.concat([xs.slice(i, (n + i))]),
            []
        );

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (...fs) =>
        x => fs.reduceRight((a, f) => f(a), x);

    // diagonals :: [[a]] -> [[a], [a]]
    const diagonals = rows =>
        // Two diagonal sequences,
        // from top left and bottom left
        // respectively, of a given matrix.
        map(flip(zipWith(index))(
            enumFromTo(0)(pred(
                0 < rows.length ? (
                    rows[0].length
                ) : 0
            ))
        ))([rows, reverse(rows)]);

    // enumFromThenTo :: Int -> Int -> Int -> [Int]
    const enumFromThenTo = x1 => x2 => y => {
        const d = x2 - x1;
        return Array.from({
            length: Math.floor(y - x2) / d + 2
        }, (_, i) => x1 + (d * i));
    };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = m => n =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // eq (==) :: Eq a => a -> a -> Bool
    const eq = a => b => a === b;

    // flatten :: NestedList a -> [a]
    const flatten = nest => nest.flat(Infinity);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        x => y => f(y)(x);

    // index (!!) :: [a] -> Int -> a
    const index = xs => i => xs[i];

    // intercalate :: String -> [String] -> String
    const intercalate = s =>
        xs => xs.join(s);

    // isInt :: Int -> Bool
    const isInt = x => x === Math.floor(x);

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = n => cFiller => s =>
        n > s.length ? (
            s.padStart(n, cFiller)
        ) : s;

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // pred :: Enum a => a -> a
    const pred = x => x - 1;

    // replicate :: Int -> a -> [a]
    const replicate = n => x =>
        Array.from({
            length: n
        }, () => x);

    // reverse :: [a] -> [a]
    const reverse = xs =>
        'string' !== typeof xs ? (
            xs.slice(0).reverse()
        ) : xs.split('').reverse().join('');

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // str :: a -> String
    const str = x => x.toString();

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map((row) => row[iCol]));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = f => xs => ys =>
        xs.slice(
            0, Math.min(xs.length, ys.length)
        ).map((x, i) => f(x)(ys[i]));

    // MAIN ------------------------------------------------
    return main();
})();
```

{{Out}}

```txt
Order: 4
Summing to: 34
Row, column and diagonal sums checked: true

  1   15   14    4
 12    6    7    9
  8   10   11    5
 13    3    2   16

Order: 8
Summing to: 260
Row, column and diagonal sums checked: true

  1   63   62    4   60    6    7   57
 56   10   11   53   13   51   50   16
 48   18   19   45   21   43   42   24
 25   39   38   28   36   30   31   33
 32   34   35   29   37   27   26   40
 41   23   22   44   20   46   47   17
 49   15   14   52   12   54   55    9
  8   58   59    5   61    3    2   64

Order: 12
Summing to: 870
Row, column and diagonal sums checked: true

  1  143  142    4    5  139  138    8    9  135  134   12
132   14   15  129  128   18   19  125  124   22   23  121
120   26   27  117  116   30   31  113  112   34   35  109
 37  107  106   40   41  103  102   44   45   99   98   48
 49   95   94   52   53   91   90   56   57   87   86   60
 84   62   63   81   80   66   67   77   76   70   71   73
 72   74   75   69   68   78   79   65   64   82   83   61
 85   59   58   88   89   55   54   92   93   51   50   96
 97   47   46  100  101   43   42  104  105   39   38  108
 36  110  111   33   32  114  115   29   28  118  119   25
 24  122  123   21   20  126  127   17   16  130  131   13
133   11   10  136  137    7    6  140  141    3    2  144
```



## Julia


```julia
# v0.6

function magicsquaredoubleeven(order::Int)
    if order % 4 != 0; error("the order must be divisible by 4") end

    sqr = Matrix{Int}(order, order)
    mul = div(order, 4)
    ext = vcat(1:mul, order-mul+1:order)
    isext(i::Int, j::Int) = (i in ext) == (j in ext)
    boolsqr = collect(isext(i, j) for i in 1:order, j in 1:order)
    for i in linearindices(sqr)
        if boolsqr[i]; sqr[i] = i end
        if !boolsqr[end+1-i]; sqr[end+1-i] = i end
    end
    return sqr
end

for n in (4, 8, 12)
    magicconst = div(n ^ 3 + n, 2)
    sq = magicsquaredoubleeven(n)

    println("Order: $n; magic constant: $magicconst.\nSquare:")
    for r in 1:n, c in 1:n
        @printf("%4i", sq[r, c])
        if c == n; println() end
    end
    println()
end
```


{{out}}

```txt
Order: 4; magic constant: 34.
Square:
   1  12   8  13
  15   6  10   3
  14   7  11   2
   4   9   5  16

Order: 8; magic constant: 260.
Square:
   1   9  48  40  32  24  49  57
   2  10  47  39  31  23  50  58
  62  54  19  27  35  43  14   6
  61  53  20  28  36  44  13   5
  60  52  21  29  37  45  12   4
  59  51  22  30  38  46  11   3
   7  15  42  34  26  18  55  63
   8  16  41  33  25  17  56  64

Order: 12; magic constant: 870.
Square:
   1  13  25 108  96  84  72  60  48 109 121 133
   2  14  26 107  95  83  71  59  47 110 122 134
   3  15  27 106  94  82  70  58  46 111 123 135
 141 129 117  40  52  64  76  88 100  33  21   9
 140 128 116  41  53  65  77  89 101  32  20   8
 139 127 115  42  54  66  78  90 102  31  19   7
 138 126 114  43  55  67  79  91 103  30  18   6
 137 125 113  44  56  68  80  92 104  29  17   5
 136 124 112  45  57  69  81  93 105  28  16   4
  10  22  34  99  87  75  63  51  39 118 130 142
  11  23  35  98  86  74  62  50  38 119 131 143
  12  24  36  97  85  73  61  49  37 120 132 144
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.0

fun magicSquareDoublyEven(n: Int): Array<IntArray> {
    if ( n < 4 || n % 4 != 0) 
        throw IllegalArgumentException("Base must be a positive multiple of 4")

    // pattern of count-up vs count-down zones
    val bits = 0b1001_0110_0110_1001
    val size = n * n
    val mult = n / 4  // how many multiples of 4 
    val result = Array(n) { IntArray(n) }
    var i = 0
    for (r in 0 until n)
        for (c in 0 until n) {
            val bitPos = c / mult + r / mult * 4
            result[r][c] =  if (bits and (1 shl bitPos) != 0) i + 1 else size - i
            i++
        }
    return result
} 

fun main(args: Array<String>) {
    val n = 8
    for (ia in magicSquareDoublyEven(n)) { 
        for (i in ia) print("%2d  ".format(i))
        println()
    }
    println("\nMagic constant ${(n * n + 1) * n / 2}")
}
```


{{out}}

```txt

 1   2  62  61  60  59   7   8
 9  10  54  53  52  51  15  16
48  47  19  20  21  22  42  41
40  39  27  28  29  30  34  33
32  31  35  36  37  38  26  25
24  23  43  44  45  46  18  17
49  50  14  13  12  11  55  56
57  58   6   5   4   3  63  64

Magic constant 260

```



## Lua

For all three kinds of Magic Squares(Odd, singly and doubly even)<br />
See [[Magic_squares/Lua]].


## PARI/GP


A magic one-liner:

```parigp
magicsquare(n)=matrix(n,n,i,j,k=i+j*n-n;if(bitand(38505,2^((j-1)%4*4+(i-1)%4)),k,n*n+1-k))
```


Output:
```txt
magicsquare(8)

[ 1 56 48 25 33 24 16 57]

[63 10 18 39 31 42 50  7]

[62 11 19 38 30 43 51  6]

[ 4 53 45 28 36 21 13 60]

[ 5 52 44 29 37 20 12 61]

[59 14 22 35 27 46 54  3]

[58 15 23 34 26 47 55  2]

[ 8 49 41 32 40 17  9 64]
```


BTW: The bit field number 38505 = 9669h seems to come from hell to do the magic...


## Perl


See [[Magic_squares/Perl|Magic squares/Perl]] for a general magic square generator.

```perl></lang



## Perl 6

See [[Magic_squares/Perl_6|Magic squares/Perl 6]] for a general magic square generator.
{{out}}
With a parameter of 8:

```txt
 1  2 62 61 60 59  7  8
 9 10 54 53 52 51 15 16
48 47 19 20 21 22 42 41
40 39 27 28 29 30 34 33
32 31 35 36 37 38 26 25
24 23 43 44 45 46 18 17
49 50 14 13 12 11 55 56
57 58  6  5  4  3 63 64

The magic number is 260
```

With a parameter of 12:

```txt
  1   2   3 141 140 139 138 137 136  10  11  12
 13  14  15 129 128 127 126 125 124  22  23  24
 25  26  27 117 116 115 114 113 112  34  35  36
108 107 106  40  41  42  43  44  45  99  98  97
 96  95  94  52  53  54  55  56  57  87  86  85
 84  83  82  64  65  66  67  68  69  75  74  73
 72  71  70  76  77  78  79  80  81  63  62  61
 60  59  58  88  89  90  91  92  93  51  50  49
 48  47  46 100 101 102 103 104 105  39  38  37
109 110 111  33  32  31  30  29  28 118 119 120
121 122 123  21  20  19  18  17  16 130 131 132
133 134 135   9   8   7   6   5   4 142 143 144

The magic number is 870
```



## Phix

{{trans|C++}}

```Phix
constant t = {{1,1,0,0},
              {1,1,0,0},
              {0,0,1,1},
              {0,0,1,1}}

function magic_square(integer n)
    if n<4 or mod(n,4)!=0 then return false end if
    sequence square = repeat(repeat(0,n),n)
    integer i = 0
    for r=1 to n do
        for c=1 to n do
            square[r,c] = iff(t[mod(r,4)+1,mod(c,4)+1]?i+1:n*n-i)
            i += 1
        end for
    end for
    return square
end function

procedure check(sequence sq)
    integer n = length(sq)
    integer magic = n*(n*n+1)/2
    integer bd = 0, fd = 0
    for i=1 to length(sq) do
        if sum(sq[i])!=magic then ?9/0 end if
        if sum(columnize(sq,i))!=magic then ?9/0 end if
        bd += sq[i,i]
        fd += sq[n-i+1,n-i+1]
    end for
    if bd!=magic or fd!=magic then ?9/0 end if
end procedure
    
--for i=4 to 16 by 4 do
for i=8 to 8 by 4 do
    sequence square = magic_square(i)
    printf(1,"maqic square of order %d, sum: %d\n", {i,sum(square[i])})
    string fmt = sprintf("%%%dd",length(sprintf("%d",i*i)))
    pp(square,{pp_Nest,1,pp_IntFmt,fmt,pp_StrFmt,1,pp_Pause,0})
    check(square)
end for
```

{{out}}

```txt

maqic square of order 8, sum: 260
{{ 1,63,62, 4, 5,59,58, 8},
 {56,10,11,53,52,14,15,49},
 {48,18,19,45,44,22,23,41},
 {25,39,38,28,29,35,34,32},
 {33,31,30,36,37,27,26,40},
 {24,42,43,21,20,46,47,17},
 {16,50,51,13,12,54,55, 9},
 {57, 7, 6,60,61, 3, 2,64}}

```



## PureBasic

{{trans|FreeBasic}}

```PureBasic
Procedure.i MagicN(n.i)
  ProcedureReturn n*(n*n+1)/2
EndProcedure

Procedure.i MaxN(mx.i,n.i)
  If mx>n : ProcedureReturn mx : Else : ProcedureReturn n : EndIf
EndProcedure

Procedure.i MaxL(mx.i)
  Define.i i
  While mx
    mx/10 : i+1
  Wend
  ProcedureReturn i
EndProcedure

Procedure.b DblEvenMagicSquare(n.i)
  Define.i q=n/4, nr=1, x, y, max, spc
  Dim sq.i(n,n)
  
  For y=1 To n
    For x=q+1 To n-q
      sq(x,y)=1
    Next
  Next
  
  For x=1 To n
    For y=q+1 To n-q
      sq(x,y) ! 1
    Next
  Next
  
  q=n*n+1
  For y=1 To n
    For x=1 To n
      If sq(x,y)=0
        sq(x,y)=q-nr
      Else
        sq(x,y)=nr
      EndIf
      nr+1
      max=MaxN(max,sq(x,y))
    Next
  Next
  
  spc=MaxL(max)+1
  For y=n To 1 Step -1
    For x=n To 1 Step -1
      Print(RSet(Str(sq(x,y)),spc," "))
    Next
    PrintN("")
  Next
    
EndProcedure

OpenConsole("Magic-Square-Doubly-Even")
Define.i n

Repeat
  PrintN("Input [4,8,12..n] (0=Exit)")
  While (n<4) Or (n%4)
    Print(">") : n=Val(Input())
    If n=0 : End : EndIf
  Wend  
  PrintN("The magic sum = "+Str(MagicN(n)))
  DblEvenMagicSquare(n)  
  n=0
ForEver
```

{{out}}
```txt
Input [4,8,12..n] (0=Exit)
>8
The magic sum = 260
  1  2 62 61 60 59  7  8
  9 10 54 53 52 51 15 16
 48 47 19 20 21 22 42 41
 40 39 27 28 29 30 34 33
 32 31 35 36 37 38 26 25
 24 23 43 44 45 46 18 17
 49 50 14 13 12 11 55 56
 57 58  6  5  4  3 63 64
Input [4,8,12..n] (0=Exit)
>
```



## Python


### Procedural


```python

def MagicSquareDoublyEven(order):
    sq = [range(1+n*order,order + (n*order)+1) for n in range(order) ]
    n1 = order/4
    for r in range(n1):
        r1 = sq[r][n1:-n1]
        r2 = sq[order -r - 1][n1:-n1]
        r1.reverse()
        r2.reverse()
        sq[r][n1:-n1] = r2
        sq[order -r - 1][n1:-n1] = r1
    for r in range(n1, order-n1):
        r1 = sq[r][:n1]
        r2 = sq[order -r - 1][order-n1:]
        r1.reverse()
        r2.reverse()
        sq[r][:n1] = r2
        sq[order -r - 1][order-n1:] = r1
    return sq

def printsq(s):
    n = len(s)
    bl = len(str(n**2))+1
    for i in range(n):
        print ''.join( [ ("%"+str(bl)+"s")%(str(x)) for x in s[i]] )
    print "\nMagic constant = %d"%sum(s[0])

printsq(MagicSquareDoublyEven(8))

```

{{out}}
```txt

  1  2 62 61 60 59  7  8
  9 10 54 53 52 51 15 16
 48 47 19 20 21 22 42 41
 40 39 27 28 29 30 34 33
 32 31 35 36 37 38 26 25
 24 23 43 44 45 46 18 17
 49 50 14 13 12 11 55 56
 57 58  6  5  4  3 63 64

Magic constant = 260
>>> 
```




### Composition of pure functions


Generating test results and a magic square in the form of a wiki table:
{{Works with|Python|3.7}}

```python
'''Magic squares of doubly even order'''

from itertools import chain, repeat
from functools import reduce
from math import log


# doublyEvenMagicSquare :: Int -> [[Int]]
def doublyEvenMagicSquare(n):
    '''Magic square of order n'''

    # magic :: Int -> [Bool]
    def magic(n):
        '''Truth-table series'''
        if 0 < n:
            xs = magic(n - 1)
            return xs + [not x for x in xs]
        else:
            return [True]

    sqr = n * n
    power = log(sqr, 2)
    scale = replicate(n / 4)
    return chunksOf(n)([
        succ(i) if bln else sqr - i for i, bln in
        enumerate(magic(power) if isInteger(power) else (
            flatten(scale(
                map(scale, chunksOf(4)(magic(4)))
            ))
        ))
    ])


# TEST ----------------------------------------------------
# main :: IO()
def main():
    '''Tests'''

    order = 8
    magicSquare = doublyEvenMagicSquare(order)

    print(
        'Row sums: ',
        [sum(xs) for xs in magicSquare],
        '\nCol sums:',
        [sum(xs) for xs in transpose(magicSquare)],
        '\n1st diagonal sum:',
        sum(magicSquare[i][i] for i in range(0, order)),
        '\n2nd diagonal sum:',
        sum(magicSquare[i][(order - 1) - i] for i in range(0, order)),
        '\n'
    )
    print(wikiTable({
        'class': 'wikitable',
        'style': cssFromDict({
            'text-align': 'center',
            'color': '#605B4B',
            'border': '2px solid silver'
        }),
        'colwidth': '3em'
    })(magicSquare))


# GENERIC -------------------------------------------------


# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n,
       subdividing the contents of xs.
       Where the length of xs is not evenly divible
       the final list will be shorter than n.'''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''Concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output a in list
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# cssFromDict :: Dict -> String
def cssFromDict(dct):
    '''CSS string serialized from key values in a Dictionary.'''
    return reduce(
        lambda a, k: a + k + ':' + dct[k] + '; ', dct.keys(), ''
    )


# flatten :: NestedList a -> [a]
def flatten(x):
    '''A list of atoms resulting from fully flattening
       an arbitrarily nested list.'''
    return concatMap(flatten)(x) if isinstance(x, list) else [x]


# isInteger :: Num -> Bool
def isInteger(n):
    '''Divisible by one without remainder ?'''
    return 0 == (n - int(n))


# replicate :: Int -> a -> [a]
def replicate(n):
    '''A list of length n in which every element
       has the value x.'''
    return lambda x: list(repeat(x, n))


# succ :: Enum a => a -> a
def succ(x):
    '''The successor of a value. For numeric types, (1 +).'''
    return 1 + x if isinstance(x, int) else (
        chr(1 + ord(x))
    )


# transpose :: Matrix a -> Matrix a
def transpose(m):
    '''The rows and columns of the argument transposed.
       (The matrix containers and rows can be lists or tuples).'''
    if m:
        inner = type(m[0])
        z = zip(*m)
        return (type(m))(
            map(inner, z) if tuple != inner else z
        )
    else:
        return m


# wikiTable :: Dict -> [[a]] -> String
def wikiTable(opts):
    '''List of lists rendered as a wiki table string.'''
    def colWidth():
        return 'width:' + opts['colwidth'] + '; ' if (
            'colwidth' in opts
        ) else ''

    def cellStyle():
        return opts['cell'] if 'cell' in opts else ''

    return lambda rows: '{| ' + reduce(
        lambda a, k: (
            a + k + '="' + opts[k] + '" ' if k in opts else a
        ),
        ['class', 'style'],
        ''
    ) + '\n' + '\n|-\n'.join(
        '\n'.join(
            ('|' if (0 != i and ('cell' not in opts)) else (
                '|style="' + colWidth() + cellStyle() + '"|'
            )) + (
                str(x) or ' '
            ) for x in row
        ) for i, row in enumerate(rows)
    ) + '\n|}\n\n'


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Row sums:  [260, 260, 260, 260, 260, 260, 260, 260] 
Col sums: [260, 260, 260, 260, 260, 260, 260, 260] 
1st diagonal sum: 260 
2nd diagonal sum: 260 
```

{| class="wikitable" style="text-align:center; color:#605B4B; border:2px solid silver; " 
|style="width:3em; "|1
|style="width:3em; "|63
|style="width:3em; "|62
|style="width:3em; "|4
|style="width:3em; "|60
|style="width:3em; "|6
|style="width:3em; "|7
|style="width:3em; "|57
|-
|56
|10
|11
|53
|13
|51
|50
|16
|-
|48
|18
|19
|45
|21
|43
|42
|24
|-
|25
|39
|38
|28
|36
|30
|31
|33
|-
|32
|34
|35
|29
|37
|27
|26
|40
|-
|41
|23
|22
|44
|20
|46
|47
|17
|-
|49
|15
|14
|52
|12
|54
|55
|9
|-
|8
|58
|59
|5
|61
|3
|2
|64
|}


## REXX


<!--   I couldn't figure out the other code's bit shifting and bit ANDing, so I wrote my own algorithm.  --> 

"Marked" numbers   (via the   '''diag'''   subroutine)   indicate that those (sequentially generated) numbers don't get 

swapped   (and thusly, stay in place in the magic square). 

```rexx
/*REXX program constructs a  magic square  of doubly even sides (a size divisible by 4).*/
n=8;     s=n%4;    L=n%2-s+1;    w=length(n**2)  /*size; small sq;  low middle;  # width*/
@.=0;              H=n%2+s                       /*array default;  high middle.         */
call gen                                         /*generate a grid in numerical order.  */
call diag                                        /*mark numbers on both diagonals.      */
call corn                                        /*  "     "    in small corner boxen.  */
call midd                                        /*  "     "    in  the middle    "     */
call swap                                        /*swap positive numbers with highest #.*/
call show                                        /*display the doubly even magic square.*/
call sum                                         /*   "     "  magic number for square. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
o:    parse arg ?;             return n-?+1      /*calculate the "other" (right) column.*/
@:    parse arg x,y;           return abs(@.x.y)
diag:      do r=1 for n;  @.r.r=-@(r,r); o=o(r);  @.r.o=-@(r,o);  end;              return
midd:      do r=L  to H;  do c=L  to H;  @.r.c=-@(r,c);           end;  end;        return
gen:  #=0; do r=1 for n;  do c=1  for n; #=#+1;   @.r.c=#;        end;  end;        return
show: #=0; do r=1 for n;  $=; do c=1  for n; $=$ right(@(r,c),w); end;  say $; end; return
sum:  #=0; do r=1 for n;  #=#+@(r,1);  end;  say;  say 'The magic number is: '   #; return
max#:      do a=n for n  by -1;  do b=n  for n  by -1;  if @.a.b>0  then return; end;  end
/*──────────────────────────────────────────────────────────────────────────────────────*/
swap:         do   r=1  for n
                do c=1  for n;  if @.r.c<0  then iterate;  call max#  /*find max number.*/
                parse value  -@.a.b  (-@.r.c)    with    @.r.c  @.a.b /*swap two values.*/
                end  /*c*/
              end    /*r*/
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
corn:         do   r=1  for n;  if r>s & r<=n-s  then iterate   /*"corner boxen", size≡S*/
                do c=1  for n;  if c>s & c<=n-s  then iterate;  @.r.c=-@(r,c);  end  /*c*/
              end   /*r*/
      return
```

'''output'''   when using the default input:

```txt

  1  2 62 61 60 59  7  8
  9 10 54 53 52 51 15 16
 48 47 19 20 21 22 42 41
 40 39 27 28 29 30 34 33
 32 31 35 36 37 38 26 25
 24 23 43 44 45 46 18 17
 49 50 14 13 12 11 55 56
 57 58  6  5  4  3 63 64

The magic number is:  260

```



## Ruby



```ruby
def double_even_magic_square(n)
  raise ArgumentError, "Need multiple of four" if n%4 > 0
  block_size, max = n/4, n*n
  pre_pat = [true, false, false, true,
             false, true, true, false]
  pre_pat += pre_pat.reverse
  pattern = pre_pat.flat_map{|b| [b] * block_size} * block_size
  flat_ar = pattern.each_with_index.map{|yes, num| yes ? num+1 : max-num}
  flat_ar.each_slice(n).to_a
end

def to_string(square)
  n = square.size
  fmt = "%#{(n*n).to_s.size + 1}d" * n
  square.inject(""){|str,row| str << fmt % row << "\n"}
end

puts to_string(double_even_magic_square(8))
```

{{out}}

```txt

  1  2 62 61 60 59  7  8
 56 55 11 12 13 14 50 49
 48 47 19 20 21 22 42 41
 25 26 38 37 36 35 31 32
 33 34 30 29 28 27 39 40
 24 23 43 44 45 46 18 17
 16 15 51 52 53 54 10  9
 57 58  6  5  4  3 63 64

```



## Rust


```rust
use std::env;

fn main() {
    let n: usize = match env::args()
        .nth(1)
        .and_then(|arg| arg.parse().ok())
        .ok_or("Please specify the size of the magic square, as a positive multiple of 4.")
    {
        Ok(arg) if arg >= 4 && arg % 4 == 0 => arg,
        Err(e) => panic!(e),
        _ => panic!("Argument must be a positive multiple of 4."),
    };

    let mc = (n * n + 1) * n / 2;
    println!("Magic constant: {}\n", mc);
    let bits = 0b1001_0110_0110_1001u32;
    let size = n * n;
    let width = size.to_string().len() + 1;
    let mult = n / 4;
    let mut i = 0;
    for r in 0..n {
        for c in 0..n {
            let bit_pos = c / mult + (r / mult) * 4;
            print!(
                "{e:>w$}",
                e = if bits & (1 << bit_pos) != 0 {
                    i + 1
                } else {
                    size - i
                },
                w = width
            );
            i += 1;
        }
        println!();
    }
}
```

{{out}}

```txt

Magic constant: 260

  1  2 62 61 60 59  7  8
  9 10 54 53 52 51 15 16
 48 47 19 20 21 22 42 41
 40 39 27 28 29 30 34 33
 32 31 35 36 37 38 26 25
 24 23 43 44 45 46 18 17
 49 50 14 13 12 11 55 56
 57 58  6  5  4  3 63 64

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/bdTcGF3/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/gLhkwHHlRO6rPXg9U7MDzg Scastie (remote JVM)].

```Scala
object MagicSquareDoublyEven extends App {
  private val n = 8

  private def magicSquareDoublyEven(n: Int): Array[Array[Int]] = {
    require(n >= 4 || n % 4 == 0, "Base must be a positive multiple of 4.")

    // pattern of count-up vs count-down zones
    val (bits, mult, result, size) = (38505, n / 4, Array.ofDim[Int](n, n), n * n)
    var i = 0

    for (r <- result.indices; c <- result(0).indices) {
      def bitPos = c / mult + (r / mult) * 4

      result(r)(c) = if ((bits & (1 << bitPos)) != 0) i + 1 else size - i
      i += 1
    }
    result
  }

  magicSquareDoublyEven(n).foreach(row => println(row.map(x => f"$x%2s ").mkString))
  println(f"---%nMagic constant: ${(n * n + 1) * n / 2}%d")

}
```



## Sidef

{{trans|Ruby}}

```ruby
func double_even_magic_square(n) {
    assert(n%4 == 0, "Need multiple of four")
    var (bsize, max) = (n/4, n*n)
    var pre_pat = [true, false, false, true,
                   false, true, true, false]
    pre_pat += pre_pat.flip
    var pattern = (pre_pat.map{|b| bsize.of(b)... } * bsize)
    pattern.map_kv{|k,v| v ? k+1 : max-k }.slices(n)
}

func format_matrix(a) {
    var fmt = "%#{a.len**2 -> len}s"
    a.map { .map { fmt % _ }.join(' ') }.join("\n")
}

say format_matrix(double_even_magic_square(8))
```

{{out}}

```txt

 1  2 62 61 60 59  7  8
56 55 11 12 13 14 50 49
48 47 19 20 21 22 42 41
25 26 38 37 36 35 31 32
33 34 30 29 28 27 39 40
24 23 43 44 45 46 18 17
16 15 51 52 53 54 10  9
57 58  6  5  4  3 63 64

```



## VBScript

{{trans|Java}}

```vb
' Magic squares of doubly even order
n=8  'multiple of 4
pattern="1001011001101001"
size=n*n: w=len(size)
mult=n\4  'how many multiples of 4 
wscript.echo "Magic square : " & n & " x " & n
i=0
For r=0 To n-1
	l=""
	For c=0 To n-1
		bit=Mid(pattern, c\mult+(r\mult)*4+1, 1)
		If bit="1" Then t=i+1 Else t=size-i
		l=l & Right(Space(w) & t, w) & " "
		i=i+1
	Next 'c
	wscript.echo l
Next 'r
wscript.echo "Magic constant=" & (n*n+1)*n/2
```

{{out}}

```txt
Magic square : 8 x 8
 1  2 62 61 60 59  7  8 
 9 10 54 53 52 51 15 16 
48 47 19 20 21 22 42 41 
40 39 27 28 29 30 34 33 
32 31 35 36 37 38 26 25 
24 23 43 44 45 46 18 17 
49 50 14 13 12 11 55 56 
57 58  6  5  4  3 63 64 
Magic constant=260
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module MagicSquares

    Function MagicSquareDoublyEven(n As Integer) As Integer(,)
        If n < 4 OrElse n Mod 4 <> 0 Then
            Throw New ArgumentException("base must be a positive multiple of 4")
        End If

        'pattern of count-up vs count-down zones
        Dim bits = Convert.ToInt32("1001011001101001", 2)
        Dim size = n * n
        Dim mult As Integer = n / 4 ' how many multiples of 4

        Dim result(n - 1, n - 1) As Integer

        Dim i = 0
        For r = 0 To n - 1
            For c = 0 To n - 1
                Dim bitPos As Integer = Math.Floor(c / mult) + Math.Floor(r / mult) * 4
                Dim test = (bits And (1 << bitPos)) <> 0
                If test Then
                    result(r, c) = i + 1
                Else
                    result(r, c) = size - i
                End If

                i = i + 1
            Next
            Console.WriteLine()
        Next

        Return result
    End Function

    Sub Main()
        Dim n = 8
        Dim result = MagicSquareDoublyEven(n)
        For i = 0 To result.GetLength(0) - 1
            For j = 0 To result.GetLength(1) - 1
                Console.Write("{0,2} ", result(i, j))
            Next
            Console.WriteLine()
        Next
        Console.WriteLine()
        Console.WriteLine("Magic constant: {0} ", (n * n + 1) * n / 2)

        Console.ReadLine()
    End Sub

End Module
```

{{out}}

```txt
 1  2 62 61 60 59  7  8
 9 10 54 53 52 51 15 16
48 47 19 20 21 22 42 41
40 39 27 28 29 30 34 33
32 31 35 36 37 38 26 25
24 23 43 44 45 46 18 17
49 50 14 13 12 11 55 56
57 58  6  5  4  3 63 64

Magic constant: 260
```



## zkl

{{trans|Java}}

```zkl
class MagicSquareDoublyEven{
   fcn init(n){ var result=magicSquareDoublyEven(n) }
   fcn toString{
      sink,n:=Sink(String),result.len();  // num collumns
      fmt:="%2s ";
      foreach row in (result)
         { sink.write(row.apply('wrap(n){ fmt.fmt(n) }).concat(),"\n") }
      sink.write("\nMagic constant: %d".fmt((n*n + 1)*n/2));
      sink.close();
   }
   fcn magicSquareDoublyEven(n){
      if (n<4 or n%4!=0 or n>16)
	 throw(Exception.ValueError("base must be a positive multiple of 4"));
      bits,size,mult:=0b1001011001101001, n*n, n/4;
      result:=n.pump(List(),n.pump(List(),0).copy);  // array[n,n] of zero

      foreach i in (size){
	 bitsPos:=(i%n)/mult + (i/(n*mult)*4);
	 value:=(bits.bitAnd((2).pow(bitsPos))) and i+1 or size-i;
	 result[i/n][i%n]=value;
      }
      result;
   }
}
MagicSquareDoublyEven(8).println();
```

{{out}}

```txt

 1  2 62 61 60 59  7  8 
 9 10 54 53 52 51 15 16 
48 47 19 20 21 22 42 41 
40 39 27 28 29 30 34 33 
32 31 35 36 37 38 26 25 
24 23 43 44 45 46 18 17 
49 50 14 13 12 11 55 56 
57 58  6  5  4  3 63 64 

Magic constant: 260

```


+++
title = "Damm algorithm"
description = ""
date = 2019-08-06T19:29:31Z
aliases = []
[extra]
id = 21397
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

The   [https://en.wikipedia.org/wiki/Damm_algorithm '''Damm'''   algorithm] is a checksum algorithm which detects all single digit errors and adjacent transposition errors.


;Task:
Verify the checksum, stored as last digit of an input.

<!-- It's draft because I didn't specify it's requirements, see Wikipedia. -->





## 11l

{{trans|Python}}

```11l
V matrix = [
    [0, 3, 1, 7, 5, 9, 8, 6, 4, 2],
    [7, 0, 9, 2, 1, 5, 4, 8, 6, 3],
    [4, 2, 0, 6, 8, 7, 1, 3, 5, 9],
    [1, 7, 5, 0, 9, 8, 3, 4, 2, 6],
    [6, 1, 2, 3, 0, 4, 5, 9, 7, 8],
    [3, 6, 7, 4, 2, 0, 9, 5, 8, 1],
    [5, 8, 6, 9, 7, 2, 0, 1, 3, 4],
    [8, 9, 4, 5, 3, 6, 2, 0, 1, 7],
    [9, 4, 3, 8, 6, 1, 7, 2, 0, 5],
    [2, 5, 8, 1, 4, 3, 6, 7, 9, 0]
]

F damm(Int num) -> Bool
   V row = 0
   L(digit) String(num)
      row = :matrix[row][Int(digit)]
   R row == 0

L(test) [5724, 5727, 112946]
   print(test"\t Validates as: "damm(test))
```

{{out}}

```txt

5724     Validates as: 1B
5727     Validates as: 0B
112946   Validates as: 1B

```



## ALGOL 68


```algol68
BEGIN
    # returns TRUE if the check digit of s is correct according to the Damm algorithm, #
    # FALSE otherwise #
    PROC has valid damm check digit = ( STRING s )BOOL:
         BEGIN
            # operation table - as per wikipedia example #
            [,]INT operation table =
                ( [,]INT( ( 0, 3, 1, 7, 5, 9, 8, 6, 4, 2 )
                        , ( 7, 0, 9, 2, 1, 5, 4, 8, 6, 3 )
                        , ( 4, 2, 0, 6, 8, 7, 1, 3, 5, 9 )
                        , ( 1, 7, 5, 0, 9, 8, 3, 4, 2, 6 )
                        , ( 6, 1, 2, 3, 0, 4, 5, 9, 7, 8 )
                        , ( 3, 6, 7, 4, 2, 0, 9, 5, 8, 1 )
                        , ( 5, 8, 6, 9, 7, 2, 0, 1, 3, 4 )
                        , ( 8, 9, 4, 5, 3, 6, 2, 0, 1, 7 )
                        , ( 9, 4, 3, 8, 6, 1, 7, 2, 0, 5 )
                        , ( 2, 5, 8, 1, 4, 3, 6, 7, 9, 0 )
                        )
                ) [ AT 0, AT 0 ]
                ;
            INT interim digit := 0;
            FOR s pos FROM LWB s TO UPB s DO
                INT next digit = ABS s[ s pos ] - ABS "0";
                IF next digit < 0 OR next digit > 9 THEN
                    # invalid digit #
                    print( ( "Invalid damm digit: ", s[ s pos ], newline ) );
                    stop
                ELSE
                    # have a valid digit #
                    interim digit := operation table[ interim digit, next digit ]
                FI
            OD;
            interim digit = 0
         END # has valid damm check digit # ;

    # test the damm algorithm #
    PROC test damm algorithm = ( STRING s, BOOL expected )VOID:
         BEGIN
            BOOL valid = has valid damm check digit( s );
            print( ( "check digit of ", s, " is "
                   , IF valid THEN "valid" ELSE "invalid" FI
                   , IF valid = expected THEN "" ELSE " *** NOT AS EXPECTED" FI
                   , newline
                   )
                 )
         END # test damm algorithm # ;
    # test cases - as per other language samples #
    test damm algorithm( "5724",   TRUE  );
    test damm algorithm( "5727",   FALSE );
    test damm algorithm( "112946", TRUE  )
END
```

{{out}}

```txt
check digit of 5724 is valid
check digit of 5727 is invalid
check digit of 112946 is valid
```


## AWK


```AWK
# syntax: GAWK -f DAMM_ALGORITHM.AWK
BEGIN {
    damm_init()
    leng = split("5724,5727,112946",arr,",") # test cases
    for (i=1; i<=leng; i++) {
      n = arr[i]
      printf("%s %s\n",damm_check(n),n)
    }
    exit(0)
}
function damm_check(n,  a,i) {
    a = 0
    for (i=1; i<=length(n); i++) {
      a = substr(damm[a],substr(n,i,1)+1,1)
    }
    return(a == 0 ? "T" : "F")
}
function damm_init() {
#              0123456789
    damm[0] = "0317598642"
    damm[1] = "7092154863"
    damm[2] = "4206871359"
    damm[3] = "1750983426"
    damm[4] = "6123045978"
    damm[5] = "3674209581"
    damm[6] = "5869720134"
    damm[7] = "8945362017"
    damm[8] = "9438617205"
    damm[9] = "2581436790"
}
```

{{out}}

```txt
T 5724
F 5727
T 112946
```



## C

{{Output?}}

```c
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

bool damm(unsigned char *input, size_t length) {
    static const unsigned char table[10][10] = {
        {0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
        {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
        {4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
        {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
        {6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
        {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
        {5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
        {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
        {9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
        {2, 5, 8, 1, 4, 3, 6, 7, 9, 0},
    };

    unsigned char interim = 0;
    for (size_t i = 0; i < length; i++) {
        interim = table[interim][input[i]];
    }
    return interim == 0;
}

int main() {
    unsigned char input[4] = {5, 7, 2, 4};
    puts(damm(input, 4) ? "Checksum correct" : "Checksum incorrect");
    return 0;
}
```



## C++

{{trans|C#|C sharp}}

```cpp
#include <sstream>


const int TABLE[][10] = {
	{0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
	{7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
	{4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
	{1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
	{6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
	{3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
	{5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
	{8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
	{9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
	{2, 5, 8, 1, 4, 3, 6, 7, 9, 0},
};

using std::string;
bool damm(string s) {
	int interim = 0;
	for (char c : s) {
		interim = TABLE[interim][c - '0'];
	}
	return interim == 0;
}

int main() {
	auto numbers = { 5724, 5727, 112946, 112949 };
	for (int num : numbers) {
		using std::stringstream;
		stringstream ss;
		ss << num;
		bool isValid = damm(ss.str());
		if (isValid) {
			printf("%6d is valid\n", num);
		} else {
			printf("%6d is invalid\n", num);
		}
	}

	return 0;
}
```

{{out}}

```txt
  5724 is valid
  5727 is invalid
112946 is valid
112949 is invalid
```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;

namespace DammAlgorithm {
    class Program {
        static int[,] table = {
            {0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
            {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
            {4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
            {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
            {6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
            {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
            {5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
            {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
            {9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
            {2, 5, 8, 1, 4, 3, 6, 7, 9, 0},
        };

        static bool Damm(string s) {
            int interim = 0;
            foreach (char c in s) {
                interim = table[interim, c - '0'];
            }
            return interim == 0;
        }

        static void Main(string[] args) {
            int[] numbers = { 5724, 5727, 112946, 112949 };
            foreach (int number in numbers) {
                bool isValid = Damm(number.ToString());
                if (isValid) {
                    Console.WriteLine("{0,6} is valid", number);
                }
                else {
                    Console.WriteLine("{0,6} is invalid", number);
                }
            }
        }
    }
}
```

{{out}}

```txt
  5724 is valid
  5727 is invalid
112946 is valid
112949 is invalid
```



## Clojure


```clojure
(def tbl [[0 3 1 7 5 9 8 6 4 2]
          [7 0 9 2 1 5 4 8 6 3]
          [4 2 0 6 8 7 1 3 5 9]
          [1 7 5 0 9 8 3 4 2 6]
          [6 1 2 3 0 4 5 9 7 8]
          [3 6 7 4 2 0 9 5 8 1]
          [5 8 6 9 7 2 0 1 3 4]
          [8 9 4 5 3 6 2 0 1 7]
          [9 4 3 8 6 1 7 2 0 5]
          [2 5 8 1 4 3 6 7 9 0]])

(defn damm? [digits]
  (= 0 (reduce #(nth (nth tbl %1) %2) 0
    (map #(Character/getNumericValue %) (seq digits)))))
```

{{Out}}

```txt
=> (damm? "5724")
true
=> (damm? "5727")
false
=> (damm? "112946")
true
```



## D


```D
import std.stdio;

auto table = [
    [0, 3, 1, 7, 5, 9, 8, 6, 4, 2],
    [7, 0, 9, 2, 1, 5, 4, 8, 6, 3],
    [4, 2, 0, 6, 8, 7, 1, 3, 5, 9],
    [1, 7, 5, 0, 9, 8, 3, 4, 2, 6],
    [6, 1, 2, 3, 0, 4, 5, 9, 7, 8],
    [3, 6, 7, 4, 2, 0, 9, 5, 8, 1],
    [5, 8, 6, 9, 7, 2, 0, 1, 3, 4],
    [8, 9, 4, 5, 3, 6, 2, 0, 1, 7],
    [9, 4, 3, 8, 6, 1, 7, 2, 0, 5],
    [2, 5, 8, 1, 4, 3, 6, 7, 9, 0],
];

bool damm(string s) {
    int interim = 0;
    foreach (c; s) {
        interim = table[interim][c - '0'];
    }
    return interim == 0;
}

void main() {
    import std.conv : to;
    auto numbers = [5724, 5727, 112946, 112949];
    foreach (number; numbers) {
        bool isValid = damm(number.to!string());
        writef("%6d is ", number);
        if (isValid) {
            writeln("valid");
        } else {
            writeln("invalid");
        }
    }
}
```

{{out}}

```txt
  5724 is valid
  5727 is invalid
112946 is valid
112949 is invalid
```


=={{header|F#|F sharp}}==

```fsharp
open System

let TABLE = [|
    [|0; 3; 1; 7; 5; 9; 8; 6; 4; 2|];
    [|7; 0; 9; 2; 1; 5; 4; 8; 6; 3|];
    [|4; 2; 0; 6; 8; 7; 1; 3; 5; 9|];
    [|1; 7; 5; 0; 9; 8; 3; 4; 2; 6|];
    [|6; 1; 2; 3; 0; 4; 5; 9; 7; 8|];
    [|3; 6; 7; 4; 2; 0; 9; 5; 8; 1|];
    [|5; 8; 6; 9; 7; 2; 0; 1; 3; 4|];
    [|8; 9; 4; 5; 3; 6; 2; 0; 1; 7|];
    [|9; 4; 3; 8; 6; 1; 7; 2; 0; 5|];
    [|2; 5; 8; 1; 4; 3; 6; 7; 9; 0|];
|]

let damm str =
    let rec helper (v:string) interim =
        if v.Length = 0 then 0 = interim
        else helper (v.Substring(1)) (TABLE.[interim].[(int (v.[0])) - (int '0')])
    helper str 0

[<EntryPoint>]
let main _ =
    let numbers = [|5724; 5727; 112946; 112949|]
    for number in numbers do
        let isValid = damm (number.ToString())
        if isValid then
            printfn "%6d is valid" number
        else
            printfn "%6d is invalid" number

    0 // return an integer exit code

```

{{out}}

```txt
  5724 is valid
  5727 is invalid
112946 is valid
112949 is invalid
```



## Factor


```factor
USING: interpolate kernel math math.parser qw sequences ;

CONSTANT: table
{
    { 0 3 1 7 5 9 8 6 4 2 }
    { 7 0 9 2 1 5 4 8 6 3 }
    { 4 2 0 6 8 7 1 3 5 9 }
    { 1 7 5 0 9 8 3 4 2 6 }
    { 6 1 2 3 0 4 5 9 7 8 }
    { 3 6 7 4 2 0 9 5 8 1 }
    { 5 8 6 9 7 2 0 1 3 4 }
    { 8 9 4 5 3 6 2 0 1 7 }
    { 9 4 3 8 6 1 7 2 0 5 }
    { 2 5 8 1 4 3 6 7 9 0 }
}

: damm? ( str -- ? )
    0 [ digit> swap table nth nth ] reduce zero? ;

qw{ 5724 5727 112946 112949 }
[ dup damm? "" "in" ? [I ${} is ${}validI] nl ] each
```

{{out}}

```txt

5724 is valid
5727 is invalid
112946 is valid
112949 is invalid

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Damm_algorithm this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

Thanks to the ability standardised in F90 to define an array with a lower bound other than one, this can be achieved without the need for annoying offsets, as in A(i + 1) instead of just A(i). However, right from the start, Fortran has stored arrays in column-major order, so statements that initialise two-dimensional arrays via a list of consecutive values can look odd when they are nicely laid out, because they will have to be be in transposed order. Alternatively, if the layout is the same as the expected (row,column) usage, the actual usage of the array will have to be (column,row). Rather than transpose a ten by ten matrix, this is the approach here. The continuation column has the (apparent) row count, but row zero can't have the digit zero in the continuation column as this is taken to be equivalent to a space (meaning "no continuation") just in case it is used for the first line of a statement to be continued. However, the letter o will do instead. A capital O looks too much like a 0...

Possibly a more useful function would be one that returned the check digit that must be appended to a sequence to provide the full sequence, as when preparing a checksummed sequence for output. For checking input, such a function would be applied to all but the last digit of the suspect sequence, and its result compared to the supplied last digit. But for simplicity here, all that is reported is "good" or "bad", without hints as to what would have been good.
```Fortran
      LOGICAL FUNCTION DAMM(DIGIT)	!Check that a sequence of digits checks out..
Calculates according to the method of H. Michael Damm, described in 2004.
       CHARACTER*(*) DIGIT		!A sequence of digits only.
       INTEGER*1 OPTABLE(0:9,0:9)	!The special "Operation table" of the method.
       PARAMETER (OPTABLE = (/		!A set of constants...
     o  0, 3, 1, 7, 5, 9, 8, 6, 4, 2,	!        CAREFUL!
     1  7, 0, 9, 2, 1, 5, 4, 8, 6, 3,	!Fortran stores arrays in column-major order.
     2  4, 2, 0, 6, 8, 7, 1, 3, 5, 9,	!Despite the manifest row and column layout apparent here
     3  1, 7, 5, 0, 9, 8, 3, 4, 2, 6,	!This sequence of consecutive items will go into storage order.
     4  6, 1, 2, 3, 0, 4, 5, 9, 7, 8,	!The table resulting from this sequence of constants
     5  3, 6, 7, 4, 2, 0, 9, 5, 8, 1,	!Will appear to be transposed if referenced as (row,column)
     6  5, 8, 6, 9, 7, 2, 0, 1, 3, 4,	!What appears to be row=6 column=1 (counting from zero)
     7  8, 9, 4, 5, 3, 6, 2, 0, 1, 7,	!is to be accessed as OPTABLE(1,6) = 8, not OPTABLE(6,1)
     8  9, 4, 3, 8, 6, 1, 7, 2, 0, 5,	!Storage order is (0,0), (1,0), (2,0), ... (9,0)
     9  2, 5, 8, 1, 4, 3, 6, 7, 9, 0/))	!Followed by      (0,1), (1,1), (2,1), ... (9,1)
       INTEGER I,D,ID	!Assistants.
        ID = 0		!Here we go.
        DO I = 1,LEN(DIGIT)	!Step through the text.
          D = ICHAR(DIGIT(I:I)) - ICHAR("0")	!Convert to an integer. (ASCII or EBCDIC)
          IF (D.LT.0 .OR. D.GT.9) STOP "DAMM! Not a digit!"	!This shouldn't happen!
          ID = OPTABLE(D,ID)		!Transposed: D is the column index and ID the row.
        END DO			!On to the next.
        DAMM = ID .EQ. 0	!Somewhere, a check digit should ensure this.
      END FUNCTION DAMM	!Simple, fast, and alas, rarely used.

      LOGICAL DAMM	!Not a default type.

      WRITE (6,*) DAMM("5724"),"5724"
      WRITE (6,*) DAMM("5727"),"5727"
      WRITE (6,*) DAMM("112946"),"112946"

      END
```

Output:

```txt
 T 5724
 F 5727
 T 112946
```



## FreeBASIC


```freebasic
' version 04-07-2018
' compile with: fbc -s console

Function Damm(digit_str As String) As UInteger

    Dim As UInteger table(10,10) => { { 0, 3, 1, 7, 5, 9, 8, 6, 4, 2 } , _
   { 7, 0, 9, 2, 1, 5, 4, 8, 6, 3 } , { 4, 2, 0, 6, 8, 7, 1, 3, 5, 9 } , _
   { 1, 7, 5, 0, 9, 8, 3, 4, 2, 6 } , { 6, 1, 2, 3, 0, 4, 5, 9, 7, 8 } , _
   { 3, 6, 7, 4, 2, 0, 9, 5, 8, 1 } , { 5, 8, 6, 9, 7, 2, 0, 1, 3, 4 } , _
   { 8, 9, 4, 5, 3, 6, 2, 0, 1, 7 } , { 9, 4, 3, 8, 6, 1, 7, 2, 0, 5 } , _
   { 2, 5, 8, 1, 4, 3, 6, 7, 9, 0 } }

    Dim As UInteger i, col_i, old_row_i, new_row_i

    For i = 0 To Len(digit_str) -1
        col_i = digit_str[i] - Asc("0")
        new_row_i = table(old_row_i, col_i)
        old_row_i = new_row_i
    Next

    Return new_row_i

End Function

' ------=< MAIN >=------

Data "5724", "5727", "112946", ""

Dim As UInteger checksum, t
Dim As String test_string

Do

    Read test_string
    If test_string = "" Then Exit Do
    Print "Checksum test: ";test_string;

    checksum = Damm(test_string)
    If checksum = 0 Then
        Print " is valid"
    Else
        Print " is invalid"
    End If

Loop

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Checksum test: 5724 is valid
Checksum test: 5727 is invalid
Checksum test: 112946 is valid
```



## Go


```go
package main

import "fmt"

var table = [10][10]byte{
    {0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
    {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
    {4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
    {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
    {6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
    {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
    {5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
    {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
    {9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
    {2, 5, 8, 1, 4, 3, 6, 7, 9, 0},
}

func damm(input string) bool {
    var interim byte
    for _, c := range []byte(input) {
        interim = table[interim][c-'0']
    }
    return interim == 0
}

func main() {
    for _, s := range []string{"5724", "5727", "112946", "112949"} {
        fmt.Printf("%6s  %t\n", s, damm(s))
    }
}
```

{{out}}

```txt
  5724  true
  5727  false
112946  true
112949  false
```



## J

'''Solution:'''

```j
OpTbl=: _99 ". ];._2 noun define
0 3 1 7 5 9 8 6 4 2
7 0 9 2 1 5 4 8 6 3
4 2 0 6 8 7 1 3 5 9
1 7 5 0 9 8 3 4 2 6
6 1 2 3 0 4 5 9 7 8
3 6 7 4 2 0 9 5 8 1
5 8 6 9 7 2 0 1 3 4
8 9 4 5 3 6 2 0 1 7
9 4 3 8 6 1 7 2 0 5
2 5 8 1 4 3 6 7 9 0
)

getdigits=: 10&#.inv

getDamm=: verb define
  row=. 0
  for_digit. getdigits y do.
    row=. OpTbl {~ <row,digit
  end.
)

checkDamm=: 0 = getDamm
```

'''Example Usage:'''

```j
   checkDamm&> 5724 5727 112946
1 0 1
```



## Java

{{trans|Kotlin}}

```Java
public class DammAlgorithm {
    private static final int[][] table = {
        {0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
        {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
        {4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
        {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
        {6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
        {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
        {5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
        {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
        {9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
        {2, 5, 8, 1, 4, 3, 6, 7, 9, 0},
    };

    private static boolean damm(String s) {
        int interim = 0;
        for (char c : s.toCharArray()) interim = table[interim][c - '0'];
        return interim == 0;
    }

    public static void main(String[] args) {
        int[] numbers = {5724, 5727, 112946, 112949};
        for (Integer number : numbers) {
            boolean isValid = damm(number.toString());
            if (isValid) {
                System.out.printf("%6d is valid\n", number);
            } else {
                System.out.printf("%6d is invalid\n", number);
            }
        }
    }
}
```

{{out}}

```txt
  5724 is valid
  5727 is invalid
112946 is valid
112949 is invalid
```



## Julia


```julia
function checkdigit(n)
    matrix = (
        (0, 3, 1, 7, 5, 9, 8, 6, 4, 2),
        (7, 0, 9, 2, 1, 5, 4, 8, 6, 3),
        (4, 2, 0, 6, 8, 7, 1, 3, 5, 9),
        (1, 7, 5, 0, 9, 8, 3, 4, 2, 6),
        (6, 1, 2, 3, 0, 4, 5, 9, 7, 8),
        (3, 6, 7, 4, 2, 0, 9, 5, 8, 1),
        (5, 8, 6, 9, 7, 2, 0, 1, 3, 4),
        (8, 9, 4, 5, 3, 6, 2, 0, 1, 7),
        (9, 4, 3, 8, 6, 1, 7, 2, 0, 5),
        (2, 5, 8, 1, 4, 3, 6, 7, 9, 0))
    row = 0
    for d in string(n)
        row = matrix[row + 1][d - '0' + 1]
    end
    return row
end

foreach(i -> println("$i validates as: ", checkdigit(string(i)) == 0), [5724, 5727, 112946])

```
{{output}}

```txt

5724 validates as: true
5727 validates as: false
112946 validates as: true

```



## Kotlin


```scala
// version 1.1.2

val table = arrayOf(
    intArrayOf(0, 3, 1,	7, 5, 9, 8, 6, 4, 2),
    intArrayOf(7, 0, 9, 2, 1, 5, 4, 8, 6, 3),
    intArrayOf(4, 2, 0, 6, 8, 7, 1, 3, 5, 9),
    intArrayOf(1, 7, 5, 0, 9, 8, 3, 4, 2, 6),
    intArrayOf(6, 1, 2, 3, 0, 4, 5, 9, 7, 8),
    intArrayOf(3, 6, 7, 4, 2, 0, 9, 5, 8, 1),
    intArrayOf(5, 8, 6, 9, 7, 2, 0, 1, 3, 4),
    intArrayOf(8, 9, 4, 5, 3, 6, 2, 0, 1, 7),
    intArrayOf(9, 4, 3, 8, 6, 1, 7, 2, 0, 5),
    intArrayOf(2, 5, 8, 1, 4, 3, 6, 7, 9, 0)
)

fun damm(s: String): Boolean {
    var interim = 0
    for (c in s) interim = table[interim][c - '0']
    return interim == 0
}

fun main(args: Array<String>) {
    val numbers = intArrayOf(5724, 5727, 112946, 112949)
    for (number in numbers) {
        val isValid = damm(number.toString())
        println("${"%6d".format(number)} is ${if (isValid) "valid" else "invalid"}")
    }
}
```


{{out}}

```txt
  5724 is valid
  5727 is invalid
112946 is valid
112949 is invalid
```



## Lua


```lua
local tab = {
    {0,3,1,7,5,9,8,6,4,2}, {7,0,9,2,1,5,4,8,6,3},
    {4,2,0,6,8,7,1,3,5,9}, {1,7,5,0,9,8,3,4,2,6},
    {6,1,2,3,0,4,5,9,7,8}, {3,6,7,4,2,0,9,5,8,1},
    {5,8,6,9,7,2,0,1,3,4}, {8,9,4,5,3,6,2,0,1,7},
    {9,4,3,8,6,1,7,2,0,5}, {2,5,8,1,4,3,6,7,9,0}
}
function check( n )
    local idx, a = 0, tonumber( n:sub( 1, 1 ) )
    for i = 1, #n do
        a = tonumber( n:sub( i, i ) )
        if a == nil then return false end
        idx = tab[idx + 1][a + 1]
    end
    return idx == 0
end
local n, r
while( true ) do
    io.write( "Enter the number to check: " )
    n = io.read(); if n == "0" then break end
    r = check( n ); io.write( n, " is " )
    if not r then io.write( "in" ) end
    io.write( "valid!\n" )
end
```

{{out}}

```txt
Enter the number to check: 5724
5724 is valid!
Enter the number to check: 5727
5727 is invalid!
Enter the number to check: 112946
112946 is valid!
Enter the number to check: 0
```


=={{header|Modula-2}}==
{{Output?}}

```modula2
MODULE DammAlgorithm;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE TA = ARRAY[0..9],[0..9] OF INTEGER;
CONST table = TA{
        {0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
        {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
        {4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
        {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
        {6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
        {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
        {5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
        {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
        {9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
        {2, 5, 8, 1, 4, 3, 6, 7, 9, 0}
    };

PROCEDURE Damm(s : ARRAY OF CHAR) : BOOLEAN;
VAR interim,i : INTEGER;
BEGIN
    interim := 0;

    i := 0;
    WHILE s[i] # 0C DO
        interim := table[interim,INT(s[i])-INT('0')];
        INC(i);
    END;
    RETURN interim=0;
END Damm;

PROCEDURE Print(number : INTEGER);
VAR
    isValid : BOOLEAN;
    buf : ARRAY[0..16] OF CHAR;
BEGIN
    FormatString("%i", buf, number);
    isValid := Damm(buf);
    WriteString(buf);
    IF isValid THEN
        WriteString(" is valid");
    ELSE
        WriteString(" is invalid");
    END;
    WriteLn;
END Print;

BEGIN
    Print(5724);
    Print(5727);
    Print(112946);
    Print(112949);

    ReadChar;
END DammAlgorithm.
```



## Objeck

{{trans|C#}}

```objeck
class DammAlgorithm  {
  @table : static : Int[,];

  function : Main(args : String[]) ~ Nil {
    @table := [
      [0, 3, 1, 7, 5, 9, 8, 6, 4, 2]
      [7, 0, 9, 2, 1, 5, 4, 8, 6, 3]
      [4, 2, 0, 6, 8, 7, 1, 3, 5, 9]
      [1, 7, 5, 0, 9, 8, 3, 4, 2, 6]
      [6, 1, 2, 3, 0, 4, 5, 9, 7, 8]
      [3, 6, 7, 4, 2, 0, 9, 5, 8, 1]
      [5, 8, 6, 9, 7, 2, 0, 1, 3, 4]
      [8, 9, 4, 5, 3, 6, 2, 0, 1, 7]
      [9, 4, 3, 8, 6, 1, 7, 2, 0, 5]
      [2, 5, 8, 1, 4, 3, 6, 7, 9, 0]];

    numbers := [ 5724, 5727, 112946, 112949 ];
    each (i : numbers) {
      number := numbers[i];
      isValid := Damm(number->ToString());
      if (isValid) {
        "{$number} is valid"->PrintLine();
      }
      else {
        "{$number} is invalid"->PrintLine();
      };
    };
  }

  function : Damm(s : String) ~ Bool {
    interim := 0;
    each (i : s) {
      interim := @table[interim, s->Get(i) - '0'];
    };
    return interim = 0;
  }
}
```


{{out}}

```txt

5724 is valid
5727 is invalid
112946 is valid
112949 is invalid

```



## Pascal

{{works with|Free Pascal}} {{trans|Modula-2}} nearly copy&paste

```pascal
program DammAlgorithm;
uses
  sysutils;

TYPE TA = ARRAY[0..9,0..9] OF UInt8;
CONST table : TA =
                ((0,3,1,7,5,9,8,6,4,2),
                 (7,0,9,2,1,5,4,8,6,3),
                 (4,2,0,6,8,7,1,3,5,9),
                 (1,7,5,0,9,8,3,4,2,6),
                 (6,1,2,3,0,4,5,9,7,8),
                 (3,6,7,4,2,0,9,5,8,1),
                 (5,8,6,9,7,2,0,1,3,4),
                 (8,9,4,5,3,6,2,0,1,7),
                 (9,4,3,8,6,1,7,2,0,5),
                 (2,5,8,1,4,3,6,7,9,0));

function Damm(s : string) : BOOLEAN;
VAR
  interim,i : UInt8;
BEGIN
  interim := 0;
  i := 1;
  WHILE i <= length(s) DO
  Begin
    interim := table[interim,ORD(s[i])-ORD('0')];
    INC(i);
  END;
  Damm := interim=0;
END;

PROCEDURE Print(number : Uint32);
VAR
    isValid : BOOLEAN;
    buf :string;
BEGIN
    buf := IntToStr(number);
    isValid := Damm(buf);
    Write(buf);
    IF isValid THEN
      Write(' is valid')
    ELSE
      Write(' is invalid');
    WriteLn;
END;

BEGIN
    Print(5724);
    Print(5727);
    Print(112946);
    Print(112949);
    Readln;
END.
```

{{out}}

```txt

5724 is valid
5727 is invalid
112946 is valid
112949 is invalid

```



## Perl


```perl
sub damm {
    my(@digits) = split '', @_[0];
    my @tbl =([< 0 3 1 7 5 9 8 6 4 2 >],
              [< 7 0 9 2 1 5 4 8 6 3 >],
              [< 4 2 0 6 8 7 1 3 5 9 >],
              [< 1 7 5 0 9 8 3 4 2 6 >],
              [< 6 1 2 3 0 4 5 9 7 8 >],
              [< 3 6 7 4 2 0 9 5 8 1 >],
              [< 5 8 6 9 7 2 0 1 3 4 >],
              [< 8 9 4 5 3 6 2 0 1 7 >],
              [< 9 4 3 8 6 1 7 2 0 5 >],
              [< 2 5 8 1 4 3 6 7 9 0 >]
              );
    my $row = 0;
    for my $col (@digits) { $row = $tbl[$row][$col] }
    not $row
}

for (5724, 5727, 112946) {
    print "$_:\tChecksum digit @{[damm($_) ? '' : 'in']}correct.\n"
}
```

{{out}}

```txt
5724:	Checksum digit correct.
5727:	Checksum digit incorrect.
112946:	Checksum digit correct.
```



## Perl 6

{{works with|Rakudo|2017.05}}


```perl6
sub damm ( *@digits ) {
    my @tbl = [0, 3, 1, 7, 5, 9, 8, 6, 4, 2],
              [7, 0, 9, 2, 1, 5, 4, 8, 6, 3],
              [4, 2, 0, 6, 8, 7, 1, 3, 5, 9],
              [1, 7, 5, 0, 9, 8, 3, 4, 2, 6],
              [6, 1, 2, 3, 0, 4, 5, 9, 7, 8],
              [3, 6, 7, 4, 2, 0, 9, 5, 8, 1],
              [5, 8, 6, 9, 7, 2, 0, 1, 3, 4],
              [8, 9, 4, 5, 3, 6, 2, 0, 1, 7],
              [9, 4, 3, 8, 6, 1, 7, 2, 0, 5],
              [2, 5, 8, 1, 4, 3, 6, 7, 9, 0];
    my $row = 0;
    for @digits -> $col { $row = @tbl[$row][$col] }
    not $row
}

# Testing
for 5724, 5727, 112946 {
    say "$_:\tChecksum digit { damm( $_.comb ) ?? '' !! 'in' }correct."
}
```

{{out}}

```txt
5724:	Checksum digit correct.
5727:	Checksum digit incorrect.
112946:	Checksum digit correct.
```



## Phix

As phix uses 1-based indexes, 1 must be added to the operation table,
and validity is given by ending on one, rather than zero.

```Phix
constant tbl = sq_add(1,{{0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
                         {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
                         {4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
                         {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
                         {6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
                         {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
                         {5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
                         {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
                         {9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
                         {2, 5, 8, 1, 4, 3, 6, 7, 9, 0}})

function damm(string s)
    integer interim = 1
    for i=1 to length(s) do
        integer nxt = s[i]-'0'+1
        if nxt<1 or nxt>10 then return 0 end if
        interim = tbl[interim][nxt]
    end for
    return interim == 1
end function

constant tests = {"5724", "5727", "112946", "112949"}
for i=1 to length(tests) do
    string ti = tests[i]
    printf(1,"%7s is %svalid\n",{ti,iff(damm(ti)?"":"in")})
end for
```

{{out}}

```txt

   5724 is valid
   5727 is invalid
 112946 is valid
 112949 is invalid

```



## Python


```Python
def damm(num: int) -> bool:
    row = 0
    for digit in str(num):
        row = _matrix[row][int(digit)]
    return row == 0

_matrix = (
    (0, 3, 1, 7, 5, 9, 8, 6, 4, 2),
    (7, 0, 9, 2, 1, 5, 4, 8, 6, 3),
    (4, 2, 0, 6, 8, 7, 1, 3, 5, 9),
    (1, 7, 5, 0, 9, 8, 3, 4, 2, 6),
    (6, 1, 2, 3, 0, 4, 5, 9, 7, 8),
    (3, 6, 7, 4, 2, 0, 9, 5, 8, 1),
    (5, 8, 6, 9, 7, 2, 0, 1, 3, 4),
    (8, 9, 4, 5, 3, 6, 2, 0, 1, 7),
    (9, 4, 3, 8, 6, 1, 7, 2, 0, 5),
    (2, 5, 8, 1, 4, 3, 6, 7, 9, 0)
)

if __name__ == '__main__':
    for test in [5724, 5727, 112946]:
        print(f'{test}\t Validates as: {damm(test)}')
```

{{out}}

```txt
5724     Validates as: True
5727     Validates as: False
112946   Validates as: True
```



## Racket


```racket
#lang racket/base
(require racket/match)

(define operation-table
  #(#(0 3 1 7 5 9 8 6 4 2)
    #(7 0 9 2 1 5 4 8 6 3)
    #(4 2 0 6 8 7 1 3 5 9)
    #(1 7 5 0 9 8 3 4 2 6)
    #(6 1 2 3 0 4 5 9 7 8)
    #(3 6 7 4 2 0 9 5 8 1)
    #(5 8 6 9 7 2 0 1 3 4)
    #(8 9 4 5 3 6 2 0 1 7)
    #(9 4 3 8 6 1 7 2 0 5)
    #(2 5 8 1 4 3 6 7 9 0)))

(define (integer->digit-list n)
  (let loop ((n n) (a null))
    (if (zero? n) a (let-values (([q r] (quotient/remainder n 10))) (loop q (cons r a))))))

(define/match (check-digit n)
  [((list ds ...))
   (foldl
    (λ (d interim)
      (vector-ref (vector-ref operation-table interim) d))
    0 ds)]
  [((? integer? i))
   (check-digit (integer->digit-list i))])

(define/match (valid-number? n)
  [((? integer? i))
   (valid-number? (integer->digit-list i))]
  [((list ds ...))
   (zero? (check-digit ds))])

(module+ test
  (require rackunit)
  (check-equal? (integer->digit-list 572) '(5 7 2))
  (check-equal? (check-digit 572) 4)
  (check-equal? (check-digit '(5 7 2)) 4)
  (check-true (valid-number? 5724))
  (check-false (valid-number? 5274))
  (check-true (valid-number? 112946)))
```

No output from checks means that all tests passed.


## REXX


### manufactured table


```rexx
/* REXX */
Call init
Call test 5724
Call test 5727
Call test 112946
Call test 112940
Exit

test:
Parse Arg number
int_digit=0
Do p=1 To length(number)
  d=substr(number,p,1)
  int_digit=grid.int_digit.d
  If p<length(number) Then cd=int_digit
  End
If int_digit=0 Then
  Say number 'is ok'
Else
  Say number 'is not ok, check-digit should be' cd '(instead of' d')'
Return

init:
i=-2
Call setup '* 0 1 2 3 4 5 6 7 8 9'
Call setup '0 0 3 1 7 5 9 8 6 4 2'
Call setup '1 7 0 9 2 1 5 4 8 6 3'
Call setup '2 4 2 0 6 8 7 1 3 5 9'
Call setup '3 1 7 5 0 9 8 3 4 2 6'
Call setup '4 6 1 2 3 0 4 5 9 7 8'
Call setup '5 3 6 7 4 2 0 9 5 8 1'
Call setup '6 5 8 6 9 7 2 0 1 3 4'
Call setup '7 8 9 4 5 3 6 2 0 1 7'
Call setup '8 9 4 3 8 6 1 7 2 0 5'
Call setup '9 2 5 8 1 4 3 6 7 9 0'
Return
setup:
  Parse Arg list
  i=i+1
  Do col=-1 To 9
    grid.i.col=word(list,col+2)
    End
  Return
```

{{out}}

```txt
5724 is ok
5727 is not ok, check-digit should be 4 (instead of 7)
112946 is ok
112940 is not ok, check-digit should be 6 (instead of 0)
```



### static table


```rexx
/*REXX pgm uses H. Michael Damm's algorithm to validate numbers with suffixed check dig.*/
   @.0= 0317598642;  @.1= 7092154863;  @.2= 4206871359;  @.3= 1750983426;  @.4= 6123045978
   @.5= 3674209581;  @.6= 5869720134;  @.7= 8945362017;  @.8= 9438617205;  @.9= 2581436790
call Damm  5724  5727  112946  112940            /*invoke Damm's algorithm for some #'s.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Damm: arg z;  do j=1  for words(z);             x=word(z, j);                  L=length(x)
              $=0;                              VCD= 'valid check digit'
                      do p=1  for L;     g=$;   $=substr(@.$, 1 + substr(x, p, 1),  1)
                      end   /*p*/
              if $==0  then say ' ' VCD  right(x,1)   " for "   x
                       else say 'in'VCD  right(x,1)   " for "   x    '  (should be'   g")"
              end   /*j*/
      return
```

{{out|output|text=  when using the (internal) default inputs:}}

```txt

valid check digit 4  for  5724
invalid check digit 7  for  5727   (should be 4)
  valid check digit 6  for  112946
invalid check digit 0  for  112940   (should be 6)

```



## Ring


```ring
# Project : Damm algorithm

matrix = [[0, 3, 1, 7, 5, 9, 8, 6, 4, 2],
               [7, 0, 9, 2, 1, 5, 4, 8, 6, 3],
               [4, 2, 0, 6, 8, 7, 1, 3, 5, 9],
               [1, 7, 5, 0, 9, 8, 3, 4, 2, 6],
               [6, 1, 2, 3, 0, 4, 5, 9, 7, 8],
               [3, 6, 7, 4, 2, 0, 9, 5, 8, 1],
               [5, 8, 6, 9, 7, 2, 0, 1, 3, 4],
               [8, 9, 4, 5, 3, 6, 2, 0, 1, 7],
               [9, 4, 3, 8, 6, 1, 7, 2, 0, 5],
               [2, 5, 8, 1, 4, 3, 6, 7, 9, 0]]

see "5724" + encode(5724 ) + nl
see "5727" + encode(5727 ) + nl
see "112946" + encode(112946) + nl

func encode(n)
       check = 0
       for d in string(n)
           check = matrix[check+1][d-'0'+1]
       next
       if check = 0
          return " is valid"
       else
          return " is invalid"
       ok
```

Output:

```txt
5724 is valid
5727 is invalid
112946 is valid
```



## Ruby


```ruby
def dammCheck( nbr )
    idx = 0
    for i in 0 .. nbr.length - 1
        a = nbr[i].to_i
        if a == nil then return false end
        idx = @table[idx][a]
    end
    print( "this number is " )
    if idx == 0; print( "valid!" )
    else print( "invalid!" )
    end
    puts
end

@table = Array.new(
    [
        [0,3,1,7,5,9,8,6,4,2], [7,0,9,2,1,5,4,8,6,3],
        [4,2,0,6,8,7,1,3,5,9], [1,7,5,0,9,8,3,4,2,6],
        [6,1,2,3,0,4,5,9,7,8], [3,6,7,4,2,0,9,5,8,1],
        [5,8,6,9,7,2,0,1,3,4], [8,9,4,5,3,6,2,0,1,7],
        [9,4,3,8,6,1,7,2,0,5], [2,5,8,1,4,3,6,7,9,0]
    ]
)

while true
    print( "Number to check: " )
    dammCheck( gets.chomp )
end
```

{{out}}
```txt
Number to check: 5724
this number is valid!
Number to check: 5727
this number is invalid!
Number to check: 112940
this number is invalid!
Number to check: 112946
this number is valid!
Number to check: 1321
this number is valid!
```


## Scala

===Functional, (tail) recursive, concise and clean===

```Scala
import scala.annotation.tailrec

object DammAlgorithm extends App {

  private val numbers = Seq(5724, 5727, 112946, 112949)

  @tailrec
  private def damm(s: String, interim: Int): String = {
    def table =
      Vector(
        Vector(0, 3, 1, 7, 5, 9, 8, 6, 4, 2),
        Vector(7, 0, 9, 2, 1, 5, 4, 8, 6, 3),
        Vector(4, 2, 0, 6, 8, 7, 1, 3, 5, 9),
        Vector(1, 7, 5, 0, 9, 8, 3, 4, 2, 6),
        Vector(6, 1, 2, 3, 0, 4, 5, 9, 7, 8),
        Vector(3, 6, 7, 4, 2, 0, 9, 5, 8, 1),
        Vector(5, 8, 6, 9, 7, 2, 0, 1, 3, 4),
        Vector(8, 9, 4, 5, 3, 6, 2, 0, 1, 7),
        Vector(9, 4, 3, 8, 6, 1, 7, 2, 0, 5),
        Vector(2, 5, 8, 1, 4, 3, 6, 7, 9, 0)
      )

    if (s.isEmpty) if (interim == 0) "✔" else "✘"
    else damm(s.tail, table(interim)(s.head - '0'))
  }

  for (number <- numbers) println(f"$number%6d is ${damm(number.toString, 0)}.")

}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/d25pzoH/0 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/8t9RuipwRHGGoczFXPvT5A Scastie (remote JVM)].

## Sidef


```ruby
func damm(digits) {
    static tbl = [
          [0, 3, 1, 7, 5, 9, 8, 6, 4, 2],
          [7, 0, 9, 2, 1, 5, 4, 8, 6, 3],
          [4, 2, 0, 6, 8, 7, 1, 3, 5, 9],
          [1, 7, 5, 0, 9, 8, 3, 4, 2, 6],
          [6, 1, 2, 3, 0, 4, 5, 9, 7, 8],
          [3, 6, 7, 4, 2, 0, 9, 5, 8, 1],
          [5, 8, 6, 9, 7, 2, 0, 1, 3, 4],
          [8, 9, 4, 5, 3, 6, 2, 0, 1, 7],
          [9, 4, 3, 8, 6, 1, 7, 2, 0, 5],
          [2, 5, 8, 1, 4, 3, 6, 7, 9, 0],
    ]

    !digits.flip.reduce({|row,col| tbl[row][col] }, 0)
}

for n in [5724, 5727, 112946] {
    say "#{n}:\tChecksum digit #{ damm(n.digits) ? '' : 'in'}correct."
}
```

{{out}}

```txt
5724:	Checksum digit correct.
5727:	Checksum digit incorrect.
112946:	Checksum digit correct.
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    ReadOnly table = {
            {0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
            {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
            {4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
            {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
            {6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
            {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
            {5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
            {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
            {9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
            {2, 5, 8, 1, 4, 3, 6, 7, 9, 0}
        }

    Function Damm(s As String) As Boolean
        Dim interim = 0
        For Each c In s
            interim = table(interim, AscW(c) - AscW("0"))
        Next
        Return interim = 0
    End Function

    Sub Main()
        Dim numbers = {5724, 5727, 112946, 112949}
        For Each number In numbers
            Dim isvalid = Damm(number.ToString())
            If isvalid Then
                Console.WriteLine("{0,6} is valid", number)
            Else
                Console.WriteLine("{0,6} is invalid", number)
            End If
        Next
    End Sub

End Module
```

{{out}}

```txt
  5724 is valid
  5727 is invalid
112946 is valid
112949 is invalid
```



## zkl


```zkl
fcn damm(digits){  // digits is something that supports an iterator of integers
   var [const]  tbl=Data(0,Int,		// 10x10 byte bucket
      0, 3, 1, 7, 5, 9, 8, 6, 4, 2,
      7, 0, 9, 2, 1, 5, 4, 8, 6, 3,
      4, 2, 0, 6, 8, 7, 1, 3, 5, 9,
      1, 7, 5, 0, 9, 8, 3, 4, 2, 6,
      6, 1, 2, 3, 0, 4, 5, 9, 7, 8,
      3, 6, 7, 4, 2, 0, 9, 5, 8, 1,
      5, 8, 6, 9, 7, 2, 0, 1, 3, 4,
      8, 9, 4, 5, 3, 6, 2, 0, 1, 7,
      9, 4, 3, 8, 6, 1, 7, 2, 0, 5,
      2, 5, 8, 1, 4, 3, 6, 7, 9, 0);
   0 == digits.reduce(fcn(interim,digit){ tbl[interim*10 + digit]  },0)
}
```


```zkl
damm(List(5,7,2,4)).println();		// True
damm(Data(0,Int,5,7,2,7).howza(0)).println();	// stream bytes, False
damm((112946).split()).println();	// True
```

{{out}}

```txt
True
False
True
```


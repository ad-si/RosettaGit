+++
title = "Thue-Morse"
description = ""
date = 2019-09-20T23:23:06Z
aliases = []
[extra]
id = 19589
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "awk",
  "basic",
  "basic256",
  "bbc_basic",
  "befunge",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elena",
  "elixir",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "newlisp",
  "oasys_assembler",
  "objeck",
  "ocaml",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "sinclair_zx81_basic",
  "sql",
  "tcl",
  "ubasic_4th",
  "vba",
  "xlisp",
  "yabasic",
  "zkl",
]
+++

## Task

Create a [http://en.wikipedia.org/wiki/Thue%E2%80%93Morse_sequence Thue-Morse sequence].


## See also

*   YouTube entry: [https://www.youtube.com/watch?v=prh72BLNjIk The Fairest Sharing Sequence Ever]
*   YouTube entry: [https://www.youtube.com/watch?v=Tt5TTid6YXk Math and OCD - My story with the Thue-Morse sequence]





## Ada

Implementation using an L-system.


```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Thue_Morse is

   function Replace(S: String) return String is
      -- replace every "0" by "01" and every "1" by "10"
      (if S'Length = 0 then ""
      else (if S(S'First) = '0' then "01" else "10") &
	Replace(S(S'First+1 .. S'Last)));

   function Sequence (N: Natural) return String is
      (if N=0 then "0" else Replace(Sequence(N-1)));

begin
   for I in 0 .. 6 loop
      Ada.Text_IO.Put_Line(Integer'Image(I) & ": " & Sequence(I));
   end loop;
end Thue_Morse;
```


```txt
 0: 0
 1: 01
 2: 0110
 3: 01101001
 4: 0110100110010110
 5: 01101001100101101001011001101001
 6: 0110100110010110100101100110100110010110011010010110100110010110
```



## ALGOL 68


```algol68
# "flips" the "bits" in a string (assumed to contain only "0" and "1" characters) #
OP  FLIP = ( STRING s )STRING:
    BEGIN
        STRING result := s;
        FOR char pos FROM LWB result TO UPB result DO
            result[ char pos ] := IF result[ char pos ] = "0" THEN "1" ELSE "0" FI
        OD;
        result
    END; # FLIP #

# print the first few members of the Thue-Morse sequence #
STRING tm := "0";
TO 7 DO
    print( ( tm, newline ) );
    tm +:= FLIP tm
OD
```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## AppleScript


```AppleScript
-- THUE MORSE ----------------------------------------------------------------

-- thueMorse :: Int -> String
on thueMorse(nCycles)
    script concatBinaryInverse
        on |λ|(xs)
            script binaryInverse
                on |λ|(x)
                    1 - x
                end |λ|
            end script

            xs & map(binaryInverse, xs)
        end |λ|
    end script

    intercalate("", ¬
        foldl(concatBinaryInverse, [0], enumFromTo(1, nCycles)))
end thueMorse


-- TEST ----------------------------------------------------------------------
on run

    thueMorse(6)

    --> 0110100110010110100101100110100110010110011010010110100110010110
end run


-- GENERIC LIBRARY FUNCTIONS

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

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
```


```txt
"0110100110010110100101100110100110010110011010010110100110010110"
```



## AWK


```AWK
BEGIN{print x="0"}
{gsub(/./," &",x);gsub(/ 0/,"01",x);gsub(/ 1/,"10",x);print x}
```



## BASIC

=
## BASIC256
=
```BASIC256

tm = "0"

Function Thue_Morse(s)
	k = ""
	For i = 1 To Length(s)
		If Mid(s, i, 1) = "1" Then
			k += "0"
		Else
			k += "1"
		End If
	Next i
	Thue_Morse = s + k
End Function

Print tm
For j = 1 To 7
	tm = Thue_Morse(tm)
	Print tm
Next j
End

```

```txt

Igual que la entrada de FreeBASIC.

```


=
## Sinclair ZX81 BASIC
=

```basic
 10 LET T$="0"
 20 PRINT "T0=";T$
 30 FOR I=1 TO 7
 40 PRINT "T";I;"=";
 50 FOR J=1 TO LEN T$
 60 IF T$(J)="0" THEN GOTO 90
 70 LET T$=T$+"0"
 80 GOTO 100
 90 LET T$=T$+"1"
100 NEXT J
110 PRINT T$
120 NEXT I
```

```txt
T0=0
T1=01
T2=0110
T3=01101001
T4=0110100110010110
T5=01101001100101101001011001101001
T6=0110100110010110100101100110100110010110011010010110100110010110
T7=01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001
```



## BBC BASIC


```bbcbasic>REM
thuemorse
tm$ = "0"
PRINT tm$
FOR i% = 1 TO 8
  tm$ = FN_thue_morse(tm$)
  PRINT tm$
NEXT
END
:
DEF FN_thue_morse(previous$)
LOCAL i%, tm$
tm$ = ""
FOR i% = 1 TO LEN previous$
  IF MID$(previous$, i%, 1) = "1" THEN tm$ += "0" ELSE tm$ += "1"
NEXT
= previous$ + tm$
```

```txt
0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110
01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110100101100110100101101001100101100110100110010110100101100110100110010110011010010110100110010110011010011001011010010110011010010110100110010110100101100110100110010110011010010110100110010110
```



## Befunge

This implements the algorithm that counts the 1 bits in the binary representation of the sequence number.


```befunge
:0\:!v!:\+g20\<>*:*-!#@_
86%2$_:2%02p2/^^82:+1,+*
```


```txt
0110100110010110100101100110100110010110011010010110100110010110100101100110100101101001100101100110100110010110100101100110100110010110011010010110100110010110011010011001011010010110011010010110100110010110100101100110100110010110011010010110100110010110
```



## C


### C: Using string operations

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]){
	char sequence[256+1] = "0";
	char inverse[256+1] = "1";
	char buffer[256+1];
	int i;

	for(i = 0; i < 8; i++){
		strcpy(buffer, sequence);
		strcat(sequence, inverse);
		strcat(inverse, buffer);
	}

	puts(sequence);
	return 0;
}
```

```txt

0110100110010110100101100110100110010110011010010110100110010110100101100110100101101001100101100110100110010110100101100110100110010110011010010110100110010110011010011001011010010110011010010110100110010110100101100110100110010110011010010110100110010110

```




### C: By counting ones in binary representation of an iterator


```c
#include <stdio.h>

/**
 * description : Counts the number of bits set to 1
 *        input: the number to have its bit counted
 *       output: the number of bits set to 1
 */
unsigned count_bits(unsigned v) {
    unsigned c = 0;
    while (v) {
        c += v & 1;
        v >>= 1;
    }

    return c;
}

int main(void) {
    for (unsigned i = 0; i < 256; ++i) {
        putchar('0' + count_bits(i) % 2);
    }
    putchar('\n');

    return 0;
}
```


```txt

0110100110010110100101100110100110010110011010010110100110010110100101100110100101101001100101100110100110010110100101100110100110010110011010010110100110010110011010011001011010010110011010010110100110010110100101100110100110010110011010010110100110010110

```



===C: By counting ones in binary representation of an iterator (w/User options)===

```C> #include <stdio.h


/**
 * description : Counts the number of bits set to 1
 *        input: the number to have its bit counted
 *       output: the number of bits set to 1
 */
unsigned count_bits(unsigned v) {
    unsigned c = 0;
    while (v) {
        c += v & 1;
        v >>= 1;
    }

    return c;
}

int main(void) {
    /*          i: loop iterator
     *     length: the length of the sequence to be printed
     * ascii_base: the lower char for use when printing
     */
    unsigned i, length = 0;
    int ascii_base;


    /* scan in sequence length */
    printf("Sequence length: ");
    do {
        scanf("%u", &length);
    } while (length == 0);


    /* scan in sequence mode */
    printf("(a)lpha or (b)inary: ");
    do {
        ascii_base = getchar();
    } while ((ascii_base != 'a') && (ascii_base != 'b'));
    ascii_base = ascii_base == 'b' ? '0' : 'A';


    /* print the Thue-Morse sequence */
    for (i = 0; i < length; ++i) {
        putchar(ascii_base + count_bits(i) % 2);
    }
    putchar('\n');

    return 0;
}
```


```txt

Sequence length: 256
(a)lpha or (b)inary: b
0110100110010110100101100110100110010110011010010110100110010110100101100110100101101001100101100110100110010110100101100110100110010110011010010110100110010110011010011001011010010110011010010110100110010110100101100110100110010110011010010110100110010110

```



## C++


```cpp

#include <iostream>
#include <iterator>
#include <vector>
int main( int argc, char* argv[] ) {
    std::vector<bool> t;
    t.push_back( 0 );
    size_t len = 1;
    std::cout << t[0] << "\n";
    do {
        for( size_t x = 0; x < len; x++ )
            t.push_back( t[x] ? 0 : 1 );
        std::copy( t.begin(), t.end(), std::ostream_iterator<bool>( std::cout ) );
        std::cout << "\n";
        len = t.size();
    } while( len < 60 );
    return 0;
}

```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## C#

```c#
using System;
using System.Text;

namespace ThueMorse
{
    class Program
    {
        static void Main(string[] args)
        {
            Sequence(6);
        }

        public static void Sequence(int steps)
        {
            var sb1 = new StringBuilder("0");
            var sb2 = new StringBuilder("1");
            for (int i = 0; i < steps; i++)
            {
                var tmp = sb1.ToString();
                sb1.Append(sb2);
                sb2.Append(tmp);
            }
            Console.WriteLine(sb1);
            Console.ReadLine();
        }
    }
}
```


```txt
0110100110010110100101100110100110010110011010010110100110010110
```



## Common Lisp


```lisp
(defun bit-complement (bit-vector)
  (loop with result = (make-array (length bit-vector) :element-type 'bit)
        for b across bit-vector
        for i from 0
        do (setf (aref result i) (- 1 b))
        finally (return result)))

(defun next (bit-vector)
  (concatenate 'bit-vector bit-vector (bit-complement bit-vector)))

(defun print-bit-vector (bit-vector)
  (loop for b across bit-vector
        do (princ b)
        finally (terpri)))

(defun thue-morse (max)
  (loop repeat (1+ max)
        for value = #*0 then (next value)
        do (print-bit-vector value)))

(thue-morse 6)
```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## D

```d
import std.range;
import std.stdio;

struct TM {
    private char[] sequence = ['0'];
    private char[] inverse = ['1'];
    private char[] buffer;

    enum empty = false;

    auto front() {
        return sequence;
    }

    auto popFront() {
        buffer = sequence;
        sequence ~= inverse;
        inverse ~= buffer;
    }
}

void main() {
    TM sequence;

    foreach (step; sequence.take(8)) {
        writeln(step);
    }
}

```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'text;

sequence(int steps)
{
    var sb1 := TextBuilder.load("0");
    var sb2 := TextBuilder.load("1");
    for(int i := 0, i < steps, i += 1)
    {
        var tmp := sb1.Value;
        sb1.write(sb2);
        sb2.write(tmp)
    };
    console.printLine(sb1).readLine()
}

public program()
{
    sequence(6)
}
```

```txt

0110100110010110100101100110100110010110011010010110100110010110

```



## Elixir


```elixir
Enum.reduce(0..6, '0', fn _,s ->
  IO.puts s
  s ++ Enum.map(s, fn c -> if c==?0, do: ?1, else: ?0 end)
end)

# or
Stream.iterate('0', fn s -> s ++ Enum.map(s, fn c -> if c==?0, do: ?1, else: ?0 end) end)
|> Enum.take(7)
|> Enum.each(&IO.puts/1)
```


```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## Factor

```factor
USING: io kernel math math.parser sequences ;

: thue-morse ( seq n -- seq' )
    [ [ ] [ [ 1 bitxor ] map ] bi append ] times ;

: print-tm ( seq -- ) [ number>string ] map "" join print ;

7 <iota> [ { 0 } swap thue-morse print-tm ] each
```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## Fortran

```fortran
program thue_morse
  implicit none
  logical :: f(32) = .false.
  integer :: n = 1

  do
    write(*,*) f(1:n)
    if (n > size(f)/2) exit
    f(n+1:2*n) = .not. f(1:n)
    n = n * 2
  end do

end program thue_morse
```

```txt

  F
  F  T
  F  T  T  F
  F  T  T  F  T  F  F  T
  F  T  T  F  T  F  F  T  T  F  F  T  F  T  T  F
  F  T  T  F  T  F  F  T  T  F  F  T  F  T  T  F  T  F  F  T  F  T  T  F  F  T  T  F  T  F  F  T

```



## FreeBASIC


```freebasic

Dim As String tm = "0"

Function Thue_Morse(s As String) As String
    Dim As String k = ""
    For i As Integer = 1 To Len(s)
        If Mid(s, i, 1) = "1" Then
            k += "0"
        Else
            k += "1"
        End If
    Next i
    Thue_Morse = s + k
End Function

Print tm
For j As Integer = 1 To 7
    tm = Thue_Morse(tm)
    Print tm
Next j
End

```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110
01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001

```



## Go


```go
// prints the first few members of the Thue-Morse sequence

package main

import (
    "fmt"
    "bytes"
)

// sets tmBuffer to the next member of the Thue-Morse sequence
// tmBuffer must contain a valid Thue-Morse sequence member before the call
func nextTMSequenceMember( tmBuffer * bytes.Buffer ) {
    // "flip" the bytes, adding them to the buffer
    for b, currLength, currBytes := 0, tmBuffer.Len(), tmBuffer.Bytes() ; b < currLength; b ++ {
        if currBytes[ b ] == '1' {
            tmBuffer.WriteByte( '0' )
        } else {
            tmBuffer.WriteByte( '1' )
        }
    }
}

func main() {
    var tmBuffer bytes.Buffer
    // initial sequence member is "0"
    tmBuffer.WriteByte( '0' )
    fmt.Println( tmBuffer.String() )
    for i := 2; i <= 7; i ++ {
        nextTMSequenceMember( & tmBuffer )
        fmt.Println( tmBuffer.String() )
    }
}
```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## Haskell


Computing progressively longer prefixes of the sequence,


```haskell
import Control.Monad

thueMorsePxs = ap (++) (map (1-)) `iterate` [0]
  {-
    = iterate ((++) <*> map (1-)) [0]
    = iterate (\ xs -> (++) xs (map (1-) xs)) [0]
    = iterate (\ xs -> xs ++ map (1-) xs) [0]
  -}
```


'''Output:'''

```haskell
~> thueMorsePxs !! 5
[0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1]
```


The infinite sequence itself:


```haskell
thueMorse = [0] ++ g 1
      where
      g i = map (1-) (take i thueMorse) ++ g (i*2)
```


'''Output:'''

```haskell
~> take 33 thueMorse
[0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1]
```



## J


We only show a prefix of the sequence:


```J
   (, -.)@]^:[&0]9
0 1 1 0 1 0 0 1 1 0 0 1 0 1 1 0 1 0 0 1 0 1 1 0 0 1 1 0 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1 0 1 0 0 1 0 1 1 0 1 0 0 1 1 0 0 1 0 1 1 0 1 0 0 1 0 1 1 0 0 1 1 0 1 0 0 1 0 1 1 0 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1 0 1 0 0 1 1 0 0 1 0 1 1 0 1 0 0 1 0 1 1 0 0 1 1 0 1 0 0 1 ...
```


Or, more compactly:


```J
   ' '-.~":(, -.)@]^:[&0]9
0110100110010110100101100110100110010110011010010110100110010110100101100110100101101001100101100110100110010110100101100110100110010110011010010110100110010110011010011001011010010110011010010110100110010110100101100110100110010110011010010110100110010110...
```



## Java


```java
public class ThueMorse {

    public static void main(String[] args) {
        sequence(6);
    }

    public static void sequence(int steps) {
        StringBuilder sb1 = new StringBuilder("0");
        StringBuilder sb2 = new StringBuilder("1");
        for (int i = 0; i < steps; i++) {
            String tmp = sb1.toString();
            sb1.append(sb2);
            sb2.append(tmp);
        }
        System.out.println(sb1);
    }
}
```


```txt
0110100110010110100101100110100110010110011010010110100110010110
```



## Julia

```julia
function thuemorse(len::Int)
    rst = Vector{Int8}(len)
    rst[1] = 0
    i, imax = 2, 1
    while i ≤ len
        while i ≤ len && i ≤ 2 * imax
            rst[i] = 1 - rst[i-imax]
            i += 1
        end
        imax *= 2
    end
    return rst
end

println(join(thuemorse(100)))
```


```txt
0110100110010110100101100110100110010110011010010110100110010110100101100110100101101001100101100110
```



## Kotlin

```scala
// version 1.1.2
fun thueMorse(n: Int): String {
    val sb0 = StringBuilder("0")
    val sb1 = StringBuilder("1")
    (0 until n).forEach {
        val tmp = sb0.toString()
        sb0.append(sb1)
        sb1.append(tmp)
    }
    return sb0.toString()
}

fun main(args: Array<String>) {
    for (i in 0..6) println("$i : ${thueMorse(i)}")
}
```


```txt

0 : 0
1 : 01
2 : 0110
3 : 01101001
4 : 0110100110010110
5 : 01101001100101101001011001101001
6 : 0110100110010110100101100110100110010110011010010110100110010110

```



## JavaScript


### ES5

```JavaScript
(function(steps) {
    'use strict';
    var i, tmp, s1 = '0', s2 = '1';
    for (i = 0; i < steps; i++) {
        tmp = s1;
        s1 += s2;
        s2 += tmp;
    }
    console.log(s1);
})(6);
```


```txt
0110100110010110100101100110100110010110011010010110100110010110
```



### ES6


```JavaScript
(() => {
    'use strict';

    // THUE MORSE

    // thueMorse :: Int -> String
    let thueMorse = nCycles => range(1, Math.abs(nCycles))
            .reduce(a => a.concat(a.map(x => 1 - x)), [0])
            .join('');


    // GENERIC FUNCTION

    // range :: Int -> Int  -> [Int]
    let range = (m, n) => Array.from({
        length: Math.floor((n - m)) + 1
    }, (_, i) => m + i);


    // TEST

    return thueMorse(6);

    // 0110100110010110100101100110100110010110011010010110100110010110

})();

```


```txt
0110100110010110100101100110100110010110011010010110100110010110
```



## Lua


```Lua
ThueMorse = {sequence = "0"}

function ThueMorse:show ()
    print(self.sequence)
end

function ThueMorse:addBlock ()
    local newBlock = ""
    for bit = 1, self.sequence:len() do
        if self.sequence:sub(bit, bit) == "1" then
            newBlock = newBlock .. "0"
        else
            newBlock = newBlock .. "1"
        end
    end
    self.sequence = self.sequence .. newBlock
end

for i = 1, 5 do
    ThueMorse:show()
    ThueMorse:addBlock()
end
```

```txt
0
01
0110
01101001
0110100110010110
```


=={{header|Modula-2}}==

```modula2
MODULE ThueMorse;
FROM Strings IMPORT Concat;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE Sequence(steps : CARDINAL);
TYPE String = ARRAY[0..128] OF CHAR;
VAR sb1,sb2,tmp : String;
    i : CARDINAL;
BEGIN
    sb1 := "0";
    sb2 := "1";

    WHILE i<steps DO
        tmp := sb1;
        Concat(sb1, sb2, sb1);
        Concat(sb2, tmp, sb2);
        INC(i);
    END;
    WriteString(sb1);
    WriteLn;
END Sequence;

BEGIN
    Sequence(6);
    ReadChar;
END ThueMorse.
```



## NewLISP



```newlisp
(define (Thue-Morse loops)
    (setf TM '(0))
    (println TM)
    (for (i 1 (-- loops))
        (setf tmp TM)
        (replace '0 tmp '_)
        (replace '1 tmp '0)
        (replace '_ tmp '1)
        (setf TM (append TM tmp))
        (println TM)
    )
)

(Thue-Morse 5)
(exit)

```


```txt

(0)
(0 1)
(0 1 1 0)
(0 1 1 0 1 0 0 1)
(0 1 1 0 1 0 0 1 1 0 0 1 0 1 1 0)

```



## OASYS Assembler

<lang oasys_oaa>; Thue-Morse sequence

[*'A]               ; Ensure the vocabulary is not empty
[&]                 ; Declare the initialization procedure
%#1>                ; Initialize length counter
%@*>                ; Create first object
,#1>                ; Initialize loop counter
:                   ; Begin loop
  %@<.#<PI          ; Print current cell
  *.#%@<.#<NOT>     ; Create new cell
  %@%@<NXT>         ; Advance to next cell
  ,#,#<DN>          ; Decrement loop counter
  ,#</              ; Check if loop counter is now zero
    %#%#<2MUL>      ; Double length counter
    ,#%#<>          ; Reset loop counter
    %@FO>           ; Reset object pointer
    CR              ; Line break
|                   ; Repeat loop
```

The standard DOS-based interpreter will display an error message about word too long after 7 lines are output; this is because the 8th line does not fit in 80 columns.


## Objeck

```objeck
class ThueMorse {
  function : Main(args : String[]) ~ Nil {
    Sequence(6);
  }

  function : Sequence(steps : Int) ~ Nil {
    sb1 := "0";
    sb2 := "1";
    for(i := 0; i < steps; i++;) {
      tmp := String->New(sb1);
      sb1 += sb2;
      sb2 += tmp;
    };
    sb1->PrintLine();
  }
}

```


Output:

```txt

0110100110010110100101100110100110010110011010010110100110010110

```




## OCaml


### By counting ones in binary representation of an iterator

```ocaml
(* description: Counts the number of bits set to 1
         input: the number to have its bit counted
        output: the number of bits set to 1 *)
let count_bits v =
  let rec aux c v =
    if v <= 0 then c
    else aux (c + (v land 1)) (v lsr 1)
  in
  aux 0 v

let () =
  for i = 0 to pred 256 do
    print_char (
      match (count_bits i) mod 2 with
      | 0 -> '0'
      | 1 -> '1'
      | _ -> assert false)
  done;
  print_newline ()
```




### Using string operations

```ocaml
let sequence steps =
  let sb1 = Buffer.create 100 in
  let sb2 = Buffer.create 100 in
  Buffer.add_char sb1 '0';
  Buffer.add_char sb2 '1';
  for i = 0 to pred steps do
    let tmp = Buffer.contents sb1 in
    Buffer.add_string sb1 (Buffer.contents sb2);
    Buffer.add_string sb2 tmp;
  done;
  (Buffer.contents sb1)

let () =
  print_endline (sequence 6);
```




## Pascal

Like the C++ Version [[http://rosettacode.org/wiki/Thue-Morse#C.2B.2B]] the lenght of the sequence is given in advance.

```pascal
Program ThueMorse;

function fThueMorse(maxLen: NativeInt):AnsiString;
//double by appending the flipped original 0 -> 1;1 -> 0
//Flipping between two values:x oszillating A,B,A,B -> x_next = A+B-x
//Beware A+B < High(Char), the compiler will complain ...
const
  cVal0 = '^';cVal1 = 'v';//  cVal0 = '0';cVal1 = '1';

var
  pOrg,
  pRpl : pChar;
  i,k,ml : NativeUInt;//MaxLen: NativeInt
Begin
  iF maxlen < 1 then
  Begin
    result := '';
    EXIT;
  end;
  //setlength only one time
  setlength(result,Maxlen);

  pOrg := @result[1];
  pOrg[0] := cVal0;
  IF maxlen = 1 then
    EXIT;

  pRpl := pOrg;
  inc(pRpl);
  k := 1;
  ml:= Maxlen;
  repeat
    i := 0;
    repeat
      pRpl[0] := chr(Ord(cVal0)+Ord(cVal1)-Ord(pOrg[i]));
      inc(pRpl);
      inc(i);
    until i>=k;
    inc(k,k);
  until k+k> ml;
  // the rest
  i := 0;
  k := ml-k;
  IF k > 0 then
    repeat
      pRpl[0] := chr(Ord(cVal0)+Ord(cVal1)-Ord(pOrg[i]));
      inc(pRpl);
      inc(i)
    until i>=k;
end;

var
 i : integer;
Begin
  For i := 0 to 8 do
    writeln(i:3,'  ',fThueMorse(i));
  fThueMorse(1 shl 30);
end.
```

```txt
Compile with /usr/lib/fpc/3.0.1/ppc386 "ThueMorse.pas" -al -XX -Xs -O4 -MDelphi
without -O4 -> 2 secs
  0
  1  ^
  2  ^v
  3  ^vv
  4  ^vv^
  5  ^vv^v
  6  ^vv^v^
  7  ^vv^v^^
  8  ^vv^v^^v
not written: 1 shl 30 == 1GB
real  0m0.806s user  0m0.563s sys 0m0.242s
```



## Perl

```perl
sub complement
{
    my $s = shift;

    $s =~ tr/01/10/;

    return $s;
}

my $str = '0';

for (0..6) {
    say $str;
    $str .= complement($str);
}

```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## Perl 6

First 8 of an infinite sequence

```perl6
.say for (0, { '0' ~ @_.join.trans( "01" => "10", :g) } ... *)[^8];
```


```txt
0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110
01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001
^C
```



## Phix


```Phix
string tm = "0"
for i=1 to 8 do
    ?tm
    tm &= sq_sub('0'+'1',tm)
end for
```

```txt

"0"
"01"
"0110"
"01101001"
"0110100110010110"
"01101001100101101001011001101001"
"0110100110010110100101100110100110010110011010010110100110010110"
"01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001"

```



## PicoLisp


```PicoLisp
(let R 0
   (prinl R)
   (for (X 1 (>= 32 X))
      (setq R
         (bin
            (pack
               (bin R)
               (bin (x| (dec (** 2 X)) R)) ) ) )
      (prinl (pack 0 (bin R)))
      (inc 'X X) ) )
```



## PowerShell


```powershell
function New-ThueMorse ( $Digits )
    {
    #  Start with seed 0
    $ThueMorse = "0"

    #  Decrement digits remaining
    $Digits--

    #  While we still have digits to calculate...
    While ( $Digits -gt 0 )
        {
        #  Number of digits we'll get this loop (what we still need up to the maximum available), corrected to 0 base
        $LastDigit = [math]::Min( $ThueMorse.Length, $Digits ) - 1

        #  Loop through each digit
        ForEach ( $i in 0..$LastDigit )
            {
            #  Append the twos complement
            $ThueMorse += ( 1 - $ThueMorse.Substring( $i, 1 ) )
            }

        #  Calculate the number of digits still remaining
        $Digits = $Digits - $LastDigit - 1
        }

    return $ThueMorse
    }

New-ThueMorse 5
New-ThueMorse 16
New-ThueMorse 73
```

```txt
01101
0110100110010110
0110100110010110100101100110100110010110011010010110100110010110100101100
```



## PureBasic

```PureBasic
EnableExplicit

Procedure.i count_bits(v.i)
  Define c.i
  While v
    c+v&1
    v>>1
  Wend
  ProcedureReturn c
EndProcedure

If OpenConsole()
  Define n.i
  For n=0 To 255
    Print(Str(count_bits(n)%2))
  Next
  PrintN(~"\n...fin") : Input()
EndIf
```

```txt
0110100110010110100101100110100110010110011010010110100110010110100101100110100101101001100101100110100110010110100101100110100110010110011010010110100110010110011010011001011010010110011010010110100110010110100101100110100110010110011010010110100110010110
...fin
```



## Python


### Python: By substitution


```Python

m='0'
print(m)
for i in range(0,6):
     m0=m
     m=m.replace('0','a')
     m=m.replace('1','0')
     m=m.replace('a','1')
     m=m0+m
     print(m)

```

```txt
0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110
```



### Python: By counting set ones in binary representation


```Python

>>> def thue_morse_digits(digits):
...     return  [bin(n).count('1') % 2 for n in range(digits)]
...
>>> thue_morse_digits(20)
[0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1]

>>>

```


===Python: By [http://mathworld.wolfram.com/SubstitutionSystem.html substitution system]===

```Python

>>> def thue_morse_subs(chars):
...     ans = '0'
...     while len(ans) < chars:
...         ans = ans.replace('0', '0_').replace('1', '10').replace('_', '1')
...     return ans[:chars]
...
>>> thue_morse_subs(20)
'01101001100101101001'

>>>

```


## R


```rsplus

thue_morse <- function(steps) {
	sb1 <- "0"
	sb2 <- "1"
	for (idx in 1:steps) {
		tmp <- sb1
		sb1 <- paste0(sb1, sb2)
		sb2 <- paste0(sb2, tmp)
	}
	sb1
}
cat(thue_morse(6), "\n")

```

```txt

0110100110010110100101100110100110010110011010010110100110010110

```



## Racket


```racket
#lang racket
(define 1<->0 (match-lambda [#\0 #\1] [#\1 #\0]))
(define (thue-morse-step (s "0"))
  (string-append s (list->string (map 1<->0 (string->list s)))))

(define (thue-morse n)
  (let inr ((n (max (sub1 n) 0)) (rv (list "0")))
    (if (zero? n) (reverse rv) (inr (sub1 n) (cons (thue-morse-step (car rv)) rv)))))

(for-each displayln (thue-morse 7))
```

```txt
0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110
```



## REXX


### using functions

Programming note:   ''pop count''   (or   ''weight'')   is the number of   <b>1</b>'s   bits in the binary representation of a number.

```rexx
/*REXX pgm generates & displays the Thue─Morse sequence up to the Nth term (inclusive). */
parse arg N .                                    /*obtain the optional argument from CL.*/
if N=='' | N==","  then N=80                     /*Not specified?  Then use the default.*/
$=                                               /*the Thue─Morse sequence  (so far).   */
         do j=0  to N                            /*generate sequence up to the Nth item.*/
         $=$ || $weight(j) // 2                  /*append the item to the Thue─Morse seq*/
         end   /*j*/
say $
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$pop:    return  length( space( translate( arg(1), , 0), 0) )     /*count 1's in number.*/
$weight: return  $pop( x2b( d2x( arg(1) ) ) )                     /*dec──►bin, pop count*/
```

'''output'''   when using the default input:

```txt

01101001100101101001011001101001100101100110100101101001100101101001011001101001

```


===using in-line code===

```rexx
/*REXX pgm generates & displays the Thue─Morse sequence up to the Nth term (inclusive). */
parse arg N .                                    /*obtain the optional argument from CL.*/
if N=='' | N==","  then N=80                     /*Not specified?  Then use the default.*/
$=                                               /*the Thue─Morse sequence  (so far).   */
         do j=0  to N                            /*generate sequence up to the Nth item.*/
         $=$ || length( space( translate( x2b( d2x(j) ), , 0), 0) ) // 2  /*append to $.*/
         end   /*j*/
say $                                            /*stick a fork in it,  we're all done. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.


===using 2's complement===
Programming note:   this method displays the sequence, but it doubles in (binary) length each iteration.

Because of this, the displaying of the output lacks the granularity of the first two REXX versions.

```rexx
/*REXX pgm generates & displays the Thue─Morse sequence up to the Nth term (inclusive). */
parse arg N .                                    /*obtain the optional argument from CL.*/
if N=='' | N==","  then N=6                      /*Not specified?  Then use the default.*/
$=0                                              /*the Thue─Morse sequence  (so far).   */
         do j=1  for N                           /*generate sequence up to the Nth item.*/
         $=$ || translate($, 10, 01)             /*append $'s  complement to  $  string.*/
         end   /*j*/
say $                                            /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input:

```txt

0110100110010110100101100110100110010110011010010110100110010110

```



## Ring


```ring

tm = "0"
see tm
for n = 1 to 6
    tm = thue_morse(tm)
    see tm
next

func thue_morse(previous)
     tm = ""
     for i = 1 to len(previous)
         if (substr(previous, i, 1) = "1") tm = tm + "0" else tm  = tm + "1" ok
     next
     see nl
     return (previous + tm)

```

Output:

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## Ruby


```ruby
puts s = "0"
6.times{puts s << s.tr("01","10")}
```


```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## Rust


```rust
const ITERATIONS: usize = 8;

fn neg(sequence: &String) -> String {
    sequence.chars()
        .map(|ch| {
            (1 - ch.to_digit(2).unwrap()).to_string()
        })
        .collect::<String>()
}

fn main() {
    let mut sequence: String = String::from("0");
    for i in 0..ITERATIONS {
        println!("{}: {}", i + 1, sequence);
        sequence = format!("{}{}", sequence, neg(&sequence));
    }
}
```


```txt

1: 0
2: 01
3: 0110
4: 01101001
5: 0110100110010110
6: 01101001100101101001011001101001
7: 0110100110010110100101100110100110010110011010010110100110010110
8: 01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001

```



## Scala


```scala
def thueMorse(n: Int): String = {
  val (sb0, sb1) = (new StringBuilder("0"), new StringBuilder("1"))
  (0 until n).foreach { _ =>
    val tmp = sb0.toString()
    sb0.append(sb1)
    sb1.append(tmp)
  }
  sb0.toString()
}

(0 to 6).foreach(i => println(s"$i : ${thueMorse(i)}"))
```

{{Out}} See it running in your browser by [https://scastie.scala-lang.org/rsF3Y5ABQoK0zZMMA3m6Ow Scastie (JVM)].

## Sidef


```ruby
func recmap(repeat, seed, transform, callback) {
    func (repeat, seed) {
        callback(seed)
        repeat > 0 && __FUNC__(repeat-1, transform(seed))
    }(repeat, seed)
}

recmap(6, "0", {|s| s + s.tr('01', '10') }, { .say })
```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```



## SQL

This example is using SQLite.

```SQL
with recursive a(a) as (select '0' union all select replace(replace(hex(a),'30','01'),'31','10') from a) select * from a;
```

You can add a LIMIT clause to the end to limit how many lines of output you want.


## Tcl


Since <tt>string map</tt> correctly handles overlapping replacements, the simple map <tt>0 -> 01; 1 -> 10</tt> can be applied with no special handling:


```Tcl
proc tm_expand {s} {string map {0 01 1 10} $s}
# this could also be written as:
# interp alias {} tm_expand {} string map {0 01 1 10}

proc tm {k} {
    set s 0
    while {[incr k -1] >= 0} {
        set s [tm_expand $s]
    }
    return $s
}
```


Testing:


```Tcl
for {set i 0} {$i <= 6} {incr i} {
    puts [tm $i]
}
```


```txt
0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110
```


For giggles, also note that the above SQL solution can be "natively" applied in Tcl8.5+, which bundles Sqlite as a core extension:


```Tcl

package require sqlite3 ;# available with Tcl8.5+ core
sqlite3 db ""           ;# create in-memory database
set LIMIT 6
db eval {with recursive a(a) as (select '0' union all select replace(replace(hex(a),'30','01'),'31','10') from a) select a from a limit $LIMIT} {
    puts $a
}
```



## uBasic/4tH

<lang>For x = 0 to 6                         ' sequence loop
  Print Using "_#";x;": ";             ' print sequence
  For y = 0 To (2^x)-1                 ' element loop
    Print AND(FUNC(_Parity(y)),1);     ' print element
  Next                                 ' next element
  Print                                ' terminate elements line
Next                                   ' next sequence

End

_Parity Param (1)                      ' parity function
  Local (1)                            ' number of bits set
  b@ = 0                               ' no bits set yet
  Do While a@ # 0                      ' until all bits are counted
    If AND (a@, 1) Then b@ = b@ + 1    ' bit set? increment count
    a@ = SHL(a@, -1)                   ' shift the number
  Loop
Return (b@)                            ' return number of bits set
```

```txt
 0: 0
 1: 01
 2: 0110
 3: 01101001
 4: 0110100110010110
 5: 01101001100101101001011001101001
 6: 0110100110010110100101100110100110010110011010010110100110010110

0 OK, 0:123
```



## VBA


```vb
Option Explicit

Sub Main()
Dim i&, t$
    For i = 1 To 8
        t = Thue_Morse(t)
        Debug.Print i & ":=> " & t
    Next
End Sub

Private Function Thue_Morse(s As String) As String
Dim k$
    If s = "" Then
        k = "0"
    Else
        k = s
        k = Replace(k, "1", "2")
        k = Replace(k, "0", "1")
        k = Replace(k, "2", "0")
    End If
    Thue_Morse = s & k
End Function
```

```txt
1:=> 0
2:=> 01
3:=> 0110
4:=> 01101001
5:=> 0110100110010110
6:=> 01101001100101101001011001101001
7:=> 0110100110010110100101100110100110010110011010010110100110010110
8:=> 01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001
```



## XLISP


```lisp
(defun thue-morse (n)
    (defun flip-bits (s)
        (defun flip (l)
            (if (not (null l))
                (cons
                    (if (equal (car l) #\1)
                        #\0
                        #\1)
                    (flip (cdr l)))))
        (list->string (flip (string->list s))))
    (if (= n 0)
        "0"
        (string-append (thue-morse (- n 1)) (flip-bits (thue-morse (- n 1))))))

; define RANGE, for testing purposes

(defun range (x y)
    (if (< x y)
        (cons x (range (+ x 1) y))))

; test THUE-MORSE by printing the strings it returns for n = 0 to n = 6

(mapcar (lambda (n) (print (thue-morse n))) (range 0 7))
```

```txt
"0"
"01"
"0110"
"01101001"
"0110100110010110"
"01101001100101101001011001101001"
"0110100110010110100101100110100110010110011010010110100110010110"
```



## Yabasic

```Yabasic
tm$ = "0"
for i=1 to 8
    ? tm$
    tm$ = tm$ + inverte$(tm$)
next

sub inverte$(tm$)
    local i

    for i = 1 to len(tm$)
        mid$(tm$, i, 1) = str$(not val(mid$(tm$, i, 1)))
    next
    return tm$
end sub
```



## zkl


```zkl
fcn nextTM(str){ str.pump(str,'-.fp("10")) } // == fcn(c){ "10" - c }) }
```

"12233334444" - "23"-->"14444"

```zkl
str:="0"; do(7){ str=nextTM(str.println()) }
```

println() returns the result it prints (as a string).
```zkl
fcn nextTM2{
   var sb1=Data(Void,"0"), sb2=Data(Void,"1");
   r:=sb1.text; sb1.append(sb2); sb2.append(r);
   r
}
```


```zkl
do(7){ nextTM2().println() }
```

```txt

0
01
0110
01101001
0110100110010110
01101001100101101001011001101001
0110100110010110100101100110100110010110011010010110100110010110

```


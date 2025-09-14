+++
title = "Look-and-say sequence"
description = ""
date = 2019-10-18T05:02:29Z
aliases = []
[extra]
id = 4055
[taxonomies]
categories = ["task", "Text processing"]
tags = []
languages = [
  "ada",
  "algol_68",
  "apl",
  "autohotkey",
  "awk",
  "basic256",
  "bbc_basic",
  "bracmat",
  "c",
  "ceylon",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "e",
  "echolisp",
  "elixir",
  "erlang",
  "erre",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "gap",
  "go",
  "groovy",
  "haskell",
  "haxe",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "livecode",
  "logo",
  "lua",
  "m4",
  "mathematica",
  "maxima",
  "maxscript",
  "metafont",
  "miniscript",
  "ocaml",
  "oforth",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powerbasic",
  "powershell",
  "prolog",
  "pure",
  "purebasic",
  "python",
  "q",
  "r",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "smalltalk",
  "snobol4",
  "sql",
  "sql_pl",
  "swift",
  "tcl",
  "tuscript",
  "unix_shell",
  "ursala",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "yabasic",
  "yorick",
  "zkl",
]
+++

The   [[wp:Look and say sequence|Look and say sequence]]   is a recursively defined sequence of numbers studied most notably by   [[wp:John Horton Conway|John Conway]].


'''Sequence Definition'''
* Take a decimal number
* ''Look'' at the number, visually grouping consecutive runs of the same digit.
* ''Say'' the number, from left to right, group by group; as how many of that digit there are - followed by the digit grouped.
: This becomes the next number of the sequence.


'''An example:'''
* Starting with the number 1,   you have ''one'' 1 which produces 11
* Starting with 11,   you have ''two'' 1's.   I.E.:   21
* Starting with 21,   you have ''one'' 2, then ''one'' 1.   I.E.:   (12)(11) which becomes 1211
* Starting with 1211,   you have ''one'' 1, ''one'' 2, then ''two'' 1's.   I.E.:   (11)(12)(21) which becomes 111221


## Task

Write a program to generate successive members of the look-and-say sequence.


## Related tasks

*   [[Fours is the number of letters in the ...]]
*   [[Number names]]
*   [[Self-describing numbers]]
*   [[Self-referential sequence]]
*   [[Spelling of ordinal numbers]]


## See also

*   [https://www.youtube.com/watch?v=ea7lJkEhytA Look-and-Say Numbers (feat John Conway)], A Numberphile Video.
*   This task is related to, and an application of, the [[Run-length encoding]] task.
*   Sequence [https://oeis.org/A005150 A005150] on  The On-Line Encyclopedia of Integer Sequences.





## Ada


```ada
with Ada.Text_IO, Ada.Strings.Fixed;
use  Ada.Text_IO, Ada.Strings, Ada.Strings.Fixed;

function "+" (S : String) return String is
   Item : constant Character := S (S'First);
begin
   for Index in S'First + 1..S'Last loop
      if Item /= S (Index) then
         return Trim (Integer'Image (Index - S'First), Both) & Item & (+(S (Index..S'Last)));
      end if;
   end loop;
   return Trim (Integer'Image (S'Length), Both) & Item;
end "+";
```

This function can be used as follows:

```ada
Put_Line (+"1");
Put_Line (+(+"1"));
Put_Line (+(+(+"1")));
Put_Line (+(+(+(+"1"))));
Put_Line (+(+(+(+(+"1")))));
Put_Line (+(+(+(+(+(+"1"))))));
Put_Line (+(+(+(+(+(+(+"1")))))));
Put_Line (+(+(+(+(+(+(+(+"1"))))))));
Put_Line (+(+(+(+(+(+(+(+(+"1")))))))));
Put_Line (+(+(+(+(+(+(+(+(+(+"1"))))))))));
```

```txt

11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221

```



## ALGOL 68

```algol68
OP + = (STRING s)STRING:
BEGIN
   CHAR item = s[LWB s];
   STRING out;
   FOR index FROM LWB s + 1 TO UPB s DO
      IF item /= s [index] THEN
         out := whole(index - LWB s, 0) + item + (+(s [index:UPB s]));
         GO TO return out
      FI
   OD;
   out := whole (UPB s, 0) + item;
   return out: out
END  # + #;

OP + = (CHAR s)STRING:
  + STRING(s);

print ((+"1", new line));
print ((+(+"1"), new line));
print ((+(+(+"1")), new line));
print ((+(+(+(+"1"))), new line));
print ((+(+(+(+(+"1")))), new line));
print ((+(+(+(+(+(+"1"))))), new line));
print ((+(+(+(+(+(+(+"1")))))), new line));
print ((+(+(+(+(+(+(+(+"1"))))))), new line));
print ((+(+(+(+(+(+(+(+(+"1")))))))), new line));
print ((+(+(+(+(+(+(+(+(+(+"1"))))))))), new line))
```

```txt

11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221

```



## AutoHotkey


```autohotkey
AutoExecute:
    Gui, -MinimizeBox
    Gui, Add, Edit, w500 r20 vInput, 1
    Gui, Add, Button, x155 w100 Default, &Calculate
    Gui, Add, Button, xp+110 yp wp, E&xit
    Gui, Show,, Look-and-Say sequence
Return


ButtonCalculate:
    Gui, Submit, NoHide
    GuiControl,, Input, % LookAndSay(Input)
Return


GuiClose:
ButtonExit:
    ExitApp
Return


;---------------------------------------------------------------------------
LookAndSay(Input) {
;---------------------------------------------------------------------------
    ; credit for this function goes to AutoHotkey forum member Laslo
    ; http://www.autohotkey.com/forum/topic44657-161.html
    ;-----------------------------------------------------------------------
    Loop, Parse, Input          ; look at every digit
        If (A_LoopField = d)    ; I've got another one! (of the same value)
            c += 1                  ; Let's count them ...
        Else {                  ; No, this one is different!
            r .= c d                ; remember what we've got so far
            c := 1                  ; It is the first one in a row
            d := A_LoopField        ; Which one is it?
        }
    Return, r c d
}
```



## APL


```apl

  ⎕IO←0
  d←{(1↓⍵)-¯1↓⍵}
  f←{m←(0≠d ⍵),1 ⋄ ,(d ¯1,m/⍳⍴⍵),[.5](m/⍵)}
  {(f⍣⍵) ,1}¨⍳10

```



## AWK



```awk
function lookandsay(a)
{
  s = ""
  c = 1
  p = substr(a, 1, 1)
  for(i=2; i <= length(a); i++) {
    if ( p == substr(a, i, 1) ) {
      c++
    } else {
      s = s sprintf("%d%s", c, p)
      p = substr(a, i, 1)
      c = 1
    }
  }
  s = s sprintf("%d%s", c, p)
  return s
}

BEGIN {
  b = "1"
  print b
  for(k=1; k <= 10; k++) {
    b = lookandsay(b)
    print b
  }
}
```



## BASIC256


```BASIC256

# look and say

dim a$(2)

i = 0  # input string index

a$[i] = "1"

print a$[i]

for n=1 to 10
  j = 1 - i  # output string index
  a$[j] = ""
  k = 1
  while (k <= length(a$[i]))
    k0 = k + 1
    while ((k0 <= length(a$[i])) and (mid(a$[i], k, 1) = mid(a$[i], k0, 1)))
      k0 = k0 + 1
    end while
    a$[j] += string(k0 - k) + mid(a$[i], k, 1)
    k = k0
  end while
  i = j
  print a$[j]
next n

```



## BBC BASIC


```bbcbasic
      number$ = "1"
      FOR i% = 1 TO 10
        number$ = FNlooksay(number$)
        PRINT number$
      NEXT
      END

      DEF FNlooksay(n$)
      LOCAL i%, j%, c$, o$
      i% = 1
      REPEAT
        c$ = MID$(n$,i%,1)
        j% = i% + 1
        WHILE MID$(n$,j%,1) = c$
          j% += 1
        ENDWHILE
        o$ += STR$(j%-i%) + c$
        i% = j%
      UNTIL i% > LEN(n$)
      = o$
```

```txt

11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221

```



## Bracmat

In this example we use a non-linear pattern and a negation of a pattern: the end of e sequence of equal digits is (1) the end of the string or (2) the start of a sequence starting with a different digit.

```bracmat
( 1:?number
& 0:?lines
&   whl
  ' ( 1+!lines:~>10:?lines
    & :?say                           { This will accumulate all that has to be said after one iteration. }
    & 0:?begin
    & ( @( !number                    { Pattern matching. The '@' indicates we're looking in a string rather than a tree structure. }
         :   ?
             (   [!begin
                 %@?digit
                 ?
                 [?end
                 ( (|(%@:~!digit) ?)  { The %@ guarantees we're testing one character - not less (%) and not more (@). The ? takes the rest. }
                 & !say !end+-1*!begin !digit:?say
                 & !end:?begin        { When backtracking, 'begin' advances to the begin of the next sequence, or to the end of the string. }
                 )
             & ~                      { fail! This forces backtracking. Backtracking stops when all begin positions have been tried. }
             )
         )
      | out$(str$!say:?number)        { After backtracking, output string and set number to string for next iteration. }
      )
    )
);
```

```txt
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
```



## C

This program will not stop until killed or running out of memory.

```c
#include <stdio.h>
#include <stdlib.h>

int main()
{
	char *a = malloc(2), *b = 0, *x, c;
	int cnt, len = 1;

	for (sprintf(a, "1"); (b = realloc(b, len * 2 + 1)); a = b, b = x) {
		puts(x = a);
		for (len = 0, cnt = 1; (c = *a); ) {
			if (c == *++a)
				cnt++;
			else if (c) {
				len += sprintf(b + len, "%d%c", cnt, c);
				cnt = 1;
			}
		}
	}

	return 0;
}
```



## C++


```cpp
#include <iostream>
#include <sstream>
#include <string>

std::string lookandsay(const std::string& s)
{
    std::ostringstream r;

    for (std::size_t i = 0; i != s.length();) {
        auto new_i = s.find_first_not_of(s[i], i + 1);

        if (new_i == std::string::npos)
            new_i = s.length();

        r << new_i - i << s[i];
        i = new_i;
    }
    return r.str();
}

int main()
{
    std::string laf = "1";

    std::cout << laf << '\n';
    for (int i = 0; i < 10; ++i) {
        laf = lookandsay(laf);
        std::cout << laf << '\n';
    }
}
```


## C#

```c#
using System;
using System.Text;
using System.Linq;

class Program
{
    static string lookandsay(string number)
    {
        StringBuilder result = new StringBuilder();

        char repeat = number[0];
        number = number.Substring(1, number.Length-1)+" ";
        int times = 1;

        foreach (char actual in number)
        {
            if (actual != repeat)
            {
                result.Append(Convert.ToString(times)+repeat);
                times = 1;
                repeat = actual;
            }
            else
            {
                times += 1;
            }
        }
        return result.ToString();
    }

    static void Main(string[] args)
    {
        string num = "1";

        foreach (int i in Enumerable.Range(1, 10)) {
             Console.WriteLine(num);
             num = lookandsay(num);
        }
    }
}
```


```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
```


Alternate version using Regex (C#2 syntax only):

```c#

using System;
using System.Text.RegularExpressions;

namespace RosettaCode_Cs_LookAndSay
{
    public class Program
    {
        public static int Main(string[] args)
        {
            Array.Resize<string>(ref args, 2);
            string ls = args[0] ?? "1";
            int n;
            if (!int.TryParse(args[1], out n)) n = 10;
            do {
                Console.WriteLine(ls);
                if (--n <= 0) break;
                ls = say(look(ls));
            } while(true);

            return 0;
        }

        public static string[] look(string input)
        {
            int i = -1;
            return Array.FindAll(Regex.Split(input, @"((\d)\2*)"),
                delegate(string p) { ++i; i %= 3; return i == 1; }
            );
        }

        public static string say(string[] groups)
        {
            return string.Concat(
                Array.ConvertAll<string, string>(groups,
                    delegate(string p) { return string.Concat(p.Length, p[0]); }
                )
            );
        }
    }
}
```

{{out}} (with args: 1 15):

```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221
11131221131211131231121113112221121321132132211331222113112211
311311222113111231131112132112311321322112111312211312111322212311322113212221
```



## Ceylon


```ceylon
shared void run() {

    function lookAndSay(Integer|String input) {

        variable value digits = if (is Integer input) then input.string else input;
        value builder = StringBuilder();

        while (exists currentChar = digits.first) {
            if (exists index = digits.firstIndexWhere((char) => char != currentChar)) {
                digits = digits[index...];
                builder.append("``index````currentChar``");
            }
            else {
                builder.append("``digits.size````currentChar``");
                break;
            }
        }

        return builder.string;
    }

    variable String|Integer result = 1;
    print(result);
    for (i in 1..14) {
        result = lookAndSay(result);
        print(result);
    }
}
```



## Clojure

No ugly int-to-string-and-back conversions.


```clojure
(defn digits-seq
  "Returns a seq of the digits of a number (L->R)."
  [n]
  (loop [digits (), number n]
    (if (zero? number) (seq digits)
        (recur (cons (mod number 10) digits)
               (quot number 10)))))

(defn join-digits
  "Converts a digits-seq back in to a number."
  [ds]
  (reduce (fn [n d] (+ (* 10 n) d)) ds))

(defn look-and-say [n]
  (->> n digits-seq (partition-by identity)
       (mapcat (juxt count first)) join-digits))
```

```clojure>user
 (take 8 (iterate look-and-say 1))
(1 11 21 1211 111221 312211 13112221 1113213211)
```



## Common Lisp


```lisp
(defun compress (array &key (test 'eql) &aux (l (length array)))
  "Compresses array by returning a list of conses each of whose car is
a number of occurrences and whose cdr is the element occurring.  For
instance, (compress \"abb\") produces ((1 . #\a) (2 . #\b))."
  (if (zerop l) nil
    (do* ((i 1 (1+ i))
          (segments (acons 1 (aref array 0) '())))
         ((eql i l) (nreverse segments))
      (if (funcall test (aref array i) (cdar segments))
        (incf (caar segments))
        (setf segments (acons 1 (aref array i) segments))))))

(defun next-look-and-say (number)
  (reduce #'(lambda (n pair)
              (+ (* 100 n)
                 (* 10 (car pair))
                 (parse-integer (string (cdr pair)))))
          (compress (princ-to-string number))
          :initial-value 0))
```


Example use:


```lisp
(next-look-and-say 9887776666) ;=> 19283746
```


Straight character counting:

```lisp
(defun look-and-say (s)
   (let ((out (list (char s 0) 0)))
     (loop for x across s do
	   (if (char= x (first out))
	     (incf (second out))
	     (setf out (list* x 1 out))))
     (format nil "~{~a~^~}" (nreverse out))))

(loop for s = "1" then (look-and-say s)
      repeat 10
      do (write-line s))
```



## D


### Short Functional Version


```d
import std.stdio, std.algorithm, std.range;

enum say = (in string s) pure => s.group.map!q{ text(a[1],a[0]) }.join;

void main() {
    "1".recurrence!((t, n) => t[n - 1].say).take(8).writeln;
}
```

```txt
["1", "11", "21", "1211", "111221", "312211", "13112221", "1113213211"]
```



### Fast Imperative Version

Same output.

```d
import core.stdc.stdio, std.math, std.conv, std.algorithm, std.array;

void showLookAndSay(bool showArrays)(in uint n) nothrow {
    if (n == 0) // No sequences to generate and show.
        return;

    enum Digit : char { nil = '\0', one = '1', two = '2', thr = '3' }

    // Allocate an approximate upper bound size for the array.
    static Digit* allocBuffer(in uint m) nothrow {
        immutable len = cast(size_t)(100 + 1.05 *
                                     exp(0.269 * m + 0.2686)) + 1;
        auto a = len.uninitializedArray!(Digit[]);
        printf("Allocated %d bytes.\n", a.length * Digit.sizeof);
        return a.ptr;
    }

    // Can't be expressed in the D type system:
    // a1 and a2 are immutable pointers to mutable data.
    auto a1 = allocBuffer(n % 2 ? n : n - 1);
    auto a2 = allocBuffer(n % 2 ? n - 1 : n);
    printf("\n");

    a1[0] = Digit.one;
    size_t len1 = 1;
    a1[len1] = Digit.nil;

    foreach (immutable i; 0 .. n - 1) {
        static if (showArrays)
            printf("%2u: %s\n", i + 1, a1);
        else
            printf("%2u: n. digits: %u\n", i + 1, len1);
        auto p1 = a1,
             p2 = a2;

        S0: final switch (*p1++) with (Digit) { // Initial state.
                case nil: goto END;
                case one: goto S1;
                case two: goto S2;
                case thr: goto S3;
            }
        S1: final switch (*p1++) with (Digit) {
                case nil: *p2++ = one; *p2++ = one; goto END;
                case one: goto S11;
                case two: *p2++ = one; *p2++ = one; goto S2;
                case thr: *p2++ = one; *p2++ = one; goto S3;
            }
        S2: final switch (*p1++) with (Digit) {
                case nil: *p2++ = one; *p2++ = two; goto END;
                case one: *p2++ = one; *p2++ = two; goto S1;
                case two: goto S22;
                case thr: *p2++ = one; *p2++ = two; goto S3;
            }
        S3: final switch (*p1++) with (Digit) {
                case nil: *p2++ = one; *p2++ = thr; goto END;
                case one: *p2++ = one; *p2++ = thr; goto S1;
                case two: *p2++ = one; *p2++ = thr; goto S2;
                case thr: goto S33;
            }
        S11: final switch (*p1++) with (Digit) {
                case nil: *p2++ = two; *p2++ = one; goto END;
                case one: *p2++ = thr; *p2++ = one; goto S0;
                case two: *p2++ = two; *p2++ = one; goto S2;
                case thr: *p2++ = two; *p2++ = one; goto S3;
            }
        S22: final switch (*p1++) with (Digit) {
                case nil: *p2++ = two; *p2++ = two; goto END;
                case one: *p2++ = two; *p2++ = two; goto S1;
                case two: *p2++ = thr; *p2++ = two; goto S0;
                case thr: *p2++ = two; *p2++ = two; goto S3;
            }
        S33: final switch (*p1++) with (Digit) {
                case nil: *p2++ = two; *p2++ = thr; goto END;
                case one: *p2++ = two; *p2++ = thr; goto S1;
                case two: *p2++ = two; *p2++ = thr; goto S2;
                case thr: *p2++ = thr; *p2++ = thr; goto S0;
            }
        END:
            immutable len2 = p2 - a2;
            a2[len2] = Digit.nil;
            a1.swap(a2);
            len1 = len2;
    }

    static if (showArrays)
        printf("%2u: %s\n", n, a1);
    else
        printf("%2u: n. digits: %u\n", n, len1);
}

void main(in string[] args) {
    immutable n = (args.length == 2) ? args[1].to!uint : 10;
    n.showLookAndSay!true;
}
```

```txt
Allocated 116 bytes.
Allocated 121 bytes.

 1: 1
 2: 11
 3: 21
 4: 1211
 5: 111221
 6: 312211
 7: 13112221
 8: 1113213211
 9: 31131211131221
10: 13211311123113112211
```


With:

```d
70.showLookAndSay!false;
```

```txt
Allocated 158045069 bytes.
Allocated 206826462 bytes.

 1: n. digits: 1
 2: n. digits: 2
 3: n. digits: 2
 4: n. digits: 4
 5: n. digits: 6
...
60: n. digits: 12680852
61: n. digits: 16530884
62: n. digits: 21549544
63: n. digits: 28091184
64: n. digits: 36619162
65: n. digits: 47736936
66: n. digits: 62226614
67: n. digits: 81117366
68: n. digits: 105745224
69: n. digits: 137842560
70: n. digits: 179691598
```

Using the LDC2 compiler with n=70 the run-time is about 3.74 seconds.


### Intermediate Version

This mostly imperative version is intermediate in both speed and code size:

```d
void main(in string[] args) {
    import std.stdio, std.conv, std.algorithm, std.array, std.string;

    immutable n = (args.length == 2) ? args[1].to!uint : 10;
    if (n == 0)
        return;

    auto seq = ['1'];
    writefln("%2d: n. digits: %d", 1, seq.length);
    foreach (immutable i; 2 .. n + 1) {
        Appender!(typeof(seq)) result;
        foreach (const digit, const count; seq.representation.group) {
            result ~= "123"[count - 1];
            result ~= digit;
        }
        seq = result.data;
        writefln("%2d: n. digits: %d", i, seq.length);
    }
}
```

The output is the same as the second version.

If you modify the first program to print only the lengths of the strings
(with a <code>.map!(s => s.length)</code>),
compiling with LDC2 the run-times of the three versions
with n=55 are about 31.1, 0.10 and 0.23 seconds.


### More Direct Version

Translated and modified from C code by Reddit user "skeeto":
http://www.reddit.com/r/dailyprogrammer/comments/2ggy30/9152014_challenge180_easy_looknsay/

Using ideas from:
http://www.njohnston.ca/2010/10/a-derivation-of-conways-degree-71-look-and-say-polynomial/

This recursive version is able to generate very large sequences in a short time without memory for the intermediate sequence (and with stack space proportional to the sequence order).


```d
import core.stdc.stdio, std.conv;

// On Windows this uses the printf from the Microsoft C runtime,
// that doesn't handle real type and some of the C99 format
// specifiers, but it's faster for bulk printing.
version (LDC) version (Windows)
extern(C) nothrow @nogc int printf(const char*, ...);

// http://www.njohnston.ca/2010/10/a-derivation-of-conways-degree-71-look-and-say-polynomial/
struct Sequence {
    string seq;
    uint[6] next;
}

immutable Sequence[93] sequences = [
    {"", []},
    {"1112", [63]},
    {"1112133", [64, 62]},
    {"111213322112", [65]},
    {"111213322113", [66]},
    {"1113", [68]},
    {"11131", [69]},
    {"111311222112", [84, 55]},
    {"111312", [70]},
    {"11131221", [71]},
    {"1113122112", [76]},
    {"1113122113", [77]},
    {"11131221131112", [82]},
    {"111312211312", [78]},
    {"11131221131211", [79]},
    {"111312211312113211", [80]},
    {"111312211312113221133211322112211213322112", [81, 29, 91]},
    {"111312211312113221133211322112211213322113", [81, 29, 90]},
    {"11131221131211322113322112", [81, 30]},
    {"11131221133112", [75, 29, 92]},
    {"1113122113322113111221131221", [75, 32]},
    {"11131221222112", [72]},
    {"111312212221121123222112", [73]},
    {"111312212221121123222113", [74]},
    {"11132", [83]},
    {"1113222", [86]},
    {"1113222112", [87]},
    {"1113222113", [88]},
    {"11133112", [89, 92]},
    {"12", [1]},
    {"123222112", [3]},
    {"123222113", [4]},
    {"12322211331222113112211", [2, 61, 29, 85]},
    {"13", [5]},
    {"131112", [28]},
    {"13112221133211322112211213322112", [24, 33, 61, 29, 91]},
    {"13112221133211322112211213322113", [24, 33, 61, 29, 90]},
    {"13122112", [7]},
    {"132", [8]},
    {"13211", [9]},
    {"132112", [10]},
    {"1321122112", [21]},
    {"132112211213322112", [22]},
    {"132112211213322113", [23]},
    {"132113", [11]},
    {"1321131112", [19]},
    {"13211312", [12]},
    {"1321132", [13]},
    {"13211321", [14]},
    {"132113212221", [15]},
    {"13211321222113222112", [18]},
    {"1321132122211322212221121123222112", [16]},
    {"1321132122211322212221121123222113", [17]},
    {"13211322211312113211", [20]},
    {"1321133112", [6, 61, 29, 92]},
    {"1322112", [26]},
    {"1322113", [27]},
    {"13221133112", [25, 29, 92]},
    {"1322113312211", [25, 29, 67]},
    {"132211331222113112211", [25, 29, 85]},
    {"13221133122211332", [25, 29, 68, 61, 29, 89]},
    {"22", [61]},
    {"3", [33]},
    {"3112", [40]},
    {"3112112", [41]},
    {"31121123222112", [42]},
    {"31121123222113", [43]},
    {"3112221", [38, 39]},
    {"3113", [44]},
    {"311311", [48]},
    {"31131112", [54]},
    {"3113112211", [49]},
    {"3113112211322112", [50]},
    {"3113112211322112211213322112", [51]},
    {"3113112211322112211213322113", [52]},
    {"311311222", [47, 38]},
    {"311311222112", [47, 55]},
    {"311311222113", [47, 56]},
    {"3113112221131112", [47, 57]},
    {"311311222113111221", [47, 58]},
    {"311311222113111221131221", [47, 59]},
    {"31131122211311122113222", [47, 60]},
    {"3113112221133112", [47, 33, 61, 29, 92]},
    {"311312", [45]},
    {"31132", [46]},
    {"311322113212221", [53]},
    {"311332", [38, 29, 89]},
    {"3113322112", [38, 30]},
    {"3113322113", [38, 31]},
    {"312", [34]},
    {"312211322212221121123222113", [36]},
    {"312211322212221121123222112", [35]},
    {"32112", [37]}
];

void evolve(in uint seq, in uint n) nothrow @nogc {
    if (n <= 0) {
        printf(sequences[seq].seq.ptr);
    } else {
        foreach (immutable next; sequences[seq].next) {
            if (next == 0)
                break;
            evolve(next, n - 1);
        }
    }
}

void main(in string[] args) {
    immutable uint n = (args.length != 2) ? 10 : args[1].to!uint;

    immutable base = 8;
    immutable string[base] results = ["", "1", "11", "21", "1211",
                                      "111221", "312211", "13112221"];
    if (n < base) {
        printf("%s\n", results[n].ptr);
        return;
    }

    evolve(24, n - base);
    evolve(39, n - base);
    '\n'.putchar;
}
```



## E


```e
def lookAndSayNext(number :int) {
  var seen := null
  var count := 0
  var result := ""
  def put() {
    if (seen != null) {
      result += count.toString(10) + E.toString(seen)
    }
  }
  for ch in number.toString(10) {
    if (ch != seen) {
      put()
      seen := ch
      count := 0
    }
    count += 1
  }
  put()
  return __makeInt(result, 10)
}

var number := 1
for _ in 1..20 {
  println(number)
  number := lookAndSayNext(number)
}
```



## EchoLisp


```scheme

(lib 'math) ;; for (number->list) = explode function
(lib 'list) ;; (group)

(define (next L)
	(for/fold (acc null) ((g (group L)))
		(append acc (list (length g) (first g)))))


(define (task n starter)
(for/fold (L (number->list starter)) ((i n))
	(writeln (list->string L))
	(next L)))


```

```scheme

(task 10 1)
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211

```



## Elixir


```elixir
defmodule LookAndSay do
  def next(n) do
    Enum.chunk_by(to_char_list(n), &(&1))
    |> Enum.map(fn cl=[h|_] -> Enum.concat(to_char_list(length cl), [h]) end)
    |> Enum.concat
    |> List.to_integer
  end

  def sequence_from(n) do
    Stream.iterate n, &(next/1)
  end

  def main([start_str|_]) do
    {start_val,_} = Integer.parse(start_str)
    IO.inspect sequence_from(start_val) |> Enum.take 9
  end

  def main([]) do
    main(["1"])
  end
end

LookAndSay.main(System.argv)
```


```txt
[1, 11, 21, 1211, 111221, 312211, 13112221, 1113213211, 31131211131221]
```


'''Regex version:'''

```elixir
defmodule RC do
  def look_and_say(n) do
    Regex.replace(~r/(.)\1*/, to_string(n), fn x,y -> [to_string(String.length(x)),y] end)
    |> String.to_integer
  end
end

IO.inspect Enum.reduce(1..9, [1], fn _,acc -> [RC.look_and_say(hd(acc)) | acc] end) |> Enum.reverse
```


```txt

[1, 11, 21, 1211, 111221, 312211, 13112221, 1113213211, 31131211131221,
 13211311123113112211]

```



## Erlang


```erlang
-module(str).
-export([look_and_say/1, look_and_say/2]).

%% converts a single number
look_and_say([H|T]) -> lists:reverse(look_and_say(T,H,1,"")).

%% converts and accumulates as a loop
look_and_say(_, 0) -> [];
look_and_say(Start, Times) when Times > 0 ->
    [Start | look_and_say(look_and_say(Start), Times-1)].

%% does the actual conversion for a number
look_and_say([], Current, N, Acc) ->
    [Current, $0+N | Acc];
look_and_say([H|T], H, N, Acc) ->
    look_and_say(T, H, N+1, Acc);
look_and_say([H|T], Current, N, Acc) ->
    look_and_say(T, H, 1, [Current, $0+N | Acc]).
```


```txt

1> c(str).
{ok,str}
2> str:look_and_say("1").
"11"
3> str:look_and_say("111221").
"312211"
4> str:look_and_say("1",10).
["1","11","21","1211","111221","312211","13112221",
 "1113213211","31131211131221","13211311123113112211"]

```



## ERRE

<lang>
PROGRAM LOOK

PROCEDURE LOOK_AND_SAY(N$->N$)
      LOCAL I%,J%,C$,O$
      I%=1
      REPEAT
        C$=MID$(N$,I%,1)
        J%=I%+1
        WHILE MID$(N$,J%,1)=C$ DO
          J%+=1
        END WHILE
        O$+=MID$(STR$(J%-I%),2)+C$
        I%=J%
      UNTIL I%>LEN(N$)
      N$=O$
END PROCEDURE

BEGIN
      NUMBER$="1"
      FOR I%=1 TO 10 DO
        LOOK_AND_SAY(NUMBER$->NUMBER$)
        PRINT(NUMBER$)
      END FOR
END PROGRAM

```


```txt

11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221

```


=={{header|F Sharp|F#}}==
Library functions somehow missing in F# out of the box (but present in haskell)

```fsharp

let rec brk p lst =
  match lst with
  | [] -> (lst, lst)
  | x::xs ->
    if p x
    then ([], lst)
    else
      let (ys, zs) = brk p xs
      (x::ys, zs)

let span p lst = brk (not << p) lst

let rec groupBy eq lst =
  match lst with
  | [] ->  []
  | x::xs ->
    let (ys,zs) = span (eq x) xs
	(x::ys)::groupBy eq zs

let group lst : list<list<'a>> when 'a : equality = groupBy (=) lst

```


Implementation

```fsharp

let lookAndSay =
  let describe (xs: char list) =
    List.append (List.ofSeq <| (List.length xs).ToString()) [List.head xs]
  let next xs = List.collect describe (group xs)
  let toStr xs = String (Array.ofList xs)
  Seq.map toStr <| Seq.unfold (fun xs -> Some (xs, next xs)) ['1']

let getNthLookAndSay n = Seq.nth n lookAndSay

Seq.take 10 lookAndSay

```



## Factor


```factor
: (look-and-say) ( str -- )
    unclip-slice swap [ 1 ] 2dip [
        2dup = [ drop [ 1 + ] dip ] [
            [ [ number>string % ] dip , 1 ] dip
        ] if
    ] each [ number>string % ] [ , ] bi* ;

: look-and-say ( str -- str' ) [ (look-and-say) ] "" make ;

"1" 10 [ dup print look-and-say ] times print
```



## Forth


```forth
create buf1 256 allot
create buf2 256 allot
buf1 value src
buf2 value dest

s" 1" src place

: append-run ( digit run -- )
  dest count +
  tuck c!  1+ c!
  dest c@ 2 + dest c! ;

: next-look-and-say
  0 dest c!
  src 1+ c@  [char] 0  ( digit run )
  src count bounds do
    over i c@ =
    if   1+
    else append-run  i c@ [char] 1
    then
  loop
  append-run
  src dest to src to dest ;

: look-and-say ( n -- )
  0 do next-look-and-say  cr src count type loop ;

10 look-and-say
```



## Fortran


```fortran
module LookAndSay
  implicit none

contains

  subroutine look_and_say(in, out)
    character(len=*), intent(in) :: in
    character(len=*), intent(out) :: out

    integer :: i, c
    character(len=1) :: x
    character(len=2) :: d

    out = ""
    c = 1
    x = in(1:1)
    do i = 2, len(trim(in))
       if ( x == in(i:i) ) then
          c = c + 1
       else
          write(d, "(I2)") c
          out = trim(out) // trim(adjustl(d)) // trim(x)
          c = 1
          x = in(i:i)
       end if
    end do
    write(d, "(I2)") c
    out = trim(out) // trim(adjustl(d)) // trim(x)
  end subroutine look_and_say

end module LookAndSay
```



```fortran
program LookAndSayTest
  use LookAndSay
  implicit none

  integer :: i
  character(len=200) :: t, r
  t = "1"
  print *,trim(t)
  call look_and_say(t, r)
  print *, trim(r)
  do i = 1, 10
     call look_and_say(r, t)
     r = t
     print *, trim(r)
  end do

end program LookAndSayTest
```



## FreeBASIC

```freebasic

Dim As Integer n, j, k, k0, r
Dim As String X(2)
Dim As Integer i = 0  ' índice de cadena de entrada
X(0) = "1"

Input "Indica cuantas repeticiones: ", r
Print Chr(10) & "Secuencia:"

Print X(i)
For n = 1 To r-1
    j = 1 - i  ' índice de cadena de salida
    X(j) = ""
    k = 1
    While k <= Len(X(i))
        k0 = k + 1
        While ((k0 <= Len(X(i))) And (Mid(X(i), k, 1) = Mid(X(i), k0, 1)))
            k0 += 1
        Wend
        X(j) += Str(k0 - k) + Mid(X(i), k, 1)
        k = k0
    Wend
    i = j
    Print X(j)
Next n
End

```

```txt

Indica cuantas repeticiones: 10

Secuencia:
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211

```



## Gambas

'''Code is modified from the [[http://rosettacode.org/wiki/Look-and-say_sequence#PureBasic PureBasic]] example'''

'''[https://gambas-playground.proko.eu/?gist=83d63e1706fa1dc3c7468b1e9d7bcf05 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim i, j, cnt As Integer
Dim txt$, curr$, result$ As String

txt$ = "1211"
i = 1

Print "Sequence: " & txt$ & " = ";
  Repeat
    j = 1
    result$ = ""
      Repeat
        curr$ = Mid(txt$, j, 1)
        cnt = 0
          Repeat
            Inc cnt
            Inc j
          Until Mid(txt$, j, 1) <> curr$
        result$ &= Str(cnt) & curr$
      Until j > Len(txt$)
    Print result$
    txt$ = result$
    Dec i
  Until i <= 0
End
```

Output:

```txt

Sequence: 1211 = 111221

```



## GAP


```gap
LookAndSay := function(s)
  local c, r, cur, ncur, v;
  v := "123";
  r := "";
  cur := 0;
  ncur := 0;
  for c in s do
    if c = cur then
      ncur := ncur + 1;
    else
      if ncur > 0 then
        Add(r, v[ncur]);
        Add(r, cur);
      fi;
      cur := c;
      ncur := 1;
    fi;
  od;
  Add(r, v[ncur]);
  Add(r, cur);
  return r;
end;

LookAndSay("1");     # "11"
LookAndSay(last);    # "21"
LookAndSay(last);    # "1211"
LookAndSay(last);    # "111221"
LookAndSay(last);    # "312211"
LookAndSay(last);    # "13112221"
LookAndSay(last);    # "1113213211"
LookAndSay(last);    # "31131211131221"
LookAndSay(last);    # "13211311123113112211"
LookAndSay(last);    # "11131221133112132113212221"
LookAndSay(last);    # "3113112221232112111312211312113211"
LookAndSay(last);    # "1321132132111213122112311311222113111221131221"
LookAndSay(last);    # "11131221131211131231121113112221121321132132211331222113112211"
LookAndSay(last);    # "311311222113111231131112132112311321322112111312211312111322212311322113212221"
```



## Go


```go
package main

import (
    "fmt"
    "strconv"
)

func lss(s string) (r string) {
    c := s[0]
    nc := 1
    for i := 1; i < len(s); i++ {
        d := s[i]
        if d == c {
            nc++
            continue
        }
        r += strconv.Itoa(nc) + string(c)
        c = d
        nc = 1
    }
    return r + strconv.Itoa(nc) + string(c)
}

func main() {
    s := "1"
    fmt.Println(s)
    for i := 0; i < 8; i++ {
        s = lss(s)
        fmt.Println(s)
    }
}
```

```txt

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221

```



## Groovy


```groovy
def lookAndSay(sequence) {
    def encoded = new StringBuilder()
    (sequence.toString() =~ /(([0-9])\2*)/).each { matcher ->
        encoded.append(matcher[1].size()).append(matcher[2])
    }
    encoded.toString()
}
```

Test Code

```groovy
def sequence = "1"
(1..12).each {
    println sequence
    sequence = lookAndSay(sequence)
}
```

```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
```



## Haskell


```haskell
import Control.Monad (liftM2)
import Data.List (group)

-- this function is composed out of many functions; data flows from the bottom up
lookAndSay :: Integer -> Integer
lookAndSay = read                                   -- convert digits to integer
           . concatMap                              -- concatenate for each run,
               (liftM2 (++) (show . length)         --    the length of it
                            (take 1))               --    and an example member
           . group                                  -- collect runs of the same digit
           . show                                   -- convert integer to digits

-- less comments
lookAndSay2 :: Integer -> Integer
lookAndSay2 = read . concatMap (liftM2 (++) (show . length)
                                            (take 1))
            . group . show


-- same thing with more variable names
lookAndSay3 :: Integer -> Integer
lookAndSay3 n = read (concatMap describe (group (show n)))
  where describe run = show (length run) ++ take 1 run

main = mapM_ print (iterate lookAndSay 1)           -- display sequence until interrupted
```



## Haxe


```haxe
using Std;

class Main
{

	static function main()
	{
		var test = "1";
		for (i in 0...11) {
			Sys.println(test);
			test = lookAndSay(test);
		}
	}

	static function lookAndSay(s:String)
	{
		if (s == null || s == "") return "";

		var results = "";
		var repeat = s.charAt(0);
		var amount = 1;
		for (i in 1...s.length)
		{
			var actual = s.charAt(i);
			if (actual != repeat)
			{
				results += amount.string();
				results += repeat;
				repeat = actual;
				amount = 0;
			}
			amount++;
		}
		results += amount.string();
		results += repeat;

		return results;
	}
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
every 1 to 10 do
   write(n := nextlooknsayseq(\n | 1))
end

procedure nextlooknsayseq(n)  #: return next element in look and say sequence
n2 := ""
n ? until pos(0) do {
   i := tab(any(&digits)) | fail  # or fail if not digits
   move(-1)
   n2 ||:= *tab(many(i)) || i     # accumulate count+digit
   }
return n2
end
```


```txt
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
```



## J

'''Solution''':


```j
las=: ,@((# , {.);.1~ 1 , 2 ~:/\ ])&.(10x&#.inv)@]^:(1+i.@[)
```


'''Example''':

```j
   10 las 1
1 11 21 1211 111221 312211 13112221 1113213211 31131211131221 13211311123113112211 11131221133112132113212221
```


Note the result is an actual numeric sequence (cf. the textual solutions given in other languages).


## Java

```java5
public static String lookandsay(String number){
	StringBuilder result= new StringBuilder();

	char repeat= number.charAt(0);
	number= number.substring(1) + " ";
	int times= 1;

	for(char actual: number.toCharArray()){
		if(actual != repeat){
			result.append(times + "" + repeat);
			times= 1;
			repeat= actual;
		}else{
			times+= 1;
		}
	}
	return result.toString();
}
```

Testing:

```java5
public static void main(String[] args){
	String num = "1";

	for (int i=1;i<=10;i++) {
		System.out.println(num);
		num = lookandsay(num);
	}
}
```

```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
```



## JavaScript

```javascript
function lookandsay(str) {
    return str.replace(/(.)\1*/g, function(seq, p1){return seq.length.toString() + p1})
}

var num = "1";
for (var i = 10; i > 0; i--) {
    alert(num);
    num = lookandsay(num);
}
```


Without RegExp


```javascript
function lookSay(digits) {
    var result = '',
        chars = (digits + ' ').split(''),
        lastChar = chars[0],
        times = 0;

    chars.forEach(function(nextChar) {
        if (nextChar === lastChar) {
            times++;
        }
        else {
            result += (times + '') + lastChar;
            lastChar = nextChar;
            times = 1;
        }
    });

    return result;
}

(function output(seed, iterations) {
    for (var i = 0; i < iterations; i++) {
        console.log(seed);
        seed = lookSay(seed);
    }
})("1", 10);
```



## jq

```jq
def look_and_say:
  def head(c; n): if .[n:n+1] == c then head(c; n+1) else n end;
  tostring
  | if length == 0 then ""
    else head(.[0:1]; 1) as $len
      | .[0:$len] as $head
      | ($len | tostring) + $head[0:1] + (.[$len:] | look_and_say)
    end ;

# look and say n times
def look_and_say(n):
  if n == 0 then empty
  else look_and_say as $lns
       | $lns, ($lns|look_and_say(n-1))
  end ;
```

'''Example'''
 1 | look_and_say(10)
 11
 21
 1211
 111221
 312211
 13112221
 1113213211
 31131211131221
 13211311123113112211
 11131221133112132113212221


## Julia

```julia
function lookandsay(s::String)
    rst = IOBuffer()
    c = 1
    for i in 1:length(s)
        if i != length(s) && s[i] == s[i+1]
            c += 1
        else
            print(rst, c, s[i])
            c = 1
        end
    end
    String(take!(rst))
end


function lookandsayseq(n::Integer)
    rst = Vector{String}(undef, n)
    rst[1] = "1"
    for i in 2:n
        rst[i] = lookandsay(rst[i-1])
    end
    rst
end

println(lookandsayseq(10))
```


```txt
String["1", "11", "21", "1211", "111221", "312211", "13112221", "1113213211", "31131211131221", "13211311123113112211"]
```



## K


```k
  las: {x{0$,//$(#:'n),'*:'n:(&1,~=':x)_ x:0$'$x}\1}
  las 8
1 11 21 1211 111221 312211 13112221 1113213211 31131211131221
```



## Kotlin


```scala
// version 1.0.6

fun lookAndSay(s: String): String {
    val sb = StringBuilder()
    var digit = s[0]
    var count = 1
    for (i in 1 until s.length) {
        if (s[i] == digit)
            count++
        else {
            sb.append("$count$digit")
            digit = s[i]
            count = 1
        }
    }
    return sb.append("$count$digit").toString()
}

fun main(args: Array<String>) {
    var las = "1"
    for (i in 1..15) {
        println(las)
        las = lookAndSay(las)
    }
}
```


```txt

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221
11131221131211131231121113112221121321132132211331222113112211
311311222113111231131112132112311321322112111312211312111322212311322113212221

```



## Lasso

The Look-and-say sequence is a recursive RLE, so the solution can leverage the same method as used for RLE.

```Lasso
define rle(str::string)::string => {
	local(orig = #str->values->asCopy,newi=array, newc=array, compiled=string)
	while(#orig->size) => {
		if(not #newi->size) => {
			#newi->insert(1)
			#newc->insert(#orig->first)
			#orig->remove(1)
		else
			if(#orig->first == #newc->last) => {
				#newi->get(#newi->size) += 1
			else
				#newi->insert(1)
				#newc->insert(#orig->first)
			}
			#orig->remove(1)
		}
	}
	loop(#newi->size) => {
		#compiled->append(#newi->get(loop_count)+#newc->get(loop_count))
	}
	return #compiled
}
define las(n::integer,run::integer) => {
	local(str = #n->asString)
	loop(#run) => { #str = rle(#str) }
	return #str
}
loop(15) => {^ las(1,loop_count) + '\r' ^}
```

```txt
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221
11131221131211131231121113112221121321132132211331222113112211
311311222113111231131112132112311321322112111312211312111322212311322113212221
132113213221133112132113311211131221121321131211132221123113112221131112311332111213211322211312113211
```



## LiveCode

This function takes a string and returns the next Look-And-Say iteration of it:

```Lua
function lookAndSay S
   put 0 into C
   put char 1 of S into lastChar
   repeat with i = 2 to length(S)
      add 1 to C
      if char i of S is lastChar then next repeat
      put C & lastChar after R
      put 0 into C
      put char i of S into lastChar
   end repeat
   return R & C + 1 & lastChar
end lookAndSay

on demoLookAndSay
   put 1 into x
   repeat 10
      put x & cr after message
      put lookAndSay(x) into x
   end repeat
   put x after message
end demoLookAndSay
```


```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
```



## Logo


```logo
to look.and.say.loop :in :run :c :out
  if empty? :in [output (word :out :run :c)]
  if equal? first :in :c [output look.and.say.loop bf :in :run+1 :c :out]
  output look.and.say.loop bf :in 1 first :in (word :out :run :c)
end
to look.and.say :in
  if empty? :in [output :in]
  output look.and.say.loop bf :in 1 first :in "||
end

show cascade 10 [print ? look.and.say ?] 1
```



## Lua


```lua
--returns an iterator over the first n copies of the look-and-say sequence
function lookandsayseq(n)
  local t = {1}
  return function()
    local ret = {}
    for i, v in ipairs(t) do
      if t[i-1] and v == t[i-1] then
        ret[#ret - 1] = ret[#ret - 1] + 1
      else
        ret[#ret + 1] = 1
        ret[#ret + 1] = v
      end
    end
    t = ret
    n = n - 1
    if n > 0 then return table.concat(ret) end
  end
end
for i in lookandsayseq(10) do print(i) end
```


Alternative solution, using LPeg:

```lua
require "lpeg"
local P, C, Cf, Cc = lpeg.P, lpeg.C, lpeg.Cf, lpeg.Cc
lookandsay = Cf(Cc"" * C(P"1"^1 + P"2"^1 + P"3"^1)^1, function (a, b) return a .. #b .. string.sub(b,1,1) end)
t = "1"
for i = 1, 10 do
  print(t)
  t = lookandsay:match(t)
end
```



## M4

Using regular expressions:
```M4
divert(-1)
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')

define(`las',
   `patsubst(`$1',`\(\(.\)\2*\)',`len(\1)`'\2')')


define(`v',1)
divert
for(`x',1,10,
   `v
define(`v',las(v))')dnl
v
```



## Mathematica

Custom Functions:

```Mathematica
RunLengthEncode[x_List]:=(Through[{First,Length}[#]]&)/@Split[x]
 LookAndSay[n_,d_:1]:=NestList[Flatten[Reverse/@RunLengthEncode[#]]&,{d},n-1]
```


If second argument is omitted the sequence is started with 1. Second argument is supposed to be a digits from 0 to 9. If however a larger number is supplied it will be seen as 1 number, not multiple digits. However if one wants to start with a 2 or more digit number, one could reverse the sequence to go back to a single digit start. First example will create the first 13 numbers of the sequence starting with 1, the next example starts with 7:


```Mathematica
FromDigits /@ LookAndSay[13] // Column
FromDigits /@ LookAndSay[13, 7] // Column
```


gives back:

<pre style='height:15em;overflow:scroll'>1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221

7
17
1117
3117
132117
1113122117
311311222117
13211321322117
1113122113121113222117
31131122211311123113322117
132113213221133112132123222117
11131221131211132221232112111312111213322117
31131122211311123113321112131221123113111231121123222117
```



## Maxima


```maxima
collect(a) := block(
   [n: length(a), b: [ ], x: a[1], m: 1],
   for i from 2 thru n do
      (if a[i] = x then m: m + 1 else (b: endcons([x, m], b), x: a[i], m: 1)),
   b: endcons([x, m], b)
)$

look_and_say(s) := apply(sconcat, map(lambda([p], sconcat(string(p[2]), p[1])), collect(charlist(s))))$

block([s: "1"], for i from 1 thru 10 do (disp(s), s: look_and_say(s)));
/* "1"
   "11"
   "21"
   "1211"
   "111221"
   "312211"
   "13112221"
   "1113213211"
   "31131211131221"
   "13211311123113112211" */
```



## MAXScript


```maxscript
fn lookAndSay num =
(
    local result = ""
    num += " "
    local current = num[1]
    local numReps = 1

    for digit in 2 to num.count do
    (
        if num[digit] != current then
        (
            result += (numReps as string) + current
            numReps = 1
            current = num[digit]
        )
        else
        (
            numReps += 1
        )
    )
    result
)

local num = "1"

for i in 1 to 10 do
(
    print num
    num = lookAndSay num
)
```



## Metafont


```metafont
vardef lookandsay(expr s) =
string r; r := "";
if string s:
  i := 0;
  forever: exitif not (i < length(s));
    c := i+1;
    forever: exitif ( (substring(c,c+1) of s) <> (substring(i,i+1) of s) );
      c := c + 1;
    endfor
    r := r & decimal (c-i) & substring(i,i+1) of s;
    i := c;
  endfor
fi
r
enddef;

string p; p := "1";
for el := 1 upto 10:
  message p;
  p := lookandsay(p);
endfor

end
```



## MiniScript


```MiniScript
// Look and Say Sequence
repeats = function(digit, string)
	count = 0
	for c in string
		if c != digit then break
		count = count + 1
	end for
	return str(count)
end function

numbers = "1"
print numbers
for i in range(1,10) // warning, loop size > 15 gets long numbers very quickly
	number = ""
	position = 0
	while position < numbers.len
		repeatCount = repeats(numbers[position], numbers[position:])
		number = number + repeatCount + numbers[position]
		position = position + repeatCount.val
	end while
	print number
	numbers = number
end for
```

```txt

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221

```


== {{header|Nim}} ==

```nim
proc NextInLookAndSaySequence (current: string): string =
  assert(len(current) > 0)
  Result = ""
  var ch = current[0]
  var count = 1
  for i in countup(1, len(current)-1):
    if current[i] != ch:
      Result &= $count & ch
      ch = current[i]
      count = 1
    else:
      count += 1
  Result &= $count & ch

proc LookAndSay (n = 10) =
  var next = "1"
  for i in countup(1, n):
    next = NextInLookAndSaySequence(next)
    echo next

LookAndSay()

```


=={{header|Objective-C}}==


```objc>#import <Foundation/Foundation.h


-(NSString*)lookAndSay:(NSString *)word{
    if (!word) {
        return nil;
    }
    NSMutableString *result = [NSMutableString new];

    char repeat = [word characterAtIndex:0];
    int times = 1;
    word = [NSString stringWithFormat:@"%@ ",[word substringFromIndex:1] ];

    for (NSInteger index = 0; index < word.length; index++) {
        char actual = [word characterAtIndex:index];
        if (actual != repeat) {
            [result appendFormat:@"%d%c", times, repeat];
            times = 1;
            repeat = actual;
        } else {
            times ++;
        }
    }

    return [result copy];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    NSString *num = @"1";
    for (int i=1;i<=10;i++) {
        NSLog(@"%@", num);

        num = [self lookAndSay:num];
    }
}

```



## OCaml


###  Functional

This function computes a see-and-say sequence from the previous one:

```ocaml
let rec seeAndSay = function
  | [], nys -> List.rev nys
  | x::xs, [] -> seeAndSay(xs, [x; 1])
  | x::xs, y::n::nys when x=y -> seeAndSay(xs, y::1+n::nys)
  | x::xs, nys -> seeAndSay(xs, x::1::nys)
```

It can be used like this:

```ocaml>
 let gen n =
    let xs = Array.create n [1] in
    for i=1 to n-1 do
      xs.(i) <- seeAndSay(xs.(i-1), [])
    done;
    xs;;
val gen : int -> int list array = <fun>

> gen 10;;
- : int list array =
  [|[1]; [1; 1]; [2; 1]; [1; 2; 1; 1]; [1; 1; 1; 2; 2; 1]; [3; 1; 2; 2; 1; 1];
    [1; 3; 1; 1; 2; 2; 2; 1]; [1; 1; 1; 3; 2; 1; 3; 2; 1; 1];
    [3; 1; 1; 3; 1; 2; 1; 1; 1; 3; 1; 2; 2; 1];
    [1; 3; 2; 1; 1; 3; 1; 1; 1; 2; 3; 1; 1; 3; 1; 1; 2; 2; 1; 1]|]
```



###  With regular expressions in the Str library


```ocaml
#load "str.cma";;

let lookandsay =
  Str.global_substitute (Str.regexp "\\(.\\)\\1*")
                        (fun s -> string_of_int (String.length (Str.matched_string s)) ^
                                  Str.matched_group 1 s)

let () =
  let num = ref "1" in
  print_endline !num;
  for i = 1 to 10 do
    num := lookandsay !num;
    print_endline !num;
  done
```



###  With regular expressions in the Pcre library


```ocaml
open Pcre

let lookandsay str =
  let rex = regexp "(.)\\1*" in
  let subs = exec_all ~rex str in
  let ar = Array.map (fun sub -> get_substring sub 0) subs in
  let ar = Array.map (fun s -> String.length s, s.[0]) ar in
  let ar = Array.map (fun (n,c) -> (string_of_int n) ^ (String.make 1 c)) ar in
  let res = String.concat "" (Array.to_list ar) in
  (res)

let () =
  let num = ref(string_of_int 1) in
  for i = 1 to 10 do
    num := lookandsay !num;
    print_endline !num;
  done
```


run this example with 'ocaml -I +pcre pcre.cma script.ml'


###  Imperative


```ocaml
(* see http://oeis.org/A005150 *)

let look_and_say s =
let n = String.length s
and buf = Buffer.create 0
and prev = ref s.[0]
and count = ref 0 in
let append () = Buffer.add_char buf (char_of_int (48 + !count));
                Buffer.add_char buf !prev in
String.iter (fun c ->
   if c = !prev then incr count else
   begin
      append ();
      prev := c;
      count := 1
   end
) s;
append ();
Buffer.contents buf;;

(* what about length of successive strings ? *)
let iter f a n =
let rec aux r n v = if n = 0
                    then List.rev(r::v)
                    else aux (f r) (n - 1) (r::v) in
aux a n [];;

let las = iter look_and_say "1";;

(* the first sixty terms *)

List.map (String.length) (las 59);;
(*
   [1; 2; 2; 4; 6; 6; 8; 10; 14; 20; 26; 34; 46; 62; 78; 102; 134; 176; 226;
    302; 408; 528; 678; 904; 1182; 1540; 2012; 2606; 3410; 4462; 5808; 7586;
    9898; 12884; 16774; 21890; 28528; 37158; 48410; 63138; 82350; 107312;
    139984; 182376; 237746; 310036; 403966; 526646; 686646; 894810; 1166642;
    1520986; 1982710; 2584304; 3369156; 4391702; 5724486; 7462860; 9727930;
    12680852]
*)

(* see http://oeis.org/A005341 *)
```




## Oforth



```Oforth
import: mapping

: lookAndSay ( n -- )
   [ 1 ] #[ dup .cr group map( [#size, #first] ) expand ] times( n ) ;
```


{{out}} for n = 10 :

```txt

[1]
[1, 1]
[2, 1]
[1, 2, 1, 1]
[1, 1, 1, 2, 2, 1]
[3, 1, 2, 2, 1, 1]
[1, 3, 1, 1, 2, 2, 2, 1]
[1, 1, 1, 3, 2, 1, 3, 2, 1, 1]
[3, 1, 1, 3, 1, 2, 1, 1, 1, 3, 1, 2, 2, 1]
[1, 3, 2, 1, 1, 3, 1, 1, 1, 2, 3, 1, 1, 3, 1, 1, 2, 2, 1, 1]

```



## Oz


```oz
declare
  %% e.g. "21" -> "1211"
  fun {LookAndSayString S}
     for DigitGroup in {Group S} append:Add do
        {Add {Int.toString {Length DigitGroup}}}
        {Add [DigitGroup.1]}
     end
  end

  %% lazy sequence of integers starting with N
  fun {LookAndSay N}
     fun lazy {Loop S}
        {String.toInt S}|{Loop {LookAndSayString S}}
     end
  in
     {Loop {Int.toString N}}
  end

  %% like Haskell's "group"
  fun {Group Xs}
     case Xs of nil then nil
     [] X|Xr then
	Ys Zs
        {List.takeDropWhile Xr fun {$ W} W==X end ?Ys ?Zs}
     in
        (X|Ys) | {Group Zs}
     end
  end
in
  {ForAll {List.take {LookAndSay 1} 10} Show}
```



## PARI/GP


```parigp
step(n)={
  my(v=eval(Vec(Str(n))),cur=v[1],ct=1,out="");
  v=concat(v,99);
  for(i=2,#v,
    if(v[i]==cur,
      ct++
    ,
      out=Str(out,ct,cur);
      cur=v[i];
      ct=1
    )
  );
  eval(out)
};
n=1;for(i=1,20,print(n);n=step(n))
```



## Pascal

```pascal
program LookAndSayDemo(input, output);

uses
  SysUtils;

function LookAndSay (s: string): string;
  var
    item: char;
    index: integer;
    count: integer;
  begin
    LookAndSay := '';
    item := s[1];
    count := 1;
    for index:= 2 to length(s) do
      if item = s[index] then
        inc(count)
      else
      begin
	LookAndSay := LookAndSay + intTostr(count) + item;
        item := s[index];
	count := 1;
      end;
      LookAndSay := LookAndSay + intTostr(count) + item;
  end;

var
  number: string;

begin
  writeln('Press RETURN to continue and ^C to stop.');
  number := '1';
  while not eof(input) do
  begin
   write(number);
   readln;
   number := LookAndSay(number);
  end;
end.
```

```txt
% ./LookAndSay
Press RETURN to continue and ^C to stop.

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221
11131221131211131231121113112221121321132132211331222113112211^C

```


Even faster imperative Version
Improvement:
setlength of result,no inttoStr and using pChar
But the Code Alignment is very important.
```pascal

program LookAndSayDemo(input, output);
{$IFDEF FPC}
  {$Mode Delphi}  // using result
  {$optimization ON}
// i3-4330 3.5 Ghz
//  {$CodeAlign proc=16,loop=8} //2,6 secs
  {$CodeAlign proc=16,loop=1}  //1,6 secs so much faster ???
{$ENDIF}

uses
  SysUtils;
const
  cntChar : array[1..9] of char =
           ('1','2','3','4','5','6','7','8','9');

function LookAndSay2 (const s: string): string;
//using pChar for result
var
  source,
  destin : pChar;
  len,
  idxFrom,
  idxTo :  integer;
  cnt: integer;

  item: char;
begin
  idxFrom := length(s);
  source := @s[1];

  //adjust length of result
  len := round(length(s)* 1.306+10);
  setlength(result,len);
  destin := @result[1];
  dec(destin);

  idxto := 1;
  item := source^;
  inc(source);
  cnt := 1;
  for idxFrom := idxFrom downto 2 do
  begin
    if item <> source^ then
    begin
      destin[idxTo]  := cntChar[cnt];
      destin[idxTo+1]:= item;
      item := source^;
      cnt := 1;
      inc(idxto,2);
    end
    else
      inc(cnt);
    inc(source);
  end;
  destin[idxTo] := cntChar[cnt];
  destin[idxTo+1]:= item;
  setlength(result,idxto+1);
end;

var
  number: string;
  l1,l2,
  i : integer;
begin
  number := '1';
  writeln(number);
  writeln(1:4,length(number):16,1/1:10:6);

  For i := 2 to 70 do
  begin
    l1 := length(number);
    number := LookAndSay2(number);
    l2 := length(number);
    IF i <10 then
      writeln(number);
    writeln(i:4,length(number):16,l2/l1:10:6);
  end;
end.
```

```txt
1
   1               1  1.000000
11
   2               2  2.000000
21
   3               2  1.000000
1211
   4               4  2.000000
111221
   5               6  1.500000
312211
   6               6  1.000000
13112221
   7               8  1.333333
1113213211
   8              10  1.250000
31131211131221
   9              14  1.400000
  10              20  1.428571
  11              26  1.300000
  12              34  1.307692
  13              46  1.352941
  14              62  1.347826
  15              78  1.258065
  16             102  1.307692
........
  67        81117366  1.303580
  68       105745224  1.303608
  69       137842560  1.303535
  70       179691598  1.303600

real	0m1.639s
user	0m1.593s
sys	0m0.043s
```



## Perl


```perl
sub lookandsay {
  my $str = shift;
  $str =~ s/((.)\2*)/length($1) . $2/ge;
  return $str;
}

my $num = "1";
foreach (1..10) {
  print "$num\n";
  $num = lookandsay($num);
}
```


Using string as a cyclic buffer:

```perl
for (local $_ = "1\n"; s/((.)\2*)//s;) {
	print $1;
	$_ .= ($1 ne "\n" and length($1)).$2
}
```



## Perl 6

In Perl 6 it is natural to avoid explicit loops; rather we use the sequence operator to define a lazy infinite sequence.  We'll print the first 15 values here.


```perl6
.say for ('1', *.subst(/(.)$0*/, { .chars ~ .[0] }, :g) ... *)[^15];
```


```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221
11131221131211131231121113112221121321132132211331222113112211
311311222113111231131112132112311321322112111312211312111322212311322113212221
```



## Phix


```Phix
function lookandsay(string s)
string res = ""
integer p = s[1], c = 1
    for i=2 to length(s) do
        if p=s[i] then
            c += 1
        else
            res &= sprintf("%d%s",{c,p})
            p = s[i]
            c = 1
        end if
    end for
    res &= sprintf("%d%s",{c,p})
    return res
end function

string s = "1"
?s
for i=1 to 10 do
    s = lookandsay(s)
    ?s
end for
```

```txt

"1"
"11"
"21"
"1211"
"111221"
"312211"
"13112221"
"1113213211"
"31131211131221"
"13211311123113112211"
"11131221133112132113212221"

```



## PHP


```php
<?php

function lookAndSay($str) {

	return preg_replace_callback('#(.)\1*#', function($matches) {

		return strlen($matches[0]).$matches[1];
	}, $str);
}

$num = "1";

foreach(range(1,10) as $i) {

	echo $num."<br/>";
	$num = lookAndSay($num);
}

?>
```



## PicoLisp


```PicoLisp
(de las (Lst)
   (make
      (while Lst
         (let (N 1  C)
            (while (= (setq C (pop 'Lst)) (car Lst))
               (inc 'N) )
            (link N C) ) ) ) )
```

Usage:

```PicoLisp
: (las (1))
-> (1 1)
: (las @)
-> (2 1)
: (las @)
-> (1 2 1 1)
: (las @)
-> (1 1 1 2 2 1)
: (las @)
-> (3 1 2 2 1 1)
: (las @)
-> (1 3 1 1 2 2 2 1)
: (las @)
-> (1 1 1 3 2 1 3 2 1 1)
: (las @)
-> (3 1 1 3 1 2 1 1 1 3 1 2 2 1)
```



## PowerBASIC

This uses the <code>RLEncode</code> function from the [[Run-length encoding#PowerBASIC|PowerBASIC Run-length encoding entry]].

```powerbasic
FUNCTION RLEncode (i AS STRING) AS STRING
    DIM tmp1 AS STRING, tmp2 AS STRING, outP AS STRING
    DIM Loop0 AS LONG, count AS LONG

    FOR Loop0 = 1 TO LEN(i)
        tmp1 = MID$(i, Loop0, 1)
        IF tmp1 <> tmp2 THEN
            IF count > 1 THEN
                outP = outP & TRIM$(STR$(count)) & tmp2
                tmp2 = tmp1
                count = 1
            ELSEIF 0 = count THEN
                tmp2 = tmp1
                count = 1
            ELSE
                outP = outP & "1" & tmp2
                tmp2 = tmp1
            END IF
        ELSE
            INCR count
        END IF
    NEXT

    outP = outP & TRIM$(STR$(count)) & tmp2
    FUNCTION = outP
END FUNCTION

FUNCTION lookAndSay(BYVAL count AS LONG) AS STRING
    DIM iii AS STRING, tmp AS STRING

    IF count > 1 THEN
        iii = lookAndSay(count - 1)
    ELSEIF count < 2 THEN
        iii = "1"
    END IF

    tmp = RLEncode(iii)
    lookAndSay = tmp
END FUNCTION

FUNCTION PBMAIN () AS LONG
    DIM v AS LONG
    v = VAL(INPUTBOX$("Enter a number."))
    MSGBOX lookAndSay(v)
END FUNCTION
```



## PowerShell


```powershell
function Get-LookAndSay ($n = 1) {
    $re = [regex] '(.)\1*'
    $ret = ""
    foreach ($m in $re.Matches($n)) {
        $ret += [string] $m.Length + $m.Value[0]
    }
    return $ret
}

function Get-MultipleLookAndSay ($n) {
    if ($n -eq 0) {
        return @()
    } else {
        $a = 1
        $a
        for ($i = 1; $i -lt $n; $i++) {
            $a = Get-LookAndSay $a
            $a
        }
    }
}
```

```txt
PS> Get-MultipleLookAndSay 8
1
11
21
1211
111221
312211
13112221
1113213211
```



## Prolog

Works with SWI-Prolog.


```Prolog
look_and_say(L) :-
	maplist(write, L), nl,
	encode(L, L1),
	look_and_say(L1).

% This code is almost identical to the code of "run-length-encoding"
encode(In, Out) :-
	packList(In, R1),
	append(R1,Out).


% use of library clpfd allows packList(?In, ?Out) to works
% in both ways In --> Out and In <-- Out.

:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ?- packList([a,a,a,b,c,c,c,d,d,e], L).
%  L = [[3,a],[1,b],[3,c],[2,d],[1,e]] .
% ?- packList(R,  [[3,a],[1,b],[3,c],[2,d],[1,e]]).
% R = [a,a,a,b,c,c,c,d,d,e] .
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
packList([],[]).

packList([X],[[1,X]]) :- !.


packList([X|Rest],[XRun|Packed]):-
    run(X,Rest, XRun,RRest),
    packList(RRest,Packed).


run(Var,[],[1,Var],[]).

run(Var,[Var|LRest],[N1, Var],RRest):-
    N #> 0,
    N1 #= N + 1,
    run(Var,LRest,[N, Var],RRest).


run(Var,[Other|RRest], [1,Var],[Other|RRest]):-
    dif(Var,Other).

```


```txt
 ?- look_and_say([1]).
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
..........................
```



## Pure


```pure
using system;

// Remove the trailing "L" from the string representation of bigints.
__show__ x::bigint = init (str x);

say x = val $ strcat $ map (sprintf "%d%s") $ look $ chars $ str x with
  look [] = [];
  look xs@(x:_) = (#takewhile (==x) xs,x) : look (dropwhile (==x) xs);
end;

iteraten 5 say 1; // [1,11,21,1211,111221]

// This prints the entire sequence, press Ctrl-C to abort.
do (puts.str) (iterate say 1);
```



## PureBasic


```PureBasic
If OpenConsole()
  Define i, j, cnt, txt$, curr$, result$
  Print("Enter start sequence: "): txt$=Input()
  Print("How many repetitions: "): i=Val(Input())
  ;
  PrintN(#CRLF$+"Sequence:"+#CRLF$+txt$)
  Repeat
    j=1
    result$=""
    Repeat
      curr$=Mid(txt$,j,1)
      cnt=0
      Repeat
        cnt+1
        j+1
      Until Mid(txt$,j,1)<>curr$
      result$+Str(cnt)+curr$
    Until j>Len(txt$)
    PrintN(result$)
    txt$=result$
    i-1
  Until i<=0
  ;
  PrintN(#CRLF$+"Press ENTER to exit."): Input()
  CloseConsole()
EndIf
```


```txt

 Enter start sequence: 1
 How many repetitions: 7

 Sequence:
 1
 11
 21
 1211
 111221
 312211
 13112221
 1113213211
```



## Python

```python
def lookandsay(number):
    result = ""

    repeat = number[0]
    number = number[1:]+" "
    times = 1

    for actual in number:
        if actual != repeat:
            result += str(times)+repeat
            times = 1
            repeat = actual
        else:
            times += 1

    return result

num = "1"

for i in range(10):
    print num
    num = lookandsay(num)
```


Functional
```python>>>
 from itertools import groupby
>>> def lookandsay(number):
	return ''.join( str(len(list(g))) + k
		        for k,g in groupby(number) )

>>> numberstring='1'
>>> for i in range(10):
	print numberstring
	numberstring = lookandsay(numberstring)
```


```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
```


'''As a generator'''


```python>>>
 from itertools import groupby, islice
>>>
>>> def lookandsay(number='1'):
	while True:
		yield number
		number = ''.join( str(len(list(g))) + k
		                  for k,g in groupby(number) )


>>> print('\n'.join(islice(lookandsay(), 10)))
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
```


'''Using regular expressions'''

```python
import re

def lookandsay(str):
    return re.sub(r'(.)\1*', lambda m: str(len(m.group(0))) + m.group(1), str)

num = "1"
for i in range(10):
    print num
    num = lookandsay(num)
```



## Q


```q
las:{{raze string[count@'x],'@'[;0]x:where[differ x]_x}\[x;1#"1"]}
las 8
```

```txt

,"1"
"11"
"21"
"1211"
"111221"
"312211"
"13112221"
"1113213211"
"31131211131221"

```



## R

Returning the value as an integer limits how long the sequence can get,
so the option for integer or character return values are provided.

```R
look.and.say <- function(x, return.an.int=FALSE)
{
   #convert number to character vector
   xstr <- unlist(strsplit(as.character(x), ""))
   #get run length encoding
   rlex <- rle(xstr)
   #form new string
   odds <- as.character(rlex$lengths)
   evens <- rlex$values
   newstr <- as.vector(rbind(odds, evens))
   #collapse to scalar
   newstr <- paste(newstr, collapse="")
   #convert to number, if desired
   if(return.an.int) as.integer(newstr) else newstr
}
```

Example usage:

```R
x <- 1
for(i in 1:10)
{
   x <- look.and.say(x)
   print(x)
}
```



## Racket



```Racket

#lang racket

(define (encode str)
  (regexp-replace* #px"(.)\\1*" str (lambda (m c) (~a (string-length m) c))))

(define (look-and-say-sequence n)
  (reverse (for/fold ([r '("1")]) ([n n]) (cons (encode (car r)) r))))

(for-each displayln (look-and-say-sequence 10))

```


```txt

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221

```



## REXX

Programming note:   this version works with any string   (a '''null''' is assumed, which causes   '''1'''   to be used).


If a negative number is specified (the number of iterations
to be used for the calculations), only the length of

the number (or character string) is shown.


### simple version


```rexx
/*REXX program displays the sequence (and/or lengths) for the    look and say    series.*/
parse arg N ! .                                  /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=20                     /*Not specified?  Then use the deault. */
if !=='' | !==","  then !=1                      /* "      "         "   "   "     "    */

     do j=1  for abs(N)                          /*repeat a number of times to show NUMS*/
     if j\==1  then != $lookAndSay(!)            /*invoke function to calculate next #. */
     if N<0    then say 'length['j"]:" length(!) /*Also,  display the sequence's length.*/
               else say '['j"]:"      !          /*display the number to the terminal.  */
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$lookAndSay: procedure; parse arg x,,$           /*obtain the (passed) argument  {X}.   */
fin = '0'x                                       /*use unique character to end scanning.*/
x=x || fin                                       /*append the  FIN  character to string.*/
             do k=1  by 0                        /*now,  process the given sequence.    */
                y=  substr(x, k, 1)              /*pick off one character to examine.   */
             if y== fin  then return $           /*if we're at the end, then we're done.*/
             _= verify(x, y, , k)  - k           /*see how many characters we have of Y.*/
             $= $ || _ || y                      /*build the  "look and say"  sequence. */
             k= k  + _                           /*now, point to the next character.    */
             end   /*k*/
```

```txt

[1]: 1
[2]: 11
[3]: 21
[4]: 1211
[5]: 111221
[6]: 312211
[7]: 13112221
[8]: 1113213211
[9]: 31131211131221
[10]: 13211311123113112211
[11]: 11131221133112132113212221
[12]: 3113112221232112111312211312113211
[13]: 1321132132111213122112311311222113111221131221
[14]: 11131221131211131231121113112221121321132132211331222113112211
[15]: 311311222113111231131112132112311321322112111312211312111322212311322113212221
[16]: 132113213221133112132113311211131221121321131211132221123113112221131112311332111213211322211312113211
[17]: 11131221131211132221232112111312212321123113112221121113122113111231133221121321132132211331121321231231121113122113322113111221131221
[18]: 31131122211311123113321112131221123113112211121312211213211321322112311311222113311213212322211211131221131211132221232112111312111213111213211231131122212322211331222113112211
[19]: 1321132132211331121321231231121113112221121321132122311211131122211211131221131211132221121321132132212321121113121112133221123113112221131112311332111213122112311311123112111331121113122112132113213211121332212311322113212221
[20]: 11131221131211132221232112111312111213111213211231132132211211131221131211221321123113213221123113112221131112311332211211131221131211132211121312211231131112311211232221121321132132211331121321231231121113112221121321133112132112312321123113112221121113122113121113123112112322111213211322211312113211

```

```txt

[1]: ggg
[2]: 3g
[3]: 131g
[4]: 1113111g
[5]: 3113311g
[6]: 132123211g
[7]: 11131211121312211g
[8]: 31131112311211131122211g
[9]: 132113311213211231132132211g
[10]: 11131221232112111312211213211312111322211g
[11]: 3113112211121312211231131122211211131221131112311332211g
[12]: 1321132122311211131122211213211321322112311311222113311213212322211g
[13]: 1113122113121122132112311321322112111312211312111322211213211321322123211211131211121332211g
[14]: 31131122211311122122111312211213211312111322211231131122211311123113322112111312211312111322111213122112311311123112112322211g
[15]: 132113213221133122112231131122211211131221131112311332211213211321322113311213212322211231131122211311123113223112111311222112132113311213211221121332211g
[16]: 11131221131211132221231122212213211321322112311311222113311213212322211211131221131211132221232112111312111213322112132113213221133112132113221321123113213221121113122123211211131221222112112322211g
[17]: 31131122211311123113321112132132112211131221131211132221121321132132212321121113121112133221123113112221131112311332111213122112311311123112112322211211131221131211132221232112111312211322111312211213211312111322211231131122111213122112311311221132211221121332211g

```

<pre style="height:60ex">
length[1]: 1
length[2]: 2
length[3]: 2
length[4]: 4
length[5]: 6
length[6]: 6
length[7]: 8
length[8]: 10
length[9]: 14
length[10]: 20
length[11]: 26
length[12]: 34
length[13]: 46
length[14]: 62
length[15]: 78
length[16]: 102
length[17]: 134
length[18]: 176
length[19]: 226
length[20]: 302
length[21]: 408
length[22]: 528
length[23]: 678
length[24]: 904
length[25]: 1182
length[26]: 1540
length[27]: 2012
length[28]: 2606
length[29]: 3410
length[30]: 4462
length[31]: 5808
length[32]: 7586
length[33]: 9898
length[34]: 12884
length[35]: 16774
length[36]: 21890
length[37]: 28528
length[38]: 37158
length[39]: 48410
length[40]: 63138
length[41]: 82350
length[42]: 107312
length[43]: 139984
length[44]: 182376
length[45]: 237746
length[46]: 310036
length[47]: 403966
length[48]: 526646
length[49]: 686646
length[50]: 894810
length[51]: 1166642
length[52]: 1520986
length[53]: 1982710
length[54]: 2584304
length[55]: 3369156
length[56]: 4391702
length[57]: 5724486
length[58]: 7462860
length[59]: 9727930
length[60]: 12680852

```



### faster version

This version appends the generated parts of the sequence, and after it gets to a certain size (chunkSize),

it appends the sequence generated (so far) to the primary sequence, and starts with a null sequence.

This avoids appending a small character string to a growing larger and larger character string.

```rexx
/*REXX program displays the sequence (and/or lengths) for the    look and say    series.*/
parse arg N ! .                                  /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=20                     /*Not specified?  Then use the default.*/
if !=='' | !==","  then !=1                      /* "      "         "   "   "     "    */
                                                 /* [↑]  !:   starting char for the seq.*/
     do j=1  for abs(N)                          /*repeat a number of times to show NUMS*/
     if j\==1  then != $lookAndSay(!)            /*invoke function to calculate next #. */
     if N<0    then say 'length['j"]:" length(!) /*Also,  display the sequence's length.*/
               else say '['j"]:"      !          /*display the number to the terminal.  */
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$lookAndSay: procedure; parse arg x,,$ !         /*obtain the (passed) argument  {X}.   */
             chSize= 1000                        /*define a sensible chunk size.        */
             fin = '0'x                          /*use unique character to end scanning.*/
             x=x || fin                          /*append the  FIN  character to string.*/
               do k=1  by 0                      /*now,  process the given sequence.    */
                  y=  substr(x, k, 1)            /*pick off one character to examine.   */
               if y== fin  then return $         /*if we're at the end, then we're done.*/
               _= verify(x, y, , k)  - k         /*see how many characters we have of Y.*/
               $= $ || _ || y                    /*build the  "look and say"  sequence. */
               k= k  + _                         /*now, point to the next character.    */
               if length($)<chSize  then iterate /*Less than chunkSize?  Then keep going*/
               != ! || $                         /*append   $   to the  !  string.      */
               $=                                /*now,  start   $   from scratch.      */
               chSize= chSize + 100              /*bump the  chunkSize (length) counter.*/
               end   /*k*/
            return ! || $                        /*return the ! string plus the $ string*/
```

## Ring


```ring

number = "1"
for nr = 1 to 10
    number = lookSay(number)
    see number + nl
next

func lookSay n
     i = 0 j = 0 c="" o=""
     i = 1
     while i <= len(n)
           c = substr(n,i,1)
           j = i + 1
           while substr(n,j,1) = c
                 j += 1
           end
           o += string(j-i) + c
           i = j
      end
      return o

```


## Ruby

The simplest one:

```ruby

class String
  def look_and_say
    gsub(/(.)\1*/){|s| s.size.to_s + s[0]}
  end
end

ss = '1'
12.times {puts ss; ss = ss.look_and_say}

```

```txt

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211

```


```ruby
def lookandsay(str)
  str.gsub(/(.)\1*/) {$&.length.to_s + $1}
end

num = "1"
10.times do
  puts num
  num = lookandsay(num)
end
```

```txt

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211

```


Using Enumerable#chunk

```ruby
def lookandsay(str)
  str.chars.chunk{|c| c}.map{|c,x| [x.size, c]}.join
end

puts num = "1"
9.times do
  puts num = lookandsay(num)
end
```

The '''output''' is the same above.

Without regular expression:


```ruby
# Adding clusterization (http://apidock.com/rails/Enumerable/group_by)
module Enumerable
  # clumps adjacent elements together
  # >> [2,2,2,3,3,4,2,2,1].cluster
  # => [[2, 2, 2], [3, 3], [4], [2, 2], [1]]
  def cluster
    cluster = []
    each do |element|
      if cluster.last && cluster.last.last == element
        cluster.last << element
      else
        cluster << [element]
      end
    end
    cluster
  end
end
```


Using Array#cluster defined above:


```ruby
def print_sequence(input_sequence, seq=10)
  return unless seq > 0
  puts input_sequence.join
  result_array = input_sequence.cluster.map do |cluster|
    [cluster.count, cluster.first]
  end
  print_sequence(result_array.flatten, seq-1)
end

print_sequence([1])
```

The '''output''' is the same above.


## Rust


```rust
fn next_sequence(in_seq: &[i8]) -> Vec<i8> {
    assert!(!in_seq.is_empty());

    let mut result = Vec::new();
    let mut current_number = in_seq[0];
    let mut current_runlength = 1;

    for i in &in_seq[1..] {
        if current_number == *i {
            current_runlength += 1;
        } else {
            result.push(current_runlength);
            result.push(current_number);
            current_runlength = 1;
            current_number = *i;
        }
    }
    result.push(current_runlength);
    result.push(current_number);
    result
}

fn main() {
    let mut seq = vec![1];

    for i in 0..10 {
        println!("Sequence {}: {:?}", i, seq);
        seq = next_sequence(&seq);
    }
}
```

```txt
Sequence 0: [1]
Sequence 1: [1, 1]
Sequence 2: [2, 1]
Sequence 3: [1, 2, 1, 1]
Sequence 4: [1, 1, 1, 2, 2, 1]
Sequence 5: [3, 1, 2, 2, 1, 1]
Sequence 6: [1, 3, 1, 1, 2, 2, 2, 1]
Sequence 7: [1, 1, 1, 3, 2, 1, 3, 2, 1, 1]
Sequence 8: [3, 1, 1, 3, 1, 2, 1, 1, 1, 3, 1, 2, 2, 1]
Sequence 9: [1, 3, 2, 1, 1, 3, 1, 1, 1, 2, 3, 1, 1, 3, 1, 1, 2, 2, 1, 1]
```



## Scala



### Recursive


```Scala
import scala.annotation.tailrec

object LookAndSay extends App {

  loop(10, "1")

  @tailrec
  private def loop(n: Int, num: String): Unit = {
    println(num)
    if (n <= 0) () else loop(n - 1, lookandsay(num))
  }

  private def lookandsay(number: String): String = {
    val result = new StringBuilder

    @tailrec
    def loop(numberString: String, repeat: Char, times: Int): String =
      if (numberString.isEmpty) result.toString()
      else if (numberString.head != repeat) {
        result.append(times).append(repeat)
        loop(numberString.tail, numberString.head, 1)
      } else loop(numberString.tail, numberString.head, times + 1)

    loop(number.tail + " ", number.head, 1)
  }

}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/V5Jn5mf/0 (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/7kn0fV3gTaqCDLIv4QGuMQ Scastie (JVM)].

### using Iterator

```scala
def lookAndSay(seed: BigInt) = {
  val s = seed.toString
  ( 1 until s.size).foldLeft((1, s(0), new StringBuilder)) {
    case ((len, c, sb), index) if c != s(index) => sb.append(len); sb.append(c); (1, s(index), sb)
    case ((len, c, sb), _) => (len + 1, c, sb)
  } match {
    case (len, c, sb) => sb.append(len); sb.append(c); BigInt(sb.toString)
  }
}

def lookAndSayIterator(seed: BigInt) = Iterator.iterate(seed)(lookAndSay)
```



### using Stream


```Scala
object Main extends App {

  def lookAndSay(previous: List[BigInt]): Stream[List[BigInt]] = {

    def next(num: List[BigInt]): List[BigInt] = num match {
      case Nil => Nil
      case head :: Nil => 1 :: head :: Nil
      case head :: tail =>
        val size = (num takeWhile (_ == head)).size
        List(BigInt(size), head) ::: next(num.drop(size))
    }
    val x = next(previous)
    x #:: lookAndSay(x)
  }

  (lookAndSay(1 :: Nil) take 10).foreach(s => println(s.mkString("")))
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: lookAndSay (in integer: level, in string: stri) is func
  result
    var string: lookAndSay is "";
  local
    var integer: index is 2;
  begin
    if level = 1 then
      if stri <> "" then
        while index <= length(stri) and stri[index] = stri[1] do
          incr(index);
        end while;
        lookAndSay := str(pred(index)) & stri[1 len 1] & lookAndSay(level, stri[index ..]);
      end if;
    else
      lookAndSay := lookAndSay(1, lookAndSay(pred(level), stri));
    end if;
  end func;

const proc: main is func
  local
    var integer: level is 0;
  begin
    for level range 1 to 14 do
      writeln(lookAndSay(level, "1"));
    end for;
  end func;
```


```txt

11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221
11131221131211131231121113112221121321132132211331222113112211
311311222113111231131112132112311321322112111312211312111322212311322113212221

```



## Sidef

```ruby
func lookandsay(str) {
    str.gsub(/((.)\2*)/, {|a,b| a.len.to_s + b });
}

var num = "1";
{
  say num;
  num = lookandsay(num);
} * 10;
```


```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
```



## Smalltalk

```smalltalk
String extend [
  lookAndSay [ |anElement nextElement counter coll newColl|
     coll := (self asOrderedCollection).
     newColl := OrderedCollection new.
     counter := 0.
     anElement := (coll first).
     [ coll size > 0 ]
     whileTrue: [
        nextElement := coll removeFirst.
	( anElement == nextElement ) ifTrue: [
           counter := counter + 1.
        ] ifFalse: [
	  newColl add: (counter displayString).
	  newColl add: (anElement asString).
	  anElement := nextElement.
	  counter := 1.
        ]
     ].
     newColl add: (counter displayString).
     newColl add: (anElement asString).
     ^(newColl join)
  ]
].

|r|
r := '1'.
10 timesRepeat: [
  r displayNl.
  r := r lookAndSay.
]
```



## SNOBOL4

The look-and-say sequence is an iterative run-length string encoding.
So looksay( ) is just a wrapper around the Run-length Encoding task.
This is by far the easiest solution.


```SNOBOL4
*       # Encode RLE
        define('rle(str)c,n') :(rle_end)
rle     str len(1) . c :f(return)
        str span(c) @n =
        rle = rle n c :(rle)
rle_end

*       # First m members of sequence with seed n
        define('looksay(n,m)') :(looksay_end)
looksay output = n; m = gt(m,1) m - 1 :f(return)
        n = rle(n) :(looksay)
looksay_end

*       Test and display
        looksay(1,10)
end
```


```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
```



## SQL


```sql
DROP VIEW delta;
CREATE VIEW delta AS
    SELECT sequence1.v AS x,
           (sequence1.v<>sequence2.v)*sequence1.c AS v,
           sequence1.c AS c
      FROM sequence AS sequence1,
           sequence AS sequence2
     WHERE sequence1.c = sequence2.c+1;

DROP VIEW rle0;
CREATE VIEW rle0 AS
    SELECT delta2.x AS x,
           SUM(delta2.v) AS v,
           delta2.c AS c
      FROM delta AS delta1,
           delta as delta2
     WHERE delta1.c >= delta2.c
  GROUP BY delta1.c;

DROP VIEW rle1;
CREATE VIEW rle1 AS
    SELECT sum(x)/x AS a,
           x AS b,
           c AS c
      FROM rle0
  GROUP BY v;

DROP VIEW rle2;
CREATE VIEW rle2 AS
    SELECT a as v, 1 as o, 2*c+0 as c FROM rle1 UNION
    SELECT b as v, 1 as o, 2*c+1 as c FROM rle1;

DROP VIEW normed;
CREATE VIEW normed AS
    SELECT r1.v as v, SUM(r2.o) as c
      FROM rle2 AS r1,
           rle2 AS r2
     WHERE r1.c >= r2.c
  GROUP BY r1.c;

DROP TABLE rle;
CREATE TABLE rle(v int, c int);
INSERT INTO rle SELECT * FROM normed ORDER BY c;

DELETE FROM sequence;
INSERT INTO sequence VALUES(-1,0);
INSERT INTO sequence SELECT * FROM rle;
```


Usage:
```txt
% sqlite3
SQLite version 3.4.0
Enter ".help" for instructions
sqlite> CREATE TABLE sequence(v int, c int);
sqlite> INSERT INTO sequence VALUES(-1,0);
sqlite> INSERT INTO sequence VALUES(1,1);
sqlite> SELECT * FROM sequence;
-1|0
1|1
sqlite> .read look.sql
sqlite> SELECT * FROM sequence;
-1|0
1|1
1|2
sqlite> .read look.sql
sqlite> SELECT * FROM sequence;
-1|0
2|1
1|2
sqlite> .read look.sql
sqlite> SELECT * FROM sequence;
-1|0
1|1
2|2
1|3
1|4
sqlite> .read look.sql
sqlite> SELECT * FROM sequence;
-1|0
1|1
1|2
1|3
2|4
2|5
1|6
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

SET SERVEROUTPUT ON @

BEGIN
 DECLARE NMBR VARCHAR(100) DEFAULT '1';
 DECLARE J SMALLINT DEFAULT 1;

 CALL DBMS_OUTPUT.PUT_LINE(NMBR);
 WHILE (J < 10) DO
  BEGIN
  DECLARE I SMALLINT;
  DECLARE SIZE SMALLINT;
  DECLARE ACTUAL CHAR(1);
  DECLARE REPEAT CHAR(1);
  DECLARE RESULT VARCHAR(100);
  DECLARE TIMES SMALLINT;

  SET REPEAT = SUBSTR(NMBR, 1, 1);
  SET NMBR = SUBSTR(NMBR, 2) || ' ';
  SET TIMES = 1;
  SET I = 1;
  SET SIZE = LENGTH(NMBR);

  WHILE (I <= SIZE) DO
   SET ACTUAL = SUBSTR(NMBR, I, 1);
   IF (ACTUAL <> REPEAT) THEN
    SET RESULT = COALESCE(RESULT, '') || TIMES || '' || REPEAT;
    SET TIMES = 1;
    SET REPEAT = ACTUAL;
   ELSE
    SET TIMES = TIMES + 1;
   END IF;
   SET I = I + 1;
  END WHILE;

  CALL DBMS_OUTPUT.PUT_LINE(RESULT);
  SET NMBR = RESULT;
  END ;
  SET J = J + 1;
 END WHILE;
END @

```

Output:

```txt

db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211

```



## Swift

```swift
func lookAndSay(_ seq: [Int]) -> [Int] {
  var result = [Int]()
  var cur = seq[0]
  var curRunLength = 1

  for i in seq.dropFirst() {
    if cur == i {
      curRunLength += 1
    } else {
      result.append(curRunLength)
      result.append(cur)
      curRunLength = 1
      cur = i
    }
  }

  result.append(curRunLength)
  result.append(cur)

  return result
}

var seq = [1]

for i in 0..<10 {
  print("Seq \(i): \(seq)")
  seq = lookAndSay(seq)
}
```


```txt
Seq 0: [1]
Seq 1: [1, 1]
Seq 2: [2, 1]
Seq 3: [1, 2, 1, 1]
Seq 4: [1, 1, 1, 2, 2, 1]
Seq 5: [3, 1, 2, 2, 1, 1]
Seq 6: [1, 3, 1, 1, 2, 2, 2, 1]
Seq 7: [1, 1, 1, 3, 2, 1, 3, 2, 1, 1]
Seq 8: [3, 1, 1, 3, 1, 2, 1, 1, 1, 3, 1, 2, 2, 1]
Seq 9: [1, 3, 2, 1, 1, 3, 1, 1, 1, 2, 3, 1, 1, 3, 1, 1, 2, 2, 1, 1]
```



## Tcl


```tcl
proc lookandsay n {
    set new ""
    while {[string length $n] > 0} {
        set char [string index $n 0]
        for {set count 1} {[string index $n $count] eq $char} {incr count} {}
        append new $count $char
        set n [string range $n $count end]
    }
    interp alias {} next_lookandsay {} lookandsay $new
    return $new
}

puts 1                 ;# ==> 1
puts [lookandsay 1]    ;# ==> 11
puts [next_lookandsay] ;# ==> 21
puts [next_lookandsay] ;# ==> 1211
puts [next_lookandsay] ;# ==> 111221
puts [next_lookandsay] ;# ==> 312211
```


Alternatively, with coroutines:
```tcl
proc seq_lookandsay {n {coroName next_lookandsay}} {
    coroutine $coroName apply {n {
        for {} {[yield $n] ne "stop"} {set n $new} {
            set new ""
            foreach subseq [regexp -all -inline {0+|1+|2+|3+|4+|5+|6+|7+|8+|9+} $n] {
                append new [string length $subseq] [string index $subseq 0]
            }
        }
    }} $n
}

puts [seq_lookandsay 1]
puts [next_lookandsay]
puts [next_lookandsay]
puts [next_lookandsay]
puts [next_lookandsay]
puts [next_lookandsay]
puts [next_lookandsay]
puts [next_lookandsay]
puts [next_lookandsay]
puts [next_lookandsay]
```


```txt

1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
num=1,say=""
 LOOP look
  digits=STRINGS (num," ? ")
  digitgrouped=ACCUMULATE (digits,howmany)
   LOOP/CLEAR  h=howmany,digit=digitgrouped
    say=JOIN (say,"",h,digit)
   ENDLOOP
  PRINT say
  num=VALUE(say),say=""
  IF (look==14) EXIT
 ENDLOOP

```

<pre style='height:30ex;overflow:scroll'>
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221
11131221131211131231121113112221121321132132211331222113112211
311311222113111231131112132112311321322112111312211312111322212311322113212221

```



## UNIX Shell

```bash
lookandsay() {
    local num=$1 char seq i
    for ((i=0; i<=${#num}; i++)); do
        char=${num:i:1}
        if [[ $char == ${seq:0:1} ]]; then
            seq+=$char
        else
            [[ -n $seq ]] && printf "%d%s" ${#seq} ${seq:0:1}
            seq=$char
        fi
    done
}

for ((num=1, i=1; i<=10; i++)); do
    echo $num
    num=$( lookandsay $num )
done
```


```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
```



## Ursala

The look_and_say function returns the first n results
by iterating the function that maps a given sequence to its successor.

```Ursala
#import std
#import nat

look_and_say "n" = ~&H\'1' next"n" rlc~&E; *= ^lhPrT\~&hNC %nP+ length

#show+

main = look_and_say 10
```

```txt
1
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
```



## VBA


```VBA

Public Sub LookAndSay(Optional Niter As Integer = 10)
'generate "Niter" members of the look-and-say sequence
'(argument is optional; default is 10)

Dim s As String            'look-and-say number
Dim news As String         'next number in sequence
Dim curdigit As String     'current digit in s
Dim newdigit As String     'next digit in s
Dim curlength As Integer   'length of current run
Dim p As Integer           'position in s
Dim L As Integer           'length of s

On Error GoTo Oops          'to catch overflow, i.e. number too long

'start with "1"
s = "1"
For i = 1 To Niter
  'initialise
  L = Len(s)
  p = 1
  curdigit = Left$(s, 1)
  curlength = 1
  news = ""
  For p = 2 To L
    'check next digit in s
    newdigit = Mid$(s, p, 1)
    If curdigit = newdigit Then 'extend current run
      curlength = curlength + 1
    Else ' "output" run and start new run
      news = news & CStr(curlength) & curdigit
      curdigit = newdigit
      curlength = 1
    End If
  Next p
  ' "output" last run
  news = news & CStr(curlength) & curdigit
  Debug.Print news
  s = news
Next i
Exit Sub

Oops:
  Debug.Print
  If Err.Number = 6 Then 'overflow
    Debug.Print "Oops - number too long!"
  Else
    Debug.Print "Error: "; Err.Number, Err.Description
  End If
End Sub

```


```txt

LookAndSay 7
11
21
1211
111221
312211
13112221
1113213211

```


(Note: overflow occurs at 38th iteration!)


## Vedit macro language

This implementation generates look-and-say sequence
starting from the sequence on cursor line in edit buffer.
Each new sequence is inserted as a new line.
10 sequences are created in this example.


```vedit
Repeat(10) {
  BOL
  Reg_Empty(20)
  While (!At_EOL) {
    Match("(.)\1*", REGEXP+ADVANCE)
    Num_Str(Chars_Matched, 20, LEFT+APPEND)
    Reg_Copy_Block(20, CP-1, CP, APPEND)
  }
  Ins_Newline Reg_Ins(20)
}
```


 1
 11
 21
 1211
 111221
 312211
 13112221
 1113213211
 31131211131221
 13211311123113112211
 11131221133112132113212221


## VBScript


### ==Implementation==


```vb
function looksay( n )
	dim i
	dim accum
	dim res
	dim c
	res = vbnullstring
	do
		if n = vbnullstring then exit do
		accum = 0
		c = left( n,1 )
		do while left( n, 1 ) = c
			accum = accum + 1
			n = mid(n,2)
		loop
		if accum > 0 then
			res = res & accum & c
		end if
	loop
	looksay = res
end function
```



### ==Invocation==


```vb
dim m
m = 1
for i = 0 to 13
	m = looksay(m)
	wscript.echo m
next
```


```txt

11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221
3113112221232112111312211312113211
1321132132111213122112311311222113111221131221
11131221131211131231121113112221121321132132211331222113112211
311311222113111231131112132112311321322112111312211312111322212311322113212221

```



## Yabasic


```Yabasic

dim X$(2)
i = 0  // índice de cadena de entrada
X$(i) = "1"

input "Indica cuantas repeticiones: " r
print "\nSecuencia:"

print X$(i)
for n = 1 to r-1
    j = 1 - i  // índice de cadena de salida
    X$(j) = ""
    k = 1
    while k <= len(X$(i))
        k0 = k + 1
        while ((k0 <= len(X$(i))) and (mid$(X$(i), k, 1) = mid$(X$(i), k0, 1)))
            k0 = k0 + 1
        wend
        X$(j) = X$(j) + str$(k0 - k) + mid$(X$(i), k, 1)
        k = k0
    wend
    i = j
    print X$(j)
next n
print

```

{{out}}La salida es similar a la de [[#FreeBASIC|FreeBASIC]], mostrada arriba.


## Yorick



```yorick
func looksay(input) {
    // Special case: one digit
    if(strlen(input) == 1)
        return "1" + input;
    // Convert string into an array of digits
    digits = strchar(input)(:-1);
    // Find indices where each run starts
    w = where(digits(dif));
    start = numberof(w) ? grow(1, w+1) : [1];
    // Find length of each run
    len = grow(start, numberof(digits)+1)(dif);
    // Find digits for each run
    run = digits(start);
    // Construct output array
    result = array(string, numberof(start)*2);
    // Fill in lengths
    result(1::2) = swrite(format="%d", len);
    // Fill in digits; first must add trailing nulls to coerce single string
    // into an array of strings.
    run = transpose([run, array(char(0), numberof(run))])(*);
    result(2::2) = strchar(run);
    // Merge string array into single string
    return result(sum);
}

val = "1";
do {
   write, val;
   val = looksay(val);
} while(strlen(val) < 80);
```


```txt
 1
 11
 21
 1211
 111221
 312211
 13112221
 1113213211
 31131211131221
 13211311123113112211
 11131221133112132113212221
 3113112221232112111312211312113211
 1321132132111213122112311311222113111221131221
 11131221131211131231121113112221121321132132211331222113112211
 311311222113111231131112132112311321322112111312211312111322212311322113212221
```



## zkl

Treating the task as a string manipulation problem.
```zkl
fcn lookAndSay(seed){ // numeric String --> numeric String
   len,c:=[1..seed.len()-1].reduce(fcn([(len,c)]lc,index,s,sb){
      if(c!=s[index]) { sb.write(len); sb.write(c); lc.clear(1,s[index]) }
      else lc.clear(len+1,c);
   },L(1,seed[0]), seed,sb:=Sink(String));
   sb.write(len); sb.write(c);
   sb.close();
}
```


```txt

(0).reduce(10,fcn(seed,_){ lookAndSay(seed).println() },"1");
11
21
1211
111221
312211
13112221
1113213211
31131211131221
13211311123113112211
11131221133112132113212221

```


+++
title = "Rep-string"
description = ""
date = 2019-10-09T18:40:12Z
aliases = []
[extra]
id = 13460
[taxonomies]
categories = []
tags = []
+++

{{task}}
Given a series of ones and zeroes in a string, define a repeated string or ''rep-string'' as a string which is created by repeating a substring of the ''first'' N characters of the string ''truncated on the right to the length of the input string, and in which the substring appears repeated at least twice in the original''.

For example, the string '''10011001100''' is a rep-string as the leftmost four characters of '''1001''' are repeated three times and truncated on the right to give the original string.

Note that the requirement for having the repeat occur two or more times means that the repeating unit is ''never'' longer than half the length of the input string.


;Task:
* Write a function/subroutine/method/... that takes a string and returns an indication of if it is a rep-string and the repeated string.   (Either the string that is repeated, or the number of repeated characters would suffice).
* There may be multiple sub-strings that make a string a rep-string - in that case an indication of all, or the longest, or the shortest would suffice.
* Use the function to indicate the repeating substring if any, in the following:
<dl><dd>

```txt

1001110011
1110111011
0010010010
1010101010
1111111111
0100101101
0100100
101
11
00
1

```

</dl>
* Show your output on this page.





## Ada



```Ada
with Ada.Command_Line, Ada.Text_IO, Ada.Strings.Fixed;

procedure Rep_String is

   function Find_Largest_Rep_String(S:String) return String is
      L: Natural := S'Length;
   begin
      for I in reverse 1 .. L/2 loop
	 declare
	    use Ada.Strings.Fixed;
	    T: String := S(S'First .. S'First + I-1); -- the first I characters of S
	    U: String := (1+(L/I)) * T; -- repeat T so often that U'Length >= L
	 begin -- compare first L characers of U with S
	    if U(U'First .. U'First + S'Length -1) = S then
	       return T; -- T is a rep-string
	    end if;
	 end;
      end loop;
      return ""; -- no rep string;
   end Find_Largest_Rep_String;

   X: String := Ada.Command_Line.Argument(1);
   Y: String := Find_Largest_Rep_String(X);

begin
   if Y="" then
      Ada.Text_IO.Put_Line("No rep-string for """ & X & """");
   else
      Ada.Text_IO.Put_Line("Longest rep-string for """& X &""": """& Y &"""");
   end if;
end Rep_String;
```


{{out}}


```txt
> ./rep_string 1001110011
Longest rep-string for "1001110011": "10011"
> ./rep_string 1110111011
Longest rep-string for "1110111011": "1110"
> ./rep_string 0010010010
Longest rep-string for "0010010010": "001"
> ./rep_string 1010101010
Longest rep-string for "1010101010": "1010"
> ./rep_string 1111111111
Longest rep-string for "1111111111": "11111"
> ./rep_string 0100101101
No rep-string for "0100101101"
> ./rep_string 0100100
Longest rep-string for "0100100": "010"
> ./rep_string 101
No rep-string for "101"
> ./rep_string 11
Longest rep-string for "11": "1"
> ./rep_string 00
Longest rep-string for "00": "0"
> ./rep_string 1
No rep-string for "1"
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# procedure to find the longest rep-string in a given string               #
# the input string is not validated to contain only "0" and "1" characters #
PROC longest rep string = ( STRING input )STRING:
BEGIN

    STRING result           := "";

    # ensure the string we are working on has a lower-bound of 1           #
    STRING str               = input[ AT 1 ];

    # work backwards from half the input string looking for a rep-string   #
    FOR string length FROM UPB str OVER 2 BY -1 TO 1
    WHILE
        STRING left substring  = str[ 1 : string length ];
        # if the left substgring repeated a sufficient number of times     #
        # (truncated on the right) is equal to the original string, then   #
        # we have found the longest rep-string                             #
        STRING repeated string = ( left substring
                                 * ( ( UPB str OVER string length ) + 1 )
                                 )[ 1 : UPB str ];
        IF str = repeated string
        THEN
            # found a rep-string                                           #
            result := left substring;
            FALSE
        ELSE
            # not a rep-string, keep looking                               #
            TRUE
        FI
    DO
        SKIP
    OD;

    result
END; # longest rep string #


# test the longest rep string procedure                                    #
main:
(

    []STRING tests = ( "1001110011"
                     , "1110111011"
                     , "0010010010"
                     , "1010101010"
                     , "1111111111"
                     , "0100101101"
                     , "0100100"
                     , "101"
                     , "11"
                     , "00"
                     , "1"
                     );

    FOR test number FROM LWB tests TO UPB tests
    DO
        STRING rep string = longest rep string( tests[ test number ] );
        print( ( tests[ test number ]
               , ": "
               , IF rep string = ""
                 THEN "no rep string"
                 ELSE "longest rep string: """ + rep string + """"
                 FI
               , newline
               )
             )
    OD
)
```

{{out}}

```txt
1001110011: longest rep string: "10011"
1110111011: longest rep string: "1110"
0010010010: longest rep string: "001"
1010101010: longest rep string: "1010"
1111111111: longest rep string: "11111"
0100101101: no rep string
0100100: longest rep string: "010"
101: no rep string
11: longest rep string: "1"
00: longest rep string: "0"
1: no rep string
```



## AppleScript


```AppleScript
-- REP-CYCLES ----------------------------------------------------------------

-- repCycles :: String -> [String]
on repCycles(xs)
    set n to length of xs

    script isCycle
        on |λ|(cs)
            xs = takeCycle(n, cs)
        end |λ|
    end script

    filter(isCycle, tail(inits(take(quot(n, 2), xs))))
end repCycles

-- cycleReport :: String -> [String]
on cycleReport(xs)
    set reps to repCycles(xs)

    if isNull(reps) then
        {xs, "(n/a)"}
    else
        {xs, |last|(reps)}
    end if
end cycleReport


-- TEST ----------------------------------------------------------------------
on run
    set samples to {"1001110011", "1110111011", "0010010010", ¬
        "1010101010", "1111111111", "0100101101", "0100100", ¬
        "101", "11", "00", "1"}

    unlines(cons("Longest cycle:" & linefeed, ¬
        map(curry(intercalate)'s |λ|(" -> "), ¬
            map(cycleReport, samples))))

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    if length of xs > 0 and class of (item 1 of xs) is string then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to length of xs
        set acc to acc & item i of xs
    end repeat
    acc
end concat

-- cons :: a -> [a] -> [a]
on cons(x, xs)
    {x} & xs
end cons

-- curry :: (Script|Handler) -> Script
on curry(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    |λ|(a, b) of mReturn(f)
                end |λ|
            end script
        end |λ|
    end script
end curry

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

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

-- inits :: [a] -> [[a]]
-- inits :: String -> [String]
on inits(xs)
    script elemInit
        on |λ|(_, i, xs)
            items 1 thru i of xs
        end |λ|
    end script

    script charInit
        on |λ|(_, i, xs)
            text 1 thru i of xs
        end |λ|
    end script

    if class of xs is string then
        {""} & map(charInit, xs)
    else
        {{}} & map(elemInit, xs)
    end if
end inits

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- last :: [a] -> a
on |last|(xs)
    if length of xs > 0 then
        item -1 of xs
    else
        missing value
    end if
end |last|

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

-- isNull :: [a] -> Bool
on isNull(xs)
    xs = {}
end isNull

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- quot :: Integral a => a -> a -> a
on quot(n, m)
    n div m
end quot

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

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail

-- take :: Int -> [a] -> [a]
on take(n, xs)
    if class of xs is string then
        if n > 0 then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else
        if n > 0 then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    end if
end take

-- takeCycle :: Int -> [a] -> [a]
on takeCycle(n, xs)
    set lng to length of xs
    if lng ≥ n then
        set cycle to xs
    else
        set cycle to concat(replicate((n div lng) + 1, xs))
    end if

    if class of xs is string then
        items 1 thru n of cycle as string
    else
        items 1 thru n of cycle
    end if
end takeCycle

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines
```

{{Out}}

```txt
Longest cycle:

1001110011 -> 10011
1110111011 -> 1110
0010010010 -> 001
1010101010 -> 1010
1111111111 -> 11111
0100101101 -> (n/a)
0100100 -> 010
101 -> (n/a)
11 -> 1
00 -> 0
1 -> (n/a)
```



## AutoHotkey

{{works with|AutoHotkey 1.1}}

```AutoHotkey
In := ["1001110011", "1110111011", "0010010010", "1010101010"
     , "1111111111", "0100101101", "0100100", "101", "11", "00", "1"]
for k, v in In
	Out .= RepString(v) "`t" v "`n"
MsgBox, % Out

RepString(s) {
	Loop, % StrLen(s) // 2 {
		i := A_Index
		Loop, Parse, s
		{
			pos := Mod(A_Index, i)
			if (A_LoopField != SubStr(s, !pos ? i : pos, 1))
				continue, 2
		}
		return SubStr(s, 1, i)
	}
	return "N/A"
}
```

{{Out}}

```txt
10011	1001110011
1110	1110111011
001	0010010010
10	1010101010
1	1111111111
N/A	0100101101
010	0100100
N/A	101
1	11
0	00
N/A	1
```



## BaCon


```freebasic
all$ = "1001110011 1110111011 0010010010 1010101010 1111111111 0100101101 0100100 101 11 00 1"

FOR word$ IN all$
    FOR x = LEN(word$)/2 DOWNTO 1

        ex$ = EXPLODE$(word$, x)

        FOR st$ IN UNIQ$(ex$)
            IF NOT(REGEX(HEAD$(ex$, 1), "^" & st$)) THEN CONTINUE 2
        NEXT

        PRINT "Repeating string: ", word$, " -> ", HEAD$(ex$, 1)
        CONTINUE 2
    NEXT

    PRINT "Not a repeating string: ", word$
NEXT
```

{{out}}

```txt

Repeating string: 1001110011 -> 10011
Repeating string: 1110111011 -> 1110
Repeating string: 0010010010 -> 001
Repeating string: 1010101010 -> 1010
Repeating string: 1111111111 -> 11111
Not a repeating string: 0100101101
Repeating string: 0100100 -> 010
Not a repeating string: 101
Repeating string: 11 -> 1
Repeating string: 00 -> 0
Not a repeating string: 1

```



## Bracmat


```bracmat
( ( rep-string
  =   reps L x y
    .   ( reps
        =   x y z
          .   !arg:(?x.?y)
            & ( @(!y:!x ?z)&reps$(!x.!z)
              | @(!x:!y ?)
              )
        )
      & (   :?L
          & @( !arg
             :   %?x
                 !x
                 ( ?y
                 & reps$(!x.!y)
                 & !x !L:?L
                 & ~
                 )
             )
        |   !L:
          & out$(str$(!arg " is not a rep-string"))
        | out$(!arg ":" !L)
        )
  )
& rep-string$1001110011
& rep-string$1110111011
& rep-string$0010010010
& rep-string$1010101010
& rep-string$1111111111
& rep-string$0100101101
& rep-string$0100100
& rep-string$101
& rep-string$11
& rep-string$00
& rep-string$1
);
```

{{Out}}

```txt
1001110011 : 10011
1110111011 : 1110
0010010010 : 001
1010101010 : 1010 10
1111111111 : 11111 1111 111 11 1
0100101101 is not a rep-string
0100100 : 010
101 is not a rep-string
11 : 1
00 : 0
1 is not a rep-string
```



## C


### Longest substring



```c

#include <stdio.h>
#include <string.h>

int repstr(char *str)
{
    if (!str) return 0;

    size_t sl = strlen(str) / 2;
    while (sl > 0) {
        if (strstr(str, str + sl) == str)
            return sl;
        --sl;
    }

    return 0;
}

int main(void)
{
    char *strs[] = { "1001110011", "1110111011", "0010010010", "1111111111",
        "0100101101", "0100100", "101", "11", "00", "1" };

    size_t strslen = sizeof(strs) / sizeof(strs[0]);
    size_t i;
    for (i = 0; i < strslen; ++i) {
        int n = repstr(strs[i]);
        if (n)
            printf("\"%s\" = rep-string \"%.*s\"\n", strs[i], n, strs[i]);
        else
            printf("\"%s\" = not a rep-string\n", strs[i]);
    }

    return 0;
}

```

{{out}}

```txt

"1001110011" = rep-string "10011"
"1110111011" = rep-string "1110"
"0010010010" = rep-string "001"
"1111111111" = rep-string "11111"
"0100101101" = not a rep-string
"0100100" = rep-string "010"
"101" = not a rep-string
"11" = rep-string "1"
"00" = rep-string "0"
"1" = not a rep-string

```


### shortest substring


```c

// strstr : Returns a pointer to the first occurrence of str2 in str1, or a null pointer if str2 is not part of str1.
// size_t is an unsigned integer typ
// lokks for the shortest substring
int repstr(char *str)
{
    if (!str) return 0; // if empty input

    size_t sl = 1;
    size_t sl_max = strlen(str) ;

    while (sl < sl_max) {
        if (strstr(str, str + sl) == str) // How it works ???? It checks the whole string str
        	return sl;
        ++sl;
    }

    return 0;
}

```



## C++


```cpp
#include <string>
#include <vector>
#include <boost/regex.hpp>

bool is_repstring( const std::string & teststring , std::string & repunit ) {
   std::string regex( "^(.+)\\1+(.*)$" ) ;
   boost::regex e ( regex ) ;
   boost::smatch what ;
   if ( boost::regex_match( teststring , what , e , boost::match_extra ) ) {
      std::string firstbracket( what[1 ] ) ;
      std::string secondbracket( what[ 2 ] ) ;
      if ( firstbracket.length( ) >= secondbracket.length( ) &&
	    firstbracket.find( secondbracket ) != std::string::npos ) {
	 repunit = firstbracket  ;
      }
   }
   return !repunit.empty( ) ;
}

int main( ) {
   std::vector<std::string> teststrings { "1001110011" , "1110111011" , "0010010010" ,
      "1010101010" , "1111111111" , "0100101101" , "0100100" , "101" , "11" , "00" , "1" } ;
   std::string theRep ;
   for ( std::string myString : teststrings ) {
      if ( is_repstring( myString , theRep ) ) {
	 std::cout << myString << " is a rep string! Here is a repeating string:\n" ;
	 std::cout << theRep << " " ;
      }
      else {
	 std::cout << myString << " is no rep string!" ;
      }
      theRep.clear( ) ;
      std::cout << std::endl ;
   }
   return 0 ;
}
```

{{out}}

```txt
1001110011 is a rep string! Here is a repeating string:
10011
1110111011 is a rep string! Here is a repeating string:
1110
0010010010 is a rep string! Here is a repeating string:
001
1010101010 is a rep string! Here is a repeating string:
1010
1111111111 is a rep string! Here is a repeating string:
11111
0100101101 is no rep string!
0100100 is a rep string! Here is a repeating string:
010
101 is no rep string!
11 is a rep string! Here is a repeating string:
1
00 is a rep string! Here is a repeating string:
0
1 is no rep string!
```



## Common Lisp


```lisp

(ql:quickload :alexandria)
(defun rep-stringv (a-str &optional (max-rotation (floor (/ (length a-str) 2))))
  ;; Exit condition if no repetition found.
  (cond ((< max-rotation 1) "Not a repeating string")
        ;; Two checks:
        ;; 1. Truncated string must be equal to rotation by repetion size.
        ;; 2. Remaining chars (rest-str) are identical to starting chars (beg-str)
        ((let* ((trunc (* max-rotation (truncate (length a-str) max-rotation)))
                (truncated-str (subseq a-str 0 trunc))
                (rest-str (subseq a-str trunc))
                (beg-str (subseq a-str 0 (rem (length a-str) max-rotation))))
           (and (string= beg-str rest-str)
                (string= (alexandria:rotate (copy-seq truncated-str) max-rotation)
                         truncated-str)))
         ;; If both checks pass, return the repeting string.
         (subseq a-str 0 max-rotation))
        ;; Recurse function reducing length of rotation.
        (t (rep-stringv a-str (1- max-rotation)))))

```

{{out}}

```lisp

(setf test-strings '("1001110011"
                     "1110111011"
                     "0010010010"
                     "1010101010"
                     "1111111111"
                     "0100101101"
                     "0100100"
                     "101"
                     "11"
                     "00"
                     "1"
                     ))

(loop for item in test-strings
      collecting (cons item (rep-stringv item)))

```


```txt

(("1001110011" . "10011") ("1110111011" . "1110") ("0010010010" . "001")
 ("1010101010" . "1010") ("1111111111" . "11111")
 ("0100101101" . "Not a repeating string") ("0100100" . "010")
 ("101" . "Not a repeating string") ("11" . "1") ("00" . "0")
 ("1" . "Not a repeating string"))

```



## D

Two different algorithms. The second is from the Perl 6 entry.

```d
import std.stdio, std.string, std.conv, std.range, std.algorithm,
       std.ascii, std.typecons;

Nullable!(size_t, 0) repString1(in string s) pure nothrow @safe @nogc
in {
    //assert(s.all!isASCII);
    assert(s.representation.all!isASCII);
} body {
    immutable sr = s.representation;
    foreach_reverse (immutable n; 1 .. sr.length / 2 + 1)
        if (sr.take(n).cycle.take(sr.length).equal(sr))
            return typeof(return)(n);
    return typeof(return)();
}

Nullable!(size_t, 0) repString2(in string s) pure @safe /*@nogc*/
in {
    assert(s.countchars("01") == s.length);
} body {
    immutable bits = s.to!ulong(2);

    foreach_reverse (immutable left; 1 .. s.length / 2 + 1) {
        immutable right = s.length - left;
        if ((bits ^ (bits >> left)) == ((bits >> right) << right))
            return typeof(return)(left);
    }
    return typeof(return)();
}

void main() {
    immutable words = "1001110011 1110111011 0010010010 1010101010
                       1111111111 0100101101 0100100 101 11 00 1".split;

    foreach (immutable w; words) {
        immutable r1 = w.repString1;
        //assert(r1 == w.repString2);
        immutable r2 = w.repString2;
        assert((r1.isNull && r2.isNull) || r1 == r2);
        if (r1.isNull)
            writeln(w, " (no repeat)");
        else
            writefln("%(%s %)", w.chunks(r1));
    }
}
```

{{out}}

```txt
10011 10011
1110 1110 11
001 001 001 0
1010 1010 10
11111 11111
0100101101 (no repeat)
010 010 0
101 (no repeat)
1 1
0 0
1 (no repeat)
```



## Dyalect


{{trans|Go}}


```dyalect
func rep(s) {
    var x = s.len() / 2
    while x > 0 {
        if s.startsWith(s.sub(x)) {
            return x
        }
        x -= 1
    }
    return 0
}

const m = [
    "1001110011",
    "1110111011",
    "0010010010",
    "1010101010",
    "1111111111",
    "0100101101",
    "0100100",
    "101",
    "11",
    "00",
    "1"
]

for s in m {
    if (rep(s) is n) && n > 0 {
        print("\(s)  \(n) rep-string \(s.sub(n))")
    } else {
        print("\(s)  not a rep-string")
    }
}
```


{{out}}


```txt
1001110011  5 rep-string 10011
1110111011  4 rep-string 111011
0010010010  3 rep-string 0010010
1010101010  4 rep-string 101010
1111111111  5 rep-string 11111
0100101101  not a rep-string
0100100  3 rep-string 0100
101  not a rep-string
11  1 rep-string 1
00  1 rep-string 0
1  not a rep-string
```



## EchoLisp


```scheme

(lib 'list) ;; list-rotate

;; a list is a rep-list if equal? to itself after a rotation of lam units
;; lam <= list length / 2
;; truncate to a multiple of lam before rotating
;; try cycles in decreasing lam order (longest wins)

(define (cyclic? cyclic)
    (define len (length cyclic))
    (define trunc null)

    (if (> len 1)
        (for ((lam (in-range (quotient len 2) 0 -1)))
        (set! trunc (take cyclic (- len (modulo len lam))))
         #:break (equal? trunc  (list-rotate trunc lam)) => (list->string (take cyclic lam))
         'no-rep )
   'too-short-no-rep))


```

{{out}}

```scheme

(define strings '["1001110011" "1110111011" "0010010010" "1010101010"
      "1111111111" "0100101101" "0100100" "101" "11" "00" "1"])

(define (task strings)
	(for-each (lambda (s)
	(writeln s (cyclic? (string->list s)))) strings))

(task strings)

"1001110011"     "10011"
"1110111011"     "1110"
"0010010010"     "001"
"1010101010"     "1010"
"1111111111"     "11111"
"0100101101"     no-rep
"0100100"     "010"
"101"     no-rep
"11"     "1"
"00"     "0"
"1"     too-short-no-rep

```



## Elixir


```elixir
defmodule Rep_string do
  def find(""), do: IO.puts "String was empty (no repetition)"
  def find(str) do
    IO.puts str
    rep_pos = Enum.find(div(String.length(str),2)..1, fn pos ->
      String.starts_with?(str, String.slice(str, pos..-1))
    end)
    if rep_pos && rep_pos>0 do
      IO.puts String.duplicate(" ", rep_pos) <> String.slice(str, 0, rep_pos)
    else
      IO.puts "(no repetition)"
    end
    IO.puts ""
  end
end

strs = ~w(1001110011
          1110111011
          0010010010
          1010101010
          1111111111
          0100101101
          0100100
          101
          11
          00
          1)

Enum.each(strs, fn str -> Rep_string.find(str) end)
```


{{out}}

```txt

1001110011
     10011

1110111011
    1110

0010010010
   001

1010101010
    1010

1111111111
     11111

0100101101
(no repetition)

0100100
   010

101
(no repetition)

11
 1

00
 0

1
(no repetition)

```


=={{header|F_Sharp|F#}}==

```fsharp
let isPrefix p (s : string) = s.StartsWith(p)
let getPrefix n (s : string) = s.Substring(0,n)

let repPrefixOf str =
    let rec isRepeatedPrefix p s =
        if isPrefix p s then isRepeatedPrefix p (s.Substring (p.Length))
        else isPrefix s p

    let rec getLongestRepeatedPrefix n =
        if n = 0 then None
        elif isRepeatedPrefix (getPrefix n str) str then Some(getPrefix n str)
        else getLongestRepeatedPrefix (n-1)

    getLongestRepeatedPrefix (str.Length/2)

[<EntryPoint>]
let main argv =
    printfn "Testing for rep-string (and showing the longest repeated prefix in case):"
    [
    "1001110011"
    "1110111011"
    "0010010010"
    "1010101010"
    "1111111111"
    "0100101101"
    "0100100"
    "101"
    "11"
    "00"
    "1"
    ] |>
    List.map (fun s ->
        match repPrefixOf s with | None -> s + ": NO" | Some(p) -> s + ": YES ("+ p + ")")
    |> List.iter (printfn "%s")
    0
```

{{out}}

```txt
Testing for rep-string (and showing the longest repeated prefix in case):
1001110011: YES (10011)
1110111011: YES (1110)
0010010010: YES (001)
1010101010: YES (1010)
1111111111: YES (11111)
0100101101: NO
0100100: YES (010)
101: NO
11: YES (1)
00: YES (0)
1: NO
```



## Factor


```factor
USING: formatting grouping kernel math math.ranges qw sequences ;
IN: rosetta-code.rep-string

: (find-rep-string) ( str -- str )
    dup dup length 2/ [1,b]
    [ <groups> [ head? ] monotonic? ] with find nip dup
    [ head ] [ 2drop "N/A" ] if ;

: find-rep-string ( str -- str )
    dup length 1 <= [ drop "N/A" ] [ (find-rep-string) ] if ;

qw{ 1001110011 1110111011 0010010010 1010101010 1111111111
    0100101101 0100100 101 11 00 1 }
"Shortest cycle:\n\n" printf
[ dup find-rep-string "%-10s -> %s\n" printf ] each
```

{{out}}

```txt

Shortest cycle:

1001110011 -> 10011
1110111011 -> 1110
0010010010 -> 001
1010101010 -> 10
1111111111 -> 1
0100101101 -> N/A
0100100    -> 010
101        -> N/A
11         -> 1
00         -> 0
1          -> N/A

```



## Forth

{{trans|Python}}
{{works with|GNU Forth}}

```forth
: rep-string ( caddr1 u1 -- caddr2 u2 ) \ u2=0: not a rep-string
   2dup dup >r  r@ 2/ /string
   begin  2over 2over string-prefix? 0=  over r@ <  and  while  -1 /string  repeat
   r> swap - >r  2drop  r> ;

: test ( caddr u -- )
   2dup type ."  has "
   rep-string ?dup 0= if drop ." no " else type ."  as " then
   ." repeating substring" cr ;
: tests
   s" 1001110011" test
   s" 1110111011" test
   s" 0010010010" test
   s" 1010101010" test
   s" 1111111111" test
   s" 0100101101" test
   s" 0100100" test
   s" 101" test
   s" 11" test
   s" 00" test
   s" 1" test ;
```

{{out}}

```forth
cr tests
1001110011 has 10011 as repeating substring
1110111011 has 1110 as repeating substring
0010010010 has 001 as repeating substring
1010101010 has 1010 as repeating substring
1111111111 has 11111 as repeating substring
0100101101 has no repeating substring
0100100 has 010 as repeating substring
101 has no repeating substring
11 has 1 as repeating substring
00 has 0 as repeating substring
1 has no repeating substring
 ok
```



## Go

{{trans|Python}}

```go
package main

import (
    "fmt"
    "strings"
)

func rep(s string) int {
    for x := len(s) / 2; x > 0; x-- {
        if strings.HasPrefix(s, s[x:]) {
            return x
        }
    }
    return 0
}

const m = `
1001110011
1110111011
0010010010
1010101010
1111111111
0100101101
0100100
101
11
00
1`

func main() {
    for _, s := range strings.Fields(m) {
        if n := rep(s); n > 0 {
            fmt.Printf("%q  %d rep-string %q\n", s, n, s[:n])
        } else {
            fmt.Printf("%q  not a rep-string\n", s)
        }
    }
}
```

{{out}}

```txt

"1001110011"  5 rep-string "10011"
"1110111011"  4 rep-string "1110"
"0010010010"  3 rep-string "001"
"1010101010"  4 rep-string "1010"
"1111111111"  5 rep-string "11111"
"0100101101"  not a rep-string
"0100100"  3 rep-string "010"
"101"  not a rep-string
"11"  1 rep-string "1"
"00"  1 rep-string "0"
"1"  not a rep-string

```



## Haskell


```haskell
import Data.List (maximumBy, inits)

repstring :: String -> Maybe String
-- empty strings are not rep strings
repstring [] = Nothing
-- strings with only one character are not rep strings
repstring (_:[]) = Nothing
repstring xs
    | any (`notElem` "01") xs = Nothing
    | otherwise = longest xs
    where
        -- length of the original string
        lxs = length xs
        -- half that length
        lq2 = lxs `quot` 2
        -- make a string of same length using repetitions of a part
        -- of the original string, and also return the substring used
        subrepeat x = (x, take lxs $ concat $ repeat x)
        -- check if a repeated string matches the original string
        sndValid (_, ys) = ys == xs
        -- make all possible strings out of repetitions of parts of
        -- the original string, which have max. length lq2
        possible = map subrepeat . take lq2 . tail . inits
        -- filter only valid possibilities, and return the substrings
        -- used for building them
        valid = map fst . filter sndValid . possible
        -- see which string is longer
        compLength a b = compare (length a) (length b)
        -- get the longest substring that, repeated, builds a string
        -- that matches the original string
        longest ys = case valid ys of
            [] -> Nothing
            zs -> Just $ maximumBy compLength zs

main :: IO ()
main = do
    mapM_ processIO examples
    where
        examples = ["1001110011", "1110111011", "0010010010",
            "1010101010", "1111111111", "0100101101", "0100100",
            "101", "11", "00", "1"]
        process = maybe "Not a rep string" id . repstring
        processIO xs = do
            putStr (xs ++ ": ")
            putStrLn $ process xs
```

{{Out}}

```txt
1001110011: 10011
1110111011: 1110
0010010010: 001
1010101010: 1010
1111111111: 11111
0100101101: Not a rep string
0100100: 010
101: Not a rep string
11: 1
00: 0
1: Not a rep string
```


Or, alternatively:

```haskell
import Data.List (inits, intercalate, transpose)
import Data.Bool (bool)

-- REP-CYCLES ---------------------------------------------
repCycles :: String -> [String]
repCycles cs =
  let n = length cs
  in filter ((cs ==) . take n . cycle) (tail $ inits (take (quot n 2) cs))

-- TEST ---------------------------------------------------
main :: IO ()
main =
  putStrLn $
  fTable
    "Longest cycles:\n"
    id
    ((flip bool "n/a" . last) <*> null)
    repCycles
    [ "1001110011"
    , "1110111011"
    , "0010010010"
    , "1010101010"
    , "1111111111"
    , "0100101101"
    , "0100100"
    , "101"
    , "11"
    , "00"
    , "1"
    ]

-- GENERIC ------------------------------------------------
fTable :: String -> (a -> String) -> (b -> String) -> (a -> b) -> [a] -> String
fTable s xShow fxShow f xs =
  let rjust n c = drop . length <*> (replicate n c ++)
      w = maximum (length . xShow <$> xs)
  in unlines $
     s : fmap (((++) . rjust w ' ' . xShow) <*> ((" -> " ++) . fxShow . f)) xs
```

{{Out}}

```txt
Longest cycles:

1001110011 -> 10011
1110111011 -> 1110
0010010010 -> 001
1010101010 -> 1010
1111111111 -> 11111
0100101101 -> n/a
   0100100 -> 010
       101 -> n/a
        11 -> 1
        00 -> 0
         1 -> n/a
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.


```unicon
procedure main(A)
   every write(s := !A,": ",(repString(s) | "Not a rep string!")\1)
end

procedure repString(s)
    rs := s[1+:*s/2]
    while (*rs > 0) & (s ~== lrepl(rs,*s,rs)) do rs := rs[1:-1]
    return (*rs > 0, rs)
end

procedure lrepl(s1,n,s2)   # The standard left() procedure won't work.
    while *s1 < n do s1 ||:= s2
    return s1[1+:n]
end
```


{{Out}}

```txt

->rs 1110111011 0010010010 1010101010 1111111111 0100101101 0100100 101 11 1
1110111011: 1110
0010010010: 001
1010101010: 1010
1111111111: 11111
0100101101: Not a rep string!
0100100: 010
101: Not a rep string!
11: 1
1: Not a rep string!
->

```



## J

Here's a test:


```j>replengths=:
:@i.@<.@-:@#
rep=: $@] $ $

isRepStr=: +./@((] -: rep)"0 1~ replengths)
```


Example use:


```j
   isRepStr '1001110011'
1
   Tests=: noun define
1001110011
1110111011
0010010010
1010101010
1111111111
0100101101
0100100
101
11
00
1
)
   isRepStr;._2 Tests     NB. run all tests
1 1 1 1 1 0 1 0 1 1 0
```


We could also report the lengths of the repeated prefix, though this seems more arbitrary:


```j
nRepStr=: 0 -.~ (([ * ] -: rep)"0 1~ replengths)
```


With the above examples:


```j
   ":@nRepStr;._2 Tests
5
4
3
2 4
1 2 3 4 5

3

1
1
```


Here, the "non-str-rep" cases are indicated by an empty list of prefix lengths.


## Java


```java
public class RepString {

    static final String[] input = {"1001110011", "1110111011", "0010010010",
        "1010101010", "1111111111", "0100101101", "0100100", "101", "11",
        "00", "1", "0100101"};

    public static void main(String[] args) {
        for (String s : input)
            System.out.printf("%s : %s%n", s, repString(s));
    }

    static String repString(String s) {
        int len = s.length();
        outer:
        for (int part = len / 2; part > 0; part--) {
            int tail = len % part;
            if (tail > 0 && !s.substring(0, tail).equals(s.substring(len - tail)))
                continue;
            for (int j = 0; j < len / part - 1; j++) {
                int a = j * part;
                int b = (j + 1) * part;
                int c = (j + 2) * part;
                if (!s.substring(a, b).equals(s.substring(b, c)))
                    continue outer;
            }
            return s.substring(0, part);
        }
        return "none";
    }
}
```


{{Out}}

```txt
1001110011 : 10011
1110111011 : 1110
0010010010 : 001
1010101010 : 1010
1111111111 : 11111
0100101101 : none
0100100 : 010
101 : none
11 : 1
00 : 0
1 : none
0100101 : none
```



## JavaScript


### ES6


```javascript
(() => {
    'use strict';

    const main = () => {

        // REP-CYCLES -------------------------------------

        // repCycles :: String -> [String]
        const repCycles = s => {
            const n = s.length;
            return filter(
                x => s === take(n, cycle(x)).join(''),
                tail(inits(take(quot(n, 2), s)))
            );
        };

        // TEST -------------------------------------------
        console.log(fTable(
            'Longest cycles:\n',
            str,
            xs => 0 < xs.length ? concat(last(xs)) : '(none)',
            repCycles,
            [
                '1001110011',
                '1110111011',
                '0010010010',
                '1010101010',
                '1111111111',
                '0100101101',
                '0100100',
                '101',
                '11',
                '00',
                '1'
            ]
        ));
    };

    // GENERIC FUNCTIONS ----------------------------------

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // cycle :: [a] -> Generator [a]
    function* cycle(xs) {
        const lng = xs.length;
        let i = 0;
        while (true) {
            yield(xs[i])
            i = (1 + i) % lng;
        }
    }

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // fTable :: String -> (a -> String) ->
    //                     (b -> String) -> (a -> b) -> [a] -> String
    const fTable = (s, xShow, fxShow, f, xs) => {
        // Heading -> x display function ->
        //           fx display function ->
        //    f -> values -> tabular string
        const
            ys = xs.map(xShow),
            w = Math.max(...ys.map(length));
        return s + '\n' + zipWith(
            (a, b) => a.padStart(w, ' ') + ' -> ' + b,
            ys,
            xs.map(x => fxShow(f(x)))
        ).join('\n');
    };

    // inits([1, 2, 3]) -> [[], [1], [1, 2], [1, 2, 3]
    // inits('abc') -> ["", "a", "ab", "abc"]

    // inits :: [a] -> [[a]]
    // inits :: String -> [String]
    const inits = xs => [
            []
        ]
        .concat(('string' === typeof xs ? xs.split('') : xs)
            .map((_, i, lst) => lst.slice(0, 1 + i)));

    // last :: [a] -> a
    const last = xs =>
        0 < xs.length ? xs.slice(-1)[0] : undefined;

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // quot :: Int -> Int -> Int
    const quot = (n, m) => Math.floor(n / m);

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

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // Use of `take` and `length` here allows zipping with non-finite lists
    // i.e. generators like cycle, repeat, iterate.

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i], bs[i], i));
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
Longest cycles:

1001110011 -> 10011
1110111011 -> 1110
0010010010 -> 001
1010101010 -> 1010
1111111111 -> 11111
0100101101 -> (none)
   0100100 -> 010
       101 -> (none)
        11 -> 1
        00 -> 0
         1 -> (none)
```



## jq

For each test string, a JSON object giving details about the prefixes that satisfy the requirement is presented; if the string is not a rep-string, the empty array ([]) is shown.

```jq
def is_rep_string:
  # if self is a rep-string then return [n, prefix]
  # where n is the number of full occurrences of prefix
  def _check(prefix; n; sofar):
    length as $length
    | if length <= (sofar|length) then [n, prefix]
      else (sofar+prefix) as $sofar
      | if  startswith($sofar) then _check(prefix; n+1; $sofar)
        elif ($sofar|length) > $length and
             startswith($sofar[0:$length]) then [n, prefix]
        else [0, prefix]
        end
      end
  ;

  [range (1; length/2 + 1) as $i
     | .[0:$i] as $prefix
     | _check($prefix; 1; $prefix)
     | select( .[0] > 1 ) ]
  ;

```

'''Example''':

```jq
def test:
  (
   "1001110011",
   "1110111011",
   "0010010010",
   "1010101010",
   "1111111111",
   "0100101101",
   "0100100",
   "101",
   "11",
   "00",
   "1"
  ) | { (.) : is_rep_string }
;

test
```

{{Out}}

```sh
 $ jq -n -c -f rep-string.jq
 {"1001110011":[[2,"10011"]]}
 {"1110111011":[[2,"1110"]]}
 {"0010010010":[[3,"001"]]}
 {"1010101010":[[5,"10"],[2,"1010"]]}
 {"1111111111":[[10,"1"],[5,"11"],[3,"111"],[2,"1111"],[2,"11111"]]}
 {"0100101101":[]}
 {"0100100":[[2,"010"]]}
 {"101":[]}
 {"11":[[2,"1"]]}
 {"00":[[2,"0"]]}
 {"1":[]}
```



## Julia

<tt>repstring</tt> returns a list of all of the substrings of its input that are the repeating units of a rep-string.  If the input is not a valid rep-string, it returns an empty list. Julia indexes strings, including those that contain multi-byte characters, at the byte level.  Because of this characteristic, <tt>repstring</tt> indexes its input using the <tt>chr2ind</tt> built-in.


```julia
function repstring(r::AbstractString)
    n = length(r)
    replst = String[]
    for m in 1:n÷2
        s = r[1:chr2ind(r, m)]
        if (s ^ cld(n, m))[1:chr2ind(r, n)] != r continue end
        push!(replst, s)
    end
    return replst
end

tests = ["1001110011", "1110111011", "0010010010", "1010101010", "1111111111",
         "0100101101", "0100100", "101", "11", "00", "1",
         "\u2200\u2203\u2200\u2203\u2200\u2203\u2200\u2203"]

for r in tests
    replst = repstring(r)
    if isempty(replst)
        println("$r is not a rep-string.")
    else
        println("$r is a rep-string of ", join(replst, ", "), ".")
    end
end
```


{{out}}

```txt
               1001110011 is a rep-string of 10011.
               1110111011 is a rep-string of 1110.
               0010010010 is a rep-string of 001.
               1010101010 is a rep-string of 10, 1010.
               1111111111 is a rep-string of 1, 11, 111, 1111, 11111.
               0100101101 is not a rep-string.
                  0100100 is a rep-string of 010.
                      101 is not a rep-string.
                       11 is a rep-string of 1.
                       00 is a rep-string of 0.
                        1 is not a rep-string.
                 ∀∃∀∃∀∃∀∃ is a rep-string of ∀∃, ∀∃∀∃.
```



## Kotlin


```scala
// version 1.0.6

fun repString(s: String): MutableList<String> {
    val reps = mutableListOf<String>()
    if (s.length < 2) return reps
    for (c in s) if (c != '0' && c != '1') throw IllegalArgumentException("Not a binary string")
    for (len in 1..s.length / 2) {
        val t = s.take(len)
        val n = s.length / len
        val r = s.length % len
        val u = t.repeat(n) + t.take(r)
        if (u == s) reps.add(t)
    }
    return reps
}

fun main(args: Array<String>) {
    val strings = listOf(
        "1001110011",
        "1110111011",
        "0010010010",
        "1010101010",
        "1111111111",
        "0100101101",
        "0100100",
        "101",
        "11",
        "00",
        "1"
    )
    println("The (longest) rep-strings are:\n")
    for (s in strings) {
        val reps = repString(s)
        val size = reps.size
        println("${s.padStart(10)} -> ${if (size > 0) reps[size - 1] else "Not a rep-string"}")
    }
}
```


{{out}}

```txt

The (longest) rep-strings are:

1001110011 -> 10011
1110111011 -> 1110
0010010010 -> 001
1010101010 -> 1010
1111111111 -> 11111
0100101101 -> Not a rep-string
   0100100 -> 010
       101 -> Not a rep-string
        11 -> 1
        00 -> 0
         1 -> Not a rep-string

```



## LFE


The heavy lifting:


```lisp

(defun get-reps (text)
  (lists:filtermap
   (lambda (x)
     (case (get-rep text (lists:split x text))
       ('() 'false)
       (x `#(true ,x))))
   (lists:seq 1 (div (length text) 2))))

(defun get-rep
  ((text `#(,head ,tail))
   (case (string:str text tail)
     (1 head)
     (_ '()))))

```


Displaying the results:


```lisp

(defun report
  ((`#(,text ()))
   (io:format "~p has no repeating characters.~n" `(,text)))
  ((`#(,text (,head . ,_)))
   (io:format "~p repeats ~p every ~p character(s).~n" `(,text ,head ,(length head))))
  ((data)
   (lists:map
    #'report/1
    (lists:zip data (lists:map #'get-reps/1 data)))
   'ok))

```


Running the code:


```txt

> (set data '("1001110011"
              "1110111011"
              "0010010010"
              "1010101010"
              "1111111111"
              "0100101101"
              "0100100"
              "101"
              "11"
              "00"
              "1"))
> (report data)
"1001110011" repeats "10011" every 5 character(s).
"1110111011" repeats "1110" every 4 character(s).
"0010010010" repeats "001" every 3 character(s).
"1010101010" repeats "10" every 2 character(s).
"1111111111" repeats "1" every 1 character(s).
"0100101101" has no repeating characters.
"0100100" repeats "010" every 3 character(s).
"101" has no repeating characters.
"11" repeats "1" every 1 character(s).
"00" repeats "0" every 1 character(s).
"1" has no repeating characters.
ok

```



## Maple

The built-in <code>Period</code> command in the <code>StringTools</code> package computes the length of the longest repeated prefix.

```Maple
repstr? := proc( s :: string )
  local   per := StringTools:-Period( s );
  if 2 * per <= length( s ) then
    true, s[ 1 .. per ]
  else
    false, ""
  end if
end proc:
```

For the given set of test strings, we can generate the following output.

```Maple

> Test := ["1001110011", "1110111011", "0010010010", "1010101010", "1111111111", \
     "0100101101", "0100100", "101", "11", "00", "1"]:
> for s in Test do
>   printf( "%*s\t%5s   %s\n", 3 + max(map(length,Test)), s, repstr?( s ) )
> end do:
   1001110011    true   10011
   1110111011    true   1110
   0010010010    true   001
   1010101010    true   10
   1111111111    true   1
   0100101101   false
      0100100    true   010
          101   false
           11    true   1
           00    true   0
            1   false


```



## Mathematica

Mathematica is based on pattern-based matching, so this is very easily implemented:

```Mathematica
RepStringQ[strin_String]:=StringCases[strin,StartOfString~~Repeated[x__,{2,\[Infinity]}]~~y___~~EndOfString/;StringMatchQ[x,StartOfString~~y~~___]:>x, Overlaps -> All]
```

Trying it out for the test-strings:
<lang>str={"1001110011","1110111011","0010010010","1010101010","1111111111","0100101101","0100100","101","11","00","1"};
{#,RepStringQ[#]}&/@str//Grid
```

{{out}}

```txt
1001110011	{10011}
1110111011	{1110}
0010010010	{001}
1010101010	{1010,10,10}
1111111111	{11111,1111,111,11,11,1,1}
0100101101	{}
0100100		{010}
101		{}
11		{1}
00		{0}
1		{}
```

It outputs all the possibilities for a rep-string,
if there is no rep-string it will show an empty list {}.


## NetRexx

{{trans|REXX}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

/* REXX ***************************************************************
* 11.05.2013 Walter Pachl
**********************************************************************/
runSample(arg)
return

/**
 * Test for rep-strings
 * @param s_str a string to check for rep-strings
 * @return Rexx string: boolean indication of reps, length, repeated value
 */
method repstring(s_str) public static
  s_str_n = s_str.length()
  rep_str = ''
  Loop lx = s_str.length() % 2 to 1 By -1
    If s_str.substr(lx + 1, lx) = s_str.left(lx) Then Leave lx
    End lx
  If lx > 0 Then Do label reps
    rep_str = s_str.left(lx)
    Loop ix = 1 By 1
      If s_str.substr(ix * lx + 1, lx) <> rep_str Then
        Leave ix
      End ix
    If rep_str.copies(s_str_n).left(s_str.length()) <> s_str Then
      rep_str = ''
    End reps
  Return (rep_str.length() > 0) rep_str.length() rep_str

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  parse arg samples
  if samples = '' then -
    samples = -
      '1001110011' -
      '1110111011' -
      '0010010010' -
      '1010101010' -
      '1111111111' -
      '0100101101' -
      '0100100' -
      '101' -
      '11' -
      '00' -
      '1'

  loop w_ = 1 to samples.words()
    in_str = samples.word(w_)
    parse repstring(in_str) is_rep_str rep_str_len rep_str

    sq = ''''in_str''''
    tstrlen = sq.length().max(20)
    sq=sq.right(tstrlen)
    if is_rep_str then
      Say sq 'has a repetition length of' rep_str_len "i.e. '"rep_str"'"
    else
      Say sq 'is not a repeated string'
    end w_
  return

```

{{out}}

```txt

        '1001110011' has a repetition length of 5 i.e. '10011'
        '1110111011' has a repetition length of 4 i.e. '1110'
        '0010010010' has a repetition length of 3 i.e. '001'
        '1010101010' has a repetition length of 4 i.e. '1010'
        '1111111111' has a repetition length of 5 i.e. '11111'
        '0100101101' is not a repeated string
           '0100100' has a repetition length of 3 i.e. '010'
               '101' is not a repeated string
                '11' has a repetition length of 1 i.e. '1'
                '00' has a repetition length of 1 i.e. '0'
                 '1' is not a repeated string

```



## NGS

{{trans|Python}}

```NGS
tests = [
	'1001110011'
	'1110111011'
	'0010010010'
	'1010101010'
	'1111111111'
	'0100101101'
	'0100100'
	'101'
	'11'
	'00'
	'1'
]

F is_repeated(s:Str) (s.len()/2..0).first(F(x) s.starts_with(s[x..null]))

{
	tests.each(F(test) {
		local r = is_repeated(test)
		echo("${test} ${if r "has repetition of length ${r} (i.e. ${test[0..r]})" "is not a rep-string"}")
	})
}
```

{{out}}
```txt
1001110011 has repetition of length 5 (i.e. 10011)
1110111011 has repetition of length 4 (i.e. 1110)
0010010010 has repetition of length 3 (i.e. 001)
1010101010 has repetition of length 4 (i.e. 1010)
1111111111 has repetition of length 5 (i.e. 11111)
0100101101 is not a rep-string
0100100 has repetition of length 3 (i.e. 010)
101 is not a rep-string
11 has repetition of length 1 (i.e. 1)
00 has repetition of length 1 (i.e. 0)
1 is not a rep-string
```



## Nim

{{trans|Python}}

```nim
import strutils

proc isRepeated(text): int =
  for x in countdown(text.len div 2, 0):
    if text.startsWith(text[x..text.high]): return x

const matchstr = """1001110011
1110111011
0010010010
1010101010
1111111111
0100101101
0100100
101
11
00
1"""

for line in matchstr.split():
  let ln = isRepeated(line)
  echo "'", line, "' has a repetition length of ", ln, " i.e ",
    (if ln > 0: "'" & line[0 .. <ln] & "'" else: "*not* a rep-string")
```

{{Out}}

```txt
'1001110011' has a repetition length of 5 i.e '10011'
'1110111011' has a repetition length of 4 i.e '1110'
'0010010010' has a repetition length of 3 i.e '001'
'1010101010' has a repetition length of 4 i.e '1010'
'1111111111' has a repetition length of 5 i.e '11111'
'0100101101' has a repetition length of 0 i.e *not* a rep-string
'0100100' has a repetition length of 3 i.e '010'
'101' has a repetition length of 0 i.e *not* a rep-string
'11' has a repetition length of 1 i.e '1'
'00' has a repetition length of 1 i.e '0'
'1' has a repetition length of 0 i.e *not* a rep-string
```



## Objeck


```objeck
class RepString {
  function : Main(args : String[]) ~ Nil {
    strings := ["1001110011", "1110111011", "0010010010", "1111111111",
        "0100101101", "0100100", "101", "11", "00", "1"];
    each(i : strings) {
      string := strings[i];
      repstring := RepString(string);
      if(repstring->Size() > 0) {
        "\"{$string}\" = rep-string \"{$repstring}\""->PrintLine();
      }
      else {
        "\"{$string}\" = not a rep-string"->PrintLine();
      };
    };
  }

  function : RepString(string : String) ~ String {
    offset := string->Size() / 2;

    while(offset > 0) {
      left := string->SubString(offset);
      right := string->SubString(left->Size(),left->Size());
      if(left->Equals(right)) {
        if(ValidateMatch(left, string)) {
          return left;
        }
        else {
          return "";
        };
      };

      offset--;
    };

    return "";
  }

  function : ValidateMatch(left : String, string : String) ~ Bool {
    parts := string->Size() / left->Size();
    tail := string->Size() % left->Size() <> 0;

    for(i := 1; i < parts; i+=1;) {
      offset := i * left->Size();
      right := string->SubString(offset, left->Size());
      if(<>left->Equals(right)) {
        return false;
      };
    };

    if(tail) {
      offset := parts * left->Size();
      right := string->SubString(offset, string->Size() - offset);
      each(i : right) {
        if(left->Get(i) <> right->Get(i)) {
          return false;
        };
      };
    };

    return true;
  }
}
```


Output:

```txt

"1001110011" = rep-string "10011"
"1110111011" = rep-string "1110"
"0010010010" = rep-string "001"
"1111111111" = rep-string "11111"
"0100101101" = not a rep-string
"0100100" = rep-string "010"
"101" = not a rep-string
"11" = rep-string "1"
"00" = rep-string "0"
"1" = not a rep-string

```



## Oforth


Returns null if no rep string.


```oforth
: repString(s)
| sz i |
   s size dup ->sz 2 / 1 -1 step: i [
      s left(sz i - ) s right(sz i -) == ifTrue: [ s left(i) return ]
      ]
   null ;
```


{{Out}}

```txt

["1001110011", "1110111011", "0010010010", "1010101010", "1111111111", "0100101101", "0100100", "101", "11", "00", "1"]
map(#repString) .
[10011, 1110, 001, 1010, 11111, null, 010, null, 1, 0, null] ok

```



## PARI/GP


```parigp
rep(v)=for(i=1,#v\2,for(j=i+1,#v,if(v[j]!=v[j-i],next(2)));return(i));0;
v=["1001110011","1110111011","0010010010","1010101010","1111111111","0100101101","0100100","101","11","00","1"];
for(i=1,#v,print(v[i]" "rep(Vec(v[i]))))
```

{{out}}

```txt
1001110011 5
1110111011 4
0010010010 3
1010101010 2
1111111111 1
0100101101 0
0100100 3
101 0
11 1
00 1
1 0
```



## Perl


```perl
foreach (qw(1001110011 1110111011 0010010010 1010101010 1111111111 0100101101 0100100 101 11 00 1)) {
    print "$_\n";
    if (/^(.+)\1+(.*$)(?(?{ substr($1, 0, length $2) eq $2 })|(?!))/) {
        print ' ' x length $1, "$1\n\n";
    } else {
        print " (no repeat)\n\n";
    }
}
```

{{out}}

```txt
1001110011
     10011

1110111011
    1110

0010010010
   001

1010101010
    1010

1111111111
     11111

0100101101
 (no repeat)

0100100
   010

101
 (no repeat)

11
 1

00
 0

1
 (no repeat)
```



## Perl 6


```perl6>for <1001110011 1110111011 0010010010 1010101010 1111111111 0100101101 0100100 101 11 00 1
 {
    if /^ (.+) $0+: (.*$) <?{ $0.substr(0,$1.chars) eq $1 }> / {
	my $rep = $0.chars;
	say .substr(0,$rep), .substr($rep,$rep).trans('01' => '𝟘𝟙'), .substr($rep*2);
    }
    else {
	say "$_ (no repeat)";
    }
}
```

{{out}}

```txt
10011𝟙𝟘𝟘𝟙𝟙
1110𝟙𝟙𝟙𝟘11
001𝟘𝟘𝟙0010
1010𝟙𝟘𝟙𝟘10
11111𝟙𝟙𝟙𝟙𝟙
0100101101 (no repeat)
010𝟘𝟙𝟘0
101 (no repeat)
1𝟙
0𝟘
1 (no repeat)
```

Here's a technique that relies on the fact that XORing the shifted binary number
should set all the lower bits to 0 if there are repeats.
(The cool thing is that shift will automatically
throw away the bits on the right that you want thrown away.)
This produces the same output as above.

```perl6
sub repstr(Str $s) {
    my $bits = :2($s);
    for reverse 1 .. $s.chars div 2 -> $left {
	my $right = $s.chars - $left;
	return $left if $bits +^ ($bits +> $left) == $bits +> $right +< $right;
    }
}


for '1001110011 1110111011 0010010010 1010101010 1111111111 0100101101 0100100 101 11 00 1'.words {
    if repstr $_ -> $rep {
	say .substr(0,$rep), .substr($rep,$rep).trans('01' => '𝟘𝟙'), .substr($rep*2);
    }
    else {
	say "$_ (no repeat)";
    }
}
```



## Phix

{{trans|Julia}}
Shows all possible repeated sub-strings, as Julia, but in the output style of Perl/Elixir

```Phix
function list_reps(string r)
sequence replist = {}
integer n = length(r)
    for m=1 to floor(n/2) do
        string s = r[1..m]
        if join(repeat(s,floor(n/m)+1),"")[1..n]=r then
            replist = append(replist,s)
        end if
    end for
    return replist
end function

constant tests = {"1001110011",
                  "1110111011",
                  "0010010010",
                  "1010101010",
                  "1111111111",
                  "0100101101",
                  "0100100",
                  "101",
                  "11",
                  "00",
                  "1"}

for i=1 to length(tests) do
    printf(1,"%s\n",{tests[i]})
    sequence replist = list_reps(tests[i])
    if length(replist)=0 then
        printf(1,"not a rep-string.\n")
    else
        for j=1 to length(replist) do
            string rj = replist[j],
                   pad = repeat(' ',length(rj))
            printf(1,"%s%s\n",{pad,rj})
        end for
    end if
    printf(1,"\n")
end for
```

{{out}}

```txt

1001110011
     10011

1110111011
    1110

0010010010
   001

1010101010
  10
    1010

1111111111
 1
  11
   111
    1111
     11111

0100101101
not a rep-string.

0100100
   010

101
not a rep-string.

11
 1

00
 0

1
not a rep-string.

```



## PicoLisp


```PicoLisp
(de repString (Str)
   (let Lst (chop Str)
      (for (N (/ (length Lst) 2)  (gt0 N)  (dec N))
         (T
            (use (Lst X)
               (let H (cut N 'Lst)
                  (loop
                     (setq X (cut N 'Lst))
                     (NIL (head X H))
                     (NIL Lst T) ) ) )
            N ) ) ) )
```

Test:

```PicoLisp
(test 5 (repString "1001110011"))
(test 4 (repString "1110111011"))
(test 3 (repString "0010010010"))
(test 4 (repString "1010101010"))
(test 5 (repString "1111111111"))
(test NIL (repString "0100101101"))
(test 3 (repString "0100100"))
(test NIL (repString "101"))
(test 1 (repString "11"))
(test 1 (repString "00"))
(test NIL (repString "1"))
(test NIL (repString "0100101"))
```



## PL/I

{{Incorrect|PL/I|"0100101101" is not a rep-string.}}

```PL/I
rep: procedure options (main); /* 5 May 2015 */
   declare s bit (10) varying;
   declare (i, k) fixed binary;

main_loop:
   do s = '1001110011'b, '1110111011'b, '0010010010'b, '1010101010'b,
          '1111111111'b, '0100101101'b, '0100100'b, '101'b, '11'b, '00'b, '1'b;
      k = length(s);
      do i = k/2 to 1 by -1;
         if substr(s, 1, i) = substr(s, i+1, i) then
            do;
               put skip edit (s, ' is a rep-string containing ', substr(s, 1, i) ) (a);
               iterate main_loop;
            end;
      end;
      put skip edit (s, ' is not a rep-string') (a);
   end;

end rep;
```

{{Out}}

```txt

1001110011 is a rep-string containing 10011
1110111011 is a rep-string containing 1110
0010010010 is a rep-string containing 001
1010101010 is a rep-string containing 1010
1111111111 is a rep-string containing 11111
0100101101 is a rep-string containing 010
0100100 is a rep-string containing 010
101 is not a rep-string
11 is a rep-string containing 1
00 is a rep-string containing 0
1 is not a rep-string

```



## Prolog


Using SWI-Prolog 7 library(func), for some functional syntax.


```Prolog
:- use_module(library(func)).

%% Implementation logic:

test_for_repstring(String, (String, Result, Reps)) :-
    ( setof(Rep, repstring(String, Rep), Reps)
    -> Result = 'no repstring'
    ;  Result = 'repstrings', Reps = []
    ).

repstring(Codes, R) :-
    RepLength = between(1) of (_//2) of length $ Codes,
    length(R, RepLength),
    phrase( (rep(R), prefix(~,R)),
            Codes).

rep(X) --> X, X.
rep(X) --> X, rep(X).


%% Demonstration output:

test_strings([`1001110011`, `1110111011`, `0010010010`, `1010101010`,
              `1111111111`, `0100101101`, `0100100`, `101`, `11`, `00`, `1`]).

report_repstring((S,Result,Reps)):-
    format('~s -- ~w: ', [S, Result]),
    foreach(member(R, Reps), format('~s, ', [R])), nl.

report_repstrings :-
    Results = maplist(test_for_repstring) $ test_strings(~),
    maplist(report_repstring, Results).
```


Output


```txt
?-  report_repstrings.
1001110011 -- repstrings: 10011,
1110111011 -- repstrings: 1110,
0010010010 -- repstrings: 001,
1010101010 -- repstrings: 10, 1010,
1111111111 -- repstrings: 1, 11, 111, 1111, 11111,
0100101101 -- no repstring:
0100100 -- repstrings: 010,
101 -- no repstring:
11 -- repstrings: 1,
00 -- repstrings: 0,
1 -- no repstring:
true.
```



## PureBasic


```purebasic
a$="1001110011"+#CRLF$+"1110111011"+#CRLF$+"0010010010"+#CRLF$+"1010101010"+#CRLF$+"1111111111"+#CRLF$+
   "0100101101"+#CRLF$+"0100100"   +#CRLF$+"101"       +#CRLF$+"11"        +#CRLF$+"00"        +#CRLF$+
   "1"         +#CRLF$

OpenConsole()

Procedure isRepStr(s1$,s2$)
  If Int(Len(s1$)/Len(s2$))>=2 : ProcedureReturn isRepStr(s1$,s2$+s2$)                         : EndIf
  If Len(s1$)>Len(s2$)         : ProcedureReturn isRepStr(s1$,s2$+Left(s2$,Len(s1$)%Len(s2$))) : EndIf
  If s1$=s2$                   : ProcedureReturn #True : Else : ProcedureReturn #False         : EndIf
EndProcedure

For k=1 To CountString(a$,#CRLF$)
  s1$=StringField(a$,k,#CRLF$) : s2$=Left(s1$,Len(s1$)/2)
  While Len(s2$)
    r=isRepStr(s1$,s2$)
    If Not r : s2$=Left(s2$,Len(s2$)-1) : Else : Break : EndIf
  Wend
  If Len(s2$) And r : PrintN(LSet(s1$,15,Chr(32))+#TAB$+"longest sequence: "+s2$) : EndIf
  If Not Len(s2$)   : PrintN(LSet(s1$,15,Chr(32))+#TAB$+"found nothing.")         : EndIf
Next
Input()
```

{{out}}

```txt
1001110011      longest sequence: 10011
1110111011      longest sequence: 1110
0010010010      longest sequence: 001
1010101010      longest sequence: 1010
1111111111      longest sequence: 11111
0100101101      found nothing.
0100100         longest sequence: 010
101             found nothing.
11              longest sequence: 1
00              longest sequence: 0
1               found nothing.
```



## Python



### Python: Procedural


```python
def is_repeated(text):
    'check if the first part of the string is repeated throughout the string'
    for x in range(len(text)//2, 0, -1):
        if text.startswith(text[x:]): return x
    return 0

matchstr = """\
1001110011
1110111011
0010010010
1010101010
1111111111
0100101101
0100100
101
11
00
1
"""
for line in matchstr.split():
    ln = is_repeated(line)
    print('%r has a repetition length of %i i.e. %s'
           % (line, ln, repr(line[:ln]) if ln else '*not* a rep-string'))
```


{{out}}

```txt
'1001110011' has a repetition length of 5 i.e. '10011'
'1110111011' has a repetition length of 4 i.e. '1110'
'0010010010' has a repetition length of 3 i.e. '001'
'1010101010' has a repetition length of 4 i.e. '1010'
'1111111111' has a repetition length of 5 i.e. '11111'
'0100101101' has a repetition length of 0 i.e. *not* a rep-string
'0100100' has a repetition length of 3 i.e. '010'
'101' has a repetition length of 0 i.e. *not* a rep-string
'11' has a repetition length of 1 i.e. '1'
'00' has a repetition length of 1 i.e. '0'
'1' has a repetition length of 0 i.e. *not* a rep-string
```



### Python: Functional

This returns all the possible repeated substrings

```python>>>
 def reps(text):
    return [text[:x] for x in range(1, 1 + len(text) // 2)
            if text.startswith(text[x:])]

>>> matchstr = """\
1001110011
1110111011
0010010010
1010101010
1111111111
0100101101
0100100
101
11
00
1
"""
>>> print('\n'.join('%r has reps %r' % (line, reps(line)) for line in matchstr.split()))
'1001110011' has reps ['10011']
'1110111011' has reps ['1110']
'0010010010' has reps ['001']
'1010101010' has reps ['10', '1010']
'1111111111' has reps ['1', '11', '111', '1111', '11111']
'0100101101' has reps []
'0100100' has reps ['010']
'101' has reps []
'11' has reps ['1']
'00' has reps ['0']
'1' has reps []
>>>
```



And we could also express this in terms of '''itertools.cycle'''
{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Rep-strings'''

from itertools import (accumulate, chain, cycle, islice)


# repCycles :: String -> [String]
def repCycles(s):
    '''Repeated sequences of characters in s.'''
    n = len(s)
    cs = list(s)

    return [
        x for x in
        tail(inits(take(n // 2)(s)))
        if cs == take(n)(cycle(x))
    ]


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests - longest cycle (if any) in each string.'''
    print(
        fTable('Longest cycles:\n')(repr)(
            lambda xs: ''.join(xs[-1]) if xs else '(none)'
        )(repCycles)([
            '1001110011',
            '1110111011',
            '0010010010',
            '1010101010',
            '1111111111',
            '0100101101',
            '0100100',
            '101',
            '11',
            '00',
            '1',
        ])
    )


# GENERIC -------------------------------------------------

# inits :: [a] -> [[a]]
def inits(xs):
    '''all initial segments of xs, shortest first.'''
    return accumulate(chain([[]], xs), lambda a, x: a + [x])


# tail :: [a] -> [a]
# tail :: Gen [a] -> [a]
def tail(xs):
    '''The elements following the head of a
       (non-empty) list or generator stream.'''
    if isinstance(xs, list):
        return xs[1:]
    else:
        list(islice(xs, 1))  # First item dropped.
        return xs


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, (list, tuple))
        else list(islice(xs, n))
    )


# OUTPUT FORMATTING ---------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function ->
                 fx display function ->
          f -> value list -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Longest cycles:

'1001110011' -> 10011
'1110111011' -> 1110
'0010010010' -> 001
'1010101010' -> 1010
'1111111111' -> 11111
'0100101101' -> (none)
   '0100100' -> 010
       '101' -> (none)
        '11' -> 1
        '00' -> 0
         '1' -> (none)
```



### Python: Regexp

This version, inspired by the Perl 6 entry uses the regexp substitute where what the match is substituted with is returned by a function.

```python
import re

matchstr = """\
1001110011
1110111011
0010010010
1010101010
1111111111
0100101101
0100100
101
11
00
1"""

def _checker(matchobj):
    g0, (g1, g2, g3, g4) = matchobj.group(0), matchobj.groups()
    if not g4 and g1 and g1.startswith(g3):
        return '%r repeats %r' % (g0, g1)
    return '%r is not a rep-string' % (g0,)

def checkit(txt):
    print(re.sub(r'(.+)(\1+)(.*)|(.*)', _checker, txt))

checkit(matchstr)
```


{{out}}

```txt
'1001110011' repeats '10011'
'1110111011' repeats '1110'
'0010010010' repeats '001'
'1010101010' repeats '1010'
'1111111111' repeats '11111'
'0100101101' is not a rep-string
'0100100' repeats '010'
'101' is not a rep-string
'11' repeats '1'
'00' repeats '0'
'1' is not a rep-string
```



### Python: find

See [https://stackoverflow.com/questions/29481088/how-can-i-tell-if-a-string-repeats-itself-in-python/29489919#29489919 David Zhang's solution] to the same question posed on Stack Overflow.


## Racket


```Racket
#lang racket

(define (rep-string str)
  (define len (string-length str))
  (for/or ([n (in-range 1 len)])
    (and (let loop ([from n])
           (or (>= from len)
               (let ([m (min (- len from) n)])
                 (and (equal? (substring str from (+ from m))
                              (substring str 0 m))
                      (loop (+ n from))))))
         (<= n (quotient len 2))
         (substring str 0 n))))

(for ([str '("1001110011"
             "1110111011"
             "0010010010"
             "1010101010"
             "1111111111"
             "0100101101"
             "0100100"
             "101"
             "11"
             "00"
             "1")])
  (printf "~a => ~a\n" str (or (rep-string str) "not a rep-string")))
```


{{Out}}

```txt
1001110011 => 10011
1110111011 => 1110
0010010010 => 001
1010101010 => 10
1111111111 => 1
0100101101 => not a rep-string
0100100 => 010
101 => not a rep-string
11 => 1
00 => 0
1 => not a rep-string
```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* 11.05.2013 Walter Pachl
* 14.05.2013 Walter Pachl extend to show additional rep-strings
**********************************************************************/
Call repstring '1001110011'
Call repstring '1110111011'
Call repstring '0010010010'
Call repstring '1010101010'
Call repstring '1111111111'
Call repstring '0100101101'
Call repstring '0100100'
Call repstring '101'
Call repstring '11'
Call repstring '00'
Call repstring '1'
Exit

repstring:
Parse Arg s
sq=''''s''''
n=length(s)
Do l=length(s)%2 to 1 By -1
  If substr(s,l+1,l)=left(s,l) Then Leave
  End
If l>0 Then Do
  rep_str=left(s,l)
  Do i=1 By 1
    If substr(s,i*l+1,l)<>rep_str Then
      Leave
    End
  If left(copies(rep_str,n),length(s))=s Then Do
    Call show_rep rep_str              /* show result                */
    Do i=length(rep_str)-1 To 1 By -1  /* look for shorter rep_str-s */
      rep_str=left(s,i)
      If left(copies(rep_str,n),length(s))=s Then
        Call show_rep rep_str
      End
    End
  Else
    Call show_norep
  End
Else
  Call show_norep
Return

show_rep:
  Parse Arg rs
  Say right(sq,12) 'has a repetition length of' length(rs) 'i.e.' ''''rs''''
  Return
show_norep:
  Say right(sq,12) 'is not a repeated string'
  Return
```

{{Out}}

```txt
'1001110011' has a repetition length of 5 i.e. '10011'
'1110111011' has a repetition length of 4 i.e. '1110'
'0010010010' has a repetition length of 3 i.e. '001'
'1010101010' has a repetition length of 4 i.e. '1010'
'1010101010' has a repetition length of 2 i.e. '10'
'1111111111' has a repetition length of 5 i.e. '11111'
'1111111111' has a repetition length of 4 i.e. '1111'
'1111111111' has a repetition length of 3 i.e. '111'
'1111111111' has a repetition length of 2 i.e. '11'
'1111111111' has a repetition length of 1 i.e. '1'
'0100101101' is not a repeated string
   '0100100' has a repetition length of 3 i.e. '010'
       '101' is not a repeated string
        '11' has a repetition length of 1 i.e. '1'
        '00' has a repetition length of 1 i.e. '0'
         '1' is not a repeated string
```



### version 2

A check was added to validate if the strings are binary strings.   The binary strings can be of any length.

```rexx
/*REXX pgm determines  if  a string is a repString, it returns minimum length repString.*/
parse arg s                                      /*get optional strings from the C.L.   */
if s=''  then s=1001110011 1110111011 0010010010 1010101010 1111111111 0100101101 0100100 101 11 00 1 45
                                                 /* [↑]  S  not specified?  Use defaults*/
     do k=1  for words(s);   _=word(s,k);   w=length(_)       /*process binary strings. */
     say right(_,max(25,w))  repString(_)                     /*show repString & result.*/
     end   /*k*/                                 /* [↑]  the  "result"  may be negatory.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
repString: procedure;  parse arg x;              L=length(x);         @rep='  rep string='
           if \datatype(x,'B')  then return "  ***error***  string isn't a binary string."
           h=L%2
                  do j=1  for L-1  while  j<=h;        $=left(x,j);     $$=copies($,L)
                  if left($$,L)==x  then  return @rep    left($,15)     "[length"     j']'
                  end   /*j*/                    /* [↑]  we have found a good repString.*/
           return               '      (no repetitions)'    /*failure to find repString.*/
```

'''output'''   when using the default binary strings for input:

```txt

               1001110011   rep string= 10011           [length 5]
               1110111011   rep string= 1110            [length 4]
               0010010010   rep string= 001             [length 3]
               1010101010   rep string= 10              [length 2]
               1111111111   rep string= 1               [length 1]
               0100101101       (no repetitions)
                  0100100   rep string= 010             [length 3]
                      101       (no repetitions)
                       11   rep string= 1               [length 1]
                       00   rep string= 0               [length 1]
                        1       (no repetitions)
                       45   ***error***  string isn't a binary string.

```



## Ring


```ring

# Project : Rep-string

test = ["1001110011",
        "1110111011",
        "0010010010",
        "1010101010",
        "1111111111",
        "0100101101",
        "0100100",
        "101",
        "11",
        "00",
        "1"]

for n = 1 to len(test)
    strend = ""
    for m=1 to len(test[n])
        strbegin = substr(test[n], 1, m)
        strcut = right(test[n], len(test[n]) - m)
        nr = substr(strcut, strbegin)
        if nr=1 and len(test[n]) > 1
           strend = strbegin
        ok
    next
    if strend = ""
       see "" + test[n] + " -> (none)" + nl
    else
       see "" + test[n] + " -> " + strend + nl
    ok
next

```

Output:

```txt

1001110011 -> 10011
1110111011 -> 1110
0010010010 -> 001
1010101010 -> 1010
1111111111 -> 11111
0100101101 -> 010
0100100 -> 010
101 -> (none)
11 -> 1
00 -> 0
1 -> (none)

```



## Ruby


```ruby
ar = %w(1001110011
        1110111011
        0010010010
        1010101010
        1111111111
        0100101101
        0100100
        101
        11
        00
        1)

ar.each do |str|
  rep_pos = (str.size/2).downto(1).find{|pos| str.start_with? str[pos..-1]}
  puts str, rep_pos ? " "*rep_pos + str[0, rep_pos] : "(no repetition)", ""
end
```

{{Out|Output (as Perl)}}

```txt
1001110011
     10011

1110111011
    1110

0010010010
   001

1010101010
    1010

1111111111
     11111

0100101101
(no repetition)

0100100
   010

101
(no repetition)

11
 1

00
 0

1
(no repetition)
```



## Scala


```Scala
object RepString extends App {
  def repsOf(s: String) = s.trim match {
    case s if s.length < 2 => Nil
    case s => (1 to (s.length/2)).map(s take _)
        .filter(_ * s.length take s.length equals s)
  }

  val tests = Array(
    "1001110011",
    "1110111011",
    "0010010010",
    "1010101010",
    "1111111111",
    "0100101101",
    "0100100",
    "101",
    "11",
    "00",
    "1"
  )
  def printReps(s: String) = repsOf(s) match {
    case Nil => s+": NO"
    case r => s+": YES ("+r.mkString(", ")+")"
  }
  val todo = if (args.length > 0) args else tests
  todo.map(printReps).foreach(println)
}
```

{{out}}

```txt
1001110011: YES (10011)
1110111011: YES (1110)
0010010010: YES (001)
1010101010: YES (10, 1010)
1111111111: YES (1, 11, 111, 1111, 11111)
0100101101: NO
0100100: YES (010)
101: NO
11: YES (1)
00: YES (0)
1: NO
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: repeatLength (in string: text) is func
  result
    var integer: length is 0;
  local
    var integer: pos is 0;
  begin
    for pos range succ(length(text) div 2) downto 1 until length <> 0 do
      if startsWith(text, text[pos ..]) then
        length := pred(pos);
      end if;
    end for;
  end func;

const proc: main is func
  local
    var string: line is "";
    var integer: length is 0;
  begin
    for line range [] ("1001110011", "1110111011", "0010010010", "1010101010",
                       "1111111111", "0100101101", "0100100", "101", "11", "00", "1") do
      length := repeatLength(line);
      if length = 0 then
        writeln("No rep-string for " <& literal(line));
      else
        writeln("Longest rep-string for " <& literal(line) <& ": " <& literal(line[.. length]));
      end if;
    end for;
  end func;
```


{{out}}

```txt

Longest rep-string for "1001110011": "10011"
Longest rep-string for "1110111011": "1110"
Longest rep-string for "0010010010": "001"
Longest rep-string for "1010101010": "1010"
Longest rep-string for "1111111111": "11111"
No rep-string for "0100101101"
Longest rep-string for "0100100": "010"
No rep-string for "101"
Longest rep-string for "11": "1"
Longest rep-string for "00": "0"
No rep-string for "1"

```



## Sidef


```ruby
var arr = <1001110011 1110111011
           0010010010 1010101010
           1111111111 0100101101
           0100100  101  11 00 1>;
 
 arr.each { |n|
    if (var m = /^(.+)\1+(.*$)(?(?{ substr($1, 0, length $2) eq $2 })|(?!))/.match(n)) {
       var i = m[0].len;
       say (n.substr(0, i),
            n.substr(i, i).tr('01', '𝟘𝟙'),
            n.substr(i*2));
    } else {
        say "#{n} (no repeat)";
    }
}
```

{{out}}

```txt
10011𝟙𝟘𝟘𝟙𝟙
1110𝟙𝟙𝟙𝟘11
001𝟘𝟘𝟙0010
1010𝟙𝟘𝟙𝟘10
11111𝟙𝟙𝟙𝟙𝟙
0100101101 (no repeat)
010𝟘𝟙𝟘0
101 (no repeat)
1𝟙
0𝟘
1 (no repeat)
```



## Swift



```swift
import Foundation

func repString(_ input: String) -> [String] {
  return (1..<(1 + input.count / 2)).compactMap({x -> String? in
    let i = input.index(input.startIndex, offsetBy: x)
    return input.hasPrefix(input[i...]) ? String(input.prefix(x)) : nil
  })
}

let testCases = """
                1001110011
                1110111011
                0010010010
                1010101010
                1111111111
                0100101101
                0100100
                101
                11
                00
                1
                """.components(separatedBy: "\n")

for testCase in testCases {
  print("\(testCase) has reps: \(repString(testCase))")
}
```


{{out}}


```txt
1001110011 has reps: ["10011"]
1110111011 has reps: ["1110"]
0010010010 has reps: ["001"]
1010101010 has reps: ["10", "1010"]
1111111111 has reps: ["1", "11", "111", "1111", "11111"]
0100101101 has reps: []
0100100 has reps: ["010"]
101 has reps: []
11 has reps: ["1"]
00 has reps: ["0"]
1 has reps: []
```



## Tcl


```tcl
proc repstring {text} {
    set len [string length $text]
    for {set i [expr {$len/2}]} {$i > 0} {incr i -1} {
	set sub [string range $text 0 [expr {$i-1}]]
	set eq [string repeat $sub [expr {int(ceil($len/double($i)))}]]
	if {[string equal -length $len $text $eq]} {
	    return $sub
	}
    }
    error "no repetition"
}
```

Demonstrating:

```tcl
foreach sample {
    "1001110011" "1110111011" "0010010010" "1010101010" "1111111111"
    "0100101101" "0100100" "101" "11" "00" "1"
} {
    if {[catch {
	set rep [repstring $sample]
	puts [format "\"%s\" has repetition (length: %d) of \"%s\"" \
		  $sample [string length $rep] $rep]
    }]} {
	puts [format "\"%s\" is not a repeated string" $sample]
    }
}
```

{{out}}

```txt

"1001110011" has repetition (length: 5) of "10011"
"1110111011" has repetition (length: 4) of "1110"
"0010010010" has repetition (length: 3) of "001"
"1010101010" has repetition (length: 4) of "1010"
"1111111111" has repetition (length: 5) of "11111"
"0100101101" is not a repeated string
"0100100" has repetition (length: 3) of "010"
"101" is not a repeated string
"11" has repetition (length: 1) of "1"
"00" has repetition (length: 1) of "0"
"1" is not a repeated string

```



## UNIX Shell

{{trans|Tcl}}
{{works with|bash}}

```bash
is_repeated() {
    local str=$1 len rep part
    for (( len = ${#str} / 2; len > 0; len-- )); do
        part=${str:0:len}
        rep=""
        while (( ${#rep} < ${#str} )); do
            rep+=$part
        done
        if [[ ${rep:0:${#str}} == $str ]] && (( $len < ${#str} )); then
            echo "$part"
            return 0
        fi
    done
    return 1
}

while read test; do
    if part=$( is_repeated "$test" ); then
        echo "$test is composed of $part repeated"
    else
        echo "$test is not a repeated string"
    fi
done <<END_TESTS
1001110011
1110111011
0010010010
1010101010
1111111111
0100101101
0100100
101
11
00
1
END_TESTS
```


{{out}}

```txt
1001110011 is composed of 10011 repeated
1110111011 is composed of 1110 repeated
0010010010 is composed of 001 repeated
1010101010 is composed of 1010 repeated
1111111111 is composed of 11111 repeated
0100101101 is not a repeated string
0100100 is composed of 010 repeated
101 is not a repeated string
11 is composed of 1 repeated
00 is composed of 0 repeated
1 is not a repeated string
```



## VBScript


```vb

Function rep_string(s)
	max_len = Int(Len(s)/2)
	tmp = ""
	If max_len = 0 Then
		rep_string = "No Repeating String"
		Exit Function
	End If
	For i = 1 To max_len
		If InStr(i+1,s,tmp & Mid(s,i,1))Then
			tmp = tmp & Mid(s,i,1)
		Else
			Exit For
		End If
	Next
	Do While Len(tmp) > 0
		If Mid(s,Len(tmp)+1,Len(tmp)) = tmp Then
			rep_string = tmp
			Exit Do
		Else
			tmp = Mid(tmp,1,Len(tmp)-1)
		End If
	Loop
	If Len(tmp) > 0 Then
		rep_string = tmp
	Else
		rep_string = "No Repeating String"
	End If
End Function

'testing the function
arr = Array("1001110011","1110111011","0010010010","1010101010",_
		"1111111111","0100101101","0100100","101","11","00","1")

For n = 0 To UBound(arr)
	WScript.StdOut.Write arr(n) & ": " & rep_string(arr(n))
	WScript.StdOut.WriteLine
Next

```


{{Out}}

```txt

1001110011: 10011
1110111011: 1110
0010010010: 001
1010101010: 1010
1111111111: 11111
0100101101: 010
0100100: 010
101: No Repeating String
11: 1
00: 0
1: No Repeating String

```



## Yabasic


```Yabasic
data "1001110011", "1110111011", "0010010010", "1010101010", "1111111111", "0100101101", "0100100", "101", "11", "00", "1", ""

sub rep$(c$, n)
    local r$, i

    for i = 1 to n
        r$ = r$ + c$
    next
    return r$
end sub

do
    read p$ : if p$ = "" break
    b$ = "" : l = len(p$) : m = int(l / 2)

    for i = m to 1 step -1
        t$ = left$(p$, i)
        s$ = rep$(t$, l / i + 1)
        if p$ = left$(s$, l) b$ = t$ : break
    next

    if b$ = "" then
        print p$, " is not a repeated string"
    else
        print p$, " is composed of ", b$, " repeated"
    end if
loop
```



## zkl

{{trans|D}}

```zkl
fcn repString(s){
   foreach n in ([s.len()/2+1..1,-1]){
      Walker.cycle(s[0,n]).pump(s.len(),String) :
      if(_==s and n*2<=s.len()) return(n);
   }
   return(False)
}
```

{{trans|Python}}

```zkl
fcn repString(s){
   foreach n in ([s.len()/2..0,-1]){
      if(s.matches(s[n,*] + "*") and n*2<=s.len()) return(n);
   }
   return(False)
}
```


```zkl
words:=("1001110011 1110111011 0010010010 1010101010 "
         "1111111111 0100101101 0100100 101 11 00 1").split(" ");
foreach w in (words){
   if(not n:=repString2(w)) "No repeat in ".println(w);
   else [0..*,n].tweak('wrap(z){ if(s:=w[z,n]) s else Void.Stop })
        .walk().concat(" ").println();
}
```

{{out}}

```txt

10011 10011
1110 1110 11
001 001 001 0
1010 1010 10
11111 11111
No repeat in 0100101101
010 010 0
No repeat in 101
1 1
0 0
No repeat in 1

```


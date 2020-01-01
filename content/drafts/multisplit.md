+++
title = "Multisplit"
description = ""
date = 2019-09-08T05:28:51Z
aliases = []
[extra]
id = 9299
[taxonomies]
categories = []
tags = []
+++

{{task}} [[Category:String manipulation]]
It is often necessary to split a string into pieces
based on several different (potentially multi-character) separator strings,
while still retaining the information about which separators were present in the input.

This is particularly useful when doing small parsing tasks.

The task is to write code to demonstrate this.

The function (or procedure or method, as appropriate) should
take an input string and an ordered collection of separators.

The order of the separators is significant:

The delimiter order represents priority in matching, with the first defined delimiter having the highest priority.
In cases where there would be an ambiguity as to
which separator to use at a particular point
(e.g., because one separator is a prefix of another)
the separator with the highest priority should be used.
Delimiters can be reused and the output from the function should be an ordered sequence of substrings.

Test your code using the input string “<code>a!===b=!=c</code>” and the separators “<code>==</code>”, “<code>!=</code>” and “<code>=</code>”.

For these inputs the string should be parsed as <code>"a" (!=) "" (==) "b" (=) "" (!=) "c"</code>, where matched delimiters are shown in parentheses, and separated strings are quoted, so our resulting output is <code>"a", empty string, "b", empty string, "c"</code>.
Note that the quotation marks are shown for clarity and do not form part of the output.

'''Extra Credit:''' provide information that indicates which separator was matched at each separation point and where in the input string that separator was matched.


## Ada

multisplit.adb:

```Ada
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

procedure Multisplit is
   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => String);
   use type String_Lists.Cursor;

   function Split
     (Source     : String;
      Separators : String_Lists.List)
      return       String_Lists.List
   is
      Result             : String_Lists.List;
      Next_Position      : Natural := Source'First;
      Prev_Position      : Natural := Source'First;
      Separator_Position : String_Lists.Cursor;
      Separator_Length   : Natural;
      Changed            : Boolean;
   begin
      loop
         Changed            := False;
         Separator_Position := Separators.First;
         while Separator_Position /= String_Lists.No_Element loop
            Separator_Length :=
              String_Lists.Element (Separator_Position)'Length;
            if Next_Position + Separator_Length - 1 <= Source'Last
              and then Source
                (Next_Position .. Next_Position + Separator_Length - 1) =
                String_Lists.Element (Separator_Position)
            then
               if Next_Position > Prev_Position then
                  Result.Append
                    (Source (Prev_Position .. Next_Position - 1));
               end if;
               Result.Append (String_Lists.Element (Separator_Position));
               Next_Position := Next_Position + Separator_Length;
               Prev_Position := Next_Position;
               Changed       := True;
               exit;
            end if;
            Separator_Position := String_Lists.Next (Separator_Position);
         end loop;
         if not Changed then
            Next_Position := Next_Position + 1;
         end if;
         if Next_Position > Source'Last then
            Result.Append (Source (Prev_Position .. Source'Last));
            exit;
         end if;
      end loop;
      return Result;
   end Split;

   Test_Input      : constant String := "a!===b=!=c";
   Test_Separators : String_Lists.List;
   Test_Result     : String_Lists.List;
   Pos             : String_Lists.Cursor;
begin
   Test_Separators.Append ("==");
   Test_Separators.Append ("!=");
   Test_Separators.Append ("=");
   Test_Result := Split (Test_Input, Test_Separators);
   Pos         := Test_Result.First;
   while Pos /= String_Lists.No_Element loop
      Ada.Text_IO.Put (" " & String_Lists.Element (Pos));
      Pos := String_Lists.Next (Pos);
   end loop;
   Ada.Text_IO.New_Line;
   -- other order of separators
   Test_Separators.Clear;
   Test_Separators.Append ("=");
   Test_Separators.Append ("!=");
   Test_Separators.Append ("==");
   Test_Result := Split (Test_Input, Test_Separators);
   Pos         := Test_Result.First;
   while Pos /= String_Lists.No_Element loop
      Ada.Text_IO.Put (" " & String_Lists.Element (Pos));
      Pos := String_Lists.Next (Pos);
   end loop;
end Multisplit;
```


{{out}}

```txt
 a != == b = != c
 a != = = b = != c
```



## ALGOL 68


```algol68
# split a string based on a number of separators #

# MODE to hold the split results #
MODE SPLITINFO = STRUCT( STRING text      # delimited string, may be empty          #
                       , INT    position  # starting position of the token          #
                       , STRING delimiter # the delimiter that terminated the token #
                       );
# calculates the length of string s #
OP   LENGTH = ( STRING s )INT: ( UPB s + 1 ) - LWB s;
# returns TRUE if s starts with p, FALSE otherwise #
PRIO STARTSWITH = 5;
OP   STARTSWITH = ( STRING s, p )BOOL: IF LENGTH p > LENGTH s THEN FALSE ELSE s[ LWB s : ( LWB s + LENGTH p ) - 1 ] = p FI;
# returns an array of SPLITINFO describing the tokens in str based on the delimiters #
# zero-length delimiters are ignored #
PRIO SPLIT = 5;
OP   SPLIT = ( STRING str, []STRING delimiters )[]SPLITINFO:
     BEGIN
        # count the number of tokens #
        # allow there to be as many tokens as characters in the string + 2 #
        # that would cater for a string composed of delimiters only        #
        [ 1 : ( UPB str + 3 ) - LWB str ]SPLITINFO tokens;
        INT   token count   := 0;
        INT   str pos       := LWB str;
        INT   str max        = UPB str;
        BOOL  token pending := FALSE;
        # construct the tokens #
        str pos       := LWB str;
        INT prev pos  := LWB str;
        token count   := 0;
        token pending := FALSE;
        WHILE str pos <= str max
        DO
            BOOL found delimiter := FALSE;
            FOR d FROM LWB delimiters TO UPB delimiters WHILE NOT found delimiter DO
                IF LENGTH delimiters[ d ] > 0 THEN
                    IF found delimiter := str[ str pos : ] STARTSWITH delimiters[ d ] THEN
                        token count          +:= 1;
                        tokens[ token count ] := ( str[ prev pos : str pos - 1 ], prev pos, delimiters[ d ] );
                        str pos              +:= LENGTH delimiters[ d ];
                        prev pos              := str pos;
                        token pending         := FALSE
                    FI
                FI
            OD;
            IF NOT found delimiter THEN
                # the current character is part of s token #
                token pending := TRUE;
                str pos      +:= 1
            FI
        OD;
        IF token pending THEN
            # there is an additional token after the final delimiter #
            token count +:= 1;
            tokens[ token count ] := ( str[ prev pos : ], prev pos, "" )
        FI;
        # return an array of the actual tokens #
        tokens[ 1 : token count ]
     END # SPLIT # ;


# test the SPLIT operator #
[]SPLITINFO test tokens = "a!===b=!=c" SPLIT []STRING( "==", "!=", "=" );
FOR t FROM LWB test tokens TO UPB test tokens DO
    SPLITINFO token = test tokens[ t ];
    print( ( "token: [",  text OF token, "] at: ", whole( position OF token, 0 ), " delimiter: (", delimiter OF token, ")", newline ) )
OD
```

{{out}}

```txt

token: [a] at: 1 delimiter: (!=)
token: [] at: 4 delimiter: (==)
token: [b] at: 6 delimiter: (=)
token: [] at: 8 delimiter: (!=)
token: [c] at: 10 delimiter: ()

```



## AutoHotkey


```AutoHotkey
Str := "a!===b=!=c"
Sep := ["==","!=", "="]
Res := StrSplit(Str, Sep)
for k, v in Res
	Out .= (Out?",":"")  v
MsgBox % Out
for k, v in Sep
	N .= (N?"|":"") "\Q" v "\E"
MsgBox % RegExReplace(str, "(.*?)(" N ")", "$1 {$2}")
```

{{out}}

```txt
a,,b,,c
a {!=} {==}b {=} {!=}c
```



## AWK


```AWK

# syntax: GAWK -f MULTISPLIT.AWK
BEGIN {
    str = "a!===b=!=c"
    sep = "(==|!=|=)"
    printf("str: %s\n",str)
    printf("sep: %s\n\n",sep)
    n = split(str,str_arr,sep,sep_arr)
    printf("parsed: ")
    for (i=1; i<=n; i++) {
      printf("'%s'",str_arr[i])
      if (i<n) { printf(" '%s' ",sep_arr[i]) }
    }
    printf("\n\nstrings: ")
    for (i=1; i<=n; i++) {
      printf("'%s' ",str_arr[i])
    }
    printf("\n\nseparators: ")
    for (i=1; i<n; i++) {
      printf("'%s' ",sep_arr[i])
    }
    printf("\n")
    exit(0)
}

```

{{out}}

```txt

str: a!===b=!=c
sep: (==|!=|=)

parsed: 'a' '!=' '' '==' 'b' '=' '' '!=' 'c'

strings: 'a' '' 'b' '' 'c'

separators: '!=' '==' '=' '!='

```


## BBC BASIC


```bbcbasic
      DIM sep$(2)
      sep$() = "==", "!=", "="
      PRINT "String splits into:"
      PRINT FNmultisplit("a!===b=!=c", sep$(), FALSE)
      PRINT "For extra credit:"
      PRINT FNmultisplit("a!===b=!=c", sep$(), TRUE)
      END

      DEF FNmultisplit(s$, d$(), info%)
      LOCAL d%, i%, j%, m%, p%, o$
      p% = 1
      REPEAT
        m% = LEN(s$)
        FOR i% = 0 TO DIM(d$(),1)
          d% = INSTR(s$, d$(i%), p%)
          IF d% IF d% < m% m% = d% : j% = i%
        NEXT
        IF m% < LEN(s$) THEN
          o$ += """" + MID$(s$, p%, m%-p%) + """"
          IF info% o$ += " (" + d$(j%) + ") " ELSE o$ += ", "
          p% = m% + LEN(d$(j%))
        ENDIF
      UNTIL m% = LEN(s$)
      = o$ + """" + MID$(s$, p%) + """"
```

{{out}}

```txt

String splits into:
"a", "", "b", "", "c"
For extra credit:
"a" (!=) "" (==) "b" (=) "" (!=) "c"

```



## Bracmat

This is a surprisingly difficult task to solve in Bracmat, because in a naive solution using a alternating pattern ("=="|"!="|"=") the shorter pattern <code>"="</code> would have precedence over <code>"=="</code>. In the solution below the function <code>oneOf</code> iterates (by recursion) over the operators, trying to match the start of the current subject string <code>sjt</code> with one operator at a time, until success or reaching the end of the list with operators, whichever comes first. If no operator is found at the start of the current subject string, the variable <code>nonOp</code> is extended with one byte, thereby shifting the start of the current subject string one byte to the right. Then a new attempt is made to find an operator. This is repeated until either an operator is found, in which case the unparsed string is restricted to the part of the input after the found operator, or no operator is found, in which case the <code>whl</code> loop terminates.

```bracmat
( ( oneOf
  =   operator
    .   !arg:%?operator ?arg
      & ( @(!sjt:!operator ?arg)&(!operator.!arg)
        | oneOf$!arg
        )
  )
& "a!===b=!=c":?unparsed
& "==" "!=" "=":?operators
&   whl
  ' ( @( !unparsed
       : ?nonOp [%(oneOf$!operators:(?operator.?unparsed))
       )
    & put$(!nonOp str$("{" !operator "} "))
    )
& put$!unparsed
& put$\n
);
```

{{out}}

```txt
a {!=} {==} b {=} {!=} c
```



## C

What kind of silly parsing is this?

```c
#include <stdio.h>
#include <string.h>

void parse_sep(const char *str, const char *const *pat, int len)
{
	int i, slen;
	while (*str != '\0') {
		for (i = 0; i < len || !putchar(*(str++)); i++) {
			slen = strlen(pat[i]);
			if (strncmp(str, pat[i], slen)) continue;
			printf("{%.*s}", slen, str);
			str += slen;
			break;
		}
	}
}

int main()
{
	const char *seps[] = { "==", "!=", "=" };
	parse_sep("a!===b=!=c", seps, 3);

	return 0;
}
```

{{out}}<lang>a{!=}{==}b{=}{!=}c
```



## C++

using the Boost library tokenizer!

```cpp
#include <iostream>
#include <boost/tokenizer.hpp>
#include <string>

int main( ) {
   std::string str( "a!===b=!=c" ) , output ;
   typedef boost::tokenizer<boost::char_separator<char> > tokenizer ;
   boost::char_separator<char> separator ( "==" , "!=" ) , sep ( "!" )  ;
   tokenizer mytok( str , separator ) ;
   tokenizer::iterator tok_iter = mytok.begin( ) ;
   for ( ; tok_iter != mytok.end( ) ; ++tok_iter )
      output.append( *tok_iter ) ;
   tokenizer nexttok ( output , sep ) ;
   for ( tok_iter = nexttok.begin( ) ; tok_iter != nexttok.end( ) ;
	 ++tok_iter )
      std::cout << *tok_iter << " " ;
   std::cout << '\n' ;
   return 0 ;
}
```

{{out}}
<PRE>a b c</PRE>


## C sharp


'''Extra Credit Solution'''


```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Multisplit
{
    internal static class Program
    {
        private static void Main(string[] args)
        {
            foreach (var s in "a!===b=!=c".Multisplit(true, "==", "!=", "=")) // Split the string and return the separators.
            {
                Console.Write(s); // Write the returned substrings and separators to the console.
            }
            Console.WriteLine();
        }

        private static IEnumerable<string> Multisplit(this string s, bool returnSeparators = false,
                                                      params string[] delimiters)
        {
            var currentString = new StringBuilder(); /* Initiate the StringBuilder. This will hold the current string to return
                                                      * once we find a separator. */

            int index = 0; // Initiate the index counter at 0. This tells us our current position in the string to read.

            while (index < s.Length) // Loop through the string.
            {
                // This will get the highest priority separator found at the current index, or null if there are none.
                string foundDelimiter =
                    (from delimiter in delimiters
                     where s.Length >= index + delimiter.Length &&
                           s.Substring(index, delimiter.Length) == delimiter
                     select delimiter).FirstOrDefault();

                if (foundDelimiter != null)
                {
                    yield return currentString.ToString(); // Return the current string.
                    if (returnSeparators) // Return the separator, if the user specified to do so.
                        yield return
                            string.Format("{{\"{0}\", ({1}, {2})}}",
                                          foundDelimiter,
                                          index, index + foundDelimiter.Length);
                    currentString.Clear(); // Clear the current string.
                    index += foundDelimiter.Length; // Move the index past the current separator.
                }
                else
                {
                    currentString.Append(s[index++]); // Add the character at this index to the current string.
                }
            }

            if (currentString.Length > 0)
                yield return currentString.ToString(); // If we have anything left over, return it.
        }
    }
}
```


{{out}}

```txt
a{"!=", (1, 3)}{"==", (3, 5)}b{"=", (6, 7)}{"!=", (7, 9)}c

```



## CoffeeScript


```coffeescript

multi_split = (text, separators) ->
  # Split text up, using separators to break up text and discarding
  # separators.
  #
  # Returns an array of strings, which can include empty strings when
  # separators are found either adjacent to each other or at the
  # beginning/end of the text.
  #
  # Separators have precedence, according to their order in the array,
  # and each separator should be at least one character long.
  result = []
  i = 0
  s = ''
  while i < text.length
    found = false
    for separator in separators
      if text.substring(i, i + separator.length) == separator
        found = true
        i += separator.length
        result.push s
        s = ''
        break
    if !found
      s += text[i]
      i += 1
  result.push s
  result

console.log multi_split 'a!===b=!=c', ['==', '!=', '='] # [ 'a', '', 'b', '', 'c' ]
console.log multi_split '', ['whatever'] # [ '' ]

```



## D


```d
import std.stdio, std.array, std.algorithm;

string[] multiSplit(in string s, in string[] divisors) pure nothrow {
    string[] result;
    auto rest = s.idup;

    while (true) {
	    bool done = true;
        string delim;
        {
            string best;
            foreach (const div; divisors) {
                const maybe = rest.find(div);
                if (maybe.length > best.length) {
                    best = maybe;
                    delim = div;
                    done = false;
                }
            }
        }
	    result.length++;
	    if (done) {
            result.back = rest.idup;
		    return result;
	    } else {
            const t = rest.findSplit(delim);
		    result.back = t[0].idup;
		    rest = t[2];
	    }
    }
}

void main() {
    "a!===b=!=c"
    .multiSplit(["==", "!=", "="])
    .join(" {} ")
    .writeln;
}
```

{{out}} (separator locations indicated by braces):

```txt
a {}  {} b {}  {} c
```



## Elixir

{{trans|Erlang}}

```elixir
iex(1)> Regex.split(~r/==|!=|=/, "a!====b=!=c")
["a", "", "", "b", "", "c"]
```



## Erlang


```txt

20> re:split("a!===b=!=c", "==|!=|=",[{return, list}]).
["a",[],"b",[],"c"]

```



## FreeBASIC

FreeBASIC does not have a built in 'split' function  so we need to write one:

```freebasic
' FB 1.05.0 Win64

Sub Split(s As String, sepList() As String, result() As String, removeEmpty As Boolean = False, showSepInfo As Boolean = False)
  If s = "" OrElse UBound(sepList) = -1 Then
     Redim result(0)
     result(0) = s
     Return
  End If
  Dim As Integer i = 0, j, count = 0, empty = 0, length
  Dim As Integer position(len(s) + 1)
  Dim As Integer sepIndex(1 To len(s))
  Dim As Integer sepLength(len(s))
  position(0) = 0 : sepLength(0) = 1

  While i  < Len(s)
    For j = lbound(sepList) To ubound(sepList)
      length = len(sepList(j))
      If length = 0 Then Continue For '' ignore blank separators
      If mid(s, i + 1, length) = sepList(j) Then
        count += 1
        position(count) = i + 1
        sepIndex(count) = j
        sepLength(count) = length
        i += length - 1
        Exit For
      End If
    Next j
    i += 1
  Wend

  Redim result(count)
  If count  = 0 Then
    If showSepInfo Then
      Print "No delimiters were found" : Print
    End If
    result(0) = s
    Return
  End If
  position(count + 1) = len(s) + 1

  For i = 1 To count + 1
    length = position(i) - position(i - 1) - sepLength(i - 1)
    result(i - 1 - empty) = Mid(s, position(i - 1) + sepLength(i - 1), length)
    If removeEmpty AndAlso cbool(length = 0) Then empty += 1
  Next

  If empty > 0 Then Redim Preserve result(count - empty)

  If showSepInfo Then
    Print "The 1-based indices of the delimiters found are : "
    Print
    For x As Integer = 1 To count
      Print "At index"; position(x), sepList(sepIndex(x))
    Next
    Print
  End If
End Sub


Dim s As String = "a!===b=!=c"
Print "The string to be split is : "; s
Print
Dim a() As String '' to hold results
Dim b(1 To 3) As String = {"==", "!=", "="} '' separators to be used in order of priority (highest first)
split s, b(), a(), False, True  '' show separator info
Print "The sub-strings are : "
Print
For i As integer = 0 To ubound(a)
 Print Using "##"; i + 1;
 Print " : "; a(i)
Next
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

The 1-based indices of the delimiters found are :

At index 2    !=
At index 4    ==
At index 7    =
At index 8    !=

The sub-strings are :

 1 : a
 2 :
 3 : b
 4 :
 5 : c

```


=={{header|F_Sharp|F#}}==

If we ignore the "Extra Credit" requirements and skip 'ordered separators' condition (i.e. solving absolute different task), this is exactly what one of the overloads of .NET's <code>String.Split</code> method does. Using F# Interactive:

```fsharp>
 "a!===b=!=c".Split([|"=="; "!="; "="|], System.StringSplitOptions.None);;
val it : string [] = [|"a"; ""; "b"; ""; "c"|]

> "a!===b=!=c".Split([|"="; "!="; "=="|], System.StringSplitOptions.None);;
val it : string [] = [|"a"; ""; ""; "b"; ""; "c"|]
```


<code>System.StringSplitOptions.None</code> specifies that empty strings should be included in the result.


## Go


```go
package main

import (
    "fmt"
    "strings"
)

func ms(txt string, sep []string) (ans []string) {
    for txt > "" {
        sepMatch := ""
        posMatch := len(txt)
        for _, s := range sep {
            if p := strings.Index(txt, s); p >= 0 && p < posMatch {
                sepMatch = s
                posMatch = p
            }
        }
        ans = append(ans, txt[:posMatch])
        txt = txt[posMatch+len(sepMatch):]
    }
    return
}

func main() {
    fmt.Printf("%q\n", ms("a!===b=!=c", []string{"==", "!=", "="}))
}
```

{{out}}

```txt

["a" "" "b" "" "c"]

```



## Haskell


```Haskell
import Data.List
       (isPrefixOf, stripPrefix, genericLength, intercalate)

trysplit :: String -> [String] -> Maybe (String, String)
trysplit s delims =
  case filter (`isPrefixOf` s) delims of
    [] -> Nothing
    (d:_) -> Just (d, (\(Just x) -> x) $ stripPrefix d s)

multisplit :: String -> [String] -> [(String, String, Int)]
multisplit list delims =
  let ms [] acc pos = [(acc, [], pos)]
      ms l@(s:sx) acc pos =
        case trysplit l delims of
          Nothing -> ms sx (s : acc) (pos + 1)
          Just (d, sxx) -> (acc, d, pos) : ms sxx [] (pos + genericLength d)
  in ms list [] 0

main :: IO ()
main = do
  let parsed = multisplit "a!===b=!=c" ["==", "!=", "="]
  mapM_
    putStrLn
    [ "split string:"
    , intercalate "," $ map (\(a, _, _) -> a) parsed
    , "with [(string, delimiter, offset)]:"
    , show parsed
    ]
```

{{out}}

```txt
split string:
a,,b,,c
with [(string, delimiter, offset)]:
[("a","!=",1),("","==",3),("b","=",6),("","!=",7),("c","",10)]
```


Or as a fold:


```haskell
import Data.List (find, isPrefixOf)

multiSplit :: [String] -> String -> [(String, String, Int)]
multiSplit ds s =
  let lng = length s
      (ts, ps, o) =
        foldl
          (\(tokens, parts, offset) (c, i) ->
              let inDelim = offset > i
              in case (if inDelim
                         then Nothing
                         else find (`isPrefixOf` drop i s) ds) of
                   Just x -> ([], (tokens, x, i) : parts, i + length x)
                   Nothing ->
                     ( if inDelim
                         then tokens
                         else c : tokens
                     , parts
                     , offset))
          ([], [], 0)
          (zip s [0 .. lng])
  in reverse $ (ts, [], lng) : ps

main :: IO ()
main = print $ multiSplit ["==", "!=", "="] "a!===b=!=c"
```

{{Out}}

```txt
[("a","!=",1),("","==",3),("b","=",6),("","!=",7),("c","",10)]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
   s := "a!===b=!=c"
   # just list the tokens
   every writes(multisplit(s,["==", "!=", "="])," ") | write()

   # list tokens and indices
   every ((p := "") ||:= t := multisplit(s,sep := ["==", "!=", "="])) | break write() do
      if t == !sep then writes(t," (",*p+1-*t,") ") else writes(t," ")

end

procedure multisplit(s,L)
s ? while not pos(0) do {
   t := =!L | 1( arb(), match(!L)|pos(0) )
   suspend t
   }
end

procedure arb()
suspend .&subject[.&pos:&pos <- &pos to *&subject + 1]
end
```


{{out}}

```txt
a != == b = != c
a != (2) == (4) b = (7) != (8) c
```



## J


```j
multisplit=: 4 :0
  'sep begin'=. |: t=. y /:~&.:(|."1)@;@(i.@#@[ ,.L:0"0 I.@E.L:0) x
  end=. begin + sep { #@>y
  last=. next=. 0
  r=. 2 0$0
  while. next<#begin do.
    r=. r,.(last}.x{.~next{begin);next{t
    last=. next{end
    next=. 1 i.~(begin>next{begin)*.begin>:last
  end.
  r=. r,.'';~last}.x
)
```


Explanation:

First find all potentially relevant separator instances, and sort them in increasing order, by starting location and separator index.  <code>sep</code> is separator index, and <code>begin</code> is starting location.  <code>end</code> is ending location.

Then, loop through the possibilities, skipping over those separators which would overlap with previously used separators.

The result consists of two rows:  The first row is the extracted substrings, the second row is the "extra credit" part -- for each extracted substring, the numbers in the second row are the separator index for the following separator (0 for the first separator, 1 for the second, ...), and the location in the original string where the beginning of the separator appeared (which is the same as where the end of the extracted substring appeared).  Note that the very last substring does not have a separator following it, so the extra credit part is blank for that substring.

Example use:


```j
   S=: 'a!===b=!=c'
   S multisplit '==';'!=';'='
┌───┬───┬───┬───┬─┐
│a  │   │b  │   │c│
├───┼───┼───┼───┼─┤
│1 1│0 3│2 6│1 7│ │
└───┴───┴───┴───┴─┘
   S multisplit '=';'!=';'=='
┌───┬───┬───┬───┬───┬─┐
│a  │   │   │b  │   │c│
├───┼───┼───┼───┼───┼─┤
│1 1│0 3│0 4│0 6│1 7│ │
└───┴───┴───┴───┴───┴─┘
   'X123Y' multisplit '1';'12';'123';'23';'3'
┌───┬───┬─┐
│X  │   │Y│
├───┼───┼─┤
│0 1│3 2│ │
└───┴───┴─┘
```



## Java


```java
import java.util.*;

public class MultiSplit {

    public static void main(String[] args) {
        System.out.println("Regex split:");
        System.out.println(Arrays.toString("a!===b=!=c".split("==|!=|=")));

        System.out.println("\nManual split:");
        for (String s : multiSplit("a!===b=!=c", new String[]{"==", "!=", "="}))
            System.out.printf("\"%s\" ", s);
    }

    static List<String> multiSplit(String txt, String[] separators) {
        List<String> result = new ArrayList<>();
        int txtLen = txt.length(), from = 0;

        for (int to = 0; to < txtLen; to++) {
            for (String sep : separators) {
                int sepLen = sep.length();
                if (txt.regionMatches(to, sep, 0, sepLen)) {
                    result.add(txt.substring(from, to));
                    from = to + sepLen;
                    to = from - 1; // compensate for the increment
                    break;
                }
            }
        }
        if (from < txtLen)
            result.add(txt.substring(from));
        return result;
    }
}
```



```txt
Regex split:
[a, , b, , c]

Manual split:
"a" "" "b" "" "c"
```



## JavaScript


### ES5

Based on Ruby example.
{{libheader|Underscore.js}}

```JavaScript
RegExp.escape = function(text) {
    return text.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&");
}

multisplit = function(string, seps) {
    var sep_regex = RegExp(_.map(seps, function(sep) { return RegExp.escape(sep); }).join('|'));
    return string.split(sep_regex);
}
```



### ES6

Without importing an external library, and generalizing to allow for any set of delimiters (avoiding the need for a hand-crafted regex):

{{Trans|Haskell}} (Multisplit by fold example)

```javascript
(() => {

    /// Delimiter list -> String -> list of parts, delimiters, offsets

    // multiSplit :: [String] -> String ->
    //      [{part::String, delim::String, offset::Int}]
    const multiSplit = (ds, s) => {
        const
            dcs = map(chars, ds),
            xs = chars(s),
            dct = foldl(
                (a, c, i, s) => {
                    const
                        inDelim = a.offset > i,
                        mb = inDelim ? (
                            nothing('')
                        ) : find(d => isPrefixOf(d, drop(i, xs)), dcs);
                    return mb.nothing ? {
                        tokens: a.tokens.concat(inDelim ? (
                            []
                        ) : [c]),
                        parts: a.parts,
                        offset: a.offset
                    } : {
                        tokens: [],
                        parts: append(a.parts, [{
                            part: intercalate('', a.tokens),
                            delim: intercalate('', mb.just),
                            offset: i
                        }]),
                        offset: i + length(mb.just)
                    };
                }, {
                    tokens: [],
                    parts: [],
                    offset: 0
                }, xs
            );
        return append(dct.parts, [{
            part: intercalate('', dct.tokens),
            delim: "",
            offset: length(s)
        }]);
    };

    // GENERIC FUNCTIONS -----------------------------------------------------

    // append (++) :: [a] -> [a] -> [a]
    const append = (xs, ys) => xs.concat(ys);

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // drop :: Int -> [a] -> [a]
    // drop :: Int -> String -> String
    const drop = (n, xs) => xs.slice(n);

    // find :: (a -> Bool) -> [a] -> Maybe a
    const find = (p, xs) => {
        for (var i = 0, lng = xs.length; i < lng; i++) {
            var x = xs[i];
            if (p(x)) return just(x);
        }
        return nothing('Not found');
    };

    // foldl :: (a -> b -> a) -> a -> [b] -> a
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // intercalate :: String -> [String] -> String
    const intercalate = (s, xs) => xs.join(s);

    // isPrefixOf takes two lists or strings and returns
    // true iff the first is a prefix of the second.
    // isPrefixOf :: [a] -> [a] -> Bool
    // isPrefixOf :: String -> String -> Bool
    const isPrefixOf = (xs, ys) => {
        const pfx = (xs, ys) => xs.length ? (
            ys.length ? xs[0] === ys[0] && pfx(
                xs.slice(1), ys.slice(1)
            ) : false
        ) : true;
        return typeof xs !== 'string' ? pfx(xs, ys) : ys.startsWith(xs);
    };

    // just :: a -> Just a
    const just = x => ({
        nothing: false,
        just: x
    });

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // nothing :: () -> Nothing
    const nothing = (optionalMsg) => ({
        nothing: true,
        msg: optionalMsg
    });

    // show :: Int -> a -> Indented String
    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[1], null, x[0]] : x
        );

    // TEST ------------------------------------------------------------------
    const
        strTest = 'a!===b=!=c',
        delims = ['==', '!=', '='];

    return show(2,
        multiSplit(delims, strTest)
    );
})();
```

{{Out}}

```txt
[
  {
    "part": "a",
    "delim": "!=",
    "offset": 1
  },
  {
    "part": "",
    "delim": "==",
    "offset": 3
  },
  {
    "part": "b",
    "delim": "=",
    "offset": 6
  },
  {
    "part": "",
    "delim": "!=",
    "offset": 7
  },
  {
    "part": "c",
    "delim": "",
    "offset": 10
  }
]
```



## jq

{{Works with|jq|1.4}}
This version does not use regular expressions, which are only supported in later versions of jq.

multisplit(delims) produces the desired parse using an intermediate parse produced by multisplit_parse(delims).

Both helper functions could be made inner functions of the main function, but are kept separate here for clarity.


```jq
# peeloff(delims) either peels off a delimiter or
# a single character from the input string.
# The input should be a nonempty string, and delims should be
# a non-empty array of delimiters;
# return [peeledoff, remainder]
# where "peeledoff" is either [delim] or the peeled off character:
def peeloff(delims):
  delims[0] as $delim
  | if startswith($delim) then [ [$delim], .[ ($delim|length):]]
    elif (delims|length)>1 then peeloff(delims[1:])
    else [ .[0:1], .[1:]]
    end ;

# multisplit_parse(delims) produces an intermediate parse.
# Input must be of the parse form: [ string, [ delim ], ... ]
# Output is of the same form.
def multisplit_parse(delims):
  if (delims|length) == 0 or length == 0 then .
  else
    .[length-1] as $last
    |  .[0:length-1] as $butlast
    | if ($last|type) == "array" then . # all done
      elif $last == "" then .
      else
        ($last | peeloff(delims)) as $p # [ peeledoff, next ]
        | $p[0] as $peeledoff
        | $p[1] as $next
        | if ($next|length) > 0
          then $butlast + [$peeledoff] + ([$next]|multisplit_parse(delims))
          else $butlast + $p
          end
      end
  end ;

def multisplit(delims):
  [.] | multisplit_parse(delims)
  # insert "" between delimiters, compress strings, remove trailing "" if any
  | reduce .[] as $x ([];
      if length == 0 then [ $x ]
      elif ($x|type) == "array"
      then if (.[length-1]|type) == "array" then . + ["",  $x]
           else  . + [$x]
           end
      elif .[length-1]|type == "string"
      then .[0:length-1] + [ .[length-1] + $x ]
      else  . + [$x]
      end ) ;
```

'''Examples'''
 ("a!===b=!=c",
  "aaa!===bbb=!=ccc") | multisplit( ["==", "!=", "="] )
{{Out}}
 $ jq -n -M -c -f multisplit.jq
 ["a",["!="],"",["=="],"b",["="],"",["!="],"c"]
 ["aaa",["!="],"",["=="],"bbb",["="],"",["!="],"ccc"]


## Julia

From REPL:

```julia

julia> split(s, r"==|!=|=")
 5-element Array{SubString{String},1}:
  "a"
  ""
  "b"
  ""
  "c"

```




## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val input = "a!===b=!=c"
    val delimiters = arrayOf("==", "!=", "=")
    val output = input.split(*delimiters).toMutableList()
    for (i in 0 until output.size) {
        if (output[i].isEmpty()) output[i] = "empty string"
        else output[i] = "\"" + output[i] + "\""
    }
    println("The splits are:")
    println(output)

    // now find positions of matched delimiters
    val matches = mutableListOf<Pair<String, Int>>()
    var index = 0
    while (index < input.length) {
        var matched = false
        for (d in delimiters) {
            if (input.drop(index).take(d.length) == d) {
                matches.add(d to index)
                index += d.length
                matched = true
                break
            }
        }
        if (!matched) index++
    }
    println("\nThe delimiters matched and the indices at which they occur are:")
    println(matches)
}
```


{{out}}

```txt

The splits are:
["a", empty string, "b", empty string, "c"]

The delimiters matched and the indices at which they occur are:
[(!=, 1), (==, 3), (=, 6), (!=, 7)]

```



## Lua

The function I've written here is really excessive for this task but it has historically been hard to find example code for a good Lua split function on the Internet.  This one behaves the same way as Julia's Base.split and I've included a comment describing its precise operation.

```Lua
--[[
Returns a table of substrings by splitting the given string on
occurrences of the given character delimiters, which may be specified
as a single- or multi-character string or a table of such strings.
If chars is omitted, it defaults to the set of all space characters,
and keep is taken to be false. The limit and keep arguments are
optional: they are a maximum size for the result and a flag
determining whether empty fields should be kept in the result.
]]
function split (str, chars, limit, keep)
    local limit, splitTable, entry, pos, match = limit or 0, {}, "", 1
    if keep == nil then keep = true end
    if not chars then
        for e in string.gmatch(str, "%S+") do
                table.insert(splitTable, e)
        end
        return splitTable
    end
    while pos <= str:len() do
        match = nil
        if type(chars) == "table" then
            for _, delim in pairs(chars) do
                if str:sub(pos, pos + delim:len() - 1) == delim then
                    match = string.len(delim) - 1
                    break
                end
            end
        elseif str:sub(pos, pos + chars:len() - 1) == chars then
            match = string.len(chars) - 1
        end
        if match then
            if not (keep == false and entry == "") then
                table.insert(splitTable, entry)
                if #splitTable == limit then return splitTable end
                entry = ""
            end
        else
            entry = entry .. str:sub(pos, pos)
        end
        pos = pos + 1 + (match or 0)
    end
    if entry ~= "" then table.insert(splitTable, entry) end
    return splitTable
end

local multisplit = split("a!===b=!=c", {"==", "!=", "="})

-- Returned result is a table (key/value pairs) - display all entries
print("Key\tValue")
print("---\t-----")
for k, v in pairs(multisplit) do
    print(k, v)
end
```

{{Out}}

```txt
Key     Value
---     -----
1       a
2
3       b
4
5       c
```



## M2000 Interpreter

Code from BBC BASIC with little changes to fit in M2000.


```M2000 Interpreter

Module CheckIt {
            DIM sep$()
            sep$() = ("==", "!=", "=")
            PRINT "String splits into:"
            FNmultisplit("a!===b=!=c", sep$(), FALSE)
            PRINT "For extra credit:"
            FNmultisplit("a!===b=!=c", sep$(), TRUE)
            END

            SUB FNmultisplit(s$, d$(), info%)
            LOCAL d%, i%, j%, m%, p%, o$
            p% = 1
            REPEAT {
                    m% = LEN(s$)
                    FOR i% = 0 TO DIMENSION(d$(),1)-1
                      d% = INSTR(s$, d$(i%), p%)
                      IF d% THEN IF d% < m% THEN m% = d% : j% = i%
                    NEXT I%
                    IF m% < LEN(s$) THEN {
                            o$ += """" + MID$(s$, p%, m%-p%) + """"
                            IF info% THEN  {o$ += " (" + d$(j%) + ") "} ELSE o$ += ", "
                            p% = m% + LEN(d$(j%))
                  }

            } UNTIL m% = LEN(s$)
            PRINT  o$ + """" + MID$(s$, p%) + """"
            END SUB
}
CheckIt

```




## Mathematica

Just use the built-in function "StringSplit":

```mathematica
StringSplit["a!===b=!=c", {"==", "!=", "="}]
```

{{Out}}

```txt
{a,,b,,c}
```



## MiniScript


```MiniScript
parseSep = function(s, pats)
    result = []
    startPos = 0
    pos = 0
    while pos < s.len
        for pat in pats
            if s[pos : pos+pat.len] != pat then continue
            result.push s[startPos : pos]
            result.push "{" + pat + "}"
            startPos = pos + pat.len
            pos = startPos - 1
            break
        end for
        pos = pos + 1
    end while
    return result
end function

print parseSep("a!===b=!=c", ["==", "!=", "="])
```

{{Out}}

```txt
["a", "{!=}", "", "{==}", "b", "{=}", "", "{!=}"]
```





## Nim


```nim
import strutils

iterator tokenize(text, sep): tuple[token: string, isSep: bool] =
  var i, lastMatch = 0
  while i < text.len:
    for j, s in sep:
      if text[i..text.high].startsWith s:
        if i > lastMatch: yield (text[lastMatch .. <i], false)
        yield (s, true)
        lastMatch = i + s.len
        i += s.high
        break
    inc i
  if i > lastMatch: yield (text[lastMatch .. <i], false)

for token, isSep in "a!===b=!=c".tokenize(["==", "!=", "="]):
  if isSep: stdout.write '{',token,'}'
  else:     stdout.write     token
echo ""
```

{{out}}

```txt
a{!=}{==}b{=}{!=}c
```



## Perl



```Perl
sub multisplit {
   my ($sep, $string, %opt) = @_ ;
   $sep = join '|', map quotemeta($_), @$sep;
   $sep = "($sep)" if $opt{keep_separators};
   split /$sep/, $string, -1;
}

print "'$_' " for multisplit ['==','!=','='], "a!===b=!=c";
print "\n";
print "'$_' " for multisplit ['==','!=','='], "a!===b=!=c", keep_separators => 1;
print "\n";
```


{{Out}}

```txt

'a' '' 'b' '' 'c'
'a' '!=' '' '==' 'b' '=' '' '!=' 'c'

```



## Perl 6

{{Works with|rakudo|2015-11-29}}

```perl6
sub multisplit($str, @seps) { $str.split(/ ||@seps /, :v) }

my @chunks = multisplit( 'a!===b=!=c==d', < == != = > );

# Print the strings.
say @chunks».Str.perl;

# Print the positions of the separators.
for grep Match, @chunks -> $s {
    say "  $s   from $s.from() to $s.to()";
}
```

{{out}}

```txt
("a", "!=", "", "==", "b", "=", "", "!=", "c", "==", "d")
  !=	from 1 to 3
  ==	from 3 to 5
  =	from 6 to 7
  !=	from 7 to 9
  ==	from 10 to 12
```

Using the array <tt>@seps</tt> in a pattern automatically does alternation.
By default this would do longest-term matching (that is, <tt>|</tt> semantics), but we can force it to do left-to-right matching by embedding the array in a short-circuit alternation (that is, <tt>||</tt> semantics).
As it happens, with the task's specified list of separators, it doesn't make any difference.
<p>
Perl 6 automatically returns Match objects that will stringify to the matched pattern, but can also be interrogated for their match positions, as illustrated above by post-processing the results two different ways.


## Phix


```Phix
procedure multisplit(string text, sequence delims)
integer k = 1, kdx
    while 1 do
        integer kmin = 0
        for i=1 to length(delims) do
            integer ki = match(delims[i],text,k)
            if ki!=0 then
                if kmin=0 or ki<kmin then
                    kmin = ki
                    kdx = i
                end if
            end if
        end for
        string token = text[k..kmin-1]
        if kmin=0 then
            printf(1,"Token: [%s] at %d\n",{token,k})
            exit
        end if
        printf(1,"Token: [%s] at %d, delimiter (%s) at %d\n",{token,k,delims[kdx],kmin})
        k = kmin+length(delims[kdx])
    end while
end procedure

multisplit("a!===b=!=c",{"==","!=","="})
```

{{out}}

```txt

Token: [a] at 1, delimiter (!=) at 2
Token: [] at 4, delimiter (==) at 4
Token: [b] at 6, delimiter (=) at 7
Token: [] at 8, delimiter (!=) at 8
Token: [c] at 10

```



## PicoLisp


```PicoLisp
(de multisplit (Str Sep)
   (setq Sep (mapcar chop Sep))
   (make
      (for (S (chop Str) S)
         (let L
            (make
               (loop
                  (T (find head Sep (circ S))
                     (link
                        (list
                           (- (length Str) (length S))
                           (pack (cut (length @) 'S)) ) ) )
                  (link (pop 'S))
                  (NIL S (link NIL)) ) )
            (link (pack (cdr (rot L))))
            (and (car L) (link @)) ) ) ) )

(println (multisplit "a!===b=!=c" '("==" "!=" "=")))
(println (multisplit "a!===b=!=c" '("=" "!=" "==")))
```

{{out}}

```txt
("a" (1 "!=") NIL (3 "==") "b" (6 "=") NIL (7 "!=") "c")
("a" (1 "!=") NIL (3 "=") NIL (4 "=") "b" (6 "=") NIL (7 "!=") "c")
```



## Pike


```Pike
string input = "a!===b=!=c";
array sep = ({"==", "!=", "=" });

array result = replace(input, sep, `+("\0", sep[*], "\0"))/"\0";
result;
Result: ({ "a", "!=", "", "==", "b", "=", "", "!=", "c" })

int pos = 0;
foreach(result; int index; string data)
{
    if ((<"==", "!=", "=">)[data])
        result[index] = ({ data, pos });
    pos+=sizeof(data);
}

result;
Result: ({"a", ({"!=", 1}), "", ({"==", 3}), "b", ({"=", 6}), "", ({"!=", 7}), "c"})
```



## PowerShell


```PowerShell

$string = "a!===b=!=c"
$separators = [regex]"(==|!=|=)"

$matchInfo = $separators.Matches($string) |
    Select-Object -Property Index, Value |
    Group-Object  -Property Value |
    Select-Object -Property @{Name="Separator"; Expression={$_.Name}},
                            Count,
                            @{Name="Position" ; Expression={$_.Group.Index}}

$matchInfo

```

{{Out}}

```txt

Separator Count Position
--------- ----- --------
!=            2 {1, 7}
==            1 3
=             1 6

```



## Prolog

Works with SWI-Prolog.

```Prolog
multisplit(_LSep, '') -->
	{!},
	[].

multisplit(LSep, T) -->
	{next_sep(LSep, T, [], Token, Sep, T1)},
	(   {Token \= '' },[Token], {!}; []),
	(   {Sep \= '' },[Sep], {!}; []),
	multisplit(LSep, T1).

next_sep([], T, Lst, Token, Sep, T1) :-
	% if we can't find any separator, the game is over
	(   Lst = [] ->
	Token = T, Sep = '', T1 = '';

	% we sort the list to get nearest longest separator
	predsort(my_sort, Lst, [(_,_, Sep)|_]),
	atomic_list_concat([Token|_], Sep, T),
	atom_concat(Token, Sep, Tmp),
	atom_concat(Tmp, T1, T)).

next_sep([HSep|TSep], T, Lst, Token, Sep, T1) :-
	sub_atom(T, Before, Len, _, HSep),
	next_sep(TSep, T, [(Before, Len,HSep) | Lst], Token, Sep, T1).

next_sep([_HSep|TSep], T, Lst, Token, Sep, T1) :-
	next_sep(TSep, T, Lst, Token, Sep, T1).


my_sort(<, (N1, _, _), (N2, _, _)) :-
	N1 < N2.

my_sort(>, (N1, _, _), (N2, _, _)) :-
	N1 > N2.

my_sort(>, (N, N1, _), (N, N2, _)) :-
	N1 < N2.

my_sort(<, (N, N1, _), (N, N2, _)) :-
	N1 > N2.

```

{{out}}

```txt
?- multisplit(['==', '!=', '='], 'ax!===b=!=c', Lst, []).
Lst = [ax,'!=',==,b,=,'!=',c] .

```



## Python



### Using Regular expressions


```python>>>
 import re
>>> def ms2(txt="a!===b=!=c", sep=["==", "!=", "="]):
	if not txt or not sep:
		return []
	ans = m = []
	for m in re.finditer('(.*?)(?:' + '|'.join('('+re.escape(s)+')' for s in sep) + ')', txt):
		ans += [m.group(1), (m.lastindex-2, m.start(m.lastindex))]
	if m and txt[m.end(m.lastindex):]:
		ans += [txt[m.end(m.lastindex):]]
	return ans

>>> ms2()
['a', (1, 1), '', (0, 3), 'b', (2, 6), '', (1, 7), 'c']
>>> ms2(txt="a!===b=!=c", sep=["=", "!=", "=="])
['a', (1, 1), '', (0, 3), '', (0, 4), 'b', (0, 6), '', (1, 7), 'c']
```


===Not using RE's===
'''Inspired by C-version'''

```python
def multisplit(text, sep):
    lastmatch = i = 0
    matches = []
    while i < len(text):
        for j, s in enumerate(sep):
            if text[i:].startswith(s):
                if i > lastmatch:
                    matches.append(text[lastmatch:i])
                matches.append((j, i))  # Replace the string containing the matched separator with a tuple of which separator and where in the string the match occured
                lastmatch = i + len(s)
                i += len(s)
                break
        else:
            i += 1
    if i > lastmatch:
        matches.append(text[lastmatch:i])
    return matches

>>> multisplit('a!===b=!=c', ['==', '!=', '='])
['a', (1, 1), (0, 3), 'b', (2, 6), (1, 7), 'c']
>>> multisplit('a!===b=!=c', ['!=', '==', '='])
['a', (0, 1), (1, 3), 'b', (2, 6), (0, 7), 'c']

```


'''Alternative version'''

```python
def min_pos(List):
	return List.index(min(List))

def find_all(S, Sub, Start = 0, End = -1, IsOverlapped = 0):
	Res = []
	if End == -1:
		End = len(S)
	if IsOverlapped:
		DeltaPos = 1
	else:
		DeltaPos = len(Sub)
	Pos = Start
	while True:
		Pos = S.find(Sub, Pos, End)
		if Pos == -1:
			break
		Res.append(Pos)
		Pos += DeltaPos
	return Res

def multisplit(S, SepList):
	SepPosListList = []
	SLen = len(S)
	SepNumList = []
	ListCount = 0
	for i, Sep in enumerate(SepList):
		SepPosList = find_all(S, Sep, 0, SLen, IsOverlapped = 1)
		if SepPosList != []:
			SepNumList.append(i)
			SepPosListList.append(SepPosList)
			ListCount += 1
	if ListCount == 0:
		return [S]
	MinPosList = []
	for i in range(ListCount):
		MinPosList.append(SepPosListList[i][0])
	SepEnd = 0
	MinPosPos = min_pos(MinPosList)
	Res = []
	while True:
		Res.append( S[SepEnd : MinPosList[MinPosPos]] )
		Res.append([SepNumList[MinPosPos], MinPosList[MinPosPos]])
		SepEnd = MinPosList[MinPosPos] + len(SepList[SepNumList[MinPosPos]])
		while True:
			MinPosPos = min_pos(MinPosList)
			if MinPosList[MinPosPos] < SepEnd:
				del SepPosListList[MinPosPos][0]
				if len(SepPosListList[MinPosPos]) == 0:
					del SepPosListList[MinPosPos]
					del MinPosList[MinPosPos]
					del SepNumList[MinPosPos]
					ListCount -= 1
					if ListCount == 0:
						break
				else:
					MinPosList[MinPosPos] = SepPosListList[MinPosPos][0]
			else:
				break
		if ListCount == 0:
			break
	Res.append(S[SepEnd:])
	return Res


S = "a!===b=!=c"
multisplit(S, ["==", "!=", "="]) # output: ['a', [1, 1], '', [0, 3], 'b', [2, 6], '', [1, 7], 'c']
multisplit(S, ["=", "!=", "=="]) # output: ['a', [1, 1], '', [0, 3], '', [0, 4], 'b', [0, 6], '', [1, 7], 'c']
```



## Racket



```racket

#lang racket
(regexp-match* #rx"==|!=|=" "a!===b=!=c" #:gap-select? #t #:match-select values)
;; => '("a" ("!=") "" ("==") "b" ("=") "" ("!=") "c")

```



## REXX


```rexx
/*REXX program  splits  a (character) string  based on different  separator  delimiters.*/
parse arg $                                      /*obtain optional string from the C.L. */
if $=''   then $= "a!===b=!=c"                   /*None specified?  Then use the default*/
say 'old string:' $                              /*display the old string to the screen.*/
null= '0'x                                       /*null char.   It can be most anything.*/
seps= '== != ='                                  /*list of separator strings to be used.*/
                                                 /* [↓]   process the tokens in  SEPS.  */
  do j=1  for words(seps)                        /*parse the string with all the seps.  */
  sep=word(seps,j)                               /*pick a separator to use in below code*/
                                                 /* [↓]   process characters in the sep.*/
        do k=1  for length(sep)                  /*parse for various separator versions.*/
        sep=strip(insert(null, sep, k), , null)  /*allow imbedded "nulls" in separator, */
        $=changestr(sep, $, null)                /*       ··· but not trailing "nulls". */
                                                 /* [↓]   process strings in the input. */
             do  until $==old;      old=$        /*keep changing until no more changes. */
             $=changestr(null || null, $, null)  /*reduce replicated "nulls" in string. */
             end   /*until*/
                                                 /* [↓]  use  BIF  or  external program.*/
        sep=changestr(null, sep, '')             /*remove true nulls from the separator.*/
        end        /*k*/
  end              /*j*/

showNull= ' {} '                                 /*just one more thing, display the ··· */
$=changestr(null, $, showNull)                   /*        ··· showing of "null" chars. */
say 'new string:' $                              /*now, display the new string to term. */
                                                 /*stick a fork in it,  we're all done. */
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ──►   [[CHANGESTR.REX]].


'''output'''   when using the default input:

```txt

old string=a!===b=!=c
new string=a {} b {} c

```



## Ring


```ring

# Project : Multisplit

str = "a!===b=!=c"
sep = "=== != =! b =!="
sep = str2list(substr(sep, " ", nl))
for n = 1 to len(sep)
      pos = substr(str, sep[n])
      see "" + n + ": " + substr(str, 1, pos-1) + " Sep By: " + sep[n] + nl
next

```

Output:

```txt

1: a! Sep By: ===
2: a Sep By: !=
3: a!===b Sep By: =!
4: a!=== Sep By: b
5: a!===b Sep By: =!=

```



## Ruby

The simple method, using a regular expression to split the text.


```ruby
text = 'a!===b=!=c'
separators = ['==', '!=', '=']

def multisplit_simple(text, separators)
  text.split(Regexp.union(separators))
end

p multisplit_simple(text, separators) # => ["a", "", "b", "", "c"]

```


The version that also returns the information about the separations.


```ruby
def multisplit(text, separators)
  sep_regex = Regexp.union(separators)
  separator_info = []
  pieces = []
  i = prev = 0
  while i = text.index(sep_regex, i)
    separator = Regexp.last_match(0)
    pieces << text[prev .. i-1]
    separator_info << [separator, i]
    i = i + separator.length
    prev = i
  end
  pieces << text[prev .. -1]
  [pieces, separator_info]
end

p multisplit(text, separators)
# => [["a", "", "b", "", "c"], [["!=", 1], ["==", 3], ["=", 6], ["!=", 7]]]
```


Also demonstrating a method to rejoin the string given the separator information.


```ruby
def multisplit_rejoin(info)
  str = info[0].zip(info[1])[0..-2].inject("") {|str, (piece, (sep, idx))| str << piece << sep}
  str << info[0].last
end

p multisplit_rejoin(multisplit(text, separators)) == text
# => true
```



## Run BASIC


```runbasic
str$ = "a!===b=!=c"
sep$ = "=== != =! b =!="

while word$(sep$,i+1," ") <> ""
 i = i + 1
 theSep$ = word$(sep$,i," ")
 split$  = word$(str$,1,theSep$)
 print i;" ";split$;" Sep By: ";theSep$
wend
```

{{out}}

```txt
1 a!     Sep By: ===
2 a      Sep By: !=
3 a!===b Sep By: =!
4 a!===  Sep By: b
5 a!===b Sep By: =!=
```



## Scala


```scala
import scala.annotation.tailrec
def multiSplit(str:String, sep:Seq[String])={
   def findSep(index:Int)=sep find (str startsWith (_, index))
   @tailrec def nextSep(index:Int):(Int,Int)=
      if(index>str.size) (index, 0) else findSep(index) match {
         case Some(sep) => (index, sep.size)
         case _ => nextSep(index + 1)
      }
   def getParts(start:Int, pos:(Int,Int)):List[String]={
      val part=str slice (start, pos._1)
      if(pos._2==0) List(part) else part :: getParts(pos._1+pos._2, nextSep(pos._1+pos._2))
   }
   getParts(0, nextSep(0))
}

println(multiSplit("a!===b=!=c", Seq("!=", "==", "=")))
```

{{out}}

```txt
List(a, , b, , c)
```



## Scheme

{{works with|Gauche Scheme}}

```Scheme
(use srfi-13)
(use srfi-42)

(define (shatter separators the-string)
  (let loop ((str the-string) (tmp ""))
    (if (string=? "" str)
      (list tmp)
      (if-let1 sep (find (^s (string-prefix? s str)) separators)
        (cons* tmp sep
          (loop (string-drop str (string-length sep)) ""))
        (loop (string-drop str 1) (string-append tmp (string-take str 1)))))))

(define (glean shards)
  (list-ec (: x (index i) shards)
    (if (even? i)) x))
```

<b>Testing:</b>

```txt

(glean (shatter '("==" "!=" "=") "a!===b=!=c"))
("a" "" "b" "" "c")

(shatter '("==" "!=" "=") "a!===b=!=c")
("a" "!=" "" "==" "b" "=" "" "!=" "c")

```



## Sidef


```ruby
func multisplit(sep, str, keep_sep=false) {
    sep = sep.map{.escape}.join('|');
    var re = Regex.new(keep_sep ? "(#{sep})" : sep);
    str.split(re, -1);
}

[false, true].each { |bool|
    say multisplit(%w(== != =), 'a!===b=!=c', keep_sep: bool);
}
```

{{out}}

```txt

["a", "", "b", "", "c"]
["a", "!=", "", "==", "b", "=", "", "!=", "c"]

```



## Tcl

This simple version does not retain information about what the separators were:

```tcl
proc simplemultisplit {text sep} {
    set map {}; foreach s $sep {lappend map $s "\uffff"}
    return [split [string map $map $text] "\uffff"]
}
puts [simplemultisplit "a!===b=!=c" {"==" "!=" "="}]
```

{{out}}

```txt
a {} b {} c
```

However, to keep the match information a more sophisticated technique is best. Note that the most natural model of result here
is to return the split substrings as a separate list
to the match information (because the two collections of information
are of different lengths).

```tcl
proc multisplit {text sep} {
    foreach s $sep {lappend sr [regsub -all {\W} $s {\\&}]}
    set sepRE [join $sr "|"]
    set pieces {}
    set match {}
    set start 0
    while {[regexp -indices -start $start -- $sepRE $text found]} {
	lassign $found x y
	lappend pieces [string range $text $start [expr {$x-1}]]
	lappend match [lsearch -exact $sep [string range $text {*}$found]] $x
	set start [expr {$y + 1}]
    }
    return [list [lappend pieces [string range $text $start end]] $match]
}
```

Demonstration code:

```tcl
set input "a!===b=!=c"
set matchers {"==" "!=" "="}
lassign [multisplit $input $matchers] substrings matchinfo
puts $substrings
puts $matchinfo
```

{{out}}

```txt

a {} b {} c
1 1 0 3 2 6 1 7

```



## TXR


===Using text-extraction pattern language===

Here, the separators are embedded into the syntax rather than appearing as a datum. Nevertheless, this illustrates how to do that small tokenizing task with various separators.

The clauses of <code>choose</code> are applied in parallel, and all potentially match at the current position in the text.
However <code>:shortest tok</code> means that only that clause survives (gets to propagate its bindings and position advancement) which minimizes the length of the string which is bound to the <code>tok</code> variable.
The <code>:gap 0</code> makes the horizontal collect repetitions strictly adjacent. This means that <code>coll</code> will quit when faced with a nonmatching suffix portion of the data rather than scan forward (no gap allowed!). This creates an opportunity for the <code>tail</code> variable to grab the suffix which remains, which may be an empty string.


```txr
@(next :args)
@(coll :gap 0)@(choose :shortest tok)@\
                @tok@{sep /==/}@\
              @(or)@\
                @tok@{sep /!=/}@\
              @(or)@\
                @tok@{sep /=/}@\
              @(end)@(end)@tail
@(output)
@(rep)"@tok" {@sep} @(end)"@tail"
@(end)
```


Runs:


```txt
$ ./txr multisplit.txr 'a!===b=!=c'
"a" {!=} "" {==} "b" {=} "" {!=} "c"
$ ./txr  multisplit.txr 'a!===!==!=!==b'
"a" {!=} "" {==} "" {!=} "" {=} "" {!=} "" {!=} "" {=} "b"
$ ./txr  multisplit.txr ''
""
$ ./txr  multisplit.txr 'a'
"a"
$ ./txr  multisplit.txr 'a='
"a" {=} ""
$ ./txr  multisplit.txr '='
"" {=} ""
$ ./txr  multisplit.txr '=='
"" {==} ""
$ ./txr  multisplit.txr '==='
"" {==} "" {=} ""
```


===Using the <code>tok-str</code> function===

{{trans|Racket}}


```sh
$ txr -p '(tok-str "a!===b=!=c" #/==|!=|=/ t)'
("a" "!=" "" "==" "b" "=" "" "!=" "c")
```


Here the third boolean argument means "keep the material between the tokens", which in the Racket version seems to be requested by the argument <code>#:gap-select? #:t</code>.


## UNIX Shell

{{works with|bash}}

```bash
multisplit() {
    local str=$1
    shift
    local regex=$( IFS='|'; echo "$*" )
    local sep
    while [[ $str =~ $regex ]]; do
        sep=${BASH_REMATCH[0]}
        words+=( "${str%%${sep}*}" )
        seps+=( "$sep" )
        str=${str#*$sep}
    done
    words+=( "$str" )
}

words=() seps=()

original="a!===b=!=c"
recreated=""

multisplit "$original" "==" "!=" "="

for ((i=0; i<${#words[@]}; i++)); do
    printf 'w:"%s"\ts:"%s"\n' "${words[i]}" "${seps[i]}"
    recreated+="${words[i]}${seps[i]}"
done

if [[ $original == $recreated ]]; then
    echo "successfully able to recreate original string"
fi
```


{{out}}

```txt
w:"a"	s:"!="
w:""	s:"=="
w:"b"	s:"="
w:""	s:"!="
w:"c"	s:""
successfully able to recreate original string
```



## VBScript


```vb

Function multisplit(s,sep)
	arr_sep = Split(sep,"|")
	For i = 0 To UBound(arr_sep)
		arr_s = Split(s,arr_sep(i))
		s = Join(arr_s,",")
	Next
	multisplit = s
End Function

Function multisplit_extra(s,sep)
	Set dict_sep = CreateObject("Scripting.Dictionary")
	arr_sep = Split(sep,"|")
	For i = 0 To UBound(arr_sep)
		dict_sep.Add i,"(" & arr_sep(i) & ")"
		arr_s = Split(s,arr_sep(i))
		s = Join(arr_s,i)
	Next
	For Each key In dict_sep.Keys
		s = Replace(s,key,dict_sep.Item(key))
	Next
	multisplit_extra = s
End Function

WScript.StdOut.Write "Standard: " & multisplit("a!===b=!=c","!=|==|=")
WScript.StdOut.WriteLine
WScript.StdOut.Write "Extra Credit: " & multisplit_extra("a!===b=!=c","!=|==|=")
WScript.StdOut.WriteLine
```

{{out}}

```txt

Standard: a,,b,,c
Extra Credit: a(!=)(==)b(=)(!=)c

```



## Yabasic


```Yabasic
t$ = "a!===b=!=c"
s$ = "==,!=,="

dim n$(1)

n = token(s$, n$(), ",")
dim p(n)

do
    l = len(t$)
    j = 0
    for i = 1 to n
        p(i) = instr(t$, n$(i))
        if p(i) and p(i) < l then l = p(i) : j = i end if
    next
    if not j print t$ : break
    print left$(t$, l - 1), " with separator ", n$(j)
    t$ = right$(t$, len(t$) - (l + len(n$(j))) + 1)
loop
```



## zkl

{{trans|Python}}

```zkl
fcn multisplit(text, sep){
   lastmatch := i := 0; matches := List();
   while(i < text.len()){
      foreach j,s in ([0..].zip(sep)){
         if(i == text.find(s,i)){
	    if(i > lastmatch) matches.append(text[lastmatch,i-lastmatch]);
	    matches.append(T(j,i));  # Replace the string containing the matched separator with a tuple of which separator and where in the string the match occured
	    lastmatch = i + s.len();
	    i += s.len()-1;
	    break;
	 }
      }
      i += 1;
   }
   if(i > lastmatch) matches.append(text[lastmatch,i-lastmatch]);
   return(matches);
}
```


```zkl
multisplit("a!===b=!=c", T("==", "!=", "=")).println();
multisplit("a!===b=!=c", T("!=", "==", "=")).println();
```

{{out}}

```txt

L("a",L(1,1),L(0,3),"b",L(2,6),L(1,7),"c")
L("a",L(0,1),L(1,3),"b",L(2,6),L(0,7),"c")

```


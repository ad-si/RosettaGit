+++
title = "Range expansion"
description = ""
date = 2019-08-12T23:19:55Z
aliases = []
[extra]
id = 7773
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{:Range extraction/Format}}


;Task:
Expand the range description: 
  <big> -6,-3--1,3-5,7-11,14,15,17-20 </big>

<small>Note that the second element above, 
is the '''range from minus 3 to ''minus'' 1'''. </small>


;Related task:
*   [[Range extraction]]





## 11l

{{trans|Python}}

```11l
F rangeexpand(txt)
   Array[Int] lst
   L(r) txt.split(‘,’)
      I ‘-’ C r[1..]
         V rr = r[1..].split(‘-’, 2)
         lst [+]= Int(r[0]‘’rr[0]) .. Int(rr[1])
      E
         lst.append(Int(r))
   R lst

print(rangeexpand(‘-6,-3--1,3-5,7-11,14,15,17-20’))
```



## 8th


```Forth
\ Given a low and high limit, create an array containing the numbers in the
\ range, inclusive:
: n:gen-range \ low hi -- a
  \ make sure they are in order:
  2dup n:> if swap then
  \ fill the array with the values:
  [] ' a:push 
  2swap loop ;

\ Take a string, either "X" or "X-Y", and correctly return either a number (if
\ "X") or an array of numbers (if "X-Y"):
: n:expand-one \ s -- n | a[n,..m]
  \ First see if we can parse a number.  This works in the "X" case:
  dup >n null? if
    \ Failed >n because it's (possibly) "X-Y"
    drop
    \ not a valid number, might be a range
    \ We'll use a capturing regex to handle the different cases correctly:
    /(-?[0-9]+)-(-?[0-9]+)/ tuck r:match 

    \ If the regex matches three (the whole string, plus the two captured
    \ expressions) then it's a valid "X-Y":
    3 n:= if
      1 r:@ >n swap 2 r:@ >n nip
      \ generate the range:
      n:gen-range
    else
      \ The regex didn't match, so we got garbage.  Therefore, return a 'null':
      drop null
    then
  else
    \ It was a "X", just drop the original string:
    nip
  then 
  ;

\ Take an array (possibly) containing other arrays, and flatten any contained
\ arrays so the result is a simple array:
: a:flatten \ a1 -- a2
  [] >r
  (
    nip
    array?  if
      a:flatten r> swap a:+ >r
    else
      r> swap a:push >r
    then
  ) a:each drop r> ;

\ Take a comma-delimited string of ranges, and expand it into an array of
\ numbers:
: n:range-expand \ str -- a
  "," s:/
  ' n:expand-one a:map
  a:flatten ;

\ Process a list:
"-6,-3--1,3-5,7-11,14,15,17-20" 
n:range-expand
\ print the expanded list:
. cr bye
```

{{out}}

```txt

[-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]

```


## Ada

The function Expand takes a string and returns a corresponding array of integers. 
Upon syntax errors Constraint_Error is propagated:

```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Range_Expansion is
   type Sequence is array (Positive range <>) of Integer;
   function Expand (Text : String) return Sequence is
      To    : Integer := Text'First;
      Count : Natural := 0;
      Low   : Integer;
      function Get return Integer is
         From : Integer := To;
      begin
         if Text (To) = '-' then
            To := To + 1;
         end if;
         while To <= Text'Last loop
            case Text (To) is
               when ',' | '-' => exit;
               when others => To := To + 1;
            end case;
         end loop;
         return Integer'Value (Text (From..To - 1));
      end Get;
   begin
      while To <= Text'Last loop -- Counting items of the list
         Low := Get;
         if To > Text'Last or else Text (To) = ',' then
            Count := Count + 1;
         else
            To := To + 1;
            Count := Count + Get - Low + 1;
         end if;
         To := To + 1;
      end loop;
      return Result : Sequence (1..Count) do
         Count := 0;
         To := Text'First;
         while To <= Text'Last loop -- Filling the list
            Low := Get;
            if To > Text'Last or else Text (To) = ',' then
               Count := Count + 1;
               Result (Count) := Low;
            else
               To := To + 1;
               for Item in Low..Get loop
                  Count := Count + 1;
                  Result (Count) := Item;
               end loop;
            end if;
            To := To + 1;
         end loop;
      end return;
   end Expand;
   procedure Put (S : Sequence) is
      First : Boolean := True;
   begin
      for I in S'Range loop
         if First then
            First := False;
         else
            Put (',');
         end if;
         Put (Integer'Image (S (I)));
      end loop;
   end Put;
begin
   Put (Expand ("-6,-3--1,3-5,7-11,14,15,17-20"));
end Test_Range_Expansion;
```

{{out}}

```txt

-6,-3,-2,-1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20

```



## Aime

{{incorrect|Aime|Needs "a comma separated list" without the trailing comma}}

```aime
list l;

file().b_affix("-6,-3--1,3-5,7-11,14,15,17-20").news(l, 0, 0, ",");
for (, text s in l) {
    integer a, b, p;

    p = b_frame(s, '-');
    if (p < 1) {
        o_(s, ",");
    } else {
        p -= s[p - 1] == '-' ? 1 : 0;
        a = s.cut(0, p).atoi;
        b = s.erase(0, p).atoi;
        do {
            o_(a, ",");
        } while ((a += 1) <= b);
    }
}

o_("\n");
```

or:

```aime
integer p;
list l;

file().b_affix("-6,-3--1,3-5,7-11,14,15,17-20").news(l, 0, 0, ",");
for (, text s in l) {
    if ((p = b_frame(s, '-')) < 1) {
        o_(s, ",");
    } else {
        p -= s[p - 1] == '-' ? 1 : 0;
        call_s(o_, 0, s.cut(0, p).atoi, s.erase(0, p).atoi + 1, 1, ",");
    }
}

o_("\n");
```

{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20,
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny] - string parsing and formatting code tested with 2.6.win32}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
MODE YIELDINT = PROC(INT)VOID;

MODE RANGE = STRUCT(INT lwb, upb);
MODE RANGEINT = UNION(RANGE, INT);

OP SIZEOF = ([]RANGEINT list)INT: (
# determine the length of the output array #
  INT upb := LWB list - 1;
  FOR key FROM LWB list TO UPB list DO
    CASE list[key] IN
      (RANGE value): upb +:= upb OF value - lwb OF value + 1,
      (INT): upb +:= 1
    ESAC
  OD;
  upb
);

PROC gen range expand = ([]RANGEINT list, YIELDINT yield)VOID:
  FOR key FROM LWB list TO UPB list DO
    CASE list[key] IN
      (RANGE range): FOR value FROM lwb OF range TO upb OF range DO yield(value) OD,
      (INT int): yield(int)
    ESAC
  OD;

PROC range expand = ([]RANGEINT list)[]INT: (
  [LWB list: LWB list + SIZEOF list - 1]INT out;
  INT upb := LWB out - 1;
# FOR INT value IN # gen range expand(list, # ) DO #
##   (INT value)VOID:
    out[upb +:= 1] := value
# OD #);
  out
);

#
test:(
  []RANGEINT list = (-6, RANGE(-3, -1), RANGE(3, 5),  RANGE(7, 11), 14, 15, RANGE(17, 20));
  print((range expand(list), new line))
)
#


# converts string containing a comma-separated list of ranges and values to a []RANGEINT #
OP TORANGE = ( STRING s )[]RANGEINT:
BEGIN

    # counts the number of elements - one more than the number of commas #
    # and so assumes there is always at least one element                #
    PROC count elements = INT:
    BEGIN

        INT    elements := 1;

        FOR pos FROM LWB s TO UPB s
        DO
            IF s[ pos ] = ","
            THEN
                elements +:= 1
            FI
        OD;

    # RESULT #
        elements
    END; # count elements #

    REF[]RANGEINT result = HEAP [ 1 : count elements ]RANGEINT;

    # does the actual parsing - assumes the string is syntatically valid and doesn't check for errors #
    # - in particular, a string with no elements will cause problems, as will space characters in the string #
    PROC parse range string = []RANGEINT:
    BEGIN

        INT  element := 0;
        INT  str pos := 1;

        PROC next       = VOID: str pos +:= 1;
        PROC curr char  = CHAR: IF str pos > UPB s THEN "?" ELSE s[ str pos ] FI;
        PROC have minus = BOOL: curr char = "-";
        PROC have digit = BOOL: curr char >= "0" AND curr char <= "9";


        # parses a number out of the string #
        # the number must be a sequence of digits with an optional leading minus sign #
        PROC get number = INT:
        BEGIN

            INT number := 0;

            INT sign multiplier = IF have minus
                                  THEN
                                      # negaive number #
                                      # skip the sign #
                                      next;
                                      -1
                                  ELSE
                                      # positive number #
                                      1
                                  FI;

            WHILE curr char >= "0" AND curr char <= "9"
            DO
                number *:= 10;
                number +:= ( ABS curr char - ABS "0" );
                next
            OD;

        # RESULT #
            number * sign multiplier
        END; # get number #


        # main parsing #
        WHILE str pos <= UPB s
        DO
            CHAR c = curr char;

            IF have minus OR have digit
            THEN
                # have the start of a number #
                INT from value = get number;
                element +:= 1;
                IF NOT have minus
                THEN
                    # not a range #
                    result[ element ] := from value
                ELSE
                    # have a range #
                    next;
                    INT to value = get number;
                    result[ element ] := RANGE( from value, to value )
                FI
            ELSE
                # should be a comma #
                next
            FI
        OD;

    # RESULT #
        result
    END; # parse range string #


# RESULT #
    parse range string
END; # TORANGE #


# converts a []INT to a comma separated string of the elements #
OP TOSTRING = ( []INT values )STRING:
BEGIN
    
    STRING result    := "";
    STRING separator := "";

    FOR pos FROM LWB values TO UPB values
    DO
        result +:= ( separator + whole( values[ pos ], 0 ) );
        separator := ","
    OD;

# RESULT #
    result
END; # TOSTRING #


test:(
    print( ( TOSTRING range expand( TORANGE "-6,-3--1,3-5,7-11,14,15,17-20" ), newline ) )
)

```

{{out}}

```txt

-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20

```




## AppleScript

{{Trans|JavaScript}}  (Functional ES5 version)

```AppleScript
-- Each comma-delimited string is mapped to a list of integers,
-- and these integer lists are concatenated together into a single list

-- expansion :: String -> [Int]
on expansion(strExpr)
    -- The string (between commas) is split on hyphens, 
    -- and this segmentation is rewritten to ranges or minus signs
    -- and evaluated to lists of integer values
    
    -- signedRange :: String -> [Int]
    script signedRange
        -- After the first character, numbers preceded by an
        -- empty string (resulting from splitting on hyphens)
        -- and interpreted as negative
        
        -- signedIntegerAppended:: [Int] -> String -> Int -> [Int] -> [Int]
        on signedIntegerAppended(lstAccumulator, strNum, iPosn, lst)
            if strNum ≠ "" then
                if iPosn > 1 then
                    if length of (item (iPosn - 1) of lst) > 0 then
                        set strSign to ""
                    else
                        set strSign to "-"
                    end if
                else
                    set strSign to "+"
                end if
                lstAccumulator & ((strSign & strNum) as integer)
            else
                lstAccumulator
            end if
        end signedIntegerAppended
        
        on |λ|(strHyphenated)
            tupleRange(foldl(signedIntegerAppended, {}, ¬
                splitOn("-", strHyphenated)))
        end |λ|
    end script
    
    concatMap(signedRange, splitOn(",", strExpr))
end expansion


-- TEST -----------------------------------------------------------------------
on run
    
    expansion("-6,-3--1,3-5,7-11,14,15,17-20")
    
    --> {-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20}
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    script append
        on |λ|(a, b)
            a & b
        end |λ|
    end script
    
    foldl(append, {}, map(f, xs))
end concatMap

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
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

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set xs to text items of strMain
    set my text item delimiters to dlm
    return xs
end splitOn

-- range :: (Int, Int) -> [Int]
on tupleRange(tuple)
    if tuple = {} then
        {}
    else if length of tuple > 1 then
        enumFromTo(item 1 of tuple, item 2 of tuple)
    else
        item 1 of tuple
    end if
end tupleRange
```

{{Out}}

```AppleScript
{-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20}
```



## AutoHotkey


```AutoHotkey
msgbox % expand("-6,-3--1,3-5,7-11,14,15,17-20")

expand( range ) {
    p := 0
    while p := RegExMatch(range, "\s*(-?\d++)(?:\s*-\s*(-?\d++))?", f, p+1+StrLen(f))
        loop % (f2 ? f2-f1 : 0) + 1
            ret .= "," (A_Index-1) + f1
    return SubStr(ret, 2)
}
```



## AWK



```awk
#!/usr/bin/awk -f
BEGIN {	FS=","; }

{	s="";
	for (i=1; i<=NF; i++) { expand($i); }
	print substr(s,2);
}

function expand(a) {
	idx = match(a,/[0-9]-/);
	if (idx==0) {
		s = s","a; 	
		return;
	}
	
	start= substr(a,1, idx)+0;
	stop = substr(a,idx+2)+0;
	for (m = start; m <= stop; m++) {
		s = s","m; 	
	}
	return;
} 
```



```txt

Usage: 
  echo -6,-3--1,3-5,7-11,14,15,17-20  | gawk -f ./range_expansion.awk 
  -6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20

```



## BBC BASIC


```bbcbasic
      PRINT FNrangeexpand("-6,-3--1,3-5,7-11,14,15,17-20")
      END
      
      DEF FNrangeexpand(r$)
      LOCAL i%, j%, k%, t$
      REPEAT
        i% = INSTR(r$, "-", i%+1)
        IF i% THEN
          j% = i%
          WHILE MID$(r$,j%-1,1)<>"," AND j%<>1
            j% -= 1
          ENDWHILE
          IF i%>j% IF MID$(r$,j%,i%-j%)<>STRING$(i%-j%," ") THEN
            t$ = ""
            FOR k% = VALMID$(r$,j%) TO VALMID$(r$,i%+1)-1
              t$ += STR$(k%) + ","
            NEXT
            r$ = LEFT$(r$,j%-1) + t$ + MID$(r$,i%+1)
            i% = j% + LEN(t$) + 2
          ENDIF
        ENDIF
      UNTIL i% = 0
      = r$
```

{{out}}

```txt

-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20

```



## Bracmat


```bracmat
  ( expandRanges
  =   a b L
    .     @( !arg
           :   (#(?a:?b)|#?a "-" #?b)
               (:?L|"," [%(expandRanges$!sjt:?L))
           )
        &   whl
          ' (   (!L:&!b|(!b,!L))
              : ?L
            & -1+!b:~<!a:?b
            )
        & !L
      |
  )
& out$(str$(expandRanges$"-6,-3--1,3-5,7-11,14,15,17-20"))

```

{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```



## C

Recursive descent parser.

```c>#include <stdio.h

#include <stdlib.h>
#include <ctype.h>

/* BNFesque
	rangelist := (range | number) [',' rangelist]
	range := number '-' number	*/

int get_list(const char *, char **);
int get_rnge(const char *, char **);

/* parser only parses; what to do with parsed items is up to
* the add_number and and_range functions */
void add_number(int x);
int add_range(int x, int y);

#define skip_space while(isspace(*s)) s++
#define get_number(x, s, e) (x = strtol(s, e, 10), *e != s)
int get_list(const char *s, char **e)
{
	int x;
	while (1) {
		skip_space;
		if (!get_rnge(s, e) && !get_number(x, s, e)) break;
		s = *e;

		skip_space;
		if ((*s) == '\0') { putchar('\n'); return 1; }
		if ((*s) == ',')  { s++; continue; }
		break;
	}
	*(const char **)e = s;
	printf("\nSyntax error at %s\n", s);
	return 0;
}

int get_rnge(const char *s, char **e)
{
	int x, y;
	char *ee;
	if (!get_number(x, s, &ee)) return 0;
	s = ee;

	skip_space;
	if (*s != '-') {
		*(const char **)e = s;
		return 0;
	}
	s++;
	if(!get_number(y, s, e)) return 0;
	return add_range(x, y);
}

void add_number(int x)
{
	printf("%d ", x);
}

int add_range(int x, int y)
{
	if (y <= x) return 0;
	while (x <= y) printf("%d ", x++);
	return 1;
}

int main()
{
	char *end;

	/* this is correct */
	if (get_list("-6,-3--1,3-5,7-11,14,15,17-20", &end)) puts("Ok");

	/* this is not.  note the subtle error: "-6 -3" is parsed
	 * as range(-6, 3), so synax error comes after that */
	get_list("-6 -3--1,3-5,7-11,14,15,17-20", &end);

	return 0;
}
```

{{out}}

```txt
-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20 
Ok
-6 -5 -4 -3 -2 -1 0 1 2 3 
Syntax error at --1,3-5,7-11,14,15,17-20
```


=={{header|C sharp|C#}}==
{{works with|C sharp|3.0}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

class Program
{
    static void Main(string[] args)
    {
        var rangeString = "-6,-3--1,3-5,7-11,14,15,17-20";
        var matches = Regex.Matches(rangeString, @"(?<f>-?\d+)-(?<s>-?\d+)|(-?\d+)");
        var values = new List<string>();

        foreach (var m in matches.OfType<Match>())
        {
            if (m.Groups[1].Success)
            {
                values.Add(m.Value);
                continue;
            }

            var start = Convert.ToInt32(m.Groups["f"].Value);
            var end = Convert.ToInt32(m.Groups["s"].Value) + 1;

            values.AddRange(Enumerable.Range(start, end - start).Select(v => v.ToString()));
        }

        Console.WriteLine(string.Join(", ", values));
    }
}
```


{{works with|C sharp|3.5+}} 

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace RangeExpansion {
  internal static class StringExtensions {
    internal static IEnumerable<int> ExpandRange(this string s) {
      return s.Split(',')
        .Select(rstr => {
          int start;
          if (int.TryParse(rstr, out start))
            return new {Start = start, End = start};
          var istr = new string(("+-".Any(_ => rstr[0] == _)
            ? rstr.Take(1).Concat(rstr.Skip(1).TakeWhile(char.IsDigit))
            : rstr.TakeWhile(char.IsDigit)
            ).ToArray());
          rstr = rstr.Substring(istr.Length + 1, (rstr.Length - istr.Length) - 1);
          return new {Start = int.Parse(istr), End = int.Parse(rstr)};
        }).SelectMany(_ => Enumerable.Range(_.Start, _.End - _.Start + 1));
    }
  }

  internal static class Program {
    private static void Main() {
      const string RANGE_STRING = "-6,-3--1,3-5,7-11,14,15,17-20";
      var values = RANGE_STRING.ExpandRange().ToList();
      var vstr = string.Join(", ", values.Select(_ => _.ToString()));
      Console.WriteLine(vstr);
    }
  }
}
```



## C++


```cpp>#include <iostream

#include <sstream>
#include <iterator>
#include <climits>
#include <deque>

// parse a list of numbers with ranges
//
// arguments:
//  is:  the stream to parse
//  out: the output iterator the parsed list is written to.
//
// returns true if the parse was successful. false otherwise
template<typename OutIter>
 bool parse_number_list_with_ranges(std::istream& is, OutIter out)
{
  int number;
  // the list always has to start with a number
  while (is >> number)
  {
    *out++ = number;

    char c;
    if (is >> c)
      switch(c)
      {
      case ',':
        continue;
      case '-':
        {
          int number2;
          if (is >> number2)
          {
            if (number2 < number)
              return false;
            while (number < number2)
              *out++ = ++number;
            char c2;
            if (is >> c2)
              if (c2 == ',')
                continue;
              else
                return false;
            else
              return is.eof();
          }
          else
            return false;
        }
      default:
        return is.eof();
      }
    else
      return is.eof();
  }
  // if we get here, something went wrong (otherwise we would have
  // returned from inside the loop)
  return false;
}

int main()
{
  std::istringstream example("-6,-3--1,3-5,7-11,14,15,17-20");
  std::deque<int> v;
  bool success = parse_number_list_with_ranges(example, std::back_inserter(v));
  if (success)
  {
    std::copy(v.begin(), v.end()-1,
              std::ostream_iterator<int>(std::cout, ","));
    std::cout << v.back() << "\n";
  }
  else
    std::cout << "an error occured.";
}
```

{{out}}
 -6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20


## Clojure

There is a split method in clojure.contrib, but I don't know if it is able to skip first character to so that <code>(split "-8--8") => (-8 -8)</code>.

```clojure
(defn split [s sep]
      (defn skipFirst [[x & xs :as s]]
	(cond (empty? s) [nil nil]
	      (= x sep)  [x xs]
	      true       [nil s]))
      (loop [lst '(), s s]
	 (if (empty? s) (reverse lst)
	     (let [[hd trunc] (skipFirst s)
	           [word news] (split-with #(not= % sep) trunc)
		   cWord (cons hd word)]   
		     (recur (cons (apply str cWord) lst) 
		      	  (apply str (rest news)))))))

(defn parseRange [[x & xs :as s]]
       (if (some #(= % \-) xs)
	   (let [[r0 r1] (split s \-)]
		(range (read-string r0) (inc (read-string r1))))
	   (list (read-string (str s))))))

(defn rangeexpand [s]
  (flatten (map parseRange (split s \,))))

> (rangeexpand "-6,-3--1,3-5,7-11,14,15,17-20")
(-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20)
```



## COBOL

{{works with|GNU Cobol|2.0}}

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. expand-range.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  comma-pos                           PIC 99 COMP VALUE 1.
01  dash-pos                            PIC 99 COMP.
01  end-num                             PIC S9(3).
01  Max-Part-Len                        CONSTANT 10.
01  num                                 PIC S9(3).
01  edited-num                          PIC -(3)9.
01  part                                PIC X(10).

01  part-flag                           PIC X.
    88 last-part                        VALUE "Y".

01  range-str                           PIC X(80).
01  Range-Str-Len                       CONSTANT 80.
01  start-pos                           PIC 99 COMP.
01  start-num                           PIC S9(3).

PROCEDURE DIVISION.
    ACCEPT range-str

    PERFORM WITH TEST AFTER UNTIL last-part
        UNSTRING range-str DELIMITED BY "," INTO part WITH POINTER comma-pos
        PERFORM check-if-last

        PERFORM find-range-dash
            
        IF dash-pos > Max-Part-Len
            PERFORM display-num
        ELSE
            PERFORM display-range
        END-IF
    END-PERFORM

    DISPLAY SPACES
    
    GOBACK
    .
check-if-last SECTION.
    IF comma-pos > Range-Str-Len
        SET last-part TO TRUE
    END-IF
    .
find-range-dash SECTION.
    IF part (1:1) <> "-"
        MOVE 1 TO start-pos
    ELSE
        MOVE 2 TO start-pos
    END-IF

    MOVE 1 TO dash-pos
    INSPECT part (start-pos:) TALLYING dash-pos FOR CHARACTERS BEFORE "-"
    COMPUTE dash-pos = dash-pos + start-pos - 1
    .
display-num SECTION.
    MOVE part TO edited-num
    CALL "display-edited-num" USING CONTENT part-flag, edited-num
    .
display-range SECTION.
    MOVE part (1:dash-pos - 1) TO start-num
    MOVE part (dash-pos + 1:) TO end-num

    PERFORM VARYING num FROM start-num BY 1 UNTIL num = end-num
        MOVE num TO edited-num
        CALL "display-edited-num" USING CONTENT "N", edited-num
    END-PERFORM

    MOVE end-num TO edited-num
    CALL "display-edited-num" USING CONTENT part-flag, edited-num
    .
END PROGRAM expand-range.


IDENTIFICATION DIVISION.
PROGRAM-ID. display-edited-num.

DATA DIVISION.
LINKAGE SECTION.
01  hide-comma-flag                     PIC X.
    88  hide-comma                      VALUE "Y".
01  edited-num                          PIC -(3)9.

PROCEDURE DIVISION USING hide-comma-flag, edited-num.
    DISPLAY FUNCTION TRIM(edited-num) NO ADVANCING
    IF NOT hide-comma
        DISPLAY ", " NO ADVANCING
    END-IF
    .
END PROGRAM display-edited-num.
```


Setup:

```txt
 
$ ./expand-range 
-6,-3--1,3-5,7-11,14,15,17-20

```


{{out}}

```txt

-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20

```



## Common Lisp


```lisp
(defun expand-ranges (string)
  (loop
     with prevnum = nil
     for idx = 0 then (1+ nextidx)
     for (number nextidx) = (multiple-value-list
                             (parse-integer string
                                            :start idx :junk-allowed t))
     append (cond
              (prevnum
               (prog1
                   (loop for i from prevnum to number
                      collect i)
                 (setf prevnum nil)))
              ((and (< nextidx (length string))
                    (char= (aref string nextidx) #\-))
               (setf prevnum number)
               nil)
              (t
               (list number)))
     while (< nextidx (length string))))

CL-USER> (expand-ranges "-6,-3--1,3-5,7-11,14,15,17-20")
(-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20)
```



## D


```d
import std.stdio, std.regex, std.conv, std.range, std.algorithm;

enum rangeEx = (string s) /*pure*/ => s.matchAll(`(-?\d+)-?(-?\d+)?,?`)
    .map!q{ a[1].to!int.iota(a[1 + !a[2].empty].to!int + 1) }.join;

void main() {
    "-6,-3--1,3-5,7-11,14,15,17-20".rangeEx.writeln;
}
```

{{out}}

```txt
[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
```



## DWScript


```pascal

function ExpandRanges(ranges : String) : array of Integer;
begin
   for var range in ranges.Split(',') do begin
      var separator = range.IndexOf('-', 2);
      if separator > 0 then begin
         for var i := range.Left(separator-1).ToInteger to range.Copy(separator+1).ToInteger do
            Result.Add(i);
      end else begin
         Result.Add(range.ToInteger)
      end;
   end;
end;

var expanded := ExpandRanges('-6,-3--1,3-5,7-11,14,15,17-20');
PrintLn(JSON.Stringify(expanded));

```

{{out}}

```txt
[-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]
```



## Dyalect


{{trans|Go}}


```dyalect
func main() {
    const input = "-6,-3--1,3-5,7-11,14,15,17-20"

    print("range: \(input)")
    var r = []
    var last = 0
    for part in input.split(',') {
        var i = part.sub(1).indexOf('-')
        if  i == -1 {
            var n = Integer(part)
            if r.len() > 0 {
                if last == n {
                    print("duplicate value: \(n)")
                    return
                } else if last > n {
                    print("values not ordered: \(last) > \(n)")
                    return
                }
            }
            r.add(n)
            last = n
        } else {
            var n1 = Integer(part.sub(0, i+1))
            var n2 = Integer(part.sub(i+2))
            if n2 < n1+2 {
                print("invalid range: \(part)")
                return
            }
            if r.len() > 0 {
                if last == n1 {
                    print("duplicate value: \(n1)")
                    return
                } else if last > n1 {
                    print("values not ordered: \(last) > \(n1)")
                    return
                }
            }
            for i in n1..n2 {
                r.add(i)
            }
            last = n2
        }
    }

    print("expanded: \(r)")
}

main()
```


{{out}}


```txt
range: -6,-3--1,3-5,7-11,14,15,17-20
expanded: [-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
```



## EchoLisp


```scheme

;; parsing [spaces][-]digit(s)-[-]digit(s)[spaces]
(define R (make-regexp "^ *(\-?\\d+)\-(\-?\\d+) *$" ))

;; the native (range a b) is [a ... b[
;; (range+ a b) is [a ... b]
(define (range+ a b)
	(if (< a b) (range a (1+ b))
	(if (> a b) (range a (1- b) -1)
	(list a))))
	
;; in : string : "number" or "number-number"
;; out : a range = list of integer(s)
(define (do-range str) 
(define from-to (regexp-exec R str)) ;; "1-3" --> ("1" "3")
(if from-to 
    (range+ (string->number (first from-to)) (string->number (second from-to)))
    (list (string->number str))))
    
(define (ranges str)
    (apply append (map do-range (string-split str ","))))
	

(define task "-6,-3--1,3-5,7-11,14,15,17-20")
(ranges task)
    → (-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20)

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule RC do
  def expansion(range) do
    Enum.flat_map(String.split(range, ","), fn part ->
      case Regex.scan(~r/^(-?\d+)-(-?\d+)$/, part) do
        [[_,a,b]] -> Enum.to_list(String.to_integer(a) .. String.to_integer(b))
        [] -> [String.to_integer(part)]
      end
    end)
  end
end

IO.inspect RC.expansion("-6,-3--1,3-5,7-11,14,15,17-20")
```


{{out}}

```txt

[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]

```



## Erlang


```Erlang

-module( range ).

-export( [expansion/1, task/0] ).

expansion( String ) ->
        lists:flatten( [expansion_individual(io_lib:fread("~d", X)) || X <- string:tokens(String, ",")] ).

task() ->
    io:fwrite( "~p~n", [expansion("-6,-3--1,3-5,7-11,14,15,17-20")] ).



expansion_individual( {ok, [N], []} ) -> N;
expansion_individual( {ok, [Start], "-" ++ Stop_string} ) -> lists:seq( Start, erlang:list_to_integer(Stop_string) ).

```


{{out}}

```txt

34> range:task().
[-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]

```


=={{header|F_Sharp|F#}}==

```fsharp
open System.Text.RegularExpressions

// simplify regex matching with an active pattern
let (|Regexp|_|) pattern txt =
    match Regex.Match(txt, pattern) with
    | m when m.Success -> [for g in m.Groups -> g.Value] |> List.tail |> Some
    | _                -> None

// Parse and expand a single range description.
// string -> int list
let parseRange r =
  match r with
  | Regexp @"^(-?\d+)-(-?\d+)$" [first; last] -> [int first..int last]
  | Regexp @"^(-?\d+)$"         [single]      -> [int single]
  | _ -> failwithf "illegal range format: %s" r
  

let expand (desc:string) =
  desc.Split(',')
  |> List.ofArray
  |> List.collect parseRange

printfn "%A" (expand "-6,-3--1,3-5,7-11,14,15,17-20")
```

{{out}}

```txt
[-6; -3; -2; -1; 3; 4; 5; 7; 8; 9; 10; 11; 14; 15; 17; 18; 19; 20]
```



## Factor

<code>R/ (?<=\d)-/ re-split</code> says: ''split only on hyphens immediately preceded by a digit.''

```factor
USING: kernel math.parser math.ranges prettyprint regexp
sequences sequences.extras splitting ;

: expand ( str -- seq )
    "," split [
        R/ (?<=\d)-/ re-split [ string>number ] map
        dup length 2 = [ first2 [a,b] ] when
    ] map-concat ;

"-6,-3--1,3-5,7-11,14,15,17-20" expand .
```

{{out}}

```txt

{ -6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20 }

```



## Forth


```forth>: 
snumber ( str len -- 'str 'len n )
  0. 2swap
  over c@ [char] - = if
    1 /string
    >number 2swap drop
    negate
  else
    >number 2swap drop
  then ;

: expand ( str len -- )
  begin dup while
    >snumber >r
    dup if over c@ [char] - = if
      1 /string
      >snumber r> over >r
      do i . loop
    then then
    dup if over c@ [char] , = if
      1 /string
    then then
    r> .
  repeat 2drop ;

s" -6,-3--1,3-5,7-11,14,15,17-20" expand
```



## Fortran

In order to save on the annoyance of having to declare the type of function ERANGE in each routine that invokes it (just one, here), the F90 MODULE protocol is convenient. F90 also allows for routines to contain service routines that share the context without massive parameter lists or shared COMMON areas, though alternatively, the shared variables could be inside the MODULE which would contain the service routines as separate routines also inside the module. But for this, one should not share variables having short names such as <code>I</code>. Otherwise, this would be F77 style except for the usage of the <code>I0</code> format code in subroutine SPLOT. If that were unavailable, code <code>I12</code> could be used and leading spaces would have to be skipped instead. The DO WHILE loop with its EXIT and labels would have to be less decorative for F77.

The standard problem of "How long is a piece of string?" applies yet again. Arrays and character variables have to be declared with some fixed size, and the usual approach is "surely big enough". The test input is not a problem, because the caller can use the needed size and pass that to ERANGE as a parameter, whatever its size. But function ERANGE's result must have some pre-declared size, and for this example, CHARACTER*200 will do. Via a great deal of blather, varying-size character variables can be defined and used in F90/95, and F2003 has standardised a method of doing this, whereby whenever something like <code>ALINE = ALINE // "stuff"</code> is executed, variable ALINE is reallocated with additional space. This involves copying the old content to the newly allocated larger storage area, so doing a lot of this would be bad snowballing.

The method is to grind through the input string expecting to find a ''number'' or a ''number - number'' pair (signed numbers allowed), followed by a comma if more is to follow. The state of the scan is represented by position within the scanning code rather than mess with state variables, so it is convenient to have service routines for the resulting repetition of basic actions. No checks are made for improper input, for instance a string ending with a comma. Those of a delicate disposition may be troubled by functions that don't just return a result but also mess with their parameter and change their environment. Here, the results from the functions assist with the flow of control through the scan, it is the side effects that manipulate the data.

A frustrating problem with many modern computer languages is the absence of a "shortcut" evaluation praxis for logical expressions; in Fortran's case the modern standard is that there is no standard. So a test <code>I<=LEN(TEXT) & TEXT(I:I)''etc.''</code> can't be relied upon to dodge out-of-bounds errors, and a flabby two-statement sequence is required instead. Similarly, few Fortran compilers allow for a function being evaluated via a WRITE statement to itself succeed in using a WRITE statement internally, though some do if one usage is free-format and the other formatted. If necessary, subroutine SPLOT could be re-written to convert an integer to a digit string without a WRITE statement, even for negative integers. And some compilers have difficulty with the use of the function name as a variable within the function so that it is safest to develop the result in an ordinary variable and then remember to assign its value to the function name just before exit. 

A single number is made internally into a two-number range sequence, which values are used as the bounds for a DO-loop to generate the numbers for output. Despite the '''The range syntax is to be used only for, and for every range that expands to more than two values''', I see no reason for this restriction (say because otherwise some fancy method would be stymied, except I can think of no such fancier method) and I have no desire to interpose some sort of error message, a tedious business that requires a wider interface between a routine and its caller. Similarly, if a range of 40-30 were to appear, why not take it at face value? 
```Fortran
      MODULE HOMEONTHERANGE
       CONTAINS	!The key function.
        CHARACTER*200 FUNCTION ERANGE(TEXT)	!Expands integer ranges in a list.
Can't return a character value of variable size.
         CHARACTER*(*) TEXT	!The list on input.
         CHARACTER*200 ALINE	!Scratchpad for output.
         INTEGER N,N1,N2	!Numbers in a range.
         INTEGER I,I1		!Steppers.
          ALINE = ""		!Scrub the scratchpad.
          L = 0			!No text has been placed.
          I = 1			!Start at the start.
          CALL FORASIGN		!Find something to look at.
Chug through another number or number - number range.
        R:DO WHILE(EATINT(N1))	!If I can grab a first number, a term has begun.
            N2 = N1			!Make the far end the same.
            IF (PASSBY("-")) CALL EATINT(N2)	!A hyphen here is not a minus sign.
            IF (L.GT.0) CALL EMIT(",")		!Another, after what went before?
            DO N = N1,N2,SIGN(+1,N2 - N1)	!Step through the range, possibly backwards.
              CALL SPLOT(N)				!Roll a number.
              IF (N.NE.N2) CALL EMIT(",")		!Perhaps another follows.
            END DO				!On to the next number.
            IF (.NOT.PASSBY(",")) EXIT R	!More to come?
          END DO R		!So much for a range.
Completed the scan. Just return the result.
          ERANGE = ALINE(1:L)	!Present the result. Fiddling ERANGE is bungled by some compilers.
         CONTAINS	!Some assistants for the scan to save on repetition and show intent.
          SUBROUTINE FORASIGN	!Look for one.
    1       IF (I.LE.LEN(TEXT)) THEN	!After a thingy,
              IF (TEXT(I:I).LE." ") THEN	!There may follow spaces.
                I = I + 1				!So,
                GO TO 1					!Speed past any.
              END IF			!So that the caller can see
            END IF			!Whatever substantive character follows.
          END SUBROUTINE FORASIGN	!Simple enough.

          LOGICAL FUNCTION PASSBY(C)	!Advances the scan if a certain character is seen.
Could consider or ignore case for letters, but this is really for single symbols.
           CHARACTER*1 C	!The character.
            PASSBY = .FALSE.	!Pessimism.
            IF (I.LE.LEN(TEXT)) THEN	!Can't rely on I.LE.LEN(TEXT) .AND. TEXT(I:I)...
              IF (TEXT(I:I).EQ.C) THEN	!Curse possible full evaluation.
                PASSBY = .TRUE.		!Righto, C is seen.
                I = I + 1		!So advance the scan.
                CALL FORASIGN		!And see what follows.
              END IF		!So much for a match.
            END IF		!If there is something to be uinspected.
          END FUNCTION PASSBY	!Can't rely on testing PASSBY within PASSBY either.

          LOGICAL FUNCTION EATINT(N)	!Convert text into an integer.
           INTEGER N	!The value to be ascertained.
           INTEGER D	!A digit.
           LOGICAL NEG	!In case of a minus sign.
            EATINT = .FALSE.	!Pessimism.
            IF (I.GT.LEN(TEXT)) RETURN	!Anything to look at?
            N = 0			!Scrub to start with.
            IF (PASSBY("+")) THEN	!A plus sign here can be ignored.
              NEG = .FALSE.		!So, there's no minus sign.
             ELSE			!And if there wasn't a plus,
              NEG = PASSBY("-")		!A hyphen here is a minus sign.
            END IF			!One way or another, NEG is initialised.
            IF (I.GT.LEN(TEXT)) RETURN	!Nothing further! We wuz misled!
Chug through digits. Can develop -2147483648, thanks to the workings of two's complement.
   10       D = ICHAR(TEXT(I:I)) - ICHAR("0")	!Hope for a digit.
            IF (0.LE.D .AND. D.LE.9) THEN	!Is it one?
              N = N*10 + D			!Yes! Assimilate it, negatively.
              I = I + 1				!Advance one.
              IF (I.LE.LEN(TEXT)) GO TO 10	!And see what comes next.
            END IF			!So much for a sequence of digits.
            IF (NEG) N = -N		!Apply the minus sign.
            EATINT = .TRUE.		!Should really check for at least one digit.
            CALL FORASIGN		!Ram into whatever follows.
          END FUNCTION EATINT	!Integers are easy. Could check for no digits seen.

          SUBROUTINE EMIT(C)	!Rolls forth one character.
           CHARACTER*1 C	!The character.
            L = L + 1		!Advance the finger.
            IF (L.GT.LEN(ALINE)) STOP "Ran out of ALINE!"	!Maybe not.
            ALINE(L:L) = C	!And place the character.
          END SUBROUTINE EMIT	!That was simple.

          SUBROUTINE SPLOT(N)	!Rolls forth a signed number.
           INTEGER N		!The number.
           CHARACTER*12 FIELD	!Sufficient for 32-bit integers.
           INTEGER I		!A stepper.
            WRITE (FIELD,"(I0)") N	!Roll the number, with trailing spaces.
            DO I = 1,12		!Now transfer the ALINE of the number.
              IF (FIELD(I:I).LE." ") EXIT	!Up to the first space.
              CALL EMIT(FIELD(I:I))	!One by one.
            END DO		!On to the end.
          END SUBROUTINE SPLOT	!Not so difficult either.
        END FUNCTION ERANGE	!A bit tricky.
      END MODULE HOMEONTHERANGE

      PROGRAM POKE
      USE HOMEONTHERANGE
      CHARACTER*(200) SOME
      SOME = "-6,-3--1,3-5,7-11,14,15,17-20"
      SOME = ERANGE(SOME)
      WRITE (6,*) SOME	!If ERANGE(SOME) then the function usually can't write output also.
      END
```


Output: 
```txt
 -6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```


A check by using -2147483648 showed that EATINT develops the correct value even if N is built positively. Adding 8 to 2147483640 VIA <code>N = N*10 + D</code> indeed produces -2147483648 (and NEG was '''t'''), but the <code>IF (NEG) N = -N</code> still results in -2147483648 because of the working of (flip all bits and add one): invert(-2147483648) = 2147483647, and, adding one to that produces -2147483648.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub split (s As Const String, sepList As Const String, result() As String)
  If s = "" OrElse sepList = "" Then 
     Redim result(0)
     result(0) = s
     Return
  End If
  Dim As Integer i, j, count = 0, empty = 0, length
  Dim As Integer position(Len(s) + 1)
  position(0) = 0
 
  For i = 0 To len(s) - 1
    For j = 0 to Len(sepList) - 1
      If s[i] = sepList[j] Then 
        count += 1
        position(count) = i + 1       
      End If
    Next j
  Next i
 
  Redim result(count)
  If count  = 0 Then
    result(0) = s
    Return
  End If
 
  position(count + 1) = len(s) + 1
 
  For i = 1 To count + 1  
    length = position(i) - position(i - 1) - 1 
    result(i - 1) = Mid(s, position(i - 1) + 1, length)
  Next
End Sub

Function expandRange(s As Const String) As String
  If s = "" Then Return ""
  Dim b() As String
  Dim c() As String 
  Dim result As String = "" 
  Dim As Integer start = 0, finish = 0, length
  split s, ",", b()
  For i As Integer = LBound(b) To UBound(b)
    split b(i), "-", c()
    length = UBound(c) - LBound(c) + 1
    If length = 1 Then 
      start  = ValLng(c(LBound(c)))
      finish = start
    ElseIf length = 2 Then
       If Left(b(i), 1) = "-" Then
         start  = -ValLng(c(UBound(c)))
         finish = start
       Else
         start  = ValLng(c(LBound(c)))
         finish = ValLng(c(UBound(c)))
       End If
    ElseIf length = 3 Then
      start  = -ValLng(c(LBound(c) + 1))
      finish = ValLng(c(UBound(c))) 
    Else 
      start  = -ValLng(c(LBound(c) + 1))
      finish = -ValLng(c(UBound(c)))
    End If
    For j As Integer = start To finish
      result += Str(j) + ", "
    Next j
  Next i
  Return Left(result, Len(result) - 2) '' get rid of final ", "
End Function

Dim s As String = "-6,-3--1,3-5,7-11,14,15,17-20"
Print expandRange(s)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20

```



## Go

A version rather strict with input

```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

const input = "-6,-3--1,3-5,7-11,14,15,17-20"

func main() {
    fmt.Println("range:", input)
    var r []int
    var last int
    for _, part := range strings.Split(input, ",") {
        if i := strings.Index(part[1:], "-"); i == -1 {
            n, err := strconv.Atoi(part)
            if err != nil {
                fmt.Println(err)
                return
            }
            if len(r) > 0 {
                if last == n {
                    fmt.Println("duplicate value:", n)
                    return
                } else if last > n {
                    fmt.Println("values not ordered:", last, ">", n)
                    return
                }
            }
            r = append(r, n)
            last = n
        } else {
            n1, err := strconv.Atoi(part[:i+1])
            if err != nil {
                fmt.Println(err)
                return
            }
            n2, err := strconv.Atoi(part[i+2:])
            if err != nil {
                fmt.Println(err)
                return
            }
            if n2 < n1+2 {
                fmt.Println("invalid range:", part)
                return
            }
            if len(r) > 0 {
                if last == n1 {
                    fmt.Println("duplicate value:", n1)
                    return
                } else if last > n1 {
                    fmt.Println("values not ordered:", last, ">", n1)
                    return
                }
            }
            for i = n1; i <= n2; i++ {
                r = append(r, i)
            }
            last = n2
        }
    }
    fmt.Println("expanded:", r)
}
```



## Groovy

Ad Hoc Solution:
# translate the task's range syntax into Groovy range syntax
# wrap with list delimiters
# evaluate the script expression
# flatten the nested lists
# express as a string
# unwrap the list delimiters

```groovy
def expandRanges = { compressed ->
    Eval.me('['+compressed.replaceAll(~/(\d)-/, '$1..')+']').flatten().toString()[1..-2]
}
```

Test:

```groovy
def s = '-6,-3--1,3-5,7-11,14,15,17-20'
println (expandRanges(s))
```

{{out}}

```txt
-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20
```



## Haskell

Given either of the below implementations of <code>expandRange</code>:

```haskell>
 expandRange "-6,-3--1,3-5,7-11,14,15,17-20"
[-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]
```


### With conventional list processing


```haskell
expandRange :: String -> [Int]
expandRange = concatMap f . split ','
  where f str@(c : cs) | '-' `elem` cs = [read (c : a) .. read b]
                       | otherwise     = [read str]
            where (a, _ : b) = break (== '-') cs

split :: Eq a => a -> [a] -> [[a]]
split delim [] = []
split delim l = a : split delim (dropWhile (== delim) b)
  where (a, b) = break (== delim) l
```


### With a parser


```haskell
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative (Applicative((<*>), (*>)), (<$>))
import Text.Parsec

expandRange :: String -> Maybe [Int]
expandRange = either (const Nothing) Just . parse rangeParser ""

rangeParser
  :: (Enum a, Read a, Stream s m Char)
  => ParsecT s u m [a]
rangeParser = concat <$> (item `sepBy` char ',')
  where
    item = do
      n1 <- num
      n2 <- option n1 (char '-' *> num)
      return [n1 .. n2]
    num = read `dot` (++) <$> option "" (string "-") <*> many1 digit
    dot = (.) . (.)

main :: IO ()
main = print $ expandRange "-6,-3--1,3-5,7-11,14,15,17-20"
```

{{Out}}

```txt
Just [-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
s := "-6,-3--1,3-5,7-11,14,15,17-20"
write("Input string      := ",s)
write("Expanded list   := ", list2string(range_expand(s)) | "FAILED")
end

procedure range_expand(s)          #: return list of integers extracted from an ordered string representation
local R,low,high
R := []

s ? until pos(0) do {
   put(R,low := integer(tab(upto(',-')|0))| fail)           # get lower bound
   if ="-" || (high := integer(tab(find(",")|0))|fail) then
      until low = high do put(R,low +:= 1)                  # find range
   =","
   }
return R
end

procedure list2string(L)        #: helper function to convert a list to a string
local s

   every (s := "[ ") ||:= !L || " "
   return s || "]"
end
```

{{out}}

```txt
Input string      := -6,-3--1,3-5,7-11,14,15,17-20
Expanded list   := [ -6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20 ]
```



## J


```j
require'strings'
thru=: <. + i.@(+*)@-~
num=: _&".
normaliz=: rplc&(',-';',_';'--';'-_')@,~&','
subranges=:<@(thru/)@(num;._2)@,&'-';._1
rngexp=: ;@subranges@normaliz
```

{{out|Example}}

```j
   rngexp '-6,-3--1,3-5,7-11,14,15,17-20'
_6 _3 _2 _1 3 4 5 7 8 9 10 11 14 15 17 18 19 20
```


Notes:

thru: given two integers (left: start of range, right: end of range) return the corresponding sequence of adjacent integers

num: given the string representation of a number, returns the number

normaliz: given the task required string representing a sequence of ranges, create a fresh copy with fewer micro ambiguities:  All subranges are preceded by a comma.  Negative numbers use a different character ('_') than the continuous range character ('-').

subranges: given the result of normaliz, return a sequence of boxes (one box for each comma). Each box contains the subrange which is described after its comma.

As an aside, note also that thru/ is an identity function when applied to a single number.  This is because (verb/) inserts the verb between each number (or each item in a list), and this is an identity function on a single number, regardless of any definition of the verb. Fortunately, this is consistent with the definition of thru (and is also consistent for any combining verb which has an identity element). Note that this is very similar to the self-justifying [[wp:Fold_(higher-order_function)|fold]] definition (specifically a right fold, because of J's structure), but fold can be considerably harder to reason about because it explicitly avoids the concept of identity for combining functions. You should maybe consider using a loop in contexts where this is an issue.

Also note that current versions of J no longer need <code>require'strings'</code> as those routines are included by default. (But let's leave this in place because current versions of J still have some catching up to do in some areas, such as lab support.)


## Java


```java
import java.util.*;

class RangeExpander implements Iterator<Integer>, Iterable<Integer> {

    private static final Pattern TOKEN_PATTERN = Pattern.compile("([+-]?\\d+)-([+-]?\\d+)");

    private final Iterator<String> tokensIterator;

    private boolean inRange;
    private int upperRangeEndpoint;
    private int nextRangeValue;

    public RangeExpander(String range) {
        String[] tokens = range.split("\\s*,\\s*");
        this.tokensIterator = Arrays.asList(tokens).iterator();
    }

    @Override
    public boolean hasNext() {
        return hasNextRangeValue() || this.tokensIterator.hasNext();
    }

    private boolean hasNextRangeValue() {
        return this.inRange && this.nextRangeValue <= this.upperRangeEndpoint;
    }

    @Override
    public Integer next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }

        if (hasNextRangeValue()) {
            return this.nextRangeValue++;
        }

        String token = this.tokensIterator.next();

        Matcher matcher = TOKEN_PATTERN.matcher(token);
        if (matcher.find()) {
            this.inRange = true;
            this.upperRangeEndpoint = Integer.valueOf(matcher.group(2));
            this.nextRangeValue = Integer.valueOf(matcher.group(1));
            return this.nextRangeValue++;
        }

        this.inRange = false;
        return Integer.valueOf(token);
    }

    @Override
    public Iterator<Integer> iterator() {
        return this;
    }

}

class RangeExpanderTest {
    public static void main(String[] args) {
        RangeExpander re = new RangeExpander("-6,-3--1,3-5,7-11,14,15,17-20");
        for (int i : re) {
            System.out.print(i + " ");
        }
    }
}
```



## JavaScript


===Imperative (Spidermonkey)===


```javascript
#!/usr/bin/env js

function main() {
    print(rangeExpand('-6,-3--1,3-5,7-11,14,15,17-20'));
}

function rangeExpand(rangeExpr) {
    
    function getFactors(term) {
        var matches = term.match(/(-?[0-9]+)-(-?[0-9]+)/);
        if (!matches) return {first:Number(term)};
        return {first:Number(matches[1]), last:Number(matches[2])};
    }
    
    function expandTerm(term) {
        var factors = getFactors(term);
        if (factors.length < 2) return [factors.first];
        var range = [];
        for (var n = factors.first; n <= factors.last;  n++) {
            range.push(n);
        }
        return range;
    }
    
    var result = [];
    var terms = rangeExpr.split(/,/);
    for (var t in terms) {
        result = result.concat(expandTerm(terms[t]));
    }
    
    return result;
}

main();

```


{{out}}
 -6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20


### Functional



### =ES5=



```JavaScript
(function (strTest) {
    'use strict';

    // s -> [n]
    function expansion(strExpr) {

        // concat map yields flattened output list
        return [].concat.apply([], strExpr.split(',')
            .map(function (x) {
                return x.split('-')
                    .reduce(function (a, s, i, l) {

                        // negative (after item 0) if preceded by an empty string
                        // (i.e. a hyphen-split artefact, otherwise ignored)
                        return s.length ? i ? a.concat(
                            parseInt(l[i - 1].length ? s :
                                '-' + s, 10)
                        ) : [+s] : a;
                    }, []);

                // two-number lists are interpreted as ranges
            })
            .map(function (r) {
                return r.length > 1 ? range.apply(null, r) : r;
            }));
    }


    // [m..n]
    function range(m, n) {
        return Array.apply(null, Array(n - m + 1))
            .map(function (x, i) {
                return m + i;
            });
    }

    return expansion(strTest);

})('-6,-3--1,3-5,7-11,14,15,17-20');
```


{{Out}}


```JavaScript
[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
```




### =ES6=



```JavaScript
(strTest => {
    
    // expansion :: String -> [Int]
    let expansion = strExpr =>

        // concat map yields flattened output list
        [].concat.apply([], strExpr.split(',')
            .map(x => x.split('-')
                .reduce((a, s, i, l) =>

                    // negative (after item 0) if preceded by an empty string
                    // (i.e. a hyphen-split artefact, otherwise ignored)
                    s.length ? i ? a.concat(
                        parseInt(l[i - 1].length ? s :
                            '-' + s, 10)
                    ) : [+s] : a, [])

                // two-number lists are interpreted as ranges
            )
            .map(r => r.length > 1 ? range.apply(null, r) : r)),



        // range :: Int -> Int -> Maybe Int -> [Int]
        range = (m, n, step) => {
            let d = (step || 1) * (n >= m ? 1 : -1);

            return Array.from({
                length: Math.floor((n - m) / d) + 1
            }, (_, i) => m + (i * d));
        };



    return expansion(strTest);

})('-6,-3--1,3-5,7-11,14,15,17-20');
```


{{Out}}

```JavaScript
[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
```



## jq

{{works with|jq|with regex support}}

```jq
def expand_range:
  def number: "-?[0-9]+";
  def expand: [range(.[0]; .[1] + 1)];
  
  split(",")
  | reduce .[] as $r
      ( []; . +
            ($r | if test("^\(number)$") then [tonumber]
                  else sub( "(?<x>\(number))-(?<y>\(number))"; "\(.x):\(.y)")
                  | split(":") | map(tonumber) | expand
	          end));
```

'''Example''':

```jq
"-6,-3--1,3-5,7-11,14,15,17-20" | expand_range
```

{{out}}$ jq -c -n -f Range_expansion.jq
[-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]


## Jsish

{{trans|Javascript}}

```javascript
#!/usr/bin/env jsish
"use strict";

/* Range expansion, in Jsish */ 
function rangeExpand(rangeExpr) {
 
    function getFactors(term) {
        var matches = term.match(/(-?[0-9]+)-(-?[0-9]+)/);
        if (!matches) return {first:Number(term)};
        return {first:Number(matches[1]), last:Number(matches[2])};
    }
 
    function expandTerm(term) {
        var factors = getFactors(term);
        if (factors.length < 2) return [factors.first];
        var range = [];
        for (var n = factors.first; n <= factors.last;  n++) {
            range.push(n);
        }
        return range;
    }

    var result = [];
    var terms = rangeExpr.split(",");
    for (var t in terms) {
        result = result.concat(expandTerm(terms[t]));
    }

    return result;
}

if (Interp.conf('unitTest')) {
;    rangeExpand('-6,-3--1,3-5,7-11,14,15,17-20');
}

/*
=!EXPECTSTART!=
rangeExpand('-6,-3--1,3-5,7-11,14,15,17-20') ==> [ -6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20 ]
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U rangeExpansion.jsi
rangeExpand('-6,-3--1,3-5,7-11,14,15,17-20') ==> [ -6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20 ]
```



## Julia


```Julia
slurp(s) = readcsv(IOBuffer(s))

conv(s)= colon(map(x->parse(Int,x),match(r"^(-?\d+)-(-?\d+)$", s).captures)...)

expand(s) = mapreduce(x -> isa(x,Number)? Int(x) : conv(x), vcat, slurp(s))
```

{{out}}

```txt
julia> show(expand("-6,-3--1,3-5,7-11,14,15,17-20"))
[-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]
```



## K


```k
grp : {1_'(&x=*x)_ x:",",x}
pos : {:[3=l:#p:&"-"=x;0,p@1;2=l;p;0=*p;,0;0,p]}
conv: 0${(x;1_ y)}/'{(pos x)_ x}'
expd: {,/@[x;&2=#:'x;{(*x)+!1+,/-':x}]}
rnge: {expd@conv grp x}
```

{{out|Example}}

```k
  rnge "-6,-3--1,3-5,7-11,14,15,17-20"
-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20
```



## Kotlin


```scala
// version 1.0.6

fun expandRange(s: String): MutableList<Int> {
    val list = mutableListOf<Int>()    
    val items = s.split(',')
    var first: Int
    var last:  Int
    for (item in items) {
        val count = item.count { it == '-' }
        if (count == 0 || (count == 1 && item[0] == '-'))
            list.add(item.toInt())
        else {
            val items2 = item.split('-')
            if (count == 1) {
                first = items2[0].toInt()
                last  = items2[1].toInt()            
            }
            else if (count == 2) {
                first = items2[1].toInt() * -1
                last  = items2[2].toInt()
            }
            else {
                first = items2[1].toInt() * -1
                last  = items2[3].toInt() * -1
            }
            for (i in first..last) list.add(i) 
        }
    }
    return list
}  

fun main(args: Array<String>) {
    val s = "-6,-3--1,3-5,7-11,14,15,17-20"
    println(expandRange(s))
}
```


{{out}}

```txt

[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]

```



## Lasso


```lasso
define range_expand(expression::string) => {
    local(parts) = regexp(`^(-?\d+)-(-?\d+)$`)
    
    return (
        with elm in #expression->split(`,`)
        let isRange = #parts->setInput(#elm)&matches
        select #isRange
            ? (integer(#parts->matchString(1)) to integer(#parts->matchString(2)))->asString
            | integer(#elm)->asString
    )->join(', ')
}

range_expand(`-6,-3--1,3-5,7-11,14,15,17-20`)
```


{{out}}

```txt
-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20
```




## Liberty BASIC


```lb
print ExpandRange$( "-6,-3--1,3-5,7-11,14,15,17-20")
end

function ExpandRange$( compressed$)
    for i = 1 to ItemCount( compressed$, ",")
        item$ = word$( compressed$, i, ",")
        dash  = instr( item$, "-", 2) 'dash that is not the first character, is a separator
        if dash then
            for k = val( left$( item$, dash - 1)) to val( mid$( item$, dash + 1))
                ExpandRange$ = ExpandRange$ + str$( k) + ","
            next k
        else
            ExpandRange$ = ExpandRange$ + item$ + ","
        end if
    next i
    ExpandRange$ = left$( ExpandRange$, len( ExpandRange$) - 1)
end function

function ItemCount( list$, separator$)
    while word$(list$, ItemCount + 1, separator$) <> ""
        ItemCount = ItemCount + 1
    wend
end function
```

{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```



## Lingo


```lingo
-- Note: currently does not support extra white space in input string
on expandRange (str)
  res = ""
  _player.itemDelimiter = ","
  cnt = str.item.count
  repeat with i = 1 to cnt
    part = str.item[i]
    pos = offset("-", part.char[2..part.length])
    if pos>0 then
      a = integer(part.char[1..pos])
      b = integer(part.char[pos+2..part.length])
      repeat with j = a to b
        put j&"," after res
      end repeat
    else
      put part&"," after res
    end if
  end repeat
  delete the last char of res
  return res
end
```


```lingo
put expandRange("-6,-3--1,3-5,7-11,14,15,17-20")
-- "-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20"
```



## LiveCode


```LiveCode
function range beginning ending stepping
    local tRange, tBegin, tEnd, tstep
    if stepping is empty or stepping is 0 then
        put 1 into tstep
    else
        put abs(stepping) into tstep
    end if
    
    if ending is empty or isNumber(ending) is not true then
        put 0 into tEnd
    else
        put ending into tEnd
    end if
    
    if beginning is empty or isNumber(beginning) is not true then
        put 0 into tBegin
    else
        put beginning into tBegin
    end if
    
    repeat with r = tBegin to tEnd step tstep
        put space & r after tRange
    end repeat
    return word 1 to -1 of tRange
end range

function expandRange rangeExpr
    put rangeExpr into tRange
    split tRange by comma
    repeat with n = 1 to the number of elements of tRange
        if matchText(tRange[n],"^(\-*\d+)\-(\-*\d+)",beginning, ending) then
            put range(beginning, ending, 1) & space after z
        else
            put tRange[n] & space after z
        end if
    end repeat
    return z
end expandRange
```


Test

```LiveCode
expandRange("-6,-3--1,3-5,7-11,14,15,17-20")
-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20 
```



## Lua



```lua
function range(i, j)
    local t = {}
    for n = i, j, i<j and 1 or -1 do
        t[#t+1] = n
    end
    return t
end

function expand_ranges(rspec)
    local ptn = "([-+]?%d+)%s?-%s?([-+]?%d+)"
    local t = {}

    for v in string.gmatch(rspec, '[^,]+') do
        local s, e = v:match(ptn)

        if s == nil then
            t[#t+1] = tonumber(v)
        else
            for i, n in ipairs(range(tonumber(s), tonumber(e))) do
                t[#t+1] = n
            end
        end
    end
    return t
end

local ranges = "-6,-3--1,3-5,7-11,14,15,17-20"
print(table.concat(expand_ranges(ranges), ', '))
```


Due to the way Lua's <code>tonumber</code> function works and the way the string pattern to parse ranges is written, whitespace is allowed around commas and the dash separating the range start and end (but not between the plus/minus sign and the number).

{{out}}

```txt

    -6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20

```



## Maple


```Maple

ExpandRanges := proc( s :: string )
  uses  StringTools;
  local DoOne := proc( input )
    uses StringTools;
    local lo, hi, pos;
    if IsDigit( input ) or input[ 1 ] = "-"
     and IsDigit( input[ 2 .. -1 ] ) then
      parse( input )
    else
      pos := Search( "--", input );
      if pos > 0 then
        lo := input[ 1 .. pos - 1 ];
        hi := input[ 1 + pos .. -1 ];
      elif input[ 1 ] = "-" then
        pos := FirstFromLeft( "-", input[ 2 .. -1 ] );
        if pos = 0 then
          lo := input;
          hi := lo
        else
          lo := input[ 1 .. pos ];
          hi := input[ 2 + pos .. -1 ];
        end if;
      else
        pos := FirstFromLeft( "-", input );
        if pos = 0 then
          error "incorrect syntax"
        end if;
        lo := input[ 1 .. pos - 1 ];
        hi := input[ 1 + pos .. -1 ];
      end if;
      lo := parse( lo );
      hi := parse( hi );
      seq( lo .. hi )
    end if
  end proc:
  map( DoOne, map( Trim, Split( s, "," ) ) )
end proc:

```

Running this on the example input we get the following.

```Maple

> rng := "-6,-3--1,3-5,7-11,14,15,17-20":
> ExpandRanges( rng );
   [-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]

```

Here is an additional example which my first attempt got wrong.

```Maple

> rng := "-6,-3-1,3-5,7-11,14,15,17-20":
> ExpandRanges( rng );
[-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]

```



## Mathematica


```Mathematica
rangeexpand[ rng_ ] := Module[ { step1 },
step1 = StringSplit[StringReplacePart[rng,"S",StringPosition[ rng,DigitCharacter~~"-"] /. {x_,y_} -> {y,y}],","];
Flatten@ToExpression/@Quiet@StringReplace[step1,x__~~"S"~~y__->"Range["<>x<>","<>y<>"]"] ]
```

{{out|Example}}

```txt
rangeexpand["-6,-3--1,3-5,7-11,14,15,17-20"]
{-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20}
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function L=range_expansion(S)
% Range expansion
if nargin < 1; 
	S='[]';
end

if ~all(isdigit(S) | (S=='-')  | (S==',') | isspace(S))
	error 'invalid input';
end
ixr = find(isdigit(S(1:end-1)) & S(2:end) == '-')+1;
S(ixr)=':';
S=['[',S,']'];
L=eval(S);
```

Usage:

```txt

   range_expansion('-6,-3--1,3-5,7-11,14,15,17-20 ')
ans =
   -6   -3   -2   -1    3    4    5    7    8    9   10   11   14   15   17   18   19   20


```



## MUMPS


```MUMPS
RANGEXP(X) ;Integer range expansion
 NEW Y,I,J,X1,H SET Y=""
 FOR I=1:1:$LENGTH(X,",") DO
 .S X1=$PIECE(X,",",I) FOR  Q:$EXTRACT(X1)'=" "  S X1=$EXTRACT(X1,2,$LENGTH(X1)) ;clean up leading spaces
 .SET H=$FIND(X1,"-")-1
 .IF H=1 SET H=$FIND(X1,"-",(H+1))-1 ;If the first value is negative ignore that "-"
 .IF H<0 SET Y=$SELECT($LENGTH(Y)=0:Y_X1,1:Y_","_X1)
 .IF '(H<0) FOR J=+$EXTRACT(X1,1,(H-1)):1:+$EXTRACT(X1,(H+1),$LENGTH(X1)) SET Y=$SELECT($LENGTH(Y)=0:J,1:Y_","_J)
 KILL I,J,X1,H
 QUIT Y
```

{{out|Example}}

```txt
USER>SET U="-6,-3--1,3-5,7-11,14,15,17-20"

USER>WRITE $$RANGEXP^ROSETTA(U)
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```



## NetRexx

Translation of: [[Range_expansion#Version_2_somewhat_simplified_.21.3F.21|Rexx Version 2]]

```NetRexx
/*NetRexx program to expand a range of integers into a list. *************
* 09.08.2012 Walter Pachl derived from my Rexx version
* Changes: translate(old,' ',',') -> old.translate(' ',',')
*          dashpos=pos('-',x,2)   -> dashpos=x.pos('-',2)
*          Do                     -> Loop
*          Parse Var a x a        -> Parse a x a
*          Parse Var x ...        -> Parse x ...
**********************************************************************/

parse arg old
if old = '' then
  old='-6,-3--1,3-5,7-11,14,15,17-20' /*original list of nums/ranges */

  Say 'old='old                     /*show old list of nums/ranges.  */
  a=old.translate(' ',',')          /*translate commas to blanks     */
  new=''                            /*new list of numbers (so far).  */

  comma=''
  Loop While a<>''                  /* as long as there is input     */
    Parse a x a                     /* get one element               */
    dashpos=x.pos('-',2)            /* find position of dash, if any */
    If dashpos>0 Then Do            /* element is low-high           */
      Parse x low =(dashpos) +1 high /* split the element        */
      Loop j=low To high            /* output all numbers in range   */
        new=new||comma||j           /* with separating commas        */
        comma=','                   /* from now on use comma         */
        End
      End
    Else Do                         /* element is a number           */
      new=new||comma||x             /* append (with comma)           */
      comma=','                     /* from now on use comma         */
      End
    End
  Say 'new='new                     /*show the expanded list         */

```

{{out}}

```txt

old=-6,-3--1,3-5,7-11,14,15,17-20
new=-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20   

```



## Nim


```nim
import parseutils, re, strutils

proc expandRange(input: string): string =
  var output: seq[string] = @[]
  for range in input.split(','):
    var sep = range.find('-', 1)
    if sep > 0: # parse range
      var first = -1
      if range.substr(0, sep-1).parseInt(first) == 0:
        break
      var last = -1
      if range.substr(sep+1).parseInt(last) == 0:
        break
      for i in first..last:
        output.add($i)
    else: # parse single number
      var n = -1
      if range.parseInt(n) > 0:
        output.add($n)
      else:
        break
  return output.join(",")

echo("-6,-3--1,3-5,7-11,14,15,17-20".expandRange)
```


{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE LIVector;
IMPORT SYSTEM;
TYPE
	LIPool = POINTER TO ARRAY OF LONGINT;
	LIVector*= POINTER TO LIVectorDesc;
	LIVectorDesc = RECORD
		cap-: INTEGER;
		len-: INTEGER;
		LIPool: LIPool;
	END;
	
	PROCEDURE (v: LIVector) Init*(cap: INTEGER);
	BEGIN
		v.cap := cap;
		v.len := 0;
		NEW(v.LIPool,cap);
	END Init;
	
	PROCEDURE (v: LIVector) Add*(x: LONGINT);
	VAR 
		newLIPool: LIPool;
	BEGIN
		IF v.len = LEN(v.LIPool^) THEN
			(* run out of space *)
			v.cap := v.cap + (v.cap DIV 2);
			NEW(newLIPool,v.cap);
			SYSTEM.MOVE(SYSTEM.ADR(v.LIPool^),SYSTEM.ADR(newLIPool^),v.cap * SIZE(LONGINT));
			v.LIPool := newLIPool
		END;
		v.LIPool[v.len] := x;
		INC(v.len)
	END Add;
	
	PROCEDURE (v: LIVector) At*(idx: INTEGER): LONGINT;
	BEGIN
		RETURN v.LIPool[idx];
	END At;
END LIVector.

MODULE LIRange;
IMPORT Out, LIV := LIVector;

TYPE
	Range* = POINTER TO RangeDesc;
	RangeDesc = RECORD
		l,r: POINTER TO ARRAY 1 OF LONGINT;
	END;
	
	PROCEDURE (r: Range) Init*();
	BEGIN
		r.l := NIL;
		r.r := NIL;
	END Init;
	
	PROCEDURE (r: Range) IsEmpty*(): BOOLEAN;
	BEGIN
		RETURN (r.l = NIL) & (r.l = NIL);
	END IsEmpty;
	
	PROCEDURE (r: Range) SetLeft*(v: LONGINT);
	BEGIN
		IF r.l = NIL THEN NEW(r.l) END;
		r.l[0] := v;
	END SetLeft;
	
	PROCEDURE (r: Range) SetRight*(v : LONGINT);
	BEGIN
		IF r.r = NIL THEN NEW(r.r) END;
		r.r[0] := v;
	END SetRight;
	
	PROCEDURE (r: Range) LeftPart*(): BOOLEAN;
	BEGIN
		RETURN r.l # NIL;
	END LeftPart;
	
	PROCEDURE (r: Range) GetLeft(): LONGINT;
	BEGIN
		RETURN r.l[0];
	END	GetLeft;
	
	PROCEDURE (r: Range) RightPart*(): BOOLEAN;
	BEGIN
		RETURN r.l # NIL;
	END RightPart;
	
	PROCEDURE (r: Range) GetRight*(): LONGINT;
	BEGIN
		RETURN r.r[0];
	END	GetRight;
	
	PROCEDURE (r: Range) Show*();
	BEGIN
		Out.Char('(');
		IF r.l # NIL THEN Out.LongInt(r.l[0],10) END;
		Out.String(" - ");
		IF r.r # NIL THEN Out.LongInt(r.r[0],10); END;
		Out.Char(')');Out.Ln
	END Show;
	
	PROCEDURE (r: Range) Expand*(VAR liv: LIV.LIVector);
	VAR 
		from, to : LONGINT;
	BEGIN
		IF r.l # NIL THEN from := r.l[0] ELSE from := 0 END;
		IF r.r # NIL THEN to := r.r[0] ELSE to := from END;
		WHILE (from <= to) DO
			liv.Add(from);INC(from)
		END
	END Expand;
END LIRange.

MODULE Splitter;
TYPE
	Splitter* = POINTER TO SplitterDesc;
	SplitterDesc = RECORD
		from: INTEGER;
		c: CHAR;
		s: POINTER TO ARRAY OF CHAR;
	END;
	
	PROCEDURE (s: Splitter) Init*;
	BEGIN
		s.c := ',';
		s.from := 0;
		s.s := NIL;
	END Init;
	
	PROCEDURE (s: Splitter) On*(str: ARRAY OF CHAR);
	BEGIN
		s.from := 0;
		NEW(s.s,LEN(str));
		COPY(str,s.s^)
	END On;
	
	PROCEDURE (s: Splitter) OnWithChar*(str: ARRAY OF CHAR;c: CHAR);
	BEGIN
		s.from := 0;
		s.c := c;
		NEW(s.s,LEN(str));
		COPY(str,s.s^)
	END OnWithChar;
	
	PROCEDURE (s: Splitter) Next*(VAR str: ARRAY OF CHAR);
	VAR 
		k : INTEGER;	
	BEGIN
		k := 0;
		IF (s.from < LEN(s.s^) - 1) & (s.s[s.from] = 0X) THEN str[0] := 0X END; 
		WHILE (k < LEN(str) - 1) & (s.from < LEN(s.s^) - 1) & (s.s[s.from] # s.c) DO
			str[k] := s.s[s.from];
			INC(k);INC(s.from)
		END;
		IF k < LEN(str) - 1 THEN str[k] := 0X ELSE str[LEN(str) - 1] := 0X END;
		WHILE (s.from < LEN(s.s^) - 1) & (s.s[s.from] # s.c) DO INC(s.from) END;
		INC(s.from)
	END Next;
END Splitter.

MODULE ExpandRange;
IMPORT Out, LIV := LIVector, LIR := LIRange, S := Splitter;

PROCEDURE GetNumberFrom(s: ARRAY OF CHAR; VAR from: INTEGER; VAR done: BOOLEAN): LONGINT;
VAR
	d,i: INTEGER;
	num,sign: LONGINT;
BEGIN
	i := from; num := 0;sign := 1;
	CASE s[i] OF
		 '-': sign := -1;INC(i)
		|'+': INC(i);
		ELSE
	END;
	WHILE (i < LEN(s) - 1) & (s[i] >= '0') & (s[i] <= '9') DO
		d := ORD(s[i]) - ORD('0');
		num := d + num * 10;
		INC(i);
	END;
	IF i = from THEN done := FALSE ELSE done := TRUE; from := i END;
	RETURN sign * num
END GetNumberFrom;

PROCEDURE GetRange(s: ARRAY OF CHAR): LIR.Range;
VAR
	r: LIR.Range;
	i: INTEGER;
	num: LONGINT;
	done: BOOLEAN;
BEGIN
	i := 0;NEW(r);r.Init();
	WHILE (i < LEN(s) - 1) & (s[i] = 20X) DO INC(i) END;
	(* Left value *)
	done := FALSE;
	num := GetNumberFrom(s,i,done);
	IF ~done THEN RETURN r END; 
	r.SetLeft(num);

	WHILE (i < LEN(s) - 1) & (s[i] = 20X) DO INC(i) END;
	CASE s[i] OF
		 '-' : INC(i);
		| 0X : RETURN r; 
		ELSE
	END;
	WHILE (i < LEN(s) - 1) & (s[i] = 20X) DO INC(i) END;
	
	(* Right Value *)
	done := FALSE;
	num := GetNumberFrom(s,i,done);
	IF ~done THEN RETURN r END;
	r.SetRight(num);
	RETURN r;
END GetRange;

VAR
	i: INTEGER;
	r: LIR.Range;
	sp: S.Splitter; 
	p : ARRAY 128 OF CHAR;
	liv: LIV.LIVector;
BEGIN
	NEW(sp);sp.Init();
	NEW(liv);liv.Init(128);

	sp.On("-6,-3--1,3-5,7-11,14,15,17-20");
	sp.Next(p);
	WHILE (p[0] # 0X) DO
		r := GetRange(p);
		r.Expand(liv);
		sp.Next(p);
	END;
	FOR i := 0 TO liv.len - 2 DO
		Out.LongInt(liv.At(i),3);Out.Char(',');
	END;
	Out.LongInt(liv.At(liv.len - 1),3);Out.Ln;
END ExpandRange.


```

{{out}}

```txt

-6, -3, -2, -1,  3,  4,  5,  7,  8,  9, 10, 11, 14, 15, 17, 18, 19, 20

```



## OCaml


```ocaml
#load "str.cma"

let range a b =
  if b < a then invalid_arg "range";
  let rec aux i acc =
    if i = b then List.rev (i::acc)
    else aux (succ i) (i::acc)
  in
  aux a []

let parse_piece s =
  try Scanf.sscanf s "%d-%d" (fun a b -> range a b)
  with _ -> [int_of_string s]

let range_expand rng =
  let ps = Str.split (Str.regexp_string ",") rng in
  List.flatten (List.map parse_piece ps)

let () =
  let rng = "-6,-3--1,3-5,7-11,14,15,17-20" in
  let exp = range_expand rng in
  List.iter (Printf.printf " %d") exp;
  print_newline ()
```



## Oforth


```oforth
: addRange( s res -- )
| i n |
    s asInteger dup ifNotNull: [ res add return ] drop
    s indexOfFrom('-', 2) ->i
    s left( i 1- ) asInteger  s right( s size i - ) asInteger
    for: n [ n res add ]
;

: rangeExpand ( s -- [ n ] )
    ArrayBuffer new  s wordsWith( ',' ) apply( #[ over addRange ] ) ;
```



## ooRexx


```ooRexx

list = '-6,-3--1,3-5,7-11,14,15,17-20'
expanded = expandRanges(list)

say "Original list: ["list"]"
say "Expanded list: ["expanded~tostring("l", ",")"]"

-- expand a string expression a range of numbers into a list
-- of values for the range.  This returns an array
::routine expandRanges
  use strict arg list
  values = list~makearray(',')
  -- build this up using an array first.  Make this at least the
  -- size of the original value set.
  expanded = .array~new(values~items)

  -- now process each element in the range
  loop element over values
      -- if this is a valid number, it's not a range, so add it directly
      if element~datatype('whole') then expanded~append(element)
      else do
          -- search for the divider, starting from the second position
          -- to allow for the starting value to be a minus sign.
          split = element~pos('-', 2)
          parse var element start =(split) +1 finish
          loop i = start to finish
              expanded~append(i)
          end
      end
  end
  return expanded

```

{{out}}

```txt

Original list: [-6,-3--1,3-5,7-11,14,15,17-20]
Expanded list: [-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]

```



## Oz


```oz
declare
  fun {Expand RangeDesc}
     {Flatten
      {Map {ParseDesc RangeDesc}
       ExpandRange}}
  end

  fun {ParseDesc Txt}
     {Map {String.tokens Txt &,} ParseRange}
  end

  fun {ParseRange R}
     if {Member &- R.2} then
        First Second
     in
        {String.token R.2 &- ?First ?Second}
        {String.toInt R.1|First}#{String.toInt Second}
     else
        Singleton = {String.toInt R}
     in
        Singleton#Singleton
     end
  end

  fun {ExpandRange From#To}
     {List.number From To 1}
  end
in
  {System.showInfo
   {Value.toVirtualString {Expand "-6,-3--1,3-5,7-11,14,15,17-20"} 100 100}}
```

{{out|Sample output}}

```oz
[~6 ~3 ~2 ~1 3 4 5 7 8 9 10 11 14 15 17 18 19 20]
```



## Perl

One-liner:

```Perl
sub rangex {
    map { /^(.*\d)-(.+)$/ ? $1..$2 : $_ } split /,/, shift
}

# Test and display 
print join(',', rangex('-6,-3--1,3-5,7-11,14,15,17-20')), "\n";
```

{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```


Alternative:

```Perl
sub rangex {
    (my $range = shift) =~ s/(?<=\d)-/../g;
    eval $range;
}
```



## Perl 6


{{works with|Rakudo|2016.07}}

```Perl6
sub range-expand (Str $range-description) {
    my token number { '-'? \d+ }
    my token range  { (<&number>) '-' (<&number>) }
    
    $range-description
        .split(',')
        .map({ .match(&range) ?? $0..$1 !! +$_ })
        .flat
}
 
say range-expand('-6,-3--1,3-5,7-11,14,15,17-20').join(', ');
```


{{out}}

```txt
-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20
```




Alternatively, using a grammar:


```perl6
grammar RangeList {
    token TOP    { <term>* % ','    { make $<term>.map(*.made)       } }
    token term   { [<range>|<num>]  { make ($<num> // $<range>).made } }
    token range  { <num> '-' <num>  { make +$<num>[0] .. +$<num>[1]  } }
    token num    { '-'? \d+         { make +$/                       } }
}

say RangeList.parse('-6,-3--1,3-5,7-11,14,15,17-20').made.flat.join(', ');
```


{{out}}

```txt
-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20
```



## Phix


```Phix
function range_expansion(string range)
sequence s = split(range,','),
         res = {}
    for i=1 to length(s) do
        string si = s[i]
        integer k = find('-',si,2)
        if k=0 then
            res = append(res,to_number(si))
        else
            integer startrange = to_number(si[1..k-1])
            integer endofrange = to_number(si[k+1..$])
            for l=startrange to endofrange do
                res = append(res,l)
            end for
        end if
    end for
    return res
end function

?range_expansion("-6,-3-1,3-5,7-11,14,15,17-20")
?range_expansion("-6,-3--1,3-5,7-11,14,15,17-20")
```

{{out}}

```txt

{-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20}
{-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20}

```



## PHP

{{trans|Python}}

```PHP
function rangex($str) {
    $lst = array();
    foreach (explode(',', $str) as $e) {
        if (strpos($e, '-', 1) !== FALSE) {
            list($a, $b) = explode('-', substr($e, 1), 2);
            $lst = array_merge($lst, range($e[0] . $a, $b));
        } else {
            $lst[] = (int) $e;
        }
    }
    return $lst;
}
```



## PicoLisp


```PicoLisp
(de rangeexpand (Str)
   (make
      (for S (split (chop Str) ",")
         (if (index "-" (cdr S))
            (chain
               (range
                  (format (head @ S))
                  (format (tail (- -1 @) S)) ) )
            (link (format S)) ) ) ) )
```

{{out}}

```txt
: (rangeexpand "-6,-3--1,3-5,7-11,14,15,17-20")
-> (-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20)
```



## PL/I


```PL/I
range_expansion:
   procedure options (main);

get_number:
   procedure (Number, c, eof);
   declare number fixed binary (31), c character (1), eof bit (1) aligned;
   declare neg fixed binary (1);

   number = 0; eof = false;
   do until (c ^= ' ');
      get edit (c) (a(1));
   end;
   if c = '-' then do; get edit (c) (a(1)); neg = -1; end; else neg = 1;
   do forever;
      select (c);
         when ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
                    number = number*10 + c;
         when (',', '-') do; number = neg*number; return; end;
         otherwise signal error;
      end;
      on endfile (sysin) go to exit;
      get edit (c) (a(1));
   end;
exit:
   number = neg*number;
   eof = true;
end get_Number;

   declare c character, (i, range_start, range_end) fixed binary (31);
   declare eof bit (1) aligned;
   declare true bit (1) value ('1'b), false bit (1) value ('0'b);
   declare delimiter character (1) initial (' ');
   declare out file output;

   open file (out) output title ('/out, type(text),recsize(80)');
   do while (^eof);
      call get_number(range_start, c, eof);
      if c = '-' then /* we have a range */
         do;
            call get_number (range_end, c, eof);
            do i = range_start to range_end;
               put file (out) edit (delimiter, i) (a, f(3));
            end;
         end;
      else
         do;
            put file (out) edit (delimiter, range_start) (a, f(3));
         end;
      delimiter = ',';
   end;
end range_expansion;
```

{{out}}

```txt

  -6, -3, -2, -1,  3,  4,  5,  7,  8,  9, 10, 11, 14, 15, 17, 18, 19, 20

```



## PowerShell


```PowerShell

function range-expansion($array) {
    function expansion($arr) { 
        if($arr) {
            $arr = $arr.Split(',')
             $arr | foreach{
                $a = $_
                $b, $c, $d, $e = $a.Split('-')
                switch($a) {
                    $b {return $a}
                    "-$c" {return $a}
                    "$b-$c" {return "$(([Int]$b)..([Int]$c))"}
                    "-$c-$d" {return "$(([Int]$("-$c"))..([Int]$d))"}
                    "-$c--$e" {return "$(([Int]$("-$c"))..([Int]$("-$e")))"}
                }
             }
        } else {""}
    }
    $OFS = ", "
    "$(expansion $array)"
    $OFS = " "
}
range-expansion "-6,-3--1,3-5,7-11,14,15,17-20"

```

<b>Output:</b>

```txt

-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20

```


===Alternate Half-Assed Regex Version===
Ten times faster (only minimally tested).

```PowerShell

function Expand-Range
{
    [CmdletBinding()]
    [OutputType([int])]
    Param
    (
        [Parameter(Mandatory=$true, 
                   Position=0)]
        [ValidateNotNullOrEmpty()]
        [ValidatePattern('^[0-9,-]*$')]
        [string]
        $Range
    )

    try
    {
        if ($Range -match '-,')       # I'm not good enough to weed this case out with Regex
        {
            throw "Input string was not in a correct format."
        }

        [int[]]$output = $Range -split ',' | ForEach-Object {

            [int[]]$array = $_ -split '(?<=\d)-'  

            if ($array.Count -gt 1)   # $array contains one or two elements
            {
                $array[0]..$array[1]  # two elements = start and end of range
            }
            else
            {
                $array                # one element = an integer
            }
        }
    }
    catch
    {
        throw "Input string was not in a correct format."
    }

    $output
}

```


```PowerShell

(Expand-Range "-6,-3--1,3-5,7-11,14,15,17-20") -join ", "

```

{{Out}}

```txt

-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20

```



## Prolog

{{Works with|SWI Prolog}}
{{libheader|clpfd}}
The code uses three predicates '''extract_Range/2''', '''study_Range/2''' and '''pack_Range/2'''.<BR>
Every predicate works in both directions arg1 towards arg2 and arg2 towards arg1, so that '''Range expansion''' and '''Range extraction''' work with the same predicates but in reverse order.

```Prolog
range_expand :-
	L = '-6,-3--1,3-5,7-11,14,15,17-20',
	writeln(L),
	atom_chars(L, LA),
	extract_Range(LA, R),
	maplist(study_Range, R, LR),
	pack_Range(LX, LR),
	writeln(LX).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extract_Range(?In, ?Out)
% In  : '-6,-3--1,3-5,7-11,14,15,17-20'  
% Out : [-6], [-3--1], [3-5],[7-11], [14],[15], [17-20]
% 
extract_Range([], []).

extract_Range(X , [Range | Y1]) :-
	get_Range(X, U-U, Range, X1),
	extract_Range(X1, Y1).

get_Range([], Range-[], Range, []).
get_Range([','|B], Range-[], Range, B) :- !.

get_Range([A | B], EC, Range, R) :-
	append_dl(EC, [A | U]-U, NEC),
	get_Range(B, NEC, Range, R).


append_dl(X-Y, Y-Z, X-Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% study Range(?In, ?Out)
% In  : [-6]
% Out : [-6,-6]
% 
% In  : [-3--1]
% Out : [-3, -1]
% 
study_Range(Range1, [Deb, Deb]) :-
       catch(number_chars(Deb, Range1), Deb, false).

study_Range(Range1, [Deb, Fin]) :-
       append(A, ['-'|B], Range1),
       A \= [],
       number_chars(Deb, A),
       number_chars(Fin, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
:- use_module(library(clpfd)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pack Range(?In, ?Out)
% In  : -6,
% Out : [-6]
% 
% In  : -3, -2,-1
% Out : [-3,-1]
%
pack_Range([],[]).

pack_Range([X|Rest],[[X | V]|Packed]):-
    run(X,Rest, [X|V], RRest),
    pack_Range(RRest,Packed).


run(Fin,[Other|RRest], [Deb, Fin],[Other|RRest]):-
	Fin #\= Deb,
	Fin #\= Deb + 1,
	Other #\= Fin+1.

run(Fin,[],[_Var, Fin],[]).

run(Var,[Var1|LRest],[Deb, Fin], RRest):-
	Fin #\= Deb,
	Fin #\= Deb + 1,
	Var1 #= Var + 1,
	run(Var1,LRest,[Deb, Fin], RRest).

run(Val,[Other|RRest], [Val, Val],[Other|RRest]).
```

{{out}}

```txt
 ?- range_expand.
-6,-3--1,3-5,7-11,14,15,17-20
[-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]
true
```



## PureBasic


```PureBasic
Procedure rangeexpand(txt.s, List outputList())
  Protected rangesCount = CountString(txt, ",") + 1
  Protected subTxt.s, r, rangeMarker, rangeStart, rangeFinish, rangeIncrement, i
  
  LastElement(outputList())
  For r = 1 To rangesCount
    subTxt = StringField(txt, r, ",")
    rangeMarker = FindString(subTxt, "-", 2)
    If rangeMarker
      rangeStart = Val(Mid(subTxt, 1, rangeMarker - 1))
      rangeFinish = Val(Mid(subTxt, rangeMarker + 1))
      
      If rangeStart > rangeFinish
        rangeIncrement = -1
      Else
        rangeIncrement = 1
      EndIf 
      
      i = rangeStart - rangeIncrement
      Repeat 
        i + rangeIncrement
        AddElement(outputList()): outputList() = i
      Until i = rangeFinish
    Else
      AddElement(outputList()): outputList() = Val(subTxt)
    EndIf 
  Next
EndProcedure 

Procedure outputListValues(List values())
  Print("[ ")
  ForEach values()
    Print(Str(values()) + " ") 
  Next
  PrintN("]")
EndProcedure

If OpenConsole()
  NewList values()
  rangeexpand("-6,-3--1,3-5,7-11,14,15,17-20", values())
  outputListValues(values())
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
[ -6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20 ]
```



## Python


### Procedural


```python
def rangeexpand(txt):
    lst = []
    for r in txt.split(','):
        if '-' in r[1:]:
            r0, r1 = r[1:].split('-', 1)
            lst += range(int(r[0] + r0), int(r1) + 1)
        else:
            lst.append(int(r))
    return lst

print(rangeexpand('-6,-3--1,3-5,7-11,14,15,17-20'))
```


another variant, using [[regular expressions]] to parse the ranges,

```python
import re

def rangeexpand(txt):
    lst = []
    for rng in txt.split(','):
        start,end = re.match('^(-?\d+)(?:-(-?\d+))?$', rng).groups()
        if end:
            lst.extend(xrange(int(start),int(end)+1))
        else:
            lst.append(int(start))
    return lst
```



### Functional

As a fold/catamorphism:
{{Works with|Python|3.7}}

```python
'''Range expansion'''

from functools import (reduce)


# rangeExpansion :: String -> [Int]
def rangeExpansion(s):
    '''List of integers expanded from a
       comma-delimited string of individual
       numbers and hyphenated ranges.
    '''
    def go(a, x):
        tpl = breakOn('-')(x[1:])
        r = tpl[1]
        return a + (
            [int(x)] if not r
            else enumFromTo(int(x[0] + tpl[0]))(
                int(r[1:])
            )
        )
    return reduce(go, s.split(','), [])


# TEST ----------------------------------------------------
def main():
    '''Expansion test'''

    print(
        fTable(__doc__ + ':')(
            lambda x: "\n'" + str(x) + "'"
        )(lambda x: '\n\n\t' + showList(x))(
            rangeExpansion
        )([
            '-6,-3--1,3-5,7-11,14,15,17-20'
        ])
    )


# GENERIC FUNCTIONS ---------------------------------------

# breakOn :: String -> String -> (String, String)
def breakOn(needle):
    '''A tuple of:
       1. the prefix of haystack before needle,
       2. the remainder of haystack, starting
          with needle.
    '''
    def go(haystack):
        xs = haystack.split(needle)
        return (xs[0], haystack[len(xs[0]):]) if (
            1 < len(xs)
        ) else (haystack, '')
    return lambda haystack: go(haystack) if (
        needle
    ) else None


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# fTable :: String -> (a -> String) ->
#                     (b -> String) ->
#        (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(lambda x: len(xShow(x)), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + (
                ' -> '
            ) + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(str(x) for x in xs) + ']'


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Range expansion:

'-6,-3--1,3-5,7-11,14,15,17-20' -> 

    [-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]
```



## R


```R

rangeExpand <- function(text) {
  lst <- gsub("(\\d)-", "\\1:", unlist(strsplit(text, ",")))
  unlist(sapply(lst, function (x) eval(parse(text=x))), use.names=FALSE)
}

rangeExpand("-6,-3--1,3-5,7-11,14,15,17-20")
 [1] -6 -3 -2 -1  3  4  5  7  8  9 10 11 14 15 17 18 19 20

```



## Racket


```racket

#lang racket

(define (range-expand s)
  (append*
   (for/list ([r (regexp-split "," s)])
     (match (regexp-match* "(-?[0-9]+)-(-?[0-9]+)" r 
                           #:match-select cdr)
       [(list (list f t)) 
        (range (string->number f) (+ (string->number t) 1))]
       [(list)     
        (list (string->number r))]))))

(range-expand "-6,-3--1,3-5,7-11,14,15,17-20")

```

{{out}}

```txt

'(-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20)

```


Here is an alternative version without regular expressions. 
It uses the builtin function read to read the numbers.
Since 3--4 is normally parsed as a symbol 
rather than 3 followed by - followed by -4, 
a readtable is installed that makes - a delimiter.

```racket

#lang racket

(define on-minus
  (case-lambda
    [(ch ip) (on-minus ch ip #f #f #f #f)]
    [(ch ip src line col pos)
     (if (char-numeric? (peek-char ip))
         (- (read ip))
         (datum->syntax #f '-))]))

(define minus-delimits
  (make-readtable (current-readtable) #\- 'terminating-macro on-minus))

(define (range-expand s)
  (parameterize ([current-readtable minus-delimits])
    (append*
     (for/list ([f (in-port read s)])
       (match (peek-char s)
         [#\, (read-char s)
              (list f)]
         [#\- (read-char s)
              (define t (read s))
              (read-char s)
              (range f (+ t 1))])))))
     
(range-expand (open-input-string "-6,-3--1,3-5,7-11,14,15,17-20"))

```

Note that one can use the full number syntax in this alternative version:

```txt

> (range-expand (open-input-string "1-6/3,3e1-32"))
'(1 2 30.0 31.0 32.0)

```



## Raven

Based loosely on Ruby

```raven
define get_num use $lst
    # "-22" split by "-" is [ "", "22" ]  so check if
    # first list item is "" -> a negative number
    $lst 0 get "" = if
        # negative number
        #
        # convert str to integer and multiply by -1
        -1 $lst 1 get 0 prefer *
        $lst shift $lst shift drop drop
    else
        # positive number
        $lst 0 get 0 prefer
        $lst shift drop

define range_expand use $rng
    [ ] as $res
    $rng "," split each as $r
        $r m/^(-?\d+)-(-?\d+)$/ TRUE = if
            $r s/-/g as $parts
            $parts get_num as $from
            $parts get_num as $to
            # int list to str list, then joined by ","
            group
                $from $to 1 range each "" prefer
            list "," join $res push
            # range doesn't include the $to, so add to end of generated range
            $to "%d" $res push
        else
            $r $res push
    $res "," join print
    "\n" print

'-6,-3--1,3-5,7-11,14,15,17-20' range_expand
```

{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```



## REXX


### version 1

Extra imbedded blanks were added to the   old   list (which are ignored) to make the   ''over/under''   comparison easier   (in the output).

```rexx
/*REXX program expands an  ordered list  of  integers  into  an expanded list.          */
old= '-6,-3--1,   3-5,  7-11,       14,15,17-20';       a=translate(old,,',')
new=                                             /*translate [↑]  commas (,) ───► blanks*/
      do until a=='';   parse var a X a          /*obtain the next integer ──or── range.*/
      p=pos('-', X, 2)                           /*find the location of a dash (maybe). */
      if p==0 then  new=new   X                  /*append integer   X   to the new list.*/
              else  do j=left(X,p-1)  to substr(X,p+1);     new=new j
                    end   /*j*/                  /*append a single [↑] integer at a time*/
      end                 /*until*/
                                                 /*stick a fork in it,  we're all done. */
new=translate( strip(new),  ',',  " ")           /*remove the first blank,  add commas. */
say 'old list: '   old                           /*show the  old list of numbers/ranges.*/
say 'new list: '   new                           /*  "   "   new   "   " numbers.       */
```

'''output'''

```txt

old list:  -6,-3--1,   3-5,  7-11,       14,15,17-20
new list:  -6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20

```


===Version 2 somewhat simplified !?!===

```rexx
/*REXX program to expand a range of integers into a list. *************
* 09.08.2012 Walter Pachl
**********************************************************************/

  parse arg old
  if old = '' then -
  old='-6,-3--1,3-5,7-11,14,15,17-20' /*original list of nums/ranges */

  Say 'old='old                     /*show old list of nums/ranges.  */
  a=translate(old,,',')             /*translate commas to blanks     */
  new=''                            /*new list of numbers (so far).  */

  comma=''
  Do While a<>''                    /* as long as there is input     */
    Parse var a x a                 /* get one element               */
    dashpos=pos('-',x,2)            /* find position of dash, if any */
    If dashpos>0 Then Do            /* element is low-high           */
      Parse Var x low =(dashpos) +1 high /* split the element        */
      Do j=low To high              /* output all numbers in range   */
        new=new||comma||j           /* with separating commas        */
        comma=','                   /* from now on use comma         */
        End
      End
    Else Do                         /* element is a number           */
      new=new||comma||x             /* append (with comma)           */
      comma=','                     /* from now on use comma         */
      End
    End
  Say 'new='new                     /*show the expanded list         */
```

{{out}}

```txt

old=-6,-3--1,3-5,7-11,14,15,17-20
new=-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20   

```



## Ring


```ring

# Project : Range expansion

int = "-6,-3--1,3-5,7-11,14,15,17-20"
int = str2list(substr(int, ",", nl))
newint = []
for n=1 to len(int)
     nrint = substr(int[n], "-")
     nrint2 = substr(int[n], "--")
     if nrint2 > 0
        temp1 = left(int[n], nrint2 -1)
        temp2 = right(int[n], len(int[n]) - nrint2)
        add(newint, [temp1,temp2])
     else
        if len(int[n]) <= 2
           add(newint, [int[n], ""])
        else 
           if nrint > 0 and nrint2 = 0
              temp1 = left(int[n], nrint - 1)
              temp2 = right(int[n], len(int[n]) - nrint)
              add(newint, [temp1,temp2]) 
           ok  
        ok
     ok
next
showarray(newint)

func showarray(vect)
       see "["
       svect = ""
       for n = 1 to len(vect)
           if newint[n][2] != ""
              for nr = newint[n][1] to newint[n][2]
                  svect = svect +"" + nr + ", "
              next
           else
              svect = svect +"" + newint[n][1] + ", "
           ok
       next
       svect = left(svect, len(svect) - 2)
       see svect
       see "]" + nl

```

Output:

```txt

[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]

```



## Ruby


```ruby
def range_expand(rng)
  rng.split(',').flat_map do |part|
    if part =~ /^(-?\d+)-(-?\d+)$/
      ($1.to_i .. $2.to_i).to_a
    else
      Integer(part)
    end
  end
end

p range_expand('-6,-3--1,3-5,7-11,14,15,17-20')
```

{{out}}

```txt
[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
```



## Run BASIC


```runbasic
PRINT rangeExpand$("-6,-3--1,3-5,7-11,14,15,17-20")
end

function rangeExpand$(range$)
[loop]
i	= INSTR(range$, "-", i+1)
IF i THEN
  j = i
  WHILE MID$(range$,j-1,1) <> "," AND j <> 1
    j = j - 1
  wend
  IF i > j then
    IF MID$(range$,j,i-j) <> str$(i-j)+" " THEN
      t$ = ""
      FOR k = VAL(MID$(range$,j)) TO VAL(MID$(range$,i+1))-1
        t$ = t$ + str$(k) + ","
      NEXT k
      range$ = LEFT$(range$,j-1) + t$ + MID$(range$,i+1)
      i = j + LEN(t$) + 2
    end if
  end if
end if
if i <> 0 then goto [loop]
rangeExpand$ = range$
end function
```

{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```



## Rust

Rust doesn't have regex in standard library yet.


```rust
use std::str::FromStr;

// Precondition: range doesn't contain multibyte UTF-8 characters
fn range_expand(range : &str) -> Vec<i32> {
   range.split(',').flat_map(|item| {
        match i32::from_str(item) {
            Ok(n) => n..n+1,
            _ => {
                let dashpos=
                    match item.rfind("--") {
                        Some(p) => p,
                        None => item.rfind('-').unwrap(),
                    };
                let rstart=i32::from_str(
                    unsafe{ item.slice_unchecked(0,dashpos)} ).unwrap();
                let rend=i32::from_str(
                    unsafe{ item.slice_unchecked(dashpos+1,item.len()) } ).unwrap();
                rstart..rend+1
            },
        }
    }).collect()
}

fn main() {
    println!("{:?}", range_expand("-6,-3--1,3-5,7-11,14,15,17-20"));
}

```


{{out}}


```txt
[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]

```


=={{header|S-lang}}==
<lang S-lang>variable r_expres = "-6,-3--1,3-5,7-11,14,15,17-20", s, r_expan = {}, dpos, i;

foreach s (strchop(r_expres, ',', 0))
{
  % S-Lang built-in RE's are fairly limited, and have a quirk:
  %   grouping is done with \\( and \\), not ( and )
  % [PCRE and Oniguruma RE's are available via standard libraries]
  if (string_match(s, "-?[0-9]+\\(-\\)-?[0-9]+", 1)) {
    
    (dpos, ) = string_match_nth(1);

    % Create/loop-over a "range array": from num before - to num after it:
    foreach i ( [integer(substr(s, 1, dpos)) : integer(substr(s, dpos+2, -1))] )
      list_append(r_expan, string(i));
  }
  else
    list_append(r_expan, s);
}
print(strjoin(list_to_array(r_expan), ", "));
```



## Scala


```Scala
def rangex(str: String): Seq[Int] =
  str split "," flatMap { (s) =>
    val r = """(-?\d+)(?:-(-?\d+))?""".r
    val r(a,b) = s
    if (b == null) Seq(a.toInt) else a.toInt to b.toInt
  }
```


{{out}}

```txt
> println(rangex("-6,-3-1,3-5,7-11,14,15,17-20"))
ArraySeq(-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20)
> println(rangex("-6,-3--1,3-5,7-11,14,15,17-20"))
ArraySeq(-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20)
```



## Scheme


```scheme
(define split
  (lambda (str char skip count)
    (let ((len (string-length str)))
      (let loop ((index skip)
                 (last-index 0)
                 (result '()))
        (if (= index len)
            (reverse (cons (substring str last-index) result))
            (if (eq? char (string-ref str index))
                (loop (if (= count (+ 2 (length result)))
                          len
                          (+ index 1))
                      (+ index 1)
                      (cons char (cons (substring str last-index index)
                                       result)))
                (loop (+ index 1)
                      last-index
                      result)))))))

(define range-expand
  (lambda (str)
    (for-each
     (lambda (token)
       (if (char? token)
           (display token)
           (let ((range (split token #\- 1 2)))
             (if (null? (cdr range))
                 (display (car range))
                 (do ((count (string->number (list-ref range 0)) (+ 1 count))
                      (high (string->number (list-ref range 2))))
                     ((= count high) (display high))
                   (display count)
                   (display ","))))))
     (split str #\, 0 0))
    (newline)))
```

{{out}}

```txt

(range-expand "-6,-3--1,3-5,7-11,14,15,17-20")
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20

```



## Seed7

The library [http://seed7.sourceforge.net/libraries/scanstri.htm scanstri.s7i] defines the function [http://seed7.sourceforge.net/libraries/scanstri.htm#getInteger%28inout_string%29 getInteger] to extract substrings with integer literals (optional sign followed by a sequence of digits) from a string.
The integer literals are converted to the type [http://seed7.sourceforge.net/libraries/integer.htm integer] with the [http://seed7.sourceforge.net/libraries/integer.htm#%28attr_integer%29parse%28in_string%29 parse] operator.

```seed7
$ include "seed7_05.s7i";
  include "scanstri.s7i";

const func array integer: rangeExpansion (in var string: rangeStri) is func
  result
    var array integer: numbers is 0 times 0;
  local
    var integer: number is 0;
  begin
    while rangeStri <> "" do
      number := integer parse getInteger(rangeStri);
      numbers &:= number;
      if startsWith(rangeStri, "-") then
        rangeStri := rangeStri[2 ..];
        for number range succ(number) to integer parse getInteger(rangeStri) do
          numbers &:= number;
        end for;
      end if;
      if startsWith(rangeStri, ",") then
        rangeStri := rangeStri[2 ..];
      elsif rangeStri <> "" then
        raise RANGE_ERROR;
      end if;
    end while;
  end func;

const proc: main is func
  local
    var integer: number is 0;
  begin
    for number range rangeExpansion("-6,-3--1,3-5,7-11,14,15,17-20") do
      write(number <& " ");
    end for;
    writeln;
  end func;
```

{{out}}

```txt

-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20 

```



## Sidef


```ruby
func rangex(str) {
    str.split(',').map { |r|
        var m = r.match(/^
            (?(DEFINE) (?<int>[+-]?[0-9]+) )
            (?<from>(?&int))-(?<to>(?&int))
        $/x)
        m ? do {var c = m.ncap; (Num(c{:from}) .. Num(c{:to}))...}
          : Num(r)
    }
}

say rangex('-6,-3--1,3-5,7-11,14,15,17-20').join(',')
```

{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```



## SNOBOL4


```SNOBOL4
*       # Return range n1 .. n2        
        define('range(n1,n2)') :(range_end)
range   range = range n1 ','; n1 = lt(n1,n2) n1 + 1 :s(range)
        range rtab(1) . range :(return)
range_end        

        define('rangex(range)d1,d2') 
        num = ('-' | '') span('0123456789') :(rangex_end)
rangex  range num . d1 '-' num . d2 = range(d1,d2) :s(rangex)
        rangex = range :(return)
rangex_end

*       # Test and display        
        output = rangex('-6,-3--1,3-5,7-11,14,15,17-20')
end
```

{{out}}

```txt
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
```



## Tailspin


```tailspin

composer expand
  [<element>*]
  rule element: <range|INT> (<','>?)
  rule range: (def start: <INT>; <'-'>) <INT> -> $start..$
end expand

'-6,-3--1,3-5,7-11,14,15,17-20' -> expand -> !OUT::write

```

{{out}}

```txt

[-6, -3, -2, -1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]

```



## Tcl


```tcl
proc rangeExpand desc {
    set result {}
    foreach term [split $desc ","] {
	set count [scan $term %d-%d from to]
	if {$count == 1} {
	    lappend result $from
	} elseif {$count == 2} {
	    for {set i $from} {$i <= $to} {incr i} {lappend result $i}
	}
    }
    return $result
}

puts [rangeExpand "-6,-3--1,3-5,7-11,14,15,17-20"]
```

{{out}}

```txt
-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
rangednrs="-6,-3--1,3-5,7-11,14,15,17-20"
expandnrs=SPLIT (rangednrs,":,:")

LOOP/CLEAR r=expandnrs
 test=STRINGS (r,":><-><<>>/:")
 sz_test=SIZE (test)
 IF (sz_test==1) THEN
  expandnrs=APPEND (expandnrs,r)
 ELSE
  r=SPLIT (r,"::<|->/::-:",beg,end)
  expandnrs=APPEND (expandnrs,beg)
  LOOP/CLEAR next=beg,end
   next=next+1
   expandnrs=APPEND (expandnrs,next)
   IF (next==end) EXIT
  ENDLOOP
 ENDIF
ENDLOOP
expandnrs= JOIN (expandnrs,",")

PRINT expandnrs
```

{{out}}

```txt

-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20 

```



## TXR


A solution with three main parts: 
* a parse-expression-grammar driven parser to decimate the input to a Lisp data structure;
* some Lisp code to expand the list, sort it, and remove duplicates (recursion, hashing, sorting).
* driver code which matches the input with the grammar, and produces output with the help of the Lisp code.

The grammar is:

```txt
num := [ + | - ] { digit } +

entry := num [ ws ] - [ ws ] num
      |  num

rangelist := entry [ ws ] , [ ws ] rangelist
          |  entry
          |  /* empty */
```


Code:


```txr
@(define num (n))@(local tok)@{tok /[+\-]?\d+/}@(bind n @(int-str tok))@(end)
@(define entry (e))@\
  @(local n1 n2)@\
  @(cases)@\
    @(num n1)@/\s*-\s*/@(num n2)@\
    @(bind e (n1 n2))@\
  @(or)@\
    @(num n1)@\
    @(bind e n1)@\
  @(end)@\
@(end)
@(define rangelist (list))@\
  @(local first rest)@\
  @(cases)@\
    @(entry first)@/\s*,\s*/@(rangelist rest)@\
    @(bind list @(cons first rest))@\
  @(or)@\
    @(entry first)@\
    @(bind list (first))@\
  @(or)@\
    @(bind list nil)@\
  @(end)@\
@(end)
@(do
   (defun expand-helper (list)
     (cond
       ((null list) nil)
       ((consp (first list))
        (append (range (first (first list))
                       (second (first list)))
                (rangeexpand (rest list))))
       (t (cons (first list) (rangeexpand (rest list))))))

   (defun rangeexpand (list)
     (uniq (expand-helper list))))
@(repeat)
@(rangelist x)@{trailing-junk}
@(output)
raw syntax: @x
expansion:  @(rangeexpand x)
your junk:  @{trailing-junk}
@(end)
@(end)
```


{{out|Run}}

```txt
$ txr range-expansion.txr -
1,2,3-5,-3--1
raw syntax: 1 2 (3 5) (-3 -1)
expansion:  (-3 -2 -1 1 2 3 4 5)
your junk:
-6,-3--1,3-5,7-11,14,15,17-20
raw syntax: -6 (-3 -1) (3 5) (7 11) 14 15 (17 20)
expansion:  (-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20)
your junk:
-6,-3--1,3-5,7-11,14,15,17-20,cg@foo
raw syntax: -6 (-3 -1) (3 5) (7 11) 14 15 (17 20)
expansion:  (-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20)
your junk:  cg@foo
```


Note how the junk in the last example does not contain the trailing comma. 
This is because the rangelist grammar production allows for an empty range, 
so syntax like "5," is valid: it's an entry followed by a comma and a rangelist, 
where the rangelist is empty.


## UNIX Shell

{{works with|bash}}

```bash
#!/usr/bin/bash

range_expand () (
    IFS=,
    set -- $1
    n=$#
    for element; do
        if [[ $element =~ ^(-?[0-9]+)-(-?[0-9]+)$ ]]; then
            set -- "$@" $(eval echo "{${BASH_REMATCH[1]}..${BASH_REMATCH[2]}}")
        else
            set -- "$@" $element
        fi
    done
    shift $n
    echo "$@"
    # to return a comma-separated value: echo "${*// /,}"
)

range_expand "-6,-3--1,3-5,7-11,14,15,17-20"
```

{{out}}

```txt
-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20
```



## Ursala


```Ursala
#import std
#import int

rex = sep`,; zrange+*= %zp~~htttPzztPQhQXbiNC+ rlc ~&r~=`-

#cast %zL

t = rex '-6,-3--1,3-5,7-11,14,15,17-20'
```

{{out}}

```txt
<-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20>
```



## VBA


```VBA
Public Function RangeExpand(AString as string)
' return a list with the numbers expressed in AString
Dim Splits() As String
Dim List() As Integer
Dim count As Integer

count = -1 'to start a zero-based List() array
' first split it using comma as delimiter
Splits = Split(AString, ",")
' process all fragments
For Each fragment In Splits
  'is there a "-" in it (do not consider first character)?
  P = InStr(2, fragment, "-")
  If P > 0 Then 'yes, so it's a range: find start and end numbers
    nstart = Val(left$(fragment, P - 1))
    nend = Val(Mid$(fragment, P + 1))
    j = count
    count = count + (nend - nstart + 1)
    'add numbers in range to List
    ReDim Preserve List(count)
    For i = nstart To nend
      j = j + 1
      List(j) = i
    Next
  Else
    'not a range, add a single number
    count = count + 1
    ReDim Preserve List(count)
    List(count) = Val(fragment)
  End If
Next
RangeExpand = List
End Function

Public Sub RangeExpandTest()
'test function RangeExpand
Dim X As Variant

X = RangeExpand("-6,-3--1,3-5,7-11,14,15,17-20")
'print X
Debug.Print "Result:"
For Each el In X
  Debug.Print el;
Next
Debug.Print
End Sub
```

{{out}}

```txt

RangeExpandTest
Result:
-6 -3 -2 -1  3  4  5  7  8  9  10  11  14  15  17  18  19  20 

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings, instead of MSb
char Str;
int  Char, Inx;


proc GetCh;                     \Get character from Str
[Char:= Str(Inx);
Inx:= Inx+1;
]; \GetCh


func GetNum;                    \Get number from Str and return its value
int Neg, Num;
[Neg:= false;
if Char = ^- then [Neg:= true;  GetCh];
Num:= 0;
while Char>=^0 & Char<=^9 do
        [Num:= Num*10 + Char-^0;
        GetCh;
        ];
return if Neg then -Num else Num;
]; \GetNum


int I, N0, N1;
[Str:= "-6,-3--1,3-5,7-11,14,15,17-20";
Inx:= 0;
GetCh;                          \one character look ahead
loop    [N0:= GetNum;
        IntOut(0,N0);
        case Char of
          ^,:   [GetCh;  ChOut(0,^,)];
          ^-:   [GetCh;
                N1:= GetNum;
                for I:= N0+1 to N1 do   \expand range
                        [ChOut(0,^,);  IntOut(0,I)];
                if Char=^, then [GetCh;  ChOut(0,^,)] else quit]
        other   quit;                   \must be 0 string terminator
        ];
CrLf(0);
]
```


{{out}}

```txt

-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20

```



## Yabasic


```Yabasic
print RangeExpand$("-6,-3--1,3-5,7-11,14,15,17-20")

sub RangeExpand$(s$)
    local w$(1), n, i, r$, p, a, b
    
    n = token(s$, w$(), ",")
    
    for i = 1 to n
        p = instr(w$(i), "-", 2)
        if p then
            a = val(left$(w$(i), p-1))
            b = val(right$(w$(i), len(w$(i)) - p))
            repeat
                r$ = r$ + str$(a) + ","
                a = a + 1
            until(a > b)
        else
            r$ = r$ + w$(i) + ","
        end if
    next
    return left$(r$, len(r$) - 1)
end sub

```



## zkl


```zkl
fcn rangex(s){
   fcn(s,re){
      if (re.search(s)){
	 a,b:=re.matched[1,*].apply("toInt");
	 [a..b].walk();
      }
      else s;
   } :
   s.split(",").pump(List, _.fp1(RegExp(0'|(.*\d+)-(.*\d+)|)))
      .flatten().concat(",");
}
```


```zkl
ns="-6,-3-1,3-5,7-11,14,15,17-20";
rangex(ns).println();

ns="-6,-3--1,3-5,7-11,14,15,17-20";
rangex(ns).println();
```


{{out}}

```txt

-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
-6,-3,-2,-1,3,4,5,7,8,9,10,11,14,15,17,18,19,20

```


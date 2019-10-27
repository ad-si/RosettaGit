+++
title = "Commatizing numbers"
description = ""
date = 2019-03-19T13:42:37Z
aliases = []
[extra]
id = 17481
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}

''Commatizing''   numbers (as used here, a handy expedient made-up word) is the act of adding commas to a number (or string), or the numeric part of a larger string.


;Task:
Write a function that takes a string as an argument with optional arguments or parameters (the format of parameters/options is left to the programmer) that in general, adds commas (or some
other characters, including blanks or tabs) to the first numeric part of a string (if it's suitable for commatizing as per the rules below), and returns that newly commatized string. 

Some of the commatizing rules (specified below) are arbitrary, but they'll be a part of this task requirements, if only to make the results consistent amongst national preferences and other disciplines.

The number may be part of a larger (non-numeric) string such as:
::::*   «US$1744 millions»       ──or──
::::*   ±25000 motes.



The string may possibly ''not'' have a number suitable for commatizing, so it should be untouched and ''no error generated''.

If any argument (option) is invalid, nothing is changed and no error ''need be'' generated (quiet execution, no fail execution).   Error message generation is optional.

The exponent part of a number is never commatized.   The following string isn't suitable for commatizing:   9.7e+12000

Leading zeroes are never commatized.   The string   0000000005714.882   after commatization is:   0000000005,714.882

Any   ''period''   (<big><b>.</b></big>)   in a number is assumed to be a   ''decimal point''.

The original string is never changed   ''except''   by the addition of commas   [or whatever character(s) is/are used for insertion], if at all.

To wit, the following should be preserved:

::*   leading signs ('''+''', '''-''')       ── even superfluous signs
::*   leading/trailing/embedded blanks, tabs, and other whitespace 
::*   the case (upper/lower) of the exponent indicator, e.g.:   4.8903d-002



Any exponent character(s) should be supported:
::::::*   1247e12
::::::*   57256.1D-4
::::::*   4444^60
::::::*   7500<b>∙</b>10**35
::::::*   8500x10**35
::::::*   +55000↑3
::::::*   1000**100
::::::*   2048²
::::::*   4096<sup>32</sup>
::::::*   10000pow(pi)


Numbers may be terminated with any non-digit character, including subscripts and/or superscript:   4142135624<sup>3</sup>   or   7320509076<sub>(base 24)</sub>.

The character(s) to be used for the comma can be specified, and may contain blanks, tabs, and other whitespace characters, as well as multiple characters.   The default is the comma (<big>''','''</big>) character.

The   ''period length''   can be specified   (sometimes referred to as "thousands" or "thousands separators").   The   ''period length''   can be defined as the length (or number) of the decimal digits between commas.   The default period length is   <big>3</big>.

::: E.G.:   in this example, the   ''period length''   is five:   56789,12340,14148

The location of where to start the scanning for the target field (the numeric part) should be able to be specified.   The default is   <big>1</big>.

The character strings below may be placed in a file (and read) or stored as simple strings within the program.


;Strings to be used as a minimum:
The value of   pi   (expressed in base 10)   should be separated with blanks every   '''5'''   places past the decimal point,

the Zimbabwe dollar amount should use a decimal point for the "comma" separator:

:*   pi=3.14159265358979323846264338327950288419716939937510582097494459231
:*   The author has two Z$100000000000000 Zimbabwe notes (100 trillion).
:*   "-in Aus$+1411.8millions"
:*   ===US$0017440 millions=== (in 2000 dollars)
:*   123.e8000 is pretty big.
:*   The land area of the earth is  57268900(29% of the surface) square miles.
:*   Ain't no numbers in this here words, nohow, no way, Jose.
:*   James was never known as  0000000007
:*   Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.
:*   ␢␢␢$-140000±100  millions.
:*   6/9/1946 was a good year for some.

where the penultimate string has three leading blanks   (real blanks are to be used).


;Also see:
* The Wiki entry:   [http://en.wikipedia.org/wiki/Eddington_number Arthur Eddington's number of protons in the universe]. 




## ALGOL 68


```algol68
# returns text commatized according to the rules of the task and the      #
#         period, location and separator paramters                        #
PROC commatize = ( STRING text, INT location, INT period, STRING separator )STRING:
     IF  STRING str := text[ AT 1 ];
         # handle the options                                             #
         INT    start position   := IF location  = 0  THEN  1  ELSE location  FI;
         INT    period length    := IF period    = 0  THEN  3  ELSE period    FI;
         STRING separator string := IF separator = "" THEN "," ELSE separator FI;
         period length < 1 OR start position < 1 OR start position > UPB str
     THEN
         # invalid parameters - return the text unchanged                 #
         text
     ELIF # attempt to find a non-zero digit                              #
          INT number pos := start position;
          WHILE IF number pos > UPB str
                THEN FALSE
                ELSE str[ number pos ] < "1" OR str[ number pos ] > "9"
                FI
          DO
              number pos +:= 1
          OD;
          number pos > UPB str
     THEN # no digits in the string - return the text unchanged           #
          text
     ELSE # have at least one digit                                       #
          STRING result := str[ 1 : number pos - 1 ];
          # find the final digit                                          #
          INT number end := number pos;
          WHILE IF number end >= UPB str
                THEN FALSE
                ELSE str[ number end + 1 ] >= "0" AND str[ number end + 1 ] <= "9"
                FI
          DO
              number end +:= 1
          OD;
          # copy the digits commatizing as required                       #
          INT   digit count := ( number end - number pos ) + 1;
          WHILE digit count > 1 DO
              result      +:= str[ number pos ];
              number pos  +:= 1;
              digit count -:= 1;
              IF digit count MOD period length = 0 THEN
                  # need a comma after this digit                         #
                  result +:= separator string
              FI
          OD;
          # final digit and the rest of the string                        #
          result +:= str[ number pos : ];
          result
     FI # commatize # ;

# modes and operators to allow us to specify optional parameters to the   #
# commatizing procedure                                                   #
MODE COMMATIZINGOPTIONS = STRUCT( STRING text, INT location, INT period, STRING separator );
PRIO LOCATION  = 9;
OP   LOCATION  = ( STRING text, INT location )COMMATIZINGOPTIONS:     COMMATIZINGOPTIONS( text, location, 0, "" );
PRIO PERIOD    = 9;
OP   PERIOD    = ( STRING text, INT period   )COMMATIZINGOPTIONS:     COMMATIZINGOPTIONS( text, 0, period, "" );
PRIO SEPARATOR = 9;
OP   SEPARATOR = ( STRING text, CHAR   separator )COMMATIZINGOPTIONS: COMMATIZINGOPTIONS( text, 0, 0, separator );
OP   SEPARATOR = ( STRING text, STRING separator )COMMATIZINGOPTIONS: COMMATIZINGOPTIONS( text, 0, 0, separator );
OP   LOCATION  = ( COMMATIZINGOPTIONS opts, INT location     )COMMATIZINGOPTIONS:
                 COMMATIZINGOPTIONS( text OF opts, location, period OF opts, separator OF opts );
OP   PERIOD    = ( COMMATIZINGOPTIONS opts, INT period       )COMMATIZINGOPTIONS:
                 COMMATIZINGOPTIONS( text OF opts, location OF opts, period, separator OF opts );
OP   SEPARATOR = ( COMMATIZINGOPTIONS opts, CHAR   separator )COMMATIZINGOPTIONS:
                 COMMATIZINGOPTIONS( text OF opts, location OF opts, period OF opts, separator );
OP   SEPARATOR = ( COMMATIZINGOPTIONS opts, STRING separator )COMMATIZINGOPTIONS:
                 COMMATIZINGOPTIONS( text OF opts, location OF opts, period OF opts, separator );
OP   COMMATIZE = ( STRING text             )STRING: commatize( text, 0, 0, "" );
OP   COMMATIZE = ( COMMATIZINGOPTIONS opts )STRING:
                 commatize( text OF opts, location OF opts, period OF opts, separator OF opts );

# test the commatization procedure and operators                          #
print( ( COMMATIZE( "pi=3.14159265358979323846264338327950288419716939937510582097494459231" PERIOD 5 SEPARATOR " " LOCATION 6 ),
                                                                                                                       newline ) );
print( ( COMMATIZE( "The author has two Z$100000000000000 Zimbabwe notes (100 trillion)."             SEPARATOR "." ), newline ) );
print( ( COMMATIZE  """-in Aus$+1411.8millions""",                                                                     newline ) );
print( ( COMMATIZE  "===US$0017440 millions=== (in 2000 dollars)",                                                     newline ) );
print( ( COMMATIZE  "123.e8000 is pretty big.",                                                                        newline ) );
print( ( COMMATIZE  "The land area of the earth is 57268900(29% of the surface) square miles.",                        newline ) );
print( ( COMMATIZE  "Ain't no numbers in this here words, nohow, no way, Jose.",                                       newline ) );
print( ( COMMATIZE  "James was never known as 0000000007",                                                             newline ) );
print( ( COMMATIZE  "Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.",
                                                                                                                       newline ) );
print( ( COMMATIZE  "   $-140000±100 millions.",                                                                       newline ) );
print( ( COMMATIZE  "6/9/1946 was a good year for some.",                                                              newline ) )
```

{{out}}

```txt

pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
"-in Aus$+1,411.8millions"
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is 57,268,900(29% of the surface) square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as 0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100 millions.
6/9/1946 was a good year for some.

```



## C#


```csharp

static string[] inputs = {
	"pi=3.14159265358979323846264338327950288419716939937510582097494459231",
	"The author has two Z$100000000000000 Zimbabwe notes (100 trillion).",
	"\"-in Aus$+1411.8millions\"",
	"===US$0017440 millions=== (in 2000 dollars)"
};

void Main()
{
	inputs.Select(s => Commatize(s, 0, 3, ","))
              .ToList()
              .ForEach(Console.WriteLine);
}

string Commatize(string text, int startPosition, int interval, string separator)
{
	var matches = Regex.Matches(text.Substring(startPosition), "[0-9]*");
	var x = matches.Cast<Match>().Select(match => Commatize(match, interval, separator, text)).ToList();
	return string.Join("", x);
}


string Commatize(Match match, int interval, string separator, string original)
{
	if (match.Length <= interval)
		return original.Substring(match.Index, 
                match.Index == original.Length ? 0 : Math.Max(match.Length, 1));
	
	return string.Join(separator, match.Value.Split(interval));
}

public static class Extension
{
	public static string[] Split(this string source, int interval)
	{
		return SplitImpl(source, interval).ToArray();
	}
	
	static IEnumerable<string>SplitImpl(string source, int interval)
	{
		for	(int i = 1; i < source.Length; i++)
		{
			if (i % interval != 0) continue;
			
			yield return source.Substring(i - interval, interval);
		}
	}
}

```



## D

Better to have more tests than more features.

```d
import std.stdio, std.regex, std.range;

auto commatize(in char[] txt, in uint start=0, in uint step=3,
        in string ins=",") @safe
in {
    assert(step > 0);
} body {
    if (start > txt.length || step > txt.length)
        return txt;

    // First number may begin with digit or decimal point. Exponents ignored.
    enum decFloField = ctRegex!("[0-9]*\\.[0-9]+|[0-9]+");

    auto matchDec = matchFirst(txt[start .. $], decFloField);
    if (!matchDec)
        return txt;

    // Within a decimal float field:
    // A decimal integer field to commatize is positive and not after a point.
    enum decIntField = ctRegex!("(?<=\\.)|[1-9][0-9]*");
    // A decimal fractional field is preceded by a point, and is only digits.
    enum decFracField = ctRegex!("(?<=\\.)[0-9]+");

    return txt[0 .. start] ~ matchDec.pre ~ matchDec.hit
        .replace!(m => m.hit.retro.chunks(step).join(ins).retro)(decIntField)
        .replace!(m => m.hit.chunks(step).join(ins))(decFracField)
        ~ matchDec.post;
}

unittest {
    // An attempted solution may have one or more of the following errors:
    //    ignoring a number that has only zero before its decimal point
    assert("0.0123456".commatize == "0.012,345,6");
    //    commatizing numbers other than the first 
    assert("1000 2.3000".commatize == "1,000 2.3000");
    //    only commatizing in one direction from the decimal point
    assert("0001123.456789".commatize == "0001,123.456,789");
    //    detecting prefixes such as "Z$" requires detecting other prefixes
    assert(" NZ$300000".commatize == " NZ$300,000");
    //    detecting a decimal field that isn't attached to the first number
    assert(" 2600 and .0125".commatize == " 2,600 and .0125");
    //    ignoring the start value, or confusing base 0 (used here) with base 1
    assert("1 77000".commatize(1) == "1 77,000");
    //    ignoring a number that begins with a point, or treating it as integer
    assert(" .0104004".commatize == " .010,400,4");   
}

void main() {
    "pi=3.14159265358979323846264338327950288419716939937510582097494459231"
        .commatize(0, 5, " ").writeln;
    "The author has two Z$100000000000000 Zimbabwe notes (100 trillion)."
        .commatize(0, 3, ".").writeln;
    foreach (const line; "commatizing_numbers_using_defaults.txt".File.byLine)
        line.commatize.writeln;
}
```

{{out}}

```txt
pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
"-in Aus$+1,411.8millions"
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is 57,268,900(29% of the surface) square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as 0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100 millions.
6/9/1946 was a good year for some.
```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "regexp"
    "strings"
)

var reg = regexp.MustCompile(`(\.[0-9]+|[1-9]([0-9]+)?(\.[0-9]+)?)`)

func reverse(s string) string {
    r := []rune(s)
    for i, j := 0, len(r)-1; i < len(r)/2; i, j = i+1, j-1 {
        r[i], r[j] = r[j], r[i]
    }
    return string(r)
}

func commatize(s string, startIndex, period int, sep string) string {
    if startIndex < 0 || startIndex >= len(s) || period < 1 || sep == "" {
        return s
    }
    m := reg.FindString(s[startIndex:]) // this can only contain ASCII characters
    if m == "" {
        return s
    }
    splits := strings.Split(m, ".")
    ip := splits[0]
    if len(ip) > period {
        pi := reverse(ip)
        for i := (len(ip) - 1) / period * period; i >= period; i -= period {
            pi = pi[:i] + sep + pi[i:]
        }
        ip = reverse(pi)
    }
    if strings.Contains(m, ".") {
        dp := splits[1]
        if len(dp) > period {
            for i := (len(dp) - 1) / period * period; i >= period; i -= period {
                dp = dp[:i] + sep + dp[i:]
            }
        }
        ip += "." + dp
    }
    return s[:startIndex] + strings.Replace(s[startIndex:], m, ip, 1)
}

func main() {
    tests := [...]string{
        "123456789.123456789",
        ".123456789",
        "57256.1D-4",
        "pi=3.14159265358979323846264338327950288419716939937510582097494459231",
        "The author has two Z$100000000000000 Zimbabwe notes (100 trillion).",
        "-in Aus$+1411.8millions",
        "===US$0017440 millions=== (in 2000 dollars)",
        "123.e8000 is pretty big.",
        "The land area of the earth is 57268900(29% of the surface) square miles.",
        "Ain't no numbers in this here words, nohow, no way, Jose.",
        "James was never known as 0000000007",
        "Arthur Eddington wrote: I believe there are " +
            "15747724136275002577605653961181555468044717914527116709366231425076185631031296" +
            " protons in the universe.",
        "   $-140000±100 millions.",
        "6/9/1946 was a good year for some.",
    }
    fmt.Println(commatize(tests[0], 0, 2, "*"))
    fmt.Println(commatize(tests[1], 0, 3, "-"))
    fmt.Println(commatize(tests[2], 0, 4, "__"))
    fmt.Println(commatize(tests[3], 0, 5, " "))
    fmt.Println(commatize(tests[4], 0, 3, "."))
    for _, test := range tests[5:] {
        fmt.Println(commatize(test, 0, 3, ","))
    }
}
```


{{out}}

```txt

1*23*45*67*89.12*34*56*78*9
.123-456-789
5__7256.1D-4
pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
-in Aus$+1,411.8millions
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is 57,268,900(29% of the surface) square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as 0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100 millions.
6/9/1946 was a good year for some.

```



## J

These rules are relatively baroque, which demands long names and minimally complex statements, thus:


```J
require'regex'
commatize=:3 :0"1 L:1 0
  (i.0) commatize y
:
NB. deal with all those rules about options
  opts=. boxopen x
  char=. (#~ ' '&=@{.@(0&#)@>) opts
  num=. ;opts-.char
  delim=. 0 {:: char,<','
  'begin period'=. _1 0+2{.num,(#num)}.1 3
NB. initialize
  prefix=. begin {.y
  text=. begin }. y
NB. process
  'start len'=. ,'[1-9][0-9]*' rxmatch text
  if.0=len do. y return. end.
  number=. (start,:len) [;.0 text
  numb=. (>:period|<:#number){.number
  fixed=. numb,;delim&,each (-period)<\ (#numb)}.number
  prefix,(start{.text),fixed,(start+len)}.text
)
```


In use, this might look like:


```J
   (5;5;' ') commatize 'pi=3.14159265358979323846264338327950288419716939937510582097494459231'
pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
   '.' commatize 'The author has two Z$100000000000000 Zimbabwe notes (100 trillion).'
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
   commatize '-in Aus$+1411.8millions'
-in Aus$+1,411.8millions
   commatize '===US$0017440 millions=== (in 2000 dollars)'
===US$0017,440 millions=== (in 2000 dollars)
   commatize '123.e8000 is pretty big.'
123.e8000 is pretty big.
   commatize 'The land area of the earth is  57268900(29% of the surface)  square miles.'
The land area of the earth is  57,268,900(29% of the surface)  square miles.
   commatize 'Ain''t no numbers in this here words, nohow, no way, Jose.'
Ain't no numbers in this here words, nohow, no way, Jose.
   commatize 'James was never known as  0000000007'
James was never known as  0000000007
   commatize 'Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.'
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   commatize '   $-140000±100  millions.'
   $-140,000±100  millions.
   commatize '6/9/1946 was a good year for some.'
6/9/1946 was a good year for some.
```



## Java


```java
import java.io.File;
import java.util.*;
import java.util.regex.*;

public class CommatizingNumbers {

    public static void main(String[] args) throws Exception {
        commatize("pi=3.14159265358979323846264338327950288419716939937510582"
                + "097494459231", 6, 5, " ");

        commatize("The author has two Z$100000000000000 Zimbabwe notes (100 "
                + "trillion).", 0, 3, ".");

        try (Scanner sc = new Scanner(new File("input.txt"))) {
            while(sc.hasNext())
                commatize(sc.nextLine());
        }
    }

    static void commatize(String s) {
        commatize(s, 0, 3, ",");
    }

    static void commatize(String s, int start, int step, String ins) {
        if (start < 0 || start > s.length() || step < 1 || step > s.length())
            return;

        Matcher m = Pattern.compile("([1-9][0-9]*)").matcher(s.substring(start));
        StringBuffer result = new StringBuffer(s.substring(0, start));

        if (m.find()) {
            StringBuilder sb = new StringBuilder(m.group(1)).reverse();
            for (int i = step; i < sb.length(); i += step)
                sb.insert(i++, ins);
            m.appendReplacement(result, sb.reverse().toString());
        }

        System.out.println(m.appendTail(result));
    }
}
```



```txt
pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
"-in Aus$+1,411.8millions"
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is 57,268,900(29% of the surface) square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as 0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100 millions.
6/9/1946 was a good year for some.
```




## Julia

{{trans|Perl}}

```julia
input = [
    ["pi=3.14159265358979323846264338327950288419716939937510582097494459231", " ", 5],
    [raw"The author has two Z$100000000000000 Zimbabwe notes (100 trillion).", "."],
    [raw"-in Aus$+1411.8millions"],
    [raw"===US$0017440 millions=== (in 2000 dollars)"],
    ["123.e8000 is pretty big."],
    ["The land area of the earth is  57268900(29% of the surface)  square miles."],
    ["Ain\'t no numbers in this here words, nohow, no way, Jose."],
    ["James was never known as  0000000007"],
    ["Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe."],
    [raw"   $-140000±100  millions."],
    ["6/9/1946 was a good year for some."]]

function commatize(tst)
    grouping = (length(tst) == 3) ? tst[3] : 3
    sep = (length(tst) > 1) ? tst[2] : ","
    rmend(s) = replace(s, Regex("$sep\\Z") =>"")
    greg = Regex(".{$grouping}")
    cins(str) = reverse(rmend(replace(reverse(str), greg => s -> s * sep)))
    mat = match(Regex("(?<![eE\\/])([1-9]\\d{$grouping,})"), tst[1])
    if mat != nothing
        return replace(tst[1], mat.match => cins)
    end
    return tst[1]
end

for tst in input
    println(commatize(tst))
end

```
 {{output}} 
```txt

pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
-in Aus$+1,411.8millions
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is  57,268,900(29% of the surface)  square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as  0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100  millions.
6/9/1946 was a good year for some.

```



## Kotlin


```scala
// version 1.1.4-3

val r = Regex("""(\.[0-9]+|[1-9]([0-9]+)?(\.[0-9]+)?)""")

fun String.commatize(startIndex: Int = 0, period: Int = 3, sep: String = ","): String {
    if ((startIndex !in 0 until this.length) || period < 1 || sep == "") return this
    val m = r.find(this, startIndex)
    if (m == null) return this
    val splits = m.value.split('.')
    var ip = splits[0]
    if (ip.length > period) {       
        val sb = StringBuilder(ip.reversed())
        for (i in (ip.length - 1) / period * period downTo period step period) {
            sb.insert(i, sep)
        }
        ip = sb.toString().reversed()
    }
    if ('.' in m.value) { 
        var dp = splits[1]
        if (dp.length > period) {
            val sb2 = StringBuilder(dp)
            for (i in (dp.length - 1) / period * period downTo period step period) {
                sb2.insert(i, sep)
            }
            dp = sb2.toString()
        }
        ip += "." + dp
    } 
    return this.take(startIndex) + this.drop(startIndex).replaceFirst(m.value, ip)
}

fun main(args: Array<String>) {
    val tests = arrayOf(
        "123456789.123456789",
        ".123456789",
        "57256.1D-4",
        "pi=3.14159265358979323846264338327950288419716939937510582097494459231",
        "The author has two Z$100000000000000 Zimbabwe notes (100 trillion).",
        "-in Aus$+1411.8millions",
        "===US$0017440 millions=== (in 2000 dollars)",
        "123.e8000 is pretty big.",
        "The land area of the earth is 57268900(29% of the surface) square miles.",
        "Ain't no numbers in this here words, nohow, no way, Jose.",
        "James was never known as 0000000007",
        "Arthur Eddington wrote: I believe there are " + 
        "15747724136275002577605653961181555468044717914527116709366231425076185631031296" +     
        " protons in the universe.",
        "   $-140000±100 millions.",
        "6/9/1946 was a good year for some."        
    )

    println(tests[0].commatize(period = 2, sep = "*"))
    println(tests[1].commatize(period = 3, sep = "-"))
    println(tests[2].commatize(period = 4, sep = "__"))   
    println(tests[3].commatize(period = 5, sep = " "))
    println(tests[4].commatize(sep = "."))
    for (test in tests.drop(5)) println(test.commatize())
}
```


{{out}}

```txt

1*23*45*67*89.12*34*56*78*9
.123-456-789
5__7256.1D-4
pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
-in Aus$+1,411.8millions
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is 57,268,900(29% of the surface) square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as 0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100 millions.
6/9/1946 was a good year for some.

```



## Perl

Displaying before/after only when changes applied.

```perl
@input = (
    ['pi=3.14159265358979323846264338327950288419716939937510582097494459231', ' ', 5],
    ['The author has two Z$100000000000000 Zimbabwe notes (100 trillion).', '.'],
    ['-in Aus$+1411.8millions'],
    ['===US$0017440 millions=== (in 2000 dollars)'],
    ['123.e8000 is pretty big.'],
    ['The land area of the earth is  57268900(29% of the surface)  square miles.'],
    ['Ain\'t no numbers in this here words, nohow, no way, Jose.'],
    ['James was never known as  0000000007'],
    ['Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.'],
    ['   $-140000±100  millions.'],
    ['5/9/1946 was a good year for some.']
);

for $i (@input) {
    $old = @$i[0];
    $new = commatize(@$i);
    printf("%s\n%s\n\n", $old, $new) if $old ne $new;
}

sub commatize {
    my($str,$sep,$by) = @_;
    $sep = ',' unless $sep;
    $by  = 3   unless $by;

    $str =~ s/                      # matching rules:
            (?<![eE\/])             #   not following these characters
            ([1-9]\d{$by,})         #   leading non-zero digit, minimum number of digits required
            /c_ins($1,$by,$sep)/ex; # substitute matched text with subroutine output
    return $str;
}

sub c_ins {
    my($s,$by,$sep) = @_;
    ($c = reverse $s) =~ s/(.{$by})/$1$sep/g;
    $c =~ s/$sep$//;
    return reverse $c;
}
```

{{out}}

```txt
pi=3.14159265358979323846264338327950288419716939937510582097494459231
pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231

The author has two Z$100000000000000 Zimbabwe notes (100 trillion).
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).

-in Aus$+1411.8millions
-in Aus$+1,411.8millions

===US$0017440 millions=== (in 2000 dollars)
===US$0017,440 millions=== (in 2000 dollars)

The land area of the earth is  57268900(29% of the surface)  square miles.
The land area of the earth is  57,268,900(29% of the surface)  square miles.

Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.

   $-140000±100  millions.
   $-140,000±100  millions.
```



## Perl 6


```perl6
for ('pi=3.14159265358979323846264338327950288419716939937510582097494459231', {:6at, :5by, :ins(' ')}),
    ('The author has two Z$100000000000000 Zimbabwe notes (100 trillion).', {:ins<.>}),
    '-in Aus$+1411.8millions',
    '===US$0017440 millions=== (in 2000 dollars)',
    '123.e8000 is pretty big.',
    'The land area of the earth is  57268900(29% of the surface)  square miles.',
    'Ain\'t no numbers in this here words, nohow, no way, Jose.',
    'James was never known as  0000000007',
    'Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.',
    '   $-140000±100  millions.',
    '6/9/1946 was a good year for some.'
    {
        say "Before: ", .[0];
        say " After: ", .[1] ?? .[0].&commatize( |.[1] ) !! .&commatize;
    }
 
sub commatize($s, :$at = 0, :$ins = ',', :$by = 3) {
    $s.subst: :continue($at), :1st, / <[1..9]> <[0..9]>* /,
        *.flip.comb(/<{ ".**1..$by" }>/).join($ins).flip;
}
```

{{out}}

```txt
Before: pi=3.14159265358979323846264338327950288419716939937510582097494459231
 After: pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
Before: The author has two Z$100000000000000 Zimbabwe notes (100 trillion).
 After: The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
Before: -in Aus$+1411.8millions
 After: -in Aus$+1,411.8millions
Before: ===US$0017440 millions=== (in 2000 dollars)
 After: ===US$0017,440 millions=== (in 2000 dollars)
Before: 123.e8000 is pretty big.
 After: 123.e8000 is pretty big.
Before: The land area of the earth is  57268900(29% of the surface)  square miles.
 After: The land area of the earth is  57,268,900(29% of the surface)  square miles.
Before: Ain't no numbers in this here words, nohow, no way, Jose.
 After: Ain't no numbers in this here words, nohow, no way, Jose.
Before: James was never known as  0000000007
 After: James was never known as  0000000007
Before: Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.
 After: Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
Before:    $-140000±100  millions.
 After:    $-140,000±100  millions.
Before: 6/9/1946 was a good year for some.
 After: 6/9/1946 was a good year for some.
```



## Phix


```Phix
procedure commatize(string s, string sep=",", integer start=1, integer step=3)
integer l = length(s)
    for i=start to l do
        if find(s[i],"123456789") then
            for j=i+1 to l+1 do
                if j>l or not find(s[j],"0123456789") then
                    for k=j-1-step to i by -step do
                        s[k+1..k] = sep
                    end for
                    exit
                end if
            end for
            exit
        end if
    end for
    printf(1,"%s\n",{s})
end procedure

commatize("pi=3.14159265358979323846264338327950288419716939937510582097494459231"," ",6,5)
commatize("The author has two Z$100000000000000 Zimbabwe notes (100 trillion).",".")
commatize("\"-in Aus$+1411.8millions\"")
commatize("===US$0017440 millions=== (in 2000 dollars)")
commatize("123.e8000 is pretty big.")
commatize("The land area of the earth is 57268900(29% of the surface) square miles.")
commatize("Ain't no numbers in this here words, nohow, no way, Jose.")
commatize("James was never known as 0000000007")
commatize("Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.")
commatize("   $-140000±100 millions.")
commatize("6/9/1946 was a good year for some.")
```

{{Out}}

```txt

pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
"-in Aus$+1,411.8millions"
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is 57,268,900(29% of the surface) square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as 0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100 millions.
6/9/1946 was a good year for some.

```



## Python


```Python

import re as RegEx


def Commatize( _string, _startPos=0, _periodLen=3, _separator="," ):
	outString = ""
	strPos = 0
	matches = RegEx.findall( "[0-9]*", _string )

	for match in matches[:-1]:
		if not match:
			outString += _string[ strPos ]
			strPos += 1
		else:
			if len(match) > _periodLen:
				leadIn = match[:_startPos]
				periods =  [ match [ i:i + _periodLen ] for i in range ( _startPos, len ( match ), _periodLen ) ]
				outString += leadIn + _separator.join( periods )
			else:
				outString += match

			strPos += len( match )

	return outString



print ( Commatize( "pi=3.14159265358979323846264338327950288419716939937510582097494459231", 0, 5, " " ) )
print ( Commatize( "The author has two Z$100000000000000 Zimbabwe notes (100 trillion).", 0, 3, "." ))
print ( Commatize( "\"-in Aus$+1411.8millions\"" ))
print ( Commatize( "===US$0017440 millions=== (in 2000 dollars)" ))
print ( Commatize( "123.e8000 is pretty big." ))
print ( Commatize( "The land area of the earth is 57268900(29% of the surface) square miles." ))
print ( Commatize( "Ain't no numbers in this here words, nohow, no way, Jose." ))
print ( Commatize( "James was never known as 0000000007" ))
print ( Commatize( "Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe." ))
print ( Commatize( "␢␢␢$-140000±100 millions." ))
print ( Commatize( "6/9/1946 was a good year for some." ))

```



## Racket



Note that the post-number part of the split catches the date in <code>6/9/1946</code> (I think that's
desirable) but <code>1946-09-06</code> would still be commatized as <code>1,946-09-06</code> -- it
would take date recognition to capture that case.

All tests pass (so it's as good as Perl, I guess).


```racket
#lang racket
(require (only-in srfi/13 [string-reverse gnirts]))

;; returns a string with the "comma"s inserted every step characters from the RIGHT of n.
;; because of the right handedness of this, there is a lot of reversal going on
(define ((insert-commas comma step) n)
  (define px (pregexp (format ".{1,~a}" step)))
  (string-join (add-between (reverse (map gnirts (regexp-match* px (gnirts n)))) comma) ""))

(define (commatize s #:start (start 0) #:comma (comma ",") #:step (step 3))
  (define ins-comms (insert-commas comma step)) ; specific to our comma and step
  
  (define split-into-numbers
    (match-lambda
      [(regexp
        #px"^([^1-9]*)([1-9][0-9.]*)(\\S*)(.*)$" ; see below for description of bits
        (list _                               ; the whole match
              (app split-into-numbers pre)    ; recur on left
              num                             ; the number bit before any exponent or other
                                              ; interestingness
              post-number                     ; from exponent to the first space
              (app split-into-numbers post))) ; recur on right
       (define skip (substring num 0 start))
       (match-define
         (regexp #px"^(.*?)(\\..*)?$"
                 (list _                      ; whole match
                       (app ins-comms n)      ; the bit that gets the commas added
                       (or (? string? d)      ; if it matches, then the raw string is in d
                           (and #f (app (lambda (f) "") d))))) ; if (...)? doesn't match it returns
                                                               ; #f which we thunk to an empty string
         (substring num start))                       ; do the match on the unskipped bit
       (string-append pre skip n d post-number post)] ; stitch it back together
      [else else]))                                   ; if it doesn't match leave as is
  
  ;; kick it off
  (split-into-numbers s))

(module+ test
  (require tests/eli-tester)
  
  (test
   (commatize "pi=3.14159265358979323846264338327950288419716939937510582097494459231"
              #:start 6 #:comma " " #:step 5)
   =>"pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231"
   
   (commatize "The author has two Z$100000000000000 Zimbabwe notes (100 trillion)." #:comma ".")
   =>"The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion)."
   
   (commatize "-in Aus$+1411.8millions")
   =>"-in Aus$+1,411.8millions"
   
   (commatize "===US$0017440 millions=== (in 2000 dollars)")
   =>"===US$0017,440 millions=== (in 2,000 dollars)"
   
   (commatize "123.e8000 is pretty big.")
   =>"123.e8000 is pretty big."
   
   (commatize "The land area of the earth is  57268900(29% of the surface)  square miles.")
   =>"The land area of the earth is  57,268,900(29% of the surface)  square miles."
   
   (commatize "Ain't no numbers in this here words, nohow, no way, Jose.")
   =>"Ain't no numbers in this here words, nohow, no way, Jose."
   
   (commatize "James was never known as  0000000007")
   =>"James was never known as  0000000007"
   
   (commatize "Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.")
   =>"Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe."
   
   (commatize "   $-140000±100  millions.")
   
   =>"   $-140,000±100  millions."
   (commatize "6/9/1946 was a good year for some.")
   =>"6/9/1946 was a good year for some."))

```



## REXX

The hardest part of the   '''comma'''   function is to locate where a   ''usable''   number starts and ends.

```rexx
/*REXX program add commas (or other chars)  to a number within a string (or a char str).*/
@. =
@.1= "pi=3.14159265358979323846264338327950288419716939937510582097494459231"
@.2= "The author has two Z$100000000000000 Zimbabwe notes (100 trillion)."
@.3= "-in Aus$+1411.8millions"
@.4= "===US$0017440 millions=== (in 2000 dollars)"
@.5= "123.e8000 is pretty big."
@.6= "The land area of the earth is  57268900(29% of the surface)  square miles."
@.7= "Ain't no numbers in this here words, nohow, no way, Jose."
@.8= "James was never known as  0000000007"
@.9= "Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe."
@.10= "   $-140000±100  millions."
@.11= "6/9/1946 was a good year for some."

       do i=1  while  @.i\=='';               if i\==1  then say  /*process each string.*/
                     say 'before──►'@.i                           /*show the before str.*/
       if i==1  then say ' after──►'comma(@.i, 'blank', 5, , 6)   /*   p=5,  start=6.   */
       if i==2  then say ' after──►'comma(@.i, ".")               /*comma=decimal point.*/
       if i>2   then say ' after──►'comma(@.i)                    /*use the defaults.   */
       end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
comma: procedure;  parse arg _,c,p,t,s           /*obtain the number & optional options.*/
       arg ,cU .                                 /*obtain an uppercase version of  C.   */
       c=word(c ',', 1)                          /*obtain the  commatizing character(s).*/
       if cU=='BLANK'  then c=' '                /*special case for a "blank" separator.*/
       o=word(p 3, 1)                            /*obtain the optional period length.   */
       p=abs(o)                                  /*obtain the positive period length.   */
       t=word(t 999999999, 1)                    /*obtain max # of "commas" to insert.  */
       s=word(s 1, 1)                            /*obtain the optional  start  position.*/

       if \datatype(p, 'W')   |   \datatype(t, "W")   |   \datatype(s, 'W')    | ,
          t<1  |  s<1  |  p==0  |  arg()>5   then return _       /*any invalid options? */

       n=_'.9';     #=123456789;      k=0        /*define some  handy-dandy  variables. */

       if o<0  then do                           /*using a  negative  period length ?   */
                        b=verify(_, ' ', , s)        /*position of first  blank  in string. */
                        e=length(_)  -  verify( reverse(_), ' ')  + 1 - p
                    end
               else do                           /*using a  positive  period length.    */
                        b=verify(n, #, 'M', s)       /*position of first useable decimal dig*/
                        z=max(1, verify(n, #'0.', "M", s))  /*    "     "  last    "       "*/
                        e=verify(n, #'0', ,  max(1, verify(n, #"0.", 'M', s) ) )   - p - 1
                    end

       if e>0 & b>0  then do j=e  to b  by -p  while  k<t        /*commatize the digits.*/
                          _=insert(c, _, j)                      /*comma spray  ───►  #.*/
                          k= k + 1                               /*bump the commatizing.*/
                          end   /*j*/
       return _
```

{{out|output|text=  when using the internal default inputs:}}

```txt

before──►pi=3.14159265358979323846264338327950288419716939937510582097494459231
 after──►pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231

before──►The author has two Z$100000000000000 Zimbabwe notes (100 trillion).
 after──►The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).

before──►-in Aus$+1411.8millions
 after──►-in Aus$+1,411.8millions

before──►===US$0017440 millions=== (in 2000 dollars)
 after──►===US$0017,440 millions=== (in 2000 dollars)

before──►123.e8000 is pretty big.
 after──►123.e8000 is pretty big.

before──►The land area of the earth is  57268900(29% of the surface)  square miles.
 after──►The land area of the earth is  57,268,900(29% of the surface)  square miles.

before──►Ain't no numbers in this here words, nohow, no way, Jose.
 after──►Ain't no numbers in this here words, nohow, no way, Jose.

before──►James was never known as  0000000007
 after──►James was never known as  0000000007

before──►Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe.
 after──►Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.

before──►   $-140000±100  millions.
 after──►   $-140,000±100  millions.

before──►6/9/1946 was a good year for some.
 after──►6/9/1946 was a good year for some.

```



## Scala

===Java-ish version===

```Scala
import java.io.File
import java.util.Scanner
import java.util.regex.Pattern

object CommatizingNumbers extends App {

  def commatize(s: String): Unit = commatize(s, 0, 3, ",")

  def commatize(s: String, start: Int, step: Int, ins: String): Unit = {
    if (start >= 0 && start <= s.length && step >= 1 && step <= s.length) {
      val m = Pattern.compile("([1-9][0-9]*)").matcher(s.substring(start))
      val result = new StringBuffer(s.substring(0, start))
      if (m.find) {
        val sb = new StringBuilder(m.group(1)).reverse
        for (i <- step until sb.length by step) sb.insert(i, ins)
        m.appendReplacement(result, sb.reverse.toString)
      }
      println(m.appendTail(result))
    }
  }

  commatize("pi=3.14159265358979323846264338327950288419716939937510582" + "097494459231", 6, 5, " ")
  commatize("The author has two Z$100000000000000 Zimbabwe notes (100 " + "trillion).", 0, 3, ".")

  val sc = new Scanner(new File("input.txt"))
  while (sc.hasNext) commatize(sc.nextLine)
}
```



## Swift


{{trans|Kotlin}}


```Swift
import Foundation

extension String {
  private static let commaReg = try! NSRegularExpression(pattern: "(\\.[0-9]+|[1-9]([0-9]+)?(\\.[0-9]+)?)")

  public func commatize(start: Int = 0, period: Int = 3, separator: String = ",") -> String {
    guard separator != "" else {
      return self
    }

    let sep = Array(separator)
    let startIdx = index(startIndex, offsetBy: start)
    let matches = String.commaReg.matches(in: self, range: NSRange(startIdx..., in: self))

    guard !matches.isEmpty else {
      return self
    }

    let fullMatch = String(self[Range(matches.first!.range(at: 0), in: self)!])
    let splits = fullMatch.components(separatedBy: ".")
    var ip = splits[0]

    if ip.count > period {
      var builder = Array(ip.reversed())

      for i in stride(from: (ip.count - 1) / period * period, through: period, by: -period) {
        builder.insert(contentsOf: sep, at: i)
      }

      ip = String(builder.reversed())
    }

    if fullMatch.contains(".") {
      var dp = splits[1]

      if dp.count > period {
        var builder = Array(dp)

        for i in stride(from: (dp.count - 1) / period * period, through: period, by: -period) {
          builder.insert(contentsOf: sep, at: i)
        }

        dp = String(builder)
      }

      ip += "." + dp
    }

    return String(prefix(start)) + String(dropFirst(start)).replacingOccurrences(of: fullMatch, with: ip)
  }
}

let tests = [
  "123456789.123456789",
  ".123456789",
  "57256.1D-4",
  "pi=3.14159265358979323846264338327950288419716939937510582097494459231",
  "The author has two Z$100000000000000 Zimbabwe notes (100 trillion).",
  "-in Aus$+1411.8millions",
  "===US$0017440 millions=== (in 2000 dollars)",
  "123.e8000 is pretty big.",
  "The land area of the earth is 57268900(29% of the surface) square miles.",
  "Ain't no numbers in this here words, nohow, no way, Jose.",
  "James was never known as 0000000007",
  "Arthur Eddington wrote: I believe there are " +
      "15747724136275002577605653961181555468044717914527116709366231425076185631031296" +
      " protons in the universe.",
  "   $-140000±100 millions.",
  "6/9/1946 was a good year for some."
]

print(tests[0].commatize(period: 2, separator: "*"))
print(tests[1].commatize(period: 3, separator: "-"))
print(tests[2].commatize(period: 4, separator: "__"))
print(tests[3].commatize(period: 5, separator: " "))
print(tests[4].commatize(separator: "."))

for testCase in tests.dropFirst(5) {
  print(testCase.commatize())
}
```


{{out}}


```txt
1*23*45*67*89.12*34*56*78*9
.123-456-789
5__7256.1D-4
pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
-in Aus$+1,411.8millions
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is 57,268,900(29% of the surface) square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as 0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100 millions.
6/9/1946 was a good year for some.
```



## VBA

{{trans|Phix}}

```vb
Public Sub commatize(s As String, Optional sep As String = ",", Optional start As Integer = 1, Optional step As Integer = 3)
    Dim l As Integer: l = Len(s)
        For i = start To l
            If Asc(Mid(s, i, 1)) >= Asc("1") And Asc(Mid(s, i, 1)) <= Asc("9") Then
                For j = i + 1 To l + 1
                    If j > l Then
                        For k = j - 1 - step To i Step -step
                            s = Mid(s, 1, k) & sep & Mid(s, k + 1, l - k + 1)
                            l = Len(s)
                        Next k
                        Exit For
                    Else
                        If (Asc(Mid(s, j, 1)) < Asc("0") Or Asc(Mid(s, j, 1)) > Asc("9")) Then
                            For k = j - 1 - step To i Step -step
                                s = Mid(s, 1, k) & sep & Mid(s, k + 1, l - k + 1)
                                l = Len(s)
                            Next k
                            Exit For
                        End If
                    End If
                Next j
                Exit For
            End If
        Next i
        Debug.Print s
    End Sub
Public Sub main()
    commatize "pi=3.14159265358979323846264338327950288419716939937510582097494459231", " ", 6, 5
    commatize "The author has two Z$100000000000000 Zimbabwe notes (100 trillion).", "."
    commatize """-in Aus$+1411.8millions"""
    commatize "===US$0017440 millions=== (in 2000 dollars)"
    commatize "123.e8000 is pretty big."
    commatize "The land area of the earth is 57268900(29% of the surface) square miles."
    commatize "Ain't no numbers in this here words, nohow, no way, Jose."
    commatize "James was never known as 0000000007"
    commatize "Arthur Eddington wrote: I believe there are 15747724136275002577605653961181555468044717914527116709366231425076185631031296 protons in the universe."
    commatize "   $-140000±100 millions."
    commatize "6/9/1946 was a good year for some."
End Sub
```
{{out}}

```txt
pi=3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59231
The author has two Z$100.000.000.000.000 Zimbabwe notes (100 trillion).
"-in Aus$+1,411.8millions"
===US$0017,440 millions=== (in 2000 dollars)
123.e8000 is pretty big.
The land area of the earth is 57,268,900(29% of the surface) square miles.
Ain't no numbers in this here words, nohow, no way, Jose.
James was never known as 0000000007
Arthur Eddington wrote: I believe there are 15,747,724,136,275,002,577,605,653,961,181,555,468,044,717,914,527,116,709,366,231,425,076,185,631,031,296 protons in the universe.
   $-140,000±100 millions.
6/9/1946 was a good year for some.
```


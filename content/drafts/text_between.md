+++
title = "Text between"
description = ""
date = 2019-10-21T18:27:44Z
aliases = []
[extra]
id = 21703
[taxonomies]
categories = []
tags = []
+++

{{draft task|Text between}}

;Task:
Get the text in a string that occurs between a start and end delimiter.
Programs will be given a search string, a start delimiter string, and an end delimiter string. The delimiters will not be unset, and will not be the empty string.

The value returned should be the text in the search string that occurs between the '''first''' occurrence of the start delimiter (starting after the text of the start delimiter) and the '''first''' occurrence of the end delimiter after that.

If the start delimiter is not present in the search string, a blank string should be returned.

If the end delimiter is not present after the end of the first occurrence of the start delimiter in the search string, the remainder of the search string after that point should be returned.

There are two special values for the delimiters. If the value of the start delimiter is "start", the beginning of the search string will be matched. If the value of the end delimiter is "end", the end of the search string will be matched.

Example 1. Both delimiters set


```txt

Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: " world"
Output: "Rosetta Code"

```


Example 2. Start delimiter is the start of the string


```txt

Text: "Hello Rosetta Code world"
Start delimiter: "start"
End delimiter: " world"
Output: "Hello Rosetta Code"

```


Example 3. End delimiter is the end of the string


```txt

Text: "Hello Rosetta Code world"
Start delimiter: "Hello"
End delimiter: "end"
Output: "Rosetta Code world"

```


Example 4. End delimiter appears before and after start delimiter


```txt

Text: "</div><div style=\"chinese\">你好嗎</div>"
Start delimiter: "<div style=\"chinese\">"
End delimiter: "</div>"
Output: "你好嗎"

```


Example 5. End delimiter not present


```txt

Text: "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"
Start delimiter: "<text>"
End delimiter: "<table>"
Output: "Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"

```


Example 6. Start delimiter not present


```txt

Text: "<table style=\"myTable\"><tr><td>hello world</td></tr></table>"
Start delimiter: "<table>"
End delimiter: "</table>"
Output: ""

```


Example 7. Multiple instances of end delimiter after start delimiter (match until the first one)


```txt

Text: "The quick brown fox jumps over the lazy other fox"
Start delimiter: "quick "
End delimiter: " fox"
Output: "brown"

```


Example 8. Multiple instances of the start delimiter (start matching at the first one)


```txt

Text: "One fish two fish red fish blue fish"
Start delimiter: "fish "
End delimiter: " red"
Output: "two fish"

```


Example 9. Start delimiter is end delimiter


```txt

Text: "FooBarBazFooBuxQuux"
Start delimiter: "Foo"
End delimiter: "Foo"
Output: "BarBaz"

```






## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}<br/>
Uses the Algol 68G specific string in string, for other compilers/interpreters, a version of string in string is here : [[ALGOL_68/prelude]].<br/>
As Algol 68 predates Unicode, the fourth example deviates from the task.

```algol68
BEGIN
    # some utility operators                                                 #
    # returns the length of a string                                         #
    OP   LENGTH = ( STRING a )INT: ( UPB a - LWB a ) + 1;
    # returns the position of s in t or UPB t + 1 if s is not present        #
    PRIO INDEXOF = 1;
    OP   INDEXOF = ( STRING t, STRING s )INT:
         IF INT pos; string in string( s, pos, t ) THEN pos ELSE UPB t + 1 FI;
    # returns the text after s in t or "" if s is not present                #
    PRIO AFTER   = 1;
    OP   AFTER   = ( STRING t, STRING s )STRING:
         IF INT pos = t INDEXOF s; pos > UPB t THEN "" ELSE t[ pos + LENGTH s : ] FI;
    # returns the text before s in t or t if s is not present                #
    PRIO BEFORE  = 1;
    OP   BEFORE  = ( STRING t, STRING s )STRING:
         IF INT pos = t INDEXOF s; pos > UPB t THEN t ELSE t[ : pos - 1 ] FI;

    # mode to hold a pair of STRINGs for the BETWEEN operator                #
    MODE STRINGPAIR = STRUCT( STRING left, right );
    # returns a STRINGPAIR composed of a and b (standard priority for AND)   #
    # with additional operators for CHARs as "a" is a CHAR denotation,       #
    # not a STRING of length 1                                               #
    OP   AND     = ( STRING a, STRING b )STRINGPAIR: (        a,         b );
    OP   AND     = ( STRING a, CHAR   b )STRINGPAIR: ( STRING(a),        b  );
    OP   AND     = ( CHAR   a, CHAR   b )STRINGPAIR: ( STRING(a), STRING(b) );
    OP   AND     = ( CHAR   a, STRING b )STRINGPAIR: (        a , STRING(b) );

    # traceing flag for BETWEEN - if TRUE, debug output is shown             #
    BOOL trace between := FALSE;
    # returns the text of s between the delimitors specified in d            #
    PRIO BETWEEN = 1;
    OP   BETWEEN = ( STRING s, STRINGPAIR d )STRING:
         BEGIN
            STRING result := s;
            IF  left OF d /= "start" THEN result := result AFTER   left OF d FI;
            IF right OF d /= "end"   THEN result := result BEFORE right OF d FI;
            IF trace between THEN
                # show debug output                                          #
                print( ( "Text: """,                     s, """", newline
                       , "Start delimiter: """,  left OF d, """", newline
                       , "End delimiter: """,   right OF d, """", newline
                       , "Output: """,              result, """", newline
                       ,                                          newline
                       )
                     )
            FI;
            result
         END # BETWEEN # ;

    # test cases                                                             #
    BEGIN
        STRING s;
        trace between := TRUE;
        s := "Hello Rosetta Code world" BETWEEN "Hello "  AND " world";
        s := "Hello Rosetta Code world" BETWEEN "start"   AND " world";
        s := "Hello Rosetta Code world" BETWEEN "Hello "  AND "end";
        s := "</div><div style=""french"">bonjour</div>"
                                        BETWEEN "<div style=""french"">"
                                                          AND "</div>";
        s := "<text>Hello <span>Rosetta Code</span> world</text><table style=""myTable"">"
                                        BETWEEN "<text>"  AND "<table>";
        s := "<table style=""myTable""><tr><td>hello world</td></tr></table>"
                                        BETWEEN "<table>" AND "</table>";
        s := "The quick brown fox jumps over the lazy other fox"
                                        BETWEEN "quick "  AND " fox";
        s := "One fish two fish red fish blue fish"
                                        BETWEEN "fish "   AND " red";
        s := "FooBarBazFooBuxQuux"      BETWEEN "Foo"     AND "Foo";
        trace between := FALSE
    END
END
```

{{out}}

```txt

Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: " world"
Output: "Rosetta Code"

Text: "Hello Rosetta Code world"
Start delimiter: "start"
End delimiter: " world"
Output: "Hello Rosetta Code"

Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: "end"
Output: "Rosetta Code world"

Text: "</div><div style="french">bonjour</div>"
Start delimiter: "<div style="french">"
End delimiter: "</div>"
Output: "bonjour"

Text: "<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">"
Start delimiter: "<text>"
End delimiter: "<table>"
Output: "Hello <span>Rosetta Code</span> world</text><table style="myTable">"

Text: "<table style="myTable"><tr><td>hello world</td></tr></table>"
Start delimiter: "<table>"
End delimiter: "</table>"
Output: ""

Text: "The quick brown fox jumps over the lazy other fox"
Start delimiter: "quick "
End delimiter: " fox"
Output: "brown"

Text: "One fish two fish red fish blue fish"
Start delimiter: "fish "
End delimiter: " red"
Output: "two fish"

Text: "FooBarBazFooBuxQuux"
Start delimiter: "Foo"
End delimiter: "Foo"
Output: "BarBaz"


```



## AppleScript


```applescript

my text_between("Hello Rosetta Code world", "Hello ", " world")

on text_between(this_text, start_text, end_text)
	set return_text to ""
	try
		if (start_text is not "start") then
			set AppleScript's text item delimiters to start_text
			set return_text to text items 2 thru end of this_text as string
		else
			set return_text to this_text
		end if
		if (end_text is not "end") then
			set AppleScript's text item delimiters to end_text
			set return_text to text item 1 of return_text as string
			set AppleScript's text item delimiters to ""
		end if
	end try
	set AppleScript's text item delimiters to ""

	return return_text
end text_between

```


## AWK


```AWK

# syntax: GAWK -f TEXT_BETWEEN.AWK
BEGIN {
    main("Hello Rosetta Code world","Hello "," world","1. Both delimiters set")
    main("Hello Rosetta Code world","start"," world","2. Start delimiter is the start of the string")
    main("Hello Rosetta Code world","Hello","end","3. End delimiter is the end of the string")
    main("</div><div style=\"chinese\">???</div>","<div style=\"chinese\">","</div>",
    "4. End delimiter appears before and after start delimiter")
    main("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">","<text>","<table>",
    "5. End delimiter not present")
    main("<table style=\"myTable\"><tr><td>hello world</td></tr></table>","<table>","</table>",
    "6. Start delimiter not present")
    main("The quick brown fox jumps over the lazy other fox","quick "," fox",
    "7. Multiple instances of end delimiter after start delimiter (match until the first one)")
    main("One fish two fish red fish blue fish","fish "," red",
    "8. Multiple instances of the start delimiter (start matching at the first one)")
    main("FooBarBazFooBuxQuux","Foo","Foo","9. Start delimiter is end delimiter")
    main("Hello Rosetta Code world","start","end","10. Start and end delimiters use special values")
    main("Hello Rosetta Code world","","x","11. Null start delimiter")
    main("Hello Rosetta Code world","x","","12. Null end delimiter")
    exit(0)
}
function main(text,sdelim,edelim,example,  pos,str) {
    printf("Example %s\n",example)
    printf("Text: '%s'\n",text)
    printf("sDelim: '%s'\n",sdelim)
    printf("eDelim: '%s'\n",edelim)
    if (sdelim == "" || edelim == "") {
      printf("error: null delimiter\n\n")
      return
    }
    if (sdelim == "start") {
      str = text
    }
    else {
      pos = index(text,sdelim)
      if (pos > 0) {
        str = substr(text,pos+length(sdelim))
      }
    }
    if (edelim == "end") {
    }
    else {
      pos = index(str,edelim)
      if (pos > 0) {
        str = substr(str,1,pos-1)
      }
    }
    printf("Output: '%s'\n\n",str)
}

```

{{out}}

```txt

Example 1. Both delimiters set
Text: 'Hello Rosetta Code world'
sDelim: 'Hello '
eDelim: ' world'
Output: 'Rosetta Code'

Example 2. Start delimiter is the start of the string
Text: 'Hello Rosetta Code world'
sDelim: 'start'
eDelim: ' world'
Output: 'Hello Rosetta Code'

Example 3. End delimiter is the end of the string
Text: 'Hello Rosetta Code world'
sDelim: 'Hello'
eDelim: 'end'
Output: ' Rosetta Code world'

Example 4. End delimiter appears before and after start delimiter
Text: '</div><div style="chinese">???</div>'
sDelim: '<div style="chinese">'
eDelim: '</div>'
Output: '???'

Example 5. End delimiter not present
Text: '<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">'
sDelim: '<text>'
eDelim: '<table>'
Output: 'Hello <span>Rosetta Code</span> world</text><table style="myTable">'

Example 6. Start delimiter not present
Text: '<table style="myTable"><tr><td>hello world</td></tr></table>'
sDelim: '<table>'
eDelim: '</table>'
Output: ''

Example 7. Multiple instances of end delimiter after start delimiter (match until the first one)
Text: 'The quick brown fox jumps over the lazy other fox'
sDelim: 'quick '
eDelim: ' fox'
Output: 'brown'

Example 8. Multiple instances of the start delimiter (start matching at the first one)
Text: 'One fish two fish red fish blue fish'
sDelim: 'fish '
eDelim: ' red'
Output: 'two fish'

Example 9. Start delimiter is end delimiter
Text: 'FooBarBazFooBuxQuux'
sDelim: 'Foo'
eDelim: 'Foo'
Output: 'BarBaz'

Example 10. Start and end delimiters use special values
Text: 'Hello Rosetta Code world'
sDelim: 'start'
eDelim: 'end'
Output: 'Hello Rosetta Code world'

Example 11. Null start delimiter
Text: 'Hello Rosetta Code world'
sDelim: ''
eDelim: 'x'
error: null delimiter

Example 12. Null end delimiter
Text: 'Hello Rosetta Code world'
sDelim: 'x'
eDelim: ''
error: null delimiter

```



## C


```c

/*
 * textBetween: Gets text between two delimiters
 */
char* textBetween(char* thisText, char* startText, char* endText, char* returnText)
{
	//printf("textBetween\n");

    char* startPointer = NULL;
    int stringLength = 0;

    char* endPointer = NULL;
    int endLength = 0;

	if (strstr(startText, "start") != NULL)
	{
		// Set the beginning of the string
		startPointer = thisText;
	} else {
		startPointer = strstr(thisText, startText);

    	if (startPointer != NULL)
	    {
        	startPointer = startPointer + strlen(startText);
        }
	} // end if the start delimiter is "start"

    if (startPointer != NULL)
    {

		if (strstr(endText, "end") != NULL)
		{
			// Set the end of the string
			endPointer = thisText;
			endLength = 0;
		} else {
			endPointer = strstr(startPointer, endText);
			endLength = (int)strlen(endPointer);
		} // end if the end delimiter is "end"

        stringLength = strlen(startPointer) - endLength;

        if (stringLength == 0)
        {
		    returnText = "";
		    startPointer = NULL;
        } else {
	        // Copy characters between the start and end delimiters
    	    strncpy(returnText,startPointer, stringLength);
	        returnText[stringLength++] = '\0';
		}

    } else {
	    //printf("Start pointer not found\n");
	    returnText = "";

    } // end if the start pointer is not found

    return startPointer;
} // end textBetween method
```



## C++

{{trans|C#}}

```cpp
#include <iostream>

std::ostream& operator<<(std::ostream& out, const std::string& str) {
    return out << str.c_str();
}

std::string textBetween(const std::string& source, const std::string& beg, const std::string& end) {
    size_t startIndex;
    if (beg == "start") {
        startIndex = 0;
    } else {
        startIndex = source.find(beg);
        if (startIndex == std::string::npos) {
            return "";
        }
        startIndex += beg.length();
    }

    size_t endIndex = source.find(end, startIndex);
    if (endIndex == std::string::npos || end == "end") {
        return source.substr(startIndex);
    }

    return source.substr(startIndex, endIndex - startIndex);
}

void print(const std::string& source, const std::string& beg, const std::string& end) {
    using namespace std;
    cout << "text: '" << source << "'\n";
    cout << "start: '" << beg << "'\n";
    cout << "end: '" << end << "'\n";
    cout << "result: '" << textBetween(source, beg, end) << "'\n";
    cout << '\n';
}

int main() {
    print("Hello Rosetta Code world", "Hello ", " world");
    print("Hello Rosetta Code world", "start", " world");
    print("Hello Rosetta Code world", "Hello ", "end");
    print("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">", "<text>", "<table>");
    print("<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>");
    print("The quick brown fox jumps over the lazy other fox", "quick ", " fox");
    print("One fish two fish red fish blue fish", "fish ", " red");
    print("FooBarBazFooBuxQuux", "Foo", "Foo");

    return 0;
}
```

{{out}}

```txt
text: 'Hello Rosetta Code world'
start: 'Hello '
end: ' world'
result: 'Rosetta Code'

text: 'Hello Rosetta Code world'
start: 'start'
end: ' world'
result: 'Hello Rosetta Code'

text: 'Hello Rosetta Code world'
start: 'Hello '
end: 'end'
result: 'Rosetta Code world'

text: '<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">'
start: '<text>'
end: '<table>'
result: 'Hello <span>Rosetta Code</span> world</text><table style="myTable">'

text: '<table style="myTable"><tr><td>hello world</td></tr></table>'
start: '<table>'
end: '</table>'
result: ''

text: 'The quick brown fox jumps over the lazy other fox'
start: 'quick '
end: ' fox'
result: 'brown'

text: 'One fish two fish red fish blue fish'
start: 'fish '
end: ' red'
result: 'two fish'

text: 'FooBarBazFooBuxQuux'
start: 'Foo'
end: 'Foo'
result: 'BarBaz'
```


## C#
{{trans|D}}

```c#
using System;

namespace TextBetween {
    class Program {
        static string TextBetween(string source, string beg, string end) {
            int startIndex;

            if (beg == "start") {
                startIndex = 0;
            }
            else {
                startIndex = source.IndexOf(beg);
                if (startIndex < 0) {
                    return "";
                }
                startIndex += beg.Length;
            }

            int endIndex = source.IndexOf(end, startIndex);
            if (endIndex < 0 || end == "end") {
                return source.Substring(startIndex);
            }
            return source.Substring(startIndex, endIndex - startIndex);
        }

        static void Print(string s, string b, string e) {
            Console.WriteLine("text: '{0}'", s);
            Console.WriteLine("start: '{0}'", b);
            Console.WriteLine("end: '{0}'", e);
            Console.WriteLine("result: '{0}'", TextBetween(s, b, e));
            Console.WriteLine();
        }

        static void Main(string[] args) {
            Print("Hello Rosetta Code world", "Hello ", " world");
            Print("Hello Rosetta Code world", "start", " world");
            Print("Hello Rosetta Code world", "Hello ", "end");
            Print("</div><div style=\"chinese\">你好嗎</div>", "<div style=\"chinese\">", "</div>");
            Print("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">", "<text>", "<table>");
            Print("<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>");
            Print("The quick brown fox jumps over the lazy other fox", "quick ", " fox");
            Print("One fish two fish red fish blue fish", "fish ", " red");
            Print("FooBarBazFooBuxQuux", "Foo", "Foo");
        }
    }
}
```



## D


```D
import std.algorithm.searching;
import std.stdio;
import std.string;

string textBetween(string source, string beg, string end) in {
    assert(beg.length != 0, "beg cannot be empty");
    assert(end.length != 0, "end cannot be empty");
} body {
    ptrdiff_t si = source.indexOf(beg);
    if (beg == "start") {
        si = 0;
    } else if (si < 0) {
        return "";
    } else {
        si += beg.length;
    }

    auto ei = source.indexOf(end, si);
    if (ei < 0 || end == "end") {
        return source[si..$];
    }

    return source[si..ei];
}

void print(string s, string b, string e) {
    writeln("text: '", s, "'");
    writeln("start: '", b, "'");
    writeln("end: '", e, "'");
    writeln("result: '", s.textBetween(b, e), "'");
    writeln;
}

void main() {
    print("Hello Rosetta Code world", "Hello ", " world");
    print("Hello Rosetta Code world", "start", " world");
    print("Hello Rosetta Code world", "Hello ", "end");
    print("</div><div style=\"chinese\">你好嗎</div>", "<div style=\"chinese\">", "</div>");
    print("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">", "<text>", "<table>");
    print("<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>");
    print("The quick brown fox jumps over the lazy other fox", "quick ", " fox");
    print("One fish two fish red fish blue fish", "fish ", " red");
    print("FooBarBazFooBuxQuux", "Foo", "Foo");
}
```

{{out}}

```txt
text: 'Hello Rosetta Code world'
start: 'Hello '
end: ' world'
result: 'Rosetta Code'

text: 'Hello Rosetta Code world'
start: 'start'
end: ' world'
result: 'Hello Rosetta Code'

text: 'Hello Rosetta Code world'
start: 'Hello '
end: 'end'
result: 'Rosetta Code world'

text: '</div><div style="chinese">你好嗎</div>'
start: '<div style="chinese">'
end: '</div>'
result: '你好嗎'

text: '<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">'
start: '<text>'
end: '<table>'
result: 'Hello <span>Rosetta Code</span> world</text><table style="myTable">'

text: '<table style="myTable"><tr><td>hello world</td></tr></table>'
start: '<table>'
end: '</table>'
result: ''

text: 'The quick brown fox jumps over the lazy other fox'
start: 'quick '
end: ' fox'
result: 'brown'

text: 'One fish two fish red fish blue fish'
start: 'fish '
end: ' red'
result: 'two fish'

text: 'FooBarBazFooBuxQuux'
start: 'Foo'
end: 'Foo'
result: 'BarBaz'
```



## Factor


```factor
USING: combinators formatting kernel locals math
prettyprint.config sequences ;
IN: rosetta-code.text-between

:: start ( sdelim text -- n )
    {
        { [ sdelim "start" = ] [ 0 ] }
        { [ sdelim text subseq-start ] [ sdelim text subseq-start sdelim length + ] }
        [ text length ]
    } cond ;

:: end ( edelim text i -- n )
    {
        { [ edelim "end" = ] [ text length ] }
        { [ edelim text i subseq-start-from ] [ edelim text i subseq-start-from ] }
        [ text length ]
    } cond ;

:: text-between ( text sdelim edelim -- seq )
    sdelim text start :> start-index
    edelim text start-index end :> end-index
    start-index end-index text subseq ;

: text-between-demo ( -- )
    {
        { "Hello Rosetta Code world" "Hello " " world" }
        { "Hello Rosetta Code world" "start" " world" }
        { "Hello Rosetta Code world" "Hello " "end" }
        { "</div><div style=\"chinese\">你好嗎</div>" "<div style=\"chinese\">" "</div>" }
        { "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">" "<text>" "<table>" }
        { "<table style=\"myTable\"><tr><td>hello world</td></tr></table>" "<table>" "</table>" }
        { "The quick brown fox jumps over the lazy other fox" "quick " " fox" }
        { "One fish two fish red fish blue fish" "fish " " red" }
        { "FooBarBazFooBuxQuux" "Foo" "Foo" }
    }
    [
        first3 3dup text-between [
            "Text: %u\nStart delimiter: %u\nEnd delimiter: %u\nOutput: %u\n\n"
            printf
        ] without-limits  ! prevent the prettyprinter from culling output
    ] each ;

MAIN: text-between-demo
```

{{out}}

```txt

Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: " world"
Output: "Rosetta Code"

Text: "Hello Rosetta Code world"
Start delimiter: "start"
End delimiter: " world"
Output: "Hello Rosetta Code"

Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: "end"
Output: "Rosetta Code world"

Text: "</div><div style=\"chinese\">你好嗎</div>"
Start delimiter: "<div style=\"chinese\">"
End delimiter: "</div>"
Output: "你好嗎"

Text: "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"
Start delimiter: "<text>"
End delimiter: "<table>"
Output: "Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"

Text: "<table style=\"myTable\"><tr><td>hello world</td></tr></table>"
Start delimiter: "<table>"
End delimiter: "</table>"
Output: ""

Text: "The quick brown fox jumps over the lazy other fox"
Start delimiter: "quick "
End delimiter: " fox"
Output: "brown"

Text: "One fish two fish red fish blue fish"
Start delimiter: "fish "
End delimiter: " red"
Output: "two fish"

Text: "FooBarBazFooBuxQuux"
Start delimiter: "Foo"
End delimiter: "Foo"
Output: "BarBaz"

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "strings"
)

func textBetween(str, start, end string) string {
    if str == "" || start == "" || end == "" {
        return str
    }
    s := 0
    if start != "start" {
        s = strings.Index(str, start)
    }
    if s == -1 {
        return ""
    }
    si := 0
    if start != "start" {
        si = s + len(start)
    }
    e := len(str)
    if end != "end" {
        e = strings.Index(str[si:], end)
        if e == -1 {
            return str[si:]
        }
        e += si
    }
    return str[si:e]
}

func main() {
    texts := [9]string{
        "Hello Rosetta Code world",
        "Hello Rosetta Code world",
        "Hello Rosetta Code world",
        "</div><div style=\"chinese\">你好嗎</div>",
        "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">",
        "<table style=\"myTable\"><tr><td>hello world</td></tr></table>",
        "The quick brown fox jumps over the lazy other fox",
        "One fish two fish red fish blue fish",
        "FooBarBazFooBuxQuux",
    }
    starts:= [9]string{
        "Hello ", "start", "Hello ", "<div style=\"chinese\">",
        "<text>", "<table>", "quick ", "fish ", "Foo",
    }
    ends := [9]string{
        " world", " world", "end", "</div>", "<table>",
        "</table>", " fox", " red", "Foo",
    }
    for i, text := range texts {
        fmt.Printf("Text: \"%s\"\n", text)
        fmt.Printf("Start delimiter: \"%s\"\n", starts[i])
        fmt.Printf("End delimiter: \"%s\"\n", ends[i])
        b := textBetween(text, starts[i], ends[i])
        fmt.Printf("Output: \"%s\"\n\n", b)
    }
}
```


{{out}}

```txt

Same as Kotlin entry.

```



## Haskell


```Haskell
import Data.Text (Text, pack, unpack, breakOn, stripPrefix)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Arrow ((***))


-- TEXT BETWEEN -----------------------------------------------------------
textBetween :: (Either String Text, Either String Text) -> Text -> Text
textBetween (start, end) txt =
  let retain sub part delim t =
        either (Just . const t) (sub $ part . flip breakOn t) delim
  in fromMaybe
       (pack [])
       (retain (stripPrefix <*>) snd start txt >>= retain (Just .) fst end)

-- TESTS ------------------------------------------------------------------
samples :: [Text]
samples =
  pack <$>
  [ "Hello Rosetta Code world"
  , "</div><div style=\"chinese\">你好吗</div>"
  , "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"
  , "<table style=\"myTable\"><tr><td>hello world</td></tr></table>"
  ]

delims :: [(Either String Text, Either String Text)]
delims =
  (wrap *** wrap) <$>
  [ ("Hello ", " world")
  , ("start", " world")
  , ("Hello", "end")
  , ("<div style=\"chinese\">", "</div>")
  , ("<text>", "<table>")
  , ("<text>", "</table>")
  ]

wrap :: String -> Either String Text
wrap x =
  if x `elem` ["start", "end"]
    then Left x
    else Right (pack x)

main :: IO ()
main = do
  mapM_ print $ flip textBetween (head samples) <$> take 3 delims
  (putStrLn . unlines) $
    zipWith
      (\d t -> intercalate (unpack $ textBetween d t) ["\"", "\""])
      (drop 3 delims)
      (tail samples)
```

{{Out}}

```txt
"Rosetta Code"
"Hello Rosetta Code"
" Rosetta Code world"
"你好吗"
"Hello <span>Rosetta Code</span> world</text><table style="myTable">"
""
```



## J

'''Solution:'''

```j
textBetween=: dyad define
  text=. y
  'start end'=. x
  start=. ''"_^:('start'&-:) start
  end=. text"_^:('end'&-:) end
  end taketo start takeafter text
)
```


'''Example Usage:'''

```j
  ('Hello ';' world') textBetween 'Hello Rosetta Code world'
Rosetta Code
```


'''Examples:'''

```j
   Test_text=: <;._2 noun define
Hello Rosetta Code world
Hello Rosetta Code world
Hello Rosetta Code world
</div><div style=\"chinese\">你好嗎</div>
<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">
<table style=\"myTable\"><tr><td>hello world</td></tr></table>
The quick brown fox jumps over the lazy other fox
One fish two fish red fish blue fish
FooBarBazFooBuxQuux
)

   Test_delim=: <"1 '|'&cut;._2 noun define
Hello | world
start| world
Hello |end
<div style=\"chinese\">|</div>
<text>|<table>
<table>|</table>
quick | fox
fish | red
Foo|Foo
)

   Test_output=: <;._2 noun define
Rosetta Code
Hello Rosetta Code
Rosetta Code world
你好嗎
Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">

brown
two fish
BarBaz
)

   Test_output = Test_delim textBetween&.> Test_text
1 1 1 1 1 1 1 1 1
```



## Java


```txt

javac textBetween.java
java -cp . textBetween "hello Rosetta Code world" "hello " " world"

```



```java

public class textBetween
{
    /*
     * textBetween: Get the text between two delimiters
     */
    static String textBetween(String thisText, String startString, String endString)
    {
    	String returnText = "";
    	int startIndex = 0;
    	int endIndex = 0;

    	if (startString.equals("start"))
    	{
    		startIndex = 0;
    	} else {
	    	startIndex = thisText.indexOf(startString);

	    	if (startIndex < 0)
	        {
	        	return "";
	        } else {
	        	startIndex = startIndex + startString.length();
	        }
    	}

    	if (endString.equals("end"))
    	{
    		endIndex = thisText.length();
    	} else {
    		endIndex = thisText.indexOf(endString);

            if (endIndex <= 0)
            {
            	return "";
            } else {

            }
    	}

    	returnText = thisText.substring(startIndex,endIndex);

    	return returnText;
    } // end method textBetween

    /**
     * Main method
     */
    public static void main(String[] args)
    {
    	String thisText = args[0];
    	String startDelimiter = args[1];
    	String endDelimiter = args[2];

    	String returnText = "";
    	returnText = textBetween(thisText, startDelimiter, endDelimiter);

        System.out.println(returnText);

    } // end method main

} // end class TextBetween

```



## JavaScript


### ES5


```javascript

	function textBetween(thisText, startString, endString)
	{
		if (thisText == undefined)
		{
			return "";
		}

		var start_pos = 0;
		if (startString != 'start')
		{
			start_pos = thisText.indexOf(startString);

			// If the text does not contain the start string, return a blank string
			if (start_pos < 0)
			{
				return '';
			}

			// Skip the first startString characters
			start_pos = start_pos + startString.length;
		}

		var end_pos = thisText.length;
		if (endString != 'end')
		{
			end_pos = thisText.indexOf(endString,start_pos);
		}

		// If the text does not have the end string after the start string, return the whole string after the start
		if (end_pos < start_pos)
		{
			end_pos = thisText.length;
		}

		var newText = thisText.substring(start_pos,end_pos);

		return newText;
	} // end textBetween

```


### ES6

{{Trans|Haskell}}
Composed from a set of generic functions

```javascript
(() => {
    'use strict';

    // TEXT BETWEEN ----------------------------------------------------------

    // Delimiter pair -> Haystack -> Any enclosed text
    // textBetween :: (Either String String, Either String String) ->
    //                      String -> String
    const textBetween = ([start, end], txt) => {
        const
            retain = (post, part, delim, t) =>
            either(
                d => just(const_(t, d)), // 'start' or 'end'. No clipping.
                d => post(part(flip(breakOnDef)(t, d))), // One side of break
                delim
            ),
            mbResidue = bindMay(
                retain( // Start token stripped from text after any break
                    curry(stripPrefix)(start.Right),
                    snd, start, txt
                ), // Left side of any break retained.
                curry(retain)(just, fst, end)
            );
        return mbResidue.nothing ? (
            ""
        ) : mbResidue.just;
    }

    // GENERIC FUNCTIONS -----------------------------------------------------

    // append (++) :: [a] -> [a] -> [a]
    const append = (xs, ys) => xs.concat(ys);

    // bindMay (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    const bindMay = (mb, mf) =>
        mb.nothing ? mb : mf(mb.just);

    // Needle -> Haystack -> (prefix before match, match + rest)
    // breakOnDef :: String -> String -> (String, String)
    const breakOnDef = (pat, src) =>
        Boolean(pat) ? (() => {
            const xs = src.split(pat);
            return xs.length > 1 ? [
                xs[0], src.slice(xs[0].length)
            ] : [src, ''];
        })() : undefined;

    // const_ :: a -> b -> a
    const const_ = (k, _) => k;

    // Handles two or more arguments
    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat(Array.from(arguments)));
            };
        return go([].slice.call(args));
    };

    // drop :: Int -> [a] -> [a]
    // drop :: Int -> String -> String
    const drop = (n, xs) => xs.slice(n);

    // either :: (a -> c) -> (b -> c) -> Either a b -> c
    const either = (lf, rf, e) => {
        const ks = Object.keys(e);
        return elem('Left', ks) ? (
            lf(e.Left)
        ) : elem('Right', ks) ? (
            rf(e.Right)
        ) : undefined;
    };

    // elem :: Eq a => a -> [a] -> Bool
    const elem = (x, xs) => xs.includes(x);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // fst :: (a, b) -> a
    const fst = pair => pair.length === 2 ? pair[0] : undefined;

    // just :: a -> Just a
    const just = x => ({
        nothing: false,
        just: x
    });

    // Left :: a -> Either a b
    const Left = x => ({
        Left: x
    });

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // min :: Ord a => a -> a -> a
    const min = (a, b) => b < a ? b : a;

    // nothing :: () -> Nothing
    const nothing = (optionalMsg) => ({
        nothing: true,
        msg: optionalMsg
    });

    // Right :: b -> Either a b
    const Right = x => ({
        Right: x
    });

    // show :: Int -> a -> Indented String
    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[1], null, x[0]] : x
        );

    // snd :: (a, b) -> b
    const snd = tpl => Array.isArray(tpl) ? tpl[1] : undefined;

    // stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
    const stripPrefix = (pfx, s) => {
        const
            blnString = typeof pfx === 'string',
            [xs, ys] = blnString ? (
                [pfx.split(''), s.split('')]
            ) : [pfx, s];
        const
            sp_ = (xs, ys) => xs.length === 0 ? (
                just(blnString ? ys.join('') : ys)
            ) : (ys.length === 0 || xs[0] !== ys[0]) ? (
                nothing()
            ) : sp_(xs.slice(1), ys.slice(1));
        return sp_(xs, ys);
    };

    // tailDef :: [a] -> [a]
    const tailDef = xs => xs.length > 0 ? xs.slice(1) : [];

    // take :: Int -> [a] -> [a]
    const take = (n, xs) => xs.slice(0, n);

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) =>
        Array.from({
            length: Math.min(xs.length, ys.length)
        }, (_, i) => f(xs[i], ys[i], i));


    // TESTS -----------------------------------------------------------------

    // samples :: [String]
    const samples = [
        'Hello Rosetta Code world',
        '</div><div style=\'chinese\'>你好吗</div>',
        '<text>Hello <span>Rosetta Code</span> world</text><table style=\'myTable\'>',
        '<table style=\'myTable\'><tr><td>hello world</td></tr></table>'
    ];

    // delims :: [(Either String String, Either String String)]
    const delims = map(
        curry(map)(x =>
            elem(x, ['start', 'end']) ? (
                Left(x) // Marker token
            ) : Right(x) // Literal text
        ), [
            ['Hello ', ' world'],
            ['start', ' world'],
            ['Hello', 'end'],
            ['<div style=\'chinese\'>', '</div>'],
            ['<text>', '<table>'],
            ['<text>', '</table>']
        ]);

    return show(2,
        append(
            map(
                fromTo => textBetween(fromTo, samples[0]),
                take(3, delims)
            ), zipWith(
                textBetween,
                drop(3, delims),
                tailDef(samples)
            )
        )
    );
})();
```

{{Out}}

```txt
[
  "Rosetta Code",
  "Hello Rosetta Code",
  " Rosetta Code world",
  "你好吗",
  "Hello <span>Rosetta Code</span> world</text><table style='myTable'>",
  ""
]
```



## jq


The implementation uses `explode` to ensure arbitrary Unicode will be handled properly.

```jq

def textbetween_strings($startdlm; $enddlm):
  explode
  | . as $in
  | (if $startdlm == "start" then 0 else ($startdlm | length) end) as $len
  | (if $startdlm == "start" then 0 else index($startdlm | explode) end) as $ix
  | if $ix
    then $in[$ix + $len:]
    | if $enddlm == "end" then .
      else index($enddlm | explode) as $ex
      | if $ex then .[:$ex] else . end
      end
    else []
    end
  | implode;

```


###  Verification


<lang>
def testdata:
  (["Hello Rosetta Code world", "Hello ", " world"],
   ["Hello Rosetta Code world", "start", " world"],
   ["Hello Rosetta Code world", "Hello", "end"],
   ["</div><div style=\"chinese\">你好嗎</div>", "<div style=\"chinese\">", "</div>"],
   ["<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">", "<text>", "<table>"],
   ["<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>"],
   ["The quick brown fox jumps over the lazy other fox", "quick ", " fox"],
   ["One fish two fish red fish blue fish", "fish ", " red"],
   ["FooBarBazFooBuxQuux", "Foo", "Foo"] )
   ;

testdata
| . as $in
| $in[0]
| textbetween_strings($in[1]; $in[2])

```


### Output

<lang>
"Rosetta Code"
"Hello Rosetta Code"
" Rosetta Code world"
"你好嗎"
"Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"
""
"brown"
"two fish"
"BarBaz"

```



## Julia

{{works with|Julia|0.6}}


```julia
function textbetween(text::AbstractString, startdlm::AbstractString, enddlm::AbstractString)
    startind = startdlm != "start" ? last(search(text, startdlm)) + 1 : 1
    endind   = enddlm   != "end"   ? first(search(text, enddlm, startind)) - 1 : endof(text)
    if iszero(startind) || iszero(endind) return "" end
    return text[startind:endind]
end

testcases = [("Hello Rosetta Code world", "Hello ", " world"),
             ("Hello Rosetta Code world", "start", " world"),
             ("Hello Rosetta Code world", "Hello", "end"),
             ("</div><div style=\"chinese\">你好嗎</div>", "<div style=\"chinese\">", "</div>"),
             ("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">", "<text>", "<table>"),
             ("<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>"),
             ("The quick brown fox jumps over the lazy other fox", "quick ", " fox"),
             ("One fish two fish red fish blue fish", "fish ", " red"),
             ("FooBarBazFooBuxQuux", "Foo", "Foo")]


for (text, s, e) in testcases
    println("\nText: ", text, "\nStart delim: ", s, "\nEnd delim: ", e, "\nOutput: ", textbetween(text, s, e))
end
```


{{out}}

```txt
Text: Hello Rosetta Code world
Start delim: Hello
End delim:  world
Output: Rosetta Code

Text: Hello Rosetta Code world
Start delim: start
End delim:  world
Output: Hello Rosetta Code

Text: Hello Rosetta Code world
Start delim: Hello
End delim: end
Output:  Rosetta Code world

Text: </div><div style="chinese">你好嗎</div>
Start delim: <div style="chinese">
End delim: </div>
Output: 你好嗎

Text: <text>Hello <span>Rosetta Code</span> world</text><table style="myTable">
Start delim: <text>
End delim: <table>
Output:

Text: <table style="myTable"><tr><td>hello world</td></tr></table>
Start delim: <table>
End delim: </table>
Output:

Text: The quick brown fox jumps over the lazy other fox
Start delim: quick
End delim:  fox
Output: brown

Text: One fish two fish red fish blue fish
Start delim: fish
End delim:  red
Output: two fish

Text: FooBarBazFooBuxQuux
Start delim: Foo
End delim: Foo
Output: BarBaz
```



## Kotlin

In the third example, I've assumed that the start delimiter should be "Hello " (not "Hello") to match the required output.

```scala
// version 1.2.10

fun String.textBetween(start: String, end: String): String {
    require(!start.isEmpty() && !end.isEmpty())
    if (this.isEmpty()) return this
    val s = if (start == "start") 0 else this.indexOf(start)
    if (s == -1) return ""
    val si = if (start == "start") 0 else s + start.length
    val e = if (end == "end") this.length else this.indexOf(end, si)
    if (e == -1) return this.substring(si)
    return this.substring(si, e)
}

fun main(args: Array<String>) {
    val texts = listOf(
        "Hello Rosetta Code world",
        "Hello Rosetta Code world",
        "Hello Rosetta Code world",
        "</div><div style=\"chinese\">你好嗎</div>",
        "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">",
        "<table style=\"myTable\"><tr><td>hello world</td></tr></table>",
        "The quick brown fox jumps over the lazy other fox",
        "One fish two fish red fish blue fish",
        "FooBarBazFooBuxQuux"
    )
    val startEnds = listOf(
        "Hello " to " world",
        "start" to " world",
        "Hello " to "end",
        "<div style=\"chinese\">" to "</div>",
        "<text>" to "<table>",
        "<table>" to "</table>",
        "quick " to " fox",
        "fish " to " red",
        "Foo" to "Foo"
    )
    for ((i, text) in texts.withIndex()) {
        println("Text: \"$text\"")
        val (s, e) = startEnds[i]
        println("Start delimiter: \"$s\"")
        println("End delimiter: \"$e\"")
        val b = text.textBetween(s, e)
        println("Output: \"$b\"\n")
    }
}
```

{{out}}

```txt

Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: " world"
Output: "Rosetta Code"

Text: "Hello Rosetta Code world"
Start delimiter: "start"
End delimiter: " world"
Output: "Hello Rosetta Code"

Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: "end"
Output: "Rosetta Code world"

Text: "</div><div style="chinese">你好嗎</div>"
Start delimiter: "<div style="chinese">"
End delimiter: "</div>"
Output: "你好嗎"

Text: "<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">"
Start delimiter: "<text>"
End delimiter: "<table>"
Output: "Hello <span>Rosetta Code</span> world</text><table style="myTable">"

Text: "<table style="myTable"><tr><td>hello world</td></tr></table>"
Start delimiter: "<table>"
End delimiter: "</table>"
Output: ""

Text: "The quick brown fox jumps over the lazy other fox"
Start delimiter: "quick "
End delimiter: " fox"
Output: "brown"

Text: "One fish two fish red fish blue fish"
Start delimiter: "fish "
End delimiter: " red"
Output: "two fish"

Text: "FooBarBazFooBuxQuux"
Start delimiter: "Foo"
End delimiter: "Foo"
Output: "BarBaz"

```



## Maple


```Maple
textBetween := proc(str,delim1,delim2)
	local on, off,extra:
	on := piecewise(delim1="start", 1, SearchText(delim1, str)):
	if on = 0 then return ""; end if:
	extra := piecewise(delim1="start", 0, StringTools:-Length(delim1)):
	off := piecewise(delim2="end", 0, SearchText(delim2, str, on+extra..-1)):
	if off <> 0 then off := off+on+extra-1: end if:
	return str[on+extra..off-1]:
end proc:
```

{{Out|Examples}}

```txt
>textBetween("Hello Rosetta Code world", "Hello ", " world");
"Rosetta Code"
>textBetween("Hello Rosetta Code world", "start", " world");
"Hello Rosetta Code"
>textBetween("Hello Rosetta Code world", "Hello ", "end");
"Rosetta Code world"
>textBetween("</div><div style=\"chinese\">你好嗎</div>", "<div style=\"chinese\">", "</div>");
"你好嗎"
>textBetween("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">","<text>","<table>");
"Hello <span>Rosetta Code</span> world</text><table style="myTable">"
>textBetween("<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>");
""
>textBetween("The quick brown fox jumps over the lazy other fox", "quick ", " fox");
"brown"
>textBetween("One fish two fish red fish blue fish", "fish ", " red");
"two fish"
>textBetween("FooBarBazFooBuxQuux", "Foo", "Foo");
"BarBaz"
```



## MiniScript


```MiniScript
textBetween = function(s, startDelim, endDelim)
    startPos = s.indexOf(startDelim) + startDelim.len
    if startDelim == "start" then startPos = 0
    endPos = s.indexOf(endDelim, startPos)
    if endDelim == "end" then endDelim = null
    return s[startPos:endPos]
end function

print textBetween("Hello Rosetta Code world", "Hello ", " world")
print textBetween("Hello Rosetta Code world", "start", " world")
print textBetween("Hello Rosetta Code world", "Hello ", "end")
print textBetween("The quick brown fox jumps over the lazy other fox", "quick ", " fox")
print textBetween("FooBarBazFooBuxQuux", "Foo", "Foo")
```


{{out}}


```txt
Rosetta Code
Hello Rosetta Code
Rosetta Code world
brown
BarBaz
```



## Objeck

{{trans|Java}}

```objeck
class TextBetween {
  function : Main(args : String[]) ~ Nil {
    if(args->Size() = 3) {
      TextBetween(args[0], args[1], args[2])->PrintLine();
    };
  }

  function : TextBetween(thisText : String, startString : String, endString : String) ~ String {
    startIndex := 0;
      endIndex := 0;

    if (startString->Equals("start"))
      {
        startIndex := 0;
      } else {
        startIndex := thisText->Find(startString);

        if (startIndex < 0)
          {
            return "";
          } else {
            startIndex := startIndex + startString->Size();
          };
      };

      if (endString->Equals("end"))
      {
        endIndex := thisText->Size();
      } else {
        endIndex := thisText->Find(endString);

            if (endIndex <= 0)
            {
              return "";
            } else {

            };
      };

    return thisText->SubString(startIndex, endIndex - startIndex);
  }
}
```



## Perl

{{trans|Perl 6}}

```perl
use feature 'say';

sub text_between {
    my($text, $start, $end) = @_;
    return join ',', $text =~ /$start(.*?)$end/g;
}

$text = 'Hello Rosetta Code world';

# String start and end delimiter
say '1> '. text_between($text,  'Hello ', ' world' );

# Regex string start delimiter
say '2> '. text_between($text,  qr/^/, ' world' );

# Regex string end delimiter
say '3> '. text_between($text,  'Hello ',  qr/$/ );

# End delimiter only valid after start delimiter
say '4> '. text_between('</div><div style="chinese">你好嗎</div>', '<div style="chinese">', '</div>' );

# End delimiter not found, default to string end
say '5> '. text_between('<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">', '<text>', qr/<table>|$/ );

# Start delimiter not found, return blank string
say '6> '. text_between('<table style="myTable"><tr><td>hello world</td></tr></table>', '<table>', '</table>' );

# Multiple end delimiters, match frugally
say '7> '. text_between( 'The quick brown fox jumps over the lazy other fox', 'quick ', ' fox' );

# Multiple start delimiters, match frugally
say '8> '. text_between( 'One fish two fish red fish blue fish', 'fish ', ' red' );

# Start delimiter is end delimiter
say '9> '. text_between('FooBarBazFooBuxQuux', 'Foo', 'Foo' );

# Return all matching strings when multiple matches are possible
say '10> '. text_between( $text, 'e', 'o' );

# Ignore start and end delimiter string embedded in longer words
$text = 'Soothe a guilty conscience today, string wrangling is not the best tool to use for this job.';
say '11> '.  text_between($text, qr/\bthe /, qr/ to\b/);
```

{{out}}

```txt
1> Rosetta Code
2> Hello Rosetta Code
3> Rosetta Code world
4> 你好嗎
5> Hello <span>Rosetta Code</span> world</text><table style="myTable">
6>
7> brown
8> two fish
9> BarBaz
10> ll,tta C, w
11> best tool
```



## Perl 6

{{works with|Rakudo|2017.12}}
It seems somewhat pointless to write a special purpose routine to do text matching as built-in primitives can do so more flexibly and concisely, but whatever.

This version doesn't use strings for meta indexes ('start' and 'end'), rather it accepts regex assertions which are parsed differently from strings. This allows much more robust and fine grained control over what does and doesn't match. (and allows delimiter strings of 'start' and 'end' incidentally.) See the 11th example below which will confound nearly all of the current string-only based implementations.


```perl6
sub text-between ( $text, $start, $end ) {
    return $/»[0]».Str if $text ~~ m:g/ $start (.*?) $end /;
    []
}

# Testing
my $text = 'Hello Rosetta Code world';

# String start and end delimiter
put '1> ', $text.&text-between( 'Hello ', ' world' );

# Regex string start delimiter
put '2> ', $text.&text-between( rx/^/, ' world' );

# Regex string end delimiter
put '3> ', $text.&text-between( 'Hello ',  rx/$/ );

# End delimiter only valid after start delimiter
put '4> ', '</div><div style="chinese">你好嗎</div>'\
    .&text-between( '<div style="chinese">', '</div>' );

# End delimiter not found, default to string end
put '5> ', '<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">'\
    .&text-between( '<text>', rx/'<table>' | $/ );

# Start delimiter not found, return blank string
put '6> ', '<table style="myTable"><tr><td>hello world</td></tr></table>'\
    .&text-between( '<table>', '</table>' );

# Multiple end delimiters, match frugally
put '7> ', 'The quick brown fox jumps over the lazy other fox'\
    .&text-between( 'quick ', ' fox' );

# Multiple start delimiters, match frugally
put '8> ', 'One fish two fish red fish blue fish'\
    .&text-between( 'fish ', ' red' );

# Start delimiter is end delimiter
put '9> ', 'FooBarBazFooBuxQuux'\
    .&text-between( 'Foo', 'Foo' );

# Return all matching strings when multiple matches are possible
put '10> ', join ',', $text.&text-between( 'e', 'o' );

# Ignore start and end delimiter string embedded in longer words
put '11> ', 'Soothe a guilty conscience today, string wrangling is not the best tool to use for this job.'\
    .&text-between( rx/«'the '/, rx/' to'»/ );
```


{{out}}

```txt
1> Rosetta Code
2> Hello Rosetta Code
3> Rosetta Code world
4> 你好嗎
5> Hello <span>Rosetta Code</span> world</text><table style="myTable">
6>
7> brown
8> two fish
9> BarBaz
10> ll,tta C, w
11> best tool
```



## Phix


```Phix
function text_between(string text, start_delimiter, end_delimiter)
    if start_delimiter!="start" then
        integer k = match(start_delimiter,text)
        if k=0 then return "" end if
        text = text[k+length(start_delimiter)..$]
    end if
    if end_delimiter!="end" then
        integer k = match(end_delimiter,text)
        if k!=0 then
            text = text[1..k-1]
        end if
    end if
    return text
end function

constant tests = {
{"Hello Rosetta Code world","Hello "," world","Rosetta Code"},
{"Hello Rosetta Code world","start"," world","Hello Rosetta Code"},
{"Hello Rosetta Code world","Hello ","end","Rosetta Code world"},
{"</div><div style=\"french\">bonjour</div>","<div style=\"french\">","</div>","bonjour"},
{"<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">","<text>","<table>",
 "Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"},
{"<table style=\"myTable\"><tr><td>hello world</td></tr></table>","<table>","</table>",""},
{"The quick brown fox jumps over the lazy other fox","quick "," fox","brown"},
{"One fish two fish red fish blue fish","fish "," red","two fish"},
{"FooBarBazFooBuxQuux","Foo","Foo","BarBaz"},
{"Hello Rosetta Code world","start","end","Hello Rosetta Code world"}}

constant fmt = """
Text: "%s"
Start delimiter: "%s"
End delimiter: "%s"
Output: "%s"
Expect: "%s" ***ERROR***

"""

for i=1 to length(tests) do
    string {text,start_delimiter,end_delimiter,expected} = tests[i],
            actual = text_between(text,start_delimiter,end_delimiter)
    if actual!=expected then
        printf(1,fmt,{text,start_delimiter,end_delimiter,actual,expected})
    end if
end for
```

{{out}}
All tests pass, so no output.


## PHP


```txt

http://localhost/textBetween.php?thisText=hello%20Rosetta%20Code%20world&start=hello%20&end=%20world

```



```php

<?php
function text_between($string, $start, $end)
{
    //$string = " ".$string;
    $startIndex = strpos($string,$start);

    if ($start == "start")
    {
    	$startIndex = 0;
    } else {
    	if ($startIndex == 0)
    	{
    		return "Start text not found";
    	}
    }

    if ($end == "end")
    {
    	$endIndex=strlen($string);
    	$resultLength = $endIndex - $startIndex;
    } else {
	    $resultLength = strpos($string,$end,$startIndex) - $startIndex;
	}

    if ($start != "start")
    {
		$startIndex += strlen($start);
	}

    if ($resultLength <= 0)
    {
    	return "End text not found";
    }

    return substr($string,$startIndex,$resultLength);
}

$thisText=$_GET["thisText"];
$startDelimiter=$_GET["start"];
$endDelimiter=$_GET["end"];

$returnText = text_between($thisText, $startDelimiter, $endDelimiter);

print_r($returnText);
?>

```



## PowerBASIC


```powerbasic
#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6

'*********************

FUNCTION TextBetween( _
     BYVAL Text AS WSTRING, _
     BYVAL StartDelim AS WSTRING, _
     BYVAL EndDelim AS WSTRING) _
  AS WSTRING

LOCAL indS AS LONG
LOCAL indE AS LONG

  IF StartDelim = "start" THEN
    indS = 1
  ELSE
    indS = INSTR(1, TEXT, StartDelim)
    IF indS THEN
      indS += LEN(StartDelim)
    END IF
  END IF

  IF indS THEN
    IF EndDelim = "end" THEN
      indE = LEN(TEXT) + 1
    ELSE
      indE = INSTR(indS, TEXT, EndDelim)
      IF indE = 0 THEN
        indE = LEN(TEXT) + 1
      END IF
    END IF
    indE -= indS
    IF indE THEN
      FUNCTION = MID$(TEXT, indS, indE)
    END IF
  END IF

END FUNCTION

'*********************

FUNCTION PBMAIN () AS LONG
LOCAL sText AS WSTRING
LOCAL StartDelim AS WSTRING
LOCAL EndDelim AS WSTRING
LOCAL Expected AS WSTRING

'Ex. 1
sText = "Hello Rosetta Code world"
StartDelim = "Hello "
EndDelim = " world"
Expected = "Rosetta Code"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Ex. 2
sText = "Hello Rosetta Code world"
StartDelim = "start"
EndDelim = " world"
Expected = "Hello Rosetta Code"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Ex. 3
sText = "Hello Rosetta Code world"
StartDelim = "Hello "
EndDelim = "end"
Expected = "Rosetta Code world"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Ex. 4
Expected = CHR$$(&H4F60) & CHR$$(&H597D) & CHR$$(&H55CE)
sText = "</div><div style=""chinese"">" & Expected & "</div>"
StartDelim = "<div style=""chinese"">"
EndDelim = "</div>"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Ex. 5
sText = "<text>Hello <span>Rosetta Code</span> world</text><table style=""myTable"">"
StartDelim = "<text>"
EndDelim = "<table>"
Expected = "Hello <span>Rosetta Code</span> world</text><table style=""myTable"">"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Ex. 6
sText = "<table style=""myTable""><tr><td>hello world</td></tr></table>"
StartDelim = "<table>"
EndDelim = "</table>"
Expected = ""
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Ex. 7
sText = "The quick brown fox jumps over the lazy other fox"
StartDelim = "quick "
EndDelim = " fox"
Expected = "brown"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Ex. 8
sText = "One fish two fish red fish blue fish"
StartDelim = "fish "
EndDelim = " red"
Expected = "two fish"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Ex. 9
sText = "FooBarBazFooBuxQuux"
StartDelim = "Foo"
EndDelim = "Foo"
Expected = "BarBaz"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

'Extra test: empty text between delimiters
StartDelim = "Foo"
EndDelim = "BarBaz"
Expected = ""
sText = StartDelim & Expected & EndDelim & "FooBuxQuux"
CON.PRINT IIF$(TextBetween(sText, StartDelim, EndDelim) = Expected, "OK", "failed")

END FUNCTION
```



## Python


```python

#!/usr/bin/env python
from sys import argv

# textBetween in python
# Get the text between two delimiters
# Usage:
# python textBetween.py "hello Rosetta Code world" "hello " " world"

def textBetween( thisText, startString, endString ):
    try:
    	if startString is 'start':
    		startIndex = 0
    	else:
    		startIndex = thisText.index( startString )

    	if not (startIndex >= 0):
    		return 'Start delimiter not found'
    	else:
        	if startString is not 'start':
        		startIndex = startIndex + len( startString )

        returnText = thisText[startIndex:]


    	if endString is 'end':
    		return returnText
    	else:
    		endIndex = returnText.index( endString )

    	if not (endIndex >= 0):
    		return 'End delimiter not found'
    	else:
        	returnText = returnText[:endIndex]

        return returnText
    except ValueError:
        return "Value error"

script, first, second, third = argv

thisText = first
startString = second
endString = third

print textBetween( thisText, startString, endString )

```



## Racket


```racket
#lang racket
(require (prefix-in 13: srfi/13))

(define (text-to-end text end)
  (cond [(13:string-contains text end) => (λ (i) (substring text 0 i))]
        [else text]))

(define (text-from-start text start)
  (cond [(13:string-contains text start) => (λ (i) (substring text (+ i (string-length start))))]
        [else ""]))

(define text-between
  (match-lambda**
   [("start" "end" text) text]
   [("start" end text) (text-to-end text end)]
   [(start "end" text) (text-from-start text start)]
   [(start end text) (text-to-end (text-from-start text start) end)]))

(module+ test
  (require rackunit)

  (define (test-case text start end output)
    (check-equal? (text-between start end text) output))

  (test-case "Hello Rosetta Code world" "Hello " " world" "Rosetta Code")
  (test-case "Hello Rosetta Code world" "start" " world" "Hello Rosetta Code")
  (test-case "Hello Rosetta Code world" "Hello" "end" " Rosetta Code world")
  (test-case "</div><div style=\"chinese\">你好嗎</div>" "<div style=\"chinese\">" "</div>" "你好嗎")
  (test-case "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">" "<text>" "<table>" "Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">")
  (test-case "<table style=\"myTable\"><tr><td>hello world</td></tr></table>" "<table>" "</table>" "")
  (test-case "The quick brown fox jumps over the lazy other fox" "quick " " fox" "brown")
  (test-case "One fish two fish red fish blue fish" "fish " " red" "two fish")
  (test-case "FooBarBazFooBuxQuux" "Foo" "Foo" "BarBaz"))
```

{{out}}
All tests pass, so no output.


## REXX


### version 1

{{trans|Kotlin}}

```rexx
Call test  "Hello Rosetta Code world","Hello "," world"
Call test  "Hello Rosetta Code world","start"," world"
Call test  "Hello Rosetta Code world","Hello ","end"
Call test  "</div><div style=""chinese"">???</div>","<div style=""chinese"">","</div>"
Call test  "<text>Hello <span>Rosetta Code</span> world</text><table style=""myTable"">","<text>","<table>"
Call test  "<table style=""myTable""><tr><td>hello world</td></tr></table>","<table>","</table>"
Call test  "The quick brown fox jumps over the lazy other fox","quick "," fox"
Call test  "One fish two fish red fish blue fish","fish "," red"
Call test  "FooBarBazFooBuxQuux","Foo","Foo"
Exit

test: Procedure
  Parse Arg t,s,e
  res=text_between(t,s,e)
  Call o 'Text: "'t'"'
  Call o 'Start delimiter: "'s'"'
  Call o 'End delimiter: "'e'"'
  Call o 'Output: "'res'"'
  Call o ''
  Return

text_between: Procedure
  Parse Arg this_text, start_text, end_text
  If start_text='start' Then
    rest=this_text
  Else Do
    s=pos(start_text,this_text)
    If s>0 Then
      rest=substr(this_text,s+length(start_text))
    Else
      Return ''
    End
  If end_text='end' Then
    Return rest
  Else Do
    e=pos(end_text,rest)
    If e=0 Then
      Return rest
    Return left(rest,e-1)
    End

o: Say arg(1)
```

{{out|Output}}

```txt
Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: " world"
Output: "Rosetta Code"

Text: "Hello Rosetta Code world"
Start delimiter: "start"
End delimiter: " world"
Output: "Hello Rosetta Code"

Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: "end"
Output: "Rosetta Code world"

Text: "</div><div style="chinese">???</div>"
Start delimiter: "<div style="chinese">"
End delimiter: "</div>"
Output: "???"

Text: "<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">"
Start delimiter: "<text>"
End delimiter: "<table>"
Output: "Hello <span>Rosetta Code</span> world</text><table style="myTable">"

Text: "<table style="myTable"><tr><td>hello world</td></tr></table>"
Start delimiter: "<table>"
End delimiter: "</table>"
Output: ""

Text: "The quick brown fox jumps over the lazy other fox"
Start delimiter: "quick "
End delimiter: " fox"
Output: "brown"

Text: "One fish two fish red fish blue fish"
Start delimiter: "fish "
End delimiter: " red"
Output: "two fish"

Text: "FooBarBazFooBuxQuux"
Start delimiter: "Foo"
End delimiter: "Foo"
Output: "BarBaz"
```



### version 2

This REXX version makes use of the REXX   '''parse'''   instruction to extract the required string.

Also, it wasn't necessary, but I <u>assummed</u> (bad assumption?) that the   <big>'''\'''</big>   could be an escape character, but unless clarified,

it's being treated as a commom character,   REXX has no need for escape characters (within character strings).

```rexx
/*REXX programs displays the text between two text deliminiters in a target text string.*/
call TB 'Hello Rosetta Code world', "Hello ",   ' world'
call TB 'Hello Rosetta Code world', "start",    ' world'
call TB 'Hello Rosetta Code world', "Hello",    'end'
call TB '</div><div style=\"chinese\">???</div>', '<div style=\"chinese\">', "</div>"
call TB '<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">',"<text>",'<table>'
call TB '<table style=\"myTable\"><tr><td>hello world</td></tr></table>',"<table>",'</table>'
call TB 'The quick brown fox jumps over the lazy other fox', "quick ", ' fox'
call TB 'One fish two fish red fish blue fish',              "fish ",  ' red'
call TB 'FooBarBazFooBuxQuux',  "Foo",   'Foo'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
TB: procedure: parse arg T,S,E;      say         /*obtain text, start delim, end delim. */
    say '           Text: "'T'"'                 /*echo the  text          to terminal. */
    say 'Start delimiter: "'S'"'                 /*  "   "   start delim    "    "      */
    say 'End   delimiter: "'E'"'                 /*  "   "    end    "      "    "      */
    $=T;  if S\=='start'  then parse var T (S) $ /*extract stuff  after  the START delim*/
          if E\=='end'    then parse var $ $ (E) /*   "      "   before   "   END.   "  */
    say '         Output: "'$'"'                 /*display the extracted string to term.*/
    return
```

{{out|output}}

```txt

           Text: "Hello Rosetta Code world"
Start delimiter: "Hello "
End   delimiter: " world"
         Output: "Rosetta Code"

           Text: "Hello Rosetta Code world"
Start delimiter: "start"
End   delimiter: " world"
         Output: "Hello Rosetta Code"

           Text: "Hello Rosetta Code world"
Start delimiter: "Hello"
End   delimiter: "end"
         Output: " Rosetta Code world"

           Text: "</div><div style=\"chinese\">???</div>"
Start delimiter: "<div style=\"chinese\">"
End   delimiter: "</div>"
         Output: "???"

           Text: "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"
Start delimiter: "<text>"
End   delimiter: "<table>"
         Output: "Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"

           Text: "<table style=\"myTable\"><tr><td>hello world</td></tr></table>"
Start delimiter: "<table>"
End   delimiter: "</table>"
         Output: ""

           Text: "The quick brown fox jumps over the lazy other fox"
Start delimiter: "quick "
End   delimiter: " fox"
         Output: "brown"

           Text: "One fish two fish red fish blue fish"
Start delimiter: "fish "
End   delimiter: " red"
         Output: "two fish"

           Text: "FooBarBazFooBuxQuux"
Start delimiter: "Foo"
End   delimiter: "Foo"
         Output: "BarBaz"

```



## Ring


```ring

# Project : Text between

text = "Hello Rosetta Code world"
startdelimiter = "Hello"
enddelimiter = "world"
textdel = []

see "Example 1. Both delimiters set :" + nl
see 'Text = "Hello Rosetta Code world"' + nl
see 'Start delimiter = "Hello"' + nl
see 'End delimiter = "world"'+ nl
see "Output = "
textarr = str2list(substr(text , " ", nl))
posstart = find(textarr, startdelimiter)
posend = find(textarr, enddelimiter)
for n = posstart + 1 to posend - 1
      add(textdel, textarr[n])
next
see '"' + substr(list2str(textdel), nl, " ") +'"' + nl + nl

see "Example 2. Start delimiter is the start of the string :" + nl
see 'Text = "Hello Rosetta Code world"' + nl
see 'Start delimiter = "start"' + nl
see 'End delimiter = "world"'+ nl
see "Output = "
textdel = []
textarr = str2list(substr(text , " ", nl))
posend = find(textarr, enddelimiter)
for n = 1 to posend - 1
      add(textdel, textarr[n])
next
see '"' + substr(list2str(textdel), nl, " ") +'"' + nl + nl

see "Example 3. End delimiter is the end of the string :" + nl
see 'Text = "Hello Rosetta Code world"' + nl
see 'Start delimiter = "Hello"' + nl
see 'End delimiter = "end"'+ nl
see "Output = "
textdel = []
textarr = str2list(substr(text , " ", nl))
posstart = find(textarr, startdelimiter)
for n = posstart + 1 to len(textarr)
      add(textdel, textarr[n])
next
see '"' + substr(list2str(textdel), nl, " ") +'"' + nl + nl

```

Output:

```txt

Example 1. Both delimiters set :
Text = "Hello Rosetta Code world"
Start delimiter = "Hello"
End delimiter = "world"
Output = "Rosetta Code"

Example 2. Start delimiter is the start of the string :
Text = "Hello Rosetta Code world"
Start delimiter = "start"
End delimiter = "world"
Output = "Hello Rosetta Code"

Example 3. End delimiter is the end of the string :
Text = "Hello Rosetta Code world"
Start delimiter = "Hello"
End delimiter = "end"
Output = "Rosetta Code world"

```



## Ruby

Test

```ruby

class String
  def textBetween startDelimiter, endDelimiter

  	if (startDelimiter == "start") then
  		startIndex = 0
  	else
  		startIndex = self.index(startDelimiter) + startDelimiter.length
  	end

  	if (startIndex == nil) then
  		return "Start delimiter not found"
  	end

  	thisLength = self.length

  	returnText = self[startIndex, thisLength]

 	if (endDelimiter == "end") then
  		endIndex = thisLength
  	else
  		endIndex = returnText.index(endDelimiter)
  	end

  	if (endIndex == nil) then
  		return "End delimiter not found"
  	end

  	returnText = returnText[0, endIndex]

  	return returnText

  end
end

thisText = ARGV[0]
startDelimiter = ARGV[1]
endDelimiter = ARGV[2]

#puts thisText
#puts startDelimiter
#puts endDelimiter

returnText = thisText.textBetween(startDelimiter, endDelimiter)

puts returnText

```


## Rust


```Rust

//Use Into<String> so input can be String, &str or anything else that implements Into<String>
fn text_between<S: Into<String>>(input: S, start: S, end: S) -> String {
    //Convert to strings
    let search_str = input.into();
    let start_str = start.into();
    let end_str = end.into();

    let start_idx  = if start_str.as_str() == "start" {
        0
    } else {
        let start_point = search_str.find(start_str.as_str());
        if start_point.is_none() {
            return String::from("");
        }
        start_point.unwrap() + start_str.len()
    };
    let remaining = &search_str[start_idx..];

    let end_idx = if end_str.as_str() == "end" {
        remaining.len()
    } else {
        remaining.find(end_str.as_str()).unwrap_or(remaining.len())
    };

    return remaining[..end_idx].to_string()
}

fn main() {
    println!("'{}'", text_between("Hello Rosetta Code world", "Hello ", " world"));
    println!("'{}'", text_between("Hello Rosetta Code world", "start", " world"));
    println!("'{}'", text_between("Hello Rosetta Code world", "Hello", "end"));
    println!("'{}'", text_between("</div><div style=\"chinese\">你好嗎</div>", "<div style=\"chinese\">", "</div>"));
    println!("'{}'", text_between("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">", "<text>", "<table>"));
    println!("'{}'", text_between("<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>"));
    println!("'{}'", text_between("The quick brown fox jumps over the lazy other fox", "quick ", " fox"));
    println!("'{}'", text_between("One fish two fish red fish blue fish", "fish ", " red"));
    println!("'{}'", text_between("FooBarBazFooBuxQuux", "Foo", "Foo"));
}

```

{{out}}

```txt

'Rosetta Code'
'Hello Rosetta Code'
' Rosetta Code world'
'你好嗎'
'Hello <span>Rosetta Code</span> world</text><table style="myTable">'
''
'brown'
'two fish'
'BarBaz'

```


## Scala


```Scala
object TextBetween extends App {
    val (thisText, startDelimiter, endDelimiter) = (args(0), args(1),args(2))

  /*
   * textBetween: Get the text between two delimiters
   */
  private def textBetween(thisText: String, startString: String, endString: String): String = {
    var startIndex = 0
    var endIndex = 0
    if (startString != "start")
    {
      startIndex = thisText.indexOf(startString)
      if (startIndex < 0) return ""
      else startIndex = startIndex + startString.length
    }
    if (endString == "end") endIndex = thisText.length
    else {
      endIndex = thisText.indexOf(endString)
      if (endIndex <= 0) return ""
    }

    thisText.substring(startIndex, endIndex)
  } // end method textBetween

  println(textBetween(thisText, startDelimiter, endDelimiter))

}
```



## Sidef

Uses /^/ and /$/ as start and end delimiters. Additionally, the start and end delimiters can be regular expressions.
{{trans|Perl 6}}

```ruby
func text_between (text, beg, end) {

    beg.escape! if beg.kind_of(String)
    end.escape! if end.kind_of(String)

    Regex("#{beg}(.*?)(?:#{end}|\\z)", 's').match(text)[0] \\ ""
}

var tests = [
    Hash(
        text  => "Hello Rosetta Code world",
        start => "Hello ",
        end   => " world",
        out   => "Rosetta Code",
    ),
    Hash(
        text  => "Hello Rosetta Code world",
        start => /^/,
        end   => " world",
        out   => "Hello Rosetta Code",
    ),
    Hash(
        text  => "Hello Rosetta Code world",
        start => "Hello ",
        end   => /$/,
        out   => "Rosetta Code world",
    ),
    Hash(
        text  => "</div><div style=\"chinese\">你好嗎</div>",
        start => "<div style=\"chinese\">",
        end   => "</div>",
        out   => "你好嗎",
    ),
    Hash(
        text  => "<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">",
        start => "<text>",
        end   => "<table>",
        out   => "Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">",
    ),
    Hash(
        text  => "<table style=\"myTable\"><tr><td>hello world</td></tr></table>",
        start => "<table>",
        end   => "</table>",
        out   => "",
    ),
    Hash(
        text  => "The quick brown fox jumps over the lazy other fox",
        start => "quick ",
        end   => " fox",
        out   => "brown",
    ),
    Hash(
        text  => "One fish two fish red fish blue fish",
        start => "fish ",
        end   => " red",
        out   => "two fish",
    ),
    Hash(
        text  => "FooBarBazFooBuxQuux",
        start => "Foo",
        end   => "Foo",
        out   => "BarBaz",
    ),
]

tests.each { |t|
    var r = text_between(t{:text}, t{:start}, t{:end})
    assert_eq(t{:out}, r)
    say "text_between(#{t{:text}.dump}, #{t{:start}.dump}, #{t{:end}.dump}) = #{r.dump}"
}
```

{{out}}
```txt

text_between("Hello Rosetta Code world", "Hello ", " world") = "Rosetta Code"
text_between("Hello Rosetta Code world", /^/, " world") = "Hello Rosetta Code"
text_between("Hello Rosetta Code world", "Hello ", /$/) = "Rosetta Code world"
text_between("</div><div style=\"chinese\">\x{4F60}\x{597D}\x{55CE}</div>", "<div style=\"chinese\">", "</div>") = "\x{4F60}\x{597D}\x{55CE}"
text_between("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">", "<text>", "<table>") = "Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">"
text_between("<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>") = ""
text_between("The quick brown fox jumps over the lazy other fox", "quick ", " fox") = "brown"
text_between("One fish two fish red fish blue fish", "fish ", " red") = "two fish"
text_between("FooBarBazFooBuxQuux", "Foo", "Foo") = "BarBaz"
```



## Swift



```swift
import Foundation

public extension String {
  func textBetween(_ startDelim: String, and endDelim: String) -> String {
    precondition(!startDelim.isEmpty && !endDelim.isEmpty)

    let startIdx: String.Index
    let endIdx: String.Index

    if startDelim == "start" {
      startIdx = startIndex
    } else if let r = range(of: startDelim) {
      startIdx = r.upperBound
    } else {
      return ""
    }

    if endDelim == "end" {
      endIdx = endIndex
    } else if let r = self[startIdx...].range(of: endDelim) {
      endIdx = r.lowerBound
    } else {
      endIdx = endIndex
    }

    return String(self[startIdx..<endIdx])
  }
}

let tests = [
  ("Hello Rosetta Code world", "Hello ", " world"),
  ("Hello Rosetta Code world", "start", " world"),
  ("Hello Rosetta Code world", "Hello ", "end"),
  ("</div><div style=\"chinese\">你好嗎</div>", "<div style=\"chinese\">", "</div>"),
  ("<text>Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">", "<text>", "<table>"),
  ("<table style=\"myTable\"><tr><td>hello world</td></tr></table>", "<table>", "</table>"),
  ("The quick brown fox jumps over the lazy other fox", "quick ", " fox"),
  ("One fish two fish red fish blue fish", "fish ", " red"),
  ("FooBarBazFooBuxQuux", "Foo", "Foo")
]

for (input, start, end) in tests {
  print("Input: \"\(input)\"")
  print("Start delimiter: \"\(start)\"")
  print("End delimiter: \"\(end)\"")
  print("Text between: \"\(input.textBetween(start, and: end))\"\n")
}
```


{{out}}

<pre style="scroll: overflow; height: 20em">Input: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: " world"
Text between: "Rosetta Code"

Input: "Hello Rosetta Code world"
Start delimiter: "start"
End delimiter: " world"
Text between: "Hello Rosetta Code"

Input: "Hello Rosetta Code world"
Start delimiter: "Hello "
End delimiter: "end"
Text between: "Rosetta Code world"

Input: "</div><div style="chinese">你好嗎</div>"
Start delimiter: "<div style="chinese">"
End delimiter: "</div>"
Text between: "你好嗎"

Input: "<text>Hello <span>Rosetta Code</span> world</text><table style="myTable">"
Start delimiter: "<text>"
End delimiter: "<table>"
Text between: "Hello <span>Rosetta Code</span> world</text><table style="myTable">"

Input: "<table style="myTable"><tr><td>hello world</td></tr></table>"
Start delimiter: "<table>"
End delimiter: "</table>"
Text between: ""

Input: "The quick brown fox jumps over the lazy other fox"
Start delimiter: "quick "
End delimiter: " fox"
Text between: "brown"

Input: "One fish two fish red fish blue fish"
Start delimiter: "fish "
End delimiter: " red"
Text between: "two fish"

Input: "FooBarBazFooBuxQuux"
Start delimiter: "Foo"
End delimiter: "Foo"
Text between: "BarBaz"
```



## UNIX Shell

{{works with|Bash}}
{{works with|Dash}}
{{works with|Zsh}}

This implementation creates no processes/subshells in modern shells (e.g. shells in which 'echo' and '[' are builtins). It modifies/leaks no global state other than the "text_between" function's name. Its behavior is not changed by the presence or absence of common shell options (e.g. "-e", "-u", "pipefail", or POSIX compatibility mode) or settings (e.g. "IFS").

This can be made to work with ksh (93) by removing all uses of the "local" keyword, though this will cause it to modify global variables.

The "hard" assertions when unpacking the arguments to the "text_between" function reflect the assumptions in the requirements for this problem: that null/empty arguments will never be provided. If any empty arguments are given, the interpreter running this function will exit after printing an error. If this function is invoked without a subshell, that will crash the invoking program as well. In practical use, that may not be desirable, in which case the ":?" assertions should be replaced with less harsh conditional-unpack code (e.g. <code>if [ -z "${1:-}" ]; then echo "Invalid input!" && return 127; else local var="$1"; fi</code>).


```bash
text_between() {
	local search="${1:?Search text not provided}"
	local start_str="${2:?Start text not provided}"
	local end_str="${3:?End text not provided}"
	local temp=

	if [ "$start_str" != "start" ]; then
		# $temp will be $search with everything before the first occurrence of
		# $start_str (inclusive) removed, searching from the beginning.
		temp="${search#*$start_str}"
		# If the start delimiter wasn't found, return an empty string.
		# Comparing length rather than string equality because character
		# comparison is not necessary here.
		if [ "${#temp}" -eq "${#search}" ]; then
			search=
		else
			search="$temp"
		fi
	fi

	if [ "$end_str" = "end" ]; then
		echo "$search"
	else
		# Output will be $search with everything after the last occurrence of
		# $end_str (inclusive) removed, searching from the end.
		echo "${search%%$end_str*}"
	fi
	return 0
}

text_between "Hello Rosetta Code world" "Hello " " world"
text_between "Hello Rosetta Code world" "start" " world"
text_between "Hello Rosetta Code world" "Hello " "end"
```




## VBA


```vb
Option Explicit

Private Const STRING_START As String = "Start"
Private Const STRING_END As String = "End"

Sub Main()
Dim Text As String, First As String, Last As String, Output As String
'Example 1. Both delimiters set
    Text = "Hello Rosetta Code world"
        First = "Hello "
            Last = " world"
    Output = "1- " & Text_Between(Text, First, Last) & vbCrLf
'Example 2. Start delimiter is the start of the string
    Text = "Hello Rosetta Code world"
        First = "Start"
            Last = " world"
    Output = Output & "2- " & Text_Between(Text, First, Last) & vbCrLf
'Example 3. End delimiter is the end of the string
    Text = "Hello Rosetta Code world"
        First = "Hello "
            Last = "End"
    Output = Output & "3- " & Text_Between(Text, First, Last) & vbCrLf
'Example 4. End delimiter appears before and after start delimiter
    Text = "</div><div style=\""chinese\"">你好嗎</div>..."
        First = "<div style=\""chinese\"">"
            Last = "</div>"
    Output = Output & "4- " & Text_Between(Text, First, Last) & vbCrLf
'Example 5. End delimiter not present
    Text = "<text>Hello <span>Rosetta Code</span> world</text><table style=\""myTable\"">"
        First = "<text>"
            Last = "<table>"
    Output = Output & "5- " & Text_Between(Text, First, Last) & vbCrLf
'Example 6. Start delimiter not present
    Text = "<table style=\""myTable\""><tr><td>hello world</td></tr></table>"
        First = "<table>"
            Last = "</table>"
    Output = Output & "6- " & Text_Between(Text, First, Last) & vbCrLf
'Example 7. Multiple instances of end delimiter after start delimiter (match until the first one)
    Text = "The quick brown fox jumps over the lazy other fox"
        First = "quick "
            Last = " fox"
    Output = Output & "7- " & Text_Between(Text, First, Last) & vbCrLf
'Example 8. Multiple instances of the start delimiter (start matching at the first one)
    Text = "One fish two fish red fish blue fish"
        First = "fish "
            Last = " red"
    Output = Output & "8- " & Text_Between(Text, First, Last) & vbCrLf
'Example 9. Start delimiter is end delimiter
    Text = "FooBarBazFooBuxQuux"
        First = "Foo"
            Last = "Foo"
    Output = Output & "9- " & Text_Between(Text, First, Last) & vbCrLf
'Example 10 : End delimiter appears before and NOT after start delimiter
    Text = "</div><div style=\""chinese\"">你好嗎..."
        First = "<div style=\""chinese\"">"
            Last = "</div>"
    Output = Output & "10- " & Text_Between(Text, First, Last) & vbCrLf
'Example 11. Text = ""
    Text = ""
        First = "Start"
            Last = "End"
    Output = Output & "11- " & Text_Between(Text, First, Last) & vbCrLf
'Example 12. Start and end delimiters use special values
    Text = "Hello Rosetta Code world"
        First = "Start"
            Last = "End"
    Output = Output & "12- " & Text_Between(Text, First, Last)
'Result :
    Debug.Print Output
End Sub

Private Function Text_Between(T$, F$, L$) As String
Dim i As Long
    i = InStr(T, L) + 1
    Select Case True
        Case T = "", F = "", InStr(T, F) = 0 And F <> STRING_START
            Text_Between = ""
        Case F = STRING_START And L = STRING_END
            Text_Between = T
        Case F = STRING_START
            Text_Between = Mid(T, 1, InStr(T, L) - 1)
        Case L = STRING_END, InStr(T, L) = 0, InStr(T, L) < InStr(T, F) And InStr(i, T, L) = 0
            Text_Between = Mid(T, Len(F) + InStr(T, F))
        Case F = L
            Text_Between = Mid(T, Len(F) + InStr(T, F), InStr(i, T, F) - Len(F) - 1)
        Case Else
            Text_Between = Mid(T, Len(F) + InStr(T, F), InStr(InStr(T, F), T, L) - (Len(F) + InStr(T, F)))
    End Select
End Function
```

{{out}}

```txt
1- Rosetta Code
2- Hello Rosetta Code
3- Rosetta Code world
4- 你好嗎
5- Hello <span>Rosetta Code</span> world</text><table style=\"myTable\">
6-
7- brown
8- two fish
9- BarBaz
10- 你好嗎...
11-
12- Hello Rosetta Code world
```



## Visual Basic

{{works with|Visual Basic|6}}

```vb
Public Function TextBetween(ByVal Text As String, ByVal StartDelim As String, ByVal EndDelim As String) As String
Dim indS As Long
Dim indE As Long

  If StartDelim = "start" Then
    indS = 1
  Else
    indS = InStr(1, Text, StartDelim)
    If indS Then
      indS = indS + Len(StartDelim)
    End If
  End If

  If indS Then
    If EndDelim = "end" Then
      indE = Len(Text) + 1
    Else
      indE = InStr(indS, Text, EndDelim)
      If indE = 0 Then
        indE = Len(Text) + 1
      End If
    End If
    indE = indE - indS
    If indE Then
      TextBetween = Mid$(Text, indS, indE)
    End If
  End If

End Function

' *********************

Sub Main()
' tests
Dim Text As String
Dim StartDelim As String
Dim EndDelim As String
Dim Expected As String

'Ex. 1
Text = "Hello Rosetta Code world"
StartDelim = "Hello "
EndDelim = " world"
Expected = "Rosetta Code"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Ex. 2
Text = "Hello Rosetta Code world"
StartDelim = "start"
EndDelim = " world"
Expected = "Hello Rosetta Code"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Ex. 3
Text = "Hello Rosetta Code world"
StartDelim = "Hello "
EndDelim = "end"
Expected = "Rosetta Code world"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Ex. 4
Expected = ChrW$(&H4F60) & ChrW$(&H597D) & ChrW$(&H55CE)
Text = "</div><div style=""chinese"">" & Expected & "</div>"
StartDelim = "<div style=""chinese"">"
EndDelim = "</div>"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Ex. 5
Text = "<text>Hello <span>Rosetta Code</span> world</text><table style=""myTable"">"
StartDelim = "<text>"
EndDelim = "<table>"
Expected = "Hello <span>Rosetta Code</span> world</text><table style=""myTable"">"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Ex. 6
Text = "<table style=""myTable""><tr><td>hello world</td></tr></table>"
StartDelim = "<table>"
EndDelim = "</table>"
Expected = ""
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Ex. 7
Text = "The quick brown fox jumps over the lazy other fox"
StartDelim = "quick "
EndDelim = " fox"
Expected = "brown"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Ex. 8
Text = "One fish two fish red fish blue fish"
StartDelim = "fish "
EndDelim = " red"
Expected = "two fish"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Ex. 9
Text = "FooBarBazFooBuxQuux"
StartDelim = "Foo"
EndDelim = "Foo"
Expected = "BarBaz"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected

'Extra test: empty text between delimiters
StartDelim = "Foo"
EndDelim = "BarBaz"
Expected = ""
Text = StartDelim & Expected & EndDelim & "FooBuxQuux"
Debug.Assert TextBetween(Text, StartDelim, EndDelim) = Expected
End Sub
```



## zkl


```zkl
fcn getText(text,start,end){
   s = (if((s:=text.find(start))==Void) 0 else s + start.len());
   e = (if((e:=text.find(end,s))==Void) text.len() else e);
   text[s,e - s]
}
getText("Hello Rosetta Code world","Hello "," world").println();
getText("Hello Rosetta Code world","start", " world").println();
getText("Hello Rosetta Code world","Hello",  "end" ).println();
```

{{out}}

```txt

Rosetta Code
Hello Rosetta Code
 Rosetta Code world

```


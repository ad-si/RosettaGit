+++
title = "Tokenize a string with escaping"
description = ""
date = 2019-10-14T14:39:02Z
aliases = []
[extra]
id = 17730
[taxonomies]
categories = ["task", "String manipulation"]
tags = []
languages = [
  "arturo",
  "awk",
  "bbc_basic",
  "c",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dyalect",
  "elena",
  "fortran",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lingo",
  "lua",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "tcl",
  "vba",
  "zkl",
]
+++

## Task

Write a function or program that can split a string at each non-escaped occurrence of a separator character.

It should accept three input parameters:
* The <b>string</b>
* The <b>separator character</b>
* The <b>escape character</b>



It should output a list of strings.

Rules for splitting:
* The fields that were separated by the separators, become the elements of the output list.
* Empty fields should be preserved, even at the start and end.



Rules for escaping:
* "Escaped" means preceded by an occurrence of the escape character that is not already escaped itself.
* When the escape character precedes a character that has no special meaning, it still counts as an escape (but does not do anything special).
* Each occurrences of the escape character that was used to escape something, should '''not''' become part of the output.

Demonstrate that your function satisfies the following test-case:
{| class="wikitable"
|-
! Input
! Output
|-
| style="vertical-align:top" |
{| style="border-collapse:collapse; border:none" border="0"
|-
| style="border:none; text-align:right" | string:
| style="border:none" | <pre style="display:inline;padding:0.1em;margin:0.3em;">one^|uno||three^^^^|four^^^|^cuatro|
```

|-
| style="border:none; text-align:right" | separator character:
| style="border:none" | <pre style="display:inline;padding:0.1em;margin:0.3em;">|
```

|-
| style="border:none; text-align:right" | escape character:
| style="border:none" | <pre style="display:inline;padding:0.1em;margin:0.3em;">^
```

|}
|
{| style="border-collapse:collapse; border:none" border="0"
|-
| style="border:none" | <pre style="display:inline;padding:0.1em;margin:0.3em;">one|uno
```

|-
| style="border:none" | <pre style="display:inline;padding:0.1em;margin:0.3em;">
```

|-
| style="border:none" | <pre style="display:inline;padding:0.1em;margin:0.3em;">three^^
```

|-
| style="border:none" | <pre style="display:inline;padding:0.1em;margin:0.3em;">four^|cuatro
```

|-
| style="border:none" | <pre style="display:inline;padding:0.1em;margin:0.3em;">
```

|}
|}
(Print the output list in any format you like, as long as it is it easy to see what the fields are.)

* [[Tokenize a string]]
* [[Brace expansion]]




=={{Header|AppleScript}}==

```AppleScript
-- TOKENIZE ------------------------------------------------------------------

-- tokenize :: String -> Character -> Character -> [String]
on tokenize(str, chrDelim, chrEsc)

    script charParse
        -- Record: {esc:Bool, token:String, tokens:[String]}
        -- charParse :: Record -> Character -> Record
        on |λ|(a, x)
            set blnEsc to esc of a
            set blnEscChar to ((not blnEsc) and (x = chrEsc))

            if ((not blnEsc) and (x = chrDelim)) then
                set strToken to ""
                set lstTokens to (tokens of a) & token of a
            else
                set strToken to (token of a) & cond(blnEscChar, "", x)
                set lstTokens to tokens of (a)
            end if

            {esc:blnEscChar, token:strToken, tokens:lstTokens}
        end |λ|
    end script

    set recParse to foldl(charParse, ¬
        {esc:false, token:"", tokens:[]}, splitOn("", str))

    tokens of recParse & token of recParse
end tokenize


-- TEST ----------------------------------------------------------------------
on run
    script numberedLine
        on |λ|(a, s)
            set iLine to lineNum of a
            {lineNum:iLine + 1, report:report of a & iLine & ":" & tab & s & linefeed}
        end |λ|
    end script

    report of foldl(numberedLine, {lineNum:1, report:""}, ¬
        tokenize("one^|uno||three^^^^|four^^^|^cuatro|", "|", "^"))
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- cond :: Bool -> a -> a -> a
on cond(bool, f, g)
    if bool then
        f
    else
        g
    end if
end cond
```

```txt
1:    one|uno
2:
3:    three^^
4:    four^|cuatro
5:

```


## Arturo


```arturo
tokenize [s,sep,esc]{
	escaping 0
	loop $(range 0 $(size s)-1) [i]{
		chr $(get $(characters s) i)

		if escaping=1 {
			print chr true
			escaping 0
		} {
			if chr=sep {
				print ""

			} {
				if chr=esc {
					escaping 1
				} {
					print chr true
				}
			}
		}
	}
	print ""
}

str "one^|uno||three^^^^|four^^^|^cuatro|"
print $(tokenize str "|" "^")
```


```txt
one|uno

three^^
four^|cuatro


```



## AWK


```AWK

# syntax: GAWK -f TOKENIZE_A_STRING_WITH_ESCAPING.AWK
BEGIN {
    tokenize("one^|uno||three^^^^|four^^^|^cuatro|","|","^")
    exit(0)
}
function tokenize(str,sep,esc,  chr,escaping,field,i) {
    printf(">%s<\n",str)
    printf("%02d: >",++field)
    for (i=1; i<=length(str); i++) {
      chr = substr(str,i,1)
      if (escaping == 1) {
        printf("%s",chr)
        escaping = 0
      }
      else if (chr == sep) {
        printf("<\n%02d: >",++field)
      }
      else if (chr == esc) {
        escaping = 1
      }
      else {
        printf("%s",chr)
      }
    }
    printf("<\n")
}

```

```txt

>one^|uno||three^^^^|four^^^|^cuatro|<
01: >one|uno<
02: ><
03: >three^^<
04: >four^|cuatro<
05: ><

```



## BBC BASIC


```bbcbasic>REM
tokenizer
PROC_tokenize("one^|uno||three^^^^|four^^^|^cuatro|", "|", "^")
END
:
DEF PROC_tokenize(src$, sep$, esc$)
LOCAL field%, char$, escaping%, i%
field% = 1
escaping% = FALSE
PRINT field%; " ";
FOR i% = 1 TO LEN src$
  char$ = MID$(src$, i%, 1)
  IF escaping% THEN
    PRINT char$;
    escaping% = FALSE
  ELSE
    CASE char$ OF
    WHEN sep$
      PRINT
      field% += 1
      PRINT field%; " ";
    WHEN esc$
      escaping% = TRUE
    OTHERWISE
      PRINT char$;
    ENDCASE
  ENDIF
NEXT
PRINT
ENDPROC
```

```txt
         1 one|uno
         2
         3 three^^
         4 four^|cuatro
         5
```



## C

```cpp
#include <iostream>
#include <stdio.h>

#define STR_DEMO "one^|uno||three^^^^|four^^^|^cuatro|"
#define SEP '|'
#define ESC '^'

typedef char* Str; /* just for an easier reading */

/*
### > FUNCTION PROTOTYPES <=============================================
 */
unsigned int ElQ( const char *s, char sep, char esc );
Str *Tokenize( char *s, char sep, char esc, unsigned int *q );

/*
### ========================================================================

Main function.
Just passes a copy of the STR_DEMO string to the tokenization function and shows
the results.

### ========================================================================
*/

int main() {
    char s[] = STR_DEMO;
    unsigned int i, q;

    Str *list = Tokenize( s, SEP, ESC, &q );

    if( list != NULL ) {
        printf( "\n Original string: %s\n\n", STR_DEMO );
        printf( " %d tokens:\n\n", q );

        for( i=0; i<q; ++i )
            printf( " %4d. %s\n", i+1, list[i] );

        free( list );
    }

    return 0;
}

/*
### ========================================================================

"ElQ" stands for "Elements Quantity". Counts the amount of valid element in the
string s, according to the separator character provided in sep and the escape
character provided in esc.

### ========================================================================
*/

unsigned int ElQ( const char *s, char sep, char esc ) {
    unsigned int q, e;
    const char *p;

    for( e=0, q=1, p=s; *p; ++p ) {
        if( *p == esc )
            e = !e;
        else if( *p == sep )
            q += !e;
        else e = 0;
    }

    return q;
}

/*
### ========================================================================

The actual tokenization function.
Allocates as much dynamic memory as needed to contain the pointers to the
tokenized portions of the string passed as the "s" parameter, then looks for the
separators characters sep, paying attention to the occurrences of the escape
character provided in esc. When a valid separator is found, the function swaps
it with a '\0' terminator character and stores the pointer to the next string
into the array of pointers in dynamic memory. On output, the value of *q is the
number of pointers in the array. The caller is responsible for deallocating with
free() the returned array of pointers when it is no longer needed.
In case of failure, NULL is returned.

### ========================================================================
*/

Str *Tokenize( char *s, char sep, char esc, unsigned int *q ) {
    Str *list = NULL;

    *q = ElQ( s, sep, esc );
    list = malloc( *q * sizeof(Str) );

    if( list != NULL ) {
        unsigned int e, i;
        char *p;

        i = 0;
        list[i++] = s;

        for( e=0, p=s; *p; ++p ) {
            if( *p == esc ) {
                e = !e;
            }
            else if( *p == sep && !e ) {
                list[i++] = p+1;
                *p = '\0';
            }
            else {
                e = 0;
            }
        }
    }

    return list;
}
```


```txt

 Original string: one^|uno||three^^^^|four^^^|^cuatro|

 5 tokens:

    1. one^|uno
    2.
    3. three^^^^
    4. four^^^|^cuatro
    5.

```



## C++


```cpp
#include <iostream>
#include <string>
#include <vector>

using namespace std;

vector<string> tokenize(string input, char seperator, char escape) {
	vector<string> output;
	string token = "";

	bool inEsc = false;
	for (char ch : input) {
		if (inEsc) {
			inEsc = false;
		}
		else if (ch == escape) {
			inEsc = true;
			continue;
		}
		else if (ch == seperator) {
			output.push_back(token);
			token = "";
			continue;
		}
		token += ch;
	}
	if (inEsc) {
		throw new exception("Invalid terminal escape");
	}

	output.push_back(token);
	return output;
}

int main() {
	string sample = "one^|uno||three^^^^|four^^^|^cuatro|";

	cout << sample << endl;
	cout << "[";
	for (auto t : tokenize(sample, '|', '^')) {
		cout << '"' << t << "\", ";
	}
	cout << "]" << endl;

	return 0;
}
```

```txt
one^|uno||three^^^^|four^^^|^cuatro|
["one|uno", "", "three^^", "four^|cuatro", "", ]
```



## C#


```c#
using System;
using System.Text;
using System.Collections.Generic;

public class TokenizeAStringWithEscaping
{
    public static void Main() {
        string testcase = "one^|uno||three^^^^|four^^^|^cuatro|";
        foreach (var token in testcase.Tokenize(separator: '|', escape: '^')) {
            Console.WriteLine(": " + token); //Adding a : so we can see empty lines
        }
    }
}

public static class Extensions
{
    public static IEnumerable<string> Tokenize(this string input, char separator, char escape) {
        if (input == null) yield break;
        var buffer = new StringBuilder();
        bool escaping = false;
        foreach (char c in input) {
            if (escaping) {
                buffer.Append(c);
                escaping = false;
            } else if (c == escape) {
                escaping = true;
            } else if (c == separator) {
                yield return buffer.Flush();
            } else {
                buffer.Append(c);
            }
        }
        if (buffer.Length > 0 || input[input.Length-1] == separator) yield return buffer.Flush();
    }

    public static string Flush(this StringBuilder stringBuilder) {
        string result = stringBuilder.ToString();
        stringBuilder.Clear();
        return result;
    }
}
```

```txt

: one|uno
:
: three^^
: four^|cuatro
:
```



## COBOL


```cobol>       >
SOURCE FORMAT FREE
identification division.
program-id. 'tokenizewithescaping'.
environment division.
configuration section.
repository.
    function all intrinsic.
data division.
working-storage section.

01 escape-char pic x value '^'.
01 separator-char pic x value '|'.
01 reference-string pic x(64) value
   'one^|uno||three^^^^|four^^^|^cuatro|'.

01 input-string pic x(64).
01 c pic 99.
01 escaped pic x.

01 t pic 99.
01 t-max pic 99.
01 t-lim pic 99 value 32.
01 token-entry occurs 32.
   03  token-len pic 99.
   03  token pic x(16).

01 l pic 99.
01 l-lim pic 99 value 16.

01 error-found pic x.

procedure division.
start-tokenize-with-escaping.

    move reference-string to input-string
    perform tokenize

    move 'token' to input-string
    perform tokenize

    move '^^^^^^^^' to input-string
    perform tokenize

    move '||||||||' to input-string
    perform tokenize

    move all 'token' to input-string
    perform tokenize

    move all 't|' to input-string
    perform tokenize

    move spaces to input-string
    perform tokenize

    display space

    stop run
    .
tokenize.
    display space
    display 'string:'
    display input-string

    move 'N' to escaped error-found
    move 1 to t-max
    initialize token-entry(t-max)
    move 0 to l

    perform varying c from 1 by 1 until
    c > length(input-string)
    or input-string(c:) = spaces

        evaluate escaped also input-string(c:1)
        when 'N' also escape-char
            move 'Y' to escaped
        when 'N' also separator-char
            perform increment-t-max
            if error-found = 'Y'
                exit paragraph
            end-if
        when 'N' also any
            perform move-c
            if error-found = 'Y'
                exit paragraph
            end-if
        when 'Y' also any
            perform move-c
            if error-found = 'Y'
                exit paragraph
            end-if
            move 'N' to escaped
        end-evaluate

    end-perform
    if l > 0
        move l to token-len(t-max)
    end-if

    if c = 1
        display 'no tokens'
    else
        display 'tokens:'
        perform varying t from 1 by 1 until t > t-max
            if token-len(t) > 0
                display t ': ' token-len(t) space token(t)
            else
                display t ': ' token-len(t)
            end-if
        end-perform
    end-if
    .
increment-t-max.
    if t-max >= t-lim
        display 'error: at ' c ' number of tokens exceeds ' t-lim
        move 'Y' to error-found
    else
        move l to token-len(t-max)
        add 1 to t-max
        initialize token-entry(t-max)
        move 0 to l
        move 'N' to error-found
    end-if
    .
move-c.
    if l >= l-lim
        display 'error: at ' c ' token length exceeds ' l-lim
        move 'Y' to error-found
    else
        add 1 to l
        move input-string(c:1) to token(t-max)(l:1)
        move 'N' to error-found
    end-if
    .
end program 'tokenizewithescaping'.

```


```txt

$ cobc -xj tokenizewithescaping.cbl

string:
one^|uno||three^^^^|four^^^|^cuatro|
tokens:
01: 07 one|uno
02: 00
03: 07 three^^
04: 12 four^|cuatro
05: 00

string:
token
tokens:
01: 05 token

string:
^^^^^^^^
tokens:
01: 04 ^^^^

string:
||||||||
tokens:
01: 00
02: 00
03: 00
04: 00
05: 00
06: 00
07: 00
08: 00
09: 00

string:
tokentokentokentokentokentokentokentokentokentokentokentokentoke
error: at 17 token length exceeds 16

string:
t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|t|
error: at 64 number of tokens exceeds 32

string:

no tokens

```



## Common Lisp


```lisp
(defun split (input separator escape)
  (flet ((make-string-buffer ()
           (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
    (loop with token = (make-string-buffer)
          with result = nil
          with to-be-escaped = nil
          for ch across input
          do (cond (to-be-escaped
                    (vector-push-extend ch token)
                    (setf to-be-escaped nil))
                   ((char= ch escape)
                    (setf to-be-escaped t))
                   ((char= ch separator)
                    (push token result)
                    (setf token (make-string-buffer)))
                   (t
                    (vector-push-extend ch token)))
          finally (push token result)
                  (return (nreverse result)))))

(defun main ()
  (dolist (token (split "one^|uno||three^^^^|four^^^|^cuatro|" #\| #\^))
    (format t "'~A'~%" token)))
```

```txt
'one|uno'
''
'three^^'
'four^|cuatro'
''
```



## D

```D
import std.stdio;

void main() {
    string sample = "one^|uno||three^^^^|four^^^|^cuatro|";

    writeln(sample);
    writeln(tokenizeString(sample, '|', '^'));
}

auto tokenizeString(string source, char seperator, char escape) {
    import std.array : appender;
    import std.exception : enforce;

    auto output = appender!(string[]);
    auto token = appender!(char[]);

    bool inEsc;
    foreach(ch; source) {
        if (inEsc) {
            inEsc = false;
        } else if (ch == escape) {
            inEsc = true;
            continue;
        } else if (ch == seperator) {
            output.put(token.data.idup);
            token.clear();
            continue;
        }
        token.put(ch);
    }
    enforce(!inEsc, "Invalid terminal escape");

    output.put(token.data.idup);
    return output.data;
}
```


```txt
one^|uno||three^^^^|four^^^|^cuatro|
["one|uno", "", "three^^", "four^|cuatro", ""]
```



## Dyalect

```dyalect
func String.tokenize(separator, escape) {
    var buffer = []
    var escaping = false
    for c in this {
        if escaping {
            buffer.add(c)
            escaping = false
        } else if c == escape {
            escaping = true
        } else if c == separator {
            yield buffer.flush();
        } else {
            buffer.add(c);
        }
    }

    if buffer.len() > 0 || this[this.len() - 1] == separator {
        yield buffer.flush()
    }
}

func Array.flush() {
    var str = String.concat(values: this)
    this.clear()
    str
}

const testcase = "one^|uno||three^^^^|four^^^|^cuatro|";
for token in testcase.tokenize(separator: '|', escape: '^') {
    print(": \(token)")
}
```


```txt
: one|uno
:
: three^^
: four^|cuatro
:
```



## Elena

ELENA 4.x :

```elena
import extensions;
import extensions'routines;
import system'collections;
import system'routines;
import system'text;

extension op : String
{
    tokenize(separator,escape)
    {
        auto buffer := new TextBuilder();
        auto list := new ArrayList();

        bool escaping := false;
        self.forEach:(ch)
        {
            if (escaping)
            {
                buffer.write:ch;
                escaping := false
            }
            else if (ch == escape)
            {
                escaping := true
            }
            else if (ch == separator)
            {
                list.append(buffer.Value);
                buffer.clear()
            }
            else
            {
                buffer.write:ch
            }
        };

        ^ list
    }
}

const string testcase = "one^|uno||three^^^^|four^^^|^cuatro|";

public program()
{
    testcase.tokenize("|", "^").forEach:printingLn
}
```

```txt

one|uno

three^^
four^|cuatro

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.Text.RegularExpressions

(*
    .NET regexes have unlimited look-behind, so we can look for separators
    which are preceeded by an even number of (or no) escape characters
*)
let split esc sep s =
    Regex.Split (
        s,
        String.Format("(?<=(?:\b|[^{0}])(?:{0}{0})*){1}", Regex.Escape(esc), Regex.Escape(sep))
        )

let unescape esc s =
    Regex.Replace(
        s,
        Regex.Escape(esc) + "(.)",
        "$1"
        )

[<EntryPoint>]
let main argv =
    let (esc, sep) = ("^", "|")
    "one^|uno||three^^^^|four^^^|^cuatro|"
    |> split esc sep
    |> Seq.map (unescape esc)
    |> Seq.iter (fun s -> printfn "'%s'" s)
    0
```

```txt
'one|uno'
''
'three^^'
'four^|cuatro'
''
```



## Fortran

First Fortran (1958) offered no facilities for inspecting or manipulating text, until Fortran IV when the <code>A</code> format code was introduced whereby text could be read or written from numeric variables. The difficulties and incompatibilities between different computers were eased with F77 that offered CHARACTER*n variables, though they are not quite strings that have a varying length. F95 introduces the ability to define a compound entity such as a string and F2003 standardised a version of strings whereby with each assignment to such a variable, it would be re-allocated with the required amount of storage. Otherwise, one proceeds with CHARACTER variables and an associated variable containing its current length as with <code>TOKEN</code> and <code>L</code>. However, when passed to subroutines (or functions) as a parameter, a CHARACTER variable is supplied along with a secret additional parameter giving the size of the variable, and this is stringlike, so long as there is no need to change the length. Thus, the length of parameter TEXT to subroutine SPLIT can be found via LEN(TEXT).

The source style is F90 simply for the convenience of having subroutine SPLOT defined within subroutine SPLIT so as to gain access to certain variables. If separate subroutines were to be used, then there would have to be parameters or COMMON variables, or, one could just replicate the code within SPLIT. A further F90 feature involves declaring the size of internal variable <code>TOKEN</code> to be <code>LEN(TEXT)</code>, which is surely the largest it could be. Otherwise, one would have to select some "surely big enough" value.
```Fortran
      SUBROUTINE SPLIT(TEXT,SEP,ESC)	!Identifies and prints tokens from within a text.
       CHARACTER*(*) TEXT	!To be scanned.
       CHARACTER*(1) SEP	!The only separator for tokens.
       CHARACTER*(1) ESC	!Miscegnator.
       CHARACTER*(LEN(TEXT)) TOKEN	!Surely sufficient space.
       INTEGER N	!Counts the tokens as they're found.
       INTEGER I	!Steps through the text.
       INTEGER L	!Length of the token so far accumulated.
       LOGICAL ESCAPING	!Miscegnatory state.
        N = 0		!No tokens so far.
        L = 0		!Nor any text for the first.
        ESCAPING = .FALSE.	!And the state is good.
        DO I = 1,LEN(TEXT)	!Step through the text.
          IF (ESCAPING) THEN	!Are we in a mess?
            L = L + 1			!Yes. An ESC character had been seen.
            TOKEN(L:L) = TEXT(I:I)	!So, whatever follows is taken as itself.
            ESCAPING = .FALSE.		!There are no specially-recognised names.
           ELSE			!Otherwise, we're in text to inspect.
            IF (TEXT(I:I).EQ.ESC) THEN	!So, is it a troublemaker?
             ESCAPING = .TRUE.			!Yes! Trouble is to follow.
            ELSE IF (TEXT(I:I).EQ.SEP) THEN	!If instead a separator,
             CALL SPLOT				!Then the token up to it is complete.
            ELSE			!Otherwise, a simple constituent character.
             L = L + 1				!So, count it in.
             TOKEN(L:L) = TEXT(I:I)		!And copy it in.
            END IF			!So much for grist.
          END IF		!So much for that character.
        END DO			!On to the next.
Completes on end-of-text with L > 0, or, if the last character had been SEP, a null token is deemed to be following.
        CALL SPLOT	!Tail end.
       CONTAINS	!Save on having two copies of this code.
        SUBROUTINE SPLOT	!Show the token and scrub.
         N = N + 1			!Another one.
         WRITE (6,1) N,TOKEN(1:L)	!Reveal.
    1    FORMAT ("Token ",I0," >",A,"<")!Fancy layout.
         L = 0				!Prepare for a fresh token.
        END SUBROUTINE SPLOT	!A brief life.
      END SUBROUTINE SPLIT	!And then oblivion.

      PROGRAM POKE

      CALL SPLIT("one^|uno||three^^^^|four^^^|^cuatro|","|","^")

      END
```


The output has the text of the tokens marked >thus<

```txt

Token 1 >one|uno<
Token 2 ><
Token 3 >three^^<
Token 4 >four^|cuatro<
Token 5 ><

```

The terminating separator character is deemed to mark the start of a null token in the problem's specification. If the text ends without one, then the end-of-text ends the token and the DO-loop quits with L > 0 and so something for SPLOT. If the penultimate character were an ESC followed by a SEP, then the loop also ends with L > 0. If the text ends with a SEP but not preceded by an ESC (as in the example) then L = 0 - but SPLOT is invoked unconditionally. A SEP at the start of the text will also elicit a null token, as will an entirely null text.

If the text ends with an ESC, then this is surely a mistake and could be caught via a test that <code>ESCAPING</code> was ''true'' on exit from the loop. But no error messages are called for...

In this example the DO-loop relentlessly steps through the text, and in general this would not be convenient. Normally, token identification proceeds within a much larger context where one would not discard the token immediately after it is isolated, and rather than copying the text hither and thither, one might prefer to identify it in-place, say with variables <code>L1</code> and <code>L2</code> identifying the start and end positions within the working area. In such a case there would no longer be a need for a variable <code>TOKEN</code> and the angst of deciding on a suitable maximum size. This would also make it easier in any error messages to show context and provenance. However, the bizarre miscegnation of "escape" sequences (especially confusing within text ''literals''), means that the source text does not necessarily constitute the text of the token.


## Go


```go
package main

import (
	"errors"
	"fmt"
)

func TokenizeString(s string, sep, escape rune) (tokens []string, err error) {
	var runes []rune
	inEscape := false
	for _, r := range s {
		switch {
		case inEscape:
			inEscape = false
			fallthrough
		default:
			runes = append(runes, r)
		case r == escape:
			inEscape = true
		case r == sep:
			tokens = append(tokens, string(runes))
			runes = runes[:0]
		}
	}
	tokens = append(tokens, string(runes))
	if inEscape {
		err = errors.New("invalid terminal escape")
	}
	return tokens, err
}

func main() {
	const sample = "one^|uno||three^^^^|four^^^|^cuatro|"
	const separator = '|'
	const escape = '^'

	fmt.Printf("Input:   %q\n", sample)
	tokens, err := TokenizeString(sample, separator, escape)
	if err != nil {
		fmt.Println("error:", err)
	} else {
		fmt.Printf("Tokens: %q\n", tokens)
	}
}
```

```txt

Input:   "one^|uno||three^^^^|four^^^|^cuatro|"
Tokens: ["one|uno" "" "three^^" "four^|cuatro" ""]

```



## Haskell



###  Deterministic Finite Automaton



```haskell
splitEsc :: (Foldable t1, Eq t) => t -> t -> t1 t -> [[t]]
splitEsc sep esc = reverse . map reverse . snd . foldl process (0, [[]])
  where process (st, r:rs) ch
          | st == 0 && ch == esc               = (1,      r:rs)
          | st == 0 && ch == sep               = (0,   []:r:rs)
          | st == 1 && sep == esc && ch /= sep = (0, [ch]:r:rs)
          | otherwise                          = (0, (ch:r):rs)
```


```txt
λ> splitEsc '|' '^' "one^|uno||three^^^^|four^^^|^cuatro|"
["one|uno","","three^^","four^|cuatro",""]
```


The solution works with any foldable structures.

```txt
λ> splitEsc 11 0 [2,3,11,3,4,5,11,0,11,2,3,4]
[[2,3],[3,4,5],[11,2,3,4]]
```


It handles pathological case when separator and escape are the same:

```txt
λ> split '|' '|' "one^|uno||three^^^^|four^^^|^cuatro|"
["one^","uno|three^^^^","four^^^","^cuatro"]
```


For splitting lists without escaping see <tt>Data.List.Split</tt> package.

=== Counduit-based solution ===

Constant in space (~ O(k), where k -- is token length), as fast as DFA-based solution.


```haskell
{-#Language LambdaCase #-}
import Conduit

splitEscC :: (Monad m, Eq t) => t -> t -> Conduit t m [t]
splitEscC sep esc = mapOutput reverse $ go True []
  where
    go notEsc b = await >>= \case
      Nothing -> yield b
      Just ch | notEsc && ch == esc -> go False b
              | notEsc && ch == sep -> yield b >> go True []
              | otherwise -> go True (ch:b)
```


This new conduit could be used in a pipeline as follows:


```haskell
main = runConduit $
  yieldMany "one^|uno||three^^^^|four^^^|^cuatro|"
  .| splitEscC '|' '^'
  .| mapM_C print
```



```txt
λ> main
"one|uno"
""
"three^^"
"four^|cuatro"
""
```



### Alternative

This is essentially equivalent to the first (DFA) example, but, though possibly less elegant than the guard idiom, appears to be fractionally faster with larger (eg 180k) test strings.

```haskell
tokenize
  :: (Eq a, Foldable t)
  => a -> a -> t a -> [[a]]
tokenize delim esc str = reverse $ reverse <$> (token : list)
  where
    (token, list, _) =
      foldl
        (\(aToken, aList, aEsc) x ->
            let literal = not aEsc
                isEsc = literal && (x == esc)
            in if literal && (x == delim)
                 then ([], aToken : aList, isEsc)
                 else ( if isEsc
                          then aToken
                          else x : aToken
                      , aList
                      , isEsc))
        ([], [], False)
        str

main :: IO ()
main = mapM_ print $ tokenize '|' '^' "one^|uno||three^^^^|four^^^|^cuatro|"
```


```txt
"one|uno"
""
"three^^"
"four^|cuatro"
""
```



## J


From the python example:

```J

tokenize1=: tokenize =: '^|'&$: :(4 : 0)
 'ESC SEP' =. x
 STATE =. 0
 RESULT =. 0 $ a:
 TOKEN =. ''
 for_C. y do.
  if. STATE do.
   TOKEN =. TOKEN , C
   STATE =. 0
  else.
   if. C = ESC do.
    STATE =. 1
   elseif. C = SEP do.
    RESULT =. RESULT , < TOKEN
    TOKEN =. ''
   elseif. do.
    TOKEN =. TOKEN , C
   end.
  end.
 end.
 RESULT =. RESULT , < TOKEN
)

```


```txt

tokenize 'one^|uno||three^^^^|four^^^|^cuatro|'
┌───────┬┬───────┬────────────┬┐
│one|uno││three^^│four^|cuatro││
└───────┴┴───────┴────────────┴┘


```


Here's a somewhat more efficient approach (over 100 times faster on a 100k textual example):


```J
tokenize2=: tokenize=:3 :0
  '^|' tokenize2 y  NB. task default escape and separator
:
  'ESC SEP'=. x
  E=. 18 b./\.&.|.ESC=y NB. escape positions
  S=. (SEP=y)>_1}.0,E NB. separator positions
  K=. -.E+.S NB. keep positions
  T=. (#y){. 1,}.S NB. token beginnings
  (T<;.1 K)#&.>T<;.1 y
)
```


Example use:


```J
   '^|' tokenize 'one^|uno||three^^^^|four^^^|^cuatro|'
┌───────┬┬───────┬────────────┬┐
│one|uno││three^^│four^|cuatro││
└───────┴┴───────┴────────────┴┘
```



Solution invoking the sequential machine primitive verb.[[http://jsoftware.com/pipermail/programming/2014-December/040658.html|See this thread.]]
```J
charTokens =: (0;(3 2 2$(2 1 1 1 2 2 1 2 1 0 1 0));<<'^')&;:  NB. sequential machine
splitTokens =: ((<,'|')&= <;._1 ])@:((<,'|'),])
removeExtra =: (}.^:(1<#)) L:0
tokenize3=: tokenize=: ; each @: (removeExtra @: splitTokens @: charTokens)
```
Example use:
```J
   t=: 'one^|uno||three^^^^|four^^^|^cuatro|'

  tokenize t
┌───────┬┬───────┬────────────┬┐
│one|uno││three^^│four^|cuatro││
└───────┴┴───────┴────────────┴┘

   $tokenize t
5
```


Relative efficiencies:


```J
   txt=: 1e5$'one^|uno||three^^^^|four^^^|^cuatro|'

   (%"1 <./) timespacex every 'tokenize1 txt';'tokenize2 txt';'tokenize3 txt'
132.856       1
      1 7.73534
8.29568 19.9766
```


So tokenize2 is the fastest, while tokenize1 uses the least amount of memory. Also, tokenize1 is the slowest and tokenize3 uses the most memory. (First column is relative time used, second is relative space used, rows correspond to implementations.)


## Java

```java
import java.util.*;

public class TokenizeStringWithEscaping {

    public static void main(String[] args) {
        String sample = "one^|uno||three^^^^|four^^^|^cuatro|";
        char separator = '|';
        char escape = '^';

        System.out.println(sample);
        try {
            System.out.println(tokenizeString(sample, separator, escape));
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    public static List<String> tokenizeString(String s, char sep, char escape)
            throws Exception {
        List<String> tokens = new ArrayList<>();
        StringBuilder sb = new StringBuilder();

        boolean inEscape = false;
        for (char c : s.toCharArray()) {
            if (inEscape) {
                inEscape = false;
            } else if (c == escape) {
                inEscape = true;
                continue;
            } else if (c == sep) {
                tokens.add(sb.toString());
                sb.setLength(0);
                continue;
            }
            sb.append(c);
        }
        if (inEscape)
            throw new Exception("Invalid terminal escape");

        tokens.add(sb.toString());

        return tokens;
    }
}
```



```txt
[one|uno, , three^^, four^|cuatro, ]
```



## JavaScript


### ES5


### =Iterative=


```JavaScript
function tokenize(s, esc, sep) {
	for (var a=[], t='', i=0, e=s.length; i<e; i+=1) {
		var c = s.charAt(i)
		if (c == esc) t+=s.charAt(++i)
		else if (c != sep) t+=c
		else a.push(t), t=''
	}
	a.push(t)
	return a
}

var s = 'one^|uno||three^^^^|four^^^|^cuatro|'
document.write(s, '
')
for (var a=tokenize(s,'^','|'), i=0; i<a.length; i+=1) document.write(i, ': ', a[i], '
')
```

```txt
one^|uno||three^^^^|four^^^|^cuatro|
0: one|uno
1:
2: three^^
3: four^|cuatro
4:

```



### =Functional=


```JavaScript
(function () {
    'use strict';

    // tokenize :: String -> Character -> Character -> [String]
    function tokenize(str, charDelim, charEsc) {
        var dctParse = str.split('')
            .reduce(function (a, x) {

                var blnEsc = a.esc,
                    blnBreak = !blnEsc && x === charDelim,
                    blnEscChar = !blnEsc && x === charEsc;

                return {
                    esc: blnEscChar,
                    token: blnBreak ? '' : (
                        a.token + (blnEscChar ? '' : x)
                    ),
                    list: a.list.concat(blnBreak ? a.token : [])
                };
            }, {
                esc: false,
                token: '',
                list: []
            });

        return dctParse.list.concat(
            dctParse.token
        );
    }

    return tokenize(
            'one^|uno||three^^^^|four^^^|^cuatro|',
            '|','^'
        )
        .join('\n');

})();
```

```txt
one|uno

three^^
four^|cuatro


```



### ES6

{{Trans|Haskell}}  (Single fold version)

```JavaScript
((() => {

    // tokenize :: String -> Character -> Character -> [String]
    const tokenize = (charDelim, charEsc, str) => {
        const [token, list, _] = str.split('')
            .reduce(([aToken, aList, aEsc], x) => {
                const
                    blnBreak = !aEsc && x === charDelim,
                    blnEscChar = !aEsc && x === charEsc;

                return [
                    blnBreak ? '' : (
                        aToken + (blnEscChar ? '' : x)
                    ),
                    aList.concat(blnBreak ? aToken : []),
                    blnEscChar
                ];
            }, ['', [], false]);

        return list.concat(token);
    };

    // splitEsc :: String -> [String]
    const splitEsc = str => tokenize('|', '^', str);


    // TEST
    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    return splitEsc(
            'one^|uno||three^^^^|four^^^|^cuatro|',
        )
        .map(show)
        .join('\n');
}))();
```


```txt
"one|uno"
""
"three^^"
"four^|cuatro"
""
```



## jq

```jq
# Tokenize the input using the string "escape" as the prefix escape string
def tokenize(separator; escape):

  # Helper functions:
  # mapper/1 is like map/1, but for each element, $e, in the input array,
  # if $e is an array, then it is inserted,
  # otherwise the elements of ($e|f) are inserted.
  def mapper(f): reduce .[] as $e
    ( [];
      if ($e|type) == "array" then . + [$e] else . + ($e | f) end ) ;

  # interpolate x
  def interpolate(x):
    reduce .[] as $i ([]; . +  [$i, x]) | .[0:-1];

  def splitstring(s; twixt):
    if type == "string" then split(s) | interpolate(twixt)
    else .
    end;

  # concatenate sequences of non-null elements:
  def reform:
    reduce .[] as $x ([];
      if $x == null and .[-1] == null then .[0:-1] + ["", null]
      elif $x == null then . + [null]
      elif .[-1] == null then .[0:-1] + [$x]
      else .[0:-1] +  [ .[-1] + $x ]
      end)
    | if .[-1] == null then .[-1] = "" else . end;

  splitstring(escape + escape; [escape])
  | mapper( splitstring( escape + separator; [separator]) )
  | mapper( splitstring( separator; null ) )
  | map( if type == "string" then split(escape) else . end)
  | flatten
  | reform ;
```


'''Example:'''

```jq
"one^|uno||three^^^^|four^^^|^cuatro|" | tokenize("|"; "^")
```


```sh
$ jq -n -f tokenize.jq
[
  "one|uno",
  "",
  "three^^",
  "four^|cuatro",
  ""
]
```



## Julia

```julia
function tokenize2(s::AbstractString, sep::Char, esc::Char)
    SPE = "\ufffe"
    SPF = "\uffff"
    s = replace(s, "$esc$esc", SPE) |>
        s -> replace(s, "$esc$sep", SPF) |>
        s -> last(s) == esc ? string(replace(s[1:end-1], esc, ""), esc) : replace(s, esc, "")
    return map(split(s, sep)) do token
        token = replace(token, SPE, esc)
        return replace(token, SPF, sep)
    end
end

@show tokenize2("one^|uno||three^^^^|four^^^|^cuatro|", '|', '^')
```


```txt
tokenize2("one^|uno||three^^^^|four^^^|^cuatro|", '|', '^') = String["one|uno", "", "three^^", "four^|cuatro", ""]
```



## Kotlin


```scala
// version 1.1.3

const val SPE = "\ufffe"  // unused unicode char in Specials block
const val SPF = "\uffff"  // ditto

fun tokenize(str: String, sep: Char, esc: Char): List<String> {
    var s = str.replace("$esc$esc", SPE).replace("$esc$sep", SPF)
    s = if (s.last() == esc) // i.e. 'esc' not escaping anything
        s.dropLast(1).replace("$esc", "") + esc
    else
        s.replace("$esc", "")
    return s.split(sep).map { it.replace(SPE, "$esc").replace(SPF, "$sep") }
}

fun main(args: Array<String>) {
    var str = "one^|uno||three^^^^|four^^^|^cuatro|"
    val sep = '|'
    val esc = '^'
    val items = tokenize(str, sep, esc)
    for (item in items) println(if (item.isEmpty()) "(empty)" else item)
}
```


```txt

one|uno
(empty)
three^^
four^|cuatro
(empty)

```



## Lingo


```lingo
-- in some movie script

on tokenize (str, sep, esc)
  l = []
  _player.itemDelimiter = sep
  cnt = str.item.count
  repeat with i = 1 to cnt
    prev = l.getLast() -- can be VOID
    if _trailEscCount(prev, esc) mod 2 then
      l[l.count] = prev.char[1..prev.length-1]&sep&str.item[i]
    else
      l.add(str.item[i])
    end if
  end repeat
  -- remove escape characters from tokens
  cnt = l.count
  repeat with i = 1 to cnt
    l[i] = _removeEsc(l[i], esc)
  end repeat
  return l
end

-- counts number of trailing escape characters
on _trailEscCount (str, esc)
  n = 0
  repeat with i = str.length down to 1
    if str.char[i]=esc then n=n+1
    else exit repeat
  end repeat
  return n
end

-- could be implemented more efficiently by using offset()
on _removeEsc (str, esc)
  cnt = str.length-1
  repeat with i = 1 to cnt
    if str.char[i]=esc then
      delete char i of str
      cnt = cnt-1
    end if
  end repeat
  return str
end
```



```lingo
str = "one^|uno||three^^^^|four^^^|^cuatro|"
sep = "|"
esc = "^"
put tokenize(str, sep, esc)
-- ["one|uno", "", "three^^", "four^|cuatro", ""]
```



## Lua


```Lua
function tokenise (str, sep, esc)
    local strList, word, escaped, ch = {}, "", false
    for pos = 1, #str do
        ch = str:sub(pos, pos)
        if ch == esc then
            if escaped then
                word = word .. ch
                escaped = false
            else
                escaped = true
            end
        elseif ch == sep then
            if escaped then
                word = word .. ch
                escaped = false
            else
                table.insert(strList, word)
                word = ""
            end
        else
            escaped = false
            word = word .. ch
        end
    end
    table.insert(strList, word)
    return strList
end

local testStr = "one^|uno||three^^^^|four^^^|^cuatro|"
local testSep, testEsc = "|", "^"
for k, v in pairs(tokenise(testStr, testSep, testEsc)) do
    print(k, v)
end
```

```txt
1       one|uno
2
3       three^^
4       four^|cuatro
5
```



## OCaml



```ocaml
let split_with_escaping ~esc ~sep s =
  let len = String.length s in
  let buf = Buffer.create 16 in
  let rec loop i =
    if i = len then [Buffer.contents buf]
    else if s.[i] = esc && i + 1 < len then begin
      Buffer.add_char buf s.[i + 1];
      loop (i + 2)
    end else if s.[i] = sep then begin
      let s = Buffer.contents buf in
      Buffer.clear buf;
      s :: loop (i + 1)
    end else begin
      Buffer.add_char buf s.[i];
      loop (i + 1)
    end
  in
  loop 0
```


Example:

```ocaml
let res = split_with_escaping ~esc:'^' ~sep:'|' "one^|uno||three^^^^|four^^^|^cuatro|";;
val res : string list = ["one|uno"; ""; "three^^"; "four^|cuatro"; ""]
```




## Perl



The built-in <code>split</code> function can be used with a regex that matches the delimiter ''(although [http://perldoc.perl.org/perlre.html#Special-Backtracking-Control-Verbs advanced backtracking control verbs] are needed to skip escaped delimiters)'':


```perl
sub tokenize {
    my ($string, $sep, $esc) = (shift, quotemeta shift, quotemeta shift);

    my @fields = split /$esc . (*SKIP)(*FAIL) | $sep/sx, $string, -1;
    return map { s/$esc(.)/$1/gsr } @fields;
}
```


A more traditional approach is to parse the input string step by step ''(using a repeatedly-matching regex of the form [http://perldoc.perl.org/perlretut.html#Global-matching <code>/\G.../g</code>])'', and throw away the separators ''(which can be done implicitly using [http://perldoc.perl.org/perlre.html#%28?%3C=pattern%29-\K \K])'':


```perl
    my @fields = $string =~ /\G (?:^ | $sep) \K (?: [^$sep$esc] | $esc .)*/gsx;
```


In both cases, stripping the escape characters happens as a separate step.

Testing:


```perl
print "'$_'\n" for tokenize("one^|uno||three^^^^|four^^^|^cuatro|", '|', '^');
```


```txt

'one|uno'
''
'three^^'
'four^|cuatro'
''

```



## Perl 6



```perl6
sub tokenize ($string, :$sep!, :$esc!) {
    return $string.match(/([ <!before $sep | $esc> . | $esc . ]*)+ % $sep/)\
                  .[0].map(*.subst: /$esc )> ./, '', :g);
}

say "'$_'" for tokenize 'one^|uno||three^^^^|four^^^|^cuatro|', sep => '|', esc => '^';
```


```txt

'one|uno'
''
'three^^'
'four^|cuatro'
''

```


Notable Perl 6 innovations that make this different from the equivalent [[#Perl]] solution:

* string variables can be safely interpolated into regexes without having to 'quotemeta' them
* regexes matches return a nested <code>Match</code> object which allows retrieving ''all'' results for a given capture group ''(rather than just the last thing that it matched)'', thus getting rid of the need for repeated global matching
* the <code>&lt;field&gt;+ % &lt;delimiter&gt;</code> regex construct allows handling the delimiters in a more idiomatic way
* the <code>)&gt;</code> regex construct can be used to exclude anything that follows it from the returned match result


## Phix


```Phix
function tokenize(string s, integer sep, integer esc)
sequence ret = {}
string this = ""
integer skip = 0

    if length(s)!=0 then
        for i=1 to length(s) do
            integer si = s[i]
            if skip then
                this &= si
                skip = 0
            elsif si=esc then
                skip = 1
            elsif si=sep then
                ret = append(ret,this)
                this = ""
            else
                this &= si
            end if
        end for
        ret = append(ret,this)
    end if
    return ret
end function

?tokenize("one^|uno||three^^^^|four^^^|^cuatro|",'|','^')
```

```txt

{"one|uno","","three^^","four^|cuatro",""}

```



## PicoLisp


```PicoLisp
(de tokenize (Str Sep Esc)
   (split
      (make
         (for (L (chop Str)  L)
            (let C (pop 'L)
               (cond
                  ((= C Esc) (link (pop 'L)))
                  ((= C Sep) (link 0))
                  (T (link C)) ) ) ) )
      0 ) )
```

Test:

```PicoLisp
(for (I . S) (tokenize "one\^|uno||three\^\^\^\^|four\^\^\^|\^cuatro|" "|" "\^")
   (prinl I ": " S) )
```

Output:

```txt
1: one|uno
2:
3: three^^
4: four^|cuatro
5:
```



## PowerShell


```PowerShell

function Split-String ([string]$String, [char]$Separator, [char]$Escape)
{
    if ($String -notmatch "\$Separator|\$Escape") {return $String}

    [bool]$escaping = $false
    [string]$output = ""

    for ($i = 0; $i -lt $String.Length; $i++)
    {
        [char]$character = $String.Substring($i,1)

        if ($escaping)
        {
            $output += $character
            $escaping = $false
        }
        else
        {
            switch ($character)
            {
                {$_ -eq $Separator} {$output; $output = ""; break}
                {$_ -eq $Escape}    {$escaping = $true    ; break}
                Default             {$output += $character}
            }
        }
    }

    if ($String[-1] -eq $Separator) {[String]::Empty}
}

```


```PowerShell

Split-String "one^|uno||three^^^^|four^^^|^cuatro|" -Separator "|" -Escape "^" | ForEach-Object `
                                                                                        -Begin   {$n = 0} `
                                                                                        -Process {$n+= 1; "{0}: {1}" -f $n, $_}

```

```txt

1: one|uno
2:
3: three^^
4: four^|cuatro
5:

```



## Python


```python

def token_with_escape(a, escape = '^', separator = '|'):
    '''
        Issue  python -m doctest thisfile.py  to run the doctests.

        >>> print(token_with_escape('one^|uno||three^^^^|four^^^|^cuatro|'))
        ['one|uno', '', 'three^^', 'four^|cuatro', '']
    '''
    result = []
    token = ''
    state = 0
     for c in a:
        if state == 0:
            if c == escape:
                state = 1
            elif c == separator:
                result.append(token)
                token = ''
            else:
                token += c
        elif state == 1:
            token += c
            state = 0
    result.append(token)
    return result

```



## Racket


```racket
#lang racket/base
(require racket/match)

;; Returns a tokenising function based on sep and esc
(define ((tokenise-with-escape sep esc) str)
  (define tsil->string (compose list->string reverse))
  (define (inr rem l-acc acc)
    (match rem
      ['() (if (and (null? acc) (null? l-acc)) null (reverse (cons (tsil->string l-acc) acc)))]
      [(list (== sep)   tl ...) (inr tl null (cons (tsil->string l-acc) acc))]
      [(list (== esc) c tl ...) (inr tl (cons c l-acc) acc)]
      [(list c          tl ...) (inr tl (cons c l-acc) acc)]))
  (inr (string->list str) null null))

;; This is the tokeniser that matches the parameters in the task
(define task-tokeniser (tokenise-with-escape #\| #\^))

(define (report-input-output str)
  (printf "Input:  ~s~%Output: ~s~%~%" str (task-tokeniser str)))

(report-input-output "one^|uno||three^^^^|four^^^|^cuatro|")
(report-input-output "")
(report-input-output "|")
(report-input-output "^")
(report-input-output ".")
```


```txt
Input:  "one^|uno||three^^^^|four^^^|^cuatro|"
Output: ("one|uno" "" "three^^" "four^|cuatro" "")

Input:  ""
Output: ()

Input:  "|"
Output: ("" "")

Input:  "^"
Output: ("^")

Input:  "."
Output: (".")
```



## REXX


### IF/THEN logic


```rexx
/*REXX program demonstrates tokenizing and displaying a string with escaping sequences. */
  str = 'one^|uno||three^^^^|four^^^|^cuatro|'   /*the character string to be tokenized.*/
  esc = '^'                                      /* "    escape  character to be used.  */
  sep = '|'                                      /* "  separator     "      "  "   "    */
  out =                                          /* "  output string  (so far).         */
eMode = 0                                        /*a flag,  escape is in progress.      */

  do j=1  for length(str);  _=substr(str, j, 1)  /*parse a single character at a time.  */
  if eMode   then do; out=out || _;  eMode=0;  iterate;  end   /*are we in escape mode? */
  if _==esc  then do;                eMode=1;  iterate;  end   /*is it an escape char ? */
  if _==sep  then do; call show;               iterate;  end   /* "  " a separator char?*/
  out=out || _                                                 /*append the character.  */
  end   /*j*/

if out\=='' | _==sep  then call show             /*handle a residual str or a separator.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  say  '[length'right(length(out),4)"]"   out;             out=;               return
```

'''output'''

```txt

[length   7] one|uno
[length   0]
[length   7] three^^
[length  12] four^|cuatro
[length   0]

```



### SELECT logic

This REXX version also shows a scale in the output.

```rexx
/*REXX program demonstrates tokenizing and displaying a string with escaping sequences. */
  str = 'one^|uno||three^^^^|four^^^|^cuatro|'   /*the character string to be tokenized.*/
  esc = '^'                                      /* "    escape  character to be used.  */
  sep = '|'                                      /* "  separator     "      "  "   "    */
  $   =                                          /* "  output string  (so far).         */
eMode = 0                                        /*a flag,  escape is in progress.      */
say ' output len        output'                  /*title  verbiage  used for the output.*/
say '──────────── ────────────────────'          /*  "    separator   "   "   "     "   */

  do j=1  for length(str);  _=substr(str, j, 1)  /*parse a single character at a time.  */
      select
      when eMode   then do; $=$ || _;  eMode=0; end      /*are we in in escape  mode?   */
      when _==esc  then                eMode=1           /*is it an  escape  character? */
      when _==sep  then do; call show;          end      /* "  " a separator character? */
      otherwise             $=$ || _                     /*append the single character. */
      end   /*select*/
  end       /*j*/

if $\=='' | _==sep  then call show               /*handle a residual str or a separator.*/
say '──────────── ────────────────────'          /*the foot separator for the output.   */
say '             ····^····1····^····2'          /*show the    top    part of the scale.*/
say '  {scale}    12345678901234567890'          /*  "   "    bottom    "   "  "    "   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  say  '[length'right(length($),4)"]"   $;                 $=;                 return
```

'''output'''

```txt

 output len        output
──────────── ────────────────────
[length   7] one|uno
[length   0]
[length   7] three^^
[length  12] four^|cuatro
[length   0]
──────────── ────────────────────
             ····^····1····^····2
  {scale}    12345678901234567890

```



## Ring


```ring

tokenize("one^|uno||three^^^^|four^^^|^cuatro|", "|", "^")

func tokenize(src, sep, esc)
field = 1
escaping = false
see "" + field + " "
for i = 1 to len(src)
    char = substr(src, i, 1)
    if escaping
       see char
       escaping = false
    else
       switch char
              on sep
                 see nl
                 field = field + 1
                 see "" + field + " "
              on esc
                 escaping = true
              other
                 see char
       off
    ok
next
see nl

```

Output:

```txt

1 one|uno
2
3 three^^
4 four^|cuatro
5

```



## Ruby


I had to drop the \K flag and instead drop the seperator at the beginning manually.
I am not sure if \K is broken or works different than Perl
```ruby

def tokenize(string, sep, esc)
  sep = Regexp.escape(sep)
  esc = Regexp.escape(esc)
  string.scan(/\G (?:^ | #{sep}) (?: [^#{sep}#{esc}] | #{esc} .)*/x).collect do |m|
    m.gsub(/#{esc}(.)/, '\1').gsub(/^#{sep}/, '')
  end
end

p tokenize('one^|uno||three^^^^|four^^^|^cuatro|', '|', '^')


```



## Rust


```rust
const SEPARATOR: char = '|';
const ESCAPE: char = '^';
const STRING: &str = "one^|uno||three^^^^|four^^^|^cuatro|";

fn tokenize(string: &str) -> Vec<String> {
    let mut token = String::new();
    let mut tokens: Vec<String> = Vec::new();
    let mut chars = string.chars();
    while let Some(ch) = chars.next() {
        match ch {
            SEPARATOR => {
                tokens.push(token);
                token = String::new();
            },
            ESCAPE => {
                if let Some(next) = chars.next() {
                    token.push(next);
                }
            },
            _ => token.push(ch),
        }
    }
    tokens.push(token);
    tokens
}

fn main() {
    println!("{:#?}", tokenize(STRING));
}
```

```txt

[
    "one|uno",
    "",
    "three^^",
    "four^|cuatro",
    "",
]

```



## Scala


### Old fashioned Imperative

Imperative with removed (ugly) mutable variables.
```Scala
object TokenizeStringWithEscaping0 extends App {

  val (markerSpE,markerSpF) = ("\ufffe" , "\uffff")

  def tokenize(str: String, sep: String, esc: String): Array[String] = {

    val s0 = str.replace( esc + esc, markerSpE).replace(esc + sep, markerSpF)
    val s = if (s0.last.toString == esc) s0.replace(esc, "") + esc else s0.replace(esc, "")
    s.split(sep.head).map (_.replace(markerSpE, esc).replace(markerSpF, sep))
  }

  def str = "one^|uno||three^^^^|four^^^|^cuatro|"

  tokenize(str, "|", "^").foreach(it => println(if (it.isEmpty) "<empty token>" else it))
}
```



### Idiomatic


### =Functional with Tail recursion=


```Scala
import scala.annotation.tailrec

object TokenizeStringWithEscaping1 extends App {

  def tokenize(str: String, sep: String, esc: String): Seq[String] = {
    @tailrec
    def loop(accu: Seq[String], s: String): Seq[String] = {
      def append2StringInList(char: String): Seq[String] =
        accu.init :+ (accu.last + char)

      s.length match {
        case 0 => accu
        case 1 => if (s.head.toString == sep) accu :+ "" else append2StringInList(s)
        case _ => (s.head.toString, s.tail.head.toString) match {
          case c@((`esc`, `sep`) | (`esc`, `esc`)) => loop(append2StringInList(c._2), s.tail.tail)
          case (`sep`, _)                          => loop(accu :+ "", s.tail)
          case (`esc`, _)                          => loop(accu, s.tail)
          case (sub, _)                            => loop(append2StringInList(sub.head.toString), s.tail)
        }
      }
    }

    loop(Seq(""), str)
  }

  def str = "one^|uno||three^^^^|four^^^|^cuatro|"

  tokenize(str, "|", "^")
    .foreach(it =>
      println(
        f"[length:${it.length}%3d] ${if (it.isEmpty) "<empty token>" else it}"))
}
```


{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/EsIjPQg/0 ScalaFiddle (JavaScript)] or by [https://scastie.scala-lang.org/O3DgMmuOSCS5DD6zQXK7MA Scastie (JVM)].

## Sidef

```ruby
func tokenize(string, sep, esc) {
    var fields = string.split(
        Regex(esc.escape + '.(*SKIP)(*FAIL)|' + sep.escape, 's'), -1
    )
    fields.map{.gsub(Regex(esc + '(.)'), {|s1| s1 }) }
}

tokenize("one^|uno||three^^^^|four^^^|^cuatro|", '|', '^').each { |str|
    say str.dump
}
```

```txt

"one^|uno"
""
"three^^^^"
"four^^^|^cuatro"
""

```



## Tcl

Putting a coroutine in a TclOO object following the "generator pattern" gives a nice structure:

```Tcl
oo::class create tokens {
    constructor {s} {
        puts [coroutine Next my Iter $s]
        oo::objdefine [self] forward next Next
    }
    method Iter {s} {
        yield [info coroutine]
        for {set i 0} {$i < [string length $s]} {incr i} {
            yield [string index $s $i]
        }
        return -code break
    }
}

proc tokenize {s {sep |} {escape ^}} {
    set part ""
    set parts ""
    set iter [tokens new $s]
    while {1} {
        set c [$iter next]
        if {$c eq $escape} {
            append part [$iter next]
        } elseif {$c eq $sep} {
            lappend parts $part
            set part ""
        } else {
            append part $c
        }
    }
    lappend parts $part
    return $parts
}

puts [tokenize one^|uno||three^^^^|four^^^|^cuatro| | ^]
```


```txt
one|uno {} three^^ four^|cuatro {}
```



## VBA

```vb
Private Function tokenize(s As String, sep As String, esc As String) As Collection
    Dim ret As New Collection
    Dim this As String
    Dim skip As Boolean

    If Len(s) <> 0 Then
        For i = 1 To Len(s)
            si = Mid(s, i, 1)
            If skip Then
                this = this & si
                skip = False
            Else
                If si = esc Then
                    skip = True
                Else
                    If si = sep Then
                        ret.Add this
                        this = ""
                    Else
                        this = this & si
                    End If
                End If
            End If
        Next i
        ret.Add this
    End If
    Set tokenize = ret
End Function

Public Sub main()
    Dim out As Collection
    Set out = tokenize("one^|uno||three^^^^|four^^^|^cuatro|", "|", "^")
    Dim outstring() As String
    ReDim outstring(out.Count - 1)
    For i = 0 To out.Count - 1
        outstring(i) = out(i + 1)
    Next i
    Debug.Print Join(outstring, ", ")
End Sub
```
```txt
one|uno, , three^^, four^|cuatro,
```



## zkl

Two simplifying assumptions (since their behavior is undefined): A string ending with an un-escaped escape is an error and 0xff is not an allowed character in the string.

```zkl
fcn tokenize(str,sep,esc){
   sink:=Sink(String);
   foreach c in (str){
      switch(c){
         case(esc){ sink.write(__cWalker.next()); }  // error if ^EoS
	 case(sep){ sink.write("\xff"); }
	 else     { sink.write(c) }
      }
   }
   sink.close().split("\xff");
}
```

Or, if you prefer brevity:

```zkl
fcn tokenize(str,sep,esc){
   sink:=Sink(String);
   foreach c in (str){ sink.write( (c==esc and __cWalker.next()) or (c==sep and "\xff") or c ) }
   sink.close().split("\xff");
}
```


```zkl
tokenize("one^|uno||three^^^^|four^^^|^cuatro|", "|","^").println();
```

```txt
L("one|uno","","three^^","four^|cuatro","")
```


+++
title = "Pangram checker"
description = ""
date = 2019-10-17T23:32:31Z
aliases = []
[extra]
id = 5383
[taxonomies]
categories = []
tags = []
+++

{{task}} [[Category:String manipulation]]
{{omit from|Lilypond}}

A pangram is a sentence that contains all the letters of the English alphabet at least once.

For example:   ''The quick brown fox jumps over the lazy dog''.


;Task:
Write a function or method to check a sentence to see if it is a   [[wp:Pangram|pangram]]   (or not)   and show its use.





## 360 Assembly


```360asm
*        Pangram RC                11/08/2015
PANGRAM  CSECT
         USING  PANGRAM,R12
         LR     R12,R15
BEGIN    LA     R9,SENTENCE
         LA     R6,4
LOOPI    LA     R10,ALPHABET       loop on sentences
         LA     R7,26
LOOPJ    LA     R5,0               loop on letters
         LR     R11,R9
         LA     R8,60
LOOPK    MVC    BUFFER+1(1),0(R10) loop in sentence
         CLC    0(1,R10),0(R11)    if alphabet[j=sentence[i]
         BNE    NEXTK
         LA     R5,1               found
NEXTK    LA     R11,1(R11)         next character
         BCT    R8,LOOPK
         LTR    R5,R5              if found
         BNZ    NEXTJ
         MVI    BUFFER,C'?'        not found
         B      PRINT
NEXTJ    LA     R10,1(R10)         next letter
         BCT    R7,LOOPJ
         MVC    BUFFER(2),=CL2'OK'
PRINT    MVC    BUFFER+3(60),0(R9)
         XPRNT  BUFFER,80
NEXTI    LA     R9,60(R9)          next sentence
         BCT    R6,LOOPI
RETURN   XR     R15,R15
         BR     R14
ALPHABET DC     CL26'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
SENTENCE DC     CL60'THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG.'
         DC     CL60'THE FIVE BOXING WIZARDS DUMP QUICKLY.'
         DC     CL60'HEAVY BOXES PERFORM WALTZES AND JIGS.'
         DC     CL60'PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS.'
BUFFER   DC     CL80' '
         YREGS
         END    PANGRAM
```

{{out}}

```txt
OK THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG.
?J THE FIVE BOXING WIZARDS DUMP QUICKLY.
?C HEAVY BOXES PERFORM WALTZES AND JIGS.
OK PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS.
```



## ACL2


```Lisp
(defun contains-each (needles haystack)
   (if (endp needles)
       t
       (and (member (first needles) haystack)
            (contains-each (rest needles) haystack))))

(defun pangramp (str)
   (contains-each (coerce "abcdefghijklmnopqrstuvwxyz" 'list)
                  (coerce (string-downcase str) 'list)))
```



## ActionScript

{{works with|ActionScript|2.0}}

```ActionScript
function pangram(k:string):Boolean {
  var lowerK:String = k.toLowerCase();
  var has:Object = {}

  for (var i:Number=0; i<=k.length-1; i++) {
    has[lowerK.charAt(i)] = true;
  }

  var result:Boolean = true;

  for (var ch:String='a'; ch <= 'z'; ch=String.fromCharCode(ch.charCodeAt(0)+1)) {
      result = result && has[ch]
  }

  return result || false;
}
```



## Ada


###  Using character sets


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;
procedure pangram is

   function ispangram(txt: String) return Boolean is
     (Is_Subset(To_Set(Span => ('a','z')), To_Set(To_Lower(txt))));

begin
   put_line(Boolean'Image(ispangram("This is a test")));
   put_line(Boolean'Image(ispangram("The quick brown fox jumps over the lazy dog")));
   put_line(Boolean'Image(ispangram("NOPQRSTUVWXYZ  abcdefghijklm")));
   put_line(Boolean'Image(ispangram("abcdefghijklopqrstuvwxyz"))); --Missing m, n
end pangram;

```


###  Using quantified expressions


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
procedure pangram is

  function ispangram(txt : in String) return Boolean is
     (for all Letter in Character range 'a'..'z' =>
         (for some Char of txt => To_Lower(Char) = Letter));

begin
   put_line(Boolean'Image(ispangram("This is a test")));
   put_line(Boolean'Image(ispangram("The quick brown fox jumps over the lazy dog")));
   put_line(Boolean'Image(ispangram("NOPQRSTUVWXYZ  abcdefghijklm")));
   put_line(Boolean'Image(ispangram("abcdefghijklopqrstuvwxyz"))); --Missing m, n
end pangram;

```

{{out}}

```txt

FALSE
TRUE
TRUE
FALSE

```



## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards)}}

```algol68
# init pangram: #
INT la = ABS "a", lz = ABS "z";
INT ua = ABS "A", uz = ABS "Z";
IF lz-la+1 > bits width THEN
  put(stand error, "Exception: insufficient bits in word for task");
  stop
FI;

PROC is a pangram = (STRING test)BOOL: (
  BITS a2z := BIN(ABS(2r1 SHL (lz-la))-1); # assume: ASCII & Binary #
  FOR i TO UPB test WHILE
    INT c = ABS test[i];
    IF la <= c AND c <= lz THEN
      a2z := a2z AND NOT(2r1 SHL (c-la))
    ELIF ua <= c AND c <= uz THEN
      a2z := a2z AND NOT(2r1 SHL (c-ua))
    FI;
# WHILE # a2z /= 2r0 DO
    SKIP
  OD;
  a2z = 2r0
);

main:(
  []STRING test list = (
    "Big fjiords vex quick waltz nymph",
    "The quick brown fox jumps over a lazy dog",
    "A quick brown fox jumps over a lazy dog"
  );
  FOR key TO UPB test list DO
    STRING test = test list[key];
    IF is a pangram(test) THEN
      print(("""",test,""" is a pangram!", new line))
    FI
  OD
)
```

{{out}}

```txt

"Big fjiords vex quick waltz nymph" is a pangram!
"The quick brown fox jumps over a lazy dog" is a pangram!

```



## APL


```apl

    a←'abcdefghijklmnopqrstuvwxyz'
    A←'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    Panagram←{∧/ ∨⌿ 2 26⍴(a,A) ∊ ⍵}
    Panagram 'This should fail'
0
    Panagram 'The quick brown fox jumps over the lazy dog'
1

```




## AppleScript


Out of the box, AppleScript lacks many library basics – no regex, no higher order functions, not even string functions for mapping to upper or lower case.

From OSX 10.10 onwards, we can, however, use ObjC functions from AppleScript by importing the Foundation framework. We do this below to get a toLowerCase() function. If we also add generic filter and map functions, we can write and test a simple isPangram() function as follows:


```AppleScript
use framework "Foundation" -- ( for case conversion function )

-- PANGRAM CHECK -------------------------------------------------------------

-- isPangram :: String -> Bool
on isPangram(s)
    script charUnUsed
        property lowerCaseString : my toLower(s)
        on |λ|(c)
            lowerCaseString does not contain c
        end |λ|
    end script

    length of filter(charUnUsed, "abcdefghijklmnopqrstuvwxyz") = 0
end isPangram


-- TEST ----------------------------------------------------------------------
on run
    map(isPangram, {¬
        "is this a pangram", ¬
        "The quick brown fox jumps over the lazy dog"})

    --> {false, true}
end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- toLower :: String -> String
on toLower(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        lowercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toLower
```

{{Out}}

```AppleScript
{false, true}
```



## ATS


```ATS

(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)
//
fun
letter_check
(
cs: string, c0: char
) : bool = cs.exists()(lam(c) => c0 = c)
//
(* ****** ****** *)

fun
Pangram_check
  (text: string): bool = let
//
val
alphabet = "abcdefghijklmnopqrstuvwxyz"
val
((*void*)) = assertloc(length(alphabet) = 26)
//
in
  alphabet.forall()(lam(c) => letter_check(text, c) || letter_check(text, toupper(c)))
end // end of [Pangram_check]

(* ****** ****** *)

implement
main0 () =
{
//
val
text0 = "The quick brown fox jumps over the lazy dog."
//
val-true = Pangram_check(text0)
val-false = Pangram_check("This is not a pangram sentence.")
//
} (* end of [main0] *)

(* ****** ****** *)

```


An alternate implementation that makes a single pass through the string:


```ATS
fn is_pangram{n:nat}(s: string(n)): bool = loop(s, i2sz(0)) where {
  val letters: arrayref(bool, 26) = arrayref_make_elt<bool>(i2sz(26), false)
  fn check(): bool = loop(0) where {
    fun loop{i:int | i >= 0 && i <= 26}(i: int(i)) =
      if i < 26 then
        if letters[i] then loop(i+1) else
        false
      else true
  }
  fun add{c:int}(c: char(c)): void =
    if (c >= 'A') * (c <= 'Z') then letters[char2int1(c) - char2int1('A')] := true else
    if (c >= 'a') * (c <= 'z') then letters[char2int1(c) - char2int1('a')] := true
  fun loop{i:nat | i <= n}.<n-i>.(s: string(n), i: size_t(i)): bool =
    if string_is_atend(s, i) then check() else
    begin
      add(s[i]);
      loop(s, succ(i))
    end
}

```



## AutoHotkey


```autohotkey
Gui, -MinimizeBox
Gui, Add, Edit, w300 r5 vText
Gui, Add, Button, x105 w100 Default, Check Pangram
Gui, Show,, Pangram Checker
Return

GuiClose:
    ExitApp
Return

ButtonCheckPangram:
    Gui, Submit, NoHide
    Loop, 26
        If Not InStr(Text, Char := Chr(64 + A_Index)) {
            MsgBox,, Pangram, Character %Char% is missing!
            Return
        }
    MsgBox,, Pangram, OK`, this is a Pangram!
Return
```



## AutoIt


```autoit

Pangram("The quick brown fox jumps over the lazy dog")
Func Pangram($s_String)
For $i = 1 To 26
	IF Not StringInStr($s_String, Chr(64 + $i)) Then
		Return MsgBox(0,"No Pangram", "Character " & Chr(64 + $i) &" is missing")
	EndIf
Next
Return MsgBox(0,"Pangram", "Sentence is a Pangram")
EndFunc

```



## AWK


===Solution using string-operations===

```AWK
#!/usr/bin/awk -f
BEGIN {
   allChars="ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   print isPangram("The quick brown fox jumps over the lazy dog.");
   print isPangram("The quick brown fo.");
}

function isPangram(string) {
    delete X;
    for (k=1; k<length(string); k++) {
        X[toupper(substr(string,k,1))]++;  # histogram
    }
    for (k=1; k<=length(allChars); k++) {
        if (!X[substr(allChars,k,1)]) return 0;
    }
    return 1;
}
```


{{out}}

```txt
1
0
```



### Solution using associative arrays and split

{{Works with|gawk|4.1.0}}
{{Works with|mawk|1.3.3}}

```AWK
# usage: awk -f pangram.awk -v p="The five boxing wizards dump quickly." input.txt
#
# Pangram-checker, using associative arrays and split
BEGIN {
  alfa="ABCDEFGHIJKLMNOPQRSTUVWXYZ"; ac=split(alfa,A,"")
  print "# Checking for all",ac,"chars in '" alfa "' :"

  print testPangram("The quick brown fox jumps over the lazy dog.");
  print testPangram(p);
}

{ print testPangram($0) }

function testPangram(str,   c,i,S,H,hit,miss) {
    print str  						##
    split( toupper(str), S, "")
    for (c in S) {
      H[ S[c] ]++
     #print c, S[c], H[ S[c] ]				##
    }
    for (i=1; i<=ac; i++) {
      c = A[i]
     #printf("%2d %c : %4d\n", i, c, H[c] )  		##
      if (H[c]) { hit=hit c } else { miss=miss c }
    }
    print "# hit:",hit, "# miss:",miss, "."		##
    if (miss) return 0
    return 1
}
```


{{out}}

```txt

# Checking for all 26 chars in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' :
The quick brown fox jumps over the lazy dog.
# hit: ABCDEFGHIJKLMNOPQRSTUVWXYZ # miss:  .
1
The five boxing wizards dump quickly.
# hit: ABCDEFGHIKLMNOPQRSTUVWXYZ # miss: J .
0
Heavy boxes perform waltzes and jigs
# hit: ABDEFGHIJLMNOPRSTVWXYZ # miss: CKQU .
0
The quick onyx goblin jumps over the lazy dwarf.
# hit: ABCDEFGHIJKLMNOPQRSTUVWXYZ # miss:  .
1
Pack my box with five dozen liquor jugs
# hit: ABCDEFGHIJKLMNOPQRSTUVWXYZ # miss:  .
1

```



## BASIC

{{works with|QBasic}}


```qbasic
DECLARE FUNCTION IsPangram! (sentence AS STRING)

DIM x AS STRING

x = "My dog has fleas."
GOSUB doIt
x = "The lazy dog jumps over the quick brown fox."
GOSUB doIt
x = "Jackdaws love my big sphinx of quartz."
GOSUB doIt
x = "What's a jackdaw?"
GOSUB doIt

END

doIt:
    PRINT IsPangram!(x), x
    RETURN

FUNCTION IsPangram! (sentence AS STRING)
    'returns -1 (true) if sentence is a pangram, 0 (false) otherwise
    DIM l AS INTEGER, s AS STRING, t AS INTEGER
    DIM letters(25) AS INTEGER

    FOR l = 1 TO LEN(sentence)
        s = UCASE$(MID$(sentence, l, 1))
        SELECT CASE s
            CASE "A" TO "Z"
                t = ASC(s) - 65
                letters(t) = 1
        END SELECT
    NEXT

    FOR l = 0 TO 25
        IF letters(l) < 1 THEN
            IsPangram! = 0
            EXIT FUNCTION
        END IF
    NEXT

    IsPangram! = -1
END FUNCTION
```

{{out}}

```txt

  0            My dog has fleas.
 -1            The quick brown fox jumps over the lazy dog.
 -1            Jackdaws love my big sphinx of quartz.
  0            What's a jackdaw?

```


=
## Sinclair ZX81 BASIC
=
Works (just) with the 1k RAM model. The "37" that crops up a couple of times stops being a mystery if we remember that the ZX81 character code for <code>A</code> is 38 and that strings (like arrays) are indexed from 1, not from 0.

```basic
 10 LET A$="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 20 LET L=26
 30 INPUT P$
 40 IF LEN P$<26 THEN GOTO 170
 50 FAST
 60 LET C=1
 70 IF P$(C)<"A" OR P$(C)>"Z" THEN GOTO 120
 80 IF A$(CODE P$(C)-37)=" " THEN GOTO 120
 90 LET A$(CODE P$(C)-37)=" "
100 LET L=L-1
110 IF L=0 THEN GOTO 150
120 IF C=LEN P$ THEN GOTO 170
130 LET C=C+1
140 GOTO 70
150 PRINT "PANGRAM"
160 GOTO 180
170 PRINT "NOT A PANGRAM"
180 SLOW
```

{{in}}

```txt
THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG.
```

{{out}}

```txt
PANGRAM
```

{{in}}

```txt
AND DARK THE SUN AND MOON, AND THE ALMANACH DE GOTHA
```

{{out}}

```txt
NOT A PANGRAM
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

	%
### The Main Thing
%
call :pangram "The quick brown fox jumps over the lazy dog."
call :pangram "The quick brown fox jumped over the lazy dog."
echo.
pause
exit /b 0

	%
### The Function
%
:pangram
set letters=abcdefgihjklmnopqrstuvwxyz
set cnt=0
set inp=%~1
set str=!inp: =!

:loop
set chr=!str:~%cnt%,1!
if "!letters!"=="" (
	echo %1 is a pangram^^!
	goto :EOF
)
if "!chr!"=="" (
	echo %1 is not a pangram.
	goto :EOF
)
set letters=!letters:%chr%=!
set /a cnt+=1
goto loop
```

{{Out}}

```txt
"The quick brown fox jumps over the lazy dog." is a pangram!
"The quick brown fox jumped over the lazy dog." is not a pangram.

Press any key to continue . . .
```



## BBC BASIC


```bbcbasic
      FOR test% = 1 TO 2
        READ test$
        PRINT """" test$ """ " ;
        IF FNpangram(test$) THEN
          PRINT "is a pangram"
        ELSE
          PRINT "is not a pangram"
        ENDIF
      NEXT test%
      END

      DATA "The quick brown fox jumped over the lazy dog"
      DATA "The five boxing wizards jump quickly"

      DEF FNpangram(A$)
      LOCAL C%
      A$ = FNlower(A$)
      FOR C% = ASC("a") TO ASC("z")
        IF INSTR(A$, CHR$(C%)) = 0 THEN = FALSE
      NEXT
      = TRUE

      DEF FNlower(A$)
      LOCAL A%, C%
      FOR A% = 1 TO LEN(A$)
        C% = ASCMID$(A$,A%)
        IF C% >= 65 IF C% <= 90 MID$(A$,A%,1) = CHR$(C%+32)
      NEXT
      = A$
```

{{out}}

```txt
"The quick brown fox jumped over the lazy dog" is not a pangram
"The five boxing wizards jump quickly" is a pangram
```



## Befunge


Reads the sentence to test from stdin.


```befunge>
~>:65*`!#v_:"`"`48*v>g+04p1\4p
^#*`\*93\`0<::-"@"-*<^40!%2g4:_
"pangram."<v*84<_v#-":"g40\" a"
>>:#,_55+,@>"ton">48*>"si tahT"
```


{{in}}

```txt
The quick brown fox jumps over the lazy dog.
```


{{out}}

```txt
That is a pangram.
```



## Bracmat


```bracmat
(isPangram=
  k
.   low$!arg:?arg
  & a:?k
  &   whl
    ' ( @(!arg:? !k ?)
      & chr$(1+asc$!k):?k:~>z
      )
  & !k:>z
  &
);
```

Some examples:

```txt
isPangram$("the Quick brown FOX jumps over the lazy do")
no
isPangram$("the Quick brown FOX jumps over the lazy dog")
yes
isPangram$"My dog has fleas."
no
isPangram$"The quick brown fox jumps over the lazy dog."
yes
isPangram$"Jackdaws love my big sphinx of quartz."
yes
isPangram$"What's a jackdaw?"
no
isPangram$"Lynx c.q. vos prikt bh: dag zwemjuf!"
yes
```



## Brat


```brat
pangram? = { sentence |
  letters = [:a :b :c :d :e :f :g :h :i :j :k :l :m
    :n :o :p :q :r :s :t :u :v :w :x :y :z]

    sentence.downcase!

    letters.reject! { l |
      sentence.include? l
    }

  letters.empty?
}

p pangram? 'The quick brown fox jumps over the lazy dog.' #Prints true
p pangram? 'Probably not a pangram.'  #Prints false
```


Alternative version:


```brat
pangram? = { sentence |
  sentence.downcase.dice.unique.select(:alpha?).length == 26
}
```



## C


```c
#include <stdio.h>

int is_pangram(const char *s)
{
	const char *alpha = ""
		"abcdefghjiklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	char ch, wasused[26] = {0};
	int total = 0;

	while ((ch = *s++) != '\0') {
		const char *p;
		int idx;

		if ((p = strchr(alpha, ch)) == NULL)
			continue;

		idx = (p - alpha) % 26;

		total += !wasused[idx];
		wasused[idx] = 1;
		if (total == 26)
			return 1;
	}
	return 0;
}

int main(void)
{
	int i;
	const char *tests[] = {
		"The quick brown fox jumps over the lazy dog.",
		"The qu1ck brown fox jumps over the lazy d0g."
	};

	for (i = 0; i < 2; i++)
		printf("\"%s\" is %sa pangram\n",
			tests[i], is_pangram(tests[i])?"":"not ");
	return 0;
}
```


### Using bitmask

Assumes an execution environment using the ASCII character set (will invoke undefined behavior on other systems).


```c
#include <stdio.h>

int pangram(const char *s)
{
	int c, mask = (1 << 26) - 1;
	while ((c = (*s++)) != '\0') /* 0x20 converts lowercase to upper */
		if ((c &= ~0x20) <= 'Z' && c >= 'A')
			mask &= ~(1 << (c - 'A'));

	return !mask;
}

int main()
{
	int i;
	const char *s[] = {	"The quick brown fox jumps over lazy dogs.",
				"The five boxing wizards dump quickly.",  };

	for (i = 0; i < 2; i++)
		printf("%s: %s\n", pangram(s[i]) ? "yes" : "no ", s[i]);

	return 0;
}
```

{{out}}

```txt
yes: The quick brown fox jumps over lazy dogs.
no : The five boxing wizards dump quickly.
```


=={{header|C sharp|C#}}==
C# 3.0 or higher (.NET Framework 3.5 or higher)


```csharp
using System;
using System.Linq;

static class Program
{
    static bool IsPangram(this string text, string alphabet = "abcdefghijklmnopqrstuvwxyz")
    {
        return alphabet.All(text.ToLower().Contains);
    }

    static void Main(string[] arguments)
    {
        Console.WriteLine(arguments.Any() && arguments.First().IsPangram());
    }
}
```


Any version of C# language and .NET Framework


```csharp
using System;

namespace PangrammChecker
{
    public class PangrammChecker
    {
        public static bool IsPangram(string str)
        {
            bool[] isUsed = new bool[26];
            int ai = (int)'a';
            int total = 0;
            for (CharEnumerator en = str.ToLower().GetEnumerator(); en.MoveNext(); )
            {
                int d = (int)en.Current - ai;
                if (d >= 0 && d < 26)
                    if (!isUsed[d])
                    {
                        isUsed[d] = true;
                        total++;
                    }
            }
            return (total == 26);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            string str1 = "The quick brown fox jumps over the lazy dog.";
            string str2 = "The qu1ck brown fox jumps over the lazy d0g.";
            Console.WriteLine("{0} is {1}a pangram", str1,
                PangrammChecker.IsPangram(str1)?"":"not ");
            Console.WriteLine("{0} is {1}a pangram", str2,
                PangrammChecker.IsPangram(str2)?"":"not ");
            Console.WriteLine("Press Return to exit");
            Console.ReadLine();
        }
    }
}
```



## C++


```cpp
#include <algorithm>
#include <cctype>
#include <string>
#include <iostream>

const std::string alphabet("abcdefghijklmnopqrstuvwxyz");

bool is_pangram(std::string s)
{
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
    std::sort(s.begin(), s.end());
    return std::includes(s.begin(), s.end(), alphabet.begin(), alphabet.end());
}

int main()
{
    const auto examples = {"The quick brown fox jumps over the lazy dog",
                           "The quick white cat jumps over the lazy dog"};

    std::cout.setf(std::ios::boolalpha);
    for (auto& text : examples) {
        std::cout << "Is \"" << text << "\" a pangram? - " << is_pangram(text) << std::endl;
    }
}

```



## Ceylon


```ceylon
shared void run() {

	function pangram(String sentence) =>
 		let(alphabet = set('a'..'z'),
			letters = set(sentence.lowercased.filter(alphabet.contains)))
 		letters == alphabet;

 	value sentences = [
 		"The quick brown fox jumps over the lazy dog",
 		"""Watch "Jeopardy!", Alex Trebek's fun TV quiz game.""",
 		"Pack my box with five dozen liquor jugs.",
 		"blah blah blah"
 	];
 	for(sentence in sentences) {
 		print("\"``sentence``\" is a pangram? ``pangram(sentence)``");
 	}
}
```



## Clojure


```lisp
(defn pangram? [s]
  (let [letters (into #{} "abcdefghijklmnopqrstuvwxyz")]
    (= (->> s .toLowerCase (filter letters) (into #{})) letters)))
```



## COBOL


```COBOL
       identification division.
       program-id. pan-test.
       data division.
       working-storage section.
       1 text-string pic x(80).
       1 len binary pic 9(4).
       1 trailing-spaces binary pic 9(4).
       1 pangram-flag pic x value "n".
        88 is-not-pangram value "n".
        88 is-pangram value "y".
       procedure division.
       begin.
           display "Enter text string:"
           accept text-string
           set is-not-pangram to true
           initialize trailing-spaces len
           inspect function reverse (text-string)
           tallying trailing-spaces for leading space
               len for characters after space
           call "pangram" using pangram-flag len text-string
           cancel "pangram"
           if is-pangram
               display "is a pangram"
           else
               display "is not a pangram"
           end-if
           stop run
           .
       end program pan-test.

       identification division.
       program-id. pangram.
       data division.
       1 lc-alphabet pic x(26) value "abcdefghijklmnopqrstuvwxyz".
       linkage section.
       1 pangram-flag pic x.
        88 is-not-pangram value "n".
        88 is-pangram value "y".
       1 len binary pic 9(4).
       1 text-string pic x(80).
       procedure division using pangram-flag len text-string.
       begin.
           inspect lc-alphabet converting
               function lower-case (text-string (1:len))
               to space
           if lc-alphabet = space
               set is-pangram to true
           end-if
           exit program
           .
       end program pangram.
```



## CoffeeScript


```coffeescript

is_pangram = (s) ->
  # This is optimized for longish strings--as soon as all 26 letters
  # are encountered, we will be done.  Our worst case scenario is a really
  # long non-pangram, or a really long pangram with at least one letter
  # only appearing toward the end of the string.
  a_code = 'a'.charCodeAt(0)
  required_letters = {}
  for i in [a_code...a_code+26]
    required_letters[String.fromCharCode(i)] = true

  cnt = 0
  for c in s
    c = c.toLowerCase()
    if required_letters[c]
      cnt += 1
      return true if cnt == 26
      delete required_letters[c]
  false

do ->
  tests = [
    ["is this a pangram", false]
    ["The quick brown fox jumps over the lazy dog", true]
  ]

  for test in tests
    [s, exp_value] = test
    throw Error("fail") if is_pangram(s) != exp_value
    # try long strings
    long_str = ''
    for i in [1..500000]
      long_str += s
    throw Error("fail") if is_pangram(long_str) != exp_value
    console.log "Passed tests: #{s}"

```




## Common Lisp


```lisp
(defun pangramp (s)
  (null (set-difference
          (loop for c from (char-code #\A) upto (char-code #\Z) collect (code-char c))
          (coerce (string-upcase s) 'list))))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE BbtPangramChecker;
IMPORT StdLog,DevCommanders,TextMappers;

PROCEDURE Check(str: ARRAY OF CHAR): BOOLEAN;
CONST
	letters = 26;
VAR
	i,j: INTEGER;
	status: ARRAY letters OF BOOLEAN;
	resp : BOOLEAN;
BEGIN
	FOR i := 0 TO LEN(status) -1 DO status[i] := FALSE END;

	FOR i := 0 TO LEN(str) -  1 DO
		j := ORD(CAP(str[i])) - ORD('A');
		IF (0 <= j) & (25 >= j) & ~status[j] THEN status[j] := TRUE END
	END;

	resp := TRUE;
	FOR i := 0 TO LEN(status) - 1 DO;
		resp := resp & status[i]
	END;
	RETURN resp;
END Check;

PROCEDURE Do*;
VAR
	params: DevCommanders.Par;
	s: TextMappers.Scanner;
BEGIN
	params := DevCommanders.par;
	s.ConnectTo(params.text);
	s.SetPos(params.beg);
	s.Scan;
	WHILE (~s.rider.eot) DO
		IF (s.type = TextMappers.char) & (s.char = '~') THEN
			RETURN
		ELSIF (s.type # TextMappers.string) THEN
			StdLog.String("Invalid parameter");StdLog.Ln
		ELSE
			StdLog.Char("'");StdLog.String(s.string + "' is pangram?:> ");
			StdLog.Bool(Check(s.string));StdLog.Ln
		END;
		s.Scan
	END
END Do;

END BbtPangramChecker.

```

Execute: ^Q BbtPangramChecker.Do "The quick brown fox jumps over the lazy dog"~ <br/>
^Q BbtPangramChecker.Do "abcdefghijklmnopqrstuvwxyz"~<br/>
^Q BbtPangramChecker.Do "A simple text"~<br/>
{{out}}

```txt

'The quick brown fox jumps over the lazy dog' is pangram?:>  $TRUE
'abcdefghijklmnopqrstuvwxyz' is pangram?:>  $TRUE
'A simple text' is pangram?:>  $FALSE

```



## D


### ASCII Bitmask version


```d
bool isPangram(in string text) pure nothrow @safe @nogc {
    uint bitset;

    foreach (immutable c; text) {
        if (c >= 'a' && c <= 'z')
            bitset |= (1u << (c - 'a'));
        else if (c >= 'A' && c <= 'Z')
            bitset |= (1u << (c - 'A'));
    }

    return bitset == 0b11_11111111_11111111_11111111;
}

void main() {
    assert("the quick brown fox jumps over the lazy dog".isPangram);
    assert(!"ABCDEFGHIJKLMNOPQSTUVWXYZ".isPangram);
    assert(!"ABCDEFGHIJKL.NOPQRSTUVWXYZ".isPangram);
    assert("ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ".isPangram);
}
```



### Unicode version


```d
import std.string, std.traits, std.uni;

// Do not compile with -g (debug info).
enum Alphabet : dstring {
    DE = "abcdefghijklmnopqrstuvwxyzßäöü",
    EN = "abcdefghijklmnopqrstuvwxyz",
    SV = "abcdefghijklmnopqrstuvwxyzåäö"
}

bool isPangram(S)(in S s, dstring alpha = Alphabet.EN)
pure /*nothrow*/ if (isSomeString!S) {
    foreach (dchar c; alpha)
       if (indexOf(s, c) == -1 && indexOf(s, std.uni.toUpper(c)) == -1)
            return false;
    return true;
}

void main() {
    assert(isPangram("the quick brown fox jumps over the lazy dog".dup, Alphabet.EN));
    assert(isPangram("Falsches Üben von Xylophonmusik quält jeden größeren Zwerg"d, Alphabet.DE));
    assert(isPangram("Yxskaftbud, ge vår wczonmö iqhjälp"w, Alphabet.SV));
}
```



## Delphi


```Delphi
program PangramChecker;

{$APPTYPE CONSOLE}

uses StrUtils;

function IsPangram(const aString: string): Boolean;
var
  c: char;
begin
  for c := 'a' to 'z' do
    if not ContainsText(aString, c) then
      Exit(False);

  Result := True;
end;

begin
  Writeln(IsPangram('The quick brown fox jumps over the lazy dog')); // true
  Writeln(IsPangram('Not a panagram')); // false
end.
```



## E



```e
def isPangram(sentence :String) {
    return ("abcdefghijklmnopqrstuvwxyz".asSet() &! sentence.toLowerCase().asSet()).size() == 0
}
```


<code>&amp;!</code> is the “but-not” or set difference operator.


## Elixir


```elixir
defmodule Pangram do
  def checker(str) do
    unused = Enum.to_list(?a..?z) -- to_char_list(String.downcase(str))
    Enum.empty?(unused)
  end
end

text = "The quick brown fox jumps over the lazy dog."
IO.puts "#{Pangram.checker(text)}\t#{text}"
text = (Enum.to_list(?A..?Z) -- 'Test') |> to_string
IO.puts "#{Pangram.checker(text)}\t#{text}"
```


{{out}}

```txt

true    The quick brown fox jumps over the lazy dog.
false   ABCDEFGHIJKLMNOPQRSUVWXYZ

```



## Erlang


```Erlang
-module(pangram).
-export([is_pangram/1]).

is_pangram(String) ->
  ordsets:is_subset(lists:seq($a, $z), ordsets:from_list(string:to_lower(String))).
```


=={{header|F Sharp|F#}}==
If the difference between the set of letters in the alphabet and the set of letters in the given string (after conversion to lower case) is the empty set then every letter appears somewhere in the given string:

```fsharp
let isPangram (str: string) = (set['a'..'z'] - set(str.ToLower())).IsEmpty
```



## Factor

{{trans|E}}

```factor
: pangram? ( str -- ? )
    [ "abcdefghijklmnopqrstuvwxyz" ] dip >lower diff length 0 = ;

"How razorback-jumping frogs can level six piqued gymnasts!" pangram? .
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Pangram_checker this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: pangram? ( addr len -- ? )
  0 -rot bounds do
    i c@ 32 or [char] a -
    dup 0 26 within if
      1 swap lshift or
    else drop then
  loop
  1 26 lshift 1- = ;

s" The five boxing wizards jump quickly." pangram? .   \ -1
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
module pangram

  implicit none
  private
  public :: is_pangram
  character (*), parameter :: lower_case = 'abcdefghijklmnopqrstuvwxyz'
  character (*), parameter :: upper_case = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

contains

  function to_lower_case (input) result (output)

    implicit none
    character (*), intent (in) :: input
    character (len (input)) :: output
    integer :: i
    integer :: j

    output = input
    do i = 1, len (output)
      j = index (upper_case, output (i : i))
      if (j /= 0) then
        output (i : i) = lower_case (j : j)
      end if
    end do

  end function to_lower_case

  function is_pangram (input) result (output)

    implicit none
    character (*), intent (in) :: input
    character (len (input)) :: lower_case_input
    logical :: output
    integer :: i

    lower_case_input = to_lower_case (input)
    output = .true.
    do i = 1, len (lower_case)
      if (index (lower_case_input, lower_case (i : i)) == 0) then
        output = .false.
        exit
      end if
    end do

  end function is_pangram

end module pangram
```

Example:

```fortran
program test

  use pangram, only: is_pangram

  implicit none
  character (256) :: string

  string = 'This is a sentence.'
  write (*, '(a)') trim (string)
  write (*, '(l1)') is_pangram (string)
  string = 'The five boxing wizards jumped quickly.'
  write (*, '(a)') trim (string)
  write (*, '(l1)') is_pangram (string)

end program test
```

{{out}}

```txt
This is a sentence.
F
The five boxing wizards jumped quickly.
T
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isPangram(s As Const String) As Boolean
  Dim As Integer length = Len(s)
  If length < 26 Then Return False
  Dim p As String = LCase(s)
  For i As Integer = 97 To 122
    If Instr(p, Chr(i)) = 0 Then Return False
  Next
  Return True
End Function

Dim s(1 To 3) As String = _
{ _
 "The quick brown fox jumps over the lazy dog", _
 "abbdefghijklmnopqrstuVwxYz", _ '' no c!
 "How vexingly quick daft zebras jump!" _
}

For i As Integer = 1 To 3:
  Print "'"; s(i); "' is "; IIf(isPangram(s(i)), "a", "not a"); " pangram"
  Print
Next

Print
Print "Press nay key to quit"
Sleep
```


{{out}}

```txt

'The quick brown fox jumps over the lazy dog' is a pangram

'abbdefghijklmnopqrstuVwxYz' is not a pangram

'How vexingly quick daft zebras jump!' is a pangram

```



## Go


```go
package main

import "fmt"

func main() {
    for _, s := range []string{
        "The quick brown fox jumps over the lazy dog.",
        `Watch "Jeopardy!", Alex Trebek's fun TV quiz game.`,
        "Not a pangram.",
    } {
        if pangram(s) {
            fmt.Println("Yes:", s)
        } else {
            fmt.Println("No: ", s)
        }
    }
}

func pangram(s string) bool {
	var missing uint32 = (1 << 26) - 1
	for _, c := range s {
		var index uint32
		if 'a' <= c && c <= 'z' {
			index = uint32(c - 'a')
		} else if 'A' <= c && c <= 'Z' {
			index = uint32(c - 'A')
		} else {
			continue
		}

		missing &^= 1 << index
		if missing == 0 {
			return true
		}
	}
	return false
}
```

{{out}}

```txt

Yes: The quick brown fox jumps over the lazy dog.
Yes: Watch "Jeopardy!", Alex Trebek's fun TV quiz game.
No:  Not a pangram.

```



## Haskell



```haskell
import Data.Char (toLower)
import Data.List ((\\))

pangram :: String -> Bool
pangram = null . (['a' .. 'z'] \\) . map toLower

main = print $ pangram "How razorback-jumping frogs can level six piqued gymnasts!"
```



## HicEst


```HicEst
PangramBrokenAt("This is a Pangram.") ! => 2 (b is missing)
PangramBrokenAt("The quick Brown Fox jumps over the Lazy Dog") ! => 0 (OK)

FUNCTION PangramBrokenAt(string)
   CHARACTER string, Alfabet="abcdefghijklmnopqrstuvwxyz"
   PangramBrokenAt = INDEX(Alfabet, string, 64)
   ! option 64: verify = 1st letter of string not in Alfabet
END
```


=={{header|Icon}} and {{header|Unicon}}==
A panagram procedure:

```Icon
procedure panagram(s)     #: return s if s is a panagram and fail otherwise
if (map(s) ** &lcase) === &lcase then return s
end
```


And a main  to drive it:

```Icon
procedure main(arglist)

if *arglist > 0 then
   every ( s := "" ) ||:= !arglist || " "
else
   s := "The quick brown fox jumps over the lazy dog."

writes(image(s), " -- is")
writes(if not panagram(s) then "n't")
write(" a panagram.")
end
```



## Io


```Io
Sequence isPangram := method(
    letters := " " repeated(26)
    ia := "a" at(0)
    foreach(ichar,
        if(ichar isLetter,
            letters atPut((ichar asLowercase) - ia, ichar)
        )
    )
    letters contains(" " at(0)) not     // true only if no " " in letters
)

"The quick brown fox jumps over the lazy dog." isPangram println    // --> true
"The quick brown fox jumped over the lazy dog." isPangram println   // --> false
"ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ" isPangram println           // --> true
```



## Ioke


```ioke
Text isPangram? = method(
  letters = "abcdefghijklmnopqrstuvwxyz" chars
  text = self lower chars
  letters map(x, text include?(x)) reduce(&&)
)
```


Here is an example of it's use in the Ioke REPL:


```ioke

iik> "The quick brown fox jumps over the lazy dog" isPangram?
"The quick brown fox jumps over the lazy dog" isPangram?
+> true

iik> "The quick brown fox jumps over the" isPangram?
"The quick brown fox jumps over the" isPangram?
+> false
```



## J

'''Solution:'''

```j
require 'strings'
isPangram=: (a. {~ 97+i.26) */@e. tolower
```


'''Example use:'''

```j
   isPangram 'The quick brown fox jumps over the lazy dog.'
1
   isPangram 'The quick brown fox falls over the lazy dog.'
0
```


## Java

{{works with|Java|1.5+}}

```java5
public class Pangram {
    public static boolean isPangram(String test){
        for (char a = 'A'; a <= 'Z'; a++)
            if ((test.indexOf(a) < 0) && (test.indexOf((char)(a + 32)) < 0))
                return false;
        return true;
    }

    public static void main(String[] args){
        System.out.println(isPangram("the quick brown fox jumps over the lazy dog"));//true
        System.out.println(isPangram("the quick brown fox jumped over the lazy dog"));//false, no s
        System.out.println(isPangram("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));//true
        System.out.println(isPangram("ABCDEFGHIJKLMNOPQSTUVWXYZ"));//false, no r
        System.out.println(isPangram("ABCDEFGHIJKL.NOPQRSTUVWXYZ"));//false, no m
        System.out.println(isPangram("ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ"));//true
        System.out.println(isPangram(""));//false
    }
}
```

{{out}}

```txt
true
false
true
false
false
true
false
```



## JavaScript


### ES5


### =Iterative=



```javascript
function isPangram(s) {
    var letters = "zqxjkvbpygfwmucldrhsnioate"
    // sorted by frequency ascending (http://en.wikipedia.org/wiki/Letter_frequency)
    s = s.toLowerCase().replace(/[^a-z]/g,'')
    for (var i = 0; i < 26; i++)
        if (s.indexOf(letters[i]) < 0) return false
    return true
}

console.log(isPangram("is this a pangram"))  // false
console.log(isPangram("The quick brown fox jumps over the lazy dog"))  // true
```



### ES6


### =Functional=



```JavaScript
(() => {
    'use strict';

    // isPangram :: String -> Bool
    let isPangram = s => {
        let lc = s.toLowerCase();

        return 'abcdefghijklmnopqrstuvwxyz'
            .split('')
            .filter(c => lc.indexOf(c) === -1)
            .length === 0;
    };

    // TEST
    return [
        'is this a pangram',
        'The quick brown fox jumps over the lazy dog'
    ].map(isPangram);

})();
```


{{Out}}

```txt
[false, true]
```



## jq


```jq
def is_pangram:
  explode
  | map( if 65 <= . and . <= 90 then . + 32 # uppercase
         elif 97 <= . and . <= 122 then .   # lowercase
         else empty
         end )
  | unique
  | length == 26;

# Example:
"The quick brown fox jumps over the lazy dog" | is_pangram
```

{{Out}}
 $ jq -M -n -f pangram.jq
 true


## Julia

<tt>makepangramchecker</tt> creates a function to test for pangramity based upon the contents of its input string, allowing one to create arbitrary pangram checkers.

```Julia
function makepangramchecker(alphabet)
    alphabet = Set(uppercase.(alphabet))
    function ispangram(s)
        lengthcheck = length(s) ≥ length(alphabet)
        return lengthcheck && all(c in uppercase(s) for c in alphabet)
    end
    return ispangram
end

const tests = ["Pack my box with five dozen liquor jugs.",
                "The quick brown fox jumps over a lazy dog.",
                "The quick brown fox jumps\u2323over the lazy dog.",
                "The five boxing wizards jump quickly.",
                "This sentence contains A-Z but not the whole alphabet."]

is_english_pangram = makepangramchecker('a':'z')

for s in tests
    println("The sentence \"", s, "\" is ", is_english_pangram(s) ? "" : "not ", "a pangram.")
end
```


{{out}}

```txt

The sentence "Pack my box with five dozen liquor jugs." is a pangram.
The sentence "The quick brown fox jumps over a lazy dog." is a pangram.
The sentence "The quick brown fox jumps⌣over the lazy dog." is a pangram.
The sentence "The five boxing wizards jump quickly." is a pangram.
The sentence "This sentence contains A-Z but not the whole alphabet." is not a pangram.

```



## K


```k
lcase   : _ci 97+!26
ucase   : _ci 65+!26
tolower : {@[x;p;:;lcase@n@p:&26>n:ucase?/:x]}
panagram: {&/lcase _lin tolower x}
```


Example:

```k
  panagram "The quick brown fox jumps over the lazy dog"
1
  panagram "Panagram test"
0
```



## Kotlin


```scala
// version 1.0.6

fun isPangram(s: String): Boolean {
    if (s.length < 26) return false
    val t = s.toLowerCase()
    for (c in 'a' .. 'z')
        if (c !in t) return false
    return true
}

fun main(args: Array<String>) {
   val candidates = arrayOf(
       "The quick brown fox jumps over the lazy dog",
       "New job: fix Mr. Gluck's hazy TV, PDQ!",
       "A very bad quack might jinx zippy fowls",
       "A very mad quack might jinx zippy fowls"   // no 'b' now!
   )
   for (candidate in candidates)
       println("'$candidate' is ${if (isPangram(candidate)) "a" else "not a"} pangram")
}
```


{{out}}

```txt

'The quick brown fox jumps over the lazy dog' is a pangram
'New job: fix Mr. Gluck's hazy TV, PDQ!' is a pangram
'A very bad quack might jinx zippy fowls' is a pangram
'A very mad quack might jinx zippy fowls' is not a pangram

```



## Liberty BASIC


```lb
'Returns 0 if the string is NOT a pangram or >0 if it IS a pangram
string$ = "The quick brown fox jumps over the lazy dog."

Print isPangram(string$)

Function isPangram(string$)
    string$ = Lower$(string$)
    For i = Asc("a") To Asc("z")
        isPangram = Instr(string$, chr$(i))
        If isPangram = 0 Then Exit Function
    Next i
End Function
```



## Logo


```logo
to remove.all :s :set
  if empty? :s [output :set]
  if word? :s [output remove.all butfirst :s remove first :s :set]
  output remove.all butfirst :s remove.all first :s :set
end
to pangram? :s
  output empty? remove.all :s "abcdefghijklmnopqrstuvwxyz
end

show pangram? [The five boxing wizards jump quickly.]   ; true
```


## Lua


```lua
require"lpeg"
S, C = lpeg.S, lpeg.C
function ispangram(s)
  return #(C(S(s)^0):match"abcdefghijklmnopqrstuvwxyz") == 26
end

print(ispangram"waltz, bad nymph, for quick jigs vex")
print(ispangram"bobby")
print(ispangram"long sentence")
```



## Maple


```Maple
#Used built-in StringTools package
is_pangram := proc(str)
	local present := StringTools:-LowerCase~(select(StringTools:-HasAlpha, StringTools:-Explode(str)));
	local alphabets := {"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"};
	present := convert(present, set);
	return evalb(present = alphabets);
end proc;

```

{{out|Usage}}
<lang>is_pangram("The quick brown fox jumps over the lazy dog.");
is_pangram("The 2 QUIck brown foxes jumped over the lazy DOG!!");
is_pangram(""The quick brown fox jumps over the lay dog.");
```

{{out|Output}}

```txt

true
true
false

```


## Mathematica


```Mathematica
pangramQ[msg_]:=Complement[CharacterRange["a", "z"], Characters[ToLowerCase[msg]]]=== {}
```

Usage:

```txt
pangramQ["The quick brown fox jumps over the lazy dog."]
True
```


Or a slightly more verbose version that outputs the missing characters if the string is not a pangram:

```Mathematica
pangramQ[msg_] :=
 Function[If[# === {}, Print["The string is a pangram!"],
    Print["The string is not a pangram. It's missing the letters " <>
      ToString[#]]]][
  Complement[CharacterRange["a", "z"], Characters[ToLowerCase[msg]]]]
```

Usage:

```txt
pangramQ["The quick brown fox jumps over the lazy dog."]
The string is a pangram!
```


```txt
pangramQ["Not a pangram"]
The string is not a pangram. It's missing the letters {b, c, d, e, f, h, i, j, k, l, q, s, u, v, w, x, y, z}
```



## MATLAB


```MATLAB
function trueFalse = isPangram(string)

    %This works by histogramming the ascii character codes for lower case
    %letters contained in the string (which is first converted to all
    %lower case letters). Then it finds the index of the first letter that
    %is not contained in the string (this is faster than using the find
    %without the second parameter). If the find returns an empty array then
    %the original string is a pangram, if not then it isn't.

    trueFalse = isempty(find( histc(lower(string),(97:122))==0,1 ));

end
```


{{out}}

```MATLAB
isPangram('The quick brown fox jumps over the lazy dog.')

ans =

     1
```



## MiniScript


```MiniScript
sentences = ["The quick brown fox jumps over the lazy dog.",
    "Peter Piper picked a peck of pickled peppers.",
    "Waltz job vexed quick frog nymphs."]

alphabet = "abcdefghijklmnopqrstuvwxyz"

pangram = function (toCheck)
    sentence = toCheck.lower
    fail = false
    for c in alphabet
        if sentence.indexOf(c) == null then return false
    end for
    return true
end function

for sentence in sentences
    if pangram(sentence) then
        print """" + sentence + """ is a Pangram"
    else
        print """" + sentence + """ is not a Pangram"
    end if
end for
```

{{out}}

```txt

"The quick brown fox jumps over the lazy dog." is a Pangram
"Peter Piper picked a peck of pickled peppers." is not a Pangram
"Waltz job vexed quick frog nymphs." is a Pangram

```



## NetRexx

NetRexx's <code>verify</code> built&ndash;in method is all you need!

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

A2Z = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

pangrams = create_samples

loop p_ = 1 to pangrams[0]
  pangram = pangrams[p_]
  q_ = A2Z.verify(pangram.upper) -- <= it basically all happens in this function call!
  say pangram.left(64)'\-'
  if q_ == 0 then -
    say ' [OK, a pangram]'
  else -
    say ' [Not a pangram.  Missing:' A2Z.substr(q_, 1)']'
  end p_

method create_samples public static returns Rexx

  pangrams = ''

  x_ = 0
  x_ = x_ + 1; pangrams[0] = x_; pangrams[x_] = 'The quick brown fox jumps over a lazy dog.'    -- best/shortest pangram
  x_ = x_ + 1; pangrams[0] = x_; pangrams[x_] = 'The quick brown fox jumps over the lazy dog.'  -- not as short but at least it's still a pangram
  x_ = x_ + 1; pangrams[0] = x_; pangrams[x_] = 'The quick brown fox jumped over the lazy dog.' -- common misquote; not a pangram
  x_ = x_ + 1; pangrams[0] = x_; pangrams[x_] = 'The quick onyx goblin jumps over the lazy dwarf.'
  x_ = x_ + 1; pangrams[0] = x_; pangrams[x_] = 'Bored? Craving a pub quiz fix? Why, just come to the Royal Oak!' -- (Used to advertise a pub quiz in Bowness-on-Windermere)

  return pangrams

```

{{out}}
<pre style="overflow:scroll">
The quick brown fox jumps over a lazy dog.                       [OK, a pangram]
The quick brown fox jumps over the lazy dog.                     [OK, a pangram]
The quick brown fox jumped over the lazy dog.                    [Not a pangram.  Missing: S]
The quick onyx goblin jumps over the lazy dwarf.                 [OK, a pangram]
Bored? Craving a pub quiz fix? Why, just come to the Royal Oak!  [OK, a pangram]

```



## NewLISP


```newlisp

(context 'PGR)                              ;; Switch to context (say namespace) PGR
(define (is-pangram? str)
    (setf chars (explode (upper-case str))) ;; Uppercase + convert string into a list of chars
    (setf is-pangram-status true)           ;; Default return value of function
    (for (c (char "A") (char "Z") 1 (nil? is-pangram-status)) ;; For loop with break condition
        (if (not (find (char c) chars))     ;; If char not found in list, "is-pangram-status" becomes "nil"
            (setf is-pangram-status nil)
        )
    )
    is-pangram-status                       ;; Return current value of symbol "is-pangram-status"
)
(context 'MAIN)                             ;; Back to MAIN context

;; - - - - - - - - - -

(println (PGR:is-pangram? "abcdefghijklmnopqrstuvwxyz"))  ;; Print true
(println (PGR:is-pangram? "abcdef"))  ;; Print nil
(exit)

```



## Nim


```nim
import rdstdin

proc isPangram(sentence: string, alphabet = {'a'..'z'}): bool =
  var sentset: set[char] = {}
  for c in sentence: sentset.incl c
  alphabet <= sentset

echo isPangram(readLineFromStdin "Sentence: ")
```

Example usage:

```txt
Sentence: The quick brown fox jumps over the lazy dog
true
```



## Objeck

{{trans|Java}}

```objeck

bundle Default {
  class Pangram {
    function : native : IsPangram(test : String) ~ Bool {
      for(a := 'A'; a <= 'Z'; a += 1;) {
        if(test->Find(a) < 0 & test->Find(a->ToLower()) < 0) {
          return false;
        };
      };

      return true;
    }

    function : Main(args : String[]) ~ Nil {
      IsPangram("the quick brown fox jumps over the lazy dog")->PrintLine(); # true
      IsPangram("the quick brown fox jumped over the lazy dog")->PrintLine(); # false, no s
      IsPangram("ABCDEFGHIJKLMNOPQRSTUVWXYZ")->PrintLine(); # true
      IsPangram("ABCDEFGHIJKLMNOPQSTUVWXYZ")->PrintLine(); # false, no r
      IsPangram("ABCDEFGHIJKL.NOPQRSTUVWXYZ")->PrintLine(); # false, no m
      IsPangram("ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ")->PrintLine(); # true
      IsPangram("")->PrintLine(); # false
    }
  }
}

```



## OCaml



```ocaml
let pangram str =
  let ar = Array.make 26 false in
  String.iter (function
  | 'a'..'z' as c -> ar.(Char.code c - Char.code 'a') <- true
  | _ -> ()
  ) (String.lowercase str);
  Array.fold_left ( && ) true ar
```



```ocaml
let check str =
  Printf.printf " %b -- %s\n" (pangram str) str

let () =
  check "this is a sentence";
  check "The quick brown fox jumps over the lazy dog.";
;;
```


{{out}}
 false -- this is a sentence
 true -- The quick brown fox jumps over the lazy dog.

=={{header|MATLAB}} / {{header|Octave}}==


```matlab
function trueFalse = isPangram(string)
    % X is a histogram of letters
    X = sparse(abs(lower(string)),1,1,128,1);
    trueFalse = full(all(X('a':'z') > 0));
end
```


{{out}}

```txt
>>isPangram('The quick brown fox jumps over the lazy dog.')
ans = 1

```



## min

{{works with|min|0.19.3}}

```min
"abcdefghijklmnopqrstuvwxyz" "" split =alphabet
('alphabet dip lowercase (swap match) prepend all?) :pangram?

"The quick brown fox jumps over the lazy dog." pangram? puts
```



## ML

=
## mLite
=

```ocaml
fun to_locase s = implode ` map (c_downcase) ` explode s

fun is_pangram
	(h :: t, T) =
		let
			val flen = len (filter (fn c = c eql h) T)
		in
			if (flen = 0) then
				false
			else
				is_pangram (t, T)
		end
|	([], T) = true
| 	S = is_pangram (explode "abcdefghijklmnopqrstuvwxyz", explode ` to_locase S)

fun is_pangram_i
	(h :: t, T) =
		let
			val flen = len (filter (fn c = c eql h) T)
		in
			if (flen = 0) then
				false
			else
				is_pangram (t, T)
		end
|	([], T) = true
| 	(A,S) = is_pangram (explode A, explode ` to_locase S)

fun test (f, arg, res, ok, notok) = if (f arg eql res) then ("'" @ arg @ "' " @ ok) else ("'" @ arg @ "' " @ notok)
fun test2 (f, arg, res, ok, notok) = if (f arg eql res) then ("'" @ ref (arg,1) @ "' " @ ok) else ("'" @ ref (arg,1) @ "' " @ notok)

;
println ` test (is_pangram, "The quick brown fox jumps over the lazy dog", true, "is a pangram", "is not a pangram");
println ` test (is_pangram, "abcdefghijklopqrstuvwxyz", true, "is a pangram", "is not a pangram");
val SValphabet = "abcdefghijklmnopqrstuvwxyzåäö";
val SVsentence = "Yxskaftbud, ge vår wczonmö iq hjälp";
println ` test2 (is_pangram_i, (SValphabet, SVsentence), true, "is a Swedish pangram", "is not a Swedish pangram");

```

{{out}}

```txt
'The quick brown fox jumps over the lazy dog' is a pangram
'abcdefghijklopqrstuvwxyz' is not a pangram
'Yxskaftbud, ge vår wczonmö iq hjälp' is a Swedish pangram

```




## Oz


```oz
declare
  fun {IsPangram Xs}
     {List.sub
      {List.number &a &z 1}
      {Sort {Map Xs Char.toLower} Value.'<'}}
  end
in
  {Show {IsPangram "The quick brown fox jumps over the lazy dog."}}
```



## PARI/GP


```parigp
pangram(s)={
  s=vecsort(Vec(s),,8);
  for(i=97,122,
    if(!setsearch(s,Strchr(i)) && !setsearch(s,Strchr(i-32)),
      return(0)
    )
  );
  1
};

pangram("The quick brown fox jumps over the lazy dog.")
pangram("The quick brown fox jumps over the lazy doe.")
```



## Pascal

See [[Pangram_checker#Delphi | Delphi]]


## Perl

Get an answer with a module, or without.

```perl
use strict;
use warnings;
use feature 'say';

sub pangram1 {
    my($str,@set) = @_;
    use List::MoreUtils 'all';
    all { $str =~ /$_/i } @set;
}

sub pangram2 {
    my($str,@set) = @_;
    '' eq (join '',@set) =~ s/[$str]//gir;
}

my @alpha = 'a' .. 'z';

for (
    'Cozy Lummox Gives Smart Squid Who Asks For Job Pen.',
    'Crabby Lummox Gives Smart Squid Who Asks For Job Pen.'
) {
    say pangram1($_,@alpha) ? 'Yes' : 'No';
    say pangram2($_,@alpha) ? 'Yes' : 'No';
}
```

{{out}}

```txt
Yes
Yes
No
No
```



## Perl 6


```perl6
constant Eng = set 'a' .. 'z';
constant Cyr = set <а б в г д е ж з и й к л м н о п р с т у ф х ц ч ш щ ъ ы ь э ю я ё>;
constant Hex = set 'a' .. 'f';

sub pangram($str, Set $alpha = Eng) {
  $alpha ⊆ $str.lc.comb;
}

say pangram("The quick brown fox jumps over the lazy dog.");
say pangram("My dog has fleas.");
say pangram("My dog has fleas.", Hex);
say pangram("My dog backs fleas.", Hex);
say pangram "Съешь же ещё этих мягких французских булок, да выпей чаю", Cyr;
```

{{out}}

```txt
True
False
False
True
True
```



## Phix


```Phix
function pangram(string s)
sequence az = repeat(false,26)
integer count = 0
    for i=1 to length(s) do
        integer ch = lower(s[i])
        if ch>='a'
        and ch<='z'
        and not az[ch-96] then
            count += 1
            if count=26 then return {true,0} end if
            az[ch-96] = true
        end if
    end for
    return {false,find(false,az)+96}
end function

sequence checks = {"The quick brown fox jumped over the lazy dog",
                   "The quick brown fox jumps over the lazy dog",
                   ".!$\"AbCdEfghijklmnoprqstuvwxyz",
                   "THE FIVE BOXING WIZARDS DUMP QUICKLY.",
                   "THE FIVE BOXING WIZARDS JUMP QUICKLY.",
                   "HEAVY BOXES PERFORM WALTZES AND JIGS.",
                   "PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS.",
                   "Big fjiords vex quick waltz nymph",
                   "The quick onyx goblin jumps over the lazy dwarf.",
                   "no"}
for i=1 to length(checks) do
    string ci = checks[i]
    integer {r,ch} = pangram(ci)
    printf(1,"%-50s - %s\n",{ci,iff(r?"yes":"no "&ch)})
end for
```

{{out}}

```txt

The quick brown fox jumped over the lazy dog       - no s
The quick brown fox jumps over the lazy dog        - yes
.!$"AbCdEfghijklmnoprqstuvwxyz                     - yes
THE FIVE BOXING WIZARDS DUMP QUICKLY.              - no j
THE FIVE BOXING WIZARDS JUMP QUICKLY.              - yes
HEAVY BOXES PERFORM WALTZES AND JIGS.              - no c
PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS.           - yes
Big fjiords vex quick waltz nymph                  - yes
The quick onyx goblin jumps over the lazy dwarf.   - yes
no                                                 - no a

```



## PicoLisp


```PicoLisp
(de isPangram (Str)
   (not
      (diff
         '`(chop "abcdefghijklmnopqrstuvwxyz")
         (chop (lowc Str)) ) ) )
```



## PHP

{{trans|D}}

```php
function isPangram($text) {
    foreach (str_split($text) as $c) {
        if ($c >= 'a' && $c <= 'z')
            $bitset |= (1 << (ord($c) - ord('a')));
        else if ($c >= 'A' && $c <= 'Z')
            $bitset |= (1 << (ord($c) - ord('A')));
    }
    return $bitset == 0x3ffffff;
}

$test = array(
    "the quick brown fox jumps over the lazy dog",
    "the quick brown fox jumped over the lazy dog",
    "ABCDEFGHIJKLMNOPQSTUVWXYZ",
    "ABCDEFGHIJKL.NOPQRSTUVWXYZ",
    "ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ"
);

foreach ($test as $str)
    echo "$str : ", isPangram($str) ? 'T' : 'F', '</br>';
```



```txt
the quick brown fox jumps over the lazy dog : T
the quick brown fox jumped over the lazy dog : F
ABCDEFGHIJKLMNOPQSTUVWXYZ : F
ABCDEFGHIJKL.NOPQRSTUVWXYZ : F
ABC.D.E.FGHI*J/KL-M+NO*PQ R STUVWXYZ : T

```


Using array

```php
function is_pangram( $sentence ) {

    // define "alphabet"
    $alpha = range( 'a', 'z' );

    // split lowercased string into array
    $a_sentence = str_split( strtolower( $sentence ) );

    // check that there are no letters present in alpha not in sentence
    return empty( array_diff( $alpha, $a_sentence ) );

}

$tests = array(
    "The quick brown fox jumps over the lazy dog.",
    "The brown fox jumps over the lazy dog.",
    "ABCDEFGHIJKL.NOPQRSTUVWXYZ",
    "ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ",
    "How vexingly quick daft zebras jump",
    "Is hotdog?",
    "How razorback-jumping frogs can level six piqued gymnasts!"
);

foreach ( $tests as $txt ) {
    echo '"', $txt, '"', PHP_EOL;
    echo is_pangram( $txt ) ? "Yes" : "No", PHP_EOL, PHP_EOL;
}

```


{{Out}}

```txt

"The quick brown fox jumps over the lazy dog."
Yes

"The brown fox jumps over the lazy dog."
No

"ABCDEFGHIJKL.NOPQRSTUVWXYZ"
No

"ABC.D.E.FGHI*J/KL-M+NO*PQ R
STUVWXYZ"
Yes

"How vexingly quick daft zebras jump"
Yes

"Is hotdog?"
No

"How razorback-jumping frogs can level six piqued gymnasts!"
Yes

```



## PL/I


```PL/I

test_pangram: procedure options (main);

is_pangram: procedure() returns (bit(1) aligned);

   declare text character (200) varying;
   declare c character (1);

   get edit (text) (L);
   put skip list (text);

   text = lowercase(text);

   do c = 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
          'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
          'v', 'w', 'x', 'y', 'z';
      if index(text, c) = 0 then return ('0'b);
   end;
   return ('1'b);
end is_pangram;

   put skip list ('Please type a sentence');

   if is_pangram() then
      put skip list ('The sentence is a pangram.');
   else
      put skip list ('The sentence is not a pangram.');

end test_pangram;

```


{{out}}

```txt

Please type a sentence

the quick brown fox jumps over the lazy dog
The sentence is a pangram.

```



## PowerShell

Cyrillic test sample borrowed from Perl 6.
{{works with|PowerShell|2}}

```PowerShell

function Test-Pangram ( [string]$Text, [string]$Alphabet = 'abcdefghijklmnopqrstuvwxyz' )
    {
    $Text = $Text.ToLower()
    $Alphabet = $Alphabet.ToLower()

    $IsPangram = @( $Alphabet.ToCharArray() | Where-Object { $Text.Contains( $_ ) } ).Count -eq $Alphabet.Length

    return $IsPangram
    }

Test-Pangram 'The quick brown fox jumped over the lazy dog.'
Test-Pangram 'The quick brown fox jumps over the lazy dog.'
Test-Pangram 'Съешь же ещё этих мягких французских булок, да выпей чаю' 'абвгдежзийклмнопрстуфхцчшщъыьэюяё'

```

{{out}}

```txt

False
True
True

```

A faster version can be created using .Net HashSet to do what the F# version does:

```PowerShell

Function Test-Pangram ( [string]$Text, [string]$Alphabet = 'abcdefghijklmnopqrstuvwxyz' )
{
    $alSet   = [Collections.Generic.HashSet[char]]::new($Alphabet.ToLower())
    $textSet = [Collections.Generic.HashSet[char]]::new($Text.ToLower())

    $alSet.ExceptWith($textSet)    # remove text chars from the alphabet

    return $alSet.Count -eq 0    # any alphabet letters still remaining?
}

```



## Prolog

Works with SWI-Prolog


```Prolog
pangram(L) :-
	numlist(0'a, 0'z, Alphabet),
	forall(member(C, Alphabet), member(C, L)).

pangram_example :-
	L1 = "the quick brown fox jumps over the lazy dog",
	(   pangram(L1) -> R1= ok; R1 = ko),
	format('~s --> ~w ~n', [L1,R1]),

	L2 = "the quick brown fox jumped over the lazy dog",
	(   pangram(L2) -> R2 = ok; R2 = ko),
	format('~s --> ~w ~n', [L2, R2]).

```

{{out}}

```txt
?- pangram_example.
the quick brown fox jumps over the lazy dog --> ok
the quick brown fox jumped over the lazy dog --> ko
true.
```



## PureBasic


```PureBasic
Procedure IsPangram_fast(String$)
  String$ = LCase(string$)
  char_a=Asc("a")
  ; sets bits in a variable if a letter is found, reads string only once
  For a = 1 To Len(string$)
    char$ = Mid(String$, a, 1)
    pos   = Asc(char$) - char_a
    check.l |  1 << pos
  Next
  If check & $3FFFFFF = $3FFFFFF
    ProcedureReturn 1
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure IsPangram_simple(String$)
  String$ = LCase(string$)
  found   = 1
  For a = Asc("a") To Asc("z")
  ; searches for every letter in whole string
    If FindString(String$, Chr(a), 0) = 0
      found = 0
    EndIf
  Next
  ProcedureReturn found
EndProcedure

Debug IsPangram_fast("The quick brown fox jumps over lazy dogs.")
Debug IsPangram_simple("The quick brown fox jumps over lazy dogs.")
Debug IsPangram_fast("No pangram")
Debug IsPangram_simple("No pangram")
```



## Python

Using set arithmetic:

```python
import string, sys
if sys.version_info[0] < 3:
    input = raw_input

def ispangram(sentence, alphabet=string.ascii_lowercase):
    alphaset = set(alphabet)
    return alphaset <= set(sentence.lower())

print ( ispangram(input('Sentence: ')) )
```


{{out}}

```txt
Sentence: The quick brown fox jumps over the lazy dog
True
```



## R

Using the built-in R vector "letters":

```R
checkPangram <- function(sentence){
  my.letters <- tolower(unlist(strsplit(sentence, "")))
  is.pangram <- all(letters %in% my.letters)

  if (is.pangram){
    cat("\"", sentence, "\" is a pangram! \n", sep="")
  } else {
    cat("\"", sentence, "\" is not a pangram! \n", sep="")
  }
}

```


{{out}}

```txt
s1 <- "The quick brown fox jumps over the lazy dog"
s2 <- "The quick brown fox jumps over the sluggish dog"
checkPangram(s1)
"The quick brown fox jumps over the lazy dog" is a pangram!
checkPangram(s2)
"The quick brown fox jumps over the sluggish dog" is not a pangram!

```



## Racket



```Racket

#lang racket
(define (pangram? str)
  (define chars (regexp-replace* #rx"[^a-z]+" (string-downcase str) ""))
  (= 26 (length (remove-duplicates (string->list chars)))))
(pangram? "The quick Brown Fox jumps over the Lazy Dog")

```



## Retro


```Retro
: isPangram? ( $-f )
  ^strings'toLower
  heap [ 27 allot ] preserve
  [ @ 'a - dup 0 25 within [ [ 'a + ] [ here + ] bi ! ] &drop if ]
  ^types'STRING each@ here "abcdefghijklmnopqrstuvwxyz" compare ;
```



## REXX


```REXX
/*REXX program  verifies  if an  entered/supplied  string  (sentence)  is a pangram.    */
@abc= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'               /*a list of all (Latin) capital letters*/

    do forever;    say                           /*keep promoting 'til null (or blanks).*/
    say '──────── Please enter a pangramic sentence   (or a blank to quit):';      say
    pull y                                       /*this also uppercases the  Y variable.*/
    if y=''  then leave                          /*if nothing entered,  then we're done.*/
    absent= space( translate( @abc, , y), 0)     /*obtain a list of any absent letters. */
    if absent==''  then say  "──────── Sentence is a pangram."
                   else say  "──────── Sentence isn't a pangram, missing: "    absent
    say
    end   /*forever*/

say '──────── PANGRAM program ended. ────────'   /*stick a fork in it,  we're all done. */
```

{{out|output|:}}

```txt

──────── Please enter a pangramic sentence   (or a blank to quit):
The quick brown fox jumped over the lazy dog.      ◄■■■■■■■■■■ user input.
──────── Sentence isn't a pangram, missing:  S


──────── Please enter a pangramic sentence   (or a blank to quit):
The quick brown fox JUMPS over the lazy dog!!!     ◄■■■■■■■■■■ user input.
──────── Sentence is a pangram.


──────── Please enter a pangramic sentence   (or a blank to quit):
                                                   ◄■■■■■■■■■■ user input   (null  or  some blanks).

──────── PANGRAM program ended. ────────

```



## Ring


```ring

pangram = 0
s = "The quick brown fox jumps over the lazy dog."
see "" + pangram(s) + " " + s + nl

s = "My dog has fleas."
see "" + pangram(s) + " " + s + nl

func pangram str
     str  = lower(str)
     for i = ascii("a") to ascii("z")
             bool = substr(str, char(i)) > 0
             pangram = pangram + bool
     next
     pan = (pangram = 26)
     return pan

```



## Ruby


```ruby
def pangram?(sentence)
  ('a'..'z').all? {|chars| sentence.downcase.include? (chars) }
end

p pangram?('this is a sentence')  # ==> false
p pangram?('The quick brown fox jumps over the lazy dog.')  # ==> true
```



## Run BASIC


```runbasic
s$ = "The quick brown fox jumps over the lazy dog."
Print pangram(s$);" ";s$

s$ = "My dog has fleas."
Print pangram(s$);" ";s$

function pangram(str$)
  str$  = lower$(str$)
  for i = asc("a") to asc("z")
      pangram = pangram + (instr(str$, chr$(i)) <> 0)
  next i
pangram = (pangram = 26)
end function
```

```txt
1 The quick brown fox jumps over the lazy dog.
0 My dog has fleas.
```



## Rust


```rust
#![feature(test)]

extern crate test;

use std::collections::HashSet;

pub fn is_pangram_via_bitmask(s: &str) -> bool {

    // Create a mask of set bits and convert to false as we find characters.
    let mut mask = (1 << 26) - 1;

    for chr in s.chars() {
        let val = chr as u32 & !0x20; /* 0x20 converts lowercase to upper */
        if val <= 'Z' as u32 && val >= 'A' as u32 {
            mask = mask & !(1 << (val - 'A' as u32));
        }
    }

    mask == 0
}

pub fn is_pangram_via_hashset(s: &str) -> bool {

    // Insert lowercase letters into a HashSet, then check if we have at least 26.
    let letters = s.chars()
        .flat_map(|chr| chr.to_lowercase())
        .filter(|&chr| chr >= 'a' && chr <= 'z')
        .fold(HashSet::new(), |mut letters, chr| {
            letters.insert(chr);
            letters
        });

    letters.len() == 26
}

pub fn is_pangram_via_sort(s: &str) -> bool {

    // Copy chars into a vector, convert to lowercase, sort, and remove duplicates.
    let mut chars: Vec<char> = s.chars()
        .flat_map(|chr| chr.to_lowercase())
        .filter(|&chr| chr >= 'a' && chr <= 'z')
        .collect();

    chars.sort();
    chars.dedup();

    chars.len() == 26
}

fn main() {

    let examples = ["The quick brown fox jumps over the lazy dog",
                    "The quick white cat jumps over the lazy dog"];

    for &text in examples.iter() {
        let is_pangram_sort = is_pangram_via_sort(text);
        println!("Is \"{}\" a pangram via sort? - {}", text, is_pangram_sort);

        let is_pangram_bitmask = is_pangram_via_bitmask(text);
        println!("Is \"{}\" a pangram via bitmask? - {}",
                 text,
                 is_pangram_bitmask);

        let is_pangram_hashset = is_pangram_via_hashset(text);
        println!("Is \"{}\" a pangram via bitmask? - {}",
                 text,
                 is_pangram_hashset);
    }
}
```



## Scala


```scala
def is_pangram(sentence: String) = sentence.toLowerCase.filter(c => c >= 'a' && c <= 'z').toSet.size == 26

```



```scala

scala> is_pangram("This is a sentence")
res0: Boolean = false

scala> is_pangram("The quick brown fox jumps over the lazy dog")
res1: Boolean = true

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: isPangram (in string: stri) is func
  result
    var boolean: isPangram is FALSE;
  local
    var char: ch is ' ';
    var set of char: usedChars is (set of char).value;
  begin
    for ch range lower(stri) do
      if ch in {'a' .. 'z'} then
        incl(usedChars, ch);
      end if;
    end for;
    isPangram := usedChars = {'a' .. 'z'};
  end func;

const proc: main is func
  begin
    writeln(isPangram("This is a test"));
    writeln(isPangram("The quick brown fox jumps over the lazy dog"));
    writeln(isPangram("NOPQRSTUVWXYZ  abcdefghijklm"));
    writeln(isPangram("abcdefghijklopqrstuvwxyz"));  # Missing m, n
  end func;
```


{{out}}

```txt

FALSE
TRUE
TRUE
FALSE

```



## Sidef

{{trans|Perl 6}}

```ruby
define Eng = 'a'..'z';
define Hex = 'a'..'f';
define Cyr = %w(а б в г д е ж з и й к л м н о п р с т у ф х ц ч ш щ ъ ы ь э ю я ё);

func pangram(str, alpha=Eng) {
    var lstr = str.lc;
    alpha.all {|c| lstr.contains(c) };
}

say pangram("The quick brown fox jumps over the lazy dog.");
say pangram("My dog has fleas.");
say pangram("My dog has fleas.", Hex);
say pangram("My dog backs fleas.", Hex);
say pangram("Съешь же ещё этих мягких французских булок, да выпей чаю", Cyr);
```

{{out}}

```txt

true
false
false
true
true

```



## Smalltalk


```smalltalk
!String methodsFor: 'testing'!
isPangram
	^((self collect: [:c | c asUppercase]) select: [:c | c >= $A and: [c <= $Z]]) asSet size = 26

```



```smalltalk

'The quick brown fox jumps over the lazy dog.' isPangram

```



## SNOBOL4


{{works with|Macro Spitbol}}
{{works with|Snobol4+}}
{{works with|CSnobol}}


```SNOBOL4
        define('pangram(str)alfa,c') :(pangram_end)
pangram str = replace(str,&ucase,&lcase)
        alfa = &lcase
pgr_1   alfa len(1) . c = :f(return)
        str c :s(pgr_1)f(freturn)
pangram_end

        define('panchk(str)tf') :(panchk_end)
panchk  output = str
        tf = 'False'; tf = pangram(str) 'True'
        output = 'Pangram: ' tf :(return)
panchk_end

*       # Test and display
        panchk("The quick brown fox jumped over the lazy dogs.")
        panchk("My girl wove six dozen plaid jackets before she quit.")
        panchk("This 41-character string: it's a pangram!")
end
```


{{out}}

```txt
The quick brown fox jumped over the lazy dogs.
Pangram: True
My girl wove six dozen plaid jackets before she quit.
Pangram: True
This 41-character string: it's a pangram!
Pangram: False
```


## Swift


```Swift
import Foundation

let str = "the quick brown fox jumps over the lazy dog"

func isPangram(str:String) -> Bool {
    let stringArray = Array(str.lowercaseString)
    for char in "abcdefghijklmnopqrstuvwxyz" {
        if (find(stringArray, char) == nil) {
            return false
        }
    }
    return true
}

isPangram(str) // True
isPangram("Test string") // False
```

Swift 2.0:


```swift
func isPangram(str: String) -> Bool {
  let (char, alph) = (Set(str.characters), "abcdefghijklmnopqrstuvwxyz".characters)
  return !alph.contains {!char.contains($0)}
}
```



## Tcl


```tcl
proc pangram? {sentence} {
    set letters [regexp -all -inline {[a-z]} [string tolower $sentence]]
    expr {
        [llength [lsort -unique $letters]] == 26
    }
}
puts [pangram? "This is a sentence"];  # ==> false
puts [pangram? "The quick brown fox jumps over the lazy dog."]; # ==> true
```


=={{header|TI-83 BASIC}}==

```ti83b
:Prompt Str1
:For(L,1,26
:If not(inString(Str1,sub("ABCDEFGHIJKLMNOPQRSTUVWXYZ",L,1))
:L=28
:End
:If L<28
:Disp "IS A PANGRAM"
```

(not tested yet)


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
alfabet="abcdefghijklmnopqrstuvwxyz"
sentences = *
DATA The quick brown fox jumps over the lazy dog
DATA the quick brown fox falls over the lazy dog
LOOP s=sentences
 getchars      =STRINGS    (s," {&a} ")
 sortchars     =ALPHA_SORT (getchars)
 reducechars   =REDUCE     (sortchars)
 chars_in_s    =EXCHANGE   (reducechars," '  ")
 IF (chars_in_s==alfabet) PRINT "   pangram: ",s
 IF (chars_in_s!=alfabet) PRINT "no pangram: ",s
ENDLOOP

```

{{out}}

```txt

   pangram: The quick brown fox jumps over the lazy dog
no pangram: the quick brown fox falls over the lazy dog

```



## TXR


```txr
@/.*[Aa].*&.*[Bb].*&.*[Cc].*&.*[Dd].*& \
  .*[Ee].*&.*[Ff].*&.*[Gg].*&.*[Hh].*& \
  .*[Ii].*&.*[Jj].*&.*[Kk].*&.*[Ll].*& \
  .*[Mm].*&.*[Nn].*&.*[Oo].*&.*[Pp].*& \
  .*[Qq].*&.*[Rr].*&.*[Ss].*&.*[Tt].*& \
  .*[Uu].*&.*[Vv].*&.*[Ww].*&.*[Xx].*& \
  .*[Yy].*&.*[Zz].*/
```


{{out|Run}}

```txt
$ echo "The quick brown fox jumped over the lazy dog." | txr is-pangram.txr -
$echo $? # failed termination
1
$ echo "The quick brown fox jumped over the lazy dogs." | txr is-pangram.txr -
$ echo $?   # successful termination
0
```



## UNIX Shell

{{works with|Bourne Again SHell}}

```bash
function pangram? {
  local alphabet=abcdefghijklmnopqrstuvwxyz
  local string="$*"
  string="${string,,}"
  while [[ -n "$string" && -n "$alphabet" ]]; do
    local ch="${string%%${string#?}}"
    string="${string#?}"
    alphabet="${alphabet/$ch}"
  done
  [[ -z "$alphabet" ]]
}
```



## Ursala


```Ursala

#import std

is_pangram = ^jZ^(!@l,*+ @rlp -:~&) ~=`A-~ letters

```

example usage:

```Ursala

#cast %bL

test =

is_pangram* <
   'The quick brown fox jumps over the lazy dog',
   'this is not a pangram'>

```

{{out}}

```txt

<true,false>

```



## VBA

The function pangram() in the VBScript section below will do just fine.

Here is an alternative version:


```vb

Function pangram2(s As String) As Boolean
    Const sKey As String = "abcdefghijklmnopqrstuvwxyz"
    Dim sLow As String
    Dim i As Integer

    sLow = LCase(s)
    For i = 1 To 26
      If InStr(sLow, Mid(sKey, i, 1)) = 0 Then
        pangram2 = False
        Exit Function
      End If
    Next
    pangram2 = True
End Function

```


Invocation e.g. (typed in the Immediate window):

```txt

print pangram2("the quick brown dog jumps over a lazy fox")
print pangram2("it is time to say goodbye!")

```



## VBScript


### =Implementation=


```vb
function pangram( s )
	dim i
	dim sKey
	dim sChar
	dim nOffset
	sKey = "abcdefghijklmnopqrstuvwxyz"
	for i = 1 to len( s )
		sChar = lcase(mid(s,i,1))
		if sChar <> " "  then
			if instr(sKey, sChar) then
				nOffset = asc( sChar ) - asc("a")  + 1
				if nOffset > 1 then
					sKey = left(sKey, nOffset - 1) & " " & mid( sKey, nOffset + 1)
				else
					sKey = " " & mid( sKey, nOffset + 1)
				end if
			end if
		end if
	next
	pangram = ( ltrim(sKey) = vbnullstring )
end function

function eef( bCond, exp1, exp2 )
	if bCond then
		eef = exp1
	else
		eef = exp2
	end if
end function
```



### =Invocation=


```vb
wscript.echo eef(pangram("a quick brown fox jumps over the lazy dog"), "is a pangram", "is not a pangram")
wscript.echo eef(pangram(""), "is a pangram", "is not a pangram")"
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings

func StrLen(Str);               \Return number of characters in an ASCIIZ string
char Str;
int  I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;

func Pangram(S);
char S;
int  A, I, C;
[A:= 0;
for I:= 0 to StrLen(S)-1 do
        [C:= S(I);
        if C>=^A & C<=^Z then C:= C or $20;
        if C>=^a & C<=^z then [C:= C - ^a;  A:= A or 1<<C];
        ];
return A = $3FFFFFF;
]; \Pangram

int Sentence, I;
[Sentence:=
    ["The quick brown fox jumps over the lazy dog.",
     "Pack my box with five dozen liquor jugs.",
     "Now is the time for all good men to come to the aid of their country."];
for I:= 0 to 3-1 do
    [Text(0, if Pangram(Sentence(I)) then "yes" else "no");
    CrLf(0);
    ];
]
```


{{out}}

```txt

yes
yes
no

```



## Yabasic


```Yabasic
sub isPangram$(t$, l1$)
	local lt, ll, r$, i, cc, ic

	if numparams = 1 then
		l1$ = "abcdefghijklmnopqrstuvwxyz"
	end if

	t$ = lower$(t$)
	ll = len(l1$)
	for i = 1 to ll
		r$ = r$ + " "
	next
	lt = len(t$)
	cc = asc("a")

	for i = 1 to lt
		ic = asc(mid$(t$, i, 1)) - cc + 1
		if ic > 0 and ic <= ll then
			mid$(r$, ic, 1) = chr$(ic + cc - 1)
		end if
	next i

	if l1$ = r$ then return "true" else return "false" end if

end sub

print isPangram$("The quick brown fox jumps over the lazy dog.")   // --> true
print isPangram$("The quick brown fox jumped over the lazy dog.")  // --> false
print isPangram$("ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ")          // --> true
```



## zkl


```zkl
var letters=["a".."z"].pump(String); //-->"abcdefghijklmnopqrstuvwxyz"
fcn isPangram(text){(not (letters-text.toLower()))}
```

{{out}}

```txt

isPangram("The quick brown fox jumps over the lazy dog.")
True
isPangram("Pack my box with five dozen liquor jugs.")
True
isPangram("Now is the time for all good men to come to the aid of their country.")
False

```


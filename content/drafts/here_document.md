+++
title = "Here document"
description = ""
date = 2019-09-06T23:49:24Z
aliases = []
[extra]
id = 9634
[taxonomies]
categories = []
tags = []
+++

{{task}} [[Category: Syntax elements]]
{{omit from|BBC BASIC}}
{{omit from|Déjà Vu}}
{{omit from|Gambas}}
{{omit from|GW-BASIC}}
{{omit from|Pascal}}
{{omit from|MATLAB|MATLAB has no multiline string literal functionality}}
{{omit from|VBA|VBA can't create a console application}}
A   ''here document''    (or "heredoc")   is a way of specifying a text block, preserving the line breaks, indentation and other whitespace within the text. 

Depending on the language being used, a   ''here document''   is constructed using a command followed by "<<" (or some other symbol) followed by a token string. 

The text block will then start on the next line, and will be followed by the chosen token at the beginning of the following line, which is used to mark the end of the text block.


;Task:
Demonstrate the use of   ''here documents''   within the language.


;Related task:
*   [[Documentation]]





## 8th

Multiline strings are simply parsed using "quote", which parses first a character to use as a separator, and scans until that character is found:

```forth

quote *
Hi
   there
* . 

```

{{out}}

```txt

Hi
   there

```


## Ada


Ada has neither heredocs nor multiline strings. 
A workaround is to use containers of strings:


```Ada
with Ada.Containers.Indefinite_Vectors, Ada.Text_IO;

procedure Here_Doc is

   package String_Vec is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   use type String_Vec.Vector;

   Document: String_Vec.Vector := String_Vec.Empty_Vector
    & "This is a vector of strings with the following properties:"
    & "  - indention is preserved, and"
    & "  - a quotation mark '""' must be ""escaped"" by a double-quote '""""'.";
begin
   Document := Document & "Have fun!";
   for I in Document.First_Index .. Document.Last_Index loop
      Ada.Text_IO.Put_Line(Document.Element(I));
   end loop;
end Here_Doc;
```


{{out}}

```txt
This is a vector of strings with the following properties:
  - indention is preserved, and
  - a quotation mark '"' must be "escaped" by a double-quote '""'.
Have fun!
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
Algol 68 does not have a "heredoc" feature.  
It can be crudely achieved using an array of strings:


```algol68
#!/usr/local/bin/a68g --script #

[]STRING help = (
"Usage: thingy [OPTIONS]",
"     -h                        Display this usage message",
"     -H hostname               Hostname to connect to"
);

printf(($gl$,help,$l$));

printf(($gl$,
"The river was deep but I swam it, Janet.",
"The future is ours so let's plan it, Janet.",
"So please don't tell me to can it, Janet.",
"I've one thing to say and that's ...",
"Dammit. Janet, I love you."
))

```


{{out}}

```txt

Usage: thingy [OPTIONS]
     -h                        Display this usage message
     -H hostname               Hostname to connect to

The river was deep but I swam it, Janet.
The future is ours so let's plan it, Janet.
So please don't tell me to can it, Janet.
I've one thing to say and that's ...
Dammit. Janet, I love you.
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program heredoc.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ BUFFERSIZE,          100

/* Initialized data */
.data
szMessString:            .asciz "String :
 ABCD
            EFGH	TAB    \n"

szCarriageReturn:       .asciz "\n"

/* UnInitialized data */
.bss 

/*  code section */
.text
.global main 
main: 

    ldr r0,iAdrszMessString                     @ display message
    bl affichageMess
 

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessString:         .int szMessString
iAdrszCarriageReturn:     .int szCarriageReturn

/******************************************************************/
/*     display text with size calculation                         */ 
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers 
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index 
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop 
                                                @ so here r2 contains the length of the message 
    mov r1,r0                                   @ address message in r1 
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write" 
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return

```




## AutoHotkey

AutoHotkey uses "continuation sections" for literal text:


```AutoHotkey
MyVar = "This is the text inside MyVar"
MyVariable =
(
   Note that whitespace is preserved
   As well as newlines.
   The LTrim option can be present to remove left whitespace.
   Variable references such as %MyVar% are expanded.
)
MsgBox % MyVariable
```



## AWK


The awk extraction and reporting language does not provide any markup facility for embedding here documents within an awk script. The awk utility is really a helper tool often used from within the Unix shell. The Unix shell in which awk scripts are usually embedded does support the use of here documents, and the way that here documents are used within the shell make them ideal for passing to awk as is, without the need for an additional facility in awk.


## Bracmat

Strings in Bracmat can continue over many lines. They start and end with a quote. Quotes in the text must be escaped with a reverse solidus, like the reverse solidus itself.


```bracmat
( {Multiline string:}
"
Second line
Third line
\"quoted\"
A backslash: \\
":?stringA
& out$("Multiline string:")
& out$(!stringA)
)
```

Output:

```txt
Multiline string:

Second line
Third line
"quoted"
A backslash: \

```



## C

The program speaks for itself ;)

```C

#include<stdio.h>

int main()
{
    printf("\nThe Heredoc task was marked not implementable in C.\
    \nFrankly the person who did so seems to have little idea of what C\
    is capable of.\n\
    After all, what would one call this multiline printf statement ?\
    I may be old, but do not forget that it all started with me.\
    \n\nEver enigmatic...\n\
    C   ");
    
    return 0;
}

```

Output, extra spaces are due to source code indentation:

```txt

The Heredoc task was marked not implementable in C.
Frankly the person who did so seems to have little idea of what C       is capable of.
        After all, what would one call this multiline printf statement ?        I may be old, but do not forget that it all started with me.

Ever enigmatic...
        C

```



## C++

{{works with|C++11}}
C++11 raw string literals are similar to heredocs, except there is no newline after the opening token or before the ending token (unless you actually want newlines there).


```cpp>#include <iostream
 // Only for cout to demonstrate

int main()
{
  std::cout <<
R"EOF(  A  raw  string  begins  with  R,  then a double-quote ("),  then an optional
identifier (here I've used "EOF"),  then an opening parenthesis ('(').  If you
use  an  identifier,  it  cannot  be longer than 16 characters,  and it cannot
contain a space,  either opening or closing parentheses, a backslash, a tab, a
vertical tab, a form feed, or a newline.

  It  ends with a closing parenthesis (')'),  the identifer (if you used one),
and a double-quote.

  All  characters are okay in a raw string,  no escape sequences are necessary
or recognized, and all whitespace is preserved.
)EOF";
}
```



## C sharp

C# has a string literal call which is used for heredoc functionality


```C sharp
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.Write(@"
multiline
strings are easy
to put together
in C#");
    }
}
```



## Clojure

There are no heredocs built in, but Clojure Strings themselves are multiline and whitespace/indentation is preserved.  If your text has many characters that have to be escaped, it may make more sense to store it as a resource file and read it in.

For more information on the possibility of supporting a tripled-quoted syntax, see: [http://dev.clojure.org/display/design/Alternate+string+quote+syntaxes alternate string quote syntaxes].


## CoffeeScript

CoffeeScript borrows the triple-quoted string syntax from Python. Note that these strings strip leading whitespace in CoffeeScript, to allow you to neatly align the heredoc string.


```coffeescript
myDoc = '''
        Single-quoted heredocs allows no '#{foo}' interpolation.
        This behavior is similar to single-quoted strings.
        '''
doc2  = """
        However, double-quoted heredocs *do* allow these.
        See it in action:
            Content: "#{myDoc}"
        """

console.log doc2
```


{{out}}

```txt

However, double-quoted heredocs *do* allow these.
See it in action:
    Content: "Single-quoted heredocs allows no '#{foo}' interpolation.
This behavior is similar to single-quoted strings."

```


Note how the extra indentation in the third line of doc2 is preserved.


## D


```d
import std.stdio, std.string;

void main() {
    // Delimited strings: a 'q' followed by double quotes and an
    // opening and closing delimiter of choice:

    q"[a string that you "don't" have to escape]"
    .writeln;

    // If the delimiter is an identifier, the identifier must be
    // immediately followed by a newline, and the matching delimiter
    // is the same identifier starting at the beginning of the line:

    q"EOS
    This
    is a multi-line
    heredoc string
EOS".outdent.writeln;

    // std.string.outdent is used to remove the four spaces indent.
}
```

{{out}}

```txt
a string that you "don't" have to escape
This
is a multi-line
heredoc string

```



## DWScript

Double-quotes (") denote a multi-line string, to include a double-quote in such a string, you need to double it.

```delphi
PrintLn("This is
a multiline
""string""
sample");
```

{{out}}

```txt
This is
a multiline
"string"
sample
```



## EchoLisp

The delimiters are #<< .... >># for a here-doc string, which may include any special character: double quotes, new line, etc. and is read 'as is', i.e does not recognize escape sequences.

```txt

(string-delimiter "")
(writeln
#<<
A noir, E blanc, I rouge, U vert, O bleu : voyelles,
Je dirai quelque jour vos naissances latentes :
A, noir corset velu des mouches éclatantes
Qui bombinent autour des puanteurs cruelles,

Golfes d'ombre ; E, candeur des vapeurs et des tentes,
Lances des glaciers fiers, rois blancs, frissons d'ombelles ;
I, pourpres, sang craché, rire des lèvres belles
Dans la colère ou les ivresses pénitentes ;

U, cycles, vibrements divins des mers virides,
Paix des pâtis semés d'animaux, paix des rides
Que l'alchimie imprime aux grands fronts studieux ;

O, suprême Clairon plein des strideurs étranges,
Silences traversés des Mondes et des Anges :
- O l'Oméga, rayon violet de Ses Yeux ! -

A. Rimbaud "Voyelles"
>>#

```

{{out}}

```txt

A noir, E blanc, I rouge, U vert, O bleu : voyelles,
Je dirai quelque jour vos naissances latentes :
A, noir corset velu des mouches éclatantes
Qui bombinent autour des puanteurs cruelles,

Golfes d'ombre ; E, candeur des vapeurs et des tentes,
Lances des glaciers fiers, rois blancs, frissons d'ombelles ;
I, pourpres, sang craché, rire des lèvres belles
Dans la colère ou les ivresses pénitentes ;

U, cycles, vibrements divins des mers virides,
Paix des pâtis semés d'animaux, paix des rides
Que l'alchimie imprime aux grands fronts studieux ;

O, suprême Clairon plein des strideurs étranges,
Silences traversés des Mondes et des Anges :
- O l'Oméga, rayon violet de Ses Yeux ! -

A. Rimbaud "Voyelles"

```



## Elixir

In Elixir, one can use either a pair of triple single-quotation marks or a pair of triple double-quotation marks, but in both cases, string interpolation occurs:

```elixir
IO.puts """
привет
мир
"""
```

produces:
```sh
привет
мир
```


Here is an illustrative iex transcript:

```elixir
iex(1)> a=2
2
iex(2)> '''
...(2)> 1 + 1 = #{a}
...(2)> '''
'1 + 1 = 2\n'
iex(3)> 
iex(3)> '''2'''
** (SyntaxError) iex:3: heredoc start must be followed by a new line after '''
    
iex(3)>
```



## Erlang

Multiline strings look like this in the Erlang shell:

```Erlang

2> S = " ad
2> 123
2> the end".
3> io:fwrite( S ).
 ad
123
the end

```


=={{header|F_Sharp|F#}}==
All the usual string variations are no HEREDOCs, because they don't comply to this requirement:
<blockquote style="font-size:90%">The text block will then start on the next line, and will be followed by the chosen token at the beginning of the following line, which is used to mark the end of the textblock.</blockquote>
That said, as with other languages, F# has two string syntax variations which come close:
<dl>
<dt>verbatim strings</dt>
<dd>begin with <code>@"</code> and end with the next single double quote <code>"</code>. A double quote in the string has to be quoted by doubling it. All other characters can be in such a string as-is, including newlines.</dd>
<dt>tripple-quoted strings</dt>
<dd>are delimited by <code>"""</code> on both ends and can contain any character other than the delimiter sequence of three double quote characters.</dd>
</dl>
Full information on F# strings can be found in [https://msdn.microsoft.com/visualfsharpdocs/conceptual/strings-%5bfsharp%5d MSDN].


## Factor

Factor strings surround by '"' are multiline, but accept escape sequences (like "\n", "\uxxxxxxxx"). Strings surrounded by '"""' don't have to escape '"'. Use HEREDOC: (and other variants) for verbatim text

```factor
"    a multiline
string\n(with escape sequences: \u{greek-capital-letter-sigma})
"
"""this is "easier".."""
HEREDOC: EOF
             this
 is not \n escaped at all
EOF
```


## Forth



### version 1

{{works with|GForth}}

```Forth
\ GForth specific words:
\ under+ ( a b c -- a+c b) , latest ( -- nt ) , name>string ( nt -- ca u )
\ Should not be a problem to modify it to work with other Forth implementation:

: $!   ( ca u -- a )
  dup >R dup , here swap move R> allot ;
: $@   ( a -- ca u )
  dup @ 1 cells under+ ;
: c!+   ( c ca - ca+1 )
  tuck c! 1+ ;
: $!+   ( a u a' -- a'+u ; string-store-plus )
  2dup + >R swap move R> ;

\ --- UNIX end-of-line adapted words
10 constant EOL
: input-fix   ( -- ; for interactive use ! )
  source-id 0= IF cr THEN ;
: get_line  ( -- ca u )  EOL parse input-fix ;
: EOL!+  ( a -- a' ; eol-store-plus )  EOL swap c!+ ;

: EOD   ( -- ca u ; end-of-doc )
  latest name>string ;

: >>   ( "doc-name" "doc-body" -- )   input-fix 
  CREATE 0 ,              \ post body length
  here dup >R
  BEGIN  refill >R
         get_line 2dup EOD compare
         R> AND           \ notEOD && input-stream ->
  WHILE  rot $!+ EOL!+
  REPEAT 2drop
  R> tuck - dup allot
  swap -1 cells + !       \ fixup body length
  DOES>  ( -- ca u )  $@ ;

\ TEST ;   excerpt from Project Gutenberg 'Alice in Wonderland'

>> ALICE
CHAPTER I. Down the Rabbit-Hole

Alice was beginning to get very tired of sitting by her sister on the
bank, and of having nothing to do: once or twice she had peeped into the
book her sister was reading, but it had no pictures or conversations in
it, 'and what is the use of a book,' thought Alice 'without pictures or
conversation?'
ALICE
>> RABBIT
RABBIT
ALICE type ." --" cr RABBIT type
```



```txt

CHAPTER I. Down the Rabbit-Hole

Alice was beginning to get very tired of sitting by her sister on the
bank, and of having nothing to do: once or twice she had peeped into the
book her sister was reading, but it had no pictures or conversations in
it, 'and what is the use of a book,' thought Alice 'without pictures or
conversation?'
--

```



### version 2

{{works with|GForth}}
Here's a slightly different implementation, with less stack juggling. It does not echo linefeeds in interactive mode, but that can easily be added.

It's written to works with Forth's default implementation of counted strings. Those are limited to 255 characters, which is fine in many cases, but on the short side for here documents, so therefore we temporarily redefine them to use a full cell for size (16 bits would probably be ideal) while compiling the here document words.

```Forth
get-current get-order wordlist swap 1+ set-order definitions
: place		over >r rot over cell+ r> move ! ;
: +place	2dup >r >r dup @ cell+ + swap move r> r> dup @ rot + swap ! ;
: count		dup cell+ swap @ ;
set-current

CREATE eol	10 C,  DOES> 1 ;
CREATE eod	128 ALLOT  DOES> count ;
: seteod	0 parse ['] eod >body place ;
: start		0 0 here place ;
: read		refill  0= IF abort" Unterminated here document" THEN ;
: ~end		source eod compare ;
: store		source here +place  eol here +place ;
: slurp		BEGIN read ~end WHILE store REPEAT  refill drop ;
: totalsize	here count + here - ;
: finish	totalsize ALLOT  align ;
: <<		seteod  start slurp finish  DOES> count ;
previous

( Test )

CREATE Alice << ~~ end ~~
They got a building down New York City, it’s called Whitehall Street,
Where you walk in, you get injected, inspected, detected, infected,
Neglected and selected.
~~ end ~~

Alice type  bye
```


{{out}}

```txt

They got a building down New York City, it’s called Whitehall Street,
Where you walk in, you get injected, inspected, detected, infected,
Neglected and selected.

```



## Fortran

In Fortran, spaces outside text literals are nowhere significant, not even within words, so that <code>GOTO = G OTO = GO TO</code> and there are no reserved words either so GOTO might be the name of a variable as well and both can be used in the same source. Statements were originally specified with a special format: columns one to five were for statement labels (integers only, not zero), column six specified a continuation line if not blank or zero, and columns 73-80 were for sequence numbers in case you dropped the deck of cards. Thus, source was found in columns 7-72, and layout of the source could be varied without effect on the results. Originally, the only way to produce text in the output (as for annotating the numerical results) was via the "Hollerith" format code, of the form nH where n specified the ''exact'' count of characters following the H. Any characters. Miscounts would cause a syntax error - if you were lucky! Happily, a quoted literal syntax was soon introduced, using apostrophes as delimiters with two apostrophes in a row signifying a contained apostrophe. Later, either an apostrophe or a double quote could be used to start a text string (and the same one must be used to end it) so that if one or the other were desired within a text literal, the other could be used as its delimiters. If both were desired, then there would be no escape from doubling for one.

Producing a source file containing a block of text that will be presented as output in the same layout (new lines and all) is only possible if the added layout stuff (generating the required new line starts) and delimiters in the source are disregarded. But if one accepts a few constraints, the following shows a possible approach:

```Fortran

      INTEGER I         !A stepper.
      CHARACTER*666 I AM    !Sufficient space.
      I AM =                                                           "<col72
C              111111111122222222223333333333444444444455555555556666666666
C     123456789012345678901234567890123456789012345678901234567890123456789
     1                                                                  <col72
     2              I AM                                                <col72
     3                                                                  <col72
     4           THAT I AM                                              <col72
     5"

Chug through the text blob.
      DO I = 0,600,66   !Known length.
        WRITE (6,1) I AM(I + 1:I + 66)  !Reveal one line.
    1   FORMAT (A66)            !No control characters are expected.
      END DO                !On to the next line.
      END

```

With old-style fixed-format source, rather amusingly there is space for sixty-six characters of text per line and so this is the canvas. Any usage of tab characters must be resolved into spaces (and some compilers count up to 72 oddly when tabs are involved) and likewise with new lines. A surprise was provided to an old-time card flapper when it became apparent that trailing spaces were ''not'' being incorporated into the text literal unless the "<col72" markers were appended to column seventy-two of the source. An old-time card deck copied to disc and with the sequence numbers in columns 73-80 removed (as no-one is going to drop a disc file so some storage space can be saved) might thereby compile incorrectly! Similarly, the source text highlighter here does not fully recognise the odd tricks available for Fortran syntax, apparently deeming the comments a part of a text literal. The F90 compiler's highlighting does, and shows that text beyond column 72 is not Fortran code.

The text blob could instead be an array, but in that case, each line would have to be delimited by quotes and a comma added, reducing the width of the canvas and adding more stuff to be disregarded. But with a single blob of text, the output statement must step through the parts of the blob to extract the lines. If the statement were <code> WRITE (6,1) I AM</code> then only the first 66 characters of I AM would be rolled forth. If the format code were just A instead of A66, then all the text would be rolled, but the output would not be presented with 66 characters per line. So, some complications lead to :

```txt


              I AM

           THAT I AM




```



## Free Pascal

In Pascal, the only forbidden character in string literals is the newline character.
However, as of 2019‑09‑06 in a trunk version of the FPC (Free Pascal compiler) support for string literals spanning multiple lines can be enabled.


## Frink

Frink does not have awkward here-docs.  Triple-quoted strings serve the same purpose, but more concisely.  (The Perl, PHP, etc. syntax for here-documents is a violation of the "define everything at most once" principle of software engineering.)  Variable interpolation is allowed within triple-quoted strings.

```frink

lyrics = """Oh, Danny Boy,
The pipes, the pipes are calling
From glen to glen and down the mountainside"""

```



## Genie

Genie includes triple quoted verbatim strings and "at" quoted template strings, which can be used as Heredoc data in source. Given the limitation of terminating quotes not being a user defined sequence, inner quotations will need to be escaped or transcluded.


```genie
[indent=4]
/*
  Here documents, as template and verbatim strings in Genie
  valac heredoc.gs
*/
init
    test:string = "Genie string"

    var multilineString = """
this is a $test
"""

    var templateString = @"
this is a $test template
with math for six times seven = $(6 * 7)
"

    stdout.printf("%s", multilineString)
    stdout.printf("%s", templateString)
```


{{out}}

```txt
prompt$ valac heredoc.gs
prompt$ ./heredoc

this is a $test

this is a Genie string template
with math for six times seven = 42
```



## Go

Go does not have here documents.  Multiline string literals serve this purpose.


```go
var m = `    leading spaces

and blank lines`
```



## Groovy

Groovy has two types of multi-line strings, which behave similarly to "here documents"

===Multi-line String literal===
The literal text, preserving lines and spacing

```groovy
println '''
Time's a strange fellow;
                        more he gives than takes
(and he takes all) nor any marvel finds
quite disappearance but some keener makes
losing, gaining
               --love! if a world ends
'''
```


{{out}}

```txt

Time's a strange fellow;
                        more he gives than takes
(and he takes all) nor any marvel finds
quite disappearance but some keener makes
losing, gaining
               --love! if a world ends

```


===Multi-line GString expression===
Like single-line GString expressions, any subexpression delimited with ${ } is substituted with its "toString()" value. Preserves lines and spacing outside of the subexpressions.

```groovy
def expired='defunct'
def horse='stallion'
def christ='Jesus'

println """
Buffalo Bill's 
              ${expired} 
                          who used to 
                          ride a watersmooth-silver 
                                                                  ${horse} 
              and break onetwothreefourfive pigeonsjustlikethat 
                                                                                           ${christ}

              he was a handsome man 
                                                    and what i want to know is 
              how do you like your blueeyed boy 
              Mister Death
"""
```


{{out}}

```txt

Buffalo Bill's 
              defunct 
                          who used to 
                          ride a watersmooth-silver 
                                                                  stallion 
              and break onetwothreefourfive pigeonsjustlikethat 
                                                                                           Jesus

              he was a handsome man 
                                                    and what i want to know is 
              how do you like your blueeyed boy 
              Mister Death

```



## Haskell



```Haskell


main :: IO ()
main = do

-- multiline String
  putStrLn "Hello\
            \ World!\n"

-- more haskell-ish way
  putStrLn $ unwords ["This", "is", "an", "example", "text!\n"]

-- now with multiple lines
  putStrLn $ unlines [
             unwords ["This", "is", "the", "first" , "line."]
           , unwords ["This", "is", "the", "second", "line."]
           , unwords ["This", "is", "the", "third" , "line."]
           ]


```


Output:


```txt


Hello World!

This is an example text!

This is the first line.
This is the second line.
This is the third line.


```



## J


```j
here=:0 :0
  0 :0 will be replaced by the text on the following lines.
  This is three tokens: two instances of the number 0 and
  one instance of the explicit definition token ':'.
  Any indentation in the here document will be retained in the result.
  There must be a space to the left of : or it will combine with the
  0 on its left to form the token 0: which is something completely 
  different.

  The here document is terminated by a line which contains only a
  single right parenthesis ')' and optional white space.  In J's
  documentation the family of entities which include here documents
  (and verb definitions and so on) are called 'scripts'.

  When several scripts are referenced on the same line, they are used
  sequentially in an order determined by their appearance on the line.
  The leftmost 'script' reference gets the last script and the rightmost
  reference gets the first script.  But this is a rare usage.

  Typically, such values are assigned a name so that they can be
  used later.  However, they may also be discarded and/or ignored, in
  which case they are logically equivalent to multi-line comments.
)

and_here=:noun define
  'noun define' is an alternative and perhaps more "user friendly"
  way of declaring a here document.  It achieves the same thing as
  0 :0 and in fact 'noun' has the value 0 and 'define' has the value :0
  And, of course, there must be a space between the word 'noun' and
  the word 'define'.

  Other useful alternatives include verb (which has the value 3)
  and dyad (which has the value 4), and adverb (which has the value 1).
  In other words 'verb define' (if unquoted) would be replaced by a 
  verb whose definition is provided in the following 'script'.
  However, all of these names are normal variables which can
  be declared to have different values by the developer.  And, of course,
  note that this mechanism is significantly more verbose than using
  the underlying 0 :0 mechanism directly.
)
```



## JavaScript


### ES6

ES6 introduced template literals. These are string literals that suport multi-line text and can include 
interpolated expressions using ${···} syntax. It is indicated with the backtich character [`]

```JavaScript
const myVar = 123;
const tempLit = `Here is some
multi-line string. And here is
the value of "myVar": ${myVar}
That's all.`;
console.log(tempLit)

```



```txt
Here is some
multi-line string. And here is
the value of "myVar": 123
That's all.
```



## jq

No special syntax is required to support "here documents" in jq in that any JSON string, and indeed any string specifier (as explained below), can be presented using a multiline format.

For example, consider:
```jq

def s:
"x
y
z";

s
```

{{Out}}

```sh
$ jq -n s.jq
"x\ny\nz"
```


String specifiers, that is possibly non-JSON strings which incorporate references to jq expressions, are handled in the same way. For example, the following program produces the same result:

```jq
def specifier(a):
"x
\(a)
z";

specifier("y")
```


Most control characters, such as Control-A and Control-Z, can also be presented literally, but the RosettaCode.org editor disallows them in general, so the next example only shows an embedded literal tab:

```jq
"a
tab:    end
parens:()
single quotation mark:'
double quotation mark must be escaped:\"
b
d"
```

{{out}}

```sh
"a\ntab:\tend\nparens:()\nsingle quotation mark:'\ndouble quotation mark must be escaped:\"\nb\nd"
```



## Julia

Like Python, Julia has triple-quoted string literals, which are similar to here-docs:


```julia
print("""\
Usage: thingy [OPTIONS]
     -h                        Display this usage message
     -H hostname               Hostname to connect to
""")
```



## Kotlin

Kotlin's 'raw string literals' behave in a similar way to 'here' documents in that line breaks, indentation and other whitespace is preserved within the string. They are distingished from normal strings by being surrounded by triple quote characters rather than by single quotes.

Escaped characters (such as \n, \t etc.) do not have a special meaning and so are treated literally.

It's also possible to embed variables and other expressions in a raw string literal using Kotlin's string interpolation facilities.

Here's an example of all this:

```scala
// version 1.1.0

fun main(args: Array<String>) {
    val ev = "embed variables"

    val here = """
               This is a raw string literal   
               which does not treat escaped characters 
               (\t, \b, \n, \r, \', \", \\, \$ and \u)
               specially and can contain new lines,
               indentation and other        whitespace
               within the string.
 
"Quotes" or doubled ""quotes"" can
be included without problem but not
tripled quotes.

           It's also possible to $ev
           in a raw string literal using string
           interpolation. 

  If you need to include a
  literal ${'$'} sign in a raw string literal then
  don't worry you've just done it!
               """

    println(here)
}
```


{{out}}

```txt

               This is a raw string literal
               which does not treat escaped characters
               (\t, \b, \n, \r, \', \", \\, \$ and \u)
               specially and can contain new lines,
               indentation and other        whitespace
               within the string.

"Quotes" or doubled ""quotes"" can
be included without problem but not
tripled quotes.

           It's also possible to embed variables
           in a raw string literal using string
           interpolation.

  If you need to include a
  literal $ sign in a raw string literal then
  don't worry you've just done it!


```



## Lingo

Lingo has no heredoc syntax. Quotes inside string literals have to be escaped with "&QUOTE&".<br />
But you can define multi-line strings using the line continuation character "\":


```lingo
str = "This is the first line.\
This is the second line.\
This "&QUOTE&"line"&QUOTE&" contains quotes."

put str
```

{{out}}

```txt

This is the first line.
This is the second line.
This "line" contains quotes.

```


For longer text it's more comfortable to put it into a named "field member" (=asset container) and load its text into a variable when needed:
```lingo
str = member("my long text").text
put str
```



## Lua

Lua uses the [ [ to mark the start of a dochere block and ] ] to mark the end. It can be used directly or while assigning strings to a variable.

```lua

print([[
This is a long paragraph of text
it is the simplest while using it
with lua, however it will have the
same line breaks and spacing as
     you set in this block.
]])

local msg = [[this is a message that spans
multiple lines and will have the next lines
preserved as they were entered, so be careful
when using this]]

print(msg)


```



## M2000 Interpreter

A block of { } can be used for code and for text. Editor in M2000 environment can color code/string on the fly, analyzing the first line, where the block start.

```M2000 Interpreter

Module Checkit {
            a$={This is the first line
                  This is the second line
                  End bracket position declare indentation fortext (except for first line)
                  }
            Report a$  ' print all lines, without space at the left
}
CheckIt

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Print["Mathematica
   is an
interesing
 language,
 with its
  strings
   being
 multiline
 by\
 default
 when not
back\
s\\ashed!"];
```

{{out}}

```txt
Mathematica
   is an
interesing
 language,
 with its
  strings
   being
 multiline
 by default
 when not
backs\ashed!
```



## NMAKE.EXE


```nmake.exe
target0: dependent0
    command0 <<
temporary, discarded inline file
...
<<

target1: dependent1
    command1 <<
temporary, but preserved inline file
...
<<KEEP

target2: dependent2
    command2 <<filename2
named, but discarded inline file
...
<<NOKEEP

target3: dependent3
    command3 <<filename3
named and preserved inline file
...
<<KEEP
```



## Common Lisp


[https://github.com/e-user/cl-heredoc cl-heredoc] provide read-macro for heredoc:


```lisp
;; load cl-heredoc with QuickLisp
(ql:quickload 'cl-heredoc)

;; use #>xxx>yyyyyyyy!xxx as read-macro for heredoc
(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

;; example:
(format t "~A~%" #>eof1>Write whatever (you) "want",
  no matter how many lines or what characters until
the magic end sequence has been reached!eof1)

```

{{out}}

```txt

Write whatever (you) "want",
  no matter how many lines or what characters until
the magic end sequence has been reached!
NIL

```


Note: <code>NIL</code> is the return value of function <code>format</code> when first argument is <code>t</code>


## NewLISP


```NewLISP
; here-document.lsp
; oofoe 2012-01-19
; http://rosettacode.org/wiki/Here_document

(print (format [text]
    Blast it %s! I'm a %s,
    not a %s!
       --- %s
[/text] "James" "magician" "doctor" "L. McCoy"))

(exit)
```


{{out}}

```txt

    Blast it James! I'm a magician,
    not a doctor!
       --- L. McCoy

```



## Nim


There are no heredocs, but triple-quoted-strings can be used:

```nim
echo """Usage: thingy [OPTIONS]
     -h                        Display this usage message
     -H hostname               Hostname to connect to
"""
```



## OCaml



```ocaml

print_string {whatever|
  'hahah', `this`
  <is>
  \a\
  "Heredoc" ${example}
  in OCaml
|whatever}
;;

```

{{out}}

```txt

  'hahah', `this`
  <is>
  \a\
  "Heredoc" ${example}
  in OCaml

```



## OxygenBasic

Use the keyword '''quote''' followed by a keyword or group of symbols to mark the beginning and end of text

```txt


s=quote
""""
    She said "He said 'I said `There is a rumour
    going around.` ' "
""""

print s


```



## Perl

In Perl, there must not be a space between the "<<" and the token string. By default, the ending token must the entire end line (i.e. no surrounding spaces) for it to be recognized (but see exception below). Interpolation is allowed, like a double-quoted string:


```perl
$address = <<END;
1, High Street,
$town_name,
West Midlands.
WM4 5HD.
END
```


If the token string contains spaces, the token after the "<<" must be quoted; otherwise the double-quotes is implicit:

```perl
$pancake = <<"NO MORE INGREDIENTS";
egg
milk
flour
NO MORE INGREDIENTS
```


It is possible to make a here-document that behaves differently than a double-quoted string, by applying a different kind of quoting to the token. For example, if you use single quotes, then the here document will not support interpolation, like a normal single-quoted string:


```perl
$x = <<'FOO';
No
$interpolation
here
FOO
```


Alternately, you can use backticks to cause the here document to be executed 
and the result returned, just like a normal backtick operator:


```perl
$output = <<`BAR`;
ls /home
BAR
```


Note that in the above examples, that a semicolon was left 
after the here document's token string. 
This is because (unlike PHP) the here document does not start 
immediately at the "<<END" token -- it starts on the next line. 
The "<<END" is actually an expression, whose value will be substituted 
by the contents of the here document. 
The "<<END" must still live inside a valid statement on the line that it's used. 
To further illustrate this fact, we can use the "<<END" inside a complex, 
nested expression:


```perl
print(<<EOF . "lamb\n");
Mary had
  a little
EOF
```


Since Perl 5.26, the here document can be indented by putting a '~' before the token.  All spaces to the left of the ending token then become insignificant.  This avoids having a disconcerting exdent in the middle of your code.


```perl
sub flibbertigibbet {
    print <<~END;
        Mary had
          a little
        lamb
        END
}
```



## Perl 6

Heredocs in Perl 6 use the <code>:to</code> modifier to a quoting operator, 
such as <code>q</code> or <code>qq</code>.
The indentation of the end marker is removed from every line.


```perl6
my $color = 'green';
say qq :to 'END';
    some line
    color: $color
    another line
    END
```

{{out}}

```txt
some line
color: green
another line

```


Note that the quotes around the "END" are not magic --- the marker is just a regular string; it's the `q` or `qq` that decides whether or not the heredoc interpolates.

Multiple here docs may be stacked on top of each other.


```perl6
my $contrived_example = 'Dylan';
sub freewheelin() {
        print q :to 'QUOTE', '-- ', qq :to 'AUTHOR';
          I'll let you be in my dream,
            if I can be in yours.
        QUOTE
                Bob $contrived_example
                AUTHOR
}

freewheelin;
```


{{out}}

```txt

  I'll let you be in my dream,
    if I can be in yours.
-- Bob Dylan

```


Both q and qq are specialised forms of [http://design.perl6.org/S02.html#Q_forms Q] which comes with many adverbs. Here a heredoc that only interpolates @-sigils.


```perl6>my @a = <1 2 3 4
;
say Q :array :to 'EOH';
    123 \n '"`
        @a$bc
        @a[]
    EOH
```


{{out}}

```txt

123 \n '"`
    @a$bc
    1 2 3 4
```



## Phix

Phix does not have here-docs. In Phix normal double quoted strings are single line and require escaping. Strings can also be entered by using triple doublequotes or single backticks to include linebreaks and avoid any backslash interpretation. If the literal begins with a newline, it is discarded and any immediately following leading underscores specify a (maximum) trimming that should be applied to all subsequent lines. Interpolation is left to printf and friends. Both """`""" and `"""` are valid. Examples: 

```Phix
string ts1 = """
this
"string"\thing"""

string ts2 = """this
"string"\thing"""

string ts3 = """
_____________this
             "string"\thing"""

string ts4 = `
this
"string"\thing`

string ts5 = `this
"string"\thing`

string ts6 = `
_____________this
             "string"\thing`

string ts7 = "this\n\"string\"\\thing"

constant tests={ts1,ts2,ts3,ts4,ts5,ts6,ts7}
for i=1 to length(tests) do
    for j=1 to length(tests) do
        if tests[i]!=tests[j] then crash("error") end if
    end for
end for
printf(1,"""
____________Everything
            (all %d tests)
            works
             just
            file.""",length(tests))
printf(1,"""`""")
printf(1,`"""`)
```

{{out}}

```txt

Everything
(all 7 tests)
works
 just
file.`"""

```



## PHP

In PHP, the here document symbol is 3 less-than signs, not two: <code><<<</code>

There must not be a space between the "<<<" and the token string. 
The ending token must always be the entire end line (i.e. no surrounding spaces) 
for it to be recognised, except for a possible semicolon. 
Interpolation is allowed, like a double-quoted string:


```php
$address = <<<END
1, High Street,
$town_name,
West Midlands.
WM4 5HD.
END;
```


In PHP 5.3+, it is possible to make a here-document that does not interpolate 
(PHP calls this a "nowdoc"), by surrounding the token with single-quotes 
(like in Perl):


```php
$x = <<<'FOO'
No
$interpolation
here
FOO;
```



## PicoLisp

We can use the '[http://software-lab.de/doc/refH.html#here here]' function:


```PicoLisp
(out "file.txt"                        # Write to "file.txt"
   (prinl "### This is before the text ###")
   (here "TEXT-END")
   (prinl "### This is after the text ###") )
"There must be some way out of here", said the joker to the thief
"There's too much confusion, I can't get no relief"
TEXT-END

(in "file.txt" (echo))                 # Show "file.txt"
```


{{out}}

```txt

### This is before the text ###
"There must be some way out of here", said the joker to the thief
"There's too much confusion, I can't get no relief"
### This is after the text ###

```



## PowerShell


In PowerShell, here-docs are known as "Here-Strings".
The Key is the At symbol @.


```PowerShell

$XMLdata=@"
<?xml version="1.0" encoding="utf-8"?>
<unattend xmlns="urn:schemas-microsoft-com:unattend">
    <servicing>
        <package action="configure">
            <assemblyIdentity name="Microsoft-Windows-Foundation-Package" version="${strFullVersion}" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" language="" />
            <selection name="RemoteServerAdministrationTools" state="true" />
            <selection name="RemoteServerAdministrationTools-Roles-AD" state="true" />
            <selection name="RemoteServerAdministrationTools-Roles-AD-DS" state="true" />
            <selection name="RemoteServerAdministrationTools-Roles-AD-DS-SnapIns" state="true" />
            <selection name="RemoteServerAdministrationTools-Features-StorageManager" state="true" />
            <selection name="RemoteServerAdministrationTools-Features-Wsrm" state="true" />
        </package>
    </servicing>
    <cpi:offlineImage cpi:source="wim:d:/2008r2wim/install.wim#Windows Server 2008 R2 SERVERSTANDARD" xmlns:cpi="urn:schemas-microsoft-com:cpi" />
</unattend>
"@

```



## Python

Python does not have here-docs. 
It does however have [http://docs.python.org/py3k/tutorial/introduction.html#strings triple-quoted strings] which can be used similarly.


```python
print("""\
Usage: thingy [OPTIONS]
     -h                        Display this usage message
     -H hostname               Hostname to connect to
""")
```



## Racket


Racket has a raw built-in here-document syntax, with the usual problems
that this implies (breaks code indentation, no "interpolation"):


```Racket

#lang racket/base

(displayln #<<EOF
  Blah blah blah
    with indentation intact
    and "free" \punctuations\
EOF
)

```

{{out}}

```txt
  Blah blah blah
    with indentation intact
    and "free" \punctuations\

```

In addition, Racket has "@-forms", which are a syntax for free text ([http://barzilay.org/misc/scribble-reader.pdf described here])
that works well with code:


```Racket

#lang at-exp racket/base

(require scribble/text)

(define excited "!!!")
(define (shout . text) @list{>>> @text <<<})

(output
 @list{Blah blah blah
         with indentation intact
           but respecting the indentation of
           the whole code
         and "free" \punctuations\
         and even string interpolation-like @excited
           but really @shout{any code}

       })

(output @list|<<{And customizable delimiters
                 so @foo{} is just plain text}>>|)

```

{{out}}

```txt
Blah blah blah
  with indentation intact
    but respecting the indentation of
    the whole code
  and "free" \punctuations\
  and even string interpolation-like !!!
    but really >>> any code <<<
And customizable delimiters
so @foo{} is just plain text
```



## Raven

As a list:

```Raven
'Buffy the Vampire Slayer' as sender
'Spike' as recipient

[
"Dear %(recipient)s,
"
"I wish you to leave Sunnydale and never return.
"
"Not Quite Love,
"%(sender)s
] "\n" join print

```

Using group to place the data on the stack:

```Raven
'Buffy the Vampire Slayer' as sender
'Spike' as recipient

group
        "Dear %(recipient)s,
        "
        "I wish you to leave Sunnydale and never return.
        "
        "Not Quite Love,
        %(sender)s\n"
list "\n" join print

```

{{out}}

```txt
Dear Spike,

I wish you to leave Sunnydale and never return.

Not Quite Love,
Buffy the Vampire Slayer
```



## REXX


```rexx
/*REXX program demonstrates a method to use  "here"  documents in REXX. */
parse arg doc .                        /*"here" name is case sensitive. */

     do j=1  for sourceline()
     if sourceline(j)\=='◄◄'doc  then iterate
           do !=j+1  to sourceline()  while sourceline(!)\=='◄◄.'
           say sourceline(!)
           end   /*!*/
     exit                              /*stick a fork in it, we're done.*/
     end         /*j*/

say doc '"here" document not found.'
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────start of "here" docs──────────────────
◄◄rs-232
RS─232 Signals and Pinouts  ┌─────────────────────────────────────────────────┐
                            │13  12  11  10  9   8   7   6   5   4   3   2   1│
──►  Interface between data └┐ 25  24  23  22  21  20  19  18  17  16  15  14┌┘
terminal equipment (DTE/male)└───────────────────────────────────────────────┘
and data communication equipment
[DCE/female]  employing  serial   ┌───────────────────────────────────────────┐
binary  data  interchange.        │ 12◄─secondary carrier detect   [SCD]  DCE │
┌─────────────────────────────────┤ 13◄─secondary clear to send    [SCS]  DCE │
│ 1◄─protective ground  [PG, GND] │ 14◄─secondary transmitted data [STD]  DTE │
│ 2◄─transmitted data   [TD]  DTE │ 15◄─transmit clock             [TC]   DCE │
│ 3◄─received data      [RD]  DCE │ 16◄─secondary received data    [SRD]  DCE │
│ 4◄─request to send    [RTS] DTE │ 17◄─receiver clock             [RC]   DCE │
│ 5◄─clear to send      [CTS] DCE │ 18◄─unassigned                            │
│ 6◄─data set ready     [DSR] DCE │ 19◄─secondary request to send  [SRS]  DTE │
│ 7◄─signal ground      [SG]      │ 20◄─data terminal ready        [DTR]  DTE │
│      (common return)            │ 21◄─signal quality detector    [SQD]  DCE │
│ 8◄─carrier detect     [CD]  DCE │ 22◄─ring indicator             [RI]   DCE │
│ 9◄─positive voltage   [-]       │ 23◄─data rate select       [DRS]  DCE/DTE │
│10◄─negative voltage   [-]       │ 24◄─external clock             [XTC]  DTE │
│11◄─unassigned                   │ 25◄─unassigned                            │
└─────────────────────────────────┴───────────────────────────────────────────┘
◄◄.
◄◄can
        ┌──────┐
        │      │
        │      ├┐
        │      ├┘
        │      │
        │      │
        │      │
        │      │
        │      │                                         ┌─────┐
        └──┬┬──┘                                         │┌───┐│
           ││                                            ├┤   ├┤
           ││    ┌───────────────┐                       ││   ││
           ││   ┌┴──────────────┬┘                       └┤   ├┘
           │└───┤               │                         └───┘
           └────┤            ┌──┘
                │            │
                └──┐         │
                   │         │
                   │         │
                   │         │
                   └─────────┘
◄◄.
────────────────────────────────────end of "here" docs──────────────────*/
```

{{out}} when using the input of: <tt> rs-232 </tt>

```txt

RS─232 Signals and Pinouts  ┌─────────────────────────────────────────────────┐
                            │13  12  11  10  9   8   7   6   5   4   3   2   1│
──►  Interface between data └┐ 25  24  23  22  21  20  19  18  17  16  15  14┌┘
terminal equipment (DTE/male)└───────────────────────────────────────────────┘
and data communication equipment
[DCE/female]  employing  serial   ┌───────────────────────────────────────────┐
binary  data  interchange.        │ 12◄─secondary carrier detect   [SCD]  DCE │
┌─────────────────────────────────┤ 13◄─secondary clear to send    [SCS]  DCE │
│ 1◄─protective ground  [PG, GND] │ 14◄─secondary transmitted data [STD]  DTE │
│ 2◄─transmitted data   [TD]  DTE │ 15◄─transmit clock             [TC]   DCE │
│ 3◄─received data      [RD]  DCE │ 16◄─secondary received data    [SRD]  DCE │
│ 4◄─request to send    [RTS] DTE │ 17◄─receiver clock             [RC]   DCE │
│ 5◄─clear to send      [CTS] DCE │ 18◄─unassigned                            │
│ 6◄─data set ready     [DSR] DCE │ 19◄─secondary request to send  [SRS]  DTE │
│ 7◄─signal ground      [SG]      │ 20◄─data terminal ready        [DTR]  DTE │
│      (common return)            │ 21◄─signal quality detector    [SQD]  DCE │
│ 8◄─carrier detect     [CD]  DCE │ 22◄─ring indicator             [RI]   DCE │
│ 9◄─positive voltage   [-]       │ 23◄─data rate select       [DRS]  DCE/DTE │
│10◄─negative voltage   [-]       │ 24◄─external clock             [XTC]  DTE │
│11◄─unassigned                   │ 25◄─unassigned                            │
└─────────────────────────────────┴───────────────────────────────────────────┘

```

{{out}} when using the input of: <tt> can </tt>

```txt

        ┌──────┐
        │      │
        │      ├┐
        │      ├┘
        │      │
        │      │
        │      │
        │      │
        │      │                                         ┌─────┐
        └──┬┬──┘                                         │┌───┐│
           ││                                            ├┤   ├┤
           ││    ┌───────────────┐                       ││   ││
           ││   ┌┴──────────────┬┘                       └┤   ├┘
           │└───┤               │                         └───┘
           └────┤            ┌──┘
                │            │
                └──┐         │
                   │         │
                   │         │
                   │         │
                   └─────────┘

```



## Ring


```ring

text ="
<<'FOO'
Now
   is
     the 
       time
            for
               all
                  good mem
to come to the aid of their country."
see text + nl

```

Output:

```txt

<<'FOO'
Now
   is
     the
       time
            for
               all
                  good mem
to come to the aid of their country.

```



## Ruby

In Ruby, there must not be a space between the "<<" and the token string. 
The ending token must always be the entire end line (i.e. no surrounding spaces) for it to be recognised, unless you use "<<-" instead of "<<", in which case indentation before the ending token is allowed. 
Interpolation is allowed, like a double-quoted string:


```ruby
address = <<END
1, High Street,
#{town_name},
West Midlands.
WM4 5HD.
END
```


If the token string contains spaces, the token after the "<<" must be quoted; otherwise the double-quotes is implicit:


```ruby
pancake = <<"NO MORE INGREDIENTS"
egg
milk
flour
NO MORE INGREDIENTS
```


It is possible to make a here-document that behaves differently than a double-quoted string, by applying a different kind of quoting to the token. 
For example, if you use single quotes, then the here document will not support interpolation, like a normal single-quoted string:


```ruby
x = <<'FOO'
No
#{interpolation}
here
FOO
```


Alternately, you can use backticks to cause the here document to be executed 
and the result returned, just like a normal backtick operator:


```ruby
output = <<`BAR`
ls /home
BAR
```


The here document does not start immediately at the "<<END" token -- it starts on the next line. 
The "<<END" is actually an expression, whose value will be substituted by the contents of the here document. 
The "<<END" must still live inside a valid statement on the line that it's used. 
To further illustrate this fact, we can use the "<<END" inside a complex, nested expression:


```ruby
puts <<EOF + "lamb"
Mary had
  a little
EOF
```



## Run BASIC


```runbasic
text$ ="
<<'FOO'
Now
   is
     the 
       time
            for
               all
                  good mem
to come to the aid of their country."
print text$
```

{{out}}

```txt
<<'FOO'
Now
   is
     the 
       time
            for
               all
                  good mem
to come to the aid of their country.
```



## Rust


Similar to [[#C++|C++]], Rust offers raw strings:


```rust
let x = r#"
    This is a "raw string literal," roughly equivalent to a heredoc.   
  "#;
```



## Scala



### All versions

Scala multiline literal are called raw strings. 
Triple quotes (""") marks the beginning and end. 
Specially handy when using escape sequences in e.g. regular expressions.
{{libheader|Scala}}
```Scala
object temp {
val MemoriesOfHolland=
  """Thinking of Holland
    |I see broad rivers
    |slowly chuntering
    |through endless lowlands,
    |rows of implausibly
    |airy poplars
    |standing like tall plumes
    |against the horizon;
    |and sunk in the unbounded
    |vastness of space
    |homesteads and boweries
    |dotted across the land,
    |copses, villages,
    |couchant towers,
    |churches and elm-trees,
    |bound in one great unity.
    |There the sky hangs low,
    |and steadily the sun
    |is smothered in a greyly
    |iridescent smirr,
    |and in every province
    |the voice of water
    |with its lapping disasters
    |is feared and hearkened.""".stripMargin
}
```

All control codes are transparent e.g. new lines. 
In order for a neat code each lines has as prefix spaces and a | symbol 
which will be removed by the stripMargin function.


## Sidef

There must not be a space between the "<<" and the token string. 
When the token string is double-quoted ("") or not quoted, 
the content will be interpolated like a double-quoted string:

```ruby
var text = <<"EOF";
a = #{1+2}
b = #{3+4}
EOF
```


If single quotes are used, then the here document will not support interpolation, like a normal single-quoted string:

```ruby
var x = <<'FOO';
No
#{interpolation}
here
FOO
```

The here document does not start immediately at the "<<END" token -- it starts on the next line. The "<<END" is actually an expression, whose value will be substituted by the contents of the here document. 
To further illustrate this fact, we can use the "<<END" inside a complex, nested expression:

```ruby
say (<<EOF + "lamb");
Mary had
  a little
EOF
```

which is equivalent with:

```ruby
say (<<EOF
Mary had
  a little
EOF
+ "lamb");
```



## SequenceL


```sequencel
main := 
"In SequenceL
strings are
multiline
by default.
'All' non-\"
characters are
valid for inclusion
in a string.";
```


{{out}}

```txt

"In SequenceL
strings are
multiline
by default.
'All' non-"
characters are
valid for inclusion
in a string."

```



## SQL PL

{{works with|Db2 LUW}}
With SQL only from command line. The single quote string is opened along the three lines:

```bash

db2 "select 'This is the first line.
This is the second line.
This is the third line.' from sysibm.sysdummy1"

```

Output:

```txt

1                                                                       
------------------------------------------------------------------------
This is the first line.
This is the second line.
This is the third line.

  1 record(s) selected.

```

With SQL only from script with concat function (one row):

```sql pl

select 'This is the first line.' || chr(10) ||
'This is the second line.' || chr(10) ||
'This is the third line.' from sysibm.sysdummy1;

```

Output:

```txt

1
------------------------------------------------------------------------
This is the first line.
This is the second line.
This is the third line.

  1 record(s) selected.

```

With SQL only from script with union operator (three rows):

```sql pl

select 'This is the first line.' from sysibm.sysdummy1
union
select 'This is the second line.' from sysibm.sysdummy1
union
select 'This is the third line.' from sysibm.sysdummy1;

```

Output:

```txt

1
------------------------
This is the first line.
This is the second line.
This is the third line.

  3 record(s) selected.

```

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

SET serveroutput ON
CALL DBMS_OUTPUT.PUT_LINE('This is the first line.' || chr(10) ||
'This is the second line.' || chr(10) ||
'This is the third line.');

```

Output:

```txt


  Return Status = 0

This is the first line.
This is the second line.
This is the third line.

```



## Tcl


```tcl
set hereDocExample {
In Tcl, the {curly brace} notation is strictly a here-document style notation
as it permits arbitrary content inside it *except* for an unbalanced brace.
That is typically not a problem as seen in reality, as almost all content that
might be placed in a here-doc is either brace-free or balanced. 
The content of the braces is not interpreted at all; 
no substitutions are performed on it.

The sole exception is that there is limited processing of backslashes; 
a single backslash at the end of a line causes the end-of-line 
plus all whitespace at the start of the next line 
to be compressed to a single space.
}
```


If substitution is desired within the document, it should either be written 
inside <tt>"</tt>double quotes<tt>"</tt> (instead of <tt>{</tt>braces<tt>}</tt>) ''or'' it should be passed through the <code>subst</code> command, 
which performs another round of substitutions.


## TXR

TXR was originally conceived out of the need to have "there documents": 
parse a document and extract variables, but in a style similar to generation 
of here documents. 
Here doc output was added later.

We use @(maybe)/@(or)/@(end) to set up some default values for variables 
which are overridden from the command line.
Unification fails for an overridden variable, which is why we have 
to separate out the bind directives into the branches of a maybe.

By passing the script to txr using -f we can pass additional command arguments 
to the resulting script which are interpreted by txr.


```txr
#!/usr/bin/txr -f
@(maybe)
@(bind USER "Unknown User")
@(or)
@(bind MB "???")
@(end)
@(output)
Dear @USER

Your are over your disk quota by @MB megabytes.

The Computer
@(end)
```


Test runs


```txt

$ ./quota.txr -DMB=20
Dear Unknown User

Your are over your disk quota by 20 megabytes.

The Computer
$ ./quota.txr -DUSER=Bob
Dear Bob

Your are over your disk quota by ??? megabytes.

The Computer
$ ./quota.txr -DUSER=Bob -DMB=15
Dear Bob

Your are over your disk quota by 15 megabytes.

The Computer

```


Unbound variables throw exceptions:


```txt

$ txr -c '@(output)
@FOO
@(end)'
txr: unhandled exception of type query_error:
txr: (cmdline:2) bad substitution: FOO

```



## UNIX Shell

In the shell, here document act as input to the command, 
rather than providing a string definition.

{{works with|Bourne Shell}}


```bash
#!/bin/sh
cat << ANARBITRARYTOKEN
The river was deep but I swam it, Janet.
The future is ours so let's plan it, Janet.
So please don't tell me to can it, Janet.
I've one thing to say and that's ...
Dammit. Janet, I love you.
ANARBITRARYTOKEN
```



```bash
cat << EOF
Here documents do parameter and command substitution:
 * Your HOME is $HOME
 * 2 + 2 is `expr 2 + 2`
 * Backslash quotes a literal \$, \` or \\
EOF
```



```bash
if true; then
    cat <<- EOF
    The <<- variant deletes any tabs from start of each line.
    EOF
fi
```



```bash
cat << 'TOKEN'
If TOKEN has any quoted characters (like 'TOKEN', "TOKEN" or \TOKEN),
then all $ ` \ in the here document are literal characters.

$PATH \$PATH `shutdown now`
TOKEN
```


```bash
echo '
In any unix shell, you specify a text block and can use all whitespace like
  (spaces) &    (tab) in it, by using single quotes. 
As mentioned above, a Unix "here document" is a type of redirection.
In a literal Bash here document, the starting token must be
quoted, but the end token must not, so they are not the same, which does not
conform to the task definition.
'

```


=
## C Shell
=

```csh
#!/bin/csh -f
cat << ANARBITRARYTOKEN
 * Your HOME is $HOME
 * 2 + 2 is `@ n = 2 + 2; echo \$n`
ANARBITRARYTOKEN

cat << 'ANARBITRARYTOKEN'
$PATH \$PATH `shutdown now`
'ANARBITRARYTOKEN'
```



## Ursala


```Ursala
hd = 

-[
The text enclosed within the so called dash-brackets shown above
and below will be interpreted as a list of character strings. It
can contain anything except uninterpreted dash-brackets, and can
be used in any declaration or expression. The dash-brackets don't
have to be on a line by themselves.
]-


example =

-[Some additional facilities allow here-documents to be nested and
combined. Writing something like -[ hd ]- within a nested pair of
dash-brackets will cause the text declared above (having the
identifer hd) to be inserted at that point. The enclosed item can
be any expression that evaluates to a list of character strings.
We could therefore "escape" a literal dash-bracket within a
here-document by writing -[ <'-['> ]-. Dash-brackets can be nested
to any depth, alternating between literal text and compiled code
on each level.]-

template "x" =

-[A further use of this notation involves defining a text-valued
function. The output of this function will be this text, with
the argument inserted here -["x"]- and again here -["x"]-. The
argument type should be a list of character strings.]-

formletter ("x","y") =

-[Other calling conventions are possible. The left argument
comes out here -["x"]- and the right one here -["y"]-.]-

designpattern =

-[A point-free style of function declaration is also supported.
The argument comes out here -[. ~& ]-, after being fed through
the function appearing within the nested dash-brackets (in this
case the identity function). This usage is indicated by a period
after the left inner dash-bracket. Nesting is also allowed in
point free dash-bracket function specifications.]-

abstractionastronaut =

-[Higher order functions to any level are specified by piling on
the periods like this -[.. ~&]-. This one is a second order function
that needs to be applied to another function in order to get a
first order function such as the previous three examples.]-
```


{{omit from|8086 Assembly}}
{{omit from|80386 Assembly}}
{{omit from|BASIC}}
{{omit from|GUISS}}
{{omit from|Icon}}{{omit from|Unicon}}
{{omit from|Java|Unsupported}}
{{omit from|JavaScript}}
{{omit from|LabVIEW|Not a text-based language}}
{{omit from|Locomotive Basic|Does not support here documents}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|NetRexx}}
{{omit from|Openscad}}
{{omit from|Z80 Assembly}}
{{omit from|ZX Spectrum Basic|Does not support here documents}}



## VBScript

There is no such thing in VBScript but we need it, too.
The following is a workaround tool.

It will prompt you to select a Txt-based file and do its best to create VBScript code that will recreate that Txt-based file!

```VBScript

'Purpose: Converts TXT files into VBS code with a function that returns a text string with the contents of the TXT file
'         The TXT file can even be another VBS file.

'History:
'   1.0 8may2009    Initial release
'
'
Const ForReading = 1
Const ForWriting = 2
Const ForAppending = 8
Const TristateUseDefault = -2

set WshShell = CreateObject("WSCript.shell")

'File browse dialog box
Set objDialog = CreateObject("UserAccounts.CommonDialog")
objDialog.Filter = "All Files|*.*"
objDialog.InitialDir = WshShell.CurrentDirectory
intResult = objDialog.ShowOpen
 
If intResult = 0 Then
    WshShell.Popup "No file selected.", 2, " ", 64
    Wscript.Quit
Else
    strFileNameIN = objDialog.FileName
End If

strFileNameOUT= strFileNameIN & "_CONVERTED.Vbs"

'Check if strFileNameOUT exists already
Set objFSO = CreateObject("Scripting.FileSystemObject")
If objFSO.FileExists(strFileNameOUT) then  'does the file EXIST?
'   WScript.Echo "found"
    OVRWT=MSGBOX(strFileNameOUT & " exists already"&vbCRLF&"Overwrite?",vbYesNoCancel,"Overwrite?")
    if OVRWT = 6 then
        'proceed
        objFSO.DeleteFile(strFileNameOUT)
    else
        WshShell.Popup "Exiting as requested.", 1, " ", 64
        Wscript.Quit
    End If 
Else
'   WScript.Echo "not found" 'strFileNameOUT does NOT exists already
    
END if

strBaseName=objFSO.GetBaseName(strFileNameIN)


'open strFileNameANSI file, and put entire file into a variable ****SIZE LIMIT ??*****
Set objFile = objFSO.OpenTextFile(strFileNameIN, ForReading)
strText = objFile.ReadAll
objFile.Close

'Start converting

'Convert " to ""
strOldText = Chr(34)
strNewText = Chr(34)&Chr(34)
strText = Replace(strText, strOldText, strNewText)

'Add objTXTFile.writeline ("
strOldText = VBCRLF
strNewText = """)  &vbCrLf"&VBCRLF&"    strText=strText& ("""
strText = Replace(strText, strOldText, strNewText)
'Converting done

strFileName=objFSO.GetFileName(strFileNameIN)

'Write to file
Set objFile = objFSO.OpenTextFile(strFileNameOUT, ForAppending, True)
objFile.WriteLine "'this Function will return a string containing the contents of the file called "&strFileName
objFile.WriteLine "msgbox "&strBaseName &"()"
objFile.WriteLine vbCrLf
objFile.WriteLine "Function "&strBaseName&"()"
objFile.WriteLine " 'returns a string containing the contents of the file called "&strFileName
objFile.WriteLine " Dim strText"
objFile.WriteLine " strText= ("""&strText&""") &vbCrLf"
objFile.WriteLine " "&strBaseName&"=strText"
objFile.WriteLine "End Function"
objFile.Close

WshShell.Popup "created " & strFileNameOUT, 3, "Completed", 64

```



## XPL0


```XPL0
code Text=12;
Text(0, "   ^"Heredocs^" are pretty much automatic. Multiple lines and
whitespace, such as indentations, are output exactly as written. Quote
marks (^") and any carets (^^) within the string must be escaped.")
```



## XSLT


Being a dialect of XML, XSLT inherits [http://www.w3.org/TR/REC-xml/#sec-cdata-sect CDATA sections]. Not quite heredocs, these are more like raw triple quotes in Python (<code>r"""…"""</code>) or Scala (<code>"""…"""</code>) in that anything except the end delimiter is treated literally.


```xml
<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text"/>

<xsl:template match="/">
<![CDATA[

This text is in a CDATA section. In here, it's okay to include <, >, &, ", and '
without any special treatment.

The section is terminated by a three-character sequence consisting of two right
brackets ("]]") followed by a greater-than (">"). If this sequence appears in
your text, a workaround is to drop out of the CDATA section, output part of the
terminator, then start a new CDATA section and output the rest. Let's do this
now:

    ]]>]]<![CDATA[>

Newlines and spacing are retained as well, as long as they're evaluated in a
context that bothers preserving them. Whether or not the spaces before and after
the CDATA section are also preserved may be application-dependent.

]]>
</xsl:template>
</xsl:stylesheet>
```


Output from xsltproc (input is ignored):


```txt




This text is in a CDATA section. In here, it's okay to include <, >, &, ", and '
without any special treatment.

The section is terminated by a three-character sequence consisting of two right
brackets ("]]") followed by a greater-than (">"). If this sequence appears in
your text, a workaround is to drop out of the CDATA section, output part of the
terminator, then start a new CDATA section and output the rest. Let's do this
now:

    ]]>

Newlines and spacing are retained as well, as long as they're evaluated in a
context that bothers preserving them. Whether or not the spaces before and after
the CDATA section are also preserved may be application-dependent.



```



## zkl


```zkl
x:=
#<<<
"#<<< starts a block of lines that are concatenated verbatim 
and fed into the parser as one line. #<<< ends the block.
Both #<<< tokens must start the line that is otherwise ignored

Note that is isn't a string, but arbitrary source " + 1 + 23;
#<<<
x.println();
```

{{out}}

```txt

#<<< starts a block of lines that are concatenated verbatim 
and fed into the parser as one line. #<<< ends the block.
Both #<<< tokens must start the line that is otherwise ignored

Note that is isn't a string, but arbitrary source 123

```


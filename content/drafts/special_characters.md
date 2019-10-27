+++
title = "Special characters"
description = ""
date = 2019-09-12T19:34:07Z
aliases = []
[extra]
id = 2227
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}

Special characters are symbols (single characters or sequences of characters) that have a "special" built-in meaning in the language and typically cannot be used in identifiers. 

Escape sequences are methods that the language uses to remove the special meaning from the symbol, enabling it to be used as a normal character, or sequence of characters when this can be done.


;Task:
List the special characters and show escape sequences in the language.

See also: [[Quotes]]





## 360 Assembly

<!-- Special characters -->

'''Basic assembler'''

Assembler 360 has the following special characters:
* <code>*</code>  indicates a comment (if placed in column 1)
* <code>*</code>  indicates the location counter - ex: <code>*+72</code>
* <code>=</code>  literal reference - ex: <code>=A(BUFFER)</code> , <code>=F'1'</code>
* <code>'</code>  attribute operator - ex: <code>L'BUFFER</code>
* <code>'</code>  string delimiter - ex: <code>C'HELLO'</code> , <code>C'I DON&apos;&apos;T'</code>

'''Macro assembler'''

Assembler 360 has for writting macros the following special characters:
* <code>.*</code> macro comment
* <code>&</code>  macro symbol specifier - ex: <code>&VAR</code>
* <code>=</code>  macro keyword - ex: <code>&REF=A</code>
* <code>.</code>  macro concatenation - ex: <code>&VAR.A</code>
* <code>.</code>  macro sequence symbol - ex: <code>.SEQ</code>


## ActionScript


* , function argument separator
* ; statement separator
* // comment prefix


###  Enclosures 


* " "   literal string enclosures
* ( )   function argument enclosures
* /* */ comment block enclosures


## Ada

Ada uses the following characters:
The alphabet A-Z
The digits 0-9
The following characters "#&'()*+,-./:;<=>_|
The space character
The following compound characters => .. ** := /= >= <= << >> <>

Identifiers consist of a letter followed by any number of letters or Numbers, which may be separated by a single underscore.

There is no escape sequences in character literals. Any character supported by the source encoding is allowed. The only escape sequence of string literals is "" (doubled double quotation marks) which denotes ". When characters need to be specified by their code positions (in Unicode), this is made using the 'Val attribute:

```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test is
begin
   Put ("Quote """ & ''' & """" & Character'Val (10));
end Test;
```

{{out}}

```txt

Quote "'"


```

Note that character and string literals serve all character and string types. For example with Wide_Wide characters (32-bit) and strings:

```Ada
with Ada.Wide_Wide_Text_IO;  use Ada.Wide_Wide_Text_IO;

procedure Test is
begin
   Put ("Unicode """ & ''' & """" & Wide_Wide_Character'Val (10));
end Test;
```



## ALGOL 68

ALGOL 68 has several built-in character constants.  The following characters
are (respectively) the representations of TRUE and FALSE, the blank character "<u>.</u>",
the character displayed when a number cannot being printed in the width provided.  And
the null character indicating the end of characters in a BYTES array.

```algol68
printf(($"flip:"g"!"l$,flip));
printf(($"flop:"g"!"l$,flop));
printf(($"blank:"g"!"l$,blank));
printf(($"error char:"g"!"l$,error char));
printf(($"null character:"g"!"l$,null character))
```

{{out}}

```txt

flip:T!
flop:F!
blank: !
error char:*!
null character:

```

To handle the output movement to (and input movement from) a device 
ALGOL 68 has the following four positioning procedures:

```algol68
print(("new page:",new page));
print(("new line:",new line));
print(("space:",space));
print(("backspace:",backspace))
```
These procedures may not all be supported on a particular device.

If a particular device (CHANNEL) is '''set possible''', then there 
are three built-in procedures that allow movement about this device.
* set char number - set the position in the current line.
* reset - move to the first character of the first line of the first page.  For example a home or tape rewind.
* set - allows the movement to selected page, line and character. 

ALGOL 68 pre-dates the current ASCII standard, and hence supports many 
non ASCII characters.  Moreover ALGOL 68 had to work on 6-bits per byte 
hardware, hence it was necessary to be able to write the same ALGOL 68 code
in strictly upper-case.  Here are the special characters together with their 
upper-case alternatives (referred to as "worthy characters").
{|border="1" style="border-collapse: collapse;"
!bgcolor=#C0FFEE|Character
!bgcolor=#C0FFEE|ASCII
!bgcolor=#C0FFEE|Worthy
!bgcolor=#C0FFEE|'''bold'''
|-
| &cent; || # || CO || '''co'''
|-
| &ge; || >= || GE || '''ge'''
|-
| &le; || <= || LE || '''le'''
|-
| &ne; || /= or ~= || NE || '''ne'''
|-
| &not; || ~ || NOT || '''not'''
|-
| &or; || \/ || OR || '''or'''
|-
| &and; || /\ or &  || AND || '''and'''
|-
| &divide; || % || OVER || '''over'''
|-
| &times; || * || TIMES || '''times'''
|-
| &uarr; || ** || UP || '''up'''
|-
| &darr; || || DOWN || '''down'''
|-
| &rarr; || -> || OF || '''of'''
|-
| &perp; or &times;+ || *+ || I || '''i'''
|-
| ⏨ || \ || E || '''e'''
|-
| ○ || || NIL || '''nil'''
|-
| □ || || ELEM || '''elem'''
|-
| &lfloor; || || LWB || '''lwb'''
|-
| &lceil; || || UPB || '''upb'''
|-
| ⎩ || || LWS || '''lws'''
|-
| ⎧ || || UPS || '''ups'''
|}
Most of these characters made their way into European standard 
characters sets (eg ALCOR and GOST). Ironically the &cent; character 
was dropped from later versions of America's own ASCII character set.

The character "⏨" is one ALGOL 68 byte (not two bytes).


## ALGOL W


Algol W has the following special characters:

* " opening and closing string quote
* # bit string constant prefix
* ' indicates the exponent in a real or imaginary constant
* | separates the start and length in a substring
* , separates parameters and other list elements
* ; separates statements
* : separates a label from the next label or the labelled statement
* . decimal point
* ¬ logical/bit string not operator
* + arithmetic operator
* - arithmetic operator
* * arithmetic operator
* / arithmetic operator
* < comparison operator
* = comparison operator
* > comparison operator
* % comment delimiter - the comment can be terminated by either % or ;


Parenthesis are used to delimit parameters, subscripts, substrings etc.
* (
* )


The following combinations of the above also form symbols:

* ** raise-to-the-power operator
* <= comparison operator
* ¬= comparison operator
* >= comparison operator
* :: separates the lower-bound from the upper-bound in array declarations
* := assignment
* // alternative substring start/length separator


Certain combinations of letters form reserved words in Algol W and cannot be used as identifiers. Also "not", "not =" and "comment" can be used as alternatives for ¬ and ¬= and % (however only ; can terminate a comment started with "comment").



## AutoHotkey

The escape character defaults to accent/backtick (`).
* `, = , (literal comma). Note: Commas that appear within the last parameter of a command do not need to be escaped because the program knows to treat them literally. The same is true for all parameters of MsgBox because it has smart comma handling. 
* `% = % (literal percent) 
* `` = ` (literal accent; i.e. two consecutive escape characters result in a single literal character) 
* `; = ; (literal semicolon). Note: This is necessary only if a semicolon has a space or tab to its left. If it does not, it will be recognized correctly without being escaped. 
* `n = newline (linefeed/LF) 
* `r = carriage return (CR) 
* `b = backspace 
* `t = tab (the more typical horizontal variety) 
* `v = vertical tab -- corresponds to Ascii value 11. It can also be manifest in some applications by typing Control+K. 
* `a = alert (bell) -- corresponds to Ascii value 7. It can also be manifest in some applications by typing Control+G. 
* `f = formfeed -- corresponds to Ascii value 12. It can also be manifest in some applications by typing Control+L. 
* Send = When the Send command or Hotstrings are used in their default (non-raw) mode, characters such as {}^!+# have special meaning. Therefore, to use them literally in these cases, enclose them in braces. For example: Send {^}{!}{{} 
* "" = Within an expression, two consecutive quotes enclosed inside a literal string resolve to a single literal quote. For example: Var := "The color ""red"" was found."


## AWK


AWK uses the following special characters:
* ! logical NOT
* ; statement separator
* # comment marker
* $ field reference operator and regular expression anchor
* % modulus, output format specifier prefix
* * multiplication operator and regular expression repetition operator
* + addition operator and regular expression operator
* - subtraction operator, regular expression range operator
* , separates items in a list, range pattern delimiter
* . decimal point and regular expression operator
* / division operator and regular expression enclosure symbol
* : ternary operation component
* ; statement and rule separator
* = assignment operator
* < comparative less than operator, infeed operator
* > comparative greater than operator, outfeed operator
* ? regular expression match no more than once, ternary operation component
* \  escape sequence, line continuation, suppression of interpolation (in code and regular expressions), literal character insertion
* ^ modulus, regular expression anchor and compliment box indicator
* | regular expression alternation operator, pipefeed operator
* ~ regular expression binding operator


### Digraphs


* == equality comparative operator
* != inequality comparative operator
* <= comparative less than or equal to
* >= comparative greater than or equal to
* >> appendfeed operator
* && logical AND
* || logical OR
* ++ increment nudge operator
* -- decrement nudge operator
* += addition compound assignment operator
* -= subtraction compound assignment operator
* *= multiplication compound assignment operator
* /= division compound assignment operator
* ^= exponent compound assignment operator
* %= modulus compound assignment operator
* !~ regular expression non containment operator


### Enclosures

* " " literal string enclosures
* / / regular expression enclosures
* { } body of code, code block, action enclosure, or body of if/for/while block
* ( ) conditional constructs in for/while loops; arguments to a function, grouping expression components, regular expression subexpression enclosures, overriding precedence
* [ ] array element enclosures

In addition, regular expressions and (s)printf have their own "little languages".
Note that the ampersand, snail, underscore, backtick and apostrophe symbols have no special meanings in isolation.


## BASIC



###  Assignment operator symbols 


* = assignment operator 


###  Arithmetic operator symbols 


* + addition
* - subtraction
* * multiplication
* / division
* \ integer division


###  Data type indicators 


* % suffix sigil following integer variable names
* $ suffix sigil following string variable names


###  Comparative operator symbols 


* = equality
* < less than
* > greater than
* <= less than or equal to
* >= greater than or equal to
* <> inequality


###  Enclosures 


* " " used as enclosures for strings
* ( ) function argument enclosures, array element reference, and used to dictate mathematical precedence


###  Output separators 


* ; move cursor to next column instead of newline and separates redirection stream from data
* , move cursor to next tabstop instead of newline and alternative to semicolon for separation of stream from data


###  Statement and argument separators 


* : separates multiple statements on a line
* , separates multiple arguments to functions


###  Redirection operator 


* # prefixes a stream number for input or output redirection


## Batch File

Check [http://www.robvanderwoude.com/escapechars.php HERE] for more details.



Basically, these are the special characters in Batch Files:

* <code>%</code> (Escape Sequence: <code>%%</code>) - Used for using variables.
* <code>&</code> (Escape Sequence: <code>^&</code>) - Used for executing multiple commands in one line.
* <code>(</code> and <code>)</code> (Escape Sequence: <code>^(</code> and <code>^)</code>, respectively) - grouping symbols, works similar to the curly brackets in Java, C, etc.
* <code>></code> (Escape Sequence: <code>^></code>) - The "redirection" symbol, used for redirecting the output of a command to a file.
* <code><</code> (Escape Sequence: <code>^<</code>) - Used for sending the content of a file into a command.
* <code>|</code> (Escape Sequence: <code>^|</code>) - The "pipe" symbol, Used for sending the output of a command into another command.
* <code>^</code> (Escape Sequence: <code>^^</code>) - Escapes the next character. (quite weird...)
* <code>!</code> (Escape Sequence: <code>^^!</code>) - Used for using delayed variables. (Required iff delayed variable expansion is enabled)


## BBC BASIC

These are the principal special characters, in addition to the regular symbols used in BASIC for arithmetic operations, comparisons, delimiters etc.:

```txt

?     A unary or dyadic operator giving 8 bit indirection. 
!     A unary or dyadic operator giving 32 bit indirection. 
#     As a prefix indicates a file channel number.
      As a suffix indicates a 64-bit numeric variable or constant. 
$     As a prefix indicates a 'fixed string' (string indirection).
      As a suffix indicates a string variable.  
%     As a prefix indicates a binary constant e.g. %11101111.
      As a suffix indicates an integer (signed 32-bit) variable. 
&     As a prefix indicates a hexadecimal constant e.g. &EF.
      As a suffix indicates a byte (unsigned 8-bit) variable. 
'     Causes an additional new-line in PRINT or INPUT.
;     Suppresses a forthcoming action, e.g. the new-line in PRINT. 
@     A prefix character for 'system' variables. 
^     A unary operator returning a pointer (address of an object).
      The dyadic exponentiation (raise to the power) operator.
\     The line continuation character, to split code across lines. 
[ ]   Delimiters for assembler statements.
{ }   Indicates a structure.  
~     Causes conversion to hexadecimal, in PRINT and STR$. 
|     A unary operator giving floating-point indirection.
      A delimiter in the VDU statement.  

```



## bc


* = assignment operator
* % modulus operator
* - negative number prefix
* == equality comparative operator


### Enclosures


* " " literal string enclosures
* ( ) expression enclosures, precedence override
* /* */ block comment enclosures


## Befunge


Every command in Befunge is a single character, but there are no identifiers in the language, so no need for escaping these command characters.

In '''Befunge-93''' there are only 37 commands:

* <code>+</code> add
* <code>-</code> subtract
* <code>*</code> multiply
* <code>/</code> divide
* <code>%</code> modulo
* <code>!</code> negate
* <code>`</code> greater than
* <code>></code> go right
* <code><</code> go left
* <code>^</code> go up
* <code>v</code> go down
* <code>?</code> go random direction
* <code>_</code> branch left or right
* <code>|</code> branch up or down
* <code>"</code> toggle string mode
* <code>:</code> duplicate
* <code>\</code> swap
* <code>$</code> pop
* <code>.</code> output integer
* <code>,</code> output character
* <code>#</code> bridge
* <code>g</code> get value
* <code>p</code> put value
* <code>&</code> input integer
* <code>~</code> input character
* <code>0</code> to <code>9</code> push integer values 0 to 9
* <code>SPACE</code> nop
* <code>@</code> end program

Every other character is considered an unsupported instruction (unless inside a string), and will typically be ignored. Although some implementations may generate a warning (the default behaviour in the current reference implementation), and others may even terminate the program when encountering an unsupported character.

'''Befunge-96''' added 12 new commands to the language (if you count the <code>;</code> interpreter directive), bringing the total to 49:

<code> !"#$%&'()*+,-./0123456789:;<>?@GPT\^_`ghijpv{|}~</code>

'''Befunge-97''' withdrew the <code>h</code> command, changed the meaning of the <code>;</code>, and added 29 new commands, making a total of 77:

<code> !"#$%&'()*+,-./0123456789:;<=>?@ADEFGHJPQRTUVXY[\]^_`abcdefgijnopqrsvwyz{|}~</code>

And in '''Befunge-98''', basically every printable ASCII character is assigned some functionality. The uppercase <code>A</code> to <code>Z</code> characters are reserved for fingerprint extensions, though, so they're initially undefined and are only assigned a behaviour at runtime.

And unlike the earlier versions, when Befunge-98 encounters an unsupported instructions (typically an unassigned fingerprint character), the program counter is reflected instead of the character being ignored.


## Bracmat

Almost all ASCII characters that are not alphanumeric are special characters in Bracmat. The one exception (April 2014) is the closing square bracket <code>]</code>. Any character can be part of an identifier if the identifier is enclosed in double quotes. The only characters that must be escaped are <code>\</code> and <code>"</code>.

Some special characters - the prefix characters <code>[ ~ / # &lt; > % @ ` ? ! -</code> - can occur inside an unquoted identifier, as long as they are preceded by an alphanumeric character.

The usual control codes can occur in unquoted identifiers if represented as escape sequences <code>\a \b \t \n \v \f \r</code>. If a control code occurs in an identifier in own person, then the identifier must be quoted.

Also <code>\"</code> and <code>\\</code> can occur in unquoted identifiers.

If in doubt whether an identifier needs quotes, use them in your code and see whether Bracmat needs them by inspecting the result of a program listing produced by the built-in function <code>lst$</code>. If the quotes have disappeared, they were not necessary. It is never wrong to enclose an identifier in quotes.

=={{header|Brainf***}}==
The only characters that mean anything in BF are its commands: 

* > move the pointer one to the right
* < move the pointer one to the left
* + increment the value at the pointer
* - decrement the value at the pointer
* , input one byte to memory at the pointer
* . output one byte from memory at the pointer
* [ begin loop if the value at the pointer is not 0
* ] end loop

All other characters are comments.


## C

See [[Special characters#C++|C++]].

As in C++, <tt>?</tt>, <tt>#</tt>, <tt>\</tt>, <tt>'</tt> and <tt>"</tt> have special meaning (altogether with <tt>{</tt> and <tt>}</tt>). Also trigraphs work (they are an "old" way to avoid the "old" difficulties of finding characters like { } etc. on some keyboards).

* = assignment operator, enumeration value
* , function parameter separator
* ; statement separator, loop construct component
* . element member selector for structures or unions, decimal point
* ! logical NOT operator
* ~ bitwise BWNOT
* # preprocessor stringize operator, preprocessor directive prefix
* \ literal character notation prefix, line continuation
* & address resolution prefix, bitwise BWAND operator
* * multiplication, pointer resolution, file pointer prefix, indirection
* + addition, optional unary positive, prefix increment, postfix increment
* - subtraction, unary negative
* / division
* ^ bitwise BWXOR operator
* | bitwise BWOR operator
* _ internal library identifier prefix
* % modulus, output format specifier prefix
* > greater than comparative operator
* < less than comparative operator


###  Digraphs 


* == equality operator
* != inequality operator
* ++ incremental nudge operator
* -- decremental nudge operator
* && logical AND
* || logical OR
* += additive compound assignment operator
* -= subtractive compound assignment operator
* *= multiplication compound assignment operator
* /= division compound assignment operator
* %= modulus assignment operator
* <= less than  or equal to comparative operator
* >= greater than or equal to comparative operator
* &= bitwise BWAND combination assignment operator
* |= bitwise BWOR combination assignment operator
* ^= bitwise BWXOR combination assignment operator
* << bitwise left shift
* >> bitwise right shift
* .* pointer to object member
* .> pointer to structure or union element
* :> base operator
* ## preprocessor concatenation operator
* #@ preprocessor charizing operator


###  Trigraphs 


* .>* pointer to pointer member
* <<= bitwise left shift compound assignment
* >>= bitwise right shift compound assignment
* ... used in variadic function declaration


###  Enclosures 


* " " literal string enclosures
* { } Group statements together into blocks of code
* ( ) Enclosure for function parameters
* < > header filename enclosure
* /* */ Comment enclosures


###  Ternary operators 


* ? , : The hook and colon are used together to produce ternary operation syntax


###  C99 Extensions 


* // Comment prefix

C99 standard (but not previous standards) recognizes also '''universal character names''', like C++.

'''String and character literals''' are like C++ (or rather the other way around!), and even the meaning and usage of the <tt>#</tt> character is the same.


## C++

C++ has several types of escape sequences, which are interpreted in various contexts. The main characters with special properties are the question mark (<tt>?</tt>), the pound sign (<tt>#</tt>), the backslash (<tt>\</tt>), the single quote (<tt>'</tt>) and the double quote (<tt>"</tt>).


###  Digraphs 


* // comment prefix
* << infeed operator
* >> outfeed operator
* :: scope modifier


###  Trigraphs 

Trigraphs are certain character sequences starting with two question marks, which can be used instead of certain characters, and which are always and in all contexts interpreted as the replacement character. They can be used anywhere in the source, including, but not limited to string constants. The complete list is:
 Trigraph  Replacement letter
   ??(       [
   ??)       ]
   ??<       {
   ??>       }
   ??/       \
   ??=       #
   ??'       ^
   ??!       |
   ??-       ~

Note that interpretation of those trigraphs is the very first step in C++ compilation, therefore the trigraphs can be used instead of their replacement letters everywhere, including in all of the following escape sequences (e.g. instead of \u00CF (see next section) you can also write ??/u00CF, and it will be interpreted the same way).

Also note that some compilers don't interpret trigraphs by default, since today's character sets all contain the replacement characters, and therefore trigraphs are practically not used. However, accidentally using them (e.g. in a string constant) may change the code semantics on some compilers, so one should still be aware of them.


###  Universal character names and escaping newlines 

Moreover, C++ allows to use arbitrary Unicode letters to be represented in the basic execution character set (which is a subset of ASCII), by using a so-called universal character name. Those have one of the forms
 \uXXXX
 \UXXXXXXXX
where each X is to be replaced by a hex digit. For example, the German umlaut letter ü can be written as
 \u00CF
or
 \U000000CF
However, letters in the basic execution character set may not be written in this form (but since all those characters are in standard ASCII, writing them as universal character constants would only obfuscate anyway). If the compiler accepts direct usage of of non-ASCII characters somewhere in the code, the result must be the same as with the corresponding universal character name. For example, the following two lines, if accepted by the compiler, should have the same effect:

```cpp
std::cout << "Tür\n";
std::cout << "T\u00FC\n";
```

Note that in principle, C++ would also allow to use such letters in identifiers, e.g.

```cpp
extern int Tür; // if the compiler allows literal ü
extern int T\u00FCr; // should in theory work everywhere
```

but that's not generally supported by existing compilers (e.g. g++ 4.1.2 doesn't support it).

Another escape sequence working everywhere is to escape the newline: If a backslash is at the end of the line, the next line is pasted to it without any space in between. For example:

```cpp
int const\
ant; // defines a variable of type int named constant, not a variable of type int const named ant
```



###  String and character literal 

A string literal is surrounded by double quotes(<tt>"</tt>). A character literal is surrounded by single quotes (<tt>'</tt>). Example:

```cpp
char const str = "a string literal";
char c = 'x'; // a character literal
```


The following escape sequences are only allowed inside string constants and character constants:
 escape seq.  meaning          ASCII character/codepoint
  \a           alert             BEL ^G/7
  \b           backspace         BS  ^H/8
  \f           form feed         FF  ^L/12
  \n           newline           LF  ^J/10
  \r           carriage return   CR  ^M/13
  \t           tab               TAB ^I/9
  \v           vertical tab      VT  ^K/11
  \'           single quote      '           (unescaped ' would end character constant)
  \"           double quote      "           (unescaped " would end string constant)
  \\           backslash         \           (unescaped \ would introduce escape sequence)
  \?           question mark     ?           (useful to break trigraphs in strings)
  \0           string end marker NUL ^@/0    (special case of octal char value)
  \''nnn''         (octal char value)            (each ''n'' must be an octal digit)
  \x''nn''         (hex char value)              (each ''n'' must be a hexadecimal digit)

Note that C++ doesn't guarantee ASCII. On non-ASCII platforms (e.g. EBCDIC), the rightmost column of course doesn't apply. However, <tt>\0</tt> unconditionally has the value 0.

Also note that some compilers add the non-standard escape sequence <tt>\e</tt> for Escape (that is, the ASCII escape character).


###  The # character 

The <tt>#</tt> character in C++ is special as it is interpreted only in the preprocessing phase, and shouldn't occur (outside of character/string constants) after preprocessing.
*If <tt>#</tt> appears as first non-whitespace character in the line, it introduces a preprocessor directive. For example

```cpp>#include <iostream></lang

*Inside macro definitions, a single <tt>#</tt> is the stringification operator, which turns its argument into a string. For example:

```cpp
#define STR(x) #x
int main()
{
  std::cout << STR(Hello world) << std::endl; // STR(Hello world) expands to "Hello world"
}
```

*Also inside macro definitions, <tt>##</tt> is the token pasting operator. For example:

```cpp
#define THE(x) the_ ## x
int THE(answer) = 42; // THE(answer) expands to the_answer
```


Note that the # character is not interpreted specially inside character or string literals.


## Clojure

See [http://clojure.org/reader Clojure's Reader documentation].


## E


E uses typical C-style backslash escapes within literals. The defined escapes are:

{|
! Sequence !! Unicode !! Meaning
|-
| \b || U+0008 || (Backspace)
|-
| \t || U+0009 || (Tab)
|-
| \n || U+000A || (Line feed)
|-
| \f || U+000C || (Form feed)
|-
| \r || U+000D || (Carriage return)
|-
| \" || U+0022 || "
|-
| \' || U+0027 || '
|-
| \\ || U+005C || \
|-
| \&lt;newline> || None || (Line continuation -- stands for no characters)
|-
| \u''XXXX'' || U+XXXX || ([[wp:Basic Multilingual Plane|BMP]] Unicode character, 4 hex digits)
|}

Consensus has not been reached on handling non-BMP characters. All other backslash-followed-by-character sequences are syntax errors. 

Within E ''quasiliterals'', backslash is not special and <code>$\</code> plays the same role;


```e
? println(`1 + 1$\n= ${1 + 1}`)
1 + 1
= 2
```



## Erlang

Erlang variables can use A-Z, a-z, _ and 0-9. They must start with A-Z or _. Erlang atoms (sort of enums) can use the same characters, but must start with a-z. If the atom is quoted, ie starts and stops with ', it can contain any character (except ').


## Forth


When Forth fails to interpret a symbol as a defined word, an attempt is made to interpret it as a number.  In numerical interpretation there arise a number of special characters:


```forth

  10   \ single cell number
  -10  \ negative single cell number
  10.  \ double cell number
  10e  \ floating-point number
```


Many systems - and the Forth200x standard - extend this set with base prefixes:


```forth

  #10  \ decimal
  $10  \ hex
  %10  \ binary
```


Of strings, Forth200x [http://www.forth200x.org/escaped-strings.html Escaped Strings] adds a string-parsing word with very familiar backslashed escapes.

There are otherwise no special characters or escapes in Forth.


## Fortran


### In source coding

Fortran source code started off in a fixed format using decks of punched cards. Columns one to five were for labels (only integer numbers) and column six marked whether the line was a continuation of the previous line, indicated by any character other than blank or zero there. It is usual to employ consecutive digits for consecutive continuation lines and, by back-formation, a zero can be used for the first line of the statement - which is not a continuation line. Source code occupied columns seven to seventy-two. The last columns could be used as a sequence field, helpful if a deck of cards was dropped. Various characters had special meanings for the interpretation:
 C  or c in column one mark a comment line.
 D  or d in column one mark a debugging statement, ignored or compiled according to an option set at compile time.
 *  in column one for compiler control statements (e.g. Fortran II, Fortran IV)
 0  in column six does ''not'' indicate a continuation line, even though not a blank.
    Space: ignored within source outside text literals, even between parts of a word. Allows <code>G O  TO</code>
 '  Delimits a text literal. Also has been used in READ(F'N) to [[Read_a_specific_line_from_a_file#Random_access|read record N of the file attached to F]].
 "  Delimits a text literal. Doubling required for each contained in the literal.    
 !  Outside a text literal marks an "escape comment" - only text before it on the line will be compiled. The B6700 used % for this.
 &  Outside a text literal indicated that further source is continued on the next line.
F90 standardised on ! as a comment marker, in any column (except six, for fixed-format source) and the & is used for free-format source, wherein spaces become significant. With the move from source files via decks of punched cards to text files in ASCII, there has always been confusion over the meaning of the ASCII control characters such as HT, CR, LF, etc. so that, for example, a tab in column one might space to column seven. Likewise within text literals, and the once pure notion of a text ''literal'' may now be adulterated by the notion of included "backslash escape sequences", as in C.
 
Text literals were first defined within FORMAT statements via the H-code process involving counting so that 3H! ! meant three characters following the H, any characters, and don't miscount! Later, apostrophes were used to delimit a text literal with any contained apostrophe being doubled, as in 'Isn' 't' [a space between the two apostrophes to prevent misinterpretation here!] then either an apostrophe or a double quote could be used to mark the start of a text literal with the same used to mark its end and doubling for contained markers, while the other sort may be used freely, as in "Isn't".

The names of variables (and functions and subroutines) in Fortran must start with a letter and can continue with letters or digits. Early Fortran did not recognise lower-case letters; later compilers do but do not distinguish upper-case from lower-case. Additional symbols such as _ and $ may be accepted in names. With F90 came the ability to define data aggregates, whose parts might be referenced via LIST%LINK and LIST%DATUM where the % is ''not'' a part of a long name but marks the parts of a compound name. A common alternative to % is a full stop, which is usual in many other languages. Spaces are ignored outside text literals so LIST % LINK would also be valid.


Numbers are the usual sequence of digits, possibly preceded by a sign and using a full stop to mark a decimal point - they are in base ten. However, the exponential form follows those digits with a E or D (or Q if quadruple precision is available) which ''in that context'' become special symbols. Thus, <code>+3.14159E+0</code>. Similarly, non-decimal constants can be specified via what looks like a text literal but which is prefixed by one of Z, O, or B which ''in that context'' become special symbols. Thus, Z"FFFFFFFFFFFFFFFF" specifies a value in hexadecimal that, if assigned to a sixty-four bit floating-point variable, will result in it having the state NaN for "not a number" - on systems supporting this notion. Thus, O is for octal and B for binary; H is preempted so Z is used instead.

Basic special symbols:
 .  Decimal point, also a "strop" symbol for special names such as .TRUE. or .GT.
 ,  Comma, to separate many types of list: parameters, array indices, I/O list expressions, ...
 +  Plus sign, also for addition.
 -  Minus sign, also for subtraction.
 /  For division. Also as a syntax mark as in <code>DATA PI /3.14159/</code>
 *  For multiplication. 
       Also for an unspecified array bound as in A(*), sizes as in <code>REAL*8 X</code>, 
       part of an [[Flow-control_structures#Deviant_RETURN|alternate return]] specification.
       indicates "standard output" or input as in <code>WRITE(*,26)</code> where 26 labels a FORMAT statement
       indicates "free format" when in place of a FORMAT indicator, as in <code>WRITE(6,*) "Hello!"</code>
 ** For exponentiation.
 ^  May be accepted for exponentiation.
 =  For assignment, as in PI = 4*ATAN(1), not for asserting or testing equality.
 (  For bracketing. Within expressions, or for parameter lists, or other syntax such as IF (...) THEN
 )  Corresponding. No usage of {[ ]}.
 _  Possibly allowed as a part of a variable's name.
 $  Possibly allowed as a part of a variable's name.
 #  Possibly allowed to specify a constant in another base, as in <code>3#20</code> to represent six in base three.
F90 added more, standardising variants on .EQ. and .GT. ''etc.''
 == or .EQ. tests equality.
 /= or .NE. tests non-equality (beware variables holding NaN) Alas ¬ has been lost from the ASCII keyboard.
 <= or .LE.
 >= or .GE.
 <  or .LT.
 >  or .GT.
 |  or .OR.
 &  or .AND. Also an alternative to * in an alternate return scheme.
 :  Used in array span specifications, such as A(3:36) = 0, in other syntax such as labels.
 :: Used in compound declarations, such as <code>INTEGER, ALLOCATABLE:: STUFF(:)</code>
 ;  Ends a statement. Another may follow on the same line, as in Algol, etc.
 %  Used to separate the parts of a compound name; a full stop may be accepted for this as well.
 \  Within a text literal (!) may introduce a "backslash escape sequence" as in C to be interpreted non-literally.
 (/ Starts a list of values that are to be treated as an array. 
 /) As in (/1,2,3,4/) Possibly recognised is [1,2,3,4].
 => For messing with pointers...

Some parallel-processing facilities have been devised via the notion of "coarray" Fortran that introduces a use for the special characters [ and ]. Basically, if <code>X</code> is a variable (simple or array) then <code>X[i]</code> would be a reference to the version of <code>X</code> belonging not to the current "image" executing but that of image ''i'', supposing that multiple "images" of the code were executing more or less in parallel.


### In FORMAT coding

Within a FORMAT statement various single letters are used to indicate a format code, and these letters have no connection to any variables of that name outside the FORMAT statement. Not all compilers recognise all editing codes as there has been variation, extension, retraction, and standardisation in various directions. A list of such codes is prepared and there is the option of repeat counts, for single items or a group of items within brackets. Most codes select the form desired for a datum and may be constrained as to the type of the datum that is appropriate. The codes are single characters, often associated with integers that specify a width so that I6 means an integer field of six characters. 3I6 means three such fields and 3(I6,I3) means three sets of a six and a three-digit field.

The basic idea is that the READ or WRITE statement has an I/O list which for output is a sequence of expressions (though usually just the name of a variable), while the input list may only be the names of variables that are to receive the incoming data. Thus, the I/O list is worked through, in parallel with working through the items in the format list. Some format codes accept a datum and for output, describe what text is to go to the output buffer, while for input the format code describes what text from the input buffer is to be converted to a value to be placed in the waiting variable of the READ statement's input list. Other format items specify actions to be taken before the next datum is dealt with and also adjust the working position along the text being read or written.

  I9   Integer, width nine. I0 on output means a width sized to suit the numerical value.
  L9   Logical, width nine but on output only <code>T</code> or <code>F</code> appears - see below.
  F9.3 Floating-point, width nine with three decimal digits after its decimal point.
  D9.3 Floating-point double-precision. Type clashes between datum and format code may be declared an error.
  Q9.3 Floating-point quadruple precision, if available.
  E9.3 Floating-point, exponential form, whereby one would appear as <code>0.100E+01</code> (E10.3 would allow enough space for the sign)
  A9   Character, width nine. Just <code>A</code> to transfer as many characters as the variable holds. For input, up to the end of the record.
  B9   Any type. Transfers binary bits as stored in the variable. Not necessarily its numerical value in binary.
  O9             In octal. For floating-point especially the bit pattern will not be that of the value in binary.
  Z9             In hexadecimal.
 9X    Space over nine columns. Alas, 4X3 is not allowed for four advances of three. Only 12X will do.
  T9   Move the work position to column nine. Does ''not'' produce "tab" characters to do so.
  Q    reveals the number of characters as yet unread in the input record.
 3Hxxx Exactly three characters as a text literal follow the H. Count carefully!
  '    Delimits a text literal in a more sensible way, as described above.
  "    Likewise.
Those are the commonly available codes. Still more codes can be used as a prefix to modify the function of an edit code, for instance to specify leading zeroes, or cause a + sign to appear for positive values (and zero, alas) rather than only a - for negative values. The E9.3 example code would fail for a value such as -1 because the exponent part, the digits of the fractional part, and the decimal point use up too much of the allowed space of nine. Such overflows lead to the field being filled by *
 1P    Shifts a value by one power, as in 1PE9.3, which shows one as <code>1.000E+00</code> Not a repeat count.
 $F9.2 For output: floats a dollar sign against the leading digit, so 12·45 appears as <code>   $12.45</code>
 SP    Writes a + sign, instead of only writing - for negative numbers. Produces +0, which may not be wanted.
 
There are further special symbols and some recondite usages:
 () For bracketing. In <code>FORMAT ("List",6(I3,","))</code> a seventh integer written out will start on a new line, ''not'' prefixed by List...
  , for separating items.
  . splitting parts of a format code as in F9.3
  / End line. Subsequent output (perhaps further along the FORMAT sequence) will start on the next line.
  : Signifies that format items following will be acted on only if the WRITE list has further items to send.
  $ If the WRITE list reaches this item, there will be no "end line" action. The next output continues the line.
  \ May be recognised as the same as $, if $ is recognised.
  < Introduces an arithmetic expression, whose value stands for the constant normally in that place.
  > Ends it. Thus F<NDIGITS + 3>.3 instead of F12.3 say (if NDIGITS had the value 9)
  
The text of a FORMAT statement is normally inspected by the compiler, also if it is supplied as a text literal instead of via the label of a FORMAT statement in a READ or WRITE statement. A special internal representation that can be more speedily followed at run time may be produced. However, instead of a text literal, an expression may be used, and it is quite possible to use a FORMAT statement to prepare the desired format code sequence in a CHARACTER variable. See for example [[Multiplication_tables#Traditional_approach]]


### In input and output
  

### =Unformatted=

This is "binary" input/output where the items of the I/O list are transferred in their "internal" form and size. The FORMAT apparatus is not engaged, at a great saving in time. Transferring such data files between computers with different data representations will be problematic and much depends on the file system facilities.


### =Formatted=

The characters being read have their normal interpretation, so <code>READ(F,"(I4,4I2,F5.3)") YYYY,MM,HH,HR,MN,SEC</code> would work with text such as <code>20170526150621123</code> as expected but there are some special features. In a numerical field a space is interpreted as a zero, and a full stop (as a decimal point) if present over-rides the implicit location of the decimal point given by the format code. Thus with F5.3, <code>12345</code> yields a value of 123·45 (note, ''six'' characters from a five-character field) but reading <code>12.45</code> would give the value 12·45. If a value can't be fitted into its allocated field, the field will be filled with * characters to signify overflow. Thus, <code>1234.</code> could be read via F5.3 but if that value were written with that format, ***** would appear.

Variables of type LOGICAL can be read and written with a format code of L, but on output, just T or F appears, not .TRUE. or .FALSE. as in Fortran source files. Further, with input if the first non-space sequence is <code>.T</code> or <code>T</code> (or lower case) then .TRUE. will result, and similarly for .FALSE. but with the added option that an entirely blank field will also be taken as .FALSE.

Later systems support special floating-point states such as NaN and Infinity, and these texts (possibly partly truncated) may be produced on output or accepted for input.

Other symbols present in input fields will evoke a format error. However, a comma in a numerical field may be treated specially. It ends the number being read, and the character following in the input becomes the start of the next field. Thus, a F20.6 field may be cut short by suitable input and the alignment of input data fields with the sizes declared in the FORMAT statement will be disrupted.




### =Free format=

This is also called list-directed input/output and is indicated by an * in place of a format reference, as in <code>WRITE(6,*) "X=",X</code> The output format for each item in the ''output-list'' will be appropriate for the type of the item, while for input, data are separated by commas or spaces, and if text values are to include spaces (or commas) they must be specified within quotes. If quoting is used, the content can be declared hexadecimal, octal, or binary via a prefix of Z, O, or B as in <code>Z"DEAD"</code> Complex numbers are enclosed in brackets, <code>(-41.29980,174.7763)</code> unlike the case with FORMAT statements. A surprising feature is that a / may be recognised as ending the line for input processing (perhaps a comment follows?) which will make the likes of dates such as 26/5/2017 unreadable via free-format input. Only the 26 will be read, and further input will be sought on following lines. <code>26 5 2017</code> would work, but <code>2017-05-26</code> will not because there are no delimiters (space or a comma) between the numbers. All these could be read via suitable FORMAT statements, but, alignment is critical! Stick to two-digit month and day numbers, or else!


### =Namelist=

This is a further increase in flexibility. I/O is performed via what looks like assignment statements (as might appear in DATA statements), of the form ''name'' = ''value''. An example appears in [[Update_a_configuration_file#Fortran]] and there are many details. The idea is that a collection of variables may be written out without the effort of devising a suitable FORMAT statement for them, as with free-format, but, the values are revealed along with the names holding them to aid in interpretation. For input, the idea is that only some of the variables may have their values specified, while the others in the name list stay as they were. This might be for a run needing only some of many parameters, or another try at an iteration, varying the values of some variables each time, again with the names involved to aid memory.

Namelist I/O involves blocks of text, over many lines. If the namelist is called STUFF, the block starts with a line bearing <code>&STUFF</code> and the block ends with a line bearing <code>/</code> All I/O starts in column two (as with free-format output) so that column one holds a space, in case lineprinters are involved. Each variable in the namelist is then shown, starting a new line (also in column two) as if it were an assignment statement in Fortran source followed by a comma - except for the last in the list. So, <code> TRACE = T,</code> would be for a LOGICAL variable. For arrays, there is a repeat count (as in a DATA statement) so that <code> A = 1,2,3,66*0,7,8,9,</code> would assign 72 values to the array. Alas, the @ symbol is not used for repetition, so actual arithmetic expressions are not interpreted because the multiply symbol is preempted. So:
   (space) must appear in column one.
 & introduces the block name,
 / marks its end,
 = shows an assignment as in ''name'' = ''value''
 * for repeated values, as in <code>F,33*T,F,</code> for thirty-three T values going to an array.
 : part of an array span as in <code>VALUE(6:60)</code>
 , for punctuation of the list of values and assignments,
 ' for literals.
 " with doubling for contained quotes as within Fortran source.
 ? or =? may evoke assistance...
If the namelist I/O is being conducted via standard input and standard output and this feature is recognised, entering ? instead of normal namelist input causes the namelist to be printed - names plus current values. Then, memory jogged, you carry on.


### =Carriage control=

An output device such as a lineprinter would traditionally interpret the first character of its output as a "carriage control" that would be acted upon then followed by printing the remaining text. Thus, a 120-column lineprinter would actually require to be presented with 121 characters for its rightmost column to be used. The code would be acted on as follows:
 + No motion - thus this output overprints the previous output.
   (space) - advance one line, thus write to the next line.
 0 Advance two lines - thus leave a blank line after the previous output.
 1 Advance to the top of the next page.
Other output devices may or may not follow this scheme. A teletype attached to a PDP-15 in the 1970s certainly would chug along to the top of the next page, but screens attached to IBM mainframes would not start at the top line. Still later systems have generally ignored this protocol, but the output from free-format WRITE statements still starts with a space, just in case.


## Gambas


* # prefixes stream numbers for input / output redirection
* ' comment prefix
* ; output separator moves cursor to next column instead of newline, separates redirection stream from data 
* : statement separator
* . object element separator
* , separates arguments to functions
* = assignment, equality
* + addition, optional unary positive
* - subtraction, unary negative
* * multiplication
* / division
* < less than
* > greater than


### Digraphs
 

* <= less than or equal to
* >= greater than or equal to
* <> inequality


### Enclosures


* " " string enclosures
* ( ) function parameter enclosures, overriding precedence


## Go

Within a character literals and string literals, the backslash is a special character that begins an escape sequence.  Examples are '\n' and “\xFF”.  These sequences are documented in the [http://golang.org/doc/go_spec.html#Character_literals language specification].

Special purpose escape sequences are also defined within the context of certain packages in the standard library, such html and regexp.

Go keywords, operators, and delimiters are all predefined are all composed of ASCII characters, however the character encoding of Go source code is specified to be UTF-8.  This allows user-defined identifiers and literals to incorporate non-ASCII characters.

Whitespace is generally ignored except as is it delimits tokens, with one exception:  Newline is a very special character.  As explained by the [http://golang.org/doc/go_spec.html#Semicolons language specification], translation (that is compilation) involves a step where the tokenizer converts (most) newlines to semicolons, which are then handled as terminators in the grammar of the formal language.  Of course you as the programmer, or user of the language, are not involved in this intermediate stage of the compilation process and so the effect you see is somewhat different.  The effect for the programmer is that the grammatical structure is partially determined by the 2D layout of the source code.


## GUISS


* , statement separator
* : Used as a separator (usually between the user interface component and the component name or gist)
* > Used to specify user input or selected item
* [ ] Enclosure for symbol or digraph names


## Haskell


Comments

```haskell
-- comment here until end of line
{- comment here -}
```


Operator symbols (nearly any sequence can be used)

```haskell
! # $ % & * + - . / < = > ? @ \ ^ | - ~ :
: as first character denotes constructor
```


Reserved symbol sequences

```haskell
.. : :: = \ | <- -> @ ~ => _
```


Infix quotes

```haskell
`identifier` (to use as infix operator)
```


Characters

```haskell
'.'
\ escapes
```


Strings

```haskell
"..."
\ escapes
```


Special escapes

```haskell
\a alert
\b backspace
\f form feed
\n new line
\r carriage return
\t horizontal tab
\v vertical tab
```


Other

```haskell
( )   (grouping)
( , ) (tuple type/tuple constructor)
{ ; } (grouping inside let, where, do, case without layout)
[ , ] (list type/list constructor)
[ | ] (list comprehension)
```


Unicode characters, according to category:

```haskell
Upper case (identifiers)
Lower case (identifiers)
Digits (numbers)
Symbol/punctuation (operators)
```



## HicEst

HicEst has no escape characters.
Strings may contain all characters.
String constants can be delimited by most non-standard characters, usually ' or ".
* ! starts a comment. The comment extends to the end of the line.
* The global variable [http://www.HicEst.com/$.htm $] is the current linear left hand side array index in array expressions
* The global variable [http://www.HicEst.com/$$.htm $$] is set to the sequence number of either of the last activated toolbar button number, or menu item number, or popup item number
* If # appears as the first character in a line, it starts the optional [http://www.HicEst.com/APPENDIX.htm appendix] section of the script. This terminates the program section. Appendix chapters are not compiled and are therefore not executable. They serve to store information that can be retrieved by the APPENDIX function.


## HTML


* = assignment (within a tag)
* / prefixes tag closures (within tag enclosures)


###  Enclosures 


* " " string value enclosures (within a tag)
* < > tag enclosures
* <!-- --> comment enclosures

=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon strings and csets may contain the following special characters

```txt
\b backspace
\d delete
\e escape
\f formfeed
\l linefeed
\n newline
\r return
\t horizontal tab
\v vertical tab
\' single quote
\" double quote
\\ backslash
\ddd octal code
\xdd hexadecimal code
\^c control code
```



## J


The closest thing J has to an escape sequence is that paired quotes, in a character literal, represent a single quote character.


```J
   ''      NB. empty string

   ''''    NB. one quote character
'
   ''''''  NB. two quote characters
''
```


Since it's not clear what "special characters" would mean, in the context of J, here is an informal treatment of J's word forming rules:

Lines are terminated by newline characters, and J sentences are separated by newline characters.  J sometimes treats sequences of lines specially, in which case a line with a single right parenthesis terminates the sequence.

A character literal consists of paired quote characters with any other characters between them.


```J
   'For example, this is a character literal'
```


A numeric literal consists of a leading numeric character (a digit or _) followed by alphanumeric (numeric or alphabetic) characters, dots and spaces.  A sequence of spaces will end a numeric literal if it is not immediately followed by a numeric character.


```J
   1
   1 0 1 0 1 0 1
   _3.14159e6
```


Some numeric literals are not implemented by the language


```J
   3l1t3
|ill-formed number
```


Words consist of an alphabetic character (a-z or A-Z) followed by alphanumeric characters and optionally followed by a sequence of dots or colons.  Words which do not contain . or : can be given definitions by the user.  The special word NB. continues to the end of the line and is ignored (it's a comment) during execution. Words may also contain the underscore character (_) but if there's a trailing underscore, or if there's two adjacent underscores in a word, that has special significance in name lookup.


```J>   example=: ARGV NB. example and ARGV are user definable words</lang


Tokens consist of any other printable character optionally followed by a sequence of dots or colons.  (Tokens which begin with . or : must be preceded by a space character).


```J
  +/ .*  NB. + / . and * are all meaningful tokens in J
```



## Java

Math:

```java
& | ^ ~ //bitwise AND, OR, XOR, and NOT
>> << //bitwise arithmetic shift
>>> //bitwise logical shift
+ - * / = % //+ can be used for String concatenation)
```

Any of the previous math operators can be placed in front of an equals sign to make a self-operation replacement:

```java
x = x + 2 is the same as x += 2
++ -- //increment and decrement--before a variable for pre (++x), after for post(x++)
== < > != <= >= //comparison
```

Boolean:

```java
! //NOT
&& || //short-circuit AND, OR
^ & | //long-circuit XOR, AND, OR
```

Other:

```java
{ } //scope
( ) //for functions
; //statement terminator
[ ] //array index
" //string literal
' //character literal
? : //ternary operator
// //comment prefix         (can be escaped by \u unicode escape sequence see below)
/* */ //comment enclosures  (can be escaped by \u unicode escape sequence see below)

```

Escape characters:

```java
\b     //Backspace
\n     //Line Feed
\r     //Carriage Return
\f     //Form Feed
\t     //Tab
\0     //Null) Note. This is actually a OCTAL escape but handy nonetheless
\'     //Single Quote
\"     //Double Quote
\\     //Backslash
\DDD   //Octal Escape Sequence, D is a number between 0 and 7; can only express characters from 0 to 255 (i.e. \0 to \377)
```

Unicode escapes:

```java
\uHHHH //Unicode Escape Sequence, H is any hexadecimal digit between 0 and 9 and between A and F
```

Be extremely careful with Unicode escapes. Unicode escapes are special and are substituted with the specified character ''before'' the source code is parsed. In other words, they apply anywhere in the code, not just inside character and string literals. Variable names can contain foreign characters. It also means that you can use Unicode escapes to write any character in the source code, and it would work. For example, you can say <code>\u002b</code> instead of saying <code>+</code> for addition; you can say <code>String\u0020foo</code> and it would be interpreted as two identifiers: <code>String foo</code>; you can even write the entire Java source file with Unicode escapes, as a poor form of obfuscation.

However, this leads to many problems:
* <code>\u000A</code> will become a line return in the code, which will terminate line-end comments:

```java>// hello \u000A this looks like a comment</lang

: is a syntax error, because the part after <code>\u000A</code> is on the next line and no longer in the comment
* <code>\u0022</code> will become a double-quote in the code, which ends / begins a string literal:

```java
"hello \u0022 is this a string?"
```

: is a syntax error, because the part after <code>\u0022</code> is outside the string literal
* An invalid sequence of <code>\u</code>, even in comments that usually are ignored, will cause a parsing error:

```java
/*
 * c:\unix\home\
 */
```

: is a syntax error, because <code>\unix</code> is not a valid Unicode escape, even though you think that it should be inside a comment


## JavaScript

See [[#Java|Java]]


## jq

Any JSON entity can be specified in a jq program in accordance with the JSON specification. See json.org for details.  The following discussion accordingly ignores JSON literals.

jq severely restricts the characters that can be used as "identifiers" in a jq program. The regular expression governing the choice of identifiers is currently:
```jq
^[a-zA-Z_][a-zA-Z_0-9]*$
```


That is, identifiers are alphanumeric except that _ may also be used.

jq variables take the form of an identifier preceded by "$", e.g. "$a". 

Almost all the ASCII printing characters that are invalid in jq identifiers have special significance in jq programs. There are currently just five exceptions -- ~`^&' -- but "^" has its usual significance in connection with regular expression specifications.


### =Object Keys=

The restriction on jq identifiers does not apply to object keys. If o is an object, and if k is a key (i.e. a JSON string), then the value of k in o can accessed as o[k], e.g. o["mykey"]; furthermore, as a convenience, if the string of characters in the key conforms to the rules for jq identifiers, then the form o.id may be used, where id is the key without the enclosing quotation marks. For example, .["mykey"] is synonymous with .mykey


### =String Interpolation=

jq also supports "string interpolation". To interpolate the string value of any JSON entity, e, a string literal such as "\(e)" is used.  Notice that such interpolating strings are not valid JSON strings themselves.


## Julia

Operators are just a special kind of function in Julia. Thus, the same syntax 
used for function identifiers can be used for operators. The converse is not true.
There are many characters, such '+', that are valid for use as operators, but that
cannot be used as characters in identifiers.

To quote the documentation: "Variable names must begin with a letter (A-Z or a-z), 
underscore, or a subset of Unicode code points greater than 00A0; in particular, 
Unicode character categories Lu/Ll/Lt/Lm/Lo/Nl (letters), Sc/So (currency and 
other symbols), and a few other letter-like characters (e.g. a subset of the Sm 
math symbols) are allowed. Subsequent characters may also include ! and digits 
(0-9 and other characters in categories Nd/No), as well as other Unicode code 
points: diacritics and other modifying marks (categories Mn/Mc/Me/Sk), some 
punctuation connectors (category Pc), primes, and a few other characters."



## Kotlin

Kotlin uses a large number of special symbols which are listed on its website at https://kotlinlang.org/docs/reference/keyword-reference.html#operators-and-special-symbols.

The escape sequences are listed at https://kotlinlang.org/docs/reference/basic-types.html#characters and are somewhat smaller in number than most other C-family languages as rarely used escapes such as \a, \f and \v are not supported. However, an additional escape \$ is needed to represent a literal $ symbol because this symbol is used for string interpolation purposes.


## Lasso

Lasso has the follow special characters (excluding math / string functions) [http://lassoguide.com/language/operators.html].


```Lasso
#	defined local ie. #mylocal will fail if not defined
$	defined variable ie. $myvar will fail if not defined
=	assignment
:=	assign as return assigned value
?	ternary conditional true ? this
|	ternary else false ? this | that
||	or
&&	and
!	negative operator
{	open capture
}	close capture
=>	specify givenblock / capture 
->	invoke method: mytype->mymethod
&  	retarget: mytype->mymethod&  // returns mytype
^	autocollect from capture: {^ 'this will be outputted' ^}
::	tag prefix, ie. ::mytype->gettype // returns myype
::	type constraint, ie. define mymethod(p::integer) => #i * 2 
\	escape method: ie. \mymethod->invoke(2)
// comment
/* open comment
*/ close comment
```



## LaTeX


LaTeX has ten special characters: # $ % & ~ _ ^ \ { }

To make some of these characters appear literally in output, prefix the character with a \. For example, to typeset 5% of $10 you would type


```latex
5\% of \$10
```


Note that the set of special characters in LaTeX isn't really fixed, but can be changed by LaTeX code. For example, the package <tt>ngerman</tt> (providing German-specific definitions, including easier access to umlaut letters) re-defines the double quote character (") as special character, so you can more easily write German words like "hören" (as <tt>h"oren</tt> instead of <tt>h{\"o}ren</tt>).


## Lilypond


* = assignment
* % comment prefix
* \ command prefix, function name prefix
* / time signature notation separator
* # direct scheme expression prefix
* - sharp of flat semipitch prefix (note: this is not special in a direct scheme expression)
* . dotted note suffix
* | barline marker
* ' raise octave suffix
* , lower octave suffix
* ? cautionary accidental
* ~ accidental tie
* \\ combined voice fragment separator


###  Enclosures 


* " " title enclosure, voice name enclosure
* { } compound music expression enclosure, markup text enclosure, expression enclosure
* < > chord grouping enclosures
* %{ %} multiline comment enclosures
* << >> combined voice fragment enclosures


## Lingo

*Assignment: =
*Comment prefix: --
*Arithmetic operators: * / + -
*Comparative operators: = <> < > <= >=
*Decimal point: .
*Literal string enclosure: "
*String concatenation operators: & &&
*Operator for extracting parts of a string: ..
*Overriding precedence and function argument enclosures: ( )
*List/property list enclosures: [ ]
*List/property list element separator: ,
*Property list key/value assignment: :
*Line continuation character: \
<br />
In literal string assignments only " has to be escaped. This can be done by using the constant QUOTE:

```lingo
str = "Hello " & QUOTE & "world!" & QUOTE
put str
-- "Hello "world!""
```

<br />
The special characters listed above are not allowed in variable or function names.


## Lua



###  Assignment 


* = Assignment


###  Arithmetic Operators 


* + Addition, Optional unary positive
* - Subtraction, Optional unary minus
* * Multiplication
* / Division
* % Modulus (also character class prefix)
* ^ Exponent


###  Comparative Operators 


* == equality
* <  less than
* >  greater than
* <= less than or equal to
* >= greater than or equal to
* ~=


###  Concatenation Operators 


* .. concatenation operator


###  Length Counter 


* # Length operator (also used as a directive prefix)


###  Logical Operators 


* && logical and
* || logical or


###  Markup Components 


* ;
* :
* , list separator, function argument separator
* . decimal point
* ... vararg placeholder in function definition
* ;:=
* ::=


###  Prefixes 


* \ Literal character representation prefix
* -- Comment prefix
* __ metamethod prefix


###  Regular expression components 


* * regular expression repetition operator
* + regular expression repetition operator
* - regular expression range operator


###  Enclosures 


* ' ' Literal string enclosures (interpolated)
* " " Literal string enclosures (interpolated)
* ( ) Overriding precedence, function argument enclosures
* [ ] Element number enclosure, regular expression enclosure
* [^ ] compliment box enclosure
* { } Enumeration enclosures
* --[[ ]] Comment block enclosures
* ---[[ ]] Uncommented block enclosures


## Mathematica


```Mathematica
Markup : 
()   Sequence
{}   List
"    String
\    Escape for following character
(* *) Comment block
base^^number`s 
`    Context
[[]] Indexed reference

Within expression:
\        At end of line: Continue on next line, skipping white space
```



## MBS


* '''!''' start of comment line
* ''':=''' assignment operator
* '''=''' comparative equality operator
* ''';''' end of statement marker
* ''',''' argument separator
* '''+''' addition operator
* '''*''' string length assignment
* '''" "''' used as enclosures for strings
* '''( )''' function argument enclosures
* '''/*''' and '''*/''' enclosure symbols for alternative style comments


## MUMPS

MUMPS doesn't have any special characters among the printable ASCII set. The double quote character, &quot;, is a bit odd when it is intended to be part of a string. You double it, which can look quite odd when it's adjacent to the delimiting edge of a string.
```txt

USER>Set S1="Hello, World!"  Write S1
Hello, World!
USER>Set S2=""Hello, World!"" Write S2
 
SET S2=""Hello, World!"" Write S2
^
<SYNTAX>
USER>Set S3="""Hello, World!"" she typed." Write S3
"Hello, World!" she typed.
USER>Set S4="""""""Wow""""""" Write S4
"""Wow"""

```



## Nim

From [http://nim-lang.org/manual.html#string-literals the Nim Manual]:
<table border="1" class="docutils"><tr><th>Escape sequence</th><th>Meaning</th></tr>
<tr><td><tt class="docutils literal"><span class="pre">\n</span></tt></td><td><span id="newline_109139365">newline</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\r</span></tt>, <tt class="docutils literal"><span class="pre">\c</span></tt></td><td><span id="carriage-return_731232527">carriage return</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\l</span></tt></td><td><span id="line-feed_1443601756">line feed</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\f</span></tt></td><td><span id="form-feed_295412702">form feed</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\t</span></tt></td><td><span id="tabulator_1835392927">tabulator</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\v</span></tt></td><td><span id="vertical-tabulator_359268340">vertical tabulator</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\\</span></tt></td><td><span id="backslash_112366856">backslash</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\&quot;</span></tt></td><td><span id="quotation-mark_2111627364">quotation mark</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\'</span></tt></td><td><span id="apostrophe_551932232">apostrophe</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\</span></tt> '0'..'9'+</td><td><span id="character-with-decimal-value-d_519620758">character with decimal value d</span>; all decimal digits directly following are used for the character</td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\a</span></tt></td><td><span id="alert_2043891028">alert</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\b</span></tt></td><td><span id="backspace_1274784623">backspace</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\e</span></tt></td><td><span id="escape_471864567">escape</span> <span id="esc_611322509">[ESC]</span></td></tr>
<tr><td><tt class="docutils literal"><span class="pre">\x</span></tt> HH</td><td><span id="character-with-hex-value-hh_1992745580">character with hex value HH</span>; exactly two hex digits are allowed</td></tr>
</table>

There are also raw string literals that are preceded with the letter r (or R) and are delimited by matching double quotes (just like ordinary string literals) and do not interpret the escape sequences. This is especially convenient for regular expressions or Windows paths:

```nim
var f = openFile(r"C:\texts\text.txt") # a raw string, so ``\t`` is no tab
```

To produce a single " within a raw string literal, it has to be doubled:

```nim
r"a""b"
```

Produces:

```nim
a"b
```



## OASYS Assembler

The special characters are:
 ;   Comment (to end of line)
 -   Introduces a negative number (not used for subtraction)
 =   Load an include file or define a macro
 &   Prefix for a method
 %   Prefix for a global variable
 !   Prefix for an object
 .   Prefix for a property
 '   Prefix for a vocabulary word
 ?   Prefix for a class; check if an object is of this class
 *   Prefix for a class; create a new object of this class
 :   Prefix for a label; define the label
 /   Prefix for a label; jump if true
 \   Prefix for a label; jump if false
 |   Prefix for a label; jump unconditionally
 ,   Prefix for a local variable or argument
 [   Begin a declaration heading or phrase
 ]   End a declaration heading or phrase
 (   Begin a dispatch method
 )   End a dispatch method
 <   Read through a pointer
 >   Write through a pointer
 +   The object that the method was called on
 "   Begin string literal
 {   Begin string literal
 ~   Special (used for advanced macros)
 ^   Suffix for pointer types
 @   Suffix for object type
 #   Suffix for integer type
 $   Suffix for string type


## Objeck



```objeck

\b     //Backspace
\n     //Line Feed
\r     //Carriage Return
\t     //Tab
\0     //Null
\'     //Single Quote
\"     //Double Q

```


Unicode escapes:

```objeck
\uHHHH //Unicode Escape Sequence, H is any hexadecimal digit between 0 and 9 and between A and F
```



## OCaml


Character escape sequences
 \\     backslash
 \"     double quote
 \'     single quote
 \n     line feed
 \r     carriage return
 \t     tab
 \b     backspace
 \  (backslash followed by a space) space
 \DDD   where D is a decimal digit; the character with code DDD in decimal
 \xHH   where H is a hex digit; the character with code HH in hex


## Ol

Reserved characters are:

```txt

   #[]\(){}',|`

   ()   List
   []{} Reserved for feature use
   #\   Character constant
   '    Quote
   `    Backquote
   ,    Unquote
   |    Used by code blocks (like multiline comment, symbol name container)

```


Any reserved character you can use as character value using #\ prefix.

```scheme

(print #\|) (print #\#)
; ==> 124
; ==> 35

```


Any reserved character you can use as part of symbol name using | prefix and postfix. You can't use | itself as part of symbol name without using internal libraries.

```scheme

(define |I'm the ,`stra[]ge symbol :))| 123)

(print (put #empty '|I'm the ,`stra[]ge symbol :))| 444))
; ==> #ff((|I'm the ,`stra[]ge symbol :))| . 444))

(print (+ |I'm the ,`stra[]ge symbol :))| 17))
; ==> 140

```



## PARI/GP

{|border="1" style="border-collapse: collapse;"
|<nowiki>\e</nowiki>
|escape
|-
|<nowiki>\t</nowiki>
|tab
|-
|<nowiki>\n</nowiki>
|newline
|}

Any other character that is quoted simply becomes itself.  In particular, <code>\"</code> is useful for adding quotes inside strings.

While not a special character as such, whitespace is handled differently in gp than in most languages.  While whitespace is said to be ignored in free-form languages, it is truly ignored in gp scripts: the gp parser literally removes whitespace outside of strings.  Thus

```PARI/GP
is square(9)
```

is interpreted the same as

```PARI/GP
issquare(9)
```

or even

```PARI/GP
iss qua re(9)
```



### Enclosures


* ( ) function parameter enclosures


## Pascal


* ; statement separator
* . end of program marker
* , declaration separator,function argument separator
* = equality operator
* + addition operator, string concatenation
* - subtraction operator
* * multiplication operator


### Digraphs


* := assignment operator


### Enclosures


* ' ' literal string enclosures
* ( ) function parameter enclosures
* { } comment enclosures


## Perl


Note that in perl quotation operator designations may temporarily change a symbol into an enclosure and the meaning of symbols may vary according to context.


###  Assignment operator symbols 


* = assignment operator 


###  Arithmetic operator symbols 


* + addition (also optional unary positive number designator)
* - subtraction (also negative number designator and named unary operator prefix)
* * multiplication (also used as a prefix sigil for typeglob variables)
* / division (also used as a regularexpression delimiter)
* % modulus (also used as a placeholder prefix)
* ** exponent

Note that perl does not provide an integer division operator, but does support modulus


###  Bitwise operator symbols 


* & bitwise AND operator
* | bitwise OR operator
* ^ bitwise XOR operator
* ~ bitwise NOT operator


###  Comparative operator symbols 


* == numeric equality
* < numeric less than (also used as a readline enclosure)
* > numeric greater than (also used as a readline enclosure)
* <= numeric less than or equal to
* >= numeric greater than or equal to
* <> numeric inequality (also used for readline input)
* <=> numeric tristate comparative
* ~~ smartmatch operator


###  Comment markers 


* # prefixes comments


###  Context switching operators 


* =()= Array context operator
* ~~ String context operator
* -+- Convert numerical prefix to numerical context
* 0+ Numerical context prefix


###  Data type indicators 


* $ prefix sigil and prototyping placeholder for scalar variables (also used as a placeholder modifier for element reordering)
* @ prefix sigil and prototyping placeholder for array variables
* % prefix sigil and prototyping placeholder for associative container variables
* & prefix sigil and prototyping placeholder for subroutine names (also used as a bitwise AND operator)
* * prefix sigil and prototyping placeholder for typeglob variables


###  Enclosures 


* ' Literal string enclosures
* " Interpolated string enclosures
* / Regular expression enclosures
* ( ) Overriding precedence, list construction, control element enclosures, treat functions as terms rather than operators
* [ ] Array reference enclosure, Array definition structure
* < > Readline enclosures
* { } Group statements together into blocks of code, literal character representation construct
* $( ) Dereferencing enclosures
* @{[ ]} Interpolates enclosed array inside a string


###  Escape sequences 


These escape sequences can be used in any construct with interpolation. See [http://perldoc.perl.org/perlop.html#Quote-and-Quote-like-Operators Quote-and-Quote-like-Operators] for more info.
{|border="1" style="border-collapse: collapse;"
| \t || tab (HT,TAB) || 
|-
| \n || newline (NL) || 
|-
| \r || carriage return (CR) || 
|-
| \f || form feed (FF) || 
|-
| \b || backspace (BS) || 
|-
| \a ||	alarm (BEL) || 
|-
| \e || escape (ESC) || 
|-
| \0?? || octal char || example: \033 (ESC)
|-
| \x?? || hex char || example: \x1b (ESC)
|-
| \x{???} || wide hex char || example: \x{263a} (SMILEY)
|-
| \c? || control char || example: \c[ (ESC)
|-
| \N{U+????} || Unicode character || example: \N{U+263D} (FIRST QUARTER MOON)
|-
| \N{????} || named Unicode character || example: \N{FIRST QUARTER MOON}, see [http://perldoc.perl.org/charnames.html charnames]
|}


###  Here document allocation 


* << The double open chevron symbol may be used to allocate [[Here_document|here documents]]


###  Nudge operators 


* ++ incremental nudge operator
* -- decremental nudge operator
* ~- decremental nudge (positive numbers only)


###  Shift operators 


* << bitshift left (dyadic) (also here document allocation)
* >> bitshift right (dyadic)


###  Combination assignment operators 



### = Arithmetic Combination Assignment Operators =


* += addition
* -= subtraction
* *= multiplication
* /= division
* **= exponent
* %= modulus


### = String Manipulation Combination Assignment Operators =


* x= repetition
* .= concatenation


### = Shift Combination Assignment Operators =


* <<= Binary Shift Left
* >>= Binary Shift Right


### = Logical Combination Assignment Operators =


* ||= OR
* &&= AND


### = Bitwise Combination Assignment Operators =


* |= BWOR
* &= BWAND
* ^= BWXOR

=== Ellipsis, Range, Flip Flop, Concatenation, Repetition ===

* . concatenation (also regular expression wildcard)
* x repetition operator
* .. range or flipflop operator, depends on context
* ... ellipsis operator


###  Quoting Operators 


* q literal string enclosure designator
* qq interpolated string enclosure designator
* qr regular expression enclosure designator
* qw word list enclosure designator
* qx external command enclosure designator


###  Referencing and dereferencing operators 


* \ referencing operator (also escape sequencing prefix, and regular expression symbol grouping)
* $$ dereferencing operator
* -> dereferencing and associative container lookup


###  Regular expression symbols 


* / modifier and delimiter
* =~ regular expression binding operator
* ~ regular expression negation operator
* [ ] match box enclosure
* [^ ] compliment box enclosure
* \ symbol grouping prefix character
* . wildcard
* ( ) grouping subexpressions, phrase enclosure, marked subexpression definition, negation operation enclosures
* (?: ) non backreferenced grouping enclosures
* (?= ) positive lookahead enclosures
* (?! ) negative lookahead enclosures
* (?<= ) positive lookbehind enclosures
* (?<! ) negative lookbehind enclosures
* (? ) enforcement or negation operation enclosures
* + repetition operator
* * repetition operator
* | alternation operator
* , count separator
* $ anchor


###  Special variables 


* $. sequence number
* $, output field separator
* $;
* $_ [[Topic_variable|default variable]]
* $" alternative output field separator
* $# output specifier for formatted numbers
* $*
* $! autoflush flag
* $+ last substring matched to a regular expression subpattern
* $0
* $/
* $\
* $|
* $& string matched by last regular expression
* $' substring following last matched regular expression
* $` substring preceding last matched regular expression
* %ENV associative container holding the environment variables
* %SIG
* @+
* @-
* @ARGV array containing the command line parameters
* @F
* @INC library search path

=== Statement, argument and element separators ===

* ; end of statement marker
* , function argument separator, list element separator


###  Ternary operators 


* ? , : The hook and colon are used together to produce ternary operation syntax


###  Miscellaneous symbols 


* <BR />


## Perl 6

Technically speaking, all characters are special in Perl 6, since
they're all just the result of particular mixes of parse rules.
Nevertheless, some characters may appear to be more special than
others.  (However, we will not document any operators here, which
contain plenty of odd characters in Perl 6.)

Sigils:


```txt
    $   Item
    @	Positional
    %	Associative
    &	Callable
```


Twigils may occur only after a sigil, and indicate special scoping:


```txt
    *	Dynamically scoped
    ?	Compile-time constant
    ^	Positional placeholder
    :	Named placeholder
    !	Private attribute
    .	Public attribute/accessor
    ~	Slang
    =	Pod data
    <	Named match from $/
```


Quote characters:


```txt
    ''	Single quotes
    ""	Double quotes
    //	Regex
    ｢｣	Quotes that allow no interpolation at all
    <>	Quote words
    «»	Shell-style words
```


Escapes within single quotes:


```txt
    \\	Backslash
    \'	Quote char
```


Escapes within double quotes:


```txt
    {}	Interpolate results of block
    $	Interpolate item
    @	Interpolate list (requires postcircumfix)
    %	Interpolate hash (requires postcircumfix)
    &	Interpolate call (requires postcircumfix)
    \\	Backslash
    \"	Quote char
    \a	Alarm
    \b	Backspace
    \c	Decimal, control, or named char
    \e	Escape
    \f	Formfeed
    \n	Newline
    \o	Octal char
    \r	Return
    \t	Tab
    \x	Hex char
    \0	Null
```


Escapes within character classes and translations include most of the double-quote backslashes, plus:


```txt
    ..	range
```


Escapes within regexes include most of the double-quote escapes, plus:


```txt
    :	Some kind of declaration
    <>	Some kind of assertion
    []	Simple grouping
    ()	Capture grouping
    {}	Side effect block
    .	Any character
    \d	Digit
    \w	Alphanumeric
    \s	Whitespace
    \h	Horizontal whitespace
    \v	Vertical whitespace
    ''	Single quoted string
    ""	Double quoted string
    «	Word initial boundary
    »	Word final boundary
    ^	String start
    ^^	Line start
    $	String end
    $$	Line end
```


Note that all non-alphanumeric characters are reserved for escapes and operators in Perl 6 regexes.

Any lowercase backslash escape in a regex may be uppercased to negate it, hence <tt>\N</tt> matches anything that is not a newline.

## Phix

In terms of special characters, Phix is pretty much the polar opposite of languages like Perl, APL, and J, and needs a touch fewer brackets than C-based languages (and obviously far fewer than lisp-based languages).

The following are taken directly from the Phix.syn (syntax colouring) file, which can be edited as needed (for errors or new compiler features): 

 Delimiters #$:.%\^
 Operators , = := == != < <= > >= @= @== @!= @< @<= @> @>= + - * / += -= *= /= @+= @-= @*= @/= .. & &= ? ; : |
 Braces ()[]{}
 BlockComment /* */ --/* --*/
 LineComment --
 TokenStart abcedfghijklmnopqrstuvwxyz
 TokenStart ABCDEFGHIJKLMNOPQRSTUVWXYZ_
 TokenChar 0123456789
 Escapes \rnt\'"eE#x0buU
The last line means that escapes in string literals start with a backslash, and there are 14 of them: CR, LF, TAB, backslash, single and double quotes, escape (#1B, e and E allowed), hex byte (# and x allowed), NUL, backspace, and 4 and 8-digit unicode characters.


Taking the others in order:

 # hex literal, or #iXXX compiler directive (see [[[[Pragmatic_directives#Phix|Pragmatic_directives]]]])
 $ roughly means "end". For instance s[2..$] is equivalent to s[2..length(s)]. 
 Can also optionally terminate declarations, eg integer a,b,c and integer a,b,c,$ are equivalent.
 : namespace qualification, for example arwen:hiWord() means the one in arwen, not some other hiWord(). See also :=
 . decimal separator, or part of .. Note there is no dot notation in Phix, such as this.that.theother.
 % deprecated. Was once used for things like %isVar, nowadays it is illegal.
 \ outside strings, the only other place this can be used is as part of a path in an include statement.
 ^ illegal. I think it is only in the syntax file to stop error files from being painful on the eyes.
 , argument and sequence element separator
 = assignment or equality operator, depending on context
 := assignment operator, also used for named parameters
 == equality operator. := and == are just slightly more explicit forms of =
 != < <= > >= standard comparison operators
 @ roughly means "all", as above but apply to an entire sequence (rarely used)
 + - * / standard maths operators
 += -= *= /= ditto, with assignment/implied operand
 @+= @-= @*= @/= as above but applying to an entire sequence (ditto)
 .. slice, for example s[4..6] is three elements of s
 & concatenation operator, eg "this"&"that" is "thisthat"
 &= ditto, with assignment/implied operand
 ? debug print shorthand
 ; (optional) statement separator
 : already described
 | illegal. I think it is only in the syntax file to stop profile listings from being painful on the eyes.
 () parameter delimiters and precedence override
 [] subscripts
 {} sequence formation


## PicoLisp


```txt
Markup:
   () []    List
   .        Dotted pair (when surounded by white space)
   "        Transient symbol (string)
   {}       External symbol (database object)
   \        Escape for following character
   #        Comment line
   #{ }#    Comment block


Read macros:
   '        The 'quote' function
   `        Evaluate and insert a list element
   ~        Evaluate and splice a partial list
   ,        Indexed reference

Within strings:
   ^        ASCII control character
   \        At end of line: Continue on next line, skipping white space
```



## plainTeX


[[TeX]] attachs to each character a category code, that determines its "meaning" for TeX. Macro packages can redefine the category code of '''any''' character. Ignoring the category code 10 (blank), 11 (letters) and 12 (a category embracing all characters that are not letters nor "special" characters according to TeX) and few more not interesting here, when TeX begins the only characters that have a category code so that we can consider "special" for the purpose of this page, are

* \ %

Then [[plainTeX]] assigns few more (here I don't list some non-printable characters that also get assigned a "special" category code)

* { } $ & # ^ _ ~

and these all are "inherited" by a lot of other macro packages (among these, LaTeX).



## PL/I


PL/I has no escape characters as such.
However, in string constants, enclosed in apostrophes or (since PL/I for OS/2) quotation marks, a single apostrophe/quote in the string
must be duplicated, thus:

```PL/I
'John''s pen' which is stored as <<John's pen>>
"He said ""Go!"" and opened the door" which is stored as <<He said "Go!" and opened the door>>
```

Of course, in either of the above the string can be enclosed with the "other" delimiter and no duplication is required.


## PowerShell


PowerShell is unusual in that it retains many of the escape sequences of languages descended from C, except that unlike these languages it uses a backtick ` as the escape character rather than a backslash \. For example `n is a new line and `t is a tab.


## Progress


* . - End of statement marker
* @ - (alternative ;&)
* [ - (alternative ;<)
* ] - (alternative ;>)
* ^ - (alternative ;*)
* ` - (alternative ;')
* { - (alternative ;()
* | - (alternative ;%)
* } - (alternative ;))
* ~ - (alternative ;?)


## PureBasic

There is no escape sequences in character literals. Any character supported by the source encoding is allowed and to insert the quote (“) sign either the constant #DOUBLEQUOTE$ or the its Ascii-code can be used. 

The code is based on readable words and only a semicolon (;) as start-of-comment & a normal colon (:) as command separator are used.

```PureBasic
a=1             ; The ';' indicates that a comment starts
b=2*a:  a=b*33  ; b will now be 2, and a=66
```



## Python

(From [http://docs.python.org/reference/lexical_analysis.html#literals the Python Documentation]):

Unless an <tt class="docutils literal"><span class="pre">'r'</span></tt> or <tt class="docutils literal"><span class="pre">'R'</span></tt> prefix is present, escape sequences in strings are
interpreted according to rules similar to those used by Standard C.  The
recognized escape sequences are:

<table class="docutils" border="1">


<tr><th class="head">Escape Sequence</th>
<th class="head">Meaning</th>
<th class="head">Notes</th>
</tr>


<tr><td><tt class="docutils literal"><span class="pre">\newline</span></tt></td>
<td>Ignored</td>

<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\\</span></tt></td>
<td>Backslash (<tt class="docutils literal"><span class="pre">\</span></tt>)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\'</span></tt></td>
<td>Single quote (<tt class="docutils literal"><span class="pre">'</span></tt>)</td>
<td> </td>

</tr>
<tr><td><tt class="docutils literal"><span class="pre">\"</span></tt></td>
<td>Double quote (<tt class="docutils literal"><span class="pre">"</span></tt>)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\a</span></tt></td>
<td>ASCII Bell (BEL)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\b</span></tt></td>

<td>ASCII Backspace (BS)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\f</span></tt></td>
<td>ASCII Formfeed (FF)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\n</span></tt></td>
<td>ASCII Linefeed (LF)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\N{name}</span></tt></td>

<td>Character named <em>name</em> in the
Unicode database (Unicode only)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\r</span></tt></td>
<td>ASCII Carriage Return (CR)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\t</span></tt></td>
<td>ASCII Horizontal Tab (TAB)</td>

<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\uxxxx</span></tt></td>
<td>Character with 16-bit hex value
<em>xxxx</em> (Unicode only)</td>
<td>(1)</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\Uxxxxxxxx</span></tt></td>
<td>Character with 32-bit hex value
<em>xxxxxxxx</em> (Unicode only)</td>

<td>(2)</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\v</span></tt></td>
<td>ASCII Vertical Tab (VT)</td>
<td> </td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">\ooo</span></tt></td>
<td>Character with octal value
<em>ooo</em></td>
<td>(3,5)</td>
</tr>

<tr><td><tt class="docutils literal"><span class="pre">\xhh</span></tt></td>
<td>Character with hex value <em>hh</em></td>
<td>(4,5)</td>
</tr>

</table>
<p>Notes:</p>
<ol class="arabic simple">
<li>Individual code units which form parts of a surrogate pair can be encoded using
this escape sequence.</li>
<li>Any Unicode character can be encoded this way, but characters outside the Basic
Multilingual Plane (BMP) will be encoded using a surrogate pair if Python is
compiled to use 16-bit code units (the default).  Individual code units which
form parts of a surrogate pair can be encoded using this escape sequence.</li>

<li>As in Standard C, up to three octal digits are accepted.</li>
<li>Unlike in Standard C, exactly two hex digits are required.</li>
<li>In a string literal, hexadecimal and octal escapes denote the byte with the
given value; it is not necessary that the byte encodes a character in the source
character set. In a Unicode literal, these escapes denote a Unicode character
with the given value.</li></ol>


## Racket


Racket, like many Schemes, has very little "special" syntax.  You can use pretty much anything in identifiers in your code, including exotic Unicode characters.  (As usual in other Lisps, symbols share the same syntax as identifiers.)  Notable exceptions:

* Parentheses: round, square, curly (all equivalent, only required to be balanced)

* Spaces are obviously the usual delimiters

* A period is used for improper pairs and related things, but it is fine if it's in an identifier that has more characters

* The hash character "#" is used as a general mechanism for various new syntaxes, but it is fine to use in the middle of an identifier

* Backslash is used to escape any character, making the above characters possible to use

* Vertical bars can be used as identifier quotations used around it


## REXX


### blanks in digraphs and trigraphs

Digraphs and trigraphs can have imbedded blanks (or whitespace) between the characters, so:

```rexx
  if a¬==b      then say 'not equal'
  if a ¬== b    then say 'not equal'
  if a ¬ = = b  then say 'not equal'
  if a ¬ ,
         = = b  then say 'not equal'
```

are equivalent   (assuming the   '''¬'''   symbol is supported by the REXX interpreter). 

### assignment operator symbol

* '''=''' assignment operator       (There are other methods to assign variables, however.)


### =compound assignment operators=

REXX doesn't support compound assignment operators, so the 
* ''' += '''
* ''' -= '''
* ''' *= '''
* ''' /= ''' 
digraphs (above) aren't legal for assignments in classic REXX.

Note that /= is a valid infix operator in some Rexx implementations meaning 'not equal' as in

```rexx
If a/=b Then Say 'a is not equal to b'
```

Note: the above is not an infix operator for an '''assignment''' as this section is named.


### arithmetic operator symbols

* '''+''' addition
* '''-''' subtraction
* '''*''' multiplication
* '''/''' division
* '''%''' integer division
* '''//''' modulus   (or remainder division)
* '''**''' exponent (only for integer powers)


### unary operator symbols

* '''+''' positive value
* '''-''' negative value
* '''\''' logicial negation
* '''¬''' logical negation (some REXXes)
* '''~''' logical negation (some REXXes)


### comparative operator symbols

* '''=''' equal to
* '''==''' strictly equal to
* '''\=''' not equal to
* '''¬=''' not equal to (some REXXes)
* '''/=''' not equal to (some REXXes)
* '''~=''' not equal to (some REXXes) 
* '''\==''' strictly not equal to
* '''/==''' strictly not equal to
* '''¬==''' strictly not equal to (some REXXes)
* '''~==''' strictly not equal to (some REXXes)
* '''<''' less than
* '''>''' greater than
* '''<=''' less than or equal to
* '''>=''' greater than or equal to
* '''\<''' not less than
* '''¬<''' not less than (some REXXes)
* '''~<''' not less than (some REXXes)
* '''\>''' not greater than 
* '''¬>''' not greater than (some REXXes)
* '''~>''' not greater than (some REXXes)
* '''<>''' not equal to
* '''><''' not equal to 
* '''<<''' strictly less than
* '''>>''' strictly greater than
* '''<<=''' strictly less than or equal to
* '''>>=''' strictly greater than or equal to
* '''\<<''' strictly not less than
* '''¬<<''' strictly not less than (some REXXes)
* '''~<<''' strictly not less than (some REXXes) 
* '''\>>''' strictly not greater than
* '''¬>>''' strictly not greater than (some REXXes)
* '''~>>''' strictly not greater than (some REXXes)
* ··· and others


### logical operator symbols

* '''&''' logical AND
* '''|''' logical OR
* '''&&''' logical XOR
* '''\''' logical not
* '''/''' logical not (some REXXes)
* '''¬''' logical not (some REXXes)
* '''~''' logical not (some REXXes)


### concatenation operator symbol

* '''||''' concatenation (or abuttal)


### expression enclosures

The   '''('''   and   ''')'''   symbols are used as enclosures for expressions to help/clarify the priority/priorities for evaluation expressions in REXX.
* ''' ( '''     is the '''start''' of an expression.
* ''' ) '''     is the  '''end'''  of an expression.

```rexx
  y = (a+b)/(c-d)
  z = ((y-2)/(y+2)) / ((a**2 * b**2)* abs(j))
  p = 2**(2**3)
```
 

===function/subroutine argument enclosures, separators===
The   '''('''   and   ''')'''   symbols are used as enclosures for function arguments in REXX.
* ''' ( '''     is the '''start''' of a list of arguments.
* ''' ) '''     is the  '''end'''  of a list of arguments.
* ''' , '''     are used to separate arguments (if any) or to indicate omitted arguments for functions/subroutines.
Arguments may be omitted. 

```rexx
  tn = time()
  tc = time('C')
   x = strip(y,,'+')
```



### comment enclosures

The   '''/*'''   and   '''*/'''   symbols are used as enclosures for comments in REXX.
* ''' /* '''     is the '''start''' of a comment.
* ''' */ '''     is the  '''end'''  of a comment.
REXX comments may span multiple lines.


Note that REXX supports ''nesting'' of comments, so ''nested'' comments must

have matching opening (start) and closing (end) comment delimiters.


### literal enclosures


### =simple literals=

The   ''' ' '''   and   '''"'''   symbols are used as enclosures for literal (character) strings.
* ''' ' '''     is called an apostrophe   (also called a ''single'' quote)
* ''' " '''     is called a ''double'' quote

```rexx
  a = 'ready, set, go!'
```

or

```rexx
  a = "ready, set, go!"
```
 
To assign a null, two formats that can be used are:

```rexx
  nuttin =''
  nothing=""
```


====apostophe [''' ' '''] duplication====
In string constants (enclosed in ''single'' apostrophes), a literal apostrophe in the string must be duplicated, thus:

```rexx
  yyy = 'John''s pen'
```

which is stored as 

```txt
John's pen

```

An alternate way of expressing the above is:

```rexx
  yyy = "John's pen"
```


====quotation mark [''' " '''] duplication====
In string constants (enclosed in ''double'' quotes), a literal double quote in the string must be duplicated, thus:

```rexx
  jjj = "Quote the Raven, ""Nevermore"""
```

which is stored as

```txt

Quote the Raven, "Nevermore"

```



### =binary literals=

The lowercase   '''b'''   or uppercase   '''B'''   (letter) acts as a literal character notation marker, enabling binary literals to be stored as

character strings by using binary (bit) representation of character codes:

```rexx
  lf = '000001010'b                                      /* (ASCII)       */
  cr = "1101"B                                           /* (ASCII)       */
  greeting = '100100001100101011011000110110001101111'b  /* (ASCII) Hello */
```



### =hexadecimal literals=

The lowercase   '''x'''   or uppercase   '''X'''   (letter) acts as a literal character notation marker, enabling hexadecimal literals to be stored as 

character strings by using hexadecimal representation of character codes (which can be in lower or uppercase):

```rexx
  lf = '0A'x                  /* (ASCII)       */
  lf = 'a'x                   /*same as above. */  
  lf = "a"X                   /*same as above. */
  cr = '0D'x                  /* (ASCII)       */
  greeting = '48656C6C6F'x    /* (ASCII) Hello */
```


====literal character digraphs aren't supported====
The rexx language doesn't support the use of character representation digraphs (escape sequences) using a backslash ['''\'''] symbol.


### nudge operators

REXX doesn't support the use of nudge operators, so the   '''++'''   and   '''--'''   symbols aren't special in REXX other than that they are used as unary prefix operators.

Note that in later versions of the Regina interpreter,   '''--'''   ''may''   be used to introduce a line comment (if the appropriate option is in effect.)

```rexx
something=12 -- an assignment, what else
```


The above usage invalidates negative unary operators for classic Rexx.   In the following:

```rexx
something=--12
```

or the string may not be hardcoded within a single REXX statement:

```rexx
x='--12 ++12 -12.000 +12 12 12. 012 0012 0012.00 1.2E1 1.2e+1 --12e-00 120e-1 ++12 ++12. 0--12 00--12 --12'
  do j=1 for words(x)
  interpret 'something=' word(x,j)
  say 'j=' j  '   x=' x   '   something='something
  end
```

or the expression may be user specified as in:

```rexx
say 'enter an expression:'
parse pull x
interpret 'expression=' x
say 'expression=' expression
```
 
can do completely different assignments [or evaluation(s)], depending what version of (classic) REXX is being used.

The following REXX interpreters (for the 1<sup>st</sup> example assign a   '''12'''   to the variable   '''something''':
* PC/REXX
* Personal REXX
* R4
* REXX/imc
* BREXX
* CTC REXX
* CRC REXX
* OS/2 REXX
* KEXX
* OS/400 REXX
* Regina 3.3 and earlier
* Regina 3.4 and later if the option is in effect: ''noSingle_line_comments''
* CMS REXX
* TSO REXX
* REXX compiler (IBM)
* and others.
-- [[User:Gerard Schildberger|Gerard Schildberger]] 20:57, 17 February 2013 (UTC)

===end-of-statement symbol===
Normally, the ''end-of-line'' (or ''end-of-program'') is assumed the end of a REXX statement, 

but multiple REXX statements can be used (on one line) by separating them with a semicolon   [''';'''].

```rexx
  x=5;  y=6;z=y**2;    say x y z
```



### continuation symbol

A REXX statement can be continued on the next line by appending a comma [''','''] as the 

last significant symbol on the line to be continued.

```rexx
  say 'The sum of the first three numbers in the file "INSTANCE.INPUTS" is',
 a b c ' and the total of all three is' grandTotal
```


```rexx
  say 'The sums of the first three numbers in the file "INSTANCE.INPUTS" is',  /*tell sums.*/
 a b c ' and the total of all three is' grandTotal
```



### statement label

A REXX statement label which can be "jumped to/branched to" by a
* '''signal'''
* '''call'''
* ''invoked'' as a function   x='''func'''(y+4)
A REXX label is any valid REXX symbol followed by a colon [''':'''] with or without leading/intervening/trailing blanks.


### argument separator

To separate the arguments of a ''subroutine'', ''function'', or '''BIF''' (built-in function) calls/invocations, 

the comma   [''',''']   is used.

```rexx
  secondChar = substr(xxx, 2, 1)
  thirdChar  = substr(xxx,3,1)
  pruned     = strip(zebra, 'Trailing', ".")      /*remove trailing period(s).*/
```

Note that a comma is also used for continuation if it is the last significant character on a REXX statement   (see ''continuation character'' above).

Also note that REXX only examines the 1<sup>st</sup> character of the (''trailing'') option, and that the ''case'' of the letter is irrelevant.    




===period [.]===
A period   '''[.]'''   can be used for:
* a decimal point   (in a number)
* as part of a label   (a label can start and/or end with one or more periods)
* as part of a variable name   (a variable can't start with a period, but it can end with one or more periods)
* as a placeholder in a ''parsing'' template to indicate that one ''token'' is to be ignored (skipped)
* as a generic stemmed array element;   to assign '''all''' elements:   '''K.=2'''   assigns   '''2'''   to all elements in   '''K'''.  
* as a stemmed array index delimiter   (to indicate multiple indexes):   ''' G.2.x = "tuna" '''
* (in Regina) a variable starting with a period can be one of several global scope variables that can't be modified by the programmer.


## Ruby


### Scope and Variable Naming 

* '''@''' Instance variables - when first character in a variable name is "@" the variable is an Instance variable
* '''@@''' Class variables - when first two characters in a variable name are "@@" the variable is an Class variable
* '''$''' Global variables - when first character in a variable name is "$" the variable is an Global variable
* '''_''' Local variables - when first character in a variable name is "_" the variable is an Local variable (Local variables can also start with lowercase letters)
Although letters are not special characters - just for the sake of completeness . . .
* '''[a-z]''' Local variables - when first character in a variable name is a lower case letter the variable is an Local variable (Local variables can also start with "_")
* '''[A-Z]''' Constants - If it looks like a variable and it starts with an uppercase letter, is is actually a Constant


### assignment operator symbol

* '''=''' assignment operator


### here document markers

* '''<<''' here document marker
* '''<<-''' alternative here document marker
===Variable Length Argument List, Asterisk Operator===
* '''*''' The last parameter of a method may be preceded by an asterisk(*), which is sometimes called the 'splat' operator. This indicates that more parameters may be passed to the function. Those parameters are collected up and an array is created.

### Unary Operators

A unary operator is an operator with only one operand, i.e. a single input.
Ruby borrows heavily from Perl and C - the unary operators often (in some cases) are similar.
Search online for "The Strange Ruby Splat", "ruby operators" or "Ruby's Unary Operators and How to Redefine Their Functionality".
Also Search for the alternate names given below.
* '''-''' unary minus, calls "-@" method
* '''+''' unary plus, calls "+@" method
* '''!''' unary logical not, unary bang, Logical NOT Operator, calls the "!" method
* '''~''' unary binary not, Binary Ones Complement Operator, unary tilde, tilde operator, bitwise NOT, calls the "~" method
* '''*''' unary asterisk, unary splat, RHS convert to a list, LHS assign to an array, calls the "to_a" or "to_ary" method
* '''&''' unary ampersand, unary amper, calls "to_proc" method


## Scala

The most of Java special characters are available in Scala. The big difference is they are not built in the compiler but defined in the appropriate library of classes. Because operators works on classes they are actual methods of that classes. Example: 
```Scala>val n = 1 + 2</lang
This is interpreted as "1" is of class Int and use the method "+" with parameter "2". (Donn't worry, later this will be unboxed to e.g. native JVM primitives.)

This flexible approach gives the possibility to define and redefine (overridden) operators. Sometimes new are invented but the recommendation is to use this with care.


## Seed7

Within character literals and string literals, the backslash is a special character that begins an [http://seed7.sourceforge.net/manual/types.htm#escape_sequences escape sequence]:

```txt
    audible alert    BEL      \a    backslash    (\)   \\
    backspace        BS       \b    apostrophe   (')   \'   
    escape           ESC      \e    double quote (")   \"
    formfeed         FF       \f
    newline          NL (LF)  \n    control-A          \A
    carriage return  CR       \r      ...
    horizontal tab   HT       \t    control-Z          \Z
    vertical tab     VT       \v
```

Additionally the following escape sequences can be used:

* A backslash followed by an integer literal and a semicolon is interpreted as character with the specified ordinal number. Note that the integer literal is interpreted decimal unless it is written as [http://seed7.sourceforge.net/manual/types.htm#based_integer based integer].
* Two backslashes with a sequence of blanks, horizontal tabs, carriage returns and new lines between them are completely ignored. The ignored characters are not part of the string. This can be used to continue a string in the following line. Note that in this case the leading spaces in the new line are not part of the string. Although this possibility exists also for character literals it makes more sense to use it with string literals.

E.g.:

```txt

"\""   "'"   "A\"B !"   "Euro: \8364;"   "CRLF\r\n"

```



## SQL

All characters can be used as identifiers if you put double-quotes around it.

Other than that, the special characters are:
 ' '  String literal
 " "  Identifier
 [ ]  Identifier
 ` `  Identifier
 ?    Numbered host parameter
 :    Named host parameter
 $    Named host parameter
 @    Named host parameter
 ( )  Parentheses
 +    Add
 -    Subtract/negative
 *    Multiply
 /    Divide
 %    Modulo
 &    Bitwise AND
 |    Bitwise OR
 ~    Bitwise NOT
 <<   Left shift
 >>   Right shift
 =    Equal
 ==   Equal
 <>   Not equal
 !=   Not equal
 <    Less
 >    Greater
 <=   Less or equal
 >=   Greater or equal
 ||   String concatenation


## Tcl

As documented in man Tcl, the following special characters are defined:

```Tcl
{...}     ;# group in one word, without substituting content; nests
"..."     ;# group in one word, with substituting content
[...]     ;# evaluate content as script, then substitute with its result; nests
$foo      ;# substitute with content of variable foo
$bar(foo) ;# substitute with content of element 'foo' of array 'bar'
\a        ;# audible alert (bell)
\b        ;# backspace
\f        ;# form feed
\n        ;# newline
\r        ;# carriage return
\t        ;# Tab
\v        ;# vertical tab
\\        ;# backslash
\ooo      ;# the Unicode with octal value 'ooo'
\xhh      ;# the character with hexadecimal value 'hh'
\uhhhh    ;# the Unicode with hexadecimal value 'hhhh'
#         ;# if first character of a word expected to be a command, begin comment
          ;# (extends till end of line)
{*}       ;# if first characters of a word, interpret as list of words to substitute,
          ;# not single word (introduced with Tcl 8.5)
```



## TXR

Text not containing the character <code>@</code> is a TXR query representing a match that text.
The sequence <code>@@</code> encodes a single literal <code>@</code>.

All other special syntax is introduced by @:

* <code>@</code># comment
* <code>@\n</code> # escaped character, embedded into surrounding text. Similar to C escapes, with <code>\e</code> for ASCII ESC. 
* <code>@\x1234 @\1234</code> Hex or octal escapes: Unicode width, not byte.
* <code>@symbol</code> variable ref
* <code>@*symbol</code> variable ref with longest match semantics
* <code>@{symbol <i>expr</i> ...}</code> variable ref extended syntax
* <code>@<i>expr</i></code> directive

Where <i>expr</i> is Lispy syntax which can be an atom, or a list of atoms or lists in parentheses, or possibly a dotted list (terminated by an atom other than nil):

* <code>(elem1 elem2 ... elemn)</code>  proper
* <code>(elem1 elem2 ... elemn . atom)</code> dotted

Atoms can be:

* <code>ABc123_4</code> symbols, represented by tokens consisting of letters, underscores and digits, beginning with a letter. Symbols have packages, e.g., system:foo, but this is not accessible from the TXR lexical conventions.
* <code>:FoO42</code> keyword symbols, denoted by colon, which is not part of the symbol name.
* <code>"string literals"</code>
* <code>`quasi @literals`</code> with embedded @ syntax
* <code>'c'</code> characters
* <code>123</code> integers
* <code>/reg/</code> regular expressions

Within literals and regexes:

* <code>\r</code>  various backslash escapes similar to C
* <code>\\</code>  single backslash

Within literals, quasiliterals and character constants:

* <code>\' \" \`</code>  escape any of the quotes: not available within regex.

The regex syntax is fairly standard fare, with these extensions:

* <code>~R</code>   complement of R: set of strings other than those that match R
* <code>R%S</code>  match shortest number of repetitions of R prior to S.
* <code>R&S</code>  match R and S simultaneously: the intersection of the set of strings matching S and the set matching R.
* <code>[]</code>   empty class; match nothing, not even the empty string.


## UNIX Shell


The Bourne shell treats the following as special characters:

* # comment marker
* ! logical not (within a test command), compliment box operator
* $ variable referencing prefix
* & referencing open file descriptors and background process marker
* * filename and string matching wildcard
* . inclusion command
* / pathname separator
* : parameter expansion and do nothing command
* ; command separator
* = assignment and parameter expansion operator
* \ escape sequence prefix
* ? wildcard metacharacter
* ) switch conditional component
* ` external command enclosure
* | pipeline connector
* - parameter expansion operator
* + parameter expansion operator


### Digraphs


* *) switch conditional component
* #! hashbang
* ;; switch conditional component
* << here document operator
* >> append redirection operator
* $* single element command line expansion special variable
* $@ multiple element command line expansion special variable
* $# number of command line parameters special variable
* $? special variable holding return value of last operation
* :- parameter expansion operator
* := parameter expansion operator
* :+ parameter expansion operator
* :? parameter expansion operator


### Enclosures


* " " interpolated string enclosures
* ' ' non interpolated string enclosures
* [ ] test command substitute and character range enclosures
* ( ) subshell execution enclosures, empty for function declaration
* `( )` external subshell execution enclosures
* [! ] compliment box enclosures
* { } code block enclosures and variable name isolation enclosures
* ${ } variable name isolation enclosures


### Extended shell features


The Korn shell, Bourne Again Shell and Posix shell provide the following additional special characters:

* - korn shell unary arithmetic operator
* { brace expansion marker
* ~ home directory expansion operator
* && Extended syntax for execute if true (on success)
* || Extended syntax for execute if false (on failure)
* -- Extended syntax marker for end of command line switches
* == bash specific feature


### =Extended shell enclosures=


* $( ) Extended syntax for external command capture construct
* [[ ]] bash specific feature
* (( )) arithmetic expansion enclosures


## XSLT

XSLT is based on XML, and so has the same special characters which must be escaped using character entities:
* & - &amp;amp;
* < - &amp;lt;
* > - &amp;gt;
* " - &amp;quot;
* ' - &amp;apos;

Any Unicode character may also be represented via its decimal code point (&amp;#nnnn;) or hexadecimal code point (&amp;#xdddd;).


## zkl

*In a string: The C ones: \r \n \t \f \e \b \ (the escape)
*//, #, /* */ comments, no escaping
*C-like math & logic: == != > < <= => + - * / += -= /= */ and or 
*Block scope: { }
*Brackets([]): Subscripting, range([0..10]), attributes(class [static] foo {})
*Function/method call: name(...)
*Method access: .method
*Compose this chunk of stuff in that chunk of stuff: :(colon)
*List comprehension: [[ ]], [& ]]
*Ops (for use in function calls): '+ '- '* '/ '> '>= '< '<=
*Closures: 'wrap
*Comma(,): separate parameters, list assignment
*Star(*): rest of, forever: [0..*], [1,*]
*Underline(_): valid in a name but also used as a throw away: a,_,c=...

[[Category: Syntax elements]]

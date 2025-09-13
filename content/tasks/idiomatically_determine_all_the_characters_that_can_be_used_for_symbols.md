+++
title = "Idiomatically determine all the characters that can be used for symbols"
description = ""
date = 2019-09-09T22:26:00Z
aliases = []
[extra]
id = 17428
[taxonomies]
categories = ["task"]
tags = []
+++

Idiomatically determine all the characters that can be used for ''symbols''.
The word ''symbols'' is meant things like names of variables, procedures (i.e., named fragments of programs, functions, subroutines, routines), statement labels, events or conditions, and in general, anything a computer programmer can choose to ''name'', but not being restricted to this list. ''Identifiers'' might be another name for ''symbols''.

The method should find the characters regardless of the hardware architecture that is being used  (ASCII, EBCDIC, or other).

;Task requirements

Display the set of all the characters that can be used for symbols which can be used (allowed) by the computer program. 
You may want to mention what hardware architecture is being used, and if applicable, the operating system.

Note that most languages have additional restrictions on what characters can't be used for the first character of a variable or statement label, for instance.  These type of restrictions needn't be addressed here (but can be mentioned).

## See also

* [[Idiomatically_determine_all_the_lowercase_and_uppercase_letters|Idiomatically determine all the lowercase and uppercase letters]].





## AWK


```AWK

# syntax: GAWK -f IDIOMATICALLY_DETERMINE_ALL_THE_CHARACTERS_THAT_CAN_BE_USED_FOR_SYMBOLS.AWK
BEGIN {
    fn = "TEMP.AWK"
    cmd = sprintf("GAWK -f %s 2>NUL",fn)
    for (i=0; i<=255; i++) {
      c = sprintf("%c",i)
      if (c ~ /\x09|\x0D|\x0A|\x20/) { ng++; continue } # tab,CR,LF,space
      (run(c)     == 0) ? (ok1 = ok1 c) : (ng1 = ng1 c) # 1st character
      (run("_" c) == 0) ? (ok2 = ok2 c) : (ng2 = ng2 c) # 2nd..nth character
    }
    printf("1st character: %d NG, %d OK %s\n",length(ng1)+ng,length(ok1),ok1)
    printf("2nd..nth char: %d NG, %d OK %s\n",length(ng2)+ng,length(ok2),ok2)
    exit(0)
}
function run(c,  rc) {
    printf("BEGIN{%s+=0}\n",c) >fn
    close(fn)
    rc = system(cmd)
    return(rc)
}

```

<p>output:</p>

```txt

1st character: 203 NG, 53 OK ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz
2nd..nth char: 193 NG, 63 OK 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz

```

=={{header|F_Sharp|F#}}==
Well, if the purpose of this task is to determine what can be used as an identifier then in F# anything so long as you enclose it in double backticks so:

```fsharp

let ``+`` = 5
printfn "%d" ``+``

```

```txt

5

```

Is this idiotmatically determined?


## Factor

```factor
USING: parser see ;
\ scan-word-name see
```

```txt

: scan-word-name ( -- string )
    scan-token dup "\"" = [ t ] [ dup string>number ] if
    [ invalid-word-name ] when ;

```

From this code we can see that any characters may be used in an identifier unless it parses as a string or a number.


## Go


Most of the code is concerned with printing the Unicode ranges of the valid characters. The remaining part of the code parses the possible identifier and verifies that it is indeed an identifier.


```go
package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"strings"
	"unicode"
)

type runeRanges struct {
	ranges   []string
	hasStart bool
	start    rune
	end      rune
}

func (r *runeRanges) add(cp rune) {
	if !r.hasStart {
		r.hasStart = true
		r.start = cp
		r.end = cp
		return
	}

	if cp == r.end+1 {
		r.end = cp
		return
	}

	r.writeTo(&r.ranges)

	r.start = cp
	r.end = cp
}

func (r *runeRanges) writeTo(ranges *[]string) {
	if r.hasStart {
		if r.start == r.end {
			*ranges = append(*ranges, fmt.Sprintf("%U", r.end))
		} else {
			*ranges = append(*ranges, fmt.Sprintf("%U-%U", r.start, r.end))
		}
	}
}

func (r *runeRanges) String() string {
	ranges := r.ranges
	r.writeTo(&ranges)
	return strings.Join(ranges, ", ")
}

func isValidIdentifier(identifier string) bool {
	node, err := parser.ParseExpr(identifier)
	if err != nil {
		return false
	}
	ident, ok := node.(*ast.Ident)
	return ok && ident.Name == identifier
}

func main() {
	var validFirst runeRanges
	var validFollow runeRanges
	var validOnlyFollow runeRanges

	for r := rune(0); r <= unicode.MaxRune; r++ {
		first := isValidIdentifier(string([]rune{r}))
		follow := isValidIdentifier(string([]rune{'_', r}))
		if first {
			validFirst.add(r)
		}
		if follow {
			validFollow.add(r)
		}
		if follow && !first {
			validOnlyFollow.add(r)
		}
	}

	_, _ = fmt.Println("Valid first:", validFirst.String())
	_, _ = fmt.Println("Valid follow:", validFollow.String())
	_, _ = fmt.Println("Only follow:", validOnlyFollow.String())
}
```

```txt

Valid first: U+0041-U+005A, U+005F, U+0061-U+007A, U+00AA, ..., U+00F8-U+02C1, U+02C6-U+02D1, ...
Valid follow: U+0030-U+0039, U+0041-U+005A, U+005F, U+0061-U+007A, U+00AA, ..., U+00F8-U+02C1, ..., U+2CEB0-U+2EBE0, U+2F800-U+2FA1D
Only follow: U+0030-U+0039, U+0660-U+0669, U+06F0-U+06F9, U+07C0-U+07C9, ..., U+1D7CE-U+1D7FF, U+1E950-U+1E959

```



## J


J is defined in terms of ascii, but that would not prevent it from being ported to other environments. But we can still use J's parser to determine if a specific character combination is a single, legal word:


```J
   a.#~1=#@;: ::0:"1 'b',.a.,.'c'
0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz
```


Here, [http://www.jsoftware.com/help/dictionary/dadot.htm a.] is the set of chararacters we are testing. We prefix each of these with an arbitrary letter, and suffix each with an arbitrary character and then try counting how many parsed tokens are formed by the result. If the token count is 1, then that character was a legal word-forming character.

Of course, we also only need to do this once. Once we have a set of these characters, it's faster and easier to use a set membership test on the characters themselves than on the expression which generates them.


## Java

```java
import java.util.function.IntPredicate;
import java.util.stream.IntStream;

public class Test {
    public static void main(String[] args) throws Exception {
        print("Java Identifier start:     ", 0, 0x10FFFF, 72,
                Character::isJavaIdentifierStart, "%c");

        print("Java Identifier part:      ", 0, 0x10FFFF, 25,
                Character::isJavaIdentifierPart, "[%d]");

        print("Identifier ignorable:      ", 0, 0x10FFFF, 25,
                Character::isIdentifierIgnorable, "[%d]");

        print("Unicode Identifier start:  ", 0, 0x10FFFF, 72,
                Character::isUnicodeIdentifierStart, "%c");

        print("Unicode Identifier part :  ", 0, 0x10FFFF, 25,
                Character::isUnicodeIdentifierPart, "[%d]");
    }

    static void print(String msg, int start, int end, int limit, 
        IntPredicate p, String fmt) {

        System.out.print(msg);
        IntStream.rangeClosed(start, end)
                .filter(p)
                .limit(limit)
                .forEach(cp -> System.out.printf(fmt, cp));
        System.out.println("...");
    }
}
```



```txt
Java Identifier start: $ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz¢£¤¥ªµºÀÁÂÃÄÅÆÇÈÉÊ...
Java Identifier part: [0][1][2][3][4][5][6][7][8][14][15][16][17][18][19][20][21][22][23][24][25][26][27][36][48]...
Java Identifier ignorable: [0][1][2][3][4][5][6][7][8][14][15][16][17][18][19][20][21][22][23][24][25][26][27][127][128]...
Unicode Identifier start: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzªµºÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐ...
Unicode Identifier part: [0][1][2][3][4][5][6][7][8][14][15][16][17][18][19][20][21][22][23][24][25][26][27][48][49]...
```



## jq


### jq identifiers

Excluding key names from consideration, in jq 1.4 the set of characters that can be
used in jq identifiers corresponds to the regex: [A-Za-z0-9$_].
Thus, assuming the availability of test/1 as a builtin, the test in jq
for a valid identifier character is: test("[A-Za-z0-9$_]").

To generate a string of such characters idiomatically:

```jq
[range(0;128) | [.] | implode | select(test("[A-Za-z0-9$_]"))] | add
```


jq 1.5 also allows ":" as a joining character in the form "module::name".



### JSON key names

Any JSON string can be used as a key.  Accordingly,
some characters must be entered as escaped character sequences,
e.g. \u0000 for NUL, \\ for backslash, etc.  Thus any Unicode character
except for the control characters can appear in a jq key.
Therefore, assuming the availability in jq of the test/1 builtin, the test 
in jq for whether a character can appear literally in a jq identifier or key is:

```jq
test("[^\u0000-\u0007F]")
```



### Symbols

The following function screens for characters by "\p" class:

```jq
def is_character(class):
   test( "\\p{" + class + "}" );
```

For example, to test whether a character is a Unicode letter, symbol or numeric character:

```jq
is_character("L") or is_character("S") or is_character("N")
```


An efficient way to count the number of Unicode characters within a character class is
to use the technique illustrated by the following function:

```jq
def count(class; m; n):
  reduce (range(m;n) | [.] | implode | select( test( "\\p{" + class + "}" ))) as $i
    (0; . + 1);
```


For example the number of Unicode "symbol" characters can be obtained by evaluating:

```jq
count("S"; 0; 1114112)
```

The result is 3958.


## Julia

Julia allows almost any Unicode character as a symbol if it is not a reserved 
operator symbol and, if numeric, if it is not the first letter of a symbol name. 

For example, x2 is a valid identifier, but 2x is not-- it is interpreted as 2 times the identifier x. In Julia, the Symbol() function turns a string into a symbolic token. So, for example:

```julia

for i in 1:0x200000 - 1
    Symbol("x" * Char(i))
end

```


When run, this loop runs without error up to 0x200000 but not at Unicode symbol numbered 0x200000.


## Kotlin

According to the Kotlin grammar, the rules regarding which characters can appear in symbols (or identifiers as we usually call them) are the same as in Java, namely:

1. An identifier is a sequence of any number of unicode letters or digits, other than a reserved word.

2. Identifiers are case sensitive.

3. The first character must be a letter, an underscore or a $ sign. Subsequent characters can include digits and certain control characters as well though the latter are ignored for identifier matching purposes.  

However, in practice, identifiers which include a $ symbol or control characters don't compile unless (in the case of $) the entire identifier is enclosed in back-ticks. The use of this device also allows one to use a reserved word or many otherwise prohibited unicode characters in an identifier including spaces and dashes.

A Kotlin label name is a valid identifier followed by an @ symbol and an annotation name is an identifier preceded by an @ symbol.

```scala
// version 1.1.4-3

typealias CharPredicate = (Char) -> Boolean

fun printChars(msg: String, start: Int, end: Int, limit: Int, p: CharPredicate, asInt: Boolean) {
    print(msg)
    (start until end).map { it.toChar() }
                     .filter { p(it) }
                     .take(limit)
                     .forEach { print(if (asInt) "[${it.toInt()}]" else it) }
    println("...")
}

fun main(args: Array<String>) {
    printChars("Kotlin Identifier start:     ", 0, 0x10FFFF, 72,
                Char::isJavaIdentifierStart, false)

    printChars("Kotlin Identifier part:      ", 0, 0x10FFFF, 25,
                Character::isJavaIdentifierPart, true)

    printChars("Kotlin Identifier ignorable: ", 0, 0x10FFFF, 25,
                Character::isIdentifierIgnorable, true)
}
```


```txt

Kotlin Identifier start:     $ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz¢£¤¥ªµºÀÁÂÃÄÅÆÇÈÉÊ...
Kotlin Identifier part:      [0][1][2][3][4][5][6][7][8][14][15][16][17][18][19][20][21][22][23][24][25][26][27][36][48]...
Kotlin Identifier ignorable: [0][1][2][3][4][5][6][7][8][14][15][16][17][18][19][20][21][22][23][24][25][26][27][127][128]...

```



## Ol

Absolutely any Unicode or ANSI character can be used as part of symbol name. There only some limitations in form of symbol declaration.

1. Direct symbol declaration (in form of quote or ') must not be started from control codes (first 32 characters), numbers and @. Next characters in symbol must not be control code neither @.

2. Direct symbol declaration (in form of ||) must not contain character |.

3. Functional symbol creation (in form of string->symbol) have no any limitations.


## ooRexx

Although this program does not use any feature that is not in Classic Rexx, 
it is included here to show what characters are valid for symbols in ooRexx.

```oorexx
/*REXX program determines what characters are valid for REXX symbols.*/
/* copied from REXX version 2                                        */
Parse Version v
Say v
symbol_characters=''                   /* start with no chars        */
do j=0 To 255                          /* loop through all the chars.*/
  c=d2c(j)                             /* convert number to character*/
  if datatype(c,'S') then              /* Symbol char                */
    symbol_characters=symbol_characters || c  /* add to list.        */
  end
say 'symbol characters:' symbol_characters /*display all             */
```

```txt
REXX-ooRexx_4.2.0(MT)_32-bit 6.04 22 Feb 2014
symbol characters: !.0123456789?ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz 
```



## PARI/GP

The only symbols that can be used in variable names (including function names as a special case) are a-z, A-Z, 0-9, and the underscore. Additionally, the first character must be a letter. (That is, they must match this regex: <code>[a-zA-Z][a-zA-Z0-9_]*</code>.)

```parigp
v=concat(concat([48..57],[65..90]),concat([97..122],95));
apply(Strchr,v)
```

```txt
%1 = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "_"]
```



## Perl


```perl># When not using the <code>use utf8</code
 pragma, any word character in the ASCII range is allowed.
# the loop below returns: 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz
for $i (0..0x7f) {
    $c = chr($i);
    print $c if $c =~ /\w/;
}

# When 'use utf8' is in force, the same holds true, but the Unicode-aware version of the 'word-character' test is used.
# not supplying output, too much of it
use utf8;
binmode STDOUT, ":utf8";
for (0..0x1ffff) {
    $c = chr($_);
    print $c if $c =~ /\p{Word}/;
}
```



## Perl 6

Any Unicode character or combination of characters can be used for symbols in Perl 6.  Here's some counting rods and some cuneiform:

```perl6
sub postfix:<𒋦>($n) { say "$n trilobites" }

sub term:<𝍧> { unival('𝍧') }

𝍧𒋦
```

```txt
8 trilobites
```


And here is a Zalgo-text symbol:


```perl6
sub Z̧̔ͩ͌͑̉̎A̢̲̙̮̹̮͍̎L̔ͧ́͆G̰̬͎͔̱̅ͣͫO͙̔ͣ̈́̈̽̎ͣ ($n) { say "$n COMES" }


Z̧̔ͩ͌͑̉̎A̢̲̙̮̹̮͍̎L̔ͧ́͆G̰̬͎͔̱̅ͣͫO͙̔ͣ̈́̈̽̎ͣ 'HE'
```

```txt
HE COMES
```


Of course, as in other languages, most of the characters you'll typically see in names are going to be alphanumerics from ASCII (or maybe Unicode), but that's a convention, not a limitation, due to the syntactic category notation demonstrated above, which can introduce any sequence of characters as a term or operator.

Actually, the above is a slight prevarication.  The syntactic category notation does not allow you to use whitespace in the definition of a new symbol. But that leaves many more characters allowed than not allowed.  Hence, it is much easier to enumerate the characters that <em>cannot</em> be used in symbols:

```perl6
say .fmt("%4x"),"\t", uniname($_)
    if uniprop($_,'Z')
        for 0..0x1ffff;
```

```txt
  20	SPACE
  a0	NO-BREAK SPACE
1680	OGHAM SPACE MARK
2000	EN QUAD
2001	EM QUAD
2002	EN SPACE
2003	EM SPACE
2004	THREE-PER-EM SPACE
2005	FOUR-PER-EM SPACE
2006	SIX-PER-EM SPACE
2007	FIGURE SPACE
2008	PUNCTUATION SPACE
2009	THIN SPACE
200a	HAIR SPACE
2028	LINE SEPARATOR
2029	PARAGRAPH SEPARATOR
202f	NARROW NO-BREAK SPACE
205f	MEDIUM MATHEMATICAL SPACE
3000	IDEOGRAPHIC SPACE
```

We enforce the whitespace restriction to prevent insanity in the readers of programs.
That being said, even the whitespace restriction is arbitrary, and can be bypassed by deriving a new grammar and switching to it.  We view all other languages as dialects of Perl 6, even the insane ones.  <tt>:-)</tt>


## Phix

Translation of AWK, extended with separation of ansi and utf8 handling

```Phix
function run(string ident)
    integer fn = open("test.exw","w")
    printf(fn,"object %s",ident)
    close(fn)
    return system_exec("p -batch test.exw")
end function

function check(integer lo, hi)
    string ok1 = "", ok2 = ""
    integer ng1 = 0, ng2 = 0
    for ch=lo to hi do
        printf(1,"%d/%d...\r",{ch,hi})
        if find(ch,"\t\r\n \0\x1A;") then
            ng1 += 1
            ng2 += 1
        else
            string c = sprintf("%c",ch)
            if run(c)==0 then ok1 &= c else ng1 += 1 end if
            if run("_"&c)==0 then ok2 &= c else ng2 += 1 end if
        end if
    end for
    return {{ng1,length(ok1),ok1},
            {ng2,length(ok2),ok2}}
end function

sequence r = check(0,127)
printf(1,"ansi characters:\n
### =========
\n")
printf(1,"1st character: %d bad, %d OK %s\n",r[1])
printf(1,"2nd..nth char: %d bad, %d OK %s\n\n",r[2])
r = check(128,255)
integer ok8 = 0, ng8 = 0
for i=#80 to #10FFFF do
    if i<#D800 or i>#DFFF then
        printf(1,"#%x/#10FFFF...\r",i)
        string utf8 = utf32_to_utf8({i})
        bool ok = true
        if not find(utf8[1],r[1][3]) then
            ok = false
        else
            for j=2 to length(utf8) do
                if not find(utf8[j],r[2][3]) then
                    ok = false
                    exit
                end if
            end for
        end if
        if ok then
            ok8 += 1
        else
            ng8 += 1
        end if
    end if
end for
printf(1,"utf8 characters:   \n
### =========
\n")
printf(1,"bad:%,d, good:%,d\n",{ng8,ok8})
```

```txt

ansi characters:

### =========

1st character: 75 bad, 53 OK ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz
2nd..nth char: 65 bad, 63 OK 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz

utf8 characters:

### =========

bad:0, good:1,111,936

```

Note that versions prior to 0.8.1 only permit a mere 48 utf8 characters, running the same code on 0.7.9 gave me

```txt

utf8 characters:

### =========

bad:1,111,888, good:48

```



## Python

See [[Idiomatically_determine_all_the_lowercase_and_uppercase_letters#Python|String class isidentifier]].


## Racket


[http://docs.racket-lang.org/guide/symbols.html Symbols in the Racket Guide] states that:

<blockquote>Any string (i.e., any character sequence) can be supplied to <code>string->symbol</code> to obtain
the corresponding symbol.</blockquote>

[http://docs.racket-lang.org/reference/reader.html#%28part._parse-symbol%29 Reading Symbols] defines
what symbols can be "read" without needing quoting.

The docuementation for
[http://docs.racket-lang.org/reference/characters.html#%28def._%28%28quote._~23~25kernel%29._integer-~3echar%29%29 <code>integer->char</code>]
says that a character must lie in the ranges: 0 to 55295, and 57344 to 1114111.

That's too much to be printing out here... call <code>(main)</code> yourself, at home.


```racket
#lang racket
;; Symbols that don't need to be specially quoted:
(printf "~s~%" '(a a-z 3rd ...---... .hidden-files-look-like-this))

;; Symbols that do need to be specially quoted:
(define bar-sym-list
  `(|3|
    |i have a space|
    |i've got a quote in me|
    |i'm not a "dot on my own", but my neighbour is!|
    |.|
    ,(string->symbol "\u03bb")
    ,(string->symbol "my characters aren't even mapped in unicode \U10e443")))
(printf "~s~%" bar-sym-list)
(printf "~a~%" bar-sym-list)

(define (main)
  (for
      ((c (sequence-map
           integer->char
           (in-sequences (in-range 0 (add1 55295))
                         (in-range 57344 (add1 1114111)))))
       (i (in-naturals 1)))
    (when (zero? (modulo i 80)) (newline))
    (display (list->string (list c)))))

```


```txt
(a a-z 3rd ...---... .hidden-files-look-like-this)
(|3| |i have a space| |i've got a quote in me| |i'm not a "dot on my own", but my neighbour is!| |.| λ |my characters aren't even mapped in unicode 􎑃|)
(3 i have a space i've got a quote in me i'm not a "dot on my own", but my neighbour is! . λ my characters aren't even mapped in unicode 􎑃)
```

The output to <code>(main)</code> is massive, and probably not dissimilar to Tcl's (anyone want to compare?)


## REXX


### version 1


```rexx
/*REXX program determines what  characters  are valid for REXX symbols. */
@=                                     /*set   symbol characters "   "  */
   do j=0  for 2**8                    /*traipse through all the chars. */
   _=d2c(j)                            /*convert decimal number to char.*/
   if datatype(_,'S')  then @=@ || _   /*Symbol char?  Then add to list.*/
   end   /*j*/                         /* [↑] put some chars into a list*/

say '     symbol characters: '  @      /*display all  symbol characters.*/
                                       /*stick a fork in it, we're done.*/
```

Programming note:   REXX allows any symbol to begin a (statement) label, but variables can't begin with a period ('''.''') or a numeric digit.


All examples below were executed on a (ASCII) PC using Windows/XP and Windows/7 with code page 437 in a DOS window. 


 
Using PC/REXX 

Using Personal REXX 

Using Regina (versions 3.2 ───► 3.82) 

'''output'''

```txt

     symbol characters:  !#$.0123456789?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz

```


Using R4 

'''output'''

```txt

     symbol characters:  !#$.0123456789?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyzÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£áíóúñÑ╡╢╖─╞╟╨╤╥╙╘╒╓╫╪▐αßΓπΣσµτΦΘΩδ∞φ

```


Using ROO 

'''output'''

```txt


     symbol characters:  !#$.0123456789?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyzÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£áíóúñÑ╡╢╖╞╟╨╤╥╙╘╒╓╫╪▐αßΓπΣσµτΦΘΩδ∞φ

```



### version 2 ooRexx compatible
 
Because version 1 does not work correctly with ooRexx - showing this error message:

```txt
     2 *-* @
Error 13 running D:\v1.rex line 2:  Invalid character in program
Error 13.1:  Incorrect character in program "@" ('40'X)

```

I've added version 2 which should work correctly for all Rexx interpreters and compilers

```rexx
/*REXX program determines what characters are valid for REXX symbols.*/
/* version 1 adapted for general acceptance                          */
Parse Version v
Say v
symbol_characters=''                   /* start with no chars        */
do j=0 To 255                          /* loop through all the chars.*/
  c=d2c(j)                             /* convert number to character*/
  if datatype(c,'S') then              /* Symbol char                */
    symbol_characters=symbol_characters || c  /* add to list.        */
  end
say 'symbol characters:' symbol_characters /*display all        */ 

```

{{out}} for some interpreters
Note that $#@ are not valid symbol characters for ooRexx.

```txt
REXX-ooRexx_4.2.0(MT)_32-bit 6.04 22 Feb 2014
symbol characters: !.0123456789?ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz

REXX-Regina_3.8.2(MT) 5.00 22 Jun 2014
symbol characters: !#$.0123456789?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz       

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/ZyPkGW8/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/4XdxscWGTtyw9MDQXCtRdg Scastie (remote JVM)].

```Scala
object IdiomaticallyDetermineSymbols extends App {

  private def print(msg: String, limit: Int, p: Int => Boolean, fmt: String) =
    println(msg + (0 to 0x10FFFF).filter(p).take(limit).map(fmt.format(_)).mkString + "...")

  print("Java Identifier start:    ", 72, cp => Character.isJavaIdentifierStart(cp), "%c")
  print("Java Identifier part:     ", 25, cp => Character.isJavaIdentifierPart(cp), "[%d]")
  print("Identifier ignorable:     ", 25, cp => Character.isIdentifierIgnorable(cp), "[%d]")
  print("Unicode Identifier start: ", 72, cp => Character.isUnicodeIdentifierStart(cp), "%c")
  print("Unicode Identifier part : ", 25, cp => Character.isUnicodeIdentifierPart(cp), "[%d]")

}
```


## Tcl

Tcl permits ''any'' character to be used in a variable or command name (subject to the restriction that <code>::</code> is a namespace separator and, for variables only, a <code>(…)</code> sequence is an array reference). The set of characters that can be used after <code>$</code> is more restricted, excluding many non-letter-like symbols, but still large. It is ''recommended practice'' to only use ASCII characters for variable names as this makes scripts more resistant to the majority of encoding problems when transporting them between systems, but the language does not itself impose such a restriction.

```tcl
for {set c 0;set printed 0;set special {}} {$c <= 0xffff} {incr c} {
    set ch [format "%c" $c]
    set v "_${ch}_"
    #puts "testing variable named $v"
    if {[catch {set $v $c; set $v} msg] || $msg ne $c} {
	puts [format "\\u%04x illegal in names" $c]
	incr printed
    } elseif {[catch {subst $$v} msg] == 0 && $msg eq $c} {
	lappend special $ch
    }
}
if {$printed == 0} {
    puts "All Unicode characters legal in names"
}
puts "Characters legal after \$: $special"
```

Only the first 256 characters are displayed:

```txt
All Unicode characters legal in names
Characters legal after $: 0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _ a b c d e f g h i j k l m n o p q r s t u v w x y z ª µ º À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï Ð Ñ Ò Ó Ô Õ Ö Ø Ù Ú Û Ü Ý Þ ß à á â ã ä å æ ç è é ê ë ì í î ï ð ñ ò ó ô õ ö ø ù ú û ü ý þ ÿ İ ı Ĳ ĳ Ĵ ĵ Ķ ķ ĸ Ĺ Ł ł Ń ń Ņ ņ Ň ň ŉ Ŋ ŋ Ō ō Ŏ ŏ Ő ő Œ œ Ŕ ŕ Ŗ ŗ Ř ř Ś ş š Ţ ţ Ť ť Ŧ ŧ Ũ ũ Ū ū Ŭ ŭ Ů ů Ű ű Ų ų Ŵ ŵ Ŷ ŷ Ÿ Ź ź ƪ Ƶ ƺ ǀ ǁ ǂ ǃ Ǆ ǅ ǆ Ǉ ǈ ǉ Ǌ ǋ ǌ Ǎ ǎ Ǐ ǐ Ǒ ǒ Ǔ ǔ Ǖ ǖ ǘ Ǚ ǚ Ǜ ǜ ǝ Ǟ ǟ Ǡ ǡ Ǣ ǣ Ǥ ǥ Ǧ ǧ Ǩ ǩ Ǫ ǫ Ǭ ǭ Ǯ ǯ ǰ Ǳ ǲ ǳ Ǵ ǵ Ƕ Ǹ ǹ Ǻ ǻ Ǽ ǽ Ǿ ǿ ...
```



## zkl

zkl only supports ASCII, although other character sets might be finessed.

```zkl
[0..255].filter(fcn(n){
   try{ Compiler.Compiler.compileText("var "+n.text) }
   catch{ False }
}).apply("text").concat()
```

```txt

<compiler noise>
;ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz

```

This code works by compiling "var <char>". Since "var ;" is valid syntax (dead code), ";" is a false positive. We could also use "fcn <char>{}" but "fcn {}" is lambda syntax, so space would be a false positive. "_" is excluded because it is not valid variable name although it can be anywhere in a multi-character name.

+++
title = "Comments"
description = ""
date = 2019-10-15T08:29:33Z
aliases = []
[extra]
id = 1996
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "4d",
  "6502_assembly",
  "8086_assembly",
  "acl2",
  "actionscript",
  "ada",
  "agena",
  "algol_60",
  "algol_68",
  "algol_w",
  "amigae",
  "antlang",
  "apex",
  "apl",
  "applescript",
  "arendelle",
  "arm_assembly",
  "arturo",
  "asymptote",
  "autohotkey",
  "autoit",
  "awk",
  "axe",
  "babel",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bc",
  "befunge",
  "blast",
  "bracmat",
  "brat",
  "brlcad",
  "burlesque",
  "c",
  "c_shell",
  "chapel",
  "chef",
  "chuck",
  "clean",
  "clojure",
  "cobol",
  "coffeescript",
  "coldfusion",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dc",
  "delphi",
  "deluge",
  "dragon",
  "dwscript",
  "dyalect",
  "dylan",
  "e",
  "easylang",
  "echolisp",
  "ecl",
  "edsac_order_code",
  "egl",
  "eiffel",
  "ela",
  "elena",
  "elixir",
  "elm",
  "emacs_lisp",
  "erlang",
  "erre",
  "es",
  "euphoria",
  "factor",
  "falcon",
  "false",
  "fancy",
  "fish",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "futhark",
  "fuze_basic",
  "gambas",
  "gap",
  "gema",
  "genie",
  "gml",
  "gnuplot",
  "go",
  "golfscript",
  "gri",
  "groovy",
  "haskell",
  "haxe",
  "hicest",
  "html",
  "idl",
  "inform_7",
  "io",
  "j",
  "java",
  "javascript",
  "jcl",
  "joy",
  "jq",
  "jsish",
  "julia",
  "k",
  "konsolscript",
  "kotlin",
  "labview",
  "lang5",
  "lasso",
  "latex",
  "liberty_basic",
  "lily",
  "lilypond",
  "lingo",
  "livecode",
  "logo",
  "logtalk",
  "lotusscript",
  "lse64",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "mbs",
  "metafont",
  "microsoft_small_basic",
  "min",
  "mirah",
  "mirc_scripting_language",
  "monte",
  "montilang",
  "moo",
  "neko",
  "nemerle",
  "nesl",
  "netrexx",
  "newlisp",
  "nim",
  "nsis",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "oorexx",
  "openscad",
  "oxygenbasic",
  "oz",
  "pari_gp",
  "pascal",
  "pasm",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pl_i",
  "pl_sql",
  "plaintex",
  "pop11",
  "postscript",
  "powershell",
  "processing",
  "prodos",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "rebol",
  "related_tasks",
  "retro",
  "rexx",
  "ring",
  "rlab",
  "robotic",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "sather",
  "scala",
  "scheme",
  "scilab",
  "sed",
  "seed7",
  "set_lang",
  "setl",
  "sidef",
  "simula",
  "slate",
  "smalltalk",
  "smart_basic",
  "snobol4",
  "snusp",
  "spl",
  "sql",
  "sql_pl",
  "squirrel",
  "ssem",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "tern",
  "toka",
  "torquescript",
  "tpp",
  "tuscript",
  "txr",
  "unix_shell",
  "unlambda",
  "ursa",
  "ursala",
  "vba",
  "vbscript",
  "verbexx",
  "verilog",
  "vhdl",
  "vim_script",
  "visual_basic",
  "visual_basic_.net",
  "visual_objects",
  "vorpal",
  "wart",
  "xlisp",
  "xojo",
  "xpl0",
  "xquery",
  "xslt",
  "xul",
  "zig",
  "zkl",
  "zonnon",
]
+++

## Task

Show all ways to include text in a language source file
that's completely ignored by the compiler or interpreter.


## Related tasks

*   [[Documentation]]
*   [[Here_document]]


## See also

*   [https://en.wikipedia.org/wiki/Comment_(computer_programming) Wikipedia]
*   [http://xkcd.com/156 xkcd] (Humor: hand gesture denoting <code>//</code> for "commenting out" people.)





## 11l


```11l
// Single line comment
\\ Also single line comment (continuation of the comment in previous line)

\[ This is
a multi line
comment ]

\{ And
this }

\( And
this )

\‘ And
this ’
```



## 360 Assembly


```360 Assembly

* An asterisk in column one denotes a comment line
* Comments may also follow any syntactically complete instruction:
         LA    1,0           Comment
         NOP                 Comment (after a NOP instruction)
* Comments after instructions with omitted operands require a comma ","
         END   ,             Comment (without comma, "Comment" assumed an operand of "END")

```



## 4D


```4d
`Comments in 4th Dimension begin with the accent character and extend to the end of the line (until 4D version 2004).
// This is a comment starting from 4D v11 and newer. Accent character is replaced by //
```



## 6502 Assembly

Note: syntax depends on the assembler software but use of a semicolon is fairly standard

```6502 Assembly
          nop           ; comments begin with a semicolon>
```



## 8086 Assembly

Note: syntax depends on the assembler software but use of a semicolon is fairly standard

```asm
	MOV AX, 4C00h 		; go back to DOS
	INT 21h                 ; BIOS interrupt 21 base 16
```



## ACL2

Just like Common Lisp:

```Lisp
; Single line comment
#| Multi-line
comment |#
```



## ActionScript

:''See [[Comments#Java|Java]]''


## Ada


```ada
-- All Ada comments begin with "--" and extend to the end of the line
```



## Agena

Agena has single line comments and two styles of multi-line comments.

```agena
# single line comment

#/ multi-line comment
   - ends with the "/ followed by #" terminator on the next line
/#

/* multi-line comment - C-style
   - ends with the "* followed by /" terminator on the next line
*/
```



## ALGOL 60

A comment in ALGOL 60 takes the place of a single instruction.
<lang algol_60>
'COMMENT' this is a first comment;
'COMMENT'
****** this is a second comment ******
;

```



## ALGOL 68


### With Standard

Comments can be inserted in variety of ways:
{|border="1" style="border-collapse: collapse; border: 5px double grey;"
|align=center|Algol68 as typically published,
includes '''bold''' typeface.
|align=center|''Quote'' stropping,
like to [[wp:Wiki markup|Wiki markup]].
|align=center|''Case'' stropping,
7-bit/ascii implementations.
|align=center|''Res'' stropping,
detecting reserved words.
|align=center|''Point'' stropping,
6-bits/byte implementations.
|-
|colspan=4 align=center|¢ The original way of adding your 2 cents worth to a program with the "cent" character ¢||¢ TEXT ¢
|-
|'''co''' Style i comment '''co'''
'''comment''' text '''comment'''
||'co' text 'co'
'comment' text 'comment'
||CO text CO
COMMENT text COMMENT
||co text co
comment text comment
||.CO TEXT .CO
.COMMENT TEXT .COMMENT
|-
|colspan=4 align=center|# Style ii comment with the hash character #||# TEXT #
|}
Notes:
* The <tt># test #</tt> and <tt>¢ text ¢</tt> comment tends to be used for inline comments.  And the <tt>COMMENT text COMMENT</tt> style tends to be used to comment out entire blocks.
* The script [http://www.vim.org/scripts//script.php?script_id=1927 '''algol68.vim'''] can be used to highlight commented blocks while editing source code.


### With Extensions

 &pound; This is a hash/pound comment for a UK keyboard &pound;


## ALGOL W

Comments in Algol W can appear anywhere whitespace is allowed. A comment starts with the reserved word 'comment' and ends with the next semi-colon. Alternatively a comment can start with a percent sign and end with the next percent sign or semi-colon.

A single word in the form of an identifier following the reserved word 'end' is also a comment.


```algolw
begin
    comment a comment;
    % another comment
    ;
    % and
      another
    %
end this_word_is_also_a_comment.
```


## AmigaE


```amigae
/* multiline comment
are like C ... */
-> this is a end of line comment
```



## AntLang


```AntLang
2 + 2 /This is a comment>
```



## Apex


```apex

System.debug ('I will execute');   // This comment is ignored.
/*
 I am a large comment, completely ignored as well.
*/

```



## APL


```apl
⍝ This is a comment
```



## AppleScript


```AppleScript

--This is a single line comment

display dialog "ok" --it can go at the end of a line

# Hash style comments are also supported

(* This is a multi
line comment*)

(* This is a comment. --comments can be nested
  (* Nested block comment *)
*)

```


```AppleScript
display dialog "ok" #Starting in version 2.0, end-line comments can begin with a hash
```



## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI comment one line */
/*  comment line 1
    comment line 2
*/

	mov r0,#0    @ this comment on end of line
	mov r1,#0    //  authorized comment


```



## Arendelle


Arendelle uses C style comments


## Arturo



```arturo
// This is a simple single-line comment

a 10 // another single-line comment

/* Now, this is a
   multi-line comment
*/
```



## Asymptote


```Asymptote
// double slash to newline>
```


See [http://asymptote.sourceforge.net/doc/Programming.html programming introduction in the Asymptote manual].

When reading data files a comment character in them can be specified as <code>comment="#"</code> etc.  See [http://asymptote.sourceforge.net/doc/Files.html Files in the Asymptote manual].


## AutoHotkey


```AutoHotkey
Msgbox, comments demo ; end of line comment
/*
multiline comment1
multiline comment2
*/
```


For multi-line comments, the '''/*''' and '''*/''' '''must''' be on their own separate lines. Nothing else can be on the same line.


## AutoIt



```AutoIt

#cs
Everything between the cs and and the ce is commented.
Commented code is not used by the computer.
#ce
;individual lines after a semicolon are commented.

```



## AWK


The ''hash'' symbol # start a comment; it ends at the end of line.


```awk
BEGIN { # this code does something
  # do something
}
```



## Axe



```axe
.This is a single-line comment
```



```axe
...
This is a multi-line comment
...
```



```axe
...If 0
This is a comment only if the condition evaluates to zero
...
```



```axe
...!If 1
This is a comment only if the condition evaluates to nonzero
...
```



```axe
...Else
This is a comment only if the previous conditional comment was executed (and vice versa)
...
```




## Babel


```babel

-- This is a line-comment

#
    This is a block-comment
    It goes until de-dent

dedent: 0x42 -- The comment block above is now closed

```



## BASIC

<!-- IMPORTANT NOTE: Until geshi gets updated to include REM and /' ... '/ as comment delimiters,
don't add markup to this section, or BASIC keywords inside the comments will be marked up
as if they were uncommented. (QB section is actually OK for markup, but since the other two aren't,
leave them all alone for consistency.) -- Erik Siers, 19 Feb '11 -->

The only truly standard method of marking a comment in BASIC is using the <code>REM</code> keyword. This dates back to (at least) the late 1970's, and ''should'' work with most BASICs available today:


```gwbasic
100 REM Standard BASIC comments begin with "REM" (remark) and extend to the end of the line
110 PRINT "this is code": REM comment after statement
```


This may not be well known, but you may include text after the line number of GOTO and GOSUB statements.  This is kind of a comment in absence of labels.

 100  GOTO 200HERE
 110  GOSUB 300THERE
 120  GOTO 400THEOTHERPLACE
 130  GOTO 500MOM AND  POP

Spaces are removed from non-keyword text.  BASIC keywords can be used.
List outputs spaces around keywords.

Most BASICs also support alternate comment characters,
commonly an apostrophe (single quote):


```qbasic
 'this is a comment
 PRINT "this is code"  'comment after statement
```


Characters other than apostrophe are used in some BASICs. For example, DarkBASIC uses a back-tick character ('''`''', a.k.a. [[wp:grave accent|grave accent]]):

 `this is a comment
 PRINT "this is code" `comment after statement
 'this is NOT a comment!

In addition to single-line comments, a few BASICs support block comments. FreeBASIC was influenced by [[Comments#C|the C family's]] block comment characters:

<!-- This is currently without syntax highlighting because whoever wrote freebasic.php for GeSHi didn't add the multi-line comments. -->
 /' This is a multi line comment.
 Requires FreeBASIC 0.16 or later.
 Last line of the comment block. '/

 DIM a AS /' Comment in the middle of statement '/ Integer

=
## BaCon
=
BaCon accepts '''REM''' (or single quote apostrophe) for line comments.

C-style block comments can be used with <nowiki>/* and */</nowiki> pairs; these comment blocks may cross line boundaries.

Inside ''USEC'' sections, all comment styles accepted by the configured C compiler will also be ignored.

==={{header|IS-BASIC}}===
<lang IS-BASIC>100 REM Standard BASIC comments begin with "REM" (remark) and extend to the end of the line
110 PRINT "this is code" ! comment after statement
```



## Batch File


```dos
rem Single-line comment.
```


There is another (unsupported) option, using a double-colon <code>::</code>. However, this has issues with some syntactic constructs and therefore may raise syntax errors.

```dos
:: Another option, though unsupported and known
:: to fail in some cases. Best avoided.
```


Since comment lines are skipped entirely by the parser multi-line comments aren't possible even with line continuation.


## BBC BASIC


```bbcbasic
      REM This is a comment which is ignored by the compiler
      *| This is a comment which is compiled but ignored at run time
```



## bc


```bc
/* This is a comment. */

2 + /* Comment between tokens. */ 3

"This is a string, /* NOT a comment */."

/*
 * A comment can have multiple lines. These asterisks in the middle
 * of the comment are only for style. You must not nest a comment
 * inside another comment; the first asterisk-slash ends the comment.
 */
```


----
```bc
#!/usr/bin/bc

# This form of comment is an extension, not part of standard bc.

# Your program must not have a #! shebang line
# unless your bc supports this form of comment.

2 + 3  # It can be after a statement.

a = 1  # The newline is not part of the comment.
b = 2  # So this line is a different statement.
```




## Befunge

Like Brainfuck, all characters and whitespace which are not commands are ignored.
Also, since the code/data-space is two-dimensional, comments can be placed anywhere that will be untouched by the instruction pointer and data access commands.
Finally, in Funge-98, the ; instruction immediately skips to the next ; instruction, which allows to isolate comments from code.

```befunge
& read a number 2+ add two .@ display result and exit
  ^- inline comments -^     <-^- other comments
```


```befunge
&;read a number;2+;add two;.@;display result and exit;
  ^- inline comments -^     <-^- other comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;completely isolated comment block for the paranoid;
;(almost - you can still skip into it.)            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```



## Blast



```blast
# A hash symbol at the beginning of a line marks the line as a comment

```



## Bracmat

Bracmat uses curly braces as comment delimiters. Curly braces inside comments must be balanced. Comments are so much ignored when source code is read, that they do not reappear in listings created by the built-in <code>lst$</code> function, an otherwise very useful function to autoindent your code.

=={{header|Brainfuck}}==

```bf
This is a comment>
```


Most ASCII characters may be used for comments; only the eight characters "+-<>[],." are Brainfuck commands. Extra care must be used when using punctuation, particularly the comma or period. These are I/O operators and are actually commands rather than comments, and are instead compiled into the program if used and may have to be "debugged" and removed if you forget this issue.


## Brat


```brat
# Single line comment

#* Multi
   Line
   Comment *#
```



## Brlcad



```brlcad

 # Comments in mget scripts are prefixed with a hash symbol
 ls   # comments may appear at the end of a line

```



## Burlesque


Burlesque does NOT have comments. However, you can comment code by pushing a string and popping it immediately.


```burlesque

"I'm sort of a comment"vv

```


Since strings are multi-line strings:


```burlesque

"I'm a
very long comment spanning
over several lines"vv

```



## C


```c
/* This is a comment. */
/* So is this
   multiline comment.
 */
```

The comment starts at the <tt>/*</tt>, and ends at the <tt>*/</tt>. A comment may be used between any tokens. It cannot be used inside tokens, that is, given the code

```c
struct charisma {};
void f(char/* comment */isma) {}
```

the function takes an argument of type char, named isma, not an unnamed argument of type charisma.

Comments cannot be nested; that is, if you write

```c
/* some comment /* trying to nest some other comment */ inside */
```

the comment ends at the first <tt>*/</tt>, and <tt>inside */</tt> is again interpreted as source code (almost certainly causing a compile error). Some compilers have the option to allow nested comments, but this is not a standard feature.

Conditional compilation also can be used to make the compiler ignore some text:

```c
#if 0
While technically not a comment, this is also ignored by the compiler
#endif
```

The trick is that 0 is always false, therefore the text between <tt>#if 0</tt> and <tt>#endif</tt> is never compiled. While this should never be used for actual comments, it's an easy way to comment out some code, especially because it doesn't interfere with normal (documentation) comments.

Conditional compile "comments" can be nested:

```c
#ifdef UNDEFINED
This is not compiled.
#if 0
Nor is this.
#endif
And this still is not compiled.
#endif
```


Even though the compiler doesn't see '''#if 0''' text, the preprocessor does. Therefore some minimal rules still have to be followed. For example, the following code is ''not'' valid:

```c
#if 0
This isn't valid.
#endif
```

That's because the preprocessor will interpret the apostrophe as beginning of a character constant, and will complain because that character constant isn't terminated with another apostrophe.

Note that the problem mentioned above cannot occur if there's valid code between the <tt>#if 0</tt> and <tt>#endif</tt>.

```c
// C++ single-line comments were adopted in the C99 standard.
```



## ChucK


```ChucK

<-- Not common
// Usual comment

```


## C++

:''See also [[Comments#C|C]]''

Single line C++-style comments

```cpp
// This is a comment
```

C++-style comments start with <tt>//</tt> and reach up to, but not including, the end of line (more exactly, up to the next unescaped newline). While formally, C++-style comments cannot be nested either, in practice they can:

```cpp
// This is a valid comment // with a "nested" comment
```

That's because starting with the first <tt>//</tt> everything in the line is ignored, including the second <tt>//</tt>.
The fact that the newline is ''not'' part of the comment is important for multi-line macro definitions. It means that in the code

```cpp
#define FOO \
  (macro text) // comment
  (no more macro text)
```

the line <tt>(no more macro text)</tt> is ''not'' part of the macro definition. Also escaping the line break at the end of the comment with '\' doesn't help, because that would make the third line part of the ''comment'' instead. Comments inside macros therefore have to be C-style.

## C#


```c#
//This is a comment.
//This is other comment.

/* This is a comment too. */

/* This is a
multi-line
comment */
```



## Chapel


```chapel
// single line

/* multi
line */
```



## Chef


```Chef
Comment Stew.

This is a comment.
The other comment is a loop, but you can name it anything (single word only).
You can also name ingredients as comments
This is pseudocode.

Ingredients.
Ingredient list

Method.
Methods.
SingleWordCommentOne the Ingredient.
Methods.
SingleWordCommentTwo until SingleWordCommentOned.
Methods.
```



## Clean

Clean comments are similar to C++.

```clean
Start = /* This is a multi-
           line comment     */ 17 // This is a single-line comment
```

In contrast to C++ comments can be nested.

```clean
Start = /* This is a comment /* Nested comment */ still a comment */ 17
```



## Clojure


Anything from a semicolon to the end of a line is a comment.


```lisp
;; This is a comment
(defn foo []
  123) ; also a comment
```


The <code>(comment)</code> macro will prevent a form from being evaluated, returning <code>nil</code> no matter what is contained in the comment.  However the forms inside the <code>comment</code> form must be properly parseable (parentheses balanced, etc.) or an exception will be thrown.


```lisp
(comment (println (foo)) "bar" :baz 123 (System/exit 0))  ;; does nothing, returns nil
```


Finally, the <code>#_</code> reader macro will cause a form to be ignored by the reader.  Unlike <code>(comment)</code>, this does not return <code>nil</code>; the surrounding code is evaluated as though the ignored form isn't even there.


```lisp
(+ 1 (comment "foo") 3)  ;; Throws an exception, because it tries to add nil to an integer
(+ 1 #_"foo" 3)          ;; Returns 4
```



## COBOL


###  Fixed format


```cobol
      * an asterisk in 7th column comments the line out
```

A D in the 7th column indicates a debugging line which is treated like a comment unless a compiler flag is set.

```cobol
      D    DISPLAY "Debug"
```



###  Free format


```cobol
*> This comment syntax was defined (with free format code) in COBOL 2002.
```


This indicates a debugging line like above, but if it is used in fixed format files, it must be in the 8th column or beyond. ''Not necessarily.''  GnuCOBOL also supports D as an indicator in column 7, the >>D format works (more by trickery than spec) if the angle brackets start in column 5, the D ending up in column 7.  The >>D debug marker can then be both fixed and free form compatible.

```cobol
>
D DISPLAY "Debug"
```



###  <code>NOTE</code> statement

This statement causes everything following it up to the next separator period to be treated as a comment. This statement was deleted in COBOL-74.

```cobol
           NOTE this paragraph is
               commented out and ignored
           .
```



###  <code>REMARKS</code> and other statements

There are quite a few <code>IDENTIFICATION DIVISION</code> obsolete and extension reserved words that will work in GnuCOBOL 2.


```COBOL

        IDENTIFICATION DIVISION.
        PROGRAM-ID. program.

        AUTHOR. Rest of line ignored.
        REMARKS. Rest of line ignored.
        REMARKS. More remarks.
        SECURITY. line ignored.
        INSTALLATION. line ignored.
        DATE-WRITTEN. same, human readable dates are allowed for instance
        DATE-COMPILED. same.
        DATE-MODIFIED. this one is handy when auto-stamped by an editor.

```
  Those lines can occur multiple times each within the IDENTIFICATION DIVISION.  There can be many AUTHORs, SECURITY notes, etc.  These words are also supported by other COBOL dialects, but may have different rules on order, multiples allowed of if full stop periods are required (or allowed) before the end of line.


## CoffeeScript


```coffeescript
# one line comment

### multi
line
comment ###
```



## ColdFusion

In tags:

```cfm
As ColdFusion's grammar is based around HTML syntax, commenting is similar to HTML.
<!--- This is a comment.  Nothing in this tag can be seen by the end user.
       Note the three-or-greater dashes to open and close the tag. --->
<!--  This is an HTML comment.  Any HTML between the opening and closing of the tag will be ignored, but any ColdFusion code will still run.
       Note that in the popular FuseBox framework for ColdFusion, the circuit.xml files require that you use this style of comment. -->
```


In script:

```cfm
/* This is a comment */
// This is also a comment
```



## Common Lisp

Common Lisp provides [http://www.lispworks.com/documentation/HyperSpec/Body/02_dd.htm line comments (<tt>;</tt>)] and [http://www.lispworks.com/documentation/HyperSpec/Body/02_dhs.htm block comments (<tt>#|...|#</tt>)].

Block comments can nest (<tt>#|...#|...|#...|#</tt>), unlike block comments in e.g. [[C]].

In a common convention, header comments are prefaced with four semicolons, top-level (function level) comments use three, comments for sections of code use two, and margin comments use one.


```lisp
;;;; This code implements the foo and bar functions

;;; The foo function calls bar on the first argument and multiplies the result by the second.
;;; The arguments are two integers
(defun foo (a b)
   ;; Call bar and multiply
   (* (bar a) ; Calling bar
      b))

;;; The bar function simply adds 3 to the argument
(defun bar (n)
   (+ n 3))
```


However, comments should not be used for inline documentation, as most defining constructs permit a documentation string (which is then available at runtime). <!-- It would be better to make the above example not do this, instead of showing bad style -->


```lisp
(defun bar (n)
  "Add 3 to the argument."
  (+ n 3))

(defclass button (widget)
  (label action)
  (:documentation "This is a push-button widget."))
```



## Component Pascal


```oberon2

(* Comments (* can nest *)
   and they can span multiple lines.
 *)

```


## D


```d
void main() {
    // A single line comment.

    /* This is a simple C-style comment that can't be nested.
    Comments mostly work similar to C, newlines are irrelevant.
    */

    /+ This is a nestable comment
      /+ See?
      +/
    +/

    /// Documentation single line comment.

    /**
    Simple C-style documentation comment.
    */

    /++
    Nestable documenttion comment.
    +/
}
```



## Dart



```dart
// This is a single line comment, which lasts until the end of the line. The Dart linter prefers this one.

/* This is also a valid single line comment. Unlike the first one, this one terminates after one of these -> */

/*
  You can use the syntax above to make multi line comments as well.
  Like this!
*/

/// These are doc comments. You can use dartdoc to generate doc pages for your classes with these.
///
/// Formatting [variable] and [function] names like so allows dartdoc to link to the documentation for those entities.

```



## dc

There is no comment syntax in POSIX dc. The convention is to make a string on the stack and move it to an unused register; a no-op.

```dc
[Making and discarding a string acts like a comment] sz
```

GNU dc added the comment syntax of many other scripting languages.

```dc
# remainder of line is a comment>
```



## Deluge


Comments are only allowed in places such as "on load" scripts. You cannot put them in form or view definitions.


```deluge
// single line comment>
```



## Delphi

:''See also [[Comments#Pascal|Pascal]]''

In addition to Pascal, Delphi also allows C++ style single line comments:

```delphi
// single line comment>
```


=={{header|Déjà Vu}}==

```dejavu
#this is a comment
!print "this is not a comment, obviously" #this is a comment as well
```



## Dragon


```dragon
// This is a comment>
```



```dragon
/*
  This is
  a multiple
  line comment.
 */
```



```dragon


showln "Hello " /* This is an inline comment */ "world"


```



## DWScript


```delphi
(* This is a comment.
   It may extend across multiple lines. *)

{ Alternatively curly braces
  can be used. }

/* C-style multi-line comments
   are supported  */

// and single-line C++ style comments too
```



## Dyalect



```dyalect
/* This is a
multi-line comment */

//This is a single-line comment
```



## Dylan


```Dylan
// This is a comment

/*
   This is a comment
   that spans multiple
   lines
*/
```



## E



```e
# This is a regular comment.

? "This is an Updoc comment, which
> is an executable example or test case.".split(" ")
# value: ["This", "is", "an", "Updoc", "comment,", "which
#        is", "an", "executable", "example", "or", "test", "case."]
```


All comments span to the end of the line; there are no paired-delimiter comment syntaxes. “<code>#</code>” begins a comment anywhere outside of quotes; “<code>?</code>” and “<code>&gt;</code>” begin comments only if they are at the beginning of a line (except for whitespace), because those characters are also used for infix operators.

In Updoc, “<code>?</code>” indicates the beginning of a program fragment, “<code>&gt;</code>” the continuation of one, and  “<code>#</code>” begins the expected output from its evaluation; “<code>??</code>” indicates special directives.


## EasyLang

<lang># This is a comment
```



## EchoLisp


```lisp

666 ; this is an end-of-line comment

#|
 This is a multi-line comment
 Nesting is not allowed
|#

;; The (info <name> [<string>)] function associates a symbol and a comment
;; These info strings are saved in permanent memory (local storage)
;; Unicode characters may be used, as everywhere in the language

(define mynumber 666) → mynumber
(info 'mynumber "👀 Symbols may be commented with an information string 👺")
(info 'mynumber) → displays the above inside the 'info' field.

```



## ECL


Single-line comments must begin with //

```ECL
// this is a one-line comment
```


Block comments must be delimited with /* and */


```ECL
 /* this is a block comment - the terminator can be on the same line
or any succeeding line – everything in between is ignored */
```



## EDSAC order code

EDSAC programs were handwritten on "programme sheets" designed for the purpose. The programmer, or a computer operator, then copied the "orders" (instructions) to punched tape for input to the machine. Programme sheets had a column for "notes" (comments), but these were not copied to the tape.
Modern simulators, however, accept square brackets as comment delimiters.

```edsac
[This is a comment]
[
And so
is
this
]
[But in 1949 they wouldn't have been]
```



## EGL

:''See [[Comments#Java|Java]]''


## Eiffel



```Eiffel
-- inline comment, continues until new line
```



## Ela



```Ela
//single line comment

/*multiple line
comment*/
```


## Elena


```elena
//single line comment

/*multiple line
comment*/
```



## Elixir

Elixir does not have multiple line comments.

```elixir

# single line comment

```



## Elm


```elm

-- a single line comment

{- a multiline comment
   {- can be nested -}
-}

```



## Emacs Lisp

A comment is started by <code>;</code> and reaches to the end of the line.

```lisp
; This is a comment
```


There are some coding conventions for <code>;;</code> align to indentation, <code>;;;</code> sections, etc,

: [http://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html http://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html]

Another way to add comments is to use strings at places where the result of an expression is ignored, since they simply evaluate to themselves without any effect. Note that strings can be multi-line:

```lisp
"This is effectively a comment,
if used at a place where the result is ignored"
```

Note that strings at the beginning of function definitions are interpreted as documentation strings for the function (i.e. Emacs will display them if asked for help about the function), e.g.

```lisp
(defun subtract-second-from-first (x y)
  "This function subtracts its second argument from its first argument."
  (- y x))
```

Due to this, it's debatable if the string at that place can be considered as comment.


## Erlang



```erlang
% Erlang comments begin with "%" and extend to the end of the line.
```




## ERRE


```ERRE

! Standard ERRE comments begin with ! and extend to the end of the line

PRINT("this is code") ! comment after statement

```



## Euphoria

Single line comment:

```Euphoria
-- This is a comment
```



Multiline C-style comment:

```Euphoria
/*
This is a comment
*/
```

## FreeBASIC

<lang>' FB 1.05.0 Win64

' This a single line comment

REM This is another way of writing a single line comment

/'
  This is a
  multi-line
  comment
'/

/'
  Multi-line comments
  /'
    can also be nested
  '/
  like this
'/
```


=={{header|F_Sharp|F#}}==
F# accepts C++ type line comments and OCaml type block comments

```fsharp
// this comments to the end of the line
(* this comments a region
   which can be multi-line *)
```



## Factor


```factor
! Comments starts with "! "
#! Or with "#! "
! and last until the end of the line

```



## Fancy



```fancy
# Comments starts with "#"
# and last until the end of the line

```


## Falcon

Falcon supports C-language style single line and block comments. A single line comment begins with two slashes (//) and ends at the end of the line. A block comment begins with a slash followed by an asterisk, and terminates when an asterisk followed by a slash is met (/*...*/).

```falcon

/* Start comment block
 My Life Story
 */

// set up my bank account total
bank_account_total = 1000000 // Wish this was the case

```



## FALSE


```false
{comments are in curly braces}
```



## Fish

Since ><> is a funge-like language, all characters not touched by the command pointer or modified by the <tt>p</tt> and <tt>g</tt> commands can be comments.
Unlike Brainfuck, unknown commands are not ignored by the compiler, they just raise an error.

```Fish
v This is the Fish version of the Integer sequence task
>0>:n1+v all comments here
  ^o" "< still here
And of course here :)
```



## Forth


Standard Forth includes a number of ways to add comment text. As with everything in Forth, comment characters are actually words that control the compiler.

```forth
\ The backslash skips everything else on the line
( The left paren skips everything up to the next right paren on the same line)
```


Traditionally, the paren comments are used for "stack effect" notation:

```forth
: myword ( a b -- c )  ...
```


This comment means "myword takes two cells on the stack and leaves one".  Sometimes, stack effect comment names give clues about the word's function:

```forth
: add'em ( a b -- a+b )   + ;
: strlen ( addr -- len )   count nip ;
```


Some Forth systems implement other commenting words, such as these words from Win32Forth:

```forth
\s skips all remaining text in the file
(( skips until the next double-paren,
   stretching across multiple lines ))
comment:
   Ignore all text in this section
comment;
doc
   Another comment block
enddoc
/* C-style comment */
(* Pascal-style comment *)
```



## Fortran


Compiler: ANSI FORTRAN 77 or compatible (like <tt>[[g77]] -strict</tt>)

The first six columns in Fortran are traditionally reserved for labels and certain special characters. In particular the letter "C" in the first column indicates a comment:


```fortran
C     This would be some kind of comment
C     Usually one would avoid columns 2-6 even in a comment.
```


Some Fortran compilers have the extension that comments starting with D are treated as non-comments if a special debugging flag is given at the compiler invocation. For example:


```fortran
C     If compiled in debugging mode, print the current value of I
D     PRINT *, I
```


ISO Fortran 90 or later have an inline comment (!) syntax:


```fortran
real :: a = 0.0   ! initialize A to be zero
```


In ISO Fortran 90 or later, "C in first column" comments are only allowed in the "fixed" source form familiar to FORTRAN 77 programmers. The "free" source form only has inline comments (!).

ISO Fortran 95 or later has an optional conditional compilation syntax. If present, it can be used (abused?) to (in effect) comment out blocks of code:


```fortran
?? if (.false.) then
do while (oh_no)
   a = bad_news()
   b = big_mistake()
   c = gigo()
end do
?? end if
```



## Frink


```frink

// This is a single-line comment
/*  This is a comment
    that spans multiple lines
    and so on.
*/

```



## Futhark



```Futhark

-- Single-line comment

-- Multi-line
-- comment (yes, just several single-line comments).

```




## FUZE BASIC


```qbasic
//Comment (No space required)
# Comment (Space required)
REM Comment (Space require)
PRINT "This is an inline comment."//Comment (No space required)
END
```



## Gambas


In gambas, comments can be inserted by prefixing them with an apostrophe. The gambas interpreter will ignore the apostrophe and any other characters that follow it until the end of the line:


```gambas

 ' This whole line is a comment and is ignored by the gambas interpreter
 print "Hello" ' Comments after an apostrophe are ignored
 '' A bold-style comment
 ' TODO:  To Do  comment will appear in Task Bar
 ' FIXME: Fix Me comment will appear in Task Bar
 ' NOTE:  Note   commnet will appear in Task Bar

```



## GAP


```gap
# Comment (till end of line)
```



## Gema


```gama
! comment starts with "!" and continues to end of line
```

A shebang (#!) may be used as a comment in the first line of a file.


## Genie

Genie allows comments in code in two different ways.

```genie
// Comment continues until end of line

/* Comment lasts between delimiters */
```


Delimited comments cannot be nested.


## GML

single-line comment:

```GML
 // comment starts with "//" and continues to the end of the line
```


multi-line comment:

```GML
 /* a multi-line comment starts with slash-asterisk and,
ends with asterisk-slash.
also note:
 * A multi-line comment is ignored inside a string
 * A multi-line comment can be ended inside a line
*/
```



## gnuplot


```gnuplot
# this is a comment

# backslash continues \
a comment to the next \
line or lines
```


The way backslash continues a comment means that comments can't usefully be put within a multi-line function definition,


```gnuplot
# this doesn't work
foo(n) = (n                \
          + 2    # no good \
          + 3)

# behaves as if you wrote merely
foo(n) = (n+2
```



## Go


```go
// this is a single line comment
/* this is
   a multi-line
   block comment.
/* It does not nest */
```



## Golfscript


```golfscript
# end of line comment>
```



## Gri

<code>#</code> through to newline.


```Gri
# this is a comment
show 123        # this too is a comment
```


<code>//</code> works similarly but is reckoned the "old way" (as of Gri 2.12.23)

<lang>// this is a comment
show 123        // this too is a comment
```


Both forms can be used in input data files too.


## Groovy

:''See [[Comments#Java|Java]]''

=={{header|GW-BASIC}}==
```gwbasic
100 REM Standard BASIC comments begin with "REM" (remark) and extend to the end of the line
110 PRINT "this is code": REM comment after statement
```



## Haskell



```haskell
i code = True -- I am a comment.

{- I am also
   a comment. {-comments can be nested-}
   let u x = x x (this code not compiled)
   Are you? -}

-- |This is a Haddock documentation comment for the following code
i code = True
-- ^This is a Haddock documentation comment for the preceding code

{-|
  This is a Haddock documentation block comment
-}
i code = True
```



## Haxe


```haxe
// Single line commment.

/*
   Multiple
   line
   comment.
*/
```



## HicEst


```hicest
! a comment starts with a "!" and ends at the end of the line
```



## HTML


```html5
<!-- Anything within these bracket tags is commented, single or multi-line. -->
```


=={{header|Icon}} and {{header|Unicon}}==
Any text after "#" is a comment.

```Icon
# This is a comment

procedure x(y,z)    #: This is a comment and an IPL meta-comment for a procedure

```

The [[:Category:Icon_Programming_Library|The Icon Programming Library]] established conventions for commenting library additions and functions.  This included both header block comments and meta comments on procedures within library files.


## IDL


The comment character in IDL is the semicolon - everything starting with it and to the end of the line is a comment. Like this:


```idl
; The following computes the factorial of a number "n"
fact = product(indgen( n )+1) ; where n should be an integer
```



## Inform 7


```inform7
[This is a single-line comment.]

[This is a
multi-line comment.]

[Comments can [be nested].]
```



## Io


```io
# Single-line comment

// Single-line comment

/* Multi-line
   comment */
```



## J


```j
NB. Text that follows 'NB.' has no effect on execution.

0 : 0
Multi-line comments may be placed in strings,
like this.
)

Note 'example'
Another way to record multi-line comments as text is to use 'Note', which is actually
a simple program that makes it clearer when defined text is used only to provide comment.
)

'A simple string which is not used is legal, and will be discarded'
```



## Java

Java has two ways to enter normal comments, plus a third type of comment that doubles as a way to generate HTML documentation.

### C Style


```java
/* This is a comment */
```



```java
/*
 * This is
 * a multiple
 * line comment.
 */
```

This ''C-style'' comment starts with <tt>/*</tt> and ends with <tt>*/</tt>.
The two delimiters may be on the same or separate lines.
This style comment may be used anywhere white space is permitted.
===C++ Style (inline)===

```java
// This is a comment
```

This ''C++-style'' comment starts with <tt>//</tt> and extends to the end of line.

===Java Documentation (Javadoc)===

```java
/** This is a Javadoc comment */
```



```java
/**
 * This is
 * a multiple
 * line Javadoc comment
 */
```

[http://en.wikipedia.org/wiki/Javadoc Javadoc] is a standardized documentation code for Java. Its comments begin with a forward slash and two stars. Javadoc comments have different tags that signify different things in the methods and classes that they precede.


### Sneaky

Your editor will probably colour this as great big comment, but it compiles and prints "Hello World!". Once you've figured out how this works, try this [http://stackoverflow.com/questions/4448180/why-does-java-permit-escaped-unicode-characters-in-the-source-code discussion on why it's allowed].

```java
public class JustComments {
    /*
    \u002A\u002F\u0070\u0075\u0062\u006C\u0069\u0063\u0020\u0073\u0074\u0061\u0074\u0069\u0063
    \u0020\u0076\u006F\u0069\u0064\u0020\u006D\u0061\u0069\u006E\u0028
    \u0053\u0074\u0072\u0069\u006E\u0067\u005B\u005D\u0061\u0072\u0067\u0073\u0029
    \u007B\u0053\u0079\u0073\u0074\u0065\u006D\u002E\u006F\u0075\u0074\u002E
    \u0070\u0072\u0069\u006E\u0074\u006C\u006E\u0028\u0022\u0048\u0065\u006C\u006C\u006F\u0022
    \u002B\u0022\u0020\u0057\u006F\u0072\u006C\u0064\u0021\u0022\u0029\u003B\u007D\u002F\u002A
    */
}
```



## JavaScript


```javascript
n = n + 1; // This is a comment
```


```javascript
// This is a valid comment // with a "nested" comment
```


```javascript
/* This is
a multi line
comment
// with a "nested" comment
and another line in the comment
*/
```



## JCL

first form

```JCL

//* This is a comment line (//* in columns 1-3)

```

second form

```JCL

/* This is also a comment line (/*  in columns 1-3)

```



## Joy


```joy
# this is a single line comment

(* this is a
multi-line comment *)
```


Multi-line comments cannot be nested.



## jq

Except when a hash symbol (#) appears within a string, it begins a comment that continues to the end of the line:

```jq
# this is a single line comment
"Hello #world" # the first # on this line is part of the jq program

```



## Jsish


```javascript
#!/usr/bin/env/jsish
/* Comments, in Jsish */

// to end of line comment, double slash

/*
 Enclosed comment, slash star, ending with star slash
 Cannot be nested, but can cross line boundaries and occur
 pretty much anywhere whitespace is allowed
*/

var x = 'X'; /* A var called X */
/* active code on this line */ printf("Result %q %d\n", /* comment code mix */ x, /**/42);

;x;
// jsish also handles double slash commented
// unit test echo lines as a special case of "expect failure"

;//noname(x);

/*
=!EXPECTSTART!=
Result X 42
x ==> X
noname(x) ==>
PASS!: err = can not execute expression: 'noname' not a function
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish comments.jsi
Result X 42
prompt$ jsish --U comments.jsi
Result X 42
x ==> X
noname(x) ==>
PASS!: err = can not execute expression: 'noname' not a function
prompt$ jsish -u comments.jsi
[PASS] comments.jsi
```



## Julia


```Julia
# single line

#=
Multi-
line
comment
=#
```



## K


```K
  / this is a comment
  2+2  / as is this

```



## KonsolScript



```KonsolScript
//This is a comment.
//This is another comment.

/* This is a comment too. */

/* This is a
multi-line
comment */
```



## Kotlin


```scala
// This is a single line comment

/*
    This is a
    multi-line
    comment
*/

/*
    Multi-line comments
    /*
        can also be nested
    */
    like so
*/

const val CURRENT_VERSION = "1.0.5-2"  // A comment can also be added at the end of a line
const val /* or even in the middle of a line */ NEXT_MAJOR_VERSION = "1.1"

/**
 * This is a documentation comment used by KDoc.
 *
 * It's documenting the main function which is the entry-point to a Kotlin executable.
 *
 * @param [args] A string array containing the command line arguments (if any) passed to the executable
 * @return Implicit return value is Unit which signifies no meaningful return value (like 'void' in java)
 */
fun main(args: Array<String>) {
    println("Current stable version is $CURRENT_VERSION")
    println("Next major version is $NEXT_MAJOR_VERSION")
}
```



## LabVIEW

## Lang5


```Lang5
# This is a comment.>
```



## Lasso



```Lasso
//This is a comment.

/* This is also a comment. */

/* A multi-line
comment */

/*
### ====================

A multi-line
comment

### =====================
 */
```



## LaTeX

In LaTeX, comments look like this:

```latex
% This is a comment
```

LaTeX comments start with <tt>%</tt> and continue up to ''and including'' the line break. The fact that the line break itself is commented out as well makes it useful for adding line breaks in the source code of complex macros without LaTeX interpreting them (which may cause extra space or even a paragraph break in the resulting typeset text). For example, the following results in the ''one'' word "understandable":

```latex
\newcommand{\firstpart}[1]{under#1}
\newcommand{\secondpart}{able}
\newcommand{\complete}{%
\firstpart{stand}%
\secondpart}

\complete
```

Without the percent sign after <tt>\firstpart{stand}</tt>, it would have been the ''two'' words "understand able".


## Liberty BASIC


```lb
'This is a comment
REM This is a comment

print "This has a comment on the end of the line." 'This is a comment
print "This also has a comment on the end of the line." : REM This is a comment
```



## Lily

There are two kinds of comments:


```lily
# This is a single-line comment
```


and


```lily
#[ This
is
a
block
comment ]#
```


Like with C, block comments don't nest.


## Lilypond



```lilypond
% This is a comment

%{ This is a comment
spanning several lines %}
```



## Lingo

In Lingo any line starting with "--" is a comment and ignored by the interpreter.


```lingo
-- This is a comment.
-- This is another comment
```



## LiveCode


```LiveCode
-- comment may appear anywhere on line
// comment may appear anywhere on line
# comment may appear anywhere on line
/*  this is a
block comment that
may span any number of lines */
```



## Logo


```logo
; comments come after a semicolon, and last until the end of the line
```



## Logtalk


```logtalk
% single-line comment; extends to the end of the line
```


```logtalk
/* multi-line
comment */
```



## LotusScript

LotusScript has two ways to enter comments.

```lotusscript
' This is a comment
```

Wherever the single quote (<tt>'</tt>) is used, the rest of the line is treated as a comment and ignored. Multi-line comments would each need a single quote mark. This style of comment is usually used for making small in-line or single line comments.

```lotusscript
%REM
This is a multi-
line comment.
%END REM
```

A <tt>%REM</tt> marker begins a comment block, and a <tt>%END REM</tt> marker ends the comment block. This style of comment is used for making longer multi-line comments, often at the beginning of a class, sub or function.


## LSE64


```lse64
# single line comment (space after # is required)
```

The author of LSE comments the stack effect of words with header comments as follows:

```lse64
# arg1 arg2 '''yields''' result|''nothing''
```



## Lua



```lua
-- A single line comment

--[[A multi-line
    comment --]]
```


```lua
--[====[ A multi-line comment that can contain [[ many square brackets ]]
]====]
```


## M2000 Interpreter

There is no multi line comment. We have to use ' or \

There are three types of remarks. After statement with a dark color, in a line, with no statements, with the current pen color, and the Rem statement. Rem statement skip statements in current line, but m2000 editor render these using syntax highlight.


```M2000 Interpreter

Module Comments {
      Print "ok" ' comment at the end of line
      Print "ok"  \ comment at the end of line
      \ comment  in one line - different color with previous two
      'comment in one line
      Rem : Print "ok"   ' statements after Rem skipped, but stay with syntax highlight
}
Comments


```




## M4



```M4
eval(2*3)  # eval(2*3)  "#" and text after it aren't processed but passed along

dnl  this text completely disappears, including the new line

divert(-1)
Everything diverted to -1 is processed but the output is discarded.
A comment could take this form as long as no macro names are used.
divert
```


```txt
6  # eval(2*3)  "#" and text after it aren't processed but passed along
```


<code>dnl</code> must be a separate word.
An empty pair of quotes
can separate it from preceding text if necessary


```m4
some text`'dnl then a deleted comment
```


<code>changecom()</code> can set a different character for <code>#</code>,


```m4
changecom(%)
% now percent prevents macro expansion
```


In GNU m4 an empty <code>changecom()</code> string means no such commenting char at all (but in BSD m4 means reset to the default <code>#</code>)


```m4
changecom()
GNU m4 now no macro expansion suppression character at all
```


In GNU m4 <code>changecom()</code> also takes separate start and end strings and they can be multi-character sequences, allowing for example C style,


```m4
changecom(/*,*/)
/* GNU m4 now no macro expansion in C style comments */
```




## Maple


```Maple
x := 4: x; # Everything on this line, after this, is a comment.

17; (* This
   is
   a multiline comment *) 23.4;
```

```txt

                               4
                               17
                              23.4

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
(*this is a comment*)
```

It can be used everywhere and nested if necessary:

```Mathematica
If[a(*number 1*)<(* is smaller than number 2*) b, True (*return value (*bool true*)*), False (*return bool false*)]
```

evaluates to:

```Mathematica
If[a < b, True, False]
```



## MATLAB



```MATLAB
%This is a comment
%% Two percent signs and a space are called a cell divider
```



## Maxima


```maxima
/* Comment
  /* Nested comment */
*/
```


## MAXScript


```maxscript
-- Two dashes precede a single line comment

/* This is a
   multi-line comment */
```



## MBS



```mbs
! A pling in a line starts a comment

INT   n:=5   ! Comments can appear at the end of a line

/* A comment block can also be defined using climbstar and starclimb symbols.
  This allows comments to be stretched across several lines */
```



## Metafont


```metafont
% this is "to-end-of-line" comment
```



## Microsoft Small Basic

Microsoft Small Basic uses the quote symbol to mark it's comments. After placing a quote everything in that line will be ignored.


```smallbasic
' This is a comment
i = i + 1  ' You can also append comments to statements
```



## min

```min
; this is a comment
1 1 + ; add one and one together
```



## Mirah


```mirah
puts 'code' # I am a comment
/* This is
 * a multiple
 * line comment */

```



## mIRC Scripting Language


```mirc
;Single Line Comment
/*
Multiple
Line
Comment
*/
```


=={{header|Modula-2}}==
<lang Modula-2>(* Comments (* can nest *)
   and they can span multiple lines.
 *)
```


=={{header|Modula-3}}==

```modula3
(* Comments (* can nest *)
   and they can span multiple lines.
 *)
```



## Monte



```Monte

# This comment goes to the end of the line
/** This comment is multi-line.
    Yes, it starts with a two stars
    and ends with only one.
    These should only be used for docstrings. */

```



## MontiLang



```MontiLang

/# This is a comment #/
/#
comments can span multiple lines
nested comments are not supported #/

```



## MOO


```moo
"String literals are technically the only long-term comment format";
// Some compilers will, however, compile // one-liners to string literals as well (and vice-versa)
/* Classical C-style comments are removed entirely during compile */
```



## Neko


```ActionScript
// Single line comment, of course!

/*
Multi line comment!
*/

/**
Documentation block
<doc>can include XML parsed nodes between doc tags</doc>
**/
```



## Nemerle


```Nemerle
// This comment goes up to the end of the line
/* This
is
a
multiline
comment */
```



## NESL


```nesl
% This is a comment. %
```



## NetRexx

NetRexx supports block-comments and line comments.  Block comments are started with a <code>'''/*'''</code> and terminated with a <code>'''*/'''</code>.  Line comments follow a <code>'''--'''</code> sequence anywhere on a line.
NetRexx supports nested comments (see [[#REXX|REXX]]).

```NetRexx
/*

 NetRexx comment block

*/

-- NetRexx line comment


```



## NewLISP

A comment is started by <code>;</code> and reaches to the end of the line.

```lisp
; This is a comment
```



## Nim


```Python
# Nim supports single-line comments

var x = 0 ## Documentation comments start with double hash characters.

var y = 0 ## Documentation comments are a proper part of the syntax (they're not discarded by parser, and a real part of AST).

#[
There are also multi-line comments
Everything inside of #[]# is commented.
]#

# You can also discard multiline statements:

discard """This can be considered as a "comment" too
This is multi-line"""

```



## NSIS


```nsis

# This is a comment that goes from the # to the end of the line.
; This is a comment that goes from the ; to the end of the

/* This is a
multi-line
comment */

```


=={{header|Oberon-2}}==

```oberon2

(* this is a comment *)
(*
   and this is a
   multiline comment
   (* with a nested comment *)
*)

```



## Objeck


```objeck

#This is a comment.
# This is other comment.

#~ This is a comment too. ~#

#~ This is a
multi-line
comment ~#

```


=={{header|Objective-C}}==
:''See [[Comments#C|C]]''


## OCaml



```ocaml
(* This a comment
   (* containing nested comment *)
 *)

(** This an OCamldoc documentation comment *)
```



## Oforth

Oforth has only single line comment (inside or outside definition)


```oforth
// This is a comment...
```



## Octave


```octave
# I am a comment till the end of line
% I am a comment till the end of line

%{
  This comment spans
  multiple lines
%}

```



## ooRexx

Comments in ooRexx follow the same rules as [[#REXX|REXX]] and [[#NetRexx|NetRexx]]


```ooRexx
/*
  Multi-line comment block
 */

-- this type of comment works in ooRexx, NetRexx and some of the more popular REXX implementations like Regina

hour = 0  -- which is, like midnight, dude.

hour = 12 /* time for lunch! works as well (and really everywhere) */


```



## Openscad


The openscad geometry compiler supports C++ style comments:

```openscad

// This is a single line comment

/*
  This comment spans
  multiple lines
*/


```



## OxygenBasic


```oxygenbasic

'  Basic line comment
;  Assembly code line comment
// C line comment
/* C block comment */

```



## Oz


```oz
% one line comment

%% often with double "%" because then the indentation is correct in Emacs

/* multi line
   comment
*/

```



## PARI/GP

Comments are similar to C. The block comment is identical: <code>/* comment */</code>.  The line comment uses backslashes instead of slashes: <code>\\ comment</code>.


## Pascal


```pascal
(* This is a comment.
   It may extend across multiple lines. *)

{ Alternatively curly braces
  can be used. }

(* This is a valid comment in Standard Pascal,
   but not valid in [[Turbo Pascal]]. }

{ The same is true in this case *)
```


In Pascal, comments cannot be nested.


## PASM



```pasm
# This is a comment
print "Hello\n"    # This is also a comment
end
```



## Peloton

Peloton encloses all comments inside <@ OMT></@> (fixed length opcode) or <# OMIT></#> (variable length opcode) whether single- or multi- line.

```html

<@ OMT>This is a
multiline
comment</@>

```

OMT suppresses evaluation of everything contained. There are a variety of extra opcodes which can be used to control how OMT functions at run time.


## Perl

Single line comment


```perl
# this is commented
```


These may also be at the end of a line


```perl
my $var = 1; # this is the comment part
```


Multi-line comments for inline documentation (Plain Old Documentation, or POD in Perl parlance) follow the format:


```perl
=pod

Here are my comments
this is multi-line

=cut
```


Note that technically, both of the lines beginning with the equals sign must be surrounded on either side for compatibility with all "POD" parsers.

Note also that any string beginning with an equals sign, and that appears in the initial column of a line, begins a multi-line comment. It does not have to be a POD "command:" the following are all valid:


```perl
=head1
=head4
=over 4
=Any Old String
```


Such blocks always end in =cut.

For more info, type at a command prompt (or into a search engine): "perldoc perlpod"


## Perl 6


'''Single-line comments'''

A single-line comment starts with # and extends to the end of the line.


```perl6
# the answer to everything
my $x = 42;
```


'''Multi-line comments'''

A multi-line comment starts with #` and followed by the commented text enclosed by bracketing characters (e.g., (), [], {}, 「」, etc.).


```perl6
#`(
    Comments beginning with a backtick and one or more opening bracketing characters are embedded comments.
    They can span more than one line…
)

my $y = #`{ …or only part of a line. } 3;
```


Multi-line comments can also be embedded into code.


```perl6
for #`(each element in) @array {
    say #`(or print element) $_ #`(with a newline);
}
```


Using more than one bracketing character lets you include an unmatched close bracket, as shown below.


```perl6
#`{{
  This close curly brace } won't terminate the comment early.
}}
```


'''Pod comments'''


```perl6
=begin comment

Pod is the successor to Perl 5's POD. This is the simplest way to use it for multi-line comments.
For more about Pod, see Pod: https://docs.perl6.org/language/pod

=end comment
```


Pod also provides declarator blocks which are special comments that attach to some source code and can be extracted as documentation. They are either #| or #= and must be immediately followed by either a space or an opening curly brace. In short, blocks starting with #| are attached to the code after them, and blocks starting with #= are attached to the code before them.


```perl6
#| Compute the distance between two points in the plane.
sub distance(
    Rat \x1, #= First point's abscissa,
    Rat \y1, #= First point's ordinate,
    Rat \x2, #= Second point's abscissa,
    Rat \y2, #= Second point's ordinate,
){
    return sqrt((x2 - x1)**2 + (y2 - y1)**2)
}
```



## Phix

Single line comment:

```Phix
-- This is a comment
```



Nestable multiline comments:

```Phix
/*
This is a comment
procedure oldproc()
   /*
     This is also a comment
   */
   puts(1,"kill me now")
end procedure
*/
puts(1,"this is not a comment")
```

```txt

this is not a comment

```



## PHP

Single line comment:


```php
# this is commented
// this is commented
```


These may also be at the end of a line:


```php
$var = 1; # this is the comment part
$var = 1; // this is the comment part
```


Basic syntax for multi-line comments:


```php
/*
Here are my comments
this is multi-line
*/
```


Note that; it is more common to see phpDocumentor styled multi-lined comments:


```php
/**
 * phpdoc Comments
 * @todo this is a todo stub
 */
```



## PicoLisp


```PicoLisp
# The rest of the line is ignored
#{
   This is a
   multiline comment
}#
NIL
Immediately stop reading this file. Because all text in the input file following
a top-level 'NIL' is ignored.

This is typically used conditionally, with a read-macro expression like
`*Dbg
so that this text is only read if in debugging mode.
```



## Pike


```pike
// This is a comment.
/* This is a
   multi
   line
   comment */

int e = 3; // end-of-statement comment.
```



## plainTeX


The default raw/bare [[TeX]] assigns the category code 14 (comment character) to the character %, and
[[plainTeX]], as also [[LaTeX]] (see here [[Comments#LaTeX|Comments in LaTeX]], does not change
it; so the % starts a to-end-of-line comment in many TeX macro packages.


```tex
% this is a comment
This is not.
```


The final newline character is eaten and since it normally behaves like a space, the comment can
be used to hide the newline:


```tex
\def\firstpart#1{under#1}
\def\secondpart{able}
\def\complete{\firstpart{stand}%
\secondpart}

\complete
```


Outputs <tt>understandable</tt>; without % it would output <tt>understand able</tt>.


## PL/I


```pli
/* This is a comment. */
```


```pli
/*
This is a multiline comment.
*/
```

Note: In PL/I, comments cannot be nested.


## PL/SQL

Single line comment:


```plsql
--this is a single line comment
```


Multiline comment:


```plsql
/*
this is a multiline
comment
*/
```


End of line comment:


```plsql
v_var number; --this is an end of line comment
```



## Pop11


Pop11 has two kinds of comments: endline and C-like. Endline comment
begins with tree consecutive semicolons and ends at the end of line:


```pop11
;;; This is a comment>
```


C-like comments may be multiline:


```pop11
/* First line
   Second line */
```


C-like comments (unlike C) may be nested:


```pop11
/* This is a comment /* containing nested comment */ */
```


One can also use conditional compilation to comment out sections of code


```pop11
#_IF false
some code
#_ENDIF
```


however, commented out part must consist of valid Pop11 tokens. In particular, C-like comments must balance and strings must be terminated.
The following is an error:


```pop11
#_IF false
This w'ont work
#_ENDIF
```


because apostrophe starts an unterminated string.


## PostScript

<lang>
%This is a legal comment in PostScript

```


## PowerShell


```powershell
# single-line comment
```


```powershell
<# multi-line
   comment #>
```



## Processing


```processing
// a single-line comment

/* a multi-line
   comment
*/

/*
 * a multi-line comment
 * with some decorative stars
 */
```



## ProDOS

I don't know why this is even a task because it should be included in any decent programming language.

```ProDOS
IGNORELINE your text here>
```



## Prolog


```prolog
% this is a single-line comment that extends to the end of the line
```


```prolog
/* This is a
multi-line comment */
```



## PureBasic

PureBasic uses the ";" symbol to mark its comments.  All text entered after ";" on a line is ignored by the compiler.

```PureBasic
;comments come after an unquoted semicolon and last until the end of the line
foo = 5 ;This is a comment
c$ = ";This is not a comment"  ;This is also a comment
```



## Python

Python uses the "#" symbol to mark it's comments. After placing a "#", everything to the right of it in that line will be ignored.


```python
# This is a comment
foo = 5 # You can also append comments to statements
```


Certain 'do nothing' expressions resemble comments


```python
"""Un-assigned strings in triple-quotes might be used
   as multi-line comments
"""

'''
   "triple quoted strings" can be delimited by either 'single' or "double" quote marks; and they can contain mixtures
   of other quote marks without any need to \escape\ them using any special characters.  They also may span multiple
   lines without special escape characters.
'''
```


Note that strings inserted among program statements in Python are treated as expressions (which, in void context, do nothing).  Thus it's possible to "comment out" a section of code by simply wrapping the lines in "triple quotes" (three consecutive instances of quotation marks, or of apostrophes, and terminated with a matching set of the same).


### Documentation Strings


Python makes pervasive use of strings which immediately follow class and function definition statements, and those which appear as the first non-blank, non-comment line in any module or program file. These are called "documentation" strings or "docstrings" for short; and they are automatically associated with the '''__doc__''' attribute of the class, function, or module objects in which they are defined.  Thus a fragment of code such as:

```python
#!/usr/bin/env python
# Example of using doc strings
"""My Doc-string example"""

class Foo:
     '''Some documentation for the Foo class'''
     def __init__(self):
        "Foo's initialization method's documentation"

def bar():
    """documentation for the bar function"""

if __name__ == "__main__":
    print (__doc__)
    print (Foo.__doc__)
    print (Foo.__init__.__doc__)
    print (bar.__doc__)
```



... would print each of the various documentation strings in this example. (In this particular example it would print two copies of the first doc string which because __doc__ in the "current" name space is the same as __main__.__doc__ when our program is running as a script). If some other script were to ''import'' this file (under the name "example" perhaps) then "My Doc-string example" would be the value of ''example.__doc__''

Python "docstrings" are used by a number of tools to automatically generate documentation (for most of the Python standard libraries, classes, functions, etc, as well as for user programs which define docstrings). They are also used by tools such as ''doctest'' to automatically derive test suites from properly formatted examples of class instantiations, function invocations and other usage samples.  The standard ''pydoc'' utility can search through Python source trees generating documentation and can function as a local web server allowing a programmer to browse "live" hyperlinked documentation of their project.

(As noted above extraneous strings interspersed throughout a Python source file can be used as comments, though this is rarely done in practice; only those strings which lexically follow the definition of a class, function, module or package are assigned to __doc__ attributes in their respective name spaces).

## R


```rsplus
# end of line comment>
```



## Racket


```racket

; this is a to-end-of-line coment

#| balanced comment, #| can be nested |# |#

#;(this expression is ignored)

#; ; the following expression is commented because of the #; in the beginning
(ignored)


```



## Raven



```raven
 # this is a comment>
```




## REBOL



```REBOL

; This is a line comment.

{ Multi-line strings can
  be used as comments
  if you like }

```


Functions have special commenting options which make them self documenting:


```REBOL

plus2: func [
    "Adds two to a number."
    n [number!] "The number to increase."
][
    n + 2
]

```


If you say "help plus2" at REBOL's REPL, you'll get this help information:

    USAGE:
        PLUS2 n

    DESCRIPTION:
         Adds two to a number.
         PLUS2 is a function value.

    ARGUMENTS:
         n -- The number to increase. (Type: number)


## Retro


```Retro
( comments are placed between parentheses. A space must follow the opening parenthesis. )
```



## REXX

It should be noted that comments in the REXX language support '''nested''' comments, so comments aren't totally ignored by the REXX interpreter (and compiler).

REXX comments are scanned and preserved for use by the   '''sourceline'''   BIF.   [The   '''sourceline'''   BIF allows the retrieval of any or all lines of source (of the REXX program).]

Also, redundant blanks are removed and processed/shown for various   '''trace'''   options   ('''trace'''   is a REXX statement that may show various interpretation/execution stages of REXX statements (clauses, values, etc.),   including comments and also blank lines).   The   '''trace'''   statement is also used for interactive debugging.

Nested comments must have matching delimiters, so the contents of the comments can't just be willy-nilly characters.


Also, some REXX interpreters show the comment (if part of a REXX statement) as part of the information displayed when (if) a   '''syntax'''   error occurs and an informative error message is generated.   For instance, in the program   (named c:\COMMENTD.REX):

```rexx
/*REXX program that demonstrates what happens when dividing by zero.  */
y=7
say 44 / (7-y)      /* divide by some strange thingy.*/
```

'''output'''   when using the Regina REXX interpreter:'

```txt

     3 +++ say 44 / (7-y)      /* divide by some strange thingy.*/
Error 42 running "c:\COMMENTD.REX", line 3: Arithmetic overflow/underflow
Error 42.3: Arithmetic overflow; divisor must not be zero

```

'''output'''   when using the R4 REXX interpreter:'

```txt

Error 42 : Arithmetic overflow/underflow (SYNTAX)
Information: Divide by zero
Error occurred in statement# 3
Statement source: say 44/(7-y)
Statement context: c:\commentdv.rex, procedure: commentdv

```

'''output'''   when using the Personal REXX interpreter:'

```txt

     3 +++ say 44 / (7-y)      /* divide by some strange thingy.*/
Error 42 on line 3 of C:\COMMENTD.REX: Arithmetic overflow/underflow

```



The REXX language was heavily modeled after PL/I, both languages have the same comment construct, but PL/I doesn't support nested comments.


Nested comments allow an easy way to comment large chunks of code where the commented-out code has its own comments.

```rexx
/*REXX program to demonstrate various uses and types of comments. */

/* everything between a "climbstar" and a "starclimb" (exclusive of literals) is
   a comment.
                         climbstar =  /*   [slash-asterisk]
                         starclimb =  */   [asterisk-slash]

            /* this is a nested comment, by gum! */
            /*so is this*/

Also, REXX comments can span multiple records.

There can be no intervening character between the slash and asterisk  (or
the asterisk and slash).  These two joined characters cannot be separated
via a continued line, as in the manner of:

       say 'If I were two─faced,' ,
           'would I be wearing this one?' ,
           '      --- Abraham Lincoln'

 Here comes the thingy that ends this REXX comment. ───┐
                                                       │
                                                       │
                                                       ↓

                                                       */

    hour = 12       /*high noon                   */
midnight = 00       /*first hour of the day       */
   suits = 1234     /*card suits:   ♥  ♦  ♣  ♠    */

hutchHdr = '/*'
hutchEnd = "*/"

    /* the previous two "hutch" assignments aren't
       the start  nor  the end of a REXX comment. */

  x=1000000 **   /*¡big power!*/   1000

/*not a real good place for a comment (above),
  but essentially, a REXX comment can be
  anywhere whitespace is allowed.            */
```


Some REXX implementations (e.g. Regina, ooRexx) also allow line comments which start with a   '''--'''   sequence and extend to the end of the line:

[Note:   Regina REXX releases since 3.4 allow this type of single-line comment ''only'' if the option '''noSingle_Line_comments''' isn't present in the '''REGINA_OPTIONS''' environmental variable. So, Regina ''may'' allow the use of single-line comments.   That is to say, one can't depend (or assume) that the Regina extension (of single-line comments) for the aforementioned option will be or not be present (supported and/or allowed) in the environment being used].   Which means one should set this option to their preference if using Regina REXX later than version 3.3.   Since Single-Line comments aren't part of the ANSI standard nor a part of the Classic REXX language, the use of which would make the REXX code non-portable.]

<!--

The default is apparently that the nice -- feature  (which I dont have on TSO) is available.

That comment should probably be moved to the discussion page.   One can't assume that a particular person   (or administrator?)   has set the environmental variable (which may be controlled or restricted by policy -- this is especially true of company-owned PCs, many companies place ''severe'' restrictions about what users are allowed to change in the environment -- if changes are allowed at all, let alone installing software);   it may already have been configured/installed/copied that way by someone else (such as a network or system administrator).   It is important to recognize that this extension may or may not be available, depending on an environmental variable setting, and/or depending upon which version of Regina is being used, and it can't be assumed that the latest version of the Regina REXX interpreter has been installed or being used.   During regression testing, many possible versions (of Regina) would be used.   Not all programmers think that feature is nice, which is quite subjective, and that is one reason people refrain from praising certain extensions -- one reason is that it breaks the ASCII standard of allowing multiple (unary) negative prefixes.   Furthermore, not all (older) versions of Regina have this extension, some people have to use an older version of Regina for various reasons.

The fact that Regina REXX is the only Classic REXX to support this feature makes the use of single-line comments non-portable (to other Classic REXX interpreters).   -- ~~~~

-->



```rexx
-- A REXX line comment (maybe)
say "something" -- another line comment (maybe)
```

It should be noted that the above type of comments are not part of Classic REXX, nor are they described nor sanctioned in the REXX ANSI standard.


## Ring

in Ring language we can use // or # for one line comments:

```ring

//this is a single line comment
#this also a single line comment!

```


and for multi-line comments we use /* */:

```ring

/*This is a multi-line
comment that will be completely
ignored by the compiler/interpreter
*/

```



## RLaB


RLaB only has single line comment indicator, as in following examples


```RLaB

x = "code" # I am a comment
x = "code" // Here I comment thee
#  matlab-like document line
// C++ like document line

```



## Robotic

Comments can only be created in one way:

```robotic

. "This is a comment line"

. "Print Hello world"
* "Hello world."

. "This is the only way to comment a line in Robotic"

```


Although these are comments, the interpreter doesn't completely ignore it. For one, the code speed can be affected if they are ever encountered. Also, if an @ character exists at the beginning of the comment line, then the rest of the string after it is now the Robot's new name (there is a 14 character limit).

Example of changing the robot's name:

```robotic

. "@NewRobotName"

```


This would then change the robot's name from whatever it was before to "NewRobotName" instead.


## Ruby



```ruby
x = "code" # I am a comment

=begin hello
I a POD documentation comment like Perl
=end puts "code"
```



## Run BASIC

Comments have a ' (single quote) or REM for remarks

```runbasic
'This is a comment
REM This is a comment

print "Notice comment at  the end of the line."          'This is a comment
print "Also notice this comment at the end of the line." : REM This is a comment

```



## Rust


```rust
// A single line comment

/*
    This is a multi-line (aka block) comment

    /*
        containing nested multi-line comment
        (nesting supported since 0.9-pre https://github.com/mozilla/rust/issues/9468)
    */
*/


/// Outer single line Rustdoc comments apply to the next item.

/**
    Outer multi-line Rustdoc comments.

 *  Leading asterisk (*) in multi-line Rustdoc comments
 *  is not considered to be part of the comment text,
 *  blanks and tabs preceding the initial asterisk (*) are also stripped.
*/

fn example() {

    //! Inner single line Rustdoc comments apply to their enclosing item.

    /*!
        Inner multi-line Rustdoc comments.
        See also https://github.com/mozilla/rust/wiki/Doc-using-rustdoc
    */
}

#[doc = "Unsugared outer Rustdoc comments.
        (outer attributes are not terminated by a semi-colon)"]
fn example() {
    #[doc = "Unsugared inner Rustdoc comments.
            (inner attributes are terminated by a semi-colon)
            See also https://github.com/mozilla/rust/blob/master/doc/rust.md#attributes"];
}
```



## SAS


```sas
/* comment */

*another comment;

* both
  may
  be
  multiline;
```



## Sather


```sather
-- a single line comment
```



## Scala



```scala
// A single line comment

/* A multi-line
   comment */
```



## Scilab

Specify a comment starting with // to the end of line
<lang>// this is a comment
i=i+1 // this is a comment
```



## Scheme


```scheme
; Basically the same as Common Lisp
; While R5RS does not provide block comments, they are defined in SRFI-30, as in Common Lisp :

#| comment
... #| nested comment
... |#
|#

; See http://srfi.schemers.org/srfi-30/srfi-30.html

```



## sed


```sed
# a single line comment
```



## Seed7



```seed7
# A single line comment

(* A multi-line
    comment *)

(* In Seed7,
(* comments can be nested. *) *)
```



## Set lang

<lang set_lang>> Comments start where a > (greater than symbol) starts
set a 0 > Comments may start after a Set command
```



## SETL


```setl
print("This is not a comment"); -- This is a comment
$ For nostalgic reasons, this is also a comment.
```



## Sidef

Single line comment

```ruby
# this is commented
```

These may also be at the end of a line

```ruby
var i = 1; # this is the comment part
```

Embedded comments

```ruby
var distance #`{in meters} = (30 #`{meters} * 100 #`{seconds});
say distance; # prints: 3000
```


Multi-line comments

```ruby
/*
    This is a multi-line comment
*/
```



## Simula

The same as Algol 60:
<lang>COMMENT This is a comment for Simula 67;
```

And an new form:
<lang>!This is a comment for Simula 67;
```

'''Pitfall''': it's ''not'' easy to ''comment-out'' parts of code:

```Simula
!OutText("Dying."); !Outimage; !terminate_program;
```



## Slate


```slate
"basically the same as smalltalk"
```



## Smalltalk



```smalltalk
"Comments traditionally are in double quotes."
"Multiline comments are also supported.
 Comments are saved as metadata along with the source to a method.
 A comment just after a method signature is often given to explain the
 usage of the method. The class browser may display such comments
 specially."
```



## smart BASIC


<lang>'Single line comments are preceded by a single quote or the command REM

PRINT "Hello" 'Single line comments may follow code

PRINT "Hello" REM You can also use the command REM following code

/*
Multi-line comments
are surrounded by
mirrored slash
and asterisk
*/

/*Multi-line comments do not have to actually have multiple lines*/

/* Spaces before or after comment bounds are optional.*/

/* A comment can also follow another comment */  'Like this

Some programmers like to do this to allow for /* Procedural comments */ followed by 'Programmer's notes.
```



## SNOBOL4



```SNOBOL4

* An asterisk in column 1 is the standard Snobol comment
* mechanism, marking the entire line as a comment. There
* are no block or multiline comments.

*               Comments may begin at
*               any position on the line.

- A hyphen in column 1 begins a control statement.
- Unrecognized control statements are ignored and
- may also mark comment lines. Not recommended.

                   ;* The semicolon statement separator
    output = 'FOO' ;* begins a new statement. This idiom
    output = 'BAR' ;* simulates an asterisk in the first
                   ;* column, allowing end of line comments.

END

Any text after the required END label is ignored.
```



## SNUSP

As with [[Brainfuck]] and [[Befunge]], any character that is not part of the language is ignored and can be used as commentary, and you can add comments anywhere the instruction pointer is not expected to traverse.  Reserved characters are:
* Core: + - &gt; &lt; , . ? ! / \ $ #
* Modular: @ #
* Bloated: : ; & %

As a matter of convention, the characters '=' and '|' are used for spacing to indicate horizontal and vertical flow of control, respectively.


## SPL


```spl
'This is single-line comment

''This is
multiline comment''
```



## SQL

The double hyphen ( -- ) is used to include a comment on an SQL statement.

The comment appears on the same line as the statement:

```sql
SELECT * FROM mytable -- Selects all columns and rows
```

or before:

```sql
-- Selects all columns and rows
SELECT * FROM mytable
```

or after:

```sql
SELECT * FROM mytable
-- Selects all columns and rows
```



## SQL PL

Single line comment:


```sql pl

--This is a single line comment.

```


Multiline comment:


```sql pl

/* This is
a multiline
comment */

```


Another way to do multiline comments


```sql pl

(= This is
a multiline
comment =)

```


End of line comment:


```sql pl

declare myvar number; --This is an end of line comment.

```


Comments work the same as in [[SQL]].


## SSEM

The SSEM can only be programmed in pure binary, by setting front panel switches: the concepts of "text" and "source file" (both mentioned in the specification) are therefore not directly applicable to it. If binary numbers have any mnemonic or explanatory value for you, however, there is a way of including information in your program that the computer will ignore. This is a direct result of the machine's rather poor code density. Each 32-bit instruction word consists of (a) a five-bit address field giving the operand, (b) eight unused bits, (c) a three-bit instruction field giving the operation to be performed, and (d) sixteen more unused bits. If the instruction field is set to <tt>011 Test</tt> or <tt>111 Stop</tt>, even the address field is unused. In the case of a <tt>Sub.</tt> instruction, finally, the leftmost bit of the instruction field is disregarded: <tt>001</tt> and <tt>101</tt> both mean "subtract". We therefore have at least 24 and sometimes 25 or 29 bits in each instruction that we can, if we like, use for comments. The word

```ssem
00101010010001000100100100001100>
```

will be understood by the machine as <tt>Add 20 to CI</tt>, a normal instruction. But it also fits four comment characters into the unused bits, employing a simple five-bit encoding where <tt>A</tt>=0 and <tt>Z</tt>=25. The instruction breaks down as follows:

<tt>00101 -- </tt>address field = 20

<tt>01001 -- </tt>"comment" field = 18

<tt>000 -- </tt>three unused bits

<tt>100 -- </tt>instruction field = <tt>Add to CI</tt>

<tt>01001 -- </tt>"comment" field = 18

<tt>00100 -- </tt>"comment" field = 4

<tt>01100 -- </tt>"comment" field = 12

<tt>0 -- </tt>unused bit

Applying our simple alphabetic encoding, we see that the "spare" bits spell out 18, 18, 4, 12 = S, S, E, M.

More realistically, you can include comments when you are drafting your program using mnemonic notation and then simply leave the comments out when it comes time to toggle the program in.


## Standard ML



```sml
(* This a comment
   (* containing nested comment *)
 *)
```



## Squirrel


```squirrel
//this is a single line comment

#this is also a single line comment

/*
    this is a multi-line comment
*/
```



## Stata


```stata
* Line comment: must be used at the beginning of a line (does not work in Mata)

// Line comment until the end of the line

/* Multiline comment

*/
```


## Swift


```swift
// this is a single line comment
/* This a block comment
   /* containing nested comment */
 */

///This is a documentation comment

/**
  This is a documentation block comment
*/
```



## Tcl


Tcl follows the usual scripting language rules: a comment starts at a "#" symbol, which can be placed after a command if that is terminated by a semicolon:


```tcl
# comment on a line by itself. The next is a command by itself:
set var1 $value1
set var2 $value2 ; # comment that follows a line of code
```


The reason for the need for a semi-colon on a trailing comment is this:

"If a hash character (“#”) appears at a point where Tcl is expecting ''the first character of the first word of a command'', then the hash character and the characters that follow it, up through the next newline, are treated as a comment and ignored. '''The comment character only has significance when it appears at the beginning of a command.'''" (from the [http://www.tcl.tk/man/tcl8.5/TclCmd/Tcl.htm#M29 Tcl man page] -- emphasis [[User:Glennj|mine]])

The "#" symbol has no special meaning if it is not where a command would appear -- it's just data.  (Syntax highlighters often get this wrong.)


```tcl
set aList {foo}
lappend aList # bar
puts $aList           ;# ==> prints "foo # bar"
puts [llength $aList] ;# ==> 3
```


TCL has no native multi-line comment format. However, in most circumstances, a multi-line comment can be faked by wrapping it within a block that will never be executed:


```tcl
if 0 {
   Comments...
}
```



## Tern

:''See [[Comments#Java|Java]]''

=={{header|TI-83 BASIC}}==
There is no 'proper' way of adding comments in TI-BASIC, however there are ways to add text to a program that will be ignored by the calculator.

One common approach is to put the comment in a string which is not stored anywhere:

```ti83b
:"THIS IS A COMMENT
```

However this will change the Ans variable.

This approach, while messier, does not affect the Ans variable:

```ti83b
:If 0
:THIS IS A COMMENT
```



=={{header|TI-89 BASIC}}==


```ti89b
© This is a comment. Everything from © to the end of the line is ignored.
```



## Toka


There are two ways to add comments in Toka. For full lines, or at the end of
a line, the shebang is normally used:


```toka
#! Everything on this line (after the shebang to the left) will be ignored.
```


The shebang comments can not be used inside of functions.

In addition, Toka also accepts parenthetical comments. These are enclosed in parenthesis, and are often used for stack comments or comments inside functions.


```toka
[ ( a b -- c )
  ... ] is myword
```


In addition, parenthetical comments can span multiple lines.


```toka
( This is a
  simple, multi-line
  comment )
```


Since comments are provided by actual functions, the comment function must be whitespace delimited, just as with all other functions in Toka.

A final way to include text in a file is to mark a false ending with '''end.'''


```toka
... code ....
end.
Nothing following the end. will be evaluated by Toka.
```



## TorqueScript



```torquescript
//This is a one line comment. There are no other commenting options in TorqueScript.>
```



## TPP



```tpp
--## comments are prefixed with a long handed double paintbrush
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
- This is a comment

```



## TXR



```txr
@# old-style comment to end of line
@; new-style comment to end of line
@(bind a ; comment within expression
       "foo")
```



## UNIX Shell

```bash
#!/bin/sh
# A leading hash symbol begins a comment.
echo "Hello"      # Comments can appear after a statement.

# The hash symbol must be at the beginning of a word.
echo This_Is#Not_A_Comment
#Comment
```


=
## C Shell
=

```csh
#!/bin/csh -f

# C Shell has a similar comment syntax, but only allows comments in a
# script file, not in terminal input.

echo Hello#With C Shell, the hash can also be in the middle of a word.
```


=
## es
=

```es
# Comments in es (extensible shell) look like those of other shells.

echo Hello#With es, the hash can also be in the middle of a word.
```



## Unlambda

Unlambda comments start with <code>#</code> and extend to the end of the line:

```txt

# this is a comment.

```

Note that comments don't need to start at the beginning of a line, e.g.

```txt

`  # apply
.a # output "a"
i  # identity

```

is equivalent to

```txt

`.ai

```



## Ursa

Comments in Ursa must be on a single line, and are denoted by a #

```ursa
# this is a comment
# this is another comment

```



## Ursala

There are lots of ways to have comments in Ursala.
Here are the conventional ones.

```Ursala
# this is single line a comment

# this is a\
continued comment

(# this is a
multi-line comment #)

(# comments in (# this form #) can (#
be (# arbitrarily #) #) nested #)

---- this is also a comment\
and can be continued

###
The whole rest of the file after three hashes
is a comment.
```



###  Commenting out code


There are also ways to comment out sections of code during testing.
An individual item of a syntactically correct list or aggregate is commented
out like this.

```Ursala
x = <1,## 2,3>
```

The 2 is ignored but 1 and 3 aren't. This also works with
nested aggregates and multiple lines.

```Ursala
a =

<
   'to',
   ## <
      'be',
      'or'>,
   'not',
   'to',
   ## 'be'>
```

A syntactically correct declaration can be commented out like this.

```Ursala
foo = 1

##

bar = 2

baz = 3
```

As far as the compiler is concerned, bar is not defined, but foo and baz are.
It wouldn't matter if bar took multiple lines.


###  Comments in compiled files

The compiler can be directed
to embed comments in executable files and libraries it generates without
affecting their semantics.

```Ursala
#comment -[
I document the source text but will also be embedded in
the output library or executable file.]-

#comment gpl'3'
```

The latter comment puts the standard GPL license notification in the output file.


###  Comments as diagnostics


A function f annotated with a crash dump wrapper expressed like this
during debugging

```Ursala
my_input_type%C f
```

is equivalent to just f when changed to this in the production code.

```Ursala
my_input_type%Ck f
```



###  Comments as hooks


Compiling with the --depend command line option makes the compiler only scan for the #depend'ed
expressions and send them to standard output.

```Ursala
#depend <this,expression> is (parsed)* but {
   otherwise,
   ignored}
```

This way, scripts and source management tools can have information passed to them from
the programmer by running the compiler instead of re-implementing their own parsers.


## VBA

A comment starts with a quote (') and it ends at end of line

```vb
' This is a VBA comment

```




## VBScript

A comment starts with a quote (') and it ends at end of line

```vb
' This is a VBScript comment

```



## Verbexx


```verbexx

//////////////////////////////////////////////////////////////////////////////////////////////
//
// Line Comments:
//
### =======

//
@VAR v1 = 10; // Line comments start from the "//" and continue to end of the line.
//               (normal code can appear on the same line, before the //)
//
//   Line comments can span a complete line, or start in the middle of a line.
///
//// Additional // chars and /* /*  /[  ]/ and  /] are ignored
//// Line comments can be appear to be nested, since any additional // is ignored.
///
//   Note: // can appear in strings without triggering a line comment
//         // cannot appear inside an operator (or verbname), since a line comment
//            would start
//
/////////////////////////////////////////////////////////////////////////////////////////////

/********************************************************************************************
 *
 *  Block Comments:
 *
### ========

 *
 ********************************************************************************************/
//*
//*  These start with /* and end with the next */ .  They cannot be nested, since the first */
//*  will end the block comment.  For example, the comment, /* /* */ */ would end after the
//*  first */. Note that /* is ignored inside a block comment, as are   //   /[   /] and  /].
//*
//*  Also note that something like the following will cause trouble in a block comment:
//*
//*    /* comments                          //
//*     * more comments                     //   */  (the // does not prevent the */ from ending
//*     * (no longer part of the comment)   //        block comment)
//*     */
//*
//*    Note: /* can appear in strings without triggering the start of a block comment
//*          /* cannot appear inside an operator (or verbname), since a line comment will
//*             start, although */ is allowed inside an operator (verbname).  Commenting
//*             out such a verbname may cause problems.
//*
//*    Note: Since string literals are not recognized in block comments, */ appearing
//*          in a string literal inside a block comment (perhaps commented-out code)
//*          will cause the block comment to end.
//*
//*    Note: It is an error to start a block comment and not end it, so that it is still
//*          in progresss when the end-of-file is reached.
//*
//*    Block comments can appear inside lines of code:
//*
/*1*/@VAR/*2*/v2/*3*/=/*4*/20/*5*/;/*6*/  // a line comment can follow block comments on the
                                           // same line

/[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
/[]                                                                                          []
/[]     Nestable Block Comments:                                                             []
 []
### ==================
                                                             []/
 []                                                                                          []/
 [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]/

//[]
//[]  These start with /[ and end with the next matching ]/ .  Additional occurrences
//[]  of /[ ... ]/ can appear inside a nestable block comment.  The nestable block comment
//[]  will end only when the nest level reaches 0.  Note that /* is ignored inside a nestable
//[]  block comment, as are  */   //  and  /].
//[]
//[]  Nestable block comments can be used to comment out blocks of code containing line
//[]  comments or regular comments, and even balanced and well-formed nestable block comments.
//[]
//[]    Note: /[ can appear in strings without triggering the start of a block comment.
//[]          However, strings literals are not recognized inside a nestable block comment, so
//[]          any appearances of /[ and /] inside a string literal in a nestable block commment
//[]          will affect the nest level, and may cause problems.
//[]
//[]    Note: It is an error to start a nestable block comment and not end it, so that it is
//[]          still in progresss when the end of file is reached.
//[]
//[]    Nestable block comments can appear inside lines of code:
//[]
/[1]/@VAR/[2]/v3/[3]/=/[4]/30/[5]/;/[6]/  // a line comment can follow nestable block comments
                                          // on the same line

@SAY v1 v2 v3;                            // should see:   10 20 30

/]
/
### ===========================================================================================
\
|                                                                                                 |
|   /] starts a block comment that lasts until the end of the current file.  Everything after     |
|   the /] is ignored.                                                                            |
|                                                                                                 |
\
### ===========================================================================================
/

```



## Verilog


```Verilog
// Single line commment.

/*
   Multiple
   line
   comment.
*/
```



## VHDL


```vhdl
-- Single line commment in VHDL
```



## Vim Script

All lines starting with " are comments and will be ignored.

In most cases, " will also work after a command (i.e. the rest of the line will be ignored). But some commands like <code>echo</code> treat the whole line as their argument and thus will raise an error (''Missing quote'').


```vim
let a = 4 " A valid comment
echo "foo" " Not a comment but an argument that misses the closing quote
```



## Visual Basic


In addition to the methods mentioned in [[#BASIC|BASIC]] above, it is also somewhat common to effectively comment out code by including the unwanted code inside an <code>#If 0 ... #End If</code> block. (This works because 0 evaluates to False in VB.) Note, however, that the IDE will complain about actual comments inside an <code>#If 0</code> block unless it's also commented normally (i.e., using <code>Rem</code> or <code>'</code>).


```vb
'comment
Rem comment
#If 0
  Technically not a comment; the compiler may or may not ignore this, but the
  IDE won't. Note the somewhat odd formatting seen here; the IDE will likely
  just mark the entire line(s) as errors.
#End If
```



## Visual Basic .NET

Visual Basic .NET uses the "'" symbol or "REM" to mark it's comments. After placing a "'", or "REM", everything in that line will be ignored.


```vbnet
' This is a comment
REM This is also a comment
Dim comment as string ' You can also append comments to statements
Dim comment2 as string REM You can append comments to statements
```



## Visual Objects


```visualfoxpro

// This is a comment
/* This is a comment */
* This is a comment
&& This is a comment
NOTE This is a commen


```



## Vorpal



```vorpal
# single line comment>
```



## Wart



```wart
# single-line comment
```



## XLISP


```xlisp
; this is a comment>
```



## Xojo



```VB

// Comments are denoted by a preceding double slash or or single quote
' and continue to the end of the line. There are no multi-line comment blocks
Dim foo As Integer // Comments can also occupy the ends of code lines
```



## XPL0

Comments are enclosed in backslash characters, but the end of a line
always terminates a comment. Consequently there is no multi-line
comment. For example:

```XPL0
Text(0, \comment\ "Hello \not a comment\ World!"); \comment
```


Since backslashes toggle comments on and off, it could be inconvenient to
comment out a line of code that contains a comment. For example, two
additional backslashes could be used to comment out this line, as shown here:

```XPL0
 Text(0, "Hello World");  \comment
\Text(0, "Hello World"); \\comment
```


However, two backslashes together comment out everything to the end of
the line regardless of any backslashes the line might contain. So the
first example could be commented out like this:

```XPL0
\\Text(0, \comment\ "Hello \not a comment\ World"); \comment
```


Conditional compilation can be used to effectively comment out multiple
lines of code. For example:

```XPL0
cond false;
Text(0, "Hello World"); \comment
CrLf(0);
cond true;
```



## XQuery


```xquery
(: This is a XQuery comment :)
```



## XSLT


```xml
<!-- Comment syntax is borrowed from XML and HTML. -->
```



## XUL



```xul
<!-- Comment syntax is borrowed from XML and HTML. -->
```


## zonnon


```zonnon

(* this is a comment *)
(*
   and this is a
   multiline comment
   (* with a nested comment *)
*)

```



## zig


```zig
// This is a normal comment in Zig
/// This is a documentation comment in Zig (for the following line)
```



## zkl


```zkl
x=1; // comment ala C++
x=2; # ala scripts
/* ala C, these comments are parsed (also ala C) */
/* can /* be */ nested */
#if 0
  also ala C (and parsed)
#endif
#<<<#
  "here" comment, unparsed
#<<<#
```


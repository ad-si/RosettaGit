+++
title = "Talk:Compiler/lexical analyzer"
description = ""
date = 2018-05-24T14:18:36Z
aliases = []
[extra]
id = 21048
[taxonomies]
categories = []
tags = []
+++

==Clarification==

I like this task, thanks for contributing it. But some more clarification needs to be added before moving it out of draft status. Off the top of my head:

* '''encoding:''' Should we expect the input files in a specific encoding? Maybe ''latin-1'' or ''utf-8''?
* '''encoding:''' Should string and char literals support Unicode, or just ASCII?
* '''char literals:''' The stated regex is <code>'x'</code>, but that's not actually a regex. Shouldn't it be <code>'\\?[^']'</code> (a.k.a. an escape sequence or any character except <code>'</code>, enclosed in single quotes")?
* '''char literals:''' How can a single quote be represented as a char, if there are no other escape sequences besides <code>\n</code> and <code>\\</code>?
* '''string literals:''' The stated regex is <code>".*"</code>, but this would match e.g. <code>"foo bar" < "</code> due to the asterisk performing greedy matching. Shouldn't it be <code>"[^"]*"</code> (a.k.a. "match zero or more characters except the  double quote, enclosed in double quotes")?
* '''string literals:''' How can a double quote be represented inside a string literal, if there are no other escape sequences besides <code>\n</code> and <code>\\</code>?
* '''whitespace:''' This needs an actual thorough description, instead of just an example. Am I right to assume that zero or more whitespace characters or comments are allowed between ''any'' two tokens, with no exceptions, and that "longest token matching" is used to resolve conflicts (e.g. in order to match <code><=</code> as a single token rather than the two tokens <code><</code> and <code>=</code>)?
* '''operators''': How is the lexer supposed to differentiate between <code>Sub</code> and <code>Uminus</code>? And why does the third test-case print "Sub" for both?

Sorry if some of these sound pedantic, but experience on rosettacode has shown that tasks of this complexity absolutely need to be precise and unambiguous in order to not cause problems for people who will try to add solutions... :)

--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 14:22, 14 August 2016 (UTC)

Another small clarification. The table of valid tokens refers to a "char literal" but the error examples reference "char constants". Are these the same token? --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 14:44, 14 August 2016 (UTC)

==Response==

This first category is part of a larger example.  In the works, I have syntax analysis,
code generation (for a stack based virtual machine) and virtual machine emulator.  The
code is already complete in C and Python.  But no writeups yet.

There are lots of things missing from this simple compiler, as I attempted to weed out
features, in order to keep the implementations down to a manageable size.  Things like
'''else''', '''>=''', '''==''', data declarations, functions and so on.

The goal was to be able to run simple programs like the prime number generator in the
white-space example.

'''1) encoding (overall):'''

latin-1

'''2) encoding - string and char literals:'''

ASCII

Thinking about it a bit, for a hand-written scanner, there is really nothing that I am
aware of preventing string literals and comments from including utf-8.  Of course this
does not include character literals, where the code would have to be utf-8 aware.

Not sure how/where to update the page regarding the above two.

'''3) char literal regex:'''

The (new) definition I'm using for Flex:


```txt
\'([^'\n]|\\n|\\\\)\'
```


Page has been updated.

'''4) char literals: embedded single quote?'''

Not supported  It is one of the features I arbitrarily removed.

Page has been updated.

'''5) string literals: regex:'''

The (new)) definition I'm using for flex: 
```txt
\"[^"\n]*\"
```

(thanks for the new definition!)

Page has been updated.

'''6) string literals: embedded double quotes?'''

Not supported  It is one of the features I arbitrarily removed.

Page has been updated.

'''7) Whitespace:'''

I have updated the description.

'''8) Operators: Sub vs. Uminus'''

'''Uminus''' cannot be recognized by the scanner.  It is recognized by
the syntax analyzer, i.e., the parser.  The token type is there since
it will turn up in the parser and the code generator.

Not sure if the page should be updated regarding this.

'''10) char literal vs char constants'''

Yes, char literal and char constants represent the same thing.

Interestingly, when I was researching this I got the following doing a Google search for:
("string literal") OR ("string constant")

https://en.wikipedia.org/wiki/String_literal
<br/>
''A '''string literal''' or anonymous string is the representation of a string value within
the source code ..... Among other things, it must be possible to encode the character that
normally terminates the '''string constant''', plus there must be some way to ...''

Not sure if the page should be updated regarding this.

--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 16:10, 15 August 2016 (UTC)

==Output Format==

Thanks for the clarifications. With that out the way, two questions about apparent inconsistencies in the output format:

<ol>
<li>
Why are character literals fed through the evaluator and their calculated value printed, but string literals are printed "raw", exactly as they appeared in the code?

```txt
line    18  col    26 Integer         32
```


```txt
line     4  col     7 String   "Hello, World!\n"
```

I suppose this is arbitrary, and done because of potential newlines in strings &ndash; if so, wouldn't it be easier to remove support for <code>\n</code>, and let the lexer call its evaluator on strings as well, so that it can print the actual plain string value in the output? So the second line (without the <code>\n</code>) would look like:

```txt
line     4  col     7 String   Hello, World!
```

Maybe require a single TAB as separator between the output fields to avoid whitespace ambiguity, and maybe replace the <tt>print</tt> built-in with <tt>println</tt> so strings can be printed on their own line.


</li>
<li>
The output consists of four data fields (line number, column number, token type, token value) - how come only the first two fields have a label included in the output? Shouldn't either none or all fields include the label? i.e.:

```txt
4	7	String	Hello, World!
```


```txt
line	4	col	7	token	String	value	Hello, World!
```

</li>
</ol>
--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 16:35, 15 August 2016 (UTC)

1) Yes, it is arbitrary.  There isn't a ''right'' way to do this,
as far as I'm aware.  You could do it either way I suppose.
However, since character literals are really just small integer
constants, that is what the parser needs, and so they have to be
converted somewhere.  Since the scanner readily has that
information, it is easy to do the conversion there. Again, there
isn't a right a wrong way.  This is just the way I've always done
it.  For the integration of the scanner with the parser, this
seemed to be the easier way.  I personally like the way it is
done now :-)  Again, just my opinion.  I hope I have answered the
question(s)!

2) I would rather remove the "line" and "col" labels than add
additional ones.  Consider it done.  It will actually make some
of the code a tad shorter :-)

--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 17:32, 15 August 2016 (UTC)

==Token names==

Some more bikeshedding. I hope you don't mind it - it's just that once the task goes out of draft status and gets many solutions, it will be very annoying to make changes, so now is the time to get it in as good a shape as possible.

What caught my eye regarding the token names is, that the extensive use of uncommon abbreviations makes them less "user-friendly" to deal with than they could be. For this task, itself, it doesn't matter too much, but when it gets to syntax analysis, I image people who will try to write solutions will find themselves saying like "Huh, 'Lss Ident Semi', what tokens were those again?" and having to look it up. How would you feel about renaming them to the following slightly more verbose but friendlier names?

{| class="wikitable" style="display:inline-table"
|-
! Old !! New
|-
| <tt>Mul</tt> || <tt>OP_MULTIPLY</tt>
|-
| <tt>Div</tt> || <tt>OP_DIVIDE</tt>
|-
| <tt>Add</tt> || <tt>OP_ADD</tt>
|-
| <tt>Sub</tt> || <tt>OP_SUBTRACT</tt>
|-
| <tt>Uminus</tt> || <tt>OP_NEGATE</tt>
|-
| <tt>Lss</tt> || <tt>OP_LESSEQUAL</tt>
|-
| <tt>Leq</tt> || <tt>OP_LESS</tt>
|-
| <tt>Gtr</tt> || <tt>OP_GREATER</tt>
|-
| <tt>Neq</tt> || <tt>OP_NOTEQUAL</tt>
|-
| <tt>Assign</tt> || <tt>OP_ASSIGN</tt>
|-
| <tt>And</tt> || <tt>OP_AND</tt>
|}

{| class="wikitable" style="display:inline-table"
|-
! Old !! New
|-
| <tt>If</tt> || <tt>KEYWORD_IF</tt>
|-
| <tt>While</tt> || <tt>KEYWORD_WHILE</tt>
|-
| <tt>Print</tt> || <tt>KEYWORD_PRINT</tt>
|-
| <tt>Putc</tt> || <tt>KEYWORD_PUTC</tt>
|}

{| class="wikitable" style="display:inline-table"
|-
! Old !! New
|-
| <tt>Lparen</tt> || <tt>LEFTPAREN</tt>
|-
| <tt>Rparen</tt> || <tt>RIGHTPAREN</tt>
|-
| <tt>Lbrace</tt> || <tt>LEFTBRACE</tt>
|-
| <tt>Rbrace</tt> || <tt>RIGHTBRACE</tt>
|-
| <tt>Semi</tt> || <tt>SEMICOLON</tt>
|-
| <tt>Comma</tt> || <tt>COMMA</tt>
|}

{| class="wikitable" style="display:inline-table"
|-
! Old !! New
|-
| <tt>Ident</tt> || <tt>IDENTIFIER</tt>
|-
| <tt>Integer</tt> || <tt>INTEGER</tt>
|-
| <tt>String</tt> || <tt>STRING</tt>
|}
--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 17:04, 15 August 2016 (UTC)

These names came from textbooks and other compiler source code.

Interestingly, I remember when I had my compilers course back in
1978, I could not figure out what "Lss" stood for :-)  I guess
I've been using those names for so long that now they are second
nature.

I can change them.

--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 17:46, 15 August 2016 (UTC)

I have updated the token names, as well as remove the "line" and "col" labels.
--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 21:04, 15 August 2016 (UTC)

== What is next? ==


What is left to do, to make this a non-draft task?
Thanks!

--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 13:39, 16 August 2016 (UTC)

: It's customary to wait until a few people other than the task author have added a solution, but I don't think that's a hard rule. I'm writing a Perl solution right now, btw. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:36, 17 August 2016 (UTC)

I was hoping someone would do a Perl solution.  I've only done a very little with Perl - basically maintain a few scripts over the years - but I was always impressed with it.  Cool! :-)

--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 12:53, 17 August 2016 (UTC)

Wow!  I '''really''' like your Perl solution.  I don't understand much of it, but what I do understand is really cool.  
--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 10:28, 18 August 2016 (UTC)

: Thanks! :) I tried to be clever and dynamically construct a single regex (with one branch per token) to act as the scanner, since it's safe to assume that the Perl regex engine is more bug-free and better optimized than a <code>substr</code>-based scanner that I could have written by hand. But then I realized that there's no easy way to get the line and column number of a regex match, so I had to scan and accumulate those separately, which introduced overhead again. I wonder if the approach was still worth it, performance-wise. Not that a solution in an interpreted language like Perl could ever compete with the C solution, but it might be interesting to benchmark it against the Python solution for large input files... --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 17:06, 18 August 2016 (UTC)

It's easy to get line and column numbers out of a regex. See the Alternate.
--[[User:Tybalt89|Tybalt89]] ([[User talk:Tybalt89|talk]]) 14:10, 24 May 2018 (UTC)

== Simple benchmark ==

I ran some simple benchmarks, using a source file consisting of the following two programs, repeated over and over, until I got to 248,880 lines.


```c

count = 1;
n = 1;
limit = 100;
while (n < limit) {
    k=3;
    p=1;
    n=n+2;
    while ((k*k<=n) && (p)) {
        p=n/k*k!=n;
        k=k+2;
    }
    if (p) {
        print(n, " ");
        count = count + 1;
    }
}
print(count, "\n");

{
/*
 This is an integer ascii Mandelbrot generator
 */
    left_edge   = -420;
    right_edge  =  300;
    top_edge    =  300;
    bottom_edge = -300;
    x_step      =    7;
    y_step      =   15;

    max_iter    =  200;

    y0 = top_edge;
    while (y0 > bottom_edge) {
        x0 = left_edge;
        while (x0 < right_edge) {
            y = 0;
            x = 0;
            the_char = ' ';
            i = 0;
            while (i < max_iter) {
                x_x = (x * x) / 200;
                y_y = (y * y) / 200;
                if (x_x + y_y > 800 ) {
                    the_char = '0' + i;
                    if (i > 9) {
                        the_char = '@';
                    }
                    i = max_iter;
                }
                y = x * y / 100 + y0;
                x = x_x - y_y + x0;
                i = i + 1;
            }
            putc(the_char);
            x0 = x0 + x_step;
        }
        putc('\n');
        y0 = y0 - y_step;
    }
}

```


I ran them as follows:

timer python lex.py big.t >foo.bar

So the startup time for Python, Perl, and Euphoria is also included in the timings.

All the output files were 1,101,601 lines in length.

I ran each test 3 times, and took the shortest run.

Here are the specs for my machine:

Windows 7, Service Pack 1, 64-bit

Intel Core i7-3720QM CPU @2.60GHz

16.0 GB (15.9 usable)


{| class="wikitable"
|-
! Processor   !! Time
|-
|   C(1)      ||  1.08
|-
|   Flex      ||  1.13
|-
|   C         ||  1.34
|-
|   Euphoria  ||  4.15
|-
|   Perl      ||  8.36
|-
|   Python    ||  9.24
|}

(1) I swapped out getc(fp) with _fgetc_nolock(fp), and added setvbuf(fp, NULL, _IOFBF, 1024*1024).

To me, the Euphoria, Perl and Python times are '''very''' impressive.

: Indeed, I would have expected a larger difference between C and the interpreted languages. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 13:23, 19 August 2016 (UTC)

==Future==

My goal is to add the following related tasks:

;Syntax Analysis
: this is basically a parser that outputs a textural parse tree
;Code Generation
: code generation for a simple stack based virtual machine - outputs stack vm assembly code
;Virtual Machine
: virtual machine code interpreter - interprets the vm assembly code

I have already implemented all 3 of these in C and Python.  I can do something like:

Given the following program:

 count = 1;
 while (count < 10) {
     print("count is: ", count, "\n");
     count = count + 1;
 }

Running:

 lex count.t | parse

Will output a parse tree in textural format:

 Sequence
 Sequence
 ;
 Assign
 Identifier     count
 Integer        1
 While
 Less
 Identifier     count
 Integer        10
 Sequence
 Sequence
 ;
 Sequence
 Sequence
 Sequence
 ;
 Prts
 String         "count is: "
 ;
 Prti
 Identifier     count
 ;
 Prts
 String         "\n"
 ;
 Assign
 Identifier     count
 Add
 Identifier     count
 Integer        1

Running:

 lex count.t | parse | gen

Will output the following virtual assembly code:

 Datasize: 1 Strings: 2
 "count is: "
 "\n"
     0 push  1
     5 store [0]
    10 fetch [0]
    15 push  10
    20 lt
    21 jz     (43) 65
    26 push  0
    31 prts
    32 fetch [0]
    37 prti
    38 push  1
    43 prts
    44 fetch [0]
    49 push  1
    54 add
    55 store [0]
    60 jmp    (-51) 10
    65 halt

Running:

 lex count.t | parse | gen | vm

And it will output the following.

 count is: 1
 count is: 2
 count is: 3
 count is: 4
 count is: 5
 count is: 6
 count is: 7
 count is: 8
 count is: 9

And, I can mix and match - I can use the C lexer, the Python parser, the C code generator, and the Python vm, if that makes sense.

I've already started the write-ups for these.  I'll make them draft tasks.  '''Question:'''  What is the protocol, e.g., is it acceptable to post very-rough draft work, in order to solicit feedback?  Or should I wait until I have it specified more clearly?

--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 15:01, 13 September 2016 (UTC)

==Status==
I vetted this on 2 programming forums/mailing lists, and 2 compiler specific forums.  Got feedback, and got two additional solutions!   I believe it is ready to go!
--[[User:Ed Davis|Ed Davis]] ([[User talk:Ed Davis|talk]]) 10:55, 22 October 2016 (UTC)

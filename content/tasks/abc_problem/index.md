+++
title = "ABC Problem"
description = ""
date = 2019-10-17T04:29:11Z
aliases = []
[extra]
id = 17061
task = """
  Write a function that takes a string (word)
  and determines whether the word can be spelled
  with the given collection of blocks.
"""
[taxonomies]
categories = ["task"]
languages = [
  "11l",
  "360_assembly",
  "8th",
  "acurity_architect",
  "ada",
  "algol_68",
  "algol_w",
  "apex",
  "apl",
  "applescript",
  "astro",
  "autohotkey",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "c",
  "ceylon",
  "clojure",
  "coffeescript",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dyalect",
  "echolisp",
  "ela",
  "elena",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fbsl",
  "fortran",
  "freebasic",
  "fsharp",
  "gambas",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "liberty_basic",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica_wolfram_language",
  "matlab",
  "maxscript",
  "mercury",
  "miniscript",
  "nim",
  "oberon_2",
  "objeck",
  "ocaml",
  "oforth",
  "openedge_progress",
  "pari_gp",
  "pascal",
  "perl",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powerbasic",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rapidq",
  "red",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sequencel",
  "sidef",
  "simula",
  "smalltalk",
  "spad",
  "swift",
  "tcl",
  "tuscript",
  "txr",
  "unicon",
  "unix_shell",
  "utfool",
  "vba",
  "yabasic",
  "zkl",
  "zonnon",
  "zx_spectrum_basic",
]
tags = ["game", "puzzle"]
+++

You are given a collection of ABC blocks
(maybe like the ones you had when you were a kid).

There are twenty blocks with two letters on each block.

A complete alphabet is guaranteed amongst all sides of the blocks.

The sample collection of blocks:

```txt
(B O)
(X K)
(D Q)
(C P)
(N A)
(G T)
(R E)
(T G)
(Q D)
(F S)
(J W)
(H U)
(V I)
(A N)
(O B)
(E R)
(F S)
(L Y)
(P C)
(Z M)
```

The rules are simple:

1. Once a letter on a block is used that block cannot be used again
1. The function should be case-insensitive
1. Show the output on this page for the following 7 words
    in the following example


Example:

```txt
>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True
```


## 11l

Translated from Python


{{ code(src="content/tasks/abc_problem/11l.11l", lang="11l") }}




## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible.


{{ code(src="content/tasks/abc_problem/360_assembly.360asm", lang="360asm") }}


Output:

```txt

A                   true
BARK                true
BOOK                false
TREAT               true
COMMON              false
SQUAD               true
CONFUSE             true

```



## 8th


{{ code(src="content/tasks/abc_problem/8th.8th", lang="8th") }}




## Acurity Architect


```txt

Using #HASH-OFF

```



{{ code(src="content/tasks/abc_problem/acurity_architect.txt", lang="acurity architect") }}


Output:

```txt
bCAN_MAKE_WORD("A") returns TRUE
bCAN_MAKE_WORD("BARK") returns TRUE
bCAN_MAKE_WORD("BOOK") returns FALSE
bCAN_MAKE_WORD("TREAT") returns TRUE
bCAN_MAKE_WORD("COMMON") returns FALSE
bCAN_MAKE_WORD("SQUAD") returns TRUE
bCAN_MAKE_WORD("CONFUSE") returns TRUE
```



## Ada

```txt
Build with gnatchop abc.ada; gnatmake abc_problem
```


{{ code(src="content/tasks/abc_problem/ada.adb", lang="ada") }}



Output:

```txt

A: TRUE
BARK: TRUE
BOOK: FALSE
TREAT: TRUE
COMMON: FALSE
SQUAD: TRUE
CONFUSE: TRUE

```



## ALGOL 68

Works with ALGOL 68G (tested with release 2.8.win32)


{{ code(src="content/tasks/abc_problem/algol_68.a68", lang="algol68") }}


Output:

```txt

can spell: "A" -> yes
can spell: "BaRK" -> yes
can spell: "BOOK" -> no
can spell: "TREAT" -> yes
can spell: "COMMON" -> no
can spell: "SQUAD" -> yes
can spell: "CONFUSE" -> yes

```



## ALGOL W



{{ code(src="content/tasks/abc_problem/algol_w.alw", lang="algolw") }}


Output:

```txt

can    spell "a                   "
can    spell "bark                "
cannot spell "BOOK                "
can    spell "treat               "
cannot spell "commoN              "
can    spell "Squad               "
can    spell "confuse             "

```



## Apex



{{ code(src="content/tasks/abc_problem/apex.java", lang="Java") }}


Output:

```txt
"": true
"A": true
"BARK": true
"book": false
"treat": true
"COMMON": false
"SQuAd": true
"CONFUSE": true
```



## APL

Works with Dyalog APL 16.0


{{ code(src="content/tasks/abc_problem/apl.apl", lang="APL") }}


Output:

```txt
      )COPY dfns ucase
      b W←(≠∘' '⊆⊢)∘ucase¨'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM' 'A BaRK BOoK tREaT COmMOn SqUAD CoNfuSE'
      b∘abc¨W
1 1 0 1 0 1 1

```



## AppleScript


### Imperative



{{ code(src="content/tasks/abc_problem/applescript_1.applescript", lang="AppleScript") }}




### Functional composition



{{ code(src="content/tasks/abc_problem/applescript_2.applescript", lang="AppleScript") }}


Output:

```txt
''  ->  true
'A'  ->  true
'BARK'  ->  true
'BoOK'  ->  false
'TrEAT'  ->  true
'COmMoN'  ->  false
'SQUAD'  ->  true
'conFUsE'  ->  true
```



## Astro



{{ code(src="content/tasks/abc_problem/astro.astro", lang="astro") }}



## AutoHotkey


{{ code(src="content/tasks/abc_problem/autohotkey_1.ahk", lang="autohotkey") }}



'''Test Input''' (as per question)


{{ code(src="content/tasks/abc_problem/autohotkey_2.ahk", lang="autohotkey") }}



Output:

```txt
A - 1
BARK - 1
BOOK - 0
TREAT - 1
COMMON - 0
SQUAD - 1
CONFUSE - 1
```


## AWK

Here are 2 slightly different versions:

```txt

#!/usr/bin/awk -f
# tested with mawk 1.3.3 on Raspberry Pi 3
#        also GNU awk 3.1.5, busybox 1.21.1 and 1.27.1 on AMD Sempron 2800+
#
function setblocks() {
# key to the algorithm is the representation of a block
# each block is represented by 4 characters in the string "blocks"
# for example, the "BO" block becomes "-BO-"
#
blocks="-BO--XK--DQ--CP--NA--GT--RE--TG--QD--FS--JW--HU--VI--AN--OB--ER--FS--LY--PC--ZM-"
true=1
false=0
}
function found(letter){
#
# the function "found" scans for the letter on the top of a block
# using the pattern "-B", for example, to find a "B",
# returning "true" (or 1) if found
# if not found on the top, look on the bottoms using the pattern "B-"
# again returning "true" if found
# if the letter is found on either top or bottom, the 4 character block is set to "----"
# so that block is unavailable
# finally, if no available copy of letter is found,
# the function returns "false" (0)
position= index(blocks,"-" letter)
if (position > 0)
   {
  blocks = substr(blocks,1,position-1) "----" substr(blocks,position+4)
  return true
   }
position = index(blocks,letter "-")
if (position > 0)
   {blocks = substr(blocks,1,position-3) "----" substr(blocks,position+2)
     return true
    }
return false
}
# awk's BEGIN statement allows for initialization before processing input;
# in this case, initializing the string "blocks"
#
BEGIN{
setblocks()
}
# in awk, the input record is contained in the string variable "$0"
# the main process checks each letter in turn to see if it is on a usable block,
# summing the values returned by "found"
# if the sum equals the number of input characters the word can be spelled with the blocks
# otherwise it is not possible
#
{
nchars=length($0)
possible=false
for (i=1;i<=nchars;i++){
     possible=possible + found(substr($0,i,1))
}
if (possible==nchars) print $0 " is possible"
   else print $0 " is not possible"
setblocks()
}

```

and

```txt

#!/usr/bin/awk -f
# tested with mawk 1.3.3 on Raspberry Pi 3
#        also GNU awk 3.1.5, busybox 1.21.1 and 1.27.1 on AMD Sempron 2800+
#
function setblocks() {
#
#  key to the algorithm is the representation of the blocks
# each block is represented by 1 character in the string "tops"
# and by 1 character in the string "bottoms"
#
   tops="BXDCNGRTQFJHVAOEFLPZ"
bottoms="OKQPATEGDSWUINBRSYCM"
true=1
false=0
}
function found(letter){
#
# the function "found" scans first the string "tops" for a letter and
# then the string "bottoms" if the letter is not in "tops"
# if the letter is found, it marks "tops" and "bottoms" to show
# the block is unavailable by changing the letters on the block to "-"
# and returns "true" (1); if the letter is not found
# the function returns "false" (0)
#
position= index(tops,letter)
if (position > 0)
   {
  tops = substr(tops,1,position-1) "-" substr(tops,position+1)
  bottoms = substr(bottoms,1,position-1) "-" substr(bottoms,position+1)
  return true
   }
position = index(bottoms,letter)
if (position > 0)
   {bottoms = substr(bottoms,1,position-1) "-" substr(bottoms,position+1)
    tops = substr(tops,1,position-1) "-" substr(tops,position+1)
     return true
    }
return false
}
# awk's BEGIN statement allows for initialization before processing input;
# in this case, initializing the string "blocks"
#
BEGIN{
setblocks()
}
# in awk, the input record is contained in the string variable "$0"
# the main process checks each letter in turn to see if it is on a usable block,
# summing the values returned by "found"
# if the sum equals the number of input characters the word can be spelled with the blocks
# otherwise it is not possible
#
{
nchars=length($0)
possible=false
for (i=1;i<=nchars;i++){
     possible=possible + found(substr($0,i,1))
}
if (possible==nchars) print $0 " is possible"
   else print $0 " is not possible"
setblocks()
}

```

Output:

```txt

pi@raspberrypi:~/Documents/rosettacode $ ./abcProblem.awk
A
A is possible
BARK
BARK is possible
BOOK
BOOK is not possible
TREAT
TREAT is possible
COMMON
COMMON is not possible
SQUAD
SQUAD is possible
CONFUSE
CONFUSE is possible
^C
pi@raspberrypi:~/Documents/rosettacode $

```



## Batch File



{{ code(src="content/tasks/abc_problem/batch_file.bat", lang="dos") }}




## BaCon



{{ code(src="content/tasks/abc_problem/bacon.qbasic", lang="qbasic") }}


Output:

```txt
A         : True
BARK      : True
BOOK      : False
TREAT     : True
Common    : False
Squad     : True
Confuse   : True
```



## BASIC

Works with:VB-DOS, QB64, QBasic, QuickBASIC


{{ code(src="content/tasks/abc_problem/basic_1.bas", lang="qbasic") }}



### Commodore BASIC

Based on the Sinclair ZX81 BASIC solution.
Indentations are for legibility only,
will not be preserved in real Commodore BASIC editor.


{{ code(src="content/tasks/abc_problem/basic_2.bas", lang="basic") }}


Output:

```txt
A -> YES
BARK -> YES
BOOK -> NO
TREAT -> YES
COMMON -> NO
SQUAD -> YES
CONFUSE -> YES
```


### Sinclair ZX81 BASIC

Works with 1k of RAM.
A nice unstructured algorithm.
Unfortunately the requirement that it be case-insensitive is moot,
because the ZX81 does not support lower-case letters.


{{ code(src="content/tasks/abc_problem/basic_3.bas", lang="basic") }}


Input:

```txt
A
```

Output:

```txt
YES
```

Input:

```txt
BARK
```

Output:

```txt
YES
```

Input:

```txt
BOOK
```

Output:

```txt
NO
```

Input:

```txt
TREAT
```

Output:

```txt
YES
```

Input:

```txt
COMMON
```

Output:

```txt
NO
```

Input:

```txt
SQUAD
```

Output:

```txt
YES
```

Input:

```txt
CONFUSE
```

Output:

```txt
YES
```



## BBC BASIC

Works with BBC BASIC for Windows}}


{{ code(src="content/tasks/abc_problem/bbc_basic.bas", lang="bbcbasic") }}



Output:

```txt
A -> True
BARK -> True
BOOK -> False
TREAT -> True
COMMON -> False
SQUAD -> True
Confuse -> True
```


## Bracmat


{{ code(src="content/tasks/abc_problem/bracmat.bra", lang="bracmat") }}


Output:

```txt
A yes
BARK yes
BOOK no
TREAT yes
COMMON no
SQUAD yes
CONFUSE yes
```



## C

Recursive solution.
Empty string returns true.


{{ code(src="content/tasks/abc_problem/c.c", lang="c") }}


Output:

```txt
        1
A       1
BARK    1
BOOK    0
TREAT   1
COMMON  0
SQUAD   1
Confuse 1
```


## C++

Uses C++11. Build with g++-4.7 -Wall -std=c++0x abc.cpp


{{ code(src="content/tasks/abc_problem/cpp.cpp", lang="cpp") }}



Output:

```txt
A: true.
BARK: true.
BOOK: false.
TREAT: true.
COMMON: false.
SQUAD: true.
CONFUSE: true.
```


## C#

### Regex

This Method uses regular expressions to do the checking.
Given that n = length of blocks string and
m = length of word string,
then CheckWord's time complexity comes out to about m * (n - (m-1)/2).


{{ code(src="content/tasks/abc_problem/csharp_1.cs", lang="c#") }}


Output:

```txt
A: True
BARK: True
BOOK: False
TREAT: True
COMMON: False
SQUAD: True
CONFUSE: True
```

'''Unoptimized'''


{{ code(src="content/tasks/abc_problem/csharp_2.cs", lang="c#") }}


Output:

```txt
A :True
BARK :True
BOOK :False
TREAT :True
COMMON :False
SQUAD :True
CONFUSE :True
```


## Ceylon

Functional programming/recursive solution.
No variable values.

<b>module.ceylon</b>


{{ code(src="content/tasks/abc_problem/ceylon_1.ceylon", lang="ceylon") }}


<b>run.ceylon</b>


{{ code(src="content/tasks/abc_problem/ceylon_2.ceylon", lang="ceylon") }}


Output:

```txt
A:true
BARK:true
BOOK:false
TREAT:true
COMMON:false
SQUAD:true
CONFUSE:true
```


## Clojure

A translation of the Haskell solution.


{{ code(src="content/tasks/abc_problem/clojure.clj", lang="clojure") }}


Output:

```txt
A: true
BARK: true
Book: false
treat: true
COMMON: false
SQUAD: true
CONFUSE: true
```


## CoffeeScript


{{ code(src="content/tasks/abc_problem/coffeescript.coffee", lang="CoffeeScript") }}



Output:

```txt
A -> true
BARK -> true
BOOK -> false
TREAT -> true
COMMON -> false
squad -> true
CONFUSE -> true
STORM -> true
```


## Common Lisp


{{ code(src="content/tasks/abc_problem/common_lisp.lisp", lang="lisp") }}



Output:

```txt
> (defparameter *blocks*
    '("BO" "XK" "DQ" "CP" "NA" "GT" "RE" "TG" "QD" "FS"
      "JW" "HU" "VI" "AN" "OB" "ER" "FS" "LY" "PC" "ZM"))
> (dolist (w '("" "A" "bArk" "BOOK" "trEAt" "CoMmoN" "squad" "conFUse"))
    (format t "~s is possible: ~a~%" w (word-possible-p w *blocks*)))
"" is possible: T
"A" is possible: T
"bArk" is possible: T
"BOOK" is possible: NIL
"trEAt" is possible: T
"CoMmoN" is possible: NIL
"squad" is possible: T
"conFUse" is possible: T
NIL
> (word-possible-p "abba" '("AB" "AB" "AC" "AC"))
T
```


## Component Pascal

Works with BlackBox Component Builder}}


{{ code(src="content/tasks/abc_problem/component_pascal.oberon2", lang="oberon2") }}


Execute: ^Q ABCProblem.CanMakeWord A BARK BOOK TREAT COMMON SQUAD confuse~
Output:

```txt
A:>  $TRUE
BARK:>  $TRUE
BOOK:>  $FALSE
TREAT:>  $TRUE
COMMON:>  $FALSE
SQUAD:>  $TRUE
confuse:>  $TRUE
```


## D

### Basic Version

Translated from Python
A simple greedy algorithm is enough for the given sequence of blocks.
canMakeWord is true on an empty word
because you can compose it using zero blocks.


{{ code(src="content/tasks/abc_problem/d_1.d", lang="d") }}


Output:

```txt
"" true
"A" true
"BARK" true
"BoOK" false
"TrEAT" true
"COmMoN" false
"SQUAD" true
"conFUsE" true
```



### @nogc Version

The same as the precedent version, but it avoids all heap allocations
and it's lower-level and ASCII-only.


{{ code(src="content/tasks/abc_problem/d_2.d", lang="d") }}




### Recursive Version

This version is able to find the solution for the word "abba" given the blocks AB AB AC AC.
Translated from C


{{ code(src="content/tasks/abc_problem/d_3.d", lang="d") }}


Output:

```txt
"" true
"A" true
"BARK" true
"BoOK" false
"TrEAT" true
"COmMoN" false
"SQUAD" true
"conFUsE" true
"abba" true
```



### Alternative Recursive Version

This version doesn't shuffle the input blocks, but it's more complex and it allocates an array of indexes.


{{ code(src="content/tasks/abc_problem/d_4.d", lang="d") }}


The output is the same.


## Delphi

Just to be different I implemented a block as a set of (2) char rather than as an array of (2) char.


{{ code(src="content/tasks/abc_problem/delphi.dpr", lang="Delphi") }}



Output:

```txt
Output:
Can make A
Can make BARK
Can NOT make BOOK
Can make TREAT
Can NOT make COMMON
Can make SQUAD
Can make CONFUSE

```



## Dyalect


{{ code(src="content/tasks/abc_problem/dyalect.dy", lang="dyalect") }}



Output:


```txt
"A" can be spelled with blocks.
"BARK" can be spelled with blocks.
"BooK" cannot be spelled with blocks.
"TrEaT" can be spelled with blocks.
"comMON" cannot be spelled with blocks.
"sQuAd" can be spelled with blocks.
"Confuse" can be spelled with blocks.
```



## EchoLisp



{{ code(src="content/tasks/abc_problem/echolisp.lisp", lang="scheme") }}


Output:

```txt

(for ((w WORDS))
  (writeln
    (string-randcase w)
    (spell (string-upcase w) BLOCKS)))

A     #t
bARK     #t
BooK     #f
TReAt     #t
ComMOn     #f
sqUAd     #t
COnfUSe     #t

```



## Ela


{{ code(src="content/tasks/abc_problem/ela.ela", lang="ela") }}



Output:

```txt
("conFUsE",true)
("SQUAD",true)
("COmMoN",false)
("TrEAT",true)
("BoOK",false)
("BARK",true)
("A",true)
("",true)
```


## Elena

ELENA 4.1


{{ code(src="content/tasks/abc_problem/elena.l", lang="elena") }}


Output:

```txt

can make '' : true
can make 'A' : true
can make 'BARK' : true
can make 'BOOK' : false
can make 'TREAT' : true
can make 'COMMON' : false
can make 'SQUAD' : true
can make 'Confuse' : true

```



## Elixir

Works with Elixir|1.3}}


{{ code(src="content/tasks/abc_problem/elixir.exs", lang="elixir") }}



Output:

```txt

A: true
Bark: true
Book: false
Treat: true
Common: false
Squad: true
Confuse: true

```



## Erlang



{{ code(src="content/tasks/abc_problem/erlang.erl", lang="erlang") }}



Output:

```txt
A: true
Bark: true
Book: false
Treat: true
Common: false
Squad: true
Confuse: true

```



## ERRE



{{ code(src="content/tasks/abc_problem/erre.erre", lang="ERRE") }}




## Euphoria

implemented using OpenEuphoria


{{ code(src="content/tasks/abc_problem/euphoria.e", lang="Euphoria") }}


Output:

```txt

A: TRUE
BarK: TRUE
BOOK: FALSE
TrEaT: TRUE
COMMON: FALSE
SQUAD: TRUE
CONFUSE: TRUE

..press Enter..

```


## F#

This solution does not depend on the order of the blocks, neither on the symmetry of blocks we see in the example block set.
(Symmetry: if AB is a block, an A comes only with another AB|BA)


{{ code(src="content/tasks/abc_problem/fsharp.fs", lang="fsharp") }}


Output:

```txt
h:\RosettaCode\ABC\Fsharp>RosettaCode "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM" a bark book threat common squad confuse
Using the blocks we can make the word 'A': true
Using the blocks we can make the word 'BARK': true
Using the blocks we can make the word 'BOOK': false
Using the blocks we can make the word 'THREAT': true
Using the blocks we can make the word 'COMMON': false
Using the blocks we can make the word 'SQUAD': true
Using the blocks we can make the word 'CONFUSE': true

h:\RosettaCode\ABC\Fsharp>RosettaCode  "aB aB Ac Ac" abba
Using the blocks we can make the word 'ABBA': true

h:\RosettaCode\ABC\Fsharp>RosettaCode "US TZ AO QA" Auto
Using the blocks we can make the word 'AUTO': true
```



## Factor



{{ code(src="content/tasks/abc_problem/factor.factor", lang="factor") }}


Output:

```txt

Available blocks:
(B O) (X K) (D Q) (C P) (N A)
(G T) (R E) (T G) (Q D) (F S)
(J W) (H U) (V I) (A N) (O B)
(E R) (F S) (L Y) (P C) (Z M)

Word    Can make word from blocks?

### ==== =======================

A       Yes
BARK    Yes
BOOK    No
TREAT   Yes
COMMON  No
SQUAD   Yes
CONFUSE Yes

```



## FBSL

This approach uses a string, blanking out the pair previously found. Probably faster than array manipulation.


{{ code(src="content/tasks/abc_problem/fbsl.qbasic", lang="qbasic") }}


Output:

```txt

A can be spelled with blocks.
BARK can be spelled with blocks.
BooK cannot be spelled with blocks.
TrEaT can be spelled with blocks.
comMON cannot be spelled with blocks.
sQuAd can be spelled with blocks.
Confuse can be spelled with blocks.

Press any key to continue...

```



## Fortran

Attempts to write the word read from unit 5.  Please find the output, bash command, and gfortran compilation instructions as commentary at the start of the source, which starts right away!


{{ code(src="content/tasks/abc_problem/fortran_1.f90", lang="Fortran") }}




### But if backtracking might be needed

The example set does not exercise the possible need for backtracking, as when an initial selection of blocks prevents completion because available letters have been used up. This can only arise when the same letter appears on more than one block and does so with different partners. The example set does contain duplicated letters, but they appear only via blocks with the same letters. Suppose instead that the block collection was AB, BC, CD, ... XY, YZ so that every letter appears twice except for A and Z. If the target word was STOPPED then both OP and PQ would be needed to supply P, but if the O had been supplied via OP then the second P would be unavailable. If instead the O were to be supplied by NO then all would be well.

The method involves the stack-style usage of array MOVE, but there is no explicit attempt at recursion. The array contains the possible moves at each level, and if necessary, a move made can later be retracted and an alternative sought. This is the standard style of playing board games such as chess via developing a "game tree", but in this case the tree traversal is not a large task.

The following source begins with some support routines. Subroutine PLAY inspects the collection of blocks to make various remarks, and function CANBLOCK reports on whether a word can be spelled out with the supplied blocks. The source requires only a few of the F90 features. The MODULE protocol eases communication, but the key feature is that subprograms can now declare arrays of a size determined on entry via parameters. Previously, a constant with the largest-possible size would be required.


{{ code(src="content/tasks/abc_problem/fortran_2.f90", lang="Fortran") }}


Output: the first column of T/F is the report from CANBLOCK, the second is the expected answer from the example, and the third is whether the two are in agreement.

```txt

Arranges alphabet blocks, attending only to the letters on the blocks, and ignoring case and orientation.

     20 blocks, with at most 2 letters: BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM
      ... the letters in reverse order: OB XK QD PC NA TG RE TG QD SF WJ UH VI NA OB RE SF YL PC ZM
       ... the blocks in reverse order: ZM YL XK WJ VI UH TG TG SF SF RE RE QD QD PC PC OB OB NA NA
               Letters of the alphabet:  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
           ... number thereof supplied:  2  2  2  2  2  2  2  1  1  1  1  1  1  2  2  2  2  2  2  2  1  1  1  1  1  1
        Blocks with duplicated letters: None.
                     Duplicated blocks: TG SF RE QD PC OB NA           7
Duplicated letters on different blocks: None.

Now to see if some words can be spelled out.
 T T T A
 T T T BARK
 F F T BOOK
 T T T TREAT
 F F T COMMON
 T T T SQUAD
 T T T CONFUSE

```


## FreeBASIC



{{ code(src="content/tasks/abc_problem/freebasic.bas", lang="freebasic") }}


Output:

```txt
A           true
BARK          true
BOOK          false
TREAT         true
COMMON        false
SQUAD         true
CONFUSE       true
```



## Gambas

'''[Click this link to run this code](https://gambas-playground.proko.eu/?gist=ae860292d4588b3627d77c85bcc634ee)'''


{{ code(src="content/tasks/abc_problem/gambas.gambas", lang="gambas") }}


Output:

```txt

A - True
BARK - True
BOOK - False
TREAT - True
COMMON - False
SQUAD - True
CONFUSE - True

```



## Go



{{ code(src="content/tasks/abc_problem/go.go", lang="go") }}


Output:

```txt

A true
BARK true
BOOK false
TREAT true
COMMON false
SQUAD true
CONFUSE true

```



## Groovy

Solution:


{{ code(src="content/tasks/abc_problem/groovy_1.groovy", lang="groovy") }}



Test:


{{ code(src="content/tasks/abc_problem/groovy_2.groovy", lang="groovy") }}



Output:

```txt
'': true
'A': true
'BARK': true
'book': false
'treat': true
'COMMON': false
'SQuAd': true
'CONFUSE': true
```



## Harbour

Harbour Project implements a cross-platform Clipper/xBase compiler.


{{ code(src="content/tasks/abc_problem/harbour.prg", lang="visualfoxpro") }}


Output:

```txt

         A can be spelled with blocks.
      BARK can be spelled with blocks.
      BooK cannot be spelled with blocks.
     TrEaT can be spelled with blocks.
    comMON cannot be spelled with blocks.
     sQuAd can be spelled with blocks.
   Confuse can be spelled with blocks.
```



## Haskell


The following function returns a list of all the solutions. Since Haskell is lazy, testing whether the list is null will only do the minimal amount of work necessary to determine whether a solution exists.


{{ code(src="content/tasks/abc_problem/haskell_1.hs", lang="haskell") }}



Output:

```txt

("",True)
("A",True)
("BARK",True)
("BoOK",False)
("TrEAT",True)
("COmMoN",False)
("SQUAD",True)
("conFUsE",True)

```


Or, in terms of the bind operator:



{{ code(src="content/tasks/abc_problem/haskell_2.hs", lang="haskell") }}


Output:

```txt
("",True)
("A",True)
("BARK",True)
("BoOK",False)
("TrEAT",True)
("COmMoN",False)
("SQUAD",True)
("conFUsE",True)
```


## Icon and Unicon
Translated from C

Works in both languages:


{{ code(src="content/tasks/abc_problem/unicon.icn", lang="unicon") }}



Sample run:

```txt

->abc "" A BARK BOOK TREAT COMMON SQUAD CONFUSE
"" can be spelled with blocks.
"A" can be spelled with blocks.
"BARK" can be spelled with blocks.
"BOOK" can not be spelled with blocks.
"TREAT" can be spelled with blocks.
"COMMON" can not be spelled with blocks.
"SQUAD" can be spelled with blocks.
"CONFUSE" can be spelled with blocks.
->

```



## J

'''Solution:'''


{{ code(src="content/tasks/abc_problem/j_1.ijs", lang="j") }}


'''Examples:'''


{{ code(src="content/tasks/abc_problem/j_2.ijs", lang="j") }}



'''Tacit version'''


{{ code(src="content/tasks/abc_problem/j_3.ijs", lang="j") }}



Output:

```txt
   (,.Blocks&forms) ExampleWords
┌───────┬─┐
│A      │1│
├───────┼─┤
│BaRK   │1│
├───────┼─┤
│BOoK   │0│
├───────┼─┤
│tREaT  │1│
├───────┼─┤
│COmMOn │0│
├───────┼─┤
│SqUAD  │1│
├───────┼─┤
│CoNfuSE│1│
└───────┴─┘
```



### Alternative Implementation


Another approach might be:



{{ code(src="content/tasks/abc_problem/j_4.ijs", lang="j") }}



Example use:



{{ code(src="content/tasks/abc_problem/j_5.ijs", lang="J") }}



Explanation:

We only need to consider blocks which contain letters in common with a normalized (upper case) version of the desired word. But we do need to consider all possible combinations of letters from those blocks (see talk page discussion of words like 'ABBA' for more on this issue).

We can classify possibilities by counting how many of each letter occur. If a candidate has at least as many of the required letters as a test case constructed from the word itself, it's a valid candidate.

For example:



{{ code(src="content/tasks/abc_problem/j_6.ijs", lang="J") }}



Here, the word is simply 'A', and we have two blocks to consider for our word: AN and NA. So we form all possible combinations of the letters of those two bocks, prefix each of them with our word and test whether any of them contain two copies of the letters of our word. (As it happens, all of the candidates are valid, for this trivial example.)


## Java

Translated from C
Works with Java|1.6+}}


{{ code(src="content/tasks/abc_problem/java.java", lang="java5") }}


Output:

```txt
"": true
A: true
BARK: true
book: false
treat: true
COMMON: false
SQuAd: true
CONFUSE: true
```



## JavaScript


### ES5


### =Imperative=

The following method uses regular expressions and the string replace function to allow more support for older browsers.


{{ code(src="content/tasks/abc_problem/javascript_1.js", lang="javascript") }}



Result:

```txt

A: true
BARK: true
BOOK: false
TREAT: true
COMMON: false
SQUAD: true
CONFUSE: true

```



### =Functional=



{{ code(src="content/tasks/abc_problem/javascript_2.js", lang="JavaScript") }}


Output:


{{ code(src="content/tasks/abc_problem/javascript_3.js", lang="JavaScript") }}




### ES6


### =Imperative=



{{ code(src="content/tasks/abc_problem/javascript_4.js", lang="javascript") }}



Result:

```txt
A: true
BARK: true
BOOK: false
TREAT: true
COMMON: false
SQUAD: true
CONFUSE: true
```




### =Functional=


{{ code(src="content/tasks/abc_problem/javascript_5.js", lang="JavaScript") }}


Output:

```txt
["",true]
["A",true]
["BARK",true]
["BoOK",false]
["TrEAT",true]
["COmMoN",false]
["SQUAD",true]
["conFUsE",true]
```



## jq

The problem description seems to imply that if a letter, X, appears on more than one block, its partner will be the same on all blocks. This makes the problem trivial.

{{ code(src="content/tasks/abc_problem/jq_1.jq", lang="jq") }}


Task:

{{ code(src="content/tasks/abc_problem/jq_2.jq", lang="jq") }}


Output:
 A : true
 BARK : true
 BOOK : false
 TREAT : true
 COMMON : false
 SQUAD : true
 CONFUSE : true


## Jsish

Based on Javascript ES5 imperative solution.


{{ code(src="content/tasks/abc_problem/jsish.jsi", lang="javascript") }}



Output:

```txt
prompt$ jsish ABCProblem.jsi
Using blocks: BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM
can spell A
can spell BARK
can't spell BOOK
can spell TREAT
can't spell COMMON
can spell SQUAD
can spell CONFUSE

prompt$ jsish -u ABCProblem.jsi
[PASS] ABCProblem.jsi
```



## Julia



{{ code(src="content/tasks/abc_problem/julia.jl", lang="julia") }}



Output:

```txt
A        |  true
BARK     |  true
BOOK     |  false
TREAT    |  true
COMMON   |  false
SQUAD    |  true
CONFUSE  |  true
```



## Kotlin

Translated from Java


{{ code(src="content/tasks/abc_problem/kotlin.kt", lang="scala") }}


Output:

```txt
"": true
A: true
BARK: true
book: false
treat: true
COMMON: false
SQuAd: true
CONFUSE: true
```



## Liberty BASIC


### Recursive solution



{{ code(src="content/tasks/abc_problem/liberty_basic_1.bas", lang="lb") }}


Output:

```txt

RosettaGit - ABC problem (recursive solution)

>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True
Program complete.

```


### Procedural solution



{{ code(src="content/tasks/abc_problem/liberty_basic_2.bas", lang="lb") }}


Output:

```txt

RosettaGit - ABC problem (procedural solution)

>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True
Program complete.

```



## Logo



{{ code(src="content/tasks/abc_problem/logo.lg", lang="logo") }}



Output:

```txt
A: true
BARK: true
BOOK: false
TREAT: true
COMMON: false
SQUAD: true
CONFUSE: true
```



## Lua



{{ code(src="content/tasks/abc_problem/lua.lua", lang="lua") }}



Output:

```txt

canMake("A"): true
canMake("BARK"): true
canMake("BOOK"): false
canMake("TREAT"): true
canMake("COMMON"): false
canMake("SQUAD"): true
canMake("CONFUSE"): true
```



## Maple



{{ code(src="content/tasks/abc_problem/maple.mpl", lang="maple") }}


Output:

```txt

a: true
Bark: true
bOok: false
treat: true
COMMON: false
squad: true
confuse: true

```



## M2000 Interpreter

We use a subroutine inside a module. Subs are in the same namespace as the module which call them. Subs may exist in the end of module, or in the parent module (which module defined). We have to use Local to define new variables which shadow any module variable. When a sub exit all new variables which made there erased. Modules run on objects which "interprets" code, and subs use modules objects, so they are lighter than modules. A module hold a separate return stack for subs, gosub and for next structures ( a for {} use process stack, and is twice faster as the simple For Next). This return stack is a stack object, which is a collection of objects in heap, so we can use  '''Recursion.Limit 100000''' to set limit to 100000 calls for subs. Here we use a for next and a subroutine, using modules dedicated return stack. We can call can_make_word() using name or using Gosub. Gosub can call subs as labels, and expect Return to return from sub. These routines are more lighter than subs, because they run as code is in module, and any new variable stay until module exit. So we never make local variables or if we want locals we have to use Fopr This { }, the block for temporary definitions.




{{ code(src="content/tasks/abc_problem/m2000_interpreter.m2000", lang="M2000 Interpreter") }}



Output:

```txt
A          True
BaRk       True
BOOK      False
TREAT      True
CommoN    False
SQUAD      True
CONFUSE    True
```

## Mathematica / Wolfram Language


{{ code(src="content/tasks/abc_problem/mathematica_wolfram_language.mathematica", lang="Mathematica") }}


Output:

```txt
ABCBlockQ["A"]
ABCBlockQ["BARK"]
ABCBlockQ["BOOK"]
ABCBlockQ["TREAT"]
ABCBlockQ["COMMON"]
ABCBlockQ["SQUAD"]
ABCBlockQ["CONFUSE"]
True
True
False
True
False
True
True
```


## MATLAB


{{ code(src="content/tasks/abc_problem/matlab.m", lang="MATLAB") }}


Output:

```txt
Can make word A.
Can make word BARK.
CanNOT make word BOOK.
Can make word TREAT.
CanNOT make word COMMON.
Can make word SQUAD.
Can make word CONFUSE.
```



## MAXScript

###  Recursive


Recursively checks if the word is possible if a block is removed from the array.



{{ code(src="content/tasks/abc_problem/maxscript_1.maxscript", lang="MAXScript") }}


Output:


{{ code(src="content/tasks/abc_problem/maxscript_2.maxscript", lang="MAXScript") }}




### Non-recursive


{{ code(src="content/tasks/abc_problem/maxscript_3.maxscript", lang="MAXScript") }}



Both versions are good for this example, but the non-recursive version won't work if the blocks are more random, because it just takes the first found block, and the recursive version decides which one to use.
For example, if blocks are: #("RT","WA","WO","TB","RE")
Then:



{{ code(src="content/tasks/abc_problem/maxscript_4.maxscript", lang="MAXScript") }}



Non-recursive version quickly decides that it's not possible, even though it clearly is.


## Mercury



{{ code(src="content/tasks/abc_problem/mercury.m", lang="Mercury") }}



Note that 'P', in the foldl near the end, is not a boolean variable, but a zero-arity currying of can_make_word (i.e., it's a 'lambda' that takes no arguments and then calls can_make_word with all of the already-supplied arguments).


## MiniScript



{{ code(src="content/tasks/abc_problem/miniscript.ms", lang="MiniScript") }}




## Nim



{{ code(src="content/tasks/abc_problem/nim.nim", lang="nim") }}


Output:

```txt
Can the blocks make the word "A"? yes
Can the blocks make the word "bArK"? yes
Can the blocks make the word "BOOK"? no
Can the blocks make the word "treat"? yes
Can the blocks make the word "common"? no
Can the blocks make the word "sQuAd"? yes
Can the blocks make the word "CONFUSE"? yes
```


## Oberon 2

Works with oo2c Version 2


{{ code(src="content/tasks/abc_problem/oberon_2.oberon2", lang="oberon2") }}


Output:

```txt

A: TRUE
BARK: TRUE
BOOK: FALSE
TREAT: TRUE
COMMON: FALSE
SQAD: TRUE
confuse: TRUE

```



## Objeck

Translated from Java


{{ code(src="content/tasks/abc_problem/objeck.obs", lang="objeck") }}



```txt

"": true
A: true
BARK: true
book: false
treat: true
COMMON: false
SQuAd: true
CONFUSE: true

```



## OCaml



{{ code(src="content/tasks/abc_problem/ocaml.ml", lang="ocaml") }}



Output:

```txt

 $ ocaml canmakeword.ml
 - can make word "A" = true  (should: true)
 - can make word "BARK" = true  (should: true)
 - can make word "BOOK" = false  (should: false)
 - can make word "TREAT" = true  (should: true)
 - can make word "COMMON" = false  (should: false)
 - can make word "SQUAD" = true  (should: true)
 - can make word "CONFUSE" = true  (should: true)

```



## Oforth




{{ code(src="content/tasks/abc_problem/oforth.of", lang="Oforth") }}



Output:

```txt

["A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"] map(#[ ABCBlocks canMakeWord]) .
[1, 1, 0, 1, 0, 1, 1]

```



## OpenEdge/Progress



{{ code(src="content/tasks/abc_problem/openedge_progress.txt", lang="Progress (Openedge ABL)") }}



Output:

```txt

A = yes
BARK = yes
BOOK = no
TREAT = yes
COMMON = no
SQUAD = yes
CONFUSE = yes

```



## PARI/GP



{{ code(src="content/tasks/abc_problem/pari_gp.gp", lang="parigp") }}



Output:
```txt
A	1
Bark	1
BOOK	0
Treat	1
COMMON	0
SQUAD	1
conFUSE	1
```




## Pascal


Works with Free Pascal|2.6.2}}



{{ code(src="content/tasks/abc_problem/pascal.pas", lang="Pascal") }}



Output:


```txt

./ABCProblem.pas
>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True

```



## Perl

Recursive solution that can handle characters appearing on different blocks:


{{ code(src="content/tasks/abc_problem/perl_1.pl", lang="perl") }}


<p>Testing:


{{ code(src="content/tasks/abc_problem/perl_2.pl", lang="perl") }}




## Perl 6

Works with rakudo|6.0.c}}
Blocks are stored as precompiled regexes. We do an initial pass on the blockset to include in the list only those regexes that match somewhere in the current word.  Conveniently, regexes scan the word for us.


{{ code(src="content/tasks/abc_problem/perl_6.p6", lang="perl6") }}


Output:

```txt
A	True
BaRK	True
BOoK	False
tREaT	True
COmMOn	False
SqUAD	True
CoNfuSE	True
```



## Phix




{{ code(src="content/tasks/abc_problem/phix.exw", lang="Phix") }}


Output:

```txt

: True
A: True
BarK: True
BOOK: False
TrEaT: True
COMMON: False
SQUAD: True
CONFUSE: True

```



## PHP




{{ code(src="content/tasks/abc_problem/php.php", lang="PHP") }}


Output:

```txt

A: True
BARK: True
BOOK: False
TREAT: True
COMMON: False
SQUAD: True
Confuse: True

```



## PicoLisp

Mapping and recursion.


{{ code(src="content/tasks/abc_problem/picolisp.l", lang="picolisp") }}




## PL/I


### version 1



{{ code(src="content/tasks/abc_problem/pl_i_1.pli", lang="pli") }}



```txt

A                       true
BARK                    true
BOOK                    false
TREAT                   true
COMMON                  false
SQuAd                   true
CONFUSE                 true

```



### version 2



{{ code(src="content/tasks/abc_problem/pl_i_2.pli", lang="pli") }}


Output:

```txt
'$'       cannot be spelt.
'A'       can be spelt in        2 ways.
'BARK'    can be spelt in        8 ways.
'BOOK'    cannot be spelt.
'TREAT'   can be spelt in        8 ways.
'COMMON'  cannot be spelt.
'SQUAD'   can be spelt in        8 ways.
'CONFUSE' can be spelt in       32 ways.
```



## PowerBASIC

Works with PowerBASIC 6 Console Compiler



{{ code(src="content/tasks/abc_problem/powerbasic.powerbasic", lang="PowerBASIC") }}


Output:

```txt
$ FALSE
A TRUE
bark TRUE
bOOk FALSE
treAT TRUE
COmmon FALSE
sQuaD TRUE
CONFUSE TRUE
GearyChopoff TRUE

```



## PowerShell



{{ code(src="content/tasks/abc_problem/powershell.ps1", lang="powershell") }}


Output:

```txt

VERBOSE: match for letter - a - removing block NA
VERBOSE: a : True
True
VERBOSE: match for letter - b - removing block BO
VERBOSE: match for letter - a - removing block NA
VERBOSE: match for letter - r - removing block RE
VERBOSE: match for letter - k - removing block XK
VERBOSE: bark : True
True
VERBOSE: match for letter - b - removing block BO
VERBOSE: match for letter - o - removing block OB
VERBOSE: match for letter - k - removing block XK
VERBOSE: book : False
False
VERBOSE: match for letter - t - removing block GT
VERBOSE: match for letter - r - removing block RE
VERBOSE: match for letter - e - removing block ER
VERBOSE: match for letter - a - removing block NA
VERBOSE: match for letter - t - removing block TG
VERBOSE: treat : True
True
VERBOSE: match for letter - c - removing block CP
VERBOSE: match for letter - o - removing block BO
VERBOSE: match for letter - m - removing block ZM
VERBOSE: match for letter - o - removing block OB
VERBOSE: match for letter - n - removing block NA
VERBOSE: common : False
False
VERBOSE: match for letter - s - removing block FS
VERBOSE: match for letter - q - removing block DQ
VERBOSE: match for letter - u - removing block HU
VERBOSE: match for letter - a - removing block NA
VERBOSE: match for letter - d - removing block QD
VERBOSE: squad : True
True
VERBOSE: match for letter - c - removing block CP
VERBOSE: match for letter - o - removing block BO
VERBOSE: match for letter - n - removing block NA
VERBOSE: match for letter - f - removing block FS
VERBOSE: match for letter - u - removing block HU
VERBOSE: match for letter - s - removing block FS
VERBOSE: match for letter - e - removing block RE
VERBOSE: confuse : True
True

or without verbose

True
True
False
True
False
True
True


```



## Prolog



###  Traditional


Works with SWI-Prolog 6.5.3



{{ code(src="content/tasks/abc_problem/prolog_1.pro", lang="Prolog") }}


Output:

```txt
 ?- abc_problem.
 OK
A OK
bark OK
bOOk KO
treAT OK
COmmon KO
sQuaD OK
CONFUSE OK
true.

```


###  Constraint Handling Rules


An approach using [CHR <https://dtai.cs.kuleuven.be/CHR/>] via SWI-Prolog's [library(chr) <https://www.swi-prolog.org/pldoc/man?section=chr>] and a module I'm working on for composing predicates [composer](https://github.com/aBathologist/protelog/blob/master/composer.pl):

Works with SWI Prolog 7}}



{{ code(src="content/tasks/abc_problem/prolog_2.pro", lang="Prolog") }}




Demonstration:



{{ code(src="content/tasks/abc_problem/prolog_3.pro", lang="Prolog") }}




## PureBasic


### PureBasic: Iterative



{{ code(src="content/tasks/abc_problem/purebasic_1.pb", lang="purebasic") }}




### PureBasic: Recursive



{{ code(src="content/tasks/abc_problem/purebasic_2.pb", lang="purebasic") }}


Output:

```txt
a               = True
BaRK            = True
BOoK            = False
TREAt           = True
cOMMON          = False
SqUAD           = True
COnFUSE         = True
```



## Python


===Python: Iterative, with tests===


{{ code(src="content/tasks/abc_problem/python_1.py", lang="python") }}



Output:

```txt
'': False, 'a': True, 'baRk': True, 'booK': False, 'treat': True, 'COMMON': False, 'squad': True, 'Confused': True
```



### Python: Recursive



{{ code(src="content/tasks/abc_problem/python_2.py", lang="python") }}



Output:

```txt
Can we spell       ''? False
Can we spell       'A'? True
Can we spell    'BARK'? True
Can we spell    'BoOK'? False
Can we spell   'TrEAT'? True
Can we spell  'COmMoN'? False
Can we spell   'SQUAD'? True
Can we spell 'conFUsE'? True
```


===Python: Recursive, telling how===


{{ code(src="content/tasks/abc_problem/python_3.py", lang="python") }}



Output:
Note the case of empty list returned for empty string; whether it means true or false is up to you.

```txt

'' -> []
'A' -> ['NA']
'bark' -> ['BO', 'NA', 'RE', 'XK']
'book' -> None
'treat' -> ['GT', 'RE', 'ER', 'NA', 'TG']
'common' -> None
'SQUAD' -> ['FS', 'DQ', 'HU', 'NA', 'QD']
'conFUsEd' -> ['CP', 'BO', 'NA', 'FS', 'HU', 'FS', 'RE', 'DQ']

```



## R



### With recursion

Vectorised function for R which will take a character vector and return a logical vector of equal length with TRUE and FALSE as appropriate for words which can/cannot be made with the blocks.



{{ code(src="content/tasks/abc_problem/r_1.r", lang="R") }}



Output:

```txt
      A    BARK    BOOK   TREAT  COMMON   SQUAD CONFUSE
   TRUE    TRUE   FALSE    TRUE   FALSE    TRUE    TRUE
```



### Without recursion

Second version without recursion and giving every unique combination of blocks for each word:


{{ code(src="content/tasks/abc_problem/r_2.r", lang="R") }}


Output:

```txt
$A
     [,1] [,2]
[1,] "AN" "NA"

$BARK
     [,1] [,2] [,3] [,4]
[1,] "BO" "AN" "RE" "XK"
[2,] "OB" "AN" "RE" "XK"
[3,] "BO" "NA" "RE" "XK"
[4,] "OB" "NA" "RE" "XK"
[5,] "BO" "AN" "ER" "XK"
[6,] "OB" "AN" "ER" "XK"
[7,] "BO" "NA" "ER" "XK"
[8,] "OB" "NA" "ER" "XK"

$BOOK
character(0)

$TREAT
     [,1] [,2] [,3] [,4] [,5]
[1,] "GT" "RE" "ER" "AN" "TG"
[2,] "GT" "ER" "RE" "AN" "TG"
[3,] "GT" "RE" "ER" "NA" "TG"
[4,] "GT" "ER" "RE" "NA" "TG"
[5,] "TG" "RE" "ER" "AN" "GT"
[6,] "TG" "ER" "RE" "AN" "GT"
[7,] "TG" "RE" "ER" "NA" "GT"
[8,] "TG" "ER" "RE" "NA" "GT"

$COMMON
character(0)

$SQUAD
     [,1] [,2] [,3] [,4] [,5]
[1,] "FS" "QD" "HU" "AN" "DQ"
[2,] "FS" "QD" "HU" "AN" "DQ"
[3,] "FS" "QD" "HU" "NA" "DQ"
[4,] "FS" "QD" "HU" "NA" "DQ"
[5,] "FS" "DQ" "HU" "AN" "QD"
[6,] "FS" "DQ" "HU" "AN" "QD"
[7,] "FS" "DQ" "HU" "NA" "QD"
[8,] "FS" "DQ" "HU" "NA" "QD"

$CONFUSE
      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
 [1,] "CP" "OB" "NA" "FS" "HU" "FS" "ER"
 [2,] "PC" "OB" "NA" "FS" "HU" "FS" "ER"
 [3,] "CP" "BO" "NA" "FS" "HU" "FS" "ER"
 [4,] "PC" "BO" "NA" "FS" "HU" "FS" "ER"
 [5,] "CP" "OB" "AN" "FS" "HU" "FS" "ER"
 [6,] "PC" "OB" "AN" "FS" "HU" "FS" "ER"
 [7,] "CP" "BO" "AN" "FS" "HU" "FS" "ER"
 [8,] "PC" "BO" "AN" "FS" "HU" "FS" "ER"
 [9,] "CP" "OB" "NA" "FS" "HU" "FS" "ER"
[10,] "PC" "OB" "NA" "FS" "HU" "FS" "ER"
[11,] "CP" "BO" "NA" "FS" "HU" "FS" "ER"
[12,] "PC" "BO" "NA" "FS" "HU" "FS" "ER"
[13,] "CP" "OB" "AN" "FS" "HU" "FS" "ER"
[14,] "PC" "OB" "AN" "FS" "HU" "FS" "ER"
[15,] "CP" "BO" "AN" "FS" "HU" "FS" "ER"
[16,] "PC" "BO" "AN" "FS" "HU" "FS" "ER"
[17,] "CP" "OB" "NA" "FS" "HU" "FS" "RE"
[18,] "PC" "OB" "NA" "FS" "HU" "FS" "RE"
[19,] "CP" "BO" "NA" "FS" "HU" "FS" "RE"
[20,] "PC" "BO" "NA" "FS" "HU" "FS" "RE"
[21,] "CP" "OB" "AN" "FS" "HU" "FS" "RE"
[22,] "PC" "OB" "AN" "FS" "HU" "FS" "RE"
[23,] "CP" "BO" "AN" "FS" "HU" "FS" "RE"
[24,] "PC" "BO" "AN" "FS" "HU" "FS" "RE"
[25,] "CP" "OB" "NA" "FS" "HU" "FS" "RE"
[26,] "PC" "OB" "NA" "FS" "HU" "FS" "RE"
[27,] "CP" "BO" "NA" "FS" "HU" "FS" "RE"
[28,] "PC" "BO" "NA" "FS" "HU" "FS" "RE"
[29,] "CP" "OB" "AN" "FS" "HU" "FS" "RE"
[30,] "PC" "OB" "AN" "FS" "HU" "FS" "RE"
[31,] "CP" "BO" "AN" "FS" "HU" "FS" "RE"
[32,] "PC" "BO" "AN" "FS" "HU" "FS" "RE"
```



## Racket

I believe you can make an empty word by using no blocks.
So '(can-make-word? "")' is true for me.



{{ code(src="content/tasks/abc_problem/racket.rkt", lang="racket") }}



Output:

```txt
Can we make: ""       ? yes
Can we make: "A"      ? yes
Can we make: "BARK"   ? yes
Can we make: "BOOK"   ? no
Can we make: "TREAT"  ? yes
Can we make: "COMMON" ? no
Can we make: "SQUAD"  ? yes
Can we make: "CONFUSE"? yes
```



## RapidQ



{{ code(src="content/tasks/abc_problem/rapidq.vb", lang="vb") }}


Output:

```txt
Can make: A = TRUE
Can make: BARK = TRUE
Can make: BOOK = FALSE
Can make: TREAT = TRUE
Can make: COMMON = FALSE
Can make: SQUAD = TRUE
Can make: CONFUSE = TRUE

```


## Red



{{ code(src="content/tasks/abc_problem/red.red", lang="Red") }}


Output:

```txt

A        : true
bark     : true
book     : false
TrEAT    : true
COmMoN   : false
SQUAD    : true
conFUsE  : true

```


## REXX


### version 1



{{ code(src="content/tasks/abc_problem/rexx_1.rexx", lang="rexx") }}


```txt

                             A    can be spelt.
                          bark    can be spelt.
                          bOOk  can't be spelt.
                         treat    can be spelt.
                        common  can't be spelt.
                         squaD    can be spelt.
                       conFuse    can be spelt.

```



### version 2



{{ code(src="content/tasks/abc_problem/rexx_2.rexx", lang="rexx") }}


Output:

```txt
''        cannot be spelt.
'$'       cannot be spelt.
'A'       can be spelt in 2 ways.
'BARK'    can be spelt in 8 ways.
'BOOK'    cannot be spelt.
'TREAT'   can be spelt in 8 ways.
'COMMON'  cannot be spelt.
'SQUAD'   can be spelt in 8 ways.
'CONFUSE' can be spelt in 32 ways.
```

Output: extended

```txt
''        cannot be spelt.
'$'       cannot be spelt.
'A'       can be spelt in 2 ways.
            NA
            AN
'BARK'    can be spelt in 8 ways.
            BO NA RE XK
            OB NA RE XK
            BO AN RE XK
            OB AN RE XK
            BO NA ER XK
            OB NA ER XK
            BO AN ER XK
            OB AN ER XK
'BOOK'    cannot be spelt.
'TREAT'   can be spelt in 8 ways.
            TG ER RE NA GT
            TG RE ER NA GT
            TG ER RE AN GT
            TG RE ER AN GT
            GT ER RE NA TG
            GT RE ER NA TG
            GT ER RE AN TG
            GT RE ER AN TG
'COMMON'  cannot be spelt.
'SQUAD'   can be spelt in 8 ways.
            FS QD HU NA DQ
            FS QD HU NA DQ
            FS QD HU AN DQ
            FS QD HU AN DQ
            FS DQ HU NA QD
            FS DQ HU NA QD
            FS DQ HU AN QD
            FS DQ HU AN QD
'CONFUSE' can be spelt in 32 ways.
            CP BO NA FS HU FS RE
            PC BO NA FS HU FS RE
            CP OB NA FS HU FS RE
            PC OB NA FS HU FS RE
            CP BO AN FS HU FS RE
            PC BO AN FS HU FS RE
            CP OB AN FS HU FS RE
            PC OB AN FS HU FS RE
            CP BO NA FS HU FS RE
            PC BO NA FS HU FS RE
            CP OB NA FS HU FS RE
            PC OB NA FS HU FS RE
            CP BO AN FS HU FS RE
            PC BO AN FS HU FS RE
            CP OB AN FS HU FS RE
            PC OB AN FS HU FS RE
            CP BO NA FS HU FS ER
            PC BO NA FS HU FS ER
            CP OB NA FS HU FS ER
            PC OB NA FS HU FS ER
            CP BO AN FS HU FS ER
            PC BO AN FS HU FS ER
            CP OB AN FS HU FS ER
            PC OB AN FS HU FS ER
            CP BO NA FS HU FS ER
            PC BO NA FS HU FS ER
            CP OB NA FS HU FS ER
            PC OB NA FS HU FS ER
            CP BO AN FS HU FS ER
            PC BO AN FS HU FS ER
            CP OB AN FS HU FS ER
            PC OB AN FS HU FS ER
```



## Ring



{{ code(src="content/tasks/abc_problem/ring.ring", lang="ring") }}


Output:

```txt

>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
 >>> can_make_word("CONFUSE")
True

```



## Ruby

This one uses a case insensitive regular expression. The 'sub!' method substitutes the first substring it finds and returns nil if nothing is found.


{{ code(src="content/tasks/abc_problem/ruby.rb", lang="ruby") }}


Output:

```txt

"A": true
"BaRK": true
"BOoK": false
"tREaT": true
"COmMOn": false
"SqUAD": true
"CoNfuSE": true
"": true

```



## Run BASIC



{{ code(src="content/tasks/abc_problem/run_basic.bas", lang="unbasic") }}



```txt
A	 True
BARK	 True
BOOK	 False
TREAT	 True
COMMON	 False
SQUAD	 True
Confuse	 True
```



## Rust

This implementation uses a backtracking search.


{{ code(src="content/tasks/abc_problem/rust.rs", lang="rust") }}


Output:

```txt

A -> true
BARK -> true
BOOK -> false
TREAT -> true
COMMON -> false
SQUAD -> true
CONFUSE -> true

```



## Scala


{{ code(src="content/tasks/abc_problem/scala.scala", lang="Scala") }}




## Scheme

In R5RS:


{{ code(src="content/tasks/abc_problem/scheme.scm", lang="scheme") }}


Output:

```txt

   Can make word: A
   Can make word: Bark
Cannot make word: book
   Can make word: TrEaT
Cannot make word: COMMON
   Can make word: squaD
   Can make word: CONFUSE

```



## Seed7



{{ code(src="content/tasks/abc_problem/seed7.sd7", lang="seed7") }}



Output:

```txt
          TRUE
A         TRUE
BARK      TRUE
BOOK      FALSE
TREAT     TRUE
COMMON    FALSE
SQUAD     TRUE
Confuse   TRUE
```


## SequenceL

### Recursive Search Version


{{ code(src="content/tasks/abc_problem/sequencel_1.sl", lang="sequencel") }}



Output:

```txt

cmd:> main.exe A BARK BOOK TREAT COMMON SQUAD CONFUSE
"A: true
BARK: true
BOOK: false
TREAT: true
COMMON: false
SQUAD: true
CONFUSE: true"

```



### RegEx Version



{{ code(src="content/tasks/abc_problem/sequencel_2.sl", lang="sequencel") }}




## Sidef


{{ code(src="content/tasks/abc_problem/sidef_1.sf", lang="ruby") }}



Tests:


{{ code(src="content/tasks/abc_problem/sidef_2.sf", lang="ruby") }}



Output:

```txt

      A -> true
   BARK -> true
   BOOK -> false
  TREAT -> true
 COMMON -> false
  SQUAD -> true
CONFUSE -> true
   auto -> true

```


## Simula



{{ code(src="content/tasks/abc_problem/simula.sim", lang="simula") }}


Output:

```txt
A => T OK
BARK => T OK
BOOK => F OK
TREAT => T OK
COMMON => F OK
SQUAD => T OK
CONFUSE => T OK

```



## Smalltalk

Recursive solution. Tested in Pharo.


{{ code(src="content/tasks/abc_problem/smalltalk.st", lang="smalltalk") }}


Output:

```txt

ABCPuzzle new test

A: true
BARK: true
BOOK: false
TreaT: true
COMMON: false
sQUAD: true
CONFuSE: true

```



## SPAD

Works with FriCAS, OpenAxiom, Axiom}}


{{ code(src="content/tasks/abc_problem/spad.spad", lang="SPAD") }}



Programming details:[UserGuide](https://fricas.github.io/book.pdf)

Output:

```txt


  [true,true,false,true,false,true,true]
                                                     Type: List(Boolean)

```


There is optimization potential of course.



## Swift



{{ code(src="content/tasks/abc_problem/swift_1.swift", lang="Swift") }}


Output:

```txt

'A' can be spelled with blocks.
'BARK' can be spelled with blocks.
'BooK' cannot be spelled with blocks.
'TrEaT' can be spelled with blocks.
'comMON' cannot be spelled with blocks.
'sQuAd' can be spelled with blocks.
'Confuse' can be spelled with blocks.

```


Works with Swift|3.0.2}}


{{ code(src="content/tasks/abc_problem/swift_2.swift", lang="Swift") }}


Output:

```txt

A true
BARK true
BooK false
TrEaT true
comMON false
sQuAd true
Confuse true

```



## Tcl

Works with Tcl|8.6}}


{{ code(src="content/tasks/abc_problem/tcl.tcl", lang="tcl") }}


Output:

```txt

Can we spell        ''? false
Can we spell       'A'? true
Can we spell    'BARK'? true
Can we spell    'BOOK'? false
Can we spell   'TREAT'? true
Can we spell  'COMMON'? false
Can we spell   'SQUAD'? true
Can we spell 'CONFUSE'? true

```



## TUSCRIPT



{{ code(src="content/tasks/abc_problem/tuscript.tuscript", lang="tuscript") }}


Output:

```txt
A true
BARK true
BOOK false
TREAT true
COMMON false
SQUAD true
CONFUSE true
```



## TXR




{{ code(src="content/tasks/abc_problem/txr.txr", lang="txr") }}



Run:


```txt
$ cat abc-problem.data
a
bark
book
treat
common
squad
confuse
$ txr abc-problem.txr abc-problem.data
>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True
```



## UNIX Shell

Works with bash}}



{{ code(src="content/tasks/abc_problem/unix_shell.sh", lang="bash") }}



Output:

```txt
	no
A	yes
BARK	yes
Book	no
treat	yes
COMMON	no
Squad	yes
confuse	yes
```



## UTFool


'''String-based solution'''



{{ code(src="content/tasks/abc_problem/utfool_1.utfool", lang="UTFool") }}



'''Collection-based solution'''



{{ code(src="content/tasks/abc_problem/utfool_2.utfool", lang="UTFool") }}




## VBA




{{ code(src="content/tasks/abc_problem/vba.vba", lang="vb") }}


Output:

```txt

>>> can_make_word A => True
>>> can_make_word BARK => True
>>> can_make_word BOOK => False
>>> can_make_word TREAT => True
>>> can_make_word COMMON => False
>>> can_make_word SQUAD => True
>>> can_make_word CONFUSE => True
```



## Yabasic



{{ code(src="content/tasks/abc_problem/yabasic.yab", lang="Yabasic") }}




## zkl

Translated from C


{{ code(src="content/tasks/abc_problem/zkl.zkl", lang="zkl") }}


Output:

```txt

True:
True: A
True: BarK
False: BOOK
True: TREAT
False: COMMON
True: SQUAD
True: Confuse
True: abba

```



## zonnon



{{ code(src="content/tasks/abc_problem/zonnon.zonnon", lang="zonnon") }}


Output:

```txt
                  A   ?  true
                BARK   ?  true
                BOOK   ? false
               TREAT   ?  true
              COMMON   ? false
             confuse   ?  true
```



## ZX Spectrum Basic



{{ code(src="content/tasks/abc_problem/zx_spectrum_basic.bas", lang="zxbasic") }}


Output:

```txt
Can make word A? Yes
Can make word BARK? Yes
Can make word BOOK? No
Can make word TREAT? Yes
Can make word COMMON? No
Can make word SQUAD? Yes
Can make word CONFYUSE? Yes
```


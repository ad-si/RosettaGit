+++
title = "Quine"
description = ""
date = 2019-09-06T18:57:15Z
aliases = []
[extra]
id = 2210
[taxonomies]
categories = []
tags = []
+++

{{task}}

A [[wp:Quine_%28computing%29|Quine]] is a self-referential program that can, 
without any external access, output its own source. 

It is named after the [[wp:Willard_Van_Orman_Quine|philosopher and logician]] 
who studied self-reference and quoting in natural language, 
as for example in the paradox "'Yields falsehood when preceded by its quotation' yields falsehood when preceded by its quotation."

"Source" has one of two meanings. It can refer to the text-based program source.  
For languages in which program source is represented as a data structure, "source" may refer to the data structure: quines in these languages fall into two categories: programs which print a textual representation of themselves, or expressions which evaluate to a data structure which is equivalent to that expression.

The usual way to code a Quine works similarly to this paradox: The program consists of two identical parts, once as plain code and once ''quoted'' in some way (for example, as a character string, or a literal data structure). The plain code then accesses the quoted code and prints it out twice, once unquoted and once with the proper quotation marks added. Often, the plain code and the quoted code have to be nested.


;Task:
Write a program that outputs its own source code in this way. If the language allows it, you may add a variant that accesses the code directly. You are not allowed to read any external files with the source code. The program should also contain some sort of self-reference, so constant expressions which return their own value which some top-level interpreter will print out.  Empty programs producing no output are not allowed.

There are several difficulties that one runs into when writing a quine, mostly dealing with quoting:
* Part of the code usually needs to be stored as a string or structural literal in the language, which needs to be quoted somehow. However, including quotation marks in the string literal itself would be troublesome because it requires them to be escaped, which then necessitates the escaping character (e.g. a backslash) in the string, which itself usually needs to be escaped, and so on.
** Some languages have a function for getting the "source code representation" of a string (i.e. adds quotation marks, etc.); in these languages, this can be used to circumvent the quoting problem.
** Another solution is to construct the quote character from its [[character code]], without having to write the quote character itself. Then the character is inserted into the string at the appropriate places. The ASCII code for double-quote is 34, and for single-quote is 39.
* Newlines in the program may have to be reproduced as newlines in the string, which usually requires some kind of escape sequence (e.g. "\n"). This causes the same problem as above, where the escaping character needs to itself be escaped, etc.
** If the language has a way of getting the "source code representation", it usually handles the escaping of characters, so this is not a problem.
** Some languages allow you to have a string literal that spans multiple lines, which embeds the newlines into the string without escaping.
** Write the entire program on one line, for free-form languages (as you can see for some of the solutions here, they run off the edge of the screen), thus removing the need for newlines. However, this may be unacceptable as some languages require a newline at the end of the file; and otherwise it is still generally good style to have a newline at the end of a file. (The task is not clear on whether a newline is required at the end of the file.) Some languages have a print statement that appends a newline; which solves the newline-at-the-end issue; but others do not.



'''Next to the Quines presented here, many other versions can be found on the [http://www.nyx.net/~gthompso/quine.htm Quine] page.'''





## ABAP

I copied one of my examples from http://www.yaabb.de/viewtopic.php?t=44

```ABAP
REPORT R NO STANDARD PAGE HEADING LINE-SIZE 67.
DATA:A(440),B,C,N(3) TYPE N,I TYPE I,S.
A+000 = 'REPORT R NO STANDARD PAGE HEADING LINE-SIZE 6\7.1DATA:A'.
A+055 = '(440),B,C,N(\3) TYPE N,I TYPE I,S.?1DO 440 TIMES.3C = A'.
A+110 = '+I.3IF B = S.5IF C CA `\\\?\1\3\5\7`.7B = C.5ELSEIF C ='.
A+165 = ' `\``.7WRITE ```` NO-GAP.5ELSE.7WRITE C NO-GAP.5ENDIF.3'.
A+220 = 'ELSEIF B = `\\`.5WRITE C NO-GAP.5B = S.3ELSEIF B = `\?`'.
A+275 = '.5DO 8 TIMES.7WRITE:/ `A+` NO-GAP,N,`= ``` NO-GAP,A+N(\'.
A+330 = '5\5) NO-GAP,```.`.7N = N + \5\5.5ENDDO.5B = C.3ELSE.5WR'.
A+385 = 'ITE AT /B C NO-GAP.5B = S.3ENDIF.3I = I + \1.1ENDDO.   '.
DO 440 TIMES.
  C = A+I.
  IF B = S.
    IF C CA '\?1357'.
      B = C.
    ELSEIF C = '`'.
      WRITE '''' NO-GAP.
    ELSE.
      WRITE C NO-GAP.
    ENDIF.
  ELSEIF B = '\'.
    WRITE C NO-GAP.
    B = S.
  ELSEIF B = '?'.
    DO 8 TIMES.
      WRITE:/ 'A+' NO-GAP,N,'= ''' NO-GAP,A+N(55) NO-GAP,'''.'.
      N = N + 55.
    ENDDO.
    B = C.
  ELSE.
    WRITE AT /B C NO-GAP.
    B = S.
  ENDIF.
  I = I + 1.
ENDDO.
```

Here is another one, requiring SY-SAPRL >= '620':

```ABAP
REPORT R NO STANDARD PAGE HEADING.DATA:A TYPE TABLE OF STRING,B(8).APPEND:
`REPORT.FORM F TABLES T.NEW-PAGE LINE-SIZE 78.WRITE:'REPORT R NO',` TO A,
`'STANDARD PAGE HEADING.DATA:A TYPE TABLE OF STRING,B(8).APPEND:'.LOOP` TO A,
`AT T.REPLACE ALL OCCURENCES OF'``' IN T WITH'````'.WRITE:/'``'` TO A,
`NO-GAP,T NO-GAP,'`` TO A,'.ENDLOOP.WRITE:AT 78'.','GENERATE',` TO A,
`'SUBROUTINE POOL A NAME B.PERFORM F IN PROGRAM (B) TABLES A.'.ENDFORM.` TO A.
GENERATE SUBROUTINE POOL A NAME B.PERFORM F IN PROGRAM (B) TABLES A.
```

Please note that the program still works if you fix the spelling error (s/OCCURENCES/OCCURRENCES/), thus increasing the size of the source code by 1 character.

One more program, created in a 7.0 unicode system, but it should work in any SAP system with SY-SAPRL >= '700', even if you'll have a hard time verifying the correctness just by studying the source code:

```ABAP
REPORT A NO STANDARD
PAGE HEADING
LINE-SIZE
72.DATA:C(33),A LIKE
TABLE OF C,X(3333)
TYPE X,Y TYPE
XSTRING.DEFINE A.X+C
='&1'.C = C + 33.
END-OF-DEFINITION.
DEFINE B.LOOP AT A
INTO C &1 &2.WRITE
/ C.ENDLOOP.
END-OF-DEFINITION.
A FF060201020280003431303300000000AC030000121F9D02BB522ADA69108AA1C7 .
A E8B32FFEC07DD21907936962B28407983089732C8811FC4413FD02A7BFE6690B03 .
A 262F72B38EF69EB6A7A71C9F82CF44CC11469E081F86785777F269DE372CE9EC4B .
A 5E0A24D8224781128E290E1B7A0ECFF423BEDFD316B43B456FE9AD98E1F0401B31 .
A 9E11B3A23F3C865EEB6D028FD532BC69DED831F41DE6F0B59F745E604996373C97 .
A 982A2FA9F6C81A86164CCC98D4CC91D22E89AB9A1CCBEB6A97A839A5602BA26AFE .
A 7791BF4C2A9DBE6866134E093BD82CA291CF2A57EC67E81017384740EB33E6102A .
A 174784531EFEA076A29A7ACAD9EB55CED8316374D3E00D3DEC1CF36E4D4C4EE64E .
A 75B28DB568C195BA3DE92F9CC48AAAAF3A4DD9CC6BE899E27C18A3B66ECBF65093 .
A FFF1168545878AD10C4F075F588821EF947739516EBF7D99F5851D52F629E8D5AC .
A 13EF77291306AA6CABF7B56EC9E273F47997DA3FE146FB2A2C30E3BE22FEA603B4 .
A EDB5FBEE64A7637B35B46DD79491EEC2D1A19B26C0ADAAB2FB39F9050000000000 .
Y = X.IMPORT A = A
FROM DATA BUFFER Y.B
TO 13.C = 0.DO 12
TIMES.WRITE:/'A',
X+C(33),'.'.C = C +
33.ENDDO.B FROM 14.
```


And a final one (cheating, even though not using READ REPORT), it should work in any SAP system with SY-SAPRL >= '46B':

```ABAP
.REPORT Q NO STANDARD PAGE HEADING LINE-SIZE 72
.DATA QQ(69) OCCURS 0
.DATA Q LIKE QQ WITH HEADER LINE
.APPEND 'SYNTAX-TRACE ON.INCLUDE' TO QQ
.APPEND SY-REPID TO QQ
.APPEND '.' TO QQ
.SYNTAX-CHECK FOR QQ MESSAGE Q LINE Q WORD Q TRACE-TABLE Q
.LOOP AT Q
.CHECK Q(1) = '#'
.Q = Q+5
.IF Q < Q OR SY-SAPRL > '5'
.SPLIT Q AT '' INTO Q Q
.ENDIF
.IF Q < Q OR SY-LINNO < 22
.CHECK Q CA 'Q.' OR Q+4 = 'F'
.ENDIF
.IF Q < Q OR SY-LINNO > 23
.CHECK Q CA '.'
.ENDIF
.CHECK Q(1) NA 'IT+' OR Q+1(1) = 'F'
.WRITE / '.' NO-GAP
.WRITE Q
.ENDLOOP
.WRITE / '.'
.
```



## ACL2


```Lisp
(defun print-quine (quine)
   (cw quine quine))
(print-quine
"(defun print-quine (quine)
  (cw quine quine))
(print-quine ~x0)~%")
```


A shorter one:


```Lisp
(let((q"(let((q~x0))(cw q q))"))(cw q q))
```



## Ada

The program text must be in one line.
<div style="width:100%;overflow:scroll">

```Ada
with Ada.Text_IO;procedure Self is Q:Character:='"';A:String:="with Ada.Text_IO;procedure Self is Q:Character:='&#39;;A:String:=;begin Ada.Text_IO.Put_Line(A(1..49)&Q&A(50..61)&Q&A&Q&A(62..A'Last));end Self;";begin Ada.Text_IO.Put_Line(A(1..49)&Q&A(50..61)&Q&A&Q&A(62..A'Last));end Self;
```

</div>


## Aime


```aime
integer f;
text s, t;

f = 36;
s = "integer f;
text s, t;

f = 36;
s = \"\";

o_text(cut(s, 0, f));
o_text(cut(s, 0, f - 1));
o_etext(cut(s, f - 1, 2));
o_text(cut(s, f + 1, 8888 - f));
o_text(cut(s, f, 8888 - f));
";

o_text(cut(s, 0, f));
o_text(cut(s, 0, f - 1));
o_etext(cut(s, f - 1, 2));
o_text(cut(s, f + 1, 8888 - f));
o_text(cut(s, f, 8888 - f));
```



## ALGOL 68

The following program assumes that the target machine is ASCII, hence the use of character 34 as a double quote.

```algol68
STRINGa="STRINGa=,q=REPR34;print(a[:8]+q+a+q+a[9:])",q=REPR34;print(a[:8]+q+a+q+a[9:])
```

The following is a shorter and character set independent - hence portable - implementation.

```algol68
[]CHARa="[]CHARa="";print(2*a[:9]+2*a[9:])";print(2*a[:9]+2*a[9:])
```

The original program - from which this is derived - was written by Richard Wendland, who is one of the team who implemented Algol 68 on Honeywell's Multics. [http://portal.acm.org/ft_gateway.cfm?id=1061754&type=pdf The original can be found in Algol Bulletin 46 - 2.1 - Page 5].


## Applesoft BASIC


```Applesoft BASIC>10 LIST</lang



## AutoHotkey

All from http://www.autohotkey.com/forum/viewtopic.php?t=14336:
The "cheating" way:

```AutoHotkey
FileRead, quine, %A_ScriptFullPath%
MsgBox % quine
```

Another:

```AutoHotkey
D(n, s) 
{ 
   global 
   Loop %n% 
   { 
      l := %s%%A_Index% 
      If l = # 
         l := "script =" . nl . "( %" . nl . script . nl . ")" 
      FileAppend %l%%nl%, %A_ScriptDir%\Q.txt 
   } 
} 
nl := Chr(13) . Chr(10) 
script = 
( % 
D(n, s) 
{ 
   global 
   Loop %n% 
   { 
      l := %s%%A_Index% 
      If l = # 
         l := "script =" . nl . "( %" . nl . script . nl . ")" 
      FileAppend %l%%nl%, %A_ScriptDir%\Q.txt 
   } 
} 
nl := Chr(13) . Chr(10) 
# 
StringSplit q, script, %nl% 
D(q0, "q") 
) 
StringSplit q, script, %nl% 
D(q0, "q")
```

Another:
<div style="width:100%;overflow:scroll">

```AutoHotkey
quote := Chr(34) 
sep := Chr(36) 
nl := Chr(13) . Chr(10) 
script := "quote := Chr(34)$sep := Chr(36)$nl := Chr(13) . Chr(10)$script := #$s := script$StringReplace script, script, %sep%, %nl%, All$StringReplace script, script, #, %quote%%s%%quote%$FileAppend %script%, %A_ScriptDir%\Q.txt" 
s := script 
StringReplace script, script, %sep%, %nl%, All 
StringReplace script, script, #, %quote%%s%%quote% 
FileAppend %script%, %A_ScriptDir%\Q.txt
```
</div>
Another "cheating" method:

```AutoHotkey
FileCopy, %A_ScriptFullPath%, %A_ScriptDir%\Copy-Of--%A_ScriptName%
```


## AWK


### version 1


```AWK

BEGIN{c="BEGIN{c=%c%s%c;printf(c,34,c,34);}";printf(c,34,c,34);}

```


### version 2


```AWK

BEGIN{c="BEGIN{c=%c%s%c;printf c,34,c,34}";printf c,34,c,34}

```




## Babel


Demonstrating from the commandline:


```babel
% bin/babel quine.sp
{ "{ '{ ' << dup [val 0x22 0xffffff00 ] dup <- << << -> << ' ' << << ' }' << } !" { '{ ' << dup [val 0x22 0xffffff00 ] dup <- << << -> << ' ' << << '}' << } ! }%
%
% cat quine.sp
{ "{ '{ ' << dup [val 0x22 0xffffff00 ] dup <- << << -> << ' ' << << ' }' << } !" { '{ ' << dup [val 0x22 0xffffff00 ] dup <- << << -> << ' ' << << '}' << } ! }%
%
```


Demonstrating in interactive mode:


```babel
% bin/babel
babel> { "{ '{ ' << dup [val 0x22 0xffffff00 ] dup <- << << -> << ' ' << << ' }' << } !" { '{ ' << dup [val 0x22 0xffffff00 ] dup <- << << -> << ' ' << << '}' << } ! }
babel> eval
{ "{ '{ ' << dup [val 0x22 0xffffff00 ] dup <- << << -> << ' ' << << ' }' << } !" { '{ ' << dup [val 0x22 0xffffff00 ] dup <- << << -> << ' ' << << '
}' << } !}babel>
babel>
```



## BASIC

{{header|ZX Spectrum Basic}}
For dialects of BASIC that include the <code>LIST</code> command, Quines are trivial.

```qbasic>10 LIST</lang


For dialects that include the <code>DATA</code> keyword, it is almost as easy.
{{works with|QBasic}}

```qbasic
READ d$
DO
    READ x$
    PRINT x$
LOOP UNTIL LEN(x$) < 1
RESTORE
DO
    READ x$
    PRINT d$; CHR$(34); x$; CHR$(34)
LOOP UNTIL LEN(x$) < 1
END

DATA "DATA "
DATA "READ d$"
DATA "DO"
DATA "    READ x$"
DATA "    PRINT x$"
DATA "LOOP UNTIL LEN(x$) < 1"
DATA "RESTORE"
DATA "DO"
DATA "    READ x$"
DATA "    PRINT d$; CHR$(34); x$; CHR$(34)"
DATA "LOOP UNTIL LEN(x$) < 1"
DATA "END"
DATA ""
```


=
## BaCon
=

```qbasic
?SOURCE$;
```
 or more in line with the task description 
```freebasic
a$=SOURCE$:?a$;
```


{{out}}

```txt
prompt$ echo -n 'a$=SOURCE$:?a$;' > quine.bac
prompt$ bacon -q quine.bac
Converting 'quine.bac'... done, 0 lines were processed in 0.005 seconds.
Compiling 'quine.bac'... cc  -c quine.bac.c
cc -o quine quine.bac.o -lbacon -lm
Done, program 'quine' ready.
prompt$ ./quine
a$=SOURCE$:?a$;prompt$
```

If the source file ends in a newline it will work just as well, the newline will be part of the quine.

And one that is a little less cheaty and a little more meaty:


```freebasic
s$="s$=%c%s%c:?34,s$,34 FORMAT s$":?34,s$,34 FORMAT s$
```


{{out}}

```txt
prompt$ ./quine2
s$="s$=%c%s%c:?34,s$,34 FORMAT s$":?34,s$,34 FORMAT s$prompt$
```


=
## Sinclair ZX81 BASIC
=
We can of course do it trivially with <code>10 LIST</code>; but, if that feels like cheating, we can also <code>PEEK</code> the source code out of memory and print it.

Works with 1k of RAM.

```basic
  10 LET L$="10"
  20 LET I=VAL "16512"
  30 PRINT TAB (VAL "4"-LEN L$);L$;
  40 LET I=I+VAL "1"
  50 LET C=PEEK I
  60 IF C=VAL "118" THEN GOTO VAL "90"
  70 PRINT CHR$ C;
  80 GOTO VAL "40"
  90 PRINT
 100 LET L$=STR$ (VAL L$+VAL "10")
 110 LET I=I+VAL "4"
 120 IF VAL L$<=VAL "120" THEN GOTO VAL "30"
```

{{out}}
Exactly the same.

A couple of notes on how it works:

(1) all the numbers in the source code are dressed up as strings, so that the interpreter does not parse them as numbers and include their floating-point representation in the tokenized source;

(2) when the system lists a program, it allocates four columns for the line number—we replicate this behaviour;

(3) character 118 is the new line character.


## bash


```sh
mapfile < $0
printf "%s" ${MAPFILE[@]}
```


```sh
history | tail -n 1 | cut -c 8-
```



## Batch File


```dos
@type %0
```

For Windows 2000 and later only:

```windowsnt
@type "%~f0"
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      PRINT $(PAGE+22)$(PAGE+21):REM PRINT $(PAGE+22)$(PAGE+21):REM
```

{{works with|ARM BBC BASIC}}

```bbcbasic
      PRINT $(PAGE+23)$(PAGE+22):REM PRINT $(PAGE+23)$(PAGE+22):REM
```



## beeswax

Instruction pointers in beeswax programs can drop values in their own source code (growing the program space, if necessary), or pick up values from everywhere in the source code.


```beeswax
_4~++~+.@1~0@D@1J
```

or

```beeswax
*4~++~+.@1~0@D@1J
```


The instruction pointer starts at the left, travels to the right, and at instruction <code>D</code> the IP drops the current value at the top of its own stack—96, or ASCII value for <code>`</code> —left of the source code, which changes the program to:


```beeswax
`_4~++~+.@1~0@D@1J
```

or

```beeswax
`*4~++~+.@1~0@D@1J
```

respectively.

Arriving at instruction <code>J</code> (jump) the instruction pointer jumps to the newly dropped instruction, which switches the IP to print out mode, and it outputs every encountered symbol or value to STDOUT until it encounters the next <code>`</code> or the program ends.

A version that does not modify itself:


```beeswax
`_4~++~+.}1fJ
```



## Befunge

The code space is also the data space of a Befunge program. Programs can be read and modified on the fly. This quine works by reading and printing each character of the source. (This is a implicit loop, since the Befunge codespace wraps around.)

```Befunge
:0g,:66+`#@_1+
```



## Bob


```bob

c=","; n="\n"; q="\""; s="\\";
v=\[
"c=\",\"; n=\"\\n\"; q=\"\\\"\"; s=\"\\\\\";",
"v=\\[",
"define prtQuote(str) {",
" local j,t,v;",
" stdout.Display(q);",
" for (j=0; j<str.size; j++) {",
"  t = str.Substring(j,1);",
"  if (t==q) { stdout.Display(s); }",
"  if (t==s) { stdout.Display(s); }",
"  stdout.Display(t);",
" }",
" stdout.Display(q);",
"}",
"for(i=0; i<2; i++){ stdout.Display(v[i]); stdout.Display(n); }",
"for(i=0; i<v.size-1; i++){ prtQuote(v[i]); stdout.Display(c); stdout.Display(n); }",
"prtQuote(v[v.size-1]); stdout.Display(n);",
"stdout.Display(v[v.size-1]); stdout.Display(n);",
"for(i=2; i<v.size-1; i++){ stdout.Display(v[i]); stdout.Display(n); }",
"];"
];
define prtQuote(str) {
 local j,t,v;
 stdout.Display(q);
 for (j=0; j<str.size; j++) {
  t = str.Substring(j,1);
  if (t==q) { stdout.Display(s); }
  if (t==s) { stdout.Display(s); }
  stdout.Display(t);
 }
 stdout.Display(q);
}
for(i=0; i<2; i++){ stdout.Display(v[i]); stdout.Display(n); }
for(i=0; i<v.size-1; i++){ prtQuote(v[i]); stdout.Display(c); stdout.Display(n); }
prtQuote(v[v.size-1]); stdout.Display(n);
stdout.Display(v[v.size-1]); stdout.Display(n);
for(i=2; i<v.size-1; i++){ stdout.Display(v[i]); stdout.Display(n); }

```

Bob is a dynamic object-oriented language with syntax similar to C/C++, Java, and JavaScript.  Bob was created by David Betz, a former technical editor for DDJ, and the author of XLisp and XScheme, among other languages.

```bracmat
quine$
```


=={{header|Brainf***}}==

```bf
->+>+++>>+>++>+>+++>>+>++>>>+>+>+>++>+>>>>+++>+>>++>+>+++>>++>++>>+>>+>++>++>+>>>>+++>+>>>>++>++>>>>+>>++>+>+++>>>++>>++++++>>+>>++>
+>>>>+++>>+++++>>+>+++>>>++>>++>>+>>++>+>+++>>>++>>+++++++++++++>>+>>++>+>+++>+>+++>>>++>>++++>>+>>++>+>>>>+++>>+++++>>>>++>>>>+>+>+
+>>+++>+>>>>+++>+>>>>+++>+>>>>+++>>++>++>+>+++>+>++>++>>>>>>++>+>+++>>>>>+++>>>++>+>+++>+>+>++>>>>>>++>>>+>>>++>+>>>>+++>+>>>+>>++>+
>++++++++++++++++++>>>>+>+>>>+>>++>+>+++>>>++>>++++++++>>+>>++>+>>>>+++>>++++++>>>+>++>>+++>+>+>++>+>+++>>>>>+++>>>+>+>>++>+>+++>>>+
+>>++++++++>>+>>++>+>>>>+++>>++++>>+>+++>>>>>>++>+>+++>>+>++>>>>+>+>++>+>>>>+++>>+++>>>+[[->>+<<]<+]+++++[->+++++++++<]>.[+]>>
[<<+++++++[->+++++++++<]>-.------------------->-[-<.<+>>]<[+]<+>>>]<<<[-[-[-[>>+<++++++[->+++++<]]>++++++++++++++<]>+++<]++++++
[->+++++++<]>+<<<-[->>>++<<<]>[->>.<<]<<]
```



## Burlesque


Using the official interpreter:


```burlesque

blsq ) "I'm a quine."
"I'm a quine."

```


Every string, every number, every block is a quine.


## C


```c>#include <stdio.h


static char sym[] = "\n\t\\\"";

int main(void) {
	const char *code = "#include <stdio.h>%c%cstatic char sym[] = %c%cn%ct%c%c%c%c%c;%c%cint main(void) {%c%cconst char *code = %c%s%c;%c%cprintf(code, sym[0], sym[0], sym[3], sym[2], sym[2], sym[2], sym[2], sym[2], sym[3], sym[3], sym[0], sym[0], sym[0], sym[1], sym[3], code, sym[3], sym[0], sym[1], sym[0], sym[0], sym[1], sym[0], sym[0]);%c%c%creturn 0;%c}%c";
	printf(code, sym[0], sym[0], sym[3], sym[2], sym[2], sym[2], sym[2], sym[2], sym[3], sym[3], sym[0], sym[0], sym[0], sym[1], sym[3], code, sym[3], sym[0], sym[1], sym[0], sym[0], sym[1], sym[0], sym[0]);

	return 0;
}

```



### Shorter version


```c>#include <stdio.h

int main(){char*c="#include <stdio.h>%cint main(){char*c=%c%s%c;printf(c,10,34,c,34,10);return 0;}%c";printf(c,10,34,c,34,10);return 0;}

```



## C sharp


```csharp
class Program { static void Main() { var s = "class Program {{ static void Main() {{ var s = {0}{1}{0}; System.Console.WriteLine(s, (char)34, s); }} }}"; System.Console.WriteLine(s, (char)34, s); } }
```



## C++


```cpp>#include<cstdio

int main(){char n[]=R"(#include<cstdio>
int main(){char n[]=R"(%s%c";printf(n,n,41);})";printf(n,n,41);}
```



## C1R

The C1R compiler usually copies C solutions from tasks at Rosetta Code. The Quine task is an exception; it gets special treatment in the C1R compiler.

```c>Quine</lang



## Ceylon


```ceylon
shared void run() {print(let (x = """shared void run() {print(let (x = $) x.replaceFirst("$", "\"\"\"" + x + "\"\"\""));}""") x.replaceFirst("$", "\"\"\"" + x + "\"\"\""));}
```



## Clojure


```clojure
((fn [x] (list x (list (quote quote) x))) (quote (fn [x] (list x (list (quote quote) x)))))
```

A shorter but less interesting version:

```clojure
(#(print (str "(#" % " '" % ")")) '(print (str "(#" % " '" % ")")))
```



## COBOL

Here is one that works with GnuCOBOL, December 2015, by Simon Sobisch, tweaked by Bill Woodger.

One line, 150 characters.  Some warnings regarding relaxed syntax assumptions when compiled with

    cobc -x -free -frelax quine.cob


```cobol

linkage section. 78 c value "display 'linkage section. 78 c value ' x'22' c x'222e20' c.". display 'linkage section. 78 c value ' x'22' c x'222e20' c.
```


The following two quines were in a gray past (around 2004?) posted to the (currently inaccessible) language forum of the [http://mvshelp.net/vbforums/ mvsHelp Boards]

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRICE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           SYMBOLIC CHARACTERS FULL-STOP IS 76.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT1.
       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE
           RECORDING MODE F
           LABEL RECORDS OMITTED.
       01  OUTPUT-RECORD                     PIC X(80).
       WORKING-STORAGE SECTION.
       01  SUB-X                             PIC S9(4) COMP.
       01  SOURCE-FACSIMILE-AREA.
           02  SOURCE-FACSIMILE-DATA.
               03  FILLER                    PIC X(40) VALUE
               "       IDENTIFICATION DIVISION.         ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       PROGRAM-ID. GRICE.               ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       ENVIRONMENT DIVISION.            ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       CONFIGURATION SECTION.           ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       SPECIAL-NAMES.                   ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           SYMBOLIC CHARACTERS FULL-STOP".
               03  FILLER                    PIC X(40) VALUE
               " IS 76.                                 ".
               03  FILLER                    PIC X(40) VALUE
               "       INPUT-OUTPUT SECTION.            ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       FILE-CONTROL.                    ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           SELECT OUTPUT-FILE ASSIGN TO ".
               03  FILLER                    PIC X(40) VALUE
               "OUTPUT1.                                ".
               03  FILLER                    PIC X(40) VALUE
               "       DATA DIVISION.                   ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       FILE SECTION.                    ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       FD  OUTPUT-FILE                  ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           RECORDING MODE F             ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           LABEL RECORDS OMITTED.       ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       01  OUTPUT-RECORD                ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC X(80).                         ".
               03  FILLER                    PIC X(40) VALUE
               "       WORKING-STORAGE SECTION.         ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       01  SUB-X                        ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC S9(4) COMP.                    ".
               03  FILLER                    PIC X(40) VALUE
               "       01  SOURCE-FACSIMILE-AREA.       ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           02  SOURCE-FACSIMILE-DATA.   ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "               03  FILLER               ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC X(40) VALUE                    ".
               03  FILLER                    PIC X(40) VALUE
               "           02  SOURCE-FACSIMILE-TABLE RE".
               03  FILLER                    PIC X(40) VALUE
               "DEFINES                                 ".
               03  FILLER                    PIC X(40) VALUE
               "                   SOURCE-FACSIMILE-DATA".
               03  FILLER                    PIC X(40) VALUE
               ".                                       ".
               03  FILLER                    PIC X(40) VALUE
               "               03  SOURCE-FACSIMILE OCCU".
               03  FILLER                    PIC X(40) VALUE
               "RS 68.                                  ".
               03  FILLER                    PIC X(40) VALUE
               "                   04  SOURCE-FACSIMILE-".
               03  FILLER                    PIC X(40) VALUE
               "ONE  PIC X(40).                         ".
               03  FILLER                    PIC X(40) VALUE
               "                   04  SOURCE-FACSIMILE-".
               03  FILLER                    PIC X(40) VALUE
               "TWO  PIC X(40).                         ".
               03  FILLER                    PIC X(40) VALUE
               "       01  FILLER-IMAGE.                ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           02  FILLER                   ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC X(15) VALUE SPACES.            ".
               03  FILLER                    PIC X(40) VALUE
               "           02  FILLER                   ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC X     VALUE QUOTE.             ".
               03  FILLER                    PIC X(40) VALUE
               "           02  FILLER-DATA              ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC X(40).                         ".
               03  FILLER                    PIC X(40) VALUE
               "           02  FILLER                   ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC X     VALUE QUOTE.             ".
               03  FILLER                    PIC X(40) VALUE
               "           02  FILLER                   ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC X     VALUE FULL-STOP.         ".
               03  FILLER                    PIC X(40) VALUE
               "           02  FILLER                   ".
               03  FILLER                    PIC X(40) VALUE
               "     PIC X(22) VALUE SPACES.            ".
               03  FILLER                    PIC X(40) VALUE
               "       PROCEDURE DIVISION.              ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       MAIN-LINE SECTION.               ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       ML-1.                            ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           OPEN OUTPUT OUTPUT-FILE.     ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE 1 TO SUB-X.             ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       ML-2.                            ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE SOURCE-FACSIMILE (SUB-X)".
               03  FILLER                    PIC X(40) VALUE
               " TO OUTPUT-RECORD.                      ".
               03  FILLER                    PIC X(40) VALUE
               "           WRITE OUTPUT-RECORD.         ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           IF  SUB-X < 19               ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "               ADD 1 TO SUB-X           ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "               GO TO ML-2.              ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE 1 TO SUB-X.             ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       ML-3.                            ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE SOURCE-FACSIMILE (20) TO".
               03  FILLER                    PIC X(40) VALUE
               " OUTPUT-RECORD.                         ".
               03  FILLER                    PIC X(40) VALUE
               "           WRITE OUTPUT-RECORD.         ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE SOURCE-FACSIMILE-ONE (SU".
               03  FILLER                    PIC X(40) VALUE
               "B-X) TO FILLER-DATA.                    ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE FILLER-IMAGE TO OUTPUT-R".
               03  FILLER                    PIC X(40) VALUE
               "ECORD.                                  ".
               03  FILLER                    PIC X(40) VALUE
               "           WRITE OUTPUT-RECORD.         ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE SOURCE-FACSIMILE (20) TO".
               03  FILLER                    PIC X(40) VALUE
               " OUTPUT-RECORD.                         ".
               03  FILLER                    PIC X(40) VALUE
               "           WRITE OUTPUT-RECORD.         ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE SOURCE-FACSIMILE-TWO (SU".
               03  FILLER                    PIC X(40) VALUE
               "B-X) TO FILLER-DATA.                    ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE FILLER-IMAGE TO OUTPUT-R".
               03  FILLER                    PIC X(40) VALUE
               "ECORD.                                  ".
               03  FILLER                    PIC X(40) VALUE
               "           WRITE OUTPUT-RECORD.         ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           IF  SUB-X < 68               ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "               ADD 1 TO SUB-X           ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "               GO TO ML-3.              ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE 21 TO SUB-X.            ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       ML-4.                            ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           MOVE SOURCE-FACSIMILE (SUB-X)".
               03  FILLER                    PIC X(40) VALUE
               " TO OUTPUT-RECORD.                      ".
               03  FILLER                    PIC X(40) VALUE
               "           WRITE OUTPUT-RECORD.         ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           IF  SUB-X < 68               ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "               ADD 1 TO SUB-X           ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "               GO TO ML-4.              ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "       ML-99.                           ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           CLOSE OUTPUT-FILE.           ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
               03  FILLER                    PIC X(40) VALUE
               "           STOP RUN.                    ".
               03  FILLER                    PIC X(40) VALUE
               "                                        ".
           02  SOURCE-FACSIMILE-TABLE REDEFINES
                   SOURCE-FACSIMILE-DATA.
               03  SOURCE-FACSIMILE OCCURS 68.
                   04  SOURCE-FACSIMILE-ONE  PIC X(40).
                   04  SOURCE-FACSIMILE-TWO  PIC X(40).
       01  FILLER-IMAGE.
           02  FILLER                        PIC X(15) VALUE SPACES.
           02  FILLER                        PIC X     VALUE QUOTE.
           02  FILLER-DATA                   PIC X(40).
           02  FILLER                        PIC X     VALUE QUOTE.
           02  FILLER                        PIC X     VALUE FULL-STOP.
           02  FILLER                        PIC X(22) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-LINE SECTION.
       ML-1.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE 1 TO SUB-X.
       ML-2.
           MOVE SOURCE-FACSIMILE (SUB-X) TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           IF  SUB-X < 19
               ADD 1 TO SUB-X
               GO TO ML-2.
           MOVE 1 TO SUB-X.
       ML-3.
           MOVE SOURCE-FACSIMILE (20) TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           MOVE SOURCE-FACSIMILE-ONE (SUB-X) TO FILLER-DATA.
           MOVE FILLER-IMAGE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           MOVE SOURCE-FACSIMILE (20) TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           MOVE SOURCE-FACSIMILE-TWO (SUB-X) TO FILLER-DATA.
           MOVE FILLER-IMAGE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           IF  SUB-X < 68
               ADD 1 TO SUB-X
               GO TO ML-3.
           MOVE 21 TO SUB-X.
       ML-4.
           MOVE SOURCE-FACSIMILE (SUB-X) TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           IF  SUB-X < 68
               ADD 1 TO SUB-X
               GO TO ML-4.
       ML-99.
           CLOSE OUTPUT-FILE.
           STOP RUN.
```



```cobol
       ID DIVISION.
       PROGRAM-ID. QUINE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 X PIC S9(4) COMP.
       1 A. 2 B.
       3 PIC X(40) VALUE "       ID DIVISION.                     ".
       3 PIC X(40) VALUE "                                        ".
       3 PIC X(40) VALUE "       PROGRAM-ID. QUINE.               ".
       3 PIC X(40) VALUE "                                        ".
       3 PIC X(40) VALUE "       DATA DIVISION.                   ".
       3 PIC X(40) VALUE "                                        ".
       3 PIC X(40) VALUE "       WORKING-STORAGE SECTION.         ".
       3 PIC X(40) VALUE "                                        ".
       3 PIC X(40) VALUE "       1 X PIC S9(4) COMP.              ".
       3 PIC X(40) VALUE "                                        ".
       3 PIC X(40) VALUE "       1 A. 2 B.                        ".
       3 PIC X(40) VALUE "                                        ".
       3 PIC X(40) VALUE "       2 T REDEFINES B. 3 TE OCCURS 16. ".
       3 PIC X(40) VALUE "4 T1 PIC X(40). 4 T2 PIC X(40).         ".
       3 PIC X(40) VALUE "       1 F. 2 PIC X(25)           VALUE ".
       3 PIC X(40) VALUE "'       3 PIC X(40) VALUE '.            ".
       3 PIC X(40) VALUE "       2 PIC X VALUE QUOTE. 2 FF PIC X(4".
       3 PIC X(40) VALUE "0). 2 PIC X VALUE QUOTE.                ".
       3 PIC X(40) VALUE "       2 PIC X VALUE '.'.               ".
       3 PIC X(40) VALUE "                                        ".
       3 PIC X(40) VALUE "       PROCEDURE DIVISION.              ".
       3 PIC X(40) VALUE "                                        ".
       3 PIC X(40) VALUE "           PERFORM VARYING X FROM 1 BY 1".
       3 PIC X(40) VALUE " UNTIL X > 6 DISPLAY TE (X)             ".
       3 PIC X(40) VALUE "           END-PERFORM PERFORM VARYING X".
       3 PIC X(40) VALUE " FROM 1 BY 1 UNTIL X > 16               ".
       3 PIC X(40) VALUE "           MOVE T1 (X) TO FF DISPLAY F M".
       3 PIC X(40) VALUE "OVE T2 (X) TO FF DISPLAY F              ".
       3 PIC X(40) VALUE "           END-PERFORM PERFORM VARYING X".
       3 PIC X(40) VALUE " FROM 7 BY 1 UNTIL X > 16               ".
       3 PIC X(40) VALUE "           DISPLAY TE (X) END-PERFORM ST".
       3 PIC X(40) VALUE "OP RUN.                                 ".
       2 T REDEFINES B. 3 TE OCCURS 16. 4 T1 PIC X(40). 4 T2 PIC X(40).
       1 F. 2 PIC X(25)           VALUE '       3 PIC X(40) VALUE '.
       2 PIC X VALUE QUOTE. 2 FF PIC X(40). 2 PIC X VALUE QUOTE.
       2 PIC X VALUE '.'.
       PROCEDURE DIVISION.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 6 DISPLAY TE (X)
           END-PERFORM PERFORM VARYING X FROM 1 BY 1 UNTIL X > 16
           MOVE T1 (X) TO FF DISPLAY F MOVE T2 (X) TO FF DISPLAY F
           END-PERFORM PERFORM VARYING X FROM 7 BY 1 UNTIL X > 16
           DISPLAY TE (X) END-PERFORM STOP RUN.
```

An even smaller one could be found of the site of [http://www.tmdg.co.uk/programing/quine.cbl.php Tom Dawes-Gamble], but is reproduced below, as the original seems to have vanished:

```cobol

        Author. Tom Dawes-Gamble. (c) 2000
       01 src-lines pic x(768) value
       "        Author. Tom Dawes-Gamble. (c) 2000
      -"01 src-lines pic x(768) value
      -"01 sl redefines src-lines pic x(64) occurs 12 indexed by i.
      -"  Perform varying i from 1 by 1 until i > 2
      -"    Display '       ' sl(i).
      -"  Display '       ' quote sl(1).
      -"  Perform varying i from 2 by 1 until i > 11
      -"    Display '      ' '-' quote sl(i).
      -"  Display '      ' '-' quote '  Stop run.' quote '.'.
      -"  Perform varying i from 3 by 1 until i > 12
      -"    Display '       ' sl(i).
      -"  Stop run.".
       01 sl redefines src-lines pic x(64) occurs 12 indexed by i.
         Perform varying i from 1 by 1 until i > 2
           Display '       ' sl(i).
         Display '       ' quote sl(1).
         Perform varying i from 2 by 1 until i > 11
           Display '      ' '-' quote sl(i).
         Display '      ' '-' quote '  Stop run.' quote '.'.
         Perform varying i from 3 by 1 until i > 12
           Display '       ' sl(i).
         Stop run.

```



## CoffeeScript


```coffeescript
s="s=#&# ;alert s.replace(/&/,s).replace /#(?=[^&;'(]|';;$)/g, '#';;" ;alert s.replace(/&/,s).replace /#(?=[^&;'(]|';;$)/g, '"';;
```



## Commodore BASIC

A rather long and unwieldy (although nicely formatted) BASIC quine that doesn't use the <tt>10 LIST</tt> cheat.  Works on the PET, VIC-20, C-64, C-128, C-16 and Plus/4, CBM-II, and CBM-5x0 series, and probably several non-Commodore BASICs as well, especially those based on Microsoft BASIC.


```basic
10 DATA 49,54,48,32,78,61,51,51,48,13,49,55,48,32,68,73,77,32,65,40,78,41,13
20 DATA 49,56,48,32,70,79,82,32,73,61,48,32,84,79,32,78,13,49,57,48,32,58,32
30 DATA 82,69,65,68,32,65,40,73,41,13,50,48,48,32,78,69,88,84,32,73,13,50,49
40 DATA 48,32,70,79,82,32,73,61,48,32,84,79,32,49,52,32,13,50,50,48,32,58,32
50 DATA 80,82,73,78,84,32,77,73,68,36,40,83,84,82,36,40,40,73,43,49,41,42,49
60 DATA 48,41,44,50,41,59,34,32,68,65,84,65,32,34,59,77,73,68,36,40,83,84,82
70 DATA 36,40,65,40,73,42,50,51,41,41,44,50,41,59,13,50,51,48,32,58,32,70,79
80 DATA 82,32,74,61,49,32,84,79,32,50,50,13,50,52,48,32,58,32,32,32,75,61,73
90 DATA 42,50,51,43,74,13,50,53,48,32,58,32,32,32,73,70,32,75,32,60,61,32,78
100 DATA 32,84,72,69,78,32,80,82,73,78,84,32,34,44,34,59,77,73,68,36,40,83,84
110 DATA 82,36,40,65,40,75,41,41,44,50,41,59,13,50,54,48,32,58,32,78,69,88,84
120 DATA 32,74,13,50,55,48,32,58,32,80,82,73,78,84,13,50,56,48,32,78,69,88,84
130 DATA 32,73,13,50,57,48,32,70,79,82,32,73,61,48,32,84,79,32,78,13,51,48,48
140 DATA 32,58,32,80,82,73,78,84,32,67,72,82,36,40,65,40,73,41,41,59,13,51,49
150 DATA 48,32,78,69,88,84,32,73,13
160 N=330
170 DIM A(N)
180 FOR I=0 TO N
190 : READ A(I)
200 NEXT I
210 FOR I=0 TO 14 
220 : PRINT MID$(STR$((I+1)*10),2);" DATA ";MID$(STR$(A(I*23)),2);
230 : FOR J=1 TO 22
240 :   K=I*23+J
250 :   IF K <= N THEN PRINT ",";MID$(STR$(A(K)),2);
260 : NEXT J
270 : PRINT
280 NEXT I
290 FOR I=0 TO N
300 : PRINT CHR$(A(I));
310 NEXT I
```



## Common Lisp

There are many simple ways to write a quine in Common Lisp, since source can be quoted and manipulated; this one defines an anonymous function, which is applied to its own source, which writes out that application of itself to its source.  Note that Common Lisp by default does not distinguish cases.

```lisp
((lambda (s) (print (list s (list 'quote s))))
 '(lambda (s) (print (list s (list 'quote s)))))
```


This one does the same thing using quasiquote (template) syntax; in some implementations it may not print nicely (but will still work):

```lisp
((lambda (s) (print `(,s ',s))) '(lambda (s) (print `(,s ',s))))
```


or another version:

```lisp
((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
```


This program's source contains an explicit reference to itself, which it prints in a manner preserving that reference:

```lisp
#1=(write '#1# :circle t)
```


or another version (the former version cause looping in SBCL 1.0.56):

```lisp
#1=(progn (setq *print-circle* t) (write '#1#))
```


This is an example using format and explicit referencing:

```lisp
(format t #1='"(format t #1='~s #1#)" #1#)
```


An example using [[REPL]] variables:

```lisp
(write -)
```



## D

This Quine outputs its own source both during compiling and running.

```d
const auto s=`const auto q="const auto s=\x60"~s~"\x60;
mixin(s);";import std.stdio;void main(){writefln(q);pragma(msg,q);}`;
mixin(s);

```

''NB:'' last line should be CRLF to match <tt>pragma</tt>'s newline output behaviour.


## Dao

Dao's BNF-like meta-programming macro supports quoting expressions as strings,
which allow writing a quine as the following:

```dao
syntax{Q $EXP}as{io.writef('syntax{Q $EXP}as{%s}Q %s',\'$EXP\',\'$EXP\')}Q io.writef('syntax{Q $EXP}as{%s}Q %s',\'$EXP\',\'$EXP\')
```



## dc


```dc
[91PP93P[dx]P]dx
```

=={{header|Déjà Vu}}==

```dejavu
"!print !. dup"
!print !. dup
```



## E


```e
" =~ x; println(E.toQuote(x),x)" =~ x; println(E.toQuote(x),x)
```


## Elixir


```elixir
a = <<"a = ~p~n:io.fwrite(a,[a])~n">>
:io.fwrite(a,[a])
```



## Erlang

<div style="width:100%;overflow:scroll">

```Erlang

-module(quine).
-export([do/0]).

do() -> Txt=txt(), io:format("~s~ntxt() ->~n~w.~n",[Txt,Txt]), halt().
txt() ->
[45,109,111,100,117,108,101,40,113,117,105,110,101,41,46,10,45,101,120,112,111,114,116,40,91,100,111,47,48,93,41,46,10,10,100,111,40,41,32,45,62,32,84,120,116,61,116,120,116,40,41,44,32,105,111,58,102,111,114,109,97,116,40,34,126,115,126,110,116,120,116,40,41,32,45,62,126,110,126,119,46,126,110,34,44,91,84,120,116,44,84,120,116,93,41,44,32,104,97,108,116,40,41,46].


```

</div>


## ERRE


```ERRE
PROGRAM QUINE
BEGIN
READ(D$,Y$)
LOOP
    READ(X$)
    EXIT IF LEN(X$)<1
    PRINT(X$)
END LOOP
RESTORE
LOOP
    READ(X$)
    EXIT IF LEN(X$)<1
    PRINT(D$;CHR$(34);X$;CHR$(34);CHR$(41))
END LOOP
PRINT(D$;CHR$(34);CHR$(34);CHR$(41))
PRINT(Y$)
DATA("DATA(")
DATA("END PROGRAM")
DATA("PROGRAM QUINE")
DATA("BEGIN")
DATA("READ(D$,Y$)")
DATA("LOOP")
DATA("    READ(X$)")
DATA("    EXIT IF LEN(X$)<1")
DATA("    PRINT(X$)")
DATA("END LOOP")
DATA("RESTORE")
DATA("LOOP")
DATA("    READ(X$)")
DATA("    EXIT IF LEN(X$)<1")
DATA("    PRINT(D$;CHR$(34);X$;CHR$(34);CHR$(41))")
DATA("END LOOP")
DATA("PRINT(D$;CHR$(34);CHR$(34);CHR$(41))")
DATA("PRINT(Y$)")
DATA("")
END PROGRAM
```



## Euphoria

{{trans|C}}

```euphoria
constant p="constant p=%s%s%s printf(1,p,{34,p,34})" printf(1,p,{34,p,34})
```


=={{header|F Sharp|F#}}==
Using .NET string formatting

```fsharp
let s = "let s = {0}{1}{0} in System.Console.WriteLine(s, char 34, s);;" in System.Console.WriteLine(s, char 34, s);;
```

Using Printf library

```fsharp
(fun s c -> printf s c s.Value c) "(fun s c -> printf s c s.Value c) %c%s%c <| char 34;;" <| char 34;;
```



## Factor

With printf:

```factor
"%s [ 34 1string dup surround ] keep printf" [ 34 1string dup surround ] keep printf
```

With prettyprinter:

```factor
"[ pprint ] [ write ] bi"[ pprint ] [ write ] bi
```



## Fish

All the zeros and +'s on the first line are just for aesthetic purposes. Only two zeros are needed, although the v at the end must line up.

```fish
 00000000000000000000++++++++++++++++++ v
2[$:{:@]$g:0=?v    >o:4a*=?!v~1+:3=?;0ao>
              >~" "^        >1+         ^
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Quine this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

A large number of quine methods are listed [http://www.complang.tuwien.ac.at/forth/quines.html here], the simplest of which is:

```forth>SOURCE TYPE</lang



## Fortran


### F90+

Fortran allows the start of a text literal to be marked by either an apostrophe or a double quote and the end by the corresponding character. Doubling is used to signify that the delimiter appears within a literal.

```fortran
character*46::s='("character*46::s=",3a,";print s,39,s,39;end")';print s,39,s,39;end
```

{{out}}

```txt

character*46::s='("character*46::s=",3a,";print s,39,s,39;end")';print s,39,s,39;end

```

In ASCII, character 39 is an apostrophe. When run, the last "print" is preceded by three spaces after the semicolon that do not appear in the source. This wouldn't worry a Fortran compiler, because spaces are ignored outside text literals. More troublesome is that the first letter of the output appears in column one, and, being the letter "c", that marks a comment. This is why the text lighlighting has rendered the source text in italic. Later Fortran compilers may disregard this comment formalism in favour of the now standard in-line comment starter of an exclamation mark, and for them, the source would not be a comment.

### Older Fortran

This reverts to fixed-format source form, relying on the format code / to signify a new line so that ASCII control characters need not be represented. Code T''n'' means "tab" to column ''n'' - no ASCII tab characters are involved. And difficulty with quoted strings is avoided by using the Hollerith form, ''n''H, signifying that ''n'' characters follow the H as a text literal. 
```Fortran
      WRITE(6,100)                                    
      STOP                                            
  100 FORMAT(6X,12HWRITE(6,100)/6X,4HSTOP/            
     .42H  100 FORMAT(6X,12HWRITE(6,100)/6X,4HSTOP/          ,2(/5X,67H.
     .42H  100 FORMAT(6X,12HWRITE(6,100)/6X,4HSTOP/          ,2(/5X,67H.
     .)/T48,2H)/T1,5X2(21H.)/T48,2H)/T1,5X2(21H)/
     .T62,10H)/6X3HEND)T1,5X2(28H.T62,10H)/6X3HEND)T1,5X2(28H)/6X3HEND)
      END
```

And indeed, exactly that opaque gibberish is written, with leading spaces to column seven and full stops in column six to mark a continuation. This was checked by UltraCompare, not just by eye!


### Nostalgia note
 
I remember when this challenge came up, way back in 1966 --- FORTRAN was the game in town, and there existed a feature (well, really, a bug) that allowed an IBM FORTRAN program under IBM's OS/PCP/MFT/MVT [no HASP] to '''REWIND''' the default input stream (which were punched cards, and ''REWIND'' of course, was only intended for reel-to-reel tapes), which essentially then allowed the program to read it's own source, and then it was a simple matter to PRINT it.

The current CONTROL DATA FORTRAN had a feature that allowed the FORTRAN programmer to WRITE and then re-read I/O (the feature could've been called the REREAD statement, the WRITE may have been to device 0). If one didn't WRITE, just REREAD, the file stream then pointed to the source just read, thus allowing access to the FORTRAN program's source.

This challenge was extended to other languages, the concept was to print the actual source of the program, not use a recursive (or some derivative) method. There were some very clever (and obtuse and/or obscure) methods presented. A lot of operating systems, being in their infancy, had loopholes that allowed programmers to access files in their own job stream.

Another challenge at that time was to write a multi-language program (with NO changes) that would compile (or assemble) in various languages (without errors, of course) and produce the identical output. 
There were some very creative uses of "comments". --- Gerard Schildberger.


## Free Pascal


```freepascal
const s=';begin writeln(#99#111#110#115#116#32#115#61#39,s,#39,s);readln;end.';begin writeln(#99#111#110#115#116#32#115#61#39,s,#39,s);readln;end.
```



## FreeBASIC

===Código 1:===

```freebasic

#Define P(X) Print X: Print "P(" & #X & ")"
P("#Define P(X) Print X: Print ""P("" & #X & "")""")

```


===Código 2:===

```freebasic

Dim As String s = "Dim As String s = : Print Left(s, 18) + Chr(34) + s + Chr(34) + Mid(s, 18)" : Print Left(s, 18) + Chr(34) + s + Chr(34) + Mid(s, 18)

```



## Frink

This is not a particularly clever nor concise quine, but it is indeed a quine.

```frink
d="633d636861725b33345d3b653d643b7072696e745b22643d2463246424635c6e222b28653d7e25732f285b612d7a302d395d7b327d292f636861725b7061727365496e745b24312c31365d5d2f6567295d"
c=char[34];e=d;print["d=$c$d$c\n"+(e=~%s/([a-z0-9]{2})/char[parseInt[$1,16]]/eg)]
```

A more concise quine is:

```frink>1</lang



## GAP


```gap
f:=function (  )
    Print( "f:=", f, ";;\nf();\n" );
    return;
end;;
f();

```



## Gema


```gema
*=$1@quote{$1}\}\n@abort;@{\*\=\$1\@quote\{\$1\}\\\}\\n\@abort\;\@\{}
```



## Go


```go
package main

import "fmt"

func main() {
	a := "package main\n\nimport \"fmt\"\n\nfunc main() {\n\ta := %q\n\tfmt.Printf(a, a)\n}\n"
	fmt.Printf(a, a)
}
```



## Groovy

There are several ways to do this. Here are five:

```groovy
s="s=%s;printf s,s.inspect()";printf s,s.inspect()
```


```groovy
evaluate s='char q=39;print"evaluate s=$q$s$q"'
```


```groovy
s="s=%c%s%c;printf s,34,s,34";printf s,34,s,34
```


```groovy
s='s=%c%s%1$c;printf s,39,s';printf s,39,s
```


```groovy
printf _='printf _=%c%s%1$c,39,_',39,_
```

Also Groovy has a trivial solution of an empty (0 length) file even though that isn't an allowable solution here.

=={{header|GW-BASIC}}==

```qbasic>10 LIST</lang



## Haskell

(Haskell does not provide access to a source-code equivalent representation of the code at runtime.)

In Haskell, function arguments are not surrounded by parentheses, which permits a simple quine where there is only the unquoted code followed by the quoted code.

```haskell
let q s = putStrLn (s ++ show s) in q "let q s = putStrLn (s ++ show s) in q "
```


It is also possible to eliminate one or both of the variables (point-free style): the <code>let</code> can be replaced with a lambda. (<code>.</code> is function composition.)

```haskell
(\s -> putStrLn (s ++ show s)) "(\\s -> putStrLn (s ++ show s)) "
```


```haskell
(putStrLn . \s -> s ++ show s) "(putStrLn . \\s -> s ++ show s) "
```


and <var><code>s</code></var> can be replaced by <code>ap</code>, which when applied to functions has the effect of the [[wp:SKI combinator calculus|S combinator]]:

```haskell
import Control.Monad.Reader
(putStrLn . ap (++) show) "(putStrLn . ap (++) show) "
```


Since Haskell is a purely functional language, it is better at evaluating to things than printing them out. The following expression evaluates to a string of itself:

```haksell
ap(++)show"ap(++)show"
```


Finally, here is a standalone program (the preceding examples only work in the REPL):

```haskell
main = putStrLn $ (++) <*> show $ "main = putStrLn $ (++) <*> show $ "
```



## Hoon

Unfortunately, this is quite a long quine due to the pretty-printing that is employeed. A majority of the code is fixing up the printed result of the array so it can be ran directly.

```Hoon
!:  :-  %say  |=  [^ [~ ~]]  =+  ^=  s  ((list ,@tas) ~['!:  :-  %say  |=  [^ [~ ~]]  =+  ^=  s  ((list ,@tas) ~[' 'x' '])  :-  %noun  (,tape (turn s |=(a=@tas ?:(=(a %x) (crip `(list ,@tas)`(turn s |=(b=@tas =+([s=?:(=(b %x) " " "") m=(trip ~~~27.)] (crip :(welp s m (trip b) m s)))))) a))))'])  :-  %noun  (,tape (turn s |=(a=@tas ?:(=(a %x) (crip `(list ,@tas)`(turn s |=(b=@tas =+([s=?:(=(b %x) " " "") m=(trip ~~~27.)] (crip :(welp s m (trip b) m s)))))) a))))
```


Two much shorter quines were created and tweeted from the @urbit_ twitter account, which might be more interesting:

```Hoon
=-(`@t`(rap 3 - (scot %uw -) ")") 0wwai8F.8y0Fb.i1Tti.kwt6Z.zsOww.bi0P8.71xsy.xwt41.wa2QZ)
```
[https://twitter.com/urbit_/status/651127726670147584 1]

```Hoon
=<(`_""`~[. `@`39 . `@`10.535] '=<(`_""`~[. `@`39 . `@`10.535] ')
```
[https://twitter.com/urbit_/status/651126581499314176 2]


## HQ9+

Any program with a single “<tt>Q</tt>” is a quine. The simplest possible such program is just this:

```hq9plus>Q</lang


=={{header|HTML}} + CSS==
{{works with|Opera|10.0}} <!-- At the very least, guaranteed to work with these -->
{{works with|Firefox|3.5}}

This solution uses CSS to print out the source itself, e.g. the "direct accessing" method. Doesn't work in Internet Explorer; try it in one of Opera, Firefox, Safari, Chromium etc.

```css
<!DOCTYPE html>
<html>
<head>
	<title>HTML/CSS Quine</title>
	<style type="text/css">
	* { font: 10pt monospace; }

	head, style { display: block; }
	style { white-space: pre; }

	style:before {
		content:
			"\3C""!DOCTYPE html\3E"
			"\A\3Chtml\3E\A"
			"\3Chead\3E\A"
			"\9\3Ctitle\3E""HTML/CSS Quine""\3C/title\3E\A"
			"\9\3Cstyle type=\22text/css\22\3E";
	}
	style:after {
		content:
			"\3C/style\3E\A"
			"\3C/head\3E\A"
			"\3C""body\3E\3C/body\3E\A"
			"\3C/html\3E";
	}
	</style>
</head>
<body></body>
</html>
```



## Huginn


```huginn
#! /bin/sh
exec huginn --no-argv -E "${0}"
#! huginn

main() {
	c = "#! /bin/sh{1}~"
		"exec huginn --no-argv -E {3}${{0}}{3}{1}#! huginn{1}{1}~"
		"main() {{{1}{2}c = {3}{0}{3};{1}{2}print({1}~"
		"{2}{2}copy( c ).replace( {3}{5}{3}, {3}{3} )~"
		".format({1}{2}{2}{2}c.replace( {3}{5}{3}, ~"
		"{3}{5}{4}{3}{4}n{4}t{4}t{4}{3}{3} ), ~"
		"{3}{4}n{3}, {3}{4}t{3}, {3}{4}{3}{3}, {3}{4}{4}{3}, ~"
		"{3}{5}{3}{1}{2}{2}){1}{2});{1}}}{1}{1}";
	print(
		copy( c ).replace( "~", "" ).format(
			c.replace( "~", "~\"\n\t\t\"" ), "\n", "\t", "\"", "\\", "~"
		)
	);
}
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main();x:="write(\"procedure main();x:=\",image(x));write(x);end"
write("procedure main();x:=",image(x));write(x);end
```



## Io


```io>thisMessage print</lang



## Inform 7


```inform7
R is a room. To quit: (- quit; -). When play begins: say entry 1 in Q; say Q in brace notation; quit. Q is a list of text variable. Q is {"R is a room. To quit: (- quit; -). When play begins: say entry 1 in Q; say Q in brace notation; quit. Q is a list of text variable. Q is "}
```



## J

Technically, the empty program in J is a quine, as it has an empty result. For example:


```J
   

```


Also, many many numbers in J are quines. For example (result not shown, here, for clarity):

```J>0 </lang


Note: this implementation assumes that J is being used interactively.  If J is being used in a command line script, having the script read and output itself would be a better approach. And it's not clear whether quines are a relevant concept in a none textual context (such as a windowing environment).

And other [[j:Puzzles/Quine#Solutions|solutions]] are also possible.


## Java

Copied from [http://www.nyx.net/~gthompso/copyrights.htm The Quine Page]

Author: Bertram Felgenhauer
<div style="width:100%;overflow:scroll">

```java
class S{public static void main(String[]a){String s="class S{public static void main(String[]a){String s=;char c=34;System.out.println(s.substring(0,52)+c+s+c+s.substring(52));}}";char c=34;System.out.println(s.substring(0,52)+c+s+c+s.substring(52));}}
```

</div>

{{works with|Java|1.5+}}
<div style="width:100%;overflow:scroll">

```java
class S{public static void main(String[]a){String p="class S{public static void main(String[]a){String p=%c%s%1$c;System.out.printf(p,34,p);}}";System.out.printf(p,34,p);}}
```

</div>


## JavaScript

{{works with|SpiderMonkey}} 1.7.0

```javascript
(function(){print("("+arguments.callee.toString().replace(/\s/g,'')+")()");})()
```



###  Using eval 

{{works with|SpiderMonkey}} 1.7.0
This version doesn't use arguments.callee.toString() to return the string representation of itself. Instead, it relies on eval().

```javascript
var code='var q=String.fromCharCode(39);print("var code="+q+code+q+";eval(code)")';eval(code)
```



###  Replacing String 


```JavaScript
(function(){str=["(function(){str=[F].join(String.fromCharCode(34));str=str.replace(/F/,String.fromCharCode(34)+str+String.fromCharCode(34));console.log(str)})()"].join(String.fromCharCode(34));str=str.replace(/F/,String.fromCharCode(34)+str+String.fromCharCode(34));console.log(str)})()
```



### Code As Data


```javascript
q=`%3Bconsole.log(%60q%3D%5C%60%24%7Bq%7D%5C%60%60%2BdecodeURIComponent(q))`;console.log(`q=\`${q}\``+decodeURIComponent(q))
```



### Another Method


```javascript
var a=function () {var b="var a="+a.toString()+"\;a()";alert(b)};a()
```



### A simple module which simply evaluates to itself



```javascript
(function f() {
 
    return '(' + f.toString() + ')();';
    
})();
```


{{Out}}


```javascript
(function f() {

    return '(' + f.toString() + ')();';
    
})();
```



### Or logs itself to the console


```javascript
(function f() {

    console.log('(' + f.toString() + ')();');

})();
```


{{Out}}

```txt
(function f() {

    console.log('(' + f.toString() + ')();');

})();
```



## Joy


```Joy
"dup put putchars 10 putch." dup put putchars 10 putch.
```



## Jsish

Based on Javascript eval solution.

```javascript
var code='var q=String.fromCharCode(39);puts("var code="+q+code+q+";eval(code)")';eval(code)
```

{{out}}
Double test, run quine.jsi output through the jsish argument evaluator:

```txt
prompt$ jsish -e "$(jsish quine.jsi)"
var code='var q=String.fromCharCode(39);puts("var code="+q+code+q+";eval(code)")';eval(code)
```



## Julia


```Julia

x="println(\"x=\$(repr(x))\\n\$x\")"
println("x=$(repr(x))\n$x")

```


In Julia, <tt>$x</tt> in a string literal interpolates the value of the variable into the string.
<tt>$(expression)</tt> evaluates the expression and interpolates the result into the string.
Normally, the string value <tt>"hi\tworld"</tt> would be inserted without quotation marks and with a literal tab.
The <tt>repr</tt> function returns a string value that contains quotation marks and in which the literal tab is replaced by the characters <tt>\t</tt>.
When the result of the <tt>repr</tt> function is interpolated, the result is what you would type into your code to create that string literal.


## Kotlin

The following is based on the classic C quine but makes use of Kotlin's 'raw' (i.e. triple quoted) string literals.

```scala
// version 1.1.2

const val F = """// version 1.1.2

const val F = %c%c%c%s%c%c%c

fun main(args: Array<String>) {
    System.out.printf(F, 34, 34, 34, F, 34, 34, 34)
}
"""

fun main(args: Array<String>) {
    System.out.printf(F, 34, 34, 34, F, 34, 34, 34)
}
```



## Lasso


In template mode, any text that doesn't include Lasso tags is a trivial quine, because it is passed through unmodified by the interpreter.  But here is a more traditional quine:


```lasso
var(a=(:10,39,118,97,114,40,97,61,40,58,39,10,36,97,45,62,106,111,105,110,40,39,44,39,41,10,39,41,41,39,10,118,97,114,40,98,61,98,121,116,101,115,41,10,36,97,45,62,102,111,114,101,97,99,104,32,61,62,32,123,32,36,98,45,62,105,109,112,111,114,116,56,98,105,116,115,40,35,49,41,32,125,10,36,98,45,62,97,115,83,116,114,105,110,103))
'var(a=(:'
$a->join(',')
'))'
var(b=bytes)
$a->foreach => { $b->import8bits(#1) }
$b->asString
```



## LDPL


```ldpl

DATA:
  A IS NUMBER VECTOR
  C IS TEXT
  N IS NUMBER
  I IS NUMBER
  J IS NUMBER
PROCEDURE:

  SUB-PROCEDURE SHOWU
    STORE 0 IN I
    WHILE I IS LESS THAN N DO
        DISPLAY "  STORE "
        DISPLAY A:I
        DISPLAY " IN "
        DISPLAY " A:"
        DISPLAY I CRLF
        ADD I AND 1 IN I
    REPEAT
    DISPLAY "  STORE "
    DISPLAY N
    DISPLAY " IN N" CRLF
  END SUB-PROCEDURE

  SUB-PROCEDURE SHOW
    STORE 0 IN J
    WHILE J IS LESS THAN N DO
        IF A:J IS EQUAL TO 42 THEN
          CALL SUB-PROCEDURE SHOWU
        ELSE
          STORE CHARACTER A:J IN C
          DISPLAY C
        END-IF
        ADD J AND 1 IN J
    REPEAT
  END SUB-PROCEDURE

  STORE 10 IN  A:0
  STORE 68 IN  A:1
  STORE 65 IN  A:2
  STORE 84 IN  A:3
  STORE 65 IN  A:4
  STORE 58 IN  A:5
  STORE 10 IN  A:6
  STORE 32 IN  A:7
  STORE 32 IN  A:8
  STORE 65 IN  A:9
  STORE 32 IN  A:10
  STORE 73 IN  A:11
  STORE 83 IN  A:12
  STORE 32 IN  A:13
  STORE 78 IN  A:14
  STORE 85 IN  A:15
  STORE 77 IN  A:16
  STORE 66 IN  A:17
  STORE 69 IN  A:18
  STORE 82 IN  A:19
  STORE 32 IN  A:20
  STORE 86 IN  A:21
  STORE 69 IN  A:22
  STORE 67 IN  A:23
  STORE 84 IN  A:24
  STORE 79 IN  A:25
  STORE 82 IN  A:26
  STORE 10 IN  A:27
  STORE 32 IN  A:28
  STORE 32 IN  A:29
  STORE 67 IN  A:30
  STORE 32 IN  A:31
  STORE 73 IN  A:32
  STORE 83 IN  A:33
  STORE 32 IN  A:34
  STORE 84 IN  A:35
  STORE 69 IN  A:36
  STORE 88 IN  A:37
  STORE 84 IN  A:38
  STORE 10 IN  A:39
  STORE 32 IN  A:40
  STORE 32 IN  A:41
  STORE 78 IN  A:42
  STORE 32 IN  A:43
  STORE 73 IN  A:44
  STORE 83 IN  A:45
  STORE 32 IN  A:46
  STORE 78 IN  A:47
  STORE 85 IN  A:48
  STORE 77 IN  A:49
  STORE 66 IN  A:50
  STORE 69 IN  A:51
  STORE 82 IN  A:52
  STORE 10 IN  A:53
  STORE 32 IN  A:54
  STORE 32 IN  A:55
  STORE 73 IN  A:56
  STORE 32 IN  A:57
  STORE 73 IN  A:58
  STORE 83 IN  A:59
  STORE 32 IN  A:60
  STORE 78 IN  A:61
  STORE 85 IN  A:62
  STORE 77 IN  A:63
  STORE 66 IN  A:64
  STORE 69 IN  A:65
  STORE 82 IN  A:66
  STORE 10 IN  A:67
  STORE 32 IN  A:68
  STORE 32 IN  A:69
  STORE 74 IN  A:70
  STORE 32 IN  A:71
  STORE 73 IN  A:72
  STORE 83 IN  A:73
  STORE 32 IN  A:74
  STORE 78 IN  A:75
  STORE 85 IN  A:76
  STORE 77 IN  A:77
  STORE 66 IN  A:78
  STORE 69 IN  A:79
  STORE 82 IN  A:80
  STORE 10 IN  A:81
  STORE 80 IN  A:82
  STORE 82 IN  A:83
  STORE 79 IN  A:84
  STORE 67 IN  A:85
  STORE 69 IN  A:86
  STORE 68 IN  A:87
  STORE 85 IN  A:88
  STORE 82 IN  A:89
  STORE 69 IN  A:90
  STORE 58 IN  A:91
  STORE 10 IN  A:92
  STORE 10 IN  A:93
  STORE 32 IN  A:94
  STORE 32 IN  A:95
  STORE 83 IN  A:96
  STORE 85 IN  A:97
  STORE 66 IN  A:98
  STORE 45 IN  A:99
  STORE 80 IN  A:100
  STORE 82 IN  A:101
  STORE 79 IN  A:102
  STORE 67 IN  A:103
  STORE 69 IN  A:104
  STORE 68 IN  A:105
  STORE 85 IN  A:106
  STORE 82 IN  A:107
  STORE 69 IN  A:108
  STORE 32 IN  A:109
  STORE 83 IN  A:110
  STORE 72 IN  A:111
  STORE 79 IN  A:112
  STORE 87 IN  A:113
  STORE 85 IN  A:114
  STORE 10 IN  A:115
  STORE 32 IN  A:116
  STORE 32 IN  A:117
  STORE 32 IN  A:118
  STORE 32 IN  A:119
  STORE 83 IN  A:120
  STORE 84 IN  A:121
  STORE 79 IN  A:122
  STORE 82 IN  A:123
  STORE 69 IN  A:124
  STORE 32 IN  A:125
  STORE 48 IN  A:126
  STORE 32 IN  A:127
  STORE 73 IN  A:128
  STORE 78 IN  A:129
  STORE 32 IN  A:130
  STORE 73 IN  A:131
  STORE 10 IN  A:132
  STORE 32 IN  A:133
  STORE 32 IN  A:134
  STORE 32 IN  A:135
  STORE 32 IN  A:136
  STORE 87 IN  A:137
  STORE 72 IN  A:138
  STORE 73 IN  A:139
  STORE 76 IN  A:140
  STORE 69 IN  A:141
  STORE 32 IN  A:142
  STORE 73 IN  A:143
  STORE 32 IN  A:144
  STORE 73 IN  A:145
  STORE 83 IN  A:146
  STORE 32 IN  A:147
  STORE 76 IN  A:148
  STORE 69 IN  A:149
  STORE 83 IN  A:150
  STORE 83 IN  A:151
  STORE 32 IN  A:152
  STORE 84 IN  A:153
  STORE 72 IN  A:154
  STORE 65 IN  A:155
  STORE 78 IN  A:156
  STORE 32 IN  A:157
  STORE 78 IN  A:158
  STORE 32 IN  A:159
  STORE 68 IN  A:160
  STORE 79 IN  A:161
  STORE 10 IN  A:162
  STORE 32 IN  A:163
  STORE 32 IN  A:164
  STORE 32 IN  A:165
  STORE 32 IN  A:166
  STORE 32 IN  A:167
  STORE 32 IN  A:168
  STORE 32 IN  A:169
  STORE 32 IN  A:170
  STORE 68 IN  A:171
  STORE 73 IN  A:172
  STORE 83 IN  A:173
  STORE 80 IN  A:174
  STORE 76 IN  A:175
  STORE 65 IN  A:176
  STORE 89 IN  A:177
  STORE 32 IN  A:178
  STORE 34 IN  A:179
  STORE 32 IN  A:180
  STORE 32 IN  A:181
  STORE 83 IN  A:182
  STORE 84 IN  A:183
  STORE 79 IN  A:184
  STORE 82 IN  A:185
  STORE 69 IN  A:186
  STORE 32 IN  A:187
  STORE 34 IN  A:188
  STORE 10 IN  A:189
  STORE 32 IN  A:190
  STORE 32 IN  A:191
  STORE 32 IN  A:192
  STORE 32 IN  A:193
  STORE 32 IN  A:194
  STORE 32 IN  A:195
  STORE 32 IN  A:196
  STORE 32 IN  A:197
  STORE 68 IN  A:198
  STORE 73 IN  A:199
  STORE 83 IN  A:200
  STORE 80 IN  A:201
  STORE 76 IN  A:202
  STORE 65 IN  A:203
  STORE 89 IN  A:204
  STORE 32 IN  A:205
  STORE 65 IN  A:206
  STORE 58 IN  A:207
  STORE 73 IN  A:208
  STORE 10 IN  A:209
  STORE 32 IN  A:210
  STORE 32 IN  A:211
  STORE 32 IN  A:212
  STORE 32 IN  A:213
  STORE 32 IN  A:214
  STORE 32 IN  A:215
  STORE 32 IN  A:216
  STORE 32 IN  A:217
  STORE 68 IN  A:218
  STORE 73 IN  A:219
  STORE 83 IN  A:220
  STORE 80 IN  A:221
  STORE 76 IN  A:222
  STORE 65 IN  A:223
  STORE 89 IN  A:224
  STORE 32 IN  A:225
  STORE 34 IN  A:226
  STORE 32 IN  A:227
  STORE 73 IN  A:228
  STORE 78 IN  A:229
  STORE 32 IN  A:230
  STORE 34 IN  A:231
  STORE 10 IN  A:232
  STORE 32 IN  A:233
  STORE 32 IN  A:234
  STORE 32 IN  A:235
  STORE 32 IN  A:236
  STORE 32 IN  A:237
  STORE 32 IN  A:238
  STORE 32 IN  A:239
  STORE 32 IN  A:240
  STORE 68 IN  A:241
  STORE 73 IN  A:242
  STORE 83 IN  A:243
  STORE 80 IN  A:244
  STORE 76 IN  A:245
  STORE 65 IN  A:246
  STORE 89 IN  A:247
  STORE 32 IN  A:248
  STORE 34 IN  A:249
  STORE 32 IN  A:250
  STORE 65 IN  A:251
  STORE 58 IN  A:252
  STORE 34 IN  A:253
  STORE 10 IN  A:254
  STORE 32 IN  A:255
  STORE 32 IN  A:256
  STORE 32 IN  A:257
  STORE 32 IN  A:258
  STORE 32 IN  A:259
  STORE 32 IN  A:260
  STORE 32 IN  A:261
  STORE 32 IN  A:262
  STORE 68 IN  A:263
  STORE 73 IN  A:264
  STORE 83 IN  A:265
  STORE 80 IN  A:266
  STORE 76 IN  A:267
  STORE 65 IN  A:268
  STORE 89 IN  A:269
  STORE 32 IN  A:270
  STORE 73 IN  A:271
  STORE 32 IN  A:272
  STORE 67 IN  A:273
  STORE 82 IN  A:274
  STORE 76 IN  A:275
  STORE 70 IN  A:276
  STORE 10 IN  A:277
  STORE 32 IN  A:278
  STORE 32 IN  A:279
  STORE 32 IN  A:280
  STORE 32 IN  A:281
  STORE 32 IN  A:282
  STORE 32 IN  A:283
  STORE 32 IN  A:284
  STORE 32 IN  A:285
  STORE 65 IN  A:286
  STORE 68 IN  A:287
  STORE 68 IN  A:288
  STORE 32 IN  A:289
  STORE 73 IN  A:290
  STORE 32 IN  A:291
  STORE 65 IN  A:292
  STORE 78 IN  A:293
  STORE 68 IN  A:294
  STORE 32 IN  A:295
  STORE 49 IN  A:296
  STORE 32 IN  A:297
  STORE 73 IN  A:298
  STORE 78 IN  A:299
  STORE 32 IN  A:300
  STORE 73 IN  A:301
  STORE 10 IN  A:302
  STORE 32 IN  A:303
  STORE 32 IN  A:304
  STORE 32 IN  A:305
  STORE 32 IN  A:306
  STORE 82 IN  A:307
  STORE 69 IN  A:308
  STORE 80 IN  A:309
  STORE 69 IN  A:310
  STORE 65 IN  A:311
  STORE 84 IN  A:312
  STORE 10 IN  A:313
  STORE 32 IN  A:314
  STORE 32 IN  A:315
  STORE 32 IN  A:316
  STORE 32 IN  A:317
  STORE 68 IN  A:318
  STORE 73 IN  A:319
  STORE 83 IN  A:320
  STORE 80 IN  A:321
  STORE 76 IN  A:322
  STORE 65 IN  A:323
  STORE 89 IN  A:324
  STORE 32 IN  A:325
  STORE 34 IN  A:326
  STORE 32 IN  A:327
  STORE 32 IN  A:328
  STORE 83 IN  A:329
  STORE 84 IN  A:330
  STORE 79 IN  A:331
  STORE 82 IN  A:332
  STORE 69 IN  A:333
  STORE 32 IN  A:334
  STORE 34 IN  A:335
  STORE 10 IN  A:336
  STORE 32 IN  A:337
  STORE 32 IN  A:338
  STORE 32 IN  A:339
  STORE 32 IN  A:340
  STORE 68 IN  A:341
  STORE 73 IN  A:342
  STORE 83 IN  A:343
  STORE 80 IN  A:344
  STORE 76 IN  A:345
  STORE 65 IN  A:346
  STORE 89 IN  A:347
  STORE 32 IN  A:348
  STORE 78 IN  A:349
  STORE 10 IN  A:350
  STORE 32 IN  A:351
  STORE 32 IN  A:352
  STORE 32 IN  A:353
  STORE 32 IN  A:354
  STORE 68 IN  A:355
  STORE 73 IN  A:356
  STORE 83 IN  A:357
  STORE 80 IN  A:358
  STORE 76 IN  A:359
  STORE 65 IN  A:360
  STORE 89 IN  A:361
  STORE 32 IN  A:362
  STORE 34 IN  A:363
  STORE 32 IN  A:364
  STORE 73 IN  A:365
  STORE 78 IN  A:366
  STORE 32 IN  A:367
  STORE 78 IN  A:368
  STORE 34 IN  A:369
  STORE 32 IN  A:370
  STORE 67 IN  A:371
  STORE 82 IN  A:372
  STORE 76 IN  A:373
  STORE 70 IN  A:374
  STORE 10 IN  A:375
  STORE 32 IN  A:376
  STORE 32 IN  A:377
  STORE 69 IN  A:378
  STORE 78 IN  A:379
  STORE 68 IN  A:380
  STORE 32 IN  A:381
  STORE 83 IN  A:382
  STORE 85 IN  A:383
  STORE 66 IN  A:384
  STORE 45 IN  A:385
  STORE 80 IN  A:386
  STORE 82 IN  A:387
  STORE 79 IN  A:388
  STORE 67 IN  A:389
  STORE 69 IN  A:390
  STORE 68 IN  A:391
  STORE 85 IN  A:392
  STORE 82 IN  A:393
  STORE 69 IN  A:394
  STORE 10 IN  A:395
  STORE 10 IN  A:396
  STORE 32 IN  A:397
  STORE 32 IN  A:398
  STORE 83 IN  A:399
  STORE 85 IN  A:400
  STORE 66 IN  A:401
  STORE 45 IN  A:402
  STORE 80 IN  A:403
  STORE 82 IN  A:404
  STORE 79 IN  A:405
  STORE 67 IN  A:406
  STORE 69 IN  A:407
  STORE 68 IN  A:408
  STORE 85 IN  A:409
  STORE 82 IN  A:410
  STORE 69 IN  A:411
  STORE 32 IN  A:412
  STORE 83 IN  A:413
  STORE 72 IN  A:414
  STORE 79 IN  A:415
  STORE 87 IN  A:416
  STORE 10 IN  A:417
  STORE 32 IN  A:418
  STORE 32 IN  A:419
  STORE 32 IN  A:420
  STORE 32 IN  A:421
  STORE 83 IN  A:422
  STORE 84 IN  A:423
  STORE 79 IN  A:424
  STORE 82 IN  A:425
  STORE 69 IN  A:426
  STORE 32 IN  A:427
  STORE 48 IN  A:428
  STORE 32 IN  A:429
  STORE 73 IN  A:430
  STORE 78 IN  A:431
  STORE 32 IN  A:432
  STORE 74 IN  A:433
  STORE 10 IN  A:434
  STORE 32 IN  A:435
  STORE 32 IN  A:436
  STORE 32 IN  A:437
  STORE 32 IN  A:438
  STORE 87 IN  A:439
  STORE 72 IN  A:440
  STORE 73 IN  A:441
  STORE 76 IN  A:442
  STORE 69 IN  A:443
  STORE 32 IN  A:444
  STORE 74 IN  A:445
  STORE 32 IN  A:446
  STORE 73 IN  A:447
  STORE 83 IN  A:448
  STORE 32 IN  A:449
  STORE 76 IN  A:450
  STORE 69 IN  A:451
  STORE 83 IN  A:452
  STORE 83 IN  A:453
  STORE 32 IN  A:454
  STORE 84 IN  A:455
  STORE 72 IN  A:456
  STORE 65 IN  A:457
  STORE 78 IN  A:458
  STORE 32 IN  A:459
  STORE 78 IN  A:460
  STORE 32 IN  A:461
  STORE 68 IN  A:462
  STORE 79 IN  A:463
  STORE 10 IN  A:464
  STORE 32 IN  A:465
  STORE 32 IN  A:466
  STORE 32 IN  A:467
  STORE 32 IN  A:468
  STORE 32 IN  A:469
  STORE 32 IN  A:470
  STORE 32 IN  A:471
  STORE 32 IN  A:472
  STORE 73 IN  A:473
  STORE 70 IN  A:474
  STORE 32 IN  A:475
  STORE 65 IN  A:476
  STORE 58 IN  A:477
  STORE 74 IN  A:478
  STORE 32 IN  A:479
  STORE 73 IN  A:480
  STORE 83 IN  A:481
  STORE 32 IN  A:482
  STORE 69 IN  A:483
  STORE 81 IN  A:484
  STORE 85 IN  A:485
  STORE 65 IN  A:486
  STORE 76 IN  A:487
  STORE 32 IN  A:488
  STORE 84 IN  A:489
  STORE 79 IN  A:490
  STORE 32 IN  A:491
  STORE 52 IN  A:492
  STORE 50 IN  A:493
  STORE 32 IN  A:494
  STORE 84 IN  A:495
  STORE 72 IN  A:496
  STORE 69 IN  A:497
  STORE 78 IN  A:498
  STORE 10 IN  A:499
  STORE 32 IN  A:500
  STORE 32 IN  A:501
  STORE 32 IN  A:502
  STORE 32 IN  A:503
  STORE 32 IN  A:504
  STORE 32 IN  A:505
  STORE 32 IN  A:506
  STORE 32 IN  A:507
  STORE 32 IN  A:508
  STORE 32 IN  A:509
  STORE 67 IN  A:510
  STORE 65 IN  A:511
  STORE 76 IN  A:512
  STORE 76 IN  A:513
  STORE 32 IN  A:514
  STORE 83 IN  A:515
  STORE 85 IN  A:516
  STORE 66 IN  A:517
  STORE 45 IN  A:518
  STORE 80 IN  A:519
  STORE 82 IN  A:520
  STORE 79 IN  A:521
  STORE 67 IN  A:522
  STORE 69 IN  A:523
  STORE 68 IN  A:524
  STORE 85 IN  A:525
  STORE 82 IN  A:526
  STORE 69 IN  A:527
  STORE 32 IN  A:528
  STORE 83 IN  A:529
  STORE 72 IN  A:530
  STORE 79 IN  A:531
  STORE 87 IN  A:532
  STORE 85 IN  A:533
  STORE 10 IN  A:534
  STORE 32 IN  A:535
  STORE 32 IN  A:536
  STORE 32 IN  A:537
  STORE 32 IN  A:538
  STORE 32 IN  A:539
  STORE 32 IN  A:540
  STORE 32 IN  A:541
  STORE 32 IN  A:542
  STORE 69 IN  A:543
  STORE 76 IN  A:544
  STORE 83 IN  A:545
  STORE 69 IN  A:546
  STORE 10 IN  A:547
  STORE 32 IN  A:548
  STORE 32 IN  A:549
  STORE 32 IN  A:550
  STORE 32 IN  A:551
  STORE 32 IN  A:552
  STORE 32 IN  A:553
  STORE 32 IN  A:554
  STORE 32 IN  A:555
  STORE 32 IN  A:556
  STORE 32 IN  A:557
  STORE 83 IN  A:558
  STORE 84 IN  A:559
  STORE 79 IN  A:560
  STORE 82 IN  A:561
  STORE 69 IN  A:562
  STORE 32 IN  A:563
  STORE 67 IN  A:564
  STORE 72 IN  A:565
  STORE 65 IN  A:566
  STORE 82 IN  A:567
  STORE 65 IN  A:568
  STORE 67 IN  A:569
  STORE 84 IN  A:570
  STORE 69 IN  A:571
  STORE 82 IN  A:572
  STORE 32 IN  A:573
  STORE 65 IN  A:574
  STORE 58 IN  A:575
  STORE 74 IN  A:576
  STORE 32 IN  A:577
  STORE 73 IN  A:578
  STORE 78 IN  A:579
  STORE 32 IN  A:580
  STORE 67 IN  A:581
  STORE 10 IN  A:582
  STORE 32 IN  A:583
  STORE 32 IN  A:584
  STORE 32 IN  A:585
  STORE 32 IN  A:586
  STORE 32 IN  A:587
  STORE 32 IN  A:588
  STORE 32 IN  A:589
  STORE 32 IN  A:590
  STORE 32 IN  A:591
  STORE 32 IN  A:592
  STORE 68 IN  A:593
  STORE 73 IN  A:594
  STORE 83 IN  A:595
  STORE 80 IN  A:596
  STORE 76 IN  A:597
  STORE 65 IN  A:598
  STORE 89 IN  A:599
  STORE 32 IN  A:600
  STORE 67 IN  A:601
  STORE 10 IN  A:602
  STORE 32 IN  A:603
  STORE 32 IN  A:604
  STORE 32 IN  A:605
  STORE 32 IN  A:606
  STORE 32 IN  A:607
  STORE 32 IN  A:608
  STORE 32 IN  A:609
  STORE 32 IN  A:610
  STORE 69 IN  A:611
  STORE 78 IN  A:612
  STORE 68 IN  A:613
  STORE 45 IN  A:614
  STORE 73 IN  A:615
  STORE 70 IN  A:616
  STORE 10 IN  A:617
  STORE 32 IN  A:618
  STORE 32 IN  A:619
  STORE 32 IN  A:620
  STORE 32 IN  A:621
  STORE 32 IN  A:622
  STORE 32 IN  A:623
  STORE 32 IN  A:624
  STORE 32 IN  A:625
  STORE 65 IN  A:626
  STORE 68 IN  A:627
  STORE 68 IN  A:628
  STORE 32 IN  A:629
  STORE 74 IN  A:630
  STORE 32 IN  A:631
  STORE 65 IN  A:632
  STORE 78 IN  A:633
  STORE 68 IN  A:634
  STORE 32 IN  A:635
  STORE 49 IN  A:636
  STORE 32 IN  A:637
  STORE 73 IN  A:638
  STORE 78 IN  A:639
  STORE 32 IN  A:640
  STORE 74 IN  A:641
  STORE 10 IN  A:642
  STORE 32 IN  A:643
  STORE 32 IN  A:644
  STORE 32 IN  A:645
  STORE 32 IN  A:646
  STORE 82 IN  A:647
  STORE 69 IN  A:648
  STORE 80 IN  A:649
  STORE 69 IN  A:650
  STORE 65 IN  A:651
  STORE 84 IN  A:652
  STORE 10 IN  A:653
  STORE 32 IN  A:654
  STORE 32 IN  A:655
  STORE 69 IN  A:656
  STORE 78 IN  A:657
  STORE 68 IN  A:658
  STORE 32 IN  A:659
  STORE 83 IN  A:660
  STORE 85 IN  A:661
  STORE 66 IN  A:662
  STORE 45 IN  A:663
  STORE 80 IN  A:664
  STORE 82 IN  A:665
  STORE 79 IN  A:666
  STORE 67 IN  A:667
  STORE 69 IN  A:668
  STORE 68 IN  A:669
  STORE 85 IN  A:670
  STORE 82 IN  A:671
  STORE 69 IN  A:672
  STORE 10 IN  A:673
  STORE 10 IN  A:674
  STORE 42 IN  A:675
  STORE 10 IN  A:676
  STORE 32 IN  A:677
  STORE 32 IN  A:678
  STORE 67 IN  A:679
  STORE 65 IN  A:680
  STORE 76 IN  A:681
  STORE 76 IN  A:682
  STORE 32 IN  A:683
  STORE 83 IN  A:684
  STORE 85 IN  A:685
  STORE 66 IN  A:686
  STORE 45 IN  A:687
  STORE 80 IN  A:688
  STORE 82 IN  A:689
  STORE 79 IN  A:690
  STORE 67 IN  A:691
  STORE 69 IN  A:692
  STORE 68 IN  A:693
  STORE 85 IN  A:694
  STORE 82 IN  A:695
  STORE 69 IN  A:696
  STORE 32 IN  A:697
  STORE 83 IN  A:698
  STORE 72 IN  A:699
  STORE 79 IN  A:700
  STORE 87 IN  A:701
  STORE 10 IN  A:702
  STORE 10 IN  A:703
  STORE 704 IN N

  CALL SUB-PROCEDURE SHOW

```



## Liberty BASIC


```lb
s$ = "s$ = : Print Left$(s$, 5) + chr$(34) + s$ + chr$(34) + Mid$(s$, 5)" : Print Left$(s$, 5) + chr$(34) + s$ + chr$(34) + Mid$(s$, 5)
```



## LIL

LIL is a library with lil, an interactive shell.  In the interactive shell


```txt

# reflect this
reflect this
```


Source from a file read from lil when the shell is invoked ''gets a little trickier''.  The interactive shell wraps code read from files which changes '''this'''.  So, as a testament to ease of embedding, here's a new non-interactive LIL one liner shell so we can '''write [reflect this]'''.


```c
/*
   One line of LIL, given as argv[1]
   Tectonics: gcc -o lilone lilone.c lil.c -lm
      ./lilone 'print [reflect this]'
*/
#include <stdio.h>
#include "lil.h"
int main(int argc, char** argv) {
    lil_t lil = lil_new();
    lil_value_t result = lil_parse(lil, argv[1], 0, 1);

    const char *show = lil_to_string(result);
    if (show && show[0]) printf("%s\n", show);

    lil_free_value(result);
    lil_free(lil);
    return 0;
}
```


{{out}}

```txt
prompt$ gcc -o lilone lilone.c lil.c -lm
prompt$ ./lilone 'print [reflect this]'
print [reflect this]
prompt$
```

More attractive in the capture, the above is not quite right when it comes to newlines.  So, the better answer would be:

```txt

prompt$ ./lilone 'write [reflect this]'
write [reflect this]prompt$
```


As an extra evaluation trial, command substitution from lilone, back into lilone:

```txt
prompt$ ./lilone "$(./lilone 'write [reflect this]')"
write [reflect this]prompt$
```


To avoid illusions of cheating in lilone.

```txt
prompt$ ./lilone 'expr 6 * 7'
42
```


Use '''print [reflect this]''' if the input source ends in a newline or just '''write [reflect this]'''.

Turns out the kernel of this quine can also be used with carefully selected surrounding code:


```txt
./lilone 'set something; write [reflect this]; write $something'
set something; write [reflect this]; write $something
```
 as one example


## Lisp

Lisp has a mechanism to ''quote'' code without the need to use strings:

```lisp
((lambda (x) (list x (list 'quote x)))
  '(lambda (x) (list x (list 'quote x))))
```



## Logo

Somewhat brute-force..

```logo
make "a [ 116 121 112 101 32 34 124 109 97 107 101 32 34 97 32 91 124 10 102 111 114 101 97 99 104 32 58 97 32 91 32 116 121 112 101 32 119 111 114 100 32 34 124 32 124 32 63 32 93 10 112 114 105 110 116 32 34 124 32 93 124 10 102 111 114 101 97 99 104 32 58 97 32 91 32 116 121 112 101 32 99 104 97 114 32 63 32 93 10 98 121 101 10 ]
type "|make "a [|
foreach :a [ type word "| | ? ]
print "| ]|
foreach :a [ type char ? ]
bye
```



## Lua


```lua
s=[[io.write('s=[','[',s,']','];',s)]];io.write('s=[','[',s,']','];',s)
```



## M4




```m4
define(`quine',``$1(`$1')'')dnl
quine(`define(`quine',``$1(`$1')'')dnl
quine')
```



## Mathematica


```Mathematica
a="Print[\"a=\",InputForm[a],\";\",a]";Print["a=",InputForm[a],";",a]
```


or


```Mathematica
a := Print[InputForm[Definition[a]], ";a"];a
```


or


```Mathematica
(#1[#0[#1]] &)[Defer]
```


or


```Mathematica
ToString[#0][] & []
```


or


```Mathematica
Unevaluated[1989 - 1989]
```


or


```Mathematica>x</lang
 :)

=={{header|MATLAB}} / {{header|Octave}}==
This script runs either from an m-file or directly from workspace (author: J. C. Lucero): 

```Matlab
x='{>\(y>(((-y-(((<(^<ejtq)\{-y.2^*<';z=['x=''',x,''';'];disp([z,x-1]);
```


----

The example works for functions and scripts stored as m-files. It does not work for functions defined only in the workspace.  

```Matlab
  function quine()
    fid = fopen([mfilename,'.m']);
    while ~feof(fid)
      printf('%s\n',fgetl(fid));
    end; 
    fclose(fid); 	
  end;
```

or

```Matlab
  function quine()
    type(mfilename);
  end;
```


Without accessing the file:

```Matlab
function quine; y = 'function quine; y = %c%s%c; fprintf(1, y, 39, y, 39)\n'; fprintf(1, y, 39, y, 39)
```



## Maxima


```maxima
/* Using ?format from the unerlying Lisp system */

lambda([],block([q:ascii(34),s:"lambda([],block([q:ascii(34),s:~A~A~A],print(?format(false,s,q,s,q))))()$"],print(?format(false,s,q,s,q))))()$
```


=={{header|Modula-2}}==

```modula2
MODULE Quine;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

CONST src = "MODULE Quine;\nFROM FormatString IMPORT FormatString;\nFROM Terminal IMPORT WriteString,ReadChar;\n\nCONST src = \x022%s\x022;\nVAR buf : ARRAY[0..2048] OF CHAR;\nBEGIN\n    FormatString(src, buf, src);\n    WriteString(buf);\n    ReadChar\nEND Quine.\n";
VAR buf : ARRAY[0..2048] OF CHAR;
BEGIN
    FormatString(src, buf, src);
    WriteString(buf);
    ReadChar
END Quine.
```



## MUMPS


```MUMPS
QUINE
 NEW I,L SET I=0
 FOR  SET I=I+1,L=$TEXT(+I) Q:L=""  WRITE $TEXT(+I),!
 KILL I,L
 QUIT

SMALL
 S %=0 F  W $T(+$I(%)),! Q:$T(+%)=""
```

Both of the routines will work, but the second has the minor advantage of only using one variable instead of two.


## NASM

Compiles to the source code when -f raw 

```ASM
%define a "%define "
%define b "db "
%define c "%deftok "
%define d "a, 97, 32, 34, a, 34, 10, a, 98, 32, 34, b, 34, 10, a, 99, 32, 34, c, 34, 10, a, 100, 32, 34, d, 34, 10, c, 101, 32, 100, 10, b, 101, 10"
%deftok e d
db e
```


## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

Q = "'"
S = "\\"
N = "\n"
A = "&"
code = [ -
  '/* NetRexx */', -
  'options replace format comments java crossref savelog symbols nobinary', -
  '', -
  'Q = "&QS"', -
  'S = "&ESC"', -
  'N = "&NL"', -
  'A = "&AMP"', -
  'code = [ -', -
  '&REP', -
  '  ]', -
  '', -
  'pgm = ""', -
  'txt = ""', -
  'loop t_ = 0 for code.length', -
  '  txt = txt || "  " || Q || code[t_] || Q || ", -" || N', -
  '  end t_', -
  'txt = txt.strip("T", N)', -
  'txt = txt.delstr(txt.lastpos(","), 1)', -
  '', -
  'K = ""', -
  'K[0] = 5', -
  'K[1] = A"NL"', -
  'K[2] = A"AMP"', -
  'K[3] = A"ESC"', -
  'K[4] = A"QS"', -
  'K[5] = A"REP"', -
  'loop c_ = 0 for code.length', -
  '  loop v_ = 1 to K[0]', -
  '    T = K[v_]', -
  '    if code[c_].pos(T) <> 0 then do', -
  '      parse code[c_] pre(T)post', -
  '      select case T', -
  '        when K[1] then do', -
  '          code[c_] = pre || S || "n" || post', -
  '          end', -
  '        when K[2] then do', -
  '          code[c_] = pre || A || post', -
  '          end', -
  '        when K[3] then do', -
  '          code[c_] = pre || S || S || post', -
  '          end', -
  '        when K[4] then do', -
  '          code[c_] = pre || Q || post', -
  '          end', -
  '        when K[5] then do', -
  '          code[c_] = txt', -
  '          end', -
  '        otherwise nop', -
  '        end', -
  '      end', -
  '    end v_', -
  '  pgm = pgm || code[c_].strip("T") || N', -
  '  end c_', -
  'pgm = pgm.strip("T", N) || N', -
  'say pgm', -
  '', -
  'return', -
  '' -
  ]

pgm = ""
txt = ""
loop t_ = 0 for code.length
  txt = txt || "  " || Q || code[t_] || Q || ", -" || N
  end t_
txt = txt.strip("T", N)
txt = txt.delstr(txt.lastpos(","), 1)

K = ""
K[0] = 5
K[1] = A"NL"
K[2] = A"AMP"
K[3] = A"ESC"
K[4] = A"QS"
K[5] = A"REP"
loop c_ = 0 for code.length
  loop v_ = 1 to K[0]
    T = K[v_]
    if code[c_].pos(T) <> 0 then do
      parse code[c_] pre(T)post
      select case T
        when K[1] then do
          code[c_] = pre || S || "n" || post
          end
        when K[2] then do
          code[c_] = pre || A || post
          end
        when K[3] then do
          code[c_] = pre || S || S || post
          end
        when K[4] then do
          code[c_] = pre || Q || post
          end
        when K[5] then do
          code[c_] = txt
          end
        otherwise nop
        end
      end
    end v_
  pgm = pgm || code[c_].strip("T") || N
  end c_
pgm = pgm.strip("T", N) || N
say pgm

return
```



## NewLISP


```NewLISP

(lambda (s) (print (list s (list 'quote s))))

```



## Nim


The empty program is a quine in Nim:

```nim></lang


Another quine:

```nim
const x = "var x = echo x[0..7],chr(34),x,chr(34),chr(10),x[8 .. ^1]"
echo x[0..7],chr(34),x,chr(34),chr(10),x[8 .. ^1]
```


This quine prints its own sourcecode at compiletime as well as at runtime:

```nim
const x = "const x = |const y = x[0..9]&34.chr&x&34.chr&10.chr&x[11..100]&10.chr&x[102..115]&10.chr&x[117 .. ^1]|static: echo y|echo y"
const y = x[0..9]&34.chr&x&34.chr&10.chr&x[11..100]&10.chr&x[102..115]&10.chr&x[117 .. ^1]
static: echo y
echo y
```

Compile with: nim --verbosity:0 c quine

=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 LIST
```



## Objeck


```ocaml
class Program { function : Main(args : String[]) ~ Nil { s := "class Program { function : Main(args : String[]) ~ Nil { s :=;  IO.Console->Print(s->SubString(61))->Print(34->As(Char))->Print(s)->Print(34->As(Char))->PrintLine(s->SubString(61, 129)); } }";  IO.Console->Print(s->SubString(61))->Print(34->As(Char))->Print(s)->Print(34->As(Char))->PrintLine(s->SubString(61, 129)); } }
```



## OCaml

(newline included)

```ocaml
(fun p -> Printf.printf p (string_of_format p)) "(fun p -> Printf.printf p (string_of_format p)) %S;;\n";;
```


Alternative:
(newline included)

```ocaml
(fun s -> Printf.printf "%s%S;;\n" s s) "(fun s -> Printf.printf \"%s%S;;\\n\" s s) ";;
```



## Oforth



```Oforth
"dup 34 emit print 34 emit BL emit print" dup 34 emit print 34 emit BL emit print
```



## Ol

Like others Lisps,

```ol
((lambda (s) (display (list s (list (quote quote) s)))) (quote (lambda (s) (display (list s (list (quote quote) s))))))
```



## ooRexx



### Cheating


```ooRexx
say sourceline(1)
```


===Non-Cheating, with INTERPRET===

```ooRexx
r="say'r='x2c(22)r||22~x2c';interpret r'";interpret r
```


===Non-Cheating, without INTERPRET===

```ooRexx
r=";say'r=.'r'.'r~changestr(.,'22'x,2)";say'r="'r'"'r~changestr(.,'22'x,2)
```


===Non-Cheating, without INTERPRET, with unprintable characters===
Note:   This version will not work correctly on an ASCII machine   (where   '07'x   is the   '''bell'''   control code), 

:: it will only work correctly on an EBCDIC machine   (where '07'x is the   '''del'''   control code). 


The two unprintable characters are '07'x.  Somewhat difficult to enter, but still a perfectly valid ooRexx program (and Quine).

```ooRexx
r=";say'r=�'r'�'r~bitor(,' ')";say'r="'r'"'r~bitor(,' ')
```



## OxygenBasic


```oxygenbasic

'RUNTIME COMPILING

source="print source"

a=compile source : call a : freememory a

```



## Oz

A one-liner that uses the ASCII code for <code>"</code> to avoid "quoted quotes". We put the value assignment to the end by using multithreading and implicit synchronization on the dataflow variable <code>I</code>.

```oz
declare I in thread {System.showInfo I#[34]#I#[34]} end I ="declare I in thread {System.showInfo I#[34]#I#[34]} end I ="
```



## PARI/GP


```parigp
()->quine
```


Assign this source code to variable quine:

```parigp
quine=()->quine
```


Output of interpreter:
```txt
gp > quine
()->quine

gp > quine()
()->quine

gp > quine == quine()
1

```

The last command tests if source code in variable ''quine'' is equal to output of executed function ''quine()''. This is true (''1'').


## Pascal

A modification of one Pascal example (Author: Oliver Heen) from http://www.nyx.net/~gthompso/self_pasc.txt. This example includes newline at the end.

```pascal
const s=';begin writeln(#99#111#110#115#116#32#115#61#39,s,#39,s)end.';begin writeln(#99#111#110#115#116#32#115#61#39,s,#39,s)end.
```


This version does compile in Free Pascal and UCSD Pascal. Newline included.

```pascal
program Quine(Output);const A='program Quine(Output);const A=';B='begin writeln(A,char(39),A,char(39),char(59),char(66),char(61),char(39),B,char(39),char(59),B)end.';begin writeln(A,char(39),A,char(39),char(59),char(66),char(61),char(39),B,char(39),char(59),B)end.
```


A quality quine should not depend on a particular character set. This version also works on systems limited to 80-character lines and on legacy word-based systems which require literal strings to be packed and of a fixed size if assigned to an array.

```pascal
program main(output);type string=packed array[1..60]of char;
var l:array[1..9]of string; c:array[1..7]of char; i:integer;
lc, a, o, k, n, e, s, t:char; begin
l[1]:='program main(output);type string=packed array[1..60]of char;';
l[2]:='var l:array[1..9]of string; c:array[1..7]of char; i:integer;';
l[3]:='lc, a, o, k, n, e, s, t:char; begin                         ';
l[4]:='for i := 1 to 3 do writeln(l[i]);                           ';
l[5]:='a:=c[1];t:=c[2];o:=c[3];k:=c[4];n:=c[5];e:=c[6];s:=c[7];    ';
l[6]:='for i := 1 to 9 do writeln(a,o,i:1,k,n,e,lc,l[i],lc,s);     ';
l[7]:='writeln(a, t, n, e, lc, lc, lc, lc, s);                     ';
l[8]:='for i := 1 to 7 do write(t,o,i:1,k,n,e,lc,c[i],lc,s);       ';
l[9]:='writeln; for i := 4 to 9 do writeln(l[i]); end.             ';
lc:='''';
c[1]:='l';c[2]:='c';c[3]:='[';c[4]:=']';c[5]:=':';c[6]:='=';c[7]:=';';
for i := 1 to 3 do writeln(l[i]);
a:=c[1];t:=c[2];o:=c[3];k:=c[4];n:=c[5];e:=c[6];s:=c[7];
for i := 1 to 9 do writeln(a,o,i:1,k,n,e,lc,l[i],lc,s);
writeln(a, t, n, e, lc, lc, lc, lc, s);
for i := 1 to 7 do write(t,o,i:1,k,n,e,lc,c[i],lc,s);
writeln; for i := 4 to 9 do writeln(l[i]); end.
```


## Perl

This relatively simple Perl example imitates the C example.

```perl
$s = q($s = q(%s); printf($s, $s);
); printf($s, $s);

```

Note the terminating newline.

===Self-reading (some consider cheating)===
Accessing source code via <code>DATA</code> filehandle:
```Perl
seek(DATA, 0, 0);
print <DATA>
__DATA__
```

even simpler:

```Perl
open ME, $0 and print <ME>;
```


```Perl>open 0; print <0>;</lang



### Quine Generator


Perhaps the simplest quine in Perl 5 is:

```Perl
$_=q{print"\$_=q{$_};eval"};eval
```

By carefully examining the code, you will find that you can insert any valid perl sentences before and after the print statement in q{}.
It is just a few lines of codes away before you come up with a quine generator that turns a given perl script into a quine.  Note this quine generator itself is a quine.

cf. [http://blog.livedoor.jp/dankogai/archives/51519405.html http://blog.livedoor.jp/dankogai/archives/51519405.html]



```Perl
{local$_=q{
{
    package Quine;
    use strict;
    use warnings;
    our $VERSION = sprintf "%d.%02d", q$Revision: 0.2 $ =~ /(\d+)/g;
    my $head = '{local$_=q' . "\x7B\n";
    my $tail = 'print"{local\$_=q{$_};eval}\n"' . "\x7D;eval}\n";

    sub new {
        my $class = shift;
        my $quine = $head . shift;
        my $ret   = shift || 1;
        my $ln    = ( $quine =~ tr/\n/\n/ );
        $ln++;
        $quine .= "return $ret if caller(1)or(caller)[2]!=$ln;$tail";
        bless \$quine, $class;
    }

    sub from_file {
        my ( $class, $fn, $ret ) = @_;
        local $/;
        open my $fh, '<', $fn or die "$fn : $!";
        my $src = <$fh>;
        close $fh;
        $class->new( $src, $ret );
    }

    sub quine { ${ $_[0] } }

=head1 NAME

Quine - turn your perl modules/apps into a true quine!

=head1 VERSION

$Id: Quine.pm,v 0.2 2010/09/15 20:23:53 dankogai Exp dankogai $

=head1 SYNOPSIS

  use Quine;
  print Quine->from_file("woot.pl")->quine;
  print Quine->from_file("moot.psgi", '$app')->quine;

=cut
}
return 1 if caller(1);print"{local\$_=q{$_};eval}\n"};eval}
```



## Perl 6

{{trans|Haskell}}
{{works with|Rakudo|#32 "Pisa"}}

```perl6
my &f = {say $^s, $^s.perl;}; f "my \&f = \{say \$^s, \$^s.perl;}; f "

```

Note the terminating newline.


{{works with|Rakudo|2014.04}}
A more compact, but still purely functional, approach:

```perl6
{.fmt($_).say}(<{.fmt($_).say}(<%s>)>)

```

Note again the terminating newline.


## Phix

Credit: Aidan Bindoff

```Phix
constant c="constant c=%sprintf(1,c,{34&c&34})"printf(1,c,{34&c&34})
```



## PHP

{{trans|C}}

```php
<?php $p = '<?php $p = %c%s%c; printf($p,39,$p,39); ?>
'; printf($p,39,$p,39); ?>
```
 
Note the terminating newline.

Technically, anything outside of <code><?php </code>, <code>&lt;script language="php"></code>, <code><?</code> (deprecated) or <code><%</code> (deprecated) tags is automatically echoed by PHP allowing for easier inclusion of HTML in PHP documents. So the following is a quine:

 Rosetta code :).


## PicoLisp

===Using 'quote'===
Note that 'quote' in PicoLisp corresponds to 'lambda' in other Lisps

```PicoLisp
('((X) (list (lit X) (lit X))) '((X) (list (lit X) (lit X))))
```

{{out}}

```txt
-> ('((X) (list (lit X) (lit X))) '((X) (list (lit X) (lit X))))
```


===Using 'let'===

```PicoLisp
(let X '(list 'let 'X (lit X) X) (list 'let 'X (lit X) X))
```

{{out}}

```txt
-> (let X '(list 'let 'X (lit X) X) (list 'let 'X (lit X) X))
```



## PlainTeX


### Output to terminal

This assumes the filename of the source file to be ‘q.tex’, and the banner from vanilla Knuth’s TeX:

```plainTeX
This is TeX, Version 3.1415926 (no format preloaded)
(q.tex \output {\message {\output \the \output \end }\batchmode }\end 
```


### Output to dvi


```plainTeX
    \let~\expandafter\def\0{~\ttraggedright~\let~\s~\string~\def~~~{~\s~~~\s~}~\output~
~{~}~\s~\let~~~\expandafter~\s~\def~\s~\0~\s~{~\0~\s~}~\s~\0~\end}\0
```



## PL/I

The following code, containing 709 significant bytes, compiles completely clean using the old MVS PL/I V2.3.0 Compiler, using the default 2,72,1 margins:

```PL/I
 s:proc options(main)reorder;dcl sysprint file,m(7)init(
 ' s:proc options(main)reorder\dcl sysprint file,m(7)init(',
 ' *)char(99),i,j,(translate,substr)builtin,c char\i=1\j=n',
 ' \do i=1 to 6\put skip list('' '''''')\do j=2 to 56\c=substr',
 ' (m(i),j)\put edit(c)(a)\n:proc\put list(translate(m(i),',
 ' ''5e''x,''e0''x))\end n\if c='''''''' then put edit(c)(a)\end\ ',
 ' put edit('''''','')(a(50))\end\do i=2 to 6\j=n\end\end s\  ',
 *)char(99),i,j,(translate,substr)builtin,c char;i=1;j=n
 ;do i=1 to 6;put skip list(' ''');do j=2 to 56;c=substr
 (m(i),j);put edit(c)(a);n:proc;put list(translate(m(i),
 '5e'x,'e0'x));end n;if c='''' then put edit(c)(a);end;
 put edit(''',')(a(50));end;do i=2 to 6;j=n;end;end s;
```


An even smaller version (believed to be the smallest possible PL/I Quine), containing 326 significant bytes, shows the unique capabilities of the same old V2.3.0 compiler in being able to generate correct code, ovecoming 2 pre-processor errors, 8 severe errors, 10 errors, 6 warnings and 2 informational messages. It requires the following non-default compile options, 'M,MAR(1,90),C'.

```PL/I
%dcl z%z='put edit';proc options(main;q=''''put list(m;do i=1,2;z(q)skip;do j=
1to 78c=substr(m(i),j;if c=q z(c;z(c;end;z(q',';dcl(c,q)char,m(2)char(99)init(
'%dcl z%z=''put edit'';proc options(main;q=''''''''put list(m;do i=1,2;z(q)skip;do j=',
'1to 78c=substr(m(i),j;if c=q z(c;z(c;end;z(q'','';dcl(c,q)char,m(2)char(99)init(',
```


The equivalent version that compiles with the newer z/OS Enterprise PL/I compilers, coming in at 434 significant bytes and requiring the following two compiler options, 'm,margins(1,80,0)', is:

```PL/I
%dcl z;%z="put edit";w:proc options(main);q="'";put list(m)do i=1,2,3;z
(q)(a)skip;do j=1to 71;c=substr(m(i),j)if c=q then z(c)(a)z(c)(a)end;if
i<3then z(q,",")(a)else z(q,";")(a)end;dcl(c,q)char,m(3)char(99)init(
'%dcl z;%z="put edit";w:proc options(main);q="''";put list(m)do i=1,2,3;z',
'(q)(a)skip;do j=1to 71;c=substr(m(i),j)if c=q then z(c)(a)z(c)(a)end;if',
'i<3then z(q,",")(a)else z(q,";")(a)end;dcl(c,q)char,m(3)char(99)init(  ';
```



## PowerBASIC

This is ''technically'' based on the [[#BASIC|BASIC]] code above, but is in fact a complete rewrite.

```powerbasic
FUNCTION PBMAIN () AS LONG
    REDIM s(1 TO DATACOUNT) AS STRING
    o$ = READ$(1)
    d$ = READ$(2)
    FOR n& = 1 TO DATACOUNT
        s(n&) = READ$(n&)
    NEXT
    OPEN o$ FOR OUTPUT AS 1
    FOR n& = 3 TO DATACOUNT - 1
        PRINT #1, s(n&)
    NEXT
    PRINT #1,
    FOR n& = 1 TO DATACOUNT
        PRINT #1, d$ & $DQ & s(n&) & $DQ
    NEXT
    PRINT #1, s(DATACOUNT)
    CLOSE

    DATA "output.src"
    DATA "    DATA "
    DATA "FUNCTION PBMAIN () AS LONG"
    DATA "    REDIM s(1 TO DATACOUNT) AS STRING"
    DATA "    o$ = READ$(1)"
    DATA "    d$ = READ$(2)"
    DATA "    FOR n& = 1 TO DATACOUNT"
    DATA "        s(n&) = READ$(n&)"
    DATA "    NEXT"
    DATA "    OPEN o$ FOR OUTPUT AS 1"
    DATA "    FOR n& = 3 TO DATACOUNT - 1"
    DATA "        PRINT #1, s(n&)"
    DATA "    NEXT"
    DATA "    PRINT #1,"
    DATA "    FOR n& = 1 TO DATACOUNT"
    DATA "        PRINT #1, d$ & $DQ & s(n&) & $DQ"
    DATA "    NEXT"
    DATA "    PRINT #1, s(DATACOUNT)"
    DATA "    CLOSE"
    DATA "END FUNCTION"
END FUNCTION
```



## PowerShell

{{works with|PowerShell|2}}
Adapted from Liberty BASIC.

```PowerShell

$S = '$S = $S.Substring(0,5) + [string][char]39 + $S + [string][char]39 + [string][char]10 + $S.Substring(5)'
$S.Substring(0,5) + [string][char]39 + $S + [string][char]39 + [string][char]10 + $S.Substring(5)

```

{{out}}

```txt

$S = '$S = $S.Substring(0,5) + [string][char]39 + $S + [string][char]39 + [string][char]10 + $S.Substring(5)'
$S.Substring(0,5) + [string][char]39 + $S + [string][char]39 + [string][char]10 + $S.Substring(5)

```


<b>Variations to access the code directly</b>


<b>In a saved script</b>

```PowerShell

$MyInvocation.MyCommand.ScriptContents

```

<b>At the command line</b>

```PowerShell

$MyInvocation.MyCommand.Definition

```

<b>In a function</b>

```PowerShell

function Quine { $MyInvocation.MyCommand.Definition }

```



## Prolog

One method to produce a quine is to read the data structures that hold the source code:

```Prolog
quine :- 
	listing(quine).
```


Producing a quine without cheating is more complex. The following program produces a data structure equivalent to the program itself, then outputs it:


```Prolog
quine:-asserta(a((quine:-asserta(a(A,B)),a(C,D+E),a(D,E),numbervars(C),write(C),write(.)),A+B)),a(F,G+H),a(G,H),numbervars(F),write(F),write(.).
```


The use of <tt>numbervars</tt> is a GNU Prolog extension that fixes the variable names in the output to be single letters, in alphabetical order by appearance, and thus match the original program. Removing it, the quine still works, but the variable names in the output will probably be different; thus, it will produce a program equivalent to itself, rather than an exact copy of itself.

A version that is source-code-centric rather than AST-centric, and does not rely on any built-in facilities for formatted printing, might look as follows:


```Prolog
% Tested with SWI-Prolog version 7.1.37
:- initialization(main).

before(Lines) :- Lines = [
  "% Tested with SWI-Prolog version 7.1.37",
  ":- initialization(main).",
  "",
  "before(Lines) :- Lines = ["
].

after(Lines) :- Lines = [
  "].",
  "",
  "% replaces quotes by harmless ats",
  "% replaces backslashes by harmless slashes",
  "% replaces linebreaks by harmless sharps",
  "maskCode(34, 64).",
  "maskCode(92, 47).",
  "maskCode(10, 35).",
  "maskCode(X, X).",
  "",
  "% Encodes dangerous characters in a string",
  "encode(D, S) :- ",
  "  string_codes(D, DC),",
  "  maplist(maskCode, DC, SC),",
  "  string_codes(S, SC).",
  "",
  "decode(S, D) :- ",
  "  string_codes(S, SC),",
  "  maplist(maskCode, DC, SC),",
  "  string_codes(D, DC).",
  "",
  "% writes each entry indented by two spaces,",
  "% enclosed in quotes and separated by commas,",
  "% with a newline between the list entries.",
  "mkStringList([],@@).",
  "mkStringList([Single],Out) :-",
  "  atomics_to_string([@  /@@, Single, @/@@], Out).",
  "",
  "mkStringList([H|T], Res) :-",
  "  mkStringList(T, TailRes),",
  "  atomics_to_string([@  /@@, H, @/@,/n@, TailRes], Res).",
  "",
  "quine(Q) :- ",
  "  before(BeforeEncoded),",
  "  after(AfterEncoded),",
  "  maplist(decode, BeforeEncoded, BeforeDecoded),",
  "  maplist(decode,  AfterEncoded, AfterDecoded),",
  "  atomic_list_concat(BeforeDecoded, @/n@, B),",
  "  atomic_list_concat(AfterDecoded, @/n@, A),",
  "  mkStringList(BeforeEncoded, BeforeData),",
  "  mkStringList(AfterEncoded, AfterData),",
  "  Center = @/n]./n/nafter(Lines) :- Lines = [/n@,",
  "  atomic_list_concat([",
  "     B, @/n@, BeforeData, ",
  "     Center, ",
  "     AfterData, @/n@, A, @/n@",
  "  ], Q).",
  "",
  "main :- (quine(Q), write(Q);true),halt.",
  "% line break in the end of file is important"
].

% replaces quotes by harmless ats
% replaces backslashes by harmless slashes
% replaces linebreaks by harmless sharps
maskCode(34, 64).
maskCode(92, 47).
maskCode(10, 35).
maskCode(X, X).

% Encodes dangerous characters in a string
encode(D, S) :- 
  string_codes(D, DC),
  maplist(maskCode, DC, SC),
  string_codes(S, SC).

decode(S, D) :- 
  string_codes(S, SC),
  maplist(maskCode, DC, SC),
  string_codes(D, DC).

% writes each entry indented by two spaces,
% enclosed in quotes and separated by commas,
% with a newline between the list entries.
mkStringList([],"").
mkStringList([Single],Out) :-
  atomics_to_string(["  \"", Single, "\""], Out).

mkStringList([H|T], Res) :-
  mkStringList(T, TailRes),
  atomics_to_string(["  \"", H, "\",\n", TailRes], Res).

quine(Q) :- 
  before(BeforeEncoded),
  after(AfterEncoded),
  maplist(decode, BeforeEncoded, BeforeDecoded),
  maplist(decode,  AfterEncoded, AfterDecoded),
  atomic_list_concat(BeforeDecoded, "\n", B),
  atomic_list_concat(AfterDecoded, "\n", A),
  mkStringList(BeforeEncoded, BeforeData),
  mkStringList(AfterEncoded, AfterData),
  Center = "\n].\n\nafter(Lines) :- Lines = [\n",
  atomic_list_concat([
     B, "\n", BeforeData, 
     Center, 
     AfterData, "\n", A, "\n"
  ], Q).

main :- (quine(Q), write(Q);true),halt.
% line break in the end of file is important

```

It could be made shorter. This version contains some introns that have been used for code generation (like the encode/2 predicate, that is not really needed).


## PureBasic


```PureBasic
s$="s$= : Debug Mid(s$,1,3)+Chr(34)+s$+Chr(34)+Mid(s$,4,100)" : Debug Mid(s$,1,3)+Chr(34)+s$+Chr(34)+Mid(s$,4,100)
```



## Python


{{works with|Python|2.x and 3.x}}
A simple and straightforward quine. Character chr(34) is a double quote, and chr(10) is a new line (Author: J. C. Lucero).
 

```python

w = "print('w = ' + chr(34) + w + chr(34) + chr(10) + w)"
print('w = ' + chr(34) + w + chr(34) + chr(10) + w) 

```


----

{{works with|Python|2.x and 3.x}}
Python's <tt>%r</tt> format conversion uses the <tt>repr()</tt> function to return a string containing the source code representation of its argument:

```python
x = 'x = %r\nprint(x %% x)'
print(x % x)
```


{{works with|Python|3.x and 2.6+}}
With the new <tt>str.format</tt>:

```python
x = 'x = {!r};print(x.format(x))';print(x.format(x))
```


{{works with|Python|2.x and 3.x}}
After creating the file "Quine.py" with the following source, running the
program will spit the code back out on a terminal window:


```python
import sys; sys.stdout.write(open(sys.argv[0]).read())
```

Note: actually an empty file could be treated as python quine too.

{{works with|Python|2.x and 3.x}}

```python
import sys,inspect;sys.stdout.write(inspect.getsource(inspect.currentframe()))
```


----

Due to Leon Naley (name guessed) from [http://forums.devshed.com/python-programming-11/2nd-shortest-quine-in-python-ever-937355.html#post2845853 devshed python forum]

''I think I just thought of the shortest quine for python I think there can ever be! (except a blank program)

make a program with this single line in it

```python

print(__file__[:-3])

```

and name the file print(__file__[:-3]).py

and run the file!''

bash example command:

```bash
$ python print\(__file__\[\:-3\]\).py 
print(__file__[:-3])

```


Python 3, from same thread at same forum, created by wewhgyih, tested by LambertDW.  Works on unix and Windows7.  On Windows7 I commanded python "print(__file__)" .

```bash

$ cat print\(__file__\) 
print(__file__)
$ python print\(__file__\) 
print(__file__)

```

I saw this clever solution somewhere some time ago (dont' remember the source.)  Assuming the input does not have to be a correct program...
<lang>$ cat reproducing.py 
  File "reproducing.py", line 1
    File "reproducing.py", line 1
    ^
IndentationError: unexpected indent

$ python reproducing.py 
  File "reproducing.py", line 1
    File "reproducing.py", line 1
    ^
IndentationError: unexpected indent
```


----

Here's a few by me (Nathaniel Virgo). They represent attempts to make clean, readable "Pythonic" quines. The first one is straightforward, using format and chr(34) to construct a multiline string:


```python

x = """x = {0}{1}{0}
print x.format(chr(34)*3,x)"""
print x.format(chr(34)*3,x)

```


This next one uses a base64 encoding, which is an off-the-shelf way to pack strings containing quotes and newlines etc. into ones that consist only of printable characters. Python purists will probably tell me I should have used the base64 package instead.


```python

a = 'YSA9ICcnCmIgPSBhLmRlY29kZSgnYmFzZTY0JykKcHJpbnQgYls6NV0rYStiWzU6XQ=='
b = a.decode('base64')
print b[:5]+a+b[5:]

```


In the above quine I felt it was inelegant to hard code the insertion of the data after the 5th character and I wondered if I could do it in a cleaner way. Eventually I came up with this:


```python

data = (
	'ZGF0YSA9ICgKCSc=',
	'JywKCSc=',
	'JwopCnByZWZpeCwgc2VwYXJhdG9yLCBzdWZmaXggPSAoZC5kZWNvZGUoJ2Jhc2U2NCcpIGZvciBkIGluIGRhdGEpCnByaW50IHByZWZpeCArIGRhdGFbMF0gKyBzZXBhcmF0b3IgKyBkYXRhWzFdICsgc2VwYXJhdG9yICsgZGF0YVsyXSArIHN1ZmZpeA=='
)
prefix, separator, suffix = (d.decode('base64') for d in data)
print prefix + data[0] + separator + data[1] + separator + data[2] + suffix

```


Finally, here's one that echoes the classic "Y combinator" way of constructing quines. First we define a function that takes some code, wraps it in a function and then applies than function to its own source code, then we apply *that* function to its own source code:


```python

def applyToOwnSourceCode(functionBody):
	print "def applyToOwnSourceCode(functionBody):"
	print functionBody
	print "applyToOwnSourceCode(" + repr(functionBody) + ")"
applyToOwnSourceCode('\tprint "def applyToOwnSourceCode(functionBody):"\n\tprint functionBody\n\tprint "applyToOwnSourceCode(" + repr(functionBody) + ")"')

```



## R

Adapted from the C version in this list.

```R
(function(){x<-intToUtf8(34);s<-"(function(){x<-intToUtf8(34);s<-%s%s%s;cat(sprintf(s,x,s,x))})()";cat(sprintf(s,x,s,x))})()
```


Another version, perhaps more understandable.

```R

src <- "\nwriteLines(c(paste(\"src <-\", encodeString(src, quote='\"')), src))"

writeLines(c(paste("src <-", encodeString(src, quote='"')), src))

```



## Racket

The classic:

```racket
((λ (x) `(,x ',x)) '(λ (x) `(,x ',x)))
```


As a module:

```racket
(module quine racket
  (pretty-write
   ((λ (x) `(module quine racket (pretty-write (,x ',x))))
    '(λ (x) `(module quine racket (pretty-write (,x ',x)))))))
```


As a module via #lang line:

```racket
#lang racket
((λ(x)(printf "#lang racket\n(~a\n ~s)" x x))
 "(λ(x)(printf \"#lang racket\\n(~a\\n ~s)\" x x))")
```



## REBOL


```REBOL
rebol [] q: [print ["rebol [] q:" mold q "do q"]] do q
```



## REXX



### version 1


```rexx
/*REXX program outputs its own 1─line source.*/    say sourceline(1)
```

{{out|output}}

```txt

/*REXX program outputs its own 1─line source.*/    say sourceline(1)

```



### version 2


```rexx
/*REXX program outputs its own multi─line source.*/

    do j=1  for sourceline()
    say sourceline(j)
    end   /*j*/
```

{{out|output}}

```txt

/*REXX program outputs its own multi─line source.*/

    do j=1  for sourceline()
    say sourceline(j)
    end   /*j*/

```



### version 3

A version that doesn't use <code>sourceline()</code> which is kind of a cheat.

---But it   ''is''   allowed (see the 2<sup>nd</sup> bullet point in the task's preamble).  



<s>'''Error?'''</s>
''The rest of this conversation moved to [[Talk:Quine|Talk]]''


```rexx
/* Rexx */

Q = "'"
Queue '/* Rexx */'
Queue ''
Queue 'Q = "$"'
Queue '&QQQ'
Queue ''
Queue 'X = 0'
Queue 'Do while Queued() \= 0'
Queue '  Parse pull code'
Queue '  X = X + 1; codel.0 = X; codel.X = code'
Queue '  End'
Queue ''
Queue 'Do x_ = 1 for codel.0'
Queue '  line = codel.x_'
Queue '  If abbrev(line, "Q = ") then Do'
Queue '    line = translate(line, Q, "$")'
Queue '    End'
Queue '  If line = "&QQQ" then Do'
Queue '    Do y_ = 1 to codel.0'
Queue '      qline = codel.y_'
Queue '      Say "Queue" Q || qline || Q'
Queue '      End y_'
Queue '    End'
Queue '  else Do'
Queue '    Say line'
Queue '    End'
Queue '  End x_'
Queue ''

X = 0
Do while Queued() \= 0
  Parse pull code
  X = X + 1; codel.0 = X; codel.X = code
  End

Do x_ = 1 for codel.0
  line = codel.x_
  If abbrev(line, "Q = ") then Do
    line = translate(line, Q, "$")
    End
  If line = "&QQQ" then Do
    Do y_ = 1 to codel.0
      qline = codel.y_
      Say "Queue" Q || qline || Q
      End y_
    End
  else Do
    Say line
    End
  End x_

```



## Ring


```ring
v = "see substr(`v = ` + char(34) + `@` + char(34) + nl + `@` ,`@`,v)"
see substr(`v = ` + char(34) + `@` + char(34) + nl + `@` ,`@`,v)
```



## Ruby

Found online:

```txt
$ ruby -e '_="_=%p;puts _%%_";puts _%_'
_="_=%p;puts _%%_";puts _%_
$ ruby -e '_="_=%p;puts _%%_";puts _%_' | ruby
_="_=%p;puts _%%_";puts _%_
```


more readably:

```ruby
x = "x = %p; puts x %% x"; puts x % x
```

The <tt>%p</tt> specifier outputs the result of calling the <tt>.inspect</tt> method on the argument.

even shorter (by a few characters):

```ruby
puts <<e*2,'e'
puts <<e*2,'e'
e
```


perhaps the simplest:

```ruby
eval s="puts'eval s='+s.inspect"
```


One (maybe a bit verbose) version, to be appended to the end of any file. This doesn't work in IRB, because it isn't a file.

```ruby

f = File.open __FILE__
f.each_line do |line|
 puts line
f.close

```


or 

```ruby

p open(__FILE__).read

```



## Rust



A short quine (works with Rust 1.3.0):


```rust
fn main() {
    let x = "fn main() {\n    let x = ";
    let y = "print!(\"{}{:?};\n    let y = {:?};\n    {}\", x, x, y, y)\n}\n";
    print!("{}{:?};
    let y = {:?};
    {}", x, x, y, y)
}
```


Using the method on Wikipedia (0.9-pre-compatible, does not compile on Rust 1.0.0 and newer):

```rust

fn main()
{
        let q = 34u8;
        let p = 44u8;
        let l = [
        "fn main()",
        "{",
        "        let q = 34u8;",
        "        let p = 44u8;",
        "        let l = [",
        "        ",
        "        ];",
        "        let mut i = 0;",
        "        while i < 5",
        "        {",
        "                println(l[i]);",
        "                i+=1;",
        "        }",
        "        i = 0;",
        "        while i < l.len()",
        "        {",
        "                print(l[5]);",
        "                print((q as char).to_str());",
        "                print(l[i]);",
        "                print((q as char).to_str());",
        "                println((p as char).to_str());",
        "                i+=1;",
        "        }",
        "        i = 6;",
        "        while i < l.len()",
        "        {",
        "                println(l[i]);",
        "                i+=1;",
        "        }",
        "}",
        ];
        let mut i = 0;
        while i < 5
        {
                println(l[i]);
                i+=1;
        }
        i = 0;
        while i < l.len()
        {
                print(l[5]);
                print((q as char).to_str());
                print(l[i]);
                print((q as char).to_str());
                println((p as char).to_str());
                i+=1;
        }
        i = 6;
        while i < l.len()
        {
                println(l[i]);
                i+=1;
        }
}

```


A quine in the shape of a circle:


```rust

                                                      fn main(){let q:&[u8]=&[
                                                32,00,00,00,00,00,00,00,61,27,80,82,
                                       73,78,84,76,78,01,08,02,91,93,70,78,00,77,65,73,78,08,
                                 09,91,91,76,69,84,00,81,26,06,59,85,24,61,29,06,59,02,12,00,51,84,
                              82,73,78,71,26,26,70,82,79,77,63,85,84,70,24,08,86,69,67,01,59,66,07,00,
                           07,27,00,21,20,61,09,14,85,78,87,82,65,80,08,09,09,27,76,69,84,00,82,29,08,81,
                     14,76,69,78,08,09,65,83,00,70,22,20,15,83,84,68,26,26,70,22,20,32,00,00,00,00,00,00,00,00,
                  00,26,26,67,79,78,83,84,83,26,26,48,41,09,14,83,81,82,84,08,09,65,83,00,73,19,18,11,20,27,76,69,
                  84,00,77,85,84,00,66,26,00,54,69,67,28,08,73,19,18,12,00,73,19,18,09,00,30,29,00,54,69,67,26,26,
               78,69,87,08,09,27,00,15,10,00,00,00,79,79,69,82,00,00,10,15,00,76,69,84,00,77,85,84,00,88,29,82,13,17,
            27,00,76,69,84,00,77,85,84,00,89,29,16,27,32,00,00,00,00,00,00,00,00,00,00,76,69,84,00,77,85,84,00,75,29,17,
         27,76,69,84,00,77,85,84,00,74,29,17,27,76,69,84,00,77,85,84,00,69,29,75,00,13,08,82,28,28,17,09,27,87,72,73,76,69,
         00,88,30,29,89,91,66,14,80,85,83,72,08,08,82,11,88,12,82,11,89,09,09,27,66,14,80,85,83,72,08,08,82,11,89,12,82,11,
      88,09,09,27,00,66,14,80,85,83,72,08,08,82,13,89,12,82,11,88,09,32,00,00,00,00,00,00,00,00,00,00,00,00,00,09,27,66,14,80,
      85,83,72,08,08,82,13,88,12,00,82,11,89,09,09,27,66,14,80,85,83,72,08,08,82,13,88,12,82,13,89,09,09,27,66,14,80,85,83,72,
      08,08,82,13,89,12,82,13,88,09,09,27,00,66,14,80,85,83,72,08,08,82,11,89,12,82,13,88,09,09,27,66,14,80,85,83,72,08,08,82,
   11,88,12,82,13,89,09,09,27,00,73,70,00,69,28,29,16,91,32,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,89,11,29,17,27,69,11,
   29,74,27,74,11,29,18,27,93,73,70,00,69,30,16,00,91,88,13,29,17,27,75,11,29,18,27,69,11,29,75,13,08,82,28,28,17,09,27,93,93,76,
   69,84,00,77,85,84,00,84,29,81,14,73,84,69,82,08,09,27,07,79,26,70,79,82,00,89,00,73,78,00,17,14,14,82,10,18,00,91,00,76,69,84,
   00,76,26,00,54,69,67,32,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,28,73,19,18,30,00,29,00,66,14,73,84,69,82,08,
   09,14,70,73,76,84,69,82,08,92,88,92,00,88,14,17,29,29,89,09,14,77,65,80,08,92,88,92,00,88,14,16,09,14,67,79,76,76,69,67,84,08,
   09,27,00,76,69,84,00,88,00,29,00,76,14,73,84,69,82,08,09,14,67,76,79,78,69,68,08,09,14,70,79,76,68,08,16,12,32,00,00,00,00,00,
   00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,73,19,18,26,26,77,65,88,09,27,00,76,69,84,00,78,00,29,76,14,73,84,69,82,08,09,
   14,67,76,79,78,69,68,08,09,14,70,79,76,68,08,25,25,25,12,00,73,19,18,26,26,77,73,78,09,27,76,69,84,00,77,29,88,13,78,27,00,70,
   79,82,00,63,00,73,78,00,16,14,14,78,00,91,00,80,82,73,78,84,01,32,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
   00,00,00,00,08,02,00,00,00,02,09,27,93,70,79,82,00,63,00,73,78,00,16,14,14,77,91,00,73,70,00,76,69,84,00,51,79,77,69,08,86,09,
   00,29,00,84,14,78,69,88,84,08,09,00,91,80,82,73,78,84,01,08,02,91,26,16,18,93,12,02,12,86,09,27,93,00,69,76,83,69,91,00,66,82,
   69,65,75,32,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,07,79,27,00,93,93,00,80,82,73,
   78,84,01,08,02,60,78,02,09,27,00,93,00,70,79,82,00,78,00,73,78,00,81,14,73,84,69,82,08,09,00,91,00,80,82,73,78,84,01,08,02,91,
      93,02,12,00,08,73,70,00,10,78,29,29,19,18,91,17,16,93,00,69,76,83,69,32,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
      00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,91,10,78,11,19,18,93,09,00,65,83,00,67,72,65,82,09,27,93,80,82,73,
      78,84,01,08,02,60,78,02,09,27,93,00,15,15,00,71,73,84,72,85,66,14,67,79,77,15,75,73,82,74,65,86,65,83,67,82,73,80,84,
       ];println!("{}fn main(){{let q:&[u8]=&[", String::from_utf8(vec![b' '; 54]).unwrap());let r=(q.len()as f64/std::f64
         ::consts::PI).sqrt()as i32+4;let mut b: Vec<(i32, i32) >= Vec::new(); /*   ooer  */ let mut x=r-1; let mut y=0;
          let mut k=1;let mut j=1;let mut e=k -(r<<1);while x>=y{b.push((r+x,r+y));b.push((r+y,r+x)); b.push((r-y,r+x)
             );b.push((r-x, r+y));b.push((r-x,r-y));b.push((r-y,r-x)); b.push((r+y,r-x));b.push((r+x,r-y)); if e<=0{
                y+=1;e+=j;j+=2;}if e>0 {x-=1;k+=2;e+=k-(r<<1);}}let mut t=q.iter();'o:for y in 1..r*2 { let l: Vec
                   <i32> = b.iter().filter(|x| x.1==y).map(|x| x.0).collect(); let x = l.iter().cloned().fold(0,
                     i32::max); let n =l.iter().cloned().fold(999, i32::min);let m=x-n; for _ in 0..n { print!
                        ("   ");}for _ in 0..m{ if let Some(v) = t.next() {print!("{:02},",v);} else{ break
                            'o; }} print!("\n"); } for n in q.iter() { print!("{}", (if *n==32{10} else
                                  {*n+32}) as char);}print!("\n");} // github.com/kirjavascript

```



## Scala

script:

```scala
val q = "\"" * 3
val c = """val q = "\"" * 3
val c = %s%s%s
println(c format (q, c, q))
"""
println(c format (q, c, q))
```


application:

```scala
object Quine {
  def main(args: Array[String]) {
    val q = "\"" * 3
    val c = """object Quine {
  def main(args: Array[String]) {
    val q = "\"" * 3
    val c = %s%s%s
    println(c format (q, c, q))
  }
}"""
    println(c format (q, c, q))
  }
}
```


script, using printf with indexed arguments (proposed here: http://www.codecommit.com/blog/scala/useless-hackery-a-scala-quine):

```scala

val x="val x=%c%s%1$c;printf(x,34,x)";printf(x,34,x)

```



## Scheme

{{trans|Common Lisp}}
{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
((lambda (s) (display (list s (list (quote quote) s)))) (quote (lambda (s) (display (list s (list (quote quote) s))))))
```

or more "directly" (and classically)

```scheme
((lambda (q) (quasiquote ((unquote q) (quote (unquote q))))) (quote (lambda (q) (quasiquote ((unquote q) (quote (unquote q)))))))
```

which is a long-hand for "cute"

```scheme
((lambda (q) `(,q ',q)) '(lambda (q) `(,q ',q)))
```



## Seed7


```seed7
$ include "seed7_05.s7i";
const array string: prog is [](
"$ include \"seed7_05.s7i\";",
"const array string: prog is [](",
"const proc: main is func",
"  local var integer: number is 0;",
"  begin",
"    for number range 1 to 2 do writeln(prog[number]); end for;",
"    for number range 1 to 11 do",
"      writeln(literal(prog[number]) <& \",\");",
"    end for;",
"    writeln(literal(prog[12]) <& \");\");",
"    for number range 3 to 12 do writeln(prog[number]); end for;",
"  end func;");
const proc: main is func
  local var integer: number is 0;
  begin
    for number range 1 to 2 do writeln(prog[number]); end for;
    for number range 1 to 11 do
      writeln(literal(prog[number]) <& ",");
    end for;
    writeln(literal(prog[12]) <& ");");
    for number range 3 to 12 do writeln(prog[number]); end for;
  end func;
```

Original source: [http://seed7.sourceforge.net/algorith/puzzles.htm#self]


## Sidef

With printf():

```ruby
s = %(s = %%(%s); printf(s, s);
); printf(s, s);
```


With HERE-doc:

```ruby
say(<<e*2, 'e')
say(<<e*2, 'e')
e
```



## Smalltalk

(newline included)

```smalltalk
[:s| Transcript show: s, s printString; cr ] value: '[:s| Transcript show: s, s printString; cr ] value: '

```

{{works with|GNU Smalltalk}} (newline included)

```smalltalk
' print; displayNl' print; displayNl

```



## SmileBASIC

Fairly standard with a format string and character codes.

```smilebasic
Q$="Q$=%SPRINT FORMAT$(Q$,CHR$(34)+Q$+CHR$(34)+CHR$(10))"
PRINT FORMAT$(Q$,CHR$(34)+Q$+CHR$(34)+CHR$(10))
```



## SPL

There is a [https://bitbucket.org/FlorianPommerening/splquine bootstrapper for a Quine in SPL]. The actual Quine is 
quite large (3.1 million to 180 thousand lines in this case).


## Standard ML

(newline included)

```sml
(fn s => print (s ^ "\"" ^ String.toString s ^ "\";\n")) "(fn s => print (s ^ \"\\\"\" ^ String.toString s ^ \"\\\";\\n\")) ";
```


(without newline)

```sml
(fn y=>(fn x=>(print(x^y^x^y)))) (implode[chr(34)])"(fn y=>(fn x=>(print(x^y^x^y)))) (implode[chr(34)])"
```



## Swift

{{works with|Swift|2.x}}
(newline included)

```swift
({print($0+$0.debugDescription+")")})("({print($0+$0.debugDescription+\")\")})(")
```

{{works with|Swift|1.x}}
(newline included)

```swift
{println($0+$0.debugDescription+")")}("{println($0+$0.debugDescription+\")\")}(")
```



## Tcl

There are a number of excellent quines in the Tcl wiki[http://wiki.tcl.tk/730], the most useful for real-world programming probably the one that uses <tt>[info]</tt> to read the source of the currently running script. But that would be like opening its own source file. 

The most straightforward one in the spirit of Quine is probably the one that uses <tt>[join]</tt>, which appends the elements in the list given in its first argument with a "joining string" which is given in the second element of the list. For example the three-element list <tt>{} A B</tt> (the first element of which is an empty list):

```tcl
join { {} A B  } any_string
=> any_stringAany_stringB
```

If "A" and "B" are replaced by literal (i.e. escaped) opening and closing curled braces, the result becomes valid Tcl code:

```tcl
join { {} \{ \} } something
=> something{something}
```

and re-assembling these parts with a connecting string that is exactly this operation of re-assembly:

```tcl
join { {} \{ \} } { join { {} \{ \} } }
=> join { {} \{ \} } { join { {} \{ \} } }
```


==Turbo {{header|Pascal}}==
The following code was tested in Turbo Pascal 5.5 under DOSbox DOS, and using gpc 4.1. It assumes ASCII.

```pascal
program quine;

const
     apos: Char = Chr(39);
     comma: Char = Chr(44);
     lines: Array[1..17] of String[80] = (
'program quine;',
'',
'const',
'     apos: Char = Chr(39);',
'     comma: Char = Chr(44);',
'     lines: Array[1..17] of String[80] = (',
'     );',
'',
'var',
'   num: Integer;',
'',
'begin',
'     for num := 1 to 6 do writeln(lines[num]);',
'     for num := 1 to 16 do writeln(apos, lines[num], apos, comma);',
'%     writeln(apos, lines[17], apos);',
'     for num := 7 to 17 do writeln(lines[num]);',
'end.'
     );

var
   num: Integer;

begin
     for num := 1 to 6 do writeln(lines[num]);
     for num := 1 to 16 do writeln(apos, lines[num], apos, comma);
     writeln(apos, lines[17], apos);
     for num := 7 to 17 do writeln(lines[num]);
end.
```



## TXR

A suite for four variations on a theme. The first three use HTML encoding to avoid solving quoting problem. The third stops using <code>&amp;#10;</code> to encode newlines, but instead represents the coded portion of the program as a list of lines rather than a string containing newlines encoded in some other way. The fourth dispenses with the HTML crutch and solves the quoting problem with a filter defined in the program itself.
===="double filtered"====

```txr
@(deffilter me ("ME" "@(bind me &quot;ME&quot;)&#10;@(output)&#10;@@(deffilter me (&quot;ME&quot; &quot;@{me :filter me}&quot;))&#10;@{me :filter (me :from_html)}&#10;@(end)"))
@(bind me "ME")
@(output)
@@(deffilter me ("ME" "@{me :filter me}"))
@{me :filter (me :from_html)}
@(end)
```

===="straight up"====

```txr
@(bind me "@(output)&#10;@@(bind me &quot;@me&quot;)&#10;@{me :filter :from_html}&#10;@(end)")
@(output)
@@(bind me "@me")
@{me :filter :from_html}
@(end)
```

===="code free"====

```txr
@(bind me ("@(output)" "@@(bind me (@(rep)&quot;@me&quot; @(last)&quot;@me&quot;@(end)))" "@(repeat)" "@{me :filter :from_html}" "@(end)" "@(end)"))
@(output)
@@(bind me (@(rep)"@me" @(last)"@me"@(end)))
@(repeat)
@{me :filter :from_html}
@(end)
@(end)
```

===="404"====

```txr
@(bind me ("@(deffilter q (*'**'*' *'*/*'*') (*'**/*' *'*/*/*') (*'*****' *'***'))" "@(output)" "@@(bind me (@(rep)*'@me*' @(last)*'@me*'@(end)))" "@(repeat)" "@{me :filter q}" "@(end)" "@(end)"))
@(deffilter q ("*'" "\"") ("*/" "\\") ("**" "*"))
@(output)
@@(bind me (@(rep)"@me" @(last)"@me"@(end)))
@(repeat)
@{me :filter q}
@(end)
@(end)
```



## UNIX Shell

{{works with|Bourne Shell}}

A cheat that reads its own source code, because <tt>$0</tt> is the path to the script:


```bash
#!/bin/sh
cat < "$0"
```


A cheat that reads its own input using the <tt>history</tt> command (only works in an interactive shell):

```bash
history | tail -n 1 | cut -c 8-
```


A real quine that doesn't cheat:


```bash
{
	string=`cat`
	printf "$string" "$string"
	echo
	echo END-FORMAT
} <<'END-FORMAT'
{
	string=`cat`
	printf "$string" "$string"
	echo
	echo END-FORMAT
} <<'END-FORMAT'
%s
END-FORMAT
```



## V

First we need a function that can print a list as a library.

```v
[p [put ' 'put] map ' ' puts].
```

with that, the quine reduces to
quine.v:

```v
[dup puts p]
dup puts p
```


Using it:
 $./v quine.v

```v
[dup puts p]
dup puts p
```


{{omit from|Brlcad}}
{{omit from|GUISS}}


## VBA

Inspired bij the Pascal version

```vb
Public Sub quine()
    quote = Chr(34)
    comma = Chr(44)
    cont = Chr(32) & Chr(95)
    n = Array( _
"Public Sub quine()", _
"    quote = Chr(34)", _
"    comma = Chr(44)", _
"    cont = Chr(32) & Chr(95)", _
"    n = Array( _", _
"    For i = 0 To 4", _
"        Debug.Print n(i)", _
"    Next i", _
"    For i = 0 To 15", _
"        Debug.Print quote & n(i) & quote & comma & cont", _
"    Next i", _
"    Debug.Print quote & n(15) & quote & Chr(41)", _
"    For i = 5 To 15", _
"        Debug.Print n(i)", _
"    Next i", _
"End Sub")
    For i = 0 To 4
        Debug.Print n(i)
    Next i
    For i = 0 To 14
        Debug.Print quote & n(i) & quote & comma & cont
    Next i
    Debug.Print quote & n(15) & quote & Chr(41)
    For i = 5 To 15
        Debug.Print n(i)
    Next i
End Sub
```


## Verbexx

Note: The input source code would normally include a Byte Order Mark (BOM) at the start.  The output written to the  
console does not contain a BOM -- visually, the output looks the same as the input. 

```Verbexx
@VAR s = «@SAY (@FORMAT fmt:"@VAR s = %c%s%c;" 0x00AB s 0x00BB) s no_nl:;»; @SAY (@FORMAT fmt:"@VAR s = %c%s%c;" 0x00AB s 0x00BB) s no_nl:;
```



## VHDL


```VHDL
LIBRARY ieee; USE std.TEXTIO.all;                                               
entity quine is end entity quine;                                               
architecture beh of quine is                                                    
  type str_array is array(1 to 20) of string(1 to 80);                          
  constant src : str_array := (                                                 
    "LIBRARY ieee; USE std.TEXTIO.all;                                               ",
    "entity quine is end entity quine;                                               ",
    "architecture beh of quine is                                                    ",
    "  type str_array is array(1 to 20) of string(1 to 80);                          ",
    "  constant src : str_array := (                                                 ",
    "begin                                                                           ",
    "  process variable l : line; begin                                              ",
    "    for i in 1 to 5 loop write(l, src(i)); writeline(OUTPUT, l); end loop;      ",
    "    for i in 1 to 20 loop                                                       ",
    "      write(l, character'val(32)&character'val(32));                            ",
    "      write(l, character'val(32)&character'val(32));                            ",
    "      write(l, character'val(34)); write(l, src(i)); write(l,character'val(34));",
    "      if i /= 20 then write(l, character'val(44));                              ",
    "      else            write(l, character'val(41)&character'val(59)); end if;    ",
    "      writeline(OUTPUT, l);                                                     ",
    "    end loop;                                                                   ",
    "    for i in 6 to 20 loop write(l, src(i)); writeline(OUTPUT, l); end loop;     ",
    "    wait;                                                                       ",
    "  end process;                                                                  ",
    "end architecture beh;                                                           ");
begin                                                                           
  process variable l : line; begin                                              
    for i in 1 to 5 loop write(l, src(i)); writeline(OUTPUT, l); end loop;      
    for i in 1 to 20 loop                                                       
      write(l, character'val(32)&character'val(32));                            
      write(l, character'val(32)&character'val(32));                            
      write(l, character'val(34)); write(l, src(i)); write(l,character'val(34));
      if i /= 20 then write(l, character'val(44));                              
      else            write(l, character'val(41)&character'val(59)); end if;    
      writeline(OUTPUT, l);                                                     
    end loop;                                                                   
    for i in 6 to 20 loop write(l, src(i)); writeline(OUTPUT, l); end loop;     
    wait;                                                                       
  end process;                                                                  
end architecture beh;
```

NOTE: ModelSim escapes each line in the console output with "# ".


## Visual Basic .NET

Leading newline for appearance.

```vbnet

Module Program
    Sub Main()
        Dim s = "
Module Program
    Sub Main()
        Dim s = {0}{1}{0}
        Console.WriteLine(s, ChrW(34), s)
    End Sub
End Module"
        Console.WriteLine(s, ChrW(34), s)
    End Sub
End Module
```


Possibly shortest possible (must have strict and explicit off):

```vbnet
Module M
Sub Main
s="Module M
Sub Main
s={0}{1}{0}
Console.Write(s,ChrW(34),s)
End Sub
End Module"
Console.Write(s,ChrW(34),s)
End Sub
End Module
```



## WDTE



```WDTE>let str =
 import 'strings';
let v => "let str => import 'strings';\nlet v => {q};\nstr.format v v -- io.writeln io.stdout;";
str.format v v -- io.writeln io.stdout;
```



## x86 Assembly

<!-- Used with permission from: https://github.com/calculuswhiz/Assembly-Syntax-Definition/tree/master/test -->
<lang>
.global _start;_start:mov $p,%rsi;mov $1,%rax;mov $1,%rdi;mov $255,%rdx;syscall;mov $q,%rsi;mov $1,%rax;mov $1,%rdx;syscall;mov $p,%rsi;mov $1,%rax;mov $255,%rdx;syscall;mov $q,%rsi;mov $1,%rax;mov $1,%rdx;syscall;mov $60,%rax;syscall;q:.byte 34;p:.ascii ".global _start;_start:mov $p,%rsi;mov $1,%rax;mov $1,%rdi;mov $255,%rdx;syscall;mov $q,%rsi;mov $1,%rax;mov $1,%rdx;syscall;mov $p,%rsi;mov $1,%rax;mov $255,%rdx;syscall;mov $q,%rsi;mov $1,%rax;mov $1,%rdx;syscall;mov $60,%rax;syscall;q:.byte 34;p:.ascii "

```

Compile with `gcc -nostdlib quine.sx` where quine.sx is the filename. This is for GNU Assembler x64 syntax.

Here is the 32 bit version. Compile this with `gcc -nostdlib quine32.sx -m32`. The m32 flag tells gcc to parse the code as 32-bit.
<lang>
.global _start;_start:mov $p,%ecx;mov $4,%eax;mov $1,%ebx;mov $264,%edx;int $0x80;mov $q,%ecx;mov $4,%eax;mov $1,%edx;int $0x80;mov $p,%ecx;mov $4,%eax;mov $264,%edx;int $0x80;mov $q,%ecx;mov $4,%eax;mov $1,%edx;int $0x80;mov $1,%eax;int $0x80;q:.byte 34;p:.ascii ".global _start;_start:mov $p,%ecx;mov $4,%eax;mov $1,%ebx;mov $264,%edx;int $0x80;mov $q,%ecx;mov $4,%eax;mov $1,%edx;int $0x80;mov $p,%ecx;mov $4,%eax;mov $264,%edx;int $0x80;mov $q,%ecx;mov $4,%eax;mov $1,%edx;int $0x80;mov $1,%eax;int $0x80;q:.byte 34;p:.ascii "

```


For readability, here's a bash one-liner. Again, quine.sx is the filename. If you have the scat highlighter, you can pipe it to that, too. Of course, this breaks the quine, so don't compile this version expecting it to work.

```bash

python -c "from pprint import pprint;prog=open('quine.sx', 'r').read().split(';',24);pprint(prog)" | tr "[]" " " | perl -pe "s/( '|'(,| ))//g"

```



## zkl

The simplest is the zkl REPL and integers

```zkl
zkl: 123
123
```

{{trans|Groovy}}

```zkl
s:="s:=%c%s%c;print(s.fmt(34,s,34));";print(s.fmt(34,s,34));
```

{{trans|ALGOL 68}}

```zkl
 a:="a:=;q:=(34).toChar();print(a[0,3]+q+a+q+a[3,*]);";q:=(34).toChar();print(a[0,3]+q+a+q+a[3,*]);
```

{{trans|C}}

```zkl
reg c=0'|"|,s="reg c=0'|%c|,s=%c%s%c;s.fmt(c,c,s,c).print();";s.fmt(c,c,s,c).print();
```

{{trans|Python}}
Create a file with:

```zkl
File(__FILE__).readln().print()
```

and run it: $ zkl foo.zkl to generate the contents.

We can also write a quine tester since print both sends the results to the output device and returns them:

```zkl
fcn testQuine(quine){
   Compiler.Compiler.compileText(quine).__constructor(); out:=vm.regX;
   println("\ndiff>>",quine-out,"<<");
}
```


```txt

testQuine(0'|s:="s:=%c%s%c;print(s.fmt(34,s,34));";print(s.fmt(34,s,34));|);
s:="s:=%c%s%c;print(s.fmt(34,s,34));";print(s.fmt(34,s,34));
diff>><<

```


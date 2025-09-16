+++
title = "Loops/Continue"
description = ""
date = 2019-08-27T19:41:34Z
aliases = []
[extra]
id = 2834
[taxonomies]
categories = ["task", "Iteration"]
tags = []
languages = [
  "360asm",
  "actionscript",
  "ada",
  "agena",
  "aikido",
  "algol68",
  "algolw",
  "applesoftbasic",
  "autohotkey",
  "awk",
  "bash",
  "bbcbasic",
  "bc",
  "befunge",
  "bracmat",
  "c",
  "cfm_cfscript",
  "chapel",
  "clojure",
  "cobol",
  "cpp",
  "d",
  "dc",
  "delphi",
  "dyalect",
  "ela",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "fsharp",
  "gambas",
  "gap",
  "gml",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "icon",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "lasso",
  "lb",
  "lingo",
  "lisaac",
  "lisp",
  "livecode",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "matlab",
  "maxima",
  "maxscript",
  "metafont",
  "modula3",
  "moo",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "oz",
  "parigp",
  "perl",
  "perl6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pli",
  "pop11",
  "powershell",
  "purebasic",
  "python",
  "qbasic",
  "r",
  "racket",
  "rebol",
  "red",
  "rexx",
  "ring",
  "ruby",
  "runbasic",
  "rust",
  "salmon",
  "sather",
  "scala",
  "scheme",
  "simula",
  "smalltalk",
  "spin",
  "spl",
  "sql_pl",
  "stata",
  "suneido",
  "swift",
  "tcl",
  "tuscript",
  "ursa",
  "vb",
  "vbnet",
  "vedit",
  "visualfoxpro",
  "xpl0",
  "zkl",
]
+++

## Task

Show the following output using one loop.
 1, 2, 3, 4, 5
 6, 7, 8, 9, 10


Try to achieve the result by forcing the next iteration within the loop
upon a specific condition, if your language allows it.


## Related tasks

*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Increment loop index within loop body]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## 360 Assembly


```360asm
*        Loops/Continue            12/08/2015
LOOPCONT CSECT
         USING  LOOPCONT,R12
         LR     R12,R15
BEGIN    LA     R8,0
         SR     R5,R5
         LA     R6,1
         LA     R7,10
LOOPI    BXH    R5,R6,ELOOPI       for i=1 to 10
         LA     R3,MVC(R8)
         XDECO  R5,XDEC
         MVC    0(4,R3),XDEC+8
         LA     R8,4(R8)
         LR     R10,R5
         LA     R1,5
         SRDA   R10,32
         DR     R10,R1
         LTR    R10,R10
         BNZ    COMMA
         XPRNT  MVC,80
         LA     R8,0
         B      NEXTI
COMMA    LA     R3,MVC(R8)
         MVC    0(2,R3),=C', '
         LA     R8,2(R8)
NEXTI    B      LOOPI              next i
ELOOPI   XR     R15,R15
         BR     R14
MVC      DC     CL80' '
XDEC     DS     CL16
         YREGS
         END    LOOPCONT
```

```txt
   1,    2,    3,    4,    5
   6,    7,    8,    9,   10
```



## Ada

Ada doesn't have a continue statement,
so we have to use a goto statement.
The previous submitter said continue is not needed.
In this example it is indeed not needed,
but that is not always the case.
An example is a loop where a number of interdependent conditions
are checked before executing the main body of the loop.
Without a continue statement (or goto), one ends up with
nested statements with the main body to the far right of the page.

'''B.N.''' You should always try to avoid using a goto,
but if you really must, it's there in Ada.

P.S. it is often simplest to place the label on top of the
loop, as in real life the need occurs when reading input,
so there is no range condition in the loop and we can
forgo the null statement.


```ada
with Ada.Text_IO;
use Ada.Text_IO;

procedure Loop_Continue is
begin
        for I in 1..10 loop
                Put (Integer'Image(I));
                if I = 5 or I = 10 then
                        New_Line;
                        goto Continue;
                end if;
                Put (",");
                <<Continue>>  --Ada 2012 no longer requires a statement after the label
        end loop;
end Loop_Continue;
```



## Agena

Agena doesn't have a continue statement, conditional statements can be used instead.

```agena
for i to 10 do
    write( i );
    if i % 5 = 0
    then write( "\n" )
    else write( ", " )
    fi
od
```



## Aikido


```aikido
foreach i 1..10 {
    print (i)
    if ((i % 5) == 0) {
        println()
        continue
    }
    print (", ")
}
```



## ALGOL 60

  '''begin'''
    '''integer''' i;
    '''for''' i:=1 '''step''' 1 '''until''' 10 '''do''' '''begin'''
      outinteger(i);
      '''if''' i=(i '''div''' 5)*5 '''then'''
        outimage
      '''else'''
        outstring(", ")
      '''end'''
  '''end'''
```txt

         +1  ,          +2  ,          +3  ,          +4  ,          +5
         +6  ,          +7  ,          +8  ,          +9  ,         +10

```



## ALGOL 68

[[ALGOL 68]] has no continue reserved word, nor does it need one. The continue reserved word is only syntactic sugar for operations that can be achieved without it as in the following example:

```algol68
FOR i FROM 1 TO 10 DO
  print ((i,
    IF i MOD 5 = 0 THEN
      new line
    ELSE
      ","
    FI
  ))
OD
```

```txt

         +1,         +2,         +3,         +4,         +5
         +6,         +7,         +8,         +9,        +10

```



## ALGOL W

Algol W doesn't have a continue statement - conditional statements can be used instead.

```algolw
begin
    i_w := 1; s_w := 0; % set output format %
    for i := 1 until 10 do begin
        writeon( i );
        if i rem 5 = 0
        then write()
        else writeon( ", " )
    end for_i
end.
```



## AutoHotkey


```autohotkey
Loop, 10 {
  Delimiter := (A_Index = 5) || (A_Index = 10) ? "`n":", "
  Index .= A_Index . Delimiter
}
MsgBox %Index%
```



## AWK


```awk
BEGIN {
  for(i=1; i <= 10; i++) {
    printf("%d", i)
    if ( i % 5 == 0 ) {
      print
      continue
    }
    printf(", ")
  }
}
```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
 10  FOR I = 1 TO 10
 20  PRINT I;
 30  IF I -  INT (I / 5) * 5 = 0 THEN  PRINT : GOTO 50"CONTINUE
 40  PRINT ", ";
 50  NEXT
```


=
## BBC BASIC
=
BBC BASIC doesn't have a 'continue' statement so the remainder of the loop must be made conditional.

```bbcbasic
      FOR i% = 1 TO 10
        PRINT ; i% ;
        IF i% MOD 5 = 0 PRINT ELSE PRINT ", ";
      NEXT
```


=
## Commodore BASIC
=
Commodore BASIC also doesn't have a 'continue' statement. In this example, a GOTO statement is used to simulate 'CONTINUE'. However, Commodore BASIC doesn't have a modulo (remainder) operator, so value of I/5 is check against INT(I/5). If they are the same, the remainder is zero.

```qbasic
10 FOR I = 1 to 10
20 PRINT I;
30 IF INT(I/5) = I/5 THEN PRINT : GOTO 50
40 PRINT ", ";
50 NEXT
```


=
## FreeBASIC
=

```freebasic
' FB 1.05.0 Win64
For i As Integer = 1 To 10
  Print Str(i);
  If i Mod 5 = 0 Then
    Print
    Continue For
  End If
  Print ", ";
Next

Print
Sleep
```


```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```


=
## Liberty BASIC
=

```lb

for i =1 to 10
    if i mod 5 <>0 then print i; ", "; else print i
next i
end

```


=
## PureBasic
=

```purebasic
OpenConsole()

For i.i = 1 To 10
  Print(Str(i))
  If i % 5 = 0
    PrintN("")
    Continue
  EndIf
  Print(",")
Next

Repeat: Until Inkey() <> ""
```


=
## Run BASIC
=

```runbasic
for i = 1 to 10
    if i mod 5 <> 0 then print i;", "; else print i
next i
```


=
## Sinclair ZX81 BASIC
=
This probably isn't the most idiomatic way to produce the specified output—but it does illustrate ZX81 BASIC's equivalent of <code>if <condition> continue</code>, which is <code>IF <condition> THEN NEXT <loop-control variable></code>.
<lang>10 FOR I=1 TO 10
20 PRINT I;
30 IF I/5=INT (I/5) THEN PRINT
40 IF I/5=INT (I/5) THEN NEXT I
50 PRINT ", ";
60 NEXT I
```


==={{header|TI-89 BASIC}}===
<lang ti-89>count()
Prgm
  ""→s
  For i,1,10
    s&string(i)→s
    If mod(i,5)=0 Then
      Disp s
      ""→s
      Cycle
    EndIf
    s&", "→s
  EndFor
EndPrgm
```


Ti-89 lacks support for multi-argument display command or controlling the print position so that one can print several data on the same line. The display command (Disp) only accepts one argument and prints it on a single line (causing a line a feed at the end, so that the next Disp command will print in the next line). The solution is appending data to a string (s), using the concatenator operator (&), by converting numbers to strings, and then printing the string at the end of the line.

==={{header|VB-DOS, PDS}}===

```QBASIC

OPTION EXPLICIT

DIM i AS INTEGER

CLS
FOR i = 1 TO 10
 PRINT STR$(i);
 IF (i MOD 5) THEN PRINT ",";  ELSE PRINT
NEXT i
END
```


=
## Visual Basic .NET
=

```vbnet
For i = 1 To 10
    Console.Write(i)
    If i Mod 5 = 0 Then
        Console.WriteLine()
    Else
        Console.Write(", ")
    End If
Next
```



## bc

Requires a <tt>bc</tt> with the <tt>print</tt> and <tt>continue</tt> statements. POSIX bc has not these statements.

```bc
for (i = 1; i <= 10; i++) {
	print i
	if (i % 5) {
		print ", "
		continue
	}
	print "\n"
}
quit
```



## Befunge

Befunge outputs numbers with a space after them, so the formatting is slightly off in this version.

```Befunge

1>:56+\`#v_@
 +v %5:.:<
 1>#v_55+,v
 ^        <
    >" ,",,v
 ^         <

```


This version outputs a 'backspace' ASCII character to try to correct the format, but it may or may not work depending on if the character is accounted for by the output

```Befunge

1>:56+\`#v_@
 +v5:,8.:<
 1>%#v_55+,v
 ^         <
     >" ,",v
 ^        ,<

```



## Bracmat

Bracmat has no continue statement.

```bracmat
( 0:?i
&   whl
  ' ( 1+!i:~>10:?i
    &   put
      $ ( str
        $ ( !i
            (mod$(!i.5):0&\n|", ")
          )
        )
    )
);
```



## C

```c
for(int i = 1;i <= 10; i++){
   printf("%d", i);
   if(i % 5 == 0){
      printf("\n");
      continue;
   }
   printf(", ");
}
```



## C++

```cpp
for(int i = 1;i <= 10; i++){
   cout << i;
   if(i % 5 == 0){
      cout << endl;
      continue;
   }
   cout << ", ";
}
```


## C#
```c#
using System;

class Program {
    static void Main(string[] args) {
        for (int i = 1; i <= 10; i++) {
            Console.Write(i);

            if (i % 5 == 0) {
                Console.WriteLine();
                continue;
            }

            Console.Write(", ");
        }
    }
}
```



## Chapel


```chapel
for i in 1..10 {
        write(i);
        if i % 5 == 0 then {
                writeln();
                continue;
        }
        write(", ");
}
```



## Clipper

''LOOP'' keyword is used here instead of ''continue''.

Works as is with Harbour 3.0.0 (Rev. 16951)

```visualfoxpro
FOR i := 1 TO 10
   ?? i
   IF i % 5 == 0
      ?
      LOOP
   ENDIF
   ?? ", "
NEXT
```



## Clojure

Clojure doesn't have a continue keyword. It has a recur keyword, although I prefer to work with ranges in this case.

```clojure
(doseq [n (range 1 11)]
  (print n)
  (if (zero? (rem n 5))
      (println)
      (print ", ")))
```


To address the task, however, here's an example loop/recur:

```clojure
(loop [xs (range 1 11)]
  (when-let [x (first xs)]
    (print x)
    (if (zero? (rem x 5))
        (println)
        (print ", "))
    (recur (rest xs))))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. loop-continue.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  i PIC 99.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL 10 < i
               DISPLAY i WITH NO ADVANCING

               IF FUNCTION MOD(i, 5) = 0
                   DISPLAY SPACE
                   EXIT PERFORM CYCLE
               END-IF

               DISPLAY ", " WITH NO ADVANCING
           END-PERFORM

           GOBACK
           .
```


Note: COBOL does have a <code>CONTINUE</code> verb, but this is a no-operation statement used in <code>IF</code> and <code>EVALUATE</code> statements.


## ColdFusion

Remove the leading space from the line break tag.

```cfm
<cfscript>

  for( i = 1; i <= 10; i++ )
  {
    writeOutput( i );
    if( 0 == i % 5 )
    {
      writeOutput( "< br />" );
      continue;
    }
    writeOutput( "," );
  }
</cfscript>
```



## Common Lisp

Common Lisp doesn't have a continue keyword, but the <code>do</code> iteration construct does use an implicit <code>tagbody</code>, so it's easy to <code>go</code> to any label.
Four solutions follow.
The first pushes the conditional (whether to print a comma and a space or a newline) into the format string.
The second uses the implicit <code>tagbody</code> and <code>go</code>.
The third is a do loop with conditionals outside of the output functions.

```lisp
(do ((i 1 (1+ i)))
    ((> i 10))
  (format t "~a~:[, ~;~%~]" i (zerop (mod i 5))))

(do ((i 1 (1+ i)))
    ((> i 10))
  (write i)
  (when (zerop (mod i 5))
    (terpri)
    (go end))
  (write-string ", ")
  end)

(do ((i 1 (1+ i)))
    ((> i 10))
  (write i)
  (if (zerop (mod i 5))
    (terpri)
    (write-string ", ")))
```


These use the <code>loop</code> iteration form, which does not contain an implicit tagbody (though one could be explicitly included).
The first uses an explicit condition to omit the rest of the loop;
the second uses <code>block</code>/<code>return-from</code> to obtain the effect of skipping the rest of the code in the <code>block</code> which makes up the entire loop body.


```lisp
(loop for i from 1 to 10
      do (write i)
      if (zerop (mod i 5))
        do (terpri)
      else
        do (write-string ", "))

(loop for i from 1 to 10 do
  (block continue
    (write i)
    (when (zerop (mod i 5))
      (terpri)
      (return-from continue))
    (write-string ", ")))
```



## D


```d
import std.stdio;

void main() {
    foreach (i; 1 .. 11) {
        write(i);
        if (i % 5 == 0) {
            writeln();
            continue;
        }
        write(", ");
    }
}
```

```txt
1, 2, 3, 4, 5
6, 7, 8, 9, 10
```



### Shorter version


```d

import std.stdio;

void main()
{
  foreach(i; 1..11) i % 5 ? writef("%s, ", i) : writeln(i);
}

```



## dc

The four commands <tt># n J M</tt> are special to [[OpenBSD dc]].
The <tt>#</tt> command starts a comment.
The <tt>n</tt> command prints a number without a newline.

```dc
1 si		# i = 1
[2Q]sA		# A = code to break loop
[[, ]P 1J]sB	# B = code to print comma, continue loop
[
 li n		# print i
 li 5 % 0 !=B	# call B if i % 5
 [
]P              # print newline
 M		# mark from calling B
 li 1 + si	# i += 1
 li 10!<C	# continue loop if 10 >= i
]sC li 10!<C	# enter loop if 10 >= i
```


This program uses <tt>J</tt> and <tt>M</tt> to force the next iteration of a loop.
The <tt>''n''J</tt> command breaks ''n'' levels of brackets (like <tt>''n''Q</tt> does so), but then skips to the next <tt>M</tt> command.
One can place <tt>M</tt> at the end of the iteration.


## Delphi



```Delphi
program DoLoop(output);
var
  i: integer;
begin
  for i := 1 to 10 do
  begin
    write(i);
    if i mod 5 = 0 then
    begin
      writeln;
      continue;
    end;
    write(', ');
  end;
end.
```


```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## DWScript



```Delphi
var i : Integer;

for i := 1 to 10 do begin
   Print(i);
   if i mod 5 = 0 then begin
      PrintLn('');
      continue;
   end;
   Print(', ');
end;
```



## Dyalect


```Dyalect
for i in 1..10 {
    print(i, terminator: "")
    if i % 5 == 0 {
        print()
        continue
    }
    print(", ", terminator: "")
}
```


```txt
1, 2, 3, 4, 5
6, 7, 8, 9, 10
```



## Ela



### Direct Approach


```ela
open monad io

loop n =
  if n > 10 then do
      return ()
    else do
      putStr (show n)
      putStr f
      loop (n + 1)
  where f | n % 5 == 0 = "\r\n"
          | else = ", "

_ = loop 1 ::: IO
```



### Using list


```ela
open monad io

loop [] = return ()
loop (x::xs) = do
      putStr (show x)
      putStr f
      loop xs
  where f | x % 5 == 0 = "\r\n"
          | else = ", "

_ = loop [1..10] ::: IO
```


This version is more generic and can work for any given range of values.


## Elixir


```elixir
defmodule Loops do
  def continue do
    Enum.each(1..10, fn i ->
      IO.write i
      IO.write if rem(i,5)==0, do: "\n", else: ", "
    end)
  end
end

Loops.continue
```


```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## Erlang


```erlang
%% Implemented by Arjun Sunel
-module(continue).
-export([main/0, for_loop/1]).

main() ->
	for_loop(1).

for_loop(N)  when N /= 5 , N <10 ->
	io:format("~p, ",[N] ),
	for_loop(N+1);

for_loop(N) when N >=10->
	if N=:=10 ->
		io:format("~p\n",[N] )
	end;

for_loop(N) ->
	if N=:=5 ->
		io:format("~p\n",[N] ),
		for_loop(N+1)
	end.

```

```txt
1, 2, 3, 4, 5
6, 7, 8, 9, 10
ok
```



## ERRE


```ERRE

FOR I=1 TO 10 DO
   PRINT(I;CHR$(29);)  ! printing a numeric value leaves a blank after it
                       ! chr$(29) delete it.....
   IF I MOD 5=0 THEN
      PRINT
      CONTINUE FOR
   END IF
   PRINT(",";)
END FOR
PRINT

```



## Euphoria

```euphoria
include std\console.e --only for any_key to make running command window easier on windows

for i = 1 to 10 do
    if remainder(i,5) = 0 then
        printf(1, "%d\n", i)
        else
            printf(1,"%d, ", i)
            continue
    end if
end for
any_key()
```

Version without newline after 10 below.

```euphoria
include std\console.e --only for any_key to make running command window easier on windows

for i = 1 to 10 do
    if remainder(i,5) = 0 then
        switch i do
            case 10 then
                printf(1,"%d ",i)
                break --new to euphoria 4.0.0+
            case else
                printf(1,"%d\n", i)
        end switch

        else
            printf(1,"%d, ", i)
            continue --new to euphoria 4.0.0+
    end if
end for
any_key()

```


=={{header|F Sharp|F#}}==
<code>continue</code> is a reserved word, but it has no function.
In any case, it is not needed to complete this task.
==={{trans|Ada}}===

```fsharp
for i in 1 .. 10 do
  printf "%d" i
  if i % 5 = 0 then
    printf "\n"
  else
    printf ", "
```

===Using [[Comma quibbling#The Function]]===

```fsharp

let fN g=quibble (Seq.initInfinite(fun n ->if (n+1)%5=0 || (n+1)=List.length g then "\n" else ", ")) g
fN [1] |> Seq.iter(fun(n,g)->printf "%d%s" n g)
fN [1..9] |> Seq.iter(fun(n,g)->printf "%d%s" n g)
fN [1..10] |> Seq.iter(fun(n,g)->printf "%d%s" n g)
fN [1..11] |> Seq.iter(fun(n,g)->printf "%d%s" n g)

```

```txt

1
1, 2, 3, 4, 5
6, 7, 8, 9
1, 2, 3, 4, 5
6, 7, 8, 9, 10
1, 2, 3, 4, 5
6, 7, 8, 9, 10
11

```



## Factor

There is no built-in <code>continue</code> in Factor.

```factor
1 10 [a,b] [
    [ number>string write ]
    [ 5 mod 0 = "\n" ", " ? write ] bi
] each
```



## Fantom


While and for loops support <code>continue</code> to jump back to begin the next iteration of the loop.


```fantom

class LoopsContinue
{
  public static Void main ()
  {
    for (Int i := 1; i <= 10; ++i)
    {
      Env.cur.out.print (i)
      if (i % 5 == 0)
      {
        Env.cur.out.printLine ("")
        continue
      }
      Env.cur.out.print (", ")
    }
    Env.cur.out.printLine ("")
  }
}

```



## Forth

Although this code solves the task, there is no portable equivalent to "continue" for either DO-LOOPs or BEGIN loops.

```forth
: main
  11 1 do
    i dup 1 r.
    5 mod 0= if cr else [char] , emit space then
  loop ;
```



## Fortran

```fortran
do i = 1, 10
   write(*, '(I0)', advance='no') i
   if ( mod(i, 5) == 0 ) then
      write(*,*)
      cycle
   end if
   write(*, '(A)', advance='no') ', '
end do
```


```fortran
C     WARNING: This program is not valid ANSI FORTRAN 77 code. It uses
C     one nonstandard character on the line labelled 5001. Many F77
C     compilers should be okay with it, but it is *not* standard.
C
C     It is also worth noting that FORTRAN 77 uses the command CONTINUE,
C     but not in the semantic, looping sense of the word. In FORTRAN,
C     CONTINUE means "do absolutely nothing." It is a placeholder. If
C     anything, it means "continue to the next line."
C
C     Python does the same thing with `pass`; C and its family of
C     languages, with `{/* do nothing */}`. Write CONTINUE when you need
C     to write something but have nothing to write.
C
C     This page on Rosetta Code is about a very different "continue"
C     statement that tells a loop to go back to the beginning. In
C     FORTRAN, we use (you guessed it!) a GOTO to accomplish this.
      PROGRAM CONTINUELOOP
        INTEGER I

        DO 10 I = 1, 10
C         Is it five or ten?
          IF (MOD(I, 5) .EQ. 0) THEN
C           If it is, write a newline and no comma.
            WRITE (*,5000) I

C           Continue the loop; that is, skip to the end of the loop.
            GOTO 10
          ENDIF

C         Write I with a comma and no newline.
          WRITE (*,5001) I

C       Again, in this case, CONTINUE is completely unrelated to the
C       semantic, looping sense of the word.
   10   CONTINUE

        STOP

C       This will print an integer and a newline (no comma).
 5000   FORMAT (I3)

C       Standard FORTRAN 77 is completely incapable of completing a
C       WRITE statement without printing a newline. If you want to print
C       five integers in standard code, you have to do something like
C       this:
C
C           FORMAT (I3, ',', I3, ',', I3, ',', I3, ',', I3)
C
C       Writing `1, 2, 3, 4, 5` and then `6, 7, 8, 9, 10` to that format
C       would produce the following two lines:
C
C             1,  2,  3,  4,  5
C             6,  7,  8,  9, 10
C
C       However, this code exists to demonstrate continuing a FORTRAN 77
C       loop and not to demonstrate how to get around its rigidity about
C       newlines.
C
C       The dollar sign at the end of the format is a nonstandard
C       character. It tells the compiler not to print a newline. If you
C       are actually using FORTRAN 77, you should figure out what your
C       particular compiler accepts. If you are actually using Fortran
C       90 or later, you should replace this line with the commented
C       line that follows it.
 5001   FORMAT (I3, ',', $)
C5001   FORMAT (I3, ',', ADVANCE='NO')
      END
```



### Relying instead upon the looping features of FORMAT

For historical reasons, 6 is often the default unit number for standard output.

```Fortran

      WRITE (6,1) (I,I = 1,10)
    1 FORMAT (4(1X,I0,","),1X,I0)
      END

```

Here the break and continuation comes through the workings of the FORMAT interpreter. The feature 4(etc) means four repetitions of the format items within the brackets, and as each datum from the WRITE statement arrives, it is aligned with the next format item that can receive a datum, the I-format specifier (here I0, which means an integer of only as many digits as are needed for the value) and until such a reciever is encountered, intervening format items are acted upon - 1X means "one space", and the quotes surround a text literal. Accordingly, the first datum generates a space, a one-digit value, and a comma, as does the second and so on. When the sixth datum is received, the end of the format statement has been reached, and the convention is to write the current line and start a new line of output, and further, go back in the FORMAT specification to the first-encountered open-bracket symbol (the rightmost) which in this case is not the beginning of the FORMAT statement but the one that has a repetition count of four in front of it, and, resume interpretation. When the last datum has been accepted, naturally, the line is printed.

An alternative might be FORMAT (4(I2,","),I2) but that would generate
 1, 2, 3, 4, 5
 6, 7, 8, 9,10
Alternatively, FORMAT (4(I2,","),I2,/,4(I2,","),I3) would do the trick but there would no longer be the loop, break, continue aspect to the interpretation of the FORMAT statement, merely a grinding through a list.

This sort of scheme facilitates a compact way of printing a table with a heading, where the WRITE statement simply pours forth the data and relies on something like FORMAT("heading",/,(complex details for one line)) - thus printing the table line-by-line with only the first line having the heading, a saving on having a write and format statement pair for the heading and a second pair for the table body.


## Gambas

'''[https://gambas-playground.proko.eu/?gist=bf629ae9a09ffa1f5ecc95b89854b14b Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short

For siCount = 1 To 10
  Print siCount;
  If siCount <> 5 And siCount <> 10 Then Print ",";
  If siCount = 5 Then Print gb.NewLine;
Next

End
```

Output:

```txt

1,2,3,4,5
6,7,8,9,10

```



## GAP


```gap
for i in [1 .. 11] do
    if RemInt(i, 5) = 0 then
        Print(i, "\n");
        continue;
    fi;
    Print(i, ", ");
od;

# 1, 2, 3, 4, 5
# 6, 7, 8, 9, 10
```



## GML


```GML
for(i = 1; i <= 10; i += 1)
    {
    show_message(string(i))
    i += 1
    if(i <= 10)
        continue
    }
```



## Go


```go
package main

import "fmt"

func main() {
    for i := 1; i <= 10; i++ {
        fmt.Printf("%d", i)
        if i%5 == 0 {
            fmt.Printf("\n")
            continue
        }
        fmt.Printf(", ")
    }
}
```

```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## Groovy


```groovy
for (i in 1..10) {
    print i
    if (i % 5 == 0) {
        println ()
        continue
    }
    print ', '
}
```



## Haskell

As a functional language, it is not idiomatic to have true loops - recursion is used instead. Below is one of many possible implementations of the task.
The below code uses a guard (| symbol) to compose functions differently for the two alternative output paths, instead of using continue like in an imperative language.


```haskell
import Control.Monad (forM)
main = forM [1..10] out
    where
      out x | x `mod` 5 == 0 = print x
            | otherwise = (putStr . (++", ") . show) x
```



## HicEst


```hicest
DO i = 1, 10
  IF( MOD(i, 5) == 1 ) THEN
      WRITE(Format="i3") i
    ELSE
      WRITE(APPend, Format=" ',', i3 ") i
    ENDIF
ENDDO
```


== {{header|Icon}} and {{header|Unicon}} ==
The following code demonstrates the use of 'next' (the reserved word for 'continue'):

```Icon
procedure main()
every writes(x := 1 to 10) do {
   if x % 5 = 0 then {
      write()
      next
      }
   writes(", ")
   }
end
```

However, the output sequence can be written without 'next' and far more succinctly as:

```Icon
every writes(x := 1 to 10, if x % 5 = 0 then "\n" else ", ")
```



## Io


```io
for(i,1,10,
    write(i)
    if(i%5 == 0, writeln() ; continue)
    write(" ,")
)
```



## J

J is array-oriented, so there is very little need for loops.
For example, one could satisfy this task this way:


```j
_2}."1'lq<, >'8!:2>:i.2 5
```


J does support loops for those times they can't be avoided
(just like many languages support gotos for those time they can't be avoided).

```j
3 : 0 ] 10
        z=.''
        for_i. 1 + i.y do.
            z =. z , ": i

             if. 0 = 5 | i do.
                  z 1!:2 ]2
                  z =. ''
                  continue.
             end.

             z =. z , ', '
        end.
     i.0 0
   )
```


Though it's rare to see J code like this.


## Java


```java
for(int i = 1;i <= 10; i++){
   System.out.print(i);
   if(i % 5 == 0){
      System.out.println();
      continue;
   }
   System.out.print(", ");
}
```



## JavaScript

Using the <code>print()</code> function from [[Rhino]] or [[SpiderMonkey]].

```javascript
var output = "";
for (var i = 1; i <= 10; i++) {
  output += i;
  if (i % 5 == 0) {
    print(output);
    output = "";
    continue;
  }
  output += ", ";
}
```



Stepping back from any assumption that repetitive patterns of computation necessarily entail 'loops', and using a functional idiom of JavaScript, we can make the value of one or more subexpressions in a ''reduce()'' fold conditional on any special cases that we define.

For example:


```JavaScript
function rng(n) {
  return n ? rng(n - 1).concat(n) : [];
}

console.log(
  rng(10).reduce(
    function (a, x) {
      return a + x.toString() + (x % 5 ? ', ' : '\n');
    }, ''
  )
);
```


Output:

```JavaScript
1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## jq

jq does not have a "continue" statement.
In jq 1.4, the simplest way to accomplish the given task is probably as follows:

```jq
reduce range(1;11) as $i
  (""; . + "\($i)" + (if $i % 5 == 0 then "\n" else ", " end))
```



## Jsish


```javascript
/* Loop/continue in jsish */
for (var i = 1; i <= 10; i++) {
    printf("%d", i);
    if (i % 5 == 0) {
        printf("\n");
        continue;
    }
    printf(", ");
}
```


```txt
prompt$ jsish loop-continue.jsi
1, 2, 3, 4, 5
6, 7, 8, 9, 10
```



## Julia


```Julia

for i in 1:10
    print(i)
    if i%5 == 0
        println()
        continue
    end
    print(", ")
end

```


```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    for(i in 1 .. 10) {
        if (i % 5 == 0) {
            println(i)
            continue
        }
        print("$i, ")
    }
}
```


```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## Lasso


```Lasso
loop(10) => {^
	loop_count
	loop_count % 5 ? ', ' | '\r'
	loop_count < 100 ? loop_continue
	'Hello, World!' // never gets executed
^}
```



## Lingo


```lingo
str = ""
repeat with i = 1 to 10
  put i after str
  if i mod 5 = 0 then
    put RETURN after str
    next repeat
  end if
  put ", " after str
end repeat
put str
```



## Lisaac


```Lisaac
1.to 10 do { i : INTEGER;
  i.print;
  (i % 5 = 0).if { '\n'.print; } else { ','.print; };
};
```



## LiveCode


```LiveCode
repeat with n = 1 to 10
    put n
    if n is 5 then put return
    if n < 10 and n is not 5 then put ","
end repeat
```



## Lua


```Lua
for i = 1, 10 do
    io.write( i )
    if i % 5 == 0 then
        io.write( "\n" )
    else
    	io.write( ", " )
    end
end
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      \\ A For {} loop
      For i=1 to 10 {
            Print i;
            if i mod 5 Else Print : continue
            Print ",";
      }
      Print i=11
      \\ A For Next loop
      For i=1 to 10
            Print i;
            if i mod 5 Else Print : continue
            Print ",";
      Next i
      Print i=11
      \\ A for loop using  a block and a Loop statement
      i=0
      {     i++
            if i>10  then Exit
            loop
            Print i;
            if i mod 5 Else Print : continue
            Print ",";
       }
      Print i=11
      \\ as above but end value for i=10 not 11
      i=0
      {     i++
            if i<10  then loop
            Print i;
            if i mod 5 Else Print : continue
            Print ",";
       }
      Print i=10  ' not 11 but 10
}
Checkit

```



## Maple


```Maple
for i from 1 to 10 do
        printf( "%d", i );
        if irem( i, 5 ) = 0 then
                printf( "\n" );
                next
        end if;
        printf( ", " )
end do:
```


This can also be done as follows, but without the use of "next".

```Maple
for i to 10 do
        printf( "%d%s", i, `if`( irem( i, 5 ) = 0, "\n", ", " ) )
end do:
```



## Mathematica


```Mathematica
tmp = "";
For[i = 1, i <= 10, i++,
  tmp = tmp <> ToString[i];
  If[Mod[i, 5] == 0,
   tmp = tmp <> "\n";
   ,
   tmp = tmp <> ", ";
   ];
  ];
Print[tmp]
```


=={{header|MATLAB}} / {{header|Octave}}==

Loops are considered slow in Matlab and Octave,
it is preferable to vectorize the code.

```Matlab
disp([1:5; 6:10])
```

or

```Matlab
disp(reshape([1:10],5,2)')
```


A non-vectorized version of the code is shown below in Octave


```Matlab
for i = 1:10
  printf(' %2d',  i);
  if ( mod(i, 5) == 0 )
    printf('\n');
    continue
  end
end
```



## Maxima


```maxima
/* There is no "continue" in Maxima, the easiest is using a "if" instead */
block(
   [s: ""],
   for n thru 10 do (
      s: sconcat(s, n),
      if mod(n, 5) = 0 then (
         ldisp(s),
         s: ""
      ) else (
         s: sconcat(s, ", ")
      )
   )
)$
```



## MAXScript


```maxscript
for i in 1 to 10 do
(
    format "%" i
    if mod i 5 == 0 then
    (
        format "\n"
        continue
    )   continue
    format ", "
)
```

<nowiki>Insert non-formatted text here</nowiki>


## Metafont

Metafont has no a <tt>continue</tt> (or similar) keyword.
As the [[Loop/Continue#Ada|Ada solution]], we can complete the task just with conditional.


```metafont
string s; s := "";
for i = 1 step 1 until 10:
if i mod 5 = 0:
  s := s & decimal i & char10;
else:
  s := s & decimal i & ", "
fi; endfor
message s;
end
```


Since <tt>message</tt> append always a newline at the end,
we need to build a string and output it at the end,
instead of writing the output step by step.

'''Note''': <tt>mod</tt> is not a built in; like TeX, "bare Metafont" is rather primitive, and normally a set of basic macros is preloaded to make it more usable; in particular <tt>mod</tt> is defined as


```metafont
primarydef x mod y = (x-y*floor(x/y)) enddef;
```


=={{header|Modula-3}}==
Modula-3 defines the keyword <tt>RETURN</tt> as an exception,
but when it is used with no arguments it works just like <tt>continue</tt> in [[C]].

Note, however, that <tt>RETURN</tt> only works inside a procedure or
a function procedure; use <tt>EXIT</tt> otherwise.

Module code and imports are omitted.

```modula3
FOR i := 1 TO 10 DO
  IO.PutInt(i);
  IF i MOD 5 = 0 THEN
    IO.Put("\n");
    RETURN;
  END;
  IO.Put(", ");
END;
```



## MOO


```moo
s = "";
for i in [1..10]
  s += tostr(i);
  if (i % 5 == 0)
    player:tell(s);
    s = "";
    continue;
  endif
  s += ", ";
endfor
```



## Neko


```ActionScript
/**
 Loops/Continue in Neko
 Tectonics:
   nekoc loops-continue.neko
   neko loops-continue
*/

var index = 0;

while index < 10 {
  index += 1;
  $print(index);
  if $not($istrue(index % 5)) {
    $print("\n");

    continue;

  }
  $print(", ");
}
```


```txt
prompt$ nekoc loops-continue.neko
prompt$ neko loops-continue.n
1, 2, 3, 4, 5
6, 7, 8, 9, 10
```



## Nemerle

```Nemerle
using System;
using System.Console;
using Nemerle.Imperative;

module Continue
{
    Main() : void
    {
        foreach (i in [1 .. 10])
        {
            Write(i);
            when (i % 5 == 0) {WriteLine(); continue;}
            Write(", ");
        }
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/Continue'

  nul = '\-'
  loop i_ = 1 to 10
    say i_.right(2) || nul
    if i_ // 5 = 0 then do
      say
      iterate i_
      end
    say ', ' || nul

    end i_

```



## NewLISP


```NewLISP
(for (i 1 10)
  (print i)
  (if (= 0 (% i 5))
      (println)
    (print ", ")))
```



## Nim

```nim
for i in 1..10:
  if i mod 5 == 0:
    echo i
    continue
  stdout.write i, ","
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 FOR I=1 TO 10
20 PRINT I;
30 IF I-I/5*5=0 THEN PRINT :GOTO 50"CONTINUE
40 PRINT ",";
50 NEXT
```



## Objeck


```objeck
class Continue {
  function : Main(args : String[]) ~ Nil {
    for(i := 1; i <= 10; i += 1;) {
      if(i = 5) {
        "{$i}, "->PrintLine();
        continue;
      };
      "{$i}, "->Print();
    };
  }
}
```



## OCaml

There is no continue statement for for loops in OCaml,
but it is possible to achieve the same effect with an exception.

```ocaml
# for i = 1 to 10 do
    try
      print_int i;
      if (i mod 5) = 0 then raise Exit;
      print_string ", "
    with Exit ->
      print_newline()
  done
  ;;
1, 2, 3, 4, 5
6, 7, 8, 9, 10
- : unit = ()
```

Though even if the continue statement does not exist,
it is possible to add it with camlp4.


## Octave


```octave
v = "";
for i = 1:10
  v = sprintf("%s%d", v, i);
  if ( mod(i, 5) == 0 )
    disp(v)
    v = "";
    continue
  endif
  v = sprintf("%s, ", v);
endfor
```



## Oforth



```Oforth
: loopCont
| i |
   10 loop: i [
      i dup print 5 mod ifZero: [ printcr continue ]
      "," .
      ] ;
```



## Oz

By using the "continue" feature of the for-loop, we bind C to a nullary procedure which, when invoked, immediately goes on to the next iteration of the loop.

```oz
for I in 1..10 continue:C do
   {System.print I}
   if I mod 5 == 0 then
      {System.printInfo "\n"}
      {C}
   end
   {System.printInfo ", "}
end
```



## PARI/GP


```parigp
for(n=1,10,
  print1(n);
  if(n%5 == 0, print();continue);
  print1(", ")
)
```



## Pascal

See [[Loops/Continue#Delphi | Delphi]]


## Perl


```perl
foreach (1..10) {
    print $_;
    if ($_ % 5 == 0) {
        print "\n";
        next;
    }
    print ', ';
}
```


It is also possible to use a goto statement
to jump over the iterative code section for a particular loop:


```perl
foreach (1..10) {
    print $_;
    if ($_ % 5 == 0) {
        print "\n";
        goto MYLABEL;
    }
    print ', ';
MYLABEL:
}
```



## Perl 6

```perl6
for 1 .. 10 {
    .print;
    if $_ %% 5 {
        print "\n";
        next;
    }
    print ', ';
}
```


or without using a loop:


```perl6
$_.join(", ").say for [1..5], [6..10];
```



## Phix


```Phix
for i=1 to 10 do
    printf(1,"%d", i)
    if remainder(i,5)=0 then
        printf(1, "\n")
        continue
    end if
    printf(1,", ")
end for
{} = wait_key()
```



## PHP


```php
for ($i = 1; $i <= 10; $i++) {
    echo $i;
    if ($i % 5 == 0) {
        echo "\n";
        continue;
    }
    echo ', ';
}
```



## PicoLisp

PicoLisp doesn't have an explicit 'continue' functionality.
It can always be emulated with a conditional expression.

```PicoLisp
(for I 10
   (print I)
   (if (=0 (% I 5))
      (prinl)
      (prin ", ") ) )
```



## Pike


```pike
int main(){
   for(int i = 1; i <= 10; i++){
      write(sprintf("%d",i));
      if(i % 5 == 0){
         write("\n");
         continue;
      }
      write(", ");
   }
}
```



## PL/I


```pli
loop:
do i = 1 to 10;
   put edit (i) (f(3));
   if mod(i,5) = 0 then do; put skip; iterate loop; end;
   put edit (', ') (a);
end;
```



## Pop11


```pop11
lvars i;
for i from 1 to 10 do
   printf(i, '%p');
   if i rem 5 = 0 then
       printf('\n');
       nextloop;
   endif;
   printf(', ')
endfor;
```



## PowerShell

```powershell
for ($i = 1; $i -le 10; $i++) {
    Write-Host -NoNewline $i
    if ($i % 5 -eq 0) {
        Write-Host
        continue
    }
    Write-Host -NoNewline ", "
}
```



## Python


```python
for i in xrange(1,11):
    if i % 5 == 0:
        print i
        continue
    print i, ",",
```



## R

```R
for(i in 1:10)
{
   cat(i)
   if(i %% 5 == 0)
   {
      cat("\n")
      next
   }
   cat(", ")
}
```



## Racket


It is possible to skip loop iterations in Racket,
but an explicit <tt>continue</tt> construct is rarely used:


```racket

#lang racket

;; Idiomatic way
(for ([i (in-range 1 11)])
  (if (= (remainder i 5) 0)
      (printf "~a~n" i)
      (printf "~a, " i)))

;; Forces a skip, but not idiomatic because
;; the logic is less obvious
(for ([i (in-range 1 11)]
      #:unless (and (= (remainder i 5) 0)
                    (printf "~a~n" i)))
  (printf "~a, " i))

```



## REBOL


```REBOL
REBOL [
	Title: "Loop/Continue"
	URL: http://rosettacode.org/wiki/Loop/Continue
]

; REBOL does not provide a 'continue' word for loop constructs,
; however, you may not even miss it:

print "One liner (compare to ALGOL 68 solution):"
repeat i 10 [prin rejoin [i  either 0 = mod i 5 [crlf][", "]]]

print [crlf "Port of ADA solution:"]
for i 1 10 1 [
	prin i
	either 0 = mod i 5 [
		prin newline
	][
		prin ", "
	]
]
```


```txt
One liner (compare to ALGOL 68 solution):
1, 2, 3, 4, 5
6, 7, 8, 9, 10

Port of ADA solution:
1, 2, 3, 4, 5
6, 7, 8, 9, 10
```



## Red


```Red
repeat i 10 [
    prin i
    if i = 10 [break]
    either i = 5 [print ""][prin ","]
]
1,2,3,4,5
6,7,8,9,10
```



## REXX


### version 1

(This program could be simpler by using a   '''then/else'''   construct, but an   '''iterate'''   was used to conform to the task.)

```rexx
/*REXX program  illustrates  an example of a   DO   loop with an  ITERATE  (continue).  */

  do j=1  for 10                                 /*this is equivalent to:  DO J=1 TO 10 */
  call charout ,  j                              /*write the integer to the terminal.   */
  if j//5\==0  then do                           /*Not a multiple of five?   Then ···   */
                    call charout , ", "          /*  write a comma to the terminal, ··· */
                    iterate                      /* ··· & then go back for next integer.*/
                    end
  say                                            /*force REXX to display on next line.  */
  end   /*j*/
                                                 /*stick a fork in it,  we're all done. */
```

Program note:    the comma (<big><b>,</b></big>) immediately after the   '''charout'''   BIF indicates to use the terminal output stream.

'''output'''

```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



### version 2


```rexx
/*REXX program  illustrates  an example of a   DO   loop with an  ITERATE  (continue).  */
$=                                               /*nullify the variable used for display*/
    do j=1  for 10                               /*this is equivalent to:  DO J=1 TO 10 */
    $=$ || j', '                                 /*append the integer to a placeholder. */
    if j//5==0  then say left($, length($) - 2)  /*Is  J  a multiple of five?  Then SAY.*/
    if j==5     then $=                          /*start the display line over again.   */
    end   /*j*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   is the same as the 1<sup>st</sup> REXX version.




## Ring


```ring

for i = 1 TO 10
   see i
   if i % 5 = 0
      see nl
      loop
   ok
   see ", "
next

```



## Ruby


```ruby
for i in 1..10 do
   print i
   if i % 5 == 0 then
      puts
      next
   end
   print ', '
end
```

The "for" look could be written like this:

```ruby
(1..10).each do |i| ...
1.upto(10) do |i| ...
10.times do |n| i=n+1; ...
```

Without meeting the criteria (showing loop continuation), this task could be written as:

```ruby
(1..10).each_slice(5){|ar| puts ar.join(", ")}
```



## Rust


```rust
fn main() {
    for i in 1..=10 {
        print!("{}", i);
        if i % 5 == 0 {
            println!();
            continue;
        }
        print!(", ");
    }
}
```



## Salmon


```Salmon
iterate (x; [1...10])
  {
    print(x);
    if (x % 5 == 0)
      {
        print("\n");
        continue;
      };
    print(", ");
  };
```



## Sather

There's no <code>continue!</code> in Sather. The code solve the task without forcing a new iteration.

```sather
class MAIN is
  main is
    i:INT;
    loop i := 1.upto!(10);
      #OUT + i;
      if i%5 = 0 then
        #OUT + "\n";
      else
        #OUT + ", ";
      end;
    end;
  end;
end;
```



## Scala

Scala doesn't have a <code>continue</code> keyword.
However, you may not even miss it, <code>if</code> could be used here.


### The intuitive way


```scala
for (i <- 1 to 10) {
  print(i)
  if (i % 5 == 0) println() else print(", ")
  }
```



### Functional solution

Thinking In Scala<sup>©</sup> says: we avoid for loops and handle it the [[functional_programming|Functional]] way:
#Create a Range 1..10 included
#Split the range after converting to a List to a pair of List's
#A List of the elements of pair of will be created: List(List(1,2,3,4,5),List(6,7,8,9,10))
#The map makes for both elements in the List a conversion to a comma separated String, yielding a List of two Strings.
#Both comma separated strings will be separated by an EOL

```scala
  val a = (1 to 10 /*1.*/ ).toList.splitAt(5) //2.
  println(List(a._1, a._2) /*3.*/ .map(_.mkString(", ") /*4.*/ ).mkString("\n") /*5.*/ )
```



## Scheme


```scheme
(define (loop i)
  (if (> i 10) 'done
      (begin
       (display i)
       (cond ((zero? (modulo i 5))
              (newline) (loop (+ 1 i)))
             (else (display ", ")
                   (loop (+ 1 i)))))))
```



## Scilab

<lang>for i=1:10
    printf("%2d ",i)
    if modulo(i,5)~=0 then
      printf(", ")
      continue
    end
    printf("\n")
end
```

```txt
 1 ,  2 ,  3 ,  4 ,  5
 6 ,  7 ,  8 ,  9 , 10
```



## Sidef


```ruby
for i in (1..10) {
    print i
    if (i %% 5) {
        print "\n"
        next
    }
    print ', '
}
```



## Simula

```simula
! Loops/Continue - simula67 - 07/03/2017;
begin
    integer i;
    for i:=1 step 1 until 10 do begin
        outint(i,5);
        if mod(i,5)=0 then begin
            outimage;
            goto loop
        end;
        outtext(", ");
    loop:
    end
 end
```

```txt

    1,     2,     3,     4,     5
    6,     7,     8,     9,    10

```



## Smalltalk

```Smalltalk

1 to: 10 do: [ :i |
    [ :continue |
        i % 5 = 0 ifTrue: [
            Transcript show: i; cr.
            continue value ].
        Transcript
            show: i;
            show: ', '.
    ] valueWithExit.
]

```



## Spin

```Spin
con
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub main | i
  ser.start(31, 30, 0, 115200)

  repeat i from 1 to 10
    ser.dec(i)
    if i // 5
      ser.str(string(", "))
      next
    ser.str(string(13,10))

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```

```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## SPL


```spl
>
 n, 1..10
  s += n
  ? n%5, s += ", "
  >> n%5
  #.output(s)
  s = ""
<
```

```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON @

BEGIN
 DECLARE I SMALLINT DEFAULT 1;

 Loop: WHILE (I <= 10) DO
  CALL DBMS_OUTPUT.PUT(I);
  SET I = I + 1;
  IF (MOD(I - 1, 5) = 0) THEN
   CALL DBMS_OUTPUT.PUT_LINE(' ');
   ITERATE Loop;
  END IF;
  CALL DBMS_OUTPUT.PUT(', ');
 END WHILE Loop;
END @

```

Output:

```txt

db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## Stata

See '''[https://www.stata.com/help.cgi?continue continue]''' in Stata help. Notice that the _continue option of '''[https://www.stata.com/help.cgi?display display]''' has another purpose: it suppresses the automatic newline at the end of the display command.


```stata
forvalues n=1/10 {
	display `n' _continue
	if mod(`n',5)==0 {
		display
		continue
	}
	display ", " _continue
}
```



## Suneido


```Suneido
ob = Object()
for (i = 1; i <= 10; ++i)
    {
    ob.Add(i)
    if i is 5
        {
        Print(ob.Join(','))
        ob = Object()
        }
    }
Print(ob.Join(','))
```


```Suneido
1,2,3,4,5
6,7,8,9,10
ok
```



## Swift


```swift
for i in 1...10 {
    print(i, terminator: "")
    if i % 5 == 0 {
        print()
        continue
    }
    print(", ", terminator: "")
}
```

```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```


=={{header|Transact-SQL}}==

<lang Transact-SQL>
DECLARE @i INT = 0;
DECLARE @str VarChar(40) = '';
WHILE @i<10
  BEGIN
    SET @i = @i + 1;
    SET @str = @str + CONVERT(varchar(2),@i);
    IF @i % 5 = 0
      BEGIN
        PRINT @str;
        SET @str =''
        CONTINUE;
      END
    SET @str = @str +', ';
  END;

```



## Tcl


```tcl
for {set i 1} {$i <= 10} {incr i} {
   puts -nonewline $i
   if {$i % 5 == 0} {
      puts ""
      continue
   }
   puts -nonewline ", "
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
numbers=""
LOOP n=1,10
numbers=APPEND (numbers,", ",n)
rest=n%5
IF (rest!=0) CYCLE
 PRINT numbers
 numbers=""
ENDLOOP

```

```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## UnixPipes


```bash
yes \ | cat -n | head -n 10 | xargs -n 5 echo | tr ' ' ,
```



## UNIX Shell


```bash
Z=1
while (( Z<=10 )); do
    echo -e "$Z\c"
    if (( Z % 5 != 0 )); then
        echo -e ", \c"
    else
        echo -e ""
    fi
    (( Z++ ))
done
```


```bash
for ((i=1;i<=10;i++)); do
  echo -n $i
  if [ $((i%5)) -eq 0 ]; then
    echo
    continue
  fi
  echo -n ", "
done
```



## Ursa

```ursa
decl int i
for (set i 1) (< i 11) (inc i)
        if (= (mod i 5) 0)
                out i endl console
                continue
        end if
        out i ", " console
end for
```



## VBA


```VB
Public Sub LoopContinue()
    Dim value As Integer
    For value = 1 To 10
        Debug.Print value;
        If value Mod 5 = 0 Then
            'VBA does not have a continue statement
            Debug.Print
        Else
            Debug.Print ",";
        End If
    Next value
End Sub
```


## Vedit macro language


```vedit
for (#1 = 1; #1 <= 10; #1++) {
    Num_Type(#1, LEFT+NOCR)
    if (#1 % 5 == 0) {
        Type_Newline
        Continue
    }
    Message(", ")
}
```



## XPL0

Like Ada and ALGOL there's no 'continue' command. The task is solved very
simply anyway. The commands 'int' and 'rem' are shown spelled out here.
Only the first three characters of a command are required.


```XPL0
code CrLf=9, IntOut=11, Text=12;
integer N;
for N:= 1 to 10 do
        [IntOut(0, N); if remainder(N/5) \#0\ then Text(0, ", ") else CrLf(0)]
```


```txt

1, 2, 3, 4, 5
6, 7, 8, 9, 10

```



## zkl


```zkl
foreach n in ([1..10]){print(n); if(n%5==0){println(); continue;} print(", ")}
// or foreach n in ([1..10]){print(n,(n%5) and ", " or "\n")}
```

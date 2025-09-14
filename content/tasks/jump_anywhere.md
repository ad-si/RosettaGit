+++
title = "Jump anywhere"
description = ""
date = 2019-10-07T18:00:57Z
aliases = []
[extra]
id = 9722
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "applesoft_basic",
  "autohotkey",
  "basic",
  "c",
  "clipper",
  "cobol",
  "common_lisp",
  "computer_zero_assembly",
  "csharp",
  "d",
  "dcl",
  "erlang",
  "erre",
  "fbsl",
  "forth",
  "fortran",
  "freebasic",
  "futurebasic",
  "go",
  "harbour",
  "haskell",
  "i",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lingo",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "mbs",
  "mumps",
  "neko",
  "nim",
  "oforth",
  "ol",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "pl_sql",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "robotic",
  "run_basic",
  "scala",
  "spl",
  "ssem",
  "tcl",
  "vba",
  "vbscript",
  "zkl",
]
+++

{{task}} [[Category:Branches]] [[Category:Simple]]
[[Imperative programming|Imperative programs]] like to jump around, but some languages restrict these jumps. Many structured languages restrict their [[conditional structures]] and [[loops]] to ''local jumps'' within a function. Some assembly languages limit certain jumps or branches to a small range.

This task is demonstrate a local jump and a global jump and the various other types of jumps that the language supports.
For the purpose of this task, the jumps need not be used for a single purpose and you have the freedom to use these jumps for different purposes.
You may also defer to more specific tasks, like [[Exceptions]] or [[Generator]].
This task provides a "grab bag" for several types of jumps. There are ''non-local jumps'' across function calls, or ''long jumps'' to anywhere within a program. Anywhere means not only to the tops of functions!

* Some languages can ''go to'' any global label in a program.
* Some languages can break multiple function calls, also known as ''unwinding the call stack''.
* Some languages can save a ''continuation''. The program can later continue from the same place. So you can jump anywhere, but only if you have a previous visit there (to save the continuation).


These jumps are not all alike.
A simple ''goto'' never touches the call stack.
A continuation saves the call stack, so you can continue a function call after it ends.


## Task

Use your language to demonstrate the various types of jumps that it supports.

Because the possibilities vary by language, this task is not specific.
You have the freedom to use these jumps for different purposes.
You may also defer to more specific tasks, like [[Exceptions]] or [[Generator]].





## Ada


```ada

procedure Goto_Test is
begin

   Stuff;
   goto The_Mother_Ship; -- You can do this if you really must!
   Stuff;
   if condition then
      Stuff;
   <<Jail>>
      Stuff;
   end if;
   Stuff;

   -- Ada does not permit any of the following
   goto Jail;
   goto The_Sewer;
   goto The_Morgue;

   Stuff;
   case condition is
      when Arm1 =>
         Stuff;
         goto The_Gutter; -- Cant do this either
         Stuff;
      when Arm2 =>
         Stuff;
      <<The_Gutter>>
         Stuff;
      <<The_Sewer>>
         Stuff;
   end case;

   Stuff;
   for I in Something'Range loop
      Stuff;
   <<The_Morgue>>
      if You_Are_In_Trouble then
         goto The_Mother_Ship;
         -- This is the usual use of a goto.
      end if;
      Stuff;
   end loop;

   Stuff;
<<The_Mother_Ship>>
   Stuff;

end Goto_Test;


```



## AutoHotkey


```ahk
; Define a function.
function()
{
	MsgBox, Start
	gosub jump

	free:
	MsgBox, Success
}

; Call the function.
function()
goto next
return

jump:
MsgBox, Suspended
return

next:
Loop, 3
{
	gosub jump
}
return

/*
Output (in Message Box):

Start
Suspended
Success
Suspended
Suspended
Suspended

*/
```



## BASIC


```basic
10 GOTO 100: REM jump to a specific line
20 RUN 200: REM start the program running from a specific line
```


Some versions of basic allow line labels to be used. Here we jump to a label:
```qbasic>GOTO mylabel</lang

=
## Applesoft BASIC
=
caveat: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html

```ApplesoftBASIC
  0 REM GOTO
100 GOTO 110 : REM JUMP TO A SPECIFIC LINE
110 RUN 120 : REM START THE PROGRAM RUNNING FROM A SPECIFIC LINE
120 IF 1 GOTO 130 : REM CONDITIONAL JUMP
130 IF 1 THEN 140 : REM THEN ALSO WORKS IN PLACE OF GOTO
140 IF 1 THEN GOTO 150 : REM BE VERBOSE BY USING THEN GOTO
150 ON A GOTO 170, 180, 190 : REM JUMP A SPECIFIC LINE NUMBER IN THE LIST INDEXED BY THE VALUE OF A STARTING AT 1, IF A IS OUT OF RANGE DO NOT JUMP
160 ON ERR GOTO 270 : REM WHEN AN ERROR OCCURS JUMP TO A SPECIFIC LINE
170 GOSUB 180 : REM JUMP TO LINE 180, PUSHING THE CURRENT PLACE ON THE STACK
180 POP : REM POP THE CURRENT PLACE FROM THE STACK, EFFECTIVELY MAKING THE PREVIOUS LINE A JUMP
190 CALL -151 : REM CALL ANY MACHINE LANGUAGE SUBROUTINE, IT MIGHT RETURN, IT MIGHT NOT
200 & : REM CALL THE USER-DEFINED AMPERSAND ROUTINE, IT MIGHT RETURN, IT MIGHT NOT
210 ? USR(0) : REM CALL THE USER-DEFINED FUNCTION, IT MIGHT RETURN, IT MIGHT NOT
220 S = 6 : ?CHR$(4)"PR#"S : REM CALL THE ROM ROUTINE IN SLOT S
230 S = 6 : ?CHR$(4)"IN#"S : REM CALL THE ROM ROUTINE IN SLOT S
240 ?CHR$(4)"RUN PROGRAM" : REM RUN A BASIC PROGRAM FROM DISK
250 ?CHR$(4)"BRUN BINARY PROGRAM": REM RUN A MACHINE LANGUAGE BINARY PROGRAM FROM DISK
260 ?CHR$(4)"EXEC PROGRAM.EX" : REM EXECUTE THE TEXT THAT IS CONTAINED IN THE FILE PROGRAM.EX
270 RESUME : REM JUMP BACK TO THE STATEMENT THAT CAUSED THE ERROR
280 STOP : REM BREAK THE PROGRAM
290 END : REM END THE PROGRAM
300 GOTO : REM NO LINE NUMBER, JUMPS TO LINE 0
```


```ApplesoftBASIC
CONT : REM CONTINUE, JUMP BACK TO WHERE THE PROGRAM STOPPED
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>10 GOTO 100 ! jump to a specific line
20 RUN 200 ! start the program running from a specific line
```


=
## Run BASIC
=

```runbasic
for i = 1 to 10
 if i = 5 then goto [label5]
next i
end

[label5]
print i
while i < 10
 if i = 6 then  goto [label6]
i = i + 1
wend
end

[label6]
print i
if i = 6 then goto [finish]
print "Why am I here"

[finish]
 print "done"
```



## C

C has <code>goto LABEL</code> keyword.

```c
	if (x > 0) goto positive;
	else goto negative;

positive:
	printf("pos\n"); goto both;

negative:
	printf("neg\n");

both:
	...
```

The label must be literal, not computed at run time.  This won't work:

```c
goto (x > 0 ? positive : negative);
```

<!-- Except if you're using GCC extensions, when you can do something that's very similar to that. Scary stuff! -->
You can <code>goto</code> ''almost'' anywhere inside the same function, but can't go across function boundaries.  It's sometimes used to break out of nested loops:
```c
for (i = 0; ...) {
	for (j = 0; ...) {
		if (condition_met) goto finish;
	}
}
```
although you can (not that you ''should'') jump into a loop, too:
```c
	goto danger;
	for (i = 0; i < 10; i++) {
danger: /* unless you jumped here with i set to a proper value */
		printf("%d\n", i);
	}
```

For unwrapping call stack and go back up to a caller, see [[Exceptions#C|longjmp example]]; more powerful, but more expensive and complicated, is POSIX [[Exceptions/Catch an exception thrown in a nested call#C|ucontext]].
The best application for goto is error handling, this simplifies the resource clean up of a large function.
This is used in the linux kernel.




```c

  char *str;
  int *array;
  FILE *fp;

   str = (char *) malloc(100);
   if(str == NULL) {
     return;
   }


   fp=fopen("c:\\test.csv", "r");
   if(fp== NULL) {
     free(str );
     return;
   }

   array = (int *) malloc(15);
   if(array==NULL)   if(fp== NULL) {
     free(str );
     fclose(fp);
     return;
   }

   ...// read in the csv file and convert to integers


```




```c

  char *str;
  int *array;
  FILE *fp;

   str = (char *) malloc(100);
   if(str == NULL)
    goto:  exit;

   fp=fopen("c:\\test.csv", "r");
   if(fp== NULL)
      goto:  clean_up_str;

   array = (int *) malloc(15);
   if(array==NULL)
     goto: clean_up_file;
   ...// read in the csv file and convert to integers

   clean_up_array:
     free(array);
   clean_up_file:
     fclose(fp);
   clean_up_str:
     free(str );
   exit:
   return;

```


## C#
Like C, C# also has a <code>goto LABEL</code> keyword. This section is partly copied from the section on C, since both languages share common syntax.

```c#
if (x > 0) goto positive;
    else goto negative;

positive:
    Console.WriteLine("pos\n"); goto both;

negative:
    Console.WriteLine("neg\n");

both:
    ...
```

The label must be literal, not computed at run time.  This won't work:

```c#
goto (x > 0 ? positive : negative);
```


You can <code>goto</code> ''almost'' anywhere inside the same method, but can't go across method boundaries.  It's sometimes used to break out of nested loops:

```c#
for (i = 0; ...) {
    for (j = 0; ...) {
        if (condition_met) goto finish;
    }
}
```
although you can (not that you ''should'') jump into a loop, too:

```c#
goto danger;
for (i = 0; i < 10; i++) {
    danger: /* unless you jumped here with i set to a proper value */
    Console.WriteLine("{0}", i);
}
```


In C#, you can also <code>goto</code> a label from within any section of a try .. catch .. finally block:

```c#

int i = 0;
tryAgain:
try {
    i++;
    if (i < 10) goto tryAgain;
}
catch {
    goto tryAgain;
}
finally {
    goto end; // This is completely unnecessary, just here to demonstrate.
}
end:

```



## Clipper

Clipper has no labels and ''goto'' statements. The ''exit'' statements allows leave the current loop and doesn't contain any label where to go.


## COBOL


```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. JUMPS-PROGRAM.
* Nobody writes like this, of course; but...
PROCEDURE DIVISION.
* You can jump anywhere you like.
START-PARAGRAPH.
    GO TO AN-ARBITRARY-PARAGRAPH.
YET-ANOTHER-PARAGRAPH.
    ALTER START-PARAGRAPH TO PROCEED TO A-PARAGRAPH-SOMEWHERE.
* That's right, folks: we don't just have GO TOs, we have GO TOs whose
* destinations can be changed at will, from anywhere in the program,
* at run time.
    GO TO START-PARAGRAPH.
* But bear in mind: once you get there, the GO TO no longer goes to
* where it says it goes to.
A-PARAGRAPH-SOMEWHERE.
    DISPLAY 'Never heard of him.'
    STOP RUN.
SOME-OTHER-PARAGRAPH.
* You think that's bad? You ain't seen nothing.
    GO TO YET-ANOTHER-PARAGRAPH.
AN-ARBITRARY-PARAGRAPH.
    DISPLAY 'Edsger who now?'
    GO TO SOME-OTHER-PARAGRAPH.
```

```txt
Edsger who now?
Never heard of him.
```


'''COBOL''' also supports computed go to phrases, given a list of labels (paragraph names) and an integer index into that list.


```COBOL
01 province pic 99 value 2.
GO TO quebec, ontario, manitoba DEPENDING ON province
```



## Common Lisp

In Common Lisp you can jump anywhere inside a tagbody.

```lisp

(tagbody
  beginning
    (format t "I am in the beginning~%")
    (sleep 1)
    (go end)
  middle
    (format t "I am in the middle~%")
    (sleep 1)
    (go beginning)
  end
    (format t "I am in the end~%")
    (sleep 1)
    (go middle))

```

```txt

I am in the beginning
I am in the end
I am in the middle
I am in the beginning
I am in the end
I am in the middle
I am in the beginning
...

```



## Computer/zero Assembly

A <tt>JMP</tt> (jump) instruction can transfer control to any point in memory. Its target can be modified at run time, if required, by using instruction arithmetic:

```czasm
        LDA  goto
        SUB  somewhere
        ADD  somewhereElse
        STA  goto
goto:   JMP  somewhere
```

By the time execution reaches the instruction labelled <tt>goto</tt>, that instruction has become <tt>JMP somewhereElse</tt>. (This kind of coding does not, however, necessarily make the flow of control easier to follow.)


## D

Apart from exception handling, D has the break and continue statements that can jump to a label in the current scope. The goto statement can jump to any label inside the current function.

## DCL


```DCL
$ return  ! ignored since we haven't done a gosub yet
$
$ if p1 .eqs. "" then $ goto main
$ inner:
$ exit
$
$ main:
$ goto label  ! if label hasn't been read yet then DCL will read forward to find label
$ label:
$ write sys$output "after first occurrence of label"
$
$ on control_y then $ goto continue1  ! we will use this to get out of the loop that's coming up
$
$ label:  ! duplicate labels *are* allowed, the most recently read is the one that will be the target
$  write sys$output "after second occurrence of label"
$  wait 0::2  ! since we are in a loop this will slow things down
$  goto label  ! hit ctrl-y to break out
$
$ continue1:  ! the previous "on control_y" remains in force despite having been triggered
$
$ label = "jump"
$ goto 'label  ! target can be a variable; talk about handy
$ jump:
$ write sys$output "after first occurrence of jump"
$
$ first_time = "true"
$ continue_label = "continue2"
$ 'continue_label:  ! even the label can be a variable (but only backwards); talk about handy
$ if first_time then $ goto skip
$ break = "true"
$ return
$
$ skip:
$ first_time = "false"
$
$ on control_y then $ gosub 'continue_label  ! setup a new on control_y to get out the next loop coming up
$
$ break = "false"
$ 'label:
$  write sys$output "after second occurrence of jump"
$  wait 0::2
$  if .not. break then $ goto 'label
$
$ gosub sub1  ! no new scope or parameters
$ label = "sub1"
$ gosub 'label
$
$ call sub4 a1 b2 c3  ! new scope and parameters
$
$ @nl:  ! new scope and parameters in another file but same process
$
$ procedure_filename = f$environment( "procedure " )  ! what is our own filename?
$ @'procedure_filename inner
$
$ exit  ! exiting outermost scope exits the command procedure altogether, i.e. back to shell
$
$ sub1:
$ return
$
$ sub2:
$ goto break  ! structurally disorganized but allowed
$
$ sub3:
$ return
$
$ break:
$ return
$
$ sub4: subroutine
$ exit
$ endsubroutine
```

```txt
$ @jump_anywhere
after first occurrence of label
after second occurrence of label
after second occurrence of label
 Interrupt

after first occurrence of jump
after second occurrence of jump
after second occurrence of jump
 Interrupt

$
```

Same thing but with verify (tracing) on
```txt
$ @jump_anywhere
$ return  ! ignored since we haven't done a gosub yet
$
$ if p1 .eqs. "" then $ goto main
$ main:
$ goto label  ! if label hasn't been read yet then DCL will read forward to find label
$ label:
$ write sys$output "after first occurrence of label"
after first occurrence of label
$
$ on control_y then $ goto continue1  ! we will use this to get out of the loop that's coming up
$
$ label:  ! duplicate labels *are* allowed, the most recently read is the one that will be the target
$  write sys$output "after second occurrence of label"
after second occurrence of label
$  wait 0::2  ! since we are in a loop this will slow things down
$  goto label  ! hit ctrl-y to break out
$ label:  ! duplicate labels *are* allowed, the most recently read is the one that will be the target
$  write sys$output "after second occurrence of label"
after second occurrence of label
$  wait 0::2  ! since we are in a loop this will slow things down
 Interrupt

$ continue1:  ! the previous "on control_y" remains in force despite having been triggered
$
$ label = "jump"
$ goto jump  ! target can be a variable; talk about handy
$ jump:
$ write sys$output "after first occurrence of jump"
after first occurrence of jump
$
$ first_time = "true"
$ continue_label = "continue2"
$ continue2:
$ if first_time then $ goto skip
$ skip:
$ first_time = "false"
$
$ on control_y then $ gosub continue2  ! setup a new on control_y to get out the next loop coming up
$
$ break = "false"
$ jump:  ! even the target can be a variable (but only backwards); talk about handy
$  write sys$output "after second occurrence of jump"
after second occurrence of jump
$  wait 0::2
$  if .not. break then $ goto jump
$ jump:  ! even the target can be a variable (but only backwards); talk about handy
$  write sys$output "after second occurrence of jump"
after second occurrence of jump
$  wait 0::2
 Interrupt

$ continue2:
$ if first_time then $ goto skip
$ break = "true"
$ return
$  if .not. break then $ goto jump
$
$ gosub sub1  ! no new scope or parameters
$ sub1:
$ return
$ label = "sub1"
$ gosub sub1
$ sub1:
$ return
$
$ call sub4 a1 b2 c3  ! new scope and parameters
$ sub4: subroutine
$ exit
$
$ @nl:  ! new scope and parameters in another file but same process
$
$ procedure_filename = f$environment( "procedure " )  ! what is our own filename?
$ @DSVE_PAA_ROOT:[NG25957]JUMP_ANYWHERE.COM;1 inner
$ return  ! ignored since we haven't done a gosub yet
$
$ if p1 .eqs. "" then $ goto main
$ inner:
$ exit
$
$ exit  ! exiting outermost scope exits the command procedure altogether, i.e. back to shell
```


=={{header|Déjà Vu}}==
Déjà Vu supports continuations:


```dejavu
example:
    !print "First part"
    yield
    !print "Second part"

local :continue example
!print "Interrupted"
continue
```

```txt
First part
Interrupted
Second part
```


The byte-code language supports jumping to arbitrary positions in the same file.


## Erlang

Jumps are limited to [[Exceptions]].


## ERRE

ERRE has a GOTO statement in the form GOTO <label>. <label> is a numercic string and must be declared. ERRE GOTO is local only: you can jump only within the same procedure or within the main program. In the following example there are two errors:
<lang>
PROGRAM GOTO_ERR

LABEL 99,100

PROCEDURE P1
   INPUT(I)
   IF I=0 THEN GOTO 99 END IF
END PROCEDURE

PROCEDURE P2
99: PRINT("I'm in procdedure P2")
END PROCEDURE

BEGIN
100:
    INPUT(J)
    IF J=1 THEN GOTO 99 END IF
    IF J<>0 THEN GOTO 100 END IF
END PROGRAM

```

You can't jump from procedure P1 to procedure P2 or from main to procedure P2 (label 99). Jump to label 100 is allowed (within main program).


## FBSL

FBSL's BASIC supports labels (jump targets) and the corresponding GoTo and GoSub/Return commands. However, they are considered obsolete and have been superceded in everyday practice by more modern structured programming methods - subprocedures (Subs and Functions), block constructs, loops, and OOP.

FBSL's BASIC labels are denoted with colon prefixes:
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<nowiki>:</nowiki>label <span style="color:green">' this is a label named "label"</span>

<span style="color:blue">GOTO</span> label <span style="color:green">' this is a jump to the label named "label"</span>

</code></b></div>
and belong to the global namespace. This means they are accessible for the GoTo and GoSub commands from anywhere throughout the script regardless of whether they are defined locally (in a subprocedure) or globally (outside all subprocedures). Consequently, a label's name must be unique throughout the script. Duplicate label names generate a compile-time exception.

FBSL's DynAsm labels may be named or nameless (automatic):
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:maroon">@</span>label <span style="color:green">; this is a label named "label"</span>

<span style="color:blue">JMP</span> label <span style="color:green">; this is a jump to the label named "label"</span>

<span style="color:maroon">@@</span> <span style="color:green">; this is an automatic label</span>

<span style="color:blue">JMP</span> <span style="color:maroon">@</span><span style="color:red">F</span> <span style="color:green">; this is a jump to the nearest @@ label ahead</span>

<span style="color:blue">JMP</span> <span style="color:maroon">@</span><span style="color:red">B</span> <span style="color:green">; this is a jump to the nearest @@ label behind</span>

<span style="color:maroon">@@</span> <span style="color:green">; this is another automatic label</span>
</code></b></div>
and are local to the block they are defined in.

FBSL's DynC labels are denoted with colon postfixes and follow the ANSI C scoping rules:
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
label<nowiki>:</nowiki> <span style="color:green">// this is a label named "label"</span>

<span style="color:blue">goto</span> label; <span style="color:green">// this is a jump to the label named "label"</span>

</code></b></div>


## Forth


```forth
\ this prints five lines containing elements from the two
\ words 'proc1' and 'proc2'. gotos are used here to jump
\ into and out of the two words at various points, as well
\ as to create a loop. this functions with ficl, pfe,
\ gforth, bigforth, swiftforth, iforth, and vfxforth; it
\ may work with other forths as well.

create goto1 1 cells allot create goto2 1 cells allot
create goto3 1 cells allot create goto4 1 cells allot
create goto5 1 cells allot create goto6 1 cells allot
create goto7 1 cells allot create goto8 1 cells allot
create goto9 1 cells allot create goto10 1 cells allot

: proc1
[ here goto1 ! ] s" item1 " type goto7 @ >r exit
[ here goto2 ! ] s" item2 " type goto8 @ >r exit
[ here goto3 ! ] s" item3 " type goto9 @ >r exit
[ here goto4 ! ] s" item4 " type goto10 @ >r exit
[ here goto5 ! ] s" item5" type cr 2dup = if 2drop exit then 1+ goto6 @ >r ;

: proc2
[ here goto6 ! ] s" line " type dup . s" --> item6 " type goto1 @ >r exit
[ here goto7 ! ] s" item7 " type goto2 @ >r exit
[ here goto8 ! ] s" item8 " type goto3 @ >r exit
[ here goto9 ! ] s" item9 " type goto4 @ >r exit
[ here goto10 ! ] s" item10 " type goto5 @ >r ;

5 1 proc2
bye
```


Output:

```forth
line 1 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
line 2 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
line 3 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
line 4 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
line 5 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
```




```forth
\ this is congruent to the previous demonstration, employing
\ a data structure to store goto/jump addresses instead of
\ separate variables, and two additional words 'mark_goto' and
\ 'goto'. works with ficl, pfe, gforth, bigforth, and vfxforth.
\ swiftforth and iforth crash.

create gotos 10 cells allot  \ data structure for storing goto/jump addresses
: mark_goto here swap 1- cells gotos + ! ; immediate  \ save addresses for jumping
: goto  r> drop 1- cells gotos + @ >r ;

\ designations for commands are immaterial when using goto's,
\ since the commands are not referenced by name, and are instead
\ jumped into by means of the goto marker.

: command1
[ 1 ] mark_goto s" item1 " type 7 goto
[ 2 ] mark_goto s" item2 " type 8 goto
[ 3 ] mark_goto s" item3 " type 9 goto
[ 4 ] mark_goto s" item4 " type 10 goto
[ 5 ] mark_goto s" item5 " type cr 2dup = if 2drop exit then 1+ 6 goto ;

: command2
[ 6 ] mark_goto s" line " type dup . s" --> item6 " type 1 goto
[ 7 ] mark_goto s" item7 " type 2 goto
[ 8 ] mark_goto s" item8 " type 3 goto
[ 9 ] mark_goto s" item9 " type 4 goto
[ 10 ] mark_goto s" item10 " type 5 goto ;

: go 5 1 6 goto ; go
bye
```


Output:

```forth
line 1 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
line 2 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
line 3 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
line 4 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
line 5 --> item6 item1 item7 item2 item8 item3 item9 item4 item10 item5
```




```forth
\ works with ficl, pfe, gforth, bigforth, and vfx.
\ swiftforth may crash, iforth does not function.

: create_goto create 4 allot does> r> drop @ >r ;
: mark_goto here ' >body ! ; immediate

create_goto goto1
create_goto goto2
create_goto goto3
create_goto goto4
create_goto goto5
create_goto goto6
create_goto goto7
create_goto goto8
create_goto goto9
create_goto stop_here

:noname
mark_goto goto1   s" iteration " type dup . s" --> " type
                  s" goto1 " type   goto3
mark_goto goto2   s" goto2 " type   goto4
mark_goto goto3   s" goto3 " type   goto5
mark_goto goto4   s" goto4 " type   goto6
mark_goto goto5   s" goto5 " type   goto7
mark_goto goto6   s" goto6 " type   goto8
mark_goto goto7   s" goto7 " type   goto9
mark_goto goto8   s" goto8 " type   stop_here
mark_goto goto9   s" goto9 " type   goto2 ; drop

:noname  mark_goto stop_here
  cr 2dup = if 2drop exit then 1+ goto1
\ cr 2dup = if 2drop bye  then 1+ goto1  \ for swiftforth
; drop

: go goto1 ;
5 1 go

bye
```


Output:

```forth
iteration 1 --> goto1 goto3 goto5 goto7 goto9 goto2 goto4 goto6 goto8
iteration 2 --> goto1 goto3 goto5 goto7 goto9 goto2 goto4 goto6 goto8
iteration 3 --> goto1 goto3 goto5 goto7 goto9 goto2 goto4 goto6 goto8
iteration 4 --> goto1 goto3 goto5 goto7 goto9 goto2 goto4 goto6 goto8
iteration 5 --> goto1 goto3 goto5 goto7 goto9 goto2 goto4 goto6 goto8
```



Minimal Option <BR>
Although Forth was designed to be a structured programming language it is simple to add a generic jump anywhere.
The Forth dictionary of WORDs is essentially a linked list of labels.  We can find the execution token for any label in the dictionary with the [']  operator and jump to it with the keyword EXECUTE.

```forth
: GOTO     [']  EXECUTE ;
```


TEST

```forth
: WORLD ." World!" ;
: HELLO ." Hello" ;

GOTO CR GOTO HELLO GOTO SPACE GOTO WORLD
Hello World!
```



## Fortran

Fortran programmers have long been condemned for their usage of GO TO ''label'' (a label being an integer only: no text) which can be to any labelled statement in a program unit. That is, within a subroutine (or function) or within the main line. There is no provision for referencing labels outside a program unit, as with jumping from one subroutine into another. Even when F90 introduced the ability to nest routines whereby a routine is defined within another and the inner routine can reference the containing routine's variables, it is not allowed to reference its labels - purely as an intended restriction and despite this being allowed in Algol and PL/I. However, the "alternate return" protocol can be used, whereby a routine is invoked with additional parameters that are not numbers but labels within the calling routine. The called routine can then execute a normal RETURN statement, or say RETURN 2 to return not to the calling location but to the second nominated label in the invocation. By this means, the called routine can jump to some location in the calling routine, in the same way that in <code>READ (IN,*,ERR = 666) X</code> provides an alternate return for error conditions.

Early Fortran was devised without much worry over "structured" constructions, so it was quite possible to jump into the scope of a DO-loop (perhaps after jumping out) and subsequent results would depend on the method used by the compiler to control the number of iterations that are made. With F77 and the IF ... THEN ... ELSE ... ENDIF constructions, possibly nested, came the chance to jump from one clause to another - say from the ELSE clause to part-way into the THEN clause. As with the DO-loop violation, this behaviour gathered opprobrium and although allowed by the syntax is likely deemed to be an error by later compilers.

As for "Jump Anywhere", some implementations of the computed-GO TO statement really did do a calculate-offset-and-jump so that <code>GO TO (665,666,667), HOP</code> would produce an array of three addresses indexed by the value of HOP, and if HOP was outside the range of one to three, whatever was found at the indexed location would be taken as the address to jump to... Even worse opportunities might be exercised via the ASSIGN statement, which "assigns" a label to an integer variable as in <code>ASSIGN 666 TO THENCE</code> - effectively, storing the machine address associated with label 666 to variable THENCE. Such a variable is open to adjustment and might be available via a COMMON storage area to other routines, whose execution of <code>GO TO THENCE</code> is sure to prove interesting.


## FreeBASIC

The default and most modern dialect of FreeBASIC (-lang fb) supports the 'Goto' keyword either on its own or within the following constructs:

* On (Local) Error Goto (requires compilation with -e, -ex or -exx switch)
* On ... Goto
* If ... Goto (deprecated)


'Goto' is always followed by a label which must be a valid identifier and can only jump to locations at the same level (either within the same procedure or within module-level code).

However, a compiler error or warning is issued if 'Goto' skips a variable definition but not the end of the variable's scope. This is intended to prevent potential accesses to unconstructed or uninitialized variables and ensures that automatic destruction never happens to such variables.

'On (Local) Error Goto' can be used for error handling at module level or procedure level. In theory, the 'Local' keyword should restrict error handling to procedure level but currently is ignored by the compiler. It still works with the 'Error' statement, which forces an error to be generated, even if the above compiler switches are not used

'On ... Goto' allows one to define a 'jump table' which jumps to a specific label depending on the value of a variable or expression. A value of 1 will jump to the first label, a value of 2 to the second label and so on.

'If ... Goto' is shorthand for 'If ... Then Goto' in  a single line 'If' statement. It's an old construct which is now deprecated but still works.

In the other dialects of FreeBASIC, 'Goto' behaves in a similar fashion except that:

* In the -lang qb dialect, labels can still be line numbers for compatibility with old QuickBasic code.

* 'Goto' is allowed to skip uninitialized variables in the -lang qb and -lang fblite dialects because these dialects do not support nested scopes and all local variable declarations are moved to the top of their procedures.

* The -lang qb dialect also supports the 'Gosub' and 'On ... Gosub' constructs which (on execution of the 'Return' statement) return to the location from which they were called. In the -lang fblite dialect these constructs are disabled by default - you have to use the 'Option Gosub' statement to turn them on or 'Option NoGosub' statement to turn them off again.

* The -lang qb and -lang fblite dialects also support the 'Resume' and 'Resume Next' statements in conjunction with the 'On (Local) Error Goto' statement (requires compilation with -ex or -exx switch). These statements resume execution at the current or next line, respectively, following the execution of the error handling routine.


None of the FreeBASIC dialects support jumping out of multiple procedures or continuations. However, you can leave procedures prematurely (and rejoin the calling procedure or module-level code) using the 'Exit ... ' or 'Return ...' statements.

The following program demonstrate the use of the various 'Goto' constructs in the -lang fb dialect only:


```freebasic
' FB 1.05.0 Win64

' compiled with -lang fb (the default) and -e switches

Sub MySub()
  Goto localLabel
  Dim a As Integer = 10  '' compiler warning that this variable definition is being skipped
  localLabel:
  Print "localLabel reached"
  Print "a = "; a  '' prints garbage as 'a' is uninitialized
End Sub

Sub MySub2()
  On Error Goto handler
  Open "zzz.zz" For Input As #1 '' this file doesn't exist!
  On Error Goto 0 '' turns off error handling
  End

  handler:
  Dim e As Integer = Err '' cache error number before printing message
  Print "Error number"; e; " occurred - file not found"
End Sub

Sub MySub3()
  Dim b As Integer = 2
  On b Goto label1, label2 '' jumps to label2

  label1:
  Print "Label1 reached"
  Return '' premature return from Sub

  label2:
  Print "Label2 reached"
End Sub

Sub MySub4()
  Dim c As Integer = 3
  If c = 3 Goto localLabel2 '' better to use If ... Then Goto ... in new code
  Print "This won't be seen"
  localLabel2:
  Print "localLabel2 reached"
End Sub

MySub
MySub2
MySub3
MySub4
Print
Print "Pres any key to quit"
Print
```


```txt

localLabel reached
a =  5
Error number 2 occurred - file not found
Label2 reached
localLabel2 reached

```



## FutureBasic

FB supports both goto and gosub.

The goto statement causes program execution to continue at the statement at the indicated line number or statement label. The target statement must be within the same "scope" as the goto statement (i.e., they must both be within the "main" part of the program, or they must both be within the same local function). Also, you should not use goto to jump into the middle of any "block" statement structures (such as for...next, select...end select, long if...end if, etc.).

The gosub statement should include a return statement; return causes execution to continue at the statement following the gosub statement.

All that said, don't use spaghetti code -- use functions. You can still by spark plugs for a 1985 Yugo, but why?

```futurebasic

include "ConsoleWindow"

print "First line."
gosub "sub1"
print "Fifth line."
goto "Almost over"
"sub1"
print "Second line."
gosub "sub2"
print "Fourth line."
return
"Almost over"
print "We're just about done..."
goto "Outa here"
"sub2"
print "Third line."
return
"Outa here"
print "... with goto and gosub, thankfully."
end

```

Output:

```txt

First line.
Second line.
Third line.
Fourth line.
Fifth line.
We're just about done...
... with goto and gosub, thankfully.

```



## Go

Go has labelled 'break' and 'continue' statements which enable one to easily break out of a nested loop or continue with a containing loop.

Go also has a 'goto' statement which allows one to jump to a label in the same function. However, 'goto' is not allowed to jump over the creation of new variables or to jump to a label inside a block from outside that block.

For example:

```go
package main

import "fmt"

func main() {
    outer:
    for i := 0; i < 4; i++ {
        for j := 0; j < 4; j++ {
            if i + j == 4 { continue outer }
            if i + j == 5 { break outer }
            fmt.Println(i + j)
        }
    }

    k := 3
    if k == 3 { goto later }
    fmt.Println(k)  // never executed
    later:
    k++
    fmt.Println(k)
}
```


```txt

0
1
2
3
1
2
3
2
3
3
4

```



## Harbour

Harbour has no labels and ''goto'' statements. The ''exit'' statements allows leave the current loop and doesn't contain any label where to go.


## Haskell

Haskell being pure functional language doesn't need labels or goto's. However it is flexible and powerful enough to implement imperative constructions using monads. Hackage has a library [https://hackage.haskell.org/package/GotoT-transformers-1.0.0.1/docs/Control-Monad-Trans-Goto.html GoToT-transformers]. This module provides a <code>Goto</code> monad and corresponding monad transformer that allow the user to transfer the flow of execution from an arbitrary point of a monadic computation to another monadic computation. It works with any monad, not only with <code>IO</code>.

We show possible implementation of <code>goto</code>, given [https://www.reddit.com/r/haskell/comments/1jk06q/goto_in_haskell/ here], with some simplifications.

First some boilerplate, where we define labels and goto "operator".

```Haskell
import Control.Monad.Cont

data LabelT r m = LabelT (ContT r m ())

label :: ContT r m (LabelT r m)
label = callCC subprog
  where subprog lbl = let l = LabelT (lbl l) in return l

goto :: LabelT r m -> ContT r m b
goto (LabelT l) = const undefined <$> l

runProgram :: Monad m => ContT r m r -> m r
runProgram program = runContT program return
```


Here is example of using labels in IO:

```Haskell
main = runProgram $
  do
    start <- label
    lift $ putStrLn "Enter your name, please"
    name <- lift $ getLine
    if name == ""
      then do lift $ putStrLn "Name can't be empty!"
              goto start
      else lift $ putStrLn ("Hello, " ++ name)
```


```txt
λ> main
Enter your name, please

Name can't be empty!
Enter your name, please
John
Hello, John
```


We can build tiny EDSL to get rid of explicit lifting and to add refferences:

```Haskell
import Data.IORef

readInt = lift $ readLn >>= newIORef
get ref = lift $ readIORef ref
set ref expr = lift $ modifyIORef ref (const expr)
output expr = lift $ putStrLn expr
```


and implement famous Euclid's algorithm as an imperative program:

```Haskell
gcdProg = runProgram $ callCC $ \exit ->          -- <--------+
  do                                              --          |
    start <- label                                -- <-----+  |
    output "Enter two integers, or zero to exit"  --       |  |
    nr <- readInt                                 --       |  |
    n <- get nr                                   --       |  |
    when (n == 0) $                               --       |  |
      do output "Exiting"                         --       |  |
         exit ()                                  -- ---------+
    mr <- readInt                                 --       |
    loop <- label                                 -- <--+  |
    n <- get nr                                   --    |  |
    m <- get mr                                   --    |  |
    when (n == m) $                               --    |  |
      do output ("GCD: " ++ show n)               --    |  |
         goto start                               -- ------+
    when (n > m) $ set nr (n - m)                 --    |
    when (m > n) $ set mr (m - n)                 --    |
    goto loop                                     -- ---+
```


```txt
λ> gcdProg
Enter two integer numbers, or zero to exit
12
15
GCD: 3
Enter two integer numbers, or zero to exit
0
Exiting
```


In native Haskell such flow jumps are done by function calls:


```Haskell
gcdFProg = start
  where
    start = do
      putStrLn "Enter two integers, or zero to exit"
      n <- readLn
      if n == 0
        then
          putStrLn "Exiting"
        else do
          m <- readLn
          putStrLn $ "GCD: " ++ show (loop n m)
          start

loop n m
  | n == m = n
  | n < m  = loop n (m-n)
  | n > m  = loop (n-m) m
```


Or without explicit recursion and branching:


```Haskell
import Control.Applicative
import Control.Monad.Trans.Maybe

gcdFProg2 = forever mainLoop <|> putStrLn "Exiting"
  where
    mainLoop = putStrLn "Enter two integers, or zero to exit" >>
               runMaybeT process >>=
               maybe empty (\r -> putStrLn ("GCD: " ++ show r))

    process = gcd <$> (lift readLn >>= exitOn 0) <*> lift readLn

    exitOn n x = if x == n then empty else pure x
```



## i


```i
//'i' does not have goto statements, instead control flow is the only legal way to navigate the program.
concept there() {
	print("Hello there")
	return //The return statement goes back to where the function was called.
	print("Not here")
}

software {
	loop {
		break //This breaks the loop, the code after the loop block will be executed next.

		print("This will never print")
	}

	loop {
		loop {
			loop {
				break //This breaks out of 1 loop.
			}
			print("This will print")
			break 2 //This breaks out of 2 loops.
		}
		print("This will not print")
	}


	//Move to the code contained in the 'there' function.
	there()
}
```



## J

J allows control to be passed to any named verb at any time.  When the verb completes, control returns to the caller, unless an unhandled exception was raised (or unless a handled exception was raised, where the handler is outside the caller).

For example:

```j
F=: verb define
  smoutput 'Now we are in F'
  G''
  smoutput 'Now we are back in F'
)

G=: verb define
  smoutput 'Now we are in G'
  throw.
)

   F''
Now we are in F
Now we are in G
```


J also supports jumps to labels within a definition:


```j
H=: verb define
  smoutput 'a'
  label_b.
  smoutput 'c'
  goto_f.
  label_d.
  smoutput 'e' return.
  label_f.
  smoutput 'g'
  goto_d.
  smoutput 'h'
)

   H''
a
c
g
e
```



## Java

The closest thing that Java has to a "goto" is labelled loops:

```java
loop1: while (x != 0) {
    loop2: for (int i = 0; i < 10; i++) {
        loop3: do {
            //some calculations...
            if (/*some condition*/) {
                //this continue will skip the rest of the while loop code and start it over at the next iteration
                continue loop1;
            }
            //more calculations skipped by the continue if it is executed
            if (/*another condition*/) {
                //this break will end the for loop and jump to its closing brace
                break loop2;
            }
        } while (y < 10);
        //loop2 calculations skipped if the break is executed
    }
    //loop1 calculations executed after loop2 is done or if the break is executed, skipped if the continue is executed
}
```


It's frowned upon to use exceptions for non-exceptional paths, so get ready to frown at this fizz-buzz player that makes use of <tt>try/catch/finally</tt> to jump to the right places for printing:

```java

public class FizzBuzzThrower {
    public static void main( String [] args ) {
        for ( int i = 1; i <= 30; i++ ) {
            try {
                String message = "";
                if ( i % 3 == 0 ) message = "Fizz";
                if ( i % 5 == 0 ) message += "Buzz";
                if ( ! "".equals( message ) ) throw new RuntimeException( message );
                System.out.print( i );
            } catch ( final RuntimeException x ) {
                System.out.print( x.getMessage() );
            } finally {
                System.out.println();
            }
        }
    }
}

```



## jq


jq supports named labels ("label $NAME") and break statements ("break $NAME").  Usage
follows the pattern:

```jq
label $out | ... break $out ...
```


When, during program execution, a "break" statement is encountered, the program
continues as though the nearest `label $label_name`
statement on the left produced `empty`.

The label specified by the "break" statement has to be visible (in the
sense of being within scope) at the break statement.

Break statements are used to interrupt a generator.  For example, to emit
the least positive integer satisfying sin(1/N) == (1/N) using IEEE 64-bit arithmetic:

```jq
label $out | 1e7 | while(true; .+1) | if (1/.) | . == sin then (., break $out) else empty end
```


Here, the "while" filter is an unbounded generator.  The answer is 46530688.

The main reason why jq has a "break" statement is so that various control structures, such as the short-circuiting control structures "any" and "all", can be implemented in jq itself.

Indeed, a better way to solve the type of problem mentioned above is by using one of the jq-provided control structures.
For example, the above problem can be solved more succinctly and transparently using until/2:

```jq
1e7 | until(1/. | . == sin; .+1)
```



## Julia


Julia provides the @goto and @label macros for goto within functions but these cannot be used at the global level or to jump from one function to another. The macros can however be used for a typical use of @goto -- jumping out to a specific level from nested loops within a function.

```julia

function example()
    println("Hello ")
    @goto world
    println("Never printed")
    @label world
    println("world")
end

```



## Kotlin

Kotlin does not have a 'goto' statement but does have labelled 'break' and 'continue' statements which enable one to easily break out of a nested loop or continue with a containing loop.

Kotlin also has a labelled 'return' statement to return from a nested function or lambda expression.

For example:

```scala
// version 1.0.6

fun main(args: Array<String>) {
    intArrayOf(4, 5, 6).forEach lambda@ {
        if (it == 5) return@lambda
        println(it)
    }
    println()
    loop@ for (i in 0 .. 3) {
        for (j in 0 .. 3) {
            if (i + j == 4) continue@loop
            if (i + j == 5) break@loop
            println(i + j)
        }
    }
}
```


```txt

4
6

0
1
2
3
1
2
3
2
3
3

```



## Lingo


* Lingo does not support jumping to markers ("goto").

* Lingo supports "unwinding the call stack" via the abort command that exits the current call stack:

```lingo
on foo
  abort()
end

on bar ()
  foo()
  put "This will never be printed"
end
```


* Rarely used, but Lingo supports saving a continuation via the play ... play done construct:

```lingo
on testPlayDone ()

  -- Start some asynchronous process (like e.g. a HTTP request).
  -- The result might be saved in some global variable, e.g. in _global.result
  startAsyncProcess()

  -- pauses execution of current function
  play(_movie.frame)

  -- The following will be executed only after 'play done' was called in the asynchronous process code
  put "Done. The asynchronous process returned the result:" && _global.result
end
```



## Logtalk

Jumps are limited to [[Exceptions]].


## Lua

Lua 5.2 introduced a <code>goto</code> statement along with labels. It was somewhat controversially implemented instead of a <code>continue</code> keyword, but it is more flexible and supports other types of jumps as well. <code>goto</code> only supports statically-defined labels.

```lua
-- Forward jump
goto skip_print
print "won't print"
::skip_print::

-- Backward jump
::loop::
print "infinite loop"
goto loop

-- Labels follow the same scoping rules as local variables, but with no equivalent of upvalues
goto next
do
    ::next:: -- not visible to above goto
    print "won't print"
end
::next:: -- goto actually jumps here

-- goto cannot jump into or out of a function
::outside::
function nope () goto outside end -- error: no visible label 'outside' for <goto> at line 2

goto inside
function nope () ::inside:: end -- error: no visible label 'inside' for <goto> at line 1

-- Convenient for breaking out of nested loops
for i = 1, 10 do
    for j = 1, 10 do
        for k = 1, 10 do
            if i^2 + j^2 == k^2 then
                print(("found: i=%d j=%d k=%d"):format(i, j, k))
                goto exit
            end
        end
    end
end
print "not found"
::exit::
```



## M2000 Interpreter

We can use basic like numbers, and Goto, On Goto, Gosub, On Gosub, and an Exit For to specific label.

We can place statements right in a label if is numeric. If label is not numeric we can place rem only using ' or \

After Else and Then we can place number to jump (same as Goto number). Goto can't take variable. We can use On Goto to use an expression to choose label (we can place numbers or named labels)

### Using GOTO and GOSUB


```M2000 Interpreter

Module Checkit {
      Module Alfa {
            10 Rem this code is like basic
            20 Let K%=1
            30 Let A=2
            40 Print "Begin"
            50 On K% Gosub 110
            60 If A=2 then 520
            70 For I=1 to 10
            80 if i>5 then exit for 120
            90 Gosub 110
            100 Next i
            110 On A Goto 150,  500
            120 Print "This is the End ?"
            130 Return
            150 Print "from loop pass here", i
            160 Return
            200 Print "ok"
            210 Return
            500 Print "Routine 500"
            510 Goto 200
            520 Let A=1
            530 Gosub 70
            540 Print "Yes"
      }
      Alfa
      \\ this can be done. Code executed like it is from this module
      \\ because 200 is a label inside code of Module Checkit
      \\ and search is not so smart. After first search. position saved in a hash table
      Gosub 200  ' print "ok"
      Gosub 200  ' print "ok"
}
Checkit

```



### Simulate Run Basic Entry


```M2000 Interpreter

Module LikeRunBasic {
      for i = 1 to 10
      if i = 5 then goto label5
      next i
      end

      label5:
      print i
      while i < 10 {
            if i = 6 then  goto label6
            i = i + 1
      }
      end

      label6:
      print i
      if i = 6 then goto finish
      print "Why am I here"

      finish:
      print "done"
}
LikeRunBasic

```



### Simulate Go Entry



```M2000 Interpreter

Module LikeGo {
      \\ simulate Go for
      \\ need to make var Empty, swapping uninitialized array item
      Module EmptyVar (&x) {
            Dim A(1)
            Swap A(0), x
      }
      Function GoFor(&i,  first, comp$, step$) {
            \\ checking for empty we now if i get first value
            if type$(i)="Empty" then {
                  i=first
                  } else {
                  i+=Eval(step$)
            }
            if Eval("i"+comp$) Else Exit
            =true
          }
      def i, j
      EmptyVar &i
      outer:
       {
            if GoFor(&i, 0, "<4", "+1") else exit
            EmptyVar &j
            {
                 if GoFor(&j, 0, "<4", "+1") else exit
                 if i+j== 4 then goto outer
                 if i+j == 5 then goto break_outer
                 print i+j
                 loop
            }
            loop
      }
      break_outer:
      k = 3
      if k == 3 Then Goto later
      Print k  \\ never executed
      later:
      k++
      Print k
}
LikeGo


\\ or we can use For {} block and put label outer to right place
Module LikeGo {
      For i=0 to 3 {
            For j=0 to 3  {
                  if i+j== 4 then goto outer
                  if i+j == 5 then goto break_outer
                  print i+j
            }
            outer:
      }
      break_outer:
      k = 3
      if k == 3 Then Goto later
      Print k  \\ never executed
      later:
      k++
      Print k
}
LikeGo


Module LikeGo_No_Labels {
      For i=0 to 3 {
            For j=0 to 3  {
                  if i+j== 4 then exit ' exit breaks only one block
                  if i+j == 5 then break ' break breaks all blocks, but not the Module's block.
                  print i+j
            }
      }
      k = 3
      if k == 3 Else {
            Print k  \\ never executed
      }
      k++
      Print k
}
LikeGo_No_Labels


```



## Mathematica

Mathematica supports non-local jumps to a previous Catch[] (via Throw[]).

There is Goto[Label] function in Mathematica. This allows "jumps" to arbitrary locations within (the same or other) functions.


## MBS



```MBS>goto mylabel;</lang


=={{header|MK-61/52}}==

<lang>БП	XX
```


'''XX''' is any address.


## MUMPS

You can go to any point in any routine in the same namespace. In some implementations, you can even go between
namespaces. It's not recommended though.

Some interpreters will allow you jump between blocks of the same depth. Others will let you leave, but not enter, a block.

```MUMPS
 ;Go to a label within the program file
 Goto Label
 ;Go to a line below a label
 Goto Label+lines
 ;Go to a different file
 Goto ^Routine
 ;Go to a label within a different file
 Goto Label^Routine
 ;and with offset
 Goto Label+2^Routine
 ;
 ;The next two goto commands will both return error M45 in ANSI MUMPS.
NoNo
 For
 . Goto Out
Out Quit
 Goto NoNo+2
```



## Neko


Neko supports colon terminated labels, and a builtin '''$goto(label)'''.  This builtin is special in that the label argument is not dereferenced as a normal Neko expression, but specifically as a label.


```ActionScript
$print("start\n")
$goto(skip)
$print("Jumped over")
skip:
$print("end\n")
```


The NekoVM keeps exception and call stacks in sync when jumping across boundaries to code blocks, but may not include some initiating side effects.  For instance, jumping past a '''try''' phrase into the middle of the code block (where the try is not evaluated) will not catch the '''catch''' of the try catch pair.  There are other cases where '''$goto()''' can cross semantic boundaries; a practice best avoided.


## Nim

Nim has exceptions and labelled breaks:

```nim

block outer:
  for i in 0..1000:
    for j in 0..1000:
      if i + j == 3:
        break outer
```



## Oforth


Oforth has no goto and no labels.

Apart from exceptions handling (see exceptions tasks), Oforth has continue (to go to next loop) and break (to leave a loop).


## Ol

No jumps in Ol because Ol is purely functional language.

Some portion of code can be restarted (as recursion) and skipped (as continuation).

```scheme

; recursion:
(let loop ((n 10))
   (unless (= n 0)
      (loop (- n 1))))

; continuation
(call/cc (lambda (break)
   (let loop ((n 10))
      (if (= n 0)
         (break 0))
      (loop (- n 1)))))

(print "ok.")

```



## PARI/GP

GP lacks gotos and continuations, but can break out of arbitrarily many nested loops with <code>break(n)</code> which can be used to simulate goto within a given function.  It can also use <code>local</code> values or <code>error</code> to break out of several layers of function calls, if needed.

PARI inherits C's ability to <code>goto</code> or <code>longjmp</code>; the latter is used extensively in the library.


## Perl

Perl's <code>goto LABEL</code> and <code>goto EXPR</code> are a little too powerful to be safe.  Use only under extreme duress (actually, not even then). <code>goto &SUB</code> is esoteric but much more innocuous and can occasionally be handy.

```perl
sub outer {
    print "In outer, calling inner:\n";
    inner();
  OUTER:
    print "at label OUTER\n";
}

sub inner {
    print "In inner\n";

    goto SKIP;      # goto same block level
    print "This should be skipped\n";
  SKIP:
    print "at label SKIP\n";

    goto OUTER;     # goto whatever OUTER label there is on frame stack.
                    # if there isn't any, exception will be raised
    print "Inner should never reach here\n";
}

sub disguise {
    goto &outer;    # a different type of goto, it replaces the stack frame
                    # with the outer() function's and pretend we called
                    # that function to begin with
    print "Can't reach this statement\n";
}

print "Calling outer:\n";
outer();

print "\nCalling disguise:\n";
disguise();

print "\nCalling inner:\n";
inner();                # will die
```


## Perl 6


===Label-based jumps===

```perl6
    outer-loop: loop {
        inner-loop: loop {
            # NYI # goto inner-loop if rand > 0.5; # Hard goto
            next inner-loop if rand > 0.5; # Next loop iteration
            redo inner-loop if rand > 0.5; # Re-execute block
            last outer-loop if rand > 0.5; # Exit the loop
            ENTER { say "Entered inner loop block" }
            LEAVE { say "Leaving inner loop block" }
        }
        ENTER { say "Entered outer loop block" }
        LEAVE { say "Leaving outer loop block" }
        LAST  { say "Ending outer loop" }
    }
```

Produces random output, but here's a representative run:

  Entered outer loop block
  Entered inner loop block
  Leaving inner loop block
  Entered inner loop block
  Leaving inner loop block
  Entered inner loop block
  Leaving inner loop block
  Leaving outer loop block
  Ending outer loop

===Continuation-based execution===

Continuations in Perl 6 are currently limited to use in generators via the gather/take model:

```perl6
    my @list = lazy gather for ^100 -> $i {
        if $i.is-prime {
            say "Taking prime $i";
            take $i;
        }
    }

    say @list[5];
```

This outputs:

  Taking prime 2
  Taking prime 3
  Taking prime 5
  Taking prime 7
  Taking prime 11
  Taking prime 13
  13

Notice that no further execution of the loop occurs. If we then asked for the element at index 20, we would expect to see 15 more lines of "Taking prime..." followed by the result: 73.


### Failures and exceptions


Exceptions are fairly typical in Perl6:

```perl
    die "This is a generic, untyped exception";
```

Will walk up the stack until either some `CATCH` block intercepts the specific exception type or we exit the program.

But if a failure should be recoverable (e.g. execution might reasonably continue along another path) a failure is often the right choice. The fail operator is like "return", but the returned value will only be valid in boolean context or for testing definedness. Any other operation will produce the original exception with the original exception's execution context (e.g. traceback) along with the current context.

```perl6
    sub foo() { fail "oops" }
    my $failure = foo;
    say "Called foo";
    say "foo not true" unless $failure;
    say "foo not defined" unless $failure.defined;
    say "incremented foo" if $failure++; # exception
```

Produces:

  Called foo
  foo not true
  foo not defined
  oops
    in sub foo at fail.p6 line 1
    in block <unit> at fail.p6 line 2
  Actually thrown at:
    in any  at gen/moar/m-Metamodel.nqp line 3090
    in block <unit> at fail.p6 line 6

However, an exception can `.resume` in order to jump back to the failure point (this is why the stack is not unwound until after exception handling).

```perl6
  sub foo($i) {
      if $i == 0 {
          die "Are you sure you want /0?";
      }
      say "Dividing by $i";
      1/$i.Num + 0; # Fighting hard to make this fail
  }

  for ^10 -> $n {
      my $recip = foo($n);
      say "1/{$n} = {$recip.perl}";
  }

  CATCH {
      when ~$_ ~~ m:s/Are you sure/ { .resume; #`(yes, I'm sure) }
  }
```

This code raises an exception on a zero input, but then resumes execution, divides be zero and then raises a divide by zero exception which is not caught:

  Dividing by 0
  Attempt to divide 1 by zero using /
    in sub foo at fail.p6 line 6
    in block <unit> at fail.p6 line 10

  Actually thrown at:
    in sub foo at fail.p6 line 6
    in block <unit> at fail.p6 line 10


## Phix

In Phix, when absolutely necessary, gotos and labels can be implemented using inline assembly.
Using this 'baroque' syntax is viewed as an effective means of dissuading novices from adopting goto as a weapon of choice.

```Phix
#ilASM{ jmp :%somelabel }
    ...
#ilASM{ :%somelabel }
```

The above shows a global label, which can only be declared in top-level code, not inside a routine, and can be referenced from anywhere.

Local labels are declared with a double colon (::) and referenced with a single colon

```Phix
#ilASM{ jmp :local
            ...
       ::local }
```

They can only be referenced at the top level from within the same #ilASM construct, or anywhere within the routine where they are defined.
Phix also supports anonymous local labels

```Phix
#ilASM{ jmp @f
        ...
      @@:
        ...
        jle @b }
```

There are also optional (global) labels:

```Phix
#ilASM{ call :!optlbl
        [32]
          pop eax
        [64]
          pop rax
        []
            ...
       :!optlbl
        [32]
          push dword[esp]
        [64]
          push qword[rsp]
        []
          ret }
```

These are obviously more useful when the reference and declaration are in separate files: if the file containing the declaration is not included, the reference quietly resolves to 0.
The above code shows how to duplicate the return address and discard on return, so that execution carries on at the next instruction if the definition is not present.
Optional labels are most heavily used as part of the run-time diagnostics, for example pDiagN.e contains lines such as <code>cmp edx,:!opXore92a</code> which checks to see whether an
invalid memory access wants mapping to a "variable has not been assigned a value" error (and retrieve the correct return address for any error that occurs at that specific location);
if <code>xor</code> is not used/included, the instruction quietly resolves to cmp edx,0.

Lastly (for completeness) there are init labels, defined using

```Phix
#ilASM{ :>init }
```

These are used by the VM (eg builtins\VM\pStack.e declares :>initStack) to specify initialisation code which must be run at startup. They can also be called like a normal global label.

Obviously there are also several hll keywords such as <code>exit</code> and <code>return</code> which can often be used to achieve the same effect in a much more structured fashion.


## PicoLisp

PicoLisp supports non-local jumps to a previously setup environment (see [[Exceptions#PicoLisp|exceptions]]) via '[http://software-lab.de/doc/refC.html#catch catch]' and '[http://software-lab.de/doc/refT.html#throw throw]', or to some location in another coroutine with '[http://software-lab.de/doc/refY.html#yield yield]' (see [[Generator#PicoLisp|generator]]).

'[http://software-lab.de/doc/refQ.html#quit quit]' is similar to 'throw', but doesn't require a corresponding 'catch', as it directly jumps to the error handler (where the program may catch that error again).

There is no 'go' or 'goto' function in PicoLisp, but it can be emulated with normal list processing functions. This allows "jumps" to arbitrary locations within (the same or other) functions. The following example implements a "loop":

```PicoLisp
(de foo (N)
   (prinl "This is 'foo'")
   (printsp N)
   (or (=0 (dec 'N)) (run (cddr foo))) )
```

Test:

```txt
: (foo 7)
This is 'foo'
7 6 5 4 3 2 1 -> 0
```



## PL/I

The <code>goto</code> statement causes control to be transferred to a labeled statement in the current or any outer procedure.

A <code>goto</code> statement cannot transfer control:
* Into an inactive <code>begin</code> block.
* From outside into a <code>do</code> group.


In structured programming, the only place where a <code>goto</code> can be is inside <code>on</code> units to handle exceptions without recursion.


```pli>on conversion goto restart;</lang



## PL/SQL

PL/SQL supports both GOTOs and structured exception handling:

```PL/SQL
DECLARE
    i number := 5;
    divide_by_zero EXCEPTION;
    PRAGMA exception_init(divide_by_zero, -20000);
BEGIN
    DBMS_OUTPUT.put_line( 'startLoop' );
    <<startLoop>>
        BEGIN
            if i = 0 then
                raise divide_by_zero;
            end if;
            DBMS_OUTPUT.put_line( 100/i );
            i := i - 1;
            GOTO startLoop;
        EXCEPTION
        WHEN divide_by_zero THEN
            DBMS_OUTPUT.put_line( 'Oops!' );
            GOTO finally;
        END;
    <<endLoop>>
    DBMS_OUTPUT.put_line( 'endLoop' );

    <<finally>>
    DBMS_OUTPUT.put_line( 'Finally' );
END;
/
```

```txt
startLoop
20
25
33.33333333333333333333333333333333333333
50
100
Oops!
Finally
```



## PowerShell

A Break statement can include a label. If you use the Break keyword with
a label, Windows PowerShell exits the labeled loop instead of exiting the
current loop. The syntax for a label is as follows (this example shows a
label in a While loop):

```PowerShell

:myLabel while (<condition>) { <statement list>}

```

The label is a colon followed by a name that you assign. The label must be
the first token in a statement, and it must be followed by the looping
keyword, such as While.


In Windows PowerShell, only loop keywords, such as Foreach, For, and While
can have a label.


Break moves execution out of the labeled loop. In embedded loops, this has
a different result than the Break keyword has when it is used by itself.
This schematic example has a While statement with a For statement:


```PowerShell

    :myLabel while (<condition 1>)
    {
        for ($item in $items)
        {
            if (<condition 2>) { break myLabel }
            $item = $x   # A statement inside the For-loop
        }
    }
    $a = $c  # A statement after the labeled While-loop

```


If condition 2 evaluates to True, the execution of the script skips down
to the statement after the labeled loop. In the example, execution starts
again with the statement "$a = $c".

You can nest many labeled loops, as shown in the following schematic
example.

```PowerShell

:red while (<condition1>)
{
    :yellow while (<condition2>)
    {
        while (<condition3>)
        {
            if ($a) {break}
            if ($b) {break red}
            if ($c) {break yellow}
        }
        # After innermost loop
    }
    # After "yellow" loop
}
# After "red" loop

```

If the $b variable evaluates to True, execution of the script resumes
after the loop that is labeled "red". If the $c variable evaluates to
True, execution of the script control resumes after the loop that is
labeled "yellow".

If the $a variable evaluates to True, execution resumes after the innermost
loop. No label is needed.

Windows PowerShell does not limit how far labels can resume execution. The
label can even pass control across script and function call boundaries.


## PureBasic


```purebasic
OnErrorGoto(?ErrorHandler)
OpenConsole()
Gosub label4
Goto label3

label1:
Print("eins ")
Return

label2:
Print("zwei ")
Return

label3:
Print("drei ")

label4:
While i<3
  i+1
  Gosub label1
  Gosub label2
Wend
Print("- ")
i+1
If i<=4 : Return : EndIf
x.i=Val(Input()) : y=1/x
Input()
End

ErrorHandler:
PrintN(ErrorMessage()) : Goto label4
```

```txt
eins zwei eins zwei eins zwei - drei -
```



## Python

Python has both [[Exceptions#Python|exceptions]] and [[Generator#Python|generators]] but no unstructured goto ability.

The "goto" module was an April Fool's joke, published on 1st April 2004. Yes, it works, but it's a joke nevertheless. Please don't use it in real code! For those who like computer languages with a sense of humour it can be downloded [http://entrian.com/goto/goto-1.0.tar.gz here]. It is well documented and comes with many examples. My favorite:

```Python

# Example 2: Restarting a loop:
from goto import goto, label
label .start
for i in range(1, 4):
    print i
    if i == 2:
        try:
            output = message
        except NameError:
            print "Oops - forgot to define 'message'!  Start again."
            message = "Hello world"
            goto .start
print output, "\n"

```

It goes the extra mile and adds a <code>comefrom</code> keyword. This should be used only if you are evil and proud of it. It is reported to have caused maintenance programmers to have so strongly believed themselves insane that it became so. They are now under strong medication in padded cells. Basically whenever the code passes a label it jumps to the comefrom point. For best results I advise writing the comefrom code as far as possible from the label and using no comments near the label.


## Racket

Racket, being a descendant of Scheme, supports full continuations.

As a little example:

```racket
#lang racket
(define (never-divides-by-zero return)
  (displayln "I'm here")
  (return "Leaving")
  (displayln "Never going to reach this")
  (/ 1 0))

(call/cc never-divides-by-zero)

;    outputs:
;        I'm here
;        "Leaving"    (because that's what the function returns)

```


Here, return is the program continuation being passed to the function, when it is called, the string "Leaving" i the result of the function and the following code is never executed.

A much more complicated example done [http://blog.racket-lang.org/2007/07/callcc-and-self-modifying-code.html here]
Where we generate elements of a list one at a time:


```racket
#lang racket
;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end-off-the-list)
(define (generate-one-element-at-a-time a-list)
  ;; (-> X u 'you-fell-off-the-end-off-the-list)
  ;; this is the actual generator, producing one item from a-list at a time
  (define (generator)
     (call/cc control-state))
  ;; [CONTINUATION X] -> EMPTY
  ;; hand the next item from a-list to "return" (or an end-of-list marker)'
  (define (control-state return)
     (for-each
        (lambda (an-element-from-a-list)
           (set! return ;; fixed
             (call/cc
               (lambda (resume-here)
                 (set! control-state resume-here)
                 (return an-element-from-a-list)))))
        a-list)
     (return 'you-fell-off-the-end-off-the-list))
  ;; time to return the generator
  generator)

```


== {{header|Retro}} ==
Retro allows the user to jump to any address.


```Retro
: foo #19 &n:inc jump #21 ;
```


When executed, the stack will contain 20; control does not return to the '''foo''' function.


## REXX


### REXX signal

Note: some REXXes don't allow jumping into a '''DO''' loop, although the language specificiations appear to allow it,

as long as the '''END''' or the '''DO''' loop isn't executed.

The following used PC/REXX to illustrate this example.

```rexx
/*REXX pgm demonstrates various jumps (GOTOs).  In REXX, it's a SIGNAL. */
say 'starting...'
signal aJump
say 'this statement is never executed.'

aJump:  say 'and here we are at aJump.'
                                           do j=1  to 10
                                           say  'j=' j
                                           if j==7 then signal bJump
                                           end  /*j*/
bJump:  say 'and here we are at bJump.'

signal cJump
say 'this statement is never executed.'
                                           do k=1 to 10
                                           say 'k=' k
                                   cJump:  say 'and here we are at cJump.'
                                           exit
                                           end  /*k*/
```

```txt

starting...
and here we are at aJump.
j= 1
j= 2
j= 3
j= 4
j= 5
j= 6
j= 7
and here we are at bJump.
and here we are at cJump.

```



### compared to PL/I goto

After a rather longwinded discussion on the subject I offer here my view:
Whereas PL/I disallows the use of GOTO to jump to anywhere

```txt

Compiler Messages
Message       Line.File Message Description
IBM1847I S     212.0    GOTO target is inside a (different) DO loop.

```

Rexx is very liberal as to the use of Signal.
   As mentioned above some implementations may have restrictions on that.
This Signal jumps into a Do loop inside a Procedure:

```rexx
i=13
signal label
say 'This is never executed'
sub: Procedure Expose i
  Do i=1 To 10;
label:
    Say 'label reached, i='i
    Signal real_start
    End
  Return
 real_start:
```

Without the 'Signal real_start' which leads us out of the control structure the program would end with a syntax error when encountering the End correponding to the Do.

I recommend to use Signal only for condition handling and 'global' jumps to labels that are not within some structured constructs such as Do...End
An example:

```rexx
/* REXX ***************************************************************
* 12.12.2012 Walter Pachl
**********************************************************************/
Signal On Syntax
Parse Upper Arg part
If part<>'' Then
  Interpret 'Signal' part
  Say 'Executing default part'
  Signal eoj
a:Say 'executing part A'
  Signal eoj
b:Say 'executing part B'
  Signal eoj
Syntax:
  Say 'argument must be a or b or omitted'
  Exit
eoj: say 'here we could print statistics'
```

This can be useful when the different parts of the program span a few pages.
Also a Signal eoj in order to Exit from any point in the program to some final activities can be useful.

== {{header|Ring}} ==
Ring has no labels and goto statements. The exit statements allows leave the current loop and doesn't contain any label where to go.


## Robotic

There are multiple ways to use labels and goto statements.


### =Simple=

A simple example of labels and goto:

```robotic

. "The label 'touch' is used to let the player touch the robot"
. "to execute the following"
end
: "touch"
goto "label_b"

: "label_a"
* "Label A was reached"
end

: "label_b"
* "Label B was reached"
end

```


It prints "Label B was reached", skipping "label_a" entirely.


### =Conditional=

This will jump to a given label, depending on the condition of "local1":

```robotic

set "local1" to 2
end
: "touch"
if "local1" = 1 then "label_a"
if "local1" = 2 then "label_b"
end

: "label_a"
* "Label A was reached"
end

: "label_b"
* "Label B was reached"
end

```


Since "local1" equals 2, it prints "Label B was reached".


### =Label Zapping=

When you goto a label, it chooses the top-most label name in the code.
However, with the use of "zap" and "restore", we can use the same label name multiple times in our code:

```robotic

end
: "touch"
goto "label_a"

: "label_a"
* "Label A was reached"
zap "label_a" 1
end

: "label_a"
* "Alternate Label A was reached"
restore "label_a" 1
end

```


When the first label is reached, it zaps "label_a" once. This allows us to reach the second label below. Conversely, restoring "label_a" allows us to go back to the first label once more.


### =Subroutines=

With subroutines, we can jump to a given label and come back to where we left off after we called the inital "goto" statement.
To use these, the labels and the string supplied in the "goto" statements must have a number sign (#):

```robotic

. "The 'wait' statements are used to demonstrate what is happening"
set "local1" to 1
end
: "touch"
goto "#label_a"
wait for 50
* "Done with Label A"
wait for 50
goto "#label_b"
wait for 50
* "Skipped finishing Label B and C"
end

: "#label_a"
* "Label A was reached"
goto "#return"

: "#label_b"
* "Label B was reached"
wait for 50
goto "#label_d"

: "#label_c"
* "Label C was reached"
goto "#return"

: "#label_d"
* "Label D was reached"
goto "#top"

```


The following is printed out in order:
<lang>
Label A was reached
Done with Label A
Label B was reached
Label D was reached
Skipped finishing Label B and C

```


Using "#return", we go back up to the last "goto" statement called. However, using "#top" will ignore all the other subroutines called previously and go straight back to the first "goto" statement that started it all (in this case, goto "#label_b").

== {{header|Ruby}} ==
Ruby programs almost never use continuations. [[MRI]] copies the call stack when it saves or calls a continuation, so continuations are slow.

The next example abuses a continuation to solve [[FizzBuzz#Ruby]]. It is slower and more confusing than an ordinary loop.
```ruby
require 'continuation' unless defined? Continuation

if a = callcc { |c| [c, 1] }
  c, i = a
  c[nil] if i > 100

  case 0
  when i % 3
    print "Fizz"
    case 0
    when i % 5
      print "Buzz"
    end
  when i % 5
    print "Buzz"
  else
    print i
  end

  puts
  c[c, i + 1]
end
```


This code uses the Continuation object <code>c</code> to jump to the top of the loop. For the first iteration, <code>callcc</code> creates <code>c</code> and returns <code>[c, 1]</code>. For later iterations, <code>callcc</code> returns <code>[c, i + 1]</code>. For the last iteration, <code>callcc</code> returns <code>nil</code> to break the loop.


## Scala

Goto's are in the European programmer community [http://en.wikipedia.org/wiki/Considered_harmful considered harmful]. They are error-prune and not essential. A good programmer would stay away from that. Scala is not equipped with goto's.


## SPL

In SPL jumps can be non-conditional and conditional.
This is an example of non-conditional jump to label "myLabel":

```spl
myLabel ->
...
:myLabel
```

This is an example of conditional jump to label "4", which is done if a=0:

```spl
4 -> a=0
...
:4
```

In SPL it is possible not only jump, but also visit a label. "Visiting" a label means that program execution can be returned back to the place from where label was visited.
This is an example of visiting label "label 55":

```spl
label 55 <->
#.output("1")
:label 55
#.output("2")
<-
```

and output is:
```txt

2
1
2

```



## SSEM

The SSEM provides two jump instructions: the absolute jump <tt>000 <operand> to CI</tt> and the relative jump <tt>100 Add <operand> to CI</tt>. The operand in both cases is a memory address, whose contents are to be either loaded into the CI (Current Instruction) register or else added to it. Since CI is incremented <i>after</i> an instruction is executed, not before, the value stored at the operand address must be one less than the value we actually want. For example, this code accomplishes a jump to absolute address 20:

```ssem
10000000000000000000000000000000   0. 1 to CI
11001000000000000000000000000000   1. 19
```

and this accomplishes a relative jump forward by five words:

```ssem
10000000000001000000000000000000  0. Add 1 to CI
00100000000000000000000000000000  1. 4
```



## Tcl

Tcl has both [[Exceptions#Tcl|exceptions]] and (from 8.6 onwards) [[Generator#Tcl|generators/coroutines]] but no unstructured goto ability. However, the main case where it might be desired, coding a general state machine, can be handled through metaprogramming (as discussed at some length on [http://wiki.tcl.tk/8363 the Tcler's Wiki]) so the absence is not strongly felt in practice.


## VBA


```vb
Public Sub jump()
    Debug.Print "VBA only allows"
    GoTo 1
    Debug.Print "no global jumps"
1:
    Debug.Print "jumps in procedures with GoTo"
    Debug.Print "However,"
    On 2 GoSub one, two
    Debug.Print "named in the list after 'GoSub'"
    Debug.Print "and execution will continue on the next line"
    On 1 GoTo one, two
    Debug.Print "For On Error, see Exceptions"
one:
    Debug.Print "On <n> GoTo let you jump to the n-th label"
    Debug.Print "and won't let you continue."
    Exit Sub
two:
    Debug.Print "On <n> GoSub let you jump to the n-th label": Return
End Sub
```
```txt
VBA only allows
jumps in procedures with GoTo
However,
On <n> GoSub let you jump to the n-th label
named in the list after 'GoSub'
and execution will continue on the next line
On <n> GoTo let you jump to the n-th label
and won't let you continue.
```


## VBScript

In VBScript, there is no <code>goto</code> statement. It is a good thing for structured programming.


## zkl

No gotos, just exceptions, generators, switch and the run of the mill looping constructs plus break/continue (which are limited to their loop scope). The VM has jmp instructions that the compiler uses to implement looping but those jmps are limited to function scope.

Having to hack up a goto with exceptions, state or duplicate code can be a PITA but luckily those cases seem to be rare and implementing goto could really bugger the VM state.


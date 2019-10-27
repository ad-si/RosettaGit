+++
title = "Keyboard input/Obtain a Y or N response"
description = ""
date = 2019-08-29T16:11:38Z
aliases = []
[extra]
id = 8435
[taxonomies]
categories = []
tags = []
+++

{{task}} 
[[Category:Simple]]
[[user input::task| ]]

;Task:
Obtain a valid   '''Y'''   or   '''N'''   response from the [[input device::keyboard]]. 

The keyboard should be flushed, so that any outstanding key-presses are removed, preventing any existing   '''Y'''   or   '''N'''   key-press from being evaluated. 

The response should be obtained as soon as   '''Y'''   or   '''N'''   are pressed, and there should be no need to press an   enter   key.





## 8th


```forth

\ get a yes or no response from the keyboard
: yes-no
	con:key $20 bor
	dup 'y n:= if ;; then
	dup 'n n:= if ;; then
	drop yes-no ;
: no? 'n n:= if "No" else "Yes" then . ;

"Yes or no? " con:print  yes-no no?
cr bye

```



## Ada



```Ada
   function Yes_Or_No (Prompt : String := "Your answer (Y/N): ") return Boolean is
      Answer : Character;
   begin
      Ada.Text_IO.Put (Prompt);
      loop
         Ada.Text_IO.Get_Immediate (Answer);
         case Answer is
            when 'Y'|'y' => return True;
            when 'N'|'n' => return False;
            when others  => null;
         end case;
      end loop;
   end Yes_Or_No;
```



## AutoHotkey


```AutoHotkey
Loop, {
	Input, Key, L1
	if (Key = "n" || Key = "y")
		break
}
MsgBox, % "The response was """ Key """."
ExitApp
```



## AWK


```AWK

# syntax: GAWK -f KEYBOARD_INPUT_OBTAIN_A_Y_OR_N_RESPONSE.AWK
BEGIN {
    printf("you entered %s\n",prompt_user())
    exit(0)
}
function prompt_user(  rec) {
# AWK lacks the ability to get keyboard input without pressing the enter key.
    while (1) {
      printf("enter Y or N ")
      getline rec <"con"
      gsub(/ /,"",rec) # optional
      if (rec ~ /^[nyNY]$/) {
        break
      }
    }
    return(rec)
}

```

{{out}}

```txt

enter Y or N y
you entered y

```



## Axe

Since the TI-83/84 require a modifier key to access the letters, this example uses the 2nd key as Y and the Clear key as N.

```axe
While getKey(0)
End

While 1
If getKey(15)
 Disp "N",i
 Return
ElseIf getKey(54)
 Disp "Y",i
 Return
End
End
```



## BASIC


=
## Applesoft BASIC
=


```applesoftbasic
10  LET C =  PEEK (49168): REM CLEAR KEYBOARD
20  PRINT "PRESS Y OR N TO CONTINUE"
30  GET K$
40  IF K$ <  > "Y" AND K$ <  > "N" THEN 30
50  PRINT "THE RESPONSE WAS ";K$

```


=
## BBC BASIC
=

```bbcbasic
      REPEAT UNTIL INKEY$(0) = ""
      PRINT "Press Y or N to continue"
      REPEAT
        key$ = GET$
      UNTIL key$="Y" OR key$="N"
      PRINT "The response was " key$
```

=
## Commodore BASIC
=

```basic
10 PRINT "PRESS Y OR N TO CONTINUE:";
20 POKE 198, 0: REM CLEAR KEY BUFFER
30 GET K$
40 IF K$ <> "Y" AND K$ <> "N" THEN 30
50 PRINT K$
```


Note that 198 is the location of the keyboard buffer index on the VIC-20, C-64, and C-128. On the PET, the correct location is 158, while on the Plus/4 and C-16, it's 239.

The loop on lines 30 - 40 will cycle as fast as the interpreter can go, assigning K$ the empty string until the user presses a key.  On versions of BASIC later than the 2.0 on the VIC and 64 (e.g. 3.5 on the C-16 and Plus/4, 7.0 on the C-128), GETKEY may be used in place of GET.  GETKEY will wait for the user to press a key before continuing, so the polling is done in the BASIC interpreter's machine language code, and the BASIC loop only cycles when the user presses a key other than Y or N.

=
## GWBASIC
=


```gwbasic
10 CLS: PRINT "Press Y or N to continue."
20 WHILE T$<>"y" AND T$<>"Y" AND T$<>"n" AND T$<>"N"
30   T$=""
40   WHILE T$=""
50     T$ = INKEY$
60   WEND
70   IF T$<>"y" AND T$<>"Y" AND T$<>"n" AND T$<>"N" THEN BEEP
80 WEND
90 PRINT "The response was "; T$

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 GET K$ ! Flush the keyboard buffer
110 PRINT "Press Y or N to continue."
120 DO 
130   LET K$=LCASE$(INKEY$)
140 LOOP UNTIL K$="y" OR K$="n"
150 PRINT "The response was ";K$
```


=
## Locomotive Basic
=


```locobasic
10 CLEAR INPUT
20 PRINT "Press Y or N to continue"
30 a$=LOWER$(INKEY$)
40 IF a$="" THEN 30
50 IF a$="y" THEN PRINT "Yes":END
60 IF a$="n" THEN PRINT "No":END
70 PRINT "Try again"
80 GOTO 30
```


=
## ZX Spectrum Basic
=
Note that this will also work in [[GW-BASIC]] and most [[QBasic]]-compatible BASICs if all instances of "<code>GO TO</code>" are changed to "<code>GOTO</code>".


```qbasic
10 IF INKEY$<>"" THEN GO TO 10: REM flush the keyboard buffer
20 PRINT "Press Y or N to continue"
30 LET k$ = INKEY$
40 IF k$ <> "y" AND k$ <> "Y" AND k$ <> "n" AND k$ <> "N" THEN GO TO 30
50 PRINT "The response was "; k$
```



## Batch File


```dos

@echo off
choice
if errorlevel 2 echo You chose N
if errorlevel 1 echo You chose Y
>nul pause

```



## C

For POSIX compliant systems (in theory that includes WinNT family).

```C

#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
 
void set_mode(int want_key)
{
	static struct termios old, new;
	if (!want_key) {
		tcsetattr(STDIN_FILENO, TCSANOW, &old);
		return;
	}
 
	tcgetattr(STDIN_FILENO, &old);
	new = old;
	new.c_lflag &= ~(ICANON);
	tcsetattr(STDIN_FILENO, TCSANOW, &new);
}
 
int get_key(int no_timeout)
{
	int c = 0;
	struct timeval tv;
	fd_set fs;
	tv.tv_usec = tv.tv_sec = 0;
 
	FD_ZERO(&fs);
	FD_SET(STDIN_FILENO, &fs);

	select(STDIN_FILENO + 1, &fs, 0, 0, no_timeout ? 0 : &tv);
	if (FD_ISSET(STDIN_FILENO, &fs)) {
		c = getchar();
		set_mode(0);
	}
	return c;
}
 
int main()
{
	int c;
	while(1) {
		set_mode(1);
		while (get_key(0)); /* clear buffer */
		printf("Prompt again [Y/N]? ");
		fflush(stdout);

		c = get_key(1);
		if (c == 'Y' || c == 'y') {
			printf("\n");
			continue;
		}

		if (c == 'N' || c == 'n') {
			printf("\nDone\n");
			break;
		}

		printf("\nYes or no?\n");
	}

	return 0;
}
```



## C++

Windows specific

```cpp>#include <conio.h

#include <iostream>

using namespace std;

int main()
{
	char ch;
	_cputs( "Yes or no?" );
	do
	{
		ch = _getch();
		ch = toupper( ch );
	} while(ch!='Y'&&ch!='N');

	if(ch=='N')
	{
		cout << "You said no" << endl;
	}
	else
	{
		cout << "You said yes" << endl;
	}
	return 0;
}

```



## C sharp



```c sharp
using System;

namespace Y_or_N
{
    class Program
    {
        static void Main()
        {
            bool response = GetYorN();
        }

        static bool GetYorN()
        {
            ConsoleKey response; // Creates a variable to hold the user's response.

            do
            {
                while (Console.KeyAvailable) // Flushes the input queue.
                    Console.ReadKey();

                Console.Write("Y or N? "); // Asks the user to answer with 'Y' or 'N'.
                response = Console.ReadKey().Key; // Gets the user's response.
                Console.WriteLine(); // Breaks the line.
            } while (response != ConsoleKey.Y && response != ConsoleKey.N); // If the user did not respond with a 'Y' or an 'N', repeat the loop.

             /* 
              * Return true if the user responded with 'Y', otherwise false.
              * 
              * We know the response was either 'Y' or 'N', so we can assume 
              * the response is 'N' if it is not 'Y'.
              */
            return response == ConsoleKey.Y;
        }
    }
}
```



## Clojure

{{libheader|jline}}

Note: If you run it with Leiningen, use the special trampoline run to prevent issues:


```txt
$ lein trampoline run
```




```clojure

(ns yprompt.core
  (:import jline.Terminal)
  (:gen-class))

(defn yes? [k]
  (if (or (= k 89) (= k 121)) true false))

(defn prompt []
    (println "\nPrompt again [Y/N]?")
    (let [term (Terminal/getTerminal)
          ykey (yes? (.readCharacter term System/in))]
      (if-not ykey
        (recur)
        (println "Yes!"))))

(defn -main [& args]
  (prompt))

```



## Common Lisp


=
## LispWorks
=

Version 1:


```lisp

(defun rosetta-y-or-n ()
  (clear-input *query-io*)
  (y-or-n-p))

```


Version 2:


```lisp

(defun y-or-n ()
  (clear-input *standard-input*)
  (loop as dum = (format t "Y or N for yes or no: ")
        as c = (read-char)
        as q = (and (not (equal c #\n)) (not (equal c #\y)))
        when q do (format t "~%Need Y or N~%")
        unless q return (if (equal c #\y) 'yes 'no)))

```


Version 1 and 2 work as required in a LispWorks GUI interface, i.e. they return immediately when the y or n keys are pressed, without waiting for the Enter key.

=
## ncurses
=

When called from a REPL in a Linux terminal, y-or-n-p is ''line buffered'', which means any input has to be confirmed by an Enter key.

In order to have keys available immediately to the program, line buffering has to be disabled in the tty driver. This can be done by utilizing the ncurses terminal library available on most GNU/Linux systems. To interface ncurses from Lisp, the ''croatoan'' library can be used:

Version 3:


```lisp

(defun y-or-no ()
  (with-screen (scr :input-buffering nil :input-blocking t)
    (clear scr)
    (princ "Do you want to continue? [Y/N]" scr)
    (refresh scr)
    (event-case (scr event)
      ((#\Y #\y) (return-from event-case t))
      ((#\N #\n) (return-from event-case nil)))))

```



## D


```d
import std.stdio: stdout, write, writefln;

extern (C) nothrow {
    void _STI_conio();
    void _STD_conio();
    int kbhit();
    int getch();
}

void main() {
    _STI_conio();
    write("Enter Y or N: ");
    stdout.flush();

    int c;
    do {
        while(!kbhit()) {}
        c = getch();

        // Visual feedback for each keypress.
        write(cast(char)c);
        stdout.flush();
    } while(c != 'Y' && c != 'y' && c != 'N' && c != 'n');

    writefln("\nResponse: %c", cast(char)c);
    _STD_conio();
}
```

{{out}}

```txt
Enter Y or N: abcN
Response: N
```



## Elm


```Elm
import Char
import Graphics.Element exposing (Element, empty, show)
import Keyboard


view : Int -> Element
view keyCode =
  let
    char = 
      Char.fromCode keyCode

    showChar =
      toString >> ((++) "The last (y/n) key pressed was: ") >> show
  in
    case char of
      'n' ->
        showChar char

      'y' ->
        showChar char

      _ ->
        empty


main : Signal Element
main =
  Signal.map view Keyboard.presses
```



## ERRE


```ERRE

!$KEY
................
! flush the keyboard buffer
! --------------------------------
! you can use POKE(198,0) in C-64 
! ERRE version
! --------------------------------
REPEAT
  GET(K$)
UNTIL K$=""

PRINT("Press Y or N to continue")
REPEAT
  GET(K$)
UNTIL INSTR("YyNn",K$)<>0
!
! with C-64 you must write a line like 
! UNTIL K$="Y" OR K$="N"
!  

PRINT("The response was ";K$)
.................

```

<code>!$KEY </code> is a directive pragma: using it <code>GET</code> become an equivalent to Qbasic INKEY$, otherwise it's equivalent to QBasic INPUT$(1). !$KEY is also used to mantain portability with the C-64 version of ERRE language.


## Euphoria


```Euphoria
integer key

puts(1,"Your answer? (Y/N)\n")
while get_key()!=-1 do
end while

while 1 do
    key = get_key()
    if key!=-1 and (key = 'Y' or key = 'y' or key = 'N' or key = 'n') then
        exit
    end if
end while

printf(1,"Your response was %s\n",key)
```



## EGL

{{Works with|EDT}}
{{Works with|RBD}}

```EGL
handler YesOrNoHandler type RUIhandler{initialUI =[ui], onConstructionFunction = start}

    ui Div { };
	
    const KEY_N int = 78;
    const KEY_Y int = 89;
	
    function start()
    	document.onKeyDown = d_onKeyDown;
    end
    
    function d_onKeyDown(e Event in)
		
	case (e.ch)
  	    when (KEY_N)
	        ui.innerText = "N pressed.";
	    when (KEY_Y)
	        ui.innerText = "Y pressed.";
	end
	
	e.preventDefault();
    
    end
	
end
```



=={{header|F_Sharp|F#}}==

```fsharp
open System

let rec yorn () =
    let rec flush () = if Console.KeyAvailable then ignore (Console.ReadKey()); flush ()
    flush ()

    printf "\nY or N? "
    match Console.ReadKey().Key with
    | ConsoleKey.Y -> 'Y'
    | ConsoleKey.N -> 'N'
    | _ -> yorn()

printfn "\nYour choice: %c" (yorn())
```



## Forth


```Forth
: flush ( -- )  \ discard pending input
  begin key? while key drop repeat ;

: y-or-n ( c-addr u -- f )
  flush begin
    cr 2dup type key bl or                  \ note 1.
    dup [char] y = swap [char] n = over or  \ note 2.
    if nip nip exit then
  drop again ;

\ Note 1. KEY BL OR returns a lowercase letter in the case that an
\ uppercase letter was entered, an unchanged lowercase letter in the
\ case that a lowercase letter was entered, and garbage otherwise.  BL
\ returns the ASCII code for a space, 32, which is incidentally the
\ "bit of difference" between ASCII uppercase and lowercase letters.

\ Note 2. this line has the stack effect ( x -- f1 f2 ), where F1 is
\ true only if x='y', and F2 is true only if x='y' OR if x='n'.

\ I think these expressions aren't too clever, but they _are_ rather
\ optimized for the task at hand.  This might be more conventional:

: y-or-n ( c-addr u -- f )
  flush begin
    cr 2dup type key case
      [char] y of 2drop true  exit endof
      [char] Y of 2drop true  exit endof
      [char] n of 2drop false exit endof
      [char] N of 2drop false exit endof
  endcase again ;
```



## Fortran

Standard Fortran has no special I/O statements that allow asynchronous actions (such as the KeyPressed and ReadKey functions of Turbo Pascal), so input is awaited in the usual fashion and a prompt should be supplied to indicate to the reader that a response is awaited, otherwise the user will confront a blank screen with nothing happening and will have to guess what might be expected. Further, there is no scheme for knowing if impending input has been waiting in an input buffer since before the need for a question arose, so it is not possible to flush such lines before requesting the special input. Impatience at the screenface can prompt typing ahead so that the next command will be immediately available but incorrectly anticipated input will likely wreck the run, though for yes/no responses you may be rescued if such input does not conform to the required form: the bad input will be ignored and the question asked afresh. Thus, the details of the specification cannot be met via standard Fortran, though a given system may have special subroutines equivalent to KeyPressed, etc. available.   

Even so, asking questions can often be useful when messing about with tests, etc., so some routines for this can help. These were devised afresh at the Culham Science Centre, so there was some language generality:

```Fortran

      CHARACTER*120 FUNCTION REPLY(QUERY)    !Obtain a text in reply.
Concocted by R.N.McLean (whom God preserve), December MM.
       CHARACTER*(*) QUERY	!The question.
       CHARACTER*120 TEXT	!Alas, oh for proper strings.
       INTEGER MSG,KEYS,LSTNB	!Let's hope everyone has the same type.
       COMMON /IOUNITS/ MSG,KEYS!Orifices.
        WRITE (MSG,1) QUERY(1:LSTNB(QUERY)),"?"!So, splurt.
    1   FORMAT (2A,$)		!A trailing text literal may not be rolled.
        READ (KEYS,1) TEXT	!Dare not use REPLY itself. Some implementations bungle.
        REPLY = TEXT		!So, shuffle.
       RETURN			!Take that.
      END 			!Others interpret the reply.

      REAL*8 FUNCTION REPLYN(QUERY)	!Obtain a number in reply.
Concocted by R.N.McLean (whom God preserve), December MM.
       CHARACTER*(*) QUERY	!The question.
       REAL X			!The answer, presumably not 42.
       INTEGER MSG,KEYS,LSTNB	!Let's hope everyone has the same type.
       COMMON /IOUNITS/ MSG,KEYS!Orifices.
    1   WRITE (MSG,2) QUERY(1:LSTNB(QUERY))	!No trailing spaces.
    2   FORMAT (A,$)		!The $ obviously suppresses the newline.
        READ (KEYS,*,ERR = 3) X	!Presume adequate testing for now.
        REPLYN = X		!The value!
       RETURN			!All done.
    3   WRITE (MSG,4)		!Or perhaps not.
    4   FORMAT ('Distasteful number. Try again...')	!All sorts of ways.
        GO TO 1			!My patience is unconditional.
      END			!One way or another, a number will be secured.

      LOGICAL FUNCTION YEA(QUERY)	!Obtain a Yes in reply?
Concocted by R.N.McLean (whom God preserve), December MM.
       CHARACTER*(*) QUERY	!The question.
       CHARACTER*120 WHAT,REPLY	!Quite so.
       CHARACTER*1 C		!Scratchpad.
       INTEGER MSG,KEYS		!Let's hope everyone has the same type.
       COMMON /IOUNITS/ MSG,KEYS!Orifices.
       INTEGER L		!A finger.
    1   WHAT = REPLY(QUERY)	!So, get an answer.
        DO L = 1,LEN(WHAT)	!Sigh. Oh for Trim(string)
          C = WHAT(L:L)		!Sniff a CHARACTER.
          IF (C .NE. ' ') GO TO 10	!A starter?
        END DO			!No. Try further on.
        WRITE (MSG,2)		!Surely not.
    2   FORMAT ('All blank?')	!Poke.
    3   WRITE (MSG,4) 		!Sigh.
    4   FORMAT ('I dig it not. Try Yes/Si/Da/Oui/Ja, or No')
        GO TO 1			!Get it right, this time?
   10   IF (INDEX('YySsDdOoJj',C) .GT. 0) THEN	!Yes/Si/Da/Oui/Ja...
          YEA = .TRUE.		!A decision.
        ELSE IF (INDEX('Nn',C) .GT. 0) THEN	!No,No,Nyet,Non...
          YEA = .FALSE.		!Even if negative.
        ELSE			!But if unrecognised,
          GO TO 3		!Try again.
        END IF			!So much for choices.
       RETURN			!Pass the word.
      END			!Enough of yes-beings.
      LOGICAL FUNCTION NAY(QUERY)	!Perhaps this reads better.
Concocted by R.N.McLean (whom God preserve), December MM.
       CHARACTER*(*) QUERY	!The question.
       LOGICAL YEA		!Let us hope so.
        NAY = .NOT.YEA(QUERY)	!Straightforward.
       RETURN			!Pass the inverted word.
      END			!So much for naysayers.

```

Usage might be something like <code>IF (NAY("Keep the results")) CALL PURGE</code>


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

While InKey <> "" : Wend '' flush keyboard buffer
Print "Do you want to continue y/n : ";
Dim answer As String

Do
  answer = LCase(Inkey)
Loop Until answer = "y" OrElse answer = "n"

Print answer '' echo response to console
If answer = "y" Then
  Print "OK, continuing"
Else
  Print "OK, finishing"
End If

Sleep
```


Sample input/output:

{{out}}

```txt

Do you want to continue y/n : y
OK, continuing

```



## FutureBasic


```futurebasic

local fn DoDialog
dim as long ev, id

ev = dialog(0)
id = dialog(ev)

select case( ev )
  case _wndClose : end
  case _evKey
    select id
     // Trap upper and lower case Y and N
     case 78, 110 : cls : print "No "
     case 89, 121 : cls : print "Yes"
    end select
  end select
end fn

on dialog fn DoDialog

window 1, @"Yes-No", (0,0)-(150,80), _docNoGrow
text _applFont, 14, _boldBit%

RunApplicationEventLoop()

```



## GlovePIE


```glovepie
if var.end=0 then
var.end=0
debug="Press the Y key or the N key to continue:"
endif
if pressed(Key.Y)and var.end=0 then
var.end=1
debug="You pressed the Y key."
endif
if pressed(Key.N)and var.end=0 then
var.end=1
debug="You pressed the N key."
endif
```



## Go

{{libheader|Curses}}

```go
package main

import (
    "log"

    gc "code.google.com/p/goncurses"
)

func main() {
    s, err := gc.Init()
    if err != nil {
        log.Fatal("init:", err)
    }
    defer gc.End()
    var k gc.Key
    for {
        gc.FlushInput()
        s.MovePrint(20, 0, "Press y/n ")
        s.Refresh()
        switch k = s.GetChar(); k {
        default:
            continue
        case 'y', 'Y', 'n', 'N':
        }
        break
    }
    s.Printf("\nThanks for the %c!\n", k)
    s.Refresh()
    s.GetChar()
}
```


=={{header|GW-BASIC}}==

```qbasic
10 IF INKEY$<>"" THEN GOTO 10: REM flush the keyboard buffer
20 PRINT "Press Y or N to continue"
30 LET k$ = INKEY$
40 IF k$ <> "y" AND k$ <> "Y" AND k$ <> "n" AND k$ <> "N" THEN GOTO 30
50 PRINT "The response was "; k$
```



## Haskell


This may not be very idiomatic; it's pretty monad-oriented, and the use of do expressions makes the whole thing feel rather imperative.


```haskell
import System.IO

hFlushInput :: Handle -> IO ()
hFlushInput hdl = do
  r <- hReady hdl
  if r then do
    c <- hGetChar hdl
    hFlushInput hdl
  else
    return ()

yorn :: IO Char
yorn = do
  c <- getChar
  if c == 'Y' || c == 'N' then return c
  else if c == 'y' then return 'Y'
  else if c == 'n' then return 'N'
  else yorn

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Press Y or N to continue: "

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hFlushInput stdin
  answer <- yorn
  putStrLn [answer]
```


=={{header|Icon}} and {{header|Unicon}}==

This solution works in both Icon and Unicon.  It also accepts <tt>y</tt> or <tt>n</tt>.

```unicon
procedure main()
    write("Response was ",getResponse("OK? (Y or N): "))
end 

procedure getResponse(prompt)
    while kbhit() do getch()   # flush input
    writes(prompt)
    repeat if map(answer := getch()) == ("y"|"n") then break
    return answer
end
```



## Inform 7


Keyboard input goes through a virtual machine that's only required to provide blocking input operations, so flushing the buffer isn't possible.

Inform 7 has a built-in function to ask the user for yes-or-no input, but it requires them to press enter afterward:

```inform7
Qwantz is a room.

When play begins:
	say "A wizard has turned you into a whale. Is this awesome (Y/N)? ";
	if the player consents, say "Awesome!";
	end the story.
```


To read a single key without waiting for enter, we can redefine the function by including a snippet of Inform 6 code:

```inform7
To decide whether player consents: (- (YesOrNoKey()) -).

Include (-
[ YesOrNoKey ch;
    do { ch = VM_KeyChar(); } until (ch == 'y' or 'Y' or 'n' or 'N');
    return ch == 'y' or 'Y';
]; -).
```



## JavaScript


Here's a synchronous ES6 implementation. The synchronous code must be executed in an async function definition. In this example, `wait_key` returns the key pressed and `done` must be called decouple the listening to stdin and end the process. The example pauses for a second to show that the keys pressed before `wait_key` is called are not heard.


```javascript
const readline = require('readline');
readline.emitKeypressEvents(process.stdin);
process.stdin.setRawMode(true);

var wait_key = async function() {
  return await new Promise(function(resolve,reject) {
    var key_listen = function(str,key) {
      process.stdin.removeListener('keypress', key_listen);
      resolve(str);
    }
    process.stdin.on('keypress', key_listen);
  });
}

var done = function() {
  process.exit();
}

var go = async function() {
  do {
    console.log('Press any key...');
    var key = await wait_key();
    console.log("Key pressed is",key);
    await new Promise(function(resolve) { setTimeout(resolve,1000); });
  } while(key != 'y');
  done();
}

go();

```


Here's how you can asynchronously read a single character in Node.js, using the <code>keypress</code> package.
This does not seem to be possible to do synchronously in Node.js or at all in the SpiderMonkey shell.


```javascript
var keypress = require('keypress');

keypress(process.stdin);

process.stdin.on('keypress', function (ch, key) {
    if (key && (key.name === 'y' || key.name === 'n')) {
       console.log('Reply:' + key.name);
    }
});

process.stdin.setRawMode(true);
process.stdin.resume();
```


Using DOM events.


```javascript
document.body.addEventListener('keyup', function (e) {
  var key = String.fromCharCode(e.keyCode).toLowerCase();
  if (key === 'y' || key === 'n') {
    console.log('response is: ' + key);
  }
}, false);
```



## Julia

Uses the Gtk library.

```julia
using Gtk.ShortNames
 
function keypresswindow()

    # This code creates  the Gtk widgets on the screen.
    txt = "Type Y or N"
    win = Window("Keypress Test", 250, 30) |> (Frame() |> ((vbox = Box(:v)) |> (lab = Label(txt))))
    
    # this is the keystroke processing code, a function and a callback for the function.
    function keycall(w, event)
        ch = Char(event.keyval)
        set_gtk_property!(lab,:label, ch in('n','N','y','Y') ? "You hit the $ch key." : txt)
    end
    Gtk.signal_connect(keycall, win, "key-press-event")

    # this code sets up a proper exit when the widow is closed.
    c = Condition()
    endit(w) = notify(c)
    Gtk.signal_connect(endit, win, :destroy)
    Gtk.showall(win)
    wait(c)
end
 
keypresswindow()

```



## Kotlin


```scala
// version 1.0.6

import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import javax.swing.JFrame
import javax.swing.SwingUtilities

class Test: JFrame() {
    init {
        while (System.`in`.available() > 0) System.`in`.read()
        println("Do you want to quit Y/N")
        addKeyListener(object: KeyAdapter() {
            override fun keyPressed(e: KeyEvent) {
                if (e.keyCode == KeyEvent.VK_Y) {
                    println("OK, quitting")
                    quit()
                } else if (e.keyCode == KeyEvent.VK_N) {
                    println("N was pressed but the program is about to end anyway")
                    quit()
                } else {
                    println("Only Y/N are acceptable, please try again")
                }
            }
        })
    }

    private fun quit() {
        isVisible = false
        dispose()
        System.exit(0)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = Test()
        f.isFocusable = true
        f.isVisible = true
    }
}
```



## Liberty BASIC


```lb

nomainwin
open "Y/N" for graphics_nsb_nf as #1
#1 "trapclose Quit"
#1 "down;setfocus;when characterInput KeyCheck"
#1 "place 10 50;\Press Y or N"
Inkey$=""
wait

sub KeyCheck hndle$,k$
    k$=upper$(k$)
    #hndle$ "cls;place 10 50"
    select case k$
        case "Y"
        #hndle$ "\ Yes"
        case "N"
        #hndle$ "\No"
        case else
        #hndle$ "\Incorrect input. Press Y or N"
        end select
    end sub

sub Quit hndle$
    close #hndle$
    end
    end sub
 
```



## LiveCode

In the Card script, add a handler for the OpenCard event, putting empty into the text field. 

In the text field, put the following in its code

```LiveCode
on KeyDown k
    if toUpper(k) is among the items of "Y,N" then 
        answer "Thanks for your response"
    else
        answer "You need to enter Y or N"
    end if
    put empty into me
end KeyDown
```


n.b. This sort of confirmation in GUI apps is usually presented as a dialog box with Yes/No buttons, which automatically handles keyboard input.


## Logo


```logo
to yorn
  type [Press Y or N to continue: ]
  local "clear
  make "clear readchars 0 ; clear input buffer
  local "yorn
  do.until [make "yorn readchar] [or equal? :yorn "Y equal? :yorn "N]
  print :yorn
  output :yorn
end
```




## Mathematica


```Mathematica
CreateDialog[TextCell["Yes or no?[Y/N]"],
  NotebookEventActions -> {
    "KeyDown" :> Switch[ToUpperCase@CurrentValue["EventKey"],
      "Y", Print["You said yes"]; DialogReturn[],
      "N", Print["You said no"]; DialogReturn[]
      ]}];
```



## M2000 Interpreter

===Simple Loop using Key$===
If keyboard is Greek the we have to change to English. Other examples use Keyboard codes.

```M2000 Interpreter

Module Simple {
            \\ a small modification from BBC BASIC entry
            REPEAT {} UNTIL INKEY$ = ""
            PRINT "Press Y or N to continue"
            REPEAT {
                    k$ =Ucase$(Key$)
            }  UNTIL K$="Y" OR k$="N"
            PRINT "The response was "; k$
}
Simple

```



### Use a Function to return keypress and by reference return value


```M2000 Interpreter

Module Checkit {
      Function GetYN$ (&Ret) {
            const Y=0x59
            const N=0x4E
            Ret=False
            Do {
                  if keypress(Y) then Ret=True : exit
                  if keypress(N) then exit
                  drop$=inkey$
            } Always
            K$=key$
            do {} until  filter$(Inkey$,k$)=""
            =Ucase$(K$)
      }
      keyboard "abcde" ' feed keyboard (inkey$ get these characters)
      Y=0
      Print "Your answer (Y/N):"; GetYN$(&Y)
      Print Y
}
Checkit

```



### Using Thread to read/write Keyboard buffer

We use a thread, using after, for one run, after 10ms, when Input wait for keypress. So when call to GetYN module exit has Y or N with Enter to keyboard. Now Input finish.

Threads runs in same namespace as the module they created. So module name and Y variable are visible.Module GetYN can't read parent module variables, except M which declared as GLOBAL. After 500ms N is returned.

Using Profiler and Print Timecount we get the real duration (using high resolution timer), of response.


```M2000 Interpreter

Module CheckisToo {
      Module GetYN (&Ret) {
            const Y=0x59
            const N=0x4E
            Ret=False
            Do {
                  If M>50 then Keyboard "N" : exit
                  if keypress(Y) then Ret=True : exit
                  if keypress(N) then exit
                  drop$=inkey$
                  \\ ensure thread MM run using wait 
                  wait 1
            } Always
            Keyboard Ucase$(Key$)+Chr$(13)
      }
      keyboard "abcde"
      Y=0
      Global M=0
      Thread {
            M++
      } as MM interval 10
      While Inkey$<>"" {}
      After 10 { 
            Module GetYN &Y 
      }
      Profiler
      Input "Your answer (Y/N):", A$
      Print timecount
      Print Y, M
      Threads Erase
}
CheckisToo

```


===Using User Form (GUI)===

```M2000 Interpreter

Module UseUIForm {
      Const Y=0x59, N=0x4E, Center=2
      Ret=False
      Declare Form1 form
      Layer Form1 {
            Window 22, 8000, 4000;
            Cls #333333,0
            Cursor 0, Height/2
            Report Center, "Press (Y/N)"
      }
      Function form1.Keydown {
                 Read New &key, &shiftKey
                 IF key=Y then  ret=True : Method Form1, "CloseNow"
                 If key=N Then Method Form1, "CloseNow"
      }
      Method Form1, "Show", 1  ' modal show
      Print Ret
      Declare Form1 Nothing
}
UseUIForm

```



## Microsoft Small Basic


Submitted by: '''AykayayCiti''' (''Earl L. Montgomery'') on Mar 19, 2018.  
Once you hit a key a separate dialog box will appear. Place them side by side to see the results.

```vb
'From:
'Andy Oneill, 2-6-2015, "Small Basic: Key Input,
'" TechNet, https://social.technet.microsoft.com/wiki/contents/articles/29850.small-basic-key-input.aspx, accessed 3-19-2018

GraphicsWindow.DrawText(10, 10, "Hit any key to dump.")
GraphicsWindow.KeyDown = OnKeyDown
Sub OnKeyDown
  TextWindow.WriteLine(GraphicsWindow.LastKey)
EndSub
```



## MiniScript

Access to hardware like the keyboard is very dependent on the host app, but here's a version that works with [https://miniscript.org/MiniMicro/ MiniMicro], a standardized MiniScript virtual machine.


```MiniScript
// flush the keyboard
while key.available
    key.get
end while

// and now prompt and wait for Y or N
print "Press Y or N:"
k = ""
while k != "Y" and k != "N"
    k = key.get.upper
end while
print "You pressed: " + k
```



## MUMPS

{{works with|Caché ObjectScript}}
Version from terminal shown below.

```MUMPS
for  read !,"Enter Y or N to continue: ",input  quit:input?1(1"Y",1"y",1"N",1"n")
```


{{out}}
```txt
Enter Y or N to continue: J
Enter Y or N to continue: YES
Enter Y or N to continue: no
Enter Y or N to continue: N
SAMPLES>        
```



## NetRexx


```netrexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

Say 'Please enter Y or N'
parse ask c
Select
  when c='Y' Then Say 'YES'
  when c='N' Then Say 'NO'
  otherwise       Say 'Undecided'
  End 
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 PRINT "PRESS Y OR N TO CONTINUE."
20 IF INKEY$<>"Y" AND INKEY$<>"N" THEN GOTO 20
30 PRINT "THE RESPONSE WAS ";INKEY$;"."
```



## Oforth



```Oforth
import: console

: YorN
| c |
   System.Console flush
   doWhile: [
      System.Console receiveChar toUpper ->c
      c 'Y' <> c 'N' <> and
      ]
   c ;
```



## OpenEdge/Progress


```progress
DEF VAR lanswer AS LOGICAL INITIAL ?.

DO WHILE lanswer = ?:
   READKEY.
   IF CHR( LASTKEY ) = "n" OR CHR( LASTKEY ) = "y" THEN
      lanswer = CHR( LASTKEY ) = "y".
END.

MESSAGE lanswer VIEW-AS ALERT-BOX.
```



## PARI/GP


GP's <code>input</code> is not able to read an unbuffered single character, so one must use PARI where the solution is identical to that of [[#C|C]].


## Pascal

{{works with|Free_Pascal}}
{{libheader|CRT}}

```pascal
Program ObtainYN;

uses
  crt;

var
  key: char;

begin
  write('Your answer? (Y/N): ');
  repeat
    key := readkey;
  until (key in ['Y', 'y', 'N', 'n']);
  writeln;
  writeln ('Your answer was: ', key);
end.
```

Output:

```txt
% ./ObtainYN
Your answer? (Y/N): 
Your answer was: y

```



## Perl


```perl
use Term::ReadKey;

ReadMode 4; # change to raw input mode

my $key = '';

while($key !~ /(Y|N)/i) {
    1 while defined ReadKey -1; # discard any previous input
    print "Type Y/N: ";
    $key = ReadKey 0; # read a single character
    print "$key\n";
}

ReadMode 0; # reset the terminal to normal mode

print "\nYou typed: $key\n";

```


## Perl 6


```perl6
my $TTY = open("/dev/tty");

sub prompt-char($prompt) {
    ENTER shell "stty raw -echo min 1 time 1";
    LEAVE shell "stty sane";

    print $prompt;
    $TTY.read(1).decode('latin1');
}

say so prompt-char("Y or N? ") ~~ /:i y/;
```



## Phix


```Phix
integer key
 
while get_key()!=-1 do end while -- flush

puts(1,"Your answer? (Y/N)")
 
while 1 do
    key = upper(get_key())
    if find(key,"YN") then exit end if
end while
 
printf(1,"\nYour response was %s\n",key)
```



## PicoLisp


```PicoLisp
(de yesno ()
   (loop
      (NIL (uppc (key)))
      (T (= "Y" @) T)
      (T (= "N" @)) ) )
```



## PL/I


```pli
 yn: Proc Options(main):
 Dcl sysin stream input;
 Dcl sysprint stream output;
 Dcl c Char(1);
 Put Skip List('Please enter Y or N');
 Get Edit(c)(a(1));
 Select(c);
   When('Y','y','J','j')
     Put Skip List('YES');
   When('N','n')
     Put Skip List('NO');
   Otherwise
     Put Skip List('Undecided?');
   End;
 End;
```



## PowerShell

This is for console use only.  The ISE is geared for a different type of input.

```PowerShell

do
{
    $keyPress = [System.Console]::ReadKey()
}
until ($keyPress.Key -eq "Y" -or $keyPress.Key -eq "N")

$keyPress | Format-Table -AutoSize

```

If the user pressed the "Y" key...
{{Out}}

```txt

KeyChar Key Modifiers
------- --- ---------
      y   Y         0

```

If the user pressed the "N" key...
{{Out}}

```txt

KeyChar Key Modifiers
------- --- ---------
      n   N         0

```



## PureBasic

Inkey() returns the character string of the key which is being pressed at the time. 

```PureBasic
PrintN("Press Y or N to continue")

Repeat
  ; Get the key being pressed, or a empty string.
  Key$=UCase(Inkey())
  ;
  ; To Reduce the problems with an active loop
  ; a Delay(1) will release the CPU for the rest
  ; of this quanta if no key where pressed.
  Delay(1)
Until   Key$="Y" Or Key$="N"
PrintN("The response was "+Key$)
```



## Python


```python
#!/usr/bin/env python

try:
    from msvcrt import getch
except ImportError:
    def getch():
        import sys, tty, termios
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(sys.stdin.fileno())
            ch = sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch

print "Press Y or N to continue"
while True:
    char = getch()
    if char.lower() in ("y", "n"):
        print char
        break
```

----

```python
#!/usr/bin/env python
# -*- coding: utf-8 -*-
from curses import wrapper
#
#
def main(stdscr):
  # const
  #y = ord("y")
  #n = ord("n")
  while True:
    # keyboard input interceptor|listener
    #window.nodelay(yes)
    # - If yes is 1, getch() will be non-blocking.
    # return char code
    #kb_Inpt = stdscr.getch()
    # return string
    kb_Inpt = stdscr.getkey()
    #if kb_Inpt == (y or n):
    if kb_Inpt.lower() == ('y' or 'n'):
      break
      return None
  #
  return None
#
#*** unit test ***#
if __name__ == "__main__":
  #
  wrapper(main)
```



## QUACKASM

Note: The following is not a full program (it is only a subroutine, using standard calling conventions), nor does it flush the keyboard buffer (there is no standard way to do this in QUACKVM; it may be possible using extensions, but none are currently defined).

```quackasm

; Stores result in cell 2; 1 if yes, 0 if no.
:YORN
PRINT YORNMSG
:YORN1
INPUT >2
AND *2,$5F,'Y >2 /YORN2
AND *2,,'N \YORN1
:YORN2
PRINTC *2
PRINTC 13
AND *2,1 >2
RETURN
:YORNMSG " (Y/N)? \

```



## Racket


```Racket

#lang racket

;; GUI version
(require racket/gui)
(message-box "Yes/No example" "Yes or no?" #f '(yes-no))

;; Text version, via stty
(define stty
  (let ([exe (find-executable-path "stty")])
    (λ args (void (apply system* exe args)))))
(define tty-settings (string-trim (with-output-to-string (λ() (stty "-g")))))
(printf "Yes or no? ") (flush-output)
(stty "-icanon" "-echo" "min" "1")
(let loop () (when (char-ready?) (loop)))
(let loop ()
  (define ch (read-char))
  (case (char-downcase ch)
    [(#\y #\Y #\n #\N) (displayln ch) (if (memq ch '(#\y #\Y)) 'yes 'no)]
    [else (loop)]))
(stty tty-settings)

```



## Retro


```Retro

  : y|n ( -c )
    "\nPress Y or N..." puts
    0 [ drop getc dup [ 'Y <> ] [ 'N <> ] bi and ] while cr ;

```



## REXX


### version for all classic REXXes

This version works with all classic REXXes.


REXX (in general) requires the user to press the   '''ENTER'''   key after entering text.  

This is because the original (IBM) REXX was designed and written for a system when all I/O to a user's terminal screen was

in block mode and required the user to press one of the following before any data was sent to the computer:
* the   '''ENTER'''   key
* a   '''PF'''     (program function key)
* a   '''PA'''     (program assist key)
* the   '''ATTN'''     (attention) key
* possibly some other special key(s)

Note that the above keys may have different names on terminals that emulate an IBM 3270 type terminal (block mode terminals).

Some older Classic REXX interpreters have a keyboard read subroutine (BIF) so that the program can read keyboard keys as 

they are pressed   (see the other versions below).

```rexx
/*REXX program tests for a    Y  or  N    key when entered from keyboard after a prompt.*/

  do queued();   pull;   end                     /*flush the stack if anything is queued*/

prompt = 'Please enter   Y  or  N   for verification:'   /*this is the  PROMPT  message.*/

  do  until  pos(ans,'NY')\==0 & length(ans)==1  /*keep looking for a  Y  or  N  answer.*/
  say;       say prompt                          /*display blank line;  display prompt. */
  pull ans                                       /*get the answer(s)  and  uppercase it.*/
  ans=space(ans, 0)                              /*elide all blanks.                    */
  end   /*until*/
                                                 /*stick a fork in it,  we're all done. */
```



### version 1 for PC/REXX and Personal REXX

This version of a REXX program works with PC/REXX and Personal REXX.

```rexx
/*REXX program tests for a    Y  or  N    key when entered from keyboard after a prompt.*/
prompt = 'Please enter   Y  or  N   for verification:'   /*this is the  PROMPT  message.*/

  do  until  pos(ans, 'NYny') \== 0              /*keep prompting until answer= Y N y n */
  say;       say prompt                          /*display blank line;  display prompt. */
  ans=inKey('wait')                              /*get the answer(s) from the terminal. */
  end   /*until*/
                                                 /*stick a fork in it,  we're all done. */
```



### version 2 for PC/REXX and Personal REXX

This version is the same as above, but has a more idiomatic technique for testing the response.

```rexx
/*REXX program tests for a    Y  or  N    key when entered from keyboard after a prompt.*/
prompt = 'Please enter   Y  or  N   for verification:'   /*this is the  PROMPT  message.*/

  do  until  pos(ans, 'NY')\==0                   /*keep prompting 'til user answers Y│N */
  say;       say prompt                         /*display blank line;  display prompt. */
  ans=inKey('wait');  upper ans                  /*get the answer(s);  and uppercase it.*/
  end   /*until*/
                                                 /*stick a fork in it,  we're all done. */
```



## Ring


```ring

while true
      give c
      if c = "Y" see "You said yes!" + nl
      but c = "N" see "You said no!" + nl
      else see "Try again!" + nl ok
end

```



## Ruby


```Ruby

def yesno
  begin
    system("stty raw -echo")
    str = STDIN.getc
  ensure
    system("stty -raw echo")
  end
  if str == "Y"
    return true
  elsif str == "N"
    return false
  else
    raise "Invalid character."
  end
end

```


Ruby provides the io/console module since version 2.0:

```Ruby

require 'io/console'

def yesno
  case $stdin.getch
    when "Y" then true
    when "N" then false
    else raise "Invalid character."
  end
end

```



## Run BASIC


```runbasic
[loop] cls                ' Clear screen
html "Click Y or N"                      ' no other options
      button #y, "Y", [Y]                '   they either click [Y]
      button #n, "N", [N]                '   or they click [N]
html "
";msg$                         ' print message showing what they entered
wait
[Y] msg$ = "You entered [Y]es": goto [loop]
[N] msg$ = "You entered [N]o" : goto [loop]

```


## Rust

{{libheader|Ncurses}}

```rust
//cargo-deps: ncurses

extern crate ncurses;
use ncurses::*;

fn main() {
    initscr();
    loop {
        printw("Yes or no? ");
        refresh();

        match getch() as u8 as char {
            'Y'|'y' => {printw("You said yes!");},
            'N'|'n' => {printw("You said no!");},
            _ => {printw("Try again!\n"); continue;},
        }
        break
    }
    refresh();
    endwin();
}
```



## Scala


```scala
  println(if (scala.io.StdIn.readBoolean) "Yes typed." else "Something else.")
```


----

```scala

import java.io.InputStreamReader
val in = new InputStreamReader(System.in)
if (Seq(121, 89, 110, 78).contains(in.read()) ) {println("Yes|No")} else {println("other")}

```

----

```scala

import scala.io.{Source, BufferedSource}
val kbd_In: BufferedSource = Source.stdin
//kbd_In.next()
//res?: Char = 'y' not :String = "y"
if (Seq('y', 'Y', 'n', 'Y').contains(kbd_In.next()) ) {println("Typed y|Y|n|N")} else {println("other key")}

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "keybd.s7i";

const func boolean: yesOrNo (in string: prompt) is func
  result
    var boolean: yes is FALSE;
  local
    var char: answer is ' ';
  begin
    while keypressed(KEYBOARD) do
      ignore(getc(KEYBOARD));
    end while;
    write(prompt);
    repeat
      answer := lower(getc(KEYBOARD));
    until answer in {'y', 'n'};
    yes := answer = 'y';
  end func;

const proc: main is func
  begin
    writeln(yesOrNo("Press Y or N to continue "));
  end func;
```



## Sidef

{{trans|Perl}}

```ruby
func prompt_yn {
    static rk = frequire('Term::ReadKey');
    rk.ReadMode(4);     # change to raw input mode

    var key = '';
    while (key !~ /[yn]/i) {
        while (rk.ReadKey(-1) != nil) {};   # discard any previous input
        print "Type Y/N: ";
        say (key = rk.ReadKey(0));          # read a single character
    }

    rk.ReadMode(0);     # reset the terminal to normal mode
    return key.uc;
}

var key = prompt_yn();
say "You typed: #{key}";
```

{{out}}

```txt

Type Y/N: a
Type Y/N: b
Type Y/N: c
Type Y/N: y
You typed: Y

```



## Tcl


Using the console (expects U*Xish <tt>stty</tt>)


```tcl
proc yesno {{message "Press Y or N to continue"}} {
    fconfigure stdin -blocking 0
    exec stty raw
    read stdin ; # flush
    puts -nonewline "${message}: "
    flush stdout
    while {![eof stdin]} {
        set c [string tolower [read stdin 1]]
        if {$c eq "y" || $c eq "n"} break
    }
    puts [string toupper $c]
    exec stty -raw
    fconfigure stdin -blocking 1
    return [expr {$c eq "y"}]
}

set yn [yesno "Do you like programming (Y/N)"]
```


Without a console (answer in the global variable yn; this should work in any GUI for which there is a TCL):


```tcl

proc yesno {message} {
  toplevel .msg 
  pack [label .msg.l -text "$message\n (type Y/N)?"]
  set ::yn ""
  bind .msg <Key-y> {set ::yn "Y"}
  bind .msg <Key-n> {set ::yn "N"}
  vwait ::yn
  destroy .msg
}

yesno "Do you like programming?"


```



## TXR


This works not only on Unix-like platforms, but also on Microsoft Windows, because TXR is ported to Windows using a [https://www.kylheku.com/cygnal/index.html modified version of Cygwin].


```txrlisp
(with-resources ((tio-orig (tcgetattr) (tcsetattr tio-orig)))
  (let ((tio (copy tio-orig)))
    tio.(go-raw)
    (tcsetattr tio tcsaflush) ;; third arg optional, defaults to tcsadrain
    (whilet ((k (get-char))
             ((not (member k '(#\y #\n #\Y #\N))))))))
```


The <code>go-raw</code> method on the <code>termios</code> structure only manipulates the structure contents; <code>tcsetattr</code> pushes it down to the TTY driver.

<code>go-raw</code> is defined in the TXR standard library like this:


```txrlisp
(defmeth termios go-raw (tio)
  tio.(clear-iflags ignbrk brkint parmrk istrip inlcr igncr icrnl ixon)
  tio.(clear-oflags opost)
  tio.(clear-cflags csize parenb)
  tio.(clear-lflags echo echonl icanon isig)
  (if (boundp 'iexten)
    tio.(clear-lflags iexten))
  tio.(set-cflags cs8)
  (set tio.[cc vmin] 1)
  (set tio.[cc vtime] 0))
```



## UNIX Shell

{{works with|Bourne Again SHell}}

```bash
getkey() {
  local stty="$(stty -g)"
  trap "stty $stty; trap SIGINT; return 128" SIGINT
  stty cbreak -echo 
  local key
  while true; do
    key=$(dd count=1 2>/dev/null) || return $?
    if [ -z "$1" ] || [[ "$key" == [$1] ]]; then
      break
    fi
  done
  stty $stty
  echo "$key"
  return 0
}

yorn() {
  echo -n "${1:-Press Y or N to continue: }" >&2
  local yorn="$(getkey YyNn)" || return $?
  case "$yorn" in 
    [Yy]) echo >&2 Y; return 0;;
    [Nn]) echo >&2 N; return 1;;
  esac
}
```


Cleaner version using bash built-ins


```sh
#!/bin/bash

yorn() {
  echo -n "${1:-Press Y or N to continue: }"

  shopt -s nocasematch

  until [[ "$ans" == [yn] ]]
  do
    read -s -n1 ans
  done

  echo "$ans"

  shopt -u nocasematch
}

yorn
```


=={{header|VB-DOS}}==

```vb
OPTION EXPLICIT
DIM T AS INTEGER
T = MSGBOX("Click on yes or no", 4, "Option")
PRINT "The response is ";
IF T = 6 THEN PRINT "yes"; ELSE PRINT "no";
PRINT "."
END
```



## Vedit macro language


```vedit
Key_Purge()                                     // flush keyboard buffer
do {
    #1 = Get_Key("Are you sure? (Y/N): ")       // prompt for a key
    #1 &= 0xdf                                  // to upper case
} while (#1 != 'Y' && #1 != 'N') 
```



## Wee Basic


```Wee Basic
print 1 "Enter Y for yes, or N for no. (not case sensitive)"
let loop=0
let keycode=0
while loop=0
let keycode=key()
if keycode=121
let response$="y"
let loop=1
elseif keycode=89
let response$="Y"
let loop=1
elseif keycode=110
let response$="n"
let loop=1
elseif keycode=78
let response$="N"
let loop=1
endif
wend
print 1 "You entered"+response$
end
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
loop    [OpenI(1);              \flush any pending keystroke
        case ChIn(1) of         \get keystroke
         ^Y,^y: Text(0, "yes");
         ^N,^n: Text(0, "no");
         $1B:   quit            \Esc key terminates program
        other ChOut(0, 7\bel\);
        CrLf(0);
        ]
```



{{omit from|GUISS}}

[[Category:Keyboard Input]]

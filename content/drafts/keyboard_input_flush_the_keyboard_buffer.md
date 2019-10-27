+++
title = "Keyboard input/Flush the keyboard buffer"
description = ""
date = 2019-03-02T13:30:29Z
aliases = []
[extra]
id = 8432
[taxonomies]
categories = []
tags = []
+++

{{task|Keyboard input}} 
[[Category:Hardware]] 
[[Category:Terminal control]] 
[[Category:Simple]] 
[[user input::task| ]] 
{{omit from|ACL2}}
{{omit from|GUISS}}
{{omit from|PARI/GP}}

Flush the   [[input device::keyboard]]   buffer.
 
This reads characters from the keyboard input and 
discards them until there are no more currently buffered,    
and then allows the program to continue. 

''The program must not wait for users to type anything.''





## Ada



```Ada
with Ada.Text_IO;
procedure Flushtest is
   use Text_IO;
begin
   Put_Line ("Type anything for 2 s");
   delay 2.0;
Flush_Input:
   declare
      Ch   : Character;
      More : Boolean;
   begin
      loop
         Get_Immediate (Ch, More);
         exit when not More;
      end loop;
   end Flush_Input;
   New_Line;
   Put_Line ("Okay, thanks. Here is some input from you:");
   Put_Line (Get_Line);
end Flushtest;
```



## Axe


```axe
While getKey(0)
End
```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
10  IF  PEEK (49152) > 127 THEN C =  PEEK (49168): GOTO 10
```


=
## Locomotive Basic
=


```locobasic>10 CLEAR INPUT</lang


(Only available in BASIC 1.1 though, i.e. not on the CPC 464.)

=
## ZX Spectrum Basic
=

There is no need to flush keyboard buffer in Spectrum since key presses are not buffered.
If a key is currently pressed, the following waits until key is released.

```basic
10 IF INKEY$ <> "" THEN GO TO 10
```



## BBC BASIC


```bbcbasic
      *FX 15,1
```

Strictly speaking *FX 15,1 is an Operating System command, but it is emulated in BBC BASIC for Windows.  Alternatively the keyboard buffer may be flushed as follows:

```bbcbasic
      REPEAT UNTIL INKEY(0)=-1
```



## C

{{libheader|POSIX}}
Code lifted from [[Keyboard input/Obtain a Y or N response]]:

```c>#include <stdio.h

#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>

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

int get_key()
{
	int c = 0;
	fd_set fs;

	FD_ZERO(&fs);
	FD_SET(STDIN_FILENO, &fs);
	select(STDIN_FILENO + 1, &fs, 0, 0, 0);

	if (FD_ISSET(STDIN_FILENO, &fs)) {
		c = getchar();
		set_mode(0);
	}
	return c;
}

int main()
{
	int c = 0;
	while (c != 'n') {
		set_mode(1);

		/* flush pending input so we won't format the hardrive
		   because user accidentally typed 'y' before we even prompted */
		tcflush(STDIN_FILENO, TCIFLUSH);

		printf("Show this prompt again [Yes/No/Ignore you]? ");
		fflush(stdout);

		switch(c = tolower(get_key())) {
		case 'y':	putchar('\n');
				break;

		case 'n':	printf("\nDone\n");
				break;

		case 'i':	puts("\nI'll ignore keys for 5 seconds");
				sleep(5);
				putchar('\n');
				break;
		default:
				puts("\nAssume that was the cat.");
		}
	}

	return 0;
}
```



## D


```d
extern (C) {
    void _STI_conio();
    void _STD_conio();
    int kbhit();
    int getch();
}

void main() {
    void flushKB() {
        while (kbhit()) getch();
    }

    _STI_conio();

    flushKB();

    _STD_conio();
}
```


## DCL

<lang>$ wait 0::10  ! gives us 10 seconds to get keystrokes into the type-ahead buffer
$ on control_y then $ goto clean
$ set terminal /noecho
$ loop: read /prompt="" /time=0 sys$command /error=clean buffer
$ goto loop
$ clean:
$ set terminal /echo
```

{{out}}

```txt
$ @flush_the_keyboard_buffer  ! ignores/discards keystrokes for 10 seconds
$
```



## ERRE


```ERRE

!$KEY
..........
REPEAT
   GET(K$)
UNTIL K$=""
..........

```

Note: Test after K$ can be replaced with <code>LEN(K$)=0</code>.


## Euphoria


```Euphoria
while get_key()!=-1 do
end while
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
    _, err := gc.Init()
    if err != nil {
        log.Fatal("init:", err)
    }
    defer gc.End()
    gc.FlushInput()
}
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Get characters from the keyboard buffer until there are none left
While Inkey <> "" : Wend 
Print "Keyboard buffer flushed"
Sleep
```



## Haskell

This relies upon POSIX terminal support.

```haskell
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import System.IO (hFlush, stdout)
import System.Posix

-- If the file descriptor is associated with a terminal, then flush its input,
-- otherwise do nothing.
termFlush :: Fd -> IO ()
termFlush fd = do
  isTerm <- queryTerminal fd
  when isTerm $ discardData fd InputQueue

main :: IO ()
main = do
  putStrLn "Type some stuff...\n"
  threadDelay $ 3 * 1000000
  putStrLn "\n\nOk, stop typing!\n"
  threadDelay $ 2 * 1000000

  termFlush stdInput

  putStr "\n\nType a line of text, ending with a newline: "
  hFlush stdout
  line <- getLine
  putStrLn $ "You typed: " ++ line
  termFlush stdInput
```



## i

There is no need to manually flush the keyboard buffer in 'i', the only way to receive keyboard input is in graphics mode and keyboard input is automatically flushed every update.

=={{header|Icon}} and {{header|Unicon}}==
The following solution works in both Icon and Unicon.

```unicon
procedure flushKB()
    while kbhit() do getch()   # flush input
end
```



## Julia

{{libheader|Gtk}}

```julia

using Gtk

function flush_keyboard()
    win = GtkWindow("", 1, 1)
    keyget(w, event) = Int32(0)
    signal_connect(keyget, win, "key-press-event")
    visible(win, false)
    sleep(0.25)
end

```




## Kotlin

There appears to be no completely satisfactory, platform independent, way in Java (and hence in the JVM-targetted version of Kotlin) to flush the keyboard buffer. The method presented here may not always work (as the InputStream.available method only gives an estimate of the bytes in the buffer) but is better than nothing and does not block as other approaches to the problem may do.

```scala
// version 1.0.6

fun main(args: Array<String>) {
    while (System.`in`.available() > 0) System.`in`.read()
    println("Goodbye!")
}
```



## M2000 Interpreter

M2000 run in M2000 Environment so has total control, including keyboard. Here we read keyboard from console (we can't read with this way for froms, we have to read from events).

Inkey$ return "" if not key pressed, or the key. Return unicode char

Key$ wait for key, to press, we can use Keyboard to send keys

inkey(1000) wait for a new key to press, we can't use Keyboard to send keys, there is no auto repeat, so if we have a second read inkey(1000) and we keep press the same key, nothing happen (return -1, as no new key)

Keypress(32) we can read if a space key is now pressed. Can't read keyboard if m2000 environment application hasn't focus. It's not a key logger. Keypress(1) read left mouse button, Keypress(2) read right mouse button.



```M2000 Interpreter

Module Checkit {
      \\ feed keyboard
      Keyboard "abcd"
      \\ Work in Windows not in Linux (no Osk.exe exist)
      \\ no error return in linux
      Keyboard !  'open virtual keyboard
      Wait 3000
      \\ flush keyboard
      \\ we can use Do or Repeat (is the same)
      Repeat {
            a$=inkey$
            if a$="" then Print :exit
            Print a$;
      } Always
}
Checkit

```


This isn't the task. Input ends when statement Input End occur, in a thread.

Statement After make a thread for one time only. When in Input interpreter wait for A$ to input, threads allowed to run. If we write but forget to press enter then input flush. If no input statement run then nothing happen when Input End run.



```M2000 Interpreter

Module checkit {
      Print "You have 3 seconds to write your name (press enter)"
      After 3000 { 
            Input End
      }
      Input "Your name:", A$ 
      If A$="" Then Print "Not Ready" : Exit
      Print "Ok:";A$
}
Checkit

```




## Nim

{{libheader|POSIX}}

```nim
const TCIFLUSH: cint = 0
proc tcflush(fd, queue_selector: cint): cint {.header: "termios.h".}

discard tcflush(cint(getFileHandle(stdin)), TCIFLUSH)
```



## Oforth



```Oforth
import: console

System.Console flush
```



## Perl



```perl
use Term::ReadKey;
ReadMode 'restore';    # Flush the keyboard and returns input stream to initial state
# ReadMode 0;            # Numerical equivalent of keyboard restore (move comment marker to use instead)

# A more complete example for use in keyboard handler programming.
# We should also check we are being used in an interactive context (not done here).

use Term::ReadKey;
ReadMode 'cbreak';

# Flush the keyboard in terminal character break mode
while (defined ReadKey -1) {
  # Do nothing
}

# Don't forget to restore the readmode, when we are finished using the keyboard
ReadMode 'restore';
```



## Perl 6

{{works with|Rakudo|2018.12}}
Using termios to set some input attributes, flush the buffer & do unbuffered reads. Longer than strictly necessary to demonstrate concepts and make it easy to verify that it actually works as advertised.   

```perl6
use Term::termios;

constant $saved   = Term::termios.new( :fd($*IN.native-descriptor) ).getattr;
constant $termios = Term::termios.new( :fd($*IN.native-descriptor) ).getattr;

# set some modified input flags
$termios.unset_iflags(<BRKINT ICRNL ISTRIP IXON>);
$termios.unset_lflags(< ECHO ICANON IEXTEN>);
$termios.setattr(:DRAIN);

# reset terminal to original settings on exit
END { $saved.setattr(:NOW) }


# Sleep for a few seconds to give you time to fill the input buffer,
# type a bunch of random characters.
sleep 2;

# ------- The actual task --------
# Flush the input buffer

$termios.setattr(:FLUSH);

# --------------------------------

# Ctrl-C to exit
loop {
    # Read up to 5 bytes from STDIN
    # F5 through F12 are 5 bytes each
    my $keypress = $*IN.read: 5;
    # print the ordinals of the keypress character
    print $keypress.decode.ords;
    print "|";
}
```



## Phix


```Phix
while get_key()!=-1 do end while
```



## PicoLisp


```PicoLisp
(while (key 10))
```



## PowerShell

The following uses the special <code>$Host</code> variable which points to an instance of the PowerShell host application. Since the host's capabilities may vary this may not work in all PowerShell hosts. In particular, this works in the console host, but not in the PowerShell ISE.

```powershell
while ($Host.UI.RawUI.KeyAvailable) {
    $Host.UI.RawUI.ReadKey() | Out-Null
}
```


To flush the keyboard buffer use: 

```powershell

$Host.UI.RawUI.FlushInputBuffer()

```



## PureBasic


```PureBasic
While Inkey(): Wend
```



## Python


```Python
def flush_input():
    try:
        import msvcrt
        while msvcrt.kbhit():
            msvcrt.getch()
    except ImportError:
        import sys, termios
        termios.tcflush(sys.stdin, termios.TCIOFLUSH)

```



## Racket


Using <tt>stty</tt> to get the terminal into raw mode.


```racket

#lang racket
(define-syntax-rule (with-raw body ...)
  (let ([saved #f])
    (define (stty x) (system (~a "stty " x)) (void))
    (dynamic-wind (λ() (set! saved (with-output-to-string (λ() (stty "-g"))))
                       (stty "raw -echo opost"))
                  (λ() body ...)
                  (λ() (stty saved)))))

(with-raw
  (printf "Keys pressed from now will be ignored\n")
  (sleep 2)
  (let loop () (when (char-ready?) (read-char) (loop))) ; flush input
  (printf "Now press a key which will not be ignored\n")
  (printf "You pressed ~a\n" (read-char)))

```



## REXX

This will work for Regina:

```rexx>call dropbuf</lang

This will work for CMS REXX, PC/REXX, Personal REXX, and TSO REXX:

```rexx
'DROPBUF'
```



## Ring


```ring

# Project: Keyboard input/Flush the keyboard buffer

Fflush(stdin)

```



## Ruby

Each terminal device has an ''input queue'' for keyboard input. We can either flush this input queue, or read it empty.

Ruby 1.9.3 adds a new library 'io/console', providing IO#iflush to flush and discard the input queue. If its IO object is not a terminal, it raises an error, perhaps Errno::ENODEV.

{{works with|Ruby|1.9.3}}

```ruby
require 'io/console'
$stdin.iflush
```


The other option uses IO#read_nonblock to read the input, without any blocking or waiting. This has a caveat: if the terminal uses the ''canonical input mode'', IO reads only entire lines; and if the input queue contains part of a line, IO#read_nonblock cannot discard this last partial line!


```ruby
loop { $stdin.read_nonblock(256) } rescue nil
```


The complete solution calls IO#iflush, or turns off canonical input mode and calls IO#read_nonblock.


```ruby
class IO
  def discard_input
    icanon = false
    if tty?
      begin
        # With Ruby 1.9.3, simply call IO#iflush.
        require 'io/console'
        return iflush
      rescue LoadError
        # Try to run stty(1) to check if this terminal uses
        # canonical input mode. Acts like `stty -a`, but redirects
        # stdin from tty. Works with Ruby 1.8, no Process#spawn.
        r, w, pid = nil
        begin
          r, w = IO.pipe
          pid = fork do
            IO.for_fd(0).reopen(self)  # stdin from tty
            IO.for_fd(1).reopen(w)     # stdout to pipe
            exec 'stty', '-a'
          end
          w.close; w = nil
          icanon = (not r.read.include? "-icanon")
        rescue
          # stty(1) only works with Unix clones.
        ensure
          pid and Process.wait pid
          w and w.close
          r and r.close
        end
      end
    end

    if icanon
      # Turn off canonical input mode.
      pid = nil
      begin
        pid = fork do
          IO.for_fd(0).reopen(self)  # stdin from tty
          exec 'stty', '-icanon'
        end
      ensure
        pid and Process.wait pid
      end
    end

    # Discard input.
    loop { $stdin.read_nonblock(256) } rescue nil

    if icanon
      # Turn on canonical input mode.
      pid = nil
      begin
        pid = fork do
          IO.for_fd(0).reopen(self)  # stdin from tty
          exec 'stty', 'icanon'
        end
      ensure
        pid and Process.wait pid
      end
    end

    nil
  end
end
```



```ruby
# Demonstration: discard input, then input a line from user.
puts 'Type anything for 2 seconds.'
sleep 2
$stdin.discard_input
print 'Enter a line? '
if line = $stdin.gets
then print 'Got line. ', line
else puts 'No line!'
end
```



## Scala


```scala
def flush() { out.flush() }
```


## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/keybd.htm keybd.s7i]
defines the file [http://seed7.sourceforge.net/manual/file.htm#Keyboard_file KEYBOARD],
which provides functions like [http://seed7.sourceforge.net/libraries/keybd.htm#keypressed%28in_keyboard_file%29 keypressed]
and [http://seed7.sourceforge.net/libraries/keybd.htm#getc%28in_console_keybd_file%29 getc].


```seed7
while keypressed(KEYBOARD) do
  ignore(getc(KEYBOARD));
end while;
```



## Sidef

{{trans|Perl}}

```ruby
var k = frequire('Term::ReadKey');

k.ReadMode('restore');    # Flush the keyboard and returns input stream to initial state
# ReadMode 0;             # Numerical equivalent of keyboard restore (move comment marker to use instead)

# A more complete example for use in keyboard handler programming.
# We should also check we are being used in an interactive context (not done here).

k.ReadMode('cbreak');

# Flush the keyboard in terminal character break mode
while (k.ReadKey(-1) != nil) {
   # Do nothing
}

# Don't forget to restore the readmode, when we are finished using the keyboard
k.ReadMode('restore');
```



## Tcl


```tcl
# No waiting for input
fconfigure stdin -blocking 0
# Drain the data by not saving it anywhere
read stdin

# Flip back into blocking mode (if necessary)
fconfigure stdin -blocking 1
```



## Vedit macro language


```vedit
Key_Purge()
```



## XPL0


```XPL0
code OpenI=13;
OpenI(0)
```


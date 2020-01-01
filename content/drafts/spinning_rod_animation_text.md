+++
title = "Spinning rod animation/Text"
description = ""
date = 2019-09-02T14:37:45Z
aliases = []
[extra]
id = 21875
[taxonomies]
categories = []
tags = []
+++

{{task|Spinning rod animation/Text}}

;Task:
An animation with the following frames in the following order must animate with a delay of 0.25 seconds between each frame, with the previous frame being cleared before the next frame appears: <big>
:*   |
:*   /
:*   -
:*   \

</big>

A version that loops and/or a version that doesn't loop can be made.





## AWK


```AWK

# syntax: GAWK -f SPINNING_ROD_ANIMATION_TEXT.AWK
@load "time"
BEGIN {
    while (1) {
      printf(" %s\r",substr("|/-\\",x++%4+1,1))
      sleep(.25)
    }
    exit(0)
}

```


## Bash


```bash
while : ; do
  for rod in \| / - \\ ; do printf '  %s\r' $rod; sleep 0.25; done
done
```

(Added an indent in the printf to better see the spinning rod).


## C

{{trans|Go}}

```c
#include <stdio.h>
#include <time.h>

int main() {
    int i, j, ms = 250;
    const char *a = "|/-\\";
    time_t start, now;
    struct timespec delay;
    delay.tv_sec = 0;
    delay.tv_nsec = ms * 1000000L;
    printf("\033[?25l");  // hide the cursor
    time(&start);
    while(1) {
        for (i = 0; i < 4; i++) {
            printf("\033[2J");          // clear terminal
            printf("\033[0;0H");        // place cursor at top left corner
            for (j = 0; j < 80; j++) {  // 80 character terminal width, say
                printf("%c", a[i]);
            }
            fflush(stdout);
            nanosleep(&delay, NULL);
        }
        // stop after 20 seconds, say
        time(&now);
        if (difftime(now, start) >= 20) break;
    }
    printf("\033[?25h"); // restore the cursor
    return 0;
}
```



## C Shell


```csh
while 1
  foreach rod ('|' '/' '-' '\')
    printf '  %s\r' $rod; sleep 0.25
  end
end
```

(Added an indent in the printf to better see the spinning rod).

=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>SPINROD
  ; spin 10 times with quarter-second wait
  for i = 1:1:10 {
    for j = 1:1:4 {
      set x = $case(j,1:"|",2:"/",3:"-",:"\")

      ; $char(8) backspaces on the terminal
      write $char(8)_x
      hang 0.25
    }
  }
  quit
```



## Emacs Lisp


```Lisp

(while t
  (mapcar
   (lambda(n)(princ n)(sit-for 0.25) (backward-delete-char 1))
   (list "\\" "|" "-" "/") ) )

```



## Factor


```factor
USING: calendar combinators.extras formatting io sequences
threads ;

[
    "\\|/-" [ "%c\r" printf flush 1/4 seconds sleep ] each
] forever
```


## FreeBASIC


```freebasic
' version 13-07-2018
' compile with: fbc -s console

Dim As String spinning_rod = "|/-\"
Dim As UInteger c

While InKey <> "" : Wend

While InKey = ""
    Cls
    Print
    Print " hit any key to end program "; Chr(spinning_rod[c And 3])
    c += 1
    Sleep(250)  ' in milliseconds
Wend

End
```



## Forth

Tested in gforth 0.7.9

```forth

: rod
  cr
  begin
  [char] \\ emit 250 ms
  13 emit [char] | emit 250 ms
  13 emit [char] - emit 250 ms
  13 emit [char] / emit 250 ms
  again
;
rod

```



## GlovePIE

Because GlovePIE is a looping programming language, which means the script is ran over and over again in a looping fashion, this code loops again and again until it's stopped.

```glovepie
debug="|"
wait 250 ms
debug="/"
wait 250 ms
debug="-"
wait 250 ms
debug="\"
wait 250 ms
```



## Go

{{works with|Ubuntu 16.04}}

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    a := `|/-\`
    fmt.Printf("\033[?25l")  // hide the cursor
    start := time.Now()
    for {
        for i := 0; i < 4; i++ {
            fmt.Print("\033[2J")       // clear terminal
            fmt.Printf("\033[0;0H")    // place cursor at top left corner
            for j := 0; j < 80; j++ {  // 80 character terminal width, say
                fmt.Printf("%c", a[i])
            }
            time.Sleep(250 * time.Millisecond)
        }
        if time.Since(start).Seconds() >= 20.0 { // stop after 20 seconds, say
            break
        }
    }
    fmt.Print("\033[?25h") // restore the cursor
}
```



## Haskell

Uses the terminfo library to make the cursor invisible, if possible.

```haskell
import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Monad (forM_)
import System.Console.Terminfo
import System.IO (hFlush, stdout)

-- Use the terminfo database to write the terminal-specific characters
-- for the given capability.
runCapability :: Terminal -> String -> IO ()
runCapability term cap =
  forM_ (getCapability term (tiGetOutput1 cap)) (runTermOutput term)

-- Control the visibility of the cursor.
cursorOff, cursorOn :: Terminal -> IO ()
cursorOff term = runCapability term "civis"
cursorOn  term = runCapability term "cnorm"

-- Print the spinning cursor.
spin :: IO ()
spin = forM_ (cycle "|/-\\") $ \c ->
  putChar c >> putChar '\r' >>
  hFlush stdout >> threadDelay 250000

main :: IO ()
main = do
  putStrLn "Spinning rod demo.  Hit ^C to stop it.\n"
  term <- setupTermFromEnv
  bracket_ (cursorOff term) (cursorOn term) spin
```



## Java

{{trans|Go}}

```java
public class SpinningRod
{
    public static void main(String[] args) throws InterruptedException {
        String a = "|/-\\";
        System.out.print("\033[2J");   // hide the cursor
        long start = System.currentTimeMillis();
        while (true) {
            for (int i = 0; i < 4; i++) {
                System.out.print("\033[2J");     // clear terminal
                System.out.print("\033[0;0H");   // place cursor at top left corner
                for (int j = 0; j < 80; j++) {   // 80 character terminal width, say
                    System.out.print(a.charAt(i));
                }
                Thread.sleep(250);
            }
            long now = System.currentTimeMillis();
            // stop after 20 seconds, say
            if (now - start >= 20000) break;
        }
        System.out.print("\033[?25h"); // restore the cursor
    }
}
```



## Javascript

Node JS:

```javascript

const rod = (function rod() {
    const chars = "|/-\\";
    let i=0;
    return function() {
        i= (i+1) % 4;
        // We need to use process.stdout.write since console.log automatically adds a \n to the end of lines
        process.stdout.write(` ${chars[i]}\r`);
    }
})();
setInterval(rod, 250);

```



## Julia

{{trans|Python}}

```julia
while true
  for rod in "\|/-" # this needs to be a string, a char literal cannot be iterated over
    print(rod,'\r')
    sleep(0.25)
  end
end

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.2.50

const val ESC = "\u001b"

fun main(args: Array<String>) {
    val a = "|/-\\"
    print("$ESC[?25l") // hide the cursor
    val start = System.currentTimeMillis()
    while (true) {
        for (i in 0..3) {
            print("$ESC[2J")       // clear terminal
            print("$ESC[0;0H")     // place cursor at top left corner
            for (j in 0..79) {     // 80 character terminal width, say
                print(a[i])
            }
            Thread.sleep(250)
        }
        val now = System.currentTimeMillis()
        // stop after 20 seconds, say
        if (now - start >= 20000) break
    }
    print("$ESC[?25h") // restore the cursor
}
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      n$=lambda$ n=1, a$="|/-\" -> {
            =mid$(a$, n, 1)
            n++
            if n>4 then n=1
      }
      \\ 1000 is 1 second
      Every 250 {
      \\ Print Over: erase line before print. No new line append.
      Print Over  n$()
      }
}
CheckIt


```



```M2000 Interpreter

Module Checkit {
      n=1
      a$="|/-\"
      Every 250 {
            Print Over mid$(a$, n, 1)
            n++
            if n>4 then n=1
      }
}
CheckIt


```



## Microsoft Small Basic


```microsoftsmallbasic
a[1]="|"
a[2]="/"
a[3]="-"
a[4]="\"
b=0
While b=0
  For c=1 To 4
    TextWindow.Clear()
    TextWindow.WriteLine(a[c])
    Program.Delay(250)
  EndFor
EndWhile
```



## MiniScript

Control over the text cursor -- or indeed, whether there ''is'' a text cursor, or even text at all -- depends on the host environment.  Here's a version that works with [https://miniscript.org/MiniMicro/ MiniMicro]:

```MiniScript
print "Press control-C to exit..."
while true
    for c in "|/-\"
        text.setCell 0, 0, c
        wait 0.25
    end for
end while
```


And here's a version that will work with command-line MiniScript, running on a terminal that interprets standard VT100 escape sequences:

```MiniScript
while true
    for c in "|/-\"
        print c
        wait 0.25
        print char(27) + "[2A"  // move cursor up 2 lines
    end for
end while
```



## Perl

The statement <code>$| =1</code> is required in order to disable output buffering.

```perl
$|= 1;

while () {
    for (qw[ | / - \ ]) {
        select undef, undef, undef, 0.25;
        printf "\r ($_)";
    }
}
```



## Perl 6

{{works with|Rakudo|2018.05}}
Traditionally these are know as [[wp:throbber|throbbers]] or progress indicators.

This implementation will accept an array of elements to use as its throbber frames, or as a scrolling marquee and optionally a delay before it returns the next element.


```perl6
class throbber {
    has @.frames;
    has $.delay is rw = 0;
    has $!index = 0;
    has Bool $.marquee = False;
    method next {
        $!index = ($!index + 1) % +@.frames;
        sleep $.delay if $.delay;
        if $!marquee {
            ("\b" x @.frames) ~ @.frames.rotate($!index).join;
        }
        else {
            "\b" ~ @.frames[$!index];
        }
    }
}

my $rod = throbber.new( :frames(< | / - \ >), :delay(.25) );
print "\e[?25lLong running process...  ";
print $rod.next for ^20;

my $clock = throbber.new( :frames("🕐" .. "🕛") );
print "\b \nSomething else with a delay...  ";
until my $done {
    # do something in a loop;
    sleep 1/12;
    print $clock.next;
    $done = True if $++ >= 60;
}

my $scroll = throbber.new( :frames('PLEASE STAND BY...      '.comb), :delay(.1), :marquee );
print "\b \nEXPERIENCING TECHNICAL DIFFICULTIES: { $scroll.frames.join }";
print $scroll.next for ^95;

END { print "\e[?25h\n" } # clean up on exit
```



## Phix


```Phix
puts(1,"please_wait... ")
cursor(NO_CURSOR)
for i=1 to 10 do    -- (approx 10 seconds)
    for j=1 to 4 do
        printf(1," \b%c\b",`|/-\`[j])
        sleep(0.25)
    end for
end for
puts(1," \ndone") -- clear rod, "done" on next line

```



## PicoLisp


```Lisp

(de rod ()
   (until ()
      (for R '(\\ | - /)
         (prin R (wait 250) "\r")(flush) ) ) )
(rod)

```



## Python


```python
from time import sleep
while True:
    for rod in r'\|/-':
        print(rod, end='\r')
        sleep(0.25)
```



## Racket


```racket

#lang racket
(define (anim)
  (for ([c "\\|/-"])
    (printf "~a\r" c)
    (sleep 0.25))
  (anim))
(anim)

```



## REXX

This REXX program would work for all REXXes if there was a common way to sleep (suspend) execution for fractional seconds.

This REXX version will work for:
::* Personnal REXX
::* PC REXX

```rexx
/*REXX program displays a  "spinning rod"  (AKA:  trobbers  or  progress indicators).   */
if 4=='f4'x  then bs= "16"x                      /*EBCDIC?  Then use this backspace chr.*/
             else bs= "08"x                      /* ASCII?    "   "    "      "      "  */
signal on halt                                   /*jump to   HALT   when user halts pgm.*/
$= '│/─\'                                        /*the throbbing characters for display.*/
                  do j=1                         /*perform  until  halted by the user.  */
                  call charout ,  bs  ||  substr($, 1 + j//length($), 1)
                  call delay .25                 /*delays a quarter of a second.        */
                  end   /*j*/
halt: say bs ' '                                 /*stick a fork in it,  we're all done. */
```



## Ring


```ring
load "stdlib.ring"
rod = ["|", "/", "-", "\"]
for n = 1 to len(rod)
     see rod[n] + nl
     sleep(0.25)
     system("cls")
next
```

Output:
 |
 /
 -
 \


## Ruby


```ruby
def spinning_rod
  begin
    printf("\033[?25l") # Hide cursor
    %w[| / - \\].cycle do |rod|
      print rod
      sleep 0.25
      print "\b"
    end
  ensure
    printf("\033[?25h") # Restore cursor
  end
end

puts "Ctrl-c to stop."
spinning_rod

```



## Rust



```rust
fn main() {
    let characters = ['|', '/', '-', '\\'];
    let mut current = 0;

    println!("{}[2J", 27 as char); // Clear screen.

    loop {
        println!("{}[;H{}", 27 as char, characters[current]); // Move cursor to 1,1 and output the next character.
        current += 1; // Advance current character.
        if current == 4 {current = 0;} // If we reached the end of the array, start from the beginning.
        std::thread::sleep(std::time::Duration::from_millis(250)); // Sleep 250 ms.
    }
}
```



## Scala


```Scala
object SpinningRod extends App {
  val start = System.currentTimeMillis

  def a = "|/-\\"

  print("\033[2J") // hide the cursor

  while (System.currentTimeMillis - start < 20000) {
    for (i <- 0 until 4) {
      print("\033[2J\033[0;0H") // clear terminal, place cursor at top left corner
      for (j <- 0 until 80) print(a(i)) // 80 character terminal width, say
      Thread.sleep(250)
    }
  }
  print("\033[?25h") // restore the cursor

}
```



## Wee Basic

Since the "|" character isn't built into Wee Basic on the Nintendo DS, and it looks the part in Wee Basic on the Nintendo DS, the character "l" is used as a substitute. Also, since no working delay command has been found yet, a for loop is used to work around this problem.

```Wee Basic
let loop=1
sub delay:
for i=1 to 10000
next
cls 1
return
while loop=1
print 1 "l"
gosub delay:
print 1 "/"
gosub delay:
print 1 "-"
gosub delay:
print 1 "\"
gosub delay:
wend
end
```



## zkl

{{trans|C Shell}}

```zkl
foreach n,rod in ((1).MAX, T("|", "/", "-", "\\")){
   print("  %s\r".fmt(rod));

   Atomic.sleep(0.25);
}
```

A loop foreach a,b in (c,d) translates to
foreach a in (c) foreach b in (d). n.MAX is a 64 bit int (9223372036854775807).

A more useful example would be a worker thread showing a "I'm working" display
(in another thread) and turning it off when that work is done.

```zkl
fcn spin{	// this will be a thread that displays spinner
   try{
      foreach n,rod in ((1).MAX, "\\|/-"){
         print("  ",rod,"\r");
	 Atomic.sleep(0.25);
      }
   }catch{}	// don't complain about uncaught exception that stops thread
}
```


```zkl
// main body of code
spinner:=spin.launch();	 // start spinner thread, returns reference to thread
Atomic.sleep(10);	 // do stuff
vm.kick(spinner.value);  // stop thread by throwing exception at it
```



## ZX Spectrum Basic


```ZX Basic
10 LET A$="|/-\"
20 FOR C=1 TO 4
30 PRINT AT 0,0;A$(C)
40 PAUSE 4
50 NEXT C
60 GOTO 20

```


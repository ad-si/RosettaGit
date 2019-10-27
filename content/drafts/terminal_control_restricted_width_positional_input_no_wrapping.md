+++
title = "Terminal control/Restricted width positional input/No wrapping"
description = ""
date = 2018-11-11T22:00:02Z
aliases = []
[extra]
id = 12909
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

Create a routine to obtain data entry using a specific place on the terminal screen. Data entry fields should be restricted to a specific length, and it should not be possible for the cursor to move beyond the input length.

The routine should accept parameters for row number, column number and input length, and should obtain a string
value entered by the user.

For the purpose of this task, obtain input from the user, showing data entry at row 3, column 5, with input width restricted to a maximum of 8 characters.

Note: In this task input wrapping is not allowed. (See [[Terminal control/Restricted width positional input/With wrapping]]) for input wrapping variant.

For a similar task using a graphical user interface, see [[Graphical User Interface/Restricted width positional input/No wrapping]].

[[Category:Terminal control]]


## Go

{{works with|Windows 10}}
{{trans|Kotlin}}


This uses _getch() rather than _getwch() as only ASCII is supported.

```go
package main

/*
#include <windows.h>
#include <conio.h>
*/
import "C"
import (
    "fmt"
    "os"
    "os/exec"
)

var conOut = C.GetStdHandle(C.STD_OUTPUT_HANDLE)

func setCursor(p C.COORD) {
    C.SetConsoleCursorPosition(conOut, p)
}

func cls() {
    cmd := exec.Command("cmd", "/c", "cls")
    cmd.Stdout = os.Stdout
    cmd.Run()
}

func getInput(row, col, width int) string {
    if row < 0 || row > 20 || col < 0 || width < 1 || width > 78 || col > (79 - width) {
        panic("Invalid parameter(s) to getInput. Terminating program")
    }
    coord := C.COORD{C.short(col), C.short(row)}
    setCursor(coord)
    var sb []byte
    full := false
    loop:
    for {
        ch := C._getch()                         // gets next character, no echo
        switch c := byte(ch); c {
        case 3, 13:
            break loop                           // break on Ctrl-C or enter key
        case 8:
            if len(sb) > 0 {                     // mimic backspace
		fmt.Print("\b \b")
                sb = sb[:len(sb) - 1]
            }
        case 0, 224:
            C._getch()                           // consume extra character
        default:
            if c >= 32 && c <= 126 && !full {
                C._putch(ch)                     // echo ascii character, ignore others
                sb = append(sb, c)
            }
        }
        full = len(sb) == width                  // can't exceed width
    }
    return string(sb)
}

func main() {
    cls() // clear the console
    s := getInput(2, 4, 8) // Windows console row/col numbering starts at 0
    coord := C.COORD{0, 22}
    setCursor(coord)
    fmt.Printf("You entered '%s'\n", s)
}
```



## Kotlin

{{works with|Windows 10}}
This assumes an 80 x 25 Windows console and uses the Win32 function _getwch() to get character input without (immediately) echoing it to the console. Only ASCII characters in the range 32 to 126 are then printed if the maximum width would not be exceeded. 

Apart from these, only the backspace key (8), return key (13) and Ctrl-C (3) are handled, all other keys being ignored. Some keys (such as the function and arrow keys) return a two character sequence starting with either 0 or 224 and so, after the first character is trapped, the second character needs to be removed from the buffer.

It would also be possible to allow for printable Unicode characters (>= 160) to be entered by adding an extra clause to the 'when' statement. However, for this to work properly, you will need to be using a suitable code page (65001, say) and a suitable font (Lucida Console, say).

```scala
// Kotlin Native v0.5

import kotlinx.cinterop.*
import platform.windows.*
import platform.posix.*

val ascii = 32..126
val conOut = GetStdHandle(STD_OUTPUT_HANDLE)!!

fun setCursor(p: COORD) = SetConsoleCursorPosition(conOut, p.readValue())

fun getInput(row: Short, col: Short, width: Int): String {
    require(row in 0..20 && col in 0..(79 - width) && width in 1..78) { "Invalid parameter(s)" }
    memScoped {
        val coord = alloc<COORD>().apply { X = col; Y = row }
        setCursor(coord)
    }
    val sb = StringBuilder(width)
    var full = false
    loop@ while (true) {
        val ch = _getwch()                                                   // gets next character, no echo
        when (ch.toInt()) {
                3, 13 -> break@loop                                          // break on Ctrl-C or enter key
                    8 -> if (sb.length > 0) { print("\b \b"); sb.length-- }  // mimic backspace
               0, 224 -> _getwch()                                           // consume extra character
             in ascii -> if (!full) { _putwch(ch); sb.append(ch.toChar()) }  // echo ascii character
                 else -> {}                                                  // igore other characters
        }
        full = sb.length == width                                            // can't exceed width
    }
    return sb.toString()
}

fun main(args: Array<String>) {
    system("cls")  // clear the console
    val s = getInput(2, 4, 8)  // Windows console row/col numbering starts at 0
    memScoped {
        val coord = alloc<COORD>().apply { X = 0 ; Y = 22 }
        setCursor(coord)
    }
    println("You entered '$s'")
}
```



## Perl 6

{{works with|Rakudo|2018.10}}
Should work with any termios compatible terminal.

All printable character keys work, as does backspace and enter. Ctrl-c to exit. All other keys / key-combos are ignored. 


```perl6
use Term::termios;

constant $saved   = Term::termios.new(fd => 1).getattr;
constant $termios = Term::termios.new(fd => 1).getattr;
# raw mode interferes with carriage returns, so
# set flags needed to emulate it manually
$termios.unset_iflags(<BRKINT ICRNL ISTRIP IXON>);
$termios.unset_lflags(<ECHO ICANON IEXTEN ISIG>);
$termios.setattr(:DRAIN);


END {
    $saved.setattr(:NOW); # reset terminal to original settings
    print "\e[?25h \e[H\e[J"; # clear and reset screen
}

my $row     = 3;
my $column  = 5;
my $field   = '';
my $spacer  = ' ' x 8;
my $pointer = 0;

my ($rows,$cols) = qx/stty size/.words; # get screen size

my @screen = "\e[41m{' ' x $cols}\e[0m" xx $rows;

update($spacer);

loop {
    my $key = $*IN.read(4).decode;
    given $key {
        when ' '..'~' {
            if $pointer < 8 {
                $field ~= $_;
                $spacer = ' ' x 8 - $field.chars;
                $pointer +=1;
                update($field~$spacer)
            }
        }
        when "\c[127]" { # backspace
            if $pointer > 0 {
                $field.=substr(0,*-1);
                $spacer = ' ' x 8 - $field.chars;
                $pointer -= 1;
                update($field~$spacer)
            }
        }
        when "\c[13]" {
            update('        ');
            print "\e[10;6H\e[1;33;41mYou entered: $field\e[0m\e[$row;{$column}H";
            $field = '';
            $pointer = 0;
        }
        when "\c[0003]" { exit } # Ctrl-c
        default { }
    }
}

sub update ($str) {
    ($rows,$cols) = qx/stty size/.words;
    @screen = "\e[41m{' ' x $cols}\e[0m" xx $rows;
    print "\e[H\e[J{@screen.join: "\n"}\e[$row;{$column}H$str\e[$row;{$column + $pointer}H";
}
```



## Phix


```Phix
function getInput(integer row, col, width)
    position(row,col)
    string s = ""
    while 1 do
        integer ch = wait_key()
        if ch='\r' then exit end if
        if ch='\b' then
            if length(s)>0 then
                puts(1,"\b \b")
                s = s[1..$-1]
            end if
        elsif ch>=' ' and ch<='~' then
            if length(s)<=width then
                puts(1,ch)
                s &= ch
            end if
        end if
    end while
    return s
end function 

clear_screen() -- clear the console
string s = getInput(3, 5, 8)
position(23,1)
printf(1,"You entered '%s'\n",{s})
```


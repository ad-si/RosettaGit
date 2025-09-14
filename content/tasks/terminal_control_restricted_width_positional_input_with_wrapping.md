+++
title = "Terminal control/Restricted width positional input/With wrapping"
description = ""
date = 2018-11-12T00:13:38Z
aliases = []
[extra]
id = 12910
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "kotlin",
  "perl_6",
  "phix",
]
+++

Create a routine to obtain data entry using a specific place on the terminal screen. Data entry fields should be restricted to a specific length, and the cursor should not move beyond the input length.

The routine should accept parameters for row number, column number and input length, and should obtain a string
value entered by the user.

For the purpose of this task, obtain input from the user, showing data entry at row 3, column 5, with input width restricted to a maximum of 8 characters.

Note: In this task input wrapping is allowed. If the length of input exceeds the maximum width, the left hand side of the field should disappear, allowing additional input to be obtained. However, the cursor must not move beyond the designated input length.

It is permissible to use navigation keys to see input field length.

(See [[Terminal control/Restricted width positional input/No wrapping]] for similar input routine with no wrapping).

For a similar task using a graphical user interface, see [[Graphical User Interface/Restricted width positional input/With wrapping]].

## Go

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
    wlen := 0                                    // length of text in editing window
    full := false
    loop:
    for {
        ch := C._getch()                         // gets next character, no echo
        switch c := byte(ch); c {
        case 3, 13:
            break loop                           // break on Ctrl-C or enter key
        case 8:
            if wlen > 0 {                        // mimic backspace
		fmt.Print("\b \b")
                sb = sb[:len(sb) - 1]
                wlen--
            }
            if len(sb) > wlen {
		coord.X = C.short(col)
                coord.Y = C.short(row)
                setCursor(coord)
                fmt.Print(string(sb)[len(sb) - width:])
                wlen = width
            }
        case 0, 224:
            C._getch()                           // consume extra character
        default:
            if c >= 32 && c <= 126 {             // echo ascii character, ignore others
                sb = append(sb, c)
                if !full {
                    C._putch(ch)
                    wlen++
                } else if len(sb) > wlen {
		    coord.X = C.short(col)
                    coord.Y = C.short(row)
                    setCursor(coord)
                    fmt.Print(string(sb)[len(sb) - width:])
                }
            }
        }
        full = wlen == width                     // wlen can't exceed width
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

This follows a similar approach to the Kotlin entry for the [[Terminal control/Restricted width positional input/No wrapping]] task except, of course, that the code now allows for wrapping.

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
    val coord = nativeHeap.alloc<COORD>().apply { X = col; Y = row }
    setCursor(coord)
    val sb = StringBuilder(width)
    var wlen = 0                                          // length of text in editing window
    var full = false
    loop@ while (true) {
        val ch = _getwch()                                // gets next character, no echo
        when (ch.toInt()) {
            3, 13 -> break@loop                           // break on Ctrl-C or enter key

            8 -> {                                        // mimic backspace
                if (wlen > 0) {
                    print("\b \b")
                    sb.length--
                    wlen--
                }
                if (sb.length > wlen) {
                    coord.apply { X = col; Y = row }
                    setCursor(coord)
                    print(sb.toString().takeLast(width))
                    wlen = width
                }
            }

            0, 224 -> _getwch()                           // consume extra character

            in ascii -> {                                 // echo ascii character
                sb.append(ch.toChar())
                if (!full) {
                    _putwch(ch)
                    wlen++
                }
                else if (sb.length > wlen) {
                    coord.apply { X = col; Y = row }
                    setCursor(coord)
                    print(sb.toString().takeLast(width))
                }
            }

            else -> {}                                    // igore other characters
        }
        full = wlen == width                              // wlen can't exceed width
    }
    nativeHeap.free(coord)
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

Should work with any termios compatible terminal.

All printable character keys (except Tab) work, as does backspace and enter. Ctrl-c to exit. All other keys / key-combos are ignored. 


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
            } elsif $pointer >= 8 {
                $field ~= $_;
                $spacer = '';
                update($field.substr(*-8))
            }
        }
        when "\c[127]" { # backspace
            if $pointer > 0 {
                $field.=substr(0,*-1);
                $spacer = ' ' x (8 - $field.chars max 0);
                $pointer -= 1 if $field.chars < 8;
                my $display = $field.chars < 8 ?? $field !! $field.substr(*-8);
                update($display~$spacer)
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
            if length(s)>=width then
                position(row,col)
                puts(1,s[-width..-1])
            end if
        elsif ch>=' ' and ch<='~' then
            s &= ch
            if length(s)<=width then
                puts(1,ch)
            else
                position(row,col)
                puts(1,s[-width..-1])
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


+++
title = "Terminal control/Unicode output"
description = ""
date = 2019-05-14T19:52:40Z
aliases = []
[extra]
id = 10505
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "awk",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "elixir",
  "funl",
  "go",
  "haskell",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "lasso",
  "m2000_interpreter",
  "mathematica",
  "mercury",
  "nemerle",
  "nim",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "unix_shell",
  "zkl",
  "zx_spectrum_basic",
]
+++

The task is to check that the terminal supports Unicode output, before outputting a Unicode character. If the terminal supports Unicode, then the terminal should output a Unicode delta (U+25b3). If the terminal does not support Unicode, then an appropriate error should be raised.

Note that it is permissible to use system configuration data to determine terminal capabilities if the system provides such a facility.


## AutoHotkey



```autohotkey
DllCall("AllocConsole")
hConsole:=DllCall("GetConsoleWindow","UPtr")
Stdout:=FileOpen(DllCall("GetStdHandle", "int", -11, "ptr"), "h `n")
Stdin:=FileOpen(DllCall("GetStdHandle", "int", -10, "ptr"), "h `n")

;Full Unicode-support font needed
e:=SetConsoleOutputCP(65001)
if (e && A_IsUnicode)
{
	Print("△ - Unicode delta (U+25b3)")
	GetPos(x,y)
	if (x=0 && y=0) ;nothing prints if Non-Unicode font
		Print("Non-Unicode font")
}
else
	Print("Unicode not supported")
Pause()

Print(string=""){
	global Stdout
	if (!StrLen(string))
		return 1
	e:=DllCall("WriteConsole" . ((A_IsUnicode) ? "W" : "A")
			, "UPtr", Stdout.__Handle
			, "Str", string
			, "UInt", strlen(string)
			, "UInt*", Written
			, "uint", 0)
	if (!e) or (ErrorLevel)
		return 0 ;Failure
	Stdout.Read(0)
	return e
}

SetConsoleOutputCP(codepage) {
	e:=DllCall("SetConsoleOutputCP","UInt",codepage)
	if (!e) or (ErrorLevel)
		return 0 ;Failure
	return 1
}

GetPos(ByRef x, ByRef y) {
	global Stdout
	VarSetCapacity(struct,22,0)
	e:=DllCall("GetConsoleScreenBufferInfo","UPtr",Stdout.__Handle,"Ptr",&struct)
	if (!e) or (ErrorLevel)
			return 0 ;Failure
	x:=NumGet(&struct,4,"UShort")
	y:=NumGet(&struct,6,"UShort")
	return 1
}

Pause() {
	RunWait, %comspec% /c pause>NUL
}
```



## AWK



```awk
#!/usr/bin/awk -f
BEGIN {
  unicodeterm=1   # Assume Unicode support
  if (ENVIRON["LC_ALL"] !~ "UTF") {
    if (ENVIRON["LC_ALL"] != ""
      unicodeterm=0    # LC_ALL is the boss, and it says nay
    else {
      # Check other locale settings if LC_ALL override not set
      if (ENVIRON["LC_CTYPE"] !~ "UTF") {
        if (ENVIRON["LANG"] !~ "UTF")
          unicodeterm=0    # This terminal does not support Unicode
      }        
    }    
  }

  if (unicodeterm) {
      # This terminal supports Unicode
      # We need a Unicode compatible printf, so we source this externally
      # printf might not know \u or \x, so use octal.
      # U+25B3 => UTF-8 342 226 263
      "/usr/bin/printf \\342\\226\\263\\n"
  } else {
      print "HW65001 This program requires a Unicode compatible terminal"|"cat 1>&2"
    exit 252    # Incompatible hardware
  }
```



## BBC BASIC

```bbcbasic
      VDU 23,22,640;512;8,16,16,128+8 : REM Enable UTF-8 mode
      *FONT Arial Unicode MS,36
      PRINT CHR$(&E2)+CHR$(&96)+CHR$(&B3)
```



## C


```c

#include<stdlib.h>
#include<stdio.h>

int
main ()
{
  int i;
  char *str = getenv ("LANG");

  for (i = 0; str[i + 2] != 00; i++)
    {
      if ((str[i] == 'u' && str[i + 1] == 't' && str[i + 2] == 'f')
          || (str[i] == 'U' && str[i + 1] == 'T' && str[i + 2] == 'F'))
        {
          printf
            ("Unicode is supported on this terminal and U+25B3 is : \u25b3");
          i = -1;
          break;
        }
    }

  if (i != -1)
    printf ("Unicode is not supported on this terminal.");

  return 0;
}

```

Output:

```txt

Unicode is supported on this terminal and U+25B3 is : â³

```



## Clojure


```clojure

(if-not (empty? (filter #(and (not (nil? %)) (.contains (.toUpperCase %) "UTF"))
  (map #(System/getenv %) ["LANG" "LC_ALL" "LC_CTYPE"]))) 
    "Unicode is supported on this terminal and U+25B3 is : \u25b3"
    "Unicode is not supported on this terminal.")

```


```txt
"Unicode is supported on this terminal and U+25B3 is : △"
```



## Common Lisp

Each implementation has a different "getenv" function, to work with various implementations was created the "my-getenv" function.

```lisp

(defun my-getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
    #+Allegro (sys:getenv name)
    #+CLISP (ext:getenv name)
    #+ECL (si:getenv name)
    #+SBCL (sb-unix::posix-getenv name)
    #+ABCL (getenv name)
    #+LISPWORKS (lispworks:environment-variable name)
    default))
 
(if (not ( null (remove-if #'null (mapcar #'my-getenv '("LANG" "LC_ALL" "LC_CTYPE")))))
  (format t "Unicode is supported on this terminal and U+25B3 is : ~a~&" (code-char #x25b3))
  (format t "Unicode is not supported on this terminal.~&")
  )

```

```txt
Unicode is supported on this terminal and U+25B3 is : △
```



## Elixir


```elixir

if ["LANG", "LC_CTYPE", "LC_ALL"]
     |> Enum.map(&System.get_env/1)
     |> Enum.any?(&(&1 != nil and String.contains?(&1, "UTF")))
do
  IO.puts "This terminal supports Unicode: \x{25b3}"
else
  raise "This terminal does not support Unicode."
end

```



## FunL


```funl
if map( v -> System.getenv(v), ["LC_ALL", "LC_CTYPE", "LANG"]).filter( (!= null) ).exists( ('UTF' in) )
  println( '\u25b3' )
else
  println( 'Unicode not supported' )
```



## Go

```go
package main

import (
    "fmt"
    "os"
    "strings"
)

func main() {
    lang := strings.ToUpper(os.Getenv("LANG"))
    if strings.Contains(lang, "UTF") {
        fmt.Printf("This terminal supports unicode and U+25b3 is : %c\n", '\u25b3')
    } else {
        fmt.Println("This terminal does not support unicode")
    }
}
```


```txt

This terminal supports unicode and U+25b3 is : △

```



## Haskell


```Haskell
import System.Environment
import Data.List
import Data.Char
import Data.Maybe

main = do 
        x <- mapM lookupEnv ["LANG", "LC_ALL", "LC_CTYPE"]
        if any (isInfixOf "UTF". map toUpper) $ catMaybes x 
         then putStrLn "UTF supported: \x25b3"
         else putStrLn "UTF not supported"

```

Output:

```txt

UTF supported: △

```



## jq

The jq "env" function is required to inspect environment variables. It is NOT available in jq version 1.4.

Note also that "The values of locale categories are determined by a precedence order ..."
--  http://pubs.opengroup.org/onlinepubs/007908799/xbd/envvar.html

"has_unicode_support" therefore cannot simply test whether one of the variables LC_ALL, LC_TYPE and LANG contains the string UTF.

```jq
def has_unicode_support:
  def utf: if . == null then false else contains("UTF") or contains("utf") end;
  env.LC_ALL
  | if utf then true
    elif . != null and . != "" then false
    elif env.LC_CTYPE | utf then true
    else env.LANG | utf
    end ;

def task:
  if has_unicode_support then "\u25b3"
  else error("HW65001 This program requires a Unicode-compatible terminal")
  end ;

task
```

 $ jq -M -r -n -f Terminal_control.jq
 jq: error: HW65001 This program requires a Unicode-compatible terminal

 # In a galaxy not far away:
 $ jq -M -r -n -f Terminal_control.jq
 △


## Jsish

Detection based on code from other entries, looking in LC_ALL, LC_CTYPE, LANG for hints of 'UTF'.

```javascript
/* Terminal Control/Unicode, in Jsish */

var utf = false;
for (var envar of ['LC_ALL', 'LC_CTYPE', 'LANG']) {
    var val = Util.getenv(envar);
    if (val && (val.search(/utf/i) > 0)) {
        utf = true;
        break;
    }
}

puts((utf) ? '\u25b3' : 'Unicode support not detected');

/*
=!EXPECTSTART!=
△
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish --U terminalControlUnicode.jsi
△
```



## Julia

```Julia

c = '\u25b3'

if ismatch(r"UTF", get(ENV, "LANG", ""))
    println("This output device supports Unicode: ", c)
else
    println("This output device does not support Unicode.")
end

```


```txt

This output device supports Unicode: △

```



## Kotlin

```scala
// version 1.1.2

fun main(args: Array<String>) {
    val supportsUnicode = "UTF" in System.getenv("LANG").toUpperCase()
    if (supportsUnicode)
        println("This terminal supports unicode and U+25b3 is : \u25b3")
    else
        println("This terminal does not support unicode")
}
```


```txt

This terminal supports unicode and U+25b3 is : △

```



## Lasso


```Lasso
local(env_vars = sys_environ -> join('###'))
if(#env_vars >> regexp(`(LANG|LC_ALL|LC_CTYPE).*?UTF.*?###`)) => {
	stdout('UTF supported \u25b3')
else
	stdout('This terminal does not support UTF')
}
```


```txt
UTF supported △

```


## M2000 Interpreter

M2000 Environment has own console (with graphics support)

```M2000 Interpreter

Module CheckIt {
      If IsWine then Font "DejaVu Sans"
      Cls
      Report format$("\u25B3")
      Keyboard 0x25B3, format$("\u25B3")
      \\ report use kerning
      Report Key$+"T"+Key$
      Keyboard 0x25B3, format$("\u25B3")
      Print Key$;"T";Key$
}
Checkit

```


## Mathematica


```Mathematica
If[StringMatchQ[$CharacterEncoding, "UTF*"], Print[FromCharacterCode[30000]], Print["UTF-8 capable terminal required"]]
->田
```



## Mercury


```mercury
:- module unicode_output.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    list.map_foldl(io.get_environment_var, ["LANG", "LC_ALL", "LC_CTYPE"], EnvValues, !IO),
    ( if
        list.member(EnvValue, EnvValues),
        EnvValue = yes(Lang),
        string.sub_string_search(Lang, "UTF-8", _)
    then
        io.write_string("Unicode is supported on this terminal and U+25B3 is : \u25b3\n", !IO)
    else
        io.write_string("Unicode is not supported on this terminal.\n", !IO)
    ).
```

Output:

```txt
Unicode is supported on this terminal and U+25B3 is : △
```



## Nemerle

Typically, on a windows system, the output encoding is '''not''' UTF-8, so in an actual application it would make more sense to set <tt>Console.OutputEncoding</tt> than to merely check it.

```Nemerle
using System.Console;

module UnicodeOut
{
    Main() : void
    {
        if (OutputEncoding.ToString() == "System.Text.UTF8Encoding") Write("Δ") 
        else Write("Console encoding may not support Unicode characters.");
    }
}
```



## Nim


```nim
import os, strutils

if "utf" in getEnv("LANG").toLower:
  echo "Unicode is supported on this terminal and U+25B3 is: △"
else:
  echo "Unicode is not supported on this terminal."
```



## Perl

Much like Perl 6...

```perl
die "Terminal can't handle UTF-8"
    unless $ENV{LC_ALL} =~ /utf-8/i or $ENV{LC_CTYPE} =~ /utf-8/i or $ENV{LANG} =~ /utf-8/i;

print "△ \n";
```


## Perl 6


```perl6
die "Terminal can't handle UTF-8"
    unless first(*.defined, %*ENV<LC_ALL LC_CTYPE LANG>) ~~ /:i 'utf-8'/;
say "△";
```

```txt
△
```



## Phix

Works on both linux and windows.

The following (grubby low-level details are hidden away in) builtins/unicode_console.e which is now included in the standard distribution:

```Phix
include builtins\cffi.e
constant tGSH = """
HANDLE WINAPI GetStdHandle(
  _In_  DWORD nStdHandle
);
""",
tSCOCP = """
BOOL WINAPI SetConsoleOutputCP(
  _In_  UINT wCodePageID
);
""",		   
STD_OUTPUT_HANDLE = -11,
CP_UTF8 = 65001,
envset = {"LANG","LC_ALL","LC_CTYPE"}

atom k32 = NULL, xGetStdHandle, hConsole, xSetConsoleOutputCP

global function unicode_console()
-- initialises the windows console for unicode, and
-- returns true if unicode is supported, else false.
bool res = false
    if platform()=WINDOWS then
        if k32=NULL then
            puts(1,"")  -- force console to exist
            k32 = open_dll("kernel32.dll")
            xGetStdHandle = define_cffi_func(k32,tGSH)
            hConsole = c_func(xGetStdHandle,{STD_OUTPUT_HANDLE})
            xSetConsoleOutputCP = define_cffi_func(k32,tSCOCP)
        end if
        -- following is equivalent to running "chcp 65001":
        res = c_func(xSetConsoleOutputCP,{CP_UTF8})
    else    -- LINUX
        for i=1 to length(envset) do
            if match("UTF",upper(getenv(envset[i])))!=0 then
                res = true
                exit
            end if
        end for
    end if
    return res
end function
```

Which can be used like this:

```Phix
include builtins\unicode_console.e

-- pi, root, lambda, sigma, delta
constant prlsd = "\u03C0\u221A\u03BB\u03A3\u25B3"

if unicode_console() then
    puts(1,prlsd&"\n")
else
    puts(1,"unicode is not supported\n")
end if
```

Note that delta does not work on Windows (see talk page) but the others do, and all five work on linux.


## PicoLisp


```PicoLisp
(if (sub? "UTF-8" (or (sys "LC_ALL") (sys "LC_CTYPE") (sys "LANG")))
   (prinl (char (hex "25b3")))
   (quit "UTF-8 capable terminal required") )
```



## Python


```Python
import sys

if "UTF-8" in sys.stdout.encoding:
    print("△")
else:
    raise Exception("Terminal can't handle UTF-8")
```



## Racket


```racket

#lang racket
(displayln
 (if (regexp-match? #px"(?i:utf-?8)"
                    (or (getenv "LC_ALL") (getenv "LC_CTYPE") (getenv "LANG")))
   "\u25b3" "No Unicode detected."))

```



## Ruby


```ruby
#encoding: UTF-8       # superfluous in Ruby >1.9.3

if ENV.values_at("LC_ALL","LC_CTYPE","LANG").compact.first.include?("UTF-8")
  puts "△"
else
  raise "Terminal can't handle UTF-8"
end

```



## Scala

Ad hoc in the REPL:
```Scala>scala
 println(s"Unicode is supported on this terminal and U+25B3 is : \u25b3")
Unicode is supported on this terminal and U+25B3 is : △
```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/console.htm console.s7i] defines
[http://seed7.sourceforge.net/libraries/console.htm#STD_CONSOLE STD_CONSOLE], which can used directly,
or be assigned to [http://seed7.sourceforge.net/libraries/stdio.htm#OUT OUT] (which is the default
output file). STD_CONSOLE supports Unicode under Linux and Windows.


```seed7
$ include "seed7_05.s7i";
  include "environment.s7i";
  include "console.s7i";

const proc: main is func
  begin
    if pos(lower(getenv("LANG")), "utf") <> 0 or
       pos(lower(getenv("LC_ALL")), "utf") <> 0 or
       pos(lower(getenv("LC_CTYPE")), "utf") <> 0 then
      writeln(STD_CONSOLE, "Unicode is supported on this terminal and U+25B3 is: △");
    else
      writeln("Unicode is not supported on this terminal.");
    end if;
  end func;
```



## Sidef


```ruby
if (/\bUTF-?8/i ~~ [ENV{"LC_ALL","LC_CTYPE","LANG"}]) {
    say "△"
} else {
    die "Terminal can't handle UTF-8.\n";
}
```



## Tcl

Tcl configures the standard output channel to use the system encoding by default. The system encoding is formally the encoding for use when communicating with the OS (e.g., for filenames) but is virtually always correlated with the default terminal encoding.
```tcl
# Check if we're using one of the UTF or "unicode" encodings
if {[string match utf-* [encoding system]] || [string match *unicode* [encoding system]]} {
    puts "\u25b3"
} else {
    error "terminal does not support unicode (probably)"
}
```
Note that idiomatic Tcl code would not perform such a check; it would just produce the output which would be translated as best as possible (possibly into the target encoding's placeholder character).


## UNIX Shell

This script only checks if the name of the locale contains "UTF-8". This often works because many UTF-8 locales have names like "en_US.UTF-8". This script will fail to recognize a Unicode terminal if:

* The locale is a UTF-8 locale, but does not have "UTF-8" in its name.
* The locale uses some other Unicode Transformation Format, such as GB18030.

```bash
unicode_tty() {
  # LC_ALL supersedes LC_CTYPE, which supersedes LANG.
  # Set $1 to environment value.
  case y in
  ${LC_ALL:+y})		set -- "$LC_ALL";;
  ${LC_CTYPE:+y})	set -- "$LC_CTYPE";;
  ${LANG:+y})		set -- "$LANG";;
  y)			return 1;;  # Assume "C" locale not UTF-8.
  esac
  # We use 'case' to perform pattern matching against a string.
  case "$1" in
  *UTF-8*)		return 0;;
  *)			return 1;;
  esac
}

if unicode_tty; then
  # printf might not know \u or \x, so use octal.
  # U+25B3 => UTF-8 342 226 263
  printf "\342\226\263\n"
else
  echo "HW65001 This program requires a Unicode compatible terminal" >&2
  exit 252    # Incompatible hardware
fi
```


The terminal might support UTF-8, but its fonts might not have every Unicode character. Unless they have U+25B3, the output will not look correct. Greek letters like U+25B3 tend to be common, but some fonts might not have Chinese characters (for example), and almost no fonts have dead scripts such as Cuneiform.


## zkl

This code works for Unix/Linux, Windows XP cmd terminals don't support UTF-8.

```zkl
if(System.isUnix and T("LC_CTYPE","LC_LANG","LANG").apply(System.getenv)
		.filter().filter("holds","UTF"))
   println("This terminal supports UTF-8 (\U25B3;)");
else println("I have doubts about UTF-8 on this terminal.");
```

```txt

This terminal supports UTF-8 (△)

```



## ZX Spectrum Basic



```zxbasic
10 REM There is no Unicode delta in ROM
20 REM So we first define a custom character
30 FOR l=0 TO 7
40 READ n
50 POKE USR "d"+l,n
60 NEXT l
70 REM our custom character is a user defined d
80 PRINT CHR$(147): REM this outputs our delta
9500 REM data for our custom delta
9510 DATA 0,0,8,20,34,65,127,0

```



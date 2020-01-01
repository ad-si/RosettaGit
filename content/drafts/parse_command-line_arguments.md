+++
title = "Parse command-line arguments"
description = ""
date = 2019-10-17T05:53:26Z
aliases = []
[extra]
id = 10250
[taxonomies]
categories = []
tags = []
+++

{{draft task|Basic language learning}}

[[Command-line arguments]] can be quite complicated, as in:

:::: <big> '''nc   -v   -n   -z   -w   1   192.168.1.2   1-1000''' </big>


Many languages provide a library (getopt or GetOpt) to parse the raw command line options in an intelligent way.





## AutoHotkey

For AutoHotkey v1.1+

```AutoHotkey
;Get Arguments as an array
if 0 > 0
{
	argc=%0%
	args:=[]
	Loop, %argc%
		args.Insert(%A_Index%)
}
else
{
	;if got no arguments, run self with arguments
	Run,%a_scriptFullpath% -i Lib\* -c files.c --verbose -o files.o --Optimze
	ExitApp
}

;Parse arguments
i:=0, msg:=""
while( i++ < argc ) {
	c:=SubStr(args[i],1,1)
	if c in -,/ ; List all switch chars
	{
		if ( SubStr(args[i],1,2) == "--" ) ; if "--" is used like "--verbose"
			msg:=msg args[i] "`t:`tTrue (Boolean)`n" ; parse as boolean
		else
			msg:=msg args[i] "`t:`t" args[++i] "`n"
	}
	else
		msg:=msg args[i] "`t:`t(normal)`n"
}

MsgBox % "Parsed Arguments :`n" msg
```

'''Output (MsgBox):'''

```txt
Parsed Arguments :
-i        :  Lib\*
-c        :  files.c
--verbose :  True (Boolean)
-o        :  files.o
--Optimze :  True (Boolean)
```



## AWK

{{works with|gawk}}

```awk
#!/usr/bin/awk -E
# -E instead of -f so program arguments don't conflict with Gawk arguments
@include "getopt.awk"
BEGIN {
  while ((C = getopt(ARGC, ARGV, "ht:u:")) != -1) {
    opti++
    if(C == "h") {
      usage()
      exit
    }
    if(C == "t")
      tval = Optarg
    if(C == "u")
      uval = Optarg
  }
  print "There are " opti " arguments."
  if(tval) print "-t = " tval
  if(uval) print "-u = " uval
}
```



## Bracmat

Per default, Bracmat treats all arguments as expressions and parses and evaluates them from left to right. A call to the function <code>arg$</code> pops the next argument from the list of arguments and returns it as an inert string in no need of further parsing and evaluation.

```txt
bracmat arg$:?a 123 arg$:?b 77 !a+!b:?c out$!c
```

Output:

```txt
200
```



## C

The man page for getopt (man 3 getopt) provides better option handling with examples. But if you just want to parse one argument... (adapted from simple database task):

```c
#include <stdio.h>
int main(int argc, char **argv){
    int i;
    const char *commands[]={"-c", "-p", "-t", "-d", "-a", NULL};
    enum {CREATE,PRINT,TITLE,DATE,AUTH};
    if (argc<2) {
usage:   printf ("Usage: %s [commands]\n"
            "-c  Create new entry.\n"
            "-p  Print the latest entry.\n"
            "-t  Sort by title.\n"
            "-d  Sort by date.\n"
            "-a  Sort by author.\n",argv[0]);
        return 0;
    }
    for (i=0;commands[i]&&strcmp(argv[1],commands[i]);i++);
    switch (i) {
    case CREATE:
...
    break;
    case PRINT:
...
    break;
...
...
    default:
    printf ("Unknown command..." ...);
    goto usage;
    }
    return 0;
}
```



## Clojure

See [https://github.com/clojure-cookbook/clojure-cookbook/blob/master/03_general-computing/3-07_parse-command-line-arguments.asciidoc Parsing Command-Line Arguments] from O'Reilly's Clojure Cookbook github.


## D

The [http://dlang.org/phobos/std_getopt.html getopt module] in D's standard library is inspired by Perl's Getopt::Long module. The syntax of Phobos getopt infers the expected parameter types from the static types of the passed-in pointers.

```d
import std.stdio, std.getopt;

void main(string[] args) {
    string data = "file.dat";
    int length = 24;
    bool verbose;
    enum Color { no, yes }
    Color color;

    args.getopt("length",  &length,  // Integer.
                "file",    &data,    // String.
                "verbose", &verbose, // Boolean flag.
                "color",   &color);  // Enum.

    writeln("length: ", length);
    writeln("file: ", data);
    writeln("verbose: ", verbose);
    writeln("color: ", color);
}
```

{{out|Usage example}}

```txt
C:\getopt_test --verbose --length 12
length: 12
file: file.dat
verbose: true
color: no
```



## Elixir

Elixir provides an option parser in a library module called <tt>OptionParser</tt>.


```elixir
#!/usr/bin/env elixir
IO.puts 'Arguments:'
IO.inspect OptionParser.parse(System.argv())
```



```bash
$ ./parse-args.exs --a --b --c=yes --no-flag --verbose -V -a=1 -b=t -- apple banana
Arguments:
{[a: true, b: true, c: "yes", no_flag: true, verbose: true],
 ["apple", "banana"], [{"-V", nil}, {"-a", "1"}, {"-b", "t"}]}
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Program (commandline.exe) invoked like this:
' commandline nc   -v   -n   -z   -w   1   192.168.1.2   1-1000

Dim argc As Integer = __FB_ARGC__
Dim argv As ZString Ptr Ptr = __FB_ARGV__

Print "The program was invoked with the following command line arguments:"
Print

For i As Integer = 0 To argc - 1
   Print "Arg"; i + 1; " = "; *argv[i]
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

The program was invoked with the following command line arguments:

Arg 1 = commandline
Arg 2 = nc
Arg 3 = -v
Arg 4 = -n
Arg 5 = -z
Arg 6 = -w
Arg 7 = 1
Arg 8 = 192.168.1.2
Arg 9 = 1-1000

```



## Go

Most simply, implementing the suggested example from the talk page:

```go
package main

import (
    "flag"
    "fmt"
)

func main() {
    b := flag.Bool("b", false, "just a boolean")
    s := flag.String("s", "", "any ol' string")
    n := flag.Int("n", 0, "your lucky number")
    flag.Parse()
    fmt.Println("b:", *b)
    fmt.Println("s:", *s)
    fmt.Println("n:", *n)
}
```

Example runs:

```txt

> parse
b: false
s:
n: 0

> parse -s bye -b
b: true
s: bye
n: 0

> parse -n 99 -s "say my name"
b: false
s: say my name
n: 99

```


=={{header|Icon}} and {{header|Unicon}}==
The Icon Programming Library provides a procedure for processing command line options.  See the library reference for detailed documentation.  The code below is an example.


```Icon
link options

procedure main(ARGLIST)
/errproc := stop                                  # special error procedure or stop()
opstring := "f!s:i+r.flag!string:integer+real."   # example
opttable := options(ARGLIST,optstring,errproc)

if \opttable[flag] then ...  # test a flag
r  := opttable(real)         # assign a real
r2 := opttable(r)            # assign another real
s  := opttable(s)            # assign a string
i  := opttable(i)            # assign an integer
...
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/options.icn options.icn supports getting command-line options]


## J


When J starts up from the command line, the command line arguments are available in the array <code>ARGV</code>.  On modern machines, the first command line argument is the name of the executable (the J interpeter, in this case).

Typically, the next argument (if present) is the name of a file whose contents will be executed.

Further command line analysis might include:

:Test if an argument is present:

::
```j
   (<'-b') e. ARGV
```


::This is true if the argument is present and false, if it is not.

:Or, find the name of an optional file:

::
```j
   (ARGV i.<'-f') {:: }.ARGV,a:
```


::This is the name of the first file named after the first -f argument, or empty if there was no such file.

Other concepts are also possible...


## Julia

{{works with|Julia|0.6}}

Example taken from the official documentation of [https://carlobaldassi.github.io/ArgParse.jl/stable/ ArgParse docs].


```julia
using ArgParse

function parse_commandline()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--opt1"
            help = "an option with an argument"
        "--opt2", "-o"
            help = "another option with an argument"
            arg_type = Int
            default = 0
        "--flag1"
            help = "an option without argument, i.e. a flag"
            action = :store_true
        "arg1"
            help = "a positional argument"
            required = true
    end

    return parse_args(s)
end

function main()
    parsed_args = parse_commandline()
    println("Parsed args:")
    for (arg,val) in parsed_args
        println("  $arg  =>  $val")
    end
end

main()
```



## Kotlin


```scala
// version 1.0.6 (packaged as parse_cla.jar)

fun main(args: Array<String>) = println(args.asList())
```


{{out}}

```txt

c:\kotlin-compiler-1.0.6>java -jar parse_cla.jar nc -v -n -z -w 1 192.168.1.2 1-1000
[nc, -v, -n, -z, -w, 1, 192.168.1.2, 1-1000]

```



## Mathematica

The command line is parsed and stored into a list of strings to ease manual handling by list processing functions.

```Mathematica

$CommandLine
-> {math, -v, -n, -z, -w, 1, 192.168.1.2, 1-1000}

```


## PARI/GP

GP exists in a REPL and so it doesn't make sense to parse command-line arguments. But PARI can parse them just like [[#C|C]]:

```c
#include <pari/pari.h>
#include <stdio.h>

int main(int argc, char **argv){
	if(strcmp(argv[1],"-n"))
		pari_printf("8 + 1 = %Ps\n", addii(int2u(3), gen_1));
	return 0;
}
```



## Nim


```nim
import os
import parseopt

proc main =
  # Directly accessing the app name and parameters
  echo "app name: ", getAppFilename().extractFilename()
  echo "# parameters: ", paramCount()
  for ii in 1 .. paramCount():    # 1st param is at index 1
    echo "param ", ii, ": ", paramStr(ii)

  echo ""

  # Using parseopt module to extract short and long options and arguments
  var argCtr : int

  for kind, key, value in getOpt():
    case kind
    of cmdArgument:
      echo "Got arg ", argCtr, ": \"", key, "\""
      argCtr.inc

    of cmdLongOption, cmdShortOption:
      case key
      of "v", "n", "z", "w":
        echo "Got a \"", key, "\" option with value: \"", value, "\""
      else:
        echo "Unknown option: ", key

    of cmdEnd:
      discard


main()

```


Sample command line:

```txt

parsecmdline ab -z cd ef -w=abcd --w=1234 -v -e -x 1-1000

```


Output:

```txt

app name: parsecmdline
# parameters: 10
param 1: ab
param 2: -z
param 3: cd
param 4: ef
param 5: -w=abcd
param 6: --w=1234
param 7: -v
param 8: -e
param 9: -x
param 10: 1-1000

Got arg 0: "ab"
Got a "z" option with value: ""
Got arg 1: "cd"
Got arg 2: "ef"
Got a "w" option with value: "abcd"
Got a "w" option with value: "1234"
Got a "v" option with value: ""
Unknown option: e
Unknown option: x
Got arg 3: "1-1000"

```



## Perl


Use the <tt>Getopt::Long</tt> module:


```perl
# Copyright Shlomi Fish, 2013 under the MIT/X11 License.

use strict;
use warnings;

use Getopt::Long qw(GetOptions);
my $output_path;
my $verbose = '';
my $length = 24;

GetOptions(
    "length=i" => \$length,
    "output|o=s" => \$output_path,
    "verbose!" => \$verbose,
) or die ("Error in command line arguments");

print "Outputting to '", ($output_path // '(undefined)'), "' path, with ",
    ($verbose ? "Verbosity" : "No verbosity"),
    " and a length of $length.\n";

```


The output from it is:


```txt

$ perl getopt-test.pl --verbose -o foo.xml
Outputting to 'foo.xml' path, with Verbosity and a length of 24.
$ perl getopt-test.pl --verbose
Outputting to '(undefined)' path, with Verbosity and a length of 24.
$ perl getopt-test.pl --verbose --length=190
Outputting to '(undefined)' path, with Verbosity and a length of 190.
$ perl getopt-test.pl --verbose --length=190 -o test.txt
Outputting to 'test.txt' path, with Verbosity and a length of 190.

```



## Perl 6

At the end of running any top-level code (which can preprocess the arguments if it likes), Perl 6 automatically examines any remaining arguments and transforms them into a call to a <tt>MAIN</tt> routine, if one is defined.  The arguments are parsed based on the signature of the routine, so that options are mapped to named arguments.

```perl6
sub MAIN (Bool :$b, Str :$s = '', Int :$n = 0, *@rest) {
    say "Bool: $b";
    say "Str: $s";
    say "Num: $n";
    say "Rest: @rest[]";
}
```

{{out}}

```txt
$ ./main -h
Usage:
  ./main [-b] [-s=<Str>] [-n=<Int>] [<rest> ...]

$ ./main -b -n=42 -s=turtles all the way down
Bool: True
Str: turtles
Num: 42
Rest: all the way down
```

If there are multiple <tt>MAIN</tt> subs, they are differentiated by multiple dispatch.  A help message can automatically be generated for all the variants.  The intent of this mechanism is not to cover every possible switch structure, but just to make it drop-dead easy to handle most of the common ones.


## Phix


```Phix
sequence res = command_line()
?res
```

{{out}}
Interpreted: res[1] is the interpreter, res[2] the source

```txt

> p test nc -v -n -z -w 1 192.168.1.2 1-1000
{"C:\\Program Files (x86)\\Phix\\p.exe","C:\\Program Files (x86)\\Phix\\test.exw","nc","-v","-n","-z","-w","1","192.168.1.2","1-1000"}

```

Compiled: both res[1] and res[2] are the executable

```txt

> p -c test nc -v -n -z -w 1 192.168.1.2 1-1000
{"C:\\Program Files (x86)\\Phix\\test.exe","C:\\Program Files (x86)\\Phix\\test.exe","nc","-v","-n","-z","-w","1","192.168.1.2","1-1000"}
> test nc -v -n -z -w 1 192.168.1.2 1-1000
{"C:\\Program Files (x86)\\Phix\\test.exe","C:\\Program Files (x86)\\Phix\\test.exe","nc","-v","-n","-z","-w","1","192.168.1.2","1-1000"}

```




## Prolog

Works in SWI-Prolog.


```Prolog
:- initialization(main, main).

main(Argv) :-
	opt_spec(Spec),
	opt_parse(Spec, Argv, Opts, _),
	(
		member(help(true), Opts) -> show_help
		; maplist(format('~w~n'), Opts)
	).

show_help :-
	opt_spec(Spec),
	opt_help(Spec, HelpText),
	write('Usage: swipl opts.pl <options>\n\n'),
	write(HelpText).

opt_spec([
	[opt(help),
		type(boolean),
		default(false),
		shortflags([h]),
		longflags([help]),
		help('Show Help')],

	[opt(noconnect),
		type(boolean),
		default(false),
		shortflags([n]),
		longflags([noconnect]),
		help('do not connect, just check server status')],

	[opt(server),
		type(atom),
		default('www.google.com'),
		shortflags([s]),
		longflags([server]),
		help('The server address.')],

	[opt(port),
		type(integer),
		default(5000),
		shortflags([p]),
		longflags([port]),
		help('The server port.')]
]).
```


{{out}}

```Powershell

# no options set (use defaults)
$ swipl .\opts.pl
help(false)
noconnect(false)
server(www.google.com)
port(5000)

# setting various options
$ swipl .\opts.pl --server www.test.com -p 2342 -n
help(false)
server(www.test.com)
port(2342)
noconnect(true)

# show help
$ swipl .\opts.pl -h
Usage: swipl opts.pl <options>

--help       -h  boolean=false        Show Help
--noconnect  -n  boolean=false        do not connect, just check server status
--server     -s  atom=www.google.com  The server address.
--port       -p  integer=5000         The server port.

```



## Python

Version 2.3+

```Python

from optparse import OptionParser
[...]
parser = OptionParser()
parser.add_option("-f", "--file", dest="filename",
                  help="write report to FILE", metavar="FILE")
parser.add_option("-q", "--quiet",
                  action="store_false", dest="verbose", default=True,
                  help="don't print status messages to stdout")

(options, args) = parser.parse_args()

example:

<yourscript> --file=outfile -q

```



## PicoLisp

PicoLisp doesn't have a library to get options. Instead, the command line is parsed at startup and handled in the following way: Each command line argument is executed (interpreted) as a Lisp source file, except that if the first character is a hypen '-', then that arguments is taken as a Lisp function call (without the surrounding parentheses). For example, the command line

```shell
$ ./pil abc.l -foo def.l -"bar 3 4" -'mumble "hello"' -bye
```

has the effect that
# The file "abc.l" is executed
# (foo) is called
# The file "def.l" is executed
# (bar 3 4) is called
# (mumble "hello") is called
# (bye) is called, resulting in program termination
Command line arguments like "-v", "-n" and "-z" can be implemented simply by defining three functions 'v', 'n' and 'z'.

In addition to the above mechanism, the command line can also be handled "manually", by either processing the list of arguments returned by '[http://software-lab.de/doc/refA.html#argv argv]', or by fetching arguments individually with '[http://software-lab.de/doc/refO.html#opt opt]'.


## PowerShell

Powershell functions and filters handle options organically, with advanced .NET support to handle complex and advanced options including aliases, ranges, sets, datatypes, and more. [https://msdn.microsoft.com/en-us/powershell/reference/5.0/microsoft.powershell.core/about/about_parsing See] [https://msdn.microsoft.com/powershell/reference/5.1/Microsoft.PowerShell.Core/about/about_Functions more] [https://msdn.microsoft.com/en-us/powershell/reference/5.0/microsoft.powershell.core/about/about_functions_advanced here]. However, to parse options 'classically', you can write a custom parser. A slightly messy version (inspired by ruby optparse) that can only handle switches and relies on RegEx:

```Powershell

$options = @{
        opt1 = [bool] 0
        opt2 = [bool] 0
        opt3 = [bool] 0
    }
$help = @"
    FUNCTION usage: FUNCTION [-p] [-w] [-h] [-c] <int><float><string>PARAMETERS...

    Lorem Ipsum blah blah blah

    NOTE something yada yada

    Options:
        -p,--pxx    Name    Some option that has significance with the letter 'p'
        -w,--wxx    Name    Some option that has significance with the letter 'w'
        -c,--cxx    Name    Some option that has significance with the letter 'c'
        -h,--help   Help    Prints this message
"@

    function parseOptions ($argv,$options) {
        $opts = @()
        if (!$argv) { return $null }
        foreach ($arg in $argv) {
            # Make sure the argument is something you are expecting
            $test = ($arg -is [int]) -or
                    ($arg -is [string]) -or
                    ($arg -is [float])
            if (!$test) {
                Write-Host "Bad argument: $arg is not an integer, float, nor string." -ForegroundColor Red
                throw "Error: Bad Argument"
            }
            if ($arg -like '-*') { $opts += $arg }
        }
        $argv = [Collections.ArrayList]$argv
        if ($opts) {
            foreach ($opt in $opts) {
                switch ($opt) {
                    {'-p' -or '--pxx'}   { $options.opt1 = [bool] 1 }
                    {'-w' -or '--wxx'}   { $options.opt2 = [bool] 1 }
                    {'-c' -or '--cxx'}   { $options.opt3 = [bool] 1 }
                    {'-h' -or '--help'}  { Write-Host $help -ForegroundColor Cyan; break 1 }
                    default {
                        Write-Host "Bad option: $opt is not a valid option." -ForegroundColor Red
                        throw "Error: Bad Option"
                    }
                }
            $argv.Remove($opt)
            }
        }
        return [array]$argv,$options
    }#fn

```

Usage (in some function or script):

```Powershell


   $argv,$options = parseOptions $args $options

    if ($options.opt3) {
        $foo = $blah - ($yada * $options.opt1) + ($yada * $options.opt2)
        $bar = $argv | SomeOtherFilter | Baz
    }

```

Usage in shell:

```shell

$> function -c --wxx arg1 arg2

```

Note that this works in Powershell. All of the arguments after the function name will be passed as strings to the function, which then calls them as an array with the automatic variable, $args. The custom parser/function does the work from there, turning the strings into flags and typed arguments. WARNING: This is reinventing the wheel to an extreme degree.


## Racket


Racket has a good command-line parsing library, the following demonstrates some of its features:


```racket

#!/usr/bin/env racket
#lang racket

(define loglevel 1)
(define mode 'new)
(define ops '())
(define root #f)

(command-line
 #:multi
 [("-v" "--verbose")    "more verbose"      (set! loglevel (add1 loglevel))]
 [("-q" "--quiet")      "be quiet"          (set! loglevel 0)]
 #:once-any
 [("-i" "--in-place")   "edit in-place"     (set! mode 'in-place)]
 [("-c" "--create-new") "create a new file" (set! mode 'new)]
 [("-n" "--dry-run")    "do nothing"        (set! mode #f)]
 #:once-each
 [("-d" "--directory") dir "work in a given directory" (set! root dir)]
 #:help-labels "operations to perform:"
 #:multi
 [("+l" "++line") "add a line"    (set! ops `(,@ops "add"))]
 [("-l" "--line") "delete a line" (set! ops `(,@ops "delete"))]
 [("-e" "--edit") "edit a line"   (set! ops `(,@ops "edit"))]
 #:args (file . files)
 (printf "Running on: ~a\n" (string-join (cons file files) ", "))
 (when root (printf "In Dir:     ~a\n" root))
 (printf "Mode:       ~s\n" mode)
 (printf "Log level:  ~s\n" loglevel)
 (printf "Operations: ~a\n" (string-join ops ", ")))

```


Sample runs:

```txt

$ ./foo -h
foo [ <option> ... ] <file> [<files>] ...
 where <option> is one of
* -v, --verbose : more verbose
* -q, --quiet : be quiet
/ -i, --in-place : edit in-place
| -c, --create-new : create a new file
\ -n, --dry-run : do nothing
  -d <dir>, --directory <dir> : work in a given directory
 operations to perform:
* +l, ++line : add a line
* -l, --line : delete a line
* -e, --edit : edit a line
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 * Asterisks indicate options allowed multiple times.
 /|\ Brackets indicate mutually exclusive options.
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'

$ /tmp/zz.rkt -viqvvd /tmp +ll -el foo bar baz
Running on: foo, bar, baz
In Dir:     /tmp
Mode:       in-place
Log level:  2
Operations: add, add, edit, delete

```



## REXX


```txt

╔═════════════════════════════════════════════════════════════════════════════╗
║ The subject of parsing text  (such as a command line)  is ingrained in the  ║
║ REXX language;  it has a PARSE instruction.  It's too rich to describe all  ║
║ its functionality/capabilities here,  but since the task isn't described,   ║
║ I'm assuming the    NC   [NetCat]  example  (or something like it)  is toe  ║
║ be parsed,  and I'm guessing at its syntax  (from other examples found on   ║
║ the web),  but I'll take a incomplete stab at it.                           ║
║                                                                             ║
║ For the most part,  every command appear to have their own rules for their  ║
║ operands (options),  as does the   NC   command.     For instance           ║
║                                                                             ║
║         nc  -u xxx -p port1 ···                                             ║
║                  and                                                        ║
║         nc  -u -p port1     ···                                             ║
║                                                                             ║
║ where the   -u   option has an operand in the first case,  but not the 2nd, ║
║ even though there is something following the   -u   option.                 ║
║                                                                             ║
║ It can only be assumed that any operand for the   -u   option can't start   ║
║ with a minus sign  [-].                                                     ║
╚═════════════════════════════════════════════════════════════════════════════╝

```


```rexx
/*REXX program demonstrates one method to parse options for a command (entered on the CL*/
parse arg opts                                   /*this preserves the case of  options. */
opts=space(opts)                                 /*elide superfluous blanks in options. */
!.=                                              /*all options to be  "null"  (default).*/
   do  while opts\==''                           /*keep parsing 'til all  opts  examined*/
   parse var opts x opts                         /*obtain a single keyword from options.*/

     select                                      /*hard-coded WHENs for option detection*/
     when x=='-e'   then parse var opts !.e_                                          opts
     when x=='-p'   then parse var opts !.p_                                          opts
     when x=='-n'   then !.z_=1
     when x=='-u'   then parse var opts !.uname_ !.unnn_                              opts
     when x=='-ul'  then parse var opts !.ul_                                         opts
     when x=='-vzu' then parse var opts !.vzu_   !.vzurange                           opts
     when x=='-w'   then parse var opts !.wStart_ !.waddr_ !.wrange_1 '-' !.wrange_2  opts
     when x=='-z'   then !.=1
     otherwise call sayer 'option  '      x      " isn't a known option."
     end     /*select*/
  end        /*do while*/

                                   /*check for conflicts here and/or validity of values.*/

if !.z_==1 & !.n_==1  then call sayer  "N  and  Z  can't both be specified."

if !.wrange_1\==''  then do                     /*see if it's a whole number (integer). */
                         if \isInt(!.wrange1_)  then call sayer "wRange isn't an integer."
                         yada yada yada
                          .
                          .
                          .
                         end

               ...stuff...
          ...more stuff...
...and still more stuff...
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isInt:   return  datatype(arg(1), 'W')           /*return  1  if  argument is an integer*/
isNum:   return  datatype(arg(1), 'N')           /*return  1  if  argument is a  number.*/
sayer:   say;      say '***error***'  arg(1);      exit 13
```


```txt

╔═══════════════════════════════════════════════════════════════════════╗
║ Note:  a programming trick is to append (say)  an underscore [_]  to  ║
║        an option's name as to not preclude that variable being used   ║
║        elsewhere in the REXX program.   That way, the option  J  can  ║
║        be used, as well as the variable  J  in the program.           ║
╚═══════════════════════════════════════════════════════════════════════╝

```



## Ruby

Ruby's standard library provides two different packages to parse command-line arguments.

* 'getoptlong' resembles the libraries from other languages.
* 'optparse' has more features.

=== Ruby with 'getoptlong' ===

```ruby
#!/usr/bin/env ruby

# == Synopsis
#
# pargs: Phone a friend
#
# == Usage
#
# pargs [OPTIONS]
#
# --help, -h:
#    show usage
#
# --eddy, -e <message>
#    call eddy
#
# --danial, -d <message>
#    call daniel
#
# --test, -t
#    run unit tests

require "getoptlong"
require "rdoc/usage"

def phone(name, message)
	puts "Calling #{name}..."
	puts message
end

def test
	phone("Barry", "Hi!")
	phone("Cindy", "Hello!")
end

def main
	mode = :usage

	name = ""
	message = ""

	opts=GetoptLong.new(
		["--help", "-h", GetoptLong::NO_ARGUMENT],
		["--eddy", "-e", GetoptLong::REQUIRED_ARGUMENT],
		["--daniel", "-d", GetoptLong::REQUIRED_ARGUMENT],
		["--test", "-t", GetoptLong::NO_ARGUMENT]
	)

	opts.each { |option, value|
		case option
		when "--help"
			RDoc::usage("Usage")
		when "--eddy"
			mode = :call
			name = "eddy"
			message = value
		when "--daniel"
			mode = :call
			name = "daniel"
			message = value
		when "--test"
			mode = :test
		end
	}

	case mode
	when :usage
		RDoc::usage("Usage")
	when :call
		phone(name, message)
	when :test
		test
	end
end

if __FILE__==$0
	begin
		main
	rescue Interrupt => e
		nil
	end
end
```



```txt
$ ./pargs.rb -h

Usage
-----
pargs [OPTIONS]

--help, -h:

   show usage

--eddy, -e <message>

   call eddy

--daniel, -d <message>

   call daniel

--test, -t

   run unit tests

$ ./pargs.rb -e Yo!
Calling eddy...
Yo!
$ ./pargs.rb --test
Calling Barry...
Hi!
Calling Cindy...
Hello!
```


=== Ruby with 'optparse' ===

```ruby
require 'optparse'

sflag = false
longflag = false
count = 0
percent = 50
fruit = nil

OptionParser.new do |opts|
  # Default banner is "Usage: #{opts.program_name} [options]".
  opts.banner += " [arguments...]"
  opts.separator "This demo prints the results of parsing the options."
  opts.version = "0.0.1"

  opts.on("-s", "Enable short flag") {sflag = true}
  opts.on("--long", "Enable long flag") {longflag = true}
  opts.on("-b", "--both", "Enable both -s and --long"
          ) {sflag = true; longflag = true}
  opts.on("-c", "--count", "Add 1 to count") {count += 1}

  # Argument must match a regular expression.
  opts.on("-p", "--percent PERCENT", /[0-9]+%?/i,
          "Percent [50%]") {|arg| percent = arg.to_i}

  # Argument must match a list of symbols.
  opts.on("-f", "--fruit FRUIT",
          [:apple, :banana, :orange, :pear],
          "Fruit (apple, banana, orange, pear)"
          ) {|arg| fruit = arg}

  begin
    # Parse and remove options from ARGV.
    opts.parse!
  rescue OptionParser::ParseError => error
    # Without this rescue, Ruby would print the stack trace
    # of the error. Instead, we want to show the error message,
    # suggest -h or --help, and exit 1.

    $stderr.puts error
    $stderr.puts "(-h or --help will show valid options)"
    exit 1
  end
end

print <<EOF
Short flag: #{sflag}
Long flag: #{longflag}
Count: #{count}
Percent: #{percent}%
Fruit: #{fruit}
Arguments: #{ARGV.inspect}
EOF
```



```txt
$ ruby takeopts.rb -h
Usage: takeopts [options] [arguments...]
This demo prints the results of parsing the options.
    -s                               Enable short flag
        --long                       Enable long flag
    -b, --both                       Enable both -s and --long
    -c, --count                      Add 1 to count
    -p, --percent PERCENT            Percent [50%]
    -f, --fruit FRUIT                Fruit (apple, banana, orange, pear)
$ ruby takeopts.rb -v
takeopts 0.0.1
$ ruby takeopts.rb -b -c
Short flag: true
Long flag: true
Count: 1
Percent: 50%
Fruit:
Arguments: []
$ ruby takeopts.rb -ccccp90% -f oran -- -arg
Short flag: false
Long flag: false
Count: 4
Percent: 90%
Fruit: orange
Arguments: ["-arg"]
```



## Rust


Using the [https://docs.rs/structopt StructOpt]:


```rust
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    #[structopt(short)]
    b: bool,
    #[structopt(short, required = false, default_value = "")]
    s: String,
    #[structopt(short, required = false, default_value = "0")]
    n: i32,
}

fn main() {
    let opt = Opt::from_args();
    println!("b: {}", opt.b);
    println!("s: {}", opt.s);
    println!("n: {}", opt.n);
}
```


Examples:


```txt

> parse
b: false
s:
n: 0

> parse -s bye -b
b: true
s: bye
n: 0

> parse -n 99 -s "say my name"
b: false
s: say my name
n: 99

```



## Scala

{{libheader|Scala}}

```Scala
object CommandLineArguments extends App {
    println(s"Received the following arguments: + ${args.mkString("", ", ", ".")}")
}
```



## Standard ML

{{works with|SML/NJ}}
{{works with|MLton}}
The following code listing can be compiled with both [[SML/NJ]] and [[MLton]]:

```sml
structure Test = struct

exception FatalError of string

fun main (prog, args) =
	(let
	  exception Args

	  val switch = ref false

	  fun do_A arg = print ("Argument of -A is " ^ arg ^ "\n")
	  fun do_B ()  = if !switch then print "switch is on\n" else print "switch is off\n"

          fun usage () = print ("Usage: " ^ prog ^ " [-help] [-switch] [-A Argument] [-B]\n")

	  fun parseArgs nil = ()
	    | parseArgs ("-help"     :: ts) = (usage();        parseArgs ts)
	    | parseArgs ("-switch"   :: ts) = (switch := true; parseArgs ts)
	    | parseArgs ("-A" :: arg :: ts) = (do_A arg;       parseArgs ts)
	    | parseArgs ("-B"        :: ts) = (do_B();         parseArgs ts)
	    | parseArgs _ = (usage(); raise Args)

	in
	  parseArgs args handle Args => raise FatalError "Error parsing args. Use the -help option.";
	  (* Do something; *)
	  OS.Process.success
	end)
	handle FatalError e => (print ("Fatal Error:\n"^e^"\n"); OS.Process.failure)
end

(* MLton *)
val _ = Test.main (CommandLine.name(), CommandLine.arguments())
```



###  SML/NJ

[[SML/NJ]] can compile source code to a "heap file", witch can than be executed by the interpreter with arguments given (see [http://stackoverflow.com/questions/5053149/sml-nj-how-to-compile-standalone-executable this entry on stackowerflow.com] for more information).
The <code>source.cm</code> file should lock like this:
<lang>Group
is
  SOURCE_FILE.sml
  $/basis.cm
```

To compile the program, use <code>ml-build sources.cm</code>. This should create a "heap file" <code>sources.x86-linux</code>, depending on your architecture.
The heap file can be executed with <code>sml @SMLload=sources.x86-linux ARGUMENTS</code>, or the script [http://www.smlnj.org/doc/heap2exec/index.html <code>heap2exec</code>] can be used to make a single executable.


###  MLton

[[MLton]] compiles the source file directly to a executable by invoking <code>mlton SOURCE_FILE.sml</code>.


## Tcl

The following proc detects and removes argument-less (-b) and one-argument options from the argument vector.

```Tcl
proc getopt {_argv name {_var ""} {default ""}} {
     upvar 1 $_argv argv $_var var
     set pos [lsearch -regexp $argv ^$name]
     if {$pos>=0} {
         set to $pos
         if {$_var ne ""} {set var [lindex $argv [incr to]]}
         set argv [lreplace $argv $pos $to]
         return 1
     } else {
         if {[llength [info level 0]] == 5} {set var $default}
         return 0
     }
 }
```

Usage examples:
 getopt argv -sep sep ";"     ;# possibly override default with user preference
 set verbose [getopt argv -v] ;# boolean flag, no trailing word

Searching with -regexp allows to specify longer mnemonic names, so it still succeeds on longer flags, e.g.
 $ myscript.tcl -separator '\t' ...


## zkl

The Argh class provides command line parsing, it can do actions during parsing, leave it for you to do after parsing, print errors, the option list, short or long options, with or without option args, etc.

File myprogram.zkl:

```zkl
var ip;
argh := Utils.Argh(
T("v","v","print version",fcn{println("Version stub")}),
T("n","n","ignored"),
T("z","z","zazzle"),
T("+ip","","get IP address",fcn(arg){ip=arg}),
);
parsedArgs := argh.parse(vm.arglist);

println("Unparsed stuff: ",argh.loners);
println("The IP address is ",ip);
foreach option,arg in (parsedArgs){
   switch(option)   {
      case("z") { println("zazzle") }
   }
}
```


```txt
zkl myprogram nc -v -n -z --ip 192.168.1.2 1-1000
```

{{out}}

```txt
Version stub
Unparsed stuff: L("nc","1-1000")
The IP address is 192.168.1.2
zazzle

```


```txt
zkl myprogram nc -v -n -z --ip
```

{{out}}

```txt

Option "ip" is missing an arg
Options:
  --ip <arg>: get IP address
  --n (-n) : ignored
  --v (-v) : print version
  --z (-z) : zazzle

```


{{omit from|Axe}}

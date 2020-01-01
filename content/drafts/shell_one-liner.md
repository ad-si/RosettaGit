+++
title = "Shell one-liner"
description = ""
date = 2019-09-21T17:01:51Z
aliases = []
[extra]
id = 3028
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}

;Task:
Show how to specify and execute a short program in the language from a command shell, where the input to the command shell is only one line in length.

Avoid depending on the particular shell or operating system used as much as is reasonable; if the language has notable implementations which have different command argument syntax, or the systems those implementations run on have different styles of shells, it would be good to show multiple examples.





## ACL2


```bash
$ acl2 <<< '(cw "Hello.")'
```



## Ada

{{works with|gnat}}
'''under a unixoid shell''' (bash, sh, ...)


```bash
echo 'with Ada.text_IO; use Ada.text_IO; procedure X is begin Put("Hello!"); end X;' > x.adb; gnatmake x; ./x; rm x.adb x.ali x.o x
```


Note that this mercilessly overwrites and later deletes any files x.adb, x.ali, x,o and x in the current directory.


## Aikido


```aikido
echo 'println ("Hello")' | aikido
```



## Aime


```sh
$ src/aime -c 'o_text("Hello, World!\n");'
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386 - Interpret straight off}}

```bash
$ a68g -e 'print(("Hello",new line))'
```

Output:

```txt
Hello
```

{{works with|ELLA ALGOL 68|Any - tested with release 1.8.8d.fc9.i386 - translate to [[C]] and then compile and run}}
For an [[ELLA ALGOL 68]] one-liner, merge these lines of shell code:

```bash
code='print(("Hello", new line))'
a=/tmp/algol$$ s=/usr/share/algol68toc;
echo -e "PROGRAM algol$$ CONTEXT VOID\nUSE standard\nBEGIN\n$code\nEND\nFINISH\n" > $a.a68 &&
a68toc -lib $s -dir $s -uname TMP -tmp $a.a68 && rm $a.a68 &&
gcc $s/Afirst.o $a.c  -l{a68s,a68,m,c} -o $a && rm $a.c &&
$a; rm $a
```

Output:

```txt
Hello
```



## AppleScript


```AppleScript
osascript -e 'say "Hello, World!"'
```



## AWK

Maybe the most common way one can use awk is from the command line for one-liners, feeding the interpreter with an input.

```bash
$ awk 'BEGIN { print "Hello"; }'
```


A more "complex" and "real" example:

```bash
$ awk '/IN/ { print $2, $4; }' <input.txt
```


''Select'' field 2 and 4 of lines matching the regular expression <tt>/IN/</tt> (i.e. where IN appears)


## BASIC

The name of the BASIC executable will vary (common ones are ''basic'', ''bas'', and ''bwbasic''), but in general, a short program can be piped to the interpreter like any other language:

```bash
echo 'print "foo"'|basic
```


Note that under Windows (and presumably DOS) the two apostrophes (a.k.a. single quotes) should be omitted, since Windows doesn't remove them from the piped text (and the apostrophe is the comment character in many modern BASICs):

```dos
echo print "foo"|basic
```


Also, some popular interpreters (including [http://www.moria.de/~michael/bas/ Michael Haardt's '''bas'''] and [[Chipmunk Basic]]) will include an extra prompt before exiting unless you include <code>exit</code> or <code>system</code> (depending on the specific interpreter's syntax). This sample output shows both with and without <code>system</code> in bas:
 erik@satan:~$ echo 'print "foo"'|bas
 bas 2.2
 Copyright 1999-2009 Michael Haardt.
 This is free software with ABSOLUTELY NO WARRANTY.
 > foo
 > erik@satan:~$ echo 'print "foo":system'|bas
 bas 2.2
 Copyright 1999-2009 Michael Haardt.
 This is free software with ABSOLUTELY NO WARRANTY.
 > foo
 erik@satan:~$

Note that this is rather specific to [[Unix]]-like systems; most [[DOS]] and [[Windows]] interpreters are generally unable to handle programs in this manner, unless they were ported from a *nix system in the first place.

==={{Header|ZX Spectrum Basic}}===
On the ZX Spectrum, the ROM basic allows direct commands to be entered from the system prompt:


```zxbasic
PRINT "Hello World!"
```



## Bc


```bash
$ echo 'print "Hello "; var=99; ++var + 20 + 3' | bc
```

{{out}}

```txt
Hello 123
```



## Bracmat

This example uses the predefined function <code>tay</code> to make a taylor expansion of <code>e^x</code>.

DOS:

```bracmat
bracmat "put$tay$(e^x,x,20)&"
```

Linux:

```bracmat
bracmat 'put$tay$(e^x,x,10)&'
```

Output:

```txt
  1
+ x
+ 1/2*x^2
+ 1/6*x^3
+ 1/24*x^4
+ 1/120*x^5
+ 1/720*x^6
+ 1/5040*x^7
+ 1/40320*x^8
+ 1/362880*x^9
+ 1/3628800*x^10
+ 1/39916800*x^11
+ 1/479001600*x^12
+ 1/6227020800*x^13
+ 1/87178291200*x^14
+ 1/1307674368000*x^15
+ 1/20922789888000*x^16
+ 1/355687428096000*x^17
+ 1/6402373705728000*x^18
+ 1/121645100408832000*x^19
+ 1/2432902008176640000*x^20
```



## Burlesque


<lang>
Burlesque.exe --no-stdin "5 5 .+"

```


Using the official interpreter.


## C

{{works with|gcc}}
The following code leaves the file <tt>a.out</tt> in the current directory (it does not
delete it to avoid to call another shell/system dependent command/program). The
''current directory'' is not specified by <tt>./</tt> in every system...

```bash
$ echo 'main() {printf("Hello\n");}' | gcc -w -x c -; ./a.out
```


## C#
Note: whilst small, this is more than one line.

Requires PowerShell 2:

```powershell>
 Add-Type -TypeDefinition "public class HelloWorld { public static void SayHi() { System.Console.WriteLine(""Hi!""); } }"
> [HelloWorld]::SayHi()
Hi!
```



## Clojure

Note: whilst small, this is more than one line.

clj-env-dir comes with clojure-contrib.


```bash
$ clj-env-dir -e "(defn add2 [x] (inc (inc x))) (add2 40)"
#'user/add2
42
```



## CMake

This only works with [[Unix]] systems that have the device node <code>/dev/stdin</code>.


```bash
echo 'message(STATUS "Goodbye, World!")' | cmake -P /dev/stdin
```



## COBOL

Works with GnuCOBOL 2.0 or later


```bash
echo 'display "hello".' | cobc -xFj -frelax -
```


Longer, but avoids two relaxed syntax warnings:

```bash
echo 'id division. program-id. hello. procedure division. display "hello".' | cobc -xFj -
```



## Common Lisp

Varies by implementation

{{works with|SBCL}}

```bash
sbcl --noinform --eval '(progn (princ "Hello") (terpri) (quit))'
```

{{works with|CLISP}}

```bash
clisp.exe -q -x "(progn (format t \"Hello from CLISP\") (quit))"
```



## D

{{works with|D|2}}
requires [https://github.com/D-Programming-Language/tools/blob/master/rdmd.d rdmd]

```d
rdmd --eval="writeln(q{Hello World!})"
```


```txt
Hello World!
```



## Dc


```bash
dc -e '22 7/p'
```



## E


```bash
rune --src.e 'println("Hello")'
```


The <code>--src</code> option ends with the the filename extension the provided type of program would have:

<lang>rune --src.e-awt 'def f := &lt;swing:makeJFrame>("Hello"); f.show(); f.addWindowListener(def _{to windowClosing(_) {interp.continueAtTop()} match _{}}); interp.blockAtTop()'
```



## Elixir


```Elixir
$ elixir -e "IO.puts 'Hello, World!'"
Hello, World!
```



## Emacs Lisp


```bash
emacs -batch -eval '(princ "Hello World!\n")'
```

Or another example that does something useful: indent a [[C]] source file:

```bash
emacs -batch sample.c --eval '(indent-region (point-min) (point-max) nil)' -f save-buffer
```



## Erlang

Erlang always starts other applications that can run in parallel in the background, and as such will not die by itself. To kill erl, we sequentially run the 'halt' function from the 'erlang' module (the -S is there to guarantee 'halt' will be evaluated after the io function).

```bash
$ erl -noshell -eval 'io:format("hello~n").' -s erlang halt
hello
```


=={{header|F_Sharp|F#}}==

```cmd>
 echo printfn "Hello from F#" | fsi --quiet
Hello from F#
```



## Factor


```bash
$ factor -run=none -e="USE: io \"hi\" print"
```



## Forth

{{works with|GNU Forth}}

```bash
$ gforth -e ".( Hello) cr bye"
Hello
```



## Fortran

This example, stolen from the [[Shell_one-liner#c|c]] example is subject to the same caveats.  While contrived, FORTRAN as a one liner can easily handle some unique tasks.  Let's plot a Bessel function:

```bash

$ gawk 'BEGIN{print"write(6,\"(2(g12.3,x))\")(i/10.0,besj1(i/10.0), i=0,1000)\nend";exit(0)}'|gfortran -ffree-form -x f95 - | gnuplot -p -e 'plot "<./a.out" t "Bessel function of 1st kind" w l'

```

Sorry, I don't know how to upload my jpeg file for the Image tag.  Let's use the dumb display instead.

```txt

  0.6 +*------------+-------------+------------+-------------+------------++
      +**           +             +     Bessel function of 1st kind ****** +
  0.5 +**                                                                 ++
      |**                                                                  |
  0.4 +**                                                                 ++
      * *                                                                  |
  0.3 *+*   *                                                             ++
      * *  **   *                                                          |
  0.2 *+*  ***  **  **                                                    ++
  0.1 *+*  * *  **  **  ***  **  **   *   **   *                          ++
      * ** * * ***  **  * * ***  **  ***  **  **  ***  **  **  ***  **  ** |
    0 *+ * * * * *  * * * * * *  *** * * * *  *** * * ***  *** * * ***  ***+
      |  * * * * ***  * * * * ***  * * * * ***  *** * * **** *** *** ***  *|
 -0.1 ++ * * * *  **  *** ***  **  **  ***  **  **   **  **  **   **  **  **
      |  *** ***  **  **   **  **   *   *                                  |
 -0.2 ++ **   **  **                                                      ++
      |  **   *                                                            |
 -0.3 ++ **                                                               ++
      +  **         +             +            +             +             +
 -0.4 ++------------+-------------+------------+-------------+------------++
      0             20            40           60            80           100


```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Shell "echo For i As Integer = 1 To 10 : Print i : Next > zzz.bas && fbc zzz.bas && zzz"
Sleep
```


{{out}}

```txt

 1
 2
 3
 4
 5
 6
 7
 8
 9
 10

```



## Free Pascal

The FPC (Free Pascal compiler) comes with the utility <tt>instantfpc(1)</tt> or <tt>ifpc(1)</tt> for short (Debian or FreeBSD package <tt>fpc-utils</tt>):

```bash
echo "begin writeLn('Hi'); end." | ifpc /dev/stdin
```



## FutureBasic

This is forcing the issue. FB has much more elegant ways of interacting with the Unix Shell.

```futurebasic

include "ConsoleWindow":dim a$:open "Unix",1,"cal 10 2018":do:line input #1,a$:print a$:until eof(1):close 1

```


Output

```txt

    October 2018
Su Mo Tu We Th Fr Sa
    1  2  3  4  5  6
 7  8  9 10 11 12 13
14 15 16 17 18 19 20
21 22 23 24 25 26 27
28 29 30 31

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=4385adf6a841435779a7afff3dadb58b Click this link to run this code]'''

```gambas
Public Sub Main()

Shell "echo Hello World"

End
```

Output:

```txt

Hello World

```



## Gema


```bash
$ gema -p '\B=Hello\n@end'
Hello
```



## Go


```bash
echo 'package main;func main(){println("hlowrld")}'>/tmp/h.go;go run /tmp/h.go
```

{{out}}

```txt

hlowrld

```



## Groovy

{{works with|UNIX Shell}}

```bash
$ groovysh -q "println 'Hello'"
Hello
```


{{works with|Windows Command Interpreter}}

```cmd>C:\Users\user
 groovysh -q "println 'Hello'"
Hello
```



## Haskell


```bash
$ ghc -e 'putStrLn "Hello"'
Hello
```



## Huginn

Result of an expression is printed by default:

```bash
$ huginn -c '"Hello"'
```

Output:

```txt
"Hello"
```

Even with an explicit `print` function was used:

```bash
$ huginn -c 'print("Hello\n")'
```

Output:

```txt
Hello
none
```

Unless the last expression ended with a semicolon:

```bash
$ huginn -c 'print("Hello\n");'
```

Output:

```txt
Hello
```


=={{header|Icon}} and {{header|Unicon}}==

These examples work with posix shells.


```icon
echo "procedure main();write(\"hello\");end" | icont - -x
```



```unicon
echo "procedure main();write(\"hello world\");end" >hello.icn; unicon hello.icn -x
```



## J


```bash
$ jconsole -js "exit echo 'Hello'"
Hello
```


That said, note that J interpreters can themselves be thought of as [[wp:Command_shell|command shells]].


## Java

{{works with|bash}}
These three lines work with Bourne Shell (or compatible) or C Shell (or compatible), or bash on Unix/Linux/MacOSX/Windows+cygwin


```bash
$ echo 'public class X{public static void main(String[]args){' \
>     'System.out.println("Hello Java!");}}' >X.java
$ javac X.java && java X
```


A user can also enter this as one (very long) line:


```bash
$ echo 'public class X{public static void main(String[]args){System.out.println("Hello Java!");}}'>X.java;javac X.java&&java X
```


{{works with|MS-DOS}} Compatible Environments (such as [[wp:cmd.exe|cmd.exe]])
Works with cmd.exe on Windows (tested on Microsoft Windows XP [Version 5.1.2600])

```cmd>C:\
echo public class X{public static void main(String[] args){System.out.println("Hello Java!");}}>X.java&&javac X.java&&java X
Hello Java!
```



## JavaScript

{{works with|SpiderMonkey}}

```bash
$ js -e 'print("hello")'
hello
```



## jq


```sh
$ jq -M -n 1+1
2
```



## Julia


```julia
$ julia -e 'for x in ARGS; println(x); end' foo bar
foo
bar
```



## K

{{works with|Kona}}

```bash
$ k -e "\`0: \"hello\\n\""
```



## Kotlin

The following one-liner works with GNOME Terminal on Ubuntu 14.04:

```txt

echo 'fun main(args: Array<String>) = println("Hello Kotlin!")' >X.kt;kotlinc X.kt -include-runtime -d X.jar && java -jar X.jar

```

{{out}}

```txt

Hello Kotlin!

```



## Lasso


From stdin:


```Lasso
echo " 'The date and time is: ' + date " | lasso9 --
```


Or alternatively:


```Lasso
$ lasso9 -s " 'The date and time is: ' + date "
```



## Liberty BASIC


```lb

echo print "hello">oneLiner.bas & liberty -r oneLiner.bas echo print "hello">oneLiner.bas & liberty -r oneLiner.bas

```




## Lua


```bash
lua -e 'print "Hello World!"'
```



## Maple


```bash
maple -c'print(HELLO);' -cquit
```



## Mathematica


```Mathematica
echo Print[2+2] > file & math.exe -script file
```



## NetRexx

{{works with|UNIX Shell}}
Create a temporary file, execute the file via the NetRexx interpreter then delete the temporary file and any files generated via the translation.  (i.e. Java class files etc.)

```bash

$ TNRX=`mktemp T_XXXXXXXXXXXX` && test ! -e $TNRX.* && (echo 'say "Goodbye, World!"' >$TNRX; nrc -exec $TNRX; rm $TNRX $TNRX.*; unset TNRX)

```


'''Output:'''

```txt

NetRexx portable processor, version NetRexx 3.01, build 40-20120823-0156
Copyright (c) RexxLA, 2011,2012.  All rights reserved.
Parts Copyright (c) IBM Corporation, 1995,2008.
Program T_dO7RQs5HPElq
===== Exec: T_dO7RQs5HPElq =====
Goodbye, World!
Processing of 'T_dO7RQs5HPElq' complete

```



## NewLISP


```NewLISP
newlisp -e "\"Hello\"
->"Hello"
```



## Nim


```txt
echo 'for i in 0..10: echo "Hello World"[0..i]' | nim i
>>> H
He
Hel
Hell
Hello
Hello
Hello W
Hello Wo
Hello Wor
Hello Worl
Hello World
>>>
```



## Objeck

{{works with|UNIX Shell}}

```bash
./obc -run '"Hello"->PrintLine();' -dest hello.obe ; ./obr hello.obe
```



## OCaml


```bash
$ ocaml <(echo 'print_endline "Hello"')
Hello
```

{{works with|OCaml|4.00+}}

```bash
$ echo 'print_endline "Hello"' | ocaml -stdin
Hello
```



## Octave


```matlab
$ octave --eval 'printf("Hello World, it is %s!\n",datestr(now));'
Hello World, it is 28-Aug-2013 17:53:47!
```




## Oforth



```Oforth
oforth --P"1000 seq map(#sqrt) sum print"
```


{{out}}

```txt

21097.4558874807

```



## ooRexx


```bash

rexx -e "say 'Goodbye, world.'"

```



## Oz

This is difficult to do in Oz because the compiler/interpreter always wants the source code in a file and does not read from stdin. We can do somethings like this on Unix-like systems:

```bash>echo
tmp.oz "{System.show hello}"; ozc -l System -e tmp.oz
hello
```


With <code>-l System</code> we make the System module available so that we can print something.


## PARI/GP


```bash
echo "print(Pi)" | gp -q
```



## Pascal

''See [[#Free Pascal|Free Pascal]]''


## Perl


```bash
$ perl -e 'print "Hello\n"'
Hello
```


More information about the many ways of invoking perl can be found in [http://perldoc.perl.org/perlrun.html perlrun].


## Perl 6

{{works with|Rakudo|#22 "Thousand Oaks"}}


```bash
$ perl6 -e 'say "Hello, world!"'
Hello, world!
```



## Phix

Command line option -e added for 0.8.1. Outer quotes only rqd if snippet contains spaces, otherwise ignored.

Most one-liners will probably start with '?' since eg "1+2" gives a compilation error.

```txt

C:\Program Files (x86)\Phix>p -e ?357+452
809
C:\Program Files (x86)\Phix>p -e "?357+452"
809

```



## PHP

assuming you have the PHP CLI (command-line interface) installed, not just the web server plugin

```bash
$ php -r 'echo "Hello\n";'
Hello
```



## PicoLisp


```PicoLisp
$ picolisp -'prinl "Hello world!"' -bye
Hello world!
```



## Pike


```bash
$ pike -e 'write("Hello\n");'
Hello
```



## PowerShell


```cmd>
 powershell -Command "Write-Host 'Hello'"
Hello
```


## Prolog

===Command-Line Options===

```prolog
$ swipl -g "writeln('hello world')." -t 'halt.'
hello world
$
```


```prolog
$ gprolog --init-goal "write('goodbye'),nl,halt"
goodbye
$
```


```prolog
$ yap -q -g "current_prolog_flag(dialect, D), writeln(D), halt"
yap
```


###  <<<


```prolog
$ swipl -q <<< "current_prolog_flag(dialect,D), writeln(D), halt."
swi

$ yap -q <<< "current_prolog_flag(dialect,D), writeln(D), halt."
yap
```


###  Pipe


```prolog
$ echo "current_prolog_flag(dialect,D), writeln(D), halt." | swipl -q
swi

$ echo "current_prolog_flag(dialect,D), writeln(D), halt." | yap -q
yap
```



## PureBasic

Runs on Linux with(thanks to) bash. Path variables must be set as decribed in INSTALL.

```bash
$ echo 'messagerequester("Greetings","hello")' > "dib.pb" && ./pbcompiler dib.pb -e "dib" && ./dib
```



## Python

===Prints "Hello"===

```bash
$ python -c 'print "Hello"'
Hello
```



### Web server with CGI

The python CGIHTTPServer module is also an [[Executable library|executable library]] that performs as a web server with CGI. to start enter:

```bash
python -m CGIHTTPServer
```

It returns with:

```txt
Serving HTTP on 0.0.0.0 port 8000 ...
```



## R


```bash
$ echo 'cat("Hello\n")' | R --slave
Hello
```


Alternatively, using the Rscript front-end,

```bash
$ Rscript -e 'cat("Hello\n")'
Hello
```



## Racket


```bash
$ racket -e "(displayln \"Hello World\")"
Hello World
```



## REBOL


```bash
rebview -vswq --do "print {Hello!} quit"
```


Output:

```txt
Hello!
```



## Retro


```Retro
echo '\'hello s:put nl bye' | retro
```



## REXX

Note:   Regina REXX is the only version of REXX that supports this type of behavior   (taking it's input from a console stream).

```rexx

        ╔══════════════════════════════════════════════╗
        ║                                              ║
        ║  from the MS Window command line  (cmd.exe)  ║
        ║                                              ║
        ╚══════════════════════════════════════════════╝


echo   do j=10  by 20  for 4;   say right('hello',j); end   |   regina
```

'''output'''   when entering the (above) from the (DOS) command line:

```txt

     hello
                         hello
                                             hello
                                                                 hello

```



## Ring


```ring

see "Hello World!" + nl

```

Output:

```txt

Hello World!

```



## Ruby

From [[Unix]]:

```bash
$ ruby -e 'puts "Hello"'
Hello
```


{{works with|JRuby}}

```bash
$ jruby -e 'puts "Hello from JRuby"'
Hello from JRuby
```


{{works with|Rubinius}}

```bash
$ rbx -e 'puts "Hello from Rubinius"'
Hello from Rubinius
```



## Run BASIC


```bash
print shell$("echo hello world")
```



## Rust

The following code leaves the file <tt>rust_out</tt> in the current directory (it does not
delete it to avoid to call another shell/system dependent command/program). The
''current directory'' is not specified by <tt>./</tt> in every system...

```bash
$ echo 'fn main(){println!("Hello!")}' | rustc -;./rust_out
```


=={{header|S-lang}}==
<lang S-lang>slsh -e 'print("Hello, World")'
```


Or, in MSW cmd.exe:

<lang S-lang>slsh -e "print(\"Hello, World\")"
```


Note that print() is included w/slsh, but is not part of S-Lang itself.


## Scala

{{libheader|Scala}}

```cmd>C:\
scala -e "println(\"Hello\")"
Hello
```


```cmd

PS C:\> scala -e 'println(\"Hello\")'
Hello
```


The escaping of quotes is required by Windows.
On Unix and shown in the example on [[PowerShell|Windows PowerShell]],
one could just use single quotes around the code.


## Scheme

{{works with|Guile}}

```scheme
guile -c '(display "Hello, world!\n")'
```



## Shiny


```shiny
shiny -e "say 'hi'"
```



## Sidef


```ruby
% sidef -E "say 'hello'"
```



## Slate


```slate
./slate --eval "[inform: 'hello'] ensure: [exit: 0].".
```



## SNOBOL4

Portable version

```snobol
echo 'a output = "Hello, World!";end' | snobol4 -b
```


Bash version

```snobol
snobol4 -b <<<'a output = "Hello, World!";end'
```



## Tcl

This is an area where Tcl is lacking, though when shell one-liners are required a construct like this is typically used:

```bash
$ echo 'puts Hello' | tclsh
Hello
```



## TXR



```bash
$ echo 123-456-7890 | txr -c '@a-@b-@c' -
a="123"
b="456"
c="7890"

```


Most useful txr queries consist of multiple lines, and the line structure is important. Multi-liners can be passed via <code>-c</code> easily, but there is no provision in the syntax that would allow multi-liners to be actually written as one physical line. There are opposite provisions for splitting long logical lines into multiple physical lines.

The <code>-e</code> (evaluate) and <code>-p</code> (evaluate and print) options provide shell one-liner access to
TXR Lisp:


```bash
$ txr -p '(+ 2 2)'
4
```



```bash
$ txr -e '(mkdir "foo" #o777)'
$ ls -ld foo
drwxrwxr-x 2 kaz kaz 4096 Mar  4 23:36 foo
```



## UNIX Shell

Explicit call of the shell, passing the shell command via the <code>-c</code> option:

```bash
$ sh -c ls
```


```bash
$ sh -c "echo hello"
```


To invoke a specific shell like [[Bash]], [[Korn Shell]] or [[Z Shell]]:


```bash
$ bash -c 'paste <(echo 1) <(echo 2)'
$ ksh -c 'let i=3+4; print $i'
$ zsh -c 'if [[ 5 -lt 6 ]] { echo ok };'
```


Shell scripts almost never use <code>sh -c</code>, because there are various implicit ways whereby the shell command language evaluates a command in a subshell:


```bash
$ VAR=`echo hello`   # obsolescent backtick notation
$ VAR=$(echo hello)  # modern POSIX notation
$ (echo hello)       # execute in another shell process, not in this one
```


There are more details about <code>`echo hello`</code> and <code>$(echo hello)</code> at [[Execute a system command#UNIX Shell]].

=
## C Shell
=
Run a C shell command from any shell:


```bash
$ csh -fc 'if (5 < 6) echo ok'
```


=
## es
=
Run a command, in extensible shell, from any shell:


```bash
$ es -c 'if {test 5 -lt 6} {echo ok}'
```



## Ursala

The command to execute the Ursala compiler is fun. An expression supplied as a parameter to the --main option is compiled and evaluated. If the expression evaluates to a list of character strings, it can be displayed on standard output with --show. If it's some other type, it can be formatted for display by --cast <type expression>,


```bash
$ fun --main=-[hello]- --show
hello
$ fun --main="power/2 32" --cast %n
4294967296
$ fun --m="..mp2str mpfr..pi 120" --c %s
'3.1415926535897932384626433832795028847E+00'
```



## Vedit macro language

The following DOS command starts Vedit and displays a message.
When the user presses any key, Vedit exits.

```cmd
vpw -c'Get_Key("Hello!") exit'
```



## Wart


```bash
echo "prn 34" |wart
```



## zkl

With a unix like shell, just pipe the program into the REPL. Kinda greasy and noisy. To shut it up, send stdout to /dev/null

```bash
echo 'println("Hello World ",5+6)' | zkl
```

{{out}}

```txt

zkl 1.12.9, released 2014-05-01
Hello World 11
Hello World 11

```


{{omit from|AutoHotkey|No interactive shell}}
{{Omit From|Java}}
{{omit from|Maxima}}
{{omit from|Modula-3}}
{{omit from|SAS}}
{{omit from|Stata}}
{{omit from|TI-83 BASIC}}
{{omit from|TI-89 BASIC}} <!-- Does not have an external OS/command processor. -->

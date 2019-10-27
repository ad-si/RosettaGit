+++
title = "Multiline shebang"
description = ""
date = 2019-06-27T03:19:25Z
aliases = []
[extra]
id = 10246
[taxonomies]
categories = []
tags = []
+++

{{draft task|Basic language learning}}
Simple shebangs can help with scripting, e.g., <code>#!/usr/bin/env python</code> at the top of a Python script will allow it to be run in a terminal as "<code>./script.py</code>".

Occasionally, a more complex shebang line is needed. For example, some languages do not include the program name in ARGV; a multiline shebang can reorder the arguments so that the program name is included in ARGV.

The syntax for a multiline shebang is complicated. The shebang lines must be simultaneously commented away from the main language and revealed to some shell (perhaps [[Bash]]) so that they can be executed. In other words, [http://en.wikipedia.org/wiki/Polyglot_(computing) Polyglots].

Warning: Using a multiline shebang of the form <code>#!/bin/sh ... exec ... !#</code> will set the code's mimetype to <code>text/x-shellscript</code>, which creates problems such as Emacs treating the file as a shell script, no matter which language and file extension it really uses.

;See also
* [[Native shebang]] - where the "program loaded" is ''of'' the actual native task language.


## Ada


{{trans|C}}


```Ada
#!/bin/bash
sed -n -e '7,$p' < "$0" > mulshbang.adb
gnatmake -q mulshbang
./mulshbang $*
rm mulshbang*
exit 
with Ada.Text_IO, Ada.Command_Line; -- first line of Ada program

procedure Mulshbang is
  use Ada.Text_IO;
begin
  Put_Line("Name: " & Ada.Command_Line.Command_Name);
  for I in 1 .. Ada.Command_Line.Argument_Count loop
    Put_Line("  Arg" & Integer'Image(I) & ": " &
             Ada.Command_Line.Argument(I));
  end loop;
end Mulshbang;
```


{{out}}


```txt
>./adamulshbang
Name: ./mulshbang
>./adamulshbang one two three
Name: ./mulshbang
  Arg 1: one
  Arg 2: two
  Arg 3: three
```



## C


```txt
#!/bin/bash
sed -n -e '7,$p' < "$0" | /usr/bin/gcc -x c -o "$0.$$.out" -
$0.$$.out "$0" "$@"
STATUS=$?
rm $0.$$.out
exit $STATUS
#include <stdio.h>

int main(int argc, char **argv)
{
  int i;
  for (i = 0; i < argc; i++)
    printf("argv[%d] -> %s\n", i, argv[i]);
  return 0;
}
```


Test runs:


```txt
$ ./cmulshbang.c
argv[0] -> ./cmulshbang.c.4062.out
argv[1] -> ./cmulshbang.c
$ ./cmulshbang.c 1
argv[0] -> ./cmulshbang.c.4071.out
argv[1] -> ./cmulshbang.c
argv[2] -> 1
$ ./cmulshbang.c 1 2
argv[0] -> ./cmulshbang.c.4080.out
argv[1] -> ./cmulshbang.c
argv[2] -> 1
argv[3] -> 2
```


'''Student exercise:''' use a stable filename for the executable, e.g. <code>"$0.out"</code>.  Do not remove it, and only recompile it if the script's timestamp is newer than that of the executable.


## Clojure

The namespace = basename = filename minus the extension must be passed as a value to Clojure's -m flag.


```clojure
":";exec clj -m `basename $0 .clj` $0 ${1+"$@"}
```


Alternate shebang, using the [https://github.com/kumarshantanu/lein-exec Leiningen 'exec' plugin]:


```clojure
":";exec lein exec $0 ${1+"$@"}
```



## Common Lisp


{{works with|CLISP}}

Here, the script name is passed once to CLISP and once to ext:*args*, which normally omits it.


```lisp
#!/bin/sh
#|
exec clisp -q -q $0 $0 ${1+"$@"}
|#
```



## E


E uses only “#” for line comments, like the shell, so there is no straightforward answer. We can abuse the fact that “>” is also a line comment to achieve this effect. Note that a “>” line comment should ordinarily only occur as part of Updoc (test/documentation) text, so this is not good practice.

In this example, we are including the command name itself in the argument list, which would ordinarily not include it.


```e
#!/bin/sh
>/dev/null; exec rune $0 $0 ${1+"$@"}

println(`I was called as ${interp.getArgs()[0]}.`)
```



## Emacs Lisp


```lisp
:;exec emacs -batch -l $0 -f main $*
```



## Erlang


hello.erl


```Erlang
#!/usr/bin/env escript

-module(hello).
-export([main/1]).

main(_) -> io:format("Hello World!~n", []).
```


This works fine when the module is run by itself with dot slash:


```sh
$ ./hello.erl 
Hello World!
```


But when another Erlang module tries to import the code, or you try to compile manually in erl, you get a syntax error.


```sh
$ erl
Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.8.4  (abort with ^G)
1> c(hello).
./hello.erl:1: syntax error before: '#'
./hello.erl:4: no module definition
error
```


=={{header|F Sharp|F#}}==

F# scripts may be run with dot-slash notation using the following multiline shebang:


```f#
#light (*
	exec fsharpi --exec "$0" --quiet
*)

let main = printfn "Hello World"
```


However, if a script has any dependencies that need to be compiled in, the fsharpi interpreter will not understand how to import them. This means dot-slashing is no longer viable, and the script must be compiled in order to run properly. The shebang can stay, but it is best to remove it, to make clear to users that the script should not be dot-slashed.


## Factor

Factor no longer requires a space after <code>#!</code> as of v0.95.


```factor
#!/usr/bin/env factor -script
```


=={{header|Forth|Gforth}}==
We can use Gforth's (non-ANS standard) support for shebangs and the '#' number prefix to make Gforth skip over the shebang without interfering with shell script interpretation.


```forth
#! /bin/sh
#0 [IF] \ lines below read by shell but ignored by Gforth
   exec gforth \
   -m 256M \
   -d 16M \
   "$0" "$@"
[THEN]
.( hello world) CR BYE

```



## Groovy


```groovy
#!/bin/bash
script_dir="$(cd $(dirname $0) >/dev/null; pwd -P)"

if [ -z "${GROOVY_HOME}" ]
then
  echo 'GROOVY_HOME must be defined.' >&2
  exit 1
fi

CLASSPATH="${script_dir}" "${GROOVY_HOME}/bin/groovy" -e "$(sed -e '1,/^!#$/d' $0)" "${@:1}"
exit
!#
println 'aoeu'

```



## Go


```go
#!/bin/bash
sed -n -e '12,$p' < "$0" > ttmmpp.go
go build ttmmpp.go
rm ttmmpp.go
binfile="${0%.*}"
mv ttmmpp $binfile
$binfile "$@"
STATUS=$?
rm $binfile
exit $STATUS
######## Go Code start on line 12
package main
import (
  "fmt"
  "os"
)
 
func main() {
  for i, x := range os.Args {
    if i == 0 {
      fmt.Printf("This program is named %s.\n", x)
    } else {
      fmt.Printf("the argument #%d is %s\n", i, x)
    }
  }
}

```



## Haskell


{{trans|C}}


```Haskell
#!/bin/bash
sed -n -e '7,$p' < "$0" > $0.$$.hs
ghc $0.$$.hs > /dev/null
./$0.$$ "$0" "$@"
rm $0.$$*
exit
import Text.Printf
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ (uncurry $ printf "argv[%d] -> %s\n") . zip ([0..] :: [Int])
```


{{out}}


```txt
$ ./multibang.hs
argv[0] -> ./multibang.hs
$ ./multibang.hs Goodbye, World!
argv[0] -> ./multibang.hs
argv[1] -> Goodbye,
argv[2] -> World!
```


Or you can 'cheat' by ignoring Bash's complaints about Haskell comments (gives the exact same output as above):


```Haskell
#!/bin/bash
{- 2> /dev/null
exec runghc $0 $0 $@
-}
import Text.Printf
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ (uncurry $ printf "argv[%d] -> %s\n") . zip ([0..] :: [Int])
```



## J


Assuming this task is asking for a mix of unix shell commands and J, and also that the J binary directory is listed in <code>$PATH</code>


```J
#!/bin/sh
# 0 :0
  echo unix shell commands go here
  echo presumably this will condition the environment
  echo for example:
  cd working-directory
  echo or maybe you want to modify $PATH, ... whatever... 
  echo then start up J:
  exec jconsole "$0" "$@"
)

NB. exit on error
onfail_z_=:3 :0
  1!:2&2 ARGV
  1!:2&2]13!:12'' NB. display error message
  2!:55>:13!:11'' NB. exit with 1 origin error number
)
9!:27 'onfail 1'
9!:29]1

NB. and then the rest of the file is J
echo 'hi!'
echo 'your command line arguments were:'
echo ARGV
echo p:i. 3 4
exit 0

```


'''Notes:'''

The <code>#!/bin/sh</code> line is interpreted by J as a verb train with no arguments - in other words, it is ignored.

The <code># 0 :0</code> line is interpreted by shell as a comment and by J as the beginning of a multiline "hereis" script which basically ignores everything up to the lone right parenthesis.

So then it's just regular shell script up until the line where we turn control over to J. On that line, we use <code>exec</code> (so that the shell process does not hang around, waiting for J to finish - J takes over the current process). And we pass any shell script command line arguments on to J.

On the J side of the fence, we presumably want this code to behave like a normal unix module, so we need to override J's default behavior (which is to provide the J command line). <code>9!:29]1[9!:27'2!:55]1</code> is a bit of magic that accomplishes that: it stacks a command to exit with exit code 1 to be executed when we reach the command line. So any errors will terminate the program.

Next, we run the system J profile so that we have all of the standard stuff that that provides. (Or leave this out if that's what you want.)

Finally we do some J stuff and then exit. If everything goes right, the command line exit we stacked earlier just gets ignored.

Here's a variant where the shell script tests J's exit code and does something different based on success or failure.


```J
#!/bin/sh
# 0 :0
  echo unix shell commands go here
  echo presumably this will condition the environment
  echo for example:
  cd working-directory
  echo or maybe you want to modify $PATH, ... whatever... 
  echo then start up J:
  if jconsole -jprofile "$0" "$@"; then
    echo success
  else
    echo failure
  fi
  exit $?
)

9!:29]1[9!:27'2!:55]1' NB. exit on error
(3 :'0!:0 y')<BINPATH,'/profile.ijs'

NB. and then the rest of the file is J
echo 'hi!'
echo 'your command line arguments were:'
echo ARGV
echo p:i. 3 4
exit 0

```


The <code>exit $?</code> line tells the shell interpreter to ignore the J part of the file, and the <code>$?</code> reuses J's exit code as the exit code from the shell instance.

Note that we've left off the onfail handler within J, and just used a minimal definition to give us a non-zero exit code for the error case. Mostly, the assumption here would be that the error message would not be interesting, and that any failure should be handled by a retry. But you could replace the exit on error line here with the full definition and <code>9!:</code> preparatory bit from the previous example and you could also of course change the <code>1!:2&2</code> lines (<code>1!:2&2</code> is the "low-level" write to stdout mechanism for J - and, yes, those numbers are part of the definition of the language - or at least the "Foreigns" part of the language - note that ultimately all computer languages resolve to things which can be thought of as numbers or sequences of numbers, though some people will vigorously assert other things).


## Julia


```julia

#!/bin/sh
#=
  echo Julia will ignore as commented all text between #= and =#
  echo which allows us to place arbitrary unix shell code here
  echo perhaps to change environment settings for Julia or
  echo set the directory prior to starting the Julia program.
  echo for example:
  cd /user/meeting/working
  echo then start the Julia program
  exec julia "$0" "$@"
#  comments ignored by Julia end here --> =#

function countto(n)
    i = zero(n)
    println("Counting to $n...")
    while i < n
        i += 1
    end
    println("Done!")
end

@time countto(10^10)

```




## MATLAB


Unlike Octave, MATLAB has no built-in support for shebangs. In fact, several tricks are required to even approximate a shebang, due to the byzantine way that MATLAB structures scripts and function files.

~/bin/shmatlab%:


```bash
#!/bin/sh
matlab -nojvm -nodisplay -nosplash -r "varargin = regexp('${1+"$@"}', ' ', 'split'); nvarargin = length(varargin); run('$1'); exit" | tail -n +16

```


args.m:


```matlab
'shmatlab'% $0 ${1+"$@"}
'exit';

for i = 1:nvarargin
  disp(varargin{i});
end
```


Example:


```txt

$ ./args.m a b c
./args.m
a
b
c

```



## OCaml


ocamlc hates shebangs, so much trickery is needed. The number of underscores in the dummy kkkk identifier corresponds to the number of bash strings in the shebang. Thus, core library .cma files can be specified this way in interpreted mode, though accessing other OCaml scripts requires compiling them first, and referencing the .cmo's here.


```ocaml
if true then ignore begin let kkkk _ _ _ _ = 0 in kkkk
"exec" "ocaml" "$0" "$@" + let fi = 0 and exit _ _ = 0 in if false
then exit
fi
true else 0
end;;

let main = print_endline "Hello World!"
```


Example:


```txt

$ head -n 2 she.ml
if true then ignore begin let kkkk _ _ _ _ _ _ = 0 in kkkk
"exec" "ocaml" "$0" "unix.cma" "graphics.cma" "$@" + let fi = 0 and exit _ _ = 0 in if false
$ ocaml she.ml
Hello World!
$ /bin/bash she.ml
Hello World!
$ ocamlc -o she.byte she.ml
$ ./she.byte
Hello World!
$ ocamlopt -o she.opt she.ml
$ ./she.opt
Hello World!

```



## PARI/GP

The PARI equivalent to a multiline shebang is a collection of <code>GP;</code> lines:

```C
/*
GP;install("C_function_name","G","GP_name","./filename.gp.so");
GP;addhelp(GP_name, "GP_name(n): Computes the foo of bar(n).");
*/
```

These commands are passed to GP when invoked by gp2c.


## Perl

From <code>perldoc perlrun</code>, the following is supposed to find perl one way or another under sh, csh or perl.

```perl
#!/usr/bin/perl
eval '(exit $?0)' && eval 'exec perl -wS $0 ${1+"$@"}'
& eval 'exec /usr/bin/perl -wS $0 $argv:q'
	if $running_under_some_shell;
```


## Perl 6



```perl6
#!/usr/local/bin/perl6
use MONKEY; EVAL '(exit $?0)' && EVAL 'exec perl6 $0 ${1+"$@"}'
& EVAL 'exec perl6 $0 $argv:q'
        if 0;
```



## PicoLisp

We can use a multi-line comment #{ ... }# to hide the shell commands from Lisp. The opening #{ in turn is a coment for the shell.

```PicoLisp
#!/bin/bash
#{
exec pil $0 foo bar
# }#

# Lisp code
(println (cadr (file)) (opt) (opt))
(bye)
```

{{out}}

```txt
$ ./myScript
"myScript" "foo" "bar"
```



## Pike

we use a multiline comment to hide the shell command from pike, and we can use a preprocessor directive to hide the comment begin from the shell.

```Pike
#!/bin/bash 
#define foo foo /*
exec pike $0 hello world 
*/

int main(int argc, array argv)
{
   write("%O\n", argv);
}
```


{{out}}
 ({ /* 3 elements */
    "/local/users/mbaehr/src/pike/multiline-shebang/multiline-shebang.pike",
    "hello",
    "world"
 })


## PostgreSQL


This style of shebang would also work with other languages that use double dashes for comments, though most of them (Lua, Haskell) already support traditional #!... shebangs.


```postgresql
--() { :; }; exec psql -f "$0"

SELECT 'Hello World!';
```



## Python

We can use multiple strings to make the shell commands do nothing from Python (actually they become the module docstring.).

```Python
#!/bin/bash
"exec" "python" "$0"

print "Hello World"
```

{{out}}

```txt
$ ./myScript
Hello World
```


Control structures (if/for/etc.) can't be quoted, 
but one can use the following to embed any script:

```Python
#!/bin/sh
"true" '''\'
if [ -L $0 ]; then
...
exec "$interpreter" "$@"
exit 127
'''

__doc__ = """module docstring"""

print "Hello World"
```


Here we use a) the code <nowiki>'''\'</nowiki> translates to \ in shell, but opens a multi-line string in Python; b) the true command ignores its argument, c) we always exit before the ending <nowiki>'''</nowiki> so that the shell interpreter never reads it. Also, remember to set any docstrings by assigning to __doc__ since the docstring is already used for the shell script.


## Racket


```Racket

#!/bin/sh
#| -*- scheme -*-
# this is sh code
echo running "$0", passing it into itself as an argument
exec racket -tm "$0" "$0"
|#

#lang racket

(provide main)
(define (main arg)
  (printf "argument: ~a\nexecuted as: ~a\n"
          arg (find-system-path 'exec-file)))

```



## Ruby

One can use a single-line shebang, like <code>#!/usr/bin/env ruby</code>, 
and use Kernel#system or `backquotes` to run any extra shell commands. 
A multi-line shebang is possible, but not necessary.

This script works both ways: either <code>/bin/sh script.rb</code> or <code>ruby script.rb</code> would run multiple lines of shell commands, and then start Ruby.


```ruby
#!/bin/sh

# Insert shell code here!
printf '%s\n' "Shell running $0"
i=1
for arg do
  printf '  %s\n' "\${$i}: $arg"
  i=`expr $i + 1`
done

# Switch from shell to Ruby.
exec ${RUBY-ruby} -x "$0" --coming-from-sh "$@"

#!ruby

ARGV[0] == "--coming-from-sh" or exec "/bin/sh", $0, *ARGV
ARGV.shift

# Insert Ruby code here!
puts "Ruby running #$0"
ARGV.each_with_index do |arg, i|
  puts "  ARGV[#{i}]: #{arg}"
end
```


When running <code>/bin/sh scratch.rb</code>, the shell:

# ignores <code>#!/bin/sh</code>, because it is a comment.
# runs multiple lines of shell code.
# executes <code>ruby -x</code>; user can set RUBY environment variable to pick different Ruby, like RUBY=ruby19 or RUBY=jruby.

<code>ruby -x</code> skips every line until the first Ruby shebang. This line must start with "#!" and must contain "ruby". (So "#!ruby" is the shortest shebang to work.)

When running <code>ruby scratch.rb</code> (without -x option), Ruby notices that the first line "#!/bin/sh" is a foreign shebang.

* Ruby 1.8 then interprets this shebang and executes /bin/sh.
* Ruby 1.9 then assumes -x option and skips to the first Ruby shebang. The script is not <code>--coming-from-sh</code>, so it executes /bin/sh.


## Scala


The [http://www.scala-lang.org/files/archive/nightly/docs-master/manual/html/scala.html scala(1)] interpreter parses a header section. 
The scalac compiler does not.


```scala

#!/bin/bash
FOO=bar
scala $0 $@
exit
!#
def fact(n : Int) : Int = {
  var i = n ;
  var a = 1 ;
  while (i > 0) {
    a = a*i ;
    i -= 1 ;
  }
  return a ;
}

println("fact(5) = " + fact(5));
```



## Scheme

{{works with|Chicken Scheme}}


```scheme
#!/usr/bin/env csi -ss
```



## Sidef


```ruby
#!/bin/sh

#`(if running under some shell) {
    eval 'exec /usr/bin/sidef $0 ${1+"$@"} "world"'
}

say "Hello, #{ARGV[0]}!"
```


{{out}}

```txt

$ ./script.sf
Hello, world!

$ ./script.sf Sidef
Hello, Sidef!

$ sidef script.sf RosettaCode
Hello, RosettaCode!

```



## Smalltalk


```smalltalk
"exec" "gst" "-f" "$0" "$0" "$@"
```



## SQL PL

{{works with|Db2 LUW}}
Based on the Postgres shebang, it works the same with IBM Db2. 


```sql pl

--() { :; }; exec db2 -txf "$0"

get instance;
connect to sample;
select 'Hello' from sysibm.sysdummy1;
values current date;

```

Output:

```txt

$ ./myScript 

 The current database manager instance is:  db2inst1



   Database Connection Information

 Database server        = DB2/LINUXX8664 11.1.1
 SQL authorization ID   = DB2INST1
 Local database alias   = SAMPLE


Hello

04/22/2018

```


The db2profile should be loaded before executing the 'db2' command (. ~db2inst1/sqllib/db2profile).

The options used in the example are: t - delimited by semi colon, x - Suppress printing of column headings, f - Read from input file. For other options, you can execute 'db2 ? options', and change the shebang.


## Tcl

It is normal to use a line like this:

```tcl
#!/usr/bin/env tclsh
```

But in cases where that is not enough  perhaps because it needs some logic to locate the Tcl interpreter to use  the differences in the way Tcl and the Bourne shell interpret end-of-line backslashes in comments can be used:

```tcl
#!/bin/sh
# Next line is comment in Tcl, but not in sh... \
exec tclsh "$0" ${1+"$@"}
```

Additional complexity can be added so long as the lines for the shell are commented in a Tcl sense.


## TXR



```txt
#!/bin/sh
sed -n -e '4,$p' < "$0" | /usr/bin/txr -B - "$0" "$@"
exit $?
@(next :args)
@(collect)
@arg
@(end)
```


Test run:

```txt
$ ./multilineshebang.txr
arg[0]="./multilineshebang.txr"
$ ./multilineshebang.txr 1
arg[0]="./multilineshebang.txr"
arg[1]="1"
$ ./multilineshebang.txr 1 2 3
arg[0]="./multilineshebang.txr"
arg[1]="1"
arg[2]="2"
arg[3]="3"
$
```



## zkl

zkl has a variant of the here doc that means ignore the doc, as in "#if 0" but more so. But that doesn't mean a shell has to ignore it.

File foo.zkl (the .zkl extension is needed):

```zkl
#!/bin/sh
#<<<#
echo "A shell script in a zkl program ($0)"
echo "Now run zkl <this file> with Hello World as args"
zkl $0 Hello World!
exit
#<<<#
println("The shell script says ",vm.arglist.concat(" "));
```

{{out}}

```txt

$ ./foo.zkl 
A shell script in a zkl program (./foo.zkl)
Now run zkl <this file> with Hello World as args
The shell script says Hello World!
$

```


{{omit from|BASIC}}
{{omit from|BBC BASIC}}
{{omit from|Blast}}
{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Locomotive Basic}}
{{omit from|Nemerle}}
{{omit from|Openscad}}
{{omit from|Z80A}}
{{omit from|ZX Spectrum Basic}}

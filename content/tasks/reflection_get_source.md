+++
title = "Reflection/Get source"
description = ""
date = 2019-03-30T04:53:00Z
aliases = []
[extra]
id = 21019
[taxonomies]
categories = ["task", "Reflection"]
tags = []
+++

## Task

The goal is to get the source code or file path and line number where a programming object (e.g. module, class, function, method) is defined.





## Clojure



```clojure

; Use source function for source code.
(source println)

; Use meta function for filenames and line numbers (and other metadata)
(meta #'println)
```



## Factor

Printing definitions:

```factor
USE: see
\ integer see ! class
nl
\ dip see     ! word
```

```txt

IN: math
UNION: integer fixnum bignum ;

IN: kernel
: dip ( x quot -- x ) swap [ call ] dip ;

```

Obtaining the code that makes up a word as a quotation (an anonymous function/collection that stores code):

```factor
USE: accessors
\ partition def>> .
```

```txt

[ over [ 2selector [ each ] 2dip ] dip [ like ] curry bi@ ]

```

Obtaining the vocabulary name a word belongs to:

```factor
USE: accessors
\ dip vocabulary>> print
```

```txt

kernel

```

Obtaining file paths for a particular vocabulary:

```factor
USE: vocabs.files
"sequences" vocab-files .
```

```txt

{
    "resource:core/sequences/sequences.factor"
    "resource:core/sequences/sequences-docs.factor"
    "resource:core/sequences/sequences-tests.factor"
}

```

Obtaining the path and line number where a word is defined:

```factor
"loc" \ dip props>> at
```

```txt

{ "resource:core/kernel/kernel.factor" 111 }

```



## FreeBASIC

FreeBASIC is a fully compiled language which does not support reflection as such.

Nevertheless, the language does have a number of built-in macros which can be used to map certain entities back to the source code file.
This is mainly useful for debugging purposes. Here's a simple example :


```freebasic
' FB 1.05.0 Win64 (getsource.bas)

Sub Proc()
  Print __Function__  & " is defined in " & __Path__ & "\" & __File__ & " at line " & ( __line__ - 1) 
End Sub 

Proc()
Sleep
```


```txt

PROC is defined in c:\FreeBasic\getsource.bas at line 3

```



## Go

It is possible to get the file name/path and line number of a given function in Go as follows.

```go
package main

import (
    "fmt"
    "path"
    "reflect"
    "runtime"
)

func someFunc() {
    fmt.Println("someFunc called")
}

func main() {
    pc := reflect.ValueOf(someFunc).Pointer()
    f := runtime.FuncForPC(pc)
    name := f.Name()
    file, line := f.FileLine(pc)
    fmt.Println("Name of function :", name)
    fmt.Println("Name of file     :", path.Base(file))
    fmt.Println("Line number      :", line)
}
```


```txt

Name of function : main.someFunc
Name of file     : reflection_get_source.go
Line number      : 10

```



## J


Source code which when executed will recreate the definition can be obtained using <code>5!:5 <'name'</code> where ''name'' is the name of the thing you want source code for. Or, you can use 5!:6 which will provide a "fully parenthesized" variant for the tacit part of any definition.

You can also use 4!:4 and 4!:3 to find the file containing the name's definition (if there is one). Line number is not tracked.

Examples:


```J
   mean=:+/ %#
   5!:5 <'mean'
+/ % #
   5!:6 <'mean'
(+/) % #
   4!:4 <'mean'
_1
   4!:4 <'names'
2
   2 { 4!:3 ''
┌────────────────────────────────────────────┐
│/Applications/j64-804/system/main/stdlib.ijs│
└────────────────────────────────────────────┘
```


We could also provide convenience functions for these mechanisms:


```J
   linrep=: 5!:5@<
   srcfile=: (4!:4@<) { a:,~ 4!:3 bind ''
```


Example use:


```J
   linrep 'names'
list_z_@nl
   srcfile 'names'
┌────────────────────────────────────────────┐
│/Applications/j64-804/system/main/stdlib.ijs│
└────────────────────────────────────────────┘
   srcfile 'mean'
┌┐
││
└┘
```


Note that these mechanisms can be disabled (using [http://www.jsoftware.com/help/dictionary/dx003.htm 3!:6]).


## JavaScript

<code>Function.toString()</code> will return the source code for user-defined functions.


```javascript
function foo() {...}
foo.toString();
// "function foo() {...}"

```


For native functions, the function body typically will be a syntactically invalid string indicating the function is native. This behavior isn't part of any ECMAScript standard, but is common practice.

```javascript
Math.sqrt.toString();
// "function sqrt() { [native code] }"

```



## Julia

```julia
# Definition
function foo() end

@which foo() # where foo is defined
@less foo() # first file where foo is defined
```



## Kotlin

It's possible to do this (in a fashion) in Kotlin JS by using inline JavaScript and applying toString() to the function name to get its source code in a similar way to the JavaScript entry above. However, there are a couple of things to note:

1. Kotlin JS transpiles to JavaScript and it will therefore be the JS code for the function which will be printed. To my knowledge, there is no way to recover the original Kotlin code.

2. In the example below the ''hello'' function will actually be referred to as ''_.hello'' in the generated JavaScript from within the main() function.
 

```scala
// Kotlin JS Version 1.2.31

fun hello() {
    println("Hello")
}

fun main(args: Array<String>) {
    val code = js("_.hello.toString()")
    println(code)
}

```


```txt

function hello() {  
    println('Hello');
}

```



## Lingo

Lingo does not allow to identify specific line numbers (automatically), but you can get
the full source code of the script which defines either a class or a global function.
Class scripts (called "parent scripts") only define a single class, so no additional parsing needed.
Global functions are defined in movie scripts, there can be any number of such movie scripts,
but in each movie script all function names must be unique. So it's not too hard to manually
find the line number for a specific function in the returned code (e.g. using a RegExp).

```lingo
----------------------------------------
-- Returns source code either for a class (parent script) or a class instance (object)
-- @param {script|instance} class
-- @return {string}
----------------------------------------
on getClassCode (class)
  if ilk(class)=#instance then class=class.script
  return class.text
end

----------------------------------------
-- Returns the source code of the movie script that defines the specified global function
-- @param {symbol} func - function specified as symbol
-- @return {string|VOID}
----------------------------------------
on getGlobalFunctionCode (func)
  -- iterate over all members in all castlibs
  repeat with i = 1 to _movie.castlib.count
    repeat with j = 1 to _movie.castlib[i].member.count
      m = _movie.castlib[i].member[j]
      if m.type<>#script then next repeat
      if m.scriptType=#movie and m.script.handler(func) then return m.script.text
    end repeat
  end repeat
end
```

Usage:

```lingo
obj = script("MyClass").new()
put getClassCode(obj)
-- script text is printed...

func = #startMovie
put getGlobalFunctionCode(func)
-- script text is printed...
```



## Perl 6

A full path is provided for built-in routines/methods. However for routines exported by pre-compiled modules a precompilation hash is returned, not a proper file path.


```perl6
say &sum.file;
say Date.^find_method("day-of-week").file;
```


```txt

SETTING::src/core/Any.pm
SETTING::src/core/Dateish.pm

```



## Phix

There are at least two separate methods for achieving this.

### From Edita

When using the Edita editor as shipped with Phix, pressing F1 on a call shows a pop-up with the definition and an option to jump to that file/source.
However that only works for routines, not variables/constants/etc. That (older) method is powered by a background scan which periodically saves the required information in edita.edb - and you can investigate the contents of that via Tools/Database Viewer.

Holding down the Ctrl key and hovering the mouse over an identifier changes it to a link; left clicking will jump straight to the definition, and right-clicking will show a pop-up with a basic summary, and an option to jump directly to that file/source.

The latter is achieved (see demo\edita\src\eaisense.ew) by invoking "pw.exe -isense file line col partial_key mode main_file" which plants the requested information from a partial compilation into %TMP%\isense.txt (don't worry, it's near-instant) and sends back a message once it is ready. 
Edita handles the messy details of all that for you automatically, but of course you are free to copy and modify those techniques however you like.

I should warn you that Edita is windows-only, however Edix is well under way and if ever finished will offer the same functionality cross-platform.


### Programmatically

The autoinclude file builtins\pincpathN.e defines the include_paths() builtin, which returns something like:

```txt

    {"C:\\Program Files (x86)\\Phix\\builtins\\",
     "C:\\Program Files (x86)\\Phix\\builtins\\VM\\",
     "C:\\Program Files (x86)\\Phix\\"}

```

or the directories where your project is located. If you examine that source file (pincpathN.e) you will see a commented-out constant T_fileset and it should not be hard to imagine a matching (but currently missing) sister function named include_files() that retrieves a set of {idx,name} where idx are indexes into the result from above, should that be of interest to you. Note that if it is a pre-compiled executable installed on an end users machine, those sources might not be available, but the internal routines still provide the same names since they are generally quite useful for diagnostic purposes.

A caught exception (see the documentation for throw) contains the line number, file, and path (with the same caveat as above when installed elsewhere).

The file pincpathN.e also shows you how to get your grubby little mitts on the symbol table, and you may also want to look at builtins/VM/prtnidN.e for some ideas on scanning that. Throwing and catching an exception is one way to ensure the symbol table is populated with proper names rather than obscure integer ternary tree indexes. See pglobals.e for detailed notes about the contents of the symbol table. Some caution is advised here; the compiler my plant some otherwise illegal raw values in the symbol table, and play fast and loose with reference counts, etc, and of course messing it up may make life (/subsequent debugging) rather troublesome. 

Of course the source files are just files that you can read like any other text files, and for all its detail, the symbol table contains very little in the way of context, which may make mapping of non-unique identifiers rather difficult.


## Python

Modules loaded from files have a <code>__file__</code> attribute.

```python
import os
os.__file__
# "/usr/local/lib/python3.5/os.pyc"

```



## REXX

This REXX version was modeled after the   '''zkl'''   example, but in addition, it also displays the source. 

```rexx
/*REXX program gets the source function (source code) and */
/*───────────────────────── displays the number of lines. */
#=sourceline()
                do j=1  for sourceline()
                say 'line'  right(j, length(#) )  '──►'   ,
                            strip( sourceline(j), 'T')
                end   /*j*/
say
parse source x y sID
say  'The name of the  source file (program) is: '    sID
say  'The number of lines in the source program: '     #
                     /*stick a fork in it, we're all done.*/
```

```txt

line  1 ──► /*REXX program gets the source function (source code) and */
line  2 ──► /*───────────────────────── displays the number of lines. */
line  3 ──► #=sourceline()
line  4 ──►                 do j=1  for sourceline()
line  5 ──►                 say 'line'  right(j, length(#) )  '──►'   ,
line  6 ──►                             strip( sourceline(j), 'T')
line  7 ──►                 end   /*j*/
line  8 ──► say
line  9 ──► parse source x y sID
line 10 ──► say  'The name of the  source file (program) is: '    sID
line 11 ──► say  'The number of lines in the source program: '     #
line 12 ──►                      /*stick a fork in it, we're all done.*/

The name of the  source file (program) is:  c:\reflecti.rex
The number of lines in the source program:  12

```



## Ring


```ring

# Project : Reflection/Get source

fp = fopen("C:\Ring\applications\fifteenpuzzle\CalmoSoftFifteenPuzzleGame.ring","r")
r = ""
str = ""
flag = 0
numline = 0
see "give the function: "
give funct 
funct = "func " + funct
while isstring(r)
        r = fgetc(fp)
        if r = char(10)
           flag = 1
           numline = numline + 1
        else 
           flag = 0
           str = str + r
        ok
        if flag = 1
           if left(str,len(funct)) = funct
              see '"' + funct + '"' +" is in the line: " + numline + nl
           ok
           str = ""
        ok
end
fclose(fp)

```

Output:

```txt

give the function: rotateleft
"func rotateleft" is in the line: 328

```



## Ruby

<code>[http://ruby-doc.org/core/Method.html#method-i-source_location Method#source_location]</code> will return the file and line number of a Ruby method. If a method wasn't defined in Ruby, <code>Method#source_location</code> returns nil.

```ruby
require 'mathn'
Math.method(:sqrt).source_location
# ["/usr/local/lib/ruby2.3/2.3.0/mathn.rb", 119]

Class.method(:nesting).source_location
# nil, since Class#nesting is native

```



## Tcl

Tcl's <tt>info</tt> command makes it possible to access the source of nearly anything.  This example can show the source code of any proc.  The popular <b>tkcon</b> includes a <tt>dump</tt> command which is capable of showing aliases, arrays and more .. and a <tt>edit</tt> command which lets you edit them in an interactive window!


```Tcl
proc getproc {name} {
    set name [uplevel 1 [list namespace which -command $name]]
    set args [info args $name]
    set args [lmap arg $args {  ;# handle default arguments, if it has them!
        if {[info default $name $arg default]} {
            list $name $default
        } else {
            return -level 0 $arg
        }
    }]
    set body [info body $name]
    list proc $name $args $body
}

puts [getproc getproc]
```


Note the output differs very slightly from the original source:  the procedure's name is fully namespace-qualified, and the arguments are in "canonical list form", which does not include braces in this simple case.


```Tcl
proc ::getproc name {
    set name [uplevel 1 [list namespace which -command $name]]
    set args [info args $name]
    set args [lmap arg $args {  ;# handle default arguments, if it has them!
        if {[info default $name $arg default]} {
            list $name $default
        } else {
            return -level 0 $arg
        }
    }]
    set body [info body $name]
    list proc $name $args $body
}
```



## zkl

Reads the source file and counts the lines.

```zkl
src:=File(__FILE__).read();
println("Src file is \"%s\" and has %d lines".fmt(__FILE__,src.len(1)));
```

```txt

$ zkl foo
Src file is "foo.zkl" and has 2 lines

```


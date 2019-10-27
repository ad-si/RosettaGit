+++
title = "Safe mode"
description = ""
date = 2019-09-21T22:31:23Z
aliases = []
[extra]
id = 22190
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

Does the language implementation allow for a "safer mode" of execution?  Usually termed '''Safe mode''', a more realistic view is probably '''Safer mode''' or '''restricted mode'''.  It is one thing to place restrictions on execution, and another thing entirely to allow execution of scripts from untrusted sources and assume nothing untoward will happen.

Along with a simple yes/no answer, describe what features are restricted when running in safe mode.


## AWK


```AWK

# syntax: GAWK --sandbox -f SAFE_MODE.AWK
#
# Per The GNU Awk User’s Guide, edition 4.2.1
# https://www.gnu.org/software/gawk/manual/gawk.html
#   --sandbox or -S
#   Disable the system() function, input redirections with getline, output
#   redirections with print and printf, and dynamic extensions.  This is
#   particularly useful when you want to run awk scripts from questionable
#   sources and need to make sure the scripts can't access your system (other
#   than the specified input data file).
#
# Error message when running in sandbox mode:
# gawk: SAFE_MODE.AWK:16: fatal: redirection not allowed in sandbox mode
#
BEGIN {
    print("hello world") >"A.TMP"
    exit(0)
}

```


<!-- == Free Pascal == -->
{{omit from|Free Pascal}}


## Go

''Any'' code written in Go is considered to be 'safe' unless it uses one or more of the following features:

* The 'unsafe' package.

* The 'reflect' package.

* cgo.


Although 'normal' Go code uses pointers, arithmetic on them is not permitted and so they cannot be made to point to arbitrary locations in memory. However, the 'unsafe' package contains features which do allow one to perform pointer arithmetic with all the risks this entails.

The 'reflect' package allows one to inspect and manipulate objects of arbitrary types and exposes internal data structures such as string and slice headers. This can result in fragile code where mistakes which would normally be identified at compile time will instead manifest themselves as runtime panics.

'cgo' is Go's bridge to using C code. As such it is just as unsafe as writing C code directly.


## Frink

Frink has an extensive and fine-grained security manager architecture which allows the language to disallow dangerous operations by everyone, or allow very specific operations for specific users.

The easiest way to test this is to add the <CODE>--sandbox</CODE> option when starting Frink.  This enforces the strictest sandboxing mode.  Similarly, when creating a Frink interpreter from Java code, the most restrictive security can be enabled by calling its <CODE><I>Frink</I>.setRestrictiveSecurity(true)</CODE> method.


```java

frink.parser.Frink interp = new frink.parser.Frink();
interp.setRestrictiveSecurity(true);

```


Below are some operations that can be allowed/disallowed from a custom security manager.  For most of these, the permission can be restricted to allow/disallow a ''particular'' file, URL, or class, or method:

* Read a file or URL
* Call <CODE>unsafeEval</CODE>
* Import another file using <CODE>use</CODE>
* Construct a new Java object
* Call a static method on a Java class or object
* Access a static Java field
* Call a method on a Java object
* Write a field on a Java object
* Define a function
* Set a global flag
* Print to a printer
* Write a file
* Open a graphics window
* Construct an expression from an expression type and argument list
* Transform an expression
* Create a transformation rule
* Set a class-level variable

All of these operations are disallowed when the most restrictive security is enabled.


## Jsish


The '''jsish''' interpreter allows a '''-s''', '''--safe''' command line switch to restrict access to the file system.

For example, given '''safer.jsi''':


```javascript
File.write('/tmp/safer-mode.txt', 'data line');
```


{{out}}

```txt
prompt$ jsish safer.jsi
prompt$ jsish -s safer.jsi
/home/btiffin/lang/jsish/safer.jsi:2: error: write access denied by safe interp: /tmp/safer-mode.txt    (at or near "data line")

ERROR
```


The Jsish implementation borrows many ideas from [[Tcl]], and also includes an Interp module.  These sub interpreters can also be set to run in a safer mode. 


```txt
prompt$ jsish
# var si = new Interp({isSafe:true});
variable
# si.source('safer.jsi');
error: read access denied: /home/btiffin/lang/jsish/safer.jsi
ERROR
```


Some control is allowed over the restrictions provided by safer mode.


```javascript
var interp1 = new Interp({isSafe:true, safeWriteDirs:['/tmp'], , safeReadDirs:['/tmp']});
```



## Perl


The only built-in 'safer' way to run Perl is to invoke it in 'taint mode' with the command line option <code>-T</code>. While in this mode input from the user, and all variables derived from it, cannot be used in certain contexts until 'sanitized' by being passed through a regular expression.

There is a CPAN module <code>Safe</code> that purports to allow Perl to be compiled and executed in <tt>restricted compartments</tt>, isolated namespaces with limits on allowed operators. However, it has not been updated in over 6 years, and so not tested with recent releases of Perl. Further, it does not appear to work at all on BSD-derived versions of UNIX. 

There's really no switch to flip to make Perl code more secure. It is up to the programmer to follow security best-practices, such as employing the <code>strict</code> and <code>warnings</code> pragmas, using 3-argument form of <code>open</code> for filehandles, being careful about the contents of <code>$ENV{PATH}</code>, and so forth.  The CPAN module <code>Perl::Critic</code> can be helpful in this regard.  Read further on this topic in the language documentation on [https://perldoc.perl.org/perlsec.html Perl security]


## Perl 6

''Mostly a cut-n-paste from the [[Untrusted_environment#Perl_6|Untrusted environment]] task.

Perl 6 doesn't really provide a high security mode for untrusted environments. By default, Perl 6 is sort of a walled garden. It is difficult to access memory directly, especially in locations not controlled by the Perl 6 interpreter, so unauthorized memory access is unlikely to be a threat with default Perl 6 commands and capabilities.

It is possible (and quite easy) to run Perl 6 with a restricted setting which will disable many IO commands that can be used to access or modify things outside of the Perl 6 interpreter. However, a determined bad actor could theoretically work around the restrictions, especially if the nativecall interface is available. The nativecall interface allows directly calling in to and executing code from C libraries so anything possible in C is now possible in Perl 6. This is great for all of the power it provides, but along with that comes the responsibility and inherent security risk. The same issue arises with unrestricted loading of modules. If modules can be loaded, especially from arbitrary locations, then any and all restriction imposed by the setting can be worked around.

The restricted setting is modifiable, but by default places restrictions on or completely disables the following things:

;User Subroutines (disabled)

:* sub chmod() ''modify filesystem permissions''
:* sub copy()  ''copy a file''
:* sub link()  ''create a link to a file''
:* sub mkdir() ''make a filesystem directory''
:* sub open()  ''open a filesystem location / file''
:* sub pipe()  ''open a pipe''
:* sub QX()    ''execute arbitrary code''
:* sub rename() ''rename a file''
:* sub rmdir() ''remove a directory''
:* sub run()   ''run arbitrary code''
:* sub shell() ''execute code in a shell''
:* sub socket() ''open a socket''
:* sub spurt() ''write a file''
:* sub symlink() ''create a symbolic link to a location''
:* sub unlink() ''delete a file''

;Internal Subroutines (disabled)

:* sub CHANGE-DIRECTORY ''change directory''
:* sub CHMOD-PATH ''change permissions''
:* sub COPY-FILE ''copy a file''
:* sub MAKE-DIR ''make a directory''
:* sub REMOVE-DIR ''remove a directory''
:* sub RENAME-PATH ''rename a directory''
:* sub SYMLINK-PATH ''create a symbolic link''
:* sub UNLINK-PATH ''delete a file''

;Classes (disabled)

:* class IO::CatHandle ''streaming file handle''
:* class IO::Handle ''file handle''
:* class IO::Path ''filesystem path''
:* class IO::Pipe ''OS pipe''
:* class IO::Socket ''OS socket''
:* class IO::Socket::INET ''Network socket''
:* class NativeCall ''Nativecall interface to foreign code (C mostly)''
:* class Proc ''OS Process''
:* class Proc::Async ''Asynchronous OS Process''

;Method Mixins / Roles (locked down so can't be overridden)

:* method FALLBACK() ''handle unknown method calls''
:* method new() ''create a new instance''
:* method gist() ''display method''

Really, if you want to lock down a Perl 6 instance so it is "safe" for unauthenticated, untrusted, general access, you are better off running it in some kind of locked down virtual machine or sandbox managed by the operating system rather than trying to build an ad hoc "safe" environment.


## Phix

''See [[Untrusted_environment#Phix]]


## Rust


While Rust compiles to native code and does not provide any kind of runtime sandbox, it does implement a compile-time enforced distinction between "safe" and "unsafe" code, intended to improve the maintainability of complex codebases by confining sources of certain types of difficult-to-debug problems to small, clearly marked subsets of the code which can be audited more intensely.

Safe code, which is the default, cannot cause memory unsafety or data races as long the unsafe code it depends on upholds the invariants expected of it.

Unsafe code, enabled by marking a function, block, or trait (interface) with the <code>unsafe</code> keyword, enables the use of four additional language capabilities which the compiler cannot verify correct use of and which are intended for building safe abstractions, such as the standard library's <code>Mutex</code> and reference-counted pointers.

Those four capabilities are:
* Dereferencing raw pointers (Rust's name for C-style pointers)
* Calling <code>unsafe</code> functions (All foreign functions, as well native APIs with safety invariants that are impossible or impractical to encode in the type system)
* Interacting with mutable static variables (the idiomatic solution is to use "interior mutability" via a wrapper type like <code>Mutex</code> or <code>RWLock</code> which allows a mutable value to be stored in an "immutable" static variable.)
* Implementing an <code>unsafe</code> trait (interface)

To further the goal of improving maintainability in large codebases, the Rust compiler can also be configured to warn or error out if <code>unsafe</code> code is encountered within a given scope.

(An example of this would be an enterprise project where the coders most experienced in low-level work are responsible for the module where <code>unsafe</code> is allowed, while the majority of the codebase lives in modules which depend on the unsafe-containing module, but are configured to forbid the use of <code>unsafe</code> within their own code.)


## REXX

For running REXX on IBM mainframes,   REXX supports the option   '''Scan'''   for the   '''trace'''   statement.

This allows the program to be processed (and be checked for syntax errors),   but commands to the "host system" won't be executed.

However, not all REXXes support this option.

Regina REXX supports a '''--restricted''' command-line option, and embedded interpreters can also be set to run restricted.  Many commands are disabled in this mode, including most access to hosted services.  The intrinsic '''FUNCTION REXX()''' extension in GnuCOBOL defaults to restricted mode, and programmers must explicitly use '''FUNCTION REXX-UNRESTRICTED(script, args...)''' for access to the full REXX programming environment from that [[COBOL]] implementation.


```cobol
       identification division.
       program-id. rexxtrial.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.

       procedure division.
      *> First attempt fails and return statement does not execute
       display rexx("ADDRESS SYSTEM; 'ls rexxtrial.cob'; return 'fail'")
       display "Exception: " exception-status

      *> Second is allowed and succeeds
       display "Try with rexx-unrestricted"
       display rexx-unrestricted(
           "ADDRESS SYSTEM; 'ls -l rexxtrial.cob'; return 'success'")
       display "No exception raised: " exception-status
       goback.
       end program rexxtrial.
```


{{out}}

```txt
$ cobc -xj rexxtrial.cob
     1 +++ 'ls rexxtrial.cob'
Error 95 running "gnucobol", line 1: [Restricted feature used in "safe" mode]
Error 95.5: [Running external commands invalid in "safe" mode]

Exception: EC-IMP-SCRIPT
Try with rexx-unrestricted
-rw-rw-r--. 1 btiffin btiffin 727 Feb 19 04:26 rexxtrial.cob
success
No exception raised:
```


## Scala

Actually, with a high-level programming language as Scala, it's a bad idea to flag or unflag for a "Safe mode"..
This should be a task for the target system.


## zkl

zkl is unsafe.  Any program can access any method and many methods access
the system.  Additionally, any program can load a program or 
eval (compile and run) text.

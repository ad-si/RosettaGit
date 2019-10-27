+++
title = "Native shebang"
description = ""
date = 2019-09-03T15:30:29Z
aliases = []
[extra]
id = 16108
[taxonomies]
categories = []
tags = []
+++

﻿{{draft task|Basic language learning}} [[Category:Programming environment operations]]
'''In short:''' Use the specimen language (native) for "scripting".

;Example: If your language is "foo", then the test case of "echo.foo" runs in a terminal as "<tt>./echo.foo Hello, world!"</tt>.


'''In long''': Create a program (in the specimen language) that will automatically compile a ''test case'' (of the same specimen language) to a native binary executable and then transparently load and run this ''test case'' executable.  
  
Make it so that all that is required is a custom [[wp:Shebang (Unix)|shebangs]] 
at the start of the ''test case''. e.g. "<tt>#!/usr/local/bin/script_foo</tt>"

'''Importantly:''' This task must be coded '''strictly''' in the specimen language, '''neither''' using a shell script '''nor''' any other 3rd language.

Optimise this progress so that the ''test program'' '''binary executable''' is only created if the original ''test program'' '''source code''' as been touched/edited.

Note: If the lauguage (or a specific implementation) handles this automatically, 
then simple provide an example of "echo.foo"


'''Background:'''

Simple [[wp:Shebang (Unix)|shebangs]] can help with scripting, e.g. "<tt>#!/usr/bin/env python</tt>" at the top of a [[Python]] script will allow it to be run in a terminal as "./script.py".

The task [[Multiline shebang]] largely demonstrates how to use "shell" code in the shebang to compile and/or run source-code from a 3rd language, typically "<tt>#!/bin/bash</tt>" or "<tt>#!/bin/sh</tt>".


'''This task:'''

However in this task '''Native shebang''' task we are go ''native''.  
In the shebang, instead of running a shell, we call a binary-executable 
generated from the original native language, 
e.g. when using [[C]] with gcc "<tt>#!/usr/local/bin/script_gcc</tt>" 
to extract, compile and run the native "script" source code.

Other small innovations required of this ''Native shebang'' task:
* Cache the executable in some appropriate place in a path, dependant on available write permissions.
* Generate a new cached executable only when the source has been touched.
* If a cached is available, then run this instead of regenerating a new executable.

'''Difficulties:'''
* Naturally, some languages are not compiled. These languages are forced to use shebang executables from another language, eg "<tt>#!/usr/bin/env python</tt>" uses the C binaries /usr/bin/env and /usr/bin/python. If this is the case, then simply document the details of the case.
* In a perfect world, the test file (e.g. ''echo.c'') would still be a valid program, and would compile without error using the native compiler (e.g. ''gcc'' for ''text.c'').  The problem is that "#!" is syntactically incorrect on many languages, but in others it can be parsed as a comment.
* The "test binary" should be [[wp:exec (computing)|exec]]-ed and hence retain the original [[wp:Process identifier|Process identifier]].

'''Test case:'''
* Create a simple "script file" (in the same native language) called "echo" then use the "script" to output "Hello, world!"


## ALGOL 68


### Using ALGOL 68G to script ALGOL 68G

{{works with|ALGOL 68|Revision 1.}}
{{works with|ALGOL 68G|Any - Tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.7 algol68g-2.7].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to algol68toc actually doing a real compilation.}}
Note: With Algol68G the option "-O3" will compile the script file to a ".so" file, 
this ".so" file is a binary executable and dynamically loaded library. 
Also note that this ".so" will only be generated if the ".a68" source file 
has been touched.
'''File: echo.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

STRING ofs := "";
FOR i FROM 4 TO argc DO print((ofs, argv(i))); ofs:=" " OD
```

'''Test Execution:'''

```txt

$ ./echo.a68 Hello, world!

```

{{out}}

```txt

Hello, world!

```



## C


### Using gcc to script C

{{incorrect|C|[[Talk:Native shebang#Problems]]: "The C example doesn't work for me (unless a segmentation fault from script_gcc.sh can be described as "working" or a bad interpreter error from echo.c can be described as "working"). --Rdm" }}

I was able to get this functional, by renaming the itoa() function to itoa_()  NOTE, there is an itoa() function already.  I will also write a 'correct' 2nd version of this example, that does not use a bash helper script (no reason for that).  --JimF

'''File: script_gcc.c'''

```c
#!/usr/local/bin/script_gcc.sh
/* Optional: this C code initially is-being/can-be boot strapped (compiled) using bash script_gcc.sh */
#include <errno.h>
#include <libgen.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* the actual shebang for C target scripts is:
#!/usr/local/bin/script_gcc.c
*/

/* general readability constants */
typedef char /* const */ *STRING;
typedef enum{FALSE=0, TRUE=1} BOOL;
const STRING ENDCAT = NULL;

/* script_gcc.c specific constants */
#define DIALECT "c" /* or cpp */
const STRING
  CC="gcc",
  COPTS="-lm -x "DIALECT,
  IEXT="."DIALECT,
  OEXT=".out";
const BOOL OPT_CACHE = TRUE;

/* general utility procedured */
char strcat_out[BUFSIZ];

STRING STRCAT(STRING argv, ... ){
  va_list ap;
  va_start(ap, argv);
  STRING arg;
  strcat_out[0]='\0';
  for(arg=argv; arg != ENDCAT; arg=va_arg(ap, STRING)){
     strncat(strcat_out, arg, sizeof strcat_out);
  }
  va_end(ap);
  return strndup(strcat_out, sizeof strcat_out);
}

char itoa_out[BUFSIZ];

STRING itoa_(int i){
  sprintf(itoa_out, "%d", i);
  return itoa_out;
}

time_t modtime(STRING filename){
  struct stat buf;
  if(stat(filename, &buf) != EXIT_SUCCESS)perror(filename);
  return buf.st_mtime;
}

/* script_gcc specific procedure */
BOOL compile(STRING srcpath, STRING binpath){
  int out;
  STRING compiler_command=STRCAT(CC, " ", COPTS, " -o ", binpath, " -", ENDCAT);
  FILE *src=fopen(srcpath, "r"),
       *compiler=popen(compiler_command, "w");
  char buf[BUFSIZ];
  BOOL shebang;

  for(shebang=TRUE; fgets(buf, sizeof buf, src); shebang=FALSE)
    if(!shebang)fwrite(buf, strlen(buf), 1, compiler);

  out=pclose(compiler);
  return out;
}

void main(int argc, STRING *argv, STRING *envp){

  STRING binpath,
         srcpath=argv[1],
         argv0_basename=STRCAT(basename((char*)srcpath /*, .DIALECT */), ENDCAT),
         *dirnamew, *dirnamex;
  argv++; /* shift */

/* Warning: current dir "." is in path, AND * /tmp directories are common/shared */
  STRING paths[] = {
    dirname(strdup(srcpath)), /* not sure why strdup is required? */
    STRCAT(getenv("HOME"), "/bin", ENDCAT),
    "/usr/local/bin",
    ".",
    STRCAT(getenv("HOME"), "/tmp", ENDCAT),
    getenv("HOME"),
    STRCAT(getenv("HOME"), "/Desktop", ENDCAT),
/*  "/tmp" ... a  bit of a security hole */
    ENDCAT
  };

  for(dirnamew = paths; *dirnamew; dirnamew++){
    if(access(*dirnamew, W_OK) == EXIT_SUCCESS) break;
  }

/* if a CACHEd copy is not to be kept, then fork a sub-process to unlink the .out file */
  if(OPT_CACHE == FALSE){
    binpath=STRCAT(*dirnamew, "/", argv0_basename, itoa_(getpid()), OEXT, ENDCAT);
    if(compile(srcpath, binpath) == EXIT_SUCCESS){
      if(fork()){
        sleep(0.1); unlink(binpath);
      } else {
        execvp(binpath, argv);
      }
    }
  } else {
/* else a CACHEd copy is kept, so find it */
    time_t modtime_srcpath = modtime(srcpath);
    for(dirnamex = paths; *dirnamex; dirnamex++){
      binpath=STRCAT(*dirnamex, "/", argv0_basename, OEXT, ENDCAT);
      if((access(binpath, X_OK) == EXIT_SUCCESS) && (modtime(binpath) >= modtime_srcpath))
        execvp(binpath, argv);
    }
  }

  binpath=STRCAT(*dirnamew, "/", argv0_basename, OEXT, ENDCAT);
  if(compile(srcpath, binpath) == EXIT_SUCCESS)
    execvp(binpath, argv);

  perror(STRCAT(binpath, ": executable not available", ENDCAT));
  exit(errno);
}
```


'''Test Source File: echo.c'''

```c
#!/usr/local/bin/script_gcc.c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char **argv, char **envp){
  char ofs = '\0';
  for(argv++; *argv; argv++){
    if(ofs)putchar(ofs); else ofs=' ';
    fwrite(*argv, strlen(*argv), 1, stdout);
  }
  putchar('\n');
  exit(EXIT_SUCCESS);
}
```


'''Test Execution:'''

```txt

$ ./echo.c Hello, world!

```

{{out}}

```txt

Hello, world!

```


===2nd version. Pure C, no extra bash script===
'''File: script_gcc.c'''

```c
/*
 * rosettacode.org: Native shebang
 *
 * Copyright 2015, Jim Fougeron.  Code placed in public domain. If you
 * use this, please provide attribution to author. Code originally came
 * from the code found on rosettacode.org/wiki/Native_shebang, however
 * the code was about 80% rewritten.  But the logic of the compile()
 * function still strongly is based upon original code, and is a key
 * part of the file.
 *
 * Native C language shebang scripting. Build this program to /usr/local/bin
 * using:
 *    gcc -o/usr/local/bin/script_gcc script_gcc.c
 *
 * The name of the executable: "script_gcc" IS critical. It is used in knowing
 * that we have have found the proper shebang file when parsing commandline
 *
 * the actual shebang for executable C source scripts is:

#!/usr/local/bin/script_gcc [extra compile/link options]

 * If there additional lib's needed by your source file, then add the proper
 * params to the shebang line. So for instance if gmp, openssl, and zlib
 * were needed (and an additional include path), you would use this shebang:

 #!/usr/local/bin/script_gcc -lgmp -lssl -lcrypto -lz -I/usr/local/include

 * NOTE, we leak strdup calls, but they are 1 time leaks, and this process
 * will simply exec another process, so we ignore them.
 */

#include <errno.h>
#include <libgen.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#define shebangNAME "script_gcc"
#define CC          "gcc"
#define CC_OPTS     "-lm -x c"
#define IEXT        ".c"
#define OEXT        ".out"

/* return time of file modification. If file does not exit, return 0
 * so that if we compare, it will always be older, and will be built */
time_t modtime(const char *filename) {
  struct stat st;

  if(stat(filename, &st) != EXIT_SUCCESS)
    return 0;
  return st.st_mtime;
}
/* join a pair of strings */
char *sjoin(const char *s1, const char *s2){
  char buf[BUFSIZ];

  if (!s1) s1=""; if (!s2) s2="";
  snprintf(buf, sizeof(buf), "%s%s", s1, s2);
  return strdup(buf);
}
/* compiles the original script file. It skips the first line (the shebang)
 * and compiles using "gcc .... -x c -" which avoids having to write a temp
 * source file (minus the shebang), we instead pipe source to gcc */
int compile(const char *srcpath, const char *binpath, const char *ex_comp_opts) {
  char buf[BUFSIZ];
  FILE *fsrc, *fcmp;

  sprintf(buf, "%s %s %s -o %s -", CC, CC_OPTS, ex_comp_opts, binpath);
  fsrc=fopen(srcpath, "r");
  if (!fsrc) return -1;
  fcmp=popen(buf, "w"); /* open up our gcc pipe to send it source */
  if (!fcmp) { fclose(fsrc); return -1; }

   /* skip shebang line, then compile rest of the file. */
  fgets(buf, sizeof(buf), fsrc);
  fgets(buf, sizeof(buf), fsrc);
  while (!feof(fsrc)) {
    fputs(buf, fcmp); /* compile this line of source with gcc */
    fgets(buf, sizeof(buf), fsrc);
  }
  fclose(fsrc);
  return pclose(fcmp);
}

/* tries to open the file 'argv0'. If we can open that file we read first line
 * and make SURE it is a script_gcc file. If it is a script_gcc file, we
 * look for any extra params for the compiler (params to the shebang line)
 * and if we find them, they are returned in ex_comp_opts.
 * return 0 if this is NOT a shebang file, return 1 if it IS the shebang file.
 */
int load_shebangline(const char *argv0, char **ex_comp_opts) {
  char opt[BUFSIZ], *cp;
  FILE *in = fopen(argv0, "r");

  if (!in) return 0;
  fgets(opt, sizeof(opt), in);
  fclose(in);
  /* ok, we found a readable file, but IS it our shebang file? */
  strtok(opt, "\r\n");
  if (strncmp(opt, "#!", 2) || !strstr(opt, shebangNAME))
    return 0; /* nope, keep looking */
  cp = strstr(opt, shebangNAME)+strlen(shebangNAME);
  if (*cp) /* capture compiler extra params, if any */
    *ex_comp_opts = strdup(cp);
  return 1;
}

/* NOTE, the argv[] array is different than 'normal' C programs.  argv[0] is
 * the shebang exe file. then argv[1] ... argv[p] are the params that follow
 * the shebang script name (from line1 of the script file). NOTE, some systems
 * (Linux, cygwin), will pack all of these options into argv[1] with spaces.
 * There is NO 'standard' on exactly what the layout it of these shebang params
 * may be, we only know that there will be 0 or more params BEFORE the script
 * name (which is the argv[0] we normally 'expect). Then argv[p+1] is the name
 * of script being run. NOTE if there are no shebang args, argv[1] will be
 * the script file name.  The script file name is our expected argv[0], and
 * all the params following that are the normal argv[1]...argv[n] we expect.
 */
int main(int argc, char *const argv[]) {
  int i;
  char exec_path[BUFSIZ], *argv0_basename, *ex_comp_opts=0, **dir,
  *paths[] = {
    NULL, /* paths[0] will be filled in later, to dir of the script */
    "/usr/local/bin",
    sjoin(getenv("HOME"), "/bin"),
    sjoin(getenv("HOME"), "/tmp"),
    getenv("HOME"),
    sjoin(getenv("HOME"), "/Desktop"),
    /* . and /tmp removed due to security concerns
    ".",
    "/tmp", */
    NULL
  };

  /* parse args, looking for the one that is the script. This would have been argv[0] if not exec'd as a shebang */
  for (i = 1; i < argc; ++i) {
    if (load_shebangline(argv[1], &ex_comp_opts)) {
      argc -= i;
      i = 0;
      break;
    }
    ++argv;
  }
  if (i)
    return !fprintf(stderr, "could not locate proper %s shebang file!!\n", shebangNAME) | ENOENT;
  ++argv;
  /* found it.  Now argv[0] is the 'script' name, and rest of argv[] is params to the script */
  argv0_basename = basename(strdup(argv[0]));
  paths[0] = dirname(strdup(argv[0]));
  /* find a cached version of the script, and if we find it, run it */
  for(dir = paths; *dir; dir++) {
    snprintf(exec_path, sizeof(exec_path), "%s/%s%s", *dir, argv0_basename, OEXT);
    if(modtime(exec_path) >= modtime(argv[0]))
      execvp(exec_path, argv); /* found a newer cached compiled file. Run it */
  }
  /* no cached file, or script is newer. So find a writeable dir */
  for(dir = paths; *dir; dir++) {
    if(!access(*dir, W_OK))
      break;
  }
  if (!*dir)
    return !fprintf(stderr, "No writeable directory for compile of the script %s\n", argv[0]) | EACCES;
  /* compile and exec the result from our C script source file */
  snprintf(exec_path, sizeof(exec_path), "%s/%s%s", *dir, argv0_basename, OEXT);
  if(!compile(argv[0], exec_path, ex_comp_opts))
    execvp(exec_path, argv);
  return !fprintf(stderr, "%s : executable not available\n", exec_path) | ENOENT;
}

```


'''Test Source File: echo.c'''

```c
#!/usr/local/bin/script_gcc
/*
 * note, any additional libs or include paths would have params added after
 * the script_gcc parts of the shebang line, such as:
 * #!/usr/local/bin/script_gcc -lgmp -I/usr/local/include/gmp5
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char **argv, char **envp){
  char ofs = '\0';
  for(argv++; *argv; argv++){
    if(ofs)putchar(ofs); else ofs=' ';
    fwrite(*argv, strlen(*argv), 1, stdout);
  }
  putchar('\n');
  exit(EXIT_SUCCESS);
}
```


'''Test Execution:'''

```txt

$ ./echo.c Hello, world!

```

{{out}}

```txt

Hello, world!

```



## Free Pascal

Since FPC (FreePascal compiler) version 2.6.0 the distribution – e.g. the Debian or FreeBSD packages <tt>fpc-utils</tt> – come with the program <tt>instantfpc(1)</tt>, or <tt>ifpc(1)</tt> for short.
The program fulfills this task’s specifications, plus other goodies.
The sources are available in [https://svn.freepascal.org/cgi-bin/viewvc.cgi/trunk/utils/instantfpc/instantfpc.pas?view=markup <tt>trunk/utils/instantfpc/instantfpc.pas</tt>] and are not repeated here.
See the [https://wiki.freepascal.org/InstantFPC FreePascal wiki for <tt>ifpc</tt> usage].


## Go

In Go, single line comments can only begin with // and so we have to use the former in place of a normal shebang (#!) to achieve the same effect.

The 'go run' command compiles a .go source code file to a binary executable and then runs the latter automatically. The executable is placed in a temporary directory which is then deleted when the process ends. 

To cache the executable in the current directory, one would have to use 'go build' instead (replace the opening line with ///usr/bin/env go build echo.go; ./echo "$@"; exit).

The following works fine on Ubuntu 16.04.

```go
///usr/bin/env go run echo.go "$@"; exit
package main

import (
    "fmt"
    "os"
)

func main() {
    if len(os.Args) > 1 {
        fmt.Println(os.Args[1])
    }
}
```


{{out}}

```txt

$ ./echo.go "Hello, world!"
Hello, world!

```



## J

As no compiling versions of J are currently available, the binaries are trivially empty and we shall store them in the empty path. We shall use /usr/local/bin/ijconsole (which was compiled using a C compiler) as the J interpreter, and <code>echo each ARGV</code> as our sample code:


```J
#!/usr/local/bin/ijconsole
echo each ARGV
```



## jq

{{works with|jq|1.4}}

jq can be invoked on the shebang line, e.g. as

 #!/usr/local/bin/jq -M -n -f

or

 #!/usr/bin/env jq -M -n -f

'''Example 1:'''

```sh
$ cat echo.foo
#!/usr/bin/env/jq -M -n -r -f
"Klaatu barada nikto!"
```



 $ ./echo.foo 
 Klaatu barada nikto!

Command-line parameters of a script created with a shebang line in this manner are processed as jq command-line parameters.
Thus, instead of being able to invoke the script along the lines of 
 $ ./echo.foo "Hello world!"   # nope

one would have to introduce a named variable to hold the command-line parameter,
as illustrated in the next example:

'''Example 2:'''

```sh
$ cat echo.foo
#!/usr/bin/env/jq -M -n -r -f
$x
```

{{out}}

```sh
$ ./echo.foo --arg x "Hello, world!"
Hello, world!
```



## Julia

usage: ./thisfile.jl "hello"

```julia
#!/usr/local/bin/julia

# Put the Julia code below this line. It will be compiled and run.

Base.banner()
println(ARGS)


```
{{out}}

```txt

               _
   _       _ _(_)_     |  Documentation: https://docs.julialang.org
  (_)     | (_) (_)    |
   _ _   _| |_  __ _   |  Type "?" for help, "]?" for Pkg help.
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version 1.1.1 (2019-05-16)
 _/ |\__'_|_|_|\__'_|  |  Official https://julialang.org/ release
|__/ 
hello                  |

```



## OCaml

OCaml can run in script mode or compiled (compiled to bytecode or native binary).

'''File: echo.ml'''

```ocaml
#! /usr/bin/env ocaml

let () =
  let argl = Array.to_list Sys.argv in
  print_endline (String.concat " " (List.tl argl))
```

{{out}}

```txt

$ chmod u+x echo.ml
$ ./echo.ml Hello, world!
Hello, world!

```

But we have to remove the shebang if we want to compile the code, or we get an error:

```txt

$ ocamlc -c echo.ml
File "echo.ml", line 1, characters 0-2:
Error: Syntax error

```



## Perl

Perl is a script language. It's natural and easy to script native Perl code.

'''File: echo.pl'''

```perl
#!/usr/bin/perl
print "@ARGV\n";

```


'''Usage:'''

```txt

./echo.pl Hello, world!

```

{{out}}

```txt

Hello, world!

```



## Perl 6

Perl 6 is not installed by default on most systems and does not have a default install directory, so the path will vary by system.
{{works with|Rakudo|2015-11-20}}

'''File: echo.p6'''

```perl6
#!/path/to/perl6
put @*ARGS;
```


'''Usage:'''

```txt

./echo.p6 Hello, world!

```

{{out}}

```txt

Hello, world!

```



## Python

Extract: "If you need to create a .pyc file for a module that is not imported, you can use the py_compile and compileall modules. The py_compile module can manually compile any module. One way is to use the py_compile.compile function in that module interactively:[http://effbot.org/pyfaq/how-do-i-create-a-pyc-file.htm]:"

```txt

>>> import py_compile
>>> py_compile.compile('echo.py')

```


'''File: echo.py'''

```python
#!/path/to/python
# Although `#!/usr/bin/env python` may be better if the path to python can change

import sys
print " ".join(sys.argv[1:])
```


'''Usage:'''

```txt

./echo.py Hello, world!

```

{{out}}

```txt

Hello, world!

```



## Racket

Racket has [http://docs.racket-lang.org/raco/index.html?q=raco <tt>raco: Racket Command Line Tools</tt>]
which can be used to compile to bytecode or compile to standalone executables (along with a whole load of
other fun stuff to do with packages, unit testing and the likes).

To properly compile a file/program, one needs to invoke <tt>raco</tt> or go through invocations of racket
to see what needs to be done. Compilation is expensive. Dependency management is expensive and difficult
to do. The only one who can probably be trusted to do this is <tt>raco</tt>. So (as with Python), if you
need to compile the program, do so with the compiler.

Once you have done this, however, racket (in the shebang) will use the 'compiled' version, not the source.

In this example:

File <tt>native-shebang.rkt</tt> contains the following:

```racket
#! /usr/local/racket-6.1/bin/racket
#lang racket
(displayln "hello")
```


My directory contains only this:

```txt
-bash-3.2$ ls
native-shebang.rkt
```

Which runs:

```txt
-bash-3.2$ ./native-shebang.rkt 
hello
```

But has not self-compiled or anything like that:

```txt
-bash-3.2$ ls
native-shebang.rkt
```

I run <tt>raco</tt> to compile it:

```txt
-bash-3.2$ raco make native-shebang.rkt 
-bash-3.2$ ls -R
.:
compiled  native-shebang.rkt

./compiled:
native-shebang_rkt.dep  native-shebang_rkt.zo

```

The dependency file and byte-code -- <tt>.zo</tt> -- file are in a <tt>compiled</tt> directory.

I still run <tt>native-shebang.rkt</tt> from the script (with the <tt>racket</tt> shebang). Racket will use the compiled code instead of the source in the script:

```txt
-bash-3.2$ ./native-shebang.rkt 
hello
```

(although it's hard to prove)


## REXX


### Unix shebang

Using e.g. Regina open source REXX interpreter

```rexx

#!/usr/local/bin/regina
/* Echo the command line argument */
say arg(1)

```



### ARexx

Under AmigaOS, the obligatory REXX starting comment /* is recognised as a shebang of its own, automatically causing the file to be parsed by ARexx as long as the file's script flag is set.

```rexx

/* Echo the command line argument */
say arg(1)

```



## Ruby

Ruby does not compile to a binary, thankfully.


## Sidef

Sidef is a scripting language and does not compile to a binary.

```ruby
#!/usr/bin/sidef
say ARGV.join(" ")
```


{{out}}

```txt

$ ./echo.sf Hello, World!
Hello, World!

```



## Swift

Using Swift REPL:

'''File: echo.swift'''

```swift
#!/usr/bin/swift

import Foundation

print(Process.arguments[1..<Process.arguments.count].joinWithSeparator(" "))

```


'''Usage:'''


```bash

./echo.swift Hello, world!

```


{{Out}}


```txt

Hello, world!

```



## UNIX Shell


### Using sh to script sh

In strictly shell this is natural, native and easy:

'''File: echo.sh'''

```sh
#!/bin/sh
echo "$@"
```


'''Usage:'''

```txt

./echo.sh Hello, world!

```

{{out}}

```txt

Hello, world!

```



### Using bash to script C

{{works with|Bourne Again SHell}}

Note: this '''Native shebang''' task does not exactly apply to [[bash]] because bash is interpretive, but as a skeleton template the following script is an example of how compiled languages can implement the shebang.  Also: this bash code can be used to ''automatically'' compile the C code in /usr/local/bin/script_gcc.c above.

'''File: script_gcc.sh'''

```bash
#!/bin/bash

# Actual shebang when using bash:
#!/usr/local/bin/script_gcc.sh

# Alternative shebang when using bash:
#!/bin/bash /usr/local/bin/script_gcc.sh

# CACHE=No # to turn off caching...

# Note: this shell should be re-written in actual C! :-)

DIALECT=c # or cpp
CC="gcc"
COPTS="-lm -x $DIALECT"
IEXT=.$DIALECT
OEXT=.out

ENOENT=2

srcpath="$1"; shift # => "$@"
#basename="$(basename "$srcpath" ."$DIALECT")"
basename="$(basename "$srcpath")"

# Warning: current dir "." is in path, AND */tmp directories are common/shared
paths="$(dirname "$srcpath")
$HOME/bin
/usr/local/bin
.
$HOME/tmp
$HOME
$HOME/Desktop"
#/tmp

while read dirnamew; do
  [ -w "$dirnamew" ] && break
done << end_here_is
$paths
end_here_is

compile(){
  sed -n '2,$p' "$srcpath" | "$CC" $COPTS -o "$binpath" -
}

if [ "'$CACHE'" = "'No'" ]; then
  binpath="$dirnamew/$basename-v$$$OEXT"
  if compile; then
    ( sleep 0.1; exec rm "$binpath" ) & exec "$binpath" "$@"
  fi
else
  while read dirnamex; do
    binpath="$dirnamex/$basename$OEXT"
    if [ -x "$binpath" -a "$binpath" -nt "$srcpath" ];
      then exec "$binpath" "$@"; fi
  done << end_here_is
$paths
end_here_is

  binpath="$dirnamew/$basename$OEXT"
  if compile; then exec "$binpath" "$@"; fi

  echo "$binpath: executable not available" 1>&2
  exit $ENOENT
fi
```

'''Test Source File: echo.c'''

```c
#!/usr/local/bin/script_gcc.sh
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char **argv, char **envp){
  char ofs = '\0';
  for(argv++; *argv; argv++){
    if(ofs)putchar(ofs); else ofs=' ';
    fwrite(*argv, strlen(*argv), 1, stdout);
  }
  putchar('\n');
  exit(EXIT_SUCCESS);
}
```


'''Test Execution:'''

```txt

$ ./echo.c Hello, world!

```

{{out}}

```txt

Hello, world!

```



## zkl

This isn't something that is usually done with zkl as the compiler is fast enough (for scripts) to just run the source and let it get compiled and run. But, it can be done. Note: The binary still "links" against zkl (the VM) so the #! is required.

Since the #! parsing is done by a compiler front end and was designed to be used from the command line, we'll do that by forking zkl to compile the source if it is newer than the binary.

```zkl
#!/home/craigd/Bin/zkl
// This file: nativeShebang.zkl, compiles to nativeShebang.zsc
// zkl --#! . -c nativeShebang -o.
// chmod a+x nativeShebang.z*
// ./nativeShebang.zsc

// If this [source] file is newer than the compiled version (or the
// compiled file doesn't exist), compile up a new version

runningName :=System.argv[1];  // argv==("zkl","nativeShebang.zkl|zsc")
parts,path  :=File.splitFileName(runningName),parts[0,2].concat() or ".";
srcName     :=parts[0,-1].concat() + ".zkl";
compiledName:=parts[0,-1].concat() + ".zsc";
if(not File.exists(compiledName) or 
   File.info(srcName)[2] > File.info(compiledName)[2]){ // compare modifed dates
      System.cmd("zkl --#! . -c %s -o%s --exit".fmt(srcName,path));
      System.cmd("chmod a+x %s".fmt(compiledName));
}

////////////// the script:
println("Hello world!");
```

{{out}}

```txt

#run the source in the usual manner to generate a binary
#could use ./nativeShebang.zkl if chmod'd it
$ zkl nativeShebang.zkl
Compiled Class(nativeShebang)  (0.0 seconds, ??? lines/sec)
Wrote Class(nativeShebang) to ./nativeShebang.zsc
Hello world!

#eyeball the binary
$ zkl hexDump nativeShebang.zsc
   0: 23 21 2f 68 6f 6d 65 2f | 63 72 61 69 67 64 2f 42   #!/home/craigd/B
  16: 69 6e 2f 7a 6b 6c 0a 7a | 6b 44 00 00 28 31 2e 31   in/zkl.zkD..(1.1
  32: 00 5a 4b 4c 20 53 65 72 | 69 61 6c 69 7a 65 64 20   .ZKL Serialized 
  48: 43 6c 61 73 73 00 6e 61 | 74 69 76 65 53 68 65 62   Class.nativeSheb
  64: 61 6e 67 00 00 02 00 00 | 34 2b 61 74 74 72 69 62   ang.....4+attrib
  80: 75 74 65 73 3a 73 74 61 | 74 69 63 20 63 72 65 61   utes:static crea
  96: 74 65 52 65 74 75 72 6e | 73 53 65 6c 66 00 6e 61   teReturnsSelf.na
 112: 74 69 76 65 53 68 65 62 | 61 6e 67 00 00 00 01 00   tiveShebang.....
...

#run the binary
$ ./nativeShebang.zsc
Hello world!

#see if update works
$ touch ./nativeShebang.zkl
$ ./nativeShebang.zsc
Compiled Class(nativeShebang)  (0.0 seconds, ??? lines/sec)
Wrote Class(nativeShebang) to ./nativeShebang.zsc
Hello world!
#yep, new binary generated

```


+++
title = "Call a foreign-language function"
description = ""
date = 2019-09-30T18:03:22Z
aliases = []
[extra]
id = 4711
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}

;Task:
Show how a [[Foreign function interface|foreign language function]] can be called from the language.


As an example, consider calling functions defined in the [[C]] language. Create a string containing "Hello World!" of the string type typical to the language. Pass the string content to [[C]]'s <code>strdup</code>. The content can be copied if necessary. Get the result from <code>strdup</code> and print it using language means. Do not forget to free the result of <code>strdup</code> (allocated in the heap).


;Notes:
* It is not mandated if the [[C]] run-time library is to be loaded statically or dynamically. You are free to use either way.
* [[C++]] and [[C]] solutions can take some other language to communicate with.
* It is ''not'' mandatory to use <code>strdup</code>, especially if the foreign function interface being demonstrated makes that uninformative.


;See also:
*   [[Use another language to call a function]]





## ALGOL 68

The designers of Algol 68 made it extremely hard to incorporate code written in other languages.  To be fair, this was a long time ago when such considerations weren't thought important and one should be careful to apply Hanlon's razor.

The entry below is wildly non-portable, inefficient, violates the spirit of the specification and is just plain sick.  However, it gives the correct results with Algol 68 Genie on Linux and, I claim, meets the letter of the spec.  It also omits most of the error checking which should be present in production code.

Note that I chose a non-trivial library function because the suggested strdup() doesn't really demonstrate the technique all that well.

```algol68

BEGIN
   MODE PASSWD = STRUCT (STRING name, passwd, INT uid, gid, STRING gecos, dir, shell);
   PROC getpwnam = (STRING name) PASSWD :
   BEGIN
      FILE c source;
      create (c source, stand out channel);
      putf (c source, ($gl$,
"#include <sys/types.h>",
"#include <pwd.h>",
"#include <stdio.h>",
"main ()",
"{",
"  char name[256];",
"  scanf (""%s"", name);",
"  struct passwd *pass = getpwnam (name);",
"  if (pass == (struct passwd *) NULL) {",
"    putchar ('\n');",
"  } else {",
"    printf (""%s\n"", pass->pw_name);",
"    printf (""%s\n"", pass->pw_passwd);",
"    printf (""%d\n"", pass->pw_uid);",
"    printf (""%d\n"", pass->pw_gid);",
"    printf (""%s\n"", pass->pw_gecos);",
"    printf (""%s\n"", pass->pw_dir);",
"    printf (""%s\n"", pass->pw_shell);",
"  }",
"}"
		       ));
      STRING source name = idf (c source);
      STRING bin name = source name + ".bin";
      INT child pid = execve child ("/usr/bin/gcc",
				    ("gcc", "-x", "c", source name, "-o", bin name),
				    "");
      wait pid (child pid);
      PIPE p = execve child pipe (bin name, "Ding dong, a68g calling", "");
      put (write OF p, (name, newline));
      STRING line;
      PASSWD result;
      IF get (read OF p, (line, newline)); line = ""
      THEN
	 result := ("", "", -1, -1, "", "", "")
	 CO
         Return to sender, address unknown.
         No such number, no such zone.
	 CO
      ELSE
	 name OF result := line;
	 get (read OF p, (passwd OF result, newline));
	 get (read OF p, (uid OF result, newline));
	 get (read OF p, (gid OF result, newline));
	 get (read OF p, (gecos OF result, newline));
	 get (read OF p, (dir OF result, newline));
	 get (read OF p, (shell OF result, newline))
      FI;
      close (write OF p);			CO Sundry cleaning up. CO
      close (read OF p);
      execve child ("/bin/rm", ("rm", "-f", source name, bin name), "");
      result
   END;
   PASSWD mr root = getpwnam ("root");
   IF name OF mr root = ""
   THEN
      print (("Oh dear, we seem to be rootless.", newline))
   ELSE
      printf (($2(g,":"), 2(g(0),":"), 2(g,":"), gl$, mr root))
   FI
END

```

{{out}}

```txt

root:x:0:0:root:/root:/bin/bash

```



## 8th


```forth

\ tell 8th what the function expects:
"ZZ" "strdup" func: strdup
"VZ" "free" func: free
\ call the external funcs
"abc" dup     \ now we have two strings "abc" on the stack
strdup .s cr   \ after strdup, you'll have the new (but duplicate) string on the stack
\ the ".s" will show both strings and you can see they are different items on the stack
free   \ let the c library free the string

```


## Ada

Ada provides standard interfaces to [[C]], [[C++]], [[Fortran]] and [[Cobol]]. Other language interfaces can be provided as well, but are not mandatory. Usually it is possible to communicate to any language that supports calling conventions standard to the [[OS]] ('''cdecl''', '''stdcall''' etc).

```Ada
with Ada.Text_IO;           use Ada.Text_IO;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

procedure Test_C_Interface is
   function strdup (s1 : Char_Array) return Chars_Ptr;
   pragma Import (C, strdup, "_strdup");

   S1 : constant String := "Hello World!";
   S2 : Chars_Ptr;
begin
   S2 := strdup (To_C (S1));
   Put_Line (Value (S2));
   Free (S2);
end Test_C_Interface;
```



## Aikido

There are two ways to call a <em>native</em> function in Aikido.  The first is to write a wrapper function in C++ that is invoked from the Aikido interpreter.  In a C++ file:

```aikido>#include <aikido.h

extern "C" {       // need C linkage

// define the function using a macro defined in aikido.h
AIKIDO_NATIVE(strdup) {
    aikido::string *s = paras[0].str;
    char *p = strdup (s->c_str());
    aikido::string *result = new aikido::string(p);
    free (p);
    return result;
}

}
```


Then in the Aikido program:

```aikido
native function strdup(s)
println (strdup ("Hello World!"))
```


The second way is to use a <em>raw native</em> function.  These functions must adhere to a defined set of rules and can be called directly from the Aikido interpreter.  In the case of <code>strdup</code> we need to play a nasty trick because it returns a pointer that we need to print as a string.


```aikido
native function strdup (s)  // declare native
native function free(p)    // also need to free the result

var s = strdup ("hello world\n")
var p = s     // this is an integer type
for (;;) {
    var ch = peek (p, 1)   // read a single character
    if (ch == 0) {
        break
    }
    print (cast<char>(ch))   // print as a character
    p++
}
free (s)   // done with the memory now
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program forfunction.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szString: .asciz "Hello word\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                            @ entry of program
    push {fp,lr}                 @ saves registers

    ldr r0,iAdrszString          @ string address
    bl strdup                    @ call function C
                                 @ return new pointer
    bl affichageMess             @ display dup string
    bl free                      @ free heap

100:                             @ standard end of the program */
    mov r0, #0                   @ return code
    pop {fp,lr}                  @restaur 2 registers
    mov r7, #EXIT                @ request to exit program
    swi 0                        @ perform the system call
iAdrszString:      .int szString

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return

```


## AutoHotkey

from the documentation for dllcall:
```AutoHotkey
; Example: Calls the Windows API function "MessageBox" and report which button the user presses.

WhichButton := DllCall("MessageBox", "int", "0", "str", "Press Yes or No", "str", "Title of box", "int", 4)
MsgBox You pressed button #%WhichButton%.
```


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      SYS "LoadLibrary", "MSVCRT.DLL" TO msvcrt%
      SYS "GetProcAddress", msvcrt%, "_strdup" TO `strdup`
      SYS "GetProcAddress", msvcrt%, "free" TO `free`

      SYS `strdup`, "Hello World!" TO address%
      PRINT $$address%
      SYS `free`, address%

```



## C++

While calling C functions from C++ is generally almost trivial, <code>strdup</code> illustrates some fine point in communicating with C libraries. However, to illustrate how to generally use C functions, a C function <code>strdup1</code> is used, which is assumed to have the same interface and behaviour as strdup, but cannot be found in a standard header.

In addition, this code demonstrates a call to a FORTRAN function defined as

```cpp
FUNCTION MULTIPLY(X, Y)
DOUBLE PRECISION MULTIPLY, X, Y
```

Note that the calling convention of FORTRAN depends on the system and the used FORTRAN compiler, and sometimes even on the command line options used for the compiler; here, GNU Fortran with no options is assumed.

```cpp
#include <cstdlib>  // for C memory management
#include <string>   // for C++ strings
#include <iostream> // for output

// C functions must be defined extern "C"
extern "C" char* strdup1(char const*);

// Fortran functions must also be defined extern "C" to prevent name
// mangling; in addition, all fortran names are converted to lowercase
// and get an undescore appended. Fortran takes all arguments by
// reference, which translates to pointers in C and C++ (C++
// references generally work, too, but that may depend on the C++
// compiler)
extern "C" double multiply_(double* x, double* y);

// to simplify the use and reduce the probability of errors, a simple
// inline forwarder like this can be used:
inline double multiply(double x, double y)
{
  return multiply_(&x, &y);
}

int main()
{
  std::string msg = "The product of 3 and 5 is ";

  // call to C function (note that this should not be assigned
  // directly to a C++ string, because strdup1 allocates memory, and
  // we would leak the memory if we wouldn't save the pointer itself
  char* msg2 = strdup1(msg.c_str());

  // C strings can be directly output to std::cout, so we don't need
  // to put it back into a string to output it.
  std::cout << msg2;

  // call the FORTRAN function (through the wrapper):
  std::cout << multiply(3, 5) << std::endl;

  // since strdup1 allocates with malloc, it must be deallocated with
  // free, not delete, nor delete[], nor operator delete
  std::free(msg2);
}
```



## Clojure

{{libheader|clojure-jna}}
Since Clojure is hosted on the JVM, you can follow the same approach as the [[#Java|Java]] solution and invoke your Java class from Clojure:

```clojure
(JNIDemo/callStrdup "Hello World!")
```


Alternatively, to avoid having to create a library in native code you could use JNA and the clojure-jna library for convenience.  Here's how you can invoke ''strcmp'' from the libc shared library:

```clojure
(require '[net.n01se.clojure-jna :as jna])

(jna/invoke Integer c/strcmp "apple" "banana" )      ; returns -1

(jna/invoke Integer c/strcmp "banana" "apple" )      ; returns 1

(jna/invoke Integer c/strcmp "banana" "banana" )     ; returns 0
```



## CMake

''This code uses a deprecated feature of CMake.'' In 2014, CMake 3.0 deprecated load_command(). CMake 3.0 can run this code but shows a deprecation warning. When a future version of CMake removes load_command(), this code will stop working, and there will be no way to call C functions from CMake.

In old versions of CMake, we can write custom commands for CMake in C. This example defines CMake command div() to call [http://man7.org/linux/man-pages/man3/div.3.html C function div()]. Only works in a project, not in a <code>cmake -P</code> script. Uses more than 20 lines of CMake and 50 lines of C. There are 3 files: ''CMakeLists.txt'' in the top directory, and ''div/CMakeLists.txt'' and ''div/div-command.c'' in a subdirectory. Run it with the command <code>cmake .</code> in the top directory.

'''CMakeLists.txt'''

```cmake
cmake_minimum_required(VERSION 2.6)
project("outer project" C)

# Compile cmDIV.
try_compile(
  compiled_div                  # result variable
  ${CMAKE_BINARY_DIR}/div       # bindir
  ${CMAKE_SOURCE_DIR}/div       # srcDir
  div)                          # projectName
if(NOT compiled_div)
  message(FATAL_ERROR "Failed to compile cmDIV")
endif()

# Load cmDIV.
load_command(DIV ${CMAKE_BINARY_DIR}/div)
if(NOT CMAKE_LOADED_COMMAND_DIV)
  message(FATAL_ERROR "Failed to load cmDIV")
endif()

# Try div() command.
div(quot rem 2012 500)
message("
  2012 / 500 = ${quot}
  2012 % 500 = ${rem}
")
```


'''div/CMakeLists.txt'''

```cmake
cmake_minimum_required(VERSION 2.6)
project(div C)

# Find cmCPluginAPI.h
include_directories(${CMAKE_ROOT}/include)

# Compile cmDIV from div-command.c
add_library(cmDIV MODULE div-command.c)
```


'''div/div-command.c'''

```c
#include <cmCPluginAPI.h>
#include <stdio.h>
#include <stdlib.h>

static cmCAPI *api;

/*
 * Respond to DIV(quotient remainder numerator denominator).
 */
static int
initial_pass(void *info, void *mf, int argc, char *argv[])
{
	div_t answer;
	int count, i, j, n[2];
	char buf[512], c;

	if (argc != 4) {
		api->SetError(info, "Wrong number of arguments");
		return 0;  /* failure */
	}

	/* Parse numerator and denominator. */
	for(i = 2, j = 0; i < 4; i++, j++) {
		count = sscanf(argv[i], "%d%1s", &n[j], &c);
		if (count != 1) {
			snprintf(buf, sizeof buf,
			    "Not an integer: %s", argv[i]);
			api->SetError(info, buf);
			return 0;  /* failure */
		}
	}

	/* Call div(). */
	if (n[1] == 0) {
		api->SetError(info, "Division by zero");
		return 0;  /* failure */
	}
	answer = div(n[0], n[1]);

	/* Set variables to answer. */
	snprintf(buf, sizeof buf, "%d", answer.quot);
	api->AddDefinition(mf, argv[0], buf);
	snprintf(buf, sizeof buf, "%d", answer.rem);
	api->AddDefinition(mf, argv[1], buf);

	return 1;  /* success */
}

CM_PLUGIN_EXPORT void
DIVInit(cmLoadedCommandInfo *info)
{
	info->Name = "DIV";
	info->InitialPass = initial_pass;
	api = info->CAPI;
}
```



## COBOL

Tested with GnuCOBOL


```cobol
       identification division.
       program-id. foreign.

       data division.
       working-storage section.
       01 hello.
          05 value z"Hello, world".
       01 duplicate    usage pointer.
       01 buffer       pic x(16) based.
       01 storage      pic x(16).

       procedure division.
       call "strdup" using hello returning duplicate
           on exception
               display "error calling strdup" upon syserr
       end-call
       if duplicate equal null then
           display "strdup returned null" upon syserr
       else
           set address of buffer to duplicate
           string buffer delimited by low-value into storage
           display function trim(storage)
           call "free" using by value duplicate
               on exception
                   display "error calling free" upon syserr
       end-if
       goback.
```


{{out}}

```txt

prompt$ cobc -x foreign.cob
prompt$ ./foreign
Hello, world

```



## Common Lisp


{{libheader|CFFI}}


```lisp
CL-USER> (let* ((string "Hello World!")
                (c-string (cffi:foreign-funcall "strdup" :string string :pointer)))
           (unwind-protect (write-line (cffi:foreign-string-to-lisp c-string))
             (cffi:foreign-funcall "free" :pointer c-string :void))
           (values))
Hello World!
; No value
```



## Crystal

Crystal allows to easily interface with C functions, both from object files and shared libraries.

```ruby
@[Link("c")] # name of library that is passed to linker. Not needed as libc is linked by stdlib.
lib LibC
  fun free(ptr : Void*) : Void
  fun strdup(ptr : Char*) : Char*
end

s1 = "Hello World!"
p = LibC.strdup(s1) # returns Char* allocated by LibC
s2 = String.new(p)
LibC.free p # pointer can be freed as String.new(Char*) makes a copy of data

puts s2
```



## D


```d
import std.stdio: writeln;
import std.string: toStringz;
import std.conv: to;

extern(C) {
    char* strdup(in char* s1);
    void free(void* ptr);
}

void main() {
    // We could use char* here (as in D string literals are
    // null-terminated) but we want to comply with the "of the
    // string type typical to the language" part.
    // Note: D supports 0-values inside a string, C doesn't.
    auto input = "Hello World!";

    // Method 1 (preferred):
    //   toStringz converts D strings to null-terminated C strings.
    char* str1 = strdup(toStringz(input));

    // Method 2:
    // D strings are not null-terminated, so we append '\0'.
    // .ptr returns a pointer to the 1st element of the array,
    // just as &array[0]
    // This has to be done because D dynamic arrays are
    // represented with:  { size_t length; T* pointer; }
    char* str2 = strdup((input ~ '\0').ptr);

    // We could have just used printf here, but the task asks to
    // "print it using language means":
    writeln("str1: ", to!string(str1));
    writeln("str2: ", to!string(str2));

    free(str1);
    free(str2);
}
```

{{out}}

```txt
str1: Hello World!
str2: Hello World!
```



## Delphi


### Importing the function from a shared library

If you have the function to be called available as a shared library you just do an import of that function using the means as shown for [[Call_a_function_in_a_shared_library#Delphi|calling a function from a shared library]].


### Object Files

There is limited support for linking a function using an object file. For this to work the object file has to be in Borland Linker compatible format. Trying to use a GCC-created object file doesn't work.

The file first has to be bound to your unit:


```delphi

{$O myhello.obj}

```


The next step is to do an external declaration for the function:

```delphi

procedure Hello(S: PChar); stdcall; external;

```


Afterwards usage of the function is just as with any other function.


## Factor

If you declare a parameter as <code>c-string</code>, Factor automatically converts NULL-terminated C strings to Factor strings and back. In this case we additionally have to free the returned string, so we have to do the conversion explicitly; else the reference to the pointer would be dropped behind the scenes.

libc is already loaded, it is used by Factor elsewhere.

```factor
FUNCTION: char* strdup ( c-string s ) ;

: my-strdup ( str -- str' )
    strdup [ utf8 alien>string ] [ (free) ] bi ;
```


 ( scratchpad ) "abc" my-strdup .
 "abc"


## FBSL

Alongside its interpretative BASIC-style layer, FBSL also hosts built-in Intel-style Dynamic Assembler JIT and ANSI-C Dynamic C JIT compiler layers. BASIC, DynAsm and DynC procedures can be mixed freely to best suit the host script's intended purposes. The procedures follow their own respective syntaxes but are called in the host script in exactly the same way:
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:gray">#APPTYPE CONSOLE</span>

<span style="color:blue">PRINT</span> BasFoo(1), <span style="color:maroon">" "</span>, AsmFoo(2), <span style="color:maroon">" "</span>, CeeFoo(3)

<span style="color:blue">PAUSE</span>

<span style="color:red">FUNCTION</span> BasFoo(parm <span style="color:blueviolet">AS INTEGER</span>) <span style="color:blueviolet">AS INTEGER</span>
:<span style="color:red">RETURN</span> parm
<span style="color:red">END FUNCTION</span>

<span style="color:red">DYNASM</span> AsmFoo(parm <span style="color:blueviolet">AS INTEGER</span>) <span style="color:blueviolet">AS INTEGER</span>
:<span style="color:blue">ENTER</span> 0, 0
:<span style="color:blue">MOV</span> <span style="color:red">EAX</span>, parm
:<span style="color:blue">LEAVE</span>
:<span style="color:blue">RET</span>
<span style="color:red">END DYNASM</span>

<span style="color:red">DYNC</span> CeeFoo(parm <span style="color:blueviolet">AS INTEGER</span>) <span style="color:blueviolet">AS INTEGER</span>
:<span style="color:blue">int</span> <span style="color:red">main</span>(<span style="color:blue">int</span> parm)
:{
::<span style="color:blue">return</span> parm;
:}
<span style="color:red">END DYNC</span>
</code></b></div>

Output:
<div style="overflow:auto;white-space:nowrap;background-color:black;border:1px dashed rgb(167, 215, 249); padding:12px"><b><code>
<span style="color:white">1 2 3

Press any key to continue...</span>
</code></b></div>

FBSL has been specifically designed to cooperate with dynamic-link libraries. Such Windows system libraries as Kernel32.dll, User32.dll, and Gdi32.dll (also msvcrt.dll for the Dynamic C layer) are mapped into the process memory at app start so that over 2,300 functions of their API are always ready for use in an FBSL script as if they were native to FBSL's own namespace.

Other DLL's can be mapped into the script namespace either on a per-function basis (BASIC only) or entirely in one swoop, e.g.:
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:green">' all BASIC names are case-insensitive</span>

<span style="color:gray">#DLLDECLARE</span> Gdiplus(GdiplusStartup <span style="color:blueviolet">AS</span> SpawnIt, GdipLoadImageFromFile, GdiplusShutdown <span style="color:blueviolet">AS</span> KillIt)

<span style="color:gray">#DLLIMPORTS</span> OpenGL32
</code></b></div>

<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:green">; all DynAsm names are case-insensitive</span>

<span style="color:blueviolet">INCLUDELIB</span> OpenGL32
</code></b></div>

<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:green">// DLL names in DynC are case-insensitive</span>

<span style="color:gray">#pragma comment</span>(<span style="color:gray">lib</span>, <span style="color:maroon">"OpenGL32"</span>)
</code></b></div>
whereby you may start to use the three GDI+ function names (either directly or through arbitrary aliases as shown above), as well as the entire namespace of OpenGL functions, in your script.

Alternatively, a dynamic call to a DLL function can be made in FBSL's BASIC regardless of whether the DLL is already loaded or not:
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:blue">APICALL</span>(szFooName, szDllName[, parameters...])
</code></b></div>

Lastly, there are at least two methods in FBSL's BASIC to call in-memory machine code routines given their entry points:
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:blue">CALLABSOLUTE</span>(varByteBuffer[, parm1[, parm2[, parm3[, parm4]]]]) <span style="color:green">' varByteBuffer stores machine code bytes</span>

<span style="color:blue">FUNCALL</span>(varEntryPoint[, parameters...]) <span style="color:green">' varEntryPoint stores function call address</span>
</code></b></div>

All FBSL function calls support Integer, Quad, Single, Double and String parameters and returns. FBSL supports natively ASCIIZ strings only. Unicode strings in FBSL's BASIC require explicit ANSITOWIDE()/WIDETOANSI() transforms.

FBSL features a built-in stack balancing mechanism which eliminates stack corruption regardless of whether the API calls are using STDCALL or CDECL calling conventions. Please note that FBSL's BASIC and DynAsm '''do not''' make use of forward function declarations or header files.


## Forth

{{works with|GNU Forth|0.7.0}}

Every version of GNU Forth has experimented with a different means to do C foreign function calls. The current implementation resolves various incompatibilities which had plagued earlier mechanisms by parsing C header files and using the host's native toolchain (i.e. gcc and ld) to generate thunks.


```forth
c-library cstrings

\c #include <string.h>
c-function strdup strdup a -- a ( c-string -- duped string )
c-function strlen strlen a -- n ( c-string -- length )

end-c-library

\ convenience function (not used here)
: c-string ( addr u -- addr' )
    tuck  pad swap move  pad + 0 swap c!  pad ;

create test s" testing" mem, 0 c,

test strdup value duped

test .
test 7 type		\ testing
cr
duped .                 \ different address
duped dup strlen type   \ testing

duped free throw	\ gforth ALLOCATE and FREE map directly to C's malloc() and free()
```



## Fortran

Since Fortran 2003, the standard provides the ISO_C_BINDING module to help interface with C programs. Before this, compiler vendors often provided nonstandard extensions to do this. Even with this new facility, some features, such as calling a STDCALL function on Windows, need some nonstandard extension.

For instance, with GNU Fortran one would write "!GNU$ ATTRIBUTES STDCALL :: f" to declare ''f'' as a STDCALL function. This can be used in a function body, for instance a DLL export that will be used by a program that expects a STDCALL function (say, a VBA program). Or it can be used in an interface block to make bindings to Windows API from Fortran.

For other languages, it ''may'' happen that one needs an intermediate wrapper to be able to call a foreign function. However, anything that can be called from C could likely be called from Fortran using ISO_C_BINDING.

Here is an example using the ISO_C_BINDING standard module to link against the C API functions ''strdup'', ''free'' and ''puts''. The program will print two copies of the string ''"Hello, World!"'' using the ''puts'' function. One copy is obtained from ''strdup'', then released with ''free''. The C bindings are placed in an interface module to simplify reuse. The addresses of the two copies are also printed.


```fortran
module c_api
    use iso_c_binding
    implicit none

    interface
        function strdup(ptr) bind(C)
            import c_ptr
            type(c_ptr), value :: ptr
            type(c_ptr) :: strdup
        end function
    end interface

    interface
        subroutine free(ptr) bind(C)
            import c_ptr
            type(c_ptr), value :: ptr
        end subroutine
    end interface

    interface
        function puts(ptr) bind(C)
            import c_ptr, c_int
            type(c_ptr), value :: ptr
            integer(c_int) :: puts
        end function
    end interface
end module

program c_example
    use c_api
    implicit none

    character(20), target :: str = "Hello, World!" // c_null_char
    type(c_ptr) :: ptr
    integer(c_int) :: res

    ptr = strdup(c_loc(str))

    res = puts(c_loc(str))
    res = puts(ptr)

    print *, transfer(c_loc(str), 0_c_intptr_t), &
             transfer(ptr, 0_c_intptr_t)
    call free(ptr)
end program
```



## FreeBASIC

Normally it's an easy matter to call a function in the C Standard Library, statically, from FreeBASIC.
However, 'strdup' isn't in the Standard Library so instead we will call the version in the Windows Shell, dynamically.
As this uses LocalAlloc in kernel32.dll internally to allocate memory for the duplicated string, we need to call
LocalFree to free this memory using the pointer returned by strdup.

```freebasic
' FB 1.05.0 Win64

'Using StrDup function in Shlwapi.dll
Dim As Any Ptr library = DyLibLoad("Shlwapi")
Dim strdup As Function (ByVal As Const ZString Ptr) As ZString Ptr
strdup = DyLibSymbol(library, "StrDupA")

'Using LocalFree function in kernel32.dll
Dim As Any Ptr library2 = DyLibLoad("kernel32")
Dim localfree As Function (ByVal As Any Ptr) As Any Ptr
localfree = DyLibSymbol(library2, "LocalFree")

Dim As ZString * 10 z = "duplicate"  '' 10 characters including final zero byte
Dim As Zstring Ptr pcz = strdup(@z)  '' pointer to the duplicate string
Print *pcz                           '' print duplicate string by dereferencing pointer
localfree(pcz)                       '' free the memory which StrDup allocated internally
pcz = 0                              '' set pointer to null
DyLibFree(library)                   '' unload first dll
DyLibFree(library2)                  '' unload second fll
End
```


{{out}}

```txt

duplicate

```



## Go

Using cgo, part of the standard Go command set.

```go
package main

// #include <string.h>
// #include <stdlib.h>
import "C"
import (
    "fmt"
    "unsafe"
)

func main() {
    // a go string
    go1 := "hello C"
    // allocate in C and convert from Go representation to C representation
    c1 := C.CString(go1)
    // go string can now be garbage collected
    go1 = ""
    // strdup, per task. this calls the function in the C library.
    c2 := C.strdup(c1)
    // free the source C string.  again, this is free() in the C library.
    C.free(unsafe.Pointer(c1))
    // create a new Go string from the C copy
    go2 := C.GoString(c2)
    // free the C copy
    C.free(unsafe.Pointer(c2))
    // demonstrate we have string contents intact
    fmt.Println(go2)
}
```

Output:

```txt

hello C

```



## Haskell



```Haskell
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign (free)
import Foreign.C.String (CString, withCString, peekCString)

-- import the strdup function itself
-- the "unsafe" means "assume this foreign function never calls back into Haskell and avoid extra bookkeeping accordingly"
foreign import ccall unsafe "string.h strdup" strdup :: CString -> IO CString

testC = withCString "Hello World!" -- marshall the Haskell string "Hello World!" into a C string...
        (\s -> -- ... and name it s
         do s2 <- strdup s
            s2_hs <- peekCString s2 -- marshall the C string called s2 into a Haskell string named s2_hs
            putStrLn s2_hs
            free s2) -- s is automatically freed by withCString once done
```


==Icon and {{header|Unicon}}==

(This probably also works for Icon, but only tested on Unicon, and on Linux.)

The first step is to create a shared library, to wrap the target C functions and do type conversions on the input and returned values.  The arguments to the wrapper functions form a list, and this list must be unpacked to retrieve the arguments to send to the target function.  To get at <code>strdup</code> and <code>strcat</code> we would have:


```C

#include <string.h>
#include "icall.h"  // a header routine from the Unicon sources - provides helpful type-conversion macros

int strdup_wrapper (int argc, descriptor *argv)
{
  ArgString (1); // check that the first argument is a string

  RetString (strdup (StringVal(argv[1]))); // call strdup, convert and return result
}

// and strcat, for a result that does not equal the input
int strcat_wrapper (int argc, descriptor *argv)
{
  ArgString (1);
  ArgString (2);
  char * result = strcat (StringVal(argv[1]), StringVal(argv[2]));
  RetString (result);
}

```


Then the Unicon program must 'access' the function in the shared library: the important step is 'loadfunc' which accesses the named function in the shared library.  After that, the C function can be called from within a program:


```Unicon

$define LIB "libstrdup-wrapper.so"

# the unicon wrapper to access the C function
procedure strdup (str)
  static f
  initial {
    f := loadfunc (LIB, "strdup_wrapper") // pick out the wrapped function from the shared library
  }
  return f(str) // call the wrapped function
end

procedure strcat (str1, str2)
  static f
  initial {
    f := loadfunc (LIB, "strcat_wrapper")
  }
  return f(str1, str2)
end

procedure main ()
  write (strdup ("abc"))
  write (strcat ("abc", "def"))
end

```


Output:

```txt

$ ./str-test
abc
abcdef

```



## J


Here is a windows specific implementation (for relatively recent versions of windows):


```J
require 'dll'
strdup=: 'msvcrt.dll _strdup >x *' cd <
free=: 'msvcrt.dll free n x' cd <
getstr=: free ] memr@,&0 _1
```


With these definitions:

```J
   getstr@strdup 'Hello World!'
Hello World!
```


Portability is possible, but often irrelevant for a task of this sort.  To make this work with a different OS, you would need to use the appropriate file name for libc for the os in question.  For example, on linux, replace msvcrt.dll with /lib/libc.so.6 (or whichever version of libc you are using).

See also: [http://www.jsoftware.com/help/user/call_procedure.htm J's documentation]


## Java

Java uses JNI to call other languages directly. Because it is a managed language, a "shim" layer needs to be created when dealing with things outside of the managed environment.

First, we start with the java source code:

'''JNIDemo.java'''

```java
public class JNIDemo
{
  static
  {  System.loadLibrary("JNIDemo");  }

  public static void main(String[] args)
  {
    System.out.println(callStrdup("Hello World!"));
  }

  private static native String callStrdup(String s);
}
```


Two things to note: First, the "native" stub which will be linked with a native library, and second, the call to System.loadLibrary to actually do the linking at runtime. The class must then be compiled without the native library.

Next, a C-style ".h" file needs to be created from the class. This can be done by running javah on our compiled class:


```txt
javah -jni JNIDemo
```


The generated file, '''JNIDemo.h''':

```c
/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class JNIDemo */

#ifndef _Included_JNIDemo
#define _Included_JNIDemo
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     JNIDemo
 * Method:    callStrdup
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_JNIDemo_callStrdup
  (JNIEnv *, jclass, jstring);

#ifdef __cplusplus
}
#endif
#endif
```


Next, the C code which utilizes JNI to bridge between the managed and unmanaged environments. It should include the "h" file, and implement the exported function declared in that file. The specifics of writing JNI code are beyond the scope of this task.

'''JNIDemo.c'''

```c
#include "string.h"
#include "JNIDemo.h"

void throwByName(JNIEnv* env, const char* className, const char* msg)
{
  jclass exceptionClass = (*env)->FindClass(env, className);
  if (exceptionClass != NULL)
  {
    (*env)->ThrowNew(env, exceptionClass, msg);
    (*env)->DeleteLocalRef(env, exceptionClass);
  }
  return;
}

JNIEXPORT jstring JNICALL Java_JNIDemo_callStrdup(JNIEnv *env, jclass cls, jstring s)
{
  const jbyte* utf8String;
  char* dupe;
  jstring dupeString;

  if (s == NULL)
  {
    throwByName(env, "java/lang/NullPointerException", "String is null");
    return NULL;
  }

  // Convert from UTF-16 to UTF-8 (C-style)
  utf8String = (*env)->GetStringUTFChars(env, s, NULL);

  // Duplicate
  dupe = strdup(utf8String);

  // Free the UTF-8 string back to the JVM
  (*env)->ReleaseStringUTFChars(env, s, utf8String);

  // Convert the duplicate string from strdup to a Java String
  dupeString = (*env)->NewStringUTF(env, dupe);

  // Free the duplicate c-string back to the C runtime heap
  free(dupe);

  return dupeString;
}

```


In a Windows environment, a dll by the same name should be created ("JNIDemo.dll"). In a Linux environment, a shared object marked executable and with a name preceded by "lib" should be created (in this case, "libJNIDemo.so"). Your compiler will need to know the location of "jni.h", which is in the "include" directory of the JDK. Linux may also need includes that are in the "include/linux" directory. Linux example using gcc:


```txt
gcc -shared -fPIC -I/usr/jdk/include -I/usr/jdk/include/linux -o libJNIDemo.so JNIDemo.c
```


And finally, to run the program, the library must be in the runtime's library path. If the directory in which the library resides is not in this path, it can be explicitly specified using the "-D" option (e.g. "-Djava.library.path=." would specify the current directory as the library path).


```txt
java -Djava.library.path=. JNIDemo
Hello World!

```



## Julia

{{works with|Julia|0.6}}

Julia has a built-in keyword <code>ccall</code> to call external C-like functions.  For example:

```julia
p = ccall(:strdup, Ptr{Cuchar}, (Ptr{Cuchar},), "Hello world")
@show unsafe_string(p) # "Hello world"
ccall(:free, Void, (Ptr{Cuchar},), p)
```


'''PyCall''', [https://github.com/JuliaPy/PyCall.jl source]:

```julia
using PyCall
@pyimport math
@show math.cos(1) # 0.5403023058681398
```



## Kotlin

{{Works with|Ubuntu|14.04}}

```scala
// Kotlin Native v0.2

import kotlinx.cinterop.*
import string.*

fun main(args: Array<String>) {
    val hw = strdup ("Hello World!")!!.toKString()
    println(hw)
}
```


{{out}}

```txt

Hello World!

```



## LabVIEW

Use Connectivity >> Libraries & Executables >> Call Library Function Node to call an external .dll file. This example uses the WinAPI's MessageBoxA function.<br/>{{VI snippet}}<br/>
[[File:LabVIEW Call a foreign-language function.png]]


## Lisaac

Use backtick notation (`...`) for referencing foreign language (C) features.

```Lisaac
Section Header

+ name := TEST_C_INTERFACE;

// this will be inserted in front of the program
- external := `#include <string.h>`;

Section Public

- main <- (
  + s : STRING_CONSTANT;
  + p : NATIVE_ARRAY[CHARACTER];

  s := "Hello World!";
  p := s.to_external;
  // this will be inserted in-place
  // use `expr`:type to tell Lisaac what's the type of the external expression
  p := `strdup(@p)` : NATIVE_ARRAY[CHARACTER];
  s.print;
  '='.print;
  p.println;
  // this will also be inserted in-place, expression type disregarded
  `free(@p)`;
);
```



## Lua


Using the [http://luajit.org/ext_ffi.html FFI library] available in [http://luajit.org/ LuaJIT]:


```lua
local ffi = require("ffi")
ffi.cdef[[
char * strndup(const char * s, size_t n);
int strlen(const char *s);
]]

local s1 = "Hello, world!"
print("Original: " .. s1)
local s_s1 = ffi.C.strlen(s1)
print("strlen: " .. s_s1)

local s2 = ffi.string(ffi.C.strndup(s1, s_s1), s_s1)
print("Copy: " .. s2)
print("strlen: " .. ffi.C.strlen(s2))

```



## Luck


Luck supports interfacing with most C libraries out of the box:


```luck
import "stdio.h";;
import "string.h";;

let s1:string = "Hello World!";;
let s2:char* = strdup(cstring(s1));;
puts(s2);;
free(s2 as void*)
```



## M2000 Interpreter


###  Call C Functions from Dll

There is a difference between Windows and Wine implementation of _strdump and swprintf.
Value of '''a''' has to hold chars to read from sBuf$ as returned from msvcrt.swprintf, but this can't work in Ubuntu using Wine and in Windows as expected, so we can use '''LeftPart$(string, string as delimiter sign not included as result)'''

```M2000 Interpreter


Module CheckCCall {
      mybuf$=string$(chr$(0), 1000)
      a$="Hello There 12345"+Chr$(0)
      Print Len(a$)
      Buffer Clear Mem as Byte*Len(a$)
      \\ copy to Mem the converted a$ (from Utf-16Le to ANSI)
      Return Mem, 0:=str$(a$)

      Declare  MyStrDup Lib C "msvcrt._strdup" { Long Ptr}
      Declare  MyFree Lib C "msvcrt.free" { Long Ptr}
      \\ see & means by reference
      \\ ... means any number of arguments
      Declare  MyPrintStr Lib C "msvcrt.swprintf" { &sBuf$,  sFmt$, long Z }

      \\ Now we use address Mem(0) as pointer (passing by value)
      Long Z=MyStrDup(Mem(0))
      a=MyPrintStr(&myBuf$, "%s", Z)
      Print MyFree(Z), a
      Print LeftPart$(chr$(mybuf$), chr$(0))
}
CheckCCall

```


Output:
Hello There



### Call VbScript



```M2000 Interpreter

Module Checkit {
      Global a()
      mm=10
      Module CallFromVb {
            \\ Number get first parameter is numeric else error
            Print Number
      }
      Module Global CallFromVbGlobal {
            Read X()
            X(0)++
            a()=X()
            Print "ok"
      }
      Declare Global vs "MSScriptControl.ScriptControl"
      Declare Alfa Module
      Print Type$(Alfa)  \\ name is CallBack2
      With vs, "Language","Vbscript", "AllowUI", true, "SitehWnd",  hwnd
      Method vs, "Reset"
      Method vs, "AddObject", "__global__",  Alfa, true
      Method vs, "AddCode", {
            ' This is VBScript code
            dim M(9), k   ' 0 to 9, so 10 items
            Sub main()
                  CallModule "CallFromVb", 1000
                  M(0)=1000
                  CallGlobal "CallFromVbGlobal", M
                  ExecuteStatement "Print a(0)"
                  k=me.Eval("a(0)")
                  CallModule "CallFromVb", k
                  ' use Let to assign a number to variable
                  ExecuteStatement "let mm=12345"
                  k=me.Eval("mm")
                  CallModule "CallFromVb", k
                  CallModule "CallFromVb", M(0)
            End Sub
      }
      Method vs, "run", "main"
      Declare vs nothing
      If error then print error$
      Print Len(a())
      Print a()
}
CheckIt

```



### Call Javascript


```M2000 Interpreter

Module CheckJavaScript {
      Clear
      Module ok {
            if match("S") then {
                  read m$
                  print "ok",  m$
            } else {
                  read m
                  print "ok",  m
            }
      }
      Declare vs "MSScriptControl.ScriptControl"
      Declare Alfa Module
      Print Type$(Alfa)
      With vs, "Language","Jscript", "AllowUI", true
      Method vs, "Reset"
      Print Type$(Alfa)
      Method vs, "AddObject", "M2000",  Alfa
      Inventory alfa1=1,2,3,4:="Ok"
      If exist(alfa1,4) then print "Ok..."
      Print type$(alfa1)
      Method vs, "AddObject", "Inventory", alfa1
      A=(1,2,3,4,"Hello")
      Method vs, "AddObject", "Arr", A
      Method vs, "ExecuteStatement", {
            M2000.AddExecCode("Function BB {=1234 **number} : k=2");
            M=M2000.ExecuteStatement("Print 1234, BB(k)");
                  // wait a key
            M2000.AddExecCode("aa$=key$");
            var m=[10,10+5,20];
            M2000.CallModule("ok" , Inventory.count) ;
            n=Inventory.Find("4");
            Inventory.Value="Not Ok"
            M2000.CallModule("ok" ,Inventory.Value) ;
            M2000.CallModule("ok" ,Arr.Count)
            Arr.item(4)="George"
            Arr.item(1)++;
            M2000.CallModule("ok" ,Arr.item(4))
      }
      Print Alfa1$(4)  '' Not Ok.
      Print Array$(A, 1) ' 3
      Print Array$(A, 4) ' George
      Modules ?
      \\ BB() and K created from javascript
      Print BB(k)
      Method vs, "eval", {"hello there"} as X$
      Print X$
      Method vs, "eval", {"hello there too"} as X$
      Print X$
      List ' print all variables
      Declare vs Nothing
}
CheckJavaScript

```



===Call A System Function (Win32)===

```M2000 Interpreter

Declare MessageBox Lib "user32.MessageBoxW" {long alfa, lptext$, lpcaption$, long type}
Print MessageBox(Hwnd, "HELLO THERE", "GEORGE", 2)
Remove "user32"

```


===Make, use and remove a C Dll at runtime===
H C dll to produce an array of primes. We can

```M2000 Interpreter

Module checkit {
      Static DisplayOnce=0
      N=100000
      Read ? N
      Form 60
      Pen 14
      Background { Cls 5}
      Cls 5
      \\ use f1 do unload lib - because only New statemend unload it
      FKEY 1,"save ctst1:new:load ctst1"
      \\ We use a function as string container, because c code can easy color decorated in M2000.
      Function ccode {
            long primes(long a[], long b)
            {
                  long k=2;
                  long k2,d=2, l, i;
                  k2=k*k;
                  if (b>2)
                  {
                        if (k2<b)
                        {
                            do {
                                    for (l=k2; l<=b; l+=k)
                                          a[l]--;
                                    k++;
                                    while (a[k])
                                          k++;
                                    k2=k*k;
                              } while (k2<=b);
                        }
                         for (i=2;i<=b;i++)
                         {
                              if (a[i]==0)
                              {
                                    a[d]=i ; d++ ;
                              }
                         }
                  }
                  else {
                              if (b>1)
                                 {
                                      if (b>2)
                                       {
                                             d=2; a[0]=2; a[1]=3 ;
                                       }
                                       else {
                                             d=1; a[0]=2;
                                       }
                                    }
                  }
                  a[b+1]=d;
                  return 0;
            }
      }
      \\ extract code. &functionname() is a string with the code inside "{ }"
      \\ a reference to function actual is code of function in m2000
      \\ using Document object we have an easy way to drop paragraphs
      document code$=Mid$(&ccode(), 2, len(&ccode())-2)
      \\ remove 1st line two times \\ one line for an edit information from interpreter
      \\ paragraph$(code$, 1) export paragraph 1st,using  third parameter -1 means delete after export.
      drop$=paragraph$(code$,1,-1)+paragraph$(code$,1,-1)
      If DisplayOnce Else {
            Report 2, "c code for primes"
            Report code$ \\ report stop after 3/4 of screen lines use. Press spacebar or mouse button to continue
            DisplayOnce++
      }

      \\ dos "del c:\MyName.*", 200;

      If not exist("c:\MyName.dll") then {
            Report 2, "Now we have to make a dll"
            Rem : Load Make \\ we can use a Make.gsb in current folder - this is the user folder for now
            Module MAKE ( fname$, code$, timeout ) {
                  if timeout<1000 then timeout=1000
                  If left$(fname$,2)="My" Else Error "Not proper name - use 'My' as first two letters"
                  Print "Delete old files"
                  try { remove "c:\MyName" }
                  Dos "del  c:\"+fname$+".*", timeout;
                  Print "Save c file"
                  Open "c:\"+fname$+".c" for output as F \\ use of non unicode output
                  Print #F, code$
                  Close #F
                  \\ use these two lines for opening dos console and return to M2000 command line
                  rem : Dos "cd c:\ && gcc -c -DBUILD_DLL "+fname$+".c"
                  rem : Error "Check for errors"
                  \\ by default we give a time to process dos command and then continue
                  Print "make object file"
                  dos "cd c:\ && gcc -c -DBUILD_DLL "+fname$+".c" , timeout;
                  if exist("c:\"+fname$+".o") then {
                        Print "make dll"
                        dos "cd c:\ && gcc -shared -o "+fname$+".dll "+fname$+".o -Wl,--out-implib,libmessage.a", timeout;
                  } else Error "No object file - Error"
                  if not exist("c:\"+fname$+".dll") then Error "No dll - Error"
            }
            Make "MyName", code$, 1000
      }
      Declare primes lib c "c:\MyName.primes" {long c, long d} \\ c after lib mean CDecl call
      \\ So now we can check error
      \\ make a Buffer (add two more longs for any purpose)
      Buffer Clear A as Long*(N+2) \\ so A(0) is base address, of an array of 100002 long (unsign for M2000).
      \\ profiler enable a timecount
      profiler
      Call primes(A(0), N)
      m=timecount
      total=Eval(A,N+1)-2
      Clear Yes, No
      Print "Press Y or N to display or not the primes"
      Repeat {
            Yes=keypress(89) : No=Keypress(78)
            wait 10
      } Until Yes or No
      If Yes then {
            Form 80,50
            Refresh
            For i=2 to total+1
                  Print Eval(A,i),
            next i
            Print
      }
      Print format$("Compute {0} primes in range 1 to {1}, in msec:{2:3}", total, N, m)
      \\ unload dll, we have to use exactly the same name, as we  use it  in declare except for last chars ".dll"
      remove "c:\MyName"
}
checkit


```



## Maple

We can call strdup, as requested, in the following way

```Maple>
 strdup := define_external( strdup, s::string, RETURN::string, LIB = "/lib/libc.so.6" ):
> strdup( "foo" );
                                 "foo"

```

However, this doesn't make a lot of sense in Maple, since there can be only one copy of any Maple string in memory.  Moreover, I don't see any easy way to free the memory allocated by strdup.  A more sensible example for Maple follows.  (It might be sensible if you wanted to compare your system library version of sin with the one built-in to Maple, for instance.)

```Maple>
 csin := define_external( sin, s::float[8], RETURN::float[8], LIB = "libm.so" );
csin := proc(s::numeric)
option call_external, define_external(sin, s::float[8],
RETURN::float[8], LIB = "libm.so");
    call_external(
    Array(1..8, [...], datatype = integer[4], readonly), false,
    args)
end proc

> csin( evalf( Pi / 2 ) );
                                   1.
```



## Mathematica

This works on windows and on linux/mac (through Mono)

```Mathematica
Needs["NETLink`"];
externalstrdup = DefineDLLFunction["_strdup", "msvcrt.dll", "string", {"string"}];
Print["Duplicate: ", externalstrdup["Hello world!"]]
```

output

```txt
Duplicate: Hello world!
```



## Maxima


```maxima
/* Maxima is written in Lisp and can call Lisp functions.
Use load("funcs.lisp"), or inside Maxima: */

to_lisp();
> (defun $f (a b) (+ a b))
> (to-maxima)

f(5, 6);
11
```


## Mercury


Mercury is designed to interact sensibly with foreign code, even while keeping itself as pure and as safe as is possible in such circumstances.  Here is an example of calling C's strdup() function from within Mercury:


```mercury
:- module test_ffi.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

% The actual FFI code begins here.
:- pragma foreign_decl("C", "#include <string.h>").

:- func strdup(string::in) = (string::out) is det.
:- pragma foreign_proc("C", strdup(S::in) = (SD::out),
                       [will_not_call_mercury, not_thread_safe, promise_pure],
                       "SD = strdup(S);").
% The actual FFI code ends here.

main(!IO) :-
  io.write_string(strdup("Hello, worlds!\n"), !IO).

:- end_module test_ffi.
```


Only the lines wrapped in comments matter for this.  The rest is an application skeleton so this can be compiled and tested.

The first pragma, foreign_decl, inserts C code into the output of the compiler.  Here the C function's type is declared.  Other things that can be put in this pragma include globally-accessed macros, function declarations, variable declarations, etc.

After this the Mercury strdup/1 function itself is declared.  For purposes of exposition it has been declared fully with types and modes.  The modes, however, are redundant since by default functions in Mercury have all input parameters and an output return value.  Also, the determinism is declared which is again redundant.  By default Mercury functions are deterministic.  That line could easily have been written thusly instead:


```mercury
:- func strdup(string) = string.
```


The next block of code is the foreign_proc pragma declaration.  In this declaration the language ("C") is declared, the footprint of the function is again provided, this time with variable names and modes but without the determinism, a set of properties is declared and the actual C code to be executed is provided.  This last piece is trivial, but the properties themselves are worth looking more closely at.

Flagging appropriate properties to foreign language code is vital to the efficient and safe execution of foreign functions.  Here we are saying that the foreign code will not be calling back in to the Mercury runtime (will_not_call_mercury), should not be called in parallel (not_thread_safe) and that the C function is "pure" and has no (visible) side effects (promise_pure).  Each of these has implications for efficiency and safety; the Mercury compiler will generate the best code possible within the properties' provided constraints.

Of note is that '''no separate C source file needs to be provided'''.  The compiler takes care of putting in all the required boilerplate code necessary to conform to the specifications provided.  The resulting code can be treated as much a part of the program as any native Mercury code would be: types, modes, determinism, purity, etc. all managed similarly.

=={{header|Modula-2}}==
The first file (Vga.c) creates the function prototypes.

```c
#include <vga.h>

int   Initialize (void)
{
   if ( vga_init () == 0 )
     return 1;
   else
     return 0;
}

void  SetMode (int newmode)
{
   vga_setmode (newmode);
}

int   GetMode (void)
{
   return vga_getcurrentmode ();
}

int   MaxWidth (void)
{
   return vga_getxdim ();
}

int   MaxHeight (void)
{
   return vga_getydim ();
}

void  Clear (void)
{
   vga_clear ();
}
void  SetColour (int colour)
{
   vga_setcolor (colour);
}

void  SetEGAcolour (int colour)
{
   vga_setegacolor (colour);
}

void  SetRGB (int red, int green, int blue)
{
   vga_setrgbcolor (red, green, blue);
}

void   DrawLine (int x0, int y0, int dx, int dy)
{
   vga_drawline (x0, y0, x0 + dx, y0 + dy);
}

void   Plot (int x, int y)
{
   vga_drawpixel (x, y);
}

int    ThisColour (int x, int y)
{
   return vga_getpixel (x, y);
}

void   GetKey (char *ch)
{
   *ch = vga_getkey ();
}
```

The next file is the definition module, but in this context it is called a '''FOREIGN MODULE'''.

```modula2
FOREIGN MODULE Vga;

TYPE    EGAcolour  = (black, blue, green, cyan, red,    pink,  brown, white,
                       GREY, BLUE, GREEN, CYAN, RED, MAGENTA, YELLOW, WHITE);

PROCEDURE Initialize () : BOOLEAN;

PROCEDURE MaxWidth () : CARDINAL;

PROCEDURE MaxHeight () : CARDINAL;

PROCEDURE Clear;

PROCEDURE SetColour (colour : CARDINAL);

PROCEDURE SetEGAcolour (colour : CARDINAL);

PROCEDURE SetRGB (red, green, blue : CARDINAL);

PROCEDURE DrawLine (x0, y0, dx, dy : CARDINAL);

PROCEDURE Plot (x, y : CARDINAL);

PROCEDURE ThisColour (x, y : CARDINAL) : CARDINAL;

PROCEDURE SetMode (newmode : CARDINAL);

PROCEDURE GetMode () : CARDINAL;

PROCEDURE GetKey (VAR ch : CHAR);

END Vga.
```

The third file is an example program.

```modula2
MODULE svg01;

FROM  InOut      IMPORT  Read, Write, WriteBf, WriteString;

IMPORT Vga;

VAR   OldMode, x, y             : CARDINAL;
      ch                        : CHAR;

BEGIN
   IF  Vga.Initialize () = FALSE  THEN
      WriteString ('Could not start SVGAlib libraries. Aborting...');
      WriteBf;
      HALT
   END;
   OldMode := Vga.GetMode ();
   Vga.SetMode (4);
   Vga.SetColour (14);
   Vga.Clear ();
   Vga.SetColour (10);
   FOR y := 125 TO 175 DO
      FOR x := 100 TO 500 DO
         Vga.Plot (x, y)
      END
   END;
   LOOP
      Read (ch);
      IF  ch = 'X'  THEN  EXIT  END
   END;
   Vga.SetMode (OldMode);
   Write (ch);
   WriteBf;
END svg01.
```


=={{header|Modula-3}}==
Modula-3 provides many predefined interfaces to C files.  Here we use <tt>Cstring</tt> which uses C string functions.  Note we have to convert strings of type <tt>TEXT</tt> into C strings (NULL terminated character arrays).  Also note the code requires the <tt>UNSAFE</tt> keyword because it interfaces with C (which is unsafe).

```modula3
UNSAFE MODULE Foreign EXPORTS Main;

IMPORT IO, Ctypes, Cstring, M3toC;

VAR string1, string2: Ctypes.const_char_star;

BEGIN
  string1 := M3toC.CopyTtoS("Foobar");
  string2 := M3toC.CopyTtoS("Foobar2");
  IF Cstring.strcmp(string1, string2) = 0 THEN
    IO.Put("string1 = string2\n");
  ELSE
    IO.Put("string1 # string2\n");
  END;
  M3toC.FreeCopiedS(string1);
  M3toC.FreeCopiedS(string2);
END Foreign.
```

Output:

```txt

string1 # string2

```



## Never

Never includes libffi for access to foreign functions, but currently only supports very basic types, int, float, string.  ''strdup'' will work, but the ''voidness'' of ''free'' is not yet supported.  This solution uses some of the Math functions in libm instead.


```fsharp
extern "libm.so.6" func sinhf(x : float) -> float
extern "libm.so.6" func coshf(x : float) -> float
extern "libm.so.6" func powf(base : float, exp : float) -> float
extern "libm.so.6" func atanf(x : float) -> float

func main() -> int
{
    var v1 = sinhf(1.0);
    var v2 = coshf(1.0);
    var v3 = powf(10.0, 2.0);
    var pi = 4.0 * atanf(1.0);

    printf(v1);
    printf(v2);
    printf(v3);
    printf(pi);
    printf(sinhf(1.0));

    0
}
```



{{out}}

```txt
prompt$ never -f callffi.nev
1.18
1.54
100.00
3.14
1.18

```



## NewLISP

newLISP has two FFI APIs. The simple API needs no type specifiers but is limited to integers and pointers.
The extended API can specify types for return values and parameters and can also be used for floats and structs.

```NewLISP
; simple FFI interface on Mac OSX
(import "libc.dylib" "strdup")
(println (get-string (strdup "hello world")))

; or extended FFI interface on Mac OSX
(import "libc.dylib" "strdup" "char*" "char*")
(println (strdup "hello world"))

```



## Nim

Since Nim compiles to C by default, this task is easily done:


```nim
proc strcmp(a, b: cstring): cint {.importc: "strcmp", nodecl.}
echo strcmp("abc", "def")
echo strcmp("hello", "hello")

proc printf(formatstr: cstring) {.header: "<stdio.h>", varargs.}

var x = "foo"
printf("Hello %d %s!\n", 12, x)
```



## OCaml



### Outline of what is linked against

For the hypothetical [[C]] library that contains functions described by a header file with this in:

```ocaml
void myfunc_a();
float myfunc_b(int, float);
char *myfunc_c(int *, int);
```


The header file is named "<tt>mylib.h</tt>", and linked against the library with <tt>-lmylib</tt> and compiled with <tt>-I/usr/include/mylib</tt>.

### Required files

Here are provided all the files, including a Makefile.

====file "mylib.ml":====

```ocaml
external myfunc_a: unit -> unit = "caml_myfunc_a"
external myfunc_b: int -> float -> float = "caml_myfunc_b"
external myfunc_c: int array -> string = "caml_myfunc_c"
```


====file "wrap_mylib.c":====

```c
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <mylib.h>

CAMLprim value
caml_myfunc_a(value unit) {
    myfunc_a();
    return Val_unit;
}

CAMLprim value
caml_myfunc_b(value a, value b) {
    float c = myfunc_b(Int_val(a), Double_val(b));
    return caml_copy_double(c);
}

CAMLprim value
caml_myfunc_c(value ml_array) {
    int i, len;
    int *arr;
    char *s;
    len = Wosize_val(ml_array);
    arr = malloc(len * sizeof(int));
    for (i=0; i < len; i++) {
        arr[i] = Int_val(Field(ml_array, i));
    }
    s = myfunc_c(arr, len);
    free(arr);
    return caml_copy_string(s);
}
```



### =the Makefile:=

(replace spaces by tabs)

```makefile
wrap_mylib.o: wrap_mylib.c
        ocamlc -c -ccopt -I/usr/include/mylib $<

dllmylib_stubs.so: wrap_mylib.o
        ocamlmklib -o mylib_stubs $< -lmylib

mylib.mli: mylib.ml
        ocamlc -i $< > $@

mylib.cmi: mylib.mli
        ocamlc -c $<

mylib.cmo: mylib.ml mylib.cmi
        ocamlc -c $<

mylib.cma: mylib.cmo dllmylib_stubs.so
        ocamlc -a -o $@ $< -dllib -lmylib_stubs -cclib -lmylib

mylib.cmx: mylib.ml mylib.cmi
        ocamlopt -c $<

mylib.cmxa: mylib.cmx dllmylib_stubs.so
        ocamlopt -a -o $@ $< -cclib -lmylib_stubs -cclib -lmylib

clean:
        rm -f *.[oa] *.so *.cm[ixoa] *.cmxa
```


the file <tt>mylib.cma</tt> is used for the interpreted and bytecode modes, and <tt>mylib.cmxa</tt> is for the native mode.


===Using ocaml-ctypes===

There is another solution for calling C functions from a C library which is to use '''ocaml-ctypes'''. We can then define bindings by writing only OCaml code without any C stubs. The equivalent for wrapping the previous hypothetical [[C]] library will be:


```ocaml
open Ctypes
open Foreign

let myfunc_a = foreign "myfunc_a" (void @-> returning void)
let myfunc_b = foreign "myfunc_b" (int @-> float @-> returning float)
let myfunc_c = foreign "myfunc_c" (ptr void @-> int @-> returning string)

let myfunc_c lst =
  let arr = CArray.of_list int lst in
  myfunc_c (to_voidp (CArray.start arr)) (CArray.length arr)
;;
```



## Oz

First we need to create a so-called "native functor" that converts the arguments and describes the C functions:

```cpp
#include "mozart.h"
#include <string.h>

OZ_BI_define(c_strdup,1,1)
{
  OZ_declareVirtualString(0, s1);
  char* s2 = strdup(s1);
  OZ_Term s3 = OZ_string(s2);
  free( s2 );
  OZ_RETURN( s3 );
}
OZ_BI_end

OZ_C_proc_interface * oz_init_module(void)
{
  static OZ_C_proc_interface table[] = {
    {"strdup",1,1,c_strdup},
    {0,0,0,0}
  };
  return table;
}
```


Save this file as "strdup.cc". To automate compiling and linking, we need a makefile for <code>ozmake</code>, the Oz build tool. Save this file as "makefile.oz":

```oz
makefile(
   lib : [
	  'strdup.o' 'strdup.so'
	 ]
   rules:o('strdup.so':ld('strdup.o'))
   )
```

Call <code>ozmake</code> in the same directory.

Now we can write some code that uses the wrapped C function (make sure Emacs' working directory is set to the same directory):


```oz
declare
  [Strdup] = {Module.link ['strdup.so{native}']}
in
  {System.showInfo {Strdup.strdup "hello"}}
```



## PARI/GP

Of course it is trivial to include C functions in PARI, and not uncommon. C++ functions are similar, as PARI is written in a C++-friendly style. The <code>system</code> and <code>install</code> commands allow foreign-language functions to be called from within gp.


## Pascal

See [[Call_a_foreign-language_function#Delphi | Delphi]]


## Perl

Perl code calls a C function <code>c_dup()</code> passing a string <code>'Hello'</code> as an argument, which gets transparently converted to a C string, the <code>c_dup()</code> function makes a copy of that string using <code>strdup()</code> function, stores pointer to the copy in the <code>copy</code> variable and returns it. The returned <code>char</code> pointer gets converted transparently to a Perl string value and gets returned to the calling Perl code which prints it. Then the Perl code calls a C function <code>c_free()</code> to free the allocated memory. Both of the C functions are defined inline in the Perl program and are automatically compiled (only once, unless they change) and linked at runtime. Here is the entire program:

```perl>use Inline C =
 q{
    char *copy;
    char * c_dup(char *orig) {
        return copy = strdup(orig);
    }
    void c_free() {
        free(copy);
    }
};
print c_dup('Hello'), "\n";
c_free();
```


Another example, instead of returning the copy to Perl code it prints it using C printf:

```perl>use Inline C =
 q{
    void c_hello (char *text) {
        char *copy = strdup(text);
        printf("Hello, %s!\n", copy);
        free(copy);
    }
};
c_hello 'world';
```


## Perl 6

{{Works with|rakudo|2016.07}}

```perl6
use NativeCall;

sub strdup(Str $s --> OpaquePointer) is native {*}
sub puts(OpaquePointer $p --> int32) is native {*}
sub free(OpaquePointer $p --> int32) is native {*}

my $p = strdup("Success!");
say 'puts returns ', puts($p);
say 'free returns ', free($p);
```

{{out}}

```txt
Success!
puts returns 9
free returns 0
```



## Phix

The foreign language functions must be compiled to .dll (or .so) form.

Using standard winapi routines to demonstrate the mechanism, this stuff is normally done once in
a library component which can be re-used in different applications.

See also builtins/cffi.e, a text-based C interface that handles C-style structs, unions, and function declarations directly.

```Phix
constant shlwapi = open_dll("shlwapi.dll")
constant xStrDup = define_c_func(shlwapi,"StrDupA",{C_PTR},C_PTR)
constant kernel32 = open_dll("kernel32.dll")
constant xLocalFree = define_c_func(kernel32,"LocalFree",{C_PTR},C_PTR)
constant HelloWorld = "Hello World!"

atom pMem = c_func(xStrDup,{HelloWorld})
?peek_string(pMem)
if c_func(xLocalFree,{pMem})!=NULL then ?9/0 end if
```

{{out}}

```txt

"Hello World!"

```



## PicoLisp

The easiest is to inline the C code. Another possibility would be to write it
into a separate shared object file (see "Call a function in a shared library").

There are differences between the 32-bit and 64-bit versions. While the 64-bit
version can interface directly to C functions, requires the 32-bit function some
glue code.

===32-bit version===

```PicoLisp
(load "@lib/gcc.l")

(gcc "str" NIL                # The 'gcc' function passes all text
   'duptest )                 # until /**/ to the C compiler

any duptest(any ex) {
   any x = evSym(cdr(ex));    // Accept a symbol (string)
   char str[bufSize(x)];      // Create a buffer to unpack the name
   char *s;

   bufString(x, str);         // Upack the string
   s = strdup(str);           // Make a duplicate
   x = mkStr(s);              // Build a new Lisp string
   free(s);                   // Dispose the duplicate
   return x;
}
/**/

(println 'Duplicate (duptest "Hello world!"))
```

===64-bit version===

```PicoLisp
(load "@lib/native.l")

(gcc "str" NIL
   (duptest (Str) duptest 'S Str) )

#include <stdlib.h>
#include <string.h>

char *duptest(char *str) {
   static char *s;

   free(s);    // We simply dispose the result of the last call
   return s = strdup(str);
}
/**/

(println 'Duplicate (duptest "Hello world!"))
```


Output in both cases:

```txt
Duplicate "Hello world!"
```



## PL/I

<lang>declare strdup entry (character (30) varyingz) options (fastcall16);

put (strdup('hello world') );
```



## Prolog

In SWI-Prolog we need to do two things.  First we need to declare a mapping from a Prolog file to a C implementation:


```prolog
:- module(plffi, [strdup/2]).
:- use_foreign_library(plffi).
```


This declares a module "plffi" that exports the '''predicate''' (''not'' function!) "strdup/2".  This predicate has two arguments: the first being the atom being strduped, the second being the duped atom.  (You can think of these as an in parameter and an out parameter and be about 2/3 right.)

Then we need to write a C file that gives us the interface to the underlying C function (strdup in this case), mapping the ''predicate''' call to a C '''function''' call:


```c
#include <string.h>
#include <stdio.h>
#include <SWI-Prolog.h>

static foreign_t pl_strdup(term_t string0, term_t string1)
{
  char *input_string, *output_string;

  if (PL_get_atom_chars(string0, &input_string))
  {
    output_string = strdup(input_string);
    return PL_unify_atom_chars(string1, output_string);
  }
  PL_fail;
}

install_t install_plffi()
{
  PL_register_foreign("strdup", 2, pl_strdup, 0);
}
```


This C code provides us with two things.  The function install_plffi() is provided to register the name "strdup" and to map it to its C implementation pl_strdup().  Here we're saying that "strdup" has an arity of 2, is implemented by pl_strdup and has no special flags.

The function pl_strdup() is where the action is.  First we extract the input string from the first parameter (the in parameter for a slightly inaccurate way of looking at it).  If that succeeds, we call C's strdup() function for the output string.  We then unify this with the second parameter (the out parameter for that same slightly inaccurate way of thinking).

We compile this very easily:


```sh
$ swipl-ld -o plffi -shared plffi.c
```


Then, from within the SWI-Prolog interactor:


```Prolog
?- [plffi].
% plffi compiled into plffi 0.04 sec, 1,477 clauses
true.

?- strdup('Booger!', X).
X = 'Booger!'.

?- strdup(booger, X).
X = booger.

?- strdup(booger, booger).
true.

?- X = booger, strdup(booger, X).
X = booger.
```



## PureBasic

Here we will use [http://flatassembler.net/ Fasm (flat assembler)] to create an object file and then import the function
strucase(t.s) As "_strucase@4". The object file is statically linked within
the resulting executable. [http://www.purebasic.com/ PureBasic] supports {Windows, Linux, MacOS}.


```PureBasic

; Call_a_foreign_language_function.fasm -> Call_a_foreign_language_function.obj
; the assembler code...

; format COFF or
; format COFF64 classic (DJGPP) variants of COFF file

; format MS COFF or
; format MS COFF64 Microsoft's variants of COFF file

format  MS COFF

include "Win32A.Inc"

section ".text" executable readable code

proc	strucase stdcall str:dword
xor	eax,eax
mov	ebx,[str]
strucase_loop:
mov	al,byte[ebx]
cmp	al,0
jz	strucase_is_null_byte
cmp	al,'a'
jb	strucase_skip
cmp	al,'z'
ja	strucase_skip
and	al,11011111b
strucase_skip:
; mov	byte[ebx],al
xchg	al,byte[ebx]
inc	ebx
jmp	strucase_loop
strucase_is_null_byte:
xor	eax,eax
mov	eax,[str]
ret
endp

public strucase as "_strucase@4"

```



```PureBasic

; the PureBasic code...

Import "Call_a_foreign_language_function.obj"
strucase(t.s) As "_strucase@4"
EndImport

t.s="hElLo WoRld!!"
*r=StrUcase(t.s) ; PureBasic is case-insensitive
; cw(peeks(*r))
Debug peeks(*r)

```



'''Sample Output'''

```txt

HELLO WORLD!!

```



## Python



```python
import ctypes
libc = ctypes.CDLL("/lib/libc.so.6")
libc.strcmp("abc", "def")     # -1
libc.strcmp("hello", "hello") #  0
```





## Racket


```racket

#lang racket/base
(require ffi/unsafe)

(provide strdup)

;; Helper: create a Racket string from a C string pointer.
(define make-byte-string
  (get-ffi-obj "scheme_make_byte_string" #f (_fun _pointer -> _scheme)))

;; Take special care not to allow NULL (#f) to be passed as an input,
;; as that will crash strdup.
(define _string/no-null
  (make-ctype _pointer
    (lambda (x)
      (unless (string? x)
        (raise-argument-error '_string/no-null "string" x))
      (string->bytes/utf-8 x))
    ;; We don't use _string/no-null as an output type, so don't care:
    (lambda (x) x)))

; Make a Scheme string from the C string, and free immediately.
(define _string/free
  (make-ctype _pointer
    ;; We don't use this as an input type, so we don't care.
    (lambda (x) x)
    (lambda (x)
      (cond
       [x
        (define s (bytes->string/utf-8 (make-byte-string x)))
        (free x)
        s]
       [else
        ;; We should never get null from strdup unless we're out of
        ;; memory:
        (error 'string/free "Out of memory")]))))

(define strdup
  (get-ffi-obj "strdup" #f (_fun _string/no-null -> _string/free)))

;; Let's try it:
(strdup "Hello World!")

```





## REALbasic


```vb

  Declare Function CreateFileW Lib "Kernel32" (FileName As WString, DesiredAccess As Integer, ShareMode As Integer, SecurityAttributes As Integer, _
        CreateDisposition As Integer, Flags As Integer, Template As Integer) As Integer
  Declare Function WriteFile Lib "Kernel32" (fHandle As Integer, writeData As Ptr, numOfBytes As Integer, ByRef numOfBytesWritten As Integer, _
        overlapped As Ptr) As Boolean
  Declare Function GetLastError Lib "Kernel32" () As Integer
  Declare Function CloseHandle Lib "kernel32" (hObject As Integer) As Boolean

  Const FILE_SHARE_READ = &h00000001
  Const FILE_SHARE_WRITE = &h00000002
  Const OPEN_EXISTING = 3

  Dim fHandle As Integer = CreateFileW("C:\foo.txt", 0,  FILE_SHARE_READ Or FILE_SHARE_WRITE, 0, OPEN_EXISTING, 0, 0)
  If fHandle > 0 Then
    Dim mb As MemoryBlock = "Hello, World!"
    Dim bytesWritten As Integer
    If Not WriteFile(fHandle, mb, mb.Size, bytesWritten, Nil) Then
      MsgBox("Error Number: " + Str(GetLastError))
    End If
    Call CloseHandle(fHandle)
  Else
    MsgBox("Error Number: " + Str(GetLastError))
  End If

```



## REXX

The use of the   '''address'''   statement isn't normally required, but it's shown here as an illustrative example.

```rexx
/*REXX program calls (invoke) a "foreign" (non-REXX) language routine/program.*/

cmd = "MODE"                           /*define the command that is to be used*/
opts= 'CON:  CP  /status'              /*define the options to be used for cmd*/

address  'SYSTEM'  cmd  opts           /*invoke a cmd via the SYSTEM interface*/

                                       /*stick a fork in it,  we're all done. */
```

'''output'''   when executing under a Microsoft Windows system in the USA:

```txt

Status for device CON:
----------------------
    Code page:      437

```





## Ruby


There are three or four different approaches one can take.


###  C extension


The most common approach is to write a C extension. It is compiled on installation. It has to be recompiled when the underlying library changes, and sometimes when the Ruby version changes. C extensions are for [[MRI]], and might not work with other Ruby interpreters.

Put ''rc_strdup.c'' and ''extconf.rb'' in an empty directory. Run <code>ruby extconf.rb</code> then <code>make</code> to build the extension. Put ''demo.rb'' in the same directory, then run <code>ruby -I. demo.rb</code> to see if it works. (The <code>-I.</code> finds the extension in the current directory.)

{{works with|MRI}}

```c
/* rc_strdup.c */
#include <stdlib.h>   /* free() */
#include <string.h>   /* strdup() */
#include <ruby.h>

static VALUE
rc_strdup(VALUE obj, VALUE str_in)
{
    VALUE str_out;
    char *c, *d;

    /*
     * Convert Ruby value to C string.  May raise TypeError if the
     * value isn't a string, or ArgumentError if it contains '\0'.
     */
    c = StringValueCStr(str_in);

    /* Call strdup() and perhaps raise Errno::ENOMEM. */
    d = strdup(c);
    if (d == NULL)
	rb_sys_fail(NULL);

    /* Convert C string to Ruby string. */
    str_out = rb_str_new_cstr(d);
    free(d);
    return str_out;
}

void
Init_rc_strdup(void)
{
    VALUE mRosettaCode = rb_define_module("RosettaCode");
    rb_define_module_function(mRosettaCode, "strdup", rc_strdup, 1);
}
```



```ruby
# extconf.rb
require 'mkmf'
create_makefile('rc_strdup')
```



```ruby
# demo.rb
require 'rc_strdup'
puts RosettaCode.strdup('This string gets duplicated.')
```



###  FFI


A recent effort to make it easier to write libraries, portable across platforms and interpreters, led to the creation of a [http://sourceware.org/libffi/ libffi] binding simply called [http://wiki.github.com/ffi/ffi/ ffi] for completely dynamic calls.


```ruby

require 'ffi'

module LibC
  extend FFI::Library
  ffi_lib FFI::Platform::LIBC

  attach_function :strdup, [:string], :pointer
  attach_function :free, [:pointer], :void
end

string = "Hello, World!"
duplicate = LibC.strdup(string)
puts duplicate.get_string(0)
LibC.free(duplicate)

```





###  Fiddle

Fiddle is part of Ruby's standard library, and is another wrapper for libffi (different from the above FFI module). Fiddle replaces DL in the standard library. DL passed all C values as pointer-size integers, so it didn't work on some platforms. Fiddle uses libffi to pass C values as correct types. Ruby 1.9.2 added Fiddle to the standard library, but scripts needed to mix DL and Fiddle. Ruby 2.0 made Fiddle independent of DL. Ruby 2.2 removed DL, so old scripts don't work now.

{{works with|Ruby|2.0+}}

```ruby
require 'fiddle'

# Find strdup().  It takes a pointer and returns a pointer.
strdup = Fiddle::Function
           .new(Fiddle::Handle['strdup'],
                [Fiddle::TYPE_VOIDP], Fiddle::TYPE_VOIDP)

# Call strdup().
#   - It converts our Ruby string to a C string.
#   - It returns a Fiddle::Pointer.
duplicate = strdup.call("This is a string!")
puts duplicate.to_s     # Convert the C string to a Ruby string.
Fiddle.free duplicate   # free() the memory that strdup() allocated.
```


Fiddle::Importer is also part of Ruby's standard library.

{{works with|Ruby|2.0+}}

```ruby
require 'fiddle'
require 'fiddle/import'

module C
  extend Fiddle::Importer
  dlload Fiddle::Handle::DEFAULT
  extern 'char *strdup(char *)'
end

duplicate = C.strdup("This is a string!")
puts duplicate.to_s
Fiddle.free duplicate
```



###  RubyInline


Using {{libheader|RubyGems}} package [http://www.zenspider.com/ZSS/Products/RubyInline/ RubyInline], which compiles the inlined code on demand during runtime.


```ruby
require 'rubygems'
require 'inline'

class InlineTester
  def factorial_ruby(n)
    (1..n).inject(1, :*)
  end

  inline do |builder|
    builder.c <<-'END_C'
      long factorial_c(int max) {
        long result = 1;
        int i;
        for (i = 1; i <= max; ++i)
          result *= i;
        return result;
      }
    END_C
  end

  inline do |builder|
    builder.include %q("math.h")
    builder.c <<-'END_C'
      int my_ilogb(double value) {
        return ilogb(value);
      }
    END_C
  end
end

t = InlineTester.new
11.upto(14) {|n| p [n, t.factorial_ruby(n), t.factorial_c(n)]}
p t.my_ilogb(1000)
```


outputs (note Ruby's implicit use of Bignum past 12!, while C is stuck with a long int):

```txt
[11, 39916800, 39916800]
[12, 479001600, 479001600]
[13, 6227020800, 1932053504]
[14, 87178291200, 1278945280]
9
```



## Rust



```rust
extern crate libc;

//c function that returns the sum of two integers
extern {
    fn add_input(in1: libc::c_int, in2: libc::c_int) -> libc::c_int;
}

fn main() {
    let (in1, in2) = (5, 4);
    let output = unsafe {
        add_input(in1, in2) };
    assert!( (output == (in1 + in2) ),"Error in sum calculation") ;
}
```



## Scala


```Scala
object JNIDemo {
  try System.loadLibrary("JNIDemo")

  private def callStrdup(s: String)

  println(callStrdup("Hello World!"))
}
```


## Stata

Here are examples showing how to build and call from Stata a plugin written in C or Java. See also the entries 29 to 32 in the ''[https://blog.stata.com/2016/01/15/programming-an-estimation-command-in-stata-a-map-to-posted-entries/ Programming an estimation command in Stata]'' series by David M. Drukker, on [https://blog.stata.com/ Stata Blog].


###  Calling C

It's possible to call a C program from Stata using a '''[https://www.stata.com/plugins/ plugin]'''. A plugin is a C program that is compiled to a DLL, then used as any other command in Stata after being loaded.

As an example let's build a '''[https://en.wikipedia.org/wiki/Hilbert_matrix Hilbert matrix]''' in C.


```cpp
#include <iostream>
#include "stplugin.h"

STDLL stata_call(int argc, char *argv[]) {
    int i, j, n = strtol(argv[1], NULL, 0);

    for (i = 1; i <= n; i++) {
        for (j = 1; j <= n; j++) {
            // Don't forget array indices are 1-based in Stata.
            SF_mat_store(argv[0], i, j, 1.0/(double)(i+j-1));
        }
    }
    return 0;
}
```


The DLL can be built from '''Visual Studio''', or in the console with <code>cl /LD hilbertmat.c stplugin.c</code>. With '''MinGW''', compile with <code>gcc -shared stplugin.c hilbertmatrix.c -o hilbertmat.plugin</code>. With '''Pelles C''', compile with <code>cc /Tx64-coff /Ze stplugin.c hilbertmat.c /DLL /OUT:hilbertmat.plugin</code>. The DLL must be renamed with the '''.plugin''' extension, and put in a directory visible in [https://www.stata.com/help.cgi?adopath adopath].

Declare also an ADO file to call the plugin:


```stata
program hilbert
	matrix define `1'=J(`2',`2',0)
	plugin call hilbertmat, `1' `2'
end

program hilbertmat, plugin
```


Then, you may call


```stata
. hilbert mymat 4

. matrix list mymat

symmetric mymat[4,4]
           c1         c2         c3         c4
r1          1
r2         .5  .33333333
r3  .33333333        .25         .2
r4        .25         .2  .16666667  .14285714
```


Notice the program as is has minimal protection against invalid arguments. Production code should be more careful.


###  Calling Java

It's possible to call a Java program from Stata using the '''[https://www.stata.com/help.cgi?javacall javacall]''' command. Using the '''[https://www.stata.com/java/api15/ Stata Java API]''', one can access the current dataset, matrices, macros...

As an example let's build a '''[https://en.wikipedia.org/wiki/Hilbert_matrix Hilbert matrix]''' in Java.


```java
import com.stata.sfi.*;

public class HilbertMatrix {
    public static int run(String[] args) {
        int n, i, j;
        n = Integer.parseInt(args[1]);
        Matrix.createMatrix(args[0], n, n, 0.0);
        for (i = 0; i < n; i++) {
            for (j = 0; j < n; j++) {
                // Unlike Stata and the C API, indices are 0-based in the Java API.
                Matrix.storeMatrixAt(args[0], i, j, 1.0/(double)(i+j+1));
            }
        }
        return 0;
    }
}
```


Compile with <code>javac -cp %STATA%\utilities\jar\sfi-api.jar HilbertMatrix.java</code>, assuming %STATA% is the path to the Stata install directory.

In Stata, assuming HilbertMatrix.class resides in K:\java:


```stata
. javacall HilbertMatrix run, classpath(K:\java) args(mymat 4)

. matrix list mymat

symmetric mymat[4,4]
           c1         c2         c3         c4
r1          1
r2         .5  .33333333
r3  .33333333        .25         .2
r4        .25         .2  .16666667  .14285714
```


Notice that Mata has the builtin function '''[https://www.stata.com/help.cgi?mf_Hilbert Hilbert]''' to do the same:


```stata
. mata: Hilbert(4)
[symmetric]
                 1             2             3             4
    +---------------------------------------------------------+
  1 |            1                                            |
  2 |           .5   .3333333333                              |
  3 |  .3333333333           .25            .2                |
  4 |          .25            .2   .1666666667   .1428571429  |
    +---------------------------------------------------------+
```



## Swift

Because Swift uses the Objective-C runtime it is trivial to call C/Objective-C functions directly in Swift.

```Swift
import Foundation

let hello = "Hello, World!"
let fromC = strdup(hello)
let backToSwiftString = String.fromCString(fromC)
```



## Tcl

{{libheader|critcl}}
In this solution, we wrap up the <code>ilogb</code> function from C's math library with critcl so that it becomes one of Tcl's normal functions (assuming Tcl 8.5):

```tcl
package require critcl
critcl::code {
    #include <math.h>
}
critcl::cproc tcl::mathfunc::ilogb {double value} int {
    return ilogb(value);
}
package provide ilogb 1.0
```

Note that we do not show <code>strdup</code> here because Tcl manages the memory for strings in complex ways and does not guarantee to preserve string pointers from one call into the C API to the next (e.g., if it has to apply an encoding transformation behind the scenes).
<!-- TODO: a basic thunk, and show off using SWIG -->


## TXR



```txt
This is the TXR Lisp interactive listener of TXR 176.
Use the :quit command or type Ctrl-D on empty line to exit.
1> (with-dyn-lib nil
     (deffi strdup "strdup" str-d (str)))
#:lib-0177
2> (strdup "hello, world!")
"hello, world!"
```


The requirement to free the memory is taken care of the semantics of the <code>str-d</code> ("dynamic") variant of the <code>str</code> type. The semantics denotes the passage of ownership of <code>malloc</code>-ed memory across the interface.

When the C-to-Lisp value conversion takes place on the return value, FFI releases the memory, knowing that it has received ownership of it from the function, which entails that responsibility. If the <code>str</code> type were used by mistake, a memory leak would result.

There is no way to use the <code>str</code> family of types, yet do manual memory management; FFI manages automatically. Code that wants to manually manage a foreign resource referenced by pointer should use <code>cptr</code> or <code>carray</code>, depending on required semantics.


## zkl

In my opinion, FFIs are very problematic and it is better, if you really need external functionality, to spend the effort to write a glue library. Certainly a lot more work. And it only works for C or C++.

For this example, I'll use strlen, nice and simple. strdup doesn't make a lot of sense in the zkl world but would illustrate hooking externally malloc()d space into the garbage collector (easy, one call). This example leaves out the huge amount of code that is usually needed to wrap big chunks of functionality into "proper" garbage collected classes but there are several extension libraries that can be copied.

flf.c:

```c
//-*-c-*-
// flf.c, Call a foreign-language function

// export zklRoot=/home/ZKL
// clang -O -fPIC -I $zklRoot/VM  -c -o flf.o flf.c
// clang flf.o -L$zklRoot/Lib -lzkl -shared -Wl,-soname,flf.so -o flf.so

#include <string.h>

#include "zklObject.h"
#include "zklMethod.h"
#include "zklString.h"
#include "zklImports.h"

    // strlen(str)
static Instance *zkl_strlen(Instance *_,pArglist arglist,pVM vm)
{
   Instance *s  = arglistGetString(arglist,0,"strlen",vm);
   size_t    sz = strlen(stringText(s));
   return intCreate(sz,vm);
}

static int one;

DllExport void *construct(void *vm)
{
   if (!vm) return (void *)ZKLX_PROTOCOL;	// handshake
   	// If this is reloaded, nothing happens except
	// construct() is called again so don't reinitialize
   if (!one)	// static items are zero
   {
      // do some one time initialization
      one = 1;
   }
   return methodCreate(Void,0,zkl_strlen,vm);
}
```

In use on Linux:
{{out}}

```txt

$ clang -O -fPIC -I $zklRoot/VM  -c -o flf.o flf.c
$ clang flf.o -L$zklRoot/Lib -lzkl -shared -Wl,-soname,flf.so -o flf.so
$ zkl
zkl 1.12.3, released 2016-11-01
zkl: var strlen=Import("./flf.so")
Method(Void.???)
zkl: strlen("this is a test")
14
zkl: strlen(123)
3

```



{{omit from|Batch File|Can only execute other programs but cannot call arbitrary functions elsewhere.}}
{{omit from|GUISS}}
{{omit from|M4}}
{{omit from|ML/I}}
{{omit from|Order|Doesn't allow to call functions from other languages.}}
{{omit from|Retro|No FFI}}
{{omit from|TI-83 BASIC|Does not have a standard FFI.}}
{{omit from|TI-89 BASIC|Does not have a standard FFI.}}
{{omit from|Unlambda|Doesn't allow to call functions from other languages.}}

+++
title = "Use another language to call a function"
description = ""
date = 2019-09-11T04:37:36Z
aliases = []
[extra]
id = 4712
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
+++

## Task

This task is inverse to the task [[Call foreign language function]]. Consider the following [[C]] program:

```c
#include <stdio.h>

extern int Query (char * Data, size_t * Length);

int main (int argc, char * argv [])
{
   char     Buffer [1024];
   size_t   Size = sizeof (Buffer);

   if (0 == Query (Buffer, &Size))
   {
      printf ("failed to call Query\n");
   }
   else
   {
      char * Ptr = Buffer;
      while (Size-- > 0) putchar (*Ptr++);
      putchar ('\n');
   }
}
```


Implement the missing <code>Query</code> function in your language, and let this C program call it. The function should place the string ''<tt style="margin:0 0.5em">Here am I</tt>'' into the buffer which is passed to it as the parameter <code>Data</code>. The buffer size in bytes is passed as the parameter <code>Length</code>. When there is no room in the buffer, <code>Query</code> shall return 0. Otherwise it overwrites the beginning of <code>Buffer</code>, sets the number of overwritten bytes into <code>Length</code> and returns 1.


## Ada

The interface package Exported specification:

```Ada
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package Exported is
   function Query (Data : chars_ptr; Size : access size_t)
      return int;
   pragma Export (C, Query, "Query");
end Exported;
```

The package implementation:

```Ada
package body Exported is
   function Query (Data : chars_ptr; Size : access size_t)
      return int is
      Result : char_array := "Here am I";
   begin
      if Size.all < Result'Length then
         return 0;
      else
         Update (Data, 0, Result);
         Size.all := Result'Length;
         return 1;
      end if;
   end Query;
end Exported;
```

With [[GNAT]] it can be built as follows:

```ada
gcc -c main.c
gnatmake -c exported.adb
gnatbind -n exported.ali
gnatlink exported.ali main.o -o main
```

Sample output:

```txt

Here am I

```



## AutoHotkey

It is possible to register an autohotkey function as a callback and get a pointer to it using the builtin registercallback function.  Care should be taken that the external language code is running in the same thread as autohotkey.  This is not a problem when using dllcall to use the external language.  To run an autohotkey function from an external program running in a different thread, you can use [http://www.autohotkey.net/~HotKeyIt/AutoHotkey/ahkFunction.htm ahkFunction] in [http://www.autohotkey.net/~tinku99/ahkdll/ AutoHotkey.dll]
From the documentation on registercallback:

```AutoHotkey
; Example: The following is a working script that displays a summary of all top-level windows.

; For performance and memory conservation, call RegisterCallback() only once for a given callback:
if not EnumAddress  ; Fast-mode is okay because it will be called only from this thread:
    EnumAddress := RegisterCallback("EnumWindowsProc", "Fast")

DetectHiddenWindows On  ; Due to fast-mode, this setting will go into effect for the callback too.

; Pass control to EnumWindows(), which calls the callback repeatedly:
DllCall("EnumWindows", UInt, EnumAddress, UInt, 0)
MsgBox %Output%  ; Display the information accumulated by the callback.

EnumWindowsProc(hwnd, lParam)
{
    global Output
    WinGetTitle, title, ahk_id %hwnd%
    WinGetClass, class, ahk_id %hwnd%
    if title
        Output .= "HWND: " . hwnd . "`tTitle: " . title . "`tClass: " . class . "`n"
    return true  ; Tell EnumWindows() to continue until all windows have been enumerated.
}
```



## C

I rewrote the driver as

```c
#if 0
I rewrote the driver according to good sense, my style,
and discussion.

This is file main.c on Autumn 2011 ubuntu linux release.
The emacs compile command output:

-*- mode: compilation; default-directory: "/tmp/" -*-
Compilation started at Mon Mar 12 20:25:27

make -k CFLAGS=-Wall main.o
cc -Wall   -c -o main.o main.c

Compilation finished at Mon Mar 12 20:25:27
#endif

#include <stdio.h>
#include <stdlib.h>

extern int Query(char *Data, unsigned *Length);

int main(int argc, char *argv[]) {
  char Buffer[1024], *pc;
  unsigned Size = sizeof(Buffer);
  if (!Query(Buffer, &Size))
    fputs("failed to call Query", stdout);
  else
    for (pc = Buffer; Size--; ++pc)
      putchar(*pc);
  putchar('\n');
  return EXIT_SUCCESS;
}

```

With solution

```c

#if 0
This is file query.c

-*- mode: compilation; default-directory: "/tmp/" -*-
Compilation started at Mon Mar 12 20:36:25

make -k CFLAGS=-Wall query.o
cc -Wall   -c -o query.o query.c

Compilation finished at Mon Mar 12 20:36:26
#endif

#include<string.h>

int Query(char *Data, unsigned *Length) {
  const char *message = "Here am I";
  unsigned n = strlen(message);
  if (n <= *Length)
    return strncpy(Data, message, (size_t)n), *Length = n, 1;
  return 0;
}

```

And finally, excitement!

```bash
$ gcc main.c query.o -o main && ./main
Here am I
$

```



## C++


```cpp
#include <string>
using std::string;

// C++ functions with extern "C" can get called from C.
extern "C" int
Query (char *Data, size_t *Length)
{
   const string Message = "Here am I";

   // Check that Message fits in Data.
   if (*Length < Message.length())
      return false;  // C++ converts bool to int.

   *Length = Message.length();
   Message.copy(Data, *Length);
   return true;
}
```


We must compile main() with a C compiler and Query() with a C++ compiler. One can use gcc and g++ (or clang and clang++).


```bash
$ gcc -c main.c
$ g++ -c query.cpp
$ g++ -o main main.o query.o
$ ./main
Here am I
```



## COBOL

GnuCOBOL uses C intermediates and blends well with C programming.  GnuCOBOL is also a fixed length data item language, so this Query routine has to set some limits on passed in external value lengths.  8K in this example, defined using OCCURS DEPENDING ON the input Length.

Instead of C being the master builder, cobc is used to combine the .c source and .cob source into the executable simplifying the tectonic for this example (cobc can generate and use .o object code, but that is all hidden here).  GnuCOBOL also requires a COBOL runtime system, implicitly initialized with the -fimplicit-init compiler switch here.  This emits code to ensure libcob is properly setup for calling from foreign languages (that would otherwise have to call cob_init() before invoking COBOL modules).  The source code in the task description was saved to disk as <code>call-query.c</code>, and the listing below was saved as <code>query.cob</code> (with the internal subprogram named <code>Query</code>).  A special <code>call-convention</code> is also used so the GnuCOBOL module does not make any assumptions about how the Query module is invoked (normal COBOL programs set some control fields in the libcob runtime space when calling modules, which won't be set when called from a foreign C ABI program).  GnuCOBOL also sets <code>RETURN-CODE</code> to zero unless told otherwise (or some error occurs).


```cobol
       identification division.
       program-id. Query.

       environment division.
       configuration section.
       special-names.
           call-convention 0 is extern.

       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 query-result.
          05 filler value "Here I am".

       linkage section.
       01 data-reference.
          05 data-buffer   pic x occurs 0 to 8192 times
                                 depending on length-reference.
       01 length-reference usage binary-long.

       procedure division extern using data-reference length-reference.

       if length(query-result) less than or equal to length-reference
                           and length-reference less than 8193 then
           move query-result to data-reference
           move length(query-result) to length-reference
           move 1 to return-code
       end-if

       goback.
       end program Query.
```


```txt

prompt$ cobc -x -fimplicit-init call-query.c query.cob
prompt$ ./call-query
Here I am
```



## D

This shows how to perform the task on Windows. Elsewhere the procedure is very similar.

First write a D module like this, named "query_func.d":


```d
import core.stdc.string;

extern(C) bool query(char *data, size_t *length) pure nothrow {
    immutable text = "Here am I";

    if (*length < text.length) {
        *length = 0; // Also clears length.
        return false;
    } else {
        memcpy(data, text.ptr, text.length);
        *length = text.length;
        return true;
    }
}
```


Generate a library file with:


```txt
dmd -lib query_func.d
```


This generates a <code>query_func.lib</code> file.

Then create a C file named "mainc.c", given in the task description and here improved a little:


```c
#include <stdio.h>
#include <stdbool.h>

extern bool query(char *data, size_t *length);

int main() {
    char buffer[1024];
    size_t size = sizeof(buffer);

    if (query(buffer, &size))
        printf("%.*s\n", size, buffer);
    else
        puts("The call to query has failed.");

    return 0;
}
```


Then you can compile and link all with the [http://www.digitalmars.com/download/freecompiler.html DMC C compiler](on Linux you can use GCC):


```txt
dmc query_func.lib mainc.c
```


It generates the "mainc.exe" binary, that prints the desired output:


```txt
Here am I
```



## Delphi


```delphi

function Query(Buffer: PChar; var Size: Int64): LongBool;
const
    Text = 'Hello World!';
begin
    If not Assigned(Buffer) Then
    begin
        Size := 0;
        Result := False;
        Exit;
    end;
    If Size < Length(Text) Then
    begin
        Size := 0;
        Result := False;
        Exit;
    end;

    Size := Length(Text);
    Move(Text[1], Buffer^, Size);
    Result := True;
end;

```


To use this function from C you have to export this as a DLL and bind your C program to this function.

== {{header|Fortran}} ==
Simple task because interoperability with C is in Fortran language since F2003 standard.

```Fortran

!-----------------------------------------------------------------------
!Function
!-----------------------------------------------------------------------
function  fortran_query(data, length) result(answer) bind(c, name='Query')
   use, intrinsic  :: iso_c_binding, only: c_char, c_int, c_size_t, c_null_char
   implicit none
   character(len=1,kind=c_char), dimension(length),  intent(inout) ::  data
   integer(c_size_t), intent(inout) :: length
   integer(c_int) :: answer
   answer = 0
   if(length<10) return
   data = transfer("Here I am"//c_null_char, data)
   length = 10_c_size_t
   answer = 1
end function fortran_query

```

compile it:  gfortran main.c query.f90 -o main.x


## Go

Possible&mdash;if you allow a small stretch of the task specification.

Cgo, Go's interface to C, allows calls from C to Go, but only if it gets to start Go first.  That is, it doesn't work with a program started with C startup code and C main(), but only with a program started with Go startup code and Go main().

Thus, I changed the specified C code to begin as follows,

```c
#include <stdio.h>
#include "_cgo_export.h"

void Run()
{
   char     Buffer [1024];
   size_t   Size = sizeof (Buffer);

   if (0 == Query (Buffer, &Size))
   ...
```

The biggest change is that I renamed main, since it is no longer a C main function.  Another small change is that the extern declaration is replaced by an include.  The included file is generated by cgo and contains an equivalent extern declaration.

In the Go code, below, you see that all main does is call C.Run.  The C code is then in the driver's seat.

```go
package main

// #include <stdlib.h>
// extern void Run();
import "C"
import "unsafe"

func main() {
    C.Run()
}

const msg = "Here am I"

//export Query
func Query(cbuf *C.char, csiz *C.size_t) C.int {
    if int(*csiz) <= len(msg) {
        return 0
    }
    pbuf := uintptr(unsafe.Pointer(cbuf))
    for i := 0; i < len(msg); i++ {
        *((*byte)(unsafe.Pointer(pbuf))) = msg[i]
        pbuf++
    }
    *((*byte)(unsafe.Pointer(pbuf))) = 0
    *csiz = C.size_t(len(msg) + 1)
    return 1
}
```

Output:

```txt

Here am I

```



###  Alternative Method

As of Go 1.5, this is now possible without modifying the C code thanks to the addition of buildmodes. Buildmodes allow Go code to be compiled to standard C libraries (both dynamic and static).

The Go code for this task is as follows:

```go

// This buildmode requires the package to be main
package main

// Import C so we can export the function to C and use C types

//#include <stdlib.h> // for size_t
import "C"

// Import reflect and unsafe so we can wrap the C array in a Go slice
import "reflect"
import "unsafe"

// This buildmode also requires a main function, but it is never actually called
func main() {}

// The message to copy into the buffer
const msg = "Here am I"

// Here we declare the Query function using C types and export it to C

//export Query
func Query(buffer *C.char, length *C.size_t) C.int {
        // Check there is enough space in the buffer
        if int(*length) < len(msg) {
                return 0
        }

        // Wrap the buffer in a slice to make it easier to copy into
        sliceHeader := reflect.SliceHeader {
                Data: uintptr(unsafe.Pointer(buffer)),
                Len: len(msg),
                Cap: len(msg),
        }
        bufferSlice := *(*[]byte)(unsafe.Pointer(&sliceHeader))

        // Iterate through the message and copy it to the buffer, byte by byte
        for i:=0;i<len(msg);i++ {
                bufferSlice[i] = msg[i]
        }

        // Set length to the amount of bytes we copied
        (*length) = C.size_t(len(msg))

        return 1
}

```


Assuming this is saved to query.go (and that the C code is saved as main.c) it can be compiled with:

```txt

$ go build -buildmode=c-shared query.go
$ gcc main.c -L. -lquery -o main

```


This creates a library file, a header file for the library (not used) and an executable dynamically linked to the library file.

The executable can be run with:

```txt

$ LD_LIBRARY_PATH=. ./main
Here am I

```



## Haskell

I modified the C source to include Haskell-specific headers and to init the Haskell environment.  I also changed "Query" to "query_hs" due to capitalization issues:

```c
#ifdef __GLASGOW_HASKELL__
#include "Called_stub.h"
extern void __stginit_Called(void);
#endif
#include <stdio.h>
#include <HsFFI.h>

int main (int argc, char * argv [])
{
    char     Buffer [1024];
    size_t   Size = sizeof (Buffer);

    hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Called);
#endif

    if (0 == query_hs (Buffer, &Size))
        {
            printf ("failed to call Query\n");
        }
    else
        {
            char * Ptr = Buffer;
            while (Size-- > 0) putchar (*Ptr++);
            putchar ('\n');
        }

    hs_exit();
    return 0;
}
```


The Haskell code then is:


```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module Called where

import Foreign
import Foreign.C.String (CString, withCStringLen)
import Foreign.C.Types

-- place a string into the buffer pointed to by ptrBuff (with size
-- pointed to by ptrSize). If successful, sets number of overwritten
-- bytes in ptrSize and returns 1, otherwise, it does nothing and
-- returns 0
query_hs ::  CString -> Ptr CSize -> IO CInt
query_hs ptrBuff ptrSize = withCStringLen "Here I am"
               (\(str, len) -> do
                   buffSize <- peek ptrSize
                   if sizeOf str > (fromIntegral buffSize)
                     then do
                       poke ptrSize 0
                       return 0
                     else do
                       poke ptrSize (fromIntegral len)
                       copyArray ptrBuff str len
                       return 1)

foreign export ccall query_hs :: CString -> Ptr CSize -> IO CInt
```


Compile the Haskell code with:

```bash
ghc -c -O Called.hs
```


Then compile the C code together with the generated Haskell files (using GHC):

```bash
ghc -optc-O calling.c Called.o Called_stub.o -o calling
```


Output:

```txt

Here I am

```



## Haxe


###  PHP


```haxe
untyped __call__("functionName", args);
```



## J


Install an input and an output routine to use the J engine externally.  These pass character strings arguments.  To complete the task, I made these two routines communicate using compilation-unit scope variables (static).  I tested the program on a 64 bit Ubuntu linux 2011 Autumn release with j versions 602 and 701.  Comment of asterisks marks the input and output routines.

The J verb evaluates to the string unless there is no space.
File <tt>rc_embed.ijs</tt>

```J

query=:3 :'0&#^:(y < #)''Here am I'''

```


main.c

```c

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

int Query(char*,unsigned*);

int main(int argc,char*argv[]) {
  char Buffer[1024], *pc;
  unsigned Size = (unsigned)sizeof(Buffer);
  if (!Query(Buffer,&Size))
    fputs("Failed to call Query",stdout);
  else
    for (pc = Buffer; Size--; ++pc)
      putchar(*pc);
  putchar('\n');
  return EXIT_SUCCESS;
}

```


Query.c

```c


// J Front End Example
// define _WIN32 for Windows, __MACH__ for MAC, J64 for 64-bit
// JE is loaded from current working directory

//make jfex && LD_LIBRARY_PATH=/usr/local/j64-701/bin ./jfex

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <windows.h>
#include <direct.h>
#define GETPROCADDRESS(h,p) GetProcAddress(h,p)
#define JDLLNAME "\\j.dll"
#else
#define _stdcall
#include <dlfcn.h>
#define GETPROCADDRESS(h,p)	dlsym(h,p)
#ifdef __MACH__
#define JDLLNAME "/libj.dylib"
#else
#define JDLLNAME "/libj.so"
#endif
#define _getcwd getcwd
#endif

#include<stdio.h>
#include<signal.h>
#include<stdlib.h>
#include<string.h>
#include"jfex.h"
#include"jlib.h"

static JDoType jdo;
static JFreeType jfree;
static JgaType jga;
static JGetLocaleType jgetlocale;

static J jt;
static void* hjdll;

static char **adadbreak;
static void sigint(int k){**adadbreak+=1;signal(SIGINT,sigint);}
static char input[1000];

// J calls for input (debug suspension and 1!:1[1) and we call for input
char* _stdcall Jinput(J jt,char* prompt)
{
  fputs(prompt,stdout);
  if(fgets(input, sizeof(input), stdin))
    {
      fputs("\n",stdout);
      **adadbreak+=1;
    }
  return input;
}

static char*buffer = NULL;   /**************************************/
static unsigned length = 0;  /**************************************/
static int Jouts = 0;        /**************************************/

// J calls for output
#define LINEFEED 10          /**************************************/
void _stdcall Joutput(J jt,int type, char* s)  /********************/
{
  size_t L;
  if(MTYOEXIT==type) exit((int)(I)s);
  L = strlen(s);
  L -= (L && (LINEFEED==s[L-1])); /* CRLF not handled. */
  if (L && (!Jouts)) {
    length = L;
    strncpy(buffer,s,L);
    Jouts = 1;
  }
}

int Query(char*Data,unsigned*Length)
{
  void* callbacks[] = {Joutput,NULL,Jinput,0,(void*)SMCON};
  char pathdll[1000];
  _getcwd(pathdll,sizeof(pathdll));
  strcat(pathdll,JDLLNAME);
#ifdef _WIN32
  hjdll=LoadLibraryA(pathdll);
#else
  hjdll=dlopen(pathdll,RTLD_LAZY);
  if (NULL == hjdll)
    hjdll=dlopen(JDLLNAME+1,RTLD_LAZY); /* use LD_LIBRARY_PATH */
#endif
  if(NULL == hjdll)
    {
      fprintf(stderr,"Unix use: $ LD_LIBRARY_PATH=path/to/libj.so %s\n","programName");//*argv);
      fputs("Load library failed: ",stderr);
      fputs(pathdll,stderr);
      fputs("\n",stderr);
      return 0; // load library failed
    }
  jt=((JInitType)GETPROCADDRESS(hjdll,"JInit"))();
  if(!jt) return 0; // JE init failed
  ((JSMType)GETPROCADDRESS(hjdll,"JSM"))(jt,callbacks);
  jdo=(JDoType)GETPROCADDRESS(hjdll,"JDo");
  jfree=(JFreeType)GETPROCADDRESS(hjdll,"JFree");
  jga=(JgaType)GETPROCADDRESS(hjdll,"Jga");
  jgetlocale=(JGetLocaleType)GETPROCADDRESS(hjdll,"JGetLocale");
  adadbreak=(char**)jt; // first address in jt is address of breakdata
  signal(SIGINT,sigint);
  {
    char input[999];
    //memset(input,0,sizeof input);
    buffer = Data;
    sprintf(input,"query %u [ 0!:110<'rc_embed.ijs'\n",*Length); /***deceptive input routine, a hard coded string*********/
    jdo(jt,input);
    if (!Jouts)
      return 0;
    *Length = length;
  }
  jfree(jt);
  return 1;
}

```


makefile, adjust for your j installation.

```make

# jfe makefile info
# customize to create makefile suitable for your platform
# 32bit builds on 64bit systems require -m32 in CFLAGS and FLAGS
# Unix requires -ldl  in FLAGS and Windows does not

CPPFLAGS= -I/usr/local/j64-602/system/examples/jfe
CFLAGS= -O0 -g
LOADLIBES= -ldl

main: main.o Query.o

```


Finally, build and execution.  Again, adjust LD_LIBRARY_PATH to the directory of libj.so .

```bash

$ make main && LD_LIBRARY_PATH=~/Downloads/jgplsrc/j/bin ./main
Here am I
$

```



## Java

We write a Java method, then write a C function <code>Query()</code> to use the Java Native Interface (JNI) to call our Java method. The C compiler must find ''jni.h'' and ''jni_md.h'' and link with ''libjvm''.


```java
/* Query.java */
public class Query {
    public static boolean call(byte[] data, int[] length)
	throws java.io.UnsupportedEncodingException
    {
	String message = "Here am I";
	byte[] mb = message.getBytes("utf-8");
	if (length[0] < mb.length)
	    return false;
	length[0] = mb.length;
	System.arraycopy(mb, 0, data, 0, mb.length);
	return true;
    }
}
```



```c
/* query-jni.c */
#include <stdio.h>
#include <stdlib.h>
#include <jni.h>

static JavaVM *jvm = NULL;
static JNIEnv *jenv = NULL;

static void die(const char *message) {
    fprintf(stderr, "%s\n", message);
    exit(1);
}

static void oom(void) {
    die("Query: out of memory");
}

static void except(void) {
    if ((*jenv)->ExceptionCheck(jenv))
	die("Query: unexpected Java exception");
}

static void do_at_exit(void) {
    (*jvm)->DestroyJavaVM(jvm);
}

static void require_jvm(void) {
    JavaVMInitArgs args;

    if (jvm)
	return;

    args.version = JNI_VERSION_1_4;
    args.nOptions = 0;
    args.options = NULL;
    args.ignoreUnrecognized = JNI_FALSE;
    if (JNI_CreateJavaVM(&jvm, (void **)&jenv, &args) != JNI_OK)
	die("Query: can't create Java VM");
    atexit(do_at_exit);
}

int Query(char *data, size_t *length) {
    jclass cQuery;
    jmethodID mcall;
    jintArray jlength;
    jint jlength0;
    jbyteArray jdata;
    jboolean result;

    jlength0 = (jint)length[0];
    if ((size_t)jlength0 != length[0])
	die("Query: length is too large for Java array");

    require_jvm();

    /* Create a local frame for references to Java objects. */
    if ((*jenv)->PushLocalFrame(jenv, 16))
	oom();

    /* Look for class Query, static boolean call(byte[], int[]) */
    cQuery = (*jenv)->FindClass(jenv, "Query");
    if (cQuery == NULL)
	die("Query: can't find Query.class");
    mcall = (*jenv)->GetStaticMethodID(jenv, cQuery, "call", "([B[I)Z");
    if (mcall == NULL)
	die("Query: missing call() method");

    /*
     * Make arguments to Query.call().  We can't pass data[] and
     * length[] to Java, so we make new Java arrays jdata[] and
     * jlength[].
     */
    jdata = (*jenv)->NewByteArray(jenv, (jsize)jlength0);
    if (jdata == NULL)
	oom();
    jlength = (*jenv)->NewIntArray(jenv, 1);
    if (jlength == NULL)
	oom();

    /* Set jlength[0] = length[0]. */
    (*jenv)->SetIntArrayRegion(jenv, jlength, 0, 1, &jlength0);
    except();

    /*
     * Call our Java method.
     */
    result = (*jenv)->CallStaticBooleanMethod
	(jenv, cQuery, mcall, jdata, jlength);
    except();

    /*
     * Set length[0] = jlength[0].
     * Copy length[0] bytes from jdata[] to data[].
     */
    (*jenv)->GetIntArrayRegion(jenv, jlength, 0, 1, &jlength0);
    except();
    length[0] = (size_t)jlength0;
    (*jenv)->GetByteArrayRegion
	(jenv, jdata, 0, (jsize)jlength0, (jbyte *)data);

    /* Drop our local frame and its references. */
    (*jenv)->PopLocalFrame(jenv, NULL);

    return (int)result;
}
```



```make
# Makefile

# Edit these lines to match your JDK.
JAVA_HOME = /Library/Java/Home
CPPFLAGS = -I$(JAVA_HOME)/include
LIBS = -framework JavaVM
JAVAC = $(JAVA_HOME)/bin/javac
CC = cc

all: calljava Query.class

calljava: main.o query-jni.o
	$(CC) -o calljava main.o query-jni.o $(LIBS)

.SUFFIXES: .c .class .java .o
.c.o:
	$(CC) $(CPPFLAGS) -c $<
.java.class:
	$(JAVAC) $<

clean:
	rm -f calljava main.o query-jni.o Query.class
```



## Kotlin

Reverse interop (calling Kotlin from C) was added to Kotlin Native in version 0.5 and the following shows how to perform this task on Ubuntu Linux.

First we compile the following Kotlin source file (Query.kt) using the '-platform dynamic' flag:

```scala
// Kotlin Native v0.6

import kotlinx.cinterop.*
import platform.posix.*

fun query(data: CPointer<ByteVar>, length: CPointer<size_tVar>): Int {
    val s = "Here am I"
    val strLen = s.length
    val bufferSize = length.pointed.value
    if (strLen > bufferSize) return 0  // buffer not large enough
    for (i in 0 until strLen) data[i] = s[i].toByte()
    length.pointed.value = strLen.signExtend<size_t>()
    return 1
}
```


This produces the dynamic library, libQuery.so, and the C header file libQuery_api.h:

```c
#ifndef KONAN_LIBQUERY_H
#define KONAN_LIBQUERY_H
#ifdef __cplusplus
extern "C" {
#endif
typedef unsigned char   libQuery_KBoolean;
typedef char            libQuery_KByte;
typedef unsigned short  libQuery_KChar;
typedef short           libQuery_KShort;
typedef int             libQuery_KInt;
typedef long long       libQuery_KLong;
typedef float           libQuery_KFloat;
typedef double          libQuery_KDouble;
typedef void*           libQuery_KNativePtr;
struct libQuery_KType;
typedef struct libQuery_KType libQuery_KType;

typedef struct {
  /* Service functions. */
  void (*DisposeStablePointer)(libQuery_KNativePtr ptr);
  void (*DisposeString)(const char* string);
  libQuery_KBoolean (*IsInstance)(libQuery_KNativePtr ref, const libQuery_KType* type);

  /* User functions. */
  struct {
    struct {
      libQuery_KInt (*query)(void* data, void* length);
    } root;
  } kotlin;
} libQuery_ExportedSymbols;
extern libQuery_ExportedSymbols* libQuery_symbols(void);
#ifdef __cplusplus
}  /* extern "C" */
#endif
#endif  /* KONAN_LIBQUERY_H */
```


We now compile a slightly modified version of the C program required for this task, linking to the above library, and 'including' the header file:

```c
#include <stdio.h>
#include <stdlib.h>
#include "libQuery_api.h"

static int Query (char * Data, size_t * Length)
{
    return libQuery_symbols() -> kotlin.root.query(Data, Length);
}

int main (int argc, char * argv [])
{
    char     Buffer [1024];
    size_t   Size = sizeof (Buffer);

    if (0 == Query (Buffer, &Size))
    {
        printf ("failed to call Query\n");
    }
    else
    {
        char * Ptr = Buffer;
        while (Size-- > 0) putchar (*Ptr++);
        putchar ('\n');
    }
}
```


which when executed produces the expected output:

```txt

Here am I

```



## Lisaac

query.li

```Lisaac
Section Header

+ name := QUERY;
- external := `#define main _query_main`;
- external := `#define query Query`;

Section External

- query(buffer : NATIVE_ARRAY[CHARACTER], size : NATIVE_ARRAY[INTEGER]) : INTEGER <- (
  + s : STRING_CONSTANT;
  + len, result : INTEGER;
  s := "Here am I";
  len := s.count;
  (len > size.item(0)).if {
    result := 0;
  } else {
    1.to len do { i : INTEGER;
      buffer.put (s @ i) to (i - 1);
    };
    size.put len to 0;
    result := 1;
  };
  result
);

Section Public

- main <- (
  + buffer : NATIVE_ARRAY[CHARACTER];
  + size : NATIVE_ARRAY[INTEGER];
  query(buffer, size); // need this to pull the query() method
);
```

Makefile

```lisaac
TARGET=test_query

all: $(TARGET)

$(TARGET): main.o query.o
	gcc -o $@ main.o query.o

.c.o:
	gcc -c $<

query.c: query.li
	-lisaac $<

clean:
	rm -f $(TARGET) *.o query.c
```



## Mercury


The code as written is horrible for Mercury, so some additional C is added as a shim that actually calls the Mercury predicate. Although no changes are required to the C code in this simple example, in a larger project, with modules that need initialization, there are [https://mercurylang.org/information/doc-latest/mercury_user_guide/Using-mmc.html some additional compilation steps] needed to get that initialization code in.


```Mercury
:- module query.
:- interface.

:- pred query(string::in, string::out) is det.

:- implementation.

query(_, "Hello, world!").

:- pragma foreign_export("C", query(in, out), "query").

:- pragma foreign_decl("C",
"
#include <string.h>
int Query (char * Data, size_t * Length);
").
:- pragma foreign_code("C",
"
int Query (char *Data, size_t *Length) {
    MR_String input, result;
    MR_allocate_aligned_string_msg(input, *Length, MR_ALLOC_ID);
    memmove(input, Data, *Length);
    query(input, &result);
    *Length = strlen(result);
    memmove(Data, result, *Length);
    return 1;
}
").
```


Building with the unchanged C in useanother.c:


```txt
$ mmc -c query
$ gcc -Wall -c useanother.c
$ ml -o useanother useanother.o query.o
```


```txt
Hello, world!
```



## Nim


```nim
proc Query*(data: var array[1024, char], length: var cint): cint {.exportc.} =
  const text = "Here am I"
  if length < text.len:
    return 0

  for i in 0 .. <text.len:
    data[i] = text[i]
  length = text.len
  return 1
```

Compile the above with <code>nim c --app:staticlib --no_main query.nim</code>.

```c
#include <stdio.h>

extern int Query (char * Data, size_t * Length);

int main (int argc, char * argv [])
{
   char     Buffer [1024];
   size_t   Size = sizeof (Buffer);

   if (0 == Query (Buffer, &Size))
   {
      printf ("failed to call Query\n");
   }
   else
   {
      char * Ptr = Buffer;
      while (Size-- > 0) putchar (*Ptr++);
      putchar ('\n');
   }
}
```

Compile the above with <code>gcc -ldl -o main main.c libquery.nim.a</code>, then execute the resulting <code>main</code> binary:

```txt
./main
Here am I

```



## OCaml


```c
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

extern int Query (char * Data, size_t * Length)
{
   static value * closure_f = NULL;
   if (closure_f == NULL) {
       closure_f = caml_named_value("Query function cb");
   }
   value ret = caml_callback(*closure_f, Val_unit);
   *Length = Int_val(Field(ret, 1));
   strncpy(Data, String_val(Field(ret, 0)), *Length);
   return 1;
}

int main (int argc, char * argv [])
{
   char     Buffer [1024];
   unsigned Size = 0;

   caml_main(argv);  /* added from the original main */

   if (0 == Query (Buffer, &Size))
   {
      printf ("failed to call Query\n");
   }
   else
   {
      char * Ptr = Buffer;
      printf("size: %d\n", Size);
      while (Size-- > 0) putchar (*Ptr++);
      putchar ('\n');
   }
}
```



```ocaml
let caml_query () =
  let s = "Here am I" in
  (s, String.length s)
;;

let () =
  Callback.register "Query function cb" caml_query;
;;
```


compile with:

 ocamlopt -output-obj caml_part.ml -o caml_part_obj.o
 gcc -c main.c  -I"`ocamlc -where`"
 gcc -o prog.opt  main.o  caml_part_obj.o \
       -L"`ocamlc -where`" \
       -lm -ldl -lasmrun


## PARI/GP


This is a Linux solution. Message "Here I am" is encrypted with ROT13: "Urer V nz".

ROT13() is implemented as a PARI one-liner:
```parigp
Strchr(Vecsmall(apply(k->if(k>96&&k<123,(k-84)%26+97,if(k>64&&k<91,(k-52)%26+65,k)),Vec(Vecsmall(s)))))
```


PARI's interface for Query()... query.c:

```C>#include <pari/pari.h


#define PARI_SECRET     "s=\"Urer V nz\";Strchr(Vecsmall(apply(k->if(k>96&&k<123,(k-84)%26+97,if(k>64&&k<91,(k-52)%26+65,k)),Vec(Vecsmall(s)))))"

int Query(char *Data, size_t *Length)
{
  int rc = 0;
  GEN result;

  pari_init(1000000, 2);

  result = geval(strtoGENstr(PARI_SECRET));     /* solve the secret */

  if (result) {
    strncpy(Data, GSTR(result), *Length);	/* return secret */
    rc = 1;
  }

  pari_close();

  return rc;
}
```


Compile interface to a library: ''gcc -O2 -Wall -fPIC -shared query.c -o libquery.so -lpari''

Compile main() C code from above and link against this library: ''gcc -O2 -Wall main.c -o main -L. libquery.so''

Start main(): ''LD_LIBRARY_PATH=. ./main''

PARI solves the ROT13 encrypted message and returns result to caller.

Output:
```txt
Here I am
```


NB. It's also possible to compile both files together without building an interface: ''gcc -O2 -Wall main.c query.c -o main2 -lpari''

''./main2'' yields same output as stated above.


## Pascal

See [[Use_another_language_to_call_a_function#Delphi | Delphi]]


## Phix

The following code declares a callback for the C code (which I'd expect to be in a .dll or .so) to invoke.

A 32-bit-only or a 64-bit-only version would of course be slightly shorter.

```Phix
constant Here_am_I = "Here am I"
function Query(atom pData, atom pLength)
integer len = peekNS(pLength,machine_word(),0)
    if poke_string(pData,len,Here_am_I) then
        return 0
    end if
    pokeN(pLength,length(Here_am_I)+1,machine_word())
    return 1
end function
constant Query_cb = call_back(routine_id("Query"))
```



## PicoLisp

Calling a PicoLisp function from another program requires a running interpreter.
There are several possibilities, like IPC via fifo's or sockets using the PLIO
(PicoLisp-I/O) protocol, but the easiest is calling the interpreter in a pipe.
This is relatively efficient, as the interpreter's startup time is quite short.

If there is a file "query.l"

```PicoLisp
(let (Str "Here am I"  Len (format (opt)))  # Get length from command line
   (unless (>= (size Str) Len)              # Check buffer size
      (prinl Str) ) )                       # Return string if OK
```

then the C function 'Query' could be

```C
int Query(char *Data, size_t *Length) {
   FILE *fp;
   char buf[64];

   sprintf(buf, "/usr/bin/picolisp query.l %d -bye", *Length);
   if (!(fp = popen(buf, "r")))
      return 0;
   fgets(Data, *Length, fp);
   *Length = strlen(Data);
   return pclose(fp) >= 0 && *Length != 0;
}
```



## Python


Our embedded python function a) uses information from the main routine in c, and b) determines the information to populate the result returned to the main routine.  This, I believe, fulfills the task requirement.  The modifications and compilation are shown for Ubuntu linux Autumn 2011 version, with python3.  It's easier to call a dynamic library from python using the ctypes module.  Consider using <tt>PyRun_SimpleString</tt> to have main.c call python calling back to c.

```python

# store this in file rc_embed.py
# store this in file rc_embed.py
def query(buffer_length):
    message = b'Here am I'
    L = len(message)
    return message[0:L*(L <= buffer_length)]

```


main.c

```c

#if 0
//I rewrote the driver according to good sense, my style,
//and discussion --Kernigh 15:45, 12 February 2011 (UTC).
#endif

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

extern int Query(char*,unsigned*);

int main(int argc,char*argv[]) {
  char Buffer[1024], *pc;
  unsigned Size = sizeof(Buffer);
  if (!Query(Buffer,&Size))
    fputs("Failed to call Query",stdout);
  else
    for (pc = Buffer; Size--; ++pc)
      putchar(*pc);
  putchar('\n');
  return EXIT_SUCCESS;
}

```


In Query.c I don't promise to have tested every case with missing module, missing function, or to have used <tt>Py_DECREF</tt> correctly.

```c

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<Python.h>

int Query(char*Data,unsigned*Length) {
  char *module = "rc_embed", *function = "query";
  PyObject *pName, *pModule, *pFunc, *pResult, *pArgs, *pLength;
  long result = 0;
  if (!Py_IsInitialized())
    Py_Initialize();
  pName = PyUnicode_FromString(module);
  pModule = PyImport_Import(pName);
  Py_DECREF(pName);
  if (NULL == pModule) {
    PyErr_Print();
    fprintf(stderr,"Failed to load \"%s\"\n",module);
    return 0;
  }
  pFunc = PyObject_GetAttrString(pModule,function);
  if ((NULL == pFunc) || (!PyCallable_Check(pFunc))) {
    if (PyErr_Occurred())
      PyErr_Print();
    fprintf(stderr,"Cannot find function \"%s\"\n",function);
    if (NULL != pFunc)
      Py_DECREF(pFunc);
    Py_DECREF(pModule);
    return 0;
  }
  pArgs = PyTuple_New(1);
  pLength = PyLong_FromUnsignedLong((unsigned long)(*Length));
  if (NULL == pLength) {
    Py_DECREF(pArgs);
    Py_DECREF(pFunc);
    Py_DECREF(pModule);
    return 0;
  }
  PyTuple_SetItem(pArgs,0,pLength);
  pResult = PyObject_CallObject(pFunc, pArgs);
  if (NULL == pResult)
    result = 0;
  else if (!PyBytes_Check(pResult)) {
    result = 0;
    Py_DECREF(pResult);
  } else {
    if (! PyBytes_Size(pResult))
      result = 0;
    else {
      *Length = (unsigned)PyBytes_Size(pResult);
      strncpy(Data,PyBytes_AsString(pResult),*Length);
      Py_DECREF(pResult);
      result = 1;
    }
  }
  Py_DECREF(pArgs);
  Py_DECREF(pFunc);
  Py_DECREF(pModule);
  Py_Finalize();
  return result;
}

```


Compilation, linkage, execution.  Note the python tools used to extract the correct flags.

```bash

$ make main.o
cc    -c -o main.o main.c
$ D=$( dirname $( which python3 ) )
$ gcc $( $D/python3.2-config --cflags ) -c Query.c
In file included from /usr/include/python3.2mu/Python.h:8:0,
                 from Q.c:18:
/usr/include/python3.2mu/pyconfig.h:1173:0: warning: "_POSIX_C_SOURCE" redefined [enabled by default]
/usr/include/features.h:214:0: note: this is the location of the previous definition
$ gcc -o main main.o Query.o $( $D/python3.2-config --ldflags )
$ ./main
Here am I
$

```



## Racket


Since this problem is presented as the inverse to [[Call foreign language function]], I've focused on just demonstrating a callback from C into Racket, instead of showing how to [http://docs.racket-lang.org/inside/embedding.html embed the whole Racket runtime into C].

Starting with the given C code, modify it so that <tt>Query</tt> is a variable instead of an external:


```C

typedef int strfun (char * Data, size_t * Length);
strfun *Query = NULL;

```


The rest of the C code is left as-is.  Compile it into a dynamic library, then run the following Racket code:


```racket

#lang racket

(require ffi/unsafe)

(define xlib (ffi-lib "./x.so"))

(set-ffi-obj! "Query" xlib (_fun _pointer _pointer -> _bool)
  (λ(bs len)
    (define out #"Here I am")
    (let ([bs (make-sized-byte-string bs (ptr-ref len _int))])
      (and ((bytes-length out) . <= . (bytes-length bs))
           (begin (bytes-copy! bs 0 out)
                  (ptr-set! len _int (bytes-length out))
                  #t)))))

((get-ffi-obj "main" xlib (_fun _int (_list i _bytes) -> _void))
 0 '())

```


Note that this code is intentionally in a simple low-level form, for example, it sets the pointer directly instead of using a C function.

The output is the expected “<tt>Here I am</tt>” line.


## Ruby

We have four files.  First, ''main.c'' has the <code>main()</code> from the top of this page.  Second, ''query.rb'' uses Fiddle to create a C function at run time.  It sets the C variable <code>QueryPointer</code> to this function.  It needs at least Ruby 2.3 for <code>Array#unpack('J')</code>.  Our <code>main()</code> can't call <code>QueryPointer()</code>, because it expects <code>Query()</code> to exist at link time, and it would crash if Ruby raised an error.  The third file ''query-rb.c'' provides <code>Query()</code> as a wrapper around <code>QueryPointer()</code>.  The wrapper embeds the Ruby interpreter and protects against Ruby errors.  The fourth file ''Rakefile'' builds the program.

```ruby
# query.rb
require 'fiddle'

# Look for a C variable named QueryPointer.
# Raise an error if it is missing.
c_var = Fiddle.dlopen(nil)['QueryPointer']

int = Fiddle::TYPE_INT
voidp = Fiddle::TYPE_VOIDP
sz_voidp = Fiddle::SIZEOF_VOIDP

# Implement the C function
#   int Query(void *data, size_t *length)
# in Ruby code.  Store it in a global constant in Ruby (named Query)
# to protect it from Ruby's garbage collector.
#
Query = Fiddle::Closure::BlockCaller
          .new(int, [voidp, voidp]) do |datap, lengthp|
  message = "Here am I"

  # We got datap and lengthp as Fiddle::Pointer objects.
  # Read length, assuming sizeof(size_t) == sizeof(void *).
  length = lengthp[0, sz_voidp].unpack('J').first

  # Does the message fit in length bytes?
  if length < message.bytesize
    0  # failure
  else
    length = message.bytesize
    datap[0, length] = message  # Copy the message.
    lengthp[0, sz_voidp] = [length].pack('J')  # Update the length.
    1  # success
  end
end

# Set the C variable to our Query.
Fiddle::Pointer.new(c_var)[0, sz_voidp] = [Query.to_i].pack('J')
```



```c
/* query-rb.c */
#include <stdlib.h>
#include <ruby.h>

/*
 * QueryPointer() uses Ruby and may raise a Ruby error.  Query() is a
 * C wrapper around QueryPointer() that loads Ruby, sets QueryPointer,
 * and protects against Ruby errors.
 */
int (*QueryPointer)(char *, size_t *) = NULL;

static int in_bad_exit = 0;

static void
do_at_exit(void)
{
    RUBY_INIT_STACK;

    if (!in_bad_exit)
	ruby_cleanup(0);
}

static void
bad_exit(int state)
{
    in_bad_exit = 1;
    ruby_stop(state);  /* Clean up Ruby and exit the process. */
}

static void
require_query(void)
{
    static int done = 0;
    int state;

    if (done)
	return;
    done = 1;

    ruby_init();
    atexit(do_at_exit);
    ruby_init_loadpath();  /* needed to require 'fiddle' */

    /* Require query.rb in current directory. */
    rb_eval_string_protect("require_relative 'query'", &state);
    if (!state && !QueryPointer)
	rb_eval_string_protect("fail 'missing QueryPointer'", &state);
    if (state)
	bad_exit(state);  /* Ruby will report the error. */
}

struct args {
    char *data;
    size_t *length;
    int result;
};

static VALUE
Query1(VALUE v) {
    struct args *a = (struct args *)v;
    a->result = QueryPointer(a->data, a->length);
    return Qnil;
}

int
Query(char *data, size_t *length)
{
    struct args a;
    int state;
    RUBY_INIT_STACK;

    require_query();

    /* Call QueryPointer(), protect against errors. */
    a.data = data;
    a.length = length;
    rb_protect(Query1, (VALUE)&a, &state);
    if (state)
	bad_exit(state);
    return a.result;
}
```



```ruby
# Rakefile

# To build and run:
#   $ rake
#   $ ./callruby

# Must link with cc -Wl,-E so query.c exports QueryPointer.
CC = ENV.fetch('CC', 'cc')
LDFLAGS = '-Wl,-E'
CPPFLAGS = RbConfig.expand('-I$(rubyarchhdrdir) -I$(rubyhdrdir)')
LIBS = RbConfig.expand('$(LIBRUBYARG) $(LIBS)')

task 'default' => 'callruby'

desc 'compiles callruby'
file 'callruby' => %w[main.o query-rb.o] do |t|
  sh "#{CC} #{LDFLAGS} -o #{t.name} #{t.sources.join(' ')} #{LIBS}"
end

rule '.o' => %w[.c] do |t|
  sh "#{CC} #{CPPFLAGS} -o #{t.name} -c #{t.source}"
end

desc 'removes callruby and .o files'
task 'clean' do
  rm_f %w[callruby main.o query-rb.o]
end
```



## Scala


### Using the JVM

We write a Scala function, then write a C function <code>Query()</code> to use the Java Native Interface (JNI) to call our Scala method. The C compiler must find ''jni.h'' and ''jni_md.h'' and link with ''libjvm''.


```scala
/* Query.scala */
object Query {
  def call(data: Array[Byte], length: Array[Int]): Boolean = {
    val message = "Here am I"
    val mb = message.getBytes("utf-8")
    if (length(0) >= mb.length) {
      length(0) = mb.length
      System.arraycopy(mb, 0, data, 0, mb.length)
      true
    } else false
  }
}
```



```c
/* query-jni.c */
#include <stdio.h>
#include <stdlib.h>
#include <jni.h>

static JavaVM *jvm = NULL;
static JNIEnv *jenv = NULL;

static void die(const char *message) {
    fprintf(stderr, "%s\n", message);
    exit(1);
}

static void oom(void) {
    die("Query: out of memory");
}

static void except(void) {
    if ((*jenv)->ExceptionCheck(jenv))
	die("Query: unexpected Java exception");
}

static void do_at_exit(void) {
    (*jvm)->DestroyJavaVM(jvm);
}

static void require_jvm(void) {
    JavaVMInitArgs args;

    if (jvm)
	return;

    args.version = JNI_VERSION_1_4;
    args.nOptions = 0;
    args.options = NULL;
    args.ignoreUnrecognized = JNI_FALSE;
    if (JNI_CreateJavaVM(&jvm, (void **)&jenv, &args) != JNI_OK)
	die("Query: can't create Java VM");
    atexit(do_at_exit);
}

int Query(char *data, size_t *length) {
    jclass cQuery;
    jmethodID mcall;
    jintArray jlength;
    jint jlength0;
    jbyteArray jdata;
    jboolean result;

    jlength0 = (jint)length[0];
    if ((size_t)jlength0 != length[0])
	die("Query: length is too large for Scala array");

    require_jvm();

    /* Create a local frame for references to Scala objects. */
    if ((*jenv)->PushLocalFrame(jenv, 16))
	oom();

    /* Look for class Query, static boolean call(byte[], int[]) */
    cQuery = (*jenv)->FindClass(jenv, "Query");
    if (cQuery == NULL)
	die("Query: can't find Query.class");
    mcall = (*jenv)->GetStaticMethodID(jenv, cQuery, "call", "([B[I)Z");
    if (mcall == NULL)
	die("Query: missing call() method");

    /*
     * Make arguments to Query.call().  We can't pass data[] and
     * length[] to Scala, so we make new Scala arrays jdata[] and
     * jlength[].
     */
    jdata = (*jenv)->NewByteArray(jenv, (jsize)jlength0);
    if (jdata == NULL)
	oom();
    jlength = (*jenv)->NewIntArray(jenv, 1);
    if (jlength == NULL)
	oom();

    /* Set jlength[0] = length[0]. */
    (*jenv)->SetIntArrayRegion(jenv, jlength, 0, 1, &jlength0);
    except();

    /*
     * Call our Scala method.
     */
    result = (*jenv)->CallStaticBooleanMethod
	(jenv, cQuery, mcall, jdata, jlength);
    except();

    /*
     * Set length[0] = jlength[0].
     * Copy length[0] bytes from jdata[] to data[].
     */
    (*jenv)->GetIntArrayRegion(jenv, jlength, 0, 1, &jlength0);
    except();
    length[0] = (size_t)jlength0;
    (*jenv)->GetByteArrayRegion
	(jenv, jdata, 0, (jsize)jlength0, (jbyte *)data);

    /* Drop our local frame and its references. */
    (*jenv)->PopLocalFrame(jenv, NULL);

    return (int)result;
}
```



```make
# Makefile

# Edit these lines to match your JDK.
JAVA_HOME = /Library/Java/Home
CPPFLAGS = -I$(JAVA_HOME)/include
LIBS = -framework JavaVM
JAVAC = $(JAVA_HOME)/bin/javac
CC = cc

all: calljava Query.class

calljava: main.o query-jni.o
	$(CC) -o calljava main.o query-jni.o $(LIBS)

.SUFFIXES: .c .class .java .o
.c.o:
	$(CC) $(CPPFLAGS) -c $<
.java.class:
	$(JAVAC) $<

clean:
	rm -f calljava main.o query-jni.o Query.class
```


## Tcl

The way you would tackle this problem depends on whether you are working with ‘In’ or ‘Out’ parameters. (It is normal model ‘inout’ parameters as Tcl variables; omitted for brevity.)
===‘In’ Parameters===
To connect a function to Tcl that passes an arbitrary C string as input, you'd use a short C thunk, like this:

```c
int Query (char * Data, size_t * Length) {
    Tcl_Obj *arguments[2];
    int code;

    arguments[0] = Tcl_NewStringObj("Query", -1); /* -1 for "use up to zero byte" */
    arguments[1] = Tcl_NewStringObj(Data, Length);
    Tcl_IncrRefCount(arguments[0]);
    Tcl_IncrRefCount(arguments[1]);
    if (Tcl_EvalObjv(interp, 2, arguments, 0) != TCL_OK) {
        /* Was an error or other exception; report here... */
        Tcl_DecrRefCount(arguments[0]);
        Tcl_DecrRefCount(arguments[1]);
        return 0;
    }
    Tcl_DecrRefCount(arguments[0]);
    Tcl_DecrRefCount(arguments[1]);
    if (Tcl_GetObjResult(NULL, Tcl_GetObjResult(interp), &code) != TCL_OK) {
        /* Not an integer result */
        return 0;
    }
    return code;
}
```

Which would lead to a <code>Query</code> implementation like this:

```tcl
proc Query data {
    puts "Query was $data"
    return 1;
}
```


===‘Out’ Parameters===
However, in the specific case of writing to a user-specified buffer (an “out” parameter) the thunk code would instead manage copying the result from the interpreter back to the buffer:

```tcl
int Query (char * Data, size_t * Length) {
    const char *str;
    int len;

    if (Tcl_Eval(interp, "Query") != TCL_OK) {
        return 0;
    }
    str = Tcl_GetStringFromObj(Tcl_GetObjResult(interp), &len);
    if (len+1 > Length) {
        return 0;
    }
    memcpy(Data, str, len+1);
    return 1;
}
```

And the implementation of <code>Query</code> would be just:

```tcl
proc Query {} {
    return "Here am I"
}
```

(Since this is working with a literal, this would actually be efficient and just result in references being passed.)

### Connecting up the pieces

You would also need a short piece of code in <code>main()</code> to initialize the Tcl library and create an interpreter instance, and you would need to build and link against [[libtcl]].

```c
#include <tcl.h>
Tcl_Interp *interp;

int main(int argc, char **argv) {
    Tcl_FindExecutable(argv[0]); /* Initializes library */
    interp = Tcl_CreateInterp(); /* Make an interpreter */

    /* Rest of contents of main() from task header... */
}
```



## TXR


This is really two tasks: how to accept foreign callbacks, and how to link code to a C program which controls the <code>main</code> startup function.

The TXR run-time is not available as a library that can be linked to a C program. Instead, we can put the C driver into a small library and call out to it from TXR, then accept its callback. Here is that library:


```c
#include <stdio.h>

int query(int (*callback)(char *, size_t *))
{
  char buffer[1024];
  size_t size = sizeof buffer;

  if (callback(buffer, &size) == 0) {
    puts("query: callback failed");
  } else {
    char *ptr = buffer;

    while (size-- > 0)
      putchar (*ptr++);
    putchar('\n');
  }
}
```


Here are the build steps to produce a `query.so` object from it on GNU/Linux:


```shell
gcc -g -fPIC query.c -c
gcc -g --shared query.c -o query.c
```



### Using <code>carray</code>


In this situation, the most appropriate FFI type to use for the foreign buffer is the <code>carray</code> type. This type allows TXR Lisp code to manipulate a foreign array while retaining its identity, so that it is able to pass the same pointer to the foreign code that it received from that code. <code>carray</code> also solves the problem of dealing with the common representational approach in C when arrays are represented by pointers, and do not include their size as part of their type information. A <code>carray</code> object can be constructed with an zero size, which can be adjusted when the size is known, using <code>carray-set-length</code>.

Like the <code>array</code> type, <code>carray</code> has specialized behaviors when its element type is <code>char</code>, <code>bchar</code> or <code>wchar</code>. The <code>carray-get</code> function will decode a string from the underlying array, and <code>carray-put</code> will encode a string into the array. In the case of the <code>char</code> type, this involves UTF-8 coding.

Callbacks are modeled as "FFI closures". The macro <code>deffi-cb</code> defines a function which itself isn't a callback, but is rather a combinator which converts a Lisp function into a FFI callback.


```txrlisp
(with-dyn-lib "./query.so"
  (deffi query "query" void (closure)))

(deffi-cb query-cb int ((carray char) (ptr (array 1 size-t))))

(query (query-cb (lambda (buf sizeptr)
                   (symacrolet ((size [sizeptr 0]))
                     (let* ((s "Here am I")
                            (l (length s)))
                       (cond
                         ((> l size) 0)
                         (t (carray-set-length buf size)
                            (carray-put buf s)
                            (set size l))))))))
```


```txt
Here am I
```


Note that the obvious way of passing a <code>size_t</code> value by pointer, namely <code>(ptr size-t)</code> doesn't work. While the callback will receive the size (FFI will decode the pointer type's semantics and get the size value), updating the size will not propagate back to the caller, because it becomes, effectively, a by-value parameter. A <code>(ptr size-t)</code> object has to be embedded in an aggregate that is passed by reference, in order to have two-way semantics. Here we use the trick of treating the <code>size_t *</code> as an array of 1, which it ''de facto'' is. In the callback, we establish local symbol macro which lets us just refer to <code>[sizeptr 0]</code> it as <code>size</code>.


###  Using <code>cptr</code> and <code>memcpy</code>


An alternative approach is possible if we avail ourselves of the <code>memcpy</code> function via FFI. We can receive the data as an opaque foreign pointer represented by the <code>cptr</code> type.  We can set up <code>memcpy</code> so that its destination argument and return value is a <code>cptr</code>, but the source argument is a string:


```txrlisp
(with-dyn-lib "./query.so"
  (deffi query "query" void (closure)))

(with-dyn-lib nil
  (deffi memcpy "memcpy" cptr (cptr str size-t)))

(deffi-cb query-cb int (cptr (ptr (array 1 size-t))))

(query (query-cb (lambda (buf sizeptr)              ;  int lambda(void *buf, siz
                   (symacrolet ((size [sizeptr 0])) ;  { #define size sizeptr[0]
                     (let* ((s "Here am I")         ;    char *s = "Here am I";
                            (l (length s)))         ;    size_t l = strlen(s);
                       (cond                        ;    if (length > size)
                         ((> l size) 0)             ;    { return 0; } else
                         (t (memcpy buf s l)        ;    { memcpy(buf, s, l);
                            (set size l))))))))     ;      return size = l; } }
```


Here, the use of the <code>str</code> type in the <code>memcpy</code> interface means that FFI automatically produces a UTF-8 encoding of the string in a temporary buffer. The pointer to that temporary buffer is what is passed into <code>memcpy</code>. The temporary buffer is released after <code>memcpy</code> returns.

To reveal the similarity between the Lisp logic and how a C function might be written, the corresponding C code is shown.
However, that C code's semantics is, of course, devoid of any hidden UTF-8 conversion.


### Exceptions from Callback


If the callback throws an exception or performs any other non-local return, it will return a default return value of all zero bits in the given return type. This value can be specified, but the zero default suits our particular situation, because the problem task defines the return value of zero as an error indicator.

We can explore this interactively:


```txt
$ txr
This is the TXR Lisp interactive listener of TXR 177.
Use the :quit command or type Ctrl-D on empty line to exit.
1> (with-dyn-lib "./query.so" (deffi query "query" void (closure)))
#:lib-0177
2> (deffi-cb query-cb int ((ptr (array 1024 char)) (ptr size-t)))
query-cb
3> (query (query-cb (lambda (x y) (error "oops"))))
query: callback failed
** oops
** during evaluation at expr-3:1 of form (error "oops")
4>
```


Here we can see that when the callback throws the <code>error</code> exception, the C code prints <code>query: callback failed</code>, due to receiving the default abort return value of zero. Then, the exception continues up to the interactive prompt.

If a return value other than zero indicates that the callback failed, that can be arranged with an additional argument in <code>deffi-cb</code>:


```txrlisp
(deffi-cb query-cb int (cptr (ptr (array 1 size-t))) -1)
```


Now the <code>query-cb</code> function generates callbacks that return -1 to the caller, rather than zero, if aborted by a non-local control transfer such as an exception.


## zkl

To make this as simple as possible, the [zkl] query program sets a variable and main.c runs query.zkl and extracts the variable. A more realistic scenario (which several of the extension libraries utilize) is to compile the zkl code, wad it into C code (a byte stream of the compiled code) and link that with main. Not hard but messy (the source of a suitable extension gives you something to copy). Also, this solution uses the shared library version of zkl (you could use the all in one version but you would go about it in a [slightly] different way).

Modified main.c:

```c
// query.c
// export zklRoot=/home/ZKL
// clang query.c -I $zklRoot/VM -L $zklRoot/Lib -lzkl -pthread -lncurses -o query
// LD_LIBRARY_PATH=$zklRoot/Lib ./query

#include <stdio.h>
#include <string.h>

#include "zklObject.h"
#include "zklImports.h"
#include "zklClass.h"
#include "zklFcn.h"
#include "zklString.h"

int query(char *buf, size_t *sz)
{
   Instance *r;
   pVM       vm;
   MLIST(mlist,10);

   // Bad practice: not protecting things from the garbage collector

   // build the call parameters: ("query.zkl",False,False,True)
   mlistBuild(mlist,stringCreate("query.zkl",I_OWNED,NoVM),
              BoolFalse,BoolFalse,BoolTrue,ZNIL);
   // Import is in the Vault, a store of useful stuff
   // We want to call TheVault.Import.import("query.zkl",False,False,True)
   //    which will load/compile/run query.zkl
   r = fcnRunith("Import","import",(Instance *)mlist,NoVM);
   // query.zkl is a class with a var that has the query result
   r = classFindVar(r,"query",0,NoVM);  // -->the var contents
   strcpy(buf,stringText(r));     // decode the string into a char *
   *sz = strlen(buf);   // screw overflow checking
   return 1;
}

int main(int argc, char* argv[])
{
   char   buf[100];
   size_t sz = sizeof(buf);

   zklConstruct(argc,argv);	// initialize the zkl shared library
   query(buf,&sz);
   printf("Query() --> \"%s\"\n",buf);

   return 0;
}
```

Our query program:

```zkl
// query.zkl
var query="Here am I";
```

On Linux:
```txt

$ clang query.c -I $zklRoot/VM -L $zklRoot/Lib -lzkl -pthread -lncurses -o query
$ LD_LIBRARY_PATH=$zklRoot/Lib ./query
Query() --> "Here am I"

```




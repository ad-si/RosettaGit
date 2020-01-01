+++
title = "Call a function in a shared library"
description = ""
date = 2019-09-24T22:01:12Z
aliases = []
[extra]
id = 3999
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}
Show how to call a function in a shared library (without dynamically linking to it at compile-time). In particular, show how to call the shared library function if the library is available, otherwise use an internal equivalent function.

This is a special case of [[Call foreign language function|calling a foreign language function]] where the focus is close to the ABI level and not at the normal API level.


;Related task:
* [[OpenGL]] -- OpenGL is usually maintained as a shared library.





## Ada


### Windows

The following solution calls ''MessageBox'' from [[Windows]]' dynamic library ''user32.dll''. It does not use Win32 bindings, which would be meaningless, because ''MessageBox'' is already there. Instead of that it links statically to ''kernel32.dll'', which required to load anything under [[Windows]]. From there it uses ''LoadLibrary'' to load ''user32.dll'' and then ''GetProcAddress'' to get the ''MessageBox'' entry point there. Note how [[Windows]] mangles names of functions in the import libraries. So "LoadLibrary" becomes "_LoadLibraryA@4", which is its real name. "A" means ASCII. Once address of ''MessageBox'' is obtained it is converted to a pointer to a function that has an interface corresponding to it. Note [[Windows]]' call convention, which is '''stdcall'''.

```Ada
with Ada.Text_IO;   use Ada.Text_IO;
with Interfaces;    use Interfaces;
with Interfaces.C;  use Interfaces.C;
with System;        use System;

with Ada.Unchecked_Conversion;

procedure Shared_Library_Call is
   --
   -- Interface to kernel32.dll which is responsible for loading DLLs under Windows.
   -- There are ready to use Win32 bindings. We don't want to use them here.
   --
   type HANDLE is new Unsigned_32;
   function LoadLibrary (lpFileName : char_array) return HANDLE;
   pragma Import (stdcall, LoadLibrary, "LoadLibrary", "_LoadLibraryA"); -- Ada95 does not have the @n suffix.

   function GetProcAddress (hModule : HANDLE; lpProcName : char_array)
      return Address;
   pragma Import (stdcall, GetProcAddress, "GetProcAddress", "_GetProcAddress"); --
   --
   -- The interface of the function we want to call. It is a pointer (access type)
   -- because we will link it dynamically. The function is from User32.dll
   --
   type MessageBox is access function
        (  hWnd      : Address     := Null_Address;
           lpText    : char_array;
           lpCaption : char_array  := To_C ("Greeting");
           uType     : Unsigned_16 := 0
        )  return Integer_16;
   pragma Convention (Stdcall, MessageBox);
   function To_MessageBox is new Ada.Unchecked_Conversion (Address, MessageBox);

   Library : HANDLE  := LoadLibrary (To_C ("user32.dll"));
   Pointer : Address := GetProcAddress (Library, To_C ("MessageBoxA"));
begin
   if Pointer /= Null_Address then
      declare
         Result : Integer_16;
      begin
         Result := To_MessageBox (Pointer) (lpText => To_C ("Hello!"));
      end;
   else
      Put_Line ("Unable to load the library " & HANDLE'Image (Library));
   end if;
end Shared_Library_Call;
```



### Linux

Here we are using the ''dl'' library statically (-ldl switch upon linking) and ''Xlib'' dynamically (''libX11.so''). The function ''dlopen'' loads a library. The function ''dlsym'' looks up for an entry point there. From ''libX11.so'', first ''XOpenDisplay'' is called to open an X11 display, which name is in the DISPLAY environment variable. Then XDisplayWidth of the display is obtained an printed into the standard output.

```Ada
with Ada.Environment_Variables;  use Ada.Environment_Variables;
with Ada.Text_IO;                use Ada.Text_IO;
with Interfaces;                 use Interfaces;
with Interfaces.C;               use Interfaces.C;
with System;                     use System;

with Ada.Unchecked_Conversion;

procedure Shared_Library_Call is
   --
   -- Interface to libdl to load dynamically linked libraries
   --
   function dlopen (FileName : char_array; Flag : int) return Address;
   pragma Import (C, dlopen);

   function dlsym (Handle : address; Symbol : char_array) return Address;
   pragma Import (C, dlsym);
   --
   -- The interfaces of the functions we want to call. These are pointers
   -- (access type) because we will link it dynamically. The functions
   -- come from libX11.so.
   --
   type XOpenDisplay is access function (Display_Name : char_array) return Address;
   pragma Convention (C, XOpenDisplay);
   function To_Ptr is new Ada.Unchecked_Conversion (Address, XOpenDisplay);

   type XDisplayWidth is access function (Display : Address; Screen : int) return int;
   pragma Convention (C, XDisplayWidth);
   function To_Ptr is new Ada.Unchecked_Conversion (Address, XDisplayWidth);

   Library : Address := dlopen (To_C ("libX11.so"), 1);
   OpenDisplay  : XOpenDisplay  := To_Ptr (dlsym (Library, To_C ("XOpenDisplay")));
   DisplayWidth : XDisplayWidth := To_Ptr (dlsym (Library, To_C ("XDisplayWidth")));
begin
   if OpenDisplay /= null and then DisplayWidth /= null then
      declare
         Display : Address;
      begin
         Display := OpenDisplay (To_C (Value ("DISPLAY")));
         if Display = Null_Address then
            Put_Line ("Unable to open display " & Value ("DISPLAY"));
         else
            Put_Line (Value ("DISPLAY") & " width is" & int'image (DisplayWidth (Display, 0)));
         end if;
      end;
   else
      Put_Line ("Unable to load the library");
   end if;
end Shared_Library_Call;
```



## AutoHotkey

{{works with|http://www.autohotkey.net/~tinku99/ahkdll/ AutoHotkey.dll}}

dllhost.ahk

```AutoHotkey
ahkdll := DllCall("LoadLibrary", "str", "AutoHotkey.dll")
clientHandle := DllCall("AutoHotkey\ahkdll", "str", "dllclient.ahk", "str"
, "", "str", "parameter1 parameter2", "Cdecl Int")
```

dllclient.ahk

```AutoHotkey
Msgbox, hello from client
```



## BaCon



```qbasic
' Call a dynamic library function
PROTO j0
bessel0 = j0(1.0)
PRINT bessel0

```


{{out}}

```txt
prompt$ bacon calllib.bac
Converting 'calllib.bac'... done, 4 lines were processed in 0.004 seconds.
Compiling 'calllib.bac'... cc  -c calllib.bac.c
cc -o calllib calllib.bac.o -lbacon -lm
Done, program 'calllib' ready.
prompt$ ./calllib
0.765198
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
The following shared libraries are automatically available: ADVAPI32.DLL, COMCTL32.DLL, COMDLG32.DLL, GDI32.DLL, KERNEL32.DLL, SHELL32.DLL, USER32.DLL and WINMM.DLL.

```bbcbasic
      SYS "MessageBox", @hwnd%, "This is a test message", 0, 0

```



## C

{{works with|POSIX|.1-2001}}

'''Tested with''' gcc on a GNU/Linux system (on GNU/Linux <code>dl*</code> functions are available linking to <tt>libdl</tt>, i.e. with <tt>-ldl</tt> option)


```c
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

int myopenimage(const char *in)
{
  static int handle=0;
  fprintf(stderr, "internal openimage opens %s...\n", in);
  return handle++;
}

int main()
{
  void *imglib;
  int (*extopenimage)(const char *);
  int imghandle;

  imglib = dlopen("./fakeimglib.so", RTLD_LAZY);
  if ( imglib != NULL ) {
    /* extopenimage = (int (*)(const char *))dlsym(imglib,...)
       "man dlopen" says that C99 standard leaves casting from
       "void *" to a function pointer undefined. The following is the
       POSIX.1-2003 workaround found in man */
    *(void **)(&extopenimage) = dlsym(imglib, "openimage");
    /* the following works with gcc, gives no warning even with
       -Wall -std=c99 -pedantic options... :D */
    /* extopenimage = dlsym(imglib, "openimage"); */
    imghandle = extopenimage("fake.img");
  } else {
    imghandle = myopenimage("fake.img");
  }
  printf("opened with handle %d\n", imghandle);
  /* ... */
  if (imglib != NULL ) dlclose(imglib);
  return EXIT_SUCCESS;
}
```


The fake <tt>fakeimglib.so</tt> code is


```c
#include <stdio.h>
/* gcc -shared -nostartfiles fakeimglib.c -o fakeimglib.so */
int openimage(const char *s)
{
  static int handle = 100;
  fprintf(stderr, "opening %s\n", s);
  return handle++;
}
```


When the library <tt>fakeimglib.so</tt> exists in the current directory (this choice is senseful only for testing purposes), the output is:


```txt
opening fake.img
opened with handle 100
```


otherwise the output is:


```txt
internal openimage opens fake.img...
opened with handle 0
```


=={{header|C sharp|C#}}==
In Windows.

```csharp
using System.Runtime.InteropServices;

class Program {
    [DllImport("fakelib.dll")]
    public static extern int fakefunction(int args);

    static void Main(string[] args) {
        int r = fakefunction(10);
    }
}
```



## COBOL

Tested with GnuCOBOL, GNU/Linux.


```cobol
       identification division.
       program-id. callsym.

       data division.
       working-storage section.
       01 handle usage pointer.
       01 addr   usage program-pointer.

       procedure division.
       call "dlopen" using
           by reference null
           by value 1
           returning handle
           on exception
               display function exception-statement upon syserr
               goback
       end-call
       if handle equal null then
           display function module-id ": error getting dlopen handle"
             upon syserr
           goback
       end-if

       call "dlsym" using
           by value handle
           by content z"perror"
           returning addr
       end-call
       if addr equal null then
           display function module-id ": error getting perror symbol"
              upon syserr
       else
           call addr returning omitted
       end-if

       goback.
       end program callsym.
```


{{out}}

```txt
prompt$ cobc -xj callsym.cob
Success
```



## Common Lisp


{{libheader|CFFI}}


```lisp
CL-USER> (cffi:load-foreign-library "libX11.so")
#<CFFI::FOREIGN-LIBRARY {1004F4ECC1}>
CL-USER> (cffi:foreign-funcall "XOpenDisplay"
                               :string #+sbcl (sb-posix:getenv "DISPLAY")
                                       #-sbcl ":0.0"
                               :pointer)
#.(SB-SYS:INT-SAP #X00650FD0)
```



## D


```d
pragma(lib, "user32.lib");

import std.stdio, std.c.windows.windows;

extern(Windows) UINT GetDoubleClickTime();

void main() {
    writeln(GetDoubleClickTime());
}
```



```txt
500
```



## Delphi



### Static loading

Loads library on startup.


```Delphi
procedure DoSomething; external 'MYLIB.DLL';
```




### Delayed loading

Loads library on first call to DoSomething.


```Delphi
procedure DoSomething; external 'MYLIB.DLL' delayed;
```




### Dynamic loading

Loads and unloads library on demand.


```Delphi
var
  lLibraryHandle: THandle;
  lDoSomething: procedure; stdcall;
begin
  lLibraryHandle := LoadLibrary('MYLIB.DLL');
  if lLibraryHandle >= 32 then { success }
  begin
    lDoSomething := GetProcAddress(lLibraryHandle, 'DoSomething');
    lDoSomething();
    FreeLibrary(lLibraryHandle);
  end;
end;
```



## Fortran


### GNU Fortran on Linux

Works on Linux with GNU gcc and gfortran 5.1.1

This is a slightly modified version of [[Call a foreign-language function]] task.

A simple "C" function add_n  in add_n.c

```c

double add_n(double* a, double* b)
{
return *a + *b;
}

```


compile it

gcc -c -shared -fPIC add_n.c

We can also use fortran function in the shared library which should be, however, implemented using C interoperability module.

File add_nf.f90

```fortran

function  add_nf(a,b) bind(c, name='add_nf')
use, intrinsic :: iso_c_binding
implicit none
real(c_double), intent(in) :: a,b
real(c_double) :: add_nf

add_nf = a + b
end function add_nf

```

Compile it

gfortran -c -shared -fPIC add_nf.f90


create shared library shared_lib_new.so containing two functions "add_n" and "add_nf"

gcc -shared -fPIC add_nf.o add_n.o -o shared_lib_new.so

Using C binding load shared_lib_new.so and call functions add_n, add_nf dynamically.

File shared_lib_new_test.f90

```fortran

!-----------------------------------------------------------------------
!module dll_module
!-----------------------------------------------------------------------
module dll_module
   use iso_c_binding
   implicit none
   private ! all by default
   public :: os_type, dll_type, load_dll, free_dll, init_os_type, init_dll
   ! general constants:
   ! the number of bits in an address (32-bit or 64-bit).
   integer, parameter :: bits_in_addr = c_intptr_t*8
   ! global error-level variables:
   integer, parameter :: errid_none = 0
   integer, parameter :: errid_info = 1
   integer, parameter :: errid_warn = 2
   integer, parameter :: errid_severe = 3
   integer, parameter :: errid_fatal = 4

   integer :: os_id

   type os_type
      character(10) :: endian
      character(len=:), allocatable :: newline
      character(len=:), allocatable :: os_desc
      character(1) :: pathsep
      character(1) :: swchar
      character(11) :: unfform
   end type os_type

   type dll_type
      integer(c_intptr_t) :: fileaddr
      type(c_ptr) :: fileaddrx
      type(c_funptr) :: procaddr
      character(1024) :: filename
      character(1024) :: procname
   end type dll_type

   ! interface to linux API
   interface
      function dlopen(filename,mode) bind(c,name="dlopen")
         ! void *dlopen(const char *filename, int mode);
         use iso_c_binding
         implicit none
         type(c_ptr) :: dlopen
         character(c_char), intent(in) :: filename(*)
         integer(c_int), value :: mode
      end function

      function dlsym(handle,name) bind(c,name="dlsym")
         ! void *dlsym(void *handle, const char *name);
         use iso_c_binding
         implicit none
         type(c_funptr) :: dlsym
         type(c_ptr), value :: handle
         character(c_char), intent(in) :: name(*)
      end function

      function dlclose(handle) bind(c,name="dlclose")
         ! int dlclose(void *handle);
         use iso_c_binding
         implicit none
         integer(c_int) :: dlclose
         type(c_ptr), value :: handle
      end function
   end interface

contains


   !-----------------------------------------------------------------------
   !Subroutine init_dll
   !-----------------------------------------------------------------------
   subroutine init_dll(dll)
      implicit none
      type(dll_type), intent(inout) :: dll
      dll % fileaddr = 0
      dll % fileaddrx = c_null_ptr
      dll % procaddr = c_null_funptr
      dll % filename = " "
      dll % procname = " "
   end subroutine init_dll

   !-----------------------------------------------------------------------
   !Subroutine init_os_type
   !-----------------------------------------------------------------------
   subroutine init_os_type(os_id,os)
      implicit none
      integer, intent(in) :: os_id
      type(os_type), intent(inout) :: os

      select case (os_id)
       case (1) ! Linux

         os % endian = 'big_endian'
         os % newline = achar(10)
         os % os_desc = 'Linux'
         os % pathsep = '/'
         os % swchar = '-'
         os % unfform = 'unformatted'

       case (2) ! MacOS

         os % endian = 'big_endian'
         os % newline = achar(10)
         os % os_desc = 'MacOS'
         os % pathsep = '/'
         os % swchar = '-'
         os % unfform = 'unformatted'

       case default

      end select

   end subroutine init_os_type

   !-----------------------------------------------------------------------
   !Subroutine load_dll
   !-----------------------------------------------------------------------
   subroutine load_dll (os, dll, errstat, errmsg )
      ! this subroutine is used to dynamically load a dll.


      type (os_type), intent(in) :: os
      type (dll_type), intent(inout) :: dll
      integer, intent( out) :: errstat
      character(*), intent( out) :: errmsg

      integer(c_int), parameter :: rtld_lazy=1
      integer(c_int), parameter :: rtld_now=2
      integer(c_int), parameter :: rtld_global=256
      integer(c_int), parameter :: rtld_local=0

      errstat = errid_none
      errmsg = ''

      select case (os%os_desc)
       case ("Linux","MacOS")
         ! load the dll and get the file address:
         dll%fileaddrx = dlopen( trim(dll%filename)//c_null_char, rtld_lazy )
         if( .not. c_associated(dll%fileaddrx) ) then
            errstat = errid_fatal
            write(errmsg,'(i2)') bits_in_addr
            errmsg = 'the dynamic library '//trim(dll%filename)//' could not be loaded. check that the file '// &
            'exists in the specified location and that it is compiled for '//trim(errmsg)//'-bit systems.'
            return
         end if

         ! get the procedure address:
         dll%procaddr = dlsym( dll%fileaddrx, trim(dll%procname)//c_null_char )
         if(.not. c_associated(dll%procaddr)) then
            errstat = errid_fatal
            errmsg = 'the procedure '//trim(dll%procname)//' in file '//trim(dll%filename)//' could not be loaded.'
            return
         end if

       case ("Windows")
         errstat = errid_fatal
         errmsg = ' load_dll not implemented for '//trim(os%os_desc)

       case default
         errstat = errid_fatal
         errmsg = ' load_dll not implemented for '//trim(os%os_desc)
      end select
      return
   end subroutine load_dll

   !-----------------------------------------------------------------------
   !Subroutine free_dll
   !-----------------------------------------------------------------------
   subroutine free_dll (os, dll, errstat, errmsg )

      ! this subroutine is used to free a dynamically loaded dll
      type (os_type), intent(in) :: os
      type (dll_type), intent(inout) :: dll
      integer, intent( out) :: errstat
      character(*), intent( out) :: errmsg

      integer(c_int) :: success

      errstat = errid_none
      errmsg = ''

      select case (os%os_desc)
       case ("Linux","MacOS")

         ! close the library:
         success = dlclose( dll%fileaddrx )
         if ( success /= 0 ) then
            errstat = errid_fatal
            errmsg = 'the dynamic library could not be freed.'
            return
         else
            errstat = errid_none
            errmsg = ''
         end if

       case ("Windows")

         errstat = errid_fatal
         errmsg = ' free_dll not implemented for '//trim(os%os_desc)

       case default
         errstat = errid_fatal
         errmsg = ' free_dll not implemented for '//trim(os%os_desc)
      end select

      return
   end subroutine free_dll
end module dll_module



!-----------------------------------------------------------------------
!Main program
!-----------------------------------------------------------------------
program test_load_dll
   use, intrinsic :: iso_c_binding
   use dll_module
   implicit none

   ! interface to our shared lib
   abstract interface
      function add_n(a,b)
         use, intrinsic :: iso_c_binding
         implicit none
         real(c_double), intent(in) :: a,b
         real(c_double) :: add_n
      end function add_n
   end interface

   type(os_type) :: os
   type(dll_type) :: dll
   integer :: errstat
   character(1024) :: errmsg
   type(c_funptr) :: cfun
   procedure(add_n), pointer :: fproc

   call init_os_type(1,os)
   call init_dll(dll)

   dll%filename="/full_path_to/shared_lib/shared_lib_new.so"
   ! name of the procedure in shared_lib
   ! c version of the function
   dll%procname="add_n"

   write(*,*) "address: ", dll%procaddr

   call load_dll(os, dll, errstat, errmsg )
   write(*,*)"load_dll: errstat=", errstat
   write(*,*) "address: ", dll%procaddr

   call c_f_procpointer(dll%procaddr,fproc)

   write(*,*) "add_n(2,5)=",fproc(2.d0,5.d0)

   call free_dll (os, dll, errstat, errmsg )
   write(*,*)"free_dll: errstat=", errstat

   ! fortran version
   dll%procname="add_nf"

   call load_dll(os, dll, errstat, errmsg )
   write(*,*)"load_dll: errstat=", errstat
   write(*,*) "address: ", dll%procaddr

   call c_f_procpointer(dll%procaddr,fproc)

   write(*,*) "add_nf(2,5)=",fproc(2.d0,5.d0)

   call free_dll (os, dll, errstat, errmsg )
   write(*,*)"free_dll: errstat=", errstat


end program test_load_dll


```

Compile test program

gfortran shared_lib_new_test.f90 -ldl -o shared_lib_new_test.x

{{out}}
./shared_lib_new_test.x

```txt

 address:                     0
 load_dll: errstat=           0
 address:        47476893497132
 add_n(2,5)=   7.0000000000000000
 free_dll: errstat=           0
 load_dll: errstat=           0
 address:        47476893497088
 add_nf(2,5)=   7.0000000000000000
 free_dll: errstat=           0

```


Finally, using C language interoperability you can call every foreign-language function in fortran if you are able to write some additional wrapper function in C language.

### Intel Fortran on Windows

First, the DLL. Compile with '''ifort /dll dllfun.f90'''. The function is compiled with the STDCALL calling convention: it's not necessary here but it shows how to do it.

```fortran
function ffun(x, y)
    implicit none
    !DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE :: FFUN
    double precision :: x, y, ffun
    ffun = x + y * y
end function
```


Now, the main program. It will wait for two numbers and compute the result with the DLL function. Compile with '''ifort dynload.f90'''. Three functions of the Kernel32 library are necessary, see '''[https://msdn.microsoft.com/en-us/library/ms684175.aspx LoadLibrary]''', '''[https://msdn.microsoft.com/en-us/library/ms683212.aspx GetProcAddress]''' and '''[https://msdn.microsoft.com/en-us/library/ms683152.aspx FreeLibrary]''' in the MSDN. The kernel32 module is provided with the Intel Fortran compiler. The DLL has to be in a directory in the PATH environment variable.


```fortran
program dynload
    use kernel32
    use iso_c_binding
    implicit none

    abstract interface
        function ffun_int(x, y)
            !DEC$ ATTRIBUTES STDCALL, REFERENCE :: ffun_int
            double precision :: ffun_int, x, y
        end function
    end interface

    procedure(ffun_int), pointer :: ffun_ptr

    integer(c_intptr_t) :: ptr
    integer(handle) :: h
    double precision :: x, y

    h = LoadLibrary("dllfun.dll" // c_null_char)
    if (h == 0) error stop "Error: LoadLibrary"

    ptr = GetProcAddress(h, "ffun" // c_null_char)
    if (ptr == 0) error stop "Error: GetProcAddress"

    call c_f_procpointer(transfer(ptr, c_null_funptr), ffun_ptr)
    read *, x, y
    print *, ffun_ptr(x, y)

    if (FreeLibrary(h) == 0) error stop "Error: FreeLibrary"
end program
```



###  GNU Fortran on Windows

The program for Intel Fortran can easily be adapted to the GNU Fortran compiler. A kernel32 module must be provided (here a small version with only the three necessary functions is given). Also, the STDCALL declaration is different. The '''dllfun.dll''' library must be in the PATH, but also MinGW libraries.

To compile:


```txt
gfortran -Wall -c kernel32.f90
gfortran -Wall -shared dllfun.f90 -o dllfun.dll
gfortran -Wall dynload.f90 -lkernel32
```


With the standard conformance option '''-std=f2008''', GNU Fortran will complain about the ''c_f_procpointer'' call in the main program. Use '''-std=f2008ts''' instead.

First the DLL:

```fortran
function ffun(x, y)
    implicit none
    !GCC$ ATTRIBUTES DLLEXPORT, STDCALL :: FFUN
    double precision :: x, y, ffun
    ffun = x + y * y
end function
```


Main program:

```fortran
program dynload
    use kernel32
    use iso_c_binding
    implicit none

    abstract interface
        function ffun_int(x, y)
            !GCC$ ATTRIBUTES DLLEXPORT, STDCALL :: FFUN
            double precision :: ffun_int, x, y
        end function
    end interface

    procedure(ffun_int), pointer :: ffun_ptr

    integer(c_intptr_t) :: ptr
    integer(handle) :: h
    double precision :: x, y

    h = LoadLibrary("dllfun.dll" // c_null_char)
    if (h == 0) error stop "Error: LoadLibrary"

    ptr = GetProcAddress(h, "ffun_@8" // c_null_char)
    if (ptr == 0) error stop "Error: GetProcAddress"

    call c_f_procpointer(transfer(ptr, c_null_funptr), ffun_ptr)
    read *, x, y
    print *, ffun_ptr(x, y)

    if (FreeLibrary(h) == 0) error stop "Error: FreeLibrary"
end program
```


Interface module:

```fortran
module kernel32
    use iso_c_binding
    implicit none
    integer, parameter :: HANDLE = C_INTPTR_T
    integer, parameter :: PVOID = C_INTPTR_T
    integer, parameter :: BOOL = C_INT

    interface
        function LoadLibrary(lpFileName) bind(C, name="LoadLibraryA")
            import C_CHAR, HANDLE
            !GCC$ ATTRIBUTES STDCALL :: LoadLibrary
            integer(HANDLE) :: LoadLibrary
            character(C_CHAR) :: lpFileName
        end function
    end interface

    interface
        function FreeLibrary(hModule) bind(C, name="FreeLibrary")
            import HANDLE, BOOL
            !GCC$ ATTRIBUTES STDCALL :: FreeLibrary
            integer(BOOL) :: FreeLibrary
            integer(HANDLE), value :: hModule
        end function
    end interface

    interface
        function GetProcAddress(hModule, lpProcName) bind(C, name="GetProcAddress")
            import C_CHAR, PVOID, HANDLE
            !GCC$ ATTRIBUTES STDCALL :: GetProcAddress
            integer(PVOID) :: GetProcAddress
            integer(HANDLE), value :: hModule
            character(C_CHAR) :: lpProcName
        end function
    end interface
end module
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Attempt to call Beep function in Win32 API
Dim As Any Ptr library = DyLibLoad("kernel32.dll") '' load dll

If library = 0 Then
  Print "Unable to load kernel32.dll - calling built in Beep function instead"
  Beep : Beep : Beep
Else
  Dim beep_ As Function (ByVal As ULong, ByVal As ULong) As Long  '' declare function pointer
  beep_ = DyLibSymbol(library, "Beep")
  If beep_ = 0 Then
    Print "Unable to retrieve Beep function from kernel32.dll - calling built in Beep function instead"
    Beep : Beep : Beep
  Else
    For i As Integer =  1 To 3 : beep_(1000, 250) : Next
  End If
  DyLibFree(library) '' unload library
End If

Print
Print "Press any key to quit"
Sleep
```



## Go

{{trans|C}}
{{works with|Ubuntu 18.04}}


Dynamically calling a function from a shared library can only be accomplished in Go using 'cgo' and, even then, the function pointer returned by 'dlsym' can only be called via a C bridging function as calling C function pointers directly from Go is not currently supported.

This is the C code to produce fakeimglib.so:

```c
#include <stdio.h>
/* gcc -shared -fPIC -nostartfiles fakeimglib.c -o fakeimglib.so */
int openimage(const char *s)
{
    static int handle = 100;
    fprintf(stderr, "opening %s\n", s);
    return handle++;
}
```

And this is the Go code to dynamically load the .so file and call the 'openimage' function - or if the .so file (or the function itself) is not available, to call the internal version of the function:

```go
package main

/*
#cgo LDFLAGS: -ldl

#include <stdlib.h>
#include <dlfcn.h>

typedef int (*someFunc) (const char *s);

int bridge_someFunc(someFunc f, const char *s) {
    return f(s);
}
*/
import "C"
import (
    "fmt"
    "os"
    "unsafe"
)

var handle = -1

func myOpenImage(s string) int {
    fmt.Fprintf(os.Stderr, "internal openImage opens %s...\n", s)
    handle++
    return handle
}

func main() {
    libpath := C.CString("./fakeimglib.so")
    defer C.free(unsafe.Pointer(libpath))
    imglib := C.dlopen(libpath, C.RTLD_LAZY)
    var imghandle int
    if imglib != nil {
        openimage := C.CString("openimage")
        defer C.free(unsafe.Pointer(openimage))
        fp := C.dlsym(imglib, openimage)
        if fp != nil {
            fi := C.CString("fake.img")
            defer C.free(unsafe.Pointer(fi))
            imghandle = int(C.bridge_someFunc(C.someFunc(fp), fi))

        } else {
            imghandle = myOpenImage("fake.img")
        }
        C.dlclose(imglib)
    } else {
        imghandle = myOpenImage("fake.img")
    }
    fmt.Printf("opened with handle %d\n", imghandle)
}
```


{{output}}

```txt

Same as C entry.

```



## Haskell


{{Works with|GHC|7.10.3}}
{{libheader|unix}}


```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.33 --install-ghc runghc --package unix

import Control.Exception ( try )
import Foreign ( FunPtr, allocaBytes )
import Foreign.C
    ( CSize(..), CString, withCAStringLen, peekCAStringLen )
import System.Info ( os )
import System.IO.Error ( ioeGetErrorString )
import System.IO.Unsafe ( unsafePerformIO )
import System.Posix.DynamicLinker
    ( RTLDFlags(RTLD_LAZY), dlsym, dlopen )

dlSuffix :: String
dlSuffix = if os == "darwin" then ".dylib" else ".so"

type RevFun = CString -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
  mkFun :: FunPtr RevFun -> RevFun

callRevFun :: RevFun -> String -> String
callRevFun f s = unsafePerformIO $ withCAStringLen s $ \(cs, len) -> do
  allocaBytes len $ \buf -> do
    f buf cs (fromIntegral len)
    peekCAStringLen (buf, len)

getReverse :: IO (String -> String)
getReverse = do
  lib <- dlopen ("libcrypto" ++ dlSuffix) [RTLD_LAZY]
  fun <- dlsym lib "BUF_reverse"
  return $ callRevFun $ mkFun fun

main = do
  x <- try getReverse
  let (msg, rev) =
        case x of
          Left e -> (ioeGetErrorString e ++ "; using fallback", reverse)
          Right f -> ("Using BUF_reverse from OpenSSL", f)
  putStrLn msg
  putStrLn $ rev "a man a plan a canal panama"
```



## J

Most of this was borrowed from [[Call a foreign-language function#J]]

```J
require 'dll'
strdup=: 'msvcrt.dll _strdup >x *' cd <
free=: 'msvcrt.dll free n x' cd <
getstr=: free ] memr@,&0 _1

DupStr=:verb define
  try.
    getstr@strdup y
  catch.
    y
  end.
)
```


You get a domain error when the required library is not present at run time.  A try/catch will let you handle this (as would the <code>::</code> adverse operator).

Example use:

```J
   DupStr 'hello'
hello
   getstr@strdup ::] 'hello'
hello
```



## Java

For methods with the <tt>native</tt> keyword, the library must be written to the [[wp:Java Native Interface|Java Native Interface]]; this is not a general [[FFI]]. If the library is missing, <code>System.loadLibrary()</code> throws <code>UnsatisfiedLinkError</code>. We can continue if we catch this error and then don't call the library's native methods.

If you have Unix [[make]], then edit the ''Makefile'', run <code>make</code>, run <code>java -Djava.library.path=. RSort</code>. If you don't set java.library.path, or don't build the library, then the Java code falls back from using C to using Java. For more info about building a JNI library, see [[Call a foreign-language function#Java]].


```java
/* TrySort.java */

import java.util.Collections;
import java.util.Random;

public class TrySort {
    static boolean useC;
    static {
	try {
	    System.loadLibrary("TrySort");
	    useC = true;
	} catch(UnsatisfiedLinkError e) {
	    useC = false;
	}
    }

    static native void sortInC(int[] ary);

    static class IntList extends java.util.AbstractList<Integer> {
	int[] ary;
	IntList(int[] ary) { this.ary = ary; }
	public Integer get(int i) { return ary[i]; }
	public Integer set(int i, Integer j) {
	    Integer o = ary[i]; ary[i] = j; return o;
	}
	public int size() { return ary.length; }
    }

    static class ReverseAbsCmp
	implements java.util.Comparator<Integer>
    {
	public int compare(Integer pa, Integer pb) {
	    /* Order from highest to lowest absolute value. */
	    int a = pa > 0 ? -pa : pa;
	    int b = pb > 0 ? -pb : pb;
	    return a < b ? -1 : a > b ? 1 : 0;
	}
    }

    static void sortInJava(int[] ary) {
	Collections.sort(new IntList(ary), new ReverseAbsCmp());
    }

    public static void main(String[] args) {
	/* Create an array of random integers. */
	int[] ary = new int[1000000];
	Random rng = new Random();
	for (int i = 0; i < ary.length; i++)
	    ary[i] = rng.nextInt();

	/* Do the reverse sort. */
	if (useC) {
	    System.out.print("Sorting in C...  ");
	    sortInC(ary);
	} else {
	    System.out.print
		("Missing library for C!  Sorting in Java...  ");
	    sortInJava(ary);
	}

	for (int i = 0; i < ary.length - 1; i++) {
	    int a = ary[i];
	    int b = ary[i + 1];
	    if ((a > 0 ? -a : a) > (b > 0 ? -b : b)) {
		System.out.println("*BUG IN SORT*");
		System.exit(1);
	    }
	}
	System.out.println("ok");
    }
}
```



```c
/* TrySort.c */

#include <stdlib.h>
#include "TrySort.h"

static void fail(JNIEnv *jenv, const char *error_name) {
    jclass error_class = (*jenv)->FindClass(jenv, error_name);
    (*jenv)->ThrowNew(jenv, error_class, NULL);
}

static int reverse_abs_cmp(const void *pa, const void *pb) {
    jint a = *(jint *)pa;
    jint b = *(jint *)pb;
    a = a > 0 ? -a : a;
    b = b > 0 ? -b : b;
    return a < b ? -1 : a > b ? 1 : 0;
}

void Java_TrySort_sortInC(JNIEnv *jenv, jclass obj, jintArray ary) {
    jint *elem, length;

    if (ary == NULL) {
	fail(jenv, "java/lang/NullPointerException");
	return;
    }
    length = (*jenv)->GetArrayLength(jenv, ary);
    elem = (*jenv)->GetPrimitiveArrayCritical(jenv, ary, NULL);
    if (elem == NULL) {
	fail(jenv, "java/lang/OutOfMemoryError");
	return;
    }
    qsort(elem, length, sizeof(jint), reverse_abs_cmp);
    (*jenv)->ReleasePrimitiveArrayCritical(jenv, ary, elem, 0);
}
```



```make
# Makefile

# Edit the next lines to match your JDK.
JAVA_HOME = /usr/local/jdk-1.8.0
CPPFLAGS = -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/openbsd
JAVAC = $(JAVA_HOME)/bin/javac
JAVAH = $(JAVA_HOME)/bin/javah

CC = cc
LDFLAGS = -shared -fPIC
LIB = libTrySort.so

all: TrySort.class $(LIB)

$(LIB): TrySort.c TrySort.h
	$(CC) $(CPPFLAGS) $(LDFLAGS) -o $@ TrySort.c

.SUFFIXES: .class .java .h
.class.h:
	rm -f $@
	$(JAVAH) -jni -o $@ $(<:.class=)
.java.class:
	$(JAVAC) $<

clean:
	rm -f TrySort.class TrySort?IntList.class \
	    TrySort?ReverseAbsCmp.class TrySort.h $(LIB)
```



### JNA

{{libheader|JNA}}

```java
import com.sun.jna.Library;
import com.sun.jna.Native;

public class LoadLibJNA{
   private interface YourSharedLibraryName extends Library{
      //put shared library functions here with no definition
      public void sharedLibraryfunction();
   }

   public static void main(String[] args){
      YourSharedLibraryName lib = (YourSharedLibraryName)Native.loadLibrary("sharedLibrary",//as in "sharedLibrary.dll"
                                                                          YourSharedLibraryName.class);
      lib.sharedLibraryFunction();
   }
}
```



## Jsish

Jsish includes a '''load('library.so');''' function, which calls a specially crafted management function in the library,
'''JsiInitmoduleName''', where the moduleName part of the exported symbol is the name of the library loaded.

Normally, this function would register commands to the shell, but this is just a DISPLAY statement on load, and then again on unload as jsish runs down.  Note the name used, "Jsi_Initbyjsi", from "byjsi.so".


```javascript
#!/usr/local/bin/jsish
load('byjsi.so');
```


For example, a COBOL library generated from


```COBOL
       identification division.
       program-id. sample as "Jsi_Initbyjsi".

       environment division.
       configuration section.
       special-names.
           call-convention 0 is extern.
       repository.
           function all intrinsic.

       data division.
       linkage section.
       01 jsi-interp usage pointer.
       01 rel usage binary-long.

       procedure division using by value jsi-interp rel.
       sample-main.
       if rel equal zero then
           display "GnuCOBOL from jsish load of " module-source()
                   " and cobc -m -fimplicit-init" upon syserr
           goback
       end-if

       display "Called again with: " jsi-interp ", " rel upon syserr
       goback.
       end program sample.
```


{{out}}

```txt
prompt$ cobc -m -debug  -fimplicit-init byjsi.cob
prompt$ ./callcob.jsi
GnuCOBOL from jsish load of byjsi.cob and cobc -m -fimplicit-init
Called again with: 0x00000000013a9260, +0000000002
prompt$
```



## Julia

Julia has the `ccall` function which follows the form: ccall((symbol, library), RetType, (ArgType1, ...), ArgVar1, ...)

```julia

#this example works on Windows
ccall( (:GetDoubleClickTime, "User32"), stdcall,
	Uint, (), )

ccall( (:clock, "libc"), Int32, ())
```

For more information, see here [http://docs.julialang.org/en/latest/manual/calling-c-and-fortran-code.html]


## Kotlin

{{trans|C}}
{{Works with|Ubuntu 14.04}}

This is the C code to produce fakeimglib.so:

```c
#include <stdio.h>
/* gcc -shared -fPIC -nostartfiles fakeimglib.c -o fakeimglib.so */
int openimage(const char *s)
{
    static int handle = 100;
    fprintf(stderr, "opening %s\n", s);
    return handle++;
}
```

And this is the Kotlin code to dynamically load the .so file and call the 'openimage' function - or if the .so file (or the function itself) is not available, to call the internal version of the function:

```scala
// Kotlin Native version 0.5

import kotlinx.cinterop.*
import platform.posix.*
import platform.linux.*

typealias Func = (String)-> Int

var handle = 0

fun myOpenImage(s: String): Int {
    fprintf(stderr, "internal openImage opens %s...\n", s)
    return handle++
}

fun main(args: Array<String>) {
    var imgHandle: Int
    val imglib = dlopen("./fakeimglib.so", RTLD_LAZY)
    if (imglib != null) {
        val fp = dlsym(imglib, "openimage")
        if (fp != null) {
            val extOpenImage: CPointer<CFunction<Func>> = fp.reinterpret()
            imgHandle = extOpenImage("fake.img")
        }
        else {
            imgHandle = myOpenImage("fake.img")
        }
        dlclose(imglib)
    }
    else {
        imgHandle = myOpenImage("fake.img")
    }
    println("opened with handle $imgHandle")
}
```


{{out}}

```txt

Same as C entry

```



## Lingo


```lingo
-- calculate CRC-32 checksum
str = "The quick brown fox jumps over the lazy dog"

-- is shared library (in Director called "Xtra", a DLL in windows, a sharedLib in
-- OS X) available?
if ilk(xtra("Crypto"))=#xtra then

  -- use shared library
  cx = xtra("Crypto").new()
  crc = cx.cx_crc32_string(str)

else

  -- otherwise use (slower) pure lingo solution
  crcObj = script("CRC").new()
  crc = crcObj.crc32(str)

end if
```



## Maple


```Maple>
 cfloor := define_external( floor, s::float[8], RETURN::float[8], LIB = "libm.so" ):
> cfloor( 2.3 );
                                   2.
```





## Mathematica

This works on windows and on linux/mac too (through Mono)

```Mathematica
Needs["NETLink`"];
externalFloor = DefineDLLFunction["floor", "msvcrt.dll", "double", { "double" }];
externalFloor[4.2]
-> 4.
```



## Nim


### Interacting with C code


```nim
proc openimage(s: cstring): cint {.importc, dynlib: "./fakeimglib.so".}

echo openimage("foo")
echo openimage("bar")
echo openimage("baz")
```

The fake <code>fakeimglib.so</code> code is

```c
#include <stdio.h>
/* gcc -shared -nostartfiles fakeimglib.c -o fakeimglib.so */
int openimage(const char *s)
{
  static int handle = 100;
  fprintf(stderr, "opening %s\n", s);
  return handle++;
}
```

Output:

```txt
opening foo
100
opening bar
101
opening baz
102
```



### Interacting with Nim code


```nim
proc openimage(s: string): int {.importc, dynlib: "./libfakeimg.so".}

echo openimage("foo")
echo openimage("bar")
echo openimage("baz")
```

The fake <code>libfakeimg.so</code> code is

```nim
# nim c --app:lib fakeimg.nim
var handle = 100

proc openimage*(s: string): int {.exportc, dynlib.} =
  stderr.writeln "opening ", s
  result = handle
  inc(handle)
```

Output:

```txt
opening foo
100
opening bar
101
opening baz
102
```



## OCaml

As far as I know there is no solution in OCaml standard library to load a function from a C library dynamically. So I have quickly implemented [[Call a function in a shared library/OCaml|a module named Dlffi that you can find in this sub-page]]. It is basically a wrapper around the GNU/Linux dl* functions and the libffi.

On Windows there is [http://alain.frisch.fr/flexdll.html FlexDLL].

Here is an example of use of this [[Call a function in a shared library/OCaml|Dlffi module]]:

```ocaml
open Dlffi

let get_int = function Int v -> v | _ -> failwith "get_int"
let get_ptr = function Ptr v -> v | _ -> failwith "get_ptr"
let get_float = function Float v -> v | _ -> failwith "get_float"
let get_double = function Double v -> v | _ -> failwith "get_double"
let get_string = function String v -> v | _ -> failwith "get_string"

let () =
  (* load the library *)
  let xlib = dlopen "/usr/lib/libX11.so" [RTLD_LAZY] in
  (* load the functions *)
  let _open_display = dlsym xlib "XOpenDisplay"
  and _default_screen = dlsym xlib "XDefaultScreen"
  and _display_width = dlsym xlib  "XDisplayWidth"
  and _display_height = dlsym xlib "XDisplayHeight"
  in
  (* wrap functions to provide a higher level interface *)
  let open_display ~name = get_ptr(fficall _open_display [| String name |] Return_ptr)
  and default_screen ~dpy = get_int(fficall _default_screen [| (Ptr dpy) |] Return_int)
  and display_width ~dpy ~scr = get_int(fficall _display_width [| (Ptr dpy); (Int scr) |] Return_int)
  and display_height ~dpy ~scr = get_int(fficall _display_height [| (Ptr dpy); (Int scr) |] Return_int)
  in
  (* use our functions *)
  let dpy = open_display ~name:":0" in
  let screen_number = default_screen ~dpy in
  let width = display_width ~dpy ~scr:screen_number
  and height = display_height ~dpy ~scr:screen_number in
  Printf.printf "# Screen dimensions are: %d x %d pixels\n" width height;
  dlclose xlib;
;;
```



## OxygenBasic


```oxygenbasic

'Loading a shared library at run time and calling a function.

declare MessageBox(sys hWnd, String text,caption, sys utype)

sys user32 = LoadLibrary "user32.dll"

if user32 then @Messagebox = getProcAddress user32,"MessageBoxA"

if @MessageBox then MessageBox 0,"Hello","OxygenBasic",0

'...

FreeLibrary user32

```



## PARI/GP


```parigp
install("function_name","G","gp_name","./test.gp.so");
```

where "G" is the parser code; see section 5.7.3 in the [http://pari.math.u-bordeaux.fr/pub/pari/manuals/2.4.4/libpari.pdf User's Guide to the PARI library] for more information.


## Pascal

See [[Call_a_function_in_a_shared_library#Delphi | Delphi]]


## Perl

Examples for simple <code>C</code> library calls, but each module is capable of much more (and can work with other languages). Refer to their documentation for details.

### Inline

This modules auto-builds a wrapper to the library on the first call, and subsequently uses that interface with no delay.

```perl
use Inline
    C => "DATA",
    ENABLE => "AUTOWRAP",
    LIBS => "-lm";

print 4*atan(1) . "\n";

__DATA__
__C__
double atan(double x);
```

{{out}}

```txt
3.14159265358979
```


### FFI

This module is smart about finding libraries, here getting <code>atan</code> (from 'lm') and <code>puts</code> (from 'libc').

```perl
use FFI::Platypus;
my $ffi = FFI::Platypus->new;
$ffi->lib(undef);
$ffi->attach(puts => ['string'] => 'int');
$ffi->attach(atan => ['double'] => 'double');

puts(4*atan(1));
```

{{out}}

```txt
3.14159265358979
```



## Perl 6

{{works with|Rakudo|2018.11}}

```perl6
use NativeCall;

sub XOpenDisplay(Str $s --> int64) is native('X11') {*}
sub XCloseDisplay(int64 $i --> int32) is native('X11') {*}

if try my $d = XOpenDisplay ":0.0" {
    say "ID = $d";
    XCloseDisplay($d);
}
else {
    say "No X11 library!";
    say "Use this window instead --> ⬜";
}
```

{{out}}

```txt
ID = 94722089782960
```



## Phix


```Phix
string {libname,funcname} = iff(platform()=WINDOWS?{"user32","CharLowerA"}:{"libc","tolower"})
atom lib = open_dll(libname)
integer func = define_c_func(lib,funcname,{C_INT},C_INT)
if func=-1 then
    ?{{lower('A')}}
else
    ?c_func(func,{'A'}) -- ('A'==65)
end if
```

{{out}}

```txt

97  -- (or {{97}} if func not found)

```



## PicoLisp

This differs between the 32-bit and 64-bit versions. While the 64-bit version
can interface directly to C functions (in external libraries or not), requires
the 32-bit function some glue code.
===32-bit version===
For the 32-bit version, we need some glue code:

```PicoLisp
(load "@lib/gcc.l")

(gcc "x11" '("-lX11") 'xOpenDisplay 'xCloseDisplay)

#include <X11/Xlib.h>

any xOpenDisplay(any ex) {
   any x = evSym(cdr(ex));    // Get display name
   char display[bufSize(x)];  // Create a buffer for the name

   bufString(x, display);     // Upack the name
   return boxCnt((long)XOpenDisplay(display));
}

any xCloseDisplay(any ex) {
   return boxCnt(XCloseDisplay((Display*)evCnt(ex, cdr(ex))));
}
/**/

# With that we can open and close the display:
: (setq Display (xOpenDisplay ":0.7"))   # Wrong
-> 0
: (setq Display (xOpenDisplay ":0.0"))   # Correct
-> 158094320
: (xCloseDisplay Display)
-> 0
```

===64-bit version===
In the 64-bit version, we can call the library directly:

```PicoLisp
: (setq Display (native "/usr/lib/libX11.so.6" "XOpenDisplay" 'N ":0.0"))
-> 6502688
: (native "/usr/lib/libX11.so.6" "XCloseDisplay" 'I Display)
-> 0
```



## PowerBASIC

{{Works with|PowerBASIC for Windows}}
In this example, if the library can't be found (user32), or the desired function in the library (MessageBoxA), the equivalent built-in function (MSGBOX) is at the "epicFail" label... but really, if you can't find user32.dll, you've got bigger things to worry about.

```powerbasic
#INCLUDE "Win32API.inc"

FUNCTION PBMAIN () AS LONG
    DIM hWnd AS LONG
    DIM msg AS ASCIIZ * 14, titl AS ASCIIZ * 8

    hWnd = LoadLibrary ("user32")
    msg = "Hello, world!"
    titl = "Example"
    IF ISTRUE (hWnd) THEN
        funcAddr& = GetProcAddress (hWnd, "MessageBoxA")
        IF ISTRUE (funcAddr&) THEN
            ASM push 0&
            tAdr& = VARPTR(titl)
            ASM push tAdr&
            mAdr& = VARPTR(msg)
            ASM push mAdr&
            ASM push 0&
            CALL DWORD funcAddr&
        ELSE
            GOTO epicFail
        END IF
    ELSE
        GOTO epicFail
    END IF

    GOTO getMeOuttaHere

epicFail:
    MSGBOX msg, , titl

getMeOuttaHere:
    IF ISTRUE(hWnd) THEN
        tmp& = FreeLibrary (hWnd)
        IF ISFALSE(tmp&) THEN MSGBOX "Error freeing library... [shrug]"
    END IF
END FUNCTION
```



## PureBasic

Older PureBasic versions normally relied on CallFunction() and CallFunctionFast()

```Purebasic
if OpenLibrary(0, "USER32.DLL")
  *MessageBox = GetFunction(0, "MessageBoxA")
  CallFunctionFast(*MessageBox, 0, "Body", "Title", 0)
  CloseLibrary(0)
endif
```

Since versions 4 the recommended way is via the usage of Prototypes even if the old system still is supported.

```PureBasic
Prototype.l ProtoMessageBoxW(Window.l, Body.p-unicode, Title.p-unicode, Flags.l = 0)

If OpenLibrary(0, "User32.dll")
  MsgBox.ProtoMessageBoxW = GetFunction(0, "MessageBoxW")
  MsgBox(0, "Hello", "World")
  CloseLibrary(0)
EndIf
```



## Python


###  ctypes

Example that call User32.dll::GetDoubleClickTime() in windows.

```python
import ctypes

user32_dll = ctypes.cdll.LoadLibrary('User32.dll')
print user32_dll.GetDoubleClickTime()
```

Or, to call printf out of the C standard library:

```python>>>
 import ctypes
>>> # libc = ctypes.cdll.msvcrt # Windows
>>> # libc = ctypes.CDLL('libc.dylib') # Mac
>>> libc = ctypes.CDLL('libc.so') # Linux and most other *nix
>>> libc.printf(b'hi there, %s\n', b'world')
hi there, world.
17
```



###  CFFI

[https://cffi.readthedocs.io/ CFFI] isn't built into the stdlib, but, on the other hand, it works with other Python implementations like PyPy. It also has a variety of advantages and disadvantages over ctypes, even for simple cases like this:

```python

>>> from cffi import FFI
>>> ffi = FFI()
>>> ffi.cdef("""
...     int printf(const char *format, ...);   // copy-pasted from the man page
... """)
>>> C = ffi.dlopen(None)                     # loads the entire C namespace
>>> arg = ffi.new("char[]", b"world")         # equivalent to C code: char arg[] = "world";
>>> C.printf(b"hi there, %s.\n", arg)         # call printf
hi there, world.
17
```



## R

This is possible in R in only a few limited ways.  If the library function one wishes to call is a (C-level) R function (of type SEXP), then one may call

```rsplus
dyn.load("my/special/R/lib.so")
.Call("my_lib_fun", arg1, arg2)
```

It is also possible to use <code>.C()</code> and <code>.Fortran()</code> to call voids and subroutines respectively; here the return value(s) should be in the argument list (rather than merely modifying state). An example of this might look like

```rsplus
.C("my_lib_fun", arg1, arg2, ret)
```

The return of the <code>.C()</code> function is an R list.



## Racket


```racket
#lang racket
(require ffi/unsafe)
(define libm (ffi-lib "libm")) ; get a handle for the C math library
; look up sqrt in the math library. if we can't find it, return the builtin sqrt
(define extern-sqrt (get-ffi-obj 'sqrt libm (_fun _double -> _double)
                                 (lambda () sqrt)))
```


Output:
```txt
> (extern-sqrt 42.0)
6.48074069840786
```



## REXX

{{works with|Regina REXX}}

The example is using the standard library that is supplied with Regina REXX that contains a wide range of functions.

```rexx
/*REXX pgm calls a function (systextscreensize) in a shared library (regutil).*/
z=rxfuncadd('sysloadfuncs', "regutil", 'sysloadfuncs')   /*add a function lib.*/
if z\==0  then do                                        /*test the return cod*/
               say 'return code'  z  "from rxfuncadd"    /*tell about bad RC. */
               exit z                                    /*exit this program. */
               end

call sysloadfuncs                                        /*load the functions.*/

                                       /* [↓]   call a particular function.   */
y=systextscreensize()                  /*Y now contains 2 numbers:  rows cols */
parse var y rows cols .                /*obtain the two numeric words in  Y.  */
say 'rows='  rows                      /*display the number of (terminal) rows*/
say 'cols='  cols                      /*   "     "     "    "     "      cols*/
call SysDropFuncs                      /*clean up: make functions inaccessible*/
                                       /*stick a fork in it,  we're all done. */
```

'''output'''   (which happens to reflect the program's author's particular screen size for the "DOS" window):

```txt

rows= 62
cols= 96

```



## Ruby

This script uses Fiddle from Ruby's standard library to open <code>fakeimglib.so</code> from the [[#C|C example]].

{{works with|Ruby|2.0+}}

```ruby
require 'fiddle/import'

module FakeImgLib
  extend Fiddle::Importer
  begin
    dlload './fakeimglib.so'
    extern 'int openimage(const char *)'
  rescue Fiddle::DLError
    # Either fakeimglib or openimage() is missing.
    @@handle = -1
    def openimage(path)
      $stderr.puts "internal openimage opens #{path}\n"
      @@handle += 1
    end
    module_function :openimage
  end
end

handle = FakeImgLib.openimage("path/to/image")
puts "opened with handle #{handle}"
```


The next script tries to use ImageMagick. First, it tries [https://rmagick.github.io/ rmagick] from RubyGems. If that library is missing, it tries to use [http://wiki.github.com/ffi/ffi ffi] from RubyGems to call C functions in ImageMagick. (FFI is an alternative to Fiddle). If that doesn't work, it falls back to code that only handles PNG images.

{{libheader|ImageMagick}}
{{libheader|RubyGems}}
{{works with|Ruby|1.9+}}

```ruby
# This script shows the width x height of some images.
# Example:
#   $ ruby imsize.rb dwarf-vs-elf.png swedish-chef.jpg
#   dwarf-vs-elf.png: 242x176
#   swedish-chef.jpg: 256x256

begin
  require 'rmagick'
  lib = :rmagick
rescue LoadError
  # Missing rmagick.  Try ffi.
  begin
    require 'ffi'
    module F
      extend FFI::Library
      ffi_lib 'MagickWand-6.Q16'
      attach_function :DestroyMagickWand, [:pointer], :pointer
      attach_function :MagickGetImageHeight, [:pointer], :size_t
      attach_function :MagickGetImageWidth, [:pointer], :size_t
      attach_function :MagickPingImage, [:pointer, :string], :bool
      attach_function :MagickWandGenesis, [], :void
      attach_function :NewMagickWand, [], :pointer
    end
    lib = :ffi
  rescue LoadError
    # Missing ffi, MagickWand lib, or function in lib.
  end
end

case lib
when :rmagick
  # Returns [width, height] of an image file.
  def size(path)
    img = Magick::Image.ping(path).first
    [img.columns, img.rows]
  end
when :ffi
  F.MagickWandGenesis()
  def size(path)
    wand = F.NewMagickWand()
    F.MagickPingImage(wand, path) or fail 'problem reading image'
    [F.MagickGetImageWidth(wand), F.MagickGetImageHeight(wand)]
  ensure
    F.DestroyMagickWand(wand) if wand
  end
else
  PngSignature = "\x89PNG\r\n\x1A\n".force_encoding('binary')
  def size(path)
    File.open(path, 'rb') do |file|
      # Only works with PNG: https://www.w3.org/TR/PNG/
      # Reads [width, height] from IDHR chunk.
      # Checks height != nil, but doesn't check CRC of chunk.
      sig, width, height = file.read(24).unpack('a8@16NN')
      sig == PngSignature and height or fail 'not a PNG image'
      [width, height]
    end
  end
end

# Show the size of each image in ARGV.
status = true
ARGV.empty? and (warn "usage: $0 file..."; exit false)
ARGV.each do |path|
  begin
    r, c = size(path)
    puts "#{path}: #{r}x#{c}"
  rescue
    status = false
    puts "#{path}: #$!"
  end
end
exit status
```



## Rust

The standard library does not provide a way to load dynamic libraries. Without using third-party libraries, we must use the FFI to call the relevant C functions directly.


### Unix


```rust
#![allow(unused_unsafe)]
extern crate libc;

use std::io::{self,Write};
use std::{mem,ffi,process};

use libc::{c_double, RTLD_NOW};

// Small macro which wraps turning a string-literal into a c-string.
// This is always safe to call, and the resulting pointer has 'static lifetime
macro_rules! to_cstr {
    ($s:expr) => {unsafe {ffi::CStr::from_bytes_with_nul_unchecked(concat!($s, "\0").as_bytes()).as_ptr()}}
}

macro_rules! from_cstr {
    ($p:expr) => {ffi::CStr::from_ptr($p).to_string_lossy().as_ref() }
}

fn main() {
    unsafe {
        let handle = libc::dlopen(to_cstr!("libm.so.6"), RTLD_NOW);

        if handle.is_null() {
            writeln!(&mut io::stderr(), "{}", from_cstr!(libc::dlerror())).unwrap();
            process::exit(1);
        }

        let extern_cos = libc::dlsym(handle, to_cstr!("cos"))
                .as_ref()
                .map(mem::transmute::<_,fn (c_double) -> c_double)
                .unwrap_or(builtin_cos);
        println!("{}", extern_cos(4.0));
    }
}

fn builtin_cos(x: c_double) -> c_double {
    x.cos()
}
```



## Scala


### Windows


### =Get free disk space=

{{libheader|net.java.dev.sna.SNA}}

```Scala
import net.java.dev.sna.SNA
import com.sun.jna.ptr.IntByReference

object GetDiskFreeSpace extends App with SNA {

  snaLibrary = "Kernel32" // Native library name
/*
 * Important Note!
 *
 * The val holding the SNA-returned function must have the same name as the native function itself
 * (see line following this comment). This is the only place you specify the native function name.
 */
  val GetDiskFreeSpaceA = SNA[String, IntByReference, IntByReference, IntByReference, IntByReference, Boolean]

  // This Windows function is described here:
  //     http://msdn.microsoft.com/en-us/library/aa364935(v=vs.85).aspx
  val (disk, spc, bps, fc, tc) = ("C:\\",
    new IntByReference, // Sectors per cluster
    new IntByReference, // Bytes per sector
    new IntByReference, // Free clusters
    new IntByReference) // Total clusters

  val ok = GetDiskFreeSpaceA(disk, spc, bps, fc, tc) // status
  println(f"'$disk%s' ($ok%s): sectors/cluster: ${spc.getValue}%d,  bytes/sector: ${bps.getValue}%d, " +
    f" free-clusters: ${fc.getValue}%d,  total/clusters: ${tc.getValue}%d%n")
}}
```



## Smalltalk

{{works with|GNU Smalltalk}}
The code tries to load the <tt>fakeimglib</tt> (cfr [[Call function in shared library#C|C example]]); if it succeed, the symbol <tt>openimage</tt> will exist, and will be called; otherwise, it is executed an "internal" code for <tt>openimage</tt>. In this example return code of the function of the library is ignored (<tt>ValueHolder null</tt>)

```smalltalk
DLD addLibrary: 'fakeimglib'.

Object subclass: ExtLib [
  ExtLib class >> openimage: aString [
    (CFunctionDescriptor isFunction: 'openimage')
    ifTrue: [
       (CFunctionDescriptor for: 'openimage'
                            returning: #int
                            withArgs: #( #string ) ) callInto: (ValueHolder null).
    ] ifFalse: [ ('internal open image %1' % { aString }) displayNl ]
  ]
].

ExtLib openimage: 'test.png'.
```



## Tcl

{{libheader|Ffidl}}

```Tcl
package require Ffidl

if {[catch {
    ffidl::callout OpenImage {pointer-utf8} int [ffidl::symbol fakeimglib.so openimage]
}]} then {
    # Create the OpenImage command by other means here...
}
set handle [OpenImage "/the/file/name"]
```

Note that if the library is appropriately set up with the correct entry function, it can be accessed directly with <code>load</code> which will cause it to register a Tcl command for the functionality it exports. [http://www.swig.org SWIG] can be used to automatically generate the interface code. Alternatively, [[:Category:Critcl|critcl]] can be used to allow writing glue [[C]] code directly embedded within a Tcl script.

With this many ways to perform the call, the best approach often depends on the size and complexity of the API being mapped. SWIG excels at large APIs, Ffidl is better when you just want to call a particular simple function, and critcl handles complex cases (callbacks, etc.) better than the other two.


## TXR



### = Call <code>uname</code> on Linux =



```txt
This is the TXR Lisp interactive listener of TXR 176.
Use the :quit command or type Ctrl-D on empty line to exit.
1> (typedef utsarray (zarray 65 char))
#<ffi-type (zarray 65 char)>
2> (typedef utsname (struct utsname (sysname utsarray)
                                    (nodename utsarray)
                                    (release utsarray)
                                    (version utsarray)
                                    (machine utsarray)
                                    (domainname utsarray)))
#<ffi-type (struct utsname (sysname utsarray) (nodename utsarray) (release utsarray)
            (version utsarray) (machine utsarray) (domainname utsarray))>
3> (with-dyn-lib nil (deffi uname "uname" int ((ptr-out utsname))))
** warning: (expr-3:1) defun: redefining uname, which is a built-in defun
#:lib-0176
4> (let ((u (new utsname))) (prinl (uname u)) u)
0
#S(utsname sysname "Linux" nodename "zelenka" release "3.2.0-40-generic"
           version "#64-Ubuntu SMP Mon Mar 25 21:22:26 UTC 2013" machine "i686"
           domainname "(none)")
```


We use <code>typedef</code> to condense the declarations, much like in C. The FFI handles nested types like arrays in structures.

The <code>zarray</code> type denotes null-terminated arrays. A <code>zarray</code> of <code>char</code> is specialized; it converts between Lisp strings (which use wide characters made of Unicode code points) and C <code>char</code> strings encoded in UTF-8.

The argument of <code>uname</code> is <code>(ptr-out utsname)</code>.  The semantics of <code>ptr-out</code> in this situation is that FFI prepares a C version of the Lisp structure, but doesn't perform any conversions from Lisp to initialize it. This not only saves CPU cycles, but allows us to use a blank structure produced by <code>(new utsname)</code> all of whose slots are <code>nil</code> and so wouldn't convert to C character arrays anyway! The function is called, and then conversions out of the structure to the Lisp structure take place, filling its slots with string values.

The <code>nil</code> argument in the <code>with-dyn-lib</code> macro causes the underlying implementation to call <code>dlopen(NULL)</code> to get access to the dynamic symbols available in the executable. We can use the name of a shared library instead, or a handle from TXR's <code>dlopen</code> library function.


## Ursala

When abs(x) is evaluated, a run time check is performed for the
availability of the system library's absolute value function (fabs),
and if found, it is used. If not, the user defined replacement
function is invoked.

```Ursala
#import std
#import flo

my_replacement = fleq/0.?/~& negative

abs = math.|fabs my_replacement
```



## VBA

Here is an example using a Fortran function compiled as a DLL, using Intel Fortran.

First the DLL. Compile with '''ifort /dll vbafun.f90'''. The DLL must be in a directory in the PATH environment variable. Notice that for 32 bits VBA, DLL functions must be STDCALL, and not CDECL (the default with Intel Fortran). In 64 bits, there is only one calling convention, so it's not a problem anymore.


```fortran
function ffun(x, y)
    implicit none
    !DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE :: ffun
    double precision :: x, y, ffun
    ffun = x + y * y
end function
```


Here is a VBA subroutine using the DLL


```vb
Option Explicit
Declare Function ffun Lib "vbafun" (ByRef x As Double, ByRef y As Double) As Double
Sub Test()
    Dim x As Double, y As Double
    x = 2#
    y = 10#
    Debug.Print ffun(x, y)
End Sub
```



## zkl

In zkl, extensions/new objects are written in C as shared libraries. For example, big nums are implemented as a small glue library in front of GMP:

```zkl
var BN=Import("zklBigNum");
BN(1)+2  //--> BN(3)
```

and it "just works" as all objects are "the same" whether statically or dynamically linked.


{{omit from|Batch File|Except for rundll32.exe (which is rather limited) there is no way of calling an external function}}
{{omit from|GUISS}}
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|TI-83 BASIC|Does not have a standard FFI.}}
{{omit from|TI-89 BASIC|Does not have a standard FFI.}}
{{omit from|Scheme|No standard FFI, due to no standard implementation.}}
{{omit from|Retro|No FFI}}

[[Category:Functions and subroutines]]

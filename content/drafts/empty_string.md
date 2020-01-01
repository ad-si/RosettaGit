+++
title = "Empty string"
description = ""
date = 2019-10-07T20:04:34Z
aliases = []
[extra]
id = 10022
[taxonomies]
categories = []
tags = []
+++

{{task}}

Languages may have features for dealing specifically with empty strings
(those containing no characters).


;Task:
::*   Demonstrate how to assign an empty string to a variable.
::*   Demonstrate how to check that a string is empty.
::*   Demonstrate how to check that a string is not empty.




[[Category:String manipulation]] [[Category:Simple]]


## 11l


```11l
V s = ‘’
I s.empty
   print(‘String s is empty.’)
I !s.empty
   print(‘String s is not empty.’)
```



## 8th

Assign an empty string to a variable:

```Forth
"" var, str
```


Check that the string is empty:

```Forth>str @ s:len 0 n:= if ... then</lang


The check for a non-empty string is the same, but with "not" after the n:=


## AArch64 Assembly


Declare an empty string at address <code>str</code>:

<lang ARM_Assembly>str: .asciz ""
```


Check if a string stored at <code>x0</code> is empty:

<lang ARM_Assembly>	mov x5, #0
	ldrb w5, [x0]
	cmp x5, #0
```


Full program demo:

{{works with|aarch64-linux-gnu-as/qemu-aarch64}}

<lang ARM_Assembly>.equ STDOUT, 1
.equ SVC_WRITE, 64
.equ SVC_EXIT, 93

.text
.global _start

_start:
	stp x29, x30, [sp, -16]!
	ldr x0, =str1
	mov x29, sp
	bl str_empty // str_empty("");
	ldr x0, =str2
	bl str_empty // str_empty("non-empty");
	ldp x29, x30, [sp], 16
	mov x0, #0
	b _exit

str1:	.asciz ""
str2:	.asciz "non-empty"
.align 4

// void str_empty(const char *s) - print "String is empty" if s is empty, "String is not empty" otherwise
str_empty:
	mov x5, #0
	ldrb w5, [x0]
	ldr x1, =msg_empty
	ldr x3, =msg_not_empty
	mov x2, #16
	mov x4, #20
	cmp x5, #0
	csel x1, x1, x3, eq // msg = s[0] == 0 ? msg_empty : msg_not_empty;
	csel x2, x2, x4, eq // len = s[0] == 0 ? 16 : 20;
	mov x0, #STDOUT
	b _write // write(stdout, msg, len);

msg_empty:
	.ascii "String is empty\n"
msg_not_empty:
	.ascii "String is not empty\n"
.align 4

//////////////// system call wrappers
// ssize_t _write(int fd, void *buf, size_t count)
_write:
	stp x29, x30, [sp, -16]!
	mov x8, #SVC_WRITE
	mov x29, sp
	svc #0
	ldp x29, x30, [sp], 16
	ret

// void _exit(int retval)
_exit:
	mov x8, #SVC_EXIT
	svc #0
```



## ACL2

To check if a string is empty:

```Lisp
(= (length str) 0)
```



## Ada



```Ada
procedure Empty_String is

   function Is_Empty(S: String) return Boolean is
   begin
      return S = ""; -- test that S is empty
   end Is_Empty;

   Empty: String := ""; -- Assign empty string
   XXXXX: String := "Not Empty";

begin
   if (not Is_Empty(Empty)) or Is_Empty(XXXXX) then
      raise Program_Error with "something went wrong very very badly!!!";
   end if;
end Empty_String;
```



## Aime


```aime
text s;
s = "";
if (length(s) == 0) {
    ...
}
if (length(s) != 0) {
    ....
}
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# declare a string variable and assign an empty string to it                  #
STRING s := "";

# test the string is empty                                                    #
IF s = "" THEN write( ( "s is empty", newline ) ) FI;

# test the string is not empty                                                #
IF s /= "" THEN write( ( "s is not empty", newline ) ) FI;

# as a string is an array of characters, we could also test for emptyness by  #
# checking for lower bound > upper bound                                      #
IF LWB s > UPB s THEN write( ( "s is still empty", newline ) ) FI
```



## Apex


```Apex

String.isBlank(record.txt_Field__c);
--Returns true if the specified String is white space, empty (''), or null; otherwise, returns false.

```



## AppleScript


```AppleScript

-- assign empty string to str
set str to ""


-- check if string is empty
if str is "" then
	-- str is empty
end if
-- or
if id of str is {} then
	-- str is empty
end if
-- or
if (count of str) is 0 then
	-- str is empty
end if


-- check if string is not empty
if str is not "" then
	-- string is not empty
end if
-- or
if id of str is not {} then
	-- str is not empty
end if
-- or
if (count of str) is not 0 then
	-- str is not empty
end if

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program strEmpty.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szNotEmptyString:   .asciz "String is not empty. \n"
szEmptyString:      .asciz "String is empty. \n"
@ empty string
szString:            .asciz ""   @ with zero final
szString1:           .asciz "A"  @ with zero final

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    @ load string
    ldr r1,iAdrszString
    ldrb r0,[r1]    @ load first byte of string
    cmp r0,#0        @ compar with zero ?
    bne 1f
    ldr r0,iAdrszEmptyString
    bl affichageMess
    b 2f
1:

    ldr r0,iAdrszNotEmptyString
    bl affichageMess
	/* second string */
2:
    @ load string 1
    ldr r1,iAdrszString1
    ldrb r0,[r1]        @ load first byte of string
    cmp r0,#0            @ compar with zero ?
    bne 3f
    ldr r0,iAdrszEmptyString
    bl affichageMess
    b 100f
3:
    ldr r0,iAdrszNotEmptyString
    bl affichageMess
    b 100f

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszString:             .int szString
iAdrszString1:            .int szString1
iAdrszNotEmptyString:   .int szNotEmptyString
iAdrszEmptyString:       .int szEmptyString

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {fp,lr}    			/* save  registres */
    push {r0,r1,r2,r7}    		/* save others registers */
    mov r2,#0   				/* counter length */
1:      	/* loop length calculation */
    ldrb r1,[r0,r2]  			/* read octet start position + index */
    cmp r1,#0       			/* if 0 its over */
    addne r2,r2,#1   			/* else add 1 in the length */
    bne 1b          			/* and loop */
                                /* so here r2 contains the length of the message */
    mov r1,r0        			/* address message in r1 */
    mov r0,#STDOUT      		/* code to write to the standard output Linux */
    mov r7, #WRITE             /* code call system "write" */
    swi #0                      /* call systeme */
    pop {r0,r1,r2,r7}     		/* restaur others registers */
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */



```



## AutoHotkey

AutoHotkey has both "Traditional" or literal text, and "Expression" mode.
This code demonstrates the task using both methods.

```AutoHotkey
;; Traditional
; Assign an empty string:
var =
; Check that a string is empty:
If var =
   MsgBox the var is empty
; Check that a string is not empty
If var !=
   Msgbox the var is not empty


;; Expression mode:
; Assign an empty string:
var := ""
; Check that a string is empty:
If (var = "")
   MsgBox the var is empty
; Check that a string is not empty
If (var != "")
   Msgbox the var is not empty
```



## AWK


```awk
#!/usr/bin/awk -f
BEGIN {
  # Demonstrate how to assign an empty string to a variable.
  a="";
  b="XYZ";
  print "a = ",a;
  print "b = ",b;
  print "length(a)=",length(a);
  print "length(b)=",length(b);
  # Demonstrate how to check that a string is empty.
  print "Is a empty ?",length(a)==0;
  print "Is a not empty ?",length(a)!=0;
  # Demonstrate how to check that a string is not empty.
  print "Is b empty ?",length(b)==0;
  print "Is b not empty ?",length(b)!=0;
}
```

{{out}}

```txt
$ awk -f R/tmp/string.awk
a =
b =  XYZ
length(a)= 0
length(b)= 3
Is a empty ? 1
Is a not empty ? 0
Is b empty ? 0
Is b not empty ? 1

```



## Axe


```axe
""→Str1
!If length(Str1)
 Disp "EMPTY",i
Else
 Disp "NOT EMPTY",i
End
```



## BASIC



```basic
10 LET A$=""
20 IF A$="" THEN PRINT "THE STRING IS EMPTY"
30 IF A$<>"" THEN PRINT "THE STRING IS NOT EMPTY"
40 END
```


=
## Applesoft BASIC
=
The terminating quote may be left off.  By default, strings are initially empty so the assignment is not necessary.  Another way to check for an empty string is to use the LEN function.

```basic
 10  LET A$ = "
 40  IF  LEN (A$) = 0 THEN  PRINT "THE STRING IS EMPTY"
 50  IF  LEN (A$) THEN  PRINT "THE STRING IS NOT EMPTY"
```


=
## BaCon
=
The literal empty string in BaCon is <tt>""</tt>.


```freebasic
' Empty string
a$ = ""
IF a$ = "" THEN PRINT "Empty string"
IF a$ != "" THEN PRINT "Non empty string"
```


There are other ways, such as a zero return from the <tt>LEN(s$)</tt> or <tt>ULEN(utf$)</tt> functions.  <tt>EQUAL(s$, "")</tt> would be another way.

==={{header|IS-BASIC}}===
<lang IS-BASIC>10 LET A$=""
20 IF A$="" THEN PRINT "The string is empty."
30 IF A$<>"" THEN PRINT "The string is not empty."
```



## Batch File



```dos

@echo off

::set "var" as a blank string.
set var=

::check if "var" is a blank string.
if not defined var echo Var is a blank string.
::Alternatively,
if %var%@ equ @ echo Var is a blank string.

::check if "var" is not a blank string.
if defined var echo Var is defined.
::Alternatively,
if %var%@ neq @ echo Var is not a blank string.

```



## BBC BASIC


```bbcbasic
      REM assign an empty string to a variable:
      var$ = ""

      REM Check that a string is empty:
      IF var$ = "" THEN PRINT "String is empty"

      REM Check that a string is not empty:
      IF var$ <> "" THEN PRINT "String is not empty"

```



## Bracmat

There are two ways to assign a string to a variable. The variant using the <code>=</code> operator does not evaluate the value before the assignment, the variant using the <code>:</code> (match) operator does. If the value is a string, there is no difference, as a string always evaluates to itself.

```bracmat
( :?a
& (b=)
& abra:?c
& (d=cadabra)
& !a:           { a is empty string }
& !b:           { b is also empty string }
& !c:~          { c is not an empty string }
& !d:~          { neither is d an empty string }
)

```



## Burlesque


Empty string is <tt>""</tt> and checking for empty strings (or empty lists) can be done with the <tt>nu</tt> command.


```blsq

blsq ) ""
""
blsq ) ""nu
1
blsq ) "a"nu
0

```



## C

In C the strings are <code>char</code> pointers.  A string terminates with the null char (U+0000, <code>'\0'</code>), which is not considered part of the string. Thus an empty string is <code>"\0"</code>, while a null string is a null pointer which points to nothing.

```C
/* assign an empty string */
const char *str = "";
/* to test a null string */
if (str) { ... }
/* to test if string is empty */
if (str[0] == '\0') { ... }
/* or equivalently use strlen function */
if (strlen(str) == 0) { ... }
/* or compare to a known empty string, same thing. "== 0" means strings are equal */
if (strcmp(str, "") == 0) { ... }

```



## C++


```cpp
#include <string>

// ...

std::string str; // a string object for an empty string

if (str.empty()) { ... } // to test if string is empty

// we could also use the following
if (str.length() == 0) { ... }
if (str == "") { ... }
```



## C sharp


```csharp
using System;

class Program {
    static void Main (string[] args) {
        string example = string.Empty;
        if (string.IsNullOrEmpty(example)) { }
        if (!string.IsNullOrEmpty(example)) { }
    }
}
```



### In depth

'''Compiler:''' Roslyn C# (language version >= 6)

Note: implementation information provided in comments was obtained reflecting .NET libraries and viewing the .NET Core reference source and may not be correct or remain relevant as time passes.


```csharp
using System;
using System.Collections.Generic;

static class Program
{
    // TL; DR:
    public static void Foo()
    {
        string s;

        // Assign empty string:
        s = "";
        // or
        s = string.Empty;

        // Check for empty string only (false if s is null):
        if (s != null && s.Length == 0) { }

        // Check for null or empty (most idiomatic .NET way):
        if (string.IsNullOrEmpty(s)) { }
    }

    public static void Main()
    {
        // Equality is somewhat convoluted in .NET.
        // The means above are the author's recommendation for each case.

        // s is initialized to null. It is a variable of the System.String type that is a null reference and is not
        // the empty string.
        string s = null;

        // Alias Console.WriteLine with a shorter name to make the demonstration code less verbose.
        void P(bool x) => Console.WriteLine(x);

        // Assign the empty string literal to s.
        s = "";

        // ' Assign String.Empty to s.
        s = string.Empty;

        // The empty string literal is the same object reference as String.Empty because of string interning, meaning the
        // behavior of the two is identical.
        // From this point on, "" will be used instead of String.Empty for brevity.

        //#== operator (object)
        // The == operator tests for reference equality when overload resolution fails to find an operator defined by
        // either operand type. However, which strings are interned is a CLR implementation detail and may be unreliable
        // when comparing non-empty strings. The equivalent in VB.NET would be s Is "".
        // Note that there is no such operator as Object.op_Equality(Object, Object): the use of the == operator for
        // types of type Object is a C# language feature.
        P((object)s == "");

        //#Object.ReferenceEquals(Object, Object)
        // The previous line is semantically to the following, though it does not involve a method call.
        P(object.ReferenceEquals(s, ""));

        //#String.op_Equality(String, String)
        // The equality operator of System.String is implemented as a call to String.Equals(String). Operators cannot be
        // called with method syntax in C#.
        P(s == "");

        //#String.Equals(String, String)
        // Call the static method defined on the String type, which first calls Object.ReferenceEquals and then, after
        // verifying that both are strings of the same length, compares the strings character-by-character.
        P(string.Equals(s, ""));

        //#Object.Equals(Object, Object)
        // First checks for reference equality and whether one or both of the arguments is null. It then invokes the
        // instance Equals method of the left parameter.
        P(object.Equals(s, ""));

        //#String.Equals(String)
        // The method is called with the string literal as the receiver because a NullReferenceException is thrown if s
        // is null.
        P("".Equals(s));

        //#String.Length
        // Check the Length property. The ?. (null-conditional) operator is used to avoid NullReferenceException. The Equals
        // call above can also be done this way. Null propagation makes the equality operator return false if one operand
        // is a Nullable<T> and does not have a value, making this result in false when s is null.
        P(s?.Length == 0);

        //#String.Length
        // A more traditional version of the null-conditional using a guard clause.
        // Both the null-conditional and this are noticeably (~4 times) faster than "".Equals(s). In general, it appears that
        // for empty strings, using the length is faster than using an equality comparison.
        P(s != null && s.Length == 0);

        //#String.IsNullOrEmpty(String)
        // Note that all of the other methods give false for null.
        // A static method of System.String that returns true if the string is null or its length is zero.
        P(string.IsNullOrEmpty(s));

        //#System.Collections.Generic.EqualityComparer(Of String).Default.Equals(String, String)
        // The EqualityComparer(Of T) class provides default implementations when an IEqualityComparer(Of T) is required.
        // The implementation for String calls String.Equals(String).
        P(EqualityComparer<string>.Default.Equals(s, ""));

        Console.WriteLine();

        // Each of the means described above, except testing for a non-empty string.
        P((object)s != "");
        P(!object.ReferenceEquals(s, ""));
        P(s != "");
        P(!string.Equals(s, ""));
        P(!object.Equals(s, ""));
        P(!"".Equals(s));
        P(s?.Length != 0); // Still false when s is null!
        P(s == null || s.Length != 0);
        P(!string.IsNullOrEmpty(s));
        P(!EqualityComparer<string>.Default.Equals(s, ""));
    }
}
```



## Dyalect


Demonstrate how to assign an empty string to a variable:


```dyalect
var str = ""
```


Demonstrate how to check that a string is empty:


```dyalect
if !str { }
//or
if str.isEmpty() { }
```


Demonstrate how to check that a string is not empty:


```dyalect
if str { }
//or
if !str.isEmpty() { }
```


=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>EMPTYSTR
    ; Demonstrate how to assign an empty string to a variable.
    set x = ""

      ; Demonstrate how to check that a string is empty.
      ; Length 0 is empty; equality/pattern check are 1=T, 0=F
      write !,"Assigned x to null value.  Tests: "
      write !,"String length: "_$length(x)_", Equals null: "_(x = "")_", Empty pattern: "_(x?."")    ; length 0 is empty

      ; Demonstrate how to check that a string is not empty.  Same as above.
      set x = " "    ;assign to a space - not null
      write !!,"Assigned x to a single blank space.  Tests: "
      write !,"String length: "_$length(x)_", Equals null: "_(x = "")_", Empty pattern: "_(x?."")

      quit
```


{{out}}SAMPLES>do EMPTYSTR^ROSETTA

Assigned x to null value.  Tests:
String length: 0, Equals null: 1, Empty pattern: 1

Assigned x to a single blank space.  Tests:
String length: 1, Equals null: 0, Empty pattern: 0


## Clojure


```clojure

(def x "") ;x is "globally" declared to be the empty string
(let [x ""]
  ;x is bound to the empty string within the let
  )
(= x "")    ;true if x is the empty string
(not= x "") ;true if x is not the empty string

```



## COBOL


```cobol

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EMPTYSTR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  str                     PIC X(10).

       PROCEDURE DIVISION.
       Begin.

*     *    Assign an empty string.
           INITIALIZE str.

*     *    Or
           MOVE " " TO str.

           IF (str = " ")
              DISPLAY "String is empty"
           ELSE
              DISPLAY "String is not empty"
           END-IF.

           STOP RUN.

```



## CoffeeScript

Empty strings are mostly straightforward in CoffeeScript, but there's one gotcha.


```coffeescript

isEmptyString = (s) ->
  # Returns true iff s is an empty string.
  # (This returns false for non-strings as well.)
  return true if s instanceof String and s.length == 0
  s == ''

empties = ["", '', new String()]
non_empties = [new String('yo'), 'foo', {}]
console.log (isEmptyString(v) for v in empties) # [true, true, true]
console.log (isEmptyString(v) for v in non_empties) # [false, false, false]
console.log (s = '') == "" # true
console.log new String() == '' # false, due to underlying JavaScript's distinction between objects and primitives

```



## Common Lisp

Common Lisp treats empty strings as true (T in Common Lisp), therefore one must check the length of the string to know if it is empty or not.

```lisp

(defparameter *s* "") ;; Binds dynamic variable *S* to the empty string ""
(let ((s "")) ;; Binds the lexical variable S to the empty string ""
  (= (length s) 0) ;; Check if the string is empty
  (> (length s) 0)) ;; Check if length of string is over 0 (that is: non-empty)

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE EmptyString;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	s: ARRAY 64 OF CHAR;
	(* s := "" <=> s[0] := 0X => s isEmpty*)
BEGIN
	s := "";
	StdLog.String("Is 's' empty?:>  ");StdLog.Bool(s = "");StdLog.Ln;
	StdLog.String("Is not 's' empty?:> ");StdLog.Bool(s # "");StdLog.Ln;
	StdLog.Ln;
	(* Or *)
	s := 0X;
	StdLog.String("Is 's' empty?:>  ");StdLog.Bool(s = 0X);StdLog.Ln;
	StdLog.String("Is not 's' empty?:> ");StdLog.Bool(s # 0X);StdLog.Ln;
	StdLog.Ln;
END Do;
END EmptyString.

```

Execute: ^Q EmptyString.Do<br/>
{{out}}

```txt

Is 's' empty?:>   $TRUE
Is not 's' empty?:>  $FALSE

Is 's' empty?:>   $TRUE
Is not 's' empty?:>  $FALSE

```



## D

D treats null strings and empty strings as equal on the value level, but different on object level. You need to take this into account when checking for emptiness.

```d
import std.array;

bool isEmptyNotNull(in string s) pure nothrow @safe {
    return s is "";
}

void main(){
    string s1 = null;
    string s2 = "";

    // the content is the same
    assert(!s1.length);
    assert(!s2.length);
    assert(s1 == "" && s1 == null);
    assert(s2 == "" && s2 == null);
    assert(s1 == s2);

    // but they don't point to the same memory region
    assert(s1 is null && s1 !is "");
    assert(s2 is "" && s2 !is null);
    assert(s1 !is s2);
    assert(s1.ptr == null);
    assert(*s2.ptr == '\0'); // D string literals are \0 terminated

    assert(s1.empty);
    assert(s2.isEmptyNotNull());
}
```



## Dart



```dart
main() {
  var empty = '';

  if (empty.isEmpty) {
    print('it is empty');
  }

  if (empty.isNotEmpty) {
    print('it is not empty');
  }
}
```


=={{header|Déjà Vu}}==
Like in Python, empty strings are falsy, non-empty strings are truthy.

```dejavu
local :e ""

if not e:
    !print "an empty string"

if e:
    !print "not an empty string"
```



## Delphi


```Delphi
program EmptyString;

{$APPTYPE CONSOLE}

uses SysUtils;

function StringIsEmpty(const aString: string): Boolean;
begin
  Result := aString = '';
end;

var
  s: string;
begin
  s := '';
  Writeln(StringIsEmpty(s)); // True

  s := 'abc';
  Writeln(StringIsEmpty(s)); // False
end.
```



## DWScript


```delphi
var s : String;

s := ''; // assign an empty string (can also use "")

if s = '' then
   PrintLn('empty');

s := 'hello';

if s <> '' then
   PrintLn('not empty');
```



## EasyLang


<lang>a$ = ""
if a$ = ""
  print "empty"
.
if a$ <> ""
  print "no empty"
.
```



## Elena

ELENA 4.x:

```elena
import extensions;

public program()
{
    auto s := emptyString;

    if (s.isEmpty())
        { console.printLine("'", s, "' is empty") };

    if (s.isNonempty())
        { console.printLine("'", s, "' is not empty") }
}
```

{{out}}

```txt

'' is empty

```



## Elixir

To check whether a given variable holds an empty string, either compare it to the empty string literal, check its length - O(M), or check it's byte size - O(1)).

```elixir

empty_string = ""
not_empty_string = "a"

empty_string == ""
# => true
String.length(empty_string) == 0
# => true
byte_size(empty_string) == 0
# => true

not_empty_string == ""
# => false
String.length(not_empty_string) == 0
# => false
byte_size(not_empty_string) == 0
# => false

```



## Emacs Lisp


```Lisp
(setq str "")   ;; empty string literal

(if (= 0 (length str))
    (message "string is empty"))
(if (/= 0 (length str))
    (message "string is not empty"))
```


Also possible is <code>(string= "" str)</code>.


## Erlang


```erlang

1> S = "". % erlang strings are actually lists, so the empty string is the same as the empty list [].
[]
2> length(S).
0
3> case S of [] -> empty; [H|T] -> not_empty end.
empty
4> case "aoeu" of [] -> empty; [H|T] -> not_empty end.
not_empty

```



## Euphoria


```euphoria
sequence s

-- assign an empty string
s = ""

-- another way to assign an empty string
s = {} -- "" and {} are equivalent

if not length(s) then
    -- string is empty
end if

if length(s) then
    -- string is not empty
end if
```



=={{header|F_Sharp|F#}}==

```fsharp
open System

[<EntryPoint>]
let main args =
    let emptyString = String.Empty  // or any of the literals "" @"" """"""
    printfn "Is empty %A: %A" emptyString (emptyString = String.Empty)
    printfn "Is not empty %A: %A" emptyString (emptyString <> String.Empty)
    0
```

{{out}}

```txt
Is empty "": true
Is not empty "": false
```



## Factor


It's idiomatic in Factor to prefer using the stack over variable bindings.


```factor
"" empty? .
```

{{out}}

```txt

t

```


However, Factor provides lexical variables:


```factor
USE: locals
[let
    "" :> empty-string
    empty-string empty? .
    empty-string empty? not .
]
```

{{out}}

```txt

t
f

```



## Fantom


Fantom uses "" to represent an empty string, and provides the isEmpty method to check if a string is empty.


```fantom

a := ""       // assign an empty string to 'a'
a.isEmpty     // method on sys::Str to check if string is empty
a.size == 0   // what isEmpty actually checks
a == ""       // alternate check for an empty string
!a.isEmpty    // check that a string is not empty

```



## Forth

Strings are represented as an addr-len pair on the stack. An empty string has len 0.

```forth

\ string words operate on the address and count left on the stack by a string
\ ? means the word returns a true/false flag on the stack

: empty? ( c-addr u -- ? ) nip 0= ;
: filled?  ( c-addr u -- ? ) empty? 0= ;
: =""      ( c-addr u -- ) drop 0 ;  \ It's OK to copy syntax from other languages

```


Forth Console Test (True= -1, False=0)

```txt
s" This is not empty" empty? . 0  ok

s" This is filled" filled? . -1  ok

s" " empty? . -1  ok

s" this is filled" =""  empty? . -1

```



## Fortran

Early Fortran offered only rather strange methods of manipulating text, involving overwriting text literals within a FORMAT statement via a READ statement that used that format statement. Such text could not be inspected, whether to see if it was blank or anything else. Fortran 4 introduced the A format whereby text could be stored in integer or floating-point variables or arrays, and then those variables could be manipulated and inspected - though their numerical values would be unusual, especially if in floating-point variables. Fortran 77 introduced a CHARACTER definition which greatly eased matters but it was not a "string" type, which is to say, a variable storing some sequence of characters (or, in principle, integers, or other data) and ''also'' having a length. A variable may be declared as having a fixed size, as in CHARACTER*24 TEXT, and there is a library function LEN which for that variable would return 24, no matter what the variable contained. That is to say, it reports the size of the variable, not the length in current use of a string of up to 24 characters as would be the case for a similar declaration in for example, Pascal.

Such variables, or text literals, may be passed as a parameter to a subprogram, and it may use the LEN function to ascertain the size of the parameter, which in that sense could be considered a string because CHARACTER parameters are passed with a secret additional parameter, their size, which is available to the LEN function within the subprogram.

```fortran
      SUBROUTINE TASTE(T)
       CHARACTER*(*) T       !This form allows for any size.
        IF (LEN(T).LE.0) WRITE(6,*) "Empty!"
        IF (LEN(T).GT.0) WRITE(6,*) "Not empty!"
      END
      CHARACTER*24 TEXT
      CALL TASTE("")
      CALL TASTE("This")
      TEXT = ""              !Fills the entire variable with space characters.
      CALL TASTE(TEXT)       !Passes all 24 of them. Result is Not empty!
      END
```

Otherwise, you could employ the Fortran protocol that trailing spaces are irrelevant in text comparisons. Thus <code>TEXT .EQ. ""</code> would give ''true'' even though TEXT might contain thousands of space characters, and so would <code>TEXT .EQ. "  "</code> - thus an empty string is one containing nothing other than spaces.

Alternatively, the programmer can be diligent, and associate an integer with every such CHARACTER variable, such as LTEXT for TEXT, to hold the current length of characters in use. Tests for empty strings and the like would thus be made by inspecting the value of LTEXT, which hopefully, would always contain correct values.

With F90, compound data aggregates can be defined and as well procedures for operating on them, so that, after a great deal of syntactic struggle, a string data type will be available. F2000 standardised one such scheme whereby character variables are de-allocated and re-allocated with usage so that a statement such as <code>TEXT = "This" // "That"</code> would cause a de-allocation of whatever storage had been associated with TEXT followed by a re-allocation of storage for eight characters, the required size, and LEN(TEXT) would give 8.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub IsEmpty(s As String)
   If Len(s) = 0 Then
     Print "String is empty"
   Else
     Print "String is not empty"
   End If
End Sub

Dim s As String  ' implicitly assigned an empty string
IsEmpty(s)
Dim t As String = "" ' explicitly assigned an empty string
IsEmpty(t)
Dim u As String = "not empty"
IsEmpty(u)
Sleep
```


{{out}}

```txt

String is empty
String is empty
String is not empty

```



## Free Pascal

Assigning an empty string:

```pascal
s := '';
```

Checking for an empty string:

```pascal
s = ''
length(s) = 0
```

Checking for a non-empty string:

```pascal>s <
 ''
length(s) > 0
longBool(length(s))
```

The <code>sysUtils</code> unit defines the constants <code>emptyStr</code> and <code>emptyWideStr</code>, which can be used in place of <code>&#39;&#39;</code>.


## FutureBasic

FB has several ways to determine string length as demonstrated below.
Note: The length -- or number of characters -- of a Pascal string in FB is stored in the first element of the string array. Here the string is dimensioned a s, hence s[0] contains the length of the string, and any individual character in the string can be found using s[i], where i is the position of the character. This makes iterating over the individual characters in a string easier than using functions such as instr or mid$.

```futurebasic

include "ConsoleWindow"

dim as Str255 s

s = ""
if s == "" then print "String is empty"
if s[0] == 0 then print "String is empty"
if len(s) == 0 then print "String is empty"

s = "Hello"
if s <> "" then print "String not empty."
if s[0] then print "String not empty."
if len(s) > 0 then print "String not empty."

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=4d8a208f49364dc25361bf2042af1005 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String[] = ["", "Hello", "world", "", "Today", "Tomorrow", "", "", "End!"]
Dim sTemp As String
Dim siCount As Short

For Each sTemp In sString
  If sString[siCount] Then
    Print "String " & siCount & " = " & sString[siCount]
  Else
    Print "String " & siCount & " is empty"
  End If
  Inc siCount
Next

End
```

Output:

```txt

String 0 is empty
String 1 = Hello
String 2 = world
String 3 is empty
String 4 = Today
String 5 = Tomorrow
String 6 is empty
String 7 is empty
String 8 = End!

```



## Go

Go has no special syntax for empty strings.
In Go variables are always initialized to a provided value or to the "zero value" of the type.
The zero value of a string is the empty string.

```go
// define and initialize an empty string
var s string
s2 := ""

// assign an empty string to a variable
s = ""

// check that a string is empty, any of:
s == ""
len(s) == 0

// check that a string is not empty, any of:
s != ""
len(s) != 0 // or > 0
```



## Groovy


```groovy
def s = ''  // or "" if you wish
assert s.empty

s = '1 is the loneliest number'
assert !s.empty
```



## Haskell


```haskell
import Control.Monad

-- In Haskell strings are just lists (of characters), so we can use the function
-- 'null', which applies to all lists.  We don't want to use the length, since
-- Haskell allows infinite lists.

main = do
  let s = ""
  when (null s) (putStrLn "Empty.")
  when (not $ null s) (putStrLn "Not empty.")
```



## HolyC


```holyc
/* assign an empty string */
U8 *str = StrNew("");
/* or */
U8 *str = "";

/* to test if string is empty */
if (StrLen(str) == 0) { ... }
/* or compare to a known empty string. "== 0" means strings are equal */
if (StrCmp(str, "") == 0) { ... }

/* to test if string is not empty */
if (StrLen(str)) { ... }
```



## i


```i
software {
	s = ""

	// Can either compare the string to an empty string or
	// test if the length is zero.
	if s = "" or #s = 0
		print("Empty string!")
	end

	if s - "" or #s - 0
		print("Not an empty string!")
	end
}

```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon can produce empty strings in several ways:

```Icon
s := ""                 # null string
s := string('A'--'A')   # ... converted from cset difference
s := char(0)[0:0]       # ... by slicing

s1 == ""                # lexical comparison, could convert s1 to string
s1 === ""               # comparison won't force conversion
*s1 = 0                 # zero length, however, *x is polymorphic
*string(s1) = 0         # zero length string

s1 ~== ""               # non null strings comparisons
s1 ~=== ""
*string(s1) ~= 0

s := &null              # NOT a null string, null type
/s                      # test for null type
\s                      # test for non-null type
```



## J



```j
   variable=: ''
   0=#variable
1
   0<#variable
0
```


Note that J attempts to make no distinction between empty lists, regardless of their type.  In other words, while some operations can reveal the type of an empty list (for example, anything that can introduce padding based on the type of the list itself) this distinction is ignored whenever possible.  You can perform arithmetic on an empty string, and you can append text to an empty list of numbers even though these operations would not succeed on non-empty lists of the same type.

Thus it's not appropriate, in general case J code, to check that an empty string is of type string.

Note also that in an <code>if.</code> or <code>while.</code> statement, J treats an empty string (or the absence of any argument) as true.


## Java

<code>String.isEmpty()</code> is part of Java 1.6. Other options for previous versions are noted.

```java5
String s = "";
if(s != null && s.isEmpty()){//optionally, instead of "s.isEmpty()": "s.length() == 0" or "s.equals("")"
   System.out.println("s is empty");
}else{
   System.out.println("s is not empty");
}
```




## JavaScript

Create an empty String

```javascript
var s = "";
var s = new String();
```


Boolean expressions representing emptiness

```javascript
s == ""
s.length == 0
!s
!Boolean(s)
```


Non-emptiness

```javascript
!!s
s != ""
s.length != 0
s.length > 0
Boolean(s)
```



## jq

jq strings are JSON strings.  The empty string literal is simply <tt>""</tt>.  It can be assigned to a variable as illustrated by this example:
```jq
"" as $x
```
If s is a string or an array, then the additive "zero" for s can be created by writing s[0:0].  That is, if s is a string, then s[0:0] will yield the empty string.  This is useful when writing polymorphic functions.

To determine whether a string, s, is empty:
```jq
s == ""
# or:
s|length == 0
```
To determine whether a string, s, is non-empty:
```jq
s != ""
# or:
s.length != 0 # etc.
```



## Jsish


```javascript
/* Empty string, in Jsish */
var em1 = '';
var em2 = new String();

var str = 'non-empty';

;'Empty string tests';
;em1 == '';
;em1 === '';
;em1.length == 0;
;!em1;
;(em1) ? false : true;
;Object.is(em1, '');
;Object.is(em1, new String());

;'Non empty string tests';
;str != '';
;str !== '';
;str.length != 0;
;str.length > 0;
;!!str;
;(str) ? true : false;

;'Compare two empty strings';
;(em1 == em2);
;(em1 === em2);

/*
=!EXPECTSTART!=
'Empty string tests'
em1 == '' ==> true
em1 === '' ==> true
em1.length == 0 ==> true
!em1 ==> true
(em1) ? false : true ==> true
Object.is(em1, '') ==> true
Object.is(em1, new String()) ==> true
'Non empty string tests'
str != '' ==> true
str !== '' ==> true
str.length != 0 ==> true
str.length > 0 ==> true
!!str ==> true
(str) ? true : false ==> true
'Compare two empty strings'
(em1 == em2) ==> true
(em1 === em2) ==> true
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U emptyString.jsi
'Empty string tests'
em1 == '' ==> true
em1 === '' ==> true
em1.length == 0 ==> true
!em1 ==> true
(em1) ? false : true ==> true
Object.is(em1, '') ==> true
Object.is(em1, new String()) ==> true
'Non empty string tests'
str != '' ==> true
str !== '' ==> true
str.length != 0 ==> true
str.length > 0 ==> true
!!str ==> true
(str) ? true : false ==> true
'Compare two empty strings'
(em1 == em2) ==> true
(em1 === em2) ==> true

prompt$ jsish -u emptyString.jsi
[PASS] emptyString.jsi
```



## Julia


```Julia

blank = ""
nonblank = "!"

println("The length of blank is ", length(blank))
println("That blank is empty is ", isempty(blank))
println("That blank is not empty is ", !isempty(blank))

println()
println("The length of nonblank is ", length(nonblank))
println("That nonblank is empty is ", isempty(nonblank))
println("That nonblank is not empty is ", !isempty(nonblank))

```


{{out}}

```txt

The length of blank is 0
That blank is empty is true
That blank is not empty is false

The length of nonblank is 1
That nonblank is empty is false
That nonblank is not empty is true

```



## K

{{trans|J}}


```K
   variable: ""
   0=#variable
1
   0<#variable
0
```



## Kotlin


```scala
fun main(args: Array<String>) {
    val s = ""
    println(s.isEmpty())    // true
    println(s.isNotEmpty()) // false
    println(s.length)       // 0
    println(s.none())       // true
    println(s.any())        // false
}
```



## LabVIEW

{{VI solution|LabVIEW_Empty_string.png}}


## Lasso


```Lasso
//Demonstrate how to assign an empty string to a variable.
local(str = string)
local(str = '')

//Demonstrate how to check that a string is empty.
#str->size == 0 	// true
not #str->size		// true

//Demonstrate how to check that a string is not empty.
local(str = 'Hello, World!')
#str->size > 0 		// true
#str->size		// true
```



## Latitude


Assigning the empty string.

```latitude
s := "".
s := String clone.
```


Checking whether a string is empty.

```latitude
s == "".
s empty?.
s length == 0.
```


Note that all strings are truthy in Latitude, so simply checking the truthiness of a string is an insufficient means to check whether it is empty.


## LFE

{{trans|Clojure}}
{{trans|Common Lisp}}
{{trans|Erlang}}


```lisp

> (set str "")
()
> (length str)
0
> (=:= 0 (length str))
true
> (=:= 0 (length "apple"))
false
> (=:= "apple" "")
false
> (=/= "apple" "")
true
> (=:= str "")
true
> (=:= "apple" '())
false
> (=/= "apple" '())
true
> (=:= str '())
true
> (case str  ('() 'empty) ((cons head tail) 'not-empty))
empty
> (case "apple"  ('() 'empty) ((cons head tail) 'not-empty))
not-empty

```



## Lhogho

Lhogho is a Logo compiler for Windows and Linux

```logo
make "str " ;make null-string word
print empty? :str ;prints 'true'
print not empty? :str ;prints 'false'

```



## Liberty BASIC


```lb

'assign empty string to variable
a$ = ""
'check for empty string
if a$="" then print "Empty string."
if len(a$)=0 then print "Empty string."
'check for non-empty string
if a$<>"" then print "Not empty."
if len(a$)>0 then print "Not empty."

```



## Lingo


```lingo
str = EMPTY -- same as: str = ""
put str=EMPTY
-- 1
put str<>EMPTY
-- 0
```



## LOLCODE

The empty string is a false value in LOLCODE, and is thus amenable to use as the condition of an <tt>O RLY?</tt>

```LOLCODE
HAI 1.3

I HAS A string ITZ ""
string, O RLY?
    YA RLY, VISIBLE "STRING HAZ CONTENZ"
    NO WAI, VISIBLE "Y U NO HAS CHARZ?!"
OIC

KTHXBYE
```



## Lua


```Lua

-- create an empty string 3 different ways
str = ""
str = ''
str = [[]]

-- test for empty string
if str == "" then
  print "The string is empty"
end

-- test for nonempty string
if str ~= "" then
  print "The string is not empty"
end

-- several different ways to check the string's length
if string.len(str) == 0 then
  print "The library function says the string is empty."
end
if str:len() == 0 then
  print "The method call says the string is empty."
end
if #str == 0 then
  print "The unary operator says the string is empty."
end


```



## M2000 Interpreter

Easy reply for this task

```M2000 Interpreter

A$=""
Print A$<>"", A$="", Len(A$)=0

```


Depends of variable visibility, and what we want to do: To make a new local, to shadow a local or a global on.


```M2000 Interpreter

Module Checkit {
      \\
      \\ Part 1: Make global variable, alter it, make a shadow local or global one, use temporary variable
      \\
      Global a$="ok"
      Module Global What {
            Print a$
      }
      Module Checkit {
            Print a$="ok"
            a$<=""
            Print a$=""
            a$<="ok2"
            a$=""
            Print a$="", a$<>""
            Global  a$="ok again"
            Module Inner {
                  Print a$="ok again"
            }
            Inner
            What   \\ now What use new global a$
            \\ display list of public variables
            List
            \\ we can define locals using Def, but raise error if local exist
            Try {
                  Def a$="error"
            }
            Def b$
            Print b$=""
            For This {
                  \\ block for temporary definitions
                  For i=1 to 10 {
                        Local a$=str$(i)
                  }
                  \\ we get 10 more a$
                  List
                  Print a$=" 10"
            }
            Print a$=""
            List
            \\ using current stack
      }
      \\ we call always a local module, or a global, but not this module,
      \\ no recursion for standard call for modules.
      \\ we have to use Call Checkit to call this module recursive
      Checkit
      What  \\ now what use old global a$
      Print a$<>""  ' true
      List

      \\
      \\ Part 2:  Pass an empty string to a variable through stack of values
      \\
      Module Checkit2 {
             \\ read make a local by default
             Read a$
             Print a$=""  ' true
             For This {
                   Push "Hello"
                   Read New a$
                   Print a$="Hello"
                   List
            }
            Print a$=""
      }
      Checkit2 ""
      Print a$<>""  ' true
      Module Checkit3 {
            \\ using Set we change to global  space, for the end of line
             Set Read a$
             Print a$=""  ' true
             list
      }
      Checkit3 ""
      Print a$<>"" ' true
      Module Checkit4 {
            \\ this make a local if no global exist
            \\ so if global exist, alter the global one
             Let a$=Letter$
             Print a$=""  ' true
             list
      }
      Checkit4 ""
      Print a$="" ' true
}
Checkit

```



## Maple


```Maple

s := ""; # Create an empty string
evalb(s = ""); # test if the string is empty
evalb(s <> ""); # test if the string is not empty

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

str=""; (*Create*)
str==="" (*test empty*)
str=!="" (*test not empty*)

```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
   % Demonstrate how to assign an empty string to a variable.
    str = '';
    % Demonstrate how to check that a string is empty.
    isempty(str)
    (length(str)==0)
    % Demonstrate how to check that a string is not empty.
    ~isempty(str)
    (length(str)>0)
```



## Maxima


```maxima
s: ""$

/* check using string contents */
sequal(s, "");
not sequal(s, "");

/* check using string length */
slength(s) = "";
slength(s) # "";
```



## Mercury


There's nothing special about empty strings in Mercury.


```Mercury
S = "",  % assignment

( if S = "" then ... else ... ),  % checking if a string is empty

( if not S = "" then ... else ... ),  % checking if a string is not empty
```



## min

{{works with|min|0.19.3}}
The <code>bool</code> operator returns <code>false</code> on an empty string and <code>true</code> on a non-empty string. We can define <code>empty?</code> as the negation of <code>bool</code>.

```min
(bool not) :empty?
"" empty? puts!
"Rosetta Code" empty? puts!
```

{{out}}

```txt

true
false

```



## Mirah


```mirah
empty_string1 = ""
empty_string2 = String.new

puts "empty string is empty" if empty_string1.isEmpty()
puts "empty string has no length" if empty_string2.length() == 0
puts "empty string is not nil" unless empty_string1 == nil
```



## Nanoquery


```nanoquery
$s = ""

if len($s)=0
        println "$s is empty"
end

if len($s)>0
        println "$s is not empty"
end
```



## Nemerle

Assign an empty string:

```Nemerle
def empty = "";
mutable fill_later = "";
```

Check if a string is empty/not empty:

```Nemerle
a_string == ""; a_string != 0;
a_string.Length == 0; a_string.Length > 0;
```



## NESL


```nesl
my_empty_string = "";

% To make sure it is empty, we can ask whether its length is equal to zero. %

#my_empty_string == 0;
```

{{out}}

```txt
my_empty_string = "" : [char]

it = T : bool
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

s1 = '' -- assignment
s2 = "" -- equivalent to s1
parse '.' . s3 . -- parsing a token that doesn't exist results in an empty string

strings = [s1, s2, s3, ' ']

loop s_ = 0 to strings.length - 1
  say (Rexx s_).right(3)':\-'
  select
    when strings[s_] == ''      then say ' "'strings[s_]'" is really empty'
    when strings[s_].length = 0 then say ' "'strings[s_]'" is empty'
    when strings[s_] = ''       then say ' "'strings[s_]'" looks empty but may not be'
    when strings[s_].length > 0 then say ' "'strings[s_]'" is not empty'
    otherwise nop
    end
  end s_

return

```

{{out}}

```txt

  0: "" is really empty
  1: "" is really empty
  2: "" is really empty
  3: " " looks empty but may not be

```



## Nim


```nim
var x = ""

if x == "":
  echo "empty"
if x != "":
  echo "not empty"

# Alternatively:
if x.len == 0:
  echo "empty"
if x.len > 0:
  echo "not empty"
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 STRING$=""
20 IF STRING$="" THEN PRINT "THE STRING IS EMPTY."
30 IF STRING$<>"" THEN PRINT "THE STRING IS NOT EMPTY."
```



## Nyquist


### Lisp Syntax


```lisp

(setf emptystring "") ;binds variable'emptystring' to the empty string ""

(let ((emptystring "")) ;; Binds local variable 'emptystring' to the empty string ""
  (when (string-equal emptystring "")  ;;case insensitive string comparison
    (print "Is an empty string"))  ;;bad argument error if not a string
  (when (stringp emptystring)
    (print "Is a string"))
  (when (not (stringp emptystring))
    (print "Is not a string"))
  (when (and (stringp emptystring)(= (length emptystring) 0))
    (print "Is an empty string"))
  (when (and (stringp emptystring)(> (length emptystring) 0))
    (print "Is a non-empty string")))

```



### SAL Syntax


```sal

define variable emptystring = ""  ;binds variable'emptystring' to the empty string ""

if emptystring = "" then
  print "is empty string"
else
  print "is not empty string"

```



===Audacity plug-in (LISP syntax)===

```lisp

;nyquist plug-in
;version 4
;type tool
;name "Empty string"

(setq emptystring "") ;; Define global variable

(if (string= emptystring "")  ;;case sensitive string comparison
    "The string is empty."
    "The string is not empty.")

```



===Audacity plug-in (SAL syntax)===

```Nyquist

;nyquist plug-in
;version 4
;codetype sal
;type tool
;name "Empty string"

define variable emptystring = "a" ;; Define global variable

;; The ternary operator is #?
return #?(emptystring = "",
                        "The string is empty.",
                        "The string is not empty.")

```


=={{header|oberon-2}}==
{{works with|oo2c version 2}}

```oberon2

MODULE EmptyString;
IMPORT Out;
VAR
  str: ARRAY 64 OF CHAR;
BEGIN
  str := "";
  Out.String("for str := ");Out.Char('"');Out.Char('"');Out.Char(';');Out.Ln;
  Out.String("checking str = ");Out.Char('"');Out.Char('"');Out.String(" Is Empty? ");Out.Bool(str = "");Out.Ln;
  Out.String("checking str[0] = 0X. Is Empty? ");Out.Bool(str[0] = 0X);Out.Ln;
  str := "Hello Rossetta";
  Out.String("for str :=");Out.Char('"');Out.String(str);Out.Char('"');Out.Char(";");Out.Ln;
  Out.String("checking str = ");Out.Char('"');Out.String(str);Out.Char('"');Out.String(" Is Empty? ");Out.Bool(str = "");Out.Ln;
  Out.String("checking str[0] = 0X. Is Empty? ");Out.Bool(str[0] = 0X);Out.Ln;
END EmptyString.

```

{{out}}

```txt

for str := "";
checking str = "" Is Empty? TRUE
checking str[0] = 0X. Is Empty? TRUE
for str :="Hello Rossetta";
checking str = "Hello Rossetta" Is Empty? FALSE
checking str[0] = 0X. Is Empty? FALSE

```



## Objeck


```objeck

s := "";
if(s->IsEmpty()) {
   "s is empty"->PrintLine();
} else{
   "s is not empty"->PrintLine();
};

```



## OCaml



```ocaml
let is_string_empty s =
  (s = "")

let () =
  let s1 = ""
  and s2 = "not empty" in
  Printf.printf "s1 empty? %B\n" (is_string_empty s1);
  Printf.printf "s2 empty? %B\n" (is_string_empty s2);
;;
```


{{out}}

```txt

s1 empty? true
s2 empty? false

```



## Oforth


isEmpty can be used on all collections, not only strings.


```Oforth
"" isEmpty
"" isEmpty not
```



## OpenEdge/Progress

Strings can be stored in CHARACTER and LONGCHAR variables. Both are initially empty. Both can also be unknown. A CHARACTER has a maximum length of approx 32000 bytes.


```progress
DEFINE VARIABLE cc AS CHARACTER.

IF cc > '' THEN
   MESSAGE 'not empty' VIEW-AS ALERT-BOX.
ELSE IF cc = ? THEN
   MESSAGE 'unknown' VIEW-AS ALERT-BOX.
ELSE /* IF cc = '' */
   MESSAGE 'empty' VIEW-AS ALERT-BOX.

```



## Ol



```ol

; define the empty string
(define empty-string "")

; three simplest tests for 'the-string emptiness
(if (or
      (string-eq? the-string "")
      (string=?   the-string "")
      (eq? (string-length the-string) 0))
   (print "the-string is empty")

; four simplest tests for 'the-string not emptiness
(if (or
      (not (string-eq? the-string ""))
      (not (string=?   the-string ""))
      (not (eq? (string-length the-string) 0))
      (less? 0 (string-length the-string)))
   (print "the-string is NOT empty))

```



## ooRexx

should work with each and every REXX interpreter/compiler.

```oorexx
v=''
w=' '
if v=='' Then Say 'v contains the empty string'<
If length(w)>0 Then Say 'Variable w does not contain the empty string'
If w='' Then Say 'this is not a good test'
```

{{out}}

```txt
v contains the empty string
Variable w does not contain the empty string
this is not a good test
```



## PARI/GP


```parigp
a="";
isEmpty(s)=s==""    \\ Alternately:
isEmpty(s)=#s==0
isNonempty(s)=s!=""    \\ Alternatively:
isNonempty(s)=#s
```



## Pascal

''See [[#Delphi|Delphi]] or [[#Free Pascal|Free Pascal]]''


## Perl


```perl
if ($s eq "") {    # Test for empty string
  print "The string is empty";
}
if ($s ne "") {    # Test for non empty string
  print "The string is not empty";
}
```


In Perl, an empty string is often used to represent a false value.

```Perl
$s = "";
if ($s) { ... }  # false

# to tell if a string is false because it's empty, or it's plain not there (undefined)
$s = undef;
if (defined $s) { ... } # false; would be true on ""

# though, perl implicitly converts between strings and numbers, so this is also false
$s = "0";
if ($s) { ... } # false; also false on "000", "0.0", "\x0", "0 with text", etc

# but a string that converts to number 0 is not always false, though:
$s = "0 but true";
if ($s) { ... }  # it's true! black magic!
```



## Perl 6



```perl6
my $s = '';
say 'String is empty' unless $s;
say 'String is not empty' if $s;
```


Unlike in Perl 5, only empty strings test as false - the string "0" tests as true now.


## Phix


```euphoria
string s

s = ""                  -- assign an empty string

if length(s)=0 then     -- string is empty
if s="" then            -- string is empty

if length(s)!=0 then    -- string is not empty
if s!="" then           -- string is not empty
```



## PHP


```Php
<?php

$str = ''; // assign an empty string to a variable

// check that a string is empty
if (empty($str)) { ... }

// check that a string is not empty
if (! empty($str)) { ... }

// we could also use the following
if ($str == '') { ... }
if ($str != '') { ... }

if (strlen($str) == 0) { ... }
if (strlen($str) != 0) { ... }
```



## PicoLisp

The empty string is represented by '[http://software-lab.de/doc/ref.html#nilSym NIL]' in PicoLisp. During input, two subsequent double quotes '""' return the symbol NIL.

```PicoLisp
# To assign a variable an empty string:
(off String)
(setq String "")
(setq String NIL)

# To check for an empty string:
(or String ..)
(ifn String ..)
(unless String ..)

# or a non-empty string:
(and String ..)
(if String ..)
(when String ..)
```



## PL/I


```PL/I
Dcl s Char(10) Varying;
s = '';                   /* assign an empty string to a variable. */
if length(s) = 0 then ... /* To test whether a string is empty */
if length(s) > 0 then ... /* to test for a non-empty string */

```




## PowerShell

Assignment (the type identifier is optional):

```PowerShell

[string]$alpha = "abcdefghijklmnopqrstuvwxyz"
[string]$empty = ""
# or...
[string]$empty = [String]::Empty

```

Tests:

```PowerShell

[String]::IsNullOrEmpty($alpha)
[String]::IsNullOrEmpty($empty)

```

{{Out}}

```txt

False
True

```



## Prolog

{{works with|SWI-Prolog|7}}

```prolog
assign_empty_string(Variable) :- Variable = "".

is_empty_string(String)  :- String ==  "".
not_empty_string(String) :- String \== "".

```



## PureBasic

In PureBasic we can just test a string for truth to determine if it has a value.

```PureBasic
Procedure.s isStringEmpty(a.s)
  If a
    ProcedureReturn "String is not empty, it contains '" + a + "'."
  Else
    ProcedureReturn "String is empty, or null."
  EndIf
EndProcedure

If OpenConsole()
  Define a.s = ""
  Define b.s = "stuff"
  PrintN(isStringEmpty(a))
  PrintN(isStringEmpty(b))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
String is empty, or null.
String is not empty, it contains 'stuff'.
```



## Python

The empty string is printed by Python REPL as <nowiki>''</nowiki>, and is treated as boolean false (as are most empty container types, by convention). Any non-empty ''string'', including '0', is always treated as True in a boolean context.

```python
s = ''
if not s:
    print('String s is empty.')
if s:
    print('String s is not empty.')
```



## R



```R
s <- ''

if (s == '') cat('Empty\n')
#or
if (nchar(s) == 0) cat('Empty\n')

if (s != '') cat('Not empty\n')
#or
if (nchar(s) > 0) cat('Not empty\n')
```



## Racket



```Racket
#lang racket

(define empty-string "")
(define (string-null? s) (string=? "" s))
(define (string-not-null? s) (string<? "" s))
```



## Rascal


```rascal
str s = "";
if (s=="") print("string s is empty");
if (s!="") print("string s is not empty");

```

Or the build-in operator:

```rascal
import String;
if (isEmpty(s)) print("string s is empty");
if (isEmpty(s)) print("string s is not empty");
```



## Red


```Red
Red []
s: copy ""   ;; assign empty string
?? s
if empty? s [print "string is empty "]          ;; check if string is empty
s: "abc"
prin s unless empty? s  [print " not empty"]

```

{{out}}

```txt
s: ""
string is empty
abc not empty
>>

```



## Retro

Create an empty string and assign it to a variable. In these '''keepString''' is used to ensure that the string is permanent.

```Retro

( by creating a variable )
"" keepString variable: foo

( by setting an existing variable 'foo' )
"" keepString !foo

```


Checking that a string is empty. A string with a length of zero is assumed to be empty.


```Retro

: emtpy? ( $-f )  getLength 0 = ;

"" empty? putn
"hello" empty? putn

```


Check that a string is not empty.


```Retro

: notEmpty? ( $-f ) getLength 0 > ;

"" notEmpty? putn
"hello" notEmpty? putn

```



## REXX


```rexx
/*REXX program shows how to assign an empty string, & then check for empty/not-empty str*/

                /*─────────────── 3 simple ways to assign an empty string to a variable.*/
auk=''          /*uses two single quotes (also called  apostrophes);  easier to peruse. */
ide=""          /*uses two quotes,  sometimes called a  double quote.                   */
doe=            /*··· nothing at all   (which in this case, a null value is assigned.   */

                /*─────────────── assigning multiple null values to vars, 2 methods are:*/
parse var doe emu pug yak nit moa owl pas jay koi ern ewe fae gar hob

                /*where  emu, pug, yak, ···  (and the rest) are all set to a null value.*/

                /*───or─── (with less clutter ─── or more, depending on your perception)*/
parse value 0 with . ant ape ant imp fly tui paa elo dab cub bat ayu
                /*where the value of  zero  is skipped,  and  the rest are set to  null,*/
                /*which is the next value  AFTER  the  0  (zero):   nothing (or a null).*/

                /*─────────────── how to check that a string is empty, several methods: */
if cat==''         then say "the feline is not here."
if pig==""         then say 'no ham today.'
if length(gnu)==0  then say "the wildebeest's stomach is empty and hungry."
if length(ips)=0   then say "checking with   ==   instead of  =  is faster"
if length(hub)<1   then method = "this is rather obtuse,  don't do as I do ···"

nit=''                                           /*assign an empty string to a lice egg.*/
if cow==nit        then say 'the cow has no milk today.'

                /*─────────────── how to check that a string isn't empty, several ways: */
if dog\==''        then say "the dogs are out!"
                                                 /*most REXXes support the ¬ character. */
if fox¬==''        then say "and the fox is in the henhouse."
if length(rat)>0   then say "the rat is singing" /*an obscure-ish (or ugly) way to test.*/

if elk==''         then nop; else say "long way obtuse for an elk to be tested."

if length(eel)\==0 then fish=eel                 /*a fast compare (than below), & quick.*/
if length(cod)\=0  then fish=cod                 /*a not-as-fast compare.               */

                /*────────────────────────── anyway, as they say: "choose your poison." */
```



## Ring


```ring

cStr = NULL # empty string
if cStr = NULL
   see "cstr is an empty string!" + nl
else
   see "cstr is not empty string!" + nl
ok

```



## Robotic


```robotic

set "$string" to ""
if "$string.length" = 0 then "empty"
* "Not an empty string."
end

: "empty"
* "Empty string"
end

```



## Ruby


Create an empty string

```ruby
s = ""
s = String.new
s = "any string"; s.clear
```


These expressions all evaluate to true to determine emptiness:

```ruby
s == ""
s.eql?("")
s.empty?
s.length == 0
s[/\A\z/]

# also silly things like
s.each_char.to_a.empty?
```


Non-empty expressions, in addition to simply negating the above expressions:

```ruby
s != ""
s.length > 0
s[/./m]
```


Note that we can '''not''' do the following, because the empty string is equivalent to true in Ruby ([[Boolean values#Ruby]]):

```ruby
if s then puts "not empty" end  # This code is wrong!
```



## Run BASIC


```runbasic
var$ = ""
' --------------
'empty string
' -------------
if var$=""	then print "String is Empty"
if len(var$)=0	then print "String is Empty"
' -------------
'not empty string
' -------------
if var$<>"" 	then print "String Not empty."
if len(var$)>0	then print "String Not empty."
```



## Rust


```rust
let s = "";
println!("is empty: {}", s.is_empty());
let t = "x";
println!("is empty: {}", t.is_empty());
let a = String::new();
println!("is empty: {}", a.is_empty());
let b = "x".to_string();
println!("is empty: {}", b.is_empty());
println!("is not empty: {}", !b.is_empty());
```



## Scala


```scala
// assign empty string to a variable
val s=""
// check that string is empty
s.isEmpty    // true
s==""        // true
s.size==0    // true
// check that string is not empty
s.nonEmpty   // false
s!=""        // false
s.size>0     // false
```



## Scheme


```scheme
(define empty-string "")
(define (string-null? s) (string=? "" s))
(define (string-not-null? s) (string<? "" s))
```



## Seed7


```seed7
# assign empty string to a variable
s := ""

# check that string is empty
s = ""

# check that string is not empty
s <> ""
```



## Self


```self

"Put an empty string in a slot called 'str'"
str: ''.

"Check that string is empty"
str isEmpty.

"Check that string is not empty"
str isEmpty not.
```



## Sidef

Create an empty string:

```ruby
var s = "";
var s = String.new;
```


These expressions all evaluate to true to determine emptiness:

```ruby
s == "";
s.length == 0;
s.is_empty;
s ~~ /^\z/;
s ~~ /\A\z/;
```


Non-empty expressions, in addition to simply negating the above expressions:

```ruby
s != "";
s.length > 0;
s ~~ /./s;
s !~ /^\z/;
```



## Smalltalk


```smalltalk

"Assign empty string to a variable"
str := ''.

"Check that string is empty"
str isEmpty.

"Check that string is not empty"
str isEmpty not.

```



## SNOBOL4

An assignment statement with nothing to the right of the <tt>=</tt> operator assigns the empty string (or, as it is more commonly called in SNOBOL, the null string).

```snobol4
* ASSIGN THE NULL STRING TO X
        X =
* CHECK THAT X IS INDEED NULL
        EQ(X, NULL)         :S(YES)
        OUTPUT = 'NOT NULL' :(END)
YES     OUTPUT = 'NULL'
END

```

{{out}}

```txt
NULL
```



## Standard ML


```sml
(* Assign empty string to a variable *)
val s = ""
(* Check that a string is empty*)
s = ""
(* Check that a string is nonempty *)
s <> ""
```



## Stata


```stata
scalar s=""

display s==""

* Alternatively, check the length
display length(s)==0
```



## Swift


```swift
var s = ""
if s.isEmpty { // alternately, s == ""
  println("s is empty")
} else {
  println("s is not empty")
}
```



## Tcl

The only special position that the empty string has in Tcl is that a great many commands return it, and the REPL of [[tclsh]] and [[wish]] doesn't print it. Otherwise, it is just like any other value.

```tcl
set s ""
if {$s eq ""} {puts "s contains an empty string"}
if {$s ne ""} {puts "s contains a non-empty string"}
```

There are other ways to check for emptiness and non-emptiness too (though the above are favored for reasons of simplicity, clarity and speed):

```tcl
if {[string equal $s ""]} {puts "is empty"}
if {[string length $s] == 0} {puts "is empty"}
if {[string compare $s ""] != 0} {puts "is non-empty"}
```



## TorqueScript

Assign an empty string to $empty
  $empty = "";
Check if $empty is an empty string
  if($empty $= "") { echo("$empty is an empty string."); }
Check if $empty is not an empty string
  if($empty !$= "") { echo("$empty is not an empty string."); }


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
s=""
IF (s=="") PRINT "s is an empty string"
IF (s!="") PRINT "s is a non-empty string"

```

{{out}}

```txt

s is an empty string

```



## TXR



### =Pattern Matching=



```txr
@(bind a "")
```


If <code>a</code> is unbound, a binding is created, containing the empty string.
If <code>a</code> is already bound, <code>bind</code> succeeds if <code>a</code> contains the empty string, and the pattern matching continues at the next directive.
Or else a failure occurs, triggering backtracking behavior.


### =TXR Lisp=



```txrlisp
(defvarl a "")

(if (equal a "")
  (format t "empty string\n"))

(set a "nonempty")

(if (zerop (length a))
  (format t "guess what?\n"))
```



## UNIX Shell



```bash
# assign an empty string to a variable
s=""

# the "test" command can determine truth by examining the string itself
if [ "$s" ]; then echo "not empty"; else echo "empty"; fi

# compare the string to the empty string
if [ "$s" = "" ]; then echo "s is the empty string"; fi
if [ "$s" != "" ]; then echo "s is not empty"; fi

# examine the length of the string
if [ -z "$s" ]; then echo "the string has length zero: it is empty"; fi
if [ -n "$s" ]; then echo "the string has length non-zero: it is not empty"; fi
```


When using comparison operators, it is crucial to double-quote the variable within the conditional expression. This will ensure the shell sees the correct number of arguments. For example, if one were to write <tt>[ $s = "" ]</tt>, then after variable substitition, the shell will try to evaluate <tt>[ = "" ]</tt> which is a syntax error.


## Ursa


```ursa
decl string s
set s ""

if (= s "")
	out "empty" endl console
else
	out "not empty" endl console
end if
```



## VAX Assembly



```VAX Assembly
desc:  .ascid "not empty"         ;descriptor (len+addr) and text
.entry empty, ^m<>
       tstw desc                  ;check length field
       beql is_empty
       ;... not empty
       clrw desc                  ;set length to zero -> empty
is_empty:
       ;... empty
       ret
.end empty
```



## VBA


```vb

dim s as string

' assign an empty string to a variable:
s = ""

' test if a string is empty:
if s = "" then Debug.Print "empty!"
' or:
if Len(s) = 0 then Debug.Print "still empty!"

'test if a string is not empty:
if s <> "" then Debug.Print "not an empty string"
'or:
if Len(s) > 0 then Debug.Print "not empty."

```



## VBScript

See [[Empty_string#VBA|VBA]]


## Visual Basic

See [[Empty_string#VBA|VBA]]


## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >= 14, e.g. with Visual Studio 2015)


### In brief


```vbnet
Dim s As String

' Assign empty string:
s = ""
' or
s = String.Empty

' Check for empty string only (false if s is null):
If s IsNot Nothing AndAlso s.Length = 0 Then
End If

' Check for null or empty (most idiomatic .NET way):
If String.IsNullOrEmpty(s) Then
End If
```



### In depth

Note: implementation information provided in comments was obtained reflecting .NET libraries and viewing the .NET Core reference source and may not be correct or remain relevant as time passes.

In .NET Core, functions defined in <code>Microsoft.VisualBasic</code> are only available for .NET Core 3.0 and above.


```vbnet
Option Strict On

Module Program

    Sub Main()
        ' Equality is somewhat convoluted in .NET, and VB doesn't help by adding legacy means of comparison.
        ' The means above are the author's recommendation for each case.
        ' Some methods also return true if the string is Nothing/null; this is noted in the description for those that
        ' do.

        ' s is initialized to Nothing. It is a variable of the System.String type that is a null reference and is not
        ' the empty string.
        Dim s As String = Nothing

        ' Alias Console.WriteLine with a shorter name to make the demonstration code less verbose.
        Dim P = Sub(x As Boolean) Console.WriteLine(x)

        ' Assign the empty string literal to s.
        s = ""

        '' Assign String.Empty to s.
        s = String.Empty

        ' The empty string literal is the same object reference as String.Empty because of string interning, meaning the
        ' behavior of the two is identical.
        ' From this point on, "" will be used instead of String.Empty for brevity.

        '#Is operator
        ' The Is operator tests for reference equality. However, which strings are interned is a CLR implementation
        ' detail and may be unreliable when comparing non-empty strings. The equivalent in C# would be (object)s == "".
        ' Note that there is no such operator as Object.op_Equality(Object, Object): the use of the == operator for
        ' types of type Object is a C# language feature.
        P(s Is "")

        '#Object.ReferenceEquals(Object, Object)
        ' The previous line is semantically to the following, though it does not involve a method call.
        P(Object.ReferenceEquals(s, ""))

        '#= Operator
        'True for Nothing.
        ' The VB.NET compiler does not use the System.String implementation of the equality operator. Instead, it emits
        ' a call to a method in the Visual Basic runtime, Operators.CompareString, which checks for reference equality
        ' before calling System.String.CompareOrdinal(String, String), which checks again for reference equality before
        ' comparing character-by-character.
        P(s = "")

        '#Microsoft.VisualBasic.CompilerServices.Operators.CompareString(String, String, Boolean)
        'True for Nothing.
        ' Equivalent to the above line, though methods in the CompilerServices namespace are not meant for use by
        ' regular code.
        ' The third argument indicates whether to use a textual comparison (e.g. ignore case and diacritics).
        P(0 = CompilerServices.Operators.CompareString(s, "", False))

        '#Microsoft.VisualBasic.Strings.StrComp(String, String, [CompareMethod])
        'True for Nothing.
        ' A wrapper around CompareString that is intended for use.
        P(0 = StrComp(s, ""))

        '#String.op_Equality(String, String)
        ' It is possible to directly call the equality operator of System.String, which is implemented as a call to
        ' String.Equals(String).
        P(String.op_Equality(s, ""))

        '#String.Equals(String, String)
        ' Call the static method defined on the String type.
        '  first calls Object.ReferenceEquals and then, after verifying that both are strings of the same length,
        ' compares the strings character-by-character.
        P(String.Equals(s, ""))

        '#Object.Equals(Object, Object)
        ' First checks for reference equality and whether one or both of the arguments is Nothing. It then invokes the
        ' instance Equals method of the left parameter.
        P(Object.Equals(s, ""))

        '#String.Equals(String)
        ' The method is called with the string literal as the receiver because a NullReferenceException is thrown if s
        ' is Nothing.
        P("".Equals(s))

        '#Microsoft.VisualBasic.Strings.Len(String)
        'True for Nothing.
        ' Check the length using Microsoft.VisualBasic.Strings.Len(String). This method returns s?.Length (see below).
        P(0 = Len(s))

        '#String.Length
        ' Check the Length property. The ?. (null-conditional) operator is used to avoid NullReferenceException. The Equals
        ' call above can also be done this way.
        ' A method call must be added because the equality operator propagates Nothing/null (that is, the result of the
        ' expression is Nullable(Of Boolean)). This has the side effect of making it behave "correctly" for null.
        P((s?.Length = 0).GetValueOrDefault())

        ' The If statement automatically unwraps nullable Booleans, however.
        If s?.Length = 0 Then
        End If

        '#String.Length
        ' A more traditional version of the null-conditional using a guard clause.
        ' Both the null-conditional and this are noticeably (~4 times) faster than "".Equals(s). In general, it appears that
        ' for empty strings, using the length is faster than using an equality comparison.
        P(s IsNot Nothing AndAlso s.Length = 0)

        '#String.IsNullOrEmpty(String)
        'True for Nothing
        ' A static method of System.String that returns true if the string is Nothing or its length is zero.
        P(String.IsNullOrEmpty(s))

        '#System.Collections.Generic.EqualityComparer(Of String).Default.Equals(String, String)
        ' The EqualityComparer(Of T) class provides default implementations when an IEqualityComparer(Of T) is required.
        ' The implementation for String calls String.Equals(String).
        P(EqualityComparer(Of String).Default.Equals(s, ""))

        Console.WriteLine()

        ' Each of the means described above, except testing for a non-empty string.
        P(s IsNot "")
        P(Not Object.ReferenceEquals(s, ""))
        P(s <> "")
        P(0 <> CompilerServices.Operators.CompareString(s, "", False))
        P(0 <> StrComp(s, ""))
        P(String.op_Inequality(s, ""))
        P(Not String.Equals(s, ""))
        P(Not Object.Equals(s, ""))
        P(Not "".Equals(s))
        P(Len(s) <> 0)
        P((s?.Length <> 0).GetValueOrDefault())
        P(s Is Nothing OrElse s.Length <> 0)
        P(Not String.IsNullOrEmpty(s))
        P(Not EqualityComparer(Of String).Default.Equals(s, ""))
    End Sub
End Module
```


{{out}}

```txt
True
True
True
True
True
True
True
True
True
True
True
True
True
True

False
False
False
False
False
False
False
False
False
False
False
False
False
False
```



## Wee Basic


```Wee Basic
let string$=""
if string$=""
print 1 "The string is empty."
elseif string$<>""
print 1 "The string is not empty."
endif
end
```



## XLISP

<code>STRING-NULL?</code> returns true if its argument is a string of length zero. In a REPL:

```scheme
[1] (define my-empty-string "") ;; assign an empty string to a variable

MY-EMPTY-STRING
[2] (string-null? my-empty-string)

#T
[3] (string-null? "A non-empty string")

()
```



## XPL0


```XPL0
code Text=12;
string 0;       \use zero-terminated convention, instead of MSb set
char S;
[S:= "";        \assign an empty string
if S(0) = 0 then Text(0, "empty
");
S:= "Hello";
if S(0) # 0 then Text(0, "not empty
");
]
```



## zkl


```zkl
s:="";     // or s:=String, String is the object ""
s.toBool() //-->False
if (s) println("not empty")
```



## ZX Spectrum Basic

:''See [[#BASIC|BASIC]]''

{{omit from|GUISS}}

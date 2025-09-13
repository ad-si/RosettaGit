+++
title = "Null object"
description = ""
date = 2019-05-30T08:08:11Z
aliases = []
[extra]
id = 2958
[taxonomies]
categories = ["Basic language learning", "Simple", "task"]
tags = []
+++

## Task

'''Null''' (or '''nil''') is the computer science concept of an undefined or unbound object.
Some languages have an explicit way to access the null object, and some don't.
Some languages distinguish the null object from [[undefined values]], and some don't.


;Task:
Show how to access null in your language by checking to see if an object is equivalent to the null object.


''This task is not about whether a variable is defined. The task is about "null"-like values in various languages, which may or may not be related to the defined-ness of variables in your language.''





## 8th


```forth

null? if "item was null" . then

```


## ActionScript


```actionscript
if (object == null)
    trace("object is null");
```


ActionScript also has an '''undefined''' value: see [[Undefined values#ActionScript]].


## Ada


```ada
with Ada.Text_Io;

if Object = null then
   Ada.Text_Io.Put_line("object is null");
end if;
```



## ALGOL 68

In ALGOL 68 the NIL yields a name that does not refer to any value. NIL can never be
naturally coerced and can only appear where the context is [[ALGOL 68#strong|strong]].

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
REF STRING no result = NIL;
STRING result := "";

IF no result :=: NIL THEN print(("no result :=: NIL", new line)) FI;
IF result :/=: NIL THEN print(("result :/=: NIL", new line)) FI;

IF no result IS NIL THEN print(("no result IS NIL", new line)) FI;
IF result ISNT NIL THEN print(("result ISNT NIL", new line)) FI;

COMMENT using the UNESCO/IFIP/WG2.1 ALGOL 68 character set
  result := °;
  IF REF STRING(result) :≠: ° THEN print(("result ≠ °", new line)) FI;
END COMMENT

# Note the following gotcha: #

REF STRING var := NIL;
IF var ISNT NIL THEN print(("The address of var ISNT NIL",new line)) FI;
IF var IS REF STRING(NIL) THEN print(("The address of var IS REF STRING(NIL)",new line)) FI
```

Output:

```txt

no result :=: NIL
result :/=: NIL
no result IS NIL
result ISNT NIL
The address of var ISNT NIL
The address of var IS REF STRING(NIL)

```

NIL basically is an untyped '''ref''' (pointer) that does not refer anywhere.

ALGOL 68 also has '''empty'''. This is a "constant" of size 0 and type '''void'''.
c.f. [[Roots_of_a_function#ALGOL_68|Roots of a function]] for two different
examples of usage.
* '''empty''' as an undefined argument to a routine.
* '''empty''' as a routine return if no result is found.
'''empty''' is typically used to refer to am empty leaf in a tree structure.

Basically:
* ALGOL 68's '''empty''' is python's <code>None</code>,
* ALGOL 68's '''void''' is python's <code>NoneType</code>, and
* ALGOL 68's '''nil''' is python's <code>hash(None)</code>


## ALGOL W


```algolw
begin
    % declare a record type - will be accessed via references                %
    record R( integer f1, f2, f3 );
    % declare a reference to a R instance                                    %
    reference(R) refR;
    % assign null to the reference                                           %
    refR := null;
    % test for a null reference - will write "refR is null"                  %
    if refR = null then write( "refR is null" ) else write( "not null" );
end.
```



## AmigaE


```amigae
DEF x : PTR TO object
-> ...
IF object <> NIL
  -> ...
ENDIF
```



## AppleScript

Many applications will return <code>missing value</code>, but <code>null</code> is also available.

```AppleScript
if x is missing value then
  display dialog "x is missing value"
end if

if x is null then
  display dialog "x is null"
end if
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program nullobj.s   */

/* Constantes    */
.equ STDIN,  0                                        @ Linux input console
.equ STDOUT, 1                                        @ Linux output console
.equ EXIT,   1                                        @ Linux syscall
.equ READ,   3                                        @ Linux syscall
.equ WRITE,  4                                        @ Linux syscall

/* Initialized data */
.data
szCarriageReturn:       .asciz "\n"
szMessResult:           .asciz "Value is null.\n"     @ message result

iPtrObjet:		.int 0                        @ objet pointer

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                                                @ entry of program

    ldr r0,iAdriPtrObjet                             @ load pointer address
    ldr r0,[r0]                                      @ load pointer value
    cmp r0,#0                                        @ is null ?
    ldreq r0,iAdrszMessResult                        @ yes -> display message
    bleq affichageMess


100:                                                 @ standard end of the program
    mov r0, #0                                       @ return code
    pop {fp,lr}                                      @ restaur 2 registers
    mov r7, #EXIT                                    @ request to exit program
    svc 0                                            @ perform the system call

iAdrszMessResult:        .int szMessResult
iAdrszCarriageReturn:    .int szCarriageReturn
iAdriPtrObjet:           .int iPtrObjet
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                           @ save  registres
    mov r2,#0                                       @ counter length
1:                                                  @ loop length calculation
    ldrb r1,[r0,r2]                                 @ read octet start position + index
    cmp r1,#0                                       @ if 0 its over
    addne r2,r2,#1                                  @ else add 1 in the length
    bne 1b                                          @ and loop
                                                    @ so here r2 contains the length of the message
    mov r1,r0                                       @ address message in r1
    mov r0,#STDOUT                                  @ code to write to the standard output Linux
    mov r7, #WRITE                                  @ code call system "write"
    svc #0                                          @ call systeme
    pop {r0,r1,r2,r7,lr}                            @ restaur des  2 registres */
    bx lr                                           @ return


```



## AutoHotkey


```AutoHotkey
If (object == null)
  MsgBox, object is null
```



## AutoIt


```AutoIt
Local $object = Null
If $object = Null Then MsgBox(0, "NULL", "Object is null")
```



## AWK

Undefined elements correspond to an empty string; when converted to a numerical value, it evaluates to 0. In order to distinguish a undefined value from a value of 0, length(var) need to be used.

```AWK
#!/usr/bin/awk -f
BEGIN {
  b=0;
  print "<"b,length(b)">"
  print "<"u,length(u)">"
  print "<"u+0,length(u+0)">";
}
```

Output

```txt
<0 1>
< 0>
<0 1>
```



## Axe

Null pointers can be checked by simply comparing the pointer with 0.

```axe
If P=0
 Disp "NULL PTR",i
End
```



## Babel

In this example, we place nil on the stack, then perform an if-then-else (ifte) based on the value returned by the 'nil?' operator which returns true if top-of-stack (TOS) is nil. If TOS is nil, then we can be relieved, otherwise, the interpreter has gone absolutely haywire. The '<<' operator prints the selected string to STDOUT.

```babel
{ nil { nil? } { "Whew!\n" } { "Something is terribly wrong!\n" } ifte << }
```



## BASIC


=
## Applesoft BASIC
=
caveat: http://qconlondon.com/london-2009/presentation/Null+References%3A+The+Billion+Dollar+Mistake

Applesoft has no built-in object system.  The closest values to NULL or nil for each of the types are 0 for integers and floating point numbers, and "" for strings.  There is also the NUL character: CHR$(0).  One could create an object system using global variables and include a special value for NULL, but this is probably a mistake.

```ApplesoftBasic
TRUE = 1 : FALSE = 0
NULL = TRUE
IF NULL THEN PRINT "NULL"
NULL = FALSE
IF NOT NULL THEN PRINT "NOT NULL"
```
'''Output:'''
```txt
NULL
NOT NULL
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}
A null object has a pointer with a value of zero or one.

```bbcbasic
      PROCtestobjects
      END

      DEF PROCtestobjects
      PRIVATE a(), b(), s{}, t{}
      DIM a(123)
      DIM s{a%, b#, c$}

      IF !^a() <= 1 PRINT "a() is null" ELSE PRINT "a() is not null"
      IF !^b() <= 1 PRINT "b() is null" ELSE PRINT "b() is not null"
      IF !^s{} <= 1 PRINT "s{} is null" ELSE PRINT "s{} is not null"
      IF !^t{} <= 1 PRINT "t{} is null" ELSE PRINT "t{} is not null"
      ENDPROC
```

'''Output:'''

```txt

a() is not null
b() is null
s{} is not null
t{} is null

```



## Bracmat

Bracmat has no null objects.

The operators for multiplication, addition and concatenation have neutral elements, which are <code>1</code>, <code>0</code> and the empty string, respectively, but these are values like any other string.


```bracmat

a:?x*a*?z  {assigns 1 to x and to z}
a:?x+a+?z  {assigns 0 to x and to z}
a:?x a ?z  {assigns "" (or (), which is equivalent) to x and to z}

```



## C

C has the null pointer, written as "0", whose internal representation is often, [http://c-faq.com/null/varieties.html though not always], the same as integer zero. It is (supposedly) garanteed to be pointing to nothing, so receiving one of those likely means you are not looking at an object--<i>but</i>, there are occasions where changing content of a null pointer actually does something (say, on DOS); and a function that's supposed to return a pointer on success doesn't always return a 0 otherwise (e.g. mmap returns -1 for failure).

There is a very common macro, <code>NULL</code>, which evaluates to <code>(void*) 0</code> or an equivalent value. NULL is compatible with all pointer types, including both data pointers and function pointers.

The standard library defines NULL in locale.h, stddef.h, stdio.h, stdlib.h, string.h, time.h and wchar.h. [[POSIX]] systems also define NULL in dirent.h and unistd.h. Many C files include at least one of these headers, so NULL is almost always available.


```c
#include <stdio.h>

int main()
{
	char *object = 0;

	if (object == NULL) {
		puts("object is null");
	}
	return 0;
}
```



## C++

In C++ non-pointer types do not support null. (C++ provides value semantics rather than reference semantics). When using pointers C++ permits checking for null by comparing the pointer to a literal of 0, or (as in C) by way of a macro (NULL) which simply expands to 0.

```cpp
#include <iostream>
#include <cstdlib>
if (object == 0) {
   std::cout << "object is null";
}
```


boost::optional is available for cases where the programmer wishes to pass by value, but still support a null value.


```cpp

#include <boost/optional.hpp>
#include <iostream>

boost::optional<int> maybeInt()

int main()
{
  boost::optional<int> maybe = maybeInt();

  if(!maybe)
    std::cout << "object is null\n";
}

```



### C++11


In C++11 there is <code>nullptr</code> of type <code>nullptr_t</code> which represents a pointer to an invalid place. You can use it like

```cpp

int *p = nullptr;
...
if (p == nullptr){
  // do some thing
}
//or just
if (p){
  // do some thing
}

```


## C#
As with Java, any reference type may be null, and testing for nullity uses ordinary boolean operators.

```c#
if (foo == null)
    Console.WriteLine("foo is null");
```


C# 2.0 introduced nullable types for situations in which even primitive value types may have undefined or unknown values (for example, when reading from a database). Prior to the introduction of nullable types, these situations would require writing wrapper classes or casting to a reference type (e.g., object), incurring the penalties of boxing and reduced type safety. A variable with nullable type can be declared simply by adding the '?' operator after the type.

{{works with|C sharp|C#|2.0+}}

```c#
int? x = 12;
x = null;
```


Also new in C# 2.0 was the null coalescing operator, '??', which is simply syntactic sugar allowing a default value to replace an operand if the operand is null:

{{works with|C sharp|C#|2.0+}}

```c#
Console.WriteLine(name ?? "Name not specified");

//Without the null coalescing operator, this would instead be written as:
//if(name == null){
//	Console.WriteLine("Name not specified");
//}else{
//	Console.WriteLine(name);
//}
```



## Chapel


Objects variables without an initializer expression will be initiallized to nil:

```chapel
class C { };
var c:C; // is nil
writeln(if c == nil then "nil" else "something");
```



## Clojure

Clojure's <code>nil</code> is equivalent to Java's <code>null</code>.


```lisp
(let [x nil]
 (println "Object is" (if (nil? x) "nil" "not nil")))
```


Test wether symbol <code>foo</code> is defined:


```lisp
(find (ns-interns *ns*) 'foo)
```


Undefining <code>foo</code>:


```lisp
(ns-unmap *ns* 'foo)
```



## COBOL

Works with GnuCOBOL 2.0


```COBOL
       identification division.
       program-id. null-objects.
       remarks. test with cobc -x -j null-objects.cob

       data division.
       working-storage section.
       01 thing-not-thing      usage pointer.

      *> call a subprogram
      *>   with one null pointer
      *>   an omitted parameter
      *>   and expect void return (callee returning omitted)
      *>   and do not touch default return-code (returning nothing)
       procedure division.
       call "test-null" using thing-not-thing omitted returning nothing
       goback.
       end program null-objects.

      *> Test for pointer to null (still a real thing that takes space)
      *>   and an omitted parameter, (call frame has placeholder)
      *>   and finally, return void, (omitted)
       identification division.
       program-id. test-null.

       data division.
       linkage section.
       01 thing-one            usage pointer.
       01 thing-two            pic x.

       procedure division using
           thing-one
           optional thing-two
           returning omitted.

       if thing-one equal null then
           display "thing-one pointer to null" upon syserr
       end-if

       if thing-two omitted then
           display "no thing-two was passed" upon syserr
       end-if
       goback.
       end program test-null.
```


{{out}}

```txt

prompt$ cobc -x -j null-objects.cob
thing-one pointer to null
no thing-two was passed

```



## Common Lisp



### =Basics=


Common Lisp has an object denoted by the symbol <code>nil</code>. When the symbol <code>nil</code> is evaluated as an expression, it evaluates to itself.

<code>nil</code> uniquely represents boolean false, and so code like
```lisp
(if (condition) (do-this))
```
 is actually testing whether <code>(condition)</code> returns the value <code>nil</code>. The object <code>nil</code> is also used to denote the empty list which also terminates other lists.  The value is also used as a default when some function returns fewer values than expected. <code>(list (values))</code> produces <code>(nil)</code> (list containing one element, which is the empty list), because <code>(values)</code> produces no value, but the function call <code>(list ...)</code> needs to reduce the expression to a single argument value, and so <code>nil</code> is supplied.


### =Beginnings of Null Object=


The idea of making functions accept <code>nil</code> without failing did not appear in early Lisps. For instance <code>(car nil)</code> was erroneous: it was incorrect to try to access the first element of a non-list.

The defaulting behavior <code>(car nil)</code> which Common Lisp programmers take for granted was introduced in InterLisp, and then copied into MacLisp. (InterLisp had other liberties that do not survive into Common Lisp: it was possible to call a function with insufficient arguments, and the missing ones defaulted to <code>nil</code>. Likewise, excess arguments were ignored. CL has a disciplined syntax and semantics for default and variable arguments.)

This <code>(car nil) -> nil</code> behavior shows <code>nil</code> in an kind of new role: the role of a null object which takes methods that apply to other objects and provides some default non-failing behavior. It is the beginnings of the [[http://en.wikipedia.org/wiki/Null_Object_pattern null object design pattern]].

====Object-Oriented Null Object====

In Common Lisp, in fact, there is a class called <code>null</code>, of which the object <code>nil</code> is understood to be the only instance. Furthermore, the <code>null</code> class is at the bottom of the type spindle: it is a subclass of every class. This is in contrast with the type <code>T</code> which is a superclass of every class.

Since <code>null</code> is at the bottom of the class hierarchy, it is possible to write methods specialized to parameters of class <code>null</code> which will only be applicable if the argument is the object <code>nil</code>. No other object is a subtype of <code>null</code>.

Some traditional Lisp functions could be expressed using the object system like this.
Suppose that the <code>car</code> function did not have a safe defaulting behavior for <code>nil</code>. We could use the methods of the object system to define a <code>car*</code> which does have the safe behavior:


```lisp
(defmethod car* ((arg cons))
  (car arg))

(defmethod car* ((arg null))
  nil)
```


Now if we invoke <code>car*</code> on something which is neither a cons, nor <code>nil</code>, we get an error about no applicable method being found.

We can handle that ourselves by writing a method specialized to the master supertype <code>t</code>:


```lisp
(defmethod car* ((arg t))  ;; can just be written (defmethod car* (arg) ...)
  (error "CAR*: ~s is neither a cons nor nil" arg))
```


The classes <code>t</code> and <code>null</code> are widely exploited in Lisp OO programming.


## Component Pascal


```Oberon2

MODULE ObjectNil;
IMPORT StdLog;
TYPE
	Object = POINTER TO ObjectDesc;
	ObjectDesc = RECORD
	END;
VAR
	x: Object; (* default initialization to NIL *)

PROCEDURE DoIt*;
BEGIN
	IF x = NIL THEN
		StdLog.String("x is NIL");StdLog.Ln
	END
END DoIt;

END ObjectNil.

```



## D

In D ''is'' is used to perform bitwise identity, like to compare an object reference against null.

```d
import std.stdio;

class K {}

void main() {
    K k;
    if (k is null)
        writeln("k is null");
    k = new K;
    if (k !is null)
        writeln("Now k is not null");
}
```

{{out}}

```txt
k is null
Now k is not null
```



## Delphi


```Delphi
  // the following are equivalent
  if lObject = nil then
  ...

  if not Assigned(lObject) then
  ...
```


=={{header|Déjà Vu}}==
There isn't an actual null object, so generally falsy objects are used to indicate a missing value, or when that's impractical a specific ident:

```dejavu
if not obj:
    pass #obj is seen as null

if = :nil obj:
    pass #obj is seen as null
```



## DWScript

See [[#Delphi | Delphi]]


## Dyalect


Dyalect has a notion of <code>nil</code> - a special sigleton value which can be used in the cases when no other meaningful value can be provided.


```dyalect
var x = nil
if x == nil {
  //Do something
}
```



## E



```e>object == null</lang



## EchoLisp

The null object - '''null''' - is the same as the empty list (). It may be tested with the '''null?''' or '''!null?''' predicates. NB : null is not the same as the boolean #f (false). null evaluates to #t (true) in logical operations.


```lisp

null → null
() → null
(null? 3) → #f
(!null? 4) → #t
(null? null) → #t

;; careful - null is not false :
(if null 'OUI 'NON) → OUI

;; usual usage : recursion on lists until (null? list)
(define (f list)
    (when (!null? list)
    (write (first list)) (f (rest list))))

(f '( a b c))  →  a b c

```



## Eiffel

Any reference type variable can be Void. In the following example, STRING is a reference type, while INTEGER is an expanded type. The keyword "detachable" (as opposed to "attached") is used to indicate that the variable "s" may be Void. The default interpretation when neither of these two keywords is used depends on a compiler option. The first if statement will cause a compiler warning because an expanded type variable such as i will never be Void.

```Eiffel

class
	APPLICATION
inherit
	ARGUMENTS
create
	make

feature {NONE} -- Initialization

	make
		local
			i: INTEGER
			s: detachable STRING
		do
			if i = Void then
				print("i = Void")
			end
			if s = Void then
				print("s = Void")
			end
		end
end
```

{{out}}
```txt
s = Void
```



## Elixir

<code>nil</code> is atom in fact:

```elixir
iex(1)> nil == :nil
true
iex(2)> is_nil(nil)
true
```

<code>nil</code> is thought of as being <code>false</code> in the conditional expression.

If the condition given to <code>if/2</code> returns <code>false</code> or <code>nil</code>, the body given between <code>do</code>/<code>end</code> is not executed and it simply returns <code>nil</code>.

```elixir
iex(3)> if nil, do: "not execute"
nil
```



## Erlang


Erlang does not have an null object.

As an alternative, many applications tend to pick a convention for returning an empty condition and use that.

Example alternatives:
# Something like
```txt
{ok, 3} % normal case
```
 or
```txt
{err, no_more} % error case
```
 on error.
# Don't ever allow an undefined return value, and throw an exception instead.
# Return an atom:
## '''undefined'''*
## '''undef'''
## '''null'''
## '''nil'''
## '''none'''

'''undefined''' is often used by records as an initial value and the stdlib module.

Atoms are erlang's user-defined constants that always evaluates to is itself.  It is also equal to no other value else but itself.

=={{header|F_Sharp|F#}}==
As a .Net languages F# inherits the null as a potential value for object variables.
Other than in interfacing assemblies written in other .Net languages, null rarely serves a purpose in F# code.
Contrived code, to show using null, as per task description:

```fsharp
let sl : string list = [null; "abc"]

let f s =
    match s with
    | null -> "It is null!"
    | _ -> "It's non-null: " + s

for s in sl do printfn "%s" (f s)
```



## Factor


```factor
: is-f? ( obj -- ? ) f = ;
```



## Fantom


Test for equality with 'null', which is the null value.


```fantom

fansh> x := null
fansh> x == null
true
fansh> x = 1
1
fansh> x == null
false

```


Note, nullable objects have a type ending in a question mark, for example:

<code>Int? y := null</code> is valid, but

<code>Int y := null</code> is not.


## Forth

Standard ANS Forth does not distinguish a particular invalid memory value like NULL. Instead, ALLOCATE returns an out-of-band success code to indicate a failed allocation. Dictionary words have the option of throwing an exception on a dictionary space overrun. Forth lacks a NULL symbol because it has such a wide variety of target platforms. On some embedded targets, the memory space may be as small as 64 direct-mapped addresses, where eliminating a valid zero address would have a high price.

In practice, all notable hosted implementations follow the C practice of being able to treat a zero address (i.e. FALSE) as a null address for the purpose of list termination.


## FreeBASIC


```freebasic
'FB 1.05.0 Win64

' FreeBASIC does not have a NULL keyword but it's possible to create one using a macro

#Define NULL CPtr(Any Ptr, 0) '' Any Ptr is implicitly convertible to pointers of other types

Type Dog
  name As String
  age As Integer
End Type

Dim d As Dog Ptr = New Dog
d->Name = "Rover"
d->Age = 5
Print d->Name, d->Age
Delete d
d = NULL '' guard against 'd' being used accidentally in future

' in practice many FB developers would simply have written: d = 0 above
Sleep
```


{{out}}

```txt

Rover          5

```



## Go

Nil is a predefined identifier, defined for six types in Go.  In each case, it represents the zero value for the type, that is, the memory representation of all zero bytes.  This is the value of a newly created object.  In the cases of these six types, an object must be subsequently initialized in some way before it has much use.  Examples of initialization are given in the [[Undefined values#Go|Go solution]] of task [[Undefined values]].

```go

package main

import "fmt"

var (
    s []int       // slice type
    p *int        // pointer type
    f func()      // function type
    i interface{} // interface type
    m map[int]int // map type
    c chan int    // channel type
)

func main() {
    fmt.Println(s == nil)
    fmt.Println(p == nil)
    fmt.Println(f == nil)
    fmt.Println(i == nil)
    fmt.Println(m == nil)
    fmt.Println(c == nil)
}

```

Output is "true" in each case.


## Haskell


Haskell does not have a universal null value. There is a 'value of every type', the undefined value (sometimes written ⊥, 'bottom'), but it is essentially a sort of exception — any attempt to use it is an error.


```haskell
undefined      -- undefined value provided by the standard library
error "oops"   -- another undefined value
head []        -- undefined, you can't take the head of an empty list
```


When one would use "null" as a marker for "there is no normal value here" (e.g. a field which is either an integer or null), one uses the Maybe type instead. The definition of Maybe is:


```haskell
 data Maybe a = Nothing | Just a
```


That is, a <tt>Maybe Integer</tt> is either <tt>Nothing</tt> or <tt>Just </tt>&lt;some integer&gt;.

There are many ways to work with Maybe, but here's a basic case expression:


```haskell
case thing of
 Nothing -> "It's Nothing. Or null, whatever."
 Just v  -> "It's not Nothing; it is " ++ show v ++ "."
```


It is easy to work with Maybe type using do-notation (since Maybe is a monad):

```haskell
add_two_maybe_numbers x y do
  a <- x
  b <- y
  return (a+b)
```

Then

```haskell
*Main> add_two_maybe_numbers (Just 2) (Just 3)
Just 5
*Main> add_two_maybe_numbers (Just 2) Nothing
Nothing
```


=={{header|Icon}} and {{header|Unicon}}==
Icon/Unicon have a [[Icon%2BUnicon/Intro#null|null value/datatype]].  It isn't possible to undefine a variable.


```Icon
procedure main()
nulltest("a",a)                # unassigned variables are null by default
nulltest("b",b := &null)       # explicit assignment is possible
nulltest("c",c := "anything")
nulltest("c",c := &null)       # varibables can't be undefined
end

procedure nulltest(name,var)
return write(name, if /var then " is" else " is not"," null.")
end
```



## Io


```io
if(object == nil, "object is nil" println)
```



## J

J doesn't have an untyped NULL.  Instead, it has a concept of "fill". Numeric fill is 0, character fill is the space character, and boxed fill is the ace (a:) which is an empty box. Fill is what is used to pad an array structure when that is needed. (And some operations support using a user specified value in place of the default fill.)

To indicate "missing data", "normal" data is usually pressed into service (e.g. <tt>0</tt> or <tt>_1</tt> in a numeric context, <tt>' '</tt> in a literal context, <tt>a:</tt> in a boxed context, etc).  Frequently, missing data is represented by the empty vector <tt><nowiki>''</nowiki></tt>, or other arrays without any elements.

That said, undefined names in J are not associated with any data of any type. Furthermore, any attempt to use the value of an undefined is treated as an error (this is distinct from the concept of an empty array, which contains no data but which is not an error to use). However, it is possible to check if a name is defined before attempting to use it:


```J
isUndefined=: _1 = nc@boxxopen
```


Example use:


```J
   isUndefined 'foo'
1
   foo=:9
   isUndefined 'foo'
0
```


Note, of course, that this "name is not defined" state is not a first class value in J -- you can not create a list of "undefineds".

Finally, note: the concept of an empty array can be natural in J (and APL) for representing data which is not there -- it is the structural equivalent of the number zero.  That said, its implications can sometimes be non-obvious for people coming from a languages which requires that arrays have content.  As a result, you will sometimes encounter empty array jokes...

:Marie Pennysworth, having spent a productive day shopping, stopped by Robert Cuttingham's butcher shop.
:"How much for your t-bones?" she asked.
:"Eleven dollars per pound," he responded.
:"How about for your sirloin?" she continued.
:"Sirloin is thirteen dollars per pound today," he answered.
:"But Harkin's Grocery down the street is selling sirloin for nine dollars per pound!" she exclaimed.
:"So, why don't you buy it from them?" he asked.
:"Well, they're out," she sighed.
:He smiled, "When I am out, I only charge seven dollars a pound."

That said, note that a typical way to indicate missing or invalid data, in J, is to have a parallel array which is a bit mask (which selects the desired or valid values and, by implication, does not select the invalid values).  Or, as a logical equivalent: a list of indices which select the desired and/or valid values.  Alternatively, you can have an array without the invalid values and a bit mask which demonstrates how the data would be populated on a larger array -- in other words instead of 3,4,null,5 you could have (3 4 5) and (1 1 0 1).  And you can transform between some of these representations:


```j
   1 1 0 1#3 4 _ 5           NB. use bitmask to select numbers
3 4 5
   I.1 1 0 1                 NB. get indices for bitmask
0 1 3
   0 1 3 { 3 4 _ 5           NB. use indices to select numbers
3 4 5
   1 1 0 1 #inv 3 4 5        NB. use bitmask to restore original positions
3 4 0 5
   1 1 0 1 #!._ inv 3 4 5    NB. specify different fill element
3 4 _ 5
   3 4 5 (0 1 3}) _ _ _ _    NB. use indices to restore original positions
3 4 _ 5
```



## Java

In Java, "null" is a value of every reference type.

```java
// here "object" is a reference
if (object == null) {
   System.out.println("object is null");
}
```



## JavaScript

In Javascript <tt>null</tt> is the value that isn't anything. <tt>null</tt> is not an object, but because of a bug <tt>typeof null</tt> will return "object".

```javascript
if (object === null) {
  alert("object is null");
  // The object is nothing
}

typeof null === "object"; // This stands since the beginning of JavaScript
```



## jq

jq has <tt>null</tt> as a value, but while on the subject of nothing, it may be worth mentioning that jq also has a filter, <tt>empty</tt>, for producing an empty sequence, a.k.a. nothing.

<tt>null</tt> is distinct from <tt>false</tt>.
Here are some examples:

```jq
null|type      # => "null"

null == false  # => false

null == null   # => true

empty|type     # =>  # i.e. nothing (as in, nada)

empty == empty # =>  # niente

empty == "black hole" # =>  # Ничего
```



## Jsish

Like Javascript, Jsish has '''undefined''' and '''null'''.  Unlike Javascript, null is not typed as '''object''', but '''null'''.

Jsish, with parameter typed functions, also allows '''void''' as a type spec, to indicate the parameter (of whatever type) may be omitted by a caller.


```javascript
/* null non value */

if (thing == null) { puts("thing tests as null"); }
if (thing === undefined) { puts("thing strictly tests as undefined"); }
puts(typeof thing);
puts(typeof null);
puts(typeof undefined);
```


{{out}}

```txt
prompt$ jsish nulling.jsi
thing tests as null
thing strictly tests as undefined
undefined
null
undefined

```



## Julia

See language reference: https://docs.julialang.org/en/stable/manual/faq/#Nothingness-and-missing-values-1


## K

'''''K''''' has well developed notions of data ''null'' :
: The special numeric atoms 0I and 0N refer to integer infinity and “not-a-number” (or “null” in database parlance) concepts, and similarly 0i and 0n for floating-point.
eg :  ( 1 2 3 0N 6 7 )

and and missing value ''nil'' :
: Empty expressions in both list expressions and function expressions actually represent a special atomic value called ''nil''. ... A list may contain one or more empty items (i.e. the nil value _n), which are typically indicated by omission:

```k

  (1;;2) ~ (1 ; _n ; 2)    /  ~  is ''identical to'' or ''match'' .
1
  _n ~' ( 1 ; ; 2 )        /  ''match each''
0 1 0

additional properties :  _n@i and _n?i are i; _n`v is _n

```


For more detail on K's concept of typed nulls, see http://code.kx.com/wiki/Reference/Datatypes#Primitive_Types


## Kotlin

Kotlin distinguishes between non-nullable types and nullable types. The latter are distinguished from the former by a '?' suffix. Only nullable types have a 'null' value indicating that they don't currently refer to an object of their non-nullable equivalent.

In addition, Kotlin has a Nothing type which has no instances and is a sub-type of every other type. There is also a nullable Nothing? type whose only value is 'null' and so, technically, this is the type of 'null' itself.

Here are some examples:

```scala
// version 1.1.0

fun main(args: Array<String>) {
    val i: Int  = 3           // non-nullable Int type - can't be assigned null
    println(i)
    val j: Int? = null        // nullable Int type - can be assigned null
    println(j)
    println(null is Nothing?) // test that null is indeed of type Nothing?
}
```


{{out}}

```txt

3
null
true

```



## Lasso


```Lasso
local(x = string, y = null)
#x->isA(::null)
// 0 (false)

#y->isA(::null)
// 1 (true)

#x == null
// false

#y == null
//true

#x->type == 'null'
// false

#y->type == 'null'
//true
```



## Latitude


Nil is an object in Latitude, like any other.

```latitude
foo := Nil.
if { foo nil?. } then {
  putln: "Foo is nil".
} else {
  putln: "Foo is not nil".
}.
```


In particular, Nil satisfies the Collection mixin, so it can be treated as an (immutable) collection.

```latitude
Nil to (Array). ;; []
```


Nil is the default value returned if a method body is empty.

```latitude
func := {}.
func. ;; Nil
```



## Lingo

Null/nil is called "<Void>" in Lingo. Lingo doesn't distinguish undefined variables from <Void> objects, and by using the constant VOID you can even assign <Void> to variables. Functions that don't return anything, return <Void>. Checking for <Void> (e.g. by using built-in function voidP) can be used to implement optional function arguments: if voidP() returns TRUE (1) for some argument, a default value can be assigned in the function body.

```lingo
put _global.doesNotExist
-- <Void>

put voidP(_global.doesNotExist)
-- 1

x = VOID
put x
-- <Void>

put voidP(x)
-- 1
```



## Lily

Lily doesn't provide a built-in nothing type, but allows one to be created using enum class:


```Lily
enum class Option[A] {
  Some(A)
  None
}

# Only variables of class Option can be assigned to None.

# Type: Option[integer]
var v = Some(10)

# Valid: v is an Option, and any Option can be assigned to None
v = None

# Invalid! v is an Option[integer], not just a plain integer.
v = 10

# Type: integer
var w = 10

# Invalid! Likewise, w is an integer, not an Option.
w = None
```



## Logo


```logo
to test :thing
if empty? :thing [print [list or word is empty]]
end

print empty? []  ; true
print empty? "|| ; true
```



## Lua


```lua

isnil = (object == nil)
print(isnil)

```



## M2000 Interpreter


### For Com Objects

There is a Nothing to assign to a COM object to released (but time to actually released depends from system). A com pointer can't get another value (only the first value, and the Nothing at the end).

```M2000 Interpreter

Module CheckWord {
      Declare Alfa "WORD.APPLICATION"
      Declare Alfa Nothing
      Print Type$(Alfa)="Nothing"
      Try ok {
            Declare Alfa "WORD.APPLICATION"
            \\ we can't declare again Alfa

      }
      If Not Ok Then Print Error$  ' return Bad Object declaration
}
CheckWord

```


### For Containers

Container's pointers (for arrays, inventories, stack) we have to assign an empty container, there is not a null one.


```M2000 Interpreter

Module CheckContainers {
      \\ Arrays  (A() and B() are value types)
      Dim A(10)=1, B()
      \\ B() get a copy of A(), is not a reference type
      B()=A()
      \\ we make a pointer to Array
      B=A()
      \\ now B is a reference type object
      Print Len(B)=10  ' ten items
      B+=10
      Print A(3)=11, A(7)=11
      \\ we can change pointer using a pointer to an empty array
      B=(,)
      \\ we can erase A() and B()
      Dim A(0), B(0)
      Print Len(A())=0, Len(B())=0
      Print Len(B)=0
      B=(123,)
      \\ B() is a value type so get a copy
      B()=B
      Print Len(B)=1, B(0)=123
      \\ Using Clear we pass a new empty array
      Clear B
      Print Type$(B)="mArray"
      Print Len(B)=1, B(0)=123


      \\ Inventrories. Keys must be unique (for normal inventories)
      Inventory M=1,2,3,4:=400,5
      Print M
      Clear M
      Inventory M=1,2,3,4,5
      Print M

      \\ Inventory Queue can have same keys.
      Inventory Queue N=1,1,2:="old",2:="ok",3
      If Exist(N,2) Then Print Eval$(N)="ok", Eval(N!)=3 ' 4th item
      Clear N
      Print Len(N)=0, Type$(N)="Inventory"

      \\ Stack Object
       Z=Stack:=1,2,3
       Stack Z {
            While not empty {Print Number}
      }
      Print Len(Z)=0
      Z=Stack((Stack:=1,2,3,4),Stack:=20,30,40 )
      Print Len(Z)=7
      Print Z    ' 1 2 3 4 20 30 49
      Z=Stack  ' This is an empty stacl
      Print Len(Z)=0
      Print Type$(Z)="mStiva"
}
CheckContainers

```


### For Groups

Groups are value types, but we can make reference to them,or pointer to them

A Named referenced can't get a new reference

A pointer to a named group is actual a reference, and can change type and reference

A pointer to a copy of group (as float group) is actually a pointer to group.

Pointers to groups can be point to an empty Group, assigning a 0& value (a long type)



```M2000 Interpreter

Module CheckGroup {
      \\ Group Objects
      Class Beta {x=10, z=3}
      G=Beta()
      Group G {
            \\ we can add a member
            y=2
      }
      Print Group.Count(G)=3  ' 3 members ' not counting modules/functions/operators
      \\ G is a value type
      Print G.x=10
      G.x+=20
      \\ we can make a pointer to G
      pG->G
      Print pG=>x=30
      \\ we can assign a null, which give a pointer to an empty group
      pG->0&
      Print type$(pG)="Group"
      \\ make a pointerto a copy of G
      pG->(G)
      G.x=0
      Print pG=>x=30
      \\ assign new value to G, using a copy of pG
      G=Group(pG)
      Print G.x=30
      pG1=pG
      pG->0&
      Print pG1=>x=30, Valid(pG=>x)=False
      pG->G
      \\ we can clear G, but  clear only te relationship with G.x, G.y, G.z
      Clear G
      Print Group.Count(G)=0
      \\ G.x exist.
      \\ pG=>x also exist becase pG hold actual name of G
      Print G.x, pG=>x
      pG1=>x++
      For pG1 {.y+=10 : .z+=40}
      \\ we can reload the group list
      G=Group(pG1)
      Print Group.Count(G)=3
      For G {
            Print .x, .y, .z  ' 31 12 43
      }
      \\ now we get all variable's list, and types of them (For simple numeric variables we get value also)
      List
}
CheckGroup

```



## Maple

In Maple, NULL and () represent the null object.

```maple
a := NULL;
                                                         a :=
is (NULL = ());
                                                         true
if a = NULL then
    print (NULL);
end if;

```

A null object is different from an undefined value.

```maple
b := Array([1, 2, 3, Integer(undefined), 5]);
                                                        b := [ 1 2 3 undefined 5 ]
numelems(b);
                                                        5
b := Array([1, 2, 3, Float(undefined), 5]);
                                                        b := [ 1 2 3 Float(undefined) 5 ]
numelems(b);
                                                        5
b := Array([1, 2, 3, NULL, 5]);
                                                        b := [ 1 2 3 5 ]
numelems(b);
                                                        4

```



## Mathematica

Mathematica can assign a Null value to a symbol, two examples:

```Mathematica>x=Null;</lang


```Mathematica
x =.
x = (1 + 2;)
FullForm[x]
```

Both set x to be Null. To specifically test is something is Null one can use the SameQ function (with infix operator: ===):

```Mathematica
SameQ[x,Null]
```

Or equivalent:

```Mathematica>x===Null</lang

will give back True if and only if x is assigned to be Null. If x is empty (nothing assigned) this will return False.
To test if an object has something assigned (number, list, graphics, null, infinity, symbol, equation, pattern, whatever) one uses ValueQ:

```Mathematica
x =.;
ValueQ[x]
x = 3;
ValueQ[x]
```

gives:

```Mathematica
False
True
```


=={{header|MATLAB}} / {{header|Octave}}==
The closest think to a NULL element in Matlab/Octave is an empty field or empty string; empty fields in a conditional expression evaluate to false.

```MATLAB
a = []; b='';
isempty(a)
isempty(b)
if (a)
  1,
else,
  0
end;
```



```txt
octave:4> a = []; b='';
octave:5> isempty(a)
ans =  1
octave:6> isempty(b)
ans =  1
octave:7> if (a) 1, else, 0, end;
ans = 0
```




## Maxima

There is no ''null object'' in Maxima. Usually, a function that returns nothing (as the builtin "disp") returns in fact the symbol 'done.


## MAXScript


```maxscript
if obj == undefined then print "Obj is undefined"
```


=={{header|Modula-3}}==
In Modula-3, <code>NIL</code> is a value, and <code>NULL</code> is a type.  The <code>NULL</code> type contains only one value, <code>NIL</code>.  <code>NULL</code> is a subtype of all reference types, which allows all reference types to have the value <code>NIL</code>.

This can lead to errors, if for example you write:

```modula3>VAR foo := NIL</lang

This (most likely incorrectly) gives foo the type <code>NULL</code>, which can only have the value <code>NIL</code>, so trying to assign it anything else will not work.  To overcome this problem, you must specify the reference type when declaring foo:

```modula3>VAR foo: REF INTEGER := NIL;</lang


```modula3
IF foo = NIL THEN
  IO.Put("Object is nil.\n");
END;
```



## MUMPS

A variable can be declared implicitly by using it as on the left side in a SET, or by making a new version for the current scope with a NEW statement. A variable can have descendants without having a value set.

The $DATA (or $D) function will return a number:
<table border="1">
  <tr>
  <th>$DATA returns:</th>
  <th colspan="3">Variable is defined</th>
  <tr align="center">
    <th rowspan=3>Variable has children</th>
    <td> </td>
    <td><strong>No</strong></td>
    <td><strong>Yes</strong></td>
  </tr>
  <tr>
    <td><strong>No</strong></td>
    <td>0</td>
    <td>1</td>
  </tr>
  <tr>
    <td><strong>Yes</strong></td>
    <td>10</td>
    <td>11</td>
  </tr>
</table>
<p>Or, by examples (in immediate mode):</p>

```MUMPS

CACHE>WRITE $DATA(VARI)
0
CACHE>SET VARI="HELLO" WRITE $DATA(VARI)
1
CACHE>NEW VARI WRITE $DATA(VARI) ;Change to a new scope
0
CACHE 1S1>SET VARI(1,2)="DOWN" WRITE $DATA(VARI)
10
CACHE 1S1>WRITE $DATA(VARI(1))
10
CACHE 1S1>WRITE $D(VARI(1,2))
1
CACHE 1S1>SET VARI(1)="UP" WRITE $DATA(VARI(1))
11
<CACHE 1S1>QUIT ;Leave the scope

<CACHE>W $DATA(VARI)," ",VARI
1 HELLO

```



## Neko


```ActionScript
/**
 <doc>
 <p>Neko uses <i>null</i> for undefined variables,
  and also as a programmer accessible value.</p>
 <p>The <i>null</i> value can be treated as a boolean value with the
  builtin $istrue, and tests as false.</p>
 </doc>
*/

var n = null
if n == null $print("n is null\n")
if $not($istrue(n)) $print("and tests as boolean false\n")
```



## NetRexx

In NetRexx as in Java, "null" is a value of every reference type.


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

robject = Rexx -- create an object for which the value is undefined
say String.valueOf(robject) -- will report the text "null"
if robject = null then say 'Really, it''s "null"!'

```

'''Output:'''

```txt

null
Really, it's "null"!

```



## NewLISP


```newlisp

#! /usr/local/bin/newlisp
(setq myobject nil)
(println (nil? myobject))
(exit)

```


```txt

true

```



## Nim

There is a <code>nil</code> value in Nim, which is the same as a 0. It can be explicitly forbidden as a value:

```nim
let s: pointer = nil

{.experimental: "notnil".}
let ns: pointer not nil = nil # Compile time error
```


=={{header|Oberon-2}}==
{{works with|oo2c}}

```oberon2

MODULE Null;
IMPORT
  Out;
TYPE
  Object = POINTER TO ObjectDesc;
  ObjectDesc = RECORD
  END;

VAR
  o: Object;  (* default initialization to NIL *)

BEGIN
  IF o = NIL THEN Out.String("o is NIL"); Out.Ln END
END Null.

```

{{out}}

```txt

o is NIL

```


=={{header|Objective-C}}==
The value <code>nil</code> is used to indicate that an object pointer (variable of type <code>id</code>) doesn't point to a valid object.

```objc
// here "object" is an object pointer
if (object == nil) {
   NSLog("object is nil");
}
```

An interesting thing is that in Objective-C, it is possible to send a message to <code>nil</code>, and the program will not crash or raise an exception (nothing will be executed and <code>nil</code> will be returned in place of the usual return value).

```objc
[nil fooBar];
```


Note that <code>nil</code> is distinct from <code>NULL</code>, which is only used for regular C pointers.

For class pointers (values of type <code>Class</code>), they have a separate null pointer value called <code>Nil</code>.

Confusingly, there is also <code>NSNull</code>, a singleton class with one value, <code>[NSNull null]</code>, used as a dummy object to represent the lack of a useful object. This is needed in collections like arrays and dictionaries, etc., because they do not allow <code>nil</code> elements, so if you want to represent some "empty" slots in the array you would use this.


## Objeck

In Objeck, "Nil" is a value of every reference type.

```Objeck

# here "object" is a reference
if(object = Nil) {
   "object is null"->PrintLine();
};

```



## OCaml

Maybe the closest type of OCaml would be the type option, which is defined like this in the standard library:

```ocaml
type 'a option = None | Some of 'a
```


```ocaml
match v with
| None -> "unbound value"
| Some _ -> "bounded value"
```



## Oforth


null is an object, the only instance of Null class.
When an object is created, all attributes are initiallized to null value.
When a method or function is called, all local variables begin with null value.


```Oforth
null isNull
"abcd" isNull
: testNull { | a | a ifNull: [ "Variable value is null" println ] ;
```



## ooRexx

ooRexx has a special singleton object called .nil that is used to indicate the absence of values in some situations (such as the default values returned from collection objects).

```ooRexx

   if a[i] == .nil then say "Item" i "is missing"

```

Uninitialized ooRexx variables do not evaluate to .nil, but rather the character string name of the variable (all uppercase).  The var() built-in function allows variable validity to be tested:

```ooRexx
a=.array~of('A','B')
  i=3
  if a[i] == .nil then say "Item" i "of array A is missing"
  if \var("INPUT") then say "Variable INPUT is not assigned"
  if \var("var") then say "Variable" var "is not assigned"
```

Output:

```txt
Item 3 of array A is missing
Variable INPUT is not assigned
Variable VAR is not assigned
```



## Oz

There is no explicit null in Oz.

### Unbound variables

If an unbound variable is accessed, the current thread will be suspended:

```oz
declare
  X
in
  {Show X+2}  %% blocks
```

If you later assign a value to X in another thread, the original thread will resume and print the result of the addition. This is the basic building block of Oz' [http://c2.com/cgi/wiki?DeclarativeConcurrency declarative concurrency].

### Undefined values

Access to undefined values (like using an out-of-range array index or a non-existing record feature) will usually provoke an exception in Oz.

It is also possible to assign a unique "failed" value to a variable. Such a failed value encapsulates an exception. This can be useful in concurrent programming to propagate exceptions across thread boundaries.

```oz
declare
  X = {Value.failed dontTouchMe}
in
  {Wait X}  %% throws dontTouchMe
```


Sometimes algebraic data types like Haskell's Maybe are simulated using records.

```oz
declare
  X = just("Data")
in
  case X of nothing then skip
  [] just(Result) then {Show Result}
  end
```



## PARI/GP

GP does not have good facilities for this, but this test suffices for most purposes:

```parigp
foo!='foo
```



## Pascal

See [[#Delphi | Delphi]]


## Perl

In Perl, <code>undef</code> is a special scalar value, kind of like null in other languages. A scalar variable that has been declared but has not been assigned a value will be initialized to <code>undef</code>. (Array and hash variables are initialized to empty arrays or hashes.)

If <code>strict</code> mode is not on, you may start using a variable without declaring it; it will "spring" into existence, with value <code>undef</code>. In <code>strict</code> mode, you must declare a variable before using it. Indexing an array or hash with an index or key that does not exist, will return <code>undef</code> (however, this is not an indication that the index or key does not exist; rather, it could be that it does exist, and the value is <code>undef</code> itself). If <code>warnings</code> is on, most of the time, if you use the <code>undef</code> value in a calculation, it will produce a warning. <code>undef</code> is considered false in boolean contexts.

It is possible to use <code>undef</code> like most other scalar values: you can assign it to a variable (either by doing <code>$var = undef;</code> or <code>undef($var);</code>), return it from a function, assign it to an array element, assign it to a hash element, etc. When you do list assignment (i.e. assign a list to a list of variables on the left side), you can use <code>undef</code> to "skip" over some elements of the list that you don't want to keep.

You can check to see if a value is <code>undef</code> by using the <code>defined</code> operator:

```perl
print defined($x) ? 'Defined' : 'Undefined', ".\n";
```

From the above discussion, it should be clear that if <code>defined</code> returns false, it does not mean that the variable has not been set; rather, it could be that it was explicitly set to <code>undef</code>.

Starting in Perl 5.10, there is also a [http://perldoc.perl.org/perlop.html#C-style-Logical-Defined-Or defined-or] operator in Perl. For example:

```perl
say $number // "unknown";
```

prints $number if it is defined (even if it is false) or the string "unknown" otherwise.


## Perl 6

{{trans|Haskell}}  (as it were...)

In Perl 6 you can name the concept of <tt>Nil</tt>, but it not considered an object, but rather the <i>absence</i> of an object, more of a "bottom" type.  The closest analog in real objects is an empty list, but an empty list is considered defined, while <tt>Nil.defined</tt> always returns false.  <tt>Nil</tt> is what you get if you try to read off the end of a list, and <tt>()</tt> is just very easy to read off the end of...  <tt>:-)</tt>

If you try to put <tt>Nil</tt> into a container, you don't end up with a container that has <tt>Nil</tt> in it.  Instead the container reverts to an uninitialized state that is consistent with the declared type.  Hence, Perl 6 has the notion of typed undefined values, that are real objects in the sense of "being there", but are generic in the sense of representing type information without being instantiated as a real object.  We call these <i>type objects</i> since they can stand in for real objects when one reasons about the types of objects.  So type objects fit into the type hierarchy just as normal objects do.  In physics terms, think of them as "type charge carriers" that are there for bookkeeping between the "real" particles.

All type objects derive from <tt>Mu</tt>, the most-undefined type
object, and the object most like "null" in many languages.  All other object types derive from <tt>Mu</tt>,
so it is like <tt>Object</tt> in other languages as well, except <tt>Mu</tt> also encompasses various objects that are not discrete, such as junctions.  So Perl 6 distinguishes <tt>Mu</tt> from <tt>Any</tt>, which is the type that functions the most like a discrete, mundane object.

Mostly the user doesn't have to think about it.  All object containers behave like "Maybe" types in Haskell terms; they may either hold a valid value or a "nothing" of an appropriate type.
Most containers default to an object of type <tt>Any</tt> so you don't accidentally send quantum superpositions (junctions) around in your program.


```perl6
my $var;
say $var.WHAT;      # Any()
$var = 42;
say $var.WHAT;      # Int()
say $var.defined;   # True
$var = Nil;
say $var.WHAT;      # Any()
say $var.defined    # False
```


You can declare a variable of type <tt>Mu</tt> if you wish to propagate superpositional types:


```perl6
my Mu $junction;
say $junction.WHAT;      # Mu()
$junction = 1 | 2 | 3;
say $junction.WHAT;      # Junction()
```


Or you can declare a more restricted type than <tt>Any</tt>


```perl6
my Str $str;
say $str.WHAT;      # Str()
$str = "I am a string.";
say $str.WHAT;      # Str()
$str = 42;          # (fails)
```


But in the Perl 6 view of reality, it's completely bogus to ask
for a way "to see if an object is equivalent to the null object."
The whole point of such a non-object object is that it doesn't exist,
and can't participate in computations.  If you think you mean the
null object in Perl 6, you really mean some kind of generic object
that is uninstantiated, and hence undefined.  One of those is your "null object",
except there are many of them, so you can't just check for equivalence.  Use the <tt>defined</tt> predicate (or match on a subclass of your type that forces recognition of abstraction or definedness).

Perl 6 also has <tt>Failure</tt> objects that, in addition to being
undefined carriers of type, are also carriers of the <i>reason</i>
for the value's undefinedness.  We tend view them as lazily thrown
exceptions, at least until you try to use them as defined values,
in which case they're thrown for real.


## Phix

There is a builtin NULL, however it is equivalent to the integer 0 and will trigger a type check if assigned to a variable declared as string or sequence. In most programs the zero-length string/sequence (""/{}) suffices,
but if you want a variable that can be a string/sequence or NULL, but not other arbitrary integer/float values, use something like the following user-defined types:

```Phix
type nullableString(object o)
    return string(o) or o=NULL
end type
nullableString s
s = "hello"
s = NULL
--s = 1 -- error
--s = {1,2,3} -- error

type nullableSequence(object o)
    return sequence(o) or o=NULL
end type
nullableSequence q
q = {1,2,3}
q = "string"    -- fine (strings are a subset of sequences)
q = NULL
--q = 1         -- error
```

See also [[Undefined_values#Phix|Undefined_values]]


## PHL



```phl
if (obj == null) printf("obj is null!\n");
```



## PHP

There is a special value <tt>NULL</tt>. You can test for it using <tt>is_null()</tt> or <tt>!isset()</tt>

```php
$x = NULL;
if (is_null($x))
  echo "\$x is null\n";
```



## PicoLisp

New internal symbols are initialized with the value NIL. NIL is also the value
for "false", so there is never really an "undefined value".
'[http://software-lab.de/doc/refN.html#not not]' is the predicate to check for
NIL, but many other (typically flow control) functions can be used.

```PicoLisp
(if (not MyNewVariable)
   (handle value-is-NIL) )
```

or

```PicoLisp
(unless MyNewVariable
   (handle value-is-NIL) )
```


## Pike

In Pike all variables are initialized to <math>0</math>, regardless of their type. thus <math>0</math> functions as a <code>Null</code> value for all types except integer.

<math>0</math> is also used to indicate the absence of a key or object member.

to tell the difference between a value <math>0</math> and absence of a key, <code>zero_type()</code> is used:

```Pike>
 mapping bar;
> bar;
Result: 0
> bar = ([ "foo":0 ]);
> bar->foo;
Result 0;
> zero_type(bar->foo);
Result: 0
> bar->baz;
Result: 0
> zero_type(bar->baz);
Result: 1
```



## PL/I


```PL/I

declare x fixed decimal (10);
...
if ^valid(x) then signal error;

declare y picture 'A9XAAA9';
...
if ^valid(y) then signal error;

```

Comment:-
In the picture specification, the content of variable y
must consist of letters where the letter 'A' is given,
digits or space where the digit '9' appears,
and the letter X signfies that any character is acceptable.


## PowerShell

In PowerShell the automatic variable <code>$null</code> represents a null value.  Comparisons are not left/right symmetrical which means placing <code>$null</code> on the left side greatly assists when comparing to an array.

```powershell
if ($null -eq $object) {
    ...
}
```



## PureBasic

All variables that has not yet been given any other value will be initiated to #Null

```PureBasic
If variable = #Null
  Debug "Variable has no value"
EndIf
```



## Python


```python
x = None
if x is None:
  print "x is None"
else:
  print "x is not None"
```

Output:
```txt

x is None

```



## R

R has the special value NULL to represent a null object.  You can test for it using the function is.null.  Note that R also has a special value NA to represent missing or unknown values.

```R
is.null(NULL)       # TRUE
is.null(123)        # FALSE
is.null(NA)         # FALSE
123==NULL           # Empty logical value, with a warning
foo <- function(){} # function that does nothing
foo()               # returns NULL
```



## Racket


"null", or its literal form "'()", is used to denote empty lists and
sometimes it is used as a generic null value.


```Racket

-> null
'()
-> (null? null)
#t
-> (null? 3)
#f

```


But a value that is more used as a generic "nothing" value is "#f",
false.  Racket also has a void value, mostly the result of side-effect
functions.  (And an undefined value.)


## Raven

{{improve|Raven|Add NULL handling with MySQL data.}}

```Raven
NULL as $v
$v NULL =     # TRUE
$v NULL !=    # FALSE

1 NULL =      # FALSE
1.1 NULL =    # FALSE

NULL as $v2
$v2 $v =      # TRUE
```



## REBOL


```REBOL
x: none

print ["x"  either none? x ["is"]["isn't"]  "none."]
```


Output:


```txt
x is none.
```


REBOL also has the concept of <code>unset</code> values, testable with <code>get/any</code>


```REBOL
unset? get/any 'some-var
unset? get 'some-var
```


{{out}}

```txt
true
** Script Error: some-var has no value
** Near: unset? get 'some-var
```



## REXX

REXX can have variables with a null value.

With the   '''symbol'''   built-in function, it can be determined if a variable is defined (or not).

The   '''length'''   built-in function can be used to see what the length of the value of a defined variable.

A variable with a   '''null'''   value has a length of   '''0'''   (zero).


The   '''drop'''   statement can be used to "undefine" a REXX variable.

```rexx
/*REXX program demonstrates null strings, and also undefined values. */

if symbol('ABC')=="VAR" then say 'variable ABC is defined, value='abc"<<<"
                        else say "variable ABC isn't defined."
xyz=47
if symbol('XYZ')=="VAR" then say 'variable XYZ is defined, value='xyz"<<<"
                        else say "variable XYZ isn't defined."
drop xyz
if symbol('XYZ')=="VAR" then say 'variable XYZ is defined, value='xyz"<<<"
                        else say "variable XYZ isn't defined."
cat=''
if symbol('CAT')=="VAR" then say 'variable CAT is defined, value='cat"<<<"
                        else say "variable CAT isn't defined."
```

'''output'''
<pre style=overflow:scroll">
variable ABC isn't defined.
variable XYZ is defined, value=47<<<
variable XYZ isn't defined.
variable CAT is defined, value=<<<

```



## Ring


```ring

see isnull(5) + nl +        # print 0
isnull("hello") + nl +      # print 0
isnull([1,3,5]) + nl +      # print 0
isnull("") + nl +           # print 1
isnull("NULL")              # print 1

```



## Ruby

The value when referring to the instance variable which isn't initialized is nil.

```ruby
puts "@object is nil" if @object.nil?		# instance variable

puts "$object is nil" if $object.nil?		# global variable, too

# It recognizes as the local variable even if it isn't executed.
object = 1  if false
puts "object is nil" if object.nil?

# nil itself is an object:
puts nil.class  # => NilClass
```

{{out}}

```txt

@object is nil
$object is nil
object is nil
NilClass

```



## Rust



```rust
// If an option may return null - or nothing - in Rust, it's wrapped
// in an Optional which may return either the type of object specified
// in <> or None. We can check this using .is_some() and .is_none() on
// the Option.

fn check_number(num: &Option<u8>) {
    if num.is_none() {
        println!("Number is: None");
    } else {
        println!("Number is: {}", num.unwrap());
    }
}

fn main() {
    let mut possible_number: Option<u8> = None;
    check_number(&possible_number);

    possible_number = Some(31);
    check_number(&possible_number);
}
```


=={{header|S-lang}}==
S-Lang uses NULL; it is the only object of type Null_Type:
<lang S-lang>variable foo = NULL;
print(foo);
if (foo == NULL)
   print(typeof(foo));

```

{{out}}

```txt
NULL
Null_Type

```



## Scala

[http://blog.sanaulla.info/2009/07/12/nothingness/ This blog post] has a good explanations of the different types of null-like values.


```scala

scala> Nil
res0: scala.collection.immutable.Nil.type = List()

scala> Nil == List()
res1: Boolean = true

scala> Null
<console>:8: error: not found: value Null
              Null
              ^

scala> null
res3: Null = null

scala> None
res4: None.type = None

scala> Unit
res5: Unit.type = object scala.Unit

scala> val a = println()
a: Unit = ()

```



## Scheme


```scheme
(null? object)
```

Note: "null?" here tests whether a value is the empty list.


## Sidef

The absence of a value is represented by ''nil''

```ruby
var undefined;         # initialized with an implicit nil
say undefined==nil;    # true
say defined(nil)       # false
```


However, ''nil'' is not an object, so we can't call methods on it. Alternatively, Sidef provides the ''null'' object:


```ruby
var null_obj = null;        # initialize with a null value
say null_obj.is_a(null);    # true
say defined(null_obj);      # true
```



## Slate



```slate>Nil isNil = True.</lang




## Smalltalk



```smalltalk
object isNil ifTrue: [ "true block" ]
             ifFalse: [ "false block" ].
nil isNil ifTrue: [ 'true!' displayNl ]. "output: true!"
foo isNil ifTrue: [ 'ouch' displayNl ].
x := (foo == nil).
x := foo isNil
```


notice that nil is the singleton instance of the UndefinedObject class; i.e. it is a first class object. Thus we can do:

```smalltalk
foo := nil.
foo class. "-> UndefinedObject"
foo respondsTo: #'bar'. "asking if a message is implemented"

foo class compile:'fancyOperation ^ 123'.
foo fancyOperation "->123"
```


the last example being for demonstration only - it is not considered well behaved to add arbitrary code that way, except for framework support, such as encoding, decoding marshalling etc.)


## Standard ML

Maybe the closest type of Standard ML would be the type option, which is defined like this in the standard library:

```sml
datatype 'a option = NONE | SOME of 'a
```


```sml>case v of NONE =
 "unbound value"
        | SOME _ => "bounded value"
```



## Swift

Swift has <code>Optional<T></code> type, where <code>nil</code> means a lack of value.
<code>T?</code> is syntactic sugar for <code>Optional<T></code>.

```swift>let maybeInt: Int? = nil</lang

To just check if variable is nil, you can use <code>==</code> operator.

```swift
if maybeInt == nil {
  print("variable is nil")
} else {
  print("variable has some value")
}
```


Usually you want to access the value after checking if it's nil. To do that you use <code>if let</code>

```swift
if let certainlyInt = maybeInt {
  print("variable has value \(certainlyInt)")
} else {
  print("variable is nil")
}
```



## Tcl

In Tcl, where every value is a string, there is no out-of band value corresponding to NULL. In many cases, using the empty string is sufficient:

```Tcl
if {$value eq ""} ...
```

A stricter approximation to NULL can be had with non-existing variables or elements of a dict or array:

```Tcl
if {![info exist nullvar]} ...
if {![info exists arr(nullval)]} ...
if {![dict exists $dic nullval]} ...
```

Note that lists do not support anything like nulls, since they are strictly sequences of values.


## Ursa


```ursa
# the type at declaration doesn't matter
decl int x

set x null
if (= x null)
    out "x is null" endl console
else
    out "x is not null" endl console
end if
```



## VBA


```vb
Public Sub Main()
    Dim c As VBA.Collection

    ' initial state: Nothing
    Debug.Print c Is Nothing

    ' create an instance
    Set c = New VBA.Collection
    Debug.Print Not c Is Nothing

    ' release the instance
    Set c = Nothing
    Debug.Print c Is Nothing
End Sub
```


## Visual Basic

{{works with|VB6}}
Null by the definition of this task is called "Nothing" in VB6:

```vb

Public Sub Main()
Dim c As VBA.Collection

' initial state: Nothing
Debug.Assert c Is Nothing

' create an instance
Set c = New VBA.Collection
Debug.Assert Not c Is Nothing

' release the instance
Set c = Nothing
Debug.Assert c Is Nothing

End Sub

```

The Null keyword has a different meaning in VB6: it's one of the states that a Variant type variable can be in. Null means that a Variant doesn't hold valid (i.e.: defined) data.

```vb

Public Sub Main()
Dim v As Variant

' initial state: Empty
Debug.Assert IsEmpty(v)
Debug.Assert VarType(v) = vbEmpty

v = 1&
Debug.Assert VarType(v) = vbLong

' assigning the Null state
v = Null
' checking for Null state
Debug.Assert IsNull(v)
Debug.Assert VarType(v) = vbNull

End Sub

```



## Wart

The null value <code>nil</code> is also the only false value.

```wart
(not nil)

```



## zkl

In zkl, there isn't a C like 0/NULL, a value that, if referenced, causes bad things to happen. There is an Object, Void, that is used as generic NULL like thing but it is just another object.

```zkl
if(Void == n) ...
return(Void)
```


{{omit from|GUISS}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a null value. -->

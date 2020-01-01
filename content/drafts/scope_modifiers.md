+++
title = "Scope modifiers"
description = ""
date = 2019-09-13T12:55:46Z
aliases = []
[extra]
id = 4362
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
Most programming languages offer support for [[Creating a function|subroutines]].
When execution changes between subroutines, different sets of variables and functions ("scopes") are available to the program.
Frequently these sets are defined by the placement of the variable and function declarations ("static scoping" or "lexical scoping").
These sets may also be defined by special modifiers to the variable and function declarations.

Show the different scope modifiers available in your language and briefly explain how they change the scope of their variable or function.
If your language has no scope modifiers, note it.


## Ada


### Public and private declarative parts

In [[Ada]] declarative region of a package has publicly visible and private parts.
The private part is introduced by '''private''':

```ada
package P is
   ... -- Declarations placed here are publicly visible
private
   ... -- These declarations are visible only to the children of P
end P;
```

Correspondingly a type or object declaration may be incomplete in the public part providing an official interface. For example:

```ada
package P is
   type T is private; -- No components visible
   procedure F (X : in out T); -- The only visible operation
   N : constant T; -- A constant, which value is hidden
private
   type T is record -- The implementation, visible to children only
      Component : Integer;
   end record;
   procedure V (X : in out T); -- Operation used only by children
   N : constant T := (Component => 0); -- Constant implementation
end P;
```


===Bodies (invisible declarations)===
The keyword '''body''' applied to the packages, protected objects and tasks. It specifies an implementation of the corresponding entity invisible from anywhere else:

```ada
package body P is
   -- The implementation of P, invisible to anybody
   procedure W (X : in out T); -- Operation used only internally
end P;
```


### Private children

The keyword '''private''' can be applied to the whole package, a child of another package:

```ada
private package P.Q is
   ... -- Visible to the siblings only
private
   ... -- Visible to the children only
end P.Q;
```

This package can be then used only by private siblings of the same parent P.


## ALGOL 60

Algol 60 has one scope modifier: 'OWN' which specifies that the variable is static and will retain values between calls of the procedure that contains it. This is similar to e.g. 'static' in C.


## ALGOL 68

There are no scope modifiers in Algol 68, however the standard prelude contains declarations of structures (such as FILE and SEMA) whose fields are inaccessible to the programmer. This is notionally achieved by the hidden field names starting with the letter aleph which is different from any letter the programmer could use in the source.


## ALGOL W

There are no scope modifiers in Algol W.


## AutoHotkey

{{AutoHotkey case}}

```AutoHotkey
singleton = "global variable"

assume_global()
{
  Global  ; assume all variables declared in this function are global in scope
  Static callcount := 0   ; except this one declared static, initialized once only
  MsgBox % singleton  ; usefull to initialize a bunch of singletons
  callcount++
}

assume_global2()
{
  Local var1  ; assume global except for var1  (similar to global scope declaration)
  MsgBox % singleton
}

object(member, value = 0, null = 0)
{
  Static  ; assume all variables in this function to be static
  If value    ; can be used to simulate objects
	_%member% := value
  Else If null
	_%member% := ""
  Return (_%member%)
}
```



## Axe

Axe has no variable scope modifiers because all variables are static in memory.


## BASIC

=
## Applesoft BASIC
=
All variables are global by default, except the parameter which is local to the function.  There are no scope modifiers.

```ApplesoftBasic
 10 X = 1
 20  DEF  FN F(X) = X
 30  DEF  FN G(N) = X
 40  PRINT  FN F(2)
 50  PRINT  FN G(3)
```

{{out}}

```txt
2
1

```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}
All variables are global by default, except formal parameters which are local to the function.

The scope modifier LOCAL declares a variable local to a function; it sets the value to zero/NULL.

The scope modifier PRIVATE declares a variable static to a function; it sets the value to zero/NULL initially.

```bbcbasic
      var1$ = "Global1"
      var2$ = "Global2"

      PRINT "Before function call:"
      PRINT "var1$ = """ var1$ """"
      PRINT "var2$ = """ var2$ """"

      PROCtestscope(var1$)
      PROCtestscope(var1$)

      PRINT "After function call:"
      PRINT "var1$ = """ var1$ """"
      PRINT "var2$ = """ var2$ """"
      END

      DEF PROCtestscope(var2$)
      PRINT "On entry to function:"
      PRINT "var1$ = """ var1$ """"
      PRINT "var2$ = """ var2$ """"

      LOCAL var1$
      PRIVATE var2$
      PRINT "After LOCAL/PRIVATE:"
      PRINT "var1$ = """ var1$ """"
      PRINT "var2$ = """ var2$ """"

      var1$ = "Local"
      var2$ = "Private"
      PRINT "After assignments:"
      PRINT "var1$ = """ var1$ """"
      PRINT "var2$ = """ var2$ """"

      ENDPROC
```

{{out}}

```txt

Before function call:
var1$ = "Global1"
var2$ = "Global2"
On entry to function:
var1$ = "Global1"
var2$ = "Global1"
After LOCAL/PRIVATE:
var1$ = ""
var2$ = ""
After assignments:
var1$ = "Local"
var2$ = "Private"
On entry to function:
var1$ = "Global1"
var2$ = "Global1"
After LOCAL/PRIVATE:
var1$ = ""
var2$ = "Private"
After assignments:
var1$ = "Local"
var2$ = "Private"
After function call:
var1$ = "Global1"
var2$ = "Global2"

```



## bc

All identifiers are global by default (except function parameters which are local to the function). There is one scope modifier: <code>auto</code>. All identifiers following the <code>auto</code> statement are local to the function and it must be the first statement inside the function body if it is used. Furthermore there can only be one <code>auto</code> per function.

One can think of each identifier as a stack. Function parameters and local identifiers are pushed onto the stack and shadow the values of identifiers with the same names from outer scopes. They are popped from the stack when the function returns. Thus a function that is called from another function has access to the local identifiers and parameters of its caller if itself doesn't use the same name as a local identifier/parameter. In other words, always the innermost value (the value at the top of the stack) for each identifier is visible, regardless of the scope level where it is accessed.


```bc
define g(a) {
    auto b

    b = 3

    "Inside g: a = "; a
    "Inside g: b = "; b
    "Inside g: c = "; c
    "Inside g: d = "; d

    a = 3; b = 3; c = 3; d = 3
}

define f(a) {
    auto b, c

    b = 2; c = 2
    "Inside f (before call): a = "; a
    "Inside f (before call): b = "; b
    "Inside f (before call): c = "; c
    "Inside f (before call): d = "; d
    x = g(2)    /* Assignment prevents output of the return value */
    "Inside f (after call): a = "; a
    "Inside f (after call): b = "; b
    "Inside f (after call): c = "; c
    "Inside f (after call): d = "; d

    a = 2; b = 2; c = 2; d = 2
}

a = 1; b = 1; c = 1; d = 1
"Global scope (before call): a = "; a
"Global scope (before call): b = "; b
"Global scope (before call): c = "; c
"Global scope (before call): d = "; d
x = f(1)
"Global scope (before call): a = "; a
"Global scope (before call): b = "; b
"Global scope (before call): c = "; c
"Global scope (before call): d = "; d
```


{{Out}}

```txt
Global scope (before call): a = 1
Global scope (before call): b = 1
Global scope (before call): c = 1
Global scope (before call): d = 1
Inside f (before call): a = 1
Inside f (before call): b = 2
Inside f (before call): c = 2
Inside f (before call): d = 1
Inside g: a = 2
Inside g: b = 3
Inside g: c = 2
Inside g: d = 1
Inside f (after call): a = 1
Inside f (after call): b = 2
Inside f (after call): c = 3
Inside f (after call): d = 3
Global scope (before call): a = 1
Global scope (before call): b = 1
Global scope (before call): c = 1
Global scope (before call): d = 2
```



## Bracmat

Undeclared variables have always global scope and declared variables have always dynamic scope. Also the function argument (always called "arg" and never explicitly declared) has always dynamic scope. The Bracmat program contained in file "lex.bra" (see Bracmat on GitHub) analyses another Bracmat program to find the places where variables are not in lexical scope. Following the suggestions to declare such variables (and to remove declared, but unused variables) will improve the readability of the analysed code.


```bracmat
  67:?x           {x has global scope}
& 77:?y           { y has global scope }
& ( double
  =
    .  !y+!y      { y refers to the variable declared in myFunc, which
                    shadows the global variable with the same name }
  )
& ( myFunc
  =   y,z         { y and z have dynamic scope. z is never used. }
    .   !arg:?y   { arg is dynamically scoped }
      & double$
      & !x+!y
  )
```


"Variables" in lambda expressions have lexical scope. But can of course not be varied.


```bracmat
/(x./(y.x$+y$))   { x and y have lexical scope }
```



## C

The only scope modifier in C is <tt>static</tt>. The keyword <tt>static</tt> can make a global variable local to the file where it is declared (it has ''file scope''); but it has a different meaning used inside functions or blocks. The <tt>extern</tt> keyword allows to access a "global" variable defined somewhere else.

'''file1.c'''

```c
int a;          // a is global
static int p;   // p is "locale" and can be seen only from file1.c

extern float v; // a global declared somewhere else

// a "global" function
int code(int arg)
{
  int myp;        // 1) this can be seen only from inside code
                  // 2) In recursive code this variable will be in a
                  //    different stack frame (like a closure)
  static int myc; // 3) still a variable that can be seen only from
                  //    inside code, but its value will be kept
                  //    among different code calls
                  // 4) In recursive code this variable will be the
                  //    same in every stack frame - a significant scoping difference
}

// a "local" function; can be seen only inside file1.c
static void code2(void)
{
  v = v * 1.02;    // update global v
  // ...
}
```


'''file2.c'''

```c
float v;         // a global to be used from file1.c too
static int p;    // a file-scoped p; nothing to share with static p
                 // in file1.c

int code(int);   // this is enough to be able to use global code defined in file1.c
                 // normally these things go into a header.h

// ...
```



## C#


```c#
public //visible to anything.
protected //visible to current class and to derived classes.
internal //visible to anything inside the same assembly (.dll/.exe).
protected internal //visible to anything inside the same assembly and also to derived classes outside the assembly.
private //visible only to the current class.
//C# 7.2 adds:
private protected //visible to current class and to derived classes inside the same assembly.

//                   |       |     subclass     |    other class   ||     subclass     |    other class
//Modifier           | class | in same assembly | in same assembly || outside assembly | outside assembly
//-------------------------------------------------------------------------------------------------------
//public             |  Yes  |        Yes       |        Yes       ||        Yes       |       Yes
//protected internal |  Yes  |        Yes       |        Yes       ||        Yes       |       No
//protected          |  Yes  |        Yes       |        No        ||        Yes       |       No
//internal           |  Yes  |        Yes       |        Yes       ||        No        |       No
//private            |  Yes  |        No        |        No        ||        No        |       No
// C# 7.2:
//private protected  |  Yes  |        Yes       |        No        ||        No        |       No
```

If no modifier is specified, it defaults to the most restrictive one.<br/>
In case of top-level classes/structs/interfaces/enums this means internal, otherwise it means private.

Special case: explicit interface implementation.<br/>
When a class explicitly implements an interface method, it is 'hidden' and that method can only be accessed through the interface:

```c#
public interface IPrinter
{
    void Print();
}

public class IntPrinter : IPrinter
{
    void IPrinter.Print() { // explicit implementation
        Console.WriteLine(123);
    }

    public static void Main() {
        //
### =Error=

        IntPrinter p = new IntPrinter();
        p.Print();

        //
### =Valid=

        IPrinter p = new IntPrinter();
        p.Print();
    }
}

```

Other declarations follow lexical scoping.<br/>
Visibility is determined by the enclosing braces { }<br/>


## COBOL

The EXTERNAL clause specifies that a data item or a file connector is external. The constituent data items and
group data items of an external data record are available in a run unit to every runtime element that describes the
record as external.

The GLOBAL clause specifies that a constant-name, a data-name, a file-name, a report-name, or a screen-name is
a global name. A global name is available to every program contained within the program that declares it.

The COMMON clause specifies that the program is common. A common program is contained within another
program but may be called from programs other than that containing it.


## Common Lisp

Common Lisp has exactly one scope modifier, the <code>special</code> declaration, which causes occurrences of a variable within the scope of the declaration to have dynamic scope ("special variables") rather than lexical scope.

The defining operators <code>defvar</code> and <code>defparameter</code> globally declare a variable special, though this can also be done using <code>declaim</code>. Local special declarations are rarely used.

The next example declaims that <code>*bug*</code> has dynamic scope. Meanwhile, <code>shape</code> has lexical scope.


```lisp
;; *bug* shall have a dynamic binding.
(declaim (special *bug*))

(let ((shape "triangle") (*bug* "ant"))
  (flet ((speak ()
           (format t "~%  There is some ~A in my ~A!" *bug* shape)))
    (format t "~%Put ~A in your ~A..." *bug* shape)
    (speak)

    (let ((shape "circle") (*bug* "cockroach"))
      (format t "~%Put ~A in your ~A..." *bug* shape)
      (speak))))
```


The function <code>speak</code> tries to use both <code>*bug*</code> and <code>shape</code>. For lexical scope, the value comes from where the program ''defines'' <code>speak</code>. For dynamic scope, the value comes from where the program ''calls'' <code>speak</code>. So <code>speak</code> always uses the same "triangle", but can use a different bug.


```txt
Put ant in your triangle...
  There is some ant in my triangle!
Put cockroach in your circle...
  There is some cockroach in my triangle!
```


The stars around <code>*bug*</code> are not a special syntax. Rather, they are part of the symbol's name. This widely-used convention effectively places dynamic variables into their own namespace, which is necessary for preventing bugs. Common Lisp itself follows this tradition in its standard dynamic variables like <code>*print-circle*</code>, <code>*readtable*</code> et cetera.


## Delphi


```Delphi>private</lang

Can only be seen inside declared class.


```Delphi>protected</lang

Can be seen in descendent classes.


```Delphi>public</lang

Can be seen from outside the class.


```Delphi>protected</lang

Same visibility as Public, but run time type information (RTTI) is generated, allowing these members to be viewed dynamically.  Members need to be published in order to be streamed or shown in the Object Inspector.


```Delphi>automated</lang

Same visibility as Public, and used for Automation Objects.  This is currently only maintained for backward compatibility.


```Delphi
strict private
strict protected
```

Private and Protected members of a class are visible to other classes declared in the same unit.  The "strict" modifier was added in Delphi 2005 to treat public and private members as private and protected, even from classes declared in the same unit.

=={{header|Déjà Vu}}==
Variables are lexically scoped in Déjà Vu. Doing a <code>set</code> or a <code>get</code> starts looking for <code>local</code> declarations in the current scope, going upward until the global scope. One can use <code>setglobal</code> and <code>getlocal</code> to bypass this process, and only look at the global scope.

```dejavu
set :a "global"
if true:
    !print a
    local :a "local"
    !print a
    !print getglobal :a
!print a

```

{{out}}

```txt
global
local
global
global
```



## E

E has no scope modifiers; all variables (including function definitions) are lexical. When more than one file is involved, all import/export of definitions is handled by explicit return values, parameters, or reified environments.


## Eiffel

Routines (i.e. functions and procedures) always have global scope.

Variables have global scope except for:
* routine parameters
* variables declared local to a routine
* the 'Result' of a function

A local variable cannot hide (have the same name as):
* any routine
* a global variable


```Eiffel
feature
	some_procedure(int: INTEGER; char: CHARACTER)
		local
			r: REAL
			i: INTEGER
		do
			-- r, i and s have scope here
			-- as well as int and char
			-- some_procedure and some_function additionally have scope here
		end

	s: STRING

	some_function(int: INTEGER): INTEGER
		do
			-- s and Result have scope here
			-- as well as int (int here differs from the int of some_procedure)
			-- some_procedure and some_function additionally have scope here
		end

	-- s, some_procedure and some_function have scope here
```



## Ela

Variables in Ela are lexically scoped (pretty similar to Haskell) and can be declared using let/in and where bindings. Additionally Ela provides a 'private' scope modifier for global bindings:


```ela
pi # private
pi = 3.14159

sum # private
sum x y = x + y
```


Names declared with 'private' modifier are not visible outside of a module. All other bindings are visible and can be imported. It is an error to use 'private' modifier on local bindings.


## Erlang

Erlang is lexically scoped. Variables, which must begin with an upper case letter, are only available inside their functions. Functions are only available inside their modules. Unless they are exported.

```Erlang

-module( a_module ).

-export( [double/1] ).

double( N ) -> add( N, N ).



add( N, N ) -> N + N.

```


{{out}}

```txt

3> a_module:double( 3 ).
6
4> a_module:add( 3, 3 ).
** exception error: undefined function a_module:add/2

```



## Free Pascal

''See  [[#Pascal|Pascal]]''


## Go

Go is lexically scoped and has just one scope modification feature, exported identifiers.  Identifiers&mdash;variables and field names&mdash;are not visible outside of the package in which they are defined unless they begin with an upper case letter, as defined by Unicode class "Lu".


## Haskell

Haskell has no scope modifiers; all variables are lexically scoped.

{|
! Site of declaration
! Scope
|-
|Top level
|The current module
|-
|<code>where</code> construct
|The definition to which the <code>where</code> is attached
|-
|<code>let</code> or <code>case</code> expression or lambda
|The entire expression
|-
|<code><-</code> or <code>let</code> in a <code>do</code> block
|All statements in the <code>do</code> block after the declaration
|}

=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon data types are not declared and variables can take on any value; however, variables can be declared as to their scope.  For more see [[Icon%2BUnicon/Intro#un-Declarations.2C_it.27s_all_about_Scope|un-Declarations it's all about scope]].  Additionally, Unicon supports classes with methods.

```Icon
global var1    # used outside of procedures

procedure one() # a global procedure (the only kind)
local  var2    # used inside of procedures
static var3    # also used inside of procedures
end
```


Co-expressions (both languages) also redefine scope - any local variables referenced
within the body of a co-expression are restricted in scope to that body, but are
initialized to the values they had with the co-expression is created.


## J

J's scoping rules are dynamic scope, limited to behave as lexical scope.

First approximation:  All variables are either "global" in scope, or are local to the currently executing explicit definition.  Local names shadow global names.  J provides kinds of assignment -- assignment to a local name (<tt>=.</tt>) and assignment to a global name (<tt>=:</tt>).  Shadowed global names ("global" names which have the same name as a name that has a local definition) can not be assigned to (because this is typically a programming mistake and can be easily avoided by performing the assignment in a different execution context).  Here's an interactive session:


```J
   A=: 1
   B=: 2
   C=: 3
   F=: verb define
      A=:4
      B=.5
      D=.6
      A+B+C+D
   )
   F ''
18
   A
4
   B
2
   D
|value error
```


Second approximation: J does not really have a global namespace.  Instead, each object and each class has its own namespace.  By default, interactive use updates the namespace for the class named 'base'.  Further discussion of this issue is beyond the scope of this page.


## Java


```java
public //any class may access this member directly

protected //only this class, subclasses of this class,
//and classes in the same package may access this member directly

private //only this class may access this member directly

static //for use with other modifiers
//limits this member to one reference for the entire JVM

//adding no modifier (sometimes called "friendly") allows access to the member by classes in the same package

// Modifier    | Class | Package | Subclass | World
// ------------|-------|---------|----------|-------
// public      |  Y    |    Y    |    Y     |   Y
// protected   |  Y    |    Y    |    Y     |   N
// no modifier |  Y    |    Y    |    N     |   N
// private     |  Y    |    N    |    N     |   N

//method parameters are available inside the entire method

//Other declarations follow lexical scoping,
//being in the scope of the innermost set of braces ({}) to them.
//You may also create local scopes by surrounding blocks of code with braces.

public void function(int x){
   //can use x here
   int y;
   //can use x and y here
   {
      int z;
      //can use x, y, and z here
   }
   //can use x and y here, but NOT z
}
```



## JavaScript

There are not precisely any scope ''modifiers'' in JavaScript.

The <code>var</code> variable declaration makes a variable local to a function, and function parameters are also local. Any variable not so declared is global, except in ES5 strict mode where an undeclared variable is an error.

A named function definition (<code>function foo() { ... }</code>) is “hoisted” to the top of the enclosing function; it is therefore possible to call a function before its definition would seem to be executed.


## Julia

<code>global x </code>makes x in the current scope and its inner scopes refer to the <code>global</code> variable of that name.
<br /><br />
<code> local x </code>introduces a new local variable x.
<br /><br />

```julia

 julia> function foo(n)
             x = 0
             for i = 1:n
                 local x # introduce a loop-local x
                 x = i
             end
             x
         end
  foo (generic function with 1 method)

 julia> foo(10)
  0

```

Julia also has scopes based on modules. Variables within a standard module such as <code>MyModule; x = 0; end</code>need to be referred to with the module name prefix, such as <code>MyModule.x</code>, unless the variable is exported from the module with the <code> export</code> keyword.


## Kotlin

As Kotlin supports both procedural and object oriented programming, the usage of scope modifiers is more complicated than in some other languages and is described in the online language reference at https://kotlinlang.org/docs/reference/visibility-modifiers.html.

Note in particular the following differences between Java and Kotlin:

1. Kotlin does not have 'package private' visibility and, if no modifier is used, the default is 'public' (or 'protected' when overriding a protected member).

2. Kotlin has the 'internal' modifier which means that the entity is visible everywhere within the same 'module'. A module, for this purpose, is essentially a set of source code files which are compiled together.

3. In Kotlin private members of an inner class are not accessible by code within an outer class.

4. Kotlin does not have static members as such but instead has 'companion objects' whose members can be accessed using the class name, rather than a reference to a particular object of that class. The following is a simple example of their use:

```scala
// version 1.1.2

class SomeClass {
    val id: Int

    companion object {
        private var lastId = 0
        val objectsCreated get() = lastId
    }

    init {
        id = ++lastId
    }
}

fun main(args: Array<String>) {
    val sc1 = SomeClass()
    val sc2 = SomeClass()
    println(sc1.id)
    println(sc2.id)
    println(SomeClass.objectsCreated)
}
```


{{out}}

```txt

1
2
2

```



## Liberty BASIC

Functions, subroutines and variables are not declared before use.

Single-dimensioned arrays with 0-10 elements do not need to be declared or dimensioned before use.

Single-dimensioned arrays with indices greater than 10 and double-dimensioned arrays must be dimensioned before use.

There are two types of variables: string and numeric. Variables are visible in the scope in which they appear. unless they are declared GLOBAL.

Global variables are visible in all scopes.

Local variables may be passed ByRef and then become visible inside subs and functions.

Some entitities are global by default. These include arrays, structs, handles and special variables such as DefaultDir$, WindowWidth, and ForegroundColor$


## Logo

Traditional Logo has dynamic scope for all symbols except for parameters, ostensibly so that it is easy to inspect bound values in an educational setting.  UCB Logo also has a LOCAL syntax for declaring a dynamically scoped variable visible to a procedure and those procedures it calls.

```logo

make "g 5    ; global

to proc :p
  make "h 4    ; also global
  local "l       ; local, no initial value
  localmake "m 3

  sub 7
end

to sub :s
  ; can see :g, :h, and :s
  ; if called from proc, can also see :l and :m
  localmake "h 5     ; hides global :h within this procedure and those it calls
end

```



## Logtalk

Logtalk supports scope modifiers in predicate declarations and entity (object, category, or protocol) relations. By default, predicates are local (i.e. like private but invisible to the reflection mechanisms) and entity relations are public (i.e. not change to inherited predicate declarations is applied).

```logtalk

:- public(foo/1).     % predicate can be called from anywhere

:- protected(bar/2).  % predicate can be called from the declaring entity and its descendants

:- private(baz/3).    % predicate can only be called from the declaring entity

:- object(object,     % predicates declared in the protocol become private for the object
    implements(private::protocol)).

:- category(object,   % predicates declared in the protocol become protected for the category
    implements(protected::protocol)).

:- protocol(extended, % no change to the scope of the predicates inherited from the extended protocol
    extends(public::minimal)).

```



## M2000 Interpreter

We can use Global, Local to shadow any same variable and give scope to new one.

We can use Static for simple variables, so we can find them in the next call.

By default every new variable/module/function/group is local if defined in a module or a function

In command line, in M2000 console every variable/module/function/group is global

Local variables are prefered by interpreter at reading/writing

We have to use <= to assign new values to global variables, or to member of groups inside a module inside a group. See ResetValues in Group Alfa.


```M2000 Interpreter

Module Checkit {
      M=1000
      Function Global xz {
             =9999
      }
      Module TopModule {
            \\ clear vars and static vars
            Clear
            M=500
            Function Global xz {
                  =10000
            }
            Module Kappa {
                  Static N=1
                  Global M=1234
                  x=1
                  z=1
                  k=1
                  Group Alfa {
                  Private:
                        x=10, z=20, m=100
                        Function xz {
                              =.x*.z+M
                        }
                  Public:
                        k=50
                        Module AddOne {
                              .x++
                              .z++
                              .k++
                              Print .xz(), .m=100
                        }
                        Module ResetValues {
                              \\ use <= to change members, else using = we define local variables
                              .x<=10
                              .z<=20
                        }
                  }
                  ' print 1465
                  Alfa.AddOne
                  Print x=1, z=1, k=1, xz()=10000
                  Print N  ' 1 first time, 2 second time
                  N++
                  Push Alfa
            }
            Kappa
            Drop  ' drop one alfa
            Kappa
            Print M=500
            ' leave one alfa in stack of values
      }
      TopModule
      Read AlfaNew
      Try ok {
            AlfaNew.AddOne
      }
      \\ we get an error because M global not exist now
      \\ here M is Local.
      If Error or Not Ok Then Print Error$ ' Uknown M in .xz() in AlfaNew.AddOne
      Print M=1000, xz()=9999
      For AlfaNew {
            Global M=1234
            .ResetValues
            .AddOne  ' now works because M exist as global, for this block
      }
      Print M=1000, xz()=9999
      For This {
            Local M=50
            M++
            Print M=51
      }
      Print M=1000
}
Checkit
List  ' list of variables are empty
Modules ? ' list of modules show two: A and A.Checkit
Print Module$  ' print A

```

Subs are searched first time for current module/function, or from parent code, and stored in a list as name, internal number of code source and position in code. Modules can replaced (we sy decorated) with other modules, before call (see CheckThis changed for a call with ChangeOther).

Internal M2000 Interpreter uses "Execution Objects", named Basetasks, which hold code for consuming. Modules and functions run on own Basetasks, but subs use the current basetask. There are routines using Gosub and Return like Basic.

Threads are part of modules/functions. They have own stack of values. own static variables, but they see everything like code in module:


```M2000 Interpreter

Module CheckIt {
      Module CheckSub {
            Read Z
            M=5000
            Module CheckThis {
                  Z=500
                  Hello("Bob")
            }
            Function CheckFun {
                   Z=50
                   Hello("Mary")
            }
            Call CheckFun()
            CheckThis
            Hello("George")
            Gosub label1
            \\ sub work as exit here
            Sub Hello(a$)
                  \\ any new definition erased at exit of sub
                  Local M=100
                  Print "Hello ";a$, Z, M
            End Sub
      label1:
            \\ this light subs have no "erased new definition mode"
            \\ they are like code of module
            Print Z, M
            Return
      }
      CheckSub 10
      Module CheckOther {
            Z=1000
            Hello("John")
      }
      \\ we can replace CheckThis with CheckOther
      CheckSub 20; CheckThis as CheckOther
}
Call Checkit
Module Alfa {
      x=1
      Thread {
            x++
      } as K interval 20
      Thread {
            PrintMe()
      } as J interval 20
      Main.Task 20 {
            if x>99 then exit
      }
      Wait 100
      Sub PrintMe()
            Print x
      End Sub
}
Call Alfa

```



## Mathematica


```Mathematica
Module -> localize names of variables (lexical scoping)
Block  -> localize values of variables (dynamic scoping)

Module creates new symbols:

Module[{x}, Print[x];
 Module[{x}, Print[x]]
]

->x$119
->x$120

Block localizes values only; it does not create new symbols:

x = 7;
Block[{x=0}, Print[x]]
Print[x]
->0
->7
```



## MUMPS

MUMPS variable can be in a local scope if they are declared as NEW within a subroutine. Otherwise variables are accessible to all levels.

```MUMPS
OUTER
 SET OUT=1,IN=0
 WRITE "OUT = ",OUT,!
 WRITE "IN = ",IN,!
 DO INNER
 WRITE:$DATA(OUT)=0 "OUT was destroyed",!
 QUIT
INNER
 WRITE "OUT (inner scope) = ",OUT,!
 WRITE "IN (outer scope) = ",IN,!
 NEW IN
 SET IN=3.14
 WRITE "IN (inner scope) = ",IN,!
 KILL OUT
 QUIT
```

Execution:
```txt

USER>D ^SCOPE
OUT = 1
IN = 0
OUT (inner scope) = 1
IN (outer scope) = 0
IN (inner scope) = 3.14
OUT was destroyed
```



## Nim

Identifiers annotated with a <code>*</code> are accessible from other modules

```nim
proc foo = echo "foo" # hidden
proc bar* = echo "bar" # acessible

type MyObject = object
  name*: string # accessible
  secretAge: int # hidden
```



## PARI/GP

The modifiers are <code>local</code> and, for recent versions of Pari, <code>my</code>.  See the User's Guide to PARI/GP.
<!-- needs expansion and clarification -->


## Pascal

Pascal does not have scope modifiers.
Regular block scopes are defined simply by virtue of the declaration’s position:

```pascal
procedure super;
var
	f: boolean;

	procedure nestedProcedure;
	var
		c: char;
	begin
		// here, `f`, `c`, `nestedProcedure` and `super` are available
	end;
	procedure commonTask;
	var
		f: boolean;
	begin
		// here, `super`, `commonTask` and _only_ the _local_ `f` is available
	end;
var
	c: char;

	procedure fooBar;
	begin
		// here, `super`, `fooBar`, `f` and `c` are available
	end;
var
	x: integer;
begin
	// here, `c`, `f`, and `x`, as well as,
	// `nestedProcedure`, `commonTask` and `fooBar` are available
end;
```



## Perl

A name explicitly qualified as belonging to a package with <code>::</code> (like <code>$Foo::bar</code>; as a special case, for any identifier <code>var</code> and sigil <code>$</code>, <code>$::var</code> is short for <code>$main::var</code>) always refers to a package variable, i.e., a global variable belonging to the given package.  So only unqualified names can have context-sensitive interpretations.

By default, an unqualified name refers to a package variable in the current package. The current package is whatever you set it to with the last <code>package</code> declaration in the current lexical scope, or <code>main</code> by default. But wherever stricture is in effect, using a name that would be resolved this way is a compile-time error.

There are four kinds of declaration that can influence the scoping of a particular variable: <code>our</code>, <code>my</code>, <code>state</code>, and <code>local</code>. <code>our</code> makes a package variable lexically available. Its primary use is to allow easy access to package variables under stricture.


```perl
use strict;
$x = 1;                   # Compilation error.
our $y = 2;
print "$y\n";             # Legal; refers to $main::y.

package Foo;
our $z = 3;
package Bar;
print "$z\n";             # Refers to $Foo::z.
```


<code>my</code> creates a new lexical variable, independent of any package. It's destroyed as soon as it falls out of scope, and each execution of the statement containing the <code>my</code> creates a new, independent variable.


```perl
package Foo;
my $fruit = 'apple';
package Bar;
print "$fruit\n";         # Prints "apple".
{
    my $fruit = 'banana';
    print "$fruit\n";     # Prints "banana".
}
print "$fruit\n";         # Prints "apple".
                          # The second $fruit has been destroyed.
our $fruit = 'orange';
print "$fruit\n";         # Prints "orange"; refers to $Bar::fruit.
                          # The first $fruit is inaccessible.
```


<code>state</code> is like <code>my</code> but creates a variable only once. The variable's value is remembered between visits to the enclosing scope. The <code>state</code> feature is only available in perl 5.9.4 and later, and must be activated with <code>use feature 'state';</code> or a <code>use</code> demanding a sufficiently recent perl.


```perl
use 5.10.0;

sub count_up
{
    state $foo = 13;
    say $foo++;
}

count_up;                 # Prints "13".
count_up;                 # Prints "14".
```


<code>local</code> gives a package variable a new value for the duration of the current ''dynamic'' scope.


```perl
our $camelid = 'llama';

sub phooey
{
    print "$camelid\n";
}

phooey;                   # Prints "llama".

sub do_phooey
{
    local $camelid = 'alpaca';
    phooey;
}

do_phooey;                # Prints "alpaca".
phooey;                   # Prints "llama".
```


Usually, <code>my</code> is preferable to <code>local</code>, but one thing <code>local</code> can do that <code>my</code> can't is affect the special punctuation variables, like <code>$/</code> and <code>$"</code>. Actually, in perl 5.9.1 and later, <code>my $_</code> is specially allowed and works as you would expect.


## Perl 6

Perl 6 has a system of declarators that introduce new names into various scopes.

```perl6
my $lexical-variable;
our $package-variable;
state $persistent-lexical;
has $.public-attribute;
```

Lexically scoped variables, declared with <tt>my</tt>, are the norm.
Function definitions are intrinsically lexical by default, but allow for forward references, unlike any other declaration.

Package variables, declared with <tt>our</tt>, are de-emphasized.  Unlike in Perl 5, almost no built-ins use package declarations for anything other than type names, and most of Perl 5's global punctuational variables become dynamic variables instead, with the final recourse in the GLOBAL and PROCESS packages.  The per-interpreter GLOBAL package is mainly for users; all predefined process-wide information is stored in the PROCESS symbol table instead.  The <tt>our</tt> declarator actually just declares an alias to a variable of the same name in the current package, and is in a sense just permission to use that global variable in the current scope as if it were a lexical.  Type and constant declarations, including enums, are intrinsically considered "our" declarations, since Perl 6 considers constants to be degenerate types.

State variables, declared with <tt>state</tt>, are persistent lexicals that are not re-initialized on each function entry, but retain their previous value.  An initializer is run only the first time through.  State variables are similar but not identical to C static variables; in Perl each closure clone gets its own state variable, since such closures are really a form of generic code.

The <tt>has</tt> declarator is for declaring items in object scope.
Method declarations are implicitly in "has" scope.

In Perl 5, dynamic scoping is done via "local" to temporarily change the value of a global variable.  This mechanism is still specced for Perl 6, albeit with a different keyword, <tt>temp</tt>, that better reflects what it's doing.  None of the implementations yet implement <tt>temp</tt>, since Perl 6 does dynamic scoping via a more robust system of scanning up the call stack for the innermost dynamic declaration, which actually lives in the lexical scope of the function declaring it.  We distinguish dynamic variables syntactically by introducing a "twigil" after the sigil.  The twigil for dynamic variables is <tt>*</tt> to represent that we don't know how to qualify the location of the variable.

```perl6
sub a {
    my $*dyn = 'a';
    c();
}
sub b {
    my $*dyn = 'b';
    c();
}
sub c {
    say $*dyn;
}
a();  # says a
b();  # says b
```

The standard IO filehandles are dynamic variables $*IN, $*OUT, and $*ERR, which allows a program to easily redirect the input or output from any subroutine and all its children.  More generally, since most process-wide variables are accessed via this mechanism, and only look in the PROCESS package as a last resort, any chunk of code can pretend to be in a different kind of process environment merely by redefining one or more of the dynamic variables in question, such as %*ENV.

This mechanism automatically produces thread-local storage if you declare your dynamic variable inside the lexical scope of the thread.


## Phix

Identifiers are private (restricted to a single file) by default, or they can be made global by prefixing the
definition with the global keyword (outside of routines only - everything declared inside a routine is always private to that routine only). Should a forward declaraion of a routine exist, it must match the actual definition in terms of presence/absence of a global prefix (as well as parameter types etc), eg

```Phix
forward function localf()    -- not normally necesssary, but will not harm
forward global function globalf()   -- ""

function localf()
    return 1
end function

global function globalf()
    return 2
end function
```

Here, localf() can only be invoked from within the same file, but globalf() can be invoked from any other file
that (directly or indirectly) includes it. The global keyword is equally applicable to routines, variables, and constants.
A forward definition may be required before any named parameters can be used (on forward calls), and also occasionally to
cure a "globalf has not been declared" variety of compilation error.

Namespaces for specific (entire) files can be used to qualify global identifiers, should there be a name clash between several files.

```Phix
include somefile.e as xxx
-- alternatively, within somefile.e:
namespace xxx           -- (only supported in Phix for compatibility with OpenEuphoria)

res = xxx:globalf()     -- call a global function named globalf, specifically the one declared in somefile.e
```

Note however that one of the main reasons for namespaces is to avoid having to amend any included (3rd party)
files, so having namespaces within the file itself may prove to be less than helpful, should they (the namespaces
themselves) ever clash.

Also note that namespaces are always local, not that there is any problem at all with the same name dropping
in and out of scope in every single source file throughout the application. What this means is that if file
a includes b includes c, you can refer in a to c via the namespace of b, but not directly, unless you also
explicitly include c in a. (Obviously were something in c globally unique you could refer to it without any namespace.)

The compiler maintains a list of files it has processed; re-inclusion (by some other file) just adds any
required namespace and otherwise quietly skips it.


## PicoLisp

PicoLisp distinguishes between "scope" and "binding".
The scope of a symbol determines its visibility in a given context
(whether or not it can be accessed), while binding is about assigning it a value.


### Scope

In PicoLisp, the scope type of a symbol is either "internal", "transient" or
"external".
It is specified lexically: Internal symbols are just normal symbols.
Transient symbols are surrounded by double quotes (and thus look like strings in
other languages), and/or with an underlined font if possible.
External symbols are surrounded by braces.

* The scope of an internal symbol is global. This means that a symbol like AB123 is always the same object, residing at a certain location in memory (pointer equality).

* A transient symbol like "AB123" is the same only within the current transient scope. This is normally a single source file, but may be further subdivided.  Within that scope it can be used like an internal symbol, but after the transient scope is closed it cannot be accessed by its name any longer. This behavior is similar to "static" identifiers in the C language.

* External symbols like {AB123} are persistent database symbols. They have a permanent identity among different processes and over time. Besides that, they have the same structure like internal and transient symbols: A value, properties and a name.


### Binding

Regardless of the scope, the binding of symbols to values is always dynamic.
This happens implicitly for function parameters, or explicitly with functions
like [http://software-lab.de/doc/refL.html#let let],
[http://software-lab.de/doc/refU.html#use use],
[http://software-lab.de/doc/refB.html#bind bind],
[http://software-lab.de/doc/refJ.html#job job] and others.
This means that the current value of a symbol is saved locally, then set to the
new value. When done, the old value is restored. Closures are created by
maintaining an explicit environment. More about that
[http://software-lab.de/doc/faq.html#dynamic here].


## PowerShell

Variables can have a specific scope, which is one of '''global''', '''local''', '''script''', '''private'''. Variables with the same name can exist in different scopes and are shadowed by child scopes. The scope of a variable can be directly prefixed to the variable name:

```powershell
$a = "foo"                        # global scope
function test {
    $a = "bar"                    # local scope
    Write-Host Local: $a          # "bar" - local variable
    Write-Host Global: $global:a  # "foo" - global variable
}
```

The various cmdlets dealing with variables also have a '''–Scope''' parameter, enabling one to specify a relative or absolute scope for the variable to be manipulated.


## PureBasic

* Functions must be defined before being used and are always global in scope.

* Variables must be defined before being used.  They do not have to be explicity defined, simply using them will define them.  The keyword <tt>EnableExplicit</tt> may also be used to require explicit definitions before using a variable.

* Two main divisions in scope exist.  The first scope is the body of code outside of all procedures and the second is the scope within a single given procedure.

* If a variable is not explicitly defined its scope is local to one of the aforementioned areas.  This may be modified by using one of the keywords: <tt>Global</tt>, <tt>Protected</tt>, or <tt>Shared</tt>.  The effects are detailed by the comments in the sample code.

```PureBasic
;define a local integer variable by simply using it
baseAge.i = 10
;explicitly define local strings
Define person.s = "Amy", friend.s = "Susan"
;define variables that are both accessible inside and outside procedures
Global ageDiff = 3
Global extraYears = 5


Procedure test()
  ;define a local integer variable by simply using it
  baseAge.i = 30
  ;explicitly define a local string
  Define person.s = "Bob"
  ;allow access to a local variable in the main body of code
  Shared friend
  ;create a local variable distinct from a variable with global scope having the same name
  Protected extraYears = 2

  PrintN(person + " and " + friend + " are " + Str(baseAge) + " and " + Str(baseAge + ageDiff + extraYears) + " years old.")
EndProcedure


If OpenConsole()
  test()

  PrintN(person + " and " + friend + " are " + Str(baseAge) + " and " + Str(baseAge + ageDiff + extraYears) + " years old.")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
Bob and Susan are 30 and 35 years old.
Amy and Susan are 10 and 18 years old.
```



## Python

Python from version 3 has the global and nonlocal access modifiers:
* <code>global</code> instructs the interpreter to search for the name(s) in the outermost sccope.
* <code>nonlocal</code> instructs the interpreter to search for the name(s) starting from the innermost enclosing scope going outwards.
Without either keyword, a reference to a name must have the name defined in the current scope or if not, then it is looked for in the global scope - skipping any intermediate scopes.

In the example below the name <code>x</code> is defined at various scopes and given a different value dependent on its scope. The innermost functions demonstrate how the scope modifiers give acccess to the name from different scopes:


```python>>>
 x="From global scope"
>>> def outerfunc():
    x = "From scope at outerfunc"

    def scoped_local():
        x = "scope local"
        return "scoped_local scope gives x = " + x
    print(scoped_local())

    def scoped_nonlocal():
        nonlocal x
        return "scoped_nonlocal scope gives x = " + x
    print(scoped_nonlocal())

    def scoped_global():
        global x
        return "scoped_global scope gives x = " + x
    print(scoped_global())

    def scoped_notdefinedlocally():
        return "scoped_notdefinedlocally scope gives x = " + x
    print(scoped_notdefinedlocally())


>>> outerfunc()
scoped_local scope gives x = scope local
scoped_nonlocal scope gives x = From scope at outerfunc
scoped_global scope gives x = From global scope
scoped_notdefinedlocally scope gives x = From global scope
>>>
```

More information on the scope modifiers can be found [http://docs.python.org/3.0/reference/simple_stmts.html#grammar-token-global_stmt here].


## R

See [http://obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/ "How R Searches and Finds Stuff"] for a thorough introduction to scoping, particularly the surprisingly complicated conventions for packages. For a briefer overview, read on.

In R, functions use lexical scope: a function acquires its parent
scope at the time of definition, and each invocation creates a new
local environment within that parent scope. Variable lookup during
evaluation starts in the function's local environment and proceeds
up the chain of parent environments.


```R
X <- "global x"
f <- function() {
  x <- "local x"
  print(x) #"local x"
}
f()                          #prints "local x"
print(x)                     #prints "global x"
```


attach() will attach an environment or data set to the chain of
enclosing environments.


```R
d <- data.frame(a=c(2,4,6), b = c(5,7,9))
attach(d)
b - a        #success
detach(d)
b - a        #produces error
```


Assignment using <- or -> by default happens in the local
(innermost) environment.  The <<- and ->> operators assign a
variable in the innermost enclosing scope in which that variable is
already defined, or the global environment if no enclosing
definition is found.


```R
x <- "global x"
print(x)                                   #"global x"

local({ ## local({...}) is a shortcut for evalq({...}, envir=new.env())
        ## and is also equivalent to (function() {...})()

  x <- "outer local x"
  print(x)                                 #"outer local x"
  x <<- "modified global x"
  print(x)                                 #"outer local x" still
  y <<- "created global y"
  print(y)                                 #"created global y"
  local({

    ## Note, <<- is _not_ a global assignment operator. If an
    ## enclosing scope defines the variable, that enclosing scope gets
    ## the assignment. This happens in the order of evalution; a local
    ## variable may be defined later on in the same scope.

    x <- "inner local x"
    print(x)                               #"inner local x"
    x <<- "modified outer local x"
    print(x)                               #"inner local x"
    y <<- "modified global y"
    print(y)                               #"modified global y"
    y <- "local y"
    print(y)                               #"local y"

    ##this is the only way to reliably do a global assignment:
    assign("x", "twice modified global x", globalenv())
    print(evalq(x, globalenv()))           #"twice modified global x"
  })

  print(x)                                 #"modified outer local x"
})
print(x)                                   #"twice modified global x"
print(y)                                   #"modified global y"
```


However, the scope and other aspects of evaluation can be
explicitly manipulated at runtime. assign() and eval(), for
instance, allow you to specify where an evaluation or assignment is
to take place. parent.env() returns the lexically enclosing scope,
while parent.frame() returns the immediate scope of the calling
function.


```R
x <- "global x"
f <- function() {
  cat("Lexically enclosed x: ", x,"\n")
  cat("Lexically enclosed x: ", evalq(x, parent.env(sys.frame())),"\n")
  cat("Dynamically enclosed x: ", evalq(x, parent.frame()),"\n")
}

local({
  x <- "local x"
  f()
})
```


A function's arguments are not evaluated until needed; the function
may change the evaluation rules for expressions given to its arguments
by capturing its quoted argument via substitute() and evaluating it in a different
environment. For instance, with() evaluates its second argument in the environment
defined by its first argument, enclosed within the current scope.


```R
d <- data.frame(a=c(2,4,6), b = c(5,7,9))
also <- c(1, 0, 2)
with(d, mean(b - a + also)) #returns 4

## with() is built in, but you might have implemented it like this:

with.impl <- function(env, expr) {
  env <- as.environment(env)
  parent.env(env) <- parent.frame()
  eval(substitute(expr), envir=env)
}
with.impl(d, mean(b - a + also))
```



## Racket

Racket has no concept of scope modifiers. Depending on where an identifier is bound, it may be considered a top-level, module, or local binding. However, the binding is introduced with lexical scope in all cases. Bindings are introduced by syntactic forms such as <tt>lambda</tt>, <tt>let</tt>, or <tt>define</tt>.

However, Racket identifier bindings do exist at particular phase levels (represented by an integer). Phase levels, to a first approximation, allow the separation of computations that occur at compile-time and run-time.


## REXX


### version 1

In the REXX language, all variables are global, and only within PROCEDUREs are variables local (private), except for those identified

via the   '''expose'''   option.   There is a variant where the   '''expose'''   can have a list specified (along with variables and stems).

Any REXX variables in an external routine (program) aren't known.

Note: the R4 REXX interpreter has an   '''exposeall'''   option that allows an external REXX subroutine to access the caller's local variables.

There is a mechanism that allows external programs to access local REXX variables and is essentially restricted to assembler programs

(or other programs) that use the REXXAPI interface.

All labels (names of subroutines/functions/procedures) are global, regardless of procedures.

If more than one identical label is specified, only the first label is recognized (and it isn't considered an error).

```rexx
/*REXX program to display scope modifiers  (for subroutines/functions). */
a=1/4
b=20
c=3
d=5
call SSN_571  d**4

       /* at this point,  A   is    defined and equal to      .25       */
       /* at this point,  B   is    defined and equal to    40          */
       /* at this point,  C   is    defined and equal to    27          */
       /* at this point,  D   is    defined and equal to     5          */
       /* at this point,  FF  isn't defined.                            */
       /* at this point, EWE  is    defined and equal to 'female sheep' */
       /* at this point,  G   is    defined and equal to   625          */
exit                                   /*stick a fork in it, we're done.*/
/*─────────────────────────────────────SSN_571 submarine, er, subroutine*/
SSN_571: procedure expose b c ewe g;  parse arg g
b   = b*2
c   = c**3
ff  = b+c
ewe = 'female sheep'
d   = 55555555
return                     /*compliments to Jules Verne's Captain Nemo? */
```



### version 2 scope is DYNAMIC


```rexx
a=1
b=2
c=3
Call p  /* a Procedure  */
Say 'in m a b c x' a b c x
Call s  /* a subroutine */
Say 'in m a b c x' a b c x

Exit
p: Procedure Expose sigl b
Say 'in p sigl a b c' sigl a b c
Call s
Return
s:
Say 'in s sigl a b c' sigl a b c
x=4
Return
```

{{out}}
When s is called from p, it can only see the variable b that is exposed by p.
When called directly, it sees all the 'global' variables a, b, and c.

Assigning a variable x in the subroutine s will similarly be seen or not in the main program.

```txt
in p sigl a b c 4 A 2 C
in s sigl a b c 12 A 2 C
in m a b c x 1 2 3 X
in s sigl a b c 6 1 2 3
in m a b c x 1 2 3 4
```

You can establish g. as global variable set by adding g. to each and every Procedure Expose and use g.0a etc. everywhere.


## Ruby


### Variables

The scope of a variable is decided by its first character(s).

$variable : global variable. Visible everywhere. Very seldom used.

@@variable: class variable. Visible inside a class and it's instances. Very seldom used

@variable : instance variable. Visible inside a class instance. Commonly used.

variable  : local variable. visible inside whichever comes first: a loop, a proc, a method, a class, a program.


### Methods

Instance methods may be public, private or protected

A public method is visible and usable inside and outside an instance.

A private method is internal to the class, and it can only be invoked from within the class (or subclass).

A protected method is available within a class and available to instances of the same class.

By default, methods are public. Use like this:

```ruby
class Demo
  #public methods here

  protected
  #protect methods here

  private
  #private methods
end
```

Ruby is an open language. Declaring methods private prevents inadvertend use of methods not meant to be used outside a class. However it is easy to circumvent with metaprogramming methods like <code>instance_eval</code>.


## Scala

In the [https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#modifiers Language Specification] is exactly defined what the 3 Access modifiers do.

## Tcl


### Variables

In Tcl procedures, variables are local to the procedure unless explicitly declared otherwise (unless they contain namespace separators, which forces interpretation as namespace-scoped names). Declarations may be used to access variables in the global namespace, or the current namespace, or indeed any other namespace.
{{works with|Tcl|8.5}}

```tcl
set globalVar "This is a global variable"
namespace eval nsA {
    variable varInA "This is a variable in nsA"
}
namespace eval nsB {
    variable varInB "This is a variable in nsB"
    proc showOff {varname} {
        set localVar "This is a local variable"
        global globalVar
        variable varInB
        namespace upvar ::nsA varInA varInA
        puts "variable $varname holds \"[set $varname]\""
    }
}
nsB::showOff globalVar
nsB::showOff varInA
nsB::showOff varInB
nsB::showOff localVar
```

{{out}}

```txt
variable globalVar holds "This is a global variable"
variable varInA holds "This is a variable in nsA"
variable varInB holds "This is a variable in nsB"
variable localVar holds "This is a local variable"
```

Objects have an extra variable access mode. All the variables declared in a class definition are visible by default in the methods defined in that class. All other variable access modes are still available too.

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
oo::class create example {
    # Note that this is otherwise syntactically the same as a local variable
    variable objVar
    constructor {} {
        set objVar "This is an object variable"
    }
    method showOff {} {
        puts "variable objVar holds \"$objVar\""
    }
}
[example new] showOff
```

{{out}}

```txt
variable objVar holds "This is an object variable"
```



### Commands

Tcl commands are strictly always scoped to a particular namespace (defaulting to the global namespace, which is just a normal namespace in a somewhat privileged position). Commands are looked up in the current namespace first, then according to the current namespace's path rules (always empty prior to Tcl 8.5), and then finally in the global namespace. This effectively puts the global namespace in the scope of every namespace (though override-able in every namespace as well). By convention, library packages are placed in namespaces other than the global one (except for legacy cases or a single package access command) so that they don't cause unexpected conflicts; typically the global namespace is reserved for the Tcl language and user applications.


### General Caller Scope Access

Of considerable relevance to this area are the <code>upvar</code> and <code>uplevel</code> commands. The first allows a variable name to be resolved to a variable in the scope of a caller of the current procedure and linked to a local variable in the current stack frame, and the second allows the execution of arbitrary code in the context of a caller of the current procedure. Both can work with any stack frame on the call stack (which consequently becomes a call tree) but the two most commonly referred-to frames are the immediate caller of the current procedure and the global/topmost stack frame.

To demonstrate these capabilities, here is an example of how we can create a <code>decr</code> command that is just like the <code>incr</code> command except for working with increments in the opposite direction.

```tcl
proc decr {varName {decrement 1}} {
    upvar 1 $varName var
    incr var [expr {-$decrement}]
}
```

Here is a kind of version of <code>eval</code> that concatenates its arguments with a semicolon first, instead of the default behavior (a space):

```tcl
proc semival args {
    uplevel 1 [join $args ";"]
}
```

Of course, these capabilities are designed to be used together. Here is a command that will run a loop over a variable between two bounds, executing a "block" for each step.

```tcl
proc loop {varName from to body} {
    upvar 1 $varName var
    for {set var $from} {$var <= $to} {incr var} {
        uplevel 1 $body
    }
}

loop x 1 10 {
    puts "x is now $x"
    if {$x == 5} {
        puts "breaking out..."
        break
    }
}
puts "done"
```

which prints:

```txt
x is now 1
x is now 2
x is now 3
x is now 4
x is now 5
breaking out...
done
```

As you can see, these are very powerful capabilities which make it trivial to write control structures in next to no Tcl code at all.

=={{header|TI-89 BASIC}}==
The only scope modifier in TI-89 BASIC is the <code>Local</code> command, which makes the variable local to the enclosing program or function rather than global (in some folder).


```ti89b
Local x
2 → x
Return x^x
```



## TXR

Functions and filters are global in TXR.  Variables are pattern matching variables and have a dynamically scoped discipline. The binding established in a clause is visible to other clauses invoked from that clause, including functions. Whether or not bindings survive from a given scope usually depends on whether the scope, overall, failed or succeeded. Bindings established in scopes that terminate by failing (or by an exception) are rolled back and undone.   The <code>@(local)</code> or <code>@(forget)</code> directives, which are synonyms, are used for breaking the relationship between variables occuring in a scope, and any bindings those variables may have. If a clause declares a variable forgotten, but then fails, then this forgetting is also undone; the variable is known once again. But in successful situations, the effects of forgetting can be passed down.

Functions have special scoping and calling rules. No binding for a variable established in a function survives the execution of the function, except if its symbol matches one of the function parameters, call it P, and that parameter is unbound (i.e. the caller specified some unbound variable A as the argument). In that case, the new binding for unbound parameter P within the function is translated into a new binding for unbound argument A at the call site. Of course, this only happens if the function succeeds, otherwise the function call is a failure with no effect on the bindings.

Illustration using named blocks. In the first example, the block succeeds and so its binding passes on:


```txr
@(maybe)@# perhaps this subclause suceeds or not
@  (block foo)
@  (bind a "a")
@  (accept foo)
@(end)
@(bind b "b")
```


Result (with <code>-B</code> option to dump bindings):


```txt
a="a"
b="b"
```


By contrast, in this version, the block fails. Because it is contained in a <code>@(maybe)</code>, evaluation can proceed, but the binding for <code>a</code> is gone.


```txr
@(maybe)@# perhaps this subclause suceeds or not
@  (block foo)
@  (bind a "a")
@  (fail foo)
@(end)
@(bind b "b")
```


Result (with <code>-B</code>):


```txt
b="b"
```



## Ursala

There are no variables in Ursala except dummy variables used in
lambda abstractions, but scope rules govern the visibility of
constants and function declarations.

When compiling a library, directives such as <code>#library</code> and <code>#binary</code> can
be switched on and off throughout a source text, and only the symbols
declared when they're on will become visible library entry points.

```Ursala
local_shop      = 0
hidden_variable = 3

#library+

this_public_constant = local_shop
a_visible_function   = +

#library-

for_local_people = 7
```

By default, every symbol is visible to every other within the same
file, and multiple declarations of the same symbol are an error, but the
scope modifiers <code>#hide</code> and <code>#export</code> can create multiple
scopes within a single file. In this example, the symbol <code>x</code> will have
a value of 1,

```Ursala
foo = 1

#hide+

foo = 2
bar = 3

#hide-

x = foo
```

but it will be 2 in this example, where
the <code>#export</code> directive selectively allows an otherwise
hidden declaration to be visible outside its enclosing
scope, and allows name clashes to be resolved by proximity.

```Ursala
foo = 1

#hide+

#export+
foo = 2
#export-

bar = 3

#hide-

x = foo
```

The <code>#hide</code> directives can be arbitrarily nested in matched pairs
to create block structured scope, but doing so is likely to be
overkill.

When name clashes occur between imported and locally declared
symbols, they are resolved by default in favor of the local
declaration. However, this behavior can be overridden using
the dash operator as shown.

```Ursala
#import std

cat = 3
a_string = std-cat('foo','bar')
```

Here, <code>std-cat</code> refers to the concatenation function from the standard
library, not the locally declared constant by that name.


## zkl

Functions in zkl have only one scope: the class/file they are defined in. They are not lexically scoped, they are promoted to the "top" of their enclosing class (as are vars). They only have direct access to class instance data and their enclosing class (and its contained classes/data).

Functions can use lexically local data via closures but that is one way; changes do not "leak" back out. In order to escape, closed over data must be a [mutable] container or in a mutable container.

Side note: Classes are basically containers that hold data such as variables, functions/code, other classes and parents.

{{omit from|AWK|Does not have scope modifiers}}
{{omit from|BASIC}}
{{omit from|GUISS}}
{{omit from|Locomotive Basic|Variables are global on the Amstrad CPC464}}
{{omit from|Unlambda|Does not have scopes (nor names).}}
{{omit from|ZX Spectrum Basic|Variables are global on the ZX Spectrum}}

[[Category:Scope]]

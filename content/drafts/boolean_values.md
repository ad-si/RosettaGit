+++
title = "Boolean values"
description = ""
date = 2019-10-17T23:36:32Z
aliases = []
[extra]
id = 4490
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Simple]]

;Task:
Show how to represent the boolean states "'''true'''" and "'''false'''" in a language.

If other objects represent "'''true'''" or "'''false'''" in conditionals, note it.


;Related tasks:
*   [[Logical operations]]





## 11l

11l defines a built-in data type <code>Bool</code>, which has two values represented by the constants <code>0B</code> and <code>1B</code>.


## 360 Assembly

The are no TRUE or FALSE constants in 360 Assembly; but an often used convention is :

```360asm
FALSE    DC     X'00'
TRUE     DC     X'FF'
```



## 8051 Assembly

A single bit represents true or false. By convention, 0 (cleared) is false, 1 (set) is true.
In the following, "bit" represents the direct address of any of the 256 directly accessible bits.

```asm
clr bit ; clears
setb bit ; sets
```



## 8th

In 8th, any non-zero number is true, as is the specific boolean value 'true'.  Everything else evaluates as 'false' (including the boolean value, 'false')


## ACL2

Same as [[Common Lisp|Boolean Values#Common Lisp]].


## Ada

[[Ada]] has a predefined discrete type with the specification:

```Ada
   type Boolean is (False, True);
```

with Boolean lattice and relational operations defined on it. See [http://www.adaic.org/standards/1zrm/html/RM-A-1.html RM A.1].


## ALGOL 68

{{trans|python}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - note: null char is missing, AND the C generated is won't compile, so some conversions are missing from RS}}
ALGOL 68 Enforces strong typing and so has few default coercions.  The appropriate operators must be used to convert to and from '''bool'''[ean] and the following code demonstrates principle conversions:

```algol68
BOOL f = FALSE, t = TRUE;
[]BOOL ft = (f, t);
STRING or = " or ";
FOR key TO UPB ft DO
  BOOL val = ft[key];
  UNION(VOID, INT) void = (val|666|EMPTY);
  REF STRING ref = (val|HEAP STRING|NIL);
  INT int = ABS val;
  REAL real = ABS val;
  COMPL compl = ABS val;
  BITS bits = BIN ABS val; # or bitspack(val); #
  BYTES bytes = bytes pack((val|"?"|null char)*bytes width);
  CHAR char = (val|"?"|null char);
  STRING string = (val|"?"|"");

  print((((val | "TRUE" | "FALSE" ), ": ", val, or, (val|flip|flop), new line)));
  print(("  void: ", " => ", (void|(VOID):FALSE|TRUE), new line));
  print(("   ref: ", " => ", ref ISNT REF STRING(NIL), new line));
  print(("   int: ", int     , " => ", int /= 0, new line));
  print(("  real: ", real    , " => ", real /= 0, new line));
  print((" compl: ", compl   , " => ", compl /= 0, new line));
  print(("  bits: ", bits    , " => ", ABS bits /= 0, or, bits /= 2r0, or,
                     bits width ELEM bits, or, []BOOL(bits)[bits width], new line));
  print((" bytes: """, STRING(bytes)    , """ => ", 1 ELEM bytes /= null char, or,
                       STRING(bytes) /= null char*bytes width, or,
                       STRING(bytes)[1] /= null char, new line));
  print(("  char: """, char  , """ => ", ABS char /= 0 , or, char /= null char, new line));
  print(("string: """, string  , """ => ", string /= "", or, UPB string /= 0, new line));
  print((new line))
OD
```


{{out}}

```txt

FALSE: F or F
  void:  => F
   ref:  => F
   int:          +0 => F
  real: +0.00000000000000e  +0 => F
 compl: +0.00000000000000e  +0+0.00000000000000e  +0 => F
  bits: FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF => F or F or F or F
 bytes: "" => F or F or F
  char: "" => F or F
string: "" => F or F

TRUE: T or T
  void:  => T
   ref:  => T
   int:          +1 => T
  real: +1.00000000000000e  +0 => T
 compl: +1.00000000000000e  +0+0.00000000000000e  +0 => T
  bits: FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFT => T or T or T or T
 bytes: "????????????????????????????????" => T or T or T
  char: "?" => T or T
string: "?" => T or T


```

Note: The '''string''' '''repr'''[esentation] of '''false''' and '''true''' are defined by the variables ''flop'' and ''flip'' respectively.


## ALGOL W

The boolean type is called logical in Algol W - the values are represented by the keywords true and false. Numbers, strings etc. cannot be used where logical values are required.


## APL

0 and 1 are used for boolean types in APL (as in J below).

```APL

    1 ^ 1
1
    1 ^ 0
0

```



## AppleScript

AppleScript has built-in boolean keywords <code>true</code> and <code>false</code>. Numbers do not work in place of boolean expressions, but they do coerce to and from.


```AppleScript>1
 2     --> false
not false --> true

{true as integer, false as integer, 1 as boolean, 0 as boolean}
          --> {1, 0, true, false}

true = 1  --> false
```


AppleScript also has constants <code>yes</code> and <code>no</code>, which coerce easily to boolean values.  They have little practical value in AppleScript except if one wishes to use them as arguments in place of boolean values for novelty's sake.  They are interchangeable with boolean values as parameters in AppleScriptObjC (not demonstrated here).


```AppleScript
{yes as boolean, no as boolean}
          --> {true, false}
```


<code>yes</code> and <code>no</code> do not coerce to integer values.

Finally, AppleScript also includes keywords <code>with</code> and <code>without</code>, used in declaring parameters for and sending parameters of boolean nature to handlers.  They are synonymous with <code>true</code> and <code>false</code>, respectively, and the compiler will sometimes perform the substitution at compile time.


```AppleScript>sortItems from L given reversal : true</lang


gets compiled immediately to become:


```AppleScript>sortItems from L with reversal</lang


However, the equivalent call to the handler utilising <code>yes</code>, whilst accepted readily in place of its boolean counterpart, is left alone by the compiler:


```AppleScript>sortItems from L given reversal:yes</lang



## Applesoft BASIC

IF statement condition treats any non-zero numeric value as true. Comparison operators evaluate to 1 (true) or 0 (false).
Examples:
```ApplesoftBASIC
? 2 = 3
? 2 = 2
IF 7 THEN ?"HELLO"
```


{{out}}

```txt
0
1
HELLO
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program areaString.s   */

/* Constantes    */
@ The are no TRUE or FALSE constants in ARM Assembly
.equ FALSE,  0      @ or other value
.equ TRUE,   1      @ or other value
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessTrue: .asciz "The value is true.\n"
szMessFalse: .asciz "The value is false.\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    mov r0,#0
    //mov r0,#1   @uncomment pour other test
    cmp r0,#TRUE
    bne 1f
    @ value true
    ldr r0,iAdrszMessTrue
    bl affichageMess
    b 100f
1:   @ value False
    ldr r0,iAdrszMessFalse
    bl affichageMess

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszMessTrue:		.int szMessTrue
iAdrszMessFalse:		.int szMessFalse
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



## Arturo


In Arturo, there are constants false and true to represent their respective values (that also implicitly convert to 0 and 1).


```arturo
a true
b false

if a { print "yep" } { print "nope" }
if b { print "nope" } { print "yep" }
```


{{out}}


```txt
yep
yep
```



## AutoHotkey

When an expression is required to evaluate to true or false (such as an <code>IF</code>-statement), a blank or zero result is considered false and all other results are considered true. Operators such as <code>NOT</code>/<code>AND</code>/<code>OR</code>/<code>></code>/<code>=</code>/<code><</code> automatically produce a true or false value: they yield 1 for true and 0 for false. A variable can be used to hold a false value simply by making it blank or assigning 0 to it. The words 'true' and 'false' are built-in variables containing 1 and 0. They can be used to make a script more readable.


## AWK


There is no keyword for true or false in awk. In awk, any nonzero numeric value or any nonempty string value is true. Any other value (zero or the null string "") is false. Values containing only zeros may produce true or false depending on whether they are obtained from the datasource or by assignment, and different results may be obtained according to which version of awk is being used.

* Reference: [http://awk.freeshell.org/truth AWK Truth values]

In the following example we use zero for false, and one for true to assign boolean values. However, this is just a convention, so other values may also have been used:


```awk
BEGIN {
   # Do not put quotes round the numeric values, or the tests will fail
   a = 1    # True
   b = 0    # False

   # Boolean evaluations
   if (a) { print "first test a is true" }        # This should print
   if (b) { print "second test b is true" }       # This should not print
   if (!a) { print "third test a is false" }      # This should not print
   if (!b) { print "forth test b is false" }      # This should print

   # Boolean evaluation using comparison against zero
   if (a == 0) { print "fifth test a is false" }  # This should not print
   if (b == 0) { print "sixth test b is false" }  # This should print
   if (a != 0) { print "seventh test a is true" } # This should print
   if (b != 0) { print "eighth test b is true" }  # This should not print

 }
```



## Axe

In Axe, there are no keywords for true and false. Any expression that evaluates to zero is considered false, and any expression that evaluates to non-zero is considered true. Unlike other languages, there is no canonical value for true (e.g. 1).


## BASIC

Most BASICs have no keywords for true and false. Boolean expressions evaluate to 0 when false, and a non-zero value (traditional versions of basic use a value of one, although some variants use a value of negative one) when true. Numbers also work in place of boolean expressions following those rules.


```gwbasic
10 LET A%=0
20 LET B%=NOT(A%)
30 PRINT "THIS VERSION OF BASIC USES"
40 PRINT B%; " AS ITS TRUE VALUE"
50 IF A% THEN PRINT "TEST ONE DOES NOT PRINT"
60 IF B% THEN PRINT "TEST TWO DOES PRINT"
70 IF A%=0 THEN PRINT "TEST THREE (FALSE BY COMPARISON) DOES PRINT"
80 IF B%=0 THEN PRINT "TEST FOUR (FALSE BY COMPARISON) DOES NOT PRINT"
90 IF A%<>0 THEN PRINT "TEST FIVE (TRUE BY COMPARISON) DOES NOT PRINT"
100 IF B%<>0 THEN PRINT "TEST SIX (TRUE BY COMPARISON) DOES PRINT"
110 END
```


==={{Header|BaCon}}===

```qbasic
' Boolean TRUE and FALSE are non-zero and zero constants
a = TRUE
b = FALSE
PRINT a, ", ", b

IF 0 THEN PRINT "TRUE" : ELSE PRINT "FALSE"
IF 1 THEN PRINT "TRUE"
IF 2 THEN PRINT "TRUE"
```


{{out}}

```txt

prompt$ bacon boolean.bac
Converting 'boolean.bac'... done, 8 lines were processed in 0.004 seconds.
Compiling 'boolean.bac'... cc  -c boolean.bac.c
cc -o boolean boolean.bac.o -lbacon -lm
Done, program 'boolean' ready.
prompt$ ./boolean
1, 0
FALSE
TRUE
TRUE
```



## BASIC256


```basic256

' BASIC256 used numbers to represent true and false
' values.  Zero is false and anything else is true.
' The built in constants true and false exist
' and represent one and zero respectively.

print false
print true

```




## Batch File

The closest thing to Boolean values in batch files is using <code>if defined</code>.
If a variable has any value, it will evaluate to true.

You can make a variable false by clearing its value <code>set "var="</code>.


```dos

@echo off

::true
set "a=x"
::false
set "b="

if defined a (
	echo a is true
) else (
	echo a is false
)
if defined b (
	echo b is true
) else (
	echo b is false
)

pause>nul

```


'''Output:'''

```txt

a is true
b is false

```



## BBC BASIC


```bbcbasic
      REM BBC BASIC uses integers to represent Booleans; the keywords
      REM FALSE and TRUE equate to 0 and -1 (&FFFFFFFF) respectively:

      PRINT FALSE
      PRINT TRUE
```



## bc

POSIX bc doesn't define Boolean values (i.e. it's up to the programmer which values represent false and true).

In GNU bc, 0 is false and any other value is true (but the result of a boolean expression will always be 1 if it is true).


## Befunge

Zero is false, non-zero is true. This is only used by the horizontal and vertical switch operators (<code>_</code> and <code>|</code>).


## Bracmat

Bracmat operates with success and failure instead of true and false. Success and failure play the same role as true and false in conditional tests, but they are not values like true and false. Instead, success and failure are properties of expressions in addition to values. The simplest failing expression is the atomic expression <code>~</code>. The simplest succeeding atomic expression is the empty string <code>""</code> (or <code>()</code>). A slightly more complex failing expression is <code>1+1:3</code>, which postulates that <code>3</code> matches the result of adding <code>1</code> and <code>1</code>, while <code>1+1:2</code> of course succeeds.

=={{header|Brainfuck}}==
Zero is false, non-zero is true. This is only used by the loop brackets (<code>[</code> and <code>]</code>).


## C

In C, a value which is equal to 0 is false, while a value which is not equal to 0 is true. Relational and logical operators evaluate to 0 for false and 1 for true. Any of the following can be used:
* any integer type, where 0 gives false, and any other value gives true (note that in C, character types are also integer types, therefore this also applies to characters: the <code>'\0'</code> character is false)
* any floating point type, where again, 0 gives false and everything else gives true
* any enumeration type, again 0 gives false, anything else true
* any pointer type, where the null pointer gives false and any other pointer gives true
* in C99, the boolean type <code>bool</code> (defined in header <tt><stdbool.h></tt>), where <code>true</code> gives true and <code>false</code> gives false
* in C99, any [[Complex numbers|complex number]] type, where 0 (0 real and 0 imaginary) gives false, anything else gives true


## C++

In C++, there are the constants <code>true</code> and <code>false</code> to represent those values. However, there are numerous implicit conversions to <code>bool</code>, therefore in conditions (and other contexts expecting boolean values), any of the following can be used:
* any integer type, where 0 converts to false, and any other value converts to true (note that in C++, character types are also integer types, therefore this also applies to characters: the <code>'\0'</code> character is false)
* any floating point type, where again, 0 gives false and everything else gives true
* any enumeration type, again 0 gives false, anything else true
* any pointer type, where the null pointer gives false and any other pointer gives true
* any user-defined type with an implicit conversion operator either to <code>bool</code> or to a built-in type which itself can be converted to <code>bool</code> (i.e. any of the above). The C++ standard library contains one such implicit conversion: the implicit conversion of a stream <code>s</code> to <code>bool</code> gives <code>!s.fail()</code>

## C#
In C#, there are the reserved keywords <code>true</code> and <code>false</code>. Variables to hold these values are declared as either <code>bool</code> or <code>Boolean</code>. These types are identical, as <code>bool</code> is just shortand for <code>Boolean</code>. The collection type <code>BitArray</code> returns its values as <code>Boolean</code>, packing 8 values into each byte (In contrast, the <code>Boolean</code> type uses the entire byte for one value).

Unlike C/C++, there is no conversion in C# between other types and <code>Boolean</code>.


## Clean


The standard library defines a data type <code>Bool</code>, which has exactly two members:


```clean
::Bool = False | True
```


In addition to all the functionality of any other Clean algebraic data type (e.g. [[pattern matching]]), and the specified derived typeclass instances, the built-in guard (“<code>|</code>”) and <code>if</code> syntaxes use Bool.

As with any other Clean data type, there are no automatic conversions of other types to Bool.


## Clojure

The boolean constants are ''true'' and ''false''. In a conditional context, the only false values are ''false'' and ''nil'' -- every other value is true.


## CMake


```cmake
foreach(var 1 42 ON yes True y Princess
            0 OFF no False n Princess-NOTFOUND)
  if(var)
    message(STATUS "${var} is true.")
  else()
    message(STATUS "${var} is false.")
  endif()
endforeach(var)
```



```txt
-- 1 is true.
-- 42 is true.
-- ON is true.
-- yes is true.
-- True is true.
-- y is true.
-- Princess is true.
-- 0 is false.
-- OFF is false.
-- no is false.
-- False is false.
-- n is false.
-- Princess-NOTFOUND is false.
```


The strings "0", "OFF", "NO", "FALSE" and "N" (ignoring case) are false. Any string ending with "-NOTFOUND" (ignoring case) is false. All other strings are true.

Scripts that want <code>if(TRUE)</code> should require CMake 2.8; do refer to [http://www.cmake.org/cmake/help/cmake-2-8-docs.html#policy:CMP0012  cmake --help-policy CMP0012].


## COBOL


### Booleans

Booleans are defined as any data item having a <code>PICTURE</code> made up of ones.

```cobol
       01  some-bool               PIC 1 BIT.
```


The boolean literals <code>B"1"</code> and <code>B"0"</code> represent true and false, respectively.


### Conditions

Prior to COBOL 2002, there was no boolean data type, only ''condition names'' which could be used in conditional expressions. Condition names are subordinate to another data item, have the level-number 88, and are defined with the value(s) which their parent data item must have for them to be set to true. They can be defined like so:

```cobol
       01  X PIC 9.
           88 X-Is-One        VALUE 1.
           88 X-Is-Even       VALUE 0 2 4 6 8.
           88 X-Larger-Than-5 VALUE 6 THRU 9.
```


Conditions can be <code>SET</code> to <code>TRUE</code> or <code>FALSE</code>. Setting a condition to <code>TRUE</code> will move the (first) value in the <code>VALUE</code> clause to the parent data item. In COBOL 2002, an optional <code>FALSE</code> clause was added which allowed the condition to be <code>SET</code> to <code>FALSE</code> and consequently set the parent data item to the specified value in the clause. A <code>FALSE</code> clause can only have one value. An example of conditions in action:

```cobol
       PROGRAM-ID. Condition-Example.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Foo PIC 9 VALUE 5.
           88  Is-Not-Zero VALUE 1 THRU 9
               WHEN SET TO FALSE IS 0.

       PROCEDURE DIVISION.
       Main.
           PERFORM Is-Foo-Zero

           SET Is-Not-Zero TO FALSE
           PERFORM Is-Foo-Zero

           SET Is-Not-Zero TO TRUE
           PERFORM Is-Foo-Zero

           GOBACK
           .

       Is-Foo-Zero.
           IF Is-Not-Zero
               DISPLAY "Foo is not zero, it is " Foo "."
           ELSE
               DISPLAY "Foo is zero."
           END-IF
           .
```


{{out}}

```txt

Foo is not zero, it is 5.
Foo is zero.
Foo is not zero, it is 1.

```



## CoffeeScript

CoffeeScript is largely based on JavaScript, but that may only serve to confuse you.  Your best bet is to learn all the cases:


```coffeescript

h1 = {foo: "bar"}
h2 = {foo: "bar"}

true_expressions = [
  true
  1
  h1? # because h1 is defined above
  not false
  !false
  []
  {}
  1 + 1 == 2
  1 == 1 # simple value equality
  true or false
]

false_expressions = [
  false
  not true
  undeclared_variable?
  0
  ''
  null
  undefined
  h1 == h2 # despite having same key/values
  1 == "1" # different types
  false and true
]

```




## Common Lisp

The only value in Common Lisp that is false is the symbol <code>nil</code>; all other values are true. The symbol <code>t</code> is the canonical true value.

Considered as variables, <code>nil</code> and <code>t</code> are bound to themselves ("self-evaluating"). <code>nil</code>, as well as being false, is used as the empty list; i.e. an empty list is false.

For more information, follow the links from [http://www.lispworks.com/documentation/HyperSpec/Body/t_ban.htm CLHS: Type BOOLEAN].


## Component Pascal


```oberon2

VAR
   b,c: BOOLEAN;
...
   b := TRUE;
   c := FALSE;
...

```



## D

In D, there are constants <code>false</code> and <code>true</code> to represent their respective values (that also implicitly convert to 0 and 1).
Implicit conversions to boolean are listed below:
* Any integer type, where 0 converts to false, and any other value converts to true;
* Any floating point type, where again, 0 gives false and everything else (but NaNs) gives true;
* Any enumeration type, again 0 gives false, anything else true;
* Any pointer type, where the null pointer gives false and any other pointer gives true;
* Any class reference type, using the "is" operator, the null reference gives false and any other reference gives true;
* Any user-defined type with an implicit conversion operator (opCast) either to bool or to a built-in type which itself can be converted to bool.


## Delphi

In addition to the types defined by [[#Object Pascal|Object Pascal]], Delphi defines the type <code>Bool</code>.

=={{header|Déjà Vu}}==

Déjà Vu has <code>true</code> and <code>false</code>, two numbers that are equal to 1 and 0 respectively. Every object has a truth value. The only falsy things are numbers equal to zero, empty lists and dictionaries, and zero-length strings and blobs.


## DWScript

The standard <code>Boolean</code> type has two values: <code>True</code> and <code>False</code>, with <code>Ord(False) = 0</code> and <code>Ord(True) = 1</code>.


## Dyalect


Dyalect has a standard <code>Bool</code> type with two values: <code>true</code> and <code>false</code>. Other types in Dyalect support implicit conversion to booleans. All values except <code>false</code>, <code>nil</code>, <code>0</code>, <code>0.0</code> and empty string are converted to <code>true</code>.


## Dylan


```Dylan>#t // <boolean
 true
#f // <boolean> false
```

For the purpose of conditional statements, all objects other than <tt>#f</tt> evaluate to true.


## E


E defines two basic objects <code>true</code> and <code>false</code>, and the <code>boolean</code> [http://wiki.erights.org/wiki/Guard guard] which accepts them. All builtin operations which take booleans (e.g. the <code>if</code> control structure) coerce the input to boolean.


```e
? if (true) { "a" } else { "b" }
# value: "a"

? if (false) { "a" } else { "b" }
# value: "b"

? if (90) { "a" } else { "b" }
# problem: the int 90 doesn't coerce to a boolean
```


No objects in the standard library coerce to boolean, but user-written objects may choose to do so; they can then be used in place of booleans.


```e
? def bowlian {
>     to __conformTo(guard) {
>         if (guard == boolean) { return true }
>     }
> }
> if (bowlian) { "a" } else { "b" }
# value: "a"
```



## EchoLisp

"All that which is not false is true" - Attribué à L. Wittgenstein - The only false value is the boolean #f. '''All''' other objects, including the empty list or null or 0 ..- evaluate to #t = true.

```scheme

(not #t)  → #f
(not #f)  → #t
(not null) → #f
(not 0) → #f

```





## EGL

In EGL boolean is a primitive type, however it acts the same as an integer (type int). A boolean and an int accept integer values aswel as true and false keywords (which represent resp. 1 and 0). A boolean is always true except when it has value 0 (or keyword false).
A boolean can be converted to a string ("true" or "false") using StrLib.booleanAsString(boolean);


```EGL

myBool boolean = 0;
SysLib.writeStdout("myBool: " + StrLib.booleanAsString(myBool));
myBool = 1;
SysLib.writeStdout("myBool: " + StrLib.booleanAsString(myBool));
myBool = 2;
SysLib.writeStdout("myBool: " + StrLib.booleanAsString(myBool));
myBool = false;
SysLib.writeStdout("myBool: " + StrLib.booleanAsString(myBool));
myBool = true;
SysLib.writeStdout("myBool: " + StrLib.booleanAsString(myBool));
myInt int = 0;
SysLib.writeStdout("myInt: " + StrLib.booleanAsString(myInt));
myInt = 1;
SysLib.writeStdout("myInt: " + StrLib.booleanAsString(myInt));
myInt = 2;
SysLib.writeStdout("myInt: " + StrLib.booleanAsString(myInt));
myInt = false;
SysLib.writeStdout("myInt: " + StrLib.booleanAsString(myInt));
myInt = true;
SysLib.writeStdout("myInt: " + StrLib.booleanAsString(myInt));

```


{{out}}

```txt

myBool: false
myBool: true
myBool: true
myBool: false
myBool: true
myInt: false
myInt: true
myInt: true
myInt: false
myInt: true

```




## Elena

ELENA uses the system'BaseBoolValue class, which has two singleton sub-classes: system'true and system'false. E.g. an expression like 5 == 5 returns system'true.
There is a Boolean variable : system'Boolean.


## Elixir

Elixir utilizes Erlang's definition of boolean types; they're defined as the atoms <tt>:true</tt> and <tt>:false</tt>. No other type is equal to true or false.

```Elixir

iex(1)> true === :true
true
iex(2)> false === :false
true
iex(3)> true === 1
false

```


nil (also defined as an atom, <tt>:nil</tt>) is not equal to false.

```Elixir

iex(4)> nil === :nil
true
iex(5)> nil === false
false

```



## Elm


```Elm

--True and False directly represent Boolean values in Elm
--For eg to show yes for true and no for false
if True then "yes" else "no"

--Same expression differently
if False then "no" else "yes"

--This you can run as a program
--Elm allows you to take anything you want for representation
--In the program we take T for true F for false
import Html exposing(text,div,Html)
import Html.Attributes exposing(style)

type Expr = T | F | And Expr Expr | Or Expr Expr | Not Expr

evaluate : Expr->Bool
evaluate expression =
 case expression of
 T ->
  True

 F ->
  False

 And expr1 expr2 ->
  evaluate expr1 && evaluate expr2

 Or expr1 expr2 ->
  evaluate expr1 || evaluate expr2

 Not expr ->
  not (evaluate expr)

--CHECKING RANDOM LOGICAL EXPRESSIONS
ex1= Not F
ex2= And T F
ex3= And (Not(Or T F)) T

main =
    div [] (List.map display  [ex1, ex2, ex3])

display expr=
   div [] [ text ( toString expr ++ "-->" ++ toString(evaluate expr) ) ]
--END
```



## Emacs Lisp

Symbol <code>nil</code> is false and symbol <code>t</code> is true.  Both are self-evaluating, being variables whose value is their own symbol.  See [http://www.gnu.org/software/emacs/manual/html_node/elisp/nil-and-t.html the elisp manual] for more.

In an <code>if</code> and similar, <code>nil</code> is false and anything else is true.  To make that clear docstrings etc say "non-nil" for true.  (See last item in [http://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html elisp manual documentation tips].)


## Erlang

Erlang doesn't technically define boolean types. Instead, the atoms <tt>true</tt> and <tt>false</tt> are used. However, they are integrated well enough into the language there should be no problem with that as long as you don't expect false and true to mean anything but literal false and true.


```erlang>1
 1 < 2.
true
2> 1 < 1.
false
3> 0 == false.
false
```



## Excel

The Logical category of functions includes the constants TRUE() and FALSE() which are displayed without the parantheses in cells. There are logical functions such as AND and OR too. For an AND truth table of two variables, take 3 cells, say A1,B1 and C1. In C1 type in :


```excel
=AND(A1;B1)
```


Copy this until C4. Now as values are filled in from A1-A4 and B1-B4, C1-C4 gets updated.

<lang>0	0	FALSE
0	1	FALSE
1	0	FALSE
1	1	TRUE
```


=={{header|F_Sharp|F#}}==
The type bool is an abbreviation for the .NET framework type <code>System.Boolean</code>.

```fsharp>type bool = System.Boolean</lang

Instances of this type have values of either <code>true</code> or <code>false</code>.


## Factor

In Factor any value except <code>f</code> is true, with <code>t</code> being the canonical true value.


## FALSE

Zero is false and non-zero is true.  This is used by the if and while operators ('''?''' and '''#'''). Comparators ('''=''' and '''<''') yield -1 for true and 0 for false.


## Fantom


Conditional statements must return a <code>sys::Bool</code>, and the only two values are <code>true</code> and <code>false</code>.


## Forth

In conditionals, zero is false, non-zero is true. There are predefined constants for the canonical forms. For FORTH-83 or later, FALSE is zero and TRUE is -1 (all bits set).  For earlier FORTH standards, FALSE is zero and TRUE is 1.

```forth
TRUE .    \ -1
FALSE .   \ 0
```



## Fortran

Fortran started off in 1957 with only floating-point and fixed-point variables, so any calculations in the style of Boolean arithmetic would be done with integer values such as zero and not-zero, using multiplication and addition for '''and''' and '''or'''.

Fortran 66 introduced a '''logical''' data type which can be set to either '''.true.''' or '''.false.''' or be generated via logical expressions such as <=, etc. Such variables cannot be used in normal arithmetic with operators such as +-*/ but only with logical operators such as .OR. and so on. If via the EQUIVALENCE statement their numerical values (or, bit patterns) are inspected as say an integer, the values may well not be as anticipated and differ between computers and compilers. For instance, on the Burroughs 6700 an '''integer''' variable equivalenced to a '''logical''' variable would appear as '''.true.''' if odd, '''.false.''' if even.

The default storage size of a LOGICAL variable is the same as the default storage size of an INTEGER variable, which for many systems is 32 bits. This is done to simplify calculations of record sizes, or the alignment of variables in COMMON storage areas. It is usually possible to declare variables with certain byte sizes (normally only powers of two) so that LOGICAL*1 or similar declarations may be available. If used however there may arise alignment issues with adjacent variables of other types (such as REAL) that may require padding to even word boundaries for best access. Consider
```Fortran
      TYPE MIXED
       LOGICAL*1 LIVE
       REAL*8    VALUE
      END TYPE MIXED
      TYPE(MIXED) STUFF(100)
```

The array STUFF might occupy 900 bytes, or, 1600 bytes if each double-precision value has to be aligned to an eight-byte boundary. In the latter case, it may be better to declare LIVE and VALUE to be separate hundred-element arrays as in
```Fortran
      TYPE MIXED
       LOGICAL*1 LIVE(100)
       REAL*8    VALUE(100)
      END TYPE MIXED
      TYPE(MIXED) STUFF
```

Except that now only hundred-element variables of type MIXED can be declared. Either way, the record size needed for a disc file holding such items will need careful thought.


## FreeBASIC

FreeBASIC has a built-in Boolean type (equivalent to a signed one byte integer) with built-in constants true and false to represent values of that type. Note also that:

* Numeric expresions can be converted to the Boolean type either implicitly or using the CBool operator - zero is converted to false and non-zero to true.

* Boolean expressions can be converted to a numeric type either implicitly or using the appropriate cast operator (CInt, CByte, CDbl etc) - false is converted to 0 and true to -1.

* String expressions such as "false" and "true" (regardless of case) can also be converted to Boolean using CBool.

* It is possible to overload CBool for user-defined types to yield a Boolean value.


Sample code:

```freebasic
' FB 1.05.0 Win64

Dim i As Integer = 23
Dim s As String = "False"
Dim b As Boolean
b = i
Print b
b = CBool(s)
Print b
i = b
Print i
i = CInt(true)
Print i
Sleep
```


{{out}}

```txt

true
false
 0
-1

```



## Free Pascal

In addition to the types defined by [[#Object Pascal|Object Pascal]], free Pascal defines the <code>qWordBool</code>, that has a <code>sizeOf</code> eight.


## Futhark


Futhark has a <code>bool</code> type, with the two values <code>True</code> and <code>False</code>.  They are used for branching.


## Gambas

'''[https://gambas-playground.proko.eu/?gist=65324112fde86d51937b9cfcca0c51f9 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim bX As Boolean

Print bX
bX = True
Print bX

End
```

Output:

```txt

False
True

```



## GAP


```gap
1 < 2;
# true

2 < 1;
# false

# GAP has also the value fail, which cannot be used as a boolean but may be used i$

1 = fail;
# false

fail = fail;
# true
```



## Go

Go defines a built-in data type <code>bool</code>, which has exactly two values, represented by the keywords <code>true</code> and <code>false</code>. There is no conversion between booleans and other data types.  Conditionals require a boolean value, so if i is a numeric type, for example, you must spell out <tt>if i != 0 {</tt> if you wish to interpret it as boolean.

The template package however, uses a different rule for <tt>if</tt> actions.  There, it is testing if a "pipeline" is "empty" where the empty values are false, 0, any nil pointer or interface value, and any array, slice, map, or string of length zero.


## Groovy

[[Groovy]] has a boolean "primitive" type and a Boolean "object wrapper" type directly derived from [[Java]]. See the Java solution to this task for more details.

Unlike Java, any null reference converts to a boolean "false", while any non-null object reference converts to a boolean "true"... EXCEPT if that object has a specific defined conversion to boolean "false". For example, for any numeric type, any zero value representation converts to "false" and any non-zero value converts to "true". For any collection type, non-empty converts to "true" and empty converts to "false".


## Haskell


The Haskell standard [http://haskell.org/haskellwiki/Prelude Prelude] defines a data type <code>Bool</code>, which has exactly two members:


```haskell
data Bool = False | True    deriving (Eq, Ord, Enum, Read, Show, Bounded)
```


In addition to all the functionality of any other Haskell algebraic data type (e.g. [[pattern matching]]), and the specified derived typeclass instances (e.g. <code>False == False</code>, <code>succ False == True</code>, <code>(maxBound :: Bool) == True</code>, etc.), the built-in guard (“<code>|</code>”) and <code>if</code> syntaxes use Bool.

As with any other Haskell data type, there are no automatic conversions of other types to Bool.


## HicEst

Zero is false, non-zero is true. Numbers also work in place of boolean expressions following this rule.


## HolyC

In HolyC, there are the reserved keywords `TRUE` and `FALSE`. Variables to hold these values are declared as `Bool`.

Any value which is equal to 0 is false, while a value which is not equal to 0 is true. Relational and logical operators evaluate to 0 for false and 1 for true. Any of the following can be used:

* Any integer type, where 0 gives false, and any other value gives true.
* Any floating point type, where again, 0 gives false and everything else gives true.
* Any pointer type, where the null pointer gives false and any other pointer gives true.


## i

Any non-zero number is true in 'i'.

```i
main
	//Bits aka Booleans.
	b $= bit()

	b $= true
	print(b)

	b $= false
	print(b)

	//Non-zero values are true.
	b $= bit(1)
	print(b)

	b $= -1
	print(b)

	//Zero values are false
	b $= 0
	print(b)
}
```


== {{header|Icon}} and {{header|Unicon}}==
Icon and Unicon do not use Boolean values for flow control. Rather they use success (returning a result, any result even a null) or failure (a signal) for this purpose.  Built-in controls support not, and (&), and or (|).  For an example of how this works, see [[Short-circuit_evaluation#Icon_and_Unicon|Short Circuit Evaluation]]. Icon and Unicon do support bit operations on integers which could be used to record Boolean state. See also [[Logical_operations#Icon_and_Unicon|Logical Operations]] for an example of how and when Boolean values might be implemented.


## Idris


```idris>Idris
 :doc Bool
Data type Prelude.Bool.Bool : Type
    Boolean Data Type

Constructors:
    False : Bool


    True : Bool

```



## Inform 6

Inform 6 has the constants <code>true</code> and <code>false</code>, which are identical to <code>1</code> and <code>0</code> respectively.  One of these values will always be yielded by a condition operator (an operator that yields a boolean value).  In addition, any non-zero value is considered to be true.


## Inform 7

The Boolean type is called "truth state" and has the values "true" and "false".

However, Inform 7 distinguishes between Boolean values and conditions. Comparison expressions do not return truth states, and truth state expressions cannot be used directly in conditional statements. There is a conversion from condition to truth state:

```inform7>let B be whether or not 123 is greater than 100;</lang

And truth states can be used in conditions by adding an explicit comparison:

```inform7
if B is true, say "123 is greater than 100."
```


Phrases (functions) cannot be defined to return a truth state directly. Instead, they are defined using "to decide whether" (or "to decide if") and can then be used as conditions:

```inform7
To decide whether the CPU is working correctly:
	if 123 is greater than 100, decide yes;
	otherwise decide no.

When play begins:
	[convert to truth state...]
	let B be whether or not the CPU is working correctly;
	[...or use as a condition]
	if the CPU is working correctly, say "Whew."
```



## J


False is <tt>0</tt>, true is <tt>1</tt>.  This is an [http://keiapl.info/anec/#Maple advantage].

This approach also works well with [[wp:Bayes'_theorem|Bayes' theorem]], as false matches 0% probability and true matches 100% probability.


## Java

Java has <tt>true</tt> and <tt>false</tt> keywords, representing the only values of type <tt>boolean</tt>. There are also object wrappers <tt>Boolean.TRUE</tt> and <tt>Boolean.FALSE</tt>, of type <tt>Boolean</tt> which may be un-boxed into <tt>boolean</tt>s (auto-unboxed in Java 1.5+). There are no automatic conversions from any other types into <tt>boolean</tt>, and it is a compile-time error to use any type other than <tt>boolean</tt> or <tt>Boolean</tt> in a place that expects a <tt>boolean</tt> (e.g. if-statement condition, while-statement condition, operand of a logical operator, etc.).


## JavaScript

The Boolean type has two values: <code>true</code> and <code>false</code>

The following table shows the result of type conversions to boolean:
* Undefined: any undefined value is converted to <code>false</code>
* Null: <code>false</code>
* Number: the numbers <code>0</code>, <code>-0</code>, <code>NaN</code> are <code>false</code>; otherwise <code>true</code>
* String: the empty (zero-length) string is <code>false</code>; otherwise <code>true</code>
* Object: any object is converted to <code>true</code>

(source: [http://www.ecma-international.org/publications/standards/Ecma-262.htm ECMAScript Language Reference])


## jq

<tt>true</tt> and <tt>false</tt> are the only entities of type "boolean":

<tt>
 $ jq type
 true
 "boolean"
 false
 "boolean"
</tt>

The above shows the jq command invocation, followed by alternating lines of input and output.

jq's logical operators, however, do not require boolean inputs.  In brief, <tt>false</tt> and <tt>null</tt> are both regarded as false, and all other JSON entities are regarded as <tt>true</tt>.  That is, all values except for <tt>false</tt> and <tt>null</tt> are truthy.


## Julia

Julia has a built-in <code>Bool</code> type with values <code>true</code> and <code>false</code>.

Other objects do not represent boolean values and cannot be used in conditional expressions, for example:

```julia>julia
 if 1
         println("true")
       end
ERROR: type: non-boolean (Int64) used in boolean context
```

However, integers can be converted to boolean types with the <code>bool()</code> function (which treats nonzero values as <code>true</code>)

```julia>julia
 bool(-2:2)
5-element Bool Array:
  true
  true
 false
  true
  true
```



## KonsolScript

The Boolean type has two values: <code>true</code> and <code>false</code>

The following table shows the result of type conversions to boolean:
* Number: the number <code>0</code> is <code>false</code>; otherwise <code>true</code>
* String: the empty (zero-length) string is <code>false</code>; otherwise <code>true</code>
`

## Kotlin

Booleans in Kotlin are given by the literals true and false, case sensitive, which are the only instances of the class Boolean.


## LabVIEW

{{VI solution|LabVIEW_Boolean_values.png}}


## Lasso

Comparisons are evaluated in Lasso as either true of false, so "1 == 2" will evaluate as true, and "1 == 1" will evaluate as true.

A variable can also be assigned a boolean type, and as such then holds either true of false states.


```Lasso
!true
// => false

not false
// => true

var(x = true)
$x // => true

$x = false
$x // => false
```


In a conditional, if the result is the integer 0, it is also evaluated as boolean false.
If the conditional results in an integer greater than zero, it is evaluated as boolean true.


```Lasso
local(x = string)
// size is 0
#x->size ? 'yes' | 'no'

local(x = '123fsfsd')
// size is 8
#x->size ? 'yes' | 'no'
```


{{out}}

```txt
no
yes
```



## Latitude


In Latitude, an object's truthiness is determined by its <code>toBool</code> method, which must return one of the constants <code>True</code> or <code>False</code>. Obviously, these two constants have trivial <code>toBool</code> methods which return themselves. <code>Nil</code> is also falsy, while most other built-in objects (including numbers, strings, arrays, etc.), including "empty" objects such as the empty string, are truthy.

By convention, objects which are used to represent failure are considered falsy. For instance, the standard library <code>'unit-test</code> module provides the <code>FailedTest</code> object, which is returned when a unit test fails. This object (and its children) test falsy when used as a conditional.


## LFE


```lisp

> 'true
true
> 'false
false
> (or 'false 'false)
false
> (or 'false 'true)
true

```



## Liberty BASIC

IF-statement, loop condition treats any non-zero integer as true.
Comparison operators evaluate to 1 (true) or 0 (false).


## Lingo

Lingo has the constants TRUE and FALSE. In numerical context they have the values 1 and 0. In boolean context any nonzero integer evaluates to TRUE.

```lingo
put TRUE
-- 1
put FALSE
-- 0
if 23 then put "Hello"
-- "Hello"
```



## Little

For conditionals, numeric variables (including poly variables
with a number in them), evaluate to true or false based on
their value.

Use the defined() buildin to test if a variable is defined.

For the rest of variable types the value depend if the variable
is defined or not.



```C
int a = 0;
int b = 1;
int c;
string str1 = "initialized string";
string str2; //  "uninitialized string";

if (a) {puts("first test a is false");}         // This should not print
if (b) {puts("second test b is true");}         // This should print
if (c) {puts("third test b is false");}         // This should not print
if (!defined(c)) {puts("fourth test is true");} // This should print
if (str1) {puts("fifth test str1 is true");}    // This should print
if (str2) {puts("sixth test str2 is false");}   // This should not print
```



## LiveCode

true and the string "true"  are both logical true, similarly for false and "false" being logical false.


## Logo

Logo has predefined symbols for true and false (<code>"true</code> and <code>"false</code>), which are the values returned by predicates and required by logical operators and conditionals.

```logo
print 1 < 0    ; false
print 1 > 0    ; true
if "true [print "yes]    ; yes
if not "false [print "no]  ; no
```

Unlike other lispy languages, there are no other implicit conversions.
You must test explicitly for zero or empty collections.

```logo
if equal? 0 ln 1 [print "zero]
if empty? [] [print "empty]    ; empty list
if empty? "|| [print "empty]   ; empty word
```



## Lua

All values in Lua other than <code>false</code> or <code>nil</code> are considered <code>true</code>:

```lua
if 0 then print "0" end             -- This prints
if "" then print"empty string" end  -- This prints
if {} then print"empty table" end   -- This prints
if nil then print"this won't print" end
if true then print"true" end
if false then print"false" end      -- This does not print
```



## M2000 Interpreter

True is -1 and False is 0 (double type), but any compare return boolean. We can define boolean type variables.

Using Switches "+sbl" in console or Set Switches "+sbl" in code in a module, we get Prints of boolean values as True/False or Αληθές/Ψευδές

use Greek to change to 1032 locale and Greek error messages and dialogs

use Latin to change to 1033 locale and English error messages and dialogs

We can use Locale 1032 to change only locale to Greek.

M2000 print in console any character from Unicode, and diacritics (one or more, without moving the cursor).



```M2000 Interpreter

Module CheckBoolean {
      A=True
      Print Type$(A)="Double"
      B=1=1
      Print Type$(B)="Boolean"
      Print A=B  ' true
      Print A, B   ' -1   True
      Def boolean C=True, D=False
      Print C, D , 1>-3 ' True False True
      K$=Str$(C)
      Print K$="True"  ' True
      Function ShowBoolean$(&x) {
           x=false
           Try {
                        if keypress(32) then x=true : exit
                        If Keypress(13) then exit
                        loop
            }
            =str$(x, locale)
      }
      Wait 100
      Print "C (space for true, enter for false)="; : Print ShowBoolean$(&c)
      Print C
}
CheckBoolean

Print str$(True, "\t\r\u\e;\t\r\u\e;\f\a\l\s\e")="true"
Print str$(False, "\t\r\u\e;\t\r\u\e;\f\a\l\s\e")="false"
Print str$(2, "\t\r\u\e;\t\r\u\e;\f\a\l\s\e")="true"


```



## Maple

The keywords "true" and "false" are the default boolean values.
Expressions involving relational operators are evaluated logically using the <code>evalb</code> command.
Expressions under assumptions may be evaluated logically using the <code>is</code> command.
Types may be tested, resulting in boolean values, using the <code>type</code> command.


## Mathematica

True and False are the default boolean values.
To make any expression a boolean use the Boole[] function.


## MATLAB

The keywords "true" and "false" are the default boolean values.
But, many functions prefer to return boolean "1" or "0" instead of "true" or "false".
It is very important to note that having a function return a numerical 1 or 0 is not the same as a boolean "1" or "0".
To make a number or array of numbers a boolean use the logical() function. logical() will convert any non-zero number to a boolean "1" and any zero entries a boolean "0".

Sample Usage: (islogical() is a function that returns a boolean "1" if the input is a boolean, "0" otherwise)


```MATLAB>>
 islogical(true)

ans =

     1

>> islogical(false)

ans =

     1

>> islogical(logical(1))

ans =

     1

>> islogical(logical(0))

ans =

     1

>> islogical(1)

ans =

     0

>> islogical(0)

ans =

     0
```



## Maxima


```maxima
is(1 < 2);
/* true */

is(2 < 1);
/* false */

not true;
/* false */

not false;
/* true */
```



## Metafont

Metafont has the type <tt>boolean</tt>; a boolean variable can be <tt>true</tt> or <tt>false</tt>.
Using non boolean values (or expressions that do not evaluate to a boolean value) results in a recoverable error; by default, any non-boolean value is interpreted as false.


## min

{{works with|min|0.19.3}}
<code>true</code> and <code>false</code> are the only boolean values in min. The <code>bool</code> function converts various objects to boolean values:
*non-zero number: <code>true</code>
*zero number: <code>false</code>
*non-empty quotation: <code>true</code>
*empty quotation: <code>false</code>
*non-empty string: <code>true</code>
*empty string: <code>false</code>
*boolean: no conversion performed


## MiniScript

In MiniScript, numbers represent boolean values, with additional fuzzy logic for degrees of truth.  Built-in constants `true` and `false` are simply aliases for 1 and 0, respectively.


```MiniScript
boolTrue = true
boolFalse = false

if boolTrue then print "boolTrue is true, and its value is: " + boolTrue

if not boolFalse then print "boolFalse is not true, and its value is: " + boolFalse

mostlyTrue = 0.8
kindaTrue = 0.4
print "mostlyTrue AND kindaTrue: " + (mostlyTrue and kindaTrue)
print "mostlyTrue OR kindaTrue: " + (mostlyTrue or kindaTrue)
```

{{out}}

```txt
boolTrue is true, and its value is: 1
boolFalse is not true, and its value is: 0
mostlyTrue AND kindaTrue: 0.32
mostlyTrue OR kindaTrue: 0.88
```



## Mirah


```mirah
import java.util.ArrayList
import java.util.HashMap

# booleans
puts 'true is true' if true
puts 'false is false' if (!false)

# lists treated as booleans
x = ArrayList.new
puts "empty array is true" if x
x.add("an element")
puts "full array is true" if x
puts "isEmpty() is false" if !x.isEmpty()

# maps treated as booleans
map = HashMap.new
puts "empty map is true" if map
map.put('a', '1')
puts "full map is true" if map
puts "size() is 0 is false" if !(map.size() == 0)

# these things do not compile
# value = nil   # ==> cannot assign nil to Boolean value
# puts 'nil is false' if false == nil  # ==> cannot compare boolean to nil
# puts '0 is false' if (0 == false)    # ==> cannot compare int to false

#puts 'TRUE is true' if TRUE   # ==> TRUE does not exist
#puts 'FALSE is false' if !FALSE   # ==> FALSE does not exist


```


=={{header|Modula-2}}==

```modula2
MODULE boo;

IMPORT  InOut;

VAR     result, done            : BOOLEAN;
        A, B                    : INTEGER;

BEGIN
   result := (1 = 2);
   result := NOT result;
   done := FALSE;
   REPEAT
     InOut.ReadInt (A);
     InOut.ReadInt (B);
     done := A > B
   UNTIL done
END boo.
```


=={{header|Modula-3}}==
Similar to [[Ada]], Modula-3 has a built-in <tt>BOOLEAN</tt> type defined as

```modula3
TYPE BOOLEAN = {FALSE, TRUE}
```



## Monte


Much like [[E]], Monte has built-in objects <tt>true</tt> and <tt>false</tt>, and a boolean [http://wiki.erights.org/wiki/Guard guard].


```Monte

def example(input :boolean):
    if input:
        return "Input was true!"
    return "Input was false."

```



## MUMPS


<p>M[UMPS] has no data types per se, however, any value can be coerced to a specific
interpretation by applying certain operators.</p>

<p>Internally, the language treats any "zero" value as a "false", and any "non-zero"
value as a "true".<br/>
Values like 1, 2, 13245.08763, .1, "0.00001234ABC" and "1234ABC" are "true".<br/>
Values like 0, -3, "", " 123" (note the leading space in this one),
"+++++567", "abc", "abc1245" are "false".</p>

<p>When a boolean operator is applied to an operand, the value of that operand is
coerced to a logical value, that is: if the value starts out with a sequence of
digits that look like a non-zero number, the value is "true" (1), and otherwise
that value is "false" (0).</p>

<p>There are two standardized binary boolean operators: & (and) and ! (or).
Newer implementations of the language may also support !! (exclusve or).
There is one unary boolean operator: ' (not).</p>


## Neko


Neko includes a bool boolean data type, true and false.  Conditional execution flow only reacts to bool, numeric values test as false as are string literals.

Neko also includes two low level builtins: $not(value) and $istrue(value).  These return bool results.  $not returning true if value
is false, 0 or null.  $istrue returning true if value is not false, not 0 and not null.


```neko
/* boolean values */
$print(true, "\n");
$print(false, "\n");

if 0 {
  $print("literal 0 tests true\n");
} else {
  $print("literal 0 tests false\n");
}

if 1 {
  $print("literal 1 tests true\n");
} else {
  $print("literal 1 tests false\n");
}

if $istrue(0) {
  $print("$istrue(0) tests true\n");
} else {
  $print("$istrue(0) tests false\n");
}

if $istrue(1) {
  $print("$istrue(1) tests true\n");
} else {
  $print("$istrue(1) tests false\n");
}
```


{{out}}

```txt

prompt$ nekoc boolean.neko
prompt$ neko boolean
true
false
literal 0 tests false
literal 1 tests false
$istrue(0) tests false
$istrue(1) tests true
```




## Nemerle

In Nemerle, boolean values are held in variables of type '''bool''', and can be either '''true''' or '''false'''.  Comparison expressions evaluate to boolean values as well.


## NetRexx

NetRexx inherits boolean functionality directly from the [[Java]] virtual machine with the exception that the <code>true</code> and <code>false</code> keywords are not defined to the language.
Defining <code>true</code> and <code>false</code> variables can lead to name collisions during compilation so a simple expedient is to define boolean functions <code>isTrue</code>
and <code>isFalse</code> to return the appropriate values.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

bval = [1, 0, 5, 'a', 1 == 1, 1 \= 1, isTrue, isFalse]

loop b_ = 0 for bval.length
  select case bval[b_]
    when isTrue  then say bval[b_] 'is true'
    when isFalse then say bval[b_] 'is false'
    otherwise         say bval[b_] 'is not boolean'
    end
  end b_

method isTrue public static returns boolean
  return (1 == 1)

method isFalse public static returns boolean
  return \isTrue

```


{{out}}

```txt

1 is true
0 is false
5 is not boolean
a is not boolean
1 is true
0 is false
1 is true
0 is false

```



## Microsoft Small Basic

Microsoft Small Basic has two constants: <code>"True"</code> and <code>"False"</code>.


```smallbasic
If c Then
  notc = "False"
Else
  notc = "True"
EndIf
```



## Nim


```nim
if true: echo "yes"
if false: echo "no"

# Other objects never represent true or false:
if 2: echo "compile error"
```


=={{header|Oberon-2}}==

```oberon2

VAR
  a,b,c: BOOLEAN;
...
  a := TRUE;
  b := FALSE;
  c := 1 > 2;

```



## Objeck

Objeck has a <tt>Bool</tt> type that is set to either <tt>true</tt> or <tt>false</tt>.  By default boolean types are initialized to <tt>false</tt>.  The boolean type also allows methods to be invoked, which perform simple conversions or print given values.

=={{header|Objective-C}}==
Objective-C follows pretty much the same rules as C. In addition to C, Objective-C has a <code>BOOL</code> boolean type, with values <code>YES</code> for true and <code>NO</code> for false. Objective-C also adds several special types of pointers; for pointers to objects (values of type <code>id</code>), the <code>nil</code> pointer is false, everything else is true; for pointers to classes (values of type <code>Class</code>), the <code>Nil</code> pointer is false, everything else is true.


## Object Pascal

In addition to the <code>Boolean</code> type defined by standard [[#Pascal|Pascal]], object Pascal defines the types <code>byteBool</code>, <code>wordBool</code> and <code>longBool</code>, having a <code>sizeOf</code> one, two, or four bytes respectively.
They were introduced primarily to ease interfacing with code written in other languages, such as [[#C|C]].
These types only have <code>ord(false)</code> defined as zero, and ''any'' other ordinal value represents <code>true</code>.

Nonetheless, <code>Boolean</code> is ''the'' preferred type.

''See also [[#Delphi|Delphi]] and [[#Free Pascal|Free Pascal]]''


## OCaml


OCaml defines a built-in data type <code>bool</code>, which has exactly two members, represented by the keywords <code>true</code> and <code>false</code>:


```ocaml
type bool = false | true
```


In addition to all the functionality of any other OCaml algebraic data type (e.g. [[pattern matching]]), and the functionality of any other OCaml data type (e.g. comparisons <code>false = false</code>, <code>false < true</code>, etc.), <code>bool</code> is also used in the guards in pattern matching (“<code>when</code>”) and <code>if</code> and <code>while</code> syntaxes.

As with any other OCaml data type, there are no automatic conversions of other types to <code>bool</code>.


## Octave

Octave uses <tt>true</tt> (1) and <tt>false</tt> (0). The class of a variable holding a boolean value is ''logical'', which however can be casted to a numeric class, so that <code>r = true; r * 2</code> gives 2 as result. Any non-zero value is interpreted as true, and 0 as false.


## Oforth

Oforth uses <tt>true</tt> (1) and <tt>false</tt> (0)

Any non-zero value is interpreted as true, and 0 as false.


## Ol


<b>#true</b> is True, <b>#false</b> is False; <b>#t</b> is synonym for #true, <b>#f</b> is synonym for #false.

In conditionals everything that is not #false is True.

p.s. Empty lists - '() - in conditionals is True.


## ooRexx


<tt>.true</tt> or <tt>1</tt> are true, <tt>.false</tt> or <tt>0</tt> are false


## Order

Order supplies the keywords <code>8true</code> and <code>8false</code>. Other types are not supposed to automatically convert to any boolean value (in practice some may do so due to implementation quirks, but this is not reliable).


## Oz

<tt>true</tt> and <tt>false</tt> are the only boolean values. No other values are automatically converted to bool.


## PARI/GP

Generally, false is 0 and true is nonzero. Certain other values also behave as false, like the vector [0]. Built-in boolean functions use 0 and 1 (but note that some functions like <code>ispower</code> are not boolean!).

The details of what is considered true or false is contained in see the function <tt>gequal0</tt>:
* An integer (t_INT), polynomial (t_POL), power series (t_SER), or element of a finite field (t_FFELT) is false if and only if it is 0.
* A real number (t_REAL) or complex number (t_COMPLEX) is false if and only if its absolute value rounds to 0 at the object's precision. Note that this can make nonzero complex numbers (with tiny norm) false.
* An integer mod m (t_INTMOD) is false if and only if its residue class is 0, i.e., if <code>lift(x)</code> is 0.
* A ''p''-adic number (t_PADIC) is false if and only if it is zero up to the object's ''p''-adic precision is 0, i.e., if <code>lift(x)</code> is 0.
* A vector (t_VEC), column vector (t_COL), or matrix (t_MAT) is false if and only if all of its components are 0. Note that <code>[]</code> is thus false.
* t_QUAD, t_POLMOD, t_RFRAC


## Pascal

Pascal defines the type <code>Boolean</code> as a “special” enumeration type with exactly two elements:
<code>false</code> and <code>true</code>.
It is guaranteed that <code>ord(false)</code> is <code>0</code> and <code>ord(true)</code> is <code>1</code>.

There is no automatic conversion from integer values to Boolean values, as it is prevalent in many other languages.
Instead, one has to write a Boolean expression, for example <code>myInteger &lt;&gt; 0</code> in order to get an assignment-compatible type.

''See also [[#Delphi|Delphi]], [[#Free Pascal|Free Pascal]], and [[#Object Pascal|Object Pascal]]''


## Perl



```perl
my $x = 0.0;
my $true_or_false = $x ? 'true' : 'false';     # false
```

or

```perl
my $x = 1;          # true

my $true_or_false;

if ($x) {
    $true_or_false = 'true';
}
else {
    $true_or_false = 'false';
}
```

The values in Perl that are false are: <tt>0</tt> (as a number (including <tt>0.0</tt>), or as the string <tt>'0'</tt>, but '''not''' the string <tt>'0.0'</tt>), the empty string <tt><nowiki>''</nowiki></tt>, the empty list <tt>()</tt>, and <tt>undef</tt>. Everything else is true. See [http://perldoc.perl.org/perlsyn.html#Truth-and-Falsehood perlsyn].


###  Short circuit evaluations


Boolean comparison of zero against itself gives a value of one, but Perl uses short circuit evaluations, so any true or false value may be returned from a boolean expression:


```perl
print (7 && 2);  # 2, rather than 1(true)
print (2 && 7);  # 7, rather than 1(true)
print (7 xor 2); # empty string, rather than 0(false)
print ('apples' && 'pears');  # pears, rather than 1(true)
print ('apples' xor 'pears'); # empty string, rather than 0(false)
```



###  Objects


Objects may break these rules at will via [http://perldoc.perl.org/overload.html#*-_Boolean%2c-string-and-numeric-conversion_ overloading].


###  There are no keywords for true and false


Perl has no builtin "true" or "false" keywords. This is a caveat, because true and false are bareword strings and evaluate to true:


```perl
# This does not work
# true and false are not special so will be treated as bareword strings
if (true) { print "true is true\n" };            # This prints
if (false) { print "false is true\n" };          # So does this
if (spongebob) { print "spongebob is true\n" };  # A bareword string
```



###  Special cases


As a special case, literal <tt>1</tt>s and <tt>0</tt>s will never cause a "Useless use of a constant in void context" warning. Another special case worth pointing out here is that the string <tt>'0 but true'</tt> won't provoke a warning if it's used as a number.


## Perl 6

{{works with|Rakudo|2015.12}}

Perl 6 provides an enumeration <code>Bool</code> with two values, <code>True</code> and <code>False</code>. Values of enumerations can be used as ordinary values or as mixins:


```perl6
my Bool $crashed = False;
my $val = 0 but True;
```


For a discussion of Boolean context (i.e. how Perl decides whether something is true or false), see [http://perlcabal.org/syn/S02.html#Context Synopsis 2].


## Phix

Zero is false, any other number is true. Attempting to use a string or sequence as a boolean is assumed to be a programming logic blunder and causes a fatal run-time error.

Conditions such as <code>if length(s) then</code> are permitted, but the more explicit <code>if length(s)!=0 then</code> is preferred. Comparison operators evaluate to 1(true) or 0(false).
A boolean test is inverted by preceding it with the keyword <code>not</code>. The null character ('\0') is considered false, all other characters are deemed true.
The builtin constants TRUE/FALSE and their aliases True/true/False/false may also be used.


## PHP

The values in PHP that are false are: <tt>FALSE</tt>, <tt>NULL</tt>, the number <tt>0</tt> (as an integer <tt>0</tt>, float <tt>0.0</tt>, or string <tt>'0'</tt>, but '''not''' the string <tt>"0.0"</tt>), the empty string <tt>""</tt>, the empty array <tt>array()</tt>, and "SimpleXML objects created from empty tags"(?).

Everything else is true. The keyword <tt>TRUE</tt> exists.
[http://www.php.net/manual/en/language.types.boolean.php#language.types.boolean.casting]


## PicoLisp

Like in all Lisps, the symbol 'NIL' denotes "false", any other value "true". PicoLisp also uses NIL as the empty list, so the empty list is false.

Some functions return the symbol 'T' for "true" if no other useful (non-NIL)
value is available in the given context. Note that 'NIL' and 'T' are written in
uppercase letters (PicoLisp is case-sensitive).


## Pike


```pike

> 0;
(3) Result: 0
> false;
(4) Result: 0
> 0;
(6) Result: 0
> !true;
(7) Result: 0
> true;
(8) Result: 1
> 1;
(9) Result: 1
>

```



## PL/I

True is <code>'1'b</code> and false is <code>'0'b</code>.

```pli
Declare x bit(1);
x='1'b; /* True */
x='0'b; /* False */
```

Using the macro facility one can define reasonable symbols for true and false

```pli
*process source attributes xref macro or(!);
 tf: proc options(main);
 %Dcl true char; %true='''1''b';
 %Dcl false char; %false='''0''b';
 If true Then
   Put Skip list('That''s true');
 If false Then
   Put Skip List('ERROR');
 Else
   Put Skip List('false was recognized');
 End;
```

{{out}}

```txt
That's true
false was recognized
```



## Pony

Boolean values are <code>true</code> and <code>false</code>. Conditions must have type Bool, i.e. they are always true or false.


## PowerBASIC


In addition to what's noted above under [[#BASIC|BASIC]], [[PowerBASIC for Windows]] and [[PowerBASIC Console Compiler]] have the <code>ISTRUE</code> and <code>ISFALSE</code> functions. According to the help file, they "return the logical truth or falsity of a given expression". ([[PowerBASIC]] lacks a boolean data type, so the usual practice is to use integers in [[PB/DOS]], and longs in [[PB/CC]] and [[PB/Win]].)


```powerbasic
DIM x AS LONG
x = ISTRUE(1 = 1)  ' returns -1
x = ISTRUE(1 = 0)  ' returns 0
x = ISFALSE(1 = 1) ' returns 0
x = ISFALSE(1 = 0) ' returns -1
```



## PostScript


Predefined constants are:

```postscript
true
false
```



## PowerShell

Two automatic variables exist for this purpose:

```powershell
$true
$false
```

However, nearly everything can be converted to a boolean value, as detailed in the following list:
* any non-zero number evaluates to '''true''', zero evaluates to '''false'''
* any non-empty string evaluates to '''true''', an empty string evaluates to '''false'''
* an empty array evaluates to '''false'''
* an array containing exactly one item evaluates to whatever the only item evaluates to
* any array with more than one item evaluates to '''true'''
* a reference to any object evaluates to '''true''', <code>$null</code> evaluates to '''false'''


## PureBasic

PureBasic does not have a Boolean variable type.  An integer type is typically used instead.  Boolean values are only supported as part of a loop's condition (While/Wend, Repeat/Until) or in a conditional (If/Endif).  In these cases if the result of a variable or a numeric expression is zero it is evaluated as False, otherwise it is evaluated as True.  A string variable assigned a null string would be evaluated as False.


## Python

Python has a boolean data type with the only two possible values denoted by <code>True</code> and <code>False</code>.

The boolean type is a member of the numeric family of types (specifically a subclass of <code>int</code>), and when used in a numeric, but not boolean context, <code>True</code> has the value one and <code>False</code> the value zero. Also <code>hash(True) == hash(1)</code> and same goes for <code>False</code> and <code>0</code>. Conversely, when numbers are used in a boolean context, zero is false and anything other than zero is true. (Note however, that <code>True</code> is equal to <code>1</code>, so for example <code>True + True != True</code>.)

In a boolean context, Python extends what is meant by true and false by accepting empty collection types, such as an empty dict or an empty list as being False, and non-empty collection types as being True, so in an if statement one might branch on a list which would be the same as testing if the list had any contents.

In Python 2, a user-created class that defines a <tt>__nonzero__</tt> method to return False or 0, or whose <tt>__len__</tt> method returns 0, will be treated as <code>False</code>, otherwise the instance is treated as <code>True</code>. In Python 3, the magic method <tt>__nonzero__</tt> has been changed to <tt>__bool__</tt>, which can only return values of type <tt>bool</tt> (not even <tt>int</tt> or <tt>None</tt>).

<code>None</code> is also <code>False</code> in a boolean context.

'''Some examples:'''

```python>>>
 True
True
>>> not True
False
>>> # As numbers
>>> False + 0
0
>>> True + 0
1
>>> False + 0j
0j
>>> True * 3.141
3.141
>>> # Numbers as booleans
>>> not 0
True
>>> not not 0
False
>>> not 1234
False
>>> bool(0.0)
False
>>> bool(0j)
False
>>> bool(1+2j)
True
>>> # Collections as booleans
>>> bool([])
False
>>> bool([None])
True
>>> 'I contain something' if (None,) else 'I am empty'
'I contain something'
>>> bool({})
False
>>> bool("")
False
>>> bool("False")
True
```



## R

Similarly to Octave, R uses <tt>TRUE</tt> and <tt>FALSE</tt>, kept in variable of class logical, which is silently casted to 1 (TRUE) or 0 (FALSE) if used as numeric value. The opposite is also true: the value 0 can be used as FALSE, and non-zero numbers as TRUE.

The values T and F are given the values TRUE and FALSE respectively (for compatibility with S-Plus), though these may be changed to other values by the user.


## Racket


Racket has the standard Scheme Boolean values <tt>#t</tt> and <tt>#f</tt>, and will also accept <tt>#true</tt> and <tt>#false</tt>.  This is a literal syntax, so it can be used anywhere including in quoted positions.  There are also bindings for <tt>true</tt> and <tt>false</tt> (but of course when these are quoted, the result is plain symbols).  Like other Scheme descendants, many conditional constructs treat any non-false value as "truthy." So, for instance,


```Racket
(cond ([(< 4 3) 'apple]
       ['bloggle 'pear]
       [else 'nectarine])
```


... evaluates to <tt>'pear</tt>, because <tt>'bloggle</tt> is not false.


## Raven

Raven considers 0 as <code>FALSE</code>, -1 as <code>TRUE</code>

```Raven
TRUE print
FALSE print
2 1 > print   # TRUE (-1)
3 2 < print   # FALSE (0)
42 FALSE !=   # TRUE (-1)
```



## REBOL

REBOL uses values of type '''logic!''' to represent boolean values. A boolean value can be 'true' or 'false', which also happen to be understood as predefined constants. Other constants are also provided to improve program readability:

{| border="1"
|+ '''logic!''' Constants
! True !! False
|-
| true || false
|-
| yes || no
|-
| on || off
|-
| any [block! series! date! number! string! ...] || none
|}

As the last true value implies, pretty much any other type will evaluate to true. This is important to remember if you're used to a language where the value "0" is considered to be false -- in REBOL, it's true.


## Retro

Zero is false and non-zero is true. Comparison functions return '''-1''' for true and '''0''' for false.


## REXX

The REXX language enforces the values for   ''true''   and   ''false'',   only the two values are valid:
:::* 0     (zero)   [for false]
:::* 1     (one)     [for true]

The following aren't   '''true'''   or   '''false''':
:::* 0.
:::* 0.0
:::* 00
:::* 1.
:::* 1.0
:::* 001
:::* +1
:::* 2
:::*       (a null value, that is, length=0)
:::* any value with a blank before/in/after the value.


### simplistic


```rexx
 true = 1
false = 0
```



### spruced up

Some programmers like to "spruce up" such a simple assignment:

```rexx
true  = (1=1)
false = (1=0)
```



### more exactitudeness


```rexx
true  = (1==1)
false = (1==0)
```



### oblique


```rexx
true  = (1==1)
false = \true
```

[The parentheses aren't necessary in all of the above versions.]


Some REXX interpreters allow the   ''NOT''   (<code>¬</code>) character for negation:

```rexx
false = ¬true
```



### esoteric


```rexx
true  =            1984  =  1984
false =           'war'  =  'peace'
false =       'freedom'  =  'slavery'
false =     'ignorance'  =  'strength'
```

Of course, in Orwellian terms, the above   '''false'''   statements are   '''true''',   but REXX isn't an Eric Arthur Blair reader.





## Ring


```ring

x = True
y = False
see "x and y : " + (x and y) + nl
see "x or y : " + (x or y) + nl
see "not x : " + (not x) + nl

```



## Ruby

The only values in Ruby that are false are: <code>false</code> and <code>nil</code>.  They have synonyms <code>FALSE</code> and <code>NIL</code>.

Everything else is true.  Constants <code>true</code> (and <code>TRUE</code>) exist.  Note for Python and Perl users: unlike Python, in Ruby, the number <code>0</code>, the empty string, the empty array, and the empty hash, etc. are all true; you can instead use the <code>zero?</code> method to test for 0, and the <code>.empty?</code> method to test for an empty sequence.

<code>false</code>, <code>nil</code> and <code>true</code> are singleton instances of classes <code>FalseClass</code>, <code>NilClass</code> and <code>TrueClass</code> respectively.
[http://www.ruby-doc.org/docs/ProgrammingRuby/html/tut_expressions.html#UF]


## Run BASIC

Basically 0 is false and 1 is true

```runbasic
if 1          then print "1 is true"
if not(0)     then print "0 is false"
if 1 < 2      then print "1 < 2 TRUE"
if 2 > 1      then print "2 > 1 TRUE"
if not(2 < 1) then print "2 not < 1"
if not(1 = 0) then print "1 not = 0"
```



## Rust

Booleans are expressed using the <code>bool</code> type and the two literals <code>true</code> and <code>false</code>.
The compiler prevents any other values from being assigned to a bool variable.
This behavior is not guaranteed inside blocks labeled as unsafe, where it is the programmers responsibility to ensure this behavior.


## Sather

The BOOL type can be <code>true</code> or <code>false</code>. Sather never implicitly does casting of a type in another, so numeric value or other types cannot be used (implicitly) as boolean value; nonetheless an explicit "cast" can be done:


```sather
v:BOOL := true; -- ok
i:INT := 1;
v := 1; -- wrong
if i then ... end; -- wrong: if requires a bool!
-- BUT
v := 1.bool; -- ok
if i.bool then ... end; -- ok
```


In this case, <code>0.bool</code> is false, and <code>n.bool</code> with n not equal to 0 is true.


## Scala

Booleans in Scala are given by the literals <code>true</code> and <code>false</code>, case sensitive, which are the only instances of the class <code>Boolean</code>.


## Scheme

The only value in Scheme that is false is <tt>#f</tt>.

Everything else (including the empty list, unlike Lisp) is true. The constant <tt>#t</tt> represents the canonical true value.


## Seed7

Seed7 defines the type <tt>boolean</tt>. The only values of <tt>boolean</tt> are <tt>TRUE</tt> and <tt>FALSE</tt>. There are no automatic conversions from any other types into <tt>boolean</tt>, and it is a compile-time error to use any type other than <tt>boolean</tt> in a place that expects a <tt>boolean</tt> (e.g. if-statement condition, while-statement condition, operand of a logical operator, etc.).


## Self


Self has two objects, ''true'' and ''false''.


## Sidef

Sidef defines the ''true'' and ''false'' boolean values, which are part of the ''Bool'' type.

```ruby
var t = true;
var f = false;
```


In conditional expressions, anything that evaluates to zero or nothing is considered ''false'', including empty arrays and empty hashes.

```ruby
if (0 || "0" || false || nil || "" || [] || :()) {
    say "true"
} else {
    say "false";
}
```

{{out}}

```txt
false
```



## Simula

Simula has <tt>true</tt> and <tt>false</tt> keywords, representing the only values of type <tt>boolean</tt>. There are no automatic conversions from any other types into <tt>boolean</tt>, and it is a compile-time error to use any type other than <tt>boolean</tt> in a place that expects a <tt>boolean</tt> (e.g. if-statement condition, while-statement condition, operand of a logical operator, etc.).


## Slate

Use <tt>True</tt> or <tt>False</tt>.


## Smalltalk

Smalltalk uses the Boolean class, which has two subclasses (True and False). <tt>true</tt> and <tt>false</tt> are singleton instances of those classes. E.g. an expression like <tt>5 = 5</tt> returns <tt>true</tt>.


## smart BASIC

<ul>
<li>IF/THEN statements treat any non-zero value as true and zero as false.</li>
<li>Comparison operators DO NOT calculate in smart BASIC. For example, PRINT (1 > 4) WILL NOT evaluate to produce a zero value (0) indicating a false condition. It will produce an error. All Boolean evaluations in smart BASIC must be determined within IF/THEN statements. </li>
</ul>


## SNUSP

Zero is false and non-zero is true, as used by the sole skip-if-zero operator ('''?''').

```snusp
$!/?\=false= + =true=#
  \-/
```



## SPL

In SPL zero is false, any other value is true.


## Standard ML


Standard ML defines a top-level data type <code>bool</code>, which has exactly two members, <code>true</code> and <code>false</code>:


```sml
datatype bool = false | true
```


In addition to all the functionality of any other Standard ML algebraic data type (e.g. [[pattern matching]], equality <code>false = false</code>), <code>bool</code> is also used in <code>if</code> and <code>while</code> syntaxes.

As with any other Standard ML data type, there are no automatic conversions of other types to bool.


## Stata

Stata uses the values 0 for "false" and 1 for "true". In expressions involving boolean operators, any nonzero numeric value (including missing values) is considered true.


## Swift

Swift defines a built-in data type <code>Bool</code>, which has two values, represented by the keywords <code>true</code> and <code>false</code>. There is no conversion between booleans and other data types. Conditionals require a type that conforms to the <code>BooleanType</code> protocol, which provides a conversion to <code>Bool</code> for that type; types that conform include <code>Bool</code> and some other types.


## Tcl

;True values:
:<tt> 1</tt> (or other non-zero number, e.g., <tt>42</tt>)<tt>, true, yes, on</tt>
;False values:
:<tt> 0</tt> (or other zero number, e.g., <tt>0.0</tt>)<tt>, false, no, off</tt>
Any of these values may be abbreviated, and mixed-case spellings are also acceptable. [http://www.tcl.tk/man/tcl8.5/TclLib/GetInt.htm]
Any other value gives an error.  In an interactive tclsh session:

```tcl
% if {""} then {puts true} else {puts false}
expected boolean value but got ""
```


Test for the boolean value of a string can be stuff like

```tcl
if {[string is false -strict $string]} ...
```

which will test for "no" or "NO" or "0" or "False" or ...

=={{header|TI-89 BASIC}}==

There are boolean literals <code>true</code> and <code>false</code>. No other values may be used where a boolean is expected.


## Trith

The boolean constants are ''true'' and ''false''. In a conditional context, the only false values are ''false'' and ''nil'' -- every other value is true.


## uBasic/4tH

In conditionals, zero is false, non-zero is true. Note that '''=''' is not only used for assignment, it is also a fully qualified logical operator, so it is easy to assign a true boolean to a variable.
<lang>t = 1 = 1
f = 0 = 1

Print "False = ";f;", True = ";t
```

{{out}}

```txt
False = 0, True = 1

0 OK, 0:52
```



## UNIX Shell


The traditional Bourne shell does not provide a reserved keyword for true or false.

Truth is determined by exit codes rather than values

The evaluation of true and false within the shell is different to the evaluation of truth from within a high level language. Within the shell, a truth is based on the exitcode of the last command in the evaluation block:

* An exitcode of zero is considered to be a true condition
* An exitcode of nonzero is considered to be a false condition

In the following example, after running the test command, the then syntactical component runs the optional branch if an exitcode is of zero determined:


```sh
if
  echo 'Looking for file'  # This is the evaluation block
  test -e foobar.fil       # The exit code from this statement determines whether the branch runs
then
  echo 'The file exists'   # This is the optional branch
  echo 'I am going to delete it'
  rm foobar.fil
fi
```


In some later shells, the values ''true'' and ''false'' are defined, respectively, as a return code of 0 and a return code of greater-than zero.  While there are built-in functions for each of these values, booleans are most commonly the result of a test or a process termination.

{{works with|bash}}
{{works with|ksh}}


```Bash
true && echo "true" || echo "false"
```



## Ursa

Ursa has the boolean data type which can be declared using the declare (or decl) function.

```ursa>decl boolean bool</lang

Boolean values can be set to either true or false, or the result of an expression.

```ursa
set bool true
# same as
set bool (= 2 2)

```

or

```ursa
set bool false
# same as
set bool (not (= 2 2))

```



## VBA

VBA has a boolean type. As an integer False is 0 and anything else is True. However True converts to -1. Booleans are False by default.

```vb
Dim a As Integer
Dim b As Boolean
Debug.Print b
a = b
Debug.Print a
b = True
Debug.Print b
a = b
Debug.Print a
```

{{out}}
Output of above lines:
```txt

False
0
True
-1
```



## VBScript

VBScript has the boolean subdatatype and also the two constants <code>True</code> and <code>False</code>.
When converting an integer to a boolean, <code>0</code> is <code>False</code> and anything not equal to <code>0</code> is <code>True</code>.

```vb

a = True
b = False
Randomize Timer
x = Int(Rnd * 2) <> 0
y = Int(Rnd * 2) = 0
MsgBox a & " " & b & " " & x & " " & y
```

{{out}}

```txt

True False True False

```



## Visual Basic


VB has the <code>Boolean</code> data type and the constants <code>True</code> and <code>False</code>, in addition to what's listed under [[#BASIC|BASIC]], above. When used outside of a boolean context, <code>True</code> and <code>False</code> return values depending on their context -- <code>-1</code> and <code>0</code> in a numeric context, <code>"True"</code> and <code>"False"</code> if used as strings.

When converting an integer to a boolean, <code>0</code> is <code>False</code> and anything not equal to <code>0</code> is


```vb
Dim x As Boolean
x = IIf(Int(Rnd * 2), True, False)
MsgBox x
```



## Vim Script

A non-zero <code>Number</code> is true, 0 is false.

Since a <code>String</code> is converted automatically to a <code>Number</code> when necessary, the following will print "false" because "foo" is converted to 0:

```vim
if "foo"
    echo "true"
else
    echo "false"
endif
```



## WDTE


WDTE has a built-in boolean type, the two values of which are exposed by the <code>std</code> package's <code>true</code> and <code>false</code> functions. In general, however, built-in conditional functionality, such as <code>switch</code> expressions, considers any value that is not <code>true</code> to be <code>false</code>.


```wdte>let io =
 import 'io';
let ex => switch 'this is irrelevant for this example' {
  false => 'This is, obviously, not returned.';
  'a string' => 'This is also not returned.';
  true => 'This is returned.';
};
ex -- io.writeln io.stdout;
```


The above prints "This is returned."


## XLISP

Boolean "false" may be represented by <tt>#F</tt>, <tt>#!FALSE</tt>, <tt>NIL</tt>, or the empty list; any other value is counted as true in conditional expressions, but it is also possible to represent the Boolean value "true" using your choice of the symbols <tt>#T</tt>, <tt>#!TRUE</tt>, and <tt>T</tt>. All these symbols are case-insensitive. Note that <tt>T</tt>, unlike the others, is a variable: it is bound by default to the constant <tt>#T</tt>, but you can (although you shouldn't) assign it any other value including "false" (by doing something like <tt>(setq t nil)</tt>). Boolean values are printed as <tt>#T</tt> and <tt>()</tt>.


## XPL0

An integer value equal to 0 is false, and a value not equal to 0 is true.
Relational operations evaluate to 0 for false and -1 for true. The
command word 'true' equals -1, and 'false' equals 0.


## XSLT


```xml
<xsl:if test="true() or false()">
  True and false are returned by built-in XPath functions.
</xsl:if>
<xsl:if test="@myAttribute='true'">
  Node attributes set to "true" or "false" are just strings. Use string comparison to convert them to booleans.
</xsl:if>
<xsl:if test="@myAttribute or not($nodeSet)">
  Test an attribute for its presence (empty or not), or whether a node set is empty.
</xsl:if>
```



## zkl


```zkl
a:=True;
b:=False;
True.dir();
```

{{out}}

```txt

True : Bool
Methods:
   -- BaseClass Method Property __sGet __sSet copy create dir fp fp1 fp2
   fpM fpN isChildOf isInstanceOf isType len method noop print println
   property resolve toBool toData toFloat toInt toList toString toType
Properties:
   -- createReturnsSelf fullName id isContainer isThreadSafe itype methods
   name numObjects oID otype properties size type typeID vaultPath

```



## zonnon


```zonnon


var
	a,b,c: boolean;
begin
	a := false;
	b := true;
	c := 1 > 2;
...

```

{{omit from|ML/I}}

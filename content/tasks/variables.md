+++
title = "Variables"
description = ""
date = 2019-10-18T20:46:03Z
aliases = []
[extra]
id = 4470
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "360_assembly",
  "8th",
  "ada",
  "algol_68",
  "algol_w",
  "apex",
  "applescript",
  "applesoft_basic",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "awk",
  "axe",
  "basic",
  "batch_file",
  "bbc_basic",
  "boo",
  "bracmat",
  "c",
  "chuck",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dm",
  "dwscript",
  "dyalect",
  "e",
  "eiffel",
  "ela",
  "elena",
  "erlang",
  "factor",
  "falcon",
  "forth",
  "fortran",
  "gap",
  "glovepie",
  "go",
  "haskell",
  "hicest",
  "holyc",
  "icon",
  "j",
  "java",
  "javascript",
  "joy",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lingo",
  "logo",
  "lotusscript",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "mercury",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "openscad",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pony",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rascal",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "seed7",
  "set_lang",
  "smart_basic",
  "snobol4",
  "spl",
  "ssem",
  "supercollider",
  "swift",
  "tcl",
  "tuscript",
  "txr",
  "ubasic_4th",
  "unicon",
  "unix_shell",
  "ursa",
  "vba",
  "vbscript",
  "visual_basic",
  "visual_basic_dotnet",
  "wdte",
  "xpl0",
  "xslt",
  "zkl",
]
+++

## Task

Demonstrate a language's methods of:
:::*   variable declaration
:::*   initialization
:::*   assignment
:::*   datatypes
:::*   scope
:::*   referencing,     and
:::*   other variable related facilities





## 360 Assembly

;assignment, reference, referencing:

```360asm
*        value of F
         L      2,F                assigment      r2=f
*        reference (or address) of F
         LA     3,F                reference      r3=@f
*        referencing (or indexing) of reg3       (r3->f)
         L      4,0(3)             referencing    r4=%r3=%@f=f
```

;declarations, initialization, datatypes:

```360asm
*        declarations                             length
C        DS     C                  character        1
X        DS     X                  character hexa   1
B        DS     B                  character bin    1
H        DS     H                  half word        2
F        DS     F                  full word        4
E        DS     F                  single float     4
D        DS     D                  double float     8
L        DS     L                  extended float  16
S        DS     CL12               string          12
P        DS     PL16               packed decimal  16
Z        DS     ZL32               zoned  decimal  32
*        declarations + initialization
CI       DC     C'7'               character        1
XI       DC     X'F7'              character hexa   1
BI       DC     B'11110111'        character bin    1
HI       DC     H'7'               half word        2
FI       DC     F'7'               full word        4
EI       DC     F'7.8E3'           single float     4
DI       DC     D'7.8E3'           double float     8
LI       DC     L'7.8E3'           extended float  16
SI       DC     CL12'789'          string          12
PI       DC     PL16'7'            packed decimal  16
ZI       DC     ZL32'7'            zoned decimal   32
```

;scope
In BAL (Basic Assembler Language), variables are global and there is no scope.


## 8th

There is only one type of variable in 8th, and it is simply a single-item container which can contain any of the known data-types:

```forth

\ declare a variable which is initialized to the number '0'
var x

\ declare a variable which is initialized to a string "cat"
"cat" var, y

\ Get the value in x, add 20 and store it:
x @ 20 n:+ x !

\ Change the cat to a dog:
"dog" y !

```



## Ada


```ada
Name: declare   -- a local declaration block has an optional name
   A : constant Integer := 42;  -- Create a constant
   X : String := "Hello"; -- Create and initialize a local variable
   Y : Integer;           -- Create an uninitialized variable
   Z : Integer renames Y: -- Rename Y (creates a view)
   function F (X: Integer) return Integer is
     -- Inside, all declarations outside are visible when not hidden: X, Y, Z are global with respect to F.
     X: Integer := Z;  -- hides the outer X which however can be referred to by Name.X
   begin
     ...
   end F;  -- locally declared variables stop to exist here
begin
   Y := 1; -- Assign variable
   declare
     X: Float := -42.0E-10;  -- hides the outer X (can be referred to Name.X like in F)
   begin
     ...
   end;
end Name; -- End of the scope
```



## ALGOL 68

Local variables are generally called '''local''' variables in ALGOL 68. Variables must be declared before use. In traditional ALGOL 68, variables must be declared before any '''labels:''' in a '''compound-clause'''. The declaration of a variable, without assigning a value takes the form: <b><typename></b> <i><variablename>;</i>

```algol68>int j;</lang

Some common types are: '''char''', '''string''', '''short int''', '''int''', '''long int''', '''real''', '''long real''', '''bits''' and '''bytes'''  .

Multiple variables may be defined in a single statement as follows:

```algol68
LONG REAL double1, double2, double3;
```

It is possible to initialize variables with expressions having known values when they are defined.
The syntax follows the form <i><typename> <variablename> := <initializing expression>;</i>

```algol68
SHORT INT b1 := 2500;
LONG INT elwood = 3*bsize, jake = bsize -2;
```

The '''string'''s in ALGOL 68 are '''flex''' arrays of '''char'''. To declare initial space for a string of exactly to 20 characters, the following declaration is used.

```algol68
FLEX[20]CHAR mystring;
```

All arrays are structure that include both the lower '''lwb''' and upper '''upb''' of the array.  Hence '''string'''s in ALGOL 68 may safely contain ''null character''s and can be reassigned with longer or shorter strings.

To declare an initialized string that won't be changed the following declaration may be used:

```algol68
[]CHAR mytext = "The ALGOL 68 Language";
```

There are more rules regarding arrays, variables containing pointers, dynamic allocation,
and initialization that are too extensive to cover here.


## ALGOL W

Algol W is block structured, following the rules established by Algol 60.
There are variables of the following basic types:
integer, real, long real, complex, long complex, bits, logical, string.

Logical variables (usually called boolean in other languages) hold true/false values.
Bits variables hold bit strings.
Strings are fixed length, between 1 and 256 characters long. The length is part of the type.
case is not significant in variable names.

Declarations must appear at the beginning of the block they are contained in,
before the first executable statement.
Declaration is separate from initialisation - there is no separate initialisation syntax (except for fields of a record - see below), normal assignments are used:

```algolw
% declare some variables %
integer a1, a2; real b; long real c; complex d; long complex f;
logical g; bits h; string(32) j;

% assign "initial values" %
f := d := c := b := a2 := a1 := 0; % multiple assignment %
g := false; h := #a0; j := "Hello, World!";

```


Records can be declared, composed of fields of the basic types and references to records. E.g.:

```algolw
record R1 ( integer length; string(256) text );
reference(R1) ref1, ref2;
```

In the above, R1 is a structure containing an integer and a string. Ref1 and ref2 are variables that will refer to instances of the R1 structure.
References can be declared that can refer to a number of different record structures. The allowable references must be specified in the declaration E.g.:

```algolw
record person( string(32) name; integer age );
record date( integer day, month, year );
reference(person, date) ref3;
```

In the above, ref3 can hold references to either a person or a date.
Variables that are references to the basic types are not allowed. E.g.:

```algolw
reference(integer) refInt; % an illegal declaration %
```

The following could be used instead:

```algolw
record INT_VALUE ( integer val );
reference(INT_VALUE) refInt;
```


Fields are referred to via a function-like notation, e.g.:

```algolw
% using the person record defined above...%
    reference (person) someone;
    someone := person           % create a new person structure with uninitialised fields %
    name(someone) := "Fred";    % initialise the fields %
    age(someone)  := 27;
    % could also initialise the fields when the record is created: %
    someone := person( "Harry", 32 );
```


Arrays of the basic types and references can also be declared, but records cannot contain arrays.
There are no procedure variables though procedures can be passed as parameters to other procedures, as can arrays, references and the basic types.
Procedures can return basic types and references.


## Apex


```Apex

// If not initialized at class/member level, it will be set to null
Integer x = 0;
Integer y; // y is null here
Integer p,q,r; // declare multiple variables
Integer i=1,j=2,k=3; // declare and initialize

/*
 * Similar to Integer, below variables can be initialized together separated by ','.
*/
String s = 'a string';
Decimal d = 0.0;
Double dbl = 0.0;
Blob blb = Blob.valueOf('Any String');
Boolean b = true;
AClassName cls = new AClassName();

```



## AppleScript

Variables are untyped in AppleScript, but they must be instantiated before use.
Example:
```AppleScript>set x to 1</lang

Scope may be explicitly defined before instantiation using either the <code>global</code> or <code>local</code> declarations.
```AppleScript
global x
set x to 1
local y
set y to 2
```
If undeclared, AppleScript will automatically set the scope based on the following rule: variables declared at the top level of any script will be (implicit) globals, variables declared anywhere else will be (implicit) locals. Scope cannot be changed after being explicitly or implicitly defined.
Where a variable has both local and global instances, it is possible to use the <code>my</code> modifier to access the global (top-level) instantiation.
Example:
```AppleScript
on localx()
	set x to 0 -- implicit local
	return x
end localx

on globalx()
	set x to 0 -- implicit local
	return my x
end globalx

on run
	set x to 1 -- top-level implicit global
	return {localx(), globalx()}
end run
--> RETURNS: {0, 1}
```


Applescript also supports top-level entities known as <code>properties</code> that are global to that script.
Example:
```AppleScript>property x : 1</lang
Properties behave exactly as global variables except that they are persistent. Their most recent values are retained between script executions (or until the script is recompiled).


## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program variable.s   */

/************************************/
/* Constantes Définition            */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/*********************************/
/* Initialized data              */
/*********************************/
.data
szString:         .asciz "String définition"
sArea1:            .fill 11, 1, ' '            @ 11 spaces
     @ or
sArea2:            .space 11,' '               @ 11 spaces

cCharac:          .byte '\n'                   @ character
cByte1:            .byte 0b10101               @ 1 byte binary value

hHalfWord1:       .hword   0xFF                @ 2 bytes value hexa
.align 4
iInteger1:      .int 123456                    @  4 bytes value decimal
iInteger3:      .short 0500                    @  4 bytes value octal
iPointer1:      .int 0x4000                    @   4 bytes value hexa
     @ or
iPointer2:      .word 0x4000                   @   4 bytes value hexa
iPointer3:       .int  04000                   @   4 bytes value octal

TabInteger4:     .int  5,4,3,2                 @ Area of 4 integers = 4 * 4 = 16 bytes

iDoubleInt1:     .quad  0xFFFFFFFFFFFFFFFF     @  8 bytes

dfFLOAT1:       .double 0f-31415926535897932384626433832795028841971.693993751E-40 @  Float 8 bytes
sfFLOAT2:       .float  0f-31415926535897932384626433832795028841971.693993751E-40 @  Float 4 bytes (or use .single)

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
sBuffer:         .skip  500                       @ 500 bytes values zero
iInteger2:       .skip 4                          @ 4 bytes value zero
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program
    ldr r0,iAdriInteger2                          @ load variable address
    mov r1,#100
    str r1,[r0]                                   @ init variable iInteger2

100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdriInteger2:             .int iInteger2         @ variable address iInteger2


```



## Arturo



```arturo
num 10
str "hello world"

arrA 1 2 3
arrB "one" "two" "three"
arrC 1 "two" #(3 4 5)
arrD #("one" true { print "something"})

dict #{
	name "john"
	surname "doe"
	func {
		print "do sth"
	}
}
```



## AutoHotkey


```autohotkey
x = hello       ; assign verbatim as a string
z := 3 + 4      ; assign an expression
if !y       ; uninitialized variables are assumed to be 0 or "" (blank string)
Msgbox %x%  ; variable dereferencing is done by surrounding '%' signs
fx()
{
local x   ; variable default scope in a function is local anyways
global y  ;
static z=4  ; initialized once, then value is remembered between function calls
}
```



## AWK


### Dynamic variables


```awk
BEGIN {
# Variables are dynamically typecast, and do not need declaration prior to use:
  fruit = "banana"    # create a variable, and fill it with a string
  a = 1               # create a variable, and fill it with a numeric value
  a = "apple"         # re-use the above variable for a string
  print a, fruit

# Multiple assignments are possible from within a single statement:
  x = y = z = 3
  print "x,y,z:", x,y,z

# "dynamically typecast" means the content of a variable is used
# as needed by the current operation, e.g. for a calculation:
  a = "1"
  b = "2banana"
  c = "3*4"

  print "a,b,c=",a,b,c,  "c+0=", c+0, 0+c
  print "a+b=", a+b, "b+c=", b+c
}
```

```txt

apple banana
x,y,z: 3 3 3
a,b,c= 1 2banana 3*4 c+0= 3 3
a+b= 3 b+c= 5

```

Numeric values are always real, there is no separate datatype for integer.

awk also has [[Arrays#AWK|Arrays]].


### Assignment on commandline

It is possible to assign value to variables from the commandline:

```awk
# usage:  awk -v x=9  -f test.awk
BEGIN {
  y = 3
  z = x+y
  print "x,y,z:", x,y,z
  printf( "x=%d,y=%d,z=%d:", x,y,z )
}
```

{{Out}} with "-v x=9":

```txt

x,y,z: 9 3 12
x=9, y=3, z=12

```

{{Out}} without "-v" (x stays empty, which counts as zero in calculations):

```txt

x,y,z:  3 3
x=0, y=3, z=3

```



### Global scope

Variables have global scope, and there is no way to make a variable local to a block.

However, function arguments are local,
so it is possible to make a variable local to a function

by listing the variable as an additional dummy function argument
after the required arguments:

```awk
function foo(s,  k) {
  # s is an argument passed from caller
  # k is a dummy not passed by caller, but because it is
  # in the argument list, it will have a scope local to the function
  k = length(s)
  print "'" s "' contains", k, "characters"
}
BEGIN {
  k = 42
  s = "Test"
  foo("Demo")
  print "k is still", k
  foo(s,k)
  print "k still is", k
}
```

```txt

'Demo' contains 4 characters
k is still 42
'Test' contains 4 characters
k still is 42

```



### Builtin variables

There are some [[Special_variables#AWK|special variables]]:

When reading a line of input, the contents of that line
are automatically split into a number of variables:
* $0 is the whole input-line
* $1 is the first field
* $2 is the 2nd field, etc.
* NF is the number of fields
*$NF is the contents of the last field.

```awk
# Feeding standard-input with echo:
echo -e "2 apples 0.44$ \n 3 banana 0.33$" | awk '{p=$1*$NF; sum+=p; print $2,":",p; }; END{print "Sum=",sum}'
```

With more data, this would look like
  awk '{p=$1*$NF; sum+=p; print $2,":",p; }; END{print "Sum=",sum}' inputfile
or more typically:
  awk  -f report.awk  inputfile
```txt

apples : 0.88
banana : 0.99
Sum= 1.87

```



## Axe


In Axe, variables are global because they are (mostly) static references to memory locations. This means there is also no scope.

```axe
1→A
```


The letters A-Z and theta are all general-purpose variables. They are located at the end of the L₁ memory region by default, but they can be reallocated to any region of free memory:

```axe
#Realloc(L₂)
```


The variables r₁ through r₆ are used for function arguments. They are automatically set as the arguments passed to the function. However, they behave otherwise just like normal variables.

Other variables include pointers such as Str0-Str9, GDB0-GDB9, and Pic0-Pic9. These are effectively constant and are assigned at compile time to point to static data.


## BASIC


In BASIC, variables are global and there is no scope. However, it is an error to reference a variable before it has been assigned.


```gwbasic
10 LET A=1.3
20 LET B%=1.3:       REM The sigil indicates an integer, so this will be rounded down
30 LET C$="0121":    REM The sigil indicates a string data type. the leading zero is not truncated
40 DIM D(10):        REM Create an array of 10 digits
50 DIM E$(5.10):     REM Create an array of 5 strings, with a maximum length of 10 characters
60 LET D(1)=1.3:     REM Assign the first element of d
70 LET E$(3)="ROSE": REM Assign a value to the third string
80 PRINT D(3):       REM Unassigned array elements have a default value of zero
90 PRINT E$(3):      REM Ten spaces because string arrays are not dynamic
100 PRINT E$(3);"TTA CODE": REM There will be spaces between rose and etta
110 DIM F%(10):  REM Integers use less space than floating point values
120 PRINT G:     REM This is an error because f has not been defined
130 PRINT D(0):  REM This is an error because elements are numbered from one
140 LET D(11)=6: REM This is an error because d only has 10 elements
150 PRINT F%:    REM This is an error because we have not provided an element number
160 END
```


=
## Applesoft BASIC
=

In Applesoft BASIC, variables are global and there is no scope. And, it is not an error to reference a variable before it has been assigned.
The LET keyword is optional.  Almost all math is done using floating point numbers by default. Using floating point variables is almost always faster than using integer variables which require extra conversion between floating point and integer.  Integers use less space than floating point values.  Applesoft BASIC array indexes start at zero.


```ApplesoftBasic
 10  A = 1.7: REM LET IS NOT REQUIRED
 20   LET B% = 1.7: REM THE PERCENT SIGN INDICATES AN INTEGER; THIS GETS TRUNCATED DOWN
 30   LET C$ = "0121": REM THE DOLLAR SIGN INDICATES A STRING DATA TYPE. THE LEADING ZERO IS NOT TRUNCATED
 40   DIM D(20): REM CREATE AN ARRAY OF 21 FLOATING POINT NUMBERS
 50   DIM E$(5,10): REM CREATE A TWO DIMENSIONAL ARRAY OF 66 STRINGS
 60   LET D(1) = 1.3: REM ASSIGN THE SECOND ELEMENT OF D
 70  Y$(3) = "ROSE": REM ASSIGN A VALUE TO THE FOURTH STRING
 80   PRINT X: REM UNASSIGNED FLOATING POINT AND INTEGER VARIABLES HAVE A DEFAULT VALUE OF ZERO
 90   PRINT Y$(2): REM UNASSIGNED STRING VARIABLES ARE EMPTY
 100  PRINT Y$(3);"TTA CODE": REM THERE WON'T BE SPACES BETWEEN ROSE AND ETTA
 110 F%(10) = 0: REM IF ARRAYS ARE NOT DECLARED THEY HAVE 11 ELEMENTS BY DEFAULT; IE. DIM F%(10)
 120  PRINT G: REM THIS PRINTS 0 AND IS NOT AN ERROR EVEN THOUGH G HAS NOT BEEN DEFINED
 130  PRINT D(0): REM THIS IS NOT AN ERROR BECAUSE ELEMENTS ARE NUMBERED FROM ZERO.
 140  PRINT F%: REM THIS PRINTS 0 BECAUSE F% IS A DIFFERENT VARIABLE THAN THE ARRAY F%(10)
 150  LET D(21) = 6: REM THIS IS AN ERROR BECAUSE D ONLY HAS 21 ELEMENTS INDEXED FROM 0 TO 20.
```




## Batch File

Batch file variables are not limited to data types and they do not need to be initialized before use.


```dos

@echo off

::setting variables in defferent ways
set myInt1=5
set myString1=Rosetta Code
set "myInt2=5"
set "myString2=Rosetta Code"

::Arithmetic
set /a myInt1=%myInt1%+1
set /a myInt2+=1
set /a myInt3=myInt2+   5

set myInt
set myString
pause>nul

```



## BBC BASIC

```bbcbasic
      REM BBC BASIC (for Windows) has the following scalar variable types;
      REM the type is explicitly indicated by means of a suffix character.
      REM Variable names must start with A-Z, a-z, _ or `, and may contain
      REM any of those characters plus 0-9 and @; they are case-sensitive.

      A& = 123      : REM Unsigned 8-bit byte (0 to 255)
      A% = 12345678 : REM Signed 32-bit integer (-2147483648 to +2147483647)
      A  = 123.45E6 : REM Variant 40-bit float or 32-bit integer (no suffix)
      A# = 123.45E6 : REM Variant 64-bit double or 32-bit integer
      A$ = "Abcdef" : REM String (0 to 65535 bytes)

      REM Scalar variables do not need to be declared but must be initialised
      REM before being read, otherwise a 'No such variable' error is reported
      REM The static integer variables A% to Z% are permanently defined.

      REM BBC BASIC also has indirection operators which allow variable-like
      REM entities to be created in memory:

      DIM addr 7    : REM Allocate 8 bytes of heap
      ?addr = 123   : REM Unsigned 8-bit byte (0 to 255)
      !addr = 12345 : REM Signed 32-bit integer (-2147483648 to +2147483647)
      |addr = 12.34 : REM Variant 40-bit or 64-bit float or 32-bit integer
      $addr = "Abc" : REM String terminated by CR (0 to 65535 bytes)
      $$addr = "Abc": REM String terminated by NUL (0 to 65535 bytes)

      REM The integer indirection operators may be used in a dyadic form:
      offset = 4
      addr?offset = 12345678 : REM Unsigned 8-bit byte at addr+offset
      addr!offset = 12345678 : REM Signed 32-bit integer at addr+offset

      REM All variables in BBC BASIC have global scope unless they are used
      REM as a formal parameter of a function or procedure, or are declared
      REM as LOCAL or PRIVATE.  This is different from most other BASICs.
```



## Bracmat

<strong>Variable declaration.</strong>
Variables local to a function (<code>i</code> and <code>j</code> in the example below) can be declared just before the body of a function.

```bracmat
(myfunc=i j.!arg:(?i.?j)&!i+!j)
```

Global variables are created the first time they are assigned a value.

<strong>Initialization.</strong>
Local variables are initialised to <code>0</code>.

<strong>Assignment.</strong>
There are two ways.

To assign unevaluated code to a variable, you normally would use the <code>&lt;<i>variable</i>>=&lt;<i>unevaluated expression</i>></code> syntax.

To assign an evaluated expression to a variable, you use pattern matching as in <code>&lt;<i>evaluated expression</i>>:?&lt;<i>variable</i>></code>

<strong>Datatypes.</strong> There are no datatypes. The nature of an expression is observed by pattern matching.

<strong>Scope.</strong> Local variables have dynamic scope.

<strong>Referencing.</strong> Variables are referenced using the <code>!&lt;<i>variable</i>></code> syntax.

<strong>Other variable related facilities.</strong>
Global variables (name as well as value) can be removed from memory with the built-in <code>tbl</code> function.
The names of built-in functions such as <code>put</code> and <code>lst</code> can be used as variable names without adverse effects on the built-in function. It is not possible to redefine built-in functions to do something different.


## Boo

<strong>Variable Declaration</strong>
Boo is a statically typed language. Types can either be explicitly declared or inferred after being intitialized
<hr>
<code>
x as int                          // declares an integer


y = 23                           // declares an integer and assigns the value 23


foo as string                 // string declaration


foo = "Boo!"                 // string assignment



pi as double                 // double floating point declaration



stuff = 32.2322             // double assignment
<hr>
</code>


<strong>Boo Variable Types</strong>

sbyte: -128 to 127

short: -32768 to 32767

int: -2147483648 to 2147483647

long: -9223372036854775808 to 9223372036854775807

byte: 0 to 255

ushort:0 to 65535

uint: 0 to 4294967295

ulong: 0 to 18446744073709551615

single: Approximately ±1.5 x 10-45 - ±3.4 x 1038 with 7 significant figures

double: Approximately ±5.0 x 10-324 - ±1.7 x 10308 with 15 significant figures

decimal: Approximately ±1.0 x 10-28 - ±7.9 x 1028 with 28 significant figures

char: Any UTF-16 character

bool: True or False


## C

Local variables are generally called <i>auto</i> variables in C. Variables must be declared before use. The declaration of a variable, without assigning a value takes the form <i><typename> <variablename>;</i>

```c
int j;
```

Some common types are: <i>char, short, int, long, float, double</i> and <i>unsigned</i>.

Multiple variables may be defined in a single statement as follows:

```c
double double1, double2, double3;
```

It is possible to initialize variables with expressions having known values when they are defined.
The syntax follows the form <i><typename> <variablename> = <initializing expression>;</i>

```c
short b1 = 2500;
long elwood = 3*BSIZE, jake = BSIZE -2;
```

Strings in C are arrays of char terminated by a 0 or NULL character. To declare space for a string of up to 20 characters, the following declaration is used.

```c
char mystring[21];
```

The extra length leaves room for the terminating 0.

To declare an initialized string that won't be changed the following declaration may be used:

```c
const char * mytext = "The C Language";
```

There are more rules regarding arrays, variables containing pointers, dynamic allocation,
and initialization that are too extensive to cover here.

=={{header|Caché ObjectScript}}==
Variable type is not declared for local variables.
<lang>set MyStr = "A string"
set MyInt = 4
set MyFloat = 1.3
```


Array variables use a subscript as an element index.<br />
The size is set automatically when a new element is added.<br />
Each element can have an arbitrary number of sub elements.<br />
Arrays are automatically sorted by subscript.<br />

<lang>set MyArray(1) = "element 1"
set MyArray(2) = "element 2"
set MyArray(2,1) = "sub element 1 of element 2"
set MyArray("Element 3") "element indexed by a string"
set MyArray = "Root element"

```


Global variables are persistent. They remain set even after reboot.<br />
Global variables work the same as array variables.<br />
Global variables are indicated with the '^' character.<br />
<lang>
set ^MyGlobal("a subscript") = "My Value"

```

Process private global variables exist for the lifetime of the process, can only be accessed by the process that instantiated it and are indicated as follows:
<lang>
set ^||MyProcessPrivateGlobal("subscript 1") = "value"

```



## ChucK

Much like C or C++, declared but not initialized:
<lang> int a;
```

Multiple declaration:
<lang>int b,c,d;
```

Declared and initialized:
<lang>0 => int e;
```



## C++

Much like C, C++ variables are declared at the very start of the program after the headers are declared.
To declare a as an integer you say: the type of variable; then the variable followed by a semicolon ";".

```cpp
int a;
```


Template variables are specified with template parameters in angle brackets after the class name:

```cpp
std::vector<int> intVec;
```


## C#
Variables in C# are very dynamic, in the form that they can be declared practically anywhere, with any scope.
As in other languages, often used variables are: int, string, double etc.

They are declared with the type first, as in C:

```csharp>int j;</lang


Multiple variables may be defined in a single line as follows:

```c#
int p, a, d;
```


It is also possible to assign variables, either while declaring or in the program logic:

```c#
int a = 4;
int b;
int c = Func(a);

b = 5;
```



## COBOL

For example usage of array variables in COBOL, see [[Arrays#COBOL]].


### Assignment

Variables can be assigned values using either <code>MOVE</code> or <code>SET</code>. <code>SET</code> is used for assigning values to indexes, pointers and object references, and <code>MOVE</code> is used for everything else.

```cobol
MOVE 5 TO x
MOVE FUNCTION SOME-FUNC(x) TO y
MOVE "foo" TO z
MOVE "values 1234" TO group-item
SET some-index TO 5
```


One of COBOL's more well-known features is <code>MOVE CORRESPONDING</code>, where variables subordinate to a group item are assigned the values of variables with the same names in a different group item. The snippet below uses this to reverse the date:

```cobol
01  normal-date.
    03  year     PIC 9(4).
    03  FILLER   PIC X VALUE "-".
    03  month    PIC 99.
    03  FILLER   PIC X VALUE "-".
    03  dday     PIC 99. *> Misspelling is intentional; day is a reserved word.

01  reversed-date.
    03  dday     PIC 99.
    03  FILLER   PIC X VALUE "-".
    03  month    PIC 99.
    03  FILLER   PIC X VALUE "-".
    03  year     PIC 9(4).
...
PROCEDURE DIVISION.
    MOVE "2012-11-10" TO normal-date
    MOVE CORR normal-date TO reversed-date
    DISPLAY reversed-date *> Shows '10-11-2012'
```



### Declaration

Variables in COBOL are declared in the <code>DATA DIVISION</code>. In standard COBOL, they can be declared within it in:
* the <code>FILE SECTION</code>, where sort-files/file records are defined and associated with their respective file/sort descriptions.
* the <code>WORKING-STORAGE SECTION</code>, where static data is declared.
* the <code>LOCAL-STORAGE SECTION</code>, where automatic data is declared.
* the <code>LINKAGE SECTION</code>, where parameters are defined.
* the <code>REPORT SECTION</code>, where reports are defined and associated with report descriptions.
* the <code>SCREEN SECTION</code>, where the screens used for terminal I/O are described.

Variables are defined in the following format: ''level-number'' ''variable-name'' ''clauses''.

Variable type is defined in a <code>PICTURE</code> clause and/or a <code>USAGE</code> clause. <code>PICTURE</code> clauses can be used like so:

```cobol
01  a PIC X(20).      *> a is a string of 20 characters.
01  b PIC 9(10).      *> b is a 10-digit integer.
01  c PIC 9(10)V9(5). *> c is a decimal number with a 10-digit integral part and a 5-digit fractional part.
01  d PIC 99/99/99.   *> d is an edited number, with a slash between each pair of digits in a 6-digit integer.
```


The <code>USAGE</code> clause is used to define pointers, floating-point numbers, binary numbers, [http://en.wikipedia.org/wiki/Binary-coded_decimal#Packed_BCD packed decimals] and object references amongst others.

Each variable has a ''level-number'', which is a number from 1 to 49, or 77, which goes before the variable name. Level-numbers indicate how data is grouped together.
Variables with higher level-numbers are subordinate to variables with lower level-numbers.
The 77 level-number indicates the variable has no subordinate data and is therefore not a group item. Group items can include <code>FILLER</code> items which are parts of a group item which are not directly accessible.

```cobol
*> Group data items do not have a picture clause.
01  group-item.
    03  sub-data          PIC X(10).
    03  more-sub-data     PIC X(10).
```



### Initialization

Initialization is done via the <code>VALUE</code> clause in the <code>DATA DIVISION</code> or via the <code>INITIALIZE</code> statement in the <code>PROCEDURE DIVISION</code>. The <code>INITIALIZE</code> statement will set a variable back to either the value in its <code>VALUE</code> clause (if <code>INITIALIZE</code> has a <code>VALUE</code> clause) or to the appropriate value out of <code>NULL</code>, <code>SPACES</code> or <code>ZERO</code>.

```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
01  initialized-data      PIC X(15) VALUE "Hello, World!".
01  other-data            PIC X(15).
...
PROCEDURE DIVISION.
    DISPLAY initialized-data *> Shows 'Hello, World!'
    DISPLAY other-data       *> Will probably show 15 spaces.
```


Group items can be initialized, but they are initialized with a string like so:

```cobol
01  group-item   VALUE "Hello!12345".
    03  a-string PIC X(6). *> Contains "Hello!"
    03  a-number PIC 9(5). *> Contains '12345'.
```



### Reference Modification

Reference modification allows a range of characters to be taken from a variable.

```cobol
some-str (1:1)      *> Gets the first character from the string
some-num (1:3)      *> Get the first three digits from the number
another-string (5:) *> Get everything from the 5th character/digit onwards.

*> To reference modify an array element
some-table (1) (5:1)  *> Get the 5th character from the 1st element in the table
```



### Scope

Variables by default are local to the subprogram/class/etc. (''source element'') they are defined in. The <code>GLOBAL</code> clause allows the variable to be accessed in any nested source units as well. To be accessed from those inner source elements, the variable must be redeclared exactly as it was in the outer one, complete with <code>GLOBAL</code> clause, otherwise the variable in the inner one will shadow the global variable from the outer one.


## Common Lisp



### Declaration

'''Special''' variables are more or less like globals in other languages: http://www.lispworks.com/documentation/HyperSpec/Body/d_specia.htm

Special variables may be defined with '''defparameter'''.


```lisp
(defparameter *x* nil "nothing")
```


Here, the variable '''*x*''' is assigned the value nil. Special variables are wrapped with asterisks (called 'earmuffs'). The third argument is a docstring.

We may also use '''defvar''', which works like '''defparameter''' except that '''defvar''' won't overwrite the value of the variable that has already been bound.


```lisp
(defvar *x* 42 "The answer.")
```


It does, however, overwrite the docstring, which you can verify:


```lisp
(documentation '*x* 'variable)
```


'''defconstant''' works in the same way, but binds a constant. Constants are wrapped with '+' signs rather than earmuffs.

Common Lisp is case-insensitive, so saying <code>(equal +MoBy-DicK+ +moby-dick+)</code> will return '''t''' no matter the combination of upper and lower-case letters used in the symbol names. Symbols are hyphenated and lower-case.

For local varibles, we use let:


```lisp
(let ((jenny (list 8 6 7 5 3 0 9))
      hobo-joe)
  (apply #'+ jenny))
```


The symbols 'jenny' and 'hobo-joe' are ''lexically bound'' meaning that they are valid within the scope of the let block. If we move the apply form out of scope, the compiler will complain that 'jenny' is unbound.

The '''let''' macro binds an arbitrary number of variables: 'jenny' is bound to a list of numbers, whereas hobo-joe is bound to nil because we haven't provided a value. jenny and hobo-joe have the same scope.

Common Lisp prefers to use variables in lexical, rather than dynamic scope. If we've defined *x* as a special variable, then binding it lexically with '''let''' will create a new local value while leaving the dynamically scoped *x* untouched.


```lisp
(progn
  (let ((*x* 43))
    (print *x*)
  (print *x*))
```


If *x* has previously been bound to the value 42, then this example will output 43, then 42.


### Mutation


We use '''setf''' to modify the value of a symbol.


```lisp
(setf *x* 625)
```


We can also modify multiple symbols sequentially:


```lisp
(setf *x* 42 *y* (1+ *x*))
=>43
```


The return value is of the last form evaluated. In that example, '''*x*''' is referred to twice, and the value applied to '''*y*''' depends on the modified value of '''*x*'''. We can use '''psetf''' to set variables in parallel:


```lisp
(setf *x* 625)
(psetf *x* 42 *y* (1+ *x*)
=>NIL
```


The return value is NIL, but the resulting value of '''*y*''' is 626 instead of 43.


### Types

Common Lisp is dynamically typed, so we don't have to explicitly tell it what type of value a symbol holds. We nonetheless have the option of being explicit about which types our functions use through '''ftype''' declarations, which tell the compiler what to expect:


```lisp
(declaim (ftype (function (fixnum) fixnum) frobnicate))
(defun frobnicate (x)
  (+ x 42))
```


In this example, the function '''frobnicate''' must be written to take one fixnum (i.e., a fixed-width integer as opposed to a bignum) and must return a fixnum. Having been given this type information, the compiler can now assert that the function works the way we've told it it does, and will throw an error when you've made a mistake in implementing or calling the function. The compiler may also use more performant fixnum-specific arithmetic functions instead of the generic arithmetic functions.

You can give the compiler the same information using in-function type declarations and the '''the''' operator:


```lisp
(defun frobnicate (x)
  (declare (type fixnum x))
  (the fixnum (+ x 128)))
```


The '''declare''' statement applies to the function in which the statement appears. In the example, we assert to the compiler that '''x''' is a fixnum. In some compilers, '''the''' tells the compiler the type returned by an expression; we want our '''frobnicate''' to only ever return a fixnum and to throw an error if anything causes the return value to be anything other than a fixnum.


## Delphi


```Delphi
var
  i: Integer;
  s: string;
  o: TObject;
begin
  i := 123;
  s := 'abc';
  o := TObject.Create;
  try
    // ...
  finally
    o.Free;
  end;
end;
```



## D


```D

float bite = 36.321;	///_Defines a floating-point number (float), "bite", with a value of 36.321
float[3] bites;		///_Defines a static array of 3 floats
float[] more_bites;	///_Defines a dynamic array of floats

```



## DM

Types, procs (functions) and variables in DM are built with a very "tree" structure. Because of this, there are lots of ways to define variables. Fundamentally however, the "var" keyword is always used.

Declaring variables:

```DM
// Both of the following declarations can be seen as a tree,
// var -> <varname>
var/x
var y

// They can also be defined like this.
// This is once again a tree structure, but this time the "var" only appears once, and the x and y are children.
var
    x
    y
// And like this, still a tree structure.
var/x, y

```


The type of a variable can be declared like this, note that there is are types for strings or numbers:


```DM
// Here, "/obj" is the type, a /obj being a simple game object.
var/obj/x
// It can also be defined like this, because of the tree structure:
var
    obj
        x
// Or this
var/obj
   x

// ...
// You get the idea

```


Variables can be assigned, during declaration or later. The compiler does not enforce types here at all.


```DM

var/x = 0
x = 10
var/y
y = "hi!"

```



## DWScript


See [[Variables#Delphi|Delphi]] for "classic" declaration. In DWScript, if variables have to be declared ''before'' use, but can be declared inline, and their type can also be inferred.


```Delphi

var i := 123;             // inferred type of i is Integer
var s := 'abc';           // inferred type of s is String
var o := TObject.Create;  // inferred type of o is TObject
var s2 := o.ClassName;    // inferred type of s2 is String as that's the type returned by ClassName

```



## Dyalect



```Dyalect
//A constant declaration
const PI = 3.14

//Variable declaration
var x = 42

//Assignment
x = 42.42

//Dyalect is a dynamic language, so types are attached
//to values, not to the names
foo = (x: 2, y: 4) //foo is of type Tuple
bar = "Hello!" //bar is of type String

//Global variable
var g = 1.1

{
    //local variable (not visible outside of { } brackets)
    var loc = 2.2
}

func fun() {
    //Local variables, not visible outside of function
    var x = 1
    var y = 2
}

func parent() {
    //A local variable inside a parent function
    var x = 1
    func child() {
        //A local variable inside a nested function
        //It shadows a parent's variable
        var x = 2

        //But this is how we can reference a variable from
        //a parent function
        base.x
    }
}
```



## E


E is an [[impure]], [[lexically scoped]] language. Variables must be defined before use (they are not created on assignment). Definition of variables is a special case of [[Pattern Matching|pattern matching]].

An identifier occurring in a pattern is a simple non-assignable variable. The <code>def</code> operator is usually used to define local variables:


```e
def x := 1
x + x         # returns 2
```


<!-- Using bold rather than == so as to keep the TOC sane -->
'''Assignment'''

The pattern <code>var <var>x</var></code> makes <var>x</var> an assignable variable, and <code>:=</code> is the assignment operator.


```e
def var x := 1
x := 2
x         # returns 2
```


(As a shorthand, <code>var x := ...</code> is equivalent to <code>def var x := ...</code>.)

There are update versions of the assignment operator, in the traditional C style (<code>+=</code>, <code>-=</code>, <code>|=</code>, etc.), but also permitting any verb (method name) to be used:


```e
def var x := 1
x += 1                # equivalent to x := x + 1, or x := x.add(1)
x                     # returns 2

def var list := ["x"]
list with= "y"        # equivalent to list := list.with("y")
list                  # returns ["x", "y"]
```


'''Patterns'''

Since variable definition is part of pattern matching, a list's elements may be distributed into a set of variables:


```e
def [hair, eyes, var shirt, var pants] := ["black", "brown", "plaid", "jeans"]
```


However, ''assignment'' to a list as in Perl or Python is not currently supported.


```e
[shirt, pants] := ["white", "black"]   # This does not do anything useful.
```


'''Scoping'''

In E, a variable is visible from the point of its definition until the end of the enclosing block. Variables can even be defined inside expressions (actually, E has no statement/expression distinction):


```e
def list := [def x := timer.now(), x]    # two copies of the current time
list[0] == x                             # x is still visible here; returns true
```


'''Slots'''

The difference between assignable and non-assignable variables is defined in terms of primitive operations on non-primitive ''[http://wiki.erights.org/wiki/Slot slot]'' objects. Slots can also be employed by programmers for effects such as variables which have an effect when assigned (e.g. <code>backgroundColor := red</code>) or automatically change their values over time, but that is beyond the scope of this task. For example, it is possible to transfer a variable between scopes by referring to its slot:


```e
def makeSum() {
  var a := 0
  var b := 0
  return [&a, &b, fn { a + b }]
}

def [&x, &y, sum] := makeSum()
x := 3
y := 4
sum() # returns 7
```


As suggested by the <code>&</code> syntax, the use of slots is somewhat analogous in effect to C pointers or C++ references, allowing the passing of ''locations'' and not their values, and "pass-by-reference" or "out" parameters:


```e
def getUniqueId(&counter) {
  counter += 1
  return counter
}

var idc := 0
getUniqueId(&idc) # returns 1
getUniqueId(&idc) # returns 2
```



## Eiffel

Variables must be declared before their use with an explicit type.

```eiffel

class
	APPLICATION
inherit
	ARGUMENTS
create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			i: INTEGER
			s: STRING
		do
			i := 100
			s := "Some string"
			create a.make_empty
		end

feature {NONE} -- Class Features

a: ARRAY[INTEGER]

```

In this example, i and s have local scope and a has global scope. Two variables of the same class cannot have the same name, regardless of scope.
Global variables are considered class features and follow the same export status modifiers as normal features.

There are two types of variables, reference or expanded. Reference type variables refer either to an object of the declared datatype, or Void. Expanded type variables always correspond to an object. The basic objects are of expanded type. These include numbers, Booleans, and characters.
All variables are objects, and are auto-initialized. For reference types, the initial value is Void. For expanded types, a default creation feature is called.

Variables can be initialized through a creation feature, such as for a in the example above.

Assignment to variables depends on the type of variable

```eiffel

-- assume A and B are of the same datatype
B := A
A.some_modification_feature

```

For reference type variables, B copies the reference of A, so any modifications to A (or B) affects the other. In the case of expanded types, B will copy A (memory-wise copy) so any modification to A (or B) will never be seen by the other.


## Ela


Strictly speaking Ela doesn't have variables. Instead Ela provides a support for a declaration of names that can be bound to values. Unlike variables names are immutable - it is not possible to change a value bound to a name.

Global declarations:


```ela
x = 42

sum x y = x + y
```


Local declarations:


```ela
sum x y = let z = x + y in z

sum x y = z
          where z = x + y
```



## Elena

ELENA 4.1:

```elena
public program()
{
   var c := nil;                  // declaring variable.
   var a := 3;                    // declaring and initializing variables
   var b := "my string".Length;
   long l := 200l;                // declaring strongly typed variable
   auto lst := new List<int>();

   c := b + a;                    // assigning variable
}
```



## Erlang

Variables spring into life upon assignment, which can only happen once (single assignment). Their scope is only the local function and they must start with a capital letter.

```Erlang

two() ->
	A_variable = 1,
	A_variable + 1.

```


=={{header|F Sharp|F#}}==
Variables in F# bind a name to a value and are, by default, immutable. They can be declared nearly anywhere and are normally local to the block/assembly they are defined in. They are declared in the form: let ''name'' ''parameters'' = ''expression''.

```fsharp
let x = 5                              // Int
let mutable y = "mutable"              // Mutable string
let recordType = { foo : 6;  bar : 6 } // Record
let intWidget = new Widget<int>()      // Generic class
let add2 x = 2 + x                     // Function value
```


Types are normally inferred from the values they are initialised with. However, types can be explicitly specified using ''type annotations''.

```fsharp
let intEqual (x : int) (y : int) = x = y
let genericEqual x y : 'a = x = y
```


Mutable variables are set using the <code><-</code> operator.

```fsharp
sum <- sum + 1
```



## Factor

The SYMBOL bit defines a new symbol word which is used to identify variables. use-foo shows how one would modify and get the contents of the variable. named-param-example is an example of using :: to define a word with named inputs, similar to the way other languages do things. Last, but not least, local-example shows how to use [let to define a group of lexically scoped variables inside of a word definition.

```factor
SYMBOL: foo

: use-foo ( -- )
    1 foo set
    foo get 2 + foo set ! foo now = 3
    foo get number>string print ;

:: named-param-example ( a b -- )
    a b + number>string print ;

: local-example ( -- str ) [let "a" :> b "c" :> a a " " b 3append ] ;
```




## Falcon


```falcon

/* partially created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

/* global and local scrope
   from the Falcon survival
   guide book */
// global scope
sqr = 1.41

function square( x )
	// local scope
   sqr = x * x
   return sqr
end


number = square( 8 ) * sqr


a = [1, 2, 3] 	// array
b = 1			// variable declaration
e = 1.0  		// float
f = "string"	// string

/* There are plenty more
data types in Falcon */

```



## Forth


###  Local Variables

Historically, Forth has preferred open access to the parameter stack over named local variables. The 1994 standard however added a cell-sized local variable facility and syntax.  The semantics are similar to VALUEs: locals are initialized from stack contents at declaration, the name retrieves the value, and TO sets the value of the local name parsed at compile time ("value TO name").

```forth
: hypot ( a b -- a^2 + b^2 )
  LOCALS| b a |            \ note: reverse order from the conventional stack comment
  b b * a a * + ;
```


Modern Forth implementations often extend this facility in several ways, both for more convenient declaration syntax and to be more compatible with foreign function interfaces. Curly braces are used to replace the conventional stack comment with a similar looking local variable declaration.

```forth
: hypot { a b -- a^2 + b^2 }      \ text between "--" and "}" remains commentary
  a a * b b * + ;
```


Modern systems may also allow different local data types than just integer cells.

```forth
: length { F: a F: b F: c -- len }     \ floating point locals
  a a F* b b F* F+ c c F* F+ FSQRT ;
```



###  Global Variables

As mentioned Forth normally uses the parameter stack for input/output arguments. When there is the need for a Global variable Forth simply creates a label and allocates a piece of memory to the variable. When the variable is invoked it does not return the value in the memory but rather it returns the address of the variable on the parameter stack. This is like what other languages call a pointer, however Forth has no such obfuscation.  A named memory address is simple to understand. To store a value in the variable the '!' (store) operator is used and to fetch a value from a variable the '@' (fetch) operator is used.  VARIABLEs have the same size as the processor's native integer with a minimum size of 16 bits. Double precision variables are created with the word 2VARIABLE that are used with corresponding 2@ and 2! operators.<lang>VARIABLE X   999 X !                \ create variable x, store 999 in X
VARIABLE Y   -999 Y !               \ create variable y, store -999 in Y
2VARIABLE W  140569874. W 2!        \ create and assign a double precision variable

```

Test at the Forth Console

```txt

X @ . 999 ok
Y @ . -999 ok

X @ Y @ + . 0 ok
W 2@ D. 140569874 ok

```



## Fortran



```fortran
 program test
 implicit none

 integer :: i  !scalar integer
 integer,dimension(10) :: ivec !integer vector
 real :: r !scalar real
 real,dimension(10) :: rvec !real vector
 character(len=:),allocatable :: char1, char2  !fortran 2003 allocatable strings

!assignments:

!-- scalars:
 i = 1
 r = 3.14

!-- vectors:
 ivec = 1 !(all elements set to 1)
 ivec(1:5) = 2

 rvec(1:9) = 0.0
 rvec(10) = 1.0

!-- strings:
 char1 = 'hello world!'
 char2 = char1   !copy from one string to another
 char2(1:1) = 'H'  !change first character

end program test

```



## GAP



```gap
# At top level, global variables are declared when they are assigned, so one only writes
global_var := 1;

# In a function, local variables are declared like this
func := function(n)
    local a;
    a := n*n;
    return n + a;
end;

# One can test whether a variable is assigned
IsBound(global_var);
# true;

# And destroy a variable
Unbind(global_var);

# This works with list elements too
u := [11, 12, , 14];
IsBound(u[4]);
# true
IsBound(u[3]);
# false
Unbind(u[4]);
```



## GlovePIE


```GlovePIE
if var.end=0 then // Without the code being in this if statement, the code would keep executing until it were to stop.
var.end=1
// These variables, respectively, refer to an integer, a boolean, and a string.
var.variable1=3
var.variable2=True
var.variable3="Hi!"
// var.example1 now refers to a string object instead.
var.variable1="Bye!"
endif
```



## Go

'''Simplest and most common'''

While Go is statically typed, it provides a “short variable declaration” with no type explicitly stated, as in,

```go>x := 3</lang

This is the equivalent of,

```go
var x int // declaration
x = 3     // assignment
```

The technique of not stating the type is known as type inference, or duck typing.  The right hand side can be any expression.  Whatever type it represents is used as the type of the variable.  More examples:

```go
y := x+1                  // y is int, assuming declaration above
same := x == y            // same declared as bool
p := &same                // type of p is pointer to bool
pi := math.Floor(math.Pi) // math.Floor returns float64, so that is the type of pi
```

'''Nothing goes uninitialized'''

Variables declared without initializer expressions are initialized to the zero value for the type.

```go
var x, y int // two variables, initialized to zero.
var p *int   // initialized to nil
```

'''Opposite C'''

While putting the variable before the type feels “backwards” to programmers familiar with certain other languages, it succinctly allows multiple variables to be declared with arbitrarily complex type expressions.

'''List syntax'''

Variables can be declared in a list with the keyword var used only once.  The syntax visually groups variables and sets the declaration off from surrounding code.

```go
var (
    x, y int
    s string
)
```

'''Multiple assignment'''

Multiple values can be assigned in a single assignment statement, with many uses.

```go
x, y = y, x                 // swap x and y
sinX, cosX = math.Sincos(x) // Sincos function returns two values
// map lookup optionally returns a second value indicating if the key was found.
value, ok = mapObject[key]
```

'''Other kinds of local variables'''

Parameters and named return values of functions, methods, and function literals also represent assignable local variables, as in,

```go
func increase (x int) (more int) {
    x++
    more = x+x
    return
}
```

Parameter <tt>x</tt> and return <tt>value</tt> more both act as local variables within the scope of the function, and are both assignable.  When the function returns, both go out of scope, although the value of more is then returned as the value of the function.

While assignment of return values is highly useful, assignment of function parameters is often an error.  Novice programmers might think that modifying a parameter inside the function will affect a variable used as an argument to the function call.  It does not.

Method receivers also represent assignable local variables, and as with function parameters, assigning them inside the method is often a mistake.

'''Other common errors'''

Short declarations can involve multiple assignment, as in

```go
x, y := 3, 4
```

But there are complications involving scope and variables already defined that confuse many programmers new to Go.  A careful reading of the language specification is definitely in order, and a review of misconceptions as discussed on the mailing list is also highly recommended.

Programmers new to the concept of closures often fail to distinguish between assigning free and bound variables.  Function literals in Go are closures, and a common novice error is to start multiple goroutines from function literals, and fail to understand that multiple goroutines are accessing the same free variables.


## Haskell


You can define a variable at the top (module) level or in a <code>where</code>, <code>let</code>, or <code>do</code> construct.


```haskell
foobar = 15

f x = x + foobar
  where foobar = 15

f x = let foobar = 15
      in  x + foobar

f x = do
    let foobar = 15
    return $ x + foobar
```


One particular feature of <code>do</code> notation looks like assignment, but actually, it's just syntactic sugar for the <code>&gt;&gt;=</code> operator and a unary lambda.


```haskell
main = do
    s <- getLine
    print (s, s)

-- The above is equivalent to:

main = getLine >>= \s -> print (s, s)
```


''Pattern matching'' allows for multiple definitions of the same variable, in which case each call uses the first applicable definition.


```haskell
funkshun True  x = x + 1
funkshun False x = x - 1

foobar = funkshun True 5 + funkshun False 5   -- 6 + 4
```


<code>case</code> expressions let you do pattern-matching on an arbitrary expression, and hence provide yet another way to define a variable.


```haskell
funkshun m = case foo m of
    [a, b]           -> a - b
    a : b : c : rest -> a + b - c + sum rest
    a                -> sum a
```


''Guards'' are as a kind of syntactic sugar for if-else ladders.


```haskell
signum x | x > 0     =  1
         | x < 0     = -1
         | otherwise =  0
```


A defintion can be accompanied by a ''type signature'', which can request a less general type than the compiler would've chosen on its own. (Because of the monomorphism restriction, there are also some cases where a type signature can request a ''more'' general type than the default.) Type signatures are also useful even when they make no changes, as a kind of documentation.


```haskell
dotProduct :: [Int] -> [Int] -> Int
dotProduct ns ms = sum $ zipWith (+) ns ms
-- Without the type signature, dotProduct would
-- have a more general type.

foobar :: Num a => a
foobar = 15
-- Without the type signature, the monomorphism
-- restriction would cause foobar to have a less
-- general type.
```


Since Haskell is purely functional, most variables are immutable. It's possible to create mutable variables in an appropriate monad. The exact semantics of such variables largely depend on the monad. For example, <code>STRef</code>s must be explicitly initialized and passed between scopes, whereas the implicit state of a <code>State</code> monad is always accessible via the <code>get</code> function.


## HicEst


```HicEst
! Strings and arrays must be declared.
! Everything else is 8-byte float, READ/WRITE converts
  CHARACTER str="abcdef", str2*345, str3*1E6/"xyz"/
  REAL, PARAMETER :: named_constant = 3.1415
  REAL :: n=2, cols=4, vec(cols), mtx(n, cols)
  DATA vec/2,3,4,5/, mtx/1,2,3.1415,4,  5,6,7,8/

  named = ALIAS(alpha, beta, gamma) ! gamma == named(3)
  ALIAS(vec,n, subvec,2) ! share subvec and vec(n...n+1)
  ALIAS(str,3, substr,n) ! share substr and str(3:3+n-1)

  a = EXP(b + c)     ! assign/initialze a=1, b=0, c=0
  str = "blahblah"   ! truncate/expand if needed
  beta = "blahblah"  ! illegal

  CALL noArguments_noUSE   ! global scope SUBROUTINE
  CALL Arguments_or_USE(a) ! local scope SUBROUTINE
  t = func()               ! local scope FUNCTION

SUBROUTINE noArguments_noUSE() ! all global
  vec2 = $ ! 1,2,3,...
END

SUBROUTINE Arguments_or_USE(var) ! all local
  USE : vec                      ! use global object
  var = SUM(vec)
  t = TIME()         ! local, static, USEd by func()
END

FUNCTION func()                  ! all local
  USE Arguments_or_USE : t       ! use local object
  func = t
END
```



## HolyC

Variables must be declared before use. The declaration of a variable, without assigning a value takes the form <i><typename> <variablename>;</i>

```holyc>U8 i;</lang


Some common types are: <i>I8, I64, U8, U64, F64</i>.

It is possible to initialize variables with expressions having known values when they are defined.
The syntax follows the form <i><typename> <variablename> = <initializing expression>;</i>

```holyc
U8 b1 = 8;
U8 b2 = b1 * 10;
```


Multiple variables may be defined in a single statement as follows:

```holyc
U8 uint1, uint2, uint3;
```


To initialized a string:

```holyc
U8 *str = "The HolyC Language";
```


== Icon and Unicon ==
Icon/Unicon data types are implemented as type safe self-descriptive values and as such do not require conventional type declarations.  [[Icon%2BUnicon/Intro#un-Declarations.2C_it.27s_all_about_Scope| See Introduction to Unicon and Icon about declarations]]

Declarations are confined to scope and use and include local, static, global, procedure parameters, and record definitions.  Additionally Unicon has class definitions. Undeclared variables are local by default.

```Icon
global gvar          # a global

procedure main(arglist)         # arglist is a parameter of main
local a,b,i,x                   # a, b, i, x are locals withing main
static y                        # a static (silly in main)

x := arglist[1]
a := 1.0
i := 10
b := [x,a,i,b]

# ... rest of program
end
```


=
## Icon
=
=
## Unicon
=
This Icon solution works in Unicon.


## J



```j>val=. 0</lang


J has two assignment operators.  The =. operator declares, initializes, assigns, etc. a local variable.  The =: operator does the same for a "global" variable.


```j
fun =: 3 :0
  val1 =: 0
  val1 =. 2
  val2 =. 3
  val1, val2
)
   fun''
2 3
   val1
0
   val2
|value error
```


Note that the language forbids assigning a "global" value in a context where the name has a local definition.


```j
fun1 =: 3 :0
  val3=. 0
  val3=: 0
)
   fun1''
|domain error
```


But the purpose of this rule is to help people catch mistakes.  If you have reason to do this, you can easily set up another execution context.


```j
fun2 =: 3 :0
  val4=. 0
  3 :'val4=:y' y
)
   fun2 ''
```


Variables are referred to by name, and exist in locales (which may be used as classes, closures or other stateful references).

FIXME (working on good illustrative examples that would make sense to someone used to different languages)

That said, it is possible and not uncommon to write an entire J application without using any variables (J has a functional, "point free" style of coding known as ''tacit'').  Names are optional (though often convenient).  And, it can be possible to build code using names and then remove them using <code>f.</code> -- this is somewhat analogous to compiling code though the implementation of f. does not have to compile anything.


## Java

Variables in Java are declared before their use with explicit types:

```java
int a;
double b;
AClassNameHere c;
```

Several variables of the same type can be declared together:

```java
int a, b, c;
```

Variables can be assigned values on declaration or afterward:

```java
int a = 5;
double b;
int c = 5, d = 6, e, f;
String x = "test";
String y = x;
b = 3.14;
```

Variables can have scope modifiers, which are explained [[Scope modifiers#Java|here]].

<code>final</code> variables can only be assigned once, but if they are <code>Object</code>s or arrays, they can be modified through methods (for <code>Object</code>s) or element assignment (for arrays):

```java
final String x = "blah";
final String y;
final double[] nums = new double[15];
y = "test";
x = "blahblah"; //not legal
nums[5] = 2.5; //legal
nums = new double[10]; //not legal
final Date now = new java.util.Date();
now.setTime(1234567890); //legal
now = new Date(1234567890); //not legal
```



## JavaScript

Information lifted from [http://stackoverflow.com/questions/500431/javascript-variable-scope Stack Overflow] (credit to [http://stackoverflow.com/users/23691/krosenvold krosenvold] and [http://stackoverflow.com/users/43089/triptych triptych])

Javascript uses scope chains to establish the scope for a given function. There is typically one global scope, and each function defined has its own nested scope. Any function defined within another function has a local scope which is linked to the outer function. It's always the position in the source that defines the scope.

An element in the scope chain is basically a Map with a pointer to it's parent scope.

When resolving a variable, javascript starts at the innermost scope and searches outwards.

<div style='height:30em;width:full;overflow:scroll'>
```javascript
// a globally-scoped variable
var a=1;

// global scope
function one(){
    alert(a);
}

// local scope
function two(a){
    alert(a);
}

// local scope again
function three(){
  var a = 3;
  alert(a);
}

// Intermediate: no such thing as block scope in javascript
function four(){
    if(true){
        var a=4;
    }

    alert(a); // alerts '4', not the global value of '1'
}


// Intermediate: object properties
function Five(){
    this.a = 5;
}


// Advanced: closure
var six = function(){
    var foo = 6;

    return function(){
        // javascript "closure" means I have access to foo in here,
        // because it is defined in the function in which I was defined.
        alert(foo);
    }
}()


// Advanced: prototype-based scope resolution
function Seven(){
  this.a = 7;
}

// [object].prototype.property loses to [object].property in the scope chain
Seven.prototype.a = -1; // won't get reached, because 'a' is set in the constructor above.
Seven.prototype.b = 8; // Will get reached, even though 'b' is NOT set in the constructor.



// These will print 1-8
one();
two(2);
three();
four();
alert(new Five().a);
six();
alert(new Seven().a);
alert(new Seven().b);
```
</div>

## Joy

JOY does not have variables. Variables essentially name locations in memory, where values are stored. JOY also uses memory to store values, but has no facility to name these locations. The memory that JOY uses is commonly referred to as "the stack".

'''Initializing'''

The JOY stack can be initialized:

```joy
[] unstack
```


'''Assignment'''

Values can be pushed on the stack:

```joy>42</lang

pushes the value 42 of type integer on top of the stack.

'''Stack'''

Calling the stack by name pushes a copy of the stack on the stack. To continue the previous example:

```joy>stack</lang

pushes the list [42] on top of the stack. The stack now contains: [42] 42.

## jq

jq variables are strictly speaking not variable: they are just names given to JSON values.  For example, the expression "1 as $x | 2 as $x | $x " can be understood as assigning the value 1 to $x and then assigning another value to $x, with the final result being 2, but it must be understood that the second occurrence of $x shadows the first, so that the third occurrence is just a reference to the second.  To see this more clearly, consider the following superficially similar expression:

```jq
jq -n '1 as $x | (2 as $x | $x) | $x'
```

In this case, the expression as a whole yields 1 (rather than 2 as previously), as the subexpression in parentheses does not cause shadowing of the first $x.

'''Naming and Assignment'''

In a jq program, variable names are strings beginning with "$" followed by one or more characters chosen from the set of alphanumeric characters augmented with the "_" (underscore) character.  Assignment within a jq program is based on the syntax:

```jq
EXPRESSION as $NAME
```

This establishes $NAME as a reference to the value of EXPRESSION.  There is no special syntax to constrain the type of a variable.

'''Global Variables'''

Global variables can be given values on the jq command line.  For example, suppose the following two lines are in a file named test.jq:

```jq
def test: $x;
test
```

The following invocation:

```sh
jq --arg x 123 -n -f test.jq
```

will result in the string "123" being output.


## Liberty BASIC


```lb

'In Liberty BASIC variables are either string or numeric.
'A variable name can start with any letter and it can contain both letters and numerals, as well as dots (for example: user.firstname).
'There is no practical limit to the length of a variable name... up to ~2M characters.
'The variable names are case sensitive.

'assignments:   -numeric variables. LB assumes integers unless assigned or calculated otherwise.
'Because of its Smalltalk heritage, LB integers are of arbitrarily long precision.
'They lose this if a calculation yields a non-integer, switching to floating point.
    i = 1
    r = 3.14

'assignments    -string variables. Any string-length, from zero to ~2M.
    t$    ="21:12:45"
    flag$ ="TRUE"

'assignments    -1D or 2D arrays
'A default array size of 10 is available. Larger arrays need pre-'DIM'ming.
    height( 3)          =1.87
    dim height( 50)
    height( 23)         =123.5
    potential( 3, 5)    =4.5
    name$( 4)           ="John"

'There are no Boolean /bit variables as such.

'Arrays in a main program are global.
'However variables used in the main program code are not visible inside functions and subroutines.
'They can be declared 'global' if such visibility is desired.
'Functions can receive variables by name or by reference.

```



## Julia

Certain constructs in the language introduce scope blocks, which are regions of code that are eligible to be the scope of some set of variables. The scope of a variable cannot be an arbitrary set of source lines, but will always line up with one of these blocks. The constructs introducing such blocks are:

function bodies (either syntax)
while loops
for loops
try blocks
catch blocks
let blocks
type blocks.

```julia
#declaration/assignment, declaration is optional
x::Int32 = 1
#datatypes are inferred dynamically, but can also be set thru convert functions and datatype literals
x = 1 #x is inferred as Int, which is Int32 for 32-bit machines, Int64 for 64-bit machines
#variable reference
julia>x
1

x = int8(1) #x is of type Int8
x = 1.0 #x is Float64
x = y = 1 #assign both x and y to 1
global x = 1 #assigns 1 to global variable x (used inside scope blocks)
local x = 1 #assigns 1 to local variable x (used inside scope blocks)
x = 'a' #x is a 'Char' type, designated by single quotes
x = "a" #x is a 1-element string, designated by double quotes
```

A common use of variables is giving names to specific, unchanging values. Such variables are only assigned once. This intent can be conveyed to the compiler using the const keyword:

```julia
const e  = 2.71828182845904523536
const pi = 3.14159265358979323846
```



## Kotlin

Local variables in Kotlin (i.e. variables created within a function, constructor or a property getter/setter) must be individually declared and initialized before they are used.  They then remain in scope until the end of the block in which they are declared though their lifetime may be extended if they are 'captured' by a closure of some kind.

A variable's type can either be inferred from the type of the initialization expression or can be declared explicitly.
There are essentially two types of variables: read-only (declared with 'val') and mutable (declared with 'var').

Variables declared at top-level or class/object scope are technically properties rather than variables and only have hidden backing fields where necessary. However, like local variables, they are either read-only or mutable and (in the case of top level or object properties) can be declared to be compile-time constants using the 'const val' modifier.

Here are some examples of local variables:

```scala
// version 1.0.6

fun main(args: Array<String>) {
    /* read-only variables */
    val i = 3          // type inferred to be Int
    val d = 2.4        // type inferred to be double
    val sh: Short = 2  // type specified as Short
    val ch = 'A'       // type inferred to be Char
    val bt: Byte = 1   // type specified as Byte

    /* mutable variables */
    var s = "Hey"      // type inferred to be String
    var l =  4L        // type inferred to be Long
    var b: Boolean     // type specified as Boolean, not initialized immediately
    var f =  4.4f      // type inferred to be Float

    b = true           // now initialized
    println("$i, $d, $sh, $ch, $bt, $s, $l, $b, $f")

    s = "Bye"          // OK as mutable
    l = 5L             // OK as mutable
    b = false          // OK as mutable
    f = 5.6f           // OK as mutable

    println("$i, $d, $sh, $ch, $bt, $s, $l, $b, $f")
}
```


```txt

3, 2.4, 2, A, 1, Hey, 4, true, 4.4
3, 2.4, 2, A, 1, Bye, 5, false, 5.6

```



## Lasso


```Lasso
// declare thread variable, default type null
var(x)
$x->type // null

// declare local variable, default type null
local(x)
#x->type // null

// declare thread variable, initialize with a type, in this case integer
var(x = integer)

// declare local variable, initialize with a type, in this case integer
local(x = integer)

// assign a value to the thread var x
$x = 12

// assign a value to the local var x
$x = 177

// a var always has a data type, even if not declared - then it's null
// a var can either be assigned a type using the name of the type, or a value that is by itself the type
local(y = string)
local(y = 'hello')

'\r'
// demonstrating asCopyDeep and relationship between variables:
local(original) = array('radish', 'carrot', 'cucumber', 'olive')
local(originalaswell) = #original
local(copy)     = #original->asCopyDeep
iterate(#original) => {
    loop_value->uppercase
}
#original		// modified
//array(RADISH, CARROT, CUCUMBER, OLIVE)
'\r'
#originalaswell // modified as well as it was not a deep copy
//array(RADISH, CARROT, CUCUMBER, OLIVE)
'\r'
#copy			// unmodified as it used ascopydeep
//array(RADISH, CARROT, CUCUMBER, OLIVE)
```



## Lingo

In Lingo (local) variables are declared by assigning a value:

```lingo
x = 23
y = "Hello world!"
z = TRUE -- same effect as z = 1
```


Trying to use a non declared variable causes a (pre)compile-time error. The following script doesn't compile, but throws "Script error: Variable used before assigned a value":

```lingo
on foo
  put x
end
```


Lingo's value function can be used to "silently" - i.e. without throwing errors - check if a variable is defined or not in the current scope:

```lingo
put value("x")
-- <Void> -- means: undefined
```


But variable declarations/assignments can also explicitely assign the constant VOID. The following script compiles successfully, but value("x") would still return <Void>:

```lingo
on foo
  x = VOID
  put x
end
```


Global variables are declared by adding "global <varName>" either to the top of a script or anywhere - but before first usage - inside a function body. Both of the following scripts are valid and compile:

```lingo
global x
on foo
  put x
end
```


```lingo
on foo
  global x
  put x
end
```


Global variables can also be declared/created at runtime, by adding them as property to the special _global object:

```lingo
_global.x = 23
```


Any other function/script can then access this dynamically created global variable, either by a having a "global <varName>" statement in its code (see above), or by using this special _global object:

```lingo
put _global.x
```



## Logo

Historically, Logo only had global variables, because they were easier to access when stepping through an algorithm. Modern variants have added dynamic scoped local variables.
```logo
make "g1 0
name 2 "g2    ; same as make with parameters reversed
global "g3     ; no initial value
to func :x
  make "g4 4   ; still global
  localmake "L1 6
  local ["L2 "L3]    ; local variables, collection syntax
  func2 :g4
  print :L2      ; 9,  modified by func2
  print :L3      ; L3 has no value, was not modified by func2
end
to func2 :y
  make "g3 :y
  make "L2 :L1 + 3     ; dynamic scope: can see variables of callers
  localmake "L3 5       ; locally override L3 from caller
  (print :y :L1 :L2 :L3)      ; 4 6 9 5
end
print :g4   ; 4
print :L1   ; L1 has no value
print name? "L1    ; false, L1 is not bound in the current scope
```



## LotusScript



```Lotusscript
Sub Click()
'a few declarations as example
Dim s as New NotesSession ' declaring a New NotesSession actually returns the current, active NotesSession
Dim i as Integer ' i = 0
Dim s as String ' s= ""
Dim v as Variant ' v is nothing
Dim l as Long ' l = 0
Dim doc as NotesDocument 'doc is EMTPY

'...

End Sub

```


## Lua

In lua, variables are dynamically typecast, and do not need declaration prior to use.


```lua
a = 1    -- Here we declare a numeric variable
fruit = "banana"    -- Here we declare a string datatype
needspeeling = True    -- This is a boolean
local b = 2    -- This variable declaration is prefixed with a scope modifier
```


The lua programming language supports multiple assignments from within a single statement:


```lua
A, B, C, D, E = 2, 4, 6, 8, "It's never too late"
```



## M2000 Interpreter

Each module has own local variables and can create global variables. A local variable shadow any global with same name. A new global with same name shadow a global variable (but its wrong to create a local first with same name, we see local always). We can change global variables, but to assign to a global variable we need to use <=. Arrays defined with DIM and global arrays not use <= for assign values to items.

Variables can exist in Groups as public or private. Also variables can be closures in lambda functions.

We can use '''Local''' to make local variables, '''Let''' to make variables, '''Def''' to make once variables (second time using Def for same variable we get error). We can use '''Input''' and '''Read''' to make new variables too. We can assign a value to new name, to make a variable.




```M2000 Interpreter

\\ M2000 use inference to state the type of a new variable, at run time
\\ We can use literals of a numeric type
\\ @ for Decimal
\\ # for Currency
\\ ~ for Single
\\ & for Long (32bit)
\\ % for Integer (16bit)
\\ Double and Boolean have no symboles
Module TopA {
      Module Alfa {
                  Print A=10000, Type$(A)="Double"
                  \\ A is local, we use =
                  A=10@
                  Print A=10, Type$(A)
                  \\ Or we can state the type before
                  Def Currency K, Z=500, M
                  K=1000
                  \\ Currency Currency Currency
                  Print Type$(K), Type$(Z), Type$(M)
                  Def Double K1, K2 as Integer=10, K3=1
                  \\ double integer double
                  Print Type$(K1), Type$(K2), Type$(K3)
                  Mb=1=1
                  \\ We get a boolean
                  Print Type$(Mb)
                  Def boolean Mb1=True
                  Print Type$(Mb1)
                  \\ True and False are Double -1 and 0 not Boolean
                  Mb3=True
                  Print Type$(Mb3)="Double"
                  \\ For strings we have to use $ (like in old Basic)
                  A$="This is a String"
                  Global G1 as boolean = True
                  \\ To change a global variable we have to use <=
                  G1<=1=0
                  \\ If we do this: G1=1=0 we make a local variable, and shadow global
                  \\ In a For Object {} we can make temporary variables
                  For This {
                        Local G1=122.1212
                        Print G1, Type$(G1)="Double"
                  }
                  Print G1, Type$(G1)="Boolean"
      }
      \\ shadow A for this module only
      A=100
      \\ Now we call Alfa
      Alfa
      Print (A=100)=True
}
Global A=10000
TopA
Print A=10000
Module CheckStatic {
      \\ clear static variables and variables (for this module)
      Clear
      Module K {
            \\ if no A exist created with value 100@
            \\ Static variables can't be referenced
            Static A=100@
            Print A, Type$(A)="Decimal"
            A++
      }
      For i=1 to 10 : K : Next i
      Print A=10000
}
CheckStatic
Print A=10000

\\ reference and use of stack of values
C=100&
Module ChangeC {
      \\ we leave arguments in stack of values
      Module HereChangeC (&f) {
            \\ interpreter execute a Read &f
            f++
      }
      \\ now we call HereChangeC passing current stack of values
      HereChangeC
}
\\ Calling a module done without checking for what parameters a module take
ChangeC &C
Print C, Type$(C)="Long"
K=10010001001@
ChangeC &K
Print K, Type$(K)="Decimal"
Module TypeRef (&x as Double) {
     Print x
     x++
}
D=100
TypeRef  &D
Try ok {
      TypeRef &K
}
If Error or Not Ok then Print Error$ ' we get wrong data type

```



## Maple

Variables are dynamic in Maple, so they do not need to be defined before they are used.

The assignment operator in Maple is := . It is also possible to assign to a variable name using the assign() command. It can also be noted that variables at the top level are global for a given session.

```maple
a := 1:
print ("a is "||a);
                            "a is 1"
```



Variables in a procedure should be declared with the "local" or "global" keyword.

A local variable has a smaller scope and can only be used within the procedure it's declared in.
In the following example, b is local to f() and will be 3 within the procedure only. The local variable b does not have a value outside the procedure.

```maple
b;
f := proc()
    local b := 3;
    print("b is "||b);
end proc:
f();
print("b is "||b);
                               b
                            "b is 3"
                            "b is b"
```


A global variable has a large scope and can be used anywhere inside a program. A global variable declared outside a procedure can be used within the procedure, and a global variable declared within a procedure can be used outside of it.
In the following example, a is a global variable that is from outside the procedure. The global variable c has a value even when used outside the procedure it was declared in.

```maple
f := proc()
    global c;
    c := 3;
    print("a is "||a);
    print("c is "||c);
end proc:
f();
print("c is "||c);
                            "a is 1"
                            "c is 3"
                            "c is 3"
```



Variables can be reassigned.

```maple
print ("a is "||a);
a := 4:
print ("a is "||a);
                            "a is 1"
                            "a is 4"
```



Variables can be reassigned as different datatypes.

```maple
print ("a is "||a);
type(a, integer);
a := "Hello World":
print ("a is "||a);
type(a, integer);
type(a, string);
                            "a is 4"
                              true
                       "a is Hello World"
                             false
                              true
```



Variable names may contain spaces and other characters when you enclose them in ``.

```maple
`This is a variable` := 1:
print(`This is a variable`);
                               1
```



To unassign a variable.

```maple
print ("a is "||a);
type(a, string);
print("c is "||c);
a := 'a':
print ("a is "||a);
type(a, symbol);
unassign('c');
print("c is "||c);
                       "a is Hello World"
                              true
                            "c is 3"
                            "a is a"
                              true
                            "c is c"
```

Here, symbol is essentially another term for variable name.


## Mathematica


```txt
x=value	assign a value to the variable x
x=y=value	assign a value to both x and y
x=. or Clear[x]	remove any value assigned to x

lhs=rhs (immediate assignment)	rhs is evaluated when the assignment is made
lhs:=rhs (delayed assignment)	rhs is evaluated each time the value of lhs is requested
```



'''Atomic Objects'''


```txt
All expressions in Mathematica are ultimately made up from a small number of basic or atomic types of objects.

Symbol / String / Integer / Real / Rational / Complex

These objects have heads which are symbols that can be thought of as "tagging" their types.
The objects contain "raw data", which can usually be accessed only by functions specific to the particular type of object.
You can extract the head of the object using Head, but you cannot directly extract any of its other parts.
```



'''Symbols are the basic named objects in Mathematica'''


```txt
aaaaa	user-defined symbol
Aaaaa	system-defined symbol
$Aaaa	global or internal system-defined symbol
aaaa$	symbol renamed in a scoping construct
aa$nn	unique local symbol generated in a module
```



'''Contexts'''


```txt
aaaa`x is a symbol with short name x, and context aaaa.
Contexts in Mathematica work somewhat like file directories in many operating systems.
You can always specify a particular file by giving its complete name, including its directory.
But at any given point, there is usually a current working directory, analogous to the current Mathematica context.
Files that are in this directory can then be specified just by giving their short names.
```



'''Scoping Constructs'''


```txt
With[] evaluate with specified variables replaced by values
Module[] localize names of variables (lexical scoping)
Block[] localize values of variables (dynamic scoping)
DynamicModule[] localize names of variables in dynamic interface constructs

Other Forms of Scoping
Begin, End  localize symbol namespace
Throw, Catch  localize exceptions
Quiet, Check localize messages
BlockRandom localize pseudorandom variables
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
	a = 4; % declare variable and initialize double value,
        s = 'abc'; % string
        i8 = int8(5);	% signed byte
        u8 = uint8(5);	% unsigned byte
        i16 = int16(5);	% signed 2 byte
        u16 = uint16(5); % unsigned 2 byte integer
        i32 = int32(5);	% signed 4 byte integer
        u32 = uint32(5);% unsigned 4 byte integers
        i64 = int64(5);	% signed 8 byte integer
        u64 = uint64(5);% unsigned 8 byte integer
	f32 = float32(5); % single precision floating point number
	f64 = float64(5); % double precision floating point number , float 64 is the default data type.

	c = 4+5i; %	complex number
        colvec = [1;2;4];   % column vector
        crowvec = [1,2,4];   % row vector
        m = [1,2,3;4,5,6];  % matrix with size 2x3
```


Variables within functions have local scope, except when they are declared as global


```Matlab>   global b </lang



## Mercury


Variables are normally implicitly instantiated into their natural scope, if not referred to in the head of the rule (as in <code>Name</code> below) and can only be bound once. This feels very similar to Erlang, where variables are function-scoped and single-assignment, without declaration. However, Mercury will reorder code to satisfy data dependencies, so assignments can appear to be out of order:


```Mercury
:- func name = string.
name = Name :-
    Name = Title ++ " " ++ Given,
    Title = "Dr.",
    Given = "Bob".
```


Mercury also has state variables, actually pairs of variables, usually instantiated in the head of a rule.


```Mercury
:- pred greet(string::in, io::di, io::uo) is det.
greet(Who, !IO) :-
    io.write_string("Hello", !IO),
    io.format(", %s!\n", [s(Who)], !IO).
```


In this example <code>!IO</code> is the state variable, and this code translates to


```Mercury
:- pred greet(string::in, io::di, io::uo) is det.
greet(Who, !.IO, !:IO) :-
    io.write_string("Hello", !.IO, !:IO),
    io.format(", %s!\n", [s(Who)], !.IO, !:IO).
```


which translates roughly to:


```Mercury
:- pred greet(string::in, io::di, io::uo) is det.
greet(Who, IO0, IO) :-
    io.write_string("Hello", IO0, IO1),
    io.format(", %s!\n", [s(Who)], IO1, IO).
```


<code>!.IO</code> is the bound 'current value' of IO, <code>!:IO</code> is the free 'next value' of IO, and the lexical appearance of these variables matters to their final translation to the normal variables in the third example, where data dependency enforces the intended order of operation (so that "Hello" is always written before the name.)

When state variables are instantiated within a rule, they need explicit instantiation, which you could think of as like a declaration without initialization. The state variable in the following example is existentially quantified, and is used for a temporary string builder:


```Mercury
:- func dr(string) = string.
dr(Name) = Titled :-
    some [!SB] (
        !:SB = string.builder.init,
        put_char(handle, 'D', !SB),
        put_char(handle, 'r', !SB),
        format(handle, " %s", [s(Name)], !SB),
        Titled = to_string(!.SB)
    ).
```


Existential vs. universal quantification comes up again to let you make local use of nondeterministic code, as in the following use of a nondeterministic list.member/2.


```Mercury
    % all_under(N, L)
    % True if every member of L is less than N
    %
:- pred all_under(int::in, list(int)::in) is semidet.
all_under(Limit, L) :-
    all [N] (member(N, L) => N < Limit).
```


In pure Mercury code that only uses functional data types, and only normal functions in higher-order code, variables can be said to be either 'free' or 'ground' where they're found in code, before the goal they're in changes their instantiation (for example to bind a free variable to a value, making it ground). The only exception are the 'state of the world' values used for I/O, which have 'unique' and 'dead' instantiations.

But as soon as you have higher-order predicates or non-functional data types (and this happens very easily even in rookie code, for example if you use the getopt module to handle command line arguments), then instantiations get more complex. For non-functional data types like arrays this might just mean using some special modes in your rules, but here's an extreme example:


```Mercury
:- type accumulator == (pred(int, int, io, io)).
:- inst accumulator == (pred(in, out, di, uo) is det).

:- func accumulator >> accumulator = accumulator.
:- mode in(accumulator) >> in(accumulator) = out(accumulator).
A >> B = C :-
    C = (pred(!.N::in, !:N::out, !.IO::di, !:IO::uo) is det :-
        A(!N, !IO),
        B(!N, !IO)).

:- pred add(int::in, int::in, int::out, io::di, io::uo) is det.
add(N, !Acc, !IO) :-
    io.format("adding %d to accumulator (current value: %d)\n",
        [i(N), i(!.Acc)], !IO),
    !:Acc = !.Acc + N.

:- pred example2(io::di, io::uo) is det.
example2(!IO) :-
    F = add(5)
        >> add(5)
        >> add(-10),
    F(0, X, !IO),
    io.print_line(X, !IO).
```


```txt
adding 5 to accumulator (current value: 0)
adding 5 to accumulator (current value: 5)
adding -10 to accumulator (current value: 10)
0
```


In that last example all of the insts and modes are just there to make the higher-order code work, so this system might seem like only a chore. It helps though in permitting efficient code with uniqueness, and it lets Mercury not always pay the costs of its logical facilities (there's underlying machinery required for a nondet call, which might be backtracked through, which isn't required in a det call). The variable instantiation system can also be used to provide static guarantees similar to those offered by dependently typed languages; an example of this is the non_empty_list instantiation used by the solutions module. This is getting really advanced, but here's a simple example of instantiation (ab)use:


```Mercury
:- module evenodd.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list, string.

:- type coin
    --->    heads
    ;       tails.

:- inst heads for coin/0
    --->    heads.
:- inst tails for coin/0
    --->    tails.

main(!IO) :-
    (if heads(heads, N) then
        print_heads(N, !IO)
    else true).

:- pred print_heads(coin::in(heads), io::di, io::uo) is det.
print_heads(_, !IO) :-
    io.write_string("heads\n", !IO).

:- pred print_tails(coin::in(tails), io::di, io::uo) is det.
print_tails(_, !IO) :-
    io.write_string("tails\n", !IO).

:- pragma foreign_export_enum("C", coin/0, [prefix("COIN_"), uppercase]).

:- pred heads(coin::in, coin::out(heads)) is semidet.
:- pragma foreign_proc("C",
    heads(N::in, M::out(heads)),
    [promise_pure, will_not_call_mercury],
"
    M = N;
    SUCCESS_INDICATOR = N == COIN_HEADS;
").

```


Here, <code>print_heads</code> and <code>print_tails</code> take, by type, a coin, which could be either heads or tails, but this code has used the variable instantiation system to insist that they can each only accept one particular value. If you take this code and change the main/2 so that it instead tries to call <code>print_tails(N, !IO)</code>, you'll get a compile-time error. A practical use of this is to avoid having to handle impossible (but well-typed) cases.

=={{header|Modula-3}}==

```modula3
MODULE Foo EXPORTS Main;

IMPORT IO, Fmt;

VAR foo: INTEGER := 5; (* foo is global (to the module). *)

PROCEDURE Foo() =
  VAR bar: INTEGER := 10; (* bar is local to the procedure Foo. *)
  BEGIN
    IO.Put("foo + bar = " & Fmt.Int(foo + bar) & "\n");
  END Foo;

BEGIN
  Foo();
END Foo.
```


For procedures, the formal parameters create local variables unless the actual parameter is prefixed by VAR:

```modula3
PROCEDURE Foo(n: INTEGER) =
```

Here, <tt>n</tt> will be local to the procedure Foo, but if we instead wrote:

```modula3
PROCEDURE Foo(VAR n: INTEGER) =
```

Then <tt>n</tt> is the global variable <tt>n</tt> (if it exists).


## Nim


```nim
var x: int = 3

var y = 3 # type inferred to int

var z: int # initialized to 0

let a = 13 # immutable variable

var
  b, c: int = 10
  s = "foobar"
```



## Objeck

Different ways to declare and initialize an integer.

```objeck

a : Int;
b : Int := 13;
c := 7;

```



## OCaml

The default handlers for values in OCaml are not variables ''strictly speaking'', because as OCaml is a functional language these values can't vary (so are not variable). ''Strictly speaking'' these are bindings. An identifier is bound to a value in an immutable way.

The standard way to bind an identifier to a value is the '''let''' construct:

```ocaml>let x = 28</lang


This stated, ocaml programmers most often use the word ''variable'' when they refer to bindings, because in the programming world we usually use this word for the default values handlers.

Now to add confusion, real variables also exist in OCaml because it is an ''impure'' functional language. They are called references and are defined this way:

```ocaml>let y = ref 28</lang

References can then be accessed and modified this way:

```ocaml
  !y       (* access *)
  y := 34  (* modification *)
```


An identifier can not be declared uninitialised, it is always defined with an initial value, and this initial value is used by the OCaml type inference to infer the type of the binding.

Inside an expression, bindings are defined with the '''let .. in''' construct, and we can also define multiple bindings with the '''let .. and .. in''' construct (here the expression can be the definition of a new identifier or the definition of a function):

```ocaml
let sum =     (* sum is bound to 181 *)
  let a = 31
  and b = 150 in
  (a + b)

let sum () =  (* sum is a function which returns 181 *)
  let a = 31
  and b = 150 in
  (a + b)
```



## Oforth


Oforth does not have :

- Global variables.

- Class attributes.

Apart from the data stack that is global to a task, a function or method cannot modify something global.

Variables are local to functions or methods that declare them.
They are a list of names between two pipes, declared at the beginning of a body.
They are always initialized with null value.

Using a variable just push its value on the data stack.
Using -> removes the top of the stack and assign this value to the variable


```Oforth
import: date

: testVariable
| a b c |
   Date now ->a
   a println ;
```



## ooRexx

While REXX (and ooRexx) normally pass arguments ''by value''. ooRexx has a mechanism to pass compound variables ''by reference''.
<lang>a.=4711
Say 'before sub a.3='a.3
Call sub a.
Say ' after sub a.3='a.3
Exit
sub: Procedure
use Arg a.
a.3=3
Return
```

```txt
before sub a.3=4711
 after sub a.3=3
```



## Openscad



```openscad

mynumber=5+4; // This gives a value of nine

```



## Oz

Variable names in Oz always start with an uppercase letter.

Oz variables are dataflow variables. A dataflow variable can basically be free (unbound) or determined (has a value). Once a value has been assigned, it can not be changed. If we assign the same value again, nothing happens. If we assign a different value to an already determined variable, an exception is raised:

```oz
declare
Var         %% new variable Var, initially free
{Show Var}
Var = 42    %% now Var has the value 42
{Show Var}
Var = 42    %% the same value is assigned again: ok
Var = 43    %% a different value is assigned: exception
```


In the Emacs-based <em>interactive environment</em>, <code>declare</code> creates a new open scope in which variables can be declared. The variables are visible for the entire rest of the session.

Most operations on free variables block until the variables have been bound (but not <code>Show</code> as used above).

Assignment to dataflow variables is also called unification. It is actually a symmetric operation, e.g. the following binds B to 3:

```oz
declare
  A = 3
  B
in
  A = B
 {Show B}
```


However, variables can only be introduced at the left side of the <code>=</code> operator. So this is a syntax error:

```oz
declare
  A = 3
  A = B  %% Error: variable B not introduced
in
 {Show B}
```


It is possible to introduce multiple variables in a single statement:

```oz
declare
   [A B C D] = [1 2 3 4]  %% unification of two lists
```


In a <em>module definition</em>, toplevel variables can be introduced between the keywords <code>define</code> and <code>end</code> without the need for <code>declare</code>. The range between these two keywords is also their scope. Toplevel variables can optionally be exported.

```oz
functor
export Function
define
   ToplevelVariable = 42

   fun {Function}
     42
   end
end
```


Function and class definitions introduce a new variable with the name of the function/class and assign the new function/class to this variable.

Most Oz statement introduce a new scope and it is possible to introduce local variables at the top of this scope with the <code>in</code> keyword.

```oz
fun {Function Arg}
   LocalVar1
in
   LocalVar1 = if Arg == 42 then
		  LocalVar2
	       in
		  LocalVar2 = yes
		  LocalVar2
	       else
		  LocalVar3 = no  %% variables can be initialized when declared
	       in
		  LocalVar3
	       end
   LocalVar1
end
```

Here, <code>LocalVar1</code> is visible in the whole body of <code>Function</code> while <code>LocalVar2</code> is only visible in the <code>then</code> branch and <code>LocalVar3</code> is only visible in the <code>else</code> branch.

Additionally, new local variables can be introduced everywhere using the keyword <code>local</code>.

```oz
if {IsEven 42} then
   {System.showInfo "Here, LocalVar is not visible."}
   local
      LocalVar = "Here, LocalVar IS visible"
   in
      {System.showInfo LocalVar}
   end
end
```


New variables are also introduced in pattern matching.

```oz
case "Rosetta code" of First|_ then {Show First} end %% prints "R"
```

<code>_</code> creates a new nameless variable that is initially unbound. It is usually pronounced "don't care".

It is possible to create a read-only view of a variable with the <code>!!</code> operator. This is called a "future". We can wait for such a variable to become bound by another thread and we can read its value, but we can never set it.

```oz
declare
  A
  B = !!A %% B is a read-only view of A
in
  thread
     B = 43 %% this blocks until A is known; then it fails because 43 \= 42
  end
  A = 42
```


Additional operations on variables:

```oz
declare
  V = 42
in
  {Wait V}  %% explicitly wait for V to become determined

  if {IsDet V} then  %% check whether V is determined; not recommended
     {Show determined}
  elseif {IsFree V} then  %% check whether V is free; not recommended
     {Show free}
  end
```

<code>IsFree</code> and <code>IsDet</code> are low-level functions. If you use them, you code is no longer declarative and prone to race conditions when used in a multi-threaded context.

To have mutable references like in imperative languages, use cells:

```oz
declare
  A = {NewCell 42}
  OldVal
in
  {Show @A}         %% read a cell with @
  A := 43           %% change its value
  OldVal = A := 44  %% read and write at the same time (atomically)
```


<code>A</code> is an immutable dataflow variable that is bound to a mutable reference.


## PARI/GP

There are two types of local variables, <code>local</code> (mostly deprecated) and <code>my</code>. Variables can be used without declaration or initialization; if not previously used such a variable is a pure variable: technically, a monomial in a variable with name equal to the variable name.  This behavior can be forced with the apostrophe operator: regardless of the value (if any) currently stored in <code>x</code>,

```parigp
'x
```

displays as (and is treated internally as) <code>x</code>.  This is useful when you want to use it as a variable instead of a number (or other type of object).  For example,

```parigp
'x^3+7
```

is a cubic polynomial, not the number 8, even if x is currently 1.


## Pascal

See [[Variables#Delphi | Delphi]]


## Perl


In perl, variables are global by default and can be manipulated from anywhere in the program. Variables can be used without first being declared, providing that the strict pragmatic directive is not in effect:


```perl
sub dofruit {
  $fruit='apple';
}

dofruit;
print "The fruit is $fruit";
```


Variables can be declared prior to use and may be prefixed with [[scope modifiers]] <code>our</code>, <code>my</code>, or <code>local</code> see [[scope modifiers]] for the differences. Variables which haven't been assigned to have the undefined value by default. The undefined value acts just like <code>0</code> (if used as a number) or the empty string (if used as a string), except it can be distinguished from either of these with the <code>defined</code> function. If warnings are enabled, perl will print a message like "Use of uninitialized value $foo in addition (+)" whenever you use the undefined value as a number or string.

Initialization and assignment are the same thing in Perl: just use the <code>=</code> operator. Note that the rvalue's context (scalar or list) is determined based on the lvalue.


```perl
my $x = @a;                   # Scalar assignment; $x is set to the
                              # number of elements in @a.
my ($x) = @a;                 # List assignment; $x is set to the first
                              # element of @a.
my @b = @a;                   # List assignment; @b becomes the same length
                              # as @a and each element becomes the same.
my ($x, $y, @b) = @a;         # List assignment; $x and $y get the first
                              # two elements of @a, and @b the rest.
my ($x, $y, @b, @c, $z) = @a; # Same thing, and also @c becomes empty
                              # and $z undefined.
```


The kind of value a variable can hold depends on its sigil, "sigil" being a slang term for "funny character in front of a variable name". <code>$dollarsigns</code> can hold scalars: the undefined value, numbers, strings, or [http://perldoc.perl.org/perlref.html references]. <code>@atsigns</code> can hold arrays of scalars, and <code>%percentsigns</code> can hold hashes of scalars (associative arrays mapping strings to scalars); nested data structures are constructed by making arrays or hashes of references to arrays or hashes.

There are two other sigils, but they behave quite unlike the others. A token of the form <code>&foo</code> refers to a subroutine named <code>foo</code>. In older versions of Perl, ampersands were necessary for calling user-defined subroutines, but since they no longer are, they have only a handful of obscure uses, like making references to named subroutines. Note that you can't assign to an ampersand-marked name. But you can assign to a typeglob, a kind of object represented with the notation <code>*var</code>. A typeglob <code>*foo</code> represents the symbol-table entry for all of the otherwise independent variables <code>$foo</code>, <code>@foo</code>, <code>%foo</code>, and <code>&foo</code>. Assigning a string <code>"bar"</code> to <code>*foo</code> makes these variables aliases for <code>$bar</code>, <code>@bar</code>, <code>%bar</code>, and <code>&bar</code> respectively. Alternatively, you can assign a reference to a typeglob, which creates an alias only for the variable of the appropriate type. In particular, you can say <code>*twiddle = sub {...}</code> to change the definition of the subroutine <code>&twiddle</code> without affecting <code>$twiddle</code> and friends.


###  The strict pragmatic directive


If the strict pragmatic directive is in effect, then variables need explicit scope declaration, so should be prefixed with a my or our keyword depending on the required level of scope:


```perl
use strict;
our $fruit;             # declare a variable as global
our $veg = "carrot";    # declare a global variable and define its value
```



###  Local and global variables


The following example shows the use of local and global variables:


```perl
$fruit="apple";    # this will be global by default

sub dofruit {
  print "My global fruit was $fruit,";    # use the global variable
  my $fruit="banana";                      # declare a new local variable
  print "and the local fruit is $fruit.\n";
}

dofruit;
print "The global fruit is still $fruit";
```



## Perl 6

Much of what is true for Perl 5 is also true for Perl 6. Some exceptions:

There are no typeglobs in Perl 6.

Assigning an array to a scalar variable now makes that scalar variable a reference to the array:


```Perl6

my @y = <A B C D>; # Array of strings 'A', 'B', 'C', and 'D'
say @y[2]; # the @-sigil requires the container to implement the role Positional
@y[1,2] = 'x','y'; # that's where subscripts and many other things come from
say @y; # OUTPUT«[A x y D]␤» # we start to count at 0 btw.

my $x = @y; # $x is now a reference for the array @y

say $x[1]; # prints 'x' followed by a newline character

my Int $with-type-check; # type checks are enforced by the compiler

my Int:D $defined-i = 10; # definedness can also be asked for and default values are required in that case

my Int:D $after-midnight where * > 24 = 25; # SQL is fun and so is Perl 6

my \bad = 'good'; # if you don't like sigils
say bad; # you don't have to use them
say "this is quite bad"; # but then string interpolation
say "this is quite {bad}" # becomes more wordy

```


Laziness is a big topic in Perl 6. Sometimes Perl programmers are so lazy, they can't even be bothered with giving [http://design.perl6.org/S02.html#Names_and_Variables variables names].


```perl6
say ++$; # this is an anonymous state variable
say ++$; # this is a different anonymous state variable, prefix:<++> forces it into numerical context and defaults it to 0
say $+=2 for 1..10; # here we do something useful with another anonymous variable

sub foo { $^a * $^b } # for positional arguments we often can't be bothered to declare them or to give them fancy names
say foo 3, 4;
```


```txt
1
1
2
4
6
12
```


(Includes code modified from http://design.perl6.org/S02.html#Built-In_Data_Types. See this reference for more details.)


## Phix


Phix has just five builtin data types:

```txt

        <-------- object --------->
        |                |
        +-atom           +-sequence
          |                |
          +-integer        +-string

```

<ul>
<li>An <b>object</b> can hold any Phix data, specifically either an atom or a sequence.</li>
<li>An <b>atom</b> can hold a single floating point numeric value, or an integer.
<li>An <b>integer</b> can hold a single whole number (at least +/- 1,000,000,000).
<li>A <b>sequence</b> can hold a collection of values, nested to any depth, or a string
<li>A <b>string</b> can hold a series of characters, or raw binary data.
</ul>


Unlike traditional statically typed languages, you do not have to remember dozens of possibly complex data types, and unlike a dynamically typed language you get proper
compile-time error messages for simple typos. Phix also allows user defined types, for extra validation and debugging purposes, rather than being fundamentally different to the above.
Strings are mutable and the lengths of sequences and strings are only limited by available memory. The sequence is the real powerhouse of Phix, and can be used for lists, tables, trees, etc.

Identifiers may be of any length and are case sensitive. They must start with a letter and then be followed by letters, digits or underscores.

Variables must be declared before use, may be assigned on declaration, and can utilise multiple assignment/sequence decomposition, eg:

```Phix
integer x = 25, y = 25, z
object {a, b, c} = {{}, 5, "string"}
```

In the latter statement, c is set to "string", b is set to 5, and a is set to {}. (In multiple assignment, values are assigned right-to-left to avoid having to reorder any subscripts.) You could also use
the @= ("all equal") operator to assign a, b and c to the same (entire) thing.
Attempts to reference a variable before it has been assigned a value trigger a run-time error, except for object(<i>id</i>) which yields false in that case.
Constants are really just variables which must be assigned on declaration and for which subsequent assignment is prohibited.

Variables are by default local and restricted to the current file, routine, or block level. Static file-level variables may also be declared as global to make them visible to other source files. There are no static routine scoped variables, only file-level. Scope resolution is intended to be simple and intuitive, see the link below (Core Language/Declarations/Scope) for more details.

User defined types are declared with a single-parameter function that returns true or false, eg:

```Phix
type hour(integer x)
    return x >= 0 and x <= 23
end type
hour h1, h2
h1 = 10      -- ok
h2 = 25      -- error! program aborts with a message
```

Phix has no notion of unsigned numeric types, except via user defined types such as the above which explicitly prevent their use.

You could theoretically write an entire application declaring all variables and parameters as type object, except that it would probably not catch errors the way you might expect it to.

An online copy of the manual can be found at http://phix.x10.mx/docs/html/phix.htm


## PHP


```PHP
<?php
/**
 * @author Elad Yosifon
 */


/*
 * PHP is a weak typed language,
 * no separation between variable declaration, initialization and assignment.
 *
 * variable type is defined by the value that is assigned to it.
 * a variable name must start with a "$" sign (called "sigil", not a dollar sign).
 * variable naming rules:
 *     + case-sensitive.
 *     + first character after $ must not be a number.
 *     + after the first character all alphanumeric chars and  _(underscore) sign is allowed, e.g. $i_am_a_new_var_with_the_number_0
 *
 */

# NULL typed variable
$null = NULL;				var_dump($null);	// output: null

# defining a boolean
$boolean = true;			var_dump($boolean);	// output: boolean true
$boolean = false;			var_dump($boolean);	// output: boolean false

/*
 * casting is made using (TYPE) as a prefix
 */
# bool and boolean is the same
$boolean = (bool)1;			var_dump($boolean);	// output: boolean true
$boolean = (boolean)1;			var_dump($boolean);	// output: boolean true

$boolean = (bool)0;			var_dump($boolean);	// output: boolean false
$boolean = (boolean)0;			var_dump($boolean);	// output: boolean false

# defining an integer
$int = 0;				var_dump($int);		// output: int 0

# defining a float,
$float = 0.01;				var_dump($float);	// output: float 0.01

# which is also identical to "real" and "double"
var_dump((double)$float);					// output: float 0.01
var_dump((real)$float);						// output: float 0.01

# casting back to int (auto flooring the value)
var_dump((int)$float);						// output: int 0
var_dump((int)($float+1));					// output: int 1
var_dump((int)($float+1.9));					// output: int 1

# defining a string
$string = 'string';
var_dump($string);						// output: string 'string' (length=6)

# referencing a variable (there are no pointers in PHP).
$another_string = &$string;
var_dump($another_string);
								// output: string 'string' (length=6)

$string = "I'm the same string!";
var_dump($another_string);
								// output: string 'I'm the same string!' (length=20)
# "deleting" a variable from memory
unset($another_string);

$string = 'string';
/*
 * a string can also be defined with double-quotes, HEREDOC and NOWDOC operators.
 * content inside double-quotes is being parsed before assignment.
 * concatenation operator is .=
 */
$parsed_string = "This is a $string";
var_dump($parsed_string);
								// output: string 'This is a string' (length=16)
$parsed_string .= " with another {$string}";
var_dump($parsed_string);
								// output: string 'This is a string with another string' (length=36)

# with string parsing
$heredoc = <<<HEREDOC
This is the content of \$string: {$string}
HEREDOC;
var_dump($heredoc);
								// output: string 'This is the content of $string: string' (length=38)

# without string parsing (notice the single quotes surrounding NOWDOC)
$nowdoc = <<<'NOWDOC'
This is the content of \$string: {$string}
NOWDOC;
var_dump($nowdoc);
								// output: string 'This is the content of \$string: {$string}' (length=42)

# as of PHP5, defining an object typed stdClass => standard class
$stdObject = new stdClass();	var_dump($stdObject);
								// output: object(stdClass)[1]
# defining an object typed Foo
class Foo {}
$foo = new Foo();		var_dump($foo);
								// output: object(Foo)[2]
# defining an empty array
$array = array();		var_dump($array);
								// output: array {empty}

/*
 * an array with non-integer key is also considered as an associative array(i.e. hash table)
 * can contain mixed variable types, can contain integer based keys and non-integer keys
 */
$assoc = array(
	0 => $int,
	'integer' => $int,
	1 => $float,
	'float' => $float,
	2 => $string,
	'string' => $string,
	3 => NULL, // <=== key 3 is NULL
	3, // <=== this is a value, not a key (key is 4)
	5 => $stdObject,
	'Foo' => $foo,
);
var_dump($assoc);

// output:
//
### =

//	array
//	    0 => int 0
//	    'integer' => int 0
//	    1 => float 0.01
//	    'float' => float 0.01
//	    2 => string 'string' (length=6)
//	    'string' => string 'string' (length=6)
//	    3 => null
//	    4 => int 3
//	    5 =>
//	    	object(stdClass)[1]
//	    'Foo' =>
//		    object(Foo)[2]



/*
 * all variables are "global" but not reachable inside functions(unless specifically "globalized" inside)
 */

function a_function()
{
	# not reachable
	var_dump(isset($foo));				// output: boolean false

	global $foo;
	# "global" (reachable) inside a_function()'s scope
	var_dump(isset($foo));				// output: boolean true
}

a_function();

/**
 * there is another special type of variable called (Resource).
 * for more info regarding Resources:
 * @url http://php.net/manual/en/language.types.resource.php
 * @url http://php.net/manual/en/resource.php
 */

```



## PicoLisp

You can control the local bindings of symbols with functions like
'[http://software-lab.de/doc/refU.html#use use]' or
'[http://software-lab.de/doc/refL.html#let let]':

```PicoLisp
(use (A B C)
   (setq A 1  B 2  C 3)
   ... )
```

This is equivalent to

```PicoLisp
(let (A 1  B 2  C 3)
   ... )
```

The parentheses can be omitted if there is only a single variable

```PicoLisp
(use A
   (setq A ..)
   ... )

(let A 1
   ...)
```

Other functions that handle local bindings are
'[http://software-lab.de/doc/refL.html#let? let?]',
'[http://software-lab.de/doc/refB.html#bind bind]',
'[http://software-lab.de/doc/refJ.html#job job]',
'[http://software-lab.de/doc/refW.html#with with]' or
'[http://software-lab.de/doc/refF.html#for for]'.


## PL/I


```pli
/* The PROCEDURE block and BEGIN block are used to delimit scopes. */

declare i float; /* external, global variable, excluded from the */
                 /* local area (BEGIN block) below.              */
begin;
   declare (i, j) fixed binary; /* local variable */
   get list (i, j);
   put list (i,j);
end;

/* Examples of initialization. */

declare p fixed initial (25);
declare q(7) fixed initial (9, 3, 5, 1, 2, 8, 15);
   /* sets all elements of array Q at run time, on block entry. */
declare r(7) fixed initial (9, 3, 5, 1, 2, 8, 15);
   /* sets all elements of array R at compile time. */

p = 44; /* run-time assignment. */
q = 0; /* run-time initialization of all elements of Q to zero. */
q = r; /* run-time assignment of all elements of array R to     */
       /* corresponding elemets of S.                           */
```



## Pony


```Pony
var counter: I32 = 10 // mutable variable 'counter' with value 10
let temp: F64 = 36.6  // immutable variable 'temp'
let str: String       // immutable variable 'str' with no initial value
str = "i am a string" // variables must be initialized before use
let another_str = "i am another string" // type of variable 'another_str' infered from assigned value

let b  = true
let b' = false // variable names can contain ' to make a distinct variable with almost the same name
```


Destructive read

```Pony
var x: I32 = 10
var y: I32 = x = 20
try
  Fact(x == 20) // x gets the new value of 20
  Fact(y == 10) // y gets the old value of x which is 10
end
```



## Prolog

Variables in imperative languages are, in essence, named memory locations where items of data can be stored. Prolog, as a declarative language, has no variables in this sense, and therefore has no way of declaring them or assigning to them. Prolog variables are more like variables in formal logic or in mathematics. Here is a very simple Prolog program:

```prolog
mortal(X) :- man(X).
man(socrates).
```

The symbol <tt>:-</tt> may be read as 'when' or 'if', so that the first line is equivalent to the statement in predicate logic ∀<i>x</i>: <i>px</i> → <i>qx</i> where <i>px</i> is interpreted as '<i>x</i> is a man' and <i>qx</i> is interpreted as '<i>x</i> is mortal'. Prolog notation, however, requires variable names to start with (or consist of) a capital letter: this is how the interpreter knows that <tt>socrates</tt> is not a variable.

We can of course use more than one variable:

```prolog
student(X,Y) :- taught(Y,X).
taught(socrates,plato).
```

When we put queries to the Prolog system, it will seek to find a consistent set of interpretations that allows our query to be true in the light of the facts and rules we have provided:

```prolog
?- mortal(socrates).
yes
?- student(X,socrates).
X=plato
?- student(socrates,X).
no
```

And so forth. We can reuse the same variable names in different statements as much as we like, but within each statement the same variable must be capable of referring to the same term.

```prolog
?- mortal(zeus).
no
```

Prolog does not answer like that because it is a connoisseur of the mythology; and in any case several ancient writers report that Zeus's tomb could be seen on Crete. But the rule states that <tt>X</tt> is mortal if <tt>X</tt>—the same <i>x</i>—is a man, so it could only unify successfully if <tt>zeus</tt> and <tt>socrates</tt> were the same (which even his most devoted admirers did not claim). If our original rule had said

```prolog
mortal(X) :- man(Y).
```

then the two variables would be able to refer to two different terms, and the Prolog interpreter would agree that <tt>mortal(zeus)</tt>.


## PureBasic


```PureBasic
; Variables are initialized when they appear in sourcecode with default value of 0 and type int
Debug a
; or value "" for a string, they are not case sensitive
Debug b$
; This initializes a double precision float, if type is following the dot
Debug c.d
; They can be initialized with define (double precision float, string, integer)
Define d.d = 3.5, e$ = "Test", f.i = a + 2
; Define can have a default type (all bytes except j which is long):
Define.b g, h, j.l
; Define without following variables sets default type. In this case to single precision float
Define.f
; So this will be an single precision float and no integer
Debug k
; EnableExplicit forces declaration of used variables with define
EnableExplicit
; Will throw an error because L isn't initialized
Debug L
DisableExplicit
; Global Variables are available in Procedures and Threads too
Global M = 3, N = 2
Procedure Dummy(parameter1, parameter2 = 20)
  ; Parameter contain values which where used when calling the function,
  ; their types have to be specified in the above Procedure header.
  ; The last ones can have default values which get applied if this parameter is not given.

  ; Variables in Procedures are separate from those outside,
  ; so d can be initialized again with another type
  ; which would otherwise lead to an error
  d.i
  ; Protected makes a variable local even if another one with same name is declared as global (see above)
  Protected M = 2
  ; Shares a variable with main program like it was declared by global
  Shared a
  ; prevents a variable to be initialized with default value again when procedure is called a second time,
  ; could be used for example as a counter, which contains the number of times a function was called
  Static a
  ; N here also would have a value of 2, while for example
  ; f would, when named, initialize a new variable, and so have a value of 0
EndProcedure
; finally there are constants which are prefixed by an #:
#Test = 1
; Their value cannot be changed while program is running
#String_Constant = "blubb"
; In constrast  to variables, a constant has no types except an (optional) $ sign to mark  it as string constant
#Float_Constant = 2.3
; Maps, LinkedLists , Arrays and Structures are not handled here, because they are no elemental variables
```



## PowerShell

Variables in PowerShell start with a $ character, they are created on assignment and thus don't need to be declared:

```powershell
$s = "abc"
$i = 123
```

Uninitialized variables expand to nothing. This may be interpreted for example as an empty string or 0, depending on context:

```powershell
4 + $foo              # yields 4
"abc" + $foo + "def"  # yields "abcdef"
```

Variables all show up in the '''Variable''': drive and can be queried from there with the usual facilities:

```powershell
Get-ChildItem Variable:
```

Since Variables are provided via a flat filesystem, they can be manipulated using the common cmdlets for doing so. For example to delete a variable one can use

```powershell
Remove-Item Variable:foo
```

as if it were a file or a registry key. There are, however, several cmdlets dealing specifically with variables:

```powershell
Get-Variable      # retrieves the value of a variable
New-Variable      # creates a new variable
Set-Variable      # sets the value of a variable
Clear-Variable    # deletes the value of a variable, but not the variable itself
Remove-Variable   # deletes a variable completely
```



## Python

[http://python.net/~goodger/projects/pycon/2007/idiomatic/handout.html#python-has-names Names] in Python are not typed, although all the objects referred to by them, are. Names are lexically scoped by function/method/class definitions, and must be defined before use.

Names in global statements are looked up in the outermost context of the program or module. Names in a nonlocal statement are looked up in the order of closest enclosing scope outwards.


```python

# these examples, respectively, refer to integer, float, boolean, and string objects
example1 = 3
example2 = 3.0
example3 = True
example4 = "hello"

# example1 now refers to a string object.
example1 = "goodbye"

```



## R

Variables are dynamically typed, so they do not need to be declared and instantiated separately.  <- and = are both used as the assignment operator, though <- is preferred, for compatibility with S-Plus code.

```R
foo <- 3.4
bar = "abc"
```

It is possible to assign multiple variables with the same value, and to assign values from left to right.

```R
baz <- quux <- 1:10
TRUE -> quuux
```

There are also global assignment operators, <<- and ->>.  From their help page:
 The operators '<<-' and '->>' cause a search to made through the
 environment for an existing definition of the variable being
 assigned.  If such a variable is found (and its binding is not
 locked) then its value is redefined, otherwise assignment takes
 place in the global environment.
In practice, this usually means that variables are assigned in the user workspace (global environment) rather than a function.

```R
a <- 3

assignmentdemo <- function()
{
   message("assign 'a' locally, i.e. within the scope of the function")
   a <- 5
   message(paste("inside assignmentdemo, a = ", a))
   message(paste("in the global environment, a = ", get("a", envir=globalenv())))

   message("assign 'a' globally")
   a <<- 7
   message(paste("inside assignmentdemo, a = ", a))
   message(paste("in the global environment, a = ", get("a", envir=globalenv())))
}
assignmentdemo()
```

 assign 'a' locally, i.e. within the scope of the function
 inside assignmentdemo, a =  5
 in the global environment, a =  3
 assign 'a' globally
 inside assignmentdemo, a =  5
 in the global environment, a =  7
Finally, there is also the assign function, where you choose the environment to assign the variable.

```R
assign("b", TRUE)                         #equivalent to b <- TRUE
assign("c", runif(10), envir=globalenv()) #equivalent to c <<- runif(10)
```



## Racket



```Racket

#lang racket

;; define a variable and initialize it
(define foo 0)
;; increment it
(set! foo (add1 foo))

;; Racket is lexically scoped, which makes local variables work:
(define (bar)
  (define foo 100)
  (set! foo (add1 foo))
  foo)
(bar) ; -> 101

;; and it also makes it possible to have variables with a global value
;; that are accessible only in a specific local lexical scope:
(define baz
  (let () ; used to create a new scope
    (define foo 0)
    (define (bar)
      (set! foo (add1 foo))
      foo)
    bar)) ; this is the value that gets bound to `baz'
(list (baz) (baz) (baz)) ; -> '(1 2 3)

;; define a new type, and initialize a variable with an instance
(struct pos (x y))
(define p (pos 1 2))
(list (pos-x p) (pos-y p)) ; -> '(1 2)

;; for a mutable reference, a struct (or some specific fields in a
;; struct) can be declared mutable
(struct mpos (x y) #:mutable)
(define mp (mpos 1 2))
(set-mpos-x! mp 11)
(set-mpos-y! mp 22)
(list (mpos-x mp) (mpos-y mp)) ; -> '(11 22)

;; but for just a mutable value, we have boxes as a builtin type
(define b (box 10))
(set-box! b (add1 (unbox b)))
(unbox b) ; -> 11

;; (Racket has many more related features: static typing in typed
;; racket, structs that are extensions of other structs,
;; pattern-matching on structs, classes, and much more)

```



## Rascal

The effect of a variable declaration is to introduce a new variable Name and to assign the value of expression Exp to Name. A variable declaration has the form

```rascal> Type Name = Exp;</lang

A mention of Name later on in the same scope will be replaced by this value, provided that Name’s value has not been changed by an intermediate assignment. When a variable is declared, it has as scope the nearest enclosing block, or the module when declared at the module level.

There are two rules you have to take into account. Double declarations in the same scope are not allowed. Additionally, the type of Exp should be compatible with Type, i.e., it should be a subtype of Type.

As a convenience, also declarations without an initialization expression are permitted inside functions (but not at the module level) and have the form

```rascal>Type Name;</lang

and only introduce the variable Name.

Rascal provides local type inference, which allows the implicit declaration of variables that are used locally in functions. There are four rules that apply when doing so.
(1) An implicitly declared variable is declared at the level of the current scope, this may the whole function body or a block nested in it.
(2) An implicitly declared variable gets as type the type of the first value that is assignment to it.
(3) If a variable is implicitly declared in different execution path of a function, all these implicit declarations should result in the same type.
(4) All uses of an implicitly declared variable must be compatible with its implicit type.

'''Examples'''

Two explicit variable declarations:

```rascal>rascal
int max = 100;
int: 100
rascal>min = 0;
int: 0
```


An implicit variable declaration

```rascal>rascal
day = {<"mon", 1>, <"tue", 2>, <"wed",3>,
>>>>>>>       <"thu", 4>, <"fri", 5>, <"sat",6>, <"sun",7>};
rel[str, int]: {
  <"thu",4>,
  <"mon",1>,
  <"sat",6>,
  <"wed",3>,
  <"tue",2>,
  <"fri",5>,
  <"sun",7>
}
```


Variable declaration and assignment leading to type error

```rascal>rascal
int month = 12;
int: 12
rascal>month ="December";
|stdin:///|(7,10,<1,7>,<1,17>): Expected int, but got str
```


Pitfalls
Local type inference for variables always uses the smallest possibe scope for a variable; this implies that a variable introduced in an inner scope is not available outside that scope. Here is how things can go wrong:

```rascal>rascal
if( 4 > 3){ x = "abc"; } else { x = "def";}
str: "abc"
rascal>x;
|stdin:///|(0,1,<1,0>,<1,1>): Undeclared variable, function or constructor: x
```




## REXX


### assignments via =

REXX has only one type of variable:   a (character) string.


There is no need to declare anything   (variables, entry points, subroutines, functions, etc)   (indeed, there isn't any way to declare anything at all).

All unassigned REXX variables have a default value of the variable name (in uppercase).

There isn't any way to initialize a variable except to assign it a value   (by one of the methods below), however,

there is a way to "initialize" a (stemmed) array in REXX with a default value) ── see the 6<sup>th</sup> section below,

'''default value for an array'''.

To assign some data (value) to a variable, one method is to use the assignment operator, an equal sign   (<big>'''='''</big>):

```rexx
aa = 10                                /*assigns chars    10  ───►  AA  */
bb = ''                                /*assigns a null value ───►  BB  */
cc = 2*10                              /*assigns chars    20  ───►  CC  */
dd = 'Adam'                            /*assigns chars Adam   ───►  DD  */
ee = "Adam"                            /*same as above        ───►  EE  */
ff = 10.                               /*assigns chars   10.  ───►  FF  */
gg = '10.'                             /*same as above        ───►  GG  */
hh = "+10"                             /*assigns chars  +10   ───►  hh  */
ii = 1e1                               /*assigns chars  1e1   ───►  ii  */
jj = +.1e+2                            /*assigns chars  .1e+2 ───►  jj  */
```

Variables   '''aa,   ff,   gg,   hh,   ii,'''   and  '''jj'''   will all be considered ''numerically equal'' in REXX   (but not exactly equal ''except'' for   '''ff'''   and   '''gg''').


### assignments via PARSE


```rexx
kk = '123'x                            /*assigns hex  00000123 ───► KK  */
kk = 'dead beaf'X                      /*assigns hex  deadbeaf ───► KK  */
ll = '0000 0010'b                      /*assigns blank ───► LL  (ASCII) */
mm = '0000 0100'B                      /*assigns blank ───► MM  (EBCDIC)*/

xxx = '11 2. 333 -5'
parse var xxx   nn oo pp qq rr
                                       /*assigns     11    ───►  NN     */
                                       /*assigns     2.    ───►  OO     */
                                       /*assigns    333    ───►  PP     */
                                       /*assigns     ─5    ───►  QQ     */
                                       /*assigns   "null"  ───►  RR     */

                           /*a  "null"  is a string of length zero (0), */
                           /*and is not to be confused with a null char.*/

cat = 'A cat is a lion in a jungle of small bushes.'
                                       /*assigns a literal ───►  CAT    */
```



### assignments via VALUE

Assignments can be made via the   '''value'''   BIF   ['''B'''uilt-'''I'''n '''F'''unction]   (which also has other capabilities), the

capability used here is to create a variable name programmatically (normally using concatenation or abuttal).

```rexx
call value 'CAT', "When the cat's away, the mice will play."
                                       /*assigns a literal ───►  CAT    */
yyy='CA'
call value yyy'T', "Honest as the Cat when the meat's out of reach."
                                       /*assigns a literal ───►  CAT    */
yyy = 'CA'
call value yyy || 'T', "Honest as the Cat when the meat's out of reach."
                                       /*assigns a literal ───►  CAT    */
```



### unassigned variables

There are methods to catch unassigned variables in REXX.

```REXX
/*REXX pgm to do a "bad" assignment  (with an unassigned REXX variable).*/

signal on noValue                      /*usually, placed at pgm start.  */

xxx=aaaaa                              /*tries to assign aaaaa ───► xxx */

say xxx 'or somesuch'
exit

noValue:                               /*this can be dressed up better. */
badLine  =sigl                         /*REXX line number that failed.  */
badSource=sourceline(badLine)          /*REXX source line ···           */
badVar   =condition('D')               /*REXX var name that's ¬ defined.*/
say
say '*** error! ***'
say 'undefined variable' badvar "at REXX line number" badLine
say
say badSource
say
exit 13
```

Note:   the value (result) of the   '''condition'''   BIF can vary in different implementations of a REXX interpreter.


'''output''' using Regina (various versions), PC/REXX, Personal REXX, and ooRexx.

```txt

*** error! ***
undefined variable AAAAA at REXX line number 5

xxx=aaaaa                              /*tries to assign aaaaa ───► xxx */

```

'''output''' using R4 REXX:

```txt

Error 46 : Invalid variable reference (NOVALUE)
Information: Variable aaaaa does not have an assigned value
Error occurred in statement# 5
Statement source: xxx= aaaaa
Statement context: D:\variabl4.rex, procedure: variabl4

```



### scope of variables

REXX subroutines/functions/routines/procedures can have their own "local" variables if the   '''procedure'''   statement is used.

Variables can be shared with the main program if the variables are named on the '''procedure''' statement with the   '''expose'''   keyword.

```rexx
/*REXX pgm shows different scopes of a variable: "global"  and  "local".*/
q = 55          ;    say ' 1st q='  q  /*assign a value ───►  "main"  Q.*/
call sub        ;    say ' 2nd q='  q  /*call a procedure subroutine.   */
call gyro       ;    say ' 3rd q='  q  /*call a procedure with EXPOSE.  */
call sand       ;    say ' 4th q='  q  /*call a subroutine or function. */
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────SUB subroutine──────────────────────*/
sub:  procedure                        /*use PROCEDURE to use local vars*/
q = -777        ;    say ' sub q='  q  /*assign a value ───► "local" Q. */
return
/*──────────────────────────────────GYRO subroutine─────────────────────*/
gyro: procedure expose q               /*use EXPOSE to use global var Q.*/
q = "yuppers"   ;    say 'gyro q='  q  /*assign a value ───► "exposed" Q*/
return
/*──────────────────────────────────SAND subroutine─────────────────────*/
sand:                                  /*all REXX variables are exposed.*/
q = "Monty"     ;    say 'sand q='  q  /*assign a value ───► "global"  Q*/
return
```

'''output'''

```txt

 1st q= 55
 sub q= -777
 2nd q= 55
gyro q= yuppers
 3rd q= yuppers
sand q= Monty
 4th q= Monty

```

Programming note:   there is also a method in REXX to '''expose''' a ''list'' of variables.


### default value for an array

There is a way in REXX to assign a default value (or an initial value, if you will) to a (stemmed) array.

```rexx
aaa. = '───────nope.'                  /*assign this string as a default*/
aaa.1=1                                /*assign   1   to first  element.*/
aaa.4=4.                               /*   "     4    " fourth    "    */
aaa.7='lucky'                          /*   "     7    " seventh   "    */
              do j=0  to 8             /*go through a bunch of elements.*/
              say 'aaa.'||j '=' aaa.j  /*display element # and its value*/
              end                      /*we could've started J at  -100.*/
```



### dropping a variable

In REXX, dropping a variable (this can be thought of as deallocating it or setting the value back to its "default").


Note that the storage used by the variable's (old) value is not truly deallocated, but its storage is returned to the

pool of storage available for allocation of other REXX variables (and their values).   This action isn't mandatory

for the REXX language   (or for that matter, not even specified),   but it's apparently what all   (Classic) REXX

interpreters do at the the time of this writing.

```rexx
radius=6.28                            /*assign a value to a variable.  */
                say 'radius =' radius
drop radius                            /*now, "undefine" the variable.  */
                say 'radius =' radius
```

Note:   The value of an undefined (or deallocated) REXX variable is the uppercased name of the REXX variable name.


'''output'''

```txt

radius = 6.28
radius = RADIUS

```



### compound variables

In additions to the (simple) variables described above, REXX defines
compound variables that consist of a stem (symbol.) followed
by a list of period-separated simple variables and constants
The compound variable's tail is computed by concatenating the
variables' values (which can be any string) with the constants
and the intervening periods.

```rexx
var.='something' /* sets all possible compound variables of stem var. */
x='3 '
var.x.x.4='something else'
Do i=1 To 5
  a=left(i,2)
  Say i var.a.a.4 "(tail is '"a||'.'||a||'.'||'4'"')"
  End
```

Output:

```txt

1 something (tail is '1 .1 .4')
2 something (tail is '2 .2 .4')
3 something else (tail is '3 .3 .4')
4 something (tail is '4 .4 .4')
5 something (tail is '5 .5 .4')

```



## Ring

To create a new variable, you just need to determine the variable name & value. The value will determine the variable type and you can change the value to switch between the types using the same variable name.

Syntax:

```ring

<Variable Name> = <Value>

```


The operator ‘=’ is used here as an Assignment operator and the same operator can be used in conditions, but for testing equality of expressions.

The Variable will contains the real value (not a reference). This means that once you change the variable value, the old value will be removed from memory (even if the variable contains a list or object).

Ring is a dynamic programming language that uses Dynamic Typing.


```ring

x = "Hello"             # x is a string
see x + nl
x = 5                   # x is a number (int)
see x + nl
x = 1.2                 # x is a number (double)
see x + nl
x = [1,2,3,4]           # x is a list
see x                   # print list items
x = date()              # x is a string contains date
see x + nl
x = time()              # x is a string contains time
see x + nl
x = true                # x is a number (logical value = 1)
see x + nl
x = false               # x is a number (logical value = 0)
see x + nl

```


We can use the assignment operator ‘=’ to copy variables. We can do that to copy values like strings & numbers. Also, we can copy complete lists & objects. The assignment operator will do a complete duplication for us. This operation called Deep Copy


```ring

list = [1,2,3,"four","five"]
list2 = list
list = []
See list        # print the first list - no items to print
See "********" + nl
See list2       # print the second list - contains 5 items

```


Ring is a weakly typed language, this means that the language can automatically convert between data types (like string & numbers) when that conversion make sense.

Rules:


```ring

<NUMBER> + <STRING> --> <NUMBER>
<STRING> + <NUMBER> --> <STRING>

```


The same operator ‘+’ can be used as an arithmetic operator or for string concatenation.


```ring

x = 10                  # x is a number
y = "20"                # y is a string
sum = x + y             # sum is a number (y will be converted to a number)
Msg = "Sum = " + sum    # Msg is a string (sum will be converted to a string)
see Msg + nl

```



## Ruby

Information taken from [http://www.rubyist.net/~slagell/ruby/variables.html Variables] page at the [http://www.rubyist.net/~slagell/ruby/ Ruby User's Guide]

<blockquote>Ruby has three kinds of variables, one kind of constant and exactly two pseudo-variables. The variables and the constants have no type. While untyped variables have some drawbacks, they have many more advantages and fit well with ruby's quick and easy philosophy.

Variables must be declared in most languages in order to specify their type, modifiability (i.e., whether they are constants), and scope; since type is not an issue, and the rest is evident from the variable name as you are about to see, we do not need variable declarations in ruby.

The first character of an identifier categorizes it at a glance:

<table border>
<tr><td> $          </td><td> [http://www.rubyist.net/~slagell/ruby/globalvars.html global variable] </td></tr>
<tr><td> @          </td><td> [http://www.rubyist.net/~slagell/ruby/instancevars.html instance variable] </td></tr>
<tr><td> [a-z] or _ </td><td> [http://www.rubyist.net/~slagell/ruby/localvars.html local variable] </td></tr>
<tr><td> [A-Z]      </td><td> [http://www.rubyist.net/~slagell/ruby/constants.html constant] </td></tr>
</table>

The only exceptions to the above are ruby's pseudo-variables: <code>self</code>, which always refers to the currently executing object, and <code>nil</code>, which is the meaningless value assigned to uninitialized variables. Both are named as if they are local variables, but <code>self</code> is a global variable maintained by the interpreter, and <code>nil</code> is really a constant. As these are the only two exceptions, they don't confuse things too much.
</blockquote>

Referencing an undefined global or instance variable returns <code>nil</code>.  Referencing an undefined local variable throws a <code>NameError</code> exception.


```ruby
$a_global_var = 5
class Demo
  @@a_class_var = 6
  A_CONSTANT = 8
  def initialize
    @an_instance_var = 7
  end
  def incr(a_local_var)
    @an_instance_var += a_local_var
  end
end
```



## Scala

Variables in Scala can have three different scopes depending on the place where they are being used. They can exist as fields, as method parameters and as local variables. Below are the details about each type of scope:

* '''Fields''' are variables that belong to an object. The fields are accessible from inside every method in the object. Fields can also be accessible outside the object depending on what access modifiers the field is declared with. Object fields can be both mutable or immutable types and can be defined using respectively <tt>var</tt> or <tt>val</tt>.

* '''Method parameters''' are variables, which are used to pass the value inside a method when the method is called. Method parameters are only accessible from inside the method but the objects passed in may be accessible from the outside, if you have a reference to the object from outside the method. Method parameters are always immutable and defined by <tt>val</tt> keyword. In ''algebraic datatypes'' (Scala's <tt>case</tt> classes), -think records- the parameters looks like method parameters. In this case they are fields.

* '''Local variables''' are variables declared inside a method. Local variables are only accessible from inside the method, but the objects you create may escape the method if you return them from the method. Local variables can be both mutable or immutable types and can be defined using respectively <tt>var</tt> or <tt>val</tt>.

## Seed7

Seed7 variables must be defined with type and initialization value, before they are used.
There are global variables and variables declared local to a function.

```seed7
$ include "seed7_05.s7i";

var integer: foo is 5;   # foo is global

const proc: aFunc is func
  local
    var integer: bar is 10;   # bar is local to aFunc
  begin
    writeln("foo + bar = " <& foo + bar);
  end func;

const proc: main is func
  begin
    aFunc;
  end func;
```



## Set lang

There is no declared intialization of variables, or datatypes. '''Set''' already initializes 52 variables, named a-z with a value of 0 (ASCII for Null) each, and A-Z with ASCII values corresponding to their characters (65-90). There is also a question mark variable, which represents the number of the current line of code being executed.
<lang set_lang>set a 0        > Useless intialization - All lowercase variables have an initial value of 0
set b 66       > Simple variable assignment - ''b'' is now the ASCII value of 66, or the character 'B'
[c=0] set c 5  > Conditional variable assignment - If ''c'' is 0, then set ''c'' to 5
set ? 6        > A "goto" command; Setting the ''?'' variable defines the line of code to be executed next
set z 1        > This line of code will never be executed, because the previous skipped it
set c 10       > Line #5 skipped to here
set d A        > Assigning a variable to another variable - ''d'' is now ASCII 65, or the character 'A'
set b !        > The ''!'' deals with I/O. Setting a variable to it receives an input character and assigns it to the variable
set ! a        > Setting the exclamation point to a variable outputs that variable
set e (d+1)    > Combiners are defined inside round brackets - () - and have an addition and a subtraction function
set f (e-1)    > Variable ''e'' was assigned to ''d'' + 1 (65 + 1 = 66, character B), and ''f'' was assigned to ''e'' - 1 (66 - 1 = 65, character A)

```




## smart BASIC



###  Data Types

There are two variable data types in smart BASIC; numeric and string.

Numeric variables can contain real and complex numbers.

```qbasic
x = 14
y = 0.4E3
z = 3-2i
```


String variables use the dollar symbol ($) at the end.

```qbasic
t$ = "Name"
```


String values may include the quotation symbol " by specifying double quotes "".

```qbasic
n$ = """This is a quoted text"""
```



### Initialization

Variable names are NOT case sensitive.

Variable names may not begin with a number.

Variables are initialized when assigned as either numeric or strings. However, smart BASIC in unique in that it will automatically interpret a numeric value from a string if utilized in that manner.

```qbasic
n = 4
a$ = "6 feet"
PRINT n * a$
```

```txt

24

```



### Declaration

Variables do not require declaration in smart BASIC.

Only variable arrays need to be declared by size if the array is over 10 elements.

Arrays are DIM-ensioned by default with a size of 10. Larger arrays must be pre-DIM-entioned.

```qbasic
DIM name$(100)
```



### Assignment

The command LET used in standard BASIC may be omitted.

'''LET x = 1''' may be shortened to just '''x = 1'''.


### Precision

Numeric variable precision is limited to 64-bit representation with 1 bit for sign, 11 bits for power, and 52 bits for mantissa. Any integer greater than 2^52 (4,503,599,627,370,496) will be approximated.


### Scope

All variables are local in smart BASIC. Variables within the main program are localized from variables within defined functions as well as other defined functions. However, variables may be accessed outside of their localized areas by preceding the variable with the function name and a period. By extension, the main program scope is null, therefore non-function variables may be accessed within functions with a period before the variable and a null scope (for example: .x).

```qbasic
DEF MyFunction(x)
MyF2 = x^2
MyF3 = x^3
MyFunction = MyF2 + MyF3
END DEF
```


To access the variables within the function:

```qbasic
x = MyFunction(3)
PRINT x; MyFunction.MyF2; MyFunction.MyF3
```


Variables may also be made global using the DEF command:

```qbasic>DEF num=123</lang



## SNOBOL4


Local variables in Snobol are declared in a function definition prototype string:


```SNOBOL4
        define('foo(x,y)a,b,c') :(foo_end)
foo     a = 1; b = 2; c = 3
        foo = a * ( x * x ) + b * y + c :(return)
foo_end
```


This defines a function foo( ) taking two arguments x,y and three localized variables a,b,c. Both the argument parameters and vars are dynamically scoped to the function body, and visible to any called functions within that scope. The function name also behaves as a local variable, and may be assigned to as the return value of the function. Any variable initialization or assignment is done explicitly within the function body. Unassigned variables have a null string value, which behaves as zero in numeric context.

Snobol does not support static or lexical scoping, or module level namespaces. Any variables not defined in a prototype are global to the program.


## SPL

'''Variable declaration.'''
In SPL variables do not need and do not have declaration.

'''Initialization.'''
In SPL variables are autodeclared according to their usage. For example, this one-line program is valid because it is evident what is expected:


```spl>a += 1</lang


In contrast, this one-line program raises an error because it is not evident what object "a" is:


```spl>a = a+1</lang


'''Assignment.'''

```spl
a = 1
b,c,d = 0
```


'''Datatypes.'''
In SPL an object can be: number, text, stack, array, group, function.

'''Scope.'''
There is a main program body scope and individual function scopes. Variable from any scope can be accessed from any other scope by specifying the desired scope and variable name like "scope.variable". Variable from main program body scope can be accessed from any other scope like ".variable", because main program body scope is an empty space.

'''Referencing.'''
An object can become a reference to another object using "~" symbol, for example:


```spl
r = ~a
a = 3
#.output(r)
r = 5
#.output(a)
```


```txt

3
5

```



## SSEM

A variable is simply a storage address that the programmer chooses to treat as a variable. It requires no special allocation, declaration, etc., and an initial value can be loaded into it just as with any other storage address. Naturally, there is no concept of scope: any variable can be accessed and modified from any part of the program. To assign a fresh value to a variable, first form the desired number in the accumulator and then use a <tt>110 c to <address></tt> instruction to store it.

Note that accessing a value from a variable requires several operations, because the <tt>010 -<address> to c</tt> instruction (corresponding to <tt>load</tt>) negates the value in the process of loading it into the accumulator. The negation must thus be written back into storage and another <tt>010</tt> used to load the negation of that, which is the value we want. Depending on the algorithm, it will be found convenient either to overwrite the variable itself with its negation or to use a separate address as a scratch variable (thus preserving the original).


## SuperCollider

Variables are always local to the scope of a closure or an object.

```SuperCollider

// variable declaration
var table, chair;

// assignment
var table = 10, chair = -10;

// multiple assignment to the same value
table = chair = 0;

// multiple assignment to an array
(
var table, chair;
#table, chair = [10, -10];
#table ... chair = [10, -10, 2, 3]; // with ellipsis: now chair is [-10, 2, 3]
)

// the letters a-z are predeclared in the interpreter for interactive programming
a = 10; x = a - 8;

// variables are type-neutral and mutable: reassign to different objects
a = 10; a = [1, 2, 3]; a = nil;

// immutable variables (only in class definitions)
const z = 42;

// lexical scope
// the closures g and h refer to different values of their c
(
f = {
	var c = 0;
	{ c = c + 1 }
};
g = f.value;
h = f.value;
c = 100; // this doesn't change it.
)

// dynamic scope: environments
f = { ~table = ~table + 1 };
Environment.use { ~table = 100; f.value }; // 101.
Environment.use { ~table = -1; f.value }; // 0.

// there is a default environment
~table = 7;
f.value;

// lexical scope in environments:
(
Environment.use {
	~table = 100;
	f = { ~table = ~table + 1 }.inEnvir;
};
)
f.value; // 101.

// because objects keep reference to other objects, references are not needed:
// objects can take the role of variables. But there is a Ref object, that just holds a value

a = Ref([1, 2, 3]); // a reference to an array, can also be written as a quote `[1, 2, 3];
f = { |x| x.value = x.value.squared }; // a function that operates on a ref
f.(a); // `[ 1, 4, 9 ]

// proxy objects serve as delegators in environments. This can be called line by line:
ProxySpace.push;
~z // returns a NodeProxy
~z.play; // play a silent sound
~z = ~x + ~y; // make it the sum of two silent sounds
~x = { PinkNoise.ar(0.1) }; // … which now are noise,
~y = { SinOsc.ar(440, 0, 0.1) }; // and a sine tone


```



## Swift


```Swift
import Foundation

// All variables declared outside of a struct/class/etc are global

// Swift is a typed language
// Swift can infer the type of variables
var str = "Hello, playground" // String
let letStr:String = "This is a constant"

// However, all variables must be initialized
// Intialize variable of type String to nil
var str1:String! // str1 is nil

// Assign value to str1
str1 = "foo bar" // str1 is foo bar

// Swift also has optional types
// Declare optional with type of String
var optionalString = Optional<String>("foo bar") // (Some "foo bar")

println(optionalString) // Optional("foo bar")

// Optionals can also be declared with the shorthand ?
var optionalString1:String? = "foo bar"

// ! can be used to force unwrap but will throw a runtime error if trying to unwrap a nil value
println(optionalString1!)

optionalString1 = nil // Is now nil
// println(optionalString1!) would now throw a runtime error

// Optional chaining can be used to gracefully fail if there is a nil value
if let value = optionalString1?.lowercaseString {
    // Is never executed
} else {
    println("optionalString1 is nil")
}

// Swift also has type aliasing
typealias MyNewType = String // MyNewType is an alias of String
var myNewTypeString = MyNewType("foo bar")

// Swift also has special types Any and AnyObject
// Any can hold any type
// AnyObject can hold any object type
let myAnyObjectString:AnyObject = "foo bar"

// Downcast myAnyObjectString to String
if let myString = myAnyObjectString as? String {
    println(myString) // foo bar
} else {
    println("myString is not a string")
}

// Swift treats functions as first-class
// Declare a variable with a type of a function that takes no args
// and returns Void
var myFunc:(() -> Void)
func showScopes() {
    // Variable is scoped to function
    let myFunctionVariable = "foo bar function"

    // Nested functions inherit variables declared in enclosing scope
    func nestFunc() {
        println(myFunctionVariable)
    }
    nestFunc()
}

myFunc = showScopes // myFunc is now showScopes
myFunc() // foo bar function
```



## Tcl

Tcl's variables are local to procedures, lambdas and methods by default, and there is no initialization ''per se'': only assignment when the variable previously did not exist.

Demonstrating:

```tcl
namespace eval foo {
    # Define a procedure with two formal arguments; they are local variables
    proc bar {callerVarName argumentVar} {
        ### Associate some non-local variables with the procedure
        global globalVar;      # Variable in global namespace
        variable namespaceVar; # Variable in local (::foo) namespace
        # Access to variable in caller's context; may be local or global
        upvar 1 callerVarName callerVar

        ### Reading a variable uses the same syntax in all cases
        puts "caller's var has $callerVar"
        # But global and namespace vars can be accessed by using qualified names
        puts "global var has $globalVar which is $::globalVar"

        ### Writing a variable has no special syntax
        ### but [set] is by far the most common command for writing
        set namespaceVar $globalVar
        incr globalVar

        ### Destroying a variable is done like this
        unset argumentVar
    }
}
```

The main thing to note about Tcl is that the "<tt>$</tt>" syntax is a language level operator for reading a variable and not just general syntax for referring to a variable.

=={{header|TI-83 BASIC}}==

Variables will remain global, even after the program is complete. Global variables persist until deleted (or reset or power loss, unless they are ''archived'').

Variables may be assigned with the <code>→</code> to a value.


```ti83b

:1→A

```


=={{header|TI-89 BASIC}}==

A variable not declared local (to a program or function) is global. Global variables are grouped into ''folders'' of which one is current at any given time. Global variables persist until deleted (or reset or power loss, unless they are ''archived'').


```ti89b
Local mynum, myfunc
```


Variables may be assigned with the <code>→</code> or <code>Define</code> statements, both of which assign a new value to a variable. <code>→</code> is typically used interactively, but only <code>Define</code> can assign programs or multi-statement functions.


```ti89b
Define mynum = 1                  © Two ways to assign a number
1 → mynum

Define myfunc(x) = (sin(x))^2     © Two ways to assign a function
(sin(x))^2 → myfunc(x)

Define myfunc(x) = Func           © Multi-statement function
  If x < 0 Then
    Return –x
  Else
    Return x
  EndIf
EndFunc
```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
var1=1, var2="b"
PRINT "var1=",var1
PRINT "var2=",var2

basket=*
DATA apples
DATA bananas
DATA cherry

LOOP n,letter="a'b'c",fruit=basket
var=CONCAT (letter,n)
SET @var=VALUE(fruit)
PRINT var,"=",@var
ENDLOOP

```

Output:

```txt

var1=1
var2=b
a1=apples
b2=bananas
c3=cherry

```



## TXR


Variables have a form of pervasive dynamic scope in TXR. Each statement ("directive") of the query inherits the binding environment of the previous, invoking, or surrounding directive, as the case may be. The initial contents of the binding environment may be initialized on the interpreter's command line. The environment isn't simply a global dictionary. Each directive which modifies the environment creates a new version of the environment. When a subquery fails and TXR backtracks to some earlier directive, the original binding environment of that directive is restored, and the binding environment versions generated by backtracked portions of the query turn to garbage.

Simple example: the <code>cases</code>


```txr
@(cases)
hey @a
how are you
@(or)
hey @b
long time no see
@(end)
```

This directive has two clauses, matching two possible input cases, which have a common first line. The semantics of cases is short-circuiting: the first successful clause causes it to succeed and stop processing subsequent clauses. Suppose that the input matches the second clause. This means that the first clause will also match the first line, thereby establishing a binding for the variable <code>a</code>. However, the first clause fails to match on the second line, which means that it fails.  The interpreter then moves to the second clause, which is tried at the original input position, under the original binding environment which is devoid of the <code>a</code> variable.  Whichever clause of the <code>cases</code> is successful will pass both its environment modifications and input position increment to the next element of the query.

Under some other constructs, environments may be merged:


```txr
@(maybe)
@a bar
@(or)
foo @b
@(end)
```


The <code>maybe</code> directive matches multiple clauses such that it succeeds no matter what, even if none of the clauses succeed. Clauses which fail have no effect, but the effects of all successful clauses are merged. This means that if the input which faces the above <code>maybe</code> is the line <code>"foo bar"</code>, the first clause will match and bind <code>a</code> to foo, and the second clause will also match and bind <code>b</code> to bar.  The interpreter integrates these results together and the environment which emerges has both bindings.


## uBasic/4tH

uBasic/4tH has two kinds of variables, 26 predefined global integer variables (named <b>A</b> to <b>Z</b>) and a maximum of 26 parameters/local integer variables (named <b>A@</b> to <b>Z@</b>). The latter aren't defined, but merely allocated, using <b>LOCAL(</b><i>n</i><b>)</b> or <b>PARAM(</b><i>n</i><b>)</b>. The first allocated local variable is assigned <b>A@</b>, the second <b>B@</b>, etc. Both parameters and local variables are freed after <b>RETURN</b>. Parameters are always passed by value.
<lang>' Initialization
A = 15                 ' global variable

Proc _Initialize(24)

_Initialize Param (1)
  Local (1)            ' A@ now holds 24
  B@ = 23              ' local variable
Return

' Assignment
A = 15                 ' global variable

Proc _Assign(24)

_Assign Param (1)
  Local (1)            ' A@ now holds 24
  A@ = 5               ' reassignment of parameter A@
  B@ = 23              ' local variable
Return
```


## UNIX Shell


```sh

#!/bin/sh
# The unix shell uses typeless variables
apples=6
# pears=5+4           # Some shells cannot perform addition this way
pears = `expr 5+4`    # We use the external expr to perform the calculation
myfavourite="raspberries"

```



## Ursa


```ursa
# variable declaration
#
# declare [type] [name]
# 	-or-
# decl [type] [name]
decl int test

# initialization / assignment
#
# the set statement can be used to init variables and
# assign values to them
set test 10

# datatypes
#
# ursa currently has 10 built-in types, but
# more may be added in the future.
#     boolean
#     double
#     file
#     function
#     int
#     iodevice
#     port
#     serverport
#     string
#     task
#
# also, java classes may be used as data types
# cygnus/x ursa

# scope
#
# there is a global variable space, and functions
# have their own scope. control statements (for,
# if, try, while) don't have their own scope yet,
# but this will be implemented in the near future

# referencing
#
# variables are referenced by their name
decl port p
out p endl console
```



## WDTE


WDTE does not have variables, per se, but it does have several things that work similarly. The most obvious is function parameters:


```WDTE>let example t => io.writeln io.stdout t;</lang


The parameters are scoped to the inside of the function in which they're declared. There are also lambdas, which also have parameters:


```WDTE>let example t =
 3 -> (@ s n => * t n);
```


Lambdas are closures, so they have access to the parameters of the function that called them. The above multiplies 3 by the value of t.

There are also 'slots' for chains. Chains are essentially a series of expressions that are all 'chained' together. They take advantage of the fact that everything in WDTE is a function. In a chain, the first is called, then the second, and then the return of the second is called with the return of the first as an argument, then the third is called, and then its return is called with the output of the previous segment, and so on. In each element of the chain, a 'slot' can be specified which serves as a named binding for accessing that element's return value later. For example:


```WDTE>let writeToFile name data =

  file.open name : f
  -> str.format '{}' data
  -> io.writeln f
  -- io.close f
  ;
```


In this example, first <code>file.open name</code> is called. After this, the return value of that call can be accessed later in the chain using <code>f</code>. This is done in the last element as a way of closing the file.

Finally, an expression can also be bound to a name using a <code>let</code> expression, like in the function declarations shown above, as assigning a value to a function with no parameters is similar to creating a constant. When a <code>let</code> is encountered, all following expressions in the current block of code are run under a subscope of the current scope, which gives much the same effect as a variable assignment. For example:


```WDTE>let a =
 3;
io.writeln io.stdout a;
```



## XPL0


```txt

There are only three variable types: 32-bit signed integers (in the
32-bit version), IEEE-754 64-bit reals, and characters which are actually
32-bit addresses of (or pointers to) strings. When a 'char' variable is
subscripted, it accesses a single byte. All variable names must be
declared before they are used, for example:

int  I, J, K;
real X, Y, Array(10);
char String(80);

Variables (as well as all declared names, such as for procedures) must
start with a capital letter or underline. Names may contain digits or
underlines and be any length, but only the first 16 characters are
significant. Names are case-insensitive by default, unless the /c switch
is used when compiling.

Variables are usually assigned values like this: X:= 3.14, but the first
declared variables in a procedure can have argument values passed into
them. Variables other than arguments are only initialized by explicit
assignments in the body of the code.

Local variables are encapsulated in the procedures that declare them and
are not visible (are out of scope) outside those procedures. Procedures
can be (statically) nested several levels deep. The deepest procedure can
"see" all variables declared in the procedures in which it is nested. If
two variables are declared with identical names, the most local one is
used.

```



## XSLT

Although called variables, XSLT "variable" elements are single-assignment, and so behave more like constants. They are valid in the node scope in which they are declared.

```xml
<xsl:variable name="foo" select="XPath expression" />
<xsl:if test="$foo = 4">... </xsl:if>     <!-- prepend '$' to reference a variable or parameter-->
```



## VBA


'''Data types'''

<p>
<table border>
<tr bgcolor="#C0C0C0"><th>Data type<th>Runtime
type<th>Category<th>Storage
allocation<th>Value range
<tr><td>Boolean<td>Boolean<td><td>1 byte<td>True or False
<tr><td>Integer<td>Int16<td>signed
integer<td>2 bytes<td>-32768 through 32767
<tr><td>Long<td>Int32<td>signed
long integer<td>4 bytes<td>-2,1E+9 through 2,1E+9

precision: 9 decimals
<tr><td>Byte<td>UInt8<td>unsigned
byte<td>1 byte<td>0 through 255
<tr><td>Single<td>Single<td>single precision
floating point<td>4 bytes
<td>-3.45E+38 through -1.4E-45 for negative values;

1.4E-45 through 3.4E+38 for positive values;

precision: 6 decimals
<tr><td>Double<td>Double<td>double precision
floating point<td>8 bytes
<td>-1.7E+308 through -4.9E-324 for negative values;

4.9E-324 through 1.7E+308 for positive values;

precision: 16 decimals
<tr><td>Currency<td>Decimal<td>extended precision
fixed point<td>8 bytes
<td>-922,337,203,685,477.5808 to 922,337,203,685,477.5807

0 through &plusmn;9.2E14

smallest nonzero number is 0.0001;
<tr><td>String(n)<td>String<td>fixed length
string<td>n bytes<td>
<tr><td>String<td>String<td>variable length
string<td>10+(string length)<td>
<tr><td>Date<td>DateTime<td><td>8 bytes<td>00:00:00 AM on January 1, 0001 through
11:59:59 PM on December 31, 9999
<tr><td>Variant<td>Variant<td><td>4 bytes<td>Stores any type of data
</table>


## Visual Basic


'''Data types'''

<p>
<table border>
<tr bgcolor="#C0C0C0"><th>Data type<th>Runtime
type<th>Category<th>Storage
allocation<th>Value range
<tr><td>Boolean<td>Boolean<td><td>1 byte<td>True or False
<tr><td>Integer<td>Int16<td>signed
integer<td>2 bytes<td>-32768 through 32767
<tr><td>Long<td>Int32<td>signed
long integer<td>4 bytes<td>-2,1E+9 through 2,1E+9

precision: 9 decimals
<tr><td>Byte<td>UInt8<td>unsigned
byte<td>1 byte<td>0 through 255
<tr><td>Single<td>Single<td>single precision
floating point<td>4 bytes
<td>-3.45E+38 through -1.4E-45 for negative values;

1.4E-45 through 3.4E+38 for positive values;

precision: 6 decimals
<tr><td>Double<td>Double<td>double precision
floating point<td>8 bytes
<td>-1.7E+308 through -4.9E-324 for negative values;

4.9E-324 through 1.7E+308 for positive values;

precision: 16 decimals
<tr><td>Currency<td>Decimal<td>extended precision
fixed point<td>8 bytes
<td>-922,337,203,685,477.5808 to 922,337,203,685,477.5807

0 through &plusmn;9.2E14

smallest nonzero number is 0.0001;
<tr><td>String(n)<td>String<td>fixed length
string<td>n bytes<td>
<tr><td>String<td>String<td>variable length
string<td>10+(string length)<td>
<tr><td>Date<td>DateTime<td><td>8 bytes<td>00:00:00 AM on January 1, 0001 through
11:59:59 PM on December 31, 9999
<tr><td>Variant<td>Variant<td><td>4 bytes<td>Stores any type of data
<tr><td>Object<td>Object<td><td>4 bytes<td>Stores any object reference
</table>


## Visual Basic .NET



'''Variable declaration'''


```vbnet
Dim variable As datatype
Dim var1,var2,... As datatype
```

example:

```vbnet
Dim n1,n2 as Integer
Dim x as Double
Dim isRaining as Boolean
Dim greeting as String
```



'''Initialization'''


```vbnet
Dim variable As datatype = value
Dim var1,var2,... As datatype
```

example:

```vbnet
Dim wholeNumber1,wholeNumber2 as Integer = 3
Dim realNumber as Double = 3.0
Dim isRaining as Boolean = False
Dim greeting as String = "Hello, this is World speaking."
Dim longArray() As Long = {0, 1, 2, 3}
Dim twoDimensions(,) As Integer = {{0, 1, 2}, {10, 11, 12}}
```



'''Assignment'''


```vbnet>variable = expression</lang


```vbnet
         v = a
         d = b^2 - 4*a*c
         s3 = s1 & mid(s2,3,2)
```


```vbnet>variable <operator>= expression2</lang


```vbnet
         c += a
         c -= a
         c *= a
         c /= a
         c ^= a
         c <<= n
         c >>= n
         c &= a
```



'''Data types'''

<p>
<table border>
<tr bgcolor="#C0C0C0"><th>Data type<th>Runtime
type<th>Category<th>Storage
allocation<th>Value range
<tr><td>Boolean<td>Boolean<td><td>1 byte<td>True or False
<tr><td>Char<td>Char<td>unsigned<td>2 bytes<td>0 through 65535
<tr><td>SByte<td>Int8<td>signed
byte<td>1 byte<td>-128 through 127
<tr><td>Short<td>Int16<td>signed
short integer<td>2 bytes<td>-32,768 through 32,767
<tr><td>Integer<td>Int32<td>signed
integer<td>4 bytes<td>-2,1E+9 through 2,1E+9
precision: 9 decimals
<tr><td>Long<td>Int64<td>signed
long integer<td>8 bytes<td>-9.2E+18 through 9.2E+18
precision: 18 decimals
<tr><td>Byte<td>UInt8<td>unsigned
byte<td>1 byte<td>0 through 255
<tr><td>UShort<td>UInt16<td>unsigned
short integer<td>2 bytes<td>0 through 65,535
<tr><td>UInteger<td>UInt32<td>unsigned
integer<td>4 bytes<td>0 through 4,2E+9
<tr><td>ULong<td>UInt64<td>unsigned
long integer<td>8 bytes<td>0 through 1.8E+19
<tr><td>Single<td>Single<td>single precision
floating point<td>4 bytes
<td>-3.45E+38 through -1.4E-45 for negative values;
1.4E-45 through 3.4E+38 for positive values;
precision: 6 decimals
<tr><td>Double<td>Double<td>double precision
floating point<td>8 bytes
<td>-1.7E+308 through -4.9E-324 for negative values;
4.9E-324 through 1.7E+308 for positive values;
precision: 16 decimals
<tr><td>Decimal<td>Decimal<td>extended precision
fixed point<td>16 bytes
<td>0 through &plusmn;7.9E+28 with no decimal point;
0 through &plusmn;7.9 with 28 places to the right of the decimal;
smallest nonzero number is &plusmn;1E-28;
precision: 28 decimals
<tr><td>String<td>String<td>variable length
string<td><td>
<tr><td>Date<td>DateTime<td><td>8 bytes<td>00:00:00 AM on January 1, 0001 through
11:59:59 PM on December 31, 9999
<tr><td>Object<td>Object<td><td>4 bytes on
32-bit platform
8 bytes on
64-bit platform
<td>
</table>


'''scope'''

By default the scope of a variable is local to the <code>sub</code>, <code>function</code> or <code>module</code>.
By default the scope of a <code>sub</code> or <code>function</code> is global to the project.
Attributes <code>Public</code> or <code>Private</code> can mofidy these scopes.


## VBScript


'''Data types'''

<p>VBScript has only one data type called a Variant.


<table border>
<tr height=200 bgcolor="#C0C0C0"><th>Subtype<th>Category<th>Value range
<tr><td>Boolean<td><td>True or False
<tr><td>Integer<td>signed
integer<td>-32768 through 32767
<tr><td>Long<td>signed
long integer<td>-2,1E+9 through 2,1E+9
precision: 9 decimals
<tr><td>Byte<td>unsigned
byte<td>0 through 255
<tr><td>Single<td>single precision
floating point
<td>-3.45E+38 through -1.4E-45 for negative values;
1.4E-45 through 3.4E+38 for positive values;
precision: 6 decimals
<tr><td>Double<td>double precision
floating point
<td>-1.7E+308 through -4.9E-324 for negative values;
4.9E-324 through 1.7E+308 for positive values;
precision: 16 decimals
<tr><td>Currency<td>extended precision
fixed point
<td>-922,337,203,685,477.5808 to 922,337,203,685,477.5807
0 through &plusmn;9.2E14
smallest nonzero number is 0.0001;
<tr><td>String<td>variable length
string<td>
<tr><td>Date<td><td>00:00:00 AM on January 1, 0001 through
11:59:59 PM on December 31, 9999
<tr><td>Object<td><td>Stores any object reference
<tr><td>Empty<td><td>Variant is uninitialized
<tr><td>Null<td><td>Variant has no valid data
<tr><td>Error<td><td>Variant contains an error number
</table>


## zkl

The are two variable type in zkl, register (auto in C) and var (instance variable). vars are global to class [instance], registers are visible to their scope and enclosed scopes. In addition, a function can have vars (static var in C), which are actually [hidden] instance data. vars have no type.

```zkl
var v;  // global to the class that encloses this file
class C{ var v }  // global to class C, each instance gets a new v
class C{fcn f{var v=123;}} // v can only be seen by f, initialized when C is
class C{fcn init{var [const] v=5;}} // init is part of the constructor,
   so vars are promoted yo class scope. This allows const vars to be created at
   construction time
var v=123; v="hoho"; //not typed
class C{var v} // C.v OK, but just v is not found
class C{var[const]v=4} // C.v=3 illegal (compile or run time, depending)
class C{var[mixin]v=4} // the compiler treats v as an int for type checking
class C{var[proxy]v=f; fcn f{println("my name is ",self.fcn.name)} }
   v acts like a property to run f so C.v is the same as C.f()
class C{reg r}         // C.r is compile time error
r:=5;  // := syntax is same as "reg r=5", convenience
```





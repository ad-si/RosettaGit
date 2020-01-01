+++
title = "Sort three variables"
description = ""
date = 2019-10-18T19:45:44Z
aliases = []
[extra]
id = 21364
[taxonomies]
categories = []
languages = [
  "aime",
  "algol_68",
  "autohotkey",
  "c",
  "csharp",
  "cpp",
  "cobol",
  "d",
  "elena",
  "elixir",
  "factor",
  "forth",
  "fortran",
  "go",
  "haskell",
  "is-basic",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "min",
  "modula-2",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "php",
  "purebasic",
  "python",
  "python_2",
  "python_3",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "ring",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "visual_basic_.net",
  "zkl",
]
tags = ["sorting"]
+++

## Task

Sort   (the values of)   three variables   ('''X''',   '''Y''',   and   '''Z''')
that contain any value   (numbers and/or literals).

If that isn't possible in your language, then just sort numbers
(and note if they can be floating point, integer, or other).

I.E.:     (for the three variables   '''x''',   '''y''',   and   '''z'''),
where:
```
x =  'lions, tigers, and'
y =  'bears, oh my!'
z =  '(from the "Wizard of OZ")'
```

After sorting,  the three variables would hold:
```
x =  '(from the "Wizard of OZ")'
y =  'bears, oh my!'
z =  'lions, tigers, and'
```
<!--
  Care was taken to use a leading lowercase letter
  so that EBCDIC and ASCII machines sort the literals in the same order.
!-->

For numeric value sorting, use:
I.E.:     (for the three variables   '''x''',   '''y''',   and   '''z'''),
where:
```
x =  77444
y =    -12
z =      0
```
After sorting,  the three variables would hold:
```
x =    -12
y =      0
z =  77444
```

The variables should contain some form of a number, but specify if the algorithm
used can be for floating point or integers.
Note any limitations.

The values may or may not be unique.

The method used for sorting can be any algorithm;
the goal is to use the most idiomatic in the computer programming language used.

More than one algorithm could be shown if one isn't clearly the better choice.


One algorithm could be:
```
Θ  store the three variables   '''x''', '''y''', and '''z'''
      into an array (or a list)   '''A'''

Θ  sort  (the three elements of)  the array   '''A'''

Θ  extract the three elements from the array and place them in the
      variables '''x''', '''y''', and '''z'''   in order of extraction
```



Another algorithm   (only for numeric values):
```
    x= 77444
    y=   -12
    z=     0
 low= x
 mid= y
high= z
   x= min(low,  mid,  high)            /*determine the lowest value of X,Y,Z. */
   z= max(low,  mid,  high)            /*    "      "  highest  "    " " " "  */
   y=     low + mid + high - x - z     /*    "      "  middle   "    " " " "  */
```

Show the results of the sort here on this page using at least the values of those shown above.


## Aime


```aime
integer a, b, c;
index i;
text x, y, z;
record r;

x = "lions, tigers, and";
y = "bears, oh my!";
z = "(from the \"Wizard of OZ\")";

r.fit(x, x, y, y, z, z);

x = r.rf_pick;
y = r.rf_pick;
z = r.rf_pick;

o_form("~\n~\n~\n", x, y, z);

a = 77444;
b = -12;
c = 0;

i.fit(a, a, b, b, c, c);

a = i.if_pick;
b = i.if_pick;
c = i.if_pick;

o_form("~\n~\n~\n", a, b, c);
```

Output:

```txt
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
-12
0
77444
```



## ALGOL 68

This uses Algol 68's UNION facility which allows a variable to have values of a number of types whilst maintaining Algol 68's strong-typing.


Assignment allows any value of one of the UNION MODEs to be directly assigned to the variable.
When using the variable, its MODE (type) and value must be tested and extracted using a CASE construct.


As the task only requires sorting three values, we use a simple, three-element specific sort routine.

```algol68
BEGIN
    # MODE that can hold integers and strings - would need to be extended to #
    # allow for other types                                                  #
    MODE INTORSTRING = UNION( INT, STRING );
    # returns TRUE if a is an INT, FALSE otherwise #
    OP   ISINT    = ( INTORSTRING a )BOOL:   CASE a IN (INT):      TRUE OUT FALSE ESAC;
    # returns TRUE if a is an INT, FALSE otherwise #
    OP   ISSTRING = ( INTORSTRING a )BOOL:   CASE a IN (STRING):   TRUE OUT FALSE ESAC;
    # returns the integer in a or 0 if a isn't an integer #
    OP   TOINT    = ( INTORSTRING a )INT:    CASE a IN (INT i):    i    OUT 0     ESAC;
    # returns the string in a or "" if a isn't a string #
    OP   TOSTRING = ( INTORSTRING a )STRING: CASE a IN (STRING s): s    OUT ""    ESAC;
    # returns TRUE if a < b, FALSE otherwise #
    # a and b must have the same type #
    PRIO LESSTHAN = 4;
    OP   LESSTHAN = ( INTORSTRING a, b )BOOL:
        IF  ISSTRING a AND ISSTRING b THEN
            # both strings #
            TOSTRING a < TOSTRING b
        ELIF ISINT a AND ISINT b THEN
            # both integers #
            TOINT a < TOINT b
        ELSE
            # different MODEs #
            FALSE
        FI # LESSTHAN # ;
    # exchanges the values of a and b #
    PRIO SWAP = 9;
    OP   SWAP = ( REF INTORSTRING a, b )VOID: BEGIN INTORSTRING t := a; a := b; b := t END;
    # sorts a, b and c #
    PROC sort 3 = ( REF INTORSTRING a, b, c )VOID:
    BEGIN
        IF b LESSTHAN a THEN a SWAP b FI;
        IF c LESSTHAN a THEN a SWAP c FI;
        IF c LESSTHAN b THEN b SWAP c FI
    END # sort 3 # ;

    # task test cases #
    INTORSTRING x, y, z;
    x := "lions, tigers, and";
    y := "bears, oh my!";
    z := "(from the ""Wizard of OZ"")";
    sort 3( x, y, z );
    print( ( x, newline, y, newline, z, newline ) );
    x := 77444;
    y := -12;
    z := 0;
    sort 3( x, y, z );
    print( ( x, newline, y, newline, z, newline ) )
END
```

Output:

```txt
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
        -12
         +0
     +77444
```



## AutoHotkey


```AutoHotkey
SortThreeVariables(ByRef x,ByRef y,ByRef z){
	obj := []
	for k, v in (var := StrSplit("x,y,z", ","))
		obj[%v%] := true
	for k, v in obj
		temp := var[A_Index], %temp% := k
}
```

Examples:
```AutoHotkey
x =  lions, tigers, and
y =  bears, oh my!
z =  (from the "Wizard of OZ")
SortThreeVariables(x,y,z)
MsgBox % x "`n" y "`n" z

x =  77444
y =    -12
z =      0
SortThreeVariables(x,y,z)
MsgBox % x "`n" y "`n" z
return
```

Outputs:
```txt
---------------------------
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
---------------------------
-12
0
77444
---------------------------
```



## C

Although C does not have a generic catch-all variable type, strings or arrays of characters can be used ( or misused ) to simulate the effect. Strings are often used to process arbitrarily large or small numbers which can't be represented by the C Standard Library. Strings are thus used for this task to accept whatever the user enters. Each entry is treated as a string and is scanned. If any one of them contains a single 'non-number' character, then the whole set is treated as a set of strings.

The first implementation is generic and can be used for any entries ( integer, floating point, strings). The second one is specific to the task and only sorts integers.


### Generic Implementation


```c
#include<string.h>
#include<stdlib.h>
#include<stdio.h>

#define MAX 3

int main()
{
	char values[MAX][100],tempStr[100];
	int i,j,isString=0;
	double val[MAX],temp;

	for(i=0;i<MAX;i++){
		printf("Enter %d%s value : ",i+1,(i==0)?"st":((i==1)?"nd":"rd"));
		fgets(values[i],100,stdin);

		for(j=0;values[i][j]!=00;j++){
			if(((values[i][j]<'0' || values[i][j]>'9') && (values[i][j]!='.' ||values[i][j]!='-'||values[i][j]!='+'))
			||((values[i][j]=='.' ||values[i][j]=='-'||values[i][j]=='+')&&(values[i][j+1]<'0' || values[i][j+1]>'9')))
				isString = 1;
		}
	}

	if(isString==0){
		for(i=0;i<MAX;i++)
			val[i] = atof(values[i]);
	}

	for(i=0;i<MAX-1;i++){
		for(j=i+1;j<MAX;j++){
			if(isString==0 && val[i]>val[j]){
				temp = val[j];
				val[j] = val[i];
				val[i] = temp;
			}

			else if(values[i][0]>values[j][0]){
				strcpy(tempStr,values[j]);
				strcpy(values[j],values[i]);
				strcpy(values[i],tempStr);
			}
		}
	}

	for(i=0;i<MAX;i++)
		isString==1?printf("%c = %s",'X'+i,values[i]):printf("%c = %lf",'X'+i,val[i]);

	return 0;
}
```

The output shows three test cases, two as specified in the task, and one which mixes numbers and strings. The output is sorted considering all of them as strings in that case.

```txt

Enter 1st value : 77444
Enter 2nd value : -12
Enter 3rd value : 0
X = -12
Y = 0
Z = 77444

Enter 1st value : lions, tigers, and
Enter 2nd value : bears, oh my!
Enter 3rd value : (from the "Wizard of OZ")
X = (from the "Wizard of OZ")
Y = bears, oh my!
Z = lions, tigers, and

Enter 1st value : -12
Enter 2nd value : bears, oh my!
Enter 3rd value : 77444
X = -12
Y = 77444
Z = bears, oh my!

```


### Task Specific Implementation


```C

#include<stdio.h>

int main()
{
	int x = 77444,y=-12,z=0,temp;

	printf("Before sorting :\nx = %d\ny = %d\nz = %d",x,y,z);

	do{
	temp = x;

	if(temp > y){
		x = y;
		y = temp;
	}

	if(z < y){
		temp = y;
		y = z;
		z = temp;
	}
	}while(x>y || y>z);

	printf("\nAfter sorting :\nx = %d\ny = %d\nz = %d",x,y,z);

	return 0;
}

```

Output :

```txt
Before sorting :
x = 77444
y = -12
z = 0
After sorting :
x = -12
y = 0
z = 77444
```



## C#

Works with C sharp|7.0

```c#
using System;
public class Program
{
    public static void Main()
    {
        (int x, int y, int z) = (77444, -12, 0);

        //Sort directly:
        if (x > y) (x, y) = (y, x);
        if (x > z) (x, z) = (z, x);
        if (y > z) (y, z) = (z, y);
        Console.WriteLine((x, y, z));

        var (a, b, c) = (
            "lions, tigers, and",
            "bears, oh my!",
            "(from the 'Wizard of OZ')");

        //Sort with generic method:
        Sort(ref a, ref b, ref c);
        Console.WriteLine((a, b, c));
    }

    public static void Sort<T>(ref T a, ref T b, ref T c)
        where T : IComparable<T>
    {
        if (a.CompareTo(b) > 0) (a, b) = (b, a);
        if (a.CompareTo(c) > 0) (a, c) = (c, a);
        if (b.CompareTo(c) > 0) (b, c) = (c, b);
    }
}
```

Output:

```txt
(-12, 0, 77444)
((from the 'Wizard of OZ'), bears, oh my!, lions, tigers, and)
```



## C++


```cpp
#include <iostream>
#include <string>
#include <vector>

template < class T >
void sort3( T& x, T& y, T& z) {
    std::vector<T> v;
    v.push_back( x ); v.push_back( y ); v.push_back( z );
    bool b = true;
    while( b ) {
        b = false;
        for( size_t i = 0; i < v.size() - 1; i++ ) {
            if( v[i] > v[i+1] ) {
                T t = v[i];
                v[i] = v[i + 1];
                v[i + 1] = t;
                b = true;
            }
        }
    }
    x = v[0]; y = v[1]; z = v[2];
}
int main(int argc, char* argv[]) {
    int xi = 77444, yi = -12, zi = 0;
    sort3( xi, yi, zi );
    std::cout << xi << "\n" << yi << "\n" << zi << "\n\n";

    std::string xs, ys, zs;
    xs = "lions, tigers, and";
    ys = "bears, oh my!";
    zs = "(from the \"Wizard of OZ\")";
    sort3( xs, ys, zs );
    std::cout << xs << "\n" << ys << "\n" << zs << "\n\n";

    float xf = 11.3f, yf = -9.7f, zf = 11.17f;
    sort3( xf, yf, zf );
    std::cout << xf << "\n" << yf << "\n" << zf << "\n\n";

    return 0;
}

```

Output:

```txt
-12
0
77444
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
-9.7
11.17
11.3
```


## COBOL


```COBOL
       program-id. 3var.
       data division.
       working-storage section.
       1 n binary pic 9(4).
       1 num pic -(7)9.
       1 a1 pic x(32) value "lions, tigers, and".
       1 a2 pic x(32) value "bears, oh my!".
       1 a3 pic x(32) value "(from the ""Wizard of OZ"")".
       1 n1 pic x(8) value "77444".
       1 n2 pic x(8) value "-12".
       1 n3 pic x(8) value "0".
       1 alpha-table.
        2 alpha-entry occurs 3 pic x(32).
       1 numeric-table.
        2 numeric-entry occurs 3 pic s9(8).
       1 filler value "x = y = z = ".
        2 lead-in occurs 3 pic x(4).
       procedure division.
       begin.
           move a1 to alpha-entry (1)
           move a2 to alpha-entry (2)
           move a3 to alpha-entry (3)
           sort alpha-entry ascending alpha-entry
           perform varying n from 1 by 1
           until n > 3
               display lead-in (n) alpha-entry (n)
           end-perform

           display space

           compute numeric-entry (1) = function numval (n1)
           compute numeric-entry (2) = function numval (n2)
           compute numeric-entry (3) = function numval (n3)
           sort numeric-entry ascending numeric-entry
           perform varying n from 1 by 1
           until n > 3
               move numeric-entry (n) to num
               display lead-in (n) num
           end-perform

           stop run
           .
       end program 3var.
```

Output:

```txt
x = (from the "Wizard of OZ")
y = bears, oh my!
z = lions, tigers, and

x =      -12
y =        0
z =    77444
```


## D


```D
import std.stdio;

void main() {
    driver(77444, -12, 0);
    driver("lions, tigers, and", "bears, oh my!", "(from the \"Wizard of OZ\")");
}

void driver(T)(T x, T y, T z) {
    writeln("BEFORE: x=[", x, "]; y=[", y, "]; z=[", z, "]");
    sort3Var(x,y,z);
    writeln("AFTER: x=[", x, "]; y=[", y, "]; z=[", z, "]");
}

void sort3Var(T)(ref T x, ref T y, ref T z)
out {
    assert(x<=y);
    assert(x<=z);
    assert(y<=z);
}
body {
    import std.algorithm : swap;

    if (x < y) {
        if (z < x) {
            swap(x,z);
        }
    } else if (y < z) {
        swap(x,y);
    } else {
        swap(x,z);
    }
    if (z<y) {
        swap(y,z);
    }
}
```


Output:

```txt
BEFORE: x=[77444]; y=[-12]; z=[0]
AFTER: x=[-12]; y=[0]; z=[77444]
BEFORE: x=[lions, tigers, and]; y=[bears, oh my!]; z=[(from the "Wizard of OZ")]
AFTER: x=[(from the "Wizard of OZ")]; y=[bears, oh my!]; z=[lions, tigers, and]
```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

sortThree(ref object a, ref object b, ref object c)
{
    if (a > b) { exchange(ref a, ref b) };
    if (a > c) { exchange(ref a, ref c) };
    if (b > c) { exchange(ref b, ref c) }
}

public program()
{
    var x := 5;
    var y := 1;
    var z := 2;

    var a := "lions, tigers, and";
    var b := "bears, oh my!";
    var c := "(from the 'Wizard of OZ')";

    sortThree(ref x,ref y,ref z);
    sortThree(ref a,ref b,ref c);

    console.printLine(x,",",y,",",z);
    console.printLine(a,",",b,",",c)
}
```

Output:

```txt

1,2,5
(from the 'Wizard of OZ'),bears, oh my!,lions, tigers, and

```



## Elixir


```elixir
x = 'lions, tigers, and'
y = 'bears, oh my!'
z = '(from the "Wizard of OZ")'

[x, y, z] = Enum.sort([x, y, z])
IO.puts "x = #{x}\ny = #{y}\nz = #{z}\n"

x = 77444
y = -12
z = 0

[x, y, z] = Enum.sort([x, y, z])
IO.puts "x = #{x}\ny = #{y}\nz = #{z}"
```

Output:

```txt
x = (from the "Wizard of OZ")
y = bears, oh my!
z = lions, tigers, and

x = -12
y = 0
z = 77444
```


=={{header|F_Sharp|F#}}==

```fsharp
 let x = "lions, tigers, and"
 let y = "bears, oh my!"
 let z = """(from the "Wizard of OZ")"""
 List.iter (printfn "%s") (List.sort [x;y;z])

```

Output:

```txt
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
```



## Factor


```factor
USING: arrays io kernel prettyprint sequences sorting ;
IN: rosetta-code.sort-three

: sort3 ( b c a -- a b c ) 3array natural-sort first3 ;

"lions, tigers, and"
"bears, oh my!"
"(from the \"Wizard of OZ\")"
sort3 [ print ] tri@

77444 -12 0 sort3 [ . ] tri@
```

Output:

```txt
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
-12
0
77444
```



## Forth

=
## Integers
=
In GNU Forth the machine's integers are the default data type.
It is straightforward to sort these on the Forth Data Stack.
<BR>Note:
Assigning the values to variables is simple however unless
storage in variables was needed to retain the values, they would
typically be used directly from the stack by a subsequent operation in the
program.
<LANG>\ sort 3 integers
VARIABLE X  VARIABLE Y   VARIABLE Z

: VARS@  ( --- n n n)  X @  Y @   Z @ ; \ read variables
: VARS!  ( n n n -- )  Z !  Y !   X ! ; \ store variables

: ?SWAP     ( a b -- a b)   \ conditional swap
            2DUP < IF SWAP THEN ;

: SORT3INTS ( a b c -- c b a)  ?SWAP >R  ?SWAP  R>  ?SWAP ;</LANG>

Testing is done using the Forth console using '?' to view VARIABLE contents

```txt
 ok
77000 -12 0 VARS!  ok
X ? 77000  ok
Y ? -12  ok
Z ? 0  ok
VARS@ SORT3INTS VARS!  ok
X ? 77000  ok
Y ? 0  ok
Z ? -12  ok</PRE>

=
## Strings
=
Strings require extending the language but the primitives needed are part of ANS/ISO Forth.
<LANG>DECIMAL
: BUFFER: ( n -- ) CREATE ALLOT ;

64 BUFFER: X
64 BUFFER: Y
64 BUFFER: Z

: S'  ( <text> )  [CHAR] ' PARSE ;

S' (from the "Wizard of OZ")' X PLACE
S' bears, oh my!'             Y PLACE
S' lions, tigers, and'        Z PLACE

\ counted string compare less than
: <S        ( caddr caddr -- caddr caddr flag)
            COUNT ROT COUNT COMPARE 1 = ;

: ?SWAPSTR  ( caddr caddr -- caddr caddr)  \ conditional string swap
             2DUP <S IF SWAP THEN ;

\ sort on stack as done for integers
: SORT3STRINGS   ?SWAPSTR >R  ?SWAPSTR R> ?SWAPSTR ;

\ non-destructive print 3 counted-strings from data stack
: .STRS  ( caddr1 caddr2 caddr3 -- caddr1 caddr2 caddr3) \ order is dependant
           3 0 DO ROT  DUP CR COUNT TYPE  LOOP ; </LANG>
With these extensions we can do the same testing at the Forth console and
examine the string order with '.STRS'.
<PRE> ok
X Y Z  ok
.S <3> 2142061672 2142061760 2142061848  ok
.STRS
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and ok
SORT3STRINGS  ok
.S <3> 2142061848 2142061760 2142061672  ok
.STRS
lions, tigers, and
bears, oh my!
(from the "Wizard of OZ") ok</PRE>


## Fortran

Fortran does not offer a multi-mode facility whereby a variable can be the union of various basic types such as integer or floating-point or character. The EQUIVALENCE statement can cause three such different variables to be placed at the same storage location or indeed, because parameter type-checking is rarely performed by a Fortran compiler (especially with routines compiled separately) it is often possible to pass a floating-point variable to a subroutine expecting an integer, and so forth. Leaving aside questions of storage size mismatch (as when a subroutine stores a sixty-four bit double-precision value into a parameter it had been supplied as a sixteen-bit integer, say) the bit patterns of disparate types will rarely involve a useful ordering. For example, an eight-byte floating-point variable could contain the bit pattern of "Hi There" but the numerical value that will manifest will not likely be useful in sorting text. Though it could be. Imagine having a collection of eight-character codes that is to be searched frequently by a binary chop method. If the actual ordering is unimportant, they could be both sorted and searched as REAL*8 variables, dealing with all eight characters at one blow. However, given the modern floating-point usage of Not-a-Number and the like, it would probably be safer to work with integer variables so that every bit pattern would be distinct. Alas, on the B6700, the high-order bit of its 48-bit word was not used in arithmetic so if an eight-bit character code were to be placed in the high-order eight bits, some character codes would not be distinguished.

It is possible to use floating-point variables to store integer values and their ordering will be sensible, but the bit patterns of integer and floating-point values are usually very different. Again, a subroutine that is supplied integer variables (even of the right size) but which works with floating-point variables (or ''vice-versa'') will not produce a sensible ordering. Except perhaps on a B6700 where integer values were represented as floating-point numbers with no fractional part. Operations such as ''add'', ''subtract'', and ''multiply'' proceeded as normal while ''divide'' discarded any resulting fractional part.

The most flexible type of variable is text, as the text can represent any type of value - but as text, not as a proper number. The ordering resulting from numbers represented as text will be very dependent on their format. This is discussed in [[Natural_sorting]] for example.

The example source uses F90 mainly to enable the contained routine SWAPC though it could be made into a separate subroutine. Further F90 facilities would allow the definition of a "generic" SORT3 routine, with a lot of syntax that would enable a SORT3I2, SORT3I4, SORT3F4, ''etc.'' routines for INTEGER*2, INTEGER*4, REAL*4, ''etc.'' to be selected for each case. A suitable pre-processor scheme could perhaps generate these variations, but alas, it is not standard. The resulting SORT3 routine could be invoked for sets of parameters of any of the types prepared for, but it will only ''appear'' to be one routine working with different types of parameters; in fact different actual routines would be invoked by the compiler selecting according to the type of the actual parameters.

There would be similar requirements for a SWAP routine for each type of parameter, for alas, Fortran does not define a SWAP statement. The temporary variable needed for the SWAP process is defined rather intimidatingly as having the size of the largest of the three parameters; prior to F90 variables defined in a routine could only have a size defined at compile time, which would have to be "surely big enough". Rather than have this redefined for each invocation of SWAPC (where it would be the larger of the two parameters) it is defined once in SORT3. However, all three parameters should be the same size, or risk truncation. Using a great deal more syntax (or, as standardised in F2003) it is possible to have character variables be resized on each assignment to them to accommodate the length of text being assigned.

One could make rather more use of the facilities of F90 and define a compound data type, such as
```Fortran
      TYPE(MONGREL)
       INTEGER TYPEIS
       INTEGER VI
       REAL VF
       CHARACTER*(enuff) VC
       ...etc...
      END TYPE MONGREL
      TYPE (MONGREL) DOG
```

So that DOG.TYPEIS would indicate which component to access. Still further ploys would enable storage to be allocated only for the type currently in use, especially for CHARACTER variables, and via a great deal more syntax defining how to perform operations such as .GT. and the like, MONGREL type variables can be used in expressions involving such operators just as can variables of the basic types. This sort of approach is demonstrated in [[Arithmetic/Rational#Fortran]], but otherwise, every reference to a MONGREL will involve some sort of CASE statement to select the appropriate usage, hopefully of related types only. Serious computation with such MONGREL variables surely will not be speedy and thus would be un-Fortrannish. What to do when a text string meets a complex number remains obscure - convert the number to a text (but, what format?), does the text represent a number? And what type results from such miscegnation?

Routines that modify their parameters should not be invoked with constants (or text literals) as such parameters... Some systems allow constants to be in protected storage, and if so, an attempt to modify such storage will produce a run-time error. Otherwise, it all depends on how constants are passed as parameters. If a temporary storage item is loaded with the desired value and the address of that scratch variable is passed, then disaster will be averted - though good results may not be produced.

For convenience in setting up the two examples, an array is used to hold the test data. The subroutine is not invoked with an array parameter, it is invoked with three separate elements of the array. The DATA statement initialising the array looks to be the transpose of the desired ordering, because of the way Fortran orders elements in storage.

```Fortran
      SUBROUTINE SORT3(X,Y,Z)	!Perpetrate a bubblesort in-line.
       CHARACTER*(*) X,Y,Z	!Just three to rearrange.
       CHARACTER*(MAX(LEN(X),LEN(Y),LEN(Z))) T	!Really, they should all be the same length.
        IF (X.GT.Y) CALL SWAPC(X,Y)	!The first pass: for i:=2:3 do if a(i - 1) > a(i) swap
        IF (Y.GT.Z) CALL SWAPC(Y,Z)	!The second test of the first pass.
        IF (X.GT.Y) CALL SWAPC(X,Y)	!The second pass: for i:=2:2...
       CONTAINS		!Alas, Fortran does not offer a SWAP statement.
        SUBROUTINE SWAPC(A,B)	!So, one must be devised for each case.
        CHARACTER*(*) A,B	!To have their content swapped.
         T = A			!Ccpy the first to a safe space.
         A = B			!Copy the second on to the first.
         B = T			!Copy what had been first to the second.
        END SUBROUTINE SWAPC	!One of these will be needed for each type of datum.
       END SUBROUTINE SORT3	!No attempt is made to stop early, as for already-ordered data.

       PROGRAM POKE
       CHARACTER*28 XYZ(3,2)	!Encompass the two examples.
       DATA XYZ/		!Storage order is "column-major".
     1 'lions, tigers, and','bears, oh my!','(from the "Wizard of OZ")',	!So (1,1), (2,1), (3,1)
     2 '77444','  -12','    0'/	!So this looks like a transposed array. But	    (1,2), (2,2), (3,2)
       INTEGER I		!A stepper for the loop.
       DO I = 1,2		!Two examples.
         WRITE (6,66) "Supplied: ", XYZ(1:3,I)	!As given.
   66    FORMAT (A12,3(" >",A,"<"))		!Show as >text< for clarity.

         CALL SORT3(XYZ(1,I),XYZ(2,I),XYZ(3,I))	!Three separate variables, that happen to be in an array.

         WRITE (6,66) "Sorted, ? ", XYZ(1:3,I)	!The result.
       END DO			!On to the next example.
       END	!Nothing much.

```

Output: the texts showing numbers appear in text order, not the order of their numbers. Incidentally, not everything is done in ASCII. The EBCDIC ordering is different.

```txt
  Supplied:  >lions, tigers, and          < >bears, oh my!               < >(from the "Wizard of OZ")   <
  Sorted, ?  >(from the "Wizard of OZ")   < >bears, oh my!               < >lions, tigers, and          <
  Supplied:  >77444                       < >  -12                       < >    0                       <
  Sorted, ?  >    0                       < >  -12                       < >77444                       <
```



## Go

There are ways of doing this task in a generic way in Go but they are a bit cumbersome and it would not be idiomatic.  Shown here then are solutions coded specifically to the string and integer types of the task test cases.  Solutions would be very similar for any of the other comparable Go types such as float64.

```go
package main

import (
    "fmt"
    "log"
    "sort"
)

var (
    stringsIn = []string{
        `lions, tigers, and`,
        `bears, oh my!`,
        `(from the "Wizard of OZ")`}
    intsIn = []int{77444, -12, 0}
)

func main() {
    {
        // initialize three vars
        x, y, z := stringsIn[0], stringsIn[1], stringsIn[2]

        // I. Task suggested technique, move values to array (slice).
        // It's consise and relies on library code.
        s := []string{x, y, z}
        sort.Strings(s)
        x, y, z = s[0], s[1], s[2]

        // validate
        if x > y || y > z {
            log.Fatal()
        }

        // II. Likely fastest technique, minimizing tests and data movement.
        // Least consise though, hardest to understand, and most chance to make
        // a coding mistake.
        x, y, z = stringsIn[0], stringsIn[1], stringsIn[2] // (initialize)
        if x < y {
            switch {
            case y < z:
            case x < z:
                y, z = z, y
            default:
                x, y, z = z, x, y
            }
        } else {
            switch {
            case x < z:
                x, y = y, x
            case z < y:
                x, z = z, x
            default:
                x, y, z = y, z, x
            }
        }
        if x > y || y > z { // (validate)
            log.Fatal()
        }

        // III.  A little more consise than II, easier to understand, almost
        // as fast.
        x, y, z = stringsIn[0], stringsIn[1], stringsIn[2] // (initialize)
        if x > y {
            x, y = y, x
        }
        if y > z {
            y, z = z, y
        }
        if x > y {
            x, y = y, x
        }
        if x > y || y > z { // (validate)
            log.Fatal()
        }
        fmt.Println("sorted strings:")
        fmt.Println(" ", x)
        fmt.Println(" ", y)
        fmt.Println(" ", z)
        fmt.Println("original data:")
        fmt.Println(" ", stringsIn[0])
        fmt.Println(" ", stringsIn[1])
        fmt.Println(" ", stringsIn[2])
    }
    // same techniques, with integer test case
    {
        // task suggested technique
        x, y, z := intsIn[0], intsIn[1], intsIn[2] // (initialize)
        s := []int{x, y, z}
        sort.Ints(s)
        x, y, z = s[0], s[1], s[2]
        if x > y || y > z { // (validate)
            log.Fatal()
        }

        // minimizing data movement
        x, y, z = intsIn[0], intsIn[1], intsIn[2] // (initialize)
        if x < y {
            switch {
            case y < z:
            case x < z:
                y, z = z, y
            default:
                x, y, z = z, x, y
            }
        } else {
            switch {
            case x < z:
                x, y = y, x
            case z < y:
                x, z = z, x
            default:
                x, y, z = y, z, x
            }
        }
        if x > y || y > z { // (validate)
            log.Fatal()
        }

        // three swaps
        x, y, z = intsIn[0], intsIn[1], intsIn[2] // (initialize)
        if x > y {
            x, y = y, x
        }
        if y > z {
            y, z = z, y
        }
        if x > y {
            x, y = y, x
        }
        if x > y || y > z { // (validate)
            log.Fatal()
        }
        fmt.Println("sorted ints:", x, y, z)
        fmt.Println("original data:", intsIn)
    }
    // To put any of these techniques in a function, a function could just
    // take three values and return them sorted.
    {
        sort3 := func(x, y, z int) (int, int, int) {
            if x > y {
                x, y = y, x
            }
            if y > z {
                y, z = z, y
            }
            if x > y {
                x, y = y, x
            }
            return x, y, z
        }
        x, y, z := intsIn[0], intsIn[1], intsIn[2] // (initialize)
        x, y, z = sort3(x, y, z)
        if x > y || y > z { // (validate)
            log.Fatal()
        }
    }
    // Alternatively, a function could take pointers
    {
        sort3 := func(x, y, z *int) {
            if *x > *y {
                *x, *y = *y, *x
            }
            if *y > *z {
                *y, *z = *z, *y
            }
            if *x > *y {
                *x, *y = *y, *x
            }
        }
        x, y, z := intsIn[0], intsIn[1], intsIn[2] // (initialize)
        sort3(&x, &y, &z)
        if x > y || y > z { // (validate)
            log.Fatal()
        }
    }
}
```

Output:

```txt
sorted strings:
  (from the "Wizard of OZ")
  bears, oh my!
  lions, tigers, and
original data:
  lions, tigers, and
  bears, oh my!
  (from the "Wizard of OZ")
sorted ints: -12 0 77444
original data: [77444 -12 0]

```



## Haskell


Fortunately, Haskell prevents us from doing this kind of thing. It does makes flexible use of bound names, including names for function arguments, but it also protects us from the heavy costs to productivity and reliability that are inevitably imposed by mutating the meaning of bound names at run-time.

Although mutation is not on the menu, parameterised types do allow us to define polymorphic functions which can, for example, be applied both to lists of strings and also to lists of integers or lists of floats. The following functions work with triples of any type for which the  '''<=''' or '''compare''' functions are defined – in other words, any type for which an instance of the Ord class is defined.


```Haskell
import Data.List (sort)

sortedTriple
  :: Ord a
  => (a, a, a) -> (a, a, a)
sortedTriple (x, y, z) =
  let [a, b, c] = sort [x, y, z]
  in (a, b, c)

sortedListfromTriple
  :: Ord a
  => (a, a, a) -> [a]
sortedListfromTriple (x, y, z) = sort [x, y, z]

-- TEST ----------------------------------------------------------------------
main :: IO ()
main = do
  print $
    sortedTriple
      ("lions, tigers, and", "bears, oh my!", "(from the \"Wizard of OZ\")")
  print $
    sortedListfromTriple
      ("lions, tigers, and", "bears, oh my!", "(from the \"Wizard of OZ\")")
  print $ sortedTriple (77444, -12, 0)
  print $ sortedListfromTriple (77444, -12, 0)
```

Output:

```txt
("(from the \"Wizard of OZ\")","bears, oh my!","lions, tigers, and")
["(from the \"Wizard of OZ\")","bears, oh my!","lions, tigers, and"]
(-12,0,77444)
[-12,0,77444]
```


## IS-BASIC

```is-basic
100 LET X=77444:LET Y=-12:LET Z=0
110 PRINT X;Y;Z
120 CALL SHORT(X,Y,Z)
130 PRINT X;Y;Z
140 DEF SHORT(REF A,REF B,REF C)
150   IF A>B THEN LET T=A:LET A=B:LET B=T
160   IF B>C THEN LET T=B:LET B=C:LET C=T
170   IF A>B THEN LET T=A:LET A=B:LET B=T
180 END DEF
```

Output:

```txt
 77444 -12  0
-12  0  77444
```



## J

Note that this is extremely bad form, and you will stumble over why it is bad form if you try using it in any useful implementation:

```J
   x =:  'lions, tigers, and'
   y =:  'bears, oh my!'
   z =:  '(from the "Wizard of OZ")'
   'x y z'=: /:~".'x;y;<z'
   x
(from the "Wizard of OZ")
   y
bears, oh my!
   z
lions, tigers, and

   x =:  77444
   y =:    -12
   z =:      0
   'x y z'=: /:~".'x;y;<z'
   x
_12
   y
0
   z
77444
```



## Java


```Java

import java.util.Comparator;
import java.util.stream.Stream;

class Box {
    public int weightKg;

    Box(final int weightKg) {
        this.weightKg = weightKg;
    }
}

public class Sort3Vars {
    public static void main(String... args) {
        int iA = 21;
        int iB = 11;
        int iC = 82;
        int[] sortedInt = Stream.of(iA, iB, iC).sorted().mapToInt(Integer::intValue).toArray();
        iA = sortedInt[0];
        iB = sortedInt[1];
        iC = sortedInt[2];
        System.out.printf("Sorted values: %d %d %d%n", iA, iB, iC);

        String sA = "s21";
        String sB = "s11";
        String sC = "s82";
        Object[] sortedStr = Stream.of(sA, sB, sC).sorted().toArray();
        sA = (String) sortedStr[0];
        sB = (String) sortedStr[1];
        sC = (String) sortedStr[2];
        System.out.printf("Sorted values: %s %s %s%n", sA, sB, sC);

        Box bA = new Box(200);
        Box bB = new Box(12);
        Box bC = new Box(143);
        // Provides a comparator for Box instances
        Object[] sortedBox = Stream.of(bA, bB, bC).sorted(Comparator.comparingInt(a -> a.weightKg)).toArray();
        bA = (Box) sortedBox[0];
        bB = (Box) sortedBox[1];
        bC = (Box) sortedBox[2];
        System.out.printf("Sorted Boxes: %dKg %dKg %dKg%n", bA.weightKg, bB.weightKg, bC.weightKg);
    }
}

```

Output:

```txt
Sorted values: 11 21 82
Sorted values: s11 s21 s82
Sorted Boxes: 12Kg 143Kg 200Kg
```



## JavaScript


```JavaScript
const printThree = (note, [a, b, c], [a1, b1, c1]) => {
  console.log(`${note}
    ${a} is: ${a1}
    ${b} is: ${b1}
    ${c} is: ${c1}
  `);
};
const sortThree = () => {

  let a = 'lions, tigers, and';
  let b = 'bears, oh my!';
  let c = '(from the "Wizard of OZ")';
  printThree('Before Sorting', ['a', 'b', 'c'], [a, b, c]);

  [a, b, c] = [a, b, c].sort();
  printThree('After Sorting', ['a', 'b', 'c'], [a, b, c]);

  let x = 77444;
  let y = -12;
  let z = 0;
  printThree('Before Sorting', ['x', 'y', 'z'], [x, y, z]);

  [x, y, z] = [x, y, z].sort();
  printThree('After Sorting', ['x', 'y', 'z'], [x, y, z]);
};
sortThree();

```

Output:

```txt
Before Sorting
    a is: lions, tigers, and
    b is: bears, oh my!
    c is: (from the "Wizard of OZ")

After Sorting
    a is: (from the "Wizard of OZ")
    b is: bears, oh my!
    c is: lions, tigers, and

Before Sorting
    x is: 77444
    y is: -12
    z is: 0

After Sorting
    x is: -12
    y is: 0
    z is: 77444

```



## jq

In this entry, we shall use a JSON object to represent a set of variables and their bindings.  jq supports this interpretation: for example, the assignment operator '=' can be used to set the value associated with a key, as in `.x = 0`.
jq also allows JSON objects to be specified in a jq program using a JSON-like syntax, e.g. {x: 1}, as illustrated below.

The sorting function defined here is completely generic with respect to the number of variables and the types of their values.  For brevity, however, we will only use the required examples.


```jq
def example1:
 {x: "lions, tigers, and",
  y: "bears, oh my",
  z: "(from the \"Wizard of OZ\")"
 };

def example2:
 {x: 77444,
  y: -12,
  z: 0
 };
```


The following sorting function will accept an arbitrary JSON object:

```jq
def sortVariables:
  keys_unsorted as $keys
  | ([.[]] | sort) as $values
  | reduce range(0; $keys|length) as $i ({}; .[$keys[$i]] = ($values[$i]) ) ;
```


Examples:

```jq
example1 | sortVariables
```

Output:

```txt
{
  "x": "(from the \"Wizard of OZ\")",
  "y": "bears, oh my",
  "z": "lions, tigers, and"
}

```


```jq
example2 | sortVariables
```

Output:
```txt

{
  "x": -12,
  "y": 0,
  "z": 77444
}
```



## Jsish

```Jsish
#!/usr/bin/env jsish
/* Sort three variables, in Jsish. semi-colon start/end for unit test echo */

var x =  'lions, tigers, and';
var y =  'bears, oh my!';
var z =  '(from the "Wizard of OZ")';

var arr = [x,y,z];
arr = arr.sort();

;'As strings, before:';
;x;
;y;
;z;

x = arr.shift();
y = arr.shift();
z = arr.shift();

;'x,y,z after:';
;x;
;y;
;z;

x =  77444;
y =    -12;
z =      0;

arr = [x,y,z];
arr = arr.sort();

;'As numbers before:';
;x;
;y;
;z;

x = arr.shift();
y = arr.shift();
z = arr.shift();

;'x,y,z after:';
;x;
;y;
;z;

;'Mixed, integer, float, string';
x =  3.14159;
y =  2;
z =  '1 string';
;x;
;y;
;z;
arr = [x,y,z].sort();
x = arr.shift(); y = arr.shift(); z = arr.shift();
;'x,y,z after:';
;x;
;y;
;z;


/*
=!EXPECTSTART!=
'As strings, before:'
x ==> lions, tigers, and
y ==> bears, oh my!
z ==> (from the "Wizard of OZ")
'x,y,z after:'
x ==> (from the "Wizard of OZ")
y ==> bears, oh my!
z ==> lions, tigers, and
'As numbers before:'
x ==> 77444
y ==> -12
z ==> 0
'x,y,z after:'
x ==> -12
y ==> 0
z ==> 77444
'Mixed, integer, float, string'
x ==> 3.14159
y ==> 2
z ==> 1 string
'x,y,z after:'
x ==> 2
y ==> 3.14159
z ==> 1 string
=!EXPECTEND!=
*/
```


Output:

```txt
prompt$ jsish --U sortThree.jsi
'As strings, before:'
x ==> lions, tigers, and
y ==> bears, oh my!
z ==> (from the "Wizard of OZ")
'x,y,z after:'
x ==> (from the "Wizard of OZ")
y ==> bears, oh my!
z ==> lions, tigers, and
'As numbers before:'
x ==> 77444
y ==> -12
z ==> 0
'x,y,z after:'
x ==> -12
y ==> 0
z ==> 77444
'Mixed, integer, float, string'
x ==> 3.14159
y ==> 2
z ==> 1 string
'x,y,z after:'
x ==> 2
y ==> 3.14159
z ==> 1 string

prompt$ jsish -u sortThree.jsi
[PASS] sortThree.jsi
```



## Julia


```julia
# v0.6

a, b, c = "lions, tigers, and", "bears, oh my!", "(from the \"Wizard of OZ\")"
a, b, c = sort([a, b, c])
@show a b c

a, b, c = 77444, -12, 0
a, b, c = sort([a, b, c])
@show a b c
```


Output:

```txt
a = "(from the \"Wizard of OZ\")"
b = "bears, oh my!"
c = "lions, tigers, and"
a = -12
b = 0
c = 77444
```



## Kotlin

Kotlin's standard library contains a sort() function which can sort any generic array whose element type has a defined ordering between its instances. This includes strings, integers and floats examples of which are shown below:

```scala
// version 1.1.2

inline fun <reified T : Comparable<T>> sortThree(x: T, y: T, z: T): Triple<T, T, T> {
    val a = arrayOf(x, y, z)
    a.sort()
    return Triple(a[0], a[1], a[2])
}

fun <T> printThree(x: T, y: T, z: T) = println("x = $x\ny = $y\nz = $z\n")

fun main(args: Array<String>) {
    var x = "lions, tigers, and"
    var y = "bears, oh my!"
    var z = """(from the "Wizard of OZ")"""
    val t = sortThree(x, y, z)
    x = t.first
    y = t.second
    z = t.third
    printThree(x, y, z)

    var x2 = 77444
    var y2 = -12
    var z2 = 0
    val t2 = sortThree(x2, y2, z2)
    x2 = t2.first
    y2 = t2.second
    z2 = t2.third
    printThree(x2, y2, z2)

    var x3 = 174.5
    var y3 = -62.5
    var z3 = 41.7
    val t3 = sortThree(x3, y3, z3)
    x3 = t3.first
    y3 = t3.second
    z3 = t3.third
    printThree(x3, y3, z3)
}
```


Output:

```txt
x = (from the "Wizard of OZ")
y = bears, oh my!
z = lions, tigers, and

x = -12
y = 0
z = 77444

x = -62.5
y = 41.7
z = 174.5
```



## Lua

Essentially the same as the example algorithm except that the function is variadic.  Sorting is done by the in-built table.sort and copying the unknown number of variables in and out of the table is made simple by the (...) in the parameter list and the unpack function respectively.

```Lua
function variadicSort (...)
  local t = {}
  for _, x in pairs{...} do
    table.insert(t, x)
  end
  table.sort(t)
  return unpack(t)
end

local testCases = {
  { x = 'lions, tigers, and',
    y = 'bears, oh my!',
    z = '(from the "Wizard of OZ")'
  },
  { x = 77444,
    y = -12,
    z = 0
  }
}
for i, case in ipairs(testCases) do
  x, y, z = variadicSort(case.x, case.y, case.z)
  print("\nCase " .. i)
  print("\tx = " .. x)
  print("\ty = " .. y)
  print("\tz = " .. z)
end
```

Output:

```txt
Case 1
        x = (from the "Wizard of OZ")
        y = bears, oh my!
        z = lions, tigers, and

Case 2
        x = -12
        y = 0
        z = 77444
```



## M2000 Interpreter

Variables in M2000 are type of Variant, Interpreter use name type ($) to use proper expression parser something as string.
In this example we pass reference from numeric to string identifiers, but data under never moved, so we can read the type, and then
we can make second reference to numeric names, and then we use the proper expression parser.

Swap statement swap variants (data), not references, and always swap 16bytes. All arrays are variant type, so we can swap array item with any variable (but names must be the same type - strings or numeric both)

Subs can be found from parent context, but executed like they are in current context.  A sub has scope in Module, so can call any sub including itself. For optimization , Interpreter mark in temporary list the sub positions, at the first call.
Modules and Functions have a special list, where included at definition time, end excluded when parent exit.


```M2000 Interpreter

Module Sort3 {
      Let X=77744, Y=-12, Z=0
      Let X$ =  "lions, tigers, and",  Y$ =  "bears, oh my!",  Z$ =  {(from the "Wizard of OZ")}
      \\ & use for by reference pass
      Module SortSome (&X$, &Y$, &Z$){
            If Type$(X$)<>"String" Then {
                  Link X$,Y$, Z$ to X,Y,Z
                  Print3()
                  If Y>Z then Swap Y, Z  ' both numeric in Swap
                  If X>Z then Swap X, Z
                  If X>Y then Swap X, Y
                  Print3()
            } Else {
                  Print3Str()
                  If Y$>Z$ then Swap Y$, Z$ ' both strings in Swap
                  If X$>Z$ then Swap X$, Z$
                  If X$>Y$ then Swap X$, Y$
                  Print3Str()
            }
      }
      SortSome &X, &Y, &Z
      SortSome &X$, &Y$, &Z$
      Sub Print3()
            \\ double ,, used to insert a New Line
            Print "X=",X,,"Y=",Y,,"Z=",Z
      End Sub
      Sub Print3Str()
            Print "X$=",X$,,"Y$=",Y$,,"Z$=",Z$
      End Sub
}
Sort3

```



## Maple


```Maple
lst := sort([x,y,z]):
x,y,z := lst[1],lst[2],lst[3]:
```

{{Out|Example}}
x :=  'lions, tigers, and':
y :=  'bears, oh my!':
z :=  '(from the "Wizard of OZ")':

```txt
>>>x;
"(from the "Wizard of OZ")"
>>>y;
"bears, oh my!"
>>>z;
"lions, tigers, and"
```

x :=  77444:
y :=  -12:
z :=  0:

```txt
>>>x;
-12
>>>y;
0
>>>z;
77444
```



## Mathematica

{{Output?}}
All variables in Mathematica are typeless so the follow code works in all cases

```Mathematica
{x, y, z} = Sort[{x, y, z}]
```


## min

Works with min 0.19.3
The <code>sort3</code> operator updates the values of the variables given to it in the calling scope, much like passing by reference. A small drawback is that we must quote the symbols passed in, lest we lose access to them.

```min
(=c =b =a (a -> b -> c ->) => '> sort -> c @ b @ a @) :sort3

"lions, tigers, and" :x
"bears, oh my!" :y
"(from the \"Wizard of OZ\")" :z

'x 'y 'z sort3
x puts!
y puts!
z puts!

77444 :x
-12 :y
0 :z

'x 'y 'z sort3
x puts!
y puts!
z puts!
```

Output:

```txt

(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
-12
0
77444

```


## Modula-2

```modula2
MODULE SortThreeVariables;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE SwapInt(VAR a,b : INTEGER);
VAR t : INTEGER;
BEGIN
    t := a;
    a := b;
    b := t;
END SwapInt;

PROCEDURE Sort3Int(VAR x,y,z : INTEGER);
BEGIN
    IF x<y THEN
        IF z<x THEN
            SwapInt(x,z);
        END;
    ELSIF y<z THEN
        SwapInt(x,y);
    ELSE
        SwapInt(x,z);
    END;
    IF z<y THEN
        SwapInt(y,z);
    END;
END Sort3Int;

VAR
    buf : ARRAY[0..63] OF CHAR;
    a,b,c : INTEGER;
BEGIN
    a := 77444;
    b := -12;
    c := 0;
    FormatString("Before a=[%i]; b=[%i]; c=[%i]\n", buf, a, b, c);
    WriteString(buf);

    Sort3Int(a,b,c);
    FormatString("Before a=[%i]; b=[%i]; c=[%i]\n", buf, a, b, c);
    WriteString(buf);

    ReadChar;
END SortThreeVariables.
```



## OCaml


```ocaml
let sortrefs list =
  let sorted = List.map ( ! ) list
               |> List.sort (fun a b ->
                   if a < b then -1 else
                   if a > b then  1 else
                     0) in
  List.iter2 (fun v x -> v := x) list sorted

open Printf

let test () =
  let x = ref "lions, tigers, and" in
  let y = ref "bears, oh my!" in
  let z = ref "(from the \"Wizard of OZ\")" in
  sortrefs [x; y; z];
  print_endline "case 1:";
  printf "\tx: %s\n" !x;
  printf "\ty: %s\n" !y;
  printf "\tz: %s\n" !z;

  let x = ref 77444 in
  let y = ref (-12) in
  let z = ref 0 in
  sortrefs [x; y; z];
  print_endline "case 1:";
  printf "\tx: %d\n" !x;
  printf "\ty: %d\n" !y;
  printf "\tz: %d\n" !z
```


Output:

```txt
# test ();;
case 1:
	x: (from the "Wizard of OZ")
	y: bears, oh my!
	z: lions, tigers, and
case 1:
	x: -12
	y: 0
	z: 77444
- : unit = ()
```



## Perl


Works with Perl 5.10+


```Perl
#!/usr/bin/env perl
use 5.010_000;

# Sort strings

my $x = 'lions, tigers, and';
my $y = 'bears, oh my!';
my $z = '(from the "Wizard of OZ")';

# When assigning a list to list, the values are mapped
( $x, $y, $z ) = sort ( $x, $y, $z );

say 'Case 1:';
say "  x = $x";
say "  y = $y";
say "  z = $z";

# Sort numbers

$x = 77444;
$y = -12;
$z = 0;

# The sort function can take a customizing block parameter.
# The spaceship operator creates a by-value numeric sort
( $x, $y, $z ) = sort { $a <=> $b } ( $x, $y, $z );

say 'Case 2:';
say "  x = $x";
say "  y = $y";
say "  z = $z";
```


Output:

```txt
Case 1:
  x = (from the "Wizard of OZ")
  y = bears, oh my!
  z = lions, tigers, and

Case 2:
  x = -12
  y = 0
  z = 77444
```



## Perl 6

Perl 6 has a built in sort routine which uses a variation of quicksort. The built in sort routine will automatically select a numeric sort if given a list of Real numeric items and a lexical Unicode sort if given a list that contains strings. The default numeric sort won't sort complex numbers unless you give it a custom comparitor. It is trivial to modify the sort comparitor function to get whatever ordering you want though.

The list (77444, -12, 0) is a poor choice to demonstrate numeric sort since it will sort the same numerically or lexically. Instead we'll use (7.7444e4, -12, 18/2). ( A Num, an Int, and a Rat. )


```perl6
# Sorting strings. Added a vertical bar between strings to make them discernable
my ($a, $b, $c) = 'lions, tigers, and', 'bears,  oh my!', '(from "The Wizard of Oz")';
say "sorting: {($a, $b, $c).join('|')}";
say ($a, $b, $c).sort.join('|'), ' - standard lexical string sort';

# Sorting numeric things
my ($x, $y, $z) = 7.7444e4, -12, 18/2;
say "\nsorting: $x $y $z";
say ($x, $y, $z).sort, ' - standard numeric sort, low to high';

# Or, with a modified comparitor:
for  -*,       ' - numeric sort high to low',
     ~*,       ' - lexical "string" sort',
     *.chars,  ' - sort by string length short to long',
     -*.chars, ' - or long to short'
  -> $comparitor, $type {
    my ($x, $y, $z) = 7.7444e4, -12, 18/2;
    say ($x, $y, $z).sort( &$comparitor ), $type;
}
say '';

# sort ALL THE THINGS
# sorts by lexical order with numeric values by magnitude.
.say for ($a, $b, $c, $x, $y, $z).sort;
```


Output:

```txt
sorting: lions, tigers, and|bears,  oh my!|(from "The Wizard of Oz")
(from "The Wizard of Oz")|bears,  oh my!|lions, tigers, and - standard lexical string sort

sorting: 77444 -12 9
(-12 9 77444) - standard numeric sort, low to high
(77444 9 -12) - numeric sort high to low
(-12 77444 9) - lexical "string" sort
(9 -12 77444) - sort by string length short to long
(77444 -12 9) - or long to short

(from "The Wizard of Oz")
-12
9
77444
bears,  oh my!
lions, tigers, and
```



## Phix

Phix is naturally polymorphic

```Phix
object {x,y,z} = {"lions, tigers, and","bears, oh my","(from the \"Wizard of OZ\")"}
?{x,y,z}
{x,y,z} = sort({x,y,z})
?{x,y,z}

{x,y,z} = {77444,-12,0}
?{x,y,z}
{x,y,z} = sort({x,y,z})
?{x,y,z}
```

Output:

```txt
{"lions, tigers, and","bears, oh my","(from the \"Wizard of OZ\")"}
{"(from the \"Wizard of OZ\")","bears, oh my","lions, tigers, and"}
{77444,-12,0}
{-12,0,77444}
```



## PicoLisp


```PicoLisp
(let (X 77444  Y -12  Z 0)
   (println X Y Z)
   (mapc set '(X Y Z) (sort (list X Y Z)))
   (println X Y Z) )
```

Output:

```txt
77444 -12 0
-12 0 77444
```



## PHP


```php
<?php
//Sort strings
$x = 'lions, tigers, and';
$y = 'bears, oh my!';
$z = '(from the "Wizard of OZ")';
$items = [$x, $y, $z];
sort($items);

list($x, $y, $z) = $items;

echo <<<EOT
Case 1:
  x = $x
  y = $y
  z = $z

EOT;

//Sort numbers
$x = 77444;
$y = -12;
$z = 0;
$items = [$x, $y, $z];
sort($items);

list($x, $y, $z) = $items;

echo <<<EOT
Case 2:
  x = $x
  y = $y
  z = $z

EOT;


```


Output:

```txt
Case 1:
  x = (from the "Wizard of OZ")
  y = bears, oh my!
  z = lions, tigers, and
Case 2:
  x = -12
  y = 0
  z = 77444

```


## PureBasic


```purebasic
;sort three variables: x, y, z

;Macro handles any of the native types, including integers, floating point, and strings
;because the variable types are not declared but substituted during each instance of the macro.
;The sorting is in ascending order, i.e. x < y < z.
Macro sort3vars(x, y, z)
  If x > y: Swap x, y: EndIf
  If x > z: Swap x, z: EndIf
  If y > z: Swap y, z: EndIf
EndMacro

;Macro to perform the sorting test for each variable type, again by substitution.
Macro test_sort(x, y, z)
  PrintN("Variables before sorting: " + #CRLF$ + x + #CRLF$ + y + #CRLF$ + z + #CRLF$)
  sort3vars(x, y, z)

  PrintN("Variables after sorting: " + #CRLF$ + x + #CRLF$ + y + #CRLF$ + z + #CRLF$)
EndMacro


;Define three sets of variables each with a different type for testing
Define.s x, y, z ;string
x = "lions, tigers, and"
y = "bears, oh my!"
z = ~"(from the \"Wizard of OZ\")" ;uses an escaped string as one way to include quotation marks

Define x1 = 77444, y1 = -12, z1 = 0 ;integer

Define.f x2= 5.2, y2 = -1133.9, z2 = 0 ;floating point
If OpenConsole()
  PrintN("Sort three variables" + #CRLF$)
  test_sort(x, y, z) ;strings

  test_sort(x1, y1, z1) ;integers

  test_sort(x2, y2, z2) ;floating point

  Print(#CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Sort three variables

Variables before sorting:
lions, tigers, and
bears, oh my!
(from the "Wizard of OZ")

Variables after sorting:
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and

Variables before sorting:
77444
-12
0

Variables after sorting:
-12
0
77444

Variables before sorting:
5.1999998093
-1133.9000244141
0

Variables after sorting:
-1133.9000244141
0
5.1999998093


```



## Python


### Python2

This solution accepts 3 values either strings or numbers from user and then sort it in ascending order. This is implemented in Python 2.

```python

#python2 Code for Sorting 3 values
a= raw_input("Enter values one by one ..\n1.").strip()
b=raw_input("2.").strip()
c=raw_input("3.").strip()
if a>b :
   a,b = b,a
if a>c:
   a,c = c,a
if b>c:
   b,c = c,b
print(str(a)+" "+str(b)+" "+str(c))

```



### Python3

The following uses Python3.6 f-strings and uses the built in sorted function and assignment to a target list.

```python
while True:
    x, y, z = eval(input('Three Python values: '))
    print(f'As read: x = {x!r}; y = {y!r}; z = {z!r}')
    x, y, z = sorted((x, y, z))
    print(f' Sorted: x = {x!r}; y = {y!r}; z = {z!r}')
```


Output:

```txt
Three Python values: 3, 2, 1
As read: x = 3; y = 2; z = 1
 Sorted: x = 1; y = 2; z = 3

Three Python values: 'lions, tigers, and', 'bears, oh my!', '(from the "Wizard of OZ")'
As read: x = 'lions, tigers, and'; y = 'bears, oh my!'; z = '(from the "Wizard of OZ")'
 Sorted: x = '(from the "Wizard of OZ")'; y = 'bears, oh my!'; z = 'lions, tigers, and'

Three Python values: 77444, -12, 0
As read: x = 77444; y = -12; z = 0
 Sorted: x = -12; y = 0; z = 77444

Three Python values:
```



## R

```r
assignVec <- Vectorize("assign", c("x", "value"))
`%<<-%` <- function(x, value) invisible(assignVec(x, value, envir = .GlobalEnv)) # define multiple global assignments operator
```

Output:

```txt
x <- 'lions, tigers, and'
y <- 'bears, oh my!'
z <- '(from the "Wizard of OZ")'

c("x", "y", "z") %<<-% sort(c(x, y, z))

x
## [1] "(from the \"Wizard of OZ\")"
y
## [1] "bears, oh my!"
z
## [1] "lions, tigers, and"


x <- 77444
y <-   -12
z <-     0

c("x", "y", "z") %<<-% sort(c(x, y, z))

x
## [1] -12
y
## [1] 0
z
## [1] 77444

```


## Racket

Ugh... mutation. Anyway...

```
#lang racket

(define-syntax-rule (sort-3! x y z <?)
  (begin
    (define-syntax-rule (swap! x y) (let ((tmp x)) (set! x y) (set! y tmp)))
    (define-syntax-rule (sort-2! x y) (when (<? y x) (swap! x y)))
    (sort-2! x y)
    (sort-2! x z)
    (sort-2! y z)))

(module+ test
  (require rackunit
           data/order)

  (define (test-permutations l <?)
    (test-case
     (format "test-permutations ~a" (object-name <?))
     (for ((p (in-permutations l)))
       (match-define (list a b c) p)
       (sort-3! a b c <?)
       (check-equal? (list a b c) l))))

  (test-permutations '(1 2 3) <)

  ;; string sorting
  (let ((x  "lions, tigers, and")
        (y  "bears, oh my!")
        (z  "(from the \"Wizard of OZ\")"))
    (sort-3! x y z string<?)
    (for-each displayln (list x y z)))

  (newline)

  ;; general data sorting
  (define datum<? (order-<? datum-order))
  (let ((x  "lions, tigers, and")
        (y  "bears, oh my!")
        (z  '(from the "Wizard of OZ")))
    (sort-3! x y z datum<?)
    (for-each displayln (list x y z))))
```


Output:

```txt
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and

bears, oh my!
lions, tigers, and
(from the Wizard of OZ)
```



## REXX

Since Classic REXX has no native sorting built-in, here is an alternative algorithm.


### Generic

This version will sort numbers and/or literals.

The literals can be of any length   (only limited by virtual memory or language limitations).

```rexx
/*REXX program sorts three (any value)  variables  (X, Y, and  Z)  into ascending order.*/
parse arg x y z .                                /*obtain the three variables from C.L. */
if x=='' | x==","  then x= 'lions, tigers, and'        /*Not specified?  Use the default*/
if y=='' | y==","  then y= 'bears,  oh my!'            /* "      "        "   "     "   */
if z=='' | z==","  then z= '(from "The Wizard of Oz")' /* "      "        "   "     "   */
say '───── original value of X: '   x
say '───── original value of Y: '   y
say '───── original value of Z: '   z
if x>y  then do;  _=x;  x=y;  y=_;  end          /*swap the values of   X   and   Y.    */      /* ◄─── sorting.*/
if y>z  then do;  _=y;  y=z;  z=_;  end          /*  "   "     "    "   Y    "    Z.    */      /* ◄─── sorting.*/
if x>y  then do;  _=x;  x=y;  y=_;  end          /*  "   "     "    "   X    "    Y.    */      /* ◄─── sorting */
say                                              /*stick a fork in it,  we're all done. */
say '═════  sorted  value of X: '   x
say '═════  sorted  value of Y: '   y
say '═════  sorted  value of Z: '   z
```

Output:

```txt
───── original value of X:  lions, tigers, and
───── original value of Y:  bears,  oh my!
───── original value of Z:  (from "The Wizard of Oz")

═════  sorted  value of X:  (from "The Wizard of Oz")
═════  sorted  value of Y:  bears,  oh my!
═════  sorted  value of Z:  lions, tigers, and
```



### numeric only

This version will sort numbers   (the numbers can be in any form, floating point and/or integer).

The maximum integer than be kept   ''as an integer''   is   (in this program)   is 1,000 decimal digits.

```rexx
/*REXX program sorts  three (numeric)  variables  (X, Y, and  Z)  into ascending order. */
numeric digits 1000                              /*handle some pretty gihugic integers. */      /*can be bigger.*/
parse arg x y z .                                /*obtain the three variables from C.L. */
if x=='' | x==","  then x= 77444                 /*Not specified?  Then use the default.*/
if y=='' | y==","  then y=   -12                 /* "      "         "   "   "     "    */
if z=='' | z==","  then z=     0                 /* "      "         "   "   "     "    */
w=max( length(x), length(y), length(z) )   + 5   /*find max width of the values, plus 5.*/
say '───── original values of X, Y, and Z: '       right(x, w)   right(y, w)   right(z, w)
low = x                                          /*assign a temporary variable.         */      /* ◄─── sorting.*/
mid = y                                          /*   "   "     "        "              */      /* ◄─── sorting.*/
high= z                                          /*   "   "     "        "              */      /* ◄─── sorting.*/
              x=min(low,  mid,  high)            /*determine the lowest value of X,Y,Z. */      /* ◄─── sorting.*/
              z=max(low,  mid,  high)            /*    "      "  highest  "    " " " "  */      /* ◄─── sorting.*/
              y=    low + mid + high - x - z     /*    "      "  middle   "    " " " "  */      /* ◄─── sorting.*/
                                                 /*stick a fork in it,  we're all done. */
say '═════  sorted  values of X, Y, and Z: '       right(x, w)   right(y, w)   right(z, w)
```

Output:

```txt
───── original values of X, Y, and Z:       77444        -12          0
═════  sorted  values of X, Y, and Z:         -12          0      77444
```


## Ring

```ring
# Project : Sort three variables

x = 'lions, tigers, and'
y = 'bears, oh my!'
z = '(from the "Wizard of OZ")'
sortthree(x,y,z)
x = 77444
y = -12
z = 0
sortthree(x,y,z)

func sortthree(x,y,z)
        str = []
        add(str,x)
        add(str,y)
        add(str,z)
        str = sort(str)
        see "x = " + str[1] + nl
        see "y = " + str[2] + nl
        see "z = " + str[3] + nl
        see nl

```

Output:

```txt
x = (from the "Wizard of OZ")
y = bears, oh my!
z = lions, tigers, and

x = -12
y = 0
z = 77444
```



## Ruby


```ruby
x =  'lions, tigers, and'
y =  'bears, oh my!'
z =  '(from the "Wizard of OZ")'
x, y, z = [x, y, z].sort
puts x, y, z

x, y, z = 7.7444e4, -12, 18/2r # Float, Integer, Rational; taken from Perl 6
x, y, z = [x, y, z].sort
puts x, y, z

```

Output:

```txt
(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
-12
9/1
77444.0
```


## Ring


```ring
x = 77444
y = -12
z = 0
sList = sortList(x, y, z)
see sList + nl

x = 'lions, tigers, and'
y = 'bears, oh my!'
z = '(from the "Wizard of OZ")'
sList = sortList(x, y, z)
see sList + nl

func sortList (x, y, z)
     aList = [x, y, z]
     sList = sort(aList)
     return sList

```

Output:

```txt
-12
0
77444

(from the "Wizard of OZ")
bears, oh my!
lions, tigers, and
```


## Scala

As a Functional Programming language embraces the use of immutable variables.
Mutability (variables and states) is seen as evil.
Scala works rather with transformed collection obtained by functions then mutable variable and variable state. So this task has no sense in modern programming.


## Seed7

```seed7
$ include "seed7_05.s7i";

const proc: genSort3 (in type: elemType) is func
  begin
    global

    const proc: doSort3 (in var elemType: x, in var elemType: y, in var elemType: z) is func
      local
        var array elemType: sorted is 0 times elemType.value;
      begin
        writeln("BEFORE: x=[" <& x <& "]; y=[" <& y <& "]; z=[" <& z <& "]");
        sorted := sort([](x, y, z));
        x := sorted[1];
        y := sorted[2];
        z := sorted[3];
        writeln("AFTER: x=[" <& x <& "]; y=[" <& y <& "]; z=[" <& z <& "]");
      end func;

    end global;
  end func;

genSort3(integer);
genSort3(string);

const proc: main is func
  begin
    doSort3(77444, -12, 0);
    doSort3("lions, tigers, and", "bears, oh my!", "(from the \"Wizard of OZ\")");
  end func;
```


Output:

```txt

BEFORE: x=[77444]; y=[-12]; z=[0]
AFTER: x=[-12]; y=[0]; z=[77444]
BEFORE: x=[lions, tigers, and]; y=[bears, oh my!]; z=[(from the "Wizard of OZ")]
AFTER: x=[(from the "Wizard of OZ")]; y=[bears, oh my!]; z=[lions, tigers, and]

```


## Sidef

Generalized solution, for an arbitrary number of variable references:

```ruby
func sort_refs(*arr) {
    arr.map{ *_ }.sort ~Z arr -> each { *_[1] = _[0] }
}

var x = 77444
var y =   -12
var z =     0

sort_refs(\x, \y, \z)

say x
say y
say z
```

Output:

```txt
-12
0
77444
```

Alternatively, without using a sorting function:

```ruby
var x = 77444
var y =   -12
var z =     0

(x, y) = (y, x) if (x > y)
(x, z) = (z, x) if (x > z)
(y, z) = (z, y) if (y > z)

say x
say y
say z
```

Output:

```txt
-12
0
77444
```


## Tcl

```tcl

set x {lions, tigers, and}
set y {bears, oh my!}
set z {(from the "Wizard of OZ")}

lassign [lsort [list $x $y $z]] x y z

puts "x: $x"
puts "y: $y"
puts "z: $z"


set x 77444
set y -12
set z 0

lassign [lsort [list $x $y $z]] x y z

puts "x: $x"
puts "y: $y"
puts "z: $z"

```

Output:

```txt

x: (from the "Wizard of OZ")
y: bears, oh my!
z: lions, tigers, and
x: -12
y: 0
z: 77444

```



## Visual Basic .NET

Translated from C#

```vbnet
Module Module1

    Sub Swap(Of T)(ByRef a As T, ByRef b As T)
        Dim c = a
        a = b
        b = c
    End Sub

    Sub Sort(Of T As IComparable(Of T))(ByRef a As T, ByRef b As T, ByRef c As T)
        If a.CompareTo(b) > 0 Then
            Swap(a, b)
        End If
        If a.CompareTo(c) > 0 Then
            Swap(a, c)
        End If
        If b.CompareTo(c) > 0 Then
            Swap(b, c)
        End If
    End Sub

    Sub Main()
        Dim x = 77444
        Dim y = -12
        Dim z = 0
        Sort(x, y, z)
        Console.WriteLine((x, y, z))

        Dim a = "lions, tigers, and"
        Dim b = "bears, oh my!"
        Dim c = "(from the 'Wizard of OZ')"
        Sort(a, b, c)
        Console.WriteLine((a, b, c))
    End Sub

End Module
```

Output:

```txt
(-12, 0, 77444)
((from the 'Wizard of OZ'), bears, oh my!, lions, tigers, and)
```



## zkl

This solution uses list assignment and list sorting.
Lists are not homogeneous, but sorting usually expects that.
If that is a problem, you can give the sort a compare function.
Numbers (real and integer) are homogeneous enough to sort.

```zkl
x,y,z := "lions, tigers, and", "bears, oh my!", 0'|(from the "Wizard of OZ")|;
x,y,z = List(x,y,z).sort();
println(x," | ",y," | ",z);

x,y,z := 77444, -12, 0;
x,y,z = List(x,y,z).sort();
println(x," ",y," ",z);
```

Output:

```txt
(from the "Wizard of OZ") | bears, oh my! | lions, tigers, and
-12 0 77444
```


+++
title = "Introspection"
description = ""
date = 2019-08-25T05:24:50Z
aliases = []
[extra]
id = 1998
[taxonomies]
categories = ["task", "Programming environment operations"]
languages = [
  "ada",
  "aikido",
  "algol_68",
  "autohotkey",
  "awk",
  "bbc_basic",
  "c",
  "c_sharp",
  "clojure",
  "common_lisp",
  "d",
  "e",
  "echolisp",
  "erlang",
  "factor",
  "forth",
  "freebasic",
  "gap",
  "go",
  "haskell",
  "inform_7",
  "io",
  "j",
  "java",
  "javascript",
  "jsish",
  "julia",
  "kotlin",
  "lasso",
  "lingo",
  "locomotive_basic",
  "logo",
  "logtalk",
  "lua",
  "maple",
  "mathematica",
  "maxima",
  "maxscript",
  "netrexx",
  "nim",
  "ocaml",
  "oforth",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pop11",
  "powerbasic",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "slate",
  "smalltalk",
  "tcl",
  "toka",
  "unix_shell",
  "ursala",
  "vba",
  "yabasic",
  "zkl",
  "zx_spectrum_basic",
]
tags = []
+++

## Task

* verify the version/revision of your currently running (compiler/interpreter/byte-compiler/runtime environment/whatever your language uses) and exit if it is too old.
* check whether the variable "bloop" exists and whether the math-function "abs()" is available and if yes compute <i>abs(bloop)</i>.


;Extra credit:
* Report the number of integer variables in global scope, and their sum.





## Ada

Ada doesn't allow you to ask about compiler versions, but you can query specific parameters of the target, such as the range of the standard integer type, or the precision of the standard floating point type:

```ada
with Ada.Integer_Text_IO, Ada.Text_IO;
 procedure Introspection is
    use Ada.Integer_Text_IO, Ada.Text_IO;
 begin
    Put ("Integer range: ");
    Put (Integer'First);
    Put (" .. ");
    Put (Integer'Last);
    New_Line;

    Put ("Float digits: ");
    Put (Float'Digits);
    New_Line;
 end Introspection;
```

All Ada compilers recognize obsolete parts of a programs and either automatically recompile them or fail to link the program.


## Aikido

The version of the Aikido interpreter is in the global scope variable <em>version</em>.  AIkido doesn't have <code>abs</code> but it does have <code>fabs</code>.  Getting the variables in main involves getting their names and then evaluating them as an expression in order to retrieve their type.

```aikido

import math

if (version < 144) {
    throw "Version of aikido is too old"
}

var bloop = -1.4

// Math package doesn't have 'abs'.  We'll use 'fabs' instead.
if ("fabs" in Math) {
    if ("bloop" in main) {
        println ("fabs(bloop) is " + eval ("Math.fabs(bloop)"))
    }
}

var x = 104
var y = 598
var z = "hello"
var a = 1234

function count_ints {
    // there are builtin integer variables that we don't want to count.  There are
    // 3 of them
    var intcount = 0
    // map of builtin variables we want to ignore
    var ignore = {"version":true, "int":true, "integer":true}
    var sum = 0

    // the 'split' function can be used to split a block into a vector of the names
    // of the variables within it
    foreach v split (main, 0) {
        var varname = v
        try {
            var value = eval (varname)
            if (typeof(value) == "integer") {
                if (varname in ignore) {
                    continue
                }
                intcount++
                sum += value
            }
        } catch (e) {
            // ignore exception
        }
    }
    println ("There are " + intcount + " integer variables in the global scope")
    println ("Their sum is " + sum)
}

count_ints()



```

Here is the result:
 fabs(bloop) is 1.4
 There are 3 integer variables in the global scope
 Their sum is 1936




## ALGOL 68

ALGOL 68 doesn't allow you to ask about compiler versions, but you can use
constants specifying parameters of the target, such as the range of the standard
integer type, or the precision of the standard floating point type.

Also: The constant(s) ''int lengths'' (''real lengths'') is the number (+1) of '''long'''
precision types of '''int''' ('''reals''') available at run time.

```algol68
BEGIN
    print (("Integer range: ", -max int, " .. ", max int, new line));
    print (("Integer digits: ", int width, new line));
    print (("Float range: ", -max real, " .. ", max real, new line));
    print (("Float digits: ", real width, new line))
END
```

```txt

Integer range: -2147483647 .. +2147483647
Integer digits:         +10
Float range: -1.79769313486235e+308 .. +1.79769313486235e+308
Float digits:         +15

```


The types of a value can also - crudely - be determined at run time.  This is most useful for ALGOL 68's tagged unions, but it can also be used on any value.  Note also that if a type is '''union'''ed with a '''void''', then the '''empty''' value can be set to indicate that a variable has not been formally initialised to a value.

The following code demonstrates tagged-union introspection:
```algol68
BEGIN
    MODE SSMODES = UNION(SHORT SHORT BITS, SHORT SHORT BYTES, #SHORT SHORT CHAR,#
                         SHORT SHORT INT, SHORT SHORT REAL, SHORT SHORT COMPL);
    MODE  SMODES = UNION(SHORT BITS, SHORT BYTES, #SHORT CHAR,#
                         SHORT INT, SHORT REAL, SHORT COMPL);
    MODE  NMODES = UNION(BITS, BYTES, CHAR, INT, REAL, COMPL);
    MODE  LMODES = UNION(LONG BITS, LONG BYTES, #LONG CHAR,#
                         LONG INT, LONG REAL, LONG COMPL);
    MODE LLMODES = UNION(LONG LONG BITS, LONG LONG BYTES, #LONG LONG CHAR,#
                         LONG LONG INT, LONG LONG REAL, LONG LONG COMPL);
    MODE  XMODES = UNION(BOOL, SEMA, STRING, VOID, CHANNEL, FILE, FORMAT);

    MODE MODES = UNION(SSMODES, SMODES, NMODES, LLMODES, LMODES, XMODES);

    OP REPRTYPEOF = (MODES val)STRING:
      CASE val IN
        (VOID):"VOID",
        (INT):"INT",(SHORT INT):"SHORT INT",(SHORT SHORT INT):"SHORT SHORT INT",
                    (LONG INT):"LONG INT",(LONG LONG INT):"LONG LONG INT",
        (REAL):"REAL",(SHORT REAL):"SHORT REAL",(SHORT SHORT REAL):"SHORT SHORT REAL",
                      (LONG REAL):"LONG REAL",(LONG LONG REAL):"LONG LONG REAL",
        (COMPL):"COMPL",(SHORT COMPL):"SHORT COMPL",(SHORT SHORT COMPL):"SHORT SHORT COMPL",
                        (LONG COMPL):"LONG COMPL",(LONG LONG COMPL):"LONG LONG COMPL",
        (BITS):"BITS",(SHORT BITS):"SHORT BITS",(SHORT SHORT BITS):"SHORT SHORT BITS",
                      (LONG BITS):"LONG BITS",(LONG LONG BITS):"LONG LONG BITS",
        (BYTES):"BYTES",(SHORT BYTES):"SHORT BYTES",(SHORT SHORT BYTES):"SHORT SHORT BYTES",
                        (LONG BYTES):"LONG BYTES",(LONG LONG BYTES):"LONG LONG BYTES",
        (CHAR):"CHAR",#(SHORT CHAR):"SHORT CHAR",(SHORT SHORT CHAR):"SHORT SHORT CHAR",
                       (LONG CHAR):"LONG CHAR",(LONG LONG CHAR):"LONG LONG CHAR",#
        (BOOL):"BOOL",
        (STRING):"STRING",
        (SEMA):"SEMA",
        (CHANNEL):"CHANNEL",
        (FILE):"FILE",
        (FORMAT):"FORMAT"
      OUT
        "ARRAY, PROC or STRUCT"
      ESAC;

    []MODES x = (EMPTY, 1, 2.0, 3I4, SHORT SHORT 5, SHORT 6, LONG 7, LONG LONG 8,
                 8r666, bytes pack("abc"), TRUE, "xyz", LEVEL 1,
                 stand in channel, stand in, $ddd$);

    STRING sep := "";
    print(("Array member types: "));
    FOR i TO UPB x DO
      print((sep,REPRTYPEOF x[i]));
      sep := ", "
    OD
END
```

```txt

Array member types: VOID, INT, REAL, COMPL, INT, INT, LONG INT, LONG LONG INT, BITS, BYTES, BOOL, STRING, SEMA, CHANNEL, FILE, FORMAT

```



'''User defined "typeof" operator''':

Algol68 permits the use of "operator overloading" over different types of variables. Hence the user is able to define their own introspecting "typeof" operator.

'''File: Typeof_operator.a68'''
```algol68
#!/usr/local/bin/a68g --script #

OP TYPEOF = (INT skip)STRING: "INT";
OP TYPEOF = (CHAR skip)STRING: "CHAR";
OP TYPEOF = (REAL skip)STRING: "REAL";
OP TYPEOF = (COMPL skip)STRING: "COMPL";

printf(($g" "$,TYPEOF 1, TYPEOF "x", TYPEOF pi, TYPEOF (0 I 1 ), $l$))
```

```txt

INT CHAR REAL COMPL

```



'''Array bounds can also be inspected:'''
'''File: Introspection_array_bounds.a68'''
```algol68
#!/usr/local/bin/a68g --script #

[]INT x = (5,4,3,2,1);

print(("x =", x, new line));
print(("LWB x =", LWB x, ", UPB x = ",UPB x, new line))
```

```txt

x =         +5         +4         +3         +2         +1
LWB x =         +1, UPB x =          +5

```



## AutoHotkey


```autohotkey
if (A_AhkVersion < "1.0.48.03")
{
  MsgBox % "you are using" . A_AhkVersion . "`nplease upgrade to" . "1.0.48.03"
  ExitApp
}
bloop = -3
if bloop
  if IsFunc("abs")
    MsgBox % abs(bloop)
return
```



## AWK

{{works with|gawk|4.1.0}} PROCINFO is a gawk-extension

```AWK

# syntax: GAWK -f INTROSPECTION.AWK
BEGIN {
    if (PROCINFO["version"] < "4.1.0") {
      print("version is too old")
      exit(1)
    }
    bloop = -1
    if (PROCINFO["identifiers"]["abs"] == "user" && bloop != "") {
      printf("bloop = %s\n",bloop)
      printf("abs(bloop) = %s\n",abs(bloop))
    }
    exit(0)
}
function abs(x) { if (x >= 0) { return x } else { return -x } }

```

```txt

bloop = -1
abs(bloop) = 1

```



## BBC BASIC

```bbcbasic
      IF VAL(FNversion) < 5.94 THEN PRINT "Version is too old" : END

      ON ERROR LOCAL PRINT "Variable 'bloop' doesn't exist" : END
      test = bloop
      RESTORE ERROR

      ON ERROR LOCAL PRINT "Function 'FNabs()' is not defined" : END
      test = ^FNabs()
      RESTORE ERROR

      PRINT FNabs(bloop)
      END

      DEF FNversion
      LOCAL F%, V$
      F% = OPENOUT(@tmp$+"version.txt")
      OSCLI "OUTPUT "+STR$F%
      *HELP
      *OUTPUT 0
      PTR #F% = 0
      INPUT #F%,V$
      CLOSE #F%
      = RIGHT$(V$,5)
```



## C

Determining the make and version of the compiler, C standard,
and environment features is one of the primary uses of the C preprocessor.
This has allowed C to become the ''lingua franca'' of the [[open source]] movement.

```c
#if !defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L
 #pragma error("C compiler must adhere to at least C99 for the following code.")
 #else
 /* rest of file */
 #endif
```


However, there is no facility in C for checking whether individual variables
and functions have been declared.
In open source, the GNU [[autotools]] are often used for this purpose,
doing this kind of check in a shell script and defining symbols
such as HAVE_ABS which ''can'' be checked by the preprocessor.
## C#
There has to be some caveats made with C#.  There are no truly "global" variables - just publicly exported ones from individual classes/types.  I chose to make a couple of public static variables in my program's class.  Also, the "version" of the compiler is difficult to impossible to get at.  There are no predefined compiler constants that can be compared against as in C/C++ but then again, it's hardly the thing that counts in C#.  What really counts is the version of .NET and the framework you're working with since that determines what C++ features you can use and the various calls that can be made.  Consequently, I check the .NET version to make sure it's past 4.0 and exit if not.

```c#
using System;
using System.Reflection;

namespace Rosetta_Introspection
{
	static public class Program
	{
		static public int bloop = -10;
		static public int bloop2 = -20;

		public static void Main()
		{
			var asm = Assembly.GetExecutingAssembly();
			var version = int.Parse(asm.ImageRuntimeVersion.Split('.')[0].Substring(1));
			if (version < 4)
			{
				Console.WriteLine("Get with the program!  I'm outta here!");
				return;
			}

			FieldInfo bloopField = null;

			foreach (var type in asm.GetExportedTypes())
			{
				foreach (var field in type.GetFields())
				{
					if (field.Name != "bloop")
					{
						continue;
					}
					bloopField = field;
					if (bloopField.FieldType != typeof(int))
					{
						throw new InvalidProgramException("bloop should be an integer");
					}
					break;
				}
				if (bloopField != null)
				{
					break;
				}
			}

			if (bloopField == null)
			{
				throw new InvalidProgramException("No bloop exported value");
			}
			foreach (var refAsm in AppDomain.CurrentDomain.GetAssemblies())
			{
				foreach (var type in refAsm.GetExportedTypes())
				{
					if (type.Name == "Math")
					{
						var absMethod = type.GetMethod("Abs", new Type[] { typeof(int) });
						if (absMethod != null)
						{
							Console.WriteLine("bloop's abs value = {0}", absMethod.Invoke(null, new object[] { bloopField.GetValue(null) }));
						}
					}
				}
			}

			int intCount = 0;
			int total = 0;

			foreach (var type in asm.GetExportedTypes())
			{
				foreach (var field in type.GetFields())
				{
					if (field.FieldType == typeof(int))
					{
						intCount++;
						total += (int)field.GetValue(null);
					}
				}
			}
			Console.WriteLine("{0} exported ints which total to {1}", intCount, total);
			Console.ReadKey();
		}
	}
}

```


```txt
bloop's abs value = 10
2 exported ints which total to -30
```



## Clojure

Partial answer...

```clojure

; check Java version
(let [version (Double/parseDouble (re-find #"\d*\.\d*" (System/getProperty "java.version")))]
  (if (>= version 1.5)
    (println "Version ok")
    (throw (Error. "Bad version"))))

; check Clojure version
(let [version (Double/parseDouble (re-find #"\d*\.\d*" (clojure-version)))]
  (if (>= version 1.0)
    (println "Version ok")
    (throw (Error. "Bad version"))))

```




## Common Lisp


```lisp
(let* ((ver (lisp-implementation-version))
       (major (parse-integer ver :start 0 :end (position #\. ver))))
  #+lispworks (assert (>= 5 major) () "Requires Lispworks version 5 or above")
  #+clisp (assert (>= 2 major) () "Requires CLISP 2.n")
  )
```


```lisp
(defvar bloop -4)
(if (and (fboundp 'abs)
         (boundp 'bloop))
    (format t "~d~%" (abs bloop)))
```

The ''list-all-packages'' and ''do-symbols'' forms enable a lisp program to examine all symbols and these can be tested to identify integer variables.

```lisp
(let ((sum 0)
      (ints '()))
  (loop for pkg in (list-all-packages)
        do (do-symbols (s pkg)
                       (when (and (boundp s)
                                  (integerp (symbol-value s)))
                            (push s ints)
                            (incf sum (symbol-value s)))))
  (format t "there are ~d integer variables adding up to ~d~%"
          (length ints) sum))
```



## D

With extra credit.

```d
// Some module-level variables (D doesn't have a global scope).
immutable x = 3, y = 100, z = 3_000;
short w = 1; // Not an int, must be ignored.
immutable s = "some string"; // Not an int, must be ignored.

void main() {
    import std.compiler, std.math, std.traits;

    // Compile-time constants of the compiler version:
    static assert(version_major > 1 && version_minor > 50,
                  "I can't cope with this compiler version.");

    immutable bloop = 10;

    // To check if something compiles:
    static if (__traits(compiles, bloop.abs)) {
        pragma(msg, "The expression is compilable.");
        auto x = bloop.abs;
    } else {
        pragma(msg, "The expression can't be compiled.");
    }

    import std.stdio;
    immutable s = 10_000; // Not at module scope, must be ignored.

    int tot = 0;
    /*static*/ foreach (name; __traits(allMembers, mixin(__MODULE__)))
        static if (name != "object" &&
                   is(int == Unqual!(typeof(mixin("." ~ name)))))
            tot += mixin("." ~ name);
    writeln("Total of the module-level ints (could overflow): ", tot);
}
```

```txt
The expression is compilable.
Total of the module-level ints (could overflow): 3103
```



## E

Version:

```e
def version := interp.getProps()["e.version"]
```


(There is no built-in version comparison, and the author of this example assumes that implementing a version comparison algorithm isn't the point of this task.)

Existence:

```e
escape fail {
    def &x := meta.getState().fetch("&bloop", fn { fail("no bloop") })
    if (!x.__respondsTo("abs", 0)) { fail("no abs") }
    x.abs()
}
```


This will return either bloop.abs(), "no bloop", or "no abs".

Sum of integers:

```e
{
  var sum := 0
  for &x in interp.getTopScope() { sum += try { x :int } catch _ { 0 } }
  sum
}
```

<code>try</code> rather than an ordinary type check is used because in general a slot might be broken; this way we skip over all read failures as well as non-integers. The block around the code ensures that the sum variable itself will not be involved in the computation.


## EchoLisp


```scheme

(version)
→ EchoLisp - 2.50.3
  📗 local-db: db.version: 3

(when (< (version) 2.6)
  (writeln "Please reload EchoLisp : CTRL-F5, CMD-R or other...")
  (exit))

(if (and (bound? 'bloop) (bound? 'abs) (procedure? abs))
    (abs bloop) 'NOT-HERE)
    → NOT-HERE

(define bloop -777)
   → bloop
(if (and (bound? 'bloop) (bound? 'abs) (procedure? abs)) (abs bloop) 'NOT-HERE)
   → 777

(for/sum ((kv (environment-bindings user-initial-environment )))
    #:when (integer? (second kv))
    (writeln kv)
    (second kv))

    ("bloop" -777)
    ("gee" 555)
    ("buzz" 333)
    → 111 ;; sum



```



## Erlang

Erlang does not have global variables so I look for a function bloop/0 that returns an integer.
Moreover, I sum the available modules, instead of the unavailable global integers.

```Erlang

-module( introspection ).

-export( [task/0] ).

task() ->
    exit_if_too_old( erlang:system_info(otp_release) ),
    Bloop = lists:keyfind( bloop, 1, ?MODULE:module_info(functions) ),
    Abs = lists:keyfind( abs, 1, erlang:module_info(exports) ),
    io:fwrite( "abs( bloop ) => ~p~n", [call_abs_with_bloop(Abs, Bloop)] ),
    io:fwrite( "Number of modules: ~p~n", [erlang:length(code:all_loaded())] ).



bloop() -> -1.

call_abs_with_bloop( {abs, 1}, {bloop, 0} ) -> erlang:abs( bloop() );
call_abs_with_bloop( _Missing, _Not_here ) -> abs_and_bloop_missing.

exit_if_too_old( Release ) when Release	< "R13A" -> erlang:exit( too_old_release );
exit_if_too_old( _Release ) -> ok.

```

```txt

18> introspection:task().
abs( bloop ) => 1
Number of modules: 110

```



## Factor

Check for build number and execute a quotation if it's too old. (There are no such things as versions for Factor yet.)

```factor
: if-older ( n true false -- )
    [ build > ] 2dip if ; inline

: when-older ( n true -- )
    [ ] if-older ; inline
: unless-older ( n false -- )
    [ [ ] ] dip if-older ; inline

900 [ "Your version of Factor is too old." print 1 exit ] when-older
```


It is possible to test if a function or a variable exists (<code>search</code>), but that shouldn't be used outside of parsing.

```factor
"bloop" search [
    get [
        "abs" search [ execute( n -- n' ) ] when*
    ] [ 0 ] if*
] [ 0 ] if*
```


On the other hand, it is possible to search the global namespace for integer variables:

```factor
USING: assocs formatting kernel math namespaces ;

0 0
global [
    nip dup integer? [ + [ 1 + ] dip ] [ drop ] if
] assoc-each
"There are %d integer variables, the sum is %d\n" printf
```



## Forth

Standard Forth doesn't necessarily provide for version numbers, but you can query information about the environment at interpretation time:


```forth
s" MAX-U" environment? [IF]
   0xffffffff <> [IF] .( Requires 32 bits! ) bye [THEN]
[THEN]

[defined] bloop [if]
[defined] abs [if]
  bloop @ abs
[then] [then]
```

[[4tH]] is able to fulfill all requirements. Note that since only one variable has been declared, the sum of all integer (user)variables is consequently the value of that variable.
```forth
[hex] 362 [decimal] 4TH# - [if] [abort] [then]

-32 value bloop
[defined] bloop [if]
  [defined] abs [if]
     bloop abs . cr
  [then]
[then]

0 last cell+ first over over - .( User variables: ) .
?do i @ + loop .( Sum: ) . cr
```

```txt

32
User variables: 1 Sum: -32

```



## FreeBASIC

Version 1:

```freebasic
' FB 1.05.0 Win64

#If __FB_VERSION__ < "1.06.0"
  #Error "Compiler version is too old - needs to be 1.06.0 or later"
#EndIf

Dim bloop As Integer = -15
#IfDef bloop
  #IfDef Abs
    Print "Abs(bloop) = "; Abs(bloop)
  #Else
    Print "Abs is not available"
  #EndIf
#Else
  Print "bloop does not exist"
#EndIf
Sleep
```

```txt
introspection.bas(4) error: Compiler version is too old - needs to be 1.06.0 or later
```

Version 2:

```freebasic
' FB 1.05.0 Win64

#If __FB_VERSION__ < "1.05.0"   '' version 1.05.0 is now OK
  #Error "Compiler version is too old - needs to be 1.05.0 or later"
#EndIf

Dim bloop As Integer = -15
#IfDef bloop
  #IfDef Abs
    Print "Abs(bloop) = "; Abs(bloop)
  #Else
    Print "Abs is not available"
  #EndIf
#Else
  Print "bloop does not exist"
#EndIf
Sleep
```

```txt
Abs(bloop) =  15
```

Version 3 (version code omitted for brevity):

```freebasic
#Undef Abs  '' undefine Abs keyword
Dim bloop As Integer = -15
#IfDef bloop
  #IfDef Abs
    Print "Abs(bloop) = "; Abs(bloop)
  #Else
    Print "Abs is not available"
  #EndIf
#Else
  Print "bloop does not exist"
#EndIf
Sleep
```

```txt
Abs is not available
```

Version 4 (version code omitted for brevity):

```freebasic
#Undef Abs  '' undefine Abs keyword
'Dim bloop As Integer = -15  '' bloop declaration commented out
#IfDef bloop
  #IfDef Abs
    Print "Abs(bloop) = "; Abs(bloop)
  #Else
    Print "Abs is not available"
  #EndIf
#Else
  Print "bloop does not exist"
#EndIf
Sleep
```

```txt
bloop does not exist
```



## GAP


```gap
# Apply a function to a value, given variable names for both function and value
CheckEval := function(fun, val)
	local f, x;
	if IsBoundGlobal(fun) and IsBoundGlobal(val) then
		f := ValueGlobal(fun);
		x := ValueGlobal(val);
		return f(x);
	fi;
end;

bloop := -1;
CheckEval("AbsInt", "bloop");
# 1


# Sum of integer variables
GlobalIntegers := function()
	local s, x, name;
	s := 0;
	for name in SortedList(NamesGVars()) do
		if IsBoundGlobal(name) then
			x := ValueGlobal(name);
			if IsInt(x) then
				Print(name, " ", x, "\n");
				s := s + x;
			fi;
		fi;
	od;
	return s;
end;
```



## Go

Task variance:  "exit if it is too old" is not done here.  Go version strings do not present an easily interpreted chronology.  This version of the program simply prints the version string.

```go
package main

import (
    "debug/elf"
    "debug/gosym"
    "fmt"
    "log"
    "math"
    "os"
    "runtime"
)

var bloop = -3.4

func main() {
    fmt.Println("task 1: verify version")
    fmt.Println("   program compiled with", runtime.Version())

    fmt.Println("task 2: check for presence of variable and function")
    // inspect ELF symbol table
    f, err := elf.Open(os.Args[0])
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()
    symSection := f.Section(".gosymtab")
    lineSection := f.Section(".gopclntab")
    textSection := f.Section(".text")
    if symSection == nil || lineSection == nil || textSection == nil {
        log.Fatal("symbolic information not found")
    }
    symData, err := symSection.Data()
    if err != nil {
        log.Fatal(err)
    }
    lineData, err := lineSection.Data()
    if err != nil {
        log.Fatal(err)
    }
    table, err := gosym.NewTable(symData,
        gosym.NewLineTable(lineData, textSection.Addr))
    if err != nil {
        log.Fatal("  ", err)
    }
    var foundBloop, foundFabs bool
    for _, s := range table.Syms {
        if s.Name == "main.bloop" {
            foundBloop = true
            fmt.Println("   bloop symbol table entry:", s)
        } else if s.Name == "math.Abs" {
            foundFabs = true
            fmt.Println("   abs symbol table entry:", s)
        }
    }
    if foundBloop && foundFabs {
        fmt.Println("   bloop:     ", bloop)
        fmt.Println("   abs(bloop): ", math.Abs(bloop))
    }
}
```

```txt

task 1: verify version
   program compiled with go1.0.2
task 2: check for presence of variable and function
   bloop symbol table entry: {5468424 68 main.bloop 273827584 <nil>}
   abs symbol table entry: {4404832 84 math.Abs 943802368 0xf8401fc190}
   bloop:      -3.4
   abs(bloop):  3.4

```

Work to do for extra credit involves not just locating symbols, but decoding the fields of the the symbol table entries.  Note that once this is done, the basic task could be improved to compute abs(bloop) by accessing values located from symbol table data only, and the expression <tt>math.Abs(bloop)</tt> would not have to appear in the source code.  This would seem in the spirit of the task.


## Haskell



```haskell
import Data.Version
import Control.Monad
import System.Info

minGHCVersion = Version [6, 8] []

main = when (compilerName == "ghc" && compilerVersion < minGHCVersion) $
    fail "Compiler too old."
```


No means exists of checking whether a variable exists at runtime.  The set of variables that exist in any given scope is fixed at compile-time.

=={{header|Icon}} and {{header|Unicon}}==


```unicon
global bloop

procedure main(A)
    if older(11,7) then stop("Must have version >= 11.7!")
    bloop := -5         # global variable
    floop := -11.3      # local variable
    write(proc("abs")(variable("bloop")))
    write(proc("abs")(variable("floop")))
end

procedure older(maj,min)
    &version ? {
        (tab(find("Version ")),move(*"Version "))
        major := 1(tab(upto('.')),move(1))
        minor := tab(upto('.'))
        return (major < maj) | ((major = maj) & (minor < min))
        }
end
```

Sample run:

```txt
->introspect
5
11.3
->
```



## Inform 7

Inform 7 doesn't have built-in functionality for checking the runtime version number, but the version number is available and can be read by including a snippet of Inform 6 code. The address and format of the version number vary according to the virtual machine being targeted.

```inform7
Home is a room.

When play begins:
	let V be the current runtime version;
	if V is less than the required runtime version:
		say "Version [required runtime version] required, but [V] found.";
	otherwise:
		say "Your interpreter claims version [V].";
	end the story.

A version is a kind of value.

Section - Checking the version (for Glulx only)

1.255.255 specifies a version with parts major, minor (without leading zeros), and subminor (without leading zeros).

To decide which version is current runtime version: (- (0-->1) -).
To decide which version is required runtime version: decide on 3.1.2.

Section - Checking the version (for Z-machine only)

1.255 specifies a version with parts major and minor (without leading zeros).

To decide which version is current runtime version: (- ($32-->0) -).
To decide which version is required runtime version: decide on 1.1.
```


It's not possible to check for the existence of functions (invoking a nonexistent phrase causes a compile-time error) or list global variables.


## Io


```io
if(System version < 20080000, exit)

if(hasSlot("bloop") and bloop hasSlot("abs"), bloop abs)
```


Io can also inspect the source code of methods written in Io:

```io
getSlot("arbitraryMethod") code
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 IF VERNUM<2.1 THEN PRINT "Version is too old.":STOP
110 WHEN EXCEPTION USE ERROR
120   PRINT ABS(BLOOP)
130 END WHEN
140 HANDLER ERROR
150   PRINT EXSTRING$(EXTYPE)
160   CONTINUE
170 END HANDLER
```



## J

Exit if we're running an old version of J (earlier than version 6, which is current as of this writing), giving version number as the exit status:

```j
6 (2!:55@:]^:>) 0 ". 1 { 9!:14''
```

Compute <tt>abs(bloop)</tt> if <tt>abs</tt> is a function and <tt>bloop</tt> is data:

```j
".(#~3 0*./ .=4!:0@;:)'abs bloop'
```

'''Extra credit''': report the number of integer variables in global scope, and their sum:

```j
((],&(+/);@#~)((=<.)@[^:](''-:$)*.0=0{.@#,)&>)".&.>4!:1]0
```


This last expression is longer than the others, because it has a couple of extra guard checks; in J, the programmer doesn't need to care if the data is a single number or an array, or what hardware representation is used for numbers (32-bit int, IEEE float, etc).

So this expression takes pains to emulate solutions in other languages (i.e. only reports globals that are single numbers, and whose value = floor(value), so that even if the number is represented as a float in the machine, you still get the right answer).


## Java

You can't see if a variable or function is available in Java (it will be a compile time error if you try to use them when you they aren't available), but you can check the version number using the <tt>System</tt> class:

```java
public class VersCheck {
	public static void main(String[] args) {
		String vers = System.getProperty("java.version");
		vers = vers.substring(0,vers.indexOf('.')) + "." +  //some String fiddling to get the version number into a usable form
			vers.substring(vers.indexOf('.')+1,vers.lastIndexOf('.'));
		if(Double.parseDouble(vers) >= 1.5){
			System.out.println("YAY!");
		}else{
			System.err.println("Must use Java >=1.5");
		}
	}
}
```



## JavaScript


It is generally out of favor to explicitly test versions in JavaScript, due to the immense variety of implementations; instead, one should use “feature testing”, executing code to check for the desired behavior. Checking for “abs” as in this task is an example of feature testing.

Testing whether the variable “bloop” exists:


```javascript
if (typeof bloop !== "undefined") { ... }
```


The <code>typeof</code> operator explicitly does not throw an error when given an undeclared variable.

Test whether <code>Math.abs()</code> is available:


```javascript
if ("abs" in Math) { ... }
```


<code>abs</code> is a method of the Math object, methods are properties, and the <code>in</code> operator tests whether an object has the named property.


## Jsish


```javascript
/* Introspection, in jsish */
if (Info.version() < Util.verConvert('2.8.6')) {
    puts("need at least version 2.8.6 of jsish for this application");
    exit(1);
}

/* Check for "abs()" as function and "bloop" as defined value, call if both check true */
if ((bloop != undefined) && (typeof Math.abs == 'function')) {
    puts(Math.abs(bloop));
}

/* ECMAScript, this will sum all numeric values, not just strict integers */
var nums = 0, sums = 0, v;
for (v of Info.vars(this)) {
    if (isFinite(this[v])) {
        nums++;
        sums += this[v];
    }
}
printf("%d numerics with sum of: %d\n", nums, sums);
```


```txt
prompt$ jsish introspection.jsi
2 numerics with sum of: 2
```



## Julia

```julia
@show VERSION
VERSION < v"0.4" && exit(1)

if isdefined(:bloop) && !isempty(methods(abs))
    @show abs(bloop)
end

a, b, c = 1, 2, 3
vars = filter(x -> eval(x) isa Integer, names(Main))
println("Integer variables: ", join(vars, ", "), ".")
println("Sum of integers in the global scope: ", sum(eval.(vars)), ".")
```


```txt
VERSION = v"0.6.2"
Integer variables: a, b, c.
Sum of integers in the global scope: 6.
```



## Kotlin

We will use Java reflection for this task as Kotlin's own reflection facilities do not appear to be able to deal generically with top-level entities at the present time (i.e. ::class isn't yet supported):

```scala
// version 1.0.6 (intro.kt)

import java.lang.reflect.Method

val bloop = -3
val i = 4
val j = 5
val k = 6

fun main(args: Array<String>) {
    // get version of JVM
    val version = System.getProperty("java.version")
    if (version >= "1.6") println("The current JVM version is $version")
    else println("Must use version 1.6 or later")

    // check that 'bloop' and 'Math.abs' are available
    // note that the class created by the Kotlin compiler for top level declarations will be called 'IntroKt'
    val topLevel = Class.forName("IntroKt")
    val math = Class.forName("java.lang.Math")
    val abs = math.getDeclaredMethod("abs", Int::class.java)
    val methods = topLevel.getDeclaredMethods()
    for (method in methods) {
        // note that the read-only Kotlin property 'bloop' is converted to the static method 'getBloop' in Java
        if (method.name == "getBloop" && method.returnType == Int::class.java) {
            println("\nabs(bloop) = ${abs.invoke(null, method.invoke(null))}")
            break
        }
    }

    // now get the number of global integer variables and their sum
    var count = 0
    var sum = 0
    for (method in methods) {
        if (method.returnType == Int::class.java) {
            count++
            sum += method.invoke(null) as Int
        }
    }
    println("\nThere are $count global integer variables and their sum is $sum")
}
```


```txt

The current JVM version is 1.8.0_121

abs(bloop) = 3

There are 4 global integer variables and their sum is 12

```



## Lasso


```Lasso
var(bloob = -26)

decimal(lasso_version(-lassoversion)) < 9.2 ? abort

var_defined('bloob') and $bloob -> isa(::integer) and lasso_tagexists('math_abs') ? math_abs($bloob)
```

-> 26

Lassos equivalence of global variables are thread variables. They have scope that lasts for the entire call in contrast to local variables that are confined to the page or method they are created within.


```Lasso
var(
	bloob		= -26,
	positive	= 450
)

local(total = integer)

with v in var_keys
// Lasso creates a number of thread variables that all start with an underscore. We don't want those
where not(string(#v) -> beginswith('_')) and var(#v) -> isa(::integer)
do {
	#total += var(#v)
}

#total
```

-> 424


## Lingo

*verify the version/revision of your currently running (compiler/interpreter/byte-compiler/runtime environment/whatever your language uses) and exit if it is too old.

```lingo
put _player.productVersion
-- "11.5.9"

_player.itemDelimiter="."
if integer(_player.productVersion.item[1])<11 then _player.quit()
```


*check whether the variable "bloop" exists and whether the math-function "abs()" is available and if yes compute abs(bloop).

```lingo
-- check existence of bloop in local scope
bloopExists = not voidP(value("bloop"))
-- or for global scope:
-- bloopExists = not voidP(_global.bloop)
absExists = value("abs(1)")=1
if bloopExists and absExists then put abs(bloop) -- or abs(_global.bloop)
```


*Report the number of integer variables in global scope, and their sum.

```lingo
cnt = 0
sum = 0
repeat with v in the globals
  if integerP(v) then
    cnt = cnt + 1
    sum = sum + v
  end if
end repeat
put cnt
put sum
```



## Locomotive Basic


To get the BASIC ROM version number, we need to use a Z80 machine code routine which copies the version number (major/minor/patchlevel) to RAM where BASIC can then read it. Here is the assembly:


```z80
org &4000   ; program start address

push bc
push de
push hl
push af
ld bc,&df00 ; select BASIC ROM
out (c),c   ;   (ROM 0)

ld bc,&7f86 ; make ROM accessible at &c000
out (c),c             ;   (RAM block 3)

ld hl,&c001 ; copy ROM version number to RAM
ld de,&4040
ld bc,3
ldir

ld bc,&7f8e ; turn off ROM
out (c),c
pop af
pop hl
pop de
pop bc
ret
```


The following BASIC program POKEs that routine into memory and quits prematurely if BASIC 1.0 is detected (meaning the machine is a CPC464), as opposed to the more standard 1.1 or later:


```locobasic
10 s=&4000:SYMBOL AFTER 256:MEMORY s-1
20 FOR i=0 to 34:READ a:POKE s+i,a:NEXT
30 DATA &c5,&d5,&e5,&f5,&01,&00,&df,&ed,&49,&01,&86,&7f,&ed,&49
40 DATA &21,&01,&c0,&11,&40,&40,&01,&03,&00,&ed,&b0
50 DATA &01,&8e,&7f,&ed,&49,&f1,&e1,&d1,&c1,&c9
60 CALL s
70 PRINT "BASIC ROM version is ";PEEK(&4040);".";PEEK(&4041);".";PEEK(&4042)
80 IF PEEK(&4041)=0 THEN PRINT "Uh oh, you are still using BASIC 1.0":END
90 PRINT "You are using BASIC 1.1 or later, program can continue"
```


The second subtask, testing for the presence of a variable, is done here by trying to get the memory address of the variable. If that fails, it is obviously is not yet defined:


```locobasic
1 ' decide randomly whether to define the variable:
10 IF RND>.5 THEN bloop=-100*RND
20 ON ERROR GOTO 100
30 a=@bloop  ' try to get a pointer
40 PRINT "Variable bloop exists and its value is",bloop
50 PRINT "ABS of bloop is",ABS(bloop)
90 END
100 IF ERL=30 THEN PRINT "Variable bloop not defined":RESUME 90
```


(Like the Spectrum version, we have omitted checking for ABS because it is a builit-in function of the BASIC interpreter and therefore always present.)

'''Extra credit''': Finally, we can traverse the memory area where BASIC stores its integer, real, and string variables and add together all integers (type 1):


```locobasic
10 ' The program should find and add those three integers (%), ignoring reals:
20 foo%=-4:bar%=7:baz%=9:somereal=3.141
30 varstart=&ae68   ' for CPC 664 and 6128
40 ' varstart=&ae85 ' (use this line instead on the CPC 464)
50 start=PEEK(varstart)+256*PEEK(varstart+1)
60 WHILE start<HIMEM
70 j=2:WHILE j<43 ' skip variable name
80 IF PEEK(start+j)=0 GOTO 170
90 IF PEEK(start+j)>127 THEN ptr=start+j+1:j=100
100 j=j+1:WEND
110 vartype=PEEK(ptr) ' integer=1, string=2, real=4
120 IF vartype=1 THEN sum=sum+UNT(PEEK(ptr+1)+256*PEEK(ptr+2)):num=num+1:nvar=ptr+3
130 IF vartype=2 THEN nvar=ptr+4
140 IF vartype=4 THEN nvar=ptr+6
150 start=nvar
160 WEND
170 PRINT "There are"num"integer variables."
180 PRINT "Their sum is"sum
```


 There are 3 integer variables.
 Their sum is 12


## Logo

```logo
show logoversion   ; 5.6
if logoversion < 6.0 [print [too old!]]

if and [name? "a] [number? :a] [
  print ifelse procedure? "abs [abs :a] [ifelse :a < 0 [minus :a] [:a]]
]
```



## Logtalk


```logtalk

:- object(my_application).

    :- initialization((
        check_logtalk_version,
        compute_predicate_if_available
    )).

    check_logtalk_version :-
        % version data is available by consulting the "version_data" flag
        current_logtalk_flag(version_data, logtalk(Major,Minor,Patch,_)),
        (   (Major,Minor,Patch) @< (3,0,0) ->
            write('Logtalk version is too old! Please upgrade to version 3.0.0 or later'), nl,
            halt
        ;   true
        ).

    % Logtalk is not a functional language and thus doesn't support user-defined functions; we
    % use instead a predicate, abs/2, with a return argument to implement the abs/1 function
    compute_predicate_if_available :-
        (   % check that the variable "bloop" is defined within this object
            current_predicate(bloop/1),
            % assume that the abs/2 predicate, if available, comes from a "utilities" object
            utilities::current_predicate(abs/2) ->
            bloop(Value),
            utilities::abs(Value, Result),
            write('Function value: '), write(Result), nl
        ;   write('Could not compute function!'), nl
        ).

    % our "bloop" variable value as per task description
    bloop(-1).

:- end_object.

```



## Lua


```lua
if _VERSION:sub(5) + 0 < 5.1 then print"too old" end --_VERSION is "Lua <version>".

if bloop and math.abs then print(math.abs(bloop)) end
```



## Maple

The "version" kernel option returns a string similar to

```maple

 kernelopts( 'version' );
                 Maple 16.00, SUN SPARC SOLARIS, Mar 3 2012, Build ID 732982
```

The following does the trick for the first bit.

```maple

 if sscanf( (StringTools:-Split(kernelopts(version))[2]), "%d.%d" )[1] < 300 then `quit`(1) end;
```

(There is also an internal "version" procedure, which returns a build ID, but this is less obvious to use, as you'd need a table mapping versions to build IDs.  Besides, it prints stuff.)

It doesn't really make sense to ask whether a variable "exists"; it springs into existence by uttering it in code.  So I'll interpret the problem as asking whether it is assigned some kind of numeric value to which abs() can be applied.

```maple

 if type( bloop, complex( extended_numeric ) ) and type( abs, mathfunc ) then print( abs( bloop ) ) end:
                                   1/2
                                 13
```

Note that it is not necessary to check that the name "bloop" is assigned (though it is possible to do so), since an unassigned name is a first-class value in Maple.  Another possible interpretation is that the symbolic expression

```maple

 abs( bloop );
                               | bloop |
```

is a perfectly good expression in Maple, so checking for the the "existence" of "bloop" isn't necessary in the first place.  (One probably would not bother to check that abs() was actually there either, unless one expected that the standard library was broken.)

Here are the number and sum of the assigned integer globals in my current (fresh) session.

```maple

 nops([anames](integer));
                                   3

> eval(`+`(anames(integer)));
                                   17

```

If I change it, I get:

```Maple

> foo := 25:
> nops([anames](integer));
                                   4

> eval(`+`(anames(integer)));
                                   42
```



## Mathematica


```Mathematica
If[$VersionNumber  < 8,  Quit[]]
If[NameQ["bloop"] && NameQ["Abs"],
 Print[Abs[bloop]]]
```


```txt
7
```


<lang>globalintegers = Symbol /@ Select[Names["Global`*"], IntegerQ[Symbol[#]] &];
Print [ globalintegers //Length, " global integer(s) and their sum is: ", globalintegers // Total]
```


```txt
2 global integer(s) and their sum is: 9
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
   % convert version into numerical value
   v = version;
   v(v=='.')=' ';
   v = str2num(v);
   if v(2)>10; v(2) = v(2)/10; end;
   ver = v(1)+v(2)/10;
   if exist('OCTAVE_VERSION','builtin')
      if ver < 3.0,
         exit
      end;
   else
      if ver < 7.0,
         exit
      end;
   end

   % test variable bloob, and test whether function abs is defined as m-function, mex-function or builtin-function
   if exist('bloob','var') && any(exist('abs')==[2,3,5])
	printf('abs(bloob) is %f\n',abs(bloob));
	return;
   end;
```

Extra credit task:

```Matlab

   % find all integers
   varlist = whos;
   ix = [strmatch('int', {varlist.class}),strmatch('uint', {varlist.class})];
   intsumall = 0;
   intsum = 0;
   for k=1:length(ix)
      if prod(varlist(ix).size)==1,
         intsum = intsum + eval(varlist.name);		% sum only integer scalars
      elseif prod(varlist(ix).size)>=1,
         tmp = eval(varlist.name);
         intsumall = intsumall + sum(tmp(:));		% sum all elements of integer array.
      end;
   end;
   printf('sum of integer scalars: %i\n',intsum);
   printf('sum of all integer elements: %i\n',intsumall);

```



## Maxima


```maxima
/* Get version information */
build_info();
/* build_info("5.27.0", "2012-05-08 11:27:57", "i686-pc-mingw32", "GNU Common Lisp (GCL)", "GCL 2.6.8") */

%@lisp_version;
/* "GCL 2.6.8" */

/* One can only check for user-defined objects: functions, variables, macros, ...
   Hence we won't check for 'abs, which is built-in, but for 'weekday, defined elsewhere on RosettaCode.
   Here year, month and day are 2012, 05, 29. */

if    subsetp({'year, 'month, 'day}, setify(values))
and   member('weekday, map(op, functions))
then  weekday(year, month, day)
else  'bad\ luck;

/* Sum of integer variables */
lreduce("+", sublist(map(ev, values), integerp));
```



## MAXScript


```maxscript
fn computeAbsBloop bloop =
(
    versionNumber = maxVersion()

    if versionNumber[1] < 9000 then
    (
        print "Max version 9 required"
        return false
    )

    if bloop == undefined then
    (
        print "Bloop is undefined"
        return false
    )

    try
    (
        abs bloop
    )
    catch
    (
        print "No function abs"
        false
    )
)

computeAbsBloop -17
```



## NetRexx

Like [[Java]], NetRexx will not successfully compile a program if a variable or method is accessed before it has been defined.

The language does however return a string identifying the version of NetRexx in effect when the current class was last processed.  This information can be retrieved through the '''<tt>version</tt>''' ''special variable''.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg minVersion .
  if minVersion = '' then minVersion = 2.0
  parse version lang ver bdate
  if ver < minVersion then do
    say -
      lang 'version' ver -
      '[Build date:' bdate']' -
      'is less than' minVersion.format(null, 2)'; exiting...'
    exit
    end
  else do
    say -
      lang 'version' ver -
      '[Build date:' bdate']' -
      'meets minimum requirements of' minVersion.format(null, 2)
    end
  return

```

```txt

$ java RIntrospection
NetRexx version 3.02 [Build date: 25 Jun 2013] meets minimum requirements of 2.00

$ java RIntrospection 4
NetRexx version 3.02 [Build date: 25 Jun 2013] is less than 4.00; exiting...

```



## Nim


```nim
echo NimVersion

var bloop = -12

when compiles abs(bloop):
  echo abs(bloop)
```

```txt
0.10.3
12
```



## OCaml



```ocaml
# Sys.ocaml_version;;
- : string = "3.10.2"
```



```ocaml
# Scanf.sscanf (Sys.ocaml_version) "%d.%d.%d"
               (fun major minor micro -> major, minor, micro) ;;
- : int * int * int = (3, 10, 2)
```


Checking if an identifier (a value or a function) is bound doesn't make any sens in OCaml, which is strongly staticaly typed.

For optionnal values we would rather use a structure to contain them, for example an [http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#6_Associationlists association lists] for a small amount of items, or an [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html hash table] for a huge amount of data. Both can contain expressions or functions.


## Oforth


Oforth does not have global variables, only global constants.


```Oforth
: bloopAbs
| bl m |
   System.VERSION println

   Word find("bloop") ->bl
   bl isA(Constant) ifFalse: [ "bloop constant does not exist" println return ]

   "abs" asMethod ->m
   m ifNull: [ "abs method does not exist" println return ]

   System.Out "bloop value is : " << bl value << cr
   System.Out "bloop abs   is : " << bl value m perform << cr ;
```


```txt

>bloopAbs
V0.9.17
bloop constant does not exist
ok
>-2.3 Constant new: bloop
ok
>bloopAbs
V0.9.17
bloop value is : -2.3
bloop abs   is : 2.3
ok

```



## Oz

We cannot check whether a variable is in scope (static property).
We <em>can</em> check whether a module exports a certain value. However, for system modules the compiler will refuse to compile if we try to use a non-existing value.

```oz
declare
  Version = {Property.get 'oz.version'}
  %% Version is an atom like '1.4.0'. So we can not compare it directly.
  %% Extract the version components:
  [Major Minor Release] = {Map {String.tokens {Atom.toString Version} &.} String.toInt}
in
  if Major >= 1 andthen  Minor >= 4 then
     %% check whether module Number exports a value 'abs':
     if {HasFeature Number abs} then
        {Show {Number.abs ~42}}
     end
  else
     {System.showInfo "Your Mozart version is too old."}
  end
```



## PARI/GP

```parigp
if(lex(version(), [2,4,3]) < 0, quit()); \\ Compare the version to 2.4.3 lexicographically

if(bloop!='bloop && type(abs) == "t_CLOSURE", abs(bloop))
```



## Perl

```perl
require v5.6.1;    # run time version check
require 5.6.1;     # ditto
require 5.006_001; # ditto; preferred for backwards compatibility
```


To check if a variable exists, do a name lookup of it in the package symbol table:

```perl
#$bloop = -123;   # uncomment this line to see the difference
no strict 'refs'; # referring to variable by name goes against 'strict' pragma
if (defined($::{'bloop'})) {print abs(${'bloop'})} else {print "bloop isn't defined"};
```


To check if certain built-in function is available (maybe you are using a stripped down build of perl binary, for example), one can use eval, but make sure the statement you are eval'ing doesn't have side effect:

```perl
eval('abs(0)');  # eval("") instead of eval{}; the latter is not for run-time check
print "abs() doesn't seem to be available\n" if $@;
```


To check if a package or object provides certain method name, use 'can':

```perl
use Math::Complex;
my $cpl = Math::Complex->new(1,1);

print "package Math::Complex has 'make' method\n"
        if Math::Complex->can('make');

print "object \$cpl does not have 'explode' method\n"
        unless $cpl->can('explode');

```

Keep in mind that what a package has as method names are not equal to what method names can be called on this package, due to things like AUTOLOAD.
For most uses, introspection is meaningless, just call the method (and catch exceptions if it's important).

An example that solves the task:

```perl
use 5.010;
our $bloop = -12;
if (defined $::bloop) {
    if (eval { abs(1) }) {
        say 'abs($bloop) is ' . abs($::bloop);
    }
    else {
        say 'abs() is not available';
    }
}
else {
    say '$bloop is not defined';
}
```

Note that this program will exit with a message "Perl v5.10.0 required" if run under perl version lower than 5.10 and it actually uses a feature introduced in that version (<code>say</code>).
The program checks whether the variable is actually defined and not if it just exists.
In Perl a variable can exist and have an undefined value, and checking for existence is problematic because many read-only operations may create an empty slot in the global namespace as a side effect. It doesn't make sense to calculate absolute values of existent but undefined variables so it doesn't matter in this task.
It can be tested by commenting out the <code>our $bloop</code> line.
The existence of <code>abs()</code> function is tested using <code>eval</code> that returns false if the <code>abs(1)</code> cannot be invoked which can be tested by changing the name of the function in the <code>eval</code> test.

Extra task:

```perl
use 5.010;
package test;
use Regexp::Common;
use List::Util qw(sum);

our $a = 7;
our $b = 1;
our $c = 2;
our $d = -5;
our $e = 'text';
our $f = 0.25;

my @ints = grep { /^$RE{num}{int}$/ } map { $$_ // '' } values %::test::;
my $num = @ints;
my $sum = sum @ints;
say "$num integers, sum = $sum";
```

It prints:

```txt

4 integers, sum = 5

```

This example uses the <code>test</code> namespace instead of the default, because there already are some integer numbers in the <code>main</code> namespace like the PID, etc.
The program to sum those numbers would be:

```perl
use 5.010;
use Regexp::Common;
use List::Util qw(sum);
my @ints = grep { /^$RE{num}{int}$/ } map { $$_ // '' } values %::;
my $num = @ints;
my $sum = sum @ints;
say "$num integers, sum = $sum";
```


```txt

4 integers, sum = 74717

```



## Perl 6


```perl6
use v6;   # require Perl 6

my $bloop = -123;

if MY::{'$bloop'}.defined and CORE::{'&abs'}.defined { say abs $bloop }

my @ints = ($_ when Int for PROCESS::.values);
say "Number of PROCESS vars of type Int: ", +@ints;
say "PROCESS vars of type Int add up to ", [+] @ints;
```

```txt
123
Number of PROCESS vars of type Int: 1
PROCESS vars of type Int add up to 28785
```

Obviously Perl 6 doesn't maintain a lot of global integer variables... <tt>:-)</tt>

Nevertheless, you can use similar code to access all the variables in any package you like,
such as the GLOBAL package, which typically has absolutely nothing in it.  Since the PROCESS package is even more global than GLOBAL, we used that instead.  Go figure...


## Phix

Phix has a version() routine which returns a string such as "0.8.0":

```Phix
?version()
?scanf(version(),"%d.%d.%d")[1]
```

```txt

"0.8.0"
{0,8,0}

```

The scanf() result is probably easier to test against.

Phix has a builtin abs() routine, which will be auto-included if referenced.

```Phix
include pmaths.e -- (needed pre-0.8.1 to work around a compiler bug [oops])
--include complex.e -- (not an auto-include, needed in all versions)
integer r_abs = routine_id("abs")
--integer r_abs = routine_id("complex_abs")
if r_abs!=-1 then
    ?call_func(r_abs,{-42})
end if
```

Using complex_abs() is probably closer to the task requirement in that if complex.e is not included it will not be found/called.

In this case it happens to give exactly the same result, however under the hood it is actually returning sqrt((-42)*(-42)+(0)*(0)).

There is (as yet) no var_id() builtin, the following is a quick cobbling-together of code from builtins\VM\prtnidN.e (routine_id)
and builtins\VM\pDiagN.e (ex.err file creation), not very pretty but it seems to work, and of course all this sort of stuff is
normally hidden away out of sight in builtins\VM.

```Phix
include builtins/VM/pStack.e -- :%opGetST
-- copies from pglobals.e:
constant S_Name  = 1,   -- const/var/rtn name
         S_NTyp  = 2,   -- Const/GVar/TVar/Nspc/Type/Func/Proc
         S_FPno  = 3,   -- File and Path number
         S_Slink = 6,   -- scope/secondary chain (see below)
         S_vtype = 7,   -- variable type or namespace fileno
         S_GVar2 = 2,   -- global or static variable
         T_int   = 1,
         T_EBP   = 22,  -- compiled/listing=0, interpreted={ebp4,esp4,sym4} (set at last possible moment)
         T_ds4   = 23   -- compiled = start of data section, same but /4 when interpreted ([T_EBP]!=0)

function var_id(object s)
-- hacked copy of routine_id(), for local file-level integers only
integer res,            -- symidx for string s, else sum(local gvar integers)
        rtn,            -- routine number of callee, from callstack
        cFno,           -- calling fileno.
        tidx,
        ds4
object symtab,
       si,              -- copy of symtab[i], speedwise
       si_name          -- copy of symtab[i][S_name], speedwise/thread-sfaety

    -- get copy of symtab. NB read only! may contain nuts! (unassigned vars)
    enter_cs()
    #ilASM{
        [32]
            lea edi,[symtab]
            call :%opGetST      -- [edi]=symtab (ie our local:=the real symtab)
            mov edi,[ebp+20]    -- prev_ebp
            mov edi,[edi+8]     -- calling routine no
            mov [rtn],edi
        [64]
            lea rdi,[symtab]
            call :%opGetST      -- [rdi]=symtab (ie our local:=the real symtab)
            mov rdi,[rbp+40]    -- prev_ebp
            mov rdi,[rdi+16]    -- calling routine no
            mov [rtn],rdi
        []
          }
    if symtab[T_EBP]=0 then             -- compiled
        ds4 = floor(symtab[T_ds4]/4)
    else                                -- interpreted
        ds4 = symtab[T_ds4]
    end if
    cFno = symtab[rtn][S_FPno]      -- fileno of callee (whether routine or toplevel)
    res = iff(s=0?0:-1)
    for i=1 to length(symtab) do
        si = symtab[i]
        if sequence(si)
        and si[S_NTyp]=S_GVar2
        and si[S_FPno]=cFno
        and si[S_vtype]=T_int then
            si_name = si[S_Name]
            if s=0 then
                -- cut-down version of pDiagN.e/getGvarValue():
                integer gidx = si[S_Slink], novalue, o
                #ilASM{
                        mov [novalue],0
                    [32]
                        mov esi,[ds4]
                        mov edx,[gidx]
                        shl esi,2
                        mov esi,[esi+edx*4+16] -- ([ds+(gidx+4)*4] == gvar[gidx])
                        cmp esi,h4
                        jl @f
                            mov [novalue],1
                            xor esi,esi
                      @@:
                        mov [o],esi
                    [64]
                        mov rsi,[ds4]
                        mov rdx,[gidx]
                        shl rsi,2
                        mov rsi,[rsi+rdx*8+24] -- ([ds+(gidx+3)*8] == gvar[gidx])
                        mov r15,h4
                        cmp rsi,r15
                        jl @f
                            mov [novalue],1
                            xor rsi,rsi
                      @@:
                        mov [o],rsi
                    []
                      }
                if novalue then
                    ?{si_name,"no_value"}
                else
                    res += o
                end if
            elsif s=si_name then
                res = i
                exit
            end if
        end if
    end for
    si_name = 0
    si = 0
    symtab = 0
    leave_cs()
    return res
end function

{} = routine_id("blurgzmp") -- force symtab name population..
                            -- (alt: see rbldrqd in pDiagN.e)
integer bloop = 5,
--      barf,               -- triggers {"barf","no_value"}
        burp = 35
bloop = 6
burp += 1
?var_id("bloop")        -- >0 === exists
?var_id("blooop")       -- -1 === does not exist
?var_id(0)              -- bloop+burp = 42
?bloop+burp             --     "", doh
```

```txt

1257
-1
42
42

```

As-is, of course, being integer-only-and-no-routine-level-vars, the above represents very limited practical value.

Other routines of interest include

```Phix
?platform()      -- WINDOWS=2, LINUX=3
?machine_bits() -- 32 or 64
?machine_word() -- 4 or 8
?include_paths() -- eg {"C:\\Program Files (x86)\\Phix\\builtins\\",
                 --     "C:\\Program Files (x86)\\Phix\\builtins\\VM\\",
                 --     "C:\\Program Files (x86)\\Phix\\"}
                 --  (plus other application-specific directories)
?get_interpreter() -- eg "C:\Program Files (x86)\Phix\p.exe"
                   -- or perhaps "/home/pete/phix/p" on Linux
```

Phix supports the absolute bare minimum use of #ifdef, for compatibility with OpenEuphoria, however it is almost always better
to use platform() and friends as normal hll code, rather than that sort of language-within-a-language stuff, imnsho, and the
compiler is pretty good at optimising away most such os-specific tests and whole branches of irrelevant code (see EmitON=0).


## PHP



```php
<?php

if (version_compare(PHP_VERSION, '5.3.0', '<' ))
{
	echo("You are using PHP Version " . PHP_VERSION . ". Please upgrade to Version 5.3.0\n");
	exit();
}
$bloop = -3;
if (isset($bloop) && function_exists('abs'))
{
	echo(abs($bloop));
}
echo(count($GLOBALS) . " variables in global scope.\n");
echo(array_sum($GLOBALS) . " is the total of variables in global scope.\n");

?>
```



## PicoLisp


```PicoLisp
(unless (>= (version T) (3 0 1))       # Check version (only in the 64-bit version)
   (bye) )

# (setq bloop -7)                      # Uncomment this to get the output '7'

(and
   (num? bloop)                        # When 'bloop' is bound to a number
   (getd 'abs)                         # and 'abs' defined as a function
   (println (abs bloop)) )             # then print the absolute value
```



## PL/I


### Version 1


```PL/I

   S = SYSVERSION();
   if substr(S, 6, 6) < '050000' then
      do; put skip list ('Version of compiler is too old'); stop; end;

```


### Version 2


```PL/I
*process source attributes options m or(!);
 /*********************************************************************
 * 02-11.2013 Walter Pachl
 * I modified this to run on my Windows PL/I
 *********************************************************************/
 v: Proc Options(main);
 %version: Proc Returns(char);
 Dcl res char;
 res=sysversion;
 Return(''''!!res!!'''');
 %End;
 %Act version;
 Dcl s char(31) Init('');
 s=version;
 if substr(S,15,3) < '7.5' then Do;
   put Skip list('Version of compiler ('!!substr(S,15,3)!!
                                                     ') is too old');
   stop;
   End;
 Else
   Put Skip List('Version is '!!s);
 End
```

```txt
Version is PL/I for Win* 7.5
```



## Pop11

Variable pop_internal_version contains Poplog version in numeric form (as an integer) -- this one is most convenient for version checks. For printing one can use pop_version (which is a string containing more information).


```pop11
;;; Exit if version below 15.00
if pop_internal_version < 150000 then
    sysexit()
endif;
```


Pop11 variables are named by words. Pop11 word is a unique version of string stored in dictionary.  So we need first convert strings to words and then query about words. Pop11 variables can store any value including functions and in fact when one accesses a function like abs by name one merely access a variable abs which happen to hold predefined function abs. To follow spirit of the task as closely as possible we check if abs indeed holds functional value.


```pop11
;;; We do main task in a procedure
define check_and_call(x, y);
   lvars wx=consword(x), wy=consword(y);
   if identprops(wx) = 0 and isprocedure(valof(wx))
      and identprops(wy) = 0 then
          return(valof(wx)(valof(wy)));
   else
        return("failed");
   endif;
enddefine;

;;; Prints failed because bloop is undefined
check_and_call('abs' , 'bloop') =>
;;; Define bloop
vars bloop = -5;
;;; Now prints 5
check_and_call('abs' , 'bloop') =>
```


Note that here bloop is defined as "permanent" variable, Pop11 also have lexical variables which are not available for introspection.


## PowerBASIC


PowerBASIC has no way of determining if a variable "exists" in code. If variable declaration is required (using <code>#DIM ALL</code>), then any attempt to use a variable without declaring it will result in a failed compile; if variable declaration is ''not'' required, then the first time the program accesses the variable it is automatically created with the default data type (which is set using <code>DEFtype</code>).

The compiler directive <code>#COMPILER</code>, introduced with PB/Win 8 and PB/CC <!--uncertain version here--> 4, will fail the compile if the compiler does not match at least one of the listed compilers, and is not at least the (optional) minimum version of that compiler.


```powerbasic
#COMPILER PBWIN 9
#COMPILER PBWIN, PBCC 5
```



## PowerShell


```powershell
# version is found in $PSVersionTable
if ($PSVersionTable['PSVersion'] -lt '2.0') {
    exit
}

if ((Test-Path Variable:bloop) -and ([Math]::Abs)) {
    [Math]::Abs($bloop)
}

# find integer variables and their sum
Get-Variable -Scope global `
    | Where-Object { $_.Value -is [int] } `
    | Measure-Object -Sum Value `
    | Select-Object Count,Sum
```



## PureBasic


```PureBasic
CompilerIf #PB_Compiler_Version<441
  CompilerError "You failed the version check!"
CompilerEndIf

CompilerIf   Defined(bloop,#PB_Variable)
  CompilerIf Defined(Abs(),#PB_Function)
    Abs(bloop)
  CompilerEndIf
CompilerEndIf
```



## Python


```python
# Checking for system version
 import sys
 major, minor, bugfix = sys.version_info[:3]
 if major < 2:
     sys.exit('Python 2 is required')


 def defined(name): # LBYL (Look Before You Leap)
     return name in globals() or name in locals() or name in vars(__builtins__)

 def defined2(name): # EAFP (Easier to Ask Forgiveness than Permission)
     try:
          eval(name)
          return True
     except NameError:
          return False

 if defined('bloop') and defined('abs') and callable(abs):
     print abs(bloop)

 if defined2('bloop') and defined2('abs') and callable(abs):
     print abs(bloop)
```

You can combine both tests, (But loose sight of which variable in missing/not callable by wrapping the whole function call in a try-except statement:

```python
try:
    print abs(bloop)
except (NameError, TypeError):
    print "Something's missing"
```

Here is one way to print the sum of all the global integer variables:

```python
def sum_of_global_int_vars():
    variables = vars(__builtins__).copy()
    variables.update(globals())
    print sum(v for v in variables.itervalues() if type(v) == int)

sum_of_global_int_vars()
```



## R

```R

if(getRversion() < "2.14.1")
{
   warning("Your version of R is older than 2.14.1")
   q()  # exit R, with the option to cancel
}
```

The constants <code>version</code> and <code>R.version</code> give further information about the version that is running.  The function <code>R.Version()</code> provides the same information as a list.

We now perform three checks: we want to know if bloop is in the user workspace (global environment), if abs exists somewhere, and if abs is a function.

```R
bloop <- -3.4
if(exists("bloop", envir=globalenv()) && exists("abs") && is.function(abs))
{
   abs(bloop)
}
```

Finally, we count how many integers are in the user workspace, and find their total.  Note that a number followed by the letter L is considered to be an integer.  See [[Integer_literals#R]] for more information.

```R
#Declare some integers
qqq <- 45L
www <- -3L
#Retrieve the name of all the variables in the user workspace
var_names <- ls()
#Retrieve the actual variables as a list
all_vars <- mget(var_names, globalenv())
#See which ones are integers
is_int <- sapply(all_vars, is.integer)
#Count them
sum(is_int)
#Retrieve the variables that were integers
the_ints <- mget(varnames[is_int], globalenv())
#Add them up
sum(unlist(the_ints))
```



## Racket


The usual hack:

```Racket

#lang racket
(unless (string<=? "5.3" (version)) (error "ancient version"))

```


Proper comparison:

```Racket

(require version/utils)
(unless (version<=? "5.3" (version)) (error "ancient version"))

```



## Raven


```raven
VERSION 0 prefer 20071104 <
if  'version >= 20071104 required' print bye

'bloop' GLOBAL keys in && 'abs' CORE keys in
if  bloop abs print
```




## Retro

This will exit if the version is less than 2019.6:


```Retro
@Version #201906 lt+ &bye if
```


The existence of functions can be checked using '''d:lookup'''. In this, a helper function is provided to improve readability.


```Retro

Checks for existence of "bloop" and "n:abs"

~~~
: executeByName (s-)
  d:lookup [ d:xt fetch ] [ d:class fetch ] bi call ;

'bloop 'n:abs [ find nip ] bi@ and
  [ 'bloop executeByName 'n:abs executeByName ] if
~~~
```


Retro has no direct way to check for data types of functions. Assuming that a word class is defined for integer variables, we could do something like this:


```Retro

#0 #0 [ dup d:class fetch &class:integer eq? [ d:xt fetch + [ n:inc ] dip ] [ drop ] choose ] d:for-each

```


After execution the stack will have the number of variables found, and the accumulated sum of their values.


## REXX

Test to see if the version is at least version 4.

```rexx
                                /*output from parse version (almost all REXX versions)  */
                                /*      theREXXinterpreterName level mm Mon yyyy        */
parse version . level .
if level<4  then exit
```

Test to see if the version is at least version 4, another example.

```rexx
parse version  x  1  whatLang  level  dd  mon  yyyy  .
if level<4  then do
                 say
                 say 'version' level "is too old!"
                 say x                                  /*this displays everything.*/
                 exit                                   /*or maybe:   EXIT 13      */
                 end
```

Test to see if the REXX variable "bloop" exists, version 1.

```rexx
if symbol('bloop')=='VAR' then say 'the "bloop" variable exists.'
```

Test to see if the REXX variable "bloop" exists, version 2.

```rexx
if symbol('bloop')=='VAR'  then say 'the "bloop" variable exists.'
                           else say 'the "bloop" variable doesn''t exist.'
```

Programming note:   note the use of the double apostrophe   (<big>''' ' ' '''</big>)   which is within a quoted string (with apostrophes)   [in the above and below REXX programming examples].


Another test to see if the REXX variable "bloop" exists.

```rexx
bloop=47
if symbol('bloop')=='VAR'  then say 'the "bloop" variable exists.'
                           else say 'the "bloop" variable doesn''t exist.'
```

In REXX, the ABS function is a built-in function (BIF).

```rexx
bloop=47
g=abs(bloop)
```

However, most REXX interpreters will allow this type of test:

```rexx
if testxyz()  then say 'function XYZ not found.'
              else say 'function XYZ was found.'
exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
testxyz: signal on syntax
         call XYZ
         return 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
syntax:  return 1
```



## Ring


```ring

# Project: Introspection

if version() < 1.8
   see "Version is too old" + " (" + version() + ")" + nl
else
   see "Version is uptodate" + " (" + version() + ")" + nl
ok
bloop = 5
if isglobal("bloop") = 1
   see "Variable " + "'bloop'" + " exists" + nl
else
   see "Variable " + "'bloop'" + " doesn't exist" + nl
ok
if isglobal("bleep") = 1
   see "Variable " + "'bleep'" + " exists" + nl
else
   see "Variable " + "'bleep'" + " doesn't exist" + nl
ok
if isfunction("abs") = 1
   see "Function " + "'abs'" + " is defined" + nl
else
   see "Function " + "'abs'" + " is not defined" + nl
ok
if isfunction("abc") = 1
   see "Function " + "'abc'" + " is defined" + nl
else
   see "Function " + "'abc'" + " is not defined" + nl
ok

func abs(bloop)
        return fabs(bloop)

```

Output:

```txt

Version is uptodate (1.8)
Variable 'bloop' exists
Variable 'bleep' doesn't exist
Function 'abs' is defined
Function 'abc' is not defined

```



## Ruby


```ruby
exit if RUBY_VERSION < '1.8.6'
puts bloop.abs if defined?(bloop) and bloop.respond_to?(:abs)
```


'''Extra credit:'''

```ruby
def variable_counter(b)
  int_vars = []
  sum = 0
  check_var = lambda do |name, value|
    if value.is_a?(Integer)
      int_vars << name
      sum += value
    end
  end

  global_variables.each {|varname| check_var.call(varname, eval(varname.to_s))}
  eval('local_variables', b).each {|varname| check_var.call(varname, eval(varname.to_s, b))}

  puts "these #{int_vars.length} variables in the global scope are integers:"
  puts int_vars.inspect
  puts "their sum is: #{sum}"
end

an_int = 5
a_string = 'foo'
a_float = 3.14

variable_counter(binding)
```


```txt

(eval):1: warning: variable $= is no longer effective
(eval):1: warning: variable $KCODE is no longer effective
(eval):1: warning: variable $KCODE is no longer effective
these 5 variables in the global scope are integers:
[:$SAFE, :$., :$$, :$-W, :an_int]
their sum is: 18630

```

Note: The warning is because it accessed the global variable which was made invalid.

The meaning of these variables can be found many places, including [http://en.wikibooks.org/wiki/Ruby_Programming/Syntax/Variables_and_Constants here].


## Scala


```scala
object VersCheck extends App {
  val minimalVersion = 1.7

  val vers = System.getProperty("java.specification.version");
  println(if (vers.toDouble >= minimalVersion) "YAY!" else s"Must use Java >= $minimalVersion");

  val bloop = Option(-42)
  if (bloop.isDefined) bloop.get.abs
}
```


## Slate

No version string included inside the system presently.

```slate
Platform run: StartupArguments first ; ' --version'.
```



```slate
(lobby hasSlotNamed: #bloop) /\ [(#abs findOn: {lobby bloop}) isNotNil] ifTrue: [inform: bloop abs printString].
lobby slotValues inject: 0 into: [| :sum :value | (value is: Integer) ifTrue: [sum + value] ifFalse: [sum]].
```



## Smalltalk

```smalltalk
| s v t sum hm |
"uncomment the following to see what happens if bloop exists"
"Smalltalk at: #bloop put: -10."
s := Smalltalk version.
(s =~ '(\d+)\.(\d+)\.(\d+)')
  ifMatched: [:match |
       v := (( (match at: 1) asInteger ) * 100) +
            (( (match at: 2) asInteger ) * 10) +
            ( (match at: 3) asInteger )
  ].
( v < 300 )
  ifTrue: [
     Transcript show: 'I need version 3.0.0 or later' ; cr ]
  ifFalse: [
     Transcript show: 'Ok! I can run!' ; cr .
     "does bloop exists as global var?"
     t := Smalltalk at: #bloop
            ifAbsent: [
                 Transcript show: 'bloop var does not exist as global!' ; cr .
                 ^nil
            ].
     (t respondsTo: #abs)
          ifTrue:
             [ Transcript show: 'Absolute value of bloop: ' ;
                     show: (t abs) printString ; cr ].
  ] .

"how many 'numbers' in global scope, and compute their sums"
hm := 0.
sum := 0.
(Smalltalk keys) do: [ :els |
        ( (Smalltalk at: els) isKindOf: Number )
          ifTrue: [ hm := hm + 1.
                    sum := sum + (Smalltalk at: els).
                    "Transcript show: (els asString) ; cr" ]
    ] .
Transcript show: 'Num of global numeric vars: '; show: (hm printString); cr ;
           show: 'Sum of global numeric vars: '; show: (sum printString) ; cr.
```



## Tcl


```tcl
package require Tcl 8.4 ; # throws an error if older
if {[info exists bloop] && [llength [info functions abs]]} {
    puts [expr abs($bloop)]
}
```


'''Extra credit:'''

```tcl
namespace eval ::extra_credit {
    variable sum_global_int 0
    variable n_global_int 0
    foreach var [info vars ::*] {
        if {[array exists $var]} continue
        if {[string is int -strict [set $var]]} {
            puts "$var = [set $var]"
            incr sum_global_int [set $var]
            incr n_global_int
        }
    }
    puts "number of global ints = $n_global_int"
    puts "their sum = $sum_global_int"
}
```


=={{header|TI-89 BASIC}}==


```ti89b
()
Prgm
  Local l, i, vers
  getConfg() → l
  For i,1,dim(l),2
    If l[i] = "OS Version" or l[i] = "Version" Then
      l[i + 1] → vers
      Disp "Version: " & vers
      If expr(right(vers, 4)) < 2005 Then  © Lousy parsing strategy
        Disp vers & " is too old"
        Stop
      EndIf
    EndIf
  EndFor

  If isVar(bloop) Then        © Dynamic name check can be done with isVar(#aString)
    © Builtin functions cannot be tested for.
    Disp abs(bloop)
  Else
    Disp "No bloop"
  EndIf

  © There is no way to get a list of global variables.
EndPrgm
```



## Toka

Starting with Release 1.1, Toka allows for checking the version number:


```toka
VERSION 101>
 [ bye ] ifFalse
```


Release 1.0 can be detected by doing:


```toka
` VERSION FALSE = [ bye ] ifTrue
```


Basic introspection is possible via '''`'''


```toka
` bloop FALSE <> ` abs FALSE <> and [ ` bloop invoke @ ` abs invoke ] ifTrue
```



## UNIX Shell

There's no way to introspect the builtin arithmetic functions. We'll just try it and see if there's an error.

```bash
case ${.sh.version} in
    *93[[:alpha:]]+*) :;; #this appears to be ksh93, we're OK
    *)  echo "version appears to be too old"
        exit              # otherwise, bail out
        ;;
esac

if [[ -z ${bloop+bloop has a value} ]]; then
    print "no bloop variable"
elsif (( abs(1) )); then
    print -- $(( abs(bloop) ))
fi

typeset -a int_vars
set | while IFS='=' read -r var value; do
    if [[ $value == +([[:digit:]]) ]]; then
        int_vars[n++]=$var
        let sum += $value
    fi
done
print "${int_vars[*]}"
print -- $sum
```


bash does not have a builtin math function "abs" -- we'll check for a user-defined shell function instead.

```bash
if [[ $BASH_VERSION < "4.2" ]]; then
    echo "version is too old"
    exit
fi

if [[ ! -v bloop ]]; then
    echo "no bloop variable"
elif [[ $(type -t abs 2>/dev/null) != function ]]; then
    echo "abs is not a shell function"
else
    echo $(abs $bloop)
fi

# need to populate the variables and use them within the same subshell in a pipeline.
set | {
    shopt -s extglob
    int_vars=()
    while IFS='=' read -r var value; do
        if [[ $value == +([[:digit:]]) ]]; then
            int_vars+=($var)
            (( sum += value ))
        fi
    done
    echo "${int_vars[*]}"
    echo $sum
}
```



## Ursala

When the user defined function maybe_abs(x) is evaluated, a run time check is performed for
the availability of the system library's absolute value function
(fabs), and if found, it is used. If not, the function tries to
invoke a replacement function named 'abs' from the imported floating
point function library, flo. (The search for a replacement is performed at the time
maybe_abs is compiled, and is possible because any
imported library is presented as a list of (identifier:value) pairs.)
If no imported absolute value function is available either,
maybe_abs returns a copy of the argument x unchanged.

The #preprocess directive allows arbitrary user defined
transformations to be applied to the abstract syntax trees created by
the compiler (assuming a knowledge of the compiler internals).  This
example of a preprocessor aborts compilation if the run time system version differs
from 0.10.2. Otherwise, a search is performed for any declared symbol
named 'bloop' whose evaluation has been resolved, and its value is
replaced with the absolute value as given by maybe_abs.

On platforms where at least one of the absolute value functions is
available, the -1. shown in the source text will have been changed to
1 by the time it's displayed.


```Ursala
#import std
#import flo
#import lag

maybe_abs = math.|fabs (%QI flo)-:~&! 'abs'

#preprocess version==-[0.10.2]-?\<'wrong version'>!% *^0 ||~& -&
   -&~&vitB,~&d.lexeme=='=',~&vhd.lexeme=='bloop',~&vthd.lexeme=='(evaluated)'&-,
   &vthd.semantics:= !+ !+ maybe_abs+ ~&vthd.semantics.&iNHiNH&-

#cast %e

bloop = -1.
```

```txt
1.000000e+00
```



## VBA


Under VBA (and most, if not all, variations of Visual Basic), accessing an undeclared variable will automatically create it as a Variant, unless declaration is required (via <code>Option Explicit</code>), in which case a compile-time error is generated and the compile fails (or in the case of interpreted varieties, the code will not run).

To determine the version of the environment -- typically meaning which version of Microsoft Office is running -- the <code>Application</code> object has a <code>Version</code> property:


```vb
If Application.Version < 15 Then Exit Sub
```



## Yabasic


```Yabasic
if peek("version") < 2.63 error "Interpreter version is too old!"
```



## zkl


```zkl
Language.version //-->L(1,12,8,"2014-04-01")
if (Language.version[1] < 10) System.exit("Too old");
var bloop=-123;
if ((1).resolve("abs",1) and resolve("bloop",8)) bloop.abs().println()
```

```txt
123
```

The concept of global vars doesn't really exist, only instance data.
<lang>var n=3, x=1.0, a=5, z="zoo";
self.vars; --> L(L("a",5),L("n",3),L("x",1),L("z","zoo"))
sum:=self.vars.reduce(fcn(p,[(nm,v)],r){
   if((1).isType(v)){r.inc();p+v;} else p},0,num:=Ref(0));
println("Num int vars = ",num.value,". Sum = ",sum);
```

```txt
Num int vars = 2. Sum = 8
```



## ZX Spectrum Basic



```zxbasic
10 LET totram=PEEK 23732 + 256 * PEEK 23733: REM Check that we have a 48k machine
20 IF totram < 65535 THEN PRINT "Your 16k Spectrum is too old": STOP
30 REM variables must exist before they are used, otherwise we get an error
40 REM we can poke a new error handler and check for variable not found.
50 REM I haven't implemented this, because I have forgotten the handler address
60 LET bloob = -4: REM make sure bloob exists, by creating it.
70 PRINT ABS(bloob): REM function will be present, ZX Spectrum Basic is standardized.
```



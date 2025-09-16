+++
title = "Naming conventions"
description = ""
date = 2019-09-08T13:27:34Z
aliases = []
[extra]
id = 19588
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
languages = [
  "360_assembly",
  "algol_68",
  "antlang",
  "awk",
  "basic",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "csharp",
  "delphi",
  "dyalect",
  "factor",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "go",
  "haskell",
  "j",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "oasys_assembler",
  "oforth",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "tcl",
  "visual_basic",
  "zkl",
]
+++

## Task

{{task|Programming environment operations}} [[Category:Simple]]
Many languages have naming conventions regarding the identifiers used in the language, its libraries, and programs written in the language. Such conventions, which may be classified as ''de facto'' or ''de jure'' depending on how they are enforced,
often take the form of rules regarding prefixes, suffixes, and the use of upper-case and lower-case characters.

The naming conventions are sometimes a bit haphazard, especially if the language and/or library has gone through periods of evolution. (In this case: give a brief example and description.)

Document (with simple examples where possible) the evolution and current status of these naming conventions.
For example, name conventions for:
* Procedure and operator names.  (Intrinsic or external)
* Class, Subclass and instance names.
* Built-in versus libraries names.



If possible, indicate where the naming conventions are implicit, explicit, mandatory or discretionary.
Any tools that enforced the the naming conventions.
Any cases where the naming convention as commonly violated.

If possible, indicate where the convention is used to hint at other issues. For example the C standard library uses a prefix of "_" to "hide" raw Operating System calls from the non systems-programmer, whereas Python embeds member functions in between "__" to make a member function "private".

## See also

* Wikipedia: [[wp:Naming convention (programming)|Naming convention (programming)]]





## 360 Assembly

<!-- Naming conventions -->
No real naming conventions for S/360 Assembler except the stric rule for names (called symbols).

A symbol may contain from one to six characters; the characters may be any combination
of alphabetic (A through Z) and numerical (O through 9) characters.
The first character must be alphabetic.
Special characters and embedded blanks must not be used in symbols.


## ALGOL 68

;In the Formal Specification
The revised report used "shorthand" to indicate an MODE was "private" to the language specification.  The character ℒ was used to indicate that the name could be repeated  for every precision... e.g. ℒ INT could mean: ... SHORT SHORT INT, SHORT INT, INT, LONG INT, LONG LONG INT etc and ℓ cos could mean: short short cos, short cos, cos, long cos, long long cos etc.

```algol68
MODE ℵ SIMPLEOUT = UNION (≮ℒ INT≯, ≮ℒ REAL≯, ≮ℒ COMPL≯, BOOL, ≮ℒ BITS≯, CHAR, [ ] CHAR);
PROC ℓ cos = (ℒ REAL x) ℒ REAL: ¢ a ℒ real value close to the cosine of 'x' ¢;

PROC ℓ complex cos = (ℒ COMPL z) ℒ COMPL: ¢ a ℒ complex value close to the cosine of 'z' ¢;

PROC ℓ arccos = (ℒ REAL x) ℒ REAL: ¢ if ABS x ≤ ℒ 1, a ℒ real value close
      to the inverse cosine of 'x', ℒ 0 ≤ ℒ arccos (x) ≤ ℒ pi ¢;
```

For LONG LONG MODEs this would be coded as:

```algol68
PROC long long cos = (LONG LONG REAL x) LONG LONG REAL: ¢ a ℒ real value close to the cosine of 'x' ¢;

PROC long long complex cos = (LONG LONG COMPL z) LONG LONG COMPL: ¢ a ℒ complex value close to the cosine of 'z' ¢;

PROC long long arccos = (LONG LONG REAL x) LONG LONG REAL: ¢ if ABS x ≤ ℒ 1, a ℒ real value close
      to the inverse cosine of 'x', ℒ 0 ≤ ℒ arccos (x) ≤ ℒ pi ¢;
```

Note: The type returned by the '''proc'''edure is generally prefixed to the '''proc'''edure name.

;Standard language
Because Algol68 was required on 6-bit and 7-bit, but could take advantage of wide character sets the naming convention could be mechanically varied across platforms.  In a 7-bit environment reserved words, '''mode'''s and '''op'''erators were typically upper-case.  Constants, variable and '''proc'''edure names were typically lower-case.

The more peculiar convention was for '''reserved words''', '''mode'''s and '''op'''erators was for these to appear in code as '''bold''' typeface or even <u>underlined</u> when published.

For example:
{|border="1" style="border-collapse: collapse; border: 5px double grey;"  align="center"
|-valign="top"
|| Algol68 "strict"
 as typically published
 ¢ underline or
   bold typeface ¢
 '''mode''' '''xint''' = '''int''';
 '''xint''' sum sq:=0;
 '''for''' i '''while'''
   sum sq≠70×70
 '''do'''
   sum sq+:=i↑2
 '''od'''
|| Quote stropping<br/>(like [[wp:Lightweight markup language#Text/font-face formatting|wikitext]])

```algol68

'pr' quote 'pr'
'mode' 'xint' = 'int';
'xint' sum sq:=0;
'for' i 'while'
  sum sq≠70×70
'do'
  sum sq+:=i↑2
'od'

```

|| For a [[wp:List of binary codes#Seven-bit binary codes|7-bit]] character code compiler

```algol68

.PR UPPER .PR
MODE XINT = INT;
XINT sum sq:=0;
FOR i WHILE
  sum sq/=70*70
DO
  sum sq+:=i**2
OD

```

|| For a [[wp:Six-bit character code|6-bit]] character code compiler

```algol68

.PR POINT .PR
.MODE .XINT = .INT;
.XINT SUM SQ:=0;
.FOR I .WHILE
  SUM SQ .NE 70*70
.DO
  SUM SQ .PLUSAB I .UP 2
.OD

```

|| Algol68 using '''res''' stropping<br/>(reserved word)

```algol68

.PR RES .PR
mode .xint = int;
.xint sum sq:=0;
for i while
  sum sq≠70×70
do
  sum sq+:=i↑2
od

```

|}
Note that spaces are permitted in constants, variable and '''proc'''edure names.

Various other prefixes and suffixes (grouped by type function) can be found in the standard prelude:
{|border="1" style="border-collapse: collapse; border: 5px double grey;"  align="center"
|-valign="top"
|| To query '''file''' capabilities
|| standard '''file''' and '''channel'''s
|| file '''proc'''edures
|| Exception handling '''proc'''edures
|| Implementation specific precisions
|| '''mode''' limits and sizes
|| special '''char''acters
|-
||
*get possible
*put possible
*bin possible
*reset possible
*set possible
*reidf possible
||
*stand in
*stand out
*stand back
*stand in channel
*stand out channel
*stand back channel
||
* print, write, put, read, get
* printf, writef, putf, readf, getf
* print bin, put bin, read bin, get bin
* print ℓ int, put ℓ int, read ℓ int, get ℓ int
* print ℓ real, put ℓ real, read ℓ real, get ℓ real
* etc
||
* on logical file end
* on physical file end
* on line end
* on page end
* on format end
* on value error
* on open error
* on transput error
* on format error
||
* int lengths
* int shorths
* real lengths
* real shorths
* bits lengths
* bits shorths
* bytes lengths
* bytes shorths
||
* ℓ bits width
* ℓ bytes width
* ℓ int width
* ℓ real width
* ℓ exp width
* ℓ max int
* ℓ max real
* ℓ small real
||
*error char
*exp char
*formfeed char
*newline char
*null character
*tab char
|}


## AntLang


```AntLang
variables-are-often-lower-case-and-seperated-like-this-one
```



## AWK


```AWK

# Field names begin with $ so $1 is the first field, $2 the second and $NF the
# last.  $0 references the entire input record.
#
# Function and variable names are case sensitive and begin with an alphabetic
# character or underscore followed by any number of: a-z, A-Z, 0-9, _
#
# The awk language is type less; variables are either string or number
# depending upon usage.  Variables can be coerced to string by concatenating ""
# or to number by adding zero.  For example:
#   str = x ""
#   num = x + 0
#
# Below are the names of the built-in functions, built-in variables and other
# reserved words in the awk language separated into categories.  Also shown are
# the names of gawk's enhancements.
#
# patterns:
#   BEGIN END
#   BEGINFILE ENDFILE (gawk)
# actions:
#   break continue delete do else exit for if in next return while
#   case default switch (gawk)
# arithmetic functions:
#   atan2 cos exp int log rand sin sqrt srand
# bit manipulation functions:
#   and compl lshift or rshift xor (gawk)
# i18n functions:
#   bindtextdomain dcgettext dcngettext (gawk)
# string functions:
#   gsub index length match split sprintf sub substr tolower toupper
#   asort asorti gensub patsplit strtonum (gawk)
# time functions:
#   mktime strftime systime (gawk)
# miscellaneous functions:
#   isarray (gawk)
# variables:
#   ARGC ARGV CONVFMT ENVIRON FILENAME FNR FS NF NR OFMT OFS ORS RLENGTH RS RSTART SUBSEP
#   ARGIND BINMODE ERRNO FIELDWIDTHS FPAT FUNCTAB IGNORECASE LINT PREC PROCINFO ROUNDMODE RT SYMTAB TEXTDOMAIN (gawk)
# function definition:
#   func function
# input-output:
#   close fflush getline nextfile print printf system
# pre-processor directives:
#   @include @load (gawk)
# special files:
#   /dev/stdin /dev/stdout /dev/error

```



## BASIC

BASIC is case-insensitive, although keywords are generally written entirely in uppercase.

A variable or function can have a suffix to indicate the type (which types are available depending on what implementation is in use): <tt>!</tt> for single-precision, <tt>@</tt> for fixed-point, <tt>#</tt> for double-precision, <tt>$</tt> for strings, <tt>%</tt> for short integers, <tt>&</tt> for long integers.

It is also possible to use DEFtype commands to make the type of the variable to be based on what the first letter is (similar to FORTRAN). The default for Microsoft BASIC is: <tt>DEFSNG A-Z</tt>


## BBC BASIC

Commands and keywords have to be entered in upper case. Variable names are case-sensitive: <tt>FOO</tt>, <tt>Foo</tt>, and <tt>foo</tt> are all different variables. Further, the same name can be used with different suffixes to refer to variables of different types: <tt>foo</tt> is a float, <tt>foo%</tt> is an integer, <tt>foo$</tt> is a string, <tt>foo()</tt> is an array of floats, etc. There is nothing to prevent all these names being used in the same program.

The names of user-defined functions (which return exactly one value) and procedures (which may have no return value, or one, or several) must begin with <tt>FN</tt> or <tt>PROC</tt>, respectively. Many users find it convenient to follow this prefix with an underscore—so a procedure that takes a float, an array of strings, and an integer and then returns two integers might be defined as follows:

```bbcbasic
DEF PROC_foo(bar, baz$(), quux%, RETURN fred%, RETURN jim%)
```

Names like <tt>PROCfoo</tt> and <tt>FNbar</tt> are sometimes used, and even <tt>PROCFOO</tt> and <tt>FNBAR</tt> are entirely legal; but they are probably less readable.

TitleCase and camelCase are not much used in BBC BASIC, perhaps not used at all; <tt>lower_case_with_underscores</tt> is preferred for long names. In general, using lower case for user-defined names helps maintain a visual contrast with reserved words and the names of system variables.

The twenty-six integer variables <tt>A%</tt> to <tt>Z%</tt> (capitalized) are 'static': that is to say, they persist throughout an interpreter session and are unaffected by the commands <tt>NEW</tt> and <tt>CLEAR</tt>. They can thus be used to pass a small amount of data from one program to another.

If the first line of the program is a comment line of the form <tt>REM >myprog</tt>, the <tt>SAVE</tt> command can be used with no filename and the program will be saved as (in this case) <tt>myprog</tt>. Otherwise, it would be necessary to use <tt>SAVE "myprog"</tt>.


## C

;Base language
* All reserved words and operators are lower-case. e.g. while, for, if, sizeof and return etc.

;Libraries
Constants that appear in C "header" files are typically in upper-case:
```c
O_RDONLY, O_WRONLY, or O_RDWR. O_CREAT, O_EXCL, O_NOCTTY, and O_TRUNC
```

Note also that there are remnants of some historic naming conventions in C where constants were required to be 8 characters or less.  The "O_CREAT" constant is an example.

Types are often suffixed with a "_t", e.g. size_t, and "private" types and arguments are prefixed with "__":

```c
extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
                      size_t __n, FILE *__restrict __s) __wur;
```

However there are some instances where types use all upper-case.  The classic is the type FILE.

In C, the standard library for floating point is focused on double precision, hence the function "cos" is for double precision, and a suffix of "f" and "l" indicate single precision and quad precision respectively.

```c
#include <math.h>

double cos(double x);
float cosf(float x);
long double cosl(long double x);
```


Whereas for complex variable a prefix of "c" is added.

```c
#include <complex.h>

double complex ccos(double complex z);
float complex ccosf(float complex z);
long double complex ccosl(long double complex z);
```


This prefix/suffix convention extends to other standard c library function, for example in the following the "f" suffix indicates that an argument is a format string, the prefixes of "s", "v" and "n" hint at other argument types:

```c
#include <stdio.h>

int printf(const char *format, ...);
int fprintf(FILE *stream, const char *format, ...);
int sprintf(char *str, const char *format, ...);
int snprintf(char *str, size_t size, const char *format, ...);

#include <stdarg.h>

int vprintf(const char *format, va_list ap);
int vfprintf(FILE *stream, const char *format, va_list ap);
int vsprintf(char *str, const char *format, va_list ap);
int vsnprintf(char *str, size_t size, const char *format, va_list ap);
```


;Quirks
The Unix C standard library uses a prefix of "_" to "hide" raw Operating System calls from the non systems-programmer


## C#

'''Capitalization'''<br/>
- All keywords are lowercase (mandatory)<br/>
- Names of parameters and (private) fields should be camelCase.<br/>
- Names of anything else should be PascalCase.

Possible exception are methods that are automatically generated by a tool.<br/>
For example, the Visual Studio Gui designer will generate names of event handlers based on the variable name: submitButton_Clicked

'''Naming'''<br/>
- Names of enums should be plural if it's a flags enum, otherwise it should be singular.

```c#
public enum Planet {
    Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune
}

[Flags]
public enum Days {
    None = 0,
    Sunday = 1,
    Monday = 2,
    Tuesday = 4,
    Wednesday = 8,
    Thursday = 16,
    Friday = 32,
    Saturday = 64,
    Workdays = Monday | Tuesday | Wednesday | Thursday | Friday
    AllWeek = Sunday | Saturday | Workdays
}
```

You should:<br/>
- prefix interface names with I e.g. IPrinter
- prefix generic type parameters with T e.g.
```c#
Dictionary<TKey, TValue>
```

  most of the time, a single T is sufficient e.g.
```csharp
IPrinter<T>
```

- postfix type names that inherit from EventArgs, Exception, Attribute and EventHandler e.g. MouseMoveEventArgs<br/>
- postfix async method names with Async e.g. GetDataAsync()

You should NOT use:<br/>
- underscores, hyphens or any other nonalphanumeric characters<br/>
- hungarian notation


## Common Lisp


* The language's names are first-class Symbols. (ie: a variable is the value-binding given to a Symbol; a named function is a function object that is bound as the function-value of a symbol; etc.) As a Lisp-2, the various bindings of a symbol (value, function, class name, package, etc.) occupy their own namespaces; eg, the symbol named <code>LIST</code> can refer at once to the type LIST, the function LIST (to create a list), and is often used as a variable containing some particular list.

* Symbol names can contain any characters in Unicode, but for most common purposes it's traditional to limit the characters to those that can be entered without quoting (with “|”) the symbol. Usually, names are restricted to <code>a…z 0…9 - / + = < > . ! $ % & ? * ~</code> — although only <code>a-z 0-9 -</code> are often used.

* Using abbreviations is generally frowned-upon, at least in public (exported) symbols, unless they're quite common ones (eg: HTTP)

* Functions and variable names are usually entered in source code as lowercase-with-hyphens phrases.

** Depending on the environment, these are usually interned as symbols with all-upper-case names. Unquoted symbol-names are case-insensitive: <code>list</code>, <code>List</code>, and <code>LIST</code> (or <code>liST</code>, etc) are identical, but <code>|list|</code> and <code>|LIST|</code> are different from one another. In most environments, <code>list</code> and <code>List</code> are interned the name as <code>|LIST|</code> — ie, the name of the Symbol object is <code>"LIST"</code>. (Allegro's CL “modern” environment is case-sensitive and case-preserving, however.)

* Functions to construct an object or structure of some kind are usually named “<code>make-</code>thing,” eg, the standard library function <code>Make-Array</code>.

* Functions that have a side-effect are usually named in a “verb-phrase” form; eg, <code>Format</code> or <code>Delete-File</code>

* Functions which alter a generalized “Place” using the SetF feature (eg: object slots, structure slots, and other things) usually have an “F” suffix. (Historically, the things defined in the standard as “Places” were called “Fields.”) eg: <code>SetF</code> itself, <code>IncF</code>, etc.

* Reader (Accessor) functions that return an attribute of an object are usually a noun-like phrase; eg, the function “<code>Symbol-Name</code>” takes a symbol object and returns its name as a string: <code>(Symbol-Name 'List) ⇒ "LIST"</code> Those that act only upon a certain class may prefix the field name, with the class name; eg, “Char-Code” returns the Unicode value for a character (like reading the “code” field from a Character-class object). With the exception of the built-in class <code>Character</code> (abbreviated Char in function names in the standard library), the class name is normally spelled-out in full. While the hash-table accessor is called <code>GetHash</code> due to historical reasons, accessors generally do <i>not</i> have a “Get-” prefix.

** The SetF-accessors for altering a place are usually named identically to the reader functions; eg, a reader function named <code>Person-Surname</code> would be matched to a writer function named <code>(SetF Person-Surname)</code>. This is what is created automatically when using the <code>DefClass</code> macro's <code>:Accessor</code> option on object slots, or when defining slots in <code>DefStruct</code>.

** Writer functions generally are only defined in terms of SetF writers, and almost never as separately-named functions.

* Predicate functions (returning a generalized Boolean) are named for what they would return “true” for, with a “-P” suffix. For example, <code>Wild-Pathname-P</code> returns true if passed a pathname object with wildcards. If the predicate is a single word (no hyphens), then the “-P” is just “P” — eg, <code>StringP</code>. Predicate functions usually do <i>not</i> have a “is-” prefix, like they might in a language like Java.

* Comparator functions for number objects are <code>< <= = >= > /= </code> — for other types, one would usually prefix the operator symbols with the name of the type; eg, <code>Char= String< Timestamp/=</code>. Note that equality is usually <code>=</code> (not <code>==</code>) and not-equal is written <code>/=</code>.

* Functions or variable names with “<code>%</code>” at the end are usually meant as “internal, unstable” functions. This is distinct from whether the symbol is made external to the package — although it would be unlikely you would export a function with a <code>%</code>-name.

* Global/dynamic variables are named with “earmuffs,” ie, leading and trailing asterisks — eg, <code>*Standard-Output*</code>, <code>*Query-IO*</code>. Failure to do so will elicit compiler warnings in some compilers.

* Constants are usually defined with leading and trailing + signs — eg <code>+Some-Constant+</code> — a convention that was <i>not</i> followed in the <code>Common-Lisp</code> package itself (eg: <code>MOST-POSITIVE-FIXNUM</code>)

* Macros that define a new type of thing are usually named <code>Define-</code>some-thing; if the name of the thing being defined is a single, non-hyphenated word, they may be just <code>Def</code>Thing. eg: <code>DefClass</code>, <code>Define-Condition</code>, or <code>Define-SetF-Expander</code>. The first argument is usually the name of the thing to be defined (as a symbol).

* Functions that signal conditions (eg, errors) alike to the standard <code>Assert</code> or <code>Check-Type</code> macros usually are named <code>Assert-</code>some-condition or <code>Check-</code>some-condition. In Emacs, these patterns will highlight the function name, as well. Likewise, functions that signal warnings may be named <code>Warn-</code>whatever.

* Type names for errors or warnings usually explicitly have the word “warning” or “error” in the name, eg, <code>Server-Unreachable-Error</code>

* Macros that establish a dynamic state and clean up after it are usually named <code>With-</code>some-context; eg, <code>With-Open-File</code>, <code>With-Database-Connection</code>. Usually, such a macro will take a first argument as a list like a normal function call, followed by <code>&body</code>, mimicking <code>With-Open-File</code> and the like.
```lisp

(defvar *language* :en
  "The language to use for messages (defaults to English)")

(defmacro with-language ((language) &body body)
  "Locally binds *LANGUAGE* to LANGUAGE"
  `(let ((*language* ,language))
     ,@body))

(defgeneric complain% (language about)
  ;; The % indicates this is an “internal” implementation detail.
  (:method ((language (eql :en)) (about (eql :weather)))
     "It's too cold in winter"))

(defun complain (about)
  "Complain about something indicated by ABOUT"
  (princ (complain% *language* about) *error-output*)
  (terpri *error-output*)
  (finish-output *error-output*)
  nil)

(defmethod complain% ((language (eql :es)) (about (eql :weather)))
  "Hace demasiado frío en invierno")


(complain :weather)
(with-language (:es)
  (complain :weather))
(complain :weather)

⇓

It's too cold in winter
Hace demasiado frío en invierno
It's too cold in winter

```


* Function names ending in a <code>*</code> are minor variants of the same-named function without the star; eg, the standard <code>Let</code> and <code>Let*</code> special forms.

* Keywords for keyword arguments tend to follow the names used in standard library functions when possible, or mimic the patterns of them. “Private” arguments that callers probably won't need/want to set are usually given non-keyword symbols as their names; eg
```lisp

(defun do-something (argument &key ((secret-arg secret) "default"))
   (format t "Argument is ~a, secret is ~a" argument secret))

;; Normal caller
(do-something "Foo")
;; Special caller:
(do-something "Foo" 'secret-arg "Bar")

```


* Functions which operate on a subset selected by a predicate function usually have names ending in <code>-If</code> or <code>-If-Not</code>; eg, <code>Remove-If-Not</code>

* When a System contains multiple Packages, and there is one “main” package with which users of the System would interact, the main package usually has the same name as the System; and the others often have names that indicate the system name and a “/”. For example, the system <code>UIOP</code> has a package <code>UIOP</code>, as well as packages like <code>UIOP/Filesystem</code> and <code>UIOP/Stream</code>.

* Package names may follow the Java standard of reversed-top-level-domain prefixes; eg, <code>com.example.product.package</code>. Some tools, eg REPLs, may “hide” everything before the last “.” in the package name.

* Functions that may coërce something into a different type are often named <code>Ensure-</code>Type-Name. eg: <code>(defun ensure-string (object) (princ-to-string object))</code> It's typical to make these a function that can accept any (reasonable) type, or a generic function that can specialize on various types.

** Coërcion or conversion functions that explicitly change something from type “a” to type “b” are usually named one of these patterns: <code>a->b</code>, <code>b<-a</code>, <code>b-from-a</code>, or <code>a-b</code>. The last form, with just a hyphen, mimics the standard functions <code>code-char</code> and <code>char-code</code>, but is a bit more ambiguous.

* The name <code>_</code> (and sometimes names like <code>__</code> or <code>_2</code> are sometimes used to indicate ignored values for which a more meaningful name isn't available; for example, skipping an always-blank field in input records. There's nothing “magical” about the name, though; you still need to <code>(declare (ignore _))</code>, so using a more meaningful name is usually preferred. <code>_</code> is most often used for <code>&rest</code> arguments.
```lisp

#+sbcl
(defun user-name+home-phone (user-id)
  "Returns the “real name” and “home phone” for user with ID USER-ID as multiple values.
Reads the GECOS field. SBCL+Linux-specific, probably."
  (destructuring-bind (real-name office office-phone home-phone &rest _)
       (uiop:split-string (sb-posix:passwd-gecos (sb-posix:getpwuid user-id))
                          :separator ",")
     (declare (ignore office office-phone _))
     (values real-name home-phone)))

```


* Argument names are usually self-documenting; since most IDE's will show the argument names in some way (eg: Emacs shows them in the mode line after a function name is entered), using a name like <code>message</code> rather than <code>m</code> (for example) will make the usage clearer. Functions with more than 2 arguments will usually have many/most arguments as keyword arguments.

* Some programmers will use a trailing <code>$</code> to indicate a stringified representation of an object; eg, <code>(let ((number-of-hamsters$ (princ-to-string number-of-hamsters))) … )</code> or <code>(let ((number-of-hamsters (parse-integer number-of-hamsters$))) … )</code>

* Almost no-one will export a function (or variable) whose name conflicts with a name in the <code>Common-Lisp</code> package, since nearly every possible user will be in a package that has <code>(:use :common-lisp …)</code> in its definition.

* There are also rather strict conventions about indentation, comments, and formatting (inline) documentation which aren't really about naming things.


## Clojure


* [https://github.com/bbatsov/clojure-style-guide The Clojure Style Guide]


## Delphi

The RTL capitalizes identifiers.
Identifiers denoting a type start with <tt>T</tt>, unless it’s a pointer to an already defined type.
Then the prefix is <tt>P</tt>.
Constants (not writable via {$W}) are shouted, although Pascal is case-insensitive.


## Dyalect


Dyalect keywords, variables, constants, function, methods etc. are normally written using <code>camelCase</code>. This stays true for the module names. Type names and constructor names however use <code>PascalCase</code>:


```dyalect
var xs = Array.empty(10)
var ys = Array(1, 2, 3)
var str = xs.toString()

type Maybe = Some(x) | None()
var x = Maybe.Some(42)
```



## Factor

Words are <code>named-with-dashes</code> instead of <code>named_with_underscores</code> or <code>namedWithCamelCase</code>. We tend to avoid abbreviating names. Since we typically don't name throwaway values, this improves clarity. Parsing words are <code>NAMED-LIKE-THIS:</code> so words that perform parse time look-ahead can be easily identified.

Since words can be named anything as long as they don't parse as a number or a string, word names follow an expressive mnemonic system, outlined below. This is not enforced in any way, but encouraged as a way to improve clarity of intent.

{| class="wikitable"
|-
! General form
! Description
! Examples
|-
| <tt>foo?</tt>
| outputs a boolean
| <tt>empty?</tt>
|-
| <tt>foo!</tt>
| a variant of <tt>foo</tt> which mutates one of its arguments
| <tt>append!</tt>
|-
| <tt>?foo</tt>
| conditionally performs <tt>foo</tt>
| <tt>?nth</tt>
|-
| <tt><foo></tt>
| creates a new <tt>foo</tt>
| <tt><array></tt>
|-
| <tt>>foo</tt>
| converts the top of the stack into a <tt>foo</tt>
| <tt>>array</tt>
|-
| <tt>foo>bar</tt>
| converts a <tt>foo</tt> into a <tt>bar</tt>
| <tt>number>string</tt>
|-
| <tt>new-foo</tt>
| creates a new <tt>foo</tt>, taking some kind of parameter from the stack which determines the type of the object to be created
| <tt>new-sequence, new-lexer, new</tt>
|-
| <tt>foo*</tt>
| alternative form of <tt>foo</tt>, or a generic word called by <tt>foo</tt>
| <tt>at*, pprint*</tt>
|-
| <tt>(foo)</tt>
| implementation detail word used by <tt>foo</tt>
| <tt>(clone)</tt>
|-
| <tt>set-foo</tt>
| sets <tt>foo</tt> to a new value
| <tt>set-length</tt>
|-
| <tt>foo>></tt>
| gets the <tt>foo</tt> slot of the tuple at the top of the stack
| <tt>name>></tt>
|-
| <tt>>>foo</tt>
| sets the <tt>foo</tt> slot of the tuple at the top of the stack
| <tt>>>name</tt>
|-
| <tt>with-foo</tt>
| performs some kind of initialization and cleanup related to <tt>foo</tt>, usually in a new dynamic scope
| <tt>with-scope, with-input-stream, with-output-stream</tt>
|-
| <tt>$foo</tt>
| help markup
| <tt>$heading, $emphasis</tt>
|}

Stack effects also follow conventions. A stack effect of a word tells the stack effect checker how many objects a word takes from the stack and how many objects it leaves on the stack. For example, the stack effect for <code>+</code> looks like this: <code>( x y -- z )</code>. This declares that <code>+</code> takes two numbers from the stack and leaves one number on the stack. What you name these inputs and outputs does not matter to the stack effect checker; it only looks at how many inputs and outputs there are.

Inputs and outputs are typically named after some pun on their data type, or a description of the value's purpose if the type is very general.

{| class="wikitable"
|-
! Name
! Value
|-
| <tt>?</tt>
| a boolean
|-
| <tt><=></tt>
| an ordering specifier
|-
| <tt>m, n</tt>
| an integer
|-
| <tt>obj</tt>
| an object
|-
| <tt>quot</tt>
| a quotation
|-
| <tt>seq</tt>
| a sequence
|-
| <tt>assoc</tt>
| an associative mapping
|-
| <tt>str</tt>
| a string
|-
| <tt>x, y, z</tt>
| a number
|-
| <tt>loc</tt>
| a screen location specified as a two-element array holding x and y coordinates
|-
| <tt>dim</tt>
| a screen dimension specified as a two-element array holding width and height values
|-
| <tt>*</tt>
| when this symbol appears by itself in the list of outputs, it means the word unconditionally throws an error
|}


## Forth

True to form, Forth is very unconventional when it comes to Naming conventions. The language is based on the concept of a WORD.  A WORD in Forth is simply a text name that exists in the Forth dictionary.  When a WORD is invoked, it causes some code to run. The code that runs could be anything; a sub-routine, a constant, a named memory address or the "main" routine of the program itself.  It is completely free form.

What's more in Forth any ASCII character can be used to name a WORD with the exception of control characters and the space character. This sounds like a path to chaos and if the programmer chooses to use this power it can give new meaning to the phrase "code obfuscation".  However the language itself uses specific characters for some specific purposes and these have come to be used by Forth programmers when naming their own WORDs. The freedom in naming also allows Forth programmers to emulate naming conventions from other languages if it is appropriate.

Older Forth systems were case sensitive and all keywords were upper case. Most modern systems allow case sensitivity to be on or off.
Some programmers prefer all standard Forth words in the code to be uppercase. As with many aspects of Forth there are divergent opinions on the use of case in code but the language can accommodate them.

Disclaimer: The naming convention examples shown are taken from the experience of the author and are by no means complete.


### =Fetch and Store=

Forth has a WORD to fetch an integer from a memory address called '@' and another WORD to store an integer to memory called '!'.  There are also WORDs to fetch and store double integers called 2@ and 2!.  This is the beginning of a common naming convention in Forth. The '@' and '!' characters are used as suffixes for WORDs that get or put data.
<lang>\ fetch and store usage examples
VARIABLE MYINT1
VARIABLE MYINT2
2VARIABLE DOUBLE1
2VARIABLE DOUBLE2

MYINT1 @  MYINT2 !
MYDOUBLE 2@ MYDOUBLE 2!

\ record example using this convention

1000000 RECORDS PERSONEL

1 PERSONEL RECORD@  \ read Record 1 and put pointer on data stack

HR_RECORD 992 PERSONEL RECORD! \ store HR_RECORD

```


====Colon, Semi-colon====
The Forth compiler is activated with ":" which is just another WORD to Forth. The colon WORD accepts the name of a new word and then begins compiling the WORDs that come after until the WORD ';' is encountered in the input stream. For this reason these two characters are sometimes used in naming WORDS that create new words or for example in Object orient extensions to Forth.
<lang>\ buffer is a word that creates a named memory space and ends in a ':'
: buffer:  ( bytes -- ) create allot ;
hex 100 buffer: mybuffer       \ buffer: creates a new WORD in the dictionary call mybuff

\ if object programming extensions are added to Forth they could look like this
class: myclass <super integer
   m: get  @ ;m      \ create the methods with m: ;m
   m: put  ! ;m
   m: clear  0 swap ! ;m
;class

```



## Fortran


### From the beginning

The name of every Fortran variable must start with a letter and continues with letters and digits only. A variable has an implicit type determined by the first letter of the variable's name.  The implicit types are as follows:
```fortran
IMPLICIT REAL(A-H,O-Z), INTEGER(I-M)
```

First Fortran (1957, for the IBM704) provided no type declarations, so all variables had to be named according to the fixed implicit typing rule, further, no name could be the same as that of a library function after its terminating F was removed. Thus, SINF was for the sine function and so SIN was not an available name for a variable. Similarly, fixed-point (i.e. integer) functions had to start with X despite the rule for variables. The DIMENSION statement defined the sizes of arrays but there was no requirement on the form of the name, for instance that the first letter be an A or similar. Typically, loop variables and array indices are one of I, J, K, L, M, N but there is no requirement enforcing this, other than the integer type rule.

Fortran II (1958, for the IBM704) removed the odd linkage between the names of variables and library functions, and introduced the FUNCTION statement for user-written functions. There was still no type declaration scheme, but now the first letter of a function's name defined the type in the same way as with the names of variables.

Later compilers removed the requirement that library function names end with F, introduced type declarations (INTEGER, REAL, DOUBLE PRECISION, COMPLEX, etc.),  and allowed additional symbols such as an underline and $. Early Fortran source was written all in capitals, but later compilers allow lower case as being equivalent. With F90, the TYPE statement allowed the declaration of compound data aggregates, with % or . used in the name parts, as in <code>Location.Latitude = -38</code>


### Function names

*"D" is often used to indicate that an INTRINSIC FUNCTION returns a DOUBLE PRECISION REAL number. e.g. "cosine" in DOUBLE precision is DCOS, and as a suffix that the argument is in degrees rather than radians as in DCOSD - if available.
*"C" is often used to indicate that an INTRINSIC FUNCTION returns a COMPLEX number. e.g. "cosine" in COMPLEX use CCOS
*"Q" is often used to indicate that an INTRINSIC FUNCTION returns a QUAD PRECISION REAL number. e.g. "cosine" in QUAD precision is QCOS
And combinations can be applied...
*"CQ" is often used to indicate that an INTRINSIC FUNCTION returns a QUAD COMPLEX number. e.g. "cosine" in QUAD COMPLEX use CQCOS


### A persistent problem

Rather than evoking an "undeclared" error message any misspelled variables will be implicitly declared. Typographic mistakes may result in syntactically correct but semantically wrong statements involving such undeclared names, especially given that Fortran disregards spaces outside text literals so that <code>GO TO</code> is just as valid as <code>G OTO</code>. Such errors are very difficult to perceive in source listings. For example the output from the following snippet isn't all the integers from 1 to 10:

```fortran
      DO 999 I=1 10
        PRINT *,I
999   CONTINUE
```


Notoriously, the U.S.A.'s first Venus satellite probe was lost due to the difference between <code>DO 3 I = 1.3</code> and <code>DO 3 I = 1,3</code>  Such texts when printed in wobbly glyphs through a coarse ink ribbon onto rough paper by a lineprinter in a hurry are not obviously incorrect...

;Quirky response
In Fortran 77 then
```fortran
IMPLICIT NONE>
```
 was available to disable implicit typing and thus evoke "undeclared variable" messages. Prior to this the code could use

```fortran
IMPLICIT LOGICAL>
```
 in the hope that the compiler would detect an undeclared LOGICAL variable used in a numerical context, and hence report a semantic type error.


## FreeBASIC

FreeBASIC is not case sensitive and keywords and identifiers can be written in lower case, upper case or mixed case.

Ordinary identifiers can only consist of letters (a-z), numerals (0-9) or the underscore character (_) and cannot begin with a numeral. However, some keywords also include other characters such as #, $ or ?.

Apart from intrinsic 'defines', underscores are not used in the language's keywords even though some of them are multi-word.

There are no official or 'de facto' naming conventions for variables, constants, functions, types etc. and individual developers therefore tend to develop their own style. This may depend on their exposure to other languages such as C, C++, C#, Java or Pascal and the conventions commonly used in those languages.

As long as a consistent style is used, code readability does not seem to be an issue.


## Free Pascal

''See [[#Delphi|Delphi]] and [[#Pascal|Pascal]]''


## Go

Go's naming conventions - like the language itself - are fairly simple.

1. Names of packages should consist of all lower case letters and, preferably, be brief.

2. Names of all other entities should be either 'mixedCaps' or 'MixedCaps' (no underscores).

3. Names which are exported from a package (i.e. public names) must begin with an upper case letter and be declared either at 'top level' within the package or be field or method names. All other names are not exported and hence are private to the package. This is not a convention, it is an obligatory part of the language.

4. If a struct has a private field, 'owner' say, public access to which is via by a getter and/or setter method, then the former should use the style Owner() and the latter SetOwner().

5. One method interfaces should take the name of the method and add the suffix 'er'. For example the Stringer interface contains the single method String().

For the purposes of the above conventions, 'letter' includes any recognized Unicode letter and is not therefore restricted to ASCII letters.


## Haskell

Most keywords are in lowercase. Of punctuation marks, only the colon is considered as uppercase and all others that are valid are considered as lowercase.

Haskell requires that names of types, constructors, classes, and modules start with an uppercase letter, while names of constants, variables, fields of record types, must start with lowercase letters.

It is common to use camel case although not required. Sometimes the name of something ends with an apostrophe to represent a mathematical "prime" mark.


## J


The nice thing about conventions is much like the nice thing about standards: there are so many to choose from.

Classic J tends to favor terse names. One influence, here, is that it's rather dismaying when the name of your procedure is longer than its implementation. This matches the style of classic works on mathematics, and also makes it easier to type, and easier to keep code near to related code. This style is especially popular with local variables.

J also sometimes borrows from C's conventions (ALL CAPS constant names, for example).

Another convention describes the transformation being done using the convention afterFromBefore. This matches the right to left style of assignment operations (which much of J's syntax also adopts). When combined with the "terse naming" convention, you get things like <code>hfd</code> (meaning ''hexadecimal from decimal'').

Another convention, when dealing with external code, involves simply using the foreign names. You can see this, for example, in the opengl support. This makes it a bit easier to use the original documentation.

Other conventions are also in use.


## jq


jq has some keywords (such as 'reduce') that impose certain restrictions on naming, but these are strictly enforced and so perhaps do not count as conventions. Similarly, there are strictly enforced restrictions regarding the use of the dollar-sign "$".  Perhaps the most important convention to know about, therefore, is that identifiers beginning with "_" are reserved for internal use as function names, and should therefore not in general be used as such.

Although it is permissible and sometimes appropriate to define a function using the name of an existing built-in function, it is generally better to avoid doing so.  With this in mind, it is useful to know that the built-in `builtins` emits a stream of strings representing the currently-defined built-ins.  Furthermore, it is likely that future built-in functions will be named in accordance with existing practice, so it is useful to know that the names of built-in functions currently fall into one of these categories:

# names beginning with an underscore;
# names composed of lowercase letters [a-z] only;
# names such as `map_values`, that is, names composed of strings of lowercase letters joined by a single underscore;
# names of the form [a-z]+[0-9]+
# names composed only of uppercase letters [A-Z] (currently: IN, INDEX, JOIN)
# two exceptions: log1p, utf8bytelength


###  Variables

In 2017, the jq global $ARGS was introduced.  It behaves as if it had been defined on the command-line.  As with all such global variables, it can be shadowed.


## Julia


In general, modules and type names use capitalization and camel case: module SparseArrays, struct UnitRange.

Variables and functions are lowercase (maximum, convert) and, when readable, with multiple words squashed together (isequal, haskey).
When necessary, use underscores as word separators. Underscores are also used to indicate a combination of concepts
(remotecall_fetch as a more efficient implementation of fetch(remotecall(...))) or as modifiers (sum_kbn).

Append ! to names of functions that modify their arguments.

Conciseness is valued, but avoid abbreviation (indexin rather than indxin) as it becomes difficult to remember whether and how particular words are abbreviated.


## Kotlin

Kotlin's published 'Coding Conventions' specify the following styles for naming program entities:

* use camelCase for names (and avoid underscore)
* types start with upper case
* methods and properties start with lower case


In addition to these conventions, it is common practice for enum members and compile time constants to have names which are all upper case and therefore use snake_case to separate words.

Although most of the Kotlin code I have seen follows these conventions, they are not obligatory. Some developers, for instance, like to prefix the private members of a type with an underscore.

The following program illustrates these conventions:

```scala
// version 1.0.6

const val SOLAR_DIAMETER = 864938

enum class Planet { MERCURY, VENUS, EARTH, MARS, JUPITER, SATURN, URANUS, NEPTUNE, PLUTO } // Yeah, Pluto!

class Star(val name: String) {
    fun showDiameter() {
        println("The diameter of the $name is ${"%,d".format(SOLAR_DIAMETER)} miles")
    }
}

class SolarSystem(val star: Star) {
    private val planets = mutableListOf<Planet>()  // some people might prefer _planets

    init {
       for (planet in Planet.values()) planets.add(planet)
    }

    fun listPlanets() {
       println(planets)
    }
}

fun main(args: Array<String>) {
    val sun = Star("sun")
    val ss = SolarSystem(sun)
    sun.showDiameter()
    println("\nIts planetary system comprises : ")
    ss.listPlanets()
}
```


```txt

The diameter of the sun is 864,938 miles

Its planetary system comprises :
[MERCURY, VENUS, EARTH, MARS, JUPITER, SATURN, URANUS, NEPTUNE, PLUTO]

```



## Lua

Extract from https://github.com/zaki/lua-style-guide

 - constants use UPCASE_SNAKE
 - variable names use lower_case_snake
 - class-like structures use CamelCase
 - other functions use smallCamelCase
 - Use _ for unneeded variables
 - Don't use Hungarian Notation

```Lua
local DISTANCE_MAXIMUM   = 1
local distance_to_target = 0
local function distanceToTarget() end
local function TargetFactory() end

for _,v in ipairs(table) do
  print(v)
end
```



## M2000 Interpreter

Identifiers need one letter at least, and any number of characters/numbers after.
If we use non break space as character: alfa beta=10  is a variable alfa beta (but here we get a copy with a space). To use non break space we press Shift+Ctrl+Space. To view it we can change rendering of internal text editor with F10 (show paragraph marks and different spaces). Shift+Alt+Space return Number's space.

M2000 is like Basic, identifiers are case insensitive. For Greek names also accent marks can be used freely (so a fault accent mark isn't a fault for interpreter).


Labels although are case sensitive
We have to use $ as last character in names for anything return string. We can use % as last character for integers (as values, because under it maybe double or something else, depends of the first value we assign it).

We use <= to assign values to global variables, because = make new variables and shadow any global with same name.
We use <= for group members also inside functions which are members too.
Arrays didn't use <= to get values.

Groups which return strings also have to use $ in names, but these have two names as in this example:

```M2000 Interpreter

Class Alfa$ {
Private:
            myValue$
Public:
            Set {
                  Read .myValue$
            }
            Value {
                  =.myValue$
            }
            Module Doit {
                  Function TwoChars$(A$) {
                        =Left$(Left$(a$,1)+Right$(a$,1)+"??",2)
                  }
                  Print TwoChars$(.myValue$)
                  Dim A$(3)
                  A$(0)="zero","one","two"
                  Print A$(1)
                  k$=Lambda$ A$() (x) -> {
                        if x>0 and x<3 then {=A$(x)} else =A$(0)
                  }
                  Print k$(2)
                  Dim A$()
                  Print Len(A$())=0
                  Print k$(2)="two"
            }
}
A$=Alfa$()
Module CheckIt(&B$) {
      Input "name:", B$
      B.Doit
}
Checkit &A$
Print A$

```



## Mathematica

All built-in Mathematica commands are case sensitve and written in camel case. Most are full english words. For example: FactorInteger, ListPlot, AsynchronousTaskObject etc.
A small number of abbreviation conventions are used: Predicates end in Q, eg PrimeQ, EvenQ, MatrixQ, numerical routines start with N, eg NSolve, NLimit, NMinimize. System variables start with $PreferencesDirectory, $TimeZone, $Version etc.

Convention is that user variables and function names also also cammel case except they start with a lower case. eg myFunctionName. There is no limit to the number of characters in a user symbol, symbols can contain digits and non-ascii characters.


## OASYS Assembler

Prefixes and suffixes are required on names. It is allowed for a name to consist of only the prefix and/or suffix without any letters or numbers.

The prefix is one of:
* no prefix = Built-in opcode or a macro
* <tt>!</tt> = Static object
* <tt>%</tt> = Global variable
* <tt>,</tt> = Local variable or argument
* <tt>.</tt> = Property
* <tt>&</tt> = Method
* <tt>?</tt> or <tt>*</tt> = Class
* <tt>:</tt> = Label
* <tt>'</tt> = Vocabulary word

The suffix specifies the data type of a variable or property or argument or the type of the return value of a method, and it is one of:
* no suffix = Void; used for methods which do not return a value
* <tt>@</tt> = Object
* <tt>#</tt> = Integer
* <tt>$</tt> = String
* <tt>^</tt> = Pointer; may be followed by another suffix

Terse names are generally preferred.


## Oforth


Almost everything can be a word so almost every convention can be used.

Oforth language built-ins use thoses conventions :
- Global constants name are uppercase.
Example : JUSTIFY_LEFT

- Constants for a class are prefixed by the class name.
Example : Date.JANUARY, Date.Months

- Classes and properties begin with an uppercase letter.
Examples: Object, Integer, Comparable.

- Methods begin with a lowercase letter.
Examples: first, max, and, ....

- Functions or methods that require reading another word end with ':'.
Examples: loop: , for: , new: , ...


## Pascal

Pascal is a strongly typed language.
It likes to encourage the programmer to write what is ''meant'', not what the underlying structure is.
Since identifiers are case-insensitive, PascalCase is one, if not ''the'' capitalization strategy most programmers adhere to.
See also [[#Delphi|Delphi]] and [[#Free Pascal|Free Pascal]].


## Perl

This being Perl, you are of course give wide latitude in the names you can use for your program elements.
But it can be helpful to other programmers, and possibly your future self, to use letter case and underscores
to indicate the scope and nature of variable and routines, so their roles can be understood at a glance.
Some generally accepted conventions:

    $ALL_CAPS_HERE   constants
    $Some_Caps_Here  package-wide global/static
    $no_caps_here    function scope my/our/local variables
    $_internal_use   private
Note the use of underscores for readability.
Subroutines and variables meant to be treated as private can be prefixed with an underscore.
With all-caps constants, be careful for conflicts with Perl special variables.
Function and method names should be all lowercase.

Any reasonably modern version of Perl can use Unicode characters in names, so employ them where it makes sense. It
may be necessary to invoke the <code>use utf8</code> pragma that case.

<b>A caution about variables names and sigils</b>:
Perl won't stop you from giving three variables in the same scope the names
<code>%foo</code>, <code>@foo</code> and <code>$foo</code>, but that doesn't
mean it's a good idea.  Here's a good example of [https://gist.github.com/SqrtNegInf/df5fbab044babc6ca16e229d2d1c669f what not to do] (offsite Perl code)


## Perl 6

Perl 6 is written in Unicode, and has consistent Unicode semantics regardless of the underlying text representations. By default Perl 6 presents Unicode in "NFG" formation, where each grapheme counts as one character. A grapheme is what the user would think of as a character in their normal everyday life, including any diacritics.

Built-in object types start with an uppercase letter. This includes immutable types (e.g. Int, Num, Complex, Rat, Str, Bit, Regex, Set, Block, Iterator), as well as mutable (container) types, such as Scalar, Array, Hash, Buf, Routine, Module, and non-instantiable Roles such as Callable and Integral. The names may extend to CamelCase for compound words: IntStr, CaptureCursor, BagHash, SoftRoutine.

Non-object (native) types are lowercase: int, num, complex, rat, buf, bit.

Nearly all built-in subroutines, functions, methods and pragmas included in Perl 6 CORE are lowercase or lower kebab-case. (Compound words joined with hyphens rather than underscores or camelCase.) .grep, .pairs, .log, .defined, .subst-rw. The few notable exceptions are those which can radically change behaviour of the executing code. They are in all-cap/kebab-case to make them stand out: EVAL, MONKEY-TYPING.

All upper case names are semi-reserved. You are free to use them, but are warned that you may encounter future collisions with internal usage. Upper case names are used for pseudo-packages: MY, OUR, CORE, GLOBAL, etc., for relative scope identifiers: CALLER, OUTER, SETTING, PARENT, etc. and other things.

Variables in Perl 6 CORE tend be lower kebab-case for lexical variables and upper case for special or package globals. They have an attached, prefix sigil (or twigil) to indicate what type of object they hold and what methods are available to operate on them.

In user space, there are very few restrictions on how things are named. Identifers of any type can not contain white space. Subroutines must start with a letter character, any unicode character that has a "letter" property. Variable names can't contain any of the sigil, twigil or comment characters ($, @, %, *, ?, =, :, #). Outside of those few restrictions, it's pretty much a free-for-all.

That being said, there are some community conventions which are encouraged, though not enforced. Descriptivness is favoured over terseness, though this should be scaled to the scope of the object. It is perfectly fine to name an index variable in a three line loop, $i. An object in global scope with dozens of methods may be better off with a more descriptive name. It is encouraged to name subroutines for what they do to make it easier for others to follow your logic. Nouny things should have nouny names. Verby things should be verby. If you ''aren't'' going to follow convention, at least be consistent.


## Phix

Conventions are not strictly enforced.

Builtin and library routines are full english lowercase words and underscore separated, eg custom_sort()

Function-style invocation is common instead of infix operators for less-used and new features, eg power(3,5)

Constants are often uppercase, eg PI

Keywords are always lowercase, eg while

The end keyword always has an explicit match, eg end while


Variable names are case-sensitive and may not contain any embedded operators or spaces

Over-loading or over-riding the names of variables and routines is generally frowned upon

Some unicode characters are permitted, in an ad-hoc manner via fairly trivial mods to ptok.e

The pGUI.e library retains the original IUP C names, eg IupDialog


File extensions generally imply the following:

.e - general purpose include, not executable standalone

.ew - windows-only include

.eu - linux-only include

.exw - historically this implied windows-only, but is now also used for any cross-platform gui applications.



## PicoLisp

PicoLisp has not hard-coded naming rules, with the exception of symbols starting
with an at-mark '@': They have special meaning as "pattern variables" for the
'[http://software-lab.de/doc/refM.html#match match]' and
'[http://software-lab.de/doc/refF.html#fill fill]' functions, and as
'[http://software-lab.de/doc/ref.html#pilog Pilog]' variables.

Besides this, PicoLisp programs follow these naming conventions:

```txt
- Global variables start with an asterisk '*'
- Global constants may be written all-uppercase
- Functions and other global symbols start with a lower case letter
- Locally bound symbols start with an upper case letter
- Local functions start with an underscore '_'
- Classes start with a plus-sign '+', where the first letter
   - is in lower case for abstract classes
   - and in upper case for normal classes
- Methods end with a right arrow '>'
- Class variables may be indicated by an upper case letter
```



## PowerShell

Microsoft describes a cmdlet as a lightweight command that is used in the Windows PowerShell environment. The Windows PowerShell runtime invokes these cmdlets within the context of automation scripts that are provided at the command line. The Windows PowerShell runtime also invokes them programmatically through Windows PowerShell APIs.
The built-in cmdlets invariably follow the convention of a Verb-Noun pair, e.g.: <code>Get-Process</code> or <code>Set-Content</code>. Cmdlets are Pascal cased with a dash separating the Verb-Noun pair.

Functions are user-defined (script-based) equivalents to the built-in cmdlets; therefore, any function (that is not a simple "helper" function) should probably follow the naming convention of cmdlets.  I have experimented with function naming and found that a function could be named something as silly as <code>Out-%k+^</code>, but why would one want to do that?
Users may define modules containing functions and variables which is very handy, but a function defined in a module must follow the cmdlet naming convention or a warning will appear on the screen when the module is loaded.

Aliases may be defined for any cmdlet or function, so name your function with a meaningful (and conventional) name, then define an alias for it.
There are many built-in aliases and they are all based on shortened PowerShell names, Unix or MS-DOS, e.g.: <code>Get-Content</code> = <code>gc</code> = <code>cat</code> = <code>type</code>.

Variables are prefixed with a "$" as in Perl and as such, should probably be camel cased.  A variable must be alphanumeric (in any order) and may contain underscore characters.
PowerShell, of course, offers the option of naming a variable "anything" if the user surrounds the name with ${}, e.g.: <code>${one two three} = @(1, 2, 3)</code>.


## Python

* Class names are typically in [[wp:CamelCase|CamelCase]], often this is reflected in the module name.
* Private member functions are embeded between "__" to make a member function "private".
* Variables are generally lower-case.


## Racket


For more details, read the explanation in the Name section of the Style Guide: http://docs.racket-lang.org/style/Textual_Matters.html#%28part._names%29 .

The convention is to use full English lowercase words separated by dashes


```Racket
#lang racket
render-game-state
send-message-to-client
traverse-forest
```


Usually <code>_</code> is used only as the name of a dummy argument.

Most functions names have as prefix the data type of the main argument. Some notable exceptions are the functions for <code>list</code>s and <code>box</code>es, for backward compatibility.


```Racket
#lang racket
(string-ref "1234" 2)
(string-length "123")
(string-append "12" "34")
;exceptions:
(append (list 1 2) (list 3 4))
(unbox (box 7))
```


This convention generalizes the selector-style naming scheme of <code>struct</code>s.


```Racket
#lang racket
(struct pair (x y) #:transparent #:mutable)
(define p (pair 1 2))
(pair-x p)    ; ==> 1
(set-pair-y! p 3)
p    ; ==> (pair 1 3)

```


The name of conversion procedure is usually like <code>from-&gt;to</code>

```Racket
#lang racket
(list->vector '(1 2 3 4))
(number->string 7)
```


In addition to regular alphanumeric characters, some special characters are used by convention to indicate something about the name. The more usual are:


```Racket

;predicates and boolean-valued functions:   ?
(boolean? 5)
(list? "123")

;setters and field mutators:   !
(set! x 5)
(vector-set! v 2 "x")

; classes:   %
game-state%
button-snip%

;interfaces:   <%>;
dc<%>;
font-name-directory<%>
```




## REXX


### implicit types

The (Classic) REXX language has no implicit types for variable (names) or function (names)   except

that   ''all''   variables'   ''values''   are of the type   ''' ''character''.'''

So, it can be thought that the implicit type for   ''everything''   is the type   ''' ''character''.'''


### numbers

All numbers are stored as characters:   decimal digits, with/without signs, decimal points, and exponents

(and blanks where permitted).

Values that conform to the REXX definition of a number (below) are treated as a number:

 <big>{blanks}  {-│+}  {blanks}  {digits}  {<big>.</big>}  {digits}  {e│E}  {-│+}  {exponent}  {blanks}</big>

(Decimal)   '''digits'''   are any of the digits:   '''0 1 2 3 4 5 6 7 8 9'''.

The   '''e'''   or   '''E'''   (above) signifies the following decimal number is an exponent   (a power of ten that

is implicitly multiplied to the preceding number).

Everything (for a number) is optional,   but there must be   ''at least''   one decimal digit.

If an   '''e'''   or   '''E'''   is present, it must be immediately followed by an integer   (a decimal exponent),

with/without an optional sign.

A leading sign   (for a number)   if present, may have a minus sign (<big>'''─'''</big>)   or   a plus sign (<big>'''+'''</big>).


### variable names

Naming conventions (as far as capitalization is concerned) is that variable names may be in any case,

the REXX language definition is that variable names are stored in capital letters internally.

Another restriction is that the variable name can't start with a decimal digit.


In Classic REXX, characters other than the Latin (English) alphabet that can be used such as:
:::*   <big>    !    </big>     (explanation point)
:::*   <big>    _    </big>     (underscore or underbar)
:::*   <big>    $    </big>     (dollar sign)
:::*   <big>    ?    </big>     (question mark)
:::*   <big>    #    </big>     (pound sign or hash)
:::*   <big>    @    </big>     (commercial at sign)

Some Classic REXX interpreters allow additional characters   [such as the   <big> ¢ </big>   (cent sign)].

Other characters are also allowed   ''after''   the first character (above):
:::*   <big> '''.''' </big>     (a period or decimal point)
:::*   the decimal digits   '''0'''   through   '''9'''

So:      '''AbcXyz''',   '''abcxyz''',   '''ABCxyz'''   all refer to the same variable.


### function names

Naming conventions for the REXX BIFs   (built-in functions)   are all in uppercase, but they can be

coded in lowercase (or mixed case) for ease-of-use and readability.

For example:

```rexx
w=length(abc)
```

─── where   '''length'''   is a REXX BIF for the   ''length''   of the   value   of the variable   '''ABC'''

If there is an internal function with a built-in function's name in the program,   the built-in (REXX)

function can be invoked using its name as an uppercase literal string as shown below:

```rexx
s= 'abcd'
say "length(s) ="   length(s)            /* ──► 1000 */
say "'LENGTH'(s) ="   'LENGTH'(s)        /* ──►  4   */
exit

length: return 1000
```

```txt

length(s) = 1000
'LENGTH'(s) = 4

```



### label names

Labels in Classic REXX can be any of the Latin (English) alphabet, as well as the decimal digits   '''0 ──► 9''',

in addition to other characters such as:
:::*   <big> '''.''' </big>     (a period or decimal point)
:::*   <big>    !    </big>     (explanation point)
:::*   <big>    _    </big>     (underscore or underbar)
:::*   <big>    $    </big>     (dollar sign)
:::*   <big>    ?    </big>     (question mark)
:::*   <big>    #    </big>     (pound sign or hash)
:::*   <big>    @    </big>     (commercial at sign)


Label names may start with any of the above characters.

Some Classic REXX interpreters allow additional characters   [such as the   <big> ¢ </big>   (cent sign)].

Note that REXX keeps the label names as capitalized letters, but either lowercase/mixed/upper

case may be used interchangeably.





## Ruby


The naming conventions in Ruby are almost on the whole agreed upon by the Ruby community. Ruby is a case sensitive language which means that
<code>HelloWorld</code> is different from <code>helloworld</code>.


### Variable Names

In ruby, all variables must begin with a lower case letter or an underscore. Variables rarely begin with underscores and variable names should be relatively descriptive in ruby. Additionally, ruby uses snake case for variable names. This means that if I wanted to have a variable that held the time of day, I'd call that variable <code>time_of_day</code>. Instance variables begin with <code>@</code>, class variables begin with <code>@@</code> and global variables begin with <code>$</code>


### Constants

In ruby, constants must being with an upper case letter and are traditionally in screaming snake case (note: this is the one point of contention within the ruby community when it comes to naming conventions. A majority of ruby users use screaming snake case, but some use Pascal Case for constant names instead). Anything assignable object that begins with an uppercase letter is automatically a constant in ruby. Constant names should be descriptive as well. For example, a constant for the recursion limit could be <code>RECURSION_LIMIT</code>.


### Method names

Method names in ruby are quite similar to variable naming conventions with a few additional rules. In ruby, a method that returns a boolean traditionally ends with a question mark. Many of these methods exist in the standard library such as, <code>1.positive?</code> or <code>'test'.tainted?</code>. Also, so called <i>destructive</i> methods end with an exclamation point. For example,

```Ruby

test_variable = [1, 9, 8, 3]
test_variable.sort    # => [1, 3, 8, 9]
test_variable         # => [1, 9, 8, 3]
test_variable.sort!   # => [1, 3, 8, 9]
test_variable         # => [1, 3, 8, 9]

```

The <code>sort</code> method just returns a sorted version of the array, but the <code>sort!</code> method sorts the array in place. Additionally, Ruby has accessors in its class so you don't need to write explicit getters or setters, but if you do the proper naming convention for a getter is <code>field_name</code> and the proper naming convention for a setter is <code>field_name=</code>. An example of this will be seen later on in this description. Constructors are always named <code>initialize</code>.


### Classes and Modules

Classes and modules have the same naming convention in ruby. They should be in Pascal case and they should be descriptive.


### Putting it all together

Here is an example of a ruby file with proper naming conventions being used.


```Ruby

# Global variable
$number_of_continents = 7

module Banking
  # Module constants for semantic versioning
  VERSION = '1.0.0.1'
  class BankAccount
    attr_accessor :first_name, :last_name
    attr_reader :account_number

    @@ATM_FEE = 3.75
    @@adiministrator_password = 'secret'

    # The class's constructor
    def initialize(first_name, last_name, account_number)
      @first_name = first_name
      @last_name = last_name
      @account_number = account_number
      @balance = 0
    end

    # Explicit setter as extra behavior is required
    def account_number=(account_number)
      puts 'Enter administrator override'
      if gets == @@adiministrator_password
        @account_number = account_number
      else
        puts 'Sorry. Incorrect password. Account number not changed'
      end
    end

    # Explicit getter as extra behavior is required
    def balance
      "$#{@balance / 100}.#{@balance % 100}"
    end

    # Check if account has sufficient funds to complete transaction
    def sufficient_funds? (withdrawal_amount)
      withdrawal_amount <= @balance
    end

    # Destructive method
    def donate_all_money!
      @balance = 0
    end
  end
end

```



## Scala

An excellent documentation about naming is given in [https://docs.scala-lang.org/style/naming-conventions.html the Scala Style Guide.]

## Tcl

Tcl leaves nearly all matters of variable and procedure naming up to the programmer, so styles vary. Variables are not declared by type. However, each variable contains a scalar, list, or hash array. Once assigned a scalar, list, or array, a variable must be unset to be re-assigned a different kind (scalar, list or array).

A few conventions are common:
* Names typically use alphanumeric characters ('''a-zA-Z0-9_'''). Use of other characters are allowed, but may require extra quoting with curly braces ('''{}''') (See ref: [http://wiki.tcl.tk/10259 Rule #7])
* Two naming styles dominate for variables: '''all_lower_case_with_words_separated_by_underscore''' or '''camelCase'''
* Namespaces are usually named in lowercase, starting with a letter ('''{[a-z][a-z0-9_]*}''') and separated by double colons ('''::'''). For example space1::space2::space3
* '''TitleCase''' names are typically used for private members.  TclOO's default export pattern '''{[a-z]*}''' supports this convention.
* options/flags are typically spelled in all lowercase with no internal punctuation:  '''-nonewline''' for example.
* Internalized commands and variables may begin a leading underscore '''_''' to differentiate them from externalized API.
* The variable value '''unknown''' is special in some contexts:  it can be used to handle the "no such method" or "no such command" case.
* Specific array names '''_''' and '''{}''' (the empty string) are used quite commonly for internal state
* Procedure parameter '''args''' is used for variadic functions, and typically this name is never used for anything else
* '''Tk''' graphics toolkit window names start with '''.''' and must not have a capital letter in the next position
* Combinations of nonstandard characters may be used to create variables that are hidden ie not printable. These are still discoverable without much effort. Hiding variables is not a recommended security strategy.

Sometimes suffixes are added to variables to indicate a significant way that they are used.  Here's some examples.  These are not standardized by any means:

*  example_p The suffix '''_p''' implies Boolean value ie true/false represented as 1 or 0.
*  example_list  Implies the variable contains a list of values.
*  example_arr() The suffix _arr implies variable is an array of scalar values.
*  example_larr() The suffix _larr implies the variable is an array of list values.
*  example_ts  The suffix _ts implies its value is a timestamp
*  example_s  The suffix _s implies its value is in seconds or seconds from epoch.
*  example_i  The suffix _i implies its value is an integer.


## Visual Basic

<!-- Naming conventions -->
Microsoft was encouraging [https://en.wikipedia.org/wiki/Camel_case camelCase] in Visual Basic. And at the beginning Microsoft was also encouraging programmers to use [https://en.wikipedia.org/wiki/Hungarian_notation Hungarian notation].

'''Camel case:'''

The variable name begins with a prefix and has one or more uppercase inside.

```vb
Dim dblDistance as Double>
```

'''Hungarian notation:'''

```vb
Dim iRow as Integer, iColumn as Integer
Dim sName as String
Dim nPopulation as Long
Dim xLightYear as Double
iRow = iRow + 1
```

Prefix i is for index and n for count. The Hungarian notation reminds in a way FORTRAN implicit type: prefix characters i,j,k,l,m,n for integers and the rest for reals.

The real advantage is for the names of the VB controls.
{| class="wikitable"
|-
! Prefix
! Meaning
|-
| <tt>frm</tt>
| <tt>Form</tt>
|-
| <tt>mnu</tt>
| <tt>Menu</tt>
|-
| <tt>cmd </tt>
| <tt>Command button</tt>
|-
| <tt>chk</tt>
| <tt>Check button</tt>
|-
| <tt>opt</tt>
| <tt>Radio button</tt>
|-
| <tt>lbl</tt>
| <tt>Text label</tt>
|-
| <tt>txt</tt>
| <tt>Text edit box</tt>
|-
| <tt>pic</tt>
| <tt>Picture</tt>
|-
| <tt>cbo</tt>
| <tt>Combo box</tt>
|-
| <tt>tmr</tt>
| <tt>Timer</tt>
|-
| <tt>...</tt>
| <tt>...</tt>
|}
Exemple:

```vb
txtTraduc.Width = iWidth
cmdSolution.Enabled = False
mnuAleatoire.Checked = False
frmScore.Show vbModal
picFace.Visible = False
picFace.Picture = LoadPicture(sFileName)
```

The advantage comes clear for the “Private Sub” names of the controls:

```vb
mnuQuit_Click()
cmdSolution_Click()
frmScore_Resize()
```



## zkl

* Conventions are for the user to [create and] follow, no enforcement.
* The compiler uses the "__" prefix as its "name space". For example: __DATE__, __sGet. A program can also use that format.
* The compiler will put a "#" (comment in source code) in a name to mark it as "out of bounds". For example "__fcn#1_2" is the first lambda function and is located at source line 2.
* Names must be unique. For example, a variable can not have the same name as a function. This is a confusion reducer.
* Names are restricted to 80 characters of [A-Za-z0-9_], plus "#" when bypassing the tokenizer.

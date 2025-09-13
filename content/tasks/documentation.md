+++
title = "Documentation"
description = ""
date = 2019-10-15T09:28:33Z
aliases = []
[extra]
id = 4815
[taxonomies]
categories = ["task", "Software Engineering"]
tags = []
+++

## Task

{{task|Software Engineering}}Show how to insert documentation for classes, functions, and/or variables in your language. If this documentation is built-in to the language, note it. If this documentation requires external tools, note them.


## See also

* Related task: [[Comments]]
* Related task: [[Here_document]]





## Ada

In some ways, Ada is a self documenting language by design.  When building packages (the equivalent of modules in python), you have to create two separate files: a spec and a body.  The spec resides in a .ads file, and contains the interface to the package that is visible to someone using it.

All functions, procedures and data-types that can be used in other Ada programs have to have an entry in the spec that defines how they are used.  The only requirement for the body file is that it give definitions for everything found in the spec.  You can also include other functions in the body file to help build the functions defined in the spec.  Anything that doesn't appear in the .ads file won't be accessible to a programmer who uses that particular package.

The ada spec file gives you all the information you need to know to effectively use the functions in that package, and as such is the primary documentation that you want to look at.  If you have the .adb and .ads files physically available to you, you can also verify that the information is correct by re-compiling the package, the Ada compiler checks to make sure that the spec and the body agree, and that everything defined in the spec is implemented in the body.

Example ADS file for instrumenting sorting functions:

```ada
with Ada.Text_Io; use Ada.Text_Io;

generic
   SortName : in String;
   type DataType is (<>);
   type SortArrayType is array (Integer range <>) of DataType;
   with procedure Sort (SortArray : in out SortArrayType;
                        Comp, Write, Ex : in out Natural);

package Instrument is
   -- This generic package essentially accomplishes turning the sort
   --  procedures into first-class functions for this limited purpose.
   --  Obviously it doesn't change the way that Ada works with them;
   --  the same thing would have been much more straightforward to
   --  program in a language that had true first-class functions

   package Dur_Io is new Fixed_Io(Duration);

   procedure TimeSort (Arr : in out SortArrayType);

   procedure Put;

   procedure Put (File : in out File_Type);

end Instrument;
```

There is also a tool called [http://sourceforge.net/projects/adadoc/ AdaDoc].  This can be used to generate documentation of an Ada module (or modules?) for a variety of different formats: HTML, LaTeX, plain text and more.


## Arturo



```arturo
:doc #{
	project		~Arturo

	author 		"Dr.Kameleon"
	email		"drkameleon@gmail.com"
	license		"MIT"

	file 		"Documentation.art"
}

someFunc {
	:doc #{ description	"takes two numbers and adds them up" }

	return &0+&1
}
```



## AutoHotkey

You can use the [http://www.autohotkey.com/forum/viewtopic.php?t=22548 GenDocs library].

It generates html documentation in the style of the canonical ahk [http://www.autohotkey.com/docs/commands/MsgBox.htm documentation].

[http://en.wikipedia.org/wiki/BBCode BBCode] is supported for markup.

Example from the library distribution:
```AutoHotkey
;
; Function: example_add
; Description:
;     Adds two or three numbers (see [url=http://en.wikipedia.org/Number]Number [/url])
; Syntax: example_add(number1, number2[, number3=0])
; Parameters:
;		number1 - First number to add.
;		number2 - Second number to add.
;		number3 - (Optional) Third number to add. You can just omit this parameter.
; Return Value:
;		sum of parameters
; Example:
;		MsgBox % example_add(example_add(2, 3, 4), 5)
;
example_add(number1, number2, number3=0){
    return number1 + number2 + number3
}
```
[http://www.autohotkey.net/~tinku99/test/example_add.htm Resulting Documentation]


## C

A common tool for generating documentation is [http://www.stack.nl/~dimitri/doxygen/ Doxygen]:

```c
/**
 * \brief Perform addition on \p a and \p b.
 *
 * \param a One of the numbers to be added.
 * \param b Another number to be added.
 * \return The sum of \p a and \p b.
 * \code
 *     int sum = add(1, 2);
 * \endcode
 */
int add(int a, int b) {
    return a + b;
}

```



## Clojure


```lisp
(def
 #^{:doc "Metadata can contain documentation and can be added to vars like this."}
     test1)

(defn test2
  "defn and some other macros allow you add documentation like this. Works the same way"
  [])
```



## COBOL

ROBODoc allows for documentation extracts from pretty much any language that
allows comments. Includes highlighting of source code.  This feature has been
extended for COBOL to allow dashes in names, along with the normal C based
underscore divider. ROBODoc looks for start and end triggers.

Documentation keywords, and comment marker triggers, are managed by
configuration settings and ROBODoc ships with quite a few predefined special
words.  Lines after the colon terminated keywords are each extracted into
different paragraph types.  Graphviz can also be invoked to include DOT
drawings, images can be embedded, and shell commands can also be invoked during
the HTML generation process.


```COBOL

      *>****L* cobweb/cobweb-gtk [0.2]
      *> Author:
      *>   Author details
      *> Colophon:
      *>   Part of the GnuCobol free software project
      *>   Copyright (C) 2014 person
      *>   Date      20130308
      *>   Modified  20141003
      *>   License   GNU General Public License, GPL, 3.0 or greater
      *>   Documentation licensed GNU FDL, version 2.1 or greater
      *>   HTML Documentation thanks to ROBODoc --cobol
      *> Purpose:
      *> GnuCobol functional bindings to GTK+
      *> Main module includes paperwork output and self test
      *> Synopsis:
      *> |dotfile cobweb-gtk.dot
      *> |html <br />
      *> Functions include
      *> |exec cobcrun cobweb-gtk >cobweb-gtk.repository
      *> |html
```txt

      *> |copy cobweb-gtk.repository
      *> |html
```

      *> |exec rm cobweb-gtk.repository
      *> Tectonics:
      *>   cobc -v -b -g -debug cobweb-gtk.cob voidcall_gtk.c
      *>        `pkg-config --libs gtk+-3.0` -lvte2_90 -lyelp
      *>   robodoc --cobol --src ./ --doc cobwebgtk --multidoc --rc robocob.rc --css cobodoc.css
      *>   cobc -E -Ddocpass cobweb-gtk.cob
      *>   make singlehtml  # once Sphinx set up to read cobweb-gtk.i
      *> Example:
      *>  COPY cobweb-gtk-preamble.
      *>  procedure division.
      *>  move TOP-LEVEL to window-type
      *>  move 640 to width-hint
      *>  move 480 to height-hint
      *>  move new-window("window title", window-type,
      *>      width-hint, height-hint)
      *>    to gtk-window-data
      *>  move gtk-go(gtk-window) to extraneous
      *>  goback.
      *> Notes:
      *>  The interface signatures changed between 0.1 and 0.2
      *> Screenshot:
      *> image:cobweb-gtk1.png
      *> Source:
       REPLACE ==FIELDSIZE== BY ==80==
               ==AREASIZE==  BY ==32768==
               ==FILESIZE==  BY ==65536==.

id     identification division.
       program-id. cobweb-gtk.

       ...

done   goback.
       end program cobweb-gtk.
      *>****

```


Another style of auto generated documentation involves the Compiler Directive
Facility and using special defines to mingle documentation blocks within lines
of the source file, which can then be extracted and passed through various
markup processors, like Sphinx ReStructureText or Markdown.


```COBOL

>>IF docpass NOT DEFINED

... code ...

>>ELSE
!doc-marker!

### ==

:SAMPLE:

### ==


.. contents::

Introduction
------------
ReStructuredText or other markup source ...
>>END-IF

```


Extraction of documentation segments is feasible using just the Compiler
Directive Facility, but it is usually easier to use '''sed''' or other tools.
Otherwise any Compiler Directives within the documentation become ''live'' and
documentation may trigger other conditional compile statements, usually to bad
effect.  In the case of COBOL, this includes both the '''COPY'''  and
'''REPLACE''' Text Manipulation keywords, which can easily show up within
documentation segments.  It is safer to use an undefined conditional to simply
skip documentation blocks during actual code compilation and extract the
documentation lines with a separate tool before passing the extract to a markup
processor.


## Common Lisp


Common Lisp allows the definition of documentation strings for functions, variables, classes, ... It also allows retrieval of documentation strings.

 CL-USER 83 > (defun add (a b)
                "add two numbers a and b"
                (+ a b))
 ADD

 CL-USER 84 > (documentation 'add 'function)
 "add two numbers a and b"

 CL-USER 85 > (defvar *color* :green "the output color symbol")
 *COLOR*

 CL-USER 86 > (documentation '*color* 'variable)
 "the output color symbol"

 CL-USER 87 > (defclass person ()
                ((age :documentation "the age of the person"))
                (:documentation "the person class"))
 #<STANDARD-CLASS PERSON 402005BCBB>

 CL-USER 88 > (documentation 'person 'type)
 "the person class"

## C#
This documentation method will work perfectly with the built in object browser in Visual Studio. The object browser can then show detailed information for each component. To make documentation that can be parsed, a triple slash (///) must be used. Further information can be found in [http://aspalliance.com/696 this tutorial]. A list of available xml elements for use in documentation [http://aspalliance.com/696_Code_Documentation_in_NET.2 is here].


```csharp>/// <summary

/// The XMLSystem class is here to help handle XML items in this site.
/// </summary>
public static class XMLSystem
{
    static XMLSystem()
    {
        // Constructor
    }

    /// <summary>
    /// A function to get the contents of an XML document.
    /// </summary>
    /// <param name="name">A valid name of an XML document in the data folder</param>
    /// <returns>An XmlDocument containing the contents of the XML file</returns>
    public static XmlDocument GetXML(string name)
    {
        return null;
    }
}
```



## D

D compiler comes with a builtin documentation system called [http://digitalmars.com/d/1.0/ddoc.html Ddoc]. Alternative systems may be used (a common alternative is [http://www.stack.nl/~dimitri/doxygen/ Doxygen] which includes some D support).

```d
/**
This is a documentation comment for someFunc and someFunc2.
$(DDOC_COMMENT comment inside a documentation comment
(results in a HTML comment not displayed by the browser))

Header:
    content (does not need to be tabbed out; this is done for clarity
    of the comments and has no effect on the resulting documentation)

Params:
    arg1 = Something (listed as "int <i>arg1</i> Something")
    arg2 = Something else

Returns:
    Nothing

TODO:
    Nothing at all

BUG:
    None found
*/
void someFunc(int arg1, int arg2) {}

// This groups this function with the above (both have the
//  same doc and are listed together)
/// ditto
void someFunc2(int arg1, int arg2) {}

/// Sum function.
int sum(in int x, in int y) pure nothrow {
    return x + y;
}

// These unittests will become part of sum documentation:
///
unittest {
    assert(sum(2, 3) == 5);
}

/++ Another documentation comment +/
void main() {}
```



## Delphi

XML comments are shown in the IDE and is included in the XML documentation produced by the compiler when using --docs.


```Delphi
type
  /// <summary>Sample class.</summary>
  TMyClass = class
  public
    /// <summary>Converts a string into a number.</summary>
    /// <param name="aValue">String parameter</param>
    /// <returns>Numeric equivalent of aValue</returns>
    /// <exception cref="EConvertError">aValue is not a valid integer.</exception>
    function StringToNumber(aValue: string): Integer;
  end;
```



## E


E's documentation facilities outside of [[Comments#E|comments]] are directly inspired by [[wp:Javadoc|Javadoc]] and use a similar basic syntax; however, they are part of the language rather than being an idiom about using comments.

Objects, methods, and functions have doc-comments (much as in [[Python]] or [[Common Lisp]]); the syntax for them is <code>/** ... */</code> immediately preceding the definition of the object, method, or function. (Note that E does not recognize the <code>/*...*/</code> comment syntax in general.)

The documentation syntax is in some ways underdefined: there is no layer which discards the leading '*'s as in Javadoc, so the documentation string contains all leading indentation (which is problematic for the source prettyprinter), and there is no formal definition yet of the syntax used within the doc-comments.


```e
? /** This is an object with documentation */
> def foo {
>   # ...
> }
# value: <foo>

? foo.__getAllegedType().getDocComment()
# value: " This is an object with documentation "
```


Variables may not have doc-comments, because they are never part of the public interface of an object.


## Eiffel


The Eiffel language was designed with a goal of producing documentation directly from software text. Certain comments, contracts for publicly accessible routines, and the class invariant are all extracted into the documentation views of a class.

The following is a simple class written in Eiffel:


```eiffel
note
	description: "Objects that model Times of Day: 00:00:00 - 23:59:59"
	author: "Eiffel Software Construction Students"

class
    TIME_OF_DAY
create
    make

feature -- Initialization

    make
            -- Initialize to 00:00:00
        do
            hour := 0
            minute := 0
            second := 0
        ensure
            initialized: hour   = 0 and
                    minute = 0 and
                    second = 0
        end

feature -- Access

    hour:   INTEGER
            -- Hour expressed as 24-hour value

    minute: INTEGER
            -- Minutes past the hour

    second: INTEGER
            -- Seconds past the minute

feature -- Element change

    set_hour (h: INTEGER)
            -- Set the hour from `h'
        require
            argument_hour_valid: 0 <= h and h <= 23
        do
            hour := h
        ensure
            hour_set: hour = h
            minute_unchanged: minute = old minute
            second_unchanged: second = old second
        end

    set_minute (m: INTEGER)
            -- Set the minute from `m'
        require
            argument_minute_valid: 0 <= m and m <= 59
        do
            minute := m
        ensure
            minute_set: minute = m
            hour_unchanged: hour = old hour
            second_unchanged: second = old second
        end

    set_second (s: INTEGER)
            -- Set the second from `s'
        require
            argument_second_valid: 0 <= s and s <= 59
        do
            second := s
        ensure
            second_set: second = s
            hour_unchanged: hour = old hour
            minute_unchanged: minute = old minute
        end

feature {NONE} -- Implementation

    protected_routine
            -- A protected routine (not available to client classes)
            -- Will not be present in documentation (Contract) view
        do

        end

invariant

    hour_valid:   0 <= hour   and hour   <= 23
    minute_valid: 0 <= minute and minute <= 59
    second_valid: 0 <= second and second <= 59

end -- class TIME_OF_DAY
```


Below is the '''Contract''' (documentation) view of the same class. It is the view that potential reuse consumers would reference when writing clients.


```eiffel
note
	description: "Objects that model Times of Day: 00:00:00 - 23:59:59"
	author: "Eiffel Software Construction Students"

class interface
    TIME_OF_DAY

create
    make

feature -- Initialization

    make
            -- Initialize to 00:00:00
        ensure
            initialized: hour = 0 and minute = 0 and second = 0

feature -- Access

    hour: INTEGER_32
            -- Hour expressed as 24-hour value

    minute: INTEGER_32
            -- Minutes past the hour

    second: INTEGER_32
            -- Seconds past the minute

feature -- Element change

    set_hour (h: INTEGER_32)
            -- Set the hour from `h'
        require
            argument_hour_valid: 0 <= h and h <= 23
        ensure
            hour_set: hour = h
            minute_unchanged: minute = old minute
            second_unchanged: second = old second

    set_minute (m: INTEGER_32)
            -- Set the minute from `m'
        require
            argument_minute_valid: 0 <= m and m <= 59
        ensure
            minute_set: minute = m
            hour_unchanged: hour = old hour
            second_unchanged: second = old second

    set_second (s: INTEGER_32)
            -- Set the second from `s'
        require
            argument_second_valid: 0 <= s and s <= 59
        ensure
            second_set: second = s
            hour_unchanged: hour = old hour
            minute_unchanged: minute = old minute

invariant
    hour_valid: 0 <= hour and hour <= 23
    minute_valid: 0 <= minute and minute <= 59
    second_valid: 0 <= second and second <= 59

end -- class TIME_OF_DAY
```


It is important to notice what is missing from this view. It shows only the specification of the class. The implementations of routines and any routines which are not public are not visible. The notes and comments, public features (including contracts for routines), and the class invariant all remain as components of the specification.

This view was produced by [[EiffelStudio]] which also supports an '''Interface''' view which is similar to the Contract view above, but also includes contracts for any inherited routines, and the complete inherited class invariant. EiffelStudio can publish these and other documentation view in HTML and other formats suitable for sharing.



## Elixir

Elixir uses [https://github.com/elixir-lang/ex_doc ExDoc] in order to document code. ExDoc allows users to pull data (such as name, version, source link) into their app. For example (taken from the ExDoc documentation):

```Elixir

def project do
  [app: :repo
   version: "0.1.0-dev",
   name: "REPO",
   source_url: "https://github.com/USER/REPO",
   homepage_url: "http://YOUR_PROJECT_HOMEPAGE"
   deps: deps]
end

```


Individual modules can be documented using the @moduledoc attribute and heredocs (with markdown):

```Elixir

defmodule MyModule do
  @moduledoc """
  About MyModule

  ## Some Header

      iex> MyModule.my_function
      :result
  """
end

```



## Emacs Lisp

In <code>defun</code>, <code>defmacro</code> and <code>defsubst</code> an optional docstring can follow the formal parameters.


```Lisp
(defun hello (n)
  "Say hello to the user."
  (message "hello %d" n))
```


For <code>defvar</code>, <code>defconst</code> or <code>defcustom</code> an optional docstring follows the value.


```Lisp
(defvar one-hundred 100
  "The number one hundred.")
```


Most other defining forms have similar optional docstrings.  In all cases they must be a string constant.

Docstrings have no effect on code execution but are available to the user or programmer in <code>C-h f</code> (<code>describe-function</code>) etc.  The docstring of a major mode function is its <code>C-h m</code> help (<code>describe-mode</code>).  The Emacs Lisp Reference Manual "Tips for Documentation Strings" describes various stylistic conventions.

The byte compiler (of Emacs 19.29 up) generates "dynamic docstrings" in the <code>.elc</code> files which means docstrings are not loaded into memory until actually viewed.


## Erlang

Erlang has Edoc. From the documentation: "EDoc is the Erlang program documentation generator. Inspired by the Javadoc(TM) tool for the Java(TM) programming language, EDoc is adapted to the conventions of the Erlang world, and has several features not found in Javadoc."

It is also possible to document functions and their arguments with type information. These can be used by Dialyzer. From the documentation: "The Dialyzer is a static analysis tool that identifies software discrepancies such as definite type errors, code which has become dead or unreachable due to some programming error, unnecessary tests, etc. in single Erlang modules or entire (sets of) applications."


## Factor

Factor has an extensive documentation system built in. See docs.factorcode.org : [http://docs.factorcode.org/content/article-writing-help.html]


## Fancy


```fancy
def class Foo {
  """
  This is a docstring. Every object in Fancy can have it's own docstring.
  Either defined in the source code, like this one, or by using the Object#docstring: method
  """
  def a_method {
    """
    Same for methods. They can have docstrings, too.
    """
  }
}
Foo docstring println # prints docstring Foo class
Foo instance_method: 'a_method . docstring println # prints method's docstring
```



## Fortran


### Inside the language


### =At run time=

Fortran offers no language facilities for including documentation within a programme's code. Being typically a compiled language, the details of the source file (and any documentation that it may contain) are long gone by the time the compiled code is executing. Unlike interpreted languages, it has no access to its "source code" form for self-inspection (or modification!) nor are facilities provided to attach auxiliary information to the components of a programme that might be reported at run time. A determined and diligent programmer ''could'' develop such a scheme but only by writing a lot of extra code without any special assistance from the language.

There are ''slight'' exceptions to the absence of source information in the code file. The B6700 compiler offered a pseudo text variable that contains the contents of columns 73-80 (the line sequence field) of the line in which it appeared that might be passed as a parameter to a routine reporting an error message, thereby identifying the location that recognised the error. This is not a standard feature! The standard NAMELIST facility does enable a code file to read or write selected variables "by name" in the format <''NameOfVariable''> = <''Value(s)Of Variable''> with a detailed form dependent on the type of the variable, and arrays have a repetition count if successive elements have equal values. This is useful when wishing to write out a complex state, or indeed to read in a complex set of data so as to supply parameters to a run without having to worry over their precise layout.

A compiler may produce a "symbol table" identifying the names of all routines and variables, and in the older days, their addresses so that when studying a memory dump there is some hope of discovering what went wrong. But "dump snuffling" is rare these days. Similarly, a run-time error such as divide-by-zero usually elicits a message giving the memory address of the divide instruction and the report may include some indication of the line number (or source sequence field) of the statement, plus the name of the routine in which it lay.

Put another way, reverse-compiling the code file will be unlikely to recover even the names of variables, let alone any documentation.


### =In the source file=

Thus, documentation depends on whatever the programmer sees fit to provide in the source file, possibly under protest. Originally, only "block" comments could be provided, signified by a C in column one (and in some extensions, a letter D in column one signified "debugging" statements that would be compiled in or not according to the setting of a compiler option), however this vertical alternation of layers of code and commentary can be obstructive to the reader. F90 standardised the "escape" comment style whereby outside a text literal, everything following an exclamation mark to the end of the line was regarded as a comment (B6700 Fortran used a %-symbol), thus code and commentary can be arranged in two non-clashing columns - especially if the source is listed via a reformatting system with that in mind. There is no comment start - comment stop facility as with {blah} in Pascal or /*blah*/ in pl/i, etc. and compilers may or may not recognise non-Fortran control lines such as $Omit or similar.

Fortran 90 also introduced the ability to specify INTENT IN, OUT, or INOUT for parameters to routines,  as well as the usual type indications. Its MODULE protocol includes an INTERFACE specification to assist in the description of collections of functions or subroutines with different types and numbers of parameters. These change what the compiler allows or rejects, but are also often important parts of documentation.

Otherwise, aside from separately-prepared files, the documentation resides in the source file, with layout, choice of names and organised design being the key and subject to style disputes and fashion. One special feature of Fortran is that statement labels can be used to identify logical blocks or sections in a way similar to the numbering of paragraphs in some documents. Labels might be incremented by 100 for sections, 10 for blocks within sections and 1 for stages within blocks - even if many such labels are never used in the code.


### Outside the language

Outside systems are available to assist. It is common for text editors to employ syntax highlighting, but their designers rarely handle the full complexity of Fortran syntax correctly. For instance, the format of a number such as -3.145E+07 is difficult to scan, especially since in Fortran source, spaces are ignored outside of text literals. Then, consider the FORMAT codes, such as <code>31E16.7</code> and similar sequences. More serious efforts scan the Fortran source in order to extract information useful for documentation. There have long been available systems that will identify every routine in a programme and for each identify what routine calls it, and what routines it calls, along with further analysis on their parameters and their usage - this well before F90 introduced the optional INTENT feature. Similarly, within each routine, identify which variables are used (possibly as parameters) where and whether they are read or written there. And, analyse the usage of COMMON variables... Likewise, documentation might include a flowchart, and systems exist to produce these as well.

Aside from the Fortran source code alone, there are systems that will also inspect the commentary associated with the code, and, if their conventions are followed (quite reasonable conventions, quoth the designer: declare ''and describe'' each parameter straight away, ''etc. etc.''), will produce for example a .html file with all manner of crosslinkages. For instance, given
```Fortran
      SUBROUTINE SHOW(A,N)   !Prints details to I/O unit LINPR.
        REAL*8 A    !Distance to the next node.
        INTEGER N   !Number of the current node.
```

Quite useful documentation could be extracted just from that. By mixing actual source statements with the commentary and extracting ''both'', inane clashes can be reduced, as in <code>CHARACTER*6 TLA    !Three-character name.</code> that arose because a modification to the nature of the parameter had been made, but, while revising every routine that employed such a parameter, the programmer's consciousness faded out as soon as it saw the comment start and noted "non-Fortran source", to the puzzlement of subsequent readers. A system that extracted only the commentary part from that would not give a correct report, whereas with it known that the actual declaration would be included, there would be no longer any need to repeat its details in the comment and the comment could confine itself to expounding on meaning and usage. On the other hand, F90 introduced a verbose scheme for declarations wherein something like <code>SELECTED_REAL_KIND(6, 70)</code> might appear and copying that may well merely copy obfuscation.

Similarly, there could be a convention in the file-naming scheme whereby a code file could gain access to its source file, and with further conventions put to use, it could recognise its own commands and messages and nearby find further text with explanations or examples, etc. or indeed find its own manual and print it on request. It is ''much'' better to have one file (and to maintain synchrony within it), than to have two separate files, possibly maintained by personnel in two different departments who speak different languages. An accomplished programme might even modify its own source code, say to maintain a version number - if the source code's value matches the compiled-in value then this is the first run of the new version, so increment the counter in the source code. By keeping track of the date and time, and the timestamp of each user's last use, then a user could be advised of changes to the system since their last use. Given that the update log is also within the source file and suitably identified and timestamped.

But none of this is within the Fortran language definition itself.


## Forth

Common comments in the source code is lines starting by "\ " do not forget the space

example :

<lang>\ this is a comment
```


Another good practice is describing the stack use of a function : function ( stack before -- stack at end )

example :

<lang>: cubed ( n -- n^3 ) dup dup * * ;
```



## FreeBASIC

Although FreeBASIC has several kinds of comments it does not currently support 'documentation comments' as such.

However, there is a third party tool called [http://users.freebasic-portal.de/tjf/Projekte/fb-doc/doc/html/PagIntro.html fb-doc] which is free software and acts as a bridge to existing C documentation generators such as gtk-doc and Doxygen.


## Go

Go source has both single line and block comments.

Godoc is an external command that extracts documentation from source code and outputs it as text or serves it as HTML.  Godoc extracts exported top-level declarations and comments that preceede them.  To be extracted, comments must immediately preceed the declaration with no intervening blank line.  Godoc may or may not reformat comments, depending on output options, but it does this in a general way and there are no markup rules to follow.

Example code demonstrating what godoc extracts and what it doesn't:

```go
// Example serves as an example but does nothing useful.
//
// A package comment preceeds the package clause and explains the purpose
// of the package.
package example

// Exported variables.
var (
    // lookie
    X, Y, Z int // popular names
)

/* XP does nothing.

Here's a block comment. */
func XP() { // here we go!
    // comments inside
}

// Non-exported.
func nonXP() {}

// Doc not extracted.

var MEMEME int
```

Text output of godoc:  (HTML looks nicer, of course)

```txt

PACKAGE

package example
import "."

Example serves as an example but does nothing useful.

A package comment preceeds the package clause and explains the purpose
of the package.


VARIABLES

var MEMEME int

var (
    // lookie
    X, Y, Z int // popular names
)
Exported variables.


FUNCTIONS

func XP()
 XP does nothing.

Here's a block comment.

```



## Gri

New commands can have optional help text between the command name line and the opening <code>{</code> brace.


```Gri
`My Hello Message'
Print a greeting to the user.
This is only a short greeting.
{
    show "hello"
}
```


Any literal braces in the text must be backslash escaped <code>\{</code>.  The text is shown by the online help system, similar to builtin commands.


```Gri>help My Hello Message</lang


(See section "Simple New Command" in the GRI manual.)



## Haskell

[http://haskell.org/haddock/ Haddock] is a popular documentation generator for the Haskell language.


```haskell
-- |This is a documentation comment for the following function
square1 :: Int -> Int
square1 x = x * x

-- |It can even
-- span multiple lines
square2 :: Int -> Int
square2 x = x * x

square3 :: Int -> Int
-- ^You can put the comment underneath if you like, like this
square3 x = x * x

{-|
  Haskell block comments
  are also supported
-}
square4 :: Int -> Int
square4 x = x * x

-- |This is a documentation comment for the following datatype
data Tree a = Leaf a | Node [Tree a]

-- |This is a documentation comment for the following type class
class Foo a where
    bar :: a
```


See [http://haskell.org/haddock/doc/html/markup.html this chapter] for more information about the markup.

== Icon and Unicon ==
=
## Icon
=
There are no formal conventions for documentation built into the Icon/Unicon. However, there are conventions for the different libraries that are supported by automation.

```Icon
############################################################################
#
#	File:     filename.icn
#
#	Subject:  Short Description
#
#	Author:   Author's name
#
#	Date:     Date
#
############################################################################
#
#   This file is in the public domain. (or other license)
#
############################################################################
#
#  Long form docmentation
#
############################################################################
#
#  Links:
#
############################################################################

procedure x1()    #: short description of procedure

```


*  [http://www.cs.arizona.edu/icon/library/src/progs/ipldoc.icn ipldoc.icn] Program to collect library documentation.<br/>
*  [http://www.cs.arizona.edu/icon/library/src/progs/iplindex.icn iplindex.icn] Program to produce indexed listing of the program library.


=
## Unicon
=

```Unicon>XYZ</lang

TBD


## J

Use [http://www.jsoftware.com/trac/base/browser/trunk/main/script/doc.ijs?rev=1 scripdoc]:

```j
NB.
### ===================================================

NB.*apply v apply verb x to y
apply=: 128!:2

NB.
### ===================================================

NB.*def c : (explicit definition)
def=: :

NB.*define a : 0 (explicit definition script form)
define=: : 0

NB.*do v name for ".
do=: ".

NB.*drop v name for }.
drop=: }.

   Note 1
Note accepts multi-line descriptions.
Definitions display the source.
)

   usleep
3 : '''libc.so.6 usleep > i i''&(15!:0) >.y'

```



## Java

Documentation is typically given using [http://www.j2ee.me/j2se/javadoc/index.jsp Javadoc] comments. Documentation is generated from these comments using the Javadoc tool. The default output (from the standard doclet) takes the form of an [[HTML]] page with frames, so HTML is frequently embedded in the comments. The comments all start with <code>/**</code>, end with <code>*/</code>, and usually have "tags" embedded starting with <code>@</code>.

```java
/**
 * This is a class documentation comment. This text shows at the top of the page for this class
 * @author Joe Schmoe
 */
public class Doc{
   /**
    * This is a field comment for a variable
    */
   private String field;

   /**
    * This is a method comment. It has parameter tags (param), an exception tag (throws),
    * and a return value tag (return).
    *
    * @param num a number with the variable name "num"
    * @throws BadException when something bad happens
    * @return another number
    */
   public int method(long num) throws BadException{
      //...code here
   }
}
```



## Julia

Julia has a built-in documentation system for functions, types, and similar user-defined entities.

Quoting the Julia documentation pages:

Any string appearing at the top-level right before an object (function, macro, type or instance) will be interpreted as documenting it (these are called docstrings). Note that no blank lines or comments may intervene between a docstring and the documented object. Here is a basic example:


```julia

"Tell whether there are too foo items in the array."
foo(xs::Array) = ...

```


Documentation is interpreted as Markdown, so you can use indentation and code fences to delimit code examples from text. Technically, any object can be associated with any other as metadata; Markdown happens to be the default, but one can construct other string macros and pass them to the @doc macro just as well.

Here is a more complex example, still using Markdown:


```julia

"""
    bar(x[, y])

Compute the Bar index between `x` and `y`. If `y` is missing, compute
the Bar index between all pairs of columns of `x`.



# Examples
```julia-repl
julia> bar([1, 2], [1, 2])
1
```
"""
function bar(x, y) ...

```
When running Julia from the REPL, functions and types documented with docstrings have online help accessible with the ? key, just like Julia's builtin functions.


## Kotlin

Kotlin uses a system of comments called KDoc to document source code. This is similar to Java's Javadoc though there are some additional block tags to support Kotlin-specific constructs.

Inline markup uses an extended form of the Markdown syntax.

The documentation is generated using a tool called Dokka.

The following is a slightly extended version of the example in the official Kotlin documentation:

```scala
/**
 * A group of *members*.
 * @author A Programmer.
 * @since version 1.1.51.
 *
 * This class has no useful logic; it's just a documentation example.
 *
 * @param T the type of a member in this group.
 * @property name the name of this group.
 * @constructor Creates an empty group.
 */
class Group<T>(val name: String) {
    /**
     * Adds a [member] to this group.
     * @throws AddException if the member can't be added.
     * @return the new size of the group.
     */
    fun add(member: T): Int { ... }
}
```



## Logtalk

Logtalk provides a set of entity and predicate documenting directives. The content of these and other directives can be retrieved using the language reflection features. A companion tool, "lgtdoc", uses the reflection API to extract the documenting information and export it as XML files and provides a set of stylesheets to convert these file to e.g. HTML and PDF files.


```logtalk

:- object(set(_Type),
	extends(set)).

	% the info/1 directive is the main directive for documenting an entity
	% its value is a list of Key-Value pairs; the set of keys is user-extendable
	:- info([
		version is 1.2,
		author is 'A. Coder',
		date is 2013/10/13,
		comment is 'Set predicates with elements constrained to a single type.',
		parnames is ['Type']
	]).

	% the info/2 directive is the main directive for documenting predicates
	% its second value is a list of Key-Value pairs; the set of keys is user-extendable
	:- public(intersection/3).
	:- mode(intersection(+set, +set, ?set), zero_or_one).
	:- info(intersection/3, [
		comment is 'Returns the intersection of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Intersection']
	]).

	...

:- end_object.

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica inline documentation can be accessed directly in the code by pressing F1. No external tools required

```Mathematica
f[x_,y_] := x + y (* Function comment : adds two numbers *)
f::usage = "f[x,y] gives the sum of x and y"

?f
-> f[x,y] gives the sum of x and y
```


=={{header|MATLAB}} / {{header|Octave}}==
Script files can contain comments (being whatever follows a % character) and for a script in file Gnash.m that is in the current execute path (so that typing "Gnash" will initiate it), the command "help Gnash" will instead display the first comment block at the start of the file. So, if it starts
```MATLAB
function Gnash(GnashFile);	%Convert to a "function". Data is placed in the "global" area.
%   My first MatLab attempt, to swallow data as produced by Gnash's DUMP
%command in a comma-separated format. Such files start with a line
```

The comments will be revealed. However, a test with Octave elicited only the comment on the "function" line. If the file contained merely a sequence of commands (i.e. did not constitute the definition of a function) the first line of the above example would be absent and the file would start with a comment block.


## Neko

'''nekoc -doc <file>''' generates XHTML documentation from within source.  The compiler scans for
```ActionScript
/** ... **/
```
 multi line comments.

Special
```html><doc> ... </doc></lang
 tags can be used inside these documentation blocks and are parsed as XML nodes; and checked for valid XHTML.  Outside of the doc tags, the documentation parser can be used to create API documentation.

The feature does not care what file type is used for input, and most C sources for Neko libraries include documentation blocks of this type.


```C
/**
        result_set_conv_date : 'result -> function:1 -> void
        <doc>Set the function that will convert a Date or DateTime string
        to the corresponding value.</doc>
**/
```
 produces '''void result_set_conv_date('result, function:1)''' for the API documentation, along with the prose.  The syntax of the API style documentation is picky.


## Nim


```nim
## Nim directly supports documentation using comments that start with two
## hashes (##). To create the documentation run ``nim doc file.nim``.
## ``nim doc2 file.nim`` is the same, but run after semantic checking, which
## allows it to process macros and output more information.
##
## These are the comments for the entire module.  We can have long descriptions
## here. Syntax is reStructuredText. Only exported symbols (*) get
## documentation created for them.
##
## Here comes a code block inside our documentation:
##
## .. code-block:: nim
##   var inputStrings : seq[string]
##   newSeq(inputStrings, 3)
##   inputStrings[0] = "The fourth"
##   inputStrings[1] = "assignment"
##   inputStrings[2] = "would crash"
##   #inputStrings[3] = "out of bounds"

type TPerson* = object
  ## This type contains a description of a person
  name: string
  age: int

var numValues*: int ## \
  ## `numValues` stores the number of values

proc helloWorld*(times: int) =
  ## A useful procedure
  for i in 1..times:
    echo "hello world"
```


=={{header|Objective-C}}==
Common tools include [Doxygen http://doxygen.org], [HeaderDoc http://developer.apple.com/mac/library/documentation/DeveloperTools/Conceptual/HeaderDoc/intro/intro.html] and [http://www.gnustep.org/resources/documentation/Developer/Base/ProgrammingManual/manual_9.html GSDoc]. The use of doxygen is much like in [[C]].


### HeaderDoc


```objc
/*!
 @function add
 @abstract Adds two numbers
 @discussion Use add to sum two numbers.
 @param a an integer.
 @param b another integer.
 @return the sum of a and b
 */
int add(int a, int b) {
    return a + b;
}
```



## Objeck

Documentation can be produced for bundles, classes and methods/functions.


```objeck
#~
Sets the named row value
@param name name
@param value value
@return true of value was set, false otherwise
~#
method : public : Set(name : String, value : String) ~ Bool {
  return Set(@table->GetRowId(name), value);
}

```



## OCaml

[http://caml.inria.fr/pub/docs/manual-ocaml/manual029.html OCamldoc] is a documentation generator that is included with OCaml. Documentation comments all start with <code>(**</code> (but no more than two asterisks) and end with <code>*)</code>. Like Javadoc, "tags" can be embedded starting with <code>@</code>.


## PARI/GP

The primary method for documenting GP functions is the <code>addhelp()</code> command:

```parigp
addhelp(funcName, "funcName(v, n): This is a description of the function named funcName.");
```

PARI documentation is done as for [[#C|C]].


## Perl

Perl's documentation mechanism is called [http://perldoc.perl.org/perlpod.html POD (Plain Old Documentation)]. Basically, any line that starts with an equal sign followed by a word (e.g. <tt>=head1</tt>) starts a documentation comment; and it lasts until a line that begins with <tt>=cut</tt>. Many formatting commands are recognized; for example, a line beginning with <tt>=item</tt> starts a section for an item, text wrapped in <tt>I< ... ></tt> is italicized, text wrapped in <tt>C< ... ></tt> is formatted as code, etc. See the [http://perldoc.perl.org/perlpod.html perlpod] and [http://perldoc.perl.org/perlpodspec.html perlpodspec] manuals for more details about the format.


## Perl 6

Similarly to Perl 5, Perl 6 is documented using [http://perlcabal.org/syn/S26.html Pod] (a redesigned version of POD). However, it's not simply ignored by the parser as in Perl 5, it's an internal part of the language spec and can be used to attach documentation objects to almost any part of the code.


```perl6
#= it's yellow
sub marine { ... }
say &marine.WHY; # "it's yellow"

#= a shaggy little thing
class Sheep {
    #= not too scary
    method roar { 'roar!' }
}
say Sheep.WHY; # "a shaggy little thing"
say Sheep.^find_method('roar').WHY; # "not too scary"
```


A compiler has a built-in <code>--doc</code> switch that will generate documentation from a given source file.


## PHP

[http://www.phpdoc.org/ phpDocumentor] (phpdoc) is a documentor for PHP. The syntax is similar to Javadoc.


## PicoLisp

PicoLisp doesn't yet support inline documentation directly in the code. However, it has built-in runtime documentation via the '[http://software-lab.de/doc/refD.html#doc doc]' function. This requires no external tools, except that the interpreter must have been started in debug mode.

```PicoLisp
: (doc 'car)         # View documentation of a function

: (doc '+Entity)     # View documentation of a class

: (doc '+ 'firefox)  # Explicitly specify a browser
```



## PL/I

There is no provision for attaching documentation to functions, procedures or variables in the code, other than what may be written by the programmer as additional code, and if so, that would be without special support. The source file may contain documentation in the form of in-line comments, permitted wherever a space would appear (outside of text literals) and marked <code>/*...''comment''...*/</code> that may continue across as many lines as desired. There is no "nesting" of comments and an omitted */ might cause trouble. Being typically compiled code, such commentary is long gone when a programme is running, as is the source text. There is no provision for code examination similar to reverse compilation, so there is no way for a routine to identify its caller, or the line in the source file that is being executed. However, a programme might read its own source file and extract information therefrom. With suitable conventions, it could print a copy of its manual and so forth.

There are special functions that provide the date and time that might be of use for documentation. DATE || TIME might return something like "870304090130388" and yes, in the 80's it was still a two-digit year number. At compile time, say to determine the timestamp of the compilation as a part of version control and update logging, matters were a little more difficult because the results via the pre-processor were texts and not quoted. Using <code>"DATE"</code> won't work because that is a quoted literal of four letters. Rather than stammering """", confusion could be reduced by <code>TIMESTAMP = QUOTE || DATE || TIME || QUOTE</code> where QUOTE was a pre-processor variable that contained a quote character, stammered once. The pre-processor variable TIMESTAMP would be a text that started and ended with a quote, and could be used in normal source statements as such.


## PowerShell

PowerShell includes complete built-in help for comment-based help: Just run help about_comment_based_help.

```PowerShell

help about_comment_based_help

```



## PureBasic


```PureBasic
; This is a small demo-code to demonstrate PureBasic’s internal
; documentation system.

;- All Includes
; By starting the line with ‘;-‘ marks that specific line as a special comment,
; and this will be included in the overview, while normal comments will not.

IncludeFile "MyLibs.pbi"
IncludeFile "Combustion_Data.pbi"

;-
;- Start of functions and Macros
;- Engeneering stuff

; A small function to calculate gas properties
Procedure.f CalcR( p.f, V.f, T.f)
  ProcedureReturn p*V/T
EndProcedure

; Example of a Macro
; These are indicated by '+' in the overview
Macro HalfPI()
  (#PI/2)
EndMacro

;-
;- - - - - - - - - - -
;- IO-Functions

Procedure Write_and_Close( File, Text$)
  If IsFile(File)
    WriteString(File,Text$)
    CloseFile(file)
  EndIf
EndProcedure
```

This would as an example be shown in the code over view as the picture below.

[[File:PureBasic_Procedure_overview.png]]


## Python

A string literal which is the first expression in a class, function, or module definition is called a [http://docs.python.org/glossary.html#term-docstring docstring]. It can be subsequently accessed at runtime using the <code>.__doc__</code> attribute of the class, function, or module.


```python
class Doc(object):
   """
   This is a class docstring. Traditionally triple-quoted strings are used because
   they can span multiple lines and you can include quotation marks without escaping.
   """
   def method(self, num):
      """This is a method docstring."""
      pass
```


'''[http://docs.python.org/library/pydoc.html pydoc]''', a module of the Python standard library, can automatically generate documentation gleaned from docstrings of modules. The documentation can be presented as pages of text on a terminal; automatically served to a Web Browser; or saved as HTML. The command line pydoc command , in addition, can display docstring information in a TK based GUI browser.

The built-in help() function uses the pydoc module to display docstring information at the command prompt of the Python interactive shell.

```python>>>
 def somefunction():
	"takes no args and returns None after doing not a lot"


>>> help(somefunction)
Help on function somefunction in module __main__:

somefunction()
    takes no args and returns None after doing not a lot

>>>
```



### Sphinx

The [http://sphinx.pocoo.org/ Sphinx] Documentation generator suite is used to generate the [http://docs.python.org/whatsnew/2.6.html#new-documentation-format-restructuredtext-using-sphinx new Python documentation].


## R

R objects are documented in files written in "R documentation" (Rd) format, which is similar to LaTeX markup.  See Chapter 2 of [http://cran.r-project.org/doc/manuals/R-exts.pdf Writing R Extensions] for the full details.  Example function document files can be created using

```r
example(package.skeleton)
```

An example documentation file for a function 'f', taking arguments 'x' and 'y' is

```r
\name{f}
\Rdversion{1.1}
\alias{f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
f(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,y) x+y
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
```



## Racket


Racket documentation is written in an integrated documentation
domain-specific language called Scribble:


```racket

#lang scribble/manual
(require (for-label "sandwiches.rkt"))

@defproc[(make-sandwich [ingredients (listof ingredient?)])
         sandwich?]{
  Returns a sandwich given the right ingredients.
}

```


Programs can also be written with inline documentation using either the
<tt>scribble/srcdoc</tt> or <tt>scribble/lp</tt> (literate programming)
libraries.


## REBOL


```REBOL
REBOL [
    Title: "Documentation"
    URL: http://rosettacode.org/wiki/Documentation
	Purpose: {To demonstrate documentation of REBOL pograms.}
]

; Notice the fields in the program header. The header is required for
; valid REBOL programs, although the fields don't have to be filled
; in. Standard fields are defined (see 'system/script/header'), but
; I can also define other fields as I like and they'll be available
; there.

; This is a comment. The semicolon can be inserted anywhere outside of
; a string and will escape to end of line. See the inline comments
; below.

; Functions can have a documentation string as the first part of the
; argument definition block. Each argument can specify what types it
; will accept as well as a description. All typing/documentation
; entries are optional. Notice that local variables can be documented
; as well.

sum: func [
	"Add numbers in block."
	data [block! list!] "List of numbers to add together."
	/average "Calculate average instead of sum."
	/local
	i "Iteration variable."
	x "Variable to hold results."
] [
	x: 0  repeat i data [x: x + i]
	either average [x / length? data][x] ; Functions return last result.
]

print [sum [1 2 3 4 5 6] crlf  sum/average [7 8 9 10] crlf]

; The help message is constructed from the public information about
; the function. Internal variable information isn't shown.

help sum  print ""

; The source command provides the source to any user functions,
; reconstructing the documentation strings if they're provided:

source sum  print ""

; This is an object, describing a person, whose name is Bob.

bob: make object! [
	name: "Bob Sorkopath"
	age: 24
	hi: func ["Say hello."][print "Hello!"]
]

; I can use the 'help' word to get a list of the fields of the object

help bob  print ""

; If I want see the documentation or source for 'bob/hi', I have to
; get a little tricky to get it from the object's namespace:

x: get in bob 'hi  help x  print ""

probe get in bob 'hi
```


Output:


```txt
21
8.5

USAGE:
    SUM data /average

DESCRIPTION:
     Add numbers in block.
     SUM is a function value.

ARGUMENTS:
     data -- List of numbers to add together. (Type: block list)

REFINEMENTS:
     /average -- Calculate average instead of sum.

sum: func [
    "Add numbers in block."
    data [block! list!] "List of numbers to add together."
    /average "Calculate average instead of sum."
    /local
    i "Iteration variable."
    x "Variable to hold results."
][
    x: 0 repeat i data [x: x + i]
    either average [x / length? data] [x]
]

BOB is an object of value:
   name            string!   "Bob Sorkopath"
   age             integer!  24
   hi              function! Say hello.


USAGE:
    X

DESCRIPTION:
     Say hello.
     X is a function value.

func ["Say hello."][print "Hello!"]
```



## Retro

Retro allows for insertion of documentation blocks. The contents of these can be in any format desired, though the standard Retro system uses Restructured Text for all embedded and external documentation.


```Retro

doc{

### ==

Function: foo

### ==


Stack
----
a1 a2 - b

Usage
-----
Adds a1 to a2 returning b.
}doc

: foo ( aa-b ) + ;

```




## REXX


### version 1

One technique with REXX is to use in-line documentation as REXX supports the accessing of

the REXX program's source, which can include documentation if properly ''fenced''.


This particular example uses lowercase fences of:
:::*     '''<help> '''
:::*     '''</help>'''

to delineate the documentation.   But, any two unique strings could be used.


Note that the documentation is bracketed by the standard REXX comment delimiters to preserve program lexicographical integrity.

```rexx
/*REXX program illustrates how to display embedded documentation (help) within REXX code*/
parse arg doc                                    /*obtain (all) the arguments from C.L. */
if doc='?'  then call help                       /*show documentation if arg=a single ? */
/*■■■■■■■■■regular■■■■■■■■■■■■■■■■■■■■■■■■■*/
/*■■■■■■■■■■■■■■■■■mainline■■■■■■■■■■■■■■■■*/
/*■■■■■■■■■■■■■■■■■■■■■■■■■■code■■■■■■■■■■■*/
/*■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■here.■■■■■*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
help: ?=0;    do j=1  for sourceline();  _=sourceline(j)         /*get a line of source.*/
              if _='<help>'   then do;  ?=1;  iterate;  end      /*search for  <help>   */
              if _='</help>'  then leave                         /*   "    "   </help>  */
              if ?            then say _
              end   /*j*/
      exit                                       /*stick a fork in it,  we're all done. */
/*══════════════════════════════════start of the in═line documentation AFTER the  <help>
<help>
       To use the  YYYY  program, enter one of:

             YYYY  numberOfItems
             YYYY                            (no arguments uses the default)
             YYYY  ?                         (to see this documentation)


       ─── where:  numberOfItems             is the number of items to be used.

           If no  "numberOfItems"  are entered, the default of  100  is used.
</help>
════════════════════════════════════end of the in═line documentation BEFORE the </help> */
```



### version 2


```rexx
/* REXX ***************************************************************
* 13.10.2013 Walter Pachl  another way to show documentation
*                          no tags and good enough if only one documentation block
**********************************************************************/
beghelp=here()+1                  /* line where the docmentation begins
Documentation
any text explaining the program's invocation and workings
---
                                     and where it ends               */
endhelp=here()-2
If arg(1)='?' Then Do
  Do i=beghelp To endhelp
    Say sourceline(i)
    End
  Exit
  End
say 'the program would be here!'
Exit
here: return sigl            /* returns the invocation's line number */
```



## Ring


```ring

/*
Multiply two numbers
n1: an integer.
n2: an integer.
returns product of n1 and n2
 */
see mult(3, 5) + nl
func mult n1, n2
     return n1*n2

```



## Ruby

[http://www.ruby-doc.org/core/classes/RDoc.html RDoc] is the de-facto documentation tool that's bundled with Ruby distributions. From it's documentation:

:Rdoc is an application that produces documentation for one or more Ruby source files. We work similarly to JavaDoc, parsing the source, and extracting the definition for classes, modules, and methods (along with includes and requires). We associate with these optional documentation contained in the immediately preceding comment block, and then render the result using a pluggable output formatter.

RDoc produces output that looks like [http://www.ruby-doc.org/core-1.8.7/index.html this].

Ruby's documentation comments look like Perl's PODs. Translating Java's example:

```ruby
=begin rdoc
RDoc is documented here[http://www.ruby-doc.org/core/classes/RDoc.html].

This is a class documentation comment.  This text shows at the top of the page
for the class.

Comments can be written inside "=begin rdoc"/"end" blocks or
in normal '#' comment blocks.

There are no '@parameters' like javadoc, but 'name-value' lists can be written:
Author:: Joe Schmoe
Date:: today
=end

class Doc
  # This is a comment for a Constant
  Constant = nil

  # This is a method comment.  Parameters and return values can be named
  # with the "call-seq" directive.
  #
  # call-seq:
  #   a_method(first_arg, second_arg) -> return_value
  #
  def a_method(arg1, arg2='default value')
    do_stuff
  end

  # Class methods will be shown in a separate section of the generated documentation.
  def self.class_method
    Constant
  end
end

# :include:boilerplate.txt
```



## Rust


Rust code is documented using the <code>rustdoc</code> tool included with the compiler (typically invoked as <code>cargo doc</code>), which combines information from two sources: Automatic API documentation harvested from type signatures, and manual documentation provided by annotating the code with snippets of Markdown.

[https://doc.rust-lang.org/std/string/struct.String.html The Rust standard library documentation] serves as an example of what locally generated documentation looks like, but many people rely instead on the [https://docs.rs/ docs.rs] service, which generates and hosts documentation for crates published on [https://crates.io/ crates.io].

The rust toolchain also takes responsibility for running code blocks within the documentation as part of the project's test suite, ensuring that they will remain up-to-date with the code they exercise.

Documentation annotations are [https://doc.rust-lang.org/rustdoc/the-doc-attribute.html implemented] as standard block attributes, but the Rust toolchain allows them to take four forms:

* <code>#[doc = "string"]</code> and <code>#![doc = "string"]</code> define a documentation string using Rust's standard attribute syntax. The former syntax documents the function, struct, or other object immediately following, while the latter syntax is used to document the module it appears within.
* <code>#[doc(...)]</code> and <code>#![doc(...)]</code> are another variant of of the standard Rust attribute syntax, which are used to specify metadata such as the favicon URL to use when generating HTML documentation and whether an item should be excluded from the docs.
* <code>/// string</code> and <code>/** string */</code> are syntactic sugar for <code>#[doc = "string"]</code> and are the typical way to document everything except modules corresponding to files.
* <code>//! string</code> and <code>/*! string */</code> are syntactic sugar for <code>#![doc = "string"]</code> and are the typical way to document modules corresponding to files, as there is no way to place a documentation annotation before the beginning of a file.

Rustdoc will not generate documentation for private members of crates by default, but this can be changed by calling it with the <code>--document-private-items</code> flag. (For example, using <code>cargo doc --document-private-items</code> to generate documentation for the internals of a crate which generates an executable rather than a library.)

Here's an example of some heavily documented mock code:

```rust
//! Documentation for the module
//!
//! **Lorem ipsum** dolor sit amet, consectetur adipiscing elit. Aenean a
//! sagittis sapien, eu pellentesque ex. Nulla facilisi. Praesent eget sapien
//! sollicitudin, laoreet ipsum at, fringilla augue. In hac habitasse platea
//! dictumst. Nunc in neque sed magna suscipit mattis sed quis mi. Curabitur
//! quis mi a ante mollis commodo. Sed tincidunt ut metus vel accumsan.
#![doc(html_favicon_url = "https://example.com/favicon.ico")]
#![doc(html_logo_url = "https://example.com/logo.png")]

/// Documentation for a constant
pub const THINGY: u32 = 42;

/// Documentation for a Rust `enum` (tagged union)
pub enum Whatsit {
    /// Documentation for the `Yo` variant
    Yo(Whatchamabob),
    /// Documentation for the `HoHo` variant
    HoHo,
}

/// Documentation for a data structure
pub struct Whatchamabob {
    /// Doodads do dad
    pub doodad: f64,
    /// Whether or not this is a thingamabob
    pub thingamabob: bool
}

/// Documentation for a trait (interface)
pub trait Frobnify {
    /// `Frobnify` something
    fn frob(&self);
}

/// Documentation specific to this struct's implementation of `Frobnify`
impl Frobnify for Whatchamabob {
    /// `Frobnify` the `Whatchamabob`
    fn frob(&self) {
        println!("Frobbed: {}", self.doodad);
    }
}

/// Documentation for a function
///
/// Pellentesque sodales lacus nisi, in malesuada lectus vestibulum eget.
/// Integer rhoncus imperdiet justo. Pellentesque egestas sem ac
/// consectetur suscipit. Maecenas tempus dignissim purus, eu tincidunt mi
/// tincidunt id. Morbi ac laoreet erat, eu ultricies neque. Fusce molestie
/// urna quis nisl condimentum, sit amet fringilla nunc ornare. Pellentesque
/// vestibulum ac nibh eu accumsan. In ornare orci at rhoncus finibus. Donec
/// sed ipsum ex. Pellentesque ante nisl, pharetra id purus auctor, euismod
/// semper erat. Nunc sit amet eros elit.
pub fn main() {
    let foo = Whatchamabob{ doodad: 1.2, thingamabob: false };
    foo.frob();
}
```



## Scala

Excellent documentation about ScalaDoc is given [https://docs.scala-lang.org/style/scaladoc.html here in the Scala Style Guide.]

```Scala
/**
  * This is a class documentation comment. This text shows at the top of the page for this class
  *
  * @author Joe Schmoe
  */
class Doc {
  /**
    * This is a field comment for a variable
    */
  private val field = 0

  /**
    * This is a method comment. It has parameter tags (param) and a return value tag (return).
    *
    * @param num a number with the variable name "num"
    * @return another number
    */
  def method(num: Long): Int = {
    //...code here
    ???
  }
}
```


## Smalltalk

(Squeak/Pharo)

```smalltalk
FooClass comment: 'This is a comment ....'.
```



## SQL PL

```sql pl

CREATE TABLESPACE myTs;

COMMENT ON TABLESPACE myTs IS 'Description of the tablespace.';

CREATE SCHEMA mySch;

COMMENT ON SCHEMA mySch IS 'Description of the schema.';

CREATE TYPE myType AS (col1 int) MODE DB2SQL;

COMMENT ON TYPE mytype IS 'Description of the type.';

CREATE TABLE myTab (
  myCol1 INT NOT NULL,
  myCol2 INT
);

COMMENT ON TABLE myTab IS 'Description of the table.';
COMMENT ON myTab (
  myCol1 IS 'Description of the column.',
  myCol2 IS 'Description of the column.'
);

ALTER TABLE myTab ADD CONSTRAINT myConst PRIMARY KEY (myCol1);

COMMENT ON CONSTRAINT myTab.myConst IS 'Description of the constraint.';

CREATE INDEX myInd ON
  myTab (myCol2);

COMMENT ON INDEX myInd IS 'Description of the index.';

-- V11.1
CREATE USAGE LIST myUsList FOR TABLE myTab;

COMMENT ON USAGE LISTmyUsList IS 'Description of the usage list.';

/**
 * Detailed description of the trigger.
 */
CREATE TRIGGER myTrig
  AFTER INSERT ON myTab
  REFERENCING NEW AS N
  FOR EACH ROW
 BEGIN
 END;

COMMENT ON TRIGGER myTrig IS 'General description of the trigger.';

CREATE VARIABLE myVar INT;

COMMENT ON VARIABLE myVar IS 'General description of the variable.';

/**
 * General description of the function (reads until the first dot).
 * Detailed description of the function, until the first empty line.
 *
 * IN VAR1
 *   Description of IN parameter in variable VAR1.
 * OUT VAR2
 *   Description of OUT parameter in variable VAR2.
 * INOUT VAR3
 *   Description of INOUT parameter in variable VAR3.
 * RETURNS Description of what it returns.
 */
CREATE FUNCTION myfun (
  IN VAR1 INT,
  OUT VAR2 INT,
  INOUT VAR3 INT)
  RETURNS INT
 BEGIN
 END;

/**
 * General description of the procedure (reads until the first dot).
 * Detailed description of the procedure, until the first empty line.
 *
 * IN VAR1
 *   Description of IN parameter in variable VAR1.
 * OUT VAR2
 *   Description of OUT parameter in variable VAR2.
 * INOUT VAR3
 *   Description of INOUT parameter in variable VAR3.
 */
CREATE PROCEDURE myProc (
  IN VAR1 INT,
  OUT VAR2 INT,
  INOUT VAR3 INT)
 BEGIN
 END;

CREATE MODULE myMod;

COMMENT ON MODULE myMod IS 'General description of the module.';

/**
 * General description of the procedure (reads until the first dot).
 * Detailed description of the procedure, until the first empty line.
 *
 * IN VAR1
 *   Description of IN parameter in variable VAR1.
 * OUT VAR2
 *   Description of OUT parameter in variable VAR2.
 * INOUT VAR3
 *   Description of INOUT parameter in variable VAR3.
 */
ALTER MODULE myMod
  ADD PROCEDURE myProc (
  IN VAR1 INT,
  OUT VAR2 INT,
  INOUT VAR3 INT)
 BEGIN
 END;

CREATE ROLE myRole;

COMMENT ON ROLE myRole IS 'Description of the role.';

CREATE SEQUENCE mySeq;

COMMENT ON ROLE mySeq IS 'Description of the sequence.';

```



## Stata

Documentation is written  in files with extension .sthlp, in [https://www.stata.com/help.cgi?smcl Stata Markup and Control Language].

For instance, say we have an ado file '''hello.ado''':


```stata
prog def hello
	di "Hello, World!"
end
```


The documentation file, '''hello.sthlp''', could be:


```txt
{smcl}
{title:Syntax}

{p 8 14 2}
{cmd:hello}

{title:Description}

{pstd}
{cmd:hello} will print the infamous message "Hello, World!" to Stata Results window.
```


Put these two file in a directory visible in the ado path. Then call the command by typing <code>hello</code>, and open its help file by typing <code>help hello</code>.

See also the documentation of the [https://www.stata.com/help.cgi?help help command].


## Swift

Swift uses reStructuredText. [http://nshipster.com/swift-documentation/ This third-party article] has some information.

```swift
/**
 Adds two numbers
 :param: a an integer.
 :param: b another integer.
 :returns: the sum of a and b
 */
func add(a: Int, b: Int) -> Int {
    return a + b
}
```



## Tcl

Documentation for Tcl code is usually prepared in separate files using a tool like [http://wiki.tcl.tk/3054 doctools], but inline docs (with all their inherent limitations) can be done with the likes of [http://www.xs4all.nl/~rfsber/Robo/index.html Robodoc]. For example:

```tcl
#****f* RosettaCode/TclDocDemo
# FUNCTION
#    TclDocDemo is a simple illustration of how to do documentation
#    of Tcl code using Robodoc.
# SYNOPSYS
#    TclDocDemo foo bar
# INPUTS
#    foo -- the first part of the message to print
#    bar -- the last part of the message to print
# RESULT
#    No result
# NOTES
#    Prints a message based on a template by filling in with the
#    supplied strings.
#*****
proc TclDocDemo {foo bar} {
    puts [format "%s -- %s" $foo $bar]
}
```

Both doctools and robodoc can produce output in multiple formats.

## ZX Spectrum Basic


Inline documentation is limited to comments within the code. However, there is no inbuilt extraction tool, so this would need to be written. The example shows how documentation for a variable and a function could be embedded within the code:


```zxbasic
 10 LET a=10: REM a is the number of apples
1000 DEF FN s(q)=q*q: REM s is a function that takes a single numeric parameter and returns its square
```


Alternatively, it is possible to embed documentation into LPRINT statements within a subroutine, allowing the language to print its own documentation. In the example below, the command GOSUB 2000 would print the documentation:


```zxbasic
10 GOSUB 2000: REM call a subroutine to print the documentation
1999 STOP
2000 REM Print the documentation
2010 LPRINT "a is the number of apples"
2020 LPRINT "s is a function that takes a single numeric parameter and returns"
2030 LPRINT "its square"
2040 RETURN
```



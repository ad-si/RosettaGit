+++
title = "Start from a main routine"
description = ""
date = 2019-08-10T11:12:16Z
aliases = []
[extra]
id = 10077
[taxonomies]
categories = ["task"]
tags = []
+++

Some languages (like Gambas and Visual Basic) support two startup modes.   Applications written in these languages start with an open window that waits for events, and it is necessary to do some trickery to cause a main procedure to run instead.   Data driven or event driven languages may also require similar trickery to force a startup procedure to run.


## Task

Demonstrate the steps involved in causing the application to run a main procedure, rather than an event driven window at startup.

Languages that always run from main() can be omitted from this task.





## Ada


In Ada, the "Main" procedure doesn't have to follow a special naming scheme. Any parameterless procedure will do.


```ada
with Ada.Text_IO;
procedure Foo is
begin
   Ada.Text_IO.Put_Line("Bar");
end Foo;
```



## ALGOL 68

An Algol 68 program consists of a sequence of declarations and expressions. The expressionss are executed and so form the main program.

```algol68
BEGIN
    print( ( "Hello, World!", newline ) )
END
```



## AutoHotkey

AutoHotkey always starts at the top of the script. A main() function can be called from there, or event hooks set.


## AWK


The awk language is data driven. However, it does support the use of begin blocks, so we could use one of those to provide us with a main startup procedure:


```awk
BEGIN {
  # This is our main startup procedure
  print "Hello World!"
}
```



## C


Code execution in C always starts at main(). Macros make possible programs such as the one below, although it's not the best way to write C code.


```C

#include<stdio.h>

#define start main()

int start
{
	printf("Hello World !");
	return 0;
}

```



```txt

Hello World !

```



## Clojure

Use [http://leiningen.org/ Leiningen].  It will allow you to describe many aspects of your project, including which namespace's -main function it should invoke at startup.  When you use the 'lein new app' template, it will generate and configure a -main function for you.  You can edit the <tt>project.clj</tt> to modify <tt>:main</tt> if you wish to start from some other point.  For more details, read the [http://leiningen.org/#docs documentation].


## Component Pascal

In BlackBox Componente Builder any exported procedure without paramenter (command) can be invoked through commanders (CTRL-q ModuleName.command)

```oberon2

MODULE MainProcedure;
IMPORT StdLog;

PROCEDURE Do*;
BEGIN
	StdLog.String("From Do");StdLog.Ln
END Do;

PROCEDURE Main*;
BEGIN
	StdLog.String("From Main");StdLog.Ln
END Main;
END MainProcedure.


```

Execute:<br/>
^Q MainProcedure.Do<br/>
^Q MainProcedure.Main<br/>
Output: <br/>

```txt

From Do
From Main

```



## Erlang

When started Erlang enters a REPL (read-eval-print loop). To call a function called main in the module m you do: erl -run m main argument1 argument 2 ...


## Forth

Forth still runs the interpreter when given a file to include in order to compile the source, but you can avoid the interactive interpreter by invoking an entry point ("main") then calling BYE to exit.

```forth
include foo.fs
...
: main  ... ;

main bye
```


This pattern is also used (e.g. GNU Forth) to interpret a Forth snippet from the command line.


```forth
$ gforth -e "2 2 + . bye"
```


Furthermore, professional Forth systems like PFE and SwiftForth have a means to make a "turnkey" application which omits the interactive interpreter, suitable for installing on a third-party system.  The command would take an entry point and target executable name: 


```forth
' main turnkey app.exe
```



## FreeBASIC

FreeBASIC does not require an executable program to have a main() procedure. However, there's nothing to stop you creating one and then  calling it to start the program:


```freebasic
' FB 1.05.0 Win64

Sub main()
  Print "Hello from main!"
End Sub

main
Sleep
```


```txt

Hello from main!

```



## Gambas


In Gambas, to make an application startup from a main routine:

* Create a new module called MMain

* In the MMain module, create a public sub called Main as follows:


```gambas
PUBLIC SUB Main()
  ' This is the start of the program
END
```


* Right click the MMain module, then select Startup class from the context menu


## Go

In Go all executable programs must have a main package which includes a function called main() with no parameters nor return value.

However, execution doesn't necessarily begin with main() itself. If there are top-level functions called init() with no parameters nor return value, these are executed first in declaration order and they can call other functions including main() itself.

Here's an example which illustrates this behavior. Note that main() gets called twice, first by the second init() function and then automatically by the runtime.

In practice, init() functions are generally used to initialize top-level variables which cannot (or cannot easily) be initialized in situ rather than to pre-empt the main() function in this way. 

```go
package main

import "fmt"

var count = 0

func foo() {
    fmt.Println("foo called")
}

func init() {
    fmt.Println("first init called")
    foo()
}

func init() {
    fmt.Println("second init called")
    main()
}

func main() {
    count++
    fmt.Println("main called when count is", count)
}
```


```txt

first init called
foo called
second init called
main called when count is 1
main called when count is 2

```



## J


J, by default, starts an event loop.

If a file name is specified on the command line, that file is executed before dropping into the event loop.

Thus, if the script issues an exit command, that will happen before the event loop executes.

If you want the script to exit even when it hits an error, you can use an [http://www.jsoftware.com/help/dictionary/dx009.htm#26 immex phrase], which will be the first thing executed by the event loop, before it prompts.


## Julia

If the program, julia, is executed via the command line without a program filename argument, it will enter its REPL (Read–Evaluate-Print-Loop) by default, without executing any user code until that is entered via the REPL command line. If, instead, Julia is started with a Julia program filename as argument, it will execute that program and, after that program terminates, exit without seeking any REPL input.


## Kotlin

The version of Kotlin which targets the JVM always starts from the main(args: Array<String>) function unless it is running in REPL mode when it simply executes lines of executable code in the order presented. The REPL is started by typing, kotlinc, without any parameters at the command prompt. For example:

```txt

Welcome to Kotlin version 1.1.1 (JRE 1.8.0_121-b13)
Type :help for help, :quit for quit
>>> println("Look no main!")
Look no main!
>>> :quit

```



## Logtalk

Logtalk applications don't run a main procedure at startup by default but there are several ways to accomplish that. One of them is to include an initialization/1 in a source file. The argument of this directive is a goal that will be proved when the source file is compiled and loaded. For example:

```logtalk

:- initialization(main).

```

The initialization/1 can also be used within entities (objects and categories) to automatically run entity-specific initializations when the container source file is compiled and loaded. In alternative, it's usually possible to pass a goal as a command-line argument when running the application executable (the details depending on the backend Prolog compiler being used).


## Mathematica

Mathematica automatically starts a REPL (read-eval-print loop), which is a kind of event loop. If that is not desired, pass -run on the command line. Note that if Mathematica is called via MathLink from within an external program, then the main loop has to be defined, which will usually differ from the standard one.


## Oforth


If Oforth loads a file and this file does not launch anything, nothing happens but file interpretation, and oforth leaves.

For instance if file1.of is : 

```Oforth
: main(n)
   "Sleeping..." println
   n sleep
   "Awake and leaving." println ;
```


```txt

>oforth file1.of

```

Nothing happens because oforth has nothing to perform.

If Oforth loads a file and this file launchs a function or method, it will be interpretred (and so, performed), and oforth leaves.

With this file (for instance file2.of) : 

```Oforth
: main(n)
   "Sleeping..." println
   n sleep
   "Awake and leaving." println ;

10000 main
```


```txt

>oforth file2.of
Sleeping...
Awake and leaving.

```

The function is performed.

Another way is to load a file and to give what to run into the command line parameters. For instance, using file1.of
```txt

>oforth --P"10000 mysleep" file1.of
Sleeping...
Awake and leaving.

```


Of course, "main" as function name to launch is not required : every name is ok.


## PARI/GP

GP scripts start from the top of the script. PARI code starts from the main function.


## Pascal

Pascal programs are mostly compiled and run a 'main' procedure automatically, this  procedure is not explicitly named 
main. Units (separate files of code without a main procedure) have initialisation code which also runs automatically.  


## Perl

Same as Perl 6.

```perl6
BEGIN {...} # as soon as parsed
CHECK {...} # end of compile time
INIT {...}  # beginning of run time
END {...}   # end of run time
```



## Perl 6

When executed with the standard setting, Perl 6 code always runs the mainline code automatically, followed by the <tt>MAIN</tt> function if you have one.  However, it's possible to start up with an alternate setting that might want to create its own event loop or <tt>MAIN</tt>.  In such cases you can
always capture control at various phases with blocks that we call "phasers":

```perl6
BEGIN {...} # as soon as parsed
CHECK {...} # end of compile time
INIT {...}  # beginning of run time
END {...}   # end of run time
```



## Phix

Any code which is not part of a routine is considered 'main' code. If you want a <code>main()</code> you have to explicitly invoke it.

```Phix
procedure main()
    ...
end procedure
main()
```



## PicoLisp

PicoLisp automatically starts a REPL (read-eval-print loop), which is a kind of event loop. If that is not desired, call (wait), or pass -wait on the command line. Per convention, the GUI event loop is started by calling (go), or by passing -go on the command line.


## PureBasic

PureBasic is procedural and any code which is not part of a procedure is considered 'main' code.  This code also does not use any explicit syntax (i.e. a 'main' module) to cause it to execute and it always executes first.


## Racket


Racket can be configured to run a REPL, run a main function, or just run top-level expressions.  A <tt>main</tt> function can be run by executing <tt>racket -tm program.rkt</tt>.


```racket

#/usr/bin/env racket -tm
#lang racket
(provide main)
(define (main . args) (displayln "Hello World!"))

```



## REXX

The closest REXX has to this type of behavior is when a REXX program starts, 

then executes (as per this discusion, say) an  XEDIT session, and 

then re-directs commands to the XEDIT session via the ADDRESS command.

XEDIT has native (built-in) support for the REXX language as a macro language. 

The XEDIT mentioned above runs on the VM/CMS operating system.

```rexx
/*REXX*/
address 'XEDIT'
  .
  .
  .
[XEDIT commands here.]
  .
  .
  .
```



## Ring


```ring

func Main
     see "Hello World!" + nl

```

Output:

```txt

Hello World!

```



## Ruby

Every Ruby source file can declare blocks of code to be run as the file is being loaded (the BEGIN blocks) and after the program has finished executing (the END blocks).
[http://ruby-doc.com/docs/ProgrammingRuby/html/language.html#UA BEGIN and END Blocks]

```ruby
BEGIN {
  # begin code
}

END {
  # end code
}
```

A program may include multiple BEGIN and END blocks. BEGIN blocks are executed in the order they are encountered. END blocks are executed in reverse order.


## Scala

### No kidding and trickery

In Scala there are two concepts not available in another OO-languages e.g. Java. The concepts are <code>object</code> and <code>trait</code>. Both cannot have parameters. <code>object</code>'s are singletons (one and only one instance of a parameter-less class) and are static. Exactly the same as a <code>main</code>. By use of the <code>trait App</code> a main method is preprogrammed and brought in an <code>object</code> which can be called on the command-line. There are more the one different <code>object</code>'s possible each can be called by his object name on the command-line. In the trait <code>executionStart</code> field is initialized with the starting time. By submitting an <code>-Dscala.time</code> argument on the command-line the execution time can be reported. The field <code>executionStart</code> can also programmatically used. 
```scala
object PrimaryMain extends App {
Console.println("Hello World: " + (args mkString ", "))
}

object MainTheSecond extends App {
Console.println("Goodbye, World: " + (args mkString ", "))
}
```



## sed

A <code>sed</code> program repeats itself for each line of input, but your program can begin with commands that address line 1. (This requires that your input has a line 1. If your input is empty file, like <code>/dev/null</code>, then it is impossible to run commands.)
```sed
# This code runs only for line 1.
1 {
	i\
Explain-a-lot processed this file and
	i\
replaced every period with three exclamation points!!!
	i\

}

# This code runs for each line of input.
s/\./!!!/g
```



## Seed7

Code execution in Seed7 always starts with main. This holds for text programs:


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln("hello world");
  end func;
```


And for grahical programs:


```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";
  include "keybd.s7i";

const proc: main is func
  begin
    screen(200, 200);
    KEYBOARD := GRAPH_KEYBOARD;
    ignore(getc(KEYBOARD));
  end func;
```



## Tcl

If a Tcl interpreter (such as [[tclsh]] or [[wish]]) is started without a script file, it will (typically) provide an interactive command prompt that supports read-eval-print-loop functionality. However, if a script file is supplied the file is executed and then the program either exits or, if [[Tk]] is in use, the application waits, servicing events, until the last window is deleted.


## Visual Basic


In Visual Basic to make an application startup from a main routine:

* Create a new module called MMain.bas

* Create a new subroutine in the new module as follows:


```vb
SUB Main()
  ' This is the start of the program
END
```


* From the menu in the application development environment, choose: File, Project Options.

* Ensure that MMain.bas is selected, by clicking in the list

* From the pulldown list, choose "Sub Main"

* Click the OK button.


## Visual Basic .NET


VB.NET console apps always start in Sub Main, but Windows Forms apps use the application framework by default, meaning the compiler generates a Sub Main that shows a selected startup form. To parse command-line parameters or cancel the application startup, either the appropriate methods in the MyApplication class can be overridden, or the application framework can be disabled, in which case it is allowed to set the startup object to be a method.


### MyApplication


Comments copied from the metadata of System.Windows.Forms.dll.

OnInitialize can cancel startup by returning false, and the handler for Startup can cancel startup by setting e.Cancel to True.


```vbnet
Imports System.Collections.ObjectModel
Imports Microsoft.VisualBasic.ApplicationServices

Namespace My
    ' The following events are available for MyApplication:
    ' Startup: Raised when the application starts, before the startup form is created.
    ' Shutdown: Raised after all application forms are closed.  This event is not raised if the application terminates abnormally.
    ' UnhandledException: Raised if the application encounters an unhandled exception.
    ' StartupNextInstance: Raised when launching a single-instance application and the application is already active. 
    ' NetworkAvailabilityChanged: Raised when the network connection is connected or disconnected.

    Partial Friend Class MyApplication
        '''<summary>Sets the visual styles, text display styles, and current principal for the main application thread
        '''(if the application uses Windows authentication), and initializes the splash screen, if defined.</summary>
        '''<param name="commandLineArgs">A <see cref="ReadOnlyCollection(Of T)" /> of <see langword="String" />,
        '''containing the command-line arguments as strings for the current application.</param>
        '''<returns>A <see cref="T:System.Boolean" /> indicating if application startup should continue.</returns>
        Protected Overrides Function OnInitialize(commandLineArgs As ReadOnlyCollection(Of String)) As Boolean
            Console.WriteLine("oninitialize; args: " & String.Join(", ", commandLineArgs))
            Return MyBase.OnInitialize(commandLineArgs)
        End Function

        ' WindowsFormsApplicationBase.Startup occurs "when the application starts".
        Private Sub MyApplication_Startup(sender As Object, e As StartupEventArgs) Handles Me.Startup
            Console.WriteLine("startup; args: " & String.Join(", ", e.CommandLine))
        End Sub

        '''<summary>Provides the starting point for when the main application is ready to start running, after the
        '''initialization is done.</summary>
        Protected Overrides Sub OnRun()
            Console.WriteLine("onrun")
            MyBase.OnRun()
        End Sub
    End Class
End Namespace
```


```txt
oninitialize; args: foo, /bar, baz, visual basic
startup; args: foo, /bar, baz, visual basic
onrun
```



### Sub Main


'''In Visual Studio:'''

In project properties, create a Sub Main of the appropriate signature, uncheck 'Enable application framework', and set 'Startup object' to 'Sub Main'.

'''By editing vbproj:'''

Change

```txt
<MyType>WindowsForms</MyType>
```

to

```txt
<MyType>WindowsFormsWithCustomSubMain</MyType>
```


and

```txt
<StartupObject>[DEFAULT NAMESPACE OF APPLICATION].My.MyApplication</StartupObject>
```

to

```txt
<StartupObject>Sub Main</StartupObject>
```


Main can choose to not call Application.Run, in which case no form is created.


```vbnet

Module Main
    Sub Main(args As String())
        Console.WriteLine("main; args:" & String.Join(", ", args))
        Application.Run(New Form1())
    End Sub
End Module
```


```txt
main; args:foo, /bar, baz, visual basic
```



## zkl

In zkl there is no main per se, the constructor of the top most enclosing class (usually the file as files define a class) is run. This remains the same even when a gui front end is pasted on.

file foo.zkl:

```zkl
"Hello".println()
```

forms an anonymous class whose constructor is a function with the above code. "zkl foo" does the expected.


## ZX Spectrum Basic


On the ZX Spectrum, there is no main function as such. However a saved program can be made to start running from a particular line number by providing the line number as a parameter to the save command. The following example will save the program in memory so that it starts running from line 500:


```zxbasic
SAVE "MYPROG" LINE 500: REM For a program with main code starting at line 500
```



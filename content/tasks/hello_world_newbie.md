+++
title = "Hello world/Newbie"
description = ""
date = 2019-10-05T18:03:22Z
aliases = []
[extra]
id = 10794
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Guide a new user of a language through the steps necessary
to install the programming language and selection of a [[Editor|text editor]] if needed,
to run the languages' example in the [[Hello world/Text]] task.
* Assume the language-newbie is a programmer in another language.
* Assume the language-newbie is competent in installing software for the platform.
* Assume the language-newbie can use one simple text editor for the OS/platform, (but that may not necessarily be a particular one if the installation needs a particular editor).
* Refer to, (and link to), already existing documentation as much as possible (but provide a summary here).
* Remember to state where to view the output.
* If particular IDE's or editors are required that are not standard, then point to/explain their installation too.


;Note<nowiki>:</nowiki>
* If it is more natural for a language to give output via a GUI or to a file etc, then use that method of output rather than as text to a terminal/command-line, but remember to give instructions on how to view the output generated.
* You may use sub-headings if giving instructions for multiple platforms.





## ALGOL 68

For information on installing and using algol68g, read [http://jmvdveer.home.xs4all.nl/algol68g.pdf Learning ALGOL 68 Genie] by Marcel van der Veer.

While algol68g is available for Mac OS X, this example currently does not provide instructions on running the "Hello, World" program on OS X.

1. [http://jmvdveer.home.xs4all.nl Download] algol68g.

2. Install algol68g on Linux, or unzip it to a convenient location on Windows.

3. Use your favorite [[Editor|text editor]].

3. Open the text editor and type the following:
```algol68
main: (
  printf($"Goodbye, World!"l$)
)
```


4. Save the file with the extension .a68.

5. Open a command prompt/terminal. Execute "a68g path/to/file/filename.a68" on Linux, or "path/to/a68g/a68g.exe path/to/file/filename.a68" on Windows.

Replace the paths above with their corresponding paths on your system.

The output will appear in the command prompt/terminal.


## ARM Assembly

{
   create file helloword.s
   compile it with : as -o helloword.o helloword.s
   link  it with   : ld -o helloword helloword.o -e main
   execute it      :  helloword
   }


```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program helloword.s   */
.data
szMessage: .asciz "Hello world. \n"
.equ LGMESSAGE, . -  szMessage  @ compute length of message
.text
.global main
main:
	mov r0, #1                  @ output std linux
    ldr r1, iAdrMessage         @ adresse of message
    mov r2, #LGMESSAGE          @ sizeof(message)
    mov r7, #4                  @ select system call 'write'
    swi #0                      @ perform the system call

    mov r0, #0                  @ return code
	mov r7, #1              @ request to exit program
    swi 0
iAdrMessage: .int szMessage
}

```



## AutoHotkey

The [http://ahkscript.org/docs/Tutorial.htm Tutorial and Overview], which is part of the [http://ahkscript.org/docs/AutoHotkey.htm AutoHotkey Documentation],
covers the basics of AutoHotkey.
The documentation is available online and is included as a .chm help file
with installation. Installation and "Hello World" are summarized here.

1) [http://ahkscript.org/download/ahk-install.exe Download] and install AutoHotkey.

2) Download and install the [http://fincs.ahk4.net/scite4ahk/ SciTE4AutoHotkey] editor (recommended), or use your favorite [[Editor|texteditor]].

3) Open the text editor and type the following:
```AutoHotkey
MsgBox, Hello World!
```


4) Save the file with the extension .ahk.

5) Double-click the file to launch it. A message box appears displaying "Hello World!" and an icon appears in the taskbar notification area.

6) To exit or edit the script, right-click the green "H" icon in the taskbar notification area. Closing the message box will also exit this particular script.


## AWK


###  AWK on Linux

AWK is a standard tool in Unix / most Linux-distributions.
So, it should be already installed.

To run awk, either provide code on shell (using single quotes).

 $ awk 'BEGIN{print "Goodbye, World!"}'

Or put code to file and load it from file.

 $ awk -f hello-world.awk


###  AWK on Windows

Select one of the [[:Category:AWK Implementations|awk-implementations]] to download.

E.g. [http://gnuwin32.sourceforge.net/packages.html gnuwin32.sourceforge.net] provides gawk, mawk and nawk.

The most current gawk is at [http://ftp.gnu.org/gnu/gawk gnu.org], but only as source.

To get a ready-made executable, look at [http://code.google.com/p/gnu-on-windows/downloads/list code.google.com] - gawk v4.1.0 currently.

Download, extract the executable file and "install" it.

In most cases, installation can be done by just
moving that exe-file to a convincent place,
e.g. a directory that is already in the PATH.

Otherwise, the PATH can be changed in the Windows-Systemsettings,
under environment-variables.

Alternatively, keep the awk-program in the same directory
as the scripts you are going to write / run.

Try to run it, from a commandline:
  awk -V
or
  mawk -W version
or
  gawk --version
to display its version-number.

To do a simple calculation, try a one-liner like this:
  awk "BEGIN{ print 17/4 }"

Note the use of double-quotes, and curly-braces.

Using the windows-commandline, single-quotes cannot be used.

That means one-liners using strings need awkward quoting:
  awk "BEGIN{print \"Hello\"}"
As a workaround, you can assign variables just before the script:
  awk  -v x="Hello"  "BEGIN{print x}"

So in most cases, running scripts from files is easier.
  awk -f hello.awk

Where hello.awk contains

```awk
BEGIN {
  print "Hello" 3-1 "U", sprintf("%c",33)
}
```

  Hello2U !

To edit awk-scripts use a [[Editor|texteditor]].

Now, look at some example-programs from category [http://rosettacode.org/wiki/Category:AWK AWK], for example: [[Read a file line by line#AWK|Read a file line by line]]


## Axe


Besides the built-in program editor on the TI-83/84, Axe requires a compiler app to be installed on the calculator. This is for development only, and is not required on user's calculators.

To install Axe, [http://www.ticalc.org/archives/files/fileinfo/456/45659.html download the latest version] and transfer it to the calculator.

Next, follow the instructions below in the [[#TI-83_BASIC|TI-83 BASIC]] section to create a sample program named MYPROGRM. The contents of the program should be:


```axe
PROGRAM:MYPROGRM
:.HELLO
:Disp "HELLO, WORLD!",i
```


Note that the first line contains the name of the program to be compiled. Also note that the i is the imaginary symbol, not the lowercase letter.

Then quit the program editor and press APPS. Go to the newly installed Axe app and select Options. Use the arrow keys to select the correct shell for your setup, then press Clear to go back.

Go to Compile, select MYPROGRM, and press Enter. Your program should compile without errors and will be saved in prgmHELLO. You can run this program from whichever shell you selected in the options (or from the home screen if you selected "no shell"). Run it as you would an assembly program.


## BASIC256

<h3>Installation</h3>
Download BASIC256 here: [http://sourceforge.net/projects/kidbasic/ http://sourceforge.net/projects/kidbasic/]

<h3>Interface</h3>
BASIC256 is an Integrated Development Environment (IDE). The code editor, the text output section, and the graphics output section are displayed at the same time.

<h3>HelloWorld Program (Text Output)</h3>
To display the "HelloWorld!" in the Text Output, copy this code and paste to the code editor:

```basic256
Print "HelloWorld!"
```

Then press F5 or click the play button.

<h3>HelloWorld Program (Graphics Output)</h3>
To display the "HelloWorld!" in the Graphics Output, copy this code and paste to the code editor:

```basic256
clg			# Clear the graphics screen
font "Arial",10,100	# Set the font style, size, and weight respectively
color black		# Set the color...
text 0,0,"HelloWorld!"	# Display in (x,y) the text HelloWorld!
```

Then press F5 or click the play button.

<h3>Documentation</h3>
Press F1 or click the ''Help'' menu for language help/documentations.



## Befunge

Befunge is a two-dimensional fungeoidal (in fact, the original fungeoid) esoteric programming language invented in 1993 by Chris Pressey with the goal of being as difficult to compile as possible. A Befunge program consists of a two-dimensional playfield of fixed size. The playfield is initially loaded with the instructions of the program. It also doubles as an updateable storage unit. Execution proceeds by the means of a program counter (-93) or instruction pointer (-98). The instruction pointer begins at a set location (the upper-left corner of the playfield) and is initially travelling in a set direction (right). As it encounters instructions, they are executed. The instructions may have an effect on the instruction pointer's direction and position (-98 only). Instructions may also affect the contents of a stack. See [http://esolangs.org/wiki/Befunge Befunge on Esolang] for more information and examples.

To start, you will need a Befunge interpreter. You can find one online, or download one.

What the end product will look like:

```Befunge
"!dlrow olleH">:#,_@
```


### Explanation


===="!dlrow olleH"====
You might be wondering why the text is backwards. The quotation marks (") toggle 'stringmode'. While in stringmode, all character's ASCII values are pushed onto the stack until the next quote. When the program deals with the stack, it starts with the right most value, therefor going through out message from right to left. The result after the second quotation mark should look something like this:

```txt
33,100,108,114,111,119,32,111,108,108,101,72
```

====>:#,_@====
The last portion of code prints our message. The underscore (_) works as a horizontal 'if' statement that pops the rightmost value off of the stack and checks if it is zero or not. If it is, the direction of the pointer is set to the right, ending the program with an 'at' sign (@) in this case. Otherwise, the pointer heads to the left.

Because of the way the 'if' statement works, there are some things we have to add to get our message out. Since the 'if' statement pops a value off of the stack, we need to copy what is currently there to make sure that we can still print it. That is what the colon (:) and the comma (,) do in our program. The colon duplicates the last value of the stack and the comma pops the last value and outputs it as an ASCII character. The pound sign (#) skips over the next command in the current direction. If the pointer is going to the right, we would skip the comma, to the left, the colon. We use this to control the flow of the program. Here is what the pointer would look like for one iteration of the program.

```txt
>:#,_@      Pointer goes to the right
^

>:#,_@      Duplicate
 ^

>:#,_@      Skip
  ^

>:#,_@      Pop and Check (72!=0), go to the left
    ^

>:#,_@      Print (72)
   ^

>:#,_@      Skip
  ^

>:#,_@      Pointer to the right (repeat until finished)
^
```

Once the program finishes, you should have:

```txt
Hello World!
```


## C


### Using gcc



### =Debian Based Systems=

Install <code>gcc</code>
 $ sudo apt-get install gcc


### =Red Hat Based Systems=

Install <code>gcc</code>
 $ su
 # yum install gcc


### =All Distributions=

After you have installed <code>gcc</code> using instructions above.
Create <code>helloworld.c</code>.
This uses <code>HERE</code> document and <code>bash</code>, the standard shell.
 $ cat > helloworld.c <<HERE
 #include &lt;stdio.h&gt;
 int main( int argc, char *argv[] ) {
      puts( "Hello World!" );
      return 0;
 }
 HERE
Compile it using <code>gcc</code>.
 $ gcc -o helloworld helloworld.c
Run it
 $ ./helloworld





## C++


Create text file <code>helloworld.cpp</code>

```cpp
#include <iostream>
int main() {
    using namespace std;
    cout << "Hello, World!" << endl;
    return 0;
}
```

Compile it using <code>gcc</code>:

 $ gcc -o helloworld helloworld.cpp

Run it:

 ./helloworld


## C#
C# is an ECMA-standardized language with open-source implementations, though development of the language is dominated by Microsoft.  C# compilers and IDEs are available on most operating systems.


### C# on Windows


Download the [https://www.visualstudio.com/vs/community/ Visual Studio Community Edition], which is free for personal use.  This is a standard Windows click-through installer.

Once installed (and after a restart), open Visual Studio.

Click on "New Project..." or do File > New > Project.

Select "Console Application."  Make sure Visual C# / Windows is selected in the left pane (because Visual Studio handles multiple languages and platforms).

Type a name for your application in the "Name" field.

Click OK.

Enter the Hello World code from the Hello World task into Program.cs.

Press the "Start" button or hit F5 to compile and run.


### C# on iOS


The [http://continuous.codes/ Continuous] App allows one to write, compile, and run C# code on an iOS device (iPad Pro recommended).


### C# on Linux



### =Mono=


Mono is the oldest effort to bring C# to Linux.

Install the mono-complete package.  Installation instructions are [http://www.mono-project.com/docs/getting-started/install/linux/ here].

Use the text editor or IDE of your choice to enter the Hello World program, saving it as Hello.cs.

To compile:

$ mcs hello.cs

To run:

$ ./hello.exe


### =.NET Core=


.NET Core is Microsoft's open-source implementation of C# and also part of the .NET libraries.

Microsoft's [https://www.microsoft.com/net/core#linuxredhat installation instructions for Linux] walk through installing .NET Core on a Linux system and also compiling and running a simple "Hello, World!" console application.

It does not come with an IDE, but consider [https://code.visualstudio.com/ Visual Studio Code.]


### C# on macOS



### =Xamarin Studio=


Xamarin Studio is an IDE combined with Mono as well as cross-platform development tools.  [https://developer.xamarin.com/guides/cross-platform/xamarin-studio/ Installation instructions are here.]


### =.NET Core=


.NET Core is Microsoft's open-source implementation of C# and also part of the .NET libraries.

Microsoft's [https://www.microsoft.com/net/core#macos installation instructions for macOS] walk through installing .NET Core on macOS and also compiling and running a simple "Hello, World!" console application.

It does not come with an IDE, but consider [https://code.visualstudio.com/ Visual Studio Code.]




## Clojure

# Install [http://leiningen.org/#install Leiningen].
# Generate a hello world app: <lang>$ lein new app my-hello
```

# Run it: <lang>$ cd my-hello
$ lein run
Hello, World!
```

# Edit the generated <tt>src/my-hello/core.clj</tt> file with your favorite [[Editor|texteditor]] to make changes.
# You don't have to edit files.  Run the REPL (interactive shell) to experiment: <lang>$ lein repl
```

# Suggested reading: [http://dev.clojure.org/display/doc/Getting+Started Clojure's Getting Started Guide] and [http://www.braveclojure.com/ Clojure for the Brave and True]


## COBOL

1) Install OpenCOBOL:
<lang>$ sudo apt-get install open-cobol
```

2) Open your [[Editor|texteditor]], and write the following code:

```cobol
program-id. hello-world.

procedure division.
  display "Hello, World!"

  goback
  .
```

3) Save the code to hello.cob, and compile it with OpenCOBOL. Note: if OpenCOBOL fails with the error 'unexpected end of file', add a trailing newline after the last period. Other likely errors may be caused by omitting the periods.
<lang>$ cobc -x -Wall -free ./hello.cob
```

4) Run the compiled program from the command line:
<lang>$ ./hello
Hello, World!
```


## Common Lisp

Common Lisp is a dynamically-typed, garbage-collected language whose features have inspired dozens of other languages including Python, Perl, Ruby, Smalltalk, and even Java. Lisp is the second oldest programming language still in use today after Fortran. Lisp has AST macros, which are only just starting to be experimented with in other languages such as Nemerle and BOO. AST macros allow the programmer to define new language constructs that seamlessly integrate with the built-in ones.

### Installation


### =Choosing an Implementation=

As a standardized language, ANSI Common Lisp has multiple implementations. [http://lispworks.com/ LispWorks] and [http://franz.com/products/allegrocl/ AllegroCL] are well-known commercial implementations, while [http://www.cliki.net/Common+Lisp+Implementation the list of free implementations] is extensive. Among the free implementations, [http://www.sbcl.org/ SBCL] is the most popular. It includes a native compiler that generates fast code, supports threads, and has a C foreign function interface. Common Lisp programs compiled with SBCL are generally competitive with Java programs.

### =Choosing an Editor=

The commercial implementations come with their own IDEs, while EMACS is the most popular editor to use for the free implementations. SLIME is an EMACS extension that provides integration between Lisp and EMACS. It enables you to evaluate the Lisp expression at the cursor, provides tooltips, debugging, and more. SLIMV is an extension for Vim that brings SLIME to that editor.

### =Editing Code=

The preferred way to develop Lisp programs is interactively. Evaluate each function, macro, and variable definition in the REPL as soon as you type it into your source file. You can also evaluate subexpressions, and switch to the REPL window to play around with your code as if your functions were shell scripts. Lisp programs are typically written from the bottom up, the idea being to create a language that is perfectly tailored to writing your application, and then writing your application in that language. Output or errors are displayed immediately in the REPL window.

### Hello World

'''hello.lisp'''
<lang "Common Lisp">
(format t "Hello world!~%")

```


### Running It

Exactly how to run it from the command line depends on your Lisp implementation. Of course, from the REPL you can always
run it by entering:
<lang "Common Lisp">
(load "hello.lisp")

```

...which is also how you load source files in order to add their definitions to your program.
SBCL has the <code>save-lisp-and-die</code> function, which saves whatever has been defined in the REPL into a stand-alone executable that includes a copy of SBCL itself (because Lisp programs can generate, compile, and run additional Lisp code at runtime, and then reference functions and variables created by that generated code).


## D

There is detailed information in [http://ddili.org/ders/d.en/hello_world.html The Hello World Program chapter] of [http://ddili.org/ders/d.en/index.html the online book Programming in D].

Here is a summary:

1) Download and install the latest dmd at [http://www.digitalmars.com/d/download.html dmd download page].

2) Open your favorite [[Editor|texteditor]], write the following source code


```d
import std.stdio;

void main()
{
    writeln("Hello world!");
}
```


3) Save the source code under the name hello.d

4) Open a console window and compile the source code by entering the following command


```txt

dmd hello.d

```


5) Start the program on the console by entering the following commandz


```txt

hello

```


The program should produce the following output:

```txt
Hello world!
```



## EchoLisp


###  Requirements and installation

You need a computer running an up-to-date browser, such as FireFox, Chrome, Opera, ...
No installation needed. To run EchoLisp, follow this link : [http://www.echolalie.org/echolisp EchoLisp].

###  Hello World


```lisp

;; This is a comment
;; Type in the following -uncommented- line in the input text area, and press [RETURN]
;; or click onto the "Eval" button
;; Auto-completion : You will notice that after "(di" , EchoLisp proposes "(display" :
;; Press the [TAB] key to accept

(display "Hello, World" "color:blue")

```

=== On-line help ===
The first thing to know are the "apropos", "usage", and "help" functions. Examples:

```lisp

;; usage  give the syntax(es) for a function call
;; "usage" abbreviation is "us"
(us display) → 📗 (display object css-style-string) (display object)

;; help opens the reference manual at the right place, in a browser tab
;; "help" abbreviation is "?"
(? display) → [http://www.echolalie.org/echolisp/help.html#display]

;;  searching
;; (apropos name) displays the list of functions about 'name'
;; "apropos" abbreviation is "ap"
;; 'special' forms (you will learn that later) are flagged with 👀
;; 1:2 is the number or arguments. min 1, max 2
(ap list) →  #(👀 for*/list:2:n  👀 for/list:2:n  alist?:1  circular-list:1:n  list->stack:2
 list->vector:1  list-index:2  list-ref:2  list-sort:2  list-tail:2  list:1:n  list?:1
 maplist:2  set-plist!:2  stack->list:1  stream->list:1:2  string->list:1  sublist:3
 symbol-plist:1  vector->list:1 )

;; Or you can press the "Help" button.
;; Lost in the reference manual ?
;; Just type-in a letter, and you will go to the alphabetical index.


```


###  Editing

Functions are provided to save data, functions definitions, in the browser local storage, without leaving the read-eval-print loop. However you can edit files, using any text file editor - UTF-8 compatible - and load/eval them with the "Load" button. TextWrangler is a good choice on Mac OS/X. It supports syntax hiliting for source lisp files. In any case, the EchoLisp team is ready to help you.


## EDSAC order code

If the year is 1950, first write to Dr Wilkes at St John's College asking him whether you can submit a job to the electronic brain. Assuming he is agreeable, copy out your program—legibly!—and take it round to the keypunch operator at the Mathematical Laboratory.

If it is the present day, Nishio Hirokazu's browser-based simulator allows you to run EDSAC programs without installing anything. Navigate to [http://nhiro.org/learn_language/repos/EDSAC-on-browser/index.html this page] and click the <tt>Source</tt> tab. Clear the text area (it will contain a 'Welcome' program) and type or paste your program. When you have finished, click the <tt>Machine</tt> tab and then the button marked <tt>Load source</tt>. You should see your program, stripped of comments, newlines, and whitespace, appear in the <tt>Input tape</tt>. Click <tt>Run</tt>. The machine will first execute the Initial Orders, loading your program into storage, and then run your program. The storage tanks are displayed down the left-hand side of the window: clicking on any tank allows you to watch that tank in a larger size.


## Eiffel


###  Example

# Download and install Eiffel GPL on your Windows computer.
# Open Eiffel, creating a new Basic command line project (follow the wizard).

The resulting program will be a simple "Hello World!" program:


```eiffel

class
   APPLICATION
create
   make
feature
   make
      do
         print ("Hello World!")
      end
end

```


Where the following notes apply:

*
```eiffel>class APPLICATION ... end</lang
 defines the simplest form of an Eiffel "class".
*
```eiffel>create make</lang
 defines a "creation procedure" (i.e. constructor) for the class <b>{APPLICATION}</b>.
*
```eiffel>make do ... end
```
 is the implementation of the constructor specified with the <b
`create'</b> keyword (above).
*
```eiffel
print ( ... )
```
 is a feature inherited from class <b>{ANY}</b>—a class from which all Eiffel classes inherit (e.g. like {APPLICATION}). In the example the
```eiffel>inherit ANY</lang
 has been left out as it is implied by default.


###  Supported Platforms

Windows, Linux, Mac, and others.

###  Supporting Notes

See [[http://www.eiffel.org| Eiffel-org]] for more information.
<p>An example of "Hello World!" is on the home page. You can try Eiffel by clicking the "Play with Eiffel" button on the "Hello World!" example on the home page.</p>
<p>You can find plenty of YouTube videos on the Eiffel Software [[https://www.eiffel.org/resources/videos| Eiffel Videos]] channel or [[http://www.youtube.com/user/ljr1981yt/videos| Eiffel Videos]] channel.</p>


## Erlang


###  Erlang on Linux


Here is a summary:

1) To install Erlang in Linux, open a terminal and run the command:

```txt

sudo apt-get install erlang

```


2) Open your [[Editor|texteditor]], write the following source code:


```erlang
% Implemented by Arjun Sunel
-module(helloworld).
-export([main/0]).

main() ->
    io:fwrite("Hello world!\n").
```


3) Save the source code under the name helloworld.erl

4) Open a terminal, hit enter after typing the following command.

```txt
erl
```


5) Compile the source code by entering the following command


```txt

c(helloworld).

```


6) Run the code by calling the main() function using the command:


```txt

helloworld:main().

```


The program should produce the following output:


```txt
Hello world!
ok
```



## Fortran



###  Linux

* install gfortran
For Debian-based GNU Linux OS:

```txt

sudo apt-get install gfortran

```

For RPM-based GNU Linux OS:

```txt

su -c "yum install gcc-gfortran"

```

or

```txt

sudo zypper in gcc-fortran

```

or

```txt

sudo rpm -ihv gcc-fortran

```

Note differences in package names that provide gfortran compiler in different Linux distros.

* Create hello.f90 source code file. You can also use any text-editor (ed, joe, pico, nano) but here is shown how to create such file directly in terminal using "cat" command. Open terminal and hit enter after typing the following command.

```txt

cat > hello.f90

```

* Type in the following Fortran source code

```txt

write(*,*) "Hello world!"
end

```

and finally hit Ctrl^D in terminal.
* Compile hello.f90 source code into executable hello.x binary using gfortran compiler

```txt

gfortran hello.f90 -o hello.x

```

* Run it

```txt

./hello.x

```



## FutureBasic



###  Requirements

Macintosh OS X v10.4 or newer

=== Download FutureBasic (FB) ===
FutureBasic is freeware and is commonly called simply "FB" by its developers. The lastest version of FutureBasic can be downloaded from: http://4toc.com/fb/index.htm. The FB site also contains installation instructions, example files, older versions, and other helpful information.


###  FB Support Group

An active list group of developers who are knowledgable, friendly and helpful to both seasoned and newcomer coders can be found at: http://freegroups.net/groups/futurebasic/. Answers to any of a host of questions about FB are answered quickly by this small, but dedicated group of developers.


###  FB on Wikipedia

The FB Wikipedia page describing FB's journey from one of earliest commercial Macintosh compilers to its current freeware status can be visited at: https://en.wikipedia.org/wiki/FutureBASIC. This page may not have the latest information about FB, so the authoritative source for the latest information is the FB web  site and list group listed above.


###  Your First Program

When you have downloaded and installed FB, you will want to compile your first program, the traditional "Hello, World!"

1. Launch FB and from the File menu select New File.

2. Name your file "Hello, World!" and save it to your Desktop.

3. In the window type the following:


```futurebasic

include "ConsoleWindow

print"Goodbye, World!"

```


4. From FB's Command menu, select Build and Run "Hello, World!"

Enjoy your first program!



## Go

Currently supported platforms are FreeBSD, Linux, Mac OS X, and Windows.
From the landing page http://golang.org click the blue box "Download Go"
(under the big gopher drawing.)
This takes you to [http://golang.org/doc/install Getting Started],
a fairly concise page that is very close to satisfying this task.

The first section, Download, has a link to a downloads page
but also mentions two other options, building from source and using GCC.
I personally like building from source and have found it usually goes without a hitch.  GCC isn't just C anymore and includes a number of language front ends.
Go is one of them.
There are links there to separate pages of instructions for building
from source and using GCC.

Continuing with instructions for the precompiled binaries though, there is a section "System Requirements" and then a section "Install the Go tools", which means tools including the Go compiler.  You need to follow some steps here.
Follow the instructions for your operating system.  (The steps are few, standard,
and easy.)  Pay attention to the paragraph "Installing to a custom location"
if you are installing on a system where you do not have root or sudo access.
Setting GOROOT as described is essential in this case.
If you are installing to the standard location, you should not set
this environment variable.
(Setting it when it doesn't need to be set can lead to problems.  Just don't.)

You're ready to test the installation with Hello World!
The RC Task mentions [[Editor|texteditor]]s.
You will want an editor that can edit Unicode UTF-8.
Go source is specified to be UTF-8 and before long you will want an editor
that does this.
This task is all ASCII however, and any editor that can save plain ASCII text
will do for the moment.
Actually you probably don't even need an editor.
From a Linux command line for example, you can type

```txt

$ cat >hello.go

```

Cut and paste the code from [[Hello_world/Text#Go]], press ^D,
and you should have the program.  To run it type

```txt

$ go run hello.go

```

This compiles to a temporary executable file and runs it,
displaying output right there in the same window.
If you want a copy to give to your friends,

```txt

$ go build hello.go

```

will create an executable called hello in the current directory.

This completes the RC task, but if at any point in the future
you will use Go for more than Hello World, you really really
should continue through the next section "Set up your work environment."
This covers setting GOPATH, which is essential to standard workflow with Go.


## Groovy

First you need to have a JRE or better a JDK 1.5+ installed on your machine.
Install Groovy following the instructions at: [http://groovy-lang.org/download.html Groovy Installation]

In your repository
Just type: (no class, no parentheses, no semicolon, no import)
<lang "Groovy">println 'Hello to the Groovy world'
```

and save it in hello.groovy
On the command line, just type

```txt
$> groovy hello.groovy
```

You will see the following message:

```txt
Hello to the Groovy world
```

Note you can also define a String to test the output message
example, in your hello.groovy file replace previous code by:
<lang "Groovy">String hello = 'Hello to the Groovy world'
println hello
```

And you can add some assertions, for instance:
<lang "Groovy">assert hello.contains('Groovy')
assert hello.startsWith('Hello')
```



## Haskell

Install GHC:
<lang>$ sudo apt-get install ghc
```

Create hello.hs:
<lang>$ touch hello.hs
$ cat > hello.hs << HERE
main = putStrLn "Hello, World!"
HERE
```

Compile it:
<lang>$ ghc hello.hs -o hello
```

And run the executable:
<lang>$ ./hello
Hello, World!
```


===Non-Linux operating systems===

If you are using another operating system (e. g. Mac OS X),
the easiest way to get Haskell is to install the [http://www.haskell.org/platform/ Haskell Platform].


## Java


```Java
public class HelloWorld {

    public static void main(String[] args) {
        System.out.println("Hello world!");
    }

}
```



## JavaScript

1.- go to the "Developer Console" part of your preferred web browser.

2.-you can choose between the console.log() method or the alert() method (suggestion:try both).

console.log() method:

<lang>console.log("Hello, World!");
```


this will be printed on the Console tab part of your web browser.

<lang>
Hello, World!

```


alert() method:

<lang>alert("Hello, World!");
```


this will appear in the shape of a 'pop-up' in your browser.

3.-There is no number 3, Happy Coding!



## J

Download J804 from http://jsoftware.com/stable.htm

Install it, using the defaults.

Run the program, and bring up the IDE.

Type in:

```j
'Goodbye, World!'
```



## jq

The official jq homepage at http://stedolan.github.io/jq has an introduction
and pointers to instructions on downloading jq; a tutorial; a manual;
an online jq interpreter called jqplay; and so on.

The website is well-organized and intended for newbies as described
on this page, and so the following will avoid repeating the same
information.  Instead we will explore the "Hello world!"
task in the context of both jqplay and the jq command.  As will
become evident, an editor is not always necessary.  In practice,
though, jq is typically used in conjunction with a text editor
as discussed in the penultimate subsection of this article, which concludes with a note on programming style.


### jqplay

jq is primarly intended for use as a command-line tool, but the
online tool at jqplay.org can also be used for simple tasks.
In fact, it is instructive to consider two approaches to
the "Hello world!" task using jqplay:

'''1) "Hello world!" as data'''

"Hello world!" can be regarded as a JSON string, and it can be entered in
the JSON input box.  The program for printing it is just "." (without the quotes).
That is, using jqplay, one simply enters . in the "Filter" box.

'''2) "Hello world!" as program'''

Since any JSON entity is a filter in jq, we can also
enter "Hello world!" in the Filter box.  In this case,
no additional data is required, and so the "Null input" box
can be checked.

### The jq command

With jq installed, we can explore the two approaches mentioned above
at the command prompt.

In the following we will use C:\ to signify the context is a Windows terminal,
and $ to signify that the context is Linux/Unix/Mac/Cygwin or similar.

'''1)  "Hello world!" as data'''

The data can come from stdin or from a file:

'''Data from stdin'''

```sh
$ echo '"Hello world!"' | jq .

C:\ echo "Hello world!" | jq .
```

If there were a web server somewhere that provides "Hello world!" as JSON,
then we could also write something like:

```sh
curl -Ss http://hello.world.com | jq .
```


'''Data from a file'''

```sh
$ echo '"Hello world!"' > hello.txt
$ jq . hello.txt

C:\ echo "Hello world!" > hello.txt
C:\ jq . hello.txt
```

'''2) "Hello world!" as program'''

```sh
$ jq -n '"Hello world!"'

C:\ jq -n "Hello world!
```

We could also put the program into a file. In this particular instance,
we could use the file hello.txt that was created above. In this
particular case also, the command is the same in all the environments
under consideration:

```sh
 jq -n -f hello.txt .
```

In practice, it is recommended that jq programs be placed in text files with names
ending with the .jq suffix.

### Editors

Any text editor can be used for JSON and jq programs.

An editor such as Emacs or Aquamacs is particularly helpful as these support
shell windows.  In addition, most distributions come with "js-mode" which can be
used for editing JSON, and there is also a "json-mode" package available at
https://github.com/joshwnj/json-mode.


### A note on jq programming style

jq programs consist of definitions and pipelines.  There is much to be said
for a style illustrated by the following example:

```jq
def binary_digits:
  if . == 0 then 0
  else [recurse( if . == 0 then empty else ./2 | floor end ) % 2 | tostring]
    | reverse
    | .[1:] # remove the leading 0
    | join("")
  end ;
```



## Julia


```txt

1. Use a web browser to go to http://juliabox.com. With email or Google signin, sign up for a free account.
    1a.(optional) JuliaBox has Julia tutorials. You will find them in the default file directories when JuliaBox first loads.
    1b. (optional) download and install Julia from https://julialang.org/

2. Start a JuliaBox session and choose "New" then "Julia 1.0 ."

3. Type in the box: println("Hello world")

4. Choose the |> symbol to run that line of Julia code.
```



## Kotlin

In what follows I'm going to assume that you're using an x64 version of Windows.

First you need to download and install the Java SE Development Kit 8 from Oracle at http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html.

Secondly you need to download, unzip and install the Kotlin command line compiler version 1.1 from GitHub at https://github.com/JetBrains/kotlin/releases/tag/v1.1.

Next open up a Command Prompt session in Windows, type the following at the command line and press the Enter key:

```txt

notepad hello.kt

```

Now in Notepad, paste in the following and save it:

```scala
fun main(args: Array<String>) {
    println("Hello, World!")
}
```


Next type this at the command line to compile the program:

```txt

kotlinc hello.kt -include-runtime -d hello.jar

```


Finally, to run the program, you need to type in:

```txt

java -jar hello.jar

```

You should now see this cheery greeting printed to the console:
```txt

Hello, World!

```



## Locomotive Basic

Either use a browser-based CPC emulator (http://www.cpcbox.com/) or download
a binary for your platform.
A list of emulators is available at http://cpcwiki.eu/index.php/Emulators.
JavaCPC (http://sourceforge.net/projects/javacpc/) or WinAPE (http://www.winape.net/) are particularly recommended.
An advantage of native emulators is that they tend to have copy-and-paste functionality, so you can edit programs in external [[Editor|texteditor]]s.

In the emulator, type


```locobasic
10 print "Goodbye, World!"
run
```



## Lua

Without installing anything, lua-programs can be run online at the [http://www.lua.org/demo.html live demo].

It includes some demo-programs, such as

```lua
io.write("Hello world, from ",_VERSION,"!\n")
```


For installing Lua, there are extensive instructions for [http://www.lua.org/start.html getting started] on Linux, MacOS and Windows.


## M2000 Interpreter

M2000 Interpreter run on M2000 Environment, a Windows 32bit application. Interpreter is an ActiveX component and can be loaded from other programs/languages which can use this type of object.

===Installation on Windows (Xp to 10, 32 or 64bit):===

We have to download run-time for Access 2007.

We can use [https://github.com/M2000Interpreter/Version9/blob/master/m2000.cer] as a certificate as Trusted Root Certification Authorities, Or we have to block Defender or something like this, because they are suspicious for virus, and they predict whatever they figure from the code.

So now we can install the code downloading the setup executable from this link: [https://github.com/M2000Interpreter/Version9/blob/master/M2000language.exe]

This is the Wiki page on github
[https://github.com/M2000Interpreter/Version9/wiki/New-Installation-for-Binaries-(from-Revision-46,-Version-9.0)]

'''Starting M2000 Environment'''

When start m2000.exe a console open at full screen. We can start m2000.exe without open console, making gsb files to open with m2000.exe. So we can use any text editor, and save to any format (UTF-8 by default, but can use ANSI, UTF-16LE, UTF-16BE), and by double clicking the file we get the program on M2000 environment, and we have to include a line at the end to execute a specific module.

Console can change using Form. Form 80, 50 make console 80 characters by 50 lines. Console can be used for graphics too. There is a split screen function, where the lower part of console can scroll, and upper part used as header. Using '''Cls , -10''' we make 10 last lines as scrollable. We can use Settings to open a dialog for changing font name, size, line space, default colors.


From console we can execute command line statements (loops not allowed there), or we can make modules/functions (these are global), using Edit statement. Edit A open internal editor with syntax color, for module A. We can write Print "Hello World" and pressing Esc return to command line. We write A and pressing enter we execute this module. We can see with Modules the list of user modules (not modules inside modules) in memory and in disk (a M2000 user folder)

We can save all modules/functions using Save and a name like this: '''Save example1'''

We can load (merge) modules from a saved file using Load: '''Load example2'''

(file names: example1.gsb and example2.gsb)

We can edit a file on disk without merging using this: '''Edit "example1.gsb"'''

We can use '''New''' to erase any loaded module

We can use '''Start''' to make a Cold Reset to environment (this can be done with Break key)

We can stop execution using '''Esc''' or '''Ctrl+C'''

We can test execution using '''Test''' or '''Test nameofmodule'''

We can use '''Help All''' for a list of all statements or '''Help Print''' for help for print

To close interpreter, we have to write END in command line. We can start several interpreters. Also we can save programms and make them to start by double clicking.


### Installation on Linux using Wine



Run this in a terminal
for wine>1.8 do this first, and change prefix32 to .wine (use ctrl+H to see hidden folders in home folder).

```txt

WINEPREFIX="$HOME/prefix32" WINEARCH=win32 wine wineboot

```

Now .wine is a 32bit prefix, so we can use art2kmin mdac28 which need 32 bit prefix

```txt

winetricks vb6run art2kmin mdac28

```

Latest installation on Wine version 3.0.2 Ubuntu 18.04, 64bit.
and we have to use native oleaut32, and we get interpreter at ten times faster than with build in one. Without it in Wine 3.0 we get fault if we use this statement Print 1=1 because this produce a boolean type (Ole Variant), and only native Oleaut32 handle this.

Install Environment using  M2000Interpreter.exe

Execute Settings statement first time to set default font, size of console

Use Edit module_name to edit (with syntax highlight) or copy code from this site, press Esc to return to console prompt.

Use Edit A as B to rename a module_name

Use Save name1 to save any module/function

Use New to erase all modules/functions

Use Load name1 to modules

Use Ctrl+A to save all changes to previous loaded file.

Use arrows to see previous commands

Use Edit without argument to edit commands in a screen editor with syntax highlight.

Execute End to close environment


## Maple


Installation of Maple 2017
To start, make sure your computer meets the system requirements.
Ensure you are connected to the internet and download the installation file. Install Maple.
Once installed, find, and run the application.
Once Maple is up and running, select the "File" button in the top left corner, and then select "New", and select either Document mode or Worksheet mode.
Once in the selected mode, it becomes very easy to display "Hello World".
Maple has multiple ways to display text on the screen, but the most simple way is to type the words you want printed, surrounded by double quotes, and followed by a semi-colon.
Example:

```Maple
"Hello World";
```

                         "Hello World"
Congratulations, you executed your first line of code!

=={{header|Mathematica}} / {{header|Wolfram Language}}==
Buy & Install the program from http://www.wolfram.com/mathematica/,
e.g. "Home - For nonprofessional use by hobbyists and enthusiasts" --> €295

Start Mathematica either :
* by calling math.exe(Windows) or math (Others)
* by double clicking on the Mathematica icon.

Open a new notebook and type :
Print[ "Goodbye, World!"]



## MiniScript


```MiniScript
print "Hello World!"

```





## Monte


Follow the directions at http://monte.readthedocs.org/en/latest/intro.html#where-do-i-start to set up your Monte environment.

Then

$ bin/monte monte/src/examples/hello.mt

The code in hello.mt simply defines a function and then calls it:


```monte

def sayHello():
    traceln("Hello World")
sayHello()

```



## MontiLang


Download MontiLang binaries for windows or linux from http://montilang.ml, or from the releases page of https://github.com/lduck11007/MontiLang. Alternatively, you can easily build from source for easy customization with the instructions on Github.

To run MontiLang, open a shell by typing
```MontiLang>monti</lang
 in the terminal, or run a program by specifying
```MontiLang>monti file.mt</lang
 For documentation on the language, see file 'Documentation.mt' in /examples on github.

Here is a simple Hello World program in MontiLang


```MontiLang
|Hello, World!| PRINT .
```



## Nemerle

# Download Nemerle from [http://www.nemerle.org Nemerle.org]


### Nemerle on Windows

The installer available on Nemerle.org includes Visual Studio integration.
If you use Visual Studio, this gives syntax highlighting, name completion, etc.
Otherwise, you can use your favorite [[Editor|texteditor]] or [[IDE]].

In your text editor, type (or copy/paste):
  using System.Console;

  class Hello {
      static Main() : void {
          WriteLine("Goodbye, world.");
      }
  }

or, more concisely:
  System.Console.WriteLine("Goodbye, world.")

Save this file as "hello.n" (or whatever you like,
but the example below assumes this name)

Now, either run it from within your IDE,
or open a console window and type:
    c:\>ncc hello.n
(this assumes ncc.exe is in your path, where it should be if you used the installer)

The output file will be out.exe; run out.exe as so
    c:\>out

and be dazzled by the output:
    Goodbye, world.


## Nim

A comprehensive introduction to the Nim language can be found at [http://howistart.org/posts/nim/1/index.html How I Start: Nim].

A brief summary of installing Nim and building a simple ''Hello world!'' program is as follows:

1) Install Nim by selecting the appropriate platform at [https://nim-lang.org/install.html Install Nim] and follow the instructions.

2) Open a text editor and create a file named ''hello.nim'' with the following contents:


```nim
echo "Hello world!"
```


3) Open a terminal console, and run the following command:


```txt

nim c -r hello

```


The program should produce the following output:

```txt
Hello world!
```


The ''-r'' option causes the program to automatically run after it is successfully compiled.



## OCaml


Prerequisites: Ocaml
Download: On Linux, Ocaml should be available in your package manager.

Create file: 'hello.ml'

Type in file:
```ocaml
print_string "Hello world!\n";;
```


Compile with:
```Shell
ocamlc -o hello hello.ml
```


Run as:
```Shell>./hello</lang




## Oforth


Select one of the Oforth implementation to download : http://www.oforth.com

Copy oforth directory at a location of your choice.

Set OFORTH_PATH environnement variable value to this directory. You can also add this directory to your PATH variable in order to be able to launch Oforth from everywhere.

Create file "hello.of"

Type in file:


```Oforth
"Hello world!" println
```


Run as:
```Shell>oforth hello.of</lang


Or you can use oforth command line directly :


```Shell
oforth --P"\"Hello world!\n\" println"
```



## Pare


Prerequisites: Perl

Download: https://bitbucket.org/parelang/pare/downloads/pare

Create file 'hello.l':
```Pare
(print "hello world")
```


Run:
```Shell>perl pare hello.l</lang



## PARI/GP

PARI's official site is http://pari.math.u-bordeaux.fr/ .


### PARI/GP on Windows

Go to the download page of the PARI/GP website and download one of the following:
* The latest self-installing binary distribution 2.4.2. This is the most full-featured but lacks some of the newer bugfixes and commands.
* The basic GP binary 2.5.0. This lacks some features like high-resolution graphing but has newer features.
* The SVN version 2.6.0. This has the bleeding-edge features but lacks many nicities and may contain bugs.


### PARI/GP on the Mac

Install the latest available gp version on
* http://www.darwinports.info/ports/math/pari.html
or
* http://www.finkports.info/ports/sci/pari-gp.html


### PARI/GP on Linux

Install PARI/GP with an appropriate package manager: [http://rpmfind.net/linux/rpm2html/search.php?query=pari-gp RPM], [http://joysofprogramming.com/install-pari-gp-ubuntu/ apt], etc.
Alternately, [http://math.crg4.com/software.html#pari install it from source].


### Your first program

Open a [[Editor|texteditor]] of your choice and type

```parigp
print("Hello, world!")
```

Save the file in your PARI working directory and start the program, either in a console (command: <code>gp</code>) or in the GUI in Windows (by double-clicking the shortcut). Type <code>\r filename</code> to read in the program. (If you saved the file with a .gp extension, you can leave it off here.)
The program executes, displaying "Hello, world!".


## Perl

See [http://www.perl.org/get.html Download Perl Distributions]

```perl
=head1 Obtaining perl

On the majority of UNIX and UNIX-like operating systems
(Linux, Solaris, AIX, HPUX, et cetera), perl will already be installed.
Mac OS X also ships with perl.
Note that "Perl" refers to the language
while "perl" refers to the interpreter used to run Perl programs.

Windows does not ship with perl. Instead, you will have to install one of
the following perl distributions:

=over 4

=item Strawberry Perl

L<Strawberry Perl|http://strawberryperl.com/>: A 100% Open Source Perl for
Windows that is exactly the same as Perl everywhere else; this includes using
modules from CPAN, without the need for binary packages.

=item DWIM Perl for Windows

L<DWIM Perl for Windows|http://dwimperl.com/windows.html>: A 100% Open Source
Perl for Windows, based on Strawberry Perl.
It aims to include as many useful CPAN modules as possible.

=item ActiveState Perl

L<http://www.activestate.com/activeperl/downloads>

=back

Links and further instructions on installation can be found on
L<http://www.perl.org/get.html>.

Once perl is installed, the task of printing "Hello, World!" is quite simple.
From the command line, first check if your environment's C<PATH> variable
knows where to find perl.
On most systems, this can be achieved by entering C<which perl>;
if it spits out something like F</usr/bin/perl>, you're good to go!
If it tells you

    which: no perl in (...)

it means you need to add perl to your environment's C<PATH> variable.
This is done on most systems by entering

    export PATH=$PATH:[...]

where [...] is the full path to your perl installation (usually /usr/bin/perl).

If you do not have the C<which> command, you can probably just type C<perl>
to see if it fires up the perl interpreter.
If it does, press Ctrl+D to exit it and proceed.
Otherwise, perform the steps above to add perl to your PATH variable.

Once perl is installed, one-liners can be executed from the command line
by invoking perl with the C<-e> switch.
    $ perl -e 'print "Hello, World!\n";'
To create a script file that's more permanent, it can be put in a text file.
The name can be anything, but F<.pl> is encouraged.
The #!/usr/bin/perl at the beginning is called the shebang line;
if the operating system supports it, it tells where to find the perl interpreter.
If the script is run with C<perl>, this line will be ignored--
this is for invoking the file as an executable.

=cut

#!/usr/bin/perl
print "Hello, World!\n";
```



## Perl 6


Perl 6 is a language spec and test suite, not an implementation. Any implemention which implements the spec and passes the test suite can call itself perl 6. Philosophically, there is no official "Perl 6 distribution". Practically, at this point (late 2015), Rakudo perl 6 is the only realistic option. There are others, but they are either not currently being developed or far behind Rakudo in functionality.

There are several ways to get and install Rakudo perl 6 depending on your operating system and the balance between your technical familiarity and tolerance of outdated code. Rakudo perl 6 is still in heavy development and the code base still has features being added, refined and optimized on a daily basis; a few weeks can make a big difference.

For ease of installation, there are MSI files for Windows, Homebrew packages for OSX and pre-built Linux packages for apt and yum based packaging systems. They will get you a working compiler most easily, but may be weeks or months out of date.

At least until Rakudo makes it out of beta, it is probably best to build from source.

All of these options are detailed on the Rakuko.org website: [http://rakudo.org/how-to-get-rakudo/| http://rakudo.org/how-to-get-rakudo/ ].

If you do run into problems installing Rakudo, (or any perl 6 compiler, or have any Perl6 questions,) the #perl6 IRC channel on freenode.net nearly always has people willing to help out.

Once you have your compiler installed, open a terminal window and type:

  perl6 -e'say "Hello World!"'

or, under Windows cmd.exe,

  perl6 -e"say 'Hello World!'"

and press enter.

Alternately, type

  echo say 'Hello World!' > hw.p6

then type

  perl6 hw.p6

to execute it.

Note that the file name may have any extension or none at all.


## Phix

Installing and running Phix is as just about as straightforward as it could possibly be.

Download Phix from http://phix.x10.mx/download.php (16MB)

Full documentation is available at http://phix.x10.mx/docs/html/phix.htm as well as being included in the
download as a chm file, along with all the files that was originally generated from.

'''Windows'''

The Windows installer targets C:\Program Files[ (x86)]\Phix by default, and runs ppw.bat, which
first creates a console version of phix and then allows PATH and other registry settings to be made.

The documentation is best viewed by opening docs\phix\phix.chm.

The easiest way to get started is to open a console window in the installation directory.

An editor/ide is provided, simply run "pw edita". The F1 key provides context-sensitive help.

(A cross platform editor is currently in progress, try "pw edix" to run that.)

Enter the following:
```Phix
puts(1,"Hello world!")
```

Save as test.exw in the installation directory and run "p test".

The output will appear in the command prompt/terminal.

Note: you can also save the file anywhere and press F5 in edita/edix to run it, however unless
you add {}=wait_key() or similar, the output (window) will immediately disappear.

'''Linux'''

Extract from the plain zip to $HOME\phix, and open a terminal there.

Install a suitable program to view docs/phix/phix.chm, or run "./phix docs/phix/makephix" to populate
docs/phix/html with the plain html files.

An (in-progress) editor/ide is provided, simply run "./phix edix" from that directory, though further
steps may be required to install IUP, see demo/pGUI/lnx/installation.txt, otherwise any editor will do.

Enter the following:
```Phix
puts(1,"Hello world!")
```

Save as test.exw in the installation directory and run "./phix test".

You can also save the file anywhere and press F5 in edix to run it.

The output will appear in the command prompt/terminal.


## PHP

PHP can be run either from the command line or via a web server such as Apache.
If you are running Mac or Linux, PHP is likely already installed.  If you are on
Windows, you can set up your own web server locally by using XAMPP (https://www.apachefriends.org/index.html).

Create the file, hello.php, in your htdocs or public_html directory.

```txt

<?php
echo 'Hello, world!';
?>

```


If Apache is running on your local computer and you saved the file in htdocs, open a
web browser and go to http://localhost/hello.php.  You should see the phrase in a basic
font displayed in the window.

From the command line, simply type

```txt

php ./hello.php

```



## PicoLisp

===Debian-based systems===
Install

```txt
$ apt-get install picolisp
```

then run it

```txt
$ pil +
: (prinl "Goodbye, World!")
```


### Other POSIX systems

Fetch and unpack the tarball

```txt
$ wget software-lab.de/picoLisp.tgz && tar xfz picoLisp.tgz
```

then build the executable (see http://software-lab.de/INSTALL file for 64-bit systems)

```txt
$ cd picoLisp
$ (cd src; make)
```

and run it locally

```txt
$ ./pil +
: (prinl "Goodbye, World!")
```


===Non-POSIX systems===
On non-POSIX systems only limited implementations of PicoLisp are available.

If Java 1.6 is installed, get and unpack either the tarball given in
"Other POSIX systems", or just "software-lab.de/ersatz.tgz", and run

```txt
$ ersatz/pil
: (prinl "Goodbye, World!")
```

(see also http://software-lab.de/ersatz/README).

If no Java is available, you can compile "software-lab.de/miniPicoLisp.tgz" and run it

```txt
$ ./pil +
: (prinl "Goodbye, World!")
```



## PowerShell

Windows [[PowerShell]] should be already installed in [[Windows]] 7 and [[Windows]] 8.

### Accessing PowerShell

There are 2 ways to access Windows Powershell:
*Open Command Prompt, then type "PowerShell".
*Click the Start Button, then All Programs, then Accessories, then Windows PowerShell. From there you will find the PowerShell Console (a big blue one), and the Windows PowerShell ISE (Integrated Scripting Environment).
===Executing the "HelloWorld" Code in Console===
Now, to execute your first code in PowerShell, choose from the two options above to access the PowerShell console. If the console is already open, type any of these codes:

```powershell
"Hello, World!" #This is a comment.
```

or,

```powershell
wRiTe-HOsT "Hello, World!" #PowerShell is case-insensitive.
```

or,

```powershell
Write-Host Hello`, World! #The backtick escapes the next character.
```

Your output should be:

```txt
Hello, World!
```


### Writing and Executing Your First PowerShell Script

These are the ways to write/edit a PowerShell script:


'''Using a Simple [[Editor | Text Editor:]]'''

To make the "HelloWorld" code in a script file, copypaste the code (select one) above, and then save it as "<filename>.ps1".
To execute the script, open the PowerShell console, [[wp:Cd_(command)|go to the directory of the script file]], then type <code>./<filename></code>.


'''Using the Windows PowerShell ISE:'''

It is  '''recommended''' to use the PowerShell ISE if you are new to PowerShell. Copypaste the code (select one) above, then run the code by clicking the play button or by pressing F5.


### Exploring PowerShell

To learn the basic syntaxes and details of PowerShell, Open the PowerShell ISE, then press F1 to open the Windows PowerShell "Getting Started" Guide.


## Python

Pythons official home site is [http://www.python.org/ http://www.python.org/].
It will point you to everything Python.


### Python on Windows

(Tested on Windows 7 but should be similar for XP & Vista ).

You need to download and install Python.
Use the latest Windows installer for Windows
(64bit if you have a 64bit Windows installation).
It is a standard Windows click-through installer with an Open-source compatable license.

Once installed, use the new start-menu entry to open the "Idle (Python GUI)" application, which opens a GUI window with a command line and cursor at the bottom. This window displays program output and is a [[wp:REPL|REPL]] for Python.

Use the File->New window item of the GUI to bring up new blank window
and copy the text from [[Hello world/Text#Python]] into it,
i.e.
```python
print "Goodbye, World!"
```

use the File menu to save the file with a name hello.py,
(remember the .py extension).
Use the "<code>Run -> Run module</code>" menu item
from the hello.py Idle editor window to pop the Idle Python shell window to the front whilst executing the program.
The output of the program appears in this shell window as the line:

```txt
Goodbye, World!
```

(Followed by a prompt that is part of the REPL of the IDE
rather than programmed by the hello.py file).


### Python on OS X

Assuming you didn't delete them by accident, Python 2.x and Python 3.x are already installed.
To get started, open the Terminal in your applications folder, and type:
 $ echo 'print "Goodbye, World!"' > hello.py
You now have a Python script in your home directory, which you can run with the following command:
 $ python hello.py
Alternatively, you can work with Python's IDE, IDLE, the same way as described above for Windows, by typing:
 $ idle
You can also execute scripts by including a shebang and using the chmod command as described below for GNU/Linux.


### Python on GNU/Linux

On most Linux distribution, just install the package named <code>python</code>
with the package manager.

If you are using a mainstream Linux distribution, installing Idle
along with Python gives you a GUI and two ways of working with your code.
The easiest is through Idle, see the Windows instructions above for details.
The other is outlined below:

A script can be executed with the command:
 python my_script.py
or adding a shebang at the first line of the script:
 $ head -n 1 my_script.py
 #!/usr/bin/env python
 $ chmod a+x my_script.py
 $ ./my_script.py

==={{libheader|VPython}}===
# install Python, as above
# install VPython: at [http://vpython.org/index.html vpython.org], select the download for your system:
#* [http://vpython.org/contents/download_windows.html Windows]
#* [http://vpython.org/contents/download_linux.html Linux]
#* [http://vpython.org/contents/download_mac.html Mac]
# To edit/run programs from the IDE, instead of IDLE use VIDLE (it loads the vpython-extensions, and helptexts)
#* Look at some example programs -- for example, bounce.py
#* (on Windows, they should be located at C:\Python27\Lib\site-packages\visual\examples)
#* Try some programs from RC -- for example, [[Draw_a_cuboid#Library:_VPython|Draw a cuboid]]


## Ra


### Requirements

* [http://cobra-language.com/downloads/ Cobra]
* All Cobra requirements


### Download

Download the latest version of Ra from [https://github.com/oahmad04/ra/releases GitHub]


### Installation for Windows

* Unzip the download
* Open a command prompt window as an administrator in the unzipped folder
* Enter the following command: install.exe
* You should see the word "Success"
* Add "C:\Ra" to your PATH variable


### Installation for Mac/Unix/Linux/Ubuntu

* Unzip the download
* Open a terminal in the unzipped folder
* Enter the following command: sudo ./install
* Enter your password if prompted to by the terminal
* You should see the word "Success"


### Writing Ra Code

Create a plain text file and enter the following text:

```Ra

class HelloWorld
	**Prints "Goodbye, World!"**

	on start

		print "Goodbye, World!"

```

Save the plain text file as "HelloWorld.racode" (all Ra files must end with the .racode extension).


### Running Ra Code

Open a terminal and navigate to the directory where "HelloWorld.racode" is saved. Enter the following command:

ra HelloWorld

You should see "Goodbye, World!" printed to the terminal.

You can optionally include the file extension when running Ra code:

ra HelloWorld.racode


### Links

[https://github.com/oahmad04/ra/blob/master/README.md Installation instructions]

[http://oahmad04.github.io/ra/learn.html Learn Ra]


## Racket


### Installation

Install Racket from [http://racket-lang.org/ the Racket home page].

On Windows and OS X when you have a 64 bit platform, you can install either the 64 bit Racket or the 32 bit one, the installer page will not try to detect this, since both options can be used.  The choice will usually not make much difference; a quick summary is that with the 64 bit version you can use much more space (in 32 bit mode there's a 4gb limit), but things are going to run a bit slower due to Racket shuffling more memory around.  (This is not Racket-specific, of course.)

On Linux, you can often find a Racket package for your distribution flavor.
From the Racket web page you can find a few installers built on different Linux distros, and one of them might fit you even if you're on a different one.
The Racket installers come in shell-script form, which you can just run in a terminal (<tt>sh installer.sh</tt> would do it).  The first question that the installer will ask you is whether you want a unix-style distribution, with files spread across standard directories -- the recommendation is to not do this, and instead install Racket in a single directory.
This choice makes it much easier to try Racket quickly -- you can just remove the directory when you're done, you can easily have multiple versions installed, and you don't need write permission to system directories.


### Developing Racket Code

Racket comes with DrRacket, an IDE that is specifically designed to make Racket work easy (and implemented in Racket).
However, you can use your own [[Editor|text editor]] if you're used to one.
See the [http://docs.racket-lang.org/guide/other-editors.html Racket Guide] section
on working with different editors, and running Racket from the command line.

As with Common Lisp, EMACS is a good editor to use for Racket code. Its inferior-lisp mode allows you to evaluate Racket expressions from within EMACS, however it doesn't provide debugging, symbol lookup, or other features that are available when editing Common Lisp code with EMACS.


### Learning Racket

One of Racket's main points is coming with thorough documentation, which you get with the installation and but you can also view it [http://docs.racket-lang.org/ on-line].  Specifically, pay attention to the [http://docs.racket-lang.org/getting-started/ Getting Started] page that will help with a few convenient entry points.

### Hello World

Save it as "hello.rkt":
<lang "Racket">
#lang racket
(displayln "Hello world!")

```



### Running it


### =From BASH=


```txt

$ racket hello.rkt

```


### =On Windows=

Click the icon for hello.rkt in Explorer, or click the Run button within DrRacket.


## REXX


### installation

Each REXX interpreter for a particular operating system has their own
requirements, but most involve just copying (usually) two files:
* the REXX interpreter,
* the REXX run-time library (the built-in functions and REXX statement executors).

Note that some operating systems have REXX "built-in", so there
isn't any installation necessary.

The "MVS" and "VM/CMS" IBM mainframe
operating systems have REXX installed (indeed, built-into the operating
system) as well does OS/2 and AS/400.

Note that MVS and VM/CMS are more-or-less generic terms in the IBM world.


### program creation

<><>Next, create a REXX program.
Most users use their favorite [[Editor|texteditor]] (for "plain text").

Note that some operating systems require the first statement
to be a REXX-type comment,

another requires the first word in the comment to be the word REXX
(in any case, upper/lower/mixed).

Most REXX interpreters enforce the first of these two rules.

The reason for this is that some operating systems have more than
one scripting language,

and this [[wp:Shebang (Unix)|first record]] is used
to invoke the appropriate interpreter.

So, a <tt> Hello world! </tt> REXX program (two lines) could look like:

```rexx
/*REXX program to greet the world enthusiastically. */
say "Hello world!"
```

The above program could be made into one statement
(since REXX comments are treated like whitespace).


### output

The output (for the SAY instruction) will be shown
on the terminal screen (or window).

```txt

Hello world!

```



### execution


To execute the program depends on the operating system.
Just placing the program in a special library (or folder) and invoking
the name of the REXX program will execute it.

You could also invoke the name
of the REXX interpreter followed by the name of the REXX program.


For any CMS system, just place the "HELLO EXEC" file on any disk
that is accessed (normally the 'A' minidisk is used).

For Windows (in a DOS window), place the file "HELLO.REX" in the
current directory (folder) or place the file in the folder that is
specified for the REXX interpreter to use when the program can't be found
in the current directory (CD).
Windows class of NT operating systems (NT, XP, and later) can be set up
so that whenever a program (file) that has an extension of (say) REX,
it will be executed (interpreted) by the REXX interpreter (EXE program).
It's possible to have a REXX program without a file extension (filetype),
but that would make it not obvious to what the file's purpose is
and also it'll make it harder to invoke easily.




## Ring


```ring

sayHello()

func sayHello
     see "Hello World" + nl

```



## Robotic


### Note

This language runs exclusively on MegaZeux. If you are unfamilar with how to use MegaZeux, I would suggest reading into the [http://vault.digitalmzx.net/help.php online manual] provided on their website. Alternatively, MegaZeux has the same manual built in to the program itself and can be accessed any time by pressing F1.

This "Hello World" tutorial will only cover the very basics of the user interface and Robotic coding.


### Short Summary

Robotic is a programming language used in the game creation system titled [http://www.digitalmzx.net/wiki/index.php?title=MegaZeux MegaZeux]. It provides simple things like counters and arithmetic expressions, as well as more complex functions like file reading/writing. A short summary of this language can be found [http://www.digitalmzx.net/wiki/index.php?title=Robotic here].


### Download

To get started in programming with Robotic, you must [http://vault.digitalmzx.net/ download MegaZeux] first. The top of the page will show symbols corresponding to the OS you want to download it on (currently supports Windows, Ubuntu, Mac OS X, and MS-DOS).

After downloading the program, make sure to extract the contents of the .zip in a directory of your choosing.


### Running Megazeux

When running MegaZeux, it will always request to load a world file (represented as .mzx). For the purpose of this tutorial, we want to get past that screen. Simply hit ESC and then press E. This will bring you to the World Editor.

Take your time to explore the editor. You are able to click on the tabs below to show the controls that are available to you.

When you are ready, move the cursor to an empty spot (using the arrow keys), then press F10 and select "Robot" (name and character is optional).


### The Code

The code is as simple as it looks:

```robotic

* "Hello world!"
end

```


After typing the code down, hit ESC and press Alt+T to test it out. If done properly, you will see "Hello World" flashing at the bottom of the screen.


### =Breakdown=

* The asterisk is the symbol that tells it to display the given string on the bottom of the screen (positions of the text can be changed, but this is the default).
* The keyword 'end' stops the code from executing any further. It is not needed for this simple program, but it is recommended that you end your code so that it will not continue to reach other parts of code you may end up writing below.


### Final note

'''Remember:''' This is just a bare-bones tutorial on how to program Robotic. Please refer to the manual (stated above) if you want to learn more about Robotic and MegaZeux itself.


## Ruby


1. Install Ruby
For windows you can download Ruby from [http://rubyforge.org/frs/?group_id=167] for Linux try [http://www.rpmfind.net.]

2. Use your favorite [[Editor|text editor]].

3. Open the text editor and type the following:
```Ruby

  puts "Hello World!!"

```


4. Save the file with the extension .rb

5. Open a command prompt/terminal. Execute "ruby filename.rb" on Windows.
'''Output'''

```txt

Hello World!!

```


## Rust

A complete description of how to install the language can be found on the [http://www.rust-lang.org/install.html Install page] of the [http://www.rust-lang.org Rust web site].  However, for the purposes of trying out code examples, use the [http://play.rust-lang.org online REPL] which allows you to enter code, compile it and run it within a single browser window.


```rust
fn main() {
    println!("Hello world!");
}
```



## Scala

A complete and actual description can be found on the [http://www.scala-lang.org/documentation/getting-started.html Getting started guide] on the [http://www.scala-lang.org/ Scala site].

Run a program using '''Windows PowerShell''':

```Scala
scala -e 'println(\"Hello_world\")'
```

The double quotes have to be escaped.


## SETL

Get the suitable [http://setl.org/setl/bin/ precompiled executable] for your platform from the [http://setl.org/setl/ SETL Website]. The 'setl' command works in conjunction with 'setlcpp' and 'setltran' so you may want to copy them all over to /usr/bin/ (or equivalent).

Run in-line commands

```Bash
setl 'print("Hello, world!");'
```

or create a file 'myscript.setl'

```SETL
print("Hello, world!");
```

and run it

```Bash>setl myscript.setl</lang


Documentation on the setl command can be found [http://setl.org/doc/setl-user.html here] and a paper on the SETL language can be found [https://www.researchgate.net/publication/238739905_An_introduction_to_the_set_theoretical_language_SETL here].


## SQL PL

In order to make a Hello World! example run in Db2, you need:
* Get a binaries.
* Extract the binaries.
* Install the binaries.
* Create an instance.
* Create a database.
* Connect to a database.
* Execute Hello World! example.

The easies way is to install all of this in a Linux environment (Red Hat, Suse, Ubuntu).

You can download the Express-c edition of the most recent Db2 version at: https://www.ibm.com/developerworks/downloads/im/db2express/.

Once you get the binaries in the server, you can extract them by issuing

```txt
tar -xvf v11.1_linuxx64_expc.tar.gz
```

You go to the directory where the installer is

```txt
cd expc
```

and then execute the installer

```txt
./db2_install
```

It is highly probable that you need to install extra libraries in your OS in order to install Db2.
Once you have installed Db2, you go to the instance directory

```txt
cd /opt/ibm/db2/V11.1/instance
```

And you create the instance associated with an existant user

```txt
./db2icrt -u db2inst1 db2inst
```

Now, you have an instance, you change to that user

```txt
su - db2inst1
```

And create a database

```txt
db2 create database test
```

When the process has finished, you can connect to it

```txt
db2 connect to test
```

And finally, you can execute the Hello World! example

```txt
db2 "select 'Hello World!' from sysibm.sysdummy1"
```



## Smalltalk


### Pharo Smalltalk


### =Installation=

Currently supported platforms are Linux, Mac OS X, and Windows.
From the landing page http://pharo-project.org click the "Download" button
in the upper right hand corner.
You should see a new page with options for downloading a ZIP file
for your operating system. Unzip the downloaded file and open Pharo.
You may see some windows open. You can read through them and then close them.

Pharo is a live coding environment. You can inspect the entire system at any time using the tools provided. One such tool is called a Workspace. To open a Workspace, CTRL+click anywhere in the background of Pharo and select "Workspace" from the pop-up menu. A window titled "Workspace" will appear and that window should be empty.
To log output, you use the Transcript. You can open the Transcript by executing CTRL+click, then selecting Tools > Transcript from the pop-up menu.
Because Pharo is a live programming environment, you can also
open the Transcript programmatically, which we will do in the next section.


### =Code=

Copy the following text into the "Workspace" window:

```txt

Transcript open.
Transcript show: 'Hello world'.

```


This code will, when executed, open a new Transcript window
and then output "Hello world" to the Transcript.


### =Execution=

To execute the code in the Workspace, select both lines, then CTRL+click in the Workspace and select "Do it" from the pop-up menu.
A new Transcript window should open and you should see "Hello world" in the Transcript.


## smart BASIC



###  Installation


smart BASIC is an Apple iOS application for iPhones and iPads. It must be installed from the [https://itunes.apple.com/us/app/smart-basic-programming-language/id541447413?mt=8 Apple App Store].


###  Programming


<b>Code Editor</b>

The textual code editor is built into the application. To write your own program, do the following:
<ol>
<li> Press the smart BASIC icon to run the program. A file list will be displayed.</li>
<li> In the upper left corner, press the "+" symbol to create a new file. A New File name dialog box will appear.</li>
<li> Enter a filename and press 'OK'. Filenames are automatically saved with the extension ".txt" The editor will then display a blank page for code entry.</li>
<li> Your code is automatically saved when you run or exit the editor.</li>
</ol>

<b>Output</b>

Standard output is displayed upon running the application. To run your code, press the triangle icon in the upper right corner.

<b>Xcode</b> (Optional, but so cool)

Optionally, an amazing feature of Smart BASIC is the ability to write your code in Apple's free Xcode IDE and compile the code to run in a simulator on your Mac computer or on your own iOS devices. The procedure for this is beyond the scope of this entry but details and code can be found in the Forum section titled "BASIC SDK for Xcode". It might sound complicated, but it's actually very easy and only takes a few simple steps to setup. You can download detailed, step-by-step instructions [https://dl.dropboxusercontent.com/u/24473770/Simple%20Procedure%20for%20Adding%20Smart%20BASIC%20to%20Xcode.pdf here].


###  Documentation


smart BASIC has very detailed documentation installed within the application. There is also an excellent (free) PDF manual that may be downloaded directly from [https://www.dropbox.com/sh/zpsvd55g1iyjldj/AABN92ibmwUe8LgM0GCyu8M4a/Smart%20Basic%20Manual%205-7.pdf?dl=0 here] that includes current information from the smart BASIC [http://kibernetik.pro/forum/viewforum.php?f=2 Forum] (When registering at the Support Forum, the anti-spambot password is: "iOS").


## Swift



###  Installation

If you haven't installed Xcode already, then please go to the App Store for Mac and install the Xcode program. This will allow for Swift development on both OSX and IOS.


###  Use the playground

For most code examples, as from this site, you are able to enter them directly in a playground. That is start up Xcode, and either choose the playground from the default startup menu, or select ''File > New > Playground''.

Within this window you are able to write code on the left hand side of the window, and the output is shown on the right hand side. If you have errors in your code, this might prevent the output, but in this case the window displays error icons and some explanations.


###  Swift Tutorial and Example Playground

Downloading [https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/GuidedTour.playground.zip GuidedTour.playground] from the book "The Swift Programming Language" available for free from [https://itunes.apple.com/book/swift-programming-language/id881256329 Apple book store], can be opened and this has a lot of examples of the basic structures and how to do different stuff in Swift.


## Tcl


### Installation

Many operating systems come with a distribution of Tcl, though it is often
of an old version (Tcl 8.4 or occasionally before).
Some also provide Tcl 8.5; it is not normally a problem to have both
on your system at once, provided software is not coded to use a
version-less executable name.
As of the time of writing, nothing comes with Tcl 8.6.


### =Installing Tcl 8.5 on Linux=

Your distribution will probably include a package for 8.5 in its distribution,
often named something like “<tt>tcl8.5</tt>”.
Many of the tasks here on Rosetta Code also require [[:Category:Tcllib|Tcllib]], a collection of scripted Tcl packages; some distributions provide this as well.


### =Installing Tcl 8.5 on Windows and OSX=

You are recommended to use [[ActiveTcl|ActiveState's Tcl distribution]], which is of very high quality.


### =Installing Tcl 8.6=

As of the time of writing, [[ActiveTcl]] is the only “full service” distribution of Tcl 8.6.


### Running your first Tcl code

Going to a command prompt and typing:

```txt
tclsh8.5
```

(or <code>tclsh85</code> on Windows) will start an interactive Tcl shell.
At the prompt, you can just type in Tcl commands and have them executed immediately.
For example, if you type in this little program:

```tcl
puts "Hello World"
```

You will get this message printed out:

```txt
Hello World
```

before another prompt symbol. Congratulations! You've just run your first Tcl code.

If you're using ActiveTcl, you can also try typing these examples
into the Tkcon prompt.
Tkcon's very slightly different, but for most Tcl programs it works just the same.

When running Tcl 8.6, change the <tt>tclsh8.5</tt>/<tt>tclsh85</tt> to be <tt>tclsh8.6</tt>/<tt>tclsh86</tt>.
If you want the Tk package as well because you're working on writing a GUI,
it is often easier to use <tt>wish</tt> instead of <tt>tclsh</tt>
(with appropriate version number suffix), though it's still the same language.
It just has some extra commands.


### Running your first Tcl program

Now save that same command in a text file called <code>hello.tcl</code>.
Any simple [[Editor|texteditor]] will do (but not a word processor!);
your author often uses [[Emacs]] for this, and it comes with automatic
syntax highlighting for Tcl code.

To run that saved code, type one of these:
 tclsh8.5 hello.tcl                ''On Linux/OSX''
 tclsh85 hello.tcl                 ''On Windows''
The code in the file will be executed (printing the message to the screen) and then it the Tcl interpreter will exit. There you go, your first full Tcl program.


### Going Further

If you're writing a larger program, you might want to use an IDE.
The main two IDEs for Tcl are ActiveState's Komodo (commercial)
and Eclipse with the appropriate DLTK module (free to use).

For a range of interesting Tcl programs and techniques
beyond what is here on Rosetta Code,
check out [http://wiki.tcl.tk/ The Tcler's Wiki].

Documentation for many versions of Tcl is [http://www.tcl.tk/man/ also online],
which can be a handy supplement to your system docs.

=={{header|TI-83 Hex Assembly}}==

* You must have a TI-8x series calculator (or an emulator) to program in TI-83 Hex Assembly.
* On the calculator, press the "PRGM" key, then select the "NEW" tab and press the "ENTER" key.
* Choose a program name, like "HELLO".
* Start your program with the "AsmPrgm" (0x6dbb) token, which can be found in the Catalog (press the "2nd" key then the "0" key) going down to the 9th entry and pressing "ENTER".
* Now you need to enter the actual program code: We want to call the PutS system call (also known as B_CALL) with its argument "HL" being a pointer to the string "Hello, World!", then we want to exit. Note that there should be no spaces in the program at all. Newlines are allowed, but only after an even number of hex digits per line (0, 2, 4, ... are OK, but not 1, 3, 5, ...). There is no way to write comments in the program.
* First, we'll load the memory location of our string to HL, so write "21XXXX". We'll replace "XXXX" with the actual memory location later because we haven't added the data section yet.
* Next, make the PutS system call by typing "EF0A45". Here "EF" denotes a syscall and "0A45" represents the system call with address 0x450a, or _PUTS. The bytes are reversed because the TI-83 uses a Z80 processor which is little-endian. (You can find all known syscalls here: [http://wikiti.brandonw.net/index.php?title=Category:83Plus:BCALLs:By_Name])
* Next, make the NewLine (0x452e) system call by typing "EF2E45".
* Next, write an exit instruction in the program by typing "C9".
* Now, after the "C9", we'll start our data section. Now that we know where we're starting the data we should update our "21XXXX" line above. To get the "XXXX" number, simply count forward in hexadecimal from the beginning starting at 0x9d93, where the "AsmPrgm" token counts as two bytes and every other string of two consecutive hex characters/digits counts as one byte (there should always be an even number of hex characters/digits in the program). If you counted correctly, you should have gotten 0x9d9f, which after reversing the bytes for little-endian representation becomes 9F9D. So replace "XXXX" with "9F9D".
* Finally, write the ASCII representation of the string "Hello, World!" at the very end of your program with a "00" at the end to terminate the string. This is "48656C6C6F2C20576F726C642100".
* Exit the editor by pressing "QUIT" ("2nd" then "MODE").
* Execute the program by going back to the catalog ("2nd" "0") and selecting the 7th entry, "Asm(", then pressing "PRGM" and selecting your program name, then closing the parens, so you should get (for example) "Asm(prgmHELLO)".

The program should look like this when it's done:


```ti83b
PROGRAM:HELLO
:AsmPrgm
:219F9D
:EF0A45
:EF2E45
:C9
:48656C6C6F2C20576F726C642100
```


=={{header|TI-83 BASIC}}==

The TI-8x series calculators have a built-in BASIC scripter.
To access this, press the "PRGM" key.
You can create a new program by selecting the "NEW" tab and pressing "ENTER".
You will be prompted to enter 8 alphanumeric characters for the name,
but it must start with a letter.
Most programming functions can be accessed by pressing the "PRGM" key,
and all functions can be viewed in alphabetical order by pressing "2nd" then "0".

Type in code so your screen looks like this (where MYPROGRM is the name of your program):

```ti83b
PROGRAM:MYPROGRM
:Disp "HELLO, WORLD!"
```


Disp is found under the "I/O" tab of the menu from the "PRGM" key,
the "!" symbol can be found under the "PRB" tab of the menu from the "MATH" button,
and quotations are "ALPHA" then "+".

When writing programs, it is much faster to know the shortcuts for certain
common functions. Any list in the TI OS can be selected from using the number keys,
so "Disp" for instance is "PRGM" then "3".

to exit the editor, press "2nd" then "MODE", and to run the program press "PRGM" and select your program from under the "RUN" tab.


## Zig



```zig

// - Install zig from https://ziglang.org/download/.
// - Extract into your path
// - `zig run Newbie.zig`

const std = @import("std");

pub fn main() void {
    std.debug.warn("Hello, World!\n");
}

```



## zkl

Download/install Ubuntu (or other Ubuntu flavor, Windows 10, PC-BSD,
FreeBSD) on a Intel/AMD box (other CPUs have not been tested). You want cores and memory.

I'll use Ubuntu/64 as my example.

Visit http://www.zenkinetic.com/zklDownloads.html and download
zkl_vm_src.zip:  http://www.zenkinetic.com/Documents/zkl_vm_src.zip.
Other links of interest are zkl_tests.zip and the manual (zklManual.pdf).

 Extract to ~
 Open a termninal

 Install clang (I do not like GCC) and ncurses (native on PC-BSD).
 (See the Makefile if you insist on GCC).
 sudo apt-get install clang
 sudo apt-get install libncurses5-dev

 $ cd ZKL/VM
 $ make zkl
   ...
   mv zkl ../Bin
   Stand alone executable built
 $ cd ..
 $ Bin/zkl
 # and you are off and running:
 zkl 1.12.34, released 2016-11-01
 zkl: "Hello World!"
 Hello World!
 zkl: <control D>
 $

 $ echo 'println("Hello World!")' > foo.zkl
   Or edit foo.zkl with a text editor (emacs, vi, gedit, notepad, whatever)
 $ zkl foo.zkl
 Hello World!
 $


## ZX Spectrum Basic


The computer has a basic interpreter build into rom,
so there is no need to install an software before entering a program.
Switch on the computer. You should see a message as follows:

(C) 1982 Sinclair Research Limited.

Press the number 1 key followed by the number 0 key. (Note this is not the same as the letter O). Now press the P key. This should cause the keyword PRINT will appear in full. The computer knows that a keyword is required, and behaves accordingly. The flashing K cursor indicates that a keyword is expected. You should now see:

10 PRINT

followed by a flashing L cursor. The computer is now in letter entry mode, and is no longer expecting a keyword. Hold down the symbol shift key, and press the P key again. This time you should get a doublequote symbol. Let go of the symbol shift key.The screen should look as follows:

10 PRINT "

Now let go of the symbol shift key, and hold down the Caps Shift key. Press the letter H key. You should now get a capital H. Let go of the Caps Shift key and type the letters ELLO. Now press symbol shift again and press the P key, to produce another doublequote symbol.

You have now typed the first line of the program. Press the ENTER key. The program line will now appear at the top of the screen as follows:


```zxbasic
10 PRINT "Hello"
```


The cursor returns to a flashing K indicating that a keyword is expected again. Press the letter R. We now get the keyword RUN followed by a flashing L cursor. This time we have not entered a line number, because we want the command to be executed immediately. Press ENTER. The program will run and you should see the following:

Hello

0 OK 10:1

You have just created and run your first program.

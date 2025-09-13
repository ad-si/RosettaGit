+++
title = "Narcissist"
description = ""
date = 2019-10-09T15:19:43Z
aliases = []
[extra]
id = 8341
[taxonomies]
categories = ["task"]
tags = []
+++

Quoting from the [http://esolangs.org/wiki/Narcissist Esolangs wiki page]:

<blockquote>
A '''narcissist''' (or '''Narcissus program''') is the decision-problem version of a [[quine]].
</blockquote><blockquote>
A quine, when run, takes no input, but produces a copy of its own source code at its output. In contrast, a narcissist reads a string of symbols from its input, and produces no output except a "1" or "accept" if that string matches its own source code, or a "0" or "reject" if it does not.
</blockquote>

For concreteness, in this task we shall assume that symbol = character.

The narcissist should be able to cope with any finite input, whatever its length.

Any form of output is allowed, as long as the program always halts, and "accept", "reject" and "not yet finished" are distinguishable.





## Ada

Took code from [[Quine]], has to be in one line (could be done pretty printed, too, but not as simple).


```Ada
with Ada.Text_IO;procedure Self is Q:Character:='"';A:String:="with Ada.Text_IO;procedure Self is Q:Character:='';A:String:=;B:String:=A(1..49)&Q&A(50..61)&Q&A&Q&A(62..A'Last);C:String:=Ada.Text_IO.Get_Line;begin Ada.Text_IO.Put_Line(Boolean'Image(B=C));end Self;";B:String:=A(1..49)&Q&A(50..61)&Q&A&Q&A(62..A'Last);C:String:=Ada.Text_IO.Get_Line;begin Ada.Text_IO.Put_Line(Boolean'Image(B=C));end Self;
```



## ALGOL 68

```algol68
STRINGs="STRINGs="";print(readstring=2*s[:9]+2*s[9:])";print(readstring=2*s[:9]+2*s[9:])
```

Output: T or F depending on input.


## AutoHotkey

```AutoHotkey
Narcissist(Input) {
	FileRead, Source, % A_ScriptFullPath
	return Input == Source ? "accept" : "reject"
}
```

'''Example Use:'''

```AutoHotkey
MsgBox, % Narcissist(FileOpen(A_ScriptFullPath, "r").Read()) "`n"
	. Narcissist("This isn't the text you're looking for.")
```

```txt
accept
reject
```



## BBC BASIC

Prints '1' on success and '0' on failure.

```bbcbasic
INPUT a$:PRINT -(a$=$(PAGE+34)+$(PAGE+33)):REM INPUT a$:PRINT -(a$=$(PAGE+34)+$(PAGE+33)):REM
```



## Befunge

Reads from stdin up to the first carriage return, line feed, or EOF. However, the latter is not guaranteed to work on all Befunge implementations - in particular not on Befunge-98 - so a line break is recommended.

Outputs <tt>1</tt> if the given line of input matches the source code, and <tt>0</tt> if it doesn't.


```befunge
900:0g~>:::0>`#0\#:5#:5#:+#<#~-#g*#0\#:5#+8#1+#\-#!*#-_$$"E"-!>_9-!.@
```



## C

Based upon the quine. Reads until EOF or newline from stdin, and writes "1" or "0" to stdout.

```c
extern void*stdin;main(){ char*p = "extern void*stdin;main(){ char*p = %c%s%c,a[300],b[300];sprintf(a,p,34,p,34);fgets(b,300,stdin);putchar(48+!strcmp(a,b)); }",a[300],b[300];sprintf(a,p,34,p,34);fgets(b,300,stdin);putchar(48+!strcmp(a,b)); }
```



## C#


```c#

using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
namespace Narcisisst
{
	class Program
	{
		public static void Main(string[] args)
		{
			const string path = @"E:\Narcisisst";
			string[] thisFile = Directory.GetFiles(path , "Program.cs");
			StringBuilder sb = new StringBuilder();

				foreach (string readLine in File.ReadLines(thisFile[0]))
				{
					sb.Append(readLine);
					sb.Append("\n");
				}

			Console.WriteLine(sb);
			string input =String.Empty;
			       	input = Console.ReadLine();
			       	Console.WriteLine((Regex.IsMatch(sb.ToString(),input))?"accept":"reject");
			       	Console.ReadKey();
			 }
	}
}

```



## Common Lisp

Only checks the first line of stdin:

```lisp
#1=(PRINT (EQUAL (WRITE-TO-STRING '#1# :CIRCLE 1) (READ-LINE *STANDARD-INPUT*)))
```


## D


```D
import std.file; import std.stdio; import std.string; void main() { auto source = readText("narcissist.d").chomp; auto input = readln().chomp(); if (source == input) writeln("accept"); else writeln("reject"); }
```


=={{header|Déjà Vu}}==

```dejavu
!. = !prompt "Enter my code: " concat( swap !decode!utf-8 !encode!quoted dup swap ) "!. = !prompt \qEnter my code: \q concat( swap !decode!utf-8 !encode!quoted dup swap ) "
```



## Forth



```forth
: narcissist  [ source ] sliteral compare 0= ;
```



## Go


This version reads until EOF and expects a newline at the end of the input. If this is being checked from a file, make sure that the file has exactly one newline at the end of it.


```go
package main; import "os"; import "fmt"; import "bytes"; import "io/ioutil"; func main() {ios := "os"; ifmt := "fmt"; ibytes := "bytes"; iioutil := "io/ioutil"; zero := "Reject"; one := "Accept"; x := "package main; import %q; import %q; import %q; import %q; func main() {ios := %q; ifmt := %q; ibytes := %q; iioutil := %q; zero := %q; one := %q; x := %q; s := fmt.Sprintf(x, ios, ifmt, ibytes, iioutil, ios, ifmt, ibytes, iioutil, zero, one, x); in, _ := ioutil.ReadAll(os.Stdin); if bytes.Equal(in, []byte(s)) {fmt.Println(one);} else {fmt.Println(zero);};}\n"; s := fmt.Sprintf(x, ios, ifmt, ibytes, iioutil, ios, ifmt, ibytes, iioutil, zero, one, x); in, _ := ioutil.ReadAll(os.Stdin); if bytes.Equal(in, []byte(s)) {fmt.Println(one);} else {fmt.Println(zero);};}
```

A version respecting the 80 character line limit:

```go
package main

import (
    "bytes"
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    s := fmt.Sprintf("%s%c%s%c\n", x, 0x60, x, 0x60)
    in, _ := ioutil.ReadAll(os.Stdin)
    if bytes.Equal(in, []byte(s)) {
        fmt.Println("Accept")
    } else {
        fmt.Println("Reject")
    }
}

var x = `package main

import (
    "bytes"
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    s := fmt.Sprintf("%s%c%s%c\n", x, 0x60, x, 0x60)
    in, _ := ioutil.ReadAll(os.Stdin)
    if bytes.Equal(in, []byte(s)) {
        fmt.Println("Accept")
    } else {
        fmt.Println("Reject")
    }
}

var x = `
```



## Haskell


```Haskell
main = let fi t e c = if c then t else e in do ct <- getContents; putStrLn $ fi ['a','c','c','e','p','t'] ['r','e','j','e','c','t'] $ take (length ct - 1) ct == let q s = (s ++ show s) in q "main = let fi t e c = if c then t else e in do ct <- getContents; putStrLn $ fi ['a','c','c','e','p','t'] ['r','e','j','e','c','t'] $ take (length ct - 1) ct == let q s = (s ++ show s) in q "
```



## Huginn


```huginn
#! /bin/sh
exec huginn --no-argv -E "${0}"
#! huginn

main() {
	c = "#! /bin/sh{1}~"
		"exec huginn --no-argv -E {3}${{0}}{3}{1}#! huginn{1}{1}~"
		"main() {{{1}{2}c = {3}{0}{3};{1}~"
		"{2}s = {3}{3};{1}~"
		"{2}while ( ( line = input() ) != none ) {{{1}~"
		"{2}{2}s += line;{1}~"
		"{2}}}{1}~"
		"{2}self = copy( c ).replace( {3}{5}{3}, {3}{3} )~"
		".format({1}{2}{2}c.replace( {3}{5}{3}, ~"
		"{3}{5}{4}{3}{4}n{4}t{4}t{4}{3}{3} ), ~"
		"{3}{4}n{3}, {3}{4}t{3}, {3}{4}{3}{3}, {3}{4}{4}{3}, ~"
		"{3}{5}{3}{1}{2});{1}~"
		"{2}print( s == self ? {3}1{4}n{3} : {3}0{4}n{3} );{1}}}{1}{1}";
	s = "";
	while ( ( line = input() ) != none ) {
		s += line;
	}
	self = copy( c ).replace( "~", "" ).format(
		c.replace( "~", "~\"\n\t\t\"" ), "\n", "\t", "\"", "\\", "~"
	);
	print( s == self ? "1\n" : "0\n" );
}

```


=={{header|Icon}} and {{header|Unicon}}==
Since we can't put a link statement and program on a single line, we implement a simplified inline sprintf so we don't have to deal with all the double quotes or substrings and offsets.  If we'd needed to write multiple procedures on a line it can be done [[Icon%2BUnicon/Intro#Semi-colons| Semi-colons in the language intro]]

```Icon
procedure main();yes:="Accept";no:="Reject";pat:="procedure main();yes:=$;no:=$;pat:=$;a:=[yes,no,pat];narc:=char(0)[0:0];pat?{while narc||:=tab(find(char(36))) do{narc||:=image(get(a));move(1)};narc||:=tab(0)};write(if read()==narc then yes else no);end";a:=[yes,no,pat];narc:=char(0)[0:0];pat?{while narc||:=tab(find(char(36))) do{narc||:=image(get(a));move(1)};narc||:=tab(0)};write(if read()==narc then yes else no);end
```

Example:
```txt
./narcissist.exe < narcissist.icn
Accept
```

Actually, this version recognizes all files where the first line is the Narcissist.


## J


```j
#!/j602/bin/jconsole
main=:3 : 0
  self=: '#!/j602/bin/jconsole',LF,'main=:',(5!:5<'main'),LF,'main''''',LF
  echo  self -: stdin''
)
main''
```


Example use:

<lang>$ ./narcissist.ijs <narcissist.ijs
1

```


Note that this assumes a suitable os command line.


'''Alternative solution:'''

```j
   narcissist=.(-:,~,2#{:)&'(-:,~,2#{:)&'''
```


'''Example use:'''

```j
   (-:,~,2#{:)&'(-:,~,2#{:)&''' '(-:,~,2#{:)'
0
   (-:,~,2#{:)&'(-:,~,2#{:)&''' '(-:,~,2#{:)&''(-:,~,2#{:)&'''''''
1
```



## Java


```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Narcissist {
    private static final String SOURCE = "import java.io.BufferedReader;%nimport java.io.IOException;%nimport java.io.InputStreamReader;%n%npublic class Narcissist {%n    private static final String SOURCE = %c%s%c;%n    private static final char QUOTE = 0x22;%n%n    public static void main(String[] args) throws IOException {%n        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));%n        StringBuilder sb = new StringBuilder();%n%n        while (true) {%n            String line = br.readLine();%n            if (null == line) break;%n            sb.append(line).append(System.lineSeparator());%n        }%n%n        String program = String.format(SOURCE, QUOTE, SOURCE, QUOTE, QUOTE, QUOTE, QUOTE, QUOTE);%n        if (program.equals(sb.toString())) {%n            System.out.println(%caccept%c);%n        } else {%n            System.out.println(%creject%c);%n        }%n    }%n}%n";
    private static final char QUOTE = 0x22;

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringBuilder sb = new StringBuilder();

        while (true) {
            String line = br.readLine();
            if (null == line) break;
            sb.append(line).append(System.lineSeparator());
        }

        String program = String.format(SOURCE, QUOTE, SOURCE, QUOTE, QUOTE, QUOTE, QUOTE, QUOTE);
        if (program.equals(sb.toString())) {
            System.out.println("accept");
        } else {
            System.out.println("reject");
        }
    }
}

```



## JavaScript

Based upon [[Quine#Using_eval|one of the quines]]. Outputs 'true' if source is equal to inputted line (newline terminated), 'false' otherwise.

```javascript
var code='var q=String.fromCharCode(39);print("var code=" + q + code + q + "; eval(code)" == readline())'; eval(code)
```


```javascript
var oFSO = new ActiveXObject("Scripting.FileSystemObject");
function readfile(fname) {
	var h = oFSO.OpenTextFile(fname, 1, false);
	var result = h.ReadAll();
	h.Close();
	return result;
}

if (0 === WScript.Arguments.UnNamed.Count) {
	WScript.Echo(WScript.ScriptName,"filename");
	WScript.Quit();
}

// first read self
var self = readfile(WScript.ScriptFullName);
// read whatever file is given on commmand line
var whatever = readfile(WScript.Arguments.UnNamed(0));

// compare and contrast
WScript.Echo(self === whatever ? "Accept" : "Reject");

```


## Julia


```julia
mysource = Base.read(Base.source_path(), String)
println(Int(ARGS[1] == mysource))
```



## Kotlin


```scala
// version 1.1.0 (run on Windows 10)

fun main(args: Array<String>) {
    val text = java.io.File("narcissist.kt").readText()
    println("Enter the number of lines to be input followed by those lines:\n")
    val n = readLine()!!.toInt()
    val lines = Array<String>(n) { readLine()!! }
    if (lines.joinToString("\r\n") == text) println("\naccept") else println("\nreject")
}
```

First run (pasting in program text):
```txt

Enter the number of lines to be input followed by those lines:

9
// version 1.1.0 (run on Windows 10)

fun main(args: Array<String>) {
    val text = java.io.File("narcissist.kt").readText()
    println("Enter the number of lines to be input followed by those lines:\n")
    val n = readLine()!!.toInt()
    val lines = Array<String>(n) { readLine()!! }
    if (lines.joinToString("\r\n") == text) println("\naccept") else println("\nreject")
}

accept

```

Second run (entering any old rubbish):

```txt

Enter the number of lines to be input followed by those lines:

1
the quick brown fox

reject

```



## Liberty BASIC

NOTE: You have to manually type in ALL of the code since the Input statement will not successfully input data from a paste event even though it will show up in the MainWin.


```lb

 s$ = "s$ = Input a$ : Print (a$ = Left$(s$, 5) + chr$(34) + s$ + chr$(34) + Mid$(s$, 14, 3) + Mid$(s$, 6, 100)) + Mid$(s$, 23, 3)" : Input a$ : Print (a$ = Left$(s$, 5) + chr$(34) + s$ + chr$(34) + Mid$(s$, 14, 3) + Mid$(s$, 6, 100))

```



## Mathematica

```Mathematica
prog = "prog = ``;\nPrint[InputString[] == \n   ToString[StringForm[prog, ToString[prog, InputForm]]]];";
Print[InputString[] ==
   ToString[StringForm[prog, ToString[prog, InputForm]]]];
```

```txt
True
```



## PARI/GP


```parigp
narcissist()=input()==narcissist
```

Run narcissist():
```txt
Hello
0
```


```txt
narcissist() = {input() == narcissist;}
1
```


## Perl


```perl
# this is file narc.pl
local $/;
print do { open 0; <0> } eq <> ? "accept" : "reject";
```

Run:
<lang>perl narc.pl < narc.pl
```



## Perl 6


For the narcissist to work you must be very careful with whitespace. The following version works if it is given to standard input as exactly one line terminated by a newline character.

Note how the code takes advantage of Perl 6's ability to nest quoting delimiters.


```perl6
EVAL my $self = q{say slurp() eq q[EVAL my $self = q{]~$self~q[}]~10.chr ?? q{Beautiful!} !! q{Not my type.}}
```


```txt
$ narcissist='EVAL my $self = q{say slurp() eq q[EVAL my $self = q{]~$self~q[}]~10.chr ?? q{Beautiful!} !! q{Not my type.}}'
$ perl6 -e "$narcissist" <<<"$narcissist"
Beautiful!
$ perl6 -e "$narcissist" <<<"$narcissist # a comment ruining it all"
Not my type.

```



## Phix


```Phix
puts(1,{"\n\ntrue\n\n","\n\nfalse\n\n"}[1+(gets(open(command_line()[2],"r"))!=gets(0))])
```



## PicoLisp


```PicoLisp
(de narcissist (Str)
   (= Str (str narcissist)) )
```

Output:

```txt
: (narcissist "(Str) (= Str (str narcissist))")
-> T
```



## PowerShell

```PowerShell

function Narcissist
{
Param ( [string]$String )
If ( $String -eq $MyInvocation.MyCommand.Definition ) { 'Accept' }
Else { 'Reject' }
}

```


```PowerShell

Narcissist 'Banana'

Narcissist @'

Param ( [string]$String )
If ( $String -eq $MyInvocation.MyCommand.Definition ) { 'Accept' }
Else { 'Reject' }

'@

```

```txt

Reject
Accept

```



## Python

```Python

import sys
with open(sys.argv[0]) as quine:
    code = raw_input("Enter source code: ")
    if code == quine.read():
        print("Accept")
    else:
        print("Reject")

```


```Python

_='_=%r;print (_%%_==input())';print (_%_==input())

```



## Racket


This shows a REPL interaction, where the second expression is what is
typed when the code stops to read some input.


```Racket

-> ((lambda (x) (equal? (read) (list x (list 'quote x))))
   '(lambda (x) (equal? (read) (list x (list 'quote x)))))
((lambda (x) (equal? (read) (list x (list 'quote x))))
 '(lambda (x) (equal? (read) (list x (list 'quote x)))))
#t

```



## REXX


### version 1

(returns   '''1'''   or   '''0''')

```rexx
/*REXX*/ say arg(1)=sourceline(1)
```



### version 2

(returns   '''accept'''   or   '''reject''')

```rexx
/*REXX*/ say word('reject accept',1+(arg(1)=sourceline(1)))
```



## Ruby

Translation of the C version.

```ruby
s = "s = %s%s%s; puts(gets.chomp == (s %% [34.chr, s, 34.chr]) ? 'accept' : 'reject')"; puts(gets.chomp == (s % [34.chr, s, 34.chr]) ? 'accept' : 'reject')
```

Output:

```txt
$ ruby narcissist.rb < narcissist.rb
accept
```



## Scala


```scala
import scala.io.StdIn

object Narcissist extends App {
  val text = scala.io.Source.fromFile("Narcissist.scala", "UTF-8").toStream
  println("Enter the number of lines to be input followed by those lines:\n")
  val n = StdIn.readInt()
  val lines = Stream {
    StdIn.readLine()
  }
  if (lines.mkString("\r\n") == text) println("\naccept") else println("\nreject")
}
```


## Sidef


```ruby
say (File.new(__FILE__).open_r.slurp == ARGF.slurp);
```



## Swift



```swift
#! /usr/bin/swift
import Foundation

let script = CommandLine.arguments[0]
print(script)
let mytext = try? String.init(contentsOfFile: script, encoding: .utf8)

var enteredtext = readLine()
if mytext == enteredtext {
    print("Accept")
} else {
    print("Reject")
}
```



## Tcl

With the use of explicit reflexive introspection:

```tcl
apply {{} {puts [expr {[gets stdin] eq [info level 0]}]}}
```

Without such commands, using pure generation of strings and lists:

```tcl
apply {s {puts [expr {[gets stdin]eq[list {*}$s $s]}]}} {apply {s {puts [expr {[gets stdin]eq[list {*}$s $s]}]}}}
```



## TXR



```txr
@(bind my64 "QChuZXh0IDphcmdzKUBmaWxlbmFtZUAobmV4dCBmaWxlbmFtZSlAZmlyc3RsaW5lQChmcmVlZm9ybSAiIilAcmVzdEAoYmluZCBpbjY0IEAoYmFzZTY0LWVuY29kZSByZXN0KSlAKGNhc2VzKUAgIChiaW5kIGZpcnN0bGluZSBgXEAoYmluZCBteTY0ICJAbXk2NCIpYClAICAoYmluZCBpbjY0IG15NjQpQCAgKGJpbmQgcmVzdWx0ICIxIilAKG9yKUAgIChiaW5kIHJlc3VsdCAiMCIpQChlbmQpQChvdXRwdXQpQHJlc3VsdEAoZW5kKQ==")
@(next :args)
@filename
@(next filename)
@firstline
@(freeform "")
@rest
@(bind in64 @(base64-encode rest))
@(cases)
@  (bind firstline `\@(bind my64 "@my64")`)
@  (bind in64 my64)
@  (bind result "1")
@(or)
@  (bind result "0")
@(end)
@(output)
@result
@(end)

```


```txt
$ txr narcissist.txr narcissist.txr
1
```



## VBA

Based on the quine

```vb
Public Sub narcissist()
    quote = Chr(34)
    comma = Chr(44)
    cont = Chr(32) & Chr(95)
    rparen = Chr(41)
    n = Array( _
"Public Sub narcissist()", _
"    quote = Chr(34)", _
"    comma = Chr(44)", _
"    cont = Chr(32) & Chr(95)", _
"    rparen = Chr(41)", _
"    n = Array( _", _
"How many lines?", _
"Line ", _
"    x = InputBox(n(5))", _
"    flag = True", _
"    For i = 0 To 5", _
"        If InputBox(n(6) & i) <> n(i) Then flag = False", _
"    Next i", _
"    For i = 0 To 20", _
"        If InputBox(n(6) & i + 6) <> quote & n(i) & quote & comma & cont Then flag = False", _
"    Next i", _
"    If InputBox(n(6) & 27) <> quote & n(21) & quote & rparen Then flag = False", _
"    For i = 7 To 21", _
"        If InputBox(n(6) & i + 21) <> n(i) Then flag = False", _
"    Next i", _
"    Debug.Print IIf(flag, 1, 0)", _
"End Sub")
    x = InputBox(n(5))
    flag = True
    For i = 0 To 5
        If InputBox(n(6) & i) <> n(i) Then flag = False
    Next i
    For i = 0 To 20
        If InputBox(n(6) & i + 6) <> quote & n(i) & quote & comma & cont Then flag = False
    Next i
    If InputBox(n(6) & 27) <> quote & n(21) & quote & rparen Then flag = False
    For i = 7 To 21
        If InputBox(n(6) & i + 21) <> n(i) Then flag = False
    Next i
    Debug.Print IIf(flag, 1, 0)
End Sub
```


## UNIX Shell


```bash
cmp "$0" >/dev/null && echo accept || echo reject
```



## zkl


```zkl
stdin:=File.stdin.read();
thisFileSrc:=File(System.argv[1]).read();
println((stdin==thisFileSrc) and "input matches "+System.argv[1] or "No match");
```

Since zkl is [usually] compile-when-run, we just compare the contents of source file to stdin.
```txt

$ zkl narcissist.zkl <narcissist.zkl
input matches narcissist.zkl
$ zkl narcissist.zkl <narcissist3.zkl
No match

```



+++
title = "Readline interface"
description = ""
date = 2019-04-10T16:52:22Z
aliases = []
[extra]
id = 10767
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "cpp",
  "csharp",
  "d",
  "go",
  "java",
  "julia",
  "kotlin",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "pike",
  "racket",
  "rexx",
  "ruby",
  "sidef",
]
+++

A readline interface is a line editing facility that provides auto completion facilities and the ability to recall previously typed lines. Build a simple application that accepts at least two commands and uses a readline style interface.

The interface should provide

* the ability to recall previously typed commands
* commandline editing
* application specific commands

The application could be based on [[Simple database]] or something else.

If the language already provides a readline interface, then it should be demonstrated how build an application specific readline interface (one that only understands the applications commands) and also show how to customize the builtin readline to provide the above features and especially add application specific commands.

See also: [[Input loop]]

__TOC__


## C

A program that does absolutely nothing. Type 'help' for help.

```c
#include <readline/readline.h>
#include <readline/history.h>
#include <string.h>

int main()
{
	char *s;
	using_history();
	while (1) {
		s = readline("This be a prompt> ");

		if (!s || !strcmp(s, "quit")) {
			puts("bye.");
			return 0;
		}

		if (!strcmp(s, "help"))
			puts("commands: ls, cat, quit");
		else if (!strcmp(s, "ls") || !strcmp(s, "cat")) {
			printf("command `%s' not implemented yet.\n", s);
			add_history(s);
		} else
			puts("Yes...?");
	}
}
```



## C++

```cpp
#include <iostream>
#include <string>
#include <vector>

std::vector<std::string> hist;

std::ostream& operator<<(std::ostream& os, const std::string& str) {
    return os << str.c_str();
}

void appendHistory(const std::string& name) {
    hist.push_back(name);
}

void hello() {
    std::cout << "Hello World!\n";
    appendHistory(__func__);
}

void history() {
    if (hist.size() == 0) {
        std::cout << "No history\n";
    } else {
        for (auto& str : hist) {
            std::cout << " - " << str << '\n';
        }
    }
    appendHistory(__func__);
}

void help() {
    std::cout << "Available commands:\n";
    std::cout << "  hello\n";
    std::cout << "  hist\n";
    std::cout << "  exit\n";
    std::cout << "  help\n";
    appendHistory(__func__);
}

int main() {
    bool done = false;
    std::string cmd;

    do {
        std::cout << "Enter a command, type help for a listing.\n";
        std::cin >> cmd;
        for (size_t i = 0; i < cmd.size(); ++i) {
            cmd[i] = toupper(cmd[i]);
        }

        if (strcmp(cmd.c_str(), "HELLO") == 0) {
            hello();
        } else if (strcmp(cmd.c_str(), "HIST") == 0) {
            history();
        } else if (strcmp(cmd.c_str(), "EXIT") == 0) {
            done = true;
        } else {
            help();
        }
    } while (!done);

    return 0;
}
```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace ReadlineInterface {
    class Program {
        static LinkedList<string> histArr = new LinkedList<string>();

        static void AppendHistory([CallerMemberName] string name = "unknown") {
            histArr.AddLast(name);
        }

        static void Hist() {
            if (histArr.Count == 0) {
                Console.WriteLine("No history");
            }
            else {
                foreach (string cmd in histArr) {
                    Console.WriteLine(" - {0}", cmd);
                }
            }
            AppendHistory();
        }

        static void Hello() {
            Console.WriteLine("Hello World!");
            AppendHistory();
        }

        static void Help() {
            Console.WriteLine("Available commands:");
            Console.WriteLine("  hello");
            Console.WriteLine("  hist");
            Console.WriteLine("  exit");
            Console.WriteLine("  help");
            AppendHistory();
        }

        static void Main(string[] args) {
            Dictionary<string, Action> cmdDict = new Dictionary<string, Action>();
            cmdDict.Add("help", Help);
            cmdDict.Add("hist", Hist);
            cmdDict.Add("hello", Hello);

            Console.WriteLine("Enter a command, type help for a listing.");
            while (true) {
                Console.Write(">");
                string line = Console.ReadLine();
                if (line=="exit") {
                    break;
                }

                Action action;
                if (cmdDict.TryGetValue(line, out action)) {
                    action.Invoke();
                } else {
                    Help();
                }
            }
        }
    }
}
```



## D


```D
module readline_interface;

import std.stdio;
import std.string;

alias VOIDF = void function();

void hello() {
    writeln("Hello World!");
    histArr ~= __FUNCTION__;
}

string[] histArr;
void hist() {
    if (histArr.length == 0) {
        writeln("No history");
    } else {
        foreach(cmd; histArr) {
            writeln(" - ", cmd);
        }
    }
    histArr ~= __FUNCTION__;
}

void help() {
    writeln("Available commands:");
    writeln("  hello");
    writeln("  hist");
    writeln("  exit");
    writeln("  help");
    histArr ~= __FUNCTION__;
}

void main() {
    VOIDF[string] aa;
    aa["help"] = &help;
    aa["hist"] = &hist;
    aa["hello"] = &hello;

    writeln("Enter a command, type help for a listing.");

    string line;

    write(">");
    while ((line = readln()) !is null) {
        line = chomp(line);
        if (line == "exit") {
            break;
        }
        aa.get(line, &help)();
        write(">");
    }
}
```

```txt
Enter a command, type help for a listing.
>hist
No history
>help
Available commands:
  hello
  hist
  exit
  help
>hello
Hello World!
>hist
 - readline_interface.hist
 - readline_interface.help
 - readline_interface.hello
>exit
```



## Go

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strings"
)

type vf = func()

var history []string

func hello() {
    fmt.Println("Hello World!")
    history = append(history, "hello")
}

func hist() {
    if len(history) == 0 {
        fmt.Println("No history")
    } else {
        for _, cmd := range history {
            fmt.Println("  -", cmd)
        }
    }
    history = append(history, "hist")
}

func help() {
    fmt.Println("Available commands:")
    fmt.Println("  hello")
    fmt.Println("  hist")
    fmt.Println("  exit")
    fmt.Println("  help")
    history = append(history, "help")
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    vfs := map[string]vf{"help": help, "hist": hist, "hello": hello}
    fmt.Println("Enter a command, type help for a listing.")
    for {
        fmt.Print(">")
        scanner.Scan()
        if scerr := scanner.Err(); scerr != nil {
            log.Fatal(scerr)
        }
        line := strings.TrimSpace(scanner.Text())
        if line == "exit" {
            return
        }
        cmd, ok := vfs[line]
        if !ok {
            fmt.Println("Unknown command, try again")
        } else {
            cmd()
        }
    }
}
```


Sample session:

```txt

Enter a command, type help for a listing.
>hit
Unknown command, try again
>hist
No history
>help
Available commands:
  hello
  hist
  exit
  help
>hello
Hello World!
>hist
  - hist
  - help
  - hello
>exit

```



## Java

```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public class ReadlineInterface {
    private static LinkedList<String> histArr = new LinkedList<>();

    private static void hist() {
        if (histArr.isEmpty()) {
            System.out.println("No history");
        } else {
            histArr.forEach(cmd -> System.out.printf(" - %s\n", cmd));
        }

        class Crutch {}
        histArr.add(Crutch.class.getEnclosingMethod().getName());
    }

    private static void hello() {
        System.out.println("Hello World!");

        class Crutch {}
        histArr.add(Crutch.class.getEnclosingMethod().getName());
    }

    private static void help() {
        System.out.println("Available commands:");
        System.out.println("  hello");
        System.out.println("  hist");
        System.out.println("  exit");
        System.out.println("  help");

        class Crutch {}
        histArr.add(Crutch.class.getEnclosingMethod().getName());
    }

    public static void main(String[] args) throws IOException {
        Map<String, Runnable> cmdMap = new HashMap<>();
        cmdMap.put("help", ReadlineInterface::help);
        cmdMap.put("hist", ReadlineInterface::hist);
        cmdMap.put("hello", ReadlineInterface::hello);

        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

        System.out.println("Enter a command, type help for a listing.");
        while (true) {
            System.out.print(">");
            String line = in.readLine();
            if ("exit".equals(line)) {
                break;
            }

            cmdMap.getOrDefault(line, ReadlineInterface::help).run();
        }
    }
}
```



## Julia

Simple program that does nothing (copy of [[#C | C]]).


```julia
function input(prompt::AbstractString)
    print(prompt)
    r = readline(STDIN)

    if isempty(r) || r == "quit"
        println("bye.")
    elseif r == "help"
        println("commands: ls, cat, quit")
    elseif r ∈ ("ls", "cat")
        println("command `$r` not implemented yet")
    else
        println("Yes...?")
    end
end

input("This is a common prompt> ")
```



## Kotlin


```scala
// version 1.0.6

var range   = intArrayOf()  // empty initially
val history = mutableListOf<String>()

fun greeting() {
    ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor()  // clears console in windows 10
    println("**     Welcome to the Ranger readline interface     **")
    println("** which performs operations on a range of integers **\n")
    println("Commands available:")
    println(" range [i] [j], help, show, square, cube, add [n], sub [n]")
    println(" mul [n], prev, hist, quit")
    println()
}

fun setRange(start: Int, end: Int) {
    range = IntArray(end - start + 1) { it + start }
    show()
}

fun help() {
    println(" range [i] [j] - sets the current range to [i, j] and shows them")
    println("               - if i > j, sets the current range to the single value [i]")
    println(" help          - displays help for each command")
    println(" show          - displays a maximum of 10 values from the current range")
    println(" square        - sets the current range to their squares and shows them")
    println(" cube          - sets the current range to their cubes and shows them")
    println(" add [i]       - adds [i] to the current range and shows them")
    println(" sub [i]       - subtracts [i] from the current range and shows them")
    println(" mul [i]       - multiplies the current range by [i] and shows them")
    println(" prev          - displays previous command entered")
    println(" hist          - displays a maximum of last 10 commands entered, newest first")
    println(" quit          - quits Ranger")
    println()
    println(" < items in square brackets denote integers to be supplied e.g. range 1 5 >")
    println(" < right/left arrow keys can be used to scroll through previous commands  >")
}

fun show() {
    val max = if (range.size > 10) 10 else range.size
    val more = range.size - max
    for (i in 0 until max) println("  ${range[i]}")
    if (more > 0) println("  plus $more more")
}

fun square() {
    for (i in 0 until range.size) range[i] *= range[i]
    show()
}

fun cube() {
    for (i in 0 until range.size) range[i] *= range[i] * range[i]
    show()
}

fun add(n: Int) {
    for (i in 0 until range.size) range[i] += n
    show()
}

fun sub(n: Int) {
    for (i in 0 until range.size) range[i] -= n
    show()
}

fun mul(n: Int) {
    for (i in 0 until range.size) range[i] *= n
    show()
}

fun prev() {
    if (history.size == 0) println("- No history yet") else println("- ${history.last()}")
}

fun showHistory() {
    val size = history.size
    if (size == 0) {println("- No history yet"); return}
    val max = if (size > 10) 10 else size
    val more = size - max
    for (i in size - 1 downTo size - max) println("- ${history[i]}")
    if (more > 0) println("- plus $more more")
}

fun main(args: Array<String>) {
    val r = Regex ("\\s+")
    val rangeCommands = arrayOf("show", "square", "cube", "add", "sub", "mul")
    greeting()
    while (true) {
        var command = readLine()!!.trim().toLowerCase()
        if (command == "") continue
        var i: Int = 0
        var j: Int = 0
        var addToHistory = true

        if (command.startsWith("range")) {
            if (command == "range") { println("- Parameters required, try again\n"); continue}
            val splits = command.split(r)
            if (splits.size == 3) {
               try {
                   i = splits[1].toInt()
                   j = splits[2].toInt()
                   if (i > j) j = i
                   command = "range"
                   history.add("$command $i $j")
                   addToHistory = false
               }
               catch(ex: NumberFormatException) {
               }
            }
        }
        else if (command.startsWith("add") || command.startsWith("sub") || command.startsWith("mul")) {
            if (command.length == 3) { println("- Parameter required, try again\n"); continue}
            val splits2 = command.split(r)
            if (splits2.size == 2) {
                try {
                   i = splits2[1].toInt()
                   command = command.take(3)
                   history.add("$command $i")
                   addToHistory = false
               }
               catch(ex: NumberFormatException) {
               }
            }
        }

        if (range.size == 0 && command in rangeCommands) {
            println("- Range has not yet been set\n")
            if (addToHistory) history.add(command)
            continue
        }

        when (command) {
            "range"  -> setRange(i, j)
            "help"   -> help()
            "show"   -> show()
            "square" -> square()
            "cube"   -> cube()
            "add"    -> add(i)
            "sub"    -> sub(i)
            "mul"    -> mul(i)
            "prev"   -> prev()
            "hist"   -> showHistory()
            "quit"   -> return
             else    -> {addToHistory = false; println("- Invalid command, try again")}
        }

        if (addToHistory) history.add(command)
        println()
    }
}
```


```txt

**     Welcome to the Ranger readline interface     **
** which performs operations on a range of integers **

Commands available:
 range [i] [j], help, show, square, cube, add [n], sub [n]
 mul [n], prev, hist, quit

help
 range [i] [j] - sets the current range to [i, j] and shows them
               - if i > j, sets the current range to the single value [i]
 help          - displays help for each command
 show          - displays a maximum of 10 values from the current range
 square        - sets the current range to their squares and shows them
 cube          - sets the current range to their cubes and shows them
 add [i]       - adds [i] to the current range and shows them
 sub [i]       - subtracts [i] from the current range and shows them
 mul [i]       - multiplies the current range by [i] and shows them
 prev          - displays previous command entered
 hist          - displays a maximum of last 10 commands entered, newest first
 quit          - quits Ranger

 < items in square brackets denote integers to be supplied e.g. range 1 5 >
 < right/left arrow keys can be used to scroll through previous commands  >

range 1 3
  1
  2
  3

add 1
  2
  3
  4

mul 2
  4
  6
  8

sub 3
  1
  3
  5

square
  1
  9
  25

cube
  1
  729
  15625

hist
- cube
- square
- sub 3
- mul 2
- add 1
- range 1 3
- help

prev
- hist

show
  1
  729
  15625

quit

```



## PARI/GP


gp uses readline, but it can also be used directly in PARI:


```c
#include <stdio.h>
#include <pari/pari.h>
#include <readline/readline.h>
#include <setjmp.h>

jmp_buf env;
void gp_err_recover(long numerr) { longjmp(env, numerr); }

/* History handling (%1, %2, etc.)*/
pari_stack s_history;
GEN *history;
GEN parihist(long p)
{
  if (p > 0 && p<=s_history.n)
    return history[p-1];
  else if (p<=0 && s_history.n+p-1>=0)
    return history[s_history.n+p-1];
  pari_err(talker,"History result %ld not available [%%1-%%%ld]",p,s_history.n);
  return NULL; /* not reached */
}

int main(int argc, char **argv)
{
  char *in, *out;
  GEN z;
  entree hist={"%",0,(void*)parihist,13,"D0,L,","last history item."};
  printf("Welcome to minigp!\n");
  pari_init(8000000,500000);
  cb_pari_err_recover = gp_err_recover;
  pari_add_function(&hist);
  stack_init(&s_history,sizeof(*history),(void**)&history);
  (void)setjmp(env);
  while(1)
  {
    in = readline("? ");
    if (!in) break;
    if (!*in) continue;
    z = gp_read_str(in);
    stack_pushp(&s_history,(void*)gclone(z)); /*Add to history */
    out = GENtostr(z);
    printf("%%%ld = %s\n",s_history.n,out);
    free(in); free(out); avma=top;
  }
  return 0;
}
```


<small>Code thanks to [http://pari.math.u-bordeaux.fr/archives/pari-dev-1002/msg00023.html Bill Allombert]</small>


## Perl

A Perl shell with command history, line-editing and variable-name completion. Simplified from the example supplied with the CPAN module <code>Term::Readline::Gnu</code>.

```perl
use strict;
use warnings;
use Term::ReadLine;
use POSIX;

my $term = new Term::ReadLine 'simple Perl shell';
my $attribs = $term->Attribs;
$attribs->{completion_append_character}     = ' ';
$attribs->{attempted_completion_function}   = \&attempt_perl_completion;
$attribs->{completion_display_matches_hook} = \&perl_symbol_display_match_list;

while (defined(my $command = &reader)) {
    my @result = eval ("package main; $command");
    print "$_\n" for @result;
}

sub reader {
    my $command = $term->readline('> ');
    $term->addhistory($command) if $command;
    return $command;
}

sub perl_symbol_display_match_list {
    my($matches, $num_matches, $max_length) = @_;
    map { $_ =~ s/^((\$#|[\@\$%&])?).*::(.+)/$3/; }(@{$matches});
    $term->display_match_list($matches);
    $term->forced_update_display;
}

sub attempt_perl_completion {
    my ($text, $line, $start, $end) = @_;
    return  $term->completion_matches($text, \&perl_symbol_completion_function);
}

use vars qw($i @matches $prefix);
sub perl_symbol_completion_function {
    my($text, $state) = @_;
    my %type = ('$' => 'SCALAR', '*' => 'SCALAR', '@' => 'ARRAY', '$#' => 'ARRAY', '%' => 'HASH', '&' => 'CODE');

    if ($state) {
        $i++;
    } else {
        my ($pre, $pkg, $sym);
        $i = 0;

        no strict qw(refs);
        ($prefix, $pre, $pkg) = ($text =~ m/^((\$#|[\@\$%&])?(.*::)?)/);
        @matches = grep /::$/, $pkg ? keys %$pkg : keys %::;
        $pkg = '::' unless $pkg;
        @matches = (@matches, grep (/^\w+$/ && ($sym = $pkg . $_, defined *$sym{$type{$pre}}), keys %$pkg));
    }
    my $entry;
    for (; $i <= $#matches; $i++) {
        $entry = $prefix . $matches[$i];
        return $entry if ($entry =~ /^\Q$text/);
    }
    return undef;
}
```



## Perl 6


Perl 6 has a built in REPL that can be initiated by running the perl6 executable without any parameters. It is fairly basic on its own but here are bindings available in the Perl 6 ecosystem for [https://tiswww.case.edu/php/chet/readline/rltop.html GNU Readline] and/or [https://github.com/antirez/linenoise Linenoise] either of which will automatically provide command history, tab completion and more advance command line editing capability if installed. They are not included in the Perl 6 distribution directly. There are incompatible licensing requirements and providing hooks for third party tools allows for more customization options.

Linenoise is generally the preferred option unless you really want the emacs compatible command line editing key bindings. Readline is arguably more powerful but is somewhat fiddly to set up.

If you are not inclined to install Readline ''or'' Linenoise, the REPL also works fairly well with 3rd party tools like rlwrap.


## Phix

The phix interpreter has a readline interface, however the code involved is far too much to replicate here
(see p.exw, processCommandLine() which opens p.ini and populates default_commandlines, then invokes gets0()
from pgets0.ew with keyHandler() as a callback. Currently only tested/working on Windows, not linux.)
There are no "application specific commands", however you could init default_commandlines with some.

While the lack of code may disappoint, I think this is a closer match to the task intention than most.

A snapshot of running the interpreter: first I pressed ?, then F7, then "ed": keying "e" did little,
but "ed" auto-completed the "ix" part as that was then the only match.

```txt

Phix hybrid interpreter/compiler.

Version 0.7.9 (32 bit Windows) Copyright Pete Lomax 2006..2016

Enter ? for options or filename to execute:?

<snip>

Press F7 to list prompt history, up/down to select.

Enter ? for options or filename to execute:
demo\pGUI\tee
demo\edix\edix
demo\win32demo\generic.exw
t61
p7
demo\win32demo\generic
-d! t01
-d! test/t01type.exw
edix
test\trace
p64
-c p64
-cp
-test
-c -test
-c e01
pgui
-d e01
pdemo
e01
use (pg) up/down to select:edix

```



## Pike


watch.pike is the solution from [[Simple database#Pike]]. this solution demonstrates how to retrofit an application with a readline interface by inheriting the original and overriding specific functions.


```pike
#!/usr/bin/pike

inherit "watch.pike";

Stdio.Readline readln = Stdio.Readline();

void print_help()
{
    write("The following commands are available: \n");
    write(sort(indices(functions))*", ");
    write("\n");
}

void watch_add()
{
    ::watch_add(db);
}

void watch_list()
{
    ::watch_list(db);
}

void do_exit()
{
    exit(0);
}

mapping functions = ([ "add":watch_add,
                       "list":watch_list,
                       "load":watch_load,
                       "save":watch_save,
                       "help":print_help,
                       "quit":do_exit ]);

string prompt_read(string prompt)
{
    return readln->read(prompt+": ");
}

void main()
{
  Stdio.Readline.History readline_history = Stdio.Readline.History(512);
  readln->enable_history(readline_history);

  string prompt="> ";


  print_help();
  while(1)
  {
    string input=readln->read(prompt);
    if(!input)
      exit(0);
    if(input != "")
    {
       if (functions[input])
           functions[input]();
       else
       {
           write("unknown command\n");
           print_help();
       }
    }
  }
}
```


Sample session:
 pike watch_rl.pike
 The following commands are available:
 add, help, list, load, quit, save
 > load
 > list
   Rosetta Code                            2 Simple Database                (27.10.2011.)
 > add
 Series: Rdm asks!
 Title: Balanced ternary
 Episode: 1
 Date watched: 1.11.2011
 Add new series? [y/n]: : y
 > list
   Rdm asks!                               1 Balanced ternary               (1.11.2011.)
   Rosetta Code                            2 Simple Database                (27.10.2011.)
 > hello
 unknown command
 The following commands are available:
 add, help, list, load, quit, save
 > save
 > quit


## Racket

Racket's has a readline interface which is not used by default due to its license.  This includes the usual readline-style editing, and tab-completion for Racket bindings.  It is possible to use it as a library, or as a REPL convenience (both uses described [http://docs.racket-lang.org/readline/ in the documentation]) -- but it is better to use [http://docs.racket-lang.org/xrepl/ xrepl] which provides an enhanced command-line REPL and includes the readline interaction.


## REXX

This REXX programs supports a '''REDO''' (re-do) with very limited editing to keep the program simple.

It has a history (which can be interrogated) and some of the simple (MS) DOS commands.

The HELP (or '''?'''), REDO, error checking, and abbreviations took up most of the program.

"User" commands (subroutines) are identified with a leading period ('''.''') to make it easier to understand what's what.

```rexx
/*REXX program  implements  a  simple  "readline"  shell   (modeled after a DOS shell). */
trace off                                        /*suppress echoing of non-zero retCodes*/
signal on syntax;  signal on noValue             /*handle REXX program errors.          */
cmdX='ATTRIB CAL CHDIR COPY DEL DIR ECHO EDIT FC FIND KEDIT LLL MEM MKDIR MORE REM REXX',
                   'RMDIR SET TYPE VER XCOPY'    /* ◄──── the legal/known commands.     */
cls= 'CLS'                                       /*define the program to clear screen.  */
@hist.= '*** command not defined. ***'           /*initialize the history database.     */
hist#=0                                          /*the number of commands in the history*/
prompt='Enter command   ──or──  ?  ──or──  Quit' /*the default  PROMPT  message text.   */
sw=linesize()                                    /*some REXX don't have this BIF.       */
cls                                              /*start with a clean slate (terminal). */
redoing=0                                        /*flag for executing naked  ReDO  cmd. */
                                                 /* [↓]  do it 'til the fat lady sings. */
  do forever;    if prompter()  then iterate     /*Nothing entered?  Then go try again. */
       select                                    /* [↓]  now then, let's rock n' roll.  */
       when wordpos(xxxF,cmdX)\==0      then call .cmdX
       when xxxF=='EXIT' | xxxF="QUIT"  then leave         /*da fat lady is done singing*/
       when xxxF=='HISTORY'             then call .history
       when xxxF=='HELP'                then call .help
       when xxxF=='PROMPT'              then call .prompt
       when xxxF=='REDO'                then call .redo
       otherwise                             call er 'unknown command:'  xxx  yyy
       end   /*select*/
  end        /*forever*/

say xxxF'ting···'                                /*say goodbye, 'cause it's polite to do*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
er:      say;   say;   say '****error****';   say;   say arg(1);   say;   say;     return
.prompt: if yyyU\==''  then prompt=yyy;                                            return
.cmdX:   xxxF yyy;                                                                 return
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:     say;   say;   say center(' error! ', max(40, linesize()%2), "*");   say
                               do j=1  for arg();  say arg(j);  say;  end;   say;  exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
noValue: syntax: call err 'REXX program' condition('C') "error",,
                    condition('D'),'REXX source statement (line' sigl"):",sourceline(sigl)
/*──────────────────────────────────────────────────────────────────────────────────────*/
.help:   cmdsH= 'ATTRIB CHDIR CLS COPY DEL DIR ECHO EDIT FC FIND MEM MKDIR MORE PRINT',
                'REM RMDIR SET TREE TYPE VER XCOPY'  /*these have their own help via /? */

         cmds= '?|Help|MANual ATTRIButes CALendar CD|CHDIR|CHANGEDir COPY DELete|ERASE',
               'DELete|ERASE DIR ECHO EDIT FC|FILECOMPAre FIND HISTory Kedit LLL',
               'MEM MD|MKDIR|MAKEDir MORE PRINT PROMPT Quit|EXIT',
               'R4 RD|RMDIR|REMOVEDir REGINA REMark Rexx REDO SET TREE Type VER'

          say center(strip(xxx yyy),sw-1,'═');    cmds_=cmds;    yyyF=unabbrev(yyy)

          help.      = 'No help is available for the'    yyy    "command."
          help.cal   = 'shows a calendar for the current month or specified month.'
          help.kedit = 'KEDITs the file specified.'
          help.lll   = 'shows a formatted listing of files in the current directory.'
          help.prompt= 'sets the PROMPT message to the specified text.'
          help.quit  = 'quits (exits) this program.'
          help.redo  = 're-does the command # specified  (or the last command).'
          help.rexx  = 'executes the REXX program specified.'

          if yyy=='' then do j=1 while cmds_\==''
                          parse var cmds_ x cmds_
                          say left('',sw%2) changestr('|',x,"  |  ")
                          end    /*j*/
                     else select
                          when wordpos(yyyF,cmdsH)\==0 then yyyF '/?'
                          otherwise cmd?=yyyF
                          if left(help.yyyF,1)\==' ' then say yyyF ' ' help.yyyF
                                                     else say help.yyyF
                          end    /*select*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
.history: say center('history', sw-1, '═');       w=length(hist#)
                         do j=1  for hist#;   say right(j,w)  '═══►'  @hist.j;   end /*j*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
prompter: if redoing  then do;  redoing=0;       z=hist#-1   /*special case, bare REDO. */
                                parse var  @hist.z  xxx  yyy
                           end
                      else do;  if prompt\==''  then do;   say;   say prompt;   end
                                parse pull xxx yyy
                           end

          xxxU=xxx;   upper xxxU;  if xxx==''  then return 1 /*No input?  Then try again*/
          yyyU=yyy;   upper yyyU;  yyyU=strip(yyyU)
          hist#=hist#+1;                                     /*bump the history counter.*/
          @hist.hist#=strip(xxx yyy)                         /*assign to history.       */
          xxxF=unAbbrev(xxx)                                 /*maybe expand abbreviation*/
          return 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
.redo:      select
            when yyyU==''           then redoing=1     /*assume they want the last cmd. */
            when words(yyy)\==1     then call er 'too many args specified for' xxx
            when \datatype(yyy,'W') then call er "2nd arg isn't numeric for" xxx
            otherwise               nop
            end    /*select*/
       if redoing  then return                         /*handle with kid gloves.        */
       yyy=yyy/1                                       /*normalize it: +7 7. 1e1 007 7.0*/
       say 'Re-doing:'  @hist.yyy
       @hist.yyy
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
unabbrev: procedure;  arg ccc
                              select
                              when abbrev('ATTRIBUTES' , ccc, 6)     then return 'ATTRIB'
                              when abbrev('CALENDAR'   , ccc, 3)     then return 'CAL'
                              when abbrev('CHANGEDIR'  , ccc, 7) |,
                                     ccc=='CHDIR'                |,
                                     ccc=='CD'                       then return 'CHDIR'
                              when abbrev('CLEARSCREEN', ccc, 5)     then return 'CLS'
                              when abbrev('FILECOMPARE', ccc, 9)     then return 'FC'
                              when abbrev('DELETE'     , ccc, 3) |,
                                     ccc=='ERASE'                    then return 'DEL'
                              when abbrev('HISTORY'    , ccc, 4)     then return 'HISTORY'
                              when abbrev('HELP'       , ccc, 1) |,
                                   abbrev('MANUAL'     , ccc, 3) |,
                                     ccc=='?'                        then return 'HELP'
                              when abbrev('MAKEDIR'    , ccc, 5) |,
                                     ccc=='MKDIR'                |,
                                     ccc=='MD'                       then return 'MKDIR'
                              when abbrev('KEDIT'      , ccc, 1)     then return 'KEDIT'
                              when abbrev('QUIT'       , ccc, 1)     then return 'QUIT'
                              when abbrev('REMARK'     , ccc, 3)     then return 'REM'
                              when abbrev('REMOVEDIR'  , ccc, 7) |,
                                     ccc=='RMDIR'                |,
                                     ccc=='RD'                       then return 'RMDIR'
                              when abbrev('REXX'       , ccc, 1)     then return 'REXX'
                              when abbrev('TYPE'       , ccc, 1)     then return 'TYPE'
                              otherwise nop
                              end    /*select*/
          return ccc

```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


Some older REXXes don't have a '''changestr''' BIF, so one is included here   ──►   [[CHANGESTR.REX]].



'''output'''   showing a sample session:
<pre style="height:120ex">
Enter command   ──or──  ?  ──or──  Quit
hel
═══════════════════════════════════════════hel══════════════════════════════════
                                              ?  |  Help  |  MANual
                                              ATTRIButes
                                              CALendar
                                              CD  |  CHDIR  |  CHANGEDir
                                              COPY
                                              DELete  |  ERASE
                                              DELete  |  ERASE
                                              DIR
                                              ECHO
                                              EDIT
                                              FC  |  FILECOMPAre
                                              FIND
                                              HISTory
                                              Kedit
                                              LLL
                                              MEM
                                              MD  |  MKDIR  |  MAKEDir
                                              MORE
                                              PRINT
                                              PROMPT
                                              Quit  |  EXIT
                                              R4
                                              RD  |  RMDIR  |  REMOVEDir
                                              REGINA
                                              REMark
                                              Rexx
                                              REDO
                                              SET
                                              TREE
                                              Type
                                              VER

Enter command   ──or──  ?  ──or──  Quit
rexx $mayan 123,456,787,654,321
╔════╗ ╔════╗ ╔════╗ ╔════╗ ╔════╗ ╔════╗ ╔════╗ ╔════╗ ╔════╗ ╔════╗ ╔════╗
║    ║ ║    ║ ║    ║ ║    ║ ║    ║ ║    ║ ║    ║ ║ ∙  ║ ║    ║ ║ ∙  ║ ║    ║
║ ∙∙ ║ ║    ║ ║    ║ ║    ║ ║ ∙∙ ║ ║    ║ ║    ║ ║────║ ║────║ ║────║ ║    ║
║────║ ║    ║ ║    ║ ║────║ ║────║ ║ ∙  ║ ║    ║ ║────║ ║────║ ║────║ ║    ║
║────║ ║ ∙  ║ ║ ∙∙ ║ ║────║ ║────║ ║────║ ║ ∙∙ ║ ║────║ ║────║ ║────║ ║ ∙  ║
╚════╝ ╚════╝ ╚════╝ ╚════╝ ╚════╝ ╚════╝ ╚════╝ ╚════╝ ╚════╝ ╚════╝ ╚════╝

Enter command   ──or──  ?  ──or──  Quit
cal
 ┌────────────────────────────────────────────────────────────────────────────┐
 │                                                                            │
 │                                                                            │
 │                                June   2012                                 │
 │                                                                            │
 │                                                                            │
 │  Sunday     Monday     Tuesday   Wednesday  Thursday    Friday    Saturday │
 ├──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┤
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │  ─  1 ─  │     2    │
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 ├──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 │     3    │     4    │     5    │     6    │     7    │     8    │     9    │
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 ├──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 │    10    │    11    │    12    │    13    │    14    │    15    │    16    │
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 ├──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 │    17    │    18    │    19    │    20    │    21    │    22    │    23    │
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 ├──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 │    24    │    25    │    26    │    27    │    28    │    29    │    30    │
 │          │          │          │          │          │          │          │
 │          │          │          │          │          │          │          │
 └──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┘


Enter command   ──or──  ?  ──or──  Quit
LLL
mm/dd/yyyy hh:mm ──filesize── ─────────────────filename D:\*.*─────────────────
06/01/2012 10:36       2,322  DIV_GRID.TXT
05/13/2012 20:39       1,044  MOD_EXP.REX
05/13/2012 19:39         694  MOD_EXP.TXT
06/01/2012 02:32       1,439  RCOMBN.RX_
06/01/2012 02:24       7,181  READLINT.REX
05/29/2012 22:48          60  sub00.REX
──────────────────────12,740  <──────────────total bytes  [6 files]

Enter command   ──or──  ?  ──or──  Quit
dir
 Volume in drive D is -----D-----
 Volume Serial Number is 68D0-6356

 Directory of D:\

05/09/2012  12:40    <DIR>          Config.Msi
12/16/2005  19:54    <DIR>          Documents and Settings
01/15/2006  10:33    <DIR>          Program Files
04/14/2010  19:58    <DIR>          Recycled
12/16/2005  20:29    <DIR>          System Volume Information
01/26/2011  00:21    <DIR>          temp
12/16/2005  19:42    <DIR>          WINDOWS
01/06/2006  14:43               211 boot.ini
06/01/2012  10:36             2,322 DIV_GRID.TXT
12/16/2005  20:18                 0 IO.SYS
05/13/2012  20:39             1,044 MOD_EXP.REX
05/13/2012  19:39               694 MOD_EXP.TXT
12/16/2005  20:18                 0 MSDOS.SYS
08/04/2004  12:00            47,564 NTDETECT.COM
08/04/2004  12:00           250,032 ntldr
06/01/2012  02:32             1,439 RCOMBN.RX_
06/01/2012  02:24             7,181 READLINT.REX
05/29/2012  22:48                60 sub00.REX
              11 File(s)        310,547 bytes
               7 Dir(s)   6,793,314,304 bytes free

Enter command   ──or──  ?  ──or──  Quit
help ver
════════════════════════════════════════help ver═════════════════════════════════════════
Displays the Windows XP version.

VER

Enter command   ──or──  ?  ──or──  Quit
ver

Microsoft Windows XP [Version 5.1.2600]

Enter command   ──or──  ?  ──or──  Quit
q
QUITting...

```


## Ruby

This application provides auto-completion for these commands: ''search download open help history quit url prev past''.

Auto-completion is applied by pressing the tab-key.

A history of the commands is navigated by the up- and down-keys. All commands are just echoed, except "quit" which exits.

It uses the Ruby 2.3 method Hash#to_proc.

```ruby
require "readline"
require "abbrev"

commands = %w[search download open help history quit url prev past]
Readline.completion_proc = commands.abbrev.to_proc

while buf = Readline.readline(">", true) # true means: keep history.
  exit if buf.strip == "quit"
  p buf
end
```



## Sidef


```ruby
require('Term::ReadLine')

var term = %O<Term::ReadLine>.new('Example')

term.addhistory('foo')
term.addhistory('bar')

loop {
    var cmd = term.readline("Prompt: ") \\ break

    if (cmd ~~ %w[q quit]) {
        break
    }

    say "You inserted <<#{cmd}>>"
}
```


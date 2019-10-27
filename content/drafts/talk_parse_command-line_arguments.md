+++
title = "Talk:Parse command-line arguments"
description = ""
date = 2011-12-15T21:53:13Z
aliases = []
[extra]
id = 10251
[taxonomies]
categories = []
tags = []
+++

== Reasons ==
Reasons for this article:

* Getopt documentation is often too sparse to be useful
* Different languages have different pattern syntax (think C's "a:b:cd" getopt syntax)
* As with [[ScriptedMain]], this behavior is as useful as it is obscure.
* getopt is not Google-friendly: the results tend to be C programs embedding the language in question.

The phone call example is just a quick example. I think we should define a more useful CLI example for each language.

 pmath: Perform mathematical operations.
 Examples:
 Usage
 ./pmath.rb -h
 Fibonacci
 ./pmath.rb --fib 100
 e raised to a power
 ./pmath.rb -e 2

--[[User:Mcandre]]

== Command line parameter conventions ==

I have some (currently incomplete) documentation relating to command line parameter conventions that you may wish to consider as part of this project.

http://computerconventions.wikia.com/wiki/Command_line_parameter_conventions

I have also got some part written code that will follow the conventions, but the project is in its infancy, and not ready for prime time here on rosettacode (because it is in a language that I am still learning). It will eventually become available as library code, providing an alternative parser to getopt (but using different conventions). [[User:Markhobley|Markhobley]] 19:37, 6 August 2011 (UTC)

: There are ''many different'' conventions in use. The only one that I've seen that is really close to universal is the one that says that a “<tt>--</tt>” on its own indicates the end of the options and the start of the file arguments. Otherwise, they're all over the map. It gets even more complex on Windows, where some programs are downright idiosyncratic in their command line handling. Because of this, it would really help in this task if it was nailed down exactly what sort of command line options ''should'' be parsed as well as a specific set to actually parse the sense out of. –[[User:Dkf|Donal Fellows]] 10:57, 11 September 2011 (UTC)

== Ruby example ==

What if both <tt>-d</tt> and <tt>-e</tt> are given? Then only the last one applies? That seems kinda fragile. --[[Special:Contributions/166.250.34.11|166.250.34.11]] 19:47, 8 August 2011 (UTC)

== Task needs a task ==

The task description should probably specify what is to be accomplished here.  --[[User:Rdm|Rdm]] 00:32, 9 August 2011 (UTC)

: It sure does... when will the below be added? &mdash; [[User:Crazyfirex|Crazyfirex]] 21:53, 15 December 2011 (UTC)

==Possible task==
Write a program:
# That if it sees an option called <math>b</math> will set an internal variable to Boolean true; otherwise the internal variable is false.
# Accepts an option called <math>s</math> followed by a string then sets an internal variable to the value of the string.
# Accepts an option called <math>n</math> followed by a number (integer or float) then sets an internal variable to the value of the string.
The order of the three options mentioned can vary and and option indicators such as any preceeding '-', '--', or ':' characters are allowed and should follow the conventions of an OS.

Comments please. --[[User:Paddy3118|Paddy3118]] 06:59, 13 September 2011 (UTC)

:Do you mean follow the conventions of an OS which might be different from the one where the command line is being used?  Or do you mean that the program should detect what OS it is running under (and, for an interpreted language, that can change without any changes in the program). Meanwhile, the rules for quote handling under windows look [http://ss64.com/nt/cmd.html baroque].  Also the concept of "followed by a string" can be ambiguous (in the same option? in the next option? in the same option after an = character? ...)  And then there's the handling of -- followed by whitespace that should probably be respected if we are using hyphen to delimit command line options?  Meanwhile, another possibility here (one that seems to have been followed by some "implementations") is that of documenting the default parsing provided by the language.  --[[User:Rdm|Rdm]] 10:36, 13 September 2011 (UTC)
::Hi. 
::# Choose your OS (This may affect the normal option syntax)
::# What is the normal way of adding an option that is stand-alone; i.e. the presence/absence is used to set a boolean variable within your program, and has a name of 'b'.
::# What is the normal way of adding a string value associated with an option name of 's' 
::# What is the normal way of adding a numeric value associated with an option name of 'n' 
::Use extra syntax appropriate for the OS, but implement it in such a way that the order of the options is set by the option names b, s, and n and not hard-coded into the prog.

::Note: No mention of determining an OS. I have tried to leave enough scope to cater for different option syntaxes in different OS's, hoping that what I have nailed down should be workable under more than one OS, and recognising that even within one OS, there may be multiple schemes for specifying options (choose one, probably the one adopted by your languages compiler/interpreter). Please don't invent an options scheme for the task, adhere to some 'standard'. --[[User:Paddy3118|Paddy3118]] 12:44, 13 September 2011 (UTC)
::I guess I didn't want to force adherence to something like <code>progname -b -s 'Some string' -n 1.2345</code> as some might think it doesn't fit their OS or options scheme from some languages library. --[[User:Paddy3118|Paddy3118]] 12:52, 13 September 2011 (UTC)

:Me being me, I might want to add that the program should print a table of (sorted) option name vs option value for sdifferent runs with/without the b option and with options specified in different orders, Hmmm? --[[User:Paddy3118|Paddy3118]] 12:55, 13 September 2011 (UTC)

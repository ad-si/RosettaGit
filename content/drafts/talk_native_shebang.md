+++
title = "Talk:Native shebang"
description = ""
date = 2015-07-13T23:22:55Z
aliases = []
[extra]
id = 16109
[taxonomies]
categories = []
tags = []
+++

=Suggestion for a new wording=
'''Native interpeter for #! aka shebang'''

Write a "script" file in your programming language which starts with #! (aka shebang)
and is followed by a path to a binary executable compiled from the same script file.
The executable must then rebuild itself using the contents of the script file. Following this, it
must print arguments on the command line, separated by spaces.

Background:
A shebang line (first 2 bytes of the file are #!) is a message (magic word) on UNIX
systems which indicates to the loader/exec function that the following line
should be used as a path and arguments to a call to exec, with the argument 0 set to
the executable after the shebang, and argument 1 set to the path of the file itself.

The effect is that by using a shebang line, we can have a file executed by the 
specified interpreter. The interpreter must be a binary file executable on the system.

Programming languages which cannot generate a binary executable should ignore this
task. 

If the programming language has a built-in compiler or functions to access one, then those
should be used. Otherwise the compiler can be called (e.g. gcc/clang).
--[[User:POP|POP]] ([[User talk:POP|talk]]) 16:02, 25 September 2014 (UTC)

:Where should the "binary executable" be written? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:35, 25 September 2014 (UTC)

::As this is a demo case, the path doesn't have to be absolute. A file in the local directory will work. I wrote a test case in C with "#! ./shebang"  --[[User:POP|POP]] ([[User talk:POP|talk]]) 19:15, 25 September 2014 (UTC)

:::So this task needs at least two programs - the binary to be referenced by the "script" and the "script" and the purpose of this task is to write yet another binary? This is starting to sound more than a little silly. Why bother with all these pieces? Why not just write an ordinary program? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:28, 25 September 2014 (UTC)

::The shebang file must start with #! and a binary can't start with #!, so at least two files
::will always be needed.
::The goal is a self compiling application which can be called as a script. I.e. 
::./shebang.x
::will recompile and run the app. 
::For C you need the source file "shebang.x," the executable "shebang" and the compiler (gcc, clang etc.).
::For languages with a built-in compiler, that one should be used instead. I added this to the description.
::Anyway, this is the best I can come up with for a task called "native shebang." --[[User:POP|POP]] ([[User talk:POP|talk]]) 05:59, 26 September 2014 (UTC)

::: It's not at all clear that a binary cannot begin with #! - if anything, that is an OS limitation (for example '#' corresponds to an 8086 AND opcode and the msdos COM file format is just executable code on the 8086 architecture with conventions added in an ad-hoc fashion). It's also not clear that this "self compiling application" does anything - actually, this is sounding more like a viral infection than a useful application. So let's say that the application does nothing - in that case, compiling can be a no-op. Do you see where I am going with this? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:38, 26 September 2014 (UTC)

::Since shebangs are a UNIX convention, I am thinking in terms of UNIX only. I forgot to add the default behavior which was part of the original proposal, which is to print arguments on the command line. I added it to the new wording. Most interestingly I see this as a way to demonstrate ways languages can compile code from within the language itself. The C example is not ideal, since it doesn't use internal facilities, but since not all languages have a built in compiler, this should be allowed. --[[User:POP|POP]] ([[User talk:POP|talk]]) 09:02, 26 September 2014 (UTC)

::After some thinking, I must say this task doesn't seem to make sense in terms of demonstrating programming techniques or language features. Any implementation in any language would just read the "script" file past the first line and then compile the remaining file into an executable. Most of the code is mundane and wouldn't demonstrate much anything unique to the different languages. 
::On the other hand, compiling a file from within an application is something which would be interesting. This would be a distinct task, but the idea would be something like "accept a file as argument and compile the file into a binary executable without calling external applications. Only applicable to languages with a built-in compiler." --[[User:POP|POP]] ([[User talk:POP|talk]]) 10:35, 26 September 2014 (UTC)


==What is this task asking for?==
I don't quite understand what this task is asking for, as arguably what it asks for is how many language implementations already work under the covers. In what way would <code>#!/usr/bin/python</code> not be a reasonable way of achieving it with Python? (All <code>/usr/bin/env</code> really adds is convenient path searching.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 04:30, 3 September 2013 (UTC)
:Near as I can tell (and judging by the examples), the task is asking for you to write a program in language Foo that can be run by a shebang in another Foo language file as a way of executing the second file. For example, a C++ solution would have you place <tt>#!/some/where/run_cpp_file</tt> at the top of a C++ file (call it <tt>myprgm.cpp</tt>), and then you would be able to do the following:

```txt
chmod u+x myprgm.cpp
./myprgm.cpp
```

:At which point <tt>run_cpp_file</tt> (originally written in C++) is called to compile and run <tt>myprgm.cpp</tt>. I personally think this is abusing the purpose of a shebang; languages that aren't compiled to some binary form before being run, such as Ruby or Perl already have existing suitable shebang mechanisms (this is even mentioned as a "difficulty": "Naturally, some languages are not compiled. These languages are forced to use shebang executables from another language, eg "#!/usr/bin/env python""). Languages compiled to a binary form, on the other hand, would more naturally use Makefiles and similar to accomplish the same kind of thing, that is "run with one or two simple commands". [[User:Lue|Lue]] ([[User talk:Lue|talk]]) 02:49, 2 February 2014 (UTC)

::That still doesn't answer my central point: In what way does a Python program that starts with:
::
```python
#!/usr/bin/python
```

::not ''totally'' answer this task? The task description seems to imply that it doesn't, but I don't understand what it means. Do ''not'' answer by talking about languages that normally have a separate compilation step, as that doesn't help to clear up the confusion; state '''''clearly''''' with reference to Python or one of the large number of other languages for which this is possible. (My hunch is that this is all something that doesn't make sense outside the scope of maybe one or two languages. A program is a program! What's more, the line between interpreted and compiled is ''very'' blurry.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 17:25, 19 April 2014 (UTC)

== Problems ==

The intro paragraph talks of a "third language" without identifying a second language (presumably the first language is the unix shell). Why? Is the "third language" a reference to the idea that C binaries are typically being "interpreted" by <code>ld.so</code>?

The C example doesn't work for me (unless a segmentation fault from script_gcc.sh can be described as "working" or a bad interpreter error from echo.c can be described as "working"). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 2014-03-24T01:45:49‎

:If you are sure it doesn't work then flag it as incorrect and put any extra info to help in recreating your problem here in the talk page. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:04, 26 September 2014 (UTC)

Here is what I get on OpenBSD (and OSX). Note that I obtained script_gcc.sh from the unix shell implementation as there was no such file defined in the C implementation.


```txt
$ chmod +x /usr/local/bin/script_gcc.c /usr/local/bin/script_gcc.sh echo.c; ./echo.c Hello, world
./echo.c: line 6: syntax error near unexpected token `('
./echo.c: line 6: `int main(int argc, char **argv, char **envp){'
```


I was able to get it to work on Linux. 

So I think (a) the C implementation should include the script_gcc.sh file, since that is a part of what it needs to run at all, and (b) the implementation should be documented as being non-portable, and only working on Linux. The task description should probably also be changed to more thoroughly document this volume of code that an implementation might need. Depending on how that task description issue handled, it may actually be that this task can only be solved on linux. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:21, 13 July 2015 (UTC)

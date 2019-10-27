+++
title = "Talk:Interactive programming"
description = ""
date = 2011-05-04T19:58:37Z
aliases = []
[extra]
id = 3838
[taxonomies]
categories = []
tags = []
+++

==Omitting languages==
Did I see some sort of "omit" template a while ago for tasks that are impossible in some languages? How do you use that? This task can't be done in a couple languages I know of. --[[User:Mwn3d|Mwn3d]] 18:39, 21 February 2009 (UTC)

:Do you intend to add the omit tag to all tasks that cannot be done in all languages?

:Languages can be grouped together just as tasks can. This is a task to be done by those languages that have a CLI to show how to start the CLI in that language. --[[User:Paddy3118|Paddy3118]] 05:07, 22 February 2009 (UTC)
::I just want to say that this task is un-implementable in some languages (Java, C , C++). I thought I had seen a template that marked it as such for the unimpl bot. --[[User:Mwn3d|Mwn3d]] 05:47, 22 February 2009 (UTC)
:::My apologies Mwn3d, I thought you were wanting to delete the page altogether. --[[User:Paddy3118|Paddy3118]] 06:19, 22 February 2009 (UTC)
::I don't think the template has been created yet.  Check [[User:ImpleSearchBot/Code the bot's code]] to see what it uses. (I believe I have it set to check "Category:Langname/Omit" to see if a task is in that category.)  There's currently no way to categorically drop a language or task from the listings short of removing the language from [[:Category:Programming Languages]] or the task from [[:Category:Programming Tasks]]. (I realize that's not what you had in mind, but it should help clarify how the bot's behavior can be manipulated.) --[[User:Short Circuit|Short Circuit]] 10:56, 22 February 2009 (UTC)
:::It looks like putting it in Category:language/Omit does it:
:::
```perl
 my %omitted = map {$_, 1} $editor->get_pages_in_category("Category:$language/Omit");
```

:::Does that seem right? --[[User:Mwn3d|Mwn3d]] 16:00, 22 February 2009 (UTC)
:::Yup.  That should do it.  I don't think I've created a ''template'' to add tasks to that category, but it shouldn't be difficult. (And it's preferable to adding the pages to the category directly, as using a template allows the check mechanism to change in the future.) --[[User:Short Circuit|Short Circuit]] 18:59, 22 February 2009 (UTC)

==... what was the task?==
I fail to see what concatenation of strings has to do with an interpreter and the latter with the language in which the task is to be implemented. I see a solution, but what was the task? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 19:03, 21 February 2009 (UTC)
:Just starting up a CLI doesn't show enough. I added what I thought would be a simple and achievable 'sub-task' so people would get some comparison of what working in the CLI of each language is like. (Which is hard to do as the feel of a CLI can depend a lot on the editing facilities that it supports). --[[User:Paddy3118|Paddy3118]] 05:07, 22 February 2009 (UTC)

==Interpreter or not?==
Interactive execution does not imply the system is an ''interpreter''. I propose that this task be renamed to "REPL". --[[User:Kevin Reid|Kevin Reid]] 23:57, 21 February 2009 (UTC)
:Interactive execution is most commonly supported by an interpreter and does not mislead. REPL is a less familiar acronym (for acronyms sake)?  --[[User:Paddy3118|Paddy3118]] 05:07, 22 February 2009 (UTC)
:Please expand the acronym before renaming the page. (Thinking of which...The vast majority of the editors on RC right now are folks I'd trust with more privileges than the current default for logged-in users.  I should probably create an "extended privileges" group that users can nominate and second each other for, and redefine the "Bureacrats" group back to its original purpose: Assigning user rights.) --[[User:Short Circuit|Short Circuit]] 06:55, 22 February 2009 (UTC)
::I believe that "REPL" is more recognizable than its expansion, but I can't say for certain. All I care about is that the word "interpreter" ''not'' be in the task name and description. --[[User:Kevin Reid|Kevin Reid]] 13:40, 22 February 2009 (UTC)
:Please '''DO NOT''' change the name of the page. REPL, even as its expansion is change for changes sake. --[[User:Paddy3118|Paddy3118]] 07:16, 22 February 2009 (UTC)

From [[wp:Command_line_interpreter|Command-line interpreter]]:
:''A command-line interpreter (also command line shell, command language interpreter) is a computer program that reads lines of text entered by a user and interprets them in the context of a given operating system or programming language.''

::According to this definition CLI executing a batch file is not a CLI. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:40, 23 February 2009 (UTC)

The above applies to the task. Even if the text you wrote was saved in a file, compiled, executed then the execution results presented on the console, CLI would still apply, as the term applies to how you interact and you would, in effect, be be wrapping a compiler and separate execution to look like an interpreter.

Most fun is the fact that [http://www.googlefight.com/index.php?lang=en_GB&word1=REPL&word2=CLI googlefight] gives CLI as <u>overwhelmingly</u> the more popular term:  :-)

- --[[User:Paddy3118|Paddy3118]] 15:23, 22 February 2009 (UTC)

:But the common definition of "CLI", which makes it so frequent, is "command-line interface", not "command-line interpreter". (Which, by the way, I would have no objection to the use of as the name of this task, if others see it as fitting. My goal here is entirely to promote terminology which does not propagate the impression that Anything Interactive is Interpreted (and therefore also seen as Inefficient). --[[User:Kevin Reid|Kevin Reid]] 22:02, 22 February 2009 (UTC)

CLI is the common acronym, but the full phrase is used as the page title so there should be no confusion. 

As an aside, you should be wary of equating interpretation with inefficiency without further explanation. Forth for example is interpreted, gives very small executables, and can beat C compilers on speed. Inefficiency in programmer productivity, speed, memory footprint ... ? --[[User:Paddy3118|Paddy3118]] 06:08, 23 February 2009 (UTC)

:Interpreting is inefficient because it does not use machine language code as an intermediate layer before ultimate execution. Interpreter deals exclusively with the source language. This puts some obvious constraints on the language, especially when further limitation is an ability to interpret line-by-line. You cannot factor out and manipulate the machine code as compilers usually do. In fact modern compilers have more than just one intermediate code layer. To compensate inefficiency an interpreted language must be of a lower level in order to bridge the gap to the machine language. [[Forth]] perfectly illustrates this point. Well, some interpreters do precompilation stuff, to become more efficient and less interpreters... --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:40, 23 February 2009 (UTC)
::Hi Dmitry, some argue that programmer productivity is worth more than execution speed in most situations and in their eyes an interpreter may well be much more 'efficient' for them. Don't automatically equate efficiency with speed of execution - there can be other concerns. --[[User:Paddy3118|Paddy3118]] 12:20, 23 February 2009 (UTC)

:::Hi! Well, overall economic efficiency depends on many things, and software developing process plays the major role here. If you had to develop a certified software you would find that tighter modification - execution circle would become less efficient because it does not well fit into [http://en.wikipedia.org/wiki/V-model V-model] with requirements of traceability, roles separation etc. On the opposite pole there is so-called [http://en.wikipedia.org/wiki/Test-driven_development TDD]. But that again does not very suited for interpreters, as you need to maintain a large base of tests, rather than test-as-you-type-and-forget. Further if we consider tools mounted above the code with languages of their own, I mean UML et al, we will find that present models of software development rather tend to deeply layered translations, than to direct interpretation. My take is that interpreters will survive as GUI, which brings us to back interactiveness. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:33, 23 February 2009 (UTC)

:I'm not saying we should use "CLI", I'm saying that the usage which makes "CLI" popular googlewise is "command-line interface", not "command-line interpreter", so "CLI" being popular is not an argument for "command-line interpreter" being an appropriate term.  Also -- I suppose I should have just not mentioned inefficiency; it's irrelevant. --[[User:Kevin Reid|Kevin Reid]] 11:13, 23 February 2009 (UTC)
:: Can we agree to use "Command Line Interpreter" as spelled out in full then? --[[User:Paddy3118|Paddy3118]] 12:20, 23 February 2009 (UTC)

== Interpreter behaves like... ==

If an interpreted language is by design able to read the standard input and to interpret it, this is considered like a "command line interpreter", or is just a side effect...? ... E.g. "<tt>awk -f -</tt>" starts AWK reading the source from standard input... and when the input is "finished", it ''executes'' the code... it seems like command line interpretation... can it be considered so? --[[User:ShinTakezou|ShinTakezou]] 23:00, 11 April 2009 (UTC)
:I don't think Awk has a CLI. I would draw the line here and look for a bit more support from the language. Perl's use of its debugger I think would qualify, but both Perl and Python could be called in such a way that they read their program file from standard input. Merely using pseudo-files and/or redirection to enable compilers/interpreters to use standard input in place of a program file name should not be taken, in this task, to be using a CLI. --[[User:Paddy3118|Paddy3118]] 10:24, 12 April 2009 (UTC)

== CLI or line editor ==

I don't quite understand this task. First it talks about command line interpreter, and then requires creating a function. Command line interpreters do not create functions, they interpret and execute commands. If you are creating a function into a program, you are using an editor. For example, a traditional Basic interpreter uses a command mode which also acts as line editor. You can enter a direct command, and it is executed when you press enter. But if you precede the command with a line number, it is entered as a line in the program. In that case, you are using the editor. --[[User:PauliKL|PauliKL]] 21:02, 12 April 2009 (UTC)
:Hi PauliKL, Our understandings diverge. although you might create a function in a file, many languages with a CLI allow you to execute statements interactively that can create functions as well as call functions. If this cannot be done in your dialect of Basic then maybe you could explain how much of the task could be done in it? (Together with the name of the dialect of Basic) --[[User:Paddy3118|Paddy3118]] 22:10, 12 April 2009 (UTC)

::The point is not whether you are creating a file or not. When you are creating a program in computers memory, you are editing the program. So you are using an editor, not CLI. In the old Basic interpreters, this is done by preceding the line with a line number. In Forth, it is done by preceding the line with colon and the word to be defined. Those are just different methods of editing the program. If the task is about CLI, it would have been more logical to show some CLI operation, i.e. to execute a command immediately.  --[[User:PauliKL|PauliKL]] 10:21, 13 April 2009 (UTC)

:But the commands are being interpreted immediately? If I type 1+1 into the Python shell it returns 2. If I type <code>def inc(n): return n+1</code> if creates the definition of a function, as soon as I hit return, I.e. immediately. The definition of a function doesn't print anything special but it is a valid action of the CLI. If you are running bash on linux, you can define functions at the command line, without storing them in a file and call them later. There ''is'' a difference between this and running commands from a file. --[[User:Paddy3118|Paddy3118]] 11:23, 13 April 2009 (UTC)
::We are not talking about difference between running a program from memory and running a program from a file. We are talking about the difference between executing commands and creating (editing) a program.
::If you open a text editor and type a piece of C code, the code is entered into the program immediately. Of course, since C is usually implemented as compiler, you would need to compile the program before running it. But it may not even be necessary to store the program from memory into a file to compile it. If I remember correctly, in the IDE of Turbo C, you could just type in the code and then run it, it was automatically compiled from memory into memory and then executed. The only visible difference to your definition of CLI is that Turbo C used full screen editor instead of line editor. --[[User:PauliKL|PauliKL]] 12:27, 13 April 2009 (UTC)

:I would just go back and quote something you said in your fist paragraph of this section: 
::"Command line interpreters do not create functions, they interpret and execute commands"
:In Python, a language with a CLI, when in its CLI, the creation of a function is a command that is immediately executed. The action of the interpretation of the text typed in that happens to represent a function definition is to create a function 'internally'. You seem to think that a CLI's task is restricted to executing expressions with an immediate visual output, or are you merely pointing out that the notion of a CLI is not as clear cut in Basic and forth as you use the same included editor as a CLI but in different ways when not needing longer-term storage of a program? if the latter then in Basic, if you have to add line numbers at the command line to store any function to memory then I would just go ahead and show typing in of the line numbers in the function definition, and presumably leaving off the line numbers when you want the immediate effect of calling the function with arguments so that it prints a result. You could do something similar for Forth I guess. --[[User:Paddy3118|Paddy3118]] 14:03, 13 April 2009 (UTC)

::I did not say that execution would require visual output. Where did you get that idea? The keyword here is ''execute'', whether it is some calculation or controlling a device or whatever. It seems that you do not understand the difference between '''executing''' code and '''editing''' a program. You seem to think that defining a function executes something. It does not. Defining a function means that you add the function code into the program. That is editing. Even if it is done with a line editor. And even if some pre-compilation is done. (The line editors of most Basic interpreters actually pre-compile each line that is inserted into the program.) --[[User:PauliKL|PauliKL]] 15:46, 15 April 2009 (UTC)

::: In a CLI as thought here, defining a function executes something: the definition of the function. Defining a function means the interpreter e.g. creates a new symbol-code bound. It's absolutely not editing the code; how the interpreter stores the definition, is not essential. Editing the code means using a text editor; the interpreter does not know nothing about you editing a file, until you run the interpreter over that file. Instead, the CLI '''is''' the interpreter itself (it is a way it can operate: instead of feeding it with a complete source, you interactively give commands to it, according to the same syntax you would use while writing the source in your preferred editor). Of course, to make a good experience to use the interpreter in such an interactive way, the CLI is also a line-editor... but once you hit enter, what you've written is interpreted, so even you can recall a previous input line, that line was already interpreted, and changing it will just make you able to re-run a changed copy of it. I.e. it is not luckly like a telnet session. (See readline library). --[[User:ShinTakezou|ShinTakezou]] 17:07, 15 April 2009 (UTC)

::: '''Seen''' the BASIC code. Does it work even if you remove the 10? --[[User:ShinTakezou|ShinTakezou]] 17:14, 15 April 2009 (UTC)

:Hi PauliKL, Forth and basic come from a time (the 80's), when it made sense to include an editor that could save programs to more permanent storage, as part of the language. Since then it has become more common to separate the programming language from any editor. You seem to stumble over the fact that in the modern use of the term CLI, you can define functions, and classes etc as part of a CLI session. I say you can and so the task is valid, you say you can't and so the task is invalid. I have already sugested that you are at liberty to state that the addition of line numbers in your version of Basic which you say is necessary for the creation of functions is viewed as an editor function for that version of Basic (Their are other versions of Basic that don't even require line numbers). We may have a difference in semantics and maybe your explanation for your version of Basic will avoid any confusion. To say that what is correct nomenclature for basic is true for other languages doesn't help. We have been discussing this issue for several days now and their are several entries on the page by other authors who don't see a problem. --[[User:Paddy3118|Paddy3118]] 19:50, 15 April 2009 (UTC)

: FWIW, with [[Tcl]]'s CLI (or a Tcl script) you create new executable symbols (commands, functions, procedures, whatever you want to call them) by issuing a command to create them. Thus, I think that to say that a CLI can't create new symbols is to introduce a totally arbitrary distinction in general (that just happens to correspond to ''some'' language's restrictions). The key distinction is between those where you can type in code interactively, and those where you have to describe it completely separately from executing it, typically because an explicit compilation step is required. And yet, I have seen a CLI for [[C]], so I suppose anything is possible with enough cleverness... –[[User:Dkf|Donal Fellows]] 09:12, 23 January 2010 (UTC)

==Omit AWK?==
Rather than include the current entry? --[[User:Paddy3118|Paddy3118]] 07:21, 23 January 2010 (UTC)

==Where's Lhogho??==
A quick search shows that the language is a version of Logo; a Logo compiler. I removed the Lhogho entry as it didn't complete the task. 

The best way to add a Lhogho example would be to put it under the Logo language and point out any differences it has to any existing Logo implementation or what makes it Lhogho specific if anything. There would be no need for duplicating any example in its entirety. --[[User:Paddy3118|Paddy3118]] 08:40, 14 June 2010 (UTC)
:Point taken. I should've read the blurb on the task too. --[[User:Axtens|Axtens]] 08:46, 14 June 2010 (UTC)
::Mind you, Lhogho doesn't have an interactive mode AFAICT

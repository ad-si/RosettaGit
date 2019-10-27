+++
title = "Talk:Shell one-liner"
description = ""
date = 2012-09-21T00:57:03Z
aliases = []
[extra]
id = 3029
[taxonomies]
categories = []
tags = []
+++

==Which Shell?==
Which shell? Normally the programming language does not specify existence of any shells or others environmental tasks. What happens if the program is run as an [[OS]] driver, a system service, or without any [[OS]] at all on the bare board? Otherwise, how does this task differ from [[Execute a System Command]]? Does spawning a shell qualify? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:15, 26 August 2008 (UTC)
:I guess we could restrict it to mean systems that have command line shells such as bash/tcsh/... on Unix systems, cmd.exe on Windows, or ''[http://www.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html posix-like]'' shells. Maybe we should state what shell the command line is compatible with? --[[User:Paddy3118|Paddy3118]] 13:55, 26 August 2008 (UTC)

The examples on the page seem to indicate that the task is running a line of the programming language ''from'' a shell; and not doing anything of the shell from the programming language. I am not sure exactly what you are allowed to use; because the OCaml example just echoes a string and pipes it into the ocaml program. You can do that with any language with an interpreter that reads from standard input, so it seems kind of trivial. --[[User:Spoon!|Spoon!]] 19:25, 26 August 2008 (UTC)

: As far as I know one-liners are just anything you can fit into a line (or more...!) of a shell, to do a task. So it is ok if you feed an interpreter with input through pipe, at least I believe so. --[[User:ShinTakezou|ShinTakezou]] 11:02, 9 February 2009 (UTC)

== About C and /tmp ==

Yes but this way it won't work on environments that have not /tmp, or use other convention for directory separator (e.g. \ instead of /); I suppose it would have not worked anyway out of a posix-like shell... --[[User:ShinTakezou|ShinTakezou]] 11:00, 9 February 2009 (UTC)
: It wouldn't have, which is why I felt justified adding /tmp, touch and chmod.  /tmp was the only widely-available approach I could think of for temporary files. I would have preferred a means to have gcc output to stdout, and then execute that, but I don't know of any shell or common program that would allow you to execute a raw binary fed in by way of STDIN. --[[User:Short Circuit|Short Circuit]] 16:35, 9 February 2009 (UTC)

The C example could be quite shorter:
 echo 'main() {printf("Hello\n");}' | gcc -w -x c -; ./a.out ; rm a.out
(But it would overwrite a possible 'a.out' in the current directory...) [[Special:Contributions/187.25.221.53|187.25.221.53]]

==Remove C and Autohotkey?==
... For relying on the underlying shell too much. It seems to me that that phrase was put in the task definition to stop the shell being used to create temporary files, pipe things around, execute multiple commands etc. --[[User:Paddy3118|Paddy3118]] 06:31, 5 June 2009 (UTC)
: I can't talk about Autohotkey; but for C (and Algol), to me it would possible to switch che code put in this discussion page (I'll do that after testing). --[[User:ShinTakezou|ShinTakezou]] 13:00, 5 June 2009 (UTC)

==BASIC example misinterprets task?==
The idea was to invoke the language - in this case BASIC, from a command shell outside of the language, together with one line of text which would become the program for the language to run. The basic example seems to be invoking a command shell and another program from within the BASIC shell. --[[User:Paddy3118|Paddy3118]] 06:23, 26 January 2010 (UTC)
: I guess I misunderstood the task. There's no standard way to do ''that'' for BASIC (many implementations don't have a non-interactive mode, or else can't take their input from stdin). I'll remove it. -- [[User:Eriksiers|Eriksiers]] 00:51, 28 January 2010 (UTC)

:: I think I also misinterpreted the task. When I read SPECIFY and EXECUTE, I understood that to mean to input a program for a particular language and then run it from the (shell) command line. If you had said, "show how to execute a program in a language from a shell (command line or "prompt") ...", and the point here is that it shouldn't matter if the program is 45,000 statements long (or only 3), I would've assumed you wanted example(s) to show the syntax of how to run a program (for a particular language). Of course, I'm assuming that "short" had to do with the size of the program, not the duration of it's execution or smallness of output. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:49, 23 March 2012 (UTC)

==One Line?==
The [http://rosettacode.org/mw/index.php?title=Shell_one-liner&diff=22059&oldid=22058 original aim] of the task specified one-line. I wondered why was it was changed to allow multiple line examples?

The current edited task description seems to be at odds with the task title, as "one-liner" has a well known meaning of one line; and not relying on the shell to concatenate several lines into one. --[[User:Paddy3118|Paddy3118]] 01:50, 9 March 2010 (UTC)

:I will be changing the task description back to one line to match the title. This will invalidate at least one example. --[[User:Paddy3118|Paddy3118]] 08:25, 24 September 2011 (UTC)

== Incorrect solutions ==

Several examples don't make sense.  I marked the C incorrect for now, but Java, first example of Go, Objeck, and PureBasic are in the same category that pipe source to a file, compile it and run the resulting exe, and I think they should all be marked incorrect, too.

Second class of incorrectness: not running from shell at all.  These include AutoHotkey, ZX spectrum basic, Matlab, MUMPS, and REXX.  REXX example talks about how to run an executable file which is way off topic; all others substitute "shell" with "intepreter".  These, in my mind, are undoubtedly incorrect, more so than the previous category even.

: --- Where does the REXX example talk about anything?   For that matter, where is the REXX example? -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:13, 23 March 2012 (UTC) 
:: See the history, the example mentioned was [http://rosettacode.org/mw/index.php?title=Shell_one-liner&oldid=121477#REXX marked for deletion] Sept 25, 2011 along with several others, and was later deleted when it was not fixed. [[User:Xenoker|Xenoker]] 17:46, 23 March 2012 (UTC) 

There is actually a third category, where the braindead compiler/intepreter can eval a file without producing an exe but would not do so with text supplied on commandline or stdin.  These include second example of Go and Oz.  I'm a bit indifferent about this category, it sort of fits the spirit of the task.

Also, the "line too long" template probably can be excused from examples in this task, seeing that some languages are pretty verbose and there isn't a reliable way to break long lines across different shells. --[[User:Ledrug|Ledrug]] 20:16, 25 September 2011 (UTC)

: The current task description requires to "specify and execute a short program in the language". I think that examples can pipe source to file, compile it and run the resulting exe; because nothing in the task description prohibits such actions. The ZX Spectrum Basic example looks correct because it runs from the "system prompt". The AutoHotkey example looks wrong; it goes backwards and solves [[Execute a system command]], not this task. --[[User:Kernigh|Kernigh]] 21:06, 25 September 2011 (UTC)

:: This is again the issue of "letter of the task" vs "spirit of the task", which is always muddy.  The utility value of a shell one-liner is that for certain simple jobs, you can invoke an intepreter in a simple manner without undesired side effects such as having to open an editor or leaving behind intermediate files.  The echo-compile-run methods don't quite achieve that.  Plus, if that's allowed, then pretty much any program can be written this way (editor of the champions is 'cat', you know) which renders the task pointless.
:: As to the ZX basic thing, you can argue that it's one line under a prompt, but there's the fact that it's using current interpreter prompt, not quite the same as invoking a separate interpreter.  You probably would agree that typing 'ls' under current interactive shell is not a 'shell one-liner', but typing 'sh -c ls' is, same idea. --[[User:Ledrug|Ledrug]] 00:16, 26 September 2011 (UTC)

:::+1 on letter vs spirit of the task. I would have thought most languages with only compilers might be better off omitting themselves from this task, but, that said, if a compiled language could start a web server with a one-line program then an example using cat piped to a compiler etc ''might'' be of use. (That utility thing you mentioned Ledrug). --[[User:Paddy3118|Paddy3118]] 03:50, 26 September 2011 (UTC)

:::: That does not depend much on what string eval capability is present in the language, but rather what other stuff is present on the system.  Suppose you pull the full source code of Linux kernel and type <code>make && make modules && make install</code>, then proceed to claim that the most significant part of the kernel is make's one-liner power -- no you wouldn't.  Would you? --[[User:Ledrug|Ledrug]] 04:17, 26 September 2011 (UTC)
:::::I was thinking of a hypothetical compiled language that came with a web server as part of its standard libraries (Boo perhaps)? Sitting around waiting for Linux to compile from source would be of little practical use. cat'ing a small program to compile and link to a pre-compiled web server library ''might''. but as I was alluding to earlier - its in a grey and fuzzy area. --[[User:Paddy3118|Paddy3118]] 04:41, 26 September 2011 (UTC)

Currently, the C implementation has this "incorrect" notice:

:<nowiki>{{incorrect|C|Solution doesn't have much to do with the C language, but rather a shell and compiler example.  Pending objections (or in the unlikely event, a true fix), code probably should be removed and task omitted.}}</nowiki>

But this is not a meaningful objection.  The task asks for a shell one liner, so objecting to a shell example is pointless.  Likewise, the C language is typically implemented using a compiler, so objecting to the compiler is also pointless.

So I am going to remove this from the main page. --[[User:Rdm|Rdm]] 13:47, 26 September 2011 (UTC)

:: Good, I left the note there in case people think differently about it.  Now that I seem to be holding the minority opinion, I guess the concensus is C and Java etc examples are ok. --[[User:Ledrug|Ledrug]] 21:49, 26 September 2011 (UTC)

::: As a general rule, I think we encourage "best efforts" here.  We are trying to encourage cross/language understandings and representations. --[[User:Rdm|Rdm]] 22:09, 26 September 2011 (UTC)
:::: I agree, to the extent that the example provides for good or common practice.  For C a one-liner is actually used often (to find out a system constant when porting stuff, for example).  But for Go or Java it would appear quite unnatural -- how often do you see a java programmer coding without an IDE? --[[User:Ledrug|Ledrug]] 22:28, 26 September 2011 (UTC)

==Whilst/while==
I didn't know that whilst was [http://oxforddictionaries.com/definition/whilst used mainly in Britain]. Some sentences still seem somehow better with the word rather than using while, but now that I am aware of my parochial Englishness I am quite happy to go along with swapping it :-) --[[User:Paddy3118|Paddy3118]] 04:07, 26 September 2011 (UTC)
:Heh I didn't know either. You don't need to change if you don't want to. They mean the same thing. I guess I understand now why I thought it was an unnecessary word: in my country, it isn't necessary. In any case, I guess the nature of this site means we should accept all sorts of language subtleties (spoken, written, programmed, or otherwise, where it doesn't get in the way of clarity). You can stick with "whilst" where you like it and I'll stick with "while" everywhere. --[[User:Mwn3d|Mwn3d]] 19:52, 26 September 2011 (UTC)

==Liberty BASIC oddity==
Is the Liberty BASIC one correct? Looks like it got doubled somehow.
[[User:Axtens|Axtens]] 00:57, 21 September 2012 (UTC)

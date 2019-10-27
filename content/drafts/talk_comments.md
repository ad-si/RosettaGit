+++
title = "Talk:Comments"
description = ""
date = 2016-05-10T05:35:58Z
aliases = []
[extra]
id = 2146
[taxonomies]
categories = []
tags = []
+++

== Pascal Comments ==

The Pascal section claims that '{' and '}' are Turbo Pascal extensions. However the Pascal Standard, ISO/IEC 7185:1990 explicitly contains:
: '''6.1.8 Token separators'''
: Where a commentary shall be any sequence of characters and separations of lines, containing neither
: } nor *), the construct
: ( `{' | `(*' ) commentary ( `*)' | `}' )
: shall be a comment if neither the { nor the (* occurs within a character-string or within a commentary.
: NOTES
: 1 A comment may thus commence with { and end with *), or commence with (* and end with }.
: 2 The sequence (*) cannot occur in a commentary even though the sequence {) can.

Therefore { and } are comment delimiters in standard pascal. Note that comments like
 (* this }
or
 { this *)
are valid Standard Pascal comments, but not valid Turbo Pascal comments.

Another interesting quote from the standard:
: '''6.1.9 Lexical Alternatives'''
: [...]
: The comment-delimiting characters { and } shall be the reference representations, and (* and *)
: respectively shall be alternative representations (see 6 .1 .8).
I guess it couldn't be more explicit :-) --[[User:Ce|Ce]]

::Thanks for the clarification! I guess my memory was faulty. Personally, I always used { } in Turbo Pascal. --[[User:IanOsgood|IanOsgood]] 18:33, 29 September 2007 (MDT)

== [[C]]: #if 0 and syntax errors ==

The assertion was made that
 #if 0
 This isn't valid.	 
 #endif	
would cause a compile error due to an unmatched apostrophe in a character literal. This is false. The preprocessor removes this text from the source before the compiler has a chance to parse it. --[[User:IanOsgood|IanOsgood]] 18:59, 29 September 2007 (MDT)

: Did you actually ''read'' the text you removed? The ''Compiler'' does not see the unterminated character constant, but the '''Preprocessor''' does. See e.g. http://c-faq.com/ansi/ifdefsyntax.html
: Unfortunately I don't have the C standard, so I cannot quote directly from there, but I double-checked with C++ that both CD2 (latest public draft of the 1998 version) and n2009 (a draft for the next section) contain wording identically in both (i.e. with exrtemely high probability also in the real standard) that this is true in C++ as well. More exactly, character literals are preprocessing tokens, and translation to preprocessing tokens happens in translation phase 3, while processing of preprocessing directives (this includes #ifdef) doesn't happen until translation phase 4. --[[User:Ce|Ce]] 05:17, 30 September 2007 (MDT)

::Egads! The reason I disbelieved you is that I have never used a C compiler which actually obeyed this part of the standard. The snippet compiles fine in gcc 3.4 and 4.0.1. I even tried with '''-std=c99 -pedantic-errors''' options for all supported standards. I also tried with just the preprocessor '''cpp''' in isolation.
::I believe this section should be prefaced with "'''Standard:''' ANSI, but not GCC" or a list of compilers which exhibit this behavior. Which C/C++ compiler are you using which fails on the above snippet? --[[User:IanOsgood|IanOsgood]] 09:57, 30 September 2007 (MDT)
::BTW, thanks for the detailed references, Ce! Perhaps Rosetta Code should be more like Wikipedia and encourage references on the pages themselves. It might avoid this kind of argument in the future. --[[User:IanOsgood|IanOsgood]] 10:58, 30 September 2007 (MDT)

:::Any reference should be linked. As long as it's relevant to the project, language or code used. Even though we provide a lot of code in Rosetta Code, sometimes this isn't enough. I don't see why we shouldn't allow anyone to read more on a given subject. This is of course true for this issue, but it also is for any other. --[[User:CrashandDie|CrashandDie]] 01:11, 5 October 2007 (MDT)

== [[Python]]: move docstrings to its own task ==

Python docstrings are not comments and are not ignored by the compiler/interpreter. Strings in the source are also not ignored - they are probably crated and destroyed at runtime, and are not the way to comment code.

We can add the Pythonic way to comment code:

```txt

if 0:
    print 'this line is commented out'
    print 'so is this line'

```


==Lisp: Interpreter line==

What's with the ??? for the interpreter line? --[[User:Short Circuit|Short Circuit]] 21:21, 10 December 2007 (MST)
:I've never seen some of those types of comments in generic Lisp programs, so I think the specific interpreter should be mentioned (or the text moved to [[Common Lisp]] if appropriate). --[[User:IanOsgood|IanOsgood]] 08:19, 11 December 2007 (MST)


==task wording clarification==
I wish (before this was promoted to a task) that the phrase   <big> ··· ''that's <u>completely</u> ignored'' ··· </big> was changed to something along the lines of:   ''that's largely ignored''.   (The underscoring was added my me.)   Of course,   ''largely''   would then probably have to be defined in some manner.

Some interpreters fall into the category of   ''not''   completely ignoring comments.   The '''REXX''' computer programming language, for instance, ignores comments in the usual sense in that it doesn't execute them,   but REXX doesn't ignore them completely.   The comments are still there and can be displayed as part of the statement when tracing   (via the '''trace''' statement or options),   and also for the   '''sourceline'''   BIF, where the complete source line (including comments, or in the case of no REXX statement, ''only''   comments)   can be retrieved   (and then, for instance, displayed and/or examined.   There must be other computer programming languages that don't   ''completely''   ignore comments, especially/probably those that support interactive debugging.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:58, 10 May 2016 (UTC)

: Sure - any language with a sufficiently large toolset tends to acquire some tools which introspect into comments. Comments are a convenient mechanism for ad hoc additions without syntax changes (which would require changing all the other tools - a slow process). 

: Also, textual data types can be used as comments, which can further blur these lines. 

: Anyways, you do have rights as an rosettacode editor to fix the wording. Just make sure the task and its description stays focussed on the task (and reasonably concise). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:48, 10 May 2016 (UTC)

:: I thought that once a Rosetta Code draft task gets promoted to a (full) task, the essential wording of the task is to remain largely intact.   Changing the word   '''completely'''   to   '''largely'''   would be a big step, and I would think the original author (might) want to possible change it (or add a modifier to the task's wording).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:35, 10 May 2016 (UTC)

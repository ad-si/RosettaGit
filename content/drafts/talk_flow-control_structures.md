+++
title = "Talk:Flow-control structures"
description = ""
date = 2013-12-19T07:47:08Z
aliases = []
[extra]
id = 2187
[taxonomies]
categories = []
tags = []
+++

I'm not sure why there is talk about Exceptions in the C++ section of this article, but I also don't want to remove it since it shows very good examples. Is there a nice way to move it over to the C++ section of the Exceptions article without destroying the code that's already there (it has a nice example too)? Also, we should probably update it to a newer compiler version. I haven't worked much with Exceptions in C++ so I'm not sure how to do it correctly.
: You know, looking at this page and the [[Exceptions]] page, I'm beginning to think they ought to be merged.  Exceptions are a form of flow control, and the Flow Control article is pretty empty without them. --[[User:Short Circuit|Short Circuit]] 13:22, 4 November 2007 (MST)
:: Though many languages lump them together, ''Exceptions'' (signalling errors) and ''Non-local Jumps'' (for flow control) are actually different things. And some languages have different features for them, or have different costs associated with those features. So maybe it makes more sense to have a [[Non-Local Jumps]] page in the Flow Control Category, describe the similarities there, and possibly link to [[Exceptions]]. OTOH, one then needs a good example for this page. [[User:DirkT|Dirk Thierbach]] 11 November 2007

[http://xkcd.com/292/ What happens when you use a goto]

Should break and continue be in the IDL example here? I thought they were for loops. --[[User:Mwn3d|Mwn3d]] 13:16, 15 December 2007 (MST)
:'break' at least works for any "innermost structure" -- it'll break out of the 'current loop' but also out of the 'current if-branch' or the 'current switch-branch' etc. [[User:Sgeier|Sgeier]] 22:36, 12 June 2009 (UTC)

'''How much discussion on the main page'''

The Unicon/Icon example provides explanations of details of the language relating to flow control.  My sense is that much of this should be referenced but the detail be placed elsewhere.  The question is where? For example:

* Goal Directed Evaluation and generators are really a paradigm but there isn't a paradigm that talks to this.  How does one get created?
* Some information could go on a language page but I don't really see other examples.
* Other information like a list of control elements and the lack of a goto are probably worth mention.  Again possibly through a reference, but where to?

--[[User:Dgamey|Dgamey]] 23:54, 12 April 2010 (UTC)

The typical RC example is short on explanation. If you can say something informative about the code, ''please'' put it right on the page and not buried behind a link. —[[User:Kevin Reid|Kevin Reid]] 02:35, 13 April 2010 (UTC)
:: Please have a look at the Icon/Unicon example on this page it goes into great detail like I haven't seen on the site before.  I'm asking a question about balance of explanation. What would really be helpful would be a couple of balanced examples. --[[User:Dgamey|Dgamey]] 03:54, 13 April 2010 (UTC)

== Rename to Branching structures and jumps ==

This task might be better named as "Branching structures and jumps". The term "flow control" is usually associated with data streams, and refers to things like hardwire hand shacking, xon/xoff pacing and ack/nak protocol.

[[User:Markhobley|Markhobley]] 14:29, 19 May 2011 (UTC)

== I have added the multi-processing JOB command to the MUMPS example ==
Since I think that generally, anything that initiates the flow of code, involving the program counter or stack pointer is part of Flow Control, I have included the JOB command that "duplicates" and initiates a different flow of control.

== REXX Return ==

The text does not consider the Return from a subroutine or function. It should! --[[User:Walterpachl|Walterpachl]] 17:12, 28 January 2013 (UTC)

::: The '''RETURN''' text has been updated. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:44, 17 December 2013 (UTC)

:::: What I see is wrong :-( Please reconsider the Return from a, internal subroutine or function! ..[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:39, 17 December 2013 (UTC)

::::: The RETURN text mentions "which could be a subroutine or function)" ... and passed control to the invoking program  (the program that called or invoked the subroutine/function).   If you think the phrase could use clarification, then feel free to add more verbage.   It's sometimes not clear to readers what is being referred to which REXX code is being executed (or being called/invoked) when the REXX code calls/invokes an internal REXX program versus an external REXX program.   This can be an area of misunderstanding, and certainly, an area of confusion.   I'm not sure how to succinctly put that into words that is concise and easy to understand without getting to wording that is too obtuse.   I don't want to re-write a REXX tutorial on the RETURN statement. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:07, 17 December 2013 (UTC)
--------------------------
How about this:

```txt
RETURN returns control, and possibly a result, from a Rexx program or routine to the point of
its invocation.
If no internal routine (subroutine or function) is active,
RETURN and EXIT are identical in their effect on the program that is run. (See EXIT.)
If called as a function, an expression must be present, its value is returned to the point of
invocation.
If called as a routine, control is passed back to the caller.
If an expression is present, it is evaluated and the Rexx special variable RESULT is set
to the value of expression. Otherwise the special variable RESULT is dropped.
```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:01, 17 December 2013 (UTC)
:::::: The following example shows all the flavors (and a surprise when I invoke the program as rexx ret 1 (on Windows using ooRexx)! The control structure seems to get broken)

```rexx
Call sub 1
s=sub(2)
Say 's='s
sub: parse Arg a
Select
  When a=1 Then Say 'we''ll return to line 2'
  When a=2 Then Say 'we''ll return to the expression in line 2'
  Otherwise Say 'we''ll exit the program'
  End
Return a
```

Output:

```txt
we'll return to line 2
we'll return to the expression in line 2
s=2
we'll exit the program
```


== REXX Exit ==

This sentence is incorrect (if the program with the exit is an external program)

"If the invoking program is a REXX program, it also is terminated."

consider

```rexx
/* Caller */
say 'I am the Caller'
Call called
Say 'and back in Caller. Got' result 'from Called'
```


```rexx
/* Called.rex */
say 'I am Called'
Say 'Now I am going to Exit'
Exit 4711
```

Output:

```txt
I am the Caller
I am Called
Now I am going to Exit
and back in Caller. Got 4711 from Called
```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:24, 17 December 2013 (UTC)

::: The '''EXIT''' text has been updated. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:39, 17 December 2013 (UTC)

-----

: And an additional idea/question: ooRexx has Call on exception handling (similar to Signal On). Do other Rexxes have that too?? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:28, 17 December 2013 (UTC)

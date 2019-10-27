+++
title = "Talk:Jump anywhere"
description = ""
date = 2012-12-12T20:34:41Z
aliases = []
[extra]
id = 9723
[taxonomies]
categories = []
tags = []
+++

== Draft task ==

My new task description might not match my intent. I have a short Ruby example, but I want to add more Ruby code, and some C code. --[[User:Kernigh|Kernigh]] 02:41, 19 May 2011 (UTC)

:OK, I'll monitor and ask about changes needed for the Python as things develop :-)
--[[User:Paddy3118|Paddy3118]] 02:54, 19 May 2011 (UTC)

:: Tcl (8.6) has pretty much the same capabilities as Python in this area. (When people ask for general jump anywhere capabilities, we tend to be somewhat brusque in our dismissals; it's totally against nice structured programming as well as being evil.) I'll keep an eye on the evolution of the page to see whether I want to omit or implement. Generally, should continuations be a solution of this task? Or is it more about setjmp/longjmp? –[[User:Dkf|Donal Fellows]] 17:51, 19 May 2011 (UTC)
::: Just to be clear, this is not to diss having a task on continuations. It's just they're not simple jumps; if we split, we can easily justify another task specifically for them. –[[User:Dkf|Donal Fellows]] 19:09, 19 May 2011 (UTC)

== Demonstrate a local jump and a global jump ==

It might be better for the task to be demonstrate a local jump and a global jump. (Currently the requirement appears to be for a global jump, requiring a new task to be created for a local jump).

[[User:Markhobley|Markhobley]] 14:19, 19 May 2011 (UTC)

I propose that unwinding of the call stack is moved to a separate task, and rename this task to "Demonstrate local and global jumps". Keeping the tasks separated, enables the solutions to remain simple, and separates aspects. --[[User:Markhobley|Markhobley]] 16:40, 6 June 2011 (UTC)

: What is a "global jump"? Continuation? Longjump (which involves stack)? --[[User:Ledrug|Ledrug]] 03:04, 11 June 2011 (UTC)

::I assumed that a local jump is to a label within the same procedure (or block of procedures), whereas a global jump is to anwhere within the program. --[[User:Markhobley|Markhobley]] 08:12, 11 June 2011 (UTC)

::: That would normally involves unwinding the stack, then, because if you are going out of a proc or function and not coming back, you have to clean up what's already allocated for your current scope, or everyone gets confused -- unless your language doesn't have a stack concept, which is rare. --[[User:Ledrug|Ledrug]] 08:39, 11 June 2011 (UTC)

:::: Yeah. It depends on the language. A local jump within the same procedure would probably not need an unwind. A global jumb could need an unwind, and the unwind may be done at source, prior to the jump or at destination following the jump. Jumping out of a procedure (following a call) is not usually a good idea, although it may be for a critical reason, such as an abort. I would just stick a note saying "the stack will need to be unwound, before this jump is performed" and cross reference to the stack unwinding task. Some languages may have a garbage collector, which removes the need to unwind the stack. Maybe it would be tidier to split the task into Jumps/Local, Jumps/Global and Jumps/With stack unwinding. --[[User:Markhobley|Markhobley]] 10:20, 11 June 2011 (UTC)

== Signal use in Rexx ==

__Warning__: Not only jumping into Do loops (as noted under REXX) is usually an error.
Jumping into or within Do groups is equally wrong.

:: Jumping (in REXX, '''signal'''ing) within a '''DO''' loop is legal and is illustrated within the Regina PDF documentation. [What is ''wrong'' is merely a judgement call.]  It's the execution of the '''DO''' or the '''END''' statement [after a '''signal''' instruction is executed] that is illegal and thus causes a '''syntax''' error. I don't want to endorse nor propose its use. I was merely showing what is legal, not what is considered bad practice (or wrongness) by some.  I don't pontificate what should be considered/practiced/used when it comes to what is legal vs. what is bad programming. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:02, 9 December 2012 (UTC)  

::: Your text:
"Note: some REXXes don't allow jumping into a DO loop, although the language 
specifications appear to allow it, as long as the END or the DO loop 
isn't executed. The following used PC/REXX to illustrate this example."
is, in my opinion too liberal. It's not only Do loops where Signal is bad!
--[[User:Walterpachl|Walterpachl]] 08:22, 10 December 2012 (UTC) 

:::: What exactly is too liberal about my statement?  I don't understand your concerns about what is allowed as far as the REXX language specification (as per the rules of using the SIGNAL instruction).  I have no opinion about the validity of the REXX specifications, only that what was shown is a legal construct --- especially if the code is actually shown in the Regina REXX language specification ("The Regina Rexx Interpreter" PDF document). As for your opinion about the "badness" of the '''signal''' instruction, that's an opinion, but '''signal''' is still part of the REXX language, warts and all. -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:00, 10 December 2012 (UTC)
::::: Too liberal: That you talk only about DO LOOPS when do groups are affected as well. --[[User:Walterpachl|Walterpachl]] 11:44, 10 December 2012 (UTC)

:::::: Then you should say what you mean. Restricting my comments about the legal syntax of the REXX '''signal''' instruction as mentioned in a language definition shouldn't be considered ''liberal''.  If I mention that the (Regina REXX) documentation mentions ''only'' '''DO''' loops (and not '''DO''' groups), then that shouldn't be labeled ''liberal''; I'm just parroting what the documentation says, not interpreting it.  The '''signal''' instruction shouldn't effect '''do''' groups, just active '''do''' loops (and a few others such as '''select''' groups and '''if''' statements). I say ''shouldn't'' because the Regina REXX documentation says one thing but implements another. Please take care about labeling the statements/opinions of others with judgemental names. Also note that the Regina documentation says the construct that I quoted as being legal, but the Regina REXX documentation appears to waffle (with statements like: ''Although TRL1 and TRL2 allow this construct, it will probably be disallowed in ANSI''). I feel very uncomfortable and uneasy when language (specification) documentation uses such words like ''will probably''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:22, 10 December 2012 (UTC)
::::::: Is this the right place to discuss the Regina documentation which you seemingly do here?  I haven't touched your text 'over there!'. Just tried to warn others (and maybe even you) about use of Signal. Please leave it at that and/or rephrase your text in the task if I convinced you to some extent.
I am talking here about classic Rexx and ooRexx, by the way. --[[User:Walterpachl|Walterpachl]] 20:04, 11 December 2012 (UTC) 

: Just exactly what is it that you want to be rephrased in my text (in the REXX example)? I showed an example of a '''signal''' instruction, along with the disclaimer that some REXXes don't allow the example's method of using it (the '''signal''' instruction), although a language specification that says it's legal --- along with the ''exact'' example given in a REXX (Regina) document (I consider Regina REXX to be one of the classic REXXes).  As far a what is or isn't the right place to discuss the Regina REXX language documentation, it's a good place as any as we're discussing what's legal as per the '''signal''' instruction in the REXX example shown, and also, most REXX programmers have access that particular document.  I used Regina's documentation because it had the example (REXX code) that I used verbatim.  I showed a working example of a '''signal''' instruction as per the task description (jump anywhere).  I didn't include examples of how it can fail;  every instruction can fail in some manner.  Most instructions have caveats, exceptions, and ''gotchas''.  '''a=a+1''' could produce a result that is unexpected, but that won't stop me from showing how to increment a REXX variable (and not showing a counterexample of possible failures). Also, I don't feel comfortable in pontificating what (REXX) statements should or shouldn't be used for this 'n that and/or under what circumstances, or even judging what (REXX) statements are "bad".  Showing a (SIGNAL) failure situation without explaining why the failure occurred isn't a constructive way to warn other programmers.  The conditions of why the failure (of the below shown example) occurred should be accompanied with a brief explanation of why that failure is an example of coding an ''illegal'' '''signal''' instruction construct and will cause a '''syntax''' error in REXX. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:02, 11 December 2012 (UTC) 

:: This "discussion" is getting way too scrambled. I try to tell you what I want.
Replace your Regina oriented text at the task with something like this:

"Signal should only be used for very global jumps and condition handling since Signal within or into Do loops, do groups or other control structures may not work as desired" --[[User:Walterpachl|Walterpachl]] 05:03, 12 December 2012 (UTC)

: Sorry, but I will '''not''' pontificate to others that certain (REXX) statements should ''only'' be used for thus and thus (for whatever reason, certainly the least of which is that the '''signal''' instruction is behaving as described in a REXX language specification (jumping into a '''DO''' loop and executing a '''DO''' or '''END''' instruction '''will''' cause a REXX SYNTAX error). As I mentioned elsewhere, there are other uses of the '''signal''' instruction than "long jumps" and "conditioning handling".  By the way, my ''Regina oriented text'' (whatever that means --- it doesn't use any instructions or options not available in every REXX interpreter), it isn't even a Regina REXX "program"; it was executed with PC/REXX as indicated in the REXX section header comments. I'm sorry that you can't follow this discussion. Your example (below) just shows a SYNTAX error for an illegal use of the '''signal''' instruction (the reason for the failure has been pointed out), but it fails to show a valid use of the REXX '''signal''' statement, which is what this RC task is about. If you have an valid example of the '''signal''' statement doing a "jump anywhere", then, please, add another REXX version that shows that. -- [[User:Gerard Schildberger|Gerard Schildberger]] 05:48, 12 December 2012 (UTC)
::: That's what I did right now. Hopefully in an agreeable way. Thanks --[[User:Walterpachl|Walterpachl]] 09:45, 12 December 2012 (UTC)

:::: Exactly which REXX interpreter did you test that code on?  And also, what REXX was used for the example below? -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:06, 12 December 2012 (UTC)

::::: The only ones I have: ooRexx and REXX/TSO --[[User:Walterpachl|Walterpachl]] 20:34, 12 December 2012 (UTC)

-----

as exemplified by this example:

Example:

```rexx
 Do
   Signal label
   label: Say 'label reached'
 End
```

Output:

```txt

     4 *-* End
Error 10 running H:\sig.rex line 4:  Unexpected or unmatched END
Error 10.1:  END has no corresponding DO, LOOP, or SELECT 

```


:: why did you explain here what I wanted to demonstrate? and what do you mean by: "There are other uses for the '''signal''' instruction than that."??
--[[User:Walterpachl|Walterpachl]] 05:03, 12 December 2012 (UTC)

::: I explained because just showing a SYNTAX error for an illegal use of a '''signal''' instruction isn't explaining. Non-REXX programmers won't understand why it happened. I explained what happened and WHY it happened (using a SIGNAL instruction illegally: that is, "jumping" '''into''' a DO loop and executing the END statement).  Showing an invalid example of the SIGNAL instruction and then getting a REXX SYNTAX error without informing of WHY it happened isn't doing non-REXX programmers any service.  As for other uses of SIGNAL, I was referring to other uses other than ''very global jumps'' and ''condition handling''.  Those other uses aren't within the scope of this particular RC task, so they were obviously not included herein. I have demonstrated those uses in REXX examples elsewhere in Rosetta Code. -- [[User:Gerard Schildberger|Gerard Schildberger]] 05:48, 12 December 2012 (UTC)

: Please note that that your (illegal) code is "executing" the '''END''' instruction which has been rendered invalid by the execution of the '''signal''' instruction, which resulted in the '''syntax''' error (as cautioned in the comments in the REXX section header). -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:02, 9 December 2012 (UTC)
:: This explain what I tried to show.

::: What you showed was an illegal REXX construct which resulted in a '''SYNTAX''' error.  The code snipette isn't a valid REXX program (as mentioned earlier).  I was showing a use for the SIGNAL instruction, not how to ''not'' use the SIGNAL instruction. -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:00, 10 December 2012 (UTC)

: I use the '''signal''' instruction for ''other'' than  error condition handling or very global jumps for E-O-J and such.  There are other uses for the '''signal''' instruction than that. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:02, 9 December 2012 (UTC)
:: Thats what I said:
"Please consider Signal only for condition handling or very global jumps such as Signal end_of_job."

::: What I was objecting mildly to was your suggestion that the '''signal''' instruction should ''only'' be considered for condition handling or very global jumps such as signal end-of-job. It has other uses. This isn't the forum for what REXX statements should or shouldn't be used.  If it solves a problem, use it.  If you have a better solution, include an example of its use.  Rosetta Code is about showing solutions (or flavors of solutions) for the tasks specified.  There's always room for more examples and methods.  There are always more ways to skin a cat. -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:00, 10 December 2012 (UTC)

+++
title = "Talk:Break OO privacy"
description = ""
date = 2013-01-28T07:26:05Z
aliases = []
[extra]
id = 10245
[taxonomies]
categories = []
tags = []
+++

==Should this task remain?==
Is this a correct subject for RC? Will it reflect badly on RC? Will it attract the wrong kind of audience to RC? --[[User:Paddy3118|Paddy3118]] 03:42, 6 August 2011 (UTC)
:I think that if it is something that some languages can achieve then the task should remain. Not every idea is necessarily good practice in all languages, but if it is achievable, then it can still be demonstrated for comparative purposes. [[User:Markhobley|Markhobley]] 08:23, 7 August 2011 (UTC)

== Context? ==

What is the context for this task?

In other words, are we talking about using a debugger? or are we talking about implementing an inheritance hierarchy?  Or are we talking about code analysis?   Or.... ? --[[User:Rdm|Rdm]] 17:49, 6 August 2011 (UTC)

:I put an example of one possible interpretation of the task requirements out there.  Is that what this task is about? --[[User:Rdm|Rdm]] 00:41, 7 August 2011 (UTC)

When I saw the task title before reading the details, I assumed it was about accessing “private” fields of an object ''other than'' by the explicit code of the object (i.e. the Tcl example is appropriate, the C# and Java ones are not). —[[User:Kevin Reid|Kevin Reid]] 00:54, 9 August 2011 (UTC)
: I agree. The Java one is now restructured to only present the backdoor approach. I lack the expertise to do this with the C# code, but there must be something possible. I guess this means we're getting closer to being able to write the task description more exactly… –[[User:Dkf|Donal Fellows]] 10:32, 9 August 2011 (UTC)

:: I have marked the current task description as incorrect.  Both the prior Java implementation and the current C# implementation satisfied the task description.  So if they are wrong then the task description is also wrong. --[[User:Rdm|Rdm]] 12:43, 9 August 2011 (UTC)
::: I modified the task description to reflect what I think was the intent of the task. (<s>Mwn3d</s>Paddy, are my modifications congruent with what you had in mind?) --[[User:Short Circuit|Michael Mol]] 13:31, 9 August 2011 (UTC)

::::Sorry, I've been away. Back now! 
The intent was to show ways to circumvent such protection when, for example, you are given a compiled class and wish to force access to a protected member. The reason for the task is that Python - a language that has intentionally weak protection - relying instead on a "we're all consenting adults" approach, mentions that where other languages have a culture of using protected members, that there usually exists methods to get around this. I did want to get them in one place; but I also realise that it might undermine those languages that rely more on the obscurity of their protection hacking methods so I remain quite willing to junk the whole page if the community thinks it wise. --[[User:Paddy3118|Paddy3118]] 19:05, 9 August 2011 (UTC)
::::: I'd vote that the task stay.  Of course it should have warnings about the practice being frowned upon, running with scissors, playing with live ammunition, that sort of thing.  The task need not even have a specific context.  I would like to see some elaboration in the task description about possible uses of value.  As I could see some languages may have different reasons for this kind of thing, it would also be appropriate to ask each language for some description of why this might be needed.  --[[User:Dgamey|Dgamey]] 02:20, 12 August 2011 (UTC)

::::: i'd actually like to use this task to demonstrate that in languages that claim to have protection this protection can be circumvented. of course even nicer would be if we could get proofs for languages where it can't be circumvented. but i guess that may be hard.--[[User:EMBee|eMBee]] 07:02, 25 October 2011 (UTC)
:::::: Shouldn't be that hard on any system that has had a security patch in the last year...  If you can access a program's memory, the high level language semantics can be ignored.  --[[User:Rdm|Rdm]] 10:07, 25 October 2011 (UTC)
::::::: i mean it is hard to prove that a system is secure, and that private members of an object can not be accessed.
::::::: of course anything that happens outside of the process is beyond the control of the language, but the question is if it is possible to prevent access to private datastructures within the process. 
:::::::: If the OS allows random processes to access a program's memory then it almost certainly allows that program to access its own memory.  --[[User:Rdm|Rdm]] 14:02, 25 October 2011 (UTC)
::::::::: ok, good point. we need to limit the scope then. can we assume that at least unix based OSes do not allow a program to access another programs memory? 
:::::::::: No. For example, consider /proc/FOO/mem under bsd or linux (where FOO is 'self' or a process id).  That said, you do need the appropriate file access permissions.  --[[User:Rdm|Rdm]] 15:10, 25 October 2011 (UTC)
::::::::::: but that is what i mean: if you don't have permission the OS prevents access, and circumventing OS access is beyond the scope of rosettacode.--[[User:EMBee|eMBee]] 01:36, 26 October 2011 (UTC)
:::::::::::: I thought you were talking about "preventing access to private datastructures within the process".  The OS permissions are based on ownership, so anything with generic file system access can read /proc/self/mem.  (suid programs might be an exception -- suid root programs, for example, can drop their root priviledges -- and another possibility for achieving this kind of "os privacy" would be to split the application logic across multiple programs and assign each of them their own user id.) --[[User:Rdm|Rdm]] 03:23, 26 October 2011 (UTC)
::::::::: and is it fair to say that circumventing OS restrictions is beyond the scope of the issue we try to highlight? 
:::::::::: That would depend on "we" and "the issue", no?  If you define the OS as being outside the scope of your issue, then by definition, it's outside the scope -- but that also tends avoid most practical issues. --[[User:Rdm|Rdm]] 15:10, 25 October 2011 (UTC)
::::::::::: see above: i'd say it is beyond the scope of rosettacode because it is no longer about taking advantage of language features to get around builtin security protections, but it would be using OS facilities which i can do from any language to any other language. it's no longer a question of: "how do i get access to symbol X in library Y that i am using in my program."--[[User:EMBee|eMBee]] 01:36, 26 October 2011 (UTC)
::::::::: would that be enough to confine attempts to get around protection to cases within the process?--[[User:EMBee|eMBee]] 14:20, 25 October 2011 (UTC)
::::::: consider a system that allows you to load new code at runtime (as lisp, pike, python, javascript and many other languages do) is it possible to build the language/compiler/virtual machine in such a way as to make it impossible for newly injected code to access protected datastructures? the question is related to whether it is possible to build sandboxes (like for javascript) that are actually save. certainly sandbox developers claim it is possible, and if it is possible for them then it should be possible for a regular language runtime as well.
:::::::: It's possible for a sufficiently constrained sandbox.  That said, even java implementations have had security holes.  But this is getting outside the scope of rosettacode (and is totally outside the scope of this task -- this task does not even mention sandboxes).  --[[User:Rdm|Rdm]] 15:10, 25 October 2011 (UTC)
::::::::: of course, i only mentioned this in relation to the possibility of circumventing language based access restrictions. i agree this is outside of the scope of the task, but what i am trying to do is to identify what exactly is still within the scope. i'd say accessing memory through /proc/FOO/mem is not.--[[User:EMBee|eMBee]] 01:36, 26 October 2011 (UTC)
:::::::::: Why not?  We have a variety of os-specific tasks and implementations here.  (Most anything that deals with the command line, or with linking and often enough with i/o winds up being OS specific.)  Anyways, by "scope" I was just referring to the task definition -- I was not talking about anything more formal than that.  --[[User:Rdm|Rdm]] 03:23, 26 October 2011 (UTC)
::::::: to find out which languages offer this possibility, in particular among languages that allow to inject code at runtime, is a very interesting question.--[[User:EMBee|eMBee]] 12:44, 25 October 2011 (UTC)

This task was intended to exclude low-level "work out where the raw memory location is then peek/poke memory locations" type solutions. I really wanted something more "structured". See the examples that are already out there. If however the language compiler colluded with this approach by by making it easy to pick-out the memory location of the private member then ''maybe'' that might be worth mentioning, but that is in a grey area. --[[User:Paddy3118|Paddy3118]] 06:45, 26 October 2011 (UTC)

: I think that this distinction (abstraction) boils down to some sort of introspection utility for a compiled language.  This could be library that comes with the language, a third party library, or some kind of minimal library hand crafted for this task.  --[[User:Rdm|Rdm]] 12:51, 26 October 2011 (UTC)

==Common Lisp too long==
Any chance of doing a précis of the CL examples talky bits and maybe adding a small code example? --[[User:Paddy3118|Paddy3118]] 06:51, 26 October 2011 (UTC)
: Agreed. Let's see some code, not just windy theory. –[[User:Dkf|Donal Fellows]] 19:43, 26 October 2011 (UTC)

==Valid Uses?==
While generally agreed that this is bad form, the discussions above identified things like debuggers and diagnostic tools as possibly valid uses.  Also, it seems to me that these techniques could be used in combination with 'monkey patching' (see [[Add a variable to a class instance at runtime]]) and other similar kinds of activities.  Okay, this gets a bit out on the edge of valid but still. --[[User:Dgamey|Dgamey]] 16:51, 27 December 2011 (UTC)

== What is needed to get this out of draft? ==
There's been a healthy discussion on this and it seems to have settled down. So the next question is what needs to be done?
Just reviewing the discussions above, I think it comes down to some additions to the task description and possibly marking some of the tasks for clarification due to task changes?  For the description, I'm thinking that statements along the lines of the following would help:
* On context - a note that this is a proof of concept example that could be used in things like debuggers, diagnostic tools, code analyzers, extended inheritance frameworks
* On approach - clarify that it is within the context of the language and isn't intended to get into low level hacking from another language or via the O/S (and to see the discuss page on this as a grey area)
* Possibly some linking to other tasks that might work with these techniques in some of the example contexts
* Elaborate on the un-idiomatic usage warning about dangers etc.
Thoughts anyone? --[[User:Dgamey|Dgamey]] 18:05, 27 December 2011 (UTC)

: Seems reasonable to me. The degree to which low-level hacking is required will probably vary by language though, and there's probably a need to cross-link to some tasks on introspection. –[[User:Dkf|Donal Fellows]] 18:30, 27 December 2011 (UTC)

I agree with TimToady's updates and his move out of draft.
I especially liked this bit:
:'' if your language intentionally maintains a double-standard for OO privacy ...''
--[[User:Paddy3118|Paddy3118]] 07:26, 28 January 2013 (UTC)
:-)

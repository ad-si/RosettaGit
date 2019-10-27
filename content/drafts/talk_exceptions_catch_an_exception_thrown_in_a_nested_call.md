+++
title = "Talk:Exceptions/Catch an exception thrown in a nested call"
description = ""
date = 2019-06-25T15:47:28Z
aliases = []
[extra]
id = 4002
[taxonomies]
categories = []
tags = []
+++

== As opposed to what? ==

Isn't "through nested calls" the normal behavior for [[exceptions]]? What language's exceptions ''doesn't'' work through nested calls? Isn't this basically the same as [[Exceptions]]? --[[User:Spoon!|Spoon!]] 07:01, 7 March 2009 (UTC)
:The devils in the detail. For example, some languages might have to annotate each function with details of what exceptions go through them. This example will also point out how one particular exception is caught and not another, that must nevertheless be thrown; and finally the task asks that you describe what happens when a user exception is not caught, languages may give differing amounts of information on the uncaught exception. 

:I did read [[exceptions]], but thought that there was room for another task to bring out other, specific aspects, as well as try and have the examples implement the same thing rather than [[exceptions]] 'show me what you can do' approach. (Which is fine, but different).

:After my explanation, do you think the task should be deleted?  --[[User:Paddy3118|Paddy3118]] 12:50, 7 March 2009 (UTC)

==Ada example and un-caught exception==
The Ada example catches both exceptions at some level rather than leaving one un-caught and explaining the consequences. Could someone make the change? --[[User:Paddy3118|Paddy3118]] 11:54, 22 March 2009 (UTC)
: Fixed --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:43, 22 March 2009 (UTC)

==A question on Java==
If you changed <code>public static void foo() throws U1</code> to remove the reference to it throwing U1, would that always be a compile time error? --[[User:Paddy3118|Paddy3118]] 05:14, 1 May 2009 (UTC)
:Yes. You could change it to <code>throws Exception</code>, but then you would have to surround all calls to the function with a <code>try</code> block which would <code>catch(Exception e)</code>, which would then catch the U1 Exception and fail to meet the task requirements. I'm not sure if Java could do this task (or if it would even be good practice in any language). --[[User:Mwn3d|Mwn3d]] 05:47, 1 May 2009 (UTC)
:Also <code>throws Exception</code> and <code>catch(Exception e)</code> will both usually make me cringe a bit. Try to avoid them :). --[[User:Mwn3d|Mwn3d]] 06:20, 1 May 2009 (UTC)

==R code doesn't catch anything?==
Function foo should catch the first exception raised, but fail to catch the second exception. I don't know R, but I can't see where function foo catches one exception but not the other? --[[User:Paddy3118|Paddy3118]] 17:09, 23 November 2009 (UTC)

== incomplete task description ==

This task does not specify that the second instance of foo should be independently wrapped by a different catch instance than the first.  It probably should explicitly specify this.

Consider this alternative implementation which fits a literal reading of the task specification:

   try {
      foo(1);
      foo(2);
   } catch (U0) {
   }

It is clear from reading the existing implementations that this was not intended, but a casual reading of the specification suggests this implementation.

== Go mince words ==

I'm submitting a Go solution after originally marking this task omit.  I marked it omit some time back, following a strict reading of the task description.  A long-time Rosetta Coder recently encouraged me to consider relaxing my interpretation of the task, and then perhaps even updating the task to more accurately describe the goal.

I had excluded Go because the task specifies that foo call bar, bar call baz, and foo catch U0.  It also implies that after catching U0, foo must go on to call bar the second time.  This is impossible in Go.  Well, by a strict wording, anyway.  The issue is the "go on" part.  That's basically throw, catch, ''and resume execution in the same function.''  In Go, catching terminates the function.  It's like there is no resume, only throw and catch.  If the handler is is foo, foo cannot continue.  Is it ok to put the handler in bar?  That would work in Go.  The task didn't say it was ok.  It didn't feel right.  Anyway, now the throw would only be one call away, and not in a nested call.  Is it ok to call bar the second time from the handler?  That would work too, but that seems even farther from the intent of the task.

For the solution I'm submitting, the rationale is that the equivalent of try/catch exists in Go, but try and catch cannot exist as unnamed blocks within a function; rather, they must be named and must be surrounded with function syntax.  That is, they must be separate wrapper functions.  Thus foo does not call bar directly, but calls a wrapper function.  Also, foo does not strictly contain the code of the exception handler, but instead includes it through the call to the wrapper function.  While this does not meet a strict reading of the task, I think it does essentially follow the indicated pattern.

I think it follows the indicated pattern as long as "call" doesn't have to mean "directly call."  Opinions?  Is this amount of word mincing acceptable?  Should the task description or title be improved?  Or do I just worry to much about the details?  &mdash;[[User:Sonia|Sonia]] 06:44, 30 April 2011 (UTC)

:Hi Sonia, A summary of this should be on the task page near the head of your example. You have all the elements here to state what parts of the task description are being stretched; why they need stretching, and how you do it. I don't think that their is a problem with the task ''yet''. You are stretching 'call' to become 'indirectly call' from my interpretation of the above. The task is meant to mean directly call, and the majority - even you I think, have directly call as the normal meaning. I don't want to make the task say directly call as then it might shift the meanong of so much more where I depend on some unwritten shared baseline of meaning. --[[User:Paddy3118|Paddy3118]] 12:34, 30 April 2011 (UTC)

:P.S. Now I have new insight into this quote from Lewis Carol:  "Words mean exactly what I want them to mean, nothing more, nothing less". --[[User:Paddy3118|Paddy3118]] 12:34, 30 April 2011 (UTC)

:Saw your changes and went and read a bit of the Go faq too. --[[User:Paddy3118|Paddy3118]] 09:10, 2 May 2011 (UTC)


==11l swallows U1?==
I can't see what happens to the 11l examples U1 exception when raised? If it is not caught, does it just pass silently? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:12, 25 June 2019 (UTC)
: At the present moment the only way of execution of 11l code is to transpile it to C++. So, as in C++ the exact behavior for an uncaught exception in 11l is implementation-defined (for example, MSVC shows an error window [but prints nothing to the console], and GCC prints ‘terminate called after throwing an instance of 'U1'’). --[[User:Alextretyak|Alextretyak]] ([[User talk:Alextretyak|talk]]) 13:39, 25 June 2019 (UTC)
::Thanks, I linked to this explanation. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:47, 25 June 2019 (UTC)

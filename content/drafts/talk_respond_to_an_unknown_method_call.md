+++
title = "Talk:Respond to an unknown method call"
description = ""
date = 2012-02-15T21:38:00Z
aliases = []
[extra]
id = 4319
[taxonomies]
categories = []
tags = []
+++

Donal, nice task.  --[[User:Glennj|glennj]] 13:59, 4 June 2009 (UTC)

What to do with the languages where this contract violation is always detected at compile time per language design? The task requires to show a way to circumvent the contract that explicitly states that the object ''x'' does not support the method ''f''. In a strongly typed language this is impossible to do, which is basically the whole idea of design by contract. Should such languages be mentioned as having no solution? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:56, 8 July 2009 (UTC)
:That means they are statically defined without means for dynamic type resolution, so omit. --[[User:Paddy3118|Paddy3118]] 19:45, 8 July 2009 (UTC)
:Java was omitted for that reason. That seems like a good solution. --[[User:Mwn3d|Mwn3d]] 19:45, 8 July 2009 (UTC)

::Note that Java has the opposite operation — the ability to dispatch to an existing method that you don't know at compile time — through its reflection classes, and it's actually very useful for me as I tend to write dynamic code in any language. I need to check whether there's a task to allow people to write about their language's ability to introspect its objects/classes to discover what methods are available and dispatch to them, but that's not ''this'' task; this task is about those languages which allow objects to accept any message if they wish (some like this, some don't; it's a philosophical thing). —[[User:Dkf|Donal Fellows]] 09:01, 9 July 2009 (UTC)

I see that some languages are putting in how they do errors when you try to do an unknown method. This is wrong! The object must have a chance to respond to a method call (or message) which it doesn't know the name/topic of ahead of time. Throwing an error at compile time is the total antithesis of this. I'm going to go through and remove all the contributions by people who have obviously "not got it". –[[User:Dkf|Donal Fellows]] 15:05, 25 February 2010 (UTC)
: Don't remove, but mark them '''incorrect''', and include your reasoning. If you remove them, there's not much trace of why it was removed, and some editor who comes along and drops in a new example might make the same mistake. If someone fluent in the language then comes along and marks it with the Omit template, then that's that. --[[User:Short Circuit|Michael Mol]] 16:14, 25 February 2010 (UTC)
:: I've marked them both with the omit template on the basis that they clearly stated "we can't do this"; that's quite enough evidence for me to just apply the correction. Don't mind them being omitted. Do mind a grand kerfuffle while they decide that they've made an error and change to what they're supposed to do. And yes, I'm a bit irascible today; comes of dealing with too many telecons and not enough progress. –[[User:Dkf|Donal Fellows]] 16:32, 25 February 2010 (UTC)

== PicoLisp solution ==

Sorry, I don't get it. The examples *do* demonstrate how to respond to an unknown message. If 'try' does not succeed, you can take any other measure you like (as here sending some other (known) message to the object). You might also call some other function on that object if you like, but that's not the point. [[User:Abu|Abu]] (moved from main page)
: the current solution does not respond to the unknown method call, it merely does a different call if the unknown calls fail.
: a correct solution would let the call <code>(try 'message1> Obj)</code> be successful. and actually it shouldn't even need to use <code>(try)</code>, but it should be possible to directly call <code>(message1> Obj)</code> without error.--[[User:EMBee|eMBee]] 08:37, 7 November 2011 (UTC)
:: No, it is not the case that the unknown call "fails" in the examples. Instead, his is a controlled behavior, and exactly the purpose of 'try'.
:: There are three ways to send a message to an object in PicoLisp: (message1> Obj), (send 'message1> Obj) and (try 'message1> Obj). The standard way, (message1> Obj), is just a convenient abbreviation of (send 'message1> Obj). They all three do a dynamic search for a definition of 'message1>' in Obj and its (super)classes. If a method definition is found, it is executed in the context (i.e. 'This') of Obj. If no such method is found, the case must be handled explicitly (what else?). And that's what 'try' is for, because the direct call and the one with 'send' throw an error.
:: So 'try' is the "primitive" form of 'send', which allows you to roll your own special method invocation, like calling (try 'unknown> Obj), and handling it yourself. Then calling some other function or method is the only way for the object to "respond (sensibly/usefully)". Not that an object can never "respond" all by itself, there must always be some function call involved (explictly or implicitly). --[[User:Abu|Abu]] 00:55, 7 Nov 2011 (UTC)
::: BTW, note that under the premise that "the object responds to the method call" the Common Lisp solution is also wrong. The "object" by itself doesn't do anything. The CL solution defines a _default_handler_, which is effectively the same as taking a _default_action_ after calling 'try'. (Not to mention that the latter method is more flexible, the action can be also dynamic depending on the situation, while the default handler is static and immutable at that moment) --[[User:Abu|Abu]] 12:03, 7 November 2011 (UTC)
:::: but writing a default handler is the point of this task as i understand it. ''check if a method exists and do something else otherwise'' is a different task. for that most solutions could be reduced to:

```pike
if (obj->foo)
    obj->foo();
else
    write("sorry, obj doesn't do foo");
```

:::: the task wants to find out how to preempt an unknown method call. how to write code that can handle any method call ''before it is known'' which method a user wants to call.
:::: the common lisp solution is within the limits of CLOS. objects/classes there don't have methods, but methods are written to handle certain objects. the difference is that the default handle actually receives the original message and can handle it. this is not the case in your solution. in your solution you are simply testing if a method exists when you send the message and do something else if not.
:::: the handling of the unknown method should not happen on the sending side, but on the receiving side. in other words: <code>(send 'method1> Obj)</code> should not fail. using <code>(try)</code> looks more like a cautious way to send a method.
:::: you could probably implement something similar to what common lisp has: a function <code>(no-applicable-method)</code> that is called if <code>(try)</code> fails, together with a new version of <code>(send)</code>:

```lisp
(dm no-applicable-method> ...)
(de send* (message)
   (or
     (try message Obj)
     (send 'no-applicable-method> Obj message)))
```

::::and then on the using side:

```lisp
(send* 'message1> Obj)
```

::::would be enough to get <code>message1></code> to be handled by <code>(no-applicable-method>)</code>
::::the important part is that the name <code>message1></code> is received by <code>(no-applicable-method>)</code> so that it can react differently to different messages.
:::: the difference is that <code>(no-applicable-method>)</code> and <code>(send*)</code> can be part of your library, while a user of the library only needs to do <code>(send* 'message1> Obj)</code>. this would be even more flexible than the common lisp solution where at least a generic function needs to exist before <code>(no-applicable-method)</code> can be called. but the generic function doesn't do much more than what <code>(send*)</code> would do in this example.--[[User:EMBee|eMBee]] 13:15, 7 November 2011 (UTC)
::::: Hmm, OK, though I'm not convinced. The task description doesn't say it this way. So I'll redefine 'send'. --[[User:Abu|Abu]] 14:01, 7 November 2011 (UTC)
:::::: well, maybe i am missinterpreting the task, we'd have to ask the creator.
:::::: i looked at the picoLisp docs and found that in difference to eg common lisps CLOS picoLisp classes actually do have member functions. now i am wondering, in your solution you define <code>(no-applicable-method>)</code> as a regular function and not as a class method. couldn't you also do this?

```lisp
(class +B)

(dm no-applicable-method> (Msg)
   (pack "No method for " Msg " on " This) )
```

:::::: this would provide the ability to only have specific classes respond to unknown method calls, or have different classes give different responses.--[[User:EMBee|eMBee]] 17:16, 7 November 2011 (UTC)
::::::: Yes, this would work. I avoided a definte class here for generality, but this would make sense for a common superclass. --[[User:Abu|Abu]] 17:58, 7 November 2011 (UTC)
:::::::: well, personally i think that the general case is not really interesting. the ''usual'' (for my understanding of usual) case is to use this for proxy or dispatch classes. that is, a specialized class that represents arbitrary objects in a remote system. so while every other class is normal, this one accepts any message and forwards that message to a different object or remote system. describing the special case for picoLisp would be interesting in contrast to common lisp where such a specialized case is not possible with CLOS without creating a <code>(send)</code> function (i don't know if <code>(call)</code> can be redefined), or adding the checks inside the generic function or <code>(no-applicable-method)</code> because the generic function there is called for any object.--[[User:EMBee|eMBee]] 01:32, 8 November 2011 (UTC)
::::::::: From a practical point of view, 'try' is optimal. For example, the PicoLisp GUI framework uses it in several places. It tries to send messages to database objects without knowing about their nature, so that it is up to these objects if and how they display themselves.
::::::::: The point is that when you are about to send a message where you don't know whether the object or its classes implement a method for it or not, you can handle it at the sending side. This keeps the logic local in the sender.
::::::::: The general database classes don't know and don't care about the GUI. No need to define proxy or dispatch classes. Only those concrete classes which should actually respond to these messages need to implement a method for them.
::::::::: In PicoLisp, a method can be defined on-the-fly, for a single object or a class.  The code for that definition must not reside in the same source file as the original class, but can be local to the application. --[[User:Abu|Abu]] 08:37, 8 November 2011 (UTC)

== Go solution ==

I'm hoping this satisfies the intent of the task.  It does not catch the unknown method at compile time, it does not throw an exception, it does not force the caller to check for the existance of the method nor force the caller to handle the absence of the exception.  The caller calls a method on an object, and then it is that method, a method of the receiving object, that handles the absense of the requested method.  Defining a CallMethod method seems much like what is done in some other languages when a handler is defined to handle unknown method calls.

The remaining variance which might invalidate the solution is that the method isn't called directly.  Go has no syntax for,
```txt
object.<expression that evaluates to a method>()
```
 Thus the neccessity of the CallMethod method.  &mdash;[[User:Sonia|Sonia]] 21:37, 15 February 2012 (UTC)

+++
title = "Talk:Aspect Oriented Programming"
description = ""
date = 2019-03-22T08:20:45Z
aliases = []
[extra]
id = 9900
[taxonomies]
categories = []
tags = []
+++

== What is the task here? ==
What is the task here? --[[User:Rdm|Rdm]] 19:10, 9 June 2011 (UTC)
:While it does have some code examples, I think this is supposed to be more of an informational page. It's not marked as a task, but it was made by an anonymous user who probably didn't know the process. --[[User:Mwn3d|Mwn3d]] 20:39, 9 June 2011 (UTC)
:: It's not very informational either. It doesn't say what AOP ''is'' or why you might want to do it, but at least it includes a long rambly example in C, a language which I'd have called inimical to AOP in the first place… I can't even tell if that's because they “get it” much ''more'' profoundly than me, or much ''less''. The evidence is consistent with both interpretations! –[[User:Dkf|Donal Fellows]] 23:57, 9 June 2011 (UTC)
:::I only made a guess at intent. I can't say anything about the quality. I don't understand AOP very well either. If someone wanted to improve the info here that'd be great. --[[User:Mwn3d|Mwn3d]] 01:05, 10 June 2011 (UTC)
::::In my experience, AOP is an attempt to address the issues a person might want to address using a Domain Specific Language, but without the conciseness, nor the modularity (with strong lip-service to the concept of modularity, but what I have seen in implementations does not align with my feelings about modularity).  Now, granted, the theory (MVC++) usually sounds great, but the examples I have seen (Observer++) have made me want a good DSL implementation. -- --[[User:Rdm|Rdm]] 11:31, 10 June 2011 (UTC)
::::: AOP allows you to wrap cross-cutting functionality round other code without disrupting that other code particularly. The way in which it is most strongly used in frameworks like Spring is to provide transactions round methods, so that all a method has to do is to work with the data and let the aspect handle the transaction (including things like rolling the changes back if an exception is thrown). Having tried to write transaction-handling code without AOP, using an aspect to handle it is ''far'' simpler; it's genuinely difficult to get transaction handling correct and it makes the code very ugly when you do, but splitting it off into its own concern (the “aspect”) means that the business logic is separated from the complexity. It's also used for things like logging, cacheing, security enforcement, etc. All things that are typically orthogonal to the core of what's going on. The degree to which aspects should be applied transparently or with some kind of explicit marker is something which seems to divide practitioners; I'm more of an “explicit” guy as its easier to see what's going on when maintaining the code… –[[User:Dkf|Donal Fellows]] 15:18, 10 June 2011 (UTC)

:::::: Ok, I suppose that works -- Spring, in essence, is a DSL.  No reason they can't be combined.  --[[User:Rdm|Rdm]] 16:02, 10 June 2011 (UTC)

:Maybe the task is to identify features of aspect oriented programming that the language supports. We probably don't need any specific application in mind. There are a variety of methods mentioned in the task description, so implementers could probably use that as a guideline --[[User:Markhobley|Markhobley]] 16:13, 10 June 2011 (UTC)
::a generic description of features is surely useful, but for language comparison a more specific problem would be nice. it would enable implementers to demonstrate alternative ways to solve the problem if the language does not support aspect oriented programming directly.
::a possible task could be: you have a library whose source you can not change, and you want to print a trace of the function execution that happens in the library. the task is to print each function call and its arguments before they are happening. (a backtrace that may be provided by the language is not acceptable)
::this task is i believe suitable for aspect oriented programming as it requires to intercept the function calls without changing the code in the functions. however implementers are free to take advantage of any language feature to solve the problem, thus allowing the reader to not only learn about aspect oriented programming features but also find out how the problem can be solved if the language does not support aspects directly.--[[User:EMBee|eMBee]] 02:27, 13 October 2011 (UTC)
::: Indeed. You can't implement a feature (well, you can but it's usually a lot more work than you want to show on a webpage) but you can deal with a specific task. More to the point, you can ''compare'' task solutions across languages much more easily as well, and it is the comparisons that this site really lives for. (If anyone goes to the effort of making this an actual task, please mark this as {{tmpl|draft task}} or {{tmpl|task}} please so that it pops up on my list of things to implement!) –[[User:Dkf|Donal Fellows]] 11:10, 13 October 2011 (UTC)

==Comment on the task as it stands==
This reads like an encyclopedia article only masquerading as a task. If someone had taken an example of Aspects at work in a language that supported it such as [http://www.verilab.com/files/sample_chapter_verilab_aop_cookbook.pdf Specman e] then they could have honed in on the features and asked for the like in other languages. as a description of aspect orientism in programming, again it lacks a concrete example and comparisons. Just my thoughts. --[[User:Paddy3118|Paddy3118]] 04:26, 13 October 2011 (UTC)
: You are completely right. How about a concrete task: ``Two classes are given, Subject and Observer which contain methods for adding observers to a subject (addObserver, removeObserver), and for propagating a state change in a subject among observers (stateChange on a Subject triggers update on all Observers). Furthermore, there are two classes Temperature and Sensor which do not implement/inherit these roles at all.a a A Temperature just has getTemp and setTemp methods, and a Sensor just has a setTempSource (observe a particular Temperature object), and a sampleTemp method (take the temperature of the Temperature object using getTemp). Using Aspect Oriented Programming (AOP), we can force the Temperature and Sensor classes to implement the Subject and Observer roles, without changing the definitions of these data types or adding code to them. These extensions of behavior are not visible outside of the the aspect module. Outside of the aspect, it appears that sampleTemp is "magically" called on a Sensor whenever the Temperature which it is observing is updated via its setTemp, and neither object has any extra state, and neither class inherit from Subject or Observer. Furthermore, the Subject-Observer aspect is not specific to Sensor and Temperature, but is split into two parts: an reusable abstract Subject-Observer aspect, and the concrete one which specializes to Sensor and Temperature. Use the best approximation of Aspect Oriented Programming (AOP) in your chosen language to reproduce this concept.''[[Special:Contributions/192.139.122.42|192.139.122.42]] 23:56, 14 October 2011 (UTC)

== Apologies, Motivation, Suggestions ==

Sorry for not following the process.  I may have mis-labeled the article.  I wanted to discuss ways to build up code out of optional pieces which toggle various features/forks of the core software, and I chose to call them aspects.  In C this is often done with #define.  In Java it can be done by extending classes and overwriting their methods, to make a new fork, or in a more generalised way using AspectJ.  Certainly I do feel keeping the feature code separate from the core code is a desirable solution!

It can also be done in many languages using patches from the history.  Please feel free to rewrite and reuse any parts of the article or destroy it as you see fit.  Perhaps it should be renamed "Build Configuration" or "Optional Code" or "Feature Tree".


###  A Possible Task 


Task: Write an example of a factorial function with a logging feature which may be toggled on or off, preferably at compile time, and if possible without altering the original code.

Output of factorial(4) when LOGGING is enabled:

<lang>4 x 3!
3 x 2!
2 x 1!
```


A solution for Javascript which allows us to enable logging at runtime, without polluting the original code:


```javascript

###factorial.js###
function factorial(n) {
  return ( n==1 ? 1 : n*factorial(n-1) );
}

###LOGGING.js###
// Overwrite existing global factorial function with a logging version
var oldFactorial = this.factorial;
this.factorial = function(n){
  console.log(n+" x "+(n-1)+"!");
  return oldFactorial.call(arguments);
};

###LOGGING_generalised.js###
// This can be generalised into a decorator that can add logging to any given function.
function addLoggingTo(parent,name) {
  var oldFn = parent[name];
  parent[name] = function() {
    console.log(name+"() is being called with "+arguments);
    return oldFn.apply(this,arguments);
  };
}
addLoggingTo(this,"factorial");

```


This might be considered preferable to a bunch of if-statements mixed into the code.  The aspect is separate from the original source.  However, since it works by function-rewriting, it may not always be possible, for example if we want to attach to a function that we cannot reference.

Some general fallbacks exist for a wide range of languages which have no higher-level support.  A simple if (LOGGING) check should always work.  Some applications mutate existing code at runtime using a plugin or module framework, in which case the LOGGING feature could be a plugin that may or may not be present to consume log events.

In Java, libraries such as Log4j and ApacheLogging are used, but these pollute the code with conditions.  So the AspectJ solution is preferable, although it is not pure Java.  (Actually the output is pure bytecode, but the build process is not.)

In C, I think we are stuck polluting the code using #ifdefs or the LOG(...) example I gave in the article.  I suppose we could avoid polluting the code by having the preprocessor switch certain functions for logging versions at compile time, but as I see it that would require parallel preprocessor code for every function we might want to log, so would be even more painful to maintain!

-- OP,--[[Special:Contributions/82.32.24.201|82.32.24.201]] 05:11, 19 October 2011 (UTC)

:I see several possible paths here.

:One option would be to "implement a task and then implement a variant without changing the original code".  This degenerates to including words in the original code which are later redefined to do something else.

:Another option would be to "take an existing implementation of another task and then implement a variant without changing the original code".  This degenerates to introspection with code injection and/or source analysis with code emission.

:Any of the above could be hidden behind the abstraction barriers offered by object implementations.

:That said, it's often good coding practice to modify the original when introducing pervasive changes.  This can make later maintenance much simpler. But we can have tasks here that ask for examples of bad coding practices.  But we do not have to pick the variant that uses a task implementation not designed for AOP.

:Another variant, then, would be to take an existing task, instrument it with inflection points, and then add the optional AOP code.  --[[User:Rdm|Rdm]] 10:56, 21 October 2011 (UTC)

:: Interesting!  I have been working on the assumption that we ''do not'' want to alter the original!  It would be helpful to work out under which situations this is good or bad practice...

::: I think in the case of logging it is obvious that separating this concern is useful.  Other positive examples were mentioned above: security (validating/sanitising input/output across one level of the application), caching, and tracking transactions.

::: When it is not useful: If it impacts the ability to debug an application, because you are looking at the code but you can't see that in fact some code defined elsewhere is being triggered.  (We could argue that this problem already exists when reading Java, if you think you have problems with class X, but actually at runtime you were given buggy class Y child of X!  Runtime debugging helps to unwrap these problems.)

::: In different frameworks the relationships with external code can be more or less obvious.  For example when using plugins, a number of hooks may exist in the original code, to call any plugins which are loaded; in this case the path of execution is clear.  (However the hooks might make this a less-pure example of AOP-or-whatever-we-are-calling-this.)  Event driven frameworks are an alternative view, where the code triggering the event has no idea which handlers have been registered; the developer is unsure what will be executed.

::: Other than clarity when reading the code, what other issues can make this bad practice?

:: My main objectives here are modularity.  We want to break stuff up as much as possible, so we can separate concerns and deal with each part independently, and easily remove or replace parts.  Clarity is desirable when possible, but may be a secondary concern.

:: Whilst I am not seeking bad examples, I think this paradigm is useful enough that having a working method for all languages is desirable, even when sub-optimal.  You may wish your application to have features that can be enabled or disabled at compile time, even if it does mean adding a lot of messy #ifdefs throughout the code!

:: Thank you all for your feedback. --[[Special:Contributions/82.32.31.166|82.32.31.166]] 23:20, 20 November 2011 (UTC)

::: Note that in C, for example, it's probably best practice to use #ifdefs when setting up #defines in your headers, as opposed to putting them in .c files.  --[[User:Rdm|Rdm]] 14:57, 21 November 2011 (UTC)

== Updated  task? ==

What should be logged, in the updated task? --[[User:Rdm|Rdm]] 10:08, 24 October 2011 (UTC)

: It would be good to refine the task.  Logging may be too trivial an example to make the paradigm stand out.  I like the idea of (optionally) adding a mutex around certain parts of code.  Is this a rich enough example to demonstrate all the requirements we have?  Other possible examples were mentioned earlier.

: Before considering the task, I will try to present all the requirements I would like to satisfy (use-cases for this paradigm):

:: Adding features to a program which can be enabled or disabled at runtime.  (Ideally not polluting the original code, but sometimes unavoidable.)

:: Keeping code related to different features in a separate file/module (e.g. inheritance), or at the very least relate all the feature code using some unique identifier (e.g. #ifdefs).

:: Reducing the size of the code.  Often by generalizing a common concept we can encapsulate repeated patterns in the code and reduce the overall footprint.  For example, AspectJ can manipulate methods across many classes, with a few lines of code.

: Do you think we need to separate these goals, given that the first two can conflict?  Perhaps clarity/modularity should be one goal, and enabling/disabling features another. --[[Special:Contributions/82.32.31.166|82.32.31.166]] 00:03, 21 November 2011 (UTC)

:: In some languages, mutex is implicit. --[[User:Rdm|Rdm]] 19:35, 5 December 2011 (UTC)

::: And some languages are single-threaded (at least in terms of basic model). –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 15:23, 27 April 2014 (UTC)

:::: Exactly. Also, computers are getting to be cheap enough that often you are running clusters of machines (requiring many instances of the language rather than threads within the language). And once you get into that territory, language integrity checks start failing in the sense that communication failures, hardware failures, version drift and data and storage integrity across time become dominant issues. Plus, of course, having an adequate supply of yummy food. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:20, 27 April 2014 (UTC)

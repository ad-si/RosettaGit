+++
title = "Aspect Oriented Programming"
description = ""
date = 2019-07-30T11:10:55Z
aliases = []
[extra]
id = 9898
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "common_lisp",
  "go",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "perl",
  "perl_6",
  "phix",
  "python",
  "scala",
  "tcl",
]
+++

## Task
The main goal of Aspect Oriented Programming (AOP) is to keep all the code relating to a single feature in one module of code.

There are a variety of ways to achieve this with object-oriented programming and functional programming, including inheritance, decorators and configuration trees.

In Aspect Oriented Programming existing code is extended with new behaviors invisibly, by means of stealthy instrumentation. There are some similarities between this concept and some debugging tools, such as Linux Kernel Probes. In fact, one obvious application for AOP is to add detailed tracing, with access to data, without touching the instrumented code. An aspect could be used to capture the calls to an entire API and log the arguments and return values.

The idea is that when the instrumented function is executes, either prior the execution of function, or at its point of return, or both, a block of code in instrumenting module is able to gain control. It can obtain access to the function arguments almost as if it were its own arguments, execute its own logic, and even decide whether or not the instrumented function will run at all, and with what argument values, and what it shall return.

Furthermore, there can be additional refinements, such as instrumenting calls to some function A, but only when it is directly called by function B. Or only when function B is somewhere in the activation chain, etc.

Also part of AOP is the idea of instrumenting data structures such as classes. An "aspect" is a hidden extension to some existing set of classes, which adds hidden new instance variables to the class. These variables are not accessible by the class itself, only by the instrumenting Aspect code.  An object-oriented Aspect resembles a class, except that its instance variables are injected into another class (or classes!), and its methods are instrumenting hooks that capture control from the methods of other classes.

An Aspect Oriented Programming System provides the abstractions to make this happen: a way to write aspects which specify what classes and methods they instrument, and the code, data and activation rules for doing it.

AOP introduces some new terms. A "join point" is a place in the control flow where code is instrumented, like a function call or return. A "point cut" is a selection of join points according to some query, which are tied to a common instrumenting code. (A single instrumenting function can handle a large point cut of different join points.)

AOP allows for a "separation of concerns" because new features are introduced as aspects tied to the execution of existing code, but without touching any of it, thereby keeping code bases completely separate. For instance, a mutex locking discipline can be added to a class without touching any of its code. An aspect identifies the methods that need locking, introduces the mutex variable into every instance of every instrumented class, and gains control around the invocations of the targetted methods to manage the mutex. Without AOP, the concern for locking is spread everywhere. Mutex lock/unlock calls have to be written or at the very least, methods have to be declared as synchronized, and this is repeated in every class that requires locking.

;Task
The task is to describe or show how, or to what extent, a given programming language implements, or is able to implement or simulate, Aspect Oriented Programming.


## C


It can be difficult to modularise features in C, if the feature affects different parts of the program.


### Using a define var


When a new feature introduces code scattered throughout the program, we can relate all the code together using a define and ifdefs.


```c

#define MY_NEW_FEATURE_ENABLED

...

#ifdef MY_NEW_FEATURE_ENABLED
  my_new_feature();
#endif

...

#ifdef MY_NEW_FEATURE_ENABLED
  close_my_new_feature();
#endif

```


As well as allowing us to enable or disable the feature at compile time, this also provides a way to find all the relevant code easily, by searching for the variable name.

The same approach using a boolean instead of a define variable can be found in many languages, to allow a feature to be toggled on or off at runtime.


### Using a macro


An alternative macro method can be used in C, which is shorter for one-liners.


```c

/* Enable logging: */
/* #define LOG(x) printf("%s\n",x); */
/* Disable logging: */
#define LOG(x) 

...

LOG(blah)

...

```



### Using function pointers


In C a class-like object can be created using a table, or more commonly, a structure containing function pointers. This is often done in kernel programming for device drivers.

Here is a typical layout:


```c
struct object {
  struct object_operations *ops;
  int member;
};

struct object_operations {
  void (*frob_member)(struct object *obj, int how);
};
```


In this example, an object is constructed as an instance of <code>struct object</code> and the <code>ops</code> field is filled in with a pointer to an operations table of type <code>struct object_operations</code>.  The object is usually dynamically allocated, but the operations table is often statically allocated, and the function pointers are statically initialized.

A call to the <code>frob_member</code> method, if coded by hand without the help of any macros or wrapper functions, would look like this:


```c
pobj->frob_member(pobj, 42);
```


This representation opens the door to various possibilities. To gain control over all of the calls to an object, all we have to do is replace its <code>ops</code> pointer with a pointer to another operations structure of the same type, but with different function pointers. 

With some further refinements and hacks, we can create a way for our "aspect" can keep additional context for itself, but associated with the original object, including a pointer to the original operations which the instrumenting operations can call.

Of course, this is a far cry from being able to instrument multiple functions from different classes across an entire hirarchy, in a single place.


## Common Lisp


A library was developed by Pascal Costanza called [[http://common-lisp.net/project/closer/aspectl.html AspectL]].

However, the claims that "[i]t turned out that the pointcut functionality of AspectL does not make a lot of sense in Common Lisp, and the support for dynamically scoped generic functions has been replaced with much better mechanisms in [[http://common-lisp.net/project/closer/contextl.html ContextL]]."

=={{header|F_Sharp|F#}}==

[http://eprints.bbk.ac.uk/20835/1/csci2017.pdf An Aspect-Oriented Framework for F#]


## Go


Go does not have any specific support for AOP in either the language itself or its standard library and there appears to be little interest in adding any.

Nevertheless, there are at least three third party libraries for adding AOP functionality to Go, including [https://github.com/gogap/aop this one] which includes a code example. However, none of them have progressed beyond 'alpha' status and, as there have been no recent commits, development may have stalled.


## J


In as much as I am unable to see the differences between functional programming and aspect oriented programming (they are just that stealthy, from my point of view), I'll have to say that J is as aspect oriented as the capabilities of the programmer.


## Java


Java has an aspect oriented programming library called AspectJ.  Aspects can create entry and exit intercepts on normal methods.  In aspect language, features are called cross-cutting concerns.


## JavaScript


Bemson's [https://github.com/bemson/Flow/wiki/ Flow library] introduces an aspect-like framework for JavaScript.



## Julia

Several of Julia's graphics frameworks use the idea of a backend for graphics, where the graphics module wraps and extends a lower level graphics framework. The use of one module to wrap another captures most of the ways in which aspect programming seems to be used. As an example, here we add logging to a module. Users of the Adder module can simply import the LogAspectAdder module instead of the Adder module to add logging to the functions of the Adder class.

```julia
module Adder
    exports add2

    function add2(x)
        return x + 2
    end
end

module LogAspectAdder
    exports add2
    using Adder
    const logio = [stdout]
    
    function log(s, io=logio[1])
        println(io, now(), " ", s)
    end

    function add2(x)
        log("added 2 to $x")
        Adder.add2(x)
    end
end

```



## Kotlin

The most popular framework for aspect oriented programming in Kotlin JVM is probably [https://en.wikipedia.org/wiki/Spring_Framework#Aspect-oriented_programming_framework Spring AOP] which is simpler and seems to have less issues than AspectJ, described in the Java entry.

However, one issue Spring AOP does have is that it expects all classes to be open to inheritance whereas in Kotlin the default is for classes to be closed to inheritance though this can be changed by use of the 'open' modifier. To deal with this, JetBrains provide an 'all open' compiler plug-in which makes all classes annotated with specific annotations (such as @Configuration or @Service) open without the need for the 'open' modifier.


## M2000 Interpreter

M2000 written to be an interpreter which execute source code without using something, such a second interpretation or compilation at execution stage. We get what we have program, without changes. At execution time source consumed, and we can watch that using Test statement, which opens a Control form, where we can step through execution, we can execute code, change values in variables, also we can edit code - not for code already in consuming stage. Test statement works as break-point too, and we can set a name to stop execution when a test statement use it, otherwise we see code executed in low speed, as control form stay opened. Because this interpreter uses Events for forms, there is a way to hook an event object between a form and event service functions, to intercept the events and parameters. This hook object is a same object as the "automatic" one, but need code to send messages to final event service functions. Critical events, which get feedback from variables by reference, can't be hooked.

Except of the watch, seek and change job, we can use Profiler to start high resolution timer and we can get values using Timecount. Also there is a simple way to have log files in temporary folder, using Text statement. A list statement can be used to show a list of variables to screen or exported to file. Because of dynamic addition of modules/functions, there is a module/function list which can watch using Modules ?. Also we can watch threads using Threads as for numbers of threads, and in Control form (we can step through execution including threads)

There is no way to debug machine code (we can write and execute machine code in memory buffers for Code, which code can't be altered, we use for data only another buffer(s))
  

## Perl

The CPAN module <code>Aspect</code> brings features of AOP to Perl. From the documention:

The Perl Aspect module tries to closely follow the terminology of the basic Java AspectJ project wherever possible and reasonable.

However due to the dynamic nature of the Perl language, several AspectJ features are useless for us: exception softening, mixin support, out-of-class method declarations, annotations, and others.

Currently the Perl Aspect module is focused exclusively on subroutine matching and wrapping.

It allows you to select collections of subroutines and conditions using a flexible pointcut language, and modify their behavior in any way you want.

In addition, where the Java implementation of Aspect-Oriented Programming is limited to concepts expressable at compile time, the more fluid nature of Perl means that the Aspect module can weave in aspect code at run-time. Pointcuts in Perl can also take advantage of run-time information and Perl-specific features like closures to implement more sophisticated pointcuts than are possible in Java.

This allows the Perl implementation of Aspect-Oriented Programming to be stateful and adaptive in a way that Java cannot (although the added power can come with a significant speed cost if not used carefully).


## Perl 6


All methods in Perl 6 are really just routines under the control of the meta-object protocol for purposes of dispatch, so there are several ways to subvert the system in order to deal with aspects.  First, you could intercept the MOP calls, or you could install alternate metaclasses.  More likely, since methods are really just routines in disguise, and since a routine can be wrapped (in place) in an outer routine via the <tt>.wrap</tt> method, you'd just wrap all the methods and other routines that share a cross-cutting concern.

Note that the optimizer is free to assume routines are immutable after CHECK time (the end of compilation), so while you can always wrap routines during compile time, you might need a "<tt>use soft</tt>" declaration or some such to enable wrapping at run time, that is, in order to keep considering the routine to be mutable.  (Only code doing the Routine role is ever mutable in Perl 6—bare blocks and other lambdas are considered immutable from the start.)  Note that keeping routines mutable is likely to pessimize spots where the optimizer might otherwise be able to inline the code.

Of course, you can also simply redefine Perl 6 on the fly to be any language you want in the current lexical scope, so you are really only limited by your imagination in how you wish to express these "aspects", whatever you might mean by that term…


## Phix

{{trans|Go}}
Phix does not have any specific support for AOP in either the language itself or its standard library and there appears to be little interest in adding any.

(that was my idea of a joke, btw, and not a dig at Go but just a childish and silly use of "translation of")

Actually, there is one way to instrument (specific) calls without modifying the existing code. Suppose you have somelib.e, containing:

```Phix
global procedure saysomething()
    puts(1,"something\n")
end procedure
```

Then write wraplib.e as follows:

```Phix
include somelib.e as somelib
global procedure saysomething()
    puts(1,"wrapthing\n")
    somelib:saysomething()
    puts(1,"thingwrap\n")
end procedure
```

And replace all (existing) "include somelib.e" with "include wraplib.e". Hopefully there will be (and usually there is) only one.

What this won't do is instrument internal calls to internal routines, at least not without modifying somelib.e, nor will it work for
things not in separate files, nor many of the builtins - specifically it should be possible for most of builtins\, but not possible 
for any of builtins\VM\, though doubtless a few cross over.

Personally, while I might begrudgingly accept that sort of thing for the occasional quick 'n dirty, or a temporary debug aid, I am strongly opposed to it being 
permanent: code should do what it says it does. For easy toggling, my own code tends to be littered with things like:


```Phix
global constant NEWFEATURE = true
...
if NEWFEATURE then ...
```

I also actually favour explicit shims, almost exactly what the task is asking for a way to avoid doing, such as

```Phix
function my_length(sequence s) return length(s) end function
```

and manually edit every single call, again so that any future (/temporary) changes are quick, easy & obvious.


## Python

Python has special syntax for [http://legacy.python.org/dev/peps/pep-0318/ decorators] acting on functions and methods, as well as [http://stackoverflow.com/questions/100003/what-is-a-metaclass-in-python metaclasses].


## Scala

Jones Bonér introduces AOP for Scala in [http://jonasboner.com/aop-style-mixin-composition-stacks-in-scala/ this blog].

## Tcl

Tcl's <code>trace</code> command enables much AOP-like functionality, with variable traces allowing interception of accesses to variables, and command traces allowing interception of executions of procedures, etc. In addition, TclOO (Tcl's <!--main--> object system) also supports filter methods, which wrap around invocations of methods of an object (or all methods of a class) as a generalisation of before, after and around advice. This works well when combined with a mixin, allowing interception to be applied on a per-instance basis if desired.

For example:

```tcl
oo::class create InterceptAspect {
    filter <methodCalled>
    method <methodCalled> args {
        puts "[self] - called '[self target]' with '$args'"
        set result [next {*}$args]
        puts "[self] - result was '$result'"
        return $result
    }
}

oo::class create Example {
    method calculate {a b c} {
        return [expr {$a**3 + $b**2 + $c}]
    }
}

Example create xmpl
puts ">>[xmpl calculate 2 3 5]<<"
oo::objdefine xmpl mixin InterceptAspect
puts ">>[xmpl calculate 2 3 5]<<"
```

{{out}}
 >>22<<
 ::xmpl - called '::Example calculate' with '2 3 5'
 ::xmpl - result was '22'
 >>22<<

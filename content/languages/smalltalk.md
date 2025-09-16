+++
title = "Smalltalk"
description = ""
date = 2019-10-07T22:54:10Z
aliases = []
[extra]
id = 1860
[taxonomies]
categories = []
tags = []
+++
'''Smalltalk-80''' is an [object-oriented](https://rosettacode.org/wiki/object-oriented_programming), dynamically typed, [reflective programming](https://rosettacode.org/wiki/reflective_programming) language. It was designed and created in part for educational use, more so for Constructivist teaching, at Xerox PARC by Alan Kay, Dan Ingalls, Ted Kaehler, Adele Goldberg, and others during the 1970s, influenced by Sketchpad and Simula.

The language was generally released as Smalltalk-80 and has been widely used since. Smalltalk-like languages are in continuing active development, and has gathered a loyal community of users around it.

'''Smalltalk-80''' is a fully reflective system, implemented in itself. Smalltalk-80 provides both structural and computational reflection. Smalltalk is a structurally reflective system whose structure is defined by Smalltalk-80 objects. The classes and methods that define the system are themselves objects and fully part of the system that they help define. The Smalltalk compiler compiles textual source code into method objects, typically instances of <code>CompiledMethod</code>. These get added to classes by storing them in a class's method dictionary. The part of the class hierarchy that defines classes can add new classes to the system. The system is extended by running Smalltalk-80 code that creates or redefines classes and methods. In this way a Smalltalk-80 system is a "living" system, carrying around the ability to extend itself at run-time.

## Implementations
Over time, various implementations ("dialects") of Smalltalk have appeared, some of which target different audiences and/or focus on particular applications. Their internal implementation (evaluation mechanism) may also differ radically, from bytecode interpretation, just-in-time compilation, dynamic optimizing recompilation to cross-language translators (Smalltalk-to-C, Smalltalk-to-JavaScript, Smalltalk-to-Java).

## Spirit
Smalltalk consist of both the programming language and a more or less standardized library of classes. Most Smalltalks also include a sophisticated IDE, which supports editing, compiling, refactoring, debugging, tools, source code revisioning and packaging/deployment. With a few exceptions, working in a traditional editor (vi, emacs) or general purpose IDE (eclipse) is usually not recommended, as these cannot make use of all the reflective and dynamic coding features and will make the programmer less productive. For example, typical Smalltalk debuggers allow for code to be changed while the program is running (or halted at a breakpoint), allowing for a "programming in the debugger" style of coding. Code is usually developed and tested incrementally, with breakpoints on unimplemented parts, which are filled as encountered. This makes Smalltalk a perfect environment for rapid prototyping and experimental development.

Smalltalk environments are open - almost every aspect of the language and IDE can be changed, enhanced and customized. This includes object representation, metaclass and reflection facilities, language syntax, exception handling and the IDE itself. For this reason, Smalltalk has traditionally been a testbed for new language features, mechanisms and patterns.

Smalltalk is a very high level language, which almost completely hides any underlying machine representation. Implementation details such as integer size, pointer size, byte order etc. are invisible. The numeric tower is highly polymorphic, allowing for transparent use of arbitrary large integers, fractions, floats, complex numbers etc.

Smalltalk is a very late bound language. Object types, classes, references etc. are resolved at execution time, and can be dynamically changed.

## Language Syntax
Smalltalk has a very simple and highly orthogonal syntax, which is based exclusively on message sending (aka pure virtual function calling in C-parlance). The language syntax and semantic only consists of literal (compile-time) constants, variable bindings, message sends and block closures (lambda expression objects). Control flow, loop constructs, exception handling etc. are defined as messages to booleans, blocks or by passing blocks as arguments. This makes it trivial and common practice to add new control constructs.


###  Grammar

... to be added ... (for now follow the link at the right...)

## Language Semantic
###  Variables and Scope

Variables are used to name objects. Actually they are bindings of a name to an object. Objects can only be passed and used by reference, and access to an object's internals is not possible from outside the object's own method implementation, although most classes inherit from Object, which provides some reflection protocol to access instance variables or to query for the class. However, such inherited reflection mechanisms could be overwritten in a class or a class could not inherit from Object, to prevent even access from debuggers and inspectors.

Smalltalk is lexically scoped, and outer variable bindings are closed over when a block (=lambda closure) is created.

A rich scoping hierarchy exists, which consists of:
* inner block locals
* inner block arguments
* outer block locals
* outer block arguments
* method locals
* method arguments
* instance variable slots
* inherited instance variable slots
* class variables (static, shared with subclasses; visible in the class-hierarchy only)
* class instance variables (per-class-instance; private to the class)
* pool variables (for constant pools; must be explicitly "imported" to be seen)
* namespaces / globals (for classes to be known by name)


###  Objects

In Smalltalk, every object is an instance of some class. Objects are only be referred to by reference. Everything is an object, including integers, booleans, nil (the UndefinedObject), classes, stack frames (continuations), exception handlers and code (block closures/lambda closures and methods). There are no builtin primitive types, which are not part of the class hierarchy, or which cannot be changed/enhanced by the user.


###  Message Sending

Message sends are dynamically resolved, by letting the receiver of the message determine the method (aka code to run). The message consist of a selector (= name of the message) and optional arguments. The message syntax as:

```smalltalk
receiver selector>
```

a unary message, without arguments.


```smalltalk
receiver part1: arg1 part2: arg2 ... partN: argN>
```

a keyword message; the selector consists of the concatenation of the keyword parts: 'part1:part2:...partN:'.


```smalltalk
receiver op arg>
```

a so called ''binary message''. The selector 'op' consists of one or more special characters, such as '+', -', '@' etc.
These are actually syntactic sugar, especially to make arithmetic look more familiar (i.e. instead of "rcvr add: foo" we can write "rcvr + foo").

The precedence rules are unary > binary > keyword, thus

```smalltalk
a foo: b bar + c baz>
```
 is equivalent to <lang smalltalk
a foo:( (b bar) + (c baz) )
```


As messages have no semantic meaning to the compiler (especially, the '+', '*' and other binary messages), the usual precedence rules for arithmetic expressions are not present in Smalltalk. Thus, complex expressions consisting of multiple binary messages usually need to be parenthesized.

Message lookup is done by traversing the superclass chain, looking for a class providing an implementation (method) for the messages selector. The standard defines single inheritance, with a lookup based on the receiver's class only. However, some Smalltalk implementations allow for that lookup to be redefined and provide more sophisticated mechanisms (selector namespaces, lookup objects, lookup based on argument types etc.).

If no implementation is found (i.e. no class along the superclass chain provides a corresponding method), the original selector and arguments are packed into a container and a doesNotUnderstand: message is sent instead. This is used for error handling, but can also be used for message forwarding (proxies), delegation or dynamic creation of new code. The default implementation of doesNotUnderstand: raises an exception, which can be cought and handled by the program.


###  Metaclass Hierarchy

Classes themself are objects and as such instances of some Metaclass. As classes define and provide the protocol (=set of methods) for their instances, metaclasses define and provide the protocol for their instances, the corresponding class. Every class has its own metaclass and as such can implement new class-side messages. Typically, instance creation and utility code is found on the class side. Most Smalltalk dialects allow for the metaclass to specify and return the type of compiler or other tools to be used when code is to be installed. This allows for DSLs or other programming language syntax to be implemented seamlessly by defining a metaclass which returns a compiler for a non-Smalltalk. Typical examples for this are parser generators (tgen, ometa, petite parser), data representation specs (asn1, xml etc.) and languages (smallRuby, graphical languages in squeak etc.)

Being objects, classes and metaclasses can be created dynamically, by sending a #subclass:... message to another class, or my instantiating a new metaclass.


###  Exception Handling

Smalltalk protects itself completely from any invalid memory references, null-pointer, array bounds or unimplemented message situations, by raising a corresponding exception which can be cought by the program. An unhandled exception leads to the evaluation of a default handler, which usually opens a symbolic debugger, which is always part of the Smalltalk environment. Deployed end-user and production systems usually redefine this handler to show a warning, end the program or dump the state of the system.

In contrast to most other exception handling systems, Smalltalk exceptions are both restartable and proceedable. The exception handler is evaluated with the calling chain of continuations (stack frames) still being active and alive. The handler is free to repair the situation and proceed. Proceedability allows for queries and notifications to be implemented easily using exceptions; for example, a compiler's warnings or error correction queries can be implemented this way, without a need to pass in logger objects or other state. Also situations like missing files (OpenError) can be resolved in the handler, by opening a dialog, asking the user for a replacement and proceed.

The exception framework is implemented as part of the class library, and open for change and enhancement.

## Examples
As smalltalk code seems to be hard to read for programmers with a C background, some examples are provided below.

###  Literals


```smalltalk

"a comment - everything in between double quotes"

"/ an end-of-line comment (dialect specific)

true false "the two Boolean singleton objects"

nil "the UndefinedObject singleton object"

1234 "integer constant; an instance of Integer"

16rFF00 "integer constant, base 16"

2r101010 "integer constant, base 2"

1.234 "float constant; an instance of Float"

(1/7) "fraction constant; an instance of Fraction"

123s3 "fixed point constant with precision (somewhat dialect specific)

'hello' "string constant; an instance of String"

'öäü こんにちは世界' "string constnt unicode is supported by most implementations"

#'foo' "symbol constant; similar to symbols in lisp, two symbols are identical if they are equal"

#(1 true $a 16rFF 1.0 (10 20)) "array literal constant; the last element being another array; an instance of Array"

#[ 10 20 2r1000 16rFE ] "byte-array literal constant; an instance of ByteArray"

$a "character constant"

[ ... some code ... ] "a block literal; an instance of BlockClosure (dialect specific); the object represents the piece of code which can be passed around and evaluated later (a lambda closure)"

[:a1 ... :aN | ... some code ...] "a block literal with arguments."

```


### Special "builtin" Pseudo Veriables
```smalltalk

self "refers to the current receiver"

super "for super sends"

thisContext "refers to the current context (stack frame/continuation) as an object"

```



### Message Sends


```smalltalk

1000 factorial "send the 'factorial' message to the integer receiver"

a factorial even "send the 'factorial' message to whatever "a" refers to, then send 'even' to whatever that returned"

a + 1 "send a '+' message, passing the argument '1' to whatever a refers to"

(a + 1) squared "send '+' to a, then send 'squared' to whatever we get from it"

a , b "send the (binary) ',' message, which does collection-concatenation (arrays, strings, etc)

arr at:1 put:'foo' "send the 'at:put:' message to 'arr', passing two arguments, the integer '1' and a string"

a > b ifTrue: [ a print ] "send the 'ifTrue:' message to whatever 'a > b' returned (a boolean, usually), passing a block closure as argument. The implementation of boolean will either evaluate or not evaluate the passed block's code"

a > b ifTrue: [ a ] ifFalse: [b] "send 'ifTrue:ifFalse:' to the object returned by 'a > b', passing two block closures as arguments. The 'ifTrue:ifFalse:' method will evaluate one of them and return that block's return value as its own return value"

b := [ ... somCode... ].
...
b value "evaluate the block's code"

```



###  Other


```smalltalk

expr1 . expr2 "statements within a method or block are separated by a fullstop."

foo := bar "assignment; let foo refer to the object to which bar refers to (at that particular point in time)"

^ a + 1 "return; the value of the 'a+1' message send is returned as the value of the current method invocation."

'hello' print. 'world' print "statements are separated by a period; just like in english"

|a b c| "local variables; introduces 'a', 'b' and 'c' in the current scope (let-like local bindings)"

r msg1; msg2 "so called cascade; first send msg1 to r, ignoring the return value, then send msg2.
                       Value of expression is result from last message. Syntactic sugar for r msg1. r msg2 but an expression, not a statement"

```



###  Class Definition

Classes are not defined by syntactic constructs, but by sending a message to some class (to create a subclass), a metaclass (to create an instance) or to a namespace (to create a class and install it). The details vary slightly among dialects, but usually wrappers/forwarders are provided or easily added if code is to be ported and any is missing in the target system.

```smalltalk

someClass
    subClass: #'nameOfNewClass'
    instanceVariableNames: '...list of private slots...'
    classVariableNames: '...list of class variables...'
    poolDictionaries: '..list of imported pool bindings...'
    category: 'for documentation only'.

```

Classes can be anonymous, in most systems, you can "Class new new" to create a new anonymous class and an instance of it (but be aware, that the class should contain enough state to properly specify their instance's layout, so usually more info is needed (number and names of instance variables, inheritance etc.). Otherwise some tools (Inspector) may fail to reflect on it.


###  Control Structures

As mentioned above, these are defined as messages and their implementation is found in the corresponding receiver classes. The following is only a tiny subset - there are virtually hundreds of uses of blocks for control structures in the system.
Typical are:

```smalltalk

boolean ifTrue: [ block providing value if boolean is true ]

boolean ifFalse: [ block providing value if boolean is false ]

boolean ifTrue: [ value if boolean is true ] ifFalse: [ value if boolean is false ]

[ block for condition ] whileTrue: [ block to be looped over ]

n timesRepeat:  [ block to be looped over ]

start to: stop do: [:i | block to be looped over with index ]

collection do: [:el | block to be evaluated for each element ]

```



###  Exception Handling

Originally, Smalltalk used an instance based exception handling scheme, where instances of Signal where created and raised. Now, all implementations have moved to class based exceptions, where the raised exception is a subclass of Exception. As instance based exception handling is still useful in some situations (very lightweight, no need to create a class), some dialects continue to support both.

Smalltalk supports proceedable exceptions.


```smalltalk

[ try block to be evaluated ] on: exception do:[:ex | handler ]

```

the 'ex' argument to the hander provides detail information (where and why) and also allows control of how to proceed out of the handler (ex proceed, ex return, ex restart, ex reject).
The handler basically has the following options:
* ex return - return out of the try block
* ex restart - restart the try block
* ex reject - handler cannot handle; rethrow the exception for an outer handler
* ex proceedWith: value - proceed after where the exception was raised (after a repair)
Exceptions may be specified to be nonProceedable, to protect code from proceeding handlers, where proceeding is not possible.
Exceptions form a hierarchy, so a handler will also catch any derived exceptions. If an exception is unhandled, the original exception info is packed up and an UnhandledException is raised. Thus (similar to the handling of doesNotUnderstand:). The default handler for UnhandledException opens a debugger for the misbehaving thread (while usually other threads continue to operate as usual).

Handlers can also be defined to handle a collection of non-related exceptions, by creating an exceptionSet:

```smalltalk

[ try block to be evaluated ] on: ZeroDivide, DomainError do:[:ex | handler ]

```

finally, many dialects provide syntactic sugar for common situations:

```smalltalk

exception catch: [ action ]  - to return out of the action, without any particular handler action

exception ignoreIn: [ action ] - to ignore the exception and proceed (for example: a UserInterruptSignal, as generated by the CTRL-C key)

```



###  Unwinding

Ensure blocks to make sure that cleanup is performed correctly even in exception situations are defined as:

```smalltalk

[ action to be performed ] ensure: [ action to cleanup] - will cleanup in any case (i.e. both in normal and in unwind situations)

[ action to be performed ] ifCurtailed: [ action to cleanup] - will cleanup only in unwind situations

```



###  Multithreading

New threads are started by sending 'fork' to a block; this will create a process instance which executes the block's code in a separate thread (within the same address space):

```smalltalk

[ do something ] fork.

[ do something ] forkAt: priorityLevel

```

Notice that these are technically threads, not "unix processes". They execute in the same address (or object-) space.
They are named "Process" and created with "fork" in Smalltalk for historic reasons.

The scheduling behavior is not standard among dialects/implementations. Some only manual switch by explicit yield, most provide sctrict priority based scheduling, and some even provide preemtive timeslicing, and dynamic priorities. The details may also depend on the underlying operating system.

## Implementation
Most Smalltalk implementations are based on a bytecode execution engine. Bytecode is emitted and stored in method objects by a compiler which is part of both the development and runtime environment (in contrast to external tools, like javac). Thus new methods can be generated and installed dynamically by reading scripts or a generator. Bytecodes are operations for a virtual stack based machine, which is either interpreted by an interpreter (part of the runtime system), or dynamically compiled to machine code (JITTER). Bytecode is not standardized and usually not compatible among dialects.

Some implementations support source-to-source compilation to C, JavaScript or Java. These may or may not show some limitations in the support for dynamic changes at execution time. Typically, the full dynamic bytecode is used for development, followed by a compilation phase for deployment/packaging.

All Smalltalks use and depend on garbage collection for automatic reclamation of unused objects, and most implementations use modern algorithms such as generation scavenging, incremental mark&sweep and finalization support. Imprecise conservative collectors are typically not used. Reference counting was abandoned in the 70s.

As message send performance is critical in Smalltalk, highly tuned cache mechanisms have been invented and are used: inline caches, polymorph inline caches, dynamic recompilation based on receiver and/or argument types etc.
Also block (aka lambda) creation and evaluation, especially the treatment of closed over variables has been in the focus of implementors. Algorithms similar to those found in sophisticated Lisp systems such as lambda lifting, inlining, stack allocation and heap migration etc. are also used in Smalltalk.

## Influences
Smalltalk syntax is meant to be read like english sentences.

The syntax is very compact and almost every semantic feature is implemented via a messsage send to some receiver object, instead of being a syntactic language feature of the compiler. As such, changes, fixes and enhancements of such features can be made easily. It has and is therefore often used as a testbed for research,

Smalltalk's symbols correspond to Lisp symbols, blocks are syntactic sugar for lambda closures.

ObjectiveC's message send, syntax and keyword format is a direct subset of the corresponding Smalltalk message send syntax. Also the semantic of its classes and instances are similar. However, the reflection and metaclass facilities are a small subset.

Java's container and stream class hierarchy has similarities to Smalltalk collection classes. MVC as used in many toolkits has been in Smalltalk for a long time.

Self a descendent of Smalltalk, uses a similar syntax, blocks and exception facilities, but adds instance based inheritance, dynamic slots and mirrors.

Slate and Newspeak use similar syntax and message send semantics.

Newspeak generalizes the scoping to include nested classes, namespace instantiation and abstracts variable access.

JavaScript's functions are a syntactic different but semantically equal to Smalltalk's blocks, including scoping rules (but lack the capability of returning from their containing function).

## Implementations
* Amber Smalltalk
* Dolphin Smalltalk; open source
* GemStone/S; object-oriented Smalltalk database, free for private and commercial use
* GNU Smalltalk; open source
* Pharo; open source, mostly compatible to Squeak
* S#
* Smalltalk/X; free for private and commercial use
* Smalltalk-MT; (still actively developed?)
* Squeak; open source
* VisualAge Smalltalk (Instantiations; formerly known as IBM Smalltalk); free for private use
* VisualWorks (Cincom) Smalltalk; free for private use

## Citations
* [Wikipedia:Smalltalk](https://en.wikipedia.org/wiki/SmallTalk)

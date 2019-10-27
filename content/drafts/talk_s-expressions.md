+++
title = "Talk:S-Expressions"
description = ""
date = 2017-04-27T22:13:47Z
aliases = []
[extra]
id = 10680
[taxonomies]
categories = []
tags = []
+++

== The Goal of this task==
is reliable parsing of input data that doesn't break no matter what is thrown at it (as long as it follows the rules) into any native data structrue in your language and back.--[[User:EMBee|eMBee]] 00:14, 18 October 2011 (UTC)

the purpose of this task is to read and write structured data, in order to be able to use it to save or exchange data between systems. the vehicle chosen for this data is s-expressions. just like you would use json, xml or other formats.

it really does not matter how the data is translated in a language, as long as it is in a format that is of practical use in the language. in other words, it should be possible to access the values and manipulate them. for this reason, i am not sure how useful it is to represent symbols as such, other than to help differentiate between quoted and unquoted strings as [[User:Ledrug|Ledrug]] pointed out very early on in the discussion. if the language has no support for symbols then care needs to be taken that the value of the symbol is actually accessible.

the OCaml solution is for example on the border. it doesn't handle numbers and it does not even encode the difference between quoted and unquoted strings.--[[User:EMBee|eMBee]] 01:59, 18 October 2011 (UTC)

== Existing Standards ==

People looking for "s-expression" are not finding a specification because this is not a language, but a name for a category of printed syntax from the Lisp culture. This is like looking for a standard which formalizes "reverse polish notation" or "infix". Well, C has infix, Fortran has infix, Java has infix, ...

These standard languages have read syntax which is based on S-expressions:

Common Lisp: ANSI Standard available in the form of the [[http://www.lispworks.com/documentation/HyperSpec/Front/index.htm Common Lisp HyperSpec]].  The reader syntax is described in [[http://www.lispworks.com/documentation/HyperSpec/Body/02_.htm Syntax]].
:this page describing the [http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm Character Syntax Types] seems most interesting.--[[User:EMBee|eMBee]] 02:07, 20 October 2011 (UTC)
::Yes. In Common Lisp, the reading is very simple. Characters are assigned to categories in a read-table (in a context-independent way). The reader takes a character and based on what category it is in, it assembles it into a token, or dispatches a function associated with that character. Donald Knuth's TeX has a similar approach: it also has character categories (which TeX code can manipulate).[[Special:Contributions/24.85.131.247|24.85.131.247]] 02:22, 20 October 2011 (UTC)
:::indeed, i like the approach, it also demonstrates the simplicity of s-expressions (and lisp) syntax. you could not do this as easely with xml or json.--[[User:EMBee|eMBee]] 05:22, 20 October 2011 (UTC)
::Note that there is a category of "non-terminating" macro characters. This means that if they are part of a token, they behave like token constituents. The # macro dispatch character is this way, so for instance abc#def is actually a symbol with the name "ABC#DEF": the special meaning of # is defeated. If it were a terminating character, then its presence would signal the end of the token ABC.  The parentheses are terminating, needless to say: (abc) is not a single token, but the same as ( abc ). Lisp tries to allow the programmer to have a lot of freedom in what constitutes a symbol. Just because some symbol has a special meaning doesn't mean we have to inconvenience ourselves by making it difficult to have that character in the middle of a symbol. This is quite important if you're doing domain-specific things where the symbol syntax comes from some outside domain. Another example is the writing of compilers where you have to deal with symbols from another programming language, as well as low-level symbols used by assemblers and linkers. It's not hard to map these to "native" symbols.[[Special:Contributions/24.85.131.247|24.85.131.247]] 02:22, 20 October 2011 (UTC)
:::this is what i thought of doing with the " character. sure, in lisp it is a terminal, but for this simple task it need not be.--[[User:EMBee|eMBee]] 05:22, 20 October 2011 (UTC)
ISO Standard Lisp: Also known as [[http://islisp.info/ ISLisp]].

Scheme: Revised [6] Report on the Algorithmic Language Scheme, [[http://www.r6rs.org/ R6RS]].
:syntax is in chapter [http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-7.html#node_sec_4.2 4.2]

There are many common elements, and there are differences.

In addition to some prominent languages like these, there have been countless dialects. Emacs Lisp has its own particular S-expression syntax, and so does every Lisp dialect ever hacked up.

== syntax for S-Expressions ==
it seems there is no formal definition of S-Expressions, and much of the arguments in the discussion on this task seems to revolve around what is required for S-Expressions and what is not.
this [http://people.csail.mit.edu/rivest/Sexp.txt draft RFC] makes an attempt at a definition, but it was not accepted for reasons that are unknown.
: the [http://www-formal.stanford.edu/jmc/recursive/node3.html original definition of S-Expressions] by John McCarthy can be found on his homepage.--[[User:EMBee|eMBee]] 05:28, 4 November 2011 (UTC)

for a definition i'd like to separate two aspects: 
* the syntactical representation of elements
* the interpretation of these elements


###  syntax 

if we use the task as a starting point we get:
* special characters: ( ) " \ 
* whitespace: space, tab, newline (carriage return)
* symbol: a sequence of characters without special chars (and without surrounding quotes).
* string: a sequence of characters in quotes. strings may contain special characters and whitespace. only " and \ need to be escaped.
* number: a sequence of characters capable of representing a number. (optional?, do we need to define all possible number representations? (different standards for floating point, hex, octal, etc))
* atom: a symbol, string or number
* S-Expression: a sequence of atoms or S-Expressions separated by whitespace and surrounded by parentheses ()

do we need others? the rfc above introduces special syntax for hex, and base64 encodings, as well as length-prefix encodings, all of which i consider not needed. there are other standards for hex, and there are other encodings, so i don't see why they need to be treated special.


###  interpretation 

* symbols are different from strings and should be distinguished in the implementation of the parser. 
beyond that, the implementation of symbols is not defined. symbols may be implemented in any way suitable for the language. [[wp:String Interning|interning]] is optional. (i'd like to point out that in pike strings are interned. so if i use strings to represent symbols, does that satisfy a requirement for interning symbols?)
* numbers are possibly an interpretation issue and need not be defined in the syntax.

very likely this definition is incomplete, possibly even has errors. please add and criticize.--[[User:EMBee|eMBee]] 06:02, 19 October 2011 (UTC)

== practical solutions preferred ==
this is not supposed to be an academic exercise in writing a perfect s-expression parser, but it should provide a practical demonstration that serves as a starting point for anyone who really do needs to use an s-expression parser in their project.

you should ask yourself: would i use this approach in a real project? if a solution that you would really use is different from the task, please post it anyways and explain why.--[[User:EMBee|eMBee]] 04:01, 19 October 2011 (UTC)

::I used S-exps in a C++ project about eight years ago. More or less real S-exps with interned symbols, cons cells, etc.  No garbage collection was used, but rather smart pointers with reference counting. This was used for inter-process communication over sockets, as well as for storing and retrieving complex configurations easily. The project was a mail transport. Later when an IMAP4 interface had to be developed, one of the other programmers realized that IMAP4 uses S-exps. We easily adapted the library to parse and generate IMAP4 which simplified the handling of that protocol greatly. Its commands and responses could be handled on a higher level as objects, using Lisp-like functions (in C++).  None of us had known that IMAP4 uses S-exps, so we were amazed and the programmer coding the IMAP4 interface was pretty happy.  (Some conventions had to be accomodated in our reader: IMAP4 has backslashes on symbols like \SEEN (flag indicating read). This is not an escape sequence but a constituent of the symbol name.) I had a package concept implemented in symbols: symbols belonged to packages (namespaces). This was became really useful for the IMAP4 module, because it was able to read forms in a private package to prevent symbols from polluting the regular namespace.  Later in the project, we were given a requirement to develop some test tools that could be run on the mobile device to do some automated tests. I developed an evaluator over the S-expressions, creating a multi-threaded Lisp dialect with dynamic closures. I added a GUI where you could type expressions right on the device and hit EVAL. This saved them time because they didn't have to re-flash the whole image just to fix a bug in a script: upload the script and (load ...). Only if they needed a new intrinsic function in the interpreter did they have to rebuild the image and re-flash. Developing this dialect and the GUI window, and a concise, clear reference manual, took about three days (having the Lispy library in place). QA people developed multi-threaded tests which exercised various platform functions on the device (via intrinsic function bindings exposed in the dialect). Nobody in QA came to ask any major questions about the language, even though they had no Lisp experience, since the reference manual was thorough, and explained everything.  What is a form, what is a variable, what is a binding, what is an environment, what is scoping, what does it mean to evaluate, what is a function, what is a closure, etc. '''Would I use S-exps as defined by this task?''' Not likely. You really need the correspondence to a proper Lisp-like structure which distinguishes symbols (interned objects) from strings. Interned symbols allow for identification using simple equality <code>if (car(form) == foo_s) ... // if the form begins with the foo symbol ... </code>. Strings and symbols really have to be a distinct data type, otherwise you're wasting cycles on string processing, and you have hygiene issues. If you  need a unique symbol, just construct a symbol object and you got it (since it has a unique address). If symbols are strings, then you cannot make a unique symbol, only a unique-ish one based on giving the string contents that are unlikely to coincide with any other symbol.  A lot of the power came from the simplicity of the API, not from what the notation looks like. So whether I would use a given S-exps solution in a project would depend heavily on the quality of the API used to manipulate the data structure corresponding to the S-exp. I would not use a solution that required me to manage memory with malloc and free, for instance, or which had some awfully encapsulated data structures with clumsy accessors. In this regard, some particular '''solutions''' to this task may be more useful than others. I would definitely not use the C code for instance. It is just an example. For a real task, you need a well-designed, ideally mature library for this type of thing, with some kind of garbage collection. [[Special:Contributions/192.139.122.42|192.139.122.42]] 23:06, 19 October 2011 (UTC)

::''I cannot imagine any case where I would use this for practical purposes.  I already have more robust serialization and deserialization available with language primitives, [CONTINUED ...] --[[User:Rdm|Rdm]] 10:39, 19 October 2011 (UTC)''
:::which languages are those? and what kind of serialization is it?--[[User:EMBee|eMBee]] 12:04, 19 October 2011 (UTC)
::::In the context of rosetta code, I mostly work in J.  But C#, for example, also has deep support for serialization (allowing developers to define how their classes get serialized, or whether that is not possible for classes which should not be serialized).  And both languages support multiple kinds of serialization (some more human readable -- J can serialize as executable content, C# can serialize as xml, and some not so readable -- J can serialize as array structure, C# has binary serialization).  Alternatively, if I was working in javascript, I would use json (which does not have primitive support but which does have lots of available implementations). --[[User:Rdm|Rdm]] 13:51, 19 October 2011 (UTC)
:::::thanks. i am mostly interested in human readable serializations, i loathe programs who save config data in a format that i can not edit by hand. among the alternatives including json and xml i find s-expressions conceptually the easiest to parse and also the easiest to edit manually. (i don't know about json, but xml breaks if it is not nested properly making hand editing risky). whereas i believe s-expressions can not break the parser in a way that would prevent it from reading all the data (even if it is reading it the wrong way), which means there is a higher chance to be able to fix broken data programmatically after the parser is done with it. therefore i am considering to use s-expressions for my applications and one reason for this task was to find out how easy it really is to write a parser for s-expressions.--[[User:EMBee|eMBee]] 14:51, 19 October 2011 (UTC)
::::::Here are error cases to consider for human edited sexpr:  too many right parenthesis, too many left parenthesis, both ("(like this,)))(((for example)"), no top-level parenthesis (or content outside the top level parenthesis), unbalanced quotes, misplaced but balanced parenthesis, misplaced or omitted spaces.  If none of those bother you, you might consider csv as a widely supported alternative (though that might require thinking about your data differently which may or may not make it unsuitable).  --16:54, 19 October 2011 (UTC)
:::::::they are bothering, but i can for example work around unbalanced parenthesis by adding a few extra at the beginning or end, to make the parser happy, and then later analyze where the real error is. try that with xml.
::::::::Decent editors have a Lisp mode for automatically indenting S-expressions, matching parentheses, or selecting subexpressions and moving them around, etc.[[Special:Contributions/192.139.122.42|192.139.122.42]] 22:03, 19 October 2011 (UTC)
:::::::::And there's nXML mode for emacs, for example, which automates indentation and tag closing and so on when editing xml files.  --[[User:Rdm|Rdm]]
:::::::of course this has nothing to do with the task or the definition of s-expressions, it just shows that s-expressions are easier to manipulate programmatically which probably is one of the reasons why lisp is as powerful as it is.--[[User:EMBee|eMBee]] 17:09, 19 October 2011 (UTC)
::''[... CONTINUATION] so this mechanism only makes sense for interoperability.  And interoperability only makes sense when it's defined in such a way that both my implementation and that of another language are doing the same thing.  And that's just not happening here, because that is not how the task is written. --[[User:Rdm|Rdm]] 10:39, 19 October 2011 (UTC)''
::::That said, javascript does actually have some primitive support for json (eval can be used to parse json from a trusted source). --[[User:Rdm|Rdm]] 23:20, 19 October 2011 (UTC)
:::well the task is not written that way because there is no standard. however in this discussion we may be able to flesh out some kind of standard that we can agree on.--[[User:EMBee|eMBee]] 12:04, 19 October 2011 (UTC)
: Will I use what's in this task in a real project? Probably not.  Firstly, the task doesn't appear to want symbols (the wording rather actively goes against keeping symbols as symbols).
::there is nothing stopping you from implementing symbols. my own solution implements symbols too.
::how would you change the wording to encourage symbols, but not require them?
:S-expression without "S" is pretty uninteresting, it's just another old tree structure carried in a string: imagine using an XML format with only two tags &lt;list> and &lt;string>.  Without the "S", it can't represent things like hashtable or metadata, can't cross-reference data, can't annotate structure, can't define actions, can't do anything other than being a tree of strings looking pretty.  And secondly, even that is not adequately done: even if you only want the parser to be able to exchange pretty looking trees of strings, the escape required by the task is not going to be enough: no escape for unprintable chars,
::why would that be needed? as far as i can tell the task definition has no restriction on what bytes appear in the data. it should handle unprintable characters just as well as any other. they would only need escaping if you want to output the data on a terminal, but that is not part of the task.
:no well defined whitespace behavior (consider sending it by email for example),
::huh? what kind of whitespace behavior is needed for email? i am not aware of any whitespace rules inside an email body except for >From escaping. if there is any trouble with that, email already has plenty of facilities to encode and decode data. you are free to use those to process the data before parsing it. implementing these things is beyond the scope of this task.
:no encoding spec, some implementations can't even have a double quote inside the strings.
::uhm, some implementations don't support double quote inside strings doesn't mean that you can't make your implementation handle quotes. if i find the time, i will add it to mine too...
:::The "double quote" inside of strings thing is a distraction, in my opinion.  You can easily transform the data in the sexpr so if you want to add a module that re-interprets strings with an escape language, you can do that.
::::well unless the escape syntax works in a way that it transforms quotes into something completely different, the parser needs to know about the escape, otherwise how do you escape quotes? distraction or not. \" happens to be a widely accepted way to do escapes. an alternative could be [[wp:percent encoding|%hex]], [[wp:quoted printable|=hex]] or [[wp:Numeric character reference|&#hex;]]. other suggestions?--[[User:EMBee|eMBee]] 12:04, 19 October 2011 (UTC)
:::::Yes, as you point out there are escape mechanisms which do not require an updated parser.  Backslash escapes also have a history of being able to identify characters by number. --[[User:Rdm|Rdm]] 13:54, 19 October 2011 (UTC)
:::There's nothing wrong with having that be an independent module -- or, rather, there's nothing wrong with that being an independent module unless you have reasons for disliking the modules.  But you have to draw a line somewhere, otherwise the task becomes "write a lisp interpreter".  --[[User:Rdm|Rdm]] 10:39, 19 October 2011 (UTC)
::::i agree. modules are good. or at least having the code written in a way that it is easy to see where it needs to be modified to add functionality.--[[User:EMBee|eMBee]] 12:04, 19 October 2011 (UTC)
:  I'm very much not going to use this as a general data exchange format.
:: i wasn't asking if you want to use the data format, but if you want to use your own implementation which is free to fulfill the requirements that you have. the purpose of this task is not to define the data format. however, a result of this discussion may well lead to a definition. if that is the case then the task will most likely adopt the definition.
: The task is ok for demonstrating simple parsers, but if you want to use it for real work, maybe you need something more rigorous, say, [http://people.csail.mit.edu/rivest/Sexp.txt the almost RFC]? --[[User:Ledrug|Ledrug]] 04:54, 19 October 2011 (UTC)
::i have been searching for a definition for s-expressions, and i could not find anything. this one raises some interesting points and has things i disagree with.--[[User:EMBee|eMBee]] 06:02, 19 October 2011 (UTC)
:::Common Lisp (ANSI standard), ISLisp (ISO) and Scheme are programing languages which are based on S-expressions. There are differences between them but they have a lot in common. Rivest's RFC is awful.[[Special:Contributions/192.139.122.42|192.139.122.42]] 22:07, 19 October 2011 (UTC)

== Symbols and strings ==

To be more generally useful, it's probably better to distinguish between quoted and unquoted strings instead of giving numbers special treatment.  <code>0x1, 1d0, 13#4bc, 1.3f, 1_000</code> may or may not be parsed as numbers depending on what the definition of literal numbers is, and can be deferred to a separate step -- as long as the parse remembers that they are not quoted.  On the other hand, it's more likely than not that <code>"data"</code> and <code>data</code> mean completely different things, so the parser better remember that information instead of making it optional. --[[User:Ledrug|Ledrug]] 10:48, 16 October 2011 (UTC)
:you are of course right, i just didn't want to make the task to hard. in languages that don't support symbols, an object would need to be created, if that can be done. otherwise, how can a symbol be represented?
::That's the task implementor's problem, isn't it? What do you think the S stands for in S-expression? String expression? I think not. To call symbols unquoted strings is beyond laughable. A correct implementation of this task replaces A with pointers to the same object in (A A A). There is no S-expression without interning.[[Special:Contributions/24.85.131.247|24.85.131.247]] 04:51, 17 October 2011 (UTC)
:::that is not even true in lisp: in common lisp <code>(A A A)</code>, the first <code>A</code> is a function and the other two <code>A</code> are a variable. 
::::You are incorrect. The expression (A A A) denotes a list containing three repetititions of the same object, the symbol a. Whether this denotes a function call or something else is entirely up to the context. For instance, it could be embedded inside (quote (a a a)).[[Special:Contributions/192.139.122.42|192.139.122.42]] 22:29, 17 October 2011 (UTC)
:::s-expressions are just data. the interpretation of the meaning of atoms in an s-expression is entirely up to the application. in this application they are strings.--[[User:EMBee|eMBee]] 05:37, 17 October 2011 (UTC)
::::S-expressions are a notation which denotes structure. Treating them as characters is unsatisfactory. That's like saying that XML just a bunch of characters and angle brackets. It's a bunch of characters and angle brackets, plus some rules which give them meaning and make it all correspond to some kind of structure. You contradict yourself anyway becuaes you're using the term atom, which is structural. If there is no structure, then (a b c) is just "(a b c)": a string consisting of parentheses, spaces and letters. [[Special:Contributions/192.139.122.42|192.139.122.42]] 22:29, 17 October 2011 (UTC)
:::::where i am contradicting myself? of course there is structure. structure is denoted by parenthesis. atoms are denoted by strings.--[[User:EMBee|eMBee]] 01:17, 18 October 2011 (UTC)
:whether it is useful to distinguish between quoted and unquoted strings also depends on what is done with the input. unless you age writing an interpreter of sorts, the input is just data. and if the language can only handle strings as data, then what good is it to have a special representation for unquoted strings?
:but if anyone wants to distinguish between quoted and unquoted strings and skip numbers instead, they are free to do so--[[User:EMBee|eMBee]] 12:00, 16 October 2011 (UTC)
:: Well, without context, it's not like you can actually expect the code to do something useful to the symbols anyway.  All the parser needs to do is distinguish between "123" and 123, "data" and data, just stick a is_quoted flag somewhere on the strings.  If your usage later needs to tell symbols from strings, look that flag up; if not, it does no harm.  For numbers, just assume you can check the unquoted strings and see if they match some patterns later.  It's probably simpler this way and more language neutral (parsing numbers is likely language dependent). --[[User:Ledrug|Ledrug]] 13:37, 16 October 2011 (UTC)
:::that is not as easy as it sounds, not every language can associate flags with strings without creating a new class, in which case it usually isn't a string anymore.
::::Not every language has to solve every task. There is a way in Rosetta Code to exclude a language from a task when it's starkly obvious that it can't be done.[[Special:Contributions/192.139.122.42|192.139.122.42]] 22:31, 17 October 2011 (UTC)
:::::True, but if the task is changed so that most languages cannot implement the task, that would be a problem with the specification of the task and not with the languages.  --[[User:Rdm|Rdm]] 00:41, 18 October 2011 (UTC)
::::::right. the purpose of this task is to read and write structured data, in order to be able to use it to save or exchange data between systems. the vehicle chosen for this data is s-expressions. just like you would use json, xml or other formats. that should be more or less doable in any language that's of practical use.--[[User:EMBee|eMBee]] 01:17, 18 October 2011 (UTC)
::::::A task not doable in most languages would tend to indicate that the task is not properly specified in a way that suggests there is any sort of concrete requirement or algorithm, or asks for the computation of something that is not tractable on a Universal Turing Machine ("write a program which decides whether a program in the same language terminates ..."), or is very specific to some platform ("create and manipulate sprites on a Commodore 64 ..."). There aren't any signficiant barriers are against implementing interned symbols properly in a general purpose programming language. The language doesn't have to have a symbol data type for this to be possible. Interned symbols can be "greenspunned" in C which has only low-level machine-oriented scalar data types and aggregates thereof.[[Special:Contributions/192.139.122.42|192.139.122.42]] 21:04, 18 October 2011 (UTC)
:::take a look at the pike example, now that i introduced the Symbol class without handling numbers in the parser, all but quoted strings become Symbols and i find myself having to emulate not only strings but numbers (still incomplete) as well. if i would parse numbers upfront i could store them as such and the Symbol class would be simpler. i will still have to deal with strings in the Symbol class, but with numbers out of the way i could require explicit casting to use Symbols as strings. as long as Symbols can contain numbers the Symbol class has to tell if it is a string, int or float and behave accordingly, because if i have to check what the type of the symbol is before i can use it that would just be to cumbersome.--[[User:EMBee|eMBee]] 14:03, 16 October 2011 (UTC)
:::i could of course stick all tokens into a token class and handle the conversion at a later step. but then in order to use the input that conversion step is mandatory and i just end up with more complicated code for something that was supposed to be simple.--[[User:EMBee|eMBee]] 14:15, 16 October 2011 (UTC)

== So... what is the point here? ==

Is the point here to represent data in a way which is natural to the language (thus, for example, allowing the language to throw errors for unquoted character sequences which have been reserved), or is it to emulate another system?  And are we producing a result which displays pleasantly, or do we want type annotations?  And if we want type annotations, what types do we support and when do we use them?  (In my experience, S expressions are simple only if you ignore most of the details of how they are implemented, and the task description seems ambivalent about where to draw the line.  That said, any concept is simple once you understand it, but here I am focusing on the task description and not on the abstract concepts.)  --[[User:Rdm|Rdm]] 17:34, 17 October 2011 (UTC)

:It seemed straight-forward for Python as I was given sample input and a specific example of what the intermediate Python datastructure has to be. (Nested lists with ints as ints, floats as floats, strings for the rest). Maybe it needs to be emphasisized for other languages too? --[[User:Paddy3118|Paddy3118]] 19:16, 17 October 2011 (UTC)
::So am I supposed to emulate python's data language (and, if so, where is the specification for that)?  Or am I supposed to use native types (which do not precisely match the word syntax being asked for, but would allow support for things like symbols, rational numbers, complex numbers and representations of functions)?  Anyways, for now, I am not implementing any data language, since none was specified.  --[[User:Rdm|Rdm]] 19:22, 17 October 2011 (UTC)
:::you are supposed to use native types where it makes sense. the python example was just something to get people started. now that we have a bunch of implementations i am considering to remove that.--[[User:EMBee|eMBee]] 01:26, 18 October 2011 (UTC)

:If your language has native support for nested lists of floats, ints and strings, then wouldn't that be enough to emulate the python? (Or nested lists of a variant type that could hold a string/float/int)? --[[User:Paddy3118|Paddy3118]] 20:13, 17 October 2011 (UTC)
::Yes, the data structures are doable.  But think about what each of these should be represented as:
:::# 1
:::# 0.1
:::# 127.0.0.1
:::# Do
:::# Don't
:::# 1e4
:::# 1r4
:::# 1j4
:::# ...
:::# :::
:::# "food"
:::# food
:::# 1efood
:::# 0.food
:::# 'food
:::# `food
::and so on... The task implies that some of these should be treated differently, but it also says that there are no special characters outside of ( ) " and whitespace...  Anyways, right now, I see several courses of action:  I can attempt to emulate Python's data language -- but I do not know the syntax nor rules of that language.  I can use a native data language with a special case for quote handling -- and some of the above unquoted words would result in errors if I did that.  Or, I can not implement any data language and leave issue to some other module.  And, for now, despite the task's suggestion that I should implement a data language, I am implementing the null data language (where every data element is preserved as a sequence of characters even if those characters are numeric).  --[[User:Rdm|Rdm]] 20:26, 17 October 2011 (UTC)
:::use a native data format, optionally with a special case for quote handling. anything that you can not natively represent may be stored as a string. so if there is a language that doesn't have numbers, then it's strings all the way.--[[User:EMBee|eMBee]] 01:26, 18 October 2011 (UTC)
::::Ok, that helps, though it's still ambiguous.  For example, should words that can represent functions be treated as representations of those functions or as symbols?  (This is in a language where symbols are a valid data type -- which, in effect, implements intern -- but does not grant any special syntax for symbols). --[[User:Rdm|Rdm]] 01:32, 18 October 2011 (UTC)
:::::treating them as representations of functions would be an interpretation of what the value means. unless the application of this is to for example serialize code, this is unlikely to be useful. the input should be treated as data. it should not be interpreted except in ways that lets it remain data. 
:::::if you consider a hypothetical language where the only datatype is string and numbers are treated as functions or commands, then perhaps in that language numerical values should remain strings.--[[User:EMBee|eMBee]] 02:11, 18 October 2011 (UTC)

###  J 

Ok, I have implemented that, but this data handling mechanism is more complex than the list representation (because it has to deal with so many distinct cases), so I think it deserves some kind of explicit description in the task.  --[[User:Rdm|Rdm]] 10:52, 18 October 2011 (UTC)
:can you elaborate the difference? the solution should easy enough to make it useful practically. this should not be a demo on how complex it is to implement an s-expression parser that follows everyone's wishes, but it should provide a practical starting point for those who really do need an s-expression parser for their project. if the data handling mechanism goes beyond what you would normally do in J, then maybe it isn't the right approach. unfortunately i find J very hard to read, so i can't really understand the code.--[[User:EMBee|eMBee]] 03:52, 19 October 2011 (UTC)
::The difference between what?  Between the parser that handles the list syntax and the parser that handles the atoms?  If that is what you are asking about:  The parser that handles the list syntax returns a list which itself contains things which are either contained parsed lists or strings.  The strings are the sequence of characters which appeared in the input -- things like data (four characters), "quoted data" (13 characters) or 123 (three characters).  The parser that handles the atoms is responsible for converting "quoted data" to an eleven character string and for converting 123 to an integer.  The difference is that the list parser does not concern itself with what the strings represent and the data parser does not concern itself with the list structure. --[[User:Rdm|Rdm]] 22:44, 19 October 2011 (UTC)
:::i meant the difference between "this data handling mechanism" and "the list representation". so what you are saying is that you'd like the task description to explicitly explain that parsing the structure and parsing the data into numbers, strings, symbols etc, should be separate steps?
:::i don't know if it is necessary to make them completely independent steps, but i think it is a good idea to keep them separate enough to that for example the data handling is done by a function call and someone using the code can easily swap out the function with another one. i think the pike solution exemplifies this.
:::how can we write the task description to make this more clear?--[[User:EMBee|eMBee]] 01:51, 20 October 2011 (UTC)

==How about concentrating on the example?==
How about ''not'' worrying too much about the fact that S-expressions are mentioned and concentrating on doing enough for a Rosetta Code task.

Remember - this shouldn't necessarily be code to be used, as-is, in some other project. There is an example given and, apart from the examples output being couched in terms of Python, there should be enough there: 
* Here's an example S-expression, 
* Here's what your intermediate format should look like, 
* And the final full-circle output should look like the input (apart from whitespace maybe).

This would address all the talk here on formats, Lisp, escape characters, and what types of atoms are needed. You would need strings, integers, floats and a nestable list structure to put them in.

The task description might need some work on maybe taking the specific reference to Python out and instead explaining that it is a nestable native list structure, etc, etc, .... It could also do with stressing the importance of implementing the task example over any wider considerations. --[[User:Paddy3118|Paddy3118]] 03:52, 20 October 2011 (UTC)
:well, i find the discussion quite enlightening so i don't mind the arguments. i am interested in solutions that are at least usable as a starting point. so it would be nice if they are written in a way that it is possible to swap out parts and extend where needed.
:the python reference is already gone.
:how would you stress the importance of implementing the task?--[[User:EMBee|eMBee]] 04:53, 20 October 2011 (UTC)

::There has been too much of the Python taken out I think. The intermediate language specific list structure should have been left in but instead of saying it was Python, just say enough so that implementers knew to use their languages native ints, floats, strings and nestable lists. (They might have to create a [[Linked list|linked list]] data-structure if their language or a library does not produce one).
::Also removed is the notion of taking the S-expression through to the internal representation, then back to an S-expression of the original which I thought was good too.  --[[User:Paddy3118|Paddy3118]] 07:08, 20 October 2011 (UTC)
:::huh? the only thing ever removed was: "must, in addition, recognise integers and floating point literals (that are distinguished as digits followed by a decimal point, followed by digits)." replacing it with "should, in addition, recognize numbers if the language has appropriate datatypes." i have also moved that paragraph in the process. this was the last edit on the description [http://rosettacode.org/mw/index.php?title=S-Expressions&diff=next&oldid=123268 (see diff)]
:::i have checked the complete history to make sure i didn't delete something by accident.--[[User:EMBee|eMBee]] 09:06, 20 October 2011 (UTC)

::::Hi eMBee, This got lost:
:::::
```python
[["data", "quoted data", 123, 4.5]
 ["data", ["!@#", [4.5], "(more", "data)"]]]
```

::::--[[User:Paddy3118|Paddy3118]] 18:42, 20 October 2011 (UTC)
:::::oh, now i understand what you mean: keep the list but describe it as a generic nested list, not a python list. 
:::::i removed the list intentionally because i think there are now enough examples of how a native structure might look like. at least the lisp solutions, pike, python and ruby produce directly native results. (for the other languages i can't tell how native lists look like.)--[[User:EMBee|eMBee]] 00:22, 21 October 2011 (UTC)

== Lisp Solutions ==

The lisp solutions demonstrate the steps needed to actually read an s-expression from a string, but for those interested in learning how to write a simple parser in lisp it would also be nice if we could also have solutions that re-implement a parser with basic lisp tools. assume for example the s-expressions are written with [] instead of (). 

how would a parser look like that turns 

```lisp
(setq input "[[data \"quoted data\" 123 4.5]
 [data [!@# [4.5] \"[more\" \"data]\"]]]")
```

into

```lisp
((data "quoted data" 123 4.5)
 (data (|!@#| (4.5) "[more" "data]")))
```

and back?--[[User:EMBee|eMBee]] 09:49, 20 October 2011 (UTC)
:Unfortunately the task isn't to show a parser but to get the S-expression into an internal form that just happens to be native Lisp.[[User:Paddy3118|Paddy3118]] 08:51, 23 June 2012 (UTC)
::the intention of the task was to create a collection of s-expression parsers as well as demonstrate how to write a parser. if the lisp solutions could show how to implement an actual parser similar to what needs to be done in most other languages, i would consider this a nice addition to the current solution. so, yes, if someone wants to implement a full parser go ahead.--[[User:EMBee|eMBee]] 15:18, 24 June 2012 (UTC)
:You should show how easy it is to do in Lisp. S-expressions have been used as a data format where much of the processing is done in languages other than Lisp - for example in the Electronics industry. --[[User:Paddy3118|Paddy3118]] 08:51, 23 June 2012 (UTC)
::Using S-Expressions as a data format in a language other than Lisp?!  Those godless heathens!!!  =)  --[[User:Lhignight|Larry Hignight]] 11:38, 23 June 2012 (UTC)

::: I've just looked up the formats I was thinking of. They are [[wp:EDIF|EDIF]] and [[wp:Standard Delay Format|Standard Delay Format]] which are heavily S-expression inspired but not the same as Lisp. --[[User:Paddy3118|Paddy3118]] 21:37, 23 June 2012 (UTC)

::Hi Donald, I think the Common Lisp section is solid now.  Check it out and let me know what you think. --[[User:Lhignight|Larry Hignight]] 11:38, 23 June 2012 (UTC)

:::Hi Larry, I think the Lisp does the task and a bit more which is fine as the extra is explained and close enough to the topic to belong. --[[User:Paddy3118|Paddy3118]] 21:28, 23 June 2012 (UTC)

== Writer in separate subtask? ==
Aside from the Python and Ruby implementations, I haven't seen any other languages writing native code back to S-Exps.  Perhaps the writer should be spunoff as a separate sub-task?  Also, many of the implementations don't show any kind of output.  --[[User:Lhignight|Larry Hignight]] 22:28, 22 June 2012 (UTC)
: (moved this out of the lisp section as it seems rather unrelated to it.
: one goal of this task was to provide a collection of code to handle s-expressions. for anyone writing a library both sides should be covered and seperating them may not be worth it. consider the java solution for example. that is real code in production use. splitting out the writer from it would just needlessly make it harder. also in most cases i believe the writer is not very hard so there wouldn't be much gain from separating it out.--[[User:EMBee|eMBee]] 15:18, 24 June 2012 (UTC)
: if any solution is missing a writer then it is incomplete. showing the output would of course also be nice.--[[User:EMBee|eMBee]] 15:23, 24 June 2012 (UTC)

== TCL native types ==

what would a native structure look like? i don't quite understand the comment: ''A “native” data structure could also be generated, but then that would turn things into lists that are not in the original.''
also, since tcl doesn't have a type system i think the tags are not really needed, except for the quotes.

since tcl doesn't need quotes for strings can't the structure be like: 
{{data "quoted data" 123 4.5} {data {!@# {4.5} "(more" "data)"}}}
using " as part of the string value?--[[User:EMBee|eMBee]] 10:53, 3 November 2011 (UTC)
: In TCL, "everything is a string" (or list, depending on how you look at it). <code>{a b c}</code> is the same as <code>"a b c"</code>, which will lead to some unfortunate ambiguities. --[[User:Ledrug|Ledrug]] 16:25, 3 November 2011 (UTC)
:: ah, yes, that's a problem. actually, i found that the real problem is that a single element list is indistinguishable from the element itself:
<blockquote>
```txt
% puts [list [list [list a]]]
a
```
</blockquote>
:: therefore tagging is already needed just to distinguish lists from atoms.--[[User:EMBee|eMBee]] 17:37, 3 November 2011 (UTC)
::: There are dirty ways to distinguish (which I use in the task on JSON) but generally speaking Tcl programmers write their code to not need to ask whether a particular word is a list or terminal; it's not a distinction that they draw. (It's a different attitude.) The "native" representation of the above would be:
::: 
```txt
{{data {quoted data} 123 4.5} {data {!@# 4.5 (more data)}}}
```

:::But as you can see there's a fair loss of information because lists ''are'' a subtype of strings and singleton lists can be rendered like their first element under (common) circumstances. The alternative to putting the tagging inline would have been to have returned a separate type descriptor. (Generating everything from the fully parsed representation is left as an exercise.) –[[User:Dkf|Donal Fellows]] 10:47, 4 November 2011 (UTC)

==A bit late to add the extra credit?==
:''"'''Extra Credit:''' Let the writer produce pretty printed output with indenting and line-breaks."''
There are already a large number of solutions to the task. I was thinking of arguing for the removal of the extra credit that has just been added for that reason, but would others give their opinions. Thanks.  --[[User:Paddy3118|Paddy3118]] 06:41, 26 October 2012 (UTC)
:: if it wasn't so late i would have added it to the task directly. being late is the reason for it being extra credit. this way the exiting tasks are not invalidated.
:: one could argue to make this a separate task though, but it's debatable which is better. --[[User:EMBee|eMBee]] 07:18, 26 October 2012 (UTC)

== extend task for Associative Arrays, Mappings, Dictionarys? ==

I'd like to extend this task to support Associative Arrays using the Lisp method of Cons Pairs:
 ((key1 . value1) (key2 . value2)) for { key1:value1, key2:value2 }

i think it is better to add it as Extra Credit to this task instead of creating a new task for it because a new task would require all of the code that this task needs, and then we'll get people wondering if the tasks should be merged.

i am aware that (a b) is equivalent to (a . (b)) in Lisp and therefore any list could be treated as an associative array, however for the purpose of this task (and for the usability as an interchangeable data format, we could just require that a pair with a list as value be written as (a . (b))

for the same reason a solution in lisp better ought to use a hash type when reading the data.
--[[User:EMBee|eMBee]] 11:07, 26 October 2012 (UTC)

== Haskell version failing with GHC 7.10.3 and current Stack ==
Compilation failing with two errors at the moment:
# 'between' is not in scope. (Not included in the imports)
# "Precedence parsing error cannot mix ‘<?>’ [infix 0] and ‘<?>’ [infix 0] in the same infix expression"-- [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:12, 27 April 2017 (UTC)

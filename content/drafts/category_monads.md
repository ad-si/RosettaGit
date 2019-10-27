+++
title = "Category:Monads"
description = ""
date = 2017-12-23T13:34:38Z
aliases = []
[extra]
id = 20012
[taxonomies]
categories = []
tags = []
+++

In functional programming, the [[wp:Monad_(functional_programming)|Monad]] design pattern is a general solution to the problem of nesting (or 'composing') a class of functions which enclose their output values in some kind of useful wrapping. The output envelope might, for example, contain, in addition to the returned value, a log string, or a boolean indicator of whether or not the input was a legal value. Sometimes the output might simply be enclosed in a list representing a range of possible values rather than a single value.

Functions of this type can not be directly nested with each other, because their output type (wrapped) does not match their input type (raw and unwrapped). 

The monad pattern consists of writing two higher-order functions which solve this problem, allowing the programmer to easily nest the application of such functions, by abstracting out the mechanics, and making sure that a function does not choke on an unexpected input type (a wrapped type, when it was expecting a raw type).

More specifically, the two higher-order functions of the monad pattern handle the details of: 1. wrapping data in a particular kind of envelope, and 2. Providing other functions with direct access to the contents of an enclosing envelope. 

These two functions are sometimes named as follows:

# '''Return''' or '''Unit''', which wraps a piece of raw data, returning the wrapped 'monadic' form. 
# '''Bind''', which applies some other function directly to the contents of a monadic wrapper, obtains a result, and returns a wrapped form of that result.


(The term monad derives from [[wp:Monad_(category_theory)|a concept in category theory]]. In ancient Greek the word μοναδικος means 'consisting of units').

Commonly used monads:
 
;the Writer monad
:Nests functions which return their output in an envelope that includes a log string. Nesting ('composing') such functions generates a concatenated log of a chain of function applications.
;the Maybe monad
:Nests partial functions which return their output in a wrapper that includes a boolean flag – indicating whether or not the input value was legal. Composition of these functions avoids the need for exception handling when an illegal value is encountered somewhere along the chain of function applications. The '''invalid''' flag is threaded up through the monad, allowing all further attempts at function application to be bypassed.
;the List monad
:Nests functions which output ranges of possible values, rather than single values. Composing these functions yields cartesian products, and a convenient encoding of set comprehensions.


(Other frequently used monads include the the IO monad, and the State monad, which make it possible to thread effects and state changes through a 'pure' composition of function applications).

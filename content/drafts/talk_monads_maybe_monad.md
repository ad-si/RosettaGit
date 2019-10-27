+++
title = "Talk:Monads/Maybe monad"
description = ""
date = 2019-10-03T15:35:03Z
aliases = []
[extra]
id = 20020
[taxonomies]
categories = []
tags = []
+++

==Perhaps a specific task ?==

It can help comparison across languages if there is a specific task with a readily intelligible motivation. Providing a specific set of inputs and expected outputs can make the code even more directly comparable.

One candidate might be to: 
# Create and compose safe versions (number -> Maybe number) of 3 partial functions (reciprocal, sqrt, log)
# Show the results of applying the required composition to a specific set of input values. In the ES5 JavaScript example, I have used, for instance [-2, -1, -0.5, 0, 1 / Math.E, 1, 2, Math.E, 3, 4, 5]

==Languages to which a Maybe monad is less relevant ?==

Interesting comment on the difficulty of finding something useful for J. I'm not sure whether the implication is that:
* useful things would entail a bit too much complexity (in the case of this particular monad) or that
* there might be scope for framing these tasks more in terms of a class of '''problem''' than a class of solution ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:37, 4 February 2016 (UTC)
: I agree - or, more specifically, I could accept either interpretation. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:14, 4 February 2016 (UTC)
:: I suppose I see the deeper problem as the tension between structure (often nested) and sequence (often threading more than one strand of data). Not sure if that resonates with the J case. Does J have a more natural way of avoiding/handling exceptions when a input somewhere inside a function nest is out of range ? (or is 'function nest' perhaps a less relevant form of structure in the array model ?) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:37, 4 February 2016 (UTC)
::: This is a real tension in J:
::: "Exceptions" either cancel processing, meaning no result array, or they must be avoided. If they are avoided, then you need some value in the result for that case. So there's been some extensive work done, within the language, to implement best effort consistent results for exceptional cases where that seems to make sense (and, of course, to generate errors for the nonsensical cases).
::: Also, the language does have try/catch mechanisms, but usually best practice is to instead to use preconditions, to normalize the data, or so on. Exception are usually best thought of as a sign of a problem with the code rather than a sign of a problem with the data. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:56, 4 February 2016 (UTC)

==Monad Description==
Hi, the task might be improved with a description of the Monad on the page (or a link to a relevant description like that of the main category page)? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:08, 29 September 2019 (UTC)
:I've added a draft description of Monad. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:01, 2 October 2019 (UTC)
::How do you understand the application of that formulation to, for example, the '''list''' monad (for which the bind operator is the composition of map and concat), or to the function or IO monads ? It may be that I haven't yet understood your reference in the phrases 'encapsulates several other types' and 'eliminates boiler-plate code'. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:35, 2 October 2019 (UTC)
:::I wouldn't worry about that issue. At most it's the difference between "several" and "one or more" (or even "zero or more", assuming the language supported empty monads as a construct -- they can't be used, but might have use as a placeholder in some contexts). That said, list monad does deal with both lists and elements of lists, so it is part way there. (Alternatively: in some type systems, the length of a list is a part of its type.)
:::But my quip does need a mention of bind and return. So I should fix that. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:54, 2 October 2019 (UTC)
:::And.. IO Monads? IO Monads can be thought of as encapsulating the external universe (not for direct inspection, but still...). How many types does that involve? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:59, 2 October 2019 (UTC)
::::Fair enough - the stakes, as you say, are not high. 
::::If anything is missing, perhaps it is the element of *sequencing* these 'computations in a context' ? The *applicative* abstraction also involves computations embedded in some context, but in the applicative case each successive context-embedded computation is autonomous – without sequential dependencies. In contrast, the monadic bind unwraps, applies a supplied function which both computes and rewraps/re-embeds, and then allows the *next* 'computation in a context' to build on the value resulting from the predecessor. It's this possibility of allowing one embedded computation to depend on the outcome of another which is the distinguishing feature of monadic enchaining. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:22, 2 October 2019 (UTC)
::::PS another thing which might be worth tidying is the thematic clause – the opening proposition. The monad is not, of course, the type itself, but the wrapping and bind functions which enable monadic computations with that type. The envelope is not the correspondence. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:48, 2 October 2019 (UTC)

::::More concretely, defining Just and Nothing, Left and Right, or Lists, for example, gives us a type but does not give us a monad. 
::::The monad instance for a type consists of well-formed monadic functions (bind, and return/pure) which enable monadic enchaining of computations *embedded in that type*. List is not a monad, nor are the Maybe or Either types in themselves. The List, Maybe and Either monads consists of the bind and return function instances written for each of those types. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:02, 2 October 2019 (UTC)


### Some notes

A monad consists of a pair of functions (specialised for a particular data-type or other computational context), which simplify the pipelining of computations which are wrapped in that context. 

The two functions are sometimes called return and bind, or pure and bind, or '''η''' and '''μ'''.
* pure/return simply wraps a raw value in a context (for example, wraps a number in a list).
* bind takes two arguments: (1) a wrapped or context-embedded value, and (2) a function which applies to a raw value but returns a wrapped value.


When applied, bind takes care of the mechanics of:
# Extracting the raw value from the enclosing context,
# Applying the supplied function to it, and returning the wrapped output value.


To use the metaphor of postal correspondence, pure / return places the letter in an envelope.
Bind assists the writer by discarding the envelope, and offering its contents to the reader for perusal and digestion.
(The writer (or supplied write-and-wrap function) drafts a response, and places it in a new envelope, continuing the chain of correspondence)

The data-type or context (the envelope or functor) is not itself the monad. The monad consists of the functor-specific pair of functions which abstract away the boiler-plate required for wrapping and unwrapping, (extraction from a context, and embedding in that context), to facilitate a chain of computations that are embedded in some enclosing context.

A little more formally [https://en.wikipedia.org/wiki/Monad_(category_theory)#Formal_definition] (see definition 1.5 in Moggi, which refers in turn to MacLane) A monad over a category C is a triple (T, '''η''', '''μ''') where T:C -> C is the functor (for programming purposes, the context of the computation), '''η''' is the function which simply 'lifts' a raw value into that context (places a letter in an envelope), and '''μ''' is the function from the incoming envelope containing one message, to the outgoing envelope, which contains the new derived (responding) message. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:12, 3 October 2019 (UTC)


### Foundational references


S. MacLane. Categories for the Working Mathematician. Springer Verlag, 1971.

[https://core.ac.uk/download/pdf/21173011.pdf | Moggi 1991, Notions of computation and monads]

[https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf | Wadler 1992, Monads for functional programming]

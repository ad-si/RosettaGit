+++
title = "Talk:Evaluate binomial coefficients"
description = ""
date = 2019-02-24T15:31:52Z
aliases = []
[extra]
id = 6951
[taxonomies]
categories = []
tags = []
+++

==Task Needs Refinement==
The task needs more work. I've made the math look nicer, but it would help if we had some particular values to try to calculate too; I've done binom(60,30), but that requires a non-trivial component somewhere — even the result won't fit in 32-bit ints, let alone any intermediate values — so I don't know if it is really suitable as an actual task. –[[User:Dkf|Donal Fellows]] 00:39, 12 April 2010 (UTC)

: I've refined this task a bit, and made it binom(5,3), okay? --[[User:Alegend|Alegend]] 01:03, 12 April 2010 (UTC)

:: That's great. Thanks! –[[User:Dkf|Donal Fellows]] 12:52, 12 April 2010 (UTC)

::: Welcome! :D --[[User:Alegend|Alegend]] 21:00, 12 April 2010 (UTC)

: In my times binomial coefficients were calculated using asymptotic series, which is of course a way different task. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:53, 27 June 2010 (UTC)

== Type Double inadeqate  ==

The binomial coefficients are of type Integer. If you use type Double instead (as in Java translated from Python), for instance binomCoeff(49, 6) gives 1.3983816000000002E7

== Task Title ==

Starting a task with the word "Evaluate" is unhelpful - all tasks involve evaluating. I suggest renaming this "Binomial coefficient".

==Edits on April 17 2016 left formulae invisible on OS X Chrome & Safari==
One problem will be the introduction of redundant white space flanking the Latex expressions inside Math tags. There may be others, but visibility should be restored by reverting the contents of Math tags to their pre-April 17 state.

==Please desist from deleting Functional Python examples==

Paddy or Donald (User:Paddy3118), please curb your appetite for deletionary vandalism, if you can. The example which you deleted as 'obfuscation' was deliberately '''clearer''' than the incumbent, explicitly labelling the implicit products and factorials. It was also, unlike the existing functional version, compatible with Python 3, which requires an import of reduce. Finally, it was better optimised for code reuse. 

There is no need for a resurgence of this destructive and gratuitous campaign – it is certainly not constructive, or good for Rosetta Code – and I have restored the deleted original. You did something similar recently to an example deriving cartesian products from the applicative abstraction. (Undeterred, apparently, by the fact that I was the author of the Cartesian Products task :-) Functional programming is clearly not an area in which your expertise or enthusiasm are particularly concentrated, but that doesn't really seem to be a sufficient reason for aggressively deleting examples of it without warning. Does deletion really give such a thrill ? Perhaps you could usefully consider getting such thrills elsewhere ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 10:57, 21 February 2019 (UTC)

: You continue to write bad Python under the mistaken guise of it being "functional" and so OK. There is no need to introduce more functions to hide trivial calls to builtins such as range, and no need to go to the trouble of adding comments above each function for what should be in a docstring. 
: It reads like some other languages code, poorly translated into run-able but not idiomatic Python. 
: I suggest you improve your code generation Hout. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:42, 21 February 2019 (UTC)
:: The quality of code is a function of what it is optimised for. Mine is optimised for reliability, high levels of code reuse, and speed of writing. Perhaps there is a context for which yours is well optimised (more for the parade ground than for the real battle-ground is my impression) but it would not work well in mine (not least because it appears to yield the wrong result with slightly surprising frequency. Perhaps you feel that its (occasionally chimeral) adherence to a cherished style is adequate compensation for this uncertain reliability ?).
:: More constructively, the terms which you mutter during your deletion frenzies ('obfuscated', 'convoluted') may well express a structuring difference between imperative and functional modes of coding. Writing imperative code has the advantage of requiring very few concepts (sequence, branching, mutation and looping), which give it a usefully low entry barrier.  
::Functional coding has much less need of sequence, and even less of mutation or looping, but it does require a number of other concepts – significantly more than are needed to practise the imperative mode.  The higher conceptual entry barrier of functional method has been shown to yield faster coding and lower bug counts, but can of course be a source of confusion and perhaps even irritation to those who haven't needed or wanted to cross it.
:: Can I suggest that the next time you find something puzzling or unfamiliar (Applicative functors ? Currying ? Monads ?) that in lieu of angry deletion, you experiment instead with eliciting clarity ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:06, 21 February 2019 (UTC)

: You continue to add your personal coding style that flouts established Python idioms and features hence its removal. You use personal type hinting, when Python has type hints; (and before that, had an established way of adding such information in docstrings). You add function documentation as comments instead of in a docstring. I have pointed this out to you before - I suggest you update your code generation.
: Functional programming and what you think my skill level is, is a diversion on your part; the real issue is you overriding established style for your ''personal'' convenience.
: --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:37, 21 February 2019 (UTC)

: I have improved the functional code by replacing the type information as comments, by the language supported [https://docs.python.org/3/library/typing.html type hints]. Typing <code>help(binomialCoefficient)</code> for example, will return typing information in the way to be expected by Python programmers. Your original type comments above function definitions would need equal effort it seems, to add to a function docstring and would at least help in documnting code in the standard way, but even that was not done - it looked like a poor translation from another programming language. (As you know). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:47, 21 February 2019 (UTC)
:: I'm sorry Donald, but you must stop vandalizing material of which you have a poor understanding.
:: The idiom of functional programming is the semantics of math – not arbitrary stylesheet convention. A curried enumFromTo function which returns a list from the limits of an enumeration is not the same as uncurried Python range function whose arguments do not match the limits.  It composes differently, and returns a value of a different type. 
:: Removing the semantic type comments (use of which is a very well established convention in functional programming - across different languages) simply lowers the quality of the code for a functional programmer. Your actions are not constructive, and this is clearly not an area of competence for you. Stick to what you know, and avoid gratuitous damage to the code of others. Write the code you personally prefer, and learn to desist from deleting and vandalizing the code of others. We disagree, and are optimising for different things. The Rosetta community will benefit if we write alternative versions, it will '''not''' benefit if we go around childishly deleting and defacing things that we disagree with. Enough - this has gone on for far too long, and does no service to your dignity. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:02, 22 February 2019 (UTC)

::: The language is Python. The functional idiom should adapt to the language that it is written in; you go so far, then seek to mask the rest with cries of "the rest doesn't apply to me". Python has a community that knows about docstrings, and type hints and modularisation in a certain way. It allows the comunity to learn faster as more things work as expected. Instead of explaining what does not need explaining, try defending your use of type info as a comment rather than using type hints? 
::: You read as if you state you are above this aspect of Python because your cross-language functionally standard (to a standard never divulged, and only grudgingly admitted as not being from Python) takes precedence.
::: Its not the functional style of programming I am taking issue with; if someone wrote a class without a docstring and Python type annotations, but preceded it with a comment block with Java-esque type anotations as comments, then I would take issue with that too - because you have the information but haven't gone that extra step to use the Python idioms - it may well look more like som other languages entry, but it's hardly idiomatic for the ''Python'' community - not some other languages community; and its disingenuous when entries can be marked as being translated from others. (Even then, a better entry would try and translate idioms too). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 00:47, 22 February 2019 (UTC)
:::: Your deletions are absolutely not a 'reversion to Python semantics' – they never moved, nor could they – what you have done is to vandalize and delete the next level of abstraction up - the more mathematical semantics which are a definining feature pure functional programming in any language.
:::: You have also crudely deleted the type semantics comments, making the code less readable, less easily re-used, less reliable, slower to refactor.
:::: The main problem is not, however, your peculiar sense of code quality, which demonstrably places reliability in second place (see your own edit history, and your dependence on others showing you that your code produces the wrong result), or your angry opinion that only one style of programming should ever be practised in Python, or indeed your failure to appreciate that using well-established (and mathematically well-founded) abstractions, with shared names, across several languages is: 
::::'''a)''' a classic and excellent source of Rosetta insight 
::::'''b)''' a well established practice in Python modules (see the reference to Haskell and other languages in the '''itertools''' module documentation preface).
:::: The real problem is that you have returned to a very personal and slightly disturbing campaign of personal harassment and persecution, and to an equation of your own opinion (and exlusive authority on Python) with divine righteousness, which you apparently believe to fully authorise utterly appalling behaviour, include reckless deletions, impoverishing vandalism, and clear abuse of privilege to '''curtail the editing rights''' of those who make a different use of Python, and write it in a different style. 
:::: Python is not your biggest problem right now. I think you may need a bit of help. This pattern of black and white cognition, and self-righteously destructive behaviour, is bound to be affecting other aspects of your life, and damaging other human relationships. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 05:41, 22 February 2019 (UTC)


I suggest you take this issue to the wider Python community. Try and get a mention in the Python style guide having shown to the community that your style has merit, and giving the community, in turn, a chance to suggest how it might best fit into the Python ecosystem. In fact, Python has a [https://www.python.org/dev/peps/pep-0001/ PEP] system you might want to try that is ''the'' way of getting change accepted. I note that you don't state that your style is idiomatic Python that I am missing, just that your style should take precedence. By the way, this could have been avoided if you could just point to community PEPs and/or discussion threads demonsrating that your style is accepted by the wider Python community.

Your ad hominem attack has sunk to name calling. Try convincing the community, (or showing me that the Python community ''is'' already convinced). 
 <code>help(your_style_function) # -> not much</code>.

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:32, 22 February 2019 (UTC)

: There is an ample literature on functional programming in Python, and I have no interest in influencing (or, unlike you, crudely deleting) anyone else.
: The PEP guidelines (and the subset of Python which they canonize) are not optimised for functional programming, and have never so far aimed to be. The ex-BDFL was explicit in his personal aversion to structuring code as a composition of pure functions ('lemon juice' he called it), so those who wish to write functional code are (as is widely documented online) still forced to swim against the tide of some of the design decisions associated with his period of influence.
: The use of Hindley-Milner style type comments in functional code written in Python, and several other languages, is a mathematically and practically natural choice, and not, alas, something that I invented. It is well-established outside the parochial boundaries of Rosetta Code, and I have previously pointed you to good examples of it.
: Neither technical issues, nor your chosen definition of the 'The Python Community' or sub-community are the real issue here. 
: The real issue is a simple asymmetry:
: '''1)''' I have no interest whatsoever in deleting or vandalising your code (unreliable and stylistically inconsistent though I may sometimes notice it to be), but 
: '''2)''' you repeatedly act out a burning need to delete and vandalise mine.
: The real question is whether we feel that that this egregious behaviour (nobody else does this) is good for Rosetta Code, or even really good for you. 
: There is a very strong sense of limited self-knowledge and addictively (quite frankly, abusively) destructive behaviour here. 
: What is it about my code that makes you feel so threatened, and so urgently motivated to crudely delete ? Wouldn't you rather simply write more and better code of your own, and work to convince yourself and others of its superiority ? If you were really so convinced of that, you would feel no need for frantic deletions.  
: If you really think that your Python code is good, just let it shine in contrast to mine :-) 
: Relax and allow others to judge for themselves – they will, anyway. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:18, 22 February 2019 (UTC)

:: I read your diatribe, and have noted my issues several times, above.
 Your summary is misleading, (but our actions on the page seem to be converging). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:44, 24 February 2019 (UTC)
::: We disagree, and that's fine - I have never deleted or interfered with your code, and I'm sure we can converge on an agreement that you don't continue to delete or interfere with mine, which is optimised for a different set of goals, and constructed and commented in a different way.
::: It must be difficult, and perhaps unexpected, to have Nigel and I reflecting back to you that your behaviour constitutes harassment and bullying, and I'm sure you would wish to avoid any repetitions.
::: A first step to helping yourself avoid this kind of behaviour might simply be to allow yourself the luxury of having an opinion, rather than depersonalising your pronouncements and seeking refuge behind imagined tablets of stone.

::: There is no need to replace a braver and more honest statement like 'I disagree with your summary' with a detached and unsupported olympian claim like 'your summary is misleading'.
::: Similarly, allow yourself to notice that your detached and impersonal expression 'hence the deletions' really means "so '''I''' deleted '''your''' work" – a real interaction between two real people.
::: If you can move away from depersonalising yourself, you will be in less danger of depersonalising others, and slipping back into unwitting bullying and harassment.

::: Give yourself a break. Allow yourself to have an opinion. There's no need to represent it as dissociated impersonal fact,
::: and there's no need to feel frightened or confused or angry about others having different opinions. 
::: Nature is large and each human mind is a tiny candle. None of us really understands very much at all, and that's fine. As long as we cooperate, we can build cities, and sometimes even find our way to happy times and beautiful places.

::: In the meanwhile I'll compose code from pure functions, and annotate in a way that facilitates insight and Rosetta comparison, and you can write the kind of code that you prefer, and that has the qualities which you prefer to pursue. 
::: Others can benefit from both, and judge for themselves what to emulate or avoid. That's their choice – not ours.
::: Happy coding and good luck ! [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:23, 24 February 2019 (UTC)

==A Foolish Consistency is the Hobgoblin of Little Minds==
Pep 8 starts with the following section:

```txt

A Foolish Consistency is the Hobgoblin of Little Minds

A style guide is about consistency. Consistency with this style guide is important. Consistency within a project is more important. Consistency within one module or function is the most important.

However, know when to be inconsistent -- sometimes style guide recommendations just aren't applicable. When in doubt, use your best judgment. Look at other examples and decide what looks best. And don't hesitate to ask!

In particular: do not break backwards compatibility just to comply with this PEP!

Some other good reasons to ignore a particular guideline:

1. When applying the guideline would make the code less readable, even for someone who is used to reading code that follows this PEP.
2. To be consistent with surrounding code that also breaks it (maybe for historic reasons) -- although this is also an opportunity to clean up someone else's mess (in true XP style).
3. Because the code in question predates the introduction of the guideline and there is no other reason to be modifying that code.
4. When the code needs to remain compatible with older versions of Python that don't support the feature recommended by the style guide.


```

The comments in the functional examples are consistent they fit in with other code and look good to me. The functional code has comments which  the imperative does not, in common with most RC code. Hout has applied his best judgement as to which style is appropriate and looks best for his code. Why is there so much bullying criticism of code which is better than most on RC and has certainly had more thought and care taken than most?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:27, 22 February 2019 (UTC)
: Thank you Nigel – I appreciate your kind comments and helpful observations. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:23, 22 February 2019 (UTC)

: Hi Nigel, It's a little more complex in example definition there are many functional examples, a comparison between functional and imperative is not the issue. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:45, 22 February 2019 (UTC)

:: Hi Donald, could you clarify that sentence ? (Perhaps some words are missing ? It doesn't seem to parse as intelligible English).

:: What is your view of the PEP 8 point that project consistency is more important than stylesheet consistency ?

:: Are you aware that your most recent edit (in which you have again deleted my original comments, replacing them with your own compiler type hints, and removed a number of my structural line breaks) has made  the code less readable in more than one dimension, specifically '''against''' the recommendations of PEP 8 ?

:: Do you believe that it is up to '''you''' to decide whether or not the distinction between imperative and functional code is significant to other RC contributors ?

:: Is the gist of your somewhat evasive and (to be honest) unintelligible and syntactically unparseable sentence, that you intend to simply continue your campaign of '''bullying''' – abruptly deleting, or gratuitously reducing the quality of other people's code, even abusing privilege by '''locking down your own edits''', as if aggressively marking territory in the crudest canine tradition ?

:: Do you genuinely equate the quality of a piece of code with the degree of its consistency to your personal interpretation of PEP8 ?  PEP 8 itself '''explicitly''' rejects that approach, as Nigel has reminded us ... 

:: An Etonian accent is no substitute for (or guide to) intelligence or capacity, and 'foolish' guideline consistency is, as PEP 8 takes pains to make clear, no indication of (or lazy substitute for) reliability, readability, ease of refactoring, or general optimisation for particular and specific approaches and goals. 

:: More importantly, '''theologising personal opinion''', treating it as authority to 'smite the wicked' – deleting and vandalising the code of other contributors (who never do this to you), is very bad for Rosetta Code, very bad for you, and very bad for Python. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 04:46, 23 February 2019 (UTC)



### Are type hints a good idea for RC Python examples ?


I am very much in favour of static type-checking, so are the type hints now allowed by Python 3 a good idea for RC Python examples ?

[[User:Paddy3118|Paddy3118]] seems to suggest that they might be – he is not using them in his own examples, but he suggested an enthusiasm for their nativist credentials by deleting all my Hindley Milner type comments for the reader (mathematically based - used in comments across a range of languages and libraries, and in some languages used in the code to be read by the compiler) and replacing them with type hints for the compiler.

The main page now shows a side by side comparison – my original comments for the reader can be compared with the type hints for the compiler. 

A few commments:

: The Python3 compiler tolerates, but '''does not use''', these type hints.

: The RC user could install the Python3 version of '''mypy''' and run a type check which did use them – my own test suggested that perhaps Donald himself had not done that – errors in the type annotations themselves were reported.

: The use of parseable type hints rather than comments introduces a '''larger surface for bugs''':
::Donald's first draft conflated the type of two functions which in fact had quite different type signatures, as had been clearly shown in the type comments which he deleted.
::His 'fix' of that bug introduced another bug, which created a Python3 compiler error (choking on a redundant square bracket)
::Once that compiler error was removed, an actual test with '''mypy''' revealed no errors in the code itself, but found more than one in the type hints (now fixed). To correct these, and to supply hints for functions over (or returning ) lists of items of various types, two additional symbols had to be imported from the typing module.

: For the human reader whose HM comments have been deleted, the parseable type hints to the '''mypy''' checker have a '''poorer signal to noise ratio'''. Cognitively redundant 'Callable' names are multiply inserted where HM needs none, each also bringing 4 additional square brackets. In some instances type names require lower case, in others upper case. The simply polymorphic HM `a` (any type) is replaced by the more verbose '''Any''' (or even '''typing.Any''').

:: For example, given an '''enumFromTo''' function which expands enumerations of any enumerable type, 
:: (e.g. Char or Bool enumerations '''['a' .. 'z']''', '''[False .. True]''' 
:: as well as Int and Float enumerations '''[1.. 10]''', '''[1.5 .. 10.5]'''), 

:: The HM comment: 

:::'''enumFromTo :: Enum a => a -> a -> [a]'''

:: (''A list of values derived from a start value and and end value of the same type'', 
::  ''as long as that type is enumerable'')

:: is more informative to the reader, and less verbose, than the mypy type hint:

::: '''def enumFromTo(m: Any) -> Callable[[Any], List[Any]]:''' 

:: Which is visually noisier, handles currying clumsily, and can not express the restriction to enumerable types.


: Additionally, the scope for Rosetta comparison is diminished – the links between implementations of standard abstractions across different languages are weakened and made less directly visible and comparable. (There are many Haskell, JavaScript, Python and ApplesSript examples on RC which use the same function name, and the same Hindley Milner type comments, for a given mathematically founded abstraction - we can learn a lot about a language, on a fine scale, by comparing these implementations). 

: Increased bug counts for no actual checking by Python3 itself seems an unattractive deal to me. The 'nativist' argument for them (a purer or more Etonian Python accent – why adopt the broadly used Hindley Milner comments when our native Python allows actual type hints ?)  makes no distinction between benefits to the '''reader''' and benefits to the '''compiler'''. 

: It seems a pity, however, to impose something less readable on the reader, when the Python3 compiler:
:: 1) Makes '''no actual use''' of the type hints, i.e. receives no actual benefits at all, but
:: 2) Does throw compiler errors if the hints themselves are syntactically ill-formed, as they were in Donald's second draft.

: My personal verdict is that in languages like Idris, Agda, Haskell, where the compiler does use the type signatures, they are excellent and indispensable, but that in Python3, which doesn't yet use them, they are (at the scale of Rosetta Code examples) a nice idea, but no more in practice than a source of additional bugs, and, if used in place of type comments, just make things less clear and more difficult for the reader. 
: Even Donald was clearly confused by them, assigning wrong types, and introducing (in what he presumably envisioned as an 'improvement'), both compiler and mypy errors ...
: On the scale of much larger projects, and as a complement to comments, rather than a substitute for them, the use of the '''mypy''' and the type hints which it can read, will doubtless begin to pay off the cost of the expanded bug surface which it initially introduces.

: For the small scale of Rosetta Code examples however - nice idea, but perhaps doesn't yet survive experiment, or actually benefit RC readers ? Maybe when Python4 comes ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:55, 24 February 2019 (UTC)

:: Donald [[User:Paddy3118|Paddy3118]]. Do feel free to introduce type hints into your own code - but please desist from deleting my type comments and replacing them with these hints. They damage the legibility of the code, demonstrably enlarge the surface for bugs, and in Python3, offer no benefit whatsoever to the compiler.
:: Not easy to think of a better example of the 'foolish consistency' against which PEP8 prominently warns on its opening page.  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 09:02, 24 February 2019 (UTC)

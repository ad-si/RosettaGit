+++
title = "Talk:Cheryl's Birthday"
description = ""
date = 2018-10-28T18:17:31Z
aliases = []
[extra]
id = 22044
[taxonomies]
categories = []
tags = []
+++

==Improve tag on Python entry==
I added an improve tag on the Python entry as it does not look or read like idiomatic Python. To avoid an edit war I will state my reasons:

# Commented type annotations read like those from a different programming language.
# Use of map/filter over comprehensions.
# Use of trivial functions: fst, id, _not, snd, swap
# Overwriting a builtin name: id
# main() without `if __name__ == "__main__"`
# Shares structure and naming with examples from other languages.

It reads like a translation from another language and RC is about idiomatic code. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:14, 26 October 2018 (UTC)

: I have removed that obdurately tendentious 'improvement' tag, and will suggest a practical solution. The 3 points are these:

# Functional composition, structured around immutable references, is an important and distinct current of coding architecture, with strong advantages for some contexts, and needed by significant numbers of those who come to Python principally for its libraries. The former BDFL, now outgrown by the community, was (1). viscerally and declaredly hostile to the use of this mode in Python. (2). Failed, despite his most strenuous efforts, to persuade the Python development community that he was being constructive on this issue. See Guido's failed attempt to remove 'reduce', and his intemperate relegation of it to 'functools', while describing this as 'a dumping ground for things that don't interest me'.
# The former BDFL's visceral but unreasoned ('reminds me of lemon juice' etc etc) hostility to the composition of pure functions, and the disentanglement of value definition from IO, leaves the notion of 'Pythonic' *at best* limping on a pair of very uneven legs, with one (imperative) leg fairly muscular, and the other etiolated and deliberately shackled. In short, 'Pythonic' goals, well-defined and helpful for imperative coding, are quite '''deliberately''' bad, and completely dysfunctional, for functional code. This is a problem. Not an aspirational ideal.
# While 'Pythonic' constraints are good only for imperative code, mathematical constraints are deeply and inevitably structuring in the composition of pure functions. Research on, and exposition of, these constraints, and of their mathematical underpinning, can certainly use language-independent notations, (See the classic Bird and Wadler 1988, for example) but tends, in practice, to use Haskell/ML notation, and to adopt generic function names from the ML tradition (used even in Bird and Wadler), which serve as a kind of lingua franca for functional programming. The function names (and style of type annotations) used in Haskell (which is the main vehicle of the academic research) are also commonly used in projects aiming to strengthen the functional use of other languages. This is true not only of projects in other languages (see for example Ramda and other JS functional libraries) but '''even of Python itself'''.  When Python library developers need names for functional abstractions, they '''also''', like others, naturally turn to the norms established by the ML/Haskell tradition. Haskell and Standard ML (SML), together with APL, are indeed '''explicitly mentioned''' at the top of the '''itertools''' documentation page. Hence we see in Python not only the standard functional terms like '''map''' and '''filter''', but also '''dropwhile, takewhile, groupby, cycle, repeat''', etc etc. In Python, as in other languages, when you need to strengthen functional muscle, established best practice is to turn to the Haskell/ML abstractions and names, which provide the widely shared lingua franca of the research.


The particular points of visceral resistance which you list above arise, almost without exception, from the difference between imperative and functional code, and from the scar tissue of Guido's now overturned dictatorship, which deliberately made 'Pythonic' guidelines incompatible with best functional practice.

# List comprehensions are, mathematically, '''concat . map''' (or '''concatMap''') over functions returning list-wrapped values. This is functionally distinct from bare map and filter. Ironically '''all''' of these, are, in the underlying math of function composition, specialised variants of fold/reduce, which Guido didn't fully understand ('I need pencil and paper' etc) and which the community as a whole did, and therefore would not let him ban.
# Raising trivial operators to first class functions is absolutely correct, and indeed logically inevitable, when the former are passed as arguments to higher-order functions. That is '''precisely''' why Python has an '''operator''' library.
# 'Shares structure and naming with examples from other languages' Of course !!! Functional solutions inevitably (and very helpfully) resemble each other. They are structured by the same mathematical constraints on function composition and immutable names. As for using standard Haskell/SML names for standard abstractions, that is already a well-established and explicitly documented best practice in Python. (See the itertools documentation page). In the context of a Rosetta project, this is particularly helpful. The shared names facilitate cross-reference, and the differing implementations of the same abstraction are instructive.


'''Suggested compromise'''. Given that the Guidonic/Pythonic patterns are in many cases deliberately and explicitly incompatible with good functional practice, I would propose that I explicitly label my contributions as 'Functional', distinguishing them from any neighbouring imperative variants.

(Your comment on id() is helpful, I can't remember why I explicitly defined one, and I am very happy to take a look at that. 
`if __name__ == "__main__"` I just happen to find rather noisy and ugly, and a bit raucously redundant in this context, but I'll also look at that, in the interests of better functional/imperative relations :-) 

Guido really was a bit naughty and deliberately destructive on the issue of composing pure functions. You can get some benefits from a BDFL, particularly in the early stages of a project, but in his case you paid a real, and not unnoticed price as well. That's why the PEP processes became unsustainably painful for him. Time to heal and remedy that now, if the language is to thrive, become better optimised for rapid and reliable coding. 

Composition of pure functions is not going to go away. Time, math, and multiprocessors are all on its side. As for 'idiomatic', with functional code, that maps not to the deliberately functional-dsyfunctional 'Pythonic' verities, but to the standard use of standard functional abstractions, with names taken, by default, from the very same idiom as Python's takewhile, dropwhile and groupby etc [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:16, 26 October 2018 (UTC)

: PS Imperative coding seldom works well in my context, tho I sometimes use it at a latish stage if I happen to need to trade away some reliability to gain a bit of compression (a bit like JPEG in lieu of PNG), but I have absolutely no objection at all to others working with it. Rather than slapping banning orders on functional drafts that you fear might differ from the ex-BDFL's very quirky and personal preferences, why don't you just add an imperative/'Pythonic' variant of your own ? A much more interesting and constructive form of criticism or commentary. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:23, 26 October 2018 (UTC)
:: Hi Hout, have you tried pythonnet ..."Python for .NET ( pythonnet ) is a package that gives Python programmers nearly seamless integration with the .NET 4.0+ Common Language Runtime (CLR)"... Aggregate -> reduce; Where -> filter; Select -> map


```txt

>>>clr.ImportExtensions(System.Linq)
>>>dir (List)
['Add', 'AddRange', 'Aggregate', 'All', 'Any', 'AsEnumerable', 'AsParallel', 'AsQueryable', 'AsReadOnly', 'Average', 'Bi
narySearch', 'Capacity', 'Cast', 'Clear', 'Concat', 'Contains', 'ConvertAll', 'CopyTo', 'Count', 'DefaultIfEmpty', 'Dist
inct', 'ElementAt', 'ElementAtOrDefault', 'Enumerator', 'Equals', 'Except', 'Exists', 'Find', 'FindAll', 'FindIndex', 'F
indLast', 'FindLastIndex', 'First', 'FirstOrDefault', 'ForEach', 'GetEnumerator', 'GetHashCode', 'GetRange', 'GetType',
'GroupBy', 'GroupJoin', 'IndexOf', 'Insert', 'InsertRange', 'Intersect', 'IsReadOnly', 'IsSynchronized', 'Item', 'Join',
 'Last', 'LastIndexOf', 'LastOrDefault', 'LongCount', 'Max', 'MemberwiseClone', 'Min', 'OfType', 'OrderBy', 'OrderByDesc
ending', 'ReferenceEquals', 'Remove', 'RemoveAll', 'RemoveAt', 'RemoveRange', 'Reverse', 'Select', 'SelectMany', 'Sequen
ceEqual', 'Single', 'SingleOrDefault', 'Skip', 'SkipWhile', 'Sort', 'Sum', 'SyncRoot', 'Take', 'TakeWhile', 'ToArray', '
ToDictionary', 'ToList', 'ToLookup', 'ToString', 'TrimExcess', 'TrueForAll', 'Union', 'Where', 'Zip', '__add__', '__clas
s__', '__contains__', '__delattr__', '__doc__', '__format__', '__getattribute__', '__getitem__', '__hash__', '__init__',
 '__iter__', '__len__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__setitem__', '__sizeof__'
, '__str__', '__subclasshook__']

```

or The Python Data Analysis Library with which after establishing dates as a data frame you could write something like dates.groupby('month').count()--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:39, 26 October 2018 (UTC)
: Thanks Nigel – I'll take a look. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:57, 26 October 2018 (UTC)

Hi Hout: You obviousely don't like the way Python evolved under Guido's leadership. That is plain. If you you don't like the PEP-8 guidelines then I suggest you get them changed to your liking , in the proper forums. If enough of the community under the new GUIDO are convinced then you will get your way.
 
One could take your criticism/diatribe on the support for functional programming and substitute another programming style; and no doubt there will be OO programmers lamenting the "broken" support for OO in Python with similar righteous indignation. 

Python is what it is. RC isn't the place to change it. One could try to write Haskell in Python, but that would not make it idiomatic Python. Yet? :-)
:[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:19, 26 October 2018 (UTC)
:: The way to handle the 'idiomatic' issue is with tooling – all of the code I have published here is checked with AutoPep8 and Linter-flake8. Feel free to suggest any further linters which you think might be helpful.

:: I am not trying to change anything – you are the one attempting to censor the code of others.

:: The 'Pythonic/imperative' coding style which you like was a particular optimisation for (1.) Easily accessible Python (imperative coding creates more run-time complexity and costs more debugging time, but it has a lower entry level, requiring a grasp only of sequence branching and iteration.) and (2.) Some very private (anti 'lemon-juice', 'things that interest me') aesthetics (and one formal misapprehension) of Guido, for which he failed to establish sufficient consensus to get his own way, and which eventually contributed more to problems than to solutions, making his BDFL role unsustainable, and bringing it to an end.  

:: Python is now much larger than the initial 'easy-coding-for-all' personal project, and while the simple 'Pythonic' verities are still a useful source of consistency in some contexts, they are deliberately dysfunctional, by specific design, in others. They can usefully standardise imperative and impure code, but can only lead to intentionally bad functional code.

:: Functional composition is, in fact, and fortunately, simply one of the ways in which Python interpreters are actually used. That is why '''map''', '''filter''' and '''reduce''' entered the language, and that is why Guido simply failed, in the end, to expunge them, despite his strongest and most truculent efforts. Composition of pure functions has a higher entry barrier – it requires more concepts – but it yields more reliability, reduced debugging time, and greater code reuse. It is absolutely the right way to write good Python for some contexts, and for some users, and there is no value whatsoever in trying to shoe-horn real Python code (officiously, and frankly, somewhat ignorantly) into the residual shackles and deficiencies which Guido rather perversely and self-absorbedly sought to put in the way of composing pure functions, against the grain of the needs and better judgement of the Python community as a whole. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:56, 26 October 2018 (UTC)

==Type hints for the compiler are not an alternative to semantic type comments for the reader==

Paddy's addition of type hints is a very constructive and interesting idea, and useful spur to experimenting with the mypy type checker, which I have been wanting to do. His deletion of the informal Hindley-Milner style type comments from Python draft, however, probably arises from a misunderstanding, and simply deprives the reader of clarity about the semantics denoted by the function. 

For example, it doesn't help the reader at all to replace:
'''uniquePairing :: DatePart -> [(Month, Day)] -> [(Month, Day)]''' with a bald  '''(int -> Callable)'''

The type hints for the compiler, and the informal Hindley Milner type signature comments for the human reader serve two entirely different purposes, and are not at all in tension with each other. As the useful notes on this JS project point out https://github.com/ramda/ramda/wiki/Type-Signatures comments/annotation of this kind have become a kind of language-independent standard in functional programming generally. In some projects, like Purescript, they do have a role in compilation as well as providing clarity for the reader, but in other projects, like Ramda, they are entirely for the reader, and simply serve to summarise the semantics of the function in a brief and relatively standardized way. No need to deprive the reader of them simply on grounds of tribal shibboleth zealotry, border patrolling, or heresiology. A comment is just a comment. :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:05, 26 October 2018 (UTC)
: Python type hints are also more than those I auto-generated; as is the use of docstrings. Try writing something like idiomatic Functional Python; with its none-Haskell peculiarities rather than believing such things are beneath you. Python has an existing set of functions that the community has expended the effort to learn and/or expect to learn. If you convert and create your own set of functions then readers cannot use that Python knowledge they have. Just as I don't see you using Lisps car and cadr you've probably decided to use your own names for things that were named before. It reads as if you have your own set of functions and know how to use them to solve problems; as well as how to define them in terms of Pythons functions - a translation of sorts. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:33, 28 October 2018 (UTC)

:: Paddy or Donald, the only intervention I have ever made in your code is to point out that your pythonic example for a new task that you were offering (Cosine laws, I think) generated the wrong result. Even then, I aimed for extreme tact and discretion, and simply offered an alternative draft in functional Python which produced the correct result – politely (I hope) drawing your attention, without comment, to the 'mystery' of the divergence.
:: I have no interest '''at all''' in changing your style of Python – please now desist from trying to change mine.
:: The Python community is large and diverse, does not always agree, and does not need to.
:: I respectfully suggest that if we do differ, we express those differences by submitting alternative solutions, with differing approaches, to the same problem, thereby enriching Rosetta Code rather than simply making making it noisier and more acrimonious.  
:: That is more than enough now. Your persistent and unremitting attentions and intrusions now feel persecutory. Please move on. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:11, 28 October 2018 (UTC)

== Can we turn down the heat somewhat? ==

I would prefer if discussions here would focus on the programming issues instead of on tangential issues such as the people expressing the viewpoints.

We all have different viewpoints - this is a necessary condition of life - but I think (on rosettacode) we'd all be better served if we could focus on the code itself.

Mind you, I also understand that coding standards can achieve religious significance for some people. But, there, it's probably better to link to relevant documentation of the standards than it is to try to recap those standards here.

We're not going to achieve perfection here. But we've got bigger problems to be dealing with than differing perspectives. (For example: all too often, code from here doesn't work, either because of copy/paste issues, version drift, unexplained and unknown assumptions or other similar details which might have escaped the author's notice).

(Hopefully the way I've said this here isn't too terse or too off-base to express the concept?)

Thanks. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:08, 26 October 2018 (UTC)
:: Absolutely agree. Code that actually works, and has been through standard linters, seems more than enough. I can find no value in insisting that it is also optimised exclusively for narrowly defined contexts or cultural currents. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:12, 26 October 2018 (UTC)

:: Oh dear – deletionary zeal is back again this morning ... See under mcNuggets Talk. Sigh ... [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:19, 27 October 2018 (UTC)

:: Good heavens ... the appetite for attrition seems strong ... further notices, and now a whole discussion added to my talk page. I suggest that we just (1) agree that PEP8 linters are a good idea (I have used them throughout) (2) accept that functional Python, about which there are least two books (Merz – O'Reilly Press, and Lott), several book chapters, and plenty of on-line use and discussion, is distinct from the excellent (but deliberately imperative) 'pythonic' subset of the language. Let's just divide the Python contributions into 'Pythonic' and 'Functional' subheadings. The spirit of Rosetta is to be inspired more by what is shared than by what is different. Let's just move on [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:18, 28 October 2018 (UTC)

:: To be honest, I begin to feel a little harassed. Surely that can't be the intention ? I am not ''quite'' sure what is going on ... [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:40, 28 October 2018 (UTC)

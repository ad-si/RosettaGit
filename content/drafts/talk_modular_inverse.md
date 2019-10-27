+++
title = "Talk:Modular inverse"
description = ""
date = 2018-10-25T22:29:54Z
aliases = []
[extra]
id = 16712
[taxonomies]
categories = []
tags = []
+++

== Handle a < 0 ? ==
The problem is perfectly well defined for a < 0, but many of the implementations (e.g. the C version and translations thereof) do not handle this case correctly.  Is this a bug in the implementations or in the problem specification.

[[User:Stevengj|— Steven G. Johnson]] ([[User talk:Stevengj|talk]]) 16:38, 14 November 2013 (UTC)

:It seems to be neither Steven. You don't have to consider a < 0 to answer the example given in the task description. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:45, 14 November 2013 (UTC)


==Formulae hidden to most browsers by under-tested cosmetic edits at 19:16, 15 July 2016 ==

Under-tested cosmetic edits made to the task page at 19:16, 15 July 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:17, 22 September 2016 (UTC)

==Removed Paddy's message requiring normalisation to 'good Python style'==

As with any other language, what constitutes 'good' in Python (beyond correct results) is entirely a function of what we are optimising for. The alternative draft which Paddy would like to censor optimises for reliability and high code reuse. 'Good' Python is *not* coextensive with the 'Pythonic' or 'Guidonic' style of Python's BDFL period – now past and outgrown. As we recently saw in Paddy's example draft for a new task, it is very easy to write highly 'Pythonic' Python which is clearly 'bad', in the simple sense that it returns an incorrect result. 

Python is already a very useful language, now dwarfed in its scale, as a project, by the extraordinary number of excellent libraries which have been built around it, and which now constitute a major public resource. As a self-declaredly 'multi-paradigm' language, the core of which did gain some useful coherence from the BDFL period, Python nevertheless retains a severe limp and some real scar tissue from that period. Guido's energies, and the quality of his analysis, were very unevenly divided between the imperative and functional modes of the multi-paradigm spectrum. 

Detailed and technically resourceful on 'for loops', his reasoning on functional code consisted of slightly somatic and regressive references to lemon juice and 'things that don't interest me'. In explaining his failed attempt to remove 'reduce' from the language, against the grain of the Python community's needs, he appealed, rather petulantly, to his own personal need to use pencil and paper when thinking reductions through – he had clearly never taken the time to get his head around the notion of a fold – a deeply universal and expressive structure which underlies most computation. 

The badness (failure to produce a correct result) of Paddy's scrupulously 'Pythonic' example for his recent new task, arose *precisely* because it was written *within* the Pythonic/Guidonic imperative tradition, with all the excessive complexity of undermodelled states which value mutations and indisciplined loop breaks are heir to. 

Now that Python has matured, and outgrown its BDFL infancy, that scar tissue can begin to heal (`reduce` will in time emerge from the purdah of `functools`, to which the dictator truculently relegated it when the Python developer community would not let him trash it) and the huge community of Python library users can begin to be better served by a genuinely multi-paradigm and less damagingly limping corpus of Python use. 

Functional code optimises more for correctness, speed of assembly, degree of code reuse, and ease of refactoring, than for compression of time and space. That may well make it 'bad' for some contexts and projects, but it also makes it excellent, and clearly preferable, for others. 'Pythonic' and good are *very* far from being natural synonyms, and there is room on Rosetta Code for Python code which explores both main areas of the multi-paradigm space, and is optimised for various different contexts. 'Good' code above all returns correct results, and is well optimised for a particular organisational context. Bad code, and incorrect results, are not *in any way* redeemed by being loyally 'Pythonic' or 'Guidonic'. That mould has now broken. The young Python has broken its shell and emerged from its egg. A cause for celebration. Don't try to sellotape it back into the fragments.  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:21, 25 October 2018 (UTC)

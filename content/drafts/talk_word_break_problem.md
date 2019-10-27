+++
title = "Talk:Word break problem"
description = ""
date = 2019-05-20T02:02:33Z
aliases = []
[extra]
id = 21506
[taxonomies]
categories = []
tags = []
+++

==Conjugations==
In my experiments with this, the first thing I missed was a dictionary with conjugations, plurals, past participles, etc. For instance separate would be there but there would be no separate entry for separated. I also didn't know that unixdict.txt has 'b'..'z' as separate words, and no jabberwocky... Otherwise the Phix entry could have been quite practical for throwing a 48,000+ word dictionary at a paragraph, but probably not a whole book-full, of deciphered (and de-spaced) text. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 11:41, 1 July 2017 (UTC)

==What is the task again?==
I don't understand from the task description, what is to be done.

* What is the necessary format of this string? Is it white-space, alphanumeric, and other for example?
* Are words a-zA-Z characters only
* What is to happen to numerics? other characters in the string that are not words?

Maybe the task description needs filling out? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:34, 18 August 2017 (UTC)

==lex==
Highlighting the goal to use the complete sentence would have helped me.  The "right" tool for the problem where many parsings are possible is the experimental version of bison which branches at alternatives, keeping those that resolve without error.


lex:
How the input is matched

BASIC PRINCIPLES.

(1)
When the generated scanner is run, it analyses its input string looking for strings which match any of its patterns.
(2)
If the current input can be matched by several expressions, then the ambiguity is resolved by the following rules.
(2.1)
The longest match is preferred.
(2.2)
The rule given first is preferred.
(3)
Once the match is determined,
(3.1)
the text corresponding to it is available in the global character pointer yytext its length is yyleng and the current line number is yylineno,
(3.2)
and the action corresponding to the matched pattern is then executed,
(3.3)
and then the remaining input is scanned for another match.
--LambertDW 01:52, 20 May 2019 (UTC)

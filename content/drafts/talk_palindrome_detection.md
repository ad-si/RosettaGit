+++
title = "Talk:Palindrome detection"
description = ""
date = 2017-03-28T21:43:00Z
aliases = []
[extra]
id = 3210
[taxonomies]
categories = []
tags = []
+++

==Spaces and punctuation==
The task specification states both:"The function must not ignore spaces and punctuations" and later: 
"you must make it all the same case and strip spaces".

So ignore or strip spaces - what should be done? --[[User:Paddy3118|Paddy3118]] 20:56, 5 December 2008 (UTC)

: I think what is meant is that the function itself shouldn't ignore spaces or punctuations, but if using the given example, the test code must remove spaces and convert to either uppercase or lowercase, because otherwise the example isn't a character-wise palindrome. I think it would make more sense to use some example which already is a character-wise palindrome by itself, say the German word "nennen" (to name). --[[User:Ce|Ce]] 09:42, 6 December 2008 (UTC)

:: It is what I've meant... As example of palindrome I used "In girum imus ..." just because it is the longest I know by heart: the "ignore or strip spaces" refers to that example as I wrote there; looking at the code it is clear what I've meant. According to me a function that tests the palindromicity of a "arbitrary" sequence is more general than one that "ignores" by design some characters; you can use the function you wrote to test palindromicity on "In girum imus nocte ...", provided that you strip spaces and make the case of all letters the same... outside the function, e.g. is_palindrome(stripchars(string)), and write stripchars according to any need. In the codes, I did it by hand coding the test string as "ingirumimusnocte...", that is less readable, but in this way I had not to write a stripchars function just to test! [[User:ShinTakezou|ShinTakezou]] 00:44, 7 December 2008 (UTC)

:: And hey, my english is bad, but not so bad: I've forgot it, but I wrote ''To do your test with it, you must make it all the same case and strip spaces.'', where the "it" refers to the "In girum imus..." palindrome, and ''to do your test with it'' means ... :D --[[User:ShinTakezou|ShinTakezou]] 00:44, 7 December 2008 (UTC)

::: Hi ShinTakezou, please accept my apologies if you thought I was gratuitously slighting your English - that was not my intention. I was merely pointing out what appeared odd to me in reading your task. I could figure out what is needed but was hoping for the task description to be clarified to remove any doubt. When a task accumulates several implementations it can be difficult to work out which is the first implementation so if implementations differ, it is hard to refer to the first implementation for a definitive answer. --[[User:Paddy3118|Paddy3118]] 01:12, 7 December 2008 (UTC)

:::: Got your point. Chances there are that my ''main'' contribute is in the C language :) --[[User:ShinTakezou|ShinTakezou]] 00:25, 8 December 2008 (UTC)

::: I'm afraid I have to disagree with ShinTakezou on this. The normal, non-computational definition of a palindrome is a string that reads the same forwards and backwards while ignoring spaces, case and punctuation. So the computational definition should be the same for this task. Especially since stripping non-word chars is a common subtask. If the input has to be massaged manually for a checker to work, it's not much of a checker. :-) And checking only single-word strings is really just a degenerate case. --[[User:Snoman|Snoman]] 00:43, 13 July 2010 (UTC)

:::: Since "non-computational" systems like human brain can recognize palindromicity in many different context, and humans do not read strings, but sentences/words, and blanks/punctuations are "naturally" ignored (and difference between upper and lowercase), being just a way to make it easier to recognize words and read sentences. Computational systems can identify as "naturally different" "A" and "a", and spaces/tabs/puncts are not just blank or "spurious" symbols to help reading, they are existing real symbols for the system. A general "computational" palindromicity test can operate on any symbol a computer can recognize as such (even on "unreadable" sequences of bytes, provided you avoid '\0' in C and similar langs where a string is '\0' terminated). To go back and forth to the "non-computational" "sight", it is enough to add/remove stripblanks, strippunct, lowercase/uppercase and other transformation able to make computer ignores the difference human "naturally" can ignore. But it is not necessary, and the general palindromicity approach is well, more general, and so, better since can be used '''also''' for your "non-computational" definition. --[[User:ShinTakezou|ShinTakezou]] 06:17, 13 July 2010 (UTC)

::::: Sure, understood. I think people are just pointing out that the task description was not as clear to some of us as you wanted it to be, partly because the term "palindrome" is not actually defined in the task. The linked Wikipedia article focuses mostly on natural language palindromes, as does the Latin example. As a linguist myself, I admit I find those more interesting than formal strings, and both are valid. For your more restrictive and computationally general idea, maybe specifying a formal mathematical statement of a palindromic sequence would have helped. In any case, the one type of solution is convertible to the other with a little extra string handling, so it's not a huge deal. And thanks for the explanations of your POV. --[[User:Snoman|Snoman]] 08:39, 13 July 2010 (UTC)
::::::: The wikipedia's article was linked just to clarify the sense of  palindromicity, but my initial wording should make it clear that the task was thought with "computer vision" in mind: <cite>check if a '''sequence of characters (or bytes)''' is a palindrome or not</cite>; <cite>It is not mandatory to handle properly encodings</cite>; <cite>The function must not ignore spaces and punctuations</cite>; then, for the given example, I specified <cite>To do your test with it, you must make it all the same case and strip spaces.</cite>. Anyway, if people are happier with other defintions, it's ok to change the task description, as far as the rewriting does not "invalidate" all the codes (or people should rewrite the codes too for task compliance) --[[User:ShinTakezou|ShinTakezou]] 18:13, 13 July 2010 (UTC)   

:::::: I would recommend a rephrasing:
:::::: "This task requires a modular design:  You must implement a function which tests if an arbitrary sequence of characters or bytes is a palindrome.  You must also implement a wrapper which normalizes text, removing spaces, case differences, and punctuation." --[[User:Rdm|Rdm]] 14:21, 13 July 2010 (UTC)
::::::: If people agree, go for it. I don't see my sentences ambiguous anyway. --[[User:ShinTakezou|ShinTakezou]] 18:13, 13 July 2010 (UTC)
:::::::: I do not think your sentences were ambiguous, either.  They do, however, allow for a paradoxical interpretation, from some people's points of view.  And its easy enough to change.  --[[Special:Contributions/159.54.131.7|159.54.131.7]] 18:34, 13 July 2010 (UTC)

----

Looking at the Perl, I added case/space/punctuation handling to the first 3, an easy 1-line change to each that doesn't change the existing functionality. Also corrected the spelling of "palendrome" in the regex version. :-) --[[User:Snoman|Snoman]] 00:43, 13 July 2010 (UTC)

==Haskell recursive solution note==

I suppose the Haskell recursive code can be written a lot better, but I don't know how. --[[User:ShinTakezou|ShinTakezou]] 14:08, 5 December 2008 (UTC)
: Does this look better to you?

```txt

is_palindrome_r x | length x <= 1 = True
                  | head x == last x = is_palindrome_r . tail. init $ x
                  | otherwise = False

```

:--[[User:Gaaijz|Gaaijz]] 14:37, 5 December 2008 (UTC)
::Yes, if it works, why don't you put it in? I've tested it, it works, put it in instead of mine ;) --[[User:ShinTakezou|ShinTakezou]] 19:09, 5 December 2008 (UTC)

The Python recursive example isn't testing that the string is a palindrome. It seems to actually be a test for whether the object tested supports len() and slicing. [[User:Drea|Drea]] 16:56, 5 December 2008 (UTC)
::Why? I am still learning Python... even though there's no type check (right?) I suppose the normal use of such a function is on strings or arrays, both supporting len and slicing; maybe it makes sense to be able to test palindromicity also on other objects. Anyway it worked rather well when tested ;) --[[User:ShinTakezou|ShinTakezou]] 19:09, 5 December 2008 (UTC)
:It looks like the Perl example doesn't check for character equality either. --[[User:Mwn3d|Mwn3d]] 16:59, 5 December 2008 (UTC)
::Oh my ... yes, thanks (the same as Python... can't remember if I simply forgot or were cut... fixing --[[User:ShinTakezou|ShinTakezou]] 19:09, 5 December 2008 (UTC)
::I've fixed the Python example. I don't know any Perl though, so I'll leave that for someone else. [[User:Drea|Drea]] 17:03, 5 December 2008 (UTC)
:::Oh my ... yes, thanks :) --[[User:ShinTakezou|ShinTakezou]] 19:09, 5 December 2008 (UTC)
::::No problem :o) [[User:Drea|Drea]] 19:29, 5 December 2008 (UTC)

== Prolog ==

I've tested it with gprolog... it does not work... nor I know exactly what is not working; but it seems quite interesting; I will spend a little bit of time taking a look at prolog (there's something that reminds me erlang, maybe <code>consult/1</code> or notation like this...)
:I think the problem is with the string functions I used. I can't figure out how to get the right library loaded. I found the functions [http://www.fraber.de/bap/bap74.html here]...can anyone help? --[[User:Mwn3d|Mwn3d]] 03:32, 10 February 2009 (UTC)
I've tried to run several examples of prolog, not always with success. There's something still obscure (beyond the language itself, that seems not suitable for some kind of ''out-of-logic'' task; it should share something with functional programming, but I've failed using my poor knowledge of e.g. Haskell; nor I've found well-done full manuals, and tutorials stress the ''logic'' ability of the language...) --[[User:ShinTakezou|ShinTakezou]] 15:37, 10 February 2009 (UTC)

Found this: [http://books.google.com/books?id=w-XjuvpOrjMC&pg=PA359&lpg=PA359&dq=prolog+palindrome&source=bl&ots=4WA-WMI-St&sig=Yo6gIu8mDMpwPdOygiEZC_EQS04&hl=en&ei=zZ2RSbSEK8yE_gadgLWrDA&sa=X&oi=book_result&resnum=7&ct=result palindrome?], but likely it is not suitable for the wiki because of the license... Despite this, I am not sure it works... when I feed it with a palindrome string or list, it "asks" me True? :D

```txt
| ?- pal([a,b,c]).

no
| ?- pal([a,b,c,b,a]).

true ? 

yes
```

Like if the clause were undetermined (and in fact pressing ; for the next solution, gives no, not yes!)... maybe I should read deeper the page... --[[User:ShinTakezou|ShinTakezou]] 15:50, 10 February 2009 (UTC)
:I don't think this solution is very good anyway because it uses a list of characters rather than a string. I'd rather see one using string library functions. --[[User:Mwn3d|Mwn3d]] 15:54, 10 February 2009 (UTC)
The functions you used for strings are in SWI Prolog, but not in GNU Prolog, so I can't test it, but the "logic" seems logical to me:D --[[User:ShinTakezou|ShinTakezou]] 21:26, 10 February 2009 (UTC)

Reading here and there I've realized that what we call String in Prolog could be simply called atom, and it exists atom_length/2 and atom_concat/3; atom_concat(A1,A2,A3) is true if A3 (unbound atom) is the concatenation of A1 and A2 (bound atoms, i.e. known atoms?).

The code compiles (with gplc, the GNU prolog compiler), but when I try pali('atom'), it says


```txt
uncaught exception: error(instantiation_error,atom_concat/3)
```


I still have to understand the debug part...; by guessing, I've thought the problem is that when trying the first atom_concat, the unbound atoms are three: X, Mid, and Str2; then I've tried to swap the two atom_concat, since at least the second "knows" Str, then bounds the other two and it makes sense for the second atom_concat the use of X and Str2... Done so, pali('hello') said "no"... but pali('aabbaa') asks True? meaning it can't determine if it is true or false... still needing work... Of course (?) pali('a') says yes because of the second rule. --[[User:ShinTakezou|ShinTakezou]] 22:02, 10 February 2009 (UTC)


### =What I was able to do until now with Prolog=


I sticked to use <code>sub_atom/5</code> to get Head, Tail (one ''char'') and Middle, and produced this code:


```prolog
pali(Str) :- atom_length(Str, Len), pali(Str, Len).
pali(Str, L) :- L>1, Inlen is L - 2, WLen is L - 1, sub_atom(Str, 1, Inlen, 1, Mid),
	sub_atom(Str, 0, 1, WLen, AR), sub_atom(Str, WLen, 1, 0, AL),
	AL == AR, pali(Mid, Inlen).
pali(_, 1).
pali(_, 0).
```


Does it work? Yes... and no. The first oddness is about the last two lines, but I'll write about it later. After I ''consulted'' this code, I was able to do queries, and I did. If I enter a non palindrome sequence, it says no. This is fine and promising. But if I enter a palindrome sequence, it asks True? ... So, it seems we are at the same point as before.

The sad fact is the the code for pali(Str, L) was tested interactively and built piece by piece, and each stage seemed to work, and the whole worked too... But I have not found a way to insert and test rules from the interpreter... it seems like it is able just to do queries or clauses...

Anyway, if I run


```txt

Str = 'abba', atom_length(Str, L), Inlen is L-2, WLen is L-1,
   sub_atom(Str,1,Inlen,1,Mid), sub_atom(Str,0,1,WLen,AR),
   sub_atom(Str,WLen,1,0,AL), AL==AR.

```


I obtain as output


```txt

AL = a
AR = a
Inlen = 2
L = 4
Mid = bb
Str = abba
WLen = 3

yes

```


The interactive session just tested that AL and AR are the same; now it would logical to put altogether into a <code>pali(Str, L) :-</code> appending the ''call'' <code>pali(Mid, Inlen)</code>, and I've imagined it should work... But it wasn't able to handle things like pali(<nowiki>''</nowiki>) or pali('a'), because of L-2, so added the check L&gt;1. '''And''', because of some experiments, I believe that the "code" must be differentiated by specifying another argument. The pali/1 is the "access", that "triggers" the ''right'' version of pali/2 according to the second argument.

But as said, when it is put into a rules, the code seems to be not able to determine if a word is palindrome, but only if it is not (and this, altogether with the tests made from the command line, is non-sense!).

Now, the oddness about last two lines. If I put them before pali(Str, L), then pali(<nowiki>''</nowiki>) and pali('a') ask for Truth. If I put as the code shown above, pali(<nowiki>''</nowiki>) return yes (that is ok), but pali('X') asks for truth (as pali('string with more than one char')). If I swap the two lines, it happens the opposite! pali('a') return yes, pali(<nowiki>''</nowiki>) asks for truth!

So order matters, because of a hierarchy of clauses that I tried to understand, but it is too late to be enlightened. Trying blindly other "combinations" brought the same (less or more) results.

So now I am convinced I can't do it, and I'm wondering if it exists somewhere a Prolog Guru able to enlighten my poor logic at pro'''log'''-'''ic'''. --[[User:ShinTakezou|ShinTakezou]] 01:24, 11 February 2009 (UTC)



### =Better reading of the GNU Prolog manual=


They worked... The meaning of True when feeded with a palindrome was that one (and only) solution is true, while the ? just asked for the search of the next solution; the ''no'' that is output thereafter, means there are no more solutions! When it is not a palindrome, it simply says that there are not solution (no), it does not mean that the "predicate" returned false... it means that the problem can't have a solution (i.e., the word is not palindrome). --[[User:ShinTakezou|ShinTakezou]] 14:32, 11 February 2009 (UTC)

== REXX ==

why was "remove" changed to "rename"?? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 04:26, 18 May 2013 (UTC)

I had a short-lived brilliant idea to remove 1/3 of the subroutine, but it didn't work, so I reinstated the original statement, but I re-typed the comment incorrectly. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:41, 18 May 2013 (UTC)

== COMPRESS in REXX ==

What is compress? Which Rexx has it??
Would space(...,0) do the job??? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:40, 14 September 2015 (UTC)

: The '''compress''' BIF is supported by the ARexx REXX interpreter, as well as Regina when the appropriate '''options''' are specified, and also in Regina when Regina is executing on (or an equivalent) Commodore Amiga 1000 (A1000), known commonly as the Amiga.   The '''compress''' BIF is documented in the Regina (PDF) documentation.   I can't speak to any other REXXes that may support that BIF.   As far as I know, '''compress''' is equivalent to '''space(xxx,0)''', but I don't know how it treats whitespace other than blanks.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:54, 14 September 2015 (UTC)

:: In Regina doc I read that all blanks are removed. So does SPACE(xxx,0) as far as I know.
Does ARexx >not< support space(xxx,0)? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:41, 14 September 2015 (UTC)

::: ARexx does support   '''space(xxx,0)'''.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:31, 3 March 2016 (UTC)

:::: But Compress() is more versatile. -- [[User:Idrougge|Idrougge]] ([[User talk:Idrougge|talk]]) 09:56, 22 March 2017 (UTC)

::::: That may be true, but the   '''compress'''   BIF isn't supported by most of the REXX interpreters available.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:42, 28 March 2017 (UTC)

+++
title = "Talk:Flatten a list"
description = ""
date = 2017-10-25T14:02:09Z
aliases = []
[extra]
id = 4750
[taxonomies]
categories = []
tags = []
+++

==Common Lisp and Empty Lists?==
The comment after the CL example seems to imply that it would not work with the example list given, which includes nested lists that contain an empty list. It would therefore not satisfy the task? --[[User:Paddy3118|Paddy3118]] 00:50, 17 August 2009 (UTC)
:It works with the example given. The point is that if your list-of-lists-of-...-of-lists has as one of the intended leaf values ''a list'', you won't get the proper answer. This is true of any flattener, unless you mark the leaves as not-part-of-the-tree somehow. --[[User:Kevin Reid|Kevin Reid]] 02:32, 17 August 2009 (UTC)
::Hi Kevin, The intent is for nests of ultimately empty lists to not appear in the output at all. Doesn't lisp have some operation OP, where:
   lista OP listb = lista with the elements of listb concatenated
::And:
   lista OP emptylist == lista
::You should be able to complete a solution from that. The Python recursive solution is like that as the OP is list summation where:
   [1,2,3] + [4,5,6] = [1,2,3,4,5,6]
::And:
   [1,2,3] + [] = [1,2,3]
:::--[[User:Paddy3118|Paddy3118]] 06:35, 17 August 2009 (UTC)
:::That's not the problem. The code performs the specified task, but the task is to do something that is usually considered a bad idea, and symptom of poor design elsewhere, in CL. --[[User:Kevin Reid|Kevin Reid]] 03:10, 17 October 2009 (UTC)

==OCaml and Empty lists?==
I am curious about the Ocaml entry and its problem with empty lists. Is their not some way of telling the type system to expect one or another of two types? Maybe [[wp:Algebraic data type]] (but I'm way out of my depth here) --[[User:Paddy3118|Paddy3118]] 00:58, 17 August 2009 (UTC)

Doh! I have just seen the OCaml entry which has indeed been extended with an Algebraic data type solution, but sadly it misses the deep nesting around 6 and a deeply nested, but ultimately empty sub-list. --[[User:Paddy3118|Paddy3118]] 06:45, 17 August 2009 (UTC)

==Examples of use missing==
It would be good if all the language examples show how to flatten the list mentioned in the task description. C++, Common Lisp, E, Erlang, and Slate don't do this and R doesn't show example output. --[[User:Paddy3118|Paddy3118]] 02:36, 17 October 2009 (UTC)

: OK, I've now added the code to show flattening the given list in C++ (I already had written it anyway, because I needed it to test the algorithm). I guess you can see why I originally didn't. --[[User:Ce|Ce]] 08:14, 17 October 2009 (UTC)

Any more for any more? I note that the new Clojure example doesn't show the flattening applied to the example mentioned in the task either. Thnks, --[[User:Paddy3118|Paddy3118]] 18:32, 20 October 2009 (UTC)

==Omit TI-89?==
Should the TI-89 BASIC entry be changed to an omit from ...? And maybe the text moved to this talk page? --[[User:Paddy3118|Paddy3118]] 02:36, 17 October 2009 (UTC)

Talk pages are for discussing the editing of the page, not additional content. (I also think that additional explanation of examples, as may have been done in a couple places, does not belong on talk pages.) As I wrote the entry I leave it to others to debate whether it should exist, but it definitely doesn't belong on the talk page. --[[User:Kevin Reid|Kevin Reid]] 03:02, 17 October 2009 (UTC)

== Line drawing in web browsers (J solution) ==

Out of interest which browser/platform combinations are not correctly displaying the line drawing in the [[Flatten a list#J|J solution]]?

I tested the display of the line drawing characters on IE 8 (Windows 7) and Firefox 3.63 (Windows 7 and Ubuntu) and they display fine there. --[[User:Tikkanz|Tikkanz]] 00:02, 2 June 2010 (UTC)

:Just tried Google Chrome version 5.0.375.55 which was OK. --[[User:Paddy3118|Paddy3118]] 00:47, 2 June 2010 (UTC)

:::While the current display looks fine on most of the systems I tested it on, it looks wrong in Google Chrome 5.0.375.55 running on Vista.  --[[User:Rdm|Rdm]] 02:33, 3 June 2010 (UTC)

:: Put the box into J highlighting again and it gets ugly since the digits are highlighted by GeSHi to be bold. That makes them wider than the surrounding characters, which throws off vertical line alignment. I have no clue what font the <code>&lt;pre&gt;</code> blocks are using; if I set them manually to Consolas or Courier New it all works fine. Ah, apparently it's Lucida Console. This probably can be fixed in the MW style sheet, considering that Lucida COnsole is the default choice for IE 8 at least for Latin monospace. There seem to be some fonts that work and some that don't. I agree, though, that everything should be in a single block. I made that change more of pragmatic considerations. In any case, a few sample images: [http://hypftier.de/dump/box-consolas.png Consolas], [http://hypftier.de/dump/box-courier-new.png Courier New], [http://hypftier.de/dump/box-dejavu-sans-mono.png DejaVu Sans Mono], [http://hypftier.de/dump/box-lucida-console.png Lucida Console] – that's all I have in monospaced fonts. Note that Consolas didn't have line-drawing glyphs in Vista; dunno whether that has been fixed in an update or so, but Windows 7's Consolas has them. Seemingly only Lucida Console has a different width depending on the weight, though (which makes it even more unsuitable as the default). —[[User:Hypftier|Johannes Rössel]] 08:32, 2 June 2010 (UTC)
::: If I put something in the RC global stylesheet for 'pre', I don't want to name a font that may be platform-specific. Is there a font (say, "Courier" or "Monospace") that works across platforms? Alternately, I could use some free-license webfont, if folks can agree on one. --[[User:Short Circuit|Michael Mol]] 12:18, 2 June 2010 (UTC)
:::: I have no idea whether there are such fonts. Windows itself comes with Lucida Console, Courier New and Consolas (as well as a bunch of CJK fonts which are not really meant to be used in Latin-only contexts [at least, judging from the looks of their Latin glyphs]). Linux and similar systems tend to have fonts with quite creative names, such as Mono, Sans or Serif – I have no clue whether that are the actual names of the typefaces or merely aliases for fonts one actually might know.

:::: In any case, you can list a number of typefaces in decreasing order of preference which is pretty common practice. The keyword "monospace" refers to a non-specific (user-defined) monospaced font (which is what's currently used, apparently – either intentionally or just by not redefining <code>font-family</code> on <code>&lt;pre&gt;</code> – but that runs into the problem with Lucida Console at least with IE on Windows). So you could, theoretically, give a long list of fonts that are known to work and present on most, if not all systems. Downloadable fonts (WOFF, EOT, etc.) may be a last resort option but for a single language probably way overkill. Another possibility would be to tweak J syntax highlighting that numbers in boxes are no longer rendered in boldface. —[[User:Hypftier|Johannes Rössel]] 20:53, 3 June 2010 (UTC)

::::: On that 64 bit vista system, running Chrome, text which is not bolded still looks wrong.  For example: [[Compile-time_calculation#J]].  (It looks fine in IE.  I did not install firefox.)  Anyways, it is not clear that bold is the issue.  --[[User:Rdm|Rdm]] 11:55, 4 June 2010 (UTC)

== (Only appeared) Vandalization-like modification to C code ==

Take a look at the history and the C code. It seems it was removed a correct implementation, replaced or "prefixed" with a imagined silly conversation, and then replaced with a code that works on the ASCII representation of (non generic) list, but it is no way useful to flatten a '''real''' and "general" list held in memory; to me that code does not solve the task, since the ASCII representation of a list is not what we usually have when dealing with lists; the previous removed code did. I'll fix it back if no strong motivation for keeping the current silly and simplistic tricky implementation is given. --[[User:ShinTakezou|ShinTakezou]] 10:15, 1 October 2010 (UTC)

: +1 on putting the original code back, but I'm not so sure it was vandalism - maybe an over-enthusiastic mod by someone who hasn't lurked enough? --[[User:Paddy3118|Paddy3118]] 11:11, 1 October 2010 (UTC)

:Actually, if someone were to wrtie a corresponding [[Tree traversal]] routine, in C, that took in the string "[1,[2,[4,7],5],[3,[6,8,9]]]" which is a nested list representation of the example tree, then, ''but only then'', the disputed C routine for list flattening gains merit. --[[User:Paddy3118|Paddy3118]] 13:55, 1 October 2010 (UTC)

::I vote change it back, if it's incorrect. I think it's possible that the editor doesn't understand what a list is. (He may have been going for a "clever" solution; I doubt it was meant as vandalism, else he probably wouldn't have created a user account.) -- [[User:Eriksiers|Erik Siers]] 14:56, 1 October 2010 (UTC)

I have reverted the changes I made.  Sorry for the inadvertent breach of protocol.  I didn't realize it would cause so much consternation.  I assumed most people here would immediately understand.  If not, let me plead my case.  First, I think you have to admit that my solution yields the correct answer for all well-formed lists so how wrong can it be?

Second, just because one may not be accustomed to seeing a list being represented as a string doesn't mean it is not valid.  One can easily implement the entire list API in C using a string as the internal representation, and it will have roughly the same performance as a native lisp list.  (If in doubt, see the Tcl code below.)

Third, that "imagined silly conversation" where the professor extols Scheme, ridicules C, and finally says "just strip away all the extra parentheses" is not imagined at all.  It is roughly what a professor at a prestigious university once said when flattening a list in Scheme, and it breaks my heart to see so many people so uninformed about C.  If you just want to strip out the parentheses why not just do it directly?  And, what is more expressive than a language that lets you easily do so?

Granted, to the uninitiated, my solution may seem sophomoric at first, but consider this: Most of the languages with built-in list processing are based on the lambda calculus, but for some reason, it gets lost that regular expressions have just as deep mathematical roots.  It seems pretty clear to me that the Unix/C people explicitly chose regular expressions over lambda calculus.  For example, they could have passed S-expressions through their pipes, but instead of lists, they chose to pass strings and then use regular expressions to slice and dice.  So it stands to reason, that there is probably a very deep equivalence between lists and strings for which most Unix/C people have an intuitive feel.  For example, is there any real difference between Lisp's (car lst) and (cadr lst) and Awk's $1 and $2?  I think another clear example of this is Lisp's (eval) vs. C's lex/yacc, but I'll leave it to the reader fill in those details.

Lastly, in the real world, there is probably no better example of the equivalence between lists and strings than Tcl which is implemented in C and which maintains an equivalence between all lists and strings.  For example, the following Tcl code flattens a list simply by treating it as a string.  (The Tcl code below works provided the list does not hold strings with embedded braces, but this problem can be fixed with a better regexp.)  Note that at all times, the Tcl list is available as a '''real''' and "general" list held in memory as demonstrated by the use of the "llength" function before and after flattening the list, but that doesn't prevent the list from being by treating directly as a string.  Also, regarding the point that one needs a tree traversal routine written in C that traverses lists implemented as strings before my solution is correct, one can just link with libtcl to get exactly that.


```tcl

    proc flatten lst {
        string map {"{" "" "}" ""} $lst
    }

    set lst {{1 2 3} {4 5 6}}
    puts [llength $lst]
    puts [llength [flatten $lst]]

```


It should also be pointed out that one of the hallmarks of C is its ability to reinterpret data.  The high-performance Fortran and C programmers have long used arrays to model sequences or lists.  The trick is to decouple the contents of the list from the structure of the list.  This allows you to reinterpret the list by simply replacing the part that describes the structure of the list.  Using this approach, the problem of flattening a list can be solved on the order of O(1).  Essentially, this is what my original solution did (although on order of O(n)).  It extracted the contents of the list, and because a flattened list has almost no structure, there really wasn't anything left to do except print the results.  So please consider that the solution I originally posted is far from being vandalism and should be reinterpreted in the spirit of C.  --[[User:Pserice|Paul Serice]] Fri Oct  1 22:27:51 UTC 2010
: I think it was the lack of an intermediate representation that set people off. I think there was an assumed expectation that there would be a transformation from the input format to an internal representation, and another transformation to the output format; that way, with the intermediate representation defined on input and output, the actual mechanics would be clearer to the casual observer. (I didn't write the task, I don't know what was going through the task author's head. I'm just making an educated guess based on watching activity on the site.) If I read your code correctly, you optimized out the intermediate representation.
:
: Was the code correct to the letter of the task? I don't know. My suggested approach in cases like these would be to show both examples, with an explanation of how their approaches are different.
:
: All in all, relax; this sounds to me like a case where the task description could require clarification. And I think Paddy's intuition might be right; you should stick around and get a feel for how things work. Keep participating, and look at those items I left in your talk page about recommended reading and such; I'm still looking forward to your participation. :) --[[User:Short Circuit|Michael Mol]] 00:57, 2 October 2010 (UTC)

:Hi Paul, thanks for going to so much effort to explain your example. As you can see, we do value your participation and I would hate for you to give up participation in the site. We need new contributors as well as readers. 

:On the meat of your explanation: I do understand that lists can be represented as strings - I went to the trouble of 'stringifying' the tree example to make sure. C, as you pointed out, is the implementation language of many other languages, but an answer where C just called another interpreter would not help the site. I think that most people and most publicly available examples of a list on C would ''not'' base its internal representation on strings - it is a common enough training task and most of the answers will involve dancing pointers and mallocs. Even on Unix. You might also find that although there may outwardly be a relationship between TCL lists and strings, Tcl has made optimisations over the years and internally other, more optimised data structures might be transparently used for better performance (I am unsure of just which data structures are optimised though- maybe others can help me out here). --[[User:Paddy3118|Paddy3118]] 02:12, 2 October 2010 (UTC)

:: So sorry for all this buzz; my point was very simple, indeed, and by "vandalization-like" I did not mean there was an intentional real "vandalization", just a replacement made in good faith '''but''' that replaced a perfectly working implementation that had, in my opinion, a better "view" of the task; in these cases (two implementations that looks both right someway), I am for keeping the previous implementation; and expecially in this case where clearly the new implementation works at a different "level" (the ascii representation of a list, which is usually not so easily manageable as a "binary" representation of a list, which is what "we" usually have when we need a list). I would keep the "ascii solution" too, or maybe we should have more task doing smart manipulations on strings. --[[User:ShinTakezou|ShinTakezou]] 10:49, 16 October 2010 (UTC)

==String editing examples marked incorrect==
I have marked four examples as incorrect as they go against the spirit and intent of the task by merely manipulating a string form of the input to form the output as a string. The task points to the definition of a list datatype. there is no attempt to form a list datatype it's just character manipulation rather than list manipulations. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:32, 24 October 2017 (UTC)

: (Concerning the REXX language example)     ---     In the REXX language, there is   ''only''   a string form (type), there is no other form.   All values (of variables) are stored as (character) strings.   There is other way to store data in REXX.   Because of this language feature (some might call it a limitation), I saw no reason to '''not''' create a REXX solution to this task, as the string form for the output is indistinguishable from a list form.   I didn't interpret the task that the data must be   ''in''   a list form.   I believe that the REXX solution is in the spirit of the task in that the REXX solution   <u>did work on the equivalent of the list</u>   as specified in the task's preamble.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:06, 24 October 2017 (UTC)

:: How to stop the task degenerating into lots of new examples just editing strings? I am at a loss!
:: I do note however that [[Tree_traversal#REXX]] seems to do more of what seems to be list-like processing whereas this example treats the string as text. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:01, 25 October 2017 (UTC)

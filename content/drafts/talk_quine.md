+++
title = "Talk:Quine"
description = ""
date = 2015-05-13T22:15:00Z
aliases = []
[extra]
id = 2249
[taxonomies]
categories = []
tags = []
+++

== What is a quine? ==
I did not read up as fully as I should have on this task. The program is to output its own source without accessing files. This makes the examples here incorrect. See [http://en.wikipedia.org/wiki/Quine_%28computing%29 the wiki] for more clarification. --[[User:Mwn3d|Mwn3d]] 20:32, 17 November 2007 (MST)

I don't think this task does illustrate the comparative features of different languages very well. As the Wikipedia article explains, one way to solve this task that will work in every language is put the program code into a string, together with some mechanism to replace it at a special location with a copy of itself. For languages which keep a more-or-less one-to-one representation of the code around at runtime (Lisp, Smalltalk, Forth, etc.), it just boils down to accessing this representation. Smaller solutions are in danger of becoming an exercise in obfuscation, or at least become unreadable very quickly. And the examples seen now which mostly access files are obviously missing the point.

So I'd propose to delete this task, and replace it with a task that shows how to access (and maybe manipulate) code at runtime
for languages that support it. The general solution can be subsumed by task(s) that show how string manipulation works. [[User:DirkT|Dirk Thierbach]] 18 November 2007

The name of the task is '''Quine'''. A Quine is not allowed to open its source file and to copy it to standard output. All implementations which do this should be removed. [[User:194.48.84.1|194.48.84.1]] 05:56, 20 November 2007 (MST)

The task states (perhaps not clearly enough) that the program itself should do the printing, not any toplevel read-eval-print loop, or equivalent. Otherwise, all constants in languages that have such a toplevel would be quines. But that is completely missing the point of the original problem, which (as explained for example in the book ''G&ouml;del, Escher, Bach'') is about self-application, and quoting. -- [[User:DirkT|Dirk Thierbach]] 21 November 2007
:I clarified the task.  Different scenarios have different requirements for output.  The Lisp example satisfied the spirit of the task (produce a usable reproduction of one's own code), so the task itself needed to be adjusted.  I won't take that approach in all cases; It's a matter of the spirit of the task. --[[User:Short Circuit|Short Circuit]] 13:06, 21 November 2007 (MST)
:: I beg to differ - it's not the "scenarios" which have requirement for output, it's the spirit of the task itself which is not correctly presented if you allow constants (or empty programs) as solutions. I strongly recommend to read at least the [http://en.wikipedia.org/wiki/Quine_%28computing%29 Wikipedia article], or the book mentioned above (your library should have a copy). The philosopher Quine studied '''self-reference''', as exemplified in the paradox "'Yields falsehood when preceded by its quotation' yields falsehood when preceded by its quotation."
:: And it's this idea that makes up a Quine - A piece of code, applied to itself, once quoted and once unquoted. (BTW, this then again is the same technique used to prove undecidability theorems.) And the extra requirement "should '''print''' it's output" is one way to enforce that the quoting has to be done by the program itself, not by some external read-eval-print loop.
:: The task is bad enough as it is (it doesn't really help in comparing programming languages), and it's not improved by allowing "fortunate" border cases which take away the main point. Ok, so what's the procedure to resolve differences in opinion? [[User:Dirkt|Dirkt]] 14:16, 21 November 2007 (MST)
::: To quote the Wikipedia page in question:

```txt
In computing, a quine is a program, a form of metaprogram, that produces its complete source code as its only output. For amusement, programmers sometimes attempt to develop the shortest possible quine in any given programming language.

Note that programs that take input are not considered quines. This would allow the source code to be fed to the program via keyboard input, opening the source file of the program, and similar mechanisms. Also, a quine that contains no code is ruled out as trivial; in many programming languages executing such a program will output the code (i.e. nothing). Such an empty program once won the "worst abuse of the rules" prize in the Obfuscated C contest.
```

::: While the question that Quine investigated implies the quote-interpreting solution, the definition of the problem (per Wikipedia, anyway), doesn't require it.  Reading it over, I'll agree that the Lisp example doesn't demonstrate anything one wouldn't get from [[Empty Program]], and would be better replaced with the Scheme/Common Lisp example from the Wikipedia page.  I'd have no problem modifying the task description to discard empty programs as trivial, or even requiring the program to use a human-readable output method.  Still, the task can still serve to compare languages.  Some languages make accessing the source simple, like in the Forth example, or the JavaScript example on the Wikipedia page.  Even the string-modifying solution allows for differences between languages; Different languages have different best solutions for replacing a substring.
::: It's not a 1:1 technique comparison, but neither is any task with both functional and procedural language examples.  Would you be reasonably satisified if the task was changed to require human-readable output and exclude empty programs? --[[User:Short Circuit|Short Circuit]] 21:35, 21 November 2007 (MST)
:::: The problem with Wikipedia of course is that it is great to get a rough feeling for some topic, but it's neither precise nor authoritative. So it's dangerous to take some definition there literally and insist it's the "correct" one - using the context to determine the ''ideas'' is much more appropriate for Wikipedia.
:::: It's true that this task exposes (a) access to source code and (b) ways to modify strings, but considering the amount of misunderstanding this task generates, I still think it is much better (and more informative) to handle those on pages of their own. Especially (a) could only benefit from greater detail. Languages like Forth, Smalltalk, Lisp and Tcl have interesting ways to access and modify code at runtime, but you need a code example to bring this out. A Quine isn't one.
:::: I've expanded the task description with some background to better explain the "spirit", and required the "canonical" version as one of the code examples. I've also ruled out constant expressions, and replaced your Lisp version with the one from Wikipedia. If you've objections or improvements, feel free to modify it. [[User:Dirkt|Dirkt]] 03:43, 22 November 2007 (MST)

==What is the license for the Forth example?==
It says it was copied. I could not easily find a licence for the donor site. --[[User:Paddy3118|Paddy3118]] 02:42, 30 March 2009 (UTC)
: Wow... could there exist a "restrictive" license for such a code? It's like saying Hello World is copied, and asking about the license for it (if someone did it and laws agree, I hope at least everyone will laugh at him!). Everyone, taking a look at a forth manual, e.g. I use [http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/The-Text-Interpreter.html#index-source-_0040var_007b-_002d_002d-addr-u---_007d--core-1444 this], can produce exactly the same code, without copying it from nowhere. (<code>Source</code> pushes the address of the current input buffer and its length LEN on stack: ADDR LEN; type "prints" LEN bytes starting from ADDR). Rather straightforward. If the ''copied'' is a problem, to me it's enough to strip the link (which anyway shows other more complex quines), and ask to some forth expert to produce in the smallest time the most straightforward quine using the smallest possible number of forth primitives. I bet it would write down "source type". --[[User:ShinTakezou|ShinTakezou]] 10:34, 30 March 2009 (UTC)
: Ick.  Being on Rosetta Code, it's licensed GFDL whenever distributed by us.  But being that it's copied from another site, there's a question of whether the other site's license is violated.  As for what license that would be...Under US copyright law, anything without an explicit copyright label is assumed to be under an "all rights reserved" scenario.  However, I think that with appropriate citation, it's acceptable.  I'm not sure if what's there qualifies as appropriate citation, though; Someone who knows more about published papers would be a better judge of that. --[[User:Short Circuit|Short Circuit]] 22:11, 30 March 2009 (UTC)
: After looking at the edit history, I very strongly suspect that Shin created it on his own, and IanOsgood added a link to the list later.  So I think we're completely kosher on this one, though the wording should probably be a bit more clear that the code wasn't copied.  AFAIK, if Infinite Monkey Corp independently came up with Romeo and Juliet, they'd be every bit as entitled to its copyright as Shakespeare. (Assuming modern times...) --[[User:Short Circuit|Short Circuit]] 22:18, 30 March 2009 (UTC)
:: No, I haven't written the code (as far as I remember I have not contributed to any forth code yet; surely not to quine anyway), or I would have said that here. I simply said that noone can claim a copyright on such a small piece of code, except maybe the creator of the forth language. The code simply says "get the buffer where the source text is, and print it"; provided that a language has a primitive to "print" and one to get such information, everyone able to read a manual can, '''without copying''', produce this quine. If it is not stack based, one could say something like "printf("%s", get_source());" (interpreted C?)... Anyway, if it is an issue, one could try to contact Neal Bridges ([http://www.complang.tuwien.ac.at/forth/quine-complex.fs named here]), maybe he wrote it (or know who can have written it), altogether with more complex quine(s) (and these can't be copied without thinking about a lincense, since hardly one can reproduce them only reading the forth manual...); it appears [http://www.nyx.net/~gthompso/self_forth.txt here also] (author unknown! and cheating suspect...).
:: Someone wrote it, maybe after forth manual reading, or maybe taking a look on the net... and IanOsgood added just the link to a list of forth quines, among these there's also "ours". I think there are not license violation (I imagine RC must care a lot about these ''details''), but I am not a lawyer (rather in this case I would like to be a lawyer-eater) --[[User:ShinTakezou|ShinTakezou]] 00:12, 31 March 2009 (UTC)
::: Third thought, I think the guy suspecting it's almost cheating, after all, it's right; I've created the following C cheated-quine:


```c
#include "cheater.h"
int main()
{
  printf("%s", __source__);
}
```


:::Which works (with gcc statement expression extension) provided that cheater.h is


```c
#include <stdio.h>
#define __source__ ({					\
      char *s = "#include \"cheater.h\"\n\
int main()\n\
{\n\
  printf(\"%s\", __source__);\n\
}\n"; s;\
    })
```


::: From a "functional" point of view, this works the same way of forth (and maybe others), by accessing the text of the source stored in memory (not by loading it at runtime...); since C is compiled, the binary holds no the source (compiler at least once loaded the source in memory, but compiled binary can't see its past...), and I had to include it manually... (Hm, one could work harder on debug informations and ELF maybe, and write a more forth-like quine accessing at runtime the "segment" where the whole source code, stored by the compiler itself this time, is and print it...)... Is this a honest quine or really cheating? (Interesting question to me:D) --[[User:ShinTakezou|ShinTakezou]] 00:54, 31 March 2009 (UTC)

== Second scheme example correct? ==
I have some doubt about the correctness of the second scheme example. It only works when entered into an interpreter. When compiled there will be no output. The reason for this is that the expression evaluates to a result which is identical to the original expression. However, I realise that this is not the same as simply entering a constant expression like <code>0</code>, which will evaluate to itself. When the given expression is evaluated, the lambda function will be applied to the argument that follows, resulting in an expression that is identical to the original expression. The problem description states that the program must output its own source. The given example doesn't produce any output by itself (generated by <code>display</code> in scheme). How should we handle this? --[[User:Dsnouck|Dsnouck]] 14:16, 26 May 2010 (UTC)

: If you feel that this is a significant issue, I think you should document it.  In the general case, the concept and implementation of "output" is situational and depends on the character of the host session, and Quines traditionally assume just one mode of output.  (And this is more of an issue in some languages than others.)  --[[User:Rdm|Rdm]] 14:24, 26 May 2010 (UTC)

== nostalgia comments ==
I added some nostalgia notes at the end of the Fortran example.  I had assumed that this [QUINE} being such an old challenge, that it would be pertinent.  Almost a half-century ago!! [[User:Gerard Schildberger|Gerard Schildberger]]

== Haskell evaluation ==

If you're going to allow an expression that evaluates to itself, won't "blah" evaluate to "blah"? For that matter \x.x will evaluate to \x.x [[Special:Contributions/71.176.132.192|71.176.132.192]] 20:35, 26 April 2011 (UTC)

:This is the same problem as "if you allow programs which print their source, won't 10 LIST be a quine in classic BASIC?". Or, isn't 42 a Lisp quine? Typing 42 into the REPL produces 42! The answer is that a quine must not depend on some trivial self-evaluating properties built into an object, or built in self-reference which pulls out the source code. A quine must be some kind of '''computation''' which produces its own image.  Self-evaluating objects do not compute anything: they appeal to the axiom of self-evaluation which is built into the language. Self-regurgitating programs likewise do not compute anything, they appeal to built-in access to the program source. (Many of the programs put up here so far fail these criteria.) [[Special:Contributions/24.85.131.247|24.85.131.247]] 05:59, 2 October 2011 (UTC)

:I just added an example for a compilable Haskell Quine. --[[User:Jgriebler|JMG]] ([[User talk:Jgriebler|talk]]) 22:14, 13 May 2015 (UTC)

== C quine ==

I removed some comments about style about the C quine: it's a bit silly to be serious about good coding style for this subject.  If you want properly indented, properly typed and properly header'ed C code, here you go:
```c
#include <stdio.h>
int main(void){
	char*p="#include <stdio.h>%cint main(void){%c%cchar*p=%c%s%c;%c%cprintf(p,10,10,9,34,p,34,10,9,10,9,10,10);%c%creturn 0;%c}%c";
	printf(p,10,10,9,34,p,34,10,9,10,9,10,10);
	return 0;
}
```
 which does require a trailing newline, btw.  Being "more correct", It definitely does not help one read or understand the code. --[[User:Ledrug|Ledrug]] 03:53, 1 September 2011 (UTC)

== REXX Quine ==


'''Error?'''


The REXX program (below) produces an extra blank line in the output.

This extra blank line can't be noticed easily (or not at all) when just viewing the output,

but if the output is re-directed to a file, the extra blank line then becomes obvious.

The definition of a quine isn't clear in this regard.  Is a listing (of a 19-line program)

with an extra blank line equal to a 19-line program?

'''IEC999I “Probable user error. Correct job input and resubmit.”'''
: It doesn't for me.
: The original program contains 53 lines of code (there's a new-line after the last end clause), as does the result; passing the original program and its output through both <tt>diff</tt> and <tt>wc</tt> confirm this:

```txt

$ wc -lmw RQuine03.rex
      53     215    1029 RQuine03.rex
$ cat RQuine03.rex | wc -lmw
      53     215    1029
$ rexx RQuine03.rex | wc -lmw
      53     215    1029
$ regina RQuine03.rex | wc -lmw
      53     215    1029
$ rexx RQuine03.rex |diff -s RQuine03.rex -
Files RQuine03.rex and - are identical
$ regina RQuine03.rex |diff -s RQuine03.rex -
Files RQuine03.rex and - are identical

```


:: I found that the original program (well, at least, the current program) has 52 lines of source code, not 53.  If there's a new-line after the "End x_", I can't see it. I looked at the Rosetta Code program via "edit", and there is no blank line after the trailing underscore. I used Regina  (REGINA Q3.REX &gt; Q3.OUT) and again verified that the version 3 REXX program did indeed write an extra blank line (for a total of 53.)  The Rosetta Code view of the program does not show a blank line after the program. However, to ensure a blank line after the original source, I then added a blank line after the Q3 program and ''it still produced the same result!'' (53 lines). So two different REXX programs produced the same (quine) output, but the two REXX programs are different.  So the one that produced the output as the same as the source is the quine program, and the other, ... is not. I used the FC (MS DOS), WINDIFF (MS Windows), and KEDIT programs to verify the outputs. -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:51, 28 June 2012 (UTC)

: The ''original'' code on my system is 53 lines long.  It produces another file that's also 53 lines long as demonstrated above: that's what it's meant to do.  I can't be held responsible for any accidental changes that get made to the contents of the wiki (they even post caveats about merciless editing of things you write in many places). I'm pretty sure I didn't delete the last newline and as I normally put the <tt>&lt;/lang&gt;</tt> tag on a line of its own I can't believe I chose this entry to do something different.  (I have edited the sample to insert a newline at the end of the source.)
: The fact that two programs with varying numbers of blank lines (at the end or otherwise) produce the same output is beside the point.  The one that (in this case) starts with 53 lines, produces a file that's 53 lines long and is identical to itself is the quine, the others aren't; regardless of what they produce as output.
: Here's a thought though:
:# Run the program extracted from the wiki in whatever state it is in.
:# Capture the output from 1 and save it as a source program.
:# Run the program from 2 and capture its output too.
:# Compare the results of the output from 3 with those from 2.
: Dollars to doughnuts they'll both be 53 lines long, identical and a quine.  (I tried it: they are.) --[[User:Alansam|Alansam]] 06:05, 28 June 2012 (UTC)

:: Yes, they are now after the change that was made.  I have no doubt that your original program is 53 lines.  I wish you would've done the same four steps on the 52-line version as it existed on Rosetta Code, the same program that I used.  They didn't agree before the change.  I'm not questioning what you meant to do, just what was actually in the sample code (on Rosetta Code).  I have no access to any other copy.  I'm sorry if the dog ate your homework, no demand for who's reponsible for the changes was made or implied. There's always the history file if want to know who did what. But the original file as it was on Rosetta Code didn't produce an exact copy of itself. The 52-line version didn't produce a quine, the (current) 53 line does.  If it old program did produce a quine, you wouldn't have a need to change it. I also don't understand your ad hominem attacks about 'they" posting caveats about merciless editing of things I write in many places.  Could you be more specific if you think that would be pertinent here? -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:00, 28 June 2012 (UTC)

:: Also, since this discussion is now in the ''talk'' page, I'd like to point out that saying another version is a cheat isn't professional nor appropriate. If it ''is'' a cheat, then flag it as '''incorrect''' and state why.  The REXX versions 1 and 2 don't open any external files, as per the task's description/requirements. -- [[User:Gerard Schildberger|Gerard Schildberger]] 02:01, 28 June 2012 (UTC)

: I didn't say the other version "is a cheat", I said it was "kind of a cheat", there's a '''huge''' difference in emphasis.  Please don't cherry-pick other people's words to score points: that's unprofessional!

:: Sorry, a kind of cheat or a cheat is still a cheat. I'm sorry if you think that I thought that name-calling wasn't appropriate. If that is your idea of cherry-picking, then I'll rephrase. Saying another version is a kind of cheat isn't professional ... Saying (just for instance) that something smells or kind of smells sounds like the same thing. It wasn't my intention to provide a "kind of" cheat; the mechanism used is a very simple way to provide what the quine task asked for.  I don't even begin to  understand what you meant by scoring points. -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:00, 28 June 2012 (UTC)

: Now we're here though; if you read the description of what a Quine is supposed to be (see above) with particular reference to the quote from Wikipedia:

```txt

Note that programs that take input are not considered quines. This would allow the source code to be fed to the program via keyboard input, opening the source file of the program, and similar mechanisms...

```

: I suspect that using <tt>sourceline()</tt> falls squarely into the category of "opening the source file of the program and similar mechanisms" which disqualifies it as a Quine. --[[User:Alansam|Alansam]] 06:05, 28 June 2012 (UTC)

:: I have a slightly different definition of squarely.  The REXX program doesn't open the source file of the program. It doesn't read (or take) the source file through any other mechanism.  Reading a copy of the file that is on (or in) a virtual device might be considered taking (reading?) a copy of the source. Putting a number of lines in a stack and then "reading" (or pulling) is "taking" some input could qualify as a method disallowed in the Wiki definition of a quine. I hesitate to call a stack (internal or external queue) a device, albetit a virtual one.  There isn't much difference between a stack and a virtual device.  I don't begrudge your method nor will I call it names, and I certainly wouldn't call it a kind of cheat.  I think debating the definition(s) of a quine (as it applies here) would just lead to endless arguments about the wording of a quine (and/or what the words mean), and what specifically should/could be disqualified, and what qualifies as reading a copy of the program from an input.  It's just another method that can be used. My only concern was that the (original) program as I observed it didn't produce a quine.  I don't question the method. The REXX method for REXX versions 1 and 2 can also be used, for instance, in CMS when using the NUCXLOAD function which further distances a program from its source, which in simple terms, can load a copy of a REXX program (or it could be any kind of file) in virtual memory (and also rename it), and the original source deleted or disconneted (no longer available to the user).  The program can be (much) later be invoked (even by another user) and still work as intended.  This method lends itself to a persistent program, surviving what most people call a re-boot (or re-IPL, in CMS terminology), even though the source code is longer present in any form.   This subject was of much interest when that capability was introduced into CMS and via ''saved systems'', and it made for some interesting programming techniques, the least of which was to hide the REXX code from the user. This is a lot of discourse on a simple error in the original REXX program (as it existed on Rosetta Code), just admitting that the 52-line version didn't reproduce itself due to a missing blank line. -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:00, 28 June 2012 (UTC)

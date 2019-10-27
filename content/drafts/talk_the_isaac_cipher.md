+++
title = "Talk:The ISAAC Cipher"
description = ""
date = 2014-07-25T08:30:46Z
aliases = []
[extra]
id = 17791
[taxonomies]
categories = []
tags = []
+++

==Pseudo-code?==
You might want to add pseudo-code and a description of the algorithm as well as a pointer to the C-code. More work, but would lead to a better task. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:16, 22 July 2014 (UTC)
:Thanks for the suggestions. This is my first Rosetta task, so I was naturally a little unsure as to what is allowable. I decided to err on the side of brevity, but I'll be happy to pad it out a little with more detail, if that's permitted. Bob Jenkins' original C reference code has been included here under C, my Pascal port of same under Pascal. The Pascal should (or will have to) serve as 'pseudocode' for the rather convoluted algorithm. After all, Pascal was originally developed to be a "teaching language" and you often find pseudocode that resembles Pascal very closely :) This task really falls into two halves: the translation of ISAAC, and the solution of my encryption task. Obviously far more leeway is permissible in the latter. 

:There was something I needed to ask: How to include an auto-updating table of contents of the sort seen on other task pages? I'm afraid my wiki skills are rudimentary. --[[User:BlaiseP|BlaiseP]] ([[User talk:BlaiseP|talk]]) 10:57, 22 July 2014 (UTC)

::Hi BlaiseP, how about:
::# Finishing off the C example by adding a main() routine that accomplishes the task.
::# Mentioning Pascal as well as C as being a reference implementation?
::On the contents section, it will automagically appear when there are 4+ solutions with <nowiki>{{herader|...}}</nowiki> sections.
::Hopefully that will be enough to get others started. (P.S: Welcome to RC :-)
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:23, 22 July 2014 (UTC)

:::Thanks Paddy :) Actually I've completed a few task solutions over the past year or so, but this is the first one I've actually attempted to inaugurate. My C skills are on the rusty side, I'm afraid. I can read C ok, insofar as what I read can be translated to my 'native' language and first love, Pascal. I usually get confused over which darn variables should be pointers (or not). I'll give completing it a go if there is no input from seriously competent C hackers over the next week or so. As to _reference_ implementations of ISAAC, there is really only one: the C. Everything else has been a port of that. I only mentioned 'reference' in connection with Pascal because it is the reference solution of this _task_, and hopefully will help others complete the initialization and encryption part. --[[User:BlaiseP|BlaiseP]] ([[User talk:BlaiseP|talk]]) 11:58, 22 July 2014 (UTC)

:::As suggested, I have coded a fully working solution in C and brought Pascal into line with this. The two programs' XOR encryption outputs are identical. --[[User:BlaiseP|BlaiseP]] ([[User talk:BlaiseP|talk]]) 10:14, 23 July 2014 (UTC)

== Pascal Compiler error(s) ==

Curious as I am sometimes, I installed free Pascal and got this compiler error(s)
https://www.dropbox.com/l/a6rIEG5zByiM4UDKhwGMpr?
(The IDE does not allow copy / paste :-( 
Any advice?? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 10:35, 23 July 2014 (UTC)
:Odd, but you have a default Lazarus install, I think(?). I'm compiling mine on the command line with 

::fpc isaac.pas

:and I am using Standard FPC, ie. no extra $defines. It's possible you're in Delphi mode, as you seem to be getting errors in the area of function return parameters. Delphi uses <result := VAR>, standard Pascal uses the name of the function, like <Vernam := VAR>. Please use the most recently amended copy (edited today) and also try the C version. You may have to delve into your fpc.cfg file and check what compile options you have set. --[[User:BlaiseP|BlaiseP]] ([[User talk:BlaiseP|talk]]) 10:58, 23 July 2014 (UTC)

::stop press. it works (after restart when the path became active)

```txt
Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
MOD    : 734270227D36772A783B4F2A5F206266236978
```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:50, 23 July 2014 (UTC)
:::Ah good. I was going to suggest adding the fpc bin directory to your %PATH but your install seems to have done that by default. I noticed you were using the old DOS-style (Turbo Pascal) IDE to compile. While there is nothing whatever wrong with this, I'd recommend using a good programmer's text editor like [http://notepad-plus-plus.org/ Notepad++] (Windows) or [http://www.geany.org/ Geany] (Linux) for viewing and editing your code, and then just compiling on the command line with >fpc filename.pas. Simple is often best :) --[[User:BlaiseP|BlaiseP]] ([[User talk:BlaiseP|talk]]) 06:55, 24 July 2014 (UTC)
::::I'm happily using Kedit ever since I "lost" VM's Xedit and TSO's ISPF Editor. Can I ask you one or the other Pascal Question offline if some arise)? pachl .. chello.at --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:51, 24 July 2014 (UTC)
:::::Sure - I'll gladly help in any way I can. --[[User:BlaiseP|BlaiseP]] ([[User talk:BlaiseP|talk]]) 09:30, 24 July 2014 (UTC)

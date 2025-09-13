+++
title = "J"
description = ""
date = 2019-09-25T20:43:25Z
aliases = []
[extra]
id = 2402
[taxonomies]
categories = []
tags = []
+++
## The J language
J is a notational programming language designed for interactive use.

It is an array language; data is universally structured as rectangular arrays.

It is a functional language; creation and composition of functions is emphasized.

Object-module and imperative techniques are supported, but not required.

The J programming language was designed and developed by [http://en.wikipedia.org/wiki/Kenneth_E._Iverson Ken Iverson] and Roger Hui. It is a closely related successor to [APL](https://rosettacode.org/wiki/APL), also by Iverson which itself was a successor to the notation Ken Iverson used to teach his classes about computers in the 1950s.
<br clear="all"/>

The notation draws heavily from concepts of [Abstract algebra](https://en.wikipedia.org/wiki/Abstract_algebra) and [Tensor calculus](https://en.wikipedia.org/wiki/Tensor_calculus), simplified for describing computer architecture and design to a pragmatic business audience.  (The ideas themselves are simple, but for some reason the topics scare most teachers.)

## Reading J
J is meant to be read with the aid of a computer. J sentences are single lines and trying variations and simplifications of an expression is common practice. The first step in understanding any J sentence is to understand the data you started with and the data which resulted.
When learning how a J sentence works, you can also try simpler sentences with the same data or perhaps related data.
When trying to understand contexts that use large data structures, it can often be wise to investigate small, representative samples until you understand how the code works.

Unless you attend an institution which has made a J interpreter available to you through your web browser (or preinstalled on your machine), if you want to see how J works you should probably [https://code.jsoftware.com/wiki/System/Installation install] a copy of J -- or you can try one of the "try me" links, below.
If you want to understand how to experiment with alternative expressions you should probably also be studying some of its [documentation](https://rosettacode.org/wiki/j:Guides/Getting%20Started#Documentation).

For example, the phrase <code>(+/ % #)</code> finds the average of a list of numbers.


```J
   (+/ % #) 1 2 3
2
```


To understand how this works, you might try working with simpler sentences and their variations.


```J
   +/ 1 2 3
6
   +/4 5 6
15
   # 1 2 3
3
   # 2 3 4
3
   6 % 3
2
   15 % 3
5
   (+/ % #) 4 5 6
5
```


By themselves, these experiments mean nothing, but if you know that +/ was finding the sum of a list and # was finding the length of a list and that % was dividing the two quantities (and looks almost like one of the old school division symbols) then these experiments might help confirm that you have understood things properly.

## Some Perspective
If you wish to use J you will also have to learn a few grammatical rules (J's parser has [http://www.jsoftware.com/help/dictionary/dicte.htm 9 reduction rules] and "shift" and "accept" - the above examples use four of those rules). J verbs have two definitions - a single argument "monadic" definition and a two argument "dyadic" definition.
These terms are borrowed from music and are distinct from Haskell's use of the word "monad".
The dyadic definitions are in some sense related to LISP's "cons cell" but are implemented as grammar rather than data structure, and are a pervasive part of the language.

Another pervasive feature of the language is [rank](https://en.wikipedia.org/wiki/Rank_(J_programming_language)).

The language represents capabilities of hardware.
For example, if language did not have an internal stack, a word's definition could not be used during the execution of that word. All current J implementations support recursion, but in some sense this is a convenience, and it's reasonable to imagine J implementations which do not (perhaps in a "compile to silicon" implementation).

## J on RosettaCode
Discussion of the goals of the J community on RC and general guidelines for presenting J solutions takes place at [House Style](https://rosettacode.org/wiki/J/HouseStyle).


## Jedi on RosettaCode
*[Roger Hui](https://rosettacode.org/wiki/User:Roger_Hui): [contributions](https://rosettacode.org/wiki/Special:Contributions/Roger_Hui), [J wiki](https://rosettacode.org/wiki/j:User:RogerHui)
*[Tracy Harms](https://rosettacode.org/wiki/User:TBH): [contributions](https://rosettacode.org/wiki/Special:Contributions/TBH), [J wiki](https://rosettacode.org/wiki/j:User:TracyHarms)
*[Dan Bron](https://rosettacode.org/wiki/User:DanBron): [contributions](https://rosettacode.org/wiki/Special:Contributions/DanBron), [J wiki](https://rosettacode.org/wiki/j:User:DanBron)
*[Arie Groeneveld](https://rosettacode.org/wiki/User:Gaaijz): [contributions](https://rosettacode.org/wiki/Special:Contributions/Gaaijz)
*[Raul Miller](https://rosettacode.org/wiki/User:Rdm): [contributions](https://rosettacode.org/wiki/Special:Contributions/Rdm), [J wiki](https://rosettacode.org/wiki/j:User:Raul_Miller)
*[Jose Quintana](https://rosettacode.org/wiki/User:96.57.161.34): [contributions](https://rosettacode.org/wiki/Special:Contributions/96.57.161.34), [J wiki](https://rosettacode.org/wiki/j:Stories/JoseQuintana)
*[Ric Sherlock](https://rosettacode.org/wiki/User:tikkanz): [contributions](https://rosettacode.org/wiki/Special:Contributions/tikkanz), [J wiki](https://rosettacode.org/wiki/j:User:RicSherlock)
*[Avmich](https://rosettacode.org/wiki/User:Avmich): [contributions](https://rosettacode.org/wiki/Special:Contributions/Avmich)
*[VZC](https://rosettacode.org/wiki/User:VZC): [contributions](https://rosettacode.org/wiki/Special:Contributions/VZC)
*[Alex 'bathala' Rufon](https://rosettacode.org/wiki/User:Bathala): [contributions](https://rosettacode.org/wiki/Special:Contributions/Bathala), [J wiki](https://rosettacode.org/wiki/j:bathala)
*[David Lambert](https://rosettacode.org/wiki/User:Lambertdw):[contributions](https://rosettacode.org/wiki/Special:Contributions/Lambertdw)
*[JimTheriot](https://rosettacode.org/wiki/User:JimTheriot): [contributions](https://rosettacode.org/wiki/Special:Contributions/JimTheriot)
*[Devon McCormick](https://rosettacode.org/wiki/User:DevonMcC): [contributions](https://rosettacode.org/wiki/Special:Contributions/DevonMcC)

## Try me
Want to try one of those cryptic J lines you see peppered through RC?
Try pasting it into this [http://joebo.github.io/j-emscripten/ browser-based implementation of J].

If you want to be a bit more interactive, and get some guidance from J gurus, you can join the actual J IRC channel on Freenode, #jsoftware.  Buubot and several other J eval bots run there.
If you don't have an IRC client you can try [http://webchat.freenode.net/?randomnick=1&channels=jsoftware freenode's web interface] (or just [http://webchat.freenode.net/?channels=jsoftware&randomnick=1 give it a quick spin]).
More [details about the J IRC community](https://rosettacode.org/wiki/j:Community/IRC) is available.

If any of that piques your interest, and you want to explore a little more, you can [http://www.jsoftware.com/ download J] and [http://www.jsoftware.com/forums.htm join the J forums].

If you have problems executing any of the J code here on Rosetta, please make a note of it either on the task page itself, on the talk page, or on the appropriate [http://forums.jsoftware.com J forum], whichever is best.
It might be that there's a version dependency that needs to be documented, or you might have found an actual bug.

## Todo
[Reports:Tasks_not_implemented_in_J](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_J)

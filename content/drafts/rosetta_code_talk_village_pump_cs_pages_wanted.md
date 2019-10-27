+++
title = "Rosetta Code talk:Village Pump/CS Pages Wanted"
description = ""
date = 2009-07-21T04:29:15Z
aliases = []
[extra]
id = 4015
[taxonomies]
categories = []
tags = []
+++


### Swapping in a ring

XOR swap is limited to the modulus of 2<sup>''n''</sup>. A more general swap is based on addition and subtraction, or equivalently on addition and negative inverse in the ring. This one works for any modulus. Here is an example in [[Ada]]. Given

```ada

type N is mod 5;  -- Ring of 0,1,2,3,4
X, Y : N;

```

Swap X and Y:

```ada

X := X + Y;
Y := X - Y;
X := X - Y;

```

5 is not a power of two, yet it still works. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:14, 17 March 2009 (UTC)
:Isn't the reason that the XOR swap works because integers are stored as binary and XOR will do a bitwise operation? I think the XOR one is better because not all languages have modulus types, but pretty much all languages (if not all) represent integers in binary (or can at least do bitwise operations on them as if they were binary). Maybe I'm missing the problem. --[[User:Mwn3d|Mwn3d]] 18:54, 17 March 2009 (UTC)

::There is no problem, except that they are two different mathematical structures. XOR works on a lattice. +/- does on a ring. When talking about numbers +/- is more general than XOR. Or better to say that XOR is illegal, because it is not an integer operation. XOR cannot be defined on integers. It can only be defined on modular rings of 2<sup>n</sup>. It is a language design hole which allows XOR for ''int'' in C. But it is OK for ''unsigned'', because ''unsigned'' is not an integer, it is modular 2<sup>n</sup>. Furthermore, in a strongly typed language you might have no access to the representation of a number as an array of bits in order to be able to use it as a lattice. The could be no such. Consider X and Y represented by packed chain codes or character strings. They might have different length so that you simply won't be able to XOR their memory patterns. Yet +/- will perfectly work. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 21:48, 24 March 2009 (UTC)
:::Maybe both could be shown in the same task of "Integer swap without a buffer". Bitwise XOR could be used where possible and modulus addition/subtraction (or whatever its formal name is..."ring addition"?) could be used for all examples. Do you think that's possible? --[[User:Mwn3d|Mwn3d]] 21:53, 24 March 2009 (UTC)
::::I think it is a good idea. +/- works on true integers (e.g. BigNum) and on modular integers (ring). XOR works for anything that has a fixed size contiguous representation as an array of bits (lattice). --[[User:Dmitry-kazakov|Dmitry-kazakov]] 14:22, 25 March 2009 (UTC)
::::: Actually it doesn't need to be a ring (therre's no multiplication involved). It works as is on every additive abelian group. It is easily extended to non-abelian groups: Using generic names for the group operation (op(x,y) is x+y for additive groups, x*y for multiplicative groups) and inversion (inv(x) is -x for additive, x<sup>-1</sup> for multiplicative groups), the operation is
:::::: x := op(x, y)
:::::: y := op(x, inv(y))
:::::: x := op(inv(y),x)
::::: Note that bitwise XOR also forms a group with neutral element 0 (0 XOR x = x XOR 0 = x) and the inverse of an element being equal the element itself (x XOR x = 0). --[[User:Ce|Ce]] 15:46, 25 March 2009 (UTC)

== Discussion in a VP talk page? ==

So we're talking.  In the talk page.  Of an article in a namespace whose entire purpose is to foster communication in its articles.  Perhaps the material in this talk page ought to be moved to the page proper? --[[User:Short Circuit|Short Circuit]] 02:09, 26 March 2009 (UTC)

: Well, actually the article doesn't really contain talk content. So maybe the right solution would be to move it from the village pump into the Rosetta Code namespace. --[[User:Ce|Ce]] 08:39, 27 April 2009 (UTC)
:: Actually, the Village Pump was intended to be its own namespace.  I haven't taken the time to get MediaWiki to recognize that, though... --[[User:Short Circuit|Short Circuit]] 10:55, 27 April 2009 (UTC)
::: Well, you misunderstood my point: The VP is, as you said, dedicated to communicating. But the article isn't really a communication page, therefore I think it's not correctly located in the VP, but should be in the RP namespace instead. That would not change if VP were a "real" namespace (indeed, I didn't yet even notice that it isn't) --[[User:Ce|Ce]] 12:08, 27 April 2009 (UTC)
:::: The VP serves as a sort of communal blog; Calls for content (what the request a task page was supposed to be for, but things that go there tend to get ignored) don't seem to be inappropriate, in that sense.  I need to look at replacing the "Request a task" function with a script page that feeds into a bot that will do something like post into a VP page.  That might help clarify things a bit. --[[User:Short Circuit|Short Circuit]] 16:55, 27 April 2009 (UTC)

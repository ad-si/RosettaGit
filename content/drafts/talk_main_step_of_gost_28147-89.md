+++
title = "Talk:Main step of GOST 28147-89"
description = ""
date = 2012-10-06T11:30:31Z
aliases = []
[extra]
id = 12275
[taxonomies]
categories = []
tags = []
+++

==task specifics==
Hi,
* What is goal of the task?
* How does one know one has completed the task?
* Do you have sample tests and sample output?

- Thanks. --[[User:Paddy3118|Paddy3118]] 20:22, 31 August 2012 (UTC)

: Hi!
:* The goal of the task is the comparison of programming languages for the implementation of some specific cryptographic algorithm, originally designed for efficient execution on 32-bit and 64-bit machines (though created in the 70's), while it operates with a 4-bit block; comparison of low-level with high-level languages in this aspect for the sake of brevity and clarity of code execution speed, consumption of computer resources and so on.
:* The task can be considered solved if the algorithm is implemented only true converts the input data.
:* Verification example can be used as follows:
::the input is a text block size of 8 bytes:
:: <code>[21][04][3B][04][30][04][32][04]</code>;
::and item key, size 4 bytes:
:: <code>[F9][04][C1][E2]</code>.
::Incoming text block is divided into two parts:
:: <code>[21][04][3B][04]</code> и <code>[30][04][32][04]</code>.
::First we add modulo 2<sup>32</sup> with the received key element, the result is
:: <code>[1A][09][FC][E6]</code>.
::Further, in this block is replaced with the replacement table. Are replaced by:
:: <code>[A]</code> to <code>[1]</code>, <code>[1]</code> to <code>[B]</code>, <code>[9]</code> to <code>[F]</code>, <code>[0]</code> to <code>[7]</code>, <code>[C]</code> to <code>[0]</code>, <code>[F]</code> to <code>[E]</code>, <code>[6]</code> to <code>[5]</code> and <code>[E]</code> to <code>[8]</code>.
::Finally, we obtain
:: <code>[B1][7F][E0][85]</code>.
::Next is the cyclic shift of reading towards senior level by 11 bits:
:: <code>[2F][8C][FD][03]</code>.
::This value is made up bit by bit modulo 2 with the second part on the input 8-bit block, the result is
:: <code>[1F][88][CF][07]</code>.
::Now is 8-byte block of data in the first part of which is located just obtained value, and the second - the first part was on the input 8-byte block. The resulting value
:: <code>[1F][88][CF][07][21][04][3B][04]</code>
::and will be the result of the step.
: [[User:Русский|Русский]] 18:54, 1 September 2012 (UTC)

Is there a ''definitive'' link to the definition of the algorithm? It sounds like it's something controlled by an external body, so a definitive link (or reference to paper version, of course) ought to be possible. (If the link is to an English-language version, so much the better, but being definitive is better than being readable to me.) I ask because while I could implement the task by copying an existing solution (the C version looks easy enough to copy), I would have no idea at all whether all I'd be doing is copying their mistakes! I love to be able to independently check the correctness of any code I write. –[[User:Dkf|Donal Fellows]] 10:10, 7 September 2012 (UTC)
: Yes, of course, a reference to the text of the standard, some of which I have implemented the proposed algorithm for the job, I have: [http://gostshifr.narod.ru/gostpdf.pdf]. This is a scanned version of the paper edition of the standard. But, unfortunately, I have it is only in Russian, but in the annexes have a pretty clear flowchart. Here is a diagram of the basic step: [http://traditio-ru.org/images/d/d6/ОсновнойШаг28147.PNG]. [[User:Русский|Русский]] 13:13, 8 September 2012 (UTC)
: As you can see, I have given a description of two embodiments of the algorithm: the classical and variant with a compressed table changes. The latter has traditionally been used in implementations on low-level languages, and the first is more beautiful and obvious, therefore, in my opinion, it is better to start with him (option for "JavaScript"). [[User:Русский|Русский]] 13:17, 8 September 2012 (UTC)
:: Ah, cool! Wikipedia has some relevant links. (I didn't know it was a Feistel network…) –[[User:Dkf|Donal Fellows]] 13:16, 13 September 2012 (UTC)
I attempted to follow the verification example above.  I started with the C code, made some modifications to get the intermediate results in the example, and was able to reproduce the the first 32 bit part of the result.  The instructions for producing the second part are obscure though, and I was unable to reproduce the rest of the result in the example.  Can these instructions be clarified?  &mdash;[[User:Sonia|Sonia]] 19:09, 20 September 2012 (UTC)

Sonia, you are almost there. The second part of the output is actually just a copy of the other part of the input. See http://en.wikipedia.org/wiki/Feistel_cipher or my Perl 6 solution.
&mdash;[[User:Edwin|Edwin]] 13:30, 6 October 2012

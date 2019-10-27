+++
title = "Talk:Image noise"
description = ""
date = 2010-11-29T14:42:02Z
aliases = []
[extra]
id = 8376
[taxonomies]
categories = []
tags = []
+++

==What are we supposed to be doing here?==

Are we supposed to be finding good hardware and tuning the random number generator to jack up fps? --[[User:Rdm|Rdm]] 11:57, 2 October 2010 (UTC)

:It could be hard to get a constructive use of the FPS here, even if dividing the examples in to compiled/interpreted, Intel/AMD/etc., GPU X/Y/Z… Still the focus would be more on performance/hardware than the languages. I would like to suggest that the examples better fix the FPS a low value (1 FPS?) which should be of for most – no matter hardware. And then also, if possible, includes this 1-sec synchronization. --[[User:Jofur|&lt;Jofur&gt;]] 12:51, 2 October 2010 (UTC)

:: I get better than 1 fps with (by modern standards) modest hardware running Tcl. I'm not putting in my figures though; they're highly hardware-dependent. –[[User:Dkf|Donal Fellows]] 16:04, 2 October 2010 (UTC)

Hardware information is not very important. It can be removed. It's just a reference. Improving FPS is always good. 30 FPS is enough to create a smooth animation. Any example running more than 30 FPS in most hardware is good, hence the optimization. --[[User:Guga360|Guga360]] 16:14, 2 October 2010 (UTC)

: But why is that information requested anyway? Inter-language performance differences have never been the point of RC… –[[User:Dkf|Donal Fellows]] 16:19, 2 October 2010 (UTC)
::OK. That's true. FPS is very hardware-dependent. We could test all examples in only 1 computer. Perfomance differences are not really point of RC. But we can still compare language perfomance, I think it's a good idea. --[[User:Guga360|Guga360]] 16:34, 2 October 2010 (UTC)
:::I still think the FPS should be rethought; Do you really want to set up the test on one computer; will then install 12 different C-compilers, LabView, etc.? Learning all compiler flags, libraries, etc?
:::: For me, no problem. I can run Windows/Linux VMs, download trial/free compilers/interpreters and benchmark. --[[User:Guga360|Guga360]] 22:48, 2 October 2010 (UTC)

:::For me RC is more about the languages and in this task compiled languages that fits nicely into the CPU’s cache (ex. PureBasic, C++, etc.) will excel, while in a task where we communicate with a web-server and spend ~99.99% of the time waiting all languages will be equal fast. If performance should be a part of RC it should in some way be standardized before so that we have one common way to compare it – bad data is often much worse the no data. --[[User:Jofur|&lt;Jofur&gt;]] 16:56, 2 October 2010 (UTC)
:::: I'm inclined to agree; performance numbers are only meaningful when the underlying operating platforms are comperable. On the topic of performance comparison, I wouldn't be averse to hosting code speed tests, but that would have to be backed by something like Amazon EC2, would still represent a massive amount of administrative overhead, and I'd have to be getting paid by RC to afford the personal and computer time. In other words, not likely to happen any time soon. --[[User:Short Circuit|Michael Mol]] 21:15, 2 October 2010 (UTC)
:::: I think there is no benchmark "standards". Every example is very different. --[[User:Guga360|Guga360]] 22:48, 2 October 2010 (UTC)

Don't worry about performance comparisons between languages. Put the code for FPS in there so people can see how to do it. Don't use it to say any example is better than another. --[[User:Mwn3d|Mwn3d]] 21:02, 2 October 2010 (UTC)
: What are we counting? Frames generated, or frames drawn? If frames generated, then that kind of show-how would probably be better done in a different task. (Actually, I think I created that task once before. Don't remember which it is.) --[[User:Short Circuit|Michael Mol]] 21:15, 2 October 2010 (UTC)

==The Python example doesn't seem to be working==

```txt

$ python noise.ml
  File "noise.ml", line 24
    self.img.putdata([(255,255,255) if random.random() > 0.5 else ( for i in range(pixels)])
                                                                      ^
SyntaxError: invalid syntax

$ python
Python 2.6.5 (r265:79063, Jul 14 2010, 13:26:04) 
[GCC 4.4.3] on linux2

```

:Oops. Fixed. 
:self.img.putdata([(255,255,255) if random.random() > 0.5 else ('''0,0,0)''' for i in range(pixels)])

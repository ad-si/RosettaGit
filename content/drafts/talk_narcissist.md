+++
title = "Talk:Narcissist"
description = ""
date = 2012-10-17T21:23:57Z
aliases = []
[extra]
id = 8344
[taxonomies]
categories = []
tags = []
+++

I will note that while input has been defined, output has not, nor has the bounds of the input.  Since the first implementation was in C, I would have expected a success/fail exit status to be the output, but apparently the output is meant to be the character '1' or '0' on stdout?  Anyways, that said, I wanted to also point out that this I/O specification is heavily tailored to the C environment (or the unix environment, but unix and c were invented together), and is nearly nonsensical for other languages (such as SQL).  And, from this, we can deduce that the input is probably meant to be terminated by end of file... --[[User:Rdm|Rdm]] 15:59, 22 September 2010 (UTC)
:I think that printing a literal "0" or "1" was probably the original intent, something like:
 $ narcissist narcissist.src
 1
 $ narcissist some_other_file
 0
:But I also think that perhaps the original definition is too restrictive; rather than ''specifically'' returning "0" or "1" or "accept" or "reject", I think it would be better to use whatever makes sense for the chosen language. For example, the ALGOL 68 example returns "T" or "F". -- [[User:Eriksiers|Erik Siers]] 16:41, 22 September 2010 (UTC)
:: Note that the above invocations seem to violate the specification (since they would require reading a file from disk instead of from standard input).  A command line which would not violate the specification would probably look like this:  <code>narcissist <narcissist.src</code>  --[[User:Rdm|Rdm]] 17:18, 22 September 2010 (UTC)
:::Yeah, thought of that, but :shrug:. Incidental to the output side, methinks. :-)
:::Anyway, I vote for loosening the task specification somewhat, in the manner I said above. -- [[User:Eriksiers|Erik Siers]] 17:46, 22 September 2010 (UTC)

==Using $0==

Using the representation of the code in the file system or in memory, with $ARGV[0] or equivalent, as done in the python solution, is cheating imho.  It should be explicitly excluded in the instructions.--[[User:Grondilu|Grondilu]] 21:23, 17 October 2012 (UTC)

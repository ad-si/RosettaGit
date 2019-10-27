+++
title = "Talk:Array concatenation"
description = ""
date = 2010-02-06T12:27:43Z
aliases = []
[extra]
id = 4837
[taxonomies]
categories = []
tags = []
+++

I've noticed someone turned my char * into void *; it must be noted that turning on the option <tt>-pedantic</tt> in gcc, a (to me, logical) warning is raised: pointer of type 'void *' used in arithmetic (for a 'void *', it makes few sense to write, say, 'p+10'... we need to know the "size" of what is pointed to). To me, interpreting 'char *' just as a pointer to byte (the "smallest" data unit, suitable for everything), is better since in fact we want to copy bytes (no matter what they mean), while 'void *' tells just it is a pointer, but does not tell how to cope with the data pointed; so to fix the warning we should add anyway a cast to 'char *', perform our arithmetic, and recast it into 'void *' (which is ok for memcpy and can be left implicit). Just to let people know. --[[User:ShinTakezou|ShinTakezou]] 14:39, 12 September 2009 (UTC)
:I've changed the arithmetic-ed pointer to char *. --[[User:Kevin Reid|Kevin Reid]] 16:21, 12 September 2009 (UTC)

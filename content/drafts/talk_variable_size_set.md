+++
title = "Talk:Variable size/Set"
description = ""
date = 2015-08-12T16:34:52Z
aliases = []
[extra]
id = 3243
[taxonomies]
categories = []
tags = []
+++

I'm not sure quite what this is asking.  It sounds like declaring a specific size for a variable, such that the implementation is required to make it at least that big, but the programs so far seem to be focused on making 0- or 1-bit data types.  Clarification?

:Indeed, the descriptions is vague. I agree that the purpose is probably to declare a variable that contains at least so many bits, so that the programmer can be sure that the data will fit in the variable. The C example seems to be the only one that implements the required task. Others seem to be attempting to specify as small variable as possible.
:On the other hand, in most languages it is not necessary to specify the minimum size, since the size of each type is always known. Specifying minimum size is only needed if the size of a type (for example ''int'') is different in different implementations (as it is in C). --[[User:PauliKL|PauliKL]] 15:53, 25 May 2009 (UTC)

::Even the C example does not seem to address the task specification -- it only shows how to define a variable with one size.  If defining a variable with one size is sufficient then this problem becomes trivially easy in any language that allows you to define variables. --[[User:Rdm|Rdm]] 18:08, 17 February 2010 (UTC)

== Delete this task? ==

Should this task be deleted?

Or, clarified?

How can we tell if an implementation has satisfied the task requirement? Why doesn't the task description include any required examples? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:34, 12 August 2015 (UTC)

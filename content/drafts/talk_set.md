+++
title = "Talk:Set"
description = ""
date = 2011-10-24T07:39:45Z
aliases = []
[extra]
id = 10712
[taxonomies]
categories = []
tags = []
+++

==Lists as sets in common lisp==
The common lisp entry says it forms sets from lists. Does it handle duplicates correctly? Does it ignore order i.e does the set 1,2,3,4 equal the set 3,2,4,1 when the lists that they are composed from has the elements originally in those different orders? I cannot tell from the examples given. --[[User:Paddy3118|Paddy3118]] 18:31, 23 October 2011 (UTC)
: No, it doesn't do either.  As far as I can tell (speaking out of my week and half CL expertise, so take that with a grain of salt), the set operators in Common Lisp are more of a convenience than a serious application.  Duplicate elements can be removed by <code>remove-duplicates</code>, and adding elements can use <code>adjoin</code> which is no-op if element is already in set.  The normal list equality test (likely <code>equal</code>) will not ignore order, that's why I used subset tests in the example. --[[User:Ledrug|Ledrug]] 18:42, 23 October 2011 (UTC)

:according to [http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial-24.html] the set functions assume no duplicates. duplicates may potentially lead to an error.
:instead of lists, hash-tables could be used or a library like [http://common-lisp.net/project/fset/ FSet].
:[http://common-lisp.net/project/cl-containers/ CL-Containers] also contains a set type.--[[User:EMBee|eMBee]] 07:39, 24 October 2011 (UTC)

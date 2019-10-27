+++
title = "Talk:Empty directory"
description = ""
date = 2012-01-03T18:26:55Z
aliases = []
[extra]
id = 11133
[taxonomies]
categories = []
tags = []
+++

== Things to watch for ==

If anyone's writing a solution, be aware that not every directory contains <code>..</code>. Root directories don't and they can be empty (except for <code>.</code>). Good solutions will take care of this case (which admittedly is only practically going to happen on Windows; Unix has a single root and that's unlikely to ever be empty in practice given that you're running some code at all). –[[User:Dkf|Donal Fellows]] 10:34, 3 January 2012 (UTC)

:Every root directory I have ever seen contains <code>..</code> -- in root directories, <code>..</code> and <code>.</code> are the same.  --[[User:Rdm|Rdm]] 18:25, 3 January 2012 (UTC)

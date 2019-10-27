+++
title = "Talk:Singleton"
description = ""
date = 2010-02-06T13:28:41Z
aliases = []
[extra]
id = 3208
[taxonomies]
categories = []
tags = []
+++

Is the C++ thread-safe solution correct? I don't know the MS API, but to me it looks as if the code creates a new mutex each time it executes the code. However, the mutex locking can only work if both threads lock the same mutex. --[[User:Ce|Ce]] 13:29, 5 December 2008 (UTC)

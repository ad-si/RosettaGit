+++
title = "Category:AmbientTalk"
description = ""
date = 2011-05-09T19:35:53Z
aliases = []
[extra]
id = 9575
[taxonomies]
categories = []
tags = []
+++

{{language|AmbientTalk
|strength=strong
|safety=safe
|express=explicit
|compat=duck
|checking=dynamic
|parampass=value
|gc=yes
|LCT=yes}}
AmbientTalk is a concurrent, distributed programming language designed specifically for mobile ad hoc networks. It's concurrency model is inspired by actors, and more specifically the event loop concurrency model of the [[E]] language. AmbientTalk has built-in support for distributed service discovery and (asynchronous) messaging.

The language was designed at the Software Languages Lab of the Vrije Universiteit Brussel, primarily as a research framework for exploring new concurrent and distributed language features for highly dynamic, decentralized networks, such as wireless, mobile ad hoc networks.

The default implementation of AmbientTalk is a Java interpreter, which features full interoperability with the JVM, enabling AmbientTalk code to use existing Java libraries, and enabling Java code to use AmbientTalk as a distributed scripting language. Recent version of AmbientTalk also run on the Android OS, making AmbientTalk portable across a wide range of mobile phones.

==Trying AmbientTalk==

[http://ambienttalk.googlecode.com The open source AmbientTalk interpreter]

To run an AmbientTalk program:

 
```sh
$ iat program.at
```


To get a [[REPL]] (aka prompt, shell, interactive interpreter):

 
```sh
$ iat
```


==See Also==

* [http://ambienttalk.googlecode.com Open source google code project]
* [http://soft.vub.ac.be/amop AmbientTalk home page] at the Software Languages Lab of the Vrije Universiteit Brussel.

+++
title = "BusyBox"
description = ""
date = 2014-11-21T15:14:09Z
aliases = []
[extra]
id = 18208
[taxonomies]
categories = []
tags = []
+++

[[Category:AWK Implementations]]
[[Category:UNIX Shell Implementations]]
[[Category:Editor]]
[[Category:Utility]]

[[wp:BusyBox|BusyBox]] "The Swiss Army Knife of Embedded Linux"
is a multiuse-utility, designed for embedded Linux-systems:

:BusyBox combines tiny versions of many common UNIX utilities into a single small executable. 
:It provides replacements for most of the utilities you usually find in GNU fileutils, shellutils, etc. 
<!--
The utilities in BusyBox generally have fewer options than their 
full-featured GNU cousins; however, the options that are included provide 
the expected functionality and behave very much like their GNU counterparts. 

BusyBox provides a fairly complete environment for any small or embedded system.

BusyBox has been written with size-optimization and limited resources in mind. 
It is also extremely modular so you can easily include or exclude commands 
(or features) at compile time. This makes it easy to customize your embedded systems. 
-->
A working system may consist of just
a Linux kernel,
some device nodes in /dev, 
a few configuration files in /etc, 
BusyBox,
and maybe a bootmanager.

For example, BusyBox is used in [http://distro.ibiblio.org/tinycorelinux Tiny Core Linux].

BusyBox can provide most of the functionality of the many programs 
typical found in /bin, /sbin, /usr/bin, /usr/sbin, /usr/local/bin,
all in a single binary, thus saving space on small systems.

As a shell, BusyBox provides [[Almquist Shell‎|ash]].

As a calculator, BusyBox provides [[dc]], a "Tiny RPN calculator".

As an editor, BusyBox provides [[sed]] and [[Vi]].

BusyBox can be configured (at compile-time) to include 
as little or as much "applets" as needed/desired.

[http://www.busybox.net BusyBox] can be compiled to include an [[AWK]]-implementation.

+++
title = "Tclkit"
description = ""
date = 2014-11-13T22:06:34Z
aliases = []
[extra]
id = 4241
[taxonomies]
categories = []
tags = []
+++
'''tclkit''' and '''tclkitsh''' are single-file distributions of Tcl (based on [libtcl](https://rosettacode.org/wiki/libtcl)[Category:libtcl](https://rosettacode.org/wiki/Category:libtcl)) that can be launched without prior installation; they store all auxiliary files in an internal filesystem-in-a-database. The main difference between tclkit and tclkitsh is on Windows, where the former is built as a graphical application (so not using real stdio) and the latter as a console application (forcing the opening of a console window if run from Explorer); these differences are a feature of the Windows platform. (The difference between <tt>java</tt> and <tt>javaw</tt> for [Java](https://rosettacode.org/wiki/Java) runtimes is analogous.)

Tools are available for simply building user applications into single file distributions based on tclkit, through concatenating a further filesystem-database file. Since this technique is orthogonal (and largely transparent) to the application, it has become a very popular technique for distributing applications implemented in Tcl.

## Building an Application with Tclkit
Note that unlike the [http://wiki.tcl.tk/10558 source for this material], this quick tutorial assumes that you already have things installed.


###  Write your application 

Save this into a file <tt>hello.tcl</tt>:

```tcl
package require Tk
pack [button .b -text "Hello World!" -command bell]
```


###  Wrap your application 

At the shell command line, type this:

```bash
SDX="tclkit sdx.kit"
$(SDX) qwrap hello.tcl
```


###  Test your application 


```bash>tclkit hello.kit</lang


###  Unwrap and rewrap with the full runtime attached 


```bash
$(SDX) unwrap hello.kit
```

That creates a directory, <tt>hello.vfs</tt>, that contains the packaged application. This is when you would copy into there any libraries – either pure scripted or compiled from some other language, e.g., [[C]] – that you wanted to distribute with your application. Then you rewrap it into a full, self-contained executable like this:

```bash
$(SDX) wrap hello -vfs hello.vfs -runtime `which tclkit`
```


## See Also
* [http://www.equi4.com/tclkit/ Description, fully-supported builds]
** [http://www.equi4.com/starkit/sdx.html Tool for building and working with tclkit-packaged applications, <tt>sdx.kit</tt>]
* [http://www.patthoyts.tk/tclkit/ Bleeding-edge builds]
* [http://wiki.tcl.tk/52 Tclkit on the Tcler's Wiki]
* [http://wiki.tcl.tk/3661 Starkit on the Tcler's Wiki]

+++
title = "Tclsh"
description = ""
date = 2010-09-01T10:26:16Z
aliases = []
[extra]
id = 1581
[taxonomies]
categories = []
tags = []
+++

{{implementation|Tcl}}'''tclsh''' is a [[wp:Shell (computing)|shell]] for the [[Tcl]] programming language. It is the primary implementation of Tcl as it is distributed with, built with, and a thin wrapper around [[libtcl]][[Category:libtcl]], and it is normally used in command-line environments. In graphical environments, it is more common to use [[wish]] instead, though this is a historical hold-over except on [[Windows]].

== Argument Handling ==
The tclsh shell only supports a single option, <code>-encoding</code>, which is used to specify the character encoding used for the Tcl script that it will evaluate. The first argument after that, or the first argument if no <code>-encoding</code> option is present, is the name of a Tcl script that will be executed; when the script finishes, the process will exit. All subsequent arguments are stored as a list in the global <code>argv</code> variable, and the length of the list is (redundantly) in the global <code>argc</code> variable; the name of the script itself is in the global <code>argv0</code> variable, and the full name of the tclsh program itself is retreivable with the <code>info nameofexecutable</code> command. (Note that most other Tcl-based shells adopt the same naming standards for command line arguments.)

When tclsh is started without any arguments, it sources the file <code>~/.tclshrc</code> (or <code>~/tclshrc.tcl</code> on Windows) and subsequently runs in interactive mode, allowing you to type in Tcl commands and have them executed immediately.

== Idiomatic Demonstration ==
One trick that is used when providing a file that can either be a <code>source</code>d script or run by passing it directly to tclsh as its first argument is to compare the contents of the global <code>argv0</code> variable with the result of the <code>info script</code> command; when these are the same, the file being evaluated is the main script of the whole process and can perform any additional steps necessary (e.g., by running a demonstration). This might be done like this:


```tcl
proc foo ...
proc bar ...

if {$argv0 eq [info script]} {
    puts "foo x = [foo x]"
    puts "bar y = [bar y]"
}
```


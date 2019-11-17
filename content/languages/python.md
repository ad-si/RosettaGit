+++
title = "Python"
description = ""
date = 2018-04-09T03:22:33Z
aliases = []
[extra]
id = 1696
website = "http://www.python.org"
[taxonomies]
categories = []
tags = ["garbage-collected", "interpreted", "dynamic", "object-oriented"]
+++

From the official [http://www.python.org Python] website:

> Python is a programming language that lets you work more quickly
> and integrate your systems more effectively.
> You can learn to use Python and see almost immediate gains in productivity
> and lower maintenance costs."

It is easy to create clean bug-free programs in Python due to the motto:
"Errors should never pass silently."
Python is an [[wp:Interpreter (computing)|interpreter]].
Python source files (.py files) are typically compiled
to an intermediate [[bytecode]] language (.pyc files)
and executed by a Python Virtual Machine.


### Notes

Some Python examples may deviate from idiomatic Python
because they may be written to work
in Python 3.X as well as Python 2.X environments.
This includes doing things like:

- Using brackets in print statements/functions of one expression.
- Using zip and not izip; keys(), values(), items() and not their iter- forms.
- Checking for raw_input and setting raw_input to input if not found.
- Conditionally importing reduce if it is not found.

This style is not a requirement for Python code on RosettaGit,
but it may be in use
and should not necessarily be 'corrected' if found in examples.

The command line is often used in Python for short pieces of code,
and so, again; examples showing such use are not incorrect and may be left.


## See Also

- [[wp:python_(programming_language)|Wikipedia: Python]]


## Todo

[[Reports:Tasks_not_implemented_in_Python]]

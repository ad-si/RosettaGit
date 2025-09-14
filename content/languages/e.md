+++
title = "E"
description = ""
date = 2014-03-15T19:24:40Z
aliases = []
[extra]
id = 2006
[taxonomies]
categories = []
tags = []
+++

E is a programming language designed around object-capability security and event-loop concurrency,
in order to support creation of highly robust and secure programs.

Using the [Java](https://rosettacode.org/wiki/Java) implementation of E,
Java libraries can be used from E code, and its REPL
(e.g. [Creating a Window](https://rosettacode.org/wiki/Creating_a_Window#E)).


## Trying E

[http://www.erights.org/download/ The current recommended E implementation ("E-on-Java") may be downloaded from erights.org].

To run an E program:

```sh
$ rune program.e
```


To get a [REPL](https://rosettacode.org/wiki/REPL) (aka prompt, shell, interactive interpreter):

```sh
$ rune
```


An online REPL is also available at [Rosetta Code IRC](https://rosettacode.org/wiki/Help:IRC) or `[irc://chat.freenode.net/erights #erights]` on `chat.[http://freenode.net/ freenode.net]`.


## Syntax of examples

While most of the examples on Rosetta Code are E expressions (programs), some may be written like this:

 ? ''expression''
 # value: ''print representation''

This is both the format of a transcript at an E [REPL](https://rosettacode.org/wiki/REPL), and the format employed by [http://wiki.erights.org/wiki/Updoc Updoc], a test framework for E. “?” is a prompt for user input (“&gt;” indicates multi-line input) and “# foo:” indicates responses.

- `# value:` the return value of the ''expression'', printed
- `# problem:` an exception thrown by evaluation of the ''expression''
- `# syntax error:` an exception thrown by parsing of the ''expression''
- `# stdout:` or `# stderr:` text written to the `stdout` or `stderr` streams.
    It is typically only used in test scripts and not in ordinary interactive sessions.

To try out these examples for yourself, just install E and run the `rune` command to get the “?” prompt.
Multi-line input is automatic for unbalanced brackets/parens and can be indicated in other cases by a trailing backslash.


## See Also

* [E Wiki](http://wiki.erights.org)

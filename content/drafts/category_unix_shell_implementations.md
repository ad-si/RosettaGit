+++
title = "Category:UNIX Shell Implementations"
description = ""
date = 2015-10-24T20:18:47Z
aliases = []
[extra]
id = 2248
[taxonomies]
categories = []
tags = []
+++

{{implementation cat|UNIX Shell}}

There are many [[UNIX Shell]]s and most of them belong to two families. For purposes of the Rosette Code, all examples are in Bourne-compatible syntax. The other family of shells, with a markedly different syntax, are ''csh'' and its ''tcsh'' (Tenex C Shell) "clone."

== Bourne-compatible shells ==
Common Bourne compatible shells include

* the original [[Bourne Shell]] (''/bin/sh'' on most versions of [[UNIX]]),
* the [[GNU]] [[Bourne Again SHell]] (''bash'' --- which is linked to ''/bin/sh'' on many distributions of [[Linux]], making it their default shell),
* the [[Korn Shell]] (''ksh''),
* the [[Public Domain Korn SHell]] (''pdksh'' --- which is ''/bin/ksh'' on some systems, ''/bin/sh'' on [[OpenBSD]]),
* the [[MirBSD Korn Shell]] (''mksh'' -- which is ''/bin/sh'' on some systems)
* the [[Almquist SHell]] (''ash'' --- which is ''/bin/sh'' on some systems),
* the [[Debian Almquist SHell]] (''dash''),
* and the [[Z SHell]] (''zsh'').


###  Comparison table 

{| class="wikitable"
! Feature
! Bourne ''sh''
! ''ash, dash''
! ''pdksh, mksh''
! ''bash''
|-
! Manual page
| [http://heirloom.sourceforge.net/sh/sh.1.html Heirloom sh]
|
* [http://netbsd.gw.com/cgi-bin/man-cgi?sh++NetBSD-current NetBSD sh]
* [http://man.cx/dash dash]
|
* [http://www.openbsd.org/cgi-bin/man.cgi?query=ksh&apropos=0&sektion=0&manpath=OpenBSD+Current&arch=i386&format=html OpenBSD ksh]
* [http://www.mirbsd.org/htman/i386/man1/mksh.htm mksh]
| [http://man.cx/bash bash]
|-
| <tt> `''command''`</tt>
| {{yes}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>''func''() { ''list''; }</tt>
| {{yes}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>[ -n "$''param''" ]</tt>
| {{yes}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>''PARAM''=''value''<br />export ''PARAM''</tt>
| {{yes}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>export ''PARAM''=''value''</tt>
| {{no}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>local ''param''</tt>
| {{no}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>${''param''##*/}<br />${''param''%/*}</tt>
| {{no}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>ls ~</tt>
| {{no}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>$(''command'')</tt>
| {{no}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>$(( ''i = 2 + 3'' ))</tt>
| {{no}}
| {{yes}}
| {{yes}}
| {{yes}}
|-
| <tt>(( ''i = 2 + 3'' ))</tt>
| {{no}}
| {{no}}
| {{yes}}
| {{yes}}
|-
| <tt><nowiki>[[</nowiki> -n $''param'' ]]</tt>
| {{no}}
| {{no}}
| {{yes}}
| {{yes}}
|-
| <tt>function ''name'' { ''list''; }</tt>
| {{no}}
| {{no}}
| {{yes}}
| {{yes}}
|-
| <tt>${''array''[''2'']}</tt>
| {{no}}
| {{no}}
| {{yes}}
| {{yes}}
|-
| <tt>set -A ''array 11 22 33''</tt>
| {{no}}
| {{no}}
| {{yes}}
| {{no}}
|-
| <tt>''array''=(''11 22 33'')</tt>
| {{no}}
| {{no}}
| {{optional|mksh}}
| {{yes}}
|-
| <tt>$' \t\n'</tt>
| {{no}}
| {{no}}
| {{optional|mksh}}
| {{yes}}
|}


###  Portability notes 

The original Bourne shell went through a number of revisions in the early years of UNIX, and support for some features varies considerably.  By the time the SUSv3 (Single Unix Specification, version 3) features stabilized, all versions of the various Bourne-compatible shells should support a common set of features. This is denoted in Rosette Code examples with the phrase: "SUSv3" features. The Korn shell (originally written by David Korn of AT&T) and its "public domain" clone offer extensions (such as co-processes, and "associative arrays" --- called "hash arrays" by [[Perl]], "dictionaries" by [[Python]], "maps" by [[Lua]], etc).

Note that even when using a common subset of supported features there are subtle implementation differences, and, in some cases, parsing bugs, which can affect the portability of shell script examples. For example in ''bash'' versions before 2.0 the following was tolerated:

 { echo foo; echo bar } ## Bug!!!

... though this is technically a bug in the language parsing (The braces used for command grouping are not delimiters in the same class as semicolons nor parentheses; so this example is ambiguous because ''echo }'' (outside of any command grouping) should work the same as ''echo "}"'' --- but in bash versions 1.x it behaves inconsistently). In ''bash'' versions newer than 2.0 this was fixed and the following is required:

 { echo foo; echo bar; } ## Note the required semicolon

... (Or the ''}'' token can be put on a separate line)

Variations of this bug probably account for more "breakage" during upgrades of ''bash'' and when attempting to run ''bash'' scripts under other Bourne compatible shells than any other change in the history of Bourne-compatible shells.

Another common portability issue among different Bourne-compatible shells is a subtle matter of how pipe operations are handled. In all normal UNIX shells the '''''|''''' (pipe) operator creates a unidirectional inter-process communications (IPC) stream between one shell process and another. Thus a command like:

 echo foo | read bar

... implicitly invokes a subshell (separate process) as either the producer or the consumer (writer into or reader from) this data "pipe."

The crucial difference in semantics is determined by whether a given implementation of a shell creates the subshell/sub-process to the left or the right of the pipe operator. (Conceivably a shell could even create subprocesses on both sides of the operator). To demonstrate, and even test for, the difference run the following lines of code:

 unset bar; echo "foo" | read bar; echo "$bar"

... shells such as ''ksh'' and ''zsh'' spawn their subshells to the left of the pipe ... so the sub-process is writing into the pipeline. This means that the existing process is reading values; thus the local shell variable "bar" is set after the second semicolon in this example. Under shells such as ''bash,'' ''ash,'' ''pdksh'' (and even in older versions of ''ksh'') the subshell is spawned on the right of the ''|'' operator.  In those cases the ''read'' command is setting a value to a shell variable which ceases to exist after the second semicolon (which marks the end of that command, and thus the end of the completed sub-process.

To be portable such code must use command grouping:

 unset bar; echo "foo" | { read bar; echo "$bar"; ...; }

... so that all of the commands after the pipe are executed within the same subshell.

Alternatively one could use an explicit shell sub-process (using the "parentheses" delimiters in lieu of the "brace" grouping operators), or one could re-structure the code using assignment and command substitution:

 unset bar; bar=$(echo "foo"); echo "$bar"  # some very old shells may require ` (backticks) instead of the $(...) syntax

Note that in all these examples the ''unset bar'' command is simply to avoid any confusion in the unlikely event that a variable named "bar" was present in the shell environment or local variable heap prior to our functional examples. This sort of difference, the implicit creation and scope of subshells and subproceses, and the underlying conceptual distinctions between shell and environment variables are at the root of many shell scripting portability issues and cause most of the confusion experienced by novices to UNIX shell scripting.

==Comparison of various shells==
[http://www.math.iitb.ac.in/resources/manuals/Unix_Unleashed/Vol_1/ch13.htm An excerpt] from "UNIX Unleashed, System Administrator's Edition", has a decent discussion of how to choose a shell. The article focuses on three areas: command line usage, shell scripting for personal use and shell scripting for others to use.

+++
title = "Category:OCaml Calendar Library"
description = ""
date = 2014-05-11T23:15:25Z
aliases = []
[extra]
id = 10834
[taxonomies]
categories = []
tags = []
+++

The OCaml Calendar Library is a library for handling dates and times in OCaml programs.

Homepage: http://calendar.forge.ocamlcore.org/

Project page: http://forge.ocamlcore.org/projects/calendar/

Opam package: http://opam.ocaml.org/packages/calendar/calendar.2.03.2/


###  How to use 


When this library is installed, one can run a script in interpreted mode with this command line:

 ocaml unix.cma str.cma -I +calendar calendarLib.cma script.ml

And to compile to native code:

 ocamlopt unix.cmxa str.cmxa -I +calendar calendarLib.cmxa prog.ml -o prog

Or using findlib:

 ocamlfind opt -package calendar -linkpkg prog.ml -o prog

To use it without installing it, one can open the source archive in <code>/tmp</code> for example, and then after compiling, just replace <code>+calendar</code> by <code>/tmp/calendar-2.03.1/target/</code> in the previous commands.

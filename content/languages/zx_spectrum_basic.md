+++
title = "ZX Spectrum Basic"
description = ""
date = 2011-03-28T19:48:01Z
aliases = []
[extra]
id = 8400
[taxonomies]
categories = []
tags = []
+++

'''ZX Spectrum Basic''' is the [BASIC](https://rosettacode.org/wiki/BASIC) built into the ROM of the ZX Spectrum Computer.

## Features
* Language is interpreted (but third party compiler is available).
* Line numbers are required
* Line numbers must be no longer than four digits
* No [named locations](https://rosettacode.org/wiki/named_locations)
* Some [Terminal Control](https://rosettacode.org/wiki/Terminal_Control) capabilities
* Supports [colour](https://rosettacode.org/wiki/colour)
* Support for direct display [graphics](https://rosettacode.org/wiki/graphics)
* Support for sound via the [speaker](https://rosettacode.org/wiki/speaker)
* No support for [scope](https://rosettacode.org/wiki/scope)d variables
* No support for [structure](https://rosettacode.org/wiki/structure)d variables
* No [command line parameter](https://rosettacode.org/wiki/command_line_parameter)s
* No support for [environment](https://rosettacode.org/wiki/environment)
* Supports output tho the [line printer](https://rosettacode.org/wiki/line_printer) via the [lprint](https://rosettacode.org/wiki/lprint) command
* No direct support for error handling, but error handling can be achieved using assembly language by changing the [error handler](https://rosettacode.org/wiki/error_handler) address
* Support for streams when using [Interface 1](https://rosettacode.org/wiki/Interface_1), but no support for [freefile](https://rosettacode.org/wiki/freefile)
* Can read keystrokes using <code>INKEY$</code>
* No support for multiple line <code>IF</code> conditionals
* No conditional compilation directives
* no [hashbang](https://rosettacode.org/wiki/hashbang) mechanism
* Variables have to be defined before use
* The GO TO command has a space in the middle
* VAL function interprets expressions
* INPUT function can interpret expressions (unless LINE INPUT is used)

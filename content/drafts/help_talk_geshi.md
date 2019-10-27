+++
title = "Help talk:GeSHi"
description = ""
date = 2009-12-07T21:45:25Z
aliases = []
[extra]
id = 2643
[taxonomies]
categories = []
tags = []
+++


###  Suggestion x 2

Hi!

Currently, it seems D's (also others, like Java, C etc.) syntax coloring high-light a Statement keyword by color #b1b100,eg:

<code><span style="color: #b1b100;"> return</span> <span style="color: #000000; font-weight: bold;">new</span> <span style="color: #993333;">int</span><span style="color: #66cc66;">&#91;</span><span style="color: #66cc66;">&#93;</span> ;</code>

It may be only me, it will be looked better if a darker color is used for Statement keyword, eg. that of <span style="color: #993333;">int</span> keyword (#993333):

<code><span style="color: #993333;"> return</span> <span style="color: #000000; font-weight: bold;">new</span> <span style="color: #993333;">int</span><span style="color: #66cc66;">&#91;</span><span style="color: #66cc66;">&#93;</span> ;</code>

or, in similar but darker color(#666600):

<code><span style="color: #666600;"> return</span> <span style="color: #000000; font-weight: bold;">new</span> <span style="color: #993333;">int</span><span style="color: #66cc66;">&#91;</span><span style="color: #66cc66;">&#93;</span> ;</code>

Besides, the origianl <code>&lt;pre></code> tag allowed attribute modifying, eg.<code>&lt;pre style="background-color:#ffe"></code>. But the syntax coloring tag will remove it, eg. <code>&lt;d style="background-color:#ffe"></code> => <code>&lt;pre class="d"></code>. It seems the ''class'' is redundant as coloring is already applied. It will be nice that the syntax coloring tag can be more flexible.

Thank you. -- [[User:Badmadevil|badmadevil]] 02:40, 29 February 2008 (MST)

Current syntax highlighting for D:

```D
return new int[] ;
```

: Looks a bit better, now that we've switched to class-based coloring.--[[User:Short Circuit|Short Circuit]] 21:45, 13 February 2009 (UTC)

== Page error? ==

I saw this error message:

Warning:  stristr() [function.stristr]: Empty delimiter in /home/shortcir/public_html/rosettacode.org/w/extensions/geshi/geshi.php</b> on line 2127

Don't know if that's anything anyone can fix. --[[User:Mwn3d|Mwn3d]] 13:32, 8 March 2008 (MST)
: Does that still happen?  We've been through a few upgrades, and when I tweak the source code badly, things like this can happen. --[[User:Short Circuit|Short Circuit]] 21:48, 13 February 2009 (UTC)
::I haven't seen it anymore. I promise if I do I'll email you directly. --[[User:Mwn3d|Mwn3d]] 22:17, 13 February 2009 (UTC)

== Wikipedia's version? ==

When you try to use an unsupported language via <nowiki><source lang="lang"></nowiki>, you get an error that lists quite a few more supported languages. Is there a way to use the version of GeSHi that wikipedia is using?

'''GeSHi Error:''' GeSHi could not find the language forth (using path /usr/local/apache/common-local/php-1.5/lib/GeSHi-1.0.7.19-wm1/geshi/) (code 2)

You need to specify a language like this: <nowiki><source lang="html">...</source></nowiki>

Supported languages for syntax highlighting:

actionscript, ada, apache, applescript, asm, asp, autoit, bash, blitzbasic, bnf, c, c_mac, caddcl, cadlisp, cfdg, cfm, cpp, cpp-qt, csharp, css, d, delphi, diff, div, dos, eiffel, fortran, freebasic, gml, groovy, html4strict, idl, ini, inno, io, java, java5, javascript, latex, lisp, lua, matlab, mirc, mpasm, mysql, nsis, objc, ocaml, ocaml-brief, oobas, oracle8, pascal, perl, php, php-brief, plsql, python, qbasic, rails, reg, robots, ruby, sas, scheme, sdlbasic, smalltalk, smarty, sql, tcl, text, thinbasic, tsql, vb, vbnet, vhdl, visualfoxpro, winbatch, xml, xpp, z80

== How to create GeSHi script file ==

The help page says "Feel free to provide a GeSHi script file.". However, there is no information on:
* Where to find information about the script file (with Googe, I found [http://qbnz.com/highlighter/ this page]).
* Where to get a sample script file that could be used as base for creating script for a new language. I did not find one from the above link.
* If I make a syntax file, where can I send it to be included in Rosetta Code?
--[[User:PauliKL|PauliKL]] 09:00, 6 April 2009 (UTC)

:I agree, it would be nice if there was a page containing the GeSHi files. Also, I can't find a guide anywhere, so can someone helpful make one and link it from Help:GeSHi? ~ [[User:BR|BR]] 20:21, 7 December 2009 (UTC)

:This page was pretty useless, so I redirected it to [[Help:Syntax Highlighting]], which has the things you request (or at least links to them). —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 21:45, 7 December 2009 (UTC)

==AutoHotkey==
Created a geshi lexer for autohotkey: [http://github.com/tinku99/ahklexers.git/autohotkey.php autohotkey.php].  
 Please add to rosettacode. Thanks, --[[User:Tinku99|Tinku99]] 16:49, 19 June 2009 (UTC)

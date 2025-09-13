+++
title = "Phix"
description = ""
date = 2019-09-15T15:29:44Z
aliases = []
[extra]
id = 19476
[taxonomies]
categories = []
tags = []
+++

Phix is a self-hosted hybrid interpreter/compiler, developed by Pete Lomax. It is very easy to use, and similar to Euphoria.

[http://phix.x10.mx/download.php A simple 20MB download] contains a pre-compiled executable, all the sources, and'' '''everything''' ''needed to recompile them, in about 15 seconds. The download also contains a full-featured programmer's editor and 130+ demo programs.

Perhaps the most striking feature of Phix is that it has just five builtin data types:

```txt

        <-------- object --------->
        |                |
        +-atom           +-sequence
          |                |
          +-integer        +-string

```

Despite such apparent simplicity, or perhaps precisely because of it, Phix programs are pretty fast - not quite achieving the runtime performance of C or assembly, but making up for it with a very fast edit/run cycle and proper human-readable messages should anything go wrong (even in shipped pre-compiled executables). Sequences are the real powerhouse of Phix. The one type covers lists, queues, tables, trees, and arrays, with strings being the subset that is array of character. They can grow and shrink automatically without any memory management overhead. For example if <tt>s="food"</tt> then <tt>s[2..3]="e"</tt> makes <tt>s</tt> "fed", and then <tt>s[2..1]="east"</tt> makes <tt>s</tt> "feasted".

Phix applies the principle of least surprise, for instance in some languages <tt>myproc(list)</tt> or <tt>res = myfunc(list)</tt> can mangle list, whereas in Phix if you actually want that to happen you would code <tt>list = myproc(list)</tt> (and myproc would need to become a function) or <tt>{res,list} = myfunc(list)</tt>. Likewise 1/2 is 0.5 (not 0, unless you explicitly ask for the floor()) and 0-1 is -1 (not +MAXINT). A core tenet is that for any line of code there is one and only one possible interpretation of it, and said meaning is utterly intuitive. True fact: the given answer for the question "describe what f(a++) does" in "More Exceptional C++" lists 4 possibilities for f, and 3 for a (so 12 in total) and uses the phrase "could mean just about anything" not once but twice - shudder.

Phix is not inherently object orientated, but achieves many of the claimed benefits, yet in a much simpler way. In fact I have been looking for <i><b>proof</b></i> that object orientation actually improves productivity compared to other paradigms for decades, and never found it. Not to say that I won't support it, but I certainly won't enforce it. One other thing I have never found is a "good object orientated design", and reached the conclusion that mythical creature simply does not exist, at least not as a separate entity as opposed to some ethereal quality of the finished code. Feel free to argue that one on [User_talk:Petelomax](https://rosettacode.org/wiki/User_talk:Petelomax). Phix offers perfectly decent encapsulation at the file level, proving that is not the sole purview of oo, implements polymorphism far more elegantly than C-based languages and far safer than duck-typed languages, and as for the third pillar of oop, inheritance, well isn't the current mantra "favour composition over inheritance"?

The fundamental goal of Phix is to make debugging easier, a whole subject area that does not seem to get the attention it deserves, despite several studies finding that most programmers actually spend between 70 and 90% of their time debugging. (Most of us tend to think that getting just-written code to work is somehow not debugging, but that bloke with a clipboard stood behind you would disagree.)

## See also
* [http://phix.x10.mx Phix site]
* [https://bitbucket.org/petelomax/phix/src bitbucket]

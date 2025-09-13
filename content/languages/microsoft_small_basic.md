+++
title = "Microsoft Small Basic"
description = ""
date = 2018-07-13T09:06:26Z
aliases = []
[extra]
id = 21687
[taxonomies]
categories = []
tags = []
+++


'''Microsoft Small Basic''' (not to be confused with [SmallBASIC](https://rosettacode.org/wiki/SmallBASIC)) is a [BASIC](https://rosettacode.org/wiki/:Category:BASIC) compiler for the [.Net Framework](https://rosettacode.org/wiki/.Net_Framework), largely aimed at absolute beginners. It is an extremely simplified variant of the BASIC language, and can be thought of as a sort of "[QBasic](https://rosettacode.org/wiki/QuickBASIC#QBasic).Net". In fact, its creator has called it a modern-day QBasic. But it is a language and not an implementation because it has its own syntax.

Three examples:
* the For loop has no Next statement
  For i=1 To 10
    ...
  EndFor
* arrays are neither declared nor allocated. No Dim or ReDim statements.
* arrays are associative and have a C syntax, tab(i,j)=k is coded:
  tab[i][j]=k


Small Basic lacks some things that are found in most other modern languages -- such as functions (although subroutines are supported) and the ability to call external libraries -- but this lack is promoted as deliberate simplification, to avoid confusing beginners with unnecessary complexity.

On the other hand, the language has some functionality built in that is not seen in any other language. For example, the <code>Desktop</code> and <code>Flickr</code> objects provide (limited) access to the [Windows desktop](https://en.wikipedia.org/wiki/Windows_shell#Desktop) and [Flickr](https://en.wikipedia.org/wiki/Flickr), respectively. As an interesting side note, this allows for some interestingly short programs, such as this one-liner which sets the Windows wallpaper to a random Flickr image:
 Desktop.SetWallPaper(Flickr.GetRandomPicture())

Small Basic also uses some ideas from other languages, most notably [Logo's](https://rosettacode.org/wiki/:Category:Logo) [Turtle graphics](https://en.wikipedia.org/wiki/Turtle_graphics).

Note that although this is a compiler, the only way to actually compile programs is by running them from the IDE.

## See also
*[http://msdn.microsoft.com/en-us/devlabs/cc950524.aspx Small Basic homepage]
*[Small Basic on Wikipedia](https://en.wikipedia.org/wiki/Microsoft_Small_Basic)
*[http://msdn.microsoft.com/en-us/beginner/hh304480.aspx Small Basic Getting Started Guide]

[Category:Web Application](https://rosettacode.org/wiki/Category:Web_Application)

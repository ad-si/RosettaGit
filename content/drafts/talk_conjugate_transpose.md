+++
title = "Talk:Conjugate transpose"
description = ""
date = 2016-11-21T13:41:41Z
aliases = []
[extra]
id = 11530
[taxonomies]
categories = []
tags = []
+++

== Bug in Factor ==

[[Ruby]] and [[Octave]] give the same answer.

<math>M \times M^H = \begin{bmatrix} 1 + 2i & 0 \\ 0 & 3 + 4i \end{bmatrix} \times \begin{bmatrix} 1 - 2i & 0 \\ 0 & 3 - 4i \end{bmatrix} = \begin{bmatrix} 5 & 0 \\ 0 & 25 \end{bmatrix}</math>

[[Factor]] gives a different answer.

<math>M \times M^H = \begin{bmatrix} 1 + 2i & 0 \\ 0 & 3 + 4i \end{bmatrix} \times \begin{bmatrix} 1 - 2i & 0 \\ 0 & 3 - 4i \end{bmatrix} = \begin{bmatrix} -3 - 4i & 0 \\ 0 & -7 - 24i \end{bmatrix}</math>

If Ruby and Octave are correct, then <math>M</math> is a normal matrix. If Factor is correct, then <math>M</math> is ''not'' a normal matrix. '''Factor is wrong.''' Factor's [[matrix multiplication]] has a bug: Factor confuses [https://duckduckgo.com/?q=Hermitian+inner+product Hermitian inner product] with [[scalar product]], and calculates ''<span style="text-decoration: overline">A</span> &times; B'' in place of ''A &times; B''. --[[User:Kernigh|Kernigh]] 00:30, 17 March 2012 (UTC)


==Formulae were hidden to most browsers by under-tested cosmetic edits at 23:08, 14 August 2016 ==

Under-tested cosmetic edits made to the task page at 23:08, 14 August 2016, including the injection of spaces around expressions in &lt;math&gt; tags, left some or all of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 10:10, 22 September 2016 (UTC)
: Some but not all of the introduced invisibility has now been repaired by individual edits removing the redundant spaces that were added to the &lt;math&gt; tag. For full repair it may prove necessary to simply undo the edit of 23:08, 14 August 2016 [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 10:10, 22 September 2016 (UTC)
:: Visibility now restored. (Some elements had been made invisible not only in Chrome, IE/Edge, Safari, Opera, but also in Firefox) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:41, 21 November 2016 (UTC)

== Ruby bug fixed in head ==

https://bugs.ruby-lang.org/issues/6290#change-25888


```txt
Matrix#hermitian?
```
 has been fixed in ruby 2.0

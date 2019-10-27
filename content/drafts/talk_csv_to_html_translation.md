+++
title = "Talk:CSV to HTML translation"
description = ""
date = 2013-06-03T11:17:45Z
aliases = []
[extra]
id = 8708
[taxonomies]
categories = []
tags = []
+++

==Output rendering==
Is there any way to show the rendered HTML rather than the raw HTML (without the wiki engine getting in the way? --[[User:Paddy3118|Paddy3118]] 15:22, 13 November 2010 (UTC)
: Apart from a screenshot? I don't know. I'm obviously wary of providing a raw HTML channel for fear of arbitrary code. (I suppose loading it into a DOM stripping just about any attribute might work, but the result will obviously not be pretty.) --[[User:Short Circuit|Michael Mol]] 16:05, 13 November 2010 (UTC)
:: A screenshot could be a good idea, while I guess every contributor would display the HTML code in a different browser. But while images take space, is rosettacode's hosting able to host all these images? [[User:Blue Prawn|Blue Prawn]] 19:34, 13 November 2010 (UTC)
::: As far as bandwidth? I don't know. It really depends on what kind of traffic we get, I suppose. Disk space? RC currently has 30MB of image data in the /images folder, so I'm not particularly worried about it right now. (I do prefer liberal application of tools like pngcrush, though.) As far as variations on implementation...I'm wondering what the current state of offscreen, no-X, no-Win32 render-it-to-a-file is for HTML. That's a plausible way to go about this kind of thing, too. --[[User:Short Circuit|Michael Mol]] 22:05, 13 November 2010 (UTC)
:::: Here are two screenshots with the "links" web browser:
:::: [[Image:Csv2html ocaml links kde frame.png|CSV to HTML OCaml, with links browser under KDE]]
:::: 2,8K (2866 octets)
:::: [[Image:Csv2html ocaml links blackbox frame.png|CSV to HTML OCaml, with links browser under blackbox]]
:::: 2,4K (2408 octets)
:::: (pngcrunsh saved 24 octets on the blackbox one.)
:::: [[User:Blue Prawn|Blue Prawn]] 00:54, 14 November 2010 (UTC)
::::: Heh...I didn't think of that. Using captured output of a text-mode browser to output text suitable for presentation in a fixed-width environment. I might be able to shoehorn that into a MW extension. --[[User:Short Circuit|Michael Mol]] 01:51, 14 November 2010 (UTC)
:::::: Other screenshots for the extra credit.
:::::: With a more conventional browser:
:::::: [[Image:Csv2html_ocaml_mozilla_firefox_kde_frame.png|CSV to HTML OCaml, with links browser under KDE]]
:::::: This image takes 9,0K.
:::::: [[Image:Csv2html_ocaml_konsole_kde_frame_extra_credit_solution.png|CSV to HTML OCaml, the extra credit, with links browser in konsole]]
:::::: And this one 7,0K (7087 octets.)
:::::: But well do not hesitate to remove any of these screenshots. [[User:Blue Prawn|Blue Prawn]] 02:48, 14 November 2010 (UTC)
:: Displaying someone else's HTML without excessive stripping or vulnerability is just what [http://code.google.com/p/google-caja/ Caja] is for. Web-service and Java interfaces are available. —[[User:Kevin Reid|Kevin Reid]] 23:16, 14 November 2010 (UTC)


==Cheating in Optional Part==
Would it be ok to use the :first pseudo-class? (Like 
```CSS
tr:first-child td {font-weight: 900;}
```
)
It would be easier to implement in most languages, but I think it demonstrates some abuses of CSS I don't like (CSS is formating html is markup…). 
If so, the description of that optional solution should be changed to something like “… optionally make it possible that the first row is marked as a table header (and format them nicely for further extra credits)”
If not, many languages might have a overcomplicated solution for the extra part

==HTML==
Some of the solutions distinguish the header from the body by inserting bgcolor="acolour" in the THEAD element. According to http://www.w3schools.com/tags/tag_thead.asp bgcolour is not an attribute of THEAD. By experiment I find that these solutions work on IE and Firefox under Windows, but not Opera or Firefox under Linux.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:17, 3 June 2013 (UTC)

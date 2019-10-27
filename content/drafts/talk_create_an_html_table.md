+++
title = "Talk:Create an HTML table"
description = ""
date = 2019-01-22T23:10:49Z
aliases = []
[extra]
id = 9341
[taxonomies]
categories = []
tags = []
+++

== just use a pure language? ==
Shouldn't the language be just Javascript? OK, there is [[CSV to HTML translation]] but this could be a smaller HTML producing task. --[[User:Paddy3118|Paddy3118]] 07:40, 9 March 2011 (UTC)

: I assumed this was a task to display a table, not specifically in HTML. I think that was more interesting than specifically writing HTML, and we already have *ML-generation tasks. I request that you undo your move and change to the description, since the HTML notion seems to have come solely from my example. —[[User:Kevin Reid|Kevin Reid]] 21:48, 10 March 2011 (UTC)

::Ouch. I see what you mean. I've asked the [[User_talk:Mwn3d#Create_an_HTML_table|original author]] to clarify. --[[User:Paddy3118|Paddy3118]] 22:13, 10 March 2011 (UTC)
:::I'm not the original author. It was originally made by [[Special:Contributions/142.204.88.102|an anonymous user]]. The only text back then was "how to add a column and row in nested loop". The original intent didn't seem to have anything to do with HTML. We don't seem to have any other HTML generation tasks (or at least they're not in [[:Category:HTML]]), so it might be good to have both. If we do have both, though, I think we can do some more nifty stuff with the HTML version. This simple table would be interesting enough for a plain text task. --[[User:Mwn3d|Mwn3d]] 22:19, 10 March 2011 (UTC)

:::: If the task is to [http://rosettacode.org/mw/index.php?title=Create_an_HTML_table&oldid=103045 "display a table"], then my [[Ruby]] code is wrong, because it only outputs the HTML for a table, and never displays the table. Perhaps we should delete the Ruby example, and the other examples that never display a table? --[[User:Kernigh|Kernigh]] 02:18, 11 March 2011 (UTC)
:::::The task ''was'' to display a table. It ''is'' to create the HTML for a table. The Ruby code is right now. --[[User:Mwn3d|Mwn3d]] 02:58, 11 March 2011 (UTC)

== Alignment? ==

Not sure the point of saying numbers should be aligned in the same fashion for all columns.  That seems to be just picking something at random not required by the task and saying "don't add this flourish."  Anyway, the non-disallowed flourish I added was to generate the result with a template mechanism.  (The task as written would seem to allow a single print statement that simply printed the html table from a string constant.)
<div><small>''(This was written by [[User:Sonia|Sonia]] ([[User_talk:Sonia|Talk]] | [[Special:Contributions/Sonia|contribs]]) at 06:25, 22 May 2011)''</small></div>


== 'Incomplete' notice on JavaScript – what are you after ? ==

: Hi, [[User:petelomax|petelomax]] very happy to expand this one, but not clear to me yet what you are looking for. The second JS version (like for example, the AWK version) shows the HTML table source returned by evaluating the code shown. Do you feel that one of the task bullet points has been missed ?
:Or are you looking, perhaps for some interaction with the DOM ? 
: The point there is that JS is an embedded language of which the DOM itself is not a part, and the DOM library objects are only available to JS interpreters that are embedded in browsers. JS is now widely embedded in variety of non browser applications. 
:JS itself, as a language, can only create the HTML code. 
:Let me know what extra element you are after, and I will fill it in. In the meanwhile, I will add a display of the HTML, in case that is really what you are looking for [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:09, 19 January 2019 (UTC)
::Nevermind: I have removed the incomplete tag. Today, I just added "let res =" before the snippet and "console.log(res)" after, and not surprisingly all is well. Two weeks ago I completely failed to get it to run, and tbh I'm no longer quite sure why I had a problem. Thanks anyway. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 22:51, 20 January 2019 (UTC)
:::I quite understand – the peculiar history of JS does leave a kind of undefined blank around IO ... That code returns a value, but is agnostic about what the IO mechanism of a particular JS embedding will be. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:59, 20 January 2019 (UTC)

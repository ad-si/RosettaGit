+++
title = "Rosetta Code:Village Pump"
description = ""
date = 2018-04-29T22:47:00Z
aliases = []
[extra]
id = 2891
[taxonomies]
categories = []
tags = []
+++

This is the new place for Rosetta Code community activity. To start a new "thread", just append a "/" to the end of the URL of this page and give your thread a short title. On the new topic page add 
```txt
<nowiki>{{Vptopic
|topic=Title
|summary=Summary of the topic
}}</nowiki>
```
substituting your topic title for "Title" and a summary for "Summary of the topic".
{{#ask:[[Is village pump topic::true]]
|?Summary
|?Modification date
|format=broadtable
|limit=20
|order=desc
|sort=Modification date
|searchlabel=Older topics...
|offset=0
}}

Hi, sorry to post this here but I don't know a better place.  I wanted to create a new proposed task "Create statically-validated API" but it's impossible to create a page without solving a captcha, and the browser I use doesn't show the captcha (I was able to create this account using a phone browser, but that's no good for entering much text or code).  Maybe someone can move it to the right place?  (Added: I also had to de-link the urls to bypass the captcha).

By the way I find this site a huge pain to use because of the captchas.  Email verification for account creation is likely to be enough.

== create statically-validated API ==

This is a challenge proposed by Tony Morris here:

blog.tmorris.net/posts/understanding-practical-api-design-static-typing-and-functional-programming/

It is only for statically-typed languages and the challenge is to implement a Tic-tac-toe API that supports operations like starting a game, making a move, and seeing who won, where invalid arguments (such as asking who won an unfinished game) are rejected by the compiler as compile-time type errors.  So it is mostly a test of how expressive the language's type system is.

A Haskell solution (not by me) is here:

github.com/tismith/tictactoe-haskell/blob/master/Tictactoe.hs

Further discussion and links to some other solutions:

github.com/data61/fp-course/blob/master/projects/TicTacToe/TicTacToe.markdown

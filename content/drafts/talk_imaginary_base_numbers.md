+++
title = "Talk:Imaginary base numbers"
description = ""
date = 2018-11-09T13:13:55Z
aliases = []
[extra]
id = 22062
[taxonomies]
categories = []
tags = []
+++


###  couple of minor points 

When translating the Sidef code to Phix, one thing I noticed is that parse_base() never has 
to deal with a leading '-', so I took that out. As far as I understand it, imaginary base numbers 
never use a minus sign. The same unnecessary handling exists in the Perl6 code. 
Also, the even entries in the second half of the output from Kotlin, Java, Go, D, and C# all fail
to trim the unnecessary trailing ".0" (as the heading says, both pretty minor points). [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 04:21, 9 November 2018 (UTC)

:<blockquote>"When translating the Sidef code to Phix, one thing I noticed is that parse_base() never has to deal with a leading '-'"...</blockquote> I put that in there following the principle of "Be liberal in what you accept and conservative in what you emit."
:But you're not wrong; assuming well formed input, that line is redundant. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 13:12, 9 November 2018 (UTC)

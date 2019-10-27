+++
title = "Talk:Zeckendorf arithmetic"
description = ""
date = 2019-04-04T14:45:25Z
aliases = []
[extra]
id = 12490
[taxonomies]
categories = []
tags = []
+++

Spelling should be "arithmetic".
:Got it. --[[User:Mwn3d|Mwn3d]] 14:45, 29 October 2012 (UTC)

==Perl 6 error==
I get 10100 + 1010 = 101000, aka 11 + 7 = 18 which contradicts the Perl 6 output of 10100 +z 1010 = 100101  # addition.

My output matches the first test of the tcl entry, which obviously also disagrees with the Perl output. 

In turn the next line is 100101 -z 100 = 100010  # subtraction, which I read as 17-3 = 15.

Obviously, you can refer to [[Zeckendorf_number_representation#Perl_6]] to confirm or dispute my findings.

[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 22:32, 21 August 2018 (UTC)

:Off-by-one issue fixed (affected: addition, subtraction, division) and warning removed. --[[User:SqrtNegInf|SqrtNegInf]] ([[User talk:SqrtNegInf|talk]]) 14:45, 4 April 2019 (UTC)

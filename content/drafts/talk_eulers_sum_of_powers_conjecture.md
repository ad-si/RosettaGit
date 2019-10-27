+++
title = "Talk:Euler's sum of powers conjecture"
description = ""
date = 2015-09-29T16:01:01Z
aliases = []
[extra]
id = 19370
[taxonomies]
categories = []
tags = []
+++

== Fermat's little theorem? ==

The C and C++ entries mention Fermat's little theorem. And it's true that Fermat's little theorem gives us ((x^5)modulo 5) = (x modulo 5).

However, Fermat's little theorem does not show that ((x^5)modulo 3) = (x modulo 3) nor that ((x^5)modulo 2) = (x modulo 2). 

The modulo 2 one is obvious - an odd number to any positive integer power will still be odd, and an even number to any positive integer power will still be even. That's not Fermat's little theorem. It might be possible to derive this from Fermat's little theorem? I would be interested in seeing that derivation.

The modulo 3 expression, though, seems a bit more obscure. How does that work?

(Reading that code, I'm having trouble deciding whether it could work for arbitrarily large values of N.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:21, 8 July 2015 (UTC)

ANSWER:  Fermat's little theorem says x^p == x (mod p).  Think of this as saying x^{p-1} == 1 OR x == 0 (mod p).  This implies that x^{K(p-1)+1} == x (mod p) for any K.  So we combine p=2, K=4 (trivial); p=5, K=1 (direct application of FlT; and p=3, K=2 (the case you are looking at).  In every case we show x^5 == x. --[[User:TomHyer|Tom Hyer]]

----
EchoLisp solution :

if N = 3p + 1 , N^5 = "243p^5 +405p^4 +270p^3 +90p^2 +15p +1 " , which is 1 (mod 3)

if N = 3p + 2 ; N^5 =  "243p^5 +810p^4 +1080p^3 +720p^2 +240p +32 ", which is 2 (mod 3) 
--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 22:10, 8 July 2015 (UTC)

: Ok, but why? Is that just a convenient way of hardcoding the desired result, or is there some generally valid rule here? You can fit a fifth degree polynomial such as this one to up to six points and - perhaps coincidentally - the N=1000 example has six answers. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:10, 8 July 2015 (UTC)

: Actually, I don't see any such expression in echolisp's solution. That solution does make sense to me - but I'm not seeing anything related to fermat's little theorem there, yet. Nor am I seeing anything like that polynomial. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:26, 9 July 2015 (UTC)

:: It was just a mean to demonstrate that '''N = 0,1,2 (mod 3) implies N^5 = 0,1,2 (mod 3)'''. I do'nt know any more. This is not used in EchoLisp solution to the task. In this '''talk''' 'Echolisp solution' only means I computed the powers of 3p+i polynomials with EchoLisp.  I should have written 'EchoLisp solution to your question in this talk' .Sorry for the misunderstanding. --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 05:53, 9 July 2015 (UTC)

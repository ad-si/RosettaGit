+++
title = "Talk:Roots of a quadratic function"
description = ""
date = 2015-05-19T10:47:52Z
aliases = []
[extra]
id = 4420
[taxonomies]
categories = []
tags = []
+++

I don't like being pushed to use obscure (but more stable) algorhithm over algorithm everyone instantly recognises.
Said that, I could live with it. But I want to point that:
- link for "What Every Scientist Should Know About Floating-Point Arithmetic" is dead;
- one can google for it, but formatting is different, so how to find what "page 9" actually is?
- quoted in the text, "Suggested by Middlebrook (D-OA), a better numerical method: to define two parameters q=sqr(ac)/b (...)" naturally fails over if a>0 and c<0.
"do it better" ?

I clearly see come of programs (Ada, and some marked as "translation of Ada") use
(b^2-4ac)/2*a,
there correct way is
(b^2-4ac)/(2*a).
I wonder why noone spotted this earlier!

<cite>Forsythe, Michael Malcolm and Cleve Mole suggest to try it on a=1, b=-10<sup>5</sup>, c=1</cite>, but Ada sample code uses -10e5, which is indeed -1e6 (-10<sup>6</sup>), if I was not wrong since I knew the "e" notation... --[[User:ShinTakezou|ShinTakezou]] 21:24, 22 June 2009 (UTC)

Basically all the test cases had a = 1, :-)  So I added some test cases especially where a ≠ 1, hence 2*a ≠ 2/a.... [[User:NevilleDNZ|NevilleDNZ]] 14:28, 16 September 2010 (UTC)

In the example above (2nd line) (-10<sup>6</sup>) isn't the same as -1e6.  The former is -1,000,000 and the latter is +1,000,000. --[[User:Gerard Schildberger|Gerard Schildberger]] 18:11, 25 June 2011 (UTC)

Other way round surely: <math>-10^n</math> will always be positive when n is even. --Laurie Alvey 10:45, 19 May 2015 (UTC)

== J example ==
Dumontier, hope you don't mind me replacing your example code. I understood that you were trying to illustrate the generality of <code>p.</code> however your example used a quadratic, that had already been shown above. If you were trying to illustrate some other point I apologise! --[[User:Tikkanz|Tikkanz]] 23:14, 14 October 2009 (UTC)

== C examples ==

I just provided a (more) correct C version, and am now tempted to remove other C examples, because the task specifically mentioned the shortcoming of the naive method, yet they went on that route anyway.  Opinions? --[[User:Ledrug|Ledrug]] 09:08, 25 June 2011 (UTC)

<s>Wait, what? The existing C and C++ code gives 10^20 and 10^-20 as roots to equation <code>x^2 - 10^-20 x + 1 == 0</code>? What the? --[[User:Ledrug|Ledrug]] 09:22, 25 June 2011 (UTC)</s>

Eh never mind that, and sorry about making a mess on the incorrect tags--I need sleep... --[[User:Ledrug|Ledrug]] 09:40, 25 June 2011 (UTC)

== Clojure example ==

Why are there 4 functions for clojure?

What I mean is that all of that can be simplified into:

```clojure
(defn quadratic 
  "Compute the roots of a quadratic in the form ax^2 + bx + c = 0
   Returns any of nil, a float, or a vector."
  [a b c]
  (let [sq-d (Math/sqrt (- (* b b) (* 4 a c)))
        f    #(/ (% b sq-d) (* 2 a))]
    (cond
       (neg? sq-d)  nil
       (zero? sq-d) (f +)
       (pos? sq-d)  [(f +) (f -)]
       :else nil))) ; maybe our number ended up as NaN
```


I find it ridiculous to have that much verbiage on 1 example.

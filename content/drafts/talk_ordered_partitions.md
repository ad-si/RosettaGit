+++
title = "Talk:Ordered Partitions"
description = ""
date = 2011-02-10T13:34:33Z
aliases = []
[extra]
id = 9224
[taxonomies]
categories = []
tags = []
+++

== (turns out not) incorrect math statement==

The task says:

:Note that the number of elements in the list is 
:<math>(\mathit{arg}_1+\mathit{arg}_2+...+\mathit{arg}_n\ \mathrm{choose}\ arg_1) * (\mathit{arg}_2+\mathit{arg}_3+...+\mathit{arg}_n\ \mathrm{choose}\ \mathit{arg}_2) * \ldots * (\mathit{arg}_n\ \mathrm{choose}\ \mathit{arg}_n)</math>

But this cannot be right.

The task with args 1,2,4 would generate 105 distinct partitions.  But <code>4 choose 1</code> is 0 and <code>4 choose 2</code> is 0 and <code>4 choose 4</code> is 1, so the above formula would give a result of 6.  --[[User:Rdm|Rdm]] 22:27, 7 February 2011 (UTC)

:Never mind, I fixed it.  --[[User:Rdm|Rdm]] 22:36, 7 February 2011 (UTC)

:: Is it really fixed? As far as I know choose, the bigger number should be on the left. Anyways, I'll change it to

::: <math>{\mathit{arg}_1+\mathit{arg}_2+...+\mathit{arg}_n \choose \mathit{arg}_1} \cdot {\mathit{arg}_2+\mathit{arg}_3+...+\mathit{arg}_n \choose \mathit{arg}_2} \cdot \ldots \cdot {\mathit{arg}_n \choose \mathit{arg}_n}</math>

:: to make it clearer --[[User:Newgame|Eugen]] 10:36, 8 February 2011 (UTC)

::: Oh! No, the original page was right, and I was wrong, about that left vs. right thing with the word "choose".  

::: My problems were twofold: First, I did not have a definition of "choose", and second I did not know how to group operations when faced with a + b choose c.  I went with a + (b choose c), but it's now clear to me that I should have gone with (a+b) choose c.  The current main page shows operator precedence clearly, but still could do with a link to a definition.  (The link to [[Combinations]] suggests a definition, and there can be good reasons to order the arguments that way, but that does not actually define the operation the original (anonymous) author intended for [http://mathworld.wolfram.com/Combination.html<code>choose</code>].  Perhaps the right approach here would be to mention popular notation issues on the [[Combinations]] page?)  --[[User:Rdm|Rdm]] 15:55, 8 February 2011 (UTC)

:::: (Note: I was the anonymous user that created the page, only after I created it I registered). You are right, that the first definition was ambiguous so I improved it. I also provided links to [[Ordered Partitions]] and [[Combinations]] for definitions of the binomial coefficient. 

:::: If I understand your point about notation issues correctly you mean that the notation on [[Combinations]] is different from the notation of [[Ordered Partitions]] and that is why popular notation issues should be added to the [[Combinations]] page? Actually, I don't know if there is a standard notation for the ''enumeration of combinations''. There is the [http://en.wikipedia.org/wiki/Binomial_coefficient binomial coefficient] notation for the ''number of combinations'', though, and this is why I used it on the page [[Ordered Partitions]]. I think the [[Combinations]] is clear enough imho. --[[User:Newgame|Eugen]] 11:18, 9 February 2011 (UTC)

::::: Fair enough.  Thank you.  --13:15, 9 February 2011 (UTC)

== Discarded code ==

I wrote this code for [[Ruby]]. It seems to return the correct result, but the algorithm seems to be too slow, so I will not post it on the page. --[[User:Kernigh|Kernigh]] 04:13, 10 February 2011 (UTC)


```ruby
require 'set'

def partitions(*args)
  (1..args.reduce(:+)).to_a.permutation.inject(Set[]) do |set, perm|
    set << (args.inject([]) do |dist, arg|
              dist.push(perm.shift(arg).to_set)
            end)
  end
end
```


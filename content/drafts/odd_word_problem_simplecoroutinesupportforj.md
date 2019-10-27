+++
title = "Odd word problem/SimpleCoroutineSupportForJ"
description = ""
date = 2012-08-09T15:45:03Z
aliases = []
[extra]
id = 10813
[taxonomies]
categories = []
tags = []
+++

Here is a simplistic coroutine implementation for [[../#J|J]].

Note that this version does not support switching between coroutine contexts -- that would have been an unnecessary complication for this task.  (So, technically, it might be better to call this "coroutine inspired".)


```j
NB. u coroutine y         NB. execute u in new coroutine context
coroutine=: 1 :0
  stack=. ''
  verb=. u
  noun=. y
  while. do. context=. verb noun
    select. (0 {:: context) * 1+*#stack
      case. 0 do.                   NB. yield
        stack=. stack, 1 { context
        verb=. (2 { context)`:0
        noun=.  3 {:: context
      case. 1 do.                   NB. return (with empty stack)
        1 {:: context return.
      case. 2 do.                   NB. return (with work remaining on stack)
        verb=. ({: stack)`:0
        noun=. 1 {:: context
        stack=. }: stack
      case. do.                     NB. (default)
        invalid coroutine 1 :'error.'
    end.
  end.
)        

NB. u yield v y return.   NB. 0 -- deferred result, will be executing: u v y
yield=: 2 :0
  0; u ` v,< y
)

NB. return y return.      NB. 1 -- immediate result: y
return=: 3 :0
  1; y
)
```


The philosophy here, is:  A noun represents code which has already been executed (it's the result of that code having been executed).  A verb represents code which has not yet been executed.

A coroutine verb must return a result built by one of the two helper verbs <code>return</code> or <code>yield</code>.  If <code>yield</code> is used, it must be supplied with two verbs and a noun.  The coroutine context will stack u (the left verb) for later execution and it's argument will be the result of whatever verb was most recently executed by the coroutine context.  Meanwhile the coroutine context will execute v (the right verb) right away and its argument will be the noun which was supplied to yield.  Both u and v must be coroutine verbs (so they also must must provide a result prepared by <code>return</code> or <code>yield</code>).

+++
title = "Talk:Ludic numbers"
description = ""
date = 2014-04-19T06:59:30Z
aliases = []
[extra]
id = 17389
[taxonomies]
categories = []
tags = []
+++

== 1 is the loneliest number... ==
The OEIS thinks the sequence starts at 1.  Obviously the task as originally specified draws from a source that thinks you should skip the 1, perhaps by analogy with prime numbers, which might or might not be construed as a false analogy, since the sequence contains other non-primes like 25, and since this sieve is based on position, not on value, as primes are.  I like it with the 1, and marked the Python entry as incorrect somewhat tongue-in-cheekly, but I think we can agree that we oughta agree on definition one way or the other before this becomes a real task. <tt>:-)</tt> --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 21:41, 15 March 2014 (UTC)

:Thanks Tim, I was so focused on the sieve loop, I forgot the initial 1. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:23, 16 March 2014 (UTC)

== Needs proper description ==

The OEIS wiki page's description of Ludic sequence generation is very clear, while that of the task page here is very much not. By the wording here, step 1 gives  2, 3, 4, ..., and L = 2; step 2 wants to "remove every L'th indexed item from the array (including the first)", which means what, remove 2 (the first), 3 (first L-th), 5 (second L-th), and so on?  So are we left with 4, 6, 8... now?  And what's the difference between "every L'th ''indexed''" and, say, just "every L-th"?

Also, I believe the "th" suffix is more commonly typeset as "L-th" or "''L''th" (the italic "L" in the latter optional.) --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 08:40, 16 March 2014 (UTC)

: Improved. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:31, 16 March 2014 (UTC)

==Task description generator==
Somewhere to stash it.

```python
from nth import nth

def _parray(array, indexed=None, nmax=25):
    lst = array[:nmax]
    return ('<code>' +
            ( ('<span style="color:blue;font-weight:bold"><s>%i</s></span> ' % lst[0])
              if indexed else
              ('<span style="color:blue;font-weight:bold">%i</span> ' % lst[0]) ) +
            ' '.join( (('%i' % l)
                       if not indexed or i % indexed
                       else ("<s>%i</s>" % l))
                      for i, l in enumerate(lst[1:], 1) )
            + ' ...</code>')

def ludicpr(nmax=100, prloops=4):
    print('The first ludic number is <span style="color:blue;font-weight:bold">1</span>.')
    #yield 1
    print('
To generate succeeding ludic numbers create an array of '
          'increasing integers starting from 2')
    lst = list(range(2, nmax + 1))
    print(':%s' % _parray(lst))
    print('(Loop)')
    i = 0
    while lst and i <prloops:
        i += 1
        print('* Take the first member of the resultant array as the next Ludic number '
              '<span style="color:blue;font-weight:bold">%i</span>.' % lst[0])
        #yield lst[0]
        print("* Remove every '''%s''' indexed item from the array (including the first)." % nth(lst[0]))
        print('::%s' % _parray(lst, indexed=lst[0]))
        del lst[::lst[0]]
        if i == 1:
            print('* (Unrolling a few loops...)')
    print("""\
* ...
* Take the first member of the current array as the next Ludic number <span style="color:blue;font-weight:bold">L</span>.
* Remove every '''L'th''' indexed item from the array (including the first).
* ...""")
```
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:59, 19 April 2014 (UTC)

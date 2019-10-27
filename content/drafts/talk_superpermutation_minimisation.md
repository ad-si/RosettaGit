+++
title = "Talk:Superpermutation minimisation"
description = ""
date = 2018-08-31T10:02:46Z
aliases = []
[extra]
id = 18371
[taxonomies]
categories = []
tags = []
+++

==N. Johnston's algorithm==
Hi Paddy3118,

The referenced work by N. Johnston proposes an algorithm which produces strings of length sum over k=1 to n of k!. None of the Python or D solutions produce such a good result. Would it be better if the task was to implement N. Johnston's algorithm, outputting the first 5 results, and noting that after 5 shorter strings probably exist? I am surprised that you have placed so much emphasis on timings here, given that you are so quick to complain when someone who thinks a timing says something about a solution does so. (Given that I don't think these timings mean much).--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:21, 12 December 2014 (UTC)

:Hi Nigel, I decided to write the task as a means of comparing heuristics. I needed several heuristics and a weighting scheme to compare them. 
:N. Johnston's paper wasn't one of my early references, (indeed I have lost the reference that gave me s_perm0 - my first algorithm). Maybe either I or someone else will add that algorithm to the Python entry in the future? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:06, 12 December 2014 (UTC)

== Ambiguous ==

Task, as currently specified, is ambiguous. That the underlying problem may or may not be NP Complete is a part of this.  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:51, 13 December 2014 (UTC)
:Please expand. Ambiguous in what way? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:24, 13 December 2014 (UTC)
:: "Show descriptions and comparisons of algorithms used here, and select the "Best" algorithm as being the one generating shorter superpermutations." -- this seems to imply that each task implementation should handle some arbitrary number of algorithms and mark an algorithm producing the shortest result as the "Best". (Of course, Nigel Galloway seems to have stated that the task requires each implementation to provide an algorithm which consistently produces shortest possible results - but, currently, that's not what the task description explicitly asks for.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:34, 21 August 2015 (UTC)
:The problem is NP Complete. The value of 872 for 6 is found by writing the problem as a TSP and using specialized TSP solvers. TSPs are NP Complete and very very very really hard to prove minimal solutions. Perhaps the ambiguity is with the tasks title, none of the algorithms used by D or Python produce minimal strings beyond 3. The C entry is new and produce results consistent with N. Johnson's algorithm (minimal to 5). I havn't checked if it is actually N. Johnson's algorithm. As above I don't understand the value of timing algorithms which do not do the same thing. Comparing C's value for 10 (4037913) and D's value (4_235_533) tell's me which algorithm is best. That D can do it in less than 9 secs mmm! --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:46, 13 December 2014 (UTC)

::(Minimisation means making smaller - in this context smaller than just concatenating all permutations). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:48, 13 December 2014 (UTC)
:::Noun 1. minimisation - the act of reducing something to the least possible amount or degree. Perhaps the English word you are looking for is reduction.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:44, 14 December 2014 (UTC)

::::Ummm, I have been using that word in the wrong way for so long... Thanks Nigel! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:26, 16 December 2014 (UTC)

== Problem with the translation from D to python ==

The code doesn't produce the correct strings, because <code>pos</code> got passed into <code>r()</code> by value, which is no good when <code>r()</code> recursively calls itself and modifies <code>pos</code>.  The following is what should happen (conceptually, since <code>superperm.append()</code> might get expensive depending on your Python setup.  I don't have psycho module, so it actually runs faster than the current, very C-like Python entry.)  I'm sure there's a better way to express the same algorithm without killing performance in Python, though.


```python
from string import ascii_uppercase, digits

try:
   import psyco
   psyco.full()
except:
   pass

N_MAX = 12

def r(n, superperm, count):
   if not n:
       return False

   c = superperm[-n]
   count[n] -= 1
   if not count[n]:
       count[n] = n
       if not r(n - 1, superperm, count):
           return False

   superperm.append(c)
   return True


def super_perm(n, chars = digits + ascii_uppercase):
   assert len(chars) >= N_MAX

   count = list(range(n+1))
   superperm = [chars[i] for i in range(1,n+1)]

   while r(n, superperm, count): pass
   return superperm


def main():
   for n in xrange(N_MAX):
       superperm = super_perm(n)
       print "Super perm(%2d) len = %d" % (n, len(superperm)),
#       print ':', ''.join(superperm),
       print

main()
```


== Phrasing needs improved ==

This sentence from the current task description does not make sense:

"For example, representing the characters as A..Z, using N=2 we choose to use the first three characters 'AB'."

There's two characters there, not three. Presumably the sentence author was thinking about the sequences 'ABA' or 'BAB' but that's not what got into the sentence, and even if it had, the sentence still would not make sense. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:15, 30 October 2015 (UTC)

: Addressed, (belatedly). Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:02, 31 August 2018 (UTC)

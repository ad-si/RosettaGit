+++
title = "Talk:K-d tree"
description = ""
date = 2017-08-15T18:20:36Z
aliases = []
[extra]
id = 11475
[taxonomies]
categories = []
tags = []
+++

==New task==
A couple people have encouraged me at times to contribute something from my work.  While I don't actually maintain any k-d tree code, I do know k-d trees are used in various ways in astronomy, and it seems they have become well accepted data structures.  I found the WP nearest neighbor description a bit to cursory to code from directly, but the Moore psedocode relatively easy to implement.  While Moore acknowledges some inefficiencies in his presented code, I thought the simplicity of it made it a good starting point for someone coding a k-d tree for the first time in a new language.  I first tried a data set of 1e6 points but found the tree construction took a couple of seconds.  That sure showed the motivation for the n log n algorithms!  Rather than lead the task in that direction though, I though I'd initially show the simpler, if slower algorithm and just scale back the data set.  The more interesting part, after all, is the nearest neighbor search, which is log n and returns the answer in a flash. &mdash;[[User:Sonia|Sonia]] 19:38, 6 March 2012 (UTC)


I was reading through the linked WP page and came across this paragraph:
<blockquote>k-d trees are not suitable for efficiently finding the nearest neighbour in high dimensional spaces. As a general rule, if the dimensionality is k, the number of points in the data, <math>N</math>, should be <math>N \gg 2^k</math>. Otherwise, when k-d trees are used with high-dimensional data, most of the points in the tree will be evaluated and the efficiency is no better than exhaustive search, and approximate nearest-neighbour methods are used instead.</blockquote>
It would be nice to have some sort of note along those lines here too as it is a major criterion for algorithm/data-structure selection. (Astronomy's mostly low-dimensioned, 2D or occasionally 3D, so k-d trees make plenty of sense for them. Alas, the work I've done in an astronomy-allied field recently was all very high dimensioned with some dimensions not being standard-numeric, so we couldn't make good use of this sort of thing and anyway didn't need it as “nearest neighbour” wasn't a problem we had to solve. Instead, we use ''lots'' of relational databases. But I'm rambling…) –[[User:Dkf|Donal Fellows]] 06:10, 7 March 2012 (UTC)
: Certainly true.  Once I put in the count of nodes visited I found that many searches on the WP data set (N = 6 and k = 2) lead to all nodes being visited.  My choice of the point (9, 2) was contrived to give an answer with only 3 nodes visited.  It's tough to pick how much information to present in the task description and how much to expect people to do their own homework, but sure, I added the paragraph. &mdash;[[User:Sonia|Sonia]] 19:02, 7 March 2012 (UTC)

== The REXX code: wrong? ==
I'm tempted to slap a second incorrect tag on it, for it seems fundamentally wrong: as far as I can see, it constructs no tree, just bipartitions points once about the average of ''something''.  As a consequence, searches visit about half of the points, which isn't better than a straight exhaustive search by much.  Someone better check the code, the pink tag is forthcoming. --[[User:Ledrug|Ledrug]] 07:15, 26 April 2012 (UTC)
==C Entry==
Three suggestions for the C entry:

1) To make it easy to change the coordinate type (float, double, etc) and avoid this to cause bugs in the swap function, I suggest to replace:


```c
#define MAX_DIM 3
```


With:

```c
typedef double point_t[3];
```


And then use it in kd_node_t and swap.


2) To generally use NULL instead of 0 to denote the null pointer, to increase readability. Because 0 is a literal for both the null and integral zeros, while NULL denotes only a null pointer. Using more specific literals is generally good.


3) Regarding this:

```c
typedef struct kd_node_t *kd_node, kd_node_t;
struct kd_node_t {...}
```


See the "Chapter 5: Typedefs" here:
http://www.kernel.org/doc/Documentation/CodingStyle

I think a better solution is an intermediate way between your code and that coding standard. So using typedef to mask the "struct" is acceptable, but to use it to mask a pointer is not so good. This means I suggest to use something like this in the C entry (untested):


```c
typedef double point_t[3];
typedef struct kd_node_t kd_node_t;

struct kd_node_t {
    point_t x;
    kd_node_t *left;
    kd_node_t *right;
};
```

: 1) In terms of decoupling struct defs and usage, the easiest thing is probably
:
```c
void swap(struct kd_node_t *x, struct kd_node_t *y)
{
	struct kd_node_t tmp;
	tmp = *x;
	*x = *y;
	*y = tmp;
}
```
 which hides the details best, but does unnecessary copying. Another way is
:
```c
struct point_t { double x[3]; }
struct kd_node_t {
	point_t pt;
	struct kd_node_t *left, *right;
}

void swap(struct kd_node_t *x, struct kd_node_t *y)
{
	point_t tmp;
	tmp = x->pt;
	x->pt = y->pt;
	y->pt = tmp;
}
```
 which is arguably better, because it now allows the compiler to figure out the best method to do the copying.  The thing is, hiding details from <code>swap()</code> is not right.  Being a performance critical part, it ''should'' know the inner working of the struct involved. You could also argue that one shouldn't do <code>node->x[1]</code>, instead some accessor method <code>get_node_coord(node, 1)</code> should be used.  If we were designing a library and plan for it to be used by total strangers who need an interface decoupled from data definition, maybe; for a short example on RC and a samll routine used internally, no.

: 2) I'm ambivalent towards use of <code>NULL</code> token (it's probably <code>#define NULL (0)</code> anyway), which is better than what I have to say about <code>TRUE</code> or <code>FALSE</code>.  If one is reading C code, he better know what a nul pointer is anyhow.  I prefer writing <code>0</code>, but won't mind if someone changes it to <code>NULL</code>.

: 3) The <code>typedef</code> is indeed unneeded.  My habit is <code>typedef struct {} sometype_t</code> and <code>typedef struct {} *sometype</code>, where <code>_t</code> says it's a struct, and lacking of it means a pointer.  This may make pointers to pointers easier to write (<code>sometype *p</code> instead of <code>sometype_t **p</code>), but is certainly not necessary.  I'll drop the <code>typedef</code>s here, but I doubt it will make the code more or less readable. --[[User:Ledrug|Ledrug]] 21:34, 26 April 2012 (UTC)

:: FWIW, I agree strongly with using <code>NULL</code>; it's more ''idiomatic'' and says that “we're thinking about the pointer that does not point”. It's really a code-smell thing; if someone's mixing things up, you've got to examine ''every'' use of <code>0</code> to figure out what's going on. (This isn't the same as when you're working in your own code, but we want the very best of style here as each language understands that concept.) –[[User:Dkf|Donal Fellows]] 08:25, 5 June 2012 (UTC)

== Dubious about speed claims here ==

After studying the task and these implementations I'm feeling dubious about speed claims here.

If it takes half a second to construct the tree for a million nodes, and it takes a twentieth of a second to visit a million nodes, what kinds of speedups are we getting that justifies this use pattern?

More specifically: do any of these implementations outperform a exhaustive search on a dense linear dataset by as much as a factor of 2?  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:05, 5 June 2014 (UTC)

: I worked through the exercise in J. The kd-tree implementation turns out to be slower than the brute force implementation and the larger dataset takes proportionally longer than brute force than the smaller dataset. Some of this has to do with the architecture of J, and its optimizations, but these optimizations reflect similar issues having to do with cache management.

: Fortunately, most people don't understand efficiency? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 08:14, 6 June 2014 (UTC)

:: You could say the same thing about any search tree if all you need is find one node.  Search-trees are for repeated lookups, where tree construction is amortized.  Once constructed, search complexity is the tree height which should be <math>O(log(N))</math> if the tree is reasonably full. Compare that to the sequential search's <math>O(N)</math>, the implication should be clear. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 17:16, 6 June 2014 (UTC)

::: Yes, the implications are clear: for some searches, the cost of the kdtree search is cheaper than the cost of the brute force search. For other searches, the cost of the brute force search is cheaper than the cost of the kdtree search. Meanwhile, creating the kdtree always costs more than creating the original dataset.

::: To find when the amortized kdtree cost is cheaper than the brute force cost, you need to be aware of those costs as well as the cost of building the kdtree. You also need to have an idea of how many times the kdtree gets used, which is going to be, in the general case, an unknown.

::: Put differently: kdtrees are better than brute force for single point queries against a static dataset which receives a high volume of independent queries, if the volume is sufficiently high and the tree is sufficiently large (and if it is implemented correctly).

::: But other algorithms are also possible. For example, a n-dimensional grid (partitioning using evenly sized n-cubes). The grid approach has characteristics in common with both the brute force approach and the kdtree approach. What's best depends on the dataset and how often it gets updated, among other things (but other issues: cache architecture, use patterns, etc... also matter for determining cost, and the relevance of those costs). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:43, 6 June 2014 (UTC)

== C code bug producing incorrect results from k-d tree queries ==

Hi all.  I'm new to contributing to wikis, so apologies if I'm doing this the wrong way.  I couldn't find any email address to send feedback to, or anything like that, so I take it one is supposed to report bugs via a wiki contribution like this...?  Anyway, I believe the C code for the k-d tree has a bug.  This bug arises when there are different points in the dataset that have exactly the same value for a given coordinate – identical <code>x[idx]</code> values, in the terminology used in the code.  The bug is in these lines:

   /* median has duplicate values */
   if (store->x[idx] == md->x[idx])
      return md;

I found this bug empirically; I am using the RosettaCode k-d tree code in my project, and discovered that a dataset with identical coordinate values resulted in an incorrect k-d tree being built, and then in incorrect values being returned by queries against the k-d tree.  Fixing the bug fixed that issue.  Note that the bug occurs, not only in cases where two points have exactly the same spatial position (which one might argue is against the assumptions of the algorithm, or something), but also in cases where two points have one coordinate value that is identical between them, but differ in their other coordinate values – clearly a bug.

The bug is also apparent if you think about how the quickselect algorithm implemented by <code>find_median()</code> works.  It would be nice if one could detect duplicate values and short-circuit further iterations of the algorithm, as these lines claim in their comment to do, but that does not in fact work unless one does a three-way partition around the pivot (less than, equal to, greater than).  With the standard quickselect algorithm, as implemented by <code>find_median()</code>, which does a two-way partition (less than, greater than or equal to), this attempt to detect duplicate values and short-circuit can do the wrong thing, because although <code>store->x[idx] == md->x[idx]</code>, there is no guarantee that the values in between <code>store</code> and <code>md</code> are also equal; the dataset has not been sorted, just partitioned.

If you look at the Wikipedia article on quickselect (https://en.wikipedia.org/wiki/Quickselect), the algorithm there does not attempt to do this duplicate-detection and early termination.  Instead, written in the terminology of the RosettaCode example, it does:

   if (store == md)
      return md;

which is quite different, and is correct; that says that if the value chosen for the pivot happens to end up in exactly the median position, then you got lucky and you're done since you found the median.

If this bug is fixed, other things need to be changed in the code as well, because the <code>store->x[idx] == md->x[idx]</code> test was providing the termination condition for the loop; changing it to the correct test will then cause the code to hang.  As in the Wikipedia code, one must also add a test at the top of the loop:

   if (end == start + 1)
      return start;

That is at the top of <code>find_median()</code> as it now stands; it needs to instead be at the top of the <code>while (1)</code> loop, because this case can be hit in later iterations and is the base case of the divide-and-conquer algorithm.  Finally, where the code does:

   else start = store;

that needs to be:

   else start = store + 1;

again to avoid a hang, again following the Wikipedia pseudocode.  (Other differences from the Wikipedia pseudocode are OK, I think, and have to do with the way the C code uses pointers instead of indexes, and keeps <code>end</code> pointing a position after the last element rather than to the last element itself.)

I didn't attempt to edit the example myself, partly because I don't know how :->, partly because I didn't want to be presumptuous or offend anyone.  But I'm quite sure that the code as it exists is bad; my empirical testing showed that very clearly.  It would be good for the code to more rigorously test itself; once the tree is constructed, it is straightforward enough to recursively check that each left/right subtree obeys the spatial partitioning imposed by the coordinate of the parent node.  In fact I wrote such test code in my project, along the path to understanding and fixing this bug, and it did flag the tree constructed by the original RosettaCode version of <code>find_median()</code> as having constructed an incorrect tree, and did pass the tree constructed by the algorithm once fixed as described above.  Together with a large test dataset that contains many exact duplicate coordinate values, such test code should allow the bug to be confirmed and then the fix validated. 
&mdash;[[User:Bhaller|Bhaller]] 17:18, 15 August 2017 (UTC)

: Those sound like good observations. I do not have a strong background in kd trees, myself, so I'll leave the code handling for someone else. That said, I'm wondering if it wouldn't make sense to de-dup the points before running the algorithm against them? (I understand that in some graphics applications you might have duplicate copies of the same points, but I'm wondering why that makes sense in the context of a kd-tree?)

: Also, you should sign your comments on discussion pages. The wiki will fill in the details for you if you use <nowiki>--~~~~</nowiki> when you post the comment. This time, though, I manually constructed your signature from information on the history page. Anyways, thanks for taking the time to think this through - it's always good when we have people exercising the code enough to find where it has problems. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:03, 15 August 2017 (UTC)

: Hi Rdm.  Thanks; I'm attempting to sign this post properly, we'll see what happens.  :->  Even if de-dup'ing the points were a good idea (which would depend on the application in which one is using the k-d tree; it definitely would not be a good idea for my particular application), it wouldn't be a fix for this bug, because here the bug occurs even if two points share just a single coordinate; they don't have to share all of their coordinates.  In other words, for 2-D data, the bug could be triggered by having points at (10,17) and (5, 17).  You can't de-dup those.  :-> --[[User:Bhaller|Bhaller]] ([[User talk:Bhaller|talk]]) 18:19, 15 August 2017 (UTC)

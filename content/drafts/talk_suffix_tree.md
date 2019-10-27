+++
title = "Talk:Suffix tree"
description = ""
date = 2015-08-20T23:33:57Z
aliases = []
[extra]
id = 13514
[taxonomies]
categories = []
tags = []
+++

==Different test case?==
Can we use maybe "banana" from the WP page instead? "rosettacode" simply doesn't have enough interesting repetitions. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 17:58, 17 May 2013 (UTC)
:Ok --[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 21:11, 25 May 2013 (UTC)

== definition? ==

The wikipedia definition for a suffix tree currently looks like this:

:The suffix tree for the string <math>S</math> of length <math>n</math> is defined as a tree such that:<ref>Gusfield, 1999, p.90.</ref>
:* the paths from the root to the leaves have a one-to-one relationship with the suffixes of <math>S</math>,
:* edges spell non-empty strings,
:* and all internal nodes (except perhaps the root) have at least two children.

:<references>
*{{citation
 | last1 = Barsky | first1 = Marina
 | last2 = Stege | first2 = Ulrike
 | last3 = Thomo | first3 = Alex
 | last4 = Upton | first4 = Chris
 | contribution = A new method for indexing genomes using on-disk suffix trees
 | location = New York, NY, USA
 | pages = 649–658
 | publisher = ACM
 | title = CIKM '08: Proceedings of the 17th ACM Conference on Information and Knowledge Management
 | year = 2008}}.</references>

But this can be satisfied by a tree with only a root where each node is a unique suffix.  Something is missing from this definition, and that something seems to have something to do with substrings which appear in multiple locations in the string.

What is the missing part of this definition? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:33, 23 May 2013 (UTC)
: On every node, each edge leading away from it must start with a unique letter.  So the suffix tree of string "aa$" can't have two edges "a$" and "aa$" leading away from the root node.  Instead it must have one edge "a" pointing to a second node, which in turn has two outgoing edges "a$" and "$".  The reason for unique leading letters is that, otherwise when matching substrings, one can't decide which edge to follow at each node in O(1) time. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 13:02, 24 May 2013 (UTC)
:: I'm still not seeing how to make this work in O(1) time (interpreting the big-O notation as representing worst case behavior).  Let's say our string is an arbitrary length sequence of a single letter (followed by the terminating character). Now we have a single node and the number of edges we have to pick between is O(n) and we need something like an oracle (or luck or a complete scan) to tell us which of them we need to follow. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:39, 24 May 2013 (UTC)

::: Eh, either an oracle, or a hash table maybe?  If you have a single node with n edges going out, but each edge begins with a unique letter, then map these letters to the edges.  During a string match, just see what next letter is, and do an O(1) lookup to get the corresponding edge. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 15:27, 25 May 2013 (UTC)

:::: If by "each edge begins with a unique letter" you mean "each edge begins with a uniquely different letter" then I agree. However, the example I proposed had each edge begin with a letter which is the same as every other edge (except for the final edge). This letter is in a sense unique (it's the only letter in the example before we decorate it with the final character) but it's probably better to say that we cannot be guaranteed that each edge begins with a different unique letter. Or, more concisely: did you read what I wrote? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:33, 25 May 2013 (UTC)

::::: Yes, I misread your example.  But then, your example is just like the one I provided earlier: "aa$", only with more "a"s.  If you can work out how to find the substring "a$" and "aa$" in that example, extending it to arbitrary length is a no brainer. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 01:54, 26 May 2013 (UTC)

:::::: I am not sure I can work that out. Let's take aaa$. The root node can have only one edge leading away from it, which has the label a.  I would expect that this node is similar to the tree for 'aa$' but the wikipedia page claims that except for the root node all nodes must have at least two children, and you have stated that only one edge leading from the aa$ node is allowed. I cannot think of any implementation which can satisfy both of these constraints. Perhaps because I am stuck in my thinking about this structure, I also cannot think of any useful algorithms that would use it. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:09, 26 May 2013 (UTC)

:::::: Actually, that banana$ example - enumerating the edges there, combined with a re-read of the definition and realizing that it's not making any O(1) guarantees at the nodes -- I now think that the suffix tree representation of 'aaaaaaa$' winds up enumerating all the edges which start with 'a' at a single node -- helps quite a lot. Thank you. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:08, 26 May 2013 (UTC)

::::::: No.  It's not "only one edge leading out is allowed", but "only one edge starting with each letter is allowed".  For "aaaa$", you have:
::::::: 1. from root node, edge '$' pointing to a leaf node, and edge 'a' pointing to internal node 1;
::::::: 2. from node 1, edge '$' pointing to a leaf node, and 'a' pointing to internal node 2;
::::::: 3. from node 2, edge '$' pointing to a leaf node, and 'a' pointing to internal node 3;
::::::: 4. from node 3, edge '$' pointing to a leaf node, and 'a$' pointing to a leaf node.
::::::: You can just add more nodes like 2 and 3 if you insert more 'a's to the string.  String matching works exactly like a trie lookup (actually, it ''is'' exactly a trie lookup, and it really only takes O(1) time to decide which edge to follow at each node ''unless you don't want to'' (one C implementation referrenced by the WP article stores edges in a linked list, but it could easily have used a hash table or a dynamic array.)

:::::::: Ok, this helped greatly, because it conveyed to me the definition of "edge" (or an important part of that definition). A study of the patricia tree link from the wikipedia page also helped. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:05, 27 May 2013 (UTC)

== definition (take 2) ==

I am still dissatisfied with the definition of suffix trees. This page refers to wikipedia and the wikipedia page's definition is quoted above.

My current dissatisfaction is that I see no reason, based on the wikipedia definition, to exclude an implementation, for banana, which looks like this:

 b-> 'banana$'
 a-> 'anana$', 'ana$', 'a$'
 n-> 'nana$', 'na$'
 $-> '$'

But, of course, this differs from the required result for this task. I can probably extract the definition from the example, given enough thought (and perhaps some or all of the implementations suggested on the wikipedia page can be made to match this example), but I would prefer a real definition for this task.

(A perhaps related issue is that the required result suggests that this structure is not a "tree" but a "directed acyclic graph" during construction, though of course that information can be discarded.) 

Anyways, can someone supply the missing part of the definition? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:43, 26 May 2013 (UTC)
:Well, we should discuss it in the wikipedia article.  Then come back here.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 15:45, 26 May 2013 (UTC)
:: Why? We are discussing this task, are we not? I am not prepared to discuss all of the issues raised by the wikipedia article. I do not even know that my above proposed example is wrong, in wikipedia terms - I only know that it's wrong for this task. (I would not object to someone with deeper experience with this subject addressing issues on the wikipedia page, but that does not mean that I have that depth of experience.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:47, 26 May 2013 (UTC)
::: I thought that seeking a definition for a normally well-defined algorithm was out of the scope of RC, but fair enough, I don't mind talking about it.  From what I understand, your example is incomplete.  The branch in "n" for instance should really have been "na", as all nodes in it start with "a".  Somehow in the definition there has to be a rule stating that all edges labels have to be the longest possible.   I don't know if it's clear in the wikipedia article.  Maybe it should be clarified there.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 12:05, 27 May 2013 (UTC)
:::: The distinction you draw in your opening sentence, in your paragraph here, is perhaps worth thinking about. Personally, I think, if the task page cannot convey the definition, and if the definition has its own jargon which must be studied, that the definition is very much in scope for RC. Sometimes an implementation can serve as a definition, but in this case the Racket implementation used an external library (which suggests an unbounded scope) and I do not know enough about perl6 to read its code and I do not know whether it would have been easier to learn the relevant perl6 or to just learn this algorithm (of course, both approaches have additional benefits, but since I imagine that I am not the only one that would want to implement this task, I opted to try to get the task definition clarified). 
:::: Anyways, the wikipedia definition does mention that every parent node other than the root has at least two edges leading out. Which (now that the defect in my thinking about what an edge is has been fixed) seems to address the "must be longest possible" issue. But emphasizing the point might not hurt. (As an aside, note that I almost never use trees with low children counts in coding because for my applications the constant multipliers on their costs almost always mean that another approach is better - roughly speaking, if I need a tree at all, for me "better" is something like sequence of trees (new content in a small mutable tree, old content in a larger more constant tree) where a node occupies most of an L1 cache and where the "edges" have roughly fixed size - in other words, lots of readers and very few writers - but that kind of reasoning does not seem to apply here. So things which should be just obvious for someone used to working with tall narrow tree implementations on a regular basis can easily escape my notice (similarly, things which seem obvious to me seem to be routinely overlooked by people specifying algorithms which favor skinny trees - and it's not that either approach is universally wrong it's just a reflection of different kinds information from different kinds of applications). Meanwhile, reading the wikipedia page was less than fruitful because that turned my attention to things like insertion operations (which probably means merging two of these trees, but maybe not) and I needed to focus on more basic issues.) 
:::: That said, my current impression is that the wikipedia article is confusing because it claims linear time for O(n log n) mechanisms. Average time might be linear (I do not know enough to determine that) but time is proportional to space and we need an additional copy of the text for every prefix variant that needs to be treated. I may be wrong here (I do not have proof) but thinking about using this mechanism to encode long, random bit strings seems to support this way of thinking. (And, if I am wrong, I would be very interested in seeing ''proof that the algorithm is O(n)'' that adequately covers space needed for random bit strings.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:57, 27 May 2013 (UTC)

==Definition (take 3)==

So the definition has been updated, both on Wikipedia and on RC.  I used http://www.cs.uoi.gr/~kblekas/courses/bioinformatics/Suffix_Trees1.pdf as a reference.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 17:37, 26 August 2013 (UTC)

== Valid implementations? ==

None of the displayed current implementations number their leaves, but numbering of leaves was a task requirement. Also, they are not displayed such that implicit numbering can be used (for example: node 0 first, node 1 second, node 2 third, ...) So can any of these implementations be considered valid for the current task definition? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:33, 20 August 2015 (UTC)

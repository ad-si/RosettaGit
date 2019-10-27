+++
title = "Talk:Power set"
description = ""
date = 2016-05-23T12:22:20Z
aliases = []
[extra]
id = 2886
[taxonomies]
categories = []
tags = []
+++

So how about the slow approach of coming up with random combinations and adding them to a set until it has 2<sup>n</sup> elements? The set object would have to guarantee uniqueness, but it would work eventually. To save some time you could add each element in the original list and the empty set to start then add sets of size 2 to n. --[[User:Mwn3d|Mwn3d]] 10:59, 16 June 2008 (MDT)

This is what your Ada code gives:
* A
* C
* E
* A
* A,C
* A,E
* A,C

: This is fixed. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:22, 14 October 2008 (UTC)

== Another copyleft issue? ==

About Smalltalk code... as written, is taken from a (odd?) blog hosted on the GNU Smalltalk site... so I suppose the content conforms to free doc license or GPL... check the link. I really believe there's no problem in publishing on RC; I will try to contact the author. If any problem, I'll remove it. --[[User:ShinTakezou|ShinTakezou]] 22:56, 1 April 2009 (UTC)
: Not copyleft, copyright.  No license is specified on the site, so it's implicitly All Rights Reserved.  Go ahead and contact the author, but I think we may be developing a problem of copy/paste submissions without proper thought given to source license prior to submission. I'm not sure what to do about it; the phrase "'''DO NOT SUBMIT COPYRIGHTED WORK WITHOUT PERMISSION'''" is sitting right there art the bottom of this edit form.  Maybe I can move it somewhere more visible, or use Javascript to pop up a warning dialog to anon users, and to logged-in users once (dependent on a cookie).  Annoying, yes.  But there's no way I can afford any kind of legal defense. --[[User:Short Circuit|Short Circuit]] 01:17, 2 April 2009 (UTC)
:: Maybe it needs to be explained in yet more detail "'''... NOTE: If you do not know the rights for the code, and you are not the codes author THEN DO NOT SUBMIT THE CODE!'''" ? --[[User:Paddy3118|Paddy3118]] 02:11, 2 April 2009 (UTC)
:::: It seems clear but it is not (not RC or GNU FDL fault, but contents' owners fault), and shouting makes it not clearer at all. Take a look at [[Spiral#J]] (''derivative'' work of a paper where the copyright is not explicit, so all right reserved...?)... (Unless the author is the author of the paper too)... And see also [[Multiplicative order]], where there's a "citation" of a sold book (64$ on amazon)... it should be removed (or there are other laws like: if the citation is less than N lines then it's ok? Or exercises can be copied? or there's the authors'/editor's permission?). If I fish, I fish from the GNU sea (and specify which sea), where I am sure 97% there can't be legal issue for a GNU-licensed site. --[[User:ShinTakezou|ShinTakezou]] 14:53, 2 April 2009 (UTC)
::: I've contacted the author, still not responding; anyway I posted it since I am almost sure he will be rather smooth about it (after all is a post for a community related to GNU Smalltalk, to "pump" it up too). Sounds irresponsible anyway, I know. Removing it, I will repost it when/if the author will answer. --[[User:ShinTakezou|ShinTakezou]] 10:27, 2 April 2009 (UTC)

::: Maybe this page (reached by clicking bottom-left copyright note "credits") helps, even though indeed it could not be too much clear to what "this site" can refer to (blogs and wiki are under the same domain afterall): [http://smalltalk.gnu.org/credits]

<blockquote style="padding:5px;background-color:#DDF">Verbatim copying and distribution of the contents of this site is permitted in any medium, provided this notice is preserved.</blockquote>

::: So the code could be reposted, provided I understand how/where to preserve the notice (needed even if this site is all licensed with GNU FDL?). Wainting feedback before reposting. --[[User:ShinTakezou|ShinTakezou]] 11:34, 2 April 2009 (UTC)

== CLISP: How can I run this example? ==

(powerset '(l i s p))

i.e., where do I store the function definition?
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 12:21, 23 May 2016 (UTC)

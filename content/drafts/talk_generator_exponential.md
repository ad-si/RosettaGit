+++
title = "Talk:Generator/Exponential"
description = ""
date = 2013-01-05T15:05:26Z
aliases = []
[extra]
id = 8823
[taxonomies]
categories = []
tags = []
+++

== "Draft" reasoning ==

Draft is mainly because I'm not sure if I have the task right, or that I've done a particularly good job of describing a generator either. IOW, review needed. –[[User:Dkf|Donal Fellows]] 14:03, 22 November 2010 (UTC)

Hi Donal, I tried to first understand your task description but got nowhere, so converted your Tcl to Python without understanding what it did, then printed the squares, then your filtered sequence to find that it is a list of the squares of positive integers with any that are also cubes of integers taken out. After working that out, your textual description then made perfect sense :-)

That is how it goes for me; sometimes I need the numbers to understand the text. I would wait to see what others think before acting on my tale, but I would suggest the task be formalised as maybe:
:1. Create a function returning a generator of the m'th powers of the positive integers starting from zero
:2. Use it to create a generator of:
::2.1. Squares.
::2.2. Cubes.
:3. Create a new generator that filters all cubes from the generator of squares.
:4. Drop the first 20 values from this last generator of filtered results then show the next 10 values
:Note that this tasks ''requires'' the use of generators in the calculation of the result.
::<small>(Preceding comment by [[User:Paddy3118|Paddy3118]])</small>

: Feel free to edit the task. Note that I was trying hard to make it so that it didn't require Stackless Python; that's a bit of an artificial restriction for someone coming from the Tcl world, where we didn't even bother trying to do coroutines until we had a stackless execution engine. Mind you, being able to put the yield deep inside is in general much more useful; the code between the outermost level and the point where it yields can be actually written to be unaware that it is in a generating context, which leads to cool tricks with coöperative multitasking. Maybe I'll work those into another task sometime. –[[User:Dkf|Donal Fellows]] 00:21, 23 November 2010 (UTC)

:: I look forward to the new task and will try a change to this tasks wording later today as I would still like to hear from others trying it first if possible :-)
::--[[User:Paddy3118|Paddy3118]] 05:07, 23 November 2010 (UTC)

==Name change request==

: Is this ready to be promoted to task? There are quite a few examples. [[User:MagiMaster|MagiMaster]] 04:53, 8 June 2011 (UTC)

::Yeah, it looks ok to me. Please rename to "Sequence generator" before promotion though, because "Generator" is a big generic, and there are other generators implementations that I can think of that are totally unrelated to this task, and the name Generator may cause confusion. --[[User:Markhobley|Markhobley]] 11:46, 8 June 2011 (UTC)

::: Looking at the WP [[wp:Generator_(disambiguation)|disambiguation page]], there seems to be no confusion as to the meaning of Generator within the Computing context, with all ''other'' forms having to be qualified. Since this is a computing site, I think the name would not overly confuse. --[[User:Paddy3118|Paddy3118]] 14:06, 8 June 2011 (UTC)

::: It's named “generator” because that's the name of a particular variation on the theme of coroutines (i.e., ones that go back to their caller while producing a value each time). That the values may be viewed as a sequence is tangential. Generators are interesting particularly because they can implement non-trivial transforms (e.g., where each input value can produce a variable number of output values) which can be hard to express with other approaches. –[[User:Dkf|Donal Fellows]] 14:26, 8 June 2011 (UTC)

:::: As you say, a generator is only one variation. We should maybe include this is the title, so we have forms Generator/A, Generator/B, etc. There are also generators that are not a generator at all in this sense, such as parametric generators and code generators, which all come under the context of computing. I still advise rename. --[[User:Markhobley|Markhobley]] 15:58, 8 June 2011 (UTC)
:::::Hi Markhobley, but Generator in our context is still enough. Which generator has equal prominance in our field that someone might mistake this for? We need to be wary of overlong titles, so please expand on your reasons. Thanks. --[[User:Paddy3118|Paddy3118]] 20:00, 8 June 2011 (UTC)

::::: Perhaps "sequential generator"?  But, yes, "generator" does have [http://www.sz-wholesale.com/uploadFiles/LPG-B%20Generator_802.jpg other meanings] --[[User:Rdm|Rdm]] 16:57, 8 June 2011 (UTC)

:::::: Yeah that sounds good, apart from the generator is this task is exponential, but I do like the sound of that for a sequential output version (such as the type used for generating ticket numbers or invoice numbers), if such a task was written. We could probably go with "Exponential generator", or "Sequence Generator/Exponential". --[[User:Markhobley|Markhobley]] 20:25, 8 June 2011 (UTC)

:::: Ok, tooting my own horn here a bit, but I put together a nice little Python module for [https://code.google.com/p/dastoobnet/source/browse/multi_m1/filterstack.py using stacked generators] as a filter chain. The docs might be illuminating in this discussion. [http://codepad.org/S7lYcnpO Here's its demo output], on Codepad.--[[User:Short Circuit|Michael Mol]] 15:16, 8 June 2011 (UTC)

::: Also, I think "Sequence generator" could be misleading, given that sequences can als be data structures which can be generated without a requirement for mutating state.  --[[User:Rdm|Rdm]] 15:01, 8 June 2011 (UTC)

:::: How about "Mutating state generator"? [[User:Markhobley|Markhobley]] 07:52, 13 July 2011 (UTC)

== C++ code ==

The C++ code should probably be an implementation of forward_iterator. I'd do it myself, but I don't know how (working on that is what led me back to this page) --[[User:Short Circuit|Michael Mol]] 14:08, 1 July 2011 (UTC)
:What, like this?
```cpp
#include <iostream>
#include <iterator>

template<int p>
class PowerSeq : public std::iterator<std::forward_iterator_tag, int> {
public:
    	PowerSeq(int first = 0 )  { pos = first; }
	int operator *() { return calc(); }

	PowerSeq<p>& operator ++() {
		pos ++;
		return *this;
	}

	PowerSeq<p> operator ++(int) {
		PowerSeq<p> tmp(pos);
		pos ++;
		return tmp;
	}

protected:
	int pos;
	int calc() {
		int out = pos;
		for (int i = 1; i < p; i++) out *= pos;
		return out;
	}
};

template<int in, int without>
class PowerSeqFilter : public std::iterator<std::forward_iterator_tag, int> {
public:
	PowerSeqFilter(int p1 = 0, int p2 = 0) { // NB. should check in != without
		s1 = PowerSeq<in>(p1);
		s2 = PowerSeq<without>(p2);
		if (*s1 == *s2) ++*this;
	};

	int operator *() { if (*s1 == *s2) ++*this; return *s1; }

	PowerSeqFilter<in, without>& operator ++() {
		do {
			++s1;
			while (*s1 > *s2) ++s2;
		} while (*s1 == *s2);

		return *this;
	}

	PowerSeqFilter<in, without> operator ++(int) {
		PowerSeqFilter<in, without> tmp(*s1, *s2);
		++*this;
		return tmp;
	}

protected:
	PowerSeq<in> s1;
	PowerSeq<without> s2;
};

int main()
{
	PowerSeqFilter<2, 3> f;
	for (int i = 0; i < 30; ++i, f++) {
		if (i >= 20)
			std::cout << i << " " << *f << std::endl;
	}
}
```

:Personally I find this unnecessarily complicated for the task. --[[User:Ledrug|Ledrug]] 01:58, 2 July 2011 (UTC)

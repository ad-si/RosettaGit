+++
title = "Talk:Inverted syntax"
description = ""
date = 2018-02-23T14:15:59Z
aliases = []
[extra]
id = 9840
[taxonomies]
categories = []
tags = []
+++

==More meaningful name?==
The current "Inverted syntax" doesn't mean much on its own. I don't have much of an alternative, but how about "Syntax/Trailing conditionals". --[[User:Paddy3118|Paddy3118]] 05:10, 1 June 2011 (UTC)

:Or "Syntax/Statement modifiers" --[[User:Paddy3118|Paddy3118]] 05:49, 1 June 2011 (UTC)

There is such a term as "inverted syntax", although in relation to programming, there are only a few people that use that term. I say the phrase is valid, of course,

Google for +"inverted syntax" +programming

[[User:Markhobley|Markhobley]] 14:35, 1 June 2011 (UTC)
:It's not really question of validity. It's a question of popularity. For instance, we chose "[[Greatest common divisor]]" over "Greatest common factor" partly because gcd was created first here, but also because greatest common divisor feels like it's a little more common. In any case, this is what we have redirect pages for. Try to think of what "people" call it (or would call it), and set up redirects for other names. --[[User:Mwn3d|Mwn3d]] 14:49, 1 June 2011 (UTC)

:I do not find the google results very convincing.  Most of the ones I looked at were synthetic text and the one that I found that did not relate to perl/python if statements used it apologetically, for an issue related to grammatical awkwardness in english.  That said, I do not think we need any sort of "absolute correctness", and I have no personal objection to this task being called whatever people think is right.  And if perl jargon is the way to go, so be it.  --[[User:Rdm|Rdm]] 14:56, 1 June 2011 (UTC)

:'''"Postfix flow control"'''?  Or "postfix control structures", or whatever.  Here's one relevant usage:  http://search.cpan.org/~elliotjs/Perl-Critic-1.116/lib/Perl/Critic/Policy/ControlStructures/ProhibitPostfixControls.pm .<BR> --[[User:DanBron|DanBron]] 18:58, 1 June 2011 (UTC)


We have suggestions. maybe people could argue for/against until we come to some conclusion? (P.S. what does the original task creator think)? --[[User:Paddy3118|Paddy3118]] 20:53, 1 June 2011 (UTC)

I think "Inverted Syntax" is the appropriate name and I use it in documentation that I produce here (which includes a terminology dictionary). However my inverted syntax definition, together with more than half of my dictionary is incomplete at this time.

FWIW I also link the alias "Reverse Polish Syntax" to the "Inverted Syntax" placeholder on my website (offline at the moment), but I am a strange person.

Mark.

==This is a nontask==

Answer this: What is the input data? How is it to be transformed?
: It's about language syntax, and has nothing to do with input data.  Also, are you the same rude person who was using 192.139.122.42 before, Kaz? --[[User:Ledrug|Ledrug]] 05:53, 20 September 2011 (UTC)

== perl confusion ==

Currently, the perl entry says:

:In perl, inverted syntax can also be used with the ternary operator:

:
```perl
$a = $ok ? $b : $c;      # Traditional syntax
($ok ? $b : $c) = $a;    # Inverted syntax
```


But:

<lang>$ perl -le '$ok= 1; $a= 9; $b= 2; $c= 3; ($ok ? $b : $c) = $a; print $a'
9

$ perl -le '$ok= 1; $a=9; $b= 2; $c= 3; $a = $ok ? $b : $c; print $a'
2

$ perl -le '$a=9; (1 ? 2 : 3) = $a; print $a'
Can't modify constant item in list assignment at -e line 1, near "$a;"
Execution of -e aborted due to compilation errors.

$ perl --version

This is perl, v5.10.1 ...
```


So, in some versions of perl the two statements are both legal, but not equivalent.

I imagine that the inverted syntax shown here could work in some version of perl6.  But I do not yet have any working implementation of perl6 yet, so I do not know if there are additional issues -- Raduko? Pugs? Not yet implemented?.  Anyways, I think this one could use some clarity. --[[User:Rdm|Rdm]] 14:21, 6 June 2011 (UTC)

:Ok, no, this is valid perl5, the issue was that the traditional and inverted examples were not accomplishing the same end.

:<lang>$ perl -le '$ok= 1; $a= 9; $b= 2; $c= 3; ($ok ? $b : $c) = $a;$,=" "; print $a,$b,$c,$ok'
9 9 3 1
```
 --[[User:Rdm|Rdm]] 14:30, 6 June 2011 (UTC)

:: To be clear, in the inverted syntax version, Perl is selecting which lvalue to assign to. This sort of thing is interpreted by some as proof that Perl is a language for people who value tricks over readability. –[[User:Dkf|Donal Fellows]] 14:47, 6 June 2011 (UTC)

::: More specifically, in one example Perl was being used to select one of two values to assign '''to''' the variable $a and in the other example, Perl was being used to select one of two variables ($b or $c) to be assigned '''from''' the variable $a.  And these two examples were presented side by side without comment, as if they were equivalent.  --[[User:Rdm|Rdm]] 18:27, 6 June 2011 (UTC)

::: Ok, now the perl entry says "However this may produce different results to the traditional syntax form", but I think that that is misleading.  The differing results in this case have nothing to do with the inverted conditional syntax.  The differing results are because the left and right values on either side of the assignment operator are swapped.  Both the traditional and inverted syntaxes would achieve the same result if they were both on the same side of the <code>=</code>. --[[User:Rdm|Rdm]] 20:00, 6 June 2011 (UTC)

== The C example ==

The C example is not right.  <code>do {stuff} while (...)</code> is quite different from <code>while (...) {stuff}</code>, and if you treat them as the same, it's going to have consequences.  For example, in the provided code itself, if you run the compiled program as <code>echo -n "" | ./a.out</code>, or press ctrl-D at the prompt, the <code>do{}while()</code> will access invalid memory and/or give a wrong answer. Don't tell readers <code>do{}while</code> is the same as <code>while{}</code>. --[[User:Ledrug|Ledrug]] 00:40, 4 September 2012 (UTC)

:Well, it doesn't say that do-while is the same, it only says it's the closest C comes to inverted syntax. True, it might mislead people not familiar with the language. --[[User:Oenone|Oenone]] 06:33, 5 September 2012 (UTC)

:: I don't know, given that the provided example has just the right bug, I'm not sure the original author ''didn't'' consider them exactly the same.  Anyway, since they are not the same thing, there's no point of mentioning them in this task "just because".  If someone ''has'' to put something under the C heading, it's probably better to stress their difference rather than similarity, along the lines of:
```c
#include "stdio.h"

int main(void)
{
	int i;

	// seemingly unreachable loop
	i = 0;
	while (i < 0) { i++; }
	printf("while{}: i = %d\n", i);

	puts("");
	i = 0;
	do { i++; } while (i < 0);
	printf("do{}while: i = %d\n", i);

	// if iterator is modified inside while()
	puts("");
	i = 3;
	while (--i) { printf("while{}: i = %d\n", i); }

	puts("");
	i = 3;
	do { printf("do{}while: i = %d\n", i); } while (--i);

	return 0;
}
```

== needumbrella=raining ==
Why would I abuse IF statements as this task seems to require?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:15, 23 February 2018 (UTC)

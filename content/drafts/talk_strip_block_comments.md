+++
title = "Talk:Strip block comments"
description = ""
date = 2013-07-14T05:08:57Z
aliases = []
[extra]
id = 8664
[taxonomies]
categories = []
tags = []
+++

== State Machine ==

Note that this problem definition strongly favors a sequential state machine implementation.  In particular, the example treatment for <code>/*/ stuff */</code> adds complexity to a parallel implementation.  --[[User:Rdm|Rdm]] 18:25, 2 November 2010 (UTC)
: We could bump it back to draft. Do you have a better definition? --[[User:Short Circuit|Michael Mol]] 18:40, 2 November 2010 (UTC)
::I believe the intent here is to emulate the treatment of comments in existing languages.  Note also that no example treatment is given for cases like <code>/*/*/*/*/*/</code> though presumably that sequence is equivalent to a single asterisk?

::: That depends if the language allows nested comments. The REXX language treats it as an open comment (i.e., not closed) and therefore a syntax error. Allowing nested comments allows the programmer to comment out a large section of code (which, of course, would contain comments). -- [[User:Gerard Schildberger|Gerard Schildberger]] 10:14, 1 April 2012 (UTC)

::In any event, I think that the specification should reflect the rigid character of the desired end.  I would eliminate the requirement that the comment delimiters be passed as parameters (or expand the definition of those parameters to include their aspects which have been implicitly specified... but I can not see a good way to do that).  I would also include explicit examples for the treatment of character sequences which look like delimiters but are not delimiters because of their position.
::If my interpretation is acceptable, I could tackle re-writing the task specification. --[[User:Rdm|Rdm]] 18:47, 2 November 2010 (UTC)

::: I've rewritten the task definition so that it is much more explicit about what is to be stripped, and I've made user-supplied delimiters into an extra-credit thing. (Some types of solution can do it easily and others cannot, but both are valid solutions to the core of the task IMO.) –[[User:Dkf|Donal Fellows]] 09:51, 11 November 2010 (UTC)

:::: Note that the issue with parameterized delimiters is that they have syntax associated with them, and syntax does not parameterize well.  (For example D and Cobra allow comments to be nested.  And passing delimiters as strings would not work well for Lua's block comments.)  --[[User:Rdm|Rdm]] 11:52, 11 November 2010 (UTC)
::::: I still think that a version of the 'strip comments' task that specified 'strip the comments your language supports' would be most illustrative of each language examined. :-| --[[User:Short Circuit|Michael Mol]] 14:03, 11 November 2010 (UTC)

:::::: Yet they would be doing totally different things. Some languages only have block comments, some only have to-end-of-line comments, some have both. Some even have neither (or at least it's non-trivial to determine what is really a comment with something like [[SNUSP]]…) –[[User:Dkf|Donal Fellows]] 16:35, 11 November 2010 (UTC)
::::::: Certainly. Hm. I think my goal in such a task is more to describe the nature of comments in the language by creating machinery to recognize those comments. Similar to [[Quine]] in that it's about comprehension of self's language, but with a focus on comments. It's not as consistent as recognizing specific classes of comments. --[[User:Short Circuit|Michael Mol]] 17:55, 11 November 2010 (UTC)
:::::::: Still, it's a different task to this one (which turns out to be a nice application of a modern RE engine), and for some languages, comment stripping could be rather difficult (classic [[BASIC]]'s <tt>REM</tt> for example). It's also tricky when a language has several different comment formats (modern [[C]], [[C++]], [[Java]] to name a few) because they can interact (and can interact with the language's string syntax too), which requires going to the power of at least some sort of lexer… –[[User:Dkf|Donal Fellows]] 20:40, 11 November 2010 (UTC)

== Definition of delimiters? ==

Do we have a (constant) definition of the delimiters to use or are they parameters to the stripping function? This is important because it leads to quite different solutions… –[[User:Dkf|Donal Fellows]] 09:56, 3 November 2010 (UTC)

Also, we don't have a definition of how (some) languages handle comments.

For instance:

say 66/*yuppers*/77

in the REXX language, <tt> 6677 </tt> is displayed to the screen.  

Some languages assume the comments acts like white space. -- [[User:Gerard Schildberger|Gerard Schildberger]] 10:24, 1 April 2012 (UTC)
: same with

```txt

say 66/* an enormous block comment

*/77

```

: yet to be analyzed: can any /* ... */ sequence be REMOVED? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:08, 14 July 2013 (UTC)

== J draft ==

I have not seen any advancement on this task, so I threw together a quick example where the comment delimiters are hardcoded (which, in my opinion, is a good design decision for this task).

That said, the code uses a state machine, so probably deserves a bit of comment.

First, here is the version of the code I am commenting on.  (The main page might easily be updated with a different version):


```j
str=:#~1 0 _1*./@:(|."0 1)2>4{"1(5;(0,"0~".;._2]0 :0);'/*'i.a.)&;:
  1 0 0
  0 2 0
  2 3 2
  0 2 2
)
```


The core of this code is a state machine processing a sequence of characters.  It first classifies characters into three classes:  '/', '*' and everything else.  These character classes correspond to the three columns of numbers you see there.  ('/' corresponds to the left column and '*' corresponds to the middle column.)

The rows of numbers correspond to states.  State 0 corresponds to the top row, state 1 corresponds to the next row, ..., state 3 corresponds to the bottom row.

The previous character's state (which is initially 0) and current character class are used to determine the current character's state.  (These "new state numbers" are the numeric values you see arranged in that four row table.)

We then find characters in the original text which have a state less than 2 and whose neighbors on both sides also had a state less than 2.

And we throw out everything else.

== Badly defined task ==

Stripping comments generally requires knowledge of the language involved.  Simply taking out COMMENT_BEGIN up to COMMENT_END won't garantee what's left still conform to syntax (due to nested comment, markers being part of a literal string, etc), the only proper way is by parsing the text according the syntax, which is no doubt too much work.  As an example, the C code can't strip its own source properly, which is pretty comedic. --[[User:Ledrug|Ledrug]] 06:57, 12 August 2011 (UTC)

:I guess you have to think of it in the context where stripping out those comments in the prescribed fashion leaves behind viable text?
:I have used domain-specific languages where the comment syntax was purposely defined so that simple scripts could strip them out successfully. They didn't allow nested comments and had a rule where either the whole of the comment where to be treated as a single space or the whole of the comment where to be treated as deleted and could merge text either-side of it to form a token. --[[User:Paddy3118|Paddy3118]] 07:18, 12 August 2011 (UTC)
:: You can't garantee the delimiters really marks a block of comment if they are allowed to appear inside a literal string verbatim by the language, which is almost all the languages.  Think <code>print("# not comment")</code>.  For any language where white space is not significant, you pretty much have to parse the source from begining and tokenize everything to be sure /* really starts a comment.  Simply match-and-replace will not do, just like how everyone knows using regular expression to strip HTML tag is wrong (though everyone does it any way). --[[User:Ledrug|Ledrug]] 07:35, 12 August 2011 (UTC)
::: Sure you can, and you can do so without any knowledge of string syntax.  Though this task does not do that.  For example, you can specify that comments extend to the end of the line and that on any line with a comment, the comment is the shortest string that matches the comment pattern.  This mechanism would be similar to the concept of escape characters.  It also bears resemblance to most any comment syntax which does not allow for nested comments.  --[[User:Rdm|Rdm]] 11:25, 12 August 2011 (UTC)
:::: You lost me. So you can just strip from # to end of line in the example <code>print("# not comment")</code>? --[[User:Ledrug|Ledrug]] 19:38, 12 August 2011 (UTC)
::::: That would be syntactically invalid.  But you could fix it: <code>print("# not comment")#</code> --[[User:Rdm|Rdm]] 19:58, 12 August 2011 (UTC)
:::::: Ok consider this: <code>/* printf("/* is comment") */</code>.  I guess you'll call it "nested comment", which is not: as I said above, I was talking about where comment delimiters can be part of literal strings, but I guess your rules won't really allow that because now real comment markers would have to pair up with fake markers that are part of a string (which makes no sense).  For a real example, just see what happens if you use the C code on task page and try to strip its own source code. --[[User:Ledrug|Ledrug]] 20:07, 12 August 2011 (UTC)

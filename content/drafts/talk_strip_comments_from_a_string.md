+++
title = "Talk:Strip comments from a string"
description = ""
date = 2016-06-18T18:02:47Z
aliases = []
[extra]
id = 8641
[taxonomies]
categories = []
tags = []
+++

==General==
Two thoughts. First, wouldn't a comment notation supporting ranges (i.e. /* ... */) as well as truncate tokens ( #, ;, // ...) be more interesting? Otherwise, I'd suggest renaming this task to [[Truncate a String]].  Second (and this is just an idle idea more than anything else), a task for which a language stripped comments (per its own language's rules; // and # for PHP, // and /* */ for C++, etc) would be ''very'' interesting, as it combines demonstrating string processing as well as the language's own comment syntax. --[[User:Short Circuit|Michael Mol]] 13:53, 30 October 2010 (UTC)


###  Check your data 

I have done this sort of thing in the past and the problem statement works if the format of what is being parsed does ''not'' allow the comment indicating character to be part of valid data. 

As soon as you start to get, for example, arbitrary character strings then you need a more sophisticated parser that allows a comment marker character to appear in a string without the parser treating it as the start of a comment. --[[User:Paddy3118|Paddy3118]] 04:40, 12 December 2010 (UTC)

: ... Except that some languages treat nested comments (comments within comments) as legal.  PL/I doesn't, REXX does, for instance.   It would be interesting to note which languages support nested comments. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:34, 26 April 2013 (UTC)

: Having nested comments allows a programmer to ''comment-out'' large sections of code, which of course, most assuredly contain comments. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:34, 26 April 2013 (UTC)

==Wayward space==
What should happen to the space before the comment marker? The task description seems to silently remove it. --[[User:Paddy3118|Paddy3118]] 06:15, 28 November 2010 (UTC)
:I updated the description, because the original D implementation did not remove those spaces. --[[User:Rdm|Rdm]] 12:11, 28 November 2010 (UTC)
::Cheers. --[[User:Paddy3118|Paddy3118]] 15:17, 28 November 2010 (UTC)

Whitespace before the comment marker should be removed. I have updated the task description to clarify this.

Cheers,

[[User:Markhobley|Markhobley]] 15:43, 28 March 2011 (UTC)

:Should we treat lines without comments as if they ended with an empty comment?  (In other words, should trailing white space be removed from lines without comments?  Or does that not matter?) --[[User:Rdm|Rdm]] 19:59, 29 March 2011 (UTC)

==Comments in Strings==
Are we to assume the comment character (# or ;) is not going to ever be inside a string "#" or that there are no strings in whatever language this is?  If there are strings, do we assume '#' is not a valid strings in this language? what about Ruby where strings can often be delimited by meaningless characters?  In order to properly "strip comments" we need to know what we're stripping them from.  --[[User:Deltree|Deltree]] 16:04, 27 July 2011 (UTC)
:So far, it seems like comments may contain comment characters, but that the non-comment text cannot quote or escape comment characters. This does not seem very useful.  On the other hand, normally comment stripping happens inside of a parser which has mechanics to ignore comment characters when they appear in the wrong context.  So the interesting task here is probably [[Parse_EBNF]].  --[[User:Rdm|Rdm]] 17:37, 27 July 2011 (UTC)
::Except [[Parse_EBNF]] task is a little ''too'' elaborate.  Here we want a parser based on some BNF, while that task requires ''creating'' a parser based on some BNF, a program writing a program sort of thing. --[[User:Ledrug|Ledrug]] 18:53, 27 July 2011 (UTC)

== White space ==

Besides various problems above, why the requirement to trim whitespace?  In particular, what's the point of trimming the leading whitespace?  If whitespaces are significant in the underlying language(fortran, python, etc), trimming them mangles it; if not, then what does trimming achieve at all? --[[User:Ledrug|Ledrug]] 02:35, 10 September 2011 (UTC)
:See the section above on "Wayward space". I too think it odd to trim spaces too, but just took it as a requirement of the task that seemed odd to me, but that's what is called for. --[[User:Paddy3118|Paddy3118]] 06:04, 10 September 2011 (UTC)


###  29 of 36 languages were incorrect? 

At [http://rosettacode.org/mw/index.php?title=Strip_comments_from_a_string&oldid=119409 2 September 2011], I verified that 8 languages (C, C++, Java, Perl, Python, Ruby, sed, UNIX Shell) were incorrect, all for not trimming whitespace by 29 March 2011 rules. I suspect that 21 more languages (Ada, ALGOL 68, AutoHotKey, Clojure, D, Delphi, F#, Fantom, Fortran, Go, Haskell, Icon and Unicon, Inform 7, Lua, OCaml, PicoLisp, PL/I, PureBasic, REXX, Scheme, TUSCRIPT) might be incorrect for the same reason.

:: You may suspect incorrectness all you want, but the REXX example does indeed trim whitespace.   It's better to ask before assuming errors.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:27, 15 July 2015 (UTC)

Several contributors solved the task before 29 March 2011, when the current rules appeared. I believe that most of the existing solutions became incorrect at 29 March 2011.

I propose to delete the 29 March 2011 rules, and add new rules: (1) Code may optionally remove whitespace before the comment marker. (2) Code must not remove any whitespace from the beginning of the line (unless the line begins with whitespace before a comment marker). (3) Code must not remove any whitespace from any line that has no comment. (4) Old examples are exempt from the new rules. --[[User:Kernigh|Kernigh]] 19:46, 10 September 2011 (UTC)

:As Ledrug and I have stated before, removing '''any''' whitespace outside the comment marker just seems wrong. I guess this task shows the kind of problems that ensue when task goals are changed outside of the draft task status. --[[User:Paddy3118|Paddy3118]] 20:20, 10 September 2011 (UTC)

: To avoid misleading readers, I think the task desc should also stress that, although for this task it's ok to assume the first appearance of a marker unambiguously identifies a line comment, it's definitely not true in a realistic sense.  Most languages allow comment markers in literal strings unescaped, while parsing literals is language specific: all kinds of quoting rules; quote marks may appear in a literal, too, and parsing them is not easy even for some braindead languages that try to make it simple -- matlab comes to mind -- not to mention that there are crazy languages like perl where, say, '#' can be used for other purposes (<code>s###</code>, for example). --[[User:Ledrug|Ledrug]] 20:38, 10 September 2011 (UTC)

:: Not to mention languages that accept '''#''' as a symbol (either for labels, variable names, subroutine/function names). -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:48, 3 September 2012 (UTC) 

-----

Nobody seems to be bothered by the lack of a definition of '''white space'''  (also, sometimes, '''whitespace'''). '''White space''' almost always includes blanks, and sometimes spaces  (yes, Virgina, there is a difference). 

Sometimes, white space includes such things like:

::*   blank(s)
::*   sp      (space)
::*   ht      (horizontal tab)
::*   tab     (usually, the same as HT)
::*   vt      (vertical tab)
::*   cr      (carriage return)
::*   ff      (form feed)
::*   np      (new page)
::*   lf      (line feed)
::*   nl      (new line)
::*   nul     (null character)
::*   esc     (escape)
::*   eof     (end-of-file)
::*   can     (cancel)
::*   bel     (bell)
::*   bs      (backspace)


::*   soh     (start of heading, console interrupt)
::*   eot     (end of transmission)
::*   etx     (end of text)
::*   enq     (enquiry)
::*   ack     (acknowledge)
::*   si      (shift in)
::*   so      (shift out)
::*   etb     (end of transmission block)
::*   syn     (synchronous idle)
::*   dle     (data link escape)
::*   dc1     (device control 1)
::*   dc2     (device control 2)
::*   dc3     (device control 3)
::*   dc4     (device control 4)
::*   em      (end of medium)
::*   fs      (file separator)
::*   gs      (group separator)
::*   rs      (record separator)
::*   us      (unit separator)
::*   del     (delete)



Of the above, the first sixteen or so are commonly known and used.  Essentially, anything below a '''blank''' in ASCII or EBCDIC   ''may''   be considered a control character, and in addition, ASCII also has '7f'x (DEL).  Note also that some control codes have more than one mnemonic just to keep things interesting.

 I think whitespace (in the task's description should be defined or the word '''BLANKS''' should be used instead.

It appears that most languages seem to trim blanks, not white space anyway. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:45, 3 September 2012 (UTC)

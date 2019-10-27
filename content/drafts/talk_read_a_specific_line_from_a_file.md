+++
title = "Talk:Read a specific line from a file"
description = ""
date = 2012-03-14T04:08:50Z
aliases = []
[extra]
id = 10146
[taxonomies]
categories = []
tags = []
+++

==...put it in a variable?==
Why does it need to be put in a variable?  It appears that none of the first three implementations actually do that.  --[[User:TimToady|TimToady]] 00:14, 24 July 2011 (UTC)
::I know what you mean. If we are going to read a line within a program, we need to be able to store what we have read in order to make use of it. A variable would usually be a good place. I have added "or in memory (for potential future use within the program if the code were to become embedded) ". I'm not sure whether we need the bit in brackets, and I am open to suggestions on other possible wording, if you think we need something else. [[User:Markhobley|Markhobley]] 00:45, 24 July 2011 (UTC)
:FWIW The Tcl solution looks like it is using a variable named "line", which is an acceptable solution for the purpose of this task. I cannot read the other two languages, so I can't comment on the solutions. [[User:Markhobley|Markhobley]] 00:59, 24 July 2011 (UTC)
::Looking again, I guess that the Perl6 solution is missing some file opening code, and closing code and is capturing to standard output. It probably just requires a small change. I have not got round to looking at Perl6 yet, so I am not skilled enough to make such changes at this time.[[User:Markhobley|Markhobley]] 01:06, 24 July 2011 (UTC)
:::I guess my point is that assuming there's a variable assumes you're using an imperative language.  Perl 6 is more of a functional language, and lines (without other arguments) returns a lazy list of the lines from the file named on the command line, or STDIN, much like <tt><></tt> does in Perl 5.  The open and close are implicit in the generator.  The <tt>[6]</tt> subscript selects the 7th element of the lazy list and returns it.  Then we print that.  Storing it in a variable would be an unnecessary step, and rather unidiomatic, and not needed for the main point of the task.  --[[User:TimToady|TimToady]] 01:44, 24 July 2011 (UTC)


==sparse files== 
Some operating systems allow sparse files, that is, a file may have (say) the 1st, 2nd, 500th, and the one millionth record defined  (four actual records), but it appears that there are one million records in the file, and almost all of them null (empty). 


 The REXX language was developed on such a system, and as such, the built-in function LINEIN doesn't treat the reading of a none-existant record an error. 

[[User:Gerard Schildberger|Gerard Schildberger]] 04:04, 14 March 2012 (UTC)

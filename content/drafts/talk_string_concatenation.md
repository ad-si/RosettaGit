+++
title = "Talk:String concatenation"
description = ""
date = 2013-01-28T11:12:59Z
aliases = []
[extra]
id = 8746
[taxonomies]
categories = []
tags = []
+++

SQL has string concatentation using infix || in the style of PL/1 but does not have variables as such.


```sql
create table foo ( a varchar(10),b varchar(10));
insert into foo values ('hello','dolly'),('l8r','g8r');
select 'greeting is ' ||a ,a ||' '|| b from foo;
```

:What about select into? I was never sure how SQL-fu like that worked, though. --[[User:Mwn3d|Mwn3d]] 04:58, 16 November 2010 (UTC)
::<code>select into</code> seems to be a non-standard extension to sql which is mostly used for creating backups of data in existing tables.  Here, though, we have three lines -- the first two lines are analogous to variable declaration and variable assignment, and the last line illustrates the task requirement.  That said, note that this || mechanism is the ansi standard way to concatenate string (introduced in sql-92) and some implementations of sql (such as mysql) [http://troels.arvin.dk/db/rdbms/#functions-concat do not support this syntax]. --[[User:Rdm|Rdm]] 19:28, 16 November 2010 (UTC)
::: Stick with the ANSI standard; the deviants can have their own entries (or at least their own sub-solution). –[[User:Dkf|Donal Fellows]] 23:40, 16 November 2010 (UTC)

== Rexx || ==

|| may be piping in some other language(s), In Rexx it's called concatenate!
abc'xyc' is called abuttal.
While 'abc'def worke like a charm
'Walter'b
or
'Johnny'x
do not
Rexx takes the b and x to mean bit or hex string
Beware of b and x as variable names
--[[User:Walterpachl|Walterpachl]] 11:12, 28 January 2013 (UTC)

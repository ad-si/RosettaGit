+++
title = "Talk:Walk a directory/Recursively"
description = ""
date = 2017-06-19T21:30:57Z
aliases = []
[extra]
id = 2169
[taxonomies]
categories = []
tags = []
+++

I think the precise statement of the problem is a little too restricted.  In some cases it's possible to walk the directory tree, printing all the matching filenames with somewhat less code than it is to walk the tree in order to collect a list of the matches or to perform other operations on them.  Also there are many criteria on which one might wish to select files beyond just file names. [[User:JimD|JimD]] 19:13, 15 October 2007 (MDT)
:This is true.  The instructions could be changed to call a function, but I've been hesitant to use that abstraction for the sake of simplicity.  Really, I'd just like to leave a comment along the lines of /* do something here */ in the appropriate place, but I'm not sure how to word that. --[[User:Short Circuit|Short Circuit]] 20:00, 15 October 2007 (MDT)
:: The current text of the task:
  Walk a given directory tree and print files matching a given pattern.
:: My suggestion:
  '''Walk a given directory tree, calling a function for every filename which matches
  a given wildcard, UNIX [[glob]], or [[regex]] pattern (whichever is easiest for the given language).'''
:::Sounds fine to me. Go ahead and make the change.  I'll add filling in what I know of globs and regex later.
: Question: would we want to create a small set of more complex tree walking tasks which ask how one would do things like: follow (or refrain from following) symbolic links, refrain from crossing UNIX/Linux mount points, select files based on their ''stat()'' criteria (such as link count, dates, ownership, group association, permissions, etc) or on their contents?
::I could see the components (creating/identifying symbolic links, creating hard links, getting link counts, getting create/modified dates, getting and setting file and directory ownership, getting and setting group association, getting and setting permissions and identifying mount points) as their own tasks.  None of them are even UNIX-specific; Even Windows supports symbolic and hard links.  But building '''find''' alternative is too complicated for your average task, or even a [[:Category:Puzzles|puzzle]]. --[[User:Short Circuit|Short Circuit]] 21:54, 16 October 2007 (MDT)

----

Is the problem to just find filenames, excluding the path, that match the pattern? That's usually the example, and in that case, many of the snippets here have bugs because they apply the regex to the entire path, not just the filename. ''(Unsigned comment added by 69.211.121.158 at 22:21, 24 October 2010 69.211.121.158))''
: Does someone want to clarify the task description? If we're walking a directory tree, we would normally be considering entries within each directory we examine. It may also be worthwhile setting up an example directory structure with anticipated results. (Particularly considering things like matches against directory names, such as a search for \*\.txt, and there being a 'files.txt' directory in the tree.) --[[User:Short Circuit|Michael Mol]] 13:09, 25 October 2010 (UTC)

== symlinks? ==
Just curious, how many of these hand rolled solutions can deal with a symlink (or hardlink) to a higher directory (i.e. cyclic graph)?  If encountering one, would it bail with "pathname too long", or loop until memory exausted? --[[User:Ledrug|Ledrug]] 04:56, 13 June 2011 (UTC)

:'''In Python,''' the [http://docs.python.org/library/os.html#os.walk full docs for os.walk] show that by default, symlinks are not followed. There is an optional parameter that allows symlinks to be followed and a banner note states:
::'''Note:''' Be aware that setting ''followlinks'' to True can lead to infinite recursion if a link points to a parent directory of itself. walk() does not keep track of the directories it visited already.
:There is also a note and '''warning about using relative pathnames''' and the assumption that code will not change the current directory during calls to os.walk. --[[User:Paddy3118|Paddy3118]] 06:04, 13 June 2011 (UTC)

:: Yes, I have no doubt a proper library would have thought about it.  But some of the code samples didn't use a library and just used recursion, which can have funny results.  I guess it's ok for examples here, though.  And I don't know why I said "harlink" above, bah. --[[User:Ledrug|Ledrug]] 06:57, 13 June 2011 (UTC)

::: In every OS that I am aware of, a symlink needs special treatment to be read (if they are supported at all), and a hardlink to a directory is an error.  --[[User:Rdm|Rdm]] 12:10, 13 June 2011 (UTC)

:::: It's a matter of whether the language you use by default returns true for a directory test on a symlinked dir.  For example, in bash:
::::
```bash
if [ -d /some/symlink/to/dir ]
then echo it is a dir
fi
```

::::will say it is a directory, and you can do globbing on it just like normal dirs.  You'd have to specifically remember to check if it's a symlink.  --[[User:Ledrug|Ledrug]] 20:53, 13 June 2011 (UTC)
:::::Ok, yes, bash is very inconsistent about how it handles symlinks.  I remember talking with the bash maintainer about this, a number of years ago, and felt unsatisfied afterwards.  In particular, <code>cd ../example</code> can fail, while <code>cd -P .; cd ../example</code> can succeed, because bash implements its own rules for about what directories are in the context of symbolic links, which conflicts with that of the underlying operating system.  But that's a bash issue -- I do not know of any other language which suffers from that design.  (And bash can rely on <code>find</code> or <code>ls</code> which implement sane treatment of symbolic links.)  --[[User:Rdm|Rdm]] 23:37, 13 June 2011 (UTC)
::::::Eh, not really a bash specific problem.  E.g.
```txt
perl -e 'print "is a dir\n" if -d "/some/symlink/dir"
```
 or zsh
```txt
if [[ -d /some/symlink/dir ]]; then echo "is a dir"; fi
```
 basically
```c
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

int main() {
        DIR * x = opendir("/symlink/dir");
        if (x) {
                printf("open ok\n");
                closedir(x);
        } else {
                printf("open not ok\n");
        }
        return 0;
}

```
 will succeed on openning a symlinked dir (it makes sense), and that's the thing one needs to check for. --[[User:Ledrug|Ledrug]] 00:53, 14 June 2011 (UTC)
:::::::Ok, you are right that -d under perl does the wrong thing for a terminating recursive traversal.  That said, the perl implementation does not seem to be using -d for that purpose.  Similarly, the C implementation uses stat, and not opendir, to recognize directories.  Even the zsh implementation is doing the right thing.  But now I am wondering: does zsh also have the issue where paths such as ../foo are manipulated by the shell before being passed to the operating system? --[[User:Rdm|Rdm]] 12:22, 14 June 2011 (UTC)
::::::: This script
```bash
#!/usr/bin/zsh
cd /tmp
mkdir dir1 dir2
ln -s dir1 symlink

cd dir1; pwd
cd ../dir2; pwd
cd ..; pwd
cd symlink; pwd
cd ../dir2; pwd
```
says: <lang>/tmp/dir1
/tmp/dir2
/tmp
/tmp/symlink
/tmp/dir2
```

Which doesn't seem to do anything unexpected. --[[User:Ledrug|Ledrug]] 17:20, 14 June 2011 (UTC)

::::::: I was thinking more like this:
```bash
#!/usr/bin/bash
mkdir dir1 dir2
cd dir2
ln -s ../dir1
mkdir dir2
cd dir1
(cd -P .; cd ../dir2; pwd -P)
(cd .; cd ../dir2; pwd -P)

```
  When I run that in the directory /tmp/t, I get:  <lang>/tmp/t/dir2
/tmp/t/dir2/dir2
```
  That said, the behavior now is much better than what it used to be.  If I remove the second mkdir line, and delete the directories and run that again, I get <lang>/tmp/t/dir2
/tmp/t/dir2
```
 where once upon a time if I did not use cd -P . I would get an error trying to <code>cd ../dir2</code>. --[[User:Rdm|Rdm]] 20:14, 14 June 2011 (UTC)
::::::: Tested your script in zsh, result was exactly the same.  --[[User:Ledrug|Ledrug]] 20:32, 14 June 2011 (UTC)

== Be Careful: Not Here Only ==

Regarding ''"Note: Please be careful when running any code examples found here"'': that applies to pretty much every problem page you see on Rosetta!!! Any program that is not completely trivial and transparent (to your eyes) can be hiding something unsavory.--[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 21:11, 19 June 2017 (UTC)

: Generally speaking, you should try to comprehend what the code is doing. That said, I have not noticed any abusive code here, yet. I have seen quite a lot of abusive users but mostly they are trying for search engine pollution (which we delete, of course) rather than investing their time into getting developers run bad code. Still, the whole point of this site is to understand what's going on in a variety of contexts, so putting some effort into that is good practice for a variety of reasons. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:30, 19 June 2017 (UTC)

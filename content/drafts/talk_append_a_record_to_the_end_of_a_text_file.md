+++
title = "Talk:Append a record to the end of a text file"
description = ""
date = 2011-11-14T10:59:25Z
aliases = []
[extra]
id = 10508
[taxonomies]
categories = []
tags = []
+++

== Rationale ==
Basically I was looking for an example of appending an actual record to a file and found no such task.
<div><small>''(This was written by [[User:NevilleDNZ|NevilleDNZ]] ([[User_talk:NevilleDNZ|Talk]] | [[Special:Contributions/NevilleDNZ|contribs]]) at 10:59, 12 September 2011)''</small></div>

: Still needs some work IMO; it's got lots of small bits and pieces jumbled together without quite a coherent story. It's probably not that hard to improve it to the point of being good though.
: And as a side note, POSIX systems have the O_APPEND flag to the open() syscall which makes this trivial (if it is exposed by the language) provided appropriate flushing is used. (Even better, the flag is inheritable by subprocesses; that's a ''very'' useful thing at times.) By contrast, on Windows you need to lock the file to do this safely and that's error prone and much more complex in practice. Moreover, locks aren't inheritable (of course). Damn, but I wish the folks at Redmond actually ''understood all'' of POSIX properly, as it has some really wonderful things in there. –[[User:Dkf|Donal Fellows]] 13:58, 12 September 2011 (UTC)
:: I am not sure I agree.  As I understand it, under unix, locking is advisory -- you cannot enforce it on other programs, and only if every program accessing the file uses the same mechanism do locked files get protected (fcntl is one example but other mechanisms do get used, especially if people are attempting to support nfs).  That said, if processes are designed so that collisions are rare, locking might not be necessary.  Also, often unix's atomic file rename mechanism is used to avoid locking issues.  Meanwhile, under windows, the default behavior is that only one program can have the file open for writing -- by default, you get an error when another program tries to open the file for writing.  The windows behavior can be painful, and there's no atomic file rename, but any locking mechanism that gets enforced by the OS can be painful to work with.  Meanwhile, all of these characteristics are properties of the operating system and not of the language.  --[[User:Rdm|Rdm]] 02:16, 13 September 2011 (UTC)

::: I'm under the same impression as Rdm.  Is this about appending? Or some kind of record locking? Is it about append in native I/O facilities?   I was also trying to understand the rationale behind the table and that the table has been left as somewhat vague.  Normally, I would use a table for very specific results/observations and a bullet list for less specific results/observations.  We are already seeing variations on the table content - which is fine if that's what's intended.  For instance a couple of the solutions cite that they are guaranteed for multitasking, yet I don't see that they are doing anything other than relying on the o/s or library via open/close.  --[[User:Dgamey|Dgamey]] 13:38, 26 September 2011 (UTC)

::: There is also [http://www.mjmwired.net/kernel/Documentation/filesystems/mandatory-locking.txt mandatory locking], but you don't really want that. It turns out to be a Bad Idea almost all the time since it allows any process to block all others (among various reasons). –[[User:Dkf|Donal Fellows]] 19:50, 27 September 2011 (UTC)

== CSV? ==

Is it supposed to be a CSV file? The description doesn't say anything about the file format, unless "passwd" implies some sort of format. Maybe the given records should be specified in the description. --[[User:Mwn3d|Mwn3d]] 23:57, 12 September 2011 (UTC)

:Agreed.  Will do.

:The essence of the task is "Append a record to the end of a text file".  Crudely speaking:
:* A record is something that has multi fields.
:* A text file is something that can be edited with "notepad"
:** in essence it does not contain "raw binary integers" etc. 
:** The text file "record" represents the original record fields.
:I want to avoid simply appending random text to the end of an arbitrary file as nominally programs are (concurrently) manipulating (and storing) structures of data.

:[[User:NevilleDNZ|NevilleDNZ]] 01:44, 13 September 2011 (UTC)
::The given format does not look like CSV. While "colon-separated values" technically shortens to "CSV", [[wp:Comma-separated values|CSV]] looks like this:
 value11,value12,value13 with spaces,value14
 value21,value22,value23,value24 with spaces
::Commas are the only separator. The colons in the given format make it look like something else that we shouldn't call CSV. Is that what etc/passwd looks like? It would probably be best to say something like "the record's values are separated by commas, with values in composite items separated by commas (similar to Linux's /etc/passwd file)." If at all possible you might want to make the data structures involved in the record very very simple (i.e. no structs). That would make sure the focus is on what's in the task title. Stringifying a custom data structure is a different task (which we might not have yet...). --[[User:Mwn3d|Mwn3d]] 02:23, 13 September 2011 (UTC)

:::This passwd file and linux or unix or posix passwd files would be best thought of as having records which have values which are separated by colons (and records are terminated by newlines).  The contents of a certain value within each record can be thought of as having its own internal record structure which is separated by commas (and which cannot contain colons).  --[[User:Rdm|Rdm]] 17:32, 21 September 2011 (UTC)

== Confused ==

Currently, the initial password file contents are specified as:

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Source record field types and contents.
|-
!account||password||UID||GID||fullname,office,extension,homephone,email||directory||shell
|-
!string||string||int||int||struct(string,string,string,string,string)||string||string
|-
|jsmith||x||1001||1000||Joe Smith,Room 1007,(234)555-8917,(234)555-0077||jsmith@rosettacode.org||/home/jsmith
|-
|jdoe||x||1002||1000||Jane Doe,Room 1004,(234)555-8914,(234)555-0044||jdoe@rosettacode.org||/home/jsmith
|}

But this conflicts with the [http://www.cyberciti.biz/faq/understanding-etcpasswd-file-format/ usual password file format]

Specifically the sixth field is supposed to be the home directory, and not an email address.  Email address goes in the fifth field.  And the seventh field is supposed to be the user's shell, and not their home directory.  Also, two users with different user ids should not share the same home directory

So I think the initial contents should have been specified as:

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Source record field types and contents.
|-
!account||password||UID||GID||fullname,office,extension,homephone,email||directory||shell
|-
!string||string||int||int||struct(string,string,string,string,string)||string||string
|-
|jsmith||x||1001||1000||Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org||/home/jsmith||/bin/bash
|-
|jdoe||x||1002||1000||Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org||/home/jdoe||/bin/bash
|}

Something similar applies for the appended record. --[[User:Rdm|Rdm]] 02:26, 13 September 2011 (UTC)

== Changed ==

I changed the task so that it was self consistent.  We can probably delete a lot of the above commentary now.  Some of the existing implementations should perhaps be marked incorrect.  --[[User:Rdm|Rdm]] 14:00, 26 September 2011 (UTC)

ThanX for spotting the email/home/shell error .  I've fixed the C and python code.

BTW: On unix (and linux) single writes on files opened with appends are guaranteed to append.  And the syscall to "write" is atomic.  Hence locking is not required.  HOWEVER both the C code and the Python code use printf, and I confess that I am not 100% sure if a "fflush" is called when a printf includes a "\n".  I'll take a look.

(found: http://stackoverflow.com/questions/2131463/fork-and-printf => suggests the fprintf should be replaced with a sprintf/write, not simply a fprintf/fflush)

ThanX again.  [[User:NevilleDNZ|NevilleDNZ]] 14:28, 26 September 2011 (UTC)

:Ok, but /etc/passwd is probably a bad example for this task, in part because not all operations on /etc/passed are appends.  Also, note that write() on an O_APPEND file can still give non-atomic behavior in some cases (for example, upon reaching the ulimit -f value) -- though this can be mitigated by using fixed size records which are a multiple of 512 bytes in length.  --[[User:Rdm|Rdm]] 14:58, 26 September 2011 (UTC)

:: This is beginning to sound a bit like the intent may have been to demonstrate file appends from different tasks and not having records ending up all scrambled.--[[User:Dgamey|Dgamey]] 16:11, 26 September 2011 (UTC)

Single <code>write</code> to a file opened with <code>O_APPEND</code> is atomic if the underlying file system supports it.  For example, it's not garanteed under NFS, where a lock is necessary (and still not garanteed to work, depending on the bugginess of OS).  A <code>printf</code> by default calls flush on end of line if output is tty, otherwise tends to only flush when buffer is full. --[[User:Ledrug|Ledrug]] 22:10, 26 September 2011 (UTC)

ThanX and Ouch... [Linux NFS Overview, FAQ and HOWTO Documents http://nfs.sourceforge.net/#faq_a9]
* A9. Why does opening files with O_APPEND on multiple clients cause the files to become corrupted?
* A. The NFS protocol does not support atomic append writes, so append writes are never atomic on NFS for any platform.
Commentary: Makes appending to the end of a file a bit complicated.  It would be nice if there was a way of querying a file to see if writes are atomic in its filesystem.

Is it worth continuing with this task? (It seems to lead to potentially "hairy/complicated" solutions.)

[[User:NevilleDNZ|NevilleDNZ]] 22:50, 26 September 2011 (UTC)

: I didn't like the task from the very begining because of the lack of focus.  If it's about maintaining a database, then it should include all basic operations: adding record, removing record, replacing existing record, and record lookup.  If it's about concurrent writing to a file, the passwd-like record requirement is unnecessary complication (colon-separated strings are pretty uninteresting anyway).  I think it's worthwhile to have a task that demonstrates concurrent file access, and the simpliest thing is ask for implementations of whole file or file-region locking with fixed length, random-access records. --[[User:Ledrug|Ledrug]] 00:11, 27 September 2011 (UTC)

My mistake... It seems that appending a structure/record to the end of a text file in a concurrent environment is both unusually difficult and unusual, especially if the file is a simple (but mythical) CSV "passwd" file.  I speculate that this is because it is not really a "language" function, more an OS and FS function. Certainly your feedback has been interesting. So, unless someone else objects, I'm am not strongly against having this task deleted. How do we do this?

[[User:NevilleDNZ|NevilleDNZ]] 01:05, 27 September 2011 (UTC)

Depending on what you want to salvage I suggest a couple of options:  If you know what you want, create another task (or tasks) for those requirements.  I'm not sure of the correct procedure.  Should this discussion get archived somewhere?  Rename and recast the task? Also, possibly mark this task prominently as 'dead' or 'deprecated' or whatever.  However, I haven't killed one yet so I may not be the best one to ask.  --[[User:Dgamey|Dgamey]] 01:52, 27 September 2011 (UTC)
: Some people may find file append operation interesting.  Maybe just add a note about concurrency issue and leave it at that?  It's not like the million other file related tasks worried about it. --[[User:Ledrug|Ledrug]] 02:13, 27 September 2011 (UTC)

Just a note about NFS. You don't get atomic writes on it. Ever. Moreover, you can't count on file locking working either (and mandatory locking really doesn't work). The only way to handle high-integrity data writing on NFS is to ''use a different, local filesystem''. Part of the problem is that even if the filesystem semantics worked, it's still all reliant on the underlying messages getting delivered over the network, and that's impossible to handle in a performant manner. (Locking, etc., require a basic concept called “distributed consensus”, and that's known theoretically hard, with the amount of practical difficulty depending on the probability of particular messages going AWOL. Heavy loads — when file contention is likely in the first place — is when this whole thing starts to really suck.) In practice, people put high-integrity filestores only on local disks and use other protocols (e.g., the ones for talking to webservers or remote databases) to communicate at a higher level of abstraction. –[[User:Dkf|Donal Fellows]] 10:56, 14 November 2011 (UTC)

== Table? ==

What is “Provide a summary of the language's "append record" capabilities in a table.” supposed to do? Wouldn't that information be better collected in another page so that it can be made consistent and to promote comparison of different languages? –[[User:Dkf|Donal Fellows]] 19:56, 27 September 2011 (UTC)

Collecting the information in another page is not as effective as having nearby actual code to demonstrate the properties. [[User:NevilleDNZ|NevilleDNZ]] 03:34, 6 October 2011 (UTC)
: I certainly think that demonstrating how to do things is good. That's the whole basis for RC tasks. –[[User:Dkf|Donal Fellows]] 10:59, 14 November 2011 (UTC)

+++
title = "Talk:Use another language to call a function"
description = ""
date = 2012-03-14T04:48:03Z
aliases = []
[extra]
id = 4713
[taxonomies]
categories = []
tags = []
+++

I am unsure if this is same as [[C FFI]], or not. If yes, then merge. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:57, 11 August 2009 (UTC)
:It's not. [[Call foreign language function]] and [[C FFI]] are the same task in essence though (and there are other closely related ones too; this is an area needing some rationalization). —[[User:Dkf|Donal Fellows]] 11:46, 17 August 2009 (UTC)

==Task needs work/splitting?==
While writing the Tcl implementation, it occurred to me that a number of languages might want to deal with the cases where a parameter is an ‘in’ parameter differently from the case given (really a single ‘out’ parameter, plus a bit of metadata to describe the buffer size). No time to work on this now though. —[[User:Dkf|Donal Fellows]] 13:10, 18 August 2009 (UTC)

== C code is wrong ==


```c
#include <stdio.h>

extern int Query (char * Data, size_t * Length);

int main (int argc, char * argv [])
{
   char     Buffer [1024];
   unsigned Size = sizeof (Buffer);

   if (0 == Query (Buffer, &Size))
   {
      printf ("failed to call Query\n");
   }
   else
   {
      char * Ptr = Buffer;
      while (Size-- > 0) putchar (*Ptr++);
      putchar ('\n');
   }
}
```



```c
#include <stdio.h>

int Query (char * Data, size_t * Length)
{
   printf("Length = %zu, 0x%zx\n", *Length, *Length);
   return 0;
}
```



```txt
$ cc -v
Reading specs from /usr/lib/gcc-lib/amd64-unknown-openbsd4.8/4.2.1/specs
Target: amd64-unknown-openbsd4.8
Configured with: OpenBSD/amd64 system compiler
Thread model: posix
gcc version 4.2.1 20070719
$ cc -o main main.c query.c
main.c: In function 'main':
main.c:10: warning: passing argument 2 of 'Query' from incompatible pointer type
$ ./main
Length = 7971459302400, 0x74000000400
failed to call Query
```


It should say 'Length = 1024', not 'Length = 7971459302400'. The problem is that main() passed an <tt>unsigned *</tt> but Query() expects <tt>size_t *</tt>. On my machine, <tt>unsigned</tt> is 4 bytes but <tt>size_t</tt> is 8 bytes.

I would like to fix the C code (by changing <tt>unsigned Size</tt> to <tt>size_t Size</tt>, and while there, by adding to main() some return statements), but I worry that if I fix the C code, then I will destroy all the examples that use the old C code. --[[User:Kernigh|Kernigh]] 15:45, 12 February 2011 (UTC)

:I'd say fix it.  Go saw that as a problem too.  &mdash;[[User:Sonia|Sonia]] 23:35, 12 February 2011 (UTC)

:+1 on fix. There is a template to warn others to review their entries too, but I can't remember what it is ... ... nope, still can't remember. --[[User:Paddy3118|Paddy3118]] 06:02, 13 February 2011 (UTC)

::clarified-review?  I made the change and added that template.  I hope I did it right!  &mdash;[[User:Sonia|Sonia]] 07:48, 13 February 2011 (UTC)

==Task focus and name?==
This might be better named as "Demonstrate how a provided function can be called from a foreign language". --[[User:Markhobley|Markhobley]] 14:57, 5 June 2011 (UTC)
: Task names should generally be short and sweet. A task summary might be appropriate. (Especially if we could implement something like task summaries as semantic properties) --[[User:Short Circuit|Michael Mol]] 15:20, 6 June 2011 (UTC)

:: What about "Demonstrate a function call from a foreign language"? --[[User:Markhobley|Markhobley]] 17:44, 6 June 2011 (UTC)
::: Near as I can figure, the task title is about as good as it can be. It's an inverse of [[Call foreign language function]], so if anything, it ought to be renamed [[Call function from foreign language]], removing the article 'a'. It seems good enough to not warrant that kind of redirect, though. --[[User:Short Circuit|Michael Mol]] 04:57, 7 June 2011 (UTC)

:::: The current title can be read one of two ways:
* Call (a function from a foreign language)
* (Call a function) from a foreign language

- Hence I suggested renaming this task. --14:10, 7 June 2011 (UTC)
::::: Hm. I see your point. I don't like the full-sentence approach for task names, though. Other ideas? --[[User:Short Circuit|Michael Mol]] 16:24, 7 June 2011 (UTC)

:::::: Maybe we could reverse the phrasing somehow: "Use a foreign language to call a function". --18:58, 7 June 2011 (UTC)

I don't know why we chose that particular function for the task description. Couldn't we have just done something that produces a result from a couple of numbers? The example in the task description requires an unnecessary amount of work to demonstrate something that could in essence be much simpler. --[[User:Markhobley|Markhobley]] 14:57, 5 June 2011 (UTC)
: I think the reason in this case was to intentionally call out a non-trivial area of language compatibility. Being able to pass strings back and forth between languages (especially where some of the languages have very different ways of exposing strings to programmers) is a touchy subject. That said, it may be appropriate to split the task between trivial and non-trivial scenarios. --[[User:Short Circuit|Michael Mol]] 15:20, 6 June 2011 (UTC)

Is the pipe option of pico lisp fair play?  It's a ubiquitous, and easy solution.--[[User:Lambertdw|Dave]] 04:43, 14 March 2012 (UTC)

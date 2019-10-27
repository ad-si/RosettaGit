+++
title = "Talk:Runtime evaluation"
description = ""
date = 2011-07-21T00:53:02Z
aliases = []
[extra]
id = 4315
[taxonomies]
categories = []
tags = []
+++

==Documenting calls==
Note that you can add links to online documentation for functions/statements used, but in this case it should be easy to find the items used in the example, and links should be to an authoritative source such as documentation maintained by the language maintainers. I used this to shorten the Python entry. --[[User:Paddy3118|Paddy3118]] 18:47, 3 June 2009 (UTC)

== All facilities? ==

I've only shown off some of Tcl's capabilities in this area. Documenting them all would bulk out this page rather a lot, even with all the other language examples. Hopefully there's enough there to be interesting anyway… —[[User:Dkf|Donal Fellows]] 20:51, 3 June 2009 (UTC)
:If you want to do more, you can create the page [[Eval/Tcl]] and link to it under the header on the main task page. This has been done before, but before we had support for slashes [[Polymorphism#BASIC]]. --[[User:Mwn3d|Mwn3d]] 20:54, 3 June 2009 (UTC)
::More work than I feel like tonight. :-) I'll focus on just making sure that the task itself is done for now; the fancy stuff is probably better done as another (probably pretty major) task. —[[User:Dkf|Donal Fellows]] 23:09, 3 June 2009 (UTC)

== Joke solution for C ==

Requires gdb; eval expressions as you'd type in gdb prompt; code must be compiled with -g flag and without -O.

```C>#include <stdio.h

#include <sys/types.h>
#include <unistd.h>
#include <err.h>

char spid[100], pname[1024];

#define EVAL(a) call_gdb(a, __LINE__ + 1)
void call_gdb(char * command, int lineno)
{
	if (fork()) {
		while (1);
		return;
	}

	FILE *fp = fopen("cmds", "w");
	fprintf(fp, "attach %s\nb %d\nret\n%s\n", spid, lineno, command);
	fclose(fp);

	freopen("/dev/null", "w", stdout);
	freopen("/dev/null", "w", stderr);

	execlp("gdb", pname, spid, "-x", "cmds", "-batch", "-q", 0);
	err(1, "Exec failure %s %s", pname, spid);
}

int main(int argc, char **argv)
{
	int a = 0, b = 1, c = 2;
	sprintf(spid, "%ld", (unsigned long) getpid());
	sprintf(pname, "%s", argv[0]);

	printf("before eval: a = %d\n", a);
	EVAL("set variable a = b + c");
	printf("after eval: a = %d\n", a);

	wait(0);
	return 0;
}
```

:--[[User:Ledrug|Ledrug]] 00:53, 21 July 2011 (UTC)

This does not meet the task requirements, because “You may not invoke a separate evaluator program…unless…that program…are considered part of your language/library/platform”, and gdb is not part of C. I would almost suggest adding it anyway for the amusement value, but I would not want the page to become cluttered with more not-about-practical-evaluation-and-not-enlightening-about-the-language answers. Good work anyway! —[[User:Kevin Reid|Kevin Reid]] 01:22, 20 July 2011 (UTC)

== Evaluation vs. invocation ==

Maybe we should rename this. It looks like many of the solutions are about evaluation of an expression at runtime, rather than invocation of another program. This is probably because we have used the term "evaluation" in the title. Maybe this would be better named as "Invoke another program". [[User:Markhobley|Markhobley]] 18:59, 19 July 2011 (UTC)

:Or possibly "Execute another program in the same language" [[User:Markhobley|Markhobley]] 20:00, 19 July 2011 (UTC)

:Er, what? --[[User:Ledrug|Ledrug]] 19:22, 19 July 2011 (UTC)

The task description states "Demonstrate your language's ability for programs to execute other programs in the language provided". That is "invocation" of another program, not "evaluation" of an expression. Looking through the solutions given, it looks like some solutions are about expressions and not about execution of another program, although I am not familiar with every language, so maybe I am misinterpreting the code given. [[User:Markhobley|Markhobley]] 19:53, 19 July 2011 (UTC)
: The "program" in the task description simply means "a piece of source code in your language".  It could be a file, or a string containing source code, or something along that line.  The C joke solution wasn't showing how to run another executable; in this case that executable (gdb) happens to be the debugger.  <code>EVAL()</code> calls gdb; it's constructed in such a way that the following happens: program forks; one copy busy waits, the other copy execs debugger; debugger comes online, sets a breakpoint right after the EVAL() call, interrupts the busy program and tells it to return; program returns to EVAL() and steps right into the breakpoint so debugger has control again; now debugger is looking at the correct context, evaluate the saved text, resumes the program and quits.  From the program's point of view, the net effect is a piece of text got eval'd.  It's a joke because that was a lot of hoops to jump through, highly inefficient and quite unreliable. --[[User:Ledrug|Ledrug]] 20:29, 19 July 2011 (UTC)

As the original author of the task, I declare: This task is about evaluation, not about starting processes. The task text does not mention "source code" or strings because it is OK to use ASTs as Lisp and E do. I've reworded the header to avoid talking about "programs". —[[User:Kevin Reid|Kevin Reid]] 01:17, 20 July 2011 (UTC)

::It's still no clearer. Is this chainloading? [[User:Markhobley|Markhobley]] 09:04, 20 July 2011 (UTC)
:::No. If you wanted to something like this in C, you might create a function which took C source code, compiled and linked it in your process's own address space (with some of the current process's active symbols available to that compile/link procedure), and then called its entry point. In Perl, you would simply call eval($yourSourceCode), and it would execute Perl with all variables in-scope at the time of eval() available to the eval()'d code. --[[User:Short Circuit|Michael Mol]] 11:21, 20 July 2011 (UTC)

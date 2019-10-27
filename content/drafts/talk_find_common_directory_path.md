+++
title = "Talk:Find common directory path"
description = ""
date = 2012-02-03T19:38:35Z
aliases = []
[extra]
id = 9436
[taxonomies]
categories = []
tags = []
+++

Shorter version for the C language:


```C

#define PATH_MAX 127
#include <string.h>
#include <stdio.h>

static void longestSharedPath(const char *fixed, char *moving) {
	char *t;
	unsigned n = 0, l = strlen(fixed);
	while (moving[n] == fixed[n] && n < l) n++;
	if (strlen(moving) == n && (l == n || (l > n && fixed[n] == '/'))) return;
	moving[n] = '\0';
	t = strrchr(moving, '/');
	if (t && t != moving) *t = '\0';
}

int main() {
	char *dir_list[] = {
		"/home/user1/tmp/coverage/test",
		"/home/user1/tmp/covert/operator",
		"/home/user1/tmp/coven/members",
		NULL
	};
	int i = 0;
	char tmp[PATH_MAX];
	strcpy(tmp, dir_list[0]);
	while (dir_list[++i]) {
		longestSharedPath(dir_list[i], tmp);
	}
	printf("%s\n", tmp);
	return 0;
}

```


:I think that the last two lines of leastCommonPath are wrong: 
```c
	t = strrchr(moving, '/');
	if (t && t != moving) *t = '\0';
```
.  This should place the null after the slash, and not eliminate the slash otherwise the slash will not be available for future comparisons.  If you change "coverage" to "dovetail" in the example data, I think this problem would raise its head. --[[User:Rdm|Rdm]] 11:46, 14 April 2011 (UTC)

It is intended to work that way, and it works with "dovetail" also. The first conditional handles cases both with and without terminating slash. Try it. [[User:Per|Per]] 12:07, 14 April 2011 (UTC)

:ok, yes, nevermind.  However, that does not mean that the algorithm is completely valid.  Consider, for example, what happens if you change an instance of "home" to "hone". Here's my proposed alternative:  
```c
static void longestSharedPath(const char *fixed, char *moving) {
        char *t;
        unsigned n = 0;
        while (moving[n] == fixed[n] && moving[n]) n++;
        if (!moving[n]) return;
        t = strrchr(moving, '/');
        if (t)
                if (t == moving)
                        moving[1]= '\0';
                else
                        *t = '\0';
}
```
 --[[User:Rdm|Rdm]] 14:22, 14 April 2011 (UTC)

That function will break on a path that starts the same and continues different, eg if you add "/home/user1/tmp2/coven/members" to the list. However, there are some good ideas there. Here is my combined version:


```C

static void longestSharedPath(const char *fixed, char *moving) {
	char *t;
	unsigned n = 0;
	while (moving[n] == fixed[n] && moving[n] && fixed[n]) n++;
	if (!moving[n] && (!fixed[n] || fixed[n] == '/')) return;
	moving[n] = '\0';
	t = strrchr(moving, '/');
	if (t && t != moving) *t = '\0';	// drop conflicting remainder
	else moving[1] = '\0';		// keep filesystem root
}

```


The thing about that first ''if'' line is that if all of moving is identical to matching chars in fixed, ''and'' either that is all of fixed or fixed continues into subdirectories, then we're done. But not otherwise.

:I was not able to make my version break.  Can you give me the complete definition for dir_list[] which makes it break?  Thanks.  --[[User:Rdm|Rdm]] 16:08, 14 April 2011 (UTC)


```C

     	char *dir_list[] = {
                "/home/user1/tmp/coverage/test",
                "/home/user1/tmp/covert/operator",
                "/home/user1/tmp/coven/members",
                "/home/user1/tmp2/coven/members",
                NULL
        };

```

should return "/home/user1". [[User:Per|Per]] 16:14, 14 April 2011 (UTC)

(elided)  --[[User:Rdm|Rdm]] 16:54, 14 April 2011 (UTC)

"home/user1/tmp" is not the longest common shared path any more. How about you drop by on irc to discuss this, instead of us abusing this talk page further? ;-) [[User:Per|Per]] 17:03, 14 April 2011 (UTC)

Correct version of python without reimplementing commonprefix

```Python

os.path.sep.join(os.path.commonprefix([p.split(os.path.sep) for p in ['/home/user1/tmp/coverage/test', '/home/user1/tmp/covert/operator', '/home/user1/tmp/coven/members']]))

```


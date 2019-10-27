+++
title = "Align columns/C"
description = ""
date = 2012-12-27T06:44:12Z
aliases = []
[extra]
id = 5309
[taxonomies]
categories = []
tags = []
+++

{{collection|Column Aligner}}


```c>#include <stdio.h


const char *str = 
	"Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n"
	"are$delineated$by$a$single$'dollar'$character,$write$a$program\n"
	"that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n"
	"column$are$separated$by$at$least$one$space.\n"
	"Further,$allow$for$each$word$in$a$column$to$be$either$left$\n"
	"justified,$right$justified,$or$center$justified$within$its$column.";

void align(const char *in, char alignment)
{
	int col, i, l, r;
	int w[1024] = {0};
	const char *s;

	for (s = in, i = col = 0; s[i]; s += i + 1) {
		for (i = 0; s[i] && s[i] != '$' && s[i] != '\n'; i++);

		if (i > w[col]) w[col] = i;

		if (col++ >= 1024) abort(); /* artificial limit */

		if (s[i] == '\n') col = 0;
		if (!s[i]) break;
	}

	for (s = in, i = col = 0; s[i]; s += i + 1) {
		for (i = 0; s[i] && s[i] != '$' && s[i] != '\n'; i++);

		switch(alignment) {
		case 'l':	r = w[col] - i; break;
		case 'c':	r = (w[col] - i)/2; break;
		case 'r':	r = 0; break;
		}
		l = w[col++] - i - r + 1;

		while (l--) putchar(' ');
		printf("%.*s", i, s);
		while (r--) putchar(' ');

		if (s[i] != '$') {
			putchar('\n');
			col = 0;
		}
		if (!s[i]) break;
	}
}

int main(void)
{
	puts("\n----  right ----"); align(str, 'r');
	puts("\n----  left  ----"); align(str, 'l');
	puts("\n---- center ----"); align(str, 'c');
	return 0;
}
```


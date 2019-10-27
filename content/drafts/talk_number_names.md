+++
title = "Talk:Number names"
description = ""
date = 2019-06-23T10:26:41Z
aliases = []
[extra]
id = 6632
[taxonomies]
categories = []
tags = []
+++

== Long vs. short scale ==

Please choose between [[wp:Long and short scales|long and short scale]], and amend the task description. --[[User:Short Circuit|Michael Mol]] 04:02, 22 March 2010 (UTC)
: Why not both? Split it into "Number names/Short scale" and "Number names/Long scale" with this page holding the description of the task, and definition of short scale vs. long scale, with links to the subpages. -- [[User:Eriksiers|Eriksiers]] 13:51, 22 March 2010 (UTC)

: Either? (So no present code is invalidated) --[[User:Paddy3118|Paddy3118]] 16:57, 22 March 2010 (UTC)

: Either is fine, but give a brief explanation of long form vs short form in the task description, then require the choice to be documented in the example. --[[User:Short Circuit|Michael Mol]] 19:58, 22 March 2010 (UTC)

: I'd recommend sticking to the short scale, since it's standard among English-speaking countries and Rosetta Code is in English. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 21:12, 22 March 2010 (UTC)

: Do you think the U.K. (who, most whould say, are English-speaking) may disagree with the last recommendation? [[User:Gerard Schildberger|Gerard Schildberger]] 23:22, 15 December 2010 (UTC)
::No. From the WP: "In 1974, the government of the UK switched to the short scale" --[[User:Mwn3d|Mwn3d]] 02:28, 16 December 2010 (UTC)
: I'd recommend supporting both scales.  That's what programs do, offer choices, formats, options, different parameters... [[User:Gerard Schildberger|Gerard Schildberger]] 23:24, 15 December 2010 (UTC)

::Maybe there is a locale environment variable for this? --16:06, 23 June 2011 (UTC)

: I suggest to rw-word the original task: Show how to spell out a number in (English) words. You can use ...  [[User:Gerard Schildberger|Gerard Schildberger]] 23:36, 15 December 2010 (UTC)

== Do as English does ==

Since the task deals with a natural language, a program's output should follow the normal language usage.  How do you pronounce 1,001? "one thousand <i>and</i> one", not "one thousand, one".  Currently Java, Ruby, Basic and Python solutions, probably among others, should be considered inadequate. --[[User:Ledrug|Ledrug]] 21:51, 22 June 2011 (UTC)
: That's probably a matter for local dialect variance. I pronounce it "one thousand, one", myself. Has a nice rhythmic pattern to it when counting aloud. "One ''thou''sand ''one''. One ''thou''sand ''two''. One ''thou''sand ''three''." etc. (I've been very, very bored in the past...) --[[User:Short Circuit|Michael Mol]] 13:12, 23 June 2011 (UTC)

== PARI/GP Code Doesn't Work ==

The code doesn't seem to work for numbers larger than three digits.

Using PARI/GP via the web on this page: [https://pari.math.u-bordeaux.fr/gp.html https://pari.math.u-bordeaux.fr/gp.html]


```parigp
Eng(n:int)={
	my(tmp,s="");
	if (n >= 1000000,
		tmp = n\1000000;
		s = Str(Eng(tmp), " million");
		n -= tmp * 1000000;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n >= 1000,
		tmp = n\1000;
		s = Str(Eng(tmp), " thousand");
		n -= tmp * 1000;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n >= 100,
		tmp = n\100;
		s = Str(Edigit(tmp), " hundred");
		n -= tmp * 100;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n < 20,
		return (Str(s, ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "ninteen"][n]))
	);
	tmp = n\10;
	s = Str(s, [0, "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"][tmp]);
	n -= tmp * 10;
	if (n, Str(s, "-", Edigit(n)), s)
};
Edigit(n)={
	["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"][n]
};
 
\\ Original code above here. Below is my test code.

test_nums = [1, 23, 456, 7890, 12345, 678901, 2345678];
for (x = 1, #test_nums, print(test_nums[x] " = " Eng(test_nums[x])));
```


This is the output:


```parigp
1 = one
23 = twenty-three
456 = four hundred fifty-six
7890 = eight hundred ninety
12345 = three hundred forty-five
678901 = nine hundred one
2345678 = six hundred seventy-eight
```


Follow-up: The code works for 1 thru 1099, but produces incorrect output at 1100:


```parigp
1 = one
...
1099 = one thousand ninety-nine
1100 = one hundred
```


--[[User:Chuck Coker|Chuck Coker]] ([[User talk:Chuck Coker|talk]]) 09:02, 23 June 2019 (UTC)

:I figured out the problem. In the millions, thousands, and hundreds sections, the lines:

:
```parigp
		s = Str(Eng(tmp), " million");
...
		s = Str(Eng(tmp), " thousand");
...
		s = Str(Edigit(tmp), " hundred");
```


: need to be changed to:

:
```parigp
		s = Str(s, Eng(tmp), " million");
...
		s = Str(s, Eng(tmp), " thousand");
...
		s = Str(s, Edigit(tmp), " hundred");
```


: The string s was being overwritten at each step until you get down into the hundreds section.

:The final code should look like this:

:
```parigp
Eng(n:int)={
	my(tmp, s="");
	if (n >= 1000000,
		tmp = n\1000000;
		s = Str(s, Eng(tmp), " million");
		n -= tmp * 1000000;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n >= 1000,
		tmp = n\1000;
		s = Str(s, Eng(tmp), " thousand");
		n -= tmp * 1000;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n >= 100,
		tmp = n\100;
		s = Str(s, Edigit(tmp), " hundred");
		n -= tmp * 100;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n < 20,
		return (Str(s, ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "ninteen"][n]))
	);
	tmp = n\10;
	s = Str(s, [0, "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"][tmp]);
	n -= tmp * 10;
	if (n, Str(s, "-", Edigit(n)), s)
};
Edigit(n)={
	["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"][n]
};
 
 
test_nums = [1, 23, 456, 7890, 12345, 678901, 2345678, 90123456, 789012345, 999999999, 1000000000, 6789012345, 67890123456, 789012345678, 8901234567890];
for (x = 1, #test_nums, print(test_nums[x] " = " Eng(test_nums[x])));
```


: The final output works up to 999,999,999. (I haven't checked ''all'' the numbers, but spot checks seem to work.) Numbers higher than that fail.

:
```parigp
1 = one
23 = twenty-three
456 = four hundred fifty-six
7890 = seven thousand eight hundred ninety
12345 = twelve thousand three hundred forty-five
678901 = six hundred seventy-eight thousand nine hundred one
2345678 = two million three hundred forty-five thousand six hundred seventy-eight
90123456 = ninety million one hundred twenty-three thousand four hundred fifty-six
789012345 = seven hundred eighty-nine million twelve thousand three hundred forty-five
999999999 = nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine
1000000000 = one thousand million
6789012345 = six thousand seven hundred eighty-nine million twelve thousand three hundred forty-five
67890123456 = sixty-seven thousand eight hundred ninety million one hundred twenty-three thousand four hundred fifty-six
789012345678 = seven hundred eighty-nine thousand twelve million three hundred forty-five thousand six hundred seventy-eight
8901234567890 = eight million nine hundred one thousand two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety
```


: Of course, once you get this far, it's easy enough to extend the range of numbers handled. For example, to add billions and trillions, change the following lines:

:
```parigp
Eng(n:int)={
	my(tmp, s="");
	if (n >= 1000000,
	...
```


: to:

:
```parigp
Eng(n:int)={
	my(tmp, s="");
	if (n >= 1000000000000,
		tmp = n\1000000000000;
		s = Str(s, Eng(tmp), " trillion");
		n -= tmp * 1000000000000;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n >= 1000000000,
		tmp = n\1000000000;
		s = Str(s, Eng(tmp), " billion");
		n -= tmp * 1000000000;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n >= 1000000,
	...
```


: Ouput:

:
```parigp
1 = one
23 = twenty-three
456 = four hundred fifty-six
7890 = seven thousand eight hundred ninety
12345 = twelve thousand three hundred forty-five
678901 = six hundred seventy-eight thousand nine hundred one
2345678 = two million three hundred forty-five thousand six hundred seventy-eight
90123456 = ninety million one hundred twenty-three thousand four hundred fifty-six
789012345 = seven hundred eighty-nine million twelve thousand three hundred forty-five
999999999 = nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine
1000000000 = one billion
6789012345 = six billion seven hundred eighty-nine million twelve thousand three hundred forty-five
67890123456 = sixty-seven billion eight hundred ninety million one hundred twenty-three thousand four hundred fifty-six
789012345678 = seven hundred eighty-nine billion twelve million three hundred forty-five thousand six hundred seventy-eight
8901234567890 = eight trillion nine hundred one billion two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety
```


:--[[User:Chuck Coker|Chuck Coker]] ([[User talk:Chuck Coker|talk]]) 10:22, 23 June 2019 (UTC)

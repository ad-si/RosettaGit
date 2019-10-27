+++
title = "Talk:Middle three digits"
description = ""
date = 2013-11-05T06:49:09Z
aliases = []
[extra]
id = 12885
[taxonomies]
categories = []
tags = []
+++

==Inspiration==
The idea for the task comes from this blog entry: [http://www.mikedoesweb.com/2013/interesting-interview-question-1/ Interesting interview question #1] by Michael Jasper. Thanks. --[[User:Paddy3118|Paddy3118]] 12:41, 2 February 2013 (UTC)

==Second D entry and return type==
Because the middle three digits can have leading zeroes, I would think that the more natural return type would be a string rather than an int. --[[User:Paddy3118|Paddy3118]] 19:18, 3 February 2013 (UTC)
: I didn't write that code, but I think I can explain it. The return type is a [http://dlang.org/phobos/std_variant.html#.Algebraic Variant] type that can represent ''any'' type. In this case it's limited to the types listed: string, char[]. So the function will return a Variant that represents either a string or a char[]. By looking at the returned type (peek) you can tell whether or not an error occured. [[User:Fwend|Fwend]] 21:54, 3 February 2013 (UTC)

::Ah. I get it now. The middle three digits are calculated numerically and then changed to an array of three characters for the non-error return value. Error conditions return a string type. Thanks. --[[User:Paddy3118|Paddy3118]] 22:13, 3 February 2013 (UTC)
::: Of course, you can also just check the length of the returned value, but that would be cowboy programming! :-) Discriminating between a string and  a char[] is also not very solid, when safety is concerned. They are both string types. A function could easily return an error message as a char[]. It would be better to throw an error, or if you want to avoid the overhead, to return an Exception object. Or an enum error code. Then you have strong typing. [[User:Fwend|Fwend]] 22:32, 3 February 2013 (UTC)

==C++ entry and generalization to other numbers of digits==
The check rejecting the cases where (len % 2 == 0) is only correct if the number of digits requested is also odd; it's perfectly reasonable to request the middle 2 digits of a 4-digit number, etc.  So the correct check is to reject the case where the length of the string does not have the same parity as the number of digits being requested (len % 2 != n % 2).  [[User:Markjreed|Markjreed]] 05:13, 5 February 2013 (UTC)

==Musings on the type of the return value==
The type of the return value from the function is, (purposely), not specified in the requirements. For good input, I returned a string in the Python version as 
# I found it easier to compute the answer by manipulating the string representation of the input integer, and,
# I had already thought that the function might have to return answers with leading zeroes. 

Others have functions that return integers. It is just as valid, just different. I guess the middle three digits can be calculated with modulo arithmetic and without any use of strings. In that case it may be easier to return the integer result and rely on output formatting to zero-extend results to three digits on printing? Different strokes and all that. --[[User:Paddy3118|Paddy3118]] 07:01, 5 February 2013 (UTC)

:The requirement was to return middle three digits. That can not be done with integer, since integer does not contain digits. --[[User:PauliKL|PauliKL]] 16:16, 5 February 2013 (UTC)
::Maybe just change it to something like "the middle three digits or the number represented by those digits" just to allow the freedom. --[[User:Mwn3d|Mwn3d]] 16:54, 5 February 2013 (UTC)

==task clarification==

I think the task requirement should read: 

''“Write a function/procedure/subroutine that is called with an integer value and returns the middle three digits of the integer (expressed in '''base 10''') if possible, or a clear error indication if not possible.”''

I took it to mean that the error indicator would be a ''clear'' explanation of what the error is, not just ''oops, an error''. 

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:37, 16 April 2013 (UTC)

-----

Also, the use of the word middle is being interpreted by most program examples as the middle three digits, including the end digits if the length of the number (in base 10) is exactly three digits.   If anybody was asked to pick any of the middle doors, and there is five doors in a row, I can guarentee that nobody will pick a door on either end.   Middle (to most people) means anything except the ends.   It's way to late to define what the ''middle'' is. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:05, 5 November 2013 (UTC)

:Base 10: I think it is OK to assume base 10 as the default base especially when base is not mentioned; also there are uncontested correct examples around from the start that make the same assumption. On the meaning of middle three when there are three: I would think that most people would probably have to think it through for this task, but should see that the answer asked for is a logical progression of the meaning of middle three when considering seven then five then three things.

:If asked to "pick any of the middle doors" is a different thing altogether, and much more loose in its use of English. There is less context and confusion as middle without some quantity could default to meaning one door to be picked. No context so what does middle mean? closest to a centre? numerically "half the way through"? What happens if their are two candidates for middle?

:As I hope I have shown, describing a task free from ambiguity is an impossible task. Best to take what is there and try your best to understand it. If there are many correct examples of the task there already then think that it may be better to ask for further personal clarification rather than ask for a change to the task description as task descriptions shouldn't be too long. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:49, 5 November 2013 (UTC)

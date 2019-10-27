+++
title = "Talk:Determine if a string is numeric"
description = ""
date = 2016-07-20T23:30:29Z
aliases = []
[extra]
id = 1640
[taxonomies]
categories = []
tags = []
+++

== Subtle bug in the C example? ==

Carefully reading the ''strtol()'' man page on my Linux box I see that the entire resulting string was converted to a long '''if''' (and only if)  *endptr (p in the example) is '\0' (an ASCII NUL) '''and''' if the *nptr (s in the example) was NOT '\0'.  In other words I think the example should test for a precondition to return false if it's called with an empty string as the argument.  If I were more confident in my C coding skills and my understanding of this particular function (and of the conformance of Linux to any relevant standards) I would insert a line like: ''if *s == '\0' return !*s;'' 

Am I mistaken?

[[User:JimD|JimD]] 16:16, 11 October 2007 (MDT)

:I think you are right: if you pass a void string, it returns ''true'' ... But it could be interpreted like: a void string can rapresent anything... :D I rather would add

<c>if ( s==NULL ) return 0;
if ( *s == 0 ) return 0;</c>

:This checks also for NULL pointer passed (odd!). --[[User:ShinTakezou|ShinTakezou]] 22:52, 11 December 2008 (UTC)
:: I've fixed the example (with a slightly more compact <code>if</code> :-)). BTW, there's no void string, only an empty string (well, one could call the null pointer a void string :-)). --[[User:Ce|Ce]] 14:12, 12 December 2008 (UTC)
:::Semanthic. Void means void (i.e., so to say, a container without contents); the same as empty. (Maybe I ''feel'' it since I am italian, so the fact that C uses the term its way does not let me think I can't use it as normally I would, to say ''empty''. By empty C string, I mean a pointer to a byte which is '\0'. ([http://www.merriam-webster.com/dictionary/void void Webster definition])

== Unix shell ==


```txt

#!/bin/sh

a='123'
if [ "$a" -eq "$a" ] 2>/dev/null
then
    echo "a is numeric"
else
    echo "a is abnumeric"
fi

```


== Exact definition of IsNumeric? ==

For those who don't know VB: How exactly is IsNumeric defined?
For example: Is leading/trailing whitespace allowed (i.e. "   123" or "123  ")?
Does it also accept floating point values (e.g. "2.5" or "1e5")?
What about thousands separators (e.g. "10,000")? Is that locale-dependent?
Are numbers in other bases (e.g. hexadecimal) allowed (assuming VB supports them otherwise)?
What about numbers too big to fit into a native integer (e.g. "9999999999999999999999999999999999999999999999999") resp. a native float (e.g. "1e1234567")?

: And what is its input?  The Python and C samples take a string.  The Ruby and Scheme samples take an object.  And in Tcl it's the same thing.

:Half the samples shown implement 'is integer', or 'is numeric character'. 
:In VB, IsNumeric is a function for validating input. VB examples (all true)
:: 1, 1.1, -1.1, "1", "1.1", "-1.1" "1.1-", " 1.1- ", " 1,1 ", " 1E1" "&HFF"
:VBscript:
:::msgbox isnumeric(1)
:::msgbox isnumeric(1.1)
:::msgbox isnumeric(-1.1)
:::msgbox isnumeric("1")
:::msgbox isnumeric( "1.1")
:::msgbox isnumeric( "-1.1")
:::msgbox isnumeric( " 1.1- ")
:::msgbox isnumeric(" 1,1 ")
:::msgbox isnumeric(" 1E1 ")
:::msgbox isnumeric(" &HFF ")

----

:: I hadn't realized that there was an ambiguity. I hadn't even realized that "isnumeric" is a VB function (I certainly don'ty know VB). In the two examples I contributed (IDL and TCL) I assumed that the task meant that something would be interpreted as a number if handed to the language in question. I.e. if I can multiply it with two or take the sin() of it then it is numeric. For example in IDL I might say "sin(double(x))" where "double(x)" converts the input into a "double" (8-byte float) which will fail if "x" is, for example, the string "foo". I trap the error and decide what is or isn't "numeric" based on the occurrence of this kind of error. This will allow "1.1" or "-.1e-04" or "+000003" etc.
:: Should we tag the task for clarification? [[User:Sgeier|Sgeier]] 10:34, 20 September 2007 (MDT)

:::I think we need clarification of what is classed as a numeric string. Are commas allowed in the string? Must they be in certain places? What about strings containing numbers represented using underscore annotation? Or numbers in a notation that represents a non-decimal base?
Which of the following example strings are classed as numeric for the purpose of this task?

* "20,376"
* "20367"
* "20 368"
* "203 69"
* "20_367"
* "203_76"
* "0x1234" - Hexadecimal
* "0xFFFF" - Hexadecimal
* "0xFFGF" - Is this invalid hexidecimal?
* "01677" - This could be an octal number
* "01678" - This could be an invalid octal number
* "0b10101000" - Could be a binary number
* "0b10102010" - This is probably an invalid binary number
* "10101000b" - This is a binary number in an alternative notation
* "10101020b" - This is an invalid binary number
* "1677o" - This is an octal number in an alternative notation
* "1678o" - This is an invalid octal number in an alternative notation
* "1234h" - Hexadecimal alternative notation
* "FFFFh" - Hexadecimal alternative notation
* "FFFGh" - This is not a valid hexadecimal number
* "+27" - The positive number 27
* "3+2" - This is an expression

--[[User:Markhobley|Markhobley]] 16:28, 4 June 2011 (UTC)
:Whatever would be a legal numeric literal accepted by the language compiler/interpreter - thus making it language specific? --[[User:Paddy3118|Paddy3118]] 20:10, 4 June 2011 (UTC)
::I'm not sure. In theory, the application program could support the various numeric formats, even though the underlying language may not. --[[User:Markhobley|Markhobley]] 21:38, 4 June 2011 (UTC)

I think we can say that expressions and the invalid numbers are not numeric strings, but this then means we need sufficient logic in the code to be able to identify these as such.

In the past, I have assumed that the number system was decimal and accepted only digits, an optional leading hyphen, and a single decimal point. With that logic, "a numeric string is a string consisting only of digits, an optional leading hyphen and an optional single decimal point". Maybe this is the way to go. Note that in some locales, numeric strings fall outside of this definition, so this also needs to be considered. Under that definition strings containing whitespace return a result of "not numeric", but this does not matter in practice, because code that makes use of the result can easily trim whitespace from the string before feeding it to the evaluator (I have done this before and it has worked well for me). --[[User:Markhobley|Markhobley]] 21:38, 4 June 2011 (UTC)

: If a leading hyphen (minus sign) is OK, why not a leading plus sign as well (but not both, of course)?   "A leading hyphen" was mentioned, but I assume you meant a ''single'' leading hyphen (minus sign).   However, some languages allow multiple leading signs. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:57, 28 September 2013 (UTC)

== Objective-C question ==

An anonymous user had posted a question about the Objective-C example. I'll try to translate.

"How to check the whole string to make sure it is numeric?" was the original question. I think they were looking for a character by character check? Maybe a regex? What do you Ob-C people think? --[[User:Mwn3d|Mwn3d]] 21:46, 11 December 2008 (UTC)

:As it was before, the code say the "123Not Numeric" is numeric... I made it so that it says it is numeric iff the whole string is a number. (By the way, this one says numeric for float, other implementation here would say that "123.3" is not numeric since they check for integer only...) --[[User:ShinTakezou|ShinTakezou]] 23:12, 11 December 2008 (UTC)

== How literally need the examples follow the task? ==

The task description says »A number in the syntax the language uses for literals« and that floating-point numbers should be included. First off, this probable invalidates any solution that uses the language's normal string-to-number routines since those usually are locale-aware. A string like <code>1.234.567,141</code> will convert fine on my system (using <code>de-DE</code> as locale but wouldn't be a valid numeric literal in the programming languages I know.

Furthermore – similar to the VB discussion above – many programming languages allow floating point numbers to be in the form <code>1.23e15</code> which is currently handled by very few, if any, examples. In a similar vein, hexadecimal, octal or binary numeric literals – in C and languages that follow its conventions closely, <code>09</code> would ''not'' be a valid numeric literal. —[[User:Hypftier|Johannes Rössel]] 17:56, 6 July 2010 (UTC)

:I would stick to the numeric literals that you could write in your source and get accepted as a number. If your compiler or interpreter doesn't accept locale-aware things like extra dots or commas then I'd say you were fine, (but what do I know).
:I guess examples should note if there are types of numeric literals of their language that the routines ''don't'' accept, but I think that some examples were written to implement something seen in other examples rather than with an idea to cover all the numeric literal forms the language allows. --[[User:Paddy3118|Paddy3118]] 02:44, 7 July 2010 (UTC)

==Mathematica seems lean?==
It just seems to me that Mathematica should have ways to recognize many more number types than the current entry seems to suggest. compare the E, Forth and Python entries. --[[User:Paddy3118|Paddy3118]] 14:54, 26 August 2010 (UTC)

== Second MATLAB solution might have a problem ==

I don't know if you want to call it "incorrect", but I think the second MATLAB solution would call "...." numeric. I don't see any way of determining that there is only one decimal point in the string. --[[User:Mwn3d|Mwn3d]] 15:59, 2 February 2012 (UTC)

== Problem with the VB.net example ==

The IsNumeric call will not always return a correct answer. Try "1234+" - IsNumeric will return True. Then pass this same value as an argument to Convert.ToDouble: it will fail "unhandled exception". There's no option but to loop thru the String character by character and check that each one is within range. --[[User:LazyZee|LazyZee]] 17:59, 19 October 2012 (UTC)

== R ==

The solution will not work correctly if the string contains "NA" (not a number). But NA can be used in numeric calculations. --[[User:Sigbert|Sigbert]] ([[User talk:Sigbert|talk]]) 13:01, 16 February 2015 (UTC)

== AWK code is wrong ==


```txt

awk 'BEGIN { x = "01"; print x == x+0 }'
0
awk 'BEGIN { x = "+1"; print x == x+0 }'
0
awk 'BEGIN { x = "1x"; print x == x+0 }'
0

```

But

```txt

awk 'BEGIN { x = "1x"; print x+1 }'
2

```


+++
title = "Talk:Rep-string"
description = ""
date = 2019-04-29T20:26:42Z
aliases = []
[extra]
id = 13461
[taxonomies]
categories = []
tags = []
+++

==Inspiration==
The task was inspired by [http://stackoverflow.com/questions/16474895/finding-a-pattern-in-a-binary-string this question] on stackoverflow. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:45, 10 May 2013 (UTC)

==Clarification==
The task needs some clarifation.  What is a repeat? Is string "10" itself repeated once? Is string "101" a repeat of "10"? When finding a repeated substring "if any", what's the preferred answer for "1111", is it "1" or "11", or even "111" and "1111"?  There needs be a clear definition of the word "repeat", not just relying on whatever the first sample solution says, beause can be too arbitrary. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 03:53, 12 May 2013 (UTC)

:I think I was most sloppy about ''"a series of ones and zeroes in a string"'': I meant a series of ones ''or'' zeroes. .. Oh wait .. Re-reading your question, repeat means "two or more times" which is in the description.

:Alternatively:
:# Take a string of ones and zeroes X.
:# Find out if there is at least one string Y of ones and/or zeroes that when repeated and truncated on the right to the same length as X is both equal to X ''and'' contains at least two repetitions of Y. 
:# If any valid Y is found then X is a rep-string.
:Hope this helps :-) --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:22, 12 May 2013 (UTC)

::To answer the question:   ''Is string "101" a repeat of "10"?''       Yes.   Repeated (twice, and then truncated) to the original length of three. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:22, 13 May 2013 (UTC)

:::''' '101' is ''not'' a rep-string''' as '10' does not appear at least twice. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:27, 13 May 2013 (UTC)

:::: Yes it is.  Repeat "10" twice, then truncate to the original length (which is three), that yields "101".   There is nothing in the task description that says the repeat string has to ''appear'' (I mean, multiple times) in the result string, as in the case when the string is replicated, then truncated to the original length. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:10, 13 May 2013 (UTC)
::::: The task description is not just the first paragraph; if you continue reading, it says the repeating group can't be longer than half the length, from which one may pretty easily infer the intended interpretation of the initial description, seems to me. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]])

:::::: The task description was amended twice after I had entered the REXX version 2 example.   With the latest task requirements, 101 isn't a rep-string. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:29, 13 May 2013 (UTC) 

::::::: Hi Gerard. Is the task description OK now? Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:48, 13 May 2013 (UTC)

:::::::: Well, it's more wordy.   I preferred the original task requirement without the need for the the last part of the first part, and the whole of the third paragraph.   I prefer simplicity (a clean definition) and succinctness, but then, it's not my dog.   I'm now left with my old arguments ... er, arguing with the original wording, not the amended requirements.   So it appears that my statements are out-of-sync with what's what. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:02, 13 May 2013 (UTC)

::By the way, almost all programming examples have incorrect output, showing strings to be non-reps, whilest in fact, they are. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:22, 13 May 2013 (UTC)
::: To get sane output, you need the restriction that ''the repeating unit must be no more than half of the length of the input string''. Otherwise, the longest “repeating” unit is potentially the whole string, which is nuts. Any decision procedure for this needs to be able to reject a string as well as to find the repetition if it exists. (I'll update the task description momentarily.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 08:26, 13 May 2013 (UTC)

:::: A task requirement is that the repeat occur two or more times.   One repeat doesn't meet that requirement. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:10, 13 May 2013 (UTC)

Should this sentence of the task description be changed?

Use the function to indicate the repeating substring if any, in the following: 

Use the function to indicate the repeating substrings if any, in the following: 

Does '1111' have 1 solution ('11') or 3 ('1', '11', '111')

Maybe the expected results could/should be stated.--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:25, 13 May 2013 (UTC)
::I have modified the task to require selection of the longest substring, if more than one is possible, since that's what most of the solutions seem to be assuming.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 23:24, 13 May 2013 (UTC)

::: Hi TimToady, I widened the requirement to report longest or shortest or all reps as there could be equal argument for those cases but I can't envisage an algorithm returning naturally some arbitrary other set of rep solutions unless that arbitrariness is 'self-injected'. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 04:17, 14 May 2013 (UTC)

::: '''Substring types''':
:::* longest/shortest/all ( length)
:::* ovelapping/non-overlapping
::: are any others ? --[[User:Adam majewski|Adam majewski]] ([[User talk:Adam majewski|talk]]) 06:53, 28 April 2019 (UTC)

==Reason for update request==
Ledrug posted a great algorithm for Python that worked on the previous list of examples but would have failed on a string of all ones or all zeroes without a (very) minor tweak. I applied the tweak but thought that:

```txt
'11'
'00'
```

Should added to the list of mandatory test strings. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 04:51, 13 May 2013 (UTC)
:: Why should these be nom-reps?? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 16:40, 13 May 2013 (UTC)
::: Obviously just a braino; even the Python examples agree they are indeed reps. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 17:41, 13 May 2013 (UTC)

::Hi Walter. I meant that Ledrugs' original code gave the wrong answer for these, not that they weren't rep-strings. Sorry for the confusion. Ledrugs' method was a great new way of solving things and I had to unlearn how I was doing things to figure out what he was doing. I thought that the corner cases of '11' and '00' needed mental investigation and that made me look at the limit of a range and think it - the original code, would need tweaking. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:15, 13 May 2013 (UTC)

==Draft status==
I guess we might want to keep this task as draft a while longer due to the nature of the questions above. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:26, 13 May 2013 (UTC)

==Curiouser and curiouser!==
Ledrugs' Python <tt>text.startswith(shifted_text)</tt> is equivalent to TimToadys' Perl 6 boolean shift and XOR.  I'll remember that! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:41, 13 May 2013 (UTC)

== c program ==

Hi,
C program works great. I have add new example and it failed ( or I'm wrong). The result IMHO should be   rep-string "001"

```c

/*

https://rosettacode.org/wiki/Rep-string#C

*/
#include <stdio.h>
#include <string.h>
 
int repstr(char *str)
{
    if (!str) return 0;
 
    size_t sl = strlen(str) / 2;
    while (sl > 0) {
        if (strstr(str, str + sl) == str)
            return sl;
        --sl;
    }
 
    return 0;
}
 
int main(void)
{
	// input strings = tests	
    char *strs[] = { 
    "1001110011", 
    "1110111011", 
    "0010010010", 
    "1111111111",
    "0100101101", 
    "0100100", 
    "101", 
    "11", 
    "00", 
    "00100100100100100100100100100100100100100100100100100100100100100100100100100" }; // not works
 
    size_t strslen = sizeof(strs) / sizeof(strs[0]); // number of test values
    
    size_t i;
    
    for (i = 0; i < strslen; ++i) { // check all test values
        	int n = repstr(strs[i]);
        	// print result
        	if (n) 
        		printf("\"%s\" = rep-string \"%.*s\"\n", strs[i], n, strs[i]);
        		else printf("\"%s\" = not a rep-string\n", strs[i]);
    } //
 
    return 0;
}
 

```


 
{{out}}

```txt

"1001110011" = rep-string "10011"
"1110111011" = rep-string "1110"
"0010010010" = rep-string "001"
"1111111111" = rep-string "11111"
"0100101101" = not a rep-string
"0100100" = rep-string "010"
"101" = not a rep-string
"11" = rep-string "1"
"00" = rep-string "0"
"00100100100100100100100100100100100100100100100100100100100100100100100100100" = rep-string "001001001001001001001001001001001001"


```


:When given ten 1's it returns five 1's as the rep-string. Maybe The C function returns the longest rep-string and you expected the shortest? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:50, 28 April 2019 (UTC)
:: You are right, it gives the longest. I think that it should be explicitly stated. What about splitting C section into the 2 subsections : longest and shortest ? Also example strings IMHO should have strings which give differrent results for longest and shortest test. Does it sounds good ? --[[User:Adam majewski|Adam majewski]] ([[User talk:Adam majewski|talk]]) 16:13, 28 April 2019 (UTC)

:::There is  already a section in the description on this. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:26, 29 April 2019 (UTC)

== new test values,  ==


```txt

{ 
    "1001110011", 
    "1110111011", 
    "0010010010",  /* 4 x 001 and truncated, lat char can be from 001*/
    "00100100101", /* 4 x 001 but last 2 chars are NOT from 001  */
    "1111111111",
    "0100101101", 
    "0100100", 
    "101", 
    "11", 
    "00", 
    "00100100100100100100100100100100100100100100100100100100100100100100100100100" 
};  

```


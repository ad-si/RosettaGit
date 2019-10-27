+++
title = "Talk:Symmetric difference"
description = ""
date = 2013-12-16T21:25:40Z
aliases = []
[extra]
id = 5034
[taxonomies]
categories = []
tags = []
+++

The symmetric difference should give one list which is the union of the two differences of the lists. The Perl example shows two lists. --[[User:Mwn3d|Mwn3d]] 23:19, 2 December 2009 (UTC)

*I agree with your statement about what “symmetric difference” means.
*All of the current examples produce two sets.
*The task as written tends to the two-lists interpretation.
Conclusion: Either the task should be renamed, or the task description should be clarified and the examples revised. --[[User:Kevin Reid|Kevin Reid]] 02:02, 3 December 2009 (UTC)
:I think the task name is good. Symmetric difference is actually an exercise I did in CS classes a few times. I think the task description should be changed to match the task name. --[[User:Mwn3d|Mwn3d]] 02:17, 3 December 2009 (UTC)
: Description updated, Perl example fixed, J example marked. (I saw that the Python example already provided the symmetric difference. --[[User:Short Circuit|Michael Mol]] 04:23, 3 December 2009 (UTC)

==Set type==
I noticed that the Ruby example was using lists rather than a set datatype. Although its use of lists satisfied the original task decription, in that it gave the correct answer, its use of lists would fall down if, for example, certain duplicates existed in the input lists. No duplicate values would ever exist in any result from a set based solution. Since the task is about sets rather than lists, (and has the Ritzy set expressions to prove it), I modified the task description to force a set-type result, without duplicates. It should hopefully be a small update affected implementations.

If you think an example falls foul of this then maybe you could fix/flag them? Thanks. --[[User:Paddy3118|Paddy3118]] 06:12, 30 January 2010 (UTC)
:Yeah but to deal with the duplicates issue all you have to do is run the [[Create a Sequence of unique elements]] task on the inputs to get rid of duplicates. --[[User:Spoon!|Spoon!]] 05:42, 2 February 2010 (UTC)
::Oh, I don't think it is hard to do, I just think that because we are dealing with sets then it isn't right to have the chance of duplicates in outputs. --[[User:Paddy3118|Paddy3118]] 07:23, 2 February 2010 (UTC)
:::Alright, this doesn't make sense to me. I understand what you said in the [http://rosettacode.org/mw/index.php?title=Symmetric_difference&diff=74493&oldid=prev log entry], and I'll admit I was unfamiliar with that requirement of "set", but if we're insisting on a strict definition of "set", I don't see how it makes sense to hold examples in languages without a set type to a different requirement. If the data passed into the program has duplicates within a list, then that list isn't a set. I would recommend leaving the note, but reducing it to a "optionally, verify your inputs," rather than a language-attribute-conditional requirement. --[[User:Short Circuit|Michael Mol]] 15:12, 9 February 2010 (UTC)

::::Hi Michael, I wrote the note because of personal experience. before Python had an explicit set type I had learned to ''not'' use lists as sets because of the duplicates issue. I n Python the idiom then was to use the keys of a dictionary (map or hash) and code around that to make it look like a set. It was quicker to find out if a key is in a dict than in a list and the keys of a dict are unique. Seeing more than one of the RC examples using lists, and knowing how easy it is to have duplicates in a list made me check the algorithms used.
::::I reasoned that the task is about sets. If I use a set type then the type ensures there are no duplicates. Isn't it fair that if another example is using lists then either they show how their lists are further constrained to work as sets, or that the algorithm will give a set-like answer if such checks are not shown? --[[User:Paddy3118|Paddy3118]] 23:39, 9 February 2010 (UTC)

:::::I understand where you're coming from; I couldn't earn my paycheck if I didn't deal with practical concerns when dealing with my code. However, when I write code, I work to keep input validation a component distinct from operating on that input; it improves mine and my coworkers' comprehension of my code, as well as keep the algorithms themselves visible and distinct. With this task, as I understand it and your understanding of it, the task description specifies that the input has the properties of a set, and requires that an algorithm be applied to that input. Verifying and ensuring that the data passed in meets the constraints of being a set falls under data validation and sanitation, and, for clarity's sake, I believe that such things should be a distinct component of the program where clarity is key.

:::::I'm not saying it ''must'' be a separate example, but rather that if it's included, it should be not be confused with the actual implementation of the algorithm itself (<math>(A \setminus B) \cup (B \setminus A)</math>). By all means, point out practical considerations and caveats; add them to the example's prologue, add them as an identified component of example code, or some other means, but ensure that input validation isn't confused with algorithm implementation.

:::::By changing the task to require input sanitation, it became necessary to mark a number of examples as incorrect, adding templates to identify those examples as requiring attention. If one were to change that requirement to allow noting input constraints as an alternate requirement, the ENAs aren't required, observers of the code are warned of caveats, and the core algorithm is still demonstrated. Does that make sense? --[[User:Short Circuit|Michael Mol]] 05:50, 10 February 2010 (UTC)

::::::Agree with you Micheal, about input validation and sanitation. For me, that's part of the user interface, (for user entered data) and for info in ascii/text files, the code that reads the records from the file.--[[User:Rldrenth|Rldrenth]] 17:06, 10 February 2010 (UTC)

::::The task asks for an operation of sets. It is reasonable to expect answers to be general over its inputs when those inputs are sets. If we give an answer where the input types are lists and not sets then that is a substitution of one well understood type by another well understood type, who's major differences are that a set is unordered and a list may have duplicates. 
::::I think it is reasonable when given a task where you are substituting input types that you either check it works 'as generally'. I think the list solutions would not depend on order, but may fail given duplication. If input types are being substituted then is it obvious that any input lists should/should not have duplicates and should/should not have an order imposed for the algorithm to work?
::::When comparing languages you may be doing a disservice to those that have a set type which automatically works in a more general manner over its inputs.

::::Probably gratuitous example that won't help my case :-) 
```python>>>
 s0 =list('ABCCD')
>>> # Lists as sets, not handling duplicates
>>> s1 =list('AAEEC')
>>> s0
['A', 'B', 'C', 'C', 'D']
>>> s1
['A', 'A', 'E', 'E', 'C']
>>> ans1 = ( [x for x in s0 if x not in s1] +
	     [y for y in s1 if y not in s0] )
>>> ans1
['B', 'D', 'E', 'E']
>>> 
>>> # Dictionary keys as sets giving the right answer
>>> s0 =dict((k,None) for k in 'ABCCD')
>>> s1 =dict((k,None) for k in 'AAEEC')
>>> s0
{'A': None, 'C': None, 'B': None, 'D': None}
>>> s1
{'A': None, 'C': None, 'E': None}
>>> ans2 = ( [x for x in s0 if x not in s1] +
	     [y for y in s1 if y not in s0] )
>>> ans2
['B', 'D', 'E']
>>> 
>>> # Using sets as inputs
>>> s0 =set('ABCCD')
>>> s1 =set('AAEEC')
>>> ans3 = s0 ^ s1
>>> ans3
{'B', 'E', 'D'}
```
 --[[User:Paddy3118|Paddy3118]] 07:40, 10 February 2010 (UTC)
:::::''If input types are being substituted then is it obvious that any input lists should/should not have duplicates and should/should not have an order imposed for the algorithm to work? ''
:::::Obvious or not, the concern can be resolved by simply noting the caveat in each language's example area, rather than requiring that a workaround be demonstrated in code. --[[User:Short Circuit|Michael Mol]] 13:44, 10 February 2010 (UTC)

::::::Yes, a note in each example using lists for input would work too. --[[User:Paddy3118|Paddy3118]] 16:58, 10 February 2010 (UTC)

::::::+1 --[[User:Rldrenth|Rldrenth]] 17:37, 10 February 2010 (UTC)

:: Paddy3118 wrote:<br />
:::> Oh, I don't think it is hard to do, I just think that because we are dealing with sets then it <br />
:::> isn't right to have the chance of duplicates in outputs.

::My programming philosopy would be that if the purpose of the function is to return the symmetric difference of two sets, then it is an error to call the function with a list containing duplicate elmeents.  The result of calling a function with invalid arguments is undefined, and the function can do whatever it pleases, including withdrawing all the money in your banking accounts. The symmetric difference function is not in error, the calling function is in error. 

::Now, however, if the stated function is to convert the inputs to sets, and return the symmetric difference of the sets, the function should do just that, and duplicate elements in each list are allowed as valid inputs.  It's a matter of what you defining as the purpose of your function. 

::Furthermore, you can define a function to return the symmetric difference of two sets if two sets are provided as input, or to throw an exception if lists are provided (containing one or more duplicates).  This is a different definition of the function, and it's definition includes lists containing duplicate elements as valid inputs to the functon. Just that it's response is different than if two valid sets were provided as inputs.--[[User:Rldrenth|Rldrenth]] 16:43, 10 February 2010 (UTC)

:::Hi Rldrenth, ''"...then it is an error to call the function with a list containing duplicate elmeents."''. I would go further. An example should state how and why their algorithms can use lists instead of sets; or constrain arbitrary lists as part of their routine. --[[User:Paddy3118|Paddy3118]] 16:58, 10 February 2010 (UTC)

== REXX Version 2 ==

the task says 
"If your code uses lists.." and I don't.
What is WRONG with my program (except for th items containing blanks (which is not in the requirements.
There is also no requirement to show the UNION of the sets.
Please remove the incorrect tag or be more specific. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:38, 16 December 2013 (UTC)

: The REXX version 2 ''does'' use a list of items (two lists, in fact). Any item is allowed to have blanks, the fact that none of the lists have names with special characters doesn't mean they aren't allowed. The REXX version 2 doesn't have blanks in the items, the blanks are separating the items (as delimiters). The REXX version 2 has words that are delimited by blanks instead of values within quotes, separated by commas. Because of the items are in a list, it should be able to handle duplicates (items) correctly. It doesn't.  Try and add a duplicate item to both of the lists and observe the output.  The list that version 2 uses is a list of words within a variable. That's a list. If they were a true set, then duplicates won't be a problem. So, again referring to note 1 (in the task requirements), introduce a duplicate (to each list), and observe if the duplicates are handled correctly. I agree with you there is no requirement to show the UNION of the sets. Perhaps you are referring to the optional part of the requirement: give the individual differences (symmetric difference of sets A and B). You may also want to peruse the GAP, Pike,  and Prolog examples (among others), although all of those appear to use true SETs. The PL/I example also suffers from the same problem as the Rexx version 2, although the list is stored in a fixed array (which seems problematic that the array size is hard-coded), and each value has a varying length; it also can't handle duplicates in the lists. If a language can use a true SET(s), it automatically handles duplicates (essentially by ignoring the duplicates). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:34, 16 December 2013 (UTC)
:: to me 'a b c' is a representation of a set (a, b, and c) and duplicate elements aren't a problem in this particular case/algorithm. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:47, 16 December 2013 (UTC)
::: oops, I see that there IS a problem if invalid sets (with duplicate elements) are specified.  Some other points are moot, however, --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:59, 16 December 2013 (UTC)

:::: ''No duplicate values would ever exist in any result from a set based solution.'' Paddy wrote that (above). To me, the key word is '''result''' from a set-based solution. Note that Wiki uses the word ''replicated value'' instead of ''duplicated value''. Six of one, half-dozen of the other. Note that every program solution (example) that actually ''uses'' duplicates in a list or SET handles duplications correctly. Stating that a list IS a SET doesn't make it so. 'A B C' is a list of three values (delimited by blanks, and of course, the values in this case can't contain blanks).  ["A", "B", "C"] is a better representation of that set. The list 'A B C B' is also represented by the set ["A", "B", "C"] (in any order). Also, if you could specify which points you think are moot, it would help minimize any confusion about which points are being discussed (or being dismissed as moot). I'm trying to express my opinions about lists versus SETs and the handling of duplicates in the list(s). How a list is expressed and (or) translated/transliterated to a representative SET is germane to this discussion. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:35, 16 December 2013 (UTC)

::: My (corrected) version 3 works perfectly with 'my' representation of sets:
<lang>setb='["Jim", "Mary", "John", "Jim", "Bob"]'
a=myset(seta)
b=myset(setb)
Say difference(a,b)
...
myset:
  Parse Arg li
  Return space(translate(li,' ','"[],'))
```

Your only valid point is that my algorithm can't handle elements containing blanks.

And, of coursem your '["Jim", "Mary", "John", "Jim", "Bob"]' is not a set!?!??--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:19, 16 December 2013 (UTC)

-----

Er, no.  I had other valid points.  

As for what isn't a set, again, peruse the GAP, Pike, and Prolog programming entries (solutions). Whether or not the representation (of values and/or a SET) is a valid set or not doesn't reduce the fact that Rexx version 1 and 1.5, GAP, Pike, and Prolog entries correctly handle duplicates (or ''replicated values'', as Wiki uses).  I used the SET dictated by ''note 1'' in the task's requirements as well as others solutions doing the same.

As for the '''translate''' that you used (above), it removes commas (''','''), quotes ('''"'''), right and left brackets ('''][''') from the values, not just the delimiters. Now, the values shown don't have any of those characters, but still, it's good to program ''as if'' those values had them.
Once those characters are handled correctly, it's one small step to handle blanks and nulls. That's why the REXX versions 1 and 1.5 took extra precautions (more REXX code) to only ''strip'' those characters used as delimiters and not ''translate'' them out of existence. (well, translate them to blanks). Furthermore, the older REXX entries can also handle ''null'' values, which are legal in SETs. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:40, 16 December 2013 (UTC)

: Note 1 starts with: " If your code uses lists of items to represent sets ..." 
Mine does not! 

Please let's end the discussion. My version handles sets that consist of elements not containing blanks (or maybe whitespace) period (The task descripion uses exactly such elements.) Prove the opposite or delete the invalid tag --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 20:09, 16 December 2013 (UTC)

Well, yes, Walter, it does use lists. Saying it doesn't use a list doesn't make it so. The lists used can look like SET elements, and indeed, represent elements in a SET. All you have to do is use the lists that ''note 1'' suggests to use (stating again, as does GAP, Pike, and Prolog use, among others, including REXX version 1 and 1.5). I have already executed the earlier version of the REXX version 2 with duplicates and it failed. I haven't gone through that process and validated it for you since the REXX version 2 was changed.  If you could incorporate the new lists (using duplicates) as per the task's requirements (''note 1'') and post the output to show that it handles duplicates correctly, than you can delete the tag, and we all know thereafter that duplicates in the lists are handled correctly. (I don't want to modify your program nor its output.) If you want me to modify the REXX version 2 program and its output, I can do that for you, but since you have your moniker on it, I left them intact.) Note that other programming examples do use SETs, and some of them still incorporated duplicates in the element lists, and handled them correctly, even though they didn't use lists to represent SETs. I know it seems to be a nit, but I shouldn't have to prove it wrong (what if I or others don't have a REXX interpreter that could execute/interpret the code?), all is needed is to show that the REXX version 2 code can handle duplicates. That's why output is shown, as a verification. There isn't a need to have this discussion; if it's that hard to add duplicates to the lists and verify the output, just remove the tag. It would be much simpler to just add the duplicates (as per what ''note 1'' suggests) and show the output from using those lists. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:03, 16 December 2013 (UTC)
:: but I DID change the program! It handles duplicates now correctly. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:13, 16 December 2013 (UTC)

::: Yes, good work! -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:25, 16 December 2013 (UTC)

+++
title = "Talk:Text processing/1"
description = ""
date = 2016-08-01T07:31:23Z
aliases = []
[extra]
id = 3105
[taxonomies]
categories = []
tags = []
+++

==Why?==
I was reading through [http://paddy3118.blogspot.com/2007/01/data-mining-in-three-language05.html old blog entries] and thought it would be appropriate (minus the focus on speed).

==The Sample File Was Not Found==
still missing [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 06:16, 1 August 2016 (UTC)
: I do have the file on my disk, but I'm not sure how to make it available. Maybe I should mail it to an admin. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 07:26, 1 August 2016 (UTC)

==Please clarify the task==
# Syntax errors in the file to be detected?
# The field separator is what? One space, any non-empty chain of spaces, any non-empty chain of spaces or tabs. Something else?
# Average to evaluate over all fields or else over each field separately?
# When at the same line some fields are flagged invalid but others are not, is it a gap? Or is it only when all fields are invalid?
# Further, do valid fields participate in averaging when some other fields at the same line are invalid?
# When a field is not present is it a syntax error or a gap?
# What to do when syntactically wrong fields appear (not a number, too large number etc)?
--[[User:Dmitry-kazakov|Dmitry-kazakov]] 12:13, 8 November 2008 (UTC)

Hi Dmitry the comp.lang.awk newsgroup [http://groups.google.co.uk/group/comp.lang.awk/browse_frm/thread/cbeda85544a742a1/0ecba3a3fbf247d8?hl=en&tvc=1#0ecba3a3fbf247d8 thread] contains all the information necessary for the original poster to get his job done. The example records are probably typical, but you need to try something out and make your own decisions on the format/error handling. The original newsgroup thread actually has more information than you get on some data munging problems as in many cases someone just says "wouldn't it be good if this talked to this"; or "When wasn't this working". 

Data format information might be [http://dataservice.eea.europa.eu/dataservice/metadetails.asp?id=1029 here]. (Sorry if I seem patronising, it was not meant) --[[User:Paddy3118|Paddy3118]] 17:34, 8 November 2008 (UTC)

:In my opinion, error checking should be minimal for this type of throwaway code, the assumption being that all data is formatted correctly, in forms that the target language can easily process. This type of task is all about coding speed and economy of expression. --[[User:IanOsgood|IanOsgood]] 16:30, 9 November 2008 (UTC)

: I suppose that any task should be defined in the article. The code presented for this task looks like a translation from one language into another, rather than independent implementations. Actually there is no way to verify whether they do the job or not. What would be the right output if the input file were:
```txt
2008/Mar/21    -1E-2 1
```
 On second thought I would suggest to replace it to something more general and better defined text processing task. Like parsing a [http://en.wikipedia.org/wiki/Comma-separated_values CSV file], for example. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:37, 8 November 2008 (UTC)

::How about we wait a week and see what others think? If no one else can figure it out then I will add further explanations.
::If you can find such malformed data in the readings.txt file then it becomes an issue. Asking such what-if questions seem to be finding ways to fail.
::On the Python, AWK, and Perl examples being similar: people are able to submit their own solutions in those languages if they think their solutions better fit the usual style of writing in Python/Perl/AWK, indeed someone suggests changes to the AWK solution in the newsgroup although they weren't obvious to me. --[[User:Paddy3118|Paddy3118]] 08:07, 9 November 2008 (UTC)

::: You can not assume that the source data has been formatted to suit your language. The very purpose of data munging is to convert the format when it is not what you need. (Although this task seems to be more calculating than format conversion.) Instead, we should assume that the data format is as it is in the example input file. Since in Rosetta Code we should try to keep the task as simple as possible, I think no error checking is needed.
::: Indeed, the task should be defined in the task page. You can not expect people to go to some other site and read lengthly forum conversations in order to find out what to do. Of course it does help a bit if there is a solution done by the person who defined the task in some language. But not everybody knows every language.
::: --[[User:PauliKL|PauliKL]] 13:54, 11 November 2008 (UTC)
::::PauliKL is right, this task seems to be more calculation than munging. Perhaps a new, similar, locally defined task could be made using the same data file (the date formatting is especially good for munging and some languages use _ for negative numbers instead of -)? The gap calculation seems like too much--like it's getting too far away from munging. Maybe just count the number of bad values in each line and report that in the line stats? I wouldn't worry about errors that much either. If anything, I would consider it bad data on an error (bad line for errors on dates and bad value for errors on values or flags) and just skip it.--[[User:Mwn3d|Mwn3d]] 14:25, 11 November 2008 (UTC)
 
:::::The book "Data Munging with Perl" by David Cross describes the data munging process as:

```txt
More specifically, data munging consists of a number of processes that are applied to
an initial data set to convert it into a different, but related data set. These processes will
fall into a number of categories: recognition, parsing, filtering, and transformation.
```

:::::Data munging is a loose term and ''does'' apply to the task. 
:::::There was a very real requirement to find the longest time that things were broke, and, compared to some requests, is quite reasonable. The newsgroup reference I added was to show that this was a real-world problem. The data already in the article should be enough to complete the task. The task maybe more challenging than some, but maybe trying to solve it will impart useful, (and marketable), skills, as well as being able to contrast solutions in different languages. Unfortunately their is no way to see the development process used in different programming languages because that might show if such a task is easier done in a scripting language. some of the questions asked above for example, might not occur if the language chosen made it easy to just assume correctly formed data and quickly write a parser that could be quickly re-written if your assumptions were wrong. This task is quite straightforward for a data munging task, the full file follows the syntax of the excerpt. Their are no hand editing errors, no funny escape characters, the needed results can be calculated from the data shown, ... --[[User:Paddy3118|Paddy3118]] 20:15, 11 November 2008 (UTC)
::::::Just because it's a real-world problem doesn't mean that it's a good instructional problem. People are coming here to learn and they don't need to filter through a complex task to see how to take in pre-formatted input and do a few little calculations with it. It's fine to mention that it's real, but I don't think makes the task any more valid for RC. I believe that the "broke" time calculation is simple, it just seems a little weird for learning purposes (once again, being real doesn't imply people will easily learn from it). I don't think the task as a whole is very difficult, I just think its complexity is overriding its purpose. As for the errors, in this particular case you may be able to assume perfect input, but in general it's good practice to be thinking "what if", and with data munging jobs in general, you may not be able to assume clean input. The people who ask about errors are just following their good programming instincts. So basically: it doesn't matter if it's real when people need to learn from it, simpler is better when creating tasks here, and it would be nice if we could just agree on what to do for some errors.--[[User:Mwn3d|Mwn3d]] 20:48, 11 November 2008 (UTC)
:::::::The task is a bit complicated because it performs two unrelated functions, calculating sums and averages line by line and simultaneously finding the longest gap asynchronously over multiple lines. But maybe it is not too complicated (and I already made an implementation for Vedit macro language). Anyway a few words about how the gap is calculated could be added in the task description. --[[User:PauliKL|PauliKL]] 16:39, 12 November 2008 (UTC)

: I think we should assume that the format is as in the example input file readings.txt. That is:
:* Field separator = single Tab character
:* Values are always given with 3 digits after decimal point (no scientific notation)
:* There are always 24 value/flag pairs in a line. (But if there were less, then just calculate those.)

: The original message in AWK newsgroup talks about counting gap length inside a single line. The implementations here find the longest gap that spans over multiple lines. I guess we can do that as it has been done in the existing implementations.
: --[[User:PauliKL|PauliKL]] 14:16, 11 November 2008 (UTC)
::Paul, This sentence:

```txt
However, I also need to know what the "maximum data gap" is, i.e. the
longest period with successive invalid measurements (i.e values with
flag<=0)
```
 
::Is where it is saying it wants the longest gap, not restricting it to a single record(line), as the sentence above it in the article is. --[[User:Paddy3118|Paddy3118]] 18:55, 11 November 2008 (UTC)
:::That sentence does not say whether they want the longest gap in a line or over multiple lines. However, the code included in the original message

  for (i=1;i<=24;i++)
     if ($(i*2+1)>0){
        num_valid++
        sum+=$(i*2)
     } else {
        # find out what the max_gap for this row is
     }
  
  if (num_valid>13 && max_gap<=6){
     print date,sum/num_valid,1
  } else {
     # print something else
  }

:::indicates that max gap for each row is required. In addition, if the gap is longer than 6, the average for the line would not be displayed. But if the data is from some measurement device such as weather station, it makes sense to find out the longest overall gap period. Anyway, from Rosetta Code point of view, this is insignificant. --[[User:PauliKL|PauliKL]] 13:06, 12 November 2008 (UTC)
